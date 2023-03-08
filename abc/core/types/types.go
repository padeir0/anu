package types

import (
	"sort"
	"strconv"
)

type Type struct {
	ID        TypeID
	Structure *TypeStructure

	// frontend stuff
	Name          string
	ProductFields map[string]int
}

func (this *Type) String() string {
	if this == nil {
		return "nil"
	}
	if this.Name != "" {
		return this.Name
	}
	return this.ID.String()
}

type TypeKind int

// note that we don't need to take field names into consideration,
// this is not useful for the user since if it wants two different
// types to be different, he will declare a new type `type MyNewType is {whatever}`
const (
	InvalidTypeKind TypeKind = iota

	Sum       // [possibility0Type, possibility1Type, ..., possibilityNType]
	Product   // [field0Type, field1Type, ..., fieldNType]
	Array     // [baseType]
	Map       // [keyType, valueType]
	Proc      // [Arg0Type, Arg1Type, ..., ArgNType, RetType]
	Reference // [baseType]
	Option    // [baseType]
	Basic     // [] (basic has no structure, except for Ptr)
	Named     // [baseType] (named has structure)
)

type TypeStructure struct {
	Kind      TypeKind
	Structure []TypeID
	TagSize   int // for unions only
	Size      int // computed on lowering phases
}

func (this *TypeStructure) BaseType() TypeID {
	switch this.Kind {
	case Array, Reference, Option:
		return this.Structure[0]
	}
	panic("BaseType is only valid for Arrays, References and Options")
}

// must be canonicalized
// this hash is not commutative
func (this *TypeStructure) hash() int {
	start := int(this.Kind)
	for _, item := range this.Structure {
		start = start ^ (int(item) * 31)
	}
	return start
}

// both must be canonicalized
func (this *TypeStructure) equals(other *TypeStructure) bool {
	if this.Kind != other.Kind {
		return false
	}
	if len(this.Structure) != len(other.Structure) {
		return false
	}
	for i := range this.Structure {
		if this.Structure[i] != other.Structure[i] {
			return false
		}
	}
	return true
}

// TypeID is an index inside the TypeSpace.allTypes array
type TypeID int

func (this TypeID) String() string {
	switch this {
	case Void:
		return "void"
	case Nil:
		return "nil"
	case Bool:
		return "bool"
	case I8:
		return "i8"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case Ptr:
		return "$ptr"
	case Int:
		return "int"
	}
	return strconv.FormatInt(int64(this), 10)
}

const (
	InvalidTypeID TypeID = iota
	Void
	Nil
	Bool
	I8
	I16
	I32
	I64
	Ptr // (raw pointer for internal usage only)
	// preserves the type structure so we can validate
	// transformations
	Int
	last
)

func newBasicType(name string, id TypeID) *TypeWithName {
	return &TypeWithName{Name: &QualifiedName{SymbolName: name}, TypeStructure: &TypeStructure{Kind: Basic, Structure: []TypeID{id}}}
}

func NewTypeSpace() *TypeSpace {
	alltypes := make([]*TypeWithName, 512)
	alltypes[Void] = newBasicType("void", Void)
	alltypes[Nil] = newBasicType("nil", Nil)
	alltypes[Bool] = newBasicType("bool", Bool)
	alltypes[I8] = newBasicType("i8", I8)
	alltypes[I16] = newBasicType("i16", I16)
	alltypes[I32] = newBasicType("i32", I32)
	alltypes[I64] = newBasicType("i64", I64)
	alltypes[Ptr] = newBasicType("$ptr", I64)
	alltypes[Int] = newBasicType("int", Int)

	basictypes := map[QualifiedName]TypeID{
		{SymbolName: "void"}: Void,
		{SymbolName: "nil"}:  Nil,
		{SymbolName: "bool"}: Bool,
		{SymbolName: "i8"}:   I8,
		{SymbolName: "i16"}:  I16,
		{SymbolName: "i32"}:  I32,
		{SymbolName: "i64"}:  I64,
		{SymbolName: "$ptr"}: Ptr,
		{SymbolName: "int"}:  Int,
	}

	return &TypeSpace{
		allTypes: alltypes,
		top:      int(last),
		named:    basictypes,
		tmap:     newTypeMap(),
	}
}

type TypeWithName struct {
	Name          *QualifiedName
	TypeStructure *TypeStructure
}

type QualifiedName struct {
	ModuleName string
	SymbolName string
}

func (this QualifiedName) String() string {
	return this.ModuleName + "::" + this.SymbolName
}

// to compare by identity just compare TypeID
// to compare by equivalency:
//     two new types: take the underlying TypeID and compare
//     new type with unnamed: take the underlying TypeID from the new type
//         and the TypeID of the unamed one
//     two unnamed: compare TypeID
//     two aliases: compare TypeID
//     alias with new type: take the underlying TypeID from the new type
//         and the TypeID of the alias
type TypeSpace struct {
	// all types unique by type identity
	allTypes []*TypeWithName
	top      int

	named map[QualifiedName]TypeID

	tmap *typemap
}

func (this *TypeSpace) Named(name *QualifiedName) TypeID {
	v, ok := this.named[*name]
	if ok {
		return v
	}
	id := this.pushType(&TypeWithName{Name: name, TypeStructure: nil})
	this.named[*name] = id
	return id
}

// The structure of a named type must be internalized first
func (this *TypeSpace) UpdateNamed(name *QualifiedName, id TypeID) {
	v, ok := this.named[*name]
	if ok {
		this.allTypes[v] = &TypeWithName{name, &TypeStructure{Kind: Named, Structure: []TypeID{id}}}
	}
	panic("Named type not found: " + name.String())
}

func (this *TypeSpace) Unamed(ts *TypeStructure) TypeID {
	if ts.Kind == InvalidTypeKind {
		panic("invalid type kind")
	}
	newTS := this.canonicalize(ts)
	id, ok := this.tmap.Lookup(newTS)
	if ok {
		return id
	}

	id = this.pushType(&TypeWithName{Name: nil, TypeStructure: newTS})
	this.tmap.Insert(newTS, id)

	return id
}

func (this *TypeSpace) GetStructure(id TypeID) *TypeStructure {
	if id < 0 || int(id) >= len(this.allTypes) {
		panic("Invalid TypeID: " + strconv.FormatInt(int64(id), 10))
	}
	return this.allTypes[id].TypeStructure
}

func (this *TypeSpace) pushType(info *TypeWithName) TypeID {
	if this.top >= len(this.allTypes) {
		this.allTypes = append(this.allTypes, make([]*TypeWithName, len(this.allTypes))...)
	}
	id := TypeID(this.top)
	this.allTypes[id] = info
	this.top++
	return id
}

func (this *TypeSpace) canonicalize(ts *TypeStructure) *TypeStructure {
	switch ts.Kind {
	case Sum:
		// removes voids and duplicates
		nodup := removeDuplicates(remove(ts.Structure, Void))
		// sort by TypeID
		intslice := sort.IntSlice(toInt(nodup))
		intslice.Sort()
		newStruct := fromInt([]int(intslice))
		return &TypeStructure{
			Kind:      Sum,
			Structure: newStruct,
		}
	case Product:
		// a product with one field is equal to that field
		if len(ts.Structure) == 1 {
			id := ts.Structure[0]
			return this.GetStructure(id)
		}
		// remove nils
		newStruct := remove(ts.Structure, Nil)
		return &TypeStructure{
			Kind:      Product,
			Structure: newStruct,
		}
	case Option:
		// turn into (T | nil)
		a := &TypeStructure{
			Kind:      Sum,
			Structure: []TypeID{Nil, ts.Structure[0]},
		}
		this.canonicalize(a) // orders the sum
		return a
	case InvalidTypeKind:
		panic("invalid type kind")
	default:
		return ts
	}
}

func remove(structure []TypeID, toRemove TypeID) []TypeID {
	output := []TypeID{}
	for _, t := range structure {
		if t != toRemove {
			output = append(output, t)
		}
	}
	return output
}

func removeDuplicates(structure []TypeID) []TypeID {
	output := []TypeID{}
	for i, t := range structure {
		if !isDup(t, structure[:i]) {
			output = append(output, t)
		}
	}
	return output
}

func isDup(t TypeID, others []TypeID) bool {
	for _, other := range others {
		if other == t {
			return true
		}
	}
	return false
}

func toInt(list []TypeID) []int {
	output := make([]int, len(list))
	for i := range list {
		output[i] = int(list[i])
	}
	return output
}

func fromInt(list []int) []TypeID {
	output := make([]TypeID, len(list))
	for i := range list {
		output[i] = TypeID(list[i])
	}
	return output
}

func newTypeMap() *typemap {
	mp := &typemap{
		buckets: make([]*typelist, 512),
		top:     0,
	}
	return mp
}

// we use this typemap to speed up
// looking if the structure of type is unique
type typemap struct {
	buckets []*typelist
	top     int
}

// ts must be canonicalized
func (this *typemap) Lookup(ts *TypeStructure) (TypeID, bool) {
	index := ts.hash() % len(this.buckets)
	list := this.buckets[index]
	if list == nil {
		return -1, false
	}

	curr := list
	for curr != nil {
		if curr.TypeStructure.equals(ts) {
			return curr.ID, true
		}
		curr = curr.Next
	}

	return -1, false
}

// ts must be canonicalized
func (this *typemap) Insert(ts *TypeStructure, id TypeID) {
	index := ts.hash() % len(this.buckets)
	list := this.buckets[index]
	if list == nil {
		this.buckets[index] = newtypelist(id, ts)
		return
	}

	curr := list
	for curr.Next != nil {
		curr = curr.Next
	}
	curr.Next = newtypelist(id, ts)
}

type typelist struct {
	ID TypeID
	// must be canonicalized
	TypeStructure *TypeStructure
	Next          *typelist
}

func newtypelist(id TypeID, str *TypeStructure) *typelist {
	return &typelist{
		ID:            id,
		TypeStructure: str,
		Next:          nil,
	}
}
