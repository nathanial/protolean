/-
  Efficient ByteArray builder using difference lists pattern.
  Allows O(1) append operations with O(n) final build.
-/
namespace Protolean

/-- Efficient ByteArray builder using difference lists -/
structure ByteBuilder where
  /-- The build function accumulates bytes onto a ByteArray -/
  build : ByteArray → ByteArray

namespace ByteBuilder

/-- Empty builder -/
def empty : ByteBuilder := ⟨id⟩

/-- Builder with a single byte -/
def byte (b : UInt8) : ByteBuilder := ⟨fun arr => arr.push b⟩

/-- Builder with raw bytes -/
def bytes (bs : ByteArray) : ByteBuilder := ⟨fun arr => arr ++ bs⟩

/-- Builder from a list of bytes -/
def ofList (bs : List UInt8) : ByteBuilder := ⟨fun arr => bs.foldl (·.push ·) arr⟩

/-- Builder from an array of bytes -/
def ofArray (bs : Array UInt8) : ByteBuilder := ⟨fun arr => bs.foldl (·.push ·) arr⟩

/-- Append two builders -/
def append (b1 b2 : ByteBuilder) : ByteBuilder := ⟨fun arr => b2.build (b1.build arr)⟩

instance : Append ByteBuilder where
  append := append

instance : HAppend ByteBuilder ByteBuilder ByteBuilder := ⟨append⟩

/-- Build final ByteArray -/
def toByteArray (builder : ByteBuilder) : ByteArray :=
  builder.build ByteArray.empty

/-- Get the size of the built content (O(n) - must build to compute) -/
def size (builder : ByteBuilder) : Nat :=
  builder.toByteArray.size

/-- Check if builder is empty -/
def isEmpty (builder : ByteBuilder) : Bool :=
  builder.size == 0

instance : Inhabited ByteBuilder where
  default := empty

end ByteBuilder

end Protolean
