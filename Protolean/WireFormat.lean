/-
  Protocol Buffer Wire Format types and field tags.

  Wire types define how values are encoded on the wire:
  - Varint: Variable-length integers
  - Fixed64: 8-byte fixed-width
  - LengthDelimited: Length-prefixed bytes
  - Fixed32: 4-byte fixed-width
-/
namespace Protolean

/-- Protocol Buffer wire types (Proto3) -/
inductive WireType : Type where
  | varint : WireType           -- 0: int32, int64, uint32, uint64, sint32, sint64, bool, enum
  | fixed64 : WireType          -- 1: fixed64, sfixed64, double
  | lengthDelimited : WireType  -- 2: string, bytes, embedded messages, packed repeated
  | fixed32 : WireType          -- 5: fixed32, sfixed32, float
  deriving Repr, DecidableEq, Inhabited

namespace WireType

/-- Convert wire type to its numeric representation -/
def toNat : WireType → Nat
  | varint => 0
  | fixed64 => 1
  | lengthDelimited => 2
  | fixed32 => 5

/-- Parse wire type from numeric value -/
def fromNat? : Nat → Option WireType
  | 0 => some varint
  | 1 => some fixed64
  | 2 => some lengthDelimited
  | 5 => some fixed32
  | _ => none

/-- Convert to UInt8 for encoding -/
def toUInt8 (wt : WireType) : UInt8 := wt.toNat.toUInt8

end WireType

/-- Field number in a protobuf message (1 to 2^29-1) -/
abbrev FieldNumber := UInt32

/-- A field tag combines field number and wire type: (field_number << 3) | wire_type -/
structure FieldTag where
  fieldNumber : FieldNumber
  wireType : WireType
  deriving Repr, DecidableEq

namespace FieldTag

/-- Encode a field tag as a UInt64 (for varint encoding) -/
def toUInt64 (tag : FieldTag) : UInt64 :=
  (tag.fieldNumber.toUInt64 <<< 3) ||| tag.wireType.toNat.toUInt64

/-- Decode a field tag from a UInt64 value -/
def fromUInt64? (n : UInt64) : Option FieldTag :=
  let wireTypeNum := (n &&& 0x7).toNat
  let fieldNum := (n >>> 3).toUInt32
  match WireType.fromNat? wireTypeNum with
  | some wt => if fieldNum > 0 then some ⟨fieldNum, wt⟩ else none
  | none => none

end FieldTag

end Protolean
