/-
  ProtoCodec instances for all scalar protobuf types.
-/
import Protolean.Codec

namespace Protolean

-- ==================== Varint Types ====================

instance : ProtoCodec UInt32 where
  wireType := .varint
  encode v := Encoder.emitVarint32 v
  isDefault v := v == 0
  decode := Decoder.readVarint32
  defaultValue := 0

instance : ProtoCodec UInt64 where
  wireType := .varint
  encode v := Encoder.emitVarint v
  isDefault v := v == 0
  decode := Decoder.readVarint
  defaultValue := 0

instance : ProtoCodec Int32 where
  wireType := .varint
  encode v := Encoder.emitInt32 v
  isDefault v := v == 0
  decode := Decoder.readInt32
  defaultValue := 0

instance : ProtoCodec Int64 where
  wireType := .varint
  encode v := Encoder.emitInt64 v
  isDefault v := v == 0
  decode := Decoder.readInt64
  defaultValue := 0

instance : ProtoCodec Bool where
  wireType := .varint
  encode v := Encoder.emitBool v
  isDefault v := !v
  decode := Decoder.readBool
  defaultValue := false

-- ==================== ZigZag-encoded signed integers ====================

/-- Wrapper for sint32 (ZigZag encoded) -/
structure SInt32 where
  val : Int32
  deriving Repr, DecidableEq, Inhabited

instance : ProtoCodec SInt32 where
  wireType := .varint
  encode v := Encoder.emitSInt32 v.val
  isDefault v := v.val == 0
  decode := do
    let v ← Decoder.readSInt32
    return ⟨v⟩
  defaultValue := ⟨0⟩

/-- Wrapper for sint64 (ZigZag encoded) -/
structure SInt64 where
  val : Int64
  deriving Repr, DecidableEq, Inhabited

instance : ProtoCodec SInt64 where
  wireType := .varint
  encode v := Encoder.emitSInt64 v.val
  isDefault v := v.val == 0
  decode := do
    let v ← Decoder.readSInt64
    return ⟨v⟩
  defaultValue := ⟨0⟩

-- ==================== Fixed-width types ====================

/-- Wrapper for fixed32 (unsigned 32-bit, little-endian) -/
structure Fixed32 where
  val : UInt32
  deriving Repr, DecidableEq, Inhabited

instance : ProtoCodec Fixed32 where
  wireType := .fixed32
  encode v := Encoder.emitFixed32 v.val
  isDefault v := v.val == 0
  decode := do
    let v ← Decoder.readFixed32
    return ⟨v⟩
  defaultValue := ⟨0⟩

/-- Wrapper for fixed64 (unsigned 64-bit, little-endian) -/
structure Fixed64 where
  val : UInt64
  deriving Repr, DecidableEq, Inhabited

instance : ProtoCodec Fixed64 where
  wireType := .fixed64
  encode v := Encoder.emitFixed64 v.val
  isDefault v := v.val == 0
  decode := do
    let v ← Decoder.readFixed64
    return ⟨v⟩
  defaultValue := ⟨0⟩

/-- Wrapper for sfixed32 (signed 32-bit, little-endian) -/
structure SFixed32 where
  val : Int32
  deriving Repr, DecidableEq, Inhabited

instance : ProtoCodec SFixed32 where
  wireType := .fixed32
  encode v := Encoder.emitFixed32 v.val.toUInt32
  isDefault v := v.val == 0
  decode := do
    let v ← Decoder.readSFixed32
    return ⟨v⟩
  defaultValue := ⟨0⟩

/-- Wrapper for sfixed64 (signed 64-bit, little-endian) -/
structure SFixed64 where
  val : Int64
  deriving Repr, DecidableEq, Inhabited

instance : ProtoCodec SFixed64 where
  wireType := .fixed64
  encode v := Encoder.emitFixed64 v.val.toUInt64
  isDefault v := v.val == 0
  decode := do
    let v ← Decoder.readSFixed64
    return ⟨v⟩
  defaultValue := ⟨0⟩

-- ==================== Floating point ====================

/-- Wrapper for float (32-bit IEEE 754) -/
structure ProtoFloat where
  val : Float
  deriving Repr, Inhabited

instance : BEq ProtoFloat where
  beq a b := a.val == b.val

instance : ProtoCodec ProtoFloat where
  wireType := .fixed32
  encode v := Encoder.emitFloat v.val
  isDefault v := v.val == 0.0
  decode := do
    let v ← Decoder.readFloat
    return ⟨v⟩
  defaultValue := ⟨0.0⟩

/-- Wrapper for double (64-bit IEEE 754) - uses Lean's native Float -/
structure ProtoDouble where
  val : Float
  deriving Repr, Inhabited

instance : BEq ProtoDouble where
  beq a b := a.val == b.val

instance : ProtoCodec ProtoDouble where
  wireType := .fixed64
  encode v := Encoder.emitDouble v.val
  isDefault v := v.val == 0.0
  decode := do
    let v ← Decoder.readDouble
    return ⟨v⟩
  defaultValue := ⟨0.0⟩

-- ==================== Length-delimited types ====================

instance : ProtoCodec String where
  wireType := .lengthDelimited
  encode v := Encoder.emitString v
  isDefault v := v.isEmpty
  decode := Decoder.readString
  defaultValue := ""

instance : ProtoCodec ByteArray where
  wireType := .lengthDelimited
  encode v := Encoder.emitLengthDelimited v
  isDefault v := v.isEmpty
  decode := Decoder.readLengthDelimited
  defaultValue := ByteArray.empty

end Protolean
