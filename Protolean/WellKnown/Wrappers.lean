/-
  google.protobuf.wrappers - Wrapper types for nullable scalars.

  In proto3, primitive fields cannot be null. These wrapper types
  allow expressing "absence" by using Option WrapperType.
-/
import Protolean.Message

namespace Google.Protobuf

/-- Wrapper for double values -/
structure DoubleValue where
  value : Float := 0.0
  deriving Repr, BEq, Inhabited

/-- Wrapper for float values -/
structure FloatValue where
  value : Float := 0.0
  deriving Repr, BEq, Inhabited

/-- Wrapper for int64 values -/
structure Int64Value where
  value : Int64 := 0
  deriving Repr, BEq, Inhabited

/-- Wrapper for uint64 values -/
structure UInt64Value where
  value : UInt64 := 0
  deriving Repr, BEq, Inhabited

/-- Wrapper for int32 values -/
structure Int32Value where
  value : Int32 := 0
  deriving Repr, BEq, Inhabited

/-- Wrapper for uint32 values -/
structure UInt32Value where
  value : UInt32 := 0
  deriving Repr, BEq, Inhabited

/-- Wrapper for bool values -/
structure BoolValue where
  value : Bool := false
  deriving Repr, BEq, Inhabited

/-- Wrapper for string values -/
structure StringValue where
  value : String := ""
  deriving Repr, BEq, Inhabited

/-- Wrapper for bytes values -/
structure BytesValue where
  value : ByteArray := ByteArray.empty
  deriving BEq, Inhabited

instance : Repr BytesValue where
  reprPrec bv _ := s!"BytesValue.mk (ByteArray.mk #[{", ".intercalate (bv.value.toList.map (fun b => toString b))}])"

-- ProtoMessage instances for each wrapper type
-- All have a single field with number 1

instance : Protolean.ProtoMessage DoubleValue where
  encodeFields msg := do
    if msg.value != 0.0 then
      Protolean.Encoder.emitTag 1 .fixed64
      Protolean.Encoder.emitDouble msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readDouble
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage FloatValue where
  encodeFields msg := do
    if msg.value != 0.0 then
      Protolean.Encoder.emitTag 1 .fixed32
      Protolean.Encoder.emitFloat msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readFloat
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage Int64Value where
  encodeFields msg := do
    if msg.value != 0 then
      Protolean.Encoder.emitTag 1 .varint
      Protolean.Encoder.emitInt64 msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readInt64
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage UInt64Value where
  encodeFields msg := do
    if msg.value != 0 then
      Protolean.Encoder.emitTag 1 .varint
      Protolean.Encoder.emitVarint msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readVarint
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage Int32Value where
  encodeFields msg := do
    if msg.value != 0 then
      Protolean.Encoder.emitTag 1 .varint
      Protolean.Encoder.emitInt32 msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readInt32
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage UInt32Value where
  encodeFields msg := do
    if msg.value != 0 then
      Protolean.Encoder.emitTag 1 .varint
      Protolean.Encoder.emitVarint32 msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readVarint32
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage BoolValue where
  encodeFields msg := do
    if msg.value then
      Protolean.Encoder.emitTag 1 .varint
      Protolean.Encoder.emitBool msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readBool
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage StringValue where
  encodeFields msg := do
    if !msg.value.isEmpty then
      Protolean.Encoder.emitTag 1 .lengthDelimited
      Protolean.Encoder.emitString msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readString
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

instance : Protolean.ProtoMessage BytesValue where
  encodeFields msg := do
    if !msg.value.isEmpty then
      Protolean.Encoder.emitTag 1 .lengthDelimited
      Protolean.Encoder.emitLengthDelimited msg.value
  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readLengthDelimited
        pure { value := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg
  defaultValue := {}

end Google.Protobuf
