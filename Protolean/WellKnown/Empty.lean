/-
  google.protobuf.Empty - A message with no fields.
  Used in gRPC for RPCs that have no request or response.
-/
import Protolean.Message

namespace Google.Protobuf

/-- Empty message - used for RPCs with no request/response -/
structure Empty where
  deriving Repr, BEq, Inhabited

instance : Protolean.ProtoMessage Empty where
  encodeFields _ := pure ()
  decodeField msg tag := do
    Protolean.Decoder.skipField tag.wireType
    pure msg
  defaultValue := {}

end Google.Protobuf
