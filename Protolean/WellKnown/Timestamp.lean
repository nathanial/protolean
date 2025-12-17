/-
  google.protobuf.Timestamp - A point in time.
  Represents seconds and nanoseconds since Unix epoch.
-/
import Protolean.Message

namespace Google.Protobuf

/-- A Timestamp represents a point in time independent of time zone.
    It is typically used to represent a point in time since the Unix epoch. -/
structure Timestamp where
  /-- Seconds of UTC time since Unix epoch (1970-01-01T00:00:00Z).
      Must be from 0001-01-01T00:00:00Z to 9999-12-31T23:59:59Z inclusive. -/
  seconds : Int64 := 0
  /-- Non-negative fractions of a second at nanosecond resolution.
      Must be from 0 to 999,999,999 inclusive. -/
  nanos : Int32 := 0
  deriving Repr, BEq, Inhabited

namespace Timestamp

/-- Create a Timestamp from Unix seconds -/
def fromSeconds (s : Int64) : Timestamp := { seconds := s }

/-- Create a Timestamp from seconds and nanoseconds -/
def fromSecondsNanos (s : Int64) (n : Int32) : Timestamp :=
  { seconds := s, nanos := n }

/-- Check if timestamp is valid (nanos in [0, 999999999]) -/
def isValid (t : Timestamp) : Bool :=
  t.nanos >= 0 && t.nanos < 1000000000

/-- Convert to total nanoseconds (may overflow for large timestamps) -/
def toNanos (t : Timestamp) : Int64 :=
  t.seconds * 1000000000 + t.nanos.toInt64

end Timestamp

instance : Protolean.ProtoMessage Timestamp where
  encodeFields msg := do
    -- Field 1: seconds (int64)
    if msg.seconds != 0 then
      Protolean.Encoder.emitTag 1 .varint
      Protolean.Encoder.emitInt64 msg.seconds
    -- Field 2: nanos (int32)
    if msg.nanos != 0 then
      Protolean.Encoder.emitTag 2 .varint
      Protolean.Encoder.emitInt32 msg.nanos

  decodeField msg tag := match tag.fieldNumber with
    | 1 => do
        let v ← Protolean.Decoder.readInt64
        pure { msg with seconds := v }
    | 2 => do
        let v ← Protolean.Decoder.readInt32
        pure { msg with nanos := v }
    | _ => do
        Protolean.Decoder.skipField tag.wireType
        pure msg

  defaultValue := {}

end Google.Protobuf
