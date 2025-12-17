/-
  google.protobuf.Duration - A signed, fixed-length span of time.
-/
import Protolean.Message

namespace Google.Protobuf

/-- A Duration represents a signed, fixed-length span of time.
    It is independent of any calendar and concepts like "day" or "month". -/
structure Duration where
  /-- Signed seconds of the span of time.
      Must be from -315,576,000,000 to +315,576,000,000 inclusive. -/
  seconds : Int64 := 0
  /-- Signed fractions of a second at nanosecond resolution.
      Must be from -999,999,999 to +999,999,999 inclusive.
      For durations of one second or more, the sign of nanos must match seconds. -/
  nanos : Int32 := 0
  deriving Repr, BEq, Inhabited

namespace Duration

/-- Create a Duration from seconds -/
def fromSeconds (s : Int64) : Duration := { seconds := s }

/-- Create a Duration from milliseconds -/
def fromMillis (ms : Int64) : Duration :=
  { seconds := ms / 1000
  , nanos := ((ms % 1000) * 1000000).toInt32 }

/-- Create a Duration from seconds and nanoseconds -/
def fromSecondsNanos (s : Int64) (n : Int32) : Duration :=
  { seconds := s, nanos := n }

/-- Check if duration is valid (same sign for seconds and nanos when seconds != 0) -/
def isValid (d : Duration) : Bool :=
  if d.seconds > 0 then d.nanos >= 0
  else if d.seconds < 0 then d.nanos <= 0
  else true

/-- Convert to total nanoseconds (may overflow for large durations) -/
def toNanos (d : Duration) : Int64 :=
  d.seconds * 1000000000 + d.nanos.toInt64

/-- Convert to total milliseconds (truncates nanoseconds) -/
def toMillis (d : Duration) : Int64 :=
  d.seconds * 1000 + d.nanos.toInt64 / 1000000

/-- Negate a duration -/
def neg (d : Duration) : Duration :=
  { seconds := -d.seconds, nanos := -d.nanos }

/-- Add two durations -/
def add (a b : Duration) : Duration :=
  let totalNanos := a.nanos.toInt64 + b.nanos.toInt64
  let extraSeconds := totalNanos / 1000000000
  let remainingNanos := totalNanos % 1000000000
  { seconds := a.seconds + b.seconds + extraSeconds
  , nanos := remainingNanos.toInt32 }

/-- Subtract two durations -/
def sub (a b : Duration) : Duration :=
  add a (neg b)

instance : Neg Duration := ⟨neg⟩
instance : Add Duration := ⟨add⟩
instance : Sub Duration := ⟨sub⟩

end Duration

instance : Protolean.ProtoMessage Duration where
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
