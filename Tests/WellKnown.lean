/-
  Tests for well-known Google Protobuf types.
-/
import Crucible
import Protolean

namespace Tests.WellKnown

open Crucible
open Google.Protobuf
open Protolean

-- Helper to test round-trip encode/decode
def testRoundTrip [ProtoMessage α] [BEq α] (msg : α) : IO Bool := do
  let bytes := Protolean.encodeMessage msg
  match Protolean.decodeMessage bytes with
  | .ok decoded => pure (decoded == msg)
  | .error _ => pure false

testSuite "WellKnown Tests"

-- Empty type tests
test "Empty roundtrip" := do
  (← testRoundTrip Empty.mk) ≡ true

-- Timestamp tests
test "Timestamp roundtrip" := do
  let ts := Timestamp.fromSecondsNanos 1702857600 500000000
  (← testRoundTrip ts) ≡ true

test "Timestamp zero encodes to empty" := do
  let ts : Timestamp := {}
  let bytes := encodeMessage ts
  bytes.size ≡ 0

test "Timestamp fromSeconds works" := do
  let ts := Timestamp.fromSeconds 12345
  ts.seconds ≡ 12345
  ts.nanos ≡ 0

test "Timestamp isValid detects valid" := do
  let ts := Timestamp.fromSeconds 12345
  ts.isValid ≡ true

test "Timestamp isValid detects invalid" := do
  let invalidTs := { seconds := 100, nanos := -1 : Timestamp }
  invalidTs.isValid ≡ false

-- Duration tests
test "Duration roundtrip" := do
  let d := Duration.fromSecondsNanos 120 500000000
  (← testRoundTrip d) ≡ true

test "Duration zero encodes to empty" := do
  let d : Duration := {}
  let bytes := encodeMessage d
  bytes.size ≡ 0

test "Duration fromSeconds works" := do
  let d := Duration.fromSeconds 10
  d.seconds ≡ 10
  d.nanos ≡ 0

test "Duration fromMillis works" := do
  let d := Duration.fromMillis 2500
  d.seconds ≡ 2
  d.nanos ≡ 500000000

test "Duration negation works" := do
  let d := Duration.fromSeconds 10
  let negD := -d
  negD.seconds ≡ -10
  negD.nanos ≡ 0

test "Duration addition works" := do
  let d1 := Duration.fromSeconds 10
  let d2 := Duration.fromMillis 2500
  let sum := d1 + d2
  sum.seconds ≡ 12
  sum.nanos ≡ 500000000

-- Wrapper type tests
test "DoubleValue roundtrip" := do
  (← testRoundTrip { value := 3.14159 : DoubleValue }) ≡ true

test "FloatValue roundtrip" := do
  (← testRoundTrip { value := 0.5 : FloatValue }) ≡ true

test "Int64Value roundtrip" := do
  (← testRoundTrip { value := -9223372036854775808 : Int64Value }) ≡ true

test "UInt64Value roundtrip" := do
  (← testRoundTrip { value := 18446744073709551615 : UInt64Value }) ≡ true

test "Int32Value roundtrip" := do
  (← testRoundTrip { value := -2147483648 : Int32Value }) ≡ true

test "UInt32Value roundtrip" := do
  (← testRoundTrip { value := 4294967295 : UInt32Value }) ≡ true

test "BoolValue true roundtrip" := do
  (← testRoundTrip { value := true : BoolValue }) ≡ true

test "BoolValue false roundtrip" := do
  (← testRoundTrip { value := false : BoolValue }) ≡ true

test "StringValue roundtrip" := do
  (← testRoundTrip { value := "Hello, Protocol Buffers!" : StringValue }) ≡ true

test "BytesValue roundtrip" := do
  let bytes := ByteArray.mk #[0x01, 0x02, 0x03, 0xFF]
  (← testRoundTrip { value := bytes : BytesValue }) ≡ true

test "wrapper defaults encode to empty" := do
  let testCases : List (String × ByteArray) := [
    ("DoubleValue", encodeMessage ({} : DoubleValue)),
    ("FloatValue", encodeMessage ({} : FloatValue)),
    ("Int64Value", encodeMessage ({} : Int64Value)),
    ("UInt64Value", encodeMessage ({} : UInt64Value)),
    ("Int32Value", encodeMessage ({} : Int32Value)),
    ("UInt32Value", encodeMessage ({} : UInt32Value)),
    ("BoolValue", encodeMessage ({} : BoolValue)),
    ("StringValue", encodeMessage ({} : StringValue)),
    ("BytesValue", encodeMessage ({} : BytesValue))
  ]
  for (_, bytes) in testCases do
    bytes.size ≡ 0

-- Registry tests
test "Timestamp is wellknown type" := do
  (WellKnown.isWellKnownType "google.protobuf.Timestamp") ≡ true

test "StringValue is wellknown type" := do
  (WellKnown.isWellKnownType "google.protobuf.StringValue") ≡ true

test "custom type is not wellknown" := do
  (WellKnown.isWellKnownType "foo.bar.MyMessage") ≡ false

test "Duration has Lean name" := do
  match WellKnown.getLeanName "google.protobuf.Duration" with
  | some name => name ≡ "Google.Protobuf.Duration"
  | none => ensure false "Duration should have a Lean name"

test "timestamp proto is wellknown import" := do
  (WellKnown.isWellKnownImport "google/protobuf/timestamp.proto") ≡ true

test "wrappers proto is wellknown import" := do
  (WellKnown.isWellKnownImport "google/protobuf/wrappers.proto") ≡ true

test "custom proto is not wellknown import" := do
  (WellKnown.isWellKnownImport "myapp/messages.proto") ≡ false



end Tests.WellKnown
