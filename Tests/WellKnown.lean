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
  ensure (← testRoundTrip Empty.mk) "Empty should round-trip"

-- Timestamp tests
test "Timestamp roundtrip" := do
  let ts := Timestamp.fromSecondsNanos 1702857600 500000000
  ensure (← testRoundTrip ts) "Timestamp should round-trip"

test "Timestamp zero encodes to empty" := do
  let ts : Timestamp := {}
  let bytes := encodeMessage ts
  ensure (bytes.size == 0) s!"Empty timestamp should encode to zero bytes, got {bytes.size}"

test "Timestamp fromSeconds works" := do
  let ts := Timestamp.fromSeconds 12345
  ensure (ts.seconds == 12345 && ts.nanos == 0) "Timestamp.fromSeconds should work"

test "Timestamp isValid detects valid" := do
  let ts := Timestamp.fromSeconds 12345
  ensure ts.isValid "Valid timestamp should pass isValid"

test "Timestamp isValid detects invalid" := do
  let invalidTs := { seconds := 100, nanos := -1 : Timestamp }
  ensure (!invalidTs.isValid) "Invalid timestamp should not pass isValid"

-- Duration tests
test "Duration roundtrip" := do
  let d := Duration.fromSecondsNanos 120 500000000
  ensure (← testRoundTrip d) "Duration should round-trip"

test "Duration zero encodes to empty" := do
  let d : Duration := {}
  let bytes := encodeMessage d
  ensure (bytes.size == 0) s!"Empty duration should encode to zero bytes, got {bytes.size}"

test "Duration fromSeconds works" := do
  let d := Duration.fromSeconds 10
  ensure (d.seconds == 10 && d.nanos == 0) "Duration.fromSeconds should work"

test "Duration fromMillis works" := do
  let d := Duration.fromMillis 2500
  ensure (d.seconds == 2 && d.nanos == 500000000) s!"Duration.fromMillis got {d.seconds}s {d.nanos}ns"

test "Duration negation works" := do
  let d := Duration.fromSeconds 10
  let negD := -d
  ensure (negD.seconds == -10 && negD.nanos == 0) "Duration negation should work"

test "Duration addition works" := do
  let d1 := Duration.fromSeconds 10
  let d2 := Duration.fromMillis 2500
  let sum := d1 + d2
  ensure (sum.seconds == 12 && sum.nanos == 500000000) s!"Duration add got {sum.seconds}s {sum.nanos}ns"

-- Wrapper type tests
test "DoubleValue roundtrip" := do
  ensure (← testRoundTrip { value := 3.14159 : DoubleValue }) "DoubleValue should round-trip"

test "FloatValue roundtrip" := do
  ensure (← testRoundTrip { value := 0.5 : FloatValue }) "FloatValue should round-trip"

test "Int64Value roundtrip" := do
  ensure (← testRoundTrip { value := -9223372036854775808 : Int64Value }) "Int64Value should round-trip"

test "UInt64Value roundtrip" := do
  ensure (← testRoundTrip { value := 18446744073709551615 : UInt64Value }) "UInt64Value should round-trip"

test "Int32Value roundtrip" := do
  ensure (← testRoundTrip { value := -2147483648 : Int32Value }) "Int32Value should round-trip"

test "UInt32Value roundtrip" := do
  ensure (← testRoundTrip { value := 4294967295 : UInt32Value }) "UInt32Value should round-trip"

test "BoolValue true roundtrip" := do
  ensure (← testRoundTrip { value := true : BoolValue }) "BoolValue(true) should round-trip"

test "BoolValue false roundtrip" := do
  ensure (← testRoundTrip { value := false : BoolValue }) "BoolValue(false) should round-trip"

test "StringValue roundtrip" := do
  ensure (← testRoundTrip { value := "Hello, Protocol Buffers!" : StringValue }) "StringValue should round-trip"

test "BytesValue roundtrip" := do
  let bytes := ByteArray.mk #[0x01, 0x02, 0x03, 0xFF]
  ensure (← testRoundTrip { value := bytes : BytesValue }) "BytesValue should round-trip"

test "wrapper defaults encode to empty" := do
  let tests : List (String × ByteArray) := [
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
  for (name, bytes) in tests do
    ensure (bytes.size == 0) s!"{name} default should encode to 0 bytes, got {bytes.size}"

-- Registry tests
test "Timestamp is wellknown type" := do
  ensure (WellKnown.isWellKnownType "google.protobuf.Timestamp") "Timestamp should be a well-known type"

test "StringValue is wellknown type" := do
  ensure (WellKnown.isWellKnownType "google.protobuf.StringValue") "StringValue should be a well-known type"

test "custom type is not wellknown" := do
  ensure (!WellKnown.isWellKnownType "foo.bar.MyMessage") "foo.bar.MyMessage should NOT be a well-known type"

test "Duration has Lean name" := do
  match WellKnown.getLeanName "google.protobuf.Duration" with
  | some name =>
    ensure (name == "Google.Protobuf.Duration") s!"Expected 'Google.Protobuf.Duration', got '{name}'"
  | none => ensure false "Duration should have a Lean name"

test "timestamp proto is wellknown import" := do
  ensure (WellKnown.isWellKnownImport "google/protobuf/timestamp.proto") "google/protobuf/timestamp.proto should be a well-known import"

test "wrappers proto is wellknown import" := do
  ensure (WellKnown.isWellKnownImport "google/protobuf/wrappers.proto") "google/protobuf/wrappers.proto should be a well-known import"

test "custom proto is not wellknown import" := do
  ensure (!WellKnown.isWellKnownImport "myapp/messages.proto") "myapp/messages.proto should NOT be a well-known import"

#generate_tests

end Tests.WellKnown
