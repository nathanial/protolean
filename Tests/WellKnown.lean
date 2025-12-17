/-
  Tests for well-known Google Protobuf types.
-/
import Protolean

namespace Tests.WellKnown

open Google.Protobuf
open Protolean

-- Helper to test round-trip encode/decode
def testRoundTrip [ProtoMessage α] [BEq α] [Repr α] (name : String) (msg : α) : IO Bool := do
  let bytes := Protolean.encodeMessage msg
  match Protolean.decodeMessage bytes with
  | .ok decoded =>
    if decoded == msg then
      IO.println s!"  PASS: {name} round-trip"
      return true
    else
      IO.println s!"  FAIL: {name} round-trip mismatch"
      IO.println s!"    Original: {repr msg}"
      IO.println s!"    Decoded:  {repr decoded}"
      return false
  | .error e =>
    IO.println s!"  FAIL: {name} decode error: {e}"
    return false

-- Test Empty type
def testEmpty : IO Bool := do
  testRoundTrip "Empty" (Empty.mk)

-- Test Timestamp type
def testTimestamp : IO Bool := do
  let ts := Timestamp.fromSecondsNanos 1702857600 500000000
  testRoundTrip "Timestamp" ts

def testTimestampZero : IO Bool := do
  let ts : Timestamp := {}
  let bytes := encodeMessage ts
  if bytes.size != 0 then
    IO.println s!"  FAIL: Empty timestamp should encode to zero bytes, got {bytes.size}"
    return false
  IO.println "  PASS: Timestamp zero value"
  return true

def testTimestampHelpers : IO Bool := do
  let ts := Timestamp.fromSeconds 12345
  if ts.seconds != 12345 || ts.nanos != 0 then
    IO.println "  FAIL: Timestamp.fromSeconds"
    return false
  if !ts.isValid then
    IO.println "  FAIL: Timestamp.isValid should be true"
    return false
  let invalidTs := { seconds := 100, nanos := -1 : Timestamp }
  if invalidTs.isValid then
    IO.println "  FAIL: Invalid timestamp should not pass isValid"
    return false
  IO.println "  PASS: Timestamp helpers"
  return true

-- Test Duration type
def testDuration : IO Bool := do
  let d := Duration.fromSecondsNanos 120 500000000
  testRoundTrip "Duration" d

def testDurationZero : IO Bool := do
  let d : Duration := {}
  let bytes := encodeMessage d
  if bytes.size != 0 then
    IO.println s!"  FAIL: Empty duration should encode to zero bytes, got {bytes.size}"
    return false
  IO.println "  PASS: Duration zero value"
  return true

def testDurationHelpers : IO Bool := do
  let d1 := Duration.fromSeconds 10
  let d2 := Duration.fromMillis 2500  -- 2.5 seconds
  if d1.seconds != 10 || d1.nanos != 0 then
    IO.println "  FAIL: Duration.fromSeconds"
    return false
  if d2.seconds != 2 || d2.nanos != 500000000 then
    IO.println s!"  FAIL: Duration.fromMillis got {d2.seconds}s {d2.nanos}ns"
    return false
  -- Test negation
  let negD := -d1
  if negD.seconds != -10 || negD.nanos != 0 then
    IO.println "  FAIL: Duration negation"
    return false
  -- Test addition
  let sum := d1 + d2
  if sum.seconds != 12 || sum.nanos != 500000000 then
    IO.println s!"  FAIL: Duration add got {sum.seconds}s {sum.nanos}ns"
    return false
  IO.println "  PASS: Duration helpers"
  return true

-- Test wrapper types
def testDoubleValue : IO Bool := do
  testRoundTrip "DoubleValue" { value := 3.14159 : DoubleValue }

def testFloatValue : IO Bool := do
  -- Use a value exactly representable in Float32 to avoid precision issues
  testRoundTrip "FloatValue" { value := 0.5 : FloatValue }

def testInt64Value : IO Bool := do
  testRoundTrip "Int64Value" { value := -9223372036854775808 : Int64Value }

def testUInt64Value : IO Bool := do
  testRoundTrip "UInt64Value" { value := 18446744073709551615 : UInt64Value }

def testInt32Value : IO Bool := do
  testRoundTrip "Int32Value" { value := -2147483648 : Int32Value }

def testUInt32Value : IO Bool := do
  testRoundTrip "UInt32Value" { value := 4294967295 : UInt32Value }

def testBoolValue : IO Bool := do
  if !(← testRoundTrip "BoolValue(true)" { value := true : BoolValue }) then
    return false
  testRoundTrip "BoolValue(false)" { value := false : BoolValue }

def testStringValue : IO Bool := do
  testRoundTrip "StringValue" { value := "Hello, Protocol Buffers!" : StringValue }

def testBytesValue : IO Bool := do
  let bytes := ByteArray.mk #[0x01, 0x02, 0x03, 0xFF]
  testRoundTrip "BytesValue" { value := bytes : BytesValue }

-- Test default values (zero values should encode to empty)
def testWrapperDefaults : IO Bool := do
  -- All default wrapper values should encode to zero bytes
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
    if bytes.size != 0 then
      IO.println s!"  FAIL: {name} default should encode to 0 bytes, got {bytes.size}"
      return false
  IO.println "  PASS: All wrapper defaults encode to empty"
  return true

-- Test registry
def testRegistry : IO Bool := do
  if !WellKnown.isWellKnownType "google.protobuf.Timestamp" then
    IO.println "  FAIL: Timestamp should be a well-known type"
    return false
  if !WellKnown.isWellKnownType "google.protobuf.StringValue" then
    IO.println "  FAIL: StringValue should be a well-known type"
    return false
  if WellKnown.isWellKnownType "foo.bar.MyMessage" then
    IO.println "  FAIL: foo.bar.MyMessage should NOT be a well-known type"
    return false
  match WellKnown.getLeanName "google.protobuf.Duration" with
  | some name =>
    if name != "Google.Protobuf.Duration" then
      IO.println s!"  FAIL: Expected 'Google.Protobuf.Duration', got '{name}'"
      return false
  | none =>
    IO.println "  FAIL: Duration should have a Lean name"
    return false
  IO.println "  PASS: Registry lookups"
  return true

def testWellKnownImport : IO Bool := do
  if !WellKnown.isWellKnownImport "google/protobuf/timestamp.proto" then
    IO.println "  FAIL: google/protobuf/timestamp.proto should be a well-known import"
    return false
  if !WellKnown.isWellKnownImport "google/protobuf/wrappers.proto" then
    IO.println "  FAIL: google/protobuf/wrappers.proto should be a well-known import"
    return false
  if WellKnown.isWellKnownImport "myapp/messages.proto" then
    IO.println "  FAIL: myapp/messages.proto should NOT be a well-known import"
    return false
  IO.println "  PASS: Well-known import detection"
  return true

/-- Run all well-known type tests -/
def runTests : IO Unit := do
  let mut passed := 0
  let mut failed := 0

  IO.println "Testing Empty..."
  if ← testEmpty then passed := passed + 1 else failed := failed + 1

  IO.println "Testing Timestamp..."
  if ← testTimestamp then passed := passed + 1 else failed := failed + 1
  if ← testTimestampZero then passed := passed + 1 else failed := failed + 1
  if ← testTimestampHelpers then passed := passed + 1 else failed := failed + 1

  IO.println "Testing Duration..."
  if ← testDuration then passed := passed + 1 else failed := failed + 1
  if ← testDurationZero then passed := passed + 1 else failed := failed + 1
  if ← testDurationHelpers then passed := passed + 1 else failed := failed + 1

  IO.println "Testing Wrapper types..."
  if ← testDoubleValue then passed := passed + 1 else failed := failed + 1
  if ← testFloatValue then passed := passed + 1 else failed := failed + 1
  if ← testInt64Value then passed := passed + 1 else failed := failed + 1
  if ← testUInt64Value then passed := passed + 1 else failed := failed + 1
  if ← testInt32Value then passed := passed + 1 else failed := failed + 1
  if ← testUInt32Value then passed := passed + 1 else failed := failed + 1
  if ← testBoolValue then passed := passed + 1 else failed := failed + 1
  if ← testStringValue then passed := passed + 1 else failed := failed + 1
  if ← testBytesValue then passed := passed + 1 else failed := failed + 1
  if ← testWrapperDefaults then passed := passed + 1 else failed := failed + 1

  IO.println "Testing Registry..."
  if ← testRegistry then passed := passed + 1 else failed := failed + 1
  if ← testWellKnownImport then passed := passed + 1 else failed := failed + 1

  IO.println s!"Well-known tests: {passed} passed, {failed} failed"

end Tests.WellKnown
