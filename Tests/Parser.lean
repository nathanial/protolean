/-
  Unit tests for the Proto3 parser.
-/
import Protolean.Parser.Proto
import Protolean.Syntax.AST

namespace Tests.Parser

open Protolean.Parser
open Protolean.Syntax

/-- Test parsing a simple message -/
def testSimpleMessage : IO Bool := do
  let input := "
syntax = \"proto3\";

message Person {
  string name = 1;
  int32 age = 2;
}
"
  match parse input with
  | .ok file =>
    if file.protoSyntax != "proto3" then
      IO.println "  FAIL: syntax should be proto3"
      return false
    if file.definitions.length != 1 then
      IO.println s!"  FAIL: expected 1 definition, got {file.definitions.length}"
      return false
    match file.definitions.head? with
    | some (.message msg) =>
      if msg.name != "Person" then
        IO.println s!"  FAIL: message name should be Person, got {msg.name}"
        return false
      if msg.fields.length != 2 then
        IO.println s!"  FAIL: expected 2 fields, got {msg.fields.length}"
        return false
      return true
    | _ =>
      IO.println "  FAIL: expected message definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing an enum -/
def testEnum : IO Bool := do
  let input := "
syntax = \"proto3\";

enum Status {
  STATUS_UNKNOWN = 0;
  STATUS_ACTIVE = 1;
  STATUS_INACTIVE = 2;
}
"
  match parse input with
  | .ok file =>
    match file.definitions.head? with
    | some (.enum e) =>
      if e.name != "Status" then
        IO.println s!"  FAIL: enum name should be Status, got {e.name}"
        return false
      if e.values.length != 3 then
        IO.println s!"  FAIL: expected 3 values, got {e.values.length}"
        return false
      return true
    | _ =>
      IO.println "  FAIL: expected enum definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing a message with various field types -/
def testFieldTypes : IO Bool := do
  let input := "
syntax = \"proto3\";

message AllTypes {
  double double_field = 1;
  float float_field = 2;
  int32 int32_field = 3;
  int64 int64_field = 4;
  uint32 uint32_field = 5;
  uint64 uint64_field = 6;
  sint32 sint32_field = 7;
  sint64 sint64_field = 8;
  fixed32 fixed32_field = 9;
  fixed64 fixed64_field = 10;
  sfixed32 sfixed32_field = 11;
  sfixed64 sfixed64_field = 12;
  bool bool_field = 13;
  string string_field = 14;
  bytes bytes_field = 15;
}
"
  match parse input with
  | .ok file =>
    match file.definitions.head? with
    | some (.message msg) =>
      if msg.fields.length != 15 then
        IO.println s!"  FAIL: expected 15 fields, got {msg.fields.length}"
        return false
      return true
    | _ =>
      IO.println "  FAIL: expected message definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing repeated fields -/
def testRepeatedFields : IO Bool := do
  let input := "
syntax = \"proto3\";

message Container {
  repeated string items = 1;
  repeated int32 numbers = 2;
}
"
  match parse input with
  | .ok file =>
    match file.definitions.head? with
    | some (.message msg) =>
      for field in msg.fields do
        if field.cardinality != .repeated then
          IO.println s!"  FAIL: field {field.name} should be repeated"
          return false
      return true
    | _ =>
      IO.println "  FAIL: expected message definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing a oneof -/
def testOneof : IO Bool := do
  let input := "
syntax = \"proto3\";

message Contact {
  oneof contact_type {
    string email = 1;
    string phone = 2;
  }
}
"
  match parse input with
  | .ok file =>
    match file.definitions.head? with
    | some (.message msg) =>
      if msg.oneofs.length != 1 then
        IO.println s!"  FAIL: expected 1 oneof, got {msg.oneofs.length}"
        return false
      match msg.oneofs.head? with
      | some oneof =>
        if oneof.name != "contact_type" then
          IO.println s!"  FAIL: oneof name should be contact_type, got {oneof.name}"
          return false
        if oneof.fields.length != 2 then
          IO.println s!"  FAIL: expected 2 fields in oneof, got {oneof.fields.length}"
          return false
        return true
      | none =>
        IO.println "  FAIL: no oneof found"
        return false
    | _ =>
      IO.println "  FAIL: expected message definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing a map field -/
def testMapField : IO Bool := do
  let input := "
syntax = \"proto3\";

message Dictionary {
  map<string, int32> entries = 1;
}
"
  match parse input with
  | .ok file =>
    match file.definitions.head? with
    | some (.message msg) =>
      match msg.fields.head? with
      | some field =>
        match field.fieldType with
        | .map _ _ => return true
        | _ =>
          IO.println "  FAIL: expected map field type"
          return false
      | none =>
        IO.println "  FAIL: no fields found"
        return false
    | _ =>
      IO.println "  FAIL: expected message definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing nested messages -/
def testNestedMessage : IO Bool := do
  let input := "
syntax = \"proto3\";

message Outer {
  message Inner {
    string value = 1;
  }
  Inner inner = 1;
}
"
  match parse input with
  | .ok file =>
    match file.definitions.head? with
    | some (.message msg) =>
      if msg.nestedMessages.length != 1 then
        IO.println s!"  FAIL: expected 1 nested message, got {msg.nestedMessages.length}"
        return false
      return true
    | _ =>
      IO.println "  FAIL: expected message definition"
      return false
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Test parsing imports and package -/
def testImportsAndPackage : IO Bool := do
  let input := "
syntax = \"proto3\";

package example.v1;

import \"google/protobuf/timestamp.proto\";
import public \"other.proto\";

message MyMessage {
  string name = 1;
}
"
  match parse input with
  | .ok file =>
    if file.package.isNone then
      IO.println "  FAIL: expected package"
      return false
    match file.package with
    | some pkg =>
      if pkg.parts != ["example", "v1"] then
        IO.println s!"  FAIL: package should be example.v1, got {pkg.parts}"
        return false
    | none => pure ()
    if file.imports.length != 2 then
      IO.println s!"  FAIL: expected 2 imports, got {file.imports.length}"
      return false
    return true
  | .error e =>
    IO.println s!"  FAIL: parse error: {e}"
    return false

/-- Run all parser tests -/
def runTests : IO Unit := do
  let mut passed := 0
  let mut failed := 0

  IO.println "Testing simple message..."
  if ← testSimpleMessage then passed := passed + 1 else failed := failed + 1

  IO.println "Testing enum..."
  if ← testEnum then passed := passed + 1 else failed := failed + 1

  IO.println "Testing field types..."
  if ← testFieldTypes then passed := passed + 1 else failed := failed + 1

  IO.println "Testing repeated fields..."
  if ← testRepeatedFields then passed := passed + 1 else failed := failed + 1

  IO.println "Testing oneof..."
  if ← testOneof then passed := passed + 1 else failed := failed + 1

  IO.println "Testing map field..."
  if ← testMapField then passed := passed + 1 else failed := failed + 1

  IO.println "Testing nested message..."
  if ← testNestedMessage then passed := passed + 1 else failed := failed + 1

  IO.println "Testing imports and package..."
  if ← testImportsAndPackage then passed := passed + 1 else failed := failed + 1

  IO.println s!"Parser tests: {passed} passed, {failed} failed"

end Tests.Parser
