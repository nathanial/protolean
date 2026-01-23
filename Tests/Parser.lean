/-
  Unit tests for the Proto3 parser.
-/
import Crucible
import Protolean.Parser.Proto
import Protolean.Syntax.AST
import Protolean.Codegen.Names

namespace Tests.Parser

open Crucible
open Protolean.Parser
open Protolean.Syntax
open Protolean.Codegen

testSuite "Parser Tests"

test "parse simple message" := do
  let input := "
syntax = \"proto3\";

message Person {
  string name = 1;
  int32 age = 2;
}
"
  match parse input with
  | .ok file => do
    file.protoSyntax ≡ "proto3"
    file.definitions.length ≡ 1
    match file.definitions.head? with
    | some (.message msg) => do
      msg.name ≡ "Person"
      msg.fields.length ≡ 2
    | _ => ensure false "expected message definition"
  | .error _ => ensure false "parse failed"

test "parse enum" := do
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
    | some (.enum e) => do
      e.name ≡ "Status"
      e.values.length ≡ 3
    | _ => ensure false "expected enum definition"
  | .error _ => ensure false "parse failed"

test "parse all field types" := do
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
    | some (.message msg) => msg.fields.length ≡ 15
    | _ => ensure false "expected message definition"
  | .error _ => ensure false "parse failed"

test "parse repeated fields" := do
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
      let allRepeated := msg.fields.all (·.cardinality == .repeated)
      allRepeated ≡ true
    | _ => ensure false "expected message definition"
  | .error _ => ensure false "parse failed"

test "parse oneof" := do
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
    | some (.message msg) => do
      msg.oneofs.length ≡ 1
      match msg.oneofs.head? with
      | some oneof => do
        oneof.name ≡ "contact_type"
        oneof.fields.length ≡ 2
      | none => ensure false "no oneof found"
    | _ => ensure false "expected message definition"
  | .error _ => ensure false "parse failed"

test "parse map field" := do
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
        | .map _ _ => pure ()
        | _ => ensure false "expected map field type"
      | none => ensure false "no fields found"
    | _ => ensure false "expected message definition"
  | .error _ => ensure false "parse failed"

test "parse nested message" := do
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
    | some (.message msg) => msg.nestedMessages.length ≡ 1
    | _ => ensure false "expected message definition"
  | .error _ => ensure false "parse failed"

test "parse imports and package" := do
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
  | .ok file => do
    file.package.isSome ≡ true
    match file.package with
    | some pkg => pkg.parts ≡ ["example", "v1"]
    | none => pure ()
    file.imports.length ≡ 2
  | .error _ => ensure false "parse failed"

test "parse top level option" := do
  let input := "
syntax = \"proto3\";

option java_package = \"com.example\";

message Foo {
  string name = 1;
}
"
  match parse input with
  | .ok file => do
    file.options.length ≡ 1
    match file.options.head? with
    | some opt => opt.name ≡ "java_package"
    | none => ensure false "no options found"
  | .error _ => ensure false "parse failed"

test "parse go package option" := do
  let input := "
syntax = \"proto3\";

option go_package = \"github.com/example/pb\";

message Foo {
  string name = 1;
}
"
  match parse input with
  | .ok file => do
    file.options.length ≡ 1
    match file.options.head? with
    | some opt => do
      opt.name ≡ "go_package"
      match opt.value with
      | .string s => s ≡ "github.com/example/pb"
      | _ => ensure false "option value should be string"
    | none => ensure false "no options found"
  | .error _ => ensure false "parse failed"

test "parse simple service" := do
  let input := "
syntax = \"proto3\";

message Request {
  string query = 1;
}

message Response {
  string result = 1;
}

service SearchService {
  rpc Search(Request) returns (Response);
}
"
  match parse input with
  | .ok file => do
    file.definitions.length ≡ 3
    match file.definitions[2]? with
    | some (TopLevelDef.service svc) => do
      svc.name ≡ "SearchService"
      svc.methods.length ≡ 1
      match svc.methods.head? with
      | some m => m.name ≡ "Search"
      | none => ensure false "no methods found"
    | _ => ensure false "expected service definition"
  | .error _ => ensure false "parse failed"

test "parse streaming RPCs" := do
  let input := "
syntax = \"proto3\";

message Msg {
  string data = 1;
}

service StreamService {
  rpc ClientStream(stream Msg) returns (Msg);
  rpc ServerStream(Msg) returns (stream Msg);
  rpc BidiStream(stream Msg) returns (stream Msg);
}
"
  match parse input with
  | .ok file =>
    match file.definitions[1]? with
    | some (TopLevelDef.service svc) => do
      svc.methods.length ≡ 3
      -- Check ClientStream
      match svc.methods[0]? with
      | some m => do
        m.inputStream ≡ true
        m.outputStream ≡ false
      | none => ensure false "ClientStream not found"
      -- Check ServerStream
      match svc.methods[1]? with
      | some m => do
        m.inputStream ≡ false
        m.outputStream ≡ true
      | none => ensure false "ServerStream not found"
      -- Check BidiStream
      match svc.methods[2]? with
      | some m => do
        m.inputStream ≡ true
        m.outputStream ≡ true
      | none => ensure false "BidiStream not found"
    | _ => ensure false "expected service definition"
  | .error _ => ensure false "parse failed"

test "keyword escaping" := do
  -- Test that reserved keywords get escaped with underscore
  let testCases := [
    ("prefix", "prefix_"),
    ("infix", "infix_"),
    ("notation", "notation_"),
    ("def", "def_"),
    ("where", "where_"),
    ("match", "match_"),
    -- Non-keywords should not be escaped
    ("name", "name"),
    ("value", "value"),
    ("count", "count"),
    -- Snake_case conversion with keyword escaping
    ("in_progress", "inProgress"),
    ("prefix_value", "prefixValue")
  ]
  for (input, expected) in testCases do
    let result := protoFieldToLean input
    result ≡ expected



end Tests.Parser
