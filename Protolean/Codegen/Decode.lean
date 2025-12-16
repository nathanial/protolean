/-
  Code generation for decode instances.

  This module generates string-based Lean code for decoding ProtoMessage fields.
  The generated code can be copy-pasted or parsed into the file.
-/
import Lean
import Protolean.Syntax.AST
import Protolean.Codegen.Names
import Protolean.Codegen.Types

namespace Protolean.Codegen

open Lean
open Lean.Elab.Command
open Protolean.Syntax

/-- Generate decode expression string for a scalar field -/
def generateScalarDecodeStr (scalar : ScalarType) : String :=
  match scalar with
  | .int32 => "Protolean.Decoder.readInt32"
  | .int64 => "Protolean.Decoder.readInt64"
  | .uint32 => "Protolean.Decoder.readVarint32"
  | .uint64 => "Protolean.Decoder.readVarint"
  | .sint32 => "Protolean.Decoder.readSInt32"
  | .sint64 => "Protolean.Decoder.readSInt64"
  | .fixed32 => "Protolean.Decoder.readFixed32"
  | .sfixed32 => "Protolean.Decoder.readSFixed32"
  | .fixed64 => "Protolean.Decoder.readFixed64"
  | .sfixed64 => "Protolean.Decoder.readSFixed64"
  | .bool => "Protolean.Decoder.readBool"
  | .double => "Protolean.Decoder.readDouble"
  | .float => "Protolean.Decoder.readFloat"
  | .string => "Protolean.Decoder.readString"
  | .bytes => "Protolean.Decoder.readLengthDelimited"

/-- Generate wire type string for a scalar type -/
def scalarWireTypeStr (scalar : ScalarType) : String :=
  match scalar with
  | .double | .fixed64 | .sfixed64 => ".fixed64"
  | .float | .fixed32 | .sfixed32 => ".fixed32"
  | .string | .bytes => ".lengthDelimited"
  | _ => ".varint"

/-- Generate decode arm string for a field -/
def generateFieldDecodeArmStr (ctx : CodegenContext) (field : FieldDef) : String :=
  let fieldName := protoFieldToLean field.name
  let fieldNum := field.number

  let openBrace := "{"
  let closeBrace := "}"
  match field.cardinality with
  | .optional =>
    match field.fieldType with
    | .scalar s =>
      s!"| ⟨{fieldNum}, {scalarWireTypeStr s}⟩ => do let v ← {generateScalarDecodeStr s}; pure {openBrace} msg with {fieldName} := some v {closeBrace}"
    | .named _ =>
      s!"| ⟨{fieldNum}, .lengthDelimited⟩ => do let v ← Protolean.Decoder.readLengthDelimited; pure {openBrace} msg with {fieldName} := some v {closeBrace}"
    | .map _ _ =>
      s!"| ⟨{fieldNum}, .lengthDelimited⟩ => do Protolean.Decoder.skipField .lengthDelimited; pure msg"
  | .repeated =>
    match field.fieldType with
    | .scalar s =>
      s!"| ⟨{fieldNum}, {scalarWireTypeStr s}⟩ => do let v ← {generateScalarDecodeStr s}; pure {openBrace} msg with {fieldName} := msg.{fieldName}.push v {closeBrace}"
    | _ =>
      s!"| ⟨{fieldNum}, .lengthDelimited⟩ => do Protolean.Decoder.skipField .lengthDelimited; pure msg"
  | .singular =>
    match field.fieldType with
    | .scalar s =>
      s!"| ⟨{fieldNum}, {scalarWireTypeStr s}⟩ => do let v ← {generateScalarDecodeStr s}; pure {openBrace} msg with {fieldName} := v {closeBrace}"
    | .named _ =>
      s!"| ⟨{fieldNum}, .lengthDelimited⟩ => do Protolean.Decoder.skipField .lengthDelimited; pure msg"
    | .map _ _ =>
      s!"| ⟨{fieldNum}, .lengthDelimited⟩ => do Protolean.Decoder.skipField .lengthDelimited; pure msg"

/-- Generate decode field function string -/
def generateDecodeFieldStr (ctx : CodegenContext) (msg : MessageDef) : String :=
  -- Collect regular fields (not in oneofs)
  let oneofFieldNames := msg.oneofs.flatMap (·.fields.map (·.name))
  let regularFields := msg.fields.filter fun f => !oneofFieldNames.contains f.name

  -- Generate match arms for each field
  let fieldArms := regularFields.map (generateFieldDecodeArmStr ctx)
  let fallbackArm := "| _ => do Protolean.Decoder.skipField tag.wireType; pure msg"

  let allArms := fieldArms ++ [fallbackArm]
  "\n    ".intercalate allArms

/-- Generate decodeField method body string for ProtoMessage instance -/
def generateDecodeInstanceStr (ctx : CodegenContext) (msg : MessageDef) : String :=
  let typeName := protoTypeToLean msg.name
  let decodeBody := generateDecodeFieldStr ctx msg

  s!"-- Decode field for {typeName}
decodeField msg tag := match tag with
    {decodeBody}"

/-- Generate and log decode instance for a message -/
def elaborateDecodeInstance (ctx : CodegenContext) (msg : MessageDef) : CommandElabM Unit := do
  let code := generateDecodeInstanceStr ctx msg
  logInfo s!"Generated decode method:\n{code}"

end Protolean.Codegen
