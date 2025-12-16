/-
  Code generation for encode instances.

  This module generates string-based Lean code for ProtoMessage instances.
  The generated code can be copy-pasted or parsed into the file.
-/
import Lean
import Protolean.Syntax.AST
import Protolean.Codegen.Names
import Protolean.Codegen.Types
import Protolean.Codegen.Decode

namespace Protolean.Codegen

open Lean
open Lean.Elab.Command
open Protolean.Syntax

/-- Generate encode expression string for a scalar field -/
def generateScalarEncodeStr (scalar : ScalarType) (fieldNum : Nat) (fieldExpr : String) : String :=
  match scalar with
  | .int32 | .int64 | .uint32 | .uint64 | .bool =>
    s!"Protolean.encodeField {fieldNum} {fieldExpr}"
  | .sint32 =>
    s!"(if {fieldExpr} == 0 then pure () else do Protolean.Encoder.emitTag {fieldNum} .varint; Protolean.Encoder.emitSInt32 {fieldExpr})"
  | .sint64 =>
    s!"(if {fieldExpr} == 0 then pure () else do Protolean.Encoder.emitTag {fieldNum} .varint; Protolean.Encoder.emitSInt64 {fieldExpr})"
  | .fixed32 | .sfixed32 =>
    s!"(if {fieldExpr} == 0 then pure () else do Protolean.Encoder.emitTag {fieldNum} .fixed32; Protolean.Encoder.emitFixed32 {fieldExpr}.toUInt32)"
  | .fixed64 | .sfixed64 =>
    s!"(if {fieldExpr} == 0 then pure () else do Protolean.Encoder.emitTag {fieldNum} .fixed64; Protolean.Encoder.emitFixed64 {fieldExpr}.toUInt64)"
  | .double =>
    s!"(if {fieldExpr} == 0.0 then pure () else do Protolean.Encoder.emitTag {fieldNum} .fixed64; Protolean.Encoder.emitDouble {fieldExpr})"
  | .float =>
    s!"(if {fieldExpr} == 0.0 then pure () else do Protolean.Encoder.emitTag {fieldNum} .fixed32; Protolean.Encoder.emitFloat {fieldExpr})"
  | .string =>
    s!"(if {fieldExpr}.isEmpty then pure () else do Protolean.Encoder.emitTag {fieldNum} .lengthDelimited; Protolean.Encoder.emitString {fieldExpr})"
  | .bytes =>
    s!"(if {fieldExpr}.isEmpty then pure () else do Protolean.Encoder.emitTag {fieldNum} .lengthDelimited; Protolean.Encoder.emitLengthDelimited {fieldExpr})"

/-- Generate encode expression string for a field -/
def generateFieldEncodeStr (ctx : CodegenContext) (field : FieldDef) : String :=
  let fieldName := protoFieldToLean field.name
  let fieldExpr := s!"msg.{fieldName}"

  match field.cardinality with
  | .optional =>
    s!"(match {fieldExpr} with | none => pure () | some v => Protolean.encodeFieldAlways {field.number} v)"
  | .repeated =>
    s!"(for v in {fieldExpr} do Protolean.encodeFieldAlways {field.number} v)"
  | .singular =>
    match field.fieldType with
    | .scalar s => generateScalarEncodeStr s field.number fieldExpr
    | .named _ => s!"Protolean.encodeField {field.number} {fieldExpr}"
    | .map _ _ => s!"Protolean.encodeHashMap {field.number} {fieldExpr}"

/-- Generate encode string for ProtoMessage instance -/
def generateEncodeInstanceStr (ctx : CodegenContext) (msg : MessageDef) : String :=
  let typeName := protoTypeToLean msg.name

  -- Collect regular fields (not in oneofs)
  let oneofFieldNames := msg.oneofs.flatMap (·.fields.map (·.name))
  let regularFields := msg.fields.filter fun f => !oneofFieldNames.contains f.name

  -- Generate encode expressions for each field
  let fieldEncodes := regularFields.map (generateFieldEncodeStr ctx)
  let encodeBody := if fieldEncodes.isEmpty then "pure ()"
                    else "; ".intercalate fieldEncodes

  -- Generate decode arms for each field
  let decodeBody := generateDecodeFieldStr ctx msg

  s!"instance : Protolean.ProtoMessage {typeName} where
  encodeFields msg := do
    {encodeBody}
  decodeField msg tag := match tag with
    {decodeBody}
  defaultValue := default"

/-- Generate and log encode instance for a message -/
def elaborateEncodeInstance (ctx : CodegenContext) (msg : MessageDef) : CommandElabM Unit := do
  let code := generateEncodeInstanceStr ctx msg
  logInfo s!"Generated encode instance:\n{code}"

end Protolean.Codegen
