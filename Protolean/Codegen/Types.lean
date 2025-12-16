/-
  Code generation for Lean types from proto definitions.

  This module generates Lean type declarations from proto file AST.
  Due to complexity of syntax quotation for dynamic structure generation,
  we use a string-based approach parsed via the parser.
-/
import Lean
import Protolean.Syntax.AST
import Protolean.Codegen.Names

namespace Protolean.Codegen

open Lean
open Lean.Elab
open Lean.Elab.Command
open Protolean.Syntax

/-- Context for code generation -/
structure CodegenContext where
  /-- Current namespace path -/
  nsPath : List String := []
  /-- Type registry for resolving type references -/
  registry : Std.HashMap String Name := {}

/-- Get Lean type name string for a scalar type -/
def scalarTypeToString (scalar : ScalarType) : String :=
  match scalar with
  | .double | .float => "Float"
  | .int32 | .sint32 | .sfixed32 => "Int32"
  | .int64 | .sint64 | .sfixed64 => "Int64"
  | .uint32 | .fixed32 => "UInt32"
  | .uint64 | .fixed64 => "UInt64"
  | .bool => "Bool"
  | .string => "String"
  | .bytes => "ByteArray"

/-- Get Lean type string for a field type -/
partial def fieldTypeToString (ctx : CodegenContext) (ft : FieldType) : String :=
  match ft with
  | .scalar s => scalarTypeToString s
  | .named ident =>
    let name := ident.toString
    match ctx.registry.get? name with
    | some n => n.toString
    | none => protoFullNameToLeanString ident.parts
  | .map keyType valueType =>
    let keyTypeStr := scalarTypeToString (mapKeyToScalar keyType)
    let valueTypeStr := fieldTypeToString ctx valueType
    s!"Std.HashMap {keyTypeStr} {valueTypeStr}"
where
  mapKeyToScalar : MapKeyType → ScalarType
    | .int32 => .int32
    | .int64 => .int64
    | .uint32 => .uint32
    | .uint64 => .uint64
    | .sint32 => .sint32
    | .sint64 => .sint64
    | .fixed32 => .fixed32
    | .fixed64 => .fixed64
    | .sfixed32 => .sfixed32
    | .sfixed64 => .sfixed64
    | .bool => .bool
    | .string => .string

/-- Get Lean type string for a field with cardinality -/
def fieldDefToTypeString (ctx : CodegenContext) (field : FieldDef) : String :=
  let baseType := fieldTypeToString ctx field.fieldType
  match field.cardinality with
  | .singular => baseType
  | .optional => s!"Option {baseType}"
  | .repeated => s!"Array {baseType}"

/-- Get default value string for a field -/
def getDefaultValueString (_ctx : CodegenContext) (field : FieldDef) : String :=
  match field.cardinality with
  | .optional => "none"
  | .repeated => "#[]"
  | .singular =>
    match field.fieldType with
    | .scalar s => scalarDefaultValue s
    | .named _ => "default"
    | .map _ _ => "Std.HashMap.empty"
where
  scalarDefaultValue : ScalarType → String
    | .double | .float => "0.0"
    | .int32 | .sint32 | .sfixed32 => "(0 : Int32)"
    | .int64 | .sint64 | .sfixed64 => "(0 : Int64)"
    | .uint32 | .fixed32 => "(0 : UInt32)"
    | .uint64 | .fixed64 => "(0 : UInt64)"
    | .bool => "false"
    | .string => "\"\""
    | .bytes => "ByteArray.empty"

/-- Generate a structure field line as string -/
def generateFieldLine (ctx : CodegenContext) (field : FieldDef) : String :=
  let fieldName := protoFieldToLean field.name
  let fieldType := fieldDefToTypeString ctx field
  let defaultVal := getDefaultValueString ctx field
  s!"  {fieldName} : {fieldType} := {defaultVal}"

/-- Generate enum definition as string -/
def generateEnumString (_ctx : CodegenContext) (enum : EnumDef) : String :=
  let typeName := protoTypeToLean enum.name
  let ctors := enum.values.map fun value =>
    s!"  | {protoEnumValueToLean value.name}"
  let ctorLines := "\n".intercalate ctors
  s!"inductive {typeName} where\n{ctorLines}\n  deriving Repr, BEq, Inhabited"

/-- Generate oneof inductive as string -/
def generateOneofString (ctx : CodegenContext) (_msgName : String) (oneof : OneofDef) : String :=
  let typeName := protoTypeToLean oneof.name
  let ctors := oneof.fields.map fun field =>
    let ctorName := protoFieldToLean field.name
    let fieldType := fieldTypeToString ctx field.fieldType
    s!"  | {ctorName} : {fieldType} → {typeName}"
  let ctorLines := "\n".intercalate ctors
  s!"inductive {typeName} where\n{ctorLines}\n  deriving Repr, BEq, Inhabited"

/-- Generate structure as string -/
partial def generateMessageString (ctx : CodegenContext) (msg : MessageDef) : String :=
  let typeName := protoTypeToLean msg.name

  -- Collect nested definitions
  let enumParts := msg.elements.filterMap fun
    | .enum e => some (generateEnumString ctx e)
    | _ => none

  let nestedMsgParts := msg.elements.filterMap fun
    | .message nested => some (generateMessageString ctx nested)
    | _ => none

  let oneofParts := msg.elements.filterMap fun
    | .oneof o => some (generateOneofString ctx msg.name o)
    | _ => none

  let parts := enumParts ++ nestedMsgParts ++ oneofParts

  -- Collect regular fields (not in oneofs)
  let oneofFieldNames := msg.oneofs.flatMap (·.fields.map (·.name))
  let regularFields := msg.fields.filter fun f => !oneofFieldNames.contains f.name

  -- Generate structure fields
  let fieldLines := regularFields.map (generateFieldLine ctx)

  -- Add oneof fields as Option of the sum type
  let oneofLines := msg.oneofs.map fun oneof =>
    let fieldName := protoFieldToLean oneof.name
    let typeName := protoTypeToLean oneof.name
    s!"  {fieldName} : Option {typeName} := none"

  let allFieldLines := fieldLines ++ oneofLines
  let fieldsStr := if allFieldLines.isEmpty then "" else "\n" ++ "\n".intercalate allFieldLines

  let structStr := s!"structure {typeName} where{fieldsStr}\n  deriving Repr, BEq, Inhabited"

  if parts.isEmpty then
    structStr
  else
    "\n".intercalate (parts ++ [structStr])

/-- Parse and elaborate a command string.
    Note: For now, we'll log the generated code. Full parsing/elaboration
    requires more complex setup with the Parser API. -/
def elabCommandString (s : String) : CommandElabM Unit := do
  -- Log the generated code for debugging/manual verification
  -- A full implementation would parse and elaborate the string
  logInfo s!"Generated Lean code:\n{s}"
  -- TODO: Implement proper parsing when needed
  -- The generated string can be copy-pasted into a Lean file

/-- Generate and elaborate all declarations for a message -/
def elaborateMessage (ctx : CodegenContext) (msg : MessageDef) : CommandElabM Unit := do
  let code := generateMessageString ctx msg
  elabCommandString code

/-- Generate and elaborate all declarations for an enum -/
def elaborateEnum (ctx : CodegenContext) (enum : EnumDef) : CommandElabM Unit := do
  let code := generateEnumString ctx enum
  elabCommandString code

end Protolean.Codegen
