/-
  Complete Proto3 parser using Sift.
-/
import Protolean.Parser.Lexer
import Protolean.Syntax.AST

namespace Protolean.Parser

open Sift
open Protolean.Syntax

/-- Parse a scalar type keyword -/
def scalarType : Parser Unit ScalarType := do
  let id ← ident
  match id with
  | "double" => pure .double
  | "float" => pure .float
  | "int32" => pure .int32
  | "int64" => pure .int64
  | "uint32" => pure .uint32
  | "uint64" => pure .uint64
  | "sint32" => pure .sint32
  | "sint64" => pure .sint64
  | "fixed32" => pure .fixed32
  | "fixed64" => pure .fixed64
  | "sfixed32" => pure .sfixed32
  | "sfixed64" => pure .sfixed64
  | "bool" => pure .bool
  | "string" => pure .string
  | "bytes" => pure .bytes
  | _ => Parser.fail s!"expected scalar type, got '{id}'"

/-- Parse a map key type -/
def mapKeyType : Parser Unit MapKeyType := do
  let id ← ident
  match id with
  | "int32" => pure .int32
  | "int64" => pure .int64
  | "uint32" => pure .uint32
  | "uint64" => pure .uint64
  | "sint32" => pure .sint32
  | "sint64" => pure .sint64
  | "fixed32" => pure .fixed32
  | "fixed64" => pure .fixed64
  | "sfixed32" => pure .sfixed32
  | "sfixed64" => pure .sfixed64
  | "bool" => pure .bool
  | "string" => pure .string
  | _ => Parser.fail s!"invalid map key type: '{id}'"

/-- Parse a field type (scalar, map, or named reference) -/
partial def fieldType : Parser Unit FieldType := do
  (attempt mapType) <|>
  (attempt do pure (FieldType.scalar (← scalarType))) <|>
  (attempt do
    let fid ← fullIdent
    pure (FieldType.named ⟨fid⟩))
where
  /-- Parse a map type: map<KeyType, ValueType> -/
  mapType : Parser Unit FieldType := do
    keyword "map"
    symbol "<"
    let keyT ← mapKeyType
    symbol ","
    let valT ← fieldType
    symbol ">"
    pure (FieldType.map keyT valT)

/-- Parse an option value -/
def optionValue : Parser Unit OptionValue := do
  (attempt do pure (OptionValue.string (← stringLit))) <|>
  (attempt do
    let f ← floatLit
    pure (OptionValue.float f)) <|>
  (attempt do pure (OptionValue.int (← intLit))) <|>
  (attempt do
    let id ← ident
    if id == "true" then pure (OptionValue.bool true)
    else if id == "false" then pure (OptionValue.bool false)
    else pure (OptionValue.ident ⟨[id]⟩))

/-- Parse field options: [ option, option, ... ] -/
def fieldOptions : Parser Unit (List ProtoOption) := do
  (attempt do
    symbol "["
    let opts ← sepBy1 fieldOption (symbol ",")
    symbol "]"
    pure opts) <|>
  pure []
where
  fieldOption : Parser Unit ProtoOption := do
    let name ← ident
    symbol "="
    let value ← optionValue
    pure ⟨name, value⟩

/-- Parse field cardinality (repeated, optional, or singular) -/
def fieldCardinality : Parser Unit FieldCardinality := do
  (attempt do keyword "repeated"; pure .repeated) <|>
  (attempt do keyword "optional"; pure .optional) <|>
  pure .singular

/-- Parse a regular field definition -/
def fieldDef : Parser Unit FieldDef := do
  let card ← fieldCardinality
  let typ ← fieldType
  let name ← ident
  symbol "="
  let num ← intLit
  let opts ← fieldOptions
  symbol ";"
  pure ⟨card, typ, name, num.toNat, opts⟩

/-- Parse a oneof field (no cardinality allowed) -/
def oneofField : Parser Unit OneofField := do
  let typ ← fieldType
  let name ← ident
  symbol "="
  let num ← intLit
  let opts ← fieldOptions
  symbol ";"
  pure ⟨typ, name, num.toNat, opts⟩

/-- Parse a oneof definition -/
def oneofDef : Parser Unit OneofDef := do
  keyword "oneof"
  let name ← ident
  symbol "{"
  let fields ← many oneofField
  symbol "}"
  pure ⟨name, fields.toList, []⟩

/-- Parse reserved ranges or names -/
def reserved : Parser Unit Reserved := do
  keyword "reserved"
  (attempt do
    let names ← sepBy1 stringLit (symbol ",")
    symbol ";"
    pure (Reserved.names names)) <|>
  (do
    let ranges ← sepBy1 rangeSpec (symbol ",")
    symbol ";"
    pure (Reserved.ranges ranges))
where
  rangeSpec : Parser Unit (Int × Option Int) := do
    let start ← intLit
    let end_ ← Sift.optional (attempt do
      keyword "to"
      (attempt do keyword "max"; pure none) <|>
      (do pure (some (← intLit))))
    pure (start, end_.join)

/-- Parse an enum value -/
def enumValue : Parser Unit EnumValue := do
  let name ← ident
  symbol "="
  let num ← intLit
  let opts ← fieldOptions
  symbol ";"
  pure ⟨name, num, opts⟩

/-- Parse top-level or inline option -/
def protoOption : Parser Unit ProtoOption := do
  keyword "option"
  let name ← optionName
  symbol "="
  let value ← optionValue
  symbol ";"
  pure ⟨name, value⟩
where
  optionName : Parser Unit String := do
    -- Handle (extension.name).field or simple.name
    let parts ← many1 (
      (attempt do
        symbol "("
        let ext ← fullIdent
        symbol ")"
        pure s!"({".".intercalate ext})") <|>
      (attempt ident))
    pure (".".intercalate parts.toList)

/-- Parse an enum definition -/
def enumDef : Parser Unit EnumDef := do
  keyword "enum"
  let name ← ident
  symbol "{"
  let elements ← many enumElement
  symbol "}"
  let (values, options, reserveds) := categorize elements.toList
  pure ⟨name, values, options, reserveds⟩
where
  enumElement : Parser Unit (EnumValue ⊕ ProtoOption ⊕ Reserved) := do
    (attempt do pure (.inr (.inl (← protoOption)))) <|>
    (attempt do pure (.inr (.inr (← reserved)))) <|>
    (attempt do pure (.inl (← enumValue)))

  categorize (elems : List (EnumValue ⊕ ProtoOption ⊕ Reserved))
      : List EnumValue × List ProtoOption × List Reserved :=
    elems.foldl (fun (vs, os, rs) e =>
      match e with
      | .inl v => (vs ++ [v], os, rs)
      | .inr (.inl o) => (vs, os ++ [o], rs)
      | .inr (.inr r) => (vs, os, rs ++ [r])
    ) ([], [], [])

-- Message definition and message element are mutually recursive
mutual
  /-- Parse a message definition (mutually recursive with message element) -/
  partial def messageDef : Parser Unit MessageDef := do
    keyword "message"
    let name ← ident
    symbol "{"
    let elements ← many messageElement
    symbol "}"
    pure ⟨name, elements.toList⟩

  /-- Parse a message element -/
  partial def messageElement : Parser Unit MessageElement := do
    (attempt do pure (.enum (← enumDef))) <|>
    (attempt do pure (.message (← messageDef))) <|>
    (attempt do pure (.oneof (← oneofDef))) <|>
    (attempt do pure (.reserved (← reserved))) <|>
    (attempt do pure (.option (← protoOption))) <|>
    (attempt do pure (.field (← fieldDef)))
end

/-- Parse syntax declaration -/
def syntaxDecl : Parser Unit String := do
  keyword "syntax"
  symbol "="
  let syn ← stringLit
  symbol ";"
  if syn != "proto3" then
    Parser.fail s!"expected 'proto3', got '{syn}'"
  pure syn

/-- Parse package declaration -/
def packageDecl : Parser Unit FullIdent := do
  keyword "package"
  let pkg ← fullIdent
  symbol ";"
  pure ⟨pkg⟩

/-- Parse import declaration -/
def importDecl : Parser Unit Import := do
  keyword "import"
  let kind ← importKind
  let path ← stringLit
  symbol ";"
  pure ⟨kind, path⟩
where
  importKind : Parser Unit ImportKind := do
    (attempt do keyword "public"; pure .public_) <|>
    (attempt do keyword "weak"; pure .weak) <|>
    pure .normal

/-- Parse service definition -/
def serviceDef : Parser Unit ServiceDef := do
  keyword "service"
  let name ← ident
  symbol "{"
  let elements ← many serviceElement
  symbol "}"
  let (methods, options) := categorize elements.toList
  pure ⟨name, methods, options⟩
where
  serviceElement : Parser Unit (RpcMethod ⊕ ProtoOption) := do
    (attempt do pure (.inl (← rpcMethod))) <|>
    (attempt do pure (.inr (← protoOption)))

  rpcMethod : Parser Unit RpcMethod := do
    keyword "rpc"
    let name ← ident
    symbol "("
    let inStream ← Sift.optional (keyword "stream")
    let inType ← fullIdent
    symbol ")"
    keyword "returns"
    symbol "("
    let outStream ← Sift.optional (keyword "stream")
    let outType ← fullIdent
    symbol ")"
    let opts ← rpcOptions
    pure ⟨name, ⟨inType⟩, inStream.isSome, ⟨outType⟩, outStream.isSome, opts⟩

  rpcOptions : Parser Unit (List ProtoOption) := do
    (attempt do
      symbol "{"
      let opts ← many protoOption
      symbol "}"
      pure opts.toList) <|>
    (do symbol ";"; pure [])

  categorize (elems : List (RpcMethod ⊕ ProtoOption))
      : List RpcMethod × List ProtoOption :=
    elems.foldl (fun (ms, os) e =>
      match e with
      | .inl m => (ms ++ [m], os)
      | .inr o => (ms, os ++ [o])
    ) ([], [])

/-- Parse a complete proto file -/
def protoFile : Parser Unit ProtoFile := do
  ws
  let syn ← syntaxDecl
  let pkg ← Sift.optional (attempt packageDecl)
  let imports ← many (attempt importDecl)
  let rest ← many topLevelElement
  ws
  eof
  let (defs, opts) := categorize rest.toList
  pure ⟨syn, pkg, imports.toList, opts, defs⟩
where
  topLevelElement : Parser Unit (TopLevelDef ⊕ ProtoOption) := do
    (attempt do pure (.inl (.message (← messageDef)))) <|>
    (attempt do pure (.inl (.enum (← enumDef)))) <|>
    (attempt do pure (.inl (.service (← serviceDef)))) <|>
    (attempt do pure (.inr (← protoOption)))

  categorize (elems : List (TopLevelDef ⊕ ProtoOption))
      : List TopLevelDef × List ProtoOption :=
    elems.foldl (fun (ds, os) e =>
      match e with
      | .inl d => (ds ++ [d], os)
      | .inr o => (ds, os ++ [o])
    ) ([], [])

/-- Main entry point: parse a proto3 file from string -/
def parse (input : String) : Except String ProtoFile :=
  match Parser.run protoFile input with
  | .ok file => .ok file
  | .error e => .error s!"Proto parse error at {e.pos.line}:{e.pos.column}: {e.message}"

end Protolean.Parser
