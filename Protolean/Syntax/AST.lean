/-
  Abstract Syntax Tree definitions for Proto3.
-/
import Protolean.Syntax.Position

namespace Protolean.Syntax

/-- Scalar field types in Proto3 -/
inductive ScalarType where
  | double | float
  | int32 | int64 | uint32 | uint64
  | sint32 | sint64
  | fixed32 | fixed64 | sfixed32 | sfixed64
  | bool
  | string | bytes
  deriving Repr, BEq, Inhabited, DecidableEq

namespace ScalarType

def toString : ScalarType → String
  | double => "double"
  | float => "float"
  | int32 => "int32"
  | int64 => "int64"
  | uint32 => "uint32"
  | uint64 => "uint64"
  | sint32 => "sint32"
  | sint64 => "sint64"
  | fixed32 => "fixed32"
  | fixed64 => "fixed64"
  | sfixed32 => "sfixed32"
  | sfixed64 => "sfixed64"
  | bool => "bool"
  | string => "string"
  | bytes => "bytes"

instance : ToString ScalarType := ⟨toString⟩

end ScalarType

/-- Key types allowed in map fields (subset of scalars) -/
inductive MapKeyType where
  | int32 | int64 | uint32 | uint64
  | sint32 | sint64
  | fixed32 | fixed64 | sfixed32 | sfixed64
  | bool | string
  deriving Repr, BEq, Inhabited, DecidableEq

/-- A qualified identifier (e.g., "google.protobuf.Timestamp") -/
structure FullIdent where
  parts : List String
  deriving Repr, BEq, Inhabited

def FullIdent.toString (fi : FullIdent) : String :=
  ".".intercalate fi.parts

instance : ToString FullIdent := ⟨FullIdent.toString⟩

/-- Field types -/
inductive FieldType where
  | scalar : ScalarType → FieldType
  | named : FullIdent → FieldType   -- Reference to message or enum type
  | map : MapKeyType → FieldType → FieldType
  deriving Repr, BEq, Inhabited

/-- Option value (for field options, message options, etc.) -/
inductive OptionValue where
  | ident : FullIdent → OptionValue
  | int : Int → OptionValue
  | float : Float → OptionValue
  | string : String → OptionValue
  | bool : Bool → OptionValue
  deriving Repr, BEq, Inhabited

/-- An option specification -/
structure ProtoOption where
  name : String
  value : OptionValue
  deriving Repr, BEq

/-- A single enum value definition -/
structure EnumValue where
  name : String
  number : Int
  options : List ProtoOption := []
  deriving Repr, BEq

/-- Reserved specification (for messages or enums) -/
inductive Reserved where
  | ranges : List (Int × Option Int) → Reserved  -- (start, end?) where end = none means single value
  | names : List String → Reserved
  deriving Repr, BEq, Inhabited

/-- Enum definition -/
structure EnumDef where
  name : String
  values : List EnumValue
  options : List ProtoOption := []
  reserved : List Reserved := []
  deriving Repr, BEq

/-- Field cardinality in Proto3 -/
inductive FieldCardinality where
  | singular   -- Default, no modifier
  | repeated   -- repeated keyword
  | optional   -- optional keyword (explicit presence in Proto3)
  deriving Repr, BEq, Inhabited

/-- A regular field definition -/
structure FieldDef where
  cardinality : FieldCardinality
  fieldType : FieldType
  name : String
  number : Nat
  options : List ProtoOption := []
  deriving Repr, BEq

/-- A oneof field (cannot be repeated) -/
structure OneofField where
  fieldType : FieldType
  name : String
  number : Nat
  options : List ProtoOption := []
  deriving Repr, BEq

/-- A oneof definition -/
structure OneofDef where
  name : String
  fields : List OneofField
  options : List ProtoOption := []
  deriving Repr, BEq

-- Message element and Message definition (mutually recursive)
mutual
  inductive MessageElement where
    | field : FieldDef → MessageElement
    | enum : EnumDef → MessageElement
    | message : MessageDef → MessageElement
    | oneof : OneofDef → MessageElement
    | option : ProtoOption → MessageElement
    | reserved : Reserved → MessageElement

  inductive MessageDef where
    | mk : String → List MessageElement → MessageDef
end

deriving instance Repr for MessageElement
deriving instance Repr for MessageDef

instance : BEq MessageElement where
  beq a b := toString (repr a) == toString (repr b)

instance : BEq MessageDef where
  beq a b := toString (repr a) == toString (repr b)

/-- Get the name of a message -/
def MessageDef.name : MessageDef → String
  | .mk n _ => n

/-- Get the elements of a message -/
def MessageDef.elements : MessageDef → List MessageElement
  | .mk _ es => es

/-- RPC method definition -/
structure RpcMethod where
  name : String
  inputType : FullIdent
  inputStream : Bool  -- client streaming
  outputType : FullIdent
  outputStream : Bool  -- server streaming
  options : List ProtoOption := []
  deriving Repr, BEq

/-- Service definition -/
structure ServiceDef where
  name : String
  methods : List RpcMethod
  options : List ProtoOption := []
  deriving Repr, BEq

/-- Import modifier -/
inductive ImportKind where
  | normal
  | public_  -- "public" import
  | weak     -- "weak" import
  deriving Repr, BEq, Inhabited

/-- Import declaration -/
structure Import where
  kind : ImportKind
  path : String
  deriving Repr, BEq

/-- Top-level definition -/
inductive TopLevelDef where
  | message : MessageDef → TopLevelDef
  | enum : EnumDef → TopLevelDef
  | service : ServiceDef → TopLevelDef
  deriving Repr, BEq

/-- Complete Proto3 file -/
structure ProtoFile where
  protoSyntax : String          -- Should be "proto3" (renamed from 'syntax' to avoid keyword)
  package : Option FullIdent := none
  imports : List Import := []
  options : List ProtoOption := []
  definitions : List TopLevelDef := []
  deriving Repr, BEq

/-- Get all messages from a proto file (including nested) -/
partial def ProtoFile.allMessages (file : ProtoFile) : List MessageDef :=
  file.definitions.flatMap fun
    | .message m => m :: nestedMessages m
    | _ => []
where
  nestedMessages (m : MessageDef) : List MessageDef :=
    m.elements.flatMap fun
      | .message nested => nested :: nestedMessages nested
      | _ => []

/-- Get all enums from a proto file (including nested) -/
partial def ProtoFile.allEnums (file : ProtoFile) : List EnumDef :=
  file.definitions.flatMap fun
    | .message m => messageEnums m
    | .enum e => [e]
    | _ => []
where
  messageEnums (m : MessageDef) : List EnumDef :=
    m.elements.flatMap fun
      | .enum e => [e]
      | .message nested => messageEnums nested
      | _ => []

/-- Get fields from a message (not including oneof fields) -/
def MessageDef.fields (m : MessageDef) : List FieldDef :=
  m.elements.filterMap fun
    | .field f => some f
    | _ => none

/-- Get oneofs from a message -/
def MessageDef.oneofs (m : MessageDef) : List OneofDef :=
  m.elements.filterMap fun
    | .oneof o => some o
    | _ => none

/-- Get nested messages from a message -/
def MessageDef.nestedMessages (m : MessageDef) : List MessageDef :=
  m.elements.filterMap fun
    | .message nested => some nested
    | _ => none

/-- Get nested enums from a message -/
def MessageDef.nestedEnums (m : MessageDef) : List EnumDef :=
  m.elements.filterMap fun
    | .enum e => some e
    | _ => none

end Protolean.Syntax
