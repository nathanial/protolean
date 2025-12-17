/-
  Name mangling utilities for converting proto names to Lean names.
-/
import Lean

namespace Protolean.Codegen

open Lean

/-- Escape Lean keywords by appending underscore -/
def escapeKeyword (name : String) : String :=
  if isKeyword name then name ++ "_" else name
where
  isKeyword (s : String) : Bool :=
    s ∈ ["def", "theorem", "lemma", "example", "structure", "class", "instance",
         "inductive", "where", "with", "if", "then", "else", "match", "do",
         "return", "let", "have", "fun", "for", "in", "by", "at", "this",
         "type", "sort", "prop", "true", "false", "and", "or", "not",
         "namespace", "section", "end", "open", "import", "export",
         "private", "protected", "partial", "unsafe", "opaque", "axiom",
         "constant", "abbrev", "variable", "universe", "set_option",
         "attribute", "local", "scoped", "macro", "syntax", "elab", "deriving",
         -- Notation keywords
         "prefix", "infix", "infixl", "infixr", "postfix", "notation",
         -- Other reserved words
         "mutual", "noncomputable", "nonrec", "rec", "renaming", "extends",
         "hiding", "using", "quot", "unif_hint", "init_quot"]

/-- Convert proto field name (snake_case) to Lean field name (camelCase) -/
def protoFieldToLean (name : String) : String :=
  let parts := name.splitOn "_"
  let camelCase := match parts with
    | [] => name
    | h :: t => h.decapitalize ++ String.join (t.map String.capitalize)
  escapeKeyword camelCase

/-- Convert proto type name to Lean type name (keep PascalCase) -/
def protoTypeToLean (name : String) : String :=
  name.capitalize

/-- Convert proto enum value to Lean constructor (SCREAMING_SNAKE → camelCase) -/
def protoEnumValueToLean (name : String) : String :=
  let parts := name.toLower.splitOn "_"
  match parts with
  | [] => name
  | h :: t => h ++ String.join (t.map String.capitalize)

/-- Convert a fully qualified proto name to a Lean Name -/
def protoFullNameToLean (parts : List String) : Name :=
  parts.foldl (fun acc part => acc ++ Name.mkSimple (part.capitalize)) Name.anonymous

/-- Convert a fully qualified proto name to a Lean name string -/
def protoFullNameToLeanString (parts : List String) : String :=
  ".".intercalate (parts.map String.capitalize)

/-- Create a Lean identifier from a proto field name -/
def mkFieldIdent (name : String) : Ident :=
  mkIdent (Name.mkSimple (protoFieldToLean name))

/-- Create a Lean identifier from a proto type name -/
def mkTypeIdent (name : String) : Ident :=
  mkIdent (Name.mkSimple (protoTypeToLean name))

/-- Create a Lean identifier from a proto enum value name -/
def mkEnumValueIdent (name : String) : Ident :=
  mkIdent (Name.mkSimple (protoEnumValueToLean name))

end Protolean.Codegen
