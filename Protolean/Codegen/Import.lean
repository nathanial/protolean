/-
  The proto_import command elaborator.

  Usage:
    proto_import "path/to/file.proto"

  This reads the proto file at compile time and generates Lean types
  and instances for the protobuf definitions.
-/
import Lean
import Lean.Parser
import Protolean.Parser.Proto
import Protolean.Import.Loader
import Protolean.Codegen.Types
import Protolean.Codegen.Encode
import Protolean.Codegen.Decode
import Protolean.Codegen.Service
import Protolean.Service.Types
import Protolean.WellKnown

namespace Protolean.Codegen

open Lean
open Lean.Elab
open Lean.Elab.Command
open Protolean.Syntax
open Protolean.Parser
open Protolean.Import

/-- Convert the well-known types registry from String → String to String → Name -/
def wellKnownRegistry : Std.HashMap String Name :=
  Protolean.WellKnown.registry.fold (init := {}) fun acc key value =>
    acc.insert key (Lean.Name.mkSimple value)

/-- Check if proto file imports any well-known types -/
def hasWellKnownImport (imports : List Protolean.Syntax.Import) : Bool :=
  imports.any fun imp => Protolean.WellKnown.isWellKnownImport imp.path

/-- Parse a Lean command from a string and elaborate it -/
def elaborateCodeString (code : String) : CommandElabM Unit := do
  let env ← getEnv
  let fileName ← getFileName
  -- Use runParserCategory to parse a command
  let stx ← match Lean.Parser.runParserCategory env `command code fileName with
    | .ok stx => pure stx
    | .error msg => throwError "Parse error in generated code: {msg}\n\nGenerated code:\n{code}"
  elabCommand stx

/-- Register the proto_import syntax -/
syntax (name := protoImport) "proto_import" str : command

/-- Command elaborator for proto_import -/
@[command_elab protoImport]
def elabProtoImport : CommandElab := fun stx => do
  match stx with
  | `(proto_import $pathStx:str) => do
    let pathStr := pathStx.getString

    -- Get the source file's directory for relative path resolution
    let srcPath ← do
      let fileName ← getFileName
      let filePath := System.FilePath.mk fileName
      pure (filePath.parent.getD (System.FilePath.mk "."))

    let protoPath := srcPath / pathStr

    -- Check if file exists
    let fileExists ← liftIO (protoPath.pathExists)
    if !fileExists then
      throwError s!"Proto file not found: {protoPath}"

    -- Read and parse the proto file
    let contents ← liftIO (IO.FS.readFile protoPath)

    let protoFile ← match Parser.parse contents with
      | .ok file => pure file
      | .error err => throwError s!"Failed to parse {pathStr}: {err}"

    -- Initialize codegen context with well-known types registry
    let ctx : CodegenContext := {
      nsPath := []
      registry := wellKnownRegistry
    }

    -- If proto file imports well-known types, open the namespace
    if hasWellKnownImport protoFile.imports then
      elaborateCodeString "open Google.Protobuf"

    -- Elaborate namespace if present
    if let some pkg := protoFile.package then
      elaborateCodeString s!"namespace {protoFullNameToLeanString pkg.parts}"

    -- Generate and elaborate enums first
    for def_ in protoFile.definitions do
      if let .enum e := def_ then
        elaborateCodeString (generateEnumString ctx e)

    -- Generate and elaborate messages (structures)
    for def_ in protoFile.definitions do
      if let .message m := def_ then
        elaborateCodeString (generateMessageString ctx m)

    -- Generate and elaborate ProtoMessage instances
    for def_ in protoFile.definitions do
      if let .message m := def_ then
        elaborateCodeString (generateEncodeInstanceStr ctx m)

    -- Generate and elaborate service type classes and metadata
    for def_ in protoFile.definitions do
      if let .service svc := def_ then
        let (typeClass, info) := generateServiceString ctx protoFile.package svc
        elaborateCodeString typeClass
        elaborateCodeString info

    -- Close namespace
    if let some pkg := protoFile.package then
      elaborateCodeString s!"end {protoFullNameToLeanString pkg.parts}"

  | _ => throwUnsupportedSyntax

/-- Syntax for setting proto search paths -/
syntax (name := protoPath) "proto_path" str : command

/-- Elaborator for proto_path -/
@[command_elab protoPath]
def elabProtoPath : CommandElab := fun stx => do
  match stx with
  | `(proto_path $_pathStx:str) => do
    -- For now, just acknowledge the path (could store in environment extension)
    pure ()
  | _ => throwUnsupportedSyntax

end Protolean.Codegen
