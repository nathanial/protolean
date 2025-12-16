/-
  Compile-time file loading for proto files.
-/
import Protolean.Import.Resolver
import Protolean.Parser.Proto
import Protolean.Syntax.AST

namespace Protolean.Import

open Protolean.Syntax
open Protolean.Parser

/-- Load and parse a proto file -/
def loadProtoFile (path : System.FilePath) : IO (Except String ProtoFile) := do
  let contents ← IO.FS.readFile path
  return Parser.parse contents

/-- Result of loading a proto file with its imports -/
structure LoadResult where
  /-- Main file -/
  mainFile : ProtoFile
  /-- All imported files (path → parsed file) -/
  imports : List (System.FilePath × ProtoFile)
  deriving Repr

/-- State for recursive loading -/
structure LoadState where
  visited : List String
  results : List (System.FilePath × ProtoFile)

/-- Recursively load a proto file and all its imports -/
partial def loadProtoFileWithImports (config : ImportConfig) (path : System.FilePath)
    : IO (Except String LoadResult) := do
  let stateRef ← IO.mkRef (LoadState.mk [] [])

  let rec loadRec (p : System.FilePath) : IO (Except String Unit) := do
    let pStr := p.toString
    let state ← stateRef.get
    if state.visited.contains pStr then
      return .ok ()

    stateRef.modify fun s => { s with visited := pStr :: s.visited }

    match ← loadProtoFile p with
    | .error e => return .error s!"Error loading {pStr}: {e}"
    | .ok file =>
      stateRef.modify fun s => { s with results := (p, file) :: s.results }

      -- Load imports
      for imp in file.imports do
        -- Skip well-known protos for now (they would need special handling)
        if isWellKnown imp.path then
          continue

        match ← resolveImport config imp.path with
        | none =>
          return .error s!"Cannot resolve import: {imp.path}"
        | some impPath =>
          match ← loadRec impPath with
          | .error e => return .error e
          | .ok () => pure ()

      return .ok ()

  match ← loadRec path with
  | .error e => return .error e
  | .ok () =>
    let state ← stateRef.get
    -- The main file is the last one added (first in the list)
    match state.results with
    | [] => return .error "No files loaded"
    | (_, mainFile) :: rest =>
      return .ok ⟨mainFile, rest⟩

end Protolean.Import
