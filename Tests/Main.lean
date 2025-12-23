/-
  Test runner for Protolean.
-/
import Crucible
import Tests.Varint
import Tests.Scalar
import Tests.Parser
import Tests.Import
import Tests.Service
import Tests.WellKnown

open Crucible

def main : IO Unit := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║                    Protolean Test Suite                      ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  let exitCode ← runAllSuites

  IO.println ""
  IO.println "══════════════════════════════════════════════════════════════"

  if exitCode == 0 then
    IO.println "All tests passed!"
  else
    IO.println "Some tests failed"
    IO.Process.exit 1
