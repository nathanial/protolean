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

  let mut totalPassed := 0
  let mut totalFailed := 0

  -- Varint tests
  IO.println "━━━ Varint Tests ━━━"
  let varintResult ← runTests "Varint Tests" Tests.Varint.cases
  if varintResult == 0 then
    totalPassed := totalPassed + Tests.Varint.cases.length
  else
    totalFailed := totalFailed + varintResult.toNat

  IO.println ""

  -- Scalar tests
  IO.println "━━━ Scalar Tests ━━━"
  let scalarResult ← runTests "Scalar Tests" Tests.Scalar.cases
  if scalarResult == 0 then
    totalPassed := totalPassed + Tests.Scalar.cases.length
  else
    totalFailed := totalFailed + scalarResult.toNat

  IO.println ""

  -- Parser tests
  IO.println "━━━ Parser Tests ━━━"
  let parserResult ← runTests "Parser Tests" Tests.Parser.cases
  if parserResult == 0 then
    totalPassed := totalPassed + Tests.Parser.cases.length
  else
    totalFailed := totalFailed + parserResult.toNat

  IO.println ""

  -- Import tests
  IO.println "━━━ Import Tests ━━━"
  let importResult ← runTests "Import Tests" Tests.Import.cases
  if importResult == 0 then
    totalPassed := totalPassed + Tests.Import.cases.length
  else
    totalFailed := totalFailed + importResult.toNat

  IO.println ""

  -- Service tests
  IO.println "━━━ Service Tests ━━━"
  let serviceResult ← runTests "Service Tests" Tests.Service.cases
  if serviceResult == 0 then
    totalPassed := totalPassed + Tests.Service.cases.length
  else
    totalFailed := totalFailed + serviceResult.toNat

  IO.println ""

  -- Well-known types tests
  IO.println "━━━ Well-Known Types Tests ━━━"
  let wellKnownResult ← runTests "WellKnown Tests" Tests.WellKnown.cases
  if wellKnownResult == 0 then
    totalPassed := totalPassed + Tests.WellKnown.cases.length
  else
    totalFailed := totalFailed + wellKnownResult.toNat

  IO.println ""
  IO.println "══════════════════════════════════════════════════════════════"

  let totalTests := Tests.Varint.cases.length + Tests.Scalar.cases.length +
                    Tests.Parser.cases.length + Tests.Import.cases.length +
                    Tests.Service.cases.length + Tests.WellKnown.cases.length

  if totalFailed == 0 then
    IO.println s!"All {totalTests} tests passed!"
  else
    IO.println s!"{totalTests - totalFailed} passed, {totalFailed} failed"
    IO.Process.exit 1
