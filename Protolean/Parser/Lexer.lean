/-
  Lexer utilities for Proto3 parsing using Std.Internal.Parsec.
-/
import Std.Internal.Parsec
import Std.Internal.Parsec.String
import Protolean.Syntax.AST

namespace Protolean.Parser

open Std.Internal.Parsec
open Std.Internal.Parsec.String

/-- Skip single-line comment (// ...) -/
def lineComment : Parser Unit := do
  let _ ← pstring "//"
  let _ ← many (satisfy fun c => c != '\n')
  let _ ← optional (pchar '\n')
  pure ()

/-- Skip block comment (/* ... */) -/
partial def blockComment : Parser Unit := do
  let _ ← pstring "/*"
  skipUntilClose
where
  skipUntilClose : Parser Unit := do
    let c ← any
    if c == '*' then
      let next ← peek?
      if next == some '/' then
        let _ ← any
        pure ()
      else
        skipUntilClose
    else
      skipUntilClose

/-- Skip whitespace and comments -/
partial def ws : Parser Unit := do
  let _ ← many (wsChar <|> attempt lineComment <|> attempt blockComment)
  pure ()
where
  wsChar : Parser Unit := do
    let _ ← satisfy fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r'
    pure ()

/-- Parse an identifier -/
def ident : Parser String := do
  ws
  let first ← satisfy fun c => c.isAlpha || c == '_'
  let rest ← manyChars (satisfy fun c => c.isAlphanum || c == '_')
  pure (first.toString ++ rest)

/-- Parse a full identifier (dot-separated) -/
def fullIdent : Parser (List String) := do
  let first ← ident
  let rest ← many (attempt do
    ws
    let _ ← pchar '.'
    ident)
  pure (first :: rest.toList)

/-- Check if identifier matches a keyword -/
def keyword (kw : String) : Parser Unit := attempt do
  ws
  let id ← ident
  if id == kw then pure ()
  else fail s!"expected keyword '{kw}', got '{id}'"

/-- Parse a decimal integer -/
def decimalLit : Parser Int := do
  ws
  let sign ← optional (pchar '-')
  let first ← satisfy Char.isDigit
  let rest ← manyChars (satisfy Char.isDigit)
  let digits := first.toString ++ rest
  let value := digits.toNat!
  pure (if sign.isSome then -value else value)

/-- Parse a hex integer (0x...) -/
def hexLit : Parser Int := attempt do
  ws
  let sign ← optional (pchar '-')
  let _ ← pstring "0x" <|> pstring "0X"
  let digits ← many1Chars hexDigit
  let value := digits.foldl (fun acc c => acc * 16 + hexDigitValue c) 0
  pure (if sign.isSome then -value else value)
where
  hexDigitValue (c : Char) : Nat :=
    if '0' ≤ c && c ≤ '9' then c.toNat - '0'.toNat
    else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
    else c.toNat - 'A'.toNat + 10

/-- Parse an octal integer (0...) -/
def octalLit : Parser Int := attempt do
  ws
  let sign ← optional (pchar '-')
  let _ ← pchar '0'
  let digits ← many1Chars (satisfy fun c => '0' ≤ c && c ≤ '7')
  let value := digits.foldl (fun acc c => acc * 8 + (c.toNat - '0'.toNat)) 0
  pure (if sign.isSome then -value else value)

/-- Parse an integer literal (hex, octal, or decimal) -/
def intLit : Parser Int :=
  hexLit <|> octalLit <|> decimalLit

/-- Parse a float literal -/
def floatLit : Parser Float := do
  ws
  let sign ← optional (pchar '-')
  let intPart ← manyChars (satisfy Char.isDigit)
  let _ ← pchar '.'
  let fracPart ← manyChars (satisfy Char.isDigit)
  let expPart ← optional do
    let _ ← satisfy fun c => c == 'e' || c == 'E'
    let expSign ← optional (satisfy fun c => c == '+' || c == '-')
    let expDigits ← many1Chars (satisfy Char.isDigit)
    pure (expSign, expDigits)
  let numStr := (if sign.isSome then "-" else "") ++ intPart ++ "." ++ fracPart ++
    match expPart with
    | some (expSign, expDigits) =>
      "e" ++ (match expSign with | some c => c.toString | none => "") ++ expDigits
    | none => ""
  -- Parse float manually since String.toFloat? doesn't exist
  pure (parseFloatString numStr)
where
  parseFloatString (s : String) : Float :=
    let negative := s.startsWith "-"
    let s' := if negative then s.drop 1 else s
    -- Split on 'e' or 'E' for exponent
    let (mantissa, expPart) := match s'.splitOn "e" with
      | [m, e] => (m, some e)
      | _ => match s'.splitOn "E" with
        | [m, e] => (m, some e)
        | _ => (s', none)
    -- Parse mantissa (integer.fraction)
    let (intStr, fracStr) := match mantissa.splitOn "." with
      | [i, f] => (i, f)
      | [i] => (i, "")
      | _ => ("0", "0")
    let intVal := intStr.toNat!.toFloat
    let fracVal := if fracStr.isEmpty then 0.0
                   else fracStr.toNat!.toFloat / Float.pow 10.0 fracStr.length.toFloat
    let mantissaVal := intVal + fracVal
    -- Apply exponent
    let expVal : Int := match expPart with
      | some e => e.toInt!
      | none => 0
    -- Handle positive and negative exponents
    let multiplier := if expVal >= 0
      then Float.pow 10.0 expVal.natAbs.toFloat
      else 1.0 / Float.pow 10.0 expVal.natAbs.toFloat
    let result := mantissaVal * multiplier
    if negative then -result else result

/-- Parse a string literal (single or double quoted) -/
def stringLit : Parser String := do
  ws
  let quote ← satisfy fun c => c == '"' || c == '\''
  let chars ← manyChars (stringChar quote)
  let _ ← pchar quote
  pure chars
where
  stringChar (quote : Char) : Parser Char :=
    (attempt do
      let _ ← pchar '\\'
      escapeChar) <|>
    satisfy fun c => c != quote && c != '\\'

  escapeChar : Parser Char := do
    let c ← any
    match c with
    | 'n' => pure '\n'
    | 't' => pure '\t'
    | 'r' => pure '\r'
    | '\\' => pure '\\'
    | '\'' => pure '\''
    | '"' => pure '"'
    | '0' => pure (Char.ofNat 0)
    | 'x' => do
      let d1 ← hexDigit
      let d2 ← hexDigit
      pure (Char.ofNat (hexVal d1 * 16 + hexVal d2))
    | _ => fail s!"invalid escape sequence: \\{c}"

  hexVal (c : Char) : Nat :=
    if '0' ≤ c && c ≤ '9' then c.toNat - '0'.toNat
    else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
    else c.toNat - 'A'.toNat + 10

/-- Parse a punctuation symbol -/
def symbol (s : String) : Parser Unit := do
  ws
  let _ ← pstring s
  pure ()

/-- Parse with items separated by a delimiter -/
def sepBy1 (p : Parser α) (sep : Parser Unit) : Parser (List α) := do
  let first ← p
  let rest ← many (attempt do sep; p)
  pure (first :: rest.toList)

/-- Parse with items separated by a delimiter (zero or more) -/
def sepBy (p : Parser α) (sep : Parser Unit) : Parser (List α) :=
  sepBy1 p sep <|> pure []

end Protolean.Parser
