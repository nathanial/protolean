/-
  Lexer utilities for Proto3 parsing using Sift parser combinators.
-/
import Sift
import Protolean.Syntax.AST

namespace Protolean.Parser

open Sift

/-- Skip single-line comment (// ...) -/
def lineComment : Parser Unit Unit := do
  let _ ← string "//"
  skipWhile (· != '\n')
  let _ ← Sift.optional (char '\n')

/-- Skip block comment (/* ... */) -/
partial def blockComment : Parser Unit Unit := do
  let _ ← string "/*"
  skipUntilClose
where
  skipUntilClose : Parser Unit Unit := do
    let c ← anyChar
    if c == '*' then
      let next ← peek
      if next == some '/' then
        let _ ← anyChar
        pure ()
      else
        skipUntilClose
    else
      skipUntilClose

/-- Skip whitespace and comments -/
partial def ws : Parser Unit Unit := do
  let _ ← many (wsChar <|> attempt lineComment <|> attempt blockComment)
  pure ()
where
  wsChar : Parser Unit Unit := do
    let _ ← satisfy fun c => c == ' ' || c == '\t' || c == '\n' || c == '\r'
    pure ()

/-- Parse an identifier -/
def ident : Parser Unit String := do
  ws
  let first ← satisfy fun c => c.isAlpha || c == '_'
  let rest ← manyChars (satisfy fun c => c.isAlphanum || c == '_')
  pure (first.toString ++ rest)

/-- Parse a full identifier (dot-separated) -/
def fullIdent : Parser Unit (List String) := do
  let first ← ident
  let rest ← many (attempt do
    ws
    let _ ← char '.'
    ident)
  pure (first :: rest.toList)

/-- Check if identifier matches a keyword -/
def keyword (kw : String) : Parser Unit Unit := attempt do
  ws
  let id ← ident
  if id == kw then pure ()
  else Parser.fail s!"expected keyword '{kw}', got '{id}'"

/-- Parse a decimal integer -/
def decimalLit : Parser Unit Int := do
  ws
  let sign ← Sift.optional (char '-')
  let first ← satisfy Char.isDigit
  let rest ← manyChars (satisfy Char.isDigit)
  let digits := first.toString ++ rest
  let value := digits.toNat!
  pure (if sign.isSome then -value else value)

/-- Parse a hex integer (0x...) -/
def hexLit : Parser Unit Int := attempt do
  ws
  let sign ← Sift.optional (char '-')
  let _ ← string "0x" <|> string "0X"
  let digits ← many1Chars hexDigit
  let value := digits.foldl (fun acc c => acc * 16 + hexDigitValue c) 0
  pure (if sign.isSome then -value else value)
where
  hexDigitValue (c : Char) : Nat :=
    if '0' ≤ c && c ≤ '9' then c.toNat - '0'.toNat
    else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
    else c.toNat - 'A'.toNat + 10

/-- Parse an octal integer (0...) -/
def octalLit : Parser Unit Int := attempt do
  ws
  let sign ← Sift.optional (char '-')
  let _ ← char '0'
  let digits ← many1Chars (satisfy fun c => '0' ≤ c && c ≤ '7')
  let value := digits.foldl (fun acc c => acc * 8 + (c.toNat - '0'.toNat)) 0
  pure (if sign.isSome then -value else value)

/-- Parse an integer literal (hex, octal, or decimal) -/
def intLit : Parser Unit Int :=
  hexLit <|> octalLit <|> decimalLit

/-- Parse a float literal -/
def floatLit : Parser Unit Float := do
  ws
  let sign ← Sift.optional (char '-')
  let intPart ← manyChars (satisfy Char.isDigit)
  let _ ← char '.'
  let fracPart ← manyChars (satisfy Char.isDigit)
  let expPart ← Sift.optional do
    let _ ← satisfy fun c => c == 'e' || c == 'E'
    let expSign ← Sift.optional (satisfy fun c => c == '+' || c == '-')
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
def stringLit : Parser Unit String := do
  ws
  let quote ← satisfy fun c => c == '"' || c == '\''
  let chars ← manyChars (stringChar quote)
  let _ ← char quote
  pure chars
where
  stringChar (quote : Char) : Parser Unit Char :=
    (attempt do
      let _ ← char '\\'
      escapeChar) <|>
    satisfy fun c => c != quote && c != '\\'

  escapeChar : Parser Unit Char := do
    let c ← anyChar
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
    | _ => Parser.fail s!"invalid escape sequence: \\{c}"

  hexVal (c : Char) : Nat :=
    if '0' ≤ c && c ≤ '9' then c.toNat - '0'.toNat
    else if 'a' ≤ c && c ≤ 'f' then c.toNat - 'a'.toNat + 10
    else c.toNat - 'A'.toNat + 10

/-- Parse a punctuation symbol -/
def symbol (s : String) : Parser Unit Unit := do
  ws
  let _ ← string s
  pure ()

/-- Parse with items separated by a delimiter -/
def sepBy1 (p : Parser Unit α) (sep : Parser Unit Unit) : Parser Unit (List α) := do
  let first ← p
  let rest ← many (attempt do sep; p)
  pure (first :: rest.toList)

/-- Parse with items separated by a delimiter (zero or more) -/
def sepBy (p : Parser Unit α) (sep : Parser Unit Unit) : Parser Unit (List α) :=
  sepBy1 p sep <|> pure []

/-- End of input -/
def eof : Parser Unit Unit := do
  if ← atEnd then pure ()
  else Parser.fail "expected end of input"

end Protolean.Parser
