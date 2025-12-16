/-
  Source position tracking for error reporting in proto files.
-/
namespace Protolean.Syntax

/-- Source position for error reporting -/
structure Position where
  line : Nat
  column : Nat
  offset : Nat  -- byte offset in file
  deriving Repr, BEq, Inhabited

/-- A span in the source file -/
structure Span where
  start : Position
  stop : Position
  source : Option String := none  -- filename
  deriving Repr, BEq, Inhabited

/-- Wrapper for AST nodes with location information -/
structure Located (α : Type) where
  span : Span
  value : α
  deriving Repr, BEq

instance [Inhabited α] : Inhabited (Located α) where
  default := ⟨default, default⟩

/-- Create a Located value with a dummy span (for testing) -/
def Located.mk' (value : α) : Located α :=
  ⟨default, value⟩

/-- Map over the value in a Located -/
def Located.map (f : α → β) (loc : Located α) : Located β :=
  ⟨loc.span, f loc.value⟩

end Protolean.Syntax
