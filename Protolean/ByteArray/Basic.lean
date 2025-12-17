/-
  ByteArray utility functions for little-endian reading and writing.
-/

/-- Repr instance for ByteArray to allow deriving Repr on structures with ByteArray fields -/
instance : Repr ByteArray where
  reprPrec ba _ :=
    let bytes := ba.toList.map fun b => toString b.toNat
    s!"ByteArray.mk #[{", ".intercalate bytes}]"

namespace Protolean.ByteArray

/-- Push a UInt16 in little-endian order -/
def pushUInt16LE (arr : ByteArray) (v : UInt16) : ByteArray :=
  arr.push v.toUInt8
     |>.push (v >>> 8).toUInt8

/-- Push a UInt32 in little-endian order -/
def pushUInt32LE (arr : ByteArray) (v : UInt32) : ByteArray :=
  arr.push v.toUInt8
     |>.push (v >>> 8).toUInt8
     |>.push (v >>> 16).toUInt8
     |>.push (v >>> 24).toUInt8

/-- Push a UInt64 in little-endian order -/
def pushUInt64LE (arr : ByteArray) (v : UInt64) : ByteArray :=
  arr.push v.toUInt8
     |>.push (v >>> 8).toUInt8
     |>.push (v >>> 16).toUInt8
     |>.push (v >>> 24).toUInt8
     |>.push (v >>> 32).toUInt8
     |>.push (v >>> 40).toUInt8
     |>.push (v >>> 48).toUInt8
     |>.push (v >>> 56).toUInt8

/-- Read a UInt16 from little-endian bytes at given offset -/
def getUInt16LE? (arr : ByteArray) (offset : Nat) : Option UInt16 :=
  if offset + 2 <= arr.size then
    let b0 := arr.get! offset
    let b1 := arr.get! (offset + 1)
    some (b0.toUInt16 ||| (b1.toUInt16 <<< 8))
  else
    none

/-- Read a UInt32 from little-endian bytes at given offset -/
def getUInt32LE? (arr : ByteArray) (offset : Nat) : Option UInt32 :=
  if offset + 4 <= arr.size then
    let b0 := arr.get! offset
    let b1 := arr.get! (offset + 1)
    let b2 := arr.get! (offset + 2)
    let b3 := arr.get! (offset + 3)
    some (b0.toUInt32 ||| (b1.toUInt32 <<< 8) ||| (b2.toUInt32 <<< 16) ||| (b3.toUInt32 <<< 24))
  else
    none

/-- Read a UInt64 from little-endian bytes at given offset -/
def getUInt64LE? (arr : ByteArray) (offset : Nat) : Option UInt64 :=
  if offset + 8 <= arr.size then
    let b0 := arr.get! offset
    let b1 := arr.get! (offset + 1)
    let b2 := arr.get! (offset + 2)
    let b3 := arr.get! (offset + 3)
    let b4 := arr.get! (offset + 4)
    let b5 := arr.get! (offset + 5)
    let b6 := arr.get! (offset + 6)
    let b7 := arr.get! (offset + 7)
    some (b0.toUInt64 ||| (b1.toUInt64 <<< 8) ||| (b2.toUInt64 <<< 16) ||| (b3.toUInt64 <<< 24) |||
          (b4.toUInt64 <<< 32) ||| (b5.toUInt64 <<< 40) ||| (b6.toUInt64 <<< 48) ||| (b7.toUInt64 <<< 56))
  else
    none

/-- Read a UInt32 from little-endian bytes (unsafe, no bounds check) -/
def getUInt32LE! (arr : ByteArray) (offset : Nat) : UInt32 :=
  let b0 := arr.get! offset
  let b1 := arr.get! (offset + 1)
  let b2 := arr.get! (offset + 2)
  let b3 := arr.get! (offset + 3)
  b0.toUInt32 ||| (b1.toUInt32 <<< 8) ||| (b2.toUInt32 <<< 16) ||| (b3.toUInt32 <<< 24)

/-- Read a UInt64 from little-endian bytes (unsafe, no bounds check) -/
def getUInt64LE! (arr : ByteArray) (offset : Nat) : UInt64 :=
  let b0 := arr.get! offset
  let b1 := arr.get! (offset + 1)
  let b2 := arr.get! (offset + 2)
  let b3 := arr.get! (offset + 3)
  let b4 := arr.get! (offset + 4)
  let b5 := arr.get! (offset + 5)
  let b6 := arr.get! (offset + 6)
  let b7 := arr.get! (offset + 7)
  b0.toUInt64 ||| (b1.toUInt64 <<< 8) ||| (b2.toUInt64 <<< 16) ||| (b3.toUInt64 <<< 24) |||
  (b4.toUInt64 <<< 32) ||| (b5.toUInt64 <<< 40) ||| (b6.toUInt64 <<< 48) ||| (b7.toUInt64 <<< 56)

/-- Convert Float to its IEEE 754 double-precision bit representation -/
def floatToBits (f : Float) : UInt64 :=
  f.toBits

/-- Convert IEEE 754 double-precision bits to Float -/
def bitsToFloat (bits : UInt64) : Float :=
  Float.ofBits bits

end Protolean.ByteArray
