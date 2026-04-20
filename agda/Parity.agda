open import Agda.Builtin.Nat
open import Data.Nat.Base
open import Relation.Binary.PropositionalEquality

data Parity : Set where
  odd : Parity
  even : Parity

data Number : Parity → Set where
 nzero : Number even
 osucc : Number odd -> Number even
 esucc : Number even -> Number odd

number→int : { P : Parity } → Number P → Nat
number→int nzero = 0
number→int (osucc n) = 1 + number→int n
number→int (esucc n) = 1 + number→int n

flip : Parity -> Parity
flip odd = even
flip even = odd

succ : { P : Parity } → Number P → Number (flip P)
succ nzero = esucc nzero
succ (esucc n) = osucc (esucc n)
succ (osucc n) = esucc (osucc n)
