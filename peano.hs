{--
    prove 1+1=2
--}

import Data.Word

data Nat = Z | S Nat

plus :: Nat -> Nat -> Nat
plus Z n     = n
plus (S n) m = S (plus n m)

toNat :: Word -> Nat
toNat 0 = Z
toNat n = S (toNat (n - 1))

toWord :: Nat -> Word
toWord Z = 0
toWord (S n) = 1 + toWord n

main = print $ toWord $ plus one one 
    where one = toNat 1
