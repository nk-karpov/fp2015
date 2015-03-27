import Data.Complex
import Data.List

newtype Cmplx = Cmplx (Complex Double) deriving Eq

instance Show Cmplx where
  showsPrec _ (Cmplx a) = shows (realPart a).(if (imagPart a) < 0 then (showString "-i*") else (showString "+i*")).(shows (abs (imagPart a)))

instance Read Cmplx where
  readsPrec _ s  = [((rCmplx s (q s)), "")]
--	readsPrec _ s  = [(Cmplx (0:+0), "")]

q s = if find (== '+') (tail s) == (Just '+') then '+' else '-'
rCmplx s b = Cmplx (((read first)::Double) :+ (q * ((read last)::Double))) 
							where 
								first = (head s) : ((takeWhile (/= b)) (tail s))
								last  = tail (dropWhile (/= '*') s)
								q = if (b == '+') then (1) else (-1)
					
