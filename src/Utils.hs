module Utils where

import ParseLib.Abstract
import Prelude hiding ((<$), ($>), (<*), (*>), sequence)

-- helpers
guard :: (a -> Bool) -> Parser s a -> Parser s a
guard f p = do
  x <- p
  if f x then return x else empty
-- don't think i ended up using this thanks to sorting

notCRLF :: Char -> Bool
notCRLF x = x /= '\r' && x /= '\n'