module PrettyShared where

import Text.PrettyPrint
import Text.PrettyPrint.HughesPJClass
import Types


bar = text "|"
dot = text "."

instance Pretty Kind where
  pPrint = text . show

instance Pretty Direction where
  pPrint Input = text "?"
  pPrint Output = text "!"

instance Pretty Polarity where
  pPrint Plus = empty -- text "+"
  pPrint Minus = text "-"

