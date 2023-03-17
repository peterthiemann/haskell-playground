{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module PrettyShared where

import Data.ByteString.Builder
import Data.String (IsString)
import System.IO

newtype Doc = Doc Builder
  deriving newtype (IsString, Semigroup, Monoid)

class Pretty a where
  pPrint :: a -> Doc

text :: String -> Doc
text = Doc . stringUtf8

(<+>) :: Doc -> Doc -> Doc
Doc x <+> Doc y = Doc (x <> char7 ' ' <> y)

($$) :: Doc -> Doc -> Doc
Doc x $$ Doc y = Doc (x <> char7 '\n' <> y)

joinDoc :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
joinDoc _ [] = mempty
joinDoc f ds = foldr1 f ds

sep :: [Doc] -> Doc
sep = joinDoc (<+>)

vcat :: [Doc] -> Doc
vcat = joinDoc ($$)

punctuate :: Doc -> [Doc] -> [Doc]
punctuate _ [] = []
punctuate _ [d] = [d]
punctuate p (d:ds) = (d <> p) : punctuate p ds

enclose :: Doc -> Doc -> Doc -> Doc
enclose (Doc l) (Doc r) (Doc x) = Doc (l <> x <> r)

lparen, rparen :: Doc
lparen = Doc (char7 '(')
rparen = Doc (char7 ')')

parens :: Doc -> Doc
parens = enclose lparen rparen

lbrace, rbrace :: Doc
lbrace = Doc (char7 '{')
rbrace = Doc (char7 '}')

braces :: Doc -> Doc
braces = enclose lbrace rbrace

comma :: Doc
comma = Doc (char7 ',')

semi :: Doc
semi = Doc (char7 ';')

colon :: Doc
colon = Doc (char7 ':')

equals :: Doc
equals = Doc (char7 '=')

bar :: Doc
bar = Doc (char7 '|')

dot :: Doc
dot = Doc (char7 '.')

putDoc :: Doc -> IO ()
putDoc = hPutDoc stdout

hPutDoc :: Handle -> Doc -> IO ()
hPutDoc h (Doc b) = hPutBuilder h b >> hPutChar h '\n'
