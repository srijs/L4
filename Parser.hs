module Parser where

import Data.Char
import Control.Applicative ((<$>), (<|>), (*>), (<*))
import Data.Attoparsec.Text
import Data.Scientific (Scientific)
import Data.Text

data Tree = TLst [Tree]
          | TSym Text
  deriving Show

tree = TLst <$> (char '(' *> sepBy tree (many1 space) <* char ')') <|>
       TSym <$> (takeWhile1 (\c -> c /= '(' && c /= ')' && isPrint c && not (isSpace c)))
