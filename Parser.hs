module Parser where

import Prelude hiding (takeWhile)
import Data.Char
import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*))
import Data.Attoparsec.Text
import Data.Scientific (Scientific)
import Data.Text hiding (takeWhile)
import Data.Word
import qualified Data.List as List
import qualified Data.Map as Map


identifier :: Parser Text
identifier = takeWhile1 (\c -> not (inClass "({})" c) && isPrint c && not (isSpace c))


-- Lettuce is a tree of function literals and applications, with symbols of type i.

data Lettuce i = LFun i (Lettuce i)
               | LApp (Lettuce i) (Lettuce i)
               | LSym i
  deriving Show

lettuce :: Parser (Lettuce Text)
lettuce = LFun <$> (char '{' *> skipSpace *> identifier) <*> (skipSpace *> lettuce <* skipSpace <* char '}') <|>
          LApp <$> (char '(' *> skipSpace *> lettuce) <*> (skipSpace *> lettuce <* skipSpace <* char ')') <|>
          LSym <$> identifier

data LScope i = LScope Word (Map.Map i Word) deriving Show
data LBind i = LBind [i] Word (Map.Map Word i) deriving Show

lbind :: (Eq i, Ord i) => Lettuce i -> (LBind i, Lettuce Word)
lbind = rec (LScope 0 Map.empty)
  where
    rec (LScope w mi) (LFun i e) = (LBind (List.delete i f) w' (Map.insert w i mw), LFun w e')
      where (LBind f w' mw, e') = rec (LScope (w + 1) (Map.insert i w mi)) e
    rec (LScope w mi) (LApp f e) = (LBind (List.union ef ff) fw (Map.union emw fmw), LApp f' e')
      where (LBind ef ew emw, e') = rec (LScope w mi) e
            (LBind ff fw fmw, f') = rec (LScope ew mi) f
    rec (LScope w mi) (LSym i) = case Map.lookup i mi of
      Nothing -> (LBind [i] (w + 1) (Map.singleton w i), LSym w)
      Just w' -> (LBind [] w Map.empty, LSym w')
