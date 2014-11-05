module Parser where

import Prelude hiding (takeWhile)
import Data.Char
import Control.Applicative ((<$>), (<|>), (<*>), (*>), (<*))
import Data.Attoparsec.Text
import Data.Scientific (Scientific)
import Data.Text hiding (takeWhile)
import Data.Word
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

data LettuceBinding i = LBind {
  getSymbolCountL :: Word,
  getFreeSymbolsL :: Map.Map i Word,
  getSymbolTableL :: Map.Map Word i,
  getExpressionL  :: Lettuce Word
} deriving Show

lbind :: (Eq i, Ord i) => Lettuce i -> LettuceBinding i
lbind = rec Map.empty 0 Map.empty Map.empty
  where
    rec scope id free tab (LFun sym e) = LBind id' free' tab' (LFun id e')
      where (LBind id' free' tab' e') = rec (Map.insert sym id scope) (id + 1) (Map.delete sym free) (Map.insert id sym tab) e
    rec scope id free tab (LApp f e) = LBind id'' free'' tab'' (LApp f' e')
      where (LBind id'  free'  tab'  f') = rec scope id  free  tab  f
            (LBind id'' free'' tab'' e') = rec scope id' free' tab' e
    rec scope id free tab (LSym sym) = case (Map.lookup sym free, Map.lookup sym scope) of
      (Nothing, Nothing) -> LBind (id + 1) (Map.insert sym id free) (Map.insert id sym tab) (LSym id)
      (Just id', _)      -> LBind id free tab (LSym id')
      (_, Just id')      -> LBind id free tab (LSym id')
