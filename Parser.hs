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
    rec ms w mf mb (LFun i e) = LBind w' mf' mb' (LFun w e')
      where (LBind w' mf' mb' e') = rec (Map.insert i w ms) (w + 1) (Map.delete i mf) (Map.insert w i mb) e
    rec ms w mf mb (LApp f e) = LBind ew (Map.union fmf emf) (Map.union fmb emb) (LApp f' e')
      where (LBind fw fmf fmb f') = rec ms w mf mb f
            (LBind ew emf emb e') = rec ms fw fmf fmb e
    rec ms w mf mb (LSym i) = case (Map.lookup i mf, Map.lookup i ms) of
      (Nothing, Nothing) -> LBind (w + 1) (Map.insert i w mf) (Map.insert w i mb) (LSym w)
      (Just w', _)       -> LBind w mf mb (LSym w')
      (_, Just w')       -> LBind w mf mb (LSym w')
