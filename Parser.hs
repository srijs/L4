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
identifier = takeWhile1 (\c -> not (inClass "({[]})" c) && isPrint c && not (isSpace c))


-- Lettuce is a tree of function literals and applications, with vectors and symbols of type i.

-- product types (example a * b * c):
--   decon a b c :: (a -> b -> c -> d) -> d
--   con   a b c :: a -> b -> c -> decon a b c
--   con   a b c == \f. f a b c 
--   The constructor creates the deconstructor! The deconstructor is equivalent to the type.
--   unit :: (f -> f) == id

-- sum types: given a type a, there exist coercions (a -> b). values of type b can be tagged with a coercion, and thus become
-- values of type a. Pattern matching on a value of type a requires a mapping of coercions to lambdas, like: 'case a of c {b b}' which is
-- equivalent to c, where c is a coercion (a -> b), and a and b are of their respective types.

data Lettuce i = LFun i (Lettuce i)
               | LApp (Lettuce i) (Lettuce i)
               | LTag (Lettuce i) (Lettuce i)
               | LSym i
  deriving Show

lettuce :: Parser (Lettuce Text)
lettuce = lfun <|> lapp <|> ltag <|> lsym
  where lfun = LFun <$> (char '{' *> skipSpace *> identifier) <*> (skipSpace *> lettuce <* skipSpace <* char '}')
        lapp = LApp <$> (char '(' *> skipSpace *> lettuce)    <*> (skipSpace *> lettuce <* skipSpace <* char ')')
        ltag = LTag <$> (char '[' *> skipSpace *> lettuce)    <*> (skipSpace *> lettuce <* skipSpace <* char ']')
        lsym = LSym <$> identifier

data LettuceBinding sym id = LBind {
  getNextIdL      :: id,
  getFreeSymbolsL :: Map.Map sym id,
  getSymbolTableL :: Map.Map id sym,
  getExpressionL  :: Lettuce id
} deriving Show

lbind :: (Eq sym, Ord sym, Ord id, Bounded id, Enum id) => Lettuce sym -> LettuceBinding sym id
lbind = rec Map.empty minBound Map.empty Map.empty
  where
    rec scope id free tab (LFun sym e) = LBind id' free' tab' (LFun id e')
      where (LBind id' free' tab' e') = rec (Map.insert sym id scope) (succ id) (Map.delete sym free) (Map.insert id sym tab) e
    rec scope id free tab (LApp f e) = pair scope id free tab LApp f e
    rec scope id free tab (LTag f e) = pair scope id free tab LTag f e
    rec scope id free tab (LSym sym) = case (Map.lookup sym free, Map.lookup sym scope) of
      (Nothing, Nothing) -> LBind (succ id) (Map.insert sym id free) (Map.insert id sym tab) (LSym id)
      (Just id', _)      -> LBind id free tab (LSym id')
      (_, Just id')      -> LBind id free tab (LSym id')
    pair scope id free tab c f e = LBind id'' free'' tab'' (c f' e')
      where (LBind id'  free'  tab'  f') = rec scope id  free  tab  f
            (LBind id'' free'' tab'' e') = rec scope id' free' tab' e
