module Ch5 where

import Prelude (Unit, (+), (-) , (>=), (/=), (==), negate, show, discard, otherwise, type (~>))
import Data.List (List(Nil, Cons), (:))
import Effect (Effect)
import Effect.Console (log) 
import Data.Maybe (Maybe(..))
import Data.Ord ((<))

flip:: forall a b c. (a->b->c) -> b -> a -> c
flip func x y = func y x

const :: forall a b. a -> b -> a
const x _ = x

apply :: forall a b. (a->b) -> a -> b
apply f x = f x 
  
infixr 0 apply as $

applyFlipped :: forall a b. a -> (a -> b) -> b
applyFlipped = flip apply

infixl 0 applyFlipped as #

singleton :: forall a. a -> List a
singleton x = Cons x  Nil

null :: forall a. List a -> Boolean
null Nil  = true
null _ = false

snoc :: forall a . List a -> a -> List a
snoc Nil y = singleton y
snoc (x:xs) y = Cons x $ snoc xs y

length :: forall a. List a -> Int
length l = go 0 l where
  go :: Int -> List a -> Int
  go acc Nil = acc
  go acc ( _ : xs ) = go ( acc + 1 ) xs

-- in Haskell there is no null lists for head and tail
head :: forall a. List a -> Maybe a
head Nil = Nothing
head (x:_) = Just x

tail :: forall a. List a -> Maybe (List a)
tail Nil = Nothing
tail ( _ : xs ) = Just xs

last :: forall a. List a -> Maybe a
last Nil = Nothing
last (x:Nil) = Just x
last (_:xs) = last xs

init :: forall a. List a -> Maybe(List a)
init Nil = Nothing
init ( x : Nil ) = Just Nil
init l = Just $ go l where
  go Nil = Nil
  go (_ : Nil) = Nil
  go ( x : xs ) = x : go xs

uncons :: forall a. List a -> Maybe { head :: a, tail :: List a }
uncons Nil = Nothing
uncons (x:xs) = Just { head: x, tail: xs}

index :: forall a. List a -> Int -> Maybe a
index Nil _ = Nothing
index _ i | i < 0 = Nothing
index (x : _) 0 = Just x
index (_ : xs) i = index xs (i -1)

infixl 8 index as !!  

findIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findIndex _ Nil = Nothing
findIndex predicate l = go 0 l where
      go _ Nil = Nothing
      go index (x : xs) = if predicate x then Just index else go (index + 1) xs

findLastIndex :: ∀ a. (a -> Boolean) -> List a -> Maybe Int
findLastIndex _ Nil = Nothing
findLastIndex pred l =  go Nothing 0 l where 
      go :: Maybe Int -> Int -> List a -> Maybe Int
      go fi _ Nil = fi 
      go fi i (x : xs) = go (if pred x then Just i else fi) (i + 1) xs 

reverse:: List ~> List
reverse l = go Nil l where
  go newList Nil = newList
  go newList (x : xs) = go (x : newList) xs

concat :: forall a. List (List a) -> List a
concat Nil = Nil
concat (Nil : xss) = concat xss
concat ((x : xs) : xss) = x : concat (xs : xss)

test:: Effect Unit
test = do
  log $ show $ flip const 1 2
  flip const 1 2 # show # log
  log $ show $ singleton "xyz"
  log $ show $ null Nil
  log $ show $ null ("abc" : Nil)
  log $ show $ snoc (1 : 2 : Nil) 3
  log $ show $ length $ 1 : 2 : 3 : Nil 
  log $ show $ ( head Nil :: Maybe Unit )
  log $ show $ head ("abc" : "123" : Nil)
  log $ show $  tail (Nil :: List Unit )
  log $ show $ tail ("abc" : "123" : Nil)
  log $ show $ ( last Nil :: Maybe Unit )
  log $ show $ last $ "a" : "b" : "c" : Nil
  log $ show $ init (Nil :: List Unit)
  log $ show $ init ( 1 : Nil )
  log $ show $ init ( 1 : 2 : Nil )
  log $ show $ init ( 1 : 2 : 3 : Nil )
  log $ show $ uncons (1 : 2 : 3 : Nil)
  log $ show $ index (1 : Nil) 4 
  log $ show $ index (1 : 2 : 3 : Nil) 1
  log $ show $ index (Nil :: List Unit) 0 
  log $ show $ ( 1 : 2 : 3 : Nil ) !! 1
  log $ show $ findIndex (_ >= 2) (1 : 2 : 3 : Nil) 
  log $ show $ findIndex (_ >= 99) (1 : 2 : 3 : Nil) 
  log $ show $ findIndex (10 /= _) (Nil :: List Int) 
  log $ show $ findLastIndex (_ == 10) (Nil :: List Int)
  log $ show $ findLastIndex (_ == 10) (10 : 5 : 10 : -1 : 2 : 10 : Nil)
  log $ show $ findLastIndex (_ == 10) (11 : 12 : Nil)
  log $ show $ reverse (10 : 20 : 30 : Nil)
  log $ show $ concat ((1 : 2 : 3 : Nil) : (4 : 5 : Nil) : (6 : Nil) : (Nil) : Nil)
