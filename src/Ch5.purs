module Ch5 where

import Prelude (Unit, (+), show, discard)
import Data.List (List(Nil, Cons), (:))
import Effect (Effect)
import Effect.Console (log) 
import Data.Maybe (Maybe(..))

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
