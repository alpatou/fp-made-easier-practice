module Ch9 where

import Data.Generic.Rep (class Generic)
import Data.Semiring.Generic (genericAdd)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Console (log)
import Prelude (class Eq, class Show, Unit, discard, show, ($), (&&), (==))

test :: Effect Unit
test = do
    log $ show $ ATrue <> ATrue
    log $ show $ ATrue <> AFalse
    log $ show $ AFalse <> AFalse
    log $show $ mempty <> ATrue == ATrue
    log $ show $ mempty <> AFalse == ATrue
    verifyAndBoolSemigroup
    verifyAndBoolMonoid
    verifyOrBoolSemigroup
    verifyOrBoolMonoid


class Semigroup a where 
    append :: a -> a -> a

class Semigroup m <= Monoid m where
    mempty :: m

infixr 5 append as <>

instance semigroupAndBool :: Semigroup AndBool where
    append ATrue ATrue = ATrue
    append _ _ = AFalse 

instance monoidAndBool :: Monoid AndBool where
    mempty = ATrue

data AndBool = AFalse | ATrue  

derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _

instance showAndBool :: Show AndBool where
    show = genericShow

verifyAndBoolSemigroup :: Effect Unit
verifyAndBoolSemigroup = do 
    log "Verifying AndBool Semigroup Laws (1)"
    log $ show $ (AFalse <> ATrue) <> ATrue == AFalse <> (ATrue <> ATrue)

verifyAndBoolMonoid :: Effect Unit 
verifyAndBoolMonoid = do 
    log "Verifying AndBool Monoid Laws (2 tests)"
    log $ show $ mempty <> ATrue == ATrue <> mempty && ATrue <> mempty == ATrue
    log $ show $ mempty <> AFalse == AFalse <> mempty && AFalse <> mempty == AFalse



data OrBool = OFalse | OTrue

derive instance eqOrBool :: Eq OrBool
derive instance genericOrBool :: Generic OrBool _

instance showOrBool :: Show OrBool where
    show = genericShow

instance semigroupOrBool :: Semigroup OrBool where
  append OFalse OFalse = OFalse
  append _ _ = OTrue

instance monoidOrBool :: Monoid OrBool where
  mempty = OFalse

verifyOrBoolSemigroup :: Effect Unit
verifyOrBoolSemigroup = do
  log "Verifying semigroup laws for OrBool"
  log $ show $ (OFalse <> OTrue) <> OTrue == OFalse <> (OTrue <> OTrue)

verifyOrBoolMonoid :: Effect Unit
verifyOrBoolMonoid = do 
    log "Verifying OrBool Monoid Laws (2 tests)"
    log $show $ mempty <> OTrue == OTrue <> mempty && OTrue <> mempty == OTrue
    log $ show $ mempty <> OFalse == OFalse <> mempty && OFalse <> mempty == OFalse