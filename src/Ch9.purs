module Ch9 where
import Prelude (Unit, class Show, class Eq, ($), discard, show) 
import Effect (Effect)
import Effect.Console (log)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

test :: Effect Unit
test = do
    log $ show $ ATrue <> ATrue
    log $ show $ ATrue <> AFalse
    log $ show $ AFalse <> AFalse


class Semigroup a where 
    append :: a -> a -> a

infixr 5 append as <>


class Semigroup m <= Monoid m where
    mempty :: m

data AndBool = AFalse | ATrue  

derive instance eqAndBool :: Eq AndBool
derive instance genericAndBool :: Generic AndBool _



instance showAndBool :: Show AndBool where
    show = genericShow
