
module Utils where

--------------------------------------------------------------------------------
import Prelude ((==),($),(<>),map)
import Data.List as List
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML.Properties as HP

--------------------------------------------------------------------------------

reportKey :: forall r i. String -> HP.IProp r i
reportKey = HP.attr (H.AttrName "data-ev")

reportKeyVals :: forall r i. Array (Tuple String String) -> HP.IProp r i
reportKeyVals items = HP.attr (H.AttrName "data-kv") str
  where
    str :: String
    str = List.intercalate ";" (map pair items)

    pair :: Tuple String String -> String
    pair  (Tuple k v) = k <> "=" <> v

kv :: String -> String -> Tuple String String
kv k v = Tuple k v
