module Heffal.Config where
import Data.Map

data Config = Config
    { template_y :: Maybe String
    , template :: Maybe String
    , template_tmr :: Maybe String
    , directory :: String
    , editor :: Maybe String
    , styles :: Styles
    }

data Styles = Styles
    { bullet :: Maybe String
    , todo_state_conf :: TodoStateConf
    , todo_state :: Map String String
    }

data TodoStateConf = TodoStateConf
    { empty :: String
    , brackets :: Bool
    }
