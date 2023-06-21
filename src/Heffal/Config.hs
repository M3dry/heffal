module Heffal.Config where
import qualified Data.Map.Strict as Map

data Config = Config
    { template_y :: Maybe String
    , template :: Maybe String
    , template_tmr :: Maybe String
    , directory :: String
    , editor :: Maybe String
    , styles :: Styles
    }

data Styles = Styles
    { bullet :: String
    , todo_state_conf :: TodoStateConf
    , todo_state :: Map.Map String String
    }

stylesDef :: Styles
stylesDef = Styles{ bullet = "-", todo_state_conf = todoStateConfDef, todo_state = Map.empty }

data TodoStateConf = TodoStateConf
    { empty :: String
    , brackets :: Bool
    }

todoStateConfDef :: TodoStateConf
todoStateConfDef = TodoStateConf{ empty = " ", brackets = True }