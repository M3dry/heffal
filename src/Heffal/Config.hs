module Heffal.Config where

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
    , todo_state :: [(String, String)]
    }

stylesDef :: Styles
stylesDef = Styles{ bullet = "-", todo_state_conf = todoStateConfDef, todo_state = [] }

data TodoStateConf = TodoStateConf
    { empty :: String
    , brackets :: Bool
    }

todoStateConfDef :: TodoStateConf
todoStateConfDef = TodoStateConf{ empty = " ", brackets = True }
