module Args.Doc where

data Doc
    = Doc
        { dName :: String
        , version :: String
        , summary :: String
        , usage :: Usage
        }
    deriving (Show)

newtype SubCommand = SubCommand { subCmdUsage :: Usage } deriving (Show)

data Usage = Usage
    { options :: [OptionValue]
    , enums :: [EnumInfo]
    , commands :: [(CommandInfo, SubCommand)]
    }
    deriving (Show)

data OptionValue = OptionValue
    { oNames :: [String]
    , value :: Maybe String
    , oDesc :: String
    }
    deriving (Show)

data EnumInfo = EnumInfo
    { eName :: String
    , values :: [String]
    , eDesc :: String
    }
    deriving (Show)

data CommandInfo = CommandInfo
    { cName :: String
    , cDesc :: String
    }
    deriving (Show)

help :: Doc -> String
help doc = ""

subHelp :: CommandInfo -> SubCommand -> String -> String
subHelp info cmd path = ""
