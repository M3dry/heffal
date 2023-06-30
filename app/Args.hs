module Args (args, Command, Day, Args) where

import Options.Applicative

data Command
    = Show
    | Edit
    | New Bool
    deriving (Show)

data Day
    = Today
    | Tomorrow
    | Yesterday
    deriving (Show)

instance Read Day where
    readsPrec _ "t" = [(Today, "")]
    readsPrec _ "tmr" = [(Tomorrow, "")]
    readsPrec _ "y" = [(Yesterday, "")]
    readsPrec _ _ = []

data Args
    = Todo Day Command
    | File String Command
    deriving (Show)

commandP :: Parser Command
commandP =
    subparser
        ( command "show" (info (pure Show) (progDesc "Print out the formatted contents of a file"))
       <> command "edit" (info (pure Edit) (progDesc "Open a file in an editor"))
       <> command
            "new"
            ( info
                ((New <$> switch (long "edit" <> short 'e' <> help "Open the file in an editor")) <**> helper)
                (progDesc "Create a new file")
            )
        )

fileP :: Parser Args
fileP = File <$> strArgument (metavar "FILE" <> action "file") <*> commandP

todoP :: Parser Args
todoP = Todo <$> argument auto (metavar "DAY" <> help "Which todo file [t,tmr,y]" <> completeWith ["t", "tmr", "y"]) <*> commandP

argsP :: Parser Args
argsP =
    subparser
        ( command
            "todo"
            (info (todoP <**> helper) (progDesc "Perform an action on a todo file"))
       <> command
           "file"
           (info (fileP <**> helper) (progDesc "Perform an action on a file"))
        )

args :: IO Args
args = execParser $ info (argsP <**> helper) (fullDesc <> header "Heffal - a cli client for heffal")
