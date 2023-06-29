module Args where

import Args.Doc qualified as Doc
import Args.Parser
import Control.Monad.State
import Data.Maybe
import Debug.Trace

data Command
  = New {editor :: Bool}
  | Show
  | Edit
  deriving (Show)

commandP =
  modesDefault
    [ ( "new",
        let docTemplate doc =
              ( Doc.CommandInfo "new" "Create a new heffal file",
                Doc.SubCommand
                  { Doc.subCmdUsage =
                      Doc.Usage
                        { Doc.options = [doc],
                          Doc.enums = [],
                          Doc.commands = []
                        }
                  }
              )
         in do
              args <- get
              f <- flagBool "Open file in an editor" ["-e", "--edit"]
              return $ case f of
                A e docF -> A (New e) $ docTemplate docF
                ADoc docF -> ADoc $ docTemplate docF
      ),
      ( "show",
        showP
      ),
      ( "edit",
        let docTemplate =
              ( Doc.CommandInfo "edit" "Edit a heffal file",
                Doc.SubCommand
                  { Doc.subCmdUsage =
                      Doc.Usage
                        { Doc.options = [],
                          Doc.enums = [],
                          Doc.commands = []
                        }
                  }
              )
         in do
              a <- get
              return $ case a of
                SDoc -> ADoc docTemplate
                _ -> A Edit docTemplate
      )
    ] showP
  where
    showP =
      let docTemplate =
            ( Doc.CommandInfo "show" "Render a heffal file",
              Doc.SubCommand
                { Doc.subCmdUsage =
                    Doc.Usage
                      { Doc.options = [],
                        Doc.enums = [],
                        Doc.commands = []
                      }
                }
            )
       in do
            a <- get
            return $ case a of
              SDoc -> ADoc docTemplate
              _ -> A Show docTemplate

data Day
  = Today
  | Yesterday
  | Tomorrow
  deriving (Show)

dayP = enumDefault "DAY" "from which day" [("y", Yesterday), ("t", Today), ("tmr", Tomorrow)] Today

data Args
  = Todo {day :: Day, command :: Command}
  | File {fName :: String, command :: Command}
  deriving (Show)

todoP =
  let docTemplate day cmd =
        ( Doc.CommandInfo
            { Doc.cName = "todo",
              Doc.cDesc = "Todo actions"
            },
          Doc.SubCommand
            { Doc.subCmdUsage =
                Doc.Usage
                  { Doc.options = [],
                    Doc.enums = [day],
                    Doc.commands = cmd
                  }
            }
        )
   in do
        d <- dayP
        c <- commandP
        let (day, dDoc) = case d of
                            A day doc -> (Just day, docTemplate doc)
                            ADoc doc -> (Nothing, docTemplate doc)
        let (cmd, doc) = case c of
                            A cmd doc -> (Just cmd, dDoc doc)
                            ADoc doc -> (Nothing, dDoc doc)
        return $ case (day, cmd) of
            (Just d, Just c) -> A (Todo d c) doc
            _ -> ADoc doc

fileP =
  let docTemplate doc =
        ( Doc.CommandInfo {Doc.cName = "-f", Doc.cDesc = "File actions"},
          Doc.SubCommand
            { Doc.subCmdUsage =
                Doc.Usage
                  { Doc.options = [],
                    Doc.enums = [],
                    Doc.commands = doc
                  }
            }
        )
   in do
        a <- get
        name <- case a of
          SDoc -> return Nothing
          SArgs args -> do
            Just <$> pop
        cmd <- commandP
        let (r, doc) = case cmd of
              ADoc doc -> (Nothing, doc)
              A r doc -> (Just r, doc)
        return $ case (name, r) of
          (Just name, Just cmd) -> A (File name cmd) $ docTemplate doc
          _ -> ADoc $ docTemplate doc

argsP =
  let docTemplate doc =
        Doc.Doc
          { Doc.dName = "Heffal",
            Doc.version = "0.1.0",
            Doc.summary = "Heffal client",
            Doc.usage =
              Doc.Usage
                { Doc.options = [],
                  Doc.enums = [],
                  Doc.commands = doc
                }
          }
   in do
        m <- modesDefault [("-f", fileP), ("todo", todoP)] todoP
        return $ case m of
          A r cmd -> A r $ docTemplate cmd
          ADoc cmd -> AFailed $ const (Doc.help $ docTemplate cmd)
          AFailed str -> AFailed str
