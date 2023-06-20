{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Heffal.Parser (tokensToAST, File (..), Heading (..), HeadingContent (..), isText, isBullet, isTodo, heffal) where

import Debug.Trace

import Control.Monad.State (StateT, evalStateT, get, put)
import Data.Maybe
import Heffal.Lexer qualified as Lexer
import Heffal.Lexer (TextToken(..))
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

type Tokens = [Lexer.MarkupToken]

type ParseState a = StateT Tokens Maybe (Maybe a)

class Parse a where
    parse :: ParseState a

justRet :: Monad m => a -> m (Maybe a)
justRet = return . Just

pop :: ParseState Lexer.MarkupToken
pop = do
    tokens <- get
    if null tokens
        then return Nothing
        else do
            let first = head tokens
            put $ tail tokens
            justRet first

peek :: ParseState Lexer.MarkupToken
peek = do
    tokens <- get
    if null tokens
        then return Nothing
        else do
            justRet . head $ tokens

runUntil :: (Lexer.MarkupToken -> Bool) -> (Lexer.MarkupToken -> Bool) -> ParseState a -> ParseState [a]
runUntil cond skipF f =
    do
        tokens <- get
        if null tokens
            then justRet []
            else do
                tokens <- get
                if cond $ head tokens
                    then justRet []
                    else do
                        r <-
                            if skipF $ head tokens
                                then do
                                    Just _ <- pop
                                    return Nothing
                                else f
                        Just res <- runUntil cond skipF f
                        justRet $ case r of
                            Just r -> r : res
                            _ -> res

newtype File = File [Heading] deriving (Show)

instance Parse File where
    parse =
        do
            Just res <- runUntil (const False) (== Lexer.Newline) parse
            justRet . File $ res

data Heading = Heading {name :: [Lexer.TextToken], contents :: [HeadingContent]}
    deriving (Show)

instance Parse Heading where
    parse =
        do
            tokens <- get
            if head tokens == Lexer.Newline
                then return Nothing
                else do
                    Just Lexer.Heading <- pop
                    Just (Lexer.Text name) <- pop
                    tokens <- get
                    if null tokens
                        then do
                            justRet Heading{name, contents = []}
                        else do
                            Just Lexer.Newline <- pop
                            Just contents <- runUntil (== Lexer.Heading) (== Lexer.Newline) parse
                            justRet Heading{name, contents}

data HeadingContent
    = Text [Lexer.TextToken]
    | Bullet [Lexer.TextToken]
    | Todo {state :: String, text :: [Lexer.TextToken]}
    deriving (Show)

instance Parse HeadingContent where
    parse = do
        Just tok <- pop
        case tok of
            Lexer.Text text -> do
                Just Lexer.Newline <- pop
                justRet $ Text text
            Lexer.Bullet -> do
                Just (Lexer.Text text) <- pop
                Just Lexer.Newline <- pop
                justRet $ Bullet text
            Lexer.TodoOpen -> do
                Just (Lexer.TodoState state) <- pop
                Just Lexer.TodoClose <- pop
                Just (Lexer.Text text) <- pop
                Just Lexer.Newline <- pop
                justRet $ Todo{state, text}
            _ -> return Nothing

isText :: HeadingContent -> Bool
isText (Text _) = True
isText _ = False

isBullet :: HeadingContent -> Bool
isBullet (Bullet _) = True
isBullet _ = False

isTodo :: HeadingContent -> Bool
isTodo Todo{} = True
isTodo _ = False

tokensToAST :: Tokens -> Maybe File
tokensToAST tokens = fromMaybe Nothing (evalStateT parse tokens)

heffal =
    QuasiQuoter
        { quoteExp = parseStr
        , quotePat = err "Patterns"
        , quoteType = err "Types"
        , quoteDec = err "Declarations"
        }
  where
    err name = error $ name ++ " are not handled by the heffal quasiquoter."

instance Lift File where
    lift (File xs) = [| File xs |]

instance Lift Heading where
    lift (Heading{name, contents}) = [| Heading{name, contents} |]

instance Lift HeadingContent where
    lift (Text text) = [| Text text |]
    lift (Bullet text) = [| Bullet text |]
    lift (Todo{state, text}) = [| Todo{state, text} |]

instance Lift TextToken where
    lift (Pure str) = [| Pure str |]
    lift (Verbatim text) = [| Verbatim text |]
    lift (Underline text) = [| Underline text |]
    lift (Crossed text) = [| Crossed text |]
    lift (Bold text) = [| Bold text |]
    lift (Italic text) = [| Italic text |]

parseStr :: String -> Q Exp
parseStr str = case tokensToAST $ Lexer.strToTokens str of
    Just ast -> [|ast|]
    Nothing -> fail $ "Failed to generate the AST for string: " ++ show str
