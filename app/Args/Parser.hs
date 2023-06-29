module Args.Parser where

import Args.Doc
import Control.Monad.State
import Control.Monad.Trans.Maybe
import Data.Bifunctor (second)
import Data.Maybe
import Debug.Trace

data ArgState
    = SDoc
    | SArgs [String]
    deriving (Show)

data ArgRet a d
    = ADoc d
    | A a d
    | AFailed (String -> String)

instance (Show a, Show d) => Show (ArgRet a d) where
    show (ADoc doc) = "ADoc " ++ show doc
    show (A v doc) = "A " ++ show v ++ " " ++ show doc
    show _ = "Can't show functions"

type Parser a = MaybeT (State ArgState) a

pop :: Parser String
pop = do
    SArgs (x : xs) <- lift get
    lift $ put $ SArgs xs
    return x

peek :: Parser (Maybe String)
peek = do
    SArgs args <- lift get
    if null args
        then return Nothing
        else do
            return . Just $ head args

modes :: [(String, Parser (ArgRet a (CommandInfo, SubCommand)))] -> Parser (ArgRet a [(CommandInfo, SubCommand)])
modes ms = do
    let info =
            map
                ( \(s, p) -> case evalArgs p SDoc of
                    Just (ADoc inf) -> inf
                    _ -> undefined
                )
                ms
    let info' = return $ ADoc info
    a <- lift get
    case a of
        SArgs args -> do
            if null args
                then info'
                else case head args `lookup` ms of
                    Just m -> do
                        lift . put . SArgs $ tail args
                        r <- m
                        return $ case r of
                            A r _ -> A r info
                            ADoc (inf, cmd) -> AFailed $ subHelp inf cmd
                    _ -> info'
        SDoc -> info'

modesDefault :: [(String, Parser (ArgRet a (CommandInfo, SubCommand)))] -> Parser (ArgRet a (CommandInfo, SubCommand)) -> Parser (ArgRet a [(CommandInfo, SubCommand)])
modesDefault ms def = do
    let info =
            map
                ( \(s, p) -> case evalArgs p SDoc of
                    Just (ADoc inf) -> inf
                    _ -> undefined
                )
                ms
    let def' = do
            A r _ <- def
            return $ A r info
    a <- lift get
    case a of
        SArgs args -> do
            if null args
                then def'
                else case head args `lookup` ms of
                    Just m -> do
                        lift . put . SArgs $ tail args
                        r <- m
                        return $ case r of
                            A r _ -> A r info
                            ADoc (inf, cmd) -> AFailed $ subHelp inf cmd
                    _ -> def'
        SDoc -> return $ ADoc info

enum :: String -> String -> [(String, (a, String))] -> Parser (ArgRet a EnumInfo)
enum name desc es = do
    let info = EnumInfo name (map (\(_, (_, info)) -> info) es) desc
    let info' = return $ ADoc info
    a <- lift get
    case a of
        SArgs args -> do
            p <- peek
            case p of
                Just s -> case s `lookup` es of
                    Just (r, _) -> do
                        _ <- pop
                        return $ A r info
                    _ -> info'
                _ -> info'
        SDoc -> info'

enumDefault :: String -> String -> [(String, a)] -> a -> Parser (ArgRet a EnumInfo)
enumDefault name desc es def = do
    let info = EnumInfo name (map fst es) desc
    let info' = return $ ADoc info
    let def' = return $ A def info
    a <- lift get
    case a of
        SArgs args -> do
            p <- peek
            case p of
                Just s -> case s `lookup` es of
                    Just r -> do
                        _ <- pop
                        return $ A r info
                    _ -> def'
                _ -> def'
        SDoc -> info'

flag :: String -> Maybe String -> [String] -> a -> a -> Parser (ArgRet a OptionValue)
flag desc value names on off = do
    let info = OptionValue{oNames = names, value = value, oDesc = desc}
    let info' = return $ ADoc info
    let on' = A on info
    let off' = A off info
    a <- lift get
    case a of
        SArgs args ->
            if null args
                then return off'
                else
                    if head args `elem` names
                        then do
                            lift . put . SArgs $ tail args
                            return on'
                        else return off'
        SDoc -> info'

flagBool :: String -> [String] -> Parser (ArgRet Bool OptionValue)
flagBool desc names = flag desc Nothing names True False

option :: [String] -> Parser (Maybe String)
option names = do
    SArgs args <- get
    if length args < 2
        then return Nothing
        else do
            p <- peek
            case p of
                Just s ->
                    if s `elem` names
                        then do
                            _ <- pop
                            Just <$> pop
                        else return Nothing
                _ -> return Nothing

runArgs :: Parser a -> ArgState -> (Maybe a, ArgState)
runArgs = runState . runMaybeT

evalArgs :: Parser a -> ArgState -> Maybe a
evalArgs = evalState . runMaybeT
