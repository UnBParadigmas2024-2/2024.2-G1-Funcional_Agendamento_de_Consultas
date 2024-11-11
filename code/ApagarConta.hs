module ApagarConta (apagarConta) where

import System.IO (readFile, writeFile, hGetContents,hPutStr, hIsEOF, hGetLine, openFile,hSeek, SeekMode( AbsoluteSeek ) , hClose,hPutStrLn, IOMode(ReadMode, ReadWriteMode,WriteMode))
import Data.List (isInfixOf)
import Control.Exception (catch, IOException)
import System.Directory (renameFile, removeFile)


apagarConta :: String -> String -> IO ()
apagarConta cpf arquivo = do
    let originalFile = arquivo++".txt"
    let tempFile = arquivo++".txt.tmp"
    
    -- Read from original file
    contents <- readFileStrict originalFile
    
    -- Filter the data
    let dados = filter (\linha -> not (cpf `isInfixOf` linha)) contents
    print dados
    
    -- Write to temp file
    writeFile tempFile (unlines dados)
    
    catch 
        (do
            removeFile originalFile
            renameFile tempFile originalFile
            putStrLn "Conta apagada com sucesso!")
        (\e -> do
            let err = show (e :: IOException)
            putStrLn $ "Erro ao atualizar arquivo: " ++ err)


readFileStrict :: FilePath -> IO [String]
readFileStrict filepath = do
    handle <- openFile filepath ReadMode
    contents <- readLines handle []
    hClose handle
    return (reverse contents)
  where
    readLines h acc = do
        eof <- hIsEOF h
        if eof 
            then return acc
            else do
                line <- hGetLine h
                readLines h (line:acc)