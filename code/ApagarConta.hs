module ApagarConta (apagarConta, apagarContaMedico) where

import System.IO (readFile, writeFile, hGetContents,hPutStr, hIsEOF, hGetLine, openFile,hSeek, SeekMode( AbsoluteSeek ) , hClose,hPutStrLn, IOMode(ReadMode, ReadWriteMode,WriteMode))
import Data.List (isInfixOf)
import Control.Exception (catch, IOException)
import System.Directory (renameFile, removeFile)

apagarConta :: String -> IO ()
apagarConta cpf = do
    let originalFile = "pacientes.txt"
    let tempFile = "pacientes.txt.tmp"
    
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


apagarContaMedico :: String -> IO ()
apagarContaMedico cpf = do
    let originalFile = "medicos.txt"
    let tempFile = "medicos.txt.tmp"
    
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
    