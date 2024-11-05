module Agenda (buscaConsultas) where

import System.IO (hFlush, stdout, readFile, writeFile)
import System.Directory (doesFileExist)
import Text.Regex (mkRegex, matchRegex)
import Data.List (isInfixOf, partition)
import Util(validadorCpf, validadorData, validadorFormatoCRM)

-- Função para iniciar a busca
buscaConsultas :: String -> IO ()
buscaConsultas cpf = do
    putStrLn "Buscar:"
    putStrLn "1. Por todas as suas consultas"
    putStrLn "2. Por Data"
    putStrLn "3. Por Médico"
    putStr "Digite sua escolha: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do 
            buscar cpf
            buscaConsultas cpf  

        "2" -> do 
            putStr "Informe a data (DD/MM/AAAA): "
            hFlush stdout
            dataConsulta <- getLine

            if validadorData dataConsulta
                then buscar dataConsulta
                else do
                    putStrLn "Data inválida"
                    buscaConsultas cpf 

        "3" -> do
            putStr "Informe o CRM do médico (CRM/XX 123456): "
            hFlush stdout
            crmMed <- getLine
            if validadorFormatoCRM crmMed
                then buscar crmMed
                else do 
                    putStrLn "CRM inválido"
                    buscaConsultas cpf

buscar :: String -> IO ()
buscar val = do
    conteudo <- readFile "consultas.txt"
    let (linhasApagadas, dados) = partition (\linha -> val `isInfixOf` linha) (lines conteudo)
    mapM_ putStrLn linhasApagadas
