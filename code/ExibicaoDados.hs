module ExibicaoDados (exibirDados) where

import System.IO (hFlush, stdout, appendFile, writeFile)
import System.Directory (doesFileExist)
import Data.List (isInfixOf)

-- Função para exibir o conteúdo dos arquivos filtrando pelas linhas que contêm a string CRM
exibirDados :: String -> IO ()
exibirDados key = do
    exibirDadosPacientes key
    exibirDadosMedicos key

-- Função para exibir os dados dos pacientes filtrando pelas linhas que contêm a string CRM
exibirDadosPacientes :: String -> IO ()
exibirDadosPacientes key = do
    putStrLn "\nDados dos Pacientes:"
    arquivoExistente <- doesFileExist "pacientes.txt"
    if arquivoExistente
        then do
            conteudo <- readFile "pacientes.txt"
            let linhasComCRM = filter (isInfixOf key) (lines conteudo)
            mapM_ putStrLn linhasComCRM
            putStrLn "\n"
        else putStrLn "Arquivo de pacientes não encontrado."

-- Função para exibir os dados dos médicos filtrando pelas linhas que contêm a string CRM
exibirDadosMedicos :: String -> IO ()
exibirDadosMedicos key = do
    putStrLn "\nDados dos Médicos:"
    arquivoExistente <- doesFileExist "medicos.txt"
    if arquivoExistente
        then do
            conteudo <- readFile "medicos.txt"
            let linhasComCRM = filter (isInfixOf key) (lines conteudo)
            mapM_ putStrLn linhasComCRM
            putStrLn "\n"
        else putStrLn "Arquivo de médicos não encontrado."