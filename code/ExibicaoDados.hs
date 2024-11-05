module ExibicaoDados (exibicaoDados) where

import System.IO (hFlush, stdout, appendFile, writeFile)
import System.Directory (doesFileExist)
import Text.Regex (mkRegex, matchRegex)
import Data.List (isInfixOf)

-- Função para exibir o conteúdo dos arquivos
exibirDados :: IO ()
exibirDados = do
    exibirDadosPacientes
    exibirDadosMedicos

-- Função para exibir os dados dos pacientes
exibirDadosPacientes :: IO ()
exibirDadosPacientes = do
    putStrLn "\nDados dos Pacientes:"
    arquivoExistente <- doesFileExist "pacientes.txt"
    if arquivoExistente
        then do
            conteudo <- readFile "pacientes.txt"
            putStrLn conteudo
        else putStrLn "Arquivo de pacientes não encontrado."

-- Função para exibir os dados dos médicos
exibirDadosMedicos :: IO ()
exibirDadosMedicos = do
    putStrLn "\nDados dos Médicos:"
    arquivoExistente <- doesFileExist "medicos.txt"
    if arquivoExistente
        then do
            conteudo <- readFile "medicos.txt"
            putStrLn conteudo
        else putStrLn "Arquivo de médicos não encontrado."