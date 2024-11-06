module ExibicaoDados (exibirDados) where

import System.IO (hFlush, stdout, appendFile, writeFile)
import System.Directory (doesFileExist)
import Data.List (isInfixOf)

-- Eu tenho que receber como parâmetro a string do CRM, assim que receber eu tenho que printar apenas onde o CRM aparecer, a mesma coisa com o CPF do paciente

-- Função para exibir o conteúdo dos arquivos
exibirDados :: String -> IO ()
exibirDados crm = do
    exibirDadosPacientes crm
    exibirDadosMedicos crm

-- Função para exibir os dados dos pacientes
exibirDadosPacientes :: String -> IO ()
exibirDadosPacientes crm = do
    putStrLn "\nDados dos Pacientes:"
    putStrLn crm
    arquivoExistente <- doesFileExist "pacientes.txt"
    if arquivoExistente
        then do
            conteudo <- readFile "pacientes.txt"
            putStrLn conteudo
        else putStrLn "Arquivo de pacientes não encontrado."

-- Função para exibir os dados dos médicos
exibirDadosMedicos :: String -> IO ()
exibirDadosMedicos crm = do
    putStrLn "\nDados dos Médicos:"
    putStrLn crm
    arquivoExistente <- doesFileExist "medicos.txt"
    if arquivoExistente
        then do
            conteudo <- readFile "medicos.txt"
            putStrLn conteudo
        else putStrLn "Arquivo de médicos não encontrado."