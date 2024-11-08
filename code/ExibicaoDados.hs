module ExibicaoDados (exibirDados, exibirDadosPacientes, exibirDadosMedicos, atualizarDadosMedico, atualizarDadosPaciente) where

import System.IO (hFlush, stdout, appendFile, writeFile)
import System.Directory (doesFileExist, removeFile, renameFile)
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

-- Função para atualizar dados de um médico
atualizarDadosMedico :: String -> IO ()
atualizarDadosMedico key = do
    putStrLn "\nAtualizando dados do médico:"
    putStrLn "Digite o novo nome do médico:"
    nomeMedico <- getLine
    putStrLn "Digite a nova senha:"
    senha <- getLine
    putStrLn "Digite a nova especialidade:"
    especialidade <- getLine

    -- Formata os novos dados do médico
    let novosDados = nomeMedico ++ "|" ++ senha ++ "|" ++ key ++ "|" ++ especialidade

    -- Verifica se o arquivo de médicos existe
    arquivoExistente <- doesFileExist "medicos.txt"
    if arquivoExistente
        then do
            -- Lê o conteúdo do arquivo e atualiza a linha correspondente
            conteudo <- readFile "medicos.txt"
            let linhas = lines conteudo
            let linhasAtualizadas = map (\linha -> if key `isInfixOf` linha then novosDados else linha) linhas
            -- Salva as mudanças reescrevendo o arquivo
            writeFile "medicos_temp.txt" (unlines linhasAtualizadas)
            removeFile "medicos.txt"
            renameFile "medicos_temp.txt" "medicos.txt"
            putStrLn "Dados do médico atualizados com sucesso.\n"
        else putStrLn "Arquivo de médicos não encontrado."
