module Agenda (buscaConsultas) where

import System.IO (hFlush, stdout, readFile)
import System.Directory (doesFileExist)
import Text.Regex (mkRegex, matchRegex)
import Data.List (isInfixOf, partition, find)
import Util (validadorCpf, validadorData, validadorFormatoCRM)
import Consulta (cadastroConsulta)

-- Função para iniciar a busca
buscaConsultas :: String -> IO ()
buscaConsultas cpf = do
    putStrLn "\nDigite a opção que prefere:"
    putStrLn "1. Agendar nova consulta"
    putStrLn "2. Buscar por todas as suas consultas"
    putStrLn "3. Buscar por Data"
    putStrLn "4. Buscar por Médico"
    putStrLn "5. Voltar para Menu Principal"  
    putStr "Digite sua escolha: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do  
            cadastroConsulta cpf
            buscaConsultas cpf
        "2" -> do 
            buscarPorCpf cpf
            buscaConsultas cpf  

        "3" -> do   
            putStr "Informe a data (DD/MM/AAAA): "
            hFlush stdout
            dataConsulta <- getLine
            if validadorData dataConsulta
                then do
                    buscarPorData dataConsulta 
                    buscaConsultas cpf
                else do
                    putStrLn "Data inválida"
                    buscaConsultas cpf 

        "4" -> do
            putStr "Informe o CRM ou nome do médico: "
            hFlush stdout
            valor <- getLine
            buscarPorMedico valor
            buscaConsultas cpf
        
        "5" -> return ()  -- Retornando para o Menu Paciente sem importar diretamente o Paciente

-- Função para buscar consultas por CPF do paciente
buscarPorCpf :: String -> IO ()
buscarPorCpf cpf = buscar cpf "consultas.txt"

-- Função para buscar consultas por data específica
buscarPorData :: String -> IO ()
buscarPorData dataConsulta = buscar dataConsulta "consultas.txt"

-- Função para buscar consultas por CRM ou nome do médico
buscarPorMedico :: String -> IO ()
buscarPorMedico valor = do
    maybeMedico <- obterMedico valor
    case maybeMedico of
        Just (nome, especialidade) -> do
            putStrLn $ "Médico: " ++ nome
            putStrLn $ "Especialidade: " ++ especialidade
            buscar valor "consultas.txt"  -- Passa o nome ou CRM para buscar
        Nothing -> putStrLn "Médico não encontrado."

-- Função genérica para buscar dados em arquivo
buscar :: String -> FilePath -> IO ()
buscar val filePath = do
    conteudo <- readFile filePath
    let (linhasEncontradas, _) = partition (\linha -> val `isInfixOf` linha) (lines conteudo)
    if null linhasEncontradas
        then putStrLn "Nenhuma consulta encontrada."
        else mapM_ exibirConsulta linhasEncontradas

-- Função para exibir consulta formatada
exibirConsulta :: String -> IO ()
exibirConsulta linha = do
    let [nomePaciente, cpf, crm, nomeMedico, especialidade, dataConsulta, horario, status, tipo] = wordsWhen (== '|') linha
    putStrLn "\n\n####CONSULTA####"
    putStrLn $ "Nome paciente: " ++ nomePaciente
    putStrLn $ "CPF: " ++ cpf
    putStrLn $ "CRM: " ++ crm
    putStrLn $ "Médico: " ++ nomeMedico
    putStrLn $ "Especialidade: " ++ especialidade
    putStrLn $ "Data: " ++ dataConsulta
    putStrLn $ "Horário: " ++ horario
    putStrLn $ "Status: " ++ status
    putStrLn $ "Modalidade: " ++ tipo

-- Função para dividir uma string com base em um delimitador
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'

-- Função para obter informações do médico (por nome ou CRM)
obterMedico :: String -> IO (Maybe (String, String))  
obterMedico valor = do
    conteudo <- readFile "medicos.txt"
    let medicos = map parseMedico (lines conteudo)
    return $ find (\(nome, crm) -> nome == valor || crm == valor) medicos

-- Função para processar dados do médico
parseMedico :: String -> (String, String)  -- Retorna nome e CRM
parseMedico linha = 
    let [nome, _, crm, especialidade] = wordsWhen (== '|') linha
    in (nome, crm)  -- Retorna nome e CRM
