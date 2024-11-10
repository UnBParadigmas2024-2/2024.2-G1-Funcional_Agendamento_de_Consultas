module Consulta (submenuConsulta, cadastroConsulta, buscarConsulta) where

import System.IO (hFlush, stdout, readFile, writeFile)
import System.Directory (doesFileExist)
import Text.Regex (mkRegex, matchRegex)
import Data.List (isInfixOf, partition)
import Util(validadorCpf, validadorData, validadorFormatoCRM, escolherMedico, horariosDisponiveis)

submenuConsulta :: IO ()
submenuConsulta = do
    putStrLn "\nMenu de Consultas"
    putStrLn "1. Cadastrar consulta"
    putStrLn "2. Buscar consulta"
    
    putStr "\nEscolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        --"1" -> cadastroConsulta 
        --"2" -> buscarConsulta
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuConsulta  -- Chama novamente o submenu para o usuário tentar novamente.
         
cadastroConsulta :: String -> IO ()
cadastroConsulta cpfPaciente = do
    putStrLn "\n\nCadastrar Consulta"

    crmMedico <- escolherMedico

    putStrLn "Digite a data desejada (DD/MM/AAAA):"
    dataDesejada <- getLine
    
    if not (validadorData dataDesejada)
        then putStrLn "Formato de data inválido. Use DD/MM/AAAA."
        else do
            horarios <- horariosDisponiveis dataDesejada crmMedico
            if null horarios
                then putStrLn "Não há horários disponíveis para essa data."
                else do
                    horarioEscolhido <- exibirHorariosESelecionar horarios
                    case horarioEscolhido of
                        Just horario -> putStrLn $ "Horário escolhido: " ++ horario
                        Nothing -> putStrLn "Escolha inválida."


--    dataConsulta <- coletarDataConsulta  
--    horarioConsulta <- coletarHorarioConsulta  
--    statusConsulta <- coletarStatusConsulta  
--    tipoConsulta <- coletarTipoConsulta  
--
--    let novaConsulta = cpfPaciente ++ "|" ++ crmMedico ++ "|" ++ dataConsulta ++ "|" ++ horarioConsulta ++ "|" ++ statusConsulta ++ "|" ++ tipoConsulta ++ "\n"
--    let cabecalho = "CPF|CRM|Data|Horário|Status|Tipo\n"
--
--    -- Verifica se o arquivo existe, se não existir, cria um novo com o cabeçalho
--    arquivoExistente <- doesFileExist "consultas.txt"
--    if not arquivoExistente
--        then writeFile "consultas.txt" (cabecalho ++ novaConsulta) 
--        else appendFile "consultas.txt" novaConsulta 
--    
--    putStrLn "Consulta cadastrada com sucesso!"

-- Exibe a lista enumerada de horários e permite escolher um
exibirHorariosESelecionar :: [String] -> IO (Maybe String)
exibirHorariosESelecionar horarios = do
    putStrLn "Horários disponíveis:"
    mapM_ (\(i, horario) -> putStrLn $ show i ++ ". " ++ horario) (zip [1..] horarios)
    putStrLn "Digite o número do horário desejado:"
    escolha <- getLine
    let indice = read escolha - 1
    if indice >= 0 && indice < length horarios
        then return $ Just (horarios !! indice)
        else return Nothing


-- Função para coletar a data da consulta
coletarDataConsulta :: IO String
coletarDataConsulta = do
    putStrLn "Digite a data da consulta (DD/MM/AAAA):"
    getLine

-- Função para coletar o horário da consulta
coletarHorarioConsulta :: IO String
coletarHorarioConsulta = do
    putStrLn "Digite o horário da consulta (HH:MM):"
    getLine

-- Função para coletar o status da consulta
coletarStatusConsulta :: IO String
coletarStatusConsulta = do
    putStrLn "Digite o status da consulta (agendada, concluída, cancelada):"
    status <- getLine
    if status `elem` ["agendada", "concluída", "cancelada"]
        then return status
        else do
            putStrLn "Status inválido. Tente novamente."
            coletarStatusConsulta

-- Função para coletar o tipo da consulta
coletarTipoConsulta :: IO String
coletarTipoConsulta = do
    putStrLn "Digite o tipo da consulta (presencial, online, de rotina, urgência):"
    tipo <- getLine
    if tipo `elem` ["presencial", "online", "rotina", "urgência"]
        then return tipo
        else do
            putStrLn "Tipo inválido. Tente novamente."
            coletarTipoConsulta


---------------------------------------------------------------------------------------------------------------------
verificarDuplicidadeCPF :: String -> IO Bool
verificarDuplicidadeCPF cpf = do
    arquivoExistente <- doesFileExist "pacientes.txt"
    if not arquivoExistente
        then return False  -- Se o arquivo não existir, o CPF não pode estar cadastrado
        else do
            conteudo <- readFile "pacientes.txt"
            let linhas = lines conteudo
            return $ any (\linha -> cpf `isInfixOf` linha) linhas

-- Função para verificar duplicidade de CRM
verificarDuplicidadeCRM :: String -> IO Bool
verificarDuplicidadeCRM crm = do
    arquivoExistente <- doesFileExist "medicos.txt"
    if not arquivoExistente
        then return False
        else do
            conteudo <- readFile "medicos.txt"
            let linhas = lines conteudo
            return $ any (\linha -> crm `isInfixOf` linha) linhas


-- Função para coletar o CPF com validação de 11 dígitos e verificar duplicidade
coletarCPF :: IO String
coletarCPF = do
    putStrLn "Digite o CPF do paciente (11 dígitos):"
    cpf <- getLine
    cpfExiste <- verificarDuplicidadeCPF cpf

    if length cpf == 11 && all (`elem` ['0'..'9']) cpf && cpfExiste
        then return cpf
        else do
            if not cpfExiste
                then putStrLn "CPF não cadastrado. Insira um CPF válido."
                else putStrLn "CPF inválido. Certifique-se de que tem exatamente 11 dígitos numéricos."
            coletarCPF

-- Função para coletar o CRM no formato "CRM/XX 123456" e verificar duplicidade
coletarCRM :: IO String
coletarCRM = do
    putStrLn "Digite o CRM do médico (formato: CRM/XX 123456, ex.: CRM/SP 123456):"
    crm <- getLine
    crmExiste <- verificarDuplicidadeCRM crm

    if validarFormatoCRM crm && crmExiste
        then return crm
        else do
            if not crmExiste
                then putStrLn "CRM não cadastrado. Insira um CRM válido"
                else putStrLn "CRM inválido. Certifique-se de que segue o formato correto (CRM/XX 123456)."
            coletarCRM

-- Função para validar o formato do CRM
validarFormatoCRM :: String -> Bool
validarFormatoCRM crm =
    let regex = mkRegex "^CRM/[A-Z]{2} [0-9]{6}$"  -- Expressão regular para "CRM/XX 123456"
    in case matchRegex regex crm of
        Just _ -> True
        Nothing -> False

buscar :: String -> String -> IO ()
buscar val1 val2 = do
    conteudo <- readFile "consultas.txt"
    exibeCabecalho
    let (linhas, dados) = partition (\linha -> val1 `isInfixOf` linha && val2 `isInfixOf` linha) (lines conteudo)
    mapM_ putStrLn linhas

buscarTodos :: String -> IO ()
buscarTodos val = do
    conteudo <- readFile "consultas.txt"
    exibeCabecalho
    let (linhas, dados) = partition (\linha -> val `isInfixOf` linha) (lines conteudo)
    mapM_ putStrLn linhas

buscarConsulta :: String -> IO ()
buscarConsulta cpf = do
    putStrLn "\nBuscar por:"
    putStrLn "1. Data"
    putStrLn "2. Médico"
    putStrLn "3. Todas as consultas"
    putStrLn "0. Voltar"
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do 
            putStr "Informe a data (DD/MM/AAAA):"
            hFlush stdout
            dataConsulta <- getLine

            if validadorData dataConsulta
                then buscar dataConsulta cpf
            else do
                putStrLn "Data inválida"
                buscarConsulta cpf
        "2" -> do
            putStr "Informe o crm do médico (CRM/XX 123456): "
            hFlush stdout
            crmMed <- getLine
            if validadorFormatoCRM crmMed
                then buscar crmMed cpf
            else do 
                putStrLn "CRM inválido"
                buscarConsulta cpf
            
        "3" -> buscarTodos cpf
            -- putStr "Informe o CPF do paciente: "
            -- hFlush stdout
            -- cpf <- getLine

            -- if validadorCpf cpf
            --     then buscar cpf
            -- else do 
            --     putStrLn "CPF inválido"
            --     buscarConsulta
        "0" -> putStrLn "Voltando..."
        _   -> do 
            putStr "Opção inválida. Tente novamente."
            buscarConsulta cpf


exibeCabecalho :: IO ()
exibeCabecalho = putStrLn "CPF|CRM|Data|Horário|Status|Tipo"