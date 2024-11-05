module Consulta (submenuConsulta) where

import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import Text.Regex (mkRegex, matchRegex)
import Data.List (isInfixOf)

submenuConsulta :: IO ()
submenuConsulta = do
    putStrLn "\nMenu de Consultas"
    putStrLn "1. Cadastrar consulta"
    putStrLn "2. Buscar consulta"
    
    putStr "Escolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> cadastroConsulta 
        "2" -> putStrLn "Buscar consulta"  
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuConsulta  -- Chama novamente o submenu para o usuário tentar novamente.
         
cadastroConsulta :: IO ()
cadastroConsulta = do
    putStrLn "Cadastrar Consulta"

    cpfPaciente <- coletarCPF  
    pacienteExiste <- verificarDuplicidadeCPF cpfPaciente
    if not pacienteExiste then do 
        putStrLn "Paciente não cadastrado. Verifique o CPF."
        cadastroConsulta
        else return ()

    crmMedico <- coletarCRM  
    medicoExiste <- verificarDuplicidadeCRM crmMedico
    if not medicoExiste then do 
        putStrLn "Médico não cadastrado. Verifique o CRM."
        cadastroConsulta
        else return ()

    dataConsulta <- coletarDataConsulta  
    horarioConsulta <- coletarHorarioConsulta  
    statusConsulta <- coletarStatusConsulta  
    tipoConsulta <- coletarTipoConsulta  

    let novaConsulta = cpfPaciente ++ "|" ++ crmMedico ++ "|" ++ dataConsulta ++ "|" ++ horarioConsulta ++ "|" ++ statusConsulta ++ "|" ++ tipoConsulta ++ "\n"
    let cabecalho = "CPF|CRM|Data|Horário|Status|Tipo\n"

    -- Verifica se o arquivo existe, se não existir, cria um novo com o cabeçalho
    arquivoExistente <- doesFileExist "consultas.txt"
    if not arquivoExistente
        then writeFile "consultas.txt" (cabecalho ++ novaConsulta) 
        else appendFile "consultas.txt" novaConsulta 
    
    putStrLn "Consulta cadastrada com sucesso!"

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