module Agenda (buscaConsultas) where

import System.IO (hFlush, stdout, readFile)
import System.Directory (doesFileExist)
import Data.List (isInfixOf, partition, nub, (\\))
import Util (validadorCpf, validadorData, validadorFormatoCRM)

-- Lista de horários padrão (08:00 - 18:00) com intervalos de 30 minutos
horariosPadrao :: [String]
horariosPadrao = ["08:00", "08:30", "09:00", "09:30", "10:00", "10:30", 
                  "11:00", "11:30", "12:00", "12:30", "13:00", "13:30", 
                  "14:00", "14:30", "15:00", "15:30", "16:00", "16:30", 
                  "17:00", "17:30", "18:00"]

-- Função para iniciar a busca
buscaConsultas :: String -> IO ()
buscaConsultas cpf = do
    putStrLn "Buscar:"
    putStrLn "1. Por todas as suas consultas (agendadas, concluídas e canceladas)"
    putStrLn "2. Por Data"
    putStrLn "3. Por Médico"
    putStrLn "4. Ver todos os horários disponíveis"  -- EXIBIR APENAS SE FOR MÉDICO
    putStr "Digite sua escolha: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do 
            buscarPorCpf cpf
            buscaConsultas cpf  

        "2" -> do    --- ESTÁ VOLTANDO PARA O MENU PRINCIPAL! Implementar a função de sair. 
            putStr "Informe a data (DD/MM/AAAA): "
            hFlush stdout
            dataConsulta <- getLine
            if validadorData dataConsulta
                then buscarPorData dataConsulta
                else do
                    putStrLn "Data inválida"
                    buscaConsultas cpf 

        "3" -> do
            putStr "Informe o CRM do médico (CRM/XX 123456): "
            hFlush stdout
            crmMed <- getLine
            if validadorFormatoCRM crmMed
                then buscarPorMedico crmMed
                else do 
                    putStrLn "CRM inválido"
                    buscaConsultas cpf

        "4" -> verHorariosDisponiveis

-- Função para buscar consultas por CPF do paciente
buscarPorCpf :: String -> IO ()
buscarPorCpf cpf = buscar cpf "consultas.txt"

-- Função para buscar consultas por data específica
buscarPorData :: String -> IO ()
buscarPorData dataConsulta = buscar dataConsulta "consultas.txt"

-- Função para buscar consultas por CRM do médico
buscarPorMedico :: String -> IO ()
buscarPorMedico crmMed = buscar crmMed "consultas.txt"

-- Função genérica para buscar dados em arquivo
buscar :: String -> FilePath -> IO ()
buscar val filePath = do
    conteudo <- readFile filePath
    let (linhasEncontradas, _) = partition (\linha -> val `isInfixOf` linha) (lines conteudo)
    if null linhasEncontradas
        then putStrLn "Nenhuma consulta encontrada."
        else mapM_ putStrLn linhasEncontradas

-- Função para ver todos os horários disponíveis em um dia específico
verHorariosDisponiveis :: IO ()
verHorariosDisponiveis = do
    putStr "Informe a data (DD/MM/AAAA): "
    hFlush stdout
    dataConsulta <- getLine
    if validadorData dataConsulta
        then do
            conteudo <- readFile "consultas.txt"
            let consultasNoDia = filter (\linha -> dataConsulta `isInfixOf` linha) (lines conteudo)
            let horariosOcupados = [horario | consulta <- consultasNoDia, 
                                              let campos = wordsWhen (=='|') consulta,
                                              length campos > 3, 
                                              let horario = campos !! 3]
            let horariosDisponiveis = horariosPadrao \\ nub horariosOcupados
            if null horariosDisponiveis
                then putStrLn "Nenhum horário disponível."
                else do
                    putStrLn "Horários disponíveis:"
                    mapM_ putStrLn horariosDisponiveis
        else putStrLn "Data inválida, tente novamente."

-- Função auxiliar para dividir uma string por um delimitador específico
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'
