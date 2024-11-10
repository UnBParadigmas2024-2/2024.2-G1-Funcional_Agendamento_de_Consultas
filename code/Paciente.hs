module Paciente (submenuPaciente) where

import Agenda (buscaConsultas)
import ApagarConta (apagarConta)
import System.IO (hFlush, stdout)
import ExibicaoDados (exibirDadosPacientes, atualizarDadosPaciente)
import Consulta (cadastroConsulta, buscarConsulta)

submenuPaciente :: String -> IO ()
submenuPaciente cpf = do
    putStrLn "\nMenu do Paciente"
    putStrLn "1. Visualizar meus dados"
    putStrLn "2. Apagar minha conta"
    putStrLn "3. Agendar consultas"
    putStrLn "4. Visualizar minhas consultas agendadas"
    putStrLn "5. Sair do sistema"
    putStr "Escolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do
            exibirDadosPacientes cpf
            submenuPaciente cpf
        "2" -> do
            apagarConta cpf  -- Chama a função para apagar a conta
            putStrLn "Conta apagada com sucesso!"
        "3" -> do 
            cadastroConsulta cpf
            submenuPaciente cpf
        "4" -> do
            buscaConsultas cpf
            submenuPaciente cpf
        "5" -> putStrLn "Saindo do sistema..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuPaciente cpf