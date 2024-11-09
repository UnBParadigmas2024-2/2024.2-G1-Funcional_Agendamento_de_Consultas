module Paciente (submenuPaciente) where

import Agenda (buscaConsultas)
import ApagarConta (apagarConta)
import System.IO (hFlush, stdout)
import ExibicaoDados (exibirDadosPacientes, atualizarDadosPaciente)

submenuPaciente :: String -> IO ()
submenuPaciente cpf = do
    putStrLn "\nMenu do Paciente"
    putStrLn "1. Visualizar meus dados"
    putStrLn "2. Atualizar meus dados"
    putStrLn "3. Apagar minha conta"
    putStrLn "4. Agendar consultas"
    putStrLn "5. Visualizar minhas consultas agendadas"
    putStrLn "6. Sair do sistema"
    putStr "Escolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do
            exibirDadosPacientes cpf
            submenuPaciente cpf
        "2" -> do
            atualizarDadosPaciente cpf
            submenuPaciente cpf  -- Retorna ao submenu após atualizar os dados
        "3" -> do
            apagarConta cpf  -- Chama a função para apagar a conta
            putStrLn "Conta apagada com sucesso!"
        "4" -> putStrLn "Funcionalidade de agendamento ainda não implementada."
        "5" -> buscaConsultas cpf
        "6" -> putStrLn "Saindo do sistema..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuPaciente cpf