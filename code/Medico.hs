module Medico (submenuMedico) where

import ApagarConta (apagarConta)
import System.IO (hFlush, stdout)
import ExibicaoDados (exibirDadosMedicos, atualizarDadosMedico)
import Consulta (buscarConsulta)

submenuMedico :: String -> IO ()
submenuMedico crm = do
    putStrLn "\nMenu do Médico"
    putStrLn "1. Visualizar meus dados"
    putStrLn "2. Apagar minha conta"
    putStrLn "3. Visualizar minha agenda (consultas agendadas e horários disponíveis)"
    putStrLn "4. Desmarcar consultas"
    putStrLn "5. Sair do sistema"
    putStr "Escolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do
            exibirDadosMedicos crm
            submenuMedico crm  -- Retorna ao submenu após exibir os dados
        "2" -> do
            apagarConta crm  -- Chama a função para apagar a conta
            putStrLn "Conta apagada com sucesso!"
        "3" -> putStrLn "Funcionalidade de visualização de agenda ainda não implementada."
        "4" -> putStrLn "Funcionalidade de desmarcação de consultas ainda não implementada."
        "5" -> putStrLn "Saindo do sistema..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuMedico crm