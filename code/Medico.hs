module Medico (submenuMedico) where

import ApagarConta (apagarConta)
import System.IO (hFlush, stdout)
import ExibicaoDados (exibirDadosMedicos, atualizarDadosMedico)
import Consulta (buscarConsulta)

submenuMedico :: String -> IO ()
submenuMedico crm = do
    putStrLn "\nMenu do Médico"
    putStrLn "1. Visualizar meus dados"
    putStrLn "2. Atualizar meus dados"
    putStrLn "3. Apagar minha conta"
    putStrLn "4. Visualizar minha agenda (consultas agendadas e horários disponíveis)"
    putStrLn "5. Desmarcar consultas"
    putStrLn "6. Sair do sistema"
    putStr "Escolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do
            exibirDadosMedicos crm
            submenuMedico crm  -- Retorna ao submenu após exibir os dados
        "2" -> do
            atualizarDadosMedico crm
            submenuMedico crm  -- Retorna ao submenu após atualizar os dados
        "3" -> do
            apagarConta crm  -- Chama a função para apagar a conta
            putStrLn "Conta apagada com sucesso!"
        "4" -> putStrLn "Funcionalidade de visualização de agenda ainda não implementada."
        "5" -> putStrLn "Funcionalidade de desmarcação de consultas ainda não implementada."
        "6" -> putStrLn "Saindo do sistema..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuMedico crm