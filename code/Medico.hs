module Medico (submenuMedico) where

import ApagarConta (apagarConta)
import System.IO (hFlush, stdout)
import ExibicaoDados (exibirDadosMedicos, atualizarDadosMedico)
import Consulta (buscarConsultaMedico, desmaracarConsulta)

submenuMedico :: String -> IO ()
submenuMedico crm = do
    putStrLn "\nBem vindo (a)! O que deseja?"
    putStrLn "1. Visualizar meus dados"
    --putStrLn "2. Apagar minha conta"
    putStrLn "2. Visualizar minha agenda (consultas agendadas e horários disponíveis)"
    putStrLn "3. Sair do sistema"
    putStr "Escolha uma opção: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> do
            exibirDadosMedicos crm
            submenuMedico crm  -- Retorna ao submenu após exibir os dados
      --  "2" -> do
            -- apagarConta crm "medicos" -- Chama a função para apagar a conta
          --  putStrLn "Não implementado!"
          --  submenuMedico crm

        "2" -> do
            buscarConsultaMedico crm
            submenuMedico crm
        "3" -> putStrLn "Saindo do sistema..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuMedico crm
