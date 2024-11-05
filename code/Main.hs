module Main where

import System.IO (hFlush, stdout)
import Cadastro (iniciarCadastro)
import Login (login)
import Consulta(cadastroConsulta)

main :: IO ()
main = do
    putStrLn "Menu Principal:"
    putStrLn "1. Cadastrar Paciente ou Médico"
    putStrLn "2. Login"
    putStrLn "0. Sair"
    putStr "Digite sua escolha: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> iniciarCadastro >> main  -- Chama o submenu de cadastro
        "2" -> login >> main  -- Chama a função de login
        "3" -> cadastroConsulta >> cadastroConsulta  -- Chama a função de login
        "0" -> putStrLn "Encerrando o sistema..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            main

