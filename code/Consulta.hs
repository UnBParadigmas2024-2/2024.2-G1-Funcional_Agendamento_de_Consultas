module Consulta (submenuConsulta) where

import System.IO (hFlush, stdout)

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
        "2" -> putStrLn "Buscar consulta"  -- Você pode implementar a função correspondente aqui.
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            submenuConsulta  -- Chama novamente o submenu para o usuário tentar novamente.

cadastroConsulta :: IO ()
cadastroConsulta = do
    putStrLn "Cadastrar consulta"
    -- Aqui você pode adicionar a lógica para cadastrar a consulta.
