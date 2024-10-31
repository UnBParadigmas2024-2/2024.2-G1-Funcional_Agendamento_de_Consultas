module Login (login) where

import System.IO (hFlush, stdout)
import Data.List (isInfixOf)

-- Função de login
login :: IO ()
login = do
    putStrLn "\nDigite seu CPF:"
    cpf <- getLine
    putStrLn "Digite sua senha:"
    senha <- getLine
    autenticado <- autenticar cpf senha
    if autenticado
        then putStrLn "Login realizado com sucesso!"
        else putStrLn "CPF ou senha incorretos. Tente novamente."

-- Função para autenticar CPF e senha
autenticar :: String -> String -> IO Bool
autenticar cpf senha = do
    conteudo <- readFile "pacientes.txt"
    let linhas = lines conteudo
    let usuarioValido = any (\linha -> cpf `isInfixOf` linha && senha `isInfixOf` linha) linhas
    return usuarioValido
