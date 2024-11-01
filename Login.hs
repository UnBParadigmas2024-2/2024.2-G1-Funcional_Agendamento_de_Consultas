module Login (login) where

import System.IO (hFlush, stdout)
import Data.List (isInfixOf)
import Paciente (submenuPaciente)

-- Função de login
login :: IO ()
login = do
    putStrLn "\nDigite seu CPF:"
    cpf <- getLine
    putStrLn "Digite sua senha:"
    senha <- getLine
    autenticado <- autenticar cpf senha
    if autenticado
        then do
            putStrLn "Login realizado com sucesso!"
            submenuPaciente cpf  -- Redireciona ao menu do paciente após login
        else putStrLn "CPF ou senha incorretos. Tente novamente."

-- Função para autenticar CPF e senha
autenticar :: String -> String -> IO Bool
autenticar cpf senha = do
    conteudo <- readFile "pacientes.txt"
    let linhas = lines conteudo
    -- Validação baseada na estrutura `Nome|Senha|CPF|Idade|Telefone|Email`
    let usuarioValido = any (\linha -> let campos = wordsWhen (=='|') linha
                                       in length campos >= 3 && campos !! 1 == senha && campos !! 2 == cpf) linhas
    return usuarioValido

-- Função auxiliar para dividir uma string pelo caractere `|`
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'
