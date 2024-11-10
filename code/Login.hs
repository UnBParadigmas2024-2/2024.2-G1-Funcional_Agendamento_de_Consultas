module Login (login) where

import System.IO (hFlush, stdout)
import Data.List (isPrefixOf, find)
import Paciente (submenuPaciente)
import Medico (submenuMedico)

-- Função de login
login :: IO ()
login = do
    putStrLn "\nDigite seu identificador (CPF ou CRM):"
    putStr "Identificador: "
    hFlush stdout
    identificador <- getLine
    if "CRM/" `isPrefixOf` identificador
        then realizarLoginMedico identificador
        else realizarLoginPaciente identificador

-- Função para login de paciente
realizarLoginPaciente :: String -> IO ()
realizarLoginPaciente cpf = do
    putStrLn "Digite sua senha:"
    senha <- getLine
    resultadoAutenticacao <- autenticarPaciente cpf senha
    case resultadoAutenticacao of
        Just nome -> do
            putStrLn $ "Bem-vindo, " ++ nome
            putStrLn "Login realizado com sucesso!"
            submenuPaciente cpf  -- Redireciona ao menu do paciente após login
        Nothing -> putStrLn "CPF ou senha incorretos. Tente novamente."

-- Função para login de médico
realizarLoginMedico :: String -> IO ()
realizarLoginMedico crm = do
    putStrLn "Digite sua senha:"
    senha <- getLine
    resultadoAutenticacao <- autenticarMedico crm senha
    case resultadoAutenticacao of
        Just nome -> do
            putStrLn $ "Bem-vindo, Dr(a). " ++ nome
            putStrLn "Login realizado com sucesso!"
            submenuMedico crm  -- Redireciona ao menu do médico após login
        Nothing -> putStrLn "CRM ou senha incorretos. Tente novamente."

-- Função para autenticar CPF e senha de paciente, retornando o nome se autenticado
autenticarPaciente :: String -> String -> IO (Maybe String)
autenticarPaciente cpf senha = do
    conteudo <- readFile "pacientes.txt"
    let linhas = lines conteudo
    -- Validação baseada na estrutura Nome|Senha|CPF|Idade|Telefone|Email
    let pacienteEncontrado = find (\linha -> let campos = wordsWhen (=='|') linha
                                             in length campos >= 3 && campos !! 1 == senha && campos !! 2 == cpf) linhas
    -- Retorna o nome do paciente se encontrado
    return $ fmap (\linha -> let campos = wordsWhen (=='|') linha in campos !! 0) pacienteEncontrado

-- Função para autenticar CRM e senha de médico, retornando o nome se autenticado
autenticarMedico :: String -> String -> IO (Maybe String)
autenticarMedico crm senha = do
    conteudo <- readFile "medicos.txt"
    let linhas = lines conteudo
    -- Validação baseada na estrutura Nome|Senha|CRM|Especialidade
    let medicoEncontrado = find (\linha -> let campos = wordsWhen (=='|') linha
                                           in length campos >= 3 && campos !! 1 == senha && campos !! 2 == crm) linhas
    -- Retorna o nome do médico se encontrado
    return $ fmap (\linha -> let campos = wordsWhen (=='|') linha in campos !! 0) medicoEncontrado

-- Função auxiliar para dividir uma string pelo caractere |
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s = case dropWhile p s of
                    "" -> []
                    s' -> w : wordsWhen p s''
                          where (w, s'') = break p s'
