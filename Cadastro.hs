module Cadastro (iniciarCadastro) where

import System.IO (hFlush, stdout)
import Text.Regex (mkRegex, matchRegex)
import Data.List (isInfixOf)

-- Função para validar e-mail
validarEmail :: String -> Bool
validarEmail email =
    let regex = mkRegex "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.com$"
    in case matchRegex regex email of
        Just _ -> True
        Nothing -> False

-- Função para iniciar o cadastro e perguntar se é paciente ou médico
iniciarCadastro :: IO ()
iniciarCadastro = do
    putStrLn "Escolha uma opção:"
    putStrLn "1. Cadastrar Paciente"
    putStrLn "2. Cadastrar Médico"
    putStr "Digite sua escolha: "
    hFlush stdout
    escolha <- getLine
    case escolha of
        "1" -> adicionarDados  -- Vai para o cadastro de paciente
        "2" -> putStrLn "Voltando ao Menu Principal..."
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            iniciarCadastro

-- Função para adicionar dados do paciente ao arquivo
adicionarDados :: IO ()
adicionarDados = do
    nome <- coletarNome
    cpf <- coletarCPF
    idade <- coletarIdade
    telefone <- coletarTelefone
    email <- coletarEmail
    senha <- coletarSenha

    let novoDado = "Nome: " ++ nome ++ ", CPF: " ++ cpf ++ ", Idade: " ++ idade ++ ", Telefone: " ++ telefone ++ ", E-mail: " ++ email ++ ", Senha: " ++ senha ++ "\n"
    
    appendFile "pacientes.txt" novoDado
    putStrLn "Paciente cadastrado com sucesso!"

-- Função para coletar o nome com pelo menos nome e sobrenome
coletarNome :: IO String
coletarNome = do
    putStrLn "Digite o nome completo do paciente (nome e sobrenome):"
    nome <- getLine
    if validarNomeCompleto nome
        then return nome
        else do
            putStrLn "Nome inválido. Certifique-se de que incluiu pelo menos um nome e um sobrenome."
            coletarNome

-- Função para validar o nome completo
validarNomeCompleto :: String -> Bool
validarNomeCompleto nome =
    let palavras = words nome
    in length palavras >= 2 && all (\p -> length p > 0) palavras

-- Função para coletar o CPF com validação de 11 dígitos e verificar duplicidade
coletarCPF :: IO String
coletarCPF = do
    putStrLn "Digite o CPF do paciente (11 dígitos):"
    cpf <- getLine
    cpfExiste <- verificarDuplicidadeCPF cpf
    if length cpf == 11 && all (`elem` ['0'..'9']) cpf && not cpfExiste
        then return cpf
        else do
            if cpfExiste
                then putStrLn "CPF já cadastrado. Insira um CPF diferente."
                else putStrLn "CPF inválido. Certifique-se de que tem exatamente 11 dígitos numéricos."
            coletarCPF

-- Função para verificar duplicidade de CPF
verificarDuplicidadeCPF :: String -> IO Bool
verificarDuplicidadeCPF cpf = do
    conteudo <- readFile "pacientes.txt"
    let linhas = lines conteudo
    return $ any (\linha -> cpf `isInfixOf` linha) linhas

-- Função para coletar a idade com validação de dois dígitos
coletarIdade :: IO String
coletarIdade = do
    putStrLn "Digite a idade do paciente (mínimo de 10 anos):"
    idade <- getLine
    if validarIdade idade
        then return idade
        else do
            putStrLn "Idade inválida. Insira uma idade com dois dígitos (mínimo de 10 anos)."
            coletarIdade

-- Função para validar idade
validarIdade :: String -> Bool
validarIdade idade = all (`elem` ['0'..'9']) idade && length idade == 2

-- Função para coletar o telefone com validação de 11 dígitos
coletarTelefone :: IO String
coletarTelefone = do
    putStrLn "Digite o telefone do paciente (11 dígitos):"
    telefone <- getLine
    if length telefone == 11 && all (`elem` ['0'..'9']) telefone
        then return telefone
        else do
            putStrLn "Telefone inválido. Certifique-se de que tem exatamente 11 dígitos numéricos."
            coletarTelefone

-- Função para coletar o e-mail com validação
coletarEmail :: IO String
coletarEmail = do
    putStrLn "Digite o e-mail do paciente:"
    email <- getLine
    if validarEmail email
        then return email
        else do
            putStrLn "E-mail inválido. Certifique-se de que o e-mail está no formato usuario@dominio.com."
            coletarEmail

-- Função para coletar a senha do paciente
coletarSenha :: IO String
coletarSenha = do
    putStrLn "Digite a senha do paciente:"
    senha <- getLine
    if length senha >= 6
        then return senha
        else do
            putStrLn "A senha deve ter pelo menos 6 caracteres. Tente novamente."
            coletarSenha
