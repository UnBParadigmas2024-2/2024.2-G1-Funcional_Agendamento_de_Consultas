module Cadastro (iniciarCadastro) where

import System.IO (hFlush, stdout, appendFile, writeFile)
import System.Directory (doesFileExist)
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
        "2" -> adicionarDadosMedicos
        _   -> do
            putStrLn "Opção inválida. Tente novamente."
            iniciarCadastro

-- Função para adicionar dados do paciente ao arquivo
adicionarDados :: IO ()
adicionarDados = do
    nome <- coletarNome
    senha <- coletarSenha
    cpf <- coletarCPF
    idade <- coletarIdade
    telefone <- coletarTelefone
    email <- coletarEmail

    let novoDado = nome ++ "|" ++ senha ++ "|" ++ cpf ++ "|" ++ idade ++ "|" ++ telefone ++ "|" ++ email ++ "\n"
    let cabecalho = "Nome|Senha|CPF|Idade|Telefone|Email\n"

    -- Verifica se o arquivo existe, se não existir, cria um novo com o cabeçalho
    arquivoExistente <- doesFileExist "pacientes.txt"
    if not arquivoExistente
        then writeFile "pacientes.txt" (cabecalho ++ novoDado)  -- Cria o arquivo com o cabeçalho e dados do paciente
        else appendFile "pacientes.txt" novoDado  -- Adiciona ao arquivo existente
    
    putStrLn "Paciente cadastrado com sucesso!"
	
-- Função para adicionar dados do médico ao arquivo
adicionarDadosMedicos :: IO ()
adicionarDadosMedicos = do
    nomeMedico <- coletarNome
    senha <- coletarSenha
    crm <- coletarCRM
    especialidade <- coletarEspecialidade

    let novoMedico = nomeMedico ++ "|" ++ senha ++ "|" ++ crm ++ "|" ++ especialidade ++ "\n"
    let cabecalho = "Nome|Senha|CRM|Especialidade\n"

    -- Verifica se o arquivo existe; se não, cria um novo com o cabeçalho
    arquivoExistente <- doesFileExist "medicos.txt"
    if not arquivoExistente
        then writeFile "medicos.txt" (cabecalho ++ novoMedico)  -- Cria o arquivo com cabeçalho e dados do médico
        else appendFile "medicos.txt" novoMedico  -- Adiciona ao arquivo existente
    
    putStrLn "Médico cadastrado com sucesso!"

-- Função para coletar o nome com pelo menos nome e sobrenome
coletarNome :: IO String
coletarNome = do
    putStrLn "Digite o nome completo (nome e sobrenome):"
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
	
-- Função para coletar o CRM no formato "CRM/XX 123456" e verificar duplicidade
coletarCRM :: IO String
coletarCRM = do
    putStrLn "Digite o CRM do médico (formato: CRM/XX 123456, ex.: CRM/SP 123456):"
    crm <- getLine
    crmExiste <- verificarDuplicidadeCRM crm
    if validarFormatoCRM crm && not crmExiste
        then return crm
        else do
            if crmExiste
                then putStrLn "CRM já cadastrado. Insira um CRM diferente."
                else putStrLn "CRM inválido. Certifique-se de que segue o formato correto (CRM/XX 123456)."
            coletarCRM
		
-- Função para validar o formato do CRM
validarFormatoCRM :: String -> Bool
validarFormatoCRM crm =
    let regex = mkRegex "^CRM/[A-Z]{2} [0-9]{6}$"  -- Expressão regular para "CRM/XX 123456"
    in case matchRegex regex crm of
        Just _ -> True
        Nothing -> False
		
-- Função para verificar duplicidade de CRM
verificarDuplicidadeCRM :: String -> IO Bool
verificarDuplicidadeCRM crm = do
    arquivoExistente <- doesFileExist "medicos.txt"
    if not arquivoExistente
        then return False
        else do
            conteudo <- readFile "medicos.txt"
            let linhas = lines conteudo
            return $ any (\linha -> crm `isInfixOf` linha) linhas

-- Função para coletar a especialidade do médico
coletarEspecialidade :: IO String
coletarEspecialidade = do
    putStrLn "Digite a especialidade do médico:"
    especialidade <- getLine
    if not (null especialidade)
        then return especialidade
        else do
            putStrLn "Especialidade inválida. Tente novamente."
            coletarEspecialidade

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
    arquivoExistente <- doesFileExist "pacientes.txt"
    if not arquivoExistente
        then return False  -- Se o arquivo não existir, o CPF não pode estar cadastrado
        else do
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
    putStrLn "Digite a senha para o usuário:"
    senha <- getLine
    if length senha >= 6
        then return senha
        else do
            putStrLn "A senha deve ter pelo menos 6 caracteres. Tente novamente."
            coletarSenha
