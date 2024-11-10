module Util (validadorCpf, validadorData, validadorFormatoCRM, escolherMedico) where

import Text.Regex (mkRegex, matchRegex)
import Data.Char (isDigit)

-- Validador formato de CPF
validadorCpf:: String -> Bool
validadorCpf cpf = length cpf == 11 && all isDigit cpf
        
-- Validador formato da data
validadorData:: String -> Bool
validadorData dataConsulta = 
    let regex = mkRegex "^[0-3][0-9]/[0-1][0-9]/[0-9]{4}$" -- (DD/MM/AAAA)
    in case matchRegex regex dataConsulta of
        Just _ -> True
        Nothing -> False

-- Função para validar o formato do CRM
validadorFormatoCRM :: String -> Bool
validadorFormatoCRM crm =
    let regex = mkRegex "^CRM/[A-Z]{2} [0-9]{6}$"  -- Expressão regular para "CRM/XX 123456"
    in case matchRegex regex crm of
        Just _ -> True
        Nothing -> False


-- Extrai o nome e a especialidade do médico
medicoNomeEspecialidade :: [String] -> String
medicoNomeEspecialidade [nome, _, _, especialidade] = nome ++ " - " ++ especialidade
medicoNomeEspecialidade _ = "Dados incompletos"

-- Função para dividir uma linha em colunas usando um delimitador
splitLinha :: Char -> String -> [String]
splitLinha delim linha = case break (== delim) linha of
    (parte, "")     -> [parte]
    (parte, _:resto) -> parte : splitLinha delim resto

-- Função para ler o arquivo e mostrar a lista de médicos enumerada
listarMedicos :: IO ()
listarMedicos = do
    conteudo <- readFile "medicos.txt"
    let linhas = tail (lines conteudo)  -- Remove a primeira linha (cabeçalho)
        medicos = map (splitLinha '|') linhas

    putStrLn "\n\nEscolha um médico pelo número:"
    mapM_ (\(i, medico) -> putStrLn (show i ++ ". " ++ medicoNomeEspecialidade medico)) (zip [1..] medicos)

-- Função para escolher médico pelo número e retornar o CRM
escolherMedico :: IO String
escolherMedico = do
    conteudo <- readFile "medicos.txt"
    let linhas = tail (lines conteudo)
        medicos = map (splitLinha '|') linhas
    listarMedicos
    putStrLn "Digite o número do médico desejado:"
    escolha <- readLn
    if escolha > 0 && escolha <= length medicos
        then return (medicos !! (escolha - 1) !! 3)  -- Retorna o CRM
        else return "Número inválido."




                          