module Util (validadorCpf, validadorData, validadorFormatoCRM, escolherMedico, horariosDisponiveis, buscarMedico, buscarPaciente) where

import Text.Regex (mkRegex, matchRegex)
import Data.Char (isDigit)
import Data.List (find)

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

    putStrLn "Escolha um médico pelo número:"
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
        then return (medicos !! (escolha - 1) !! 2)  -- Retorna o CRM
        else return "Número inválido."





-- Função principal para encontrar horários disponíveis
horariosDisponiveis :: String -> String -> IO [String]
horariosDisponiveis dataDesejada crm = do
    conteudo <- readFile "consultas.txt"
    let linhas = tail (lines conteudo)  -- Remove o cabeçalho
        consultas = map (splitLinha '|') linhas
        horariosOcupados = [horario | [_, _, crmConsulta, _, _, dataConsulta, horario, _, _] <- consultas,
                                       crmConsulta == crm, dataConsulta == dataDesejada]
        horariosDisponiveisDia = gerarHorariosDisponiveis horariosOcupados
    return horariosDisponiveisDia

-- Gera horários disponíveis excluindo horários ocupados e o intervalo de almoço
gerarHorariosDisponiveis :: [String] -> [String]
gerarHorariosDisponiveis horariosOcupados =
    let todosHorarios = [formatarHorario h m | h <- [8..17], m <- [0, 30], not (h == 12 || h == 13)]
    in filter (`notElem` horariosOcupados) todosHorarios

-- Formata um horário em "HH:MM"
formatarHorario :: Int -> Int -> String
formatarHorario h m = (if h < 10 then "0" else "") ++ show h ++ ":" ++ (if m == 0 then "00" else show m)


-- Função para buscar o médico pelo CRM no arquivo medicos.txt
buscarMedico :: String -> IO (Maybe [String])
buscarMedico crmProcurado = do
    conteudo <- readFile "medicos.txt"
    let linhas = tail (lines conteudo)  -- Remove o cabeçalho
        medicos = map (splitLinha '|') linhas
    return $ find (\[_, _, crm, _] -> crm == crmProcurado) medicos

-- Função para buscar o paciente pelo CPF no arquivo pacientes.txt
buscarPaciente :: String -> IO (Maybe [String])
buscarPaciente cpfProcurado = do
    conteudo <- readFile "pacientes.txt"
    let linhas = tail (lines conteudo)  -- Remove o cabeçalho
        pacientes = map (splitLinha '|') linhas
    return $ find (\[_, _, cpf, _, _, _] -> cpf == cpfProcurado) pacientes

