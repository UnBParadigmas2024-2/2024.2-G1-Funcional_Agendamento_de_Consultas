module Util (validadorCpf, validadorData, validadorFormatoCRM) where

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