module Util (validadorCpf, validadorData) where

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
