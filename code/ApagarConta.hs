module ApagarConta (apagarConta) where

import System.IO (readFile, writeFile)
import Data.List (isInfixOf)

apagarConta :: String -> IO ()
apagarConta cpf = do
    conteudo <- readFile "pacientes.txt"
    let dados = filter (\linha -> not (cpf `isInfixOf` linha)) (lines conteudo)
    writeFile "pacientes.txt" (unlines dados)
    putStrLn "Conta apagada com sucesso!"
