
cadastroConsulta :: IO
cadastroConsulta = do
    
    putStrLn "Informe o CPF do paciente:"
    cpfPaciente <- getLine

    putStrLn "Informe o CRM do médico:"
    crm <- getLine

    putStrLn "Informe a Data:"
    dataConsuta <- getLine

    putStrLn "Informe a Hora:"
    horaConsuta <- getLine

    putStrLn "Informe o status da consulta (agendada, concluída, cancelada):"
    statusConsuta <- getLine

    
    --hFlush stdout
    -- escolha <- getLine
    -- case escolha of
    --     "1" -> adicionarDados  -- Vai para o cadastro de paciente
    --     "2" -> adicionarDadosMedicos
    --     _   -> do
    --         putStrLn "Opção inválida. Tente novamente."
    --         iniciarCadastro