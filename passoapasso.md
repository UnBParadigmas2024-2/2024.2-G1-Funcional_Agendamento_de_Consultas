# Passo a Passo de como instalar e utilizar o projeto "Agendamento de Consultas"
* Obs: aconselha-se usar o sistema operacional LINUX para maior praticidade de uso da linguagem.
- Passo 1. Instale o Haskell em sua máquina, com os seguintes passos:

`` sudo apt update ``

``  sudo apt install haskell-platform `` 

`` curl -sSL https://get.haskellstack.org/ | sh ``

- Passo 2. Verifique se ele foi instalado corretamente:

`` ghc --version ``

`` stack --version `` 

- Passo 3. Clone o projeto em sua máquina:

`` git clone https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas.git ``

- Passo 4. Entre na pasta do projeto:

`` cd 2024.2-G1-Funcional_Agendamento_de_Consultas `` 

- Passo 5. Execute o compilador do projeto:

`` ghc -o SistemaGestao Cadastro.hs Main.hs ``

- Passo 6. Rode o projeto:

`` ./SistemaGestao ``  
