# ParaDigaMais

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01 <br>
**Nro do Grupo**: 01<br>
**Paradigma**: Funcional<br>

## Alunos
|Matrícula | Aluno |
| -- | -- |
| 20/0056981  |  Arthur Ferreira Rodrigues |
| 19/0084600  |  Arthur José Nascimento de Lima |
| 19/0134224  |  Artur Seppa Reiman |
| 19/0084731  |  Augusto Duraes Camargo |
| 20/2015948  |  Breno Henrique de Souza |
| 22/2014984  |  Edilberto Almeida Cantuaria |
| 18/0121308  |  Giulia Domingues de Alcantara |
| 17/0034941  |  Guilherme Peixoto Lima |
| 18/0042041  |  Gustavo Barbosa de Oliveira |
| 20/0069322  |  Samuel Alves Sato |

## O que é um paradigma funcional

O paradigma funcional é um estilo de programação que enfatiza a aplicação de funções sem efeitos colaterais, o que diferencia de abordagens imperativas, que focam na alteração do estado do programa. Baseado no Cálculo Lambda de Alonzo Church, esse paradigma ganhou força com linguagens como Lisp e, mais recentemente, com Haskell, uma linguagem puramente funcional que será utilizada neste curso. Haskell oferece recursos avançados, como tipos paramétricos e avaliação preguiçosa, permitindo a criação de soluções elegantes e precisas. Embora o paradigma funcione bem em prototipação e aprendizado, é menos eficiente em ambientes comerciais devido à sua diferença estrutural em relação às máquinas baseadas na arquitetura de Turing, que favorece programas imperativos. (COSTA, 2020) [1]

## Sobre o Projeto
Este projeto visa desenvolver um sistema de gerenciamento de pacientes utilizando a linguagem funcional Haskell. O sistema permite o cadastro e gerenciamento de usuários (pacientes e médicos) de forma modular, dividindo funcionalidades em arquivos separados para melhor organização. Entre os principais recursos, o sistema oferece um menu principal que direciona para submenus específicos dos usuários, como o submenu de pacientes. Esse submenu fornece funcionalidades como visualização e edição de dados pessoais, exclusão de contas, agendamento e consulta de consultas médicas. Há também validações para entradas importantes, como CPF, telefone e e-mail, garantindo a integridade dos dados.

Além disso, o sistema implementa uma interface de login com autenticação baseada em CPF e senha, e validações para prevenir o uso de CPFs duplicados e garantir a estrutura de dados correta (como mínimo de dígitos para CPF, telefone e idade).

### Relação do Paradigma Funcional com o Projeto
O paradigma funcional, base do Haskell utilizado neste projeto, é ideal para o desenvolvimento de um sistema de gerenciamento de pacientes, pois enfatiza funções puras e modularidade. Essas características garantem que operações como cadastro, login e agendamento sejam independentes e seguras, protegendo a integridade de dados sensíveis como CPF e e-mail.

A modularidade permite que cada funcionalidade seja isolada em módulos distintos, facilitando a manutenção e expansão do código. Além disso, a avaliação preguiçosa de Haskell otimiza o processamento, garantindo eficiência no uso de recursos. A natureza declarativa do paradigma torna o código mais legível e fácil de depurar, contribuindo para um sistema robusto, focado na segurança e na integridade dos dados.

## Por que optamos por um sistema de gerenciamento de pacientes?



## Screenshots
### Fluxograma do Projeto
![fluxos (1)](https://github.com/user-attachments/assets/4363c36c-7dbf-41db-bc0a-f9a72384e6cf)

- OBS: Para maior detalhamento de cada funcionalidade, acesse [esse link](documentacao/uso-sistema.md)
## Manual
### Instalação 
**Linguagens**: Haskell<br>
**Tecnologias**: Sistema Operacional Linux, Compilador GHC (Glasgow Haskell Compiler), Editor de Texto Nano, Terminal Linux<br>
- Para instalação do projeto, acesse [esse link](documentacao/passoapassoinstalacao.md) e leia o passo a passo.

### Uso 
### Manual
**Principais Comandos**: ``ghc -o SistemaGestao Main.h`` ;  ``./SistemaGestao`` 

#### Como usar o projeto
1. Para conseguir rodar o projeto, acesse [esse link](documentacao/passoapasso-uso.md)
2. Para aprender como utilizar o sistema em si acesse [esse link](documentacao/uso-sistema.md)


## Vídeo
Adicione 1 ou mais vídeos com a execução do projeto.
Procure: 
(i) Introduzir o projeto;
(ii) Mostrar passo a passo o código, explicando-o, e deixando claro o que é de terceiros, e o que é contribuição real da equipe;
(iii) Apresentar particularidades do Paradigma, da Linguagem, e das Tecnologias, e
(iV) Apresentar lições aprendidas, contribuições, pendências, e ideias para trabalhos futuros.
OBS: TODOS DEVEM PARTICIPAR, CONFERINDO PONTOS DE VISTA.
TEMPO: +/- 15min

## Participações
Apresente, brevemente, como cada membro do grupo contribuiu para o projeto.

|Nome do Membro | Contribuição | Significância da Contribuição para o Projeto (Excelente/Boa/Regular/Ruim/Nula) | Comprobatórios |
| -- | -- | -- | -- |
| Breno | 1. Criação dos cadastros de paciente; Criação do Login; Criação das telas de menu de paciente e médico; Criação da função de apagar médico e paciente | Excelente | ADICIONAR LINKS DE COMMITS |
| Edilberto | 1. Ajustes gerais (no código); 2. Melhoria na visualização de consultas do paciente  | Excelente | [Visualização agenda do paciente](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/6a102934fbc91ce06f9703e957c2d457da2247f4); [Ajustes gerais 1](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/afdbc8e7cd240f8983ca62469aedea51f7dde6b2); [Ajustes gerais 2](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/1ed4743a2bd93a37c1c5a845ec29f8aeacee9b62); [Ajustes na documentação](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/0630ca3c72c1b4ffe42c0f4baf15164444046352); [Refatoração na visualização da agenda do paciente](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/638d497bbc772e7f939764caef92af957c2416f0)|
| Giulia | 1. Criação da tela inicial (main); Criação dos cadastros de paciente; Criação das telas de menu de paciente e médico; Desenvolvimento da documentação; Criação da função de apagar médico e paciente | Excelente | [Main](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/ccb2483fc5a05e914ab581f82c3e6b89b1fab28e); [Documentação do paradigma e do projeto](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/20722a7839e22fb127903b82cd2316805be253e4); [Documentação sobre detalhes](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/6e1b6406f0a966ef70d97320733859e42862f6d4) [Documentação da instalação](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/a1ba4ebcd471ba3c05737b1775aa9f97757e6e1b); [Documentação de como rodar o projeto](https://github.com/UnBParadigmas2024-2/2024.2-G1-Funcional_Agendamento_de_Consultas/commit/8058fdf914c4e242cdf64d3ec7dbbb404fb2841c)|



## Outros 
### Lições Aprendidas
- A prioridade deve ser montar um fluxograma fechado, para não haver problemas de comunicação ao longo do desenvolvimento do sistema;
- Melhorar a gestão de tempo é essencial para as coisas não serem feitas com pressa de última hora;
- É necessário deixar todos os requisitos bem claros e documentados, para evitar mudanças no escopo do projeto.
  
### Percepções
- A linguagem haskell é complicada para compreensão e leitura;
- Compilar um arquivo em haskell é trabalhoso;
- A sintaxe da linguagem é complexa;
- O paradigma funcional é bem modularizado, o que auxilia no desenvolvimento do código.

### Contribuições e Fragilidades
- Contribuições: implementado um sistema de agendamento de consultas completo e funcional, com CRUD de paciente, médicos e consultas;
- Fragilidades: Dificuldade em se adaptar ao novo paradigma de programação e ao novo grupo de trabalho.

### Trabalhos Futuros
A análise do projeto em questão revela a necessidade de aprimoramentos tanto em termos de usabilidade quanto de arquitetura.

- **Usabilidade:** Atualmente, o processo de atualização de dados exige que o usuário percorra todos os campos do registro, mesmo quando apenas um necessita de modificação. Essa característica pode gerar frustração e aumentar o tempo de execução da tarefa. A implementação de uma funcionalidade que permita a edição individual de campos proporcionaria uma experiência de usuário mais intuitiva e eficiente.
- **Arquitetura:** A presença de uma dependência circular na arquitetura do sistema resultou em um *bug*, evidenciando a importância de um planejamento mais cuidadoso e de uma documentação completa dos requisitos. A revisão da arquitetura, com o objetivo de eliminar essa dependência, é fundamental para garantir a robustez e a manutenibilidade do sistema a longo prazo.

Como sugestão para trabalhos futuros, além do presente projeto, propõem-se as seguintes implementações em Haskell:
- Jogo da Cobrinha: Desenvolvimento de um jogo clássico, explorando conceitos de programação funcional e estruturas de dados.
- Sistema de Recomendação de Filmes: Implementação de um sistema capaz de sugerir filmes aos usuários com base em seus históricos e preferências, utilizando técnicas de aprendizado de máquina.
- Labirinto: Criação de um simulador de labirintos, permitindo a geração e resolução de diferentes tipos de labirintos, com foco em algoritmos de busca.

## Fontes
[1] Costa, S. (2020, 17 de abril). *Paradigma funcional*. Medium. Disponível em: https://sergiocosta.medium.com/paradigma-funcional-3194924a8d20
