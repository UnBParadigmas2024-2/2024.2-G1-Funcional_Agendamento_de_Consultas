# Motivos de escolha do projeto

## Introdução

O projeto de agendamento de consultas foi desenvolvido para facilitar a interação entre pacientes e médicos, permitindo o cadastro de ambos e possibilitando o agendamento de consultas de forma prática e eficiente. A plataforma oferece funcionalidades como:
- Cadastro de pacientes e médicos.
- Agendamento de consultas com datas e horários específicos.
- Visualização das consultas agendadas.
- Possibilidade de alteração e gerenciamento de contas dos usuários.

O objetivo é trazer uma solução simples e robusta para o gerenciamento de consultas médicas.

## Motivo da Escolha do Projeto
Dentro das opções de projetos, o agendamento de consultas foi selecionado por diversos motivos:
- Relevância Prática: O projeto busca resolver um problema real e presente no cotidiano, especialmente em ambientes de clínicas e consultórios médicos, onde a gestão de horários e cadastro de pacientes é essencial.

- Desafios Técnicos: O projeto apresenta desafios interessantes do ponto de vista da modelagem de dados e do controle de estados, além da necessidade de validações rigorosas, como verificação de CPF, CRM, e horários disponíveis, o que exige uma implementação bem estruturada.

- Adequação ao Paradigma Funcional: Este projeto visa aplicar conceitos do paradigma funcional de maneira significativa, com a linguagem Haskell, que facilita a implementação de lógica de negócios complexa e verificações de dados, trazendo benefícios claros para o desenvolvimento de sistemas seguros e eficientes.

## Paradigma Funcional e Haskell
O paradigma funcional e a linguagem Haskell foram escolhidos para este projeto por multiplas razões:

- Imutabilidade e Segurança: Em Haskell, dados imutáveis reduzem a possibilidade de estados inconsistentes. No contexto de agendamentos, isso é particularmente valioso, pois ajuda a garantir que uma consulta agendada não será alterada indevidamente.

- Pureza e Funcionalidade Sem Efeitos Colaterais: Funções puras, que são uma característica central de Haskell, garantem que as operações não têm efeitos colaterais indesejados. No sistema de agendamento, isso contribui para que cada função execute exatamente o que foi programada para fazer, sem interferir em outras partes do sistema.

- Funções de Alta Ordem: Haskell permite a criação de funções de alta ordem, facilitando a implementação de filtros e transformações complexas. Para o agendamento, isso possibilita filtrar consultas por médico, horário, paciente, entre outros.

- Abordagem Declarativa: Em Haskell, o código tende a ser mais declarativo, focando no que deve ser feito ao invés de como fazer. Essa característica torna o código do agendamento mais legível, fácil de entender e de manter.