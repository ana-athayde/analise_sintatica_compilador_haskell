# Analisador Sintático para Compilador em Haskell

Este repositório contém a implementação da análise sintática para um compilador desenvolvido em Haskell. O projeto faz parte da disciplina de Compiladores do Departamento de Ciência da Computação da UDESC, Joinville. Esta é a primeira fase do projeto, com as próximas etapas incluindo a análise semântica (verificação de tipos) e a geração de código a partir da representação intermediária.

## Estrutura do Projeto

O compilador segue um conjunto de produções de gramática que definem a estrutura da linguagem a ser compilada. A estrutura principal inclui:

- Programa
- Funções
- Tipos de retorno
- Declaração de parâmetros
- Bloco principal
- Comandos

## Expressões e Operadores

A linguagem permite expressões relacionais e lógicas com operadores como <, >, <=, >=, == e /=. Além disso, os operadores lógicos &&, || e ! estão disponíveis. Operadores aritméticos (+, -, *, /) seguem a precedência usual. As expressões podem conter identificadores de variáveis, constantes inteiras, constantes com ponto flutuante e chamadas de funções. O uso de parênteses é permitido para alterar a ordem de avaliação.

## Tokens

A análise léxica define tokens, como identificadores (id), constantes inteiras, constantes com ponto flutuante e constantes de cadeia de caracteres (literais).

## Estrutura de Arquivos

O projeto está organizado em vários arquivos, cada um com uma função específica:

- `Comandos.hs`: Contém definições e funções relacionadas à análise de comandos na linguagem.
- `DataTypes.hs`: Define os tipos de dados utilizados no compilador, como `Tipo`, `TCons`, `Expr`, `ExprR`, `ExprL`, `Var`, `Funcao`, `Programa`, `Bloco` e `Comando`.
- `Lex.hs`: Lida com a análise léxica e define os tokens da linguagem, como operadores e palavras-chave.
- `Logico.hs`: Contém funções relacionadas à análise de expressões lógicas e operadores lógicos.
- `Main.hs`: O arquivo principal que chama o analisador do compilador para processar o código-fonte.
- `Makefile`: Um arquivo de script para compilar e executar o código.
- `Parametro.hs`: Lida com a análise de parâmetros e declarações na linguagem.
- `Programa.hs`: Contém definições e funções relacionadas à análise de funções e programas na linguagem.
- `Relacional.hs`: Define operadores relacionais e funções relacionadas à análise desses operadores.
- `Tabelas.hs`: Define tabelas de precedência para operadores aritméticos e lógicos, além de funções relacionadas à análise de tipos de dados.

## Executando o Compilador

Para compilar e executar o compilador, siga estas etapas:

1. Certifique-se de ter o compilador GHC (Glasgow Haskell Compiler) instalado no seu sistema. Você pode baixá-lo em [https://www.haskell.org/ghc/](https://www.haskell.org/ghc/).

2. Clone este repositório:

   ```bash
   git clone https://github.com/ana-athayde/analise_sintatica_compilador_haskell
   ```

3. Navegue até o diretório do projeto:

   ```bash
   cd analise_sintatica_compilador_haskell
   ```

4. Compile o projeto com o comando `make`:

   ```bash
   make
   ```

5. Isso criará um executável chamado "Main".

6. Execute o compilador com o comando:

   ```bash
   ./Main
   ```

## Próximas Etapas

Este projeto é a primeira fase do desenvolvimento do compilador. As próximas etapas incluem a análise semântica (verificação de tipos) e a geração de código a partir da representação intermediária. Estamos animados para continuar aprimorando e expandindo este compilador.

Se você tiver alguma dúvida ou quiser contribuir para o projeto, sinta-se à vontade para entrar em contato ou abrir uma issue neste repositório. Obrigado por conferir nosso trabalho!

