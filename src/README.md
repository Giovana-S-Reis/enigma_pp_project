# Forca-Haskell

Módulo II da disciplina Paradigmas de Programação da Universidade de Brasília

## Instruções para execução
 
 1. Baixe o código para um diretório local. Podem ser utilizadas tanto a opção clone quanto apenas baixar o .zip que o gitHub disponibiliza.
 2. Abra o terminal e vá para o diretório onde se encontra os arquivos que foram baixados.
 3. Tendo o interpretador GHC do haskell instalado, digite o comando `ghci` no terminal. (Caso necessite instalar, clique aqui [GHC do Haslkell](https://www.haskell.org/ghc/download) )
 4. Carregue o arquivo com o comando `:load forca.hs`.
 5. Execute a função `main` no terminal. Note a necessidade ter se ter o arquivo dicionario.txt dentro do mesmo diretório da aplicação.
 6. Desfrute de toda a diversão que esta forca pode lhe porpocionar. 
 
 ## O que é este trabalho?
 
 Esta aplicação em haskell foi desenvolvida com caráter totalmente didático dentro de uma disciplina de Paradigmas de Programação ministrada por [Milene Serrano](https://fga.unb.br/milene.serrano/) na Universidade de Brasília.
 Neste módulo da disciplina, fomos responsáveis pela implementação de alguma aplicação em Haskell, de forma que pudessemos entender melhor o paradigma funcional e assim começar a pensar em contextos para sua aplicação.
 
 ## Definições da Forca
 
 Comumente chamado de Forca no brasil ("Hangman" em inglês ), este puzzle tem a particularidade de ser conhecido por praticamente qualquer pessoa. Isso se dá porque ele pode ser jogado com qualquer pedaço de papel e um lápis ou caneta.
 O jogo é simples, com no mínimo duas pessoas necessárias para se iniciar, um terá em seu poder o papel e a caneta. Esta pessoa pensa em uma palavra a qual não será dita ao seu colega.
 O número de letras desta palavra é descrita na quantidade de traços que o jogador informa ao outro. A partir disto, o jogador que não tem conhecimento da palavra começa a falar letras que poderão estar contidas na palavra descrita por traços no papel.
 A cada letra que não é encontrada dentro na palavra, é desenhada uma parte do corpo do jogdador no papel. Quando este tiver seu corpo completo na forca, este terá perdido o jogo.
 
 Para nossa aplicação, o número de partes do corpo foi limitada a 6, contendo cabeça, braços esquerdo e direito, tronco e pernas direta e esquerda. Isso significa que o jogador poderá errar até seis letras antes que perca o jogo.
 
 ## Informações importantes
 
 Para que este trabalho fosse construído, utilizamos de alguns outros trabalhos em haskell que podem ser encontrados na internet. 
 Como desenvolvedores, entendemos que nosso trabalho deve ser sempre respeitado e por isso abaixo encontramse as fontes das quais utilizamos para criar este trabalho que tem um caráter apenas didático dentro de uma disciplina de Paradigmas de Programação na Universidade de Brasília.
 
 * [Fórum de duvidas sobre haskell](http://haskell.1045720.n5.nabble.com/Hangman-game-td3106973.html)
 * [Wikibooks - Haskell/Arrow tutorial](https://en.wikibooks.org/wiki/Haskell/Arrow_tutorial)
 * [Hangman Game in Haskell Video Tutorial- Part 1/2](https://www.youtube.com/watch?v=eNPQvKRFdbQ)
 * [Fórum de Dúvidas Portugal-a-programar - Hangman](http://www.portugal-a-programar.pt/topic/33198-explicacoes-para-jogo-em-haskell/)
 * [Livro "Learn You a Haskell for Great Good!" - Disponibiliza seu uso gratuito online](http://learnyouahaskell.com/chapters)
 * [Solução de uma forca implementada por Tom Janssens - Disponível publicamente pelo github](https://gist.github.com/ToJans/e97db3b4ed3902677361)
 * [Aprenda mais sobre programação funcional](https://en.wikipedia.org/wiki/Functional_programming)
