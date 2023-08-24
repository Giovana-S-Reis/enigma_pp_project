import Data.List
import System.IO
import Data.Char
import System.Random (randomIO)
import Control.Applicative
import Path_paises
import Cesar
import System.Timeout (timeout)

--Declaração do número máximo de erros
numeroMaxErros :: Int
numeroMaxErros = 8

--Esccolhe o valor do shift aplicado
{-randomNumber :: IO Int
randomNumber = randomRIO (1, 26)

num <- randomNumber
-}


mostrarPalavra :: String -> String
mostrarPalavra palavra = encode 3 palavra

tentarLetra :: String -> Char -> Int -> IO ()
tentarLetra palavra letra tentativas
	| letra `elem` palavra 	= jogo [if letra == a then toUpper letra else a | a <- palavra] tentativas
	| otherwise = jogo palavra (tentativas -1)

-- Esta funcao faz um sorteio de uma palvra dentro de um arquivo .txt de forma aleatoria
sorteiaPalavra :: IO[Char]
sorteiaPalavra = do
	discionario <- readFile listaPaises
	let palavras = filter palavraValida $ lines discionario
	let numeroPalavras = length palavras
	numeroAleatorio <- randomIO
	let palavraAleatoria = palavras !! (numeroAleatorio `mod` numeroPalavras)
	return $ palavraAleatoria
	where 
		palavraValida palavra =
			'\'' `notElem` palavra &&
			map toUpper palavra == palavra

-- Função para alternar entre os jogadores (1 ou 2)
alternarJogador :: Int -> Int
alternarJogador 1 = 2
alternarJogador 2 = 1
alternarJogador _ = 1

adivinharPalavra :: String -> String -> IO Bool
adivinharPalavra palavra tentativa = return (map toUpper palavra == map toUpper tentativa)


-- Funcao que realiza o jogo e o loop do jogo, verificando se ainda restam numero de tentativas
jogo :: String -> Int -> IO ()
jogo palavra tentativas
    | palavra == map toUpper palavra = do
        putStrLn $ mostrarPalavra palavra
        putStrLn ("Voce Ganhou! "++ map toUpper palavra ++ " está livre do bombardeio.") 
    | tentativas == 0 = do
        putStrLn $ mostrarPalavra palavra
        putStrLn ("Voce Perdeu..." ++ map toUpper palavra ++ " sofreu um bombardeio.")
    | otherwise = do
        putStrLn $ "Voce tem " ++ show tentativas ++ " tentativas restantes."
        putStrLn $ mostrarPalavra palavra
        putStr "Digite uma letra ou tente adivinhar o nome do país: "
        tentativaDeLetra <- getLine
        if null tentativaDeLetra
            then do
                putStrLn "Nenhuma letra digitada. Por favor, tente novamente."
                jogo palavra tentativas
            else if length tentativaDeLetra == 1
                then do
                    tentarLetra palavra (head tentativaDeLetra) tentativas
                else do
                    acertouPalavra <- adivinharPalavra palavra tentativaDeLetra
                    if acertouPalavra
                        then putStrLn ("Voce Ganhou! "++ map toUpper palavra ++ " está livre do bombardeio.") 
                        else do
                            putStrLn ("Tentativa incorreta! " ++ map toUpper palavra ++ " sofreu um bombardeio.")


-- Inicia o jogo
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    putStrLn "Bem vindo ao Enigma!\nO Exercito Nazista decidiu bombardear um país. Com sorte interceptamos a mensagem com o nome! Agora o seu dever é decodificar a mensagem antes que o bombardeio ocorra!\nMas cuidado, suspeitamos de um espião entre nós. É possível que sempre que você adivinhe uma letra a criptografia mude."
    palavra <- sorteiaPalavra

    -- Defina o tempo total de jogo em microssegundos (5 minutos)
    let tempoTotal = 300 * 1000000

    -- Use a função timeout para limitar o tempo total de jogo
    mResultado <- timeout tempoTotal (jogo (map toLower palavra) numeroMaxErros)

    -- Verifique o resultado do jogo
    case mResultado of
        Just () -> putStrLn "Obrigado por jogar! :)"
        Nothing -> putStrLn ("Tempo esgotado! " ++ map toUpper palavra ++ " sofreu um bombardeio.")
