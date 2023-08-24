import Data.List
import System.IO
import Data.Char
import System.Random (randomIO)
import Control.Applicative
import Path_paises
import Cesar


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

-- Funcao que realiza o jogo e o loop do jogo, verificando se ainda restam numero de tentativas e apresentando imagem da forca
jogo :: String -> Int -> IO ()
jogo palavra tentativas
	| palavra == map toUpper palavra = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Ganhou!"
	| tentativas == 0 = do
		putStrLn $ mostrarPalavra palavra
		putStrLn "Voce Perdeu..."
	| otherwise = do
		putStrLn $ "Voce tem " ++ show tentativas ++ " tentativas restantes."
		putStrLn $ mostrarPalavra palavra
		putStr "Digite uma letra: "
		tentativaDeLetra <- getLine
		tentarLetra palavra (head tentativaDeLetra) tentativas

-- Inicia o jogo
main :: IO()
main = do
	hSetBuffering stdout NoBuffering --
	putStrLn "Bem vindo ao Jogo da Forca"
	palavra <- sorteiaPalavra
	jogo (map toLower palavra) numeroMaxErros
	putStrLn "Obrigado por jogar! :)"		
