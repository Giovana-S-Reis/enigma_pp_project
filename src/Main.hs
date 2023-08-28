import Data.List
import System.IO
import Data.Char
import System.Random (randomIO)
import Control.Applicative
import Path_paises
import Cesar
import System.Timeout (timeout)
import Graphics.Gloss.Interface.Pure.Game

--Declaração do número máximo de erros
numeroMaxErros :: Int
numeroMaxErros = 8

mostrarPalavra :: String -> String
mostrarPalavra palavra = encode 3 palavra

tentarLetraIO :: String -> Char -> Int -> IO GameState
tentarLetraIO palavra letra tentativas
    | letra `elem` palavra = jogo (atualizaPalavra palavra letra) tentativas
    | otherwise = jogo palavra (tentativas - 1)
    where
        atualizaPalavra :: String -> Char -> String
        atualizaPalavra palavra char = [if a == char then toUpper a else a | a <- palavra]

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
jogo :: String -> Int -> IO GameState
jogo palavra tentativas
    | palavra == map toUpper palavra = do
        putStrLn $ mostrarPalavra palavra
        putStrLn ("Voce Ganhou! "++ map toUpper palavra ++ " está livre do bombardeio.")
        return (initialState "" 0)
    | tentativas == 0 = do
        putStrLn $ mostrarPalavra palavra
        putStrLn ("Voce Perdeu..." ++ map toUpper palavra ++ " sofreu um bombardeio.")
        return (initialState "" 0)
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
                then tentarLetraIO palavra (head tentativaDeLetra) tentativas
                else do
                    acertouPalavra <- adivinharPalavra palavra tentativaDeLetra
                    if acertouPalavra
                        then do
                            putStrLn ("Voce Ganhou! "++ map toUpper palavra ++ " está livre do bombardeio.")
                            return (initialState "" 0)
                        else do
                            putStrLn ("Tentativa incorreta! " ++ map toUpper palavra ++ " sofreu um bombardeio.")
                            return (initialState "" (tentativas - 1))


data GameState = GameState
    { palavraSecreta :: String
    , tentativasRestantes :: Int
    , inputAtual :: String
    }

initialState :: String -> Int -> GameState
initialState palavra tentativas = GameState
    { palavraSecreta = palavra
    , tentativasRestantes = tentativas
    , inputAtual = ""
    }

window :: Display
window = InWindow "Enigma Game" (800, 600) (10, 10)

background :: Color
background = white

drawGame :: GameState -> Picture
drawGame gameState = pictures
    [ translate (-500) 0 $ scale 0.5 0.5 $ text ("Palavra: " ++ mostrarPalavra (palavraSecreta gameState))
    , translate (-200) (-50) $ scale 0.5 0.5 $ text ("Tentativas restantes: " ++ show (tentativasRestantes gameState))
    , translate (-200) (-100) $ scale 0.5 0.5 $ text "Digite uma letra ou tente adivinhar o nome do país:"
    , translate (-200) (-150) $ scale 0.5 0.5 $ text ("Input atual: " ++ inputAtual gameState)
    ]

processInput :: GameState -> String -> GameState
processInput gameState input =
    case input of
        [c] -> gameState { inputAtual = "" }  -- Limpa o input após a tentativa de adivinhação
        _   -> gameState { palavraSecreta = "", inputAtual = "" }


handleInput :: Event -> GameState -> GameState
handleInput (EventKey (Char c) Down _ _) gameState =
    gameState { inputAtual = inputAtual gameState ++ [c] }
handleInput (EventKey (SpecialKey KeyEnter) Down _ _) gameState =
    if null (inputAtual gameState)
        then gameState
        else processInput gameState (inputAtual gameState)
handleInput _ gameState = gameState


updateGame :: Float -> GameState -> GameState
updateGame _ gameState = gameState

main :: IO ()
main = do
    putStrLn "Bem vindo ao Enigma!"
    palavra <- sorteiaPalavra
    let initialState' = initialState palavra numeroMaxErros
    play window background 60 initialState' drawGame handleInput updateGame


-- Inicia o jogo
{-main :: IO ()
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
        Nothing -> putStrLn ("Tempo esgotado! " ++ palavra ++ " sofreu um bombardeio.")
-}