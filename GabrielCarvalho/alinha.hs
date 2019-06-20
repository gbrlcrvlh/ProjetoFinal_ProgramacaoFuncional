import System.IO  
import Data.Char
import Data.List

-- x = tamanho da linha, cria espaços para alinhas a esquerda
pDireita x = replicate (59-x) ' '

-- x = tamanho da linha, cria espaços para o meio da string bater com o meio da linha
pCentro x = replicate (div (59-x) 2) ' '

-- xs = linha do arquivo, transforma texto da esquerda para direita
transformaEpD xs = pDireita (length xs) ++ xs

-- xs = linha do arquivo, transforma texto da esquerda para o centro
transformaEpC xs = pCentro (length xs) ++ xs

-- a = primeiro caracter da linha, x = resto da linha sem a
-- transforma centralizado ou direita para esquerda
transformaCDpE [] = []
transformaCDpE (a:x)
 | a == ' ' = transformaCDpE x
 | otherwise = x

main = do
 conteudo <- readFile "texto.txt"
 let linha = lines conteudo
 writeFile "tudodireita.txt" (unlines $ map transformaEpD linha)
 writeFile "tudocentralizado.txt" (unlines $ map transformaEpC linha)
 
 -- transforma de volta o arquivo tudodireita.txt para esquerda
 conteudo <- readFile "tudodireita.txt"
 let linha = lines conteudo
 writeFile "devoltaesquerda.txt" (unlines $ map transformaCDpE linha)