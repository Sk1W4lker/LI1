{- |
Module      : Tarefa2_2022li1g030
Description : Geração contínua de um mapa
Copyright   : Pedro

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.

A função 'estendeMapa' acrescenta uma nova linha ao topo de um mapa. A linha é
construída célula a célula e, em cada passo, só são usados obstáculos
devolvidos por 'proximosObstaculosValidos', pelo que o resultado respeita
sempre as restrições da Tarefa 1. Além de válida, a linha é pensada para ser
jogável: os rios têm sempre troncos, as estradas têm intervalos entre carros e
a relva nunca fecha o caminho ao jogador.
-}
module Tarefa2_2022li1g030 (
  -- * Funções principais
  estendeMapa, proximosTerrenosValidos, proximosObstaculosValidos,
  -- * Geração de uma linha
  geraTerreno, geraVelocidade, geraLinha, abreCaminho,
  -- * Funções auxiliares
  obstaculoDoTerreno, segmentosLivres, escolhe
  ) where

import Data.Function (on)
import Data.List (groupBy)
import System.Random (StdGen, mkStdGen, random, randomR)

import LI12223
import Tarefa1_2022li1g030 (limiteComprimento, limiteContiguo, mesmoTipo)

{- | Acrescenta uma nova linha válida ao topo do mapa.

O inteiro recebido é usado como semente de um gerador pseudo-aleatório: a
mesma semente aplicada ao mesmo mapa produz sempre a mesma linha.

Assume-se que o mapa dado é válido; nesse caso o mapa devolvido também o é.
-}
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa mapa@(Mapa largura linhas) semente = Mapa largura ((terreno, obstaculos) : linhas)
  where
    (tipo, g1)    = geraTerreno mapa (mkStdGen semente)
    (terreno, g2) = geraVelocidade linhas tipo g1
    gerados       = geraLinha largura terreno g2
    obstaculos    = case (terreno, linhas) of
      (Relva, (Relva, abaixo) : _) -> abreCaminho abaixo gerados
      _                            -> gerados

{- | Lista os terrenos que podem ser usados numa nova linha no topo do mapa,
todos com velocidade 0. Um tipo de terreno deixa de ser válido quando o topo
do mapa já tem o número máximo de linhas seguidas desse tipo.

>>> proximosTerrenosValidos (Mapa 2 [])
[Rio 0,Estrada 0,Relva]
-}
proximosTerrenosValidos :: Mapa -> [Terreno]
proximosTerrenosValidos (Mapa _ linhas) =
  [ t | t <- [Rio 0, Estrada 0, Relva], seguidos t < limiteContiguo t ]
  where seguidos t = length (takeWhile (mesmoTipo t . fst) linhas)

{- | Lista os obstáculos que podem continuar uma linha ainda incompleta, dada a
largura do mapa e a linha construída até ao momento (da esquerda para a
direita).

* Se a linha já está completa, não há obstáculos possíveis.
* 'Nenhum' é sempre possível numa linha incompleta.
* O obstáculo próprio do terreno só é possível se não ultrapassar o seu
  comprimento máximo e se não deixar a linha sem nenhum espaço livre. Na
  última célula conta-se também com os obstáculos do início da linha, porque
  a linha é circular.

>>> proximosObstaculosValidos 10 (Rio 1, [])
[Nenhum,Tronco]
>>> proximosObstaculosValidos 2 (Estrada 1, [Carro, Nenhum])
[]
-}
proximosObstaculosValidos :: Int -> (Terreno, [Obstaculo]) -> [Obstaculo]
proximosObstaculosValidos largura (terreno, linha)
  | length linha >= largura = []
  | otherwise               = Nenhum : filter cabe [obstaculoDoTerreno terreno]
  where
    ultima   = length linha == largura - 1
    noFim o  = length (takeWhile (== o) (reverse linha))
    noInicio o
      | ultima    = length (takeWhile (== o) linha)
      | otherwise = 0
    cabe o = not (ultima && Nenhum `notElem` linha)
          && maybe True (noFim o + 1 + noInicio o <=) (limiteComprimento o)

-- | O obstáculo característico de cada terreno.
obstaculoDoTerreno :: Terreno -> Obstaculo
obstaculoDoTerreno (Rio _)     = Tronco
obstaculoDoTerreno (Estrada _) = Carro
obstaculoDoTerreno Relva       = Arvore

{- | Escolhe o tipo de terreno da nova linha entre os terrenos válidos. As
estradas são ligeiramente mais prováveis do que os rios e a relva.
-}
geraTerreno :: Mapa -> StdGen -> (Terreno, StdGen)
geraTerreno mapa = escolhe (concatMap pesado (proximosTerrenosValidos mapa))
  where
    pesado t = replicate (peso t) t
    peso :: Terreno -> Int
    peso (Estrada _) = 4
    peso _           = 3

{- | Atribui uma velocidade ao terreno escolhido.

* A relva não tem velocidade.
* Os rios têm velocidade 1 ou 2 e correm no sentido oposto ao do rio
  imediatamente abaixo, caso exista.
* As estradas têm velocidade entre 1 e 3, em qualquer sentido.
-}
geraVelocidade :: [(Terreno, [Obstaculo])] -> Terreno -> StdGen -> (Terreno, StdGen)
geraVelocidade _ Relva g = (Relva, g)
geraVelocidade linhas (Rio _) g = (Rio (sentido * modulo), g2)
  where
    (modulo, g1) = randomR (1, 2) g
    (sentido, g2) = case linhas of
      (Rio v, _) : _ | v /= 0 -> (negate (signum v), g1)
      _                       -> geraSentido g1
geraVelocidade _ (Estrada _) g = (Estrada (sentido * modulo), g2)
  where
    (modulo, g1)  = randomR (1, 3) g
    (sentido, g2) = geraSentido g1

-- | Escolhe um sentido: 1 (para a direita) ou -1 (para a esquerda).
geraSentido :: StdGen -> (Int, StdGen)
geraSentido g = (if paraDireita then 1 else -1, g')
  where (paraDireita, g') = random g

{- | Gera os obstáculos de uma linha com a largura dada.

A linha é construída alternando grupos de obstáculos com intervalos livres,
cujos comprimentos são sorteados de acordo com o terreno. Antes de colocar
cada obstáculo confirma-se que ele pertence a 'proximosObstaculosValidos';
caso contrário, começa-se um intervalo livre.
-}
geraLinha :: Int -> Terreno -> StdGen -> [Obstaculo]
geraLinha largura terreno g0 = constroi [] planoInicial g2
  where
    obstaculo        = obstaculoDoTerreno terreno
    (comecaLivre, g1) = random g0
    (planoInicial, g2) = novoPlano (if comecaLivre then Nenhum else obstaculo) g1

    constroi linha (alvo, restantes) g
      | length linha >= largura = linha
      | restantes <= 0          = uncurry (constroi linha) (novoPlano (alternado alvo) g)
      | alvo `elem` validos     = constroi (linha ++ [alvo]) (alvo, restantes - 1) g
      | otherwise               = uncurry (constroi linha) (novoPlano Nenhum g)
      where validos = proximosObstaculosValidos largura (terreno, linha)

    alternado alvo = if alvo == Nenhum then obstaculo else Nenhum

    novoPlano :: Obstaculo -> StdGen -> ((Obstaculo, Int), StdGen)
    novoPlano alvo g = ((alvo, comprimento), g')
      where (comprimento, g') = escolhe (comprimentos terreno alvo) g

{- | Comprimentos possíveis para um grupo de obstáculos (ou para um intervalo
livre, quando o obstáculo é 'Nenhum') em cada terreno. Valores repetidos são
mais prováveis.
-}
comprimentos :: Terreno -> Obstaculo -> [Int]
comprimentos (Rio _)     Nenhum = [1, 2, 2, 3]
comprimentos (Rio _)     _      = [2, 3, 3, 4]
comprimentos (Estrada _) Nenhum = [3, 4, 5, 6]
comprimentos (Estrada _) _      = [1, 1, 1, 2, 2, 3]
comprimentos Relva       Nenhum = [2, 3, 4, 5]
comprimentos Relva       _      = [1, 1, 2]

{- | Garante que uma linha de relva colocada por cima de outra linha de relva
não fecha o caminho.

Para cada grupo de espaços livres da linha de baixo tem de existir, pelo
menos, um espaço livre imediatamente acima. Quando isso não acontece, a árvore
por cima do meio do grupo é removida. Assim, de qualquer espaço livre da linha
de baixo é sempre possível chegar à linha de cima.

>>> abreCaminho [Arvore, Nenhum, Nenhum, Arvore] [Nenhum, Arvore, Arvore, Nenhum]
[Nenhum,Arvore,Nenhum,Nenhum]
-}
abreCaminho :: [Obstaculo] -- ^ linha de baixo
            -> [Obstaculo] -- ^ linha nova
            -> [Obstaculo]
abreCaminho abaixo nova = [ if i `elem` aAbrir then Nenhum else o | (i, o) <- zip [0 ..] nova ]
  where
    aAbrir = [ grupo !! (length grupo `div` 2) | grupo <- segmentosLivres abaixo, all tapado grupo ]
    tapado i = take 1 (drop i nova) == [Arvore]

{- | Índices de cada grupo de espaços livres consecutivos de uma linha.

>>> segmentosLivres [Nenhum, Nenhum, Arvore, Nenhum]
[[0,1],[3]]
-}
segmentosLivres :: [Obstaculo] -> [[Int]]
segmentosLivres obstaculos =
  [ map fst grupo
  | grupo@((_, Nenhum) : _) <- groupBy ((==) `on` snd) (zip [0 ..] obstaculos) ]

-- | Escolhe aleatoriamente um elemento de uma lista não vazia.
escolhe :: [a] -> StdGen -> (a, StdGen)
escolhe opcoes g = (opcoes !! indice, g')
  where (indice, g') = randomR (0, length opcoes - 1) g
