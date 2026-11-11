{- |
Module      : Tarefa3_2022li1g030
Description : Movimentação do personagem e obstáculos
Copyright   : Pedro

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.

A função 'animaJogo' calcula o estado seguinte de um jogo: primeiro move o
jogador, de acordo com a jogada, e depois move os obstáculos de cada linha, de
acordo com a velocidade do terreno. Segue as regras do enunciado da Fase 1 e
os esclarecimentos da adenda da Fase 2 (atropelamentos e movimentos em cima
de troncos).
-}
module Tarefa3_2022li1g030 (
  -- * Função principal
  animaJogo,
  -- * Movimento do jogador
  animaJogador, celula, posicaoLivre,
  -- * Movimento dos obstáculos
  animaLinha, roda, moveCarros
  ) where

import LI12223

{- | Anima um jogo: move o jogador segundo a jogada dada e, em seguida, os
obstáculos de todas as linhas. As dimensões do mapa nunca se alteram.

>>> animaJogo (Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 2, [Carro, Nenhum, Nenhum])])) Parado
Jogo (Jogador (1,0)) (Mapa 3 [(Estrada 2,[Nenhum,Carro,Nenhum])])
-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo jogo@(Jogo _ (Mapa largura linhas)) jogada =
  Jogo jogador (Mapa largura (zipWith (animaLinha jogador) [0 ..] linhas))
  where jogador = animaJogador jogo jogada

{- | Calcula a nova posição do jogador.

* Fora de um tronco, o jogador move-se 1 unidade na direção pedida, exceto se
  isso o levasse para fora do mapa ou para cima de uma árvore.
* Em cima de um tronco, um movimento vertical leva o jogador diretamente para
  a linha de cima ou de baixo, sem acompanhar o tronco.
* Em cima de um tronco, um movimento horizontal equivale a mover primeiro o
  tronco e depois o jogador.
* Se o jogador ficar parado (ou se o seu movimento for impedido) em cima de um
  tronco, acompanha o movimento do tronco, podendo assim sair do mapa.
-}
animaJogador :: Jogo -> Jogada -> Jogador
animaJogador (Jogo (Jogador (x, y)) mapa) jogada = Jogador $ case jogada of
  Move Cima     | posicaoLivre mapa (x, y - 1) -> (x, y - 1)
  Move Baixo    | posicaoLivre mapa (x, y + 1) -> (x, y + 1)
  Move Esquerda | noTronco                     -> (x + arrasto - 1, y)
                | posicaoLivre mapa (x - 1, y) -> (x - 1, y)
  Move Direita  | noTronco                     -> (x + arrasto + 1, y)
                | posicaoLivre mapa (x + 1, y) -> (x + 1, y)
  _                                            -> (x + arrasto, y)
  where
    (noTronco, arrasto) = case celula mapa (x, y) of
      Just (Rio v, Tronco) -> (True, v)
      _                    -> (False, 0)

{- | O terreno e o obstáculo existentes numa posição do mapa, ou 'Nothing' se a
posição estiver fora do mapa.

>>> celula (Mapa 2 [(Relva, [Nenhum, Arvore])]) (1, 0)
Just (Relva,Arvore)
-}
celula :: Mapa -> Coordenadas -> Maybe (Terreno, Obstaculo)
celula (Mapa largura linhas) (x, y)
  | x < 0 || x >= largura || y < 0 = Nothing
  | otherwise = case drop y linhas of
      (terreno, obstaculos) : _ -> case drop x obstaculos of
        o : _ -> Just (terreno, o)
        []    -> Nothing
      [] -> Nothing

-- | Indica se o jogador se pode mover para uma posição: tem de pertencer ao mapa e não ter uma árvore.
posicaoLivre :: Mapa -> Coordenadas -> Bool
posicaoLivre mapa posicao = case celula mapa posicao of
  Nothing          -> False
  Just (_, Arvore) -> False
  Just _           -> True

{- | Move os obstáculos de uma linha, sabendo a posição (já atualizada) do
jogador e o índice da linha.

Os rios e as estradas rodam os seus obstáculos tantas unidades quanto a sua
velocidade. A exceção é a estrada onde o jogador se encontra: aí os carros
avançam uma unidade de cada vez e param no momento em que o atropelam
('moveCarros').
-}
animaLinha :: Jogador -> Int -> (Terreno, [Obstaculo]) -> (Terreno, [Obstaculo])
animaLinha (Jogador (x, y)) indice (terreno, obstaculos) = case terreno of
  Rio v                   -> (terreno, roda v obstaculos)
  Estrada v | indice == y -> (terreno, moveCarros x v obstaculos)
            | otherwise   -> (terreno, roda v obstaculos)
  Relva                   -> (terreno, obstaculos)

{- | Roda uma lista de forma circular: para a direita se o número for positivo
e para a esquerda se for negativo. Os elementos que saem por um lado
reaparecem no lado oposto.

>>> roda 1 [Carro, Nenhum, Nenhum]
[Nenhum,Carro,Nenhum]
>>> roda (-1) [Carro, Nenhum, Nenhum]
[Nenhum,Nenhum,Carro]
-}
roda :: Int -> [a] -> [a]
roda _ [] = []
roda k lista = fim ++ inicio
  where
    n             = length lista
    (inicio, fim) = splitAt (n - k `mod` n) lista

{- | Move os carros da estrada onde está o jogador, dada a abcissa do jogador e
a velocidade da estrada.

Os carros avançam uma unidade de cada vez. Se, em algum momento (incluindo
antes do primeiro passo), houver um carro na posição do jogador, o movimento
de toda a linha é interrompido.

>>> moveCarros 3 3 [Carro, Nenhum, Carro, Nenhum, Nenhum, Nenhum]
[Nenhum,Carro,Nenhum,Carro,Nenhum,Nenhum]
-}
moveCarros :: Int -> Velocidade -> [Obstaculo] -> [Obstaculo]
moveCarros x v = passos (abs v)
  where
    passos :: Int -> [Obstaculo] -> [Obstaculo]
    passos 0 obstaculos = obstaculos
    passos k obstaculos
      | atropelado obstaculos = obstaculos
      | otherwise             = passos (k - 1) (roda (signum v) obstaculos)
    atropelado obstaculos = x >= 0 && take 1 (drop x obstaculos) == [Carro]
