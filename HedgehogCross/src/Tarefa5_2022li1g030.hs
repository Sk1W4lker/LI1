{- |
Module      : Tarefa5_2022li1g030
Description : Deslize do mapa
Copyright   : Pedro

Módulo para a realização da Tarefa 5 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g030 (
  deslizaJogo
  ) where

import LI12223
import Tarefa2_2022li1g030 (estendeMapa)

{- | Faz deslizar o jogo uma linha: é acrescentada uma nova linha ao topo do
mapa (gerada por 'estendeMapa' com a semente dada), a última linha é removida
e a ordenada do jogador aumenta 1 unidade, refletindo que ele ficou para trás.

O mapa mantém sempre o mesmo número de linhas. A nova linha é gerada antes de
remover a última, para que o mapa resultante continue a ser válido.

>>> let Jogo j (Mapa l ls) = deslizaJogo 7 (Jogo (Jogador (1,1)) (Mapa 2 [(Relva,[Nenhum,Nenhum]),(Relva,[Nenhum,Arvore])]))
>>> (j, l, length ls, last ls)
(Jogador (1,2),2,2,(Relva,[Nenhum,Nenhum]))
-}
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo semente (Jogo (Jogador (x, y)) mapa@(Mapa largura linhas)) =
  Jogo (Jogador (x, y + 1)) (Mapa largura (take (length linhas) estendidas))
  where Mapa _ estendidas = estendeMapa mapa semente
