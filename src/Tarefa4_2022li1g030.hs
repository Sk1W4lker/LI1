{- |
Module      : Tarefa4_2022li1g030
Description : Determinar se o jogo terminou
Copyright   : Pedro

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g030 (
  -- * Função principal
  jogoTerminou,
  -- * Funções auxiliares
  foraDoMapa, posicaoFatal
  ) where

import LI12223
import Tarefa3_2022li1g030 (celula)

{- | Indica se o jogador perdeu o jogo, o que acontece quando está fora do
mapa, dentro de água ou na mesma posição de um carro.

>>> jogoTerminou (Jogo (Jogador (0,0)) (Mapa 2 [(Rio 1, [Nenhum, Tronco])]))
True
>>> jogoTerminou (Jogo (Jogador (1,0)) (Mapa 2 [(Rio 1, [Nenhum, Tronco])]))
False
-}
jogoTerminou :: Jogo -> Bool
jogoTerminou jogo@(Jogo (Jogador posicao) mapa) =
  foraDoMapa jogo || maybe False posicaoFatal (celula mapa posicao)

-- | Indica se o jogador está fora dos limites do mapa.
foraDoMapa :: Jogo -> Bool
foraDoMapa (Jogo (Jogador (x, y)) (Mapa largura linhas)) =
  x < 0 || x >= largura || y < 0 || y >= length linhas

{- | Indica se uma célula é fatal para o jogador: a água de um rio (um rio sem
tronco) ou um carro numa estrada.
-}
posicaoFatal :: (Terreno, Obstaculo) -> Bool
posicaoFatal (Rio _, Nenhum)    = True
posicaoFatal (Estrada _, Carro) = True
posicaoFatal _                  = False
