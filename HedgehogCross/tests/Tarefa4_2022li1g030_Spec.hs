module Tarefa4_2022li1g030_Spec (testsT4) where

import Test.HUnit

import LI12223
import Tarefa4_2022li1g030

n, t, c, a :: Obstaculo
n = Nenhum
t = Tronco
c = Carro
a = Arvore

mapa :: Mapa
mapa = Mapa 3
  [ (Relva, [a, n, n])
  , (Rio 1, [t, n, t])
  , (Estrada (-1), [c, n, n])
  ]

terminou :: Coordenadas -> Bool
terminou posicao = jogoTerminou (Jogo (Jogador posicao) mapa)

testsT4 :: Test
testsT4 = TestLabel "Tarefa 4" $ test
  [ "na relva" ~: False ~=? terminou (1, 0)
  , "em cima de um tronco" ~: False ~=? terminou (0, 1)
  , "na agua" ~: True ~=? terminou (1, 1)
  , "na estrada, sem carro" ~: False ~=? terminou (1, 2)
  , "debaixo de um carro" ~: True ~=? terminou (0, 2)
  , "fora pela esquerda" ~: True ~=? terminou (-1, 0)
  , "fora pela direita" ~: True ~=? terminou (3, 1)
  , "fora por cima" ~: True ~=? terminou (1, -1)
  , "fora por baixo" ~: True ~=? terminou (1, 3)
  , "mapa vazio" ~: True ~=? jogoTerminou (Jogo (Jogador (0, 0)) (Mapa 3 []))
  , "foraDoMapa" ~: [False, True] ~=? map (\p -> foraDoMapa (Jogo (Jogador p) mapa)) [(2, 2), (3, 2)]
  , "posicaoFatal" ~: [True, True, False, False, False]
      ~=? map posicaoFatal [(Rio 1, n), (Estrada 1, c), (Rio 1, t), (Estrada 1, n), (Relva, a)]
  ]
