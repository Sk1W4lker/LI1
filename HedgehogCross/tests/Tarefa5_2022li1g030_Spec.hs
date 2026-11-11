module Tarefa5_2022li1g030_Spec (testsT5) where

import Test.HUnit

import LI12223
import Tarefa1_2022li1g030 (mapaValido)
import Tarefa2_2022li1g030 (estendeMapa)
import Tarefa5_2022li1g030

n, t, c, a :: Obstaculo
n = Nenhum
t = Tronco
c = Carro
a = Arvore

linhas :: [(Terreno, [Obstaculo])]
linhas =
  [ (Relva, [n, a, n, a, n])
  , (Rio 1, [t, t, t, n, n])
  , (Estrada 1, [n, n, n, c, c])
  ]

jogo :: Jogo
jogo = Jogo (Jogador (1, 1)) (Mapa 5 linhas)

testsT5 :: Test
testsT5 = TestLabel "Tarefa 5" $ test
  [ "o jogador fica uma linha para tras" ~:
      Jogador (1, 2) ~=? (let Jogo j _ = deslizaJogo 5 jogo in j)
  , "o mapa mantem as dimensoes" ~:
      (5, 3) ~=? (let Jogo _ (Mapa l ls) = deslizaJogo 5 jogo in (l, length ls))
  , "a ultima linha desaparece e as outras descem" ~:
      take 2 linhas ~=? (let Jogo _ (Mapa _ ls) = deslizaJogo 28 jogo in drop 1 ls)
  , "a nova linha e a de estendeMapa" ~:
      (let Mapa _ ls = estendeMapa (Mapa 5 linhas) 1673 in take 1 ls)
      ~=? (let Jogo _ (Mapa _ ls) = deslizaJogo 1673 jogo in take 1 ls)
  , "o mapa continua valido depois de muitos deslizes" ~:
      True ~=? and [ mapaValido m | Jogo _ m <- take 100 (scanl (flip deslizaJogo) jogo [0 ..]) ]
  , "o jogador pode sair do mapa" ~:
      Jogador (4, 3) ~=? (let Jogo j _ = deslizaJogo 9 (Jogo (Jogador (4, 2)) (Mapa 5 linhas)) in j)
  , "mapa vazio" ~:
      Jogo (Jogador (0, 1)) (Mapa 5 []) ~=? deslizaJogo 3 (Jogo (Jogador (0, 0)) (Mapa 5 []))
  ]
