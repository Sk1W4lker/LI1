module Tarefa3_2022li1g030_Spec (testsT3) where

import Test.HUnit

import LI12223
import Tarefa3_2022li1g030

n, t, c, a :: Obstaculo
n = Nenhum
t = Tronco
c = Carro
a = Arvore

-- | Mapa de relva com árvores, para testar os movimentos simples.
relvado :: Mapa
relvado = Mapa 3
  [ (Relva, [n, n, a])
  , (Relva, [n, n, n])
  , (Relva, [a, n, n])
  ]

-- | Mapa da adenda da Fase 2, com um rio entre duas relvas.
comRio :: Mapa
comRio = Mapa 3
  [ (Relva, [n, n, n])
  , (Rio 1, [n, t, t])
  , (Relva, [n, n, n])
  ]

comRioDepois :: Mapa
comRioDepois = Mapa 3
  [ (Relva, [n, n, n])
  , (Rio 1, [t, n, t])
  , (Relva, [n, n, n])
  ]

jogadorDe :: Jogo -> Jogador
jogadorDe (Jogo jogador _) = jogador

mapaDe :: Jogo -> Mapa
mapaDe (Jogo _ mapa) = mapa

testsT3 :: Test
testsT3 = TestLabel "Tarefa 3" $ test
  [ -- movimentos do jogador
    "parado na relva" ~: Jogo (Jogador (1, 1)) relvado ~=? animaJogo (Jogo (Jogador (1, 1)) relvado) Parado
  , "move para cima" ~: Jogador (1, 0) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 1)) relvado) (Move Cima))
  , "move para baixo" ~: Jogador (1, 2) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 1)) relvado) (Move Baixo))
  , "move para a esquerda" ~: Jogador (0, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 1)) relvado) (Move Esquerda))
  , "move para a direita" ~: Jogador (2, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 1)) relvado) (Move Direita))
  , "nao sai pelo topo" ~: Jogador (0, 0) ~=? jogadorDe (animaJogo (Jogo (Jogador (0, 0)) relvado) (Move Cima))
  , "nao sai pelo fundo" ~: Jogador (1, 2) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 2)) relvado) (Move Baixo))
  , "nao sai pela esquerda" ~: Jogador (0, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (0, 1)) relvado) (Move Esquerda))
  , "nao sai pela direita" ~: Jogador (2, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (2, 1)) relvado) (Move Direita))
  , "arvore acima trava" ~: Jogador (2, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (2, 1)) relvado) (Move Cima))
  , "arvore abaixo trava" ~: Jogador (0, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (0, 1)) relvado) (Move Baixo))
  , "arvore ao lado trava" ~: Jogador (1, 0) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 0)) relvado) (Move Direita))
  , "arvore a esquerda trava" ~: Jogador (1, 2) ~=? jogadorDe (animaJogo (Jogo (Jogador (1, 2)) relvado) (Move Esquerda))
  , "o mapa de relva nao muda" ~: relvado ~=? mapaDe (animaJogo (Jogo (Jogador (1, 1)) relvado) (Move Cima))

    -- movimento dos obstáculos
  , "obstaculos movem-se com a velocidade" ~:
      Mapa 4 [(Rio 2, [n, n, t, t]), (Estrada (-1), [n, n, c, c]), (Relva, [a, n, n, n])]
      ~=? mapaDe (animaJogo (Jogo (Jogador (1, 2)) (Mapa 4 [(Rio 2, [t, t, n, n]), (Estrada (-1), [c, n, n, c]), (Relva, [a, n, n, n])])) Parado)
  , "velocidade maior do que a largura" ~:
      Mapa 3 [(Rio 4, [n, t, n])] ~=? mapaDe (animaJogo (Jogo (Jogador (5, 5)) (Mapa 3 [(Rio 4, [t, n, n])])) Parado)
  , "velocidade nula" ~:
      Mapa 3 [(Estrada 0, [c, n, n])] ~=? mapaDe (animaJogo (Jogo (Jogador (2, 0)) (Mapa 3 [(Estrada 0, [c, n, n])])) Parado)
  , "roda para a direita" ~: [3, 1, 2] ~=? roda 1 [1, 2, 3 :: Int]
  , "roda para a esquerda" ~: [2, 3, 1] ~=? roda (-1) [1, 2, 3 :: Int]
  , "roda uma volta completa" ~: [1, 2, 3] ~=? roda 3 [1, 2, 3 :: Int]
  , "roda lista vazia" ~: [] ~=? roda 2 ([] :: [Int])

    -- troncos
  , "parado no tronco acompanha-o" ~:
      Jogo (Jogador (2, 1)) comRioDepois ~=? animaJogo (Jogo (Jogador (1, 1)) comRio) Parado
  , "parado no tronco pode sair do mapa" ~:
      Jogador (3, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (2, 1)) comRio) Parado)
  , "tronco para a esquerda leva o jogador" ~:
      Jogo (Jogador (-1, 0)) (Mapa 3 [(Rio (-2), [t, n, t])])
      ~=? animaJogo (Jogo (Jogador (1, 0)) (Mapa 3 [(Rio (-2), [n, t, t])])) Parado
  , "adenda: do tronco para cima" ~:
      Jogo (Jogador (1, 0)) comRioDepois ~=? animaJogo (Jogo (Jogador (1, 1)) comRio) (Move Cima)
  , "adenda: do tronco para baixo" ~:
      Jogo (Jogador (1, 2)) comRioDepois ~=? animaJogo (Jogo (Jogador (1, 1)) comRio) (Move Baixo)
  , "adenda: no tronco para a direita" ~:
      Jogo (Jogador (3, 1)) comRioDepois ~=? animaJogo (Jogo (Jogador (1, 1)) comRio) (Move Direita)
  , "no tronco para a esquerda" ~:
      Jogo (Jogador (1, 1)) comRioDepois ~=? animaJogo (Jogo (Jogador (1, 1)) comRio) (Move Esquerda)
  , "saltar para o rio nao arrasta o jogador" ~:
      Jogo (Jogador (1, 1)) comRioDepois ~=? animaJogo (Jogo (Jogador (1, 2)) comRio) (Move Cima)
  , "na agua nao e arrastado" ~:
      Jogador (0, 1) ~=? jogadorDe (animaJogo (Jogo (Jogador (0, 1)) comRio) Parado)

    -- atropelamentos
  , "adenda: carro para ao atropelar" ~:
      Jogo (Jogador (1, 0)) (Mapa 3 [(Estrada 2, [n, c, n])])
      ~=? animaJogo (Jogo (Jogador (1, 0)) (Mapa 3 [(Estrada 2, [c, n, n])])) Parado
  , "adenda: saltar para debaixo do carro" ~:
      Jogo (Jogador (0, 0)) (Mapa 3 [(Estrada 2, [c, n, n])])
      ~=? animaJogo (Jogo (Jogador (1, 0)) (Mapa 3 [(Estrada 2, [c, n, n])])) (Move Esquerda)
  , "adenda: toda a linha para" ~:
      Jogo (Jogador (3, 0)) (Mapa 6 [(Estrada 3, [n, c, n, c, n, n])])
      ~=? animaJogo (Jogo (Jogador (3, 0)) (Mapa 6 [(Estrada 3, [c, n, c, n, n, n])])) Parado
  , "carro para a esquerda atropela" ~:
      Jogo (Jogador (1, 0)) (Mapa 4 [(Estrada (-3), [n, c, n, n])])
      ~=? animaJogo (Jogo (Jogador (1, 0)) (Mapa 4 [(Estrada (-3), [n, n, c, n])])) Parado
  , "carro que nao chega ao jogador nao para" ~:
      Jogo (Jogador (3, 0)) (Mapa 4 [(Estrada 2, [n, n, c, n])])
      ~=? animaJogo (Jogo (Jogador (3, 0)) (Mapa 4 [(Estrada 2, [c, n, n, n])])) Parado
  , "so para a estrada do jogador" ~:
      Mapa 3 [(Estrada 2, [n, c, n]), (Estrada 2, [n, n, c])]
      ~=? mapaDe (animaJogo (Jogo (Jogador (1, 0)) (Mapa 3 [(Estrada 2, [c, n, n]), (Estrada 2, [c, n, n])])) Parado)
  , "moveCarros" ~: [n, c, n, c, n, n] ~=? moveCarros 3 3 [c, n, c, n, n, n]

    -- auxiliares
  , "celula dentro do mapa" ~: Just (Relva, a) ~=? celula relvado (2, 0)
  , "celula fora do mapa" ~: [Nothing, Nothing, Nothing, Nothing]
      ~=? map (celula relvado) [(-1, 0), (3, 0), (0, -1), (0, 3)]
  , "posicaoLivre" ~: [True, False, False] ~=? map (posicaoLivre relvado) [(1, 1), (2, 0), (5, 5)]
  , "as dimensoes do mapa nao mudam" ~:
      (3, [3, 3, 3]) ~=? let Mapa l ls = mapaDe (animaJogo (Jogo (Jogador (1, 1)) comRio) (Move Direita)) in (l, map (length . snd) ls)
  ]
