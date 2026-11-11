module Tarefa1_2022li1g030_Spec (testsT1) where

import Test.HUnit

import LI12223
import Tarefa1_2022li1g030

-- | Abreviaturas para tornar os mapas de teste mais legíveis.
n, t, c, a :: Obstaculo
n = Nenhum
t = Tronco
c = Carro
a = Arvore

valido, invalido :: String -> Mapa -> Test
valido nome mapa = nome ~: True ~=? mapaValido mapa
invalido nome mapa = nome ~: False ~=? mapaValido mapa

testsT1 :: Test
testsT1 = TestLabel "Tarefa 1" $ test
  [ valido "mapa vazio" (Mapa 5 [])
  , valido "mapa do enunciado"
      (Mapa 5 [(Relva, [a, n, a, n, a]), (Estrada (-1), [n, n, n, c, c]), (Relva, [a, n, n, a, a])])

  -- 1. obstáculos em terrenos impróprios
  , invalido "tronco na estrada" (Mapa 3 [(Estrada 1, [t, n, n])])
  , invalido "tronco na relva" (Mapa 3 [(Relva, [t, n, n])])
  , invalido "carro no rio" (Mapa 3 [(Rio 1, [c, n, n])])
  , invalido "carro na relva" (Mapa 3 [(Relva, [n, c, n])])
  , invalido "arvore no rio" (Mapa 3 [(Rio 1, [n, n, a])])
  , invalido "arvore na estrada" (Mapa 3 [(Estrada 1, [n, n, a])])
  , "obstaculoProprio aceita Nenhum em todo o lado" ~:
      True ~=? all (`obstaculoProprio` n) [Rio 1, Estrada 1, Relva]

  -- 2. rios contíguos com direções opostas
  , valido "rios opostos" (Mapa 2 [(Rio 1, [t, n]), (Rio (-2), [n, t]), (Rio 3, [t, n])])
  , invalido "rios no mesmo sentido (direita)" (Mapa 2 [(Rio 1, [t, n]), (Rio 2, [n, t])])
  , invalido "rios no mesmo sentido (esquerda)" (Mapa 2 [(Rio (-1), [t, n]), (Rio (-1), [n, t])])
  , invalido "terceiro rio repete o sentido do segundo"
      (Mapa 2 [(Rio 1, [t, n]), (Rio (-1), [n, t]), (Rio (-3), [t, n])])
  , valido "rios no mesmo sentido separados por relva"
      (Mapa 2 [(Rio 1, [t, n]), (Relva, [n, n]), (Rio 1, [n, t])])
  , valido "estradas podem ter o mesmo sentido" (Mapa 2 [(Estrada 1, [c, n]), (Estrada 1, [n, c])])

  -- 3. troncos com 5 unidades, no máximo
  , valido "tronco de 5" (Mapa 7 [(Rio 1, [t, t, t, t, t, n, n])])
  , invalido "tronco de 6" (Mapa 7 [(Rio 1, [t, t, t, t, t, t, n])])
  , valido "tronco de 5 que da a volta" (Mapa 7 [(Rio 1, [t, t, n, n, t, t, t])])
  , invalido "tronco de 6 que da a volta" (Mapa 7 [(Rio 1, [t, t, t, n, t, t, t])])

  -- 4. carros com 3 unidades, no máximo
  , valido "carro de 3" (Mapa 5 [(Estrada 1, [n, c, c, c, n])])
  , invalido "carro de 4" (Mapa 5 [(Estrada 1, [c, c, c, c, n])])
  , invalido "carro de 4 que da a volta" (Mapa 6 [(Estrada 1, [c, c, n, n, c, c])])
  , "maiorSequencia" ~: 3 ~=? maiorSequencia Carro [c, n, c, c, c, n, c, c]
  , "maiorSequencia sem ocorrencias" ~: 0 ~=? maiorSequencia Tronco [n, n]

  -- 5. pelo menos um espaço livre por linha
  , invalido "relva sem espaco livre" (Mapa 3 [(Relva, [a, a, a])])
  , invalido "rio sem espaco livre" (Mapa 3 [(Rio 1, [t, t, t])])
  , valido "linha so com espacos livres" (Mapa 3 [(Estrada 2, [n, n, n])])

  -- 6. comprimento das linhas igual à largura
  , invalido "linha curta" (Mapa 4 [(Relva, [n, a, n])])
  , invalido "linha comprida" (Mapa 2 [(Relva, [n, a, n])])
  , invalido "apenas uma linha errada" (Mapa 2 [(Relva, [n, a]), (Relva, [n])])

  -- 7. terrenos contíguos
  , valido "4 rios seguidos" (Mapa 2 (rios 4))
  , invalido "5 rios seguidos" (Mapa 2 (rios 5))
  , valido "5 estradas seguidas" (Mapa 2 (replicate 5 (Estrada 1, [c, n])))
  , invalido "6 estradas seguidas" (Mapa 2 (replicate 6 (Estrada 1, [c, n])))
  , valido "5 relvas seguidas" (Mapa 2 (replicate 5 (Relva, [a, n])))
  , invalido "6 relvas seguidas" (Mapa 2 (replicate 6 (Relva, [a, n])))
  , valido "8 rios com uma relva a meio" (Mapa 2 (rios 4 ++ [(Relva, [n, n])] ++ rios 4))
  ]
  where rios k = [ (Rio (if even i then 1 else -1), [t, n]) | i <- [1 .. k :: Int] ]
