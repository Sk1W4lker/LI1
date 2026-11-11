module Tarefa2_2022li1g030_Spec (testsT2) where

import System.Random (mkStdGen)
import Test.HUnit

import LI12223
import Tarefa1_2022li1g030 (mapaValido)
import Tarefa2_2022li1g030

n, t, c, a :: Obstaculo
n = Nenhum
t = Tronco
c = Carro
a = Arvore

-- | Estende um mapa várias vezes, com sementes consecutivas.
estendeVezes :: Int -> Int -> Mapa -> Mapa
estendeVezes vezes semente mapa = foldl estendeMapa mapa (take vezes [semente ..])

linhasDe :: Mapa -> [(Terreno, [Obstaculo])]
linhasDe (Mapa _ linhas) = linhas

testsT2 :: Test
testsT2 = TestLabel "Tarefa 2" $ test
  [ -- proximosTerrenosValidos
    "terrenos: mapa vazio" ~: [Rio 0, Estrada 0, Relva] ~=? proximosTerrenosValidos (Mapa 3 [])
  , "terrenos: 3 rios" ~: [Rio 0, Estrada 0, Relva]
      ~=? proximosTerrenosValidos (Mapa 2 (replicate 3 (Rio 1, [t, n])))
  , "terrenos: 4 rios" ~: [Estrada 0, Relva]
      ~=? proximosTerrenosValidos (Mapa 2 (replicate 4 (Rio 1, [t, n]) ++ [(Relva, [n, n])]))
  , "terrenos: 5 estradas" ~: [Rio 0, Relva]
      ~=? proximosTerrenosValidos (Mapa 2 (replicate 5 (Estrada 1, [c, n])))
  , "terrenos: 5 relvas" ~: [Rio 0, Estrada 0]
      ~=? proximosTerrenosValidos (Mapa 2 (replicate 5 (Relva, [a, n])))
  , "terrenos: so conta o topo" ~: [Rio 0, Estrada 0, Relva]
      ~=? proximosTerrenosValidos (Mapa 2 ((Estrada 1, [c, n]) : replicate 4 (Rio 1, [t, n])))

    -- proximosObstaculosValidos
  , "obstaculos: rio vazio" ~: [n, t] ~=? proximosObstaculosValidos 10 (Rio 1, [])
  , "obstaculos: estrada vazia" ~: [n, c] ~=? proximosObstaculosValidos 10 (Estrada 1, [])
  , "obstaculos: relva vazia" ~: [n, a] ~=? proximosObstaculosValidos 10 (Relva, [])
  , "obstaculos: linha completa" ~: [] ~=? proximosObstaculosValidos 2 (Estrada 1, [c, n])
  , "obstaculos: tronco de 4 pode crescer" ~: [n, t] ~=? proximosObstaculosValidos 10 (Rio 1, [n, t, t, t, t])
  , "obstaculos: tronco de 5 nao pode crescer" ~: [n] ~=? proximosObstaculosValidos 10 (Rio 1, [n, t, t, t, t, t])
  , "obstaculos: carro de 3 nao pode crescer" ~: [n] ~=? proximosObstaculosValidos 10 (Estrada 1, [c, c, c])
  , "obstaculos: carro de 2 pode crescer" ~: [n, c] ~=? proximosObstaculosValidos 10 (Estrada 1, [c, n, c, c])
  , "obstaculos: ultima celula sem espaco livre" ~: [n] ~=? proximosObstaculosValidos 3 (Relva, [a, a])
  , "obstaculos: ultima celula com espaco livre" ~: [n, a] ~=? proximosObstaculosValidos 3 (Relva, [a, n])
  , "obstaculos: ultima celula fecha um carro de 4" ~: [n] ~=? proximosObstaculosValidos 6 (Estrada 1, [c, c, n, n, c])
  , "obstaculos: ultima celula fecha um carro de 3" ~: [n, c] ~=? proximosObstaculosValidos 6 (Estrada 1, [c, n, n, n, c])
  , "obstaculos: ultima celula fecha um tronco de 6" ~: [n] ~=? proximosObstaculosValidos 7 (Rio 1, [t, t, t, n, t, t])

    -- estendeMapa
  , "estende: acrescenta uma linha no topo" ~:
      let mapa = Mapa 4 [(Relva, [n, a, n, n])]
          Mapa largura linhas = estendeMapa mapa 42
      in (4, 2, [(Relva, [n, a, n, n])]) ~=? (largura, length linhas, drop 1 linhas)
  , "estende: mesma semente, mesma linha" ~:
      estendeMapa (Mapa 8 []) 17 ~=? estendeMapa (Mapa 8 []) 17
  , "estende: mapas validos para todas as sementes de 0 a 100" ~:
      True ~=? and [ mapaValido (estendeMapa (Mapa 12 []) s) | s <- [0 .. 100] ]
  , "estende: mapas longos continuam validos" ~:
      True ~=? and [ mapaValido (estendeVezes 80 s (Mapa largura [])) | s <- [0, 1000, -57, 123456], largura <- [2, 3, 5, 8, 16, 21] ]
  , "estende: funciona com largura 1" ~:
      True ~=? mapaValido (estendeVezes 30 5 (Mapa 1 []))
  , "estende: os terrenos gerados tem velocidade" ~:
      True ~=? and [ v /= 0 | (terreno, _) <- linhasDe (estendeVezes 200 9 (Mapa 16 [])), v <- velocidades terreno ]
  , "estende: os rios gerados tem troncos" ~:
      True ~=? and [ t `elem` os | (Rio _, os) <- linhasDe (estendeVezes 200 3 (Mapa 16 [])) ]
  , "estende: gera os tres tipos de terreno" ~:
      (True, True, True) ~=?
        let terrenos = map fst (linhasDe (estendeVezes 200 77 (Mapa 16 [])))
        in (Relva `elem` terrenos, any eRio terrenos, any eEstrada terrenos)
  , "estende: a relva nunca fecha o caminho" ~:
      True ~=? and [ caminhoAberto (linhasDe (estendeVezes 300 s (Mapa 16 []))) | s <- [1, 2, 3] ]

    -- auxiliares
  , "abreCaminho: abre por cima do meio do espaco tapado" ~:
      [n, a, n, n] ~=? abreCaminho [a, n, n, a] [n, a, a, n]
  , "abreCaminho: nao mexe se ja houver passagem" ~:
      [a, n, a, a] ~=? abreCaminho [n, n, n, a] [a, n, a, a]
  , "segmentosLivres" ~: [[0, 1], [3]] ~=? segmentosLivres [n, n, a, n]
  , "geraLinha tem a largura pedida" ~:
      [16, 16, 16] ~=? [ length (geraLinha 16 terreno (mkStdGen 4)) | terreno <- [Rio 1, Estrada 2, Relva] ]
  ]
  where
    velocidades (Rio v)     = [v]
    velocidades (Estrada v) = [v]
    velocidades Relva       = []
    eRio (Rio _) = True
    eRio _       = False
    eEstrada (Estrada _) = True
    eEstrada _           = False

-- | Verifica que, em relvas seguidas, cada grupo de espaços livres tem saída para a linha de cima.
caminhoAberto :: [(Terreno, [Obstaculo])] -> Bool
caminhoAberto linhas = and
  [ any (\i -> cima !! i == Nenhum) grupo
  | ((Relva, cima), (Relva, baixo)) <- zip linhas (drop 1 linhas)
  , grupo <- segmentosLivres baixo ]
