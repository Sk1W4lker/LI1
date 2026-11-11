{- |
Module      : Tarefa1_2022li1g030
Description : Validação de um mapa
Copyright   : Pedro

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.

Um 'Mapa' é válido quando respeita, em simultâneo, as sete restrições do
enunciado. Cada restrição é verificada por uma função própria, e a função
'mapaValido' limita-se a exigir que todas se verifiquem.
-}
module Tarefa1_2022li1g030 (
  -- * Função principal
  mapaValido,
  -- * Restrições
  obstaculosProprios, riosOpostos, comprimentosValidos, temEspacoLivre,
  larguraCorreta, terrenosContiguos,
  -- * Funções auxiliares
  obstaculoProprio, maiorSequencia, mesmoTipo, limiteContiguo, limiteComprimento
  ) where

import Data.List (group, groupBy)
import LI12223

{- | Verifica se um mapa é válido, isto é, se não viola nenhuma das restrições:

1. não existem obstáculos em terrenos impróprios ('obstaculosProprios');
2. rios contíguos têm direções opostas ('riosOpostos');
3. troncos têm, no máximo, 5 unidades de comprimento ('comprimentosValidos');
4. carros têm, no máximo, 3 unidades de comprimento ('comprimentosValidos');
5. em qualquer linha existe, pelo menos, um espaço livre ('temEspacoLivre');
6. o comprimento de cada linha é igual à largura do mapa ('larguraCorreta');
7. não existem mais do que 4 rios, nem 5 estradas ou relvas, seguidos
   ('terrenosContiguos').

>>> mapaValido (Mapa 3 [(Relva, [Arvore, Nenhum, Arvore]), (Estrada 1, [Carro, Nenhum, Nenhum])])
True
>>> mapaValido (Mapa 3 [(Relva, [Arvore, Arvore, Arvore])])
False
-}
mapaValido :: Mapa -> Bool
mapaValido mapa = all ($ mapa)
  [ obstaculosProprios
  , riosOpostos
  , comprimentosValidos
  , temEspacoLivre
  , larguraCorreta
  , terrenosContiguos
  ]

-- | Restrição 1: todos os obstáculos estão num terreno onde podem existir.
obstaculosProprios :: Mapa -> Bool
obstaculosProprios (Mapa _ linhas) =
  and [ obstaculoProprio terreno o | (terreno, obstaculos) <- linhas, o <- obstaculos ]

{- | Indica se um obstáculo pode existir num dado terreno: troncos apenas em
rios, carros apenas em estradas e árvores apenas na relva. 'Nenhum' é
admissível em qualquer terreno.
-}
obstaculoProprio :: Terreno -> Obstaculo -> Bool
obstaculoProprio _           Nenhum = True
obstaculoProprio (Rio _)     Tronco = True
obstaculoProprio (Estrada _) Carro  = True
obstaculoProprio Relva       Arvore = True
obstaculoProprio _           _      = False

{- | Restrição 2: dois rios seguidos nunca correm no mesmo sentido.

Um rio com velocidade 0 não tem sentido definido, pelo que nunca entra em
conflito com os rios vizinhos.
-}
riosOpostos :: Mapa -> Bool
riosOpostos (Mapa _ linhas) = and (zipWith opostos terrenos (drop 1 terrenos))
  where
    terrenos = map fst linhas
    opostos (Rio v1) (Rio v2) = signum v1 * signum v2 /= 1
    opostos _        _        = True

{- | Restrições 3 e 4: nenhum tronco tem mais de 5 unidades e nenhum carro tem
mais de 3 unidades.

Como os obstáculos que saem por um lado do mapa reaparecem no lado oposto, a
linha é tratada como circular: um tronco que termina no fim da linha continua
no início da mesma. Para isso mede-se a maior sequência na linha concatenada
consigo própria.
-}
comprimentosValidos :: Mapa -> Bool
comprimentosValidos (Mapa _ linhas) = all (linhaOk . snd) linhas
  where
    linhaOk obstaculos = all (dentroDoLimite (obstaculos ++ obstaculos)) [Tronco, Carro]
    dentroDoLimite circular o = maybe True (maiorSequencia o circular <=) (limiteComprimento o)

-- | Comprimento máximo de um obstáculo, caso exista: 5 para troncos e 3 para carros.
limiteComprimento :: Obstaculo -> Maybe Int
limiteComprimento Tronco = Just 5
limiteComprimento Carro  = Just 3
limiteComprimento _      = Nothing

{- | Comprimento da maior sequência de um dado obstáculo numa lista.

>>> maiorSequencia Tronco [Tronco, Nenhum, Tronco, Tronco]
2
-}
maiorSequencia :: Obstaculo -> [Obstaculo] -> Int
maiorSequencia o obstaculos = maximum (0 : [ length g | g@(x:_) <- group obstaculos, x == o ])

-- | Restrição 5: todas as linhas têm, pelo menos, um espaço livre ('Nenhum').
temEspacoLivre :: Mapa -> Bool
temEspacoLivre (Mapa _ linhas) = all (elem Nenhum . snd) linhas

-- | Restrição 6: todas as linhas têm exatamente a largura do mapa.
larguraCorreta :: Mapa -> Bool
larguraCorreta (Mapa largura linhas) = all ((== largura) . length . snd) linhas

-- | Restrição 7: não há mais do que 4 rios, nem 5 estradas ou relvas, seguidos.
terrenosContiguos :: Mapa -> Bool
terrenosContiguos (Mapa _ linhas) = all grupoOk (groupBy mesmoTipo (map fst linhas))
  where
    grupoOk grupo@(t:_) = length grupo <= limiteContiguo t
    grupoOk []          = True

-- | Indica se dois terrenos são do mesmo tipo, ignorando a velocidade.
mesmoTipo :: Terreno -> Terreno -> Bool
mesmoTipo (Rio _)     (Rio _)     = True
mesmoTipo (Estrada _) (Estrada _) = True
mesmoTipo Relva       Relva       = True
mesmoTipo _           _           = False

-- | Número máximo de linhas seguidas de um tipo de terreno: 4 rios, 5 estradas ou 5 relvas.
limiteContiguo :: Terreno -> Int
limiteContiguo (Rio _) = 4
limiteContiguo _       = 5
