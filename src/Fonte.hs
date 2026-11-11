{- |
Module      : Fonte
Description : Texto desenhado com um tipo de letra de píxeis
Copyright   : Pedro

O texto vetorial do Gloss não permite saber a largura de uma frase, o que
impede de a centrar. Este módulo define um pequeno tipo de letra de píxeis
(5 por 7), em que cada carácter é desenhado com retângulos e todas as letras
têm a mesma largura. Suporta letras maiúsculas, algarismos e alguma pontuação.
-}
module Fonte (
  -- * Desenhar texto
  textoEsquerda, textoCentrado, textoDireita, comSombra,
  -- * Medidas
  larguraTexto, alturaTexto,
  -- * Caracteres
  glifo
  ) where

import Data.Char (toUpper)
import Data.List (group)
import Graphics.Gloss

{- | Desenha um texto que começa na origem e se estende para a direita. O
primeiro argumento é o tamanho de cada píxel do tipo de letra.
-}
textoEsquerda :: Float -> String -> Picture
textoEsquerda px frase = Pictures
  [ Translate (px * 6 * fromIntegral i) 0 (desenhaGlifo px (glifo c))
  | (i, c) <- zip [0 :: Int ..] frase, c /= ' ' ]

-- | Desenha um texto centrado, na horizontal e na vertical, na origem.
textoCentrado :: Float -> String -> Picture
textoCentrado px frase =
  Translate (negate (larguraTexto px frase) / 2) (negate (alturaTexto px) / 2) (textoEsquerda px frase)

-- | Desenha um texto que termina na origem.
textoDireita :: Float -> String -> Picture
textoDireita px frase = Translate (negate (larguraTexto px frase)) 0 (textoEsquerda px frase)

{- | Pinta uma imagem com a cor dada, por cima de uma cópia escura ligeiramente
deslocada que lhe serve de sombra. O primeiro argumento é o deslocamento.
-}
comSombra :: Float -> Color -> Picture -> Picture
comSombra d cor imagem = Pictures
  [ Translate d (negate d) (Color (makeColor 0 0 0 0.6) imagem)
  , Color cor imagem ]

-- | Largura de um texto, para um dado tamanho de píxel.
larguraTexto :: Float -> String -> Float
larguraTexto px frase = px * fromIntegral (max 0 (6 * length frase - 1))

-- | Altura de um texto, para um dado tamanho de píxel.
alturaTexto :: Float -> Float
alturaTexto px = 7 * px

{- | Desenha um carácter. Em cada linha, os píxeis seguidos são fundidos num
único retângulo, para reduzir o número de polígonos.
-}
desenhaGlifo :: Float -> [String] -> Picture
desenhaGlifo px linhas = Pictures
  [ Polygon [(x0, y0), (x1, y0), (x1, y1), (x0, y1)]
  | (l, linha) <- zip [0 :: Int ..] linhas
  , (inicio, comprimento) <- segmentos linha
  , let x0 = px * fromIntegral inicio
        x1 = px * fromIntegral (inicio + comprimento)
        y1 = px * fromIntegral (7 - l)
        y0 = y1 - px
  ]

-- | Posição inicial e comprimento de cada sequência de píxeis acesos de uma linha.
segmentos :: String -> [(Int, Int)]
segmentos linha =
  [ (inicio, length grupo)
  | (inicio, grupo@('#' : _)) <- zip (scanl (+) 0 (map length grupos)) grupos ]
  where grupos = group linha

{- | O desenho de um carácter: 7 linhas de 5 colunas, em que @#@ representa um
píxel aceso. As minúsculas são desenhadas como maiúsculas e os caracteres
desconhecidos ficam em branco.
-}
glifo :: Char -> [String]
glifo c = case toUpper c of
  'A' -> [".###.", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"]
  'B' -> ["####.", "#...#", "#...#", "####.", "#...#", "#...#", "####."]
  'C' -> [".###.", "#...#", "#....", "#....", "#....", "#...#", ".###."]
  'D' -> ["####.", "#...#", "#...#", "#...#", "#...#", "#...#", "####."]
  'E' -> ["#####", "#....", "#....", "####.", "#....", "#....", "#####"]
  'F' -> ["#####", "#....", "#....", "####.", "#....", "#....", "#...."]
  'G' -> [".###.", "#...#", "#....", "#.###", "#...#", "#...#", ".###."]
  'H' -> ["#...#", "#...#", "#...#", "#####", "#...#", "#...#", "#...#"]
  'I' -> [".###.", "..#..", "..#..", "..#..", "..#..", "..#..", ".###."]
  'J' -> ["..###", "...#.", "...#.", "...#.", "...#.", "#..#.", ".##.."]
  'K' -> ["#...#", "#..#.", "#.#..", "##...", "#.#..", "#..#.", "#...#"]
  'L' -> ["#....", "#....", "#....", "#....", "#....", "#....", "#####"]
  'M' -> ["#...#", "##.##", "#.#.#", "#.#.#", "#...#", "#...#", "#...#"]
  'N' -> ["#...#", "##..#", "#.#.#", "#..##", "#...#", "#...#", "#...#"]
  'O' -> [".###.", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."]
  'P' -> ["####.", "#...#", "#...#", "####.", "#....", "#....", "#...."]
  'Q' -> [".###.", "#...#", "#...#", "#...#", "#.#.#", "#..#.", ".##.#"]
  'R' -> ["####.", "#...#", "#...#", "####.", "#.#..", "#..#.", "#...#"]
  'S' -> [".####", "#....", "#....", ".###.", "....#", "....#", "####."]
  'T' -> ["#####", "..#..", "..#..", "..#..", "..#..", "..#..", "..#.."]
  'U' -> ["#...#", "#...#", "#...#", "#...#", "#...#", "#...#", ".###."]
  'V' -> ["#...#", "#...#", "#...#", "#...#", "#...#", ".#.#.", "..#.."]
  'W' -> ["#...#", "#...#", "#...#", "#.#.#", "#.#.#", "##.##", "#...#"]
  'X' -> ["#...#", "#...#", ".#.#.", "..#..", ".#.#.", "#...#", "#...#"]
  'Y' -> ["#...#", "#...#", ".#.#.", "..#..", "..#..", "..#..", "..#.."]
  'Z' -> ["#####", "....#", "...#.", "..#..", ".#...", "#....", "#####"]
  '0' -> [".###.", "#...#", "#..##", "#.#.#", "##..#", "#...#", ".###."]
  '1' -> ["..#..", ".##..", "..#..", "..#..", "..#..", "..#..", ".###."]
  '2' -> [".###.", "#...#", "....#", "...#.", "..#..", ".#...", "#####"]
  '3' -> ["####.", "....#", "....#", ".###.", "....#", "....#", "####."]
  '4' -> ["...#.", "..##.", ".#.#.", "#..#.", "#####", "...#.", "...#."]
  '5' -> ["#####", "#....", "####.", "....#", "....#", "#...#", ".###."]
  '6' -> [".###.", "#....", "#....", "####.", "#...#", "#...#", ".###."]
  '7' -> ["#####", "....#", "...#.", "..#..", "..#..", "..#..", "..#.."]
  '8' -> [".###.", "#...#", "#...#", ".###.", "#...#", "#...#", ".###."]
  '9' -> [".###.", "#...#", "#...#", ".####", "....#", "....#", ".###."]
  '!' -> ["..#..", "..#..", "..#..", "..#..", "..#..", ".....", "..#.."]
  '-' -> [".....", ".....", ".....", ".###.", ".....", ".....", "....."]
  '.' -> [".....", ".....", ".....", ".....", ".....", ".##..", ".##.."]
  ':' -> [".....", ".##..", ".##..", ".....", ".##..", ".##..", "....."]
  '>' -> ["#....", ".#...", "..#..", "...#.", "..#..", ".#...", "#...."]
  '<' -> ["....#", "...#.", "..#..", ".#...", "..#..", "...#.", "....#"]
  _   -> replicate 7 "....."
