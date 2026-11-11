{- |
Module      : Desenho
Description : Aplicação gráfica completa (desenho)
Copyright   : Pedro

Transforma o estado da aplicação ("Tarefa6_2022li1g030") numa imagem do Gloss.

Tudo é desenhado com formas geométricas, sem ficheiros de imagem, numa tela
virtual de 1600 por 900 unidades em que cada célula do mapa mede 100. No fim,
a tela é ampliada ou reduzida para caber na janela, qualquer que seja o seu
tamanho.
-}
module Desenho (
  -- * Função principal
  desenha,
  -- * Mundo
  desenhaMundo, desenhaTerreno, desenhaObstaculos, sequencias,
  -- * Figuras
  ourico, arvore, tronco, veiculo,
  -- * Menus e mensagens
  desenhaEcra
  ) where

import Data.Function (on)
import Data.List (groupBy)
import Graphics.Gloss

import Fonte
import LI12223
import Tarefa6_2022li1g030

-- | Lado de uma célula do mapa, em unidades da tela.
lado :: Float
lado = 100

-- | Largura da tela virtual.
larguraTela :: Float
larguraTela = lado * fromIntegral larguraMapa

-- | Altura da tela virtual: todas as linhas do mapa menos a que está escondida acima do ecrã.
alturaTela :: Float
alturaTela = lado * fromIntegral (alturaMapa - 1)

-- | Abcissa, na tela, do limite esquerdo de uma coluna (que pode ser fracionária).
xColuna :: Float -> Float
xColuna coluna = lado * coluna - larguraTela / 2

-- | Ordenada, na tela, do centro de uma linha (que pode ser fracionária), dado o deslize do mapa.
yLinha :: Float -> Float -> Float
yLinha desl l = alturaTela / 2 + lado / 2 - lado * (l + desl)

-- | Retângulo com centro, largura e altura dados.
retangulo :: Float -> Float -> Float -> Float -> Picture
retangulo x y largura altura = Translate x y (rectangleSolid largura altura)

-- | Círculo com centro e raio dados.
circulo :: Float -> Float -> Float -> Picture
circulo x y raio = Translate x y (circleSolid raio)

{- | Desenha o estado completo: o mundo, o ecrã atual por cima e, à volta, umas
faixas pretas que escondem o que fica fora da tela quando a janela não tem a
mesma proporção.
-}
desenha :: Estado -> Picture
desenha e = Scale escala escala (Pictures [desenhaMundo e, desenhaEcra e, margens])
  where
    (largura, altura) = janela e
    escala = max 0.05 (min (fromIntegral largura / larguraTela) (fromIntegral altura / alturaTela))
    grande = 20000
    margens = Color black $ Pictures
      [ retangulo (negate (larguraTela + grande) / 2) 0 grande grande
      , retangulo ((larguraTela + grande) / 2) 0 grande grande
      , retangulo 0 ((alturaTela + grande) / 2) grande grande
      , retangulo 0 (negate (alturaTela + grande) / 2) grande grande
      ]

------------------------------------------------------------------------------
-- Mundo

{- | Desenha o mapa e o jogador. Os terrenos ficam por baixo, depois os
obstáculos e por fim o jogador, exceto quando foi atropelado: nesse caso fica
debaixo do carro.
-}
desenhaMundo :: Estado -> Picture
desenhaMundo e = Pictures (terrenos ++ debaixo ++ obstaculos ++ porCima)
  where
    Jogo _ (Mapa _ linhas) = jogo e
    ys = map (yLinha (deslize e) . fromIntegral) [0 :: Int ..]
    seguintes = map (Just . fst) (drop 1 linhas) ++ [Nothing]
    terrenos = zipWith3 (desenhaTerreno (relogio e)) ys linhas seguintes
    obstaculos = zipWith (desenhaObstaculos (fase e)) ys linhas
    (debaixo, porCima) = case ecra e of
      Perdeu Atropelado -> ([desenhaJogador e], [])
      _                 -> ([], [desenhaJogador e])

{- | Desenha o chão de uma linha, dados o relógio (para animar a água), a
ordenada do centro da linha, a linha e o terreno da linha imediatamente
abaixo (para pintar os traços entre duas faixas de estrada).
-}
desenhaTerreno :: Float -> Float -> (Terreno, [Obstaculo]) -> Maybe Terreno -> Picture
desenhaTerreno tempo y (terreno, obstaculos) abaixo = Translate 0 y $ case terreno of
  Relva -> Pictures
    [ Color (if even (length (filter (== Arvore) obstaculos)) then verdeRelva else verdeRelvaEscuro) faixa
    , Color (makeColor 0 0 0 0.06) (retangulo 0 (4 - lado / 2) larguraTela 8)
    ]
  Estrada _ -> Pictures (Color cinzaEstrada faixa : tracos)
  Rio v -> Pictures (Color azulRio faixa : map (ondulacao v) [0 .. 5])
  where
    faixa = rectangleSolid larguraTela lado
    tracos = case abaixo of
      Just (Estrada _) ->
        [ Color (greyN 0.85) (retangulo (xColuna (2 * k + 1)) (negate lado / 2) 56 6)
        | k <- [0 .. fromIntegral (larguraMapa `div` 2) - 1] ]
      _ -> []
    ondulacao :: Int -> Int -> Picture
    ondulacao v k = Color azulRioClaro (retangulo x yOnda 70 5)
      where
        corrente = 45 * fromIntegral v * tempo + 280 * fromIntegral k + 130 * fromIntegral (abs v)
        x = corrente - larguraTela * fromIntegral (floor (corrente / larguraTela) :: Int) - larguraTela / 2
        yOnda = if even k then 22 else -24

{- | Desenha os obstáculos de uma linha, dadas a fase do movimento e a ordenada
do centro da linha.

Cada sequência de troncos ou de carros é desenhada como um único objeto
comprido, deslocado de 'desvioLinha' para o movimento parecer contínuo. Cada
objeto é desenhado também uma largura de mapa à esquerda e à direita, para
que apareça de um lado enquanto desaparece do outro.
-}
desenhaObstaculos :: Float -> Float -> (Terreno, [Obstaculo]) -> Picture
desenhaObstaculos f y (terreno, obstaculos) = Translate 0 y $ Pictures $ case terreno of
  Relva     -> [ Translate (xColuna (fromIntegral i) + lado / 2) 0 arvore
               | (i, Arvore) <- zip [0 :: Int ..] obstaculos ]
  Rio _     -> concat [ copias inicio k (tronco k) | (inicio, k) <- sequencias obstaculos ]
  Estrada v -> concat [ copias inicio k (veiculo v k) | (inicio, k) <- sequencias obstaculos ]
  where
    desvio = desvioLinha terreno f
    n = fromIntegral (length obstaculos)
    copias :: Int -> Int -> Picture -> [Picture]
    copias inicio k imagem =
      [ Translate centro 0 imagem
      | volta <- [-1, 0, 1]
      , let meio = lado * fromIntegral k / 2
            centro = xColuna (fromIntegral inicio + desvio + volta * n) + meio
      , abs centro < larguraTela / 2 + meio
      ]

{- | Sequências de obstáculos de uma linha, cada uma com a coluna onde começa e
o seu comprimento. A linha é circular: uma sequência que chega ao fim da linha
continua no início.

>>> sequencias [Tronco, Nenhum, Tronco, Tronco]
[(2,3)]
-}
sequencias :: [Obstaculo] -> [(Int, Int)]
sequencias obstaculos
  | null obstaculos             = []
  | Nenhum `notElem` obstaculos = [(0, n)]
  | otherwise =
      [ ((primeiroLivre + i) `mod` n, length grupo)
      | grupo@((i, o) : _) <- groupBy ((==) `on` snd) (zip [0 ..] rodada)
      , o /= Nenhum ]
  where
    n = length obstaculos
    primeiroLivre = length (takeWhile (/= Nenhum) obstaculos)
    rodada = drop primeiroLivre obstaculos ++ take primeiroLivre obstaculos

------------------------------------------------------------------------------
-- Figuras

-- | Uma árvore vista de cima, centrada na origem.
arvore :: Picture
arvore = Pictures
  [ Color (makeColor 0 0 0 0.18) (circulo 5 (-6) 41)
  , Color verdeCopaEscuro (circleSolid 40)
  , Color verdeCopa (circulo (-4) 5 30)
  , Color verdeCopaClaro (circulo (-9) 10 15)
  ]

-- | Um tronco com o comprimento dado (em células), centrado na origem.
tronco :: Int -> Picture
tronco k = Pictures
  [ Color castanhoEscuro corpo
  , Color castanho (Translate 0 4 (Scale 1 0.82 corpo))
  , Color castanhoEscuro (Pictures [ retangulo 0 yVeio (comprimento - 60) 3 | yVeio <- [-8, 12] ])
  , Color castanhoClaro (circulo (comprimento / 2 - raio) 4 20)
  , Color castanho (circulo (comprimento / 2 - raio) 4 11)
  ]
  where
    comprimento = lado * fromIntegral k - 12
    raio = 30
    corpo = Pictures
      [ rectangleSolid (comprimento - 2 * raio) (2 * raio)
      , circulo (raio - comprimento / 2) 0 raio
      , circulo (comprimento / 2 - raio) 0 raio
      ]

{- | Um veículo visto de cima, centrado na origem, dadas a velocidade da estrada
e o seu comprimento (em células). Fica virado para o lado para onde se
desloca. Com uma célula é um carro; com mais, é um camião com a carga atrás.
-}
veiculo :: Velocidade -> Int -> Picture
veiculo v k = Scale (if v < 0 then -1 else 1) 1 $ Pictures $
  [ Color (greyN 0.1) (retangulo xRoda 0 26 (altura + 10)) | xRoda <- rodas ]
  ++ if k <= 1 then carro else camiao
  where
    comprimento = lado * fromIntegral k - 16
    altura = 62
    frente = comprimento / 2
    rodas = [frente - 24, 22 - frente] ++ [ 0 | k >= 3 ]
    cor = coresVeiculos !! ((2 * abs v + k + (if v < 0 then 3 else 0)) `mod` length coresVeiculos)
    farois = Color (makeColor 1 0.95 0.6 1) (Pictures [ retangulo (frente - 3) yFarol 6 12 | yFarol <- [-20, 20] ])
    vidro = makeColor 0.2 0.27 0.38 1
    carro =
      [ Color cor (rectangleSolid comprimento altura)
      , Color (light cor) (retangulo (-6) 0 44 (altura - 14))
      , Color vidro (retangulo 20 0 9 (altura - 20))
      , Color vidro (retangulo (-31) 0 6 (altura - 24))
      , farois
      ]
    camiao =
      [ Color (greyN 0.92) (retangulo (-25) 0 (comprimento - 50) altura)
      , Color (greyN 0.78) (retangulo (-25) 0 (comprimento - 66) (altura - 16))
      , Color cor (retangulo (frente - 23) 0 46 (altura - 4))
      , Color vidro (retangulo (frente - 16) 0 10 (altura - 18))
      , farois
      ]

-- | Cores possíveis para os veículos.
coresVeiculos :: [Color]
coresVeiculos =
  [ makeColor 0.86 0.22 0.2 1
  , makeColor 0.2 0.45 0.85 1
  , makeColor 0.95 0.75 0.15 1
  , makeColor 0.55 0.3 0.75 1
  , makeColor 0.95 0.5 0.15 1
  , makeColor 0.15 0.65 0.6 1
  ]

-- | O ouriço controlado pelo jogador, visto de cima e virado para cima, centrado na origem.
ourico :: Picture
ourico = Pictures
  [ Color (makeColor 0 0 0 0.2) (Translate 3 (-5) (Scale 1 1.1 (circleSolid 33)))
  , Color castanhoEscuro (Pictures (map espinho [50, 80 .. 310]))
  , Color castanhoEscuro (Scale 1 1.12 (circleSolid 30))
  , Color castanho (Translate 0 (-3) (Scale 1 1.1 (circleSolid 22)))
  , Color castanhoEscuro (Pictures [ circulo x y 3 | (x, y) <- [(-9, -12), (8, -14), (0, -2), (-11, 4), (11, 2), (0, -22)] ])
  , Color bege (circulo 0 22 16)
  , Color bege (Polygon [(-10, 31), (10, 31), (0, 45)])
  , Color (makeColor 0.85 0.7 0.55 1) (Pictures [circulo (-14) 17 5, circulo 14 17 5])
  , Color black (Pictures [circulo (-7) 27 3, circulo 7 27 3, circulo 0 43 4])
  ]
  where
    espinho :: Float -> Picture
    espinho graus = Polygon [ponto (graus - 13) 27, ponto graus 41, ponto (graus + 13) 27]
    ponto graus raio = (raio * sin (graus * pi / 180), 1.1 * raio * cos (graus * pi / 180))

{- | Desenha o jogador na sua posição visual, virado para onde se moveu pela
última vez e ligeiramente ampliado a meio de um salto. Se foi atropelado fica
espalmado; se caiu à água, restam apenas umas ondas.
-}
desenhaJogador :: Estado -> Picture
desenhaJogador e = Translate x y $ case ecra e of
  Perdeu Afogado -> Pictures
    [ Color (withAlpha 0.7 white) (ThickCircle raio 4) | raio <- [14, 28, 42] ]
  Perdeu Atropelado -> Rotate angulo (Scale 1.25 0.45 ourico)
  _ -> Rotate angulo (Scale ampliacao ampliacao ourico)
  where
    (coluna, l) = posicaoVisual e
    x = xColuna coluna + lado / 2
    y = yLinha (deslize e) l
    ampliacao = 1 + 0.22 * sin (pi * tempoSalto e)
    angulo = case olhar e of
      Cima     -> 0
      Direita  -> 90
      Baixo    -> 180
      Esquerda -> 270

------------------------------------------------------------------------------
-- Menus e mensagens

-- | Desenha, por cima do mundo, o que é próprio de cada ecrã: a pontuação durante o jogo, ou um menu.
desenhaEcra :: Estado -> Picture
desenhaEcra e = case ecra e of
  AJogar -> Pictures (pontuacao e : [ ajuda | not (emMarcha e) ])
  MenuInicial op -> Pictures
    [ veu (makeColor 0 0 0 0.55)
    , Translate 0 250 (comSombra 8 amarelo (textoCentrado 16 "TRAVESSIA"))
    , Translate 0 140 (comSombra 3 white (textoCentrado 4 "AJUDA O OURICO A CHEGAR O MAIS LONGE POSSIVEL"))
    , menu 20 [ (rotuloInicial o, o == op) | o <- opcoesIniciais e ]
    , Translate 0 (-330) (comSombra 3 white (textoCentrado 4 ("RECORDE: " ++ show (recorde e))))
    , rodape "SETAS: ESCOLHER   ENTER: CONFIRMAR   ESC: FECHAR"
    ]
  Pausa op -> Pictures
    [ pontuacao e
    , veu (makeColor 0 0 0 0.55)
    , Translate 0 250 (comSombra 8 white (textoCentrado 14 "PAUSA"))
    , menu 60 [ (rotuloPausa o, o == op) | o <- opcoesPausa ]
    , if gravado e
        then Translate 0 (-300) (comSombra 3 verdeCopaClaro (textoCentrado 5 "JOGO GRAVADO!"))
        else Blank
    , rodape "SETAS: ESCOLHER   ENTER: CONFIRMAR   P: RETOMAR"
    ]
  Perdeu causa -> Pictures
    [ veu (makeColor 0.35 0 0 0.5)
    , Translate 0 240 (comSombra 8 white (textoCentrado 11 (mensagem causa)))
    , Translate 0 70 (comSombra 5 amarelo (textoCentrado 9 ("PONTOS: " ++ show (pontos e))))
    , Translate 0 (-40) $ if novoRecorde e
        then comSombra 4 verdeCopaClaro (textoCentrado 6 "NOVO RECORDE!")
        else comSombra 4 white (textoCentrado 6 ("RECORDE: " ++ show (recorde e)))
    , Translate 0 (-220) (comSombra 4 white (textoCentrado 6 "ENTER: JOGAR DE NOVO"))
    , Translate 0 (-300) (comSombra 4 white (textoCentrado 6 "M: MENU INICIAL"))
    ]
  where
    veu cor = Color cor (rectangleSolid larguraTela alturaTela)
    rodape frase = Translate 0 (20 - alturaTela / 2 + 14) (Color (greyN 0.8) (textoCentrado 3 frase))
    ajuda = Translate 0 (-390) (comSombra 3 white (textoCentrado 4 "SETAS OU WASD: MOVER     P OU ENTER: PAUSA"))

-- | A pontuação e o recorde, nos cantos superiores da tela.
pontuacao :: Estado -> Picture
pontuacao e = Pictures
  [ Translate (30 - larguraTela / 2) (alturaTela / 2 - 80) (comSombra 5 white (textoEsquerda 8 (show (pontos e))))
  , Translate (larguraTela / 2 - 30) (alturaTela / 2 - 55)
      (comSombra 3 white (textoDireita 4 ("RECORDE: " ++ show (max (recorde e) (pontos e)))))
  ]

{- | Uma lista vertical de opções, dada a ordenada da primeira. A opção
selecionada fica destacada a amarelo, entre setas.
-}
menu :: Float -> [(String, Bool)] -> Picture
menu topo opcoes = Pictures
  [ Translate 0 (topo - 95 * fromIntegral i) $
      if selecionada
        then comSombra 5 amarelo (textoCentrado 9 ("> " ++ rotulo ++ " <"))
        else comSombra 5 white (textoCentrado 8 rotulo)
  | (i, (rotulo, selecionada)) <- zip [0 :: Int ..] opcoes ]

-- | Texto de cada opção do menu inicial.
rotuloInicial :: OpcaoInicial -> String
rotuloInicial Jogar     = "JOGAR"
rotuloInicial Continuar = "CONTINUAR"
rotuloInicial Sair      = "SAIR"

-- | Texto de cada opção do menu de pausa.
rotuloPausa :: OpcaoPausa -> String
rotuloPausa Retomar    = "RETOMAR"
rotuloPausa Gravar     = "GRAVAR"
rotuloPausa VoltarMenu = "MENU INICIAL"

-- | Mensagem mostrada ao perder, de acordo com a causa.
mensagem :: Causa -> String
mensagem Atropelado       = "FOSTE ATROPELADO!"
mensagem Afogado          = "FOSTE AO BANHO!"
mensagem LevadoPeloTronco = "O TRONCO LEVOU-TE!"
mensagem FicouParaTras    = "DEMASIADO LENTO!"

------------------------------------------------------------------------------
-- Cores

verdeRelva, verdeRelvaEscuro, verdeCopa, verdeCopaEscuro, verdeCopaClaro :: Color
verdeRelva       = makeColor 0.56 0.8 0.33 1
verdeRelvaEscuro = makeColor 0.52 0.76 0.3 1
verdeCopa        = makeColor 0.2 0.55 0.24 1
verdeCopaEscuro  = makeColor 0.13 0.42 0.2 1
verdeCopaClaro   = makeColor 0.45 0.78 0.4 1

cinzaEstrada, azulRio, azulRioClaro :: Color
cinzaEstrada = makeColor 0.3 0.31 0.36 1
azulRio      = makeColor 0.25 0.62 0.9 1
azulRioClaro = makeColor 0.5 0.78 0.96 1

castanho, castanhoEscuro, castanhoClaro, bege, amarelo :: Color
castanho       = makeColor 0.55 0.36 0.2 1
castanhoEscuro = makeColor 0.4 0.25 0.13 1
castanhoClaro  = makeColor 0.8 0.64 0.42 1
bege           = makeColor 0.95 0.84 0.68 1
amarelo        = makeColor 1 0.85 0.2 1
