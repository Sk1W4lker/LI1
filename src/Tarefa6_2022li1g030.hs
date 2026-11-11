{- |
Module      : Tarefa6_2022li1g030
Description : Aplicação gráfica completa (lógica)
Copyright   : Pedro

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.

Este módulo contém o estado da aplicação e toda a lógica que o faz evoluir:
a reação ao teclado ('reageEvento') e a passagem do tempo ('reageTempo').
Todas as funções são puras. Sempre que é preciso interagir com o exterior
(gravar o jogo, sair, ...) devolve-se um 'Efeito', que é executado pelo
módulo principal. O desenho do estado está no módulo "Desenho".

== Como o tempo funciona

As tarefas anteriores descrevem um jogo discreto, em que os obstáculos saltam
de célula em célula. Para o jogo correr de forma fluida, o estado guarda uma
'fase' contínua. Uma linha com velocidade @v@ percorre @|v|@ células por cada
unidade de fase, uma célula de cada vez ('passosLinha'), e cada um desses
passos é feito pela função 'animaJogo' da Tarefa 3. Entre dois passos, o
desenho desloca os obstáculos uma fração de célula ('desvioLinha'), o que dá
a ilusão de movimento contínuo sem alterar as regras do jogo.

O deslize do mapa segue a mesma ideia: 'deslize' é a fração de linha já
percorrida e, sempre que chega a 1, é chamada a função 'deslizaJogo' da
Tarefa 5.
-}
module Tarefa6_2022li1g030 (
  -- * Estado
  Estado(..), Ecra(..), OpcaoInicial(..), OpcaoPausa(..), Causa(..), Efeito(..),
  estadoInicial, novoJogo, mapaInicial, posicaoInicial,
  -- * Constantes
  larguraMapa, alturaMapa, linhaAlvo, tamanhoJanela,
  -- * Eventos
  reageEvento, Comando(..), comando, opcoesIniciais, opcoesPausa, moveJogador,
  -- * Tempo
  reageTempo, passoJogo, avancaObstaculos, passosLinha, desvioLinha,
  duracaoPasso, velocidadeCamara, causaDerrota,
  -- * Posição do jogador no ecrã
  posicaoBase, posicaoVisual,
  -- * Gravação
  Gravacao(..), paraGravacao, deGravacao, gravacaoValida,
  -- * Funções auxiliares
  proximaSemente, vizinha
  ) where

import Data.List (elemIndex)
import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), SpecialKey (..))
import System.Random (mkStdGen, random)

import LI12223
import Tarefa1_2022li1g030 (mapaValido)
import Tarefa2_2022li1g030 (estendeMapa)
import Tarefa3_2022li1g030 (animaJogo, celula)
import Tarefa4_2022li1g030 (jogoTerminou)
import Tarefa5_2022li1g030 (deslizaJogo)

-- | Opções do menu inicial.
data OpcaoInicial
  = Jogar     -- ^ começar um jogo novo
  | Continuar -- ^ retomar o jogo gravado (só existe se houver gravação)
  | Sair      -- ^ fechar a aplicação
  deriving (Show, Eq)

-- | Opções do menu de pausa.
data OpcaoPausa
  = Retomar    -- ^ voltar ao jogo
  | Gravar     -- ^ gravar o jogo em curso
  | VoltarMenu -- ^ abandonar o jogo e voltar ao menu inicial
  deriving (Show, Eq)

-- | Motivo pelo qual o jogador perdeu.
data Causa
  = Atropelado       -- ^ ficou na mesma posição de um carro
  | Afogado          -- ^ caiu à água
  | LevadoPeloTronco -- ^ saiu do mapa em cima de um tronco
  | FicouParaTras    -- ^ deixou de estar visível quando o mapa deslizou
  deriving (Show, Eq)

-- | Ecrã em que a aplicação se encontra.
data Ecra
  = MenuInicial OpcaoInicial -- ^ menu inicial, com a opção selecionada
  | AJogar                   -- ^ jogo a decorrer
  | Pausa OpcaoPausa         -- ^ jogo em pausa, com a opção selecionada
  | Perdeu Causa             -- ^ jogo terminado
  deriving (Show, Eq)

-- | Ações sobre o exterior pedidas pela lógica e executadas pelo módulo principal.
data Efeito
  = EfGravar   -- ^ escrever o jogo em curso no ficheiro de gravação
  | EfCarregar -- ^ ler o ficheiro de gravação e continuar esse jogo
  | EfRecorde  -- ^ guardar o recorde atual
  | EfSair     -- ^ fechar a aplicação
  deriving (Show, Eq)

-- | Estado completo da aplicação.
data Estado = Estado
  { ecra        :: Ecra           -- ^ ecrã atual
  , jogo        :: Jogo           -- ^ jogador e mapa
  , semente     :: Int            -- ^ semente usada para gerar as próximas linhas
  , fase        :: Float          -- ^ fase do movimento dos obstáculos, em [0, 1[
  , deslize     :: Float          -- ^ fração de linha que o mapa já deslizou, em [0, 1[
  , linha       :: Int            -- ^ linhas avançadas desde o início (desce ao recuar)
  , pontos      :: Int            -- ^ pontuação: o máximo de linhas avançadas
  , recorde     :: Int            -- ^ melhor pontuação de sempre
  , novoRecorde :: Bool           -- ^ se o último jogo bateu o recorde
  , emMarcha    :: Bool           -- ^ se o jogador já se moveu (o mapa só desliza depois disso)
  , olhar       :: Direcao        -- ^ direção para onde o jogador está virado
  , salto       :: (Float, Float) -- ^ origem do salto em curso, relativa à posição atual (em células)
  , tempoSalto  :: Float          -- ^ fração do salto que falta completar, de 1 a 0
  , relogio     :: Float          -- ^ tempo de jogo decorrido, para animações
  , haGravacao  :: Bool           -- ^ se existe um jogo gravado
  , gravado     :: Bool           -- ^ se o jogo acabou de ser gravado (mensagem no menu de pausa)
  , janela      :: (Int, Int)     -- ^ dimensões atuais da janela
  } deriving (Show, Eq)

-- | Número de colunas do mapa.
larguraMapa :: Int
larguraMapa = 16

-- | Número de linhas do mapa: 9 visíveis e 1 escondida acima do ecrã, que vai entrando com o deslize.
alturaMapa :: Int
alturaMapa = 10

-- | Linha em que a câmara tenta manter o jogador: acima dela, o mapa desliza mais depressa.
linhaAlvo :: Int
linhaAlvo = 6

-- | Dimensões iniciais da janela.
tamanhoJanela :: (Int, Int)
tamanhoJanela = (1280, 720)

-- | Posição em que o jogador começa cada jogo.
posicaoInicial :: Coordenadas
posicaoInicial = (8, 7)

{- | Estado com que a aplicação arranca: o menu inicial, com um jogo novo em
fundo. Recebe uma semente, o recorde guardado e se existe um jogo gravado.
-}
estadoInicial :: Int -> Int -> Bool -> Estado
estadoInicial s rec gravacao = novoJogo (MenuInicial Jogar) Estado
  { ecra = MenuInicial Jogar
  , jogo = Jogo (Jogador posicaoInicial) (Mapa larguraMapa [])
  , semente = s
  , fase = 0, deslize = 0
  , linha = 0, pontos = 0, recorde = rec, novoRecorde = False
  , emMarcha = False, olhar = Cima
  , salto = (0, 0), tempoSalto = 0, relogio = 0
  , haGravacao = gravacao, gravado = False
  , janela = tamanhoJanela
  }

{- | Prepara um jogo novo, com um mapa acabado de gerar, e muda para o ecrã
indicado. O recorde, a janela e a informação sobre a gravação mantêm-se.
-}
novoJogo :: Ecra -> Estado -> Estado
novoJogo destino e = e
  { ecra = destino
  , jogo = Jogo (Jogador posicaoInicial) mapa
  , semente = s
  , fase = 0, deslize = 0
  , linha = 0, pontos = 0, novoRecorde = False
  , emMarcha = False, olhar = Cima
  , salto = (0, 0), tempoSalto = 0
  , gravado = False
  }
  where (mapa, s) = mapaInicial (semente e)

{- | Gera o mapa inicial a partir de uma semente, devolvendo também a semente
seguinte. As quatro linhas de baixo são sempre relva, para o jogador começar
em segurança; as restantes são geradas por 'estendeMapa'.
-}
mapaInicial :: Int -> (Mapa, Int)
mapaInicial = estende (alturaMapa - length base) (Mapa larguraMapa base)
  where
    estende :: Int -> Mapa -> Int -> (Mapa, Int)
    estende k mapa s
      | k <= 0    = (mapa, s)
      | otherwise = let (valor, s') = proximaSemente s
                    in estende (k - 1) (estendeMapa mapa valor) s'
    base = map relva
      [ [0, 4, 11, 15]
      , [0, 1, 2, 13, 14, 15]
      , [0, 1, 5, 10, 14, 15]
      , [0, 1, 2, 3, 6, 9, 12, 13, 14, 15]
      ]
    relva arvores = (Relva, [ if i `elem` arvores then Arvore else Nenhum | i <- [0 .. larguraMapa - 1] ])

{- | A partir de uma semente, produz um valor aleatório (para passar a
'estendeMapa' ou 'deslizaJogo') e a semente seguinte.
-}
proximaSemente :: Int -> (Int, Int)
proximaSemente s = (valor, s')
  where
    (valor, g) = random (mkStdGen s)
    (s', _)    = random g

------------------------------------------------------------------------------
-- Eventos

-- | Significado de uma tecla, independentemente do ecrã.
data Comando
  = Direcional Direcao -- ^ setas ou WASD
  | Confirma           -- ^ Enter
  | TeclaPausa         -- ^ P
  | TeclaMenu          -- ^ M
  deriving (Show, Eq)

-- | Traduz uma tecla num comando, se a tecla tiver algum significado no jogo.
comando :: Key -> Maybe Comando
comando tecla = case tecla of
  SpecialKey KeyUp    -> Just (Direcional Cima)
  SpecialKey KeyDown  -> Just (Direcional Baixo)
  SpecialKey KeyLeft  -> Just (Direcional Esquerda)
  SpecialKey KeyRight -> Just (Direcional Direita)
  SpecialKey KeyEnter -> Just Confirma
  Char c | c `elem` "wW" -> Just (Direcional Cima)
         | c `elem` "sS" -> Just (Direcional Baixo)
         | c `elem` "aA" -> Just (Direcional Esquerda)
         | c `elem` "dD" -> Just (Direcional Direita)
         | c `elem` "pP" -> Just TeclaPausa
         | c `elem` "mM" -> Just TeclaMenu
         | c `elem` "\r\n" -> Just Confirma
  _ -> Nothing

{- | Reage a um evento do Gloss. Só interessam as teclas premidas e as mudanças
de tamanho da janela; o resultado inclui o efeito a executar, se existir.
-}
reageEvento :: Event -> Estado -> (Estado, Maybe Efeito)
reageEvento (EventResize tamanho) e = (e { janela = tamanho }, Nothing)
reageEvento (EventKey tecla Down _ _) e = maybe (e, Nothing) (`reageComando` e) (comando tecla)
reageEvento _ e = (e, Nothing)

-- | Reage a um comando, de acordo com o ecrã atual.
reageComando :: Comando -> Estado -> (Estado, Maybe Efeito)
reageComando cmd e = case (ecra e, cmd) of
  (MenuInicial op, Direcional Cima)  -> (e { ecra = MenuInicial (vizinha (-1) (opcoesIniciais e) op) }, Nothing)
  (MenuInicial op, Direcional Baixo) -> (e { ecra = MenuInicial (vizinha 1 (opcoesIniciais e) op) }, Nothing)
  (MenuInicial Jogar, Confirma)      -> (e { ecra = AJogar }, Nothing)
  (MenuInicial Continuar, Confirma)  -> (e, Just EfCarregar)
  (MenuInicial Sair, Confirma)       -> (e, Just EfSair)

  (AJogar, Direcional d) -> moveJogador d e
  (AJogar, Confirma)     -> (e { ecra = Pausa Retomar }, Nothing)
  (AJogar, TeclaPausa)   -> (e { ecra = Pausa Retomar }, Nothing)

  (Pausa op, Direcional Cima)  -> (e { ecra = Pausa (vizinha (-1) opcoesPausa op), gravado = False }, Nothing)
  (Pausa op, Direcional Baixo) -> (e { ecra = Pausa (vizinha 1 opcoesPausa op), gravado = False }, Nothing)
  (Pausa _, TeclaPausa)        -> (e { ecra = AJogar, gravado = False }, Nothing)
  (Pausa Retomar, Confirma)    -> (e { ecra = AJogar, gravado = False }, Nothing)
  (Pausa Gravar, Confirma)     -> (e, Just EfGravar)
  (Pausa VoltarMenu, Confirma) -> (voltaAoMenu e, Just EfRecorde)

  (Perdeu _, Confirma)  -> (novoJogo AJogar e, Nothing)
  (Perdeu _, TeclaMenu) -> (novoJogo (MenuInicial Jogar) e, Nothing)

  _ -> (e, Nothing)
  where
    voltaAoMenu est = novoJogo (MenuInicial Jogar) est { recorde = max (recorde est) (pontos est) }

-- | Opções disponíveis no menu inicial: 'Continuar' só aparece se existir um jogo gravado.
opcoesIniciais :: Estado -> [OpcaoInicial]
opcoesIniciais e = [Jogar] ++ [ Continuar | haGravacao e ] ++ [Sair]

-- | Opções do menu de pausa.
opcoesPausa :: [OpcaoPausa]
opcoesPausa = [Retomar, Gravar, VoltarMenu]

{- | A opção que fica a uma dada distância da opção atual numa lista de opções,
dando a volta quando chega ao fim.

>>> vizinha 1 "abc" 'c'
'a'
-}
vizinha :: Eq a => Int -> [a] -> a -> a
vizinha delta opcoes atual = case elemIndex atual opcoes of
  Just i  -> opcoes !! ((i + delta) `mod` length opcoes)
  Nothing -> atual

{- | Move o jogador uma célula na direção dada, durante um jogo.

O movimento é calculado por 'animaJogo' sobre um mapa com os obstáculos
parados, pelo que respeita as regras da Tarefa 3 (as árvores e os limites do
mapa travam o jogador). Um movimento que levasse o jogador para fora do mapa
é ignorado. Subir uma linha nova aumenta a pontuação. Se o jogador saltar
para a água ou para cima de um carro, o jogo termina.
-}
moveJogador :: Direcao -> Estado -> (Estado, Maybe Efeito)
moveJogador d e
  | not valido || (x', y') == (x, y) = (e { olhar = d }, Nothing)
  | otherwise                        = verificaDerrota movido
  where
    Jogo (Jogador (x, y)) mapa@(Mapa largura linhas) = jogo e
    parado = Mapa largura [ (semVelocidade t, os) | (t, os) <- linhas ]
    Jogo (Jogador (x', y')) _ = animaJogo (Jogo (Jogador (x, y)) parado) (Move d)
    valido = x' >= 0 && x' < largura
    linha' = linha e + (y - y')
    semSalto = e
      { jogo = Jogo (Jogador (x', y')) mapa
      , olhar = d
      , emMarcha = True
      , linha = linha'
      , pontos = max (pontos e) linha'
      }
    (antesX, antesY) = posicaoVisual e
    (baseX, baseY)   = posicaoBase semSalto
    movido = semSalto { salto = (antesX - baseX, antesY - baseY), tempoSalto = 1 }

-- | Um terreno com velocidade nula.
semVelocidade :: Terreno -> Terreno
semVelocidade (Rio _)     = Rio 0
semVelocidade (Estrada _) = Estrada 0
semVelocidade Relva       = Relva

------------------------------------------------------------------------------
-- Tempo

{- | Faz avançar o estado um intervalo de tempo (em segundos).

Durante o jogo, movem-se os obstáculos e o mapa desliza ('passoJogo'). No menu
inicial apenas os obstáculos se movem, como pano de fundo. Na pausa e depois
de perder, o jogo fica parado.
-}
reageTempo :: Float -> Estado -> (Estado, Maybe Efeito)
reageTempo dt e = case ecra e of
  AJogar        -> passoJogo passo e
  MenuInicial _ -> (moveObstaculos passo e, Nothing)
  _             -> (e, Nothing)
  where passo = max 0 (min 0.1 dt) -- um bloqueio momentâneo não faz o jogo saltar no tempo

-- | Avança o relógio, a fase e os obstáculos, sem verificar se o jogo terminou.
moveObstaculos :: Float -> Estado -> Estado
moveObstaculos dt e = e
  { jogo       = avancaObstaculos (fase e) fase' (jogo e)
  , fase       = fase' - fromIntegral (floor fase' :: Int)
  , relogio    = relogio e + dt
  , tempoSalto = max 0 (tempoSalto e - dt / duracaoSalto)
  }
  where fase' = fase e + dt / duracaoPasso (pontos e)

{- | Um passo do jogo: move os obstáculos, faz deslizar o mapa e verifica, depois
de cada uma destas etapas, se o jogador perdeu.
-}
passoJogo :: Float -> Estado -> (Estado, Maybe Efeito)
passoJogo dt e
  | jogoTerminou (jogo movido) = verificaDerrota movido
  | otherwise                  = verificaDerrota (aplicaDeslize dt movido)
  where movido = moveObstaculos dt e

-- | Se o jogo tiver terminado, muda para o ecrã de derrota e atualiza o recorde.
verificaDerrota :: Estado -> (Estado, Maybe Efeito)
verificaDerrota e
  | jogoTerminou (jogo e) =
      ( e { ecra = Perdeu (causaDerrota (jogo e))
          , recorde = max (recorde e) (pontos e)
          , novoRecorde = pontos e > recorde e
          }
      , Just EfRecorde )
  | otherwise = (e, Nothing)

{- | Motivo pelo qual um jogo terminou. Só faz sentido para jogos em que
'jogoTerminou' é verdadeiro.
-}
causaDerrota :: Jogo -> Causa
causaDerrota (Jogo (Jogador (x, y)) mapa@(Mapa largura linhas))
  | y >= length linhas    = FicouParaTras
  | x < 0 || x >= largura = LevadoPeloTronco
  | otherwise = case celula mapa (x, y) of
      Just (Estrada _, _) -> Atropelado
      _                   -> Afogado

{- | Move os obstáculos do jogo entre duas fases, usando 'animaJogo'.

Cada linha recebe temporariamente como velocidade o número de células que tem
de percorrer neste intervalo ('passosLinha'), normalmente 0 ou 1. A função
'animaJogo' trata de mover os obstáculos, de arrastar o jogador que esteja em
cima de um tronco e de parar os carros que o atropelem. No fim, repõem-se as
velocidades originais.

Avançar uma unidade de fase equivale a uma chamada de 'animaJogo' com a
jogada 'Parado'.
-}
avancaObstaculos :: Float -> Float -> Jogo -> Jogo
avancaObstaculos fase0 fase1 (Jogo jogador (Mapa largura linhas)) =
  Jogo jogador' (Mapa largura (zipWith repoe linhas linhas'))
  where
    Jogo jogador' (Mapa _ linhas') = animaJogo (Jogo jogador (Mapa largura (map passo linhas))) Parado
    passo (terreno, obstaculos) = (comPassos terreno, obstaculos)
    comPassos (Rio v)     = Rio (signum v * passosLinha v fase0 fase1)
    comPassos (Estrada v) = Estrada (signum v * passosLinha v fase0 fase1)
    comPassos Relva       = Relva
    repoe (terreno, _) (_, obstaculos) = (terreno, obstaculos)

{- | Número de células que uma linha com a velocidade dada percorre entre duas
fases. O passo dá-se quando o obstáculo, no seu movimento contínuo, vai a meio
caminho entre duas células.

>>> passosLinha 2 0 1
2
>>> passosLinha 1 0.2 0.4
0
>>> passosLinha (-1) 0.4 0.6
1
-}
passosLinha :: Velocidade -> Float -> Float -> Int
passosLinha v fase0 fase1 = arredonda fase1 - arredonda fase0
  where
    arredonda :: Float -> Int
    arredonda f = floor (fromIntegral (abs v) * f + 0.5)

{- | Desvio (em células, entre -0.5 e 0.5) com que os obstáculos de uma linha
devem ser desenhados em relação à sua posição no mapa, para que o movimento
pareça contínuo. É positivo para a direita.
-}
desvioLinha :: Terreno -> Float -> Float
desvioLinha terreno f = case terreno of
  Rio v     -> desvio v
  Estrada v -> desvio v
  Relva     -> 0
  where
    desvio v = fromIntegral (signum v) * (percorrido v - fromIntegral (floor (percorrido v + 0.5) :: Int))
    percorrido v = fromIntegral (abs v) * f

{- | Segundos que uma linha de velocidade 1 demora a avançar uma célula. Diminui
com a pontuação, o que torna o jogo progressivamente mais rápido.
-}
duracaoPasso :: Int -> Float
duracaoPasso p = max 0.55 (0.95 - 0.004 * fromIntegral p)

-- | Duração, em segundos, da animação de um salto do jogador.
duracaoSalto :: Float
duracaoSalto = 0.11

{- | Velocidade a que o mapa desliza, em linhas por segundo.

Há uma velocidade mínima, que aumenta com a pontuação e obriga o jogador a
avançar. Quando o jogador passa acima da 'linhaAlvo', a câmara acelera para o
acompanhar, abrandando à medida que se aproxima dele.
-}
velocidadeCamara :: Estado -> Float
velocidadeCamara e = max minima (6 * emFalta)
  where
    Jogo (Jogador (_, y)) _ = jogo e
    minima  = min 0.9 (0.3 + 0.006 * fromIntegral (pontos e))
    emFalta = fromIntegral (linhaAlvo - y) - deslize e

-- | Faz deslizar o mapa durante um intervalo de tempo. O mapa só desliza depois do primeiro movimento do jogador.
aplicaDeslize :: Float -> Estado -> Estado
aplicaDeslize dt e
  | emMarcha e = completaDeslize e { deslize = deslize e + dt * velocidadeCamara e }
  | otherwise  = e

-- | Sempre que o deslize acumulado chega a uma linha inteira, chama 'deslizaJogo'.
completaDeslize :: Estado -> Estado
completaDeslize e
  | deslize e < 1 = e
  | otherwise     = completaDeslize e
      { deslize = deslize e - 1
      , jogo    = deslizaJogo valor (jogo e)
      , semente = s
      }
  where (valor, s) = proximaSemente (semente e)

------------------------------------------------------------------------------
-- Posição do jogador no ecrã

{- | Posição do jogador, em células, tendo em conta o movimento contínuo do
tronco em que esteja.
-}
posicaoBase :: Estado -> (Float, Float)
posicaoBase e = (fromIntegral x + arrasto, fromIntegral y)
  where
    Jogo (Jogador (x, y)) mapa = jogo e
    arrasto = case celula mapa (x, y) of
      Just (terreno@(Rio _), Tronco) -> desvioLinha terreno (fase e)
      _                              -> 0

-- | Posição em que o jogador deve ser desenhado, em células, incluindo a animação do salto.
posicaoVisual :: Estado -> (Float, Float)
posicaoVisual e = (baseX + saltoX * tempoSalto e, baseY + saltoY * tempoSalto e)
  where
    (baseX, baseY)   = posicaoBase e
    (saltoX, saltoY) = salto e

------------------------------------------------------------------------------
-- Gravação

-- | A parte do estado que é guardada no ficheiro de gravação.
data Gravacao = Gravacao
  { gJogo     :: Jogo
  , gSemente  :: Int
  , gFase     :: Float
  , gDeslize  :: Float
  , gLinha    :: Int
  , gPontos   :: Int
  , gEmMarcha :: Bool
  } deriving (Show, Read, Eq)

-- | Extrai de um estado a informação a gravar.
paraGravacao :: Estado -> Gravacao
paraGravacao e = Gravacao
  { gJogo = jogo e, gSemente = semente e
  , gFase = fase e, gDeslize = deslize e
  , gLinha = linha e, gPontos = pontos e
  , gEmMarcha = emMarcha e
  }

{- | Retoma um jogo gravado, que fica em pausa para o jogador se preparar.
Devolve 'Nothing' se a gravação não for válida ('gravacaoValida').
-}
deGravacao :: Gravacao -> Estado -> Maybe Estado
deGravacao g e
  | gravacaoValida g = Just e
      { ecra = Pausa Retomar
      , jogo = gJogo g, semente = gSemente g
      , fase = gFase g, deslize = gDeslize g
      , linha = gLinha g, pontos = gPontos g
      , emMarcha = gEmMarcha g
      , novoRecorde = False, olhar = Cima
      , salto = (0, 0), tempoSalto = 0, gravado = False
      }
  | otherwise = Nothing

{- | Verifica se uma gravação pode ser retomada: o mapa tem de ser válido e ter
as dimensões do jogo, o jogador tem de estar vivo e os valores numéricos têm
de estar dentro dos limites esperados. Protege a aplicação de ficheiros
corrompidos ou editados à mão.
-}
gravacaoValida :: Gravacao -> Bool
gravacaoValida g =
     mapaValido mapa
  && largura == larguraMapa
  && length linhas == alturaMapa
  && not (jogoTerminou (gJogo g))
  && fracao (gFase g) && fracao (gDeslize g)
  && gPontos g >= 0 && gLinha g <= gPontos g
  where
    Jogo _ mapa@(Mapa largura linhas) = gJogo g
    fracao f = f >= 0 && f < 1
