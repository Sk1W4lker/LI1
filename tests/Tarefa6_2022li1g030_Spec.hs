module Tarefa6_2022li1g030_Spec (testsT6) where

import Graphics.Gloss.Interface.IO.Game (Event (..), Key (..), KeyState (..), Modifiers (..), SpecialKey (..))
import Test.HUnit

import LI12223
import Tarefa1_2022li1g030 (mapaValido)
import Tarefa3_2022li1g030 (animaJogo)
import Tarefa4_2022li1g030 (jogoTerminou)
import Tarefa6_2022li1g030

n, t, c, a :: Obstaculo
n = Nenhum
t = Tronco
c = Carro
a = Arvore

-- | Evento de uma tecla premida.
premir :: Key -> Event
premir tecla = EventKey tecla Down (Modifiers Up Up Up) (0, 0)

seta :: SpecialKey -> Event
seta = premir . SpecialKey

enter :: Event
enter = seta KeyEnter

-- | Aplica uma sequência de eventos, ignorando os efeitos.
eventos :: [Event] -> Estado -> Estado
eventos evs e = foldl (\est ev -> fst (reageEvento ev est)) e evs

inicial :: Estado
inicial = estadoInicial 2022 12 False

comGravacao :: Estado
comGravacao = estadoInicial 2022 12 True

aJogar :: Estado
aJogar = eventos [enter] inicial

-- | Um estado de jogo com um mapa e uma posição escolhidos para o teste.
cenario :: Coordenadas -> [(Terreno, [Obstaculo])] -> Estado
cenario onde linhas = aJogar { jogo = Jogo (Jogador onde) (Mapa 3 linhas) }

posicao :: Estado -> Coordenadas
posicao e = let Jogo (Jogador p) _ = jogo e in p

-- | Simula um jogo em que o jogador repete uma lista de teclas, uma a cada 0,2 segundos, até perder.
simula :: Int -> [Key] -> Estado -> [Estado]
simula _ [] e = [e]
simula passos (tecla : teclas) e
  | passos <= 0 || ecra e /= AJogar = [e]
  | otherwise = e : simula (passos - 1) teclas depois
  where
    movido = fst (reageEvento (premir tecla) e)
    depois = iterate (fst . reageTempo (1 / 60)) movido !! 12

testsT6 :: Test
testsT6 = TestLabel "Tarefa 6" $ test
  [ -- estado inicial
    "comeca no menu inicial" ~: MenuInicial Jogar ~=? ecra inicial
  , "o mapa inicial e valido e tem as dimensoes do jogo" ~:
      (True, larguraMapa, alturaMapa) ~=?
        (let Jogo _ m@(Mapa l ls) = jogo inicial in (mapaValido m, l, length ls))
  , "o jogador comeca vivo" ~: False ~=? jogoTerminou (jogo inicial)
  , "mapas iniciais validos para muitas sementes" ~:
      True ~=? and [ mapaValido (fst (mapaInicial s)) | s <- [0 .. 200] ]
  , "sementes diferentes dao mapas diferentes" ~:
      True ~=? (fst (mapaInicial 1) /= fst (mapaInicial 2))

    -- menus
  , "sem gravacao nao ha opcao continuar" ~: [Jogar, Sair] ~=? opcoesIniciais inicial
  , "com gravacao ha opcao continuar" ~: [Jogar, Continuar, Sair] ~=? opcoesIniciais comGravacao
  , "menu inicial: baixo" ~: MenuInicial Sair ~=? ecra (eventos [seta KeyDown] inicial)
  , "menu inicial: baixo da a volta" ~: MenuInicial Jogar ~=? ecra (eventos [seta KeyDown, seta KeyDown] inicial)
  , "menu inicial: cima da a volta" ~: MenuInicial Sair ~=? ecra (eventos [seta KeyUp] comGravacao)
  , "menu inicial: jogar" ~: AJogar ~=? ecra aJogar
  , "menu inicial: continuar pede para carregar" ~:
      Just EfCarregar ~=? snd (reageEvento enter (eventos [seta KeyDown] comGravacao))
  , "menu inicial: sair" ~: Just EfSair ~=? snd (reageEvento enter (eventos [seta KeyDown] inicial))
  , "pausa com P" ~: Pausa Retomar ~=? ecra (eventos [premir (Char 'p')] aJogar)
  , "pausa com Enter" ~: Pausa Retomar ~=? ecra (eventos [enter] aJogar)
  , "retomar com P" ~: AJogar ~=? ecra (eventos [enter, premir (Char 'p')] aJogar)
  , "retomar com Enter" ~: AJogar ~=? ecra (eventos [enter, enter] aJogar)
  , "pausa: gravar" ~: Just EfGravar ~=? snd (reageEvento enter (eventos [enter, seta KeyDown] aJogar))
  , "pausa: voltar ao menu" ~: MenuInicial Jogar ~=? ecra (eventos [enter, seta KeyUp, enter] aJogar)
  , "em pausa o tempo nao passa" ~:
      (let e = eventos [enter] aJogar in e) ~=? fst (reageTempo 0.05 (eventos [enter] aJogar))
  , "em pausa o jogador nao se move" ~:
      posicaoInicial ~=? posicao (eventos [enter, seta KeyLeft, seta KeyRight, seta KeyRight] aJogar)
  , "mudar o tamanho da janela" ~: (800, 600) ~=? janela (eventos [EventResize (800, 600)] inicial)
  , "teclas desconhecidas sao ignoradas" ~: aJogar ~=? eventos [premir (Char 'x'), seta KeyF1] aJogar
  , "largar uma tecla nao faz nada" ~:
      aJogar ~=? fst (reageEvento (EventKey (SpecialKey KeyUp) Up (Modifiers Up Up Up) (0, 0)) aJogar)

    -- movimento do jogador
  , "andar para cima da pontos" ~:
      ((fst posicaoInicial, snd posicaoInicial - 1), 1, True)
      ~=? (let e = eventos [seta KeyUp] aJogar in (posicao e, pontos e, emMarcha e))
  , "WASD tambem funciona" ~:
      posicao (eventos [seta KeyUp, seta KeyLeft] aJogar) ~=? posicao (eventos [premir (Char 'w'), premir (Char 'a')] aJogar)
  , "recuar nao tira pontos, e voltar a subir nao da pontos a dobrar" ~:
      [1, 1, 1] ~=? map pontos (scanl (flip (eventos . (: []))) (eventos [seta KeyUp] aJogar) [seta KeyDown, seta KeyUp])
  , "andar para o lado nao da pontos" ~: 0 ~=? pontos (eventos [seta KeyLeft, seta KeyRight] aJogar)
  , "as arvores travam o jogador" ~:
      (1, 1) ~=? posicao (fst (moveJogador Cima (cenario (1, 1) [(Relva, [n, a, n]), (Relva, [n, n, n])])))
  , "o jogador nao sai do mapa em cima de um tronco" ~:
      (2, 0) ~=? posicao (fst (moveJogador Direita (cenario (2, 0) [(Rio 1, [n, t, t])])))
  , "saltar para a agua perde" ~:
      (Perdeu Afogado, Just EfRecorde)
      ~=? (let (e, ef) = moveJogador Cima (cenario (0, 1) [(Rio 1, [n, t, t]), (Relva, [n, n, n])]) in (ecra e, ef))
  , "saltar para um carro perde" ~:
      Perdeu Atropelado ~=? ecra (fst (moveJogador Cima (cenario (0, 1) [(Estrada 1, [c, n, n]), (Relva, [n, n, n])])))
  , "mover o jogador nao mexe no mapa" ~:
      jogoMapa aJogar ~=? jogoMapa (eventos [seta KeyUp] aJogar)

    -- tempo
  , "uma unidade de fase equivale a animaJogo" ~:
      animaJogo exemplo Parado ~=? avancaObstaculos 0 1 exemplo
  , "a fase pode ser percorrida aos poucos" ~:
      animaJogo exemplo Parado ~=? foldl (\j (f0, f1) -> avancaObstaculos f0 f1 j) exemplo (zip fases (drop 1 fases))
  , "as velocidades do mapa sao repostas" ~:
      [Rio 2, Estrada (-3), Relva] ~=? (let Jogo _ (Mapa _ ls) = avancaObstaculos 0.3 0.4 exemplo in map fst ls)
  , "passosLinha" ~: [2, 0, 1, 1, 3] ~=?
      [passosLinha 2 0 1, passosLinha 1 0.2 0.4, passosLinha (-1) 0.4 0.6, passosLinha 2 0.2 0.3, passosLinha (-3) 0 1]
  , "desvioLinha fica entre -0.5 e 0.5" ~:
      True ~=? and [ abs (desvioLinha terreno f) <= 0.5 | terreno <- [Rio 1, Rio (-2), Estrada 3, Relva], f <- [0, 0.01 .. 0.99] ]
  , "o tronco leva o jogador para fora do mapa" ~:
      Perdeu LevadoPeloTronco ~=? ecra (iterate (fst . reageTempo 0.05) (cenario (2, 0) [(Rio 1, [n, t, t])]) !! 40)
  , "um carro atropela o jogador parado" ~:
      Perdeu Atropelado ~=? ecra (iterate (fst . reageTempo 0.05) (cenario (2, 0) [(Estrada 1, [c, n, n])]) !! 60)
  , "antes do primeiro movimento o mapa nao desliza" ~:
      (0, AJogar) ~=? (let e = iterate (fst . reageTempo 0.05) aJogar !! 400 in (deslize e, ecra e))
  , "quem fica parado depois de comecar acaba por perder" ~:
      Perdeu FicouParaTras ~=? ecra (iterate (fst . reageTempo 0.05) (eventos [seta KeyLeft] aJogar) !! 1200)
  , "perder atualiza o recorde" ~:
      (20, True, Just EfRecorde) ~=?
        (let (e, ef) = passoJogo 0.01 (cenario (1, 0) [(Rio 1, [n, n, t])]) { pontos = 20 } in (recorde e, novoRecorde e, ef))
  , "depois de perder, Enter comeca um jogo novo" ~:
      (AJogar, 0, 20) ~=?
        (let e = eventos [enter] (fst (passoJogo 0.01 (cenario (1, 0) [(Rio 1, [n, n, t])]) { pontos = 20 })) in (ecra e, pontos e, recorde e))
  , "depois de perder, M volta ao menu" ~:
      MenuInicial Jogar ~=? ecra (eventos [premir (Char 'm')] (fst (passoJogo 0.01 (cenario (1, 0) [(Rio 1, [n, n, t])]))))
  , "causaDerrota" ~: [FicouParaTras, LevadoPeloTronco, Atropelado, Afogado] ~=?
      [ causaDerrota (Jogo (Jogador p) (Mapa 2 [(Estrada 1, [c, n]), (Rio 1, [n, t])])) | p <- [(0, 2), (2, 1), (0, 0), (0, 1)] ]
  , "a dificuldade aumenta com os pontos" ~: True ~=? duracaoPasso 100 < duracaoPasso 0

    -- jogos simulados: o estado mantém-se sempre coerente
  , "jogos simulados mantem o mapa valido e a pontuacao coerente" ~:
      True ~=? and
        [ mapaValido m && l == larguraMapa && length ls == alturaMapa
          && pontos e >= linha e && deslize e >= 0 && deslize e < 1 && fase e >= 0 && fase e < 1
        | s <- [1 .. 12]
        , e <- simula 400 (cycle (padrao s)) (eventos [enter] (estadoInicial s 0 False))
        , let Jogo _ m@(Mapa l ls) = jogo e ]
  , "os jogos simulados chegam a avancar no mapa" ~:
      True ~=? any (> 3) [ pontos (last (simula 400 (cycle (padrao s)) (eventos [enter] (estadoInicial s 0 False)))) | s <- [1 .. 12] ]

    -- gravação
  , "gravar e carregar devolve o mesmo jogo, em pausa" ~:
      Just (Pausa Retomar, jogo meioJogo, pontos meioJogo, semente meioJogo)
      ~=? fmap (\e -> (ecra e, jogo e, pontos e, semente e)) (deGravacao (paraGravacao meioJogo) inicial)
  , "a gravacao sobrevive a show e read" ~:
      paraGravacao meioJogo ~=? read (show (paraGravacao meioJogo))
  , "gravacao com mapa invalido e rejeitada" ~:
      Nothing ~=? deGravacao (paraGravacao meioJogo) { gJogo = Jogo (Jogador (0, 0)) (Mapa 16 [(Relva, [a])]) } inicial
  , "gravacao com o jogador morto e rejeitada" ~:
      False ~=? gravacaoValida (paraGravacao meioJogo) { gJogo = Jogo (Jogador (99, 0)) (jogoMapa meioJogo) }
  , "gravacao com valores impossiveis e rejeitada" ~:
      False ~=? gravacaoValida (paraGravacao meioJogo) { gDeslize = 7 }

    -- auxiliares
  , "vizinha" ~: "bca" ~=? map (vizinha 1 "abc") "abc"
  , "comando" ~: [Just (Direcional Cima), Just Confirma, Just TeclaPausa, Just TeclaMenu, Nothing]
      ~=? map comando [Char 'W', SpecialKey KeyEnter, Char 'P', Char 'm', SpecialKey KeySpace]
  ]
  where
    jogoMapa e = let Jogo _ m = jogo e in m
    exemplo = Jogo (Jogador (1, 2)) (Mapa 4 [(Rio 2, [t, t, n, n]), (Estrada (-3), [c, n, n, c]), (Relva, [a, n, n, n])])
    fases = [0, 0.07 .. 0.98] ++ [1]
    meioJogo = eventos [seta KeyUp, seta KeyLeft] aJogar
    padrao :: Int -> [Key]
    padrao s = map SpecialKey (concat (replicate (1 + s `mod` 3) [KeyUp]) ++ [[KeyLeft, KeyRight, KeyUp, KeyDown] !! (s `mod` 4)])
