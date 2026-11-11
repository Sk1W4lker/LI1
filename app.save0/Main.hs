{- |
Module      : Main
Description : Ponto de entrada da aplicação
Copyright   : Pedro

Liga a lógica ("Tarefa6_2022li1g030") e o desenho ("Desenho") ao Gloss e
executa os efeitos pedidos pela lógica: gravar e carregar o jogo, guardar o
recorde e sair.
-}
module Main (main) where

import Control.Exception (IOException, evaluate, try)
import Control.Monad (void, when)
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game (Event, playIO)
import System.Directory (doesFileExist, removeFile)
import System.Exit (exitSuccess)
import System.Random (randomIO)
import Text.Read (readMaybe)

import Desenho (desenha)
import Tarefa6_2022li1g030

-- | Ficheiro onde fica o jogo gravado.
ficheiroGravacao :: FilePath
ficheiroGravacao = "travessia.sav"

-- | Ficheiro onde fica o recorde.
ficheiroRecorde :: FilePath
ficheiroRecorde = "travessia.rec"

-- | Número de atualizações do jogo por segundo.
fotogramas :: Int
fotogramas = 60

-- | Arranca o jogo: lê o recorde, verifica se há um jogo gravado e abre a janela.
main :: IO ()
main = do
  s        <- randomIO
  rec      <- maybe 0 (max 0) <$> leFicheiro ficheiroRecorde
  gravacao <- doesFileExist ficheiroGravacao
  playIO
    (InWindow "Travessia" tamanhoJanela (80, 60))
    black
    fotogramas
    (estadoInicial s rec gravacao)
    (return . desenha)
    reageEventoIO
    reageTempoIO

-- | Reage a um evento e executa o efeito resultante.
reageEventoIO :: Event -> Estado -> IO Estado
reageEventoIO evento = executa . reageEvento evento

-- | Faz passar o tempo e executa o efeito resultante.
reageTempoIO :: Float -> Estado -> IO Estado
reageTempoIO dt = executa . reageTempo dt

-- | Executa o efeito pedido pela lógica, se existir, e devolve o estado atualizado.
executa :: (Estado, Maybe Efeito) -> IO Estado
executa (e, Nothing) = return e
executa (e, Just efeito) = case efeito of
  EfSair    -> exitSuccess
  EfRecorde -> escreveFicheiro ficheiroRecorde (show (recorde e)) >> return e
  EfGravar  -> do
    ok <- escreveFicheiro ficheiroGravacao (show (paraGravacao e))
    return e { haGravacao = haGravacao e || ok, gravado = ok }
  EfCarregar -> do
    gravacao <- leFicheiro ficheiroGravacao
    -- a gravação é consumida ao ser carregada, seja ou não válida
    existe <- doesFileExist ficheiroGravacao
    when existe (void (try (removeFile ficheiroGravacao) :: IO (Either IOException ())))
    let semGravacao = e { haGravacao = False }
    return $ case gravacao >>= (`deGravacao` semGravacao) of
      Just retomado -> retomado
      Nothing       -> semGravacao { ecra = MenuInicial Jogar }

{- | Lê e interpreta o conteúdo de um ficheiro. Devolve 'Nothing' se o ficheiro
não existir, não puder ser lido ou tiver um conteúdo inesperado.
-}
leFicheiro :: Read a => FilePath -> IO (Maybe a)
leFicheiro caminho = do
  conteudo <- try (readFile caminho >>= \texto -> evaluate (length texto) >> return texto)
  return $ case conteudo :: Either IOException String of
    Right texto -> readMaybe texto
    Left _      -> Nothing

-- | Escreve um ficheiro, indicando se foi possível fazê-lo.
escreveFicheiro :: FilePath -> String -> IO Bool
escreveFicheiro caminho texto = do
  resultado <- try (writeFile caminho texto)
  return $ case resultado :: Either IOException () of
    Right () -> True
    Left _   -> False
