# Travessia 🦔
Jogo recriado anos mais tarde baseado no projeto de **Laboratórios de Informática I** (LI1), escrito em Haskell com a biblioteca [Gloss](https://hackage.haskell.org/package/gloss).

Controla-se um ouriço ao longo de um mapa infinito de relva, estradas e rios, tentando chegar o mais longe possível. Os enunciados estão em [Fase1.pdf](Fase1.pdf) e [Fase2.pdf](Fase2.pdf).

## Como jogar

É preciso ter o GHC e o Cabal instalados (testado com GHC 9.10.3 e Cabal 3.12.1.0). Na pasta do projeto:

```
cabal run
```

ou, em alternativa:

```
cabal run crossyroad
```

### Teclas

| Tecla | Ação |
|---|---|
| Setas ou `W` `A` `S` `D` | Mover o ouriço / escolher opções nos menus |
| `Enter` | Confirmar nos menus / pausa durante o jogo |
| `P` | Pausa / retomar |
| `M` | Voltar ao menu inicial, depois de perder |
| `Esc` | Fechar o jogo |

### Regras

- Na **relva**, as árvores bloqueiam o caminho.
- Nas **estradas**, ser apanhado por um carro ou camião é o fim.
- Nos **rios**, só se pode estar em cima dos troncos, que arrastam o ouriço. Se o tronco sair do mapa, o ouriço vai com ele.
- O mapa começa a deslizar com o primeiro movimento. Quem ficar para trás, perde.
- Cada linha nova que se avança vale um ponto. Com os pontos, os obstáculos e o deslize ficam mais rápidos.

No menu de pausa pode gravar-se o jogo, que fica disponível na opção *Continuar* do menu inicial. O recorde fica guardado entre sessões. O jogo cria os ficheiros `travessia.sav` e `travessia.rec` na pasta de onde é executado.

## Organização

```
├── app/Main.hs                 Arranque: janela, ficheiros de gravação e recorde
├── src/
│   ├── LI12223.hs              Tipos de dados fornecidos pelos docentes
│   ├── Tarefa1_2022li1g030.hs  Validação de um mapa
│   ├── Tarefa2_2022li1g030.hs  Geração contínua do mapa
│   ├── Tarefa3_2022li1g030.hs  Movimento do jogador e dos obstáculos
│   ├── Tarefa4_2022li1g030.hs  Determinar se o jogo terminou
│   ├── Tarefa5_2022li1g030.hs  Deslize do mapa
│   ├── Tarefa6_2022li1g030.hs  Aplicação gráfica: estado, teclado e tempo
│   ├── Desenho.hs              Aplicação gráfica: desenho do estado
│   └── Fonte.hs                Tipo de letra de píxeis usado nos textos
├── tests/                      Testes unitários (HUnit)
└── doc/html/                   Documentação gerada pelo Haddock
```

O jogo é todo desenhado com formas geométricas do Gloss, sem ficheiros de imagem, e adapta-se ao tamanho da janela.

A lógica das Tarefas 1 a 5 é discreta, como pede o enunciado: os obstáculos saltam de célula em célula. A Tarefa 6 chama essas funções uma célula de cada vez e desenha os obstáculos nas posições intermédias, para o movimento ser contínuo no ecrã. A explicação completa está na documentação do módulo `Tarefa6_2022li1g030`.

## Testes

```
cabal test
```

Há testes para todas as tarefas, incluindo os exemplos dos enunciados e jogos simulados que confirmam que o mapa se mantém sempre válido.

## Documentação

A documentação está em [doc/html/index.html](doc/html/index.html). Para a gerar de novo:

```
cabal haddock
```