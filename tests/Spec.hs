module Main (main) where

import Test.HUnit

import Tarefa1_2022li1g030_Spec
import Tarefa2_2022li1g030_Spec
import Tarefa3_2022li1g030_Spec
import Tarefa4_2022li1g030_Spec
import Tarefa5_2022li1g030_Spec
import Tarefa6_2022li1g030_Spec

-- | Corre os testes de todas as tarefas.
main :: IO ()
main = runTestTTAndExit $ TestList [testsT1, testsT2, testsT3, testsT4, testsT5, testsT6]
