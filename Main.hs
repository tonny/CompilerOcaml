module Main (main) where
import UU.Parsing
import Parser
import CondicionContexto
--import GramaticaAbstracta
import Char
import Scanner
{-
===================================================================================================
   Autor   -> Antonio Mamani Quispe
   Nombre  -> Main 
   Version -> 0.9
===================================================================================================
-}

main :: IO ()
main = do s <- readFile "prueba.ml"
          d <- readFile "incorrecto.ml"
          let tokens = scanner s
          let tok    = scanner d
          arbol <- parseIO pCuerpo (tokens)
--          putStrLn (show tokens )
          putStrLn "=============================================================="
          putStrLn "                    Compilador Ocaml \nAutor: Antonio Mamani Quispe "
          putStrLn "=============================================================="
          putStrLn "Codigo Correcto"
          putStrLn "=============================================================="
          putStrLn s
          putStrLn (show arbol)
--          writeFile "salida" (show arbol)
          putStrLn "=============================================================="
          putStrLn "Codigo Incorrecto"
          putStrLn "=============================================================="
          arbol2 <- parseIO pCuerpo (tok)
          putStrLn d
          putStrLn (show arbol2)
