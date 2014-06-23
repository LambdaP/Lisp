{-# LANGUAGE NoMonomorphismRestriction #-}
module Lisp where

import Text.ParserCombinators.Parsec
import Data.List(intercalate)

type Prim      = Int
type LispError = String

data LispElem = Primitive Prim
              | Symbol String
              | Lambda (LispElem -> Either LispError LispElem)
              | List [LispElem]

instance Show LispElem where
  show (Primitive n) = "Num " ++ show n
  show (Symbol    s) = show s
  show (List      l) = "(" ++ intercalate ", " (map show l) ++ ")"
  show (Lambda    _) = "Lambda"

read' :: String -> Either LispError LispElem
read' s = case parse lispParser "read error" s of
        Right x -> Right x
        Left _  -> Left "Parse error."

lispParser = (try parsePrimitive)
         <|> (try parseList)
         <|> (try parseLispSymbol)

parsePrimitive = do d <- many1 digit
                    n <- return (read d :: Int)
                    return $ Primitive n

parseList = between (char '(') (char ')') $ do xs <- endBy lispParser spaces
                                               return $ List xs

parseLispSymbol = do s <- (try $ many1 $ noneOf " ()\n")
                     return $ Symbol s

baseContext :: [(String, LispElem)]
baseContext = [("+"   , plus)
              ,("-"   , minus)
              ,("*"   , times)
              ,("/"   , div')
              ,("read", Lambda liftedRead)
              ]
               where [plus, minus, times, div']
                        = map (\f -> Lambda (\x
                                 -> Right $ Lambda (lambdaize f x)))
                              [(+), (-), (*), (div)]
                     liftedRead (Symbol s) = read' s
                     liftedRead _          = Left "Could not read."

lambdaize :: (Prim -> Prim -> Prim)
          -> LispElem
          -> LispElem
          -> Either [Char] LispElem
lambdaize f le = case le of
        Primitive x -> liftPrimitive (f x)
        _           -> \_ -> Left "Type error."
      where liftPrimitive f (Primitive x) = Right $ Primitive (f x)
            liftPrimitive _ _             = Left $ "Type error."

eval :: LispElem -> [(String, LispElem)] -> Either LispError LispElem

eval (List l) c = case l of
        []       -> Left  $ "Evaluating empty list."
        [x]      -> Right $ x
        (f:x:xs) -> case eval f c of
                Right f' -> let x' = eval x c
                            in case x' >>= (apply f') of
                                Right x'' -> eval (List (x'':xs)) c
                                z         -> z
                z        -> z
        where apply (Lambda f) x = f x
              apply _ _          = Left $ "Type error."
eval (Symbol s) c = case lookup s c of
        Nothing -> Left "Unknown symbol."
        Just x  -> Right x
eval x _        = Right x
