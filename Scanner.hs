module Scanner where
import UU.Parsing
import Char

{-
===================================================================================================
   Autor   -> Antonio Mamani Quispe
   Nombre  -> Scanner Ocaml
   Version -> 0.9
===================================================================================================
-}

data Token = Token Type String Int Int

data Type = Keyword
          | Infix
          | Prefix
          | Operator
          | Lident
          | Uident
          | IntegerL
          | Float
          | String 
	      | Delimiter
	      | Comment
          | Char
          | LabelName
          | Label
          | OptLabel
	      | Error 
          deriving (Eq,Ord)

instance Show Token where
  show (Token t s f c) = show t ++ show s ++ " "  ++ show f ++ " " ++ show c ++ "\n"

instance Show Type where
    	show Keyword       = "PalaResevada : "
        show Prefix        = "Prefix       : "
        show Infix         = "Infix        : "
        show Lident        = "Lidentifier  : "
        show Uident        = "Uidentifier  : "
        show String        = "Cadena       : "
        show Float         = "Float        : "
        show IntegerL      = "Integer      : "
        show Delimiter     = "Delimitador  : "
        show Comment       = "Comentario   : "
        show Char          = "Char         : "
        show Operator      = "Operador     : "
        show LabelName     = "LabelName    : "
        show Label         = "Label        : "
        show OptLabel      = "OptLabel     : "
        show Error         = "Error        : "

--metodo que resive el codigo fuente de ocaml--
scanner xs = (scan xs 1 1)
scan [] _ _ 	= []
scan (x:xs) f c | x == '\n'  = scan xs (f+1) 1
                | isSpace x  = scan xs f (c+1)
                | x == '\"'  = if (elem '\"' xs) then (Token String (imprimir xs) f c):scan (elim2 xs) f (c+1) else return (Token Error "Cadena" f c)
                | isUpper x  = (Token Uident (x:(grup xs)) f c):scan (elim (grup xs)) f ((length (grup xs))+c+1)
                | (x== '\'') = if length (acepta xs) /= 0 then (Token Char (acepta xs) f c):scan (elim2 xs) f (length ( acepta xs) + c+1)
                                                          else (Token Keyword (show x) f c):scan xs f (c+1)
                
                | key2 ([x]++[head xs])         = (Token Keyword ([x]++[head xs]) f c):scan (elim1 ((length xs)-1) xs) f (c+1)
--                | x == '~' && isLower (head xs) = (Token Label (x:(grup xs)) f c):scan (elim (grup xs)) f ((length (grup xs))+c+1)
--                | x == '?' && isLower (head xs) = (Token OptLabel(x:(grup xs)) f c):scan (elim (grup xs)) f ((length (grup xs))+c+1)

                | operador x && operador(head xs) = if pre x then (Token Prefix (x:(prefix xs)) f c ):scan (elim (prefix xs)) f (length (prefix xs) + c)
                                                          else (Token Infix (x:(prefix xs)) f c ): scan (elim (prefix xs)) f (length (prefix xs) + c)

                | isLower x || x == '_'       = if keyw (x:(grup xs))then (Token Keyword (pal xs) f c):scan (rm (pal xs)) f ((length (pal xs))+c) 
                                                                 else (Token Lident (x:(grup xs)) f c):scan (elim (grup xs)) f ((length (grup xs))+c+1)
--
                | x == '(' && head xs == '*'  = if (existe '*' ')' xs) then (Token Comment (print '*' ')' xs) f c):scan (kill '*' ')' xs) f (c+1)
                                                                       else return (Token Error "Comentario" f c)

                | x == '0' || x=='-' && (head xs) == '0'     = (Token IntegerL (integer xs) f c):scan (rm (integer xs)) f ((length (integer xs))+c)

                | isDigit x || x == '-' && isDigit (head xs) = if elem '.' (x:(float xs)) then (Token Float (x:(float xs)) f c):scan (elim (float xs))
                                f ((length (float xs))+c) else (Token IntegerL (x:(float xs)) f c):scan (rm (x:(float xs))) f ((length (float xs))+c+1) 
                
                
                | key3 x                = (Token Keyword (x:[]) f c):scan xs f (c+1)
                | operador x            = (Token Operator (x:[]) f c):scan xs f (c+1)
                | otherwise             = return (Token Error "simbolo no valido" f c)
                where 

                 -- funcion que reconoce Char --
                  acepta [] = []
                  acepta (x:xs) | isAlpha x && (head xs) == '\'' = x:(head xs):acepta []
                                | x == '\\' && char (head xs) && (!!) xs 1 == '\'' = x:(head xs):( (!!) xs 1 ):acepta []
                                | otherwise                      = acepta []

                 -- funcion auxiliar que ayuda a reconoces UIdent, Lident, Label, optLabe --
                  grup [] = []
                  grup (x:xs) | (x== '_') || isDigit x || isAlpha x || (x=='\'') = x:grup xs
                              | otherwise                                        = grup []

                  -- funcion que reconoce numeros hexadecimales, octales, binarios --
                  integer (y:z:xs) | ( y == 'x' || y=='X' ) && (isDigit z || isAlpha z) = x:y:z:hexa xs
                                   | ( y == 'o' || y=='O' ) && (elem z ['0'..'7'])      = x:y:z:octal xs
                                   | ( y == 'b' || y=='B' ) && (elem z ['0','1'])       = x:y:z:bin xs

                  -- funcion auxiliar para reconocer numeros hexadecimales --
                  hexa [] = []
                  hexa (x:xs) | isDigit x || isAlpha x || x=='_' = x:hexa xs
                              | otherwise = hexa []

                  -- funcion auxiliar para reconoces numeros octales --
                  octal [] = []
                  octal (x:xs) | elem x ['1'..'7'] || x == '_' = x:octal xs
                               | otherwise         = octal []

                  -- funcion auxiliar para reconoces numeros binarios --
                  bin [] = []
                  bin (x:xs) | elem x ['0','1'] || x == '_' = x:bin xs
                             | otherwise        = bin [] 
                  
                  -- funcion que reconoce numeros Floating-Point y Integer
                  float [] = []
                  float (x:xs) | isDigit x || x == '_' = x:float xs
                               | x == '.'  = x:(resto xs)++float [] 
                               | otherwise = float []

                  -- funcion auxiliar para reconocer floating-Point
                  resto [] = []
                  resto (x:xs) | isDigit x || x == '_' = x:resto xs
                               | otherwise = resto []                  
                  -- funcion que reconoce prefixSimbol y infixSimbol
                  prefix [] = []   
                  prefix (x:xs) | operador x = x:prefix xs
                                | otherwise  = prefix []

                  elim x = drop (length x) xs

                  rm x = drop ((length x)-1) xs

                  elim2 xs         = elim1 (length(dropWhile (/=x) xs)-1) xs

                  pal p            = x:(takeWhile isLower p)

                  imprimir xs      = takeWhile (/=x) xs
                  
                  kill x y []      = []
                  kill x y (z:xs)  = if (x==z)&&(y==head xs) then tail xs else kill x y xs                  

                  print x y []     = []
                  print x y (z:xs) = if (x==z)&&(y==head xs) then print x y [] else z:print x y xs 
 
                  existe x y []     = False
                  existe x y (z:xs) = if (x==z)&&(y==head xs)then True else existe x y xs                  

                  elim1 x []     = [] 
                  elim1 x (y:xs) = if(length xs == x)then xs else elim1 x xs    
                  operador s = elem s ['!', '$','%','&','*','+','-','.','/',':','<','=','>','?','@','^','|','~']
                  key3 s     = elem s ['#','\'','_','‘','(',')',',',';','[',']'] -- & *  + -  .  : <  =  > ? | ~
                  fix s      = elem s ['=','<','>','@','^','|','&','+','-','*','/','$','%','!','?','~']
                  pre s      = elem s ['!','?','~']
                  char s     = elem s ['\\','\"','\'','n','t','b','r']

keyw s = elem s ["and","as","assert","asr","begin","class","constraint","do","done","downto","else","end","exception","external","false","for","fun",
            "function","functor","if","in","include","inherit","initializer","land","lazy","let","lor","lsl","lsr","lxor","match","method","mod","when",
            "module","mutable","new","object","of","open","or","private","rec","sig","struct","then","to","true","try","type","val","virtual","while",
            "with"]

key2 s = elem s ["::","->","..",":=",":>",";;","<-",">]",">}","??","[<","[>","[|","{<","|]"]

--"!=","&&","-.",":=",



instance Eq Token where
    (Token Uident         _ _ _) == (Token Uident        _ _ _) = True
    (Token Lident         _ _ _) == (Token Lident        _ _ _) = True
    (Token Prefix         _ _ _) == (Token Prefix        _ _ _) = True
    (Token Infix          _ _ _) == (Token Infix         _ _ _) = True
    (Token IntegerL       _ _ _) == (Token IntegerL      _ _ _) = True
    (Token Float          _ _ _) == (Token Float         _ _ _) = True
    (Token String         _ _ _) == (Token String        _ _ _) = True
    (Token Delimiter      _ _ _) == (Token Delimiter     _ _ _) = True
--    (Token Comment        _ _ _) == (Token Comment       _ _ _) = True
    (Token Char           _ _ _) == (Token Char          _ _ _) = True
    (Token LabelName      _ _ _) == (Token LabelName     _ _ _) = True
    (Token Label          _ _ _) == (Token Label         _ _ _) = True
    (Token OptLabel       _ _ _) == (Token OptLabel      _ _ _) = True
    (Token Error          _ _ _) == (Token Error         _ _ _) = True
    (Token t1            s1 _ _) == (Token t2           s2 _ _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x == y = EQ
              | x <= y = LT
              | otherwise = GT
  (Token tok1 str1 _ _) <= (Token tok2 str2 _ _) 
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token where

tSym :: Type -> String -> Parser Token String 
tSym tok str = obtenerValor <$> pSym (Token tok str 0 0 ) 

obtenerValor :: Token -> String
obtenerValor (Token _ v _ _) = v

pUidentifier   =                               tSym Uident ""
pLidentifier   =                               tSym Lident ""
pInteger       = (\x -> (read x)::Integer) <$> tSym IntegerL ""
pFloatingPonit =                               tSym Float ""
pInfix         =                               tSym Infix ""
pPrefix        =                               tSym Prefix ""
pString        =                               tSym String ""
pKeyword key   =                               tSym Keyword key
pDelimiter del =                               tSym Delimiter del
--pComment       =                               tSym Comment ""
pChar          =                               tSym Char ""
pOperator  op  =                               tSym Operator op   
pLabelName     =                               tSym LabelName ""
pLabel         =                               tSym Label ""
pOptLabel      =                               tSym OptLabel ""
pError         =                               tSym Error ""
