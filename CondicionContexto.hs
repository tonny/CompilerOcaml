

-- UUAGC 0.9.6 (CondicionContexto.ag)
module CondicionContexto where

import Char


type Lidentifier = String
type Uidentifier = String
type Prefix      = String
type Infix       = String


cargar id decls | elem id decls = decls
                | otherwise     = id:decls


errorDecl id var | elem id var  = ""
                 | otherwise    = "error variable " ++ id ++ " no declarado" 

-- var = ["a","b","c","d","f"]



typo t first second | t == "+"  = suma first second                   
                    | elem t ["<",">"] = buleano first second
                    | otherwise = ""

                 where 
                   suma [] _ = " Tipos Incompatibles \n"
                   suma _ [] = " Tipos Incompatibles \n"
--                   suma [] [] = ""
                   suma (x:fi) (y:se) | isDigit x && isDigit x = ""
                                      | isDigit x && (elem y var2) = ""
                                      | (elem x var2) && isDigit y = ""
--                                      | isAlpha x && isDigit y  = " Tipos Incompatibles " ++ fi ++ se ++ "\n"
  --                                    | isDigit x && isAlpha y  = " Tipos Incompatibles \n"
    --                                  | isDigit x  (elem y var) = " Tipos Incompatibles \n"
      --                                | (elem x var) /= isDigit y = "Tipos Incompatibles \n" 
                                      | otherwise = " Tipos Incompatibles " ++ fi ++" "++ se ++"\n"
                   buleano f s = suma f s

var2 = "abcdf"

berificar x | x == "string" = ""
            | x == [] = ""
            | otherwise ="Tipo incompatibles " ++ x

{-                where 
                  suma _ _ False = False
                  suma [] [] True = True
                  suma _ [] True = True
                  suma [] _ True = True
                  suma (x:fi) (y:se) b | ( isDigit x && isDigit y )&& b = suma fi se  b
                                       | ( isAlpha x || isAlpha y ) && b= suma [] [] False
--                                       | isAlpha x && b = suma [] False
                                       | otherwise = False

Cuerpo [DefinicionList 
          (DefinicionLet 
                EmptyRec 
                [LetBindingPat 
                      (PatternValue (ValueName "b")) 
                      (ExprMatch 
                          (ExprValuePath (ValuePath (ValueName "a"))) 
                          (PatternMatching 
                              [OptPattern (PatternCons ConstantInteger) WhenKey (ExprConstant ConstantString),
                               OptPattern (PatternCons ConstantInteger) WhenKey (ExprConstant ConstantString),
                               OptPattern (PatternCons ConstantInteger) WhenKey (ExprConstant ConstantInteger)
                              ]
                           )
                       )
                 ]
            ) 
            Fin
         ]


Cuerpo [ DefinicionList 
           ( DefinicionLet 
                 EmptyRec 
                 [ LetBindingPat 
                      (PatternValue (ValueName "a")) 
                      (ExprInfixOP (Op "+") 
                         (ExprInfixOP (Op "+") 
                              (ExprInfixOP (Op "+") 
                                  ( ExprInfixOP (Op "+") 
                                        (ExprInfixOP (Op "+") (ExprConstant ConstantInteger) (ExprConstant ConstantInteger))
                                        (ExprConstant ConstantInteger)
                                  ) (ExprConstant ConstantInteger)
                              ) (ExprConstant ConstantInteger)
                         ) 
                         (ExprConstant ConstantInteger)
                      )
               ]) Fin]
-}

-- Argument ----------------------------------------------------
data Argument  = ArgumentExp (Expr) 
               deriving ( Show)
-- cata
sem_Argument :: Argument  ->
                T_Argument 
sem_Argument (ArgumentExp _expr )  =
    (sem_Argument_ArgumentExp (sem_Expr _expr ) )
-- semantic domain
type T_Argument  = ([String]) ->
                   ( String,([String]),String)
sem_Argument_ArgumentExp :: T_Expr  ->
                            T_Argument 
sem_Argument_ArgumentExp expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ClassName ---------------------------------------------------
data ClassName  = ClassName (Lidentifier) 
                deriving ( Show)
-- cata
sem_ClassName :: ClassName  ->
                 T_ClassName 
sem_ClassName (ClassName _lidentifier )  =
    (sem_ClassName_ClassName _lidentifier )
-- semantic domain
type T_ClassName  = ( )
sem_ClassName_ClassName :: Lidentifier ->
                           T_ClassName 
sem_ClassName_ClassName lidentifier_  =
    (let 
     in  ( ))
-- ClassPath ---------------------------------------------------
data ClassPath  = ClassPath (ClassName) 
                deriving ( Show)
-- cata
sem_ClassPath :: ClassPath  ->
                 T_ClassPath 
sem_ClassPath (ClassPath _className )  =
    (sem_ClassPath_ClassPath (sem_ClassName _className ) )
-- semantic domain
type T_ClassPath  = ( )
sem_ClassPath_ClassPath :: T_ClassName  ->
                           T_ClassPath 
sem_ClassPath_ClassPath className_  =
    (let 
     in  ( ))
-- Constant ----------------------------------------------------
data Constant  = ConstantChar 
               | ConstantConrch 
               | ConstantConstr (Constr) 
               | ConstantFalse 
               | ConstantFloat 
               | ConstantInteger (Integer) 
               | ConstantParent 
               | ConstantString 
               | ConstantTag (TagName) 
               | ConstantTrue 
               deriving ( Show)
-- cata
sem_Constant :: Constant  ->
                T_Constant 
sem_Constant (ConstantChar )  =
    (sem_Constant_ConstantChar )
sem_Constant (ConstantConrch )  =
    (sem_Constant_ConstantConrch )
sem_Constant (ConstantConstr _constr )  =
    (sem_Constant_ConstantConstr (sem_Constr _constr ) )
sem_Constant (ConstantFalse )  =
    (sem_Constant_ConstantFalse )
sem_Constant (ConstantFloat )  =
    (sem_Constant_ConstantFloat )
sem_Constant (ConstantInteger _integer )  =
    (sem_Constant_ConstantInteger _integer )
sem_Constant (ConstantParent )  =
    (sem_Constant_ConstantParent )
sem_Constant (ConstantString )  =
    (sem_Constant_ConstantString )
sem_Constant (ConstantTag _tagName )  =
    (sem_Constant_ConstantTag (sem_TagName _tagName ) )
sem_Constant (ConstantTrue )  =
    (sem_Constant_ConstantTrue )
-- semantic domain
type T_Constant  = ( String)
sem_Constant_ConstantChar :: T_Constant 
sem_Constant_ConstantChar  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantConrch :: T_Constant 
sem_Constant_ConstantConrch  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantConstr :: T_Constr  ->
                               T_Constant 
sem_Constant_ConstantConstr constr_  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantFalse :: T_Constant 
sem_Constant_ConstantFalse  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantFloat :: T_Constant 
sem_Constant_ConstantFloat  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantInteger :: Integer ->
                                T_Constant 
sem_Constant_ConstantInteger integer_  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             show integer_
     in  ( _lhsOtipo))
sem_Constant_ConstantParent :: T_Constant 
sem_Constant_ConstantParent  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantString :: T_Constant 
sem_Constant_ConstantString  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             "string"
     in  ( _lhsOtipo))
sem_Constant_ConstantTag :: T_TagName  ->
                            T_Constant 
sem_Constant_ConstantTag tagName_  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
sem_Constant_ConstantTrue :: T_Constant 
sem_Constant_ConstantTrue  =
    (let _lhsOtipo :: String
         _lhsOtipo =
             ""
     in  ( _lhsOtipo))
-- Constr ------------------------------------------------------
data Constr  = Constr (ConstructorName) 
             | ConstrModule (ModulePath) (ConstructorName) 
             deriving ( Show)
-- cata
sem_Constr :: Constr  ->
              T_Constr 
sem_Constr (Constr _constructorName )  =
    (sem_Constr_Constr (sem_ConstructorName _constructorName ) )
sem_Constr (ConstrModule _modulePath _constructorName )  =
    (sem_Constr_ConstrModule (sem_ModulePath _modulePath ) (sem_ConstructorName _constructorName ) )
-- semantic domain
type T_Constr  = ( )
sem_Constr_Constr :: T_ConstructorName  ->
                     T_Constr 
sem_Constr_Constr constructorName_  =
    (let 
     in  ( ))
sem_Constr_ConstrModule :: T_ModulePath  ->
                           T_ConstructorName  ->
                           T_Constr 
sem_Constr_ConstrModule modulePath_ constructorName_  =
    (let 
     in  ( ))
-- ConstructorName ---------------------------------------------
data ConstructorName  = ConstructorName (Uidentifier) 
                      deriving ( Show)
-- cata
sem_ConstructorName :: ConstructorName  ->
                       T_ConstructorName 
sem_ConstructorName (ConstructorName _uidentifier )  =
    (sem_ConstructorName_ConstructorName _uidentifier )
-- semantic domain
type T_ConstructorName  = ( )
sem_ConstructorName_ConstructorName :: Uidentifier ->
                                       T_ConstructorName 
sem_ConstructorName_ConstructorName uidentifier_  =
    (let 
     in  ( ))
-- Cuerpo ------------------------------------------------------
data Cuerpo  = Cuerpo (ListDefinicion) 
             deriving ( Show)
-- cata
sem_Cuerpo :: Cuerpo  ->
              T_Cuerpo 
sem_Cuerpo (Cuerpo _listDefinicion )  =
    (sem_Cuerpo_Cuerpo (sem_ListDefinicion _listDefinicion ) )
-- semantic domain
type T_Cuerpo  = ( String,String)
sem_Cuerpo_Cuerpo :: T_ListDefinicion  ->
                     T_Cuerpo 
sem_Cuerpo_Cuerpo listDefinicion_  =
    (let _listDefinicionOdeclarados :: ([String])
         _lhsOboolean :: String
         _lhsOdecls :: String
         _listDefinicionIboolean :: String
         _listDefinicionIdeclarados :: ([String])
         _listDefinicionIdecls :: String
         _listDefinicionOdeclarados =
             []
         _lhsOboolean =
             _listDefinicionIboolean
         _lhsOdecls =
             _listDefinicionIdecls
         ( _listDefinicionIboolean,_listDefinicionIdeclarados,_listDefinicionIdecls) =
             (listDefinicion_ _listDefinicionOdeclarados )
     in  ( _lhsOboolean,_lhsOdecls))
-- Definicion --------------------------------------------------
data Definicion  = DefinicionLet (Rec) (ListLetBinding) 
                 deriving ( Show)
-- cata
sem_Definicion :: Definicion  ->
                  T_Definicion 
sem_Definicion (DefinicionLet _rec _listLetBinding )  =
    (sem_Definicion_DefinicionLet (sem_Rec _rec ) (sem_ListLetBinding _listLetBinding ) )
-- semantic domain
type T_Definicion  = ([String]) ->
                     ( String,([String]),String)
sem_Definicion_DefinicionLet :: T_Rec  ->
                                T_ListLetBinding  ->
                                T_Definicion 
sem_Definicion_DefinicionLet rec_ listLetBinding_  =
    (\ _lhsIdeclarados ->
         (let _listLetBindingOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listLetBindingIboolean :: String
              _listLetBindingIdeclarados :: ([String])
              _listLetBindingIdecls :: String
              _listLetBindingOdeclarados =
                  _lhsIdeclarados
              _lhsOdecls =
                  _listLetBindingIdecls
              _lhsOboolean =
                  _listLetBindingIboolean
              _lhsOdeclarados =
                  _listLetBindingIdeclarados
              ( _listLetBindingIboolean,_listLetBindingIdeclarados,_listLetBindingIdecls) =
                  (listLetBinding_ _listLetBindingOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- DefinicionList ----------------------------------------------
data DefinicionList  = DefinicionList (Definicion) (Fin) 
                     deriving ( Show)
-- cata
sem_DefinicionList :: DefinicionList  ->
                      T_DefinicionList 
sem_DefinicionList (DefinicionList _definicion _fin )  =
    (sem_DefinicionList_DefinicionList (sem_Definicion _definicion ) (sem_Fin _fin ) )
-- semantic domain
type T_DefinicionList  = ([String]) ->
                         ( String,([String]),String)
sem_DefinicionList_DefinicionList :: T_Definicion  ->
                                     T_Fin  ->
                                     T_DefinicionList 
sem_DefinicionList_DefinicionList definicion_ fin_  =
    (\ _lhsIdeclarados ->
         (let _definicionOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _definicionIboolean :: String
              _definicionIdeclarados :: ([String])
              _definicionIdecls :: String
              _definicionOdeclarados =
                  _lhsIdeclarados
              _lhsOdecls =
                  _definicionIdecls
              _lhsOboolean =
                  _definicionIboolean
              _lhsOdeclarados =
                  _definicionIdeclarados
              ( _definicionIboolean,_definicionIdeclarados,_definicionIdecls) =
                  (definicion_ _definicionOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- Else --------------------------------------------------------
data Else  = Else (Expr) 
           | EmptyElse 
           deriving ( Show)
-- cata
sem_Else :: Else  ->
            T_Else 
sem_Else (Else _expr )  =
    (sem_Else_Else (sem_Expr _expr ) )
sem_Else (EmptyElse )  =
    (sem_Else_EmptyElse )
-- semantic domain
type T_Else  = ([String]) ->
               ( String,([String]),String)
sem_Else_Else :: T_Expr  ->
                 T_Else 
sem_Else_Else expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Else_EmptyElse :: T_Else 
sem_Else_EmptyElse  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- EqualOper ---------------------------------------------------
data EqualOper  = EqualExp 
                deriving ( Show)
-- cata
sem_EqualOper :: EqualOper  ->
                 T_EqualOper 
sem_EqualOper (EqualExp )  =
    (sem_EqualOper_EqualExp )
-- semantic domain
type T_EqualOper  = ( )
sem_EqualOper_EqualExp :: T_EqualOper 
sem_EqualOper_EqualExp  =
    (let 
     in  ( ))
-- Expr --------------------------------------------------------
data Expr  = ExprArgument (ListArgument) (Expr) 
           | ExprArray (ListExpr) 
           | ExprAssert (Expr) 
           | ExprBegin (Expr) 
           | ExprConstant (Constant) 
           | ExprConstr (Constr) (Expr) 
           | ExprCostLista (Expr) (Expr) 
           | ExprExpr (ListExpr) 
           | ExprExpr2 (Expr) (Expr) 
           | ExprField (ListFieldExpr) 
           | ExprFieldKey (FieldKey) (Expr) 
           | ExprFor (ValueName) (Expr) (ToDow) (Expr) (Expr) 
           | ExprFun (MultipleMatching) 
           | ExprFunction (PatternMatching) 
           | ExprGrup (Expr) (KeyOpeT) 
           | ExprIf (Expr) (Expr) (Else) 
           | ExprInfixOP (Op) (Expr) (Expr) 
           | ExprInstVarName (InstVariantName) (ExpreFlec) 
           | ExprLazy (Expr) 
           | ExprLet (Rec) (ListLetBinding) (Expr) 
           | ExprLista (ListExpr) 
           | ExprMatch (Expr) (PatternMatching) 
           | ExprMethodName (MethodName) (Expr) 
           | ExprPrefixSym (Expr) 
           | ExprTagName (TagName) (Expr) 
           | ExprTry (Expr) (PatternMatching) 
           | ExprValuePath (ValuePath) 
           | ExprWhile (Expr) (Expr) 
           | ExprWhit (Expr) (ListFieldExpr) 
           deriving ( Show)
-- cata
sem_Expr :: Expr  ->
            T_Expr 
sem_Expr (ExprArgument _listArgument _expr )  =
    (sem_Expr_ExprArgument (sem_ListArgument _listArgument ) (sem_Expr _expr ) )
sem_Expr (ExprArray _listExpr )  =
    (sem_Expr_ExprArray (sem_ListExpr _listExpr ) )
sem_Expr (ExprAssert _expr )  =
    (sem_Expr_ExprAssert (sem_Expr _expr ) )
sem_Expr (ExprBegin _expr )  =
    (sem_Expr_ExprBegin (sem_Expr _expr ) )
sem_Expr (ExprConstant _constant )  =
    (sem_Expr_ExprConstant (sem_Constant _constant ) )
sem_Expr (ExprConstr _constr _expr )  =
    (sem_Expr_ExprConstr (sem_Constr _constr ) (sem_Expr _expr ) )
sem_Expr (ExprCostLista _firstC _seconfC )  =
    (sem_Expr_ExprCostLista (sem_Expr _firstC ) (sem_Expr _seconfC ) )
sem_Expr (ExprExpr _listExpr )  =
    (sem_Expr_ExprExpr (sem_ListExpr _listExpr ) )
sem_Expr (ExprExpr2 _fist _second )  =
    (sem_Expr_ExprExpr2 (sem_Expr _fist ) (sem_Expr _second ) )
sem_Expr (ExprField _listFieldExpr )  =
    (sem_Expr_ExprField (sem_ListFieldExpr _listFieldExpr ) )
sem_Expr (ExprFieldKey _fieldKey _expr )  =
    (sem_Expr_ExprFieldKey (sem_FieldKey _fieldKey ) (sem_Expr _expr ) )
sem_Expr (ExprFor _valueName _eval _toDow _opera _do )  =
    (sem_Expr_ExprFor (sem_ValueName _valueName ) (sem_Expr _eval ) (sem_ToDow _toDow ) (sem_Expr _opera ) (sem_Expr _do ) )
sem_Expr (ExprFun _multipleMatching )  =
    (sem_Expr_ExprFun (sem_MultipleMatching _multipleMatching ) )
sem_Expr (ExprFunction _patternMatching )  =
    (sem_Expr_ExprFunction (sem_PatternMatching _patternMatching ) )
sem_Expr (ExprGrup _expr _keyOpeT )  =
    (sem_Expr_ExprGrup (sem_Expr _expr ) (sem_KeyOpeT _keyOpeT ) )
sem_Expr (ExprIf _comp _then _else )  =
    (sem_Expr_ExprIf (sem_Expr _comp ) (sem_Expr _then ) (sem_Else _else ) )
sem_Expr (ExprInfixOP _op _first _second )  =
    (sem_Expr_ExprInfixOP (sem_Op _op ) (sem_Expr _first ) (sem_Expr _second ) )
sem_Expr (ExprInstVarName _instVariantName _expreFlec )  =
    (sem_Expr_ExprInstVarName (sem_InstVariantName _instVariantName ) (sem_ExpreFlec _expreFlec ) )
sem_Expr (ExprLazy _expr )  =
    (sem_Expr_ExprLazy (sem_Expr _expr ) )
sem_Expr (ExprLet _rec _listLetBinding _expr )  =
    (sem_Expr_ExprLet (sem_Rec _rec ) (sem_ListLetBinding _listLetBinding ) (sem_Expr _expr ) )
sem_Expr (ExprLista _listExpr )  =
    (sem_Expr_ExprLista (sem_ListExpr _listExpr ) )
sem_Expr (ExprMatch _expr _patternMatching )  =
    (sem_Expr_ExprMatch (sem_Expr _expr ) (sem_PatternMatching _patternMatching ) )
sem_Expr (ExprMethodName _methodName _expr )  =
    (sem_Expr_ExprMethodName (sem_MethodName _methodName ) (sem_Expr _expr ) )
sem_Expr (ExprPrefixSym _expr )  =
    (sem_Expr_ExprPrefixSym (sem_Expr _expr ) )
sem_Expr (ExprTagName _tagName _expr )  =
    (sem_Expr_ExprTagName (sem_TagName _tagName ) (sem_Expr _expr ) )
sem_Expr (ExprTry _expr _patternMatching )  =
    (sem_Expr_ExprTry (sem_Expr _expr ) (sem_PatternMatching _patternMatching ) )
sem_Expr (ExprValuePath _valuePath )  =
    (sem_Expr_ExprValuePath (sem_ValuePath _valuePath ) )
sem_Expr (ExprWhile _egual _doo )  =
    (sem_Expr_ExprWhile (sem_Expr _egual ) (sem_Expr _doo ) )
sem_Expr (ExprWhit _expr _listFieldExpr )  =
    (sem_Expr_ExprWhit (sem_Expr _expr ) (sem_ListFieldExpr _listFieldExpr ) )
-- semantic domain
type T_Expr  = ([String]) ->
               ( String,([String]),String)
sem_Expr_ExprArgument :: T_ListArgument  ->
                         T_Expr  ->
                         T_Expr 
sem_Expr_ExprArgument listArgument_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listArgumentOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _listArgumentIboolean :: String
              _listArgumentIdeclarados :: ([String])
              _listArgumentIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _listArgumentIdecls ++ _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _listArgumentOdeclarados =
                  _lhsIdeclarados
              _exprOdeclarados =
                  _listArgumentIdeclarados
              ( _listArgumentIboolean,_listArgumentIdeclarados,_listArgumentIdecls) =
                  (listArgument_ _listArgumentOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprArray :: T_ListExpr  ->
                      T_Expr 
sem_Expr_ExprArray listExpr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listExprOdeclarados :: ([String])
              _listExprIboolean :: String
              _listExprIdeclarados :: ([String])
              _listExprIdecls :: String
              _lhsOdecls =
                  _listExprIdecls
              _lhsOboolean =
                  _listExprIboolean
              _lhsOdeclarados =
                  _listExprIdeclarados
              _listExprOdeclarados =
                  _lhsIdeclarados
              ( _listExprIboolean,_listExprIdeclarados,_listExprIdecls) =
                  (listExpr_ _listExprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprAssert :: T_Expr  ->
                       T_Expr 
sem_Expr_ExprAssert expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprBegin :: T_Expr  ->
                      T_Expr 
sem_Expr_ExprBegin expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprConstant :: T_Constant  ->
                         T_Expr 
sem_Expr_ExprConstant constant_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _constantItipo :: String
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  _constantItipo
              _lhsOdeclarados =
                  _lhsIdeclarados
              ( _constantItipo) =
                  (constant_ )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprConstr :: T_Constr  ->
                       T_Expr  ->
                       T_Expr 
sem_Expr_ExprConstr constr_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprCostLista :: T_Expr  ->
                          T_Expr  ->
                          T_Expr 
sem_Expr_ExprCostLista firstC_ seconfC_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _firstCOdeclarados :: ([String])
              _seconfCOdeclarados :: ([String])
              _firstCIboolean :: String
              _firstCIdeclarados :: ([String])
              _firstCIdecls :: String
              _seconfCIboolean :: String
              _seconfCIdeclarados :: ([String])
              _seconfCIdecls :: String
              _lhsOdecls =
                  _firstCIdecls ++ _seconfCIdecls
              _lhsOboolean =
                  _seconfCIboolean
              _lhsOdeclarados =
                  _seconfCIdeclarados
              _firstCOdeclarados =
                  _lhsIdeclarados
              _seconfCOdeclarados =
                  _firstCIdeclarados
              ( _firstCIboolean,_firstCIdeclarados,_firstCIdecls) =
                  (firstC_ _firstCOdeclarados )
              ( _seconfCIboolean,_seconfCIdeclarados,_seconfCIdecls) =
                  (seconfC_ _seconfCOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprExpr :: T_ListExpr  ->
                     T_Expr 
sem_Expr_ExprExpr listExpr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listExprOdeclarados :: ([String])
              _listExprIboolean :: String
              _listExprIdeclarados :: ([String])
              _listExprIdecls :: String
              _lhsOdecls =
                  _listExprIdecls
              _lhsOboolean =
                  _listExprIboolean
              _lhsOdeclarados =
                  _listExprIdeclarados
              _listExprOdeclarados =
                  _lhsIdeclarados
              ( _listExprIboolean,_listExprIdeclarados,_listExprIdecls) =
                  (listExpr_ _listExprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprExpr2 :: T_Expr  ->
                      T_Expr  ->
                      T_Expr 
sem_Expr_ExprExpr2 fist_ second_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _fistOdeclarados :: ([String])
              _secondOdeclarados :: ([String])
              _fistIboolean :: String
              _fistIdeclarados :: ([String])
              _fistIdecls :: String
              _secondIboolean :: String
              _secondIdeclarados :: ([String])
              _secondIdecls :: String
              _lhsOboolean =
                  _secondIboolean
              _lhsOdeclarados =
                  _secondIdeclarados
              _lhsOdecls =
                  _secondIdecls
              _fistOdeclarados =
                  _lhsIdeclarados
              _secondOdeclarados =
                  _fistIdeclarados
              ( _fistIboolean,_fistIdeclarados,_fistIdecls) =
                  (fist_ _fistOdeclarados )
              ( _secondIboolean,_secondIdeclarados,_secondIdecls) =
                  (second_ _secondOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprField :: T_ListFieldExpr  ->
                      T_Expr 
sem_Expr_ExprField listFieldExpr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listFieldExprOdeclarados :: ([String])
              _listFieldExprIboolean :: String
              _listFieldExprIdeclarados :: ([String])
              _listFieldExprIdecls :: String
              _lhsOdecls =
                  _listFieldExprIdecls
              _lhsOboolean =
                  _listFieldExprIboolean
              _lhsOdeclarados =
                  _listFieldExprIdeclarados
              _listFieldExprOdeclarados =
                  _lhsIdeclarados
              ( _listFieldExprIboolean,_listFieldExprIdeclarados,_listFieldExprIdecls) =
                  (listFieldExpr_ _listFieldExprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprFieldKey :: T_FieldKey  ->
                         T_Expr  ->
                         T_Expr 
sem_Expr_ExprFieldKey fieldKey_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _fieldKeyOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _fieldKeyIboolean :: String
              _fieldKeyIdeclarados :: ([String])
              _fieldKeyIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _fieldKeyOdeclarados =
                  _lhsIdeclarados
              _exprOdeclarados =
                  _fieldKeyIdeclarados
              ( _fieldKeyIboolean,_fieldKeyIdeclarados,_fieldKeyIdecls) =
                  (fieldKey_ _fieldKeyOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprFor :: T_ValueName  ->
                    T_Expr  ->
                    T_ToDow  ->
                    T_Expr  ->
                    T_Expr  ->
                    T_Expr 
sem_Expr_ExprFor valueName_ eval_ toDow_ opera_ do_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _valueNameOdeclarados :: ([String])
              _evalOdeclarados :: ([String])
              _operaOdeclarados :: ([String])
              _doOdeclarados :: ([String])
              _valueNameIboolean :: String
              _valueNameIdeclarados :: ([String])
              _valueNameIdecls :: String
              _evalIboolean :: String
              _evalIdeclarados :: ([String])
              _evalIdecls :: String
              _operaIboolean :: String
              _operaIdeclarados :: ([String])
              _operaIdecls :: String
              _doIboolean :: String
              _doIdeclarados :: ([String])
              _doIdecls :: String
              _lhsOdecls =
                  _valueNameIdecls
              _lhsOboolean =
                  _doIboolean
              _lhsOdeclarados =
                  _doIdeclarados
              _valueNameOdeclarados =
                  _lhsIdeclarados
              _evalOdeclarados =
                  _valueNameIdeclarados
              _operaOdeclarados =
                  _evalIdeclarados
              _doOdeclarados =
                  _operaIdeclarados
              ( _valueNameIboolean,_valueNameIdeclarados,_valueNameIdecls) =
                  (valueName_ _valueNameOdeclarados )
              ( _evalIboolean,_evalIdeclarados,_evalIdecls) =
                  (eval_ _evalOdeclarados )
              ( _operaIboolean,_operaIdeclarados,_operaIdecls) =
                  (opera_ _operaOdeclarados )
              ( _doIboolean,_doIdeclarados,_doIdecls) =
                  (do_ _doOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprFun :: T_MultipleMatching  ->
                    T_Expr 
sem_Expr_ExprFun multipleMatching_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _multipleMatchingOdeclarados :: ([String])
              _multipleMatchingIboolean :: String
              _multipleMatchingIdeclarados :: ([String])
              _multipleMatchingIdecls :: String
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  _multipleMatchingIboolean
              _lhsOdeclarados =
                  _multipleMatchingIdeclarados
              _multipleMatchingOdeclarados =
                  _lhsIdeclarados
              ( _multipleMatchingIboolean,_multipleMatchingIdeclarados,_multipleMatchingIdecls) =
                  (multipleMatching_ _multipleMatchingOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprFunction :: T_PatternMatching  ->
                         T_Expr 
sem_Expr_ExprFunction patternMatching_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _patternMatchingOdeclarados :: ([String])
              _patternMatchingIboolean :: String
              _patternMatchingIdeclarados :: ([String])
              _patternMatchingIdecls :: String
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  _patternMatchingIboolean
              _lhsOdeclarados =
                  _patternMatchingIdeclarados
              _patternMatchingOdeclarados =
                  _lhsIdeclarados
              ( _patternMatchingIboolean,_patternMatchingIdeclarados,_patternMatchingIdecls) =
                  (patternMatching_ _patternMatchingOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprGrup :: T_Expr  ->
                     T_KeyOpeT  ->
                     T_Expr 
sem_Expr_ExprGrup expr_ keyOpeT_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprIf :: T_Expr  ->
                   T_Expr  ->
                   T_Else  ->
                   T_Expr 
sem_Expr_ExprIf comp_ then_ else_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _compOdeclarados :: ([String])
              _thenOdeclarados :: ([String])
              _elseOdeclarados :: ([String])
              _compIboolean :: String
              _compIdeclarados :: ([String])
              _compIdecls :: String
              _thenIboolean :: String
              _thenIdeclarados :: ([String])
              _thenIdecls :: String
              _elseIboolean :: String
              _elseIdeclarados :: ([String])
              _elseIdecls :: String
              _lhsOdecls =
                  _compIdecls ++ _thenIdecls
              _lhsOboolean =
                  _compIboolean
              _lhsOdeclarados =
                  _elseIdeclarados
              _compOdeclarados =
                  _lhsIdeclarados
              _thenOdeclarados =
                  _compIdeclarados
              _elseOdeclarados =
                  _thenIdeclarados
              ( _compIboolean,_compIdeclarados,_compIdecls) =
                  (comp_ _compOdeclarados )
              ( _thenIboolean,_thenIdeclarados,_thenIdecls) =
                  (then_ _thenOdeclarados )
              ( _elseIboolean,_elseIdeclarados,_elseIdecls) =
                  (else_ _elseOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprInfixOP :: T_Op  ->
                        T_Expr  ->
                        T_Expr  ->
                        T_Expr 
sem_Expr_ExprInfixOP op_ first_ second_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _firstOdeclarados :: ([String])
              _secondOdeclarados :: ([String])
              _opIboolean :: String
              _opIdecls :: String
              _firstIboolean :: String
              _firstIdeclarados :: ([String])
              _firstIdecls :: String
              _secondIboolean :: String
              _secondIdeclarados :: ([String])
              _secondIdecls :: String
              _lhsOdecls =
                  _opIdecls ++ _firstIdecls ++ _secondIdecls
              _lhsOboolean =
                  typo _opIboolean _firstIboolean _secondIboolean
              _lhsOdeclarados =
                  _secondIdeclarados
              _firstOdeclarados =
                  _lhsIdeclarados
              _secondOdeclarados =
                  _firstIdeclarados
              ( _opIboolean,_opIdecls) =
                  (op_ )
              ( _firstIboolean,_firstIdeclarados,_firstIdecls) =
                  (first_ _firstOdeclarados )
              ( _secondIboolean,_secondIdeclarados,_secondIdecls) =
                  (second_ _secondOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprInstVarName :: T_InstVariantName  ->
                            T_ExpreFlec  ->
                            T_Expr 
sem_Expr_ExprInstVarName instVariantName_ expreFlec_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _expreFlecOdeclarados :: ([String])
              _expreFlecIboolean :: String
              _expreFlecIdeclarados :: ([String])
              _expreFlecIdecls :: String
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  _expreFlecIboolean
              _lhsOdeclarados =
                  _expreFlecIdeclarados
              _expreFlecOdeclarados =
                  _lhsIdeclarados
              ( _expreFlecIboolean,_expreFlecIdeclarados,_expreFlecIdecls) =
                  (expreFlec_ _expreFlecOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprLazy :: T_Expr  ->
                     T_Expr 
sem_Expr_ExprLazy expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprLet :: T_Rec  ->
                    T_ListLetBinding  ->
                    T_Expr  ->
                    T_Expr 
sem_Expr_ExprLet rec_ listLetBinding_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listLetBindingOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _listLetBindingIboolean :: String
              _listLetBindingIdeclarados :: ([String])
              _listLetBindingIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _listLetBindingOdeclarados =
                  _lhsIdeclarados
              _exprOdeclarados =
                  _listLetBindingIdeclarados
              ( _listLetBindingIboolean,_listLetBindingIdeclarados,_listLetBindingIdecls) =
                  (listLetBinding_ _listLetBindingOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprLista :: T_ListExpr  ->
                      T_Expr 
sem_Expr_ExprLista listExpr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listExprOdeclarados :: ([String])
              _listExprIboolean :: String
              _listExprIdeclarados :: ([String])
              _listExprIdecls :: String
              _lhsOdecls =
                  _listExprIdecls
              _lhsOboolean =
                  _listExprIboolean
              _lhsOdeclarados =
                  _listExprIdeclarados
              _listExprOdeclarados =
                  _lhsIdeclarados
              ( _listExprIboolean,_listExprIdeclarados,_listExprIdecls) =
                  (listExpr_ _listExprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprMatch :: T_Expr  ->
                      T_PatternMatching  ->
                      T_Expr 
sem_Expr_ExprMatch expr_ patternMatching_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _patternMatchingOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _patternMatchingIboolean :: String
              _patternMatchingIdeclarados :: ([String])
              _patternMatchingIdecls :: String
              _lhsOdecls =
                  _exprIdecls ++ _patternMatchingIdecls
              _lhsOboolean =
                  _patternMatchingIboolean
              _lhsOdeclarados =
                  _patternMatchingIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              _patternMatchingOdeclarados =
                  _exprIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
              ( _patternMatchingIboolean,_patternMatchingIdeclarados,_patternMatchingIdecls) =
                  (patternMatching_ _patternMatchingOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprMethodName :: T_MethodName  ->
                           T_Expr  ->
                           T_Expr 
sem_Expr_ExprMethodName methodName_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprPrefixSym :: T_Expr  ->
                          T_Expr 
sem_Expr_ExprPrefixSym expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprTagName :: T_TagName  ->
                        T_Expr  ->
                        T_Expr 
sem_Expr_ExprTagName tagName_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprTry :: T_Expr  ->
                    T_PatternMatching  ->
                    T_Expr 
sem_Expr_ExprTry expr_ patternMatching_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _patternMatchingOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _patternMatchingIboolean :: String
              _patternMatchingIdeclarados :: ([String])
              _patternMatchingIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _patternMatchingIboolean
              _lhsOdeclarados =
                  _patternMatchingIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              _patternMatchingOdeclarados =
                  _exprIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
              ( _patternMatchingIboolean,_patternMatchingIdeclarados,_patternMatchingIdecls) =
                  (patternMatching_ _patternMatchingOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprValuePath :: T_ValuePath  ->
                          T_Expr 
sem_Expr_ExprValuePath valuePath_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _valuePathOdeclarados :: ([String])
              _valuePathIboolean :: String
              _valuePathIdeclarados :: ([String])
              _valuePathIdecls :: String
              _lhsOdecls =
                  _valuePathIdecls
              _lhsOboolean =
                  _valuePathIboolean
              _lhsOdeclarados =
                  _valuePathIdeclarados
              _valuePathOdeclarados =
                  _lhsIdeclarados
              ( _valuePathIboolean,_valuePathIdeclarados,_valuePathIdecls) =
                  (valuePath_ _valuePathOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprWhile :: T_Expr  ->
                      T_Expr  ->
                      T_Expr 
sem_Expr_ExprWhile egual_ doo_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _egualOdeclarados :: ([String])
              _dooOdeclarados :: ([String])
              _egualIboolean :: String
              _egualIdeclarados :: ([String])
              _egualIdecls :: String
              _dooIboolean :: String
              _dooIdeclarados :: ([String])
              _dooIdecls :: String
              _lhsOdecls =
                  _egualIdecls
              _lhsOboolean =
                  _dooIboolean
              _lhsOdeclarados =
                  _dooIdeclarados
              _egualOdeclarados =
                  _lhsIdeclarados
              _dooOdeclarados =
                  _egualIdeclarados
              ( _egualIboolean,_egualIdeclarados,_egualIdecls) =
                  (egual_ _egualOdeclarados )
              ( _dooIboolean,_dooIdeclarados,_dooIdecls) =
                  (doo_ _dooOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Expr_ExprWhit :: T_Expr  ->
                     T_ListFieldExpr  ->
                     T_Expr 
sem_Expr_ExprWhit expr_ listFieldExpr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _listFieldExprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _listFieldExprIboolean :: String
              _listFieldExprIdeclarados :: ([String])
              _listFieldExprIdecls :: String
              _lhsOdecls =
                  _exprIdecls  ++ _listFieldExprIdecls
              _lhsOboolean =
                  _listFieldExprIboolean
              _lhsOdeclarados =
                  _listFieldExprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              _listFieldExprOdeclarados =
                  _exprIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
              ( _listFieldExprIboolean,_listFieldExprIdeclarados,_listFieldExprIdecls) =
                  (listFieldExpr_ _listFieldExprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ExpreFlec ---------------------------------------------------
data ExpreFlec  = ExpreFlec (Expr) 
                deriving ( Show)
-- cata
sem_ExpreFlec :: ExpreFlec  ->
                 T_ExpreFlec 
sem_ExpreFlec (ExpreFlec _expr )  =
    (sem_ExpreFlec_ExpreFlec (sem_Expr _expr ) )
-- semantic domain
type T_ExpreFlec  = ([String]) ->
                    ( String,([String]),String)
sem_ExpreFlec_ExpreFlec :: T_Expr  ->
                           T_ExpreFlec 
sem_ExpreFlec_ExpreFlec expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _lhsOdecls =
                  _exprIdecls
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ExtendedModulePath ------------------------------------------
data ExtendedModulePath  = ExtendedModulePath (ModuleName) (ExtendedModulePath') 
                         deriving ( Show)
-- cata
sem_ExtendedModulePath :: ExtendedModulePath  ->
                          T_ExtendedModulePath 
sem_ExtendedModulePath (ExtendedModulePath _moduleName _extendedModulePath' )  =
    (sem_ExtendedModulePath_ExtendedModulePath (sem_ModuleName _moduleName ) (sem_ExtendedModulePath' _extendedModulePath' ) )
-- semantic domain
type T_ExtendedModulePath  = ( )
sem_ExtendedModulePath_ExtendedModulePath :: T_ModuleName  ->
                                             (T_ExtendedModulePath')  ->
                                             T_ExtendedModulePath 
sem_ExtendedModulePath_ExtendedModulePath moduleName_ extendedModulePath'_  =
    (let 
     in  ( ))
-- ExtendedModulePath' -----------------------------------------
data ExtendedModulePath'  = EmptyExMoPath 
                          | ExtendedModulePath' (ModuleName) (ExtendedModulePath') 
                          | ExtendedModulePath'Ext (ExtendedModulePath) (ExtendedModulePath') 
                          deriving ( Show)
-- cata
sem_ExtendedModulePath' :: (ExtendedModulePath')  ->
                           (T_ExtendedModulePath') 
sem_ExtendedModulePath' (EmptyExMoPath )  =
    (sem_ExtendedModulePath'_EmptyExMoPath )
sem_ExtendedModulePath' (ExtendedModulePath' _moduleName _extendedModulePath' )  =
    (sem_ExtendedModulePath'_ExtendedModulePath' (sem_ModuleName _moduleName ) (sem_ExtendedModulePath' _extendedModulePath' ) )
sem_ExtendedModulePath' (ExtendedModulePath'Ext _extendedModulePath _extendedModulePath' )  =
    (sem_ExtendedModulePath'_ExtendedModulePath'Ext (sem_ExtendedModulePath _extendedModulePath ) (sem_ExtendedModulePath' _extendedModulePath' ) )
-- semantic domain
type T_ExtendedModulePath'  = ( )
sem_ExtendedModulePath'_EmptyExMoPath :: (T_ExtendedModulePath') 
sem_ExtendedModulePath'_EmptyExMoPath  =
    (let 
     in  ( ))
sem_ExtendedModulePath'_ExtendedModulePath' :: T_ModuleName  ->
                                               (T_ExtendedModulePath')  ->
                                               (T_ExtendedModulePath') 
sem_ExtendedModulePath'_ExtendedModulePath' moduleName_ extendedModulePath'_  =
    (let 
     in  ( ))
sem_ExtendedModulePath'_ExtendedModulePath'Ext :: T_ExtendedModulePath  ->
                                                  (T_ExtendedModulePath')  ->
                                                  (T_ExtendedModulePath') 
sem_ExtendedModulePath'_ExtendedModulePath'Ext extendedModulePath_ extendedModulePath'_  =
    (let 
     in  ( ))
-- Field -------------------------------------------------------
data Field  = Field (FieldName) 
            | FieldModule (ModulePath) (FieldName) 
            deriving ( Show)
-- cata
sem_Field :: Field  ->
             T_Field 
sem_Field (Field _fieldName )  =
    (sem_Field_Field (sem_FieldName _fieldName ) )
sem_Field (FieldModule _modulePath _fieldName )  =
    (sem_Field_FieldModule (sem_ModulePath _modulePath ) (sem_FieldName _fieldName ) )
-- semantic domain
type T_Field  = ( )
sem_Field_Field :: T_FieldName  ->
                   T_Field 
sem_Field_Field fieldName_  =
    (let 
     in  ( ))
sem_Field_FieldModule :: T_ModulePath  ->
                         T_FieldName  ->
                         T_Field 
sem_Field_FieldModule modulePath_ fieldName_  =
    (let 
     in  ( ))
-- FieldExpr ---------------------------------------------------
data FieldExpr  = FieldExpr (Field) (Expr) 
                deriving ( Show)
-- cata
sem_FieldExpr :: FieldExpr  ->
                 T_FieldExpr 
sem_FieldExpr (FieldExpr _field _expr )  =
    (sem_FieldExpr_FieldExpr (sem_Field _field ) (sem_Expr _expr ) )
-- semantic domain
type T_FieldExpr  = ([String]) ->
                    ( String,([String]),String)
sem_FieldExpr_FieldExpr :: T_Field  ->
                           T_Expr  ->
                           T_FieldExpr 
sem_FieldExpr_FieldExpr field_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _lhsOdecls =
                  _exprIdecls
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- FieldKey ----------------------------------------------------
data FieldKey  = FieldKeyBrek (Expr) (OptField) 
               | FieldKeyCor (Expr) (OptField) 
               | FieldKeyFiel (Field) (OptField) 
               deriving ( Show)
-- cata
sem_FieldKey :: FieldKey  ->
                T_FieldKey 
sem_FieldKey (FieldKeyBrek _expr _optField )  =
    (sem_FieldKey_FieldKeyBrek (sem_Expr _expr ) (sem_OptField _optField ) )
sem_FieldKey (FieldKeyCor _expr _optField )  =
    (sem_FieldKey_FieldKeyCor (sem_Expr _expr ) (sem_OptField _optField ) )
sem_FieldKey (FieldKeyFiel _field _optField )  =
    (sem_FieldKey_FieldKeyFiel (sem_Field _field ) (sem_OptField _optField ) )
-- semantic domain
type T_FieldKey  = ([String]) ->
                   ( String,([String]),String)
sem_FieldKey_FieldKeyBrek :: T_Expr  ->
                             T_OptField  ->
                             T_FieldKey 
sem_FieldKey_FieldKeyBrek expr_ optField_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _exprOdeclarados :: ([String])
              _optFieldOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _optFieldIboolean :: String
              _optFieldIdeclarados :: ([String])
              _optFieldIdecls :: String
              _lhsOboolean =
                  _optFieldIboolean
              _lhsOdeclarados =
                  _optFieldIdeclarados
              _lhsOdecls =
                  _optFieldIdecls
              _exprOdeclarados =
                  _lhsIdeclarados
              _optFieldOdeclarados =
                  _exprIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
              ( _optFieldIboolean,_optFieldIdeclarados,_optFieldIdecls) =
                  (optField_ _optFieldOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_FieldKey_FieldKeyCor :: T_Expr  ->
                            T_OptField  ->
                            T_FieldKey 
sem_FieldKey_FieldKeyCor expr_ optField_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _exprOdeclarados :: ([String])
              _optFieldOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _optFieldIboolean :: String
              _optFieldIdeclarados :: ([String])
              _optFieldIdecls :: String
              _lhsOboolean =
                  _optFieldIboolean
              _lhsOdeclarados =
                  _optFieldIdeclarados
              _lhsOdecls =
                  _optFieldIdecls
              _exprOdeclarados =
                  _lhsIdeclarados
              _optFieldOdeclarados =
                  _exprIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
              ( _optFieldIboolean,_optFieldIdeclarados,_optFieldIdecls) =
                  (optField_ _optFieldOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_FieldKey_FieldKeyFiel :: T_Field  ->
                             T_OptField  ->
                             T_FieldKey 
sem_FieldKey_FieldKeyFiel field_ optField_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _optFieldOdeclarados :: ([String])
              _optFieldIboolean :: String
              _optFieldIdeclarados :: ([String])
              _optFieldIdecls :: String
              _lhsOboolean =
                  _optFieldIboolean
              _lhsOdeclarados =
                  _optFieldIdeclarados
              _lhsOdecls =
                  _optFieldIdecls
              _optFieldOdeclarados =
                  _lhsIdeclarados
              ( _optFieldIboolean,_optFieldIdeclarados,_optFieldIdecls) =
                  (optField_ _optFieldOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- FieldName ---------------------------------------------------
data FieldName  = FieldName (Lidentifier) 
                deriving ( Show)
-- cata
sem_FieldName :: FieldName  ->
                 T_FieldName 
sem_FieldName (FieldName _lidentifier )  =
    (sem_FieldName_FieldName _lidentifier )
-- semantic domain
type T_FieldName  = ( )
sem_FieldName_FieldName :: Lidentifier ->
                           T_FieldName 
sem_FieldName_FieldName lidentifier_  =
    (let 
     in  ( ))
-- Fin ---------------------------------------------------------
data Fin  = EmptyFin 
          | Fin 
          deriving ( Show)
-- cata
sem_Fin :: Fin  ->
           T_Fin 
sem_Fin (EmptyFin )  =
    (sem_Fin_EmptyFin )
sem_Fin (Fin )  =
    (sem_Fin_Fin )
-- semantic domain
type T_Fin  = ( )
sem_Fin_EmptyFin :: T_Fin 
sem_Fin_EmptyFin  =
    (let 
     in  ( ))
sem_Fin_Fin :: T_Fin 
sem_Fin_Fin  =
    (let 
     in  ( ))
-- InfixOp -----------------------------------------------------
data InfixOp  = Admin 
              | And 
              | Aplica 
              | Arroba 
              | Asr 
              | Asterisc 
              | Div 
              | Dolar 
              | Equals 
              | Igual 
              | InfixOP (Infix) 
              | Load 
              | Lor 
              | Lsl 
              | Lsr 
              | Lxor 
              | Mas 
              | Mayor 
              | Menor 
              | Menos 
              | Mod 
              | Option 
              | Ors 
              | Sombrero 
              deriving ( Show)
-- cata
sem_InfixOp :: InfixOp  ->
               T_InfixOp 
sem_InfixOp (Admin )  =
    (sem_InfixOp_Admin )
sem_InfixOp (And )  =
    (sem_InfixOp_And )
sem_InfixOp (Aplica )  =
    (sem_InfixOp_Aplica )
sem_InfixOp (Arroba )  =
    (sem_InfixOp_Arroba )
sem_InfixOp (Asr )  =
    (sem_InfixOp_Asr )
sem_InfixOp (Asterisc )  =
    (sem_InfixOp_Asterisc )
sem_InfixOp (Div )  =
    (sem_InfixOp_Div )
sem_InfixOp (Dolar )  =
    (sem_InfixOp_Dolar )
sem_InfixOp (Equals )  =
    (sem_InfixOp_Equals )
sem_InfixOp (Igual )  =
    (sem_InfixOp_Igual )
sem_InfixOp (InfixOP _infix )  =
    (sem_InfixOp_InfixOP _infix )
sem_InfixOp (Load )  =
    (sem_InfixOp_Load )
sem_InfixOp (Lor )  =
    (sem_InfixOp_Lor )
sem_InfixOp (Lsl )  =
    (sem_InfixOp_Lsl )
sem_InfixOp (Lsr )  =
    (sem_InfixOp_Lsr )
sem_InfixOp (Lxor )  =
    (sem_InfixOp_Lxor )
sem_InfixOp (Mas )  =
    (sem_InfixOp_Mas )
sem_InfixOp (Mayor )  =
    (sem_InfixOp_Mayor )
sem_InfixOp (Menor )  =
    (sem_InfixOp_Menor )
sem_InfixOp (Menos )  =
    (sem_InfixOp_Menos )
sem_InfixOp (Mod )  =
    (sem_InfixOp_Mod )
sem_InfixOp (Option )  =
    (sem_InfixOp_Option )
sem_InfixOp (Ors )  =
    (sem_InfixOp_Ors )
sem_InfixOp (Sombrero )  =
    (sem_InfixOp_Sombrero )
-- semantic domain
type T_InfixOp  = ( )
sem_InfixOp_Admin :: T_InfixOp 
sem_InfixOp_Admin  =
    (let 
     in  ( ))
sem_InfixOp_And :: T_InfixOp 
sem_InfixOp_And  =
    (let 
     in  ( ))
sem_InfixOp_Aplica :: T_InfixOp 
sem_InfixOp_Aplica  =
    (let 
     in  ( ))
sem_InfixOp_Arroba :: T_InfixOp 
sem_InfixOp_Arroba  =
    (let 
     in  ( ))
sem_InfixOp_Asr :: T_InfixOp 
sem_InfixOp_Asr  =
    (let 
     in  ( ))
sem_InfixOp_Asterisc :: T_InfixOp 
sem_InfixOp_Asterisc  =
    (let 
     in  ( ))
sem_InfixOp_Div :: T_InfixOp 
sem_InfixOp_Div  =
    (let 
     in  ( ))
sem_InfixOp_Dolar :: T_InfixOp 
sem_InfixOp_Dolar  =
    (let 
     in  ( ))
sem_InfixOp_Equals :: T_InfixOp 
sem_InfixOp_Equals  =
    (let 
     in  ( ))
sem_InfixOp_Igual :: T_InfixOp 
sem_InfixOp_Igual  =
    (let 
     in  ( ))
sem_InfixOp_InfixOP :: Infix ->
                       T_InfixOp 
sem_InfixOp_InfixOP infix_  =
    (let 
     in  ( ))
sem_InfixOp_Load :: T_InfixOp 
sem_InfixOp_Load  =
    (let 
     in  ( ))
sem_InfixOp_Lor :: T_InfixOp 
sem_InfixOp_Lor  =
    (let 
     in  ( ))
sem_InfixOp_Lsl :: T_InfixOp 
sem_InfixOp_Lsl  =
    (let 
     in  ( ))
sem_InfixOp_Lsr :: T_InfixOp 
sem_InfixOp_Lsr  =
    (let 
     in  ( ))
sem_InfixOp_Lxor :: T_InfixOp 
sem_InfixOp_Lxor  =
    (let 
     in  ( ))
sem_InfixOp_Mas :: T_InfixOp 
sem_InfixOp_Mas  =
    (let 
     in  ( ))
sem_InfixOp_Mayor :: T_InfixOp 
sem_InfixOp_Mayor  =
    (let 
     in  ( ))
sem_InfixOp_Menor :: T_InfixOp 
sem_InfixOp_Menor  =
    (let 
     in  ( ))
sem_InfixOp_Menos :: T_InfixOp 
sem_InfixOp_Menos  =
    (let 
     in  ( ))
sem_InfixOp_Mod :: T_InfixOp 
sem_InfixOp_Mod  =
    (let 
     in  ( ))
sem_InfixOp_Option :: T_InfixOp 
sem_InfixOp_Option  =
    (let 
     in  ( ))
sem_InfixOp_Ors :: T_InfixOp 
sem_InfixOp_Ors  =
    (let 
     in  ( ))
sem_InfixOp_Sombrero :: T_InfixOp 
sem_InfixOp_Sombrero  =
    (let 
     in  ( ))
-- InstVar -----------------------------------------------------
data InstVar  = InstVar (InstVariantName) (Expr) 
              deriving ( Show)
-- cata
sem_InstVar :: InstVar  ->
               T_InstVar 
sem_InstVar (InstVar _instVariantName _expr )  =
    (sem_InstVar_InstVar (sem_InstVariantName _instVariantName ) (sem_Expr _expr ) )
-- semantic domain
type T_InstVar  = ([String]) ->
                  ( String,([String]),String)
sem_InstVar_InstVar :: T_InstVariantName  ->
                       T_Expr  ->
                       T_InstVar 
sem_InstVar_InstVar instVariantName_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _lhsOdecls =
                  _exprIdecls
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- InstVariantName ---------------------------------------------
data InstVariantName  = InstVariantName (Lidentifier) 
                      deriving ( Show)
-- cata
sem_InstVariantName :: InstVariantName  ->
                       T_InstVariantName 
sem_InstVariantName (InstVariantName _lidentifier )  =
    (sem_InstVariantName_InstVariantName _lidentifier )
-- semantic domain
type T_InstVariantName  = ( )
sem_InstVariantName_InstVariantName :: Lidentifier ->
                                       T_InstVariantName 
sem_InstVariantName_InstVariantName lidentifier_  =
    (let 
     in  ( ))
-- KeyOpeT -----------------------------------------------------
data KeyOpeT  = KeyBrea 
              deriving ( Show)
-- cata
sem_KeyOpeT :: KeyOpeT  ->
               T_KeyOpeT 
sem_KeyOpeT (KeyBrea )  =
    (sem_KeyOpeT_KeyBrea )
-- semantic domain
type T_KeyOpeT  = ( )
sem_KeyOpeT_KeyBrea :: T_KeyOpeT 
sem_KeyOpeT_KeyBrea  =
    (let 
     in  ( ))
-- KeyPrefi ----------------------------------------------------
data KeyPrefi  = KeycBrea 
               deriving ( Show)
-- cata
sem_KeyPrefi :: KeyPrefi  ->
                T_KeyPrefi 
sem_KeyPrefi (KeycBrea )  =
    (sem_KeyPrefi_KeycBrea )
-- semantic domain
type T_KeyPrefi  = ( )
sem_KeyPrefi_KeycBrea :: T_KeyPrefi 
sem_KeyPrefi_KeycBrea  =
    (let 
     in  ( ))
-- LabelNames --------------------------------------------------
data LabelNames  = LabelNames (Lidentifier) 
                 deriving ( Show)
-- cata
sem_LabelNames :: LabelNames  ->
                  T_LabelNames 
sem_LabelNames (LabelNames _lidentifier )  =
    (sem_LabelNames_LabelNames _lidentifier )
-- semantic domain
type T_LabelNames  = ( )
sem_LabelNames_LabelNames :: Lidentifier ->
                             T_LabelNames 
sem_LabelNames_LabelNames lidentifier_  =
    (let 
     in  ( ))
-- LetBinding --------------------------------------------------
data LetBinding  = LetBindingPat (Pattern) (Expr) 
                 deriving ( Show)
-- cata
sem_LetBinding :: LetBinding  ->
                  T_LetBinding 
sem_LetBinding (LetBindingPat _pattern _expr )  =
    (sem_LetBinding_LetBindingPat (sem_Pattern _pattern ) (sem_Expr _expr ) )
-- semantic domain
type T_LetBinding  = ([String]) ->
                     ( String,([String]),String)
sem_LetBinding_LetBindingPat :: T_Pattern  ->
                                T_Expr  ->
                                T_LetBinding 
sem_LetBinding_LetBindingPat pattern_ expr_  =
    (\ _lhsIdeclarados ->
         (let _patternOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _patternIboolean :: String
              _patternIdeclarados :: ([String])
              _patternIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _patternOdeclarados =
                  _lhsIdeclarados
              _lhsOdecls =
                  _patternIdecls ++ _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _patternIdeclarados
              ( _patternIboolean,_patternIdeclarados,_patternIdecls) =
                  (pattern_ _patternOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListArgument ------------------------------------------------
type ListArgument  = [Argument]
-- cata
sem_ListArgument :: ListArgument  ->
                    T_ListArgument 
sem_ListArgument list  =
    (Prelude.foldr sem_ListArgument_Cons sem_ListArgument_Nil (Prelude.map sem_Argument list) )
-- semantic domain
type T_ListArgument  = ([String]) ->
                       ( String,([String]),String)
sem_ListArgument_Cons :: T_Argument  ->
                         T_ListArgument  ->
                         T_ListArgument 
sem_ListArgument_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _tlIboolean
              _lhsOdeclarados =
                  _tlIdeclarados
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListArgument_Nil :: T_ListArgument 
sem_ListArgument_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListDefinicion ----------------------------------------------
type ListDefinicion  = [DefinicionList]
-- cata
sem_ListDefinicion :: ListDefinicion  ->
                      T_ListDefinicion 
sem_ListDefinicion list  =
    (Prelude.foldr sem_ListDefinicion_Cons sem_ListDefinicion_Nil (Prelude.map sem_DefinicionList list) )
-- semantic domain
type T_ListDefinicion  = ([String]) ->
                         ( String,([String]),String)
sem_ListDefinicion_Cons :: T_DefinicionList  ->
                           T_ListDefinicion  ->
                           T_ListDefinicion 
sem_ListDefinicion_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdeclarados =
                  _hdIdeclarados ++ _tlIdeclarados
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _hdIboolean ++ _tlIboolean
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListDefinicion_Nil :: T_ListDefinicion 
sem_ListDefinicion_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListExpr ----------------------------------------------------
type ListExpr  = [Expr]
-- cata
sem_ListExpr :: ListExpr  ->
                T_ListExpr 
sem_ListExpr list  =
    (Prelude.foldr sem_ListExpr_Cons sem_ListExpr_Nil (Prelude.map sem_Expr list) )
-- semantic domain
type T_ListExpr  = ([String]) ->
                   ( String,([String]),String)
sem_ListExpr_Cons :: T_Expr  ->
                     T_ListExpr  ->
                     T_ListExpr 
sem_ListExpr_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _hdIboolean ++ _tlIboolean
              _lhsOdeclarados =
                  _tlIdeclarados
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListExpr_Nil :: T_ListExpr 
sem_ListExpr_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListFieldExpr -----------------------------------------------
type ListFieldExpr  = [FieldExpr]
-- cata
sem_ListFieldExpr :: ListFieldExpr  ->
                     T_ListFieldExpr 
sem_ListFieldExpr list  =
    (Prelude.foldr sem_ListFieldExpr_Cons sem_ListFieldExpr_Nil (Prelude.map sem_FieldExpr list) )
-- semantic domain
type T_ListFieldExpr  = ([String]) ->
                        ( String,([String]),String)
sem_ListFieldExpr_Cons :: T_FieldExpr  ->
                          T_ListFieldExpr  ->
                          T_ListFieldExpr 
sem_ListFieldExpr_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _tlIboolean
              _lhsOdeclarados =
                  _tlIdeclarados
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListFieldExpr_Nil :: T_ListFieldExpr 
sem_ListFieldExpr_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListLetBinding ----------------------------------------------
type ListLetBinding  = [LetBinding]
-- cata
sem_ListLetBinding :: ListLetBinding  ->
                      T_ListLetBinding 
sem_ListLetBinding list  =
    (Prelude.foldr sem_ListLetBinding_Cons sem_ListLetBinding_Nil (Prelude.map sem_LetBinding list) )
-- semantic domain
type T_ListLetBinding  = ([String]) ->
                         ( String,([String]),String)
sem_ListLetBinding_Cons :: T_LetBinding  ->
                           T_ListLetBinding  ->
                           T_ListLetBinding 
sem_ListLetBinding_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdeclarados =
                  _hdIdeclarados ++ _tlIdeclarados
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _hdIboolean ++ _tlIboolean
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListLetBinding_Nil :: T_ListLetBinding 
sem_ListLetBinding_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListOptPattern ----------------------------------------------
type ListOptPattern  = [OptPattern]
-- cata
sem_ListOptPattern :: ListOptPattern  ->
                      T_ListOptPattern 
sem_ListOptPattern list  =
    (Prelude.foldr sem_ListOptPattern_Cons sem_ListOptPattern_Nil (Prelude.map sem_OptPattern list) )
-- semantic domain
type T_ListOptPattern  = ([String]) ->
                         ( String,([String]),String)
sem_ListOptPattern_Cons :: T_OptPattern  ->
                           T_ListOptPattern  ->
                           T_ListOptPattern 
sem_ListOptPattern_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _hdIboolean ++ _tlIboolean
              _lhsOdeclarados =
                  _tlIdeclarados
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListOptPattern_Nil :: T_ListOptPattern 
sem_ListOptPattern_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ListParameter -----------------------------------------------
type ListParameter  = [Parameter]
-- cata
sem_ListParameter :: ListParameter  ->
                     T_ListParameter 
sem_ListParameter list  =
    (Prelude.foldr sem_ListParameter_Cons sem_ListParameter_Nil (Prelude.map sem_Parameter list) )
-- semantic domain
type T_ListParameter  = ([String]) ->
                        ( String,([String]),String)
sem_ListParameter_Cons :: T_Parameter  ->
                          T_ListParameter  ->
                          T_ListParameter 
sem_ListParameter_Cons hd_ tl_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _hdOdeclarados :: ([String])
              _tlOdeclarados :: ([String])
              _hdIboolean :: String
              _hdIdeclarados :: ([String])
              _hdIdecls :: String
              _tlIboolean :: String
              _tlIdeclarados :: ([String])
              _tlIdecls :: String
              _lhsOdecls =
                  _hdIdecls ++ _tlIdecls
              _lhsOboolean =
                  _hdIboolean ++ _tlIboolean
              _lhsOdeclarados =
                  _tlIdeclarados
              _hdOdeclarados =
                  _lhsIdeclarados
              _tlOdeclarados =
                  _hdIdeclarados
              ( _hdIboolean,_hdIdeclarados,_hdIdecls) =
                  (hd_ _hdOdeclarados )
              ( _tlIboolean,_tlIdeclarados,_tlIdecls) =
                  (tl_ _tlOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ListParameter_Nil :: T_ListParameter 
sem_ListParameter_Nil  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- MethodName --------------------------------------------------
data MethodName  = MethodName (Lidentifier) 
                 deriving ( Show)
-- cata
sem_MethodName :: MethodName  ->
                  T_MethodName 
sem_MethodName (MethodName _lidentifier )  =
    (sem_MethodName_MethodName _lidentifier )
-- semantic domain
type T_MethodName  = ( )
sem_MethodName_MethodName :: Lidentifier ->
                             T_MethodName 
sem_MethodName_MethodName lidentifier_  =
    (let 
     in  ( ))
-- ModtypePath -------------------------------------------------
data ModtypePath  = ClassPathMod (ModulePath) (ClassName) 
                  | ModtypePath (ModuleTypeName) 
                  | ModtypePathExt (ExtendedModulePath) (ModuleTypeName) 
                  deriving ( Show)
-- cata
sem_ModtypePath :: ModtypePath  ->
                   T_ModtypePath 
sem_ModtypePath (ClassPathMod _modulePath _className )  =
    (sem_ModtypePath_ClassPathMod (sem_ModulePath _modulePath ) (sem_ClassName _className ) )
sem_ModtypePath (ModtypePath _moduleTypeName )  =
    (sem_ModtypePath_ModtypePath (sem_ModuleTypeName _moduleTypeName ) )
sem_ModtypePath (ModtypePathExt _extendedModulePath _moduleTypeName )  =
    (sem_ModtypePath_ModtypePathExt (sem_ExtendedModulePath _extendedModulePath ) (sem_ModuleTypeName _moduleTypeName ) )
-- semantic domain
type T_ModtypePath  = ( )
sem_ModtypePath_ClassPathMod :: T_ModulePath  ->
                                T_ClassName  ->
                                T_ModtypePath 
sem_ModtypePath_ClassPathMod modulePath_ className_  =
    (let 
     in  ( ))
sem_ModtypePath_ModtypePath :: T_ModuleTypeName  ->
                               T_ModtypePath 
sem_ModtypePath_ModtypePath moduleTypeName_  =
    (let 
     in  ( ))
sem_ModtypePath_ModtypePathExt :: T_ExtendedModulePath  ->
                                  T_ModuleTypeName  ->
                                  T_ModtypePath 
sem_ModtypePath_ModtypePathExt extendedModulePath_ moduleTypeName_  =
    (let 
     in  ( ))
-- ModuleName --------------------------------------------------
data ModuleName  = ModuleName (Uidentifier) 
                 deriving ( Show)
-- cata
sem_ModuleName :: ModuleName  ->
                  T_ModuleName 
sem_ModuleName (ModuleName _uidentifier )  =
    (sem_ModuleName_ModuleName _uidentifier )
-- semantic domain
type T_ModuleName  = ( )
sem_ModuleName_ModuleName :: Uidentifier ->
                             T_ModuleName 
sem_ModuleName_ModuleName uidentifier_  =
    (let 
     in  ( ))
-- ModulePath --------------------------------------------------
data ModulePath  = ModulePath (ModuleName) (ModulePath') 
                 deriving ( Show)
-- cata
sem_ModulePath :: ModulePath  ->
                  T_ModulePath 
sem_ModulePath (ModulePath _moduleName _modulePath' )  =
    (sem_ModulePath_ModulePath (sem_ModuleName _moduleName ) (sem_ModulePath' _modulePath' ) )
-- semantic domain
type T_ModulePath  = ( )
sem_ModulePath_ModulePath :: T_ModuleName  ->
                             (T_ModulePath')  ->
                             T_ModulePath 
sem_ModulePath_ModulePath moduleName_ modulePath'_  =
    (let 
     in  ( ))
-- ModulePath' -------------------------------------------------
data ModulePath'  = EmptyModule 
                  | ModulePath' (ModuleName) (ModulePath') 
                  deriving ( Show)
-- cata
sem_ModulePath' :: (ModulePath')  ->
                   (T_ModulePath') 
sem_ModulePath' (EmptyModule )  =
    (sem_ModulePath'_EmptyModule )
sem_ModulePath' (ModulePath' _moduleName _modulePath' )  =
    (sem_ModulePath'_ModulePath' (sem_ModuleName _moduleName ) (sem_ModulePath' _modulePath' ) )
-- semantic domain
type T_ModulePath'  = ( )
sem_ModulePath'_EmptyModule :: (T_ModulePath') 
sem_ModulePath'_EmptyModule  =
    (let 
     in  ( ))
sem_ModulePath'_ModulePath' :: T_ModuleName  ->
                               (T_ModulePath')  ->
                               (T_ModulePath') 
sem_ModulePath'_ModulePath' moduleName_ modulePath'_  =
    (let 
     in  ( ))
-- ModuleTypeName ----------------------------------------------
data ModuleTypeName  = ModuleTypeNameL (Lidentifier) 
                     | ModuleTypeNameU (Uidentifier) 
                     deriving ( Show)
-- cata
sem_ModuleTypeName :: ModuleTypeName  ->
                      T_ModuleTypeName 
sem_ModuleTypeName (ModuleTypeNameL _lidentifier )  =
    (sem_ModuleTypeName_ModuleTypeNameL _lidentifier )
sem_ModuleTypeName (ModuleTypeNameU _uidentifier )  =
    (sem_ModuleTypeName_ModuleTypeNameU _uidentifier )
-- semantic domain
type T_ModuleTypeName  = ( )
sem_ModuleTypeName_ModuleTypeNameL :: Lidentifier ->
                                      T_ModuleTypeName 
sem_ModuleTypeName_ModuleTypeNameL lidentifier_  =
    (let 
     in  ( ))
sem_ModuleTypeName_ModuleTypeNameU :: Uidentifier ->
                                      T_ModuleTypeName 
sem_ModuleTypeName_ModuleTypeNameU uidentifier_  =
    (let 
     in  ( ))
-- MultipleMatching --------------------------------------------
data MultipleMatching  = MultipleMatchingPat (ListParameter) (Expr) 
                       | MultipleMatchingPatExp (ListParameter) (Expr) 
                       deriving ( Show)
-- cata
sem_MultipleMatching :: MultipleMatching  ->
                        T_MultipleMatching 
sem_MultipleMatching (MultipleMatchingPat _listParameter _expr )  =
    (sem_MultipleMatching_MultipleMatchingPat (sem_ListParameter _listParameter ) (sem_Expr _expr ) )
sem_MultipleMatching (MultipleMatchingPatExp _listParameter _expr )  =
    (sem_MultipleMatching_MultipleMatchingPatExp (sem_ListParameter _listParameter ) (sem_Expr _expr ) )
-- semantic domain
type T_MultipleMatching  = ([String]) ->
                           ( String,([String]),String)
sem_MultipleMatching_MultipleMatchingPat :: T_ListParameter  ->
                                            T_Expr  ->
                                            T_MultipleMatching 
sem_MultipleMatching_MultipleMatchingPat listParameter_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _listParameterOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _listParameterIboolean :: String
              _listParameterIdeclarados :: ([String])
              _listParameterIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _lhsOdecls =
                  _exprIdecls
              _listParameterOdeclarados =
                  _lhsIdeclarados
              _exprOdeclarados =
                  _listParameterIdeclarados
              ( _listParameterIboolean,_listParameterIdeclarados,_listParameterIdecls) =
                  (listParameter_ _listParameterOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_MultipleMatching_MultipleMatchingPatExp :: T_ListParameter  ->
                                               T_Expr  ->
                                               T_MultipleMatching 
sem_MultipleMatching_MultipleMatchingPatExp listParameter_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _listParameterOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _listParameterIboolean :: String
              _listParameterIdeclarados :: ([String])
              _listParameterIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _lhsOdecls =
                  _exprIdecls
              _listParameterOdeclarados =
                  _lhsIdeclarados
              _exprOdeclarados =
                  _listParameterIdeclarados
              ( _listParameterIboolean,_listParameterIdeclarados,_listParameterIdecls) =
                  (listParameter_ _listParameterOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- Op ----------------------------------------------------------
data Op  = Op (String) 
         deriving ( Show)
-- cata
sem_Op :: Op  ->
          T_Op 
sem_Op (Op _ope )  =
    (sem_Op_Op _ope )
-- semantic domain
type T_Op  = ( String,String)
sem_Op_Op :: String ->
             T_Op 
sem_Op_Op ope_  =
    (let _lhsOboolean :: String
         _lhsOdecls :: String
         _lhsOboolean =
             ope_
         _lhsOdecls =
             ""
     in  ( _lhsOboolean,_lhsOdecls))
-- OperatorName ------------------------------------------------
data OperatorName  = OperatorInfix (InfixOp) 
                   | OperatorPrefix (Prefix) 
                   deriving ( Show)
-- cata
sem_OperatorName :: OperatorName  ->
                    T_OperatorName 
sem_OperatorName (OperatorInfix _infixOp )  =
    (sem_OperatorName_OperatorInfix (sem_InfixOp _infixOp ) )
sem_OperatorName (OperatorPrefix _prefix )  =
    (sem_OperatorName_OperatorPrefix _prefix )
-- semantic domain
type T_OperatorName  = ( )
sem_OperatorName_OperatorInfix :: T_InfixOp  ->
                                  T_OperatorName 
sem_OperatorName_OperatorInfix infixOp_  =
    (let 
     in  ( ))
sem_OperatorName_OperatorPrefix :: Prefix ->
                                   T_OperatorName 
sem_OperatorName_OperatorPrefix prefix_  =
    (let 
     in  ( ))
-- OptField ----------------------------------------------------
data OptField  = EmptyOpt 
               | OptField (Expr) 
               deriving ( Show)
-- cata
sem_OptField :: OptField  ->
                T_OptField 
sem_OptField (EmptyOpt )  =
    (sem_OptField_EmptyOpt )
sem_OptField (OptField _expr )  =
    (sem_OptField_OptField (sem_Expr _expr ) )
-- semantic domain
type T_OptField  = ([String]) ->
                   ( String,([String]),String)
sem_OptField_EmptyOpt :: T_OptField 
sem_OptField_EmptyOpt  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_OptField_OptField :: T_Expr  ->
                         T_OptField 
sem_OptField_OptField expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- OptPattern --------------------------------------------------
data OptPattern  = OptPattern (Pattern) (WhenKey) (Expr) 
                 deriving ( Show)
-- cata
sem_OptPattern :: OptPattern  ->
                  T_OptPattern 
sem_OptPattern (OptPattern _pattern _whenKey _expr )  =
    (sem_OptPattern_OptPattern (sem_Pattern _pattern ) (sem_WhenKey _whenKey ) (sem_Expr _expr ) )
-- semantic domain
type T_OptPattern  = ([String]) ->
                     ( String,([String]),String)
sem_OptPattern_OptPattern :: T_Pattern  ->
                             T_WhenKey  ->
                             T_Expr  ->
                             T_OptPattern 
sem_OptPattern_OptPattern pattern_ whenKey_ expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _patternOdeclarados :: ([String])
              _whenKeyOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _patternIboolean :: String
              _patternIdeclarados :: ([String])
              _patternIdecls :: String
              _whenKeyIboolean :: String
              _whenKeyIdeclarados :: ([String])
              _whenKeyIdecls :: String
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  berificar  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _patternOdeclarados =
                  _lhsIdeclarados
              _whenKeyOdeclarados =
                  _patternIdeclarados
              _exprOdeclarados =
                  _whenKeyIdeclarados
              ( _patternIboolean,_patternIdeclarados,_patternIdecls) =
                  (pattern_ _patternOdeclarados )
              ( _whenKeyIboolean,_whenKeyIdeclarados,_whenKeyIdecls) =
                  (whenKey_ _whenKeyOdeclarados )
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- Parameter ---------------------------------------------------
data Parameter  = ParameterPattern (Pattern) 
                deriving ( Show)
-- cata
sem_Parameter :: Parameter  ->
                 T_Parameter 
sem_Parameter (ParameterPattern _pattern )  =
    (sem_Parameter_ParameterPattern (sem_Pattern _pattern ) )
-- semantic domain
type T_Parameter  = ([String]) ->
                    ( String,([String]),String)
sem_Parameter_ParameterPattern :: T_Pattern  ->
                                  T_Parameter 
sem_Parameter_ParameterPattern pattern_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _patternOdeclarados :: ([String])
              _patternIboolean :: String
              _patternIdeclarados :: ([String])
              _patternIdecls :: String
              _lhsOboolean =
                  _patternIboolean
              _lhsOdeclarados =
                  _patternIdeclarados
              _lhsOdecls =
                  _patternIdecls
              _patternOdeclarados =
                  _lhsIdeclarados
              ( _patternIboolean,_patternIdeclarados,_patternIdecls) =
                  (pattern_ _patternOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- Pattern -----------------------------------------------------
data Pattern  = PatternCons (Constant) 
              | PatternValue (ValueName) 
              deriving ( Show)
-- cata
sem_Pattern :: Pattern  ->
               T_Pattern 
sem_Pattern (PatternCons _constant )  =
    (sem_Pattern_PatternCons (sem_Constant _constant ) )
sem_Pattern (PatternValue _valueName )  =
    (sem_Pattern_PatternValue (sem_ValueName _valueName ) )
-- semantic domain
type T_Pattern  = ([String]) ->
                  ( String,([String]),String)
sem_Pattern_PatternCons :: T_Constant  ->
                           T_Pattern 
sem_Pattern_PatternCons constant_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _constantItipo :: String
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
              ( _constantItipo) =
                  (constant_ )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_Pattern_PatternValue :: T_ValueName  ->
                            T_Pattern 
sem_Pattern_PatternValue valueName_  =
    (\ _lhsIdeclarados ->
         (let _valueNameOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _valueNameIboolean :: String
              _valueNameIdeclarados :: ([String])
              _valueNameIdecls :: String
              _valueNameOdeclarados =
                  _lhsIdeclarados
              _lhsOdecls =
                  _valueNameIdecls
              _lhsOboolean =
                  _valueNameIboolean
              _lhsOdeclarados =
                  _valueNameIdeclarados
              ( _valueNameIboolean,_valueNameIdeclarados,_valueNameIdecls) =
                  (valueName_ _valueNameOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- PatternMatching ---------------------------------------------
data PatternMatching  = PatternMatching (ListOptPattern) 
                      deriving ( Show)
-- cata
sem_PatternMatching :: PatternMatching  ->
                       T_PatternMatching 
sem_PatternMatching (PatternMatching _listOptPattern )  =
    (sem_PatternMatching_PatternMatching (sem_ListOptPattern _listOptPattern ) )
-- semantic domain
type T_PatternMatching  = ([String]) ->
                          ( String,([String]),String)
sem_PatternMatching_PatternMatching :: T_ListOptPattern  ->
                                       T_PatternMatching 
sem_PatternMatching_PatternMatching listOptPattern_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _listOptPatternOdeclarados :: ([String])
              _listOptPatternIboolean :: String
              _listOptPatternIdeclarados :: ([String])
              _listOptPatternIdecls :: String
              _lhsOdecls =
                  _listOptPatternIdecls
              _lhsOboolean =
                  _listOptPatternIboolean
              _lhsOdeclarados =
                  _listOptPatternIdeclarados
              _listOptPatternOdeclarados =
                  _lhsIdeclarados
              ( _listOptPatternIboolean,_listOptPatternIdeclarados,_listOptPatternIdecls) =
                  (listOptPattern_ _listOptPatternOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- Raiz --------------------------------------------------------
data Raiz  = Raiz (Cuerpo) 
           deriving ( Show)
-- cata
sem_Raiz :: Raiz  ->
            T_Raiz 
sem_Raiz (Raiz _cuerpo )  =
    (sem_Raiz_Raiz (sem_Cuerpo _cuerpo ) )
-- semantic domain
type T_Raiz  = ( (IO ()))
sem_Raiz_Raiz :: T_Cuerpo  ->
                 T_Raiz 
sem_Raiz_Raiz cuerpo_  =
    (let _lhsOsalida :: (IO ())
         _cuerpoIboolean :: String
         _cuerpoIdecls :: String
         _lhsOsalida =
             putStr _cuerpoIdecls
         ( _cuerpoIboolean,_cuerpoIdecls) =
             (cuerpo_ )
     in  ( _lhsOsalida))
-- Rec ---------------------------------------------------------
data Rec  = EmptyRec 
          | Rec 
          deriving ( Show)
-- cata
sem_Rec :: Rec  ->
           T_Rec 
sem_Rec (EmptyRec )  =
    (sem_Rec_EmptyRec )
sem_Rec (Rec )  =
    (sem_Rec_Rec )
-- semantic domain
type T_Rec  = ( )
sem_Rec_EmptyRec :: T_Rec 
sem_Rec_EmptyRec  =
    (let 
     in  ( ))
sem_Rec_Rec :: T_Rec 
sem_Rec_Rec  =
    (let 
     in  ( ))
-- TagName -----------------------------------------------------
data TagName  = TagName (Uidentifier) 
              deriving ( Show)
-- cata
sem_TagName :: TagName  ->
               T_TagName 
sem_TagName (TagName _uidentifier )  =
    (sem_TagName_TagName _uidentifier )
-- semantic domain
type T_TagName  = ( )
sem_TagName_TagName :: Uidentifier ->
                       T_TagName 
sem_TagName_TagName uidentifier_  =
    (let 
     in  ( ))
-- ToDow -------------------------------------------------------
data ToDow  = Downto 
            | To 
            deriving ( Show)
-- cata
sem_ToDow :: ToDow  ->
             T_ToDow 
sem_ToDow (Downto )  =
    (sem_ToDow_Downto )
sem_ToDow (To )  =
    (sem_ToDow_To )
-- semantic domain
type T_ToDow  = ( )
sem_ToDow_Downto :: T_ToDow 
sem_ToDow_Downto  =
    (let 
     in  ( ))
sem_ToDow_To :: T_ToDow 
sem_ToDow_To  =
    (let 
     in  ( ))
-- TypeConstrName ----------------------------------------------
data TypeConstrName  = TypeConstrName (Lidentifier) 
                     deriving ( Show)
-- cata
sem_TypeConstrName :: TypeConstrName  ->
                      T_TypeConstrName 
sem_TypeConstrName (TypeConstrName _lidentifier )  =
    (sem_TypeConstrName_TypeConstrName _lidentifier )
-- semantic domain
type T_TypeConstrName  = ( )
sem_TypeConstrName_TypeConstrName :: Lidentifier ->
                                     T_TypeConstrName 
sem_TypeConstrName_TypeConstrName lidentifier_  =
    (let 
     in  ( ))
-- Typeconstr --------------------------------------------------
data Typeconstr  = Typeconstr (TypeConstrName) 
                 | TypeconstrExt (ExtendedModulePath) (TypeConstrName) 
                 deriving ( Show)
-- cata
sem_Typeconstr :: Typeconstr  ->
                  T_Typeconstr 
sem_Typeconstr (Typeconstr _typeConstrName )  =
    (sem_Typeconstr_Typeconstr (sem_TypeConstrName _typeConstrName ) )
sem_Typeconstr (TypeconstrExt _extendedModulePath _typeConstrName )  =
    (sem_Typeconstr_TypeconstrExt (sem_ExtendedModulePath _extendedModulePath ) (sem_TypeConstrName _typeConstrName ) )
-- semantic domain
type T_Typeconstr  = ( )
sem_Typeconstr_Typeconstr :: T_TypeConstrName  ->
                             T_Typeconstr 
sem_Typeconstr_Typeconstr typeConstrName_  =
    (let 
     in  ( ))
sem_Typeconstr_TypeconstrExt :: T_ExtendedModulePath  ->
                                T_TypeConstrName  ->
                                T_Typeconstr 
sem_Typeconstr_TypeconstrExt extendedModulePath_ typeConstrName_  =
    (let 
     in  ( ))
-- ValueName ---------------------------------------------------
data ValueName  = ValueName (Lidentifier) 
                | ValueNameFix (OperatorName) 
                deriving ( Show)
-- cata
sem_ValueName :: ValueName  ->
                 T_ValueName 
sem_ValueName (ValueName _lidentifier )  =
    (sem_ValueName_ValueName _lidentifier )
sem_ValueName (ValueNameFix _operatorName )  =
    (sem_ValueName_ValueNameFix (sem_OperatorName _operatorName ) )
-- semantic domain
type T_ValueName  = ([String]) ->
                    ( String,([String]),String)
sem_ValueName_ValueName :: Lidentifier ->
                           T_ValueName 
sem_ValueName_ValueName lidentifier_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados =
                  cargar  lidentifier_ _lhsIdeclarados
              _lhsOdecls =
                  errorDecl lidentifier_ _lhsIdeclarados
              _lhsOboolean =
                  ""
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ValueName_ValueNameFix :: T_OperatorName  ->
                              T_ValueName 
sem_ValueName_ValueNameFix operatorName_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- ValuePath ---------------------------------------------------
data ValuePath  = ValuePath (ValueName) 
                | ValuePathMod (ModulePath) (ValueName) 
                deriving ( Show)
-- cata
sem_ValuePath :: ValuePath  ->
                 T_ValuePath 
sem_ValuePath (ValuePath _valueName )  =
    (sem_ValuePath_ValuePath (sem_ValueName _valueName ) )
sem_ValuePath (ValuePathMod _modulePath _valueName )  =
    (sem_ValuePath_ValuePathMod (sem_ModulePath _modulePath ) (sem_ValueName _valueName ) )
-- semantic domain
type T_ValuePath  = ([String]) ->
                    ( String,([String]),String)
sem_ValuePath_ValuePath :: T_ValueName  ->
                           T_ValuePath 
sem_ValuePath_ValuePath valueName_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _valueNameOdeclarados :: ([String])
              _valueNameIboolean :: String
              _valueNameIdeclarados :: ([String])
              _valueNameIdecls :: String
              _lhsOdecls =
                  _valueNameIdecls
              _lhsOboolean =
                  _valueNameIboolean
              _lhsOdeclarados =
                  _valueNameIdeclarados
              _valueNameOdeclarados =
                  _lhsIdeclarados
              ( _valueNameIboolean,_valueNameIdeclarados,_valueNameIdecls) =
                  (valueName_ _valueNameOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_ValuePath_ValuePathMod :: T_ModulePath  ->
                              T_ValueName  ->
                              T_ValuePath 
sem_ValuePath_ValuePathMod modulePath_ valueName_  =
    (\ _lhsIdeclarados ->
         (let _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls :: String
              _valueNameOdeclarados :: ([String])
              _valueNameIboolean :: String
              _valueNameIdeclarados :: ([String])
              _valueNameIdecls :: String
              _lhsOboolean =
                  _valueNameIboolean
              _lhsOdeclarados =
                  _valueNameIdeclarados
              _lhsOdecls =
                  _valueNameIdecls
              _valueNameOdeclarados =
                  _lhsIdeclarados
              ( _valueNameIboolean,_valueNameIdeclarados,_valueNameIdecls) =
                  (valueName_ _valueNameOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
-- WhenKey -----------------------------------------------------
data WhenKey  = When (Expr) 
              | WhenKey 
              deriving ( Show)
-- cata
sem_WhenKey :: WhenKey  ->
               T_WhenKey 
sem_WhenKey (When _expr )  =
    (sem_WhenKey_When (sem_Expr _expr ) )
sem_WhenKey (WhenKey )  =
    (sem_WhenKey_WhenKey )
-- semantic domain
type T_WhenKey  = ([String]) ->
                  ( String,([String]),String)
sem_WhenKey_When :: T_Expr  ->
                    T_WhenKey 
sem_WhenKey_When expr_  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _exprOdeclarados :: ([String])
              _exprIboolean :: String
              _exprIdeclarados :: ([String])
              _exprIdecls :: String
              _lhsOdecls =
                  _exprIdecls
              _lhsOboolean =
                  _exprIboolean
              _lhsOdeclarados =
                  _exprIdeclarados
              _exprOdeclarados =
                  _lhsIdeclarados
              ( _exprIboolean,_exprIdeclarados,_exprIdecls) =
                  (expr_ _exprOdeclarados )
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))
sem_WhenKey_WhenKey :: T_WhenKey 
sem_WhenKey_WhenKey  =
    (\ _lhsIdeclarados ->
         (let _lhsOdecls :: String
              _lhsOboolean :: String
              _lhsOdeclarados :: ([String])
              _lhsOdecls =
                  ""
              _lhsOboolean =
                  ""
              _lhsOdeclarados =
                  _lhsIdeclarados
          in  ( _lhsOboolean,_lhsOdeclarados,_lhsOdecls)))