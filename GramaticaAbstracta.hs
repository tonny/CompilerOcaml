module GramaticaAbstracta where
{-
===================================================================================================
   Autor   -> Antonio Mamani Quispe
   Nombre  -> Gramatica Abstracta Ocaml 
   Version -> 0.9
===================================================================================================
-}
data Raiz = Raiz Cuerpo
  deriving Show

data Cuerpo = Cuerpo ListDefinicion 
 deriving Show

type ListDefinicion = [DefinicionList]

data DefinicionList = DefinicionList Definicion Fin
 deriving Show

data Fin 
 = Fin 
 | EmptyFin
 deriving Show

data Definicion 
 = DefinicionLet Rec ListLetBinding
 deriving Show

data Rec 
 = Rec 
 | EmptyRec
 deriving Show

type ListLetBinding = [LetBinding]


data LetBinding 
 = LetBindingPat Pattern Expr
 deriving Show

---------------------------------------------------------------------------------------------------
----------------------------------------- Pattern -------------------------------------------------
---------------------------------------------------------------------------------------------------

data Pattern 
 = PatternValue ValueName
 | PatternCons  Constant 
 deriving Show
{-
---------------------------------------------------------------------------------------------------
----------------------------------------- Constants -----------------------------------------------
---------------------------------------------------------------------------------------------------
-}
data Constant = ConstantInteger
              | ConstantFloat
              | ConstantChar
              | ConstantString
              | ConstantConstr  Constr
              | ConstantFalse
              | ConstantTrue
              | ConstantConrch
              | ConstantParent
              | ConstantTag     TagName
 deriving Show

{-
----------------------------------------- NAME ----------------------------------------------------
---------------------------------------------------------------------------------------------------
-}

type Lidentifier = String
type Uidentifier = String
type Prefix      = String
type Infix       = String

{-
------------------------------------ Naming objects -----------------------------------------------
-}

data ValueName  = ValueName     Lidentifier  
                | ValueNameFix  OperatorName
            deriving Show

data OperatorName = OperatorPrefix Prefix
                  | OperatorInfix  InfixOp
          deriving Show

data Op
 = Op String
 deriving Show

data InfixOp = InfixOP  Infix
             | Admin
             | Aplica
             | Dolar
             | Option
             | Sombrero
             | Arroba
             | Menos
             | Mayor
             | Menor
             | Div
             | Mas
             | Asterisc
             | Igual
             | Ors
             | And
             | Equals
             | Mod
             | Load
             | Lor
             | Lxor
             | Lsl
             | Lsr
             | Asr
 deriving Show

data ConstructorName =  ConstructorName Uidentifier
            deriving Show

data LabelNames      =  LabelNames      Lidentifier
             deriving Show

data TagName         =  TagName         Uidentifier
           deriving Show

data TypeConstrName  =  TypeConstrName  Lidentifier
                deriving Show

data FieldName       =  FieldName       Lidentifier
              deriving Show

data ModuleName      =  ModuleName      Uidentifier
               deriving Show

data ModuleTypeName  =  ModuleTypeNameL Lidentifier
                     | ModuleTypeNameU  Uidentifier
                 deriving Show

data ClassName       =  ClassName       Lidentifier
                 deriving Show

data MethodName      =  MethodName      Lidentifier
              deriving Show

data InstVariantName =  InstVariantName Lidentifier
                deriving Show
---------------------------------------------------------------------------------------------------
----------------------------------------- Expressions ---------------------------------------------
---------------------------------------------------------------------------------------------------

data Expr
    = ExprInfixOP     Op               Expr       Expr
    | ExprValuePath   ValuePath
    | ExprConstant    Constant 
    | ExprGrup        Expr             KeyOpeT
    | ExprBegin       Expr
    | ExprExpr        ListExpr 
    | ExprConstr      Constr           Expr
    | ExprTagName     TagName          Expr
    | ExprCostLista   Expr          Expr
    | ExprLista       ListExpr
    | ExprArray       ListExpr
    | ExprField       ListFieldExpr
    | ExprWhit        Expr             ListFieldExpr
    | ExprArgument    ListArgument     Expr     
    | ExprPrefixSym   Expr
    | ExprFieldKey    FieldKey         Expr
    | ExprIf          Expr             Expr        Else 
    | ExprWhile       Expr       Expr 
    | ExprFor         ValueName        Expr        ToDow   Expr  Expr
    | ExprExpr2       Expr        Expr
    | ExprMatch       Expr             PatternMatching
    | ExprFunction    PatternMatching
    | ExprFun         MultipleMatching
    | ExprTry         Expr             PatternMatching
    | ExprLet         Rec           ListLetBinding   Expr
    | ExprMethodName  MethodName       Expr
    | ExprInstVarName InstVariantName  ExpreFlec
    | ExprAssert      Expr
    | ExprLazy        Expr
 deriving Show
{-    | ExprClassPath   ClassPath -}
{-    | ExprTypexpr    Expr       Typexpr -}
{-    | ExprClassBody  ClassBody -}


data ExpreFlec 
 = ExpreFlec Expr 
 deriving Show

type ListExpr = [Expr]

type ListFieldExpr = [FieldExpr]

data KeyOpeT 
 = KeyBrea
 deriving Show
-- | KeyOper Typexpr KeyPrefi 
-- | KeyInfi Typexpr

data KeyPrefi 
 = KeycBrea
-- | KeyInfix Typexpr
 deriving Show

data PatternMatching 
 = PatternMatching ListOptPattern  
 deriving Show

type ListOptPattern = [OptPattern]

data OptPattern 
 = OptPattern Pattern WhenKey Expr
 deriving Show

data WhenKey 
 = WhenKey
 | When    Expr
 deriving Show

data MultipleMatching 
 = MultipleMatchingPat    ListParameter Expr
 | MultipleMatchingPatExp ListParameter ListExpr
 deriving Show

type ListParameter = [Parameter]


data EqualOper 
 = EqualExp
-- | EqualOper Typexpr
 deriving Show

data ToDow 
 = To
 | Downto
 deriving Show

data FieldExpr 
 = FieldExpr Field Expr
 deriving Show

data Else 
 = Else Expr
 | EmptyElse
 deriving Show

data InstVar 
 = InstVar InstVariantName Expr
 deriving Show

data FieldKey 
 = FieldKeyFiel Field  OptField     
 | FieldKeyBrek Expr   OptField
 | FieldKeyCor  Expr   OptField
 deriving Show

data OptField 
 = OptField Expr
 | EmptyOpt
 deriving Show

data Argument 
 = ArgumentExp Expr
 deriving Show

type ListArgument = [Argument]

data Parameter 
 = ParameterPattern Pattern
 deriving Show

{-
----------------------------- Referring to named objects ------------------------------------------
-}
 
data ValuePath = ValuePath     ValueName
               | ValuePathMod ModulePath ValueName
         deriving Show

data Constr = Constr       ConstructorName
            | ConstrModule ModulePath ConstructorName
        deriving Show

data Typeconstr =  Typeconstr   TypeConstrName 
                | TypeconstrExt ExtendedModulePath TypeConstrName
            deriving Show

data Field =  Field      FieldName
           | FieldModule ModulePath FieldName
    deriving Show

data ModulePath  = ModulePath  ModuleName ModulePath'
           deriving Show

data ModulePath' = ModulePath' ModuleName ModulePath'
                 | EmptyModule
           deriving Show

data ExtendedModulePath  =  ExtendedModulePath ModuleName ExtendedModulePath'
               deriving Show

data ExtendedModulePath' = ExtendedModulePath' ModuleName ExtendedModulePath'
                         | ExtendedModulePath'Ext ExtendedModulePath ExtendedModulePath'
                         | EmptyExMoPath
                 deriving Show

data ClassPath = ClassPath    ClassName
            deriving Show

data ModtypePath = ModtypePath    ModuleTypeName
                 | ModtypePathExt ExtendedModulePath ModuleTypeName
                 | ClassPathMod ModulePath ClassName
            deriving Show
