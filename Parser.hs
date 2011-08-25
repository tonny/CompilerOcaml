module Parser where
import UU.Parsing
import Scanner
import CondicionContexto
--import GramaticaAbstracta
{-
===================================================================================================
   Autor   -> Antonio Mamani Quispe
   Nombre  -> Main 
   Version -> 0.9
===================================================================================================
-}

pRaiz = sem_Raiz_Raiz <$> pCuerpo

pCuerpo = sem_Cuerpo_Cuerpo <$> pFoldr (sem_ListDefinicion_Cons,sem_ListDefinicion_Nil) pDefinicionList

pDefinicionList = sem_DefinicionList_DefinicionList <$> pDefinicion <*> ((pFin) `opt` sem_Fin_EmptyFin)

pFin = sem_Fin_Fin <$ pKeyword ";;"
        
pDefinicion = sem_Definicion_DefinicionLet <$ pKeyword "let" <*> ((pRec) `opt` sem_Rec_EmptyRec)
                                           <*> pFoldr1Sep_ng (sem_ListLetBinding_Cons,sem_ListLetBinding_Nil) (pKeyword "and") pLetBinding
         --  <|> sem_Definition_DefinitionType <$  pKeyword "type" <*> pModuleTypeName  <*> pOptModType

pRec = sem_Rec_Rec <$ pKeyword "rec" 

pLetBinding =  sem_LetBinding_LetBindingPat <$> pPattern <* pOperator "=" <*> pExpr 

{-

pOptModType = OptModType <$ pOperator "=" <* pkeyword "{" <*> pOptMutable <* pkeyword "}"
           <|> pSucceed EmptyModType

pOptMutable =  OptMutable      <$  pKeyword "mutable" <*> pOptInstVir
           <|> OptMutableInstr <$> pInstVariantName   <*> pOptType
           <|> OptMutableVirt  <$  pKeyword "virtual" <*> pInstVariantName <*  pOperator ":" <*> pTypexpr

pOptInstVir =  OptInstVar <$> pInstVariantName   <*> pOptType
           <|> OptInstVir <$  pKeyword "virtual" <*> pInstVariantName <* pOperator ":" <*> pTypexpr

pOptType =  OptType    <$  pOperator ":" <*> pTypexpr <* pOperator "=" <*> pExpr <* pkeyword ";"
        <|> OptTypeExp <$  pOperator "=" <*> pExpr    <* pkeyword ";"

-}
---------------------------------------------------------------------------------------------------
----------------------------------------- Pattern -------------------------------------------------
---------------------------------------------------------------------------------------------------

pPattern =  sem_Pattern_PatternValue <$> pValueName
        <|> sem_Pattern_PatternCons  <$> pConstant 

---------------------------------------------------------------------------------------------------
----------------------------------------- Constants -----------------------------------------------
---------------------------------------------------------------------------------------------------

pConstant =  sem_Constant_ConstantInteger <$>  pInteger
         <|> sem_Constant_ConstantFloat   <$  pFloatingPonit
         <|> sem_Constant_ConstantChar    <$  pChar
         <|> sem_Constant_ConstantString  <$  pString
         <|> sem_Constant_ConstantConstr  <$> pConstr
         <|> sem_Constant_ConstantFalse   <$  pKeyword "false"
         <|> sem_Constant_ConstantTrue    <$  pKeyword "true"
         <|> sem_Constant_ConstantConrch  <$  pKeyword "["     <*  pKeyword "]"
         <|> sem_Constant_ConstantParent  <$  pKeyword "("     <*  pKeyword ")"
         <|> sem_Constant_ConstantTag     <$  pKeyword "'"     <*> pTagName

------------------------------------ Naming objects -----------------------------------------------

pValueName  =  sem_ValueName_ValueName     <$> pLidentifier  
           <|> sem_ValueName_ValueNameFix  <$  pKeyword "(" <*> pOperatorName <* pKeyword ")" 

pOperatorName =  sem_OperatorName_OperatorPrefix  <$> pPrefix 
             <|> sem_OperatorName_OperatorInfix   <$> pInfixOp
              
pInfixOp =  sem_InfixOp_InfixOP   <$> pInfix
        <|> sem_InfixOp_Admin     <$ pOperator "!"
        <|> sem_InfixOp_Aplica    <$ pKeyword "->"
        <|> sem_InfixOp_Dolar     <$ pOperator "$"
        <|> sem_InfixOp_Option    <$ pOperator "|"
        <|> sem_InfixOp_Sombrero  <$ pOperator "^"
        <|> sem_InfixOp_Arroba    <$ pOperator "@"
        <|> sem_InfixOp_Menos     <$ pOperator "-"
        <|> sem_InfixOp_Menor     <$ pOperator "<"
        <|> sem_InfixOp_Mayor     <$ pOperator ">"
        <|> sem_InfixOp_Div       <$ pOperator "/"         
        <|> sem_InfixOp_Mas       <$ pOperator "+"
        <|> sem_InfixOp_Asterisc  <$ pOperator "*"
        <|> sem_InfixOp_Igual     <$ pOperator "="
        <|> sem_InfixOp_Ors       <$ pKeyword  "or" 
        <|> sem_InfixOp_And       <$ pOperator "&"
        <|> sem_InfixOp_Equals    <$ pKeyword ":=" 
        <|> sem_InfixOp_Mod       <$ pKeyword "mod"
        <|> sem_InfixOp_Load      <$ pKeyword "land"
        <|> sem_InfixOp_Lor       <$ pKeyword "lor"
        <|> sem_InfixOp_Lxor      <$ pKeyword "lxor"
        <|> sem_InfixOp_Lsl       <$ pKeyword "lsl"
        <|> sem_InfixOp_Lsr       <$ pKeyword "lsr"
        <|> sem_InfixOp_Asr       <$ pKeyword "asr"

pConstructorName =  sem_ConstructorName_ConstructorName <$> pUidentifier
pLabelNames      =  sem_LabelNames_LabelNames           <$> pLidentifier
pTagName         =  sem_TagName_TagName                 <$> pUidentifier
pTypeConstrName  =  sem_TypeConstrName_TypeConstrName   <$> pLidentifier
pFieldName       =  sem_FieldName_FieldName             <$> pLidentifier
pModuleName      =  sem_ModuleName_ModuleName           <$> pUidentifier
pModuleTypeName  =  sem_ModuleTypeName_ModuleTypeNameL  <$> pLidentifier
                <|> sem_ModuleTypeName_ModuleTypeNameU  <$> pUidentifier
pClassName       =  sem_ClassName_ClassName             <$> pLidentifier
pMethodName      =  sem_MethodName_MethodName           <$> pLidentifier
pInstVariantName =  sem_InstVariantName_InstVariantName <$> pLidentifier

---------------------------------------------------------------------------------------------------
----------------------------------------- Expressions ---------------------------------------------
---------------------------------------------------------------------------------------------------

pExpr = pRelacion

pRelacion = pChainl (((sem_Expr_ExprInfixOP).(sem_Op_Op)) <$> (pOperator "<"  <|> pOperator ">"  <|> pOperator "!" <|> pOperator "^"
                                                                              <|> pOperator "@" <|> pOperator "=")) pSumPrioridad
-- <|> pInfix "<=" <|> pInfix ">=" <|> pInfix "=="  <|> pInfix "<>" <|> pInfix "**" <|> pInfix "+."<|> pInfix "-." <|> pInfix "*."<|> pInfix "/."

pSumPrioridad  = pChainl (((sem_Expr_ExprInfixOP).(sem_Op_Op)) <$> (pOperator "+" <|> pOperator "-" )) pMultPrioridad

pMultPrioridad = pChainl (((sem_Expr_ExprInfixOP).(sem_Op_Op)) <$> (pOperator "*" <|> pOperator "/" )) pFactor

pFactor = sem_Expr_ExprValuePath <$> pValuePath        
      <|> sem_Expr_ExprConstant  <$> pConstant
      <|> sem_Expr_ExprGrup      <$  pKeyword  "("      <*> pExpr         <*> pKeyOpeT  
      <|> sem_Expr_ExprBegin     <$  pKeyword  "begin"  <*> pExpr         <*  pKeyword "end" 
--      <|> sem_Expr_ExprExpr       ListExpr 
      <|> sem_Expr_ExprConstr    <$> pConstr            <*> pExpr
      <|> sem_Expr_ExprTagName   <$  pKeyword "'"       <*> pTagName      <*> pExpr
      <|> sem_Expr_ExprCostLista <$  pKeyword "::"      <*> pExpr         <*> pExpr       
      <|> sem_Expr_ExprLista     <$  pKeyword "["       <*> pFoldr1Sep_ng (sem_ListExpr_Cons,sem_ListExpr_Nil) (pKeyword ";") pExpr <* pKeyword "]"
--      <|> sem_Expr_ExprField     <$  pKeyword "{"       <*> pFoldr1Sep_ng (sem_ListFieldExpr_Cons,sem_ListFieldExpr_Nil) (pKeyword ";") pFieldExpr 
--                                 <* pKeyword "}" 
      <|> sem_Expr_ExprWhit     <$ pKeyword "{"         <*> pExpr <* pKeyword "with" 
                                <*> pFoldr1Sep_ng (sem_ListFieldExpr_Cons,sem_ListFieldExpr_Nil) (pKeyword ";") pFieldExpr <* pKeyword "}"
--     <|> sem_Expr_ExprArgument  <$> pFoldr1 (sem_ListArgument_Cons,sem_ListArgument_Nil) pArgument <*> pExpr   
     <|> sem_Expr_ExprPrefixSym <$  pPrefix            <*> pExpr
     <|> sem_Expr_ExprIf        <$  pKeyword "if"      <*> pExpr      <* pKeyword "then" <*> pExpr <*> pElse
     <|> sem_Expr_ExprWhile     <$  pKeyword "while"   <*> pExpr      <* pKeyword "do"   <*> pExpr <* pKeyword "done" 
     <|> sem_Expr_ExprFor       <$  pKeyword "for"     <*> pValueName <* pOperator "="   <*> pExpr <*> pToDow  <*> pExpr <* pKeyword "do" 
                                                       <*> pExpr      <* pKeyword "done"
--     <|> sem_Expr_ExprField     <$  pOperator "."      <*> pFieldKey  <*> pExpr
     <|> sem_Expr_ExprMatch     <$  pKeyword "match"   <*> pExpr      <* pKeyword "with" <*> pPatternMatching 
     <|> sem_Expr_ExprFunction  <$  pKeyword "function"<*> pPatternMatching  
     <|> sem_Expr_ExprFun       <$  pKeyword "fun"     <*> pMultipleMatching 
     <|> sem_Expr_ExprTry       <$  pKeyword "try"     <*> pExpr         <* pKeyword "with" <*> pPatternMatching 
     <|> sem_Expr_ExprLet       <$  pKeyword "let"     <*> pRec <*> pFoldr1Sep_ng (sem_ListLetBinding_Cons,sem_ListLetBinding_Nil) (pKeyword "and") 
                                pLetBinding   <* pKeyword "in" <*> pExpr
--     <|> sem_Expr_ExprClassPath <$  pKeyword "new"     <*> pClassPath  
--     <|> sem_Expr_ExprClassBody <$  pKeyword "object"  <*> pClassBody    <* pKeyword "end"
     <|> sem_Expr_ExprMethodName <$  pKeyword "#"  <*> pMethodName  <*> pExpr
     <|> sem_Expr_ExprInstVarName<$> pInstVariantName   <*> pExpreFlec
     <|> sem_Expr_ExprAssert     <$  pKeyword "assert"  <*> pExpr    
     <|> sem_Expr_ExprLazy       <$  pKeyword "lazy"    <*> pExpr 


pExpreFlec =  sem_ExpreFlec_ExpreFlec <$  pKeyword "<-" <*> pExpr 


pKeyOpeT =  sem_KeyOpeT_KeyBrea  <$  pKeyword ")"
--        <|> KeyOper  <$  pOperator ":"   <*> pTypexpr <*> pKeyPrefi 
--        <|> KeyInfi  <$  pKeyword  ":>" <*> pTypexpr <* pKeyword ")" 

pKeyPrefi =  sem_KeyPrefi_KeycBrea <$ pKeyword ")"
--         <|> KeyInfix <$ pKeyword ":>" <*> pTypexpr <* pKeyword ")" 


pPatternMatching = sem_PatternMatching_PatternMatching <$> pFoldr1Sep_ng (sem_ListOptPattern_Cons,sem_ListOptPattern_Nil) (pOperator "|") pOptPattern 

pOptPattern =  sem_OptPattern_OptPattern   <$> pPattern <*> pWhenKey <*> pExpr

pWhenKey =  sem_WhenKey_WhenKey <$ pKeyword "->"
        <|> sem_WhenKey_When    <$ pKeyword "when" <*> pExpr <* pKeyword "->"

pMultipleMatching =  sem_MultipleMatching_MultipleMatchingPat    <$> pFoldr1 (sem_ListParameter_Cons,sem_ListParameter_Nil) pParameter <* pKeyword "->" <*> pExpr
                 <|> sem_MultipleMatching_MultipleMatchingPatExp <$> pFoldr1 (sem_ListParameter_Cons,sem_ListParameter_Nil) pParameter <* pKeyword "when" <*> pFoldr1Sep_ng (sem_ListExpr_Cons,sem_ListExpr_Nil) (pKeyword "->") pExpr

pEqualOper =  sem_EqualOper_EqualExp  <$ pOperator "=" 
--          <|> EqualOper <$ pOperator ":" <*> pTypexpr <* pOperator "=" 


pToDow = sem_ToDow_To      <$ pKeyword "to"
      <|> sem_ToDow_Downto <$ pKeyword "downto"

pFieldExpr = sem_FieldExpr_FieldExpr <$> pField <* pKeyword "=" <*> pExpr

pElse = sem_Else_Else <$ pKeyword "else" <*> pExpr
     <|> sem_Else_EmptyElse <$ pSucceed EmptyElse

pInstVar = sem_InstVar_InstVar <$ pKeyword ";" <*> pInstVariantName <* pKeyword "=" <*> pExpr

pFieldKey =  sem_FieldKey_FieldKeyFiel <$> pField <*> ( (pOptField) `opt` sem_OptField_EmptyOpt)     
         <|> sem_FieldKey_FieldKeyBrek <$  pKeyword "(" <*> pExpr <* pKeyword ")" <*> ( (pOptField) `opt` sem_OptField_EmptyOpt) 
         <|> sem_FieldKey_FieldKeyCor  <$  pKeyword "[" <*> pExpr <* pKeyword "]" <*> ( (pOptField) `opt` sem_OptField_EmptyOpt)

pOptField =  sem_OptField_OptField <$  pKeyword "<-" <*> pExpr

pArgument =  sem_Argument_ArgumentExp     <$> pExpr

pParameter = sem_Parameter_ParameterPattern <$> pPattern

----------------------------- Referring to named objects ------------------------------------------

pValuePath =  sem_ValuePath_ValuePath    <$> pValueName
--          <|> sem_ValuePath_ValuePathMod <$> pModulePath <* pOperator "." <*> pValueName

pConstr =  sem_Constr_Constr       <$> pConstructorName
--       <|> sem_Constr_ConstrModule <$> pModulePath <* pOperator "." <*> pConstructorName

pField =  sem_Field_Field       <$> pFieldName
--      <|> sem_Field_FieldModule <$> pModulePath <* pOperator "." <*> pFieldName



{-
-- =============================================================================================================================

pRaiz = Raiz <$> pCuerpo

pCuerpo =Cuerpo <$> pFoldr ((:),[]) pDefinicionList

pDefinicionList = DefinicionList <$> pDefinicion <*> ((pFin) `opt` EmptyFin)

pFin = Fin <$ pKeyword ";;"
        
pDefinicion = DefinicionLet <$ pKeyword "let" <*> ((pRec) `opt` EmptyRec) <*> pFoldr1Sep_ng ((:),[]) (pKeyword "and") pLetBinding

pRec = Rec <$ pKeyword "rec" 

pLetBinding =  LetBindingPat <$> pPattern   <* pOperator "=" <*> pExpr



---------------------------------------------------------------------------------------------------
----------------------------------------- Pattern -------------------------------------------------
---------------------------------------------------------------------------------------------------



pPattern =  PatternValue <$> pValueName
        <|> PatternCons  <$> pConstant 

---------------------------------------------------------------------------------------------------
----------------------------------------- Constants -----------------------------------------------
---------------------------------------------------------------------------------------------------

pConstant =  ConstantInteger <$  pInteger
         <|> ConstantFloat   <$  pFloatingPonit
         <|> ConstantChar    <$  pChar
         <|> ConstantString  <$  pString
         <|> ConstantConstr  <$> pConstr
         <|> ConstantFalse   <$  pKeyword "false"
         <|> ConstantTrue    <$  pKeyword "true"
         <|> ConstantConrch  <$  pKeyword "["     <*  pKeyword "]"
         <|> ConstantParent  <$  pKeyword "("     <*  pKeyword ")"
         <|> ConstantTag     <$  pKeyword "'"     <*> pTagName

------------------------------------ Naming objects -----------------------------------------------

pValueName  =  ValueName     <$> pLidentifier  
           <|> ValueNameFix  <$  pKeyword "(" <*> pOperatorName <* pKeyword ")" 

pOperatorName =  OperatorPrefix  <$> pPrefix 
             <|> OperatorInfix   <$> pInfixOp
              
pInfixOp =  InfixOP   <$> pInfix
        <|> Admin     <$ pOperator "!"
        <|> Aplica    <$ pKeyword "->"
        <|> Dolar     <$ pOperator "$"
        <|> Option    <$ pOperator "|"
        <|> Sombrero  <$ pOperator "^"
        <|> Arroba    <$ pOperator "@"
        <|> Menos     <$ pOperator "-"
        <|> Menor     <$ pOperator "<"
        <|> Mayor     <$ pOperator ">"
        <|> Div       <$ pOperator "/"         
        <|> Mas       <$ pOperator "+"
        <|> Asterisc  <$ pOperator "*"
        <|> Igual     <$ pOperator "="
        <|> Ors       <$ pKeyword  "or" 
        <|> And       <$ pOperator "&"
        <|> Equals    <$ pKeyword ":=" 
        <|> Mod       <$ pKeyword "mod"
        <|> Load      <$ pKeyword "land"
        <|> Lor       <$ pKeyword "lor"
        <|> Lxor      <$ pKeyword "lxor"
        <|> Lsl       <$ pKeyword "lsl"
        <|> Lsr       <$ pKeyword "lsr"
        <|> Asr       <$ pKeyword "asr"

pConstructorName =  ConstructorName <$> pUidentifier
pLabelNames      =  LabelNames           <$> pLidentifier
pTagName         =  TagName                 <$> pUidentifier
pTypeConstrName  =  TypeConstrName   <$> pLidentifier
pFieldName       =  FieldName             <$> pLidentifier
pModuleName      =  ModuleName           <$> pUidentifier
pModuleTypeName  =  ModuleTypeNameL  <$> pLidentifier
                <|> ModuleTypeNameU  <$> pUidentifier
pClassName       =  ClassName             <$> pLidentifier
pMethodName      =  MethodName           <$> pLidentifier
pInstVariantName =  InstVariantName <$> pLidentifier

---------------------------------------------------------------------------------------------------
----------------------------------------- Expressions ---------------------------------------------
---------------------------------------------------------------------------------------------------

pExpr = pRelacion

pRelacion = pChainl (((ExprInfixOP).(Op)) <$> (pOperator "<"  <|> pOperator ">"  <|> pOperator "!" <|> pOperator "^"
                                                                              <|> pOperator "@" <|> pOperator "=")) pSumPrioridad
-- <|> pInfix "<=" <|> pInfix ">=" <|> pInfix "=="  <|> pInfix "<>" <|> pInfix "**" <|> pInfix "+."<|> pInfix "-." <|> pInfix "*."<|> pInfix "/."

pSumPrioridad  = pChainl (((ExprInfixOP).(Op)) <$> (pOperator "+" <|> pOperator "-" )) pMultPrioridad

pMultPrioridad = pChainl (((ExprInfixOP).(Op)) <$> (pOperator "*" <|> pOperator "/" )) pFactor

pFactor = ExprValuePath <$> pValuePath        
      <|> ExprConstant  <$> pConstant
      <|> ExprGrup      <$  pKeyword  "("      <*> pExpr         <*> pKeyOpeT  
      <|> ExprBegin     <$  pKeyword  "begin"  <*> pExpr         <*  pKeyword "end" 
--      <|> sem_Expr_ExprExpr       ListExpr 
      <|> ExprConstr    <$> pConstr            <*> pExpr
      <|> ExprTagName   <$  pKeyword "'"       <*> pTagName      <*> pExpr
      <|> ExprCostLista <$  pKeyword "::"      <*> pExpr         <*> pExpr       
      <|> ExprLista     <$  pKeyword "["       <*> pFoldr1Sep_ng ((:),[]) (pKeyword ";") pExpr <* pKeyword "]"
--      <|> sem_Expr_ExprField     <$  pKeyword "{"       <*> pFoldr1Sep_ng (sem_ListFieldExpr_Cons,sem_ListFieldExpr_Nil) (pKeyword ";") pFieldExpr 
--                                 <* pKeyword "}" 
      <|> ExprWhit     <$ pKeyword "{"         <*> pExpr <* pKeyword "with" 
                                <*> pFoldr1Sep_ng ((:),[]) (pKeyword ";") pFieldExpr <* pKeyword "}"
--     <|> sem_Expr_ExprArgument  <$> pFoldr1 (sem_ListArgument_Cons,sem_ListArgument_Nil) pArgument <*> pExpr   
     <|> ExprPrefixSym <$  pPrefix            <*> pExpr
     <|> ExprIf        <$  pKeyword "if"      <*> pExpr      <* pKeyword "then" <*> pExpr <*> pElse
     <|> ExprWhile     <$  pKeyword "while"   <*> pExpr      <* pKeyword "do"   <*> pExpr <* pKeyword "done" 
     <|> ExprFor       <$  pKeyword "for"     <*> pValueName <* pOperator "="   <*> pExpr <*> pToDow  <*> pExpr <* pKeyword "do" 
                                                       <*> pExpr      <* pKeyword "done"
--     <|> sem_Expr_ExprField     <$  pOperator "."      <*> pFieldKey  <*> pExpr
     <|> ExprMatch     <$  pKeyword "match"   <*> pExpr      <* pKeyword "with" <*> pPatternMatching 
     <|> ExprFunction  <$  pKeyword "function"<*> pPatternMatching  
     <|> ExprFun       <$  pKeyword "fun"     <*> pMultipleMatching 
     <|> ExprTry       <$  pKeyword "try"     <*> pExpr         <* pKeyword "with" <*> pPatternMatching 
     <|> ExprLet       <$  pKeyword "let"     <*> pRec <*> pFoldr1Sep_ng ((:),[]) (pKeyword "and") 
                                pLetBinding   <* pKeyword "in" <*> pExpr
--     <|> sem_Expr_ExprClassPath <$  pKeyword "new"     <*> pClassPath  
--     <|> sem_Expr_ExprClassBody <$  pKeyword "object"  <*> pClassBody    <* pKeyword "end"
     <|> ExprMethodName <$  pKeyword "#"  <*> pMethodName  <*> pExpr
     <|> ExprInstVarName<$> pInstVariantName   <*> pExpreFlec
     <|> ExprAssert     <$  pKeyword "assert"  <*> pExpr    
     <|> ExprLazy       <$  pKeyword "lazy"    <*> pExpr 


pExpreFlec =  ExpreFlec <$  pKeyword "<-" <*> pExpr 


pKeyOpeT =  KeyBrea  <$  pKeyword ")"
--        <|> KeyOper  <$  pOperator ":"   <*> pTypexpr <*> pKeyPrefi 
--        <|> KeyInfi  <$  pKeyword  ":>" <*> pTypexpr <* pKeyword ")" 

pKeyPrefi = KeycBrea <$ pKeyword ")"
--         <|> KeyInfix <$ pKeyword ":>" <*> pTypexpr <* pKeyword ")" 


pPatternMatching = PatternMatching <$> pFoldr1Sep_ng ((:),[]) (pOperator "|") pOptPattern 

pOptPattern =  OptPattern   <$> pPattern <*> pWhenKey <*> pExpr

pWhenKey =  WhenKey <$ pKeyword "->"
        <|> When    <$ pKeyword "when" <*> pExpr <* pKeyword "->"

pMultipleMatching =  MultipleMatchingPat    <$> pFoldr1 ((:),[]) pParameter <* pKeyword "->" <*> pExpr
                 <|> MultipleMatchingPatExp <$> pFoldr1 ((:),[]) pParameter <* pKeyword "when" <*> pFoldr1Sep_ng ((:),[]) (pKeyword "->") pExpr

pEqualOper =  EqualExp  <$ pOperator "=" 
--          <|> EqualOper <$ pOperator ":" <*> pTypexpr <* pOperator "=" 


pToDow =  To      <$ pKeyword "to"
      <|> Downto <$ pKeyword "downto"

pFieldExpr = FieldExpr <$> pField <* pKeyword "=" <*> pExpr

pElse =  Else <$ pKeyword "else" <*> pExpr
     <|> EmptyElse <$ pSucceed EmptyElse

pInstVar = InstVar <$ pKeyword ";" <*> pInstVariantName <* pKeyword "=" <*> pExpr

pFieldKey =  FieldKeyFiel <$> pField <*> ( (pOptField) `opt` EmptyOpt)     
         <|> FieldKeyBrek <$  pKeyword "(" <*> pExpr <* pKeyword ")" <*> ( (pOptField) `opt` EmptyOpt) 
         <|> FieldKeyCor  <$  pKeyword "[" <*> pExpr <* pKeyword "]" <*> ( (pOptField) `opt` EmptyOpt)

pOptField = OptField <$  pKeyword "<-" <*> pExpr

pArgument = ArgumentExp     <$> pExpr

pParameter =ParameterPattern <$> pPattern

----------------------------- Referring to named objects ------------------------------------------

pValuePath = ValuePath    <$> pValueName
--          <|> sem_ValuePath_ValuePathMod <$> pModulePath <* pOperator "." <*> pValueName

pConstr =  Constr       <$> pConstructorName
--       <|> sem_Constr_ConstrModule <$> pModulePath <* pOperator "." <*> pConstructorName

pField =  Field       <$> pFieldName
--      <|> sem_Field_FieldModule <$> pModulePath <* pOperator "." <*> pFieldName

-}
