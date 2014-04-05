{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fppas2js;

{$mode objfpc}{$H+}

interface

uses
httpdefs,  Classes, SysUtils, jsbase, jstree, pastree, pparser;

Type
  EPas2JS = Class(Exception);
  { TPasToJSConverter }
  TConvertContext = Class(TObject)

  end;

  TPasToJSConverter = Class(TObject)
  private
    FCurrentContext: TJSElement;
    FMainFunction: TJSString;
    FNameSpace: TJSString;
    Procedure AddToSourceElements(Src: TJSSourceElements; El: TJSElement);
    Function CreateConstDecl(El: TPasElement; AContext: TConvertContext): TJSElement;
    Function CreateIdentifierExpr(AName: String; El: TPasElement; AContext: TConvertContext): TJSElement;
    Function CreateTypeDecl(El: TPasElement; AContext: TConvertContext): TJSElement;
    Function CreateVarDecl(El: TPasElement; AContext: TConvertContext): TJSElement;
    procedure SetCurrentContext(AValue: TJSElement);
  protected
    // helper functions
    Procedure DoError(Const Msg : String);
    Procedure DoError(Const Msg : String; Const Args : Array of Const);
    // Never create an element manually, always use the below function
    Function CreateElement(C: TJSElementClass; Src: TPasElement): TJSElement; virtual;
    Function GetExpressionValueType(El: TPasExpr; AContext: TConvertContext ): TJSType; virtual;
    Function GetIdentValueType(AName: String; AContext: TConvertContext): TJSType; virtual;
    Function TransFormIdent(El: TJSPrimaryExpressionIdent; AContext : TConvertContext): TJSElement;virtual;
    Function CreateJSContext(AContext : TConvertContext): TJSElement;virtual;
    Function TransFormVariableName(Const AName :  String; AContext : TConvertContext) : String;
    Function TransFormVariableName(El : TPasElement; AContext : TConvertContext) : String;
    Function TransFormFunctionName(El : TPasElement; AContext : TConvertContext) : String;
    Function GetExceptionObjectname(AContext : TConvertContext) : String;
    Function ResolveType(El : TPasElement; AContext : TConvertContext) : TPasType;
    // Statements
    Function ConvertImplBlockElements(El: TPasImplBlock; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBeginEndStatement(El: TPasImplBeginBlock; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertStatement(El: TPasImplStatement; AContext: TConvertContext ): TJSElement;virtual;
    Function ConvertAssignStatement(El: TPasImplAssign; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertRaiseStatement(El: TPasImplRaise; AContext: TConvertContext ): TJSElement; virtual;
    Function ConvertIfStatement(El: TPasImplIfElse; AContext: TConvertContext ): TJSElement; virtual;
    Function ConvertWhileStatement(El: TPasImplWhileDo; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertRepeatStatement(El: TPasImplRepeatUntil; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertForStatement(El: TPasImplForLoop; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertFinalizationSection(El: TFinalizationSection; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertInitializationSection(El: TInitializationSection; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertSimpleStatement(El: TPasImplSimple; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertTryStatement(El: TPasImplTry; AContext: TConvertContext ): TJSElement;virtual;
    Function ConvertWithStatement(El: TPasImplWithDo; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertTryFinallyStatement(El: TPasImplTryFinally; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertExceptOn(El: TPasImplExceptOn; AContext: TConvertContext): TJSElement;
    Function ConvertTryExceptStatement(El: TPasImplTryExcept; AContext: TConvertContext): TJSElement;
    // Expressions
    Function ConvertArrayValues(El: TArrayValues; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertInheritedExpression(El: TInheritedExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertNilExpr(El: TNilExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertParamsExpression(El: TParamsExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertRecordValues(El: TRecordValues; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertSelfExpression(El: TSelfExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertBinaryExpression(El: TBinaryExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertBoolConstExpression(El: TBoolConstExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertPrimitiveExpression(El: TPrimitiveExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertUnaryExpression(El: TUnaryExpr; AContext : TConvertContext ): TJSElement;virtual;
    Function ConvertCallExpression(El: TParamsExpr; AContext : TConvertContext ): TJSElement;virtual;
    Function TransFormStringLiteral(S : String) : String;
    // Convert various TPasElement nodes
    Function ConvertProperty(El: TPasProperty; AContext : TConvertContext ): TJSElement;virtual;
    Function ConvertCommand(El: TPasImplCommand; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertCommands(El: TPasImplCommands; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertConst(El: TPasConst; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertDeclarations(El: TPasDeclarations; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertExportSymbol(El: TPasExportSymbol; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertExpression(El: TPasExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertImplBlock(El: TPasImplBlock; AContext : TConvertContext ): TJSElement;virtual;
    Function ConvertLabelMark(El: TPasImplLabelMark; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertLabels(El: TPasLabels; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertModule(El: TPasModule; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertPackage(El: TPasPackage; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertArgument(El: TPasArgument; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertProcedure(El: TPasProcedure; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertProcedureImpl(El: TPasProcedureImpl; AContext : TConvertContext    ): TJSElement;virtual;
    Function ConvertResString(El: TPasResString; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertResultElement(El: TPasResultElement; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertType(El: TPasElement; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertVariable(El: TPasVariable; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertElement(El : TPasElement; AContext : TConvertContext) : TJSElement; virtual;

    Property CurrentContext : TJSElement Read FCurrentContext Write SetCurrentContext;
  Public
    Function ConvertElement(El : TPasElement) : TJSElement;
    Property NameSpace : TJSString Read FNameSpace Write FNameSpace;
    Property MainFunction : TJSString Read FMainFunction Write FMainFunction;
  end;
  EPasToJS = Class(Exception);

implementation

resourcestring
  SErrUNknownExpressionClass = 'Unknown expression class: %s';
  SErrUnexpected = 'Unexpected class: %s';
  SerrInitalizedArray = 'Initialized array variables not yet supported';

{ TPasToJSConverter }

Procedure TPasToJSConverter.AddToSourceElements(Src : TJSSourceElements; El : TJSElement);

Var
  L : TJSStatementList;
  A : TJSElement;

begin
  Repeat
    if Not (EL is TJSStatementList) then
      begin
      A:=EL;
      El:=Nil;
      end
    else
      begin
      L:=EL as TJSStatementList;
      A:=L.A;
      EL:=L.B;
      L.A:=Nil;
      L.B:=Nil;
      FreeAndNil(L);
      end;
    Src.Statements.AddNode.Node:=A;
  until (El=Nil);
end;

Function TPasToJSConverter.ConvertModule(El: TPasModule; AContext : TConvertContext): TJSElement;

Var
  I : Integer;
  Src : TJSSourceElements;

begin
  Result:=Nil;
  Src:=TJSSourceElements(CreateElement(TJSSourceElements,El));
  Result:=Src;
  if Assigned(El.InterfaceSection) then
    AddToSourceElements(Src,ConvertElement(El.InterfaceSection,AContext));
  if assigned(El.ImplementationSection) then
    AddToSourceElements(Src,ConvertElement(El.ImplementationSection,AContext));
  if (El is TPasProgram) then
    begin
    if Assigned(TPasProgram(El).ProgramSection) then
      AddToSourceElements(Src,ConvertElement(TPasProgram(El).ProgramSection,AContext));
    end;
  if Assigned(El.InitializationSection) then
    AddToSourceElements(Src,ConvertElement(El.InitializationSection,AContext));
  if Assigned(El.FinalizationSection) then
    AddToSourceElements(Src,ConvertElement(El.FinalizationSection,AContext));
{
TPasUnitModule = Class(TPasModule)
TPasLibrary = class(TPasModule)
}
end;

Function TPasToJSConverter.CreateElement(C : TJSElementClass; Src : TPasElement) : TJSElement;

begin
  if Assigned(Src) then
    Result:=C.Create(Src.SourceLinenumber,1,Src.SourceFilename)
  else
    Result:=C.Create(0,0,'');
end;

Function TPasToJSConverter.ConvertUnaryExpression(El: TUnaryExpr; AContext : TConvertContext): TJSElement;

Var
  C : TJSElementClass;
  U : TJSUnaryExpression;
  E : TJSElement;

begin
  Result:=Nil;
  E:=ConvertElement(El.Operand);
  Case el.OpCode of
    eopAdd:
      begin
      U:=TJSUnaryPlusExpression(CreateElement(TJSUnaryPlusExpression,El));
      U.A:=E;
      end;
    eopSubtract:
      begin
      U:=TJSUnaryPlusExpression(CreateElement(TJSUnaryMinusExpression,El));
      U.A:=E;
      end;
  end;
  Result:=U;
end;

Function TPasToJSConverter.ConvertCallExpression(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
Var
  C : TJSElementClass;
  U : TJSUnaryExpression;
  E : TJSElement;
  Id : TJSPrimaryExpressionIdent;

begin
  Raise EPasToJS.CreateFmt(SErrUnexpected,[EL.ClassName]);
  Result:=Nil;
  ID:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,EL));
  Case el.OpCode of
    eopAdd:
      begin
      U:=TJSUnaryPlusExpression(CreateElement(TJSUnaryPlusExpression,El));
      U.A:=E;
      end;
    eopSubtract:
      begin
      U:=TJSUnaryPlusExpression(CreateElement(TJSUnaryMinusExpression,El));
      U.A:=E;
      end;
  end;
  Result:=U;
end;

Function TPasToJSConverter.TransFormStringLiteral(S: String): String;
begin
  // This needs some more complicated algorithm that handles #13"abc etc.
  Result:=Copy(S,2,Length(S)-2);
  Result:=StringReplace(Result,'''''','''',[rfReplaceAll]);
end;


Function TPasToJSConverter.GetIdentValueType(AName : String; AContext : TConvertContext): TJSType;

begin
  Result:=jstUNDEFINED;
end;


Function TPasToJSConverter.GetExpressionValueType(El : TPasExpr; AContext : TConvertContext) : TJSType;

  Function CombineValueType(A,B : TJSType) : TJSType;

  begin
    If (A=jstUndefined) then
      Result:=B
    else if (B=jstundefined) then
      Result:=A
    else
      Result:=A; // pick the first
  end;

Var
  A,B : TJSType;

begin
  if (El is TBoolConstExpr) then
    Result:=jstBoolean
  else  If (EL is TPrimitiveExpr) then
    begin
    Case EL.Kind of
      pekIdent : Result:=GetIdentValueType(El.Name,AContext);
      pekNumber : Result:=jstNumber;
      pekString : Result:=jstString;
      pekSet : Result:=jstUNDEFINED;
      pekNil : Result:=jstNull;
      pekBoolConst : Result:=jstBoolean;
      pekRange : Result:=jstUNDEFINED;
      pekFuncParams : Result:=jstUNDEFINED;
      pekArrayParams : Result:=jstUNDEFINED;
      pekListOfExp : Result:=jstUNDEFINED;
      pekInherited : Result:=jstUNDEFINED;
      pekSelf : Result:=jstObject;
    end
    end
  else if (EL is TUnaryExpr) then
    Result:=GetExpressionValueType(TUnaryExpr(El).Operand,AContext)
  else if (EL is TBinaryExpr) then
    begin
    A:=GetExpressionValueType(TBinaryExpr(El).Left,AContext);
    B:=GetExpressionValueType(TBinaryExpr(El).Right,AContext);
    Result:=CombineValueType(A,B);
    end
  else
    result:=jstUndefined
end;

Function TPasToJSConverter.ConvertBinaryExpression(El: TBinaryExpr; AContext : TConvertContext): TJSElement;

Type
  TJSBinaryClass = Class of TJSBinary;

Const
  BinClasses : Array [TExprOpCode] of TJSBinaryClass = (
   Nil, //eopEmpty,
   TJSAdditiveExpressionPlus,
   TJSAdditiveExpressionMinus,
   TJSMultiplicativeExpressionMul,
   TJSMultiplicativeExpressionDiv,
   TJSMultiplicativeExpressionDiv,
   TJSMultiplicativeExpressionMod,
   Nil, //eopPower
   TJSRShiftExpression,
   TJSLShiftExpression,
   Nil, // Not
   Nil, // And
   Nil, // Or
   Nil, // XOr
   TJSEqualityExpressionEQ,
   TJSEqualityExpressionNE,
   TJSRelationalExpressionLT,
   TJSRelationalExpressionGT,
   TJSRelationalExpressionLE,
   TJSRelationalExpressionGE,
   Nil, // In
   TJSRelationalExpressionInstanceOf, // is
   Nil, // As
   Nil, // Symmetrical diff
   Nil, // Address,
   Nil, // Deref
   Nil  // SubIndent,
  );

Var
  R : TJSBinary;
  C : TJSBinaryClass;
  A,B : TJSElement;

begin
  Result:=Nil;
  C:=BinClasses[EL.OpCode];
  A:=ConvertElement(EL.left,AContext);
  try
    B:=ConvertElement(EL.right,AContext);
  except
    FreeAndNil(A);
    Raise;
  end;
  if (C=Nil) then
    Case EL.OpCode of
      eopAs : begin
              Result:=ConvertElement(El.Left,AContext);
              end;
      eopAnd,
      eopOr,
      eopXor :
        begin
        if (GetExpressionValueType(EL.Left,AContext)=jstNumber)
            or (GetExpressionValueType(EL.Right,AContext)=jstNumber) then
          Case EL.OpCode of
            eopAnd : C:=TJSBitwiseAndExpression;
            eopOr : C:=TJSBitwiseOrExpression;
            eopXor : C:=TJSBitwiseXOrExpression;
          end
        else
          Case EL.OpCode of
            eopAnd : C:=TJSLogicalAndExpression;
            eopOr : C:=TJSLogicalOrExpression;
            eopXOR : DoError('Logical XOR not supported yet');
          end;
        end;
      eopSubIdent :
        begin
        Result:=TJSDotMemberExpression(CreateElement(TJSDotMemberExpression,El));
        TJSDotMemberExpression(Result).Mexpr:=A;
        if Not (B is TJSPrimaryExpressionIdent) then
          DOError('Member expression must be an identifier');
        TJSDotMemberExpression(Result).Name:=TJSPrimaryExpressionIdent(B).Name;
        FreeAndNil(B);
        end
    else
      DoError('Unknown/Unsupported operand type for binary expression');
    end;
  if (Result=Nil) and (C<>Nil) then
    begin
    R:=TJSBinary(CreateElement(C,EL));
    R.A:=A;
    R.B:=B;
    Result:=R;
    end;
end;

Function TPasToJSConverter.TransFormIdent(El: TJSPrimaryExpressionIdent; AContext : TConvertContext): TJSElement;

begin
  EL.Name:=LowerCase(EL.Name);
  Result:=El;
end;


Function TPasToJSConverter.CreateIdentifierExpr(AName : String; El : TPasElement; AContext : TConvertContext): TJSElement;

Var
  I : TJSPrimaryExpressionIdent;

begin
  I:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,El));
  I.Name:=AName;
  Result:=TransFormIdent(I,AContext);
end;

Function TPasToJSConverter.ConvertPrimitiveExpression(El: TPrimitiveExpr; AContext : TConvertContext): TJSElement;

Var
  L : TJSLiteral;
  D : TJSNumber;
  C : Integer;

begin
  Result:=Nil;
  case El.Kind of
    pekString:
      begin
      L:=TJSLiteral(CreateElement(TJSLiteral,El));
      L.Value.AsString:=TransFormStringLiteral(El.Value);
      Result:=L;
      end;
    pekNumber:
      begin
      L:=TJSLiteral(CreateElement(TJSLiteral,El));
      Val(El.Value,D,C);
      if C<>0 then
        DoError('Invalid number: %s',[EL.Value]);
      L.Value.AsNumber:=D;
      Result:=L;
      end;
    pekIdent:
      begin
      Result:=CreateIdentifierExpr(El.Value,El,AContext);
      end;
  end;
end;


Function TPasToJSConverter.ConvertBoolConstExpression(El: TBoolConstExpr; AContext : TConvertContext): TJSElement;

Var
  L : TJSLiteral;

begin
  Result:=Nil;
  L:=TJSLiteral(CreateElement(TJSLiteral,El));
  L.Value.AsBoolean:=EL.Value;
  Result:=L;
end;

Function TPasToJSConverter.ConvertNilExpr(El: TNilExpr; AContext : TConvertContext): TJSElement;

Var
  L : TJSLiteral;

begin
  L:=TJSLiteral(CreateElement(TJSLiteral,El));
  L.Value.IsNull:=True;
  Result:=L;
end;

Function TPasToJSConverter.ConvertInheritedExpression(El: TInheritedExpr; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
//  TInheritedExpr = class(TPasExpr)
end;

Function TPasToJSConverter.ConvertSelfExpression(El: TSelfExpr; AContext : TConvertContext): TJSElement;

begin
  Result:=TJSPrimaryExpressionThis(CreateElement(TJSPrimaryExpressionThis,El));
end;

Function TPasToJSConverter.ConvertParamsExpression(El: TParamsExpr; AContext : TConvertContext): TJSElement;

Var
  b,B2 : TJSBracketMemberExpression;
  C : TJSCallExpression;
  I : Integer;
  E : TJSElement;

begin
  Result:=Nil;
  Case EL.Kind of
  pekFuncParams :
    begin
    C:=TJSCallExpression(CreateElement(TJSCallExpression,El));
    try
      C.Expr:=ConvertElement(El.Value,AContext);
      if (Length(EL.Params)>0) then
        begin
        C.Args:=TJSArguments(CreateElement(TJSArguments,El));
        For I:=0 to Length(EL.Params)-1 do
          begin
          E:=ConvertElement(EL.Params[i]);
          C.Args.Elements.AddElement.Expr:=E;
          end;
        end;
    except
      FreeAndNil(C);
      Raise;
    end;
    Result:=C;
    end;
  pekArrayParams:
    begin
    if Length(EL.Params)<>1 then
      Raise EPasToJS.Create('Only 1-dimensional expressions allowed at this point');
    B:=TJSBracketMemberExpression(CreateElement(TJSBracketMemberExpression,El));;
    B.Mexpr:=ConvertElement(El.Value,AContext);
    Result:=B;
    B.Name:=ConvertElement(EL.Params[0],AContext);
    end
  end;
end;

Function TPasToJSConverter.ConvertRecordValues(El: TRecordValues; AContext : TConvertContext): TJSElement;

Var
  R :  TJSObjectLiteral;
  I : Integer;
  It : TRecordValuesItem;
  rel : TJSObjectLiteralElement;

begin
  R:=TJSObjectLiteral(CreateElement(TJSObjectLiteral,El));
  For I:=0 to Length(El.Fields)-1 do
    begin
    it:=El.Fields[i];
    Rel:=R.Elements.AddElement;
    Rel.Name:=it.Name;
    Rel.Expr:=ConvertElement(it.ValueExp,AContext);
    end;
  Result:=R;
end;

Function TPasToJSConverter.ConvertArrayValues(El: TArrayValues; AContext : TConvertContext): TJSElement;

Var
  R :  TJSArrayLiteral;
  I : Integer;
  rel : TJSArrayLiteralElement;

begin
  R:=TJSArrayLiteral(CreateElement(TJSObjectLiteral,El));
  For I:=0 to Length(El.Values)-1 do
    begin
    Rel:=R.Elements.AddElement;
    Rel.ElementIndex:=i;
    Rel.Expr:=ConvertElement(El.Values[i],AContext);
    end;
  Result:=R;
end;

Function TPasToJSConverter.ConvertExpression(El: TPasExpr; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (El is TUnaryExpr) then
    Result:=ConvertUnaryExpression(TUnaryExpr(El),AContext)
  else if (El is TBinaryExpr) then
    Result:=ConvertBinaryExpression(TBinaryExpr(El),AContext)
  else if (El is TPrimitiveExpr) then
    Result:=ConvertPrimitiveExpression(TPrimitiveExpr(El),AContext)
  else if (El is TBoolConstExpr) then
    Result:=ConvertBoolConstExpression(TBoolConstExpr(El),AContext)
  else if (El is TNilExpr) then
    Result:=ConvertNilExpr(TNilExpr(El),AContext)
  else if (El is TInheritedExpr) then
    Result:=ConvertInheritedExpression(TInheritedExpr(El),AContext)
  else if (El is TSelfExpr) then
    Result:=ConvertSelfExpression(TSelfExpr(El),AContext)
  else if (El is TParamsExpr) then
    Result:=ConvertParamsExpression(TParamsExpr(El),AContext)
  else if (El is TRecordValues) then
    Result:=ConvertRecordValues(TRecordValues(El),AContext)
  else if (El is TRecordValues) then
    Result:=ConvertRecordValues(TRecordValues(El),AContext)
  else
    DoError(SErrUNknownExpressionClass,[EL.ClassName])
end;

Function TPasToJSConverter.CreateConstDecl(El: TPasElement; AContext : TConvertContext): TJSElement;

Var
  C : TJSElement;
  V : TJSVariableStatement;

begin
  C:=ConvertElement(El,AContext);
  V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
  V.A:=C;
  Result:=V;
end;

Function TPasToJSConverter.CreateTypeDecl(El: TPasElement; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  // Need to do something for classes and records.
end;

Function TPasToJSConverter.CreateVarDecl(El: TPasElement; AContext : TConvertContext): TJSElement;

Var
  C : TJSElement;
  V : TJSVariableStatement;

begin
  C:=ConvertElement(El,AContext);
  V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
  V.A:=C;
  Result:=V;
end;

Function TPasToJSConverter.ConvertDeclarations(El: TPasDeclarations; AContext : TConvertContext): TJSElement;

Var
  P : TPasElement;
  SL,SL2 : TJSStatementList;
  E : TJSElement;
  I : Integer;

  Procedure AddToSL;

  begin
    if Assigned(E) then
      begin
      if Assigned(SL.A) then
        begin
        SL2:=TJSStatementList(CreateElement(TJSStatementList,El));
        SL.B:=SL2;
        SL:=SL2;
        end;
      SL.A:=E;
      end;
  end;

begin
  SL:=TJSStatementList(CreateElement(TJSStatementList,El));
  Result:=SL;
  For I:=0 to El.Declarations.Count-1 do
    begin
    E:=Nil;
    P:=TPasElement(El.Declarations[i]);
    if P is TPasConst then
      E:=CreateConstDecl(P,AContext)
    else if P is TPasVariable then
      E:=CreateVarDecl(P,AContext)
    else if P is TPasType then
      E:=CreateTypeDecl(P,AContext)
    else if P is TPasProcedure then
      E:=ConvertElement(P as TPasProcedure,AContext)
    else
      DoError('Unknown class: "%s" ',[P.ClassName]);
    AddToSL;
    end;
  if (El is TProcedureBody) then
    begin
    E:=ConvertElement(TProcedureBody(El).Body,AContext);
    AddToSl;
    end;
{
  TPasDeclarations = class(TPasElement)
  TPasSection = class(TPasDeclarations)
  TInterfaceSection = class(TPasSection)
  TImplementationSection = class(TPasSection)
  TProgramSection = class(TImplementationSection)
  TLibrarySection = class(TImplementationSection)
  TProcedureBody = class(TPasDeclarations)
}
end;

Function TPasToJSConverter.ConvertType(El: TPasElement; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
{

TPasType = class(TPasElement)
TPasPointerType = class(TPasType)
TPasAliasType = class(TPasType)
TPasTypeAliasType = class(TPasAliasType)
TPasClassOfType = class(TPasAliasType)
TPasRangeType = class(TPasType)
TPasArrayType = class(TPasType)
TPasFileType = class(TPasType)
TPasEnumValue = class(TPasElement)
TPasEnumType = class(TPasType)
TPasSetType = class(TPasType)
TPasVariant = class(TPasElement)
TPasRecordType = class(TPasType)
TPasClassType = class(TPasType)
TPasProcedureType = class(TPasType)
TPasFunctionType = class(TPasProcedureType)
TPasUnresolvedSymbolRef = class(TPasType)
TPasUnresolvedTypeRef = class(TPasUnresolvedSymbolRef)
TPasUnresolvedUnitRef = Class(TPasUnresolvedSymbolRef)
TPasStringType = class(TPasUnresolvedTypeRef)
TPasTypeRef = class(TPasUnresolvedTypeRef)
}
end;

Function TPasToJSConverter.ConvertProcedure(El: TPasProcedure; AContext : TConvertContext): TJSElement;

Var
  FS : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;

begin
  FS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,EL));
  Result:=FS;
  FD:=TJSFuncDef.Create;
  FD.Name:=TransFormFunctionName(El,AContext);
  FS.AFunction:=FD;
  FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,EL.Body));
  FD.Body.A:=ConvertElement(El.Body,AContext);
  {
  TPasProcedureBase = class(TPasElement)
  TPasOverloadedProc = class(TPasProcedureBase)
  TPasProcedure = class(TPasProcedureBase)
  TPasFunction = class(TPasProcedure)
  TPasOperator = class(TPasProcedure)
  TPasConstructor = class(TPasProcedure)
  TPasDestructor = class(TPasProcedure)
  TPasClassProcedure = class(TPasProcedure)
  TPasClassFunction = class(TPasProcedure)
  }

end;

Function TPasToJSConverter.ConvertProcedureImpl(El: TPasProcedureImpl; AContext : TConvertContext): TJSElement;

Var
  FS : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;

begin
  FS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,EL));
  Result:=FS;
  FD:=TJSFuncDef.Create;
  FD.Name:=TransFormFunctionName(El,AContext);
  FS.AFunction:=FD;
  FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,EL.Body));
  FD.Body.A:=ConvertElement(El.Body,AContext);
{
  TPasProcedureImpl = class(TPasElement)
  TPasConstructorImpl = class(TPasProcedureImpl)
  TPasDestructorImpl = class(TPasProcedureImpl)
}
end;

Function TPasToJSConverter.ConvertBeginEndStatement(El: TPasImplBeginBlock; AContext : TConvertContext): TJSElement;

begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

Function TPasToJSConverter.ConvertImplBlockElements(El: TPasImplBlock; AContext : TConvertContext): TJSElement;

var
  B : TJSElement;
  S,S2 : TJSStatementList;
  I : Integer;

begin
  if Not (Assigned(EL.Elements) and (EL.Elements.Count>0)) then
    Result:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El))
  else
    begin
    S:=TJSStatementList(CreateElement(TJSStatementList,TPasImplElement(EL)));
    Result:=S;
    For I:=0 to EL.Elements.Count-1 do
      begin
      B:=ConvertElement(TPasImplElement(EL.Elements[i]),AContext);
      if not Assigned(S.A) then
        S.A:=B
      else
        begin
        if Assigned(S.B) then
          begin
          S2:=TJSStatementList(CreateElement(TJSStatementList,TPasImplElement(EL.Elements[i])));
          S2.A:=S.B;
          S.B:=S2;
          S:=S2;
          end;
        S.B:=B;
        end;
      end;
    end;
end;

Function TPasToJSConverter.ConvertInitializationSection(El: TInitializationSection; AContext : TConvertContext): TJSElement;
begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

Function TPasToJSConverter.ConvertFinalizationSection(El: TFinalizationSection; AContext : TConvertContext): TJSElement;
begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

Function TPasToJSConverter.ConvertTryStatement(El: TPasImplTry; AContext : TConvertContext): TJSElement;

Var
  B,F : TJSElement;
  T : TJSTryStatement;
  IsFin : Boolean;

begin
  F:=Nil;
  B:=ConvertImplBlockElements(El,AContext);
  try
    F:=ConvertElement(El.FinallyExcept);
    IsFin:=El.FinallyExcept is TPasImplTryFinally;
    if IsFin then
      T:=TJSTryFinallyStatement(CreateElement(TJSTryFinallyStatement,El))
    else
      begin
      T:=TJSTryCatchStatement(CreateElement(TJSTryCatchStatement,El));
      T.Ident:=GetExceptionObjectname(AContext);
      end;
  except
    FreeAndNil(B);
    FreeAndNil(F);
    Raise;
  end;
  if IsFin then
    T.BFinally:=F
  else
    T.BCatch:=F;
  T.Block:=B;
  Result:=T;
end;

Function TPasToJSConverter.ConvertTryFinallyStatement(El: TPasImplTryFinally; AContext : TConvertContext): TJSElement;


begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

Function TPasToJSConverter.ConvertTryExceptStatement(El: TPasImplTryExcept; AContext : TConvertContext): TJSElement;


begin

  Result:=ConvertImplBlockElements(El,AContext);
end;

Function TPasToJSConverter.ConvertImplBlock(El: TPasImplBlock; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (EL is TPasImplStatement) then
    Result:=ConvertStatement(TPasImplStatement(El),AContext)
  else if (EL is TPasImplIfElse) then
    Result:=ConvertIfStatement(TPasImplIfElse(El),AContext)
  else if (El is TPasImplRepeatUntil) then
    Result:=ConvertRepeatStatement(TPasImplRepeatUntil(El),AContext)
  else if (El is TPasImplBeginBlock) then
    Result:=ConvertBeginEndStatement(TPasImplBeginBlock(El),AContext)
  else if (El is TInitializationSection) then
    Result:=ConvertInitializationSection(TInitializationSection(El),AContext)
  else if (El is TFinalizationSection) then
    Result:=ConvertFinalizationSection(TFinalizationSection(El),AContext)
  else if (El is TPasImplTry) then
    Result:=ConvertTryStatement(TPasImplTry(El),AContext)
  else if (El is TPasImplTryFinally) then
    Result:=ConvertTryFinallyStatement(TPasImplTryFinally(El),AContext)
  else if (El is TPasImplTryExcept) then
    Result:=ConvertTryExceptStatement(TPasImplTryExcept(El),AContext);
(*
  TPasImplBlock = class(TPasImplElement)
  TPasImplCaseOf = class(TPasImplBlock)
  TPasImplStatement = class(TPasImplBlock)
  TPasImplCaseElse = class(TPasImplBlock)
  TPasImplTry = class(TPasImplBlock)
  TPasImplTryHandler = class(TPasImplBlock)
  TPasImplTryFinally = class(TPasImplTryHandler)
  TPasImplTryExcept = class(TPasImplTryHandler)
  TPasImplTryExceptElse = class(TPasImplTryHandler)

*)
end;

Function TPasToJSConverter.ConvertPackage(El: TPasPackage; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertResString(El: TPasResString; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertArgument(El: TPasArgument; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertResultElement(El: TPasResultElement; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertVariable(El: TPasVariable; AContext : TConvertContext): TJSElement;

Var
  V : TJSVarDeclaration;
  T : TPasType;
  L : TJSLiteral;

begin
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
  V.Name:=TransFormVariableName(EL,AContext);
  T:=ResolveType(EL.VarType,AContext);
  if (T is TPasArrayType) then
    begin
    V.Init:=TJSArrayLiteral(CreateElement(TJSArrayLiteral,EL.VarType));
    If Assigned(EL.Expr) then
      Raise EPasToJS.Create(SerrInitalizedArray);
    end
  else If Assigned(EL.Expr) then
    V.Init:=ConvertElement(El.Expr,AContext);
  Result:=V;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertConst(El: TPasConst; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertProperty(El: TPasProperty; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
//  TPasProperty = class(TPasVariable)

end;

Function TPasToJSConverter.ConvertExportSymbol(El: TPasExportSymbol; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasPackage = class(TPasElement)
end;

Function TPasToJSConverter.ConvertLabels(El: TPasLabels; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
//  TPasLabels = class(TPasImplElement)
end;

Function TPasToJSConverter.ConvertRaiseStatement(El: TPasImplRaise; AContext : TConvertContext): TJSElement;

Var
  E : TJSElement;
  T : TJSThrowStatement;

begin
  if El.ExceptObject<>Nil then
    E:=ConvertElement(El.ExceptObject)
  else
    E:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El,AContext);
  T:=TJSThrowStatement(CreateElement(TJSThrowStatement,El));
  T.A:=E;
  Result:=T;
end;

Function TPasToJSConverter.ConvertAssignStatement(El: TPasImplAssign; AContext : TConvertContext): TJSElement;

Var
  LHS,RHS : TJSElement;
  T : TJSAssignStatement;

begin
  LHS:=ConvertElement(El.left);
  try
    RHS:=ConvertElement(El.Right);
  except
    FreeAndNil(LHS);
    Raise;
  end;
  T:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
  T.Expr:=RHS;
  T.LHS:=LHS;
  Result:=T;
end;

Function TPasToJSConverter.ConvertCommand(El: TPasImplCommand; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
//  TPasImplCommand = class(TPasImplElement)
end;

Function TPasToJSConverter.ConvertIfStatement(El: TPasImplIfElse; AContext : TConvertContext): TJSElement;

Var
  C,BThen,BElse : TJSElement;
  T : TJSIfStatement;

begin
  C:=Nil;
  BThen:=Nil;
  BElse:=Nil;
  try
    C:=ConvertElement(El.ConditionExpr);
    if Assigned(El.IfBranch) then
      BThen:=ConvertElement(El.IfBranch)
    else
      BThen:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
    if Assigned(El.ElseBranch) then
      BElse:=ConvertElement(El.ElseBranch);
  except
    FreeAndNil(C);
    FreeAndNil(BThen);
    FreeAndNil(BElse);
    Raise;
  end;
  T:=TJSIfStatement(CreateElement(TJSIfStatement,El));
  T.Cond:=C;
  T.Btrue:=BThen;
  T.BFalse:=BElse;
  Result:=T;
end;

Function TPasToJSConverter.ConvertWhileStatement(El: TPasImplWhileDo; AContext : TConvertContext): TJSElement;

Var
  C : TJSElement;
  B : TJSElement;
  W : TJSWhileStatement;
begin
  Result:=Nil;
  C:=Nil;
  B:=Nil;
  try
    C:=ConvertElement(EL.ConditionExpr,AContext);
    if Assigned(EL.Body) then
      B:=ConvertElement(EL.Body,AContext)
    else
      B:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
  except
    FreeAndNil(B);
    FreeAndNil(C);
  end;
  W:=TJSWhileStatement(CreateElement(TJSWhileStatement,El));
  W.Cond:=C;
  W.body:=B;
  Result:=W;
end;

Function TPasToJSConverter.ConvertRepeatStatement(El: TPasImplRepeatUntil; AContext: TConvertContext): TJSElement;
Var
  C : TJSElement;
  N : TJSUnaryNotExpression;
  W : TJSDoWhileStatement;
  B : TJSElement;

begin
  Result:=Nil;
  C:=Nil;
  B:=Nil;
  try
    C:=ConvertElement(EL.ConditionExpr,AContext);
    N:=TJSUnaryNotExpression(CreateElement(TJSUnaryNotExpression,EL.ConditionExpr));
    N.A:=C;
    B:=ConvertImplBlockElements(El,AContext);
  except
    FreeAndNil(B);
    FreeAndNil(C);
  end;
  W:=TJSDoWhileStatement(CreateElement(TJSDoWhileStatement,El));
  W.Cond:=N;
  W.body:=B;
  Result:=W;
end;

Function TPasToJSConverter.ConvertForStatement(El: TPasImplForLoop;
  AContext: TConvertContext): TJSElement;

Var
  F : TJSForStatement;
  L : TJSStatementList;
  I : TJSSimpleAssignStatement;
  V : TJSVarDeclaration;
  VD : TJSVariableStatement;
  u : TJSUNaryExpression;
  B : TJSBinaryExpression;
  MV : String;

begin
  Result:=Nil;
  B:=Nil;
  L:=TJSStatementList(CreateElement(TJSStatementList,El));
  Result:=L;
  try
    VD:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    L.A:=VD;
    V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
    VD.A:=V;
    MV:=TransFormVariableName(El.VariableName,AContext)+'$endloopvalue';
    V.Name:=MV;
    V.Init:=ConvertElement(EL.EndExpr,AContext);
    F:=TJSForStatement(CreateElement(TJSForStatement,El));
    L.B:=F;
    I:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El.StartExpr));
    F.Init:=I;
    I.LHS:=CreateIdentifierExpr(EL.VariableName,el,AContext);
    I.Expr:=ConvertElement(El.StartExpr,AContext);
    If El.Down then
      begin
      U:=TJSUnaryPostMinusMinusExpression(CreateElement(TJSUnaryPostMinusMinusExpression,El));
      B:=TJSRelationalExpressionGE(CreateElement(TJSRelationalExpressionGE,El.EndExpr));
      end
    else
      begin
      U:=TJSUnaryPostPlusPlusExpression(CreateElement(TJSUnaryPostPlusPlusExpression,El));
      B:=TJSRelationalExpressionLE(CreateElement(TJSRelationalExpressionLE,El.EndExpr));
      end;
    F.Incr:=U;
    F.Cond:=B;
    U.A:=CreateIdentifierExpr(EL.VariableName,El,AContext);
    B.A:=CreateIdentifierExpr(EL.VariableName,El,AContext);
    B.B:=CreateIdentifierExpr(MV,El.EndExpr,AContext);
    F.body:=ConvertElement(EL.Body);
  except
    FreeAndNil(Result);
    Raise;
  end;
end;

Function TPasToJSConverter.ConvertSimpleStatement(El: TPasImplSimple; AContext : TConvertContext): TJSElement;

Var
  E : TJSElement;

begin
  E:=ConvertElement(EL.Expr,AContext);
  Result:=TJSExpressionStatement(CreateElement(TJSExpressionStatement,El));
  TJSExpressionStatement(Result).A:=E;
end;

Function TPasToJSConverter.ConvertWithStatement(El: TPasImplWithDo; AContext : TConvertContext): TJSElement;

Var
  B,E : TJSElement;
  W,W2 : TJSWithStatement;
  I : Integer;

begin
  W:=Nil;
  Result:=Nil;
  if Assigned(El.Body) then
    B:=ConvertElement(El.Body,AContext)
  else
    B:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
  try
    For I:=0 to El.Expressions.Count-1 do
      begin
      E:=ConvertElement(TPasElement(El.Expressions[i]),AContext);
      W2:=TJSWithStatement(CreateElement(TJSWithStatement,TPasElement(El.Expressions[i])));
      if Not Assigned(Result) then // result is the first
        Result:=W2;
      if Assigned(W) then // Chain
        W.B:=W2;
      W:=W2; // W is the last
      W.A:=E;
      end;
  except
    FreeAndNil(E);
    FreeAndNil(Result);
    Raise;
  end;
  W.B:=B;
end;

Function TPasToJSConverter.GetExceptionObjectname(AContext: TConvertContext): String;

begin
  Result:='jsexception';
end;

Function TPasToJSConverter.ResolveType(El: TPasElement;
  AContext: TConvertContext): TPasType;
begin
  if EL is TPasType then
    Result:=TPasType(El) // TPasUnresolvedTypeRef needs handling here
  else
    Result:=Nil;
end;

Function TPasToJSConverter.ConvertExceptOn(El: TPasImplExceptOn; AContext : TConvertContext): TJSElement;

Var
  I : TJSIfStatement;
  IO : TJSRelationalExpressionInstanceOf;
  L : TJSStatementList;
  V : TJSVarDeclaration;

begin
  I:=TJSIfStatement(CreateElement(TJSIfStatement,El));
  IO:=TJSRelationalExpressionInstanceOf(CreateElement(TJSRelationalExpressionInstanceOf,EL));
  IO.A:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El,AContext);
  IO.B:=CreateIdentifierExpr(El.TypeName,El,AContext);
  I.Cond:=IO;
  L:=TJSStatementList(CreateElement(TJSStatementList,EL.Body));
  I.btrue:=L;
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,EL));
  L.A:=V;
  V.Name:=TransFormVariableName(EL.VariableName,AContext);//
  V.Init:=CreateIdentifierExpr(GetExceptionObjectName(AContext),EL,AContext);
  L.B:=TJSStatementList(CreateElement(TJSStatementList,EL.Body));
  L:=TJSStatementList(L.B);
  L.A:=ConvertElement(EL.Body,AContext);
  Result:=I;
end;

Function TPasToJSConverter.ConvertStatement(El: TPasImplStatement; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (El is TPasImplRaise) then
    Result:=ConvertRaiseStatement(TPasImplRaise(El),AContext)
  else if (El is TPasImplAssign) then
    Result:=ConvertAssignStatement(TPasImplAssign(El),AContext)
  else if (El is TPasImplWhileDo) then
    Result:=ConvertWhileStatement(TPasImplWhileDo(El),AContext)
  else if (El is TPasImplSimple) then
    Result:=ConvertSimpleStatement(TPasImplSimple(El),AContext)
  else if (El is TPasImplWithDo) then
    Result:=ConvertWithStatement(TPasImplWithDo(El),AContext)
  else if (El is TPasImplExceptOn) then
    Result:=ConvertExceptOn(TPasImplExceptOn(El),AContext)
  else if (El is TPasImplForLoop) then
    Result:=ConvertForStatement(TPasImplForLoop(El),AContext)
  else
    DoError('Unknown statement Class: %s',[El.ClassName]);
{
  TPasImplCaseStatement = class(TPasImplStatement)
}
end;


Function TPasToJSConverter.ConvertCommands(El: TPasImplCommands; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
  //  TPasImplCommands = class(TPasImplElement)
end;

Function TPasToJSConverter.ConvertLabelMark(El: TPasImplLabelMark; AContext : TConvertContext): TJSElement;

begin
  Result:=Nil;
 //    TPasImplLabelMark = class(TPasImplLabelMark) then
end;

Function TPasToJSConverter.ConvertElement(El: TPasElement; AContext : TConvertContext): TJSElement;
begin
  If (El is TPasPackage)  then
    Result:=ConvertPackage(TPasPackage(El),AContext)
  else If (El is TPasModule)  then
    Result:=ConvertModule(TPasModule(El),AContext)
  else if (EL is TPasExpr) then
    Result:=ConvertExpression(TPasExpr(El),AContext)
  else if (EL is TPasDeclarations) then
    Result:=ConvertDeclarations(TPasDeclarations(El),AContext)
  else if (EL is TPasType) then
    Result:=ConvertType(TPasType(El),AContext)
  else if (EL is TPasProcedure) then
    Result:=ConvertProcedure(TPasProcedure(El),AContext)
  else if (EL is TPasProcedureImpl) then
    Result:=ConvertProcedureImpl(TPasProcedureImpl(El),AContext)
  else if (EL is TPasImplBlock) then
    Result:=ConvertImplBlock(TPasImplBlock(El),AContext)
  else if (EL is TPasResString) then
    Result:=ConvertResString(TPasResString(El),AContext)
  else if (EL is TPasArgument) then
    Result:=ConvertArgument(TPasArgument(El),AContext)
  else if (EL is TPasResultElement) then
    Result:=ConvertResultElement(TPasResultElement(El),AContext)
  else if (EL is TPasConst) then
    Result:=ConvertConst(TPasConst(El),AContext)
  else if (EL is TPasProperty) then
    Result:=ConvertProperty(TPasProperty(El),AContext)
  else if (EL is TPasVariable) then
    Result:=ConvertVariable(TPasVariable(El),AContext)
  else if (EL is TPasExportSymbol) then
    Result:=ConvertExportSymbol(TPasExportSymbol(El),AContext)
  else if (EL is TPasLabels) then
    Result:=ConvertLabels(TPasLabels(El),AContext)
  else if (EL is TPasImplCommand) then
    Result:=ConvertCommand(TPasImplCommand(El),AContext)
  else if (EL is TPasImplCommands) then
    Result:=ConvertCommands(TPasImplCommands(El),AContext)
  else if (EL is TPasImplLabelMark) then
    Result:=ConvertLabelMark(TPasImplLabelMark(El),AContext);
end;

Procedure TPasToJSConverter.DoError(Const Msg: String);
begin
  Raise EPas2JS.Create(Msg);
end;

Procedure TPasToJSConverter.DoError(Const Msg: String;
  Const Args: Array of Const);
begin
  Raise EPas2JS.CreateFmt(Msg,Args);
end;

procedure TPasToJSConverter.SetCurrentContext(AValue: TJSElement);
begin
  if FCurrentContext=AValue then Exit;
  FCurrentContext:=AValue;
end;

Function TPasToJSConverter.CreateJSContext(AContext : TConvertContext) : TJSElement;

begin
  Result:=TJSObjectLiteral.Create(0,0);

end;

Function TPasToJSConverter.TransFormVariableName(Const AName: String;
  AContext: TConvertContext): String;
begin
  Result:=LowerCase(AName);
end;

Function TPasToJSConverter.TransFormVariableName(El: TPasElement; AContext: TConvertContext): String;
begin
  Result:=TransFormVariableName(EL.Name,AContext);
  // Add to context.
end;

Function TPasToJSConverter.TransFormFunctionName(El: TPasElement;
  AContext: TConvertContext): String;
begin
  Result:=LowerCase(EL.Name);
end;

Function TPasToJSConverter.ConvertElement(El: TPasElement): TJSElement;
begin
//  CurrentContext:=CreateJSContext(Nil);
  Result:=ConvertElement(El,Nil);
end;

end.

