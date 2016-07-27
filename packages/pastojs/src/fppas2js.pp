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
  Classes, SysUtils, jsbase, jstree, pastree, pparser;

Type
  EPas2JS = Class(Exception);

  { TPasToJSConverter }

  TConvertContext = Class(TObject)
  public
  end;

  TPasToJSConverter = Class(TObject)
  private
    FCurrentContext: TJSElement;
    FMainFunction: TJSString;
    FNameSpace: TJSString;
    Procedure AddToSourceElements(Src: TJSSourceElements; El: TJSElement);
    Function CreateConstDecl(El: TPasConst; AContext: TConvertContext): TJSElement;
    Function CreateBuiltInIdentifierExpr(AName: string): TJSPrimaryExpressionIdent;
    Function CreateIdentifierExpr(AName: string; El: TPasElement; AContext: TConvertContext): TJSPrimaryExpressionIdent;
    Function CreateTypeDecl(El: TPasType; AContext: TConvertContext): TJSElement;
    Function CreateVarDecl(El: TPasVariable; AContext: TConvertContext): TJSElement;
    procedure SetCurrentContext(AValue: TJSElement);
    procedure RaiseNotYetImplemented(El: TPasElement; AContext: TConvertContext);
  protected
    // helper functions
    Procedure DoError(Const Msg : String);
    Procedure DoError(Const Msg : String; Const Args : Array of Const);
    // Never create an element manually, always use the below function
    Function CreateElement(C: TJSElementClass; Src: TPasElement): TJSElement; virtual;
    Function GetExpressionValueType(El: TPasExpr; AContext: TConvertContext ): TJSType; virtual;
    Function GetPasIdentValueType(AName: String; AContext: TConvertContext): TJSType; virtual;
    Function TransFormIdent(El: TJSPrimaryExpressionIdent; AContext : TConvertContext): TJSPrimaryExpressionIdent;virtual;
    Function CreateJSContext(AContext : TConvertContext): TJSElement;virtual;
    Function TransformVariableName(Const AName :  String; AContext : TConvertContext) : String;
    Function TransformVariableName(El : TPasElement; AContext : TConvertContext) : String;
    Function TransformFunctionName(El : TPasElement; AContext : TConvertContext) : String;
    Function GetExceptionObjectName(AContext : TConvertContext) : string;
    Function ResolveType(El : TPasElement; AContext : TConvertContext) : TPasType;
    Function CreateCallStatement(const JSCallName: string; JSArgs: array of string): TJSCallExpression;
    Function CreateCallStatement(const FunNameEx: TJSElement; JSArgs: array of string): TJSCallExpression;
    Function CreateProcedureDeclaration(const El: TPasElement):TJSFunctionDeclarationStatement;
    Function CreateUnary(ms: array of string; E: TJSElement): TJSUnary;
    Function CreateMemberExpression(ReversedValues: array of string): TJSDotMemberExpression;
    Procedure AddProcedureToClass(sl: TJSStatementList; E: TJSElement;const P: TPasProcedure);
    Function GetFunctionDefinitionInUnary(const fd: TJSFunctionDeclarationStatement;const funname: TJSString; inunary: boolean): TJSFunctionDeclarationStatement;
    Function GetFunctionUnaryName(var je: TJSElement;out fundec: TJSFunctionDeclarationStatement): TJSString;
    Function CreateUsesList(UsesList: TFPList; AContext : TConvertContext): TJSArrayLiteral;
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
    Function ConvertProcedureImpl(El: TPasProcedureImpl; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertResString(El: TPasResString; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertResultElement(El: TPasResultElement; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertType(El: TPasElement; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertVariable(El: TPasVariable; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertElement(El : TPasElement; AContext : TConvertContext) : TJSElement; virtual;
    function ConvertClassType(const El: TPasClassType;const AContext: TConvertContext): TJSElement;
    Function ConvertClassMember(El: TPasElement;AContext: TConvertContext): TJSElement;
    Function ConvertClassConstructor(El: TPasConstructor;AContext: TConvertContext): TJSElement;
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

procedure TPasToJSConverter.AddToSourceElements(Src: TJSSourceElements;
  El: TJSElement);

Var
  List : TJSStatementList;
  AddEl : TJSElement;

begin
  While El<>nil do
  begin
    if EL is TJSStatementList then
      begin
      List:=EL as TJSStatementList;
      // List.A is first statement, List.B is next in list, chained.
      // -> add A, continue with B and free List
      AddEl:=List.A;
      EL:=List.B;
      List.A:=Nil;
      List.B:=Nil;
      FreeAndNil(List);
      end
    else
      begin
      AddEl:=EL;
      El:=Nil;
      end;
    Src.Statements.AddNode.Node:=AddEl;
  end;
end;

function TPasToJSConverter.ConvertModule(El: TPasModule;
  AContext: TConvertContext): TJSElement;
(* ToDo:
   rtl.module('<unitname>',
      [<interface uses1>,<uses2>, ...],
      function(uses,unit){
        <interface>
        this.impl={
            <implementation>
            $<unitname>_init:function(){
              <initialization>
              }
          };
      },
      [<implementation uses1>,<uses2>, ...]);
*)
Var
  Src : TJSSourceElements;
  RegModuleCall: TJSCallExpression;
  ArgArray: TJSArguments;
  UsesList: TFPList;
  FunDef: TJSFuncDef;
  FunBody: TJSFunctionBody;
  FunDecl: TJSFunctionDeclarationStatement;
  ArgEx: TJSLiteral;
begin
  Result:=Nil;
  if (El.ClassType=TPasModule) or (El is TPasUnitModule) then
    begin
      // create 'rtl.module(...)'
      // ToDo: fix missing semicolon
      RegModuleCall:=TJSCallExpression(CreateElement(TJSCallExpression,El));
      Result:=RegModuleCall;
      RegModuleCall.Expr:=CreateMemberExpression(['module','rtl']);
      ArgArray := TJSArguments.Create(0, 0, '');
      RegModuleCall.Args:=ArgArray;

      // add parameter: unitname
      ArgEx := TJSLiteral.Create(0,0,'');
      ArgEx.Value.AsString:=TJSString(TransformVariableName(El.Name,AContext));
      ArgArray.Elements.AddElement.Expr:=ArgEx;

      // add parameter: [<interface uses1>,<uses2>, ...]
      UsesList:=nil;
      if Assigned(El.InterfaceSection) then
        UsesList:=El.InterfaceSection.UsesList;
      ArgArray.Elements.AddElement.Expr:=CreateUsesList(UsesList,AContext);

      // add parameter: function(uses, unit){}
      FunDecl:=TJSFunctionDeclarationStatement.Create(0,0,'');
      ArgArray.Elements.AddElement.Expr:=FunDecl;
      FunDef:=TJSFuncDef.Create;
      FunDecl.AFunction:=FunDef;
      FunDef.Name:='';
      FunDef.Params.Add('uses');
      FunDef.Params.Add('unit');
      FunBody:=TJSFunctionBody.Create(0,0,'');
      FunDef.Body:=FunBody;
      Src:=TJSSourceElements(CreateElement(TJSSourceElements, El));
      FunBody.A:=Src;

      // add interface
      if Assigned(El.InterfaceSection) then
        AddToSourceElements(Src,ConvertElement(El.InterfaceSection,AContext));
      // ToDo: prefix/enclose implementation section
      if Assigned(El.ImplementationSection) then
        AddToSourceElements(Src,ConvertElement(El.ImplementationSection,AContext));
      // ToDo: prefix/enclose initialization section
      if Assigned(El.InitializationSection) then
        AddToSourceElements(Src,ConvertElement(El.InitializationSection,AContext));
      if Assigned(El.FinalizationSection) then
        raise Exception.Create('TPasToJSConverter.ConvertModule: finalization section is not supported');

      // add parameter: [<implementation uses1>,<uses2>, ...]
      UsesList:=nil;
      if Assigned(El.ImplementationSection) then
        UsesList:=El.ImplementationSection.UsesList;
      ArgArray.Elements.AddElement.Expr:=CreateUsesList(UsesList,AContext);
    end
  else
    begin
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
        //AddToSourceElements(Src,ConvertElement(El.FinalizationSection,AContext));
        raise Exception.Create('TPasToJSConverter.ConvertModule: finalization section is not supported');
    end;
{
TPasUnitModule = Class(TPasModule)
TPasLibrary = class(TPasModule)
}
end;

function TPasToJSConverter.CreateElement(C: TJSElementClass; Src: TPasElement
  ): TJSElement;

begin
  if Assigned(Src) then
    Result:=C.Create(Src.SourceLinenumber,1,Src.SourceFilename)
  else
    Result:=C.Create(0,0,'');
end;

function TPasToJSConverter.ConvertUnaryExpression(El: TUnaryExpr;
  AContext: TConvertContext): TJSElement;

Var
  U : TJSUnaryExpression;
  E : TJSElement;

begin
  if AContext=nil then ;
  Result:=Nil;
  E:=ConvertElement(El.Operand);
  Case El.OpCode of
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
    else
      // ToDo: write Pascal source position
      DoError('TPasToJSConverter.ConvertUnaryExpression: OpCode not yet supported: '+IntToStr(ord(El.OpCode)));
  end;
  Result:=U;
end;

function TPasToJSConverter.ConvertCallExpression(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
begin
  if AContext=nil then ;
  // ToDo: call function
  Raise EPasToJS.CreateFmt(SErrUnexpected,[EL.ClassName]);
  Result:=nil;
end;

function TPasToJSConverter.TransFormStringLiteral(S: String): String;
begin
  // ToDo: This needs some better algorithm that handles #13"abc etc.
  Result:=Copy(S,2,Length(S)-2);
  Result:=StringReplace(Result,'''''','''',[rfReplaceAll]);
end;


function TPasToJSConverter.GetPasIdentValueType(AName: String;
  AContext: TConvertContext): TJSType;

begin
  if AContext=nil then ;
  if AName='' then ;
  Result:=jstUNDEFINED;
end;


function TPasToJSConverter.GetExpressionValueType(El: TPasExpr;
  AContext: TConvertContext): TJSType;

  Function CombineValueType(A,B : TJSType) : TJSType;

  begin
    If (A=jstUNDEFINED) then
      Result:=B
    else if (B=jstUNDEFINED) then
      Result:=A
    else
      Result:=A; // pick the first
  end;

Var
  A,B : TJSType;

begin
  if (El is TBoolConstExpr) then
    Result:=jstBoolean
  else  If (El is TPrimitiveExpr) then
    begin
    Case El.Kind of
      pekIdent : Result:=GetPasIdentValueType(El.Name,AContext);
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
  else if (El is TUnaryExpr) then
    Result:=GetExpressionValueType(TUnaryExpr(El).Operand,AContext)
  else if (El is TBinaryExpr) then
    begin
    A:=GetExpressionValueType(TBinaryExpr(El).Left,AContext);
    B:=GetExpressionValueType(TBinaryExpr(El).Right,AContext);
    Result:=CombineValueType(A,B);
    end
  else
    result:=jstUndefined
end;

function TPasToJSConverter.ConvertBinaryExpression(El: TBinaryExpr;
  AContext: TConvertContext): TJSElement;

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
  funname: string;
  ok: Boolean;
begin
  Result:=Nil;
  C:=BinClasses[EL.OpCode];
  A:=ConvertElement(EL.left,AContext);
  ok:=false;
  try
    B:=ConvertElement(EL.right,AContext);
    ok:=true;
  finally
    if not ok then
      FreeAndNil(A);
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
        if (B is TJSPrimaryExpressionIdent) then
        begin
          Result := TJSDotMemberExpression(CreateElement(TJSDotMemberExpression, El));
          TJSDotMemberExpression(Result).MExpr := A;
          TJSDotMemberExpression(Result).Name := TJSPrimaryExpressionIdent(B).Name;
          FreeAndNil(B);
        end;
        if (B is TJSCallExpression) then
        begin
          Result := B;
          funname := String(TJSPrimaryExpressionIdent(TJSCallExpression(B).Expr).Name);
          TJSCallExpression(B).Expr :=
            TJSDotMemberExpression(CreateElement(TJSDotMemberExpression, El));
          TJSDotMemberExpression(TJSCallExpression(B).Expr).MExpr := A;
          TJSDotMemberExpression(TJSCallExpression(B).Expr).Name := TJSString(funname);
        end;
        if not ((B is TJSPrimaryExpressionIdent) or (B is TJSCallExpression)) then;
        // DOError('Member expression must be an identifier');
      end
      else
        if (A is TJSPrimaryExpressionIdent) and
          (TJSPrimaryExpressionIdent(A).Name = '_super') then
        begin
          Result := B;
          funname := String(TJSPrimaryExpressionIdent(TJSCallExpression(b).Expr).Name);
          TJSCallExpression(b).Args.Elements.AddElement.Expr :=
                                            CreateBuiltInIdentifierExpr('self');
          if TJSCallExpression(b).Args.Elements.Count > 1 then
            TJSCallExpression(b).Args.Elements.Exchange(
              0, TJSCallExpression(b).Args.Elements.Count - 1);
          if CompareText(funname, 'Create') = 0 then
          begin
            TJSCallExpression(B).Expr :=
              TJSDotMemberExpression(CreateElement(TJSDotMemberExpression, El));
            TJSDotMemberExpression(TJSCallExpression(b).Expr).MExpr := A;
            TJSDotMemberExpression(TJSCallExpression(b).Expr).Name := TJSString(funname);
          end
          else
          begin
            TJSCallExpression(B).Expr :=
              CreateMemberExpression(['call', funname, 'prototype', '_super']);
          end;
        end
        else
          // ToDo: show source position
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

function TPasToJSConverter.TransFormIdent(El: TJSPrimaryExpressionIdent;
  AContext: TConvertContext): TJSPrimaryExpressionIdent;

begin
  if AContext=nil then ;
  EL.Name:=LowerCase(EL.Name);
  Result:=El;
end;


function TPasToJSConverter.CreateIdentifierExpr(AName: string; El: TPasElement;
  AContext: TConvertContext): TJSPrimaryExpressionIdent;

Var
  I : TJSPrimaryExpressionIdent;

begin
  I:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,El));
  I.Name:=TJSString(AName);
  Result:=TransFormIdent(I,AContext);
end;

function TPasToJSConverter.ConvertPrimitiveExpression(El: TPrimitiveExpr;
  AContext: TConvertContext): TJSElement;

Var
  L : TJSLiteral;
  Number : TJSNumber;
  ConversionError : Integer;

begin
  Result:=Nil;
  case El.Kind of
    pekString:
      begin
      L:=TJSLiteral(CreateElement(TJSLiteral,El));
      L.Value.AsString:=TJSString(TransFormStringLiteral(El.Value));
      Result:=L;
      end;
    pekNumber:
      begin
      L:=TJSLiteral(CreateElement(TJSLiteral,El));
      Val(El.Value,Number,ConversionError);
      if ConversionError<>0 then
        DoError('Invalid number: %s',[EL.Value]);
      L.Value.AsNumber:=Number;
      Result:=L;
      end;
    pekIdent:
      begin
      Result:=CreateIdentifierExpr(El.Value,El,AContext);
      end;
  end;
end;


function TPasToJSConverter.ConvertBoolConstExpression(El: TBoolConstExpr;
  AContext: TConvertContext): TJSElement;

Var
  L : TJSLiteral;

begin
  if AContext=nil then ;
  Result:=Nil;
  L:=TJSLiteral(CreateElement(TJSLiteral,El));
  L.Value.AsBoolean:=EL.Value;
  Result:=L;
end;

function TPasToJSConverter.ConvertNilExpr(El: TNilExpr;
  AContext: TConvertContext): TJSElement;

Var
  L : TJSLiteral;

begin
  if AContext=nil then ;
  L:=TJSLiteral(CreateElement(TJSLiteral,El));
  L.Value.IsNull:=True;
  Result:=L;
end;

function TPasToJSConverter.ConvertInheritedExpression(El: TInheritedExpr;
  AContext: TConvertContext): TJSElement;
var
   je: TJSPrimaryExpressionIdent;
begin
  if AContext=nil then ;
  if El=nil then;
  je := CreateIdentifierExpr('_super',El,AContext);
  Result := je;
  // ToDo: TInheritedExpr = class(TPasExpr)
end;

function TPasToJSConverter.ConvertSelfExpression(El: TSelfExpr;
  AContext: TConvertContext): TJSElement;

begin
  if AContext=nil then ;
  Result:=TJSPrimaryExpressionThis(CreateElement(TJSPrimaryExpressionThis,El));
end;

function TPasToJSConverter.ConvertParamsExpression(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;

Var
  b: TJSBracketMemberExpression;
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
    B:=TJSBracketMemberExpression(CreateElement(TJSBracketMemberExpression,El));
    B.MExpr:=ConvertElement(El.Value,AContext);
    Result:=B;
    B.Name:=ConvertElement(EL.Params[0],AContext);
    end
  end;
end;

function TPasToJSConverter.ConvertRecordValues(El: TRecordValues;
  AContext: TConvertContext): TJSElement;

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
    Rel.Name:=TJSString(it.Name);
    Rel.Expr:=ConvertElement(it.ValueExp,AContext);
    end;
  Result:=R;
end;

function TPasToJSConverter.ConvertArrayValues(El: TArrayValues;
  AContext: TConvertContext): TJSElement;

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

function TPasToJSConverter.ConvertExpression(El: TPasExpr;
  AContext: TConvertContext): TJSElement;

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

function TPasToJSConverter.CreateConstDecl(El: TPasConst;
  AContext: TConvertContext): TJSElement;

Var
  C : TJSElement;
  V : TJSVariableStatement;

begin
  C:=ConvertElement(El,AContext);
  V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
  V.A:=C;
  Result:=V;
end;

function TPasToJSConverter.CreateBuiltInIdentifierExpr(AName: string
  ): TJSPrimaryExpressionIdent;
var
  Ident: TJSPrimaryExpressionIdent;
begin
  Ident:=TJSPrimaryExpressionIdent.Create(0,0,'');
  Ident.Name:=TJSString(AName);
  Result:=Ident;
end;

function TPasToJSConverter.CreateTypeDecl(El: TPasType;
  AContext: TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (El is TPasClassType) then
    Result := ConvertClassType(TPasClassType(El), AContext);
  // ToDo: Need to do something for classes and records.
end;

function TPasToJSConverter.CreateVarDecl(El: TPasVariable;
  AContext: TConvertContext): TJSElement;

Var
  C : TJSElement;
  V : TJSVariableStatement;

begin
  C:=ConvertElement(El,AContext);
  V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
  V.A:=C;
  Result:=V;
end;

function TPasToJSConverter.ConvertDeclarations(El: TPasDeclarations;
  AContext: TConvertContext): TJSElement;

Var
  SL : TJSStatementList;
  E : TJSElement;

  Procedure AddToStatementList;

  var
    SL2: TJSStatementList;
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

Var
  P : TPasElement;
  I : Integer;
begin
  if (El.Declarations.Count=0) then
    exit(nil);

  SL:=TJSStatementList(CreateElement(TJSStatementList,El));
  Result:=SL;
  For I:=0 to El.Declarations.Count-1 do
    begin
    E:=Nil;
    P:=TPasElement(El.Declarations[i]);
    if P is TPasConst then
      E:=CreateConstDecl(TPasConst(P),AContext)
    else if P is TPasVariable then
      E:=CreateVarDecl(TPasVariable(P),AContext)
    else if P is TPasType then
      E:=CreateTypeDecl(TPasType(P),AContext)
    else if P is TPasProcedure then
      E:=ConvertElement(P,AContext)
    else
      DoError('Unknown class: "%s" ',[P.ClassName]);
    if (Pos('.', P.Name) > 0) then
      AddProcedureToClass(TJSStatementList(Result), E, P as TPasProcedure)
    else
    AddToStatementList;
    end;
  if (El is TProcedureBody) then
    begin
    E:=ConvertElement(TProcedureBody(El).Body,AContext);
    AddToStatementList;
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

function TPasToJSConverter.ConvertType(El: TPasElement;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
{
  ToDo:
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

function TPasToJSConverter.ConvertClassType(const El: TPasClassType;
  const AContext: TConvertContext): TJSElement;
var
  call: TJSCallExpression;
  asi: TJSSimpleAssignStatement;
  unary2: TJSUnary;
  unary: TJSUnary;
  je: TJSElement;
  FD: TJSFuncDef;
  cons: TJSFunctionDeclarationStatement;
  FS: TJSFunctionDeclarationStatement;
  tmember: TPasElement;
  j: integer;
  ret: TJSReturnStatement;
  jsName: String;
begin
  //ctname := El.FullName;
  jsName:=TransformVariableName(El.Name,AContext);
  unary := TJSUnary(CreateElement(TJSUnary,El));
  asi := TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
  unary.A := asi;
  asi.LHS := CreateIdentifierExpr(El.Name,El,AContext);
  FS := TJSFunctionDeclarationStatement(
    CreateElement(TJSFunctionDeclarationStatement, El));
  call := CreateCallStatement(FS, []);
  asi.Expr := call;
  Result := unary;
  FD := TJSFuncDef.Create;
  FS.AFunction := FD;
  FD.Body := TJSFunctionBody(CreateElement(TJSFunctionBody, El));
  FD.Body.A := TJSSourceElements(CreateElement(TJSSourceElements, El));
  if Assigned(El.AncestorType) then
  begin
    call.Args := TJSArguments(CreateElement(TJSArguments, El));
    call.Args.Elements.AddElement.Expr := CreateIdentifierExpr(El.AncestorType.Name,El,AContext);
    FD.Params.Add('_super');
    unary2 := TJSUnary(CreateElement(TJSUnary, El));
    call := CreateCallStatement('__extends', [jsName, '_super']);
    unary2.A := call;
    TJSSourceElements(FD.Body.A).Statements.AddNode.Node := unary2;
  end;
  //create default onstructor
  cons := CreateProcedureDeclaration(El);
  TJSSourceElements(FD.Body.A).Statements.AddNode.Node := cons;
  cons.AFunction.Name := TJSString(jsName);

  //convert class member
  for j := 0 to El.Members.Count - 1 do
  begin
    tmember := TPasElement(El.Members[j]);
    //memname := tmember.FullName;
    je := ConvertClassMember(tmember, AContext);
    if Assigned(je) then
      TJSSourceElements(FD.Body.A).Statements.AddNode.Node := je;
  end;

  //add return statement
  ret := TJSReturnStatement(CreateElement(TJSReturnStatement, El));
  TJSSourceElements(FD.Body.A).Statements.AddNode.Node := ret;
  ret.Expr := CreateIdentifierExpr(El.Name,El,AContext);
  Result := unary;
end;

function TPasToJSConverter.ConvertClassMember(El: TPasElement;
  AContext: TConvertContext): TJSElement;
var
  FS: TJSFunctionDeclarationStatement;
begin
  Result := nil;
  if (El is TPasProcedure) and (not (El is TPasConstructor)) then
  begin
    FS := CreateProcedureDeclaration(El);
    Result := CreateUnary([TPasProcedure(El).Name, 'prototype',
      El.Parent.FullName], FS);
  end;
  if (El is TPasConstructor)then
  begin
    Result:=ConvertClassConstructor(TPasClassConstructor(El),AContext);
  end;
  if (El is TPasProperty) then
    ConvertProperty(TPasProperty(El), AContext);
end;

function TPasToJSConverter.ConvertClassConstructor(El: TPasConstructor;
  AContext: TConvertContext): TJSElement;
var
  FS: TJSFunctionDeclarationStatement;
  n: integer;
  fun1sourceele: TJSSourceElements;
  ret: TJSReturnStatement;
  nmem: TJSNewMemberExpression;
  Arg: TPasArgument;
begin
  if AContext=nil then ;
  FS := CreateProcedureDeclaration(El);
  FS.AFunction.Name := TJSString(El.Name);
  Fs.AFunction.Body := TJSFunctionBody(CreateElement(TJSFunctionBody, EL.Body));
  fun1sourceele := TJSSourceElements.Create(0, 0, '');
  fs.AFunction.Body.A := fun1sourceele;
  ret := TJSReturnStatement.Create(0, 0, '');
  fun1sourceele.Statements.AddNode.Node := ret;
  nmem := TJSNewMemberExpression.Create(0, 0, '');
  ret.Expr := nmem;
  nmem.MExpr := CreateIdentifierExpr(El.Parent.FullName,El.Parent,AContext);
  for n := 0 to El.ProcType.Args.Count - 1 do
  begin
    if n = 0 then
      nmem.Args := TJSArguments.Create(0, 0, '');
    fs.AFunction.Params.Add(TPasArgument(El.ProcType.Args[n]).Name);
    Arg := TPasArgument(El.ProcType.Args[n]);
    nmem.Args.Elements.AddElement.Expr := CreateIdentifierExpr(Arg.Name,Arg,AContext);
  end;
  Result := CreateUnary([TPasProcedure(El).Name, El.Parent.FullName], FS);
end;

function TPasToJSConverter.ConvertProcedure(El: TPasProcedure;
  AContext: TConvertContext): TJSElement;

Var
  FS : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;
  n:Integer;

begin
  FS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,EL));
  Result:=FS;
  FD:=TJSFuncDef.Create;
  FD.Name:=TJSString(TransformFunctionName(El,AContext));
  FS.AFunction:=FD;
  for n := 0 to El.ProcType.Args.Count - 1 do
    FD.Params.Add(TPasArgument(El.ProcType.Args[0]).Name);
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

function TPasToJSConverter.ConvertProcedureImpl(El: TPasProcedureImpl;
  AContext: TConvertContext): TJSElement;

Var
  FS : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;

begin
  FS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,EL));
  Result:=FS;
  FD:=TJSFuncDef.Create;
  FD.Name:=TJSString(TransformFunctionName(El,AContext));
  FS.AFunction:=FD;
  FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,EL.Body));
  FD.Body.A:=ConvertElement(El.Body,AContext);
{
  TPasProcedureImpl = class(TPasElement)
  TPasConstructorImpl = class(TPasProcedureImpl)
  TPasDestructorImpl = class(TPasProcedureImpl)
}
end;

function TPasToJSConverter.ConvertBeginEndStatement(El: TPasImplBeginBlock;
  AContext: TConvertContext): TJSElement;

begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

function TPasToJSConverter.ConvertImplBlockElements(El: TPasImplBlock;
  AContext: TConvertContext): TJSElement;

var
  B : TJSElement;
  S,S2 : TJSStatementList;
  I : Integer;

begin
  if Not (Assigned(El.Elements) and (El.Elements.Count>0)) then
    Result:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El))
  else
    begin
    S:=TJSStatementList(CreateElement(TJSStatementList,TPasImplElement(El)));
    Result:=S;
    For I:=0 to El.Elements.Count-1 do
      begin
      B:=ConvertElement(TPasImplElement(El.Elements[i]),AContext);
      if not Assigned(S.A) then
        S.A:=B
      else
        begin
        if Assigned(S.B) then
          begin
          S2:=TJSStatementList(CreateElement(TJSStatementList,TPasImplElement(El.Elements[i])));
          S2.A:=S.B;
          S.B:=S2;
          S:=S2;
          end;
        S.B:=B;
        end;
      end;
    end;
end;

function TPasToJSConverter.ConvertInitializationSection(
  El: TInitializationSection; AContext: TConvertContext): TJSElement;
begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

function TPasToJSConverter.ConvertFinalizationSection(El: TFinalizationSection;
  AContext: TConvertContext): TJSElement;
begin
  // this is not really supported by JavaScript
  Result:=ConvertImplBlockElements(El,AContext);
end;

function TPasToJSConverter.ConvertTryStatement(El: TPasImplTry;
  AContext: TConvertContext): TJSElement;

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
      T.Ident:=TJSString(GetExceptionObjectName(AContext));
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

function TPasToJSConverter.ConvertTryFinallyStatement(El: TPasImplTryFinally;
  AContext: TConvertContext): TJSElement;


begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

function TPasToJSConverter.ConvertTryExceptStatement(El: TPasImplTryExcept;
  AContext: TConvertContext): TJSElement;


begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

function TPasToJSConverter.ConvertImplBlock(El: TPasImplBlock;
  AContext: TConvertContext): TJSElement;

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

function TPasToJSConverter.ConvertPackage(El: TPasPackage;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo TPasPackage = class(TPasElement)
end;

function TPasToJSConverter.ConvertResString(El: TPasResString;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasResString
end;

function TPasToJSConverter.ConvertArgument(El: TPasArgument;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasArgument
end;

function TPasToJSConverter.ConvertResultElement(El: TPasResultElement;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasResultElement
end;

function TPasToJSConverter.ConvertVariable(El: TPasVariable;
  AContext: TConvertContext): TJSElement;

Var
  V : TJSVarDeclaration;
  T : TPasType;
begin
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
  V.Name:=TransformVariableName(EL,AContext);
  T:=ResolveType(EL.VarType,AContext);
  if (T is TPasArrayType) then
    begin
    V.Init:=TJSArrayLiteral(CreateElement(TJSArrayLiteral,EL.VarType));
    If Assigned(EL.Expr) then
      Raise EPasToJS.Create(SerrInitalizedArray);
    end
  else If Assigned(EL.Expr) then
    V.Init:=ConvertElement(El.Expr,AContext)
  else
    ; // ToDo: init with default value to create a typed variable (faster)
  Result:=V;
end;

function TPasToJSConverter.ConvertConst(El: TPasConst; AContext: TConvertContext
  ): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasConst
end;

function TPasToJSConverter.ConvertProperty(El: TPasProperty;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasProperty = class(TPasVariable)
end;

function TPasToJSConverter.ConvertExportSymbol(El: TPasExportSymbol;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasExportSymbol
end;

function TPasToJSConverter.ConvertLabels(El: TPasLabels;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasLabels = class(TPasImplElement)
end;

function TPasToJSConverter.ConvertRaiseStatement(El: TPasImplRaise;
  AContext: TConvertContext): TJSElement;

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

function TPasToJSConverter.ConvertAssignStatement(El: TPasImplAssign;
  AContext: TConvertContext): TJSElement;

Var
  LHS,RHS : TJSElement;
  T : TJSAssignStatement;
  ok: Boolean;

begin
  if AContext=nil then ;
  LHS:=ConvertElement(El.left);
  ok:=false;
  try
    RHS:=ConvertElement(El.Right);
    ok:=true;
  finally
    if not ok then
      FreeAndNil(LHS);
  end;
  T:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
  T.Expr:=RHS;
  T.LHS:=LHS;
  Result:=T;
end;

function TPasToJSConverter.ConvertCommand(El: TPasImplCommand;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasImplCommand = class(TPasImplElement)
end;

function TPasToJSConverter.ConvertIfStatement(El: TPasImplIfElse;
  AContext: TConvertContext): TJSElement;

Var
  C,BThen,BElse : TJSElement;
  T : TJSIfStatement;
  ok: Boolean;

begin
  if AContext=nil then ;
  C:=Nil;
  BThen:=Nil;
  BElse:=Nil;
  ok:=false;
  try
    C:=ConvertElement(El.ConditionExpr);
    if Assigned(El.IfBranch) then
      BThen:=ConvertElement(El.IfBranch)
    else
      BThen:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
    if Assigned(El.ElseBranch) then
      BElse:=ConvertElement(El.ElseBranch);
    ok:=true;
  finally
    if not ok then
      begin
      FreeAndNil(C);
      FreeAndNil(BThen);
      FreeAndNil(BElse);
      end;
  end;
  T:=TJSIfStatement(CreateElement(TJSIfStatement,El));
  T.Cond:=C;
  T.BTrue:=BThen;
  T.BFalse:=BElse;
  Result:=T;
end;

function TPasToJSConverter.ConvertWhileStatement(El: TPasImplWhileDo;
  AContext: TConvertContext): TJSElement;

Var
  C : TJSElement;
  B : TJSElement;
  W : TJSWhileStatement;
  ok: Boolean;
begin
  Result:=Nil;
  C:=Nil;
  B:=Nil;
  ok:=false;
  try
    C:=ConvertElement(EL.ConditionExpr,AContext);
    if Assigned(EL.Body) then
      B:=ConvertElement(EL.Body,AContext)
    else
      B:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
    ok:=true;
  finally
    if not ok then
      begin
      FreeAndNil(B);
      FreeAndNil(C);
      end;
  end;
  W:=TJSWhileStatement(CreateElement(TJSWhileStatement,El));
  W.Cond:=C;
  W.Body:=B;
  Result:=W;
end;

function TPasToJSConverter.ConvertRepeatStatement(El: TPasImplRepeatUntil;
  AContext: TConvertContext): TJSElement;
Var
  C : TJSElement;
  N : TJSUnaryNotExpression;
  W : TJSDoWhileStatement;
  B : TJSElement;
  ok: Boolean;

begin
  Result:=Nil;
  C:=Nil;
  B:=Nil;
  ok:=false;
  try
    C:=ConvertElement(EL.ConditionExpr,AContext);
    N:=TJSUnaryNotExpression(CreateElement(TJSUnaryNotExpression,EL.ConditionExpr));
    N.A:=C;
    B:=ConvertImplBlockElements(El,AContext);
    ok:=true;
  finally
    if not ok then
      begin
      FreeAndNil(B);
      FreeAndNil(C);
      end;
  end;
  W:=TJSDoWhileStatement(CreateElement(TJSDoWhileStatement,El));
  W.Cond:=N;
  W.Body:=B;
  Result:=W;
end;

function TPasToJSConverter.ConvertForStatement(El: TPasImplForLoop;
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
  ok: Boolean;

begin
  Result:=Nil;
  B:=Nil;
  L:=TJSStatementList(CreateElement(TJSStatementList,El));
  Result:=L;
  ok:=false;
  try
    VD:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    L.A:=VD;
    V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
    VD.A:=V;
    MV:=TransformVariableName(El.VariableName,AContext)+'$endloopvalue';
    V.Name:=MV;
    V.Init:=ConvertElement(El.EndExpr,AContext);
    F:=TJSForStatement(CreateElement(TJSForStatement,El));
    L.B:=F;
    I:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El.StartExpr));
    F.Init:=I;
    I.LHS:=CreateIdentifierExpr(El.VariableName,El,AContext);
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
    U.A:=CreateIdentifierExpr(El.VariableName,El,AContext);
    B.A:=CreateIdentifierExpr(El.VariableName,El,AContext);
    B.B:=CreateIdentifierExpr(MV,El.EndExpr,AContext);
    F.Body:=ConvertElement(El.Body);
    ok:=true;
  finally
    if not ok then
      FreeAndNil(Result);
  end;
end;

function TPasToJSConverter.ConvertSimpleStatement(El: TPasImplSimple;
  AContext: TConvertContext): TJSElement;

Var
  E : TJSElement;

begin
  E:=ConvertElement(EL.Expr,AContext);
  Result:=TJSExpressionStatement(CreateElement(TJSExpressionStatement,El));
  TJSExpressionStatement(Result).A:=E;
end;

function TPasToJSConverter.ConvertWithStatement(El: TPasImplWithDo;
  AContext: TConvertContext): TJSElement;

Var
  B,E : TJSElement;
  W,W2 : TJSWithStatement;
  I : Integer;
  ok: Boolean;

begin
  W:=Nil;
  Result:=Nil;
  if Assigned(El.Body) then
    B:=ConvertElement(El.Body,AContext)
  else
    B:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
  ok:=false;
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
    ok:=true;
  finally
    if not ok then
      begin
      FreeAndNil(E);
      FreeAndNil(Result);
      end;
  end;
  W.B:=B;
end;

function TPasToJSConverter.GetExceptionObjectName(AContext: TConvertContext
  ): string;
begin
  if AContext=nil then ;
  Result:='ExceptObject'; // use the same as the FPC RTL
end;

function TPasToJSConverter.ResolveType(El: TPasElement;
  AContext: TConvertContext): TPasType;
begin
  if AContext=nil then ;
  if EL is TPasType then
    Result:=TPasType(El) // TPasUnresolvedTypeRef needs handling here
  else
    Result:=Nil;
end;
function TPasToJSConverter.CreateCallStatement(const JSCallName: string;
  JSArgs: array of string): TJSCallExpression;
var
  Call: TJSCallExpression;
  Ident: TJSPrimaryExpressionIdent;
begin
  Ident := TJSPrimaryExpressionIdent.Create(0, 0, '');
  Ident.Name := TJSString(JSCallName);
  Call := CreateCallStatement(Ident, JSArgs);
  Result := Call;
end;

function TPasToJSConverter.CreateCallStatement(const FunNameEx: TJSElement;
  JSArgs: array of string): TJSCallExpression;
var
  p: string;
  ArgEx: TJSPrimaryExpressionIdent;
  Call: TJSCallExpression;
  ArgArray: TJSArguments;
begin
  Call := TJSCallExpression.Create(0, 0, '');
  Call.Expr := FunNameEx;
  ArgArray := TJSArguments.Create(0, 0, '');
  Call.Args := ArgArray;
  for p in JSArgs do
  begin
    ArgEx := TJSPrimaryExpressionIdent.Create(0, 0, '');
    ArgEx.Name := TJSString(p);
    ArgArray.Elements.AddElement.Expr := ArgEx;
  end;
  Result := Call;
end;

function TPasToJSConverter.CreateUnary(ms: array of string; E: TJSElement): TJSUnary;
var
  unary: TJSUnary;
  asi: TJSSimpleAssignStatement;
begin
  unary := TJSUnary.Create(0, 0, '');
  asi := TJSSimpleAssignStatement.Create(0, 0, '');
  unary.A := asi;
  asi.Expr := E;
  asi.LHS := CreateMemberExpression(ms);
  Result := unary;
end;

function TPasToJSConverter.CreateMemberExpression(ReversedValues: array of string): TJSDotMemberExpression;
var
  pex: TJSPrimaryExpressionIdent;
  MExpr: TJSDotMemberExpression;
  LastMExpr: TJSDotMemberExpression;
  k: integer;
begin
  if Length(ReversedValues) < 2 then
    DoError('member expression with less than two members');
  LastMExpr := nil;
  for k:=Low(ReversedValues) to High(ReversedValues)-1 do
  begin
    MExpr := TJSDotMemberExpression.Create(0, 0, '');
    MExpr.Name := TJSString(ReversedValues[k]);
    if k = 0 then
      Result := MExpr
    else
      LastMExpr.MExpr := MExpr;
    LastMExpr := MExpr;
  end;
  pex := TJSPrimaryExpressionIdent.Create(0, 0, '');
  pex.Name := TJSString(ReversedValues[High(ReversedValues)]);
  LastMExpr.MExpr := pex;
end;

procedure TPasToJSConverter.AddProcedureToClass(sl: TJSStatementList;
  E: TJSElement; const P: TPasProcedure);
var
  clname, funname: string;
  classfound: boolean;
  fundec, fd, main_const: TJSFunctionDeclarationStatement;
  SL2: TJSStatementList;
  un1: TJSUnary;
  asi: TJSAssignStatement;
  varname: TJSString;
begin
  SL2 := TJSStatementList(sl);
  clname := Copy(p.Name, 1, Pos('.', P.Name) - 1);
  funname := Copy(p.Name, Pos('.', P.Name) + 1, Length(p.Name) - Pos('.', P.Name));
  classfound := False;
  while Assigned(SL2) and (not classfound) do
  begin
    if SL2.A is TJSUnary then
    begin
      un1 := TJSUnary(SL2.A);
      asi := TJSAssignStatement(un1.A);
      varname := TJSPrimaryExpressionIdent(asi.LHS).Name;
      if varname = TJSString(clname) then
      begin
        classfound := True;
        fd := TJSFunctionDeclarationStatement(TJSCallExpression(asi.Expr).Expr);
      end;
    end;
    SL2 := TJSStatementList(SL2.B);
  end;

  if not (classfound) then
    Exit;

  fundec := GetFunctionDefinitionInUnary(fd, TJSString(funname), True);
  if Assigned(fundec) then
  begin
    if (p is TPasConstructor) then
    begin
      main_const := GetFunctionDefinitionInUnary(fd, TJSString(clname), False);
      main_const.AFunction := TJSFunctionDeclarationStatement(E).AFunction;
      main_const.AFunction.Name := TJSString(clname);
    end
    else
    begin
      fundec.AFunction := TJSFunctionDeclarationStatement(E).AFunction;
      fundec.AFunction.Name := '';
    end;
  end;
end;

function TPasToJSConverter.GetFunctionDefinitionInUnary(
  const fd: TJSFunctionDeclarationStatement; const funname: TJSString;
  inunary: boolean): TJSFunctionDeclarationStatement;
var
  k: integer;
  fundec: TJSFunctionDeclarationStatement;
  je: TJSElement;
  cname: TJSString;
begin
  Result := nil;
  for k := 0 to TJSSourceElements(FD.AFunction.Body.A).Statements.Count - 1 do
  begin
    je := TJSSourceElements(FD.AFunction.Body.A).Statements.Nodes[k].Node;
    if inunary then
      cname := GetFunctionUnaryName(je, fundec)
    else
    begin
      if je is TJSFunctionDeclarationStatement then
      begin
        cname := TJSFunctionDeclarationStatement(je).AFunction.Name;
        fundec := TJSFunctionDeclarationStatement(je);
      end;
    end;
    if funname = cname then
      Result := fundec;
  end;
end;

function TPasToJSConverter.GetFunctionUnaryName(var je: TJSElement;
  out fundec: TJSFunctionDeclarationStatement): TJSString;
var
  cname: TJSString;
  asi: TJSAssignStatement;
  un1: TJSUnary;
begin
  fundec:=nil;
  if not (je is TJSUnary) then
    Exit;
  un1 := TJSUnary(je);
  asi := TJSAssignStatement(un1.A);
  if not (asi.Expr is TJSFunctionDeclarationStatement) then
    Exit;
  fundec := TJSFunctionDeclarationStatement(asi.Expr);
  cname := TJSDotMemberExpression(asi.LHS).Name;
  Result := cname;
end;

function TPasToJSConverter.CreateUsesList(UsesList: TFPList;
  AContext: TConvertContext): TJSArrayLiteral;
var
  ArgArray: TJSArrayLiteral;
  k: Integer;
  El: TPasElement;
  anUnitName: String;
  ArgEx: TJSLiteral;
begin
  ArgArray:=TJSArrayLiteral.Create(0,0,'');
  if UsesList<>nil then
    for k:=0 to UsesList.Count-1 do
      begin
      El:=TPasElement(UsesList[k]);
      if not (El is TPasModule) then continue;
      anUnitName := TransformVariableName(TPasModule(El).Name,AContext);
      ArgEx := TJSLiteral.Create(0,0,'');
      ArgEx.Value.AsString:=TJSString(anUnitName);
      ArgArray.Elements.AddElement.Expr := ArgEx;
      end;
  Result:=ArgArray;
end;

function TPasToJSConverter.CreateProcedureDeclaration(const El: TPasElement):
TJSFunctionDeclarationStatement;
var
  FD: TJSFuncDef;
  FS: TJSFunctionDeclarationStatement;
begin
  FS := TJSFunctionDeclarationStatement(
    CreateElement(TJSFunctionDeclarationStatement, EL));
  Result := FS;
  FD := TJSFuncDef.Create;
  FS.AFunction := FD;
  Result := FS;
end;
function TPasToJSConverter.ConvertExceptOn(El: TPasImplExceptOn;
  AContext: TConvertContext): TJSElement;

Var
  I : TJSIfStatement;
  IO : TJSRelationalExpressionInstanceOf;
  L : TJSStatementList;
  V : TJSVarDeclaration;

begin
  I:=TJSIfStatement(CreateElement(TJSIfStatement,El));
  IO:=TJSRelationalExpressionInstanceOf(CreateElement(TJSRelationalExpressionInstanceOf,El));
  IO.A:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El,AContext);
  IO.B:=CreateIdentifierExpr(El.TypeName,El,AContext);
  I.Cond:=IO;
  L:=TJSStatementList(CreateElement(TJSStatementList,El.Body));
  I.BTrue:=L;
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
  L.A:=V;
  V.Name:=TransformVariableName(El.VariableName,AContext);
  V.Init:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El,AContext);
  L.B:=TJSStatementList(CreateElement(TJSStatementList,El.Body));
  L:=TJSStatementList(L.B);
  L.A:=ConvertElement(El.Body,AContext);
  Result:=I;
end;

function TPasToJSConverter.ConvertStatement(El: TPasImplStatement;
  AContext: TConvertContext): TJSElement;

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
    // ToDo: write source position
    DoError('Unknown statement Class: %s',[El.ClassName]);
{
  TPasImplCaseStatement = class(TPasImplStatement)
}
end;


function TPasToJSConverter.ConvertCommands(El: TPasImplCommands;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo: TPasImplCommands = class(TPasImplElement)
end;

function TPasToJSConverter.ConvertLabelMark(El: TPasImplLabelMark;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotYetImplemented(El,AContext);
  Result:=Nil;
  // ToDo:   TPasImplLabelMark = class(TPasImplLabelMark) then
end;

function TPasToJSConverter.ConvertElement(El: TPasElement;
  AContext: TConvertContext): TJSElement;
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
    Result:=ConvertLabelMark(TPasImplLabelMark(El),AContext)
  else
    Result:=nil;
end;

procedure TPasToJSConverter.DoError(const Msg: String);
begin
  Raise EPas2JS.Create(Msg);
end;

procedure TPasToJSConverter.DoError(const Msg: String;
  const Args: array of const);
begin
  Raise EPas2JS.CreateFmt(Msg,Args);
end;

procedure TPasToJSConverter.SetCurrentContext(AValue: TJSElement);
begin
  if FCurrentContext=AValue then Exit;
  FCurrentContext:=AValue;
end;

procedure TPasToJSConverter.RaiseNotYetImplemented(El: TPasElement;
  AContext: TConvertContext);
begin
  if AContext=nil then ;
  raise EPas2JS.Create('conversion not yet implemented for "'+El.ClassName+'"');
end;

function TPasToJSConverter.CreateJSContext(AContext: TConvertContext
  ): TJSElement;

begin
  if AContext=nil then ;
  Result:=TJSObjectLiteral.Create(0,0);
end;

function TPasToJSConverter.TransformVariableName(const AName: String;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  Result:=LowerCase(AName);
end;

function TPasToJSConverter.TransformVariableName(El: TPasElement;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  Result:=TransformVariableName(EL.Name,AContext);
  // Add to context.
end;

function TPasToJSConverter.TransformFunctionName(El: TPasElement;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  Result:=LowerCase(EL.Name);
end;

function TPasToJSConverter.ConvertElement(El: TPasElement): TJSElement;
begin
  //  CurrentContext:=CreateJSContext(Nil);
  Result:=ConvertElement(El,Nil);
end;

end.


