{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Pascal to Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************
}(*
 Abstract:
   Converts TPasElements into TJSElements.

 Works:
   - units, programs
   - uses list
   - interface vars
   - implementation vars
   - initialization section
   - procs, params, local vars
   - assign statements
   - function results
   - record types and vars
   - for loop
   - repeat..until
   - while..do
   - try..finally
   - asm..end

 ToDos:
   - unit interface function
   - optional: use $impl
   - append to for-loop: if($loopend>i)i--;
   - rename overloaded procs, append $0, $1, ...
   - rename js identifiers: apply, bind, call, prototyp, ...
   - bug: try adds empty line
   - bug: finally adds unnecessary {}
   - record const
   - copy record
   - asm..end as whole body
   - arrays
   - classes
   - passing by reference
   - procedure modifier external
   - Optional: put implementation into $impl
   - library
   - enums, sets. For small sets use an integer, for big sets use
       var s = {};
       s["red"] = true; s["green"] = true; s["red"] = true;
       Object.keys(s).length === 2;
       s["red"] === true;
       for (var key in s) // arbitrary order
         if (s.hasOwnProperty(key))
           console.log(s[key]);
   - Fix file names on converter errors (relative instead of full)
   - 'use strict' to allow javascript compilers optimize better
   - Avoid nameclashes with the following identifiers:
      implements, interface, let, package,
        private, protected, public, static, yield,
        class, enum, export, extends, import, super,
        __extends, _super
      array, Array, null, prototype, delete, for, break, if
        do, while, constructor, each, in, function, continue, default, arguments,
        switch, try, catch, throw, var, let, with, return, getPrototypeOf, new,
        instanceof, Math, Object, anonymous, true, false, null, NaN, undefined,
        String, Number, static, this, case, default
   - use UTF8 string literals
   - dotted unit names

 Debug flags: -d<x>
   VerbosePas2JS
*)
unit fppas2js;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, jsbase, jstree, pastree, PScanner, PasResolver;

// message numbers
const
  nPasElementNotSupported = 4001;
  nIdentifierNotFound = 4002;
  nUnaryOpcodeNotSupported = 4003;
  nBinaryOpcodeNotSupported = 4004;
  nInvalidNumber = 4005;
  nInitializedArraysNotSupported = 4006;
  nMemberExprMustBeIdentifier = 4007;
// resourcestring patterns of messages
resourcestring
  sPasElementNotSupported = 'Pascal element not supported: %s';
  sIdentifierNotFound = 'identifier not found "%s"';
  sUnaryOpcodeNotSupported = 'Unary OpCode not yet supported "%s"';
  sBinaryOpcodeNotSupported = 'Binary OpCode not yet supported "%s"';
  sInvalidNumber = 'invalid number "%s"';
  sInitializedArraysNotSupported = 'Initialized array variables not yet supported';
  sMemberExprMustBeIdentifier = 'Member expression must be an identifier';

const
  LoopEndVarName = '$loopend';

Type

  { EPas2JS }

  EPas2JS = Class(Exception)
  public
    PasElement: TPasElement;
    MsgNumber: integer;
    Args: TMessageArgs;
  end;

  { TConvertContext }

  TConvertContext = Class(TObject)
  public
    Element: TPasElement;
    Resolver: TPasResolver;
    Parent: TConvertContext;
    constructor Create(aParent: TConvertContext);
    function GetRootModule: TPasModule;
  end;

  { TRootContext }

  TRootContext = Class(TConvertContext)
  end;

  { TInitializationContext }

  TInitializationContext = Class(TConvertContext)
  end;

  { TDeclContext }

  TDeclContext = Class(TConvertContext)
  end;

  { TProcBodyContext }

  TProcBodyContext = Class(TConvertContext)
    // Element is TPasProcedure
  end;

  { TPasToJSConverter }

  TPasToJSConverter = Class(TObject)
  private
    FMainFunction: TJSString;
    FNameSpace: TJSString;
    FUseLowerCase: boolean;
    Procedure AddToSourceElements(Src: TJSSourceElements; El: TJSElement);
    Function CreateConstDecl(El: TPasConst; AContext: TConvertContext): TJSElement;
    Function CreateBuiltInIdentifierExpr(AName: string): TJSPrimaryExpressionIdent;
    Function CreateIdentifierExpr(AName: string; El: TPasElement): TJSPrimaryExpressionIdent;
    Function CreateTypeDecl(El: TPasType; AContext: TConvertContext): TJSElement;
    Function CreateVarDecl(El: TPasVariable; AContext: TConvertContext; TopLvl: boolean): TJSElement;
  protected
    // helper functions
    Procedure DoError(Const Msg : String);
    Procedure DoError(Const Msg : String; Const Args : Array of Const);
    Procedure DoError(MsgNumber: integer; const MsgPattern: string; Const Args : Array of Const; El: TPasElement);
    procedure RaiseNotSupported(El: TPasElement; AContext: TConvertContext; const Msg: string = '');
    procedure RaiseIdentifierNotFound(Identifier: string; El: TPasElement);
    procedure RaiseInconsistency;
    // Never create an element manually, always use the below function
    Function CreateElement(C: TJSElementClass; Src: TPasElement): TJSElement; virtual;
    Function GetExpressionValueType(El: TPasExpr; AContext: TConvertContext ): TJSType; virtual;
    Function GetPasIdentValueType(AName: String; AContext: TConvertContext): TJSType; virtual;
    Function TransFormIdent(El: TJSPrimaryExpressionIdent): TJSPrimaryExpressionIdent;virtual;
    Function TransformVariableName(Const AName:  String; AContext : TConvertContext): String; virtual;
    Function TransformVariableName(El: TPasElement; AContext : TConvertContext) : String; virtual;
    Function TransformFunctionName(El: TPasElement; AContext : TConvertContext) : String; virtual;
    Function TransformModuleName(El: TPasModule; AContext : TConvertContext) : String; virtual;
    Function GetExceptionObjectName(AContext: TConvertContext) : string;
    Function CreateCallStatement(const JSCallName: string; JSArgs: array of string): TJSCallExpression;
    Function CreateCallStatement(const FunNameEx: TJSElement; JSArgs: array of string): TJSCallExpression;
    Function CreateProcedureDeclaration(const El: TPasElement):TJSFunctionDeclarationStatement;
    Function CreateUnary(Members: array of string; E: TJSElement): TJSUnary;
    Function CreateMemberExpression(Members: array of string): TJSDotMemberExpression;
    Procedure AddProcedureToClass(sl: TJSStatementList; E: TJSElement;const P: TPasProcedure);
    Function GetFunctionDefinitionInUnary(const fd: TJSFunctionDeclarationStatement;const funname: TJSString; inunary: boolean): TJSFunctionDeclarationStatement;
    Function GetFunctionUnaryName(var je: TJSElement;out fundec: TJSFunctionDeclarationStatement): TJSString;
    Function CreateUsesList(UsesList: TFPList; AContext : TConvertContext): TJSArrayLiteral;
    Procedure AddToStatementList(var First, Last: TJSStatementList;
      Add: TJSElement; Src: TPasElement);
    Function CreateValInit(PasType: TPasType; Expr: TPasElement; El: TPasElement; AContext: TConvertContext): TJSElement;virtual;
    Function CreateVarInit(El: TPasVariable; AContext: TConvertContext): TJSElement;virtual;
    Function CreateRecordInit(aRecord: TPasRecordType; Expr: TPasElement; El: TPasElement; AContext: TConvertContext): TJSElement;virtual;
    Function CreateTypeRef(El: TPasType; AContext : TConvertContext): TJSElement;virtual;
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
    Function ConvertAsmStatement(El: TPasImplAsmStatement; AContext: TConvertContext): TJSElement;
    Procedure CreateInitSection(El: TPasModule; Src: TJSSourceElements; AContext: TConvertContext);
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
    Function ConvertIdentifierExpr(El: TPrimitiveExpr; AContext : TConvertContext): TJSElement;virtual;
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
    Function ConvertResString(El: TPasResString; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertResultElement(El: TPasResultElement; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertType(El: TPasElement; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertVariable(El: TPasVariable; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertElement(El : TPasElement; AContext : TConvertContext) : TJSElement; virtual;
    function ConvertRecordType(El: TPasRecordType; AContext: TConvertContext): TJSElement; virtual;
    function ConvertClassType(El: TPasClassType; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertClassMember(El: TPasElement; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertClassConstructor(El: TPasConstructor; AContext: TConvertContext): TJSElement; virtual;
  Public
    constructor Create;
    Function ConvertPasElement(El : TPasElement; Resolver: TPasResolver) : TJSElement;
    Property NameSpace : TJSString Read FNameSpace Write FNameSpace;
    Property MainFunction : TJSString Read FMainFunction Write FMainFunction;
    Property UseLowerCase: boolean read FUseLowerCase write FUseLowerCase;
  end;
  EPasToJS = Class(Exception);

var
  DefaultJSExceptionObject: string = 'exceptObject';


implementation

{ TConvertContext }

constructor TConvertContext.Create(aParent: TConvertContext);
begin
  Parent:=aParent;
  if Parent<>nil then
    Resolver:=Parent.Resolver;
end;

function TConvertContext.GetRootModule: TPasModule;
var
  aContext: TConvertContext;
begin
  aContext:=Self;
  while aContext.Parent<>nil do
    aContext:=aContext.Parent;
  if aContext.Element is TPasModule then
    Result:=TPasModule(aContext.Element)
  else
    Result:=nil;
end;

{ TPasToJSConverter }

procedure TPasToJSConverter.AddToSourceElements(Src: TJSSourceElements;
  El: TJSElement);

Var
  List : TJSStatementList;
  AddEl : TJSElement;

begin
  While El<>nil do
  begin
    if El is TJSStatementList then
      begin
      List:=El as TJSStatementList;
      // List.A is first statement, List.B is next in list, chained.
      // -> add A, continue with B and free List
      AddEl:=List.A;
      El:=List.B;
      List.A:=Nil;
      List.B:=Nil;
      FreeAndNil(List);
      end
    else
      begin
      AddEl:=El;
      El:=Nil;
      end;
    Src.Statements.AddNode.Node:=AddEl;
  end;
end;

function TPasToJSConverter.ConvertModule(El: TPasModule;
  AContext: TConvertContext): TJSElement;
(* Format:
   rtl.module('<unitname>',
      [<interface uses1>,<uses2>, ...],
      function(){
        <interface>
        <implementation>
        this.$init=function(){
          <initialization>
          };
      },
      [<implementation uses1>,<uses2>, ...]);
*)
Var
  OuterSrc , Src: TJSSourceElements;
  RegModuleCall: TJSCallExpression;
  ArgArray: TJSArguments;
  UsesList: TFPList;
  FunDef: TJSFuncDef;
  FunBody: TJSFunctionBody;
  FunDecl: TJSFunctionDeclarationStatement;
  ArgEx: TJSLiteral;
  UsesSection: TPasSection;
  ModuleName: String;
begin
  Result:=Nil;
  OuterSrc:=TJSSourceElements(CreateElement(TJSSourceElements, El));
  Result:=OuterSrc;

  // create 'rtl.module(...)'
  RegModuleCall:=TJSCallExpression(CreateElement(TJSCallExpression,El));
  AddToSourceElements(OuterSrc,RegModuleCall);
  RegModuleCall.Expr:=CreateMemberExpression(['rtl','module']);
  ArgArray := TJSArguments.Create(0, 0, '');
  RegModuleCall.Args:=ArgArray;

  // add parameter: unitname
  ArgEx := TJSLiteral.Create(0,0);
  ModuleName:=El.Name;
  if El is TPasProgram then
    ModuleName:='program';
  ArgEx.Value.AsString:=TJSString(TransformVariableName(ModuleName,AContext));
  ArgArray.Elements.AddElement.Expr:=ArgEx;

  // add parameter: [<interface uses1>,<uses2>, ...]
  UsesSection:=nil;
  if (El is TPasProgram) then
    UsesSection:=TPasProgram(El).ProgramSection
  else if (El is TPasLibrary) then
    UsesSection:=TPasLibrary(El).LibrarySection
  else
    UsesSection:=El.InterfaceSection;
  UsesList:=UsesSection.UsesList;
  ArgArray.Elements.AddElement.Expr:=CreateUsesList(UsesList,AContext);

  // add parameter: function(){}
  FunDecl:=TJSFunctionDeclarationStatement.Create(0,0);
  ArgArray.Elements.AddElement.Expr:=FunDecl;
  FunDef:=TJSFuncDef.Create;
  FunDecl.AFunction:=FunDef;
  FunDef.Name:='';
  FunBody:=TJSFunctionBody.Create(0,0);
  FunDef.Body:=FunBody;
  Src:=TJSSourceElements(CreateElement(TJSSourceElements, El));
  FunBody.A:=Src;

  if (El is TPasProgram) then
    begin // program
    if Assigned(TPasProgram(El).ProgramSection) then
      AddToSourceElements(Src,ConvertDeclarations(TPasProgram(El).ProgramSection,AContext));
    CreateInitSection(El,Src,AContext);
    end
  else if El is TPasLibrary then
    begin // library
    if Assigned(TPasLibrary(El).LibrarySection) then
      AddToSourceElements(Src,ConvertDeclarations(TPasLibrary(El).LibrarySection,AContext));
    CreateInitSection(El,Src,AContext);
    end
  else
    begin // unit
    // add interface section
    if Assigned(El.InterfaceSection) then
      AddToSourceElements(Src,ConvertDeclarations(El.InterfaceSection,AContext));
    // add implementation section
    if Assigned(El.ImplementationSection) then
      AddToSourceElements(Src,ConvertDeclarations(El.ImplementationSection,AContext));
    CreateInitSection(El,Src,AContext);

    // add optional implementation uses list: [<implementation uses1>,<uses2>, ...]
    if Assigned(El.ImplementationSection) then
      begin
      UsesList:=El.ImplementationSection.UsesList;
      if (UsesList<>nil) and (UsesList.Count>0) then
        ArgArray.Elements.AddElement.Expr:=CreateUsesList(UsesList,AContext);
      end;
    end;
end;

function TPasToJSConverter.CreateElement(C: TJSElementClass; Src: TPasElement
  ): TJSElement;

begin
  if Assigned(Src) then
    Result:=C.Create(Src.SourceLinenumber,1,Src.SourceFilename)
  else
    Result:=C.Create(0,0);
end;

function TPasToJSConverter.ConvertUnaryExpression(El: TUnaryExpr;
  AContext: TConvertContext): TJSElement;

Var
  U : TJSUnaryExpression;
  E : TJSElement;

begin
  if AContext=nil then ;
  Result:=Nil;
  E:=ConvertElement(El.Operand,AContext);
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
      DoError(nUnaryOpcodeNotSupported,sUnaryOpcodeNotSupported,
              [OpcodeStrings[El.OpCode]],El);
  end;
  Result:=U;
end;

function TPasToJSConverter.ConvertCallExpression(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
begin
  if AContext=nil then ;
  RaiseNotSupported(El,AContext,'ConvertCallExpression');
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

  C:=BinClasses[El.OpCode];
  A:=ConvertElement(El.left,AContext);
  ok:=false;
  try
    B:=ConvertElement(El.right,AContext);
    ok:=true;
  finally
    if not ok then
      FreeAndNil(A);
  end;
  if (C=Nil) then
    Case El.OpCode of
      eopAs :
        begin
        // ToDo: add check
        Result:=ConvertElement(El.left,AContext);
        end;
      eopAnd,
      eopOr,
      eopXor :
        begin
        if (GetExpressionValueType(El.left,AContext)=jstNumber)
            or (GetExpressionValueType(El.right,AContext)=jstNumber) then
          Case El.OpCode of
            eopAnd : C:=TJSBitwiseAndExpression;
            eopOr : C:=TJSBitwiseOrExpression;
            eopXor : C:=TJSBitwiseXOrExpression;
          end
        else
          Case El.OpCode of
            eopAnd : C:=TJSLogicalAndExpression;
            eopOr : C:=TJSLogicalOrExpression;
          else
            DoError(nBinaryOpcodeNotSupported,sBinaryOpcodeNotSupported,['logical XOR'],El);
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
        end
        else if (B is TJSCallExpression) then
        begin
          Result := B;
          funname := String(TJSPrimaryExpressionIdent(TJSCallExpression(B).Expr).Name);
          TJSCallExpression(B).Expr :=
            TJSDotMemberExpression(CreateElement(TJSDotMemberExpression, El));
          TJSDotMemberExpression(TJSCallExpression(B).Expr).MExpr := A;
          TJSDotMemberExpression(TJSCallExpression(B).Expr).Name := TJSString(funname);
        end
        else
          DoError(nMemberExprMustBeIdentifier,sMemberExprMustBeIdentifier,[],El);
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
              CreateMemberExpression(['_super', 'prototype', funname, 'call']);
          end;
        end
        else
          DoError(nBinaryOpcodeNotSupported,sBinaryOpcodeNotSupported,[OpcodeStrings[El.OpCode]],El);
    end;
  if (Result=Nil) and (C<>Nil) then
    begin
    R:=TJSBinary(CreateElement(C,El));
    R.A:=A;
    R.B:=B;
    Result:=R;
    end;
end;

function TPasToJSConverter.TransFormIdent(El: TJSPrimaryExpressionIdent
  ): TJSPrimaryExpressionIdent;

begin
  if UseLowerCase then
    El.Name:=TJSString(lowercase(El.Name));
  Result:=El;
end;

function TPasToJSConverter.CreateIdentifierExpr(AName: string; El: TPasElement
  ): TJSPrimaryExpressionIdent;

Var
  I : TJSPrimaryExpressionIdent;

begin
  I:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,El));
  if UseLowerCase then
    AName:=LowerCase(AName);
  I.Name:=TJSString(AName);
  Result:=TransFormIdent(I);
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
        DoError(nInvalidNumber,sInvalidNumber,[El.Value],El);
      L.Value.AsNumber:=Number;
      Result:=L;
      end;
    pekIdent:
      begin
      Result:=ConvertIdentifierExpr(El,AContext);
      end;
    else
      ;
  end;
end;

function TPasToJSConverter.ConvertIdentifierExpr(El: TPrimitiveExpr;
  AContext: TConvertContext): TJSElement;
var
  Decl: TPasElement;
  Name: String;
  FoundModule: TPasModule;
begin
  if AContext=nil then ;
  if El.Kind<>pekIdent then
    RaiseInconsistency;
  if El.CustomData is TResolvedReference then
    begin
    Decl:=TResolvedReference(El.CustomData).Declaration;
    {$IFDEF VerbosePas2JS}
    writeln('TPasToJSConverter.ConvertIdentifierExpr ',GetObjName(El),' Decl=',GetObjName(Decl));
    {$ENDIF}
    if Decl is TPasModule then
      Name:='pas.'+TransformModuleName(TPasModule(Decl),AContext)
    else if (Decl is TPasFunctionType) and (CompareText(ResolverResultVar,El.Value)=0) then
      Name:=ResolverResultVar
    else
      begin
      Name:=TransformVariableName(Decl,AContext);
      {$IFDEF VerbosePas2JS}
      writeln('TPasToJSConverter.ConvertIdentifierExpr Decl.Parent=',GetObjName(Decl.Parent));
      {$ENDIF}
      if Decl.Parent is TPasSection then
        begin
        FoundModule:=Decl.GetModule;
        if FoundModule=nil then
          RaiseInconsistency;
        if AContext.GetRootModule=FoundModule then
          Name:='this.'+Name
        else
          Name:='pas.'+TransformModuleName(FoundModule,AContext)+'.'+Name;
        end;
      end;
    // ToDo: use TJSDotMemberExpression for dots
    Result:=CreateIdentifierExpr(Name,El);
    end
  else if AContext.Resolver<>nil then
    RaiseIdentifierNotFound(El.Value,El)
  else
    // simple mode
    Result:=CreateIdentifierExpr(El.Value,El);
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
  je := CreateIdentifierExpr('_super',El);
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
  Case El.Kind of
  pekFuncParams :
    begin
    C:=TJSCallExpression(CreateElement(TJSCallExpression,El));
    try
      C.Expr:=ConvertElement(El.Value,AContext);
      if (Length(El.Params)>0) then
        begin
        C.Args:=TJSArguments(CreateElement(TJSArguments,El));
        For I:=0 to Length(El.Params)-1 do
          begin
          E:=ConvertElement(El.Params[i],AContext);
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
    if Length(El.Params)<>1 then
      Raise EPasToJS.Create('Only 1-dimensional expressions allowed at this point');
    B:=TJSBracketMemberExpression(CreateElement(TJSBracketMemberExpression,El));
    B.MExpr:=ConvertElement(El.Value,AContext);
    Result:=B;
    B.Name:=ConvertElement(El.Params[0],AContext);
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
    RaiseNotSupported(El,AContext,'ConvertExpression');
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
  Ident:=TJSPrimaryExpressionIdent.Create(0,0);
  if UseLowerCase then
    AName:=LowerCase(AName);
  Ident.Name:=TJSString(AName);
  Result:=Ident;
end;

function TPasToJSConverter.CreateTypeDecl(El: TPasType;
  AContext: TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (El is TPasClassType) then
    Result := ConvertClassType(TPasClassType(El), AContext)
  else if El is TPasRecordType then
    Result := ConvertRecordType(TPasRecordType(El), AContext);
  // other types don't need a constructor function
end;

function TPasToJSConverter.CreateVarDecl(El: TPasVariable;
  AContext: TConvertContext; TopLvl: boolean): TJSElement;

Var
  C : TJSElement;
  V : TJSVariableStatement;
  AssignSt: TJSSimpleAssignStatement;
  VarName: String;

begin
  if TopLvl then
    begin
    // create 'this.A=initvalue'
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    Result:=AssignSt;
    VarName:=TransformVariableName(El.Name,AContext);
    AssignSt.LHS:=CreateMemberExpression(['this',VarName]);
    AssignSt.Expr:=CreateVarInit(El,AContext);
    end
  else
    begin
    // create 'var A=initvalue'
    C:=ConvertElement(El,AContext);
    V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    V.A:=C;
    Result:=V;
    end;
end;

function TPasToJSConverter.ConvertDeclarations(El: TPasDeclarations;
  AContext: TConvertContext): TJSElement;

Var
  E : TJSElement;
  SLFirst, SLLast: TJSStatementList;
  P: TPasElement;
  IsTopLvl, IsProcBody, IsFunction: boolean;
  I : Integer;
  SubContext: TConvertContext;

  Procedure AddFunctionResultInit;
  var
    VarSt: TJSVariableStatement;
    AssignSt: TJSSimpleAssignStatement;
    PasFun: TPasFunction;
    FunType: TPasFunctionType;
    ResultEl: TPasResultElement;
  begin
    PasFun:=El.Parent as TPasFunction;
    FunType:=PasFun.FuncType;
    ResultEl:=FunType.ResultEl;

    // add 'var result=initvalue'
    VarSt:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    AddToStatementList(SLFirst,SLLast,VarSt,El);
    Result:=SLFirst;
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    VarSt.A:=AssignSt;
    AssignSt.LHS:=CreateBuiltInIdentifierExpr(ResolverResultVar);
    AssignSt.Expr:=CreateValInit(ResultEl.ResultType,nil,El,SubContext);
  end;

  Procedure AddFunctionResultReturn;
  var
    RetSt: TJSReturnStatement;
  begin
    RetSt:=TJSReturnStatement(CreateElement(TJSReturnStatement,El));
    RetSt.Expr:=CreateBuiltInIdentifierExpr(ResolverResultVar);
    AddToStatementList(SLFirst,SLLast,RetSt,El);
  end;

begin
  Result:=nil;

  SLFirst:=nil;
  SLLast:=nil;
  IsTopLvl:=El.Parent is TPasModule;
  IsProcBody:=(El is TProcedureBody) and (TProcedureBody(El).Body<>nil);
  IsFunction:=IsProcBody and (El.Parent is TPasFunction);

  SubContext:=TDeclContext.Create(aContext);
  try
    SubContext.Element:=El;

    if IsProcBody and IsFunction then
      AddFunctionResultInit;

    For I:=0 to El.Declarations.Count-1 do
      begin
      E:=Nil;
      P:=TPasElement(El.Declarations[i]);
      if P is TPasConst then
        E:=CreateConstDecl(TPasConst(P),SubContext)
      else if P is TPasVariable then
        E:=CreateVarDecl(TPasVariable(P),SubContext,IsTopLvl)
      else if P is TPasType then
        E:=CreateTypeDecl(TPasType(P),SubContext)
      else if P is TPasProcedure then
        E:=ConvertProcedure(TPasProcedure(P),SubContext)
      else
        RaiseNotSupported(P as TPasElement,AContext,'ConvertDeclarations');
      if (Pos('.', P.Name) > 0) then
        AddProcedureToClass(TJSStatementList(Result), E, P as TPasProcedure)
      else
      AddToStatementList(SLFirst,SLLast,E,El);
      Result:=SLFirst;
      end;

    if IsProcBody then
      begin
      E:=ConvertElement(TProcedureBody(El).Body,SubContext);
      AddToStatementList(SLFirst,SLLast,E,El);
      Result:=SLFirst;
      end;

    if IsProcBody and IsFunction then
      AddFunctionResultReturn;

  {
    TPasDeclarations = class(TPasElement)
    TPasSection = class(TPasDeclarations)
    TInterfaceSection = class(TPasSection)
    TImplementationSection = class(TPasSection)
    TProgramSection = class(TImplementationSection)
    TLibrarySection = class(TImplementationSection)
    TProcedureBody = class(TPasDeclarations)
  }
  finally
    SubContext.Free;
  end;
end;

function TPasToJSConverter.ConvertType(El: TPasElement;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertType');
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

function TPasToJSConverter.ConvertClassType(El: TPasClassType;
  AContext: TConvertContext): TJSElement;
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
  asi.LHS := CreateIdentifierExpr(El.Name,El);
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
    call.Args.Elements.AddElement.Expr := CreateIdentifierExpr(El.AncestorType.Name,El);
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
  ret.Expr := CreateIdentifierExpr(El.Name,El);
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
    Result := CreateUnary([El.Parent.FullName, 'prototype', TPasProcedure(El).Name], FS);
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
  nmem.MExpr := CreateIdentifierExpr(El.Parent.FullName,El.Parent);
  for n := 0 to El.ProcType.Args.Count - 1 do
  begin
    if n = 0 then
      nmem.Args := TJSArguments.Create(0, 0, '');
    fs.AFunction.Params.Add(TPasArgument(El.ProcType.Args[n]).Name);
    Arg := TPasArgument(El.ProcType.Args[n]);
    nmem.Args.Elements.AddElement.Expr := CreateIdentifierExpr(Arg.Name,Arg);
  end;
  Result := CreateUnary([El.Parent.FullName, TPasProcedure(El).Name], FS);
end;

constructor TPasToJSConverter.Create;
begin
  FUseLowerCase:=true;
end;

function TPasToJSConverter.ConvertProcedure(El: TPasProcedure;
  AContext: TConvertContext): TJSElement;

Var
  FS : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;
  n:Integer;
  IsTopLvl: Boolean;
  FunName: String;
  AssignSt: TJSSimpleAssignStatement;
  SubContext: TConvertContext;

begin
  Result:=nil;
  IsTopLvl:=El.Parent is TPasSection;

  FunName:=TransformFunctionName(El,AContext);

  AssignSt:=nil;
  if IsTopLvl then
    begin
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    Result:=AssignSt;
    AssignSt.LHS:=CreateMemberExpression(['this',FunName]);
    end;

  FS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,El));
  if AssignSt<>nil then
    AssignSt.Expr:=FS
  else
    Result:=FS;
  FD:=TJSFuncDef.Create;
  if AssignSt=nil then
    FD.Name:=TJSString(FunName);
  FS.AFunction:=FD;
  for n := 0 to El.ProcType.Args.Count - 1 do
    FD.Params.Add(TransformVariableName(TPasArgument(El.ProcType.Args[n]).Name,AContext));
  FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,El.Body));

  SubContext:=TProcBodyContext.Create(AContext);
  try
    SubContext.Element:=El;
    FD.Body.A:=ConvertElement(El.Body,SubContext);
  finally
    SubContext.Free;
  end;
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

function TPasToJSConverter.ConvertBeginEndStatement(El: TPasImplBeginBlock;
  AContext: TConvertContext): TJSElement;

begin
  Result:=ConvertImplBlockElements(El,AContext);
end;

function TPasToJSConverter.ConvertImplBlockElements(El: TPasImplBlock;
  AContext: TConvertContext): TJSElement;

var
  First, Last: TJSStatementList;
  I : Integer;
  PasImpl: TPasImplElement;
  JSImpl : TJSElement;

begin
  if Not (Assigned(El.Elements) and (El.Elements.Count>0)) then
    Result:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El))
  else
    begin
    First:=nil;
    Result:=First;
    Last:=First;
    //writeln('TPasToJSConverter.ConvertImplBlockElements START El.Elements.Count=',El.Elements.Count);
    For I:=0 to El.Elements.Count-1 do
      begin
      PasImpl:=TPasImplElement(El.Elements[i]);
      JSImpl:=ConvertElement(PasImpl,AContext);
      //writeln('TPasToJSConverter.ConvertImplBlockElements ',i,' ',JSImpl.ClassName);
      AddToStatementList(First,Last,JSImpl,PasImpl);
      Result:=First;
      end;
    end;
end;

function TPasToJSConverter.ConvertInitializationSection(
  El: TInitializationSection; AContext: TConvertContext): TJSElement;
var
  FDS: TJSFunctionDeclarationStatement;
  FD: TJSFuncDef;
  FunName: String;
  IsMain, ok: Boolean;
  AssignSt: TJSSimpleAssignStatement;
begin
  // create: 'this.$init=function(){}'

  IsMain:=(El.Parent<>nil) and (El.Parent is TPasProgram);
  if IsMain then
    FunName:='$main'
  else
    FunName:='$init';

  AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
  Result:=AssignSt;
  ok:=false;
  try
    AssignSt.LHS:=CreateMemberExpression(['this',FunName]);
    FDS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,El));
    AssignSt.Expr:=FDS;
    FD:=TJSFuncDef.Create;
    FDS.AFunction:=FD;
    if El.Elements.Count>0 then
      begin
      FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,El));
      FD.Body.A:=ConvertImplBlockElements(El,AContext);
      end;
    ok:=true;
  finally
    if not ok then FreeAndNil(Result);
  end;
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
  IsFin , ok: Boolean;

begin
  F:=Nil;
  B:=ConvertImplBlockElements(El,AContext);
  ok:=false;
  try
    F:=ConvertElement(El.FinallyExcept,AContext);
    IsFin:=El.FinallyExcept is TPasImplTryFinally;
    if IsFin then
      T:=TJSTryFinallyStatement(CreateElement(TJSTryFinallyStatement,El))
    else
      begin
      T:=TJSTryCatchStatement(CreateElement(TJSTryCatchStatement,El));
      T.Ident:=TJSString(GetExceptionObjectName(AContext));
      end;
    ok:=true;
  finally
    if not ok then
      begin
      B.Free;
      F.Free;
      end;
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

function TPasToJSConverter.ConvertAsmStatement(El: TPasImplAsmStatement;
  AContext: TConvertContext): TJSElement;
var
  pex: TJSPrimaryExpressionIdent;
  s: String;
begin
  if AContext=nil then ;
  s:=El.Tokens.Text;
  if s='' then
    Result:=TJSEmptyStatement(CreateElement(TJSEmptyStatement,El))
  else begin
    pex:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,El));
    pex.Name := TJSString(s);
    Result:=pex;
  end;
end;

procedure TPasToJSConverter.CreateInitSection(El: TPasModule;
  Src: TJSSourceElements; AContext: TConvertContext);
var
  SubContext: TInitializationContext;
begin
  SubContext:=TInitializationContext.Create(AContext);
  try
    SubContext.Element:=El;
    // add initialization section
    if Assigned(El.InitializationSection) then
      AddToSourceElements(Src,ConvertImplBlock(El.InitializationSection,SubContext));
    // finalization: not supported
    if Assigned(El.FinalizationSection) then
      raise Exception.Create('TPasToJSConverter.ConvertInitializationSection: finalization section is not supported');
  finally
    SubContext.Free;
  end;
end;

function TPasToJSConverter.ConvertImplBlock(El: TPasImplBlock;
  AContext: TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (El is TPasImplStatement) then
    Result:=ConvertStatement(TPasImplStatement(El),AContext)
  else if (El is TPasImplIfElse) then
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
  RaiseNotSupported(El,AContext,'ConvertPackage');
  Result:=Nil;
  // ToDo TPasPackage = class(TPasElement)
end;

function TPasToJSConverter.ConvertResString(El: TPasResString;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertResString');
  Result:=Nil;
  // ToDo: TPasResString
end;

function TPasToJSConverter.ConvertArgument(El: TPasArgument;
  AContext: TConvertContext): TJSElement;

begin
  // is this still needed?
  RaiseNotSupported(El,AContext,'ConvertArgument');
  Result:=Nil;
  // ToDo: TPasArgument
end;

function TPasToJSConverter.ConvertResultElement(El: TPasResultElement;
  AContext: TConvertContext): TJSElement;

begin
  // is this still needed?
  RaiseNotSupported(El,AContext);
  Result:=Nil;
  // TPasResultElement
end;

function TPasToJSConverter.ConvertVariable(El: TPasVariable;
  AContext: TConvertContext): TJSElement;

Var
  V : TJSVarDeclaration;
begin
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
  V.Name:=TransformVariableName(El,AContext);
  V.Init:=CreateVarInit(El,AContext);
  Result:=V;
end;

function TPasToJSConverter.ConvertConst(El: TPasConst; AContext: TConvertContext
  ): TJSElement;

begin
  // is this still needed?
  RaiseNotSupported(El,AContext,'ConvertConst');
  Result:=Nil;
  // ToDo: TPasConst
end;

function TPasToJSConverter.ConvertProperty(El: TPasProperty;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertProperty');
  Result:=Nil;
  // ToDo: TPasProperty = class(TPasVariable)
end;

function TPasToJSConverter.ConvertExportSymbol(El: TPasExportSymbol;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertExportSymbol');
  Result:=Nil;
  // ToDo: TPasExportSymbol
end;

function TPasToJSConverter.ConvertLabels(El: TPasLabels;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertLabels');
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
    E:=ConvertElement(El.ExceptObject,AContext)
  else
    E:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El);
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
  LHS:=ConvertElement(El.left,AContext);
  ok:=false;
  try
    RHS:=ConvertElement(El.right,AContext);
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
  RaiseNotSupported(El,AContext,'ConvertCommand');
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
    C:=ConvertElement(El.ConditionExpr,AContext);
    if Assigned(El.IfBranch) then
      BThen:=ConvertElement(El.IfBranch,AContext)
    else
      BThen:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El));
    if Assigned(El.ElseBranch) then
      BElse:=ConvertElement(El.ElseBranch,AContext);
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
// Creates the following code:
//   LoopVar=<StartExpr>;
//   for(var $loopend=<EndExpr>; LoopVar<=$loopend; LoopVar++){}
//
// The StartExpr must be executed exactly once at beginning.
// The EndExpr must be executed exactly once at beginning.
// The $loopend variable is local to the FOR block. It's only used within
// the for header, so the name can be the same in other for loops.
// LoopVar can be a varname or programname.varname

Var
  ForSt : TJSForStatement;
  List : TJSStatementList;
  SimpleAss : TJSSimpleAssignStatement;
  VarDecl : TJSVarDeclaration;
  Incr : TJSUNaryExpression;
  BinExp : TJSBinaryExpression;
  ok: Boolean;
  VarStat: TJSVariableStatement;

begin
  Result:=Nil;
  BinExp:=Nil;
  // loopvar:=
  // for (statementlist...
  List:=TJSStatementList(CreateElement(TJSStatementList,El));
  Result:=List;
  ok:=false;
  try
    // add "LoopVar:=<StartExpr>;"
    SimpleAss:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El.StartExpr));
    SimpleAss.LHS:=ConvertElement(El.VariableName,AContext);
    SimpleAss.Expr:=ConvertElement(El.StartExpr,AContext);
    List.A:=SimpleAss;
    // add "for()"
    ForSt:=TJSForStatement(CreateElement(TJSForStatement,El));
    List.B:=ForSt;
    // add "var $loopend=<EndExpr>"
    VarStat:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    VarDecl:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
    VarStat.A:=VarDecl;
    VarDecl.Name:=LoopEndVarName;
    VarDecl.Init:=ConvertElement(El.EndExpr,AContext);
    ForSt.Init:=VarStat;
    // add "LoopVar<=$loopend"
    If El.Down then
      BinExp:=TJSRelationalExpressionGE(CreateElement(TJSRelationalExpressionGE,El.EndExpr))
    else
      BinExp:=TJSRelationalExpressionLE(CreateElement(TJSRelationalExpressionLE,El.EndExpr));
    BinExp.A:=ConvertElement(El.VariableName,AContext);
    BinExp.B:=CreateIdentifierExpr(LoopEndVarName,El.EndExpr);
    ForSt.Cond:=BinExp;
    // add "LoopVar++"
    If El.Down then
      Incr:=TJSUnaryPostMinusMinusExpression(CreateElement(TJSUnaryPostMinusMinusExpression,El))
    else
      Incr:=TJSUnaryPostPlusPlusExpression(CreateElement(TJSUnaryPostPlusPlusExpression,El));
    Incr.A:=ConvertElement(El.VariableName,AContext);
    ForSt.Incr:=Incr;
    // add body
    ForSt.Body:=ConvertElement(El.Body,AContext);
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
  Result:=DefaultJSExceptionObject; // use the same as the FPC RTL
  if UseLowerCase then
    Result:=lowercase(Result);
end;

procedure TPasToJSConverter.RaiseInconsistency;
begin
  raise Exception.Create('TPasToJSConverter.RaiseInconsistency: you found a bug');
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

function TPasToJSConverter.CreateUnary(Members: array of string; E: TJSElement): TJSUnary;
var
  unary: TJSUnary;
  asi: TJSSimpleAssignStatement;
begin
  unary := TJSUnary.Create(0, 0, '');
  asi := TJSSimpleAssignStatement.Create(0, 0, '');
  unary.A := asi;
  asi.Expr := E;
  asi.LHS := CreateMemberExpression(Members);
  Result := unary;
end;

function TPasToJSConverter.CreateMemberExpression(Members: array of string): TJSDotMemberExpression;
var
  pex: TJSPrimaryExpressionIdent;
  MExpr: TJSDotMemberExpression;
  LastMExpr: TJSDotMemberExpression;
  k: integer;
begin
  if Length(Members) < 2 then
    DoError('internal error: member expression with less than two members');
  LastMExpr := nil;
  for k:=High(Members) downto Low(Members)+1 do
  begin
    MExpr := TJSDotMemberExpression.Create(0, 0, '');
    MExpr.Name := TJSString(Members[k]);
    if LastMExpr=nil then
      Result := MExpr
    else
      LastMExpr.MExpr := MExpr;
    LastMExpr := MExpr;
  end;
  pex := TJSPrimaryExpressionIdent.Create(0, 0, '');
  pex.Name := TJSString(Members[Low(Members)]);
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
  ArgArray:=TJSArrayLiteral.Create(0,0);
  if UsesList<>nil then
    for k:=0 to UsesList.Count-1 do
      begin
      El:=TPasElement(UsesList[k]);
      if not (El is TPasModule) then continue;
      anUnitName := TransformVariableName(TPasModule(El).Name,AContext);
      ArgEx := TJSLiteral.Create(0,0);
      ArgEx.Value.AsString:=TJSString(anUnitName);
      ArgArray.Elements.AddElement.Expr := ArgEx;
      end;
  Result:=ArgArray;
end;

procedure TPasToJSConverter.AddToStatementList(var First,
  Last: TJSStatementList; Add: TJSElement; Src: TPasElement);
var
  SL2: TJSStatementList;
begin
  if not Assigned(Add) then exit;
  if Add is TJSStatementList then
    begin
    // add list
    if TJSStatementList(Add).A=nil then
      begin
      // empty list -> skip
      if TJSStatementList(Add).B<>nil then
        raise Exception.Create('internal error: AddToStatementList add list A=nil, B<>nil');
      FreeAndNil(Add);
      end
    else if Last=nil then
      begin
      // our list is not yet started -> simply take the extra list
      Last:=TJSStatementList(Add);
      First:=Last;
      end
    else
      begin
      // merge lists (append)
      if Last.B<>nil then
        begin
        // add a nil to the end of chain
        SL2:=TJSStatementList(CreateElement(TJSStatementList,Src));
        SL2.A:=Last.B;
        Last.B:=SL2;
        Last:=SL2;
        // Last.B is now nil
        end;
      Last.B:=Add;
      while Last.B is TJSStatementList do
        Last:=TJSStatementList(Last.B);
      end;
    end
  else
    begin
    if Last=nil then
      begin
      // start list
      Last:=TJSStatementList(CreateElement(TJSStatementList,Src));
      First:=Last;
      Last.A:=Add;
      end
    else if Last.B=nil then
      // second element
      Last.B:=Add
    else
      begin
      // add to chain
      SL2:=TJSStatementList(CreateElement(TJSStatementList,Src));
      SL2.A:=Last.B;
      Last.B:=SL2;
      Last:=SL2;
      Last.B:=Add;
      end;
    end;
end;

function TPasToJSConverter.CreateValInit(PasType: TPasType; Expr: TPasElement;
  El: TPasElement; AContext: TConvertContext): TJSElement;
var
  T: TPasType;
  Lit: TJSLiteral;
begin
  T:=PasType;
  if (T is TPasArrayType) then
    begin
    Result:=TJSArrayLiteral(CreateElement(TJSArrayLiteral,PasType));
    If Assigned(Expr) then
      DoError(nInitializedArraysNotSupported,sInitializedArraysNotSupported,[],PasType);
    end
  else if T is TPasRecordType then
    Result:=CreateRecordInit(TPasRecordType(T),Expr,El,AContext)
  else if Assigned(Expr) then
    Result:=ConvertElement(Expr,AContext)
  else
    begin
    // always init with a default value to create a typed variable (faster and more readable)
    Lit:=TJSLiteral(CreateElement(TJSLiteral,El));
    Result:=Lit;
    if T is TPasAliasType then
      T:=TPasAliasType(T).DestType;

    if T=nil then
      Lit.Value.IsUndefined:=true
    else if T.ClassType=TPasPointerType then
      Lit.Value.IsNull:=true
    else if T.ClassType=TPasStringType then
      Lit.Value.AsString:=''
    else if T.ClassType=TPasUnresolvedSymbolRef then
      begin
      if (CompareText(T.Name,'longint')=0)
      or (CompareText(T.Name,'int64')=0)
      or (CompareText(T.Name,'real')=0)
      or (CompareText(T.Name,'double')=0)
      then
        Lit.Value.AsNumber:=0.0
      else if (CompareText(T.Name,'boolean')=0)
      then
        Lit.Value.AsBoolean:=false
      else if (CompareText(T.Name,'string')=0)
           or (CompareText(T.Name,'char')=0)
      then
        Lit.Value.AsString:=''
      else
        begin
        Lit.Value.IsUndefined:=true;
        //writeln('TPasToJSConverter.CreateVarInit unknown PasType class=',T.ClassName,' name=',T.Name);
        end;
      end
    else
      begin
      Lit.Value.IsUndefined:=true;
      //writeln('TPasToJSConverter.CreateVarInit unknown PasType class=',T.ClassName,' name=',T.Name);
      end;
    end;
end;

function TPasToJSConverter.CreateVarInit(El: TPasVariable;
  AContext: TConvertContext): TJSElement;
begin
  Result:=CreateValInit(El.VarType,El.Expr,El,AContext);
end;

function TPasToJSConverter.CreateRecordInit(aRecord: TPasRecordType;
  Expr: TPasElement; El: TPasElement; AContext: TConvertContext): TJSElement;
var
  NewMemE: TJSNewMemberExpression;
begin
  if Expr<>nil then
    RaiseNotSupported(Expr,AContext,'CreateRecordInit Expr<>nil');
  NewMemE:=TJSNewMemberExpression(CreateElement(TJSNewMemberExpression,El));
  Result:=NewMemE;
  NewMemE.MExpr:=CreateTypeRef(aRecord,AContext);
end;

function TPasToJSConverter.CreateTypeRef(El: TPasType; AContext: TConvertContext
  ): TJSElement;
var
  FoundModule: TPasModule;
  Name: String;
begin
  Name:=TransformVariableName(El.Name,AContext);
  { $IFDEF VerbosePas2JS}
  writeln('TPasToJSConverter.CreateTypeRef El="',GetObjName(El),'" El.Parent=',GetObjName(El.Parent));
  { $ENDIF}
  if El.Parent is TPasSection then
    begin
    FoundModule:=El.GetModule;
    if FoundModule=nil then
      RaiseInconsistency;
    if AContext.GetRootModule=FoundModule then
      Name:='this.'+Name
    else
      Name:='pas.'+TransformModuleName(FoundModule,AContext)+'.'+Name;
    end;
  // ToDo: use TJSDotMemberExpression for dots
  Result:=CreateIdentifierExpr(Name,El);
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
  IO.A:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El);
  IO.B:=CreateIdentifierExpr(El.TypeName,El);
  I.Cond:=IO;
  L:=TJSStatementList(CreateElement(TJSStatementList,El.Body));
  I.BTrue:=L;
  V:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
  L.A:=V;
  V.Name:=TransformVariableName(El.VariableName,AContext);
  V.Init:=CreateIdentifierExpr(GetExceptionObjectName(AContext),El);
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
  else if (El is TPasImplAsmStatement) then
    Result:=ConvertAsmStatement(TPasImplAsmStatement(El),AContext)
  else
    RaiseNotSupported(El,AContext,'ConvertStatement');
{
  TPasImplCaseStatement = class(TPasImplStatement)
}
end;


function TPasToJSConverter.ConvertCommands(El: TPasImplCommands;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertCommands');
  Result:=Nil;
  // ToDo: TPasImplCommands = class(TPasImplElement)
end;

function TPasToJSConverter.ConvertLabelMark(El: TPasImplLabelMark;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,'ConvertLabelMark');
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
  else if (El is TPasExpr) then
    Result:=ConvertExpression(TPasExpr(El),AContext)
  else if (El is TPasDeclarations) then
    Result:=ConvertDeclarations(TPasDeclarations(El),AContext)
  else if (El is TPasType) then
    Result:=ConvertType(TPasType(El),AContext)
  else if (El is TPasProcedure) then
    Result:=ConvertProcedure(TPasProcedure(El),AContext)
  else if (El is TPasImplBlock) then
    Result:=ConvertImplBlock(TPasImplBlock(El),AContext)
  else if (El is TPasResString) then
    Result:=ConvertResString(TPasResString(El),AContext)
  else if (El is TPasArgument) then
    Result:=ConvertArgument(TPasArgument(El),AContext)
  else if (El is TPasResultElement) then
    Result:=ConvertResultElement(TPasResultElement(El),AContext)
  else if (El is TPasConst) then
    Result:=ConvertConst(TPasConst(El),AContext)
  else if (El is TPasProperty) then
    Result:=ConvertProperty(TPasProperty(El),AContext)
  else if (El is TPasVariable) then
    Result:=ConvertVariable(TPasVariable(El),AContext)
  else if (El is TPasExportSymbol) then
    Result:=ConvertExportSymbol(TPasExportSymbol(El),AContext)
  else if (El is TPasLabels) then
    Result:=ConvertLabels(TPasLabels(El),AContext)
  else if (El is TPasImplCommand) then
    Result:=ConvertCommand(TPasImplCommand(El),AContext)
  else if (El is TPasImplCommands) then
    Result:=ConvertCommands(TPasImplCommands(El),AContext)
  else if (El is TPasImplLabelMark) then
    Result:=ConvertLabelMark(TPasImplLabelMark(El),AContext)
  else
    Result:=nil;
end;

function TPasToJSConverter.ConvertRecordType(El: TPasRecordType;
  AContext: TConvertContext): TJSElement;
(*
  type
    TMyRecord = record
      i: longint;
      s: string;
      d: double;
    end;

    this.TMyRecord=function() {
                 i=0;
                 s="";
                 d=0.0;
                };
*)
var
  AssignSt: TJSSimpleAssignStatement;
  ok: Boolean;
  i: Integer;
  PasVar: TPasVariable;
  FDS: TJSFunctionDeclarationStatement;
  FD: TJSFuncDef;
  JSVar: TJSElement;
  First, Last: TJSStatementList;
begin
  AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
  Result:=AssignSt;
  ok:=false;
  try
    AssignSt.LHS:=CreateMemberExpression(['this',TransformVariableName(El.Name,AContext)]);
    FDS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,El));
    AssignSt.Expr:=FDS;
    FD:=TJSFuncDef.Create;
    FDS.AFunction:=FD;
    FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,El));
    First:=nil;
    Last:=nil;
    for i:=0 to El.Members.Count-1 do
      begin
      PasVar:=TPasVariable(El.Members[i]);
      JSVar:=ConvertVariable(PasVar,AContext);
      AddToStatementList(First,Last,JSVar,PasVar);
      FD.Body.A:=First;
      end;
    ok:=true;
  finally
    if not ok then FreeAndNil(Result);
  end;
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

procedure TPasToJSConverter.DoError(MsgNumber: integer;
  const MsgPattern: string; const Args: array of const; El: TPasElement);
var
  E: EPas2JS;
begin
  E:=EPas2JS.CreateFmt(MsgPattern,[El.ClassName]);
  E.PasElement:=El;
  E.MsgNumber:=MsgNumber;
  CreateMsgArgs(E.Args,Args);
  raise E;
end;

procedure TPasToJSConverter.RaiseNotSupported(El: TPasElement;
  AContext: TConvertContext; const Msg: string);
var
  E: EPas2JS;
begin
  if AContext=nil then ;
  E:=EPas2JS.CreateFmt(sPasElementNotSupported,[GetObjName(El)]);
  if Msg<>'' then
    E.Message:=E.Message+': '+Msg;
  E.PasElement:=El;
  E.MsgNumber:=nPasElementNotSupported;
  SetLength(E.Args,1);
  E.Args[0]:=El.ClassName;
  raise E;
end;

procedure TPasToJSConverter.RaiseIdentifierNotFound(Identifier: string;
  El: TPasElement);
var
  E: EPas2JS;
begin
  E:=EPas2JS.CreateFmt(sIdentifierNotFound,[Identifier]);
  E.PasElement:=El;
  E.MsgNumber:=nIdentifierNotFound;
  SetLength(E.Args,1);
  E.Args[0]:=Identifier;
  raise E;
end;

function TPasToJSConverter.TransformVariableName(const AName: String;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  if UseLowerCase then
    Result:=lowercase(AName)
  else
    Result:=AName;
end;

function TPasToJSConverter.TransformVariableName(El: TPasElement;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  Result:=El.Name;
  if UseLowerCase then
    Result:=lowercase(Result);
end;

function TPasToJSConverter.TransformFunctionName(El: TPasElement;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  Result:=El.Name;
  if UseLowerCase then
    Result:=lowercase(Result);
end;

function TPasToJSConverter.TransformModuleName(El: TPasModule;
  AContext: TConvertContext): String;
begin
  if AContext=nil then ;
  Result:=El.Name;
  if UseLowerCase then
    Result:=lowercase(Result);
end;

function TPasToJSConverter.ConvertPasElement(El: TPasElement;
  Resolver: TPasResolver): TJSElement;
var
  aContext: TRootContext;
begin
  aContext:=TRootContext.Create(nil);
  try
    aContext.Element:=El;
    aContext.Resolver:=Resolver;
    Result:=ConvertElement(El,aContext);
  finally
    FreeAndNil(aContext);
  end;
end;

end.


