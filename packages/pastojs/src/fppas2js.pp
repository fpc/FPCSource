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
   - unit interface function
   - uses list
   - interface vars
   - implementation vars
   - initialization section
   - procs, params, local vars
   - proc default values
   - assign statements
   - function results
   - record types and vars
   - for loop
     - if loopvar is used afterwards append  if($loopend>i)i--;
   - repeat..until
   - while..do
   - try..finally
   - try..except, try..except on else
   - raise, raise E
   - asm..end
   - type alias
   - inc/dec to += -=
   - case-of
   - use $impl for implementation declarations, can be disabled
   - classes
     - declare using createClass
     - constructor
     - destructor
     - vars
     - class vars
     - ancestor
     - virtual, override, abstract
     - "is" operator
     - "as" operator
     - call inherited "inherited;", "inherited funcname;"
     - call class method
     - read/write class var
   - arrays
     - init as "arr = []"
     - SetLength(arr,newlength,defaultvalue)
     - length(arr)
     - access element: arr[index]
   - rename name conflicts with js identifiers: apply, bind, call, prototype, ...
   - break
   - continue

 ToDos:
   - classes
     + overloads, reintroduce
     + reintroduced variables
     + property
     + class of
   - pass by reference
   - create unique id for local const
   - rename overloaded procs, append $0, $1, ...
   - convert "a div b" to "Math.floor(a / b)"
   - assembler proc modifier: asm..end as whole body
   - enums, sets.
      - include, exclude
      - in-operator
      - *-operator
      - low(), high()
      - ord()
      - type cast number to enumtype
      var s = {};
      s["red"] = enumred; s["green"] = enumgreen;
      Object.keys(s).length === 2;
      s["red"] === true;
      for (var key in s) // arbitrary order
        if (s.hasOwnProperty(key))
          console.log(s[key]);
   - with-do
   - arrays
      - array of record: setlength
      - multi dimensional [index1,index2] -> [index1][index2]
      - static array: non 0 start index
      - static array: length
      - array of static array: setlength
      - array[char]
      - low(), high()
      - constant
      - open arrays
   - record const
   - copy record
   - procedure modifier external
   - library
   - Fix file names on converter errors (relative instead of full)
   - 'use strict' to allow javascript compilers optimize better
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
  DefaultRTLVarName = 'rtl';
  DefaultModulesVarName = 'pas';
  DefaultImplementationVarName = '$impl';
  DefaultLoopEndVarName = '$loopend';
  DefaultCreateClassFuncName = 'createClass'; // rtl.createClass
  DefaultNewClassInstanceFuncName = '$create';
  DefaultFreeClassInstanceFuncName = '$destroy';
  DefaultSetArrayLengthFuncName = 'setArrayLength'; // rtl.setArrayLength
  DefaultLengthFuncName = 'length'; // rtl.length
  DefaultAsFuncName = 'as'; // rtl.as

  JSReservedWords: array[0..60] of string = (
     // keep sorted, first uppercase, then lowercase !
     'Array',
     'Infinity',
     'Math',
     'NaN',
     'Number',
     'Object',
     'String',
     '__extends',
     '_super',
     'anonymous',
     'apply',
     'arguments',
     'array',
     'bind',
     'break',
     'call',
     'case',
     'catch',
     'class',
     'constructor',
     'continue',
     'default',
     'delete',
     'do',
     'each',
     'else',
     'enum',
     'export',
     'extends',
     'false',
     'for',
     'function',
     'getPrototypeOf',
     'if',
     'implements',
     'import',
     'in',
     'instanceof',
     'interface',
     'isPrototypeOf',
     'let',
     'new',
     'null',
     'package',
     'private',
     'protected',
     'prototype',
     'public',
     'return',
     'static',
     'super',
     'switch',
     'this',
     'throw',
     'true',
     'try',
     'undefined',
     'var',
     'while',
     'with',
     'yield'
    );

const
  VarModifiersType = [vmClass,vmStatic];

Type

  { EPas2JS }

  EPas2JS = Class(Exception)
  public
    PasElement: TPasElement;
    MsgNumber: integer;
    Args: TMessageArgs;
    Id: int64;
  end;

  TCtxJSElementKind = (
    cjkRoot,
    cjkObject,
    cjkFunction,
    cjkArray,
    cjkDot);

  TFunctionContext = Class;
  { TConvertContext }

  TConvertContext = Class(TObject)
  public
    PasElement: TPasElement;
    JSElement: TJSElement;
    Resolver: TPasResolver;
    Parent: TConvertContext;
    Kind: TCtxJSElementKind;
    IsSingleton: boolean;
    IsWrite: boolean;
    TmpVarCount: integer;
    constructor Create(PasEl: TPasElement; JSEl: TJSElement; aParent: TConvertContext); virtual;
    function GetRootModule: TPasModule;
    function GetThis: TPasElement;
    function GetThisContext: TFunctionContext;
    function CreateTmpIdentifier(const Prefix: string): string;
  end;

  { TRootContext }

  TRootContext = Class(TConvertContext)
  public
    constructor Create(PasEl: TPasElement; JSEl: TJSElement; aParent: TConvertContext); override;
  end;

  { TFunctionContext }

  TFunctionContext = Class(TConvertContext)
  public
    This: TPasElement;
    constructor Create(PasEl: TPasElement; JSEl: TJSElement; aParent: TConvertContext); override;
  end;

  { TObjectContext }

  TObjectContext  = Class(TConvertContext)
  public
    constructor Create(PasEl: TPasElement; JSEl: TJSElement; aParent: TConvertContext); override;
  end;

  { TInterfaceContext }

  TInterfaceContext = Class(TFunctionContext)
  public
    constructor Create(PasEl: TPasElement; JSEl: TJSElement; aParent: TConvertContext); override;
  end;

  { TDotContext }

  TDotContext = Class(TConvertContext)
  public
    LeftResolved: TPasResolverResult;
    constructor Create(PasEl: TPasElement; JSEl: TJSElement; aParent: TConvertContext); override;
  end;

  TRefPathKind = (
    rpkPath,      // e.g. "TObject"
    rpkPathWithDot, // e.g. "TObject."
    rpkPathAndName // e.g. "TObject.ClassName"
    );

  { TPasToJSConverter }

  TPasToJSConverter = Class(TObject)
  private
    FAsFuncName: TJSString;
    FMainFunction: TJSString;
    FUseLowerCase: boolean;
    FImplementationName: TJSString;
    FUseSwitchStatement: boolean;
    FRTLVarName: TJSString;
    FLoopEndVarName: TJSString;
    FNewClassInstanceFuncName: TJSString;
    FFreeClassInstanceFuncName: TJSString;
    FCreateClassFuncName: TJSString;
    FSetArrayLengthFuncName: TJSString;
    FLengthFuncName: TJSString;
    Procedure AddToSourceElements(Src: TJSSourceElements; El: TJSElement);
    Function CreateBuiltInIdentifierExpr(AName: string): TJSPrimaryExpressionIdent;
    Function CreateIdentifierExpr(AName: string; El: TPasElement; AContext: TConvertContext): TJSPrimaryExpressionIdent;
    Function CreateSubNameExpression(El: TPasElement; const Name: string;
      AContext: TConvertContext): TJSPrimaryExpressionIdent;
    Function CreateTypeDecl(El: TPasType; AContext: TConvertContext): TJSElement;
    Function CreateVarDecl(El: TPasVariable; AContext: TConvertContext): TJSElement;
    Function CreateConstDecl(El: TPasConst; AContext: TConvertContext): TJSElement;
    Function CreateSwitchStatement(El: TPasImplCaseOf; AContext: TConvertContext): TJSElement;
    Function GetFunctionDefinitionInUnary(const fd: TJSFunctionDeclarationStatement;const funname: TJSString; inunary: boolean): TJSFunctionDeclarationStatement;
    Function GetFunctionUnaryName(var je: TJSElement;out fundec: TJSFunctionDeclarationStatement): TJSString;
    Procedure AddProcedureToClass(sl: TJSStatementList; E: TJSElement;const P: TPasProcedure);
    {$IFDEF EnableOldClass}
    Function ConvertClassConstructor(El: TPasConstructor; AContext: TConvertContext): TJSElement; virtual;
    {$ENDIF}
  private
    type
      TForLoopFindData = record
        ForLoop: TPasImplForLoop;
        LoopVar: TPasElement;
        FoundLoop: boolean;
        LoopVarWrite: boolean; // true if first acces of LoopVar after loop is a write
        LoopVarRead: boolean; // true if first acces of LoopVar after loop is a read
      end;
      PForLoopFindData = ^TForLoopFindData;
    procedure ForLoop_OnProcBodyElement(El: TPasElement; arg: pointer);
  private
    FModulesVarName: TJSString;
    type
      TTryExceptFindData = record
        HasRaiseWithoutObject: boolean;
      end;
      PTryExceptFindData = ^TTryExceptFindData;
    procedure TryExcept_OnElement(El: TPasElement; arg: pointer);
  protected
    // Error functions
    Procedure DoError(Id: int64; Const Msg : String);
    Procedure DoError(Id: int64; Const Msg : String; Const Args : Array of Const);
    Procedure DoError(Id: int64; MsgNumber: integer; const MsgPattern: string; Const Args : Array of Const; El: TPasElement);
    procedure RaiseNotSupported(El: TPasElement; AContext: TConvertContext; Id: int64; const Msg: string = '');
    procedure RaiseIdentifierNotFound(Identifier: string; El: TPasElement; Id: int64);
    procedure RaiseInconsistency(Id: int64);
    // Search
    Function GetExpressionValueType(El: TPasExpr; AContext: TConvertContext ): TJSType; virtual;
    Function GetPasIdentValueType(AName: String; AContext: TConvertContext): TJSType; virtual;
    // Name mangling
    {$IFDEF EnableOldClass}
    Function TransformIdent(El: TJSPrimaryExpressionIdent): TJSPrimaryExpressionIdent;virtual;
    {$ENDIF}
    Function TransformVariableName(El: TPasElement; Const AName: String; AContext : TConvertContext): String; virtual;
    Function TransformVariableName(El: TPasElement; AContext : TConvertContext) : String; virtual;
    Function TransformModuleName(El: TPasModule; AContext : TConvertContext) : String; virtual;
    Function IsPreservedWord(aName: string): boolean; virtual;
    Function GetExceptionObjectName(AContext: TConvertContext) : string;
    // Never create an element manually, always use the below functions
    Function CreateElement(C: TJSElementClass; Src: TPasElement): TJSElement; virtual;
    {$IFDEF EnableOldClass}
    Function CreateCallStatement(const JSCallName: string; JSArgs: array of string): TJSCallExpression;
    Function CreateCallStatement(const FunNameEx: TJSElement; JSArgs: array of string): TJSCallExpression;
    Function CreateProcedureDeclaration(const El: TPasElement):TJSFunctionDeclarationStatement;
    {$ENDIF}
    Function CreateFreeOrNewInstanceExpr(Ref: TResolvedReference; AContext : TConvertContext): TJSCallExpression; virtual;
    Procedure CreateProcedureCall(var Call: TJSCallExpression; Args: TParamsExpr; TargetProc: TPasProcedure; AContext: TConvertContext); virtual;
    Procedure CreateProcedureCallArgs(Elements: TJSArrayLiteralElements; Args: TParamsExpr; TargetProc: TPasProcedure; AContext: TConvertContext); virtual;
    Function CreateUnary(Members: array of string; E: TJSElement): TJSUnary;
    Function CreateMemberExpression(Members: array of string): TJSDotMemberExpression;
    Function CreateUsesList(UsesList: TFPList; AContext : TConvertContext): TJSArrayLiteral;
    Procedure AddToStatementList(var First, Last: TJSStatementList;
      Add: TJSElement; Src: TPasElement);
    Function CreateValInit(PasType: TPasType; Expr: TPasElement; El: TPasElement; AContext: TConvertContext): TJSElement;virtual;
    Function CreateVarInit(El: TPasVariable; AContext: TConvertContext): TJSElement;virtual;
    Function CreateRecordInit(aRecord: TPasRecordType; Expr: TPasElement; El: TPasElement; AContext: TConvertContext): TJSElement;virtual;
    Function CreateTypeRef(El: TPasType; AContext : TConvertContext): TJSElement;virtual;
    Function CreateReferencePath(El: TPasElement; AContext : TConvertContext;
      Kind: TRefPathKind; Full: boolean = false): string; virtual;
    Procedure CreateImplementationSection(El: TPasModule; Src: TJSSourceElements; AContext: TConvertContext);
    Procedure CreateInitSection(El: TPasModule; Src: TJSSourceElements; AContext: TConvertContext);
    // Statements
    Function ConvertImplBlockElements(El: TPasImplBlock; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertBeginEndStatement(El: TPasImplBeginBlock; AContext: TConvertContext): TJSElement; virtual;
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
    Function ConvertWithStatement(El: TPasImplWithDo; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertTryStatement(El: TPasImplTry; AContext: TConvertContext ): TJSElement;virtual;
    Function ConvertExceptOn(El: TPasImplExceptOn; AContext: TConvertContext): TJSElement;
    Function ConvertCaseOfStatement(El: TPasImplCaseOf; AContext: TConvertContext): TJSElement;
    Function ConvertAsmStatement(El: TPasImplAsmStatement; AContext: TConvertContext): TJSElement;
    // Expressions
    Function ConvertArrayValues(El: TArrayValues; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertInheritedExpression(El: TInheritedExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertNilExpr(El: TNilExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertParamsExpression(El: TParamsExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBuiltInLength(El: TParamsExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBuiltInSetLength(El: TParamsExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBuiltInContinue(El: TPasExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBuiltInBreak(El: TPasExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBuiltInExit(El: TPasExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBuiltInIncDec(El: TParamsExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertRecordValues(El: TRecordValues; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertSelfExpression(El: TSelfExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBinaryExpression(El: TBinaryExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertBoolConstExpression(El: TBoolConstExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertPrimitiveExpression(El: TPrimitiveExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertIdentifierExpr(El: TPrimitiveExpr; AContext : TConvertContext): TJSElement;virtual;
    Function ConvertUnaryExpression(El: TUnaryExpr; AContext: TConvertContext ): TJSElement;virtual;
    Function ConvertCallExpression(El: TParamsExpr; AContext: TConvertContext ): TJSElement;virtual;
    Function TransFormStringLiteral(S : String) : String;
    // Convert declarations
    Function ConvertElement(El : TPasElement; AContext: TConvertContext) : TJSElement; virtual;
    Function ConvertProperty(El: TPasProperty; AContext: TConvertContext ): TJSElement;virtual;
    Function ConvertCommand(El: TPasImplCommand; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertCommands(El: TPasImplCommands; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertConst(El: TPasConst; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertDeclarations(El: TPasDeclarations; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertExportSymbol(El: TPasExportSymbol; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertExpression(El: TPasExpr; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertImplBlock(El: TPasImplBlock; AContext: TConvertContext ): TJSElement;virtual;
    Function ConvertLabelMark(El: TPasImplLabelMark; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertLabels(El: TPasLabels; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertModule(El: TPasModule; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertPackage(El: TPasPackage; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertArgument(El: TPasArgument; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertProcedure(El: TPasProcedure; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertResString(El: TPasResString; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertType(El: TPasElement; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertVariable(El: TPasVariable; AContext: TConvertContext): TJSElement;virtual;
    Function ConvertRecordType(El: TPasRecordType; AContext: TConvertContext): TJSElement; virtual;
    Function ConvertClassType(El: TPasClassType; AContext: TConvertContext): TJSElement; virtual;
  Public
    Constructor Create;
    Function ConvertPasElement(El : TPasElement; Resolver: TPasResolver) : TJSElement;
    Property MainFunction: TJSString Read FMainFunction Write FMainFunction;
    Property ImplementationName: TJSString read FImplementationName write FImplementationName;// empty to not use, default '$impl'
    Property UseLowerCase: boolean read FUseLowerCase write FUseLowerCase default true;
    Property UseSwitchStatement: boolean read FUseSwitchStatement write FUseSwitchStatement;// default false, because slower than "if" in many engines
    Property RTLVarName: TJSString read FRTLVarName write FRTLVarName;
    Property ModulesVarName: TJSString read FModulesVarName write FModulesVarName;
    Property CreateClassFuncName: TJSString read FCreateClassFuncName write FCreateClassFuncName;
    Property NewClassInstanceFuncName: TJSString read FNewClassInstanceFuncName write FNewClassInstanceFuncName;
    Property FreeClassInstanceFuncName: TJSString read FFreeClassInstanceFuncName write FFreeClassInstanceFuncName;
    Property SetArrayLengthFuncName: TJSString read FSetArrayLengthFuncName write FSetArrayLengthFuncName;
    Property LengthFuncName: TJSString read FLengthFuncName write FLengthFuncName;
    Property LoopEndVarName: TJSString read FLoopEndVarName write FLoopEndVarName;
    Property AsFuncName: TJSString read FAsFuncName write FAsFuncName;
  end;
  EPasToJS = Class(Exception);

var
  DefaultJSExceptionObject: string = '$e';


implementation

{ TDotContext }

constructor TDotContext.Create(PasEl: TPasElement; JSEl: TJSElement;
  aParent: TConvertContext);
begin
  inherited Create(PasEl, JSEl, aParent);
  Kind:=cjkDot;
end;

{ TInterfaceContext }

constructor TInterfaceContext.Create(PasEl: TPasElement; JSEl: TJSElement;
  aParent: TConvertContext);
begin
  inherited;
  IsSingleton:=true;
end;

{ TObjectContext }

constructor TObjectContext.Create(PasEl: TPasElement; JSEl: TJSElement;
  aParent: TConvertContext);
begin
  inherited;
  Kind:=cjkObject;
end;

{ TFunctionContext }

constructor TFunctionContext.Create(PasEl: TPasElement; JSEl: TJSElement;
  aParent: TConvertContext);
begin
  inherited;
  Kind:=cjkFunction;
end;

{ TRootContext }

constructor TRootContext.Create(PasEl: TPasElement; JSEl: TJSElement;
  aParent: TConvertContext);
begin
  inherited;
  Kind:=cjkRoot;
end;

{ TConvertContext }

constructor TConvertContext.Create(PasEl: TPasElement; JSEl: TJSElement;
  aParent: TConvertContext);
begin
  PasElement:=PasEl;
  JSElement:=JsEl;
  Parent:=aParent;
  if Parent<>nil then
    begin
    Resolver:=Parent.Resolver;
    IsWrite:=aParent.IsWrite;
    end;
end;

function TConvertContext.GetRootModule: TPasModule;
var
  aContext: TConvertContext;
begin
  aContext:=Self;
  while aContext.Parent<>nil do
    aContext:=aContext.Parent;
  if aContext.PasElement is TPasModule then
    Result:=TPasModule(aContext.PasElement)
  else
    Result:=nil;
end;

function TConvertContext.GetThis: TPasElement;
var
  ctx: TFunctionContext;
begin
  ctx:=GetThisContext;
  if ctx<>nil then
    Result:=ctx.This
  else
    Result:=nil;
end;

function TConvertContext.GetThisContext: TFunctionContext;
var
  ctx: TConvertContext;
begin
  Result:=nil;
  ctx:=Self;
  repeat
    if ctx is TFunctionContext then
      exit(TFunctionContext(ctx));
    ctx:=ctx.Parent;
  until ctx=nil;
end;

function TConvertContext.CreateTmpIdentifier(const Prefix: string): string;
begin
  inc(TmpVarCount);
  Result:=Prefix+IntToStr(TmpVarCount);
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
  IntfContext: TInterfaceContext;
  VarSt: TJSVariableStatement;
  VarDecl: TJSVarDeclaration;
begin
  Result:=Nil;
  OuterSrc:=TJSSourceElements(CreateElement(TJSSourceElements, El));
  Result:=OuterSrc;

  // create 'rtl.module(...)'
  RegModuleCall:=TJSCallExpression(CreateElement(TJSCallExpression,El));
  AddToSourceElements(OuterSrc,RegModuleCall);
  RegModuleCall.Expr:=CreateMemberExpression([String(RTLVarName),'module']);
  ArgArray := TJSArguments.Create(0, 0, '');
  RegModuleCall.Args:=ArgArray;

  // add unitname parameter: unitname
  ArgEx := TJSLiteral.Create(0,0);
  ModuleName:=TransformModuleName(El,AContext);
  ArgEx.Value.AsString:=TJSString(ModuleName);
  ArgArray.Elements.AddElement.Expr:=ArgEx;

  // add interface-uses-section parameter: [<interface uses1>,<uses2>, ...]
  UsesSection:=nil;
  if (El is TPasProgram) then
    UsesSection:=TPasProgram(El).ProgramSection
  else if (El is TPasLibrary) then
    UsesSection:=TPasLibrary(El).LibrarySection
  else
    UsesSection:=El.InterfaceSection;
  UsesList:=UsesSection.UsesList;
  ArgArray.Elements.AddElement.Expr:=CreateUsesList(UsesList,AContext);

  // add interface parameter: function(){}
  FunDecl:=TJSFunctionDeclarationStatement.Create(0,0);
  ArgArray.Elements.AddElement.Expr:=FunDecl;
  FunDef:=TJSFuncDef.Create;
  FunDecl.AFunction:=FunDef;
  FunDef.Name:='';
  FunBody:=TJSFunctionBody.Create(0,0);
  FunDef.Body:=FunBody;
  Src:=TJSSourceElements(CreateElement(TJSSourceElements, El));
  FunBody.A:=Src;

  IntfContext:=TInterfaceContext.Create(El,Src,AContext);
  try
    IntfContext.This:=El;
    if (El is TPasProgram) then
      begin // program
      if Assigned(TPasProgram(El).ProgramSection) then
        AddToSourceElements(Src,ConvertDeclarations(TPasProgram(El).ProgramSection,IntfContext));
      CreateInitSection(El,Src,IntfContext);
      end
    else if El is TPasLibrary then
      begin // library
      if Assigned(TPasLibrary(El).LibrarySection) then
        AddToSourceElements(Src,ConvertDeclarations(TPasLibrary(El).LibrarySection,IntfContext));
      CreateInitSection(El,Src,IntfContext);
      end
    else
      begin // unit
      // add interface section
      if (ImplementationName<>'') and Assigned(El.ImplementationSection) then
        begin
        // add 'var $impl = {};'
        VarSt:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
        AddToSourceElements(Src,VarSt);
        VarDecl:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
        VarSt.A:=VarDecl;
        VarDecl.Name:=String(ImplementationName);
        VarDecl.Init:=TJSEmptyBlockStatement(CreateElement(TJSEmptyBlockStatement,El.ImplementationSection));
        end;
      if Assigned(El.InterfaceSection) then
        AddToSourceElements(Src,ConvertDeclarations(El.InterfaceSection,IntfContext));
      CreateImplementationSection(El,Src,IntfContext);
      CreateInitSection(El,Src,IntfContext);

      // add optional implementation uses list: [<implementation uses1>,<uses2>, ...]
      if Assigned(El.ImplementationSection) then
        begin
        UsesList:=El.ImplementationSection.UsesList;
        if (UsesList<>nil) and (UsesList.Count>0) then
          ArgArray.Elements.AddElement.Expr:=CreateUsesList(UsesList,AContext);
        end;
      end;
  finally
    IntfContext.Free;
  end;
end;

function TPasToJSConverter.CreateElement(C: TJSElementClass; Src: TPasElement
  ): TJSElement;

var
  Line, Col: Integer;
begin
  if Assigned(Src) then
    begin
    TPasResolver.UnmangleSourceLineNumber(Src.SourceLinenumber,Line,Col);
    Result:=C.Create(Line,Col,Src.SourceFilename);
    end
  else
    Result:=C.Create(0,0);
end;

function TPasToJSConverter.CreateFreeOrNewInstanceExpr(Ref: TResolvedReference;
  AContext: TConvertContext): TJSCallExpression;
// create "$create("funcname");"
var
  ok: Boolean;
  C: TJSCallExpression;
  Proc: TPasProcedure;
  ProcScope: TPasProcedureScope;
  ClassScope: TPasClassScope;
  aClass: TPasElement;
  ArgEx: TJSLiteral;
  ArgElems: TJSArrayLiteralElements;
begin
  Result:=nil;
  //writeln('TPasToJSConverter.CreateNewInstanceStatement Ref.Declaration=',GetObjName(Ref.Declaration));
  Proc:=Ref.Declaration as TPasProcedure;
  if Proc.Name='' then
    RaiseInconsistency(20170125191914);
  //writeln('TPasToJSConverter.CreateNewInstanceStatement Proc.Name=',Proc.Name);
  ProcScope:=Proc.CustomData as TPasProcedureScope;
  //writeln('TPasToJSConverter.CreateNewInstanceStatement ProcScope.Element=',GetObjName(ProcScope.Element),' ProcScope.ClassScope=',GetObjName(ProcScope.ClassScope),' ProcScope.DeclarationProc=',GetObjName(ProcScope.DeclarationProc),' ProcScope.ImplProc=',GetObjName(ProcScope.ImplProc),' ProcScope.CustomData=',GetObjName(ProcScope.CustomData));
  ClassScope:=ProcScope.ClassScope;
  aClass:=ClassScope.Element;
  if aClass.Name='' then
    RaiseInconsistency(20170125191923);
  //writeln('TPasToJSConverter.CreateNewInstanceStatement aClass.Name=',aClass.Name);
  C:=TJSCallExpression(CreateElement(TJSCallExpression,Ref.Element));
  ok:=false;
  try
    // add "$create()"
    C.Expr:=TJSPrimaryExpressionIdent.Create(0,0);
    if rrfNewInstance in Ref.Flags then
      TJSPrimaryExpressionIdent(C.Expr).Name:=NewClassInstanceFuncName
    else
      TJSPrimaryExpressionIdent(C.Expr).Name:=FreeClassInstanceFuncName;
    C.Args:=TJSArguments(CreateElement(TJSArguments,Ref.Element));
    ArgElems:=C.Args.Elements;
    // parameter: "funcname"
    ArgEx := TJSLiteral.Create(0,0);
    ArgEx.Value.AsString:=TJSString(TransformVariableName(Proc,AContext));
    ArgElems.AddElement.Expr:=ArgEx;
    ok:=true;
  finally
    if not ok then
      C.Free;
  end;
  Result:=C;
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
      U:=TJSUnaryMinusExpression(CreateElement(TJSUnaryMinusExpression,El));
      U.A:=E;
      end;
    else
      DoError(20161024191213,nUnaryOpcodeNotSupported,sUnaryOpcodeNotSupported,
              [OpcodeStrings[El.OpCode]],El);
  end;
  Result:=U;
end;

function TPasToJSConverter.ConvertCallExpression(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
begin
  if AContext=nil then ;
  RaiseNotSupported(El,AContext,20161024191225,'ConvertCallExpression');
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

  function CreateDotExpression: TJSElement;
  // connect El.left and El.right with a dot.
  var
    Dot: TJSDotMemberExpression;
    DotContext: TDotContext;
    BParent, A, B: TJSElement;
    ok, OldIsWrite: Boolean;
  begin
    Result:=nil;
    // convert left side
    OldIsWrite:=AContext.IsWrite;
    AContext.IsWrite:=false;
    A:=ConvertElement(El.left,AContext);
    if A=nil then
      RaiseInconsistency(20170201140821);
    AContext.IsWrite:=OldIsWrite;

    // create a dot-context for the right side
    DotContext:=TDotContext.Create(El,A,AContext);
    ok:=false;
    try
      if AContext.Resolver<>nil then
        AContext.Resolver.ComputeElement(El.left,DotContext.LeftResolved,[rcReturnFuncResult]);
      B:=ConvertElement(El.right,DotContext);
      if A=nil then
        RaiseInconsistency(20170201140827);
      // create a TJSDotMemberExpression of A and the left-most identifier of B
      // A becomes the new left-most element of B.
      Result:=B;
      BParent:=nil;
      repeat
        if (B is TJSCallExpression) then
          begin
          BParent:=B;
          B:=TJSCallExpression(B).Expr;
          end
        else if (B is TJSDotMemberExpression) then
          begin
          BParent:=B;
          B:=TJSDotMemberExpression(B).MExpr;
          end
        else if (B is TJSPrimaryExpressionIdent) then
          begin
          // left-most identifier found
            // -> replace it
          Dot := TJSDotMemberExpression(CreateElement(TJSDotMemberExpression, El));
          Dot.MExpr := A;
          Dot.Name := TJSPrimaryExpressionIdent(B).Name;
          if Result=B then
            Result:=Dot
          else if BParent is TJSDotMemberExpression then
            TJSDotMemberExpression(BParent).MExpr:=Dot
          else if BParent is TJSCallExpression then
            TJSCallExpression(BParent).Expr:=Dot
          else
            DoError(20170129141307,'');
          FreeAndNil(B);
          break;
          end
        else
          DoError(20161024191240,nMemberExprMustBeIdentifier,sMemberExprMustBeIdentifier,[],El);
      until false;
      ok:=true;
    finally
      DotContext.Free;
      if not ok then
        begin
        FreeAndNil(A);
        FreeAndNil(Result);
        end;
    end;
  end;

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
  A,B: TJSElement;
  ok: Boolean;
  DotExpr: TJSDotMemberExpression;
  Call: TJSCallExpression;
  {$IFDEF EnableOldClass}
  funname: string;
  {$ENDIF}

begin
  Result:=Nil;

  case El.OpCode of
  eopSubIdent:
    begin
    Result:=CreateDotExpression;
    exit;
    end;
  eopNone:
    if El.left is TInheritedExpr then
      begin
      Result:=ConvertInheritedExpression(TInheritedExpr(El.left),AContext);
      exit;
      end;
  end;

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
        // convert "A as B" to "rtl.as(A,B)"
        Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
        Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
        Call.Expr:=CreateBuiltInIdentifierExpr(String(RTLVarName)+'.'+String(AsFuncName));
        Call.Args.Elements.AddElement.Expr:=A;
        Call.Args.Elements.AddElement.Expr:=B;
        Result:=Call;
        exit;
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
            DoError(20161024191234,nBinaryOpcodeNotSupported,sBinaryOpcodeNotSupported,['logical XOR'],El);
          end;
        end;
      {$IFDEF EnableOldClass}
      else if (A is TJSPrimaryExpressionIdent) and
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
      {$ENDIF}
      else
        DoError(20161024191244,nBinaryOpcodeNotSupported,sBinaryOpcodeNotSupported,[OpcodeStrings[El.OpCode]],El);
    end;
  if (Result=Nil) and (C<>Nil) then
    begin
    if (El.OpCode=eopIs) and (AContext.Resolver<>nil) then
      begin
      // convert "A is B" to "B.isPrototypeOf(A)"
      Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
      Result:=Call;
      Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
      Call.Args.Elements.AddElement.Expr:=A;
      DotExpr:=TJSDotMemberExpression(CreateElement(TJSDotMemberExpression,El));
      DotExpr.MExpr:=B;
      DotExpr.Name:='isPrototypeOf';
      Call.Expr:=DotExpr;
      end
    else
      begin
      R:=TJSBinary(CreateElement(C,El));
      R.A:=A;
      R.B:=B;
      Result:=R;
      end;
    end;
end;

{$IFDEF EnableOldClass}
function TPasToJSConverter.TransformIdent(El: TJSPrimaryExpressionIdent
  ): TJSPrimaryExpressionIdent;

begin
  if UseLowerCase then
    El.Name:=TJSString(lowercase(El.Name));
  Result:=El;
end;
{$ENDIF}

function TPasToJSConverter.CreateIdentifierExpr(AName: string; El: TPasElement;
  AContext: TConvertContext): TJSPrimaryExpressionIdent;

Var
  I : TJSPrimaryExpressionIdent;

begin
  I:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,El));
  AName:=TransformVariableName(El,AName,AContext);
  I.Name:=TJSString(AName);
  Result:=I;
end;

function TPasToJSConverter.CreateSubNameExpression(El: TPasElement;
  const Name: string; AContext: TConvertContext): TJSPrimaryExpressionIdent;
var
  CurName: String;
begin
  CurName:=TransformVariableName(El,Name,AContext);
  if (ImplementationName<>'') and (El.Parent.ClassType=TImplementationSection) then
    CurName:=String(ImplementationName)+'.'+CurName
  else
    CurName:='this.'+CurName;
  Result:=TJSPrimaryExpressionIdent(CreateElement(TJSPrimaryExpressionIdent,El));
  Result.Name:=TJSString(CurName);
end;

function TPasToJSConverter.ConvertPrimitiveExpression(El: TPrimitiveExpr;
  AContext: TConvertContext): TJSElement;

Var
  L : TJSLiteral;
  Number : TJSNumber;
  ConversionError : Integer;
  i: Int64;
  S: String;
begin
  {$IFDEF VerbosePas2JS}
  str(El.Kind,S);
  writeln('TPasToJSConverter.ConvertPrimitiveExpression El=',GetObjName(El),' Context=',GetObjName(AContext),' El.Kind=',S);
  {$ENDIF}
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
      case El.Value[1] of
      '0'..'9':
        begin
        Val(El.Value,Number,ConversionError);
        if ConversionError<>0 then
          DoError(20161024191248,nInvalidNumber,sInvalidNumber,[El.Value],El);
        L:=TJSLiteral(CreateElement(TJSLiteral,El));
        L.Value.AsNumber:=Number;
        if El.Value[1] in ['0'..'9'] then
          L.Value.CustomValue:=TJSString(El.Value);
        end;
      '$','&','%':
        begin
          i:=StrToInt64Def(El.Value,-1);
          if i<0 then
            DoError(20161024224442,nInvalidNumber,sInvalidNumber,[El.Value],El);
          Number:=i;
          if Number<>i then
            // number was rounded -> we lost precision
            DoError(20161024230812,nInvalidNumber,sInvalidNumber,[El.Value],El);
          L:=TJSLiteral(CreateElement(TJSLiteral,El));
          L.Value.AsNumber:=Number;
          S:=copy(El.Value,2,length(El.Value));
          case El.Value[1] of
          '$': S:='0x'+S;
          '&': S:='0'+S; // ToDo: in ECMAScript6 use '0o'
          '%': S:=''; // ToDo: in ECMAScript6 use '0b'
          end;
          L.Value.CustomValue:=TJSString(S);
        end;
      else
        DoError(20161024223232,nInvalidNumber,sInvalidNumber,[El.Value],El);
      end;
      Result:=L;
      end;
    pekIdent:
      Result:=ConvertIdentifierExpr(El,AContext);
    else
      RaiseNotSupported(El,AContext,20161024222543);
  end;
end;

function TPasToJSConverter.ConvertIdentifierExpr(El: TPrimitiveExpr;
  AContext: TConvertContext): TJSElement;
var
  Decl: TPasElement;
  Name: String;
  Ref: TResolvedReference;
  Call: TJSCallExpression;
  Proc: TPasProcedure;
  BuiltInProc: TResElDataBuiltInProc;
begin
  if AContext=nil then ;
  if El.Kind<>pekIdent then
    RaiseInconsistency(20161024191255);
  if El.CustomData is TResolvedReference then
    begin
    Ref:=TResolvedReference(El.CustomData);
    Decl:=Ref.Declaration;
    if [rrfNewInstance,rrfFreeInstance]*Ref.Flags<>[] then
      begin
      // call constructor, destructor
      Result:=CreateFreeOrNewInstanceExpr(Ref,AContext);
      exit;
      end;
    //writeln('TPasToJSConverter.ConvertPrimitiveExpression pekIdent TResolvedReference ',GetObjName(Ref.Declaration),' ',GetObjName(Ref.Declaration.CustomData));
    if Decl.CustomData is TResElDataBuiltInProc then
      begin
      BuiltInProc:=TResElDataBuiltInProc(Decl.CustomData);
      {$IFDEF VerbosePas2JS}
      writeln('TPasToJSConverter.ConvertPrimitiveExpression ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
      {$ENDIF}
      case BuiltInProc.BuiltIn of
        bfBreak: Result:=ConvertBuiltInBreak(El,AContext);
        bfContinue: Result:=ConvertBuiltInContinue(El,AContext);
        bfExit: Result:=ConvertBuiltInExit(El,AContext);
      else
        RaiseNotSupported(El,AContext,20161130164955,'built in proc '+ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
      end;
      if Result<>nil then exit;
      end;
    {$IFDEF VerbosePas2JS}
    writeln('TPasToJSConverter.ConvertIdentifierExpr ',GetObjName(El),' Decl=',GetObjName(Decl),' Decl.Parent=',GetObjName(Decl.Parent));
    {$ENDIF}
    if Decl is TPasModule then
      Name:=String(ModulesVarName)+'.'+TransformModuleName(TPasModule(Decl),AContext)
    else if (Decl is TPasFunctionType) and (CompareText(ResolverResultVar,El.Value)=0) then
      Name:=ResolverResultVar
    else
      begin
      Name:=CreateReferencePath(Decl,AContext,rpkPathAndName);
      end;
    Result:=CreateBuiltInIdentifierExpr(Name);
    if (rrfImplicitCallWithoutParams in Ref.Flags) then
      begin
      // create a call with default parameters
      Call:=nil;
      try
        Proc:=Ref.Declaration as TPasProcedure;
        CreateProcedureCall(Call,nil,Proc,AContext);
        Call.Expr:=Result;
        Result:=Call;
      finally
        if Result<>Call then
          Call.Free;
      end;
      end;
    end
  else if AContext.Resolver<>nil then
    RaiseIdentifierNotFound(El.Value,El,20161024191306)
  else
    // simple mode
    Result:=CreateIdentifierExpr(El.Value,El,AContext);
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

  function CreateAncestorCall(ParentEl: TPasElement; Apply: boolean;
    AncestorProc: TPasProcedure; ParamsExpr: TParamsExpr): TJSElement;
  var
    FunName: String;
    Call: TJSCallExpression;
  begin
    Result:=nil;
    FunName:=CreateReferencePath(AncestorProc,AContext,rpkPathAndName,true);
    if Apply then
      // create "ancestor.funcname.apply(this,arguments)"
      FunName:=FunName+'.apply'
    else
      // create "ancestor.funcname.call(this,param1,param2,...)"
      FunName:=FunName+'.call';
    Call:=nil;
    try
      Call:=TJSCallExpression(CreateElement(TJSCallExpression,ParentEl));
      Call.Args:=TJSArguments(CreateElement(TJSArguments,ParentEl));
      Call.Expr:=CreateBuiltInIdentifierExpr(FunName);
      Call.Args.Elements.AddElement.Expr:=CreateBuiltInIdentifierExpr('this');
      if Apply then
        Call.Args.Elements.AddElement.Expr:=CreateBuiltInIdentifierExpr('arguments')
      else
        CreateProcedureCall(Call,ParamsExpr,AncestorProc,AContext);
      Result:=Call;
    finally
      if Result=nil then
        Call.Free;
    end;
  end;

var
  Right: TPasExpr;
  Ref: TResolvedReference;
  PrimExpr: TPrimitiveExpr;
  AncestorProc: TPasProcedure;
  ParamsExpr: TParamsExpr;
begin
  Result:=nil;
  {$IFDEF EnableOldClass}
  if AContext=nil then ;
  Result := CreateIdentifierExpr('_super',El);
  {$ELSE}
  if (El.Parent is TBinaryExpr) and (TBinaryExpr(El.Parent).OpCode=eopNone)
      and (TBinaryExpr(El.Parent).left=El) then
    begin
    // "inherited <name>"
    AncestorProc:=nil;
    ParamsExpr:=nil;
    Right:=TBinaryExpr(El.Parent).right;
    if Right.ClassType=TPrimitiveExpr then
      begin
      PrimExpr:=TPrimitiveExpr(Right);
      Ref:=PrimExpr.CustomData as TResolvedReference;
      if rrfImplicitCallWithoutParams in Ref.Flags then
        begin
        // inherited <function>
        // -> create "AncestorProc.call(this,defaultargs)"
        AncestorProc:=Ref.Declaration as TPasProcedure;
        end
      else
        begin
        // inherited <varname>
        // all variables have unique names -> simply access it
        Result:=ConvertPrimitiveExpression(PrimExpr,AContext);
        exit;
        end;
      end
    else if Right.ClassType=TParamsExpr then
      begin
      ParamsExpr:=TParamsExpr(Right);
      if ParamsExpr.Kind=pekFuncParams then
        begin
        if ParamsExpr.Value is TPrimitiveExpr then
          begin
          // inherited <function>(args)
          // -> create "AncestorProc.call(this,args,defaultargs)"
          PrimExpr:=TPrimitiveExpr(ParamsExpr.Value);
          Ref:=PrimExpr.CustomData as TResolvedReference;
          AncestorProc:=Ref.Declaration as TPasProcedure;
          end;
        end
      else
        begin
        // inherited <varname>[]
        // all variables have unique names -> simply access it
        Result:=ConvertElement(Right,AContext);
        exit;
        end;
      end;
    if AncestorProc=nil then
      begin
      {$IFDEF VerbosePas2JS}
      writeln('TPasToJSConverter.ConvertInheritedExpression Right=',GetObjName(Right));
      {$ENDIF}
      RaiseNotSupported(El,AContext,20170201190824);
      end;
    //writeln('TPasToJSConverter.ConvertInheritedExpression Func=',GetObjName(FuncContext.PasElement));
    Result:=CreateAncestorCall(Right,false,AncestorProc,ParamsExpr);
    end
  else
    begin
    // "inherited;"
    if El.CustomData=nil then
      exit; // "inherited;" when there is no AncestorProc proc -> silently ignore
    // create "AncestorProc.apply(this,arguments)"
    Ref:=TResolvedReference(El.CustomData);
    AncestorProc:=Ref.Declaration as TPasProcedure;
    Result:=CreateAncestorCall(El,true,AncestorProc,nil);
    end;
  {$ENDIF}
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
  Call : TJSCallExpression;
  E : TJSElement;
  ok, OldIsWrite: Boolean;
  Ref: TResolvedReference;
  BuiltInProc: TResElDataBuiltInProc;
  Elements: TJSArrayLiteralElements;
  TargetProc: TPasProcedure;
  Decl: TPasElement;
begin
  Result:=Nil;
  Case El.Kind of
  pekFuncParams :
    begin
    //writeln('TPasToJSConverter.ConvertParamsExpression START pekFuncParams ',GetObjName(El.CustomData),' ',GetObjName(El.Value.CustomData));
    Call:=nil;
    TargetProc:=nil;
    if El.Value.CustomData is TResolvedReference then
      begin
      Ref:=TResolvedReference(El.Value.CustomData);
      Decl:=Ref.Declaration;
      //writeln('TPasToJSConverter.ConvertParamsExpression pekFuncParams TResolvedReference ',GetObjName(Ref.Declaration),' ',GetObjName(Ref.Declaration.CustomData));
      if Decl.CustomData is TResElDataBuiltInProc then
        begin
        BuiltInProc:=TResElDataBuiltInProc(Decl.CustomData);
        {$IFDEF VerbosePas2JS}
        writeln('TPasToJSConverter.ConvertParamsExpression ',Decl.Name,' ',ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
        {$ENDIF}
        case BuiltInProc.BuiltIn of
          bfLength: Result:=ConvertBuiltInLength(El,AContext);
          bfSetLength: Result:=ConvertBuiltInSetLength(El,AContext);
          bfExit: Result:=ConvertBuiltInExit(El,AContext);
          bfInc,bfDec: Result:=ConvertBuiltInIncDec(El,AContext);
        else
          RaiseNotSupported(El,AContext,20161130164955,'built in proc '+ResolverBuiltInProcNames[BuiltInProc.BuiltIn]);
        end;
        if Result<>nil then exit;
        end
      else if Decl is TPasProcedure then
        TargetProc:=TPasProcedure(Decl);
      if [rrfNewInstance,rrfFreeInstance]*Ref.Flags<>[] then
        // call constructor, destructor
        Call:=CreateFreeOrNewInstanceExpr(Ref,AContext);
      end;
    if Call=nil then
      Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
    OldIsWrite:=AContext.IsWrite;
    ok:=false;
    try
      AContext.IsWrite:=false;
      if Call.Expr=nil then
        Call.Expr:=ConvertElement(El.Value,AContext);
      if Call.Args=nil then
        begin
        // append ()
        Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
        Elements:=Call.Args.Elements;
        end
      else
        begin
        // insert array parameter [], e.g. this.TObject.$create("create",[])
        Elements:=Call.Args.Elements;
        E:=TJSArrayLiteral(CreateElement(TJSArrayLiteral,El));
        Elements.AddElement.Expr:=E;
        Elements:=TJSArrayLiteral(E).Elements;
        end;
      CreateProcedureCallArgs(Elements,El,TargetProc,AContext);
      if Elements.Count=0 then
        begin
        Call.Args.Free;
        Call.Args:=nil;
        end;
      ok:=true;
    finally
      AContext.IsWrite:=OldIsWrite;
      if not ok then FreeAndNil(Call);
    end;
    Result:=Call;
    end;
  pekArrayParams:
    begin
    if Length(El.Params)<>1 then
      Raise EPasToJS.Create('Only 1-dimensional expressions allowed at this point');
    B:=TJSBracketMemberExpression(CreateElement(TJSBracketMemberExpression,El));
    OldIsWrite:=AContext.IsWrite;
    AContext.IsWrite:=false;
    B.MExpr:=ConvertElement(El.Value,AContext);
    Result:=B;
    B.Name:=ConvertElement(El.Params[0],AContext);
    AContext.IsWrite:=OldIsWrite;
    end
  end;
end;

function TPasToJSConverter.ConvertBuiltInLength(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
// length(array) -> rtl.length(array)
var
  Call: TJSCallExpression;
  Arg: TJSElement;
begin
  Result:=nil;
  Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
  try
    // rtl.length()
    Call.Expr:=CreateMemberExpression([String(RTLVarName),String(LengthFuncName)]);
    Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
    // pass param
    Arg:=ConvertElement(El.Params[0],AContext);
    Call.Args.Elements.AddElement.Expr:=Arg;
    Result:=Call;
  finally
    if Result=nil then
      Call.Free;
  end;
end;

function TPasToJSConverter.ConvertBuiltInSetLength(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
// SetLength(array,newlength)
// -> rtl.setArrayLength(array,newlength,initvalue)
var
  Param0: TPasExpr;
  ResolvedParam0: TPasResolverResult;
  ArrayType: TPasArrayType;
  Call: TJSCallExpression;
  ValInit: TJSElement;
begin
  Result:=nil;
  Param0:=El.Params[0];
  AContext.Resolver.ComputeElement(Param0,ResolvedParam0,[rcSkipTypeAlias,rcReturnFuncResult]);
  {$IFDEF VerbosePasResolver}
  writeln('TPasToJSConverter.ConvertBuiltInSetLength ',GetResolverResultDesc(ResolvedParam0));
  {$ENDIF}
  if ResolvedParam0.TypeEl is TPasArrayType then
    begin
    ArrayType:=TPasArrayType(ResolvedParam0.TypeEl);
    {$IFDEF VerbosePasResolver}
    writeln('TPasToJSConverter.ConvertBuiltInSetLength array');
    {$ENDIF}
    Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
    try
      // rtl.setArrayLength()
      Call.Expr:=CreateMemberExpression([String(RTLVarName),String(SetArrayLengthFuncName)]);
      Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
      // 1st param: array
      AContext.IsWrite:=true;
      Call.Args.Elements.AddElement.Expr:=ConvertElement(Param0,AContext);
      AContext.IsWrite:=false;
      // 2nd param: newlength
      Call.Args.Elements.AddElement.Expr:=ConvertElement(El.Params[1],AContext);
      // 3rd param: default value
      ValInit:=CreateValInit(ArrayType.ElType,nil,Param0,AContext);
      Call.Args.Elements.AddElement.Expr:=ValInit;
      Result:=Call;
    finally
      if Result=nil then
        Call.Free;
    end;
    exit;
    end;
  RaiseNotSupported(El,AContext,20170130141026);
end;

function TPasToJSConverter.ConvertBuiltInContinue(El: TPasExpr;
  AContext: TConvertContext): TJSElement;
begin
  if AContext=nil then;
  Result:=TJSContinueStatement(CreateElement(TJSContinueStatement,El));
end;

function TPasToJSConverter.ConvertBuiltInBreak(El: TPasExpr;
  AContext: TConvertContext): TJSElement;
begin
  if AContext=nil then;
  Result:=TJSBreakStatement(CreateElement(TJSBreakStatement,El));
end;

function TPasToJSConverter.ConvertBuiltInExit(El: TPasExpr;
  AContext: TConvertContext): TJSElement;
// convert "exit;" -> in a function: "return result;"  in a procedure: "return;"
// convert "exit(param);" -> "return param;"
var
  ProcEl: TPasElement;
begin
  Result:=TJSReturnStatement(CreateElement(TJSReturnStatement,El));
  if (El is TParamsExpr) and (length(TParamsExpr(El).Params)>0) then
    begin
    // with parameter. convert "exit(param);" -> "return param;"
    TJSReturnStatement(Result).Expr:=ConvertExpression(TParamsExpr(El).Params[0],AContext);
    end
  else
    begin
    // without parameter.
    ProcEl:=El.Parent;
    while not (ProcEl is TPasProcedure) do ProcEl:=ProcEl.Parent;
    if ProcEl is TPasFunction then
      // in a function, "return result;"
      TJSReturnStatement(Result).Expr:=CreateBuiltInIdentifierExpr(ResolverResultVar)
    else
      ; // in a procedure, "return;" which means "return undefined;"
    end;
end;

function TPasToJSConverter.ConvertBuiltInIncDec(El: TParamsExpr;
  AContext: TConvertContext): TJSElement;
// convert inc(a,b) to a+=b
// convert dec(a,b) to a-=b
var
  AssignSt: TJSAssignStatement;
  L: TJSLiteral;
begin
  if CompareText((El.Value as TPrimitiveExpr).Value,'inc')=0 then
    AssignSt:=TJSAddEqAssignStatement(CreateElement(TJSAddEqAssignStatement,El))
  else
    AssignSt:=TJSSubEqAssignStatement(CreateElement(TJSSubEqAssignStatement,El));
  Result:=AssignSt;
  AssignSt.LHS:=ConvertExpression(El.Params[0],AContext);
  if length(El.Params)=1 then
    begin
    L:=TJSLiteral(CreateElement(TJSLiteral,El));
    L.Value.AsNumber:=1;
    AssignSt.Expr:=L;
    end
  else
    begin
    AssignSt.Expr:=ConvertExpression(El.Params[1],AContext);
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
  {$IFDEF VerbosePas2JS}
  writeln('TPasToJSConverter.ConvertExpression El=',GetObjName(El),' Context=',GetObjName(AContext));
  {$ENDIF}
  Result:=Nil;
  if (El.ClassType=TUnaryExpr) then
    Result:=ConvertUnaryExpression(TUnaryExpr(El),AContext)
  else if (El.ClassType=TBinaryExpr) then
    Result:=ConvertBinaryExpression(TBinaryExpr(El),AContext)
  else if (El.ClassType=TPrimitiveExpr) then
    Result:=ConvertPrimitiveExpression(TPrimitiveExpr(El),AContext)
  else if (El.ClassType=TBoolConstExpr) then
    Result:=ConvertBoolConstExpression(TBoolConstExpr(El),AContext)
  else if (El.ClassType=TNilExpr) then
    Result:=ConvertNilExpr(TNilExpr(El),AContext)
  else if (El.ClassType=TInheritedExpr) then
    Result:=ConvertInheritedExpression(TInheritedExpr(El),AContext)
  else if (El.ClassType=TSelfExpr) then
    Result:=ConvertSelfExpression(TSelfExpr(El),AContext)
  else if (El.ClassType=TParamsExpr) then
    Result:=ConvertParamsExpression(TParamsExpr(El),AContext)
  else if (El.ClassType=TRecordValues) then
    Result:=ConvertRecordValues(TRecordValues(El),AContext)
  else
    RaiseNotSupported(El,AContext,20161024191314);
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
  if El is TPasClassType then
    Result := ConvertClassType(TPasClassType(El), AContext)
  else if El is TPasRecordType then
    Result := ConvertRecordType(TPasRecordType(El), AContext);
  // other types don't need a constructor function
end;

function TPasToJSConverter.CreateVarDecl(El: TPasVariable;
  AContext: TConvertContext): TJSElement;

Var
  C : TJSElement;
  V : TJSVariableStatement;
  AssignSt: TJSSimpleAssignStatement;
  Obj: TJSObjectLiteral;
  ObjLit: TJSObjectLiteralElement;

begin
  if AContext is TObjectContext then
    begin
    // create 'A: initvalue'
    Obj:=TObjectContext(AContext).JSElement as TJSObjectLiteral;
    ObjLit:=Obj.Elements.AddElement;
    ObjLit.Name:=TJSString(TransformVariableName(El,AContext));
    ObjLit.Expr:=CreateVarInit(El,AContext);
    end
  else if AContext.IsSingleton then
    begin
    // create 'this.A=initvalue'
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    Result:=AssignSt;
    AssignSt.LHS:=CreateSubNameExpression(El,El.Name,AContext);
    AssignSt.Expr:=CreateVarInit(El,AContext);
    end
  else
    begin
    // create 'var A=initvalue'
    C:=ConvertVariable(El,AContext);
    V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    V.A:=C;
    Result:=V;
    end;
end;

function TPasToJSConverter.CreateConstDecl(El: TPasConst;
  AContext: TConvertContext): TJSElement;

Var
  AssignSt: TJSSimpleAssignStatement;
  Obj: TJSObjectLiteral;
  ObjLit: TJSObjectLiteralElement;

begin
  if AContext is TObjectContext then
    begin
    // create 'A: initvalue'
    Obj:=TObjectContext(AContext).JSElement as TJSObjectLiteral;
    ObjLit:=Obj.Elements.AddElement;
    ObjLit.Name:=TJSString(TransformVariableName(El,AContext));
    ObjLit.Expr:=CreateVarInit(El,AContext);
    end
  else
    begin
    if not AContext.IsSingleton then begin
      // local const are stored in interface/implementation
      //GetSingletonParent();

      // ToDo: avoid name clash
      RaiseNotSupported(El,AContext,20161127165213,'todo: check for name clash and rename');
    end;
    // create 'this.A=initvalue'
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    Result:=AssignSt;
    AssignSt.LHS:=CreateSubNameExpression(El,El.Name,AContext);
    AssignSt.Expr:=CreateVarInit(El,AContext);
    end;
end;

function TPasToJSConverter.CreateSwitchStatement(El: TPasImplCaseOf;
  AContext: TConvertContext): TJSElement;
var
  SwitchEl: TJSSwitchStatement;
  JSCaseEl: TJSCaseElement;
  SubEl: TPasImplElement;
  St: TPasImplCaseStatement;
  ok: Boolean;
  i, j: Integer;
  BreakSt: TJSBreakStatement;
  BodySt: TJSElement;
  StList: TJSStatementList;
  Expr: TPasExpr;
begin
  Result:=nil;
  SwitchEl:=TJSSwitchStatement(CreateElement(TJSSwitchStatement,El));
  ok:=false;
  try
    SwitchEl.Cond:=ConvertExpression(El.CaseExpr,AContext);
    for i:=0 to El.Elements.Count-1 do
      begin
      SubEl:=TPasImplElement(El.Elements[i]);
      if not (SubEl is TPasImplCaseStatement) then
        continue;
      St:=TPasImplCaseStatement(SubEl);
      JSCaseEl:=nil;
      for j:=0 to St.Expressions.Count-1 do
        begin
        Expr:=TPasExpr(St.Expressions[j]);
        JSCaseEl:=SwitchEl.Cases.AddCase;
        JSCaseEl.Expr:=ConvertExpression(Expr,AContext);
        end;
      BodySt:=nil;
      if St.Body<>nil then
        BodySt:=ConvertElement(St.Body,AContext);
      // add break
      BreakSt:=TJSBreakStatement(CreateElement(TJSBreakStatement,St));
      if BodySt=nil then
        // no Pascal statement -> add only one 'break;'
        BodySt:=BreakSt
      else
        begin
        if (BodySt is TJSStatementList) then
          begin
          // list of statements -> append 'break;' to end
          StList:=TJSStatementList(BodySt);
          AddToStatementList(TJSStatementList(BodySt),StList,BreakSt,St);
          end
        else
          begin
          // single statement -> create list of old and 'break;'
          StList:=TJSStatementList(CreateElement(TJSStatementList,St));
          StList.A:=BodySt;
          StList.B:=BreakSt;
          BodySt:=StList;
          end;
        end;
      JSCaseEl.Body:=BodySt;
      end;
    if El.ElseBranch<>nil then
      begin
      JSCaseEl:=SwitchEl.Cases.AddCase;
      JSCaseEl.Body:=ConvertImplBlockElements(El.ElseBranch,AContext);
      SwitchEl.TheDefault:=JSCaseEl;
      end;
    ok:=true;
  finally
    if not ok then
      SwitchEl.Free;
  end;
  Result:=SwitchEl;
end;

function TPasToJSConverter.ConvertDeclarations(El: TPasDeclarations;
  AContext: TConvertContext): TJSElement;

Var
  E : TJSElement;
  SLFirst, SLLast: TJSStatementList;
  P: TPasElement;
  IsTopLvl, IsProcBody, IsFunction: boolean;
  I : Integer;
  PasProc: TPasProcedure;
  ProcScope: TPasProcedureScope;

  Procedure Add(NewEl: TJSElement);
  begin
    if AContext is TObjectContext then
      begin
      // NewEl is already added
      end
    else
      begin
      AddToStatementList(SLFirst,SLLast,NewEl,El);
      ConvertDeclarations:=SLFirst;
      end;
  end;

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
    Add(VarSt);
    Result:=SLFirst;
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    VarSt.A:=AssignSt;
    AssignSt.LHS:=CreateBuiltInIdentifierExpr(ResolverResultVar);
    AssignSt.Expr:=CreateValInit(ResultEl.ResultType,nil,El,aContext);
  end;

  Procedure AddFunctionResultReturn;
  var
    RetSt: TJSReturnStatement;
  begin
    RetSt:=TJSReturnStatement(CreateElement(TJSReturnStatement,El));
    RetSt.Expr:=CreateBuiltInIdentifierExpr(ResolverResultVar);
    Add(RetSt);
  end;

begin
  Result:=nil;
  {
    TPasDeclarations = class(TPasElement)
    TPasSection = class(TPasDeclarations)
    TInterfaceSection = class(TPasSection)
    TImplementationSection = class(TPasSection)
    TProgramSection = class(TImplementationSection)
    TLibrarySection = class(TImplementationSection)
    TProcedureBody = class(TPasDeclarations)
  }

  SLFirst:=nil;
  SLLast:=nil;
  IsTopLvl:=AContext.IsSingleton;
  IsProcBody:=(El is TProcedureBody) and (TProcedureBody(El).Body<>nil);
  IsFunction:=IsProcBody and (El.Parent is TPasFunction);

  if IsProcBody and IsFunction then
    AddFunctionResultInit;

  For I:=0 to El.Declarations.Count-1 do
    begin
    E:=Nil;
    P:=TPasElement(El.Declarations[i]);
    //writeln('TPasToJSConverter.ConvertDeclarations El[',i,']=',GetObjName(P));
    if P.ClassType=TPasConst then
      begin
      E:=CreateConstDecl(TPasConst(P),aContext);
      if not IsTopLvl then
        // const was added to higher context
        continue;
      end
    else if P.ClassType=TPasVariable then
      E:=CreateVarDecl(TPasVariable(P),aContext)
    else if P is TPasType then
      E:=CreateTypeDecl(TPasType(P),aContext)
    else if P is TPasProcedure then
      begin
      PasProc:=TPasProcedure(P);
      if PasProc.IsForward then continue; // JavaScript does not need the forward
      ProcScope:=TPasProcedureScope(PasProc.CustomData);
      if (ProcScope.DeclarationProc<>nil)
          and (not ProcScope.DeclarationProc.IsForward) then
        continue; // this proc was already converted in interface or class
      if ProcScope.ImplProc<>nil then
        P:=ProcScope.ImplProc;
      E:=ConvertProcedure(TPasProcedure(P),aContext);
      end
    else
      RaiseNotSupported(P as TPasElement,AContext,20161024191434);
    Add(E);
    end;

  if IsProcBody and (TProcedureBody(El).Body.Elements.Count>0) then
    begin
    E:=ConvertElement(TProcedureBody(El).Body,aContext);
    Add(E);
    end;

  if IsProcBody and IsFunction then
    AddFunctionResultReturn;
end;

function TPasToJSConverter.ConvertType(El: TPasElement;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024191443);
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

{$IFDEF EnableOldClass}
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
  aMember: TPasElement;
  j: integer;
  ret: TJSReturnStatement;
  jsName: String;
  FuncContext: TFunctionContext;
  Src: TJSSourceElements;
begin
  //ctname := El.FullName;
  jsName:=TransformVariableName(El,AContext);
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
  Src:=TJSSourceElements(CreateElement(TJSSourceElements, El));
  FD.Body.A := Src;

  FuncContext:=TFunctionContext.Create(El,Src,AContext);
  try
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
    //create default constructor
    cons := CreateProcedureDeclaration(El);
    TJSSourceElements(FD.Body.A).Statements.AddNode.Node := cons;
    cons.AFunction.Name := TJSString(jsName);

    //convert class members
    for j := 0 to El.Members.Count - 1 do
    begin
      aMember := TPasElement(El.Members[j]);
      //memname := aMember.FullName;
      je := ConvertClassMember(aMember, FuncContext);
      if Assigned(je) then
        TJSSourceElements(FD.Body.A).Statements.AddNode.Node := je;
    end;
  finally
    FuncContext.Free;
  end;

  //add return statement
  ret := TJSReturnStatement(CreateElement(TJSReturnStatement, El));
  TJSSourceElements(FD.Body.A).Statements.AddNode.Node := ret;
  ret.Expr := CreateIdentifierExpr(El.Name,El);
  Result := unary;
end;
{$ENDIF}

function TPasToJSConverter.ConvertClassType(El: TPasClassType;
  AContext: TConvertContext): TJSElement;
(*
  type
    TMyClass = class(Ancestor)
      i: longint;
    end;

    rtl.createClass(this,"TMyClass",Ancestor,function(){
      this.i = 0;
    });
*)
const
  VarModifiersAllowed = [vmClass,vmStatic];

  procedure RaiseVarModifierNotSupported(V: TPasVariable);
  var
    s: String;
    m: TVariableModifier;
  begin
    s:='';
    for m in TVariableModifiers do
      if not (m in VarModifiersAllowed) then
        begin
        str(m,s);
        RaiseNotSupported(V,AContext,20170204224818,'modifier '+s);
        end;
  end;

  procedure AddInstanceInitFunction(Src: TJSSourceElements;
    ClassContext: TConvertContext; Ancestor: TPasType);
  // add instance initialization function:
  //   this.$init = function(){
  //     ancestor.$init();
  //     ... init variables ...
  //   }
  var
    FuncVD: TJSVarDeclaration;
    FunDef: TJSFuncDef;
    New_Src: TJSSourceElements;
    New_FuncContext: TFunctionContext;
    I: Integer;
    P: TPasElement;
    NewEl: TJSElement;
    Call: TJSCallExpression;
    AncestorPath: String;
  begin
    FuncVD:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
    AddToSourceElements(Src,FuncVD);
    FuncVD.Name:='this.$init';
    FuncVD.Init:=TJSFunctionDeclarationStatement.Create(0,0);
    FunDef:=TJSFuncDef.Create;
    TJSFunctionDeclarationStatement(FuncVD.Init).AFunction:=FunDef;
    FunDef.Name:='';
    FunDef.Body:=TJSFunctionBody.Create(0,0);
    New_Src:=TJSSourceElements(CreateElement(TJSSourceElements, El));
    FunDef.Body.A:=New_Src;

    // call ancestor.$init.call(this)
    if Ancestor<>nil then
      begin
      Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
      AncestorPath:=CreateReferencePath(Ancestor,ClassContext,rpkPathAndName);
      Call.Expr:=CreateBuiltInIdentifierExpr(AncestorPath+'.$init.call');
      Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
      Call.Args.Elements.AddElement.Expr:=CreateBuiltInIdentifierExpr('this');
      AddToSourceElements(New_Src,Call);
      end;

    // add instance members
    New_FuncContext:=TFunctionContext.Create(El,New_Src,ClassContext);
    try
      New_FuncContext.This:=El;
      New_FuncContext.IsSingleton:=true;
      // add class members
      For I:=0 to El.Members.Count-1 do
        begin
        P:=TPasElement(El.Members[i]);
        if (P.ClassType=TPasVariable)
            and (VarModifiersType*TPasVariable(P).VarModifiers=[]) then
          NewEl:=CreateVarDecl(TPasVariable(P),New_FuncContext)
        else
          continue;
        if NewEl=nil then
          RaiseNotSupported(P,New_FuncContext,20170204224039);
        AddToSourceElements(New_Src,NewEl);
        end;
    finally
      New_FuncContext.Free;
    end;
  end;

var
  Call: TJSCallExpression;
  FunDecl: TJSFunctionDeclarationStatement;
  FunDef: TJSFuncDef;
  Src: TJSSourceElements;
  ArgEx: TJSLiteral;
  FuncContext: TFunctionContext;
  I: Integer;
  NewEl: TJSElement;
  P: TPasElement;
  Scope: TPasClassScope;
  Ancestor: TPasType;
  AncestorPath: String;
begin
  Result:=nil;
  if El.IsForward then
    exit(nil);

  if El.CustomData is TPasClassScope then
    Scope:=TPasClassScope(El.CustomData)
  else
    Scope:=nil;

  // create call 'rtl.createClass('
  Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
  try
    Call.Expr:=CreateMemberExpression([String(RTLVarName),String(CreateClassFuncName)]);
    Call.Args:=TJSArguments(CreateElement(TJSArguments,El));

    // add parameter: owner. 'this' for top level class.
    Call.Args.Elements.AddElement.Expr:=CreateBuiltInIdentifierExpr('this');

    // add parameter: string constant '"classname"'
    ArgEx := TJSLiteral(CreateElement(TJSLiteral,El));
    ArgEx.Value.AsString:=TJSString(TransformVariableName(El,AContext));
    Call.Args.Elements.AddElement.Expr:=ArgEx;

    // add parameter: ancestor
    if (Scope<>nil) and (Scope.AncestorScope<>nil) then
      Ancestor:=Scope.AncestorScope.Element as TPasType
    else
      Ancestor:=El.AncestorType;
    if Ancestor<>nil then
      AncestorPath:=CreateReferencePath(Ancestor,AContext,rpkPathAndName)
    else
      AncestorPath:='null';
    Call.Args.Elements.AddElement.Expr:=CreateBuiltInIdentifierExpr(AncestorPath);

    // add parameter: class initialize function 'function(){...}'
    FunDecl:=TJSFunctionDeclarationStatement.Create(0,0);
    Call.Args.Elements.AddElement.Expr:=FunDecl;
    FunDef:=TJSFuncDef.Create;
    FunDecl.AFunction:=FunDef;
    FunDef.Name:='';
    FunDef.Body:=TJSFunctionBody.Create(0,0);
    Src:=TJSSourceElements(CreateElement(TJSSourceElements, El));
    FunDef.Body.A:=Src;

    // add members
    FuncContext:=TFunctionContext.Create(El,Src,AContext);
    try
      FuncContext.IsSingleton:=true;
      FuncContext.This:=El;
      // add class members: types and class vars
      For I:=0 to El.Members.Count-1 do
        begin
        P:=TPasElement(El.Members[i]);
        //writeln('TPasToJSConverter.ConvertClassType class El[',i,']=',GetObjName(P));
        if P.ClassType=TPasVariable then
          begin
          if TPasVariable(P).VarModifiers-VarModifiersAllowed<>[] then
            RaiseVarModifierNotSupported(TPasVariable(P));
          if VarModifiersType*TPasVariable(P).VarModifiers<>[] then
            NewEl:=CreateVarDecl(TPasVariable(P),FuncContext)
          else
            continue;
          end
        else if P.ClassType=TPasConst then
          NewEl:=CreateConstDecl(TPasConst(P),aContext)
        else if P is TPasType then
          NewEl:=CreateTypeDecl(TPasType(P),aContext)
        else if P is TPasProcedure then
          continue
        else
          RaiseNotSupported(P,FuncContext,20161221233338);
        if NewEl=nil then
          RaiseNotSupported(P,FuncContext,20170204223922);
        AddToSourceElements(Src,NewEl);
        end;

      // instance initialization function
      AddInstanceInitFunction(Src,FuncContext,Ancestor);

      // add methods
      For I:=0 to El.Members.Count-1 do
        begin
        P:=TPasElement(El.Members[i]);
        //writeln('TPasToJSConverter.ConvertClassType class El[',i,']=',GetObjName(P));
        if P is TPasProcedure then
          NewEl:=ConvertProcedure(TPasProcedure(P),aContext)
        else
          continue;
        if NewEl=nil then
          continue; // e.g. abstract proc
        AddToSourceElements(Src,NewEl);
        end;

    finally
      FuncContext.Free;
    end;

    Result:=Call;
  finally
    if Result<>Call then
      Call.Free;
  end;
end;

{$IFDEF EnableOldClass}
function TPasToJSConverter.ConvertClassConstructor(El: TPasConstructor;
  AContext: TConvertContext): TJSElement;
var
  FS: TJSFunctionDeclarationStatement;
  n: integer;
  Fun1SourceEl: TJSSourceElements;
  ret: TJSReturnStatement;
  nmem: TJSNewMemberExpression;
  Arg: TPasArgument;
begin
  if AContext=nil then ;
  FS := CreateProcedureDeclaration(El);
  FS.AFunction.Name := TJSString(El.Name);
  Fs.AFunction.Body := TJSFunctionBody(CreateElement(TJSFunctionBody, El.Body));
  Fun1SourceEl := TJSSourceElements.Create(0, 0, '');
  fs.AFunction.Body.A := Fun1SourceEl;
  ret := TJSReturnStatement.Create(0, 0, '');
  Fun1SourceEl.Statements.AddNode.Node := ret;
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
  Result := CreateUnary([El.Parent.FullName, TPasProcedure(El).Name], FS);
end;
{$ENDIF}

procedure TPasToJSConverter.ForLoop_OnProcBodyElement(El: TPasElement;
  arg: pointer);
// Called by ConvertForStatement on each element of the current proc body
// Check each element that lies behind the loop if it is reads the LoopVar
var
  Data: PForLoopFindData absolute arg;
begin
  if El.HasParent(Data^.ForLoop) then
    Data^.FoundLoop:=true
  else if Data^.FoundLoop and (not Data^.LoopVarWrite) and (not Data^.LoopVarRead) then
    begin
    // El comes after loop and LoopVar was not yet accessed
    if (El.CustomData is TResolvedReference)
        and (TResolvedReference(El.CustomData).Declaration=Data^.LoopVar) then
      begin
        // El refers the LoopVar
        // ToDo: check write only access
        Data^.LoopVarRead:=true;
      end;
    end;
end;

procedure TPasToJSConverter.TryExcept_OnElement(El: TPasElement; arg: pointer);
var
  Data: PTryExceptFindData absolute arg;
begin
  if (El is TPasImplRaise) and (TPasImplRaise(El).ExceptObject=nil) then
    Data^.HasRaiseWithoutObject:=true;
end;

constructor TPasToJSConverter.Create;
begin
  FUseLowerCase:=true;
  FRTLVarName:=DefaultRTLVarName;
  FModulesVarName:=DefaultModulesVarName;
  FImplementationName:=DefaultImplementationVarName;
  FCreateClassFuncName:=DefaultCreateClassFuncName;
  FNewClassInstanceFuncName:=DefaultNewClassInstanceFuncName;
  FFreeClassInstanceFuncName:=DefaultFreeClassInstanceFuncName;
  FSetArrayLengthFuncName:=DefaultSetArrayLengthFuncName;
  FLengthFuncName:=DefaultLengthFuncName;
  FLoopEndVarName:=DefaultLoopEndVarName;
  FAsFuncName:=DefaultAsFuncName;
end;

function TPasToJSConverter.ConvertProcedure(El: TPasProcedure;
  AContext: TConvertContext): TJSElement;

Var
  FS : TJSFunctionDeclarationStatement;
  FD : TJSFuncDef;
  n:Integer;
  FunName: String;
  AssignSt: TJSSimpleAssignStatement;
  FuncContext: TFunctionContext;
  ProcScope: TPasProcedureScope;
  Arg: TPasArgument;
  DeclProc, ImplProc: TPasProcedure;

begin
  Result:=nil;

  if El.IsAbstract then exit;

  {$IFDEF VerbosePas2JS}
  writeln('TPasToJSConverter.ConvertProcedure "',El.Name,'" ',El.Parent.ClassName);
  {$ENDIF}

  DeclProc:=El;
  ImplProc:=El;
  ProcScope:=TPasProcedureScope(El.CustomData);
  if ProcScope.DeclarationProc<>nil then
    begin
    DeclProc:=ProcScope.DeclarationProc;
    FunName:=TransformVariableName(ProcScope.DeclarationProc,AContext)
    end
  else
    FunName:=TransformVariableName(El,AContext);
  if ProcScope.ImplProc<>nil then
    ImplProc:=ProcScope.ImplProc;

  AssignSt:=nil;
  if AContext.IsSingleton then
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
  for n := 0 to DeclProc.ProcType.Args.Count - 1 do
    begin
    Arg:=TPasArgument(DeclProc.ProcType.Args[n]);
    FD.Params.Add(TransformVariableName(Arg,AContext));
    end;

  if ImplProc.Body<>nil then
    begin
    FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,ImplProc.Body));
    FuncContext:=TFunctionContext.Create(ImplProc,FD.Body,AContext);
    try
      if ProcScope.ClassScope<>nil then
        FuncContext.This:=ProcScope.ClassScope.Element
      else
        FuncContext.This:=AContext.GetThis;
      FD.Body.A:=ConvertDeclarations(ImplProc.Body,FuncContext);
    finally
      FuncContext.Free;
    end;
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
      if JSImpl=nil then
        continue; // e.g. "inherited;" when there is no ancestor proc
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
  FuncContext: TFunctionContext;
begin
  // create: 'this.$init=function(){}'

  IsMain:=(El.Parent<>nil) and (El.Parent is TPasProgram);
  FunName:=String(MainFunction);
  if FunName='' then
    if IsMain then
      FunName:='$main'
    else
      FunName:='$init';

  AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
  Result:=AssignSt;
  FuncContext:=nil;
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
      FuncContext:=TFunctionContext.Create(El,FD.Body,AContext);
      FuncContext.This:=AContext.GetThis;
      FD.Body.A:=ConvertImplBlockElements(El,FuncContext);
      end;
    ok:=true;
  finally
    FuncContext.Free;
    if not ok then FreeAndNil(Result);
  end;
end;

function TPasToJSConverter.ConvertFinalizationSection(El: TFinalizationSection;
  AContext: TConvertContext): TJSElement;
begin
  Result:=nil;
  RaiseNotSupported(El,AContext,20161024192519);
end;

function TPasToJSConverter.ConvertTryStatement(El: TPasImplTry;
  AContext: TConvertContext): TJSElement;

  function NeedExceptObject: boolean;
  var
    Data: TTryExceptFindData;
  begin
    Result:=false;
    if El.FinallyExcept.Elements.Count=0 then exit;
    if TPasElement(El.FinallyExcept.Elements[0]) is TPasImplExceptOn then
      exit(true);
    Data:=Default(TTryExceptFindData);
    El.FinallyExcept.ForEachCall(@TryExcept_OnElement,@Data);
    Result:=Data.HasRaiseWithoutObject;
  end;

Var
  T : TJSTryStatement;
  ExceptBlock: TPasImplTryHandler;
  i: Integer;
  ExceptOn: TPasImplExceptOn;
  IfSt, Last: TJSIfStatement;

begin
  Result:=nil;
  T:=nil;
  try
    if El.FinallyExcept is TPasImplTryFinally then
      begin
      T:=TJSTryFinallyStatement(CreateElement(TJSTryFinallyStatement,El));
      T.Block:=ConvertImplBlockElements(El,AContext);
      T.BFinally:=ConvertImplBlockElements(El.FinallyExcept,AContext);
      end
    else
      begin
      T:=TJSTryCatchStatement(CreateElement(TJSTryCatchStatement,El));
      T.Block:=ConvertImplBlockElements(El,AContext);
      if NeedExceptObject then
        T.Ident:=TJSString(GetExceptionObjectName(AContext));
      //T.BCatch:=ConvertElement(El.FinallyExcept,AContext);
      ExceptBlock:=El.FinallyExcept;
      if (ExceptBlock.Elements.Count>0)
          and (TPasImplElement(ExceptBlock.Elements[0]) is TPasImplExceptOn) then
        begin
        Last:=nil;
        for i:=0 to ExceptBlock.Elements.Count-1 do
          begin
          ExceptOn:=TObject(ExceptBlock.Elements[i]) as TPasImplExceptOn;
          IfSt:=ConvertExceptOn(ExceptOn,AContext) as TJSIfStatement;
          if Last=nil then
            T.BCatch:=IfSt
          else
            Last.BFalse:=IfSt;
          Last:=IfSt;
          end;
        if El.ElseBranch<>nil then
          Last.BFalse:=ConvertImplBlockElements(El.ElseBranch,AContext);
        end
      else
        begin
        if El.ElseBranch<>nil then
          RaiseNotSupported(El.ElseBranch,AContext,20170205003014);
        T.BCatch:=ConvertImplBlockElements(ExceptBlock,AContext);
        end;
      end;
    Result:=T;
  finally
    if Result=nil then
      T.Free;
  end;
end;

function TPasToJSConverter.ConvertCaseOfStatement(El: TPasImplCaseOf;
  AContext: TConvertContext): TJSElement;
var
  SubEl: TPasImplElement;
  St: TPasImplCaseStatement;
  ok: Boolean;
  i, j: Integer;
  JSExpr: TJSElement;
  StList: TJSStatementList;
  Expr: TPasExpr;
  IfSt, LastIfSt: TJSIfStatement;
  TmpVarName: String;
  VarDecl: TJSVarDeclaration;
  VarSt: TJSVariableStatement;
  JSOrExpr: TJSLogicalOrExpression;
  JSAndExpr: TJSLogicalAndExpression;
  JSLEExpr: TJSRelationalExpressionLE;
  JSGEExpr: TJSRelationalExpressionGE;
  JSEQExpr: TJSEqualityExpressionEQ;
begin
  Result:=nil;
  if UseSwitchStatement then
    begin
    // convert to switch statement
    // switch does not support ranges -> check
    ok:=true;
    for i:=0 to El.Elements.Count-1 do
      begin
      SubEl:=TPasImplElement(El.Elements[i]);
      if not (SubEl is TPasImplCaseStatement) then
        continue;
      St:=TPasImplCaseStatement(SubEl);
      for j:=0 to St.Expressions.Count-1 do
        begin
        Expr:=TPasExpr(St.Expressions[j]);
        if (Expr is TBinaryExpr) and (TBinaryExpr(Expr).Kind=pekRange) then
          begin
          ok:=false;
          break;
          end;
        end;
      if not ok then break;
      end;
    if ok then
      begin
      Result:=CreateSwitchStatement(El,AContext);
      exit;
      end;
    end;

  // convert to if statements
  StList:=TJSStatementList(CreateElement(TJSStatementList,El));
  ok:=false;
  try
    // create var $tmp=CaseExpr;
    TmpVarName:=AContext.CreateTmpIdentifier('$tmp');
    VarSt:=TJSVariableStatement(CreateElement(TJSVariableStatement,El.CaseExpr));
    StList.A:=VarSt;
    VarDecl:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El.CaseExpr));
    VarSt.A:=VarDecl;
    VarDecl.Name:=TmpVarName;
    VarDecl.Init:=ConvertExpression(El.CaseExpr,AContext);

    LastIfSt:=nil;
    for i:=0 to El.Elements.Count-1 do
      begin
      SubEl:=TPasImplElement(El.Elements[i]);
      if SubEl is TPasImplCaseStatement then
        begin
        St:=TPasImplCaseStatement(SubEl);
        // create for example "if (tmp==expr) || ((tmp>=expr) && (tmp<=expr)){}"
        IfSt:=TJSIfStatement(CreateElement(TJSIfStatement,SubEl));
        if LastIfSt=nil then
          StList.B:=IfSt
        else
          LastIfSt.BFalse:=IfSt;
        LastIfSt:=IfSt;

        for j:=0 to St.Expressions.Count-1 do
          begin
          Expr:=TPasExpr(St.Expressions[j]);
          if (Expr is TBinaryExpr) and (TBinaryExpr(Expr).Kind=pekRange) then
            begin
            // range -> create "(tmp>=left) && (tmp<=right)"
            // create "() && ()"
            JSAndExpr:=TJSLogicalAndExpression(CreateElement(TJSLogicalAndExpression,Expr));
            JSExpr:=JSAndExpr;
            // create "tmp>=left"
            JSGEExpr:=TJSRelationalExpressionGE(CreateElement(TJSRelationalExpressionGE,Expr));
            JSAndExpr.A:=JSGEExpr;
            JSGEExpr.A:=CreateIdentifierExpr(TmpVarName,El.CaseExpr,AContext);
            JSGEExpr.B:=ConvertExpression(TBinaryExpr(Expr).left,AContext);
            // create "tmp<=right"
            JSLEExpr:=TJSRelationalExpressionLE(CreateElement(TJSRelationalExpressionLE,Expr));
            JSAndExpr.B:=JSLEExpr;
            JSLEExpr.A:=CreateIdentifierExpr(TmpVarName,El.CaseExpr,AContext);
            JSLEExpr.B:=ConvertExpression(TBinaryExpr(Expr).right,AContext);
            end
          else
            begin
            // value -> create (tmp==Expr)
            JSEQExpr:=TJSEqualityExpressionEQ(CreateElement(TJSEqualityExpressionEQ,Expr));
            JSExpr:=JSEQExpr;
            JSEQExpr.A:=CreateIdentifierExpr(TmpVarName,El.CaseExpr,AContext);
            JSEQExpr.B:=ConvertExpression(Expr,AContext);
            end;
          if IfSt.Cond=nil then
            // first expression
            IfSt.Cond:=JSExpr
          else
            begin
            // multi expression -> append with OR
            JSOrExpr:=TJSLogicalOrExpression(CreateElement(TJSLogicalOrExpression,St));
            JSOrExpr.A:=IfSt.Cond;
            JSOrExpr.B:=JSExpr;
            IfSt.Cond:=JSOrExpr;
            end;
          end;
        // convert statement
        if St.Body<>nil then
          IfSt.BTrue:=ConvertElement(St.Body,AContext)
        else
          IfSt.BTrue:=TJSEmptyStatement(CreateElement(TJSEmptyStatement,St));
        end
      else if SubEl is TPasImplCaseElse then
        begin
        // Pascal 'else' or 'otherwise' -> create JS "else{}"
        if LastIfSt=nil then
          RaiseNotSupported(SubEl,AContext,20161128120802,'case-of needs at least one case');
        LastIfSt.BFalse:=ConvertImplBlockElements(El.ElseBranch,AContext);
        end
      else
        RaiseNotSupported(SubEl,AContext,20161128113055);
      end;

    ok:=true;
  finally
    if not ok then
      StList.Free;
  end;
  Result:=StList;
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

procedure TPasToJSConverter.CreateImplementationSection(El: TPasModule;
  Src: TJSSourceElements; AContext: TConvertContext);
var
  AssignSt: TJSSimpleAssignStatement;
  Section: TImplementationSection;
begin
  if not Assigned(El.ImplementationSection) then
    exit;
  Section:=El.ImplementationSection;
  // add implementation section
  if ImplementationName<>'' then
    begin
    // add separat implementation object

    // add 'this.$impl = $impl;'
    AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    AddToSourceElements(Src,AssignSt);
    AssignSt.LHS:=CreateBuiltInIdentifierExpr('this.'+String(ImplementationName));
    AssignSt.Expr:=CreateBuiltInIdentifierExpr(String(ImplementationName));

    AddToSourceElements(Src,ConvertDeclarations(Section,AContext));
    end
  else
    begin
    // merge interface and implementation
    AddToSourceElements(Src,ConvertDeclarations(Section,AContext));
    end;
end;

procedure TPasToJSConverter.CreateInitSection(El: TPasModule;
  Src: TJSSourceElements; AContext: TConvertContext);
begin
  // add initialization section
  if Assigned(El.InitializationSection) then
    AddToSourceElements(Src,ConvertInitializationSection(El.InitializationSection,AContext));
  // finalization: not supported
  if Assigned(El.FinalizationSection) then
    raise Exception.Create('TPasToJSConverter.ConvertInitializationSection: finalization section is not supported');
end;

function TPasToJSConverter.ConvertImplBlock(El: TPasImplBlock;
  AContext: TConvertContext): TJSElement;

begin
  Result:=Nil;
  if (El is TPasImplStatement) then
    Result:=ConvertStatement(TPasImplStatement(El),AContext)
  else if (El.ClassType=TPasImplIfElse) then
    Result:=ConvertIfStatement(TPasImplIfElse(El),AContext)
  else if (El.ClassType=TPasImplRepeatUntil) then
    Result:=ConvertRepeatStatement(TPasImplRepeatUntil(El),AContext)
  else if (El.ClassType=TPasImplBeginBlock) then
    Result:=ConvertBeginEndStatement(TPasImplBeginBlock(El),AContext)
  else if (El.ClassType=TInitializationSection) then
    Result:=ConvertInitializationSection(TInitializationSection(El),AContext)
  else if (El.ClassType=TFinalizationSection) then
    Result:=ConvertFinalizationSection(TFinalizationSection(El),AContext)
  else if (El.ClassType=TPasImplTry) then
    Result:=ConvertTryStatement(TPasImplTry(El),AContext)
  else if (El.ClassType=TPasImplCaseOf) then
    Result:=ConvertCaseOfStatement(TPasImplCaseOf(El),AContext)
  else
    RaiseNotSupported(El,AContext,20161024192156);
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
  RaiseNotSupported(El,AContext,20161024192555);
  Result:=Nil;
  // ToDo TPasPackage = class(TPasElement)
end;

function TPasToJSConverter.ConvertResString(El: TPasResString;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192604);
  Result:=Nil;
  // ToDo: TPasResString
end;

function TPasToJSConverter.ConvertArgument(El: TPasArgument;
  AContext: TConvertContext): TJSElement;

begin
  // is this still needed?
  RaiseNotSupported(El,AContext,20161024192607);
  Result:=Nil;
  // ToDo: TPasArgument
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

function TPasToJSConverter.ConvertProperty(El: TPasProperty;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192643);
  Result:=Nil;
  // ToDo: TPasProperty = class(TPasVariable)
end;

function TPasToJSConverter.ConvertExportSymbol(El: TPasExportSymbol;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192650);
  Result:=Nil;
  // ToDo: TPasExportSymbol
end;

function TPasToJSConverter.ConvertLabels(El: TPasLabels;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192701);
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
    E:=CreateBuiltInIdentifierExpr(GetExceptionObjectName(AContext));
  T:=TJSThrowStatement(CreateElement(TJSThrowStatement,El));
  T.A:=E;
  Result:=T;
end;

function TPasToJSConverter.ConvertAssignStatement(El: TPasImplAssign;
  AContext: TConvertContext): TJSElement;

Var
  LHS,RHS: TJSElement;
  T: TJSAssignStatement;
  ok: Boolean;

begin
  AContext.IsWrite:=true;
  LHS:=ConvertElement(El.left,AContext);
  AContext.IsWrite:=false;
  ok:=false;
  try
    RHS:=ConvertElement(El.right,AContext);
    ok:=true;
  finally
    if not ok then
      FreeAndNil(LHS);
  end;
  case El.Kind of
    akDefault: T:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
    akAdd: T:=TJSAddEqAssignStatement(CreateElement(TJSAddEqAssignStatement,El));
    akMinus: T:=TJSSubEqAssignStatement(CreateElement(TJSSubEqAssignStatement,El));
    akMul: T:=TJSMulEqAssignStatement(CreateElement(TJSMulEqAssignStatement,El));
    akDivision: T:=TJSDivEqAssignStatement(CreateElement(TJSDivEqAssignStatement,El));
    else RaiseNotSupported(El,AContext,20161107221807);
  end;
  T.Expr:=RHS;
  T.LHS:=LHS;
  Result:=T;
end;

function TPasToJSConverter.ConvertCommand(El: TPasImplCommand;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192705);
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
//   var $loopend=<EndExpr>;
//   for(LoopVar=<StartExpr>; LoopVar<=$loopend; LoopVar++){}
//   if(LoopVar>$loopend)LoopVar--; // this line is only added if LoopVar is read later
//
// The StartExpr must be executed exactly once at beginning.
// The EndExpr must be executed exactly once at beginning.
// LoopVar can be a varname or programname.varname

Var
  ForSt : TJSForStatement;
  List, ListEnd: TJSStatementList;
  SimpleAss : TJSSimpleAssignStatement;
  VarDecl : TJSVarDeclaration;
  Incr, Decr : TJSUNaryExpression;
  BinExp : TJSBinaryExpression;
  VarStat: TJSVariableStatement;
  IfSt: TJSIfStatement;
  GTExpr: TJSRelationalExpression;
  CurLoopEndVarName: String;
  FuncContext: TConvertContext;

  function NeedDecrAfterLoop: boolean;
  var
    ResolvedVar: TPasResolverResult;
    aParent: TPasElement;
    ProcBody: TProcedureBody;
    FindData: TForLoopFindData;
  begin
    Result:=true;
    if AContext.Resolver=nil then exit(false);
    AContext.Resolver.ComputeElement(El.VariableName,ResolvedVar,[]);
    if ResolvedVar.IdentEl=nil then
      exit;
    if ResolvedVar.IdentEl.Parent is TProcedureBody then
      begin
      // loopvar is a local var
      ProcBody:=TProcedureBody(ResolvedVar.IdentEl.Parent);
      aParent:=El;
      while true do
        begin
        aParent:=aParent.Parent;
        if aParent=nil then exit;
        if aParent is TProcedureBody then
          begin
          if aParent<>ProcBody then exit;
          break;
          end;
        end;
      // loopvar is a local var of the same function as where the loop is
      // -> check if it is read after the loop
      FindData:=Default(TForLoopFindData);
      FindData.ForLoop:=El;
      FindData.LoopVar:=ResolvedVar.IdentEl;
      ProcBody.Body.ForEachCall(@ForLoop_OnProcBodyElement,@FindData);
      if not FindData.LoopVarRead then
        exit(false);
      end;
  end;

begin
  Result:=Nil;
  BinExp:=Nil;
  // get function context
  FuncContext:=AContext;
  while (FuncContext.Parent<>nil) and (not (FuncContext is TFunctionContext)) do
    FuncContext:=FuncContext.Parent;
  // create unique loopend var name
  CurLoopEndVarName:=FuncContext.CreateTmpIdentifier(String(LoopEndVarName));

  // loopvar:=
  // for (statementlist...
  List:=TJSStatementList(CreateElement(TJSStatementList,El));
  ListEnd:=List;
  try
    // add "var $loopend=<EndExpr>"
    VarStat:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
    List.A:=VarStat;
    VarDecl:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
    VarStat.A:=VarDecl;
    VarDecl.Name:=CurLoopEndVarName;
    VarDecl.Init:=ConvertElement(El.EndExpr,AContext);
    // add "for()"
    ForSt:=TJSForStatement(CreateElement(TJSForStatement,El));
    List.B:=ForSt;
    // add "LoopVar=<StartExpr>;"
    SimpleAss:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El.StartExpr));
    ForSt.Init:=SimpleAss;
    AContext.IsWrite:=true;
    SimpleAss.LHS:=ConvertElement(El.VariableName,AContext);
    AContext.IsWrite:=false;
    SimpleAss.Expr:=ConvertElement(El.StartExpr,AContext);
    // add "LoopVar<=$loopend"
    if El.Down then
      BinExp:=TJSRelationalExpressionGE(CreateElement(TJSRelationalExpressionGE,El.EndExpr))
    else
      BinExp:=TJSRelationalExpressionLE(CreateElement(TJSRelationalExpressionLE,El.EndExpr));
    ForSt.Cond:=BinExp;
    BinExp.A:=ConvertElement(El.VariableName,AContext);
    BinExp.B:=CreateIdentifierExpr(CurLoopEndVarName,El.EndExpr,AContext);
    // add "LoopVar++"
    if El.Down then
      Incr:=TJSUnaryPostMinusMinusExpression(CreateElement(TJSUnaryPostMinusMinusExpression,El))
    else
      Incr:=TJSUnaryPostPlusPlusExpression(CreateElement(TJSUnaryPostPlusPlusExpression,El));
    ForSt.Incr:=Incr;
    Incr.A:=ConvertElement(El.VariableName,AContext);
    // add body
    if El.Body<>nil then
      ForSt.Body:=ConvertElement(El.Body,AContext);

    if NeedDecrAfterLoop then
      begin
      // add "if(LoopVar>$loopend)LoopVar--;"
      // add "if()"
      IfSt:=TJSIfStatement(CreateElement(TJSIfStatement,El));
      AddToStatementList(List,ListEnd,IfSt,El);
      // add "LoopVar>$loopend"
      if El.Down then
        GTExpr:=TJSRelationalExpressionLT(CreateElement(TJSRelationalExpressionLT,El))
      else
        GTExpr:=TJSRelationalExpressionGT(CreateElement(TJSRelationalExpressionGT,El));
      IfSt.Cond:=GTExpr;
      GTExpr.A:=ConvertElement(El.VariableName,AContext);
      GTExpr.B:=CreateIdentifierExpr(CurLoopEndVarName,El.EndExpr,AContext);
      // add "LoopVar--"
      if El.Down then
        Decr:=TJSUnaryPostPlusPlusExpression(CreateElement(TJSUnaryPostPlusPlusExpression,El))
      else
        Decr:=TJSUnaryPostMinusMinusExpression(CreateElement(TJSUnaryPostMinusMinusExpression,El));
      IfSt.BTrue:=Decr;
      Decr.A:=ConvertElement(El.VariableName,AContext);
      end;
    Result:=List;
  finally
    if Result=nil then
      List.Free;
  end;
end;

function TPasToJSConverter.ConvertSimpleStatement(El: TPasImplSimple;
  AContext: TConvertContext): TJSElement;

Var
  E : TJSElement;

begin
  E:=ConvertElement(EL.Expr,AContext);
  if E=nil then
    exit(nil); // e.g. "inherited;" without ancestor proc
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

procedure TPasToJSConverter.RaiseInconsistency(Id: int64);
begin
  raise Exception.Create('TPasToJSConverter.RaiseInconsistency['+IntToStr(Id)+']: you found a bug');
end;

{$IFDEF EnableOldClass}
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
{$ENDIF}

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
    DoError(20161024192715,'internal error: member expression with less than two members');
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
      anUnitName := TransformVariableName(TPasModule(El),AContext);
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
      while Last.B is TJSStatementList do
        Last:=TJSStatementList(Last.B);
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
  T:=AContext.Resolver.ResolveAliasType(PasType);
  if (T is TPasArrayType) then
    begin
    Result:=TJSArrayLiteral(CreateElement(TJSArrayLiteral,PasType));
    If Assigned(Expr) then
      DoError(20161024192739,nInitializedArraysNotSupported,sInitializedArraysNotSupported,[],PasType);
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
    if T=nil then
      Lit.Value.IsUndefined:=true
    else if (T.ClassType=TPasPointerType) or (T.ClassType=TPasClassType) then
      Lit.Value.IsNull:=true
    else if T.ClassType=TPasStringType then
      Lit.Value.AsString:=''
    else if T.ClassType=TPasUnresolvedSymbolRef then
      begin
      // ToDo: check resolver type
      if (CompareText(T.Name,'longint')=0)
      or (CompareText(T.Name,'int64')=0)
      or (CompareText(T.Name,'real')=0)
      or (CompareText(T.Name,'double')=0)
      or (CompareText(T.Name,'single')=0)
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
    RaiseNotSupported(Expr,AContext,20161024192747);
  NewMemE:=TJSNewMemberExpression(CreateElement(TJSNewMemberExpression,El));
  Result:=NewMemE;
  NewMemE.MExpr:=CreateTypeRef(aRecord,AContext);
end;

function TPasToJSConverter.CreateTypeRef(El: TPasType; AContext: TConvertContext
  ): TJSElement;
var
  Name: String;
begin
  {$IFDEF VerbosePas2JS}
  writeln('TPasToJSConverter.CreateTypeRef El="',GetObjName(El),'" El.Parent=',GetObjName(El.Parent));
  {$ENDIF}
  Name:=CreateReferencePath(El,AContext,rpkPathAndName);
  Result:=CreateBuiltInIdentifierExpr(Name);
end;

function TPasToJSConverter.CreateReferencePath(El: TPasElement;
  AContext: TConvertContext; Kind: TRefPathKind; Full: boolean): string;
{ Notes:
 - local var, even higher lvl does not need a reference path
 - 'this:
   - in interface function (even nested) 'this' is the interface,
   - in implementation function (even nested) 'this' is the implementation,
   - in initialization 'this' is interface
   - in method body 'this' is the instance
   - in class method body 'this' is the class
 otherwise use absolute path
}

  function IsLocalVar: boolean;
  begin
    Result:=false;
    if El.ClassType=TPasArgument then
      exit(true);
    if El.ClassType=TPasResultElement then
      exit(true);
    if AContext.Resolver=nil then
      exit(true);
    if El.Parent=nil then
      RaiseNotSupported(El,AContext,20170203121306,GetObjName(El));
    if El.Parent.ClassType=TPasImplExceptOn then
      exit(true);
    if not (El.Parent is TProcedureBody) then exit;
    // ToDo: local const are stored in interface
    if El is TPasConst then
      RaiseNotSupported(El,AContext,20170201164310);
    Result:=true;
  end;

  procedure Prepend(var aPath: string; Prefix: string);
  begin
    if aPath<>'' then
      aPath:='.'+aPath;
    aPath:=Prefix+aPath;
  end;

  function IsClassFunction(Proc: TPasElement): boolean;
  begin
    if Proc=nil then exit(false);
    Result:=(Proc.ClassType=TPasClassFunction) or (Proc.ClassType=TPasClassProcedure)
      or (Proc.ClassType=TPasClassConstructor) or (Proc.ClassType=TPasClassDestructor);
  end;

var
  FoundModule: TPasModule;
  This, ParentEl: TPasElement;
  Dot: TDotContext;
  ThisContext: TFunctionContext;
begin
  Result:='';
  //writeln('TPasToJSConverter.CreateReferencePath START El=',GetObjName(El),' Parent=',GetObjName(El.Parent),' Context=',GetObjName(AContext));

  if AContext is TDotContext then
    begin
    Dot:=TDotContext(AContext);
    if Dot.Resolver<>nil then
      begin
      if El is TPasVariable then
        begin
        //writeln('TPasToJSConverter.CreateReferencePath Left=',GetResolverResultDesc(Dot.LeftResolved),' Right=class var ',GetObjName(El));
        if (VarModifiersType*TPasVariable(El).VarModifiers<>[])
            and Dot.IsWrite
            and Dot.Resolver.ResolvedElIsClassInstance(Dot.LeftResolved) then
          begin
          // writing a class var
          Result:='$class';
          end;
        end
      else if IsClassFunction(El) then
        begin
        if Dot.Resolver.ResolvedElIsClassInstance(Dot.LeftResolved) then
          // accessing a class method from an object
          Result:='$class';
        end;
      end;
    end
  else if IsLocalVar then
    begin
    // El is local var -> does not need path
    end
  else
    begin
    // need full path
    if El.Parent=nil then
      RaiseNotSupported(El,AContext,20170201172141,GetObjName(El));
    ThisContext:=AContext.GetThisContext;
    if ThisContext<>nil then
      This:=ThisContext.GetThis
    else
      This:=nil;
    ParentEl:=El.Parent;
    while ParentEl<>nil do
      begin
      if ParentEl.ClassType=TImplementationSection then
        begin
        // element is in an implementation section
        if ParentEl=This then
          Prepend(Result,'this')
        else
          begin
          FoundModule:=El.GetModule;
          if FoundModule=nil then
            RaiseInconsistency(20161024192755);
          if AContext.GetRootModule=FoundModule then
            // in same unit -> use '$impl'
            Prepend(Result,String(ImplementationName))
          else
            // in other unit -> use pas.unitname.$impl
            Prepend(Result,String(ModulesVarName)
               +'.'+TransformModuleName(FoundModule,AContext)
               +'.'+String(ImplementationName));
          end;
        break;
        end
      else if ParentEl is TPasModule then
        begin
        // element is in an unit interface or program/library section
        if ParentEl=This then
          Prepend(Result,'this')
        else
          Prepend(Result,String(ModulesVarName)
            +'.'+TransformModuleName(TPasModule(ParentEl),AContext));
        break;
        end
      else if (ParentEl.ClassType=TPasClassType)
          or (ParentEl.ClassType=TPasRecordType) then
        begin
        // element is a class or record
        if Full then
          Prepend(Result,ParentEl.Name)
        else
          begin
          // Pascal and JS have similar scoping rules, so we can use 'this'.
          Result:='this';
          if (ThisContext<>nil) and (not IsClassFunction(ThisContext.PasElement)) then
            begin
            // 'this' is an class instance
            if El is TPasVariable then
              begin
              writeln('AAA1 TPasToJSConverter.CreateReferencePath class var ',GetObjName(El),' This=',GetObjName(This));
              if (VarModifiersType*TPasVariable(El).VarModifiers<>[])
                  and AContext.IsWrite then
                begin
                  Result:=Result+'.$class'; // writing a class var
                end;
              end
            else if IsClassFunction(El) then
              Result:=Result+'.$class'; // accessing a class function
            end;
          break;
          end;
        end;
      ParentEl:=ParentEl.Parent;
      end;
    end;
  if (Result<>'') and (Kind in [rpkPathWithDot,rpkPathAndName]) then
    Result:=Result+'.';
  if Kind=rpkPathAndName then
    Result:=Result+TransformVariableName(El,AContext);
end;

{$IFDEF EnableOldClass}
function TPasToJSConverter.CreateProcedureDeclaration(const El: TPasElement
  ): TJSFunctionDeclarationStatement;
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
{$ENDIF}

procedure TPasToJSConverter.CreateProcedureCall(var Call: TJSCallExpression;
  Args: TParamsExpr; TargetProc: TPasProcedure; AContext: TConvertContext);
// create a call, adding call by reference and default values
begin
  if Call=nil then
    Call:=TJSCallExpression(CreateElement(TJSCallExpression,Args));
  if ((Args=nil) or (length(Args.Params)=0))
      and ((TargetProc=nil) or (TargetProc.ProcType.Args.Count=0)) then
    exit;
  if Call.Args=nil then
    Call.Args:=TJSArguments(CreateElement(TJSArguments,Args));
  CreateProcedureCallArgs(Call.Args.Elements,Args,TargetProc,AContext);
end;

procedure TPasToJSConverter.CreateProcedureCallArgs(
  Elements: TJSArrayLiteralElements; Args: TParamsExpr;
  TargetProc: TPasProcedure; AContext: TConvertContext);
// Add call arguments. Handle call by reference and default values
var
  ArgContext: TConvertContext;
  i: Integer;
  Arg: TJSElement;
  TargetArgs: TFPList;
  TargetArg: TPasArgument;
  OldIsWrite: Boolean;
begin
  // get context
  ArgContext:=AContext;
  while ArgContext is TDotContext do
    ArgContext:=ArgContext.Parent;
  i:=0;
  OldIsWrite:=ArgContext.IsWrite;
  if TargetProc<>nil then
    TargetArgs:=TargetProc.ProcType.Args
  else
    TargetArgs:=nil;
  // add params
  if Args<>nil then
    while i<length(Args.Params) do
      begin
      if TargetArgs<>nil then
        begin
        TargetArg:=TPasArgument(TargetArgs[i]);
        AContext.IsWrite:=TargetArg.Access in [argVar, argOut];
        end
      else
        AContext.IsWrite:=false;
      Arg:=ConvertElement(Args.Params[i],ArgContext);
      // ToDo: var/out params
      Elements.AddElement.Expr:=Arg;
      inc(i);
      end;
  // fill up default values
  if TargetProc<>nil then
    begin
    while i<TargetArgs.Count do
      begin
      TargetArg:=TPasArgument(TargetArgs[i]);
      if TargetArg.ValueExpr=nil then
        begin
        {$IFDEF VerbosePas2JS}
        writeln('TPasToJSConverter.CreateProcedureCallArgs missing default value: TargetProc=',TargetProc.Name,' i=',i);
        {$ENDIF}
        RaiseNotSupported(Args,AContext,20170201193601);
        end;
      AContext.IsWrite:=false;
      Arg:=ConvertElement(TargetArg.ValueExpr,ArgContext);
      Elements.AddElement.Expr:=Arg;
      inc(i);
      end;
    end;
  ArgContext.IsWrite:=OldIsWrite;
end;

function TPasToJSConverter.ConvertExceptOn(El: TPasImplExceptOn;
  AContext: TConvertContext): TJSElement;
// convert "on T do ;" to "if(T.isPrototypeOf(exceptObject)){}"
// convert "on E:T do ;" to "if(T.isPrototypeOf(exceptObject)){ var E=exceptObject; }"
Var
  IfSt : TJSIfStatement;
  ListFirst , ListLast: TJSStatementList;
  DotExpr: TJSDotMemberExpression;
  Call: TJSCallExpression;
  V: TJSVariableStatement;
  VarDecl: TJSVarDeclaration;

begin
  Result:=nil;
  // create "if()"
  IfSt:=TJSIfStatement(CreateElement(TJSIfStatement,El));
  try
    // create "T.isPrototypeOf"
    DotExpr:=TJSDotMemberExpression(CreateElement(TJSDotMemberExpression,El));
    DotExpr.MExpr:=CreateBuiltInIdentifierExpr(CreateReferencePath(El.TypeEl,AContext,rpkPathAndName));
    DotExpr.Name:='isPrototypeOf';
    // create "T.isPrototypeOf(exceptObject)"
    Call:=TJSCallExpression(CreateElement(TJSCallExpression,El));
    Call.Expr:=DotExpr;
    Call.Args:=TJSArguments(CreateElement(TJSArguments,El));
    Call.Args.Elements.AddElement.Expr:=CreateBuiltInIdentifierExpr(GetExceptionObjectName(AContext));
    IfSt.Cond:=Call;

    if El.VarEl<>nil then
      begin
      // add "var E=exceptObject;"
      ListFirst:=TJSStatementList(CreateElement(TJSStatementList,El.Body));
      ListLast:=ListFirst;
      IfSt.BTrue:=ListFirst;
      V:=TJSVariableStatement(CreateElement(TJSVariableStatement,El));
      ListFirst.A:=V;
      VarDecl:=TJSVarDeclaration(CreateElement(TJSVarDeclaration,El));
      V.A:=VarDecl;
      VarDecl.Name:=TransformVariableName(El,El.VariableName,AContext);
      VarDecl.Init:=CreateBuiltInIdentifierExpr(GetExceptionObjectName(AContext));
      // add statements
      AddToStatementList(ListFirst,ListLast,ConvertElement(El.Body,AContext),El);
      end
    else
      // add statements
      IfSt.BTrue:=ConvertElement(El.Body,AContext);

    Result:=IfSt;
  finally
    if Result=nil then
      IfSt.Free;
  end;
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
    RaiseNotSupported(El,AContext,20161024192759);
{
  TPasImplCaseStatement = class(TPasImplStatement)
}
end;

function TPasToJSConverter.ConvertCommands(El: TPasImplCommands;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192806);
  Result:=Nil;
  // ToDo: TPasImplCommands = class(TPasImplElement)
end;

function TPasToJSConverter.ConvertConst(El: TPasConst; AContext: TConvertContext
  ): TJSElement;
begin
  Result:=nil;
  RaiseNotSupported(El,AContext,20161024193129);
end;

function TPasToJSConverter.ConvertLabelMark(El: TPasImplLabelMark;
  AContext: TConvertContext): TJSElement;

begin
  RaiseNotSupported(El,AContext,20161024192857);
  Result:=Nil;
  // ToDo:   TPasImplLabelMark = class(TPasImplLabelMark) then
end;

function TPasToJSConverter.ConvertElement(El: TPasElement;
  AContext: TConvertContext): TJSElement;
var
  C: TClass;
begin
  {$IFDEF VerbosePas2JS}
  writeln('TPasToJSConverter.ConvertElement El=',GetObjName(El),' Context=',GetObjName(AContext));
  {$ENDIF}
  if El=nil then
    begin
    Result:=nil;
    RaiseInconsistency(20161024190203);
    end;
  C:=El.ClassType;
  If (C=TPasPackage)  then
    Result:=ConvertPackage(TPasPackage(El),AContext)
  else if (C=TPasResString) then
    Result:=ConvertResString(TPasResString(El),AContext)
  else if (C=TPasArgument) then
    Result:=ConvertArgument(TPasArgument(El),AContext)
  else if (C=TPasConst) then
    Result:=ConvertConst(TPasConst(El),AContext)
  else if (C=TPasProperty) then
    Result:=ConvertProperty(TPasProperty(El),AContext)
  else if (C=TPasVariable) then
    Result:=ConvertVariable(TPasVariable(El),AContext)
  else if (C=TPasExportSymbol) then
    Result:=ConvertExportSymbol(TPasExportSymbol(El),AContext)
  else if (C=TPasLabels) then
    Result:=ConvertLabels(TPasLabels(El),AContext)
  else if (C=TPasImplCommand) then
    Result:=ConvertCommand(TPasImplCommand(El),AContext)
  else if (C=TPasImplCommands) then
    Result:=ConvertCommands(TPasImplCommands(El),AContext)
  else if (C=TPasImplLabelMark) then
    Result:=ConvertLabelMark(TPasImplLabelMark(El),AContext)
  else if C.InheritsFrom(TPasExpr) then
    Result:=ConvertExpression(TPasExpr(El),AContext)
  else if C.InheritsFrom(TPasDeclarations) then
    Result:=ConvertDeclarations(TPasDeclarations(El),AContext)
  else if C.InheritsFrom(TPasType) then
    Result:=ConvertType(TPasType(El),AContext)
  else if C.InheritsFrom(TPasProcedure) then
    Result:=ConvertProcedure(TPasProcedure(El),AContext)
  else if C.InheritsFrom(TPasImplBlock) then
    Result:=ConvertImplBlock(TPasImplBlock(El),AContext)
  else if C.InheritsFrom(TPasModule)  then
    Result:=ConvertModule(TPasModule(El),AContext)
  else
    begin
    Result:=nil;
    RaiseNotSupported(El, AContext, 20161024190449);
    end;
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
                 this.i=0;
                 this.s="";
                 this.d=0.0;
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
  FuncContext: TFunctionContext;
  Obj: TJSObjectLiteral;
  ObjLit: TJSObjectLiteralElement;
begin
  ok:=false;
  FuncContext:=nil;
  AssignSt:=nil;
  try
    FDS:=TJSFunctionDeclarationStatement(CreateElement(TJSFunctionDeclarationStatement,El));
    if AContext is TObjectContext then
      begin
      // add 'TypeName: function(){}'
      Obj:=TObjectContext(AContext).JSElement as TJSObjectLiteral;
      ObjLit:=Obj.Elements.AddElement;
      ObjLit.Name:=TJSString(TransformVariableName(El,AContext));
      ObjLit.Expr:=FDS;
      end
    else
      begin
      // add 'this.TypeName = function(){}'
      AssignSt:=TJSSimpleAssignStatement(CreateElement(TJSSimpleAssignStatement,El));
      AssignSt.LHS:=CreateSubNameExpression(El,El.Name,AContext);
      AssignSt.Expr:=FDS;
      end;
    FD:=TJSFuncDef.Create;
    FDS.AFunction:=FD;
    // add variables
    FD.Body:=TJSFunctionBody(CreateElement(TJSFunctionBody,El));
    FuncContext:=TFunctionContext.Create(El,FD.Body,AContext);
    FuncContext.This:=El;
    FuncContext.IsSingleton:=true;
    First:=nil;
    Last:=nil;
    for i:=0 to El.Members.Count-1 do
      begin
      PasVar:=TPasVariable(El.Members[i]);
      JSVar:=CreateVarDecl(PasVar,FuncContext);
      AddToStatementList(First,Last,JSVar,PasVar);
      FD.Body.A:=First;
      end;
    ok:=true;
  finally
    FuncContext.Free;
    if not ok then AssignSt.Free;
  end;
  Result:=AssignSt;
end;

procedure TPasToJSConverter.DoError(Id: int64; const Msg: String);
var
  E: EPas2JS;
begin
  E:=EPas2JS.Create(Msg);
  E.Id:=Id;
  Raise E;
end;

procedure TPasToJSConverter.DoError(Id: int64; const Msg: String;
  const Args: array of const);
var
  E: EPas2JS;
begin
  E:=EPas2JS.CreateFmt(Msg,Args);
  E.Id:=Id;
  Raise E;
end;

procedure TPasToJSConverter.DoError(Id: int64; MsgNumber: integer;
  const MsgPattern: string; const Args: array of const; El: TPasElement);
var
  E: EPas2JS;
begin
  E:=EPas2JS.CreateFmt(MsgPattern,Args);
  E.PasElement:=El;
  E.MsgNumber:=MsgNumber;
  E.Id:=Id;
  CreateMsgArgs(E.Args,Args);
  raise E;
end;

procedure TPasToJSConverter.RaiseNotSupported(El: TPasElement;
  AContext: TConvertContext; Id: int64; const Msg: string);
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
  E.Id:=Id;
  raise E;
end;

procedure TPasToJSConverter.RaiseIdentifierNotFound(Identifier: string;
  El: TPasElement; Id: int64);
var
  E: EPas2JS;
begin
  E:=EPas2JS.CreateFmt(sIdentifierNotFound,[Identifier]);
  E.PasElement:=El;
  E.MsgNumber:=nIdentifierNotFound;
  SetLength(E.Args,1);
  E.Args[0]:=Identifier;
  E.Id:=Id;
  raise E;
end;

function TPasToJSConverter.TransformVariableName(El: TPasElement;
  const AName: String; AContext: TConvertContext): String;
var
  i: Integer;
  c: Char;
begin
  if AContext=nil then ;
  if Pos('.',AName)>0 then
    RaiseInconsistency(20170203164711);
  if UseLowerCase then
    Result:=LowerCase(AName)
  else
    Result:=AName;
  if IsPreservedWord(Result) then
    begin
    for i:=1 to length(Result) do
      begin
      c:=Result[i];
      case c of
      'a'..'z','A'..'Z':
        begin
        Result[i]:=chr(ord(c) xor 32);
        break;
        end;
      end;
      end;
    if IsPreservedWord(Result) then
      RaiseNotSupported(El,AContext,20170203131832);
    end;
end;

function TPasToJSConverter.TransformVariableName(El: TPasElement;
  AContext: TConvertContext): String;
begin
  Result:=TransformVariableName(El,El.Name,AContext);
end;

function TPasToJSConverter.TransformModuleName(El: TPasModule;
  AContext: TConvertContext): String;
begin
  if El is TPasProgram then
    Result:='program'
  else
    Result:=TransformVariableName(El,AContext);
end;

function TPasToJSConverter.IsPreservedWord(aName: string): boolean;
var
  l, r, m, cmp: Integer;
begin
  Result:=true;
  if aName=String(ModulesVarName) then exit;
  if aName=String(RTLVarName) then exit;
  if aName=GetExceptionObjectName(nil) then exit;

  l:=low(JSReservedWords);
  r:=high(JSReservedWords);
  while l<=r do
    begin
    m:=(l+r) div 2;
    cmp:=CompareStr(aName,JSReservedWords[m]);
    //writeln('TPasToJSConverter.IsPreservedWord Name="',aName,'" l=',l,' r=',r,' m=',m,' JSReservedWords[m]=',JSReservedWords[m],' cmp=',cmp);
    if cmp>0 then
      l:=m+1
    else if cmp<0 then
      r:=m-1
    else
      exit;
    end;
  Result:=false;
end;

function TPasToJSConverter.ConvertPasElement(El: TPasElement;
  Resolver: TPasResolver): TJSElement;
var
  aContext: TRootContext;
begin
  aContext:=TRootContext.Create(El,nil,nil);
  try
    aContext.Resolver:=Resolver;
    Result:=ConvertElement(El,aContext);
  finally
    FreeAndNil(aContext);
  end;
end;

var
  i: integer;
initialization
  for i:=low(JSReservedWords) to High(JSReservedWords)-1 do
    if CompareStr(JSReservedWords[i],JSReservedWords[i+1])>=0 then
      raise Exception.Create('20170203135442 '+JSReservedWords[i]+' >= '+JSReservedWords[i+1]);

end.

