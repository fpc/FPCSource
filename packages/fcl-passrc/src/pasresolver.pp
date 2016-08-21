{
    This file is part of the Free Component Library

    Pascal source parser
    Copyright (c) 2000-2005 by
      Areca Systems GmbH / Sebastian Guenther, sg@freepascal.org

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Abstract:
  Resolves references by setting TPasElement.CustomData as TResolvedReference.
  Creates search scopes for elements with sub identifiers by setting
    TPasElement.CustomData as TPasScope: unit, program, library, interface,
    implementation, procs

 Works:
  - built-in types as TPasUnresolvedSymbolRef: longint, int64, string, pointer, ...
  - references in statements, error if not found
  - interface and implementation types, vars, const
  - params, local types, vars, const
  - nested procedures
  - search in used units
  - unitname.identifier
  - alias types, 'type a=b'
  - type alias type 'type a=type b'
  - choose the compatible overloaded procedure

 ToDo:
  - spot duplicates
  - check if types only refer types
  - check if constant is longint or int64
  - built-in functions
  - enums, propagate to parent scopes
  - records
  - arrays
  - pointer
  - untyped parameters
  - ranges
  - sets
  - forwards of ^pointer and class of - must be queued and resolved at end of type section
  - with
  - classes
  - interfaces
  - properties
    - read
    - write
    - index properties
  - default property
  - generics, nested param lists
  - visibility (private, protected, strict private, strict protected)
  - check const expression types, e.g. bark on "const c:string=3;"
  - dotted unitnames
  - helpers
  - generics
  - many more: search for "ToDo:"

 Debug flags: -d<x>
   VerbosePasResolver
}
unit PasResolver;

{$mode objfpc}{$H+}
{$inline on}

interface

uses
  Classes, SysUtils, contnrs, PasTree, PParser, PScanner;

const
  ParserMaxEmbeddedColumn = 2048;
  ParserMaxEmbeddedRow = $7fffffff div ParserMaxEmbeddedColumn;

// message numbers
const
  nIdentifierNotFound = 3001;
  nNotYetImplemented = 3002;
  nIllegalQualifier = 3003;
  nSyntaxErrorExpectedButFound = 3004;
  nWrongNumberOfParametersForCallTo = 3005;
  nIncompatibleTypeArgNo = 3006;
  nIncompatibleTypeArgNoVarParamMustMatchExactly = 3007;
  nVariableIdentifierExpected = 3008;

// resourcestring patterns of messages
resourcestring
  sIdentifierNotFound = 'identifier not found "%s"';
  sNotYetImplemented = 'not yet implemented: %s';
  sIllegalQualifier = 'illegal qualifier "%s"';
  sSyntaxErrorExpectedButFound = 'Syntax error, "%s" expected but "%s" found';
  sWrongNumberOfParametersForCallTo = 'Wrong number of parameters specified for call to "%s"';
  sIncompatibleTypeArgNo = 'Incompatible type arg no. %s: Got "%s", expected "%s"';
  sIncompatibleTypeArgNoVarParamMustMatchExactly = 'Incompatible type arg no. %s: Got "%s", expected "%s". Var param must match exactly.';
  sVariableIdentifierExpected = 'Variable identifier expected';

type
  TResolveBaseType = (
    btNone,        // undefined
    btContext,     // a TPasType
    btUntyped,     // TPasArgument without ArgType
    btChar,        // char
    btWideChar,    // widechar
    btString,      // string
    btAnsiString,  // ansistring
    btShortString, // shortstring
    btWideString,  // widestring
    btUnicodeString,// unicodestring
    btReal,        // real  platform, single or double
    btSingle,      // single  1.5E-45..3.4E38, digits 7-8, bytes 4
    btDouble,      // double  5.0E-324..1.7E308, digits 15-16, bytes 8
    btExtended,    // extended  platform, double or 1.9E-4932..1.1E4932, digits 19-20, bytes 10
    btCExtended,   // cextended
    btComp,        // comp  -2E64+1..2E63-1, digits 19-20, bytes 8
    btCurrency,    // currency  ?, bytes 8
    btBoolean,     // boolean
    btByteBool,    // bytebool  true=not zero
    btWordBool,    // wordbool  true=not zero
    btLongBool,    // longbool  true=not zero
    btQWordBool,   // qwordbool true=not zero
    btByte,        // byte  0..255
    btShortInt,    // shortint -128..127
    btWord,        // word  unsigned 2 bytes
    btSmallInt,    // smallint signed 2 bytes
    btLongWord,    // longword unsigned 4 bytes
    btCardinal,    // cardinal see longword
    btLongint,     // longint  signed 4 bytes
    btQWord,       // qword   0..18446744073709551615, bytes 8
    btInt64,       // int64   -9223372036854775808..9223372036854775807, bytes 8
    btPointer,     // pointer
    btFile,        // file
    btText,        // text
    btVariant,     // variant
    btNil,         // nil = pointer, class, procedure, method, ...
    btCompilerFunc// SUCC, PREC, LOW, HIGH, ORD, LENGTH, COPY
    );
  TResolveBaseTypes = set of TResolveBaseType;
const
  btAllNumbers = [btComp,btCurrency,btByte,btShortInt,btWord,btSmallInt,
    btLongWord,btCardinal,btLongint,btQWord,btInt64];
  btAllStrings = [btChar,btWideChar,btString,btAnsiString,btShortString,
    btWideString,btUnicodeString];
  btAllFloats = [btReal,btSingle,btDouble,btExtended,btCExtended];
  btAllBooleans = [btBoolean,btByteBool,btWordBool,btLongBool,btQWordBool];
  btAllStandardTypes = [
    btChar,
    btWideChar,
    btString,
    btAnsiString,
    btShortString,
    btWideString,
    btUnicodeString,
    btReal,
    btSingle,
    btDouble,
    btExtended,
    btCExtended,
    btComp,
    btCurrency,
    btBoolean,
    btByteBool,
    btWordBool,
    btLongBool,
    btQWordBool,
    btByte,
    btShortInt,
    btWord,
    btSmallInt,
    btLongWord,
    btCardinal,
    btLongint,
    btQWord,
    btInt64,
    btPointer,
    btFile,
    btText,
    btVariant
    ];

  BaseTypeNames: array[TResolveBaseType] of shortstring =(
    'None',
    'Context',
    'Untyped',
    'Char',
    'WideChar',
    'String',
    'AnsiString',
    'ShortString',
    'WideString',
    'UnicodeString',
    'Real',
    'Single',
    'Double',
    'Extended',
    'CExtended',
    'Comp',
    'Currency',
    'Boolean',
    'ByteBool',
    'WordBool',
    'LongBool',
    'QWordBool',
    'Byte',
    'ShortInt',
    'Word',
    'SmallInt',
    'LongWord',
    'Cardinal',
    'Longint',
    'QWord',
    'Int64',
    'Pointer',
    'File',
    'Text',
    'Variant',
    'Nil',
    'CompilerFunc'
    );

const
  ResolverResultVar = 'Result';

type

  { EPasResolve }

  EPasResolve = class(Exception)
  private
    FPasElement: TPasElement;
    procedure SetPasElement(AValue: TPasElement);
  public
    MsgNumber: integer;
    Args: TMessageArgs;
    destructor Destroy; override;
    property PasElement: TPasElement read FPasElement write SetPasElement;
  end;

  { TResolveData - base class for data stored in TPasElement.CustomData }

  TResolveData = Class
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    Owner: TObject; // e.g. a TPasResolver
    Next: TResolveData;
    CustomData: TObject;
    constructor Create; virtual;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;
  TResolveDataClass = class of TResolveData;

  { TResolvedReference - CustomData for normal references }

  TResolvedReference = Class(TResolveData)
  private
    FDeclaration: TPasElement;
    procedure SetDeclaration(AValue: TPasElement);
  public
    destructor Destroy; override;
    property Declaration: TPasElement read FDeclaration write SetDeclaration;
  end;

  { TResolvedCustom - CustomData for compiler built-in identifiers like 'length' }

  TResolvedCustom = Class(TResolveData)
  public
    //pas2js creates descendants of this
  end;

  TPasScope = class;

  TIterateScopeElement = procedure(El: TPasElement; Scope: TPasScope;
    Data: Pointer; var Abort: boolean) of object;

  { TPasScope - CustomData for elements with sub identifiers }

  TPasScope = Class(TResolveData)
  public
    class function IsStoredInElement: boolean; virtual;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure WriteIdentifiers(Prefix: string); virtual;
  end;
  TPasScopeClass = class of TPasScope;

  { TPasModuleScope }

  TPasModuleScope = class(TPasScope)
  public
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
  end;

  TPasIdentifierKind = (
    pikNone, // not yet initialized
    pikCustom, // built-in identifiers
    pikSimple, // simple vars, consts, types, enums
    pikProc // may need parameter list with round brackets
    {
    pikIndexedProperty, // may need parameter list with edged brackets
    pikGeneric, // may need parameter list with angle brackets
    pikDottedUses // namespace, needs dotted identifierss }
    );
  TPasIdentifierKinds = set of TPasIdentifierKind;

  { TPasIdentifier }

  TPasIdentifier = Class(TObject)
  private
    FElement: TPasElement;
    procedure SetElement(AValue: TPasElement);
  public
    Identifier: String;
    NextSameIdentifier: TPasIdentifier; // next identifier with same name
    Kind: TPasIdentifierKind;
    destructor Destroy; override;
    property Element: TPasElement read FElement write SetElement;
  end;

  { TPasIdentifierScope - elements with a list of sub identifiers }

  TPasIdentifierScope = Class(TPasScope)
  private
    FItems: TFPHashList;
    procedure InternalAdd(Item: TPasIdentifier);
    procedure OnClearItem(Item, Dummy: pointer);
    procedure OnWriteItem(Item, Dummy: pointer);
  public
    constructor Create; override;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; virtual;
    function AddIdentifier(const Identifier: String; El: TPasElement;
      const Kind: TPasIdentifierKind): TPasIdentifier;
    function FindElement(const aName: string): TPasElement;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    procedure WriteIdentifiers(Prefix: string); override;
  end;

  { TPasDefaultScope - root scope }

  TPasDefaultScope = class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasSectionScope - e.g. interface, implementation, program, library }

  TPasSectionScope = Class(TPasIdentifierScope)
  public
    UsesList: TFPList; // list of TPasSectionScope
    constructor Create; override;
    destructor Destroy; override;
    function FindIdentifierInSection(const Identifier: String): TPasIdentifier;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
  end;

  { TPasProcedureScope }

  TPasProcedureScope = Class(TPasIdentifierScope)
  public
  end;

  { TPasSubScope - base class for sub scopes }

  TPasSubScope = Class(TPasIdentifierScope)
  public
    class function IsStoredInElement: boolean; override;
  end;

  { TPasIterateFilterData }

  TPasIterateFilterData = record
    OnIterate: TIterateScopeElement;
    Data: Pointer;
  end;
  PPasIterateFilterData = ^TPasIterateFilterData;

  { TPasSubModuleScope - scope for searching unitname.<identifier> }

  TPasSubModuleScope = Class(TPasSubScope)
  private
    FCurModule: TPasModule;
    procedure OnInternalIterate(El: TPasElement; Scope: TPasScope;
      Data: Pointer; var Abort: boolean);
    procedure SetCurModule(AValue: TPasModule);
  public
    InterfaceScope: TPasSectionScope;
    ImplementationScope: TPasSectionScope;
    destructor Destroy; override;
    function FindIdentifier(const Identifier: String): TPasIdentifier; override;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); override;
    property CurModule: TPasModule read FCurModule write SetCurModule;
  end;

  TPasResolvedKind = (
    rkNone,
    rkIdentifier, // IdentEl is a type, var, const, property, proc, etc, built-in types have IdentEl=nil
                  // TypeEl is the resolved type
    rkExpr, // ExprEl is a const, e.g. 3, 'pas', 1..2, [1,2+3]
    rkArrayOf, // array of <TypeEl>, IdentEl might be nil
    rkPointer // @<IdentEl>, pointer of TypeEl
    );

  TPasResolvedType = record
    Kind: TPasResolvedKind;
    BaseType: TResolveBaseType;
    IdentEl: TPasElement;
    TypeEl: TPasType;
    ExprEl: TPasExpr;
  end;
  PPasResolvedType = ^TPasResolvedType;

  { TPasResolver }

  TPasResolver = Class(TPasTreeContainer)
  private
    FDefaultScope: TPasDefaultScope;
    FLastElement: TPasElement;
    FLastCreatedData: TResolveData;
    FLastMsg: string;
    FLastMsgArgs: TMessageArgs;
    FLastMsgElement: TPasElement;
    FLastMsgNumber: integer;
    FLastMsgPattern: string;
    FLastMsgType: TMessageType;
    FScopes: array of TPasScope; // stack of scopes
    FScopeCount: integer;
    FStoreSrcColumns: boolean;
    FRootElement: TPasElement;
    FTopScope: TPasScope;
    function GetScopes(Index: integer): TPasScope; inline;
  protected
    type
      TFindFirstElementData = record
        ErrorPosEl: TPasElement;
        Found: TPasElement;
      end;
      PFindFirstElementData = ^TFindFirstElementData;
    procedure OnFindFirstElement(El: TPasElement; Scope: TPasScope;
      FindFirstElementData: Pointer; var Abort: boolean); virtual;
  protected
    type
      TProcCompatibility = (
        pcIncompatible,
        pcCompatible, // e.g. assign a longint to an int64
        pcExact
        );
      TFindProcsData = record
        Params: TParamsExpr;
        Found: TPasProcedure;
        Compatible: TProcCompatibility;
        Count: integer;
      end;
      PFindProcsData = ^TFindProcsData;
    procedure OnFindProc(El: TPasElement; Scope: TPasScope;
      FindProcsData: Pointer; var Abort: boolean); virtual;
  protected
    procedure SetCurrentParser(AValue: TPasParser); override;
    procedure CheckTopScope(ExpectedClass: TPasScopeClass);
    procedure FinishModule;
    procedure FinishUsesList;
    procedure FinishTypeSection;
    procedure FinishProcedure;
    procedure FinishProcedureHeader;
    procedure ResolveImplBlock(Block: TPasImplBlock);
    procedure ResolveImplElement(El: TPasImplElement);
    procedure ResolveImplCaseOf(CaseOf: TPasImplCaseOf);
    procedure ResolveImplForLoop(Loop: TPasImplForLoop);
    procedure ResolveExpr(El: TPasExpr);
    procedure ResolveBinaryExpr(El: TBinaryExpr);
    procedure ResolveSubIdent(El: TBinaryExpr);
    procedure ResolveParamsExpr(Params: TParamsExpr);
    procedure AddModule(El: TPasModule);
    procedure AddSection(El: TPasSection);
    procedure AddType(El: TPasType);
    procedure AddVariable(El: TPasVariable);
    procedure AddProcedure(El: TPasProcedure);
    procedure AddArgument(El: TPasArgument);
    procedure AddFunctionResult(El: TPasResultElement);
    procedure StartProcedureBody(El: TProcedureBody);
    procedure WriteScopes;
  public
    constructor Create;
    destructor Destroy; override;
    function CreateElement(AClass: TPTreeElement; const AName: String;
      AParent: TPasElement; AVisibility: TPasMemberVisibility;
      const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
      overload; override;
    function FindElement(const AName: String): TPasElement; override;
    function FindFirstElement(const AName: String; ErrorPosEl: TPasElement): TPasElement;
    procedure IterateElements(const aName: string;
      const OnIterateElement: TIterateScopeElement; Data: Pointer;
      var Abort: boolean); virtual;
    procedure FinishScope(ScopeType: TPasScopeType); override;
    class procedure UnmangleSourceLineNumber(LineNumber: integer;
      out Line, Column: integer);
    procedure Clear; virtual;
    procedure AddObjFPCBuiltInIdentifiers(BaseTypes: TResolveBaseTypes = btAllStandardTypes);
    function CreateReference(DeclEl, RefEl: TPasElement): TResolvedReference; virtual;
    function CreateScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; virtual;
    procedure PopScope;
    procedure PushScope(Scope: TPasScope); overload;
    function PushScope(El: TPasElement; ScopeClass: TPasScopeClass): TPasScope; inline; overload;
    procedure SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
      Const Fmt : String; Args : Array of const; Element: TPasElement);
    procedure RaiseMsg(MsgNumber: integer; const Fmt: String;
      Args: Array of const; ErrorPosEl: TPasElement);
    procedure RaiseNotYetImplemented(El: TPasElement; Msg: string = ''); virtual;
    procedure RaiseInternalError(const Msg: string);
    procedure RaiseInvalidScopeForElement(El: TPasElement; const Msg: string = '');
    procedure RaiseIdentifierNotFound(Identifier: string; El: TPasElement);
    function CheckProcCompatibility(Proc: TPasProcedure;
      Params: TParamsExpr; RaiseOnError: boolean): TProcCompatibility;
    function CheckParamCompatibility(Expr: TPasExpr; Param: TPasArgument;
      ParamNo: integer; RaiseOnError: boolean): TProcCompatibility;
    procedure GetResolvedType(El: TPasElement; SkipTypeAlias: boolean;
      out ResolvedType: TPasResolvedType);
  public
    property LastElement: TPasElement read FLastElement;
    property StoreSrcColumns: boolean read FStoreSrcColumns write FStoreSrcColumns; {
       If true Line and Column is mangled together in TPasElement.SourceLineNumber.
       Use method UnmangleSourceLineNumber to extract. }
    property Scopes[Index: integer]: TPasScope read GetScopes;
    property ScopeCount: integer read FScopeCount;
    property TopScope: TPasScope read FTopScope;
    property RootElement: TPasElement read FRootElement;
    property DefaultScope: TPasDefaultScope read FDefaultScope write FDefaultScope;
    property LastMsg: string read FLastMsg write FLastMsg;
    property LastMsgNumber: integer read FLastMsgNumber write FLastMsgNumber;
    property LastMsgType: TMessageType read FLastMsgType write FLastMsgType;
    property LastMsgPattern: string read FLastMsgPattern write FLastMsgPattern;
    property LastMsgArgs: TMessageArgs read FLastMsgArgs write FLastMsgArgs;
    property LastMsgElement: TPasElement read FLastMsgElement write FLastMsgElement;
  end;

function GetObjName(o: TObject): string;
function GetProcDesc(Proc: TPasProcedure): string;
function GetTypeDesc(aType: TPasType): string;
function GetTreeDesc(El: TPasElement; Indent: integer = 0): string;
function GetResolvedTypeDesc(const T: TPasResolvedType): string;
procedure SetResolvedType(out ResolvedType: TPasResolvedType;
  Kind: TPasResolvedKind; BaseType: TResolveBaseType; IdentEl: TPasElement;
  TypeEl: TPasType); overload;
procedure SetResolvedTypeExpr(out ResolvedType: TPasResolvedType;
  BaseType: TResolveBaseType; ExprEl: TPasExpr); overload;

implementation

function GetObjName(o: TObject): string;
begin
  if o=nil then
    Result:='nil'
  else if o is TPasElement then
    Result:=TPasElement(o).Name+':'+o.ClassName
  else
    Result:=o.ClassName;
end;

function GetProcDesc(Proc: TPasProcedure): string;
var
  Args: TFPList;
  i: Integer;
  Arg: TPasArgument;
begin
  if Proc=nil then exit('nil');
  Result:=Proc.Name+'(';
  Args:=Proc.ProcType.Args;
  for i:=0 to Args.Count-1 do
    begin
    if i>0 then Result:=Result+';';
    Arg:=TPasArgument(Args[i]);
    if AccessNames[Arg.Access]<>'' then
      Result:=Result+AccessNames[Arg.Access]+' ';
    if Arg.ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTypeDesc(Arg.ArgType);
    end;
  Result:=Result+')';
  if cCallingConventions[Proc.ProcType.CallingConvention]<>'' then
    Result:=Result+';'+cCallingConventions[Proc.ProcType.CallingConvention];
end;

function GetTypeDesc(aType: TPasType): string;
begin
  if aType=nil then exit('nil');
  if (aType.ClassType=TPasUnresolvedSymbolRef)
      or (aType.ClassType=TPasUnresolvedTypeRef) then
    Result:=aType.Name
  else if aType.ClassType=TPasPointerType then
    Result:='^'+GetTypeDesc(TPasPointerType(aType).DestType)
  else if aType.ClassType=TPasAliasType then
    Result:=GetTypeDesc(TPasAliasType(aType).DestType)
  else if aType.ClassType=TPasTypeAliasType then
    Result:='type '+GetTypeDesc(TPasTypeAliasType(aType).DestType)
  else if aType.ClassType=TPasClassOfType then
    Result:='class of '+TPasClassOfType(aType).DestType.Name
  else if aType.ClassType=TPasArrayType then
    Result:='array['+TPasArrayType(aType).IndexRange+'] of '+GetTypeDesc(TPasArrayType(aType).ElType)
  else
    Result:=aType.ElementTypeName;
end;

function GetTreeDesc(El: TPasElement; Indent: integer): string;

  procedure LineBreak(SubIndent: integer);
  begin
    Inc(Indent,SubIndent);
    Result:=Result+LineEnding+Space(Indent);
  end;

var
  i, l: Integer;
begin
  if El=nil then exit('nil');
  Result:=El.Name+':'+El.ClassName+'=';
  if El is TPasExpr then
    begin
    if El.ClassType<>TBinaryExpr then
      Result:=Result+OpcodeStrings[TPasExpr(El).OpCode];
    if El.ClassType=TUnaryExpr then
      Result:=Result+GetTreeDesc(TUnaryExpr(El).Operand,Indent)
    else if El.ClassType=TBinaryExpr then
      Result:=Result+GetTreeDesc(TBinaryExpr(El).left,Indent)
         +OpcodeStrings[TPasExpr(El).OpCode]
         +GetTreeDesc(TBinaryExpr(El).right,Indent)
    else if El.ClassType=TPrimitiveExpr then
      Result:=Result+TPrimitiveExpr(El).Value
    else if El.ClassType=TBoolConstExpr then
      Result:=Result+BoolToStr(TBoolConstExpr(El).Value,'true','false')
    else if El.ClassType=TNilExpr then
      Result:=Result+'nil'
    else if El.ClassType=TInheritedExpr then
      Result:=Result+'inherited'
    else if El.ClassType=TSelfExpr then
      Result:=Result+'Self'
    else if El.ClassType=TParamsExpr then
      begin
      LineBreak(2);
      Result:=Result+GetTreeDesc(TParamsExpr(El).Value,Indent)+'(';
      l:=length(TParamsExpr(El).Params);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDesc(TParamsExpr(El).Params[i],Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+')';
      end
    else if El.ClassType=TRecordValues then
      begin
      Result:=Result+'(';
      l:=length(TRecordValues(El).Fields);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+TRecordValues(El).Fields[i].Name+':'
            +GetTreeDesc(TRecordValues(El).Fields[i].ValueExp,Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+')';
      end
    else if El.ClassType=TArrayValues then
      begin
      Result:=Result+'[';
      l:=length(TArrayValues(El).Values);
      if l>0 then
        begin
        inc(Indent,2);
        for i:=0 to l-1 do
          begin
          LineBreak(0);
          Result:=Result+GetTreeDesc(TArrayValues(El).Values[i],Indent);
          if i<l-1 then
            Result:=Result+','
          end;
        dec(Indent,2);
        end;
      Result:=Result+']';
      end;
    end
  else if El is TPasProcedure then
    begin
    Result:=Result+GetTreeDesc(TPasProcedure(El).ProcType,Indent);
    end
  else if El is TPasProcedureType then
    begin
    Result:=Result+'(';
    l:=TPasProcedureType(El).Args.Count;
    if l>0 then
      begin
      inc(Indent,2);
      for i:=0 to l-1 do
        begin
        LineBreak(0);
        Result:=Result+GetTreeDesc(TPasArgument(TPasProcedureType(El).Args[i]),Indent);
        if i<l-1 then
          Result:=Result+';'
        end;
      dec(Indent,2);
      end;
    Result:=Result+')';
    if El is TPasFunction then
      Result:=Result+':'+GetTreeDesc(TPasFunctionType(TPasFunction(El).ProcType).ResultEl,Indent);
    if TPasProcedureType(El).IsOfObject then
      Result:=Result+' of object';
    if TPasProcedureType(El).IsNested then
      Result:=Result+' of nested';
    if cCallingConventions[TPasProcedureType(El).CallingConvention]<>'' then
      Result:=Result+'; '+cCallingConventions[TPasProcedureType(El).CallingConvention];
    end
  else if El.ClassType=TPasResultElement then
    Result:=Result+GetTreeDesc(TPasResultElement(El).ResultType,Indent)
  else if El.ClassType=TPasArgument then
    begin
    if AccessNames[TPasArgument(El).Access]<>'' then
      Result:=Result+AccessNames[TPasArgument(El).Access]+' ';
    if TPasArgument(El).ArgType=nil then
      Result:=Result+'untyped'
    else
      Result:=Result+GetTreeDesc(TPasArgument(El).ArgType,Indent);
    end;
end;

function GetResolvedTypeDesc(const T: TPasResolvedType): string;
begin
  case T.Kind of
  rkNone: Result:='<none>';
  rkIdentifier: Result:=GetObjName(T.IdentEl)+':'+GetTypeDesc(T.TypeEl as TPasType)+'='+BaseTypeNames[T.BaseType];
  rkExpr: Result:=GetTreeDesc(T.ExprEl)+'='+BaseTypeNames[T.BaseType];
  rkArrayOf: Result:='array of '+GetTypeDesc(T.TypeEl as TPasType)+'='+BaseTypeNames[T.BaseType];
  rkPointer: Result:='^'+GetTypeDesc(T.TypeEl as TPasType)+'='+BaseTypeNames[T.BaseType];
  else Result:='<Ouch, unknown kind>';
  end;
end;

procedure SetResolvedType(out ResolvedType: TPasResolvedType;
  Kind: TPasResolvedKind; BaseType: TResolveBaseType; IdentEl: TPasElement;
  TypeEl: TPasType);
begin
  ResolvedType.Kind:=Kind;
  ResolvedType.BaseType:=BaseType;
  ResolvedType.IdentEl:=IdentEl;
  ResolvedType.TypeEl:=TypeEl;
  ResolvedType.ExprEl:=nil;
end;

procedure SetResolvedTypeExpr(out ResolvedType: TPasResolvedType;
  BaseType: TResolveBaseType; ExprEl: TPasExpr);
begin
  ResolvedType.Kind:=rkExpr;
  ResolvedType.BaseType:=BaseType;
  ResolvedType.IdentEl:=nil;
  ResolvedType.TypeEl:=nil;
  ResolvedType.ExprEl:=ExprEl;
end;

{ TPasIdentifier }

procedure TPasIdentifier.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if Element<>nil then
    Element.Release;
  FElement:=AValue;
  if Element<>nil then
    Element.AddRef;
end;

destructor TPasIdentifier.Destroy;
begin
  Element:=nil;
  inherited Destroy;
end;

{ EPasResolve }

procedure EPasResolve.SetPasElement(AValue: TPasElement);
begin
  if FPasElement=AValue then Exit;
  if PasElement<>nil then
    PasElement.Release;
  FPasElement:=AValue;
  if PasElement<>nil then
    PasElement.AddRef;
end;

destructor EPasResolve.Destroy;
begin
  PasElement:=nil;
  inherited Destroy;
end;

{ TResolvedReference }

procedure TResolvedReference.SetDeclaration(AValue: TPasElement);
begin
  if FDeclaration=AValue then Exit;
  if Declaration<>nil then
    Declaration.Release;
  FDeclaration:=AValue;
  if Declaration<>nil then
    Declaration.AddRef;
end;

destructor TResolvedReference.Destroy;
begin
  Declaration:=nil;
  inherited Destroy;
end;

{ TPasSubScope }

class function TPasSubScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TPasSubModuleScope }

procedure TPasSubModuleScope.OnInternalIterate(El: TPasElement;
  Scope: TPasScope; Data: Pointer; var Abort: boolean);
var
  FilterData: PPasIterateFilterData absolute Data;
begin
  if El.ClassType=TPasModule then
    exit; // skip used units
  // call the original iterator
  FilterData^.OnIterate(El,Scope,FilterData^.Data,Abort);
end;

procedure TPasSubModuleScope.SetCurModule(AValue: TPasModule);
begin
  if FCurModule=AValue then Exit;
  if CurModule<>nil then
    CurModule.Release;
  FCurModule:=AValue;
  if CurModule<>nil then
    CurModule.AddRef;
end;

destructor TPasSubModuleScope.Destroy;
begin
  CurModule:=nil;
  inherited Destroy;
end;

function TPasSubModuleScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
begin
  if ImplementationScope<>nil then
    begin
    Result:=ImplementationScope.FindIdentifierInSection(Identifier);
    if (Result<>nil) and (Result.Element.ClassType<>TPasModule) then
      exit;
    end;
  if InterfaceScope<>nil then
    Result:=InterfaceScope.FindIdentifierInSection(Identifier)
  else
    Result:=nil;
end;

procedure TPasSubModuleScope.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  FilterData: TPasIterateFilterData;
begin
  FilterData.OnIterate:=OnIterateElement;
  FilterData.Data:=Data;
  if ImplementationScope<>nil then
    begin
    ImplementationScope.IterateElements(aName,@OnInternalIterate,@FilterData,Abort);
    if Abort then exit;
    end;
  if InterfaceScope<>nil then
    InterfaceScope.IterateElements(aName,@OnInternalIterate,@FilterData,Abort);
end;

{ TPasSectionScope }

constructor TPasSectionScope.Create;
begin
  inherited Create;
  UsesList:=TFPList.Create;
end;

destructor TPasSectionScope.Destroy;
begin
  FreeAndNil(UsesList);
  inherited Destroy;
end;

function TPasSectionScope.FindIdentifierInSection(const Identifier: String
  ): TPasIdentifier;
begin
  Result:=inherited FindIdentifier(Identifier);
end;

function TPasSectionScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
begin
  Result:=inherited FindIdentifier(Identifier);
  if Result<>nil then
    exit;
  for i:=0 to UsesList.Count-1 do
    begin
    UsesScope:=TPasIdentifierScope(UsesList[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasSectionScope.FindIdentifier "',Identifier,'" in used unit ',GetObjName(UsesScope.Element));
    {$ENDIF}
    Result:=UsesScope.FindIdentifier(Identifier);
    if Result<>nil then exit;
    end;
end;

procedure TPasSectionScope.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  i: Integer;
  UsesScope: TPasIdentifierScope;
begin
  inherited IterateElements(aName, OnIterateElement, Data, Abort);
  if Abort then exit;
  for i:=0 to UsesList.Count-1 do
    begin
    UsesScope:=TPasIdentifierScope(UsesList[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasSectionScope.IterateElements "',aName,'" in used unit ',GetObjName(UsesScope.Element));
    {$ENDIF}
    UsesScope.IterateElements(aName,OnIterateElement,Data,Abort);
    if Abort then exit;
    end;
end;

{ TPasModuleScope }

procedure TPasModuleScope.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
begin
  if CompareText(aName,Element.Name)<>0 then exit;
  OnIterateElement(Element,Self,Data,Abort);
end;

{ TPasDefaultScope }

class function TPasDefaultScope.IsStoredInElement: boolean;
begin
  Result:=false;
end;

{ TResolveData }

procedure TResolveData.SetElement(AValue: TPasElement);
begin
  if FElement=AValue then Exit;
  if Element<>nil then
    Element.Release;
  FElement:=AValue;
  if Element<>nil then
    Element.AddRef;
end;

constructor TResolveData.Create;
begin

end;

destructor TResolveData.Destroy;
begin
  Element:=nil;
  inherited Destroy;
end;

{ TPasScope }

class function TPasScope.IsStoredInElement: boolean;
begin
  Result:=true;
end;

procedure TPasScope.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
begin
  if aName='' then ;
  if Data=nil then ;
  if OnIterateElement=nil then ;
  if Abort then ;
end;

procedure TPasScope.WriteIdentifiers(Prefix: string);
begin
  writeln(Prefix,'Element: ',GetObjName(Element));
end;

{ TPasResolver }

// inline
function TPasResolver.PushScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  Result:=CreateScope(El,ScopeClass);
  PushScope(Result);
end;

// inline
function TPasResolver.GetScopes(Index: integer): TPasScope;
begin
  Result:=FScopes[Index];
end;

procedure TPasResolver.OnFindFirstElement(El: TPasElement; Scope: TPasScope;
  FindFirstElementData: Pointer; var Abort: boolean);
var
  Data: PFindFirstElementData absolute FindFirstElementData;
begin
  Data^.Found:=El;
  Abort:=true;
  if Scope=nil then ;
end;

procedure TPasResolver.OnFindProc(El: TPasElement; Scope: TPasScope;
  FindProcsData: Pointer; var Abort: boolean);
var
  Data: PFindProcsData absolute FindProcsData;
  Proc: TPasProcedure;
  Compatible: TProcCompatibility;
begin
  if not (El is TPasProcedure) then
    begin
    // identifier is not a proc
    Abort:=true;
    if Data^.Found=nil then
      begin
      // ToDo: use the ( as error position
      RaiseMsg(nSyntaxErrorExpectedButFound,sSyntaxErrorExpectedButFound,[';','('],
        Data^.Params.Value);
      end
    else
      exit;
  end;
  // identifier is a proc
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.OnFindProc ',GetTreeDesc(El,2));
  {$ENDIF}
  Proc:=TPasProcedure(El);
  if Scope=nil then ;
  Compatible:=CheckProcCompatibility(Proc,Data^.Params,false);
  if (Data^.Found=nil) or (ord(Compatible)>ord(Data^.Compatible)) then
    begin
    Data^.Found:=Proc;
    Data^.Compatible:=Compatible;
    Data^.Count:=1;
    end
  else if Compatible=Data^.Compatible then
    inc(Data^.Count);
end;

procedure TPasResolver.SetCurrentParser(AValue: TPasParser);
begin
  //writeln('TPasResolver.SetCurrentParser ',AValue<>nil);
  if AValue=CurrentParser then exit;
  Clear;
  inherited SetCurrentParser(AValue);
  if CurrentParser<>nil then
    CurrentParser.Options:=CurrentParser.Options+[po_resolvestandardtypes];
end;

procedure TPasResolver.CheckTopScope(ExpectedClass: TPasScopeClass);
begin
  if TopScope=nil then
    RaiseInternalError('Expected TopScope='+ExpectedClass.ClassName+' but found nil');
  if TopScope.ClassType<>ExpectedClass then
    RaiseInternalError('Expected TopScope='+ExpectedClass.ClassName+' but found '+TopScope.ClassName);
end;

procedure TPasResolver.FinishModule;
var
  CurModuleClass: TClass;
  CurModule: TPasModule;
begin
  CurModule:=CurrentParser.CurModule;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule START ',CurModule.Name);
  {$ENDIF}
  CurModuleClass:=CurModule.ClassType;
  if (CurModuleClass=TPasProgram) or (CurModuleClass=TPasLibrary) then
    begin
    // resolve begin..end block
    ResolveImplBlock(CurModule.InitializationSection);
    end
  else if (CurModuleClass=TPasModule) then
    begin
    if CurModule.FinalizationSection<>nil then
      // finalization section finished -> resolve
      ResolveImplBlock(CurModule.FinalizationSection)
    else if CurModule.InitializationSection<>nil then
      // initialization section finished -> resolve
      ResolveImplBlock(CurModule.InitializationSection)
    else
      begin
      // ToDo: check if all forward procs are implemented
      end;
    end
  else
    RaiseInternalError(''); // unknown module

  // close all sections
  while (TopScope<>nil) and (TopScope.ClassType=TPasSectionScope) do
    PopScope;
  CheckTopScope(TPasModuleScope);
  PopScope;
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishModule END ',CurModule.Name);
  {$ENDIF}
end;

procedure TPasResolver.FinishUsesList;
var
  Section: TPasSection;
  i: Integer;
  El, PublicEl: TPasElement;
  Scope: TPasSectionScope;
  UsesScope: TPasIdentifierScope;
begin
  CheckTopScope(TPasSectionScope);
  Scope:=TPasSectionScope(TopScope);
  Section:=TPasSection(Scope.Element);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishUsesList Section=',Section.ClassName,' Section.UsesList.Count=',Section.UsesList.Count);
  {$ENDIF}
  for i:=0 to Section.UsesList.Count-1 do
    begin
    El:=TPasElement(Section.UsesList[i]);
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.FinishUsesList ',GetObjName(El));
    {$ENDIF}
    if (El.ClassType=TProgramSection) then
      RaiseInternalError('used unit is a program: '+GetObjName(El));

    Scope.AddIdentifier(El.Name,El,pikSimple);

    // check used unit
    PublicEl:=nil;
    if (El.ClassType=TLibrarySection) then
      PublicEl:=El
    else if (El.ClassType=TPasModule) then
      PublicEl:=TPasModule(El).InterfaceSection;
    if PublicEl=nil then
      RaiseInternalError('uses element has no interface section: '+GetObjName(El));
    if PublicEl.CustomData=nil then
      RaiseInternalError('uses element has no resolver data: '
        +El.Name+'->'+GetObjName(PublicEl));
    if not (PublicEl.CustomData is TPasIdentifierScope) then
      RaiseInternalError('uses element has invalid resolver data: '
        +El.Name+'->'+GetObjName(PublicEl)+'->'+PublicEl.CustomData.ClassName);

    UsesScope:=TPasIdentifierScope(PublicEl.CustomData);
    Scope.UsesList.Add(UsesScope);
    end;
end;

procedure TPasResolver.FinishTypeSection;
begin
  // ToDo: resolve pending forwards
end;

procedure TPasResolver.FinishProcedure;
var
  aProc: TPasProcedure;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.FinishProcedure START');
  {$ENDIF}
  CheckTopScope(TPasProcedureScope);
  aProc:=TPasProcedureScope(TopScope).Element as TPasProcedure;
  if aProc.Body<>nil then
    ResolveImplBlock(aProc.Body.Body);
  PopScope;
end;

procedure TPasResolver.FinishProcedureHeader;
begin
  CheckTopScope(TPasProcedureScope);
  // ToDo: check class
end;

procedure TPasResolver.ResolveImplBlock(Block: TPasImplBlock);
var
  i: Integer;
begin
  if Block=nil then exit;
  for i:=0 to Block.Elements.Count-1 do
    ResolveImplElement(TPasImplElement(Block.Elements[i]));
end;

procedure TPasResolver.ResolveImplElement(El: TPasImplElement);
begin
  //writeln('TPasResolver.ResolveImplElement ',GetObjName(El));
  if El=nil then
  else if El.ClassType=TPasImplAssign then
    begin
    ResolveExpr(TPasImplAssign(El).left);
    ResolveExpr(TPasImplAssign(El).right);
    end
  else if El.ClassType=TPasImplSimple then
    ResolveExpr(TPasImplSimple(El).expr)
  else if El.ClassType=TPasImplBlock then
    ResolveImplBlock(TPasImplBlock(El))
  else if El.ClassType=TPasImplRepeatUntil then
    begin
    ResolveImplBlock(TPasImplBlock(El));
    ResolveExpr(TPasImplRepeatUntil(El).ConditionExpr);
    end
  else if El.ClassType=TPasImplIfElse then
    begin
    ResolveExpr(TPasImplIfElse(El).ConditionExpr);
    ResolveImplElement(TPasImplIfElse(El).IfBranch);
    ResolveImplElement(TPasImplIfElse(El).ElseBranch);
    end
  else if El.ClassType=TPasImplWhileDo then
    begin
    ResolveExpr(TPasImplWhileDo(El).ConditionExpr);
    ResolveImplElement(TPasImplWhileDo(El).Body);
    end
  else if El.ClassType=TPasImplCaseOf then
    ResolveImplCaseOf(TPasImplCaseOf(El))
  else if El.ClassType=TPasImplForLoop then
    ResolveImplForLoop(TPasImplForLoop(El))
  else if El.ClassType=TPasImplTry then
    begin
    ResolveImplBlock(TPasImplTry(El));
    ResolveImplBlock(TPasImplTry(El).FinallyExcept);
    ResolveImplBlock(TPasImplTry(El).ElseBranch);
    end
  else if El.ClassType=TPasImplExceptOn then
    begin
    ResolveExpr(TPasImplExceptOn(El).VarExpr);
    ResolveExpr(TPasImplExceptOn(El).TypeExpr);
    ResolveImplElement(TPasImplExceptOn(El).Body);
    end
  else if El.ClassType=TPasImplRaise then
    begin
    ResolveExpr(TPasImplRaise(El).ExceptObject);
    ResolveExpr(TPasImplRaise(El).ExceptAddr);
    end
  else if El.ClassType=TPasImplCommand then
    begin
    if TPasImplCommand(El).Command<>'' then
      RaiseNotYetImplemented(El);
    end
  else
    RaiseNotYetImplemented(El);
end;

procedure TPasResolver.ResolveImplCaseOf(CaseOf: TPasImplCaseOf);
var
  i, j: Integer;
  Stat: TPasImplCaseStatement;
begin
  ResolveExpr(CaseOf.CaseExpr);
  for i:=0 to CaseOf.Elements.Count-1 do
    begin
    Stat:=TPasImplCaseStatement(CaseOf.Elements[i]);
    for j:=0 to Stat.Expressions.Count-1 do
      ResolveExpr(TPasExpr(Stat.Expressions[j]));
    ResolveImplElement(Stat.Body);
    end;
  ResolveImplBlock(CaseOf.ElseBranch);
end;

procedure TPasResolver.ResolveImplForLoop(Loop: TPasImplForLoop);
var
  DeclEl: TPasElement;
begin
  DeclEl:=FindFirstElement(Loop.VariableName,Loop);
  //writeln('TPasResolver.ResolveImplForLoop Ref=',GetObjName(Loop)+' Decl='+GetObjName(DeclEl));
  CreateReference(DeclEl,Loop);
  ResolveExpr(Loop.StartExpr);
  ResolveExpr(Loop.EndExpr);
  ResolveImplElement(Loop.Body);
end;

procedure TPasResolver.ResolveExpr(El: TPasExpr);
var
  Primitive: TPrimitiveExpr;
  DeclEl: TPasElement;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.ResolveExpr ',GetObjName(El));
  {$ENDIF}
  if El.ClassType=TPrimitiveExpr then
    begin
    Primitive:=TPrimitiveExpr(El);
    case Primitive.Kind of
    pekIdent:
      begin
      DeclEl:=FindFirstElement(Primitive.Value,El);
      //writeln('TPasResolver.ResolveExpr Ref=',GetObjName(El)+' Decl='+GetObjName(DeclEl));
      CreateReference(DeclEl,El);
      end;
    pekNumber,pekString,pekNil,pekBoolConst: exit;
    else
      RaiseNotYetImplemented(El);
    end;
    end
  else if El.ClassType=TUnaryExpr then
    ResolveExpr(TUnaryExpr(El).Operand)
  else if El.ClassType=TBinaryExpr then
    ResolveBinaryExpr(TBinaryExpr(El))
  else if El.ClassType=TParamsExpr then
    ResolveParamsExpr(TParamsExpr(El))
  else if El.ClassType=TBoolConstExpr then
  else if El.ClassType=TNilExpr then
  else
    RaiseNotYetImplemented(El);
end;

procedure TPasResolver.ResolveBinaryExpr(El: TBinaryExpr);
begin
  ResolveExpr(El.left);
  if El.right=nil then exit;
  case El.OpCode of
  eopNone,
  eopAdd,
  eopSubtract,
  eopMultiply,
  eopDivide,
  eopDiv,
  eopMod,
  eopPower,
  eopShr,
  eopShl,
  eopNot,
  eopAnd,
  eopOr,
  eopXor,
  eopEqual,
  eopNotEqual,
  eopLessThan,
  eopGreaterThan,
  eopLessthanEqual,
  eopGreaterThanEqual,
  eopIn,
  eopIs,
  eopAs,
  eopSymmetricaldifference:
    begin
    // ToDo: check if left operand supports operator
    ResolveExpr(El.right);
    // ToDo: check if operator fits
    end;
  //eopAddress: ;
  //eopDeref: ;
  eopSubIdent:
    ResolveSubIdent(El);
  else
    RaiseNotYetImplemented(El,OpcodeStrings[El.OpCode]);
  end;
end;

procedure TPasResolver.ResolveSubIdent(El: TBinaryExpr);
var
  DeclEl: TPasElement;
  ModuleScope: TPasSubModuleScope;
  aModule: TPasModule;
begin
  //writeln('TPasResolver.ResolveSubIdent El.left=',GetObjName(El.left));
  if El.left.ClassType=TPrimitiveExpr then
    begin
    //writeln('TPasResolver.ResolveSubIdent El.left.CustomData=',GetObjName(El.left.CustomData));
    if El.left.CustomData is TResolvedReference then
      begin
      DeclEl:=TResolvedReference(El.left.CustomData).Declaration;
      //writeln('TPasResolver.ResolveSubIdent Decl=',GetObjName(DeclEl));
      if DeclEl is TPasModule then
        begin
        // e.g. unitname.identifier
        // => search in interface and if this is our module in the implementation
        aModule:=TPasModule(DeclEl);
        ModuleScope:=TPasSubModuleScope.Create;
        ModuleScope.Owner:=Self;
        ModuleScope.CurModule:=aModule;
        if aModule is TPasProgram then
          begin // program
          if TPasProgram(aModule).ProgramSection<>nil then
            ModuleScope.InterfaceScope:=
              TPasProgram(aModule).ProgramSection.CustomData as TPasSectionScope;
          end
        else if aModule is TPasLibrary then
          begin // library
          if TPasLibrary(aModule).LibrarySection<>nil then
            ModuleScope.InterfaceScope:=
              TPasLibrary(aModule).LibrarySection.CustomData as TPasSectionScope;
          end
        else
          begin // unit
          if aModule.InterfaceSection<>nil then
            ModuleScope.InterfaceScope:=
              aModule.InterfaceSection.CustomData as TPasSectionScope;
          if (aModule=CurrentParser.CurModule)
              and (aModule.ImplementationSection<>nil)
              and (aModule.ImplementationSection.CustomData<>nil)
          then
            ModuleScope.ImplementationScope:=aModule.ImplementationSection.CustomData as TPasSectionScope;
          end;
        PushScope(ModuleScope);
        ResolveExpr(El.right);
        PopScope;
        end;
      end
    else
      RaiseMsg(nIllegalQualifier,sIllegalQualifier,['.'],El);
    end
  else
    RaiseMsg(nIllegalQualifier,sIllegalQualifier,['.'],El);
end;

procedure TPasResolver.ResolveParamsExpr(Params: TParamsExpr);
var
  i: Integer;
  ProcName: String;
  FindData: TFindProcsData;
  Abort: boolean;
begin
  // first resolve params
  for i:=0 to length(Params.Params)-1 do
    ResolveExpr(Params.Params[i]);
  // then search the best fitting proc
  if Params.Value.ClassType=TPrimitiveExpr then
    begin
    ProcName:=TPrimitiveExpr(Params.Value).Value;
    FindData:=Default(TFindProcsData);
    FindData.Params:=Params;
    Abort:=false;
    IterateElements(ProcName,@OnFindProc,@FindData,Abort);
    if FindData.Found=nil then
      RaiseIdentifierNotFound(ProcName,Params.Value);
    if FindData.Compatible=pcIncompatible then
      begin
      // found one proc, but it was incompatible => raise error
      {$IFDEF VerbosePasResolver}
      writeln('TPasResolver.ResolveParamsExpr found one proc, but it was incompatible => check again to raise error');
      {$ENDIF}
      CheckProcCompatibility(FindData.Found,Params,true);
      end;
    if FindData.Count>1 then
      begin
      // ToDo: multiple overloads fit => search again and list the candidates
      RaiseMsg(nIdentifierNotFound,sIdentifierNotFound,[],Params.Value);
      end;
    // found compatible proc
    CreateReference(FindData.Found,Params.Value);
    end
  else
    RaiseNotYetImplemented(Params,' with parameters');
end;

procedure TPasResolver.AddModule(El: TPasModule);
begin
  if TopScope<>DefaultScope then
    RaiseInvalidScopeForElement(El);
  PushScope(El,TPasModuleScope);
end;

procedure TPasResolver.AddSection(El: TPasSection);
// TInterfaceSection, TImplementationSection, TProgramSection, TLibrarySection
// Note: implementation scope is within the interface scope
var
  CurModuleClass: TClass;
begin
  CurModuleClass:=CurrentParser.CurModule.ClassType;
  if (CurModuleClass=TPasProgram) or (CurModuleClass=TPasLibrary) then
    begin
    if El.ClassType=TInitializationSection then
      ; // ToDo: check if all forward procs are implemented
    end
  else if CurModuleClass=TPasModule then
    begin
    if El.ClassType=TInitializationSection then
      begin
      // finished implementation
      // ToDo: check if all forward procs are implemented
      end
    else if El.ClassType=TFinalizationSection then
      begin
      if CurrentParser.CurModule.InitializationSection<>nil then
        begin
        // resolve initialization section
        ResolveImplBlock(CurrentParser.CurModule.InitializationSection);
        end
      else
        begin
        // finished implementation
        // ToDo: check if all forward procs are implemented
        end;
      end;
    end
  else
    RaiseInternalError(''); // unknown module
  PushScope(El,TPasSectionScope);
end;

procedure TPasResolver.AddType(El: TPasType);
begin
  if (El.Name='') then exit; // sub type
  if El is TPasUnresolvedTypeRef then exit; // built-in type
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddType El=',GetObjName(El),' El.Parent=',GetObjName(El.Parent));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(El);
  TPasIdentifierScope(TopScope).AddIdentifier(El.Name,El,pikSimple);
end;

procedure TPasResolver.AddVariable(El: TPasVariable);
begin
  if (El.Name='') then exit; // anonymous var
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddVariable ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(El);
  TPasIdentifierScope(TopScope).AddIdentifier(El.Name,El,pikSimple);
end;

procedure TPasResolver.AddProcedure(El: TPasProcedure);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddProcedure ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasIdentifierScope) then
    RaiseInvalidScopeForElement(El);
  TPasIdentifierScope(TopScope).AddIdentifier(El.Name,El,pikProc);
  PushScope(El,TPasProcedureScope);
end;

procedure TPasResolver.AddArgument(El: TPasArgument);
begin
  if (El.Name='') then
    RaiseInternalError(GetObjName(El));
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.AddArgument ',GetObjName(El));
  {$ENDIF}
  if not (TopScope is TPasProcedureScope) then
    RaiseInvalidScopeForElement(El);
  TPasProcedureScope(TopScope).AddIdentifier(El.Name,El,pikSimple);
end;

procedure TPasResolver.AddFunctionResult(El: TPasResultElement);
begin
  if TopScope.ClassType<>TPasProcedureScope then
    RaiseInvalidScopeForElement(El);
  TPasProcedureScope(TopScope).AddIdentifier(ResolverResultVar,El,pikSimple);
end;

procedure TPasResolver.StartProcedureBody(El: TProcedureBody);
begin
  if El=nil then ;
  // ToDo: check if all nested forward procs are resolved
  CheckTopScope(TPasProcedureScope);
end;

procedure TPasResolver.WriteScopes;
var
  i: Integer;
  Scope: TPasScope;
begin
  writeln('TPasResolver.WriteScopes ScopeCount=',ScopeCount);
  for i:=ScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    writeln('  ',i,'/',ScopeCount,' ',GetObjName(Scope));
    Scope.WriteIdentifiers('  ');
    end;
end;

constructor TPasResolver.Create;
begin
  inherited Create;
  FDefaultScope:=TPasDefaultScope.Create;
  PushScope(FDefaultScope);
end;

function TPasResolver.CreateElement(AClass: TPTreeElement; const AName: String;
  AParent: TPasElement; AVisibility: TPasMemberVisibility;
  const ASourceFilename: String; ASourceLinenumber: Integer): TPasElement;
var
  SrcFile: String;
  aScanner: TPascalScanner;
  SrcY, SrcX: Integer;
  El: TPasElement;
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateElement ',AClass.ClassName,' Name=',AName,' Parent=',GetObjName(AParent));
  {$ENDIF}
  if (AParent=nil) and (FRootElement<>nil)
  and (not AClass.InheritsFrom(TPasUnresolvedTypeRef)) then
    RaiseInternalError('TPasResolver.CreateElement more than one root element Class="'+AClass.ClassName+'" Root='+GetObjName(FRootElement));

  // get source position for good error messages
  aScanner:=CurrentParser.Scanner;
  SrcFile:=ASourceFilename;
  SrcY:=ASourceLinenumber;
  if (SrcFile='') or StoreSrcColumns then
    begin
    SrcFile:=aScanner.CurFilename;
    SrcY:=aScanner.CurRow;
    end;
  if SrcFile='' then
    RaiseInternalError('TPasResolver.CreateElement missing filename');
  if StoreSrcColumns then
    begin
    SrcX:=aScanner.CurColumn;
    if (SrcX<ParserMaxEmbeddedColumn) and (SrcY<ParserMaxEmbeddedRow) then
      SrcY:=-(SrcY*ParserMaxEmbeddedColumn+SrcX);
    end;

  // create element
  El:=AClass.Create(AName,AParent);
  FLastElement:=El;
  Result:=FLastElement;
  El.Visibility:=AVisibility;
  El.SourceFilename:=SrcFile;
  El.SourceLinenumber:=SrcY;
  if FRootElement=nil then
    FRootElement:=Result;

  // create scope
  if AClass.InheritsFrom(TPasType) then
    AddType(TPasType(El))
  else if (AClass.ClassType=TPasVariable)
      or (AClass.ClassType=TPasConst)
      or (AClass.ClassType=TPasProperty) then
    AddVariable(TPasVariable(El))
  else if AClass.ClassType=TPasArgument then
    AddArgument(TPasArgument(El))
  else if AClass.InheritsFrom(TPasProcedure) then
    AddProcedure(TPasProcedure(El))
  else if AClass.ClassType=TPasResultElement then
    AddFunctionResult(TPasResultElement(El))
  else if AClass.ClassType=TProcedureBody then
    StartProcedureBody(TProcedureBody(El))
  else if AClass.InheritsFrom(TPasSection) then
    AddSection(TPasSection(El))
  else if AClass.InheritsFrom(TPasModule) then
    AddModule(TPasModule(El))
  else if AClass.InheritsFrom(TPasExpr) then
  else if AClass.InheritsFrom(TPasImplBlock) then
  else if AClass.ClassType=TPasOverloadedProc then
  else
    RaiseNotYetImplemented(El);
end;

function TPasResolver.FindElement(const AName: String): TPasElement;
begin
  //writeln('TPasResolver.FindElement Name="',AName,'"');
  Result:=FindFirstElement(AName,LastElement);
end;

function TPasResolver.FindFirstElement(const AName: String;
  ErrorPosEl: TPasElement): TPasElement;
var
  FindFirstData: TFindFirstElementData;
  Abort: boolean;
begin
  //writeln('TPasResolver.FindIdentifier Name="',AName,'"');
  Result:=Nil;
  Abort:=false;
  FindFirstData:=Default(TFindFirstElementData);
  IterateElements(AName,@OnFindFirstElement,@FindFirstData,Abort);
  Result:=FindFirstData.Found;
  if Result<>nil then exit;
  RaiseIdentifierNotFound(AName,ErrorPosEl);
end;

procedure TPasResolver.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  i: Integer;
  Scope: TPasScope;
begin
  for i:=FScopeCount-1 downto 0 do
    begin
    Scope:=Scopes[i];
    Scope.IterateElements(AName,OnIterateElement,Data,Abort);
    if Abort then
      exit;
    if Scope is TPasSubScope then break;
    end;
end;

procedure TPasResolver.FinishScope(ScopeType: TPasScopeType);
begin
  case ScopeType of
  stModule: FinishModule;
  stUsesList: FinishUsesList;
  stTypeSection: FinishTypeSection;
  stTypeDef: ;
  stProcedure: FinishProcedure;
  stProcedureHeader: FinishProcedureHeader;
  end;
end;

class procedure TPasResolver.UnmangleSourceLineNumber(LineNumber: integer; out
  Line, Column: integer);
begin
  Line:=Linenumber;
  Column:=0;
  if Line<0 then begin
    Line:=-Line;
    Column:=Line mod ParserMaxEmbeddedColumn;
    Line:=Line div ParserMaxEmbeddedColumn;
  end;
end;

destructor TPasResolver.Destroy;
begin
  Clear;
  PopScope; // free default scope
  inherited Destroy;
end;

procedure TPasResolver.Clear;
var
  Data: TResolveData;
begin
  // clear stack, keep DefaultScope
  while (FScopeCount>0) and (FTopScope<>DefaultScope) do
    PopScope;
  // clear CustomData
  while FLastCreatedData<>nil do
    begin
    Data:=FLastCreatedData;
    Data.Element.CustomData:=nil;
    FLastCreatedData:=Data.Next;
    Data.Free;
    end;
end;

procedure TPasResolver.AddObjFPCBuiltInIdentifiers(BaseTypes: TResolveBaseTypes
  );
var
  bt: TResolveBaseType;
begin
  for bt in BaseTypes do
    FDefaultScope.AddIdentifier(BaseTypeNames[bt],
      TPasUnresolvedSymbolRef.Create(BaseTypeNames[bt],nil),pikCustom);
end;

function TPasResolver.CreateReference(DeclEl, RefEl: TPasElement
  ): TResolvedReference;
begin
  if RefEl.CustomData<>nil then
    raise EPasResolve.Create('TPasResolver.CreateReference customdata<>nil');
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateReference RefEl=',GetObjName(RefEl),' DeclEl=',GetObjName(DeclEl));
  {$ENDIF}
  Result:=TResolvedReference.Create;
  Result.Element:=RefEl;
  Result.Owner:=Self;
  Result.Next:=FLastCreatedData;
  Result.Declaration:=DeclEl;
  FLastCreatedData:=Result;
  RefEl.CustomData:=Result;
end;

function TPasResolver.CreateScope(El: TPasElement; ScopeClass: TPasScopeClass
  ): TPasScope;
begin
  if El.CustomData<>nil then
    raise EPasResolve.Create('TPasResolver.CreateScope customdata<>nil');

  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CreateScope El=',GetObjName(El),' ScopeClass=',ScopeClass.ClassName);
  {$ENDIF}
  Result:=ScopeClass.Create;
  Result.Element:=El;
  Result.Owner:=Self;
  Result.Next:=FLastCreatedData;
  FLastCreatedData:=Result;
  El.CustomData:=Result;
end;

procedure TPasResolver.PopScope;
var
  Scope: TPasScope;
begin
  if FScopeCount=0 then
    RaiseInternalError('PopScope');
  {$IFDEF VerbosePasResolver}
  //writeln('TPasResolver.PopScope ',FScopeCount,' ',FTopScope<>nil,' IsDefault=',FTopScope=FDefaultScope);
  writeln('TPasResolver.PopScope ',FTopScope.ClassName,' IsStoredInElement=',FTopScope.IsStoredInElement,' Element=',GetObjName(FTopScope.Element));
  {$ENDIF}
  dec(FScopeCount);
  if not FTopScope.IsStoredInElement then
    begin
    Scope:=FScopes[FScopeCount];
    if Scope.Element<>nil then
      Scope.Element.CustomData:=nil;
    if Scope=FDefaultScope then
      FDefaultScope:=nil;
    Scope.Free;
    FScopes[FScopeCount]:=nil;
    end;
  if FScopeCount>0 then
    FTopScope:=FScopes[FScopeCount-1]
  else
    FTopScope:=nil;
end;

procedure TPasResolver.PushScope(Scope: TPasScope);
begin
  if Scope=nil then
    RaiseInternalError('TPasResolver.PushScope nil');
  if length(FScopes)=FScopeCount then
    SetLength(FScopes,FScopeCount*2+10);
  FScopes[FScopeCount]:=Scope;
  inc(FScopeCount);
  FTopScope:=Scope;
  writeln('TPasResolver.PushScope ScopeCount=',ScopeCount,' ',GetObjName(FTopScope),' IsDefault=',FDefaultScope=FTopScope);
end;

procedure TPasResolver.SetLastMsg(MsgType: TMessageType; MsgNumber: integer;
  const Fmt: String; Args: array of const; Element: TPasElement);
begin
  FLastMsgType := MsgType;
  FLastMsgNumber := MsgNumber;
  FLastMsgPattern := Fmt;
  FLastMsg := Format(Fmt,Args);
  FLastElement := Element;
  CreateMsgArgs(FLastMsgArgs,Args);
end;

procedure TPasResolver.RaiseMsg(MsgNumber: integer; const Fmt: String;
  Args: array of const; ErrorPosEl: TPasElement);
var
  E: EPasResolve;
begin
  SetLastMsg(mtError,MsgNumber,Fmt,Args,ErrorPosEl);
  E:=EPasResolve.Create(FLastMsg);
  E.PasElement:=ErrorPosEl;
  E.MsgNumber:=MsgNumber;
  E.Args:=FLastMsgArgs;
  raise E;
end;

procedure TPasResolver.RaiseNotYetImplemented(El: TPasElement; Msg: string);
begin
  RaiseMsg(nNotYetImplemented,sNotYetImplemented+Msg,[GetObjName(El)],El);
end;

procedure TPasResolver.RaiseInternalError(const Msg: string);
begin
  raise Exception.Create('Internal error: '+Msg);
end;

procedure TPasResolver.RaiseInvalidScopeForElement(El: TPasElement;
  const Msg: string);
var
  i: Integer;
  s: String;
begin
  s:='invalid scope for "'+GetObjName(El)+'": ';
  for i:=0 to ScopeCount-1 do
    begin
    if i>0 then s:=s+',';
    s:=s+Scopes[i].ClassName;
    end;
  if Msg<>'' then
    s:=s+': '+Msg;
  RaiseInternalError(s);
end;

procedure TPasResolver.RaiseIdentifierNotFound(Identifier: string;
  El: TPasElement);
begin
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.RaiseIdentifierNotFound START');
  WriteScopes;
  {$ENDIF}
  RaiseMsg(nIdentifierNotFound,sIdentifierNotFound,[Identifier],El);
end;

function TPasResolver.CheckProcCompatibility(Proc: TPasProcedure;
  Params: TParamsExpr; RaiseOnError: boolean): TProcCompatibility;
var
  ProcArgs: TFPList;
  i, ParamCnt: Integer;
  Param: TPasExpr;
  ParamCompatibility: TProcCompatibility;
begin
  Result:=pcExact;
  ProcArgs:=Proc.ProcType.Args;
  // check args
  ParamCnt:=length(Params.Params);
  i:=0;
  while i<ParamCnt do
    begin
    Param:=Params.Params[i];
    if i>=ProcArgs.Count then
      begin
      // too many arguments
      if RaiseOnError then
        RaiseMsg(nWrongNumberOfParametersForCallTo,
          sWrongNumberOfParametersForCallTo,[GetProcDesc(Proc)],Param);
      exit(pcIncompatible);
      end;
    {$IFDEF VerbosePasResolver}
    writeln('TPasResolver.CheckProcCompatibility ',i,'/',ParamCnt);
    {$ENDIF}
    ParamCompatibility:=CheckParamCompatibility(Param,TPasArgument(ProcArgs[i]),i+1,RaiseOnError);
    if ParamCompatibility=pcIncompatible then
      exit(pcIncompatible);
    if ord(ParamCompatibility)<ord(Result) then
      Result:=ParamCompatibility;
    inc(i);
    end;
  if (i<ProcArgs.Count) and (TPasArgument(ProcArgs[i]).ValueExpr=nil) then
    begin
    // not enough arguments
    if RaiseOnError then
      // ToDo: position cursor on identifier
      RaiseMsg(nWrongNumberOfParametersForCallTo,
        sWrongNumberOfParametersForCallTo,[GetProcDesc(Proc)],Params.Value);
    exit(pcIncompatible);
    end;
end;

function TPasResolver.CheckParamCompatibility(Expr: TPasExpr;
  Param: TPasArgument; ParamNo: integer; RaiseOnError: boolean
  ): TProcCompatibility;
var
  ExprType, ParamType: TPasResolvedType;

  function ExprCanBeVarParam: boolean;
  begin
    Result:=false;
    if (ExprType.Kind<>rkIdentifier) then exit;
    if ExprType.IdentEl=nil then exit;
    if ExprType.IdentEl.ClassType=TPasVariable then exit(true);
    if (ExprType.IdentEl.ClassType=TPasConst)
        and (TPasConst(ExprType.IdentEl).VarType<>nil) then
      exit(true); // typed const are writable
  end;

var
  MustFitExactly: Boolean;
begin
  Result:=pcIncompatible;
  MustFitExactly:=Param.Access in [argVar, argOut];

  GetResolvedType(Expr,not MustFitExactly,ExprType);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Expr=',GetTreeDesc(Expr,2),' ResolvedExpr=',GetResolvedTypeDesc(ExprType));
  {$ENDIF}
  if ExprType.Kind=rkNone then
    RaiseInternalError('GetResolvedType returned rkNone for '+GetTreeDesc(Expr));

  if MustFitExactly then
    begin
    // Expr must be a variable
    if not ExprCanBeVarParam then
      begin
      if RaiseOnError then
        RaiseMsg(nVariableIdentifierExpected,sVariableIdentifierExpected,[],Expr);
      exit;
      end;
    end;

  GetResolvedType(Param,not MustFitExactly,ParamType);
  {$IFDEF VerbosePasResolver}
  writeln('TPasResolver.CheckParamCompatibility Param=',GetTreeDesc(Param,2),' ResolvedParam=',GetResolvedTypeDesc(ParamType));
  {$ENDIF}
  if ExprType.Kind=rkNone then
    RaiseInternalError('GetResolvedType returned rkNone for '+GetTreeDesc(Param));
  if (ParamType.TypeEl=nil) and (Param.ArgType<>nil) then
    RaiseInternalError('GetResolvedType returned TypeEl=nil for '+GetTreeDesc(Param));

  if MustFitExactly then
    begin
    if (ParamType.Kind=ExprType.Kind)
        or (ParamType.BaseType=ExprType.BaseType) then
      begin
      if (ParamType.TypeEl<>nil) and (ParamType.TypeEl=ExprType.TypeEl) then
        exit(pcExact);
      end;
    if RaiseOnError then
      RaiseMsg(nIncompatibleTypeArgNoVarParamMustMatchExactly,
        sIncompatibleTypeArgNoVarParamMustMatchExactly,
        [ParamNo,GetTypeDesc(ExprType.TypeEl),GetTypeDesc(ParamType.TypeEl)],
        Expr);
    exit(pcIncompatible);
    end;

  // check if the Expr can be converted to Param
  case ParamType.Kind of
    rkIdentifier,
    rkExpr:
      if ExprType.Kind in [rkExpr,rkIdentifier] then
      begin
        if ParamType.TypeEl=nil then
          begin
          // ToDo: untyped parameter
          end
        else if ParamType.BaseType=ExprType.BaseType then
          begin
          // ToDo: check btFile, btText
          exit(pcExact); // same base type, maybe not same type name (e.g. longint and integer)
          end
        else if (ParamType.BaseType in btAllNumbers)
            and (ExprType.BaseType in btAllNumbers) then
          exit(pcCompatible) // ToDo: range check for Expr
        else if (ParamType.BaseType in btAllBooleans)
            and (ExprType.BaseType in btAllBooleans) then
          exit(pcCompatible)
        else if (ParamType.BaseType in btAllStrings)
            and (ExprType.BaseType in btAllStrings) then
          exit(pcCompatible) // ToDo: check Expr if Param=btChar/btWideChar
        else if (ParamType.BaseType in btAllFloats)
            and (ExprType.BaseType in btAllFloats) then
          exit(pcCompatible)
        else if ExprType.BaseType=btNil then
          begin
            if ParamType.BaseType=btPointer then
              exit(pcExact);
            // ToDo: allow classes and custom pointers
          end
        else
          exit(pcIncompatible);
      end;
    //rkArrayOf: ;
    //rkPointer: ;
  else
  end;

  RaiseNotYetImplemented(Expr,':TPasResolver.CheckParamCompatibility: Param='+GetResolvedTypeDesc(ParamType)+' '+GetResolvedTypeDesc(ExprType));
end;

procedure TPasResolver.GetResolvedType(El: TPasElement; SkipTypeAlias: boolean; out
  ResolvedType: TPasResolvedType);
var
  bt: TResolveBaseType;
begin
  ResolvedType:=Default(TPasResolvedType);
  if El=nil then
    exit;
  if El.ClassType=TPrimitiveExpr then
    begin
    case TPrimitiveExpr(El).Kind of
      pekIdent:
        begin
        if El.CustomData is TResolvedReference then
          GetResolvedType(TResolvedReference(El.CustomData).Declaration,SkipTypeAlias,ResolvedType)
        else
          RaiseNotYetImplemented(El,': cannot resolve this');
        end;
      pekNumber:
        // ToDo: check if btByte, btSmallInt, ...
        SetResolvedTypeExpr(ResolvedType,btLongint,TPrimitiveExpr(El));
      pekString:
        SetResolvedTypeExpr(ResolvedType,btString,TPrimitiveExpr(El));
      //pekSet:
      pekNil:
        SetResolvedTypeExpr(ResolvedType,btNil,TPrimitiveExpr(El));
      pekBoolConst:
        SetResolvedTypeExpr(ResolvedType,btBoolean,TPrimitiveExpr(El));
      //pekRange:
      //pekUnary:
      //pekBinary:
      //pekFuncParams:
      //pekArrayParams:
      //pekListOfExp:
      //pekInherited:
      //pekSelf:
    else
      RaiseNotYetImplemented(El,': cannot resolve this');
    end;
    end
  else if El.ClassType=TPasUnresolvedSymbolRef then
    begin
    // built-in type
    for bt in TResolveBaseType do
      if CompareText(BaseTypeNames[bt],El.Name)=0 then
        begin
        SetResolvedType(ResolvedType,rkIdentifier,bt,nil,TPasUnresolvedSymbolRef(El));
        break;
        end;
    end
  else if El.ClassType=TPasAliasType then
    // e.f. 'var a: b' -> resolve b
    GetResolvedType(TPasTypeAliasType(El).DestType,true,ResolvedType)
  else if (El.ClassType=TPasTypeAliasType) and SkipTypeAlias then
    // e.g. 'type a = type b;' -> resolve b
    GetResolvedType(TPasTypeAliasType(El).DestType,true,ResolvedType)
  else if (El.ClassType=TPasVariable) or (El.ClassType=TPasConst)
      or (El.ClassType=TPasProperty) then
    begin
    // e.g. 'var a:b' -> resolve b, use a as IdentEl
    GetResolvedType(TPasVariable(El).VarType,SkipTypeAlias,ResolvedType);
    ResolvedType.IdentEl:=El;
    end
  else if El.ClassType=TPasArgument then
    begin
    if TPasArgument(El).ArgType=nil then
      // untyped parameter
      SetResolvedType(ResolvedType,rkIdentifier,btUntyped,El,nil)
    else
      begin
      // typed parameter -> use param as IdentEl, resolve type
      GetResolvedType(TPasArgument(El).ArgType,SkipTypeAlias,ResolvedType);
      ResolvedType.IdentEl:=El;
      end;
    end
  else
    RaiseNotYetImplemented(El,': cannot resolve this');
end;

{ TPasIdentifierScope }

procedure TPasIdentifierScope.OnClearItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Ident: TPasIdentifier;
begin
  if Dummy=nil then ;
  //writeln('TPasIdentifierScope.OnClearItem ',PasIdentifier.Identifier+':'+PasIdentifier.ClassName);
  while PasIdentifier<>nil do
    begin
    Ident:=PasIdentifier;
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    Ident.Free;
    end;
end;

procedure TPasIdentifierScope.OnWriteItem(Item, Dummy: pointer);
var
  PasIdentifier: TPasIdentifier absolute Item;
  Prefix: String;
begin
  Prefix:=AnsiString(Dummy);
  while PasIdentifier<>nil do
    begin
    writeln(Prefix,'Identifier="',PasIdentifier.Identifier,'" Element=',GetObjName(PasIdentifier.Element));
    PasIdentifier:=PasIdentifier.NextSameIdentifier;
    end;
end;

procedure TPasIdentifierScope.InternalAdd(Item: TPasIdentifier);
var
  Index: Integer;
  OldItem: TPasIdentifier;
  LoName: ShortString;
begin
  LoName:=lowercase(Item.Identifier);
  Index:=FItems.FindIndexOf(LoName);
  //writeln('  Index=',Index);
  if Index>=0 then
    begin
    // insert LIFO - last in, first out
    OldItem:=TPasIdentifier(FItems.List^[Index].Data);
    Item.NextSameIdentifier:=OldItem;
    FItems.List^[Index].Data:=Item;
    end
  else
    FItems.Add(LoName, Item);
end;

constructor TPasIdentifierScope.Create;
begin
  FItems:=TFPHashList.Create;
end;

destructor TPasIdentifierScope.Destroy;
begin
  FItems.ForEachCall(@OnClearItem,nil);
  FItems.Clear;
  FreeAndNil(FItems);
  inherited Destroy;
end;

function TPasIdentifierScope.FindIdentifier(const Identifier: String
  ): TPasIdentifier;
var
  LoName: ShortString;
begin
  LoName:=lowercase(Identifier);
  Result:=TPasIdentifier(FItems.Find(LoName));
end;

function TPasIdentifierScope.AddIdentifier(const Identifier: String;
  El: TPasElement; const Kind: TPasIdentifierKind): TPasIdentifier;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.AddIdentifier Identifier="',Identifier,'" El=',GetObjName(El));
  Item:=TPasIdentifier.Create;
  Item.Identifier:=Identifier;
  Item.Element:=El;
  Item.Kind:=Kind;

  InternalAdd(Item);
  //writeln('TPasIdentifierScope.AddIdentifier END');
  Result:=Item;
end;

function TPasIdentifierScope.FindElement(const aName: string): TPasElement;
var
  Item: TPasIdentifier;
begin
  //writeln('TPasIdentifierScope.FindElement "',aName,'"');
  Item:=FindIdentifier(aName);
  if Item=nil then
    Result:=nil
  else
    Result:=Item.Element;
  //writeln('TPasIdentifierScope.FindElement Found="',GetObjName(Result),'"');
end;

procedure TPasIdentifierScope.IterateElements(const aName: string;
  const OnIterateElement: TIterateScopeElement; Data: Pointer;
  var Abort: boolean);
var
  Item: TPasIdentifier;
begin
  Item:=FindIdentifier(aName);
  while Item<>nil do
    begin
    // writeln('TPasIdentifierScope.IterateElements ',Item.Identifier,' ',GetObjName(Item.Element));
    OnIterateElement(Item.Element,Self,Data,Abort);
    if Abort then exit;
    Item:=Item.NextSameIdentifier;
    end;
end;

procedure TPasIdentifierScope.WriteIdentifiers(Prefix: string);
begin
  inherited WriteIdentifiers(Prefix);
  Prefix:=Prefix+'  ';
  FItems.ForEachCall(@OnWriteItem,Pointer(Prefix));
end;

end.

