{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2014 by Michael Van Canneyt

    Unit tests for Pascal-to-Javascript converter class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************

 Examples:
    ./testpas2js --suite=TTestModule.TestEmptyProgram
    ./testpas2js --suite=TTestModule.TestEmptyUnit
}
unit tcmodules;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, contnrs, fppas2js,
  pastree, PScanner, PasResolver, PParser, jstree, jswriter, jsbase;

const
  po_pas2js = [po_asmwhole,po_resolvestandardtypes];
type

  { TTestPasParser }

  TTestPasParser = Class(TPasParser)
  end;

  TOnFindUnit = function(const aUnitName: String): TPasModule of object;

  { TTestEnginePasResolver }

  TTestEnginePasResolver = class(TPas2JsResolver)
  private
    FFilename: string;
    FModule: TPasModule;
    FOnFindUnit: TOnFindUnit;
    FParser: TTestPasParser;
    FResolver: TStreamResolver;
    FScanner: TPascalScanner;
    FSource: string;
    procedure SetModule(AValue: TPasModule);
  public
    destructor Destroy; override;
    function FindModule(const AName: String): TPasModule; override;
    property OnFindUnit: TOnFindUnit read FOnFindUnit write FOnFindUnit;
    property Filename: string read FFilename write FFilename;
    property Resolver: TStreamResolver read FResolver write FResolver;
    property Scanner: TPascalScanner read FScanner write FScanner;
    property Parser: TTestPasParser read FParser write FParser;
    property Source: string read FSource write FSource;
    property Module: TPasModule read FModule write SetModule;
  end;

  { TCustomTestModule }

  TCustomTestModule = Class(TTestCase)
  private
    FConverter: TPasToJSConverter;
    FEngine: TTestEnginePasResolver;
    FExpectedErrorClass: ExceptClass;
    FExpectedErrorMsg: string;
    FExpectedErrorNumber: integer;
    FFilename: string;
    FFileResolver: TStreamResolver;
    FJSInitBody: TJSFunctionBody;
    FJSInterfaceUses: TJSArrayLiteral;
    FJSModule: TJSSourceElements;
    FJSModuleSrc: TJSSourceElements;
    FJSSource: TStringList;
    FModule: TPasModule;
    FJSModuleCallArgs: TJSArguments;
    FModules: TObjectList;// list of TTestEnginePasResolver
    FParser: TTestPasParser;
    FPasProgram: TPasProgram;
    FJSRegModuleCall: TJSCallExpression;
    FScanner: TPascalScanner;
    FSkipTests: boolean;
    FSource: TStringList;
    FFirstPasStatement: TPasImplBlock;
    function GetModuleCount: integer;
    function GetModules(Index: integer): TTestEnginePasResolver;
    function OnPasResolverFindUnit(const aUnitName: String): TPasModule;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure Add(Line: string); virtual;
    Procedure StartParsing; virtual;
    procedure ParseModule; virtual;
    procedure ParseProgram; virtual;
    procedure ParseUnit; virtual;
  protected
    function FindModuleWithFilename(aFilename: string): TTestEnginePasResolver; virtual;
    function AddModule(aFilename: string): TTestEnginePasResolver; virtual;
    function AddModuleWithSrc(aFilename, Src: string): TTestEnginePasResolver; virtual;
    function AddModuleWithIntfImplSrc(aFilename, InterfaceSrc,
      ImplementationSrc: string): TTestEnginePasResolver; virtual;
    procedure AddSystemUnit; virtual;
    procedure StartProgram(NeedSystemUnit: boolean); virtual;
    procedure StartUnit(NeedSystemUnit: boolean); virtual;
    Procedure ConvertModule; virtual;
    Procedure ConvertProgram; virtual;
    Procedure ConvertUnit; virtual;
    procedure CheckDottedIdentifier(Msg: string; El: TJSElement; DottedName: string);
    function GetDottedIdentifier(El: TJSElement): string;
    procedure CheckSource(Msg,Statements, InitStatements: string); virtual;
    procedure CheckDiff(Msg, Expected, Actual: string); virtual;
    procedure SetExpectedPasResolverError(Msg: string; MsgNumber: integer);
    procedure SetExpectedConverterError(Msg: string; MsgNumber: integer);
    function IsErrorExpected(E: Exception): boolean;
    procedure HandleScannerError(E: EScannerError);
    procedure HandleParserError(E: EParserError);
    procedure HandlePasResolveError(E: EPasResolve);
    procedure HandlePas2JSError(E: EPas2JS);
    procedure HandleException(E: Exception);
    procedure RaiseException(E: Exception);
    procedure WriteSources(const aFilename: string; aRow, aCol: integer);
    property PasProgram: TPasProgram Read FPasProgram;
    property Modules[Index: integer]: TTestEnginePasResolver read GetModules;
    property ModuleCount: integer read GetModuleCount;
    property Engine: TTestEnginePasResolver read FEngine;
    property Filename: string read FFilename;
    Property Module: TPasModule Read FModule;
    property FirstPasStatement: TPasImplBlock read FFirstPasStatement;
    property Converter: TPasToJSConverter read FConverter;
    property JSSource: TStringList read FJSSource;
    property JSModule: TJSSourceElements read FJSModule;
    property JSRegModuleCall: TJSCallExpression read FJSRegModuleCall;
    property JSModuleCallArgs: TJSArguments read FJSModuleCallArgs;
    property JSInterfaceUses: TJSArrayLiteral read FJSInterfaceUses;
    property JSModuleSrc: TJSSourceElements read FJSModuleSrc;
    property JSInitBody: TJSFunctionBody read FJSInitBody;
    property ExpectedErrorClass: ExceptClass read FExpectedErrorClass write FExpectedErrorClass;
    property ExpectedErrorMsg: string read FExpectedErrorMsg write FExpectedErrorMsg;
    property ExpectedErrorNumber: integer read FExpectedErrorNumber write FExpectedErrorNumber;
    property SkipTests: boolean read FSkipTests write FSkipTests;
  public
    property Source: TStringList read FSource;
    property FileResolver: TStreamResolver read FFileResolver;
    property Scanner: TPascalScanner read FScanner;
    property Parser: TTestPasParser read FParser;
  end;

  { TTestModule }

  TTestModule = class(TCustomTestModule)
  Published
    // modules
    Procedure TestEmptyProgram;
    Procedure TestEmptyProgramUseStrict;
    Procedure TestEmptyUnit;
    Procedure TestEmptyUnitUseStrict;

    // vars/const
    Procedure TestVarInt;
    Procedure TestVarBaseTypes;
    Procedure TestBaseTypeSingleFail;
    Procedure TestBaseTypeExtendedFail;
    Procedure TestConstBaseTypes;
    Procedure TestUnitImplVars;
    Procedure TestUnitImplConsts;
    Procedure TestUnitImplRecord;
    Procedure TestRenameJSNameConflict;
    Procedure TestLocalConst;
    Procedure TestVarExternal;
    Procedure TestVarExternalOtherUnit;

    // strings
    Procedure TestCharConst;
    Procedure TestChar_Compare;
    Procedure TestChar_Ord;
    Procedure TestChar_Chr;
    Procedure TestStringConst;
    Procedure TestString_Length;
    Procedure TestString_Compare;
    Procedure TestString_SetLength;
    Procedure TestString_CharAt;
    Procedure TestStr;
    Procedure TestBaseType_AnsiStringFail;
    Procedure TestBaseType_UnicodeStringFail;
    Procedure TestBaseType_ShortStringFail;
    Procedure TestBaseType_RawByteStringFail;

    // alias types
    Procedure TestAliasTypeRef;
    Procedure TestTypeCast_BaseTypes;
    Procedure TestTypeCast_AliasBaseTypes;

    // functions
    Procedure TestEmptyProc;
    Procedure TestProcOneParam;
    Procedure TestFunctionWithoutParams;
    Procedure TestProcedureWithoutParams;
    Procedure TestPrgProcVar;
    Procedure TestProcTwoArgs;
    Procedure TestProc_DefaultValue;
    Procedure TestUnitProcVar;
    Procedure TestImplProc;
    Procedure TestFunctionResult;
    Procedure TestNestedProc;
    Procedure TestForwardProc;
    Procedure TestNestedForwardProc;
    Procedure TestAssignFunctionResult;
    Procedure TestFunctionResultInCondition;
    Procedure TestExit;
    Procedure TestBreak;
    Procedure TestContinue;
    Procedure TestProcedureExternal;
    Procedure TestProcedureExternalOtherUnit;
    Procedure TestProcedureAsm;
    Procedure TestProcedureAssembler;
    Procedure TestProcedure_VarParam;
    Procedure TestProcedureOverload;
    Procedure TestProcedureOverloadForward;
    Procedure TestProcedureOverloadUnit;
    Procedure TestProcedureOverloadNested;
    Procedure TestProc_Varargs;

    // enums, sets
    Procedure TestEnumName;
    Procedure TestEnumNumber;
    Procedure TestEnumFunctions;
    Procedure TestSet;
    Procedure TestSetOperators;
    Procedure TestSetFunctions;
    Procedure TestSet_PassAsArgClone;
    Procedure TestEnum_AsParams;
    Procedure TestSet_AsParams;
    Procedure TestSet_Property;

    // statements
    Procedure TestNestBegin;
    Procedure TestIncDec;
    Procedure TestAssignments;
    Procedure TestArithmeticOperators1;
    Procedure TestLogicalOperators;
    Procedure TestBitwiseOperators;
    Procedure TestFunctionInt;
    Procedure TestFunctionString;
    Procedure TestForLoop;
    Procedure TestForLoopInFunction;
    Procedure TestForLoop_ReadVarAfter;
    Procedure TestForLoop_Nested;
    Procedure TestRepeatUntil;
    Procedure TestAsmBlock;
    Procedure TestTryFinally;
    Procedure TestTryExcept;
    Procedure TestCaseOf;
    Procedure TestCaseOf_UseSwitch;
    Procedure TestCaseOfNoElse;
    Procedure TestCaseOfNoElse_UseSwitch;
    Procedure TestCaseOfRange;

    // arrays
    Procedure TestArray_Dynamic;
    Procedure TestArray_Dynamic_Nil;
    Procedure TestArray_DynMultiDimensional;
    Procedure TestArrayOfRecord;
    // ToDo: Procedure TestArrayOfSet;
    Procedure TestArray_AsParams;
    Procedure TestArrayElement_AsParams;
    Procedure TestArrayElementFromFuncResult_AsParams;
    Procedure TestArrayEnumTypeRange;
    Procedure TestArray_SetLengthProperty;
    Procedure TestArray_OpenArrayOfString;
    Procedure TestArray_Concat;
    Procedure TestArray_Copy;
    Procedure TestArray_InsertDelete;
    Procedure TestExternalClass_TypeCastArrayToExternalArray;
    Procedure TestExternalClass_TypeCastArrayFromExternalArray;
    // ToDo: const array
    // ToDo: SetLength(array of static array)

    // record
    Procedure TestRecord_Var;
    Procedure TestWithRecordDo;
    Procedure TestRecord_Assign;
    Procedure TestRecord_PassAsArgClone;
    Procedure TestRecord_AsParams;
    Procedure TestRecordElement_AsParams;
    Procedure TestRecordElementFromFuncResult_AsParams;
    Procedure TestRecordElementFromWith_AsParams;
    Procedure TestRecord_Equal;
    // ToDo: const record

    // classes
    Procedure TestClass_TObjectDefaultConstructor;
    Procedure TestClass_TObjectConstructorWithParams;
    Procedure TestClass_Var;
    Procedure TestClass_Method;
    Procedure TestClass_Inheritance;
    Procedure TestClass_AbstractMethod;
    Procedure TestClass_CallInherited_NoParams;
    Procedure TestClass_CallInherited_WithParams;
    Procedure TestClasS_CallInheritedConstructor;
    Procedure TestClass_ClassVar;
    Procedure TestClass_CallClassMethod;
    Procedure TestClass_Property;
    Procedure TestClass_Property_ClassMethod;
    Procedure TestClass_Property_Index;
    Procedure TestClass_PropertyOfTypeArray;
    Procedure TestClass_PropertyDefault;
    Procedure TestClass_PropertyOverride;
    Procedure TestClass_Assigned;
    Procedure TestClass_WithClassDoCreate;
    Procedure TestClass_WithClassInstDoProperty;
    Procedure TestClass_WithClassInstDoPropertyWithParams;
    Procedure TestClass_WithClassInstDoFunc;
    Procedure TestClass_TypeCast;
    Procedure TestClass_TypeCastUntypedParam;
    Procedure TestClass_Overloads;
    Procedure TestClass_OverloadsAncestor;
    Procedure TestClass_OverloadConstructor;
    Procedure TestClass_ReintroducedVar;
    Procedure TestClass_RaiseDescendant;
    Procedure TestClass_ExternalMethod;
    Procedure TestClass_ExternalVirtualNameMismatchFail;
    Procedure TestClass_ExternalOverrideFail;
    Procedure TestClass_ExternalVar;

    // class of
    Procedure TestClassOf_Create;
    Procedure TestClassOf_Call;
    Procedure TestClassOf_Assign;
    Procedure TestClassOf_Is;
    Procedure TestClassOf_Compare;
    Procedure TestClassOf_ClassVar;
    Procedure TestClassOf_ClassMethod;
    Procedure TestClassOf_ClassProperty;
    Procedure TestClassOf_ClassMethodSelf;
    Procedure TestClassOf_TypeCast;
    Procedure TestClassOf_ImplicitFunctionCall;

    // external class
    Procedure TestExternalClass_Var;
    // ToDo TestExternalClass_Const
    Procedure TestExternalClass_DuplicateVarFail;
    Procedure TestExternalClass_Method;
    Procedure TestExternalClass_NonExternalOverride;
    Procedure TestExternalClass_Property;
    Procedure TestExternalClass_ClassProperty;
    Procedure TestExternalClass_ClassOf;
    Procedure TestExternalClass_ClassOtherUnit;
    Procedure TestExternalClass_Is;
    Procedure TestExternalClass_As;
    Procedure TestExternalClass_DestructorFail;
    Procedure TestExternalClass_New;
    Procedure TestExternalClass_ClassOf_New;
    Procedure TestExternalClass_FuncClassOf_New;
    Procedure TestExternalClass_LocalConstSameName;
    Procedure TestExternalClass_ReintroduceOverload;
    Procedure TestExternalClass_Inherited;
    Procedure TestExternalClass_PascalAncestorFail;
    Procedure TestExternalClass_NewInstance;
    Procedure TestExternalClass_NewInstance_NonVirtualFail;
    Procedure TestExternalClass_NewInstance_FirstParamNotString_Fail;
    Procedure TestExternalClass_NewInstance_SecondParamTyped_Fail;
    Procedure TestExternalClass_TypeCastToRootClass;
    Procedure TestExternalClass_TypeCastStringToExternalString;
    Procedure TestExternalClass_CallClassFunctionOfInstanceFail;
    Procedure TestExternalClass_BracketOperator;

    // proc types
    Procedure TestProcType;
    Procedure TestProcType_FunctionFPC;
    Procedure TestProcType_FunctionDelphi;
    Procedure TestProcType_AsParam;
    Procedure TestProcType_MethodFPC;
    Procedure TestProcType_MethodDelphi;
    Procedure TestProcType_PropertyFPC;
    Procedure TestProcType_PropertyDelphi;
    Procedure TestProcType_WithClassInstDoPropertyFPC;
    Procedure TestProcType_Nested;

    // jsvalue
    Procedure TestJSValue_AssignToJSValue;
    Procedure TestJSValue_TypeCastToBaseType;
    Procedure TestJSValue_Equal;
    Procedure TestJSValue_Enum;
    Procedure TestJSValue_ClassInstance;
    Procedure TestJSValue_ClassOf;
    Procedure TestJSValue_ArrayOfJSValue;
    Procedure TestJSValue_Params;
    Procedure TestJSValue_UntypedParam;
    Procedure TestJSValue_FuncType;
  end;

function LinesToStr(Args: array of const): string;
function ExtractFileUnitName(aFilename: string): string;
function JSToStr(El: TJSElement): string;

implementation

function LinesToStr(Args: array of const): string;
var
  s: String;
  i: Integer;
begin
  s:='';
  for i:=Low(Args) to High(Args) do
    case Args[i].VType of
      vtChar:         s += Args[i].VChar+LineEnding;
      vtString:       s += Args[i].VString^+LineEnding;
      vtPChar:        s += Args[i].VPChar+LineEnding;
      vtWideChar:     s += AnsiString(Args[i].VWideChar)+LineEnding;
      vtPWideChar:    s += AnsiString(Args[i].VPWideChar)+LineEnding;
      vtAnsiString:   s += AnsiString(Args[i].VAnsiString)+LineEnding;
      vtWidestring:   s += AnsiString(WideString(Args[i].VWideString))+LineEnding;
      vtUnicodeString:s += AnsiString(UnicodeString(Args[i].VUnicodeString))+LineEnding;
    end;
  Result:=s;
end;

function ExtractFileUnitName(aFilename: string): string;
var
  p: Integer;
begin
  Result:=ExtractFileName(aFilename);
  if Result='' then exit;
  for p:=length(Result) downto 1 do
    case Result[p] of
    '/','\': exit;
    '.':
      begin
      Delete(Result,p,length(Result));
      exit;
      end;
    end;
end;

function JSToStr(El: TJSElement): string;
var
  aWriter: TBufferWriter;
  aJSWriter: TJSWriter;
begin
  aWriter:=TBufferWriter.Create(1000);
  try
    aJSWriter:=TJSWriter.Create(aWriter);
    aJSWriter.IndentSize:=2;
    aJSWriter.WriteJS(El);
    Result:=aWriter.AsAnsistring;
  finally
    aWriter.Free;
  end;
end;

{ TTestEnginePasResolver }

procedure TTestEnginePasResolver.SetModule(AValue: TPasModule);
begin
  if FModule=AValue then Exit;
  if Module<>nil then
    Module.Release;
  FModule:=AValue;
  if Module<>nil then
    Module.AddRef;
end;

destructor TTestEnginePasResolver.Destroy;
begin
  FreeAndNil(FResolver);
  Module:=nil;
  FreeAndNil(FParser);
  FreeAndNil(FScanner);
  FreeAndNil(FResolver);
  inherited Destroy;
end;

function TTestEnginePasResolver.FindModule(const AName: String): TPasModule;
begin
  Result:=nil;
  if Assigned(OnFindUnit) then
    Result:=OnFindUnit(AName);
end;

{ TCustomTestModule }

function TCustomTestModule.GetModuleCount: integer;
begin
  Result:=FModules.Count;
end;

function TCustomTestModule.GetModules(Index: integer
  ): TTestEnginePasResolver;
begin
  Result:=TTestEnginePasResolver(FModules[Index]);
end;

function TCustomTestModule.OnPasResolverFindUnit(const aUnitName: String
  ): TPasModule;
var
  i: Integer;
  CurEngine: TTestEnginePasResolver;
  CurUnitName: String;
begin
  //writeln('TTestModule.OnPasResolverFindUnit START Unit="',aUnitName,'"');
  Result:=nil;
  for i:=0 to ModuleCount-1 do
    begin
    CurEngine:=Modules[i];
    CurUnitName:=ExtractFileUnitName(CurEngine.Filename);
    //writeln('TTestModule.OnPasResolverFindUnit Checking ',i,'/',ModuleCount,' ',CurEngine.Filename,' ',CurUnitName);
    if CompareText(aUnitName,CurUnitName)=0 then
      begin
      Result:=CurEngine.Module;
      if Result<>nil then exit;
      //writeln('TTestModule.OnPasResolverFindUnit PARSING unit "',CurEngine.Filename,'"');
      FileResolver.FindSourceFile(aUnitName);

      CurEngine.Resolver:=TStreamResolver.Create;
      CurEngine.Resolver.OwnsStreams:=True;
      //writeln('TTestResolver.OnPasResolverFindUnit SOURCE=',CurEngine.Source);
      CurEngine.Resolver.AddStream(CurEngine.FileName,TStringStream.Create(CurEngine.Source));
      CurEngine.Scanner:=TPascalScanner.Create(CurEngine.Resolver);
      CurEngine.Parser:=TTestPasParser.Create(CurEngine.Scanner,CurEngine.Resolver,CurEngine);
      CurEngine.Parser.Options:=CurEngine.Parser.Options+po_pas2js;
      if CompareText(CurUnitName,'System')=0 then
        CurEngine.Parser.ImplicitUses.Clear;
      CurEngine.Scanner.OpenFile(CurEngine.Filename);
      try
        CurEngine.Parser.NextToken;
        CurEngine.Parser.ParseUnit(CurEngine.FModule);
      except
        on E: EParserError do
          HandleParserError(E);
        on E: EPasResolve do
          HandlePasResolveError(E);
        on E: Exception do
          HandleException(E);
      end;
      //writeln('TTestModule.OnPasResolverFindUnit END ',CurUnitName);
      Result:=CurEngine.Module;
      exit;
      end;
    end;
  writeln('TTestModule.OnPasResolverFindUnit missing unit "',aUnitName,'"');
  Fail('can''t find unit "'+aUnitName+'"');
end;

procedure TCustomTestModule.SetUp;
begin
  inherited SetUp;
  FSkipTests:=false;
  FSource:=TStringList.Create;
  FModules:=TObjectList.Create(true);

  FFilename:='test1.pp';
  FFileResolver:=TStreamResolver.Create;
  FFileResolver.OwnsStreams:=True;
  FScanner:=TPascalScanner.Create(FFileResolver);
  FEngine:=AddModule(Filename);
  FParser:=TTestPasParser.Create(FScanner,FFileResolver,FEngine);
  Parser.Options:=Parser.Options+po_pas2js;
  FModule:=Nil;
  FConverter:=TPasToJSConverter.Create;
  FConverter.UseLowerCase:=false;

  FExpectedErrorClass:=nil;
end;

procedure TCustomTestModule.TearDown;
begin
  FSkipTests:=false;
  FJSModule:=nil;
  FJSRegModuleCall:=nil;
  FJSModuleCallArgs:=nil;
  FJSInterfaceUses:=nil;
  FJSModuleSrc:=nil;
  FJSInitBody:=nil;
  FreeAndNil(FJSSource);
  FreeAndNil(FJSModule);
  FreeAndNil(FConverter);
  Engine.Clear;
  if Assigned(FModule) then
    begin
    FModule.Release;
    FModule:=nil;
    end;
  FreeAndNil(FSource);
  FreeAndNil(FParser);
  FreeAndNil(FScanner);
  FreeAndNil(FFileResolver);
  if FModules<>nil then
    begin
    FreeAndNil(FModules);
    FEngine:=nil;
    end;

  inherited TearDown;
end;

procedure TCustomTestModule.Add(Line: string);
begin
  Source.Add(Line);
end;

procedure TCustomTestModule.StartParsing;
var
  Src: String;
begin
  Src:=Source.Text;
  FEngine.Source:=Src;
  FileResolver.AddStream(FileName,TStringStream.Create(Src));
  Scanner.OpenFile(FileName);
  Writeln('// Test : ',Self.TestName);
  Writeln(Src);
end;

procedure TCustomTestModule.ParseModule;
begin
  if SkipTests then exit;
  FFirstPasStatement:=nil;
  try
    StartParsing;
    Parser.ParseMain(FModule);
  except
    on E: EParserError do
      HandleParserError(E);
    on E: EPasResolve do
      HandlePasResolveError(E);
    on E: EPas2JS do
      HandlePas2JSError(E);
    on E: Exception do
      HandleException(E);
  end;
  if SkipTests then exit;

  AssertNotNull('Module resulted in Module',FModule);
  AssertEquals('modulename',lowercase(ChangeFileExt(FFileName,'')),lowercase(Module.Name));
  TAssert.AssertSame('Has resolver',Engine,Parser.Engine);
end;

procedure TCustomTestModule.ParseProgram;
begin
  if SkipTests then exit;
  ParseModule;
  if SkipTests then exit;
  AssertEquals('Has program',TPasProgram,Module.ClassType);
  FPasProgram:=TPasProgram(Module);
  AssertNotNull('Has program section',PasProgram.ProgramSection);
  AssertNotNull('Has initialization section',PasProgram.InitializationSection);
  if (PasProgram.InitializationSection.Elements.Count>0) then
    if TObject(PasProgram.InitializationSection.Elements[0]) is TPasImplBlock then
      FFirstPasStatement:=TPasImplBlock(PasProgram.InitializationSection.Elements[0]);
end;

procedure TCustomTestModule.ParseUnit;
begin
  if SkipTests then exit;
  ParseModule;
  if SkipTests then exit;
  AssertEquals('Has unit (TPasModule)',TPasModule,Module.ClassType);
  AssertNotNull('Has interface section',Module.InterfaceSection);
  AssertNotNull('Has implementation section',Module.ImplementationSection);
  if (Module.InitializationSection<>nil)
      and (Module.InitializationSection.Elements.Count>0)
      and (TObject(Module.InitializationSection.Elements[0]) is TPasImplBlock) then
    FFirstPasStatement:=TPasImplBlock(Module.InitializationSection.Elements[0]);
end;

function TCustomTestModule.FindModuleWithFilename(aFilename: string
  ): TTestEnginePasResolver;
var
  i: Integer;
begin
  for i:=0 to ModuleCount-1 do
    if CompareText(Modules[i].Filename,aFilename)=0 then
      exit(Modules[i]);
  Result:=nil;
end;

function TCustomTestModule.AddModule(aFilename: string
  ): TTestEnginePasResolver;
begin
  //writeln('TTestModuleConverter.AddModule ',aFilename);
  if FindModuleWithFilename(aFilename)<>nil then
    Fail('TTestModuleConverter.AddModule: file "'+aFilename+'" already exists');
  Result:=TTestEnginePasResolver.Create;
  Result.Filename:=aFilename;
  Result.AddObjFPCBuiltInIdentifiers([btChar,btString,btLongint,btInt64,btBoolean,btDouble]);
  Result.OnFindUnit:=@OnPasResolverFindUnit;
  FModules.Add(Result);
end;

function TCustomTestModule.AddModuleWithSrc(aFilename, Src: string
  ): TTestEnginePasResolver;
begin
  Result:=AddModule(aFilename);
  Result.Source:=Src;
end;

function TCustomTestModule.AddModuleWithIntfImplSrc(aFilename, InterfaceSrc,
  ImplementationSrc: string): TTestEnginePasResolver;
var
  Src: String;
begin
  Src:='unit '+ExtractFileUnitName(aFilename)+';'+LineEnding;
  Src+=LineEnding;
  Src+='interface'+LineEnding;
  Src+=LineEnding;
  Src+=InterfaceSrc;
  Src+='implementation'+LineEnding;
  Src+=LineEnding;
  Src+=ImplementationSrc;
  Src+='end.'+LineEnding;
  Result:=AddModuleWithSrc(aFilename,Src);
end;

procedure TCustomTestModule.AddSystemUnit;
begin
  AddModuleWithIntfImplSrc('system.pp',
    // interface
    LinesToStr([
    'type',
    '  integer=longint;',
    'var',
    '  ExitCode: Longint;',
    ''
    // implementation
    ]),LinesToStr([
    ''
    ]));
end;

procedure TCustomTestModule.StartProgram(NeedSystemUnit: boolean);
begin
  if NeedSystemUnit then
    AddSystemUnit
  else
    Parser.ImplicitUses.Clear;
  Add('program test1;');
  Add('');
end;

procedure TCustomTestModule.StartUnit(NeedSystemUnit: boolean);
begin
  if NeedSystemUnit then
    AddSystemUnit
  else
    Parser.ImplicitUses.Clear;
  Add('unit Test1;');
  Add('');
end;

procedure TCustomTestModule.ConvertModule;
var
  ModuleNameExpr: TJSLiteral;
  FunDecl, InitFunction: TJSFunctionDeclarationStatement;
  FunDef: TJSFuncDef;
  InitAssign: TJSSimpleAssignStatement;
  FunBody: TJSFunctionBody;
  InitName: String;
begin
  if SkipTests then exit;
  try
    FJSModule:=FConverter.ConvertPasElement(Module,Engine) as TJSSourceElements;
  except
    on E: EScannerError do
      HandleScannerError(E);
    on E: EParserError do
      HandleParserError(E);
    on E: EPasResolve do
      HandlePasResolveError(E);
    on E: EPas2JS do
      HandlePas2JSError(E);
    on E: Exception do
      HandleException(E);
  end;
  if SkipTests then exit;
  if ExpectedErrorClass<>nil then
    Fail('Missing '+ExpectedErrorClass.ClassName+' error {'+ExpectedErrorMsg+'} ('+IntToStr(ExpectedErrorNumber)+')');

  FJSSource:=TStringList.Create;
  FJSSource.Text:=JSToStr(JSModule);
  {$IFDEF VerbosePas2JS}
  writeln('TTestModule.ConvertModule JS:');
  write(FJSSource.Text);
  {$ENDIF}

  // rtl.module(...
  AssertEquals('jsmodule has one statement - the call',1,JSModule.Statements.Count);
  AssertNotNull('register module call',JSModule.Statements.Nodes[0].Node);
  AssertEquals('register module call',TJSCallExpression,JSModule.Statements.Nodes[0].Node.ClassType);
  FJSRegModuleCall:=JSModule.Statements.Nodes[0].Node as TJSCallExpression;
  AssertNotNull('register module rtl.module expr',JSRegModuleCall.Expr);
  AssertNotNull('register module rtl.module args',JSRegModuleCall.Args);
  AssertEquals('rtl.module args',TJSArguments,JSRegModuleCall.Args.ClassType);
  FJSModuleCallArgs:=JSRegModuleCall.Args as TJSArguments;
  AssertEquals('rtl.module args.count',3,JSModuleCallArgs.Elements.Count);

  // parameter 'unitname'
  AssertNotNull('module name param',JSModuleCallArgs.Elements.Elements[0].Expr);
  ModuleNameExpr:=JSModuleCallArgs.Elements.Elements[0].Expr as TJSLiteral;
  AssertEquals('module name param is string',ord(jstString),ord(ModuleNameExpr.Value.ValueType));
  if Module is TPasProgram then
    AssertEquals('module name','program',String(ModuleNameExpr.Value.AsString))
  else
    AssertEquals('module name',Module.Name,String(ModuleNameExpr.Value.AsString));

  // main uses section
  AssertNotNull('interface uses section',JSModuleCallArgs.Elements.Elements[1].Expr);
  AssertEquals('interface uses section type',TJSArrayLiteral,JSModuleCallArgs.Elements.Elements[1].Expr.ClassType);
  FJSInterfaceUses:=JSModuleCallArgs.Elements.Elements[1].Expr as TJSArrayLiteral;

  // function()
  AssertNotNull('module function',JSModuleCallArgs.Elements.Elements[2].Expr);
  AssertEquals('module function type',TJSFunctionDeclarationStatement,JSModuleCallArgs.Elements.Elements[2].Expr.ClassType);
  FunDecl:=JSModuleCallArgs.Elements.Elements[2].Expr as TJSFunctionDeclarationStatement;
  AssertNotNull('module function def',FunDecl.AFunction);
  FunDef:=FunDecl.AFunction as TJSFuncDef;
  AssertEquals('module function name','',String(FunDef.Name));
  AssertNotNull('module function body',FunDef.Body);
  FunBody:=FunDef.Body as TJSFunctionBody;
  FJSModuleSrc:=FunBody.A as TJSSourceElements;

  // init this.$main - the last statement
  if Module is TPasProgram then
    begin
    InitName:='$main';
    AssertEquals('this.'+InitName+' function 1',true,JSModuleSrc.Statements.Count>0);
    end
  else
    InitName:='$init';
  FJSInitBody:=nil;
  if JSModuleSrc.Statements.Count>0 then
    begin
    InitAssign:=JSModuleSrc.Statements.Nodes[JSModuleSrc.Statements.Count-1].Node as TJSSimpleAssignStatement;
    if GetDottedIdentifier(InitAssign.LHS)='this.'+InitName then
      begin
      InitFunction:=InitAssign.Expr as TJSFunctionDeclarationStatement;
      FJSInitBody:=InitFunction.AFunction.Body as TJSFunctionBody;
      end
    else if Module is TPasProgram then
      CheckDottedIdentifier('init function',InitAssign.LHS,'this.'+InitName);
    end;
end;

procedure TCustomTestModule.ConvertProgram;
begin
  Add('end.');
  ParseProgram;
  ConvertModule;
end;

procedure TCustomTestModule.ConvertUnit;
begin
  Add('end.');
  ParseUnit;
  ConvertModule;
end;

procedure TCustomTestModule.CheckDottedIdentifier(Msg: string; El: TJSElement;
  DottedName: string);
begin
  if DottedName='' then
    begin
    AssertNull(Msg,El);
    end
  else
    begin
    AssertNotNull(Msg,El);
    AssertEquals(Msg,DottedName,GetDottedIdentifier(El));
    end;
end;

function TCustomTestModule.GetDottedIdentifier(El: TJSElement): string;
begin
  if El=nil then
    Result:=''
  else if El is TJSPrimaryExpressionIdent then
    Result:=String(TJSPrimaryExpressionIdent(El).Name)
  else if El is TJSDotMemberExpression then
    Result:=GetDottedIdentifier(TJSDotMemberExpression(El).MExpr)+'.'+String(TJSDotMemberExpression(El).Name)
  else
    AssertEquals('GetDottedIdentifier',TJSPrimaryExpressionIdent,El.ClassType);
end;

procedure TCustomTestModule.CheckSource(Msg, Statements, InitStatements: string);
var
  ActualSrc, ExpectedSrc, InitName: String;
begin
  ActualSrc:=JSToStr(JSModuleSrc);
  ExpectedSrc:=Statements;
  if Module is TPasProgram then
    InitName:='$main'
  else
    InitName:='$init';
  if (Module is TPasProgram) or (InitStatements<>'') then
    ExpectedSrc:=ExpectedSrc+LineEnding
      +'this.'+InitName+' = function () {'+LineEnding
      +InitStatements
      +'};'+LineEnding;
  //writeln('TTestModule.CheckSource InitStatements="',InitStatements,'"');
  CheckDiff(Msg,ExpectedSrc,ActualSrc);
end;

procedure TCustomTestModule.CheckDiff(Msg, Expected, Actual: string);
// search diff, ignore changes in spaces
const
  SpaceChars = [#9,#10,#13,' '];
var
  ExpectedP, ActualP: PChar;

  function FindLineEnd(p: PChar): PChar;
  begin
    Result:=p;
    while not (Result^ in [#0,#10,#13]) do inc(Result);
  end;

  function FindLineStart(p, MinP: PChar): PChar;
  begin
    while (p>MinP) and not (p[-1] in [#10,#13]) do dec(p);
    Result:=p;
  end;

  procedure DiffFound;
  var
    ActLineStartP, ActLineEndP, p, StartPos: PChar;
    ExpLine, ActLine: String;
    i: Integer;
  begin
    writeln('Diff found "',Msg,'". Lines:');
    // write correct lines
    p:=PChar(Expected);
    repeat
      StartPos:=p;
      while not (p^ in [#0,#10,#13]) do inc(p);
      ExpLine:=copy(Expected,StartPos-PChar(Expected)+1,p-StartPos);
      if p^ in [#10,#13] then begin
        if (p[1] in [#10,#13]) and (p^<>p[1]) then
          inc(p,2)
        else
          inc(p);
      end;
      if p<=ExpectedP then begin
        writeln('= ',ExpLine);
      end else begin
        // diff line
        // write actual line
        ActLineStartP:=FindLineStart(ActualP,PChar(Actual));
        ActLineEndP:=FindLineEnd(ActualP);
        ActLine:=copy(Actual,ActLineStartP-PChar(Actual)+1,ActLineEndP-ActLineStartP);
        writeln('- ',ActLine);
        // write expected line
        writeln('+ ',ExpLine);
        // write empty line with pointer ^
        for i:=1 to 2+ExpectedP-StartPos do write(' ');
        writeln('^');
        AssertEquals(Msg,ExpLine,ActLine);
        break;
      end;
    until p^=#0;
    raise Exception.Create('diff found, but lines are the same, internal error');
  end;

var
  IsSpaceNeeded: Boolean;
  LastChar: Char;
begin
  if Expected='' then Expected:=' ';
  if Actual='' then Actual:=' ';
  ExpectedP:=PChar(Expected);
  ActualP:=PChar(Actual);
  repeat
    //writeln('TTestModule.CheckDiff Exp="',ExpectedP^,'" Act="',ActualP^,'"');
    case ExpectedP^ of
    #0:
      begin
      // check that rest of Actual has only spaces
      while ActualP^ in SpaceChars do inc(ActualP);
      if ActualP^<>#0 then
        DiffFound;
      exit;
      end;
    ' ',#9,#10,#13:
      begin
      // skip space in Expected
      IsSpaceNeeded:=false;
      if ExpectedP>PChar(Expected) then
        LastChar:=ExpectedP[-1]
      else
        LastChar:=#0;
      while ExpectedP^ in SpaceChars do inc(ExpectedP);
      if (LastChar in ['a'..'z','A'..'Z','0'..'9','_','$'])
          and (ExpectedP^ in ['a'..'z','A'..'Z','0'..'9','_','$']) then
        IsSpaceNeeded:=true;
      if IsSpaceNeeded and (not (ActualP^ in SpaceChars)) then
        DiffFound;
      while ActualP^ in SpaceChars do inc(ActualP);
      end;
    else
      while ActualP^ in SpaceChars do inc(ActualP);
      if ExpectedP^<>ActualP^ then
        DiffFound;
      inc(ExpectedP);
      inc(ActualP);
    end;
  until false;
end;

procedure TCustomTestModule.SetExpectedPasResolverError(Msg: string;
  MsgNumber: integer);
begin
  ExpectedErrorClass:=EPasResolve;
  ExpectedErrorMsg:=Msg;
  ExpectedErrorNumber:=MsgNumber;
end;

procedure TCustomTestModule.SetExpectedConverterError(Msg: string;
  MsgNumber: integer);
begin
  ExpectedErrorClass:=EPas2JS;
  ExpectedErrorMsg:=Msg;
  ExpectedErrorNumber:=MsgNumber;
end;

function TCustomTestModule.IsErrorExpected(E: Exception): boolean;
var
  MsgNumber: Integer;
begin
  Result:=false;
  if (ExpectedErrorClass=nil) or (ExpectedErrorClass<>E.ClassType) then exit;
  if E is EPas2JS then
    MsgNumber:=EPas2JS(E).MsgNumber
  else if E is EPasResolve then
    MsgNumber:=EPasResolve(E).MsgNumber
  else
    MsgNumber:=0;
  Result:=(MsgNumber=ExpectedErrorNumber) and (E.Message=ExpectedErrorMsg);
  if Result then
    SkipTests:=true;
end;

procedure TCustomTestModule.HandleScannerError(E: EScannerError);
begin
  if IsErrorExpected(E) then exit;
  WriteSources(Scanner.CurFilename,Scanner.CurRow,Scanner.CurColumn);
  writeln('ERROR: TCustomTestModule.HandleScannerError '+E.ClassName+':'+E.Message
    +' '+Scanner.CurFilename
    +'('+IntToStr(Scanner.CurRow)+','+IntToStr(Scanner.CurColumn)+')');
  RaiseException(E);
end;

procedure TCustomTestModule.HandleParserError(E: EParserError);
begin
  if IsErrorExpected(E) then exit;
  WriteSources(E.Filename,E.Row,E.Column);
  writeln('ERROR: TCustomTestModule.HandleParserError '+E.ClassName+':'+E.Message
    +' '+E.Filename+'('+IntToStr(E.Row)+','+IntToStr(E.Column)+')'
    +' Line="'+Scanner.CurLine+'"'
    );
  RaiseException(E);
end;

procedure TCustomTestModule.HandlePasResolveError(E: EPasResolve);
var
  Row, Col: integer;
begin
  if IsErrorExpected(E) then exit;
  Engine.UnmangleSourceLineNumber(E.PasElement.SourceLinenumber,Row,Col);
  WriteSources(E.PasElement.SourceFilename,Row,Col);
  writeln('ERROR: TCustomTestModule.HandlePasResolveError '+E.ClassName+':'+E.Message
    +' '+E.PasElement.SourceFilename
    +'('+IntToStr(Row)+','+IntToStr(Col)+')');
  RaiseException(E);
end;

procedure TCustomTestModule.HandlePas2JSError(E: EPas2JS);
var
  Row, Col: integer;
begin
  if IsErrorExpected(E) then exit;
  Engine.UnmangleSourceLineNumber(E.PasElement.SourceLinenumber,Row,Col);
  WriteSources(E.PasElement.SourceFilename,Row,Col);
  writeln('ERROR: TCustomTestModule.HandlePas2JSError '+E.ClassName+':'+E.Message
    +' '+E.PasElement.SourceFilename
    +'('+IntToStr(Row)+','+IntToStr(Col)+')');
  RaiseException(E);
end;

procedure TCustomTestModule.HandleException(E: Exception);
begin
  if IsErrorExpected(E) then exit;
  WriteSources('',0,0);
  writeln('ERROR: TCustomTestModule.HandleException '+E.ClassName+':'+E.Message);
  RaiseException(E);
end;

procedure TCustomTestModule.RaiseException(E: Exception);
var
  MsgNumber: Integer;
begin
  if ExpectedErrorClass<>nil then begin
    if FExpectedErrorClass=E.ClassType then begin
      if E is EPas2JS then
        MsgNumber:=EPas2JS(E).MsgNumber
      else if E is EPasResolve then
        MsgNumber:=EPasResolve(E).MsgNumber
      else
        MsgNumber:=0;
      AssertEquals('Expected error message ('+IntToStr(ExpectedErrorNumber)+')','{'+ExpectedErrorMsg+'}','{'+E.Message+'}');
      AssertEquals('Expected {'+ExpectedErrorMsg+'}, but got msg {'+E.Message+'} number',
        ExpectedErrorNumber,MsgNumber);
    end else begin
      AssertEquals('Wrong exception class',ExpectedErrorClass.ClassName,E.ClassName);
    end;
  end;
  Fail(E.Message);
end;

procedure TCustomTestModule.WriteSources(const aFilename: string; aRow,
  aCol: integer);
var
  IsSrc: Boolean;
  i, j: Integer;
  SrcLines: TStringList;
  Line: string;
  aModule: TTestEnginePasResolver;
begin
  for i:=0 to ModuleCount-1 do
    begin
    aModule:=Modules[i];
    SrcLines:=TStringList.Create;
    try
      SrcLines.Text:=aModule.Source;
      IsSrc:=ExtractFilename(aModule.Filename)=ExtractFileName(aFilename);
      writeln('Testcode:-File="',aModule.Filename,'"----------------------------------:');
      for j:=1 to SrcLines.Count do
        begin
        Line:=SrcLines[j-1];
        if IsSrc and (j=aRow) then
          begin
          write('*');
          Line:=LeftStr(Line,aCol-1)+'|'+copy(Line,aCol,length(Line));
          end;
        writeln(Format('%:4d: ',[j]),Line);
        end;
    finally
      SrcLines.Free;
    end;
    end;
end;

{ TTestModule }

procedure TTestModule.TestEmptyProgram;
begin
  StartProgram(false);
  Add('begin');
  ConvertProgram;
  CheckSource('TestEmptyProgram','','');
end;

procedure TTestModule.TestEmptyProgramUseStrict;
begin
  Converter.Options:=Converter.Options+[coUseStrict];
  StartProgram(false);
  Add('begin');
  ConvertProgram;
  CheckSource('TestEmptyProgramUseStrict','"use strict";','');
end;

procedure TTestModule.TestEmptyUnit;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  ConvertUnit;
  CheckSource('TestEmptyUnit',
    LinesToStr([
    'var $impl = {',
    '};',
    'this.$impl = $impl;'
    ]),
    '');
end;

procedure TTestModule.TestEmptyUnitUseStrict;
begin
  Converter.Options:=Converter.Options+[coUseStrict];
  StartUnit(false);
  Add('interface');
  Add('implementation');
  ConvertUnit;
  CheckSource('TestEmptyUnitUseStrict',
    LinesToStr([
    '"use strict";',
    'var $impl = {',
    '};',
    'this.$impl = $impl;'
    ]),
    '');
end;

procedure TTestModule.TestVarInt;
begin
  StartProgram(false);
  Add('var MyI: longint;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestVarInt','this.MyI=0;','');
end;

procedure TTestModule.TestVarBaseTypes;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('  s: string;');
  Add('  c: char;');
  Add('  b: boolean;');
  Add('  d: double;');
  Add('  i2: longint = 3;');
  Add('  s2: string = ''foo'';');
  Add('  c2: char = ''4'';');
  Add('  b2: boolean = true;');
  Add('  d2: double = 5.6;');
  Add('  i3: longint = $707;');
  Add('  i4: int64 = 4503599627370495;');
  Add('  i5: int64 = -4503599627370496;');
  Add('  i6: int64 =   $fffffffffffff;');
  Add('  i7: int64 = -$10000000000000;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestVarBaseTypes',
    LinesToStr([
    'this.i=0;',
    'this.s="";',
    'this.c="";',
    'this.b=false;',
    'this.d=0.0;',
    'this.i2=3;',
    'this.s2="foo";',
    'this.c2="4";',
    'this.b2=true;',
    'this.d2=5.6;',
    'this.i3=0x707;',
    'this.i4= 4503599627370495;',
    'this.i5= -4503599627370496;',
    'this.i6= 0xfffffffffffff;',
    'this.i7=-0x10000000000000;'
    ]),
    '');
end;

procedure TTestModule.TestBaseTypeSingleFail;
begin
  StartProgram(false);
  Add('var s: single;');
  SetExpectedPasResolverError('identifier not found "single"',nIdentifierNotFound);
  ConvertProgram;
end;

procedure TTestModule.TestBaseTypeExtendedFail;
begin
  StartProgram(false);
  Add('var e: extended;');
  SetExpectedPasResolverError('identifier not found "extended"',nIdentifierNotFound);
  ConvertProgram;
end;

procedure TTestModule.TestConstBaseTypes;
begin
  StartProgram(false);
  Add('const');
  Add('  i: longint = 3;');
  Add('  s: string = ''foo'';');
  Add('  c: char = ''4'';');
  Add('  b: boolean = true;');
  Add('  d: double = 5.6;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestVarBaseTypes',
    LinesToStr([
    'this.i=3;',
    'this.s="foo";',
    'this.c="4";',
    'this.b=true;',
    'this.d=5.6;'
    ]),
    '');
end;

procedure TTestModule.TestAliasTypeRef;
begin
  StartProgram(false);
  Add('type');
  Add('  a=longint;');
  Add('  b=a;');
  Add('var');
  Add('  c: A;');
  Add('  d: B;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestAliasTypeRef',
    LinesToStr([ // statements
    'this.c = 0;',
    'this.d = 0;'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestTypeCast_BaseTypes;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('  b: boolean;');
  Add('  d: double;');
  Add('  s: string;');
  Add('  c: char;');
  Add('begin');
  Add('  i:=longint(i);');
  Add('  i:=longint(b);');
  Add('  b:=boolean(b);');
  Add('  b:=boolean(i);');
  Add('  d:=double(d);');
  Add('  d:=double(i);');
  Add('  s:=string(s);');
  Add('  s:=string(c);');
  Add('  c:=char(c);');
  ConvertProgram;
  CheckSource('TestAliasTypeRef',
    LinesToStr([ // statements
    'this.i = 0;',
    'this.b = false;',
    'this.d = 0.0;',
    'this.s = "";',
    'this.c = "";',
    '']),
    LinesToStr([ // this.$main
    'this.i = this.i;',
    'this.i = (this.b ? 1 : 0);',
    'this.b = this.b;',
    'this.b = this.i != 0;',
    'this.d = this.d;',
    'this.d = this.i;',
    'this.s = this.s;',
    'this.s = this.c;',
    'this.c = this.c;',
    '']));
end;

procedure TTestModule.TestTypeCast_AliasBaseTypes;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TYesNo = boolean;');
  Add('  TFloat = double;');
  Add('  TCaption = string;');
  Add('  TChar = char;');
  Add('var');
  Add('  i: integer;');
  Add('  b: TYesNo;');
  Add('  d: TFloat;');
  Add('  s: TCaption;');
  Add('  c: TChar;');
  Add('begin');
  Add('  i:=integer(i);');
  Add('  i:=integer(b);');
  Add('  b:=TYesNo(b);');
  Add('  b:=TYesNo(i);');
  Add('  d:=TFloat(d);');
  Add('  d:=TFloat(i);');
  Add('  s:=TCaption(s);');
  Add('  s:=TCaption(c);');
  Add('  c:=TChar(c);');
  ConvertProgram;
  CheckSource('TestAliasTypeRef',
    LinesToStr([ // statements
    'this.i = 0;',
    'this.b = false;',
    'this.d = 0.0;',
    'this.s = "";',
    'this.c = "";',
    '']),
    LinesToStr([ // this.$main
    'this.i = this.i;',
    'this.i = (this.b ? 1 : 0);',
    'this.b = this.b;',
    'this.b = this.i != 0;',
    'this.d = this.d;',
    'this.d = this.i;',
    'this.s = this.s;',
    'this.s = this.c;',
    'this.c = this.c;',
    '']));
end;

procedure TTestModule.TestEmptyProc;
begin
  StartProgram(false);
  Add('procedure Test;');
  Add('begin');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestEmptyProc',
    LinesToStr([ // statements
    'this.Test = function () {',
    '};'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestProcOneParam;
begin
  StartProgram(false);
  Add('procedure ProcA(i: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  PROCA(3);');
  ConvertProgram;
  CheckSource('TestProcOneParam',
    LinesToStr([ // statements
    'this.ProcA = function (i) {',
    '};'
    ]),
    LinesToStr([ // this.$main
    'this.ProcA(3);'
    ]));
end;

procedure TTestModule.TestFunctionWithoutParams;
begin
  StartProgram(false);
  Add('function FuncA: longint;');
  Add('begin');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  I:=FUNCA();');
  Add('  I:=FUNCA;');
  Add('  FUNCA();');
  Add('  FUNCA;');
  ConvertProgram;
  CheckSource('TestProcWithoutParams',
    LinesToStr([ // statements
    'this.FuncA = function () {',
    '  var Result = 0;',
    '  return Result;',
    '};',
    'this.i=0;'
    ]),
    LinesToStr([ // this.$main
    'this.i=this.FuncA();',
    'this.i=this.FuncA();',
    'this.FuncA();',
    'this.FuncA();'
    ]));
end;

procedure TTestModule.TestProcedureWithoutParams;
begin
  StartProgram(false);
  Add('procedure ProcA;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  PROCA();');
  Add('  PROCA;');
  ConvertProgram;
  CheckSource('TestProcWithoutParams',
    LinesToStr([ // statements
    'this.ProcA = function () {',
    '};'
    ]),
    LinesToStr([ // this.$main
    'this.ProcA();',
    'this.ProcA();'
    ]));
end;

procedure TTestModule.TestIncDec;
begin
  StartProgram(false);
  Add('var');
  Add('  Bar: longint;');
  Add('begin');
  Add('  inc(bar);');
  Add('  inc(bar,2);');
  Add('  dec(bar);');
  Add('  dec(bar,3);');
  ConvertProgram;
  CheckSource('TestIncDec',
    LinesToStr([ // statements
    'this.Bar = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.Bar+=1;',
    'this.Bar+=2;',
    'this.Bar-=1;',
    'this.Bar-=3;'
    ]));
end;

procedure TTestModule.TestAssignments;
begin
  StartProgram(false);
  Parser.Options:=Parser.Options+[po_cassignments];
  Add('var');
  Add('  Bar:longint;');
  Add('begin');
  Add('  bar:=3;');
  Add('  bar+=4;');
  Add('  bar-=5;');
  Add('  bar*=6;');
  ConvertProgram;
  CheckSource('TestAssignments',
    LinesToStr([ // statements
    'this.Bar = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.Bar=3;',
    'this.Bar+=4;',
    'this.Bar-=5;',
    'this.Bar*=6;'
    ]));
end;

procedure TTestModule.TestArithmeticOperators1;
begin
  StartProgram(false);
  Add('var');
  Add('  vA,vB,vC:longint;');
  Add('begin');
  Add('  va:=1;');
  Add('  vb:=va+va;');
  Add('  vb:=va div vb;');
  Add('  vb:=va mod vb;');
  Add('  vb:=va+va*vb+va div vb;');
  Add('  vc:=-va;');
  Add('  va:=va-vb;');
  Add('  vb:=va;');
  Add('  if va<vb then vc:=va else vc:=vb;');
  ConvertProgram;
  CheckSource('TestArithmeticOperators1',
    LinesToStr([ // statements
    'this.vA = 0;',
    'this.vB = 0;',
    'this.vC = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.vA = 1;',
    'this.vB = this.vA + this.vA;',
    'this.vB = Math.floor(this.vA / this.vB);',
    'this.vB = this.vA % this.vB;',
    'this.vB = (this.vA + (this.vA * this.vB)) + Math.floor(this.vA / this.vB);',
    'this.vC = -this.vA;',
    'this.vA = this.vA - this.vB;',
    'this.vB = this.vA;',
    'if (this.vA < this.vB){ this.vC = this.vA } else this.vC = this.vB;'
    ]));
end;

procedure TTestModule.TestLogicalOperators;
begin
  StartProgram(false);
  Add('var');
  Add('  vA,vB,vC:boolean;');
  Add('begin');
  Add('  va:=vb and vc;');
  Add('  va:=vb or vc;');
  Add('  va:=true and vc;');
  Add('  va:=(vb and vc) or (va and vb);');
  Add('  va:=not vb;');
  ConvertProgram;
  CheckSource('TestLogicalOperators',
    LinesToStr([ // statements
    'this.vA = false;',
    'this.vB = false;',
    'this.vC = false;'
    ]),
    LinesToStr([ // this.$main
    'this.vA = this.vB && this.vC;',
    'this.vA = this.vB || this.vC;',
    'this.vA = true && this.vC;',
    'this.vA = (this.vB && this.vC) || (this.vA && this.vB);',
    'this.vA = !this.vB;'
    ]));
end;

procedure TTestModule.TestBitwiseOperators;
begin
  StartProgram(false);
  Add('var');
  Add('  vA,vB,vC:longint;');
  Add('begin');
  Add('  va:=vb and vc;');
  Add('  va:=vb or vc;');
  Add('  va:=vb xor vc;');
  Add('  va:=vb shl vc;');
  Add('  va:=vb shr vc;');
  Add('  va:=3 and vc;');
  Add('  va:=(vb and vc) or (va and vb);');
  Add('  va:=not vb;');
  ConvertProgram;
  CheckSource('TestBitwiseOperators',
    LinesToStr([ // statements
    'this.vA = 0;',
    'this.vB = 0;',
    'this.vC = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.vA = this.vB & this.vC;',
    'this.vA = this.vB | this.vC;',
    'this.vA = this.vB ^ this.vC;',
    'this.vA = this.vB << this.vC;',
    'this.vA = this.vB >>> this.vC;',
    'this.vA = 3 & this.vC;',
    'this.vA = (this.vB & this.vC) | (this.vA & this.vB);',
    'this.vA = ~this.vB;'
    ]));
end;

procedure TTestModule.TestPrgProcVar;
begin
  StartProgram(false);
  Add('procedure Proc1;');
  Add('type');
  Add('  t1=longint;');
  Add('var');
  Add('  vA:t1;');
  Add('begin');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestPrgProcVar',
    LinesToStr([ // statements
    'this.Proc1 = function () {',
    '  var vA=0;',
    '};'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestUnitProcVar;
begin
  StartUnit(false);
  Add('interface');
  Add('');
  Add('type tA=string; // unit scope');
  Add('procedure Proc1;');
  Add('');
  Add('implementation');
  Add('');
  Add('procedure Proc1;');
  Add('type tA=longint; // local proc scope');
  Add('var  v1:tA; // using local tA');
  Add('begin');
  Add('end;');
  Add('var  v2:tA; // using interface tA');
  ConvertUnit;
  CheckSource('TestUnitProcVar',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    'this.Proc1 = function () {',
    '  var v1 = 0;',
    '};',
    '$impl.v2 = "";'
    ]),
    '' // this.$init
    );
end;

procedure TTestModule.TestImplProc;
begin
  StartUnit(false);
  Add('interface');
  Add('');
  Add('procedure Proc1;');
  Add('');
  Add('implementation');
  Add('');
  Add('procedure Proc1; begin end;');
  Add('procedure Proc2; begin end;');
  Add('initialization');
  Add('  Proc1;');
  Add('  Proc2;');
  ConvertUnit;
  CheckSource('TestImplProc',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    'this.Proc1 = function () {',
    '};',
    '$impl.Proc2 = function () {',
    '};',
    '']),
    LinesToStr([ // this.$init
    'this.Proc1();',
    '$impl.Proc2();',
    '']));
end;

procedure TTestModule.TestFunctionResult;
begin
  StartProgram(false);
  Add('function Func1: longint;');
  Add('begin');
  Add('  Result:=3;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestFunctionResult',
    LinesToStr([ // statements
    'this.Func1 = function () {',
    '  var Result = 0;',
    '  Result = 3;',
    '  return Result;',
    '};'
    ]),
    '');
end;

procedure TTestModule.TestNestedProc;
begin
  StartProgram(false);
  Add('function DoIt(pA,pD: longint): longint;');
  Add('var');
  Add('  vB: longint;');
  Add('  vC: longint;');
  Add('  function Nesty(pA: longint): longint; ');
  Add('  var vB: longint;');
  Add('  begin');
  Add('    Result:=pa+vb+vc+pd;');
  Add('  end;');
  Add('begin');
  Add('  Result:=pa+vb+vc;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestNestedProc',
    LinesToStr([ // statements
    'this.DoIt = function (pA, pD) {',
    '  var Result = 0;',
    '  var vB = 0;',
    '  var vC = 0;',
    '  function Nesty(pA) {',
    '    var Result = 0;',
    '    var vB = 0;',
    '    Result = ((pA + vB) + vC) + pD;',
    '    return Result;',
    '  };',
    '  Result = (pA + vB) + vC;',
    '  return Result;',
    '};'
    ]),
    '');
end;

procedure TTestModule.TestForwardProc;
begin
  StartProgram(false);
  Add('procedure FuncA(Bar: longint); forward;');
  Add('procedure FuncB(Bar: longint);');
  Add('begin');
  Add('  funca(bar);');
  Add('end;');
  Add('procedure funca(bar: longint);');
  Add('begin');
  Add('  if bar=3 then ;');
  Add('end;');
  Add('begin');
  Add('  funca(4);');
  Add('  funcb(5);');
  ConvertProgram;
  CheckSource('TestForwardProc',
    LinesToStr([ // statements'
    'this.FuncB = function (Bar) {',
    '  this.FuncA(Bar);',
    '};',
    'this.FuncA = function (Bar) {',
    '  if (Bar == 3);',
    '};'
    ]),
    LinesToStr([
    'this.FuncA(4);',
    'this.FuncB(5);'
    ])
    );
end;

procedure TTestModule.TestNestedForwardProc;
begin
  StartProgram(false);
  Add('procedure FuncA;');
  Add('  procedure FuncB(i: longint); forward;');
  Add('  procedure FuncC(i: longint);');
  Add('  begin');
  Add('    funcb(i);');
  Add('  end;');
  Add('  procedure FuncB(i: longint);');
  Add('  begin');
  Add('    if i=3 then ;');
  Add('  end;');
  Add('begin');
  Add('  funcc(4)');
  Add('end;');
  Add('begin');
  Add('  funca;');
  ConvertProgram;
  CheckSource('TestNestedForwardProc',
    LinesToStr([ // statements'
    'this.FuncA = function () {',
    '  function FuncC(i) {',
    '    FuncB(i);',
    '  };',
    '  function FuncB(i) {',
    '    if (i == 3);',
    '  };',
    '  FuncC(4);',
    '};'
    ]),
    LinesToStr([
    'this.FuncA();'
    ])
    );
end;

procedure TTestModule.TestAssignFunctionResult;
begin
  StartProgram(false);
  Add('function Func1: longint;');
  Add('begin');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  i:=func1();');
  Add('  i:=func1()+func1();');
  ConvertProgram;
  CheckSource('TestAssignFunctionResult',
    LinesToStr([ // statements
     'this.Func1 = function () {',
     '  var Result = 0;',
     '  return Result;',
     '};',
     'this.i = 0;'
    ]),
    LinesToStr([
    'this.i = this.Func1();',
    'this.i = this.Func1() + this.Func1();'
    ]));
end;

procedure TTestModule.TestFunctionResultInCondition;
begin
  StartProgram(false);
  Add('function Func1: longint;');
  Add('begin');
  Add('end;');
  Add('function Func2: boolean;');
  Add('begin');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  if func2 then ;');
  Add('  if i=func1() then ;');
  Add('  if i=func1 then ;');
  ConvertProgram;
  CheckSource('TestFunctionResultInCondition',
    LinesToStr([ // statements
     'this.Func1 = function () {',
     '  var Result = 0;',
     '  return Result;',
     '};',
     'this.Func2 = function () {',
     '  var Result = false;',
     '  return Result;',
     '};',
     'this.i = 0;'
    ]),
    LinesToStr([
    'if (this.Func2());',
    'if (this.i == this.Func1());',
    'if (this.i == this.Func1());'
    ]));
end;

procedure TTestModule.TestExit;
begin
  StartProgram(false);
  Add('procedure ProcA;');
  Add('begin');
  Add('  exit;');
  Add('end;');
  Add('function FuncB: longint;');
  Add('begin');
  Add('  exit;');
  Add('  exit(3);');
  Add('end;');
  Add('function FuncC: string;');
  Add('begin');
  Add('  exit;');
  Add('  exit(''a'');');
  Add('  exit(''abc'');');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestExit',
    LinesToStr([ // statements
    'this.ProcA = function () {',
    '  return;',
    '};',
    'this.FuncB = function () {',
    '  var Result = 0;',
    '  return Result;',
    '  return 3;',
    '  return Result;',
    '};',
    'this.FuncC = function () {',
    '  var Result = "";',
    '  return Result;',
    '  return "a";',
    '  return "abc";',
    '  return Result;',
    '};'
    ]),
    '');
end;

procedure TTestModule.TestBreak;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  repeat');
  Add('    break;');
  Add('  until true;');
  Add('  while true do');
  Add('    break;');
  Add('  for i:=1 to 2 do');
  Add('    break;');
  ConvertProgram;
  CheckSource('TestBreak',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([
    'do {',
    '  break;',
    '} while (!true);',
    'while (true) break;',
    'var $loopend1 = 2;',
    'for (this.i = 1; this.i <= $loopend1; this.i++) break;',
    'if (this.i > $loopend1) this.i--;'
    ]));
end;

procedure TTestModule.TestContinue;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  repeat');
  Add('    continue;');
  Add('  until true;');
  Add('  while true do');
  Add('    continue;');
  Add('  for i:=1 to 2 do');
  Add('    continue;');
  ConvertProgram;
  CheckSource('TestContinue',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([
    'do {',
    '  continue;',
    '} while (!true);',
    'while (true) continue;',
    'var $loopend1 = 2;',
    'for (this.i = 1; this.i <= $loopend1; this.i++) continue;',
    'if (this.i > $loopend1) this.i--;'
    ]));
end;

procedure TTestModule.TestProcedureExternal;
begin
  StartProgram(false);
  Add('procedure Foo; external name ''console.log'';');
  Add('function Bar: longint; external name ''get.item'';');
  Add('function Bla(s: string): longint; external name ''apply.something'';');
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  Foo;');
  Add('  i:=Bar;');
  Add('  i:=Bla(''abc'');');
  ConvertProgram;
  CheckSource('TestProcedureExternal',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([
    'console.log();',
    'this.i = get.item();',
    'this.i = apply.something("abc");'
    ]));
end;

procedure TTestModule.TestProcedureExternalOtherUnit;
begin
  AddModuleWithIntfImplSrc('unit2.pas',
    LinesToStr([
    'procedure Now; external name ''Date.now'';',
    'procedure DoIt;'
    ]),
    'procedure doit; begin end;');

  StartUnit(true);
  Add('interface');
  Add('uses unit2;');
  Add('implementation');
  Add('begin');
  Add('  now;');
  Add('  now();');
  Add('  uNit2.now;');
  Add('  uNit2.now();');
  Add('  test1.now;');
  Add('  test1.now();');
  Add('  doit;');
  Add('  uNit2.doit;');
  Add('  test1.doit;');
  ConvertUnit;
  CheckSource('TestProcedureExternalOtherUnit',
    LinesToStr([
    'var $impl = {',
    '};',
    'this.$impl = $impl;'
    ]),
    LinesToStr([
    'Date.now();',
    'Date.now();',
    'Date.now();',
    'Date.now();',
    'Date.now();',
    'Date.now();',
    'pas.unit2.DoIt();',
    'pas.unit2.DoIt();',
    'pas.unit2.DoIt();'
    ]));
end;

procedure TTestModule.TestProcedureAsm;
begin
  StartProgram(false);
  Add('function DoIt: longint;');
  Add('begin;');
  Add('  asm');
  Add('  { a:{ b:{}, c:[]}, d:''1'' };');
  Add('  end;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestProcedureAsm',
    LinesToStr([ // statements
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  { a:{ b:{}, c:[]}, d:''1'' };',
    '  return Result;',
    '};'
    ]),
    LinesToStr([
    ''
    ]));
end;

procedure TTestModule.TestProcedureAssembler;
begin
  StartProgram(false);
  Add('function DoIt: longint; assembler;');
  Add('asm');
  Add('{ a:{ b:{}, c:[]}, d:''1'' };');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestProcedureAssembler',
    LinesToStr([ // statements
    'this.DoIt = function () {',
    '  { a:{ b:{}, c:[]}, d:''1'' };',
    '};'
    ]),
    LinesToStr([
    ''
    ]));
end;

procedure TTestModule.TestProcedure_VarParam;
begin
  StartProgram(false);
  Add('type integer = longint;');
  Add('procedure DoIt(vG: integer; const vH: integer; var vI: integer);');
  Add('var vJ: integer;');
  Add('begin');
  Add('  vg:=vg+1;');
  Add('  vj:=vh+2;');
  Add('  vi:=vi+3;');
  Add('  doit(vg,vg,vg);');
  Add('  doit(vh,vh,vj);');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj,vj,vj);');
  Add('end;');
  Add('var i: integer;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestProcedure_VarParam',
    LinesToStr([ // statements
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = 0;',
    '  vG = vG + 1;',
    '  vJ = vH + 2;',
    '  vI.set(vI.get()+3);',
    '  this.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  this.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  this.DoIt(vI.get(), vI.get(), vI);',
    '  this.DoIt(vJ, vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = 0;'
    ]),
    LinesToStr([
    'this.DoIt(this.i,this.i,{',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestProcedureOverload;
begin
  StartProgram(false);
  Add('procedure DoIt(vI: longint); begin end;');
  Add('procedure DoIt(vI, vJ: longint); begin end;');
  Add('procedure DoIt(vD: double); begin end;');
  Add('begin');
  Add('  DoIt(1);');
  Add('  DoIt(2,3);');
  Add('  DoIt(4.5);');
  ConvertProgram;
  CheckSource('TestProcedureOverload',
    LinesToStr([ // statements
    'this.DoIt = function (vI) {',
    '};',
    'this.DoIt$1 = function (vI, vJ) {',
    '};',
    'this.DoIt$2 = function (vD) {',
    '};',
    '']),
    LinesToStr([
    'this.DoIt(1);',
    'this.DoIt$1(2, 3);',
    'this.DoIt$2(4.5);',
    '']));
end;

procedure TTestModule.TestProcedureOverloadForward;
begin
  StartProgram(false);
  Add('procedure DoIt(vI: longint); forward;');
  Add('procedure DoIt(vI, vJ: longint); begin end;');
  Add('procedure doit(vi: longint); begin end;');
  Add('begin');
  Add('  doit(1);');
  Add('  doit(2,3);');
  ConvertProgram;
  CheckSource('TestProcedureOverloadForward',
    LinesToStr([ // statements
    'this.DoIt$1 = function (vI, vJ) {',
    '};',
    'this.DoIt = function (vI) {',
    '};',
    '']),
    LinesToStr([
    'this.DoIt(1);',
    'this.DoIt$1(2, 3);',
    '']));
end;

procedure TTestModule.TestProcedureOverloadUnit;
begin
  StartUnit(false);
  Add('interface');
  Add('procedure DoIt(vI: longint);');
  Add('procedure DoIt(vI, vJ: longint);');
  Add('implementation');
  Add('procedure DoIt(vI, vJ, vK, vL, vM: longint); forward;');
  Add('procedure DoIt(vI, vJ, vK: longint); begin end;');
  Add('procedure DoIt(vi: longint); begin end;');
  Add('procedure DoIt(vI, vJ, vK, vL: longint); begin end;');
  Add('procedure DoIt(vi, vj: longint); begin end;');
  Add('procedure DoIt(vi, vj, vk, vl, vm: longint); begin end;');
  Add('begin');
  Add('  doit(1);');
  Add('  doit(2,3);');
  Add('  doit(4,5,6);');
  Add('  doit(7,8,9,10);');
  Add('  doit(11,12,13,14,15);');
  ConvertUnit;
  CheckSource('TestProcedureOverloadUnit',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    'this.DoIt = function (vI) {',
    '};',
    'this.DoIt$1 = function (vI, vJ) {',
    '};',
    '$impl.DoIt$3 = function (vI, vJ, vK) {',
    '};',
    '$impl.DoIt$4 = function (vI, vJ, vK, vL) {',
    '};',
    '$impl.DoIt$2 = function (vI, vJ, vK, vL, vM) {',
    '};',
    '']),
    LinesToStr([
    'this.DoIt(1);',
    'this.DoIt$1(2, 3);',
    '$impl.DoIt$3(4,5,6);',
    '$impl.DoIt$4(7,8,9,10);',
    '$impl.DoIt$2(11,12,13,14,15);',
    '']));
end;

procedure TTestModule.TestProcedureOverloadNested;
begin
  StartProgram(false);
  Add('procedure DoIt(vA: longint); forward;');
  Add('procedure DoIt(vB, vC: longint);');
  Add('begin // 2 param overload');
  Add('  doit(1);');
  Add('  doit(1,2);');
  Add('end;');
  Add('procedure doit(vA: longint);');
  Add('  procedure DoIt(vA, vB, vC: longint); forward;');
  Add('  procedure DoIt(vA, vB, vC, vD: longint);');
  Add('  begin // 4 param overload');
  Add('    doit(1);');
  Add('    doit(1,2);');
  Add('    doit(1,2,3);');
  Add('    doit(1,2,3,4);');
  Add('  end;');
  Add('  procedure doit(vA, vB, vC: longint);');
  Add('    procedure DoIt(vA, vB, vC, vD, vE: longint); forward;');
  Add('    procedure DoIt(vA, vB, vC, vD, vE, vF: longint);');
  Add('    begin // 6 param overload');
  Add('      doit(1);');
  Add('      doit(1,2);');
  Add('      doit(1,2,3);');
  Add('      doit(1,2,3,4);');
  Add('      doit(1,2,3,4,5);');
  Add('      doit(1,2,3,4,5,6);');
  Add('    end;');
  Add('    procedure doit(vA, vB, vC, vD, vE: longint);');
  Add('    begin // 5 param overload');
  Add('      doit(1);');
  Add('      doit(1,2);');
  Add('      doit(1,2,3);');
  Add('      doit(1,2,3,4);');
  Add('      doit(1,2,3,4,5);');
  Add('      doit(1,2,3,4,5,6);');
  Add('    end;');
  Add('  begin // 3 param overload');
  Add('    doit(1);');
  Add('    doit(1,2);');
  Add('    doit(1,2,3);');
  Add('    doit(1,2,3,4);');
  Add('    doit(1,2,3,4,5);');
  Add('    doit(1,2,3,4,5,6);');
  Add('  end;');
  Add('begin // 1 param overload');
  Add('  doit(1);');
  Add('  doit(1,2);');
  Add('  doit(1,2,3);');
  Add('  doit(1,2,3,4);');
  Add('end;');
  Add('begin // main');
  Add('  doit(1);');
  Add('  doit(1,2);');
  ConvertProgram;
  CheckSource('TestProcedureOverloadNested',
    LinesToStr([ // statements
    'this.DoIt$1 = function (vB, vC) {',
    '  this.DoIt(1);',
    '  this.DoIt$1(1, 2);',
    '};',
    'this.DoIt = function (vA) {',
    '  function DoIt$3(vA, vB, vC, vD) {',
    '    this.DoIt(1);',
    '    this.DoIt$1(1, 2);',
    '    DoIt$2(1, 2, 3);',
    '    DoIt$3(1, 2, 3, 4);',
    '  };',
    '  function DoIt$2(vA, vB, vC) {',
    '    function DoIt$5(vA, vB, vC, vD, vE, vF) {',
    '      this.DoIt(1);',
    '      this.DoIt$1(1, 2);',
    '      DoIt$2(1, 2, 3);',
    '      DoIt$3(1, 2, 3, 4);',
    '      DoIt$4(1, 2, 3, 4, 5);',
    '      DoIt$5(1, 2, 3, 4, 5, 6);',
    '    };',
    '    function DoIt$4(vA, vB, vC, vD, vE) {',
    '      this.DoIt(1);',
    '      this.DoIt$1(1, 2);',
    '      DoIt$2(1, 2, 3);',
    '      DoIt$3(1, 2, 3, 4);',
    '      DoIt$4(1, 2, 3, 4, 5);',
    '      DoIt$5(1, 2, 3, 4, 5, 6);',
    '    };',
    '    this.DoIt(1);',
    '    this.DoIt$1(1, 2);',
    '    DoIt$2(1, 2, 3);',
    '    DoIt$3(1, 2, 3, 4);',
    '    DoIt$4(1, 2, 3, 4, 5);',
    '    DoIt$5(1, 2, 3, 4, 5, 6);',
    '  };',
    '  this.DoIt(1);',
    '  this.DoIt$1(1, 2);',
    '  DoIt$2(1, 2, 3);',
    '  DoIt$3(1, 2, 3, 4);',
    '};',
    '']),
    LinesToStr([
    'this.DoIt(1);',
    'this.DoIt$1(1, 2);',
    '']));
end;

procedure TTestModule.TestProc_Varargs;
begin
  StartProgram(false);
  Add('procedure ProcA(i:longint); varargs; external name ''ProcA'';');
  Add('procedure ProcB; varargs; external name ''ProcB'';');
  Add('procedure ProcC(i: longint = 17); varargs; external name ''ProcC'';');
  Add('function GetIt: longint; begin end;');
  Add('begin');
  Add('  ProcA(1);');
  Add('  ProcA(1,2);');
  Add('  ProcA(1,2.0);');
  Add('  ProcA(1,2,3);');
  Add('  ProcA(1,''2'');');
  Add('  ProcA(2,'''');');
  Add('  ProcA(3,false);');
  Add('  ProcB;');
  Add('  ProcB();');
  Add('  ProcB(4);');
  Add('  ProcB(''foo'');');
  Add('  ProcC;');
  Add('  ProcC();');
  Add('  ProcC(4);');
  Add('  ProcC(5,''foo'');');
  Add('  ProcB(GetIt);');
  Add('  ProcB(GetIt());');
  Add('  ProcB(GetIt,GetIt());');
  ConvertProgram;
  CheckSource('TestProc_Varargs',
    LinesToStr([ // statements
    'this.GetIt = function () {',
    '  var Result = 0;',
    '  return Result;',
    '};',
    '']),
    LinesToStr([
    'ProcA(1);',
    'ProcA(1, 2);',
    'ProcA(1, 2.0);',
    'ProcA(1, 2, 3);',
    'ProcA(1, "2");',
    'ProcA(2, "");',
    'ProcA(3, false);',
    'ProcB();',
    'ProcB();',
    'ProcB(4);',
    'ProcB("foo");',
    'ProcC(17);',
    'ProcC(17);',
    'ProcC(4);',
    'ProcC(5, "foo");',
    'ProcB(this.GetIt());',
    'ProcB(this.GetIt());',
    'ProcB(this.GetIt(), this.GetIt());',
    '']));
end;

procedure TTestModule.TestEnumName;
begin
  StartProgram(false);
  Add('type TMyEnum = (Red, Green, Blue);');
  Add('var e: TMyEnum;');
  Add('var f: TMyEnum = Blue;');
  Add('begin');
  Add('  e:=green;');
  ConvertProgram;
  CheckSource('TestEnumName',
    LinesToStr([ // statements
    'this.TMyEnum = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1,',
    '  "2":"Blue",',
    '  Blue:2',
    '  };',
    'this.e = 0;',
    'this.f = this.TMyEnum.Blue;'
    ]),
    LinesToStr([
    'this.e=this.TMyEnum.Green;'
    ]));
end;

procedure TTestModule.TestEnumNumber;
begin
  Converter.Options:=Converter.Options+[coEnumNumbers];
  StartProgram(false);
  Add('type TMyEnum = (Red, Green);');
  Add('var');
  Add('  e: TMyEnum;');
  Add('  f: TMyEnum = Green;');
  Add('begin');
  Add('  e:=green;');
  ConvertProgram;
  CheckSource('TestEnumNumber',
    LinesToStr([ // statements
    'this.TMyEnum = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1',
    '  };',
    'this.e = 0;',
    'this.f = 1;'
    ]),
    LinesToStr([
    'this.e=1;'
    ]));
end;

procedure TTestModule.TestEnumFunctions;
begin
  StartProgram(false);
  Add('type TMyEnum = (Red, Green);');
  Add('var');
  Add('  e: TMyEnum;');
  Add('  i: longint;');
  Add('  s: string;');
  Add('begin');
  Add('  i:=ord(red);');
  Add('  i:=ord(green);');
  Add('  i:=ord(e);');
  Add('  e:=low(tmyenum);');
  Add('  e:=low(e);');
  Add('  e:=high(tmyenum);');
  Add('  e:=high(e);');
  Add('  e:=pred(green);');
  Add('  e:=pred(e);');
  Add('  e:=succ(red);');
  Add('  e:=succ(e);');
  Add('  e:=tmyenum(1);');
  Add('  e:=tmyenum(i);');
  Add('  s:=str(e);');
  Add('  str(e,s)');
  Add('  s:=str(e:3);');
  ConvertProgram;
  CheckSource('TestEnumNumber',
    LinesToStr([ // statements
    'this.TMyEnum = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1',
    '  };',
    'this.e = 0;',
    'this.i = 0;',
    'this.s = "";'
    ]),
    LinesToStr([
    'this.i=this.TMyEnum.Red;',
    'this.i=this.TMyEnum.Green;',
    'this.i=this.e;',
    'this.e=this.TMyEnum.Red;',
    'this.e=this.TMyEnum.Red;',
    'this.e=this.TMyEnum.Green;',
    'this.e=this.TMyEnum.Green;',
    'this.e=this.TMyEnum.Green-1;',
    'this.e=this.e-1;',
    'this.e=this.TMyEnum.Red+1;',
    'this.e=this.e+1;',
    'this.e=1;',
    'this.e=this.i;',
    'this.s = this.TMyEnum[this.e];',
    'this.s = this.TMyEnum[this.e];',
    'this.s = rtl.spaceLeft(this.TMyEnum[this.e], 3);',
    '']));
end;

procedure TTestModule.TestSet;
begin
  StartProgram(false);
  Add('type');
  Add('  TColor = (Red, Green, Blue);');
  Add('  TColors = set of TColor;');
  Add('var');
  Add('  c: TColor;');
  Add('  s: TColors;');
  Add('  t: TColors = [];');
  Add('  u: TColors = [Red];');
  Add('begin');
  Add('  s:=[];');
  Add('  s:=[Green];');
  Add('  s:=[Green,Blue];');
  Add('  s:=[Red..Blue];');
  Add('  s:=[Red,Green..Blue];');
  Add('  s:=[Red,c];');
  Add('  s:=t;');
  ConvertProgram;
  CheckSource('TestEnumName',
    LinesToStr([ // statements
    'this.TColor = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1,',
    '  "2":"Blue",',
    '  Blue:2',
    '  };',
    'this.c = 0;',
    'this.s = {};',
    'this.t = {};',
    'this.u = rtl.createSet(this.TColor.Red);'
    ]),
    LinesToStr([
    'this.s={};',
    'this.s=rtl.createSet(this.TColor.Green);',
    'this.s=rtl.createSet(this.TColor.Green,this.TColor.Blue);',
    'this.s=rtl.createSet(null,this.TColor.Red,this.TColor.Blue);',
    'this.s=rtl.createSet(this.TColor.Red,null,this.TColor.Green,this.TColor.Blue);',
    'this.s=rtl.createSet(this.TColor.Red,this.c);',
    'this.s=rtl.refSet(this.t);',
    '']));
end;

procedure TTestModule.TestSetOperators;
begin
  StartProgram(false);
  Add('type');
  Add('  TColor = (Red, Green, Blue);');
  Add('  TColors = set of tcolor;');
  Add('var');
  Add('  vC: TColor;');
  Add('  vS: TColors;');
  Add('  vT: TColors;');
  Add('  vU: TColors;');
  Add('  B: boolean;');
  Add('begin');
  Add('  include(vs,green);');
  Add('  exclude(vs,vc);');
  Add('  vs:=vt+vu;');
  Add('  vs:=vt+[red];');
  Add('  vs:=[red]+vt;');
  Add('  vs:=[red]+[green];');
  Add('  vs:=vt-vu;');
  Add('  vs:=vt-[red];');
  Add('  vs:=[red]-vt;');
  Add('  vs:=[red]-[green];');
  Add('  vs:=vt*vu;');
  Add('  vs:=vt*[red];');
  Add('  vs:=[red]*vt;');
  Add('  vs:=[red]*[green];');
  Add('  vs:=vt><vu;');
  Add('  vs:=vt><[red];');
  Add('  vs:=[red]><vt;');
  Add('  vs:=[red]><[green];');
  Add('  b:=vt=vu;');
  Add('  b:=vt=[red];');
  Add('  b:=[red]=vt;');
  Add('  b:=[red]=[green];');
  Add('  b:=vt<>vu;');
  Add('  b:=vt<>[red];');
  Add('  b:=[red]<>vt;');
  Add('  b:=[red]<>[green];');
  Add('  b:=vt<=vu;');
  Add('  b:=vt<=[red];');
  Add('  b:=[red]<=vt;');
  Add('  b:=[red]<=[green];');
  Add('  b:=vt>=vu;');
  Add('  b:=vt>=[red];');
  Add('  b:=[red]>=vt;');
  Add('  b:=[red]>=[green];');
  Add('  b:=Red in vt;');
  Add('  b:=vc in vt;');
  Add('  b:=Green in [Red..Blue];');
  Add('  b:=vc in [Red..Blue];');
  ConvertProgram;
  CheckSource('TestEnumName',
    LinesToStr([ // statements
    'this.TColor = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1,',
    '  "2":"Blue",',
    '  Blue:2',
    '  };',
    'this.vC = 0;',
    'this.vS = {};',
    'this.vT = {};',
    'this.vU = {};',
    'this.B = false;'
    ]),
    LinesToStr([
    'this.vS = rtl.includeSet(this.vS,this.TColor.Green);',
    'this.vS = rtl.excludeSet(this.vS,this.vC);',
    'this.vS = rtl.unionSet(this.vT, this.vU);',
    'this.vS = rtl.unionSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.vS = rtl.unionSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.vS = rtl.unionSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.vS = rtl.diffSet(this.vT, this.vU);',
    'this.vS = rtl.diffSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.vS = rtl.diffSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.vS = rtl.diffSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.vS = rtl.intersectSet(this.vT, this.vU);',
    'this.vS = rtl.intersectSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.vS = rtl.intersectSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.vS = rtl.intersectSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.vS = rtl.symDiffSet(this.vT, this.vU);',
    'this.vS = rtl.symDiffSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.vS = rtl.symDiffSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.vS = rtl.symDiffSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.B = rtl.eqSet(this.vT, this.vU);',
    'this.B = rtl.eqSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.B = rtl.eqSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.B = rtl.eqSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.B = rtl.neSet(this.vT, this.vU);',
    'this.B = rtl.neSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.B = rtl.neSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.B = rtl.neSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.B = rtl.leSet(this.vT, this.vU);',
    'this.B = rtl.leSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.B = rtl.leSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.B = rtl.leSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.B = rtl.geSet(this.vT, this.vU);',
    'this.B = rtl.geSet(this.vT, rtl.createSet(this.TColor.Red));',
    'this.B = rtl.geSet(rtl.createSet(this.TColor.Red), this.vT);',
    'this.B = rtl.geSet(rtl.createSet(this.TColor.Red), rtl.createSet(this.TColor.Green));',
    'this.B = this.vT[this.TColor.Red];',
    'this.B = this.vT[this.vC];',
    'this.B = rtl.createSet(null, this.TColor.Red, this.TColor.Blue)[this.TColor.Green];',
    'this.B = rtl.createSet(null, this.TColor.Red, this.TColor.Blue)[this.vC];',
    '']));
end;

procedure TTestModule.TestSetFunctions;
begin
  StartProgram(false);
  Add('type');
  Add('  TMyEnum = (Red, Green);');
  Add('  TMyEnums = set of TMyEnum;');
  Add('var');
  Add('  e: TMyEnum;');
  Add('  s: TMyEnums;');
  Add('begin');
  Add('  e:=Low(TMyEnums);');
  Add('  e:=Low(s);');
  Add('  e:=High(TMyEnums);');
  Add('  e:=High(s);');
  ConvertProgram;
  CheckSource('TestSetFunctions',
    LinesToStr([ // statements
    'this.TMyEnum = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1',
    '  };',
    'this.e = 0;',
    'this.s = {};'
    ]),
    LinesToStr([
    'this.e=this.TMyEnum.Red;',
    'this.e=this.TMyEnum.Red;',
    'this.e=this.TMyEnum.Green;',
    'this.e=this.TMyEnum.Green;',
    '']));
end;

procedure TTestModule.TestSet_PassAsArgClone;
begin
  StartProgram(false);
  Add('type');
  Add('  TMyEnum = (Red, Green);');
  Add('  TMyEnums = set of TMyEnum;');
  Add('procedure DoDefault(s: tmyenums); begin end;');
  Add('procedure DoConst(const s: tmyenums); begin end;');
  Add('var');
  Add('  aSet: tmyenums;');
  Add('begin');
  Add('  dodefault(aset);');
  Add('  doconst(aset);');
  ConvertProgram;
  CheckSource('TestSetFunctions',
    LinesToStr([ // statements
    'this.TMyEnum = {',
    '  "0":"Red",',
    '  Red:0,',
    '  "1":"Green",',
    '  Green:1',
    '  };',
    'this.DoDefault = function (s) {',
    '};',
    'this.DoConst = function (s) {',
    '};',
    'this.aSet = {};'
    ]),
    LinesToStr([
    'this.DoDefault(rtl.refSet(this.aSet));',
    'this.DoConst(this.aSet);',
    '']));
end;

procedure TTestModule.TestEnum_AsParams;
begin
  StartProgram(false);
  Add('type TEnum = (Red,Blue);');
  Add('procedure DoIt(vG: TEnum; const vH: TEnum; var vI: TEnum);');
  Add('var vJ: TEnum;');
  Add('begin');
  Add('  vg:=vg;');
  Add('  vj:=vh;');
  Add('  vi:=vi;');
  Add('  doit(vg,vg,vg);');
  Add('  doit(vh,vh,vj);');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj,vj,vj);');
  Add('end;');
  Add('var i: TEnum;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestEnum_AsParams',
    LinesToStr([ // statements
    'this.TEnum = {',
    '  "0": "Red",',
    '  Red: 0,',
    '  "1": "Blue",',
    '  Blue: 1',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = 0;',
    '  vG = vG;',
    '  vJ = vH;',
    '  vI.set(vI.get());',
    '  this.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  this.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  this.DoIt(vI.get(), vI.get(), vI);',
    '  this.DoIt(vJ, vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = 0;'
    ]),
    LinesToStr([
    'this.DoIt(this.i,this.i,{',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestSet_AsParams;
begin
  StartProgram(false);
  Add('type TEnum = (Red,Blue);');
  Add('type TEnums = set of TEnum;');
  Add('procedure DoIt(vG: TEnums; const vH: TEnums; var vI: TEnums);');
  Add('var vJ: TEnums;');
  Add('begin');
  Add('  vg:=vg;');
  Add('  vj:=vh;');
  Add('  vi:=vi;');
  Add('  doit(vg,vg,vg);');
  Add('  doit(vh,vh,vj);');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj,vj,vj);');
  Add('end;');
  Add('var i: TEnums;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestSet_AsParams',
    LinesToStr([ // statements
    'this.TEnum = {',
    '  "0": "Red",',
    '  Red: 0,',
    '  "1": "Blue",',
    '  Blue: 1',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = {};',
    '  vG = rtl.refSet(vG);',
    '  vJ = rtl.refSet(vH);',
    '  vI.set(rtl.refSet(vI.get()));',
    '  this.DoIt(rtl.refSet(vG), vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  this.DoIt(rtl.refSet(vH), vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  this.DoIt(rtl.refSet(vI.get()), vI.get(), vI);',
    '  this.DoIt(rtl.refSet(vJ), vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = {};'
    ]),
    LinesToStr([
    'this.DoIt(rtl.refSet(this.i),this.i,{',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestSet_Property;
begin
  StartProgram(false);
  Add('type');
  Add('  TEnum = (Red,Blue);');
  Add('  TEnums = set of TEnum;');
  Add('  TObject = class');
  Add('    function GetColors: TEnums; external name ''GetColors'';');
  Add('    procedure SetColors(const Value: TEnums); external name ''SetColors'';');
  Add('    property Colors: TEnums read GetColors write SetColors;');
  Add('  end;');
  Add('procedure DoIt(i: TEnums; const j: TEnums; var k: TEnums; out l: TEnums);');
  Add('begin end;');
  Add('var Obj: TObject;');
  Add('begin');
  Add('  Include(Obj.Colors,Red);');
  Add('  Exclude(Obj.Colors,Red);');
  //Add('  DoIt(Obj.Colors,Obj.Colors,Obj.Colors,Obj.Colors);');
  ConvertProgram;
  CheckSource('TestSet_Property',
    LinesToStr([ // statements
    'this.TEnum = {',
    '  "0": "Red",',
    '  Red: 0,',
    '  "1": "Blue",',
    '  Blue: 1',
    '};',
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.DoIt = function (i, j, k, l) {',
    '};',
    'this.Obj = null;',
    '']),
    LinesToStr([
    'this.Obj.SetColors(rtl.includeSet(this.Obj.GetColors(), this.TEnum.Red));',
    'this.Obj.SetColors(rtl.excludeSet(this.Obj.GetColors(), this.TEnum.Red));',
    '']));
end;

procedure TTestModule.TestNestBegin;
begin
  StartProgram(false);
  Add('begin');
  Add('  begin');
  Add('    begin');
  Add('    end;');
  Add('    begin');
  Add('      if true then ;');
  Add('    end;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestNestBegin',
    '',
    'if (true) ;');
end;

procedure TTestModule.TestUnitImplVars;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  Add('var');
  Add('  V1:longint;');
  Add('  V2:longint = 3;');
  Add('  V3:string = ''abc'';');
  ConvertUnit;
  CheckSource('TestUnitImplVars',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '$impl.V1 = 0;',
    '$impl.V2 = 3;',
    '$impl.V3 = "abc";'
    ]),
    '');
end;

procedure TTestModule.TestUnitImplConsts;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  Add('const');
  Add('  v1 = 3;');
  Add('  v2:longint = 4;');
  Add('  v3:string = ''abc'';');
  ConvertUnit;
  CheckSource('TestUnitImplConsts',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '$impl.v1 = 3;',
    '$impl.v2 = 4;',
    '$impl.v3 = "abc";'
    ]),
    '');
end;

procedure TTestModule.TestUnitImplRecord;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  Add('type');
  Add('  TMyRecord = record');
  Add('    i: longint;');
  Add('  end;');
  Add('var aRec: TMyRecord;');
  Add('initialization');
  Add('  arec.i:=3;');
  ConvertUnit;
  CheckSource('TestUnitImplRecord',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '$impl.TMyRecord = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    '$impl.aRec = new $impl.TMyRecord();'
    ]),
    '$impl.aRec.i = 3;'
    );
end;

procedure TTestModule.TestRenameJSNameConflict;
begin
  StartProgram(false);
  Add('var apply: longint;');
  Add('var bind: longint;');
  Add('var call: longint;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRenameJSNameConflict',
    LinesToStr([ // statements
    'this.Apply = 0;',
    'this.Bind = 0;',
    'this.Call = 0;'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestLocalConst;
begin
  StartProgram(false);
  Add('procedure DoIt;');
  Add('const');
  Add('  cA: longint = 1;');
  Add('  cB = 2;');
  Add('  procedure Sub;');
  Add('  const');
  Add('    csA = 3;');
  Add('    cB: double = 4;');
  Add('  begin');
  Add('    cb:=cb+csa;');
  Add('    ca:=ca+csa+5;');
  Add('  end;');
  Add('begin');
  Add('  ca:=ca+cb+6;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestLocalConst',
    LinesToStr([
    'var cA = 1;',
    'var cB = 2;',
    'var csA = 3;',
    'var cB$1 = 4;',
    'this.DoIt = function () {',
    '  function Sub() {',
    '    cB$1 = cB$1 + csA;',
    '    cA = (cA + csA) + 5;',
    '  };',
    '  cA = (cA + cB) + 6;',
    '};'
    ]),
    LinesToStr([
    ]));
end;

procedure TTestModule.TestVarExternal;
begin
  StartProgram(false);
  Add('var');
  Add('  NaN: double; external name ''Global.NaN'';');
  Add('  d: double;');
  Add('begin');
  Add('  d:=NaN;');
  ConvertProgram;
  CheckSource('TestVarExternal',
    LinesToStr([
    'this.d = 0.0;'
    ]),
    LinesToStr([
    'this.d = Global.NaN;'
    ]));
end;

procedure TTestModule.TestVarExternalOtherUnit;
begin
  AddModuleWithIntfImplSrc('unit2.pas',
    LinesToStr([
    'var NaN: double; external name ''Global.NaN'';',
    'var iV: longint;'
    ]),
    '');

  StartUnit(true);
  Add('interface');
  Add('uses unit2;');
  Add('implementation');
  Add('var');
  Add('  d: double;');
  Add('  i: longint;');
  Add('begin');
  Add('  d:=nan;');
  Add('  d:=uNit2.nan;');
  Add('  d:=test1.nan;');
  Add('  i:=iv;');
  Add('  i:=uNit2.iv;');
  Add('  i:=test1.iv;');
  ConvertUnit;
  CheckSource('TestVarExternalOtherUnit',
    LinesToStr([
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '$impl.d = 0.0;',
    '$impl.i = 0;',
    '']),
    LinesToStr([
    '$impl.d = Global.NaN;',
    '$impl.d = Global.NaN;',
    '$impl.d = Global.NaN;',
    '$impl.i = pas.unit2.iV;',
    '$impl.i = pas.unit2.iV;',
    '$impl.i = pas.unit2.iV;',
    '']));
end;

procedure TTestModule.TestCharConst;
begin
  StartProgram(false);
  Add('const');
  Add('  c: char = ''1'';');
  Add('begin');
  Add('  c:=#0;');
  Add('  c:=#1;');
  Add('  c:=#9;');
  Add('  c:=#10;');
  Add('  c:=#13;');
  Add('  c:=#31;');
  Add('  c:=#32;');
  Add('  c:=#$A;');
  Add('  c:=#$0A;');
  Add('  c:=#$b;');
  Add('  c:=#$0b;');
  Add('  c:=^A;');
  Add('  c:=''"'';');
  ConvertProgram;
  CheckSource('TestCharConst',
    LinesToStr([
    'this.c="1";'
    ]),
    LinesToStr([
    'this.c="\x00";',
    'this.c="\x01";',
    'this.c="\t";',
    'this.c="\n";',
    'this.c="\r";',
    'this.c="\x1F";',
    'this.c=" ";',
    'this.c="\n";',
    'this.c="\n";',
    'this.c="\x0B";',
    'this.c="\x0B";',
    'this.c="\x01";',
    'this.c=''"'';'
    ]));
end;

procedure TTestModule.TestChar_Compare;
begin
  StartProgram(false);
  Add('var');
  Add('  c: char;');
  Add('  b: boolean;');
  Add('begin');
  Add('  b:=c=''1'';');
  Add('  b:=''2''=c;');
  Add('  b:=''3''=''4'';');
  Add('  b:=c<>''5'';');
  Add('  b:=''6''<>c;');
  Add('  b:=c>''7'';');
  Add('  b:=''8''>c;');
  Add('  b:=c>=''9'';');
  Add('  b:=''A''>=c;');
  Add('  b:=c<''B'';');
  Add('  b:=''C''<c;');
  Add('  b:=c<=''D'';');
  Add('  b:=''E''<=c;');
  ConvertProgram;
  CheckSource('TestChar_Compare',
    LinesToStr([
    'this.c="";',
    'this.b = false;'
    ]),
    LinesToStr([
    'this.b = this.c == "1";',
    'this.b = "2" == this.c;',
    'this.b = "3" == "4";',
    'this.b = this.c != "5";',
    'this.b = "6" != this.c;',
    'this.b = this.c > "7";',
    'this.b = "8" > this.c;',
    'this.b = this.c >= "9";',
    'this.b = "A" >= this.c;',
    'this.b = this.c < "B";',
    'this.b = "C" < this.c;',
    'this.b = this.c <= "D";',
    'this.b = "E" <= this.c;',
    '']));
end;

procedure TTestModule.TestChar_Ord;
begin
  StartProgram(false);
  Add('var');
  Add('  c: char;');
  Add('  i: longint;');
  Add('begin');
  Add('  i:=ord(c);');
  ConvertProgram;
  CheckSource('TestChar_Ord',
    LinesToStr([
    'this.c = "";',
    'this.i = 0;'
    ]),
    LinesToStr([
    'this.i = this.c.charCodeAt();',
    '']));
end;

procedure TTestModule.TestChar_Chr;
begin
  StartProgram(false);
  Add('var');
  Add('  c: char;');
  Add('  i: longint;');
  Add('begin');
  Add('  c:=chr(i);');
  ConvertProgram;
  CheckSource('TestChar_Chr',
    LinesToStr([
    'this.c = "";',
    'this.i = 0;'
    ]),
    LinesToStr([
    'this.c = String.fromCharCode(this.i);',
    '']));
end;

procedure TTestModule.TestStringConst;
begin
  StartProgram(false);
  Add('var');
  Add('  s: string = ''abc'';');
  Add('begin');
  Add('  s:='''';');
  Add('  s:=#13#10;');
  Add('  s:=#9''foo'';');
  Add('  s:=#$A9;');
  Add('  s:=''foo''#13''bar'';');
  Add('  s:=''"'';');
  Add('  s:=''"''''"'';');
  ConvertProgram;
  CheckSource('TestStringConst',
    LinesToStr([
    'this.s="abc";'
    ]),
    LinesToStr([
    'this.s="";',
    'this.s="\r\n";',
    'this.s="\tfoo";',
    'this.s="©";',
    'this.s="foo\rbar";',
    'this.s=''"'';',
    'this.s=''"\''"'';'
    ]));
end;

procedure TTestModule.TestString_Length;
begin
  StartProgram(false);
  Add('const c = ''foo'';');
  Add('var');
  Add('  s: string;');
  Add('  i: longint;');
  Add('begin');
  Add('  i:=length(s);');
  Add('  i:=length(s+s);');
  Add('  i:=length(''abc'');');
  Add('  i:=length(c);');
  ConvertProgram;
  CheckSource('TestString_Length',
    LinesToStr([
    'this.c = "foo";',
    'this.s = "";',
    'this.i = 0;',
    '']),
    LinesToStr([
    'this.i = this.s.length;',
    'this.i = (this.s+this.s).length;',
    'this.i = "abc".length;',
    'this.i = this.c.length;',
    '']));
end;

procedure TTestModule.TestString_Compare;
begin
  StartProgram(false);
  Add('var');
  Add('  s, t: string;');
  Add('  b: boolean;');
  Add('begin');
  Add('  b:=s=t;');
  Add('  b:=s<>t;');
  Add('  b:=s>t;');
  Add('  b:=s>=t;');
  Add('  b:=s<t;');
  Add('  b:=s<=t;');
  ConvertProgram;
  CheckSource('TestString_Compare',
    LinesToStr([ // statements
    'this.s = "";',
    'this.t = "";',
    'this.b =false;'
    ]),
    LinesToStr([ // this.$main
    'this.b = this.s == this.t;',
    'this.b = this.s != this.t;',
    'this.b = this.s > this.t;',
    'this.b = this.s >= this.t;',
    'this.b = this.s < this.t;',
    'this.b = this.s <= this.t;',
    '']));
end;

procedure TTestModule.TestString_SetLength;
begin
  StartProgram(false);
  Add('var s: string;');
  Add('begin');
  Add('  SetLength(s,3);');
  ConvertProgram;
  CheckSource('TestString_SetLength',
    LinesToStr([ // statements
    'this.s = "";'
    ]),
    LinesToStr([ // this.$main
    'this.s.length = 3;'
    ]));
end;

procedure TTestModule.TestString_CharAt;
begin
  StartProgram(false);
  Add('var');
  Add('  s: string;');
  Add('  c: char;');
  Add('  b: boolean;');
  Add('begin');
  Add('  b:= s[1] = c;');
  Add('  b:= c = s[1];');
  Add('  b:= c <> s[1];');
  Add('  b:= c > s[1];');
  Add('  b:= c >= s[1];');
  Add('  b:= c < s[1];');
  Add('  b:= c <= s[1];');
  Add('  s[1] := c;');
  ConvertProgram;
  CheckSource('TestString_CharAt',
    LinesToStr([ // statements
    'this.s = "";',
    'this.c = "";',
    'this.b = false;'
    ]),
    LinesToStr([ // this.$main
    'this.b = this.s.charAt(1-1) == this.c;',
    'this.b = this.c == this.s.charAt(1 - 1);',
    'this.b = this.c != this.s.charAt(1 - 1);',
    'this.b = this.c > this.s.charAt(1 - 1);',
    'this.b = this.c >= this.s.charAt(1 - 1);',
    'this.b = this.c < this.s.charAt(1 - 1);',
    'this.b = this.c <= this.s.charAt(1 - 1);',
    'this.s = rtl.setCharAt(this.s, 1, this.c);',
    '']));
end;

procedure TTestModule.TestStr;
begin
  StartProgram(false);
  Add('var');
  Add('  b: boolean;');
  Add('  i: longint;');
  Add('  d: double;');
  Add('  s: string;');
  Add('begin');
  Add('  str(b,s);');
  Add('  str(i,s);');
  Add('  str(d,s);');
  Add('  str(i:3,s);');
  Add('  str(d:3:2,s);');
  Add('  s:=str(b);');
  Add('  s:=str(i);');
  Add('  s:=str(d);');
  Add('  s:=str(i,i);');
  Add('  s:=str(i:3);');
  Add('  s:=str(d:3:2);');
  Add('  s:=str(i:4,i);');
  Add('  s:=str(i,i:5);');
  Add('  s:=str(i:4,i:5);');
  Add('  s:=str(s,s);');
  Add('  s:=str(s,''foo'');');
  ConvertProgram;
  CheckSource('TestStr',
    LinesToStr([ // statements
    'this.b = false;',
    'this.i = 0;',
    'this.d = 0.0;',
    'this.s = "";',
    '']),
    LinesToStr([ // this.$main
    'this.s = ""+this.b;',
    'this.s = ""+this.i;',
    'this.s = ""+this.d;',
    'this.s = rtl.spaceLeft(""+this.i,3);',
    'this.s = rtl.spaceLeft(this.d.toFixed(2),3);',
    'this.s = ""+this.b;',
    'this.s = ""+this.i;',
    'this.s = ""+this.d;',
    'this.s = (""+this.i)+this.i;',
    'this.s = rtl.spaceLeft(""+this.i,3);',
    'this.s = rtl.spaceLeft(this.d.toFixed(2),3);',
    'this.s = rtl.spaceLeft("" + this.i, 4) + this.i;',
    'this.s = ("" + this.i) + rtl.spaceLeft("" + this.i, 5);',
    'this.s = rtl.spaceLeft("" + this.i, 4) + rtl.spaceLeft("" + this.i, 5);',
    'this.s = this.s + this.s;',
    'this.s = this.s + "foo";',
    '']));
end;

procedure TTestModule.TestBaseType_AnsiStringFail;
begin
  StartProgram(false);
  Add('var s: AnsiString');
  SetExpectedPasResolverError('identifier not found "AnsiString"',nIdentifierNotFound);
  ConvertProgram;
end;

procedure TTestModule.TestBaseType_UnicodeStringFail;
begin
  StartProgram(false);
  Add('var s: UnicodeString');
  SetExpectedPasResolverError('identifier not found "UnicodeString"',nIdentifierNotFound);
  ConvertProgram;
end;

procedure TTestModule.TestBaseType_ShortStringFail;
begin
  StartProgram(false);
  Add('var s: ShortString');
  SetExpectedPasResolverError('identifier not found "ShortString"',nIdentifierNotFound);
  ConvertProgram;
end;

procedure TTestModule.TestBaseType_RawByteStringFail;
begin
  StartProgram(false);
  Add('var s: RawByteString');
  SetExpectedPasResolverError('identifier not found "RawByteString"',nIdentifierNotFound);
  ConvertProgram;
end;

procedure TTestModule.TestProcTwoArgs;
begin
  StartProgram(false);
  Add('procedure Test(a,b: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestProcTwoArgs',
    LinesToStr([ // statements
    'this.Test = function (a,b) {',
    '};'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestProc_DefaultValue;
begin
  StartProgram(false);
  Add('procedure p1(i: longint = 1);');
  Add('begin');
  Add('end;');
  Add('procedure p2(i: longint = 1; c: char = ''a'');');
  Add('begin');
  Add('end;');
  Add('procedure p3(d: double = 1.0; b: boolean = false; s: string = ''abc'');');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  p1;');
  Add('  p1();');
  Add('  p1(11);');
  Add('  p2;');
  Add('  p2();');
  Add('  p2(12);');
  Add('  p2(13,''b'');');
  Add('  p3();');
  ConvertProgram;
  CheckSource('TestProc_DefaultValue',
    LinesToStr([ // statements
    'this.p1 = function (i) {',
    '};',
    'this.p2 = function (i,c) {',
    '};',
    'this.p3 = function (d,b,s) {',
    '};'
    ]),
    LinesToStr([ // this.$main
    '  this.p1(1);',
    '  this.p1(1);',
    '  this.p1(11);',
    '  this.p2(1,"a");',
    '  this.p2(1,"a");',
    '  this.p2(12,"a");',
    '  this.p2(13,"b");',
    '  this.p3(1.0,false,"abc");'
    ]));
end;

procedure TTestModule.TestFunctionInt;
begin
  StartProgram(false);
  Add('function MyTest(Bar: longint): longint;');
  Add('begin');
  Add('  Result:=2*bar');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestFunctionInt',
    LinesToStr([ // statements
    'this.MyTest = function (Bar) {',
    '  var Result = 0;',
    '  Result = 2*Bar;',
    '  return Result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestFunctionString;
begin
  StartProgram(false);
  Add('function Test(Bar: string): string;');
  Add('begin');
  Add('  Result:=bar+BAR');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestFunctionString',
    LinesToStr([ // statements
    'this.Test = function (Bar) {',
    '  var Result = "";',
    '  Result = Bar+Bar;',
    '  return Result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestForLoop;
begin
  StartProgram(false);
  Add('var');
  Add('  vI, vJ, vN: longint;');
  Add('begin');
  Add('  VJ:=0;');
  Add('  VN:=3;');
  Add('  for VI:=1 to VN do');
  Add('  begin');
  Add('    VJ:=VJ+VI;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestForLoop',
    LinesToStr([ // statements
    'this.vI = 0;',
    'this.vJ = 0;',
    'this.vN = 0;'
    ]),
    LinesToStr([ // this.$main
    '  this.vJ = 0;',
    '  this.vN = 3;',
    '  var $loopend1 = this.vN;',
    '  for (this.vI = 1; this.vI <= $loopend1; this.vI++) {',
    '    this.vJ = this.vJ + this.vI;',
    '  };',
    '  if (this.vI > $loopend1) this.vI--;'
    ]));
end;

procedure TTestModule.TestForLoopInFunction;
begin
  StartProgram(false);
  Add('function SumNumbers(Count: longint): longint;');
  Add('var');
  Add('  vI, vJ: longint;');
  Add('begin');
  Add('  vj:=0;');
  Add('  for vi:=1 to count do');
  Add('  begin');
  Add('    vj:=vj+vi;');
  Add('  end;');
  Add('end;');
  Add('begin');
  Add('  sumnumbers(3);');
  ConvertProgram;
  CheckSource('TestForLoopInFunction',
    LinesToStr([ // statements
    'this.SumNumbers = function (Count) {',
    '  var Result = 0;',
    '  var vI = 0;',
    '  var vJ = 0;',
    '  vJ = 0;',
    '  var $loopend1 = Count;',
    '  for (vI = 1; vI <= $loopend1; vI++) {',
    '    vJ = vJ + vI;',
    '  };',
    '  return Result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    '  this.SumNumbers(3);'
    ]));
end;

procedure TTestModule.TestForLoop_ReadVarAfter;
begin
  StartProgram(false);
  Add('var');
  Add('  vI: longint;');
  Add('begin');
  Add('  for vi:=1 to 2 do ;');
  Add('  if vi=3 then ;');
  ConvertProgram;
  CheckSource('TestForLoop',
    LinesToStr([ // statements
    'this.vI = 0;'
    ]),
    LinesToStr([ // this.$main
    '  var $loopend1 = 2;',
    '  for (this.vI = 1; this.vI <= $loopend1; this.vI++);',
    '  if(this.vI>$loopend1)this.vI--;',
    '  if (this.vI==3) ;'
    ]));
end;

procedure TTestModule.TestForLoop_Nested;
begin
  StartProgram(false);
  Add('function SumNumbers(Count: longint): longint;');
  Add('var');
  Add('  vI, vJ, vK: longint;');
  Add('begin');
  Add('  VK:=0;');
  Add('  for VI:=1 to count do');
  Add('  begin');
  Add('    for vj:=1 to vi do');
  Add('    begin');
  Add('      vk:=VK+VI;');
  Add('    end;');
  Add('  end;');
  Add('end;');
  Add('begin');
  Add('  sumnumbers(3);');
  ConvertProgram;
  CheckSource('TestForLoopInFunction',
    LinesToStr([ // statements
    'this.SumNumbers = function (Count) {',
    '  var Result = 0;',
    '  var vI = 0;',
    '  var vJ = 0;',
    '  var vK = 0;',
    '  vK = 0;',
    '  var $loopend1 = Count;',
    '  for (vI = 1; vI <= $loopend1; vI++) {',
    '    var $loopend2 = vI;',
    '    for (vJ = 1; vJ <= $loopend2; vJ++) {',
    '      vK = vK + vI;',
    '    };',
    '  };',
    '  return Result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    '  this.SumNumbers(3);'
    ]));
end;

procedure TTestModule.TestRepeatUntil;
begin
  StartProgram(false);
  Add('var');
  Add('  vI, vJ, vN: longint;');
  Add('begin');
  Add('  vn:=3;');
  Add('  vj:=0;');
  Add('  VI:=0;');
  Add('  repeat');
  Add('    VI:=vi+1;');
  Add('    vj:=VJ+vI;');
  Add('  until vi>=vn');
  ConvertProgram;
  CheckSource('TestRepeatUntil',
    LinesToStr([ // statements
    'this.vI = 0;',
    'this.vJ = 0;',
    'this.vN = 0;'
    ]),
    LinesToStr([ // this.$main
    '  this.vN = 3;',
    '  this.vJ = 0;',
    '  this.vI = 0;',
    '  do{',
    '    this.vI = this.vI + 1;',
    '    this.vJ = this.vJ + this.vI;',
    '  }while(!(this.vI>=this.vN));'
    ]));
end;

procedure TTestModule.TestAsmBlock;
begin
  StartProgram(false);
  Add('var');
  Add('  vI: longint;');
  Add('begin');
  Add('  vi:=1;');
  Add('  asm');
  Add('    if (vI==1) {');
  Add('      vI=2;');
  Add('    }');
  Add('    if (vI==2){ vI=3; }');
  Add('  end;');
  Add('  VI:=4;');
  ConvertProgram;
  CheckSource('TestAsmBlock',
    LinesToStr([ // statements
    'this.vI = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.vI = 1;',
    'if (vI==1) {',
    '  vI=2;',
    '}',
    'if (vI==2){ vI=3; }',
    ';',
    'this.vI = 4;'
    ]));
end;

procedure TTestModule.TestTryFinally;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  try');
  Add('    i:=0; i:=2 div i;');
  Add('  finally');
  Add('    i:=3');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestTryFinally',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'try {',
    '  this.i = 0;',
    '  this.i = Math.floor(2 / this.i);',
    '} finally {',
    '  this.i = 3;',
    '};'
    ]));
end;

procedure TTestModule.TestTryExcept;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('  Exception = class Msg: string; end;');
  Add('  EInvalidCast = class(Exception) end;');
  Add('var vI: longint;');
  Add('begin');
  Add('  try');
  Add('    vi:=1;');
  Add('  except');
  Add('    vi:=2');
  Add('  end;');
  Add('  try');
  Add('    vi:=3;');
  Add('  except');
  Add('    raise;');
  Add('  end;');
  Add('  try');
  Add('    VI:=4;');
  Add('  except');
  Add('    on einvalidcast do');
  Add('      raise;');
  Add('    on E: exception do');
  Add('      if e.msg='''' then');
  Add('        raise e;');
  Add('    else');
  Add('      vi:=5');
  Add('  end;');
  Add('  try');
  Add('    VI:=6;');
  Add('  except');
  Add('    on einvalidcast do ;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestTryExcept',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "Exception", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '    this.Msg = "";',
    '  };',
    '});',
    'rtl.createClass(this, "EInvalidCast", this.Exception, function () {',
    '});',
    'this.vI = 0;'
    ]),
    LinesToStr([ // this.$main
    'try {',
    '  this.vI = 1;',
    '} catch {',
    '  this.vI = 2;',
    '};',
    'try {',
    '  this.vI = 3;',
    '} catch ($e) {',
    '  throw $e;',
    '};',
    'try {',
    '  this.vI = 4;',
    '} catch ($e) {',
    '  if (this.EInvalidCast.isPrototypeOf($e)){',
    '    throw $e',
    '  } else if (this.Exception.isPrototypeOf($e)) {',
    '    var E = $e;',
    '    if (E.Msg == "") throw E;',
    '  } else {',
    '    this.vI = 5;',
    '  }',
    '};',
    'try {',
    '  this.vI = 6;',
    '} catch ($e) {',
    '  if (this.EInvalidCast.isPrototypeOf($e)){' ,
    '  } else throw $e',
    '};',
    '']));
end;

procedure TTestModule.TestCaseOf;
begin
  StartProgram(false);
  Add('var vI: longint;');
  Add('begin');
  Add('  case vi of');
  Add('  1: ;');
  Add('  2: vi:=3;');
  Add('  else');
  Add('    VI:=4');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOf',
    LinesToStr([ // statements
    'this.vI = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $tmp1 = this.vI;',
    'if ($tmp1 == 1) {} else if ($tmp1 == 2){ this.vI = 3 }else {',
    '  this.vI = 4;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOf_UseSwitch;
begin
  StartProgram(false);
  Converter.UseSwitchStatement:=true;
  Add('var Vi: longint;');
  Add('begin');
  Add('  case vi of');
  Add('  1: ;');
  Add('  2: VI:=3;');
  Add('  else');
  Add('    vi:=4');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOf_UseSwitch',
    LinesToStr([ // statements
    'this.Vi = 0;'
    ]),
    LinesToStr([ // this.$main
    'switch (this.Vi) {',
    'case 1:',
    '  break;',
    'case 2:',
    '  this.Vi = 3;',
    '  break;',
    'default:',
    '  this.Vi = 4;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOfNoElse;
begin
  StartProgram(false);
  Add('var Vi: longint;');
  Add('begin');
  Add('  case vi of');
  Add('  1: begin vi:=2; VI:=3; end;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOfNoElse',
    LinesToStr([ // statements
    'this.Vi = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $tmp1 = this.Vi;',
    'if ($tmp1 == 1) {',
    '  this.Vi = 2;',
    '  this.Vi = 3;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOfNoElse_UseSwitch;
begin
  StartProgram(false);
  Converter.UseSwitchStatement:=true;
  Add('var vI: longint;');
  Add('begin');
  Add('  case vi of');
  Add('  1: begin VI:=2; vi:=3; end;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOfNoElse_UseSwitch',
    LinesToStr([ // statements
    'this.vI = 0;'
    ]),
    LinesToStr([ // this.$main
    'switch (this.vI) {',
    'case 1:',
    '  this.vI = 2;',
    '  this.vI = 3;',
    '  break;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOfRange;
begin
  StartProgram(false);
  Add('var vI: longint;');
  Add('begin');
  Add('  case vi of');
  Add('  1..3: vi:=14;');
  Add('  4,5: vi:=16;');
  Add('  6..7,9..10: ;');
  Add('  else ;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOfRange',
    LinesToStr([ // statements
    'this.vI = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $tmp1 = this.vI;',
    'if (($tmp1 >= 1) && ($tmp1 <= 3)){',
    '  this.vI = 14',
    '} else if (($tmp1 == 4) || ($tmp1 == 5)){',
    '  this.vI = 16',
    '} else if ((($tmp1 >= 6) && ($tmp1 <= 7)) || (($tmp1 >= 9) && ($tmp1 <= 10))) ;'
    ]));
end;

procedure TTestModule.TestArray_Dynamic;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrayInt = array of longint;');
  Add('var');
  Add('  Arr: TArrayInt;');
  Add('  i: longint;');
  Add('  b: boolean;');
  Add('begin');
  Add('  SetLength(arr,3);');
  Add('  arr[0]:=4;');
  Add('  arr[1]:=length(arr)+arr[0];');
  Add('  arr[i]:=5;');
  Add('  arr[arr[i]]:=arr[6];');
  Add('  i:=low(arr);');
  Add('  i:=high(arr);');
  Add('  b:=Assigned(arr);');
  ConvertProgram;
  CheckSource('TestArray_Dynamic',
    LinesToStr([ // statements
    'this.Arr = [];',
    'this.i = 0;',
    'this.b = false;'
    ]),
    LinesToStr([ // this.$main
    'this.Arr = rtl.arraySetLength(this.Arr,3,0);',
    'this.Arr[0] = 4;',
    'this.Arr[1] = rtl.length(this.Arr) + this.Arr[0];',
    'this.Arr[this.i] = 5;',
    'this.Arr[this.Arr[this.i]] = this.Arr[6];',
    'this.i = 0;',
    'this.i = rtl.length(this.Arr) - 1;',
    'this.b = rtl.length(this.Arr) > 0;',
    '']));
end;

procedure TTestModule.TestArray_Dynamic_Nil;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrayInt = array of longint;');
  Add('var');
  Add('  Arr: TArrayInt;');
  Add('procedure DoIt(const i: TArrayInt; j: TArrayInt); begin end;');
  Add('begin');
  Add('  arr:=nil;');
  Add('  if arr=nil then;');
  Add('  if nil=arr then;');
  Add('  if arr<>nil then;');
  Add('  if nil<>arr then;');
  Add('  DoIt(nil,nil);');
  ConvertProgram;
  CheckSource('TestArray_Dynamic',
    LinesToStr([ // statements
    'this.Arr = [];',
    'this.DoIt = function(i,j){',
    '};'
    ]),
    LinesToStr([ // this.$main
    'this.Arr = [];',
    'if (rtl.length(this.Arr) == 0) ;',
    'if (rtl.length(this.Arr) == 0) ;',
    'if (rtl.length(this.Arr) > 0) ;',
    'if (rtl.length(this.Arr) > 0) ;',
    'this.DoIt([],[]);',
    '']));
end;

procedure TTestModule.TestArray_DynMultiDimensional;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrayInt = array of longint;');
  Add('  TArrayArrayInt = array of TArrayInt;');
  Add('var');
  Add('  Arr: TArrayInt;');
  Add('  Arr2: TArrayArrayInt;');
  Add('  i: longint;');
  Add('begin');
  Add('  arr2:=nil;');
  Add('  if arr2=nil then;');
  Add('  if nil=arr2 then;');
  Add('  i:=low(arr2);');
  Add('  i:=low(arr2[1]);');
  Add('  i:=high(arr2);');
  Add('  i:=high(arr2[2]);');
  Add('  arr2[3]:=arr;');
  Add('  arr2[4][5]:=i;');
  Add('  i:=arr2[6][7];');
  Add('  arr2[8,9]:=i;');
  Add('  i:=arr2[10,11];');
  Add('  SetLength(arr2,14);');
  Add('  SetLength(arr2[15],16);');
  ConvertProgram;
  CheckSource('TestArray_Dynamic',
    LinesToStr([ // statements
    'this.Arr = [];',
    'this.Arr2 = [];',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.Arr2 = [];',
    'if (rtl.length(this.Arr2) == 0) ;',
    'if (rtl.length(this.Arr2) == 0) ;',
    'this.i = 0;',
    'this.i = 0;',
    'this.i = rtl.length(this.Arr2) - 1;',
    'this.i = rtl.length(this.Arr2[2]) - 1;',
    'this.Arr2[3] = this.Arr;',
    'this.Arr2[4][5] = this.i;',
    'this.i = this.Arr2[6][7];',
    'this.Arr2[8][9] = this.i;',
    'this.i = this.Arr2[10][11];',
    'this.Arr2 = rtl.arraySetLength(this.Arr2, 14, []);',
    'this.Arr2[15] = rtl.arraySetLength(this.Arr2[15], 16, 0);',
    '']));
end;

procedure TTestModule.TestArrayOfRecord;
begin
  StartProgram(false);
  Add('type');
  Add('  TRec = record');
  Add('    Int: longint;');
  Add('  end;');
  Add('  TArrayRec = array of TRec;');
  Add('var');
  Add('  Arr: TArrayRec;');
  Add('  r: TRec;');
  Add('  i: longint;');
  Add('begin');
  Add('  SetLength(arr,3);');
  Add('  arr[0].int:=4;');
  Add('  arr[1].int:=length(arr)+arr[2].int;');
  Add('  arr[arr[i].int].int:=arr[5].int;');
  Add('  arr[7]:=r;');
  Add('  r:=arr[8];');
  Add('  i:=low(arr);');
  Add('  i:=high(arr);');
  ConvertProgram;
  CheckSource('TestArrayOfRecord',
    LinesToStr([ // statements
    'this.TRec = function (s) {',
    '  if (s) {',
    '    this.Int = s.Int;',
    '  } else {',
    '    this.Int = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.Int == b.Int;',
    '  };',
    '};',
    'this.Arr = [];',
    'this.r = new this.TRec();',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.Arr = rtl.arraySetLength(this.Arr,3, this.TRec);',
    'this.Arr[0].Int = 4;',
    'this.Arr[1].Int = rtl.length(this.Arr)+this.Arr[2].Int;',
    'this.Arr[this.Arr[this.i].Int].Int = this.Arr[5].Int;',
    'this.Arr[7] = new this.TRec(this.r);',
    'this.r = new this.TRec(this.Arr[8]);',
    'this.i = 0;',
    'this.i = rtl.length(this.Arr)-1;',
    '']));
end;

procedure TTestModule.TestArray_AsParams;
begin
  StartProgram(false);
  Add('type integer = longint;');
  Add('type TArrInt = array of integer;');
  Add('procedure DoIt(vG: TArrInt; const vH: TArrInt; var vI: TArrInt);');
  Add('var vJ: TArrInt;');
  Add('begin');
  Add('  vg:=vg;');
  Add('  vj:=vh;');
  Add('  vi:=vi;');
  Add('  doit(vg,vg,vg);');
  Add('  doit(vh,vh,vj);');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj,vj,vj);');
  Add('end;');
  Add('var i: TArrInt;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestArray_AsParams',
    LinesToStr([ // statements
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = [];',
    '  vG = vG;',
    '  vJ = vH;',
    '  vI.set(vI.get());',
    '  this.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  this.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  this.DoIt(vI.get(), vI.get(), vI);',
    '  this.DoIt(vJ, vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = [];'
    ]),
    LinesToStr([
    'this.DoIt(this.i,this.i,{',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestArrayElement_AsParams;
begin
  StartProgram(false);
  Add('type integer = longint;');
  Add('type TArrayInt = array of integer;');
  Add('procedure DoIt(vG: Integer; const vH: Integer; var vI: Integer);');
  Add('var vJ: tarrayint;');
  Add('begin');
  Add('  vi:=vi;');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj[1+1],vj[1+2],vj[1+3]);');
  Add('end;');
  Add('var a: TArrayInt;');
  Add('begin');
  Add('  doit(a[1+4],a[1+5],a[1+6]);');
  ConvertProgram;
  CheckSource('TestArrayElement_AsParams',
    LinesToStr([ // statements
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = [];',
    '  vI.set(vI.get());',
    '  this.DoIt(vI.get(), vI.get(), vI);',
    '  this.DoIt(vJ[1+1], vJ[1+2], {',
    '    a:1+3,',
    '    p:vJ,',
    '    get: function () {',
    '      return this.p[this.a];',
    '    },',
    '    set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '  });',
    '};',
    'this.a = [];'
    ]),
    LinesToStr([
    'this.DoIt(this.a[1+4],this.a[1+5],{',
    '  a: 1+6,',
    '  p: this.a,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestArrayElementFromFuncResult_AsParams;
begin
  StartProgram(false);
  Add('type Integer = longint;');
  Add('type TArrayInt = array of integer;');
  Add('function GetArr(vB: integer = 0): tarrayint;');
  Add('begin');
  Add('end;');
  Add('procedure DoIt(vG: integer; const vH: integer; var vI: integer);');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  doit(getarr[1+1],getarr[1+2],getarr[1+3]);');
  Add('  doit(getarr()[2+1],getarr()[2+2],getarr()[2+3]);');
  Add('  doit(getarr(7)[3+1],getarr(8)[3+2],getarr(9)[3+3]);');
  ConvertProgram;
  CheckSource('TestArrayElementFromFuncResult_AsParams',
    LinesToStr([ // statements
    'this.GetArr = function (vB) {',
    '  var Result = [];',
    '  return Result;',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '};'
    ]),
    LinesToStr([
    'this.DoIt(this.GetArr(0)[1+1],this.GetArr(0)[1+2],{',
    '  a: 1+3,',
    '  p: this.GetArr(0),',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    'this.DoIt(this.GetArr(0)[2+1],this.GetArr(0)[2+2],{',
    '  a: 2+3,',
    '  p: this.GetArr(0),',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    'this.DoIt(this.GetArr(7)[3+1],this.GetArr(8)[3+2],{',
    '  a: 3+3,',
    '  p: this.GetArr(9),',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestArrayEnumTypeRange;
begin
  StartProgram(false);
  Add('type');
  Add('  TEnum = (red,blue);');
  Add('  TEnumArray = array[TEnum] of longint;');
  Add('var');
  Add('  e: TEnum;');
  Add('  i: longint;');
  Add('  a: TEnumArray;');
  Add('  numbers: TEnumArray = (1,2);');
  Add('  names: array[TEnum] of string = (''red'',''blue'');');
  Add('begin');
  Add('  e:=low(a);');
  Add('  e:=high(a);');
  Add('  i:=a[red]+length(a);');
  Add('  a[e]:=a[e];');
  ConvertProgram;
  CheckSource('TestArrayEnumTypeRange',
    LinesToStr([ // statements
    '  this.TEnum = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "blue",',
    '  blue: 1',
    '};',
    'this.e = 0;',
    'this.i = 0;',
    'this.a = rtl.arrayNewMultiDim([2],0);',
    'this.numbers = [1, 2];',
    'this.names = ["red", "blue"];',
    '']),
    LinesToStr([ // this.$main
    'this.e = this.TEnum.red;',
    'this.e = this.TEnum.blue;',
    'this.i = this.a[this.TEnum.red]+2;',
    'this.a[this.e] = this.a[this.e];',
    '']));
end;

procedure TTestModule.TestArray_SetLengthProperty;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrInt = array of longint;');
  Add('  TObject = class');
  Add('    function GetColors: TArrInt; external name ''GetColors'';');
  Add('    procedure SetColors(const Value: TArrInt); external name ''SetColors'';');
  Add('    property Colors: TArrInt read GetColors write SetColors;');
  Add('  end;');
  Add('var Obj: TObject;');
  Add('begin');
  Add('  SetLength(Obj.Colors,2);');
  ConvertProgram;
  CheckSource('TestArray_SetLengthProperty',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.Obj = null;',
    '']),
    LinesToStr([
    'this.Obj.SetColors(rtl.arraySetLength(this.Obj.GetColors(), 2, 0));',
    '']));
end;

procedure TTestModule.TestArray_OpenArrayOfString;
begin
  StartProgram(false);
  Add('procedure DoIt(const a: array of String);');
  Add('var');
  Add('  i: longint;');
  Add('  s: string;');
  Add('begin');
  Add('  for i:=low(a) to high(a) do s:=a[length(a)-i-1];');
  Add('end;');
  Add('var s: string;');
  Add('begin');
  Add('  DoIt([]);');
  Add('  DoIt([s,''foo'','''',s+s]);');
  ConvertProgram;
  CheckSource('TestArray_OpenArrayOfString',
    LinesToStr([ // statements
    'this.DoIt = function (a) {',
    '  var i = 0;',
    '  var s = "";',
    '  var $loopend1 = rtl.length(a) - 1;',
    '  for (i = 0; i <= $loopend1; i++) s = a[(rtl.length(a) - i) - 1];',
    '};',
    'this.s = "";',
    '']),
    LinesToStr([
    'this.DoIt([]);',
    'this.DoIt([this.s, "foo", "", this.s + this.s]);',
    '']));
end;

procedure TTestModule.TestArray_Concat;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TFlag = (big,small);');
  Add('  TFlags = set of TFlag;');
  Add('  TRec = record');
  Add('    i: integer;');
  Add('  end;');
  Add('  TArrInt = array of integer;');
  Add('  TArrRec = array of TRec;');
  Add('  TArrSet = array of TFlags;');
  Add('  TArrJSValue = array of jsvalue;');
  Add('var');
  Add('  ArrInt: tarrint;');
  Add('  ArrRec: tarrrec;');
  Add('  ArrSet: tarrset;');
  Add('  ArrJSValue: tarrjsvalue;');
  Add('begin');
  Add('  arrint:=concat(arrint);');
  Add('  arrint:=concat(arrint,arrint);');
  Add('  arrint:=concat(arrint,arrint,arrint);');
  Add('  arrrec:=concat(arrrec);');
  Add('  arrrec:=concat(arrrec,arrrec);');
  Add('  arrrec:=concat(arrrec,arrrec,arrrec);');
  Add('  arrset:=concat(arrset);');
  Add('  arrset:=concat(arrset,arrset);');
  Add('  arrset:=concat(arrset,arrset,arrset);');
  Add('  arrjsvalue:=concat(arrjsvalue);');
  Add('  arrjsvalue:=concat(arrjsvalue,arrjsvalue);');
  Add('  arrjsvalue:=concat(arrjsvalue,arrjsvalue,arrjsvalue);');
  ConvertProgram;
  CheckSource('TestArray_Concat',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "big",',
    '  big: 0,',
    '  "1": "small",',
    '  small: 1',
    '};',
    'this.TRec = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.ArrInt = [];',
    'this.ArrRec = [];',
    'this.ArrSet = [];',
    'this.ArrJSValue = [];',
    '']),
    LinesToStr([ // this.$main
    'this.ArrInt = this.ArrInt;',
    'this.ArrInt = this.ArrInt.concat(this.ArrInt);',
    'this.ArrInt = this.ArrInt.concat(this.ArrInt,this.ArrInt);',
    'this.ArrRec = this.ArrRec;',
    'this.ArrRec = rtl.arrayConcat(this.TRec, this.ArrRec);',
    'this.ArrRec = rtl.arrayConcat(this.TRec, this.ArrRec, this.ArrRec);',
    'this.ArrSet = this.ArrSet;',
    'this.ArrSet = rtl.arrayConcat("refSet", this.ArrSet);',
    'this.ArrSet = rtl.arrayConcat("refSet", this.ArrSet, this.ArrSet);',
    'this.ArrJSValue = this.ArrJSValue;',
    'this.ArrJSValue = this.ArrJSValue.concat(this.ArrJSValue);',
    'this.ArrJSValue = this.ArrJSValue.concat(this.ArrJSValue, this.ArrJSValue);',
    '']));
end;

procedure TTestModule.TestArray_Copy;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TFlag = (big,small);');
  Add('  TFlags = set of TFlag;');
  Add('  TRec = record');
  Add('    i: integer;');
  Add('  end;');
  Add('  TArrInt = array of integer;');
  Add('  TArrRec = array of TRec;');
  Add('  TArrSet = array of TFlags;');
  Add('  TArrJSValue = array of jsvalue;');
  Add('var');
  Add('  ArrInt: tarrint;');
  Add('  ArrRec: tarrrec;');
  Add('  ArrSet: tarrset;');
  Add('  ArrJSValue: tarrjsvalue;');
  Add('begin');
  Add('  arrint:=copy(arrint);');
  Add('  arrint:=copy(arrint,2);');
  Add('  arrint:=copy(arrint,3,4);');
  Add('  arrrec:=copy(arrrec);');
  Add('  arrrec:=copy(arrrec,5);');
  Add('  arrrec:=copy(arrrec,6,7);');
  Add('  arrset:=copy(arrset);');
  Add('  arrset:=copy(arrset,8);');
  Add('  arrset:=copy(arrset,9,10);');
  Add('  arrjsvalue:=copy(arrjsvalue);');
  Add('  arrjsvalue:=copy(arrjsvalue,11);');
  Add('  arrjsvalue:=copy(arrjsvalue,12,13);');
  ConvertProgram;
  CheckSource('TestArray_Copy',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "big",',
    '  big: 0,',
    '  "1": "small",',
    '  small: 1',
    '};',
    'this.TRec = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.ArrInt = [];',
    'this.ArrRec = [];',
    'this.ArrSet = [];',
    'this.ArrJSValue = [];',
    '']),
    LinesToStr([ // this.$main
    'this.ArrInt = rtl.arrayCopy(0, this.ArrInt, 0);',
    'this.ArrInt = rtl.arrayCopy(0, this.ArrInt, 2);',
    'this.ArrInt = rtl.arrayCopy(0, this.ArrInt, 3, 4);',
    'this.ArrRec = rtl.arrayCopy(this.TRec, this.ArrRec, 0);',
    'this.ArrRec = rtl.arrayCopy(this.TRec, this.ArrRec, 5);',
    'this.ArrRec = rtl.arrayCopy(this.TRec, this.ArrRec, 6, 7);',
    'this.ArrSet = rtl.arrayCopy("refSet", this.ArrSet, 0);',
    'this.ArrSet = rtl.arrayCopy("refSet", this.ArrSet, 8);',
    'this.ArrSet = rtl.arrayCopy("refSet", this.ArrSet, 9, 10);',
    'this.ArrJSValue = rtl.arrayCopy(0, this.ArrJSValue, 0);',
    'this.ArrJSValue = rtl.arrayCopy(0, this.ArrJSValue, 11);',
    'this.ArrJSValue = rtl.arrayCopy(0, this.ArrJSValue, 12, 13);',
    '']));
end;

procedure TTestModule.TestArray_InsertDelete;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TFlag = (big,small);');
  Add('  TFlags = set of TFlag;');
  Add('  TRec = record');
  Add('    i: integer;');
  Add('  end;');
  Add('  TArrInt = array of integer;');
  Add('  TArrRec = array of TRec;');
  Add('  TArrSet = array of TFlags;');
  Add('  TArrJSValue = array of jsvalue;');
  Add('var');
  Add('  ArrInt: tarrint;');
  Add('  ArrRec: tarrrec;');
  Add('  ArrSet: tarrset;');
  Add('  ArrJSValue: tarrjsvalue;');
  Add('begin');
  Add('  Insert(1,arrint,2);');
  Add('  Insert(arrint[3],arrint,4);');
  Add('  Insert(arrrec[5],arrrec,6);');
  Add('  Insert(arrset[7],arrset,7);');
  Add('  Insert(arrjsvalue[8],arrjsvalue,9);');
  Add('  Insert(10,arrjsvalue,11);');
  Add('  Delete(arrint,12,13);');
  Add('  Delete(arrrec,14,15);');
  Add('  Delete(arrset,17,18);');
  Add('  Delete(arrjsvalue,19,10);');
  ConvertProgram;
  CheckSource('TestArray_InsertDelete',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "big",',
    '  big: 0,',
    '  "1": "small",',
    '  small: 1',
    '};',
    'this.TRec = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.ArrInt = [];',
    'this.ArrRec = [];',
    'this.ArrSet = [];',
    'this.ArrJSValue = [];',
    '']),
    LinesToStr([ // this.$main
    'this.ArrInt.splice(2, 1, 1);',
    'this.ArrInt.splice(4, 1, this.ArrInt[3]);',
    'this.ArrRec.splice(6, 1, this.ArrRec[5]);',
    'this.ArrSet.splice(7, 1, this.ArrSet[7]);',
    'this.ArrJSValue.splice(9, 1, this.ArrJSValue[8]);',
    'this.ArrJSValue.splice(11, 1, 10);',
    'this.ArrInt.splice(12, 13);',
    'this.ArrRec.splice(14, 15);',
    'this.ArrSet.splice(17, 18);',
    'this.ArrJSValue.splice(19, 10);',
    '']));
end;

procedure TTestModule.TestExternalClass_TypeCastArrayToExternalArray;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array''');
  Add('    class function isArray(Value: JSValue) : boolean;');
  Add('    function concat() : TJSArray; varargs;');
  Add('  end;');
  Add('var');
  Add('  aObj: TJSArray;');
  Add('  a: array of longint;');
  Add('begin');
  Add('  if TJSArray.isArray(65) then ;');
  Add('  aObj:=TJSArray(a).concat(a);');
  ConvertProgram;
  CheckSource('TestExternalClass_TypeCastArrayToExternalArray',
    LinesToStr([ // statements
    'this.aObj = null;',
    'this.a = [];',
    '']),
    LinesToStr([ // this.$main
    'if (Array.isArray(65)) ;',
    'this.aObj = this.a.concat(this.a);',
    '']));
end;

procedure TTestModule.TestExternalClass_TypeCastArrayFromExternalArray;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TArrStr = array of string;');
  Add('  TJSArray = class external name ''Array''');
  Add('  end;');
  Add('var');
  Add('  aObj: TJSArray;');
  Add('  a: TArrStr;');
  Add('begin');
  Add('  a:=TArrStr(aObj);');
  Add('  TArrStr(aObj)[1]:=TArrStr(aObj)[2];');
  ConvertProgram;
  CheckSource('TestExternalClass_TypeCastArrayFromExternalArray',
    LinesToStr([ // statements
    'this.aObj = null;',
    'this.a = [];',
    '']),
    LinesToStr([ // this.$main
    'this.a = this.aObj;',
    'this.aObj[1] = this.aObj[2];',
    '']));
end;

procedure TTestModule.TestRecord_Var;
begin
  StartProgram(false);
  Add('type');
  Add('  TRecA = record');
  Add('    Bold: longint;');
  Add('  end;');
  Add('var Rec: TRecA;');
  Add('begin');
  Add('  rec.bold:=123');
  ConvertProgram;
  CheckSource('TestRecord_Var',
    LinesToStr([ // statements
    'this.TRecA = function (s) {',
    '  if (s) {',
    '    this.Bold = s.Bold;',
    '  } else {',
    '    this.Bold = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.Bold == b.Bold;',
    '  };',
    '};',
    'this.Rec = new this.TRecA();'
    ]),
    LinesToStr([ // this.$main
    'this.Rec.Bold = 123;'
    ]));
end;

procedure TTestModule.TestWithRecordDo;
begin
  StartProgram(false);
  Add('type');
  Add('  TRec = record');
  Add('    vI: longint;');
  Add('  end;');
  Add('var');
  Add('  Int: longint;');
  Add('  r: TRec;');
  Add('begin');
  Add('  with r do');
  Add('    int:=vi;');
  Add('  with r do begin');
  Add('    int:=vi;');
  Add('    vi:=int;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestWithRecordDo',
    LinesToStr([ // statements
    'this.TRec = function (s) {',
    '  if (s) {',
    '    this.vI = s.vI;',
    '  } else {',
    '    this.vI = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.vI == b.vI;',
    '  };',
    '};',
    'this.Int = 0;',
    'this.r = new this.TRec();'
    ]),
    LinesToStr([ // this.$main
    'var $with1 = this.r;',
    'this.Int = $with1.vI;',
    'var $with2 = this.r;',
    'this.Int = $with2.vI;',
    '$with2.vI = this.Int;'
    ]));
end;

procedure TTestModule.TestRecord_Assign;
begin
  StartProgram(false);
  Add('type');
  Add('  TEnum = (red,green);');
  Add('  TEnums = set of TEnum;');
  Add('  TSmallRec = record');
  Add('    N: longint;');
  Add('  end;');
  Add('  TBigRec = record');
  Add('    Int: longint;');
  Add('    D: double;');
  Add('    Arr: array of longint;');
  Add('    Small: TSmallRec;');
  Add('    Enums: TEnums;');
  Add('  end;');
  Add('var');
  Add('  r, s: TBigRec;');
  Add('begin');
  Add('  r:=s;');
  ConvertProgram;
  CheckSource('TestRecord_Assign',
    LinesToStr([ // statements
    'this.TEnum = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "green",',
    '  green: 1',
    '};',
    'this.TSmallRec = function (s) {',
    '  if(s){',
    '    this.N = s.N;',
    '  } else {',
    '    this.N = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.N == b.N;',
    '  };',
    '};',
    'this.TBigRec = function (s) {',
    '  if(s){',
    '    this.Int = s.Int;',
    '    this.D = s.D;',
    '    this.Arr = s.Arr;',
    '    this.Small = new pas.program.TSmallRec(s.Small);',
    '    this.Enums = rtl.refSet(s.Enums);',
    '  } else {',
    '    this.Int = 0;',
    '    this.D = 0.0;',
    '    this.Arr = [];',
    '    this.Small = new pas.program.TSmallRec();',
    '    this.Enums = {};',
    '  };',
    '  this.$equal = function (b) {',
    '    return (this.Int == b.Int) && ((this.D == b.D) && ((this.Arr == b.Arr)',
    ' && (this.Small.$equal(b.Small) && rtl.eqSet(this.Enums, b.Enums))));',
    '  };',
    '};',
    'this.r = new this.TBigRec();',
    'this.s = new this.TBigRec();'
    ]),
    LinesToStr([ // this.$main
    'this.r = new this.TBigRec(this.s);',
    '']));
end;

procedure TTestModule.TestRecord_PassAsArgClone;
begin
  StartProgram(false);
  Add('type');
  Add('  TRecA = record');
  Add('    Bold: longint;');
  Add('  end;');
  Add('procedure DoDefault(r: treca); begin end;');
  Add('procedure DoConst(const r: treca); begin end;');
  Add('var Rec: treca;');
  Add('begin');
  Add('  dodefault(rec);');
  Add('  doconst(rec);');
  ConvertProgram;
  CheckSource('TestRecord_PassAsArgClone',
    LinesToStr([ // statements
    'this.TRecA = function (s) {',
    '  if (s) {',
    '    this.Bold = s.Bold;',
    '  } else {',
    '    this.Bold = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.Bold == b.Bold;',
    '  };',
    '};',
    'this.DoDefault = function (r) {',
    '};',
    'this.DoConst = function (r) {',
    '};',
    'this.Rec = new this.TRecA();'
    ]),
    LinesToStr([ // this.$main
    'this.DoDefault(new this.TRecA(this.Rec));',
    'this.DoConst(this.Rec);',
    '']));
end;

procedure TTestModule.TestRecord_AsParams;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TRecord = record');
  Add('    i: integer;');
  Add('  end;');
  Add('procedure DoIt(vG: TRecord; const vH: TRecord; var vI: TRecord);');
  Add('var vJ: TRecord;');
  Add('begin');
  Add('  vg:=vg;');
  Add('  vj:=vh;');
  Add('  vi:=vi;');
  Add('  doit(vg,vg,vg);');
  Add('  doit(vh,vh,vj);');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj,vj,vj);');
  Add('end;');
  Add('var i: TRecord;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestRecord_AsParams',
    LinesToStr([ // statements
    'this.TRecord = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = new this.TRecord();',
    '  vG = new this.TRecord(vG);',
    '  vJ = new this.TRecord(vH);',
    '  vI.set(new this.TRecord(vI.get()));',
    '  this.DoIt(new this.TRecord(vG), vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  this.DoIt(new this.TRecord(vH), vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  this.DoIt(new this.TRecord(vI.get()), vI.get(), vI);',
    '  this.DoIt(new this.TRecord(vJ), vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = new this.TRecord();'
    ]),
    LinesToStr([
    'this.DoIt(new this.TRecord(this.i),this.i,{',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestRecordElement_AsParams;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TRecord = record');
  Add('    i: integer;');
  Add('  end;');
  Add('procedure DoIt(vG: integer; const vH: integer; var vI: integer);');
  Add('var vJ: TRecord;');
  Add('begin');
  Add('  doit(vj.i,vj.i,vj.i);');
  Add('end;');
  Add('var r: TRecord;');
  Add('begin');
  Add('  doit(r.i,r.i,r.i);');
  ConvertProgram;
  CheckSource('TestRecordElement_AsParams',
    LinesToStr([ // statements
    'this.TRecord = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = new this.TRecord();',
    '  this.DoIt(vJ.i, vJ.i, {',
    '    p: vJ,',
    '    get: function () {',
    '      return this.p.i;',
    '    },',
    '    set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '  });',
    '};',
    'this.r = new this.TRecord();'
    ]),
    LinesToStr([
    'this.DoIt(this.r.i,this.r.i,{',
    '  p: this.r,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestRecordElementFromFuncResult_AsParams;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TRecord = record');
  Add('    i: integer;');
  Add('  end;');
  Add('function GetRec(vB: integer = 0): TRecord;');
  Add('begin');
  Add('end;');
  Add('procedure DoIt(vG: integer; const vH: integer; var vI: integer);');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  doit(getrec.i,getrec.i,getrec.i);');
  Add('  doit(getrec().i,getrec().i,getrec().i);');
  Add('  doit(getrec(1).i,getrec(2).i,getrec(3).i);');
  ConvertProgram;
  CheckSource('TestRecordElementFromFuncResult_AsParams',
    LinesToStr([ // statements
    'this.TRecord = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.GetRec = function (vB) {',
    '  var Result = new this.TRecord();',
    '  return Result;',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '};'
    ]),
    LinesToStr([
    'this.DoIt(this.GetRec(0).i,this.GetRec(0).i,{',
    '  p: this.GetRec(0),',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});',
    'this.DoIt(this.GetRec(0).i,this.GetRec(0).i,{',
    '  p: this.GetRec(0),',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});',
    'this.DoIt(this.GetRec(1).i,this.GetRec(2).i,{',
    '  p: this.GetRec(3),',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestRecordElementFromWith_AsParams;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TRecord = record');
  Add('    i: integer;');
  Add('  end;');
  Add('procedure DoIt(vG: integer; const vH: integer; var vI: integer);');
  Add('begin');
  Add('end;');
  Add('var r: trecord;');
  Add('begin');
  Add('  with r do ');
  Add('    doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestRecordElementFromWith_AsParams',
    LinesToStr([ // statements
    'this.TRecord = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '  } else {',
    '    this.i = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.i == b.i;',
    '  };',
    '};',
    'this.DoIt = function (vG,vH,vI) {',
    '};',
    'this.r = new this.TRecord();'
    ]),
    LinesToStr([
    'var $with1 = this.r;',
    'this.DoIt($with1.i,$with1.i,{',
    '  p: $with1,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestRecord_Equal;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TFlag = (red,blue);');
  Add('  TFlags = set of TFlag;');
  Add('  TProc = procedure;');
  Add('  TRecord = record');
  Add('    i: integer;');
  Add('    Event: TProc;');
  Add('    f: TFlags;');
  Add('  end;');
  Add('  TNested = record');
  Add('    r: TRecord;');
  Add('  end;');
  Add('var');
  Add('  b: boolean;');
  Add('  r,s: trecord;');
  Add('begin');
  Add('  b:=r=s;');
  Add('  b:=r<>s;');
  ConvertProgram;
  CheckSource('TestRecord_Equal',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "blue",',
    '  blue: 1',
    '};',
    'this.TRecord = function (s) {',
    '  if (s) {',
    '    this.i = s.i;',
    '    this.Event = s.Event;',
    '    this.f = rtl.refSet(s.f);',
    '  } else {',
    '    this.i = 0;',
    '    this.Event = null;',
    '    this.f = {};',
    '  };',
    '  this.$equal = function (b) {',
    '    return (this.i == b.i) && (rtl.eqCallback(this.Event, b.Event) && rtl.eqSet(this.f, b.f));',
    '  };',
    '};',
    'this.TNested = function (s) {',
    '  if (s) {',
    '    this.r = new pas.program.TRecord(s.r);',
    '  } else {',
    '    this.r = new pas.program.TRecord();',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.r.$equal(b.r);',
    '  };',
    '};',
    'this.b = false;',
    'this.r = new this.TRecord();',
    'this.s = new this.TRecord();'
    ]),
    LinesToStr([
    'this.b = this.r.$equal(this.s);',
    'this.b = !this.r.$equal(this.s);',
    '']));
end;

procedure TTestModule.TestClass_TObjectDefaultConstructor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    constructor Create;');
  Add('    destructor Destroy;');
  Add('  end;');
  Add('constructor tobject.create;');
  Add('begin end;');
  Add('destructor tobject.destroy;');
  Add('begin end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj:=tobject.create;');
  Add('  obj.destroy;');
  ConvertProgram;
  CheckSource('TestClass_TObjectDefaultConstructor',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function(){',
    '  };',
    '  this.Destroy = function(){',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.TObject.$create("Create");',
    'this.Obj.$destroy("Destroy");',
    '']));
end;

procedure TTestModule.TestClass_TObjectConstructorWithParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    constructor Create(Par: longint);');
  Add('  end;');
  Add('constructor tobject.create(par: longint);');
  Add('begin end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj:=tobject.create(3);');
  ConvertProgram;
  CheckSource('TestClass_TObjectConstructorWithParams',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function(Par){',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.TObject.$create("Create",[3]);'
    ]));
end;

procedure TTestModule.TestClass_Var;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    vI: longint;');
  Add('    constructor Create(Par: longint);');
  Add('  end;');
  Add('constructor tobject.create(par: longint);');
  Add('begin');
  Add('  vi:=par+3');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj:=tobject.create(4);');
  Add('  obj.vi:=obj.VI+5;');
  ConvertProgram;
  CheckSource('TestClass_Var',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '    this.vI = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function(Par){',
    '    this.vI = Par+3;',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.TObject.$create("Create",[4]);',
    'this.Obj.vI = this.Obj.vI + 5;'
    ]));
end;

procedure TTestModule.TestClass_Method;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    vI: longint;');
  Add('    Sub: TObject;');
  Add('    constructor Create;');
  Add('    function GetIt(Par: longint): tobject;');
  Add('  end;');
  Add('constructor tobject.create; begin end;');
  Add('function tobject.getit(par: longint): tobject;');
  Add('begin');
  Add('  Self.vi:=par+3;');
  Add('  Result:=self.sub;');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj:=tobject.create;');
  Add('  obj.getit(4);');
  Add('  obj.sub.sub:=nil;');
  Add('  obj.sub.getit(5);');
  Add('  obj.sub.getit(6).SUB:=nil;');
  Add('  obj.sub.getit(7).GETIT(8);');
  Add('  obj.sub.getit(9).SuB.getit(10);');
  ConvertProgram;
  CheckSource('TestClass_Method',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '    this.vI = 0;',
    '    this.Sub = null;',
    '  };',
    '  this.$final = function () {',
    '    this.Sub = undefined;',
    '  };',
    '  this.Create = function(){',
    '  };',
    '  this.GetIt = function(Par){',
    '    var Result = null;',
    '    this.vI = Par + 3;',
    '    Result = this.Sub;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.TObject.$create("Create");',
    'this.Obj.GetIt(4);',
    'this.Obj.Sub.Sub=null;',
    'this.Obj.Sub.GetIt(5);',
    'this.Obj.Sub.GetIt(6).Sub=null;',
    'this.Obj.Sub.GetIt(7).GetIt(8);',
    'this.Obj.Sub.GetIt(9).Sub.GetIt(10);'
    ]));
end;

procedure TTestModule.TestClass_Inheritance;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    constructor Create;');
  Add('  end;');
  Add('  TClassA = class');
  Add('  end;');
  Add('  TClassB = class(TObject)');
  Add('    procedure ProcB;');
  Add('  end;');
  Add('constructor tobject.create; begin end;');
  Add('procedure tclassb.procb; begin end;');
  Add('var');
  Add('  oO: TObject;');
  Add('  oA: TClassA;');
  Add('  oB: TClassB;');
  Add('begin');
  Add('  oO:=tobject.Create;');
  Add('  oA:=tclassa.Create;');
  Add('  ob:=tclassb.Create;');
  Add('  if oo is tclassa then ;');
  Add('  ob:=oo as tclassb;');
  Add('  (oo as tclassb).procb;');
  ConvertProgram;
  CheckSource('TestClass_Inheritance',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '});',
    'rtl.createClass(this,"TClassA",this.TObject,function(){',
    '});',
    'rtl.createClass(this,"TClassB",this.TObject,function(){',
    '  this.ProcB = function () {',
    '  };',
    '});',
    'this.oO = null;',
    'this.oA = null;',
    'this.oB = null;'
    ]),
    LinesToStr([ // this.$main
    'this.oO = this.TObject.$create("Create");',
    'this.oA = this.TClassA.$create("Create");',
    'this.oB = this.TClassB.$create("Create");',
    'if (this.TClassA.isPrototypeOf(this.oO));',
    'this.oB = rtl.as(this.oO, this.TClassB);',
    'rtl.as(this.oO, this.TClassB).ProcB();'
    ]));
end;

procedure TTestModule.TestClass_AbstractMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    procedure DoIt; virtual; abstract;');
  Add('  end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_AbstractMethod',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestClass_CallInherited_NoParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoAbstract; virtual; abstract;');
  Add('    procedure DoVirtual; virtual;');
  Add('    procedure DoIt;');
  Add('  end;');
  Add('  TA = class');
  Add('    procedure doabstract; override;');
  Add('    procedure dovirtual; override;');
  Add('    procedure DoSome;');
  Add('  end;');
  Add('procedure tobject.dovirtual;');
  Add('begin');
  Add('  inherited; // call non existing ancestor -> ignore silently');
  Add('end;');
  Add('procedure tobject.doit;');
  Add('begin');
  Add('end;');
  Add('procedure ta.doabstract;');
  Add('begin');
  Add('  inherited dovirtual; // call TObject.DoVirtual');
  Add('end;');
  Add('procedure ta.dovirtual;');
  Add('begin');
  Add('  inherited; // call TObject.DoVirtual');
  Add('  inherited dovirtual; // call TObject.DoVirtual');
  Add('  inherited dovirtual(); // call TObject.DoVirtual');
  Add('  doit;');
  Add('  doit();');
  Add('end;');
  Add('procedure ta.dosome;');
  Add('begin');
  Add('  inherited; // call non existing ancestor method -> silently ignore');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_CallInherited_NoParams',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoVirtual = function () {',
    '  };',
    '  this.DoIt = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TA", this.TObject, function () {',
    '  this.DoAbstract = function () {',
    '    pas.program.TObject.DoVirtual.call(this);',
    '  };',
    '  this.DoVirtual = function () {',
    '    pas.program.TObject.DoVirtual.apply(this, arguments);',
    '    pas.program.TObject.DoVirtual.call(this);',
    '    pas.program.TObject.DoVirtual.call(this);',
    '    this.DoIt();',
    '    this.DoIt();',
    '  };',
    '  this.DoSome = function () {',
    '  };',
    '});'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestClass_CallInherited_WithParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoAbstract(pA: longint; pB: longint = 0); virtual; abstract;');
  Add('    procedure DoVirtual(pA: longint; pB: longint = 0); virtual;');
  Add('    procedure DoIt(pA: longint; pB: longint = 0);');
  Add('    procedure DoIt2(pA: longint = 1; pB: longint = 2);');
  Add('  end;');
  Add('  TClassA = class');
  Add('    procedure DoAbstract(pA: longint; pB: longint = 0); override;');
  Add('    procedure DoVirtual(pA: longint; pB: longint = 0); override;');
  Add('  end;');
  Add('procedure tobject.dovirtual(pa: longint; pb: longint = 0);');
  Add('begin');
  Add('end;');
  Add('procedure tobject.doit(pa: longint; pb: longint = 0);');
  Add('begin');
  Add('end;');
  Add('procedure tobject.doit2(pa: longint; pb: longint = 0);');
  Add('begin');
  Add('end;');
  Add('procedure tclassa.doabstract(pa: longint; pb: longint = 0);');
  Add('begin');
  Add('  inherited dovirtual(pa,pb); // call TObject.DoVirtual(pA,pB)');
  Add('  inherited dovirtual(pa); // call TObject.DoVirtual(pA,0)');
  Add('end;');
  Add('procedure tclassa.dovirtual(pa: longint; pb: longint = 0);');
  Add('begin');
  Add('  inherited; // call TObject.DoVirtual(pA,pB)');
  Add('  inherited dovirtual(pa,pb); // call TObject.DoVirtual(pA,pB)');
  Add('  inherited dovirtual(pa); // call TObject.DoVirtual(pA,0)');
  Add('  doit(pa,pb);');
  Add('  doit(pa);');
  Add('  doit2(pa);');
  Add('  doit2;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_CallInherited_WithParams',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoVirtual = function (pA,pB) {',
    '  };',
    '  this.DoIt = function (pA,pB) {',
    '  };',
    '  this.DoIt2 = function (pA,pB) {',
    '  };',
    '});',
    'rtl.createClass(this, "TClassA", this.TObject, function () {',
    '  this.DoAbstract = function (pA,pB) {',
    '    pas.program.TObject.DoVirtual.call(this,pA,pB);',
    '    pas.program.TObject.DoVirtual.call(this,pA,0);',
    '  };',
    '  this.DoVirtual = function (pA,pB) {',
    '    pas.program.TObject.DoVirtual.apply(this, arguments);',
    '    pas.program.TObject.DoVirtual.call(this,pA,pB);',
    '    pas.program.TObject.DoVirtual.call(this,pA,0);',
    '    this.DoIt(pA,pB);',
    '    this.DoIt(pA,0);',
    '    this.DoIt2(pA,2);',
    '    this.DoIt2(1,2);',
    '  };',
    '});'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestClasS_CallInheritedConstructor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create; virtual;');
  Add('    constructor CreateWithB(b: boolean);');
  Add('  end;');
  Add('  TA = class');
  Add('    constructor Create; override;');
  Add('    constructor CreateWithC(c: char);');
  Add('    procedure DoIt;');
  Add('    class function DoSome: TObject;');
  Add('  end;');
  Add('constructor tobject.create;');
  Add('begin');
  Add('  inherited; // call non existing ancestor -> ignore silently');
  Add('end;');
  Add('constructor tobject.createwithb(b: boolean);');
  Add('begin');
  Add('  inherited; // call non existing ancestor -> ignore silently');
  Add('  create; // normal call');
  Add('end;');
  Add('constructor ta.create;');
  Add('begin');
  Add('  inherited; // normal call TObject.Create');
  Add('  inherited create; // normal call TObject.Create');
  Add('  inherited createwithb(false); // normal call TObject.CreateWithB');
  Add('end;');
  Add('constructor ta.createwithc(c: char);');
  Add('begin');
  Add('  inherited create; // call TObject.Create');
  Add('  inherited createwithb(true); // call TObject.CreateWithB');
  Add('  doit;');
  Add('  doit();');
  Add('  dosome;');
  Add('end;');
  Add('procedure ta.doit;');
  Add('begin');
  Add('  create; // normal call');
  Add('  createwithb(false); // normal call');
  Add('  createwithc(''c''); // normal call');
  Add('end;');
  Add('class function ta.dosome: TObject;');
  Add('begin');
  Add('  Result:=create; // constructor');
  Add('  Result:=createwithb(true); // constructor');
  Add('  Result:=createwithc(''c''); // constructor');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_CallInheritedConstructor',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '  this.CreateWithB = function (b) {',
    '    this.Create();',
    '  };',
    '});',
    'rtl.createClass(this, "TA", this.TObject, function () {',
    '  this.Create = function () {',
    '    pas.program.TObject.Create.apply(this, arguments);',
    '    pas.program.TObject.Create.call(this);',
    '    pas.program.TObject.CreateWithB.call(this, false);',
    '  };',
    '  this.CreateWithC = function (c) {',
    '    pas.program.TObject.Create.call(this);',
    '    pas.program.TObject.CreateWithB.call(this, true);',
    '    this.DoIt();',
    '    this.DoIt();',
    '    this.$class.DoSome();',
    '  };',
    '  this.DoIt = function () {',
    '    this.Create();',
    '    this.CreateWithB(false);',
    '    this.CreateWithC("c");',
    '  };',
    '  this.DoSome = function () {',
    '    var Result = null;',
    '    Result = this.$create("Create");',
    '    Result = this.$create("CreateWithB", [true]);',
    '    Result = this.$create("CreateWithC", ["c"]);',
    '    return Result;',
    '  };',
    '});'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestClass_ClassVar;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    class var vI: longint;');
  Add('    class var Sub: TObject;');
  Add('    constructor Create;');
  Add('    class function GetIt(Par: longint): tobject;');
  Add('  end;');
  Add('constructor tobject.create;');
  Add('begin');
  Add('  vi:=vi+1;');
  Add('  Self.vi:=Self.vi+1;');
  Add('end;');
  Add('class function tobject.getit(par: longint): tobject;');
  Add('begin');
  Add('  vi:=vi+par;');
  Add('  Self.vi:=Self.vi+par;');
  Add('  Result:=self.sub;');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj:=tobject.create;');
  Add('  tobject.vi:=3;');
  Add('  if tobject.vi=4 then ;');
  Add('  tobject.sub:=nil;');
  Add('  obj.sub:=nil;');
  Add('  obj.sub.sub:=nil;');
  ConvertProgram;
  CheckSource('TestClass_ClassVar',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.vI = 0;',
    '  this.Sub = null;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function(){',
    '    this.$class.vI = this.vI+1;',
    '    this.$class.vI = this.vI+1;',
    '  };',
    '  this.GetIt = function(Par){',
    '    var Result = null;',
    '    this.vI = this.vI + Par;',
    '    this.vI = this.vI + Par;',
    '    Result = this.Sub;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.TObject.$create("Create");',
    'this.TObject.vI = 3;',
    'if (this.TObject.vI == 4);',
    'this.TObject.Sub=null;',
    'this.Obj.$class.Sub=null;',
    'this.Obj.Sub.$class.Sub=null;',
    '']));
end;

procedure TTestModule.TestClass_CallClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    class var vI: longint;');
  Add('    class var Sub: TObject;');
  Add('    constructor Create;');
  Add('    function GetMore(Par: longint): longint;');
  Add('    class function GetIt(Par: longint): tobject;');
  Add('  end;');
  Add('constructor tobject.create;');
  Add('begin');
  Add('  sub:=getit(3);');
  Add('  vi:=getmore(4);');
  Add('  sub:=Self.getit(5);');
  Add('  vi:=Self.getmore(6);');
  Add('end;');
  Add('function tobject.getmore(par: longint): longint;');
  Add('begin');
  Add('  sub:=getit(11);');
  Add('  vi:=getmore(12);');
  Add('  sub:=self.getit(13);');
  Add('  vi:=self.getmore(14);');
  Add('end;');
  Add('class function tobject.getit(par: longint): tobject;');
  Add('begin');
  Add('  sub:=getit(21);');
  Add('  vi:=sub.getmore(22);');
  Add('  sub:=self.getit(23);');
  Add('  vi:=self.sub.getmore(24);');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj:=tobject.create;');
  Add('  tobject.getit(5);');
  Add('  obj.getit(6);');
  Add('  obj.sub.getit(7);');
  Add('  obj.sub.getit(8).SUB:=nil;');
  Add('  obj.sub.getit(9).GETIT(10);');
  Add('  obj.sub.getit(11).SuB.getit(12);');
  ConvertProgram;
  CheckSource('TestClass_CallClassMethod',
    LinesToStr([ // statements
    'rtl.createClass(this,"TObject",null,function(){',
    '  this.vI = 0;',
    '  this.Sub = null;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function(){',
    '    this.$class.Sub = this.$class.GetIt(3);',
    '    this.$class.vI = this.GetMore(4);',
    '    this.$class.Sub = this.$class.GetIt(5);',
    '    this.$class.vI = this.GetMore(6);',
    '  };',
    '  this.GetMore = function(Par){',
    '    var Result = 0;',
    '    this.$class.Sub = this.$class.GetIt(11);',
    '    this.$class.vI = this.GetMore(12);',
    '    this.$class.Sub = this.$class.GetIt(13);',
    '    this.$class.vI = this.GetMore(14);',
    '    return Result;',
    '  };',
    '  this.GetIt = function(Par){',
    '    var Result = null;',
    '    this.Sub = this.GetIt(21);',
    '    this.vI = this.Sub.GetMore(22);',
    '    this.Sub = this.GetIt(23);',
    '    this.vI = this.Sub.GetMore(24);',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.TObject.$create("Create");',
    'this.TObject.GetIt(5);',
    'this.Obj.$class.GetIt(6);',
    'this.Obj.Sub.$class.GetIt(7);',
    'this.Obj.Sub.$class.GetIt(8).$class.Sub=null;',
    'this.Obj.Sub.$class.GetIt(9).$class.GetIt(10);',
    'this.Obj.Sub.$class.GetIt(11).Sub.$class.GetIt(12);',
    '']));
end;

procedure TTestModule.TestClass_Property;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    Fx: longint;');
  Add('    Fy: longint;');
  Add('    function GetInt: longint;');
  Add('    procedure SetInt(Value: longint);');
  Add('    procedure DoIt;');
  Add('    property IntA: longint read Fx write Fy;');
  Add('    property IntB: longint read GetInt write SetInt;');
  Add('  end;');
  Add('function tobject.getint: longint;');
  Add('begin');
  Add('  result:=fx;');
  Add('end;');
  Add('procedure tobject.setint(value: longint);');
  Add('begin');
  Add('  if value=fy then exit;');
  Add('  fy:=value;');
  Add('end;');
  Add('procedure tobject.doit;');
  Add('begin');
  Add('  IntA:=IntA+1;');
  Add('  Self.IntA:=Self.IntA+1;');
  Add('  IntB:=IntB+1;');
  Add('  Self.IntB:=Self.IntB+1;');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj.inta:=obj.inta+1;');
  Add('  if obj.intb=2 then;');
  Add('  obj.intb:=obj.intb+2;');
  Add('  obj.setint(obj.inta);');
  ConvertProgram;
  CheckSource('TestClass_Property',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.Fx = 0;',
    '    this.Fy = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetInt = function () {',
    '    var Result = 0;',
    '    Result = this.Fx;',
    '    return Result;',
    '  };',
    '  this.SetInt = function (Value) {',
    '    if (Value == this.Fy) return;',
    '    this.Fy = Value;',
    '  };',
    '  this.DoIt = function () {',
    '    this.Fy = this.Fx + 1;',
    '    this.Fy = this.Fx + 1;',
    '    this.SetInt(this.GetInt() + 1);',
    '    this.SetInt(this.GetInt() + 1);',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj.Fy = this.Obj.Fx + 1;',
    'if (this.Obj.GetInt() == 2);',
    'this.Obj.SetInt(this.Obj.GetInt() + 2);',
    'this.Obj.SetInt(this.Obj.Fx);'
    ]));
end;

procedure TTestModule.TestClass_Property_ClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var Fx: longint;');
  Add('    class var Fy: longint;');
  Add('    class function GetInt: longint;');
  Add('    class procedure SetInt(Value: longint);');
  Add('    class procedure DoIt;');
  Add('    class property IntA: longint read Fx write Fy;');
  Add('    class property IntB: longint read GetInt write SetInt;');
  Add('  end;');
  Add('class function tobject.getint: longint;');
  Add('begin');
  Add('  result:=fx;');
  Add('end;');
  Add('class procedure tobject.setint(value: longint);');
  Add('begin');
  Add('end;');
  Add('class procedure tobject.doit;');
  Add('begin');
  Add('  IntA:=IntA+1;');
  Add('  Self.IntA:=Self.IntA+1;');
  Add('  IntB:=IntB+1;');
  Add('  Self.IntB:=Self.IntB+1;');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  tobject.inta:=tobject.inta+1;');
  Add('  if tobject.intb=2 then;');
  Add('  tobject.intb:=tobject.intb+2;');
  Add('  tobject.setint(tobject.inta);');
  Add('  obj.inta:=obj.inta+1;');
  Add('  if obj.intb=2 then;');
  Add('  obj.intb:=obj.intb+2;');
  Add('  obj.setint(obj.inta);');
  ConvertProgram;
  CheckSource('TestClass_Property_ClassMethod',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.Fx = 0;',
    '  this.Fy = 0;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetInt = function () {',
    '    var Result = 0;',
    '    Result = this.Fx;',
    '    return Result;',
    '  };',
    '  this.SetInt = function (Value) {',
    '  };',
    '  this.DoIt = function () {',
    '    this.Fy = this.Fx + 1;',
    '    this.Fy = this.Fx + 1;',
    '    this.SetInt(this.GetInt() + 1);',
    '    this.SetInt(this.GetInt() + 1);',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.TObject.Fy = this.TObject.Fx + 1;',
    'if (this.TObject.GetInt() == 2);',
    'this.TObject.SetInt(this.TObject.GetInt() + 2);',
    'this.TObject.SetInt(this.TObject.Fx);',
    'this.Obj.$class.Fy = this.Obj.Fx + 1;',
    'if (this.Obj.$class.GetInt() == 2);',
    'this.Obj.$class.SetInt(this.Obj.$class.GetInt() + 2);',
    'this.Obj.$class.SetInt(this.Obj.Fx);'
    ]));
end;

procedure TTestModule.TestClass_Property_Index;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FItems: array of longint;');
  Add('    function GetItems(Index: longint): longint;');
  Add('    procedure SetItems(Index: longint; Value: longint);');
  Add('    procedure DoIt;');
  Add('    property Items[Index: longint]: longint read getitems write setitems;');
  Add('  end;');
  Add('function tobject.getitems(index: longint): longint;');
  Add('begin');
  Add('  Result:=fitems[index];');
  Add('end;');
  Add('procedure tobject.setitems(index: longint; value: longint);');
  Add('begin');
  Add('  fitems[index]:=value;');
  Add('end;');
  Add('procedure tobject.doit;');
  Add('begin');
  Add('  items[1]:=2;');
  Add('  items[3]:=items[4];');
  Add('  self.items[5]:=self.items[6];');
  Add('  items[items[7]]:=items[items[8]];');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj.Items[11]:=obj.Items[12];');
  ConvertProgram;
  CheckSource('TestClass_Property_Index',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FItems = [];',
    '  };',
    '  this.$final = function () {',
    '    this.FItems = undefined;',
    '  };',
    '  this.GetItems = function (Index) {',
    '    var Result = 0;',
    '    Result = this.FItems[Index];',
    '    return Result;',
    '  };',
    '  this.SetItems = function (Index, Value) {',
    '    this.FItems[Index] = Value;',
    '  };',
    '  this.DoIt = function () {',
    '    this.SetItems(1, 2);',
    '    this.SetItems(3,this.GetItems(4));',
    '    this.SetItems(5,this.GetItems(6));',
    '    this.SetItems(this.GetItems(7), this.GetItems(this.GetItems(8)));',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj.SetItems(11,this.Obj.GetItems(12));'
    ]));
end;

procedure TTestModule.TestClass_PropertyOfTypeArray;
begin
  StartProgram(false);
  Add('type');
  Add('  TArray = array of longint;');
  Add('  TObject = class');
  Add('    FItems: TArray;');
  Add('    function GetItems: tarray;');
  Add('    procedure SetItems(Value: tarray);');
  Add('    property Items: tarray read getitems write setitems;');
  Add('  end;');
  Add('function tobject.getitems: tarray;');
  Add('begin');
  Add('  Result:=fitems;');
  Add('end;');
  Add('procedure tobject.setitems(value: tarray);');
  Add('begin');
  Add('  fitems:=value;');
  Add('  fitems:=nil;');
  Add('  Items:=nil;');
  Add('  Items:=Items;');
  Add('  Items[1]:=2;');
  Add('  fitems[3]:=Items[4];');
  Add('  Items[5]:=Items[6];');
  Add('  Self.Items[7]:=8;');
  Add('  Self.Items[9]:=Self.Items[10];');
  Add('  Items[Items[11]]:=Items[Items[12]];');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj.items:=nil;');
  Add('  obj.items:=obj.items;');
  Add('  obj.items[11]:=obj.items[12];');
  ConvertProgram;
  CheckSource('TestClass_PropertyOfTypeArray',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FItems = [];',
    '  };',
    '  this.$final = function () {',
    '    this.FItems = undefined;',
    '  };',
    '  this.GetItems = function () {',
    '    var Result = [];',
    '    Result = this.FItems;',
    '    return Result;',
    '  };',
    '  this.SetItems = function (Value) {',
    '    this.FItems = Value;',
    '    this.FItems = [];',
    '    this.SetItems([]);',
    '    this.SetItems(this.GetItems());',
    '    this.GetItems()[1] = 2;',
    '    this.FItems[3] = this.GetItems()[4];',
    '    this.GetItems()[5] = this.GetItems()[6];',
    '    this.GetItems()[7] = 8;',
    '    this.GetItems()[9] = this.GetItems()[10];',
    '    this.GetItems()[this.GetItems()[11]] = this.GetItems()[this.GetItems()[12]];',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj.SetItems([]);',
    'this.Obj.SetItems(this.Obj.GetItems());',
    'this.Obj.GetItems()[11] = this.Obj.GetItems()[12];'
    ]));
end;

procedure TTestModule.TestClass_PropertyDefault;
begin
  StartProgram(false);
  Add('type');
  Add('  TArray = array of longint;');
  Add('  TObject = class');
  Add('    FItems: TArray;');
  Add('    function GetItems(Index: longint): longint;');
  Add('    procedure SetItems(Index, Value: longint);');
  Add('    property Items[Index: longint]: longint read getitems write setitems; default;');
  Add('  end;');
  Add('function tobject.getitems(index: longint): longint;');
  Add('begin');
  Add('end;');
  Add('procedure tobject.setitems(index, value: longint);');
  Add('begin');
  Add('  Self[1]:=2;');
  Add('  Self[3]:=Self[index];');
  Add('  Self[index]:=Self[Self[value]];');
  Add('  Self[Self[4]]:=value;');
  Add('end;');
  Add('var Obj: tobject;');
  Add('begin');
  Add('  obj[11]:=12;');
  Add('  obj[13]:=obj[14];');
  Add('  obj[obj[15]]:=obj[obj[15]];');
  ConvertProgram;
  CheckSource('TestClass_PropertyDefault',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FItems = [];',
    '  };',
    '  this.$final = function () {',
    '    this.FItems = undefined;',
    '  };',
    '  this.GetItems = function (Index) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetItems = function (Index, Value) {',
    '    this.SetItems(1, 2);',
    '    this.SetItems(3, this.GetItems(Index));',
    '    this.SetItems(Index, this.GetItems(this.GetItems(Value)));',
    '    this.SetItems(this.GetItems(4), Value);',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj.SetItems(11, 12);',
    'this.Obj.SetItems(13, this.Obj.GetItems(14));',
    'this.Obj.SetItems(this.Obj.GetItems(15), this.Obj.GetItems(this.Obj.GetItems(15)));'
    ]));
end;

procedure TTestModule.TestClass_PropertyOverride;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TObject = class');
  Add('    FItem: integer;');
  Add('    function GetItem: integer; external name ''GetItem'';');
  Add('    procedure SetItem(Value: integer); external name ''SetItem'';');
  Add('    property Item: integer read getitem write setitem;');
  Add('  end;');
  Add('  TCar = class');
  Add('    FBag: integer;');
  Add('    function GetBag: integer; external name ''GetBag'';');
  Add('    property Item read getbag;');
  Add('  end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  Car: tcar;');
  Add('begin');
  Add('  Obj.Item:=Obj.Item;');
  Add('  Car.Item:=Car.Item;');
  ConvertProgram;
  CheckSource('TestClass_PropertyOverride',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FItem = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TCar", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '    this.FBag = 0;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.Car = null;',
    '']),
    LinesToStr([ // this.$main
    'this.Obj.SetItem(this.Obj.GetItem());',
    'this.Car.SetItem(this.Car.GetBag());',
    '']));
end;

procedure TTestModule.TestClass_Assigned;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  b: boolean;');
  Add('begin');
  Add('  if Assigned(obj) then ;');
  Add('  b:=Assigned(obj) or false;');
  ConvertProgram;
  CheckSource('TestClass_Assigned',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.b = false;'
    ]),
    LinesToStr([ // this.$main
    'if (this.Obj != null);',
    'this.b = (this.Obj != null) || false;'
    ]));
end;

procedure TTestModule.TestClass_WithClassDoCreate;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    aBool: boolean;');
  Add('    Arr: array of boolean;');
  Add('    constructor Create;');
  Add('  end;');
  Add('constructor TObject.Create; begin end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  b: boolean;');
  Add('begin');
  Add('  with tobject.create do begin');
  Add('    b:=abool;');
  Add('    abool:=b;');
  Add('    b:=arr[1];');
  Add('    arr[2]:=b;');
  Add('  end;');
  Add('  with tobject do');
  Add('    obj:=create;');
  Add('  with obj do begin');
  Add('    create;');
  Add('    b:=abool;');
  Add('    abool:=b;');
  Add('    b:=arr[3];');
  Add('    arr[4]:=b;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestClass_WithClassDoCreate',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.aBool = false;',
    '    this.Arr = [];',
    '  };',
    '  this.$final = function () {',
    '    this.Arr = undefined;',
    '  };',
    '  this.Create = function () {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.b = false;'
    ]),
    LinesToStr([ // this.$main
    'var $with1 = this.TObject.$create("Create");',
    'this.b = $with1.aBool;',
    '$with1.aBool = this.b;',
    'this.b = $with1.Arr[1];',
    '$with1.Arr[2] = this.b;',
    'var $with2 = this.TObject;',
    'this.Obj = $with2.$create("Create");',
    'var $with3 = this.Obj;',
    '$with3.Create();',
    'this.b = $with3.aBool;',
    '$with3.aBool = this.b;',
    'this.b = $with3.Arr[3];',
    '$with3.Arr[4] = this.b;',
    '']));
end;

procedure TTestModule.TestClass_WithClassInstDoProperty;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FInt: longint;');
  Add('    constructor Create;');
  Add('    function GetSize: longint;');
  Add('    procedure SetSize(Value: longint);');
  Add('    property Int: longint read FInt write FInt;');
  Add('    property Size: longint read GetSize write SetSize;');
  Add('  end;');
  Add('constructor TObject.Create; begin end;');
  Add('function TObject.GetSize: longint; begin; end;');
  Add('procedure TObject.SetSize(Value: longint); begin; end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  i: longint;');
  Add('begin');
  Add('  with TObject.Create do begin');
  Add('    i:=int;');
  Add('    int:=i;');
  Add('    i:=size;');
  Add('    size:=i;');
  Add('  end;');
  Add('  with obj do begin');
  Add('    i:=int;');
  Add('    int:=i;');
  Add('    i:=size;');
  Add('    size:=i;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestClass_WithClassInstDoProperty',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FInt = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '  this.GetSize = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetSize = function (Value) {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $with1 = this.TObject.$create("Create");',
    'this.i = $with1.FInt;',
    '$with1.FInt = this.i;',
    'this.i = $with1.GetSize();',
    '$with1.SetSize(this.i);',
    'var $with2 = this.Obj;',
    'this.i = $with2.FInt;',
    '$with2.FInt = this.i;',
    'this.i = $with2.GetSize();',
    '$with2.SetSize(this.i);',
    '']));
end;

procedure TTestModule.TestClass_WithClassInstDoPropertyWithParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create;');
  Add('    function GetItems(Index: longint): longint;');
  Add('    procedure SetItems(Index, Value: longint);');
  Add('    property Items[Index: longint]: longint read GetItems write SetItems;');
  Add('  end;');
  Add('constructor TObject.Create; begin end;');
  Add('function tobject.getitems(index: longint): longint; begin; end;');
  Add('procedure tobject.setitems(index, value: longint); begin; end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  i: longint;');
  Add('begin');
  Add('  with TObject.Create do begin');
  Add('    i:=Items[1];');
  Add('    Items[2]:=i;');
  Add('  end;');
  Add('  with obj do begin');
  Add('    i:=Items[3];');
  Add('    Items[4]:=i;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestClass_WithClassInstDoPropertyWithParams',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '  this.GetItems = function (Index) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetItems = function (Index, Value) {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $with1 = this.TObject.$create("Create");',
    'this.i = $with1.GetItems(1);',
    '$with1.SetItems(2, this.i);',
    'var $with2 = this.Obj;',
    'this.i = $with2.GetItems(3);',
    '$with2.SetItems(4, this.i);',
    '']));
end;

procedure TTestModule.TestClass_WithClassInstDoFunc;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create;');
  Add('    function GetSize: longint;');
  Add('    procedure SetSize(Value: longint);');
  Add('  end;');
  Add('constructor TObject.Create; begin end;');
  Add('function TObject.GetSize: longint; begin; end;');
  Add('procedure TObject.SetSize(Value: longint); begin; end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  i: longint;');
  Add('begin');
  Add('  with TObject.Create do begin');
  Add('    i:=GetSize;');
  Add('    i:=GetSize();');
  Add('    SetSize(i);');
  Add('  end;');
  Add('  with obj do begin');
  Add('    i:=GetSize;');
  Add('    i:=GetSize();');
  Add('    SetSize(i);');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestClass_WithClassInstDoFunc',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '  this.GetSize = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetSize = function (Value) {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $with1 = this.TObject.$create("Create");',
    'this.i = $with1.GetSize();',
    'this.i = $with1.GetSize();',
    '$with1.SetSize(this.i);',
    'var $with2 = this.Obj;',
    'this.i = $with2.GetSize();',
    'this.i = $with2.GetSize();',
    '$with2.SetSize(this.i);',
    '']));
end;

procedure TTestModule.TestClass_TypeCast;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    Next: TObject;');
  Add('    constructor Create;');
  Add('  end;');
  Add('  TControl = class(TObject)');
  Add('    Arr: array of TObject;');
  Add('    function GetIt(vI: longint = 0): TObject;');
  Add('  end;');
  Add('constructor tobject.create; begin end;');
  Add('function tcontrol.getit(vi: longint = 0): tobject; begin end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('begin');
  Add('  obj:=tcontrol(obj).next;');
  Add('  tcontrol(obj):=nil;');
  Add('  obj:=tcontrol(obj);');
  Add('  tcontrol(obj):=tcontrol(tcontrol(obj).getit);');
  Add('  tcontrol(obj):=tcontrol(tcontrol(obj).getit());');
  Add('  tcontrol(obj):=tcontrol(tcontrol(obj).getit(1));');
  Add('  tcontrol(obj):=tcontrol(tcontrol(tcontrol(obj).getit).arr[2]);');
  ConvertProgram;
  CheckSource('TestClass_TypeCast',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.Next = null;',
    '  };',
    '  this.$final = function () {',
    '    this.Next = undefined;',
    '  };',
    '  this.Create = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TControl", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '    this.Arr = [];',
    '  };',
    '  this.$final = function () {',
    '    this.Arr = undefined;',
    '    pas.program.TObject.$final.call(this);',
    '  };',
    '  this.GetIt = function (vI) {',
    '    var Result = null;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.Obj.Next;',
    'this.Obj = null;',
    'this.Obj = this.Obj;',
    'this.Obj = this.Obj.GetIt(0);',
    'this.Obj = this.Obj.GetIt(0);',
    'this.Obj = this.Obj.GetIt(1);',
    'this.Obj = this.Obj.GetIt(0).Arr[2];',
    '']));
end;

procedure TTestModule.TestClass_TypeCastUntypedParam;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('procedure ProcA(var A);');
  Add('begin');
  Add('  TObject(A):=nil;');
  Add('  TObject(A):=TObject(A);');
  Add('  if TObject(A)=nil then ;');
  Add('  if nil=TObject(A) then ;');
  Add('end;');
  Add('procedure ProcB(out A);');
  Add('begin');
  Add('  TObject(A):=nil;');
  Add('  TObject(A):=TObject(A);');
  Add('  if TObject(A)=nil then ;');
  Add('  if nil=TObject(A) then ;');
  Add('end;');
  Add('procedure ProcC(const A);');
  Add('begin');
  Add('  if TObject(A)=nil then ;');
  Add('  if nil=TObject(A) then ;');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  ProcA(o);');
  Add('  ProcB(o);');
  Add('  ProcC(o);');
  ConvertProgram;
  CheckSource('TestClass_TypeCastUntypedParam',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.ProcA = function (A) {',
    '  A.set(null);',
    '  A.set(A.get());',
    '  if (A.get() == null);',
    '  if (null == A.get());',
    '};',
    'this.ProcB = function (A) {',
    '  A.set(null);',
    '  A.set(A.get());',
    '  if (A.get() == null);',
    '  if (null == A.get());',
    '};',
    'this.ProcC = function (A) {',
    '  if (A == null);',
    '  if (null == A);',
    '};',
    'this.o = null;',
    '']),
    LinesToStr([ // this.$main
    'this.ProcA({',
    '  p: this,',
    '  get: function () {',
    '      return this.p.o;',
    '    },',
    '  set: function (v) {',
    '      this.p.o = v;',
    '    }',
    '});',
    'this.ProcB({',
    '  p: this,',
    '  get: function () {',
    '      return this.p.o;',
    '    },',
    '  set: function (v) {',
    '      this.p.o = v;',
    '    }',
    '});',
    'this.ProcC(this.o);',
    '']));
end;

procedure TTestModule.TestClass_Overloads;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt;');
  Add('    procedure DoIt(vI: longint);');
  Add('  end;');
  Add('procedure TObject.DoIt;');
  Add('begin');
  Add('  DoIt;');
  Add('  DoIt(1);');
  Add('end;');
  Add('procedure TObject.DoIt(vI: longint); begin end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_Overloads',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '    this.DoIt();',
    '    this.DoIt$1(1);',
    '  };',
    '  this.DoIt$1 = function (vI) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestClass_OverloadsAncestor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class;');
  Add('  TObject = class');
  Add('    procedure DoIt(vA: longint);');
  Add('    procedure DoIt(vA, vB: longint);');
  Add('  end;');
  Add('  TCar = class;');
  Add('  TCar = class');
  Add('    procedure DoIt(vA: longint);');
  Add('    procedure DoIt(vA, vB: longint);');
  Add('  end;');
  Add('procedure tobject.doit(va: longint);');
  Add('begin');
  Add('  doit(1);');
  Add('  doit(1,2);');
  Add('end;');
  Add('procedure tobject.doit(va, vb: longint); begin end;');
  Add('procedure tcar.doit(va: longint);');
  Add('begin');
  Add('  doit(1);');
  Add('  doit(1,2);');
  Add('  inherited doit(1);');
  Add('  inherited doit(1,2);');
  Add('end;');
  Add('procedure tcar.doit(va, vb: longint); begin end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_OverloadsAncestor',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (vA) {',
    '    this.DoIt(1);',
    '    this.DoIt$1(1,2);',
    '  };',
    '  this.DoIt$1 = function (vA, vB) {',
    '  };',
    '});',
    'rtl.createClass(this, "TCar", this.TObject, function () {',
    '  this.DoIt$2 = function (vA) {',
    '    this.DoIt$2(1);',
    '    this.DoIt$3(1, 2);',
    '    pas.program.TObject.DoIt.call(this, 1);',
    '    pas.program.TObject.DoIt$1.call(this, 1, 2);',
    '  };',
    '  this.DoIt$3 = function (vA, vB) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestClass_OverloadConstructor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create(vA: longint);');
  Add('    constructor Create(vA, vB: longint);');
  Add('  end;');
  Add('  TCar = class');
  Add('    constructor Create(vA: longint);');
  Add('    constructor Create(vA, vB: longint);');
  Add('  end;');
  Add('constructor tobject.create(va: longint);');
  Add('begin');
  Add('  create(1);');
  Add('  create(1,2);');
  Add('end;');
  Add('constructor tobject.create(va, vb: longint); begin end;');
  Add('constructor tcar.create(va: longint);');
  Add('begin');
  Add('  create(1);');
  Add('  create(1,2);');
  Add('  inherited create(1);');
  Add('  inherited create(1,2);');
  Add('end;');
  Add('constructor tcar.create(va, vb: longint); begin end;');
  Add('begin');
  Add('  tobject.create(1);');
  Add('  tobject.create(1,2);');
  Add('  tcar.create(1);');
  Add('  tcar.create(1,2);');
  ConvertProgram;
  CheckSource('TestClass_OverloadConstructor',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function (vA) {',
    '    this.Create(1);',
    '    this.Create$1(1,2);',
    '  };',
    '  this.Create$1 = function (vA, vB) {',
    '  };',
    '});',
    'rtl.createClass(this, "TCar", this.TObject, function () {',
    '  this.Create$2 = function (vA) {',
    '    this.Create$2(1);',
    '    this.Create$3(1, 2);',
    '    pas.program.TObject.Create.call(this, 1);',
    '    pas.program.TObject.Create$1.call(this, 1, 2);',
    '  };',
    '  this.Create$3 = function (vA, vB) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    'this.TObject.$create("Create", [1]);',
    'this.TObject.$create("Create$1", [1, 2]);',
    'this.TCar.$create("Create$2", [1]);',
    'this.TCar.$create("Create$3", [1, 2]);',
    '']));
end;

procedure TTestModule.TestClass_ReintroducedVar;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  strict private');
  Add('    Some: longint;');
  Add('  end;');
  Add('  TMobile = class');
  Add('  strict private');
  Add('    Some: string;');
  Add('  end;');
  Add('  TCar = class(tmobile)');
  Add('    procedure Some;');
  Add('    procedure Some(vA: longint);');
  Add('  end;');
  Add('procedure tcar.some;');
  Add('begin');
  Add('  Some;');
  Add('  Some(1);');
  Add('end;');
  Add('procedure tcar.some(va: longint); begin end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_ReintroducedVar',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.Some = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TMobile", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '    this.Some$1 = "";',
    '  };',
    '});',
    'rtl.createClass(this, "TCar", this.TMobile, function () {',
    '  this.Some$2 = function () {',
    '    this.Some$2();',
    '    this.Some$3(1);',
    '  };',
    '  this.Some$3 = function (vA) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestClass_RaiseDescendant;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create(Msg: string);');
  Add('  end;');
  Add('  Exception = class');
  Add('  end;');
  Add('  EConvertError = class(Exception)');
  Add('  end;');
  Add('constructor TObject.Create(Msg: string); begin end;');
  Add('begin');
  Add('  raise Exception.Create(''Bar1'');');
  Add('  raise EConvertError.Create(''Bar2'');');
  ConvertProgram;
  CheckSource('TestClass_RaiseDescendant',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function (Msg) {',
    '  };',
    '});',
    'rtl.createClass(this, "Exception", this.TObject, function () {',
    '});',
    'rtl.createClass(this, "EConvertError", this.Exception, function () {',
    '});',
    '']),
    LinesToStr([ // this.$main
    'throw this.Exception.$create("Create",["Bar1"]);',
    'throw this.EConvertError.$create("Create",["Bar2"]);',
    '']));
end;

procedure TTestModule.TestClass_ExternalMethod;
begin
  AddModuleWithIntfImplSrc('unit2.pas',
    LinesToStr([
    'type',
    '  TObject = class',
    '  public',
    '    procedure Intern; external name ''$DoIntern'';',
    '  end;',
    '']),
    LinesToStr([
    '']));

  StartUnit(true);
  Add('interface');
  Add('uses unit2;');
  Add('type');
  Add('  TCar = class(TObject)');
  Add('  public');
  Add('    procedure Intern2; external name ''$DoIntern2'';');
  Add('    procedure DoIt;');
  Add('  end;');
  Add('implementation');
  Add('procedure tcar.doit;');
  Add('begin');
  Add('  Intern;');
  Add('  Intern();');
  Add('  Intern2;');
  Add('  Intern2();');
  Add('end;');
  Add('var Obj: TCar;');
  Add('begin');
  Add('  obj.intern;');
  Add('  obj.intern();');
  Add('  obj.intern2;');
  Add('  obj.intern2();');
  Add('  obj.doit;');
  Add('  obj.doit();');
  Add('  with obj do begin');
  Add('    Intern;');
  Add('    Intern();');
  Add('    Intern2;');
  Add('    Intern2();');
  Add('  end;');
  ConvertUnit;
  CheckSource('TestClass_ExternalMethod',
    LinesToStr([
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    'rtl.createClass(this, "TCar", pas.unit2.TObject, function () {',
    '    this.DoIt = function () {',
    '      this.$DoIntern();',
    '      this.$DoIntern();',
    '      this.$DoIntern2();',
    '      this.$DoIntern2();',
    '    };',
    '  });',
    '$impl.Obj = null;',
    '']),
    LinesToStr([
    '$impl.Obj.$DoIntern();',
    '$impl.Obj.$DoIntern();',
    '$impl.Obj.$DoIntern2();',
    '$impl.Obj.$DoIntern2();',
    '$impl.Obj.DoIt();',
    '$impl.Obj.DoIt();',
    'var $with1 = $impl.Obj;',
    '$with1.$DoIntern();',
    '$with1.$DoIntern();',
    '$with1.$DoIntern2();',
    '$with1.$DoIntern2();',
    '']));
end;

procedure TTestModule.TestClass_ExternalVirtualNameMismatchFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt; virtual; external name ''Foo'';');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError('Virtual method name must match external',
    nVirtualMethodNameMustMatchExternal);
  ConvertProgram;
end;

procedure TTestModule.TestClass_ExternalOverrideFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt; virtual; external name ''DoIt'';');
  Add('  end;');
  Add('  TCar = class');
  Add('    procedure DoIt; override; external name ''DoIt'';');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError('Invalid procedure modifiers override,external',
    nInvalidXModifiersY);
  ConvertProgram;
end;

procedure TTestModule.TestClass_ExternalVar;
begin
  AddModuleWithIntfImplSrc('unit2.pas',
    LinesToStr([
    '{$modeswitch externalclass}',
    'type',
    '  TObject = class',
    '  public',
    '    Intern: longint external name ''$Intern'';',
    '  end;',
    '']),
    LinesToStr([
    '']));

  StartUnit(true);
  Add('interface');
  Add('uses unit2;');
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TCar = class(tobject)');
  Add('  public');
  Add('    Intern2: longint external name ''$Intern2'';');
  Add('    procedure DoIt;');
  Add('  end;');
  Add('implementation');
  Add('procedure tcar.doit;');
  Add('begin');
  Add('  Intern:=Intern+1;');
  Add('  Intern2:=Intern2+2;');
  Add('end;');
  Add('var Obj: TCar;');
  Add('begin');
  Add('  obj.intern:=obj.intern+1;');
  Add('  obj.intern2:=obj.intern2+2;');
  Add('  with obj do begin');
  Add('    intern:=intern+1;');
  Add('    intern2:=intern2+2;');
  Add('  end;');
  ConvertUnit;
  CheckSource('TestClass_ExternalVar',
    LinesToStr([
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    'rtl.createClass(this, "TCar", pas.unit2.TObject, function () {',
    '    this.DoIt = function () {',
    '      this.$Intern = this.$Intern + 1;',
    '      this.$Intern2 = this.$Intern2 + 2;',
    '    };',
    '  });',
    '$impl.Obj = null;',
    '']),
    LinesToStr([
    '$impl.Obj.$Intern = $impl.Obj.$Intern + 1;',
    '$impl.Obj.$Intern2 = $impl.Obj.$Intern2 + 2;',
    'var $with1 = $impl.Obj;',
    '$with1.$Intern = $with1.$Intern + 1;',
    '$with1.$Intern2 = $with1.$Intern2 + 2;',
    '']));
end;

procedure TTestModule.TestClassOf_Create;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create;');
  Add('  end;');
  Add('  TClass = class of TObject;');
  Add('constructor tobject.create; begin end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  C: tclass;');
  Add('begin');
  Add('  obj:=C.create;');
  Add('  with c do obj:=create;');
  ConvertProgram;
  CheckSource('TestClassOf_Create',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.C = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj = this.C.$create("Create");',
    'var $with1 = this.C;',
    'this.Obj = $with1.$create("Create");',
    '']));
end;

procedure TTestModule.TestClassOf_Call;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure DoIt;');
  Add('  end;');
  Add('  TClass = class of TObject;');
  Add('class procedure tobject.doit; begin end;');
  Add('var');
  Add('  C: tclass;');
  Add('begin');
  Add('  c.doit;');
  Add('  with c do doit;');
  ConvertProgram;
  CheckSource('TestClassOf_Call',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '  };',
    '});',
    'this.C = null;'
    ]),
    LinesToStr([ // this.$main
    'this.C.DoIt();',
    'var $with1 = this.C;',
    '$with1.DoIt();',
    '']));
end;

procedure TTestModule.TestClassOf_Assign;
begin
  StartProgram(false);
  Add('type');
  Add('  TClass = class of TObject;');
  Add('  TObject = class');
  Add('    ClassType: TClass; ');
  Add('  end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  C: tclass;');
  Add('begin');
  Add('  c:=nil;');
  Add('  c:=obj.classtype;');
  ConvertProgram;
  CheckSource('TestClassOf_Assign',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.ClassType = null;',
    '  };',
    '  this.$final = function () {',
    '    this.ClassType = undefined;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.C = null;'
    ]),
    LinesToStr([ // this.$main
    'this.C = null;',
    'this.C = this.Obj.ClassType;',
    '']));
end;

procedure TTestModule.TestClassOf_Is;
begin
  StartProgram(false);
  Add('type');
  Add('  TClass = class of TObject;');
  Add('  TObject = class');
  Add('  end;');
  Add('  TCar = class');
  Add('  end;');
  Add('  TCars = class of TCar;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  C: tclass;');
  Add('  Cars: tcars;');
  Add('begin');
  Add('  if c is tcar then ;');
  Add('  if c is tcars then ;');
  ConvertProgram;
  CheckSource('TestClassOf_Is',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TCar", this.TObject, function () {',
    '});',
    'this.Obj = null;',
    'this.C = null;',
    'this.Cars = null;'
    ]),
    LinesToStr([ // this.$main
    'if(rtl.is(this.C,this.TCar));',
    'if(rtl.is(this.C,this.TCar));',
    '']));
end;

procedure TTestModule.TestClassOf_Compare;
begin
  StartProgram(false);
  Add('type');
  Add('  TClass = class of TObject;');
  Add('  TObject = class');
  Add('    ClassType: TClass; ');
  Add('  end;');
  Add('var');
  Add('  b: boolean;');
  Add('  Obj: tobject;');
  Add('  C: tclass;');
  Add('begin');
  Add('  b:=c=nil;');
  Add('  b:=nil=c;');
  Add('  b:=c=obj.classtype;');
  Add('  b:=obj.classtype=c;');
  Add('  b:=c=TObject;');
  Add('  b:=TObject=c;');
  Add('  b:=c<>nil;');
  Add('  b:=nil<>c;');
  Add('  b:=c<>obj.classtype;');
  Add('  b:=obj.classtype<>c;');
  Add('  b:=c<>TObject;');
  Add('  b:=TObject<>c;');
  ConvertProgram;
  CheckSource('TestClassOf_Compare',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.ClassType = null;',
    '  };',
    '  this.$final = function () {',
    '    this.ClassType = undefined;',
    '  };',
    '});',
    'this.b = false;',
    'this.Obj = null;',
    'this.C = null;'
    ]),
    LinesToStr([ // this.$main
    'this.b = this.C == null;',
    'this.b = null == this.C;',
    'this.b = this.C == this.Obj.ClassType;',
    'this.b = this.Obj.ClassType == this.C;',
    'this.b = this.C == this.TObject;',
    'this.b = this.TObject == this.C;',
    'this.b = this.C != null;',
    'this.b = null != this.C;',
    'this.b = this.C != this.Obj.ClassType;',
    'this.b = this.Obj.ClassType != this.C;',
    'this.b = this.C != this.TObject;',
    'this.b = this.TObject != this.C;',
    '']));
end;

procedure TTestModule.TestClassOf_ClassVar;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var id: longint;');
  Add('  end;');
  Add('  TClass = class of TObject;');
  Add('var');
  Add('  C: tclass;');
  Add('begin');
  Add('  C.id:=C.id;');
  ConvertProgram;
  CheckSource('TestClassOf_ClassVar',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.id = 0;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.C = null;'
    ]),
    LinesToStr([ // this.$main
    'this.C.id = this.C.id;',
    '']));
end;

procedure TTestModule.TestClassOf_ClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class function DoIt(i: longint = 0): longint;');
  Add('  end;');
  Add('  TClass = class of TObject;');
  Add('class function tobject.doit(i: longint = 0): longint; begin end;');
  Add('var');
  Add('  i: longint;');
  Add('  C: tclass;');
  Add('begin');
  Add('  C.DoIt;');
  Add('  C.DoIt();');
  Add('  i:=C.DoIt;');
  Add('  i:=C.DoIt();');
  ConvertProgram;
  CheckSource('TestClassOf_ClassMethod',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (i) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.i = 0;',
    'this.C = null;'
    ]),
    LinesToStr([ // this.$main
    'this.C.DoIt(0);',
    'this.C.DoIt(0);',
    'this.i = this.C.DoIt(0);',
    'this.i = this.C.DoIt(0);',
    '']));
end;

procedure TTestModule.TestClassOf_ClassProperty;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var FA: longint;');
  Add('    class function GetA: longint;');
  Add('    class procedure SetA(Value: longint);');
  Add('    class property pA: longint read fa write fa;');
  Add('    class property pB: longint read geta write seta;');
  Add('  end;');
  Add('  TObjectClass = class of tobject;');
  Add('class function tobject.geta: longint; begin end;');
  Add('class procedure tobject.seta(value: longint); begin end;');
  Add('var');
  Add('  b: boolean;');
  Add('  Obj: tobject;');
  Add('  Cla: tobjectclass;');
  Add('begin');
  Add('  obj.pa:=obj.pa;');
  Add('  obj.pb:=obj.pb;');
  Add('  b:=obj.pa=4;');
  Add('  b:=obj.pb=obj.pb;');
  Add('  b:=5=obj.pa;');
  Add('  cla.pa:=6;');
  Add('  cla.pa:=cla.pa;');
  Add('  cla.pb:=cla.pb;');
  Add('  b:=cla.pa=7;');
  Add('  b:=cla.pb=cla.pb;');
  Add('  b:=8=cla.pa;');
  Add('  tobject.pa:=9;');
  Add('  tobject.pb:=tobject.pb;');
  Add('  b:=tobject.pa=10;');
  Add('  b:=11=tobject.pa;');
  ConvertProgram;
  CheckSource('TestClassOf_ClassProperty',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.FA = 0;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetA = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetA = function (Value) {',
    '  };',
    '});',
    'this.b = false;',
    'this.Obj = null;',
    'this.Cla = null;'
    ]),
    LinesToStr([ // this.$main
    'this.Obj.$class.FA = this.Obj.FA;',
    'this.Obj.$class.SetA(this.Obj.$class.GetA());',
    'this.b = this.Obj.FA == 4;',
    'this.b = this.Obj.$class.GetA() == this.Obj.$class.GetA();',
    'this.b = 5 == this.Obj.FA;',
    'this.Cla.FA = 6;',
    'this.Cla.FA = this.Cla.FA;',
    'this.Cla.SetA(this.Cla.GetA());',
    'this.b = this.Cla.FA == 7;',
    'this.b = this.Cla.GetA() == this.Cla.GetA();',
    'this.b = 8 == this.Cla.FA;',
    'this.TObject.FA = 9;',
    'this.TObject.SetA(this.TObject.GetA());',
    'this.b = this.TObject.FA == 10;',
    'this.b = 11 == this.TObject.FA;',
    '']));
end;

procedure TTestModule.TestClassOf_ClassMethodSelf;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var GlobalId: longint;');
  Add('    class procedure ProcA;');
  Add('  end;');
  Add('class procedure tobject.proca;');
  Add('var b: boolean;');
  Add('begin');
  Add('  b:=self=nil;');
  Add('  b:=self.globalid=3;');
  Add('  b:=4=self.globalid;');
  Add('  self.globalid:=5;');
  Add('  self.proca;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClassOf_ClassMethodSelf',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.GlobalId = 0;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcA = function () {',
    '    var b = false;',
    '    b = this == null;',
    '    b = this.GlobalId == 3;',
    '    b = 4 == this.GlobalId;',
    '    this.GlobalId = 5;',
    '    this.ProcA();',
    '  };',
    '});'
    ]),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestClassOf_TypeCast;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure {#TObject_DoIt}DoIt;');
  Add('  end;');
  Add('  TClass = class of TObject;');
  Add('  TMobile = class');
  Add('    class procedure {#TMobile_DoIt}DoIt;');
  Add('  end;');
  Add('  TMobileClass = class of TMobile;');
  Add('  TCar = class(TMobile)');
  Add('    class procedure {#TCar_DoIt}DoIt;');
  Add('  end;');
  Add('  TCarClass = class of TCar;');
  Add('class procedure TObject.DoIt;');
  Add('begin');
  Add('  TClass(Self).{@TObject_DoIt}DoIt;');
  Add('  TMobileClass(Self).{@TMobile_DoIt}DoIt;');
  Add('end;');
  Add('class procedure TMobile.DoIt;');
  Add('begin');
  Add('  TClass(Self).{@TObject_DoIt}DoIt;');
  Add('  TMobileClass(Self).{@TMobile_DoIt}DoIt;');
  Add('  TCarClass(Self).{@TCar_DoIt}DoIt;');
  Add('end;');
  Add('class procedure TCar.DoIt; begin end;');
  Add('var');
  Add('  ObjC: TClass;');
  Add('  MobileC: TMobileClass;');
  Add('  CarC: TCarClass;');
  Add('begin');
  Add('  ObjC.{@TObject_DoIt}DoIt;');
  Add('  MobileC.{@TMobile_DoIt}DoIt;');
  Add('  CarC.{@TCar_DoIt}DoIt;');
  Add('  TClass(ObjC).{@TObject_DoIt}DoIt;');
  Add('  TMobileClass(ObjC).{@TMobile_DoIt}DoIt;');
  Add('  TCarClass(ObjC).{@TCar_DoIt}DoIt;');
  Add('  TClass(MobileC).{@TObject_DoIt}DoIt;');
  Add('  TMobileClass(MobileC).{@TMobile_DoIt}DoIt;');
  Add('  TCarClass(MobileC).{@TCar_DoIt}DoIt;');
  Add('  TClass(CarC).{@TObject_DoIt}DoIt;');
  Add('  TMobileClass(CarC).{@TMobile_DoIt}DoIt;');
  Add('  TCarClass(CarC).{@TCar_DoIt}DoIt;');
  ConvertProgram;
  CheckSource('TestClassOf_TypeCast',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '    this.DoIt();',
    '    this.DoIt$1();',
    '  };',
    '});',
    'rtl.createClass(this, "TMobile", this.TObject, function () {',
    '  this.DoIt$1 = function () {',
    '    this.DoIt();',
    '    this.DoIt$1();',
    '    this.DoIt$2();',
    '  };',
    '});',
    'rtl.createClass(this, "TCar", this.TMobile, function () {',
    '  this.DoIt$2 = function () {',
    '  };',
    '});',
    'this.ObjC = null;',
    'this.MobileC = null;',
    'this.CarC = null;',
    '']),
    LinesToStr([ // this.$main
    'this.ObjC.DoIt();',
    'this.MobileC.DoIt$1();',
    'this.CarC.DoIt$2();',
    'this.ObjC.DoIt();',
    'this.ObjC.DoIt$1();',
    'this.ObjC.DoIt$2();',
    'this.MobileC.DoIt();',
    'this.MobileC.DoIt$1();',
    'this.MobileC.DoIt$2();',
    'this.CarC.DoIt();',
    'this.CarC.DoIt$1();',
    'this.CarC.DoIt$2();',
    '']));
end;

procedure TTestModule.TestClassOf_ImplicitFunctionCall;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function CurNow: longint; ');
  Add('    class function Now: longint; ');
  Add('  end;');
  Add('function TObject.CurNow: longint; begin end;');
  Add('class function TObject.Now: longint; begin end;');
  Add('var');
  Add('  Obj: tobject;');
  Add('  vI: longint;');
  Add('begin');
  Add('  obj.curnow;');
  Add('  vi:=obj.curnow;');
  Add('  tobject.now;');
  Add('  vi:=tobject.now;');
  ConvertProgram;
  CheckSource('TestClassOf_ImplicitFunctionCall',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.CurNow = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.Now = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.vI = 0;',
    '']),
    LinesToStr([ // this.$main
    'this.Obj.CurNow();',
    'this.vI = this.Obj.CurNow();',
    'this.TObject.Now();',
    'this.vI = this.TObject.Now();',
    '']));
end;

procedure TTestModule.TestExternalClass_Var;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtObj''');
  Add('    Id: longint external name ''$Id'';');
  Add('    B: longint;');
  Add('  end;');
  Add('var Obj: TExtA;');
  Add('begin');
  Add('  obj.id:=obj.id+1;');
  Add('  obj.B:=obj.B+1;');
  ConvertProgram;
  CheckSource('TestExternalClass_Var',
    LinesToStr([ // statements
    'this.Obj = null;',
    '']),
    LinesToStr([ // this.$main
    'this.Obj.$Id = this.Obj.$Id + 1;',
    'this.Obj.B = this.Obj.B + 1;',
    '']));
end;

procedure TTestModule.TestExternalClass_DuplicateVarFail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    Id: longint external name ''$Id'';');
  Add('  end;');
  Add('  TExtB = class external ''lib'' name ''ExtB''(TExtA)');
  Add('    Id: longint;');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError('Duplicate identifier "Id" at test1.pp(6,6)',nDuplicateIdentifier);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_Method;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtObj''');
  Add('    procedure DoIt(Id: longint = 1); external name ''$Execute'';');
  Add('    procedure DoSome(Id: longint = 1);');
  Add('  end;');
  Add('var Obj: texta;');
  Add('begin');
  Add('  obj.doit;');
  Add('  obj.doit();');
  Add('  obj.doit(2);');
  Add('  with obj do begin');
  Add('    doit;');
  Add('    doit();');
  Add('    doit(3);');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestExternalClass_Method',
    LinesToStr([ // statements
    'this.Obj = null;',
    '']),
    LinesToStr([ // this.$main
    'this.Obj.$Execute(1);',
    'this.Obj.$Execute(1);',
    'this.Obj.$Execute(2);',
    'var $with1 = this.Obj;',
    '$with1.$Execute(1);',
    '$with1.$Execute(1);',
    '$with1.$Execute(3);',
    '']));
end;

procedure TTestModule.TestExternalClass_NonExternalOverride;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtObjA''');
  Add('    procedure ProcA; virtual;');
  Add('    procedure ProcB; virtual;');
  Add('  end;');
  Add('  TExtB = class external name ''ExtObjB'' (TExtA)');
  Add('  end;');
  Add('  TExtC = class (TExtB)');
  Add('    procedure ProcA; override;');
  Add('  end;');
  Add('procedure TExtC.ProcA;');
  Add('begin');
  Add('  ProcA;');
  Add('  Self.ProcA;');
  Add('  ProcB;');
  Add('  Self.ProcB;');
  Add('end;');
  Add('var');
  Add('  A: texta;');
  Add('  B: textb;');
  Add('  C: textc;');
  Add('begin');
  Add('  a.proca;');
  Add('  b.proca;');
  Add('  c.proca;');
  ConvertProgram;
  CheckSource('TestExternalClass_NonExternalOverride',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TExtC", ExtObjB, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcA = function () {',
    '    this.ProcA();',
    '    this.ProcA();',
    '    this.ProcB();',
    '    this.ProcB();',
    '  };',
    '});',
    'this.A = null;',
    'this.B = null;',
    'this.C = null;',
    '']),
    LinesToStr([ // this.$main
    'this.A.ProcA();',
    'this.B.ProcA();',
    'this.C.ProcA();',
    '']));
end;

procedure TTestModule.TestExternalClass_Property;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    function getYear: longint;');
  Add('    procedure setYear(Value: longint);');
  Add('    property Year: longint read getyear write setyear;');
  Add('  end;');
  Add('  TExtB = class (TExtA)');
  Add('    procedure OtherSetYear(Value: longint);');
  Add('    property year write othersetyear;');
  Add('  end;');
  Add('procedure textb.othersetyear(value: longint);');
  Add('begin');
  Add('  setYear(Value+4);');
  Add('end;');
  Add('var');
  Add('  A: texta;');
  Add('  B: textb;');
  Add('begin');
  Add('  a.year:=a.year+1;');
  Add('  b.year:=b.year+2;');
  ConvertProgram;
  CheckSource('TestExternalClass_NonExternalOverride',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TExtB", ExtA, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.OtherSetYear = function (Value) {',
    '    this.setYear(Value+4);',
    '  };',
    '});',
    'this.A = null;',
    'this.B = null;',
    '']),
    LinesToStr([ // this.$main
    'this.A.setYear(this.A.getYear()+1);',
    'this.B.OtherSetYear(this.B.getYear()+2);',
    '']));
end;

procedure TTestModule.TestExternalClass_ClassProperty;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    class function getYear: longint;');
  Add('    class procedure setYear(Value: longint);');
  Add('    class property Year: longint read getyear write setyear;');
  Add('  end;');
  Add('  TExtB = class (TExtA)');
  Add('    class function GetCentury: longint;');
  Add('    class procedure SetCentury(Value: longint);');
  Add('    class property Century: longint read getcentury write setcentury;');
  Add('  end;');
  Add('class function textb.getcentury: longint;');
  Add('begin');
  Add('end;');
  Add('class procedure textb.setcentury(value: longint);');
  Add('begin');
  Add('  setyear(value+11);');
  Add('  texta.year:=texta.year+12;');
  Add('  year:=year+13;');
  Add('  textb.century:=textb.century+14;');
  Add('  century:=century+15;');
  Add('end;');
  Add('var');
  Add('  A: texta;');
  Add('  B: textb;');
  Add('begin');
  Add('  texta.year:=texta.year+1;');
  Add('  textb.year:=textb.year+2;');
  Add('  TextA.year:=TextA.year+3;');
  Add('  b.year:=b.year+4;');
  Add('  textb.century:=textb.century+5;');
  Add('  b.century:=b.century+6;');
  ConvertProgram;
  CheckSource('TestExternalClass_ClassProperty',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TExtB", ExtA, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetCentury = function () {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.SetCentury = function (Value) {',
    '    this.setYear(Value + 11);',
    '    ExtA.setYear(ExtA.getYear() + 12);',
    '    this.setYear(this.getYear() + 13);',
    '    pas.program.TExtB.SetCentury(pas.program.TExtB.GetCentury() + 14);',
    '    this.SetCentury(this.GetCentury() + 15);',
    '  };',
    '});',
    'this.A = null;',
    'this.B = null;',
    '']),
    LinesToStr([ // this.$main
    'ExtA.setYear(ExtA.getYear() + 1);',
    'this.TExtB.setYear(this.TExtB.getYear() + 2);',
    'ExtA.setYear(ExtA.getYear() + 3);',
    'this.B.setYear(this.B.getYear() + 4);',
    'this.TExtB.SetCentury(this.TExtB.GetCentury() + 5);',
    'this.B.$class.SetCentury(this.B.$class.GetCentury() + 6);',
    '']));
end;

procedure TTestModule.TestExternalClass_ClassOf;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    procedure ProcA; virtual;');
  Add('    procedure ProcB; virtual;');
  Add('  end;');
  Add('  TExtAClass = class of TExtA;');
  Add('  TExtB = class external name ''ExtB'' (TExtA)');
  Add('  end;');
  Add('  TExtBClass = class of TExtB;');
  Add('  TExtC = class (TExtB)');
  Add('    procedure ProcA; override;');
  Add('  end;');
  Add('  TExtCClass = class of TExtC;');
  Add('procedure TExtC.ProcA; begin end;');
  Add('var');
  Add('  A: texta; ClA: TExtAClass;');
  Add('  B: textb; ClB: TExtBClass;');
  Add('  C: textc; ClC: TExtCClass;');
  Add('begin');
  Add('  ClA:=texta;');
  Add('  ClA:=textb;');
  Add('  ClA:=textc;');
  Add('  ClB:=textb;');
  Add('  ClB:=textc;');
  Add('  ClC:=textc;');
  ConvertProgram;
  CheckSource('TestExternalClass_ClassOf',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TExtC", ExtB, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.ProcA = function () {',
    '  };',
    '});',
    'this.A = null;',
    'this.ClA = null;',
    'this.B = null;',
    'this.ClB = null;',
    'this.C = null;',
    'this.ClC = null;',
    '']),
    LinesToStr([ // this.$main
    'this.ClA = ExtA;',
    'this.ClA = ExtB;',
    'this.ClA = this.TExtC;',
    'this.ClB = ExtB;',
    'this.ClB = this.TExtC;',
    'this.ClC = this.TExtC;',
    '']));
end;

procedure TTestModule.TestExternalClass_ClassOtherUnit;
begin
  AddModuleWithIntfImplSrc('unit2.pas',
    LinesToStr([
    '{$modeswitch externalclass}',
    'type',
    '  TExtA = class external name ''ExtA''',
    '    class var Id: longint;',
    '  end;',
    '']),
    '');

  StartUnit(true);
  Add('interface');
  Add('uses unit2;');
  Add('implementation');
  Add('begin');
  Add('  unit2.texta.id:=unit2.texta.id+1;');
  ConvertUnit;
  CheckSource('TestExternalClass_ClassOtherUnit',
    LinesToStr([
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '']),
    LinesToStr([
    'ExtA.Id = ExtA.Id + 1;',
    '']));
end;

procedure TTestModule.TestExternalClass_Is;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('  end;');
  Add('  TExtAClass = class of TExtA;');
  Add('  TExtB = class external name ''ExtB'' (TExtA)');
  Add('  end;');
  Add('  TExtBClass = class of TExtB;');
  Add('  TExtC = class (TExtB)');
  Add('  end;');
  Add('  TExtCClass = class of TExtC;');
  Add('var');
  Add('  A: texta; ClA: TExtAClass;');
  Add('  B: textb; ClB: TExtBClass;');
  Add('  C: textc; ClC: TExtCClass;');
  Add('begin');
  Add('  if a is textb then ;');
  Add('  if a is textc then ;');
  Add('  if b is textc then ;');
  Add('  if cla is textb then ;');
  Add('  if cla is textc then ;');
  Add('  if clb is textc then ;');
  ConvertProgram;
  CheckSource('TestExternalClass_Is',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TExtC", ExtB, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.A = null;',
    'this.ClA = null;',
    'this.B = null;',
    'this.ClB = null;',
    'this.C = null;',
    'this.ClC = null;',
    '']),
    LinesToStr([ // this.$main
    'if (rtl.isExt(this.A, ExtB)) ;',
    'if (this.TExtC.isPrototypeOf(this.A)) ;',
    'if (this.TExtC.isPrototypeOf(this.B)) ;',
    'if (rtl.isExt(this.ClA, ExtB)) ;',
    'if (rtl.is(this.ClA, this.TExtC)) ;',
    'if (rtl.is(this.ClB, this.TExtC)) ;',
    '']));
end;

procedure TTestModule.TestExternalClass_As;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('  end;');
  Add('  TExtB = class external name ''ExtB'' (TExtA)');
  Add('  end;');
  Add('  TExtC = class (TExtB)');
  Add('  end;');
  Add('var');
  Add('  A: texta;');
  Add('  B: textb;');
  Add('  C: textc;');
  Add('begin');
  Add('  b:=a as textb;');
  Add('  c:=a as textc;');
  Add('  c:=b as textc;');
  ConvertProgram;
  CheckSource('TestExternalClass_Is',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TExtC", ExtB, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.A = null;',
    'this.B = null;',
    'this.C = null;',
    '']),
    LinesToStr([ // this.$main
    'this.B = rtl.asExt(this.A, ExtB);',
    'this.C = rtl.as(this.A, this.TExtC);',
    'this.C = rtl.as(this.B, this.TExtC);',
    '']));
end;

procedure TTestModule.TestExternalClass_DestructorFail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    destructor Free;');
  Add('  end;');
  SetExpectedPasResolverError('Pascal element not supported: destructor',
    nPasElementNotSupported);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_New;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    constructor New;');
  Add('    constructor New(i: longint; j: longint = 2);');
  Add('  end;');
  Add('var');
  Add('  A: texta;');
  Add('begin');
  Add('  a:=texta.new;');
  Add('  a:=texta.new();');
  Add('  a:=texta.new(1);');
  Add('  with texta do begin');
  Add('    a:=new;');
  Add('    a:=new();');
  Add('    a:=new(2);');
  Add('  end;');
  Add('  a:=test1.texta.new;');
  Add('  a:=test1.texta.new();');
  Add('  a:=test1.texta.new(3);');
  ConvertProgram;
  CheckSource('TestExternalClass_ObjectCreate',
    LinesToStr([ // statements
    'this.A = null;',
    '']),
    LinesToStr([ // this.$main
    'this.A = new ExtA();',
    'this.A = new ExtA();',
    'this.A = new ExtA(1,2);',
    'var $with1 = ExtA;',
    'this.A = new $with1();',
    'this.A = new $with1();',
    'this.A = new $with1(2,2);',
    'this.A = new ExtA();',
    'this.A = new ExtA();',
    'this.A = new ExtA(3,2);',
    '']));
end;

procedure TTestModule.TestExternalClass_ClassOf_New;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtAClass = class of TExtA;');
  Add('  TExtA = class external name ''ExtA''');
  Add('    constructor New;');
  Add('  end;');
  Add('var');
  Add('  A: texta;');
  Add('  C: textaclass;');
  Add('begin');
  Add('  a:=c.new;');
  Add('  a:=c.new();');
  Add('  with C do begin');
  Add('    a:=new;');
  Add('    a:=new();');
  Add('  end;');
  Add('  a:=test1.c.new;');
  Add('  a:=test1.c.new();');
  ConvertProgram;
  CheckSource('TestExternalClass_ClassOf_New',
    LinesToStr([ // statements
    'this.A = null;',
    'this.C = null;',
    '']),
    LinesToStr([ // this.$main
    'this.A = new this.C();',
    'this.A = new this.C();',
    'var $with1 = this.C;',
    'this.A = new $with1();',
    'this.A = new $with1();',
    'this.A = new this.C();',
    'this.A = new this.C();',
    '']));
end;

procedure TTestModule.TestExternalClass_FuncClassOf_New;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtAClass = class of TExtA;');
  Add('  TExtA = class external name ''ExtA''');
  Add('    constructor New;');
  Add('  end;');
  Add('function GetCreator: TExtAClass;');
  Add('begin');
  Add('  Result:=TExtA;');
  Add('end;');
  Add('var');
  Add('  A: texta;');
  Add('begin');
  Add('  a:=getcreator.new;');
  Add('  a:=getcreator().new;');
  Add('  a:=getcreator().new();');
  Add('  a:=getcreator.new();');
  Add('  with getcreator do begin');
  Add('    a:=new;');
  Add('    a:=new();');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestExternalClass_FuncClassOf_New',
    LinesToStr([ // statements
    'this.GetCreator = function () {',
    '  var Result = null;',
    '  Result = ExtA;',
    '  return Result;',
    '};',
    'this.A = null;',
    '']),
    LinesToStr([ // this.$main
    'this.A = new (this.GetCreator())();',
    'this.A = new (this.GetCreator())();',
    'this.A = new (this.GetCreator())();',
    'this.A = new (this.GetCreator())();',
    'var $with1 = this.GetCreator();',
    'this.A = new $with1();',
    'this.A = new $with1();',
    '']));
end;

procedure TTestModule.TestExternalClass_LocalConstSameName;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    constructor New;');
  Add('  end;');
  Add('function DoIt: longint;');
  Add('const ExtA = 3;');
  Add('begin');
  Add('  Result:=ExtA;');
  Add('end;');
  Add('var');
  Add('  A: texta;');
  Add('begin');
  Add('  a:=texta.new;');
  ConvertProgram;
  CheckSource('TestExternalClass_LocalConstSameName',
    LinesToStr([ // statements
    'var ExtA$1 = 3;',
    'this.DoIt = function () {',
    '  var Result = 0;',
    '  Result = ExtA$1;',
    '  return Result;',
    '};',
    'this.A = null;',
    '']),
    LinesToStr([ // this.$main
    'this.A = new ExtA();',
    '']));
end;

procedure TTestModule.TestExternalClass_ReintroduceOverload;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    procedure DoIt;');
  Add('  end;');
  Add('  TMyA = class(TExtA)');
  Add('    procedure DoIt;');
  Add('  end;');
  Add('procedure TMyA.DoIt; begin end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestExternalClass_ReintroduceOverload',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TMyA", ExtA, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt$1 = function () {',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestExternalClass_Inherited;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('    procedure DoIt(i: longint = 1); virtual;');
  Add('    procedure DoSome(j: longint = 2);');
  Add('  end;');
  Add('  TExtB = class external name ''ExtB''(TExtA)');
  Add('  end;');
  Add('  TMyC = class(TExtB)');
  Add('    procedure DoIt(i: longint = 1); override;');
  Add('    procedure DoSome(j: longint = 2); reintroduce;');
  Add('  end;');
  Add('procedure TMyC.DoIt(i: longint);');
  Add('begin');
  Add('  inherited;');
  Add('  inherited DoIt;');
  Add('  inherited DoIt();');
  Add('  inherited DoIt(3);');
  Add('  inherited DoSome;');
  Add('  inherited DoSome();');
  Add('  inherited DoSome(4);');
  Add('end;');
  Add('procedure TMyC.DoSome(j: longint);');
  Add('begin');
  Add('  inherited;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestExternalClass_ReintroduceOverload',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TMyC", ExtB, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (i) {',
    '    ExtB.DoIt.apply(this, arguments);',
    '    ExtB.DoIt.call(this, 1);',
    '    ExtB.DoIt.call(this, 1);',
    '    ExtB.DoIt.call(this, 3);',
    '    ExtB.DoSome.call(this, 2);',
    '    ExtB.DoSome.call(this, 2);',
    '    ExtB.DoSome.call(this, 4);',
    '  };',
    '  this.DoSome$1 = function (j) {',
    '    ExtB.DoSome.apply(this, arguments);',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestExternalClass_PascalAncestorFail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TExtA = class external name ''ExtA''(TObject)');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError('Ancestor "TObject" is not external',nAncestorIsNotExternal);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_NewInstance;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('  end;');
  Add('  TMyB = class(TExtA)');
  Add('  protected');
  Add('    class function NewInstance(fnname: string; const paramarray): TMyB; virtual;');
  Add('  end;');
  Add('class function TMyB.NewInstance(fnname: string; const paramarray): TMyB;');
  Add('begin end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestExternalClass_NewInstance',
    LinesToStr([ // statements
    'rtl.createClassExt(this, "TMyB", ExtA, "NewInstance", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.NewInstance = function (fnname, paramarray) {',
    '    var Result = null;',
    '    return Result;',
    '  };',
    '});',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestExternalClass_NewInstance_NonVirtualFail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('  end;');
  Add('  TMyB = class(TExtA)');
  Add('  protected');
  Add('    class function NewInstance(fnname: string; const paramarray): TMyB;');
  Add('  end;');
  Add('class function TMyB.NewInstance(fnname: string; const paramarray): TMyB;');
  Add('begin end;');
  Add('begin');
  SetExpectedPasResolverError(sNewInstanceFunctionMustBeVirtual,nNewInstanceFunctionMustBeVirtual);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_NewInstance_FirstParamNotString_Fail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('  end;');
  Add('  TMyB = class(TExtA)');
  Add('  protected');
  Add('    class function NewInstance(fnname: longint; const paramarray): TMyB; virtual;');
  Add('  end;');
  Add('class function TMyB.NewInstance(fnname: longint; const paramarray): TMyB;');
  Add('begin end;');
  Add('begin');
  SetExpectedPasResolverError('Incompatible type arg no. 1: Got "Longint", expected "String"',
    nIncompatibleTypeArgNo);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_NewInstance_SecondParamTyped_Fail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TExtA = class external name ''ExtA''');
  Add('  end;');
  Add('  TMyB = class(TExtA)');
  Add('  protected');
  Add('    class function NewInstance(fnname: string; const paramarray: string): TMyB; virtual;');
  Add('  end;');
  Add('class function TMyB.NewInstance(fnname: string; const paramarray: string): TMyB;');
  Add('begin end;');
  Add('begin');
  SetExpectedPasResolverError('Incompatible type arg no. 2: Got "type", expected "untyped"',
    nIncompatibleTypeArgNo);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_TypeCastToRootClass;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TChild = class');
  Add('  end;');
  Add('  TExtRootA = class external name ''ExtRootA''');
  Add('  end;');
  Add('  TExtChildA = class external name ''ExtChildA''(TExtRootA)');
  Add('  end;');
  Add('  TExtRootB = class external name ''ExtRootB''');
  Add('  end;');
  Add('  TExtChildB = class external name ''ExtChildB''(TExtRootB)');
  Add('  end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  Child: TChild;');
  Add('  RootA: TExtRootA;');
  Add('  ChildA: TExtChildA;');
  Add('  RootB: TExtRootB;');
  Add('  ChildB: TExtChildB;');
  Add('begin');
  Add('  obj:=tobject(roota);');
  Add('  obj:=tobject(childa);');
  Add('  child:=tchild(tobject(roota));');
  Add('  roota:=textroota(obj);');
  Add('  roota:=textroota(child);');
  Add('  roota:=textroota(rootb);');
  Add('  roota:=textroota(childb);');
  Add('  childa:=textchilda(textroota(obj));');
  ConvertProgram;
  CheckSource('TestExternalClass_TypeCastToRootClass',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TChild", this.TObject, function () {',
    '});',
    'this.Obj = null;',
    'this.Child = null;',
    'this.RootA = null;',
    'this.ChildA = null;',
    'this.RootB = null;',
    'this.ChildB = null;',
    '']),
    LinesToStr([ // this.$main
    'this.Obj = this.RootA;',
    'this.Obj = this.ChildA;',
    'this.Child = this.RootA;',
    'this.RootA = this.Obj;',
    'this.RootA = this.Child;',
    'this.RootA = this.RootB;',
    'this.RootA = this.ChildB;',
    'this.ChildA = this.Obj;',
    '']));
end;

procedure TTestModule.TestExternalClass_TypeCastStringToExternalString;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSString = class external name ''String''');
  Add('    class function fromCharCode() : string; varargs;');
  Add('    function anchor(const aName : string) : string;');
  Add('  end;');
  Add('var');
  Add('  s: string;');
  Add('begin');
  Add('  s:=TJSString.fromCharCode(65,66);');
  Add('  s:=TJSString(s).anchor(s);');
  Add('  s:=TJSString(''foo'').anchor(s);');
  ConvertProgram;
  CheckSource('TestExternalClass_TypeCastStringToExternalString',
    LinesToStr([ // statements
    'this.s = "";',
    '']),
    LinesToStr([ // this.$main
    'this.s = String.fromCharCode(65, 66);',
    'this.s = this.s.anchor(this.s);',
    'this.s = "foo".anchor(this.s);',
    '']));
end;

procedure TTestModule.TestExternalClass_CallClassFunctionOfInstanceFail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSString = class external name ''String''');
  Add('    class function fromCharCode() : string; varargs;');
  Add('  end;');
  Add('var');
  Add('  s: string;');
  Add('  sObj: TJSString;');
  Add('begin');
  Add('  s:=sObj.fromCharCode(65,66);');
  SetExpectedPasResolverError('External class instance cannot access static class function fromCharCode',
    nExternalClassInstanceCannotAccessStaticX);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_BracketOperator;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array''');
  Add('  end;');
  Add('  TJSObject = class external name ''Object''');
  Add('  end;');
  Add('procedure DoIt(vI: JSValue; const vJ: jsvalue; var vK: jsvalue; out vL: jsvalue);');
  Add('begin end;');
  Add('var');
  Add('  Obj: tjsobject;');
  Add('  Arr: tjsarray;');
  Add('  s: string;');
  Add('  i: longint;');
  Add('  v: jsvalue;');
  Add('begin');
  Add('  arr[1]:=s;');
  Add('  arr[2]:=i;');
  Add('  arr[3]:=arr[4];');
  Add('  v:=arr[5];');
  Add('  v:=obj[''one''];');
  Add('  obj[''two'']:=i;');
  Add('  obj[''three'']:=v;');
  Add('  doit(arr[6],arr[7],arr[8],arr[9]);');
  Add('  doit(obj[''10''],obj[''11''],obj[''12''],obj[''13'']);');
  ConvertProgram;
  CheckSource('TestExternalClass_BracketOperator',
    LinesToStr([ // statements
    'this.DoIt = function (vI, vJ, vK, vL) {',
    '};',
    'this.Obj = null;',
    'this.Arr = null;',
    'this.s = "";',
    'this.i = 0;',
    'this.v = undefined;',
    '']),
    LinesToStr([ // this.$main
    'this.Arr[1] = this.s;',
    'this.Arr[2] = this.i;',
    'this.Arr[3] = this.Arr[4];',
    'this.v = this.Arr[5];',
    'this.v = this.Obj["one"];',
    'this.Obj["two"] = this.i;',
    'this.Obj["three"] = this.v;',
    'this.DoIt(this.Arr[6], this.Arr[7], {',
    '  a: 8,',
    '  p: this.Arr,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '}, {',
    '  a: 9,',
    '  p: this.Arr,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    ' this.DoIt(this.Obj["10"], this.Obj["11"], {',
    '  a: "12",',
    '  p: this.Obj,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '}, {',
    '  a: "13",',
    '  p: this.Obj,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestProcType;
begin
  StartProgram(false);
  Add('type');
  Add('  TProcInt = procedure(vI: longint = 1);');
  Add('procedure DoIt(vJ: longint);');
  Add('begin end;');
  Add('var');
  Add('  b: boolean;');
  Add('  vP, vQ: tprocint;');
  Add('begin');
  Add('  vp:=nil;');
  Add('  vp:=vp;');
  Add('  vp:=@doit;');
  Add('  vp;');
  Add('  vp();');
  Add('  vp(2);');
  Add('  b:=vp=nil;');
  Add('  b:=nil=vp;');
  Add('  b:=vp=vq;');
  Add('  b:=vp=@doit;');
  Add('  b:=@doit=vp;');
  Add('  b:=vp<>nil;');
  Add('  b:=nil<>vp;');
  Add('  b:=vp<>vq;');
  Add('  b:=vp<>@doit;');
  Add('  b:=@doit<>vp;');
  Add('  b:=Assigned(vp);');
  ConvertProgram;
  CheckSource('TestProcType',
    LinesToStr([ // statements
    'this.DoIt = function(vJ) {',
    '};',
    'this.b = false;',
    'this.vP = null;',
    'this.vQ = null;'
    ]),
    LinesToStr([ // this.$main
    'this.vP = null;',
    'this.vP = this.vP;',
    'this.vP = rtl.createCallback(this,"DoIt");',
    'this.vP(1);',
    'this.vP(1);',
    'this.vP(2);',
    'this.b = this.vP == null;',
    'this.b = null == this.vP;',
    'this.b = rtl.eqCallback(this.vP,this.vQ);',
    'this.b = rtl.eqCallback(this.vP, rtl.createCallback(this, "DoIt"));',
    'this.b = rtl.eqCallback(rtl.createCallback(this, "DoIt"), this.vP);',
    'this.b = this.vP != null;',
    'this.b = null != this.vP;',
    'this.b = !rtl.eqCallback(this.vP,this.vQ);',
    'this.b = !rtl.eqCallback(this.vP, rtl.createCallback(this, "DoIt"));',
    'this.b = !rtl.eqCallback(rtl.createCallback(this, "DoIt"), this.vP);',
    'this.b = this.vP != null;',
    '']));
end;

procedure TTestModule.TestProcType_FunctionFPC;
begin
  StartProgram(false);
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint;');
  Add('function DoIt(vI: longint): longint;');
  Add('begin end;');
  Add('var');
  Add('  b: boolean;');
  Add('  vP, vQ: tfuncint;');
  Add('begin');
  Add('  vp:=nil;');
  Add('  vp:=vp;');
  Add('  vp:=@doit;'); // ok in fpc and delphi
  //Add('  vp:=doit;'); // illegal in fpc, ok in delphi
  Add('  vp;'); // ok in fpc and delphi
  Add('  vp();');
  Add('  vp(2);');
  Add('  b:=vp=nil;'); // ok in fpc, illegal in delphi
  Add('  b:=nil=vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp=vq;'); // in fpc compare proctypes, in delphi compare results
  Add('  b:=vp=@doit;'); // ok in fpc, illegal in delphi
  Add('  b:=@doit=vp;'); // ok in fpc, illegal in delphi
  //Add('  b:=vp=3;'); // illegal in fpc, ok in delphi
  Add('  b:=4=vp;'); // illegal in fpc, ok in delphi
  Add('  b:=vp<>nil;'); // ok in fpc, illegal in delphi
  Add('  b:=nil<>vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp<>vq;'); // in fpc compare proctypes, in delphi compare results
  Add('  b:=vp<>@doit;'); // ok in fpc, illegal in delphi
  Add('  b:=@doit<>vp;'); // ok in fpc, illegal in delphi
  //Add('  b:=vp<>5;'); // illegal in fpc, ok in delphi
  Add('  b:=6<>vp;'); // illegal in fpc, ok in delphi
  Add('  b:=Assigned(vp);');
  //Add('  doit(vp);'); // illegal in fpc, ok in delphi
  Add('  doit(vp());'); // ok in fpc and delphi
  Add('  doit(vp(2));'); // ok in fpc and delphi
  ConvertProgram;
  CheckSource('TestProcType_FunctionFPC',
    LinesToStr([ // statements
    'this.DoIt = function(vI) {',
    '  var Result = 0;',
    '  return Result;',
    '};',
    'this.b = false;',
    'this.vP = null;',
    'this.vQ = null;'
    ]),
    LinesToStr([ // this.$main
    'this.vP = null;',
    'this.vP = this.vP;',
    'this.vP = rtl.createCallback(this,"DoIt");',
    'this.vP(1);',
    'this.vP(1);',
    'this.vP(2);',
    'this.b = this.vP == null;',
    'this.b = null == this.vP;',
    'this.b = rtl.eqCallback(this.vP,this.vQ);',
    'this.b = rtl.eqCallback(this.vP, rtl.createCallback(this, "DoIt"));',
    'this.b = rtl.eqCallback(rtl.createCallback(this, "DoIt"), this.vP);',
    'this.b = 4 == this.vP(1);',
    'this.b = this.vP != null;',
    'this.b = null != this.vP;',
    'this.b = !rtl.eqCallback(this.vP,this.vQ);',
    'this.b = !rtl.eqCallback(this.vP, rtl.createCallback(this, "DoIt"));',
    'this.b = !rtl.eqCallback(rtl.createCallback(this, "DoIt"), this.vP);',
    'this.b = 6 != this.vP(1);',
    'this.b = this.vP != null;',
    'this.DoIt(this.vP(1));',
    'this.DoIt(this.vP(2));',
    '']));
end;

procedure TTestModule.TestProcType_FunctionDelphi;
begin
  StartProgram(false);
  Add('{$mode Delphi}');
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint;');
  Add('function DoIt(vI: longint): longint;');
  Add('begin end;');
  Add('var');
  Add('  b: boolean;');
  Add('  vP, vQ: tfuncint;');
  Add('begin');
  Add('  vp:=nil;');
  Add('  vp:=vp;');
  Add('  vp:=@doit;'); // ok in fpc and delphi
  Add('  vp:=doit;'); // illegal in fpc, ok in delphi
  Add('  vp;'); // ok in fpc and delphi
  Add('  vp();');
  Add('  vp(2);');
  //Add('  b:=vp=nil;'); // ok in fpc, illegal in delphi
  //Add('  b:=nil=vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp=vq;'); // in fpc compare proctypes, in delphi compare results
  //Add('  b:=vp=@doit;'); // ok in fpc, illegal in delphi
  //Add('  b:=@doit=vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp=3;'); // illegal in fpc, ok in delphi
  Add('  b:=4=vp;'); // illegal in fpc, ok in delphi
  //Add('  b:=vp<>nil;'); // ok in fpc, illegal in delphi
  //Add('  b:=nil<>vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp<>vq;'); // in fpc compare proctypes, in delphi compare results
  //Add('  b:=vp<>@doit;'); // ok in fpc, illegal in delphi
  //Add('  b:=@doit<>vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp<>5;'); // illegal in fpc, ok in delphi
  Add('  b:=6<>vp;'); // illegal in fpc, ok in delphi
  Add('  b:=Assigned(vp);');
  Add('  doit(vp);'); // illegal in fpc, ok in delphi
  Add('  doit(vp());'); // ok in fpc and delphi
  Add('  doit(vp(2));'); // ok in fpc and delphi  *)
  ConvertProgram;
  CheckSource('TestProcType_FunctionDelphi',
    LinesToStr([ // statements
    'this.DoIt = function(vI) {',
    '  var Result = 0;',
    '  return Result;',
    '};',
    'this.b = false;',
    'this.vP = null;',
    'this.vQ = null;'
    ]),
    LinesToStr([ // this.$main
    'this.vP = null;',
    'this.vP = this.vP;',
    'this.vP = rtl.createCallback(this,"DoIt");',
    'this.vP = rtl.createCallback(this,"DoIt");',
    'this.vP(1);',
    'this.vP(1);',
    'this.vP(2);',
    'this.b = this.vP(1) == this.vQ(1);',
    'this.b = this.vP(1) == 3;',
    'this.b = 4 == this.vP(1);',
    'this.b = this.vP(1) != this.vQ(1);',
    'this.b = this.vP(1) != 5;',
    'this.b = 6 != this.vP(1);',
    'this.b = this.vP != null;',
    'this.DoIt(this.vP(1));',
    'this.DoIt(this.vP(1));',
    'this.DoIt(this.vP(2));',
    '']));
end;

procedure TTestModule.TestProcType_AsParam;
begin
  StartProgram(false);
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint;');
  Add('procedure DoIt(vG: tfuncint; const vH: tfuncint; var vI: tfuncint);');
  Add('var vJ: tfuncint;');
  Add('begin');
  Add('  vg:=vg;');
  Add('  vj:=vh;');
  Add('  vi:=vi;');
  Add('  doit(vg,vg,vg);');
  Add('  doit(vh,vh,vj);');
  Add('  doit(vi,vi,vi);');
  Add('  doit(vj,vj,vj);');
  Add('end;');
  Add('var i: tfuncint;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestProcType_AsParam',
    LinesToStr([ // statements
    'this.DoIt = function (vG,vH,vI) {',
    '  var vJ = null;',
    '  vG = vG;',
    '  vJ = vH;',
    '  vI.set(vI.get());',
    '  this.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  this.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  this.DoIt(vI.get(), vI.get(), vI);',
    '  this.DoIt(vJ, vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = null;'
    ]),
    LinesToStr([
    'this.DoIt(this.i,this.i,{',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestProcType_MethodFPC;
begin
  StartProgram(false);
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint of object;');
  Add('  TObject = class');
  Add('    function DoIt(vA: longint = 1): longint;');
  Add('  end;');
  Add('function TObject.DoIt(vA: longint = 1): longint;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  vP: tfuncint;');
  Add('  b: boolean;');
  Add('begin');
  Add('  vp:=@obj.doit;'); // ok in fpc and delphi
  //Add('  vp:=obj.doit;'); // illegal in fpc, ok in delphi
  Add('  vp;'); // ok in fpc and delphi
  Add('  vp();');
  Add('  vp(2);');
  Add('  b:=vp=@obj.doit;'); // ok in fpc, illegal in delphi
  Add('  b:=@obj.doit=vp;'); // ok in fpc, illegal in delphi
  Add('  b:=vp<>@obj.doit;'); // ok in fpc, illegal in delphi
  Add('  b:=@obj.doit<>vp;'); // ok in fpc, illegal in delphi
  ConvertProgram;
  CheckSource('TestProcType_MethodFPC',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (vA) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.vP = null;',
    'this.b = false;'
    ]),
    LinesToStr([
    'this.vP = rtl.createCallback(this.Obj, "DoIt");',
    'this.vP(1);',
    'this.vP(1);',
    'this.vP(2);',
    'this.b = rtl.eqCallback(this.vP, rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = rtl.eqCallback(rtl.createCallback(this.Obj, "DoIt"), this.vP);',
    'this.b = !rtl.eqCallback(this.vP, rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = !rtl.eqCallback(rtl.createCallback(this.Obj, "DoIt"), this.vP);',
    '']));
end;

procedure TTestModule.TestProcType_MethodDelphi;
begin
  StartProgram(false);
  Add('{$mode delphi}');
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint of object;');
  Add('  TObject = class');
  Add('    function DoIt(vA: longint = 1): longint;');
  Add('  end;');
  Add('function TObject.DoIt(vA: longint = 1): longint;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  vP: tfuncint;');
  Add('  b: boolean;');
  Add('begin');
  Add('  vp:=@obj.doit;'); // ok in fpc and delphi
  Add('  vp:=obj.doit;'); // illegal in fpc, ok in delphi
  Add('  vp;'); // ok in fpc and delphi
  Add('  vp();');
  Add('  vp(2);');
  //Add('  b:=vp=@obj.doit;'); // ok in fpc, illegal in delphi
  //Add('  b:=@obj.doit=vp;'); // ok in fpc, illegal in delphi
  //Add('  b:=vp<>@obj.doit;'); // ok in fpc, illegal in delphi
  //Add('  b:=@obj.doit<>vp;'); // ok in fpc, illegal in delphi
  ConvertProgram;
  CheckSource('TestProcType_MethodDelphi',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (vA) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.vP = null;',
    'this.b = false;'
    ]),
    LinesToStr([
    'this.vP = rtl.createCallback(this.Obj, "DoIt");',
    'this.vP = rtl.createCallback(this.Obj, "DoIt");',
    'this.vP(1);',
    'this.vP(1);',
    'this.vP(2);',
    '']));
end;

procedure TTestModule.TestProcType_PropertyFPC;
begin
  StartProgram(false);
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint of object;');
  Add('  TObject = class');
  Add('    FOnFoo: TFuncInt;');
  Add('    function DoIt(vA: longint = 1): longint;');
  Add('    function GetFoo: TFuncInt;');
  Add('    procedure SetFoo(const Value: TFuncInt);');
  Add('    function GetEvents(Index: longint): TFuncInt;');
  Add('    procedure SetEvents(Index: longint; const Value: TFuncInt);');
  Add('    property OnFoo: TFuncInt read FOnFoo write FOnFoo;');
  Add('    property OnBar: TFuncInt read GetFoo write SetFoo;');
  Add('    property Events[Index: longint]: TFuncInt read GetEvents write SetEvents; default;');
  Add('  end;');
  Add('function tobject.doit(va: longint = 1): longint; begin end;');
  Add('function tobject.getfoo: tfuncint; begin end;');
  Add('procedure tobject.setfoo(const value: tfuncint); begin end;');
  Add('function tobject.getevents(index: longint): tfuncint; begin end;');
  Add('procedure tobject.setevents(index: longint; const value: tfuncint); begin end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  vP: tfuncint;');
  Add('  b: boolean;');
  Add('begin');
  Add('  obj.onfoo:=nil;');
  Add('  obj.onbar:=nil;');
  Add('  obj.events[1]:=nil;');
  Add('  obj.onfoo:=obj.onfoo;');
  Add('  obj.onbar:=obj.onbar;');
  Add('  obj.events[2]:=obj.events[3];');
  Add('  obj.onfoo:=@obj.doit;');
  Add('  obj.onbar:=@obj.doit;');
  Add('  obj.events[4]:=@obj.doit;');
  //Add('  obj.onfoo:=obj.doit;'); // delphi
  //Add('  obj.onbar:=obj.doit;'); // delphi
  //Add('  obj.events[4]:=obj.doit;'); // delphi
  Add('  obj.onfoo;');
  Add('  obj.onbar;');
  //Add('  obj.events[5];'); ToDo in pasresolver
  Add('  obj.onfoo();');
  Add('  obj.onbar();');
  Add('  obj.events[6]();');
  Add('  b:=obj.onfoo=nil;');
  Add('  b:=obj.onbar=nil;');
  Add('  b:=obj.events[7]=nil;');
  Add('  b:=obj.onfoo<>nil;');
  Add('  b:=obj.onbar<>nil;');
  Add('  b:=obj.events[8]<>nil;');
  Add('  b:=obj.onfoo=vp;');
  Add('  b:=obj.onbar=vp;');
  Add('  b:=obj.events[9]=vp;');
  Add('  b:=obj.onfoo=obj.onfoo;');
  Add('  b:=obj.onbar=obj.onfoo;');
  Add('  b:=obj.events[10]=obj.onfoo;');
  Add('  b:=obj.onfoo<>obj.onfoo;');
  Add('  b:=obj.onbar<>obj.onfoo;');
  Add('  b:=obj.events[11]<>obj.onfoo;');
  Add('  b:=obj.onfoo=@obj.doit;');
  Add('  b:=obj.onbar=@obj.doit;');
  Add('  b:=obj.events[12]=@obj.doit;');
  Add('  b:=obj.onfoo<>@obj.doit;');
  Add('  b:=obj.onbar<>@obj.doit;');
  Add('  b:=obj.events[12]<>@obj.doit;');
  Add('  b:=Assigned(obj.onfoo);');
  Add('  b:=Assigned(obj.onbar);');
  Add('  b:=Assigned(obj.events[13]);');
  ConvertProgram;
  CheckSource('TestProcType_PropertyFPC',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FOnFoo = null;',
    '  };',
    '  this.$final = function () {',
    '    this.FOnFoo = undefined;',
    '  };',
    '  this.DoIt = function (vA) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    'this.GetFoo = function () {',
    '  var Result = null;',
    '  return Result;',
    '};',
    'this.SetFoo = function (Value) {',
    '};',
    'this.GetEvents = function (Index) {',
    '  var Result = null;',
    '  return Result;',
    '};',
    'this.SetEvents = function (Index, Value) {',
    '};',
    '});',
    'this.Obj = null;',
    'this.vP = null;',
    'this.b = false;'
    ]),
    LinesToStr([
    'this.Obj.FOnFoo = null;',
    'this.Obj.SetFoo(null);',
    'this.Obj.SetEvents(1, null);',
    'this.Obj.FOnFoo = this.Obj.FOnFoo;',
    'this.Obj.SetFoo(this.Obj.GetFoo());',
    'this.Obj.SetEvents(2, this.Obj.GetEvents(3));',
    'this.Obj.FOnFoo = rtl.createCallback(this.Obj, "DoIt");',
    'this.Obj.SetFoo(rtl.createCallback(this.Obj, "DoIt"));',
    'this.Obj.SetEvents(4, rtl.createCallback(this.Obj, "DoIt"));',
    'this.Obj.FOnFoo(1);',
    'this.Obj.GetFoo();',
    'this.Obj.FOnFoo(1);',
    'this.Obj.GetFoo()(1);',
    'this.Obj.GetEvents(6)(1);',
    'this.b = this.Obj.FOnFoo == null;',
    'this.b = this.Obj.GetFoo() == null;',
    'this.b = this.Obj.GetEvents(7) == null;',
    'this.b = this.Obj.FOnFoo != null;',
    'this.b = this.Obj.GetFoo() != null;',
    'this.b = this.Obj.GetEvents(8) != null;',
    'this.b = rtl.eqCallback(this.Obj.FOnFoo, this.vP);',
    'this.b = rtl.eqCallback(this.Obj.GetFoo(), this.vP);',
    'this.b = rtl.eqCallback(this.Obj.GetEvents(9), this.vP);',
    'this.b = rtl.eqCallback(this.Obj.FOnFoo, this.Obj.FOnFoo);',
    'this.b = rtl.eqCallback(this.Obj.GetFoo(), this.Obj.FOnFoo);',
    'this.b = rtl.eqCallback(this.Obj.GetEvents(10), this.Obj.FOnFoo);',
    'this.b = !rtl.eqCallback(this.Obj.FOnFoo, this.Obj.FOnFoo);',
    'this.b = !rtl.eqCallback(this.Obj.GetFoo(), this.Obj.FOnFoo);',
    'this.b = !rtl.eqCallback(this.Obj.GetEvents(11), this.Obj.FOnFoo);',
    'this.b = rtl.eqCallback(this.Obj.FOnFoo, rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = rtl.eqCallback(this.Obj.GetFoo(), rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = rtl.eqCallback(this.Obj.GetEvents(12), rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = !rtl.eqCallback(this.Obj.FOnFoo, rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = !rtl.eqCallback(this.Obj.GetFoo(), rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = !rtl.eqCallback(this.Obj.GetEvents(12), rtl.createCallback(this.Obj, "DoIt"));',
    'this.b = this.Obj.FOnFoo != null;',
    'this.b = this.Obj.GetFoo() != null;',
    'this.b = this.Obj.GetEvents(13) != null;',
    '']));
end;

procedure TTestModule.TestProcType_PropertyDelphi;
begin
  StartProgram(false);
  Add('{$mode delphi}');
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint of object;');
  Add('  TObject = class');
  Add('    FOnFoo: TFuncInt;');
  Add('    function DoIt(vA: longint = 1): longint;');
  Add('    function GetFoo: TFuncInt;');
  Add('    procedure SetFoo(const Value: TFuncInt);');
  Add('    function GetEvents(Index: longint): TFuncInt;');
  Add('    procedure SetEvents(Index: longint; const Value: TFuncInt);');
  Add('    property OnFoo: TFuncInt read FOnFoo write FOnFoo;');
  Add('    property OnBar: TFuncInt read GetFoo write SetFoo;');
  Add('    property Events[Index: longint]: TFuncInt read GetEvents write SetEvents; default;');
  Add('  end;');
  Add('function tobject.doit(va: longint = 1): longint; begin end;');
  Add('function tobject.getfoo: tfuncint; begin end;');
  Add('procedure tobject.setfoo(const value: tfuncint); begin end;');
  Add('function tobject.getevents(index: longint): tfuncint; begin end;');
  Add('procedure tobject.setevents(index: longint; const value: tfuncint); begin end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  vP: tfuncint;');
  Add('  b: boolean;');
  Add('begin');
  Add('  obj.onfoo:=nil;');
  Add('  obj.onbar:=nil;');
  Add('  obj.events[1]:=nil;');
  Add('  obj.onfoo:=obj.onfoo;');
  Add('  obj.onbar:=obj.onbar;');
  Add('  obj.events[2]:=obj.events[3];');
  Add('  obj.onfoo:=@obj.doit;');
  Add('  obj.onbar:=@obj.doit;');
  Add('  obj.events[4]:=@obj.doit;');
  Add('  obj.onfoo:=obj.doit;'); // delphi
  Add('  obj.onbar:=obj.doit;'); // delphi
  Add('  obj.events[4]:=obj.doit;'); // delphi
  Add('  obj.onfoo;');
  Add('  obj.onbar;');
  //Add('  obj.events[5];'); ToDo in pasresolver
  Add('  obj.onfoo();');
  Add('  obj.onbar();');
  Add('  obj.events[6]();');
  //Add('  b:=obj.onfoo=nil;'); // fpc
  //Add('  b:=obj.onbar=nil;'); // fpc
  //Add('  b:=obj.events[7]=nil;'); // fpc
  //Add('  b:=obj.onfoo<>nil;'); // fpc
  //Add('  b:=obj.onbar<>nil;'); // fpc
  //Add('  b:=obj.events[8]<>nil;'); // fpc
  Add('  b:=obj.onfoo=vp;');
  Add('  b:=obj.onbar=vp;');
  //Add('  b:=obj.events[9]=vp;'); ToDo in pasresolver
  Add('  b:=obj.onfoo=obj.onfoo;');
  Add('  b:=obj.onbar=obj.onfoo;');
  //Add('  b:=obj.events[10]=obj.onfoo;'); // ToDo in pasresolver
  Add('  b:=obj.onfoo<>obj.onfoo;');
  Add('  b:=obj.onbar<>obj.onfoo;');
  //Add('  b:=obj.events[11]<>obj.onfoo;'); // ToDo in pasresolver
  //Add('  b:=obj.onfoo=@obj.doit;'); // fpc
  //Add('  b:=obj.onbar=@obj.doit;'); // fpc
  //Add('  b:=obj.events[12]=@obj.doit;'); // fpc
  //Add('  b:=obj.onfoo<>@obj.doit;'); // fpc
  //Add('  b:=obj.onbar<>@obj.doit;'); // fpc
  //Add('  b:=obj.events[12]<>@obj.doit;'); // fpc
  Add('  b:=Assigned(obj.onfoo);');
  Add('  b:=Assigned(obj.onbar);');
  Add('  b:=Assigned(obj.events[13]);');
  ConvertProgram;
  CheckSource('TestProcType_PropertyDelphi',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FOnFoo = null;',
    '  };',
    '  this.$final = function () {',
    '    this.FOnFoo = undefined;',
    '  };',
    '  this.DoIt = function (vA) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    'this.GetFoo = function () {',
    '  var Result = null;',
    '  return Result;',
    '};',
    'this.SetFoo = function (Value) {',
    '};',
    'this.GetEvents = function (Index) {',
    '  var Result = null;',
    '  return Result;',
    '};',
    'this.SetEvents = function (Index, Value) {',
    '};',
    '});',
    'this.Obj = null;',
    'this.vP = null;',
    'this.b = false;'
    ]),
    LinesToStr([
    'this.Obj.FOnFoo = null;',
    'this.Obj.SetFoo(null);',
    'this.Obj.SetEvents(1, null);',
    'this.Obj.FOnFoo = this.Obj.FOnFoo;',
    'this.Obj.SetFoo(this.Obj.GetFoo());',
    'this.Obj.SetEvents(2, this.Obj.GetEvents(3));',
    'this.Obj.FOnFoo = rtl.createCallback(this.Obj, "DoIt");',
    'this.Obj.SetFoo(rtl.createCallback(this.Obj, "DoIt"));',
    'this.Obj.SetEvents(4, rtl.createCallback(this.Obj, "DoIt"));',
    'this.Obj.FOnFoo = rtl.createCallback(this.Obj, "DoIt");',
    'this.Obj.SetFoo(rtl.createCallback(this.Obj, "DoIt"));',
    'this.Obj.SetEvents(4, rtl.createCallback(this.Obj, "DoIt"));',
    'this.Obj.FOnFoo(1);',
    'this.Obj.GetFoo();',
    'this.Obj.FOnFoo(1);',
    'this.Obj.GetFoo()(1);',
    'this.Obj.GetEvents(6)(1);',
    'this.b = this.Obj.FOnFoo(1) == this.vP(1);',
    'this.b = this.Obj.GetFoo() == this.vP(1);',
    'this.b = this.Obj.FOnFoo(1) == this.Obj.FOnFoo(1);',
    'this.b = this.Obj.GetFoo() == this.Obj.FOnFoo(1);',
    'this.b = this.Obj.FOnFoo(1) != this.Obj.FOnFoo(1);',
    'this.b = this.Obj.GetFoo() != this.Obj.FOnFoo(1);',
    'this.b = this.Obj.FOnFoo != null;',
    'this.b = this.Obj.GetFoo() != null;',
    'this.b = this.Obj.GetEvents(13) != null;',
    '']));
end;

procedure TTestModule.TestProcType_WithClassInstDoPropertyFPC;
begin
  StartProgram(false);
  Add('type');
  Add('  TFuncInt = function(vA: longint = 1): longint of object;');
  Add('  TObject = class');
  Add('    FOnFoo: TFuncInt;');
  Add('    function DoIt(vA: longint = 1): longint;');
  Add('    function GetFoo: TFuncInt;');
  Add('    procedure SetFoo(const Value: TFuncInt);');
  Add('    property OnFoo: TFuncInt read FOnFoo write FOnFoo;');
  Add('    property OnBar: TFuncInt read GetFoo write SetFoo;');
  Add('  end;');
  Add('function tobject.doit(va: longint = 1): longint; begin end;');
  Add('function tobject.getfoo: tfuncint; begin end;');
  Add('procedure tobject.setfoo(const value: tfuncint); begin end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  vP: tfuncint;');
  Add('  b: boolean;');
  Add('begin');
  Add('with obj do begin');
  Add('  fonfoo:=nil;');
  Add('  onfoo:=nil;');
  Add('  onbar:=nil;');
  Add('  fonfoo:=fonfoo;');
  Add('  onfoo:=onfoo;');
  Add('  onbar:=onbar;');
  Add('  fonfoo:=@doit;');
  Add('  onfoo:=@doit;');
  Add('  onbar:=@doit;');
  //Add('  fonfoo:=doit;'); // delphi
  //Add('  onfoo:=doit;'); // delphi
  //Add('  onbar:=doit;'); // delphi
  Add('  fonfoo;');
  Add('  onfoo;');
  Add('  onbar;');
  Add('  fonfoo();');
  Add('  onfoo();');
  Add('  onbar();');
  Add('  b:=fonfoo=nil;');
  Add('  b:=onfoo=nil;');
  Add('  b:=onbar=nil;');
  Add('  b:=fonfoo<>nil;');
  Add('  b:=onfoo<>nil;');
  Add('  b:=onbar<>nil;');
  Add('  b:=fonfoo=vp;');
  Add('  b:=onfoo=vp;');
  Add('  b:=onbar=vp;');
  Add('  b:=fonfoo=fonfoo;');
  Add('  b:=onfoo=onfoo;');
  Add('  b:=onbar=onfoo;');
  Add('  b:=fonfoo<>fonfoo;');
  Add('  b:=onfoo<>onfoo;');
  Add('  b:=onbar<>onfoo;');
  Add('  b:=fonfoo=@doit;');
  Add('  b:=onfoo=@doit;');
  Add('  b:=onbar=@doit;');
  Add('  b:=fonfoo<>@doit;');
  Add('  b:=onfoo<>@doit;');
  Add('  b:=onbar<>@doit;');
  Add('  b:=Assigned(fonfoo);');
  Add('  b:=Assigned(onfoo);');
  Add('  b:=Assigned(onbar);');
  Add('end;');
  ConvertProgram;
  CheckSource('TestProcType_WithClassInstDoPropertyFPC',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FOnFoo = null;',
    '  };',
    '  this.$final = function () {',
    '    this.FOnFoo = undefined;',
    '  };',
    '  this.DoIt = function (vA) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '  this.GetFoo = function () {',
    '    var Result = null;',
    '    return Result;',
    '  };',
    '  this.SetFoo = function (Value) {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.vP = null;',
    'this.b = false;'
    ]),
    LinesToStr([
    'var $with1 = this.Obj;',
    '$with1.FOnFoo = null;',
    '$with1.FOnFoo = null;',
    '$with1.SetFoo(null);',
    '$with1.FOnFoo = $with1.FOnFoo;',
    '$with1.FOnFoo = $with1.FOnFoo;',
    '$with1.SetFoo($with1.GetFoo());',
    '$with1.FOnFoo = rtl.createCallback($with1, "DoIt");',
    '$with1.FOnFoo = rtl.createCallback($with1, "DoIt");',
    '$with1.SetFoo(rtl.createCallback($with1, "DoIt"));',
    '$with1.FOnFoo(1);',
    '$with1.FOnFoo(1);',
    '$with1.GetFoo();',
    '$with1.FOnFoo(1);',
    '$with1.FOnFoo(1);',
    '$with1.GetFoo()(1);',
    'this.b = $with1.FOnFoo == null;',
    'this.b = $with1.FOnFoo == null;',
    'this.b = $with1.GetFoo() == null;',
    'this.b = $with1.FOnFoo != null;',
    'this.b = $with1.FOnFoo != null;',
    'this.b = $with1.GetFoo() != null;',
    'this.b = rtl.eqCallback($with1.FOnFoo, this.vP);',
    'this.b = rtl.eqCallback($with1.FOnFoo, this.vP);',
    'this.b = rtl.eqCallback($with1.GetFoo(), this.vP);',
    'this.b = rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    'this.b = rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    'this.b = rtl.eqCallback($with1.GetFoo(), $with1.FOnFoo);',
    'this.b = !rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    'this.b = !rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    'this.b = !rtl.eqCallback($with1.GetFoo(), $with1.FOnFoo);',
    'this.b = rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    'this.b = rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    'this.b = rtl.eqCallback($with1.GetFoo(), rtl.createCallback($with1, "DoIt"));',
    'this.b = !rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    'this.b = !rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    'this.b = !rtl.eqCallback($with1.GetFoo(), rtl.createCallback($with1, "DoIt"));',
    'this.b = $with1.FOnFoo != null;',
    'this.b = $with1.FOnFoo != null;',
    'this.b = $with1.GetFoo() != null;',
    '']));
end;

procedure TTestModule.TestProcType_Nested;
begin
  StartProgram(false);
  Add('type');
  Add('  TProcInt = procedure(vI: longint = 1);');
  Add('procedure DoIt(vJ: longint);');
  Add('var aProc: TProcInt;');
  Add('    b: boolean;');
  Add('  procedure Sub(vK: longint);');
  Add('  var aSub: TProcInt;');
  Add('    procedure SubSub(vK: longint);');
  Add('    var aSubSub: TProcInt;');
  Add('    begin;');
  Add('      aProc:=@DoIt;');
  Add('      aSub:=@DoIt;');
  Add('      aSubSub:=@DoIt;');
  Add('      aProc:=@Sub;');
  Add('      aSub:=@Sub;');
  Add('      aSubSub:=@Sub;');
  Add('      aProc:=@SubSub;');
  Add('      aSub:=@SubSub;');
  Add('      aSubSub:=@SubSub;');
  Add('    end;');
  Add('  begin;');
  Add('  end;');
  Add('begin;');
  Add('  aProc:=@Sub;');
  Add('  b:=aProc=@Sub;');
  Add('  b:=@Sub=aProc;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestProcType_Nested',
    LinesToStr([ // statements
    'this.DoIt = function (vJ) {',
    '  var aProc = null;',
    '  var b = false;',
    '  function Sub(vK) {',
    '    var aSub = null;',
    '    function SubSub(vK) {',
    '      var aSubSub = null;',
    '      aProc = rtl.createCallback(this, "DoIt");',
    '      aSub = rtl.createCallback(this, "DoIt");',
    '      aSubSub = rtl.createCallback(this, "DoIt");',
    '      aProc = rtl.createCallback(this, Sub);',
    '      aSub = rtl.createCallback(this, Sub);',
    '      aSubSub = rtl.createCallback(this, Sub);',
    '      aProc = rtl.createCallback(this, SubSub);',
    '      aSub = rtl.createCallback(this, SubSub);',
    '      aSubSub = rtl.createCallback(this, SubSub);',
    '    };',
    '  };',
    '  aProc = rtl.createCallback(this, Sub);',
    '  b = rtl.eqCallback(aProc, rtl.createCallback(this, Sub));',
    '  b = rtl.eqCallback(rtl.createCallback(this, Sub), aProc);',
    '};',
    '']),
    LinesToStr([ // this.$main
    '']));
end;

procedure TTestModule.TestJSValue_AssignToJSValue;
begin
  StartProgram(false);
  Add('var');
  Add('  v: jsvalue;');
  Add('  i: longint;');
  Add('  s: string;');
  Add('  b: boolean;');
  Add('  d: double;');
  Add('begin');
  Add('  v:=v;');
  Add('  v:=1;');
  Add('  v:=i;');
  Add('  v:='''';');
  Add('  v:=''c'';');
  Add('  v:=''foo'';');
  Add('  v:=s;');
  Add('  v:=false;');
  Add('  v:=true;');
  Add('  v:=b;');
  Add('  v:=0.1;');
  Add('  v:=d;');
  Add('  v:=nil;');
  ConvertProgram;
  CheckSource('TestJSValue_AssignToJSValue',
    LinesToStr([ // statements
    'this.v = undefined;',
    'this.i = 0;',
    'this.s = "";',
    'this.b = false;',
    'this.d = 0.0;',
    '']),
    LinesToStr([ // this.$main
    'this.v = this.v;',
    'this.v = 1;',
    'this.v = this.i;',
    'this.v = "";',
    'this.v = "c";',
    'this.v = "foo";',
    'this.v = this.s;',
    'this.v = false;',
    'this.v = true;',
    'this.v = this.b;',
    'this.v = 0.1;',
    'this.v = this.d;',
    'this.v = null;',
    '']));
end;

procedure TTestModule.TestJSValue_TypeCastToBaseType;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TYesNo = boolean;');
  Add('  TFloat = double;');
  Add('  TCaption = string;');
  Add('  TChar = char;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  i: integer;');
  Add('  s: TCaption;');
  Add('  b: TYesNo;');
  Add('  d: TFloat;');
  Add('  c: char;');
  Add('begin');
  Add('  i:=longint(v);');
  Add('  i:=integer(v);');
  Add('  s:=string(v);');
  Add('  s:=TCaption(v);');
  Add('  b:=boolean(v);');
  Add('  b:=TYesNo(v);');
  Add('  d:=double(v);');
  Add('  d:=TFloat(v);');
  Add('  c:=char(v);');
  Add('  c:=TChar(v);');
  ConvertProgram;
  CheckSource('TestJSValue_TypeCastToBaseType',
    LinesToStr([ // statements
    'this.v = undefined;',
    'this.i = 0;',
    'this.s = "";',
    'this.b = false;',
    'this.d = 0.0;',
    'this.c = "";',
    '']),
    LinesToStr([ // this.$main
    'this.i = Math.floor(this.v);',
    'this.i = Math.floor(this.v);',
    'this.s = "" + this.v;',
    'this.s = "" + this.v;',
    'this.b = !(this.v == false);',
    'this.b = !(this.v == false);',
    'this.d = rtl.getNumber(this.v);',
    'this.d = rtl.getNumber(this.v);',
    'this.c = rtl.getChar(this.v);',
    'this.c = rtl.getChar(this.v);',
    '']));
end;

procedure TTestModule.TestJSValue_Equal;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TYesNo = boolean;');
  Add('  TFloat = double;');
  Add('  TCaption = string;');
  Add('  TChar = char;');
  Add('  TMulti = JSValue;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  i: integer;');
  Add('  s: TCaption;');
  Add('  b: TYesNo;');
  Add('  d: TFloat;');
  Add('  c: char;');
  Add('  m: TMulti;');
  Add('begin');
  Add('  b:=v=v;');
  Add('  b:=v<>v;');
  Add('  b:=v=1;');
  Add('  b:=v<>1;');
  Add('  b:=2=v;');
  Add('  b:=2<>v;');
  Add('  b:=v=i;');
  Add('  b:=i=v;');
  Add('  b:=v=nil;');
  Add('  b:=nil=v;');
  Add('  b:=v=false;');
  Add('  b:=true=v;');
  Add('  b:=v=b;');
  Add('  b:=b=v;');
  Add('  b:=v=s;');
  Add('  b:=s=v;');
  Add('  b:=v=''foo'';');
  Add('  b:=''''=v;');
  Add('  b:=v=d;');
  Add('  b:=d=v;');
  Add('  b:=v=3.4;');
  Add('  b:=5.6=v;');
  Add('  b:=v=c;');
  Add('  b:=c=v;');
  Add('  b:=m=m;');
  Add('  b:=v=m;');
  Add('  b:=m=v;');
  ConvertProgram;
  CheckSource('TestJSValue_Equal',
    LinesToStr([ // statements
    'this.v = undefined;',
    'this.i = 0;',
    'this.s = "";',
    'this.b = false;',
    'this.d = 0.0;',
    'this.c = "";',
    'this.m = undefined;',
    '']),
    LinesToStr([ // this.$main
    'this.b = this.v == this.v;',
    'this.b = this.v != this.v;',
    'this.b = this.v == 1;',
    'this.b = this.v != 1;',
    'this.b = 2 == this.v;',
    'this.b = 2 != this.v;',
    'this.b = this.v == this.i;',
    'this.b = this.i == this.v;',
    'this.b = this.v == null;',
    'this.b = null == this.v;',
    'this.b = this.v == false;',
    'this.b = true == this.v;',
    'this.b = this.v == this.b;',
    'this.b = this.b == this.v;',
    'this.b = this.v == this.s;',
    'this.b = this.s == this.v;',
    'this.b = this.v == "foo";',
    'this.b = "" == this.v;',
    'this.b = this.v == this.d;',
    'this.b = this.d == this.v;',
    'this.b = this.v == 3.4;',
    'this.b = 5.6 == this.v;',
    'this.b = this.v == this.c;',
    'this.b = this.c == this.v;',
    'this.b = this.m == this.m;',
    'this.b = this.v == this.m;',
    'this.b = this.m == this.v;',
    '']));
end;

procedure TTestModule.TestJSValue_Enum;
begin
  StartProgram(false);
  Add('type');
  Add('  TColor = (red, blue);');
  Add('  TRedBlue = TColor;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  e: TColor;');
  Add('begin');
  Add('  v:=e;');
  Add('  v:=TColor(e);');
  Add('  v:=TRedBlue(e);');
  Add('  e:=TColor(v);');
  Add('  e:=TRedBlue(v);');
  ConvertProgram;
  CheckSource('TestJSValue_Enum',
    LinesToStr([ // statements
    'this.TColor = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "blue",',
    '  blue: 1',
    '};',
    'this.v = undefined;',
    'this.e = 0;',
    '']),
    LinesToStr([ // this.$main
    'this.v = this.e;',
    'this.v = this.e;',
    'this.v = this.e;',
    'this.e = this.v;',
    'this.e = this.v;',
    '']));
end;

procedure TTestModule.TestJSValue_ClassInstance;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TBirdObject = TObject;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  o: TObject;');
  Add('begin');
  Add('  v:=o;');
  Add('  v:=TObject(o);');
  Add('  v:=TBirdObject(o);');
  Add('  o:=TObject(v);');
  Add('  o:=TBirdObject(v);');
  ConvertProgram;
  CheckSource('TestJSValue_ClassInstance',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.v = undefined;',
    'this.o = null;',
    '']),
    LinesToStr([ // this.$main
    'this.v = this.o;',
    'this.v = this.o;',
    'this.v = this.o;',
    'this.o = rtl.getObject(this.v);',
    'this.o = rtl.getObject(this.v);',
    '']));
end;

procedure TTestModule.TestJSValue_ClassOf;
begin
  StartProgram(false);
  Add('type');
  Add('  TClass = class of TObject;');
  Add('  TObject = class');
  Add('  end;');
  Add('  TBirds = class of TBird;');
  Add('  TBird = class(TObject) end;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  c: TClass;');
  Add('begin');
  Add('  v:=c;');
  Add('  v:=TClass(c);');
  Add('  v:=TBirds(c);');
  Add('  c:=TClass(v);');
  Add('  c:=TBirds(v);');
  ConvertProgram;
  CheckSource('TestJSValue_ClassOf',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TBird", this.TObject, function () {',
    '});',
    'this.v = undefined;',
    'this.c = null;',
    '']),
    LinesToStr([ // this.$main
    'this.v = this.c;',
    'this.v = this.c;',
    'this.v = this.c;',
    'this.c = rtl.getObject(this.v);',
    'this.c = rtl.getObject(this.v);',
    '']));
end;

procedure TTestModule.TestJSValue_ArrayOfJSValue;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TArray = array of JSValue;');
  Add('  TArrgh = tarray;');
  Add('  TArrInt = array of integer;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  TheArray: tarray;');
  Add('  Arr: tarrgh;');
  Add('  i: integer;');
  Add('  ArrInt: tarrint;');
  Add('begin');
  Add('  arr:=thearray;');
  Add('  thearray:=arr;');
  Add('  setlength(arr,2);');
  Add('  setlength(thearray,3);');
  Add('  arr[4]:=v;');
  Add('  arr[5]:=length(thearray);');
  Add('  arr[6]:=nil;');
  Add('  arr[7]:=thearray[8];');
  Add('  arr[low(arr)]:=high(thearray);');
  Add('  arr:=arrint;');
  Add('  arrInt:=tarrint(arr);');
  Add('  if TheArray = nil then ;');
  Add('  if nil = TheArray then ;');
  Add('  if TheArray <> nil then ;');
  Add('  if nil <> TheArray then ;');
  ConvertProgram;
  CheckSource('TestJSValue_ArrayOfJSValue',
    LinesToStr([ // statements
    'this.v = undefined;',
    'this.TheArray = [];',
    'this.Arr = [];',
    'this.i = 0;',
    'this.ArrInt = [];',
    '']),
    LinesToStr([ // this.$main
    'this.Arr = this.TheArray;',
    'this.TheArray = this.Arr;',
    'this.Arr = rtl.arraySetLength(this.Arr,2,undefined);',
    'this.TheArray = rtl.arraySetLength(this.TheArray,3,undefined);',
    'this.Arr[4] = this.v;',
    'this.Arr[5] = rtl.length(this.TheArray);',
    'this.Arr[6] = null;',
    'this.Arr[7] = this.TheArray[8];',
    'this.Arr[0] = rtl.length(this.TheArray) - 1;',
    'this.Arr = this.ArrInt;',
    'this.ArrInt = this.Arr;',
    'if (rtl.length(this.TheArray) == 0) ;',
    'if (rtl.length(this.TheArray) == 0) ;',
    'if (rtl.length(this.TheArray) > 0) ;',
    'if (rtl.length(this.TheArray) > 0) ;',
    '']));
end;

procedure TTestModule.TestJSValue_Params;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TYesNo = boolean;');
  Add('  TFloat = double;');
  Add('  TCaption = string;');
  Add('  TChar = char;');
  Add('function DoIt(a: jsvalue; const b: jsvalue; var c: jsvalue; out d: jsvalue): jsvalue;');
  Add('var');
  Add('  l: jsvalue;');
  Add('begin');
  Add('  a:=a;');
  Add('  l:=b;');
  Add('  c:=c;');
  Add('  d:=d;');
  Add('  Result:=l;');
  Add('end;');
  Add('function DoSome(a: jsvalue; const b: jsvalue): jsvalue; begin end;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  i: integer;');
  Add('  b: TYesNo;');
  Add('  d: TFloat;');
  Add('  s: TCaption;');
  Add('  c: TChar;');
  Add('begin');
  Add('  v:=doit(v,v,v,v);');
  Add('  i:=integer(dosome(i,i));');
  Add('  b:=TYesNo(dosome(b,b));');
  Add('  d:=TFloat(dosome(d,d));');
  Add('  s:=TCaption(dosome(s,s));');
  Add('  c:=TChar(dosome(c,c));');
  ConvertProgram;
  CheckSource('TestJSValue_Params',
    LinesToStr([ // statements
    'this.DoIt = function (a, b, c, d) {',
    '  var Result = undefined;',
    '  var l = undefined;',
    '  a = a;',
    '  l = b;',
    '  c.set(c.get());',
    '  d.set(d.get());',
    '  Result = l;',
    '  return Result;',
    '};',
    'this.DoSome = function (a, b) {',
    '  var Result = undefined;',
    '  return Result;',
    '};',
    'this.v = undefined;',
    'this.i = 0;',
    'this.b = false;',
    'this.d = 0.0;',
    'this.s = "";',
    'this.c = "";',
    '']),
    LinesToStr([ // this.$main
    'this.v = this.DoIt(this.v, this.v, {',
    '  p: this,',
    '  get: function () {',
    '      return this.p.v;',
    '    },',
    '  set: function (v) {',
    '      this.p.v = v;',
    '    }',
    '}, {',
    '  p: this,',
    '  get: function () {',
    '      return this.p.v;',
    '    },',
    '  set: function (v) {',
    '      this.p.v = v;',
    '    }',
    '});',
    'this.i = Math.floor(this.DoSome(this.i, this.i));',
    'this.b = !(this.DoSome(this.b, this.b) == false);',
    'this.d = rtl.getNumber(this.DoSome(this.d, this.d));',
    'this.s = "" + this.DoSome(this.s, this.s);',
    'this.c = rtl.getChar(this.DoSome(this.c, this.c));',
    '']));
end;

procedure TTestModule.TestJSValue_UntypedParam;
begin
  StartProgram(false);
  Add('function DoIt(const a; var b; out c): jsvalue;');
  Add('begin');
  Add('  Result:=a;');
  Add('  Result:=b;');
  Add('  Result:=c;');
  Add('  b:=Result;');
  Add('  c:=Result;');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  doit(i,i,i);');
  ConvertProgram;
  CheckSource('TestJSValue_UntypedParam',
    LinesToStr([ // statements
    'this.DoIt = function (a, b, c) {',
    '  var Result = undefined;',
    '  Result = a;',
    '  Result = b.get();',
    '  Result = c.get();',
    '  b.set(Result);',
    '  c.set(Result);',
    '  return Result;',
    '};',
    'this.i = 0;',
    '']),
    LinesToStr([ // this.$main
    'this.DoIt(this.i, {',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '}, {',
    '  p: this,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestJSValue_FuncType;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TJSValueArray = array of JSValue;');
  Add('  TListSortCompare = function(Item1, Item2: JSValue): Integer;');
  Add('procedure Sort(P: JSValue; aList: TJSValueArray; const Compare: TListSortCompare);');
  Add('begin');
  Add('  while Compare(P,aList[0])>0 do ;');
  Add('end;');
  Add('var');
  Add('  Compare: TListSortCompare;');
  Add('  V: JSValue;');
  Add('  i: integer;');
  Add('begin');
  Add('  if Compare(V,V)>0 then ;');
  Add('  if Compare(i,i)>1 then ;');
  Add('  if Compare(nil,false)>2 then ;');
  Add('  if Compare(1,true)>3 then ;');
  ConvertProgram;
  CheckSource('TestJSValue_UntypedParam',
    LinesToStr([ // statements
    'this.Sort = function (P, aList, Compare) {',
    '  while (Compare(P, aList[0]) > 0) {',
    '  };',
    '};',
    'this.Compare = null;',
    'this.V = undefined;',
    'this.i = 0;',
    '']),
    LinesToStr([ // this.$main
    'if (this.Compare(this.V, this.V) > 0) ;',
    'if (this.Compare(this.i, this.i) > 1) ;',
    'if (this.Compare(null, false) > 2) ;',
    'if (this.Compare(1, true) > 3) ;',
    '']));
end;

Initialization
  RegisterTests([TTestModule]);
end.

