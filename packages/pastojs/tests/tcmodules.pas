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
  Classes, SysUtils, fpcunit, testregistry, contnrs, fppas2js, pastree,
  PScanner, PasResolver, PParser, PasResolveEval, jstree, jswriter, jsbase;

const
  // default parser+scanner options
  po_pas2js = [po_asmwhole,po_resolvestandardtypes];
  co_tcmodules = [coNoTypeInfo];
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
    FJSImplementationSrc: TJSSourceElements;
    FJSImplementationUses: TJSArrayLiteral;
    FJSInitBody: TJSFunctionBody;
    FJSImplentationUses: TJSArrayLiteral;
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
    function FindUnit(const aUnitName: String): TPasModule;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure Add(Line: string); virtual;
    Procedure Add(const Lines: array of string);
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
    procedure ConvertModule; virtual;
    procedure ConvertProgram; virtual;
    procedure ConvertUnit; virtual;
    procedure CheckDottedIdentifier(Msg: string; El: TJSElement; DottedName: string);
    function GetDottedIdentifier(El: TJSElement): string;
    procedure CheckSource(Msg,Statements: String; InitStatements: string = '';
      ImplStatements: string = ''); virtual;
    procedure CheckDiff(Msg, Expected, Actual: string); virtual;
    procedure SetExpectedScannerError(Msg: string; MsgNumber: integer);
    procedure SetExpectedParserError(Msg: string; MsgNumber: integer);
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
    function GetDefaultNamespace: string;
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
    property JSImplementationUses: TJSArrayLiteral read FJSImplementationUses;
    property JSInterfaceUses: TJSArrayLiteral read FJSInterfaceUses;
    property JSModuleSrc: TJSSourceElements read FJSModuleSrc;
    property JSInitBody: TJSFunctionBody read FJSInitBody;
    property JSImplementationSrc: TJSSourceElements read FJSImplementationSrc;
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
    Procedure TestDottedUnitNames;
    Procedure TestDottedUnitExpr;
    Procedure Test_ModeFPCFail;
    Procedure Test_ModeSwitchCBlocksFail;

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

    // numbers
    Procedure TestDouble;

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
    Procedure TestTypeShortstring_Fail;

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
    Procedure TestProc_External;
    Procedure TestProc_ExternalOtherUnit;
    Procedure TestProc_Asm;
    Procedure TestProc_Assembler;
    Procedure TestProc_VarParam;
    Procedure TestProc_Overload;
    Procedure TestProc_OverloadForward;
    Procedure TestProc_OverloadUnit;
    Procedure TestProc_OverloadNested;
    Procedure TestProc_Varargs;

    // enums, sets
    Procedure TestEnum_Name;
    Procedure TestEnum_Number;
    Procedure TestEnum_Functions;
    Procedure TestEnum_AsParams;
    Procedure TestSet;
    Procedure TestSet_Operators;
    Procedure TestSet_Operator_In;
    Procedure TestSet_Functions;
    Procedure TestSet_PassAsArgClone;
    Procedure TestSet_AsParams;
    Procedure TestSet_Property;
    Procedure TestSet_EnumConst;
    Procedure TestSet_AnonymousEnumType;
    Procedure TestSet_CharFail;
    Procedure TestSet_BooleanFail;
    Procedure TestSet_ConstEnum;
    Procedure TestSet_ConstChar;

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
    Procedure TestAsmPas_Impl; // ToDo
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
    Procedure TestArray_SetLengthOutArg;
    Procedure TestArray_SetLengthProperty;
    Procedure TestArray_OpenArrayOfString;
    Procedure TestArray_Concat;
    Procedure TestArray_Copy;
    Procedure TestArray_InsertDelete;
    Procedure TestArray_DynArrayConst;
    Procedure TestExternalClass_TypeCastArrayToExternalArray;
    Procedure TestExternalClass_TypeCastArrayFromExternalArray;
    // ToDo: static array const
    // ToDo: SetLength(array of static array)
    // ToDo: SetLength(dim1,dim2)

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
    Procedure TestRecord_TypeCastJSValueToRecord;
    // ToDo: const record

    // classes
    Procedure TestClass_TObjectDefaultConstructor;
    Procedure TestClass_TObjectConstructorWithParams;
    Procedure TestClass_Var;
    Procedure TestClass_Method;
    Procedure TestClass_Implementation;
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
    Procedure TestClass_Const;
    Procedure TestClass_LocalVarSelfFail;
    Procedure TestClass_ArgSelfFail;
    Procedure TestClass_NestedSelf;
    Procedure TestClass_NestedClassSelf;
    Procedure TestClass_NestedCallInherited;
    Procedure TestClass_TObjectFree;
    Procedure TestClass_TObjectFreeNewInstance;
    Procedure TestClass_TObjectFreeLowerCase;
    Procedure TestClass_TObjectFreeFunctionFail;
    Procedure TestClass_TObjectFreePropertyFail;

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

    // nested class
    Procedure TestNestedClass_Fail;

    // external class
    Procedure TestExternalClass_Var;
    //ToDo Procedure TestExternalClass_Const;
    Procedure TestExternalClass_Dollar;
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
    Procedure TestExternalClass_PascalProperty;
    Procedure TestExternalClass_TypeCastToRootClass;
    Procedure TestExternalClass_TypeCastStringToExternalString;
    Procedure TestExternalClass_CallClassFunctionOfInstanceFail;
    Procedure TestExternalClass_BracketAccessor;
    Procedure TestExternalClass_BracketAccessor_2ParamsFail;
    Procedure TestExternalClass_BracketAccessor_ReadOnly;
    Procedure TestExternalClass_BracketAccessor_WriteOnly;
    Procedure TestExternalClass_BracketAccessor_MultiType;
    Procedure TestExternalClass_BracketAccessor_Index;

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
    Procedure TestProcType_NestedOfObject;
    Procedure TestProcType_ReferenceToProc;
    Procedure TestProcType_ReferenceToMethod;
    Procedure TestProcType_Typecast;
    Procedure TestProcType_PassProcToUntyped;

    // pointer
    Procedure TestPointer;
    Procedure TestPointer_Proc;
    Procedure TestPointer_AssignRecordFail;
    Procedure TestPointer_AssignStaticArrayFail;
    Procedure TestPointer_ArrayParamsFail;
    Procedure TestPointer_TypeCastJSValueToPointer;

    // jsvalue
    Procedure TestJSValue_AssignToJSValue;
    Procedure TestJSValue_TypeCastToBaseType;
    Procedure TestJSValue_Equal;
    Procedure TestJSValue_If;
    Procedure TestJSValue_Enum;
    Procedure TestJSValue_ClassInstance;
    Procedure TestJSValue_ClassOf;
    Procedure TestJSValue_ArrayOfJSValue;
    Procedure TestJSValue_Params;
    Procedure TestJSValue_UntypedParam;
    Procedure TestJSValue_FuncResultType;
    Procedure TestJSValue_ProcType_Assign;
    Procedure TestJSValue_ProcType_Equal;
    Procedure TestJSValue_AssignToPointerFail;
    Procedure TestJSValue_OverloadDouble;
    Procedure TestJSValue_OverloadNativeInt;
    Procedure TestJSValue_OverloadWord;
    Procedure TestJSValue_OverloadString;
    Procedure TestJSValue_OverloadChar;
    Procedure TestJSValue_OverloadPointer;

    // RTTI
    Procedure TestRTTI_ProcType;
    Procedure TestRTTI_ProcType_ArgFromOtherUnit;
    Procedure TestRTTI_EnumAndSetType;
    Procedure TestRTTI_AnonymousEnumType;
    Procedure TestRTTI_StaticArray;
    Procedure TestRTTI_DynArray;
    Procedure TestRTTI_ArrayNestedAnonymous;
    // ToDo: Procedure TestRTTI_Pointer;
    Procedure TestRTTI_PublishedMethodOverloadFail;
    Procedure TestRTTI_PublishedMethodExternalFail;
    Procedure TestRTTI_PublishedClassPropertyFail;
    Procedure TestRTTI_PublishedClassFieldFail;
    Procedure TestRTTI_PublishedFieldExternalFail;
    Procedure TestRTTI_Class_Field;
    Procedure TestRTTI_Class_Method;
    Procedure TestRTTI_Class_MethodArgFlags;
    Procedure TestRTTI_Class_Property;
    Procedure TestRTTI_Class_PropertyParams;
    // ToDo: property default value
    Procedure TestRTTI_OverrideMethod;
    Procedure TestRTTI_OverloadProperty;
    // ToDo: array argument
    Procedure TestRTTI_ClassForward;
    Procedure TestRTTI_ClassOf;
    Procedure TestRTTI_Record;
    Procedure TestRTTI_LocalTypes;
    Procedure TestRTTI_TypeInfo_BaseTypes;
    Procedure TestRTTI_TypeInfo_LocalFail;
    Procedure TestRTTI_TypeInfo_ExtTypeInfoClasses1;
    Procedure TestRTTI_TypeInfo_ExtTypeInfoClasses2;
    Procedure TestRTTI_TypeInfo_ExtTypeInfoClasses3;
    Procedure TestRTTI_TypeInfo_FunctionClassType;
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
  DefNamespace: String;
begin
  //writeln('TTestModule.OnPasResolverFindUnit START Unit="',aUnitName,'"');
  if (Pos('.',aUnitName)<1) then
    begin
    DefNamespace:=GetDefaultNamespace;
    if DefNamespace<>'' then
      begin
      Result:=FindUnit(DefNamespace+'.'+aUnitName);
      if Result<>nil then exit;
      end;
    end;
  Result:=FindUnit(aUnitName);
  if Result<>nil then exit;
  writeln('TTestModule.OnPasResolverFindUnit missing unit "',aUnitName,'"');
  Fail('can''t find unit "'+aUnitName+'"');
end;

function TCustomTestModule.FindUnit(const aUnitName: String): TPasModule;
var
  i: Integer;
  CurEngine: TTestEnginePasResolver;
  CurUnitName: String;
begin
  //writeln('TTestModule.FindUnit START Unit="',aUnitName,'"');
  Result:=nil;
  for i:=0 to ModuleCount-1 do
    begin
    CurEngine:=Modules[i];
    CurUnitName:=ExtractFileUnitName(CurEngine.Filename);
    //writeln('TTestModule.FindUnit Checking ',i,'/',ModuleCount,' ',CurEngine.Filename,' ',CurUnitName);
    if CompareText(aUnitName,CurUnitName)=0 then
      begin
      Result:=CurEngine.Module;
      if Result<>nil then exit;
      //writeln('TTestModule.FindUnit PARSING unit "',CurEngine.Filename,'"');
      FileResolver.FindSourceFile(aUnitName);

      CurEngine.Resolver:=TStreamResolver.Create;
      CurEngine.Resolver.OwnsStreams:=True;
      //writeln('TTestModule.FindUnit SOURCE=',CurEngine.Source);
      CurEngine.Resolver.AddStream(CurEngine.FileName,TStringStream.Create(CurEngine.Source));
      CurEngine.Scanner:=TPascalScanner.Create(CurEngine.Resolver);
      CurEngine.Parser:=TTestPasParser.Create(CurEngine.Scanner,CurEngine.Resolver,CurEngine);
      CurEngine.Parser.Options:=CurEngine.Parser.Options+po_pas2js+[po_KeepScannerError];
      if CompareText(CurUnitName,'System')=0 then
        CurEngine.Parser.ImplicitUses.Clear;
      CurEngine.Scanner.OpenFile(CurEngine.Filename);
      try
        CurEngine.Parser.NextToken;
        CurEngine.Parser.ParseUnit(CurEngine.FModule);
      except
        on E: Exception do
          HandleException(E);
      end;
      //writeln('TTestModule.FindUnit END ',CurUnitName);
      Result:=CurEngine.Module;
      exit;
      end;
    end;
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
  FScanner.AllowedModeSwitches:=msAllPas2jsModeSwitches;
  FScanner.ReadOnlyModeSwitches:=msAllPas2jsModeSwitchesReadOnly;
  FScanner.CurrentModeSwitches:=OBJFPCModeSwitches*msAllPas2jsModeSwitches+msAllPas2jsModeSwitchesReadOnly;
  FEngine:=AddModule(Filename);
  FParser:=TTestPasParser.Create(FScanner,FFileResolver,FEngine);
  Parser.Options:=Parser.Options+po_pas2js+[po_KeepScannerError];
  FModule:=Nil;
  FConverter:=TPasToJSConverter.Create;
  FConverter.Options:=co_tcmodules;

  FExpectedErrorClass:=nil;
end;

procedure TCustomTestModule.TearDown;
begin
  FSkipTests:=false;
  FJSModule:=nil;
  FJSRegModuleCall:=nil;
  FJSModuleCallArgs:=nil;
  FJSImplentationUses:=nil;
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

procedure TCustomTestModule.Add(const Lines: array of string);
var
  i: Integer;
begin
  for i:=low(Lines) to high(Lines) do
    Add(Lines[i]);
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
  Result.AddObjFPCBuiltInIdentifiers(btAllJSBaseTypes,bfAllJSBaseProcs);
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
  Add('program '+ExtractFileUnitName(Filename)+';');
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

  procedure CheckUsesList(UsesName: String; Arg: TJSArrayLiteralElement;
    out UsesLit: TJSArrayLiteral);
  var
    i: Integer;
    Item: TJSElement;
    Lit: TJSLiteral;
  begin
    UsesLit:=nil;
    AssertNotNull(UsesName+' uses section',Arg.Expr);
    if (Arg.Expr.ClassType=TJSLiteral) and TJSLiteral(Arg.Expr).Value.IsNull then
      exit; // null is ok
    AssertEquals(UsesName+' uses section param is array',TJSArrayLiteral,Arg.Expr.ClassType);
    FJSInterfaceUses:=TJSArrayLiteral(Arg.Expr);
    for i:=0 to FJSInterfaceUses.Elements.Count-1 do
      begin
      Item:=FJSInterfaceUses.Elements.Elements[i].Expr;
      AssertNotNull(UsesName+' uses section item['+IntToStr(i)+'].Expr',Item);
      AssertEquals(UsesName+' uses section item['+IntToStr(i)+'] is lit',TJSLiteral,Item.ClassType);
      Lit:=TJSLiteral(Item);
      AssertEquals(UsesName+' uses section item['+IntToStr(i)+'] is string lit',
        ord(jsbase.jstString),ord(Lit.Value.ValueType));
      end;
  end;

  procedure CheckFunctionParam(ParamName: string; Arg: TJSArrayLiteralElement;
    out Src: TJSSourceElements);
  var
    FunDecl: TJSFunctionDeclarationStatement;
    FunDef: TJSFuncDef;
    FunBody: TJSFunctionBody;
  begin
    Src:=nil;
    AssertNotNull(ParamName,Arg.Expr);
    AssertEquals(ParamName+' Arg.Expr type',TJSFunctionDeclarationStatement,Arg.Expr.ClassType);
    FunDecl:=Arg.Expr as TJSFunctionDeclarationStatement;
    AssertNotNull(ParamName+' FunDecl.AFunction',FunDecl.AFunction);
    AssertEquals(ParamName+' FunDecl.AFunction type',TJSFuncDef,FunDecl.AFunction.ClassType);
    FunDef:=FunDecl.AFunction as TJSFuncDef;
    AssertEquals(ParamName+' name empty','',String(FunDef.Name));
    AssertNotNull(ParamName+' body',FunDef.Body);
    AssertEquals(ParamName+' body type',TJSFunctionBody,FunDef.Body.ClassType);
    FunBody:=FunDef.Body as TJSFunctionBody;
    AssertNotNull(ParamName+' body.A',FunBody.A);
    AssertEquals(ParamName+' body.A type',TJSSourceElements,FunBody.A.ClassType);
    Src:=FunBody.A as TJSSourceElements;
  end;

var
  ModuleNameExpr: TJSLiteral;
  InitFunction: TJSFunctionDeclarationStatement;
  InitAssign: TJSSimpleAssignStatement;
  InitName: String;
  LastNode: TJSElement;
  Arg: TJSArrayLiteralElement;
begin
  if SkipTests then exit;
  try
    FJSModule:=FConverter.ConvertPasElement(Module,Engine) as TJSSourceElements;
  except
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

  // parameter 'unitname'
  if JSModuleCallArgs.Elements.Count<1 then
    Fail('rtl.module first param unit missing');
  Arg:=JSModuleCallArgs.Elements.Elements[0];
  AssertNotNull('module name param',Arg.Expr);
  ModuleNameExpr:=Arg.Expr as TJSLiteral;
  AssertEquals('module name param is string',ord(jstString),ord(ModuleNameExpr.Value.ValueType));
  if Module is TPasProgram then
    AssertEquals('module name','program',String(ModuleNameExpr.Value.AsString))
  else
    AssertEquals('module name',Module.Name,String(ModuleNameExpr.Value.AsString));

  // main uses section
  if JSModuleCallArgs.Elements.Count<2 then
    Fail('rtl.module second param main uses missing');
  Arg:=JSModuleCallArgs.Elements.Elements[1];
  CheckUsesList('interface',Arg,FJSInterfaceUses);

  // program/library/interface function()
  if JSModuleCallArgs.Elements.Count<3 then
    Fail('rtl.module third param intf-function missing');
  Arg:=JSModuleCallArgs.Elements.Elements[2];
  CheckFunctionParam('module intf-function',Arg,FJSModuleSrc);

  // search for $mod.$init or $mod.$main - the last statement
  if Module is TPasProgram then
    begin
    InitName:='$main';
    AssertEquals('$mod.'+InitName+' function 1',true,JSModuleSrc.Statements.Count>0);
    end
  else
    InitName:='$init';
  FJSInitBody:=nil;
  if JSModuleSrc.Statements.Count>0 then
    begin
    LastNode:=JSModuleSrc.Statements.Nodes[JSModuleSrc.Statements.Count-1].Node;
    if LastNode is TJSSimpleAssignStatement then
      begin
      InitAssign:=LastNode as TJSSimpleAssignStatement;
      if GetDottedIdentifier(InitAssign.LHS)='$mod.'+InitName then
        begin
        InitFunction:=InitAssign.Expr as TJSFunctionDeclarationStatement;
        FJSInitBody:=InitFunction.AFunction.Body as TJSFunctionBody;
        end
      else if Module is TPasProgram then
        CheckDottedIdentifier('init function',InitAssign.LHS,'$mod.'+InitName);
      end;
    end;

  // optional: implementation uses section
  if JSModuleCallArgs.Elements.Count<4 then
    exit;
  Arg:=JSModuleCallArgs.Elements.Elements[3];
  CheckUsesList('implementation',Arg,FJSImplentationUses);

  // optional: implementation function()
  if JSModuleCallArgs.Elements.Count<5 then
    exit;
  Arg:=JSModuleCallArgs.Elements.Elements[4];
  CheckFunctionParam('module impl-function',Arg,FJSImplementationSrc);
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

procedure TCustomTestModule.CheckSource(Msg, Statements: String;
  InitStatements: string; ImplStatements: string);
var
  ActualSrc, ExpectedSrc, InitName: String;
begin
  ActualSrc:=JSToStr(JSModuleSrc);
  ExpectedSrc:=
    'var $mod = this;'+LineEnding
   +Statements;
  if coUseStrict in Converter.Options then
    ExpectedSrc:='"use strict";'+LineEnding+ExpectedSrc;
  if Module is TPasProgram then
    InitName:='$main'
  else
    InitName:='$init';
  if (Module is TPasProgram) or (Trim(InitStatements)<>'') then
    ExpectedSrc:=ExpectedSrc+LineEnding
      +'$mod.'+InitName+' = function () {'+LineEnding
      +InitStatements
      +'};'+LineEnding;
  //writeln('TTestModule.CheckSource InitStatements="',InitStatements,'"');
  CheckDiff(Msg,ExpectedSrc,ActualSrc);

  if (JSImplementationSrc<>nil) then
    begin
    ActualSrc:=JSToStr(JSImplementationSrc);
    ExpectedSrc:=
      'var $mod = this;'+LineEnding
     +'var $impl = $mod.$impl;'+LineEnding
     +ImplStatements;
    end
  else
    begin
    ActualSrc:='';
    ExpectedSrc:=ImplStatements;
    end;
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
    Fail('diff found, but lines are the same, internal error');
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

procedure TCustomTestModule.SetExpectedScannerError(Msg: string;
  MsgNumber: integer);
begin
  ExpectedErrorClass:=EScannerError;
  ExpectedErrorMsg:=Msg;
  ExpectedErrorNumber:=MsgNumber;
end;

procedure TCustomTestModule.SetExpectedParserError(Msg: string;
  MsgNumber: integer);
begin
  ExpectedErrorClass:=EParserError;
  ExpectedErrorMsg:=Msg;
  ExpectedErrorNumber:=MsgNumber;
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
  else if E is EParserError then
    MsgNumber:=Parser.LastMsgNumber
  else if E is EScannerError then
    MsgNumber:=Scanner.LastMsgNumber
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
    +' MainModuleScannerLine="'+Scanner.CurLine+'"'
    );
  RaiseException(E);
end;

procedure TCustomTestModule.HandlePasResolveError(E: EPasResolve);
var
  P: TPasSourcePos;
begin
  if IsErrorExpected(E) then exit;
  P:=E.SourcePos;
  WriteSources(P.FileName,P.Row,P.Column);
  writeln('ERROR: TCustomTestModule.HandlePasResolveError '+E.ClassName+':'+E.Message
    +' '+P.FileName+'('+IntToStr(P.Row)+','+IntToStr(P.Column)+')');
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
  if E is EScannerError then
    HandleScannerError(EScannerError(E))
  else if E is EParserError then
    HandleParserError(EParserError(E))
  else if E is EPasResolve then
    HandlePasResolveError(EPasResolve(E))
  else if E is EPas2JS then
    HandlePas2JSError(EPas2JS(E))
  else
    begin
    if IsErrorExpected(E) then exit;
    if not (E is EAssertionFailedError) then
      begin
      WriteSources('',0,0);
      writeln('ERROR: TCustomTestModule.HandleException '+E.ClassName+':'+E.Message);
      end;
    RaiseException(E);
    end;
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
      else if E is EParserError then
        MsgNumber:=Parser.LastMsgNumber
      else if E is EScannerError then
        MsgNumber:=Scanner.LastMsgNumber
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
  writeln('TCustomTestModule.WriteSources File="',aFilename,'" Row=',aRow,' Col=',aCol);
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

function TCustomTestModule.GetDefaultNamespace: string;
var
  C: TClass;
begin
  Result:='';
  if FModule=nil then exit;
  C:=FModule.ClassType;
  if (C=TPasProgram) or (C=TPasLibrary) or (C=TPasPackage) then
    Result:=Engine.DefaultNameSpace;
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
  CheckSource('TestEmptyProgramUseStrict','','');
end;

procedure TTestModule.TestEmptyUnit;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  ConvertUnit;
  CheckSource('TestEmptyUnit',
    LinesToStr([
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
    ''
    ]),
    '');
end;

procedure TTestModule.TestDottedUnitNames;
begin
  AddModuleWithIntfImplSrc('NS1.Unit2.pas',
    LinesToStr([
    'var iV: longint;'
    ]),
    '');

  FFilename:='ns1.test1.pp';
  StartProgram(true);
  Add('uses unIt2;');
  Add('implementation');
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  i:=iv;');
  Add('  i:=uNit2.iv;');
  Add('  i:=Ns1.TEst1.i;');
  ConvertProgram;
  CheckSource('TestDottedUnitNames',
    LinesToStr([
    'this.i = 0;',
    '']),
    LinesToStr([ // this.$init
    '$mod.i = pas["NS1.Unit2"].iV;',
    '$mod.i = pas["NS1.Unit2"].iV;',
    '$mod.i = $mod.i;',
    '']) );
end;

procedure TTestModule.TestDottedUnitExpr;
begin
  AddModuleWithIntfImplSrc('NS2.SubNs2.Unit2.pas',
    LinesToStr([
    'procedure DoIt;'
    ]),
    'procedure DoIt; begin end;');

  FFilename:='Ns1.SubNs1.Test1.pp';
  StartProgram(true);
  Add('uses Ns2.sUbnS2.unIt2;');
  Add('implementation');
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  ns2.subns2.unit2.doit;');
  Add('  i:=Ns1.SubNS1.TEst1.i;');
  ConvertProgram;
  CheckSource('TestDottedUnitExpr',
    LinesToStr([
    'this.i = 0;',
    '']),
    LinesToStr([ // this.$init
    'pas["NS2.SubNs2.Unit2"].DoIt();',
    '$mod.i = $mod.i;',
    '']) );
end;

procedure TTestModule.Test_ModeFPCFail;
begin
  StartProgram(false);
  Add('{$mode FPC}');
  Add('begin');
  SetExpectedScannerError('Invalid mode: "FPC"',nErrInvalidMode);
  ConvertProgram;
end;

procedure TTestModule.Test_ModeSwitchCBlocksFail;
begin
  StartProgram(false);
  Add('{$modeswitch cblocks-}');
  Add('begin');
  SetExpectedScannerError('Invalid mode switch: "cblocks-"',nErrInvalidModeSwitch);
  ConvertProgram;
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
  Add('  i4: nativeint = 4503599627370495;');
  Add('  i5: nativeint = -4503599627370496;');
  Add('  i6: nativeint =   $fffffffffffff;');
  Add('  i7: nativeint = -$10000000000000;');
  Add('  u8: nativeuint =  $fffffffffffff;');
  Add('  u9: nativeuint =  $0000000000000;');
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
    'this.i7=-0x10000000000000;',
    'this.u8= 0xfffffffffffff;',
    'this.u9= 0x0000000000000;'
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
    '$mod.i = $mod.i;',
    '$mod.i = ($mod.b ? 1 : 0);',
    '$mod.b = $mod.b;',
    '$mod.b = $mod.i != 0;',
    '$mod.d = $mod.d;',
    '$mod.d = $mod.i;',
    '$mod.s = $mod.s;',
    '$mod.s = $mod.c;',
    '$mod.c = $mod.c;',
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
    '$mod.i = $mod.i;',
    '$mod.i = ($mod.b ? 1 : 0);',
    '$mod.b = $mod.b;',
    '$mod.b = $mod.i != 0;',
    '$mod.d = $mod.d;',
    '$mod.d = $mod.i;',
    '$mod.s = $mod.s;',
    '$mod.s = $mod.c;',
    '$mod.c = $mod.c;',
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
    '$mod.ProcA(3);'
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
    '$mod.i=$mod.FuncA();',
    '$mod.i=$mod.FuncA();',
    '$mod.FuncA();',
    '$mod.FuncA();'
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
    '$mod.ProcA();',
    '$mod.ProcA();'
    ]));
end;

procedure TTestModule.TestIncDec;
begin
  StartProgram(false);
  Add([
  'procedure DoIt(var i: longint);',
  'begin',
  '  inc(i);',
  '  inc(i,2);',
  'end;',
  'var',
  '  Bar: longint;',
  'begin',
  '  inc(bar);',
  '  inc(bar,2);',
  '  dec(bar);',
  '  dec(bar,3);',
  '']);
  ConvertProgram;
  CheckSource('TestIncDec',
    LinesToStr([ // statements
    'this.DoIt = function (i) {',
    '  i.set(i.get()+1);',
    '  i.set(i.get()+2);',
    '};',
    'this.Bar = 0;'
    ]),
    LinesToStr([ // this.$main
    '$mod.Bar+=1;',
    '$mod.Bar+=2;',
    '$mod.Bar-=1;',
    '$mod.Bar-=3;'
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
    '$mod.Bar=3;',
    '$mod.Bar+=4;',
    '$mod.Bar-=5;',
    '$mod.Bar*=6;'
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
    '$mod.vA = 1;',
    '$mod.vB = $mod.vA + $mod.vA;',
    '$mod.vB = Math.floor($mod.vA / $mod.vB);',
    '$mod.vB = $mod.vA % $mod.vB;',
    '$mod.vB = ($mod.vA + ($mod.vA * $mod.vB)) + Math.floor($mod.vA / $mod.vB);',
    '$mod.vC = -$mod.vA;',
    '$mod.vA = $mod.vA - $mod.vB;',
    '$mod.vB = $mod.vA;',
    'if ($mod.vA < $mod.vB){ $mod.vC = $mod.vA } else $mod.vC = $mod.vB;'
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
    '$mod.vA = $mod.vB && $mod.vC;',
    '$mod.vA = $mod.vB || $mod.vC;',
    '$mod.vA = true && $mod.vC;',
    '$mod.vA = ($mod.vB && $mod.vC) || ($mod.vA && $mod.vB);',
    '$mod.vA = !$mod.vB;'
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
    '$mod.vA = $mod.vB & $mod.vC;',
    '$mod.vA = $mod.vB | $mod.vC;',
    '$mod.vA = $mod.vB ^ $mod.vC;',
    '$mod.vA = $mod.vB << $mod.vC;',
    '$mod.vA = $mod.vB >>> $mod.vC;',
    '$mod.vA = 3 & $mod.vC;',
    '$mod.vA = ($mod.vB & $mod.vC) | ($mod.vA & $mod.vB);',
    '$mod.vA = ~$mod.vB;'
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
    'var $impl = $mod.$impl;',
    'this.Proc1 = function () {',
    '  var v1 = 0;',
    '};',
    '']),
    // this.$init
    '',
    // implementation
    LinesToStr([
    '$impl.v2 = "";',
    '']));
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
    'var $impl = $mod.$impl;',
    'this.Proc1 = function () {',
    '};',
    '']),
    LinesToStr([ // this.$init
    '$mod.Proc1();',
    '$impl.Proc2();',
    '']),
    LinesToStr([ // implementation
    '$impl.Proc2 = function () {',
    '};',
    ''])
    );
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
  Add('var vInUnit: longint;');
  Add('function DoIt(pA,pD: longint): longint;');
  Add('var');
  Add('  vB: longint;');
  Add('  vC: longint;');
  Add('  function Nesty(pA: longint): longint; ');
  Add('  var vB: longint;');
  Add('  begin');
  Add('    Result:=pa+vb+vc+pd+vInUnit;');
  Add('  end;');
  Add('begin');
  Add('  Result:=pa+vb+vc;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestNestedProc',
    LinesToStr([ // statements
    'this.vInUnit = 0;',
    'this.DoIt = function (pA, pD) {',
    '  var Result = 0;',
    '  var vB = 0;',
    '  var vC = 0;',
    '  function Nesty(pA) {',
    '    var Result = 0;',
    '    var vB = 0;',
    '    Result = (((pA + vB) + vC) + pD) + $mod.vInUnit;',
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
    '  $mod.FuncA(Bar);',
    '};',
    'this.FuncA = function (Bar) {',
    '  if (Bar == 3);',
    '};'
    ]),
    LinesToStr([
    '$mod.FuncA(4);',
    '$mod.FuncB(5);'
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
    '$mod.FuncA();'
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
    '$mod.i = $mod.Func1();',
    '$mod.i = $mod.Func1() + $mod.Func1();'
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
    'if ($mod.Func2());',
    'if ($mod.i == $mod.Func1());',
    'if ($mod.i == $mod.Func1());'
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
  Add('  exit;');
  Add('  exit(1);');
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
    LinesToStr([
    'return;',
    'return 1;',
    '']));
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
    'for ($mod.i = 1; $mod.i <= $loopend1; $mod.i++) break;',
    'if ($mod.i > $loopend1) $mod.i--;'
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
    'for ($mod.i = 1; $mod.i <= $loopend1; $mod.i++) continue;',
    'if ($mod.i > $loopend1) $mod.i--;'
    ]));
end;

procedure TTestModule.TestProc_External;
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
    '$mod.i = get.item();',
    '$mod.i = apply.something("abc");'
    ]));
end;

procedure TTestModule.TestProc_ExternalOtherUnit;
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
  Add('  doit;');
  Add('  uNit2.doit;');
  ConvertUnit;
  CheckSource('TestProcedureExternalOtherUnit',
    LinesToStr([
    '']),
    LinesToStr([
    'Date.now();',
    'Date.now();',
    'Date.now();',
    'Date.now();',
    'pas.unit2.DoIt();',
    'pas.unit2.DoIt();',
    '']));
end;

procedure TTestModule.TestProc_Asm;
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

procedure TTestModule.TestProc_Assembler;
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

procedure TTestModule.TestProc_VarParam;
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
    '  $mod.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vI.get(), vI.get(), vI);',
    '  $mod.DoIt(vJ, vJ, {',
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
    '$mod.DoIt($mod.i,$mod.i,{',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
end;

procedure TTestModule.TestProc_Overload;
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
    '$mod.DoIt(1);',
    '$mod.DoIt$1(2, 3);',
    '$mod.DoIt$2(4.5);',
    '']));
end;

procedure TTestModule.TestProc_OverloadForward;
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
    '$mod.DoIt(1);',
    '$mod.DoIt$1(2, 3);',
    '']));
end;

procedure TTestModule.TestProc_OverloadUnit;
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
    'var $impl = $mod.$impl;',
    'this.DoIt = function (vI) {',
    '};',
    'this.DoIt$1 = function (vI, vJ) {',
    '};',
    '']),
    LinesToStr([ // this.$init
    '$mod.DoIt(1);',
    '$mod.DoIt$1(2, 3);',
    '$impl.DoIt$3(4,5,6);',
    '$impl.DoIt$4(7,8,9,10);',
    '$impl.DoIt$2(11,12,13,14,15);',
    '']),
    LinesToStr([ // implementation
    '$impl.DoIt$3 = function (vI, vJ, vK) {',
    '};',
    '$impl.DoIt$4 = function (vI, vJ, vK, vL) {',
    '};',
    '$impl.DoIt$2 = function (vI, vJ, vK, vL, vM) {',
    '};',
    '']));
end;

procedure TTestModule.TestProc_OverloadNested;
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
    '  $mod.DoIt(1);',
    '  $mod.DoIt$1(1, 2);',
    '};',
    'this.DoIt = function (vA) {',
    '  function DoIt$3(vA, vB, vC, vD) {',
    '    $mod.DoIt(1);',
    '    $mod.DoIt$1(1, 2);',
    '    DoIt$2(1, 2, 3);',
    '    DoIt$3(1, 2, 3, 4);',
    '  };',
    '  function DoIt$2(vA, vB, vC) {',
    '    function DoIt$5(vA, vB, vC, vD, vE, vF) {',
    '      $mod.DoIt(1);',
    '      $mod.DoIt$1(1, 2);',
    '      DoIt$2(1, 2, 3);',
    '      DoIt$3(1, 2, 3, 4);',
    '      DoIt$4(1, 2, 3, 4, 5);',
    '      DoIt$5(1, 2, 3, 4, 5, 6);',
    '    };',
    '    function DoIt$4(vA, vB, vC, vD, vE) {',
    '      $mod.DoIt(1);',
    '      $mod.DoIt$1(1, 2);',
    '      DoIt$2(1, 2, 3);',
    '      DoIt$3(1, 2, 3, 4);',
    '      DoIt$4(1, 2, 3, 4, 5);',
    '      DoIt$5(1, 2, 3, 4, 5, 6);',
    '    };',
    '    $mod.DoIt(1);',
    '    $mod.DoIt$1(1, 2);',
    '    DoIt$2(1, 2, 3);',
    '    DoIt$3(1, 2, 3, 4);',
    '    DoIt$4(1, 2, 3, 4, 5);',
    '    DoIt$5(1, 2, 3, 4, 5, 6);',
    '  };',
    '  $mod.DoIt(1);',
    '  $mod.DoIt$1(1, 2);',
    '  DoIt$2(1, 2, 3);',
    '  DoIt$3(1, 2, 3, 4);',
    '};',
    '']),
    LinesToStr([
    '$mod.DoIt(1);',
    '$mod.DoIt$1(1, 2);',
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
    'ProcB($mod.GetIt());',
    'ProcB($mod.GetIt());',
    'ProcB($mod.GetIt(), $mod.GetIt());',
    '']));
end;

procedure TTestModule.TestEnum_Name;
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
    'this.f = $mod.TMyEnum.Blue;'
    ]),
    LinesToStr([
    '$mod.e=$mod.TMyEnum.Green;'
    ]));
end;

procedure TTestModule.TestEnum_Number;
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
    '$mod.e=1;'
    ]));
end;

procedure TTestModule.TestEnum_Functions;
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
    '$mod.i=$mod.TMyEnum.Red;',
    '$mod.i=$mod.TMyEnum.Green;',
    '$mod.i=$mod.e;',
    '$mod.e=$mod.TMyEnum.Red;',
    '$mod.e=$mod.TMyEnum.Red;',
    '$mod.e=$mod.TMyEnum.Green;',
    '$mod.e=$mod.TMyEnum.Green;',
    '$mod.e=$mod.TMyEnum.Green-1;',
    '$mod.e=$mod.e-1;',
    '$mod.e=$mod.TMyEnum.Red+1;',
    '$mod.e=$mod.e+1;',
    '$mod.e=1;',
    '$mod.e=$mod.i;',
    '$mod.s = $mod.TMyEnum[$mod.e];',
    '$mod.s = $mod.TMyEnum[$mod.e];',
    '$mod.s = rtl.spaceLeft($mod.TMyEnum[$mod.e], 3);',
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
    '  $mod.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vI.get(), vI.get(), vI);',
    '  $mod.DoIt(vJ, vJ, {',
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
    '$mod.DoIt($mod.i,$mod.i,{',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});'
    ]));
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
    'this.u = rtl.createSet($mod.TColor.Red);'
    ]),
    LinesToStr([
    '$mod.s={};',
    '$mod.s=rtl.createSet($mod.TColor.Green);',
    '$mod.s=rtl.createSet($mod.TColor.Green,$mod.TColor.Blue);',
    '$mod.s=rtl.createSet(null,$mod.TColor.Red,$mod.TColor.Blue);',
    '$mod.s=rtl.createSet($mod.TColor.Red,null,$mod.TColor.Green,$mod.TColor.Blue);',
    '$mod.s=rtl.createSet($mod.TColor.Red,$mod.c);',
    '$mod.s=rtl.refSet($mod.t);',
    '']));
end;

procedure TTestModule.TestSet_Operators;
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
  ConvertProgram;
  CheckSource('TestSet_Operators',
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
    '$mod.vS = rtl.includeSet($mod.vS,$mod.TColor.Green);',
    '$mod.vS = rtl.excludeSet($mod.vS,$mod.vC);',
    '$mod.vS = rtl.unionSet($mod.vT, $mod.vU);',
    '$mod.vS = rtl.unionSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.vS = rtl.unionSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.vS = rtl.unionSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.vS = rtl.diffSet($mod.vT, $mod.vU);',
    '$mod.vS = rtl.diffSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.vS = rtl.diffSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.vS = rtl.diffSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.vS = rtl.intersectSet($mod.vT, $mod.vU);',
    '$mod.vS = rtl.intersectSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.vS = rtl.intersectSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.vS = rtl.intersectSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.vS = rtl.symDiffSet($mod.vT, $mod.vU);',
    '$mod.vS = rtl.symDiffSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.vS = rtl.symDiffSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.vS = rtl.symDiffSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.B = rtl.eqSet($mod.vT, $mod.vU);',
    '$mod.B = rtl.eqSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.B = rtl.eqSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.B = rtl.eqSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.B = rtl.neSet($mod.vT, $mod.vU);',
    '$mod.B = rtl.neSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.B = rtl.neSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.B = rtl.neSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.B = rtl.leSet($mod.vT, $mod.vU);',
    '$mod.B = rtl.leSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.B = rtl.leSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.B = rtl.leSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '$mod.B = rtl.geSet($mod.vT, $mod.vU);',
    '$mod.B = rtl.geSet($mod.vT, rtl.createSet($mod.TColor.Red));',
    '$mod.B = rtl.geSet(rtl.createSet($mod.TColor.Red), $mod.vT);',
    '$mod.B = rtl.geSet(rtl.createSet($mod.TColor.Red), rtl.createSet($mod.TColor.Green));',
    '']));
end;

procedure TTestModule.TestSet_Operator_In;
begin
  StartProgram(false);
  Add('type');
  Add('  TColor = (Red, Green, Blue);');
  Add('  TColors = set of tcolor;');
  Add('var');
  Add('  vC: tcolor;');
  Add('  vT: tcolors;');
  Add('  B: boolean;');
  Add('begin');
  Add('  b:=red in vt;');
  Add('  b:=vc in vt;');
  Add('  b:=green in [red..blue];');
  Add('  b:=vc in [red..blue];');
  Add('  ');
  Add('  if red in vt then ;');
  Add('  while vC in vt do ;');
  Add('  repeat');
  Add('  until vC in vt;');
  ConvertProgram;
  CheckSource('TestSet_Operator_In',
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
    'this.vT = {};',
    'this.B = false;'
    ]),
    LinesToStr([
    '$mod.B = $mod.TColor.Red in $mod.vT;',
    '$mod.B = $mod.vC in $mod.vT;',
    '$mod.B = $mod.TColor.Green in rtl.createSet(null, $mod.TColor.Red, $mod.TColor.Blue);',
    '$mod.B = $mod.vC in rtl.createSet(null, $mod.TColor.Red, $mod.TColor.Blue);',
    'if ($mod.TColor.Red in $mod.vT) ;',
    'while ($mod.vC in $mod.vT) {',
    '};',
    'do {',
    '} while (!($mod.vC in $mod.vT));',
    '']));
end;

procedure TTestModule.TestSet_Functions;
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
    '$mod.e=$mod.TMyEnum.Red;',
    '$mod.e=$mod.TMyEnum.Red;',
    '$mod.e=$mod.TMyEnum.Green;',
    '$mod.e=$mod.TMyEnum.Green;',
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
    '$mod.DoDefault(rtl.refSet($mod.aSet));',
    '$mod.DoConst($mod.aSet);',
    '']));
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
    '  $mod.DoIt(rtl.refSet(vG), vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  $mod.DoIt(rtl.refSet(vH), vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  $mod.DoIt(rtl.refSet(vI.get()), vI.get(), vI);',
    '  $mod.DoIt(rtl.refSet(vJ), vJ, {',
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
    '$mod.DoIt(rtl.refSet($mod.i),$mod.i,{',
    '  p: $mod,',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    '$mod.Obj.SetColors(rtl.includeSet($mod.Obj.GetColors(), $mod.TEnum.Red));',
    '$mod.Obj.SetColors(rtl.excludeSet($mod.Obj.GetColors(), $mod.TEnum.Red));',
    '']));
end;

procedure TTestModule.TestSet_EnumConst;
begin
  StartProgram(false);
  Add('type');
  Add('  TEnum = (Red,Blue);');
  Add('  TEnums = set of TEnum;');
  Add('const');
  Add('  Orange = red;');
  Add('var');
  Add('  Enum: tenum;');
  Add('  Enums: tenums;');
  Add('begin');
  Add('  Include(enums,orange);');
  Add('  Exclude(enums,orange);');
  Add('  if orange in enums then;');
  Add('  if orange in [orange,red] then;');
  ConvertProgram;
  CheckSource('TestEnumConst',
    LinesToStr([ // statements
    'this.TEnum = {',
    '  "0": "Red",',
    '  Red: 0,',
    '  "1": "Blue",',
    '  Blue: 1',
    '};',
    'this.Orange = $mod.TEnum.Red;',
    'this.Enum = 0;',
    'this.Enums = {};',
    '']),
    LinesToStr([
    '$mod.Enums = rtl.includeSet($mod.Enums, $mod.Orange);',
    '$mod.Enums = rtl.excludeSet($mod.Enums, $mod.Orange);',
    'if ($mod.Orange in $mod.Enums) ;',
    'if ($mod.Orange in rtl.createSet($mod.Orange, $mod.TEnum.Red)) ;',
    '']));
end;

procedure TTestModule.TestSet_AnonymousEnumType;
begin
  StartProgram(false);
  Add('type');
  Add('  TFlags = set of (red, green);');
  Add('const');
  Add('  favorite = red;');
  Add('var');
  Add('  f: TFlags;');
  Add('  i: longint;');
  Add('begin');
  Add('  Include(f,red);');
  Add('  Include(f,favorite);');
  Add('  i:=ord(red);');
  Add('  i:=ord(favorite);');
  Add('  i:=ord(low(TFlags));');
  Add('  i:=ord(low(f));');
  Add('  i:=ord(low(favorite));');
  Add('  i:=ord(high(TFlags));');
  Add('  i:=ord(high(f));');
  Add('  i:=ord(high(favorite));');
  Add('  f:=[green,favorite];');
  ConvertProgram;
  CheckSource('TestSet_AnonymousEnumType',
    LinesToStr([ // statements
    'this.TFlags$a = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "green",',
    '  green: 1',
    '};',
    'this.favorite = $mod.TFlags$a.red;',
    'this.f = {};',
    'this.i = 0;',
    '']),
    LinesToStr([
    '$mod.f = rtl.includeSet($mod.f, $mod.TFlags$a.red);',
    '$mod.f = rtl.includeSet($mod.f, $mod.favorite);',
    '$mod.i = $mod.TFlags$a.red;',
    '$mod.i = $mod.favorite;',
    '$mod.i = $mod.TFlags$a.red;',
    '$mod.i = $mod.TFlags$a.red;',
    '$mod.i = $mod.TFlags$a.red;',
    '$mod.i = $mod.TFlags$a.green;',
    '$mod.i = $mod.TFlags$a.green;',
    '$mod.i = $mod.TFlags$a.green;',
    '$mod.f = rtl.createSet($mod.TFlags$a.green, $mod.favorite);',
    '']));
end;

procedure TTestModule.TestSet_CharFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TChars = set of char;');
  Add('begin');
  SetExpectedPasResolverError('Not supported: set of Char',nNotSupportedX);
  ConvertProgram;
end;

procedure TTestModule.TestSet_BooleanFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TBools = set of boolean;');
  Add('begin');
  SetExpectedPasResolverError('Not supported: set of Boolean',nNotSupportedX);
  ConvertProgram;
end;

procedure TTestModule.TestSet_ConstEnum;
begin
  StartProgram(false);
  Add([
  'type',
  '  TEnum = (red,blue,green);',
  '  TEnums = set of TEnum;',
  'const',
  '  teAny = [low(TEnum)..high(TEnum)];',
  '  teRedBlue = [low(TEnum)..pred(high(TEnum))];',
  'var',
  '  e: TEnum;',
  '  s: TEnums;',
  'begin',
  '  if blue in teAny then;',
  '  if blue in teAny+[e] then;',
  '  if blue in teAny+teRedBlue then;',
  '  s:=teAny;',
  '  s:=teAny+[e];',
  '  s:=[e]+teAny;',
  '  s:=teAny+teRedBlue;',
  '  s:=teAny+teRedBlue+[e];',
  '']);
  ConvertProgram;
  CheckSource('TestSet_ConstEnum',
    LinesToStr([ // statements
    'this.TEnum = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "blue",',
    '  blue: 1,',
    '  "2": "green",',
    '  green: 2',
    '};',
    'this.teAny = rtl.createSet(null, $mod.TEnum.red, $mod.TEnum.green);',
    'this.teRedBlue = rtl.createSet(null, $mod.TEnum.red, $mod.TEnum.green - 1);',
    'this.e = 0;',
    'this.s = {};',
    '']),
    LinesToStr([
    'if ($mod.TEnum.blue in $mod.teAny) ;',
    'if ($mod.TEnum.blue in rtl.unionSet($mod.teAny, rtl.createSet($mod.e))) ;',
    'if ($mod.TEnum.blue in rtl.unionSet($mod.teAny, $mod.teRedBlue)) ;',
    '$mod.s = rtl.refSet($mod.teAny);',
    '$mod.s = rtl.unionSet($mod.teAny, rtl.createSet($mod.e));',
    '$mod.s = rtl.unionSet(rtl.createSet($mod.e), $mod.teAny);',
    '$mod.s = rtl.unionSet($mod.teAny, $mod.teRedBlue);',
    '$mod.s = rtl.unionSet(rtl.unionSet($mod.teAny, $mod.teRedBlue), rtl.createSet($mod.e));',
    '']));
end;

procedure TTestModule.TestSet_ConstChar;
begin
  StartProgram(false);
  Add('const');
  Add('  LowChars = [''a''..''z''];');
  Add('  Chars = LowChars+[''A''..''Z''];');
  Add('var');
  Add('  c: char;');
  Add('  s: string;');
  Add('begin');
  Add('  if c in lowchars then ;');
  Add('  if ''a'' in lowchars then ;');
  Add('  if s[1] in lowchars then ;');
  Add('  if c in chars then ;');
  Add('  if c in [''a''..''z'',''_''] then ;');
  Add('  if ''b'' in [''a''..''z'',''_''] then ;');
  ConvertProgram;
  CheckSource('TestSet_ConstChar',
    LinesToStr([ // statements
    'this.LowChars = rtl.createSet(null, 97, 122);',
    'this.Chars = rtl.unionSet($mod.LowChars, rtl.createSet(null, 65, 90));',
    'this.c = "";',
    'this.s = "";',
    '']),
    LinesToStr([
    'if ($mod.c.charCodeAt() in $mod.LowChars) ;',
    'if (97 in $mod.LowChars) ;',
    'if ($mod.s.charCodeAt(1 - 1) in $mod.LowChars) ;',
    'if ($mod.c.charCodeAt() in $mod.Chars) ;',
    'if ($mod.c.charCodeAt() in rtl.createSet(null, 97, 122, 95)) ;',
    'if (98 in rtl.createSet(null, 97, 122, 95)) ;',
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
    'var $impl = $mod.$impl;',
    '']),
    '', // this.$init
    LinesToStr([ // implementation
    '$impl.V1 = 0;',
    '$impl.V2 = 3;',
    '$impl.V3 = "abc";',
    '']) );
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
    'var $impl = $mod.$impl;',
    '']),
    '', // this.$init
    LinesToStr([ // implementation
    '$impl.v1 = 3;',
    '$impl.v2 = 4;',
    '$impl.v3 = "abc";',
    '']) );
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
    'var $impl = $mod.$impl;',
    '']),
    // this.$init
    '$impl.aRec.i = 3;',
    LinesToStr([ // implementation
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
    '$impl.aRec = new $impl.TMyRecord();',
    '']) );
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
    '$mod.d = Global.NaN;'
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
  Add('  i: longint; external name ''$i'';');
  Add('begin');
  Add('  d:=nan;');
  Add('  d:=uNit2.nan;');
  Add('  d:=test1.d;');
  Add('  i:=iv;');
  Add('  i:=uNit2.iv;');
  Add('  i:=test1.i;');
  ConvertUnit;
  CheckSource('TestVarExternalOtherUnit',
    LinesToStr([
    'var $impl = $mod.$impl;',
    '']),
    LinesToStr([ // this.$init
    '$impl.d = Global.NaN;',
    '$impl.d = Global.NaN;',
    '$impl.d = $impl.d;',
    '$i = pas.unit2.iV;',
    '$i = pas.unit2.iV;',
    '$i = $i;',
    '']),
    LinesToStr([ // implementation
    '$impl.d = 0.0;',
    '']) );
end;

procedure TTestModule.TestDouble;
begin
  StartProgram(false);
  Add([
  'var',
  '  d: double;',
  'begin',
  '  d:=1.0;',
  '  d:=1.0/3.0;',
  '  d:=1/3;',
  '  d:=5.0E-324;',
  '  d:=1.7E308;',
  '  d:=10**3;',
  '  d:=10 mod 3;',
  '  d:=10 div 3;',
  '']);
  ConvertProgram;
  CheckSource('TestDouble',
    LinesToStr([
    'this.d=0.0;'
    ]),
    LinesToStr([
    '$mod.d = 1.0;',
    '$mod.d = 1.0 / 3.0;',
    '$mod.d = 1 / 3;',
    '$mod.d = 5.0E-324;',
    '$mod.d = 1.7E308;',
    '$mod.d = Math.pow(10, 3);',
    '$mod.d = 10 % 3;',
    '$mod.d = Math.floor(10 / 3);',
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
    '$mod.c="\x00";',
    '$mod.c="\x01";',
    '$mod.c="\t";',
    '$mod.c="\n";',
    '$mod.c="\r";',
    '$mod.c="\x1F";',
    '$mod.c=" ";',
    '$mod.c="\n";',
    '$mod.c="\n";',
    '$mod.c="\x0B";',
    '$mod.c="\x0B";',
    '$mod.c="\x01";',
    '$mod.c=''"'';'
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
    '$mod.b = $mod.c == "1";',
    '$mod.b = "2" == $mod.c;',
    '$mod.b = "3" == "4";',
    '$mod.b = $mod.c != "5";',
    '$mod.b = "6" != $mod.c;',
    '$mod.b = $mod.c > "7";',
    '$mod.b = "8" > $mod.c;',
    '$mod.b = $mod.c >= "9";',
    '$mod.b = "A" >= $mod.c;',
    '$mod.b = $mod.c < "B";',
    '$mod.b = "C" < $mod.c;',
    '$mod.b = $mod.c <= "D";',
    '$mod.b = "E" <= $mod.c;',
    '']));
end;

procedure TTestModule.TestChar_Ord;
begin
  StartProgram(false);
  Add('var');
  Add('  c: char;');
  Add('  i: longint;');
  Add('  s: string;');
  Add('begin');
  Add('  i:=ord(c);');
  Add('  i:=ord(s[i]);');
  ConvertProgram;
  CheckSource('TestChar_Ord',
    LinesToStr([
    'this.c = "";',
    'this.i = 0;',
    'this.s = "";'
    ]),
    LinesToStr([
    '$mod.i = $mod.c.charCodeAt();',
    '$mod.i = $mod.s.charCodeAt($mod.i-1);',
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
    '$mod.c = String.fromCharCode($mod.i);',
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
    '$mod.s="";',
    '$mod.s="\r\n";',
    '$mod.s="\tfoo";',
    '$mod.s="©";',
    '$mod.s="foo\rbar";',
    '$mod.s=''"'';',
    '$mod.s=''"\''"'';'
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
    '$mod.i = $mod.s.length;',
    '$mod.i = ($mod.s+$mod.s).length;',
    '$mod.i = "abc".length;',
    '$mod.i = $mod.c.length;',
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
    '$mod.b = $mod.s == $mod.t;',
    '$mod.b = $mod.s != $mod.t;',
    '$mod.b = $mod.s > $mod.t;',
    '$mod.b = $mod.s >= $mod.t;',
    '$mod.b = $mod.s < $mod.t;',
    '$mod.b = $mod.s <= $mod.t;',
    '']));
end;

procedure TTestModule.TestString_SetLength;
begin
  StartProgram(false);
  Add([
  'procedure DoIt(var s: string);',
  'begin',
  '  SetLength(s,2);',
  'end;',
  'var s: string;',
  'begin',
  '  SetLength(s,3);',
  '']);
  ConvertProgram;
  CheckSource('TestString_SetLength',
    LinesToStr([ // statements
    'this.DoIt = function (s) {',
    '  s.set(rtl.strSetLength(s.get(), 2));',
    '};',
    'this.s = "";',
    '']),
    LinesToStr([ // this.$main
    '$mod.s = rtl.strSetLength($mod.s, 3);'
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
    '$mod.b = $mod.s.charAt(1-1) == $mod.c;',
    '$mod.b = $mod.c == $mod.s.charAt(1 - 1);',
    '$mod.b = $mod.c != $mod.s.charAt(1 - 1);',
    '$mod.b = $mod.c > $mod.s.charAt(1 - 1);',
    '$mod.b = $mod.c >= $mod.s.charAt(1 - 1);',
    '$mod.b = $mod.c < $mod.s.charAt(1 - 1);',
    '$mod.b = $mod.c <= $mod.s.charAt(1 - 1);',
    '$mod.s = rtl.setCharAt($mod.s, 1, $mod.c);',
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
    '$mod.s = ""+$mod.b;',
    '$mod.s = ""+$mod.i;',
    '$mod.s = ""+$mod.d;',
    '$mod.s = rtl.spaceLeft(""+$mod.i,3);',
    '$mod.s = rtl.spaceLeft($mod.d.toFixed(2),3);',
    '$mod.s = ""+$mod.b;',
    '$mod.s = ""+$mod.i;',
    '$mod.s = ""+$mod.d;',
    '$mod.s = (""+$mod.i)+$mod.i;',
    '$mod.s = rtl.spaceLeft(""+$mod.i,3);',
    '$mod.s = rtl.spaceLeft($mod.d.toFixed(2),3);',
    '$mod.s = rtl.spaceLeft("" + $mod.i, 4) + $mod.i;',
    '$mod.s = ("" + $mod.i) + rtl.spaceLeft("" + $mod.i, 5);',
    '$mod.s = rtl.spaceLeft("" + $mod.i, 4) + rtl.spaceLeft("" + $mod.i, 5);',
    '$mod.s = $mod.s + $mod.s;',
    '$mod.s = $mod.s + "foo";',
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

procedure TTestModule.TestTypeShortstring_Fail;
begin
  StartProgram(false);
  Add('type t = string[12];');
  Add('var s: t;');
  Add('begin');
  SetExpectedPasResolverError('illegal qualifier "["',nIllegalQualifier);
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
    '  $mod.p1(1);',
    '  $mod.p1(1);',
    '  $mod.p1(11);',
    '  $mod.p2(1,"a");',
    '  $mod.p2(1,"a");',
    '  $mod.p2(12,"a");',
    '  $mod.p2(13,"b");',
    '  $mod.p3(1.0,false,"abc");'
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
    '  $mod.vJ = 0;',
    '  $mod.vN = 3;',
    '  var $loopend1 = $mod.vN;',
    '  for ($mod.vI = 1; $mod.vI <= $loopend1; $mod.vI++) {',
    '    $mod.vJ = $mod.vJ + $mod.vI;',
    '  };',
    '  if ($mod.vI > $loopend1) $mod.vI--;'
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
    LinesToStr([ // $mod.$main
    '  $mod.SumNumbers(3);'
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
    '  for ($mod.vI = 1; $mod.vI <= $loopend1; $mod.vI++);',
    '  if($mod.vI>$loopend1)$mod.vI--;',
    '  if ($mod.vI==3) ;'
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
    LinesToStr([ // $mod.$main
    '  $mod.SumNumbers(3);'
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
    LinesToStr([ // $mod.$main
    '  $mod.vN = 3;',
    '  $mod.vJ = 0;',
    '  $mod.vI = 0;',
    '  do{',
    '    $mod.vI = $mod.vI + 1;',
    '    $mod.vJ = $mod.vJ + $mod.vI;',
    '  }while(!($mod.vI>=$mod.vN));'
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
    LinesToStr([ // $mod.$main
    '$mod.vI = 1;',
    'if (vI==1) {',
    '  vI=2;',
    '}',
    'if (vI==2){ vI=3; }',
    ';',
    '$mod.vI = 4;'
    ]));
end;

procedure TTestModule.TestAsmPas_Impl;
begin
  StartUnit(false);
  Add('interface');
  Add('const cIntf: longint = 1;');
  Add('var vIntf: longint;');
  Add('implementation');
  Add('const cImpl: longint = 2;');
  Add('var vImpl: longint;');
  Add('procedure DoIt;');
  Add('const cLoc: longint = 3;');
  Add('var vLoc: longint;');
  Add('begin;');
  Add('  asm');
  //Add('    pas(vIntf)=pas(cIntf);');
  //Add('    pas(vImpl)=pas(cImpl);');
  //Add('    pas(vLoc)=pas(cLoc);');
  Add('  end;');
  Add('end;');
  ConvertUnit;
  // ToDo: check use analyzer
  CheckSource('TestAsmPas_Impl',
    LinesToStr([
    'var $impl = $mod.$impl;',
    'this.cIntf = 1;',
    'this.vIntf = 0;',
    '']),
    '', // this.$init
    LinesToStr([ // implementation
    'var cLoc = 3;',
    '$impl.cImpl = 2;',
    '$impl.vImpl = 0;',
    '$impl.DoIt = function () {',
    '  var vLoc = 0;',
    '};',
    '']) );
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
    LinesToStr([ // $mod.$main
    'try {',
    '  $mod.i = 0;',
    '  $mod.i = Math.floor(2 / $mod.i);',
    '} finally {',
    '  $mod.i = 3;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "Exception", $mod.TObject, function () {',
    '  this.$init = function () {',
    '    $mod.TObject.$init.call(this);',
    '    this.Msg = "";',
    '  };',
    '});',
    'rtl.createClass($mod, "EInvalidCast", $mod.Exception, function () {',
    '});',
    'this.vI = 0;'
    ]),
    LinesToStr([ // $mod.$main
    'try {',
    '  $mod.vI = 1;',
    '} catch ($e) {',
    '  $mod.vI = 2;',
    '};',
    'try {',
    '  $mod.vI = 3;',
    '} catch ($e) {',
    '  throw $e;',
    '};',
    'try {',
    '  $mod.vI = 4;',
    '} catch ($e) {',
    '  if ($mod.EInvalidCast.isPrototypeOf($e)){',
    '    throw $e',
    '  } else if ($mod.Exception.isPrototypeOf($e)) {',
    '    var E = $e;',
    '    if (E.Msg == "") throw E;',
    '  } else {',
    '    $mod.vI = 5;',
    '  }',
    '};',
    'try {',
    '  $mod.vI = 6;',
    '} catch ($e) {',
    '  if ($mod.EInvalidCast.isPrototypeOf($e)){' ,
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
    LinesToStr([ // $mod.$main
    'var $tmp1 = $mod.vI;',
    'if ($tmp1 == 1) {} else if ($tmp1 == 2){ $mod.vI = 3 }else {',
    '  $mod.vI = 4;',
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
    LinesToStr([ // $mod.$main
    'switch ($mod.Vi) {',
    'case 1:',
    '  break;',
    'case 2:',
    '  $mod.Vi = 3;',
    '  break;',
    'default:',
    '  $mod.Vi = 4;',
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
    LinesToStr([ // $mod.$main
    'var $tmp1 = $mod.Vi;',
    'if ($tmp1 == 1) {',
    '  $mod.Vi = 2;',
    '  $mod.Vi = 3;',
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
    LinesToStr([ // $mod.$main
    'switch ($mod.vI) {',
    'case 1:',
    '  $mod.vI = 2;',
    '  $mod.vI = 3;',
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
    LinesToStr([ // $mod.$main
    'var $tmp1 = $mod.vI;',
    'if (($tmp1 >= 1) && ($tmp1 <= 3)){',
    '  $mod.vI = 14',
    '} else if (($tmp1 == 4) || ($tmp1 == 5)){',
    '  $mod.vI = 16',
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
    LinesToStr([ // $mod.$main
    '$mod.Arr = rtl.arraySetLength($mod.Arr,3,0);',
    '$mod.Arr[0] = 4;',
    '$mod.Arr[1] = rtl.length($mod.Arr) + $mod.Arr[0];',
    '$mod.Arr[$mod.i] = 5;',
    '$mod.Arr[$mod.Arr[$mod.i]] = $mod.Arr[6];',
    '$mod.i = 0;',
    '$mod.i = rtl.length($mod.Arr) - 1;',
    '$mod.b = rtl.length($mod.Arr) > 0;',
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
    LinesToStr([ // $mod.$main
    '$mod.Arr = [];',
    'if (rtl.length($mod.Arr) == 0) ;',
    'if (rtl.length($mod.Arr) == 0) ;',
    'if (rtl.length($mod.Arr) > 0) ;',
    'if (rtl.length($mod.Arr) > 0) ;',
    '$mod.DoIt([],[]);',
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
    LinesToStr([ // $mod.$main
    '$mod.Arr2 = [];',
    'if (rtl.length($mod.Arr2) == 0) ;',
    'if (rtl.length($mod.Arr2) == 0) ;',
    '$mod.i = 0;',
    '$mod.i = 0;',
    '$mod.i = rtl.length($mod.Arr2) - 1;',
    '$mod.i = rtl.length($mod.Arr2[2]) - 1;',
    '$mod.Arr2[3] = $mod.Arr;',
    '$mod.Arr2[4][5] = $mod.i;',
    '$mod.i = $mod.Arr2[6][7];',
    '$mod.Arr2[8][9] = $mod.i;',
    '$mod.i = $mod.Arr2[10][11];',
    '$mod.Arr2 = rtl.arraySetLength($mod.Arr2, 14, []);',
    '$mod.Arr2[15] = rtl.arraySetLength($mod.Arr2[15], 16, 0);',
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
    'this.r = new $mod.TRec();',
    'this.i = 0;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.Arr = rtl.arraySetLength($mod.Arr,3, $mod.TRec);',
    '$mod.Arr[0].Int = 4;',
    '$mod.Arr[1].Int = rtl.length($mod.Arr)+$mod.Arr[2].Int;',
    '$mod.Arr[$mod.Arr[$mod.i].Int].Int = $mod.Arr[5].Int;',
    '$mod.Arr[7] = new $mod.TRec($mod.r);',
    '$mod.r = new $mod.TRec($mod.Arr[8]);',
    '$mod.i = 0;',
    '$mod.i = rtl.length($mod.Arr)-1;',
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
    '  $mod.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vI.get(), vI.get(), vI);',
    '  $mod.DoIt(vJ, vJ, {',
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
    '$mod.DoIt($mod.i,$mod.i,{',
    '  p: $mod,',
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
    '  $mod.DoIt(vI.get(), vI.get(), vI);',
    '  $mod.DoIt(vJ[1+1], vJ[1+2], {',
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
    '$mod.DoIt($mod.a[1+4],$mod.a[1+5],{',
    '  a: 1+6,',
    '  p: $mod.a,',
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
    '$mod.DoIt($mod.GetArr(0)[1+1],$mod.GetArr(0)[1+2],{',
    '  a: 1+3,',
    '  p: $mod.GetArr(0),',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    '$mod.DoIt($mod.GetArr(0)[2+1],$mod.GetArr(0)[2+2],{',
    '  a: 2+3,',
    '  p: $mod.GetArr(0),',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    '$mod.DoIt($mod.GetArr(7)[3+1],$mod.GetArr(8)[3+2],{',
    '  a: 3+3,',
    '  p: $mod.GetArr(9),',
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
    LinesToStr([ // $mod.$main
    '$mod.e = $mod.TEnum.red;',
    '$mod.e = $mod.TEnum.blue;',
    '$mod.i = $mod.a[$mod.TEnum.red]+2;',
    '$mod.a[$mod.e] = $mod.a[$mod.e];',
    '']));
end;

procedure TTestModule.TestArray_SetLengthOutArg;
begin
  StartProgram(false);
  Add([
  'type TArrInt = array of longint;',
  'procedure DoIt(out a: TArrInt);',
  'begin',
  '  SetLength(a,2);',
  'end;',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestArray_SetLengthOutArg',
    LinesToStr([ // statements
    'this.DoIt = function (a) {',
    '  a.set(rtl.arraySetLength(a.get(), 2, 0));',
    '};',
    '']),
    LinesToStr([
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.Obj = null;',
    '']),
    LinesToStr([
    '$mod.Obj.SetColors(rtl.arraySetLength($mod.Obj.GetColors(), 2, 0));',
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
    '$mod.DoIt([]);',
    '$mod.DoIt([$mod.s, "foo", "", $mod.s + $mod.s]);',
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
    LinesToStr([ // $mod.$main
    '$mod.ArrInt = $mod.ArrInt;',
    '$mod.ArrInt = $mod.ArrInt.concat($mod.ArrInt);',
    '$mod.ArrInt = $mod.ArrInt.concat($mod.ArrInt,$mod.ArrInt);',
    '$mod.ArrRec = $mod.ArrRec;',
    '$mod.ArrRec = rtl.arrayConcat($mod.TRec, $mod.ArrRec);',
    '$mod.ArrRec = rtl.arrayConcat($mod.TRec, $mod.ArrRec, $mod.ArrRec);',
    '$mod.ArrSet = $mod.ArrSet;',
    '$mod.ArrSet = rtl.arrayConcat("refSet", $mod.ArrSet);',
    '$mod.ArrSet = rtl.arrayConcat("refSet", $mod.ArrSet, $mod.ArrSet);',
    '$mod.ArrJSValue = $mod.ArrJSValue;',
    '$mod.ArrJSValue = $mod.ArrJSValue.concat($mod.ArrJSValue);',
    '$mod.ArrJSValue = $mod.ArrJSValue.concat($mod.ArrJSValue, $mod.ArrJSValue);',
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
    LinesToStr([ // $mod.$main
    '$mod.ArrInt = rtl.arrayCopy(0, $mod.ArrInt, 0);',
    '$mod.ArrInt = rtl.arrayCopy(0, $mod.ArrInt, 2);',
    '$mod.ArrInt = rtl.arrayCopy(0, $mod.ArrInt, 3, 4);',
    '$mod.ArrRec = rtl.arrayCopy($mod.TRec, $mod.ArrRec, 0);',
    '$mod.ArrRec = rtl.arrayCopy($mod.TRec, $mod.ArrRec, 5);',
    '$mod.ArrRec = rtl.arrayCopy($mod.TRec, $mod.ArrRec, 6, 7);',
    '$mod.ArrSet = rtl.arrayCopy("refSet", $mod.ArrSet, 0);',
    '$mod.ArrSet = rtl.arrayCopy("refSet", $mod.ArrSet, 8);',
    '$mod.ArrSet = rtl.arrayCopy("refSet", $mod.ArrSet, 9, 10);',
    '$mod.ArrJSValue = rtl.arrayCopy(0, $mod.ArrJSValue, 0);',
    '$mod.ArrJSValue = rtl.arrayCopy(0, $mod.ArrJSValue, 11);',
    '$mod.ArrJSValue = rtl.arrayCopy(0, $mod.ArrJSValue, 12, 13);',
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
    LinesToStr([ // $mod.$main
    '$mod.ArrInt.splice(2, 1, 1);',
    '$mod.ArrInt.splice(4, 1, $mod.ArrInt[3]);',
    '$mod.ArrRec.splice(6, 1, $mod.ArrRec[5]);',
    '$mod.ArrSet.splice(7, 1, $mod.ArrSet[7]);',
    '$mod.ArrJSValue.splice(9, 1, $mod.ArrJSValue[8]);',
    '$mod.ArrJSValue.splice(11, 1, 10);',
    '$mod.ArrInt.splice(12, 13);',
    '$mod.ArrRec.splice(14, 15);',
    '$mod.ArrSet.splice(17, 18);',
    '$mod.ArrJSValue.splice(19, 10);',
    '']));
end;

procedure TTestModule.TestArray_DynArrayConst;
begin
  StartProgram(false);
  Add([
  'type',
  '  integer = longint;',
  '  TArrInt = array of integer;',
  '  TArrStr = array of string;',
  'const',
  '  Ints: TArrInt = (1,2,3);',
  '  Names: array of string = (''a'',''foo'');',
  '  Aliases: TarrStr = (''foo'',''b'');',
  '  OneInt: TArrInt = (7);',
  '  OneStr: array of integer = (7);',
  //'  Chars: array of char = ''aoc'';',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestArray_DynArrayConst',
    LinesToStr([ // statements
    'this.Ints = [1, 2, 3];',
    'this.Names = ["a", "foo"];',
    'this.Aliases = ["foo", "b"];',
    'this.OneInt = [7];',
    'this.OneStr = [7];',
    '']),
    LinesToStr([ // $mod.$main
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
    LinesToStr([ // $mod.$main
    'if (Array.isArray(65)) ;',
    '$mod.aObj = $mod.a.concat($mod.a);',
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
    LinesToStr([ // $mod.$main
    '$mod.a = $mod.aObj;',
    '$mod.aObj[1] = $mod.aObj[2];',
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
    'this.Rec = new $mod.TRecA();'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.Rec.Bold = 123;'
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
    'this.r = new $mod.TRec();'
    ]),
    LinesToStr([ // $mod.$main
    'var $with1 = $mod.r;',
    '$mod.Int = $with1.vI;',
    'var $with2 = $mod.r;',
    '$mod.Int = $with2.vI;',
    '$with2.vI = $mod.Int;'
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
    '    this.Small = new $mod.TSmallRec(s.Small);',
    '    this.Enums = rtl.refSet(s.Enums);',
    '  } else {',
    '    this.Int = 0;',
    '    this.D = 0.0;',
    '    this.Arr = [];',
    '    this.Small = new $mod.TSmallRec();',
    '    this.Enums = {};',
    '  };',
    '  this.$equal = function (b) {',
    '    return (this.Int == b.Int) && ((this.D == b.D) && ((this.Arr == b.Arr)',
    ' && (this.Small.$equal(b.Small) && rtl.eqSet(this.Enums, b.Enums))));',
    '  };',
    '};',
    'this.r = new $mod.TBigRec();',
    'this.s = new $mod.TBigRec();'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.r = new $mod.TBigRec($mod.s);',
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
    'this.Rec = new $mod.TRecA();'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.DoDefault(new $mod.TRecA($mod.Rec));',
    '$mod.DoConst($mod.Rec);',
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
    '  var vJ = new $mod.TRecord();',
    '  vG = new $mod.TRecord(vG);',
    '  vJ = new $mod.TRecord(vH);',
    '  vI.set(new $mod.TRecord(vI.get()));',
    '  $mod.DoIt(new $mod.TRecord(vG), vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  $mod.DoIt(new $mod.TRecord(vH), vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  $mod.DoIt(new $mod.TRecord(vI.get()), vI.get(), vI);',
    '  $mod.DoIt(new $mod.TRecord(vJ), vJ, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '};',
    'this.i = new $mod.TRecord();'
    ]),
    LinesToStr([
    '$mod.DoIt(new $mod.TRecord($mod.i),$mod.i,{',
    '  p: $mod,',
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
    '  var vJ = new $mod.TRecord();',
    '  $mod.DoIt(vJ.i, vJ.i, {',
    '    p: vJ,',
    '    get: function () {',
    '      return this.p.i;',
    '    },',
    '    set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '  });',
    '};',
    'this.r = new $mod.TRecord();'
    ]),
    LinesToStr([
    '$mod.DoIt($mod.r.i,$mod.r.i,{',
    '  p: $mod.r,',
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
  Add('procedure DoIt(vG: integer; const vH: integer);');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  doit(getrec.i,getrec.i);');
  Add('  doit(getrec().i,getrec().i);');
  Add('  doit(getrec(1).i,getrec(2).i);');
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
    '  var Result = new $mod.TRecord();',
    '  return Result;',
    '};',
    'this.DoIt = function (vG,vH) {',
    '};'
    ]),
    LinesToStr([
    '$mod.DoIt($mod.GetRec(0).i,$mod.GetRec(0).i);',
    '$mod.DoIt($mod.GetRec(0).i,$mod.GetRec(0).i);',
    '$mod.DoIt($mod.GetRec(1).i,$mod.GetRec(2).i);',
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
    'this.r = new $mod.TRecord();'
    ]),
    LinesToStr([
    'var $with1 = $mod.r;',
    '$mod.DoIt($with1.i,$with1.i,{',
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
    '    this.r = new $mod.TRecord(s.r);',
    '  } else {',
    '    this.r = new $mod.TRecord();',
    '  };',
    '  this.$equal = function (b) {',
    '    return this.r.$equal(b.r);',
    '  };',
    '};',
    'this.b = false;',
    'this.r = new $mod.TRecord();',
    'this.s = new $mod.TRecord();'
    ]),
    LinesToStr([
    '$mod.b = $mod.r.$equal($mod.s);',
    '$mod.b = !$mod.r.$equal($mod.s);',
    '']));
end;

procedure TTestModule.TestRecord_TypeCastJSValueToRecord;
begin
  StartProgram(false);
  Add('type');
  Add('  TRecord = record');
  Add('    i: longint;');
  Add('  end;');
  Add('var');
  Add('  Jv: jsvalue;');
  Add('  Rec: trecord;');
  Add('begin');
  Add('  rec:=trecord(jv);');
  ConvertProgram;
  CheckSource('TestRecord_TypeCastJSValueToRecord',
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
    'this.Jv = undefined;',
    'this.Rec = new $mod.TRecord();'
    ]),
    LinesToStr([
    '$mod.Rec = new $mod.TRecord(rtl.getObject($mod.Jv));',
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.TObject.$create("Create");',
    '$mod.Obj.$destroy("Destroy");',
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
    'rtl.createClass($mod,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function(Par){',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.TObject.$create("Create",[3]);'
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.TObject.$create("Create",[4]);',
    '$mod.Obj.vI = $mod.Obj.vI + 5;'
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.TObject.$create("Create");',
    '$mod.Obj.GetIt(4);',
    '$mod.Obj.Sub.Sub=null;',
    '$mod.Obj.Sub.GetIt(5);',
    '$mod.Obj.Sub.GetIt(6).Sub=null;',
    '$mod.Obj.Sub.GetIt(7).GetIt(8);',
    '$mod.Obj.Sub.GetIt(9).Sub.GetIt(10);'
    ]));
end;

procedure TTestModule.TestClass_Implementation;
begin
  StartUnit(false);
  Add([
  'interface',
  'type',
  '  TObject = class',
  '    constructor Create;',
  '  end;',
  'implementation',
  'type',
  '  TIntClass = class',
  '    constructor Create; reintroduce;',
  '    class procedure DoGlob;',
  '  end;',
  'constructor tintclass.create;',
  'begin',
  '  inherited;',
  '  inherited create;',
  '  doglob;',
  'end;',
  'class procedure tintclass.doglob;',
  'begin',
  'end;',
  'constructor tobject.create;',
  'var',
  '  iC: tintclass;',
  'begin',
  '  ic:=tintclass.create;',
  '  tintclass.doglob;',
  '  ic.doglob;',
  'end;',
  'initialization',
  '  tintclass.doglob;',
  '']);
  ConvertUnit;
  CheckSource('TestClass_Implementation',
    LinesToStr([ // statements
    'var $impl = $mod.$impl;',
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '    var iC = null;',
    '    iC = $impl.TIntClass.$create("Create$1");',
    '    $impl.TIntClass.DoGlob();',
    '    iC.$class.DoGlob();',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '$impl.TIntClass.DoGlob();',
    '']),
    LinesToStr([
    'rtl.createClass($impl, "TIntClass", $mod.TObject, function () {',
    '  this.Create$1 = function () {',
    '    $mod.TObject.Create.apply(this, arguments);',
    '    $mod.TObject.Create.call(this);',
    '    this.$class.DoGlob();',
    '  };',
    '  this.DoGlob = function () {',
    '  };',
    '});',
    '']));
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
    'rtl.createClass($mod,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '});',
    'rtl.createClass($mod,"TClassA",$mod.TObject,function(){',
    '});',
    'rtl.createClass($mod,"TClassB",$mod.TObject,function(){',
    '  this.ProcB = function () {',
    '  };',
    '});',
    'this.oO = null;',
    'this.oA = null;',
    'this.oB = null;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.oO = $mod.TObject.$create("Create");',
    '$mod.oA = $mod.TClassA.$create("Create");',
    '$mod.oB = $mod.TClassB.$create("Create");',
    'if ($mod.TClassA.isPrototypeOf($mod.oO));',
    '$mod.oB = rtl.as($mod.oO, $mod.TClassB);',
    'rtl.as($mod.oO, $mod.TClassB).ProcB();'
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    'rtl.createClass($mod,"TObject",null,function(){',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoVirtual = function () {',
    '  };',
    '  this.DoIt = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TA", $mod.TObject, function () {',
    '  this.DoAbstract = function () {',
    '    $mod.TObject.DoVirtual.call(this);',
    '  };',
    '  this.DoVirtual = function () {',
    '    $mod.TObject.DoVirtual.apply(this, arguments);',
    '    $mod.TObject.DoVirtual.call(this);',
    '    $mod.TObject.DoVirtual.call(this);',
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    'rtl.createClass($mod, "TClassA", $mod.TObject, function () {',
    '  this.DoAbstract = function (pA,pB) {',
    '    $mod.TObject.DoVirtual.call(this,pA,pB);',
    '    $mod.TObject.DoVirtual.call(this,pA,0);',
    '  };',
    '  this.DoVirtual = function (pA,pB) {',
    '    $mod.TObject.DoVirtual.apply(this, arguments);',
    '    $mod.TObject.DoVirtual.call(this,pA,pB);',
    '    $mod.TObject.DoVirtual.call(this,pA,0);',
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    'rtl.createClass($mod, "TA", $mod.TObject, function () {',
    '  this.Create = function () {',
    '    $mod.TObject.Create.apply(this, arguments);',
    '    $mod.TObject.Create.call(this);',
    '    $mod.TObject.CreateWithB.call(this, false);',
    '  };',
    '  this.CreateWithC = function (c) {',
    '    $mod.TObject.Create.call(this);',
    '    $mod.TObject.CreateWithB.call(this, true);',
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.TObject.$create("Create");',
    '$mod.TObject.vI = 3;',
    'if ($mod.TObject.vI == 4);',
    '$mod.TObject.Sub=null;',
    '$mod.Obj.$class.Sub=null;',
    '$mod.Obj.Sub.$class.Sub=null;',
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
    'rtl.createClass($mod,"TObject",null,function(){',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.TObject.$create("Create");',
    '$mod.TObject.GetIt(5);',
    '$mod.Obj.$class.GetIt(6);',
    '$mod.Obj.Sub.$class.GetIt(7);',
    '$mod.Obj.Sub.$class.GetIt(8).$class.Sub=null;',
    '$mod.Obj.Sub.$class.GetIt(9).$class.GetIt(10);',
    '$mod.Obj.Sub.$class.GetIt(11).Sub.$class.GetIt(12);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.Fy = $mod.Obj.Fx + 1;',
    'if ($mod.Obj.GetInt() == 2);',
    '$mod.Obj.SetInt($mod.Obj.GetInt() + 2);',
    '$mod.Obj.SetInt($mod.Obj.Fx);'
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.TObject.Fy = $mod.TObject.Fx + 1;',
    'if ($mod.TObject.GetInt() == 2);',
    '$mod.TObject.SetInt($mod.TObject.GetInt() + 2);',
    '$mod.TObject.SetInt($mod.TObject.Fx);',
    '$mod.Obj.$class.Fy = $mod.Obj.Fx + 1;',
    'if ($mod.Obj.$class.GetInt() == 2);',
    '$mod.Obj.$class.SetInt($mod.Obj.$class.GetInt() + 2);',
    '$mod.Obj.$class.SetInt($mod.Obj.Fx);'
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.SetItems(11,$mod.Obj.GetItems(12));'
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.SetItems([]);',
    '$mod.Obj.SetItems($mod.Obj.GetItems());',
    '$mod.Obj.GetItems()[11] = $mod.Obj.GetItems()[12];'
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.SetItems(11, 12);',
    '$mod.Obj.SetItems(13, $mod.Obj.GetItems(14));',
    '$mod.Obj.SetItems($mod.Obj.GetItems(15), $mod.Obj.GetItems($mod.Obj.GetItems(15)));'
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FItem = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TCar", $mod.TObject, function () {',
    '  this.$init = function () {',
    '    $mod.TObject.$init.call(this);',
    '    this.FBag = 0;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.Car = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.Obj.SetItem($mod.Obj.GetItem());',
    '$mod.Car.SetItem($mod.Car.GetBag());',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.Obj = null;',
    'this.b = false;'
    ]),
    LinesToStr([ // $mod.$main
    'if ($mod.Obj != null);',
    '$mod.b = ($mod.Obj != null) || false;'
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    'var $with1 = $mod.TObject.$create("Create");',
    '$mod.b = $with1.aBool;',
    '$with1.aBool = $mod.b;',
    '$mod.b = $with1.Arr[1];',
    '$with1.Arr[2] = $mod.b;',
    'var $with2 = $mod.TObject;',
    '$mod.Obj = $with2.$create("Create");',
    'var $with3 = $mod.Obj;',
    '$with3.Create();',
    '$mod.b = $with3.aBool;',
    '$with3.aBool = $mod.b;',
    '$mod.b = $with3.Arr[3];',
    '$with3.Arr[4] = $mod.b;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    'var $with1 = $mod.TObject.$create("Create");',
    '$mod.i = $with1.FInt;',
    '$with1.FInt = $mod.i;',
    '$mod.i = $with1.GetSize();',
    '$with1.SetSize($mod.i);',
    'var $with2 = $mod.Obj;',
    '$mod.i = $with2.FInt;',
    '$with2.FInt = $mod.i;',
    '$mod.i = $with2.GetSize();',
    '$with2.SetSize($mod.i);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    'var $with1 = $mod.TObject.$create("Create");',
    '$mod.i = $with1.GetItems(1);',
    '$with1.SetItems(2, $mod.i);',
    'var $with2 = $mod.Obj;',
    '$mod.i = $with2.GetItems(3);',
    '$with2.SetItems(4, $mod.i);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    'var $with1 = $mod.TObject.$create("Create");',
    '$mod.i = $with1.GetSize();',
    '$mod.i = $with1.GetSize();',
    '$with1.SetSize($mod.i);',
    'var $with2 = $mod.Obj;',
    '$mod.i = $with2.GetSize();',
    '$mod.i = $with2.GetSize();',
    '$with2.SetSize($mod.i);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.Next = null;',
    '  };',
    '  this.$final = function () {',
    '    this.Next = undefined;',
    '  };',
    '  this.Create = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TControl", $mod.TObject, function () {',
    '  this.$init = function () {',
    '    $mod.TObject.$init.call(this);',
    '    this.Arr = [];',
    '  };',
    '  this.$final = function () {',
    '    this.Arr = undefined;',
    '    $mod.TObject.$final.call(this);',
    '  };',
    '  this.GetIt = function (vI) {',
    '    var Result = null;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.Obj.Next;',
    '$mod.Obj = null;',
    '$mod.Obj = $mod.Obj;',
    '$mod.Obj = $mod.Obj.GetIt(0);',
    '$mod.Obj = $mod.Obj.GetIt(0);',
    '$mod.Obj = $mod.Obj.GetIt(1);',
    '$mod.Obj = $mod.Obj.GetIt(0).Arr[2];',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.ProcA({',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.o;',
    '    },',
    '  set: function (v) {',
    '      this.p.o = v;',
    '    }',
    '});',
    '$mod.ProcB({',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.o;',
    '    },',
    '  set: function (v) {',
    '      this.p.o = v;',
    '    }',
    '});',
    '$mod.ProcC($mod.o);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    'rtl.createClass($mod, "TCar", $mod.TObject, function () {',
    '  this.DoIt$2 = function (vA) {',
    '    this.DoIt$2(1);',
    '    this.DoIt$3(1, 2);',
    '    $mod.TObject.DoIt.call(this, 1);',
    '    $mod.TObject.DoIt$1.call(this, 1, 2);',
    '  };',
    '  this.DoIt$3 = function (vA, vB) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    'rtl.createClass($mod, "TCar", $mod.TObject, function () {',
    '  this.Create$2 = function (vA) {',
    '    this.Create$2(1);',
    '    this.Create$3(1, 2);',
    '    $mod.TObject.Create.call(this, 1);',
    '    $mod.TObject.Create$1.call(this, 1, 2);',
    '  };',
    '  this.Create$3 = function (vA, vB) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.TObject.$create("Create", [1]);',
    '$mod.TObject.$create("Create$1", [1, 2]);',
    '$mod.TCar.$create("Create$2", [1]);',
    '$mod.TCar.$create("Create$3", [1, 2]);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.Some = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TMobile", $mod.TObject, function () {',
    '  this.$init = function () {',
    '    $mod.TObject.$init.call(this);',
    '    this.Some$1 = "";',
    '  };',
    '});',
    'rtl.createClass($mod, "TCar", $mod.TMobile, function () {',
    '  this.Some$2 = function () {',
    '    this.Some$2();',
    '    this.Some$3(1);',
    '  };',
    '  this.Some$3 = function (vA) {',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function (Msg) {',
    '  };',
    '});',
    'rtl.createClass($mod, "Exception", $mod.TObject, function () {',
    '});',
    'rtl.createClass($mod, "EConvertError", $mod.Exception, function () {',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    'throw $mod.Exception.$create("Create",["Bar1"]);',
    'throw $mod.EConvertError.$create("Create",["Bar2"]);',
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
    'var $impl = $mod.$impl;',
    'rtl.createClass($mod, "TCar", pas.unit2.TObject, function () {',
    '    this.DoIt = function () {',
    '      this.$DoIntern();',
    '      this.$DoIntern();',
    '      this.$DoIntern2();',
    '      this.$DoIntern2();',
    '    };',
    '  });',
    '']),
    LinesToStr([ // this.$init
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
    '']),
    LinesToStr([ // implementation
    '$impl.Obj = null;',
    '']) );
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
  SetExpectedPasResolverError('Invalid procedure modifier override,external',
    nInvalidXModifierY);
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
    'var $impl = $mod.$impl;',
    'rtl.createClass($mod, "TCar", pas.unit2.TObject, function () {',
    '    this.DoIt = function () {',
    '      this.$Intern = this.$Intern + 1;',
    '      this.$Intern2 = this.$Intern2 + 2;',
    '    };',
    '  });',
    '']),
    LinesToStr([
    '$impl.Obj.$Intern = $impl.Obj.$Intern + 1;',
    '$impl.Obj.$Intern2 = $impl.Obj.$Intern2 + 2;',
    'var $with1 = $impl.Obj;',
    '$with1.$Intern = $with1.$Intern + 1;',
    '$with1.$Intern2 = $with1.$Intern2 + 2;',
    '']),
    LinesToStr([ // implementation
    '$impl.Obj = null;',
    '']));
end;

procedure TTestModule.TestClass_Const;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TClass = class of TObject;');
  Add('  TObject = class');
  Add('  public');
  Add('    const cI: integer = 3;');
  Add('    procedure DoIt;');
  Add('    class procedure DoMore;');
  Add('  end;');
  Add('implementation');
  Add('procedure tobject.doit;');
  Add('begin');
  Add('  if cI=4 then;');
  Add('  if 5=cI then;');
  Add('  if Self.cI=6 then;');
  Add('  if 7=Self.cI then;');
  Add('  with Self do begin');
  Add('    if cI=11 then;');
  Add('    if 12=cI then;');
  Add('  end;');
  Add('end;');
  Add('class procedure tobject.domore;');
  Add('begin');
  Add('  if cI=8 then;');
  Add('  if Self.cI=9 then;');
  Add('  if 10=cI then;');
  Add('  if 11=Self.cI then;');
  Add('  with Self do begin');
  Add('    if cI=13 then;');
  Add('    if 14=cI then;');
  Add('  end;');
  Add('end;');
  Add('var');
  Add('  Obj: TObject;');
  Add('  Cla: TClass;');
  Add('begin');
  Add('  if TObject.cI=21 then ;');
  Add('  if Obj.cI=22 then ;');
  Add('  if Cla.cI=23 then ;');
  Add('  with obj do if ci=24 then;');
  Add('  with TObject do if ci=25 then;');
  Add('  with Cla do if ci=26 then;');
  ConvertProgram;
  CheckSource('TestClass_Const',
    LinesToStr([
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.cI = 3;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '    if (this.cI == 4) ;',
    '    if (5 == this.cI) ;',
    '    if (this.cI == 6) ;',
    '    if (7 == this.cI) ;',
    '    if (this.cI == 11) ;',
    '    if (12 == this.cI) ;',
    '  };',
    '  this.DoMore = function () {',
    '    if (this.cI == 8) ;',
    '    if (this.cI == 9) ;',
    '    if (10 == this.cI) ;',
    '    if (11 == this.cI) ;',
    '    if (this.cI == 13) ;',
    '    if (14 == this.cI) ;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.Cla = null;',
    '']),
    LinesToStr([
    'if ($mod.TObject.cI == 21) ;',
    'if ($mod.Obj.cI == 22) ;',
    'if ($mod.Cla.cI == 23) ;',
    'var $with1 = $mod.Obj;',
    'if ($with1.cI == 24) ;',
    'var $with2 = $mod.TObject;',
    'if ($with2.cI == 25) ;',
    'var $with3 = $mod.Cla;',
    'if ($with3.cI == 26) ;',
    '']));
end;

procedure TTestModule.TestClass_LocalVarSelfFail;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    constructor Create;',
  '  end;',
  'constructor tobject.create;',
  'var self: longint;',
  'begin',
  'end',
  'begin',
  '']);
  SetExpectedPasResolverError('Duplicate identifier "self" at (0)',nDuplicateIdentifier);
  ConvertProgram;
end;

procedure TTestModule.TestClass_ArgSelfFail;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    procedure DoIt(Self: longint);',
  '  end;',
  'procedure tobject.doit(self: longint);',
  'begin',
  'end',
  'begin',
  '']);
  SetExpectedPasResolverError('Duplicate identifier "Self" at test1.pp(5,23)',nDuplicateIdentifier);
  ConvertProgram;
end;

procedure TTestModule.TestClass_NestedSelf;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    Key: longint;',
  '    class var State: longint;',
  '    procedure DoIt;',
  '    function GetSize: longint; virtual; abstract;',
  '    procedure SetSize(Value: longint); virtual; abstract;',
  '    property Size: longint read GetSize write SetSize;',
  '  end;',
  'procedure tobject.doit;',
  '  procedure Sub;',
  '  begin',
  '    key:=key+2;',
  '    self.key:=self.key+3;',
  '    state:=state+4;',
  '    self.state:=self.state+5;',
  '    tobject.state:=tobject.state+6;',
  '    size:=size+7;',
  '    self.size:=self.size+8;',
  '  end;',
  'begin',
  '  sub;',
  '  key:=key+12;',
  '  self.key:=self.key+13;',
  '  state:=state+14;',
  '  self.state:=self.state+15;',
  '  tobject.state:=tobject.state+16;',
  '  size:=size+17;',
  '  self.size:=self.size+18;',
  'end;',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestClass_NestedSelf',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.State = 0;',
    '  this.$init = function () {',
    '    this.Key = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '    var Self = this;',
    '    function Sub() {',
    '      Self.Key = Self.Key + 2;',
    '      Self.Key = Self.Key + 3;',
    '      Self.$class.State = Self.State + 4;',
    '      Self.$class.State = Self.State + 5;',
    '      $mod.TObject.State = $mod.TObject.State + 6;',
    '      Self.SetSize(Self.GetSize() + 7);',
    '      Self.SetSize(Self.GetSize() + 8);',
    '    };',
    '    Sub();',
    '    Self.Key = Self.Key + 12;',
    '    Self.Key = Self.Key + 13;',
    '    Self.$class.State = Self.State + 14;',
    '    Self.$class.State = Self.State + 15;',
    '    $mod.TObject.State = $mod.TObject.State + 16;',
    '    Self.SetSize(Self.GetSize() + 17);',
    '    Self.SetSize(Self.GetSize() + 18);',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestClass_NestedClassSelf;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    class var State: longint;',
  '    class procedure DoIt;',
  '    class function GetSize: longint; virtual; abstract;',
  '    class procedure SetSize(Value: longint); virtual; abstract;',
  '    class property Size: longint read GetSize write SetSize;',
  '  end;',
  'class procedure tobject.doit;',
  '  procedure Sub;',
  '  begin',
  '    state:=state+2;',
  '    self.state:=self.state+3;',
  '    tobject.state:=tobject.state+4;',
  '    size:=size+5;',
  '    self.size:=self.size+6;',
  '    tobject.size:=tobject.size+7;',
  '  end;',
  'begin',
  '  sub;',
  '  state:=state+12;',
  '  self.state:=self.state+13;',
  '  tobject.state:=tobject.state+14;',
  '  size:=size+15;',
  '  self.size:=self.size+16;',
  '  tobject.size:=tobject.size+17;',
  'end;',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestClass_NestedClassSelf',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.State = 0;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '    var Self = this;',
    '    function Sub() {',
    '      Self.State = Self.State + 2;',
    '      Self.State = Self.State + 3;',
    '      $mod.TObject.State = $mod.TObject.State + 4;',
    '      Self.SetSize(Self.GetSize() + 5);',
    '      Self.SetSize(Self.GetSize() + 6);',
    '      $mod.TObject.SetSize($mod.TObject.GetSize() + 7);',
    '    };',
    '    Sub();',
    '    Self.State = Self.State + 12;',
    '    Self.State = Self.State + 13;',
    '    $mod.TObject.State = $mod.TObject.State + 14;',
    '    Self.SetSize(Self.GetSize() + 15);',
    '    Self.SetSize(Self.GetSize() + 16);',
    '    $mod.TObject.SetSize($mod.TObject.GetSize() + 17);',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestClass_NestedCallInherited;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    function DoIt(k: boolean): longint; virtual;',
  '  end;',
  '  TBird = class',
  '    function DoIt(k: boolean): longint; override;',
  '  end;',
  'function tobject.doit(k: boolean): longint;',
  'begin',
  'end;',
  'function tbird.doit(k: boolean): longint;',
  '  procedure Sub;',
  '  begin',
  '    inherited DoIt(true);',
  //'    if inherited DoIt(false)=4 then ;',
  '  end;',
  'begin',
  '  Sub;',
  '  inherited;',
  '  inherited DoIt(true);',
  //'  if inherited DoIt(false)=14 then ;',
  'end;',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestClass_NestedCallInherited',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (k) {',
    '    var Result = 0;',
    '    return Result;',
    '  };',
    '});',
    'rtl.createClass($mod, "TBird", $mod.TObject, function () {',
    '  this.DoIt = function (k) {',
    '    var Self = this;',
    '    var Result = 0;',
    '    function Sub() {',
    '      $mod.TObject.DoIt.call(Self, true);',
    '    };',
    '    Sub();',
    '    $mod.TObject.DoIt.apply(Self, arguments);',
    '    $mod.TObject.DoIt.call(Self, true);',
    '    return Result;',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestClass_TObjectFree;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    Obj: tobject;',
  '    procedure Free;',
  '  end;',
  'procedure tobject.free;',
  'begin',
  'end;',
  'function DoIt(o: tobject): tobject;',
  'var l: tobject;',
  'begin',
  '  o.free;',
  '  o.free();',
  '  l.free;',
  '  l.free();',
  '  o.obj.free;',
  '  o.obj.free();',
  '  with o do obj.free;',
  '  with o do obj.free();',
  '  result.Free;',
  '  result.Free();',
  'end;',
  'var o: tobject;',
  '  a: array of tobject;',
  'begin',
  '  o.free;',
  '  o.obj.free;',
  '  a[1+2].free;',
  '']);
  ConvertProgram;
  CheckSource('TestClass_TObjectFree',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.Obj = null;',
    '  };',
    '  this.$final = function () {',
    '    this.Obj = undefined;',
    '  };',
    '  this.Free = function () {',
    '  };',
    '});',
    'this.DoIt = function (o) {',
    '  var Result = null;',
    '  var l = null;',
    '  o = rtl.freeLoc(o);',
    '  o = rtl.freeLoc(o);',
    '  l = rtl.freeLoc(l);',
    '  l = rtl.freeLoc(l);',
    '  rtl.free(o, "Obj");',
    '  rtl.free(o, "Obj");',
    '  rtl.free(o, "Obj");',
    '  rtl.free(o, "Obj");',
    '  Result = rtl.freeLoc(Result);',
    '  Result = rtl.freeLoc(Result);',
    '  return Result;',
    '};',
    'this.o = null;',
    'this.a = [];',
    '']),
    LinesToStr([ // $mod.$main
    'rtl.free($mod, "o");',
    'rtl.free($mod.o, "Obj");',
    'rtl.free($mod.a, 1 + 2);',
    '']));
end;

procedure TTestModule.TestClass_TObjectFreeNewInstance;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    constructor Create;',
  '    procedure Free;',
  '  end;',
  'constructor TObject.Create; begin end;',
  'procedure tobject.free; begin end;',
  'begin',
  '  with tobject.create do free;',
  '']);
  ConvertProgram;
  CheckSource('TestClass_TObjectFreeNewInstance',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Create = function () {',
    '  };',
    '  this.Free = function () {',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    'var $with1 = $mod.TObject.$create("Create");',
    '$with1=rtl.freeLoc($with1);',
    '']));
end;

procedure TTestModule.TestClass_TObjectFreeLowerCase;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    destructor Destroy;',
  '    procedure Free;',
  '  end;',
  'destructor TObject.Destroy; begin end;',
  'procedure tobject.free; begin end;',
  'var o: tobject;',
  'begin',
  '  o.free;',
  '']);
  Converter.UseLowerCase:=true;
  ConvertProgram;
  CheckSource('TestClass_TObjectFreeLowerCase',
    LinesToStr([ // statements
    'rtl.createClass($mod, "tobject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  rtl.tObjectDestroy = "destroy";',
    '  this.destroy = function () {',
    '  };',
    '  this.free = function () {',
    '  };',
    '});',
    'this.o = null;',
    '']),
    LinesToStr([ // $mod.$main
    'rtl.free($mod, "o");',
    '']));
end;

procedure TTestModule.TestClass_TObjectFreeFunctionFail;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    procedure Free;',
  '    function GetObj: tobject; virtual; abstract;',
  '  end;',
  'procedure tobject.free;',
  'begin',
  'end;',
  'var o: tobject;',
  'begin',
  '  o.getobj.free;',
  '']);
  SetExpectedPasResolverError(sFreeNeedsVar,nFreeNeedsVar);
  ConvertProgram;
end;

procedure TTestModule.TestClass_TObjectFreePropertyFail;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    procedure Free;',
  '    FObj: TObject;',
  '    property Obj: tobject read FObj write FObj;',
  '  end;',
  'procedure tobject.free;',
  'begin',
  'end;',
  'var o: tobject;',
  'begin',
  '  o.obj.free;',
  '']);
  SetExpectedPasResolverError(sFreeNeedsVar,nFreeNeedsVar);
  ConvertProgram;
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.C.$create("Create");',
    'var $with1 = $mod.C;',
    '$mod.Obj = $with1.$create("Create");',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '  };',
    '});',
    'this.C = null;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.C.DoIt();',
    'var $with1 = $mod.C;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.C = null;',
    '$mod.C = $mod.Obj.ClassType;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TCar", $mod.TObject, function () {',
    '});',
    'this.Obj = null;',
    'this.C = null;',
    'this.Cars = null;'
    ]),
    LinesToStr([ // $mod.$main
    'if(rtl.is($mod.C,$mod.TCar));',
    'if(rtl.is($mod.C,$mod.TCar));',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.b = $mod.C == null;',
    '$mod.b = null == $mod.C;',
    '$mod.b = $mod.C == $mod.Obj.ClassType;',
    '$mod.b = $mod.Obj.ClassType == $mod.C;',
    '$mod.b = $mod.C == $mod.TObject;',
    '$mod.b = $mod.TObject == $mod.C;',
    '$mod.b = $mod.C != null;',
    '$mod.b = null != $mod.C;',
    '$mod.b = $mod.C != $mod.Obj.ClassType;',
    '$mod.b = $mod.Obj.ClassType != $mod.C;',
    '$mod.b = $mod.C != $mod.TObject;',
    '$mod.b = $mod.TObject != $mod.C;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.id = 0;',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.C = null;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.C.id = $mod.C.id;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.C.DoIt(0);',
    '$mod.C.DoIt(0);',
    '$mod.i = $mod.C.DoIt(0);',
    '$mod.i = $mod.C.DoIt(0);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.$class.FA = $mod.Obj.FA;',
    '$mod.Obj.$class.SetA($mod.Obj.$class.GetA());',
    '$mod.b = $mod.Obj.FA == 4;',
    '$mod.b = $mod.Obj.$class.GetA() == $mod.Obj.$class.GetA();',
    '$mod.b = 5 == $mod.Obj.FA;',
    '$mod.Cla.FA = 6;',
    '$mod.Cla.FA = $mod.Cla.FA;',
    '$mod.Cla.SetA($mod.Cla.GetA());',
    '$mod.b = $mod.Cla.FA == 7;',
    '$mod.b = $mod.Cla.GetA() == $mod.Cla.GetA();',
    '$mod.b = 8 == $mod.Cla.FA;',
    '$mod.TObject.FA = 9;',
    '$mod.TObject.SetA($mod.TObject.GetA());',
    '$mod.b = $mod.TObject.FA == 10;',
    '$mod.b = 11 == $mod.TObject.FA;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function () {',
    '    this.DoIt();',
    '    this.DoIt$1();',
    '  };',
    '});',
    'rtl.createClass($mod, "TMobile", $mod.TObject, function () {',
    '  this.DoIt$1 = function () {',
    '    this.DoIt();',
    '    this.DoIt$1();',
    '    this.DoIt$2();',
    '  };',
    '});',
    'rtl.createClass($mod, "TCar", $mod.TMobile, function () {',
    '  this.DoIt$2 = function () {',
    '  };',
    '});',
    'this.ObjC = null;',
    'this.MobileC = null;',
    'this.CarC = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.ObjC.DoIt();',
    '$mod.MobileC.DoIt$1();',
    '$mod.CarC.DoIt$2();',
    '$mod.ObjC.DoIt();',
    '$mod.ObjC.DoIt$1();',
    '$mod.ObjC.DoIt$2();',
    '$mod.MobileC.DoIt();',
    '$mod.MobileC.DoIt$1();',
    '$mod.MobileC.DoIt$2();',
    '$mod.CarC.DoIt();',
    '$mod.CarC.DoIt$1();',
    '$mod.CarC.DoIt$2();',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.CurNow();',
    '$mod.vI = $mod.Obj.CurNow();',
    '$mod.TObject.Now();',
    '$mod.vI = $mod.TObject.Now();',
    '']));
end;

procedure TTestModule.TestNestedClass_Fail;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class',
  '    type TNested = longint;',
  '  end;',
  'begin']);
  SetExpectedPasResolverError('not yet implemented: TNested:TPasAliasType [20170608232534] nested types',
    nNotYetImplemented);
  ConvertProgram;
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.$Id = $mod.Obj.$Id + 1;',
    '$mod.Obj.B = $mod.Obj.B + 1;',
    '']));
end;

procedure TTestModule.TestExternalClass_Dollar;
begin
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  'type',
  '  TExtA = class external name ''$''',
  '    Id: longint external name ''$'';',
  '    function Bla(i: longint): longint; external name ''$'';',
  '  end;',
  'function dollar(k: longint): longint; external name ''$'';',
  'var Obj: TExtA;',
  'begin',
  '  dollar(1);',
  '  obj.id:=obj.id+2;',
  '  obj.Bla(3);',
  '']);
  ConvertProgram;
  CheckSource('TestExternalClass_Dollar',
    LinesToStr([ // statements
    'this.Obj = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$(1);',
    '$mod.Obj.$ = $mod.Obj.$ + 2;',
    '$mod.Obj.$(3);',
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
    LinesToStr([ // $mod.$main
    '$mod.Obj.$Execute(1);',
    '$mod.Obj.$Execute(1);',
    '$mod.Obj.$Execute(2);',
    'var $with1 = $mod.Obj;',
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
    'rtl.createClassExt($mod, "TExtC", ExtObjB, "", function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.A.ProcA();',
    '$mod.B.ProcA();',
    '$mod.C.ProcA();',
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
    'rtl.createClassExt($mod, "TExtB", ExtA, "", function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.A.setYear($mod.A.getYear()+1);',
    '$mod.B.OtherSetYear($mod.B.getYear()+2);',
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
    'rtl.createClassExt($mod, "TExtB", ExtA, "", function () {',
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
    '    $mod.TExtB.SetCentury($mod.TExtB.GetCentury() + 14);',
    '    this.SetCentury(this.GetCentury() + 15);',
    '  };',
    '});',
    'this.A = null;',
    'this.B = null;',
    '']),
    LinesToStr([ // $mod.$main
    'ExtA.setYear(ExtA.getYear() + 1);',
    '$mod.TExtB.setYear($mod.TExtB.getYear() + 2);',
    'ExtA.setYear(ExtA.getYear() + 3);',
    '$mod.B.setYear($mod.B.getYear() + 4);',
    '$mod.TExtB.SetCentury($mod.TExtB.GetCentury() + 5);',
    '$mod.B.$class.SetCentury($mod.B.$class.GetCentury() + 6);',
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
    'rtl.createClassExt($mod, "TExtC", ExtB, "", function () {',
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
    LinesToStr([ // $mod.$main
    '$mod.ClA = ExtA;',
    '$mod.ClA = ExtB;',
    '$mod.ClA = $mod.TExtC;',
    '$mod.ClB = ExtB;',
    '$mod.ClB = $mod.TExtC;',
    '$mod.ClC = $mod.TExtC;',
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
    'rtl.createClassExt($mod, "TExtC", ExtB, "", function () {',
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
    LinesToStr([ // $mod.$main
    'if (rtl.isExt($mod.A, ExtB)) ;',
    'if ($mod.TExtC.isPrototypeOf($mod.A)) ;',
    'if ($mod.TExtC.isPrototypeOf($mod.B)) ;',
    'if (rtl.isExt($mod.ClA, ExtB)) ;',
    'if (rtl.is($mod.ClA, $mod.TExtC)) ;',
    'if (rtl.is($mod.ClB, $mod.TExtC)) ;',
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
    'rtl.createClassExt($mod, "TExtC", ExtB, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.A = null;',
    'this.B = null;',
    'this.C = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.B = rtl.asExt($mod.A, ExtB);',
    '$mod.C = rtl.as($mod.A, $mod.TExtC);',
    '$mod.C = rtl.as($mod.B, $mod.TExtC);',
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
  CheckSource('TestExternalClass_New',
    LinesToStr([ // statements
    'this.A = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.A = new ExtA();',
    '$mod.A = new ExtA();',
    '$mod.A = new ExtA(1,2);',
    '$mod.A = new ExtA();',
    '$mod.A = new ExtA();',
    '$mod.A = new ExtA(2,2);',
    '$mod.A = new ExtA();',
    '$mod.A = new ExtA();',
    '$mod.A = new ExtA(3,2);',
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
    LinesToStr([ // $mod.$main
    '$mod.A = new $mod.C();',
    '$mod.A = new $mod.C();',
    'var $with1 = $mod.C;',
    '$mod.A = new $with1();',
    '$mod.A = new $with1();',
    '$mod.A = new $mod.C();',
    '$mod.A = new $mod.C();',
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
    LinesToStr([ // $mod.$main
    '$mod.A = new ($mod.GetCreator())();',
    '$mod.A = new ($mod.GetCreator())();',
    '$mod.A = new ($mod.GetCreator())();',
    '$mod.A = new ($mod.GetCreator())();',
    'var $with1 = $mod.GetCreator();',
    '$mod.A = new $with1();',
    '$mod.A = new $with1();',
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
    LinesToStr([ // $mod.$main
    '$mod.A = new ExtA();',
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
    'rtl.createClassExt($mod, "TMyA", ExtA, "", function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt$1 = function () {',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
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
    'rtl.createClassExt($mod, "TMyC", ExtB, "", function () {',
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
    LinesToStr([ // $mod.$main
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
    'rtl.createClassExt($mod, "TMyB", ExtA, "NewInstance", function () {',
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
    LinesToStr([ // $mod.$main
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

procedure TTestModule.TestExternalClass_PascalProperty;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSElement = class;');
  Add('  TJSNotifyEvent = procedure(Sender: TJSElement) of object;');
  Add('  TJSElement = class external name ''ExtA''');
  Add('  end;');
  Add('  TControl = class(TJSElement)');
  Add('  private');
  Add('    FOnClick: TJSNotifyEvent;');
  Add('    property OnClick: TJSNotifyEvent read FOnClick write FOnClick;');
  Add('    procedure Click(Sender: TJSElement);');
  Add('  end;');
  Add('procedure TControl.Click(Sender: TJSElement);');
  Add('begin');
  Add('  OnClick(Self);');
  Add('end;');
  Add('var');
  Add('  Ctrl: TControl;');
  Add('begin');
  Add('  Ctrl.OnClick:=@Ctrl.Click;');
  Add('  Ctrl.OnClick(Ctrl);');
  ConvertProgram;
  CheckSource('TestExternalClass_PascalProperty',
    LinesToStr([ // statements
    'rtl.createClassExt($mod, "TControl", ExtA, "", function () {',
    '  this.$init = function () {',
    '    this.FOnClick = null;',
    '  };',
    '  this.$final = function () {',
    '    this.FOnClick = undefined;',
    '  };',
    '  this.Click = function (Sender) {',
    '    this.FOnClick(this);',
    '  };',
    '});',
    'this.Ctrl = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.Ctrl.FOnClick = rtl.createCallback($mod.Ctrl, "Click");',
    '$mod.Ctrl.FOnClick($mod.Ctrl);',
    '']));
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TChild", $mod.TObject, function () {',
    '});',
    'this.Obj = null;',
    'this.Child = null;',
    'this.RootA = null;',
    'this.ChildA = null;',
    'this.RootB = null;',
    'this.ChildB = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.Obj = $mod.RootA;',
    '$mod.Obj = $mod.ChildA;',
    '$mod.Child = $mod.RootA;',
    '$mod.RootA = $mod.Obj;',
    '$mod.RootA = $mod.Child;',
    '$mod.RootA = $mod.RootB;',
    '$mod.RootA = $mod.ChildB;',
    '$mod.ChildA = $mod.Obj;',
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
    LinesToStr([ // $mod.$main
    '$mod.s = String.fromCharCode(65, 66);',
    '$mod.s = $mod.s.anchor($mod.s);',
    '$mod.s = "foo".anchor($mod.s);',
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

procedure TTestModule.TestExternalClass_BracketAccessor;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array2''');
  Add('    function GetItems(Index: longint): jsvalue; external name ''[]'';');
  Add('    procedure SetItems(Index: longint; Value: jsvalue); external name ''[]'';');
  Add('    property Items[Index: longint]: jsvalue read GetItems write SetItems; default;');
  Add('  end;');
  Add('procedure DoIt(vI: JSValue; const vJ: jsvalue; var vK: jsvalue; out vL: jsvalue);');
  Add('begin end;');
  Add('var');
  Add('  Arr: tjsarray;');
  Add('  s: string;');
  Add('  i: longint;');
  Add('  v: jsvalue;');
  Add('begin');
  Add('  v:=arr[0];');
  Add('  v:=arr.items[1];');
  Add('  arr[2]:=s;');
  Add('  arr.items[3]:=s;');
  Add('  arr[4]:=i;');
  Add('  arr[5]:=arr[6];');
  Add('  arr.items[7]:=arr.items[8];');
  Add('  with arr do items[9]:=items[10];');
  Add('  doit(arr[7],arr[8],arr[9],arr[10]);');
  ConvertProgram;
  CheckSource('TestExternalClass_BracketAccessor',
    LinesToStr([ // statements
    'this.DoIt = function (vI, vJ, vK, vL) {',
    '};',
    'this.Arr = null;',
    'this.s = "";',
    'this.i = 0;',
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.Arr[0];',
    '$mod.v = $mod.Arr[1];',
    '$mod.Arr[2] = $mod.s;',
    '$mod.Arr[3] = $mod.s;',
    '$mod.Arr[4] = $mod.i;',
    '$mod.Arr[5] = $mod.Arr[6];',
    '$mod.Arr[7] = $mod.Arr[8];',
    'var $with1 = $mod.Arr;',
    '$with1[9] = $with1[10];',
    '$mod.DoIt($mod.Arr[7], $mod.Arr[8], {',
    '  a: 9,',
    '  p: $mod.Arr,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '}, {',
    '  a: 10,',
    '  p: $mod.Arr,',
    '  get: function () {',
    '      return this.p[this.a];',
    '    },',
    '  set: function (v) {',
    '      this.p[this.a] = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestExternalClass_BracketAccessor_2ParamsFail;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array2''');
  Add('    function GetItems(Index1, Index2: longint): jsvalue; external name ''[]'';');
  Add('    procedure SetItems(Index1, Index2: longint; Value: jsvalue); external name ''[]'';');
  Add('    property Items[Index1, Index2: longint]: jsvalue read GetItems write SetItems; default;');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError(sBracketAccessorOfExternalClassMustHaveOneParameter,
    nBracketAccessorOfExternalClassMustHaveOneParameter);
  ConvertProgram;
end;

procedure TTestModule.TestExternalClass_BracketAccessor_ReadOnly;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array2''');
  Add('    function GetItems(Index: longint): jsvalue; external name ''[]'';');
  Add('    property Items[Index: longint]: jsvalue read GetItems; default;');
  Add('  end;');
  Add('procedure DoIt(vI: JSValue; const vJ: jsvalue);');
  Add('begin end;');
  Add('var');
  Add('  Arr: tjsarray;');
  Add('  v: jsvalue;');
  Add('begin');
  Add('  v:=arr[0];');
  Add('  v:=arr.items[1];');
  Add('  with arr do v:=items[2];');
  Add('  doit(arr[3],arr[4]);');
  ConvertProgram;
  CheckSource('TestExternalClass_BracketAccessor_ReadOnly',
    LinesToStr([ // statements
    'this.DoIt = function (vI, vJ) {',
    '};',
    'this.Arr = null;',
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.Arr[0];',
    '$mod.v = $mod.Arr[1];',
    'var $with1 = $mod.Arr;',
    '$mod.v = $with1[2];',
    '$mod.DoIt($mod.Arr[3], $mod.Arr[4]);',
    '']));
end;

procedure TTestModule.TestExternalClass_BracketAccessor_WriteOnly;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array2''');
  Add('    procedure SetItems(Index: longint; Value: jsvalue); external name ''[]'';');
  Add('    property Items[Index: longint]: jsvalue write SetItems; default;');
  Add('  end;');
  Add('var');
  Add('  Arr: tjsarray;');
  Add('  s: string;');
  Add('  i: longint;');
  Add('  v: jsvalue;');
  Add('begin');
  Add('  arr[2]:=s;');
  Add('  arr.items[3]:=s;');
  Add('  arr[4]:=i;');
  Add('  with arr do items[5]:=i;');
  ConvertProgram;
  CheckSource('TestExternalClass_BracketAccessor_WriteOnly',
    LinesToStr([ // statements
    'this.Arr = null;',
    'this.s = "";',
    'this.i = 0;',
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.Arr[2] = $mod.s;',
    '$mod.Arr[3] = $mod.s;',
    '$mod.Arr[4] = $mod.i;',
    'var $with1 = $mod.Arr;',
    '$with1[5] = $mod.i;',
    '']));
end;

procedure TTestModule.TestExternalClass_BracketAccessor_MultiType;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array2''');
  Add('    procedure SetItems(Index: longint; Value: jsvalue); external name ''[]'';');
  Add('    property Items[Index: longint]: jsvalue write SetItems; default;');
  Add('    procedure SetNumbers(Index: longint; Value: longint); external name ''[]'';');
  Add('    property Numbers[Index: longint]: longint write SetNumbers;');
  Add('  end;');
  Add('var');
  Add('  Arr: tjsarray;');
  Add('  s: string;');
  Add('  i: longint;');
  Add('  v: jsvalue;');
  Add('begin');
  Add('  arr[2]:=s;');
  Add('  arr.items[3]:=s;');
  Add('  arr.numbers[4]:=i;');
  Add('  with arr do items[5]:=i;');
  Add('  with arr do numbers[6]:=i;');
  ConvertProgram;
  CheckSource('TestExternalClass_BracketAccessor_MultiType',
    LinesToStr([ // statements
    'this.Arr = null;',
    'this.s = "";',
    'this.i = 0;',
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.Arr[2] = $mod.s;',
    '$mod.Arr[3] = $mod.s;',
    '$mod.Arr[4] = $mod.i;',
    'var $with1 = $mod.Arr;',
    '$with1[5] = $mod.i;',
    'var $with2 = $mod.Arr;',
    '$with2[6] = $mod.i;',
    '']));
end;

procedure TTestModule.TestExternalClass_BracketAccessor_Index;
begin
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TJSArray = class external name ''Array2''');
  Add('    function GetItems(Index: longint): jsvalue; external name ''[]'';');
  Add('    procedure SetItems(Index: longint; Value: jsvalue); external name ''[]'';');
  Add('    property Items[Index: longint]: jsvalue read GetItems write SetItems; default;');
  Add('  end;');
  Add('var');
  Add('  Arr: tjsarray;');
  Add('  i: longint;');
  Add('  IntArr: array of longint;');
  Add('  v: jsvalue;');
  Add('begin');
  Add('  v:=arr.items[i];');
  Add('  arr[longint(v)]:=arr.items[intarr[0]];');
  Add('  arr.items[intarr[1]]:=arr[IntArr[2]];');
  ConvertProgram;
  CheckSource('TestExternalClass_BracketAccessor_Index',
    LinesToStr([ // statements
    'this.Arr = null;',
    'this.i = 0;',
    'this.IntArr = [];',
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.Arr[$mod.i];',
    '$mod.Arr[Math.floor($mod.v)] = $mod.Arr[$mod.IntArr[0]];',
    '$mod.Arr[$mod.IntArr[1]] = $mod.Arr[$mod.IntArr[2]];',
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
  Add('  if Assigned(vp) then ;');
  ConvertProgram;
  CheckSource('TestProcType',
    LinesToStr([ // statements
    'this.DoIt = function(vJ) {',
    '};',
    'this.b = false;',
    'this.vP = null;',
    'this.vQ = null;'
    ]),
    LinesToStr([ // $mod.$main
    '$mod.vP = null;',
    '$mod.vP = $mod.vP;',
    '$mod.vP = $mod.DoIt;',
    '$mod.vP(1);',
    '$mod.vP(1);',
    '$mod.vP(2);',
    '$mod.b = $mod.vP == null;',
    '$mod.b = null == $mod.vP;',
    '$mod.b = rtl.eqCallback($mod.vP,$mod.vQ);',
    '$mod.b = rtl.eqCallback($mod.vP, $mod.DoIt);',
    '$mod.b = rtl.eqCallback($mod.DoIt, $mod.vP);',
    '$mod.b = $mod.vP != null;',
    '$mod.b = null != $mod.vP;',
    '$mod.b = !rtl.eqCallback($mod.vP,$mod.vQ);',
    '$mod.b = !rtl.eqCallback($mod.vP, $mod.DoIt);',
    '$mod.b = !rtl.eqCallback($mod.DoIt, $mod.vP);',
    '$mod.b = $mod.vP != null;',
    'if ($mod.vP != null) ;',
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
    LinesToStr([ // $mod.$main
    '$mod.vP = null;',
    '$mod.vP = $mod.vP;',
    '$mod.vP = $mod.DoIt;',
    '$mod.vP(1);',
    '$mod.vP(1);',
    '$mod.vP(2);',
    '$mod.b = $mod.vP == null;',
    '$mod.b = null == $mod.vP;',
    '$mod.b = rtl.eqCallback($mod.vP,$mod.vQ);',
    '$mod.b = rtl.eqCallback($mod.vP, $mod.DoIt);',
    '$mod.b = rtl.eqCallback($mod.DoIt, $mod.vP);',
    '$mod.b = 4 == $mod.vP(1);',
    '$mod.b = $mod.vP != null;',
    '$mod.b = null != $mod.vP;',
    '$mod.b = !rtl.eqCallback($mod.vP,$mod.vQ);',
    '$mod.b = !rtl.eqCallback($mod.vP, $mod.DoIt);',
    '$mod.b = !rtl.eqCallback($mod.DoIt, $mod.vP);',
    '$mod.b = 6 != $mod.vP(1);',
    '$mod.b = $mod.vP != null;',
    '$mod.DoIt($mod.vP(1));',
    '$mod.DoIt($mod.vP(2));',
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
    LinesToStr([ // $mod.$main
    '$mod.vP = null;',
    '$mod.vP = $mod.vP;',
    '$mod.vP = $mod.DoIt;',
    '$mod.vP = $mod.DoIt;',
    '$mod.vP(1);',
    '$mod.vP(1);',
    '$mod.vP(2);',
    '$mod.b = $mod.vP(1) == $mod.vQ(1);',
    '$mod.b = $mod.vP(1) == 3;',
    '$mod.b = 4 == $mod.vP(1);',
    '$mod.b = $mod.vP(1) != $mod.vQ(1);',
    '$mod.b = $mod.vP(1) != 5;',
    '$mod.b = 6 != $mod.vP(1);',
    '$mod.b = $mod.vP != null;',
    '$mod.DoIt($mod.vP(1));',
    '$mod.DoIt($mod.vP(1));',
    '$mod.DoIt($mod.vP(2));',
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
    '  $mod.DoIt(vG, vG, {',
    '    get: function () {',
    '      return vG;',
    '    },',
    '    set: function (v) {',
    '      vG = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vH, vH, {',
    '    get: function () {',
    '      return vJ;',
    '    },',
    '    set: function (v) {',
    '      vJ = v;',
    '    }',
    '  });',
    '  $mod.DoIt(vI.get(), vI.get(), vI);',
    '  $mod.DoIt(vJ, vJ, {',
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
    '$mod.DoIt($mod.i,$mod.i,{',
    '  p: $mod,',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    '$mod.vP = rtl.createCallback($mod.Obj, "DoIt");',
    '$mod.vP(1);',
    '$mod.vP(1);',
    '$mod.vP(2);',
    '$mod.b = rtl.eqCallback($mod.vP, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = rtl.eqCallback(rtl.createCallback($mod.Obj, "DoIt"), $mod.vP);',
    '$mod.b = !rtl.eqCallback($mod.vP, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = !rtl.eqCallback(rtl.createCallback($mod.Obj, "DoIt"), $mod.vP);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    '$mod.vP = rtl.createCallback($mod.Obj, "DoIt");',
    '$mod.vP = rtl.createCallback($mod.Obj, "DoIt");',
    '$mod.vP(1);',
    '$mod.vP(1);',
    '$mod.vP(2);',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    '$mod.Obj.FOnFoo = null;',
    '$mod.Obj.SetFoo(null);',
    '$mod.Obj.SetEvents(1, null);',
    '$mod.Obj.FOnFoo = $mod.Obj.FOnFoo;',
    '$mod.Obj.SetFoo($mod.Obj.GetFoo());',
    '$mod.Obj.SetEvents(2, $mod.Obj.GetEvents(3));',
    '$mod.Obj.FOnFoo = rtl.createCallback($mod.Obj, "DoIt");',
    '$mod.Obj.SetFoo(rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.Obj.SetEvents(4, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.Obj.FOnFoo(1);',
    '$mod.Obj.GetFoo();',
    '$mod.Obj.FOnFoo(1);',
    '$mod.Obj.GetFoo()(1);',
    '$mod.Obj.GetEvents(6)(1);',
    '$mod.b = $mod.Obj.FOnFoo == null;',
    '$mod.b = $mod.Obj.GetFoo() == null;',
    '$mod.b = $mod.Obj.GetEvents(7) == null;',
    '$mod.b = $mod.Obj.FOnFoo != null;',
    '$mod.b = $mod.Obj.GetFoo() != null;',
    '$mod.b = $mod.Obj.GetEvents(8) != null;',
    '$mod.b = rtl.eqCallback($mod.Obj.FOnFoo, $mod.vP);',
    '$mod.b = rtl.eqCallback($mod.Obj.GetFoo(), $mod.vP);',
    '$mod.b = rtl.eqCallback($mod.Obj.GetEvents(9), $mod.vP);',
    '$mod.b = rtl.eqCallback($mod.Obj.FOnFoo, $mod.Obj.FOnFoo);',
    '$mod.b = rtl.eqCallback($mod.Obj.GetFoo(), $mod.Obj.FOnFoo);',
    '$mod.b = rtl.eqCallback($mod.Obj.GetEvents(10), $mod.Obj.FOnFoo);',
    '$mod.b = !rtl.eqCallback($mod.Obj.FOnFoo, $mod.Obj.FOnFoo);',
    '$mod.b = !rtl.eqCallback($mod.Obj.GetFoo(), $mod.Obj.FOnFoo);',
    '$mod.b = !rtl.eqCallback($mod.Obj.GetEvents(11), $mod.Obj.FOnFoo);',
    '$mod.b = rtl.eqCallback($mod.Obj.FOnFoo, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = rtl.eqCallback($mod.Obj.GetFoo(), rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = rtl.eqCallback($mod.Obj.GetEvents(12), rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = !rtl.eqCallback($mod.Obj.FOnFoo, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = !rtl.eqCallback($mod.Obj.GetFoo(), rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = !rtl.eqCallback($mod.Obj.GetEvents(12), rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.b = $mod.Obj.FOnFoo != null;',
    '$mod.b = $mod.Obj.GetFoo() != null;',
    '$mod.b = $mod.Obj.GetEvents(13) != null;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    '$mod.Obj.FOnFoo = null;',
    '$mod.Obj.SetFoo(null);',
    '$mod.Obj.SetEvents(1, null);',
    '$mod.Obj.FOnFoo = $mod.Obj.FOnFoo;',
    '$mod.Obj.SetFoo($mod.Obj.GetFoo());',
    '$mod.Obj.SetEvents(2, $mod.Obj.GetEvents(3));',
    '$mod.Obj.FOnFoo = rtl.createCallback($mod.Obj, "DoIt");',
    '$mod.Obj.SetFoo(rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.Obj.SetEvents(4, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.Obj.FOnFoo = rtl.createCallback($mod.Obj, "DoIt");',
    '$mod.Obj.SetFoo(rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.Obj.SetEvents(4, rtl.createCallback($mod.Obj, "DoIt"));',
    '$mod.Obj.FOnFoo(1);',
    '$mod.Obj.GetFoo();',
    '$mod.Obj.FOnFoo(1);',
    '$mod.Obj.GetFoo()(1);',
    '$mod.Obj.GetEvents(6)(1);',
    '$mod.b = $mod.Obj.FOnFoo(1) == $mod.vP(1);',
    '$mod.b = $mod.Obj.GetFoo() == $mod.vP(1);',
    '$mod.b = $mod.Obj.FOnFoo(1) == $mod.Obj.FOnFoo(1);',
    '$mod.b = $mod.Obj.GetFoo() == $mod.Obj.FOnFoo(1);',
    '$mod.b = $mod.Obj.FOnFoo(1) != $mod.Obj.FOnFoo(1);',
    '$mod.b = $mod.Obj.GetFoo() != $mod.Obj.FOnFoo(1);',
    '$mod.b = $mod.Obj.FOnFoo != null;',
    '$mod.b = $mod.Obj.GetFoo() != null;',
    '$mod.b = $mod.Obj.GetEvents(13) != null;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
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
    'var $with1 = $mod.Obj;',
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
    '$mod.b = $with1.FOnFoo == null;',
    '$mod.b = $with1.FOnFoo == null;',
    '$mod.b = $with1.GetFoo() == null;',
    '$mod.b = $with1.FOnFoo != null;',
    '$mod.b = $with1.FOnFoo != null;',
    '$mod.b = $with1.GetFoo() != null;',
    '$mod.b = rtl.eqCallback($with1.FOnFoo, $mod.vP);',
    '$mod.b = rtl.eqCallback($with1.FOnFoo, $mod.vP);',
    '$mod.b = rtl.eqCallback($with1.GetFoo(), $mod.vP);',
    '$mod.b = rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    '$mod.b = rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    '$mod.b = rtl.eqCallback($with1.GetFoo(), $with1.FOnFoo);',
    '$mod.b = !rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    '$mod.b = !rtl.eqCallback($with1.FOnFoo, $with1.FOnFoo);',
    '$mod.b = !rtl.eqCallback($with1.GetFoo(), $with1.FOnFoo);',
    '$mod.b = rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    '$mod.b = rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    '$mod.b = rtl.eqCallback($with1.GetFoo(), rtl.createCallback($with1, "DoIt"));',
    '$mod.b = !rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    '$mod.b = !rtl.eqCallback($with1.FOnFoo, rtl.createCallback($with1, "DoIt"));',
    '$mod.b = !rtl.eqCallback($with1.GetFoo(), rtl.createCallback($with1, "DoIt"));',
    '$mod.b = $with1.FOnFoo != null;',
    '$mod.b = $with1.FOnFoo != null;',
    '$mod.b = $with1.GetFoo() != null;',
    '']));
end;

procedure TTestModule.TestProcType_Nested;
begin
  StartProgram(false);
  Add([
  'type',
  '  TProcInt = procedure(vI: longint = 1);',
  'procedure DoIt(vJ: longint);',
  'var aProc: TProcInt;',
  '    b: boolean;',
  '  procedure Sub(vK: longint);',
  '  var aSub: TProcInt;',
  '    procedure SubSub(vK: longint);',
  '    var aSubSub: TProcInt;',
  '    begin;',
  '      aProc:=@DoIt;',
  '      aSub:=@DoIt;',
  '      aSubSub:=@DoIt;',
  '      aProc:=@Sub;',
  '      aSub:=@Sub;',
  '      aSubSub:=@Sub;',
  '      aProc:=@SubSub;',
  '      aSub:=@SubSub;',
  '      aSubSub:=@SubSub;',
  '    end;',
  '  begin;',
  '  end;',
  'begin;',
  '  aProc:=@Sub;',
  '  b:=aProc=@Sub;',
  '  b:=@Sub=aProc;',
  'end;',
  'begin',
  '']);
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
    '      aProc = $mod.DoIt;',
    '      aSub = $mod.DoIt;',
    '      aSubSub = $mod.DoIt;',
    '      aProc = Sub;',
    '      aSub = Sub;',
    '      aSubSub = Sub;',
    '      aProc = SubSub;',
    '      aSub = SubSub;',
    '      aSubSub = SubSub;',
    '    };',
    '  };',
    '  aProc = Sub;',
    '  b = rtl.eqCallback(aProc, Sub);',
    '  b = rtl.eqCallback(Sub, aProc);',
    '};',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestProcType_NestedOfObject;
begin
  StartProgram(false);
  Add([
  'type',
  '  TProcInt = procedure(vI: longint = 1) of object;',
  '  TObject = class',
  '    procedure DoIt(vJ: longint);',
  '  end;',
  'procedure TObject.DoIt(vJ: longint);',
  'var aProc: TProcInt;',
  '    b: boolean;',
  '  procedure Sub(vK: longint);',
  '  var aSub: TProcInt;',
  '    procedure SubSub(vK: longint);',
  '    var aSubSub: TProcInt;',
  '    begin;',
  '      aProc:=@DoIt;',
  '      aSub:=@DoIt;',
  '      aSubSub:=@DoIt;',
  '      aProc:=@Sub;',
  '      aSub:=@Sub;',
  '      aSubSub:=@Sub;',
  '      aProc:=@SubSub;',
  '      aSub:=@SubSub;',
  '      aSubSub:=@SubSub;',
  '    end;',
  '  begin;',
  '  end;',
  'begin;',
  '  aProc:=@Sub;',
  '  b:=aProc=@Sub;',
  '  b:=@Sub=aProc;',
  'end;',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestProcType_Nested',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.DoIt = function (vJ) {',
    '    var Self = this;',
    '    var aProc = null;',
    '    var b = false;',
    '    function Sub(vK) {',
    '      var aSub = null;',
    '      function SubSub(vK) {',
    '        var aSubSub = null;',
    '        aProc = rtl.createCallback(Self, "DoIt");',
    '        aSub = rtl.createCallback(Self, "DoIt");',
    '        aSubSub = rtl.createCallback(Self, "DoIt");',
    '        aProc = Sub;',
    '        aSub = Sub;',
    '        aSubSub = Sub;',
    '        aProc = SubSub;',
    '        aSub = SubSub;',
    '        aSubSub = SubSub;',
    '      };',
    '    };',
    '    aProc = Sub;',
    '    b = rtl.eqCallback(aProc, Sub);',
    '    b = rtl.eqCallback(Sub, aProc);',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestProcType_ReferenceToProc;
begin
  StartProgram(false);
  Add([
  'type',
  '  TProcRef = reference to procedure(i: longint = 0);',
  '  TFuncRef = reference to function(i: longint = 0): longint;',
  'var',
  '  p: TProcRef;',
  '  f: TFuncRef;',
  'procedure DoIt(i: longint);',
  'begin',
  'end;',
  'function GetIt(i: longint): longint;',
  'begin',
  '  p:=@DoIt;',
  '  f:=@GetIt;',
  '  f;',
  '  f();',
  '  f(1);',
  'end;',
  'begin',
  '  p:=@DoIt;',
  '  f:=@GetIt;',
  '  f;',
  '  f();',
  '  f(1);',
  '  p:=TProcRef(f);',
  '']);
  ConvertProgram;
  CheckSource('TestProcType_ReferenceToProc',
    LinesToStr([ // statements
    'this.p = null;',
    'this.f = null;',
    'this.DoIt = function (i) {',
    '};',
    'this.GetIt = function (i) {',
    '  var Result = 0;',
    '  $mod.p = $mod.DoIt;',
    '  $mod.f = $mod.GetIt;',
    '  $mod.f(0);',
    '  $mod.f(0);',
    '  $mod.f(1);',
    '  return Result;',
    '};',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.DoIt;',
    '$mod.f = $mod.GetIt;',
    '$mod.f(0);',
    '$mod.f(0);',
    '$mod.f(1);',
    '$mod.p = $mod.f;',
    '']));
end;

procedure TTestModule.TestProcType_ReferenceToMethod;
begin
  StartProgram(false);
  Add([
  'type',
  '  TFuncRef = reference to function(i: longint = 5): longint;',
  '  TObject = class',
  '    function Grow(s: longint): longint;',
  '  end;',
  'var',
  '  f: tfuncref;',
  'function tobject.grow(s: longint): longint;',
  '  function GrowSub(i: longint): longint;',
  '  begin',
  '    f:=@grow;',
  '    f:=@growsub;',
  '  end;',
  'begin',
  '  f:=@grow;',
  '  f:=@growsub;',
  'end;',
  'begin',
  '']);
  ConvertProgram;
  CheckSource('TestProcType_ReferenceToMethod',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.Grow = function (s) {',
    '    var Self = this;',
    '    var Result = 0;',
    '    function GrowSub(i) {',
    '      var Result = 0;',
    '      $mod.f = rtl.createCallback(Self, "Grow");',
    '      $mod.f = GrowSub;',
    '      return Result;',
    '    };',
    '    $mod.f = rtl.createCallback(Self, "Grow");',
    '    $mod.f = GrowSub;',
    '    return Result;',
    '  };',
    '});',
    'this.f = null;',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestProcType_Typecast;
begin
  StartProgram(false);
  Add([
  'type',
  '  TNotifyEvent = procedure(Sender: Pointer) of object;',
  '  TEvent = procedure of object;',
  '  TGetter = function:longint of object;',
  '  TProcA = procedure(i: longint);',
  '  TFuncB = function(i, j: longint): longint;',
  'procedure DoIt(); varargs; begin end;',
  'var',
  '  Notify: tnotifyevent;',
  '  Event: tevent;',
  '  Getter: tgetter;',
  '  ProcA: tproca;',
  '  FuncB: tfuncb;',
  '  p: pointer;',
  'begin',
  '  notify:=tnotifyevent(event);',
  '  event:=tevent(event);',
  '  event:=tevent(notify);',
  '  event:=tevent(getter);',
  '  event:=tevent(proca);',
  '  proca:=tproca(funcb);',
  '  funcb:=tfuncb(funcb);',
  '  funcb:=tfuncb(proca);',
  '  funcb:=tfuncb(getter);',
  '  proca:=tproca(p);',
  '  funcb:=tfuncb(p);',
  '  getter:=tgetter(p);',
  '  p:=pointer(notify);',
  '  p:=notify;',
  '  p:=pointer(proca);',
  '  p:=proca;',
  '  p:=pointer(funcb);',
  '  p:=funcb;',
  '  doit(Pointer(notify),pointer(event),pointer(proca));',
  '']);
  ConvertProgram;
  CheckSource('TestProcType_Typecast',
    LinesToStr([ // statements
    'this.DoIt = function () {',
    '};',
    'this.Notify = null;',
    'this.Event = null;',
    'this.Getter = null;',
    'this.ProcA = null;',
    'this.FuncB = null;',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.Notify = $mod.Event;',
    '$mod.Event = $mod.Event;',
    '$mod.Event = $mod.Notify;',
    '$mod.Event = $mod.Getter;',
    '$mod.Event = $mod.ProcA;',
    '$mod.ProcA = $mod.FuncB;',
    '$mod.FuncB = $mod.FuncB;',
    '$mod.FuncB = $mod.ProcA;',
    '$mod.FuncB = $mod.Getter;',
    '$mod.ProcA = $mod.p;',
    '$mod.FuncB = $mod.p;',
    '$mod.Getter = $mod.p;',
    '$mod.p = $mod.Notify;',
    '$mod.p = $mod.Notify;',
    '$mod.p = $mod.ProcA;',
    '$mod.p = $mod.ProcA;',
    '$mod.p = $mod.FuncB;',
    '$mod.p = $mod.FuncB;',
    '$mod.DoIt($mod.Notify, $mod.Event, $mod.ProcA);',
    '']));
end;

procedure TTestModule.TestProcType_PassProcToUntyped;
begin
  StartProgram(false);
  Add([
  'type',
  '  TEvent = procedure of object;',
  '  TFunc = function: longint;',
  'procedure DoIt(); varargs; begin end;',
  'procedure DoSome(const a; var b; p: pointer); begin end;',
  'var',
  '  Event: tevent;',
  '  Func: TFunc;',
  'begin',
  '  doit(event,func);',
  '  dosome(event,event,event);',
  '  dosome(func,func,func);',
  '']);
  ConvertProgram;
  CheckSource('TestProcType_PassProcToUntyped',
    LinesToStr([ // statements
    'this.DoIt = function () {',
    '};',
    'this.DoSome = function (a, b, p) {',
    '};',
    'this.Event = null;',
    'this.Func = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt($mod.Event, $mod.Func);',
    '$mod.DoSome($mod.Event, {',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.Event;',
    '    },',
    '  set: function (v) {',
    '      this.p.Event = v;',
    '    }',
    '}, $mod.Event);',
    '$mod.DoSome($mod.Func, {',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.Func;',
    '    },',
    '  set: function (v) {',
    '      this.p.Func = v;',
    '    }',
    '}, $mod.Func);',
    '']));
end;

procedure TTestModule.TestPointer;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('  TClass = class of TObject;');
  Add('  TArrInt = array of longint;');
  Add('var');
  Add('  v: jsvalue;');
  Add('  Obj: tobject;');
  Add('  C: tclass;');
  Add('  a: tarrint;');
  Add('  p: Pointer;');
  Add('begin');
  Add('  p:=p;');
  Add('  p:=nil;');
  Add('  if p=nil then;');
  Add('  if nil=p then;');
  Add('  if Assigned(p) then;');
  Add('  p:=Pointer(v);');
  Add('  p:=obj;');
  Add('  p:=c;');
  Add('  p:=a;');
  Add('  p:=tobject;');
  Add('  obj:=TObject(p);');
  Add('  c:=TClass(p);');
  Add('  a:=TArrInt(p);');
  ConvertProgram;
  CheckSource('TestPointer',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.v = undefined;',
    'this.Obj = null;',
    'this.C = null;',
    'this.a = [];',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.p;',
    '$mod.p = null;',
    'if ($mod.p == null) ;',
    'if (null == $mod.p) ;',
    'if ($mod.p != null) ;',
    '$mod.p = $mod.v;',
    '$mod.p = $mod.Obj;',
    '$mod.p = $mod.C;',
    '$mod.p = $mod.a;',
    '$mod.p = $mod.TObject;',
    '$mod.Obj = $mod.p;',
    '$mod.C = $mod.p;',
    '$mod.a = $mod.p;',
    '']));
end;

procedure TTestModule.TestPointer_Proc;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt; virtual; abstract;');
  Add('  end;');
  Add('procedure DoSome; begin end;');
  Add('var');
  Add('  o: TObject;');
  Add('  p: Pointer;');
  Add('begin');
  Add('  p:=@DoSome;');
  Add('  p:=@o.DoIt;');
  ConvertProgram;
  CheckSource('TestPointer_Proc',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.DoSome = function () {',
    '};',
    'this.o = null;',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.DoSome;',
    '$mod.p = rtl.createCallback($mod.o, "DoIt");',
    '']));
end;

procedure TTestModule.TestPointer_AssignRecordFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TRec = record end;');
  Add('var');
  Add('  p: Pointer;');
  Add('  r: TRec;');
  Add('begin');
  Add('  p:=r;');
  SetExpectedPasResolverError('Incompatible types: got "TRec" expected "Pointer"',
    nIncompatibleTypesGotExpected);
  ConvertProgram;
end;

procedure TTestModule.TestPointer_AssignStaticArrayFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TArr = array[boolean] of longint;');
  Add('var');
  Add('  p: Pointer;');
  Add('  a: TArr;');
  Add('begin');
  Add('  p:=a;');
  SetExpectedPasResolverError('Incompatible types: got "TArr" expected "Pointer"',
    nIncompatibleTypesGotExpected);
  ConvertProgram;
end;

procedure TTestModule.TestPointer_ArrayParamsFail;
begin
  StartProgram(false);
  Add('var');
  Add('  p: Pointer;');
  Add('begin');
  Add('  p:=p[1];');
  SetExpectedPasResolverError('illegal qualifier "["',nIllegalQualifier);
  ConvertProgram;
end;

procedure TTestModule.TestPointer_TypeCastJSValueToPointer;
begin
  StartProgram(false);
  Add([
  'procedure DoIt(args: array of jsvalue); begin end;',
  'procedure DoAll; varargs; begin end;',
  'var',
  '  v: jsvalue;',
  'begin',
  '  DoIt([pointer(v)]);',
  '  DoAll(pointer(v));',
  '']);
  ConvertProgram;
  CheckSource('TestPointer_TypeCastJSValueToPointer',
    LinesToStr([ // statements
    'this.DoIt = function (args) {',
    '};',
    'this.DoAll = function () {',
    '};',
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt([$mod.v]);',
    '$mod.DoAll($mod.v);',
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
  Add('  p: pointer;');
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
  Add('  v:=p;');
  ConvertProgram;
  CheckSource('TestJSValue_AssignToJSValue',
    LinesToStr([ // statements
    'this.v = undefined;',
    'this.i = 0;',
    'this.s = "";',
    'this.b = false;',
    'this.d = 0.0;',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.v;',
    '$mod.v = 1;',
    '$mod.v = $mod.i;',
    '$mod.v = "";',
    '$mod.v = "c";',
    '$mod.v = "foo";',
    '$mod.v = $mod.s;',
    '$mod.v = false;',
    '$mod.v = true;',
    '$mod.v = $mod.b;',
    '$mod.v = 0.1;',
    '$mod.v = $mod.d;',
    '$mod.v = null;',
    '$mod.v = $mod.p;',
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
    LinesToStr([ // $mod.$main
    '$mod.i = Math.floor($mod.v);',
    '$mod.i = Math.floor($mod.v);',
    '$mod.s = "" + $mod.v;',
    '$mod.s = "" + $mod.v;',
    '$mod.b = !($mod.v == false);',
    '$mod.b = !($mod.v == false);',
    '$mod.d = rtl.getNumber($mod.v);',
    '$mod.d = rtl.getNumber($mod.v);',
    '$mod.c = rtl.getChar($mod.v);',
    '$mod.c = rtl.getChar($mod.v);',
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
    LinesToStr([ // $mod.$main
    '$mod.b = $mod.v == $mod.v;',
    '$mod.b = $mod.v != $mod.v;',
    '$mod.b = $mod.v == 1;',
    '$mod.b = $mod.v != 1;',
    '$mod.b = 2 == $mod.v;',
    '$mod.b = 2 != $mod.v;',
    '$mod.b = $mod.v == $mod.i;',
    '$mod.b = $mod.i == $mod.v;',
    '$mod.b = $mod.v == null;',
    '$mod.b = null == $mod.v;',
    '$mod.b = $mod.v == false;',
    '$mod.b = true == $mod.v;',
    '$mod.b = $mod.v == $mod.b;',
    '$mod.b = $mod.b == $mod.v;',
    '$mod.b = $mod.v == $mod.s;',
    '$mod.b = $mod.s == $mod.v;',
    '$mod.b = $mod.v == "foo";',
    '$mod.b = "" == $mod.v;',
    '$mod.b = $mod.v == $mod.d;',
    '$mod.b = $mod.d == $mod.v;',
    '$mod.b = $mod.v == 3.4;',
    '$mod.b = 5.6 == $mod.v;',
    '$mod.b = $mod.v == $mod.c;',
    '$mod.b = $mod.c == $mod.v;',
    '$mod.b = $mod.m == $mod.m;',
    '$mod.b = $mod.v == $mod.m;',
    '$mod.b = $mod.m == $mod.v;',
    '']));
end;

procedure TTestModule.TestJSValue_If;
begin
  StartProgram(false);
  Add([
  'var',
  '  v: jsvalue;',
  'begin',
  '  if v then ;',
  '  while v do ;',
  '  repeat until v;',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_If',
    LinesToStr([ // statements
    'this.v = undefined;',
    '']),
    LinesToStr([ // $mod.$main
    'if ($mod.v) ;',
    'while($mod.v){',
    '};',
    'do{',
    '} while(!$mod.v);',
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
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.e;',
    '$mod.v = $mod.e;',
    '$mod.v = $mod.e;',
    '$mod.e = $mod.v;',
    '$mod.e = $mod.v;',
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
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.v = undefined;',
    'this.o = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.o;',
    '$mod.v = $mod.o;',
    '$mod.v = $mod.o;',
    '$mod.o = rtl.getObject($mod.v);',
    '$mod.o = rtl.getObject($mod.v);',
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
  Add('  v:=TObject;');
  Add('  v:=TClass(c);');
  Add('  v:=TBirds(c);');
  Add('  c:=TClass(v);');
  Add('  c:=TBirds(v);');
  ConvertProgram;
  CheckSource('TestJSValue_ClassOf',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'rtl.createClass($mod, "TBird", $mod.TObject, function () {',
    '});',
    'this.v = undefined;',
    'this.c = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.c;',
    '$mod.v = $mod.TObject;',
    '$mod.v = $mod.c;',
    '$mod.v = $mod.c;',
    '$mod.c = rtl.getObject($mod.v);',
    '$mod.c = rtl.getObject($mod.v);',
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
    LinesToStr([ // $mod.$main
    '$mod.Arr = $mod.TheArray;',
    '$mod.TheArray = $mod.Arr;',
    '$mod.Arr = rtl.arraySetLength($mod.Arr,2,undefined);',
    '$mod.TheArray = rtl.arraySetLength($mod.TheArray,3,undefined);',
    '$mod.Arr[4] = $mod.v;',
    '$mod.Arr[5] = rtl.length($mod.TheArray);',
    '$mod.Arr[6] = null;',
    '$mod.Arr[7] = $mod.TheArray[8];',
    '$mod.Arr[0] = rtl.length($mod.TheArray) - 1;',
    '$mod.Arr = $mod.ArrInt;',
    '$mod.ArrInt = $mod.Arr;',
    'if (rtl.length($mod.TheArray) == 0) ;',
    'if (rtl.length($mod.TheArray) == 0) ;',
    'if (rtl.length($mod.TheArray) > 0) ;',
    'if (rtl.length($mod.TheArray) > 0) ;',
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
    LinesToStr([ // $mod.$main
    '$mod.v = $mod.DoIt($mod.v, $mod.v, {',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.v;',
    '    },',
    '  set: function (v) {',
    '      this.p.v = v;',
    '    }',
    '}, {',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.v;',
    '    },',
    '  set: function (v) {',
    '      this.p.v = v;',
    '    }',
    '});',
    '$mod.i = Math.floor($mod.DoSome($mod.i, $mod.i));',
    '$mod.b = !($mod.DoSome($mod.b, $mod.b) == false);',
    '$mod.d = rtl.getNumber($mod.DoSome($mod.d, $mod.d));',
    '$mod.s = "" + $mod.DoSome($mod.s, $mod.s);',
    '$mod.c = rtl.getChar($mod.DoSome($mod.c, $mod.c));',
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
    LinesToStr([ // $mod.$main
    '$mod.DoIt($mod.i, {',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '}, {',
    '  p: $mod,',
    '  get: function () {',
    '      return this.p.i;',
    '    },',
    '  set: function (v) {',
    '      this.p.i = v;',
    '    }',
    '});',
    '']));
end;

procedure TTestModule.TestJSValue_FuncResultType;
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
    LinesToStr([ // $mod.$main
    'if ($mod.Compare($mod.V, $mod.V) > 0) ;',
    'if ($mod.Compare($mod.i, $mod.i) > 1) ;',
    'if ($mod.Compare(null, false) > 2) ;',
    'if ($mod.Compare(1, true) > 3) ;',
    '']));
end;

procedure TTestModule.TestJSValue_ProcType_Assign;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TObject = class');
  Add('    class function GetGlob: integer;');
  Add('    function Getter: integer;');
  Add('  end;');
  Add('class function TObject.GetGlob: integer;');
  Add('var v1: jsvalue;');
  Add('begin');
  Add('  v1:=@GetGlob;');
  Add('  v1:=@Self.GetGlob;');
  Add('end;');
  Add('function TObject.Getter: integer;');
  Add('var v2: jsvalue;');
  Add('begin');
  Add('  v2:=@Getter;');
  Add('  v2:=@Self.Getter;');
  Add('  v2:=@GetGlob;');
  Add('  v2:=@Self.GetGlob;');
  Add('end;');
  Add('function GetIt(i: integer): integer;');
  Add('var v3: jsvalue;');
  Add('begin');
  Add('  v3:=@GetIt;');
  Add('end;');
  Add('var');
  Add('  V: JSValue;');
  Add('  o: TObject;');
  Add('begin');
  Add('  v:=@GetIt;');
  Add('  v:=@o.Getter;');
  Add('  v:=@o.GetGlob;');
  ConvertProgram;
  CheckSource('TestJSValue_ProcType_Assign',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetGlob = function () {',
    '    var Result = 0;',
    '    var v1 = undefined;',
    '    v1 = rtl.createCallback(this, "GetGlob");',
    '    v1 = rtl.createCallback(this, "GetGlob");',
    '    return Result;',
    '  };',
    '  this.Getter = function () {',
    '    var Result = 0;',
    '    var v2 = undefined;',
    '    v2 = rtl.createCallback(this, "Getter");',
    '    v2 = rtl.createCallback(this, "Getter");',
    '    v2 = rtl.createCallback(this.$class, "GetGlob");',
    '    v2 = rtl.createCallback(this.$class, "GetGlob");',
    '    return Result;',
    '  };',
    '});',
    'this.GetIt = function (i) {',
    '  var Result = 0;',
    '  var v3 = undefined;',
    '  v3 = $mod.GetIt;',
    '  return Result;',
    '};',
    'this.V = undefined;',
    'this.o = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.V = $mod.GetIt;',
    '$mod.V = rtl.createCallback($mod.o, "Getter");',
    '$mod.V = rtl.createCallback($mod.o.$class, "GetGlob");',
    '']));
end;

procedure TTestModule.TestJSValue_ProcType_Equal;
begin
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TObject = class');
  Add('    class function GetGlob: integer;');
  Add('    function Getter: integer;');
  Add('  end;');
  Add('class function TObject.GetGlob: integer;');
  Add('var v1: jsvalue;');
  Add('begin');
  Add('  if v1=@GetGlob then;');
  Add('  if v1=@Self.GetGlob then ;');
  Add('end;');
  Add('function TObject.Getter: integer;');
  Add('var v2: jsvalue;');
  Add('begin');
  Add('  if v2=@Getter then;');
  Add('  if v2=@Self.Getter then ;');
  Add('  if v2=@GetGlob then;');
  Add('  if v2=@Self.GetGlob then;');
  Add('end;');
  Add('function GetIt(i: integer): integer;');
  Add('var v3: jsvalue;');
  Add('begin');
  Add('  if v3=@GetIt then;');
  Add('end;');
  Add('var');
  Add('  V: JSValue;');
  Add('  o: TObject;');
  Add('begin');
  Add('  if v=@GetIt then;');
  Add('  if v=@o.Getter then;');
  Add('  if v=@o.GetGlob then;');
  Add('  if @GetIt=v then;');
  Add('  if @o.Getter=v then;');
  Add('  if @o.GetGlob=v then;');
  ConvertProgram;
  CheckSource('TestJSValue_ProcType_Equal',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.GetGlob = function () {',
    '    var Result = 0;',
    '    var v1 = undefined;',
    '    if (rtl.eqCallback(v1, rtl.createCallback(this, "GetGlob"))) ;',
    '    if (rtl.eqCallback(v1, rtl.createCallback(this, "GetGlob"))) ;',
    '    return Result;',
    '  };',
    '  this.Getter = function () {',
    '    var Result = 0;',
    '    var v2 = undefined;',
    '    if (rtl.eqCallback(v2, rtl.createCallback(this, "Getter"))) ;',
    '    if (rtl.eqCallback(v2, rtl.createCallback(this, "Getter"))) ;',
    '    if (rtl.eqCallback(v2, rtl.createCallback(this.$class, "GetGlob"))) ;',
    '    if (rtl.eqCallback(v2, rtl.createCallback(this.$class, "GetGlob"))) ;',
    '    return Result;',
    '  };',
    '});',
    'this.GetIt = function (i) {',
    '  var Result = 0;',
    '  var v3 = undefined;',
    '  if (rtl.eqCallback(v3, $mod.GetIt)) ;',
    '  return Result;',
    '};',
    'this.V = undefined;',
    'this.o = null;',
    '']),
    LinesToStr([ // $mod.$main
    'if (rtl.eqCallback($mod.V, $mod.GetIt)) ;',
    'if (rtl.eqCallback($mod.V, rtl.createCallback($mod.o, "Getter"))) ;',
    'if (rtl.eqCallback($mod.V, rtl.createCallback($mod.o.$class, "GetGlob"))) ;',
    'if (rtl.eqCallback($mod.GetIt, $mod.V)) ;',
    'if (rtl.eqCallback(rtl.createCallback($mod.o, "Getter"), $mod.V)) ;',
    'if (rtl.eqCallback(rtl.createCallback($mod.o.$class, "GetGlob"), $mod.V)) ;',
    '']));
end;

procedure TTestModule.TestJSValue_AssignToPointerFail;
begin
  StartProgram(false);
  Add([
  'var',
  '  v: JSValue;',
  '  p: Pointer;',
  'begin',
  '  p:=v;',
  '']);
  SetExpectedPasResolverError('Incompatible types: got "JSValue" expected "Pointer"',
    nIncompatibleTypesGotExpected);
  ConvertProgram;
end;

procedure TTestModule.TestJSValue_OverloadDouble;
begin
  StartProgram(false);
  Add([
  'type',
  '  integer = longint;',
  '  tdatetime = double;',
  'procedure DoIt(d: double); begin end;',
  'procedure DoIt(v: jsvalue); begin end;',
  'var',
  '  d: double;',
  '  dt: tdatetime;',
  '  i: integer;',
  '  b: byte;',
  '  shi: shortint;',
  '  w: word;',
  '  smi: smallint;',
  '  lw: longword;',
  '  li: longint;',
  '  ni: nativeint;',
  '  nu: nativeuint;',
  'begin',
  '  DoIt(d);',
  '  DoIt(dt);',
  '  DoIt(i);',
  '  DoIt(b);',
  '  DoIt(shi);',
  '  DoIt(w);',
  '  DoIt(smi);',
  '  DoIt(lw);',
  '  DoIt(li);',
  '  DoIt(ni);',
  '  DoIt(nu);',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_OverloadDouble',
    LinesToStr([ // statements
    'this.DoIt = function (d) {',
    '};',
    'this.DoIt$1 = function (v) {',
    '};',
    'this.d = 0.0;',
    'this.dt = 0.0;',
    'this.i = 0;',
    'this.b = 0;',
    'this.shi = 0;',
    'this.w = 0;',
    'this.smi = 0;',
    'this.lw = 0;',
    'this.li = 0;',
    'this.ni = 0;',
    'this.nu = 0;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt($mod.d);',
    '$mod.DoIt($mod.dt);',
    '$mod.DoIt($mod.i);',
    '$mod.DoIt($mod.b);',
    '$mod.DoIt($mod.shi);',
    '$mod.DoIt($mod.w);',
    '$mod.DoIt($mod.smi);',
    '$mod.DoIt($mod.lw);',
    '$mod.DoIt($mod.li);',
    '$mod.DoIt($mod.ni);',
    '$mod.DoIt($mod.nu);',
    '']));
end;

procedure TTestModule.TestJSValue_OverloadNativeInt;
begin
  StartProgram(false);
  Add([
  'type',
  '  integer = longint;',
  '  int53 = nativeint;',
  '  tdatetime = double;',
  'procedure DoIt(n: nativeint); begin end;',
  'procedure DoIt(v: jsvalue); begin end;',
  'var',
  '  d: double;',
  '  dt: tdatetime;',
  '  i: integer;',
  '  b: byte;',
  '  shi: shortint;',
  '  w: word;',
  '  smi: smallint;',
  '  lw: longword;',
  '  li: longint;',
  '  ni: nativeint;',
  '  nu: nativeuint;',
  'begin',
  '  DoIt(d);',
  '  DoIt(dt);',
  '  DoIt(i);',
  '  DoIt(b);',
  '  DoIt(shi);',
  '  DoIt(w);',
  '  DoIt(smi);',
  '  DoIt(lw);',
  '  DoIt(li);',
  '  DoIt(ni);',
  '  DoIt(nu);',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_OverloadNativeInt',
    LinesToStr([ // statements
    'this.DoIt = function (n) {',
    '};',
    'this.DoIt$1 = function (v) {',
    '};',
    'this.d = 0.0;',
    'this.dt = 0.0;',
    'this.i = 0;',
    'this.b = 0;',
    'this.shi = 0;',
    'this.w = 0;',
    'this.smi = 0;',
    'this.lw = 0;',
    'this.li = 0;',
    'this.ni = 0;',
    'this.nu = 0;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt$1($mod.d);',
    '$mod.DoIt$1($mod.dt);',
    '$mod.DoIt($mod.i);',
    '$mod.DoIt($mod.b);',
    '$mod.DoIt($mod.shi);',
    '$mod.DoIt($mod.w);',
    '$mod.DoIt($mod.smi);',
    '$mod.DoIt($mod.lw);',
    '$mod.DoIt($mod.li);',
    '$mod.DoIt($mod.ni);',
    '$mod.DoIt($mod.nu);',
    '']));
end;

procedure TTestModule.TestJSValue_OverloadWord;
begin
  StartProgram(false);
  Add([
  'type',
  '  integer = longint;',
  '  int53 = nativeint;',
  '  tdatetime = double;',
  'procedure DoIt(w: word); begin end;',
  'procedure DoIt(v: jsvalue); begin end;',
  'var',
  '  d: double;',
  '  dt: tdatetime;',
  '  i: integer;',
  '  b: byte;',
  '  shi: shortint;',
  '  w: word;',
  '  smi: smallint;',
  '  lw: longword;',
  '  li: longint;',
  '  ni: nativeint;',
  '  nu: nativeuint;',
  'begin',
  '  DoIt(d);',
  '  DoIt(dt);',
  '  DoIt(i);',
  '  DoIt(b);',
  '  DoIt(shi);',
  '  DoIt(w);',
  '  DoIt(smi);',
  '  DoIt(lw);',
  '  DoIt(li);',
  '  DoIt(ni);',
  '  DoIt(nu);',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_OverloadWord',
    LinesToStr([ // statements
    'this.DoIt = function (w) {',
    '};',
    'this.DoIt$1 = function (v) {',
    '};',
    'this.d = 0.0;',
    'this.dt = 0.0;',
    'this.i = 0;',
    'this.b = 0;',
    'this.shi = 0;',
    'this.w = 0;',
    'this.smi = 0;',
    'this.lw = 0;',
    'this.li = 0;',
    'this.ni = 0;',
    'this.nu = 0;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt$1($mod.d);',
    '$mod.DoIt$1($mod.dt);',
    '$mod.DoIt$1($mod.i);',
    '$mod.DoIt($mod.b);',
    '$mod.DoIt($mod.shi);',
    '$mod.DoIt($mod.w);',
    '$mod.DoIt$1($mod.smi);',
    '$mod.DoIt$1($mod.lw);',
    '$mod.DoIt$1($mod.li);',
    '$mod.DoIt$1($mod.ni);',
    '$mod.DoIt$1($mod.nu);',
    '']));
end;

procedure TTestModule.TestJSValue_OverloadString;
begin
  StartProgram(false);
  Add([
  'type',
  '  uni = string;',
  '  WideChar = char;',
  'procedure DoIt(s: string); begin end;',
  'procedure DoIt(v: jsvalue); begin end;',
  'var',
  '  s: string;',
  '  c: char;',
  '  u: uni;',
  'begin',
  '  DoIt(s);',
  '  DoIt(c);',
  '  DoIt(u);',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_OverloadString',
    LinesToStr([ // statements
    'this.DoIt = function (s) {',
    '};',
    'this.DoIt$1 = function (v) {',
    '};',
    'this.s = "";',
    'this.c = "";',
    'this.u = "";',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt($mod.s);',
    '$mod.DoIt($mod.c);',
    '$mod.DoIt($mod.u);',
    '']));
end;

procedure TTestModule.TestJSValue_OverloadChar;
begin
  StartProgram(false);
  Add([
  'type',
  '  uni = string;',
  '  WideChar = char;',
  'procedure DoIt(c: char); begin end;',
  'procedure DoIt(v: jsvalue); begin end;',
  'var',
  '  s: string;',
  '  c: char;',
  '  u: uni;',
  'begin',
  '  DoIt(s);',
  '  DoIt(c);',
  '  DoIt(u);',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_OverloadChar',
    LinesToStr([ // statements
    'this.DoIt = function (c) {',
    '};',
    'this.DoIt$1 = function (v) {',
    '};',
    'this.s = "";',
    'this.c = "";',
    'this.u = "";',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt$1($mod.s);',
    '$mod.DoIt($mod.c);',
    '$mod.DoIt$1($mod.u);',
    '']));
end;

procedure TTestModule.TestJSValue_OverloadPointer;
begin
  StartProgram(false);
  Add([
  'type',
  '  TObject = class end;',
  'procedure DoIt(p: pointer); begin end;',
  'procedure DoIt(v: jsvalue); begin end;',
  'var',
  '  o: TObject;',
  'begin',
  '  DoIt(o);',
  '']);
  ConvertProgram;
  CheckSource('TestJSValue_OverloadPointer',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    'this.DoIt = function (p) {',
    '};',
    'this.DoIt$1 = function (v) {',
    '};',
    'this.o = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.DoIt($mod.o);',
    '']));
end;

procedure TTestModule.TestRTTI_ProcType;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TProcA = procedure;');
  Add('  TMethodB = procedure of object;');
  Add('  TProcC = procedure; varargs;');
  Add('  TProcD = procedure(i: longint; const j: string; var c: char; out d: double);');
  Add('  TProcE = function: nativeint;');
  Add('  TProcF = function(const p: TProcA): nativeuint;');
  Add('var p: pointer;');
  Add('begin');
  Add('  p:=typeinfo(tproca);');
  ConvertProgram;
  CheckSource('TestRTTI_ProcType',
    LinesToStr([ // statements
    '$mod.$rtti.$ProcVar("TProcA", {',
    '  procsig: rtl.newTIProcSig(null)',
    '});',
    '$mod.$rtti.$MethodVar("TMethodB", {',
    '  procsig: rtl.newTIProcSig(null),',
    '  methodkind: 0',
    '});',
    '$mod.$rtti.$ProcVar("TProcC", {',
    '  procsig: rtl.newTIProcSig(null, 2)',
    '});',
    '$mod.$rtti.$ProcVar("TProcD", {',
    '  procsig: rtl.newTIProcSig([["i", rtl.longint], ["j", rtl.string, 2], ["c", rtl.char, 1], ["d", rtl.double, 4]])',
    '});',
    '$mod.$rtti.$ProcVar("TProcE", {',
    '  procsig: rtl.newTIProcSig(null, rtl.nativeint)',
    '});',
    '$mod.$rtti.$ProcVar("TProcF", {',
    '  procsig: rtl.newTIProcSig([["p", $mod.$rtti["TProcA"], 2]], rtl.nativeuint)',
    '});',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TProcA"];',
    '']));
end;

procedure TTestModule.TestRTTI_ProcType_ArgFromOtherUnit;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];

  AddModuleWithIntfImplSrc('unit2.pas',
    LinesToStr([
    'type',
    '  TObject = class end;'
    ]),
    '');
  StartUnit(true);
  Add('interface');
  Add('uses unit2;');
  Add('type');
  Add('  TProcA = function(o: tobject): tobject;');
  Add('implementation');
  Add('type');
  Add('  TProcB = function(o: tobject): tobject;');
  Add('var p: Pointer;');
  Add('initialization');
  Add('  p:=typeinfo(tproca);');
  Add('  p:=typeinfo(tprocb);');
  ConvertUnit;
  CheckSource('TestRTTI_ProcType_ArgFromOtherUnit',
    LinesToStr([ // statements
    'var $impl = $mod.$impl;',
    '$mod.$rtti.$ProcVar("TProcA", {',
    '  procsig: rtl.newTIProcSig([["o", pas.unit2.$rtti["TObject"]]], pas.unit2.$rtti["TObject"])',
    '});',
    '']),
    LinesToStr([ // this.$init
    '$impl.p = $mod.$rtti["TProcA"];',
    '$impl.p = $mod.$rtti["TProcB"];',
    '']),
    LinesToStr([ // implementation
    '$mod.$rtti.$ProcVar("TProcB", {',
    '  procsig: rtl.newTIProcSig([["o", pas.unit2.$rtti["TObject"]]], pas.unit2.$rtti["TObject"])',
    '});',
    '$impl.p = null;',
    '']) );
end;

procedure TTestModule.TestRTTI_EnumAndSetType;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TFlag = (light,dark);');
  Add('  TFlags = set of TFlag;');
  Add('  TProc = function(f: TFlags): TFlag;');
  Add('var p: pointer;');
  Add('begin');
  Add('  p:=typeinfo(tflag);');
  Add('  p:=typeinfo(tflags);');
  ConvertProgram;
  CheckSource('TestRTTI_EnumAndType',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "light",',
    '  light: 0,',
    '  "1": "dark",',
    '  dark: 1',
    '};',
    '$mod.$rtti.$Enum("TFlag", {',
    '  minvalue: 0,',
    '  maxvalue: 1,',
    '  enumtype: this.TFlag',
    '});',
    '$mod.$rtti.$Set("TFlags", {',
    '  comptype: $mod.$rtti["TFlag"]',
    '});',
    '$mod.$rtti.$ProcVar("TProc", {',
    '  procsig: rtl.newTIProcSig([["f", $mod.$rtti["TFlags"]]], $mod.$rtti["TFlag"])',
    '});',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TFlag"];',
    '$mod.p = $mod.$rtti["TFlags"];',
    '']));
end;

procedure TTestModule.TestRTTI_AnonymousEnumType;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TFlags = set of (red, green);');
  Add('var');
  Add('  f: TFlags;');
  Add('begin');
  Add('  Include(f,red);');
  ConvertProgram;
  CheckSource('TestRTTI_AnonymousEnumType',
    LinesToStr([ // statements
    'this.TFlags$a = {',
    '  "0": "red",',
    '  red: 0,',
    '  "1": "green",',
    '  green: 1',
    '};',
    '$mod.$rtti.$Enum("TFlags$a", {',
    '  minvalue: 0,',
    '  maxvalue: 1,',
    '  enumtype: this.TFlags$a',
    '});',
    '$mod.$rtti.$Set("TFlags", {',
    '  comptype: $mod.$rtti["TFlags$a"]',
    '});',
    'this.f = {};',
    '']),
    LinesToStr([
    '$mod.f = rtl.includeSet($mod.f, $mod.TFlags$a.red);',
    '']));
end;

procedure TTestModule.TestRTTI_StaticArray;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TFlag = (light,dark);');
  Add('  TFlagNames = array[TFlag] of string;');
  Add('  TBoolNames = array[boolean] of string;');
  Add('  TProc = function(f: TBoolNames): TFlagNames;');
  Add('var p: pointer;');
  Add('begin');
  Add('  p:=typeinfo(TFlagNames);');
  Add('  p:=typeinfo(TBoolNames);');
  ConvertProgram;
  CheckSource('TestRTTI_StaticArray',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "light",',
    '  light: 0,',
    '  "1": "dark",',
    '  dark: 1',
    '};',
    '$mod.$rtti.$Enum("TFlag", {',
    '  minvalue: 0,',
    '  maxvalue: 1,',
    '  enumtype: this.TFlag',
    '});',
    '$mod.$rtti.$StaticArray("TFlagNames", {',
    '  dims: [2],',
    '  eltype: rtl.string',
    '});',
    '$mod.$rtti.$StaticArray("TBoolNames", {',
    '  dims: [2],',
    '  eltype: rtl.string',
    '});',
    '$mod.$rtti.$ProcVar("TProc", {',
    '  procsig: rtl.newTIProcSig([["f", $mod.$rtti["TBoolNames"]]], $mod.$rtti["TFlagNames"])',
    '});',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TFlagNames"];',
    '$mod.p = $mod.$rtti["TBoolNames"];',
    '']));
end;

procedure TTestModule.TestRTTI_DynArray;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TArrStr = array of string;');
  Add('  TArr2Dim = array of tarrstr;');
  Add('  TProc = function(f: TArrStr): TArr2Dim;');
  Add('var p: pointer;');
  Add('begin');
  Add('  p:=typeinfo(tarrstr);');
  Add('  p:=typeinfo(tarr2dim);');
  ConvertProgram;
  CheckSource('TestRTTI_DynArray',
    LinesToStr([ // statements
    '$mod.$rtti.$DynArray("TArrStr", {',
    '  eltype: rtl.string',
    '});',
    '$mod.$rtti.$DynArray("TArr2Dim", {',
    '  eltype: $mod.$rtti["TArrStr"]',
    '});',
    '$mod.$rtti.$ProcVar("TProc", {',
    '  procsig: rtl.newTIProcSig([["f", $mod.$rtti["TArrStr"]]], $mod.$rtti["TArr2Dim"])',
    '});',
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TArrStr"];',
    '$mod.p = $mod.$rtti["TArr2Dim"];',
    '']));
end;

procedure TTestModule.TestRTTI_ArrayNestedAnonymous;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TArr = array of array of longint;');
  Add('var a: TArr;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_ArrayNestedAnonymous',
    LinesToStr([ // statements
    '$mod.$rtti.$DynArray("TArr$a", {',
    '  eltype: rtl.longint',
    '});',
    '$mod.$rtti.$DynArray("TArr", {',
    '  eltype: $mod.$rtti["TArr$a"]',
    '});',
    'this.a = [];',
    '']),
    LinesToStr([ // $mod.$main
    ]));
end;

procedure TTestModule.TestRTTI_PublishedMethodOverloadFail;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  published');
  Add('    procedure Proc; virtual; abstract;');
  Add('    procedure Proc(Sender: tobject); virtual; abstract;');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError('Duplicate identifier "Proc" at test1.pp(6,18)',
    nDuplicateIdentifier);
  ConvertProgram;
end;

procedure TTestModule.TestRTTI_PublishedMethodExternalFail;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  published');
  Add('    procedure Proc; external name ''foo'';');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError(sPublishedNameMustMatchExternal,
    nPublishedNameMustMatchExternal);
  ConvertProgram;
end;

procedure TTestModule.TestRTTI_PublishedClassPropertyFail;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var FA: longint;');
  Add('  published');
  Add('    class property A: longint read FA;');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError('Invalid published property modifier "class"',
    nInvalidXModifierY);
  ConvertProgram;
end;

procedure TTestModule.TestRTTI_PublishedClassFieldFail;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  published');
  Add('    class var FA: longint;');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError(sSymbolCannotBePublished,
    nSymbolCannotBePublished);
  ConvertProgram;
end;

procedure TTestModule.TestRTTI_PublishedFieldExternalFail;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TObject = class');
  Add('  published');
  Add('    V: longint; external name ''foo'';');
  Add('  end;');
  Add('begin');
  SetExpectedPasResolverError(sPublishedNameMustMatchExternal,
    nPublishedNameMustMatchExternal);
  ConvertProgram;
end;

procedure TTestModule.TestRTTI_Class_Field;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TObject = class');
  Add('  private');
  Add('    FPropA: string;');
  Add('  published');
  Add('    VarLI: longint;');
  Add('    VarC: char;');
  Add('    VarS: string;');
  Add('    VarD: double;');
  Add('    VarB: boolean;');
  Add('    VarLW: longword;');
  Add('    VarSmI: smallint;');
  Add('    VarW: word;');
  Add('    VarShI: shortint;');
  Add('    VarBy: byte;');
  Add('    VarExt: longint external name ''VarExt'';');
  Add('  end;');
  Add('var p: pointer;');
  Add('  Obj: tobject;');
  Add('begin');
  Add('  p:=typeinfo(tobject);');
  Add('  p:=typeinfo(p);');
  Add('  p:=typeinfo(obj);');
  ConvertProgram;
  CheckSource('TestRTTI_Class_Field',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FPropA = "";',
    '    this.VarLI = 0;',
    '    this.VarC = "";',
    '    this.VarS = "";',
    '    this.VarD = 0.0;',
    '    this.VarB = false;',
    '    this.VarLW = 0;',
    '    this.VarSmI = 0;',
    '    this.VarW = 0;',
    '    this.VarShI = 0;',
    '    this.VarBy = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addField("VarLI", rtl.longint);',
    '  $r.addField("VarC", rtl.char);',
    '  $r.addField("VarS", rtl.string);',
    '  $r.addField("VarD", rtl.double);',
    '  $r.addField("VarB", rtl.boolean);',
    '  $r.addField("VarLW", rtl.longword);',
    '  $r.addField("VarSmI", rtl.smallint);',
    '  $r.addField("VarW", rtl.word);',
    '  $r.addField("VarShI", rtl.shortint);',
    '  $r.addField("VarBy", rtl.byte);',
    '  $r.addField("VarExt", rtl.longint);',
    '});',
    'this.p = null;',
    'this.Obj = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TObject"];',
    '$mod.p = rtl.pointer;',
    '$mod.p = $mod.Obj.$rtti;',
    '']));
end;

procedure TTestModule.TestRTTI_Class_Method;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  private');
  Add('    procedure Internal; external name ''$intern'';');
  Add('  published');
  Add('    procedure Click; virtual; abstract;');
  Add('    procedure Notify(Sender: TObject); virtual; abstract;');
  Add('    function GetNotify: boolean; external name ''GetNotify'';');
  Add('    procedure Println(a,b: longint); varargs; virtual; abstract;');
  Add('  end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_Class_Method',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addMethod("Click", 0, null);',
    '  $r.addMethod("Notify", 0, [["Sender", $r]]);',
    '  $r.addMethod("GetNotify", 1, null, rtl.boolean,{flags: 4});',
    '  $r.addMethod("Println", 0, [["a", rtl.longint], ["b", rtl.longint]], null, {',
    '    flags: 2',
    '  });',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_Class_MethodArgFlags;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  published');
  Add('    procedure OpenArray(const Args: array of string); virtual; abstract;');
  Add('    procedure ByRef(var Value: longint; out Item: longint); virtual; abstract;');
  Add('    procedure Untyped(var Value; out Item); virtual; abstract;');
  Add('  end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_Class_MethodOpenArray',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '$r.addMethod("OpenArray", 0, [["Args", rtl.string, 10]]);',
    '$r.addMethod("ByRef", 0, [["Value", rtl.longint, 1], ["Item", rtl.longint, 4]]);',
    '$r.addMethod("Untyped", 0, [["Value", null, 1], ["Item", null, 4]]);',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_Class_Property;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TObject = class');
  Add('  private');
  Add('    FColor: longint;');
  Add('    FColorStored: boolean;');
  Add('    procedure SetColor(Value: longint); virtual; abstract;');
  Add('    function GetColor: longint; virtual; abstract;');
  Add('    function GetColorStored: boolean; virtual; abstract;');
  Add('    FExtSize: longint external name ''$extSize'';');
  Add('    FExtSizeStored: boolean external name ''$extSizeStored'';');
  Add('    procedure SetExtSize(Value: longint); external name ''$setSize'';');
  Add('    function GetExtSize: longint; external name ''$getSize'';');
  Add('    function GetExtSizeStored: boolean; external name ''$getExtSizeStored'';');
  Add('  published');
  Add('    property ColorA: longint read FColor;');
  Add('    property ColorB: longint write FColor;');
  Add('    property ColorC: longint read GetColor write SetColor;');
  Add('    property ColorD: longint read FColor write FColor stored FColorStored;');
  Add('    property ExtSizeA: longint read FExtSize write FExtSize;');
  Add('    property ExtSizeB: longint read GetExtSize write SetExtSize stored FExtSizeStored;');
  Add('    property ExtSizeC: longint read FExtSize write FExtSize stored GetExtSizeStored;');
  Add('  end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_Class_Property',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FColor = 0;',
    '    this.FColorStored = false;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addProperty("ColorA", 0, rtl.longint, "FColor", "");',
    '  $r.addProperty("ColorB", 0, rtl.longint, "", "FColor");',
    '  $r.addProperty("ColorC", 3, rtl.longint, "GetColor", "SetColor");',
    '  $r.addProperty("ColorD", 0, rtl.longint, "FColor", "FColor",{',
    '      stored: "FColorStored"',
    '    }',
    '  );',
    '  $r.addProperty("ExtSizeA", 0, rtl.longint, "$extSize", "$extSize");',
    '  $r.addProperty("ExtSizeB", 3, rtl.longint, "$getSize", "$setSize",{',
    '      stored: "$extSizeStored"',
    '    }',
    '  );',
    '  $r.addProperty("ExtSizeC", 4, rtl.longint, "$extSize", "$extSize",{',
    '      stored: "$getExtSizeStored"',
    '    }',
    '  );',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_Class_PropertyParams;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  integer = longint;');
  Add('  TObject = class');
  Add('  private');
  Add('    function GetItems(i: integer): tobject; virtual; abstract;');
  Add('    procedure SetItems(i: integer; value: tobject); virtual; abstract;');
  Add('    function GetValues(const i: integer; var b: boolean): char; virtual; abstract;');
  Add('    procedure SetValues(const i: integer; var b: boolean; value: char); virtual; abstract;');
  Add('  published');
  Add('    property Items[Index: integer]: tobject read getitems write setitems;');
  Add('    property Values[const keya: integer; var keyb: boolean]: char read getvalues write setvalues;');
  Add('  end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_Class_PropertyParams',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addProperty("Items", 3, $r, "GetItems", "SetItems");',
    '  $r.addProperty("Values", 3, rtl.char, "GetValues", "SetValues");',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_OverrideMethod;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  published');
  Add('    procedure DoIt; virtual; abstract;');
  Add('  end;');
  Add('  TSky = class');
  Add('  published');
  Add('    procedure DoIt; override;');
  Add('  end;');
  Add('procedure TSky.DoIt; begin end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_OverrideMethod',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addMethod("DoIt", 0, null);',
    '});',
    'rtl.createClass($mod, "TSky", $mod.TObject, function () {',
    '  this.DoIt = function () {',
    '  };',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_OverloadProperty;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  protected');
  Add('    FFlag: longint;');
  Add('  published');
  Add('    property Flag: longint read fflag;');
  Add('  end;');
  Add('  TSky = class');
  Add('  published');
  Add('    property FLAG: longint write fflag;');
  Add('  end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_OverrideMethod',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.FFlag = 0;',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addProperty("Flag", 0, rtl.longint, "FFlag", "");',
    '});',
    'rtl.createClass($mod, "TSky", $mod.TObject, function () {',
    '  var $r = this.$rtti;',
    '  $r.addProperty("Flag", 0, rtl.longint, "", "FFlag");',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_ClassForward;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('  tbridge = class;');
  Add('  TProc = function: tbridge;');
  Add('  TOger = class');
  Add('  published');
  Add('    FBridge: tbridge;');
  Add('    procedure SetBridge(Value: tbridge); virtual; abstract;');
  Add('    property Bridge: tbridge read fbridge write setbridge;');
  Add('  end;');
  Add('  TBridge = class');
  Add('    FOger: toger;');
  Add('  end;');
  Add('var p: Pointer;');
  Add(' b: tbridge;');
  Add('begin');
  Add('  p:=typeinfo(tbridge);');
  Add('  p:=typeinfo(b);');
  ConvertProgram;
  CheckSource('TestRTTI_ClassForward',
    LinesToStr([ // statements
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    '$mod.$rtti.$Class("TBridge");',
    '$mod.$rtti.$ProcVar("TProc", {',
    '  procsig: rtl.newTIProcSig(null, $mod.$rtti["TBridge"])',
    '});',
    'rtl.createClass($mod, "TOger", $mod.TObject, function () {',
    '  this.$init = function () {',
    '    $mod.TObject.$init.call(this);',
    '    this.FBridge = null;',
    '  };',
    '  this.$final = function () {',
    '    this.FBridge = undefined;',
    '    $mod.TObject.$final.call(this);',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addField("FBridge", $mod.$rtti["TBridge"]);',
    '  $r.addMethod("SetBridge", 0, [["Value", $mod.$rtti["TBridge"]]]);',
    '  $r.addProperty("Bridge", 2, $mod.$rtti["TBridge"], "FBridge", "SetBridge");',
    '});',
    'rtl.createClass($mod, "TBridge", $mod.TObject, function () {',
    '  this.$init = function () {',
    '    $mod.TObject.$init.call(this);',
    '    this.FOger = null;',
    '  };',
    '  this.$final = function () {',
    '    this.FOger = undefined;',
    '    $mod.TObject.$final.call(this);',
    '  };',
    '});',
    'this.p = null;',
    'this.b = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TBridge"];',
    '$mod.p = $mod.b.$rtti;',
    '']));
end;

procedure TTestModule.TestRTTI_ClassOf;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TClass = class of tobject;');
  Add('  TProcA = function: TClass;');
  Add('  TObject = class');
  Add('  published');
  Add('    C: tclass;');
  Add('  end;');
  Add('  tfox = class;');
  Add('  TBird = class end;');
  Add('  TBirds = class of tbird;');
  Add('  TFox = class end;');
  Add('  TFoxes = class of tfox;');
  Add('  TCows = class of TCow;');
  Add('  TCow = class;');
  Add('  TCow = class end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_ClassOf',
    LinesToStr([ // statements
    '$mod.$rtti.$Class("TObject");',
    '$mod.$rtti.$ClassRef("TClass", {',
    '  instancetype: $mod.$rtti["TObject"]',
    '});',
    '$mod.$rtti.$ProcVar("TProcA", {',
    '  procsig: rtl.newTIProcSig(null, $mod.$rtti["TClass"])',
    '});',
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '    this.C = null;',
    '  };',
    '  this.$final = function () {',
    '    this.C = undefined;',
    '  };',
    '  var $r = this.$rtti;',
    '  $r.addField("C", $mod.$rtti["TClass"]);',
    '});',
    '$mod.$rtti.$Class("TFox");',
    'rtl.createClass($mod, "TBird", $mod.TObject, function () {',
    '});',
    '$mod.$rtti.$ClassRef("TBirds", {',
    '  instancetype: $mod.$rtti["TBird"]',
    '});',
    'rtl.createClass($mod, "TFox", $mod.TObject, function () {',
    '});',
    '$mod.$rtti.$ClassRef("TFoxes", {',
    '  instancetype: $mod.$rtti["TFox"]',
    '});',
    '$mod.$rtti.$Class("TCow");',
    '$mod.$rtti.$ClassRef("TCows", {',
    '  instancetype: $mod.$rtti["TCow"]',
    '});',
    'rtl.createClass($mod, "TCow", $mod.TObject, function () {',
    '});',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_Record;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  integer = longint;');
  Add('  TPoint = record');
  Add('    x,y: integer;');
  Add('  end;');
  Add('var p: pointer;');
  Add('  r: tpoint;');
  Add('begin');
  Add('  p:=typeinfo(tpoint);');
  Add('  p:=typeinfo(r);');
  Add('  p:=typeinfo(r.x);');
  ConvertProgram;
  CheckSource('TestRTTI_Record',
    LinesToStr([ // statements
    'this.TPoint = function (s) {',
    '  if (s) {',
    '    this.x = s.x;',
    '    this.y = s.y;',
    '  } else {',
    '    this.x = 0;',
    '    this.y = 0;',
    '  };',
    '  this.$equal = function (b) {',
    '    return (this.x == b.x) && (this.y == b.y);',
    '  };',
    '};',
    '$mod.$rtti.$Record("TPoint", {}).addFields("x", rtl.longint, "y", rtl.longint);',
    'this.p = null;',
    'this.r = new $mod.TPoint();',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = $mod.$rtti["TPoint"];',
    '$mod.p = $mod.$rtti["TPoint"];',
    '$mod.p = rtl.longint;',
    '']));
end;

procedure TTestModule.TestRTTI_LocalTypes;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('procedure DoIt;');
  Add('type');
  Add('  integer = longint;');
  Add('  TPoint = record');
  Add('    x,y: integer;');
  Add('  end;');
  Add('begin');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestRTTI_LocalTypes',
    LinesToStr([ // statements
    'this.DoIt = function () {',
    '  this.TPoint = function (s) {',
    '    if (s) {',
    '      this.x = s.x;',
    '      this.y = s.y;',
    '    } else {',
    '      this.x = 0;',
    '      this.y = 0;',
    '    };',
    '    this.$equal = function (b) {',
    '      return (this.x == b.x) && (this.y == b.y);',
    '    };',
    '  };',
    '};',
    '']),
    LinesToStr([ // $mod.$main
    '']));
end;

procedure TTestModule.TestRTTI_TypeInfo_BaseTypes;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('type');
  Add('  TCaption = string;');
  Add('  TYesNo = boolean;');
  Add('  TLetter = char;');
  Add('  TFloat = double;');
  Add('  TPtr = pointer;');
  Add('  TShortInt = shortint;');
  Add('  TByte = byte;');
  Add('  TSmallInt = smallint;');
  Add('  TWord = word;');
  Add('  TInt32 = longint;');
  Add('  TDWord = longword;');
  Add('  TValue = jsvalue;');
  Add('var p: TPtr;');
  Add('begin');
  Add('  p:=typeinfo(string);');
  Add('  p:=typeinfo(tcaption);');
  Add('  p:=typeinfo(boolean);');
  Add('  p:=typeinfo(tyesno);');
  Add('  p:=typeinfo(char);');
  Add('  p:=typeinfo(tletter);');
  Add('  p:=typeinfo(double);');
  Add('  p:=typeinfo(tfloat);');
  Add('  p:=typeinfo(pointer);');
  Add('  p:=typeinfo(tptr);');
  Add('  p:=typeinfo(shortint);');
  Add('  p:=typeinfo(tshortint);');
  Add('  p:=typeinfo(byte);');
  Add('  p:=typeinfo(tbyte);');
  Add('  p:=typeinfo(smallint);');
  Add('  p:=typeinfo(tsmallint);');
  Add('  p:=typeinfo(word);');
  Add('  p:=typeinfo(tword);');
  Add('  p:=typeinfo(longword);');
  Add('  p:=typeinfo(tdword);');
  Add('  p:=typeinfo(jsvalue);');
  Add('  p:=typeinfo(tvalue);');
  ConvertProgram;
  CheckSource('TestRTTI_TypeInfo_BaseTypes',
    LinesToStr([ // statements
    'this.p = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.p = rtl.string;',
    '$mod.p = rtl.string;',
    '$mod.p = rtl.boolean;',
    '$mod.p = rtl.boolean;',
    '$mod.p = rtl.char;',
    '$mod.p = rtl.char;',
    '$mod.p = rtl.double;',
    '$mod.p = rtl.double;',
    '$mod.p = rtl.pointer;',
    '$mod.p = rtl.pointer;',
    '$mod.p = rtl.shortint;',
    '$mod.p = rtl.shortint;',
    '$mod.p = rtl.byte;',
    '$mod.p = rtl.byte;',
    '$mod.p = rtl.smallint;',
    '$mod.p = rtl.smallint;',
    '$mod.p = rtl.word;',
    '$mod.p = rtl.word;',
    '$mod.p = rtl.longword;',
    '$mod.p = rtl.longword;',
    '$mod.p = rtl.jsvalue;',
    '$mod.p = rtl.jsvalue;',
    '']));
end;

procedure TTestModule.TestRTTI_TypeInfo_LocalFail;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('procedure DoIt;');
  Add('type');
  Add('  integer = longint;');
  Add('  TPoint = record');
  Add('    x,y: integer;');
  Add('  end;');
  Add('var p: pointer;');
  Add('begin');
  Add('  p:=typeinfo(tpoint);');
  Add('end;');
  Add('begin');
  SetExpectedPasResolverError(sSymbolCannotBePublished,nSymbolCannotBePublished);
  ConvertProgram;
end;

procedure TTestModule.TestRTTI_TypeInfo_ExtTypeInfoClasses1;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TTypeInfo = class external name ''rtl.tTypeInfo'' end;');
  Add('  TTypeInfoInteger = class external name ''rtl.tTypeInfoInteger''(TTypeInfo) end;');
  Add('  TFlag = (up,down);');
  Add('  TTypeInfoEnum = class external name ''rtl.tTypeInfoEnum''(TTypeInfoInteger) end;');
  Add('  TFlags = set of TFlag;');
  Add('  TTypeInfoSet = class external name ''rtl.tTypeInfoSet''(TTypeInfo) end;');
  Add('var');
  Add('  ti: TTypeInfo;');
  Add('  tiInt: TTypeInfoInteger;');
  Add('  tiEnum: TTypeInfoEnum;');
  Add('  tiSet: TTypeInfoSet;');
  Add('begin');
  Add('  ti:=typeinfo(string);');
  Add('  ti:=typeinfo(boolean);');
  Add('  ti:=typeinfo(char);');
  Add('  ti:=typeinfo(double);');
  Add('  tiInt:=typeinfo(shortint);');
  Add('  tiInt:=typeinfo(byte);');
  Add('  tiInt:=typeinfo(smallint);');
  Add('  tiInt:=typeinfo(word);');
  Add('  tiInt:=typeinfo(longint);');
  Add('  tiInt:=typeinfo(longword);');
  Add('  ti:=typeinfo(jsvalue);');
  Add('  tiEnum:=typeinfo(tflag);');
  Add('  tiSet:=typeinfo(tflags);');
  ConvertProgram;
  CheckSource('TestRTTI_TypeInfo_ExtTypeInfoClasses1',
    LinesToStr([ // statements
    'this.TFlag = {',
    '  "0": "up",',
    '  up: 0,',
    '  "1": "down",',
    '  down: 1',
    '};',
    '$mod.$rtti.$Enum("TFlag", {',
    '  minvalue: 0,',
    '  maxvalue: 1,',
    '  enumtype: this.TFlag',
    '});',
    '$mod.$rtti.$Set("TFlags", {',
    '  comptype: $mod.$rtti["TFlag"]',
    '});',
    'this.ti = null;',
    'this.tiInt = null;',
    'this.tiEnum = null;',
    'this.tiSet = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.ti = rtl.string;',
    '$mod.ti = rtl.boolean;',
    '$mod.ti = rtl.char;',
    '$mod.ti = rtl.double;',
    '$mod.tiInt = rtl.shortint;',
    '$mod.tiInt = rtl.byte;',
    '$mod.tiInt = rtl.smallint;',
    '$mod.tiInt = rtl.word;',
    '$mod.tiInt = rtl.longint;',
    '$mod.tiInt = rtl.longword;',
    '$mod.ti = rtl.jsvalue;',
    '$mod.tiEnum = $mod.$rtti["TFlag"];',
    '$mod.tiSet = $mod.$rtti["TFlags"];',
    '']));
end;

procedure TTestModule.TestRTTI_TypeInfo_ExtTypeInfoClasses2;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TTypeInfo = class external name ''rtl.tTypeInfo'' end;');
  Add('  TStaticArr = array[boolean] of string;');
  Add('  TTypeInfoStaticArray = class external name ''rtl.tTypeInfoStaticArray''(TTypeInfo) end;');
  Add('  TDynArr = array of string;');
  Add('  TTypeInfoDynArray = class external name ''rtl.tTypeInfoDynArray''(TTypeInfo) end;');
  Add('  TProc = procedure;');
  Add('  TTypeInfoProcVar = class external name ''rtl.tTypeInfoProcVar''(TTypeInfo) end;');
  Add('  TMethod = procedure of object;');
  Add('  TTypeInfoMethodVar = class external name ''rtl.tTypeInfoMethodVar''(TTypeInfoProcVar) end;');
  Add('var');
  Add('  StaticArray: TStaticArr;');
  Add('  tiStaticArray: TTypeInfoStaticArray;');
  Add('  DynArray: TDynArr;');
  Add('  tiDynArray: TTypeInfoDynArray;');
  Add('  ProcVar: TProc;');
  Add('  tiProcVar: TTypeInfoProcVar;');
  Add('  MethodVar: TMethod;');
  Add('  tiMethodVar: TTypeInfoMethodVar;');
  Add('begin');
  Add('  tiStaticArray:=typeinfo(StaticArray);');
  Add('  tiStaticArray:=typeinfo(TStaticArr);');
  Add('  tiDynArray:=typeinfo(DynArray);');
  Add('  tiDynArray:=typeinfo(TDynArr);');
  Add('  tiProcVar:=typeinfo(ProcVar);');
  Add('  tiProcVar:=typeinfo(TProc);');
  Add('  tiMethodVar:=typeinfo(MethodVar);');
  Add('  tiMethodVar:=typeinfo(TMethod);');
  ConvertProgram;
  CheckSource('TestRTTI_TypeInfo_ExtTypeInfoClasses2',
    LinesToStr([ // statements
    '  $mod.$rtti.$StaticArray("TStaticArr", {',
    '  dims: [2],',
    '  eltype: rtl.string',
    '});',
    '$mod.$rtti.$DynArray("TDynArr", {',
    '  eltype: rtl.string',
    '});',
    '$mod.$rtti.$ProcVar("TProc", {',
    '  procsig: rtl.newTIProcSig(null)',
    '});',
    '$mod.$rtti.$MethodVar("TMethod", {',
    '  procsig: rtl.newTIProcSig(null),',
    '  methodkind: 0',
    '});',
    'this.StaticArray = rtl.arrayNewMultiDim([2], "");',
    'this.tiStaticArray = null;',
    'this.DynArray = [];',
    'this.tiDynArray = null;',
    'this.ProcVar = null;',
    'this.tiProcVar = null;',
    'this.MethodVar = null;',
    'this.tiMethodVar = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.tiStaticArray = $mod.$rtti["TStaticArr"];',
    '$mod.tiStaticArray = $mod.$rtti["TStaticArr"];',
    '$mod.tiDynArray = $mod.$rtti["TDynArr"];',
    '$mod.tiDynArray = $mod.$rtti["TDynArr"];',
    '$mod.tiProcVar = $mod.$rtti["TProc"];',
    '$mod.tiProcVar = $mod.$rtti["TProc"];',
    '$mod.tiMethodVar = $mod.$rtti["TMethod"];',
    '$mod.tiMethodVar = $mod.$rtti["TMethod"];',
    '']));
end;

procedure TTestModule.TestRTTI_TypeInfo_ExtTypeInfoClasses3;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add('{$modeswitch externalclass}');
  Add('type');
  Add('  TTypeInfo = class external name ''rtl.tTypeInfo'' end;');
  Add('  TRec = record end;');
  Add('  TTypeInfoRecord = class external name ''rtl.tTypeInfoRecord''(TTypeInfo) end;');
  // ToDo: ^PRec
  Add('  TObject = class end;');
  Add('  TTypeInfoClass = class external name ''rtl.tTypeInfoClass''(TTypeInfo) end;');
  Add('  TClass = class of tobject;');
  Add('  TTypeInfoClassRef = class external name ''rtl.tTypeInfoClassRef''(TTypeInfo) end;');
  Add('  TTypeInfoPointer = class external name ''rtl.tTypeInfoPointer''(TTypeInfo) end;');
  Add('var');
  Add('  Rec: trec;');
  Add('  tiRecord: ttypeinforecord;');
  Add('  Obj: tobject;');
  Add('  tiClass: ttypeinfoclass;');
  Add('  aClass: tclass;');
  Add('  tiClassRef: ttypeinfoclassref;');
  // ToDo: ^PRec
  Add('  tiPointer: ttypeinfopointer;');
  Add('begin');
  Add('  tirecord:=typeinfo(trec);');
  Add('  tirecord:=typeinfo(trec);');
  Add('  ticlass:=typeinfo(obj);');
  Add('  ticlass:=typeinfo(tobject);');
  Add('  ticlass:=typeinfo(aclass);');
  Add('  ticlassref:=typeinfo(tclass);');
  ConvertProgram;
  CheckSource('TestRTTI_TypeInfo_ExtTypeInfoClasses3',
    LinesToStr([ // statements
    'this.TRec = function (s) {',
    '};',
    '$mod.$rtti.$Record("TRec", {});',
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '});',
    '$mod.$rtti.$ClassRef("TClass", {',
    '  instancetype: $mod.$rtti["TObject"]',
    '});',
    'this.Rec = new $mod.TRec();',
    'this.tiRecord = null;',
    'this.Obj = null;',
    'this.tiClass = null;',
    'this.aClass = null;',
    'this.tiClassRef = null;',
    'this.tiPointer = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.tiRecord = $mod.$rtti["TRec"];',
    '$mod.tiRecord = $mod.$rtti["TRec"];',
    '$mod.tiClass = $mod.Obj.$rtti;',
    '$mod.tiClass = $mod.$rtti["TObject"];',
    '$mod.tiClass = $mod.aClass.$rtti;',
    '$mod.tiClassRef = $mod.$rtti["TClass"];',
    '']));
end;

procedure TTestModule.TestRTTI_TypeInfo_FunctionClassType;
begin
  Converter.Options:=Converter.Options-[coNoTypeInfo];
  StartProgram(false);
  Add([
  '{$modeswitch externalclass}',
  'type',
  '  TClass = class of tobject;',
  '  TObject = class',
  '    function MyClass: TClass;',
  '    class function ClassType: TClass;',
  '  end;',
  '  TTypeInfo = class external name ''rtl.tTypeInfo'' end;',
  '  TTypeInfoClass = class external name ''rtl.tTypeInfoClass''(TTypeInfo) end;',
  'function TObject.MyClass: TClass;',
  'var t: TTypeInfoClass;',
  'begin',
  '  t:=TypeInfo(Self);',
  '  t:=TypeInfo(Result);',
  'end;',
  'class function TObject.ClassType: TClass;',
  'var t: TTypeInfoClass;',
  'begin',
  '  t:=TypeInfo(Self);',
  '  t:=TypeInfo(Result);',
  'end;',
  'var',
  '  Obj: TObject;',
  '  t: TTypeInfoClass;',
  'begin',
  '  t:=TypeInfo(TObject.ClassType);',
  '  t:=TypeInfo(Obj.ClassType);',
  '  t:=TypeInfo(Obj.MyClass);',
  '']);
  ConvertProgram;
  CheckSource('TestRTTI_TypeInfo_FunctionClassType',
    LinesToStr([ // statements
    '$mod.$rtti.$Class("TObject");',
    '$mod.$rtti.$ClassRef("TClass", {',
    '  instancetype: $mod.$rtti["TObject"]',
    '});',
    'rtl.createClass($mod, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '  this.$final = function () {',
    '  };',
    '  this.MyClass = function () {',
    '    var Result = null;',
    '    var t = null;',
    '    t = this.$rtti;',
    '    t = Result.$rtti;',
    '    return Result;',
    '  };',
    '  this.ClassType = function () {',
    '    var Result = null;',
    '    var t = null;',
    '    t = this.$rtti;',
    '    t = Result.$rtti;',
    '    return Result;',
    '  };',
    '});',
    'this.Obj = null;',
    'this.t = null;',
    '']),
    LinesToStr([ // $mod.$main
    '$mod.t = $mod.TObject.ClassType().$rtti;',
    '$mod.t = $mod.Obj.$class.ClassType().$rtti;',
    '$mod.t = $mod.Obj.MyClass().$rtti;',
    '']));
end;

Initialization
  RegisterTests([TTestModule]);
end.

