{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    Test WIT parser

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utcwitparser;

interface

// uncomment this to show parsed WIT content }
{ $DEFINE LOGPARSEDCONTENT}

uses
  fpcunit, testregistry, Classes, SysUtils,
  WIT.Scanner, WIT.Model, WIT.Parser;


type
  TResultIgnore = (riResult,riError);
  TResultIgnores = set of TResultIgnore;

  { TTestWITParser }

  TTestWITParser = class(TTestCase)
  private
    FScanner: TWITScanner;
    FParser: TWITParser;
    FDocument: TWITDocument;
    FInputStream: TStringStream;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure InitParser(const aContent: string);
    class function AssertListType(const aMsg: string; aTypeDef: TWitTypeDef;const aName: String; aElementKind: TWITTypeKind; aCount : Integer = 0): TWitType;
    class function AssertEnumType(const aMsg: String; aType: TWITTypeDef;const aName: String; aValues: array of string) : TWITEnumType;
    class function AssertResultType(const aMsg: string; aType: TWITTypeDef;const aName: String; aOKKind: TWITTypeKind; aErrorKind: TWitTypeKind; aIgnore: TResultIgnores) : TWITResultType;
    class function AssertOptionType(const aMsg: string; aType: TWITTypeDef;const aName: String; aOptionKind: TWITTypeKind) : TWITOptionType;
    class function AssertStreamType(const aMsg: string; aType: TWITTypeDef;const aName: String; aStreamKind: TWITTypeKind): TWITStreamType;
    class function AssertFutureType(const aMsg: string; aType: TWITTypeDef;const aName: String; aFutureKind: TWITTypeKind): TWITFutureType;
    class function AssertTupleType(const aMsg: string; aType: TWITTypeDef;const aName: String; aOptionKind: Array of TWITTypeKind) : TWITTupleType;
    class function AssertFlagsType(const aMsg: string; aType: TWITTypeDef;const aName: String; aFlagNames: Array of String) : TWITFlagsType;
    class function AssertVariantType(const aMsg: string; aType: TWITTypeDef;const aName: String; aVariantNames: array of String): TWITVariantType;
    class function AssertRecordType(const aMsg: string; aType: TWITTypeDef;const aName: String; aFieldNames: array of String; aFieldTypes : Array of TWITTypeKind): TWITRecordType;
    class function AssertResourceType(const aMsg: string; aType: TWITTypeDef;const aName: String; aConstructor: boolean; aFunctionNames: array of String): TWITResourceType;
    class function AssertAliasType(const aMsg: string; aType: TWITTypeDef;const aName: String; aAliasName:String): TWITIdentifierType;
    class function AssertHandleType(const aMsg: string; aType: TWITTypeDef;const aName: String; aAliasName:String): TWITHandleType;
    class function AssertTypeDef(const Msg: String; aType: TWITType; aExpectedName: string; aExpectedUnderlyingKind: TWITTypeKind) : TWITType;
    class procedure AssertAnnotationArgument(const Msg: String; aArgument: TWITAnnotationArgument; aName, aValue: string);
    class procedure AssertAnnotation(const Msg: String; aAnnotation: TWITAnnotation;const aName: String; Args: array of String);
    class procedure AssertInclude(const aMsg: string; aInclude: TWITInclude;const aName: String; aItemNames, aItemAliases: array of String);
    class procedure AssertFunction(const Msg: String; aFunc: TWITFunction;const aName: String; aArgCount: Integer; aHaveResult: Boolean; aAnnotationCount: Integer = 0);
    class procedure AssertFunctionParam(const Msg: String; aParam: TWITFuncParam;const aName: String; aTypeKind: TWITTypeKind; aTypeName: string);
    class procedure AssertInterface(const Msg: String; aIntf: TWITInterface;const aName: String; aFuncCount: Integer; aTypeCount : Integer; aAnnotationCount: Integer = 0);
    class procedure AssertWorld(const Msg : String; aWorld :TWITWorld; const aWorldName : String; aExportCount,aImportCount,aUseCount,aTypeDefCount,aIncludeCount : Integer);
    class procedure AssertEquals(Msg: string; aExpected, aActual: TWitTypeKind); overload;
    class procedure AssertUse(Msg: string; aUse : TWITTopLevelUse; const aPackageName, aVersion, aIdentifier, aRename: string; aNameSpaces : Array of string);
    class procedure AssertUsePath(Msg: string; aUse: TWITUsePath; const aPackageName, aVersion, aIdentifier: string; aNameSpaces: array of string);
    class procedure AssertPackage(const Msg: String; aPackage: TWITPackage; aExpectedNamespace: string; aExpectedPackageName: string;
      aExpectedVersion: string; aExpectedWorldCount: Integer=0; aExpectedImportCount: Integer=0; aExpectedExportCount: Integer=0;
      aExpectedUseCount: Integer=0; aExpectedInterfaceCount: Integer=0);
  Protected
    function ParseWorld(const aWorldName : String; aExportCount,aImportCount,aUseCount,aTypeDefCount,aIncludeCount : Integer) : TWITWorld;
    function ParseFunc(const aFuncName: String; aArgNames : Array of string; aArgTypes : Array of TWitTypeKind; aResultType : TWitTypeKind = wtVoid): TWITFunction;
    function ParseType(const aInterFaceName, aTypeName: String): TWITTypeDef;
    function ParseInterface(const aName: String; aFunctionCount, aTypeCount, aAnnotationCount: integer): TWITInterface;
    function WrapTypeDef(const aDef: String; isType : Boolean = false): string;
    function WrapFunc(const aParams: String; aResult : string = ''): string;
  published
    procedure TestParsePackageEmpty;
    procedure TestParseExitInterfaceDocument;
    procedure TestParsePackageVersions;
    procedure TestSimpleTypes;
    procedure TestListType;
    procedure TestListListType;
    procedure TestListSized;
    procedure TestListSizedListSized;
    procedure TestEnum;
    procedure TestEnumEndWithComma;
    procedure TestResultEmpty;
    procedure TestResultOneType;
    procedure TestResultTwoTypes;
    procedure TestResultOneIgnoredTyoe;
    procedure TestOption;
    procedure TestStream;
    procedure TestStreamEmpty;
    procedure TestFuture;
    procedure TestNestedFuture;
    procedure TestFutureEmpty;
    procedure TestTupleEmpty;
    procedure TestTuple1;
    procedure TestTuple2;
    procedure TestTuple3;
    procedure TestTupleComma;
    procedure TestFlagsEmpty;
    procedure TestFlags1;
    procedure TestFlags2;
    procedure TestFlags3;
    procedure TestFlagsComma;
    procedure TestVariant1;
    procedure TestVariant2;
    procedure TestVariant2Comma;
    procedure TestVariantTypedSimple;
    procedure TestVariantTypedSimpleComma;
    procedure TestVariantTypedComplex;
    procedure TestRecordEmpty;
    procedure TestRecord1;
    procedure TestRecord2;
    procedure TestRecord2Comma;
    procedure TestRecordRecordName;
    procedure TestAlias;
    procedure TestBorrowedHandle;
    procedure TestResourceEmpty;
    procedure TestResourceEmpty2;
    procedure TestResourceConstructor;
    procedure TestResourceOneMethod;
    procedure TestResourceStaticMethod;
    procedure TestResourceAsyncMethod;
    procedure TestResourceTwoMethods;
    procedure TestResourceOneMethodAndConstructor;
    procedure TestUseIdentifier;
    procedure TestUseIdentifierAs;
    procedure TestUseFullIdentifier;
    procedure TestUseFullIdentifierVersion;
    procedure TestUseFullIdentifierAs;
    procedure TestUseFullIdentifierVersionAs;
    procedure TestParseFunctionEmpty;
    procedure TestParseFunctionEmptyResult;
    procedure TestParseFunctionOneParam;
    procedure TestParseFunctionOneParamResult;
    procedure TestParseFunctionTwoParams;
    procedure TestParseFunctionTwoParamsResult;
    procedure TestParseWorldEmpty;
    procedure TestParseWorldUse;
    procedure TestParseWorldUseAnnotation;
    procedure TestParseWorldExport;
    procedure TestParseWorldExportUse;
    procedure TestParseWorldExportFunction;
    procedure TestParseWorldExportInterface;
    procedure TestParseWorldImport;
    procedure TestParseWorldImportUse;
    procedure TestParseWorldImportFunction;
    procedure TestParseWorldImportInterface;
    procedure TestParseWorldInclude;
    procedure TestParseWorldIncludeUse;
    procedure TestParseWorldIncludeUseList;
    procedure TestParseWorldIncludeUseList2;
    procedure TestParseWorldTypeDef;
    procedure TestParseWorldEnumType;
    procedure TestParseWorldVariantType;
    procedure TestParseWorldRecordType;
    procedure TestParseWorldFlagsType;
    procedure TestParseInterfaceUse;
    procedure TestParseInterfaceUseGate;
  end;


implementation

uses TypInfo;


{ TTestWITParser }

procedure TTestWITParser.InitParser(const aContent : string);
begin
  FreeAndNil(FDocument);
  FreeAndNil(FInputStream);
  {$IFDEF LOGPARSEDCONTENT}
  Writeln(TestName,' - Parsing:');
  Writeln(aContent);
  {$ENDIF}

  FInputStream := TStringStream.Create(aContent);
  // Assuming TWITScanner.Create(AStream: TStream);
  // If TWITScanner has a different constructor (e.g., taking ownership of stream), adjust accordingly.
  FreeAndNil(FScanner);
  FScanner := TWITScanner.Create(FInputStream);
  FreeAndNil(FParser);
  FParser := TWITParser.Create(FScanner);
end;

procedure TTestWITParser.SetUp;
begin
  inherited SetUp;
  FreeAndNil(FDocument);   // Freeing nil is safe
  FreeAndNil(FParser);
  FreeAndNil(FScanner);    // Assuming scanner does not own the stream, or handles it.
  FreeAndNil(FInputStream);
end;

procedure TTestWITParser.TearDown;
begin
  FreeAndNil(FDocument);   // Freeing nil is safe
  FreeAndNil(FParser);
  FreeAndNil(FScanner);    // Assuming scanner does not own the stream, or handles it.
  FreeAndNil(FInputStream);
  inherited TearDown;
end;

function TTestWITParser.WrapTypeDef(const aDef: String; isType: Boolean): string;

const
  WIT_CONTENT =
    'interface types {' + sLineBreak +
    '  %s' + sLineBreak +
    '}';

begin
  Result:=aDef;
  if isType then
    Result:='type a = '+Result+';';
  Result:=Format(WIT_CONTENT,[Result]);
end;

function TTestWITParser.WrapFunc(const aParams: String; aResult: string): string;
const
  WIT_CONTENT =
    'interface funcs {' + sLineBreak +
    '  %s' + sLineBreak +
    '}';
var
  lWIT : String;
begin
  lWIT:='a : func ('+aParams+')';
  if (aResult<>'') then
    lWIT:=lWIT+' -> '+aResult;
  lWIT:=lWIT+';';
  Result:=Format(WIT_CONTENT,[lWIT]);
end;

class procedure TTestWITParser.AssertEquals(Msg: string; aExpected, aActual: TWitTypeKind);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TWitTypeKind),ord(aExpected)),
                   GetEnumName(TypeInfo(TWitTypeKind),ord(aActual)));
end;

class procedure TTestWITParser.AssertUsePath(Msg: string; aUse: TWITUsePath; const aPackageName, aVersion, aIdentifier: string;
  aNameSpaces: array of string);
var
  I : Integer;
begin
  AssertEquals(Msg+': PackageName',aPackageName,aUse.PackageName);
  AssertEquals(Msg+': Version',aVersion,aUse.Version);
  AssertEquals(Msg+': Identifier',aIdentifier,aUse.Identifier);
  AssertEquals(Msg+': Namespace count',Length(aNameSpaces),aUse.Namespaces.Count);
  For I:=0 to Length(aNamespaces)-1 do
    AssertEquals(Msg+Format(': namespace[%d]',[i]),aNameSpaces[i],aUse.Namespaces[i]);
end;

class procedure TTestWITParser.AssertUse(Msg: string; aUse: TWITTopLevelUse; const aPackageName, aVersion, aIdentifier, aRename: string;
  aNameSpaces: array of string);

begin
  AssertNotNull(Msg+': Have use',aUse);
  AssertUsePath(Msg+': Path',aUse.Path,aPackageName,aVersion,aIdentifier,aNameSpaces);
  AssertEquals(Msg+': Rename',aRename,aUse.Rename);
end;

class procedure TTestWITParser.AssertAnnotationArgument(const Msg : String; aArgument : TWITAnnotationArgument; aName,aValue : string);

begin
  AssertNotNull(Msg+': Have argument',aArgument);
  AssertEquals(Msg+': name',aName,aArgument.Member);
  AssertEquals(Msg+': value',aValue,aArgument.Value);
end;

class procedure TTestWITParser.AssertAnnotation(const Msg: String; aAnnotation: TWITAnnotation;const aName: String;
  Args: array of String);
var
  I : Integer;
begin
  AssertNotNull(Msg+': Have annotation',aAnnotation);
  AssertEquals(Msg+': name',aName,aAnnotation.Name);
  AssertEquals(Msg+': Arg count',aAnnotation.Arguments.Count,Length(Args) div 2);
  I:=0;
  While (I<Length(Args)) do
   begin
   AssertAnnotationArgument(Msg+Format('Annotation[%d]',[i]),aAnnotation.Arguments[I div 2],Args[i],Args[i+1]);
   Inc(I,2);
   end;
end;

class procedure TTestWITParser.AssertInclude(const aMsg: string; aInclude: TWITInclude; const aName: String; aItemNames,
  aItemAliases: array of String);
begin
  AssertNotNull(aMsg+': have include',aInclude);
  AssertNotNull(aMsg+': have include path',aInclude.Path);
  AssertNotNull(aMsg+': items',aInclude.Items);
  AssertEquals(aMsg+': have path',aName,aInclude.Path.ToString);
  AssertEquals(aMsg+': item count',Length(aItemNames),aInclude.Items.Count);
end;

class procedure TTestWITParser.AssertFunction(const Msg: String; aFunc: TWITFunction; const aName: String; aArgCount: Integer;
  aHaveResult: Boolean; aAnnotationCount: Integer);

begin
  AssertNotNull(Msg+': Have function',aFunc);
  AssertEquals(Msg+': name',aName,aFunc.Name);
  AssertNotNull(Msg+': Type',aFunc.TypeDef);
  AssertEquals(Msg+': Argument count',aArgCount,aFunc.TypeDef.Parameters.Count);
  AssertEquals(Msg+': Annotation count',aArgCount,aFunc.Annotations.Count);
  if aHaveResult then
    AssertNotNull(Msg+': Have Result',aFunc.TypeDef.ResultType)
  else
    AssertNull(Msg+': Have no Result',aFunc.TypeDef.ResultType);
end;

class procedure TTestWITParser.AssertFunctionParam(const Msg: String; aParam: TWITFuncParam;const aName: String;
  aTypeKind: TWITTypeKind; aTypeName: string);
begin
  AssertNotNull(Msg+': Have param',aParam);
  AssertEquals(Msg+': param name',aName,aParam.Name);
  AssertEquals(Msg+': param Type kind',aTypeKind,aParam.ParamType.Kind);
end;

class procedure TTestWITParser.AssertInterface(const Msg: String; aIntf: TWITInterface;const aName: String;
  aFuncCount: Integer; aTypeCount: Integer; aAnnotationCount: Integer);
begin
  AssertNotNull(Msg+': Have Interface',aIntf);
  AssertEquals(Msg+': name',aName,aIntf.Name);
  AssertEquals(Msg+': function count',aFuncCount,aIntf.Functions.Count);
  AssertEquals(Msg+': Type count',aTypeCount,aIntf.Types.Count);
  AssertEquals(Msg+': Annotation count',aAnnotationCount,aIntf.Annotations.Count);
end;

class procedure TTestWITParser.AssertWorld(const Msg: String; aWorld: TWITWorld; const aWorldName: String; aExportCount,
  aImportCount, aUseCount, aTypeDefCount, aIncludeCount: Integer);
begin
  AssertNotNull(Msg+': Have world',aWorld);
  AssertEquals(Msg+': name',aWorldName,aWorld.Name);
  AssertEquals(Msg+': export count',aExportCount,aWorld.Exported.Count);
  AssertEquals(Msg+': import count',aImportCount,aWorld.Imported.Count);
  AssertEquals(Msg+': use count',aUseCount,aWorld.UsesList.Count);
  AssertEquals(Msg+': type count',aTypeDefCount,aWorld.TypeDefs.Count);
  AssertEquals(Msg+': include count',aIncludeCount,aWorld.Includes.Count);
end;

class procedure TTestWITParser.AssertPackage(
    const Msg: String;
    aPackage: TWITPackage;
    aExpectedNamespace: string;
    aExpectedPackageName: string;
    aExpectedVersion: string;
    aExpectedWorldCount: Integer = 0;
    aExpectedImportCount: Integer = 0;
    aExpectedExportCount: Integer = 0;
    aExpectedUseCount: Integer = 0;
    aExpectedInterfaceCount: Integer = 0
  );
begin
  AssertNotNull(Msg + ': Package object should exist', aPackage);
  AssertEquals(Msg + ': Namespace', aExpectedNamespace, aPackage.Namespace);
  AssertEquals(Msg + ': PackageName', aExpectedPackageName, aPackage.PackageName);
  AssertEquals(Msg + ': Version', aExpectedVersion, aPackage.Version);

  // Check that list objects themselves are created (common practice in constructors)
  AssertNotNull(Msg + ': ImportList object should exist', aPackage.ImportList);
  AssertEquals(Msg + ': ImportList count', aExpectedImportCount, aPackage.ImportList.Count);

  AssertNotNull(Msg + ': ExportList object should exist', aPackage.ExportList);
  AssertEquals(Msg + ': ExportList count', aExpectedExportCount, aPackage.ExportList.Count);

  AssertNotNull(Msg + ': UseStatements object should exist', aPackage.UseStatements);
  AssertEquals(Msg + ': UseStatements count', aExpectedUseCount, aPackage.UseStatements.Count);

  AssertNotNull(Msg + ': Interfaces list object should exist', aPackage.Interfaces);
  AssertEquals(Msg + ': Interfaces count', aExpectedInterfaceCount, aPackage.Interfaces.Count);
end;

function TTestWITParser.ParseWorld(const aWorldName: String; aExportCount, aImportCount, aUseCount, aTypeDefCount,
  aIncludeCount: Integer): TWITWorld;
begin
  FDocument := FParser.ParseDocument;
  AssertNotNull('Have Document', FDocument);
  // Assert Package Details
  AssertNotNull('Have Package.', FDocument.DefaultPackage);
  AssertEquals('Have interface', 1, FDocument.DefaultPackage.Worlds.Count);
  Result := FDocument.DefaultPackage.Worlds[0];
  AssertWorld('World def', Result, aWorldName, aExportCount, aImportCount, aUseCount, aTypeDefCount, aIncludeCount);

end;

function TTestWITParser.ParseFunc(const aFuncName: String; aArgNames: array of string; aArgTypes: array of TWitTypeKind;
  aResultType: TWitTypeKind): TWITFunction;
var
  LInterface: TWITInterface;
  i : Integer;
  lParam : TWITFuncParam;
begin
  LInterface:=ParseInterface('funcs',1,0,0);
  AssertEquals('Have function',TWITFunction,LInterface.Functions[0].ClassType);
  Result:=LInterface.Functions[0];
  AssertEquals('function name',aFuncName,Result.Name);
  AssertNotNull('Function typedef',Result.TypeDef);
  AssertNotNull('Function params',Result.TypeDef.Parameters);
  AssertEquals('Args count',Length(aArgNames),Result.TypeDef.Parameters.Count);
  for I:=0 to Length(aArgNames)-1 do
    begin
    lParam:=Result.TypeDef.Parameters[i];
    AssertNotNull('Function param '+IntTostr(i),lParam);
    AssertEquals('Function param name'+IntTostr(i),aArgNames[i],lParam.Name);
    AssertNotNull('Have Function param type '+IntTostr(i),lParam.ParamType);
    AssertEquals('Function param type kind '+IntTostr(i),aArgTypes[i],lParam.ParamType.Kind);
    end;
  if aResultType=wtVoid then
    AssertNull('No result type',Result.TypeDef.ResultType)
  else
    begin
    AssertNotNull('have result type',Result.TypeDef.ResultType);
    AssertEquals('result type kind',aResultType,Result.TypeDef.ResultType.Kind)
    end;
end;

class function TTestWITParser.AssertResultType(const aMsg: string; aType: TWITTypeDef;const aName: String; aOKKind: TWITTypeKind;
  aErrorKind: TWitTypeKind; aIgnore: TResultIgnores): TWITResultType;

var
  lRes : TWITResultType;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITResultType,aType.TypeDef.ClassType);
  lRes:=aType.TypeDef as TWITResultType;
  if riResult in aIgnore then
    AssertNull(aMsg+': no OK type',lRes.OkType)
  else
    begin
    AssertNotNull(aMsg+': OK type',lRes.OkType);
    AssertEquals(aMsg+': OK type kind',aOKKind,lRes.OkType.Kind);
    end;
  if riError in aIgnore then
    AssertNull(aMsg+': no Error type',lRes.ErrorType)
  else
    begin
    AssertNotNull(aMsg+': Error type',lRes.ErrorType);
    AssertEquals(aMsg+': Error type kind',aErrorKind,lRes.ErrorType.Kind);
    end;
  Result:=lRes;
end;

class function TTestWITParser.AssertOptionType(const aMsg: string; aType: TWITTypeDef;const aName: String; aOptionKind: TWITTypeKind
  ): TWITOptionType;
var
  lOpt : TWITOptionType;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITOptionType,aType.TypeDef.ClassType);
  lopt:=aType.TypeDef as TWITOptionType;
  AssertNotNull(aMsg+': item type',lOpt.ItemType);
  AssertEquals(aMsg+': type kind',aOptionKind,lOpt.ItemType.Kind);
  Result:=lOpt;
end;

class function TTestWITParser.AssertStreamType(const aMsg: string; aType: TWITTypeDef;const aName: String; aStreamKind: TWITTypeKind
  ): TWITStreamType;
var
  lStream : TWITStreamType;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITStreamType,aType.TypeDef.ClassType);
  lStream:=aType.TypeDef as TWITStreamType;
  AssertNotNull(aMsg+': item type',lStream.ItemType);
  AssertEquals(aMsg+': type kind',aStreamKind,lStream.ItemType.Kind);
  Result:=lStream;
end;

class function TTestWITParser.AssertFutureType(const aMsg: string; aType: TWITTypeDef;const aName: String; aFutureKind: TWITTypeKind
  ): TWITFutureType;
var
  lFuture : TWITFutureType;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITFutureType,aType.TypeDef.ClassType);
  lFuture:=aType.TypeDef as TWitFutureType;
  AssertNotNull(aMsg+': item type',lFuture.ItemType);
  AssertEquals(aMsg+': type kind',aFutureKind,lFuture.ItemType.Kind);
  Result:=lFuture;
end;

class function TTestWITParser.AssertTupleType(const aMsg: string; aType: TWITTypeDef;const aName: String;
  aOptionKind: array of TWITTypeKind): TWITTupleType;
var
  lTuple : TWITTupleType;
  I : Integer;
  S : String;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITTupleType,aType.TypeDef.ClassType);
  lTuple:=aType.TypeDef as TWITTupleType;
  AssertEquals(aMsg+': have correct count',Length(aOptionKind),lTuple.Items.Count);
  For I:=0 to Length(aOptionKind)-1 do
    begin
    S:=Format(': item[%d]',[i]);
    AssertNotNull(aMsg+S+' type ',lTuple.Items[i]);
    AssertEquals(aMsg+S+' kind',aOptionKind[i],lTuple.Items[i].Kind);
    end;
  Result:=lTuple;
end;

class function TTestWITParser.AssertFlagsType(const aMsg: string; aType: TWITTypeDef;const aName: String; aFlagNames: array of String
  ): TWITFlagsType;
var
  lFlags : TWITFlagsType;
  I : Integer;
  S : String;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITFlagsType,aType.TypeDef.ClassType);
  lFlags:=aType.TypeDef as TWITFlagsType;
  AssertEquals(aMsg+': have correct count',Length(aFlagNames),lFlags.Flags.Count);
  For I:=0 to Length(aFlagNames)-1 do
    begin
    S:=Format(': item[%d]',[i]);
    AssertEquals(aMsg+S+' name',aFlagNames[i],lFlags.Flags[i]);
    end;
  Result:=lFlags;
end;

class function TTestWITParser.AssertVariantType(const aMsg: string; aType: TWITTypeDef;const aName: String; aVariantNames: array of String
  ): TWITVariantType;
var
  lVariant : TWITVariantType;
  I : Integer;
  S : String;

begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITVariantType,aType.TypeDef.ClassType);
  lVariant:=aType.TypeDef as TWITVariantType;
  AssertEquals(aMsg+': have correct count',Length(aVariantNames),lVariant.Cases.Count);
  For I:=0 to Length(aVariantNames)-1 do
    begin
    S:=Format(': item[%d]',[i]);
    AssertEquals(aMsg+S+' name',aVariantNames[i],lVariant.Cases[i].Name);
    end;
  Result:=lVariant;
end;

class function TTestWITParser.AssertRecordType(const aMsg: string; aType: TWITTypeDef;const aName: String; aFieldNames: array of String;
  aFieldTypes: array of TWITTypeKind): TWITRecordType;
var
  lRecord: TWITRecordType;
  I : Integer;
  S : String;

begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITRecordType,aType.TypeDef.ClassType);
  lRecord:=aType.TypeDef as TWITRecordType;
  AssertEquals(aMsg+': have correct count',Length(aFieldNames),lRecord.Fields.Count);
  For I:=0 to Length(aFieldNames)-1 do
    begin
    S:=Format(': field[%d]',[i]);
    AssertEquals(aMsg+S+' name',aFieldNames[i],lRecord.Fields[i].Name);
    AssertNotNull(aMsg+S+' type',lRecord.Fields[i].FieldType);
    AssertEquals(aMsg+S+' kind',aFieldTypes[i],lRecord.Fields[i].FieldType.Kind);
    end;
  Result:=lRecord;
end;

class function TTestWITParser.AssertResourceType(const aMsg: string; aType: TWITTypeDef;const aName: String; aConstructor: boolean;
  aFunctionNames: array of String): TWITResourceType;
var
  lResource: TWITResourceType;
  I : Integer;
  S : String;
  lHaveConstructor : Boolean;
begin
  lHaveConstructor:=False;
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITResourceType,aType.TypeDef.ClassType);
  lResource:=aType.TypeDef as TWITResourceType;
  AssertEquals(aMsg+': have correct count',Length(aFunctionNames),lResource.Functions.Count);
  For I:=0 to Length(aFunctionNames)-1 do
    begin
    S:=Format(': function[%d]',[i]);
    AssertEquals(aMsg+S+' name',aFunctionNames[i],lResource.Functions[i].Name);
    if not lHaveConstructor then
      begin
      lHaveConstructor:=ffConstructor in lResource.Functions[i].TypeDef.Flags;
      if lHaveConstructor then
        AssertEquals(aMsg+': have name',aName,lResource.Functions[i].Name);
      end;
    end;
  Result:=lResource;
end;

class function TTestWITParser.AssertAliasType(const aMsg: string; aType: TWITTypeDef;const aName: String; aAliasName: String
  ): TWITIdentifierType;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITIdentifierType,aType.TypeDef.ClassType);
  Result:=aType.TypeDef as TWITIdentifierType;
  AssertEquals('Alias name ',aAliasName,Result.Name);
end;

class function TTestWITParser.AssertHandleType(const aMsg: string; aType: TWITTypeDef; const aName: String; aAliasName: String
  ): TWITHandleType;
begin
  AssertNotNull(aMsg+': have type',aType);
  AssertEquals(aMsg+': have name',aName,aType.Name);
  AssertNotNull(aMsg+': have typedef',aType.TypeDef);
  AssertEquals(aMsg+': have typedef',TWITHandleType,aType.TypeDef.ClassType);
  Result:=aType.TypeDef as TWITHandleType;
  AssertEquals('Alias name ',aAliasName,Result.Name);
end;

class function TTestWITParser.AssertTypeDef(const Msg: String; aType: TWITType; aExpectedName: string;
  aExpectedUnderlyingKind: TWITTypeKind): TWITType;
var
  lTypeDef : TWITTypeDef absolute aType;
begin
  AssertNotNull(Msg + ': Have Type', aType);
  AssertEquals(Msg + ': Type is TypeDef', TWITTypeDef, aType.ClassType);
  AssertNotNull(Msg + ': Have Type.TypeDef', lTypeDef.Typedef);
  AssertEquals(Msg + ': Type alias name', aExpectedName, lTypeDef.Name);
  AssertEquals(Msg + ': Underlying type kind of alias', aExpectedUnderlyingKind, lTypeDef.Kind);
  Result:=lTypeDef.TypeDef;
end;


class function TTestWITParser.AssertListType(const aMsg: string; aTypeDef: TWitTypeDef;const aName: String; aElementKind: TWITTypeKind;
  aCount: Integer): TWitType;
var
  lListDef : TWITListType;
begin
  AssertEquals(aMsg+'type name',aName,aTypeDef.Name);
  AssertEquals(aMsg+'List type',wtList,aTypeDef.Kind);
  AssertEquals(aMsg+'Typedef class',TWITListType,aTypeDef.TypeDef.ClassType);
  lListDef:=aTypeDef.TypeDef as TWITListType;
  AssertEquals(aMsg+'List element type',aElementKind,lListDef.ItemType.Kind);
  AssertEquals(aMsg+'List element count',aCount,lListDef.ItemCount);
  Result:=lListDef.ItemType;
end;

class function TTestWITParser.AssertEnumType(const aMsg: String; aType: TWITTypeDef;const aName: String; aValues: array of string
  ): TWITEnumType;

var
  lEnum : TWITEnumType;
  I : integer;

begin
  AssertTypeDef(aMsg,aType,aName,wtEnum);
  AssertEquals(aMsg+': name',aName,aType.Name);
  AssertEquals(aMsg+': type',TWITEnumType,aType.TypeDef.ClassType);
  lEnum:=aType.TypeDef as TWITEnumType;
  AssertEquals(aMsg+': case count',Length(aValues),lEnum.Cases.Count);
  For I:=0 to Length(aValues)-1 do
    AssertEquals(aMsg+': case '+IntToStr(i),aValues[i],lEnum.Cases[i]);
  Result:=lEnum;
end;


{ ---------------------------------------------------------------------
  Parsing aids
  ---------------------------------------------------------------------}

function TTestWITParser.ParseInterface(const aName : String; aFunctionCount, aTypeCount, aAnnotationCount : integer) : TWITInterface;

begin
  FDocument := FParser.ParseDocument;
  AssertNotNull('Have Document', FDocument);
  // Assert Package Details
  AssertNotNull('Have Package.', FDocument.DefaultPackage);
  AssertEquals('Have interface', 1, FDocument.DefaultPackage.Interfaces.Count);
  Result := FDocument.DefaultPackage.Interfaces[0];
  AssertInterface('Interface def', Result, aName, aFunctionCount,aTYpeCount, aAnnotationCount);
end;

function TTestWITParser.ParseType(const aInterFaceName,aTypeName: String): TWITTypeDef;
var
  LInterface: TWITInterface;
begin
  LInterface:=ParseInterface(aInterfaceName,0,1,0);
  AssertEquals('Have Type',TWITTypeDef,LInterface.Types[0].ClassType);
  Result:=LInterface.Types[0] as TWITTypeDef;
  AssertEquals('type name',aTypeName,Result.Name);
end;

{ ---------------------------------------------------------------------
  Actual tests
  ---------------------------------------------------------------------}



procedure TTestWITParser.TestParseExitInterfaceDocument;
const
  WIT_CONTENT =
    '@since(version = 0.2.0)' + sLineBreak +
    'interface exit {' + sLineBreak +
    '  @since(version = 0.2.0)' + sLineBreak +
    '  exit: func(status: result);' + sLineBreak +
    sLineBreak +
    '  @unstable(feature = cli-exit-with-code)' + sLineBreak +
    '  exit-with-code: func(status-code: u8);' + sLineBreak +
    '}';

var
  LInterface: TWITInterface;
  LFunc: TWITFunction;
  LParam: TWITFuncParam;
  LParamType: TWITType;
begin
  InitParser(WIT_CONTENT);
  AssertNotNull('Parser should be created.', FParser);
  FDocument := FParser.ParseDocument;
  AssertNotNull('ParseDocument should return a valid TWITDocument.', FDocument);

  // Document should contain one interface
  AssertEquals('Document should contain one interface.', 1, FDocument.Interfaces.Count);
  lInterface:=FDocument.Interfaces[0];
  AssertInterface('Exit interface',lInterface,'exit',2,0,1);
  AssertAnnotation('Intf annotation',LInterface.Annotations[0],'since',['version','0.2.0']);

  // --- Test Function 0: "exit" ---
  lFunc:=LInterface.Functions[0];
  AssertFunction('First exit func',lFunc,'exit',1,False,1);
  AssertAnnotation('First exit func annotation',lFunc.Annotations[0],'since',['version','0.2.0']);

  LParam := LFunc.TypeDef.Parameters[0];
  AssertFunctionParam('Parameter 0 of function "exit"',LParam,'status',wtResult,'');
  lParamType:=lParam.ParamType;
  AssertEquals('Parameter status type',TWITResultType,LParamType.ClassType);
  AssertNull('OkType for shorthand "result" should be nil or an empty type representation.', (LParamType as TWITResultType).OkType);
  AssertNull('ErrorType for shorthand "result" should be nil or an empty type representation.', (LParamType as TWITResultType).ErrorType);

  // --- Test Function 1: "exit-with-code" ---
  LFunc := LInterface.Functions[1];
  AssertFunction('Second exit func',lFunc,'exit-with-code',1,False,1);
  AssertAnnotation('Second exit func annotation',lFunc.Annotations[0],'unstable',['feature','cli-exit-with-code']);

  LParam := LFunc.TypeDef.Parameters[0];
  AssertFunctionParam('Parameter 0 of function "exit-with-code"',LParam,'status-code',wtU8,'');
end;

procedure TTestWITParser.TestSimpleTypes;

const
  WIT_CONTENT =
    'package foo:types;' + sLineBreak +
    sLineBreak +
    'interface types {' + sLineBreak +
    '  type t1 = u8;' + sLineBreak +
    '  type t2 = u16;' + sLineBreak +
    '  type t3 = u32;' + sLineBreak +
    '  type t4 = u64;' + sLineBreak +
    '  type t5 = s8;' + sLineBreak +
    '  type t6 = s16;' + sLineBreak +
    '  type t7 = s32;' + sLineBreak +
    '  type t8 = s64;' + sLineBreak +
    '  type t9a = f32;' + sLineBreak +
    '  type t9b = f32;' + sLineBreak + // Duplicate type kind, different name
    '  type t10a = f64;' + sLineBreak +
    '  type t10b = f64;' + sLineBreak + // Duplicate type kind, different name
    '  type t11 = char;' + sLineBreak + // Assuming char maps to wtU32
    '  type t12 = string;' + sLineBreak +
    '}';
var
  LInterface: TWITInterface;
begin
  InitParser(WIT_CONTENT);
  LInterface := ParseInterface('types',0,14,0);
  AssertTypeDef('Type t1 = u8', LInterface.Types[0], 't1', wtU8);
  AssertTypeDef('Type t2 = u16', LInterface.Types[1], 't2', wtU16);
  AssertTypeDef('Type t3 = u32', LInterface.Types[2], 't3', wtU32);
  AssertTypeDef('Type t4 = u64', LInterface.Types[3], 't4', wtU64);
  AssertTypeDef('Type t5 = s8', LInterface.Types[4], 't5', wtS8);
  AssertTypeDef('Type t6 = s16', LInterface.Types[5], 't6', wtS16);
  AssertTypeDef('Type t7 = s32', LInterface.Types[6], 't7', wtS32);
  AssertTypeDef('Type t8 = s64', LInterface.Types[7], 't8', wtS64);
  AssertTypeDef('Type t9a = f32', LInterface.Types[8], 't9a', wtFloat32);
  AssertTypeDef('Type t9b = f32', LInterface.Types[9], 't9b', wtFloat32);
  AssertTypeDef('Type t10a = f64', LInterface.Types[10], 't10a', wtFloat64);
  AssertTypeDef('Type t10b = f64', LInterface.Types[11], 't10b', wtFloat64);
  AssertTypeDef('Type t11 = char', LInterface.Types[12], 't11', wtChar);
  AssertTypeDef('Type t12 = string', LInterface.Types[13], 't12', wtString);
end;

procedure TTestWITParser.TestListType;

const
  WIT_CONTENT = 'list<char>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT, true));
  lTypeDef:=ParseType('types','a');
  AssertListType('List',lTypeDef,'a',wtChar);
end;


procedure TTestWITParser.TestListListType;
const
  WIT_CONTENT = 'list<list<list<t32>>>';
var
  lTypeDef : TWITTypeDef;
  lItem : TWITType;
  lList : TWITListType absolute litem;
  lIdent : TWITIdentifierType;

begin
  InitParser(WrapTypeDef(WIT_CONTENT, true));
  lTypeDef:=ParseType('types','a');
  lItem:=AssertListType('List',lTypeDef,'a',wtList);
  AssertEquals('Item is list class',TWITListType,lItem.ClassType);
  AssertEquals('Item.Item is list class',TWITListType,lList.ItemType.ClassType);
  lItem:=lList.ItemType;
  AssertEquals('Item.Item.Item is identifier class',TWITIdentifierType,lList.ItemType.ClassType);
  lIdent:=lList.ItemType as TWITIdentifierType;
  AssertEquals('Item.Item.Item  name','t32',lIdent.Name);
end;

procedure TTestWITParser.TestListSized;

const
  WIT_CONTENT = 'list<u32, 4>';

var
  lTypeDef : TWITTypeDef;

begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  lTypeDef:=ParseType('types','a');
  AssertListType('List',lTypeDef,'a',wtU32,4);
end;

procedure TTestWITParser.TestListSizedListSized;

const
  WIT_CONTENT = 'list<list<u32, 4>, 2>';

var
  lTypeDef : TWITTypeDef;
  lItem : TWITType;
  lList : TWITListType absolute lItem;

begin
  InitParser(WrapTypeDef(WIT_CONTENT, true));
  lTypeDef:=ParseType('types','a');
  lItem:=AssertListType('List',lTypeDef,'a',wtList,2);
  AssertEquals('Item class',TWITListType,lItem.ClassType);
  AssertEquals('List list item',wtu32,lList.ItemType.Kind);
  AssertEquals('List list item count',4,lList.ItemCount);
end;


procedure TTestWITParser.TestEnum;
const
  WIT_CONTENT = 'enum a {one,two,three}';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  lTypeDef:=ParseType('types','a');
  AssertEnumType('Enum type',lTypeDef,'a',['one','two','three']);
end;

procedure TTestWITParser.TestEnumEndWithComma;
const
  WIT_CONTENT = 'enum a {one,two,three,}';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  lTypeDef:=ParseType('types','a');
  AssertEnumType('Enum type',lTypeDef,'a',['one','two','three']);
end;



procedure TTestWITParser.TestResultEmpty;
const
  WIT_CONTENT = 'result';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  lTypeDef:=ParseType('types','a');
  AssertResultType('Result type',LTypeDef,'a',wtu32,wtu8,[riResult,riError]);
end;

procedure TTestWITParser.TestResultOneType;
const
  WIT_CONTENT = 'result<u32>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  lTypeDef:=ParseType('types','a');
  AssertResultType('Result type',LTypeDef,'a',wtu32,wtu8,[riError]);
end;

procedure TTestWITParser.TestResultTwoTypes;
const
  WIT_CONTENT = 'result<u32,u8>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  lTypeDef:=ParseType('types','a');
  AssertResultType('Result type',LTypeDef,'a',wtu32,wtu8,[]);
end;

procedure TTestWITParser.TestResultOneIgnoredTyoe;
const
  WIT_CONTENT = 'result<_,u32>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  lTypeDef:=ParseType('types','a');
  AssertResultType('Result type',LTypeDef,'a',wtu32,wtu32,[riResult]);
end;

procedure TTestWITParser.TestOption;
const
  WIT_CONTENT = 'option<u32>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertOptionType('Option type',LTypeDef,'a',wtu32);
end;

procedure TTestWITParser.TestStream;
const
  WIT_CONTENT = 'stream<u32>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertStreamType('Stream type',LTypeDef,'a',wtu32);
end;

procedure TTestWITParser.TestStreamEmpty;
const
  WIT_CONTENT = 'stream';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertStreamType('Stream type',LTypeDef,'a',wtVoid);
end;

procedure TTestWITParser.TestFuture;
const
  WIT_CONTENT = 'future<u32>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertFutureType('Future type',LTypeDef,'a',wtu32);
end;


procedure TTestWITParser.TestNestedFuture;
const
  WIT_CONTENT = 'option<stream<future>>';
var
  lTypeDef : TWITTypeDef;
  lOpt : TWITOptionType;
  lStream : TWITStreamType;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  lOpt:=AssertOptionType('Option type',LTypeDef,'a',wtStream);
  AssertEquals('Stream type',TWITStreamType,lOpt.ItemType.ClassType);
  lStream:=lOpt.ItemType as TWITStreamType;
  AssertEquals('Future type',TWITFutureType,lStream.ItemType.Classtype);
end;

procedure TTestWITParser.TestFutureEmpty;
const
  WIT_CONTENT = 'future';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertFutureType('Future type',LTypeDef,'a',wtVoid);
end;

procedure TTestWITParser.TestTupleEmpty;
const
  WIT_CONTENT = 'tuple<>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertTupleType('Tuple type',LTypeDef,'a',[]);
end;

procedure TTestWITParser.TestTuple1;
const
  WIT_CONTENT = 'tuple<u32>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertTupleType('Tuple type',LTypeDef,'a',[wtu32]);
end;

procedure TTestWITParser.TestTuple2;
const
  WIT_CONTENT = 'tuple<u32, u64>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertTupleType('Tuple type',LTypeDef,'a',[wtu32,wtu64]);
end;

procedure TTestWITParser.TestTuple3;
const
  WIT_CONTENT = 'tuple<u32, u64, u8>';
var
  lTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertTupleType('Tuple type',LTypeDef,'a',[wtu32,wtu64,wtu8]);
end;

procedure TTestWITParser.TestTupleComma;
const
  WIT_CONTENT = 'tuple<u32,>';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertTupleType('Tuple type',LTypeDef,'a',[wtU32]);
end;

procedure TTestWITParser.TestFlagsEmpty;
const
  WIT_CONTENT = 'flags a {}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertFlagsType('Tuple type',LTypeDef,'a',[]);
end;

procedure TTestWITParser.TestFlags1;
const
  WIT_CONTENT = 'flags a {a}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertFlagsType('Tuple type',LTypeDef,'a',['a']);
end;

procedure TTestWITParser.TestFlags2;
const
  WIT_CONTENT = 'flags a {a, b}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertFlagsType('Tuple type',LTypeDef,'a',['a','b']);
end;

procedure TTestWITParser.TestFlags3;
const
  WIT_CONTENT = 'flags a {a, b, c}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertFlagsType('Tuple type',LTypeDef,'a',['a','b','c']);
end;

procedure TTestWITParser.TestFlagsComma;
const
  WIT_CONTENT = 'flags a {a, b, c, }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertFlagsType('Tuple type',LTypeDef,'a',['a','b','c']);
end;

procedure TTestWITParser.TestVariant1;
const
  WIT_CONTENT = 'variant a { a }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertVariantType('Variant type',LTypeDef,'a',['a']);
end;

procedure TTestWITParser.TestVariant2;
const
  WIT_CONTENT = 'variant a { a, b }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertVariantType('Variant type',LTypeDef,'a',['a','b']);
end;

procedure TTestWITParser.TestVariant2Comma;
const
  WIT_CONTENT = 'variant a { a, b,  }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertVariantType('Variant type',LTypeDef,'a',['a','b']);
end;

(*
variant t36 { a, b(u32), }
variant t37 { a, b(option<u32>), }
*)

procedure TTestWITParser.TestVariantTypedSimple;
const
  WIT_CONTENT = 'variant a { a, b(u32) }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertVariantType('Variant type',LTypeDef,'a',['a','b']);
end;

procedure TTestWITParser.TestVariantTypedSimpleComma;
const
  WIT_CONTENT = 'variant a { a, b(u32), }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertVariantType('Variant type',LTypeDef,'a',['a','b']);
end;

procedure TTestWITParser.TestVariantTypedComplex;
const
  WIT_CONTENT = 'variant a { a, b(option<u32>) }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertVariantType('Variant type',LTypeDef,'a',['a','b']);
end;

procedure TTestWITParser.TestRecordEmpty;
const
  WIT_CONTENT = 'record a {}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertRecordType('Record type',LTypeDef,'a',[],[]);
end;

procedure TTestWITParser.TestRecord1;
const
  WIT_CONTENT = 'record a { a: u32 }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertRecordType('Record type',LTypeDef,'a',['a'],[wtU32]);
end;

procedure TTestWITParser.TestRecord2;
const
  WIT_CONTENT = 'record a { a: u32, b: u64  }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertRecordType('Record type',LTypeDef,'a',['a','b'],[wtU32,wtU64]);
end;

procedure TTestWITParser.TestRecord2Comma;
const
  WIT_CONTENT = 'record a { a: u32, b: u64,  }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertRecordType('Record type',LTypeDef,'a',['a','b'],[wtU32,wtU64]);
end;

procedure TTestWITParser.TestRecordRecordName;
const
  WIT_CONTENT = 'record %record { a: u32, b: u64,  }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','record');
  AssertRecordType('Record type',LTypeDef,'record',['a','b'],[wtU32,wtU64]);
end;

procedure TTestWITParser.TestAlias;
const
  WIT_CONTENT = 'b';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  AssertAliasType('Alias type',LTypeDef,'a','b');
end;

procedure TTestWITParser.TestBorrowedHandle;
const
  WIT_CONTENT = 'borrow<b>';
var
  LTypeDef : TWITTypeDef;
  lIdent : TWITHandleType;
begin
  InitParser(WrapTypeDef(WIT_CONTENT,True));
  LTypeDef:=ParseType('types','a');
  lIdent:=AssertHandleType(' type',LTypeDef,'a','b');
  AssertTrue('Borrowed',lIdent.Borrowed);
end;

procedure TTestWITParser.TestResourceEmpty;
const
  WIT_CONTENT = 'resource a;';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertResourceType('Resource type',LTypeDef,'a',false, []);
end;

procedure TTestWITParser.TestResourceEmpty2;
const
  WIT_CONTENT = 'resource a {}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertResourceType('Resource type',LTypeDef,'a',false, []);
end;

procedure TTestWITParser.TestResourceConstructor;
const
  WIT_CONTENT = 'resource a { constructor (c:u8); }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertResourceType('Resource type',LTypeDef,'a',true, ['a']);
end;

procedure TTestWITParser.TestResourceOneMethod;
const
  WIT_CONTENT = 'resource a { write : func (c:u8); }';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertResourceType('Resource type',LTypeDef,'a',true, ['write']);
end;

procedure TTestWITParser.TestResourceStaticMethod;
const
  WIT_CONTENT = 'resource a { write : static func (c:u8); }';
var
  LTypeDef : TWITTypeDef;
  lRes : TWITResourceType;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  lRes:=AssertResourceType('Resource type',LTypeDef,'a',true, ['write']);
  AssertTrue('Function marked static',(ffStatic in lRes.Functions[0].TypeDef.Flags));
end;

procedure TTestWITParser.TestResourceAsyncMethod;
const
  WIT_CONTENT = 'resource a { write : async func (c:u8); }';
var
  LTypeDef : TWITTypeDef;
  lRes : TWITResourceType;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  lRes:=AssertResourceType('Resource type',LTypeDef,'a',true, ['write']);
  AssertTrue('Function marked static',(ffAsync in lRes.Functions[0].TypeDef.Flags));
end;

procedure TTestWITParser.TestResourceTwoMethods;
const
  WIT_CONTENT = 'resource a { '+sLineBreak+
                ' read : func (c:u8) -> list<u8>; '+sLineBreak+
                ' write : func (c : list<u8>); '+sLineBreak+
                '}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertResourceType('Resource type',LTypeDef,'a',false, ['read','write']);
end;

procedure TTestWITParser.TestResourceOneMethodAndConstructor;
const
  WIT_CONTENT = 'resource a { '+sLineBreak+
                ' read : func (c:u8) -> list<u8>; '+sLineBreak+
                ' constructor (c : list<u8>); '+sLineBreak+
                '}';
var
  LTypeDef : TWITTypeDef;
begin
  InitParser(WrapTypeDef(WIT_CONTENT));
  LTypeDef:=ParseType('types','a');
  AssertResourceType('Resource type',LTypeDef,'a',true, ['read','a']);
end;

procedure TTestWITParser.TestUseIdentifier;
const
  WIT_CONTENT = 'use a;';
var
  lUse : TWITTopLevelUse;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;
  AssertEquals('Uses count', 1, FDocument.UseStatements.Count);
  lUse:=FDocument.UseStatements[0];
  AssertUse('Simple use',lUse,'','','a','',[]);
end;

procedure TTestWITParser.TestUseIdentifierAs;
const
  WIT_CONTENT = 'use a as b;';
var
  lUse : TWITTopLevelUse;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;
  AssertEquals('Uses count', 1, FDocument.UseStatements.Count);
  lUse:=FDocument.UseStatements[0];
  AssertUse('Simple use',lUse,'','','a','b',[]);
end;

procedure TTestWITParser.TestUseFullIdentifier;
const
  WIT_CONTENT = 'use d:c/a;';
var
  lUse : TWITTopLevelUse;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;
  AssertEquals('Uses count', 1, FDocument.UseStatements.Count);
  lUse:=FDocument.UseStatements[0];
  AssertUse('Full use',lUse,'c','','a','',['d']);
end;

procedure TTestWITParser.TestUseFullIdentifierVersion;
const
  WIT_CONTENT = 'use d:c/a@1.1.1;';
var
  lUse : TWITTopLevelUse;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;
  AssertEquals('Uses count', 1, FDocument.UseStatements.Count);
  lUse:=FDocument.UseStatements[0];
  AssertUse('Full use',lUse,'c','1.1.1','a','',['d']);
end;

procedure TTestWITParser.TestUseFullIdentifierAs;
const
  WIT_CONTENT = 'use d:c/a as b;';
var
  lUse : TWITTopLevelUse;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;
  AssertEquals('Uses count', 1, FDocument.UseStatements.Count);
  lUse:=FDocument.UseStatements[0];
  AssertUse('Full use',lUse,'c','','a','b',['d']);
end;

procedure TTestWITParser.TestUseFullIdentifierVersionAs;
const
  WIT_CONTENT = 'use d:c/a@1.1.1 as b;';
var
  lUse : TWITTopLevelUse;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;
  AssertEquals('Uses count', 1, FDocument.UseStatements.Count);
  lUse:=FDocument.UseStatements[0];
  AssertUse('Full use',lUse,'c','1.1.1','a','b',['d']);
end;

procedure TTestWITParser.TestParseFunctionEmpty;
const
  WIT_CONTENT = '';

begin
  InitParser(WrapFunc(WIT_CONTENT));
  ParseFunc('a',[],[],wtVoid);
end;

procedure TTestWITParser.TestParseFunctionEmptyResult;
const
  WIT_CONTENT = '';

begin
  InitParser(WrapFunc(WIT_CONTENT,'u8'));
  ParseFunc('a',[],[],wtU8);
end;

procedure TTestWITParser.TestParseFunctionOneParam;
const
  WIT_CONTENT = 'b:u8';

begin
  InitParser(WrapFunc(WIT_CONTENT));
  ParseFunc('a',['b'],[wtU8],wtVoid);
end;

procedure TTestWITParser.TestParseFunctionOneParamResult;
const
  WIT_CONTENT = 'b:u8';

begin
  InitParser(WrapFunc(WIT_CONTENT,'u32'));
  ParseFunc('a',['b'],[wtU8],wtU32);
end;

procedure TTestWITParser.TestParseFunctionTwoParams;
const
  WIT_CONTENT = 'b : u8, c : list<u8>';

begin
  InitParser(WrapFunc(WIT_CONTENT));
  ParseFunc('a',['b','c'],[wtU8, wtList],wtVoid);
end;

procedure TTestWITParser.TestParseFunctionTwoParamsResult;
const
  WIT_CONTENT = 'b : u8, c : list<u8>';

begin
  InitParser(WrapFunc(WIT_CONTENT,'result<_,u32>'));
  ParseFunc('a',['b','c'],[wtU8, wtList],wtResult);
end;

procedure TTestWITParser.TestParseWorldEmpty;

const
  WIT_CONTENT = 'world a {}';

begin
  InitParser(WIT_CONTENT);
  ParseWorld('a',0,0,0,0,0);
end;

procedure TTestWITParser.TestParseWorldUse;
const
  WIT_CONTENT = 'world a { use b.{c}; }';

var
  lWorld : TWITWorld;
  lUse : TWITUse;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,1,0,0);
  lUse:=lWorld.UsesList[0];
  AssertEquals('Export name','b',lUse.Path.Identifier);
end;

procedure TTestWITParser.TestParseWorldUseAnnotation;
const
  WIT_CONTENT = 'world a { @since(version = 1.1.1)  use b.{c}; }';

var
  lWorld : TWITWorld;
  lUse : TWITUse;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,1,0,0);
  lUse:=lWorld.UsesList[0];
  AssertEquals('Export name','b',lUse.Path.Identifier);
  AssertEquals('Have annotation',1,lUse.Annotations.Count);
  AssertEquals('since annotation','since',lUse.Annotations[0].Name);
end;

procedure TTestWITParser.TestParseWorldExport;
const
  WIT_CONTENT = 'world a { export b; }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',1,0,0,0,0);
  lExport:=lWorld.Exported[0];
  AssertEquals('Export name','b',lExport.Name);
end;

procedure TTestWITParser.TestParseWorldExportUse;
const
  WIT_CONTENT = 'world a { export b:c/d@3.3.1; }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',1,0,0,0,0);
  lExport:=lWorld.Exported[0];
  AssertEquals('Export name','b:c/d@3.3.1',lExport.Name);
end;

procedure TTestWITParser.TestParseWorldExportFunction;
const
  WIT_CONTENT = 'world a { export b:func (c:u32) ; }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;
  lExportFunc : TWITExchangeFunc absolute lExport;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',1,0,0,0,0);
  lExport:=lWorld.Exported[0];
  AssertEquals('Export name','b',lExport.Name);
  AssertEquals('export class',TWITExchangeFunc,lExport.ClassType);
  AssertNotNull('export typedef',lExportFunc.TypeDef);
end;

procedure TTestWITParser.TestParseWorldExportInterface;
const
  WIT_CONTENT = 'world a { export b:interface { c: func (d:u32) ; } }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;
  lExportIntf : TWITExchangeInterface absolute lExport;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',1,0,0,0,0);
  lExport:=lWorld.Exported[0];
  AssertEquals('Export name','b',lExport.Name);
  AssertEquals('export class',TWITExchangeInterface,lExport.ClassType);
  AssertInterface('counts',lExportIntf.TypeDef,'b',1,0,0)
end;

procedure TTestWITParser.TestParseWorldImport;
const
  WIT_CONTENT = 'world a { import b; }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,1,0,0,0);
  lExport:=lWorld.Imported[0];
  AssertEquals('Import name','b',lExport.Name);
end;

procedure TTestWITParser.TestParseWorldImportUse;
const
  WIT_CONTENT = 'world a { import b:c/d@3.3.1; }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,1,0,0,0);
  lExport:=lWorld.Imported[0];
  AssertEquals('Export name','b:c/d@3.3.1',lExport.Name);
end;

procedure TTestWITParser.TestParseWorldImportFunction;
const
  WIT_CONTENT = 'world a { import b:func (c:u32) ; }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;
  lExportFunc : TWITExchangeFunc absolute lExport;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,1,0,0,0);
  lExport:=lWorld.Imported[0];
  AssertEquals('Export name','b',lExport.Name);
  AssertEquals('export class',TWITExchangeFunc,lExport.ClassType);
  AssertNotNull('export typedef',lExportFunc.TypeDef);
end;

procedure TTestWITParser.TestParseWorldImportInterface;
const
  WIT_CONTENT = 'world a { import b:interface { c: func (d:u32) ; } }';

var
  lWorld : TWITWorld;
  lExport : TWITExchange;
  lExportIntf : TWITExchangeInterface absolute lExport;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,1,0,0,0);
  lExport:=lWorld.Imported[0];
  AssertEquals('Export name','b',lExport.Name);
  AssertEquals('export class',TWITExchangeInterface,lExport.ClassType);
  AssertInterface('counts',lExportIntf.TypeDef,'b',1,0,0)
end;

procedure TTestWITParser.TestParseWorldInclude;
const
  WIT_CONTENT = 'world a { include b; }';

var
  lWorld : TWITWorld;
  lInclude : TWITInclude;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,0,1);
  lInclude:=lWorld.Includes[0];
  AssertInclude('First',lInclude,'b',[],[]);
end;

procedure TTestWITParser.TestParseWorldIncludeUse;
const
  WIT_CONTENT = 'world a { include b:c/d; }';

var
  lWorld : TWITWorld;
  lInclude : TWITInclude;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,0,1);
  lInclude:=lWorld.Includes[0];
  AssertInclude('First',lInclude,'b:c/d',[],[]);
end;

procedure TTestWITParser.TestParseWorldIncludeUseList;
const
  WIT_CONTENT = 'world a { include b:c/d with { e as f } }';

var
  lWorld : TWITWorld;
  lInclude : TWITInclude;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,0,1);
  lInclude:=lWorld.Includes[0];
  AssertInclude('First',lInclude,'b:c/d',['e'],['f']);
end;

procedure TTestWITParser.TestParseWorldIncludeUseList2;
const
  WIT_CONTENT = 'world a { include b:c/d with { e as f, g as h } }';

var
  lWorld : TWITWorld;
  lInclude : TWITInclude;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,0,1);
  lInclude:=lWorld.Includes[0];
  AssertInclude('First',lInclude,'b:c/d',['e','g'],['f','h']);
end;

procedure TTestWITParser.TestParseWorldTypeDef;
const
  WIT_CONTENT = 'world a { type x = u32; }';

var
  lWorld : TWITWorld;
  lType : TWITTypeDef;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,1,0);
  lType:=lWorld.TypeDefs[0];
  AssertTypeDef('type',lType,'x',wtU32);
end;

procedure TTestWITParser.TestParseWorldEnumType;
const
  WIT_CONTENT = 'world a { enum x {y,z} }';

var
  lWorld : TWITWorld;
  lType : TWITTypeDef;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,1,0);
  lType:=lWorld.TypeDefs[0];
  AssertEnumType('type',lType,'x',['y','z']);
end;

procedure TTestWITParser.TestParseWorldVariantType;
const
  WIT_CONTENT = 'world a { variant x { y, z} }';

var
  lWorld : TWITWorld;
  lType : TWITTypeDef;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,1,0);
  lType:=lWorld.TypeDefs[0];
  AssertVariantType('type',lType,'x',['y','z']);
end;

procedure TTestWITParser.TestParseWorldRecordType;
const
  WIT_CONTENT = 'world a { record x { y: u32, z: u8} }';

var
  lWorld : TWITWorld;
  lType : TWITTypeDef;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,1,0);
  lType:=lWorld.TypeDefs[0];
  AssertRecordType('type',lType,'x',['y','z'],[wtu32,wtu8]);
end;

procedure TTestWITParser.TestParseWorldFlagsType;
const
  WIT_CONTENT = 'world a { flags x { y, z} }';

var
  lWorld : TWITWorld;
  lType : TWITTypeDef;

begin
  InitParser(WIT_CONTENT);
  lWorld:=ParseWorld('a',0,0,0,1,0);
  lType:=lWorld.TypeDefs[0];
  AssertFlagsType('type',lType,'x',['y','z']);
end;

procedure TTestWITParser.TestParseInterfaceUse;
const
  WIT_CONTENT = 'interface a { use b.{c}; }';

begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;

  AssertNotNull('Have package', FDocument.DefaultPackage);
  AssertEquals('Interface count.', 1, FDocument.Interfaces.Count);
  AssertEquals('Use count.', 1, FDocument.Interfaces[0].UseList.Count);
end;

procedure TTestWITParser.TestParseInterfaceUseGate;
const
  WIT_CONTENT = 'interface a { @since (version = 1.1.1) use b.{c}; }';

begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;

  AssertNotNull('Have package', FDocument.DefaultPackage);
  AssertEquals('Interface count.', 1, FDocument.Interfaces.Count);
  AssertEquals('Use count.', 1, FDocument.Interfaces[0].UseList.Count);
  AssertEquals('Use annotation count.', 1, FDocument.Interfaces[0].UseList[0].Annotations.Count);
end;

procedure TTestWITParser.TestParsePackageEmpty;
const
  WIT_CONTENT = 'package foo:empty;';
var
  LPackage: TWITPackage;
begin
  InitParser(WIT_CONTENT);
  FDocument := FParser.ParseDocument;

  AssertNotNull('Have package', FDocument.DefaultPackage);
  AssertEquals('Interface count.', 0, FDocument.Interfaces.Count);
  AssertEquals('World count.', 0, FDocument.Worlds.Count);

  LPackage := FDocument.DefaultPackage;
  AssertPackage('Parsed package "foo:empty"', LPackage,
    'foo', 'empty', '', 0, 0, 0, 0, 0);
end;

procedure TTestWITParser.TestParsePackageVersions;
const
  ScenarioCount = 9;
  Scenarios : array [1..ScenarioCount] of string = (
  'package a:b@1.0.0 {}',
  'package a:b@1.0.1 {}',
  'package a:b@1.0.1-- {}',
  'package a:b@1.0.1-a+a {}',
  'package a:b@1.0.1-1+1 {}',
  'package a:b@1.0.1-1a+1a {}',
  'package a:b@1.0.0-11-a {}',
  'package a:b@1.0.0-a1.1-a {}',
  'package a:b@1.0.0-11ab {}'
  );
var
  I,p : Integer;
  lScenario,
  lVersion : string;
  lMessage : String;
begin
  For I:=1 to ScenarioCount do
    begin
    lScenario:=Scenarios[i];
    P:=Pos('@',lScenario);
    lVersion:=Copy(lScenario,P+1,pos('{',lScenario)-2-P);
    lMessage:=Format('Scenario[%d] "%s": ',[i,lScenario]);
    InitParser(lScenario);
    try
      FDocument:=FParser.ParseDocument;
    except
      on E : Exception do
        Fail('Exception %s during scenario %s: "%s"',[E.ClassName,lMessage,E.Message]);
    end;
    AssertEquals('Have package ',1,FDocument.Packages.Count);
    AssertPackage(lMessage,FDocument.Packages[0],'a','b',lVersion)
    end;
end;

initialization
  RegisterTest(TTestWITParser);
end.
