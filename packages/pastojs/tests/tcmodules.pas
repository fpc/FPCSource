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

  TTestEnginePasResolver = class(TPasResolver)
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
    constructor Create;
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

  { TTestModule }

  TTestModule = Class(TTestCase)
  private
    FConverter: TPasToJSConverter;
    FEngine: TTestEnginePasResolver;
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
    FSource: TStringList;
    FFirstPasStatement: TPasImplBlock;
    function GetModuleCount: integer;
    function GetModules(Index: integer): TTestEnginePasResolver;
    function OnPasResolverFindUnit(const aUnitName: String): TPasModule;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure Add(Line: string);
    Procedure StartParsing;
    procedure ParseModule;
    procedure ParseProgram;
    procedure ParseUnit;
  protected
    function FindModuleWithFilename(aFilename: string): TTestEnginePasResolver;
    function AddModule(aFilename: string): TTestEnginePasResolver;
    function AddModuleWithSrc(aFilename, Src: string): TTestEnginePasResolver;
    function AddModuleWithIntfImplSrc(aFilename, InterfaceSrc,
      ImplementationSrc: string): TTestEnginePasResolver;
    procedure AddSystemUnit;
    procedure StartProgram(NeedSystemUnit: boolean);
    procedure StartUnit(NeedSystemUnit: boolean);
    Procedure ConvertModule;
    Procedure ConvertProgram;
    Procedure ConvertUnit;
    procedure CheckDottedIdentifier(Msg: string; El: TJSElement; DottedName: string);
    function GetDottedIdentifier(El: TJSElement): string;
    procedure CheckSource(Msg,Statements, InitStatements: string);
    procedure CheckDiff(Msg, Expected, Actual: string);
    procedure WriteSource(aFilename: string; Row: integer = 0; Col: integer = 0);
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
  public
    property Source: TStringList read FSource;
    property FileResolver: TStreamResolver read FFileResolver;
    property Scanner: TPascalScanner read FScanner;
    property Parser: TTestPasParser read FParser;
  Published
    // modules
    Procedure TestEmptyProgram;
    Procedure TestEmptyProgramUseStrict;
    Procedure TestEmptyUnit;
    Procedure TestEmptyUnitUseStrict;

    // vars/const
    Procedure TestVarInt;
    Procedure TestVarBaseTypes;
    Procedure TestConstBaseTypes;
    Procedure TestUnitImplVars;
    Procedure TestUnitImplConsts;
    Procedure TestUnitImplRecord;
    Procedure TestRenameJSNameConflict;

    // strings
    Procedure TestCharConst;
    Procedure TestChar_Compare;
    Procedure TestStringConst;
    Procedure TestString_Compare;
    Procedure TestString_SetLength;
    Procedure TestString_CharAt;
    // ToDo: TestString: read, write []

    Procedure TestEmptyProc;
    Procedure TestAliasTypeRef;

    // functions
    Procedure TestProcOneParam;
    Procedure TestFunctionWithoutParams;
    Procedure TestProcedureWithoutParams;
    Procedure TestPrgProcVar;
    Procedure TestProcTwoArgs;
    Procedure TestProc_DefaultValue;
    Procedure TestUnitProcVar;
    Procedure TestFunctionResult;
    // ToDo: overloads
    Procedure TestNestedProc;
    Procedure TestForwardProc;
    Procedure TestNestedForwardProc;
    Procedure TestAssignFunctionResult;
    Procedure TestFunctionResultInCondition;
    Procedure TestExit;
    Procedure TestBreak;
    Procedure TestContinue;
    Procedure TestProcedureExternal;
    Procedure TestProcedureAsm;
    Procedure TestProcedureAssembler;

    // ToDo: pass by reference

    Procedure TestEnumName;
    Procedure TestEnumNumber;
    Procedure TestEnumFunctions;
    Procedure TestSet;
    Procedure TestSetOperators;
    Procedure TestSetFunctions;
    // ToDo:  str
    // ToDo: pass set as non const parameter using cloneSet

    // statements
    Procedure TestIncDec;
    Procedure TestAssignments;
    Procedure TestArithmeticOperators1;
    Procedure TestLogicalOperators;
    Procedure TestBitwiseOperators;
    Procedure TestFunctionInt;
    Procedure TestFunctionString;
    Procedure TestVarRecord;
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
    Procedure TestWithRecordDo;

    // arrays
    Procedure TestArray_Dynamic;
    Procedure TestArray_Dynamic_Nil;
    Procedure TestArray_DynMultiDimensional;

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
    Procedure TestClass_Assigned;
    Procedure TestClass_WithClassDoCreate;
    Procedure TestClass_WithClassInstDoProperty;
    Procedure TestClass_WithClassInstDoPropertyWithParams;
    Procedure TestClass_WithClassInstDoFunc;
    // ToDo: overload
    // ToDo: second constructor
    // ToDo: call another constructor within a constructor
    // ToDo: event

    // ToDo: class of
    // ToDo: call classof.classmethod

    // ToDo: procedure type
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

constructor TTestEnginePasResolver.Create;
begin
  inherited Create;
  StoreSrcColumns:=true;
  Options:=Options+DefaultPasResolverOptions;
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

{ TTestModule }

function TTestModule.GetModuleCount: integer;
begin
  Result:=FModules.Count;
end;

function TTestModule.GetModules(Index: integer
  ): TTestEnginePasResolver;
begin
  Result:=TTestEnginePasResolver(FModules[Index]);
end;

function TTestModule.OnPasResolverFindUnit(const aUnitName: String
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
        on E: Exception do
          begin
          writeln('ERROR: TTestModule.OnPasResolverFindUnit during parsing: '+E.ClassName+':'+E.Message
            +' File='+CurEngine.Scanner.CurFilename
            +' LineNo='+IntToStr(CurEngine.Scanner.CurRow)
            +' Col='+IntToStr(CurEngine.Scanner.CurColumn)
            +' Line="'+CurEngine.Scanner.CurLine+'"'
            );
          raise E;
          end;
      end;
      //writeln('TTestModule.OnPasResolverFindUnit END ',CurUnitName);
      Result:=CurEngine.Module;
      exit;
      end;
    end;
  writeln('TTestModule.OnPasResolverFindUnit missing unit "',aUnitName,'"');
  raise Exception.Create('can''t find unit "'+aUnitName+'"');
end;

procedure TTestModule.SetUp;
begin
  inherited SetUp;
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
end;

procedure TTestModule.TearDown;
begin
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

procedure TTestModule.Add(Line: string);
begin
  Source.Add(Line);
end;

procedure TTestModule.StartParsing;
begin
  FileResolver.AddStream(FileName,TStringStream.Create(Source.Text));
  Scanner.OpenFile(FileName);
  Writeln('// Test : ',Self.TestName);
  Writeln(Source.Text);
end;

procedure TTestModule.ParseModule;
var
  Row, Col: integer;
begin
  FFirstPasStatement:=nil;
  try
    StartParsing;
    Parser.ParseMain(FModule);
  except
    on E: EParserError do
      begin
      WriteSource(E.Filename,E.Row,E.Column);
      writeln('ERROR: TTestModule.ParseModule Parser: '+E.ClassName+':'+E.Message
        +' '+E.Filename+'('+IntToStr(E.Row)+','+IntToStr(E.Column)+')'
        +' Line="'+Scanner.CurLine+'"'
        );
      raise E;
      end;
    on E: EPasResolve do
      begin
      Engine.UnmangleSourceLineNumber(E.PasElement.SourceLinenumber,Row,Col);
      WriteSource(E.PasElement.SourceFilename,Row,Col);
      writeln('ERROR: TTestModule.ParseModule PasResolver: '+E.ClassName+':'+E.Message
        +' '+E.PasElement.SourceFilename
        +'('+IntToStr(Row)+','+IntToStr(Col)+')');
      raise E;
      end;
    on E: Exception do
      begin
      writeln('ERROR: TTestModule.ParseModule Exception: '+E.ClassName+':'+E.Message);
      raise E;
      end;
  end;
  AssertNotNull('Module resulted in Module',FModule);
  AssertEquals('modulename',lowercase(ChangeFileExt(FFileName,'')),lowercase(Module.Name));
  TAssert.AssertSame('Has resolver',Engine,Parser.Engine);
end;

procedure TTestModule.ParseProgram;
begin
  ParseModule;
  AssertEquals('Has program',TPasProgram,Module.ClassType);
  FPasProgram:=TPasProgram(Module);
  AssertNotNull('Has program section',PasProgram.ProgramSection);
  AssertNotNull('Has initialization section',PasProgram.InitializationSection);
  if (PasProgram.InitializationSection.Elements.Count>0) then
    if TObject(PasProgram.InitializationSection.Elements[0]) is TPasImplBlock then
      FFirstPasStatement:=TPasImplBlock(PasProgram.InitializationSection.Elements[0]);
end;

procedure TTestModule.ParseUnit;
begin
  ParseModule;
  AssertEquals('Has unit (TPasModule)',TPasModule,Module.ClassType);
  AssertNotNull('Has interface section',Module.InterfaceSection);
  AssertNotNull('Has implementation section',Module.ImplementationSection);
  if (Module.InitializationSection<>nil)
      and (Module.InitializationSection.Elements.Count>0)
      and (TObject(Module.InitializationSection.Elements[0]) is TPasImplBlock) then
    FFirstPasStatement:=TPasImplBlock(Module.InitializationSection.Elements[0]);
end;

function TTestModule.FindModuleWithFilename(aFilename: string
  ): TTestEnginePasResolver;
var
  i: Integer;
begin
  for i:=0 to ModuleCount-1 do
    if CompareText(Modules[i].Filename,aFilename)=0 then
      exit(Modules[i]);
  Result:=nil;
end;

function TTestModule.AddModule(aFilename: string
  ): TTestEnginePasResolver;
begin
  //writeln('TTestModuleConverter.AddModule ',aFilename);
  if FindModuleWithFilename(aFilename)<>nil then
    raise Exception.Create('TTestModuleConverter.AddModule: file "'+aFilename+'" already exists');
  Result:=TTestEnginePasResolver.Create;
  Result.Filename:=aFilename;
  Result.AddObjFPCBuiltInIdentifiers([btChar,btString,btLongint,btInt64,btBoolean,btDouble]);
  Result.OnFindUnit:=@OnPasResolverFindUnit;
  FModules.Add(Result);
end;

function TTestModule.AddModuleWithSrc(aFilename, Src: string
  ): TTestEnginePasResolver;
begin
  Result:=AddModule(aFilename);
  Result.Source:=Src;
end;

function TTestModule.AddModuleWithIntfImplSrc(aFilename, InterfaceSrc,
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

procedure TTestModule.AddSystemUnit;
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

procedure TTestModule.StartProgram(NeedSystemUnit: boolean);
begin
  if NeedSystemUnit then
    AddSystemUnit
  else
    Parser.ImplicitUses.Clear;
  Add('program test1;');
  Add('');
end;

procedure TTestModule.StartUnit(NeedSystemUnit: boolean);
begin
  if NeedSystemUnit then
    AddSystemUnit
  else
    Parser.ImplicitUses.Clear;
  Add('unit Test1;');
  Add('');
end;

procedure TTestModule.ConvertModule;
var
  ModuleNameExpr: TJSLiteral;
  FunDecl, InitFunction: TJSFunctionDeclarationStatement;
  FunDef: TJSFuncDef;
  InitAssign: TJSSimpleAssignStatement;
  FunBody: TJSFunctionBody;
  InitName: String;
begin
  FJSModule:=FConverter.ConvertPasElement(Module,Engine) as TJSSourceElements;
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

procedure TTestModule.ConvertProgram;
begin
  Add('end.');
  ParseProgram;
  ConvertModule;
end;

procedure TTestModule.ConvertUnit;
begin
  Add('end.');
  ParseUnit;
  ConvertModule;
end;

procedure TTestModule.CheckDottedIdentifier(Msg: string; El: TJSElement;
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

function TTestModule.GetDottedIdentifier(El: TJSElement): string;
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

procedure TTestModule.CheckSource(Msg, Statements, InitStatements: string);
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

procedure TTestModule.CheckDiff(Msg, Expected, Actual: string);
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

procedure TTestModule.WriteSource(aFilename: string; Row: integer; Col: integer
  );
var
  LR: TLineReader;
  CurRow: Integer;
  Line: String;
begin
  LR:=FileResolver.FindSourceFile(aFilename);
  writeln('Testcode:-File="',aFilename,'"----------------------------------:');
  if LR=nil then
    writeln('Error: file not loaded: "',aFilename,'"')
  else
    begin
    CurRow:=0;
    while not LR.IsEOF do
      begin
      inc(CurRow);
      Line:=LR.ReadLine;
      if (Row=CurRow) then
        begin
        write('*');
        Line:=LeftStr(Line,Col-1)+'|'+copy(Line,Col,length(Line));
        end;
      writeln(Format('%:4d: ',[CurRow]),Line);
      end;
    end;
end;

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
    'this.d=0;',
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
    'if (this.vA < this.vB) this.vC = this.vA else this.vC = this.vB;'
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
  Add('  FuncA(Bar);');
  Add('end;');
  Add('procedure FuncA(Bar: longint);');
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
    '  if (Bar == 3) {',
    '  };',
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
    '    if (i == 3) {',
    '    };',
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
    'if (this.Func2()) {',
    '};',
    'if (this.i == this.Func1()) {',
    '};',
    'if (this.i == this.Func1()) {',
    '};'
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
    '    { a:{ b:{}, c:[]}, d:''1'' };',
    ';',
    'return Result;',
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
    '    { a:{ b:{}, c:[]}, d:''1'' };',
    ';',
    '};'
    ]),
    LinesToStr([
    ''
    ]));
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
    'this.i = 0;'
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
    'this.s=rtl.cloneSet(this.t);',
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
    'this.vS[this.TColor.Green] = true;',
    'delete this.vS[this.vC];',
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
    '$impl.TMyRecord = function () {',
    '  this.i = 0;',
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
    'rtl.setStringLength(this.s,3);'
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

procedure TTestModule.TestVarRecord;
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
  CheckSource('TestVarRecord',
    LinesToStr([ // statements
    'this.TRecA = function () {',
    '  this.Bold = 0;',
    '};',
    'this.Rec = new this.TRecA();'
    ]),
    LinesToStr([ // this.$main
    'this.Rec.Bold = 123;'
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
    '  if (this.vI==3){} ;'
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
    'vI=2;',
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
  ConvertProgram;
  CheckSource('TestTryExcept',
    LinesToStr([ // statements
    'rtl.createClass(this, "TObject", null, function () {',
    '  this.$init = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "Exception", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '    this.Msg = "";',
    '  };',
    '});',
    'rtl.createClass(this, "EInvalidCast", this.Exception, function () {',
    '  this.$init = function () {',
    '    pas.program.Exception.$init.call(this);',
    '  };',
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
    '} catch ('+DefaultJSExceptionObject+') {',
    '  throw '+DefaultJSExceptionObject+';',
    '};',
    'try {',
    '  this.vI = 4;',
    '} catch ('+DefaultJSExceptionObject+') {',
    '  if (this.EInvalidCast.isPrototypeOf('+DefaultJSExceptionObject+')) throw '+DefaultJSExceptionObject,
    '  else if (this.Exception.isPrototypeOf('+DefaultJSExceptionObject+')) {',
    '    var E = '+DefaultJSExceptionObject+';',
    '    if (E.Msg == "") throw E;',
    '  } else {',
    '    this.vI = 5;',
    '  }',
    '};'
    ]));
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
    'if ($tmp1 == 1) {} else if ($tmp1 == 2) this.vI = 3 else {',
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
    'if (($tmp1 >= 1) && ($tmp1 <= 3)) this.vI = 14 else if (($tmp1 == 4) || ($tmp1 == 5)) this.vI = 16 else if ((($tmp1 >= 6) && ($tmp1 <= 7)) || (($tmp1 >= 9) && ($tmp1 <= 10))) {} else {',
    '};'
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
    'this.TRec = function () {',
    '  this.vI = 0;',
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
    '  this.Create = function () {',
    '  };',
    '});',
    'rtl.createClass(this,"TClassA",this.TObject,function(){',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '  };',
    '});',
    'rtl.createClass(this,"TClassB",this.TObject,function(){',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '  };',
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
    'if (this.TClassA.isPrototypeOf(this.oO)) {',
    '};',
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
    '  this.DoVirtual = function () {',
    '  };',
    '  this.DoIt = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "TA", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '  };',
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
    '  this.DoVirtual = function (pA,pB) {',
    '  };',
    '  this.DoIt = function (pA,pB) {',
    '  };',
    '  this.DoIt2 = function (pA,pB) {',
    '  };',
    '});',
    'rtl.createClass(this, "TClassA", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '  };',
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
    '  this.Create = function () {',
    '  };',
    '  this.CreateWithB = function (b) {',
    '    this.Create();',
    '  };',
    '});',
    'rtl.createClass(this, "TA", this.TObject, function () {',
    '  this.$init = function () {',
    '    pas.program.TObject.$init.call(this);',
    '  };',
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
    'if (this.TObject.vI == 4){};',
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
    'if (this.Obj.GetInt() == 2) {',
    '};',
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
    'if (this.TObject.GetInt() == 2) {',
    '};',
    'this.TObject.SetInt(this.TObject.GetInt() + 2);',
    'this.TObject.SetInt(this.TObject.Fx);',
    'this.Obj.$class.Fy = this.Obj.Fx + 1;',
    'if (this.Obj.$class.GetInt() == 2) {',
    '};',
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
    '  this.GetItems = function () {',
    '    var Result = [];',
    '    Result = this.FItems;',
    '    return Result;',
    '  };',
    '  this.SetItems = function (Value) {',
    '    this.FItems = Value;',
    '    this.FItems = null;',
    '    this.SetItems(null);',
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
    'this.Obj.SetItems(null);',
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
    '});',
    'this.Obj = null;',
    'this.b = false;'
    ]),
    LinesToStr([ // this.$main
    'if (this.Obj != null) {',
    '};',
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

procedure TTestModule.TestArray_Dynamic;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrayInt = array of longint;');
  Add('var');
  Add('  Arr: TArrayInt;');
  Add('  i: longint;');
  Add('begin');
  Add('  SetLength(arr,3);');
  Add('  arr[0]:=4;');
  Add('  arr[1]:=length(arr)+arr[0];');
  Add('  arr[i]:=5;');
  Add('  arr[arr[i]]:=arr[6];');
  Add('  i:=low(arr);');
  Add('  i:=high(arr);');
  ConvertProgram;
  CheckSource('TestArray_Dynamic',
    LinesToStr([ // statements
    'this.Arr = [];',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.Arr = rtl.setArrayLength(this.Arr,3,0);',
    'this.Arr[0] = 4;',
    'this.Arr[1] = rtl.length(this.Arr)+this.Arr[0];',
    'this.Arr[this.i] = 5;',
    'this.Arr[this.Arr[this.i]] = this.Arr[6];',
    'this.i = 0;',
    'this.i = rtl.length(this.Arr);',
    '']));
end;

procedure TTestModule.TestArray_Dynamic_Nil;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrayInt = array of longint;');
  Add('var');
  Add('  Arr: TArrayInt;');
  Add('begin');
  Add('  arr:=nil;');
  Add('  if arr=nil then;');
  Add('  if nil=arr then;');
  ConvertProgram;
  CheckSource('TestArray_Dynamic',
    LinesToStr([ // statements
    'this.Arr = [];'
    ]),
    LinesToStr([ // this.$main
    'this.Arr = null;',
    'if (this.Arr == null) {};',
    'if (null == this.Arr) {};'
    ]));
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
    'this.Arr2 = null;',
    'if (this.Arr2 == null) {};',
    'if (null == this.Arr2) {};',
    'this.i = 0;',
    'this.i = 0;',
    'this.i = rtl.length(this.Arr2);',
    'this.i = rtl.length(this.Arr2[2]);',
    'this.Arr2[3] = this.Arr;',
    'this.Arr2[4][5] = this.i;',
    'this.i = this.Arr2[6][7];',
    'this.Arr2[8][9] = this.i;',
    'this.i = this.Arr2[10][11];',
    'this.Arr2 = rtl.setArrayLength(this.Arr2, 14, []);',
    'this.Arr2[15] = rtl.setArrayLength(this.Arr2[15], 16, 0);',
    '']));
end;

Initialization
  RegisterTests([TTestModule]);
end.

