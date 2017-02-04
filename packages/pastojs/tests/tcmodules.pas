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
    Procedure TestEmptyUnit;

    // vars/const
    Procedure TestVarInt;
    Procedure TestVarBaseTypes;
    Procedure TestConstBaseTypes;
    Procedure TestUnitImplVars;
    Procedure TestUnitImplConsts;
    Procedure TestUnitImplRecord;

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
    // ToDo: Procedure TestBreak;
    // ToDo: Procedure TestContinue;
    // ToDo: TestString; SetLength,Length,[],char

    // ToDo: pass by reference

    // ToDo: enums

    // statements
    Procedure TestIncDec;
    Procedure TestAssignments;
    Procedure TestOperators1;
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

    // arrays
    Procedure TestArray;

    // classes
    Procedure TestClass_TObjectDefaultConstructor;
    Procedure TestClass_TObjectConstructorWithParams;
    Procedure TestClass_Var;
    Procedure TestClass_Method;
    Procedure TestClass_Inheritance;
    Procedure TestClass_AbstractMethod;
    Procedure TestClass_CallInherited_NoParams;
    Procedure TestClass_CallInherited_WithParams;
    // ToDo: Procedure TestClass_CallInheritedConstructor;
    // ToDo: overload
    // ToDo: second constructor
    // ToDo: call another constructor within a constructor
    // ToDo: call class.classmethod
    // ToDo: call instance.classmethod
    // ToDo: property
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
  writeln('TTestModule.ConvertModule JS:');
  write(FJSSource.Text);

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
    AssertEquals('module name',lowercase(Module.Name),String(ModuleNameExpr.Value.AsString));

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
  CheckSource('Empty program','','');
end;

procedure TTestModule.TestEmptyUnit;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  ConvertUnit;
end;

procedure TTestModule.TestVarInt;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestVarInt','this.i=0;','');
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
    'this.test = function () {',
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
  Add('  c: a;');
  Add('  d: b;');
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
  Add('  ProcA(3);');
  ConvertProgram;
  CheckSource('TestProcOneParam',
    LinesToStr([ // statements
    'this.proca = function (i) {',
    '};'
    ]),
    LinesToStr([ // this.$main
    'this.proca(3);'
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
  Add('  i:=FuncA();');
  Add('  i:=FuncA;');
  Add('  FuncA();');
  Add('  FuncA;');
  ConvertProgram;
  CheckSource('TestProcWithoutParams',
    LinesToStr([ // statements
    'this.funca = function () {',
    '  var result = 0;',
    '  return result;',
    '};',
    'this.i=0;'
    ]),
    LinesToStr([ // this.$main
    'this.i=this.funca();',
    'this.i=this.funca();',
    'this.funca();',
    'this.funca();'
    ]));
end;

procedure TTestModule.TestProcedureWithoutParams;
begin
  StartProgram(false);
  Add('procedure ProcA;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  ProcA();');
  Add('  ProcA;');
  ConvertProgram;
  CheckSource('TestProcWithoutParams',
    LinesToStr([ // statements
    'this.proca = function () {',
    '};'
    ]),
    LinesToStr([ // this.$main
    'this.proca();',
    'this.proca();'
    ]));
end;

procedure TTestModule.TestIncDec;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  inc(i);');
  Add('  inc(i,2);');
  Add('  dec(i);');
  Add('  dec(i,3);');
  ConvertProgram;
  CheckSource('TestIncDec',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.i+=1;',
    'this.i+=2;',
    'this.i-=1;',
    'this.i-=3;'
    ]));
end;

procedure TTestModule.TestAssignments;
begin
  StartProgram(false);
  Parser.Options:=Parser.Options+[po_cassignments];
  Add('var');
  Add('  i:longint;');
  Add('begin');
  Add('  i:=3;');
  Add('  i+=4;');
  Add('  i-=5;');
  Add('  i*=6;');
  ConvertProgram;
  CheckSource('TestAssignments',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.i=3;',
    'this.i+=4;',
    'this.i-=5;',
    'this.i*=6;'
    ]));
end;

procedure TTestModule.TestOperators1;
begin
  StartProgram(false);
  Add('var');
  Add('  v1,v2,v3:longint;');
  Add('begin');
  Add('  v1:=1;');
  Add('  v2:=v1+v1;');
  Add('  v2:=v1+v1*v2+v1 div v2;');
  Add('  v3:=-v1;');
  Add('  v1:=v1-v2;');
  Add('  v2:=v1;');
  Add('  if v1<v2 then v3:=v1 else v3:=v2;');
  ConvertProgram;
  CheckSource('TestOperators1',
    LinesToStr([ // statements
    'this.v1 = 0;',
    'this.v2 = 0;',
    'this.v3 = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.v1 = 1;',
    'this.v2 = (this.v1 + this.v1);',
    'this.v2 = ((this.v1 + (this.v1 * this.v2)) + (this.v1 / this.v2));',
    'this.v3 = -this.v1;',
    'this.v1 = (this.v1 - this.v2);',
    'this.v2 = this.v1;',
    'if ((this.v1 < this.v2)) this.v3 = this.v1 else this.v3 = this.v2;'
    ]));
end;

procedure TTestModule.TestPrgProcVar;
begin
  StartProgram(false);
  Add('procedure Proc1;');
  Add('type');
  Add('  t1=longint;');
  Add('var');
  Add('  v1:t1;');
  Add('begin');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestPrgProcVar',
    LinesToStr([ // statements
    'this.proc1 = function () {',
    '  var v1=0;',
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
  Add('type t1=string; // unit scope');
  Add('procedure Proc1;');
  Add('');
  Add('implementation');
  Add('');
  Add('procedure Proc1;');
  Add('type t1=longint; // local proc scope');
  Add('var  v1:t1; // using local t1');
  Add('begin');
  Add('end;');
  Add('var  v2:t1; // using interface t1');
  ConvertUnit;
  CheckSource('TestUnitProcVar',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.proc1 = function () {',
    '  var v1 = 0;',
    '};',
    'this.$impl = $impl;',
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
    'this.func1 = function () {',
    '  var result = 0;',
    '  result = 3;',
    '  return result;',
    '};'
    ]),
    '');
end;

procedure TTestModule.TestNestedProc;
begin
  StartProgram(false);
  Add('function DoIt(a,d: longint): longint;');
  Add('var');
  Add('  b: longint;');
  Add('  c: longint;');
  Add('  function Nesty(a: longint): longint; ');
  Add('  var b: longint;');
  Add('  begin');
  Add('    Result:=a+b+c+d;');
  Add('  end;');
  Add('begin');
  Add('  Result:=a+b+c;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestNestedProc',
    LinesToStr([ // statements
    'this.doit = function (a, d) {',
    '  var result = 0;',
    '  var b = 0;',
    '  var c = 0;',
    '  function nesty(a) {',
    '    var result = 0;',
    '    var b = 0;',
    '    result = (((a + b) + c) + d);',
    '    return result;',
    '  };',
    '  result = ((a + b) + c);',
    '  return result;',
    '};'
    ]),
    '');
end;

procedure TTestModule.TestForwardProc;
begin
  StartProgram(false);
  Add('procedure FuncA(i: longint); forward;');
  Add('procedure FuncB(i: longint);');
  Add('begin');
  Add('  FuncA(i);');
  Add('end;');
  Add('procedure FuncA(i: longint);');
  Add('begin');
  Add('  if i=3 then ;');
  Add('end;');
  Add('begin');
  Add('  FuncA(4);');
  Add('  FuncB(5);');
  ConvertProgram;
  CheckSource('TestForwardProc',
    LinesToStr([ // statements'
    'this.funcb = function (i) {',
    '  this.funca(i);',
    '};',
    'this.funca = function (i) {',
    '  if ((i == 3)) {',
    '  };',
    '};'
    ]),
    LinesToStr([
    'this.funca(4);',
    'this.funcb(5);'
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
  Add('    FuncB(i);');
  Add('  end;');
  Add('  procedure FuncB(i: longint);');
  Add('  begin');
  Add('    if i=3 then ;');
  Add('  end;');
  Add('begin');
  Add('  FuncC(4)');
  Add('end;');
  Add('begin');
  Add('  FuncA;');
  ConvertProgram;
  CheckSource('TestNestedForwardProc',
    LinesToStr([ // statements'
    'this.funca = function () {',
    '  function funcc(i) {',
    '    funcb(i);',
    '  };',
    '  function funcb(i) {',
    '    if ((i == 3)) {',
    '    };',
    '  };',
    '  funcc(4);',
    '};'
    ]),
    LinesToStr([
    'this.funca();'
    ])
    );
end;

procedure TTestModule.TestAssignFunctionResult;
begin
  StartProgram(false);
  Add('function F1: longint;');
  Add('begin');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  i:=F1();');
  Add('  i:=F1()+F1();');
  ConvertProgram;
  CheckSource('TestAssignFunctionResult',
    LinesToStr([ // statements
     'this.f1 = function () {',
     '  var result = 0;',
     '  return result;',
     '};',
     'this.i = 0;'
    ]),
    LinesToStr([
    'this.i = this.f1();',
    'this.i = (this.f1() + this.f1());'
    ]));
end;

procedure TTestModule.TestFunctionResultInCondition;
begin
  StartProgram(false);
  Add('function F1: longint;');
  Add('begin');
  Add('end;');
  Add('function F2: boolean;');
  Add('begin');
  Add('end;');
  Add('var i: longint;');
  Add('begin');
  Add('  if F2 then ;');
  Add('  if i=F1() then ;');
  Add('  if i=F1 then ;');
  ConvertProgram;
  CheckSource('TestFunctionResultInCondition',
    LinesToStr([ // statements
     'this.f1 = function () {',
     '  var result = 0;',
     '  return result;',
     '};',
     'this.f2 = function () {',
     '  var result = false;',
     '  return result;',
     '};',
     'this.i = 0;'
    ]),
    LinesToStr([
    'if (this.f2()) {',
    '};',
    'if ((this.i == this.f1())) {',
    '};',
    'if ((this.i == this.f1())) {',
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
    'this.proca = function () {',
    '  return;',
    '};',
    'this.funcb = function () {',
    '  var result = 0;',
    '  return result;',
    '  return 3;',
    '  return result;',
    '};',
    'this.funcc = function () {',
    '  var result = "";',
    '  return result;',
    '  return "a";',
    '  return "abc";',
    '  return result;',
    '};'
    ]),
    '');
end;

procedure TTestModule.TestUnitImplVars;
begin
  StartUnit(false);
  Add('interface');
  Add('implementation');
  Add('var');
  Add('  v1:longint;');
  Add('  v2:longint = 3;');
  Add('  v3:string = ''abc'';');
  ConvertUnit;
  CheckSource('TestUnitImplVars',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '$impl.v1 = 0;',
    '$impl.v2 = 3;',
    '$impl.v3 = "abc";'
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
  Add('var r: TMyRecord;');
  Add('initialization');
  Add('  r.i:=3;');
  ConvertUnit;
  CheckSource('TestUnitImplRecord',
    LinesToStr([ // statements
    'var $impl = {',
    '};',
    'this.$impl = $impl;',
    '$impl.tmyrecord = function () {',
    '  this.i = 0;',
    '};',
    '$impl.r = new $impl.tmyrecord();'
    ]),
    '$impl.r.i = 3;'
    );
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
    'this.test = function (a,b) {',
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
  Add('function Test(a: longint): longint;');
  Add('begin');
  Add('  Result:=2*a');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestFunctionInt',
    LinesToStr([ // statements
    'this.test = function (a) {',
    '  var result = 0;',
    '  result = (2*a);',
    '  return result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestFunctionString;
begin
  StartProgram(false);
  Add('function Test(a: string): string;');
  Add('begin');
  Add('  Result:=a+a');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestFunctionString',
    LinesToStr([ // statements
    'this.test = function (a) {',
    '  var result = "";',
    '  result = (a+a);',
    '  return result;',
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
  Add('    B: longint;');
  Add('  end;');
  Add('var r: TRecA;');
  Add('begin');
  Add('  r.B:=123');
  ConvertProgram;
  CheckSource('TestVarRecord',
    LinesToStr([ // statements
    'this.treca = function () {',
    '  this.b = 0;',
    '};',
    'this.r = new this.treca();'
    ]),
    LinesToStr([ // this.$main
    'this.r.b = 123;'
    ]));
end;

procedure TTestModule.TestForLoop;
begin
  StartProgram(false);
  Add('var');
  Add('  i, j, n: longint;');
  Add('begin');
  Add('  j:=0;');
  Add('  n:=3;');
  Add('  for i:=1 to n do');
  Add('  begin');
  Add('    j:=j+i;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestForLoop',
    LinesToStr([ // statements
    'this.i = 0;',
    'this.j = 0;',
    'this.n = 0;'
    ]),
    LinesToStr([ // this.$main
    '  this.j = 0;',
    '  this.n = 3;',
    '  var $loopend1 = this.n;',
    '  for (this.i = 1; (this.i <= $loopend1); this.i++) {',
    '    this.j = (this.j + this.i);',
    '  };',
    '  if ((this.i > $loopend1)) this.i--;'
    ]));
end;

procedure TTestModule.TestForLoopInFunction;
begin
  StartProgram(false);
  Add('function SumNumbers(n: longint): longint;');
  Add('var');
  Add('  i, j: longint;');
  Add('begin');
  Add('  j:=0;');
  Add('  for i:=1 to n do');
  Add('  begin');
  Add('    j:=j+i;');
  Add('  end;');
  Add('end;');
  Add('begin');
  Add('  SumNumbers(3);');
  ConvertProgram;
  CheckSource('TestForLoopInFunction',
    LinesToStr([ // statements
    'this.sumnumbers = function (n) {',
    '  var result = 0;',
    '  var i = 0;',
    '  var j = 0;',
    '  j = 0;',
    '  var $loopend1 = n;',
    '  for (i = 1; (i <= $loopend1); i++) {',
    '    j = (j + i);',
    '  };',
    '  return result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    '  this.sumnumbers(3);'
    ]));
end;

procedure TTestModule.TestForLoop_ReadVarAfter;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  for i:=1 to 2 do ;');
  Add('  if i=3 then ;');
  ConvertProgram;
  CheckSource('TestForLoop',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    '  var $loopend1 = 2;',
    '  for (this.i = 1; (this.i <= $loopend1); this.i++);',
    '  if((this.i>$loopend1))this.i--;',
    '  if ((this.i==3)){} ;'
    ]));
end;

procedure TTestModule.TestForLoop_Nested;
begin
  StartProgram(false);
  Add('function SumNumbers(n: longint): longint;');
  Add('var');
  Add('  i, j, k: longint;');
  Add('begin');
  Add('  k:=0;');
  Add('  for i:=1 to n do');
  Add('  begin');
  Add('    for j:=1 to i do');
  Add('    begin');
  Add('      k:=k+i;');
  Add('    end;');
  Add('  end;');
  Add('end;');
  Add('begin');
  Add('  SumNumbers(3);');
  ConvertProgram;
  CheckSource('TestForLoopInFunction',
    LinesToStr([ // statements
    'this.sumnumbers = function (n) {',
    '  var result = 0;',
    '  var i = 0;',
    '  var j = 0;',
    '  var k = 0;',
    '  k = 0;',
    '  var $loopend1 = n;',
    '  for (i = 1; (i <= $loopend1); i++) {',
    '    var $loopend2 = i;',
    '    for (j = 1; (j <= $loopend2); j++) {',
    '      k = (k + i);',
    '    };',
    '  };',
    '  return result;',
    '};'
    ]),
    LinesToStr([ // this.$main
    '  this.sumnumbers(3);'
    ]));
end;

procedure TTestModule.TestRepeatUntil;
begin
  StartProgram(false);
  Add('var');
  Add('  i, j, n: longint;');
  Add('begin');
  Add('  n:=3;');
  Add('  j:=0;');
  Add('  i:=0;');
  Add('  repeat');
  Add('    i:=i+1;');
  Add('    j:=j+i;');
  Add('  until i>=n');
  ConvertProgram;
  CheckSource('TestRepeatUntil',
    LinesToStr([ // statements
    'this.i = 0;',
    'this.j = 0;',
    'this.n = 0;'
    ]),
    LinesToStr([ // this.$main
    '  this.n = 3;',
    '  this.j = 0;',
    '  this.i = 0;',
    '  do{',
    '    this.i = (this.i + 1);',
    '    this.j = (this.j + this.i);',
    '  }while(!(this.i>=this.n));'
    ]));
end;

procedure TTestModule.TestAsmBlock;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  i:=1;');
  Add('  asm');
  Add('    if (i==1) {');
  Add('      i=2;');
  Add('    }');
  Add('    if (i==2){ i=3; }');
  Add('  end;');
  Add('  i:=4;');
  ConvertProgram;
  CheckSource('TestAsmBlock',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'this.i = 1;',
    'if (i==1) {',
    'i=2;',
    '}',
    'if (i==2){ i=3; }',
    ';',
    'this.i = 4;'
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
    '  this.i = (2 / this.i);',
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
  Add('var i: longint;');
  Add('begin');
  Add('  try');
  Add('    i:=1;');
  Add('  except');
  Add('    i:=2');
  Add('  end;');
  Add('  try');
  Add('    i:=3;');
  Add('  except');
  Add('    raise;');
  Add('  end;');
  Add('  try');
  Add('    i:=4;');
  Add('  except');
  Add('    on EInvalidCast do');
  Add('      raise;');
  Add('    on E: Exception do');
  Add('      if E.msg='''' then');
  Add('        raise E;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestTryExcept',
    LinesToStr([ // statements
    'rtl.createClass(this, "tobject", null, function () {',
    '});',
    'rtl.createClass(this, "exception", this.tobject, function () {',
    '  this.msg = "";',
    '});',
    'rtl.createClass(this, "einvalidcast", this.exception, function () {',
    '});',
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'try {',
    '  this.i = 1;',
    '} catch {',
    '  this.i = 2;',
    '};',
    'try {',
    '  this.i = 3;',
    '} catch (exceptobject) {',
    '  throw exceptobject;',
    '};',
    'try {',
    '  this.i = 4;',
    '} catch (exceptobject) {',
    '  if (this.einvalidcast.isPrototypeOf(exceptobject)) throw exceptobject;',
    '  if (this.exception.isPrototypeOf(exceptobject)) {',
    '    var e = exceptobject;',
    '    if ((e.msg == "")) throw e;',
    '  };',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOf;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  case i of');
  Add('  1: ;');
  Add('  2: i:=3;');
  Add('  else');
  Add('    i:=4');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOf',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $tmp1 = this.i;',
    'if (($tmp1 == 1)) {} else if (($tmp1 == 2)) this.i = 3 else {',
    '  this.i = 4;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOf_UseSwitch;
begin
  StartProgram(false);
  Converter.UseSwitchStatement:=true;
  Add('var i: longint;');
  Add('begin');
  Add('  case i of');
  Add('  1: ;');
  Add('  2: i:=3;');
  Add('  else');
  Add('    i:=4');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOf_UseSwitch',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'switch (this.i) {',
    'case 1:',
    '  break;',
    'case 2:',
    '  this.i = 3;',
    '  break;',
    'default:',
    '  this.i = 4;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOfNoElse;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  case i of');
  Add('  1: begin i:=2; i:=3; end;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOfNoElse',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $tmp1 = this.i;',
    'if (($tmp1 == 1)) {',
    '  this.i = 2;',
    '  this.i = 3;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOfNoElse_UseSwitch;
begin
  StartProgram(false);
  Converter.UseSwitchStatement:=true;
  Add('var i: longint;');
  Add('begin');
  Add('  case i of');
  Add('  1: begin i:=2; i:=3; end;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOfNoElse_UseSwitch',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'switch (this.i) {',
    'case 1:',
    '  this.i = 2;',
    '  this.i = 3;',
    '  break;',
    '};'
    ]));
end;

procedure TTestModule.TestCaseOfRange;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  case i of');
  Add('  1..3: i:=14;');
  Add('  4,5: i:=16;');
  Add('  6..7,9..10: ;');
  Add('  else ;');
  Add('  end;');
  ConvertProgram;
  CheckSource('TestCaseOfRange',
    LinesToStr([ // statements
    'this.i = 0;'
    ]),
    LinesToStr([ // this.$main
    'var $tmp1 = this.i;',
    'if ((($tmp1 >= 1) && ($tmp1 <= 3))) this.i = 14 else if ((($tmp1 == 4) || ($tmp1 == 5))) this.i = 16 else if (((($tmp1 >= 6) && ($tmp1 <= 7)) || (($tmp1 >= 9) && ($tmp1 <= 10)))) {} else {',
    '};'
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
  Add('constructor TObject.Create;');
  Add('begin end;');
  Add('destructor TObject.Destroy;');
  Add('begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=TObject.Create;');
  Add('  o.Destroy;');
  ConvertProgram;
  CheckSource('TestClass_TObjectDefaultConstructor',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.create = function(){',
    '  };',
    '  this.destroy = function(){',
    '  };',
    '});',
    'this.o = null;'
    ]),
    LinesToStr([ // this.$main
    'this.o = this.tobject.$create("create");',
    'this.o.$destroy("destroy");'
    ]));
end;

procedure TTestModule.TestClass_TObjectConstructorWithParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    constructor Create(p: longint);');
  Add('  end;');
  Add('constructor TObject.Create(p: longint);');
  Add('begin end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=TObject.Create(3);');
  ConvertProgram;
  CheckSource('TestClass_TObjectConstructorWithParams',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.create = function(p){',
    '  };',
    '});',
    'this.o = null;'
    ]),
    LinesToStr([ // this.$main
    'this.o = this.tobject.$create("create",[3]);'
    ]));
end;

procedure TTestModule.TestClass_Var;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    i: longint;');
  Add('    constructor Create(p: longint);');
  Add('  end;');
  Add('constructor TObject.Create(p: longint);');
  Add('begin');
  Add('  i:=p+3');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=TObject.Create(4);');
  Add('  o.i:=o.i+5;');
  ConvertProgram;
  CheckSource('TestClass_Var',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.i = 0;',
    '  this.create = function(p){',
    '    this.i = (p+3);',
    '  };',
    '});',
    'this.o = null;'
    ]),
    LinesToStr([ // this.$main
    'this.o = this.tobject.$create("create",[4]);',
    'this.o.i = (this.o.i + 5);'
    ]));
end;

procedure TTestModule.TestClass_Method;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  public');
  Add('    i: longint;');
  Add('    Sub: TObject;');
  Add('    constructor Create;');
  Add('    function GetIt(p: longint): TObject;');
  Add('  end;');
  Add('constructor TObject.Create; begin end;');
  Add('function TObject.GetIt(p: longint): TObject;');
  Add('begin');
  Add('  Self.i:=p+3;');
  Add('  Result:=Self.Sub;');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o:=TObject.Create;');
  Add('  o.GetIt(4);');
  Add('  o.Sub.Sub:=nil;');
  Add('  o.Sub.GetIt(5);');
  Add('  o.Sub.GetIt(6).Sub:=nil;');
  Add('  o.Sub.GetIt(7).GetIt(8);');
  Add('  o.Sub.GetIt(9).Sub.GetIt(10);');
  ConvertProgram;
  CheckSource('TestClass_Method',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.i = 0;',
    '  this.sub = null;',
    '  this.create = function(){',
    '  };',
    '  this.getit = function(p){',
    '    var result = null;',
    '    this.i = (p + 3);',
    '    result = this.sub;',
    '    return result;',
    '  };',
    '});',
    'this.o = null;'
    ]),
    LinesToStr([ // this.$main
    'this.o = this.tobject.$create("create");',
    'this.o.getit(4);',
    'this.o.sub.sub=null;',
    'this.o.sub.getit(5);',
    'this.o.sub.getit(6).sub=null;',
    'this.o.sub.getit(7).getit(8);',
    'this.o.sub.getit(9).sub.getit(10);'
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
  Add('constructor TObject.Create; begin end;');
  Add('procedure TClassB.ProcB; begin end;');
  Add('var');
  Add('  o: TObject;');
  Add('  a: TClassA;');
  Add('  b: TClassB;');
  Add('begin');
  Add('  o:=TObject.Create;');
  Add('  a:=TClassA.Create;');
  Add('  b:=TClassB.Create;');
  Add('  if o is TClassA then ;');
  Add('  b:=o as TClassB;');
  Add('  (o as TClassB).ProcB;');
  ConvertProgram;
  CheckSource('TestClass_Inheritance',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.create = function () {',
    '  };',
    '});',
    'rtl.createClass(this,"tclassa",this.tobject,function(){',
    '});',
    'rtl.createClass(this,"tclassb",this.tobject,function(){',
    '  this.procb = function () {',
    '  };',
    '});',
    'this.o = null;',
    'this.a = null;',
    'this.b = null;'
    ]),
    LinesToStr([ // this.$main
    'this.o = this.tobject.$create("create");',
    'this.a = this.tclassa.$create("create");',
    'this.b = this.tclassb.$create("create");',
    'if (this.tclassa.isPrototypeOf(this.o)) {',
    '};',
    'this.b = rtl.as(this.o, this.tclassb);',
    'rtl.as(this.o, this.tclassb).procb();'
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
    'rtl.createClass(this,"tobject",null,function(){',
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
  Add('    procedure DoAbstract; override;');
  Add('    procedure DoVirtual; override;');
  Add('    procedure DoSome;');
  Add('  end;');
  Add('procedure TObject.DoVirtual;');
  Add('begin');
  Add('  inherited; // call non existing ancestor -> ignore silently');
  Add('end;');
  Add('procedure TObject.DoIt;');
  Add('begin');
  Add('end;');
  Add('procedure TA.DoAbstract;');
  Add('begin');
  Add('  inherited DoVirtual; // call TObject.DoVirtual');
  Add('end;');
  Add('procedure TA.DoVirtual;');
  Add('begin');
  Add('  inherited; // call TObject.DoVirtual');
  Add('  inherited DoVirtual; // call TObject.DoVirtual');
  Add('  inherited DoVirtual(); // call TObject.DoVirtual');
  Add('  DoIt;');
  Add('  DoIt();');
  Add('end;');
  Add('procedure TA.DoSome;');
  Add('begin');
  Add('  inherited; // call non existing ancestor method -> silently ignore');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_CallInherited_NoParams',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.dovirtual = function () {',
    '  };',
    '  this.doit = function () {',
    '  };',
    '});',
    'rtl.createClass(this, "ta", this.tobject, function () {',
    '  this.doabstract = function () {',
    '    pas.program.tobject.dovirtual.call(this);',
    '  };',
    '  this.dovirtual = function () {',
    '    pas.program.tobject.dovirtual.apply(this, arguments);',
    '    pas.program.tobject.dovirtual.call(this);',
    '    pas.program.tobject.dovirtual.call(this);',
    '    this.doit();',
    '    this.doit();',
    '  };',
    '  this.dosome = function () {',
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
  Add('    procedure DoAbstract(a: longint; b: longint = 0); virtual; abstract;');
  Add('    procedure DoVirtual(a: longint; b: longint = 0); virtual;');
  Add('    procedure DoIt(a: longint; b: longint = 0);');
  Add('    procedure DoIt2(a: longint = 1; b: longint = 2);');
  Add('  end;');
  Add('  TA = class');
  Add('    procedure DoAbstract(a: longint; b: longint = 0); override;');
  Add('    procedure DoVirtual(a: longint; b: longint = 0); override;');
  Add('  end;');
  Add('procedure TObject.DoVirtual(a: longint; b: longint = 0);');
  Add('begin');
  Add('end;');
  Add('procedure TObject.DoIt(a: longint; b: longint = 0);');
  Add('begin');
  Add('end;');
  Add('procedure TObject.DoIt2(a: longint; b: longint = 0);');
  Add('begin');
  Add('end;');
  Add('procedure TA.DoAbstract(a: longint; b: longint = 0);');
  Add('begin');
  Add('  inherited DoVirtual(a,b); // call TObject.DoVirtual(a,b)');
  Add('  inherited DoVirtual(a); // call TObject.DoVirtual(a,0)');
  Add('end;');
  Add('procedure TA.DoVirtual(a: longint; b: longint = 0);');
  Add('begin');
  Add('  inherited; // call TObject.DoVirtual(a,b)');
  Add('  inherited DoVirtual(a,b); // call TObject.DoVirtual(a,b)');
  Add('  inherited DoVirtual(a); // call TObject.DoVirtual(a,0)');
  Add('  DoIt(a,b);');
  Add('  DoIt(a);');
  Add('  DoIt2(a);');
  Add('  DoIt2;');
  Add('end;');
  Add('begin');
  ConvertProgram;
  CheckSource('TestClass_CallInherited_WithParams',
    LinesToStr([ // statements
    'rtl.createClass(this,"tobject",null,function(){',
    '  this.dovirtual = function (a,b) {',
    '  };',
    '  this.doit = function (a,b) {',
    '  };',
    '  this.doit2 = function (a,b) {',
    '  };',
    '});',
    'rtl.createClass(this, "ta", this.tobject, function () {',
    '  this.doabstract = function (a,b) {',
    '    pas.program.tobject.dovirtual.call(this,a,b);',
    '    pas.program.tobject.dovirtual.call(this,a,0);',
    '  };',
    '  this.dovirtual = function (a,b) {',
    '    pas.program.tobject.dovirtual.apply(this, arguments);',
    '    pas.program.tobject.dovirtual.call(this,a,b);',
    '    pas.program.tobject.dovirtual.call(this,a,0);',
    '    this.doit(a,b);',
    '    this.doit(a,0);',
    '    this.doit2(a,2);',
    '    this.doit2(1,2);',
    '  };',
    '});'
    ]),
    LinesToStr([ // this.$main
    ''
    ]));
end;

procedure TTestModule.TestArray;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrayInt = array of longint;');
  Add('var');
  Add('  a: TArrayInt;');
  Add('begin');
  Add('  SetLength(a,3);');
  Add('  a[0]:=4;');
  Add('  a[1]:=length(a)+a[0];');
  ConvertProgram;
  CheckSource('TestArray',
    LinesToStr([ // statements
    'this.a = [];'
    ]),
    LinesToStr([ // this.$main
    'rtl.setArrayLength(this.a,3,0);',
    'this.a[0]=4;',
    'this.a[1]=(rtl.length(this.a)+this.a[0]);'
    ]));
end;

Initialization
  RegisterTests([TTestModule]);
end.

