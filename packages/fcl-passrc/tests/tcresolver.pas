{
  Examples:
    ./testpassrc --suite=TTestResolver.TestEmpty
}
(*
  CheckReferenceDirectives:
    {#a} label "a", labels all elements at the following token
    {@a} reference "a", search at next token for an element e with
           TResolvedReference(e.CustomData).Declaration points to an element
           labeled "a".
    {=a} is "a", search at next token for a TPasAliasType t with t.DestType
           points to an element labeled "a"
*)
unit tcresolver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, PasTree, PScanner, PParser, PasResolver,
  tcbaseparser, testregistry, contnrs;

Type
  TOnFindUnit = function(const aUnitName: String): TPasModule of object;

  { TTestEnginePasResolver }

  TTestEnginePasResolver = class(TPasResolver)
  private
    FFilename: string;
    FModule: TPasModule;
    FOnFindUnit: TOnFindUnit;
    FParser: TPasParser;
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
    property Parser: TPasParser read FParser write FParser;
    property Source: string read FSource write FSource;
    property Module: TPasModule read FModule write SetModule;
  end;

  TTestResolverReferenceData = record
    Filename: string;
    Line: integer;
    StartCol: integer;
    EndCol: integer;
    Found: TFPList; // list of TPasElement at this token
  end;
  PTestResolverReferenceData = ^TTestResolverReferenceData;

  TSystemUnitPart = (
    supTObject
    );
  TSystemUnitParts = set of TSystemUnitPart;

  { TTestResolver }

  TTestResolver = Class(TTestParser)
  Private
    FFirstStatement: TPasImplBlock;
    FModules: TObjectList;// list of TTestEnginePasResolver
    FResolverEngine: TTestEnginePasResolver;
    function GetModuleCount: integer;
    function GetModules(Index: integer): TTestEnginePasResolver;
    function OnPasResolverFindUnit(const aUnitName: String): TPasModule;
    procedure OnFindReference(El: TPasElement; FindData: pointer);
    procedure OnCheckElementParent(El: TPasElement; arg: pointer);
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure CreateEngine(var TheEngine: TPasTreeContainer); override;
    procedure ParseProgram;
    procedure ParseUnit;
    procedure CheckReferenceDirectives;
    procedure CheckResolverException(Msg: string; MsgNumber: integer);
    procedure CheckParserException(Msg: string; MsgNumber: integer);
  Public
    function FindModuleWithFilename(aFilename: string): TTestEnginePasResolver;
    function AddModule(aFilename: string): TTestEnginePasResolver;
    function AddModuleWithSrc(aFilename, Src: string): TTestEnginePasResolver;
    function AddModuleWithIntfImplSrc(aFilename, InterfaceSrc,
      ImplementationSrc: string): TTestEnginePasResolver;
    procedure AddSystemUnit(Parts: TSystemUnitParts = []);
    procedure StartProgram(NeedSystemUnit: boolean; SystemUnitParts: TSystemUnitParts = []);
    procedure StartUnit(NeedSystemUnit: boolean);
    property Modules[Index: integer]: TTestEnginePasResolver read GetModules;
    property ModuleCount: integer read GetModuleCount;
    property ResolverEngine: TTestEnginePasResolver read FResolverEngine;
  Published
    Procedure TestEmpty;

    // alias
    Procedure TestAliasType;
    Procedure TestAlias2Type;
    Procedure TestAliasTypeRefs;
    Procedure TestAliasOfVarFail;

    // var, const
    Procedure TestVarLongint;
    Procedure TestVarInteger;
    Procedure TestConstInteger;
    Procedure TestDuplicateVar;
    Procedure TestVarOfVarFail;
    Procedure TestConstOfVarFail;
    Procedure TestTypedConstWrongExprFail;
    //Procedure TestVarWrongExprFail;
    Procedure TestIncDec;
    Procedure TestIncStringFail;

    // enums
    Procedure TestEnums;
    Procedure TestSets;
    Procedure TestEnumParams;
    Procedure TestSetParams;
    // test high, low

    // operators
    Procedure TestPrgAssignment;
    Procedure TestPrgProcVar;
    Procedure TestUnitProcVar;
    Procedure TestAssignIntegers;
    Procedure TestAssignString;
    Procedure TestAssignIntToStringFail;
    Procedure TestAssignStringToIntFail;
    Procedure TestIntegerOperators;
    Procedure TestBooleanOperators;
    Procedure TestStringOperators;
    Procedure TestFloatOperators;
    Procedure TestStringElementMissingArgFail;
    Procedure TestStringElementIndexNonIntFail;
    Procedure TestCAssignments;
    Procedure TestTypeCastBaseTypes;
    Procedure TestTypeCastStrToIntFail;
    Procedure TestTypeCastIntToStrFail;
    Procedure TestTypeCastDoubleToStrFail;
    Procedure TestTypeCastDoubleToIntFail;

    // statements
    Procedure TestForLoop;
    Procedure TestStatements;
    Procedure TestCaseStatement;
    Procedure TestTryStatement;
    Procedure TestTryExceptOnNonTypeFail;
    Procedure TestTryExceptOnNonClassFail;
    Procedure TestRaiseNonVarFail;
    Procedure TestRaiseNonClassFail;
    Procedure TestStatementsRefs;
    Procedure TestRepeatUntilNonBoolFail;
    Procedure TestWhileDoNonBoolFail;
    Procedure TestIfThenNonBoolFail;
    Procedure TestForLoopVarNonVarFail;
    Procedure TestForLoopStartIncompFail;
    Procedure TestForLoopEndIncompFail;
    Procedure TestCaseOf;
    Procedure TestCaseExprNonOrdFail;
    Procedure TestCaseIncompatibleValueFail;

    // units
    Procedure TestUnitRef;

    // procs
    Procedure TestProcParam;
    Procedure TestFunctionResult;
    Procedure TestProcOverload;
    Procedure TestProcOverloadWithBaseTypes;
    Procedure TestProcOverloadWithClassTypes;
    Procedure TestProcOverloadWithInhClassTypes;
    Procedure TestProcOverloadWithInhAliasClassTypes;
    Procedure TestProcDuplicate;
    Procedure TestNestedProc;
    Procedure TestForwardProc;
    Procedure TestForwardProcUnresolved;
    Procedure TestNestedForwardProc;
    Procedure TestNestedForwardProcUnresolved;
    Procedure TestForwardProcFuncMismatch;
    Procedure TestForwardFuncResultMismatch;
    Procedure TestUnitIntfProc;
    Procedure TestUnitIntfProcUnresolved;
    Procedure TestUnitIntfMismatchArgName;
    Procedure TestProcOverloadIsNotFunc;
    Procedure TestProcCallMissingParams;
    Procedure TestBuiltInProcCallMissingParams;
    Procedure TestAssignFunctionResult;
    Procedure TestAssignProcResultFail;
    Procedure TestFunctionResultInCondition;
    Procedure TestExit;
    // test high low integer

    // record
    Procedure TestRecord;
    Procedure TestRecordVariant;
    Procedure TestRecordVariantNested;

    // class
    Procedure TestClass;
    Procedure TestClassDefaultInheritance;
    Procedure TestClassTripleInheritance;
    Procedure TestClassForward;
    Procedure TestClassForwardNotResolved;
    Procedure TestClassMethod;
    Procedure TestClassMethodUnresolved;
    Procedure TestClassMethodAbstract;
    Procedure TestClassMethodAbstractWithoutVirtual;
    Procedure TestClassMethodAbstractHasBody;
    Procedure TestClassMethodUnresolvedWithAncestor;
    Procedure TestClassProcFuncMismatch;
    Procedure TestClassMethodOverload;
    Procedure TestClassMethodInvalidOverload;
    Procedure TestClassOverride;
    Procedure TestClassOverride2;
    Procedure TestClassMethodScope;
    Procedure TestClassIdentifierSelf;
    Procedure TestClassCallInherited;
    Procedure TestClassCallInheritedNoParamsAbstractFail;
    Procedure TestClassCallInheritedWithParamsAbstractFail;
    Procedure TestClassCallInheritedConstructor;
    Procedure TestClassAssignNil;
    Procedure TestClassAssign;
    Procedure TestClassNilAsParam;
    Procedure TestClassOperator_Is_As;
    Procedure TestClassOperatorIsOnNonDescendantFail;
    Procedure TestClassOperatorIsOnNonTypeFail;
    Procedure TestClassOperatorAsOnNonDescendantFail;
    Procedure TestClassOperatorAsOnNonTypeFail;
    Procedure TestClassAsFuncResult;
    Procedure TestClassTypeCast;
    Procedure TestClassTypeCastUnrelatedFail;
    Procedure TestClass_TypeCastSelf;
    Procedure TestClass_AccessMemberViaClassFail;
    Procedure TestClass_FuncReturningObjectMember;
    Procedure TestClass_StaticWithoutClassFail;
    Procedure TestClass_SelfInStaticFail;
    // ToDo: visibility

    // class of
    Procedure TestClassOf;
    Procedure TestClassOfNonClassFail;
    Procedure TestClassOfIsOperatorFail;
    Procedure TestClassOfAsOperatorFail;
    Procedure TestClass_ClassVar;
    Procedure TestClassOfDotClassVar;
    Procedure TestClassOfDotVarFail;
    Procedure TestClassOfDotClassProc;
    Procedure TestClassOfDotProcFail;
    Procedure TestClassOfDotClassProperty;
    Procedure TestClassOfDotPropertyFail;
    Procedure TestClass_ClassProcSelf;
    Procedure TestClass_ClassProcSelfTypeCastFail;
    Procedure TestClass_ClassMembers;

    // property
    Procedure TestProperty1;
    Procedure TestPropertyAccessorNotInFront;
    Procedure TestPropertyReadAccessorVarWrongType;
    Procedure TestPropertyReadAccessorProcNotFunc;
    Procedure TestPropertyReadAccessorFuncWrongResult;
    Procedure TestPropertyReadAccessorFuncWrongArgCount;
    Procedure TestPropertyReadAccessorFunc;
    Procedure TestPropertyWriteAccessorVarWrongType;
    Procedure TestPropertyWriteAccessorFuncNotProc;
    Procedure TestPropertyWriteAccessorProcWrongArgCount;
    Procedure TestPropertyWriteAccessorProcWrongArg;
    Procedure TestPropertyWriteAccessorProcWrongArgType;
    Procedure TestPropertyWriteAccessorProc;
    Procedure TestPropertyTypeless;
    Procedure TestPropertyTypelessNoAncestor;
    Procedure TestPropertyStoredAccessorProcNotFunc;
    Procedure TestPropertyStoredAccessorFuncWrongResult;
    Procedure TestPropertyStoredAccessorFuncWrongArgCount;
    Procedure TestPropertyAssign;
    Procedure TestPropertyAssignReadOnlyFail;
    Procedure TestPropertyReadNonReadableFail;
    Procedure TestPropertyArgs1;
    Procedure TestPropertyArgs2;
    Procedure TestPropertyArgsWithDefaultsFail;
    Procedure TestDefaultProperty;

    // with
    Procedure TestWithBlock1;
    Procedure TestWithBlock2;
    Procedure TestWithBlockFuncResult;
    Procedure TestWithBlockConstructor;

    // arrays
    Procedure TestDynArrayOfLongint;
    Procedure TestStaticArray;
    Procedure TestArrayOfArray;
    Procedure TestFunctionReturningArray;
    // test high, low

    // procedure types
    // ToDo: test proc type
    // ToDo: test func type
    // ToDo: test method type
    // ToDo: test Assigned
    // ToDo: test equal, notequal
    // ToDo: test proc type as parameter
  end;

function LinesToStr(Args: array of const): string;

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

{ TTestResolver }

procedure TTestResolver.SetUp;
begin
  FModules:=TObjectList.Create(true);
  inherited SetUp;
  Parser.Options:=Parser.Options+[po_resolvestandardtypes];
end;

procedure TTestResolver.TearDown;
begin
  ResolverEngine.Clear;
  if FModules<>nil then
    begin
    FModules.OwnsObjects:=false;
    FModules.Remove(ResolverEngine); // remove reference
    FModules.OwnsObjects:=true;
    FreeAndNil(FModules);// free all other modules
    end;
  inherited TearDown;
  FResolverEngine:=nil;
end;

procedure TTestResolver.CreateEngine(var TheEngine: TPasTreeContainer);
begin
  FResolverEngine:=AddModule(MainFilename);
  TheEngine:=ResolverEngine;
end;

procedure TTestResolver.ParseProgram;
begin
  FFirstStatement:=nil;
  try
    ParseModule;
  except
    on E: EParserError do
      begin
      writeln('ERROR: TTestResolver.ParseProgram Parser: '+E.ClassName+':'+E.Message
        +' Scanner at'
        +' File='+Scanner.CurFilename
        +' Row='+IntToStr(Scanner.CurRow)
        +' Col='+IntToStr(Scanner.CurColumn)
        +' Line="'+Scanner.CurLine+'"'
        );
      raise E;
      end;
    on E: EPasResolve do
      begin
      writeln('ERROR: TTestResolver.ParseProgram PasResolver: '+E.ClassName+':'+E.Message
        +' Scanner at'
        +' File='+Scanner.CurFilename
        +' Row='+IntToStr(Scanner.CurRow)
        +' Col='+IntToStr(Scanner.CurColumn)
        +' Line="'+Scanner.CurLine+'"'
        );
      raise E;
      end;
    on E: Exception do
      begin
      writeln('ERROR: TTestResolver.ParseProgram Exception: '+E.ClassName+':'+E.Message);
      raise E;
      end;
  end;
  TAssert.AssertSame('Has resolver',ResolverEngine,Parser.Engine);
  AssertEquals('Has program',TPasProgram,Module.ClassType);
  AssertNotNull('Has program section',PasProgram.ProgramSection);
  AssertNotNull('Has initialization section',PasProgram.InitializationSection);
  if (PasProgram.InitializationSection.Elements.Count>0) then
    if TObject(PasProgram.InitializationSection.Elements[0]) is TPasImplBlock then
      FFirstStatement:=TPasImplBlock(PasProgram.InitializationSection.Elements[0]);
  CheckReferenceDirectives;
end;

procedure TTestResolver.ParseUnit;
begin
  FFirstStatement:=nil;
  try
    ParseModule;
  except
    on E: EParserError do
      begin
      writeln('ERROR: TTestResolver.ParseUnit Parser: '+E.ClassName+':'+E.Message
        +' File='+Scanner.CurFilename
        +' LineNo='+IntToStr(Scanner.CurRow)
        +' Col='+IntToStr(Scanner.CurColumn)
        +' Line="'+Scanner.CurLine+'"'
        );
      raise E;
      end;
    on E: EPasResolve do
      begin
      writeln('ERROR: TTestResolver.ParseUnit PasResolver: '+E.ClassName+':'+E.Message
        +' File='+Scanner.CurFilename
        +' LineNo='+IntToStr(Scanner.CurRow)
        +' Col='+IntToStr(Scanner.CurColumn)
        +' Line="'+Scanner.CurLine+'"'
        );
      raise E;
      end;
    on E: Exception do
      begin
      writeln('ERROR: TTestResolver.ParseUnit Exception: '+E.ClassName+':'+E.Message);
      raise E;
      end;
  end;
  TAssert.AssertSame('Has resolver',ResolverEngine,Parser.Engine);
  AssertEquals('Has unit',TPasModule,Module.ClassType);
  AssertNotNull('Has interface section',Module.InterfaceSection);
  AssertNotNull('Has implementation section',Module.ImplementationSection);
  if (Module.InitializationSection<>nil)
  and (Module.InitializationSection.Elements.Count>0) then
    if TObject(Module.InitializationSection.Elements[0]) is TPasImplBlock then
      FFirstStatement:=TPasImplBlock(Module.InitializationSection.Elements[0]);
  CheckReferenceDirectives;
end;

procedure TTestResolver.CheckReferenceDirectives;
type
  TMarkerKind = (
    mkLabel,
    mkResolverReference,
    mkDirectReference
    );
  PMarker = ^TMarker;
  TMarker = record
    Kind: TMarkerKind;
    Filename: string;
    LineNumber: integer;
    StartCol, EndCol: integer; // token start, end column
    Identifier: string;
    Next: PMarker;
  end;

var
  FirstMarker, LastMarker: PMarker;
  Filename: string;
  LineNumber: Integer;
  SrcLine: String;
  CommentStartP, CommentEndP: PChar;
  FoundRefs: TTestResolverReferenceData;

  procedure GetSrc(Index: integer; out SrcLines: TStringList; out aFilename: string);
  var
    aStream: TStream;
  begin
    SrcLines:=TStringList.Create;
    aStream:=Resolver.Streams.Objects[Index] as TStream;
    aStream.Position:=0;
    SrcLines.LoadFromStream(aStream);
    aFilename:=Resolver.Streams[Index];
  end;

  procedure RaiseErrorAt(Msg: string; const aFilename: string; aLine, aCol: integer);
  var
    s, SrcFilename: String;
    i, j: Integer;
    SrcLines: TStringList;
  begin
    // write all source files
    for i:=0 to Resolver.Streams.Count-1 do
      begin
      GetSrc(i,SrcLines,SrcFilename);
      writeln('Testcode:-File="',SrcFilename,'"----------------------------------:');
      for j:=1 to SrcLines.Count do
        writeln(Format('%:4d: ',[j]),SrcLines[j-1]);
      SrcLines.Free;
      end;
    s:=Msg+' at '+aFilename+' line='+IntToStr(aLine)+', col='+IntToStr(aCol);
    writeln('ERROR: TTestResolver.CheckReferenceDirectives: ',s);
    raise Exception.Create('TTestResolver.CheckReferenceDirectives: '+s);
  end;

  procedure RaiseErrorAt(Msg: string; aMarker: PMarker);
  begin
    RaiseErrorAt(Msg,aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol);
  end;

  procedure RaiseError(Msg: string; p: PChar);
  begin
    RaiseErrorAt(Msg,Filename,LineNumber,p-PChar(SrcLine)+1);
  end;

  procedure AddMarker(Marker: PMarker);
  begin
    if LastMarker<>nil then
      LastMarker^.Next:=Marker
    else
      FirstMarker:=Marker;
    LastMarker:=Marker;
  end;

  function AddMarker(Kind: TMarkerKind; const aFilename: string;
    aLine, aStartCol, aEndCol: integer; const Identifier: string): PMarker;
  begin
    New(Result);
    Result^.Kind:=Kind;
    Result^.Filename:=aFilename;
    Result^.LineNumber:=aLine;
    Result^.StartCol:=aStartCol;
    Result^.EndCol:=aEndCol;
    Result^.Identifier:=Identifier;
    Result^.Next:=nil;
    //writeln('AddMarker Line="',SrcLine,'" Identifier=',Identifier,' Col=',aStartCol,'-',aEndCol,' "',copy(SrcLine,aStartCol,aEndCol-aStartCol),'"');
    AddMarker(Result);
  end;

  function AddMarkerForTokenBehindComment(Kind: TMarkerKind;
    const Identifer: string): PMarker;
  var
    TokenStart, p: PChar;
  begin
    p:=CommentEndP;
    ReadNextPascalToken(p,TokenStart,false,false);
    Result:=AddMarker(Kind,Filename,LineNumber,
      CommentEndP-PChar(SrcLine)+1,p-PChar(SrcLine)+1,Identifer);
  end;

  function FindLabel(const Identifier: string): PMarker;
  begin
    Result:=FirstMarker;
    while Result<>nil do
      begin
      if (Result^.Kind=mkLabel)
      and (CompareText(Result^.Identifier,Identifier)=0) then
        exit;
      Result:=Result^.Next;
      end;
  end;

  function ReadIdentifier(var p: PChar): string;
  var
    StartP: PChar;
  begin
    if not (p^ in ['a'..'z','A'..'Z','_']) then
      RaiseError('identifier expected',p);
    StartP:=p;
    inc(p);
    while p^ in ['a'..'z','A'..'Z','_','0'..'9'] do inc(p);
    SetLength(Result,p-StartP);
    Move(StartP^,Result[1],length(Result));
  end;

  procedure AddLabel;
  var
    Identifier: String;
    p: PChar;
  begin
    p:=CommentStartP+2;
    Identifier:=ReadIdentifier(p);
    //writeln('TTestResolver.CheckReferenceDirectives.AddLabel ',Identifier);
    if FindLabel(Identifier)<>nil then
      RaiseError('duplicate label "'+Identifier+'"',p);
    AddMarkerForTokenBehindComment(mkLabel,Identifier);
  end;

  procedure AddResolverReference;
  var
    Identifier: String;
    p: PChar;
  begin
    p:=CommentStartP+2;
    Identifier:=ReadIdentifier(p);
    //writeln('TTestResolver.CheckReferenceDirectives.AddReference ',Identifier);
    AddMarkerForTokenBehindComment(mkResolverReference,Identifier);
  end;

  procedure AddDirectReference;
  var
    Identifier: String;
    p: PChar;
  begin
    p:=CommentStartP+2;
    Identifier:=ReadIdentifier(p);
    //writeln('TTestResolver.CheckReferenceDirectives.AddDirectReference ',Identifier);
    AddMarkerForTokenBehindComment(mkDirectReference,Identifier);
  end;

  procedure ParseCode(SrcLines: TStringList; aFilename: string);
  var
    p: PChar;
    IsDirective: Boolean;
  begin
    //writeln('TTestResolver.CheckReferenceDirectives.ParseCode File=',aFilename);
    Filename:=aFilename;
    // parse code, find all labels
    LineNumber:=0;
    while LineNumber<SrcLines.Count do
      begin
      inc(LineNumber);
      SrcLine:=SrcLines[LineNumber-1];
      if SrcLine='' then continue;
      //writeln('TTestResolver.CheckReferenceDirectives Line=',SrcLine);
      p:=PChar(SrcLine);
      repeat
        case p^ of
          #0: if (p-PChar(SrcLine)=length(SrcLine)) then break;
          '{':
            begin
            CommentStartP:=p;
            inc(p);
            IsDirective:=p^ in ['#','@','='];

            // skip to end of comment
            repeat
              case p^ of
              #0:
                if (p-PChar(SrcLine)=length(SrcLine)) then
                  begin
                  // multi line comment
                  if IsDirective then
                    RaiseError('directive missing closing bracket',CommentStartP);
                  repeat
                    inc(LineNumber);
                    if LineNumber>SrcLines.Count then exit;
                    SrcLine:=SrcLines[LineNumber-1];
                    //writeln('TTestResolver.CheckReferenceDirectives Comment Line=',SrcLine);
                  until SrcLine<>'';
                  p:=PChar(SrcLine);
                  continue;
                  end;
              '}':
                begin
                inc(p);
                break;
                end;
              end;
              inc(p);
            until false;

            CommentEndP:=p;
            case CommentStartP[1] of
            '#': AddLabel;
            '@': AddResolverReference;
            '=': AddDirectReference;
            end;
            p:=CommentEndP;
            continue;

            end;
          '/':
            if p[1]='/' then
              break; // rest of line is comment -> skip
        end;
        inc(p);
      until false;
      end;
  end;

  function FindElementsAt(aFilename: string; aLine, aStartCol, aEndCol: integer): TFPList;
  var
    ok: Boolean;
  begin
    FoundRefs.Filename:=aFilename;
    FoundRefs.Line:=aLine;
    FoundRefs.StartCol:=aStartCol;
    FoundRefs.EndCol:=aEndCol;
    FoundRefs.Found:=TFPList.Create;
    ok:=false;
    try
      Module.ForEachCall(@OnFindReference,@FoundRefs);
      ok:=true;
    finally
      if not ok then
        FreeAndNil(FoundRefs.Found);
    end;
    Result:=FoundRefs.Found;
    FoundRefs.Found:=nil;
  end;

  procedure CheckResolverReference(aMarker: PMarker);
  // check if one element at {@a} has a TResolvedReference to an element labeled {#a}
  var
    aLabel: PMarker;
    ReferenceElements, LabelElements: TFPList;
    i, j, aLine, aCol: Integer;
    El, Ref, LabelEl: TPasElement;
  begin
    //writeln('CheckResolverReference searching reference: ',aMarker^.Filename,' Line=',aMarker^.LineNumber,' Col=',aMarker^.StartCol,'-',aMarker^.EndCol,' Label="',aMarker^.Identifier,'"');
    aLabel:=FindLabel(aMarker^.Identifier);
    if aLabel=nil then
      RaiseErrorAt('label "'+aMarker^.Identifier+'" not found',aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol);

    LabelElements:=nil;
    ReferenceElements:=nil;
    try
      LabelElements:=FindElementsAt(aLabel^.Filename,aLabel^.LineNumber,aLabel^.StartCol,aLabel^.EndCol);
      if LabelElements.Count=0 then
        RaiseErrorAt('label "'+aLabel^.Identifier+'" has no elements',aLabel);

      ReferenceElements:=FindElementsAt(aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol,aMarker^.EndCol);
      if ReferenceElements.Count=0 then
        RaiseErrorAt('reference "'+aMarker^.Identifier+'" has no elements',aMarker);

      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        Ref:=nil;
        if El.CustomData is TResolvedReference then
          Ref:=TResolvedReference(El.CustomData).Declaration
        else if El.CustomData is TPasPropertyScope then
          Ref:=TPasPropertyScope(El.CustomData).AncestorProp;
        if Ref<>nil then
          for j:=0 to LabelElements.Count-1 do
            begin
            LabelEl:=TPasElement(LabelElements[j]);
            if Ref=LabelEl then
              exit; // success
            end;
        end;

      // failure write candidates
      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        write('Reference candidate for "',aMarker^.Identifier,'" at reference ',aMarker^.Filename,'(',aMarker^.LineNumber,',',aMarker^.StartCol,'-',aMarker^.EndCol,')');
        write(' El=',GetObjName(El));
        Ref:=nil;
        if El.CustomData is TResolvedReference then
          Ref:=TResolvedReference(El.CustomData).Declaration
        else if El.CustomData is TPasPropertyScope then
          Ref:=TPasPropertyScope(El.CustomData).AncestorProp;
        if Ref<>nil then
          begin
          write(' Decl=',GetObjName(Ref));
          ResolverEngine.UnmangleSourceLineNumber(Ref.SourceLinenumber,aLine,aCol);
          write(',',Ref.SourceFilename,'(',aLine,',',aCol,')');
          end
        else
          write(' has no TResolvedReference');
        writeln;
        end;
      for i:=0 to LabelElements.Count-1 do
        begin
        El:=TPasElement(LabelElements[i]);
        write('Label candidate for "',aLabel^.Identifier,'" at reference ',aLabel^.Filename,'(',aLabel^.LineNumber,',',aLabel^.StartCol,'-',aLabel^.EndCol,')');
        write(' El=',GetObjName(El));
        writeln;
        end;

      RaiseErrorAt('wrong resolved reference "'+aMarker^.Identifier+'"',aMarker);
    finally
      LabelElements.Free;
      ReferenceElements.Free;
    end;
  end;

  procedure CheckDirectReference(aMarker: PMarker);
  // check if one element at {=a} is a TPasAliasType pointing to an element labeled {#a}
  var
    aLabel: PMarker;
    ReferenceElements, LabelElements: TFPList;
    i, LabelLine, LabelCol, j: Integer;
    El, LabelEl: TPasElement;
    DeclEl, TypeEl: TPasType;
  begin
    writeln('CheckDirectReference searching pointer: ',aMarker^.Filename,' Line=',aMarker^.LineNumber,' Col=',aMarker^.StartCol,'-',aMarker^.EndCol,' Label="',aMarker^.Identifier,'"');
    aLabel:=FindLabel(aMarker^.Identifier);
    if aLabel=nil then
      RaiseErrorAt('label "'+aMarker^.Identifier+'" not found',aMarker);

    LabelElements:=nil;
    ReferenceElements:=nil;
    try
      writeln('CheckDirectReference finding elements at label ...');
      LabelElements:=FindElementsAt(aLabel^.Filename,aLabel^.LineNumber,aLabel^.StartCol,aLabel^.EndCol);
      if LabelElements.Count=0 then
        RaiseErrorAt('label "'+aLabel^.Identifier+'" has no elements',aLabel);

      writeln('CheckDirectReference finding elements at reference ...');
      ReferenceElements:=FindElementsAt(aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol,aMarker^.EndCol);
      if ReferenceElements.Count=0 then
        RaiseErrorAt('reference "'+aMarker^.Identifier+'" has no elements',aMarker);

      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        writeln('CheckDirectReference ',i,'/',ReferenceElements.Count,' ',GetTreeDesc(El,2));
        if El.ClassType=TPasVariable then
          begin
          if TPasVariable(El).VarType=nil then
            begin
            writeln('CheckDirectReference Var without Type: ',GetObjName(El),' El.Parent=',GetObjName(El.Parent));
            AssertNotNull('TPasVariable(El='+El.Name+').VarType',TPasVariable(El).VarType);
            end;
          TypeEl:=TPasVariable(El).VarType;
          for j:=0 to LabelElements.Count-1 do
            begin
            LabelEl:=TPasElement(LabelElements[j]);
            if TypeEl=LabelEl then
              exit; // success
            end;
          end
        else if El is TPasAliasType then
          begin
          DeclEl:=TPasAliasType(El).DestType;
          ResolverEngine.UnmangleSourceLineNumber(DeclEl.SourceLinenumber,LabelLine,LabelCol);
          if (aLabel^.Filename=DeclEl.SourceFilename)
          and (aLabel^.LineNumber=LabelLine)
          and (aLabel^.StartCol<=LabelCol)
          and (aLabel^.EndCol>=LabelCol) then
            exit; // success
          end
        else if El.ClassType=TPasArgument then
          begin
          TypeEl:=TPasArgument(El).ArgType;
          for j:=0 to LabelElements.Count-1 do
            begin
            LabelEl:=TPasElement(LabelElements[j]);
            if TypeEl=LabelEl then
              exit; // success
            end;
          end;
        end;
      // failed -> show candidates
      writeln('CheckDirectReference failed: Labels:');
      for j:=0 to LabelElements.Count-1 do
        begin
        LabelEl:=TPasElement(LabelElements[j]);
        writeln('  Label ',GetObjName(LabelEl),' at ',ResolverEngine.GetElementSourcePosStr(LabelEl));
        end;
      writeln('CheckDirectReference failed: References:');
      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        writeln('  Reference ',GetObjName(El),' at ',ResolverEngine.GetElementSourcePosStr(El));
        end;
      RaiseErrorAt('wrong direct reference "'+aMarker^.Identifier+'"',aMarker);
    finally
      LabelElements.Free;
      ReferenceElements.Free;
    end;
  end;

var
  aMarker: PMarker;
  i: Integer;
  SrcLines: TStringList;
begin
  Module.ForEachCall(@OnCheckElementParent,nil);
  FirstMarker:=nil;
  LastMarker:=nil;
  FoundRefs:=Default(TTestResolverReferenceData);
  try
    //writeln('TTestResolver.CheckReferenceDirectives find all markers');
    // find all markers
    for i:=0 to Resolver.Streams.Count-1 do
      begin
      GetSrc(i,SrcLines,Filename);
      ParseCode(SrcLines,Filename);
      SrcLines.Free;
      end;

    //writeln('TTestResolver.CheckReferenceDirectives check references');
    // check references
    aMarker:=FirstMarker;
    while aMarker<>nil do
      begin
      case aMarker^.Kind of
      mkResolverReference: CheckResolverReference(aMarker);
      mkDirectReference: CheckDirectReference(aMarker);
      end;
      aMarker:=aMarker^.Next;
      end;
    writeln('TTestResolver.CheckReferenceDirectives COMPLETE');

  finally
    while FirstMarker<>nil do
      begin
      aMarker:=FirstMarker;
      FirstMarker:=FirstMarker^.Next;
      Dispose(aMarker);
      end;
  end;
end;

procedure TTestResolver.CheckResolverException(Msg: string; MsgNumber: integer);
var
  ok: Boolean;
begin
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected '+Msg+', but got msg "'+E.Message+'" number',
        MsgNumber,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('Missing resolver error '+Msg+' ('+IntToStr(MsgNumber)+')',true,ok);
end;

procedure TTestResolver.CheckParserException(Msg: string; MsgNumber: integer);
var
  ok: Boolean;
begin
  ok:=false;
  try
    ParseModule;
  except
    on E: EParserError do
      begin
      AssertEquals('Expected '+Msg+', but got msg "'+E.Message+'" number',
        MsgNumber,Parser.LastMsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('Missing parser error '+Msg+' ('+IntToStr(MsgNumber)+')',true,ok);
end;

function TTestResolver.FindModuleWithFilename(aFilename: string
  ): TTestEnginePasResolver;
var
  i: Integer;
begin
  for i:=0 to ModuleCount-1 do
    if CompareText(Modules[i].Filename,aFilename)=0 then
      exit(Modules[i]);
  Result:=nil;
end;

function TTestResolver.AddModule(aFilename: string): TTestEnginePasResolver;
begin
  //writeln('TTestResolver.AddModule ',aFilename);
  if FindModuleWithFilename(aFilename)<>nil then
    raise Exception.Create('TTestResolver.AddModule: file "'+aFilename+'" already exists');
  Result:=TTestEnginePasResolver.Create;
  Result.Filename:=aFilename;
  Result.AddObjFPCBuiltInIdentifiers;
  Result.OnFindUnit:=@OnPasResolverFindUnit;
  FModules.Add(Result);
end;

function TTestResolver.AddModuleWithSrc(aFilename, Src: string
  ): TTestEnginePasResolver;
begin
  Result:=AddModule(aFilename);
  Result.Source:=Src;
end;

function TTestResolver.AddModuleWithIntfImplSrc(aFilename, InterfaceSrc,
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

procedure TTestResolver.AddSystemUnit(Parts: TSystemUnitParts);
var
  Intf, Impl: TStringList;
begin
  Intf:=TStringList.Create;
  // interface
  Intf.Add('type');
  Intf.Add('  integer=longint;');
  Intf.Add('  sizeint=int64;');
    //'const',
    //'  LineEnding = #10;',
    //'  DirectorySeparator = ''/'';',
    //'  DriveSeparator = '''';',
    //'  AllowDirectorySeparators : set of char = [''\'',''/''];',
    //'  AllowDriveSeparators : set of char = [];',
  if supTObject in Parts then
    begin
    Intf.Add('type');
    Intf.Add('  TObject = class');
    Intf.Add('  end;');
    end;
  Intf.Add('var');
  Intf.Add('  ExitCode: Longint;');
    //'Procedure Move(const source;var dest;count:SizeInt);',

  // implementation
  Impl:=TStringList.Create;
    // 'Procedure Move(const source;var dest;count:SizeInt);',
    // 'begin',
    // 'end;',

  try
    AddModuleWithIntfImplSrc('system.pp',Intf.Text,Impl.Text);
  finally
    Intf.Free;
    Impl.Free;
  end;
end;

procedure TTestResolver.StartProgram(NeedSystemUnit: boolean;
  SystemUnitParts: TSystemUnitParts);
begin
  if NeedSystemUnit then
    AddSystemUnit(SystemUnitParts)
  else
    Parser.ImplicitUses.Clear;
  Add('program '+ExtractFileUnitName(MainFilename)+';');
end;

procedure TTestResolver.StartUnit(NeedSystemUnit: boolean);
begin
  if NeedSystemUnit then
    AddSystemUnit
  else
    Parser.ImplicitUses.Clear;
  Add('unit '+ExtractFileUnitName(MainFilename)+';');
end;

function TTestResolver.OnPasResolverFindUnit(const aUnitName: String
  ): TPasModule;
var
  i: Integer;
  CurEngine: TTestEnginePasResolver;
  CurUnitName: String;
begin
  //writeln('TTestResolver.OnPasResolverFindUnit START Unit="',aUnitName,'"');
  Result:=nil;
  for i:=0 to ModuleCount-1 do
    begin
    CurEngine:=Modules[i];
    CurUnitName:=ExtractFileUnitName(CurEngine.Filename);
    //writeln('TTestResolver.OnPasResolverFindUnit Checking ',i,'/',ModuleCount,' ',CurEngine.Filename,' ',CurUnitName);
    if CompareText(aUnitName,CurUnitName)=0 then
      begin
      Result:=CurEngine.Module;
      if Result<>nil then exit;
      //writeln('TTestResolver.OnPasResolverFindUnit PARSING unit "',CurEngine.Filename,'"');
      Resolver.FindSourceFile(aUnitName);

      CurEngine.Resolver:=TStreamResolver.Create;
      CurEngine.Resolver.OwnsStreams:=True;
      //writeln('TTestResolver.OnPasResolverFindUnit SOURCE=',CurEngine.Source);
      CurEngine.Resolver.AddStream(CurEngine.FileName,TStringStream.Create(CurEngine.Source));
      CurEngine.Scanner:=TPascalScanner.Create(CurEngine.Resolver);
      CurEngine.Parser:=TPasParser.Create(CurEngine.Scanner,CurEngine.Resolver,CurEngine);
      if CompareText(CurUnitName,'System')=0 then
        CurEngine.Parser.ImplicitUses.Clear;
      CurEngine.Scanner.OpenFile(CurEngine.Filename);
      try
        CurEngine.Parser.NextToken;
        CurEngine.Parser.ParseUnit(CurEngine.FModule);
      except
        on E: Exception do
          begin
          writeln('ERROR: TTestResolver.OnPasResolverFindUnit during parsing: '+E.ClassName+':'+E.Message
            +' File='+CurEngine.Scanner.CurFilename
            +' LineNo='+IntToStr(CurEngine.Scanner.CurRow)
            +' Col='+IntToStr(CurEngine.Scanner.CurColumn)
            +' Line="'+CurEngine.Scanner.CurLine+'"'
            );
          raise E;
          end;
      end;
      //writeln('TTestResolver.OnPasResolverFindUnit END ',CurUnitName);
      Result:=CurEngine.Module;
      exit;
      end;
    end;
  writeln('TTestResolver.OnPasResolverFindUnit missing unit "',aUnitName,'"');
  raise Exception.Create('can''t find unit "'+aUnitName+'"');
end;

procedure TTestResolver.OnFindReference(El: TPasElement; FindData: pointer);
var
  Data: PTestResolverReferenceData absolute FindData;
  Line, Col: integer;
begin
  ResolverEngine.UnmangleSourceLineNumber(El.SourceLinenumber,Line,Col);
  //writeln('TTestResolver.OnFindReference ',El.SourceFilename,' Line=',Line,',Col=',Col,' ',GetObjName(El),' SearchFile=',Data^.Filename,',Line=',Data^.Line,',Col=',Data^.StartCol,'-',Data^.EndCol);
  if (Data^.Filename=El.SourceFilename)
  and (Data^.Line=Line)
  and (Data^.StartCol<=Col)
  and (Data^.EndCol>=Col)
  then
    Data^.Found.Add(El);
end;

procedure TTestResolver.OnCheckElementParent(El: TPasElement; arg: pointer);
var
  SubEl: TPasElement;
  i: Integer;

  procedure E(Msg: string);
  var
    s: String;
  begin
    s:='TTestResolver.OnCheckElementParent El='+GetTreeDesc(El)+' '+
      ResolverEngine.GetElementSourcePosStr(El)+' '+Msg;
    writeln('ERROR: ',s);
    raise Exception.Create(s);
  end;

begin
  if arg=nil then ;
  //writeln('TTestResolver.OnCheckElementParent ',GetObjName(El));
  if El=nil then exit;
  if El.Parent=El then
    E('El.Parent=El='+GetObjName(El));
  if El is TBinaryExpr then
    begin
    if (TBinaryExpr(El).left<>nil) and (TBinaryExpr(El).left.Parent<>El) then
      E('TBinaryExpr(El).left.Parent='+GetObjName(TBinaryExpr(El).left.Parent)+'<>El');
    if (TBinaryExpr(El).right<>nil) and (TBinaryExpr(El).right.Parent<>El) then
      E('TBinaryExpr(El).right.Parent='+GetObjName(TBinaryExpr(El).right.Parent)+'<>El');
    end
  else if El is TParamsExpr then
    begin
    if (TParamsExpr(El).Value<>nil) and (TParamsExpr(El).Value.Parent<>El) then
      E('TParamsExpr(El).Value.Parent='+GetObjName(TParamsExpr(El).Value.Parent)+'<>El');
    for i:=0 to length(TParamsExpr(El).Params)-1 do
      if TParamsExpr(El).Params[i].Parent<>El then
        E('TParamsExpr(El).Params[i].Parent='+GetObjName(TParamsExpr(El).Params[i].Parent)+'<>El');
    end
  else if El is TPasDeclarations then
    begin
    for i:=0 to TPasDeclarations(El).Declarations.Count-1 do
      begin
      SubEl:=TPasElement(TPasDeclarations(El).Declarations[i]);
      if SubEl.Parent<>El then
        E('SubEl=TPasElement(TPasDeclarations(El).Declarations[i])='+GetObjName(SubEl)+' SubEl.Parent='+GetObjName(SubEl.Parent)+'<>El');
      end;
    end
  else if El is TPasImplBlock then
    begin
    for i:=0 to TPasImplBlock(El).Elements.Count-1 do
      begin
      SubEl:=TPasElement(TPasImplBlock(El).Elements[i]);
      if SubEl.Parent<>El then
        E('TPasElement(TPasImplBlock(El).Elements[i]).Parent='+GetObjName(SubEl.Parent)+'<>El');
      end;
    end
  else if El is TPasImplWithDo then
    begin
    for i:=0 to TPasImplWithDo(El).Expressions.Count-1 do
      begin
      SubEl:=TPasExpr(TPasImplWithDo(El).Expressions[i]);
      if SubEl.Parent<>El then
        E('TPasExpr(TPasImplWithDo(El).Expressions[i]).Parent='+GetObjName(SubEl.Parent)+'<>El');
      end;
    end;
end;

function TTestResolver.GetModules(Index: integer): TTestEnginePasResolver;
begin
  Result:=TTestEnginePasResolver(FModules[Index]);
end;

function TTestResolver.GetModuleCount: integer;
begin
  Result:=FModules.Count;
end;

procedure TTestResolver.TestEmpty;
begin
  StartProgram(false);
  Add('begin');
  ParseProgram;
  AssertEquals('No statements',0,PasProgram.InitializationSection.Elements.Count);
end;

procedure TTestResolver.TestAliasType;
var
  El: TPasElement;
  T: TPasAliasType;
begin
  StartProgram(false);
  Add('type');
  Add('  tint=longint;');
  Add('begin');
  ParseProgram;
  AssertEquals('1 declaration',1,PasProgram.ProgramSection.Declarations.Count);
  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('Type',TPasAliasType,El.ClassType);
  T:=TPasAliasType(El);
  AssertEquals('Type tint','tint',T.Name);
  AssertEquals('Type built-in',TPasUnresolvedSymbolRef,T.DestType.ClassType);
  AssertEquals('longint type','longint',lowercase(T.DestType.Name));
end;

procedure TTestResolver.TestAlias2Type;
var
  El: TPasElement;
  T1, T2: TPasAliasType;
  DestT1, DestT2: TPasType;
begin
  StartProgram(false);
  Add('type');
  Add('  tint1=longint;');
  Add('  tint2=tint1;');
  Add('begin');
  ParseProgram;
  AssertEquals('2 declaration',2,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('Type',TPasAliasType,El.ClassType);
  T1:=TPasAliasType(El);
  AssertEquals('Type tint1','tint1',T1.Name);
  DestT1:=T1.DestType;
  AssertEquals('built-in',TPasUnresolvedSymbolRef,DestT1.ClassType);
  AssertEquals('built-in longint','longint',lowercase(DestT1.Name));

  El:=TPasElement(PasProgram.ProgramSection.Declarations[1]);
  AssertEquals('Type',TPasAliasType,El.ClassType);
  T2:=TPasAliasType(El);
  AssertEquals('Type tint2','tint2',T2.Name);
  DestT2:=T2.DestType;
  AssertEquals('points to alias type',TPasAliasType,DestT2.ClassType);
  AssertEquals('points to tint1','tint1',DestT2.Name);
end;

procedure TTestResolver.TestAliasTypeRefs;
begin
  StartProgram(false);
  Add('type');
  Add('  {#a}a=longint;');
  Add('  {#b}{=a}b=a;');
  Add('var');
  Add('  {=a}c: a;');
  Add('  {=b}d: b;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestAliasOfVarFail;
begin
  StartProgram(false);
  Add('var');
  Add('  a: char;');
  Add('type');
  Add('  t=a;');
  Add('begin');
  CheckParserException('Expected type, but got variable',PParser.nParserExpectedTypeButGot);
end;

procedure TTestResolver.TestVarLongint;
var
  El: TPasElement;
  V1: TPasVariable;
  DestT1: TPasType;
begin
  StartProgram(false);
  Add('var');
  Add('  v1:longint;');
  Add('begin');
  ParseProgram;
  AssertEquals('1 declaration',1,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('var',TPasVariable,El.ClassType);
  V1:=TPasVariable(El);
  AssertEquals('var v1','v1',V1.Name);
  DestT1:=V1.VarType;
  AssertEquals('built-in',TPasUnresolvedSymbolRef,DestT1.ClassType);
  AssertEquals('built-in longint','longint',lowercase(DestT1.Name));
end;

procedure TTestResolver.TestVarInteger;
var
  El: TPasElement;
  V1: TPasVariable;
  DestT1: TPasType;
begin
  StartProgram(true);
  Add('var');
  Add('  v1:integer;'); // defined in system.pp
  Add('begin');
  ParseProgram;
  AssertEquals('1 declaration',1,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('var',TPasVariable,El.ClassType);
  V1:=TPasVariable(El);
  AssertEquals('var v1','v1',V1.Name);
  DestT1:=V1.VarType;
  AssertNotNull('v1 type',DestT1);
  AssertEquals('built-in',TPasAliasType,DestT1.ClassType);
  AssertEquals('built-in integer','integer',DestT1.Name);
  AssertNull('v1 no expr',V1.Expr);
end;

procedure TTestResolver.TestConstInteger;
var
  El: TPasElement;
  C1: TPasConst;
  DestT1: TPasType;
  ExprC1: TPrimitiveExpr;
begin
  StartProgram(true);
  Add('const');
  Add('  c1:integer=3;'); // defined in system.pp
  Add('begin');
  ParseProgram;
  AssertEquals('1 declaration',1,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('const',TPasConst,El.ClassType);
  C1:=TPasConst(El);
  AssertEquals('const c1','c1',C1.Name);
  DestT1:=C1.VarType;
  AssertNotNull('c1 type',DestT1);
  AssertEquals('built-in',TPasAliasType,DestT1.ClassType);
  AssertEquals('built-in integer','integer',DestT1.Name);
  ExprC1:=TPrimitiveExpr(C1.Expr);
  AssertNotNull('c1 expr',ExprC1);
  AssertEquals('c1 expr primitive',TPrimitiveExpr,ExprC1.ClassType);
  AssertEquals('c1 expr value','3',ExprC1.Value);
end;

procedure TTestResolver.TestDuplicateVar;
begin
  StartProgram(false);
  Add('var a: longint;');
  Add('var a: string;');
  Add('begin');
  CheckResolverException('duplicate identifier',PasResolver.nDuplicateIdentifier);
end;

procedure TTestResolver.TestVarOfVarFail;
begin
  StartProgram(false);
  Add('var');
  Add('  a: char;');
  Add('  b: a;');
  Add('begin');
  CheckParserException('Expected type, but got variable',PParser.nParserExpectedTypeButGot);
end;

procedure TTestResolver.TestConstOfVarFail;
begin
  StartProgram(false);
  Add('var');
  Add('  a: longint;');
  Add('const');
  Add('  b: a = 1;');
  Add('begin');
  CheckParserException('Expected type, but got variable',PParser.nParserExpectedTypeButGot);
end;

procedure TTestResolver.TestTypedConstWrongExprFail;
begin
  StartProgram(false);
  Add('const');
  Add('  a: string = 1;');
  Add('begin');
  CheckResolverException('Expected type, but got variable',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestIncDec;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('begin');
  Add('  inc(i);');
  Add('  inc(i,2);');
  Add('  dec(i);');
  Add('  dec(i,3);');
  ParseProgram;
end;

procedure TTestResolver.TestIncStringFail;
begin
  StartProgram(false);
  Add('var');
  Add('  i: string;');
  Add('begin');
  Add('  inc(i);');
  CheckResolverException('Incompatible type arg no. 1: Got "String", expected "Longint"',PasResolver.nIncompatibleTypeArgNo);
end;

procedure TTestResolver.TestEnums;
begin
  StartProgram(false);
  Add('type {#TFlag}TFlag = ({#Red}Red, {#Green}Green, {#Blue}Blue);');
  Add('var');
  Add('  {#f}{=TFlag}f: TFlag;');
  Add('  {#v}{=TFlag}v: TFlag;');
  Add('begin');
  Add('  {@f}f:={@Red}Red;');
  Add('  {@f}f:={@v}v;');
  Add('  if {@f}f={@Red}Red then ;');
  Add('  if {@f}f={@v}v then ;');
  Add('  if {@f}f>{@v}v then ;');
  Add('  if {@f}f<{@v}v then ;');
  Add('  if {@f}f>={@v}v then ;');
  Add('  if {@f}f<={@v}v then ;');
  Add('  if {@f}f<>{@v}v then ;');
  Add('  if ord({@f}f)<>ord({@Red}Red) then ;');
  Add('  {@f}f:={@TFlag}TFlag.{@Red}Red;');
  ParseProgram;
end;

procedure TTestResolver.TestSets;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TFlag}TFlag = ({#Red}Red, {#Green}Green, {#Blue}Blue, {#Gray}Gray, {#Black}Black, {#White}White);');
  Add('  {#TFlags}TFlags = set of TFlag;');
  Add('  {#TChars}TChars = set of Char;');
  Add('  {#TMyInt}TMyInt = 0..17;');
  Add('  {#TMyInts}TMyInts = set of TMyInt;');
  Add('  {#TMyBools}TMyBools = set of boolean;');
  Add('const');
  Add('  {#Colors}Colors = [{@Red}Red..{@Blue}Blue];');
  Add('  {#ExtColors}ExtColors = {@Colors}Colors+[{@White}White,{@Black}Black];');
  Add('var');
  Add('  {#f}{=TFlag}f: TFlag;');
  Add('  {#s}{=TFlags}s: TFlags;');
  Add('  {#t}{=TFlags}t: TFlags;');
  Add('  {#Chars}{=TChars}Chars: TChars;');
  Add('  {#MyInts}{=TMyInts}MyInts: TMyInts;');
  Add('  {#MyBools}{=TMyBools}MyBools: TMyBools;');
  Add('begin');
  Add('  {@s}s:=[];');
  Add('  {@s}s:={@t}t;');
  Add('  {@s}s:=[{@Red}Red];');
  Add('  {@s}s:=[{@Red}Red,{@Blue}Blue];');
  Add('  {@s}s:=[{@Gray}Gray..{@White}White];');
  Add('  {@s}s:=[{@Red}Red]+[{@Blue}Blue,{@Gray}Gray];');
  Add('  {@s}s:=[{@Blue}Blue,{@Gray}Gray]-[{@Blue}Blue];');
  Add('  {@s}s:={@t}t+[];');
  Add('  {@s}s:=[{@Red}Red]+{@s}s;');
  Add('  {@s}s:={@s}s+[{@Red}Red];');
  Add('  {@s}s:=[{@Red}Red]-{@s}s;');
  Add('  {@s}s:={@s}s-[{@Red}Red];');
  Add('  Include({@s}s,{@Blue}Blue);');
  Add('  Exclude({@s}s,{@Blue}Blue);');
  Add('  {@s}s:={@s}s+[{@f}f];');
  Add('  if {@Green}Green in {@s}s then ;');
  Add('  if {@Blue}Blue in {@Colors}Colors then ;');
  Add('  if {@f}f in {@ExtColors}ExtColors then ;');
  Add('  {@s}s:={@s}s * Colors;');
  Add('  {@s}s:=Colors * {@s}s;');
  Add('  s:=ExtColors * Colors;');
  Add('  s:=Colors >< ExtColors;');
  Add('  s:=s >< ExtColors;');
  Add('  s:=ExtColors >< s;');
  Add('  if ''p'' in [''a''..''z''] then ; ');
  Add('  if ''p'' in [''a''..''z'',''A''..''Z'',''0''..''9'',''_''] then ; ');
  Add('  if ''p'' in {@Chars}Chars then ; ');
  Add('  if 7 in {@MyInts}MyInts then ; ');
  Add('  if 7 in [1+2,(3*4)+5,(-2+6)..(8-3)] then ; ');
  Add('  {@MyInts}MyInts:=[1];');
  Add('  {@MyInts}MyInts:=[1,2];');
  Add('  {@MyInts}MyInts:=[1..2];');
  Add('  {@MyInts}MyInts:=[1..2,3];');
  Add('  {@MyInts}MyInts:=[1..2,3..4];');
  Add('  {@MyInts}MyInts:=[1,2..3];');
  Add('  {@MyBools}MyBools:=[false];');
  Add('  {@MyBools}MyBools:=[false,true];');
  Add('  {@MyBools}MyBools:=[true..false];');
  Add('  if [red,blue]*s=[red,blue] then ;');
  ParseProgram;
end;

procedure TTestResolver.TestEnumParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TFlag = (red, green, blue);');
  Add('function {#A1}FuncA: TFlag;');
  Add('begin');
  Add('  Result:=red;');
  Add('end;');
  Add('function {#A2}FuncA(f: TFlag): TFlag;');
  Add('begin');
  Add('  Result:=f;');
  Add('end;');
  Add('var');
  Add('  f: TFlag;');
  Add('begin');
  Add('  f:={@A1}FuncA;');
  Add('  f:={@A1}FuncA();');
  Add('  f:={@A2}FuncA(f);');
  ParseProgram;
end;

procedure TTestResolver.TestSetParams;
begin
  StartProgram(false);
  Add('type');
  Add('  TFlag = (red, green, blue);');
  Add('  TFlags = set of TFlag;');
  Add('function {#A1}FuncA: TFlags;');
  Add('begin');
  Add('  Result:=[red];');
  Add('end;');
  Add('function {#A2}FuncA(f: TFlags): TFlags;');
  Add('begin');
  Add('  Result:=f;');
  Add('end;');
  Add('var');
  Add('  f: TFlags;');
  Add('begin');
  Add('  f:={@A1}FuncA;');
  Add('  f:={@A1}FuncA();');
  Add('  f:={@A2}FuncA(f);');
  Add('  f:={@A2}FuncA([green]);');
  ParseProgram;
end;

procedure TTestResolver.TestPrgAssignment;
var
  El: TPasElement;
  V1: TPasVariable;
  ImplAssign: TPasImplAssign;
  Ref1: TPrimitiveExpr;
  Resolver1: TResolvedReference;
begin
  StartProgram(false);
  Add('var');
  Add('  v1:longint;');
  Add('begin');
  Add('  v1:=3;');
  ParseProgram;
  AssertEquals('1 declaration',1,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('var',TPasVariable,El.ClassType);
  V1:=TPasVariable(El);
  AssertEquals('var v1','v1',V1.Name);

  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
  AssertEquals('Assignment statement',TPasImplAssign,FFirstStatement.ClassType);
  ImplAssign:=FFirstStatement as TPasImplAssign;
  AssertEquals('Normal assignment',akDefault,ImplAssign.Kind);
  AssertExpression('Right side is constant',ImplAssign.Right,pekNumber,'3');
  AssertExpression('Left side is variable',ImplAssign.Left,pekIdent,'v1');
  AssertEquals('Left side is variable, primitive',TPrimitiveExpr,ImplAssign.Left.ClassType);
  Ref1:=TPrimitiveExpr(ImplAssign.Left);
  AssertNotNull('variable has customdata',Ref1.CustomData);
  AssertEquals('variable has resolver',TResolvedReference,Ref1.CustomData.ClassType);
  Resolver1:=TResolvedReference(Ref1.CustomData);
  AssertSame('variable resolver element',Resolver1.Element,Ref1);
  AssertSame('variable resolver declaration v1',Resolver1.Declaration,V1);
end;

procedure TTestResolver.TestPrgProcVar;
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
  ParseProgram;
  AssertEquals('1 declaration',1,PasProgram.ProgramSection.Declarations.Count);
end;

procedure TTestResolver.TestUnitProcVar;
var
  El: TPasElement;
  IntfProc1, ImplProc1: TPasProcedure;
  IntfType1, ProcSubType1: TPasAliasType;
  ImplVar1, ProcSubVar1: TPasVariable;
  ImplVar1Type, ProcSubVar1Type: TPasType;
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
  ParseUnit;

  // interface
  AssertEquals('2 intf declarations',2,Module.InterfaceSection.Declarations.Count);
  El:=TPasElement(Module.InterfaceSection.Declarations[0]);
  AssertEquals('intf type',TPasAliasType,El.ClassType);
  IntfType1:=TPasAliasType(El);
  AssertEquals('intf type t1','t1',IntfType1.Name);

  El:=TPasElement(Module.InterfaceSection.Declarations[1]);
  AssertEquals('intf proc',TPasProcedure,El.ClassType);
  IntfProc1:=TPasProcedure(El);
  AssertEquals('intf proc Proc1','Proc1',IntfProc1.Name);

  // implementation
  AssertEquals('2 impl declarations',2,Module.ImplementationSection.Declarations.Count);
  El:=TPasElement(Module.ImplementationSection.Declarations[0]);
  AssertEquals('impl proc',TPasProcedure,El.ClassType);
  ImplProc1:=TPasProcedure(El);
  AssertEquals('impl proc Proc1','Proc1',ImplProc1.Name);

  El:=TPasElement(Module.ImplementationSection.Declarations[1]);
  AssertEquals('impl var',TPasVariable,El.ClassType);
  ImplVar1:=TPasVariable(El);
  AssertEquals('impl var v2','v2',ImplVar1.Name);
  ImplVar1Type:=TPasType(ImplVar1.VarType);
  AssertSame('impl var type is intf t1',IntfType1,ImplVar1Type);

  // proc
  AssertEquals('2 proc sub declarations',2,ImplProc1.Body.Declarations.Count);

  // proc sub type t1
  El:=TPasElement(ImplProc1.Body.Declarations[0]);
  AssertEquals('proc sub type',TPasAliasType,El.ClassType);
  ProcSubType1:=TPasAliasType(El);
  AssertEquals('proc sub type t1','t1',ProcSubType1.Name);

  // proc sub var v1
  El:=TPasElement(ImplProc1.Body.Declarations[1]);
  AssertEquals('proc sub var',TPasVariable,El.ClassType);
  ProcSubVar1:=TPasVariable(El);
  AssertEquals('proc sub var v1','v1',ProcSubVar1.Name);
  ProcSubVar1Type:=TPasType(ProcSubVar1.VarType);
  AssertSame('proc sub var type is proc sub t1',ProcSubType1,ProcSubVar1Type);
end;

procedure TTestResolver.TestAssignIntegers;
begin
  StartProgram(false);
  Add('var');
  Add('  {#vbyte}vbyte:byte;');
  Add('  {#vshortint}vshortint:shortint;');
  Add('  {#vword}vword:word;');
  Add('  {#vsmallint}vsmallint:smallint;');
  Add('  {#vcardinal}vcardinal:cardinal;');
  Add('  {#vlongint}vlongint:longint;');
  Add('  {#vint64}vint64:int64;');
  Add('  {#vcomp}vcomp:comp;');
  Add('begin');
  Add('  {@vbyte}vbyte:=0;');
  Add('  {@vbyte}vbyte:=255;');
  Add('  {@vshortint}vshortint:=0;');
  Add('  {@vshortint}vshortint:=-128;');
  Add('  {@vshortint}vshortint:= 127;');
  Add('  {@vword}vword:=0;');
  Add('  {@vword}vword:=+$ffff;');
  Add('  {@vsmallint}vsmallint:=0;');
  Add('  {@vsmallint}vsmallint:=-$8000;');
  Add('  {@vsmallint}vsmallint:= $7fff;');
  Add('  {@vcardinal}vcardinal:=0;');
  Add('  {@vcardinal}vcardinal:=$ffffffff;');
  Add('  {@vlongint}vlongint:=0;');
  Add('  {@vlongint}vlongint:=-$80000000;');
  Add('  {@vlongint}vlongint:= $7fffffff;');
  Add('  {@vlongint}vlongint:={@vbyte}vbyte;');
  Add('  {@vlongint}vlongint:={@vshortint}vshortint;');
  Add('  {@vlongint}vlongint:={@vword}vword;');
  Add('  {@vlongint}vlongint:={@vsmallint}vsmallint;');
  Add('  {@vlongint}vlongint:={@vlongint}vlongint;');
  Add('  {@vcomp}vcomp:=0;');
  Add('  {@vcomp}vcomp:=$ffffffffffffffff;');
  Add('  {@vint64}vint64:=0;');
  Add('  {@vint64}vint64:=-$8000000000000000;');
  Add('  {@vint64}vint64:= $7fffffffffffffff;');
  ParseProgram;
end;

procedure TTestResolver.TestAssignString;
begin
  StartProgram(false);
  Add('var');
  Add('  vstring:string;');
  Add('  vchar:char;');
  Add('begin');
  Add('  vstring:='''';');
  Add('  vstring:=''abc'';');
  Add('  vstring:=''a'';');
  Add('  vchar:=''c'';');
  Add('  vchar:=vstring[1];');
  ParseProgram;
end;

procedure TTestResolver.TestAssignIntToStringFail;
begin
  StartProgram(false);
  Add('var');
  Add('  vstring:string;');
  Add('begin');
  Add('  vstring:=2;');
  CheckResolverException('Incompatible types: got "Longint" expected "String"',
    PasResolver.nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestAssignStringToIntFail;
begin
  StartProgram(false);
  Add('var');
  Add('  v:longint;');
  Add('begin');
  Add('  v:=''A'';');
  CheckResolverException('Incompatible types: got "String" expected "Longint"',
    PasResolver.nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestIntegerOperators;
begin
  StartProgram(false);
  Add('var');
  Add('  i,j,k:longint;');
  Add('begin');
  Add('  i:=1;');
  Add('  i:=1+2;');
  Add('  i:=1+2+3;');
  Add('  i:=1-2;');
  Add('  i:=j;');
  Add('  i:=j+1;');
  Add('  i:=-j+1;');
  Add('  i:=j+k;');
  Add('  i:=-j+k;');
  Add('  i:=j*k;');
  Add('  i:=j**k;');
  Add('  i:=j div k;');
  Add('  i:=j mod k;');
  Add('  i:=j shl k;');
  Add('  i:=j shr k;');
  Add('  i:=j and k;');
  Add('  i:=j or k;');
  Add('  i:=j and not k;');
  Add('  i:=(j+k) div 3;');
  ParseProgram;
end;

procedure TTestResolver.TestBooleanOperators;
begin
  StartProgram(false);
  Add('var');
  Add('  i,j,k:boolean;');
  Add('begin');
  Add('  i:=false;');
  Add('  i:=true;');
  Add('  i:=j and k;');
  Add('  i:=j or k;');
  Add('  i:=j or not k;');
  Add('  i:=(not j) or k;');
  Add('  i:=j or false;');
  Add('  i:=j and true;');
  ParseProgram;
end;

procedure TTestResolver.TestStringOperators;
begin
  StartProgram(false);
  Add('var');
  Add('  i,j:string;');
  Add('  k:char;');
  Add('begin');
  Add('  i:='''';');
  Add('  i:=''''+'''';');
  Add('  i:=k+'''';');
  Add('  i:=''''+k;');
  Add('  i:=''a''+j;');
  Add('  i:=''abc''+j;');
  Add('  k:=j;');
  Add('  k:=''a'';');
  ParseProgram;
end;

procedure TTestResolver.TestFloatOperators;
begin
  StartProgram(false);
  Add('var');
  Add('  i,j,k:double;');
  Add('begin');
  Add('  i:=1;');
  Add('  i:=1+2;');
  Add('  i:=1+2+3;');
  Add('  i:=1-2;');
  Add('  i:=j;');
  Add('  i:=j+1;');
  Add('  i:=-j+1;');
  Add('  i:=j+k;');
  Add('  i:=-j+k;');
  Add('  i:=j*k;');
  Add('  i:=j/k;');
  Add('  i:=j**k;');
  Add('  i:=(j+k)/3;');
  ParseProgram;
end;

procedure TTestResolver.TestStringElementMissingArgFail;
begin
  StartProgram(false);
  Add('var s: string;');
  Add('begin');
  Add('  if s[]=s then ;');
  CheckResolverException('Missing parameter character index',PasResolver.nMissingParameterX);
end;

procedure TTestResolver.TestStringElementIndexNonIntFail;
begin
  StartProgram(false);
  Add('var s: string;');
  Add('begin');
  Add('  if s[true]=s then ;');
  CheckResolverException('Incompatible types: got "Boolean" expected "Comp"',
    PasResolver.nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestCAssignments;
begin
  StartProgram(false);
  Parser.Options:=Parser.Options+[po_cassignments];
  Scanner.Options:=Scanner.Options+[po_cassignments];
  Add('Type');
  Add('  TFlag = (Flag1,Flag2);');
  Add('  TFlags = set of TFlag;');
  Add('var');
  Add('  i: longint;');
  Add('  c: char;');
  Add('  s: string;');
  Add('  d: double;');
  Add('  f: TFlag;');
  Add('  fs: TFlags;');
  Add('begin');
  Add('  i+=1;');
  Add('  i-=2;');
  Add('  i*=3;');
  Add('  s+=''A'';');
  Add('  s:=c;');
  Add('  d+=4;');
  Add('  d-=5;');
  Add('  d*=6;');
  Add('  d/=7;');
  Add('  d+=8.5;');
  Add('  d-=9.5;');
  Add('  d*=10.5;');
  Add('  d/=11.5;');
  Add('  fs+=[f];');
  Add('  fs-=[f];');
  Add('  fs*=[f];');
  Add('  fs+=[Flag1];');
  Add('  fs-=[Flag1];');
  Add('  fs*=[Flag1];');
  Add('  fs+=[Flag1,Flag2];');
  Add('  fs-=[Flag1,Flag2];');
  Add('  fs*=[Flag1,Flag2];');
  ParseProgram;
end;

procedure TTestResolver.TestTypeCastBaseTypes;
begin
  StartProgram(false);
  Add('var');
  Add('  si: smallint;');
  Add('  i: longint;');
  Add('  fs: single;');
  Add('  d: double;');
  Add('  b: boolean;');
  Add('begin');
  Add('  d:=double(i);');
  Add('  i:=shortint(i);');
  Add('  i:=longint(si);');
  Add('  d:=double(d);');
  Add('  fs:=single(d);');
  Add('  d:=single(d);');
  Add('  b:=longbool(b);');
  Add('  b:=bytebool(longbool(b));');
  Add('  d:=double(i)/2.5;');
  Add('  b:=boolean(i);');
  ParseProgram;
end;

procedure TTestResolver.TestTypeCastStrToIntFail;
begin
  StartProgram(false);
  Add('var');
  Add('  s: string;');
  Add('  i: longint;');
  Add('begin');
  Add('  i:=longint(s);');
  CheckResolverException('illegal type conversion: string to longint',PasResolver.nIllegalTypeConversionTo);
end;

procedure TTestResolver.TestTypeCastIntToStrFail;
begin
  StartProgram(false);
  Add('var');
  Add('  s: string;');
  Add('  i: longint;');
  Add('begin');
  Add('  s:=string(i);');
  CheckResolverException('illegal type conversion: longint to string',PasResolver.nIllegalTypeConversionTo);
end;

procedure TTestResolver.TestTypeCastDoubleToStrFail;
begin
  StartProgram(false);
  Add('var');
  Add('  s: string;');
  Add('  d: double;');
  Add('begin');
  Add('  s:=string(d);');
  CheckResolverException('illegal type conversion: double to string',PasResolver.nIllegalTypeConversionTo);
end;

procedure TTestResolver.TestTypeCastDoubleToIntFail;
begin
  StartProgram(false);
  Add('var');
  Add('  i: longint;');
  Add('  d: double;');
  Add('begin');
  Add('  i:=longint(d);');
  CheckResolverException('illegal type conversion: double to longint',PasResolver.nIllegalTypeConversionTo);
end;

procedure TTestResolver.TestForLoop;
begin
  StartProgram(false);
  Add('var');
  Add('  {#v1}v1,{#v2}v2,{#v3}v3:longint;');
  Add('begin');
  Add('  for {@v1}v1:=');
  Add('    {@v2}v2');
  Add('    to {@v3}v3 do ;');
  ParseProgram;
end;

procedure TTestResolver.TestStatements;
begin
  StartProgram(false);
  Add('var');
  Add('  v1,v2,v3:longint;');
  Add('begin');
  Add('  v1:=1;');
  Add('  v2:=v1+v1*v1+v1 div v1;');
  Add('  v3:=-v1;');
  Add('  repeat');
  Add('    v1:=v1+1;');
  Add('  until v1>=5;');
  Add('  while v1>=0 do');
  Add('    v1:=v1-v2;');
  Add('  for v1:=v2 to v3 do v2:=v1;');
  Add('  if v1<v2 then v3:=v1 else v3:=v2;');
  ParseProgram;
  AssertEquals('3 declarations',3,PasProgram.ProgramSection.Declarations.Count);
end;

procedure TTestResolver.TestCaseStatement;
begin
  StartProgram(false);
  Add('const');
  Add('  {#c1}c1=1;');
  Add('  {#c2}c2=1;');
  Add('var');
  Add('  {#v1}v1,{#v2}v2,{#v3}v3:longint;');
  Add('begin');
  Add('  Case {@v1}v1+{@v2}v2 of');
  Add('  {@c1}c1:');
  Add('    {@v2}v2:={@v3}v3;');
  Add('  {@c1}c1,{@c2}c2: ;');
  Add('  {@c1}c1..{@c2}c2: ;');
  Add('  {@c1}c1+{@c2}c2: ;');
  Add('  else');
  Add('    {@v1}v1:=3;');
  Add('  end;');
  ParseProgram;
end;

procedure TTestResolver.TestTryStatement;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('  {#Exec}Exception = class end;');
  Add('var');
  Add('  {#v1}v1,{#e1}e:longint;');
  Add('begin');
  Add('  try');
  Add('    {@v1}v1:={@e1}e;');
  Add('  finally');
  Add('    {@v1}v1:={@e1}e;');
  Add('  end');
  Add('  try');
  Add('    {@v1}v1:={@e1}e;');
  Add('  except');
  Add('    {@v1}v1:={@e1}e;');
  Add('  end');
  Add('  try');
  Add('    {@v1}v1:={@e1}e;');
  Add('  except');
  Add('    on {#e2}{=Exec}E: Exception do');
  Add('      if {@e2}e=nil then ;');
  Add('    on {#e3}{=Exec}E: Exception do');
  Add('      raise {@e3}e;');
  Add('    else');
  Add('      {@v1}v1:={@e1}e;');
  Add('  end');
  ParseProgram;
end;

procedure TTestResolver.TestTryExceptOnNonTypeFail;
begin
  StartProgram(false);
  Add('type TObject = class end;');
  Add('var E: TObject;');
  Add('begin');
  Add('  try');
  Add('  except');
  Add('    on E do ;');
  Add('  end;');
  CheckParserException('Expected type, but got variable',PParser.nParserExpectedTypeButGot);
end;

procedure TTestResolver.TestTryExceptOnNonClassFail;
begin
  StartProgram(false);
  Add('begin');
  Add('  try');
  Add('  except');
  Add('    on longint do ;');
  Add('  end;');
  CheckResolverException('class expected but longint found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestRaiseNonVarFail;
begin
  StartProgram(false);
  Add('type TObject = class end;');
  Add('begin');
  Add('  raise TObject;');
  CheckResolverException('var expected but type found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestRaiseNonClassFail;
begin
  StartProgram(false);
  Add('var');
  Add('  E: longint;');
  Add('begin');
  Add('  raise E;');
  CheckResolverException('class expected but longint found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestStatementsRefs;
begin
  StartProgram(false);
  Add('var');
  Add('  {#v1}v1,{#v2}v2,{#v3}v3:longint;');
  Add('begin');
  Add('  {@v1}v1:=1;');
  Add('  {@v2}v2:=');
  Add('    {@v1}v1+');
  Add('    {@v1}v1*{@v1}v1');
  Add('    +{@v1}v1 div {@v1}v1;');
  Add('  {@v3}v3:=');
  Add('    -{@v1}v1;');
  Add('  repeat');
  Add('    {@v1}v1:=');
  Add('      {@v1}v1+1;');
  Add('  until {@v1}v1>=5;');
  Add('  while {@v1}v1>=0 do');
  Add('    {@v1}v1');
  Add('    :={@v1}v1-{@v2}v2;');
  Add('  if {@v1}v1<{@v2}v2 then');
  Add('    {@v3}v3:={@v1}v1');
  Add('  else {@v3}v3:=');
  Add('    {@v2}v2;');
  ParseProgram;
  AssertEquals('3 declarations',3,PasProgram.ProgramSection.Declarations.Count);
end;

procedure TTestResolver.TestRepeatUntilNonBoolFail;
begin
  StartProgram(false);
  Add('begin');
  Add('  repeat');
  Add('  until 3;');
  CheckResolverException('boolean expected but longint found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestWhileDoNonBoolFail;
begin
  StartProgram(false);
  Add('begin');
  Add('  while 3 do ;');
  CheckResolverException('boolean expected but longint found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestIfThenNonBoolFail;
begin
  StartProgram(false);
  Add('begin');
  Add('  if 3 then ;');
  CheckResolverException('boolean expected but longint found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestForLoopVarNonVarFail;
begin
  StartProgram(false);
  Add('const i = 3;');
  Add('begin');
  Add('  for i:=1 to 2 do ;');
  CheckResolverException('variable identifier expected',nVariableIdentifierExpected);
end;

procedure TTestResolver.TestForLoopStartIncompFail;
begin
  StartProgram(false);
  Add('var i: char;');
  Add('begin');
  Add('  for i:=1 to 2 do ;');
  CheckResolverException('Incompatible types: got "Longint" expected "Char"',
    nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestForLoopEndIncompFail;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  for i:=1 to ''2'' do ;');
  CheckResolverException('Incompatible types: got "Char" expected "Longint"',
    nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestCaseOf;
begin
  StartProgram(false);
  Add('type');
  Add('  TFlag = (red,green,blue);');
  Add('var');
  Add('  i: longint;');
  Add('  f: TFlag;');
  Add('  b: boolean;');
  Add('  c: char;');
  Add('  s: string;');
  Add('begin');
  Add('  case i of');
  Add('  1: ;');
  Add('  2..3: ;');
  Add('  4,5..6,7: ;');
  Add('  else');
  Add('  end;');
  Add('  case f of');
  Add('  red: ;');
  Add('  red..green: ;');
  Add('  end;');
  Add('  case b of');
  Add('  true: ;');
  Add('  false: ;');
  Add('  end;');
  Add('  case c of');
  Add('  #0: ;');
  Add('  #10,#13: ;');
  Add('  ''0''..''9'',''a''..''z'': ;');
  Add('  end;');
  Add('  case s of');
  Add('  #10: ;');
  Add('  ''abc'': ;');
  Add('  end;');
  ParseProgram;
end;

procedure TTestResolver.TestCaseExprNonOrdFail;
begin
  StartProgram(false);
  Add('begin');
  Add('  case longint of');
  Add('  1: ;');
  Add('  end;');
  CheckResolverException('const expression expected, but Longint found',
    nXExpectedButYFound);
end;

procedure TTestResolver.TestCaseIncompatibleValueFail;
begin
  StartProgram(false);
  Add('var i: longint;');
  Add('begin');
  Add('  case i of');
  Add('  ''1'': ;');
  Add('  end;');
  CheckResolverException('Incompatible types: got "Longint" expected "Char"',
    nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestUnitRef;
var
  El, DeclEl, OtherUnit: TPasElement;
  LocalVar: TPasVariable;
  Assign1, Assign2, Assign3: TPasImplAssign;
  Prim1, Prim2: TPrimitiveExpr;
  BinExp: TBinaryExpr;
begin
  StartUnit(true);
  Add('interface');
  Add('var exitCOde: string;');
  Add('implementation');
  Add('initialization');
  Add('  ExitcodE:=''1'';');
  Add('  afile.eXitCode:=''2'';');
  Add('  System.exiTCode:=3;');
  ParseUnit;

  // interface
  AssertEquals('1 intf declaration',1,Module.InterfaceSection.Declarations.Count);
  El:=TPasElement(Module.InterfaceSection.Declarations[0]);
  AssertEquals('local var',TPasVariable,El.ClassType);
  LocalVar:=TPasVariable(El);
  AssertEquals('local var exitcode','exitCOde',LocalVar.Name);

  // initialization
  AssertEquals('3 initialization statements',3,Module.InitializationSection.Elements.Count);

  // check direct assignment to local var
  El:=TPasElement(Module.InitializationSection.Elements[0]);
  AssertEquals('direct assign',TPasImplAssign,El.ClassType);
  Assign1:=TPasImplAssign(El);
  AssertEquals('direct assign left',TPrimitiveExpr,Assign1.left.ClassType);
  Prim1:=TPrimitiveExpr(Assign1.left);
  AssertNotNull(Prim1.CustomData);
  AssertEquals('direct assign left ref',TResolvedReference,Prim1.CustomData.ClassType);
  DeclEl:=TResolvedReference(Prim1.CustomData).Declaration;
  AssertSame('direct assign local var',LocalVar,DeclEl);

  // check indirect assignment to local var: "afile.eXitCode"
  El:=TPasElement(Module.InitializationSection.Elements[1]);
  AssertEquals('indirect assign',TPasImplAssign,El.ClassType);
  Assign2:=TPasImplAssign(El);
  AssertEquals('indirect assign left',TBinaryExpr,Assign2.left.ClassType);
  BinExp:=TBinaryExpr(Assign2.left);
  AssertEquals('indirect assign first token',TPrimitiveExpr,BinExp.left.ClassType);
  Prim1:=TPrimitiveExpr(BinExp.left);
  AssertEquals('indirect assign first token','afile',Prim1.Value);
  AssertNotNull(Prim1.CustomData);
  AssertEquals('indirect assign unit ref resolved',TResolvedReference,Prim1.CustomData.ClassType);
  DeclEl:=TResolvedReference(Prim1.CustomData).Declaration;
  AssertSame('indirect assign unit ref',Module,DeclEl);

  AssertEquals('indirect assign dot',eopSubIdent,BinExp.OpCode);

  AssertEquals('indirect assign second token',TPrimitiveExpr,BinExp.right.ClassType);
  Prim2:=TPrimitiveExpr(BinExp.right);
  AssertEquals('indirect assign second token','eXitCode',Prim2.Value);
  AssertNotNull(Prim2.CustomData);
  AssertEquals('indirect assign var ref resolved',TResolvedReference,Prim2.CustomData.ClassType);
  AssertEquals('indirect assign left ref',TResolvedReference,Prim2.CustomData.ClassType);
  DeclEl:=TResolvedReference(Prim2.CustomData).Declaration;
  AssertSame('indirect assign local var',LocalVar,DeclEl);

  // check assignment to "system.ExitCode"
  El:=TPasElement(Module.InitializationSection.Elements[2]);
  AssertEquals('other unit assign',TPasImplAssign,El.ClassType);
  Assign3:=TPasImplAssign(El);
  AssertEquals('other unit assign left',TBinaryExpr,Assign3.left.ClassType);
  BinExp:=TBinaryExpr(Assign3.left);
  AssertEquals('othe unit assign first token',TPrimitiveExpr,BinExp.left.ClassType);
  Prim1:=TPrimitiveExpr(BinExp.left);
  AssertEquals('other unit assign first token','System',Prim1.Value);
  AssertNotNull(Prim1.CustomData);
  AssertEquals('other unit assign unit ref resolved',TResolvedReference,Prim1.CustomData.ClassType);
  DeclEl:=TResolvedReference(Prim1.CustomData).Declaration;
  OtherUnit:=DeclEl;
  AssertEquals('other unit assign unit ref',TPasModule,DeclEl.ClassType);
  AssertEquals('other unit assign unit ref system','system',lowercase(DeclEl.Name));

  AssertEquals('other unit assign dot',eopSubIdent,BinExp.OpCode);

  AssertEquals('other unit assign second token',TPrimitiveExpr,BinExp.right.ClassType);
  Prim2:=TPrimitiveExpr(BinExp.right);
  AssertEquals('other unit assign second token','exiTCode',Prim2.Value);
  AssertNotNull(Prim2.CustomData);
  AssertEquals('other unit assign var ref resolved',TResolvedReference,Prim2.CustomData.ClassType);
  AssertEquals('other unit assign left ref',TResolvedReference,Prim2.CustomData.ClassType);
  DeclEl:=TResolvedReference(Prim2.CustomData).Declaration;
  AssertEquals('other unit assign var',TPasVariable,DeclEl.ClassType);
  AssertEquals('other unit assign var exitcode','exitcode',lowercase(DeclEl.Name));
  AssertSame('other unit assign var exitcode',OtherUnit,DeclEl.GetModule);
end;

procedure TTestResolver.TestProcParam;
begin
  StartProgram(false);
  Add('procedure Proc1(a: longint);');
  Add('begin');
  Add('  a:=3;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestFunctionResult;
begin
  StartProgram(false);
  Add('function Func1: longint;');
  Add('begin');
  Add('  Result:=3;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestProcOverload;
var
  El: TPasElement;
begin
  StartProgram(false);
  Add('function Func1(i: longint; j: longint = 0): longint; overload;');
  Add('begin');
  Add('  Result:=1;');
  Add('end;');
  Add('function Func1(s: string): longint; overload;');
  Add('begin');
  Add('  Result:=2;');
  Add('end;');
  Add('begin');
  Add('  Func1(3);');
  ParseProgram;
  AssertEquals('2 declarations',2,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('is function',TPasFunction,El.ClassType);

  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
end;

procedure TTestResolver.TestProcOverloadWithBaseTypes;
begin
  StartProgram(false);
  Add('function {#A}Func1(i: longint; j: longint = 0): longint; overload;');
  Add('begin');
  Add('  Result:=1;');
  Add('end;');
  Add('function {#B}Func1(s: string): longint; overload;');
  Add('begin');
  Add('  Result:=2;');
  Add('end;');
  Add('begin');
  Add('  {@A}Func1(3);');
  ParseProgram;
end;

procedure TTestResolver.TestProcOverloadWithClassTypes;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class end;');
  Add('  {#TA}TClassA = class end;');
  Add('  {#TB}TClassB = class end;');
  Add('procedure {#DoA}DoIt({=TA}p: TClassA); overload;');
  Add('begin');
  Add('end;');
  Add('procedure {#DoB}DoIt({=TB}p: TClassB); overload;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#A}{=TA}A: TClassA;');
  Add('  {#B}{=TB}B: TClassB;');
  Add('begin');
  Add('  {@DoA}DoIt({@A}A)');
  Add('  {@DoB}DoIt({@B}B)');
  ParseProgram;
end;

procedure TTestResolver.TestProcOverloadWithInhClassTypes;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class end;');
  Add('  {#TA}TClassA = class end;');
  Add('  {#TB}TClassB = class(TClassA) end;');
  Add('  {#TC}TClassC = class(TClassB) end;');
  Add('procedure {#DoA}DoIt({=TA}p: TClassA); overload;');
  Add('begin');
  Add('end;');
  Add('procedure {#DoB}DoIt({=TB}p: TClassB); overload;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#A}{=TA}A: TClassA;');
  Add('  {#B}{=TB}B: TClassB;');
  Add('  {#C}{=TC}C: TClassC;');
  Add('begin');
  Add('  {@DoA}DoIt({@A}A)');
  Add('  {@DoB}DoIt({@B}B)');
  Add('  {@DoB}DoIt({@C}C)');
  ParseProgram;
end;

procedure TTestResolver.TestProcOverloadWithInhAliasClassTypes;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class end;');
  Add('  {#TA}TClassA = class end;');
  Add('  {#TB}{=TA}TClassB = TClassA;');
  Add('  {#TC}TClassC = class(TClassB) end;');
  Add('procedure {#DoA}DoIt({=TA}p: TClassA); overload;');
  Add('begin');
  Add('end;');
  Add('procedure {#DoC}DoIt({=TC}p: TClassC); overload;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#A}{=TA}A: TClassA;');
  Add('  {#B}{=TB}B: TClassB;');
  Add('  {#C}{=TC}C: TClassC;');
  Add('begin');
  Add('  {@DoA}DoIt({@A}A)');
  Add('  {@DoA}DoIt({@B}B)');
  Add('  {@DoC}DoIt({@C}C)');
  ParseProgram;
end;

procedure TTestResolver.TestProcDuplicate;
begin
  StartProgram(false);
  Add('procedure ProcA(i: longint);');
  Add('begin');
  Add('end;');
  Add('procedure ProcA(i: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('duplicate identifier',PasResolver.nDuplicateIdentifier);
end;

procedure TTestResolver.TestNestedProc;
begin
  StartProgram(false);
  Add('function DoIt({#a1}a,{#d1}d: longint): longint;');
  Add('var');
  Add('  {#b1}b: longint;');
  Add('  {#c1}c: longint;');
  Add('  function {#Nesty1}Nesty({#a2}a: longint): longint; ');
  Add('  var {#b2}b: longint;');
  Add('  begin');
  Add('    Result:={@a2}a');
  Add('      +{@b2}b');
  Add('      +{@c1}c');
  Add('      +{@d1}d;');
  Add('  end;');
  Add('begin');
  Add('  Result:={@a1}a');
  Add('      +{@b1}b');
  Add('      +{@c1}c;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestForwardProc;
begin
  StartProgram(false);
  Add('procedure {#A_forward}FuncA(i: longint); forward;');
  Add('procedure {#B}FuncB(i: longint);');
  Add('begin');
  Add('  {@A_forward}FuncA(i);');
  Add('end;');
  Add('procedure {#A}FuncA(i: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  {@A_forward}FuncA(3);');
  Add('  {@B}FuncB(3);');
  ParseProgram;
end;

procedure TTestResolver.TestForwardProcUnresolved;
begin
  StartProgram(false);
  Add('procedure FuncA(i: longint); forward;');
  Add('begin');
  CheckResolverException('forward proc not resolved',PasResolver.nForwardProcNotResolved);
end;

procedure TTestResolver.TestNestedForwardProc;
begin
  StartProgram(false);
  Add('procedure {#A}FuncA;');
  Add('  procedure {#B_forward}ProcB(i: longint); forward;');
  Add('  procedure {#C}ProcC(i: longint);');
  Add('  begin');
  Add('    {@B_forward}ProcB(i);');
  Add('  end;');
  Add('  procedure {#B}ProcB(i: longint);');
  Add('  begin');
  Add('  end;');
  Add('begin');
  Add('  {@B_forward}ProcB(3);');
  Add('  {@C}ProcC(3);');
  Add('end;');
  Add('begin');
  Add('  {@A}FuncA;');
  ParseProgram;
end;

procedure TTestResolver.TestNestedForwardProcUnresolved;
begin
  StartProgram(false);
  Add('procedure FuncA;');
  Add('  procedure ProcB(i: longint); forward;');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('forward proc not resolved',PasResolver.nForwardProcNotResolved);
end;

procedure TTestResolver.TestForwardProcFuncMismatch;
begin
  StartProgram(false);
  Add('procedure DoIt; forward;');
  Add('function DoIt: longint;');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('procedure expected, but function found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestForwardFuncResultMismatch;
begin
  StartProgram(false);
  Add('function DoIt: longint; forward;');
  Add('function DoIt: string;');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('Result type mismatch',PasResolver.nResultTypeMismatchExpectedButFound);
end;

procedure TTestResolver.TestUnitIntfProc;
begin
  StartUnit(false);
  Add('interface');
  Add('procedure {#A_forward}FuncA(i: longint);');
  Add('implementation');
  Add('procedure {#A}FuncA(i: longint);');
  Add('begin');
  Add('end;');
  Add('initialization');
  Add('  {@A_forward}FuncA(3);');
  ParseUnit;
end;

procedure TTestResolver.TestUnitIntfProcUnresolved;
begin
  StartUnit(false);
  Add('interface');
  Add('procedure {#A_forward}FuncA(i: longint);');
  Add('implementation');
  Add('initialization');
  CheckResolverException('forward proc not resolved',PasResolver.nForwardProcNotResolved);
end;

procedure TTestResolver.TestUnitIntfMismatchArgName;
begin
  StartUnit(false);
  Add('interface');
  Add('procedure {#A_forward}ProcA(i: longint);');
  Add('implementation');
  Add('procedure {#A}ProcA(j: longint);');
  Add('begin');
  Add('end;');
  CheckResolverException('function header "ProcA" doesn''t match forward : var name changes',
    PasResolver.nFunctionHeaderMismatchForwardVarName);
end;

procedure TTestResolver.TestProcOverloadIsNotFunc;
begin
  StartUnit(false);
  Add('interface');
  Add('var ProcA: longint;');
  Add('procedure {#A_Decl}ProcA(i: longint);');
  Add('implementation');
  Add('procedure {#A_Impl}ProcA(i: longint);');
  Add('begin');
  Add('end;');
  CheckResolverException('Duplicate identifier',PasResolver.nDuplicateIdentifier);
end;

procedure TTestResolver.TestProcCallMissingParams;
begin
  StartProgram(false);
  Add('procedure Proc1(a: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  Proc1;');
  CheckResolverException('Wrong number of parameters for call to "Proc1"',
    PasResolver.nWrongNumberOfParametersForCallTo);
end;

procedure TTestResolver.TestBuiltInProcCallMissingParams;
begin
  StartProgram(false);
  Add('begin');
  Add('  length;');
  CheckResolverException('Wrong number of parameters for call to "length"',
    PasResolver.nWrongNumberOfParametersForCallTo);
end;

procedure TTestResolver.TestAssignFunctionResult;
begin
  StartProgram(false);
  Add('function {#F1}F1: longint;');
  Add('begin');
  Add('end;');
  Add('function {#F2}F2: longint;');
  Add('begin');
  Add('end;');
  Add('var {#i}i: longint;');
  Add('begin');
  Add('  {@i}i:={@F1}F1();');
  Add('  {@i}i:={@F1}F1()+{@F2}F2();');
  Add('  {@i}i:={@F1}F1;');
  Add('  {@i}i:={@F1}F1+{@F2}F2;');
  ParseProgram;
end;

procedure TTestResolver.TestAssignProcResultFail;
begin
  StartProgram(false);
  Add('procedure {#P}P;');
  Add('begin');
  Add('end;');
  Add('var {#i}i: longint;');
  Add('begin');
  Add('  {@i}i:={@P}P();');
  CheckResolverException('function expected, but procedure found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestFunctionResultInCondition;
begin
  StartProgram(false);
  Add('function {#F1}F1: longint;');
  Add('begin');
  Add('end;');
  Add('function {#F2}F2: boolean;');
  Add('begin');
  Add('end;');
  Add('var {#i}i: longint;');
  Add('begin');
  Add('  if {@F2}F2 then ;');
  Add('  if {@i}i={@F1}F1() then ;');
  Add('  if {@i}i={@F1}F1 then ;');
  ParseProgram;
end;

procedure TTestResolver.TestExit;
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
  ParseProgram;
end;

procedure TTestResolver.TestRecord;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TRec}TRec = record');
  Add('    {#Size}Size: longint;');
  Add('  end;');
  Add('var');
  Add('  {#r}{=TRec}r: TRec;');
  Add('begin');
  Add('  {@r}r.{@Size}Size:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestRecordVariant;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TRec}TRec = record');
  Add('    {#Size}Size: longint;');
  Add('    case {#vari}vari: longint of');
  Add('    0: ({#b}b: longint)');
  Add('  end;');
  Add('var');
  Add('  {#r}{=TRec}r: TRec;');
  Add('begin');
  Add('  {@r}r.{@Size}Size:=3;');
  Add('  {@r}r.{@vari}vari:=4;');
  Add('  {@r}r.{@b}b:=5;');
  ParseProgram;
end;

procedure TTestResolver.TestRecordVariantNested;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TRec}TRec = record');
  Add('    {#Size}Size: longint;');
  Add('    case {#vari}vari: longint of');
  Add('    0: ({#b}b: longint)');
  Add('    1: ({#c}c:');
  Add('          record');
  Add('            {#d}d: longint;');
  Add('            case {#e}e: longint of');
  Add('            0: ({#f}f: longint)');
  Add('          end)');
  Add('  end;');
  Add('var');
  Add('  {#r}{=TRec}r: TRec;');
  Add('begin');
  Add('  {@r}r.{@Size}Size:=3;');
  Add('  {@r}r.{@vari}vari:=4;');
  Add('  {@r}r.{@b}b:=5;');
  Add('  {@r}r.{@c}c.{@d}d:=6;');
  Add('  {@r}r.{@c}c.{@e}e:=7;');
  Add('  {@r}r.{@c}c.{@f}f:=8;');
  ParseProgram;
end;

procedure TTestResolver.TestClass;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#B}b: longint;');
  Add('  end;');
  Add('var');
  Add('  {#C}{=TOBJ}c: TObject;');
  Add('begin');
  Add('  {@C}c.{@b}b:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestClassDefaultInheritance;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#OBJ_a}a: longint;');
  Add('    {#OBJ_b}b: longint;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#A_a}a: longint;');
  Add('    {#A_c}c: longint;');
  Add('  end;');
  Add('var');
  Add('  {#V}{=A}v: TClassA;');
  Add('begin');
  Add('  {@V}v.{@A_c}c:=2;');
  Add('  {@V}v.{@OBJ_b}b:=3;');
  Add('  {@V}v.{@A_a}a:=4;');
  ParseProgram;
end;

procedure TTestResolver.TestClassTripleInheritance;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#OBJ_a}a: longint;');
  Add('    {#OBJ_b}b: longint;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#A_a}a: longint;');
  Add('    {#A_c}c: longint;');
  Add('  end;');
  Add('  {#B}TClassB = class(TClassA)');
  Add('    {#B_a}a: longint;');
  Add('    {#B_d}d: longint;');
  Add('  end;');
  Add('var');
  Add('  {#V}{=B}v: TClassB;');
  Add('begin');
  Add('  {@V}v.{@B_d}d:=1;');
  Add('  {@V}v.{@A_c}c:=2;');
  Add('  {@V}v.{@OBJ_B}b:=3;');
  Add('  {@V}v.{@B_a}a:=4;');
  ParseProgram;
end;

procedure TTestResolver.TestClassForward;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  {#B_forward}TClassB = class;');
  Add('  {#A}TClassA = class');
  Add('    {#A_a}a: longint;');
  Add('    {#A_b}{=B_forward}b: TClassB;');
  Add('  end;');
  Add('  {#B}TClassB = class(TClassA)');
  Add('    {#B_a}a: longint;');
  Add('    {#B_d}d: longint;');
  Add('  end;');
  Add('var');
  Add('  {#V}{=B}v: TClassB;');
  Add('begin');
  Add('  {@V}v.{@B_d}d:=1;');
  Add('  {@V}v.{@B_a}a:=2;');
  Add('  {@V}v.{@A_b}b:=nil;');
  Add('  {@V}v.{@A_b}b.{@B_a}a:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestClassForwardNotResolved;
var
  ErrorNo: Integer;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TClassB = class;');
  Add('var');
  Add('  v: TClassB;');
  Add('begin');
  ErrorNo:=0;
  try
    ParseModule;
  except
    on E: EPasResolve do
      ErrorNo:=E.MsgNumber;
  end;
  AssertEquals('Forward class not resolved raises correct error',nForwardTypeNotResolved,ErrorNo);
end;

procedure TTestResolver.TestClassMethod;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    procedure {#A_ProcA_Decl}ProcA;');
  Add('  end;');
  Add('procedure TClassA.ProcA;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#V}{=A}v: TClassA;');
  Add('begin');
  Add('  {@V}v.{@A_ProcA_Decl}ProcA;');
  ParseProgram;
end;

procedure TTestResolver.TestClassMethodUnresolved;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TClassA = class');
  Add('    procedure ProcA;');
  Add('  end;');
  Add('begin');
  CheckResolverException('forward proc not resolved',PasResolver.nForwardProcNotResolved);
end;

procedure TTestResolver.TestClassMethodAbstract;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; virtual; abstract;');
  Add('  end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassMethodAbstractWithoutVirtual;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; abstract;');
  Add('  end;');
  Add('begin');
  CheckResolverException('abstract without virtual',PasResolver.nInvalidProcModifiers);
end;

procedure TTestResolver.TestClassMethodAbstractHasBody;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; virtual; abstract;');
  Add('  end;');
  Add('procedure TObject.ProcA;');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('abstract must not have implementation',
    PasResolver.nAbstractMethodsMustNotHaveImplementation);
end;

procedure TTestResolver.TestClassMethodUnresolvedWithAncestor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; virtual; abstract;');
  Add('  end;');
  Add('  TClassA = class');
  Add('    procedure ProcA;');
  Add('  end;');
  Add('begin');
  CheckResolverException('forward proc not resolved',PasResolver.nForwardProcNotResolved);
end;

procedure TTestResolver.TestClassProcFuncMismatch;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt;');
  Add('  end;');
  Add('function TObject.DoIt: longint;');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('procedure expected, but function found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestClassMethodOverload;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt;');
  Add('    procedure DoIt(i: longint);');
  Add('    procedure DoIt(s: string);');
  Add('  end;');
  Add('procedure TObject.DoIt;');
  Add('begin');
  Add('end;');
  Add('procedure TObject.DoIt(i: longint);');
  Add('begin');
  Add('end;');
  Add('procedure TObject.DoIt(s: string);');
  Add('begin');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassMethodInvalidOverload;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure DoIt(i: longint);');
  Add('    procedure DoIt(k: longint);');
  Add('  end;');
  Add('procedure TObject.DoIt(i: longint);');
  Add('begin');
  Add('end;');
  Add('procedure TObject.DoIt(k: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckResolverException('Duplicate identifier',PasResolver.nDuplicateIdentifier);
end;

procedure TTestResolver.TestClassOverride;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure {#TOBJ_ProcA}ProcA; virtual; abstract;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    procedure {#A_ProcA}ProcA; override;');
  Add('  end;');
  Add('procedure TClassA.ProcA;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#V}{=A}v: TClassA;');
  Add('begin');
  Add('  {@V}v.{@A_ProcA}ProcA;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOverride2;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure {#TOBJ_ProcA}ProcA; virtual; abstract;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    procedure {#A_ProcA}ProcA; override;');
  Add('  end;');
  Add('  {#B}TClassB = class');
  Add('    procedure {#B_ProcA}ProcA; override;');
  Add('  end;');
  Add('procedure TClassA.ProcA;');
  Add('begin');
  Add('end;');
  Add('procedure TClassB.ProcA;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#V}{=B}v: TClassB;');
  Add('begin');
  Add('  {@V}v.{@B_ProcA}ProcA;');
  ParseProgram;
end;

procedure TTestResolver.TestClassMethodScope;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#A_A}A: longint;');
  Add('    procedure {#A_ProcB}ProcB;');
  Add('  end;');
  Add('procedure TClassA.ProcB;');
  Add('begin');
  Add('  {@A_A}A:=3;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassIdentifierSelf;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    {#C}C: longint;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#B}B: longint;');
  Add('    procedure {#A_ProcB}ProcB;');
  Add('  end;');
  Add('procedure TClassA.ProcB;');
  Add('begin');
  Add('  {@B}B:=1;');
  Add('  {@C}C:=2;');
  Add('  Self.{@B}B:=3;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassCallInherited;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure {#TOBJ_ProcA}ProcA(i: longint); virtual;');
  Add('    procedure {#TOBJ_ProcB}ProcB(j: longint); virtual;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    procedure {#A_ProcA}ProcA(i: longint); override;');
  Add('    procedure {#A_ProcB}ProcB(j: longint); override;');
  Add('  end;');
  Add('procedure TObject.ProcA(i: longint);');
  Add('begin');
  Add('  inherited; // ignore and do not raise error');
  Add('end;');
  Add('procedure TObject.ProcB(j: longint);');
  Add('begin');
  Add('end;');
  Add('procedure TClassA.ProcA({#i1}i: longint);');
  Add('begin');
  Add('  {@A_ProcA}ProcA({@i1}i);');
  Add('  {@TOBJ_ProcA}inherited;');
  Add('  inherited {@TOBJ_ProcA}ProcA({@i1}i);');
  Add('  {@A_ProcB}ProcB({@i1}i);');
  Add('  inherited {@TOBJ_ProcB}ProcB({@i1}i);');
  Add('end;');
  Add('procedure TClassA.ProcB(j: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassCallInheritedNoParamsAbstractFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; virtual; abstract;');
  Add('  end;');
  Add('  TClassA = class');
  Add('    procedure ProcA; override;');
  Add('  end;');
  Add('procedure TClassA.ProcA;');
  Add('begin');
  Add('  inherited;');
  Add('end;');
  Add('begin');
  CheckResolverException('Abstract methods cannot be called directly',
    PasResolver.nAbstractMethodsCannotBeCalledDirectly);
end;

procedure TTestResolver.TestClassCallInheritedWithParamsAbstractFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA(c: char); virtual; abstract;');
  Add('  end;');
  Add('  TClassA = class');
  Add('    procedure ProcA(c: char); override;');
  Add('  end;');
  Add('procedure TClassA.ProcA(c: char);');
  Add('begin');
  Add('  inherited ProcA(c);');
  Add('end;');
  Add('begin');
  CheckResolverException('Abstract methods cannot be called directly',
    PasResolver.nAbstractMethodsCannotBeCalledDirectly);
end;

procedure TTestResolver.TestClassCallInheritedConstructor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor {#TOBJ_CreateA}Create(i: longint); virtual;');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    constructor {#A_CreateA}Create(i: longint); override;');
  Add('  end;');
  Add('constructor TObject.Create(i: longint);');
  Add('begin');
  Add('  inherited; // ignore and do not raise error');
  Add('end;');
  Add('constructor TClassA.Create({#i1}i: longint);');
  Add('begin');
  Add('  {@A_CreateA}Create({@i1}i);');
  Add('  {@TOBJ_CreateA}inherited;');
  Add('  inherited {@TOBJ_CreateA}Create({@i1}i);');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassAssignNil;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#FSub}FSub: TClassA;');
  Add('    property {#Sub}Sub: TClassA read {@FSub}FSub write {@FSub}FSub;');
  Add('  end;');
  Add('var');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  {@v}v:=nil;');
  Add('  if {@v}v=nil then ;');
  Add('  if nil={@v}v then ;');
  Add('  if {@v}v<>nil then ;');
  Add('  if nil<>{@v}v then ;');
  Add('  {@v}v.{@FSub}FSub:=nil;');
  Add('  if {@v}v.{@FSub}FSub=nil then ;');
  Add('  if {@v}v.{@FSub}FSub<>nil then ;');
  Add('  {@v}v.{@Sub}Sub:=nil;');
  Add('  if {@v}v.{@Sub}Sub=nil then ;');
  Add('  if {@v}v.{@Sub}Sub<>nil then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassAssign;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#FSub}FSub: TClassA;');
  Add('    property {#Sub}Sub: TClassA read {@FSub}FSub write {@FSub}FSub;');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('  {#p}{=A}p: TClassA;');
  Add('begin');
  Add('  {@o}o:={@v}v;');
  Add('  {@v}v:={@p}p;');
  Add('  if {@v}v={@p}p then ;');
  Add('  if {@v}v={@o}o then ;');
  Add('  if {@o}o={@o}o then ;');
  Add('  if {@o}o={@v}v then ;');
  Add('  if {@v}v<>{@p}p then ;');
  Add('  if {@v}v<>{@o}o then ;');
  Add('  if {@o}o<>{@o}o then ;');
  Add('  if {@o}o<>{@v}v then ;');
  Add('  {@v}v.{@FSub}FSub:={@p}p;');
  Add('  {@p}p:={@v}v.{@FSub}FSub;');
  Add('  {@o}o:={@v}v.{@FSub}FSub;');
  Add('  {@v}v.{@Sub}Sub:={@p}p;');
  Add('  {@p}p:={@v}v.{@Sub}Sub;');
  Add('  {@o}o:={@v}v.{@Sub}Sub;');
  ParseProgram;
end;

procedure TTestResolver.TestClassNilAsParam;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('procedure ProcP(o: TObject);');
  Add('begin end;');
  Add('begin');
  Add('  ProcP(nil);');
  ParseProgram;
end;

procedure TTestResolver.TestClassOperator_Is_As;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#Sub}Sub: TClassA;');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  if {@o}o is {@A}TClassA then;');
  Add('  if {@v}v is {@A}TClassA then;');
  Add('  if {@v}v.{@Sub}Sub is {@A}TClassA then;');
  Add('  {@v}v:={@o}o as {@A}TClassA;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOperatorIsOnNonDescendantFail;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  if {@v}v is {@TObj}TObject then;');
  CheckResolverException('types are not related',PasResolver.nTypesAreNotRelated);
end;

procedure TTestResolver.TestClassOperatorIsOnNonTypeFail;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  if {@o}o is {@v}v then;');
  CheckResolverException('class type expected, but got variable',
    PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestClassOperatorAsOnNonDescendantFail;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  {@o}o:={@v}v as {@TObj}TObject;');
  CheckResolverException('types are not related',PasResolver.nTypesAreNotRelated);
end;

procedure TTestResolver.TestClassOperatorAsOnNonTypeFail;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  {@o}o:={@v}v as {@o}o;');
  CheckResolverException('class expected, but o found" number',
    PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestClassAsFuncResult;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('     {#A_i}i: longint;');
  Add('     constructor {#A_CreateA}Create;');
  Add('     constructor {#A_CreateB}Create(i: longint);');
  Add('  end;');
  Add('function {#F}F: TClassA;');
  Add('begin');
  Add('  Result:=nil;');
  Add('end;');
  Add('constructor TClassA.Create;');
  Add('begin');
  Add('end;');
  Add('constructor TClassA.Create(i: longint);');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  {@o}o:={@F}F;');
  Add('  {@o}o:={@F}F();');
  Add('  {@v}v:={@F}F;');
  Add('  {@v}v:={@F}F();');
  Add('  if {@o}o={@F}F then ;');
  Add('  if {@o}o={@F}F() then ;');
  Add('  if {@v}v={@F}F then ;');
  Add('  if {@v}v={@F}F() then ;');
  Add('  {@v}v:={@A}TClassA.{@A_CreateA}Create;');
  Add('  {@v}v:={@A}TClassA.{@A_CreateA}Create();');
  Add('  {@v}v:={@A}TClassA.{@A_CreateB}Create(3);');
  Add('  {@A}TClassA.{@A_CreateA}Create.{@A_i}i:=3;');
  Add('  {@A}TClassA.{@A_CreateA}Create().{@A_i}i:=3;');
  Add('  {@A}TClassA.{@A_CreateB}Create(3).{@A_i}i:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestClassTypeCast;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    id: longint;');
  Add('  end;');
  Add('procedure ProcA(var a: TClassA);');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  {@o}o:={@v}v;');
  Add('  {@o}o:=TObject({@o}o);');
  Add('  {@v}v:=TClassA({@o}o);');
  Add('  {@v}v:=TClassA(TObject({@o}o));');
  Add('  {@v}v:=TClassA({@v}v);');
  Add('  ProcA({@v}v);');
  Add('  ProcA(TClassA({@o}o));');
  Add('  if TClassA({@o}o).id=3 then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassTypeCastUnrelatedFail;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    id: longint;');
  Add('  end;');
  Add('  {#B}TClassB = class');
  Add('    Name: string;');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#va}{=A}va: TClassA;');
  Add('  {#vb}{=B}vb: TClassB;');
  Add('begin');
  Add('  {@vb}vb:=TClassB({@va}va);');
  CheckResolverException('Illegal type conversion: "class TClassA" to "TClassB"',
    PasResolver.nIllegalTypeConversionTo);
end;

procedure TTestResolver.TestClass_TypeCastSelf;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    constructor Create;');
  Add('    procedure ProcA;');
  Add('  end;');
  Add('  TClassA = class');
  Add('    id: longint;');
  Add('  end;');
  Add('constructor TObject.Create;');
  Add('begin');
  Add('  TClassA(Self).id:=3;');
  Add('  if TClassA(Self).id=4 then;');
  Add('  if 5=TClassA(Self).id then;');
  Add('end;');
  Add('procedure TObject.ProcA;');
  Add('begin');
  Add('  TClassA(Self).id:=3;');
  Add('  if TClassA(Self).id=4 then;');
  Add('  if 5=TClassA(Self).id then;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClass_AccessMemberViaClassFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    i: longint;');
  Add('  end;');
  Add('begin');
  Add('  if TObject.i=7 then ;');
  CheckResolverException('Only class methods, class properties and class variables can be referred with class references',
    PasResolver.nOnlyClassMembersCanBeReferredWithClassReferences);
end;

procedure TTestResolver.TestClass_FuncReturningObjectMember;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    i: longint;');
  Add('  end;');
  Add('function FuncO: TObject;');
  Add('begin');
  Add('end;');
  Add('begin');
  Add('  FuncO.i:=3;');
  Add('  if FuncO.i=4 then ;');
  Add('  if 5=FuncO.i then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClass_StaticWithoutClassFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; static;');
  Add('  end;');
  Add('procedure TObject.ProcA; begin end;');
  Add('begin');
  CheckResolverException('Invalid procedure modifiers static',PasResolver.nInvalidProcModifiers);
end;

procedure TTestResolver.TestClass_SelfInStaticFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure ProcA; static;');
  Add('  end;');
  Add('class procedure TObject.ProcA;');
  Add('begin');
  Add('  if Self=nil then ;');
  Add('end;');
  Add('begin');
  CheckResolverException('identifier not found "Self"',PasResolver.nIdentifierNotFound);
end;

procedure TTestResolver.TestClassOf;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TClass}{=TObj}TClass = class of TObject;');
  Add('  {#TOBJ}TObject = class');
  Add('    ClassType: TClass; ');
  Add('  end;');
  Add('type');
  Add('  {#TMobile}TMobile = class');
  Add('  end;');
  Add('  {#TMobiles}{=TMobile}TMobiles = class of TMobile;');
  Add('type');
  Add('  {#TCars}{=TCar}TCars = class of TCar;');
  Add('  {#TShips}{=TShip}TShips = class of TShip;');
  Add('  {#TCar}TCar = class(TMobile)');
  Add('  end;');
  Add('  {#TShip}TShip = class(TMobile)');
  Add('  end;');
  Add('var');
  Add('  o: TObject;');
  Add('  c: TClass;');
  Add('  mobile: TMobile;');
  Add('  mobiletype: TMobiles;');
  Add('  car: TCar;');
  Add('  cartype: TCars;');
  Add('  ship: TShip;');
  Add('  shiptype: TShips;');
  Add('begin');
  Add('  c:=nil;');
  Add('  c:=o.ClassType;');
  Add('  if c=o.ClassType then ;');
  Add('  if c<>o.ClassType then ;');
  Add('  if Assigned(o) then ;');
  Add('  if Assigned(o.ClassType) then ;');
  Add('  if Assigned(c) then ;');
  Add('  mobiletype:=TMobile;');
  Add('  mobiletype:=TCar;');
  Add('  mobiletype:=TShip;');
  Add('  mobiletype:=cartype;');
  Add('  if mobiletype=nil then ;');
  Add('  if nil=mobiletype then ;');
  Add('  if mobiletype=TShip then ;');
  Add('  if TShip=mobiletype then ;');
  Add('  if mobiletype<>TShip then ;');
  Add('  if mobile is mobiletype then ;');
  Add('  if car is mobiletype then ;');
  Add('  if mobile is cartype then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOfNonClassFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TCars = class of longint;');
  Add('begin');
  CheckResolverException('Incompatible types: got "Longint" expected "class"',
    PasResolver.nIncompatibleTypesGotExpected);
end;

procedure TTestResolver.TestClassOfIsOperatorFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('  TCar = class end;');
  Add('  TCars = class of TCar;');
  Add('var cars: TCars;');
  Add('begin');
  Add('  if cars is TCars then ;');
  CheckResolverException('left side of is-operator expects a class, but got "class of" type',
    PasResolver.nLeftSideOfIsOperatorExpectsAClassButGot);
end;

procedure TTestResolver.TestClassOfAsOperatorFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class end;');
  Add('  TCar = class end;');
  Add('  TCars = class of TCar;');
  Add('var');
  Add('  o: TObject;');
  Add('  cars: TCars;');
  Add('begin');
  Add('  cars:=cars as TCars;');
  CheckResolverException('illegal qualifier "as"',PasResolver.nIllegalQualifier);
end;

procedure TTestResolver.TestClass_ClassVar;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var GlobalId: longint;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('var');
  Add('  o: TObject;');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  o.GlobalId:=3;');
  Add('  if o.GlobalId=4 then ;');
  Add('  if 5=o.GlobalId then ;');
  Add('  TObject.GlobalId:=6;');
  Add('  if TObject.GlobalId=7 then ;');
  Add('  if 8=TObject.GlobalId then ;');
  Add('  oc.GlobalId:=9;');
  Add('  if oc.GlobalId=10 then ;');
  Add('  if 11=oc.GlobalId then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOfDotClassVar;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var Id: longint;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('var');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  oc.Id:=3;');
  Add('  if oc.Id=4 then ;');
  Add('  if 5=oc.Id then ;');
  Add('  TObject.Id:=3;');
  Add('  if TObject.Id=4 then ;');
  Add('  if 5=TObject.Id then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOfDotVarFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    Id: longint;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('var');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  oc.Id:=3;');
  CheckResolverException('Only class methods, class properties and class variables can be referred with class references',
    PasResolver.nOnlyClassMembersCanBeReferredWithClassReferences);
end;

procedure TTestResolver.TestClassOfDotClassProc;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure ProcA;');
  Add('    class function FuncB: longint;');
  Add('    class procedure ProcC(i: longint);');
  Add('    class function FuncD(i: longint): longint;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('class procedure TObject.ProcA; begin end;');
  Add('class function TObject.FuncB: longint; begin end;');
  Add('class procedure TObject.ProcC(i: longint); begin end;');
  Add('class function TObject.FuncD(i: longint): longint; begin end;');
  Add('var');
  Add('  o: TObject;');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  o.ProcA;');
  Add('  oc.ProcA;');
  Add('  TObject.ProcA;');
  Add('  o.FuncB;');
  Add('  o.FuncB();');
  Add('  oc.FuncB;');
  Add('  oc.FuncB();');
  Add('  TObject.FuncB;');
  Add('  TObject.FuncB();');
  Add('  if oc.FuncB=3 then ;');
  Add('  if oc.FuncB()=4 then ;');
  Add('  if 5=oc.FuncB then ;');
  Add('  if 6=oc.FuncB() then ;');
  Add('  oc.ProcC(7);');
  Add('  TObject.ProcC(8);');
  Add('  oc.FuncD(7);');
  Add('  TObject.FuncD(8);');
  Add('  if oc.FuncD(9)=10 then ;');
  Add('  if 11=oc.FuncD(12) then ;');
  Add('  if TObject.FuncD(13)=14 then ;');
  Add('  if 15=TObject.FuncD(16) then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOfDotProcFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('procedure TObject.ProcA; begin end;');
  Add('var');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  oc.ProcA;');
  CheckResolverException('Only class methods, class properties and class variables can be referred with class references',
    PasResolver.nOnlyClassMembersCanBeReferredWithClassReferences);
end;

procedure TTestResolver.TestClassOfDotClassProperty;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var FA: longint;');
  Add('    class function GetA: longint; static;');
  Add('    class procedure SetA(Value: longint): longint; static;');
  Add('    class property A1: longint read FA write SetA;');
  Add('    class property A2: longint read GetA write FA;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('class function TObject.GetA: longint; begin end;');
  Add('class procedure TObject.SetA(Value: longint): longint; begin end;');
  Add('var');
  Add('  o: TObject;');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  o.A1:=3');
  Add('  if o.A1=4 then ;');
  Add('  if 5=o.A1 then ;');
  Add('  oc.A1:=6');
  Add('  if oc.A1=7 then ;');
  Add('  if 8=oc.A1 then ;');
  Add('  TObject.A1:=9');
  Add('  if TObject.A1=10 then ;');
  Add('  if 11=TObject.A1 then ;');
  ParseProgram;
end;

procedure TTestResolver.TestClassOfDotPropertyFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FA: longint;');
  Add('    property A: longint read FA;');
  Add('  end;');
  Add('  TObjectClass = class of TObject;');
  Add('var');
  Add('  oc: TObjectClass;');
  Add('begin');
  Add('  if oc.A=3 then ;');
  CheckResolverException('Only class methods, class properties and class variables can be referred with class references',
    PasResolver.nOnlyClassMembersCanBeReferredWithClassReferences);
end;

procedure TTestResolver.TestClass_ClassProcSelf;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class var GlobalId: longint;');
  Add('    class procedure ProcA;');
  Add('  end;');
  Add('class procedure TObject.ProcA;');
  Add('begin');
  Add('  if Self=nil then ;');
  Add('  if Self.GlobalId=3 then ;');
  Add('  if 4=Self.GlobalId then ;');
  Add('  Self.GlobalId:=5;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClass_ClassProcSelfTypeCastFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    class procedure ProcA;');
  Add('  end;');
  Add('class procedure TObject.ProcA;');
  Add('begin');
  Add('  if TObject(Self)=nil then ;');
  Add('end;');
  Add('begin');
  CheckResolverException('Illegal type conversion: "class TObject" to "TObject"',
    PasResolver.nIllegalTypeConversionTo);
end;

procedure TTestResolver.TestClass_ClassMembers;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TMobile = class');
  Add('  public');
  Add('    MobileId: longint;');
  Add('    class var LastVal: longint;');
  Add('    constructor Create; virtual;');
  Add('    class procedure ClProcA;');
  Add('    class function ClFuncB: longint;');
  Add('    class function StFuncC: longint; static;');
  Add('    class property ClMobileId: longint read StFuncC write LastVal;');
  Add('  end;');
  Add('  TMobiles = class of TMobile;');
  Add('  TCars = class of TCar;');
  Add('  TCar = class(TMobile)');
  Add('  public');
  Add('    CarId: longint;');
  Add('    class var LastCarVal: longint;');
  Add('    constructor Create; override;');
  Add('  end;');
  Add('constructor TMobile.Create;');
  Add('begin');
  Add('  Self.MobileId:=7;');
  Add('  LastVal:=LastVal+ClMobileId+1;');
  Add('  ClMobileId:=MobileId+3;');
  Add('  TCar(Self).CarId:=4;');
  Add('end;');
  Add('class procedure TMobile.ClProcA;');
  Add('var');
  Add('  m: TMobiles;');
  Add('begin');
  Add('  LastVal:=9;');
  Add('  Self.LastVal:=ClFuncB+ClMobileId;');
  Add('  m:=Self;');
  Add('  if m=Self then ;');
  Add('end;');
  Add('class function TMobile.ClFuncB: longint;');
  Add('begin');
  Add('  if LastVal=3 then ;');
  Add('  Result:=Self.LastVal-ClMobileId;');
  Add('end;');
  Add('class function TMobile.StFuncC: longint;');
  Add('begin');
  Add('  Result:=LastVal;');
  Add('  // Forbidden: no Self in static methods');
  Add('end;');
  Add('');
  Add('constructor TCar.Create;');
  Add('begin');
  Add('  inherited Create;');
  Add('  Self.CarId:=8;');
  Add('  TMobile(Self).LastVal:=5;');
  Add('  if TMobile(Self).LastVal=25 then ;');
  Add('end;');
  Add('');
  Add('var');
  Add('  car: TCar;');
  Add('  cartype: TCars;');
  Add('begin');
  Add('  car:=TCar.Create;');
  Add('  car.MobileId:=10;');
  Add('  car.ClProcA;');
  Add('  exit;');
  Add('  car.ClMobileId:=11;');
  Add('  if car.ClFuncB=16 then ;');
  Add('  if 17=car.ClFuncB then ;');
  Add('  cartype:=TCar;');
  Add('  cartype.LastVal:=18;');
  Add('  if cartype.LastVal=19 then ;');
  Add('  if 20=cartype.LastVal then ;');
  ParseProgram;
end;

procedure TTestResolver.TestProperty1;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    {#FB}FB: longint;');
  Add('    property {#B}B: longint read {@FB}FB write {@FB}FB;');
  Add('  end;');
  Add('var');
  Add('  {#v}{=A}v: TClassA;');
  Add('begin');
  Add('  {@v}v.{@b}b:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyAccessorNotInFront;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    property B: longint read FB;');
  Add('    FB: longint;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Identifier not found',PasResolver.nIdentifierNotFound);
end;

procedure TTestResolver.TestPropertyReadAccessorVarWrongType;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: string;');
  Add('    property B: longint read FB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Longint expected, but String found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyReadAccessorProcNotFunc;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure GetB;');
  Add('    property B: longint read GetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('function expected, but procedure found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyReadAccessorFuncWrongResult;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB: string;');
  Add('    property B: longint read GetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('function result longint expected, but function result string found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyReadAccessorFuncWrongArgCount;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(i: longint): string;');
  Add('    property B: longint read GetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('function arg count 0 expected, but 1 found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyReadAccessorFunc;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    function {#GetB}GetB: longint;');
  Add('    property {#B}B: longint read {@GetB}GetB;');
  Add('  end;');
  Add('function TObject.GetB: longint;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('begin');
  Add('  if {@o}o.{@B}B=3 then ;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyWriteAccessorVarWrongType;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: string;');
  Add('    property B: longint write FB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Longint expected, but String found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyWriteAccessorFuncNotProc;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function SetB: longint;');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('procedure expected, but function found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyWriteAccessorProcWrongArgCount;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure SetB;');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Wrong number of parameters specified for call to "SetB"',
    PasResolver.nWrongNumberOfParametersForCallTo);
end;

procedure TTestResolver.TestPropertyWriteAccessorProcWrongArg;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure SetB(var Value: longint);');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Incompatible type arg no. 1: Got "var ", expected "const "',
    PasResolver.nIncompatibleTypeArgNo);
end;

procedure TTestResolver.TestPropertyWriteAccessorProcWrongArgType;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure SetB(Value: string);');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Incompatible type arg no. 1: Got "String", expected "Longint"',
    PasResolver.nIncompatibleTypeArgNo);
end;

procedure TTestResolver.TestPropertyWriteAccessorProc;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    procedure {#SetB}SetB(Value: longint);');
  Add('    property {#B}B: longint write {@SetB}SetB;');
  Add('  end;');
  Add('procedure TObject.SetB(Value: longint);');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('begin');
  Add('  {@o}o.{@B}B:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyTypeless;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#FB}FB: longint;');
  Add('    property {#TOBJ_B}B: longint write {@FB}FB;');
  Add('  end;');
  Add('  {#TA}TClassA = class');
  Add('    {#FC}FC: longint;');
  Add('    property {#TA_B}{@TOBJ_B}B write {@FC}FC;');
  Add('  end;');
  Add('var');
  Add('  {#v}{=TA}v: TClassA;');
  Add('begin');
  Add('  {@v}v.{@TA_B}B:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyTypelessNoAncestor;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TClassA = class');
  Add('    property B;');
  Add('  end;');
  Add('begin');
  CheckResolverException('no property found to override',PasResolver.nNoPropertyFoundToOverride);
end;

procedure TTestResolver.TestPropertyStoredAccessorProcNotFunc;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    procedure GetB;');
  Add('    property B: longint read FB stored GetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('function expected, but procedure found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyStoredAccessorFuncWrongResult;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    function GetB: string;');
  Add('    property B: longint read FB stored GetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('function result longint expected, but function result string found',PasResolver.nXExpectedButYFound);
end;

procedure TTestResolver.TestPropertyStoredAccessorFuncWrongArgCount;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    function GetB(i: longint): boolean;');
  Add('    property B: longint read FB stored GetB;');
  Add('  end;');
  Add('begin');
  CheckResolverException('Wrong number of parameters specified for call to "GetB"',
    PasResolver.nWrongNumberOfParametersForCallTo);
end;

procedure TTestResolver.TestPropertyArgs1;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(Index: longint): boolean;');
  Add('    procedure SetB(Index: longint; Value: boolean);');
  Add('    property B[Index: longint]: boolean read GetB write SetB;');
  Add('  end;');
  Add('function TObject.GetB(Index: longint): boolean;');
  Add('begin');
  Add('end;');
  Add('procedure TObject.SetB(Index: longint; Value: boolean);');
  Add('begin');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.B[3]:=true;');
  Add('  if o.B[4] then;');
  Add('  if o.B[5]=true then;');
  Add('  if false=o.B[6] then;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyArgs2;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(Index: longint; const ID: string): longint;');
  Add('    procedure SetB(Index: longint; const ID: string; Value: longint);');
  Add('    property B[Index: longint; const ID: string]: longint read GetB write SetB;');
  Add('  end;');
  Add('function TObject.GetB(Index: longint; const ID: string): longint;');
  Add('begin');
  Add('end;');
  Add('procedure TObject.SetB(Index: longint; const ID: string; Value: longint);');
  Add('begin');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o.B[3,''abc'']:=7;');
  Add('  if o.B[4,'''']=8 then;');
  Add('  if 9=o.B[6,''d''] then;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyArgsWithDefaultsFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(Index: longint): boolean;');
  Add('    procedure SetB(Index: longint; Value: boolean);');
  Add('    property B[Index: longint = 0]: boolean read GetB write SetB;');
  Add('  end;');
  Add('function TObject.GetB(Index: longint): boolean;');
  Add('begin');
  Add('end;');
  Add('procedure TObject.SetB(Index: longint; Value: boolean);');
  Add('begin');
  Add('end;');
  Add('begin');
  CheckParserException('Property arguments can not have default values',
    PParser.nParserPropertyArgumentsCanNotHaveDefaultValues);
end;

procedure TTestResolver.TestDefaultProperty;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(Index: longint): longint;');
  Add('    procedure SetB(Index: longint; Value: longint);');
  Add('    property B[Index: longint]: longint read GetB write SetB; default;');
  Add('  end;');
  Add('function TObject.GetB(Index: longint): longint;');
  Add('begin');
  Add('end;');
  Add('procedure TObject.SetB(Index: longint; Value: longint);');
  Add('begin');
  Add('end;');
  Add('var o: TObject;');
  Add('begin');
  Add('  o[3]:=4;');
  Add('  if o[5]=6 then;');
  Add('  if 7=o[8] then;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyAssign;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    property B: longint read FB write FB;');
  Add('  end;');
  Add('var');
  Add('  o: TObject;');
  Add('  i: longint;');
  Add('begin');
  Add('  o.B:=i;');
  Add('  i:=o.B;');
  Add('  if i=o.B then ;');
  Add('  if o.B=3 then ;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyAssignReadOnlyFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    property B: longint read FB;');
  Add('  end;');
  Add('var');
  Add('  o: TObject;');
  Add('begin');
  Add('  o.B:=3;');
  CheckResolverException('No member is provided to access property',PasResolver.nPropertyNotWritable);
end;

procedure TTestResolver.TestPropertyReadNonReadableFail;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    property B: longint write FB;');
  Add('  end;');
  Add('var');
  Add('  o: TObject;');
  Add('begin');
  Add('  if o.B=3 then;');
  CheckResolverException('not readable',PasResolver.nNotReadable);
end;

procedure TTestResolver.TestWithBlock1;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#TOBJ_A}A: longint;');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#a}a: longint;');
  Add('begin');
  Add('  {@a}a:=1;');
  Add('  with {@o}o do');
  Add('    {@TOBJ_A}a:=2;');
  ParseProgram;
end;

procedure TTestResolver.TestWithBlock2;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#TOBJ_i}i: longint;');
  Add('  end;');
  Add('  {#TA}TClassA = class');
  Add('    {#TA_j}j: longint;');
  Add('    {#TA_b}{=TA}b: TClassA;');
  Add('  end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#a}{=TA}a: TClassA;');
  Add('  {#i}i: longint;');
  Add('begin');
  Add('  {@i}i:=1;');
  Add('  with {@o}o do');
  Add('    {@TOBJ_i}i:=2;');
  Add('  {@i}i:=1;');
  Add('  with {@o}o,{@a}a do begin');
  Add('    {@TOBJ_i}i:=3;');
  Add('    {@TA_j}j:=4;');
  Add('    {@TA_b}b:={@a}a;');
  Add('  end;');
  ParseProgram;
end;

procedure TTestResolver.TestWithBlockFuncResult;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#TOBJ_i}i: longint;');
  Add('  end;');
  Add('  {#TA}TClassA = class');
  Add('    {#TA_j}j: longint;');
  Add('    {#TA_b}{=TA}b: TClassA;');
  Add('  end;');
  Add('function {#GiveA}Give: TClassA;');
  Add('begin');
  Add('end;');
  Add('function {#GiveB}Give(i: longint): TClassA;');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#a}{=TA}a: TClassA;');
  Add('  {#i}i: longint;');
  Add('begin');
  Add('  with {@GiveA}Give do {@TOBJ_i}i:=3;');
  Add('  with {@GiveA}Give() do {@TOBJ_i}i:=3;');
  Add('  with {@GiveB}Give(2) do {@TOBJ_i}i:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestWithBlockConstructor;
begin
  StartProgram(false);
  Add('type');
  Add('  {#TOBJ}TObject = class');
  Add('    {#TOBJ_i}i: longint;');
  Add('  end;');
  Add('  {#TA}TClassA = class');
  Add('    {#TA_j}j: longint;');
  Add('    {#TA_b}{=TA}b: TClassA;');
  Add('    constructor {#A_CreateA}Create;');
  Add('    constructor {#A_CreateB}Create(i: longint);');
  Add('  end;');
  Add('constructor TClassA.Create;');
  Add('begin');
  Add('end;');
  Add('constructor TClassA.Create(i: longint);');
  Add('begin');
  Add('end;');
  Add('var');
  Add('  {#o}{=TOBJ}o: TObject;');
  Add('  {#a}{=TA}a: TClassA;');
  Add('  {#i}i: longint;');
  Add('begin');
  Add('  with TClassA.{@A_CreateA}Create do {@TOBJ_i}i:=3;');
  Add('  with TClassA.{@A_CreateA}Create() do {@TOBJ_i}i:=3;');
  Add('  with TClassA.{@A_CreateB}Create(2) do {@TOBJ_i}i:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestDynArrayOfLongint;
begin
  StartProgram(false);
  Add('type TIntArray = array of longint;');
  Add('var a: TIntArray;');
  Add('begin');
  Add('  SetLength(a,3);');
  Add('  a[0]:=1;');
  Add('  a[1]:=length(a);');
  Add('  a[2]:=a[0];');
  Add('  if a[3]=a[4] then ;');
  ParseProgram;
end;

procedure TTestResolver.TestStaticArray;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrA = array[1..2] of longint;');
  Add('  TArrB = array[char] of boolean;');
  Add('  TArrC = array[byte,''a''..''z''] of longint;');
  Add('var');
  Add('  a: TArrA;');
  Add('  b: TArrB;');
  Add('  c: TArrC;');
  Add('begin');
  Add('  a[1]:=1;');
  Add('  if a[2]=length(a) then ;');
  Add('  b[''x'']:=true;');
  Add('  if b[''y''] then ;');
  Add('  c[3,''f'']:=1;');
  Add('  if c[4,''g'']=a[1] then ;');
  ParseProgram;
end;

procedure TTestResolver.TestArrayOfArray;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrA = array[byte] of longint;');
  Add('  TArrB = array[smallint] of TArrA;');
  Add('var');
  Add('  b: TArrB;');
  Add('begin');
  Add('  SetLength(b,3);');
  Add('  SetLength(b[2],4);');
  Add('  b[1][2]:=5;');
  Add('  b[1,2]:=5;');
  Add('  if b[2,1]=b[0,1] then ;');
  ParseProgram;
end;

procedure TTestResolver.TestFunctionReturningArray;
begin
  StartProgram(false);
  Add('type');
  Add('  TArrA = array[1..20] of longint;');
  Add('  TArrB = array of TArrA;');
  Add('function FuncC: TArrB;');
  Add('begin');
  Add('  SetLength(Result,3);');
  Add('end;');
  Add('begin');
  Add('  FuncC[2,4]:=6;');
  Add('  FuncC()[1,3]:=5;');
  ParseProgram;
end;

initialization
  RegisterTests([TTestResolver]);

end.

