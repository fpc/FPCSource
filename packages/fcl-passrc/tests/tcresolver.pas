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
    procedure OnFindReference(Element, FindData: pointer);
    procedure OnCheckElementParent(data, arg: pointer);
  Protected
    Procedure SetUp; override;
    Procedure TearDown; override;
    procedure CreateEngine(var TheEngine: TPasTreeContainer); override;
    procedure ParseProgram;
    procedure ParseUnit;
    procedure CheckReferenceDirectives;
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
    // var, const
    Procedure TestVarLongint;
    Procedure TestVarInteger;
    Procedure TestConstInteger;
    Procedure TestDuplicateVar;
    // operators
    Procedure TestPrgAssignment;
    Procedure TestPrgProcVar;
    Procedure TestUnitProcVar;
    // statements
    Procedure TestForLoop;
    Procedure TestStatements;
    Procedure TestCaseStatement;
    Procedure TestTryStatement;
    Procedure TestStatementsRefs;
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
    Procedure TestClassMethodScope;
    Procedure TestClassIdentifierSelf;
    Procedure TestClassCallInherited;
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
    Procedure TestPropertyArgs1;
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
        +' File='+Scanner.CurFilename
        +' LineNo='+IntToStr(Scanner.CurRow)
        +' Col='+IntToStr(Scanner.CurColumn)
        +' Line="'+Scanner.CurLine+'"'
        );
      raise E;
      end;
    on E: EPasResolve do
      begin
      writeln('ERROR: TTestResolver.ParseProgram PasResolver: '+E.ClassName+':'+E.Message
        +' File='+Scanner.CurFilename
        +' LineNo='+IntToStr(Scanner.CurRow)
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
    El, LabelEl: TPasElement;
    Ref: TResolvedReference;
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
        if El.CustomData is TResolvedReference then
          begin
          Ref:=TResolvedReference(El.CustomData);
          for j:=0 to LabelElements.Count-1 do
            begin
            LabelEl:=TPasElement(LabelElements[j]);
            if Ref.Declaration=LabelEl then
              exit; // success
            end;
          end;
        end;

      // failure write candidates
      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        write('Reference candidate for "',aMarker^.Identifier,'" at reference ',aMarker^.Filename,'(',aMarker^.LineNumber,',',aMarker^.StartCol,'-',aMarker^.EndCol,')');
        write(' El=',GetObjName(El));
        if El.CustomData is TResolvedReference then
          begin
          Ref:=TResolvedReference(El.CustomData);
          write(' Decl=',GetObjName(Ref.Declaration));
          ResolverEngine.UnmangleSourceLineNumber(Ref.Declaration.SourceLinenumber,aLine,aCol);
          write(',',Ref.Declaration.SourceFilename,'(',aLine,',',aCol,')');
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
      LabelElements:=FindElementsAt(aLabel^.Filename,aLabel^.LineNumber,aLabel^.StartCol,aLabel^.EndCol);
      if LabelElements.Count=0 then
        RaiseErrorAt('label "'+aLabel^.Identifier+'" has no elements',aLabel);

      ReferenceElements:=FindElementsAt(aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol,aMarker^.EndCol);
      if ReferenceElements.Count=0 then
        RaiseErrorAt('reference "'+aMarker^.Identifier+'" has no elements',aMarker);

      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        //writeln('CheckDirectReference ',i,'/',ReferenceElements.Count,' ',GetTreeDesc(El,2));
        if El.ClassType=TPasVariable then
          begin
          AssertNotNull('TPasVariable(El='+El.Name+').VarType',TPasVariable(El).VarType);
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
    // find all markers
    for i:=0 to Resolver.Streams.Count-1 do
      begin
      GetSrc(i,SrcLines,Filename);
      ParseCode(SrcLines,Filename);
      SrcLines.Free;
      end;

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

  finally
    while FirstMarker<>nil do
      begin
      aMarker:=FirstMarker;
      FirstMarker:=FirstMarker^.Next;
      Dispose(aMarker);
      end;
  end;
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

procedure TTestResolver.OnFindReference(Element, FindData: pointer);
var
  El: TPasElement absolute Element;
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

procedure TTestResolver.OnCheckElementParent(data, arg: pointer);
var
  SubEl: TPasElement;
  El: TPasElement absolute Data;
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
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('var a: longint;');
  Add('var a: string;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected duplicate identifier, but got msg number "'+E.Message+'"',
        PasResolver.nDuplicateIdentifier,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('duplicate identifier spotted',true,ok);
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
  Add('  {#Exec}Exception = longint;');
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
  Add('    on {#e2}E: {@Exec}Exception do');
  Add('      if {@e2}e=nil then ;');
  Add('    on {#e3}E: {@Exec}Exception do');
  Add('      raise {@e3}e;');
  Add('    else');
  Add('      {@v1}v1:={@e1}e;');
  Add('  end');
  ParseProgram;
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
  Add('  ExitcodE:=''3'';');
  Add('  afile.eXitCode:=3;');
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
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('procedure ProcA(i: longint);');
  Add('begin');
  Add('end;');
  Add('procedure ProcA(i: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected duplicate identifier, but got msg number "'+E.Message+'"',
        PasResolver.nDuplicateIdentifier,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('duplicate identifier spotted',true,ok);
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
  Add('  {@A}FuncA(3);');
  Add('  {@B}FuncB(3);');
  ParseProgram;
end;

procedure TTestResolver.TestForwardProcUnresolved;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('procedure FuncA(i: longint); forward;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected forward proc not resolved, but got msg number "'+E.Message+'"',
        PasResolver.nForwardProcNotResolved,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('unresolved forward proc raised an error',true,ok);
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
  Add('  {@B}ProcB(3);');
  Add('  {@C}ProcC(3);');
  Add('end;');
  Add('begin');
  Add('  {@A}FuncA;');
  ParseProgram;
end;

procedure TTestResolver.TestNestedForwardProcUnresolved;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('procedure FuncA;');
  Add('  procedure ProcB(i: longint); forward;');
  Add('begin');
  Add('end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected forward proc not resolved, but got msg number "'+E.Message+'"',
        PasResolver.nForwardProcNotResolved,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('unresolved forward proc raised an error',true,ok);
end;

procedure TTestResolver.TestForwardProcFuncMismatch;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('procedure DoIt; forward;');
  Add('function DoIt: longint;');
  Add('begin');
  Add('end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected "procedure expected, but function found", but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('proc type mismatch raised an error',true,ok);
end;

procedure TTestResolver.TestForwardFuncResultMismatch;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('function DoIt: longint; forward;');
  Add('function DoIt: string;');
  Add('begin');
  Add('end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected "Result type mismatch", but got msg number "'+E.Message+'"',
        PasResolver.nResultTypeMismatchExpectedButFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('function result type mismatch raised an error',true,ok);
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
  Add('  {@A}FuncA(3);');
  ParseUnit;
end;

procedure TTestResolver.TestUnitIntfProcUnresolved;
var
  ok: Boolean;
begin
  StartUnit(false);
  Add('interface');
  Add('procedure {#A_forward}FuncA(i: longint);');
  Add('implementation');
  Add('initialization');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected forward proc not resolved, but got msg number "'+E.Message+'"',
        PasResolver.nForwardProcNotResolved,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('unresolved forward proc raised an error',true,ok);
end;

procedure TTestResolver.TestUnitIntfMismatchArgName;
var
  ok: Boolean;
begin
  StartUnit(false);
  Add('interface');
  Add('procedure {#A_forward}ProcA(i: longint);');
  Add('implementation');
  Add('procedure {#A}ProcA(j: longint);');
  Add('begin');
  Add('end;');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function header "ProcA" doesn''t match forward : var name changes, but got msg number "'+E.Message+'"',
        PasResolver.nFunctionHeaderMismatchForwardVarName,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('mismatch proc argument name raised an error',true,ok);
end;

procedure TTestResolver.TestProcOverloadIsNotFunc;
var
  ok: Boolean;
begin
  StartUnit(false);
  Add('interface');
  Add('var ProcA: longint;');
  Add('procedure {#A_Decl}ProcA(i: longint);');
  Add('implementation');
  Add('procedure {#A_Impl}ProcA(i: longint);');
  Add('begin');
  Add('end;');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected Duplicate identifier, but got msg number "'+E.Message+'"',
        PasResolver.nDuplicateIdentifier,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('overload proc/var raised an error',true,ok);
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
  Add('  {@V}v.{@A_b}b.{@B_a}a:=nil;');
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
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TClassA = class');
  Add('    procedure ProcA;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected forward proc not resolved, but got msg number "'+E.Message+'"',
        PasResolver.nForwardProcNotResolved,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('unresolved forward proc raised an error',true,ok);
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
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure ProcA; abstract;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected abstract without virtual, but got msg number "'+E.Message+'"',
        PasResolver.nInvalidProcModifiers,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('abstract method without virtual raised an error',true,ok);
end;

procedure TTestResolver.TestClassMethodAbstractHasBody;
var
  ok: Boolean;
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
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected abstract must not have implementation, but got msg number "'+E.Message+'"',
        PasResolver.nAbstractMethodsMustNotHaveImplementation,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('abstract method with body raised an error',true,ok);
end;

procedure TTestResolver.TestClassMethodUnresolvedWithAncestor;
var
  ok: Boolean;
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
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected forward proc not resolved, but got msg number "'+E.Message+'"',
        PasResolver.nForwardProcNotResolved,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('unresolved forward proc raised an error',true,ok);
end;

procedure TTestResolver.TestClassProcFuncMismatch;
var
  ok: Boolean;
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
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected "procedure expected, but function found", but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('proc type mismatch raised an error',true,ok);
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
var
  ok: Boolean;
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
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected Duplicate identifier, but got msg number "'+E.Message+'"',
        PasResolver.nDuplicateIdentifier,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('duplicate method signature raised an error',true,ok);
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
  Add('  {@A}Self.{@B}B:=3;');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

procedure TTestResolver.TestClassCallInherited;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure {#TOBJ_ProcA}ProcA(i: longint);');
  Add('    procedure {#TOBJ_ProcB}ProcB(j: longint);');
  Add('  end;');
  Add('  {#A}TClassA = class');
  Add('    procedure {#A_ProcA}ProcA(i: longint);');
  Add('    procedure {#A_ProcB}ProcB(k: longint);');
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
  Add('  {@A_ProcA}ProcA;');
  Add('  {@TOBJ_ProcA}inherited;');
  Add('  inherited {@TOBJ_ProcA}ProcA({@i1}i);');
  Add('  {@A_ProcB}ProcB;');
  Add('  inherited {@TOBJ_ProcB}ProcB({@i1}i);');
  Add('end;');
  Add('procedure TClassA.ProcB(k: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
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
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    property B: longint read FB;');
  Add('    FB: longint;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected Identifier not found, but got msg number "'+E.Message+'"',
        PasResolver.nIdentifierNotFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property accessor not in front raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyReadAccessorVarWrongType;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: string;');
  Add('    property B: longint read FB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected Longint expected, but String found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property read accessor wrong type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyReadAccessorProcNotFunc;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure GetB;');
  Add('    property B: longint read GetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function expected, but procedure found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property read accessor wrong function type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyReadAccessorFuncWrongResult;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB: string;');
  Add('    property B: longint read GetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function result longint expected, but function result string found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property read accessor function wrong result type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyReadAccessorFuncWrongArgCount;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(i: longint): string;');
  Add('    property B: longint read GetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function arg count 0 expected, but 1 found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property read accessor function wrong arg count raised an error',true,ok);
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
  Add('  {@o}o.{@B}B:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyWriteAccessorVarWrongType;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: string;');
  Add('    property B: longint write FB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected Longint expected, but String found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property read accessor wrong type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyWriteAccessorFuncNotProc;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function SetB: longint;');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected procedure expected, but function found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property write accessor wrong function instead of proc raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyWriteAccessorProcWrongArgCount;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure SetB;');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected procedure arg count 1 expected, but 0 found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property write accessor procedure wrong arg count raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyWriteAccessorProcWrongArg;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure SetB(var Value: longint);');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected procedure arg longint expected, but var found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property write accessor procedure wrong arg type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyWriteAccessorProcWrongArgType;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    procedure SetB(Value: string);');
  Add('    property B: longint write SetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected procedure(Value: longint) expected, but procedure(Value: string) found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property write accessor procedure wrong arg type raised an error',true,ok);
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
  Add('    property {#TOBJ_B}B: longint read {@FB}FB;');
  Add('  end;');
  Add('  {#TA}TClassA = class');
  Add('    {#FC}FC: longint;');
  Add('    property {#TA_B}{@TOBJ_B}B read {@FC}FC;');
  Add('  end;');
  Add('var');
  Add('  {#v}{=TA}v: TClassA;');
  Add('begin');
  Add('  {@v}v.{@TA_B}B:=3;');
  ParseProgram;
end;

procedure TTestResolver.TestPropertyTypelessNoAncestor;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('  end;');
  Add('  TClassA = class');
  Add('    property B;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected no property found to override, but got msg number "'+E.Message+'"',
        PasResolver.nNoPropertyFoundToOverride,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property typeless without ancestor property raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyStoredAccessorProcNotFunc;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    procedure GetB;');
  Add('    property B: longint read FB stored GetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function expected, but procedure found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property stored accessor wrong function type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyStoredAccessorFuncWrongResult;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    function GetB: string;');
  Add('    property B: longint read FB stored GetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function result longint expected, but function result string found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property stored accessor function wrong result type raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyStoredAccessorFuncWrongArgCount;
var
  ok: Boolean;
begin
  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    FB: longint;');
  Add('    function GetB(i: longint): boolean;');
  Add('    property B: longint read FB stored GetB;');
  Add('  end;');
  Add('begin');
  ok:=false;
  try
    ParseModule;
  except
    on E: EPasResolve do
      begin
      AssertEquals('Expected function arg count 0 expected, but 1 found, but got msg number "'+E.Message+'"',
        PasResolver.nXExpectedButYFound,E.MsgNumber);
      ok:=true;
      end;
  end;
  AssertEquals('property stored accessor function wrong arg count raised an error',true,ok);
end;

procedure TTestResolver.TestPropertyArgs1;
begin
  exit;

  StartProgram(false);
  Add('type');
  Add('  TObject = class');
  Add('    function GetB(Index: longint): boolean;');
  Add('    procedure SetB(Index: longint; Value: longint);');
  Add('    property B[Index: longint]: longint read GetB write SetB;');
  Add('  end;');
  Add('function TObject.GetB(Index: longint): boolean;');
  Add('begin');
  Add('end;');
  Add('procedure TObject.SetB(Index: longint; Value: longint);');
  Add('begin');
  Add('end;');
  Add('begin');
  ParseProgram;
end;

initialization
  RegisterTests([TTestResolver]);

end.

