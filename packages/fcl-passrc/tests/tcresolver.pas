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

  { TTestResolver }

  TTestResolver = Class(TTestParser)
  Private
    FFirstStatement: TPasImplBlock;
    FModules: TObjectList;// list of TTestEnginePasResolver
    FPasResolver: TTestEnginePasResolver;
    function GetModuleCount: integer;
    function GetModules(Index: integer): TTestEnginePasResolver;
    function OnPasResolverFindUnit(const aUnitName: String): TPasModule;
    procedure OnFindReference(Element, FindData: pointer);
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
    procedure AddSystemUnit;
    procedure StartProgram(NeedSystemUnit: boolean);
    procedure StartUnit(NeedSystemUnit: boolean);
    property Modules[Index: integer]: TTestEnginePasResolver read GetModules;
    property ModuleCount: integer read GetModuleCount;
  Published
    Procedure TestEmpty;
    Procedure TestAliasType;
    Procedure TestAlias2Type;
    Procedure TestAliasTypeRefs;
    Procedure TestVarLongint;
    Procedure TestVarInteger;
    Procedure TestConstInteger;
    Procedure TestPrgAssignment;
    Procedure TestPrgProcVar;
    Procedure TestUnitProcVar;
    Procedure TestForLoop;
    Procedure TestStatements;
    Procedure TestCaseStatement;
    Procedure TestTryStatement;
    Procedure TestStatementsRefs;
    Procedure TestUnitRef;
    Procedure TestProcParam;
    Procedure TestFunctionResult;
    Procedure TestProcOverload;
    Procedure TestProcOverloadRefs;
    Procedure TestNestedProc;
    property PasResolver: TTestEnginePasResolver read FPasResolver;
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
  PasResolver.Clear;
  if FModules<>nil then
    begin
    FModules.OwnsObjects:=false;
    FModules.Remove(PasResolver); // remove reference
    FModules.OwnsObjects:=true;
    FreeAndNil(FModules);// free all other modules
    end;
  inherited TearDown;
  FPasResolver:=nil;
end;

procedure TTestResolver.CreateEngine(var TheEngine: TPasTreeContainer);
begin
  FPasResolver:=AddModule(MainFilename);
  TheEngine:=PasResolver;
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
  TAssert.AssertSame('Has resolver',PasResolver,Parser.Engine);
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
  TAssert.AssertSame('Has resolver',PasResolver,Parser.Engine);
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
    //writeln('TTestResolver.CheckReferenceDirectives.AddPointer ',Identifier);
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
    //writeln('CheckReference searching reference: ',aMarker^.Filename,' Line=',aMarker^.LineNumber,' Col=',aMarker^.StartCol,'-',aMarker^.EndCol,' Label="',aMarker^.Identifier,'"');
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
          PasResolver.UnmangleSourceLineNumber(Ref.Declaration.SourceLinenumber,aLine,aCol);
          write(Ref.Declaration.SourceFilename,'(',aLine,',',aCol,')');
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
    ReferenceElements: TFPList;
    i, LabelLine, LabelCol: Integer;
    El: TPasElement;
    DeclEl: TPasType;
  begin
    //writeln('CheckPointer searching pointer: ',aMarker^.Filename,' Line=',aMarker^.LineNumber,' Col=',aMarker^.StartCol,'-',aMarker^.EndCol,' Label="',aMarker^.Identifier,'"');
    aLabel:=FindLabel(aMarker^.Identifier);
    if aLabel=nil then
      RaiseErrorAt('label "'+aMarker^.Identifier+'" not found',aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol);

    ReferenceElements:=nil;
    try
      ReferenceElements:=FindElementsAt(aMarker^.Filename,aMarker^.LineNumber,aMarker^.StartCol,aMarker^.EndCol);
      if ReferenceElements.Count=0 then
        RaiseErrorAt('reference "'+aMarker^.Identifier+'" has no elements',aMarker);

      for i:=0 to ReferenceElements.Count-1 do
        begin
        El:=TPasElement(ReferenceElements[i]);
        if El.ClassType=TPasAliasType then
          begin
          DeclEl:=TPasAliasType(El).DestType;
          PasResolver.UnmangleSourceLineNumber(DeclEl.SourceLinenumber,LabelLine,LabelCol);
          if (aLabel^.Filename=DeclEl.SourceFilename)
          and (aLabel^.LineNumber=LabelLine)
          and (aLabel^.StartCol<=LabelCol)
          and (aLabel^.EndCol>=LabelCol) then
            exit; // success
          writeln('CheckDirectReference Decl at ',DeclEl.SourceFilename,'(',LabelLine,',',LabelCol,')');
          RaiseErrorAt('wrong direct reference "'+aMarker^.Identifier+'"',aMarker);
          end;
        end;
    finally
    end;

  end;

var
  aMarker: PMarker;
  i: Integer;
  SrcLines: TStringList;
begin
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

procedure TTestResolver.AddSystemUnit;
begin
  AddModuleWithIntfImplSrc('system.pp',
    // interface
    LinesToStr([
    'type',
    '  integer=longint;',
    '  sizeint=int64;',
    //'const',
    //'  LineEnding = #10;',
    //'  DirectorySeparator = ''/'';',
    //'  DriveSeparator = '''';',
    //'  AllowDirectorySeparators : set of char = [''\'',''/''];',
    //'  AllowDriveSeparators : set of char = [];',
    'var',
    '  ExitCode: Longint;',
    //'Procedure Move(const source;var dest;count:SizeInt);',
    ''
    // implementation
    ]),LinesToStr([
   // 'Procedure Move(const source;var dest;count:SizeInt);',
   // 'begin',
   // 'end;',
    ''
    ]));
end;

procedure TTestResolver.StartProgram(NeedSystemUnit: boolean);
begin
  if NeedSystemUnit then
    AddSystemUnit
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
  PasResolver.UnmangleSourceLineNumber(El.SourceLinenumber,Line,Col);
  //writeln('TTestResolver.OnFindReference ',GetObjName(El),' ',El.SourceFilename,' Line=',Line,',Col=',Col,' SearchFile=',Data^.Filename,',Line=',Data^.Line,',Col=',Data^.StartCol,'-',Data^.EndCol);
  if (Data^.Filename=El.SourceFilename)
  and (Data^.Line=Line)
  and (Data^.StartCol<=Col)
  and (Data^.EndCol>=Col)
  then
    Data^.Found.Add(El);
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
  AssertEquals('1 declarations',1,PasProgram.ProgramSection.Declarations.Count);

  El:=TPasElement(PasProgram.ProgramSection.Declarations[0]);
  AssertEquals('overloaded proc',TPasOverloadedProc,El.ClassType);

  AssertEquals('1 statement',1,PasProgram.InitializationSection.Elements.Count);
end;

procedure TTestResolver.TestProcOverloadRefs;
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

initialization
  RegisterTests([TTestResolver]);

end.

