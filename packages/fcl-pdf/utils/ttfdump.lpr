program ttfdump;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cwstrings,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp,
  fpparsettf, contnrs;

type
  // forward declarations
  TTextMapping = class;


  TTextMappingList = class(TObject)
  private
    FList: TFPObjectList;
    function GetCount: Integer;
  protected
    function    GetItem(AIndex: Integer): TTextMapping; reintroduce;
    procedure   SetItem(AIndex: Integer; AValue: TTextMapping); reintroduce;
  public
    constructor Create;
    destructor  Destroy; override;
    function    Add(AObject: TTextMapping): Integer; overload;
    function    Add(const ACharID, AGlyphID: uint16): Integer; overload;
    property    Count: Integer read GetCount;
    property    Items[Index: Integer]: TTextMapping read GetItem write SetItem; default;
  end;


  TTextMapping = class(TObject)
  private
    FCharID: uint16;
    FGlyphID: uint16;
  public
    class function NewTextMap(const ACharID, AGlyphID: uint16): TTextMapping;
    property    CharID: uint16 read FCharID write FCharID;
    property    GlyphID: uint16 read FGlyphID write FGlyphID;
  end;


  TMyApplication = class(TCustomApplication)
  private
    FFontFile: TTFFileInfo;
    procedure   DumpGlyphIndex;
    function    GetGlyphIndicesString(const AText: UnicodeString): AnsiString; overload;
    function    GetGlyphIndices(const AText: UnicodeString): TTextMappingList; overload;
  protected
    procedure   DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   WriteHelp; virtual;
  end;

  TFriendClass = class(TTFFileInfo)
  end;

{ TTextMappingList }

function TTextMappingList.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TTextMappingList.GetItem(AIndex: Integer): TTextMapping;
begin
  Result := TTextMapping(FList.Items[AIndex]);
end;

procedure TTextMappingList.SetItem(AIndex: Integer; AValue: TTextMapping);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TTextMappingList.Create;
begin
  FList := TFPObjectList.Create;
end;

destructor TTextMappingList.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TTextMappingList.Add(AObject: TTextMapping): Integer;
var
  i: integer;
begin
  Result := -1;
  for i := 0 to FList.Count-1 do
  begin
    if TTextMapping(FList.Items[i]).CharID = AObject.CharID then
      Exit; // mapping already exists
  end;
  Result := FList.Add(AObject);
end;

function TTextMappingList.Add(const ACharID, AGlyphID: uint16): Integer;
var
  o: TTextMapping;
begin
  o := TTextMapping.Create;
  o.CharID := ACharID;
  o.GlyphID := AGlyphID;
  Result := Add(o);
  if Result = -1 then
    o.Free;
end;

{ TTextMapping }

class function TTextMapping.NewTextMap(const ACharID, AGlyphID: uint16): TTextMapping;
begin
  Result := TTextMapping.Create;
  Result.CharID := ACharID;
  Result.GlyphID := AGlyphID;
end;

{ TMyApplication }

procedure TMyApplication.DumpGlyphIndex;
begin
  Writeln('FHHead.numberOfHMetrics = ', FFontFile.HHead.numberOfHMetrics);
  Writeln('Length(Chars[]) = ', Length(FFontFile.Chars));

  writeln('Glyph Index values:');
  Writeln('U+0020 (space) = ', FFontFile.Chars[$0020]);
  Writeln('U+0021 (!) = ', FFontFile.Chars[$0021]);
  Writeln('U+0048 (H) = ', FFontFile.Chars[$0048]);

  Writeln('Glyph widths:');
  Writeln('3 = ', TFriendClass(FFontFile).ToNatural(FFontFile.Widths[FFontFile.Chars[$0020]].AdvanceWidth));
  Writeln('4 = ', TFriendClass(FFontFile).ToNatural(FFontFile.Widths[FFontFile.Chars[$0021]].AdvanceWidth));
  Writeln('H = ', TFriendClass(FFontFile).ToNatural(FFontFile.Widths[FFontFile.Chars[$0048]].AdvanceWidth));
end;

function TMyApplication.GetGlyphIndices(const AText: UnicodeString): TTextMappingList;
var
  i: integer;
  c: uint16;
begin
  if AText = '' then
    Exit;
  Result := TTextMappingList.Create;
  for i := 1 to Length(AText) do
  begin
    c := uint16(AText[i]);
    Result.Add(c, FFontFile.Chars[c]);
  end;
end;

function TMyApplication.GetGlyphIndicesString(const AText: UnicodeString): AnsiString;
var
  i: integer;
  c: word;
begin
  Result := '';
  for i := 1 to Length(AText) do
  begin
    c := Word(AText[i]);
    if i > 1 then
      Result := Result + ',';
    Result := Result + IntToHex(FFontFile.Chars[c], 4);
  end;
end;

procedure TMyApplication.DoRun;
var
  ErrorMsg: String;
  s: UnicodeString;
  lst: TTextMappingList;
  i: integer;
begin
  // quick check parameters
  ErrorMsg := CheckOptions('hf:', 'help');
  if ErrorMsg <> '' then
  begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if (ParamCount = 0) or HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  FFontFile.LoadFromFile(self.GetOptionValue('f'));
  DumpGlyphIndex;

  s := 'Hello, World!';
  Writeln('');
  lst := GetGlyphIndices(s);
  Writeln(Format('%d Glyph indices for: "%s"', [lst.Count, s]));
  for i := 0 to lst.Count-1 do
    Writeln(Format(#9'%s'#9'%s', [IntToHex(lst[i].GlyphID, 4), IntToHex(lst[i].CharID, 4)]));

  // stop program loop
  Terminate;
end;

constructor TMyApplication.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException := True;
  FFontFile := TTFFileInfo.Create;
end;

destructor TMyApplication.Destroy;
begin
  FFontFile.Free;
  inherited Destroy;
end;

procedure TMyApplication.WriteHelp;
begin
  writeln('Usage: ', ExeName, ' -h');
  writeln('   -h            Show this help.');
  writeln('   -f <ttf>      Load TTF font file.');
end;

var
  Application: TMyApplication;

begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'TTF Font Dump';
  Application.Run;
  Application.Free;
end.

