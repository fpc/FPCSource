program ttfdump;

{$mode objfpc}{$H+}
{$codepage utf8}

uses
  {$ifdef unix}cwstring,{$endif}  // required for UnicodeString handling.
  Classes,
  SysUtils,
  CustApp,
  fpparsettf,
  FPFontTextMapping,
  fpTTFSubsetter;

type

  TMyApplication = class(TCustomApplication)
  private
    FFontFile: TTFFileInfo;
    procedure   DumpGlyphIndex;
    function    GetGlyphIndicesString(const AText: UnicodeString): AnsiString; overload;
    function    GetGlyphIndices(const AText: UnicodeString): TTextMappingList; overload;
    procedure   CreateSubsetFontFile(const AList: TTextMappingList);
  protected
    procedure   DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   WriteHelp; virtual;
  end;


  TFriendClass = class(TTFFileInfo)
  end;


{ TMyApplication }

procedure TMyApplication.DumpGlyphIndex;
begin
  Writeln('FHHead.numberOfHMetrics = ', FFontFile.HHead.numberOfHMetrics);
  Writeln('Length(Chars[]) = ', Length(FFontFile.Chars));
  writeln;
  writeln('Glyph Index values:');
  Writeln('  U+0020 (space) = ', Format('%d  (%0:4.4x)', [FFontFile.Chars[$0020]]));
  Writeln('  U+0021 (!) = ', Format('%d  (%0:4.4x)', [FFontFile.Chars[$0021]]));
  Writeln('  U+0048 (H) = ', Format('%d  (%0:4.4x)', [FFontFile.Chars[$0048]]));
  writeln;
  Writeln('Glyph widths:');
  Writeln('  3 = ', TFriendClass(FFontFile).ToNatural(FFontFile.Widths[FFontFile.Chars[$0020]].AdvanceWidth));
  Writeln('  4 = ', TFriendClass(FFontFile).ToNatural(FFontFile.Widths[FFontFile.Chars[$0021]].AdvanceWidth));
  Writeln('  H = ', TFriendClass(FFontFile).ToNatural(FFontFile.Widths[FFontFile.Chars[$0048]].AdvanceWidth));
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

procedure TMyApplication.CreateSubsetFontFile(const AList: TTextMappingList);
var
  lSubset: TFontSubsetter;
begin
  writeln;
  writeln('called CreateSubsetFontFile...');
  lSubset := TFontSubsetter.Create(FFontFile, AList);
  try
    lSubSet.SaveToFile(ExtractFileName(GetOptionValue('f'))+'.subset.ttf');
  finally
    FreeAndNil(lSubSet);
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
  ErrorMsg := CheckOptions('hf:s', 'help');
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

  // test #1
//  s := 'Hello, World!';
  // test #2
  s := 'Typography: “What’s wrong?”';

  Writeln('');
  lst := GetGlyphIndices(s);
  Writeln(Format('%d Glyph indices for: "%s"', [lst.Count, s]));
  writeln(#9'GID'#9'CharID');
  writeln(#9'---'#9'------');
  for i := 0 to lst.Count-1 do
    Writeln(Format(#9'%s'#9'%s'#9'%s', [IntToHex(lst[i].GlyphID, 4), IntToHex(lst[i].CharID, 4), Char(lst[i].CharID)]));

  if HasOption('s','') then
    CreateSubsetFontFile(lst);
  lst.Free;

  writeln;
  writeln;
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
  writeln('   -s            Generate a subset TTF file.');
end;



var
  Application: TMyApplication;
begin
  Application := TMyApplication.Create(nil);
  Application.Title := 'TTF Font Dump';
  Application.Run;
  Application.Free;
end.

