{
    Description:
      This is a homegrown font cache. The fpReport reports can reference
      a font by its name. The job of the font cache is to look through
      its cached fonts to match the font name, and which *.ttf file it
      relates too. The reporting code can then extract font details
      correctly (eg: font width, height etc).
}
unit fpTTF;

{$mode objfpc}{$H+}

{.$define ttfdebug}

interface

uses
  Classes,
  SysUtils,
  contnrs,
  fpparsettf;

type

  TTrueTypeFontStyle = (fsRegular, fsItalic, fsBold, fsCondensed, fsExtraLight, fsLight, fsSemibold, fsMedium, fsBlack, fsFixedWidth);
  TTrueTypeFontStyles = set of TTrueTypeFontStyle;


  { Forward declaration }
  TFPFontCacheList = class;


  TFPFontCacheItem = class(TObject)
  private
    FFamilyName: String;
    FFileName: String;
    FStyleFlags: TTrueTypeFontStyles;
    FFileInfo: TTFFileInfo;
    FOwner: TFPFontCacheList; // reference to FontCacheList that owns this instance
    FPostScriptName: string;
    procedure   BuildFontCacheItem;
    procedure   SetStyleIfExists(var AText: string; var AStyleFlags: TTrueTypeFontStyles; const AStyleName: String; const AStyle: TTrueTypeFontStyle);
    function    GetIsBold: boolean;
    function    GetIsFixedWidth: boolean;
    function    GetIsItalic: boolean;
    function    GetIsRegular: boolean;
  public
    constructor Create(const AFilename: String);
    destructor  Destroy; override;
    { Result is in pixels }
    function    TextWidth(const AStr: utf8string; const APointSize: single): single;
    { Result is in pixels }
    function    TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
    property    FileName: String read FFileName;
    property    FamilyName: String read FFamilyName;
    property    PostScriptName: string read FPostScriptName;
    property    FontData: TTFFileInfo read FFileInfo;
    { A bitmasked value describing the full font style }
    property    StyleFlags: TTrueTypeFontStyles read FStyleFlags;
    { IsXXX properties are convenience properties, internally querying StyleFlags. }
    property    IsFixedWidth: boolean read GetIsFixedWidth;
    property    IsRegular: boolean read GetIsRegular;
    property    IsItalic: boolean read GetIsItalic;
    property    IsBold: boolean read GetIsBold;
  end;


  TFPFontCacheList = class(TObject)
  private
    FList: TObjectList;
    FSearchPath: TStringList;
    FDPI: integer;
    procedure   SearchForFonts(const AFontPath: String);
    procedure   SetDPI(AValue: integer);
  protected
    function    GetCount: integer; virtual;
    function    GetItem(AIndex: Integer): TFPFontCacheItem; virtual;
    procedure   SetItem(AIndex: Integer; AValue: TFPFontCacheItem); virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   BuildFontCache;
    function    Add(const AObject: TFPFontCacheItem): integer;
    procedure   AssignFontList(const AStrings: TStrings);
    procedure   Clear;
    property    Count: integer read GetCount;
    function    IndexOf(const AObject: TFPFontCacheItem): integer;
    function    Find(const AFontCacheItem: TFPFontCacheItem): integer; overload;
    function    Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TFPFontCacheItem; overload;
    function    Find(const APostScriptName: string): TFPFontCacheItem; overload;
    { not used: utility function doing a conversion for us. }
    function    PointSizeInPixels(const APointSize: single): single;
    property    Items[AIndex: Integer]: TFPFontCacheItem read GetItem write SetItem; default;
    property    SearchPath: TStringList read FSearchPath;
    property    DPI: integer read FDPI write SetDPI;
  end;


function gTTFontCache: TFPFontCacheList;

implementation

resourcestring
  rsNoSearchPathDefined = 'No search path was defined';
  rsNoFontFileName = 'The FileName property is empty, so we can''t load font data.';
  rsCharAboveWord = 'TextWidth doesn''t support characters higher then High(Word) - %d.';

var
  uFontCacheList: TFPFontCacheList;

function gTTFontCache: TFPFontCacheList;
begin
 if not Assigned(uFontCacheList) then
 begin
   uFontCacheList := TFPFontCacheList.Create;
 end;
 Result := uFontCacheList;
end;

{ TFPFontCacheItem }

function TFPFontCacheItem.GetIsBold: boolean;
begin
  Result := fsBold in FStyleFlags;
end;

function TFPFontCacheItem.GetIsFixedWidth: boolean;
begin
  Result := fsFixedWidth in FStyleFlags;
end;

function TFPFontCacheItem.GetIsItalic: boolean;
begin
  Result := fsItalic in FStyleFlags;
end;

function TFPFontCacheItem.GetIsRegular: boolean;
begin
  Result := fsRegular in FStyleFlags;
end;

procedure TFPFontCacheItem.BuildFontCacheItem;
var
  s: string;
begin
  s := FFileInfo.PostScriptName;
  FPostScriptName := s;
  FFamilyName := FFileInfo.FamilyName;
  if Pos(s, FFamilyName) = 1 then
    Delete(s, 1, Length(FFamilyName));

  FStyleFlags := [fsRegular];

  // extract simple styles first
  if FFileInfo.PostScript.isFixedPitch > 0 then
    FStyleFlags := [fsFixedWidth]; // this should overwrite Regular style

  if FFileInfo.PostScript.ItalicAngle <> 0 then
    FStyleFlags := FStyleFlags + [fsItalic];

  // Now to more complex styles stored in StyleName field. eg: 'Condensed Medium'
  SetStyleIfExists(s, FStyleFlags, 'Bold', fsBold);
  SetStyleIfExists(s, FStyleFlags, 'Condensed', fsCondensed);
  SetStyleIfExists(s, FStyleFlags, 'ExtraLight', fsExtraLight);
  SetStyleIfExists(s, FStyleFlags, 'Light', fsLight);
  SetStyleIfExists(s, FStyleFlags, 'Semibold', fsSemibold);
  SetStyleIfExists(s, FStyleFlags, 'Medium', fsMedium);
  SetStyleIfExists(s, FStyleFlags, 'Black', fsBlack);
  SetStyleIfExists(s, FStyleFlags, 'Oblique', fsItalic);
end;

procedure TFPFontCacheItem.SetStyleIfExists(var AText: string; var AStyleFlags: TTrueTypeFontStyles;
  const AStyleName: String; const AStyle: TTrueTypeFontStyle);
var
  i: integer;
begin
  i := Pos(AStyleName, AText);
  if i > 0 then
  begin
    AStyleFlags := AStyleFlags + [AStyle];
    Delete(AText, i, Length(AStyleName));
  end;
end;

constructor TFPFontCacheItem.Create(const AFilename: String);
begin
  inherited Create;
  FFileName := AFilename;
  FStyleFlags := [fsRegular];

  if AFileName = '' then
    raise ETTF.Create(rsNoFontFileName);

  if FileExists(AFilename) then
  begin
    FFileInfo := TTFFileInfo.Create;
    FFileInfo.LoadFromFile(AFilename);
    BuildFontCacheItem;
  end;
end;

destructor TFPFontCacheItem.Destroy;
begin
  FFileInfo.Free;
  inherited Destroy;
end;

{ TextWidth returns with width of the text. If APointSize = 0.0, then it returns
  the text width in Font Units. If APointSize > 0 then it returns the text width
  in Pixels. }
function TFPFontCacheItem.TextWidth(const AStr: utf8string; const APointSize: single): single;
{
    From Microsoft's Typography website:
    Converting FUnits (font units) to pixels

    Values in the em square are converted to values in the pixel coordinate system
    by multiplying them by a scale. This scale is:

    pointSize * resolution / ( 72 points per inch * units_per_em )

    where pointSize is the size at which the glyph is to be displayed, and resolution
    is the resolution of the output device. The 72 in the denominator reflects the
    number of points per inch.

    For example, assume that a glyph feature is 550 FUnits in length on a 72 dpi
    screen at 18 point. There are 2048 units per em. The following calculation
    reveals that the feature is 4.83 pixels long.

    550 * 18 * 72 / ( 72 * 2048 ) = 4.83
}
var
  i: integer;
  lWidth: integer;
  lGIndex: integer;
  us: UnicodeString;
  {$IFDEF ttfdebug}
  sl: TStringList;
  s: string;
  {$ENDIF}
begin
  Result := 0;
  if Length(AStr) = 0 then
    Exit;

  if not Assigned(FFileInfo) then
    Exit;

  {$IFDEF ttfdebug}
    sl := TStringList.Create;
    s := '';
    for i := 0 to 255 do
    begin
      lGIndex := FFileInfo.GetGlyphIndex(i);
      lWidth := FFileInfo.GetAdvanceWidth(lGIndex);
      s := s + ',' + IntToStr(lWidth);
    end;
    sl.Add(s);
    sl.Add('UnitsPerEm = ' + IntToStr(FFileInfo.Head.UnitsPerEm));
    sl.SaveToFile(GetTempDir(True) + FFileInfo.PostScriptName + '.txt');
    sl.Free;
  {$ENDIF}

  lWidth := 0;
  us := UTF8Decode(AStr);
  for i := 1 to Length(us) do
  begin
    lGIndex := FFileInfo.GetGlyphIndex(Word(us[i]));
    lWidth := lWidth + FFileInfo.GetAdvanceWidth(lGIndex);
  end;
  if APointSize = 0.0 then
    Result := lWidth
  else
  begin
    { Converting Font Units to Pixels. The formula is:
      pixels = glyph_units * pointSize * resolution / ( 72 points per inch * THead.UnitsPerEm )  }
    Result := lWidth * APointSize * FOwner.DPI / (72 * FFileInfo.Head.UnitsPerEm);
  end;
end;

function TFPFontCacheItem.TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
begin
  { Both lHeight and lDescenderHeight are in pixels }
  Result := FFileInfo.CapHeight * APointSize * gTTFontCache.DPI / (72 * FFileInfo.Head.UnitsPerEm);
  ADescender := Abs(FFileInfo.Descender) * APointSize * gTTFontCache.DPI / (72 * FFileInfo.Head.UnitsPerEm);
end;

{ TFPFontCacheList }

procedure TFPFontCacheList.SearchForFonts(const AFontPath: String);
var
  sr: TSearchRec;
  lFont: TFPFontCacheItem;
  s: String;
begin
  if FindFirst(AFontPath + AllFilesMask, faAnyFile, sr) = 0 then
  begin
    repeat
      // check if special files to skip
      if (sr.Name = '.') or (sr.Name = '..') or (sr.Name = '') then
        Continue;
      // We got something, so lets continue
      s := sr.Name;
      if (sr.Attr and faDirectory) <> 0 then // found a directory
        SearchForFonts(IncludeTrailingPathDelimiter(AFontPath + s))
      else
      begin // we have a file
        if (lowercase(ExtractFileExt(s)) = '.ttf') or
           (lowercase(ExtractFileExt(s)) = '.otf') then
        begin
          lFont := TFPFontCacheItem.Create(AFontPath + s);
          Add(lFont);
        end;
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);
end;

procedure TFPFontCacheList.SetDPI(AValue: integer);
begin
  if FDPI = AValue then Exit;
  FDPI := AValue;
end;

function TFPFontCacheList.GetCount: integer;
begin
  Result := FList.Count;
end;

function TFPFontCacheList.GetItem(AIndex: Integer): TFPFontCacheItem;
begin
  Result := TFPFontCacheItem(FList.Items[AIndex]);
end;

procedure TFPFontCacheList.SetItem(AIndex: Integer; AValue: TFPFontCacheItem);
begin
  FList.Items[AIndex] := AValue;
end;

constructor TFPFontCacheList.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
  FSearchPath := TStringList.Create;
  FDPI := 96; // The default is the most common dpi used
end;

destructor TFPFontCacheList.Destroy;
begin
  FList.Free;
  FSearchPath.Free;
  inherited Destroy;
end;

procedure TFPFontCacheList.BuildFontCache;
var
  lPath: String;
  i: integer;
begin
  if FSearchPath.Count < 1 then
    raise ETTF.Create(rsNoSearchPathDefined);

  for i := 0 to FSearchPath.Count-1 do
  begin
    lPath := FSearchPath[i];
    if DirectoryExists(lPath) then
      SearchForFonts(IncludeTrailingPathDelimiter(lPath));
  end;
end;

function TFPFontCacheList.Add(const AObject: TFPFontCacheItem): integer;
begin
  Result := FList.Add(AObject);
  AObject.FOwner := self;
end;

procedure TFPFontCacheList.AssignFontList(const AStrings: TStrings);
var
  i: integer;
begin
  if not Assigned(AStrings) then
    Exit;
  AStrings.Clear;
  for i := 0 to FList.Count-1 do
    AStrings.Add(TFPFontCacheItem(FList.Items[i]).PostScriptName);
end;

procedure TFPFontCacheList.Clear;
begin
  FList.Clear;
end;

function TFPFontCacheList.IndexOf(const AObject: TFPFontCacheItem): integer;
begin
  Result := FList.IndexOf(AObject);
end;

function TFPFontCacheList.Find(const AFontCacheItem: TFPFontCacheItem): integer;
var
  i: integer;
begin
  Result := -1; // nothing found
  for i := 0 to Count-1 do
  begin
    if (Items[i].FamilyName = AFontCacheItem.FamilyName) and
       (Items[i].StyleFlags = AFontCacheItem.StyleFlags) then
    begin
      Result := i;
      exit;
    end;
  end;
end;

function TFPFontCacheList.Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TFPFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if (Result.FamilyName = AFamilyName) and (Result.IsItalic = AItalic)
        and (Result.IsBold = ABold)
    then
      exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.Find(const APostScriptName: string): TFPFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if (Result.PostScriptName = APostScriptName) then
      Exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.PointSizeInPixels(const APointSize: single): single;
begin
  Result := APointSize * DPI / 72;
end;


initialization
  uFontCacheList := nil;

finalization
  uFontCacheList.Free;

end.

