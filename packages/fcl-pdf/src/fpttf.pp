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

const
  { constants to query FontCacheItem.StyleFlags with. }
  FP_FONT_STYLE_REGULAR = 1 shl 0;     { Regular, Plain, Book }
  FP_FONT_STYLE_ITALIC = 1 shl 1;      { Italic }
  FP_FONT_STYLE_BOLD = 1 shl 2;        { Bold }
  FP_FONT_STYLE_CONDENSED = 1 shl 3;   { Condensed }
  FP_FONT_STYLE_EXTRALIGHT = 1 shl 4;  { ExtraLight }
  FP_FONT_STYLE_LIGHT = 1 shl 5;       { Light }
  FP_FONT_STYLE_SEMIBOLD = 1 shl 6;    { Semibold }
  FP_FONT_STYLE_MEDIUM = 1 shl 7;      { Medium }
  FP_FONT_STYLE_BLACK = 1 shl 8;       { Black }
  FP_FONT_STYLE_FIXEDWIDTH = 1 shl 9;  { Fixedwidth }

type
  { Forward declaration }
  TFPFontCacheList = class;


  TFPFontCacheItem = class(TObject)
  private
    FFamilyName: String;
    FFileName: String;
    FStyleFlags: LongWord;
    FOwner: TFPFontCacheList; // reference to FontCacheList that owns this instance
    function    GetIsBold: boolean;
    function    GetIsFixedWidth: boolean;
    function    GetIsItalic: boolean;
    function    GetIsRegular: boolean;
    procedure   SetIsBold(AValue: boolean);
    procedure   SetIsFixedWidth(AValue: boolean);
    procedure   SetIsItalic(AValue: boolean);
    procedure   SetIsRegular(AValue: boolean);
  public
    constructor Create(const AFilename: String);
    { Returns the actual TTF font file information. Caller needs to free the returned instance. }
    function    GetFontData: TTFFileInfo;
    { Result is in pixels }
    function    TextWidth(AStr: string; APointSize: single): single;
    property    FileName: String read FFileName write FFileName;
    property    FamilyName: String read FFamilyName write FFamilyName;
    { A bitmasked value describing the full font style }
    property    StyleFlags: LongWord read FStyleFlags write FStyleFlags;
    { IsXXX properties are convenience properties, internally querying StyleFlags. }
    property    IsFixedWidth: boolean read GetIsFixedWidth write SetIsFixedWidth;
    property    IsRegular: boolean read GetIsRegular write SetIsRegular;
    property    IsItalic: boolean read GetIsItalic write SetIsItalic;
    property    IsBold: boolean read GetIsBold write SetIsBold;
  end;


  TFPFontCacheList = class(TObject)
  private
    FList: TObjectList;
    FSearchPath: TStringList;
    FDPI: integer;
    procedure   SearchForFont(const AFontPath: String);
    function    BuildFontCacheItem(const AFontFile: String): TFPFontCacheItem;
    procedure   SetStyleIfExists(var AText: string; var AStyleFlags: integer; const AStyleName: String; const AStyleBit: integer);
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
    procedure   Clear;
    property    Count: integer read GetCount;
    function    IndexOf(const AObject: TFPFontCacheItem): integer;
    function    Find(const AFontCacheItem: TFPFontCacheItem): integer;
    function    Find(const AFamilyName: string; ABold: boolean = False; AItalic: boolean = False): TFPFontCacheItem;
    { not used: utility function doing a conversion for use. }
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

type
  { so we can get access to protected methods }
  TFriendTTFFileInfo = class(TTFFileInfo);

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
  Result := (FStyleFlags and FP_FONT_STYLE_BOLD) <> 0;
end;

function TFPFontCacheItem.GetIsFixedWidth: boolean;
begin
  Result := (FStyleFlags and FP_FONT_STYLE_FIXEDWIDTH) <> 0;
end;

function TFPFontCacheItem.GetIsItalic: boolean;
begin
  Result := (FStyleFlags and FP_FONT_STYLE_ITALIC) <> 0;
end;

function TFPFontCacheItem.GetIsRegular: boolean;
begin
  Result := (FStyleFlags and FP_FONT_STYLE_REGULAR) <> 0;
end;

procedure TFPFontCacheItem.SetIsBold(AValue: boolean);
begin
  if AValue then
    FStyleFlags := FStyleFlags or FP_FONT_STYLE_BOLD
  else
    FStyleFlags := FStyleFlags and (not FP_FONT_STYLE_BOLD);
end;

procedure TFPFontCacheItem.SetIsFixedWidth(AValue: boolean);
begin
  if AValue then
    FStyleFlags := FStyleFlags or FP_FONT_STYLE_FIXEDWIDTH
  else
    FStyleFlags := FStyleFlags and (not FP_FONT_STYLE_FIXEDWIDTH);

  // if we are FixedWidth, then Regular can't apply
  FStyleFlags := FStyleFlags and (not FP_FONT_STYLE_REGULAR);
end;

procedure TFPFontCacheItem.SetIsItalic(AValue: boolean);
begin
  if AValue then
    FStyleFlags := FStyleFlags or FP_FONT_STYLE_ITALIC
  else
    FStyleFlags := FStyleFlags and (not FP_FONT_STYLE_ITALIC);
end;

procedure TFPFontCacheItem.SetIsRegular(AValue: boolean);
begin
  if AValue then
    FStyleFlags := FStyleFlags or FP_FONT_STYLE_REGULAR
  else
    FStyleFlags := FStyleFlags and (not FP_FONT_STYLE_REGULAR);

  // if we are Regular, then FixedWidth can't apply
  FStyleFlags := FStyleFlags and (not FP_FONT_STYLE_FIXEDWIDTH);
end;

constructor TFPFontCacheItem.Create(const AFilename: String);
begin
  inherited Create;
  FFileName := AFilename;
  FStyleFlags := FP_FONT_STYLE_REGULAR;
end;

function TFPFontCacheItem.GetFontData: TTFFileInfo;
begin
  if FileName = '' then
    raise ETTF.Create(rsNoFontFileName);
  if FileExists(FileName) then
  begin
    Result := TTFFileInfo.Create;
    Result.LoadFromFile(FileName);
  end
  else
    Result := nil;
end;

{ TextWidth returns with width of the text. If APointSize = 0.0, then it returns
  the text width in Font Units. If APointSize > 0 then it returns the text width
  in Pixels. }
function TFPFontCacheItem.TextWidth(AStr: string; APointSize: single): single;
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
  lFntInfo: TFriendTTFFileInfo;
  i: integer;
  lWidth: integer;
  lGIndex: integer;
  c: Char;
  {$IFDEF ttfdebug}
  sl: TStringList;
  s: string;
  {$ENDIF}
begin
  Result := 0;
  if Length(AStr) = 0 then
    Exit;

  lFntInfo := TFriendTTFFileInfo(GetFontData);
  if not Assigned(lFntInfo) then
    Exit;

  {$IFDEF ttfdebug}
    sl := TStringList.Create;
    s := '';
    for i := 0 to 255 do
    begin
      lGIndex := lFntInfo.GetGlyphIndex(i);
      lWidth := lFntInfo.GetAdvanceWidth(lGIndex);
      s := s + ',' + IntToStr(lWidth);
    end;
    sl.Add(s);
    sl.Add('UnitsPerEm = ' + IntToStr(lFntInfo.Head.UnitsPerEm));
    sl.SaveToFile('/tmp/' + lFntInfo.PostScriptName + '.txt');
    sl.Free;
  {$ENDIF}

  try
    lWidth := 0;
    for i := 1 to Length(AStr) do
    begin
      c := AStr[i];
      lGIndex := lFntInfo.GetGlyphIndex(Ord(c));
      lWidth := lWidth + lFntInfo.GetAdvanceWidth(lGIndex);
    end;

    if APointSize = 0.0 then
      Result := lWidth
    else
    begin
      { Converting Font Units to Pixels. The formula is:
        pixels = glyph_units * pointSize * resolution / ( 72 points per inch * THead.UnitsPerEm )  }
      Result := lWidth * APointSize * FOwner.DPI / (72 * lFntInfo.Head.UnitsPerEm);
    end;
  finally
    lFntInfo.Free;
  end;
end;

{ TFPFontCacheList }

procedure TFPFontCacheList.SearchForFont(const AFontPath: String);
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
        SearchForFont(IncludeTrailingPathDelimiter(AFontPath + s))
      else
      begin // we have a file
        if (lowercase(ExtractFileExt(s)) = '.ttf') or
           (lowercase(ExtractFileExt(s)) = '.otf') then
        begin
          lFont := BuildFontCacheItem(AFontPath + s);
          Add(lFont);
        end;
      end;
    until FindNext(sr) <> 0;
  end;
  FindClose(sr);
end;

function TFPFontCacheList.BuildFontCacheItem(const AFontFile: String): TFPFontCacheItem;
var
  lFontInfo: TTFFileInfo;
  s: string;
  flags: integer;
begin
  lFontInfo := TTFFileInfo.Create;
  try
    lFontInfo.LoadFromFile(AFontFile);

    Result := TFPFontCacheItem.Create(AFontFile);
    s := lFontInfo.PostScriptName;
    Result.FamilyName := lFontInfo.FamilyName;

    // extract simple styles first
    if lFontInfo.PostScript.isFixedPitch > 0 then
      Result.StyleFlags := FP_FONT_STYLE_FIXEDWIDTH; // this should overwrite Regular style

    if lFontInfo.PostScript.ItalicAngle <> 0 then
      Result.StyleFlags := Result.StyleFlags or FP_FONT_STYLE_ITALIC;

    // Now to more complex styles stored in StyleName field. eg: 'Condensed Medium'
    flags := Result.StyleFlags;
    SetStyleIfExists(s, flags, 'Bold', FP_FONT_STYLE_BOLD);
    SetStyleIfExists(s, flags, 'Condensed', FP_FONT_STYLE_CONDENSED);
    SetStyleIfExists(s, flags, 'ExtraLight', FP_FONT_STYLE_EXTRALIGHT);
    SetStyleIfExists(s, flags, 'Light', FP_FONT_STYLE_LIGHT);
    SetStyleIfExists(s, flags, 'Semibold', FP_FONT_STYLE_SEMIBOLD);
    SetStyleIfExists(s, flags, 'Medium', FP_FONT_STYLE_MEDIUM);
    SetStyleIfExists(s, flags, 'Black', FP_FONT_STYLE_BLACK);
    Result.StyleFlags := flags;
  finally
    lFontInfo.Free;
  end;
end;

procedure TFPFontCacheList.SetStyleIfExists(var AText: string; var AStyleFlags: integer; const AStyleName: String;
  const AStyleBit: integer);
var
  i: integer;
begin
  i := Pos(AStyleName, AText);
  if i > 0 then
  begin
    AStyleFlags := AStyleFlags or AStyleBit;
    Delete(AText, Length(AStyleName), i);
  end;
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
    SearchForFont(IncludeTrailingPathDelimiter(lPath));
  end;
end;

function TFPFontCacheList.Add(const AObject: TFPFontCacheItem): integer;
begin
  Result := FList.Add(AObject);
  AObject.FOwner := self;
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
  Result := nil;
  for i := 0 to Count-1 do
  begin
    if (Items[i].FamilyName = AFamilyName) and (items[i].IsItalic = AItalic)
        and (items[i].IsBold = ABold) then
    begin
      Result := Items[i];
      exit;
    end;
  end;
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

