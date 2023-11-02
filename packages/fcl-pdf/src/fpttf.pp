{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2015 by Graeme Geldenhuys

    Description:
      This is a homegrown font cache. The fpReport reports can reference
      a font by its name. The job of the font cache is to look through
      its cached fonts to match the font name, and which *.ttf file it
      relates too. The reporting code can then extract font details
      correctly (eg: font width, height etc).

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit fpTTF;

{$mode objfpc}
{$H+}
{$modeswitch advancedrecords}
{.$define ttfdebug}

interface

uses
  Types,
  Classes,
  SysUtils,
  contnrs,
  fpparsettf;

type

  TTrueTypeFontStyle = (fsRegular, fsItalic, fsBold, fsCondensed, fsExtraLight, fsLight, fsSemibold, fsMedium, fsBlack, fsFixedWidth);
  TTrueTypeFontStyles = set of TTrueTypeFontStyle;


  { Forward declaration }
  TFPFontCacheList = class;


  { TFPFontCacheItem }

  TFPFontCacheItem = class(TObject)
  private
    FFamilyName: String;
    FFileName: String;
    FStream: TStream;
    FStyleFlags: TTrueTypeFontStyles;
    FFileInfo: TTFFileInfo;
    FOwner: TFPFontCacheList; // reference to FontCacheList that owns this instance
    FPostScriptName: string;
    FHumanFriendlyName: string; // aka FullName
    procedure   DoLoadFileInfo;
    procedure   LoadFileInfo;
    procedure   BuildFontCacheItem;
    procedure   SetStyleIfExists(var AText: string; var AStyleFlags: TTrueTypeFontStyles; const AStyleName: String; const AStyle: TTrueTypeFontStyle);
    function    GetIsBold: boolean;
    function    GetIsFixedWidth: boolean;
    function    GetIsItalic: boolean;
    function    GetIsRegular: boolean;
    function    GetFamilyName: String;
    function    GetPostScriptName: string;
    function    GetHumanFriendlyName: string;
    function    GetFileInfo: TTFFileInfo;
  public
    constructor Create(const AFilename: String); overload;
    constructor Create(const AStream: TStream); overload; // AStream is freed on destroy
    destructor  Destroy; override;
    { Result is in pixels }
    function    TextWidth(const AStr: utf8string; const APointSize: single): single;
    { Result is in pixels }
    function    TextHeight(const AText: utf8string; const APointSize: single; out ADescender: single): single;
    property    FileName: String read FFileName;
    property    Stream: TStream read FStream;
    property    FamilyName: String read GetFamilyName;
    property    PostScriptName: string read GetPostScriptName;
    property    HumanFriendlyName: string read GetHumanFriendlyName;
    property    FontData: TTFFileInfo read GetFileInfo;
    { A bitmasked value describing the full font style }
    property    StyleFlags: TTrueTypeFontStyles read FStyleFlags;
    { IsXXX properties are convenience properties, internally querying StyleFlags. }
    property    IsFixedWidth: boolean read GetIsFixedWidth;
    property    IsRegular: boolean read GetIsRegular;
    property    IsItalic: boolean read GetIsItalic;
    property    IsBold: boolean read GetIsBold;
  end;
  TFPFontCacheItemArray = Array of TFPFontCacheItem;

  { TFPFontCacheList }
  EFontNotFound = Class(Exception);

  TFPFontCacheList = class(TObject)
  private
    FBuildFontCacheIgnoresErrors: Boolean;
    FList: TObjectList; // list of TFPFontCacheItem
    FSearchPath: TStringList;
    FDPI: integer;
    procedure   SearchForFonts(const AFontPath: String);
    procedure   SetDPI(AValue: integer);
    { Set any / or \ path delimiters to the OS specific delimiter }
    procedure   FixPathDelimiters;
  protected
    function    DoFindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean; Out aBaseFont : TFPFontCacheItem): String;
    function    GetCount: integer; virtual;
    function    GetItem(AIndex: Integer): TFPFontCacheItem; virtual;
    procedure   SetItem(AIndex: Integer; AValue: TFPFontCacheItem); virtual;
  public
    constructor Create;
    destructor  Destroy; override;
    procedure   BuildFontCache;
    function    Add(const AObject: TFPFontCacheItem): integer;
    function    AddFontFromStream(AStream: TStream): integer; // add a single font from stream, returns index
    procedure   AssignFontList(const AStrings: TStrings);
    procedure   Clear;
    procedure   LoadFromFile(const AFilename: string); // load list of filenames
    procedure   ReadStandardFonts;
    property    Count: integer read GetCount;
    function    IndexOf(const AObject: TFPFontCacheItem): integer;
    // Find postscript font name based on fontname and attributes
    function    FindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;
    // Same as Find, but raise exception when not found.
    function    GetPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;
    function    Find(const AFontCacheItem: TFPFontCacheItem): integer; overload;
    function    Find(const AFamilyName: string; ABold: boolean; AItalic: boolean): TFPFontCacheItem; overload;
    function    Find(const APostScriptName: string): TFPFontCacheItem; overload;
    function    FindHumanFriendly(const AName: string ): TFPFontCacheItem; overload;
    function    FindFamily(const AFamilyName: string ): TFPFontCacheItemArray; overload;
    function    FindFont(const AName: string): TFPFontCacheItem; overload;
    { not used: utility function doing a conversion for us. }
    function    PointSizeInPixels(const APointSize: single): single;
    property    Items[AIndex: Integer]: TFPFontCacheItem read GetItem write SetItem; default;
    property    SearchPath: TStringList read FSearchPath;
    property    DPI: integer read FDPI write SetDPI;
    Property    BuildFontCacheIgnoresErrors : Boolean Read FBuildFontCacheIgnoresErrors Write FBuildFontCacheIgnoresErrors;
  end;

function gTTFontCache: TFPFontCacheList;

type
  { TFontMapper }

  TFontMapper = class
    class function find(const family, style:string; List:TStrings):boolean; overload;
    class function find(const family, style:string; out List: TStringDynArray):boolean;
  end;

const
  style_regular = 'regular';
  style_bold = 'bold';
  style_italic = 'italic';


implementation

uses
  DOM
  , XMLRead
  , Strutils
  {$ifdef mswindows}
  ,Windows,  // for SHGetFolderPath API call used by gTTFontCache.ReadStandardFonts() method
  Shlobj, activex, registry
  {$endif}
  {$if (defined(LINUX) or defined(BSD)) and not defined(DARWIN)}
  , libfontconfig, unixtype
  {$ifend}
  ;

resourcestring
  rsNoSearchPathDefined = 'No search path was defined';
  rsNoFontFileName = 'The FileName property is empty, so we can''t load font data.';
  rsMissingFontFile = 'The font file <%s> can''t be found.';
  SErrFontNotFound = 'The font <%s> can''t be found';

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

procedure TFPFontCacheItem.DoLoadFileInfo;
begin
  if not Assigned(FFileInfo) then
    LoadFileInfo;
end;

procedure TFPFontCacheItem.LoadFileInfo;
begin
  if FStream<>nil then
  begin
    FFileInfo := TTFFileInfo.Create;
    FFileInfo.LoadFromStream(FStream);
  end else if (FFilename<>'') and FileExists(FFilename) then
  begin
    FFileInfo := TTFFileInfo.Create;
    FFileInfo.LoadFromFile(FFilename);
  end
  else
    raise ETTF.CreateFmt(rsMissingFontFile, [FFilename]);
  BuildFontCacheItem;
end;

function TFPFontCacheItem.GetIsBold: boolean;
begin
  DoLoadFileInfo;
  Result := fsBold in FStyleFlags;
end;

function TFPFontCacheItem.GetIsFixedWidth: boolean;
begin
  DoLoadFileInfo;
  Result := fsFixedWidth in FStyleFlags;
end;

function TFPFontCacheItem.GetIsItalic: boolean;
begin
  DoLoadFileInfo;
  Result := fsItalic in FStyleFlags;
end;

function TFPFontCacheItem.GetIsRegular: boolean;
begin
  DoLoadFileInfo;
  Result := fsRegular in FStyleFlags;
end;

function TFPFontCacheItem.GetFamilyName: String;
begin
  DoLoadFileInfo;
  Result := FFamilyName;
end;

function TFPFontCacheItem.GetPostScriptName: string;
begin
  DoLoadFileInfo;
  Result := FPostScriptName;
end;

function TFPFontCacheItem.GetHumanFriendlyName: string;
begin
  DoLoadFileInfo;
  Result := FHumanFriendlyName;
end;

function TFPFontCacheItem.GetFileInfo: TTFFileInfo;
begin
  DoLoadFileInfo;
  Result := FFileInfo;
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
  FHumanFriendlyName := FFileInfo.HumanFriendlyName;

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
end;

constructor TFPFontCacheItem.Create(const AStream: TStream);
begin
  inherited Create;
  if AStream = nil then
    raise ETTF.Create(rsNoFontFileName);

  FStream := AStream;
  FStyleFlags := [fsRegular];
end;

destructor TFPFontCacheItem.Destroy;
begin
  FreeAndNil(FStream);
  FreeAndNil(FFileInfo);
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
  DoLoadFileInfo;
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
  DoLoadFileInfo;
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
  if SysUtils.FindFirst(AFontPath + AllFilesMask, faAnyFile, sr) = 0 then
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
          try
            lFont := TFPFontCacheItem.Create(AFontPath + s);
            Add(lFont);
          except
            if not FBuildFontCacheIgnoresErrors then
              Raise;
          end;
        end;
      end;
    until SysUtils.FindNext(sr) <> 0;
  end;
  SysUtils.FindClose(sr);
end;

procedure TFPFontCacheList.SetDPI(AValue: integer);
begin
  if FDPI = AValue then Exit;
  FDPI := AValue;
end;

procedure TFPFontCacheList.FixPathDelimiters;
var
  i: integer;
begin
  for i := 0 to FSearchPath.Count-1 do
    FSearchPath[i] := SetDirSeparators(FSearchPath[i]);
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

  FixPathDelimiters;
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

procedure TFPFontCacheList.LoadFromFile(const AFilename: string);
var
  sl: TStringList;
  i: integer;
begin
  sl := TStringList.Create;
  try
    sl.LoadFromFile(AFilename);
    for i := 0 to sl.Count-1 do
      Add(TFPFontCacheItem.Create(sl[i]));
  finally
    sl.Free;
  end;
end;

function TFPFontCacheList.AddFontFromStream(AStream: TStream): integer;
var
  ms: TMemoryStream;
  Item: TFPFontCacheItem;
begin
  ms:=TMemoryStream.Create;
  ms.CopyFrom(AStream,AStream.Size-AStream.Position);
  ms.Position:=0;
  Item:=TFPFontCacheItem.Create(ms);
  Result:=Add(Item);
  if Item.FamilyName='' then
    raise EFontNotFound.Create('TFPFontCacheList.AddFontFromStream font has no family name');
end;

{ This is operating system dependent. Our default implementation only supports
  Linux, FreeBSD, Windows and OSX. On other platforms, no fonts will be loaded,
  until a implementation is created.

  NOTE:
    This is definitely not a perfect solution, especially due to the inconsistent
    implementations and locations of files under various Linux distros. But it's
    the best we can do for now. }

{$ifdef mswindows}
function GetWinFontsDir: string;

var
  {$if FPC_FULLVERSION < 30400}
  w :  Array[0..MaxPathLen] of AnsiChar;
  {$ELSE}
  w : pwidechar;
  {$ENDIF}

begin
  {$if FPC_FULLVERSION < 30400}
  SHGetSpecialFolderPath(0,w,CSIDL_FONTS,false);
  {$else}
  SHGetKnownFolderPath(FOLDERID_Fonts,0,0,w);
  {$endif}
  Result := w;
  {$if FPC_FULLVERSION > 30400}
  CoTaskMemFree(w);
  {$endif}
end;
{$endif}

procedure TFPFontCacheList.ReadStandardFonts;

  {$ifdef freebsd}
    {$define HasFontsConf}
    const
      cFontsConf = '/usr/local/etc/fonts/fonts.conf';
  {$endif}
  { Use same default for Linux and other BSD non-Darwin systems. }
  {$if (defined(linux) or (defined(bsd) and not(defined(darwin)) and not defined(HasFontsConf)))}
    {$define HasFontsConf}
    const
      cFontsConf = '/etc/fonts/fonts.conf';
  {$ifend}


  {$ifdef mswindows}
  function GetWinFontsDir: string;
  var
    {$if FPC_FULLVERSION < 30400}
    w :  Array[0..MaxPathLen] of Char;
    {$ELSE}
    w : pwidechar;
    {$ENDIF}
  begin
    {$if FPC_FULLVERSION < 30400}
    SHGetSpecialFolderPath(0,w,CSIDL_FONTS,false);
    {$else}
    SHGetKnownFolderPath(FOLDERID_Fonts,0,0,w);
    {$endif}
    Result := w;
    {$if FPC_FULLVERSION > 30400}
    CoTaskMemFree(w);
    {$endif}
  end;
{$endif}

{$ifdef HasFontsConf}
var
  doc: TXMLDocument;
  lChild: TDOMNode;
  FN : PFcChar8;
  lDir: string;
  config: PfcConfig;
const
  is_fc_loaded:integer=0;
{$endif}
begin
  {$ifdef HasFontsConf} // Linux & BSD
  if (is_fc_loaded=0) then
    is_fc_loaded:=loadfontconfiglib('');

  config := FcInitLoadConfigAndFonts();

  if assigned(FcConfigGetFilename) then
    FN:=FcConfigGetFilename(config,Nil)
  else if assigned(FcConfigFilename) then
    FN:=FcConfigFilename(Nil)
  else
    FN:=cFontsConf;
  ReadXMLFile(doc, FN);
  try
    lChild := doc.DocumentElement.FirstChild;
    while Assigned(lChild) do
    begin
      if lChild.NodeName = 'dir' then
      begin
        if lChild.FirstChild.NodeValue = '~/.fonts' then
          lDir := ExpandFilename(lChild.FirstChild.NodeValue)
        else
          lDir := lChild.FirstChild.NodeValue;
        SearchPath.Add(lDir);
//        writeln(lDir);
      end;
      lChild := lChild.NextSibling;
    end;
  finally
    doc.Free;
  end;
  {$endif}

  {$ifdef mswindows}
  SearchPath.Add(GetWinFontsDir);
  {$endif}

  {$ifdef darwin} // OSX
  { As per Apple Support page: https://support.apple.com/en-us/HT201722 }
  SearchPath.Add('/System/Library/Fonts/');
  SearchPath.Add('/Library/Fonts/');
  SearchPath.Add(ExpandFilename('~/Library/Fonts/'));
  {$endif}

  BuildFontCache;
end;

function TFPFontCacheList.IndexOf(const AObject: TFPFontCacheItem): integer;
begin
  Result := FList.IndexOf(AObject);
end;

function TFPFontCacheList.GetPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;

Var
  lFC : TFPFontCacheItem;
  lMissingFontName : String;

begin
  Result:=DoFindPostScriptFontName(aFontName,aBold,aItalic,lfc);
  if (Result=aFontName) and (aBold or aItalic) then
    begin
    if lFC<>Nil then
      lMissingFontName := lfc.FamilyName
    else
      lMissingFontName := aFontName;
    if (aBold and AItalic) then
      lMissingFontName := lMissingFontName + '-BoldItalic'
    else if aBold then
      lMissingFontName := lMissingFontName + '-Bold'
    else if aItalic then
      lMissingFontName := lMissingFontName + '-Italic';
    raise EFontNotFound.CreateFmt(SErrFontNotFound, [lMissingFontName]);
    end;
end;

function TFPFontCacheList.FindPostScriptFontName(const AFontName: string; ABold: boolean; AItalic: boolean): String;

Var
  lFC : TFPFontCacheItem;

begin
  Result:=DoFindPostScriptFontName(aFontName,aBold,aItalic,lfc);
end;

function TFPFontCacheList.DoFindPostScriptFontName(const AFontName: string;
  ABold: boolean; AItalic: boolean; out aBaseFont: TFPFontCacheItem): String;

Var
   lNewFC : TFPFontCacheItem;

begin
  Result:=aFontName;
  aBaseFont := FindFont(aFontName);
  if not Assigned(aBaseFont) then
    exit;
  // find corresponding font style (bold and/or italic)
  lNewFC := Find(aBaseFont.FamilyName, aBold, aItalic);
  if not Assigned(lNewFC) then
    exit;
  Result := lNewFC.PostScriptName;
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
    if SameText(Result.FamilyName,AFamilyName) and (Result.IsItalic = AItalic)
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
    if SameText(Result.PostScriptName,APostScriptName) then
      Exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.FindHumanFriendly(const AName: string): TFPFontCacheItem;
var
  i: integer;
begin
  for i := 0 to Count-1 do
  begin
    Result := Items[i];
    if SameText(Result.HumanFriendlyName,AName) then
      Exit;
  end;
  Result := nil;
end;

function TFPFontCacheList.FindFamily(const AFamilyName: string): TFPFontCacheItemArray;

var
  i,aLen: integer;
  f : TFPFontCacheItem;

begin
  aLen:=0;
  SetLength(Result,Count);
  for i := 0 to Count-1 do
    begin
    f:=Items[i];
    if SameText(F.FamilyName,AFamilyName) then
      begin
      Result[aLen]:=F;
      inc(Alen);
      end;
    end;
  SetLength(Result,aLen);
end;

function TFPFontCacheList.FindFont(const AName: string): TFPFontCacheItem;

Var
  aFamily : TFPFontCacheItemArray;

begin
  Result:=Find(AName);
  if (Result=Nil) then
    Result:=Find(aName,False,False);
  if (Result=Nil) then
    Result:=FindHumanFriendly(aName);
  if (Result=Nil) then
    begin
    aFamily:=FindFamily(aName);
    if Length(aFamily)>0 then
      Result:=aFamily[0];
    end;
end;

function TFPFontCacheList.PointSizeInPixels(const APointSize: single): single;
begin
  Result := APointSize * DPI / 72;
end;

{ ----------------------------------------------------------------------
  TFontMapper
  ----------------------------------------------------------------------}

class function TFontMapper.find(const family, style:string; out List: TStringDynArray):boolean;

var
  L : TStrings;

begin
  L:=TStringList.Create;
  try
    Result:=Find(family,style,L);
    if Result then
      List:=L.ToStringArray();
  finally
    L.Free;
  end;
end;

{$if (defined(LINUX) or defined(BSD)) and not defined(DARWIN)}

//https://stackoverflow.com/questions/10542832/how-to-use-fontconfig-to-get-font-list-c-c
class function TFontMapper.find(const family, style: string; list: TStrings): boolean;

var
  res:utf8string;
  // libfontconfig version
  config: PfcConfig;
  pat, font: PfcPattern;
  ffile: PfcChar8;
  mres:TFcResult;
const
  is_fc_loaded:integer=0;
begin
  Result:=false;
  res:='';

  if (is_fc_loaded=0) then
    is_fc_loaded:=loadfontconfiglib('');

  config := FcInitLoadConfigAndFonts();

  // configure the search pattern,
  // assume "name" is a std::string with the desired font name in it
  res:=family+':style='+style;
  pat := FcNameParse(PChar(res));
  FcConfigSubstitute(config, pat, FcMatchPattern);
  FcDefaultSubstitute(pat);

  // find the font
  font := FcFontMatch(config, pat, @mres);
  if Assigned(font) then
    begin
    FFile:=nil;
    res:=FC_FILE;
    if (FcPatternGetString(font,PcChar(res),0,@ffile) = FcResultMatch) then
      begin
      if FFile<>'' then
        List.Add(StrPas(ffile));
      Result:=true;
     end;
    FcPatternDestroy(font);
    end;
  FcPatternDestroy(pat);
end;
{$define tfontmapper_find_implemented}
{$endif}


{$IF DEFINED(MSWINDOWS) or DEFINED(DARWIN)}
Type

  { TFontItem }

  TFontItem = class
    weight : integer;
    name : UTF8String;
    Constructor Create(aWeight : Integer; aName : UTF8String);
  end;
  TMatchList = array of TFontItem;

  { TFontEnumerator }

  TFontEnumerator = Record
  public
    family,fstyle:string;
    lstyle: TStringDynArray;
    matches: TFPObjectList;
    procedure init;
    procedure done;
    procedure clear;
    procedure AddDesc(const fi:TFontItem);
    function MatchFont(const fdesc:utf8string):integer;
    function get_lst(lst: TStrings):boolean;
    procedure set_style(const str:string);
    property style:string read fstyle write set_style;
  end;

{ TFontItem }

constructor TFontItem.Create(aWeight: Integer; aName: UTF8String);
begin
  Weight:=aWeight;
  Name:=aName;
end;

Procedure TFontEnumerator.init;
begin
  family:='';
  fstyle:='';
  lstyle:=[];
  Matches:=TFPObjectList.Create(True);
  Clear;
end;

procedure TFontEnumerator.done;
begin
  lstyle:=[];
  FreeAndNil(matches);
end;

procedure TFontEnumerator.clear;

begin
  Matches.Clear;
end;

procedure TFontEnumerator.set_style(const str:string);
begin
  fstyle:=str;
  if fstyle='' then fstyle:='normal regular';
  lstyle:=SplitString(fstyle,' ');
end;

procedure TFontEnumerator.AddDesc(const fi:TFontItem);
begin
  matches.Add(fi);
end;

function TFontEnumerator.MatchFont(const fdesc:utf8string):integer;
var
  pn,i,pa:integer;
  slfn,satt:string;
begin
  Result:=0;
  pn:=pos(family,fdesc); // position of name
  if pn=1 then
    inc(Result,100)
  else if pn>0 then
    inc(Result,50)
  else
    exit;
  satt:=copy(fdesc,pn+length(family)+1,length(fdesc));
  slfn:=lowercase(satt);
  if (pn=1) and (pos(style_regular,fstyle)>0) then
    begin
    if (satt='') then
      exit;
    end;
  for i:=0 to high(lstyle) do
    begin
    pa:=pos(lstyle[i],slfn);
    if pa>0 then
      begin
      delete(slfn,pa,length(lstyle[i]));
      slfn:=trim(slfn);
      inc(Result,50)
      end
    else
      dec(result,10);
  end;
  // there is unmatched attrs
  if length(slfn)>0 then
     dec(result,10);

  if Result>0 then
    exit;
  Result:=0;
end;

function CompareWeight(Left,Right : Pointer): Integer;
begin
  Result := (TFontItem(Right).weight - TFontItem(Left).weight);
end;

function TFontEnumerator.get_lst(lst:TStrings):boolean;
var i:integer;
begin
  // sort
  Result:=Matches.Count>0;
  if not Result then exit;
  Matches.Sort(@CompareWeight);
  //QuickSort_PtrList_NoContext(PPointer(Matches),Matches),@CompareWeight);
  for i:=0 to Matches.Count-1 do
    lst.Add(TFontItem(matches[i]).name);
end;
{$ENDIF}

{$IFDEF WINDOWS}
class function TFontMapper.find(const family, style: string; list: TStrings): boolean;

var
  I: Integer;
  reg: TRegistry;
  enum : TFontEnumerator;
  fpath :string;
  FI : TFontItem;

  procedure HandleValue(const AParam: UTF8String);

  var
    ptt,aweight:integer;
    spar : UTF8String;


  begin
    Result:=true;
    ptt:=pos(' (TrueType)',AParam);
    if ptt<=0 then
      exit;
    spar:=copy(AParam,1,ptt-1);
    aWeight:=Enum.MatchFont(spar);
    if aWeight>0 then
      enum.AddDesc(TFontItem.Create(aWeight,AParam));
  end;

  procedure ProcessValues;
  var
    n : Unicodestring;
  begin
    For N in reg.GetValueNames do
      HandleValue(UTF8Encode(N));
  end;

begin
  Result:=false;
  enum:=Default(TFontEnumerator);
  reg:=TRegistry.Create;
  try
    reg.RootKey:=HKEY_LOCAL_MACHINE;
    reg.Access:=KEY_READ;
    if not reg.OpenKey('Software\Microsoft\Windows NT\CurrentVersion\Fonts',false) then
      exit;
    enum.init;
    enum.family:=family;
    enum.style:=style;
    ProcessValues;
    if (enum.matches.Count=0) then // no matches
      begin
      enum.clear;
      if (pos('Sans',enum.family)>0) then
         enum.family:='Arial'
      else if (pos('Mono',enum.family)>0) then
         enum.family:='Courier New';
      ProcessValues;
      end;
    if enum.matches.Count>0 then // there are matches
      begin
      fpath:=IncludeTrailingPathDelimiter(GetWinFontsDir);
      for i:=enum.matches.Count-1 downto 0 do
        begin
        FI:=TFontItem(enum.matches[i]);
        FI.name:=fpath+reg.ReadString(FI.name);
        if not FileExists(FI.Name) then
          Enum.matches.Delete(i)
        end;
      Result:=enum.get_lst(list);
      end;
  finally
    enum.done;
    reg.Free;
  end;
end;
{$define tfontmapper_find_implemented}
{$endif}

{$ifdef DARWIN}
class function TFontMapper.find(const family, style: string; list: TStrings): boolean;

var
  enum : TFontEnumerator;
  procedure HandleValue(const AParam:string);

  var
    spar :string;
    aweight : integer;

  begin
    Result:=true;
    spar:=ChangeFileExt(ExtractFileName(AParam),'');
    spar:=StringReplace(spar,'_',' ',[rfReplaceAll]);
    aWeight:=Enum.MatchFont(spar);
    if (aweight>0) then
      enum.Matches.Add(TFontItem.Create(aWeight,AParam));
  end;

  Procedure DoDir(aDir : string);

  var
    sr : TSearchRec;
  begin
    if FindFirst(aDir+'*',faAnyFile,sr)=0 then
      try
        enum.family:=family;
        enum.style:=style;
        repeat
           if (sr.Attr and faDirectory)=0 then
             HandleValue(aDir+sr.Name);
        until (FindNext(sr)<>0);
      finally
        FindClose(sr);
      end;
   end;

const
  syspath1 = '/System/Library/Fonts/Supplemental/';
  syspath2 = '/System/Library/Fonts/';
  syspath3 = '/Library/Fonts/';
  syspath4 = '~/Library/Fonts/';


begin
  Result:=false;
  enum:=Default(TFontEnumerator);
  enum.init;
  try
    DoDir(SysPath1);
    DoDir(SysPath2);
    DoDir(SysPath3);
    DoDir(ExpandFileName(SysPath4));
    Result:=enum.get_lst(list);
  finally
    enum.done;
  end;
end;
{$define tfontmapper_find_implemented}
{$endif}

{$ifndef tfontmapper_find_implemented}
class function TFontMapper.find(const family, style: string; list: TStrings): boolean;
begin
  Result:=false;
end;
{$endif}

initialization
  uFontCacheList := nil;

finalization
  uFontCacheList.Free;
end.


