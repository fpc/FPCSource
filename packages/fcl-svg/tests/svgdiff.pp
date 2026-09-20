{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Renders a suite of SVG documents and compares them against references.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program svgdiff;

{$mode objfpc}{$H+}
{$IFDEF UNICODERTL}
{$modeswitch unicodestrings}
{$ENDIF}

uses
{$IF defined(UNIX) and defined(UNICODERTL)}
  // A program of the unicode RTL converts its own strings.
  {$IFDEF FPC_DOTTEDUNITS}
     UnixApi.CWString,
  {$ELSE FPC_DOTTEDUNITS}
     cwstring,
  {$ENDIF FPC_DOTTEDUNITS}
{$ENDIF}
{$IFDEF FPC_DOTTEDUNITS}
     System.SysUtils, System.Classes, System.StrUtils, System.Math,
     Fcl.CustApp, FpImage, FpImage.Writer.PNG, FpImage.Reader.PNG,
     FpImage.Reader.JPEG, FpImage.Reader.GIF, FpImage.Reader.Bitmap,
     FpImage.Reader.PNM, FpImage.Reader.Targa, FpImage.Reader.XPM,
{$ELSE FPC_DOTTEDUNITS}
     sysutils, classes, strutils, math, custapp, fpimage, fpwritepng,
     fpreadpng, fpreadjpeg, fpreadgif, fpreadbmp, fpreadpnm, fpreadtga,
     fpreadxpm,
{$ENDIF FPC_DOTTEDUNITS}
     svgcompare, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.text,
     fpsvg.raster, fpsvg.soft, fpsvg.freetype, fpsvg.fonts.support,
{$IFDEF DARWIN}
     fpsvg.coretext,
{$ENDIF}
{$IFDEF WINDOWS}
     fpsvg.gdi,
{$ENDIF}
     fpsvg.fonts.provider;

const
  DefaultSuite = 'w3c';
  DefaultRoot = 'tests' + PathDelim + 'suites';
  DefaultOutput = 'build' + PathDelim + 'pixels';
  DefaultTolerance = 2;
  DefaultThreshold = 0.5;
  DefaultWidth = 480;
  DefaultHeight = 360;

type
  TSVGDiffOutcome = (doPass, doFail, doNoReference, doError);

  { One line of the report: the test, and how far its rendering fell from
    the reference. Percentage is -1 where nothing was compared. }
  TSVGDiffRow = record
    Name       : String;
    Percentage : Double;
    Revision   : Double;
    Border     : Double;
    Outcome    : TSVGDiffOutcome;
  end;
  TSVGDiffRowArray = array of TSVGDiffRow;

  { The links that the operator of a test was told to follow first.
    Some documents of the suite ask for that before their reference image
    makes sense, and they say which file to follow. }
  TSVGVisitedList = class(TInterfacedObject, ISVGLinkHistory)
  private
    FHRefs: TStringList;
  public
    constructor Create(const aHRefs: array of String);
    destructor Destroy; override;
    function WasVisited(const aHRef, aBaseURI: String): Boolean;
  end;

  TSVGDiffApplication = class(TCustomApplication)
  private
    FRoot, FSuite, FSVGDir, FRefDir, FOutDir, FFilter, FHtml: String;
    FInScope, FNoWatermark, FMaskWatermark, FMaskFrame: Boolean;
    FRendered: String;
    FPaintSample: TSVGPaintSample;
    FLeftOut, FUnmarked, FMasked: Integer;
    // Rows the watermark of the document last rendered covers. Zero when
    // it has none, and when no watermark was requested.
    FBand: Integer;
    // The box its revision line covers, and whether it has one.
    FRevLeft, FRevTop, FRevRight, FRevBottom: Integer;
    FHasRevision: Boolean;
    FFrameLeft, FFrameTop, FFrameRight, FFrameBottom, FFrameMargin: Integer;
    FHasFrame: Boolean;
    FRevPixels, FBorderPixels, FDiffPixels: Int64;
    FTolerance: Integer;
    FThreshold: Double;
    FBackground: TSVGColor;
    FWriteDiff, FWriteRender, FQuiet, FList, FSaveReference: Boolean;
    FWriteDistance: Boolean;
    FSubSamples: Integer;
    FWidth, FHeight: Integer;
    FEngine: String;
    FFonts: ISVGFontProvider;
    FFreeType: TSVGFreeTypeProvider;
{$IFDEF DARWIN}
    FCoreText: TSVGCoreTextProvider;
{$ENDIF}
{$IFDEF WINDOWS}
    FGDI: TSVGGDIProvider;
{$ENDIF}
    FImages: TSVGFileImageResolver;
    FDocuments: TSVGFileDocumentResolver;
    FStyleSheets: TSVGFileStyleSheetResolver;
    FFontFiles: TSVGFileFontResolver;
    FVisited: ISVGLinkHistory;
    FPassed, FFailed, FMissing, FErrored: Integer;
    FRows: TSVGDiffRowArray;
    FRowCount: Integer;
    procedure ReadOptions;
    procedure AddRow(const aName: String; aPercentage, aRevision,
      aBorder: Double;
      aOutcome: TSVGDiffOutcome);
    procedure SortRows;
    procedure Summarise(aSkipExplained: Boolean; out aMean,
      aMedian: Double; out aCount: Integer);
    procedure DropWatermark(aDocument: TSVGDocument);
    function WatermarkBand(aDocument: TSVGDocument; aRenderer: TSVGRenderer;
      aHeight: Integer): Integer;
    function FrameBand(aDocument: TSVGDocument; aRenderer: TSVGRenderer;
      aWidth, aHeight: Integer): Boolean;
    function CountBorder(aActual, aExpected: TFPMemoryImage): Integer;
    function RevisionBox(aDocument: TSVGDocument; aRenderer: TSVGRenderer;
      aWidth, aHeight: Integer;
      out aLeft, aTop, aRight, aBottom: Integer): Boolean;
    procedure WriteReportPage;
    function ReasonOf(const aName: String): String;
    procedure Say(const aLine: String);
    function SuiteDir(const aPart: String): String;
    function CollectTests: TStringList;
    function ReferenceFor(const aName: String): String;
    function RenderDocument(const aFileName: String;
      aWidth, aHeight: Integer): TFPMemoryImage;
    function RunTest(const aName: String): TSVGDiffOutcome;
    function RenderWithoutReference(const aName: String): TSVGDiffOutcome;
    procedure Report;
  protected
    procedure DoRun; override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp;
  end;

// Reads a number as the rest of the package does, ignoring the locale.
function NumberDef(const aText: String; aDefault: Double): Double;

var
  lValue: Double;
  lCode: Integer;

begin
  Val(Trim(aText), lValue, lCode);
  if lCode = 0 then
    Result := lValue
  else
    Result := aDefault;
end;


// The name of a file, without its directory or its extension.
function TestNameOf(const aFileName: String): String;

begin
  Result := ChangeFileExt(ExtractFileName(aFileName), '');
end;


{ TSVGVisitedList }

constructor TSVGVisitedList.Create(const aHRefs: array of String);

var
  I: Integer;

begin
  inherited Create;
  FHRefs := TStringList.Create;
  for I := Low(aHRefs) to High(aHRefs) do
    FHRefs.Add(Trim(aHRefs[I]));
end;


destructor TSVGVisitedList.Destroy;

begin
  FreeAndNil(FHRefs);
  inherited Destroy;
end;


function TSVGVisitedList.WasVisited(const aHRef, aBaseURI: String): Boolean;

var
  I: Integer;

begin
  if aBaseURI = '' then ;
  Result := False;
  for I := 0 to FHRefs.Count - 1 do
    if SameText(FHRefs[I], Trim(aHRef)) then
      Exit(True);
end;


{ TSVGDiffApplication }

constructor TSVGDiffApplication.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  StopOnException := True;
  FRoot := DefaultRoot;
  FSuite := DefaultSuite;
  FOutDir := DefaultOutput;
  FTolerance := DefaultTolerance;
  FSubSamples := SVGSubSamples;
  FThreshold := DefaultThreshold;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FBackground := TSVGColor.FromBytes(255, 255, 255, 255);
end;


destructor TSVGDiffApplication.Destroy;

begin
  FImages := nil;
  FFontFiles := nil;
  FreeAndNil(FDocuments);
  FreeAndNil(FStyleSheets);
  FFonts := nil;
  FreeAndNil(FFreeType);
{$IFDEF DARWIN}
  FreeAndNil(FCoreText);
{$ENDIF}
{$IFDEF WINDOWS}
  FreeAndNil(FGDI);
{$ENDIF}
  inherited Destroy;
end;


procedure TSVGDiffApplication.Say(const aLine: String);

begin
  if not FQuiet then
    WriteLn(aLine);
end;


procedure TSVGDiffApplication.WriteHelp;

begin
  WriteLn('svgdiff renders a suite of SVG documents and compares each one');
  WriteLn('against its reference image.');
  WriteLn;
  WriteLn('  --suite=NAME     suite under the root directory (default ',
    DefaultSuite, ')');
  WriteLn('  --root=DIR       directory holding the suites (default ',
    DefaultRoot, ')');
  WriteLn('  --svg=DIR        documents to render, overriding the suite');
  WriteLn('  --ref=DIR        reference images, overriding the suite');
  WriteLn('  --out=DIR        where images are written (default ',
    DefaultOutput, ')');
  WriteLn('  --tolerance=N    channel difference still counted equal, 0..255');
  WriteLn('  --sub-samples=N  number of lines across a row of pixels that');
  WriteLn('                   a shape is measured on (default ', SVGSubSamples,
    '). The coverage');
  WriteLn('                   of its edges comes from them, so more lines');
  WriteLn('                   follow a shape more closely and take longer');
  WriteLn('  --threshold=P    percentage of differing pixels allowed');
  WriteLn('  --filter=TEXT    only run tests whose name contains TEXT');
  WriteLn('  --fonts=DIR      add a directory of fonts; repeatable');
  WriteLn('  --font-engine=E  draw text with freetype, with coretext on');
  WriteLn('                   macOS or gdi on Windows; the platform');
  WriteLn('                   default otherwise');
  WriteLn('  --visited=HREF   count a link to HREF as one that was');
  WriteLn('                   followed, so that :visited reaches it;');
  WriteLn('                   repeatable. Some documents ask the operator');
  WriteLn('                   to follow a link before the reference image');
  WriteLn('                   is correct, and this states it was followed');
  WriteLn('  --paint-sample=WHERE  where in a pixel a paint server is');
  WriteLn('                   read: middle, the centre of the pixel, which');
  WriteLn('                   is what SVG means; or corner, its top left,');
  WriteLn('                   which is how the references were drawn. Only');
  WriteLn('                   gradients are read this way');
  WriteLn('  --rendered=DIR   compare PNGs another renderer left in DIR');
  WriteLn('                   instead of drawing the documents here');
  WriteLn('  --write-diff     write a difference map for each failing test');
  WriteLn('  --write-distance write, for every test that was measured, a');
  WriteLn('                   grey map of how far it is from its reference:');
  WriteLn('                   white where the two agree, black where they');
  WriteLn('                   differ most. Turned on by --html, which puts');
  WriteLn('                   it in the page');
  WriteLn('  --write-render   write the rendering of every test');
  WriteLn('  --html=FILE      write a page showing every test beside its');
  WriteLn('                   reference; turns on --write-render');
  WriteLn('  --save-reference render a test that has no reference and keep');
  WriteLn('                   the result as one. It catches later change');
  WriteLn('                   only, not a wrong first rendering');
  WriteLn('  --width=N        size used when there is no reference to take');
  WriteLn('  --height=N       it from (default ', DefaultWidth, ' by ',
    DefaultHeight, ')');
  WriteLn('  --mask-boilerplate  when a document has a draft');
  WriteLn('                   watermark, paint the band it covers over both');
  WriteLn('                   images before comparing them, so that neither');
  WriteLn('                   is scored on it. Also spelled');
  WriteLn('                   --mask-watermark. The revision line is left');
  WriteLn('                   alone and counted instead: see the report');
  WriteLn('  --no-watermark   drop the draft watermark of a document');
  WriteLn('                   before rendering it. Nineteen documents of');
  WriteLn('                   the W3C suite have one that their reference');
  WriteLn('                   image predates, and the bar covers a twentieth');
  WriteLn('                   of the picture');
  WriteLn('  --in-scope       leave out the documents that rest on a');
  WriteLn('                   non-goal of SPEC.md: SMIL, script and');
  WriteLn('                   interactivity, foreignObject, and the writing');
  WriteLn('                   systems that need shaping. Two documents');
  WriteLn('                   whose reference image is of the file their');
  WriteLn('                   link leads to are left out as well');
  WriteLn('  --kern-faces     set the pairs of a system face closer, from');
  WriteLn('                   its kern table. Off by default: the reference');
  WriteLn('                   images were drawn without it, and it makes');
  WriteLn('                   eleven documents worse. A face the document');
  WriteLn('                   declares is kerned in either case, from its');
  WriteLn('                   own hkern');
  WriteLn('  --mask-frame     paint out the frame of the test template');
  WriteLn('                   in both pictures. The reference renderer put');
  WriteLn('                   its thin strokes on whole pixels while this');
  WriteLn('                   one puts them where the geometry says, so the');
  WriteLn('                   frame differs on every document. Off by');
  WriteLn('                   default: it is counted and reported instead');
  WriteLn('  --list           list the tests that would run, then stop');
  WriteLn('  --quiet          print the summary alone');
  WriteLn('  --help           this text');
  WriteLn;
  WriteLn('Exit status is 0 when every test passed, 1 when one did not,');
  WriteLn('and 2 when the suite could not be read.');
end;


procedure TSVGDiffApplication.ReadOptions;

var
  lPaths: TStringArray;
  I: Integer;

begin
  if HasOption('root') then
    FRoot := GetOptionValue('root');
  if HasOption('suite') then
    FSuite := GetOptionValue('suite');
  if HasOption('svg') then
    FSVGDir := GetOptionValue('svg');
  if HasOption('ref') then
    FRefDir := GetOptionValue('ref');
  if HasOption('out') then
    FOutDir := GetOptionValue('out');
  if HasOption('filter') then
    FFilter := GetOptionValue('filter');
  if HasOption('tolerance') then
    FTolerance := StrToIntDef(GetOptionValue('tolerance'), DefaultTolerance);
  if HasOption('sub-samples') then
    FSubSamples := StrToIntDef(GetOptionValue('sub-samples'),
      SVGSubSamples);
  if HasOption('threshold') then
    FThreshold := NumberDef(GetOptionValue('threshold'), DefaultThreshold);
  FWriteDiff := HasOption('write-diff');
  if HasOption('html') then
    FHtml := GetOptionValue('html');
  // The page shows the rendering of every test, so every test must have
  // written one.
  FWriteRender := HasOption('write-render') or (FHtml <> '');
  FWriteDistance := HasOption('write-distance') or (FHtml <> '');
  FSaveReference := HasOption('save-reference');
  if HasOption('width') then
    FWidth := StrToIntDef(GetOptionValue('width'), DefaultWidth);
  if HasOption('height') then
    FHeight := StrToIntDef(GetOptionValue('height'), DefaultHeight);
  FQuiet := HasOption('quiet');
  FList := HasOption('list');
  FInScope := HasOption('in-scope');
  if HasOption('rendered') then
    FRendered := GetOptionValue('rendered');
  if SameText(GetOptionValue('paint-sample'), 'corner') then
    FPaintSample := psCorner;
  FNoWatermark := HasOption('no-watermark');
  FMaskFrame := HasOption('mask-frame');
  FMaskWatermark := HasOption('mask-boilerplate')
    or HasOption('mask-watermark');
  if FSVGDir = '' then
    FSVGDir := SuiteDir('svg');
  if FRefDir = '' then
    FRefDir := SuiteDir('png');
  FEngine := LowerCase(Trim(GetOptionValue('font-engine')));
  if FEngine = '' then
    FEngine := LowerCase(SVGPlatformFontEngine);
  if FEngine = 'core text' then
    FEngine := 'coretext';
  if (FEngine <> 'freetype') and (FEngine <> 'coretext')
     and (FEngine <> 'gdi') then
    begin
    WriteLn('svgdiff: --font-engine takes freetype, coretext or gdi.');
    Halt(2);
    end;
{$IFNDEF DARWIN}
  if FEngine = 'coretext' then
    begin
    WriteLn('svgdiff: Core Text is on macOS alone.');
    Halt(2);
    end;
{$ENDIF}
{$IFNDEF WINDOWS}
  if FEngine = 'gdi' then
    begin
    WriteLn('svgdiff: GDI is on Windows alone.');
    Halt(2);
    end;
{$ENDIF}
  lPaths := GetOptionValues(#0, 'fonts');
{$IFDEF DARWIN}
  if FEngine = 'coretext' then
    begin
    FCoreText := TSVGCoreTextProvider.Create;
    FFonts := FCoreText;
    for I := 0 to High(lPaths) do
      FCoreText.AddFontPath(lPaths[I]);
    FCoreText.AddFontPath(IncludeTrailingPathDelimiter(FSVGDir) + 'woffs');
    FCoreText.AddFontPath(ExtractFilePath(
      ExcludeTrailingPathDelimiter(FSVGDir)) + 'resources');
    end;
{$ENDIF}
{$IFDEF WINDOWS}
  if FEngine = 'gdi' then
    begin
    FGDI := TSVGGDIProvider.Create;
    FFonts := FGDI;
    FGDI.Kerning := HasOption('kern-faces');
    for I := 0 to High(lPaths) do
      FGDI.AddFontPath(lPaths[I]);
    FGDI.AddFontPath(IncludeTrailingPathDelimiter(FSVGDir) + 'woffs');
    FGDI.AddFontPath(ExtractFilePath(
      ExcludeTrailingPathDelimiter(FSVGDir)) + 'resources');
    end;
{$ENDIF}
  if FEngine = 'freetype' then
    begin
    FFreeType := TSVGFreeTypeProvider.Create;
    FFonts := FFreeType;
    FFreeType.Kerning := HasOption('kern-faces');
    for I := 0 to High(lPaths) do
      FFreeType.AddFontPath(lPaths[I]);
    if Length(lPaths) = 0 then
      FFreeType.AddSystemFonts;
    // The suite ships the faces its documents ask for next to them, and a
    // document that expects one to be installed has no other way of
    // reaching it. They are read whatever the machine holds.
    FFreeType.AddFontPath(IncludeTrailingPathDelimiter(FSVGDir) + 'woffs');
    FFreeType.AddFontPath(ExtractFilePath(
      ExcludeTrailingPathDelimiter(FSVGDir)) + 'resources');
    FFreeType.Coverage := SVGPlatformCoverage;
    end;
  lPaths := GetOptionValues(#0, 'visited');
  if Length(lPaths) > 0 then
    FVisited := TSVGVisitedList.Create(lPaths);
  FImages := TSVGFileImageResolver.Create(FSVGDir);
  FDocuments := TSVGFileDocumentResolver.Create(FSVGDir);
  FStyleSheets := TSVGFileStyleSheetResolver.Create(FSVGDir);
  FFontFiles := TSVGFileFontResolver.Create(FSVGDir);
end;


function TSVGDiffApplication.SuiteDir(const aPart: String): String;

begin
  Result := IncludeTrailingPathDelimiter(FRoot)
    + IncludeTrailingPathDelimiter(FSuite) + aPart;
end;


// True when the text holds any of aWords, which are matched in lower
// case.
function HoldsAny(const aText: String; const aWords: array of String): Boolean;

var
  I: Integer;

begin
  Result := True;
  for I := 0 to High(aWords) do
    if Pos(aWords[I], aText) > 0 then
      Exit;
  Result := False;
end;


// True when the text is written in a script that has to be shaped to be
// read: the bidirectional ones, and the Indic ones that reorder.
function NeedsShaping(const aText: String): Boolean;

var
  I: Integer;
  lCode: Cardinal;

begin
  Result := True;
  I := 1;
  while I <= Length(aText) do
    begin
    lCode := SVGNextCodePoint(aText, I);
    if ((lCode >= $0590) and (lCode <= $08FF))
       or ((lCode >= $0900) and (lCode <= $0DFF))
       or ((lCode >= $1780) and (lCode <= $17FF)) then
      Exit;
    end;
  Result := False;
end;


// The non-goal a document rests on, or an empty string when it rests on
// none of them. Text inside a comment does not count: every document of
// the W3C suite has a watermark that is commented out.
function NonGoalOf(const aFileName: String): String;

const
  { Documents whose reference image is of the page their link leads to,
    photographed after an operator followed it. Nothing a renderer draws
    from the document itself can match one. }
  LinkFollowed: array[0..1] of String = (
    'linking-a-01-b', 'linking-a-03-b');

var
  lText: String;
  lStart, lStop: Integer;

begin
  Result := '';
  for lStart := Low(LinkFollowed) to High(LinkFollowed) do
    if TestNameOf(aFileName) = LinkFollowed[lStart] then
      Exit('the reference is of the link destination');
  with TStringList.Create do
    try
      try
        LoadFromFile(aFileName);
        lText := Text;
      except
        Exit;
      end;
    finally
      Free;
    end;
  repeat
    lStart := Pos('<!--', lText);
    if lStart = 0 then
      Break;
    lStop := PosEx('-->', lText, lStart);
    if lStop = 0 then
      lStop := Length(lText)
    else
      Inc(lStop, 2);
    Delete(lText, lStart, lStop - lStart + 1);
  until False;
  if NeedsShaping(lText) then
    Exit('shaping');
  // A tab or a newline separates as a space does, and an attribute is as
  // often written after one as after the other.
  lText := LowerCase(lText);
  for lStart := 1 to Length(lText) do
    if lText[lStart] in [#9, #10, #13] then
      lText[lStart] := ' ';
  if HoldsAny(lText, ['<animate', '<set ', '<set/', '<set>',
                      'animatemotion', 'animatetransform', 'animatecolor']) then
    Exit('SMIL');
  if HoldsAny(lText, ['<script', ' onload=', ' onclick=', ' onmouse',
                      ' onfocus', ' onactivate', ' onbegin=', ' onend=',
                      ' onrepeat=']) then
    Exit('script');
  if Pos('foreignobject', lText) > 0 then
    Exit('foreignObject');
end;


function TSVGDiffApplication.CollectTests: TStringList;

var
  lSearch: TSearchRec;
  lPath, lName, lReason: String;

begin
  Result := TStringList.Create;
  Result.Sorted := True;
  lPath := IncludeTrailingPathDelimiter(FSVGDir);
  if FindFirst(lPath + '*.svg', faAnyFile, lSearch) <> 0 then
    Exit;
  try
    repeat
      if (lSearch.Attr and faDirectory) <> 0 then
        Continue;
      lName := TestNameOf(lSearch.Name);
      if (FFilter <> '')
         and (Pos(LowerCase(FFilter), LowerCase(lName)) = 0) then
        Continue;
      if FInScope then
        begin
        lReason := NonGoalOf(lPath + lSearch.Name);
        if lReason <> '' then
          begin
          Say(Format('  skip %s: %s', [lName, lReason]));
          Inc(FLeftOut);
          Continue;
          end;
        end;
      Result.Add(lName);
    until FindNext(lSearch) <> 0;
  finally
    FindClose(lSearch);
  end;
end;


function TSVGDiffApplication.ReferenceFor(const aName: String): String;

begin
  Result := IncludeTrailingPathDelimiter(FRefDir) + aName + '.png';
  if not FileExists(Result) then
    Result := IncludeTrailingPathDelimiter(FRefDir) + 'full-' + aName + '.png';
  if not FileExists(Result) then
    Result := '';
end;


// The box the revision line of the test template covers once the document
// is drawn aWidth by aHeight. False when the document has no revision
// line. The width of the text is not known, so this is the room the
// template leaves for it and not the area a renderer really drew.
function TSVGDiffApplication.RevisionBox(aDocument: TSVGDocument;
  aRenderer: TSVGRenderer; aWidth, aHeight: Integer;
  out aLeft, aTop, aRight, aBottom: Integer): Boolean;

var
  lText, lNode: TSVGElement;
  lY, lSize: TSVGLength;
  lWide, lHigh: Integer;
  lScale: Double;

begin
  Result := False;
  lText := aDocument.ElementByID('revision');
  if lText = nil then
    Exit;
  if not lY.TryParse(lText.AttributeDef('y', '0')) then
    Exit;
  // font-size is as often on the group above the text as on the text.
  lSize := TSVGLength.Create(32, luNumber);
  lNode := lText;
  while lNode <> nil do
    begin
    if lNode.HasAttribute('font-size')
       and lSize.TryParse(lNode.Attributes['font-size']) then
      Break;
    lNode := lNode.Parent;
    end;
  if not aRenderer.DocumentSize(aDocument, lWide, lHigh) or (lHigh <= 0) then
    Exit;
  lScale := aHeight / lHigh;
  aLeft := 0;
  aTop := Floor((lY.Value - lSize.Value * 1.1) * lScale);
  // Twenty characters of the size, which holds the longest revision the
  // template writes with room to spare.
  aRight := Ceil((10 + lSize.Value * 20 * 0.62) * lScale);
  aBottom := Ceil((lY.Value + lSize.Value * 0.35) * lScale);
  if aRight > aWidth then
    aRight := aWidth;
  Result := (aRight > aLeft) and (aBottom > aTop);
end;


// The band that the frame of the test template covers once the document
// is drawn. The frame is a rectangle a unit in from the edge, and its
// stroke straddles that rectangle, so the band runs a margin either side
// of it. False when the document has no frame.
function TSVGDiffApplication.FrameBand(aDocument: TSVGDocument;
  aRenderer: TSVGRenderer; aWidth, aHeight: Integer): Boolean;

var
  lRect: TSVGElement;
  lX, lY, lW, lH, lPen: TSVGLength;
  lNode: TSVGElement;
  lWide, lHigh: Integer;
  lScaleX, lScaleY: Double;

begin
  Result := False;
  lRect := aDocument.ElementByID('test-frame');
  if lRect = nil then
    Exit;
  if not lX.TryParse(lRect.AttributeDef('x', '0'))
     or not lY.TryParse(lRect.AttributeDef('y', '0'))
     or not lW.TryParse(lRect.AttributeDef('width', '0'))
     or not lH.TryParse(lRect.AttributeDef('height', '0')) then
    Exit;
  lPen := TSVGLength.Create(1, luNumber);
  lNode := lRect;
  while lNode <> nil do
    begin
    if lNode.HasAttribute('stroke-width')
       and lPen.TryParse(lNode.Attributes['stroke-width']) then
      Break;
    lNode := lNode.Parent;
    end;
  if not aRenderer.DocumentSize(aDocument, lWide, lHigh)
     or (lWide <= 0) or (lHigh <= 0) then
    Exit;
  lScaleX := aWidth / lWide;
  lScaleY := aHeight / lHigh;
  FFrameLeft := Round(lX.Value * lScaleX);
  FFrameTop := Round(lY.Value * lScaleY);
  FFrameRight := Round((lX.Value + lW.Value) * lScaleX);
  FFrameBottom := Round((lY.Value + lH.Value) * lScaleY);
  // Half the stroke reaches either side of the rectangle, and one more
  // pixel covers the softened edge.
  FFrameMargin := Ceil(lPen.Value * Max(lScaleX, lScaleY) / 2) + 1;
  Result := (FFrameRight - FFrameLeft > 2 * FFrameMargin)
        and (FFrameBottom - FFrameTop > 2 * FFrameMargin);
end;


// How many pixels of the frame band differ. That is the number of
// differing pixels inside the outer rectangle of the band, minus the
// number inside its inner rectangle.
function TSVGDiffApplication.CountBorder(
  aActual, aExpected: TFPMemoryImage): Integer;

begin
  Result := SVGCountDiffering(aActual, aExpected, FTolerance, FBackground,
              FFrameLeft - FFrameMargin, FFrameTop - FFrameMargin,
              FFrameRight + FFrameMargin, FFrameBottom + FFrameMargin)
          - SVGCountDiffering(aActual, aExpected, FTolerance, FBackground,
              FFrameLeft + FFrameMargin, FFrameTop + FFrameMargin,
              FFrameRight - FFrameMargin, FFrameBottom - FFrameMargin);
end;


// Paints the frame band out of one picture: everything inside the outer
// rectangle and outside the inner one.
procedure BlankBand(aImage: TFPMemoryImage;
  aOutLeft, aOutTop, aOutRight, aOutBottom,
  aInLeft, aInTop, aInRight, aInBottom: Integer);

var
  X, Y: Integer;

begin
  for Y := Max(0, aOutTop) to Min(aImage.Height - 1, aOutBottom) do
    for X := Max(0, aOutLeft) to Min(aImage.Width - 1, aOutRight) do
      if (X < aInLeft) or (X > aInRight)
         or (Y < aInTop) or (Y > aInBottom) then
        aImage.Colors[X, Y] := colWhite;
end;


// Paints the frame band out of both pictures, so that neither is scored
// on the position of a line half a pixel wide.
procedure MaskFrame(aLeft, aRight: TFPMemoryImage;
  aOutLeft, aOutTop, aOutRight, aOutBottom,
  aInLeft, aInTop, aInRight, aInBottom: Integer);

begin
  BlankBand(aLeft, aOutLeft, aOutTop, aOutRight, aOutBottom,
    aInLeft, aInTop, aInRight, aInBottom);
  BlankBand(aRight, aOutLeft, aOutTop, aOutRight, aOutBottom,
    aInLeft, aInTop, aInRight, aInBottom);
end;


// The rows a draft watermark covers once the document is drawn aHeight
// tall, or zero when the document has none. The band starts at the
// top: the watermark of the W3C template begins a pixel down from it.
function TSVGDiffApplication.WatermarkBand(aDocument: TSVGDocument;
  aRenderer: TSVGRenderer; aHeight: Integer): Integer;

var
  lMark, lRect: TSVGElement;
  lY, lTall: TSVGLength;
  I, lWide, lHigh: Integer;

begin
  Result := 0;
  lMark := aDocument.ElementByID('draft-watermark');
  if lMark = nil then
    Exit;
  lRect := nil;
  for I := 0 to lMark.ChildCount - 1 do
    if lMark[I] is TSVGRectElement then
      begin
      lRect := TSVGElement(lMark[I]);
      Break;
      end;
  if lRect = nil then
    Exit;
  if not lY.TryParse(lRect.AttributeDef('y', '0'))
     or not lTall.TryParse(lRect.AttributeDef('height', '0')) then
    Exit;
  if not aRenderer.DocumentSize(aDocument, lWide, lHigh) or (lHigh <= 0) then
    Exit;
  // One pixel below the bottom edge for the stroke, and one more for the
  // softened edge of it.
  Result := Ceil((lY.Value + lTall.Value + 2) * aHeight / lHigh);
  if Result > aHeight then
    Result := aHeight;
end;


// Paints the top aRows of both images alike, so that everything drawn
// there counts as equal.
procedure MaskBand(aLeft, aRight: TFPMemoryImage; aRows: Integer);

var
  X, Y: Integer;
  lFlat: TFPColor;

begin
  lFlat := colWhite;
  for Y := 0 to aRows - 1 do
    begin
    if Y < aLeft.Height then
      for X := 0 to aLeft.Width - 1 do
        aLeft.Colors[X, Y] := lFlat;
    if Y < aRight.Height then
      for X := 0 to aRight.Width - 1 do
        aRight.Colors[X, Y] := lFlat;
    end;
end;


// Takes the draft watermark out of a document. A test of the W3C suite
// that was never approved has a red bar across its head, which its
// reference image may or may not have been made with.
procedure TSVGDiffApplication.DropWatermark(aDocument: TSVGDocument);

var
  lMark: TSVGElement;

begin
  lMark := aDocument.ElementByID('draft-watermark');
  if (lMark = nil) or (lMark.Parent = nil) then
    Exit;
  lMark.Parent.RemoveChild(lMark);
  Inc(FUnmarked);
end;


function TSVGDiffApplication.RenderDocument(const aFileName: String;
  aWidth, aHeight: Integer): TFPMemoryImage;

var
  lDocument: TSVGDocument;
  lRenderer: TSVGRenderer;
  lBackend: TSVGSoftBackend;

begin
  Result := nil;
  lDocument := nil;
  lRenderer := nil;
  lBackend := nil;
  try
    lDocument := ReadSVGFile(aFileName);
    lDocument.BaseURI := aFileName;
    lBackend := TSVGSoftBackend.Create;
    lBackend.PaintSample := FPaintSample;
    lBackend.SubSamples := FSubSamples;
    lRenderer := TSVGRenderer.Create;
    FBand := 0;
    if FMaskWatermark then
      FBand := WatermarkBand(lDocument, lRenderer, aHeight);
    FHasFrame := FrameBand(lDocument, lRenderer, aWidth, aHeight);
    FHasRevision := RevisionBox(lDocument, lRenderer, aWidth, aHeight,
      FRevLeft, FRevTop, FRevRight, FRevBottom);
    // The two are counted apart and then added up, so the revision box
    // is held clear of the band the frame is counted over.
    if FHasRevision and FHasFrame then
      begin
      FRevLeft := Max(FRevLeft, FFrameLeft + FFrameMargin);
      FRevTop := Max(FRevTop, FFrameTop + FFrameMargin);
      FRevRight := Min(FRevRight, FFrameRight - FFrameMargin);
      FRevBottom := Min(FRevBottom, FFrameBottom - FFrameMargin);
      FHasRevision := (FRevRight > FRevLeft) and (FRevBottom > FRevTop);
      end;
    if FNoWatermark then
      DropWatermark(lDocument);
    lRenderer.Fonts := FFonts;
    lRenderer.LinkHistory := FVisited;
    lRenderer.Images := FImages;
    lRenderer.Documents := FDocuments;
    lRenderer.StyleSheets := FStyleSheets;
    lRenderer.FontFiles := FFontFiles;
    lRenderer.RenderToSize(lDocument, lBackend, aWidth, aHeight);
    Result := TFPMemoryImage.Create(aWidth, aHeight);
    Result.Assign(lBackend.Image);
  finally
    lRenderer.Free;
    lBackend.Free;
    lDocument.Free;
  end;
end;


function TSVGDiffApplication.RenderWithoutReference(
  const aName: String): TSVGDiffOutcome;

var
  lActual: TFPMemoryImage;
  lWriter: TFPWriterPNG;
  lTarget: String;

begin
  Result := doNoReference;
  lActual := nil;
  lWriter := nil;
  try
    try
      lActual := RenderDocument(
        IncludeTrailingPathDelimiter(FSVGDir) + aName + '.svg',
        FWidth, FHeight);
      lWriter := TFPWriterPNG.Create;
      lWriter.UseAlpha := True;
      lWriter.WordSized := False;
      if FSaveReference then
        lTarget := IncludeTrailingPathDelimiter(FRefDir) + aName + '.png'
      else
        lTarget := IncludeTrailingPathDelimiter(FOutDir) + aName + '.png';
      ForceDirectories(ExtractFileDir(lTarget));
      lActual.SaveToFile(lTarget, lWriter);
      Say(Format('  none %s: rendered to %s', [aName, lTarget]));
    except
      on E: Exception do
        begin
        WriteLn(Format('  ERR  %s: %s', [aName, E.Message]));
        Result := doError;
        end;
    end;
  finally
    lWriter.Free;
    lActual.Free;
  end;
end;


function TSVGDiffApplication.RunTest(const aName: String): TSVGDiffOutcome;

var
  lReference: String;
  lExpected, lActual, lDiff, lFar: TFPMemoryImage;
  lComparison: TSVGComparison;
  lWriter: TFPWriterPNG;
  lRevision, lBorder: Double;

begin
  Result := doError;
  lReference := ReferenceFor(aName);
  if lReference = '' then
    begin
    if FWriteRender or FSaveReference then
      begin
      Result := RenderWithoutReference(aName);
      AddRow(aName, -1, -1, -1, Result);
      Exit;
      end;
    Say(Format('  none %s: no reference image', [aName]));
    AddRow(aName, -1, -1, -1, doNoReference);
    Exit(doNoReference);
    end;
  lExpected := nil;
  lActual := nil;
  lDiff := nil;
  lFar := nil;
  lWriter := nil;
  try
    try
      lExpected := TFPMemoryImage.Create(0, 0);
      lExpected.LoadFromFile(lReference);
      // The output of another renderer is measured by loading the file it
      // wrote, instead of drawing the document here. Everything after
      // this point is the same in both cases.
      if FRendered <> '' then
        begin
        lActual := TFPMemoryImage.Create(0, 0);
        lActual.LoadFromFile(
          IncludeTrailingPathDelimiter(FRendered) + aName + '.png');
        end
      else
        lActual := RenderDocument(
          IncludeTrailingPathDelimiter(FSVGDir) + aName + '.svg',
          lExpected.Width, lExpected.Height);
      if FWriteDiff then
        lDiff := TFPMemoryImage.Create(lExpected.Width, lExpected.Height);
      if FBand > 0 then
        begin
        MaskBand(lActual, lExpected, FBand);
        Inc(FMasked);
        end;
      if FMaskFrame and FHasFrame then
        MaskFrame(lActual, lExpected,
          FFrameLeft - FFrameMargin, FFrameTop - FFrameMargin,
          FFrameRight + FFrameMargin, FFrameBottom + FFrameMargin,
          FFrameLeft + FFrameMargin, FFrameTop + FFrameMargin,
          FFrameRight - FFrameMargin, FFrameBottom - FFrameMargin);
      lComparison := SVGCompareImages(lActual, lExpected, FTolerance,
        FBackground, lDiff);
      if FWriteDistance and (lComparison.Outcome <> crSizeMismatch) then
        begin
        lFar := TFPMemoryImage.Create(lExpected.Width, lExpected.Height);
        SVGDistanceImage(lActual, lExpected, FBackground, lFar);
        end;
      // The revision line differs on nearly every document, both in its
      // text and in the face it is set in. It is counted, never masked.
      lRevision := 0;
      lBorder := 0;
      if lComparison.Pixels > 0 then
        begin
        if FHasRevision then
          begin
          lRevision := SVGCountDiffering(lActual, lExpected, FTolerance,
            FBackground, FRevLeft, FRevTop, FRevRight, FRevBottom)
            * 100.0 / lComparison.Pixels;
          FRevPixels := FRevPixels
            + Round(lRevision * lComparison.Pixels / 100);
          end;
        if FHasFrame and not FMaskFrame then
          begin
          lBorder := CountBorder(lActual, lExpected)
            * 100.0 / lComparison.Pixels;
          FBorderPixels := FBorderPixels
            + Round(lBorder * lComparison.Pixels / 100);
          end;
        FDiffPixels := FDiffPixels + lComparison.Differing;
        end;
      if lComparison.Passes(FThreshold) then
        Result := doPass
      else
        Result := doFail;
      AddRow(aName, lComparison.Percentage, lRevision, lBorder, Result);
      if Result = doPass then
        Say(Format('  ok   %s: %s', [aName, lComparison.ToString]))
      else
        WriteLn(Format('  FAIL %s: %s', [aName, lComparison.ToString]));
      // A file that cannot be written is worth reporting, but it is not
      // the result of the comparison.
      try
        lWriter := TFPWriterPNG.Create;
        lWriter.UseAlpha := True;
        lWriter.WordSized := False;
        if FWriteRender or (Result = doFail) then
          lActual.SaveToFile(IncludeTrailingPathDelimiter(FOutDir)
            + aName + '.png', lWriter);
        if (lDiff <> nil) and (Result = doFail) then
          lDiff.SaveToFile(IncludeTrailingPathDelimiter(FOutDir)
            + aName + '-diff.png', lWriter);
        if lFar <> nil then
          lFar.SaveToFile(IncludeTrailingPathDelimiter(FOutDir)
            + aName + '-distance.png', lWriter);
      except
        on E: Exception do
          WriteLn(Format('  warn %s: %s', [aName, E.Message]));
      end;
    except
      on E: Exception do
        begin
        WriteLn(Format('  ERR  %s: %s', [aName, E.Message]));
        Result := doError;
        AddRow(aName, -1, -1, -1, doError);
        end;
    end;
  finally
    lWriter.Free;
    lDiff.Free;
    lFar.Free;
    lActual.Free;
    lExpected.Free;
  end;
end;


procedure TSVGDiffApplication.AddRow(const aName: String;
  aPercentage, aRevision, aBorder: Double; aOutcome: TSVGDiffOutcome);

begin
  if FRowCount = Length(FRows) then
    SetLength(FRows, 64 + FRowCount * 2);
  FRows[FRowCount].Name := aName;
  FRows[FRowCount].Percentage := aPercentage;
  FRows[FRowCount].Revision := aRevision;
  FRows[FRowCount].Border := aBorder;
  FRows[FRowCount].Outcome := aOutcome;
  Inc(FRowCount);
end;


// Puts the rows in front of the reader worst first, which is the order
// they are worth looking at in. A test with nothing to compare goes last.
procedure TSVGDiffApplication.SortRows;

var
  I, J: Integer;
  lRow: TSVGDiffRow;

begin
  for I := 1 to FRowCount - 1 do
    begin
    lRow := FRows[I];
    J := I;
    while (J > 0) and (FRows[J - 1].Percentage < lRow.Percentage) do
      begin
      FRows[J] := FRows[J - 1];
      Dec(J);
      end;
    FRows[J] := lRow;
    end;
end;


// The mean of the measured rows and the middle one, which the few worst
// documents do not pull the way they pull the mean. Skipping the
// explained ones leaves out every row with a reason. The rows are
// worst first by the time this is asked.
procedure TSVGDiffApplication.Summarise(aSkipExplained: Boolean;
  out aMean, aMedian: Double; out aCount: Integer);

var
  I: Integer;
  lTotal: Double;
  lKept: array of Double;

begin
  aMean := 0;
  aMedian := 0;
  aCount := 0;
  lTotal := 0;
  SetLength(lKept, FRowCount);
  for I := 0 to FRowCount - 1 do
    if (FRows[I].Percentage >= 0)
       and not (aSkipExplained and (ReasonOf(FRows[I].Name) <> '')) then
      begin
      lKept[aCount] := FRows[I].Percentage;
      lTotal := lTotal + FRows[I].Percentage;
      Inc(aCount);
      end;
  if aCount = 0 then
    Exit;
  aMean := lTotal / aCount;
  if Odd(aCount) then
    aMedian := lKept[aCount div 2]
  else
    aMedian := (lKept[aCount div 2 - 1] + lKept[aCount div 2]) / 2;
end;


// The text with the four characters that cannot stand in a page spelled
// out.
function HtmlText(const aText: String): String;

begin
  Result := StringReplace(aText, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;


// The path of aFileName as the page has to write it: relative to the page
// itself, with forward slashes.
function PageLink(const aBase, aFileName: String): String;

begin
  Result := ExtractRelativePath(aBase, ExpandFileName(aFileName));
  Result := StringReplace(Result, '\', '/', [rfReplaceAll]);
  Result := HtmlText(Result);
end;


// One screenshot, which opens at its own size when it is clicked.
function Shot(const aLink, aText: String): String;

begin
  Result := '<a href="' + aLink + '"><img loading="lazy" alt="'
    + aText + '" src="' + aLink + '"></a>';
end;


// The document itself, for the browser to draw beside the two pictures.
// The size is written out because a root sized in percentages has none of
// its own, and without it the box would not match the images beside it.
function Page(const aLink, aText: String;
  aWidth, aHeight: Integer): String;

begin
  Result := Format('<a href="%s"><img loading="lazy" alt="%s" src="%s"'
    + ' width="%d" height="%d"></a>', [aLink, aText, aLink,
    aWidth, aHeight]);
end;


// Why a document cannot match its reference however it is drawn, or an
// empty string when nothing is recorded. TODO.md holds the full
// reasoning; these are the short forms of it.
function KnownReason(const aName: String): String;

const
  Recorded: array[0..25, 0..1] of String = (
    ('linking-a-01-b', 'the reference is of the file the link leads to'),
    ('linking-a-03-b', 'the reference is of the file the link leads to'),
    ('struct-cond-03-t',
     'drawn right: the reference records a viewer that had the SVG DOM'),
    ('struct-image-13-f',
     'drawn right: the reference is 480 by 361 while the document asks '
     + 'for 480 by 360, and its drawing sits one pixel to the left of the '
     + 'position the geometry gives. Crop the padding and move it back, '
     + 'and the figure falls to 1.32% and the worst channel from 255 to '
     + '59'),
    ('struct-image-14-f',
     'drawn right: the reference is 481 by 361 while the document asks '
     + 'for 480 by 360, and its drawing sits one pixel to the left of the '
     + 'position the geometry gives. Crop the padding and move it back, '
     + 'and the figure falls to 1.27% and the worst channel from 255 to '
     + '59'),
    ('struct-use-06-b',
     'drawn right: the reference is of another revision, and of a '
     + 'renderer that did not follow the references'),
    ('styling-pres-03-f',
     'drawn right: a style rule beats a presentation attribute, so the '
     + 'rectangle is green and no red shows, which is all the test asks '
     + 'for. The reference draws no rectangle at all, in green or in any '
     + 'other colour, and has nothing under its watermark but the '
     + 'frame and the revision line'),
    ('styling-pres-04-f',
     'drawn right: the reference draws none of the seven shapes'),
    ('styling-pres-05-f',
     'drawn right: the reference draws none of the seven shapes'),
    ('styling-css-09-f',
     'drawn right: the reference draws none of the seven shapes'),
    ('styling-css-10-f',
     'drawn right: four orange circles are asked for, and the reference '
     + 'is of another revision'),
    ('pservers-grad-13-b',
     'drawn right: every bar is stroked black half a pixel wide, so the '
     + 'ink lands inside a single pixel here and is spread over two in '
     + 'the reference; 69% of the difference lies along one of those '
     + 'edges, and the flat areas that remain differ by three to eight '
     + 'levels in no particular direction'),
    ('pservers-pattern-09-f',
     'drawn right: a pattern of no extent falls back to the colour '
     + 'beside it, which is what this test asks for and what the '
     + 'reference of pservers-pattern-03-f shows; this reference draws '
     + 'only the first of its two'),
    ('color-prop-04-t',
     'drawn right: it tests the CSS2 system colours, which take their '
     + 'values from the desktop and have none of their own; every shape '
     + 'and every letter is at the position the reference has it, and '
     + 'only the colours behind the names differ. AppWorkspace alone is '
     + '29.5% of the image, and six such colours 86% of the difference'),
    ('interact-zoom-01-t',
     'drawn right: the test asks the operator to magnify, and its '
     + 'reference is the document at four times the size; this draws it '
     + 'at the size it asks for, while the sibling that disables '
     + 'magnifying matches to the pixel'),
    ('interact-zoom-02-t',
     'drawn right: the test asks the operator to magnify, and its '
     + 'reference is the document at four times the size; this draws it '
     + 'at the size it asks for'),
    ('struct-cond-overview-05-f',
     'drawn right: the test says an element under a failing condition can '
     + 'still be drawn by a use that refers to it, and passes when no red '
     + 'shows; no red shows here, and the reference is three red '
     + 'squares'),
    ('styling-css-06-b',
     'drawn right for a still picture: the test exercises the five dynamic '
     + 'pseudo classes and says a static image cannot capture them, its '
     + 'reference simulating a visited link, a hovered one and selected '
     + 'text; only :link can be known here, and all three links are '
     + 'blue'),
    ('types-basic-02-f',
     'drawn right: the test asks for six circles in a thick green stroke '
     + 'and no red, and the reference shows red on three of them, having '
     + 'taken no stroke-width from a rule; five of the six are clean here'),
    ('text-text-11-t',
     'drawn right: it sets the fan in whatever the platform calls '
     + 'sans-serif, and says explicitly that the font may differ from the '
     + 'reference''s. Ours is Liberation Sans and the reference''s is '
     + 'about a tenth wider, so each line reaches the edge of the canvas '
     + 'a letter earlier there; cap height, baseline and anchoring all '
     + 'agree'),
    ('text-fonts-204-t',
     'drawn right: the document ships five weights of ZalamanderCaps and '
     + 'all five are used here, while the reference read neither the '
     + 'font-face rules, nor the family behind them, nor the variant'),
    ('filters-conv-04-f',
     'the convolution is right, and the difference is in the scaling of '
     + 'the photograph it works on: the kernel worked by hand reproduces '
     + 'our own unfiltered square to 1.1% of its pixels and the '
     + 'reference''s to 3.1%, and that square, which no filter touches, '
     + 'already differs on 41% of its pixels. Averaging the area a pixel '
     + 'covers, and reading an image at the corner as a paint is read, '
     + 'each cost the suite more than they gain here'),
    ('filters-overview-01-b',
     'drawn right: all six sources are read and the six squares show '
     + 'what the test asks for. The reference loses a few levels through '
     + 'its own blur, where a kernel adding up to one has to keep the '
     + 'value: the plane of a blue stroke at six tenths is 153 here, '
     + 'which is exactly right, and 147 there, and a flat run of the '
     + 'first square is 153 against 151. Most of the figure is those six '
     + 'levels over a whole plane'),
    ('filters-overview-02-b',
     'drawn right: the plane of FillPaint and StrokePaint has the '
     + 'gradient the element is painted with, blue to white to red to '
     + 'yellow, and the two squares match each other as the test says '
     + 'they must. The reference draws neither: a narrow band over plain '
     + 'blue in one and plain blue in the other, no yellow in either, and '
     + 'a browser agrees with this rendering. That column is 42% of the '
     + 'difference, and the rest is the reference losing a few levels '
     + 'through its own blur'),
    ('filters-overview-03-b',
     'drawn right: as filters-overview-02-b, the gradient plane read '
     + 'against the box of the element. The reference draws a band over '
     + 'plain purple in one square and plain purple in the other, while '
     + 'the test asks for the same gradient in both, and a browser agrees '
     + 'with this rendering'),
    ('filters-light-03-f',
     'drawn right: it tests how primitiveUnits resolves the z of a light, '
     + 'and the three columns are drawn alike here to the pixel, which is '
     + 'what it asks for. The document puts its light five below a '
     + 'surface that a surfaceScale of five raises five above zero, so '
     + 'the light is behind the surface it lights and the specular falls '
     + 'away towards it. The reference lit it from the other side of the '
     + 'surface: reverse the sign of z and the balls match it, with 18102 '
     + 'differing pixels falling to 2628. It is the only document of the '
     + 'suite that gives a light a negative z'));

var
  I: Integer;

begin
  Result := '';
  for I := Low(Recorded) to High(Recorded) do
    if Recorded[I, 0] = aName then
      Exit(Recorded[I, 1]);
end;


// The recorded reason for a document, or the non-goal it rests on.
// Empty when neither is known.
function TSVGDiffApplication.ReasonOf(const aName: String): String;

begin
  Result := KnownReason(aName);
  if Result <> '' then
    Exit;
  Result := NonGoalOf(IncludeTrailingPathDelimiter(FSVGDir)
    + aName + '.svg');
  if Result <> '' then
    Result := 'rests on ' + Result;
end;


procedure TSVGDiffApplication.WriteReportPage;

var
  lPage: TStringList;
  lBase, lName, lState, lReason: String;
  I, lCompared, lPlain: Integer;
  lRest, lMean, lMedian, lPlainMean, lPlainMedian: Double;

begin
  SortRows;
  lBase := IncludeTrailingPathDelimiter(
    ExpandFileName(ExtractFilePath(FHtml)));
  Summarise(False, lMean, lMedian, lCompared);
  Summarise(True, lPlainMean, lPlainMedian, lPlain);
  lPage := TStringList.Create;
  try
    lPage.Add('<!DOCTYPE html>');
    lPage.Add('<html lang="en"><head><meta charset="utf-8">');
    lPage.Add('<meta name="viewport" content="width=device-width,'
      + ' initial-scale=1">');
    lPage.Add('<title>' + HtmlText(FSuite) + ' renderings</title>');
    lPage.Add('<style>');
    lPage.Add(':root { color-scheme: light dark;');
    lPage.Add('  --ink: #1a1a1a; --dim: #666; --paper: #fff;'
      + ' --line: #d8d8d8; --panel: #f6f6f6;');
    lPage.Add('  --bad: #b3261e; --good: #1a7f37; }');
    lPage.Add('@media (prefers-color-scheme: dark) { :root {');
    lPage.Add('  --ink: #e8e8e8; --dim: #9a9a9a; --paper: #16181c;'
      + ' --line: #33363c; --panel: #1e2126;');
    lPage.Add('  --bad: #ff8a80; --good: #7ee2a8; } }');
    lPage.Add('body { margin: 0; padding: 24px; background: var(--paper);');
    lPage.Add('  color: var(--ink); font: 14px/1.5 system-ui, sans-serif; }');
    lPage.Add('h1 { font-size: 20px; margin: 0 0 4px; }');
    lPage.Add('p.sum { margin: 0 0 18px; color: var(--dim); }');
    lPage.Add('.bar { display: flex; gap: 10px; align-items: center;');
    lPage.Add('  margin-bottom: 18px; flex-wrap: wrap; }');
    lPage.Add('input, button { font: inherit; padding: 5px 10px;');
    lPage.Add('  border: 1px solid var(--line); border-radius: 6px;');
    lPage.Add('  background: var(--panel); color: var(--ink); }');
    lPage.Add('button { cursor: pointer; }');
    lPage.Add('button[aria-pressed="true"] { border-color: var(--dim); }');
    lPage.Add('table { border-collapse: collapse; width: 100%; }');
    lPage.Add('th { text-align: left; font-weight: 600; color: var(--dim);');
    lPage.Add('  padding: 6px 10px; border-bottom: 1px solid var(--line); }');
    lPage.Add('td { padding: 10px; border-bottom: 1px solid var(--line);');
    lPage.Add('  vertical-align: top; }');
    lPage.Add('.name { width: 16%; } .shot { width: 21%; }');
    lPage.Add('.tn { font-family: ui-monospace, monospace;');
    lPage.Add('  font-weight: 600; overflow-wrap: anywhere; }');
    lPage.Add('.pc { margin-top: 5px;');
    lPage.Add('  font-variant-numeric: tabular-nums; }');
    lPage.Add('.wy { margin-top: 5px; color: var(--dim);');
    lPage.Add('  font-style: italic; }');
    lPage.Add('.dim { color: var(--dim); }');
    lPage.Add('td.pct { text-align: right; font-variant-numeric: tabular-nums;');
    lPage.Add('  white-space: nowrap; }');
    lPage.Add('.fail { color: var(--bad); } .pass { color: var(--good); }');
    // A rendering may be transparent where the reference is not, so both
    // sit on a chequerboard that shows through.
    // The page holds a thousand images. They shrink to the window, and
    // the file itself opens at its own size on a click.
    lPage.Add('img { display: block; width: 100%; max-width: 480px;');
    lPage.Add('  height: auto; border: 1px solid var(--line);');
    lPage.Add('  background-color: #fff; background-size: 16px 16px;');
    lPage.Add('  background-position: 0 0, 0 8px, 8px -8px, -8px 0;');
    lPage.Add('  background-image:');
    lPage.Add('    linear-gradient(45deg, #eee 25%, transparent 25%),');
    lPage.Add('    linear-gradient(-45deg, #eee 25%, transparent 25%),');
    lPage.Add('    linear-gradient(45deg, transparent 75%, #eee 75%),');
    lPage.Add('    linear-gradient(-45deg, transparent 75%, #eee 75%); }');
    lPage.Add('td.gone { color: var(--dim); font-style: italic; }');
    lPage.Add('td.dim { color: var(--dim); }');
    lPage.Add('</style></head><body>');
    lPage.Add('<h1>' + HtmlText(FSuite) + ': our rendering beside the '
      + 'reference</h1>');
    lPage.Add('<p class="sum">Text drawn with ' + HtmlText(FEngine)
      + '.</p>');
    if lCompared > 0 then
      begin
      lPage.Add(Format('<p class="sum">%d documents, %d passed, %d failed. '
        + 'The mean difference is %s%% and the middle one differs by '
        + '%s%%. Worst first.</p>',
        [FRowCount, FPassed, FFailed,
         SVGFormatFloat(lMean), SVGFormatFloat(lMedian)]));
      // A document with a reason under it differs for something already
      // investigated, so the two figures apart show what is still open.
      if lPlain < lCompared then
        lPage.Add(Format('<p class="sum">%d of those have a recorded '
          + 'reason for their difference. Over the %d without one, the '
          + 'mean is %s%% and the middle one differs by %s%%.</p>',
          [lCompared - lPlain, lPlain,
           SVGFormatFloat(lPlainMean), SVGFormatFloat(lPlainMedian)]));
      if FDiffPixels > 0 then
        lPage.Add(Format('<p class="sum">Two parts of the test template '
          + 'differ on nearly every document, and both are counted rather '
          + 'than masked. The revision line differs both in its text and '
          + 'in the face it is set in, and accounts for %s%% of all the '
          + 'difference on this page. The frame differs because the '
          + 'renderer that made the references put a thin stroke on whole '
          + 'pixels while this one puts it where the geometry says, and '
          + 'accounts for %s%%. What remains once both are subtracted is '
          + 'the actual difference of a document.</p>',
          [SVGFormatFloat(FRevPixels * 100.0 / FDiffPixels),
           SVGFormatFloat(FBorderPixels * 100.0 / FDiffPixels)]));
      // A page drawn any way but the plain one says so, or the figures on
      // it read as the ones a plain run gives.
      if FPaintSample = psCorner then
        lPage.Add('<p class="sum">These were drawn with --paint-sample='
          + 'corner, which reads a gradient at the top left of a pixel, '
          + 'the way the references were drawn, instead of at its centre, '
          + 'where SVG means it to be read. That takes half a pixel of '
          + 'difference off every ramp, so the figures here are not the '
          + 'ones a plain run gives.</p>');
      end
    else
      lPage.Add(Format('<p class="sum">%d documents, none compared.</p>',
        [FRowCount]));
    lPage.Add('<div class="bar">');
    lPage.Add('<input id="q" type="search" placeholder="filter by name"'
      + ' autocomplete="off">');
    lPage.Add('<button id="byDiff" aria-pressed="true">worst first</button>');
    lPage.Add('<button id="byName" aria-pressed="false">by name</button>');
    lPage.Add('<span id="shown" style="color:var(--dim)"></span>');
    lPage.Add('</div>');
    lPage.Add('<table><thead><tr><th class="name">test</th>'
      + '<th class="shot">ours</th><th class="shot">reference</th>'
      + '<th class="shot">the document, drawn by this browser</th>'
      + '<th class="shot">how far apart, white where they agree</th>'
      + '</tr></thead><tbody id="rows">');
    for I := 0 to FRowCount - 1 do
      begin
      lName := FRows[I].Name;
      lPage.Add(Format('<tr data-name="%s" data-diff="%s">',
        [HtmlText(lName), SVGFormatFloat(FRows[I].Percentage)]));
      lPage.Add('<td class="name">');
      lPage.Add('<div class="tn">' + HtmlText(lName) + '</div>');
      if FRows[I].Percentage < 0 then
        lPage.Add('<div class="pc gone">no reference</div>')
      else
        begin
        if FRows[I].Outcome = doPass then
          lState := 'pass'
        else
          lState := 'fail';
        lPage.Add(Format('<div class="pc %s">%s%% differs</div>',
          [lState, SVGFormatFloat(FRows[I].Percentage)]));
        lRest := FRows[I].Percentage;
        if FRows[I].Revision > 0 then
          begin
          lRest := lRest - FRows[I].Revision;
          lPage.Add(Format('<div class="pc dim">Rev line: %s%%</div>',
            [SVGFormatFloat(FRows[I].Revision)]));
          end;
        if FRows[I].Border > 0 then
          begin
          lRest := lRest - FRows[I].Border;
          lPage.Add(Format('<div class="pc dim">Border: %s%%</div>',
            [SVGFormatFloat(FRows[I].Border)]));
          end;
        if lRest < 0 then
          lRest := 0;
        lPage.Add(Format('<div class="pc">Actual: %s%%</div>',
          [SVGFormatFloat(lRest)]));
        end;
      lReason := ReasonOf(lName);
      if lReason <> '' then
        lPage.Add('<div class="wy">' + HtmlText(lReason) + '</div>');
      lPage.Add('</td>');
      lPage.Add('<td class="shot">' + Shot(
        PageLink(lBase, IncludeTrailingPathDelimiter(FOutDir)
          + lName + '.png'), 'our rendering of ' + HtmlText(lName))
        + '</td>');
      lPage.Add('<td class="shot">' + Shot(
        PageLink(lBase, IncludeTrailingPathDelimiter(FRefDir)
          + lName + '.png'), 'the reference for ' + HtmlText(lName))
        + '</td>');
      lPage.Add('<td class="shot">' + Page(
        PageLink(lBase, IncludeTrailingPathDelimiter(FSVGDir)
          + lName + '.svg'), 'the document ' + HtmlText(lName),
        FWidth, FHeight) + '</td>');
      // White where the two agree, black where they differ most.
      // Following the link opens the image to view or to save.
      lPage.Add('<td class="shot">' + Shot(
        PageLink(lBase, IncludeTrailingPathDelimiter(FOutDir)
          + lName + '-distance.png'), 'how far ' + HtmlText(lName)
          + ' is from its reference') + '</td>');
      lPage.Add('</tr>');
      end;
    lPage.Add('</tbody></table>');
    lPage.Add('<script>');
    lPage.Add('var body = document.getElementById("rows");');
    lPage.Add('var all = Array.prototype.slice.call('
      + 'body.getElementsByTagName("tr"));');
    lPage.Add('var shown = document.getElementById("shown");');
    lPage.Add('function filter() {');
    lPage.Add('  var q = document.getElementById("q").value.toLowerCase();');
    lPage.Add('  var n = 0;');
    lPage.Add('  all.forEach(function (r) {');
    lPage.Add('    var hit = r.dataset.name.toLowerCase().indexOf(q) >= 0;');
    lPage.Add('    r.hidden = !hit; if (hit) n++; });');
    lPage.Add('  shown.textContent = n + " of " + all.length; }');
    lPage.Add('function order(byName) {');
    lPage.Add('  var rows = all.slice();');
    lPage.Add('  rows.sort(function (a, b) {');
    lPage.Add('    if (byName) return a.dataset.name < b.dataset.name'
      + ' ? -1 : 1;');
    lPage.Add('    return parseFloat(b.dataset.diff)'
      + ' - parseFloat(a.dataset.diff); });');
    lPage.Add('  rows.forEach(function (r) { body.appendChild(r); });');
    lPage.Add('  document.getElementById("byName").setAttribute('
      + '"aria-pressed", byName);');
    lPage.Add('  document.getElementById("byDiff").setAttribute('
      + '"aria-pressed", !byName); }');
    lPage.Add('document.getElementById("q").addEventListener('
      + '"input", filter);');
    lPage.Add('document.getElementById("byName").addEventListener('
      + '"click", function () { order(true); });');
    lPage.Add('document.getElementById("byDiff").addEventListener('
      + '"click", function () { order(false); });');
    lPage.Add('filter();');
    lPage.Add('</script></body></html>');
    lPage.SaveToFile(FHtml);
  finally
    lPage.Free;
  end;
  WriteLn('wrote ', FHtml);
end;


procedure TSVGDiffApplication.Report;

var
  lCompared, lPlain: Integer;
  lMean, lMedian, lPlainMean, lPlainMedian: Double;

begin
  WriteLn(Format('%d passed, %d failed, %d without a reference, %d in error',
    [FPassed, FFailed, FMissing, FErrored]));
  SortRows;
  Summarise(False, lMean, lMedian, lCompared);
  Summarise(True, lPlainMean, lPlainMedian, lPlain);
  if lCompared > 0 then
    begin
    WriteLn(Format('the mean difference is %s%% and the middle one differs '
      + 'by %s%%',
      [SVGFormatFloat(lMean), SVGFormatFloat(lMedian)]));
    if lPlain < lCompared then
      WriteLn(Format('%d have a recorded reason; over the other %d the '
        + 'mean is %s%% and the middle one %s%%',
        [lCompared - lPlain, lPlain,
         SVGFormatFloat(lPlainMean), SVGFormatFloat(lPlainMedian)]));
    end;
  if FLeftOut > 0 then
    WriteLn(Format('%d left out, resting on features that are not drawn '
      + 'here', [FLeftOut]));
  if FUnmarked > 0 then
    WriteLn(Format('%d drawn without their draft watermark',
      [FUnmarked]));
  if FMasked > 0 then
    WriteLn(Format('%d compared with the watermark band painted out of both',
      [FMasked]));
  if FDiffPixels > 0 then
    begin
    WriteLn(Format('the revision line is %s%% of the total difference, and '
      + 'the frame of the template %s%%. Both are counted in the figures '
      + 'above',
      [SVGFormatFloat(FRevPixels * 100.0 / FDiffPixels),
       SVGFormatFloat(FBorderPixels * 100.0 / FDiffPixels)]));
    end;
  if FHtml <> '' then
    WriteReportPage;
end;


procedure TSVGDiffApplication.DoRun;

var
  lTests: TStringList;
  I: Integer;

begin
  Terminate;
  if HasOption('h', 'help') then
    begin
    WriteHelp;
    Exit;
    end;
  ReadOptions;
  if not DirectoryExists(FSVGDir) then
    begin
    WriteLn('svgdiff: no documents in ', FSVGDir);
    WriteLn('The W3C suite is not part of this tree; tests/README says how');
    WriteLn('to fetch it.');
    ExitCode := 2;
    Exit;
    end;
  lTests := CollectTests;
  try
    if lTests.Count = 0 then
      begin
      WriteLn('svgdiff: no documents matched in ', FSVGDir);
      ExitCode := 2;
      Exit;
      end;
    if FList then
      begin
      for I := 0 to lTests.Count - 1 do
        WriteLn(lTests[I]);
      Exit;
      end;
    ForceDirectories(FOutDir);
    Say(Format('svgdiff: %d documents from %s, tolerance %d, threshold %s%%',
      [lTests.Count, FSVGDir, FTolerance, SVGFormatFloat(FThreshold)]));
    for I := 0 to lTests.Count - 1 do
      case RunTest(lTests[I]) of
        doPass: Inc(FPassed);
        doFail: Inc(FFailed);
        doNoReference: Inc(FMissing);
        doError: Inc(FErrored);
      end;
    Report;
    if (FFailed > 0) or (FErrored > 0) then
      ExitCode := 1;
  finally
    lTests.Free;
  end;
end;


var
  Application: TSVGDiffApplication;

begin
  Application := TSVGDiffApplication.Create(nil);
  try
    Application.Title := 'svgdiff';
    Application.Run;
  finally
    Application.Free;
  end;
end.
