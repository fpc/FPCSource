{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2026 by Michael Van Canneyt

    Draws the animated documents of a suite as GIFs, beside the SVG.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
program svgsmil;

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
     System.SysUtils, System.Classes, System.StrUtils, Fcl.CustApp,
     FpImage, FpImage.Writer.GIF, FpImage.Reader.PNG, svgcompare,
     fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render, fpsvg.anim,
     fpsvg.soft, fpsvg.freetype, fpsvg.fonts.support;
{$ELSE FPC_DOTTEDUNITS}
     sysutils, classes, strutils, custapp, fpimage, fpwritegif, fpreadpng,
     svgcompare, fpsvg.types, fpsvg.dom, fpsvg.read, fpsvg.render,
     fpsvg.anim, fpsvg.soft, fpsvg.freetype, fpsvg.fonts.support;
{$ENDIF FPC_DOTTEDUNITS}

const
  DefaultSuite = 'w3c';
  DefaultRoot = 'tests' + PathDelim + 'suites';
  DefaultOutput = 'build' + PathDelim + 'smil';
  DefaultPage = 'report-smil.html';
  DefaultWidth = 480;
  DefaultHeight = 360;
  { Frames a second, and seconds of it at most. Every frame is a render
    of its own, so the two together say what a run costs. }
  DefaultRate = 10;
  DefaultSeconds = 12;
  DefaultTail = 1.5;
  { Eight bit steps two channels may be apart and still count as equal,
    as the pixel harness counts them. }
  DefaultTolerance = 2;

type
  { One document of the report: what its timeline holds, and what was
    drawn of it. }
  TSVGSmilRow = record
    Name       : String;
    Title      : String;
    Elements   : Integer;    // animation elements the file holds
    Animations : Integer;    // those of them the timeline runs
    Duration   : Double;     // negative when it never ends
    Drawn      : Double;     // seconds of it drawn
    Frames     : Integer;
    // The frame that comes closest to the still the suite ships: how far
    // it falls from it as a percentage of the pixels, and the moment it
    // was drawn at. The percentage is negative where nothing was
    // compared.
    Stillness  : Double;
    StillAt    : Double;
    // How far the document falls from the still with no clock at all,
    // which says what the animation is worth. Negative where nothing was
    // compared.
    Authored   : Double;
    Note       : String;     // why nothing was drawn, when nothing was
    Failed     : Boolean;    // True when reading or drawing it raised
  end;
  TSVGSmilRowArray = array of TSVGSmilRow;

  TSVGSmilApplication = class(TCustomApplication)
  private
    FRoot, FSuite, FSVGDir, FRefDir, FOutDir, FPage, FFilter: String;
    FWidth, FHeight, FRate, FLoop, FTolerance: Integer;
    FSeconds, FTail: Double;
  FStamp: String;
    FQuiet, FList, FByName: Boolean;
    FBackground: TSVGColor;
    FFonts: TSVGFreeTypeProvider;
    FImages: TSVGFileImageResolver;
    FDocuments: TSVGFileDocumentResolver;
    FStyleSheets: TSVGFileStyleSheetResolver;
    FFontFiles: TSVGFileFontResolver;
    FRows: TSVGSmilRowArray;
    FRowCount: Integer;
    FDrawn, FUntimed, FErrored: Integer;
    procedure ReadOptions;
    function SuiteDir(const aPart: String): String;
    procedure Say(const aLine: String);
    procedure AddRow(const aRow: TSVGSmilRow);
    procedure SortRows;
    // The frame that comes closest to the still of that document, and
    // how far it falls from it. False when there is no still to compare
    // with, or none of the frames could be.
    function BestAgainstStill(const aName: String;
      const aFrames: array of TFPCustomImage; const aTimes: TSVGDoubleArray;
      out aAt, aPercentage: Double): Boolean;
    // The documents of the suite that animate, in name order.
    function CollectDocuments: TStringList;
    // The times to draw, from the timeline of the document.
    function FrameTimesOf(aTimeline: TSVGTimeline): TSVGDoubleArray;
    // Draws every frame of one document and writes them as one GIF.
    // False when nothing could be drawn, and the row says why.
    function DrawDocument(const aName: String; var aRow: TSVGSmilRow): Boolean;
    procedure WritePage;
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

begin
  Result := aDefault;
  if TryStrToSVGNumber(Trim(aText), lValue) then
    Result := lValue;
end;


// The name a test goes by: its file name without the extension.
function TestNameOf(const aFileName: String): String;

begin
  Result := ExtractFileName(aFileName);
  if LowerCase(ExtractFileExt(Result)) = '.svg' then
    SetLength(Result, Length(Result) - 4);
end;


// True when the text holds any of the words.
function HoldsAny(const aText: String; const aWords: array of String): Boolean;

var
  I: Integer;

begin
  Result := True;
  for I := Low(aWords) to High(aWords) do
    if Pos(aWords[I], aText) > 0 then
      Exit;
  Result := False;
end;


// True when the document declares an animation. The comments are taken
// out first, the suite writing an example of one in several of them.
function UsesSMIL(const aFileName: String): Boolean;

var
  lText: String;
  lStart, lStop: Integer;

begin
  Result := False;
  with TStringList.Create do
    try
      try
        LoadFromFile(aFileName);
        lText := LowerCase(Text);
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
  Result := HoldsAny(lText, ['<animate', '<set ', '<set/', '<set>',
                             'animatemotion', 'animatetransform',
                             'animatecolor']);
end;


// Number of animation elements below an element, itself included.
function CountAnimations(aElement: TSVGElement): Integer;

const
  Tags = ';set;animate;animateColor;animateTransform;animateMotion;';

var
  I: Integer;

begin
  Result := 0;
  if Pos(';' + aElement.TagName + ';', Tags) > 0 then
    Result := 1;
  for I := 0 to aElement.ChildCount - 1 do
    if aElement[I] is TSVGElement then
      Result := Result + CountAnimations(TSVGElement(aElement[I]));
end;


// The title a document gives itself, empty when it gives none.
function TitleOf(aDocument: TSVGDocument): String;

var
  lElement: TSVGElement;

begin
  Result := '';
  if (aDocument = nil) or (aDocument.Root = nil) then
    Exit;
  lElement := aDocument.Root.FindChildElement('title');
  if lElement <> nil then
    Result := Trim(lElement.TextContent);
end;


// The text with the characters a page cannot hold written out.
function HtmlText(const aText: String): String;

begin
  Result := StringReplace(aText, '&', '&amp;', [rfReplaceAll]);
  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
  Result := StringReplace(Result, '"', '&quot;', [rfReplaceAll]);
end;


// A percentage with two decimals, written without the locale.
function Percent(aValue: Double): String;

begin
  Str(aValue:0:2, Result);
end;


// A link from the page to a file, relative where the two share a root.
function PageLink(const aBase, aFileName: String): String;

begin
  Result := ExtractRelativepath(aBase, ExpandFileName(aFileName));
  Result := StringReplace(Result, PathDelim, '/', [rfReplaceAll]);
  Result := StringReplace(Result, ' ', '%20', [rfReplaceAll]);
end;


{ TSVGSmilApplication }

constructor TSVGSmilApplication.Create(aOwner: TComponent);

begin
  inherited Create(aOwner);
  StopOnException := True;
  FRoot := DefaultRoot;
  FSuite := DefaultSuite;
  FOutDir := DefaultOutput;
  FPage := DefaultPage;
  FWidth := DefaultWidth;
  FHeight := DefaultHeight;
  FRate := DefaultRate;
  FSeconds := DefaultSeconds;
  FTail := DefaultTail;
  FLoop := 0;
  FTolerance := DefaultTolerance;
  FStamp := FormatDateTime('yyyymmddhhnnss', Now);
  FBackground := TSVGColor.FromBytes(255, 255, 255, 255);
end;


destructor TSVGSmilApplication.Destroy;

begin
  FImages := nil;
  FFontFiles := nil;
  FreeAndNil(FDocuments);
  FreeAndNil(FStyleSheets);
  FreeAndNil(FFonts);
  inherited Destroy;
end;


procedure TSVGSmilApplication.Say(const aLine: String);

begin
  if not FQuiet then
    WriteLn(aLine);
end;


procedure TSVGSmilApplication.WriteHelp;

begin
  WriteLn('svgsmil: draws the animated documents of a suite as GIFs, and');
  WriteLn('writes a page that shows each beside the SVG itself, so that');
  WriteLn('what this draws can be held against what a browser draws.');
  WriteLn;
  WriteLn('Usage: svgsmil [options]');
  WriteLn;
  WriteLn('  --root=DIR       where the suites are, default ', DefaultRoot);
  WriteLn('  --suite=NAME     which one, default ', DefaultSuite);
  WriteLn('  --svg=DIR        the documents, default <root>/<suite>/svg');
  WriteLn('  --ref=DIR        the reference images, default');
  WriteLn('                   <root>/<suite>/png. They are stills of an');
  WriteLn('                   unstated moment, and the page says so.');
  WriteLn('  --out=DIR        where the GIFs go, default ', DefaultOutput);
  WriteLn('  --html=FILE      the page to write, default ', DefaultPage);
  WriteLn('  --filter=TEXT    only the documents whose name holds it');
  WriteLn('  --rate=N         frames a second, default ', DefaultRate);
  WriteLn('  --seconds=N      seconds of an animation at most, default ',
    DefaultSeconds, '. An');
  WriteLn('                   animation that never ends is cut off there.');
  WriteLn('  --tail=N         seconds drawn after the last animation ends,');
  WriteLn('                   default ', SVGFormatFloat(DefaultTail),
    '. A document whose point is the state');
  WriteLn('                   it settles in shows nothing without it.');
  WriteLn('  --loop=N         times a GIF plays, default 0 for over and');
  WriteLn('                   over again, which is what lets a row be');
  WriteLn('                   watched without reloading the page.');
  WriteLn('  --width=N        device width, default ', DefaultWidth);
  WriteLn('  --height=N       device height, default ', DefaultHeight);
  WriteLn('  --fonts=DIR      a directory of faces, repeatable. Without');
  WriteLn('                   one the system faces are read.');
  WriteLn('  --tolerance=N    eight bit steps two channels may be apart');
  WriteLn('                   and still count as equal, default ',
    DefaultTolerance);
  WriteLn('  --by-name        order the page by name. Without it the');
  WriteLn('                   document whose last frame falls furthest');
  WriteLn('                   from the still of the suite comes first.');
  WriteLn('  --list           name the documents and draw nothing');
  WriteLn('  --quiet          say nothing but the summary');
  WriteLn('  -h, --help       this text');
end;


procedure TSVGSmilApplication.ReadOptions;

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
  if HasOption('html') then
    FPage := GetOptionValue('html');
  if HasOption('filter') then
    FFilter := GetOptionValue('filter');
  if HasOption('rate') then
    FRate := StrToIntDef(GetOptionValue('rate'), DefaultRate);
  if FRate < 1 then
    FRate := 1;
  if HasOption('seconds') then
    FSeconds := NumberDef(GetOptionValue('seconds'), DefaultSeconds);
  if HasOption('tail') then
    FTail := NumberDef(GetOptionValue('tail'), DefaultTail);
  if FTail < 0 then
    FTail := 0;
  if HasOption('loop') then
    FLoop := StrToIntDef(GetOptionValue('loop'), 0);
  if HasOption('width') then
    FWidth := StrToIntDef(GetOptionValue('width'), DefaultWidth);
  if HasOption('height') then
    FHeight := StrToIntDef(GetOptionValue('height'), DefaultHeight);
  if HasOption('tolerance') then
    FTolerance := StrToIntDef(GetOptionValue('tolerance'), DefaultTolerance);
  FByName := HasOption('by-name');
  FQuiet := HasOption('quiet');
  FList := HasOption('list');
  if FSVGDir = '' then
    FSVGDir := SuiteDir('svg');
  if FRefDir = '' then
    FRefDir := SuiteDir('png');
  FFonts := TSVGFreeTypeProvider.Create;
  lPaths := GetOptionValues(#0, 'fonts');
  for I := 0 to High(lPaths) do
    FFonts.AddFontPath(lPaths[I]);
  if Length(lPaths) = 0 then
    FFonts.AddSystemFonts;
  // The suite ships the faces its documents ask for next to them.
  FFonts.AddFontPath(IncludeTrailingPathDelimiter(FSVGDir) + 'woffs');
  FFonts.AddFontPath(ExtractFilePath(
    ExcludeTrailingPathDelimiter(FSVGDir)) + 'resources');
  FFonts.Coverage := SVGPlatformCoverage;
  FImages := TSVGFileImageResolver.Create(FSVGDir);
  FDocuments := TSVGFileDocumentResolver.Create(FSVGDir);
  FStyleSheets := TSVGFileStyleSheetResolver.Create(FSVGDir);
  FFontFiles := TSVGFileFontResolver.Create(FSVGDir);
end;


function TSVGSmilApplication.SuiteDir(const aPart: String): String;

begin
  Result := IncludeTrailingPathDelimiter(FRoot)
    + IncludeTrailingPathDelimiter(FSuite) + aPart;
end;


procedure TSVGSmilApplication.AddRow(const aRow: TSVGSmilRow);

begin
  if FRowCount = Length(FRows) then
    SetLength(FRows, 16 + FRowCount * 2);
  FRows[FRowCount] := aRow;
  Inc(FRowCount);
end;


procedure TSVGSmilApplication.SortRows;

var
  lRow: TSVGSmilRow;
  I, J: Integer;

begin
  // The document whose last frame falls furthest from the still of the
  // suite comes first, that being the one worth looking at first. One
  // with no still to hold it against goes last.
  for I := 1 to FRowCount - 1 do
    begin
    lRow := FRows[I];
    J := I - 1;
    while (J >= 0) and ((FRows[J].Stillness < lRow.Stillness)
          or (FByName and (CompareText(FRows[J].Name, lRow.Name) > 0))) do
      begin
      FRows[J + 1] := FRows[J];
      Dec(J);
      end;
    FRows[J + 1] := lRow;
    end;
end;


function TSVGSmilApplication.BestAgainstStill(const aName: String;
  const aFrames: array of TFPCustomImage; const aTimes: TSVGDoubleArray;
  out aAt, aPercentage: Double): Boolean;

var
  lStill: TFPMemoryImage;
  lReader: TFPReaderPNG;
  lFileName: String;
  lHere: Double;
  I: Integer;

begin
  Result := False;
  aAt := 0;
  aPercentage := -1;
  lFileName := IncludeTrailingPathDelimiter(FRefDir) + aName + '.png';
  if not FileExists(lFileName) then
    Exit;
  lStill := TFPMemoryImage.Create(0, 0);
  lReader := TFPReaderPNG.Create;
  try
    try
      lStill.LoadFromFile(lFileName, lReader);
    except
      Exit;
    end;
    // The still is of a moment nothing states, so every frame is held
    // against it and the closest one says which moment that was.
    for I := 0 to High(aFrames) do
      begin
      if (lStill.Width <> aFrames[I].Width)
         or (lStill.Height <> aFrames[I].Height) then
        Exit;
      lHere := SVGCompareImages(aFrames[I], lStill, FTolerance, FBackground,
        nil).Percentage;
      if (aPercentage < 0) or (lHere < aPercentage) then
        begin
        aPercentage := lHere;
        aAt := aTimes[I];
        Result := True;
        end;
      end;
  finally
    lReader.Free;
    lStill.Free;
  end;
end;


function TSVGSmilApplication.CollectDocuments: TStringList;

var
  lSearch: TSearchRec;
  lPath, lName: String;

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
      if not UsesSMIL(lPath + lSearch.Name) then
        Continue;
      Result.Add(lName);
    until FindNext(lSearch) <> 0;
  finally
    FindClose(lSearch);
  end;
end;


function TSVGSmilApplication.FrameTimesOf(
  aTimeline: TSVGTimeline): TSVGDoubleArray;

var
  lRun, lStep: Double;
  lCount, I: Integer;

begin
  Result := nil;
  // An animation is drawn at a steady rate rather than at the times its
  // values change: a run of one is what a browser shows, and the times
  // of the changes would draw a linear animation in two frames.
  lStep := 1.0 / FRate;
  lRun := aTimeline.Duration;
  // The last animation ending is not the last thing to see: a document
  // testing restart or a fill that removes settles once it is over.
  if lRun >= 0 then
    lRun := lRun + FTail;
  if (lRun < 0) or (lRun > FSeconds) then
    lRun := FSeconds;
  lCount := Trunc(lRun / lStep + 1E-6) + 1;
  if lCount < 1 then
    lCount := 1;
  SetLength(Result, lCount);
  for I := 0 to lCount - 1 do
    Result[I] := I * lStep;
end;


function TSVGSmilApplication.DrawDocument(const aName: String;
  var aRow: TSVGSmilRow): Boolean;

var
  lDocument: TSVGDocument;
  lTimeline: TSVGTimeline;
  lRenderer: TSVGRenderer;
  lBackend: TSVGSoftBackend;
  lWriter: TFPWriterGIF;
  lStream: TFileStream;
  lFrames: array of TFPCustomImage;
  lAuthored: TFPCustomImage;
  lOne: array[0..0] of TFPCustomImage;
  lZero: TSVGDoubleArray;
  lAt: Double;
  lTimes: TSVGDoubleArray;
  lTarget: String;
  I: Integer;

begin
  Result := False;
  lDocument := nil;
  lTimeline := nil;
  lRenderer := nil;
  lBackend := nil;
  lWriter := nil;
  lFrames := nil;
  lAuthored := nil;
  try
    try
      lDocument := ReadSVGFile(
        IncludeTrailingPathDelimiter(FSVGDir) + aName + '.svg');
      lDocument.BaseURI := IncludeTrailingPathDelimiter(FSVGDir)
        + aName + '.svg';
      aRow.Title := TitleOf(lDocument);
      if lDocument.Root <> nil then
        aRow.Elements := CountAnimations(lDocument.Root);
      lTimeline := TSVGTimeline.Create(lDocument);
      aRow.Animations := lTimeline.Count;
      aRow.Duration := lTimeline.Duration;
      if not lTimeline.IsAnimated then
        begin
        // The timeline leaves out an animation it can put no time to: one
        // waiting on an event, on an indefinite begin, or on another
        // animation that never begins.
        aRow.Note := 'nothing to draw: no animation of it has a time';
        Exit;
        end;
      lTimes := FrameTimesOf(lTimeline);
      aRow.Frames := Length(lTimes);
      aRow.Drawn := lTimes[High(lTimes)];
      lRenderer := TSVGRenderer.Create;
      lRenderer.Fonts := FFonts;
      lRenderer.Images := FImages;
      lRenderer.Documents := FDocuments;
      lRenderer.StyleSheets := FStyleSheets;
      lRenderer.FontFiles := FFontFiles;
      lBackend := TSVGSoftBackend.Create;
      // The document as it stands, before a clock touched it. It is the
      // measure of what seeking is worth on this document.
      lRenderer.RenderToSize(lDocument, lBackend, FWidth, FHeight);
      lAuthored := TFPMemoryImage.Create(FWidth, FHeight);
      lAuthored.Assign(lBackend.Image);
      SetLength(lFrames, Length(lTimes));
      for I := 0 to High(lTimes) do
        begin
        lTimeline.Seek(lTimes[I]);
        lRenderer.RenderToSize(lDocument, lBackend, FWidth, FHeight);
        lFrames[I] := TFPMemoryImage.Create(FWidth, FHeight);
        lFrames[I].Assign(lBackend.Image);
        end;
      lTimeline.Reset;
      BestAgainstStill(aName, lFrames, lTimes, aRow.StillAt,
        aRow.Stillness);
      lOne[0] := lAuthored;
      lZero := nil;
      SetLength(lZero, 1);
      lZero[0] := 0;
      BestAgainstStill(aName, lOne, lZero, lAt, aRow.Authored);
      lTarget := IncludeTrailingPathDelimiter(FOutDir) + aName + '.gif';
      ForceDirectories(ExtractFileDir(lTarget));
      lWriter := TFPWriterGIF.Create;
      lWriter.Delay := Round(100.0 / FRate);
      lWriter.LoopCount := FLoop;
      lStream := TFileStream.Create(lTarget, fmCreate);
      try
        lWriter.ImagesWrite(lStream, lFrames);
      finally
        lStream.Free;
      end;
      if aRow.Stillness >= 0 then
        Say(Format('  %-32s %3d frames of %ss, closest at %ss by %s%%',
          [aName, aRow.Frames, SVGFormatFloat(aRow.Drawn),
           SVGFormatFloat(aRow.StillAt), Percent(aRow.Stillness)]))
      else
        Say(Format('  %-32s %3d frames of %ss, no still to compare',
          [aName, aRow.Frames, SVGFormatFloat(aRow.Drawn)]));
      Result := True;
    except
      on E: Exception do
        begin
        aRow.Note := E.Message;
        aRow.Failed := True;
        WriteLn(Format('  ERR  %s: %s', [aName, E.Message]));
        end;
    end;
  finally
    lAuthored.Free;
    for I := 0 to High(lFrames) do
      lFrames[I].Free;
    lWriter.Free;
    lBackend.Free;
    lRenderer.Free;
    lTimeline.Free;
    lDocument.Free;
  end;
end;


procedure TSVGSmilApplication.WritePage;

var
  lPage: TStringList;
  lBase, lSVG, lGif, lRef, lFacts: String;
  lMean: Double;
  lCount, I: Integer;

begin
  lBase := IncludeTrailingPathDelimiter(
    ExpandFileName(ExtractFilePath(FPage)));
  lPage := TStringList.Create;
  try
    lPage.Add('<!DOCTYPE html>');
    lPage.Add('<html lang="en"><head><meta charset="utf-8">');
    lPage.Add('<meta name="viewport" content="width=device-width,'
      + ' initial-scale=1">');
    lPage.Add('<title>' + HtmlText(FSuite) + ' animations</title>');
    lPage.Add('<style>');
    lPage.Add(':root { color-scheme: light dark;');
    lPage.Add('  --ink: #1a1a1a; --dim: #666; --paper: #fff;'
      + ' --line: #d8d8d8; --panel: #f6f6f6; }');
    lPage.Add('@media (prefers-color-scheme: dark) { :root {');
    lPage.Add('  --ink: #e8e8e8; --dim: #9a9a9a; --paper: #16181c;'
      + ' --line: #33363c; --panel: #1e2126; } }');
    lPage.Add('body { margin: 0; padding: 24px; background: var(--paper);');
    lPage.Add('  color: var(--ink); font: 14px/1.5 system-ui, sans-serif; }');
    lPage.Add('h1 { font-size: 20px; margin: 0 0 4px; }');
    lPage.Add('p.sum { margin: 0 0 12px; color: var(--dim);');
    lPage.Add('  max-width: 60em; }');
    lPage.Add('.bar { display: flex; gap: 10px; align-items: center;');
    lPage.Add('  margin-bottom: 18px; flex-wrap: wrap; }');
    lPage.Add('input, button { font: inherit; padding: 5px 10px;');
    lPage.Add('  border: 1px solid var(--line); border-radius: 6px;');
    lPage.Add('  background: var(--panel); color: var(--ink); }');
    lPage.Add('button { cursor: pointer; }');
    lPage.Add('table { border-collapse: collapse; width: 100%; }');
    lPage.Add('th { text-align: left; font-weight: 600; color: var(--dim);');
    lPage.Add('  padding: 6px 10px; border-bottom: 1px solid var(--line); }');
    lPage.Add('td { padding: 10px; border-bottom: 1px solid var(--line);');
    lPage.Add('  vertical-align: top; }');
    lPage.Add('.name { width: 16%; } .shot { width: 28%; }');
    lPage.Add('.tn { font-family: ui-monospace, monospace;');
    lPage.Add('  font-weight: 600; overflow-wrap: anywhere; }');
    lPage.Add('.fact { margin-top: 5px; color: var(--dim);');
    lPage.Add('  font-variant-numeric: tabular-nums; }');
    lPage.Add('.note { margin-top: 5px; color: var(--dim);');
    lPage.Add('  font-style: italic; }');
    lPage.Add('.row { margin-top: 6px; display: flex; gap: 8px;');
    lPage.Add('  align-items: center; flex-wrap: wrap; }');
    lPage.Add('.row a { color: inherit; }');
    lPage.Add('button.small { padding: 2px 8px; font-size: 12px; }');
    lPage.Add('details { margin: 0 0 12px; max-width: 60em; }');
    lPage.Add('summary { cursor: pointer; color: var(--dim); }');
    lPage.Add('details p.sum:first-of-type { margin-top: 10px; }');
    // Both panes sit on a chequerboard: a document of the suite is drawn
    // on nothing where it paints no background itself. The box keeps the
    // shape of the frame whether its content has loaded or not, so that
    // nothing on the page moves as a row comes alive.
    lPage.Add(Format('.pane { position: relative; display: block;'
      + ' width: 100%%;', []));
    lPage.Add(Format('  max-width: %dpx; aspect-ratio: %d / %d;',
      [FWidth, FWidth, FHeight]));
    lPage.Add('  border: 1px solid var(--line); cursor: pointer;');
    lPage.Add('  background-color: #fff; background-size: 16px 16px;');
    lPage.Add('  background-position: 0 0, 0 8px, 8px -8px, -8px 0;');
    lPage.Add('  background-image:');
    lPage.Add('    linear-gradient(45deg, #eee 25%, transparent 25%),');
    lPage.Add('    linear-gradient(-45deg, #eee 25%, transparent 25%),');
    lPage.Add('    linear-gradient(45deg, transparent 75%, #eee 75%),');
    lPage.Add('    linear-gradient(-45deg, transparent 75%, #eee 75%); }');
    lPage.Add('.pane > * { display: block; width: 100%; height: 100%;');
    lPage.Add('  border: 0; }');
    lPage.Add('.pane.still { cursor: default; }');
    lPage.Add('</style></head><body>');
    lPage.Add('<h1>' + HtmlText(FSuite) + ': the animation this draws '
      + 'beside the one the browser draws</h1>');
    lPage.Add(Format('<p class="sum">%d documents of the suite animate, '
      + 'and %d of them are drawn here. The rest hold nothing this can '
      + 'put a time to: an animation waiting on a click or a mouseover, '
      + 'on an indefinite begin that only a script or a link starts, or '
      + 'on another animation that never begins. Their row says so.</p>',
      [FRowCount, FDrawn]));
    lPage.Add(Format('<p class="sum">The middle column is the document '
      + 'itself, which the browser animates with its own SMIL; the right '
      + 'one is what this renderer draws, seeked %d times a second and '
      + 'written as a GIF. A row reads right when the two run alike. '
      + 'Click either pane, or the Replay button of the row, to run the '
      + 'two again from the same moment.</p>', [FRate]));
    lPage.Add('<details><summary>What the figures say, and what a GIF '
      + 'cannot</summary>');
    lPage.Add(Format('<p class="sum">The GIFs hold %s seconds at most and '
      + 'play over and over again, so a row can be watched without '
      + 'reloading; the browser plays an animation once unless the '
      + 'document says otherwise, so press Replay to start the two '
      + 'together. A GIF holds 256 colours to a file, which is what '
      + 'makes a gradient band where the SVG is smooth, and it steps '
      + 'where the browser is continuous. Neither is a fault of the '
      + 'drawing. Each runs %ss past the end of its last animation, '
      + 'which is where a document testing restart or a fill that '
      + 'removes shows what it is for.</p>',
      [SVGFormatFloat(FSeconds), SVGFormatFloat(FTail)]));
    lCount := 0;
    lMean := 0;
    for I := 0 to FRowCount - 1 do
      if FRows[I].Stillness >= 0 then
        begin
        lMean := lMean + FRows[I].Stillness;
        Inc(lCount);
        end;
    if lCount > 0 then
      lPage.Add(Format('<p class="sum">Every frame drawn is held against '
        + 'the still, and the closest one says which moment the still is '
        + 'of. Over the %d documents that have one, the closest frame '
        + 'falls %s%% from it on average and the middle one %s%%, which is '
        + 'the range the static documents of the suite differ by as well. '
        + 'A row far above that is worth a look, and so is one whose '
        + 'closest moment is nowhere near the end, which says the still '
        + 'was photographed part way through.</p>',
        [lCount, Percent(lMean / lCount),
         Percent(FRows[lCount div 2].Stillness)]));
    lPage.Add('<p class="sum">Each row also says how far the document '
      + 'falls from the still with no clock at all. A frame that comes '
      + 'much closer than that is the animation doing its work; a row '
      + 'where no frame comes closer is one where seeking changed '
      + 'nothing the still can see, which is worth knowing whether the '
      + 'fault is here or the still is simply of the first moment.</p>');
    lPage.Add('<p class="sum">The last column is the still the suite '
      + 'ships. It is a photograph of one moment that nothing states, '
      + 'often after an operator interacted, which is why the pixel '
      + 'harness leaves these documents out. It is here for what a frame '
      + 'is meant to look like, not as something to match.</p>');
    lPage.Add('<p class="sum">An animation of the suite plays once and '
      + 'stops. Clicking a pane runs the row again without reloading, so '
      + 'the page keeps its place; the button at the top does it for '
      + 'every row on show. A row starts playing when it is first '
      + 'scrolled to, and a reload comes back to where you were.</p>');
    lPage.Add('</details>');
    lPage.Add('<div class="bar">');
    lPage.Add('<input id="q" type="search" placeholder="filter by name"'
      + ' oninput="filter()">');
    lPage.Add('<button type="button" onclick="replayAll()">Replay what is '
      + 'on show</button>');
    lPage.Add('<span id="count" class="dim"></span>');
    lPage.Add('</div>');
    lPage.Add('<table><thead><tr>');
    lPage.Add('<th class="name">document</th>');
    lPage.Add('<th class="shot">the browser, from the SVG</th>');
    lPage.Add('<th class="shot">this renderer, as a GIF</th>');
    lPage.Add('<th class="shot">the still of the suite</th>');
    lPage.Add('</tr></thead><tbody id="body">');
    for I := 0 to FRowCount - 1 do
      begin
      lSVG := PageLink(lBase, IncludeTrailingPathDelimiter(FSVGDir)
        + FRows[I].Name + '.svg');
      // A GIF is written to the name it had last time, so the page asks
      // for it under a token of this run rather than the one a browser
      // has in hand.
      lGif := PageLink(lBase, IncludeTrailingPathDelimiter(FOutDir)
        + FRows[I].Name + '.gif') + '?v=' + FStamp;
      lRef := PageLink(lBase, IncludeTrailingPathDelimiter(FRefDir)
        + FRows[I].Name + '.png');
      lPage.Add('<tr data-name="' + HtmlText(LowerCase(FRows[I].Name))
        + '">');
      lPage.Add('<td class="name"><div class="tn">'
        + HtmlText(FRows[I].Name) + '</div>');
      if FRows[I].Title <> '' then
        lPage.Add('<div class="fact">' + HtmlText(FRows[I].Title)
          + '</div>');
      lFacts := Format('%d animation elements', [FRows[I].Elements]);
      if FRows[I].Animations = 0 then
        lFacts := lFacts + ', none timed'
      else
        begin
        if FRows[I].Animations < FRows[I].Elements then
          lFacts := lFacts + Format(', %d timed', [FRows[I].Animations]);
        if FRows[I].Duration < 0 then
          lFacts := lFacts + ', never ending'
        else
          lFacts := lFacts + ', ' + SVGFormatFloat(FRows[I].Duration)
            + 's long';
        if FRows[I].Frames > 0 then
          lFacts := lFacts + Format(', %d frames of %ss',
            [FRows[I].Frames, SVGFormatFloat(FRows[I].Drawn)]);
        if (FRows[I].Duration < 0)
           or (FRows[I].Duration > FRows[I].Drawn) then
          lFacts := lFacts + ', cut off before its end';
        end;
      lPage.Add('<div class="fact">' + HtmlText(lFacts) + '</div>');
      if FRows[I].Stillness >= 0 then
        begin
        lPage.Add(Format('<div class="fact">the still is closest to the '
          + 'frame at %ss, differing by %s%%</div>',
          [SVGFormatFloat(FRows[I].StillAt), Percent(FRows[I].Stillness)]));
        if FRows[I].Authored >= 0 then
          lPage.Add(Format('<div class="fact">with no clock at all it '
            + 'differs by %s%%</div>', [Percent(FRows[I].Authored)]));
        if (FRows[I].Authored >= 0)
           and (FRows[I].Stillness >= FRows[I].Authored - 0.01) then
          lPage.Add('<div class="note">no frame of it comes closer to the '
            + 'still than the document does untouched</div>');
        end;
      if FRows[I].Note <> '' then
        lPage.Add('<div class="note">' + HtmlText(FRows[I].Note)
          + '</div>');
      lPage.Add('<div class="row">');
      lPage.Add('<button type="button" class="small"'
        + ' onclick="replayRow(this)">Replay</button>');
      lPage.Add('<a href="' + lSVG + '">svg</a>');
      if FRows[I].Frames > 0 then
        lPage.Add('<a href="' + lGif + '">gif</a>');
      lPage.Add('<a href="' + lRef + '">still</a>');
      lPage.Add('</div>');
      lPage.Add('</td>');
      // The document is embedded rather than shown as an image: an
      // embedded one has an animation clock that a click can reset,
      // where the clock of an image cannot be reached.
      lPage.Add('<td class="shot"><div class="pane live"'
        + ' onclick="replayRow(this)"><object type="image/svg+xml"'
        + ' data-src="' + lSVG + '" aria-label="the document">'
        + '<img src="' + lSVG + '" alt="the document"></object></div></td>');
      if FRows[I].Frames > 0 then
        lPage.Add('<td class="shot"><div class="pane"'
          + ' onclick="replayRow(this)"><img class="gif" src="' + lGif
          + '" alt="this rendering" loading="lazy"></div></td>')
      else
        lPage.Add('<td class="shot note">nothing drawn</td>');
      lPage.Add('<td class="shot"><div class="pane still"><img src="'
        + lRef + '" alt="the still of the suite" loading="lazy"></div>'
        + '</td>');
      lPage.Add('</tr>');
      end;
    lPage.Add('</tbody></table>');
    lPage.Add('<script>');
    lPage.Add('var rows = Array.prototype.slice.call(');
    lPage.Add('  document.querySelectorAll("#body tr"));');
    lPage.Add('var box = document.getElementById("q");');
    lPage.Add('function filter() {');
    lPage.Add('  var q = box.value.toLowerCase();');
    lPage.Add('  var shown = 0;');
    lPage.Add('  rows.forEach(function (r) {');
    lPage.Add('    var on = r.dataset.name.indexOf(q) >= 0;');
    lPage.Add('    r.style.display = on ? "" : "none";');
    lPage.Add('    if (on) shown++;');
    lPage.Add('  });');
    lPage.Add('  document.getElementById("count").textContent =');
    lPage.Add('    shown + " of " + rows.length;');
    lPage.Add('  try { sessionStorage.setItem("smil-filter", box.value); }');
    lPage.Add('  catch (e) {}');
    lPage.Add('  wakeVisible();');
    lPage.Add('}');
    // An embedded document is put back to the start of its clock. The
    // clock of a GIF cannot be reached, so its element is created again,
    // which starts it at its first frame without fetching anything.
    lPage.Add('function restart(el) {');
    lPage.Add('  if (el.tagName === "OBJECT") {');
    lPage.Add('    try {');
    lPage.Add('      var d = el.contentDocument;');
    lPage.Add('      if (d && d.documentElement &&');
    lPage.Add('          d.documentElement.setCurrentTime) {');
    lPage.Add('        d.documentElement.setCurrentTime(0);');
    lPage.Add('        return;');
    lPage.Add('      }');
    lPage.Add('    } catch (e) {}');
    lPage.Add('  }');
    lPage.Add('  var fresh = el.cloneNode(true);');
    lPage.Add('  el.parentNode.replaceChild(fresh, el);');
    lPage.Add('}');
    lPage.Add('function replayRow(from) {');
    lPage.Add('  var tr = from.closest("tr");');
    lPage.Add('  if (!tr) return;');
    lPage.Add('  wake(tr);');
    lPage.Add('  tr.querySelectorAll("object, img.gif").forEach(restart);');
    lPage.Add('}');
    lPage.Add('function replayAll() {');
    lPage.Add('  rows.forEach(function (r) {');
    lPage.Add('    if (r.style.display !== "none") replayRow(r);');
    lPage.Add('  });');
    lPage.Add('}');
    // A document is embedded only once its row is reached. Eighty of
    // them at once is a heavy page, and a row that comes alive as it is
    // scrolled to starts its animation where it can be seen.
    lPage.Add('function wake(tr) {');
    lPage.Add('  tr.querySelectorAll("object[data-src]").forEach(');
    lPage.Add('    function (o) {');
    lPage.Add('      o.data = o.dataset.src;');
    lPage.Add('      o.removeAttribute("data-src");');
    lPage.Add('    });');
    lPage.Add('}');
    lPage.Add('function wakeVisible() {');
    lPage.Add('  document.querySelectorAll("#body object[data-src]").forEach(');
    lPage.Add('    function (o) {');
    lPage.Add('      var r = o.getBoundingClientRect();');
    lPage.Add('      if ((r.height > 0) && (r.bottom > -200)');
    lPage.Add('          && (r.top < innerHeight + 200)) {');
    lPage.Add('        wake(o.closest("tr"));');
    lPage.Add('      }');
    lPage.Add('    });');
    lPage.Add('}');
    lPage.Add('var ticking = false;');
    lPage.Add('function onScroll() {');
    lPage.Add('  if (ticking) return;');
    lPage.Add('  ticking = true;');
    lPage.Add('  requestAnimationFrame(function () {');
    lPage.Add('    ticking = false;');
    lPage.Add('    wakeVisible();');
    lPage.Add('  });');
    lPage.Add('}');
    lPage.Add('window.addEventListener("scroll", onScroll,');
    lPage.Add('  { passive: true });');
    lPage.Add('window.addEventListener("resize", onScroll);');
    // A reload of the page, forced or not, comes back to where it was
    // rather than to the top of eighty rows.
    lPage.Add('if ("scrollRestoration" in history) {');
    lPage.Add('  history.scrollRestoration = "manual";');
    lPage.Add('}');
    lPage.Add('window.addEventListener("beforeunload", function () {');
    lPage.Add('  try {');
    lPage.Add('    sessionStorage.setItem("smil-scroll", String(scrollY));');
    lPage.Add('  } catch (e) {}');
    lPage.Add('});');
    lPage.Add('try {');
    lPage.Add('  box.value = sessionStorage.getItem("smil-filter") || "";');
    lPage.Add('} catch (e) {}');
    lPage.Add('filter();');
    lPage.Add('window.addEventListener("load", function () {');
    lPage.Add('  var y = null;');
    lPage.Add('  try { y = sessionStorage.getItem("smil-scroll"); }');
    lPage.Add('  catch (e) {}');
    lPage.Add('  if (y !== null) window.scrollTo(0, parseInt(y, 10) || 0);');
    lPage.Add('  wakeVisible();');
    lPage.Add('});');
    lPage.Add('</script></body></html>');
    ForceDirectories(ExtractFileDir(ExpandFileName(FPage)));
    lPage.SaveToFile(FPage);
  finally
    lPage.Free;
  end;
end;


procedure TSVGSmilApplication.DoRun;

var
  lNames: TStringList;
  lRow: TSVGSmilRow;
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
    WriteLn('svgsmil: no documents in ', FSVGDir);
    WriteLn('The W3C suite is not part of this tree; tests/README says how');
    WriteLn('to fetch it.');
    ExitCode := 2;
    Exit;
    end;
  lNames := CollectDocuments;
  try
    if lNames.Count = 0 then
      begin
      WriteLn('svgsmil: no animated documents matched in ', FSVGDir);
      ExitCode := 2;
      Exit;
      end;
    if FList then
      begin
      for I := 0 to lNames.Count - 1 do
        WriteLn(lNames[I]);
      Exit;
      end;
    Say(Format('svgsmil: %d animated documents from %s, %d frames a '
      + 'second, %ss at most', [lNames.Count, FSVGDir, FRate,
      SVGFormatFloat(FSeconds)]));
    for I := 0 to lNames.Count - 1 do
      begin
      lRow := Default(TSVGSmilRow);
      lRow.Name := lNames[I];
      lRow.Stillness := -1;
      lRow.Authored := -1;
      if DrawDocument(lNames[I], lRow) then
        Inc(FDrawn)
      else if lRow.Failed then
        Inc(FErrored)
      else
        Inc(FUntimed);
      AddRow(lRow);
      end;
    SortRows;
    WritePage;
    WriteLn(Format('svgsmil: %d of %d documents drawn, %d with no '
      + 'animation to time, %d in error. The page is %s',
      [FDrawn, FRowCount, FUntimed, FErrored, FPage]));
    if FErrored > 0 then
      ExitCode := 1;
  finally
    lNames.Free;
  end;
end;


var
  Application: TSVGSmilApplication;

begin
  Application := TSVGSmilApplication.Create(nil);
  try
    Application.Title := 'svgsmil';
    Application.Run;
  finally
    Application.Free;
  end;
end.
