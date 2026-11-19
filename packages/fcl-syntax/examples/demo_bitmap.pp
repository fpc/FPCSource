{
  Renders a highlighted source file to one or more PNG images.
  Usage: demo_bitmap [-w width] [-h height] [-f font] [-s size] [-l language] [--page=N] file
}
program demo_bitmap;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, FPImage, FPCanvas, FPImgCanv, FPWritePNG, FTFont, fpTTF,
  syntax.highlighter, syntax.ada, syntax.assembly, syntax.barye, syntax.bash,
  syntax.basic, syntax.batch, syntax.c, syntax.cpp, syntax.csharp, syntax.css,
  syntax.d, syntax.diff, syntax.dockerfile, syntax.fortran, syntax.go,
  syntax.html, syntax.ini, syntax.java, syntax.javascript, syntax.json,
  syntax.kotlin, syntax.lfm, syntax.lua, syntax.makefile, syntax.markdown,
  syntax.modula2, syntax.oberon, syntax.pascal, syntax.perl, syntax.php,
  syntax.powershell, syntax.python, syntax.rst, syntax.ruby, syntax.rust,
  syntax.sql, syntax.swift, syntax.toml, syntax.typescript, syntax.xml,
  syntax.yaml, syntax.zig;

type
  // A piece of a line with one token kind
  TRun = record
    Text: UnicodeString;
    Kind: TSyntaxHighlightKind;
  end;
  TRunArray = array of TRun;
  TLineArray = array of TRunArray;

const
  TabWidth = 8;
  Margin = 10;
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
  DefaultFont = 'Courier New';
{$ELSE}
  DefaultFont = 'DejaVu Sans Mono';
{$ENDIF}

var
  SourceFile: string = '';
  Language: string = '';
  FontName: string = DefaultFont;
  FontSize: Integer = 10;
  ImageWidth: Integer = 800;
  ImageHeight: Integer = 600;
  Page: Integer = 0;

// Writes the command-line help and stops the program
procedure Usage(const aError: string);

begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' [options] file');
  Writeln('  -w width     Width of the images in pixels (default 800)');
  Writeln('  -h height    Height of the images in pixels (default 600)');
  Writeln('  -f font      Font family name or .ttf file (default ', DefaultFont, ')');
  Writeln('  -s size      Font size in points (default 10)');
  Writeln('  -l language  Language name (default: from the file extension)');
  Writeln('  --page=N     Render only page N (default: all pages)');
  Halt(Ord(aError <> ''));
end;


// Reads the command-line options into the global variables
procedure ParseOptions;

var
  lIdx: Integer;
  lOption: string;
begin
  lIdx := 1;
  while lIdx <= ParamCount do
  begin
    lOption := ParamStr(lIdx);
    if Copy(lOption, 1, 7) = '--page=' then
    begin
      Page := StrToIntDef(Copy(lOption, 8, MaxInt), 0);
      if Page < 1 then
        Usage('Invalid page number in ' + lOption);
    end
    else if (Length(lOption) = 2) and (lOption[1] = '-') then
    begin
      Inc(lIdx);
      if lIdx > ParamCount then
        Usage('Missing value for ' + lOption);
      case lOption of
        '-w': ImageWidth := StrToInt(ParamStr(lIdx));
        '-h': ImageHeight := StrToInt(ParamStr(lIdx));
        '-f': FontName := ParamStr(lIdx);
        '-s': FontSize := StrToInt(ParamStr(lIdx));
        '-l': Language := LowerCase(ParamStr(lIdx));
      else
        Usage('Unknown option ' + lOption);
      end;
    end
    else
      SourceFile := lOption;
    Inc(lIdx);
  end;
  if SourceFile = '' then
    Usage('No source file');
  if not FileExists(SourceFile) then
    Usage('File not found: ' + SourceFile);
end;


// Returns the contents of the file aFileName
function ReadSource(const aFileName: string): string;

var
  lStream: TFileStream;
begin
  Result := '';
  lStream := TFileStream.Create(aFileName, fmOpenRead or fmShareDenyWrite);
  try
    SetLength(Result, lStream.Size);
    if Length(Result) > 0 then
      lStream.ReadBuffer(Result[1], Length(Result));
  finally
    lStream.Free;
  end;
end;


// Returns the highlighter language for aFileName
function LanguageFromFileName(const aFileName: string): string;

begin
  Result := LowerCase(Copy(ExtractFileExt(aFileName), 2, MaxInt));
  case Result of
    'pp', 'pas', 'p', 'lpr', 'dpr', 'inc': Result := 'pascal';
    'h', 'cxx': Result := 'cpp';
  end;
  if ExtractFileName(aFileName) = 'Makefile' then
    Result := 'makefile';
end;


// Splits lines aFrom..aTo of aSource in tokens; one plain token if there is no highlighter for aLanguage
function Highlight(const aLanguage, aSource: string; aFrom, aTo: Integer): TSyntaxTokenArray;

var
  lHighlighter: TSyntaxHighlighter;
begin
  if TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(aLanguage) = nil then
  begin
    Writeln('No highlighter for "', aLanguage, '", using plain text');
    Result := [TSyntaxToken.Create(aSource, shDefault)];
    Result[0].Line := 1;
    Exit;
  end;
  lHighlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter(aLanguage);
  try
    lHighlighter.SetRange(aFrom, aTo);
    Result := lHighlighter.Execute(aSource);
  finally
    lHighlighter.Free;
  end;
end;


// Finds the source lines aFrom..aTo on page aPage, and the rows of line aFrom that are on earlier pages
function FindPageLines(const aSource: string; aColumns, aRows, aPage: Integer;
  out aFrom, aTo, aSkipRows: Integer): Boolean;

var
  lText: UnicodeString;
  lIdx, lLine, lColumn, lLineRows, lRow, lStartRow: Integer;

  procedure AddColumn;

  begin
    if lColumn >= aColumns then
    begin
      Inc(lLineRows);
      lColumn := 0;
    end;
    Inc(lColumn);
  end;

begin
  Result := False;
  aFrom := -1;
  aTo := -1;
  aSkipRows := 0;
  lText := UTF8Decode(aSource);
  lStartRow := (aPage - 1) * aRows;
  lLine := 1;
  lRow := 0;
  lColumn := 0;
  lLineRows := 1;
  lIdx := 1;
  while True do
  begin
    if (lIdx > Length(lText)) or (lText[lIdx] in [#10, #13]) then
    begin
      if (aFrom = -1) and (lRow + lLineRows > lStartRow) then
      begin
        aFrom := lLine;
        aSkipRows := lStartRow - lRow;
      end;
      Inc(lRow, lLineRows);
      if (aFrom <> -1) and ((lRow >= lStartRow + aRows) or (lIdx > Length(lText))) then
      begin
        aTo := lLine;
        Exit(True);
      end;
      if lIdx > Length(lText) then
        Exit;
      if (lText[lIdx] = #13) and (lIdx < Length(lText)) and (lText[lIdx + 1] = #10) then
        Inc(lIdx);
      Inc(lLine);
      lColumn := 0;
      lLineRows := 1;
    end
    else if lText[lIdx] = #9 then
      repeat
        AddColumn;
      until lColumn mod TabWidth = 0
    else
      AddColumn;
    Inc(lIdx);
  end;
end;


// Cuts aTokens from source line aFirstLine on in lines of at most aColumns characters, with tabs expanded
function LayoutTokens(const aTokens: TSyntaxTokenArray; aColumns, aFirstLine: Integer): TLineArray;

var
  lLines: TLineArray;
  lCount, lColumn, lLine: Integer;
  lAfterCR: Boolean;

  procedure NewLine;

  begin
    if lCount = Length(lLines) then
      SetLength(lLines, lCount * 2 + 16);
    lLines[lCount] := nil;
    Inc(lCount);
    lColumn := 0;
  end;

  procedure AddChar(aChar: UnicodeChar; aKind: TSyntaxHighlightKind);

  var
    lRuns: Integer;
  begin
    if lColumn >= aColumns then
      NewLine;
    lRuns := Length(lLines[lCount - 1]);
    if (lRuns > 0) and (lLines[lCount - 1][lRuns - 1].Kind = aKind) then
      lLines[lCount - 1][lRuns - 1].Text := lLines[lCount - 1][lRuns - 1].Text + aChar
    else
    begin
      SetLength(lLines[lCount - 1], lRuns + 1);
      lLines[lCount - 1][lRuns].Text := aChar;
      lLines[lCount - 1][lRuns].Kind := aKind;
    end;
    Inc(lColumn);
  end;

var
  lToken: TSyntaxToken;
  lText: UnicodeString;
  lChar: UnicodeChar;
begin
  lLines := nil;
  lCount := 0;
  NewLine;
  lLine := 1;
  if (Length(aTokens) > 0) and (aTokens[0].Line > 0) then
    lLine := aTokens[0].Line;
  lAfterCR := False;
  for lToken in aTokens do
  begin
    lText := UTF8Decode(lToken.Text);
    for lChar in lText do
    begin
      if (lChar = #13) or ((lChar = #10) and not lAfterCR) then
      begin
        if lLine >= aFirstLine then
          NewLine;
        Inc(lLine);
      end
      else if (lChar <> #10) and (lLine >= aFirstLine) then
      begin
        if lChar = #9 then
          repeat
            AddChar(' ', lToken.Kind);
          until lColumn mod TabWidth = 0
        else
          AddChar(lChar, lToken.Kind);
      end;
      lAfterCR := lChar = #13;
    end;
  end;
  SetLength(lLines, lCount);
  Result := lLines;
end;


// Returns the colour for a token of kind aKind
function KindColor(aKind: TSyntaxHighlightKind): TFPColor;

const
  Colors: array[TSyntaxHighlightKind] of Cardinal = (
    $000000, // shDefault
    $D00000, // shInvalid
    $505050, // shSymbol
    $0000C0, // shKeyword
    $007F00, // shComment
    $7F007F, // shDirective
    $00707F, // shNumbers
    $A31515, // shCharacters
    $A31515, // shStrings
    $6F4E37, // shAssembler
    $811F3F, // shRegex
    $C04000, // shInterpolation
    $C04000, // shEscape
    $A31515, // shRawString
    $A31515, // shTemplate
    $505050, // shLambda
    $505050, // shOperator
    $D00000, // shError
    $B06000, // shWarning
    $0050A0, // shAttribute
    $00007F, // shSection
    $0050A0  // shKey
  );

var
  lRGB: Cardinal;
begin
  lRGB := Colors[aKind];
  Result.Red := ((lRGB shr 16) and $FF) * $101;
  Result.Green := ((lRGB shr 8) and $FF) * $101;
  Result.Blue := (lRGB and $FF) * $101;
  Result.Alpha := alphaOpaque;
end;


// Returns the file name of the TrueType font aFont, which is a file or a family name
function FindFontFile(const aFont: string): string;

var
  lItem: TFPFontCacheItem;
begin
  if FileExists(aFont) then
    Exit(aFont);
  gTTFontCache.ReadStandardFonts;
  gTTFontCache.BuildFontCacheIgnoresErrors := True;
  gTTFontCache.BuildFontCache;
  lItem := gTTFontCache.Find(aFont, False, False);
  if lItem = nil then
    Usage('Font not found: ' + aFont);
  Result := lItem.FileName;
end;


// Draws aSource (all pages, or only page Page) on images of ImageWidth x ImageHeight and saves them as PNG files
procedure RenderImages(const aSource, aFontFile: string);

var
  lImage: TFPMemoryImage;
  lCanvas: TFPImageCanvas;
  lFont: TFreeTypeFont;
  lLines: TLineArray;
  lRun: TRun;
  lCharWidth, lLineHeight, lColumns, lRows, lFirst, lRow, lColumn, lPage: Integer;
  lFromLine, lToLine, lSkipRows: Integer;
  lBaseName, lFileName: string;
begin
  lImage := TFPMemoryImage.Create(ImageWidth, ImageHeight);
  lCanvas := TFPImageCanvas.Create(lImage);
  lFont := TFreeTypeFont.Create;
  try
    lFont.Name := aFontFile;
    lFont.Size := FontSize;
    lCanvas.Font := lFont;
    lCanvas.TextOrigin := toTop;
    lCanvas.TextMeasure := tmFont;
    lCharWidth := lCanvas.GetTextWidth('M');
    lLineHeight := lCanvas.GetTextHeight('Mg');
    lColumns := (ImageWidth - 2 * Margin) div lCharWidth;
    lRows := (ImageHeight - 2 * Margin) div lLineHeight;
    if (lColumns < 1) or (lRows < 1) then
      Usage('The image is too small for this font size');
    lFromLine := -1;
    lToLine := -1;
    lSkipRows := 0;
    if (Page > 0) and not FindPageLines(aSource, lColumns, lRows, Page, lFromLine, lToLine, lSkipRows) then
      Usage(Format('Page %d does not exist', [Page]));
    lLines := LayoutTokens(Highlight(Language, aSource, lFromLine, lToLine), lColumns, lFromLine);
    lPage := 0;
    if Page > 0 then
    begin
      lLines := Copy(lLines, lSkipRows, lRows);
      lPage := Page - 1;
    end;
    lBaseName := ChangeFileExt(ExtractFileName(SourceFile), '');
    lFirst := 0;
    while lFirst < Length(lLines) do
    begin
      Inc(lPage);
      lCanvas.Brush.FPColor := colWhite;
      lCanvas.Brush.Style := bsSolid;
      lCanvas.FillRect(0, 0, ImageWidth, ImageHeight);
      for lRow := 0 to lRows - 1 do
      begin
        if lFirst + lRow >= Length(lLines) then
          Break;
        lColumn := 0;
        for lRun in lLines[lFirst + lRow] do
        begin
          lFont.FPColor := KindColor(lRun.Kind);
          lCanvas.TextOut(Margin + lColumn * lCharWidth, Margin + lRow * lLineHeight, lRun.Text);
          Inc(lColumn, Length(lRun.Text));
        end;
      end;
      lFileName := Format('%s-%.3d.png', [lBaseName, lPage]);
      lImage.SaveToFile(lFileName);
      Writeln('Wrote ', lFileName);
      Inc(lFirst, lRows);
    end;
  finally
    lCanvas.Free;
    lFont.Free;
    lImage.Free;
  end;
end;


begin
  ParseOptions;
  if Language = '' then
    Language := LanguageFromFileName(SourceFile);
  InitEngine;
  RenderImages(ReadSource(SourceFile), FindFontFile(FontName));
end.
