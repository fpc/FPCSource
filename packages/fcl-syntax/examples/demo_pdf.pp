{
  Renders a highlighted source file to a PDF document with A4 pages.
  Usage: demo_pdf [-f font] [-s size] [-l language] file
}
program demo_pdf;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpPDF, fpTTF,
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
  // Page size and margin in millimetres
  PageWidth = 210;
  PageHeight = 297;
  Margin = 15;
  MMPerPoint = 25.4 / 72;
{$IF DEFINED(WINDOWS) OR DEFINED(DARWIN)}
  DefaultFont = 'Courier New';
{$ELSE}
  DefaultFont = 'DejaVu Sans Mono';
{$ENDIF}

var
  SourceFile: string = '';
  Language: string = '';
  FontName: string = DefaultFont;
  FontSize: Integer = 9;

// Writes the command-line help and stops the program
procedure Usage(const aError: string);

begin
  if aError <> '' then
    Writeln('Error: ', aError);
  Writeln('Usage: ', ExtractFileName(ParamStr(0)), ' [options] file');
  Writeln('  -f font      Font family name or .ttf file (default ', DefaultFont, ')');
  Writeln('  -s size      Font size in points (default 9)');
  Writeln('  -l language  Language name (default: from the file extension)');
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
    if (Length(lOption) = 2) and (lOption[1] = '-') then
    begin
      Inc(lIdx);
      if lIdx > ParamCount then
        Usage('Missing value for ' + lOption);
      case lOption of
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


// Splits aSource in tokens; one plain token if there is no highlighter for aLanguage
function Highlight(const aLanguage, aSource: string): TSyntaxTokenArray;

var
  lHighlighter: TSyntaxHighlighter;
begin
  if TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(aLanguage) = nil then
  begin
    Writeln('No highlighter for "', aLanguage, '", using plain text');
    Result := [TSyntaxToken.Create(aSource, shDefault)];
    Exit;
  end;
  lHighlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter(aLanguage);
  try
    Result := lHighlighter.Execute(aSource);
  finally
    lHighlighter.Free;
  end;
end;


// Cuts aTokens in lines of at most aColumns characters, with tabs expanded
function LayoutTokens(const aTokens: TSyntaxTokenArray; aColumns: Integer): TLineArray;

var
  lLines: TLineArray;
  lCount, lColumn: Integer;

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
  for lToken in aTokens do
  begin
    lText := UTF8Decode(lToken.Text);
    for lChar in lText do
      case lChar of
        #13: ;
        #10: NewLine;
        #9:
          repeat
            AddChar(' ', lToken.Kind);
          until lColumn mod TabWidth = 0;
      else
        AddChar(lChar, lToken.Kind);
      end;
  end;
  SetLength(lLines, lCount);
  Result := lLines;
end;


// Returns the RGB colour for a token of kind aKind
function KindColor(aKind: TSyntaxHighlightKind): TARGBColor;

const
  Colors: array[TSyntaxHighlightKind] of TARGBColor = (
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
    $0000C0, // shSection
    $0050A0  // shKey
  );

begin
  Result := Colors[aKind];
end;


// Returns the font cache entry of aFont, which is a .ttf file or a family name
function FindFont(const aFont: string): TFPFontCacheItem;

begin
  if FileExists(aFont) then
  begin
    Result := TFPFontCacheItem.Create(aFont);
    gTTFontCache.Add(Result);
    Exit;
  end;
  gTTFontCache.ReadStandardFonts;
  gTTFontCache.BuildFontCacheIgnoresErrors := True;
  gTTFontCache.BuildFontCache;
  Result := gTTFontCache.Find(aFont, False, False);
  if Result = nil then
    Usage('Font not found: ' + aFont);
end;


// Writes aTokens on as many A4 pages as needed and saves them as a PDF file
procedure RenderPDF(const aTokens: TSyntaxTokenArray; aFont: TFPFontCacheItem);

var
  lDoc: TPDFDocument;
  lSection: TPDFSection;
  lPage: TPDFPage;
  lLines: TLineArray;
  lRun: TRun;
  lFontIdx, lColumns, lRows, lFirst, lRow, lColumn: Integer;
  lCharWidth, lLineHeight: Double;
  lFileName: string;
begin
  lCharWidth := aFont.TextWidth(StringOfChar('M', 100), FontSize) / 100 * 25.4 / gTTFontCache.DPI;
  lLineHeight := FontSize * 1.2 * MMPerPoint;
  lColumns := Trunc((PageWidth - 2 * Margin) / lCharWidth);
  lRows := Trunc((PageHeight - 2 * Margin) / lLineHeight);
  if (lColumns < 1) or (lRows < 1) then
    Usage('The page is too small for this font size');
  lLines := LayoutTokens(aTokens, lColumns);
  lDoc := TPDFDocument.Create(nil);
  try
    lDoc.Options := [poPageOriginAtTop, poSubsetFont, poCompressFonts, poCompressText];
    lDoc.DefaultPaperType := ptA4;
    lDoc.DefaultOrientation := ppoPortrait;
    lDoc.DefaultUnitOfMeasure := uomMillimeters;
    lDoc.StartDocument;
    lSection := lDoc.Sections.AddSection;
    lFontIdx := lDoc.AddFont(aFont.FileName, 'SourceFont');
    Writeln('Using font ', aFont.FileName);
    lFirst := 0;
    while lFirst < Length(lLines) do
    begin
      lPage := lDoc.Pages.AddPage;
      lSection.AddPage(lPage);
      lPage.SetFont(lFontIdx, FontSize);
      for lRow := 0 to lRows - 1 do
      begin
        if lFirst + lRow >= Length(lLines) then
          Break;
        lColumn := 0;
        for lRun in lLines[lFirst + lRow] do
        begin
          lPage.SetColor(KindColor(lRun.Kind), False);
          lPage.WriteText(Margin + lColumn * lCharWidth, Margin + (lRow + 1) * lLineHeight, UTF8Encode(lRun.Text));
          Inc(lColumn, Length(lRun.Text));
        end;
      end;
      Inc(lFirst, lRows);
    end;
    lFileName := ChangeFileExt(ExtractFileName(SourceFile), '.pdf');
    lDoc.SaveToFile(lFileName);
    Writeln('Wrote ', lFileName, ' with ', lDoc.Pages.Count, ' pages');
  finally
    lDoc.Free;
  end;
end;


begin
  ParseOptions;
  if Language = '' then
    Language := LanguageFromFileName(SourceFile);
  RenderPDF(Highlight(Language, ReadSource(SourceFile)), FindFont(FontName));
end.
