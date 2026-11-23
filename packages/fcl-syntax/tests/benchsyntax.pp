{
  Throughput benchmark for the fcl-syntax highlighters.
  Usage: benchsyntax <language> <file list> [max MB] [passes]
  The file list names one source file per line; the files are joined in memory.
}
program benchsyntax;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, Generics.Collections, syntax.highlighter,
  syntax.ada, syntax.assembly, syntax.barye, syntax.bash, syntax.basic, syntax.batch, syntax.c, syntax.cpp,
  syntax.csharp, syntax.css, syntax.diff, syntax.dockerfile, syntax.d, syntax.fortran, syntax.go, syntax.html,
  syntax.ini, syntax.java, syntax.javascript, syntax.json, syntax.kotlin, syntax.lfm, syntax.lua,
  syntax.makefile, syntax.markdown, syntax.modula2, syntax.oberon, syntax.pascal, syntax.perl, syntax.php,
  syntax.powershell, syntax.python, syntax.rst, syntax.ruby, syntax.rust, syntax.sql, syntax.swift,
  syntax.toml, syntax.typescript, syntax.xml, syntax.yaml, syntax.zig;

type

  { TTokenCounter }

  TTokenCounter = class
  public
    Count: SizeInt;
    TextLength: SizeInt;
    Checksum: QWord;
    // Count aToken, its text length, and add its kind and length to the checksum
    procedure DoToken(Sender: TObject; const aToken: TSyntaxToken);
  end;

{$push}{$Q-}{$R-}
procedure TTokenCounter.DoToken(Sender: TObject; const aToken: TSyntaxToken);

begin
  Inc(Count);
  Inc(TextLength, Length(aToken.Text));
  Checksum := Checksum * 1099511628211 + QWord(Ord(aToken.Kind)) * 65536 + QWord(Length(aToken.Text));
end;
{$pop}


// Join the files named in aListFile, up to aMaxBytes, and return the number of files used
function LoadCorpus(const aListFile: string; aMaxBytes: SizeInt; out aCorpus: string): Integer;

var
  lList: TStringList;
  lStream: TStringStream;
  lFile: string;
  lParts: TStringBuilder;
begin
  Result := 0;
  lList := TStringList.Create;
  lParts := TStringBuilder.Create;
  try
    lList.LoadFromFile(aListFile);
    for lFile in lList do
      begin
      if (lFile = '') or (lParts.Length >= aMaxBytes) then
        Continue;
      lStream := TStringStream.Create('');
      try
        try
          lStream.LoadFromFile(lFile);
        except
          on E: Exception do
            Continue;
        end;
        lParts.Append(lStream.DataString);
        lParts.Append(#10);
        Inc(Result);
      finally
        lStream.Free;
      end;
      end;
    aCorpus := lParts.ToString;
  finally
    lParts.Free;
    lList.Free;
  end;
end;


// Percentage of aTokens whose text block is also used by another token
function SharedTextPercentage(const aTokens: TSyntaxTokenArray): Double;

var
  lTexts: array of PtrUInt;
  I: Integer;
  lDistinct: SizeInt;
begin
  if Length(aTokens) = 0 then
    Exit(0);
  lTexts := nil;
  SetLength(lTexts, Length(aTokens));
  for I := 0 to High(aTokens) do
    lTexts[I] := PtrUInt(Pointer(aTokens[I].Text));
  specialize TArrayHelper<PtrUInt>.Sort(lTexts);
  lDistinct := 1;
  for I := 1 to High(lTexts) do
    if lTexts[I] <> lTexts[I - 1] then
      Inc(lDistinct);
  Result := 100 * (Length(aTokens) - lDistinct) / Length(aTokens);
end;


// Megabytes per second for aBytes processed in aMs milliseconds
function MBPerSecond(aBytes: SizeInt; aMs: QWord): Double;

begin
  if aMs = 0 then
    aMs := 1;
  Result := (aBytes / (1024 * 1024)) / (aMs / 1000);
end;


var
  lLanguage, lCorpus: string;
  lMaxMB, lPasses, lFiles, lPass: Integer;
  lHighlighter: TSyntaxHighlighter;
  lCounter: TTokenCounter;
  lTokens: TSyntaxTokenArray;
  lStart, lElapsed, lBestArray, lBestEvent, lTotalArray, lTotalEvent: QWord;
  lTokenCount: SizeInt;
  lChecksum: QWord;
  lShared: Double;

begin
  if ParamCount < 2 then
    begin
    Writeln('Usage: benchsyntax <language> <file list> [max MB] [passes]');
    Halt(1);
    end;
  lLanguage := ParamStr(1);
  lMaxMB := StrToIntDef(ParamStr(3), 32);
  lPasses := StrToIntDef(ParamStr(4), 5);
  lFiles := LoadCorpus(ParamStr(2), SizeInt(lMaxMB) * 1024 * 1024, lCorpus);
  lHighlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter(lLanguage);
  lCounter := TTokenCounter.Create;
  try
    lTokens := lHighlighter.Execute(lCorpus);
    lTokenCount := Length(lTokens);
    lShared := SharedTextPercentage(lTokens);
    lTokens := nil;
    lHighlighter.Execute(lCorpus, @lCounter.DoToken);
    if lCounter.TextLength <> Length(lCorpus) then
      Writeln('Warning: token text is ', lCounter.TextLength, ' bytes for a corpus of ', Length(lCorpus), ' bytes');
    lChecksum := lCounter.Checksum;
    lBestArray := High(QWord);
    lBestEvent := High(QWord);
    lTotalArray := 0;
    lTotalEvent := 0;
    for lPass := 1 to lPasses do
      begin
      lStart := GetTickCount64;
      lTokens := lHighlighter.Execute(lCorpus);
      lTokens := nil;
      lElapsed := GetTickCount64 - lStart;
      Inc(lTotalArray, lElapsed);
      if lElapsed < lBestArray then
        lBestArray := lElapsed;
      lStart := GetTickCount64;
      lHighlighter.Execute(lCorpus, @lCounter.DoToken);
      lElapsed := GetTickCount64 - lStart;
      Inc(lTotalEvent, lElapsed);
      if lElapsed < lBestEvent then
        lBestEvent := lElapsed;
      end;
    Writeln(Format('%-12s files %6d  MB %7.2f  tokens %10d  shared %5.1f%%  checksum %.16x  array best %7.1f mean %7.1f MB/s  event best %7.1f mean %7.1f MB/s',
      [lLanguage, lFiles, Length(lCorpus) / (1024 * 1024), lTokenCount, lShared, lChecksum,
       MBPerSecond(Length(lCorpus), lBestArray), MBPerSecond(Length(lCorpus) * lPasses, lTotalArray),
       MBPerSecond(Length(lCorpus), lBestEvent), MBPerSecond(Length(lCorpus) * lPasses, lTotalEvent)]));
  finally
    lCounter.Free;
    lHighlighter.Free;
  end;
end.
