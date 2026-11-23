{
  Tests for the token callback of TSyntaxHighlighter.Execute, token line numbers and SetRange.
}
unit unittest.tokenevents;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Types, fpcunit, testregistry,
  syntax.highlighter, syntax.pascal, syntax.javascript, syntax.html, syntax.barye;

type

  { TFixedTokenHighlighter }

  TFixedTokenHighlighter = class(TSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
    // Emit the parts of aSource separated by '|' as tokens
    procedure DoExecute(const aSource: string); override;
  end;

  { TTestTokenEvents }

  TTestTokenEvents = class(TTestCase)
  private
    FTokens: TSyntaxTokenArray;
    FAbortAt: Integer;
    FSender: TObject;
    procedure DoToken(Sender: TObject; const aToken: TSyntaxToken);
    function Collect(aHighlighter: TSyntaxHighlighter; const aSource: string): TSyntaxTokenArray;
    function Join(const aTokens: TSyntaxTokenArray): string;
    function FindToken(const aTokens: TSyntaxTokenArray; const aText: string): Integer;
  protected
    procedure SetUp; override;
  published
    procedure TestCallbackMatchesExecute;
    procedure TestCallbackSender;
    procedure TestLineNumbersLF;
    procedure TestLineNumbersCRLF;
    procedure TestLineNumbersCR;
    procedure TestNoRangeByDefault;
    procedure TestRange;
    procedure TestRangeExecuteArray;
    procedure TestRangeMultiLineToken;
    procedure TestRangeOpenStart;
    procedure TestRangeOpenEnd;
    procedure TestRangeDisabled;
    procedure TestRangeAfterSource;
    procedure TestRangeStaysSet;
    procedure TestInvalidRange;
    procedure TestAbortInCallback;
    procedure TestJavaScriptRegexBeforeRange;
    procedure TestHtmlEmbeddedLines;
    procedure TestHtmlRangeInsideScript;
    procedure TestHtmlWhitespaceInTags;
    procedure TestBaryeLines;
    procedure TestBaryeRange;
    procedure TestEmitDoesNotChangeEmbeddedToken;
    procedure TestLineEndSplitOverTokens;
    procedure TestRangeWithLineEndAtTokenEnd;
  end;

implementation

class function TFixedTokenHighlighter.GetLanguages: TStringDynArray;

begin
  Result := [];
end;


procedure TFixedTokenHighlighter.DoExecute(const aSource: string);

var
  lPart: string;
begin
  for lPart in aSource.Split(['|']) do
    AddToken(lPart, shDefault);
end;


const
  TenLines =
    'program p;'#10 +
    'var'#10 +
    '  a: Integer;'#10 +
    '  b: Integer;'#10 +
    'begin'#10 +
    '  a := 1;'#10 +
    '  b := 2;'#10 +
    '  a := a + b;'#10 +
    '  writeln(a);'#10 +
    'end.';

procedure TTestTokenEvents.SetUp;

begin
  FTokens := nil;
  FAbortAt := -1;
end;


procedure TTestTokenEvents.DoToken(Sender: TObject; const aToken: TSyntaxToken);

begin
  if Length(FTokens) = FAbortAt then
    Abort;
  FSender := Sender;
  Insert(aToken, FTokens, Length(FTokens));
end;


function TTestTokenEvents.Collect(aHighlighter: TSyntaxHighlighter; const aSource: string): TSyntaxTokenArray;

begin
  FTokens := nil;
  aHighlighter.Execute(aSource, @DoToken);
  Result := FTokens;
end;


function TTestTokenEvents.Join(const aTokens: TSyntaxTokenArray): string;

var
  lToken: TSyntaxToken;
begin
  Result := '';
  for lToken in aTokens do
    Result := Result + lToken.Text;
end;


function TTestTokenEvents.FindToken(const aTokens: TSyntaxTokenArray; const aText: string): Integer;

begin
  Result := High(aTokens);
  while (Result >= 0) and (aTokens[Result].Text <> aText) do
    Dec(Result);
end;


procedure TTestTokenEvents.TestCallbackMatchesExecute;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lArray, lEvents: TSyntaxTokenArray;
  I: Integer;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lArray := lHighlighter.Execute(TenLines);
    lEvents := Collect(lHighlighter, TenLines);
  finally
    lHighlighter.Free;
  end;
  AssertEquals('token count', Length(lArray), Length(lEvents));
  for I := 0 to High(lArray) do
  begin
    AssertEquals('text of token ' + IntToStr(I), lArray[I].Text, lEvents[I].Text);
    AssertEquals('kind of token ' + IntToStr(I), Ord(lArray[I].Kind), Ord(lEvents[I].Kind));
    AssertEquals('line of token ' + IntToStr(I), lArray[I].Line, lEvents[I].Line);
    AssertEquals('categories of token ' + IntToStr(I), lArray[I].CategoriesAsString, lEvents[I].CategoriesAsString);
  end;
  AssertEquals('tokens cover the source', TenLines, Join(lEvents));
end;


procedure TTestTokenEvents.TestCallbackSender;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    Collect(lHighlighter, 'begin end.');
    AssertTrue('tokens received', Length(FTokens) > 0);
    AssertSame('sender is the highlighter', lHighlighter, FSender);
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestLineNumbersLF;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := DoPascalHighlighting(TenLines);
  AssertEquals('first token on line 1', 1, lTokens[0].Line);
  AssertEquals('var on line 2', 2, lTokens[FindToken(lTokens, 'var')].Line);
  AssertEquals('begin on line 5', 5, lTokens[FindToken(lTokens, 'begin')].Line);
  AssertEquals('writeln on line 9', 9, lTokens[FindToken(lTokens, 'writeln')].Line);
  AssertEquals('end on line 10', 10, lTokens[FindToken(lTokens, 'end')].Line);
end;


procedure TTestTokenEvents.TestLineNumbersCRLF;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := DoPascalHighlighting(StringReplace(TenLines, #10, #13#10, [rfReplaceAll]));
  AssertEquals('writeln on line 9', 9, lTokens[FindToken(lTokens, 'writeln')].Line);
  AssertEquals('end on line 10', 10, lTokens[FindToken(lTokens, 'end')].Line);
end;


procedure TTestTokenEvents.TestLineNumbersCR;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := DoPascalHighlighting(StringReplace(TenLines, #10, #13, [rfReplaceAll]));
  AssertEquals('writeln on line 9', 9, lTokens[FindToken(lTokens, 'writeln')].Line);
  AssertEquals('end on line 10', 10, lTokens[FindToken(lTokens, 'end')].Line);
end;


procedure TTestTokenEvents.TestNoRangeByDefault;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    AssertEquals('RangeFrom', -1, lHighlighter.RangeFrom);
    AssertEquals('RangeTo', -1, lHighlighter.RangeTo);
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestRange;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
  lToken: TSyntaxToken;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(3, 5);
    lTokens := Collect(lHighlighter, TenLines);
  finally
    lHighlighter.Free;
  end;
  AssertTrue('tokens received', Length(lTokens) > 0);
  for lToken in lTokens do
    AssertTrue('token "' + lToken.Text + '" on line ' + IntToStr(lToken.Line) + ' starts on line 2..5',
      (lToken.Line >= 2) and (lToken.Line <= 5));
  AssertEquals('whitespace from line 2 into line 3 is the first token', #10'  ', lTokens[0].Text);
  AssertEquals('that whitespace starts on line 2', 2, lTokens[0].Line);
  AssertEquals('program not present', -1, FindToken(lTokens, 'program'));
  AssertEquals('writeln not present', -1, FindToken(lTokens, 'writeln'));
  AssertEquals('text of lines 3 to 5 with the whitespace around them', #10'  a: Integer;'#10'  b: Integer;'#10'begin'#10'  ', Join(lTokens));
end;


procedure TTestTokenEvents.TestRangeExecuteArray;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(9, 9);
    lTokens := lHighlighter.Execute(TenLines);
  finally
    lHighlighter.Free;
  end;
  AssertEquals('text of line 9 with the whitespace around it', #10'  writeln(a);'#10, Join(lTokens));
  AssertEquals('line of writeln', 9, lTokens[FindToken(lTokens, 'writeln')].Line);
end;


procedure TTestTokenEvents.TestRangeMultiLineToken;

const
  Source = 'a := 1;'#10'{ one'#10'two'#10'three }'#10'b := 2;';
var
  lHighlighter: TPascalSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(3, 3);
    lTokens := Collect(lHighlighter, Source);
  finally
    lHighlighter.Free;
  end;
  AssertEquals('one token', 1, Length(lTokens));
  AssertEquals('whole comment', '{ one'#10'two'#10'three }', lTokens[0].Text);
  AssertEquals('comment kind', Ord(shComment), Ord(lTokens[0].Kind));
  AssertEquals('comment starts on line 2', 2, lTokens[0].Line);
end;


procedure TTestTokenEvents.TestRangeOpenStart;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(-1, 2);
    AssertEquals('lines 1 and 2', 'program p;'#10'var'#10'  ', Join(Collect(lHighlighter, TenLines)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestRangeOpenEnd;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(9, -1);
    AssertEquals('lines 9 and 10', #10'  writeln(a);'#10'end.', Join(Collect(lHighlighter, TenLines)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestRangeDisabled;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(2, 3);
    lHighlighter.SetRange(-1, -1);
    AssertEquals('whole source', TenLines, Join(Collect(lHighlighter, TenLines)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestRangeAfterSource;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(50, 60);
    AssertEquals('no tokens', 0, Length(Collect(lHighlighter, TenLines)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestRangeStaysSet;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(5, 5);
    AssertEquals('first run', 'begin'#10'  ', Join(Collect(lHighlighter, TenLines)));
    AssertEquals('second run', 'begin'#10'  ', Join(lHighlighter.Execute(TenLines)));
    AssertEquals('RangeFrom', 5, lHighlighter.RangeFrom);
    AssertEquals('RangeTo', 5, lHighlighter.RangeTo);
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestInvalidRange;

const
  Bad: array[0..4, 0..1] of Integer = ((5, 2), (0, 3), (1, 0), (-2, 4), (2, -5));
var
  lHighlighter: TPascalSyntaxHighlighter;
  I: Integer;
  lRaised: Boolean;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    for I := 0 to High(Bad) do
    begin
      lRaised := False;
      try
        lHighlighter.SetRange(Bad[I, 0], Bad[I, 1]);
      except
        on ESyntaxHighlighter do
          lRaised := True;
      end;
      AssertTrue(Format('SetRange(%d,%d) raises', [Bad[I, 0], Bad[I, 1]]), lRaised);
    end;
    AssertEquals('RangeFrom unchanged after errors', -1, lHighlighter.RangeFrom);
    AssertEquals('RangeTo unchanged after errors', -1, lHighlighter.RangeTo);
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestAbortInCallback;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lRaised: Boolean;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    FAbortAt := 3;
    lRaised := False;
    try
      Collect(lHighlighter, TenLines);
    except
      on EAbort do
        lRaised := True;
    end;
    AssertTrue('EAbort from the callback reaches the caller', lRaised);
    AssertEquals('tokens before the abort', 3, Length(FTokens));
    FAbortAt := -1;
    AssertEquals('next run is complete', TenLines, Join(Collect(lHighlighter, TenLines)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestJavaScriptRegexBeforeRange;

var
  lHighlighter: TJavaScriptSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := TJavaScriptSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(2, 2);
    lTokens := Collect(lHighlighter, 'x = 10'#10'/ 2 / 1;');
  finally
    lHighlighter.Free;
  end;
  AssertEquals('first token on line 2', '/', lTokens[0].Text);
  AssertEquals('division after a number on line 1', Ord(shOperator), Ord(lTokens[0].Kind));
end;


procedure TTestTokenEvents.TestHtmlEmbeddedLines;

const
  Source = '<html>'#10'<script>'#10'var x = 1;'#10'</script>'#10'<style>'#10'p { color: red; }'#10'</style>'#10'</html>';
var
  lTokens: TSyntaxTokenArray;
  lIdx: Integer;
begin
  lTokens := DoHtmlHighlighting(Source);
  AssertEquals('tokens cover the source', Source, Join(lTokens));
  lIdx := FindToken(lTokens, 'var');
  AssertTrue('var found', lIdx >= 0);
  AssertEquals('var on line 3', 3, lTokens[lIdx].Line);
  AssertTrue('var is embedded JavaScript', lTokens[lIdx].HasCategory(TSyntaxHighlighter.GetRegisteredCategoryID('EmbeddedJS')));
  lIdx := FindToken(lTokens, 'color');
  AssertTrue('color found', lIdx >= 0);
  AssertEquals('color on line 6', 6, lTokens[lIdx].Line);
  AssertEquals('closing html on line 8', 8, lTokens[FindToken(lTokens, '</')].Line);
end;


procedure TTestTokenEvents.TestHtmlRangeInsideScript;

const
  Source = '<script>'#10'var a = 1;'#10'var b = 2;'#10'var c = 3;'#10'</script>'#10'<p>x</p>';
var
  lHighlighter: THtmlSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := THtmlSyntaxHighlighter.Create;
  try
    lHighlighter.SetRange(3, 3);
    lTokens := Collect(lHighlighter, Source);
    AssertEquals('text of line 3', 'var b = 2;'#10, Join(lTokens));
    lHighlighter.SetRange(-1, -1);
    AssertEquals('whole source after the range is removed', Source, Join(Collect(lHighlighter, Source)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestHtmlWhitespaceInTags;

const
  Sources: array[0..3] of string = ('a < b', '< x>', '</ x >', 'a < b > c');
var
  lSource: string;
begin
  for lSource in Sources do
    AssertEquals('tokens cover "' + lSource + '"', lSource, Join(DoHtmlHighlighting(lSource)));
end;


procedure TTestTokenEvents.TestBaryeLines;

const
  Source = 'fn a() {'#10'  return 1'#10'}'#10'fn b() {}';
var
  lHighlighter: TSyntaxHighlighter;
  lArray, lEvents: TSyntaxTokenArray;
  I: Integer;
begin
  lHighlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter('barye');
  try
    lArray := lHighlighter.Execute(Source);
    lEvents := Collect(lHighlighter, Source);
  finally
    lHighlighter.Free;
  end;
  AssertEquals('line of return', 2, lEvents[FindToken(lEvents, 'return')].Line);
  AssertEquals('line of b', 4, lEvents[FindToken(lEvents, 'b')].Line);
  AssertEquals('token count of both paths', Length(lArray), Length(lEvents));
  for I := 0 to High(lArray) do
    AssertEquals('line of token ' + IntToStr(I), lArray[I].Line, lEvents[I].Line);
  AssertEquals('tokens cover the source', Source, Join(lEvents));
end;


procedure TTestTokenEvents.TestBaryeRange;

const
  Source = 'fn a() {'#10'  return 1'#10'}'#10'fn b() {}';
var
  lHighlighter: TSyntaxHighlighter;
begin
  lHighlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter('barye');
  try
    lHighlighter.SetRange(2, 3);
    AssertEquals('tokens overlapping lines 2 and 3', #10'  return 1'#10'}'#10, Join(Collect(lHighlighter, Source)));
    AssertEquals('array path with the same range', #10'  return 1'#10'}'#10, Join(lHighlighter.Execute(Source)));
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestEmitDoesNotChangeEmbeddedToken;

const
  Source = '<p>x</p>'#10'<script>'#10'var a = 1;'#10'</script>';
var
  lHighlighter: THtmlSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := THtmlSyntaxHighlighter.Create;
  try
    lTokens := Collect(lHighlighter, Source);
  finally
    lHighlighter.Free;
  end;
  AssertEquals('embedded var has the HTML line', 3, lTokens[FindToken(lTokens, 'var')].Line);
  AssertEquals('tokens cover the source', Source, Join(lTokens));
end;

procedure TTestTokenEvents.TestLineEndSplitOverTokens;

var
  lHighlighter: TFixedTokenHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := TFixedTokenHighlighter.Create;
  try
    lTokens := lHighlighter.Execute('a'#13'|'#10'b|c');
    AssertEquals('token count, CR and LF in two tokens', 3, Length(lTokens));
    AssertEquals('token ending in CR', 1, lTokens[0].Line);
    AssertEquals('token starting with the LF of a CRLF', 2, lTokens[1].Line);
    AssertEquals('token after a CRLF split over two tokens', 2, lTokens[2].Line);
    lTokens := lHighlighter.Execute('a'#10'|'#10'b|c');
    AssertEquals('token ending in LF', 1, lTokens[0].Line);
    AssertEquals('token starting with a second LF', 2, lTokens[1].Line);
    AssertEquals('token after two LFs', 3, lTokens[2].Line);
    lTokens := lHighlighter.Execute('a'#13'|'#13'b|c');
    AssertEquals('token after two CRs', 3, lTokens[2].Line);
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenEvents.TestRangeWithLineEndAtTokenEnd;

var
  lHighlighter: TFixedTokenHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := TFixedTokenHighlighter.Create;
  try
    lHighlighter.SetRange(2, -1);
    lTokens := lHighlighter.Execute('a'#10'|b|c'#10'd');
    AssertEquals('a token whose line end is its last character stays on its line', 2, Length(lTokens));
    AssertEquals('first token in range', 'b', lTokens[0].Text);
    AssertEquals('multi-line token in range', 2, lTokens[1].Line);
  finally
    lHighlighter.Free;
  end;
end;

initialization
  RegisterTest(TTestTokenEvents);
end.
