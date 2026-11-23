{
  Tests for identifier kinds in the table driven highlighters.
}
unit unittest.tablewords;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.table, syntax.c, syntax.ada, syntax.fortran;

type

  { TWordKindProbe }

  TWordKindProbe = class(TCSyntaxHighlighter)
  protected
    function WordKind(const aValue: string): TSyntaxHighlightKind; override;
  end;

  { TTestTableWords }

  TTestTableWords = class(TTestCase)
  private
    function Highlight(aHighlighter: TSyntaxHighlighter; const aSource: string): TSyntaxTokenArray;
    function Join(const aTokens: TSyntaxTokenArray): string;
    function KindOf(const aTokens: TSyntaxTokenArray; const aText: string): TSyntaxHighlightKind;
  published
    procedure TestOverriddenWordKind;
    procedure TestOverriddenWordKindEventPath;
    procedure TestBaseWordKind;
    procedure TestAdaMixedCase;
    procedure TestFortranMixedCase;
    procedure TestKeywordAtEnd;
  end;

implementation

function TWordKindProbe.WordKind(const aValue: string): TSyntaxHighlightKind;

begin
  if aValue = 'magic' then
    Result := shKey
  else
    Result := inherited WordKind(aValue);
end;


function TTestTableWords.Highlight(aHighlighter: TSyntaxHighlighter; const aSource: string): TSyntaxTokenArray;

begin
  try
    Result := aHighlighter.Execute(aSource);
  finally
    aHighlighter.Free;
  end;
  AssertEquals('tokens recompose ' + aSource, aSource, Join(Result));
end;


function TTestTableWords.Join(const aTokens: TSyntaxTokenArray): string;

var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(aTokens) do
    Result := Result + aTokens[I].Text;
end;


function TTestTableWords.KindOf(const aTokens: TSyntaxTokenArray; const aText: string): TSyntaxHighlightKind;

var
  I: Integer;
begin
  for I := 0 to High(aTokens) do
    if aTokens[I].Text = aText then
      Exit(aTokens[I].Kind);
  Fail('no token ' + aText);
  Result := shInvalid;
end;


procedure TTestTableWords.TestOverriddenWordKind;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := Highlight(TWordKindProbe.Create, 'int magic = other; return');
  AssertEquals('word chosen by the override', shKey.ToString, KindOf(lTokens, 'magic').ToString);
  AssertEquals('builtin type through inherited', shKey.ToString, KindOf(lTokens, 'int').ToString);
  AssertEquals('plain identifier through inherited', shDefault.ToString, KindOf(lTokens, 'other').ToString);
  AssertEquals('keyword at the end through inherited', shKeyword.ToString, KindOf(lTokens, 'return').ToString);
end;


procedure TTestTableWords.TestOverriddenWordKindEventPath;

var
  lHighlighter: TWordKindProbe;
  lCount: Integer;
begin
  lHighlighter := TWordKindProbe.Create;
  try
    lHighlighter.Execute('magic magic', nil);
    lCount := Length(lHighlighter.Execute('magic magic'));
  finally
    lHighlighter.Free;
  end;
  AssertEquals('tokens of the array path after the event path', 3, lCount);
end;


procedure TTestTableWords.TestBaseWordKind;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := Highlight(TCSyntaxHighlighter.Create, 'int magic = other; return');
  AssertEquals('word without override', shDefault.ToString, KindOf(lTokens, 'magic').ToString);
  AssertEquals('builtin type', shKey.ToString, KindOf(lTokens, 'int').ToString);
  AssertEquals('keyword at the end', shKeyword.ToString, KindOf(lTokens, 'return').ToString);
end;


procedure TTestTableWords.TestAdaMixedCase;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := Highlight(TAdaSyntaxHighlighter.Create, 'PROCEDURE P Is BeGiN null; END');
  AssertEquals('upper case keyword', shKeyword.ToString, KindOf(lTokens, 'PROCEDURE').ToString);
  AssertEquals('capitalized keyword', shKeyword.ToString, KindOf(lTokens, 'Is').ToString);
  AssertEquals('mixed case keyword', shKeyword.ToString, KindOf(lTokens, 'BeGiN').ToString);
  AssertEquals('upper case keyword at the end', shKeyword.ToString, KindOf(lTokens, 'END').ToString);
  AssertEquals('identifier', shDefault.ToString, KindOf(lTokens, 'P').ToString);
  lTokens := Highlight(TAdaSyntaxHighlighter.Create, 'X : Integer := 0');
  AssertEquals('mixed case builtin', shKey.ToString, KindOf(lTokens, 'Integer').ToString);
end;


procedure TTestTableWords.TestFortranMixedCase;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := Highlight(TFortranSyntaxHighlighter.Create, 'Program Demo' + #10 + 'INTEGER :: N' + #10 + 'end program');
  AssertEquals('capitalized keyword', shKeyword.ToString, KindOf(lTokens, 'Program').ToString);
  AssertEquals('upper case builtin', shKey.ToString, KindOf(lTokens, 'INTEGER').ToString);
  AssertEquals('lower case keyword at the end', shKeyword.ToString, KindOf(lTokens, 'program').ToString);
  AssertEquals('identifier', shDefault.ToString, KindOf(lTokens, 'Demo').ToString);
end;


procedure TTestTableWords.TestKeywordAtEnd;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := Highlight(TCSyntaxHighlighter.Create, 'return');
  AssertEquals('single keyword', 1, Length(lTokens));
  AssertEquals('single keyword kind', shKeyword.ToString, lTokens[0].Kind.ToString);
  lTokens := Highlight(TCSyntaxHighlighter.Create, 'x = retur');
  AssertEquals('prefix of a keyword at the end', shDefault.ToString, KindOf(lTokens, 'retur').ToString);
  lTokens := Highlight(TCSyntaxHighlighter.Create, 'x = returns');
  AssertEquals('keyword with a suffix at the end', shDefault.ToString, KindOf(lTokens, 'returns').ToString);
end;

initialization
  RegisterTest(TTestTableWords);
end.
