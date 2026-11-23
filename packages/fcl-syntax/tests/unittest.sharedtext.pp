{
  Tests for the shared token texts of TSyntaxHighlighter.SourceText.
}
unit unittest.sharedtext;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.pascal, syntax.c;

type

  { TSharedTextProbe }

  TSharedTextProbe = class(TPascalSyntaxHighlighter)
  public
    // Call the protected SourceText
    function Probe(const aSource: string; aStart, aLength: SizeInt): string;
  end;

  { TTestSharedText }

  TTestSharedText = class(TTestCase)
  private
    function TokenWithText(const aTokens: TSyntaxTokenArray; const aText: string; aOccurrence: Integer): Integer;
  published
    procedure TestIdenticalTokensShareText;
    procedure TestTableEngineSharesText;
    procedure TestLongTextNotShared;
    procedure TestModifyingOneTokenText;
    procedure TestSourceTextValues;
    procedure TestSlotCollisions;
    procedure TestCodePageOfSource;
    procedure TestEventPathDoesNotShare;
    procedure TestShareTextsResetAfterExecute;
    procedure TestCodePageWithoutSharing;
    procedure TestTokenTextReleasedAfterExecute;
    procedure TestRangeStopThenFullExecute;
  end;

  { TTokenCollector }

  TTokenCollector = class
  public
    Tokens: TSyntaxTokenArray;
    // Append aToken to Tokens
    procedure DoToken(Sender: TObject; const aToken: TSyntaxToken);
  end;

implementation

procedure TTokenCollector.DoToken(Sender: TObject; const aToken: TSyntaxToken);

begin
  Insert(aToken, Tokens, Length(Tokens));
end;


function TSharedTextProbe.Probe(const aSource: string; aStart, aLength: SizeInt): string;

begin
  ShareTexts := True;
  Result := SourceText(aSource, aStart, aLength);
end;


function TTestSharedText.TokenWithText(const aTokens: TSyntaxTokenArray; const aText: string; aOccurrence: Integer): Integer;

var
  I: Integer;
begin
  for I := 0 to High(aTokens) do
    if aTokens[I].Text = aText then
      begin
      Dec(aOccurrence);
      if aOccurrence = 0 then
        Exit(I);
      end;
  Result := -1;
end;


procedure TTestSharedText.TestIdenticalTokensShareText;

var
  lTokens: TSyntaxTokenArray;
  lFirst, lSecond: Integer;
begin
  lTokens := DoPascalHighlighting('begin Result := Result; end;');
  lFirst := TokenWithText(lTokens, 'Result', 1);
  lSecond := TokenWithText(lTokens, 'Result', 2);
  AssertTrue('two Result tokens', (lFirst >= 0) and (lSecond >= 0));
  AssertTrue('identical identifiers share one text block', Pointer(lTokens[lFirst].Text) = Pointer(lTokens[lSecond].Text));
  lFirst := TokenWithText(lTokens, ';', 1);
  lSecond := TokenWithText(lTokens, ';', 2);
  AssertTrue('identical symbols share one text block', Pointer(lTokens[lFirst].Text) = Pointer(lTokens[lSecond].Text));
end;


procedure TTestSharedText.TestTableEngineSharesText;

var
  lHighlighter: TSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
  lFirst, lSecond: Integer;
begin
  lHighlighter := TSyntaxHighlighterRegistry.Instance.CreateSyntaxHighlighter('c');
  try
    lTokens := lHighlighter.Execute('int a; int b;');
  finally
    lHighlighter.Free;
  end;
  lFirst := TokenWithText(lTokens, 'int', 1);
  lSecond := TokenWithText(lTokens, 'int', 2);
  AssertTrue('two int tokens', (lFirst >= 0) and (lSecond >= 0));
  AssertTrue('identical keywords share one text block', Pointer(lTokens[lFirst].Text) = Pointer(lTokens[lSecond].Text));
end;


procedure TTestSharedText.TestLongTextNotShared;

const
  LongName = 'AVeryLongIdentifierName';
var
  lTokens: TSyntaxTokenArray;
  lFirst, lSecond: Integer;
begin
  lTokens := DoPascalHighlighting(LongName + ' ' + LongName);
  lFirst := TokenWithText(lTokens, LongName, 1);
  lSecond := TokenWithText(lTokens, LongName, 2);
  AssertTrue('two long identifiers', (lFirst >= 0) and (lSecond >= 0));
  AssertTrue('texts over 16 bytes are separate copies', Pointer(lTokens[lFirst].Text) <> Pointer(lTokens[lSecond].Text));
end;


procedure TTestSharedText.TestModifyingOneTokenText;

var
  lTokens: TSyntaxTokenArray;
  lFirst, lSecond: Integer;
begin
  lTokens := DoPascalHighlighting('x := x;');
  lFirst := TokenWithText(lTokens, 'x', 1);
  lSecond := TokenWithText(lTokens, 'x', 2);
  lTokens[lFirst].Text[1] := 'y';
  AssertEquals('modified token', 'y', lTokens[lFirst].Text);
  AssertEquals('other token keeps its text', 'x', lTokens[lSecond].Text);
end;


procedure TTestSharedText.TestSourceTextValues;

var
  lProbe: TSharedTextProbe;
  lFirst, lSecond: string;
begin
  lProbe := TSharedTextProbe.Create;
  try
    AssertEquals('middle of the source', 'bcd', lProbe.Probe('abcdef', 2, 3));
    AssertEquals('end of the source', 'ef', lProbe.Probe('abcdef', 5, 2));
    AssertEquals('length beyond the end', 'ef', lProbe.Probe('abcdef', 5, 10));
    AssertEquals('zero length', '', lProbe.Probe('abcdef', 2, 0));
    AssertEquals('start before the source', 'ab', lProbe.Probe('abcdef', 0, 2));
    AssertEquals('long text', 'abcdefghijklmnopqrstuvwxyz', lProbe.Probe('abcdefghijklmnopqrstuvwxyz', 1, 26));
    lFirst := lProbe.Probe('foo bar', 1, 3);
    lSecond := lProbe.Probe('xx foo', 4, 3);
    AssertEquals('same text from another source', 'foo', lSecond);
    AssertTrue('same text from another source is shared', Pointer(lFirst) = Pointer(lSecond));
  finally
    lProbe.Free;
  end;
end;


procedure TTestSharedText.TestSlotCollisions;

var
  lProbe: TSharedTextProbe;
  lSource: string;
  I: Integer;
begin
  lSource := '';
  for I := 0 to 4999 do
    lSource := lSource + Format('%.5d', [I]);
  lProbe := TSharedTextProbe.Create;
  try
    for I := 0 to 4999 do
      AssertEquals('text ' + IntToStr(I), Format('%.5d', [I]), lProbe.Probe(lSource, I * 5 + 1, 5));
    for I := 4999 downto 0 do
      AssertEquals('text ' + IntToStr(I) + ' again', Format('%.5d', [I]), lProbe.Probe(lSource, I * 5 + 1, 5));
  finally
    lProbe.Free;
  end;
end;


procedure TTestSharedText.TestCodePageOfSource;

var
  lProbe: TSharedTextProbe;
  lRaw: RawByteString;
  lUTF8, lLatin: string;
begin
  lRaw := 'word';
  SetCodePage(lRaw, CP_UTF8, False);
  lUTF8 := lRaw;
  lRaw := 'word';
  SetCodePage(lRaw, 1252, False);
  lLatin := lRaw;
  AssertTrue('sources have different code pages', StringCodePage(lUTF8) <> StringCodePage(lLatin));
  lProbe := TSharedTextProbe.Create;
  try
    AssertEquals('code page of the first text', StringCodePage(lUTF8), StringCodePage(lProbe.Probe(lUTF8, 1, 4)));
    AssertEquals('code page of the same text from another code page', StringCodePage(lLatin), StringCodePage(lProbe.Probe(lLatin, 1, 4)));
    AssertEquals('code page after switching back', StringCodePage(lUTF8), StringCodePage(lProbe.Probe(lUTF8, 1, 4)));
  finally
    lProbe.Free;
  end;
end;

procedure TTestSharedText.TestEventPathDoesNotShare;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lCollector: TTokenCollector;
  lFirst, lSecond: Integer;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  lCollector := TTokenCollector.Create;
  try
    lHighlighter.Execute('x := x;', @lCollector.DoToken);
    lFirst := TokenWithText(lCollector.Tokens, 'x', 1);
    lSecond := TokenWithText(lCollector.Tokens, 'x', 2);
    AssertTrue('two x tokens', (lFirst >= 0) and (lSecond >= 0));
    AssertTrue('event tokens have their own text', Pointer(lCollector.Tokens[lFirst].Text) <> Pointer(lCollector.Tokens[lSecond].Text));
  finally
    lCollector.Free;
    lHighlighter.Free;
  end;
end;


procedure TTestSharedText.TestShareTextsResetAfterExecute;

var
  lProbe: TSharedTextProbe;
  lCollector: TTokenCollector;
  lFirst, lSecond: Integer;
begin
  lProbe := TSharedTextProbe.Create;
  lCollector := TTokenCollector.Create;
  try
    lProbe.Execute('x := x;');
    lProbe.Execute('x := x;', @lCollector.DoToken);
    lFirst := TokenWithText(lCollector.Tokens, 'x', 1);
    lSecond := TokenWithText(lCollector.Tokens, 'x', 2);
    AssertTrue('event path after the array path does not share', Pointer(lCollector.Tokens[lFirst].Text) <> Pointer(lCollector.Tokens[lSecond].Text));
  finally
    lCollector.Free;
    lProbe.Free;
  end;
end;

procedure TTestSharedText.TestCodePageWithoutSharing;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lCollector: TTokenCollector;
  lRaw: RawByteString;
  lLatin: string;
begin
  lRaw := 'word := other';
  SetCodePage(lRaw, 1252, False);
  lLatin := lRaw;
  lHighlighter := TPascalSyntaxHighlighter.Create;
  lCollector := TTokenCollector.Create;
  try
    lHighlighter.Execute(lLatin, @lCollector.DoToken);
    AssertTrue('tokens', Length(lCollector.Tokens) > 0);
    AssertEquals('code page of an event token', 1252, StringCodePage(lCollector.Tokens[0].Text));
    AssertEquals('text of an event token', 'word', lCollector.Tokens[0].Text);
  finally
    lCollector.Free;
    lHighlighter.Free;
  end;
end;


procedure TTestSharedText.TestTokenTextReleasedAfterExecute;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lCollector: TTokenCollector;
  lLast: Integer;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  lCollector := TTokenCollector.Create;
  try
    lHighlighter.Execute('x := LastIdentifierOfTheSource', @lCollector.DoToken);
    lLast := High(lCollector.Tokens);
    AssertEquals('last token', 'LastIdentifierOfTheSource', lCollector.Tokens[lLast].Text);
    AssertEquals('the highlighter holds no reference to the last token text', 1, StringRefCount(lCollector.Tokens[lLast].Text));
  finally
    lCollector.Free;
    lHighlighter.Free;
  end;
end;


procedure TTestSharedText.TestRangeStopThenFullExecute;

const
  Source = 'a := 1;'#10'b := 2;'#10'c := 3;';
var
  lHighlighter: TPascalSyntaxHighlighter;
  lCollector: TTokenCollector;
  lTokens: TSyntaxTokenArray;
  lJoined: string;
  I: Integer;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  lCollector := TTokenCollector.Create;
  try
    lHighlighter.SetRange(1, 1);
    lTokens := lHighlighter.Execute(Source);
    AssertTrue('range stop returns tokens', Length(lTokens) > 0);
    AssertEquals('range stop ends on line 1', 1, lTokens[High(lTokens) - 1].Line);
    lHighlighter.SetRange(-1, -1);
    lTokens := lHighlighter.Execute(Source);
    lJoined := '';
    for I := 0 to High(lTokens) do
      lJoined := lJoined + lTokens[I].Text;
    AssertEquals('array path after a range stop covers the source', Source, lJoined);
    lHighlighter.Execute(Source, @lCollector.DoToken);
    lJoined := '';
    for I := 0 to High(lCollector.Tokens) do
      lJoined := lJoined + lCollector.Tokens[I].Text;
    AssertEquals('event path after the array path covers the source', Source, lJoined);
  finally
    lCollector.Free;
    lHighlighter.Free;
  end;
end;

initialization
  RegisterTest(TTestSharedText);
end.
