unit utcExprParsScanner;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, punit, math, fpexprpars;

procedure RegisterTests(aTop : PSuite);

implementation

uses typinfo;

var
  FP: TFPExpressionScanner;
  FInvalidString: String;

function TestExpressionScanner_SetUp : string;
begin
  Result:='';
  FP := TFPExpressionScanner.Create;

end;

function TestExpressionScanner_TearDown : string;
begin
  Result:='';
  FreeAndNil(FP);
end;

procedure AssertEqualsToken(Msg: String; AExpected, AActual: TTokenType);
var
  S1, S2: String;
begin
  S1 := TokenName(AExpected);
  S2 := GetEnumName(TypeInfo(TTokenType), Ord(AActual));
  AssertEquals(Msg, S1, S2);
end;

procedure TestString(const AString: String; AToken: TTokenType);
begin
  FP.Source := AString;
  AssertEqualsToken('String "' + AString + '" results in token ' + TokenName(AToken), AToken, FP.GetToken);
  if not (FP.TokenType in [ttString, ttEOF]) then
    AssertEquals('String "' + AString + '" results in token string ' + TokenName(AToken), AString, FP.Token)
  else if FP.TokenType = ttString then
    AssertEquals('String "' + AString + '" results in token string ' + TokenName(AToken),
      StringReplace(AString, '''''', '''', [rfReplaceAll]),
      '''' + FP.Token + '''');
end;

function TestExpressionScanner_TestCreate: TTestString;
begin
  Result := '';
  AssertEquals('Empty source', '', FP.Source);
  AssertEquals('Pos is zero', 0, FP.Pos);
  AssertEquals('CurrentChar is zero', #0, FP.CurrentChar);
  AssertEqualsToken('Current token type is EOF', ttEOF, FP.TokenType);
  AssertEquals('Current token is empty', '', FP.Token);
end;

function TestExpressionScanner_TestSetSource: TTestString;
begin
  Result := '';
  FP.Source := 'Abc';
  FP.Source := '';
  AssertEquals('Empty source', '', FP.Source);
  AssertEquals('Pos is zero', 0, FP.Pos);
  AssertEquals('CurrentChar is zero', #0, FP.CurrentChar);
  AssertEqualsToken('Current token type is EOF', ttEOF, FP.TokenType);
  AssertEquals('Current token is empty', '', FP.Token);
end;

function TestExpressionScanner_TestWhiteSpace: TTestString;
begin
  Result := '';
  TestString('  ', ttEOF);
end;

function TestExpressionScanner_TestTokens: TTestString;
const
  TestStrings: array[TTokenType] of String =
    ('+', '-', '<', '>', '=', '/',
    'mod', '*', '(', ')', '<=',
    '>=', '<>', '1', '''abc''', 'abc',
    ',', 'and', 'or', 'xor', 'true', 'false', 'not',
    'if', 'case', '^', '');
var
  t: TTokenType;
begin
  Result := '';
  for t := Low(TTokenType) to High(TTokenType) do
    TestString(TestStrings[t], t);
end;

procedure DoInvalidNumber(AString: String);
begin
  FInvalidString := AString;
  FP.Source := AString;
  while FP.GetToken <> ttEOF do;
end;

var
  TestProcToRun: TTestRunProc;

function RunTestProc: TTestString;
begin
  Result := '';
  if Assigned(TestProcToRun) then
    TestProcToRun;
end;

procedure DoTestInvalidNumberGG;
begin
  DoInvalidNumber('$GG');
end;

procedure DoTestInvalidNumber88;
begin
  DoInvalidNumber('&88');
end;

procedure DoTestInvalidNumber22;
begin
  DoInvalidNumber('%22');
end;

procedure DoTestInvalidNumber11;
begin
  DoInvalidNumber('1..1');
end;

procedure DoTestInvalidNumber1E;
begin
  DoInvalidNumber('1.E--1');
end;

function TestExpressionScanner_TestNumber: TTestString;
begin
  Result := '';
  TestString('123', ttNumber);
  TestString('$FF', ttNumber);
  TestString('&77', ttNumber);
  TestString('%11111111', ttNumber);
  TestString('123.4', ttNumber);
  TestString('123.E4', ttNumber);
  TestString('1.E4', ttNumber);
  TestString('1e-2', ttNumber);
  TestProcToRun := @DoTestInvalidNumberGG;
  AssertException('Invalid number "$GG"', EExprScanner, @RunTestProc);
  TestProcToRun := @DoTestInvalidNumber88;
  AssertException('Invalid number "&88"', EExprScanner, @RunTestProc);
  TestProcToRun := @DoTestInvalidNumber22;
  AssertException('Invalid number "%22"', EExprScanner, @RunTestProc);
  TestProcToRun := @DoTestInvalidNumber11;
  AssertException('Invalid number "1..1"', EExprScanner, @RunTestProc);
  TestProcToRun := @DoTestInvalidNumber1E;
  AssertException('Invalid number "1.E--1"', EExprScanner, @RunTestProc);
end;

procedure DoTestInvalidCharTilde;
begin
  DoInvalidNumber('~');
end;

procedure DoTestInvalidCharHash;
begin
  DoInvalidNumber('#');
end;

procedure DoTestInvalidCharDollar;
begin
  DoInvalidNumber('$');
end;

function TestExpressionScanner_TestInvalidCharacter: TTestString;
begin
  Result := '';
  TestProcToRun := @DoTestInvalidCharTilde;
  AssertException('Invalid character "~"', EExprScanner, @RunTestProc);
  TestProcToRun := @DoTestInvalidCharHash;
  AssertException('Invalid character "#"', EExprScanner, @RunTestProc);
  TestProcToRun := @DoTestInvalidCharDollar;
  AssertException('Invalid character "$"', EExprScanner, @RunTestProc);
end;

procedure DoTestUnterminatedString;
begin
  DoInvalidNumber('''abc');
end;

function TestExpressionScanner_TestUnterminatedString: TTestString;
begin
  Result := '';
  TestProcToRun := @DoTestUnterminatedString;
  AssertException('Unterminated string', EExprScanner, @RunTestProc);
  try
    DoTestUnterminatedString;
    Fail('Unterminated string raises no exception');
  except
    on E: EExprScanner do
      AssertEquals('Unterminated string message', 'Unterminated string: ''abc', E.Message);
  end;
end;

function TestExpressionScanner_TestQuotesInString: TTestString;
begin
  Result := '';
  TestString('''That''''s it''', ttString);
  TestString('''''''s it''', ttString);
  TestString('''s it''''''', ttString);
end;

procedure TestIdentifier(const ASource, ATokenName: String);
begin
  FP.Source := ASource;
  AssertEqualsToken('Token type', ttIdentifier, FP.GetToken);
  AssertEquals('Token name', ATokenName, FP.Token);
end;

function TestExpressionScanner_TestIdentifiers: TTestString;
begin
  Result := '';
  TestIdentifier('a', 'a');
  TestIdentifier(' a', 'a');
  TestIdentifier('a ', 'a');
  TestIdentifier('a^b', 'a');
  TestIdentifier('a-b', 'a');
  TestIdentifier('a.b', 'a.b');
  TestIdentifier('"a b"', 'a b');
  TestIdentifier('c."a b"', 'c.a b');
  TestIdentifier('c."ab"', 'c.ab');
end;

const
  cTimesSign = #$C3#$97;
  cDotOperator = #$E2#$8B#$85;

type
  TTimesSignScanner = class(TFPExpressionScanner)
  public
    // Create a scanner that maps the times sign to ttMul
    constructor Create; override;
  end;

  TTimesSignParser = class(TFPExpressionParser)
  protected
    function CreateScanner: TFPExpressionScanner; override;
  end;

constructor TTimesSignScanner.Create;

begin
  inherited Create;
  AddOperatorAlias(cTimesSign, ttMul);
end;


function TTimesSignParser.CreateScanner: TFPExpressionScanner;

begin
  Result := TTimesSignScanner.Create;
end;


// Scan the next token and check its type and text
procedure AssertNextToken(const aMsg: String; aType: TTokenType; const aToken: String);

begin
  AssertEqualsToken(aMsg + ' type', aType, FP.GetToken);
  AssertEquals(aMsg + ' text', aToken, FP.Token);
end;


function TestExpressionScanner_TestDefaultConfig: TTestString;
begin
  Result := '';
  AssertEquals('Default string quote', '''', FP.StringQuoteChar);
  AssertEquals('Default identifier quote', '"', FP.IdentifierQuoteChar);
  AssertEquals('No operator aliases', 0, FP.OperatorAliasCount);
  FP.Source := '''a b''*"c d"';
  AssertNextToken('Single-quoted string', ttString, 'a b');
  AssertNextToken('Asterisk', ttMul, '*');
  AssertNextToken('Double-quoted identifier', ttIdentifier, 'c d');
  AssertNextToken('End', ttEOF, '');
end;

procedure DoSingleQuoteAfterSwitch;
begin
  FP.Source := '''abc''';
  FP.GetToken;
end;

function TestExpressionScanner_TestDoubleQuoteString: TTestString;
begin
  Result := '';
  FP.IdentifierQuoteChar := '`';
  FP.StringQuoteChar := '"';
  FP.Source := '"abc"+`a b`';
  AssertNextToken('Double-quoted string', ttString, 'abc');
  AssertNextToken('Plus', ttPlus, '+');
  AssertNextToken('Backquoted identifier', ttIdentifier, 'a b');
  AssertNextToken('End', ttEOF, '');
  FP.Source := '"say ""hi"""';
  AssertNextToken('Doubled quotes inside the string', ttString, 'say "hi"');
  TestProcToRun := @DoSingleQuoteAfterSwitch;
  AssertException('Single quote is no longer a string quote', EExprScanner, @RunTestProc);
end;

procedure DoSameQuoteChars;
begin
  FP.StringQuoteChar := '"';
end;

procedure DoDelimiterQuoteChar;
begin
  FP.StringQuoteChar := '+';
end;

procedure DoLetterQuoteChar;
begin
  FP.IdentifierQuoteChar := 'a';
end;

function TestExpressionScanner_TestInvalidQuoteChars: TTestString;
begin
  Result := '';
  TestProcToRun := @DoSameQuoteChars;
  AssertException('String quote equal to identifier quote', EExprScanner, @RunTestProc);
  AssertEquals('String quote unchanged after equal value', '''', FP.StringQuoteChar);
  TestProcToRun := @DoDelimiterQuoteChar;
  AssertException('Delimiter as string quote', EExprScanner, @RunTestProc);
  TestProcToRun := @DoLetterQuoteChar;
  AssertException('Letter as identifier quote', EExprScanner, @RunTestProc);
  AssertEquals('Identifier quote unchanged after letter', '"', FP.IdentifierQuoteChar);
end;

function TestExpressionScanner_TestOperatorAliasMul: TTestString;
begin
  Result := '';
  FP.AddOperatorAlias(cTimesSign, ttMul);
  FP.AddOperatorAlias(cDotOperator, ttMul);
  AssertEquals('Two aliases', 2, FP.OperatorAliasCount);
  FP.Source := '2' + cTimesSign + '3';
  AssertNextToken('Number before times sign', ttNumber, '2');
  AssertNextToken('Times sign', ttMul, cTimesSign);
  AssertNextToken('Number after times sign', ttNumber, '3');
  AssertNextToken('End after times sign', ttEOF, '');
  FP.Source := 'a' + cDotOperator + 'b * c';
  AssertNextToken('Identifier before dot operator', ttIdentifier, 'a');
  AssertNextToken('Dot operator', ttMul, cDotOperator);
  AssertNextToken('Identifier after dot operator', ttIdentifier, 'b');
  AssertNextToken('Asterisk still multiplies', ttMul, '*');
  AssertNextToken('Last identifier', ttIdentifier, 'c');
  FP.Source := '''2' + cTimesSign + '3''';
  AssertNextToken('Alias inside a string', ttString, '2' + cTimesSign + '3');
end;

function TestExpressionScanner_TestOperatorAliasLongest: TTestString;
begin
  Result := '';
  FP.AddOperatorAlias('**', ttPower);
  FP.AddOperatorAlias('!=', ttUnequal);
  FP.Source := '2**3*4!=a';
  AssertNextToken('First number', ttNumber, '2');
  AssertNextToken('Double asterisk', ttPower, '**');
  AssertNextToken('Second number', ttNumber, '3');
  AssertNextToken('Single asterisk', ttMul, '*');
  AssertNextToken('Third number', ttNumber, '4');
  AssertNextToken('Exclamation equals', ttUnequal, '!=');
  AssertNextToken('Identifier', ttIdentifier, 'a');
end;

procedure DoEmptyAlias;
begin
  FP.AddOperatorAlias('', ttMul);
end;

procedure DoLetterAlias;
begin
  FP.AddOperatorAlias('x', ttMul);
end;

procedure DoNumberTokenAlias;
begin
  FP.AddOperatorAlias('#', ttNumber);
end;

procedure DoClearedAlias;
begin
  FP.Source := '2' + cTimesSign + '3';
  FP.GetToken;
  FP.GetToken;
end;

function TestExpressionScanner_TestOperatorAliasManagement: TTestString;
begin
  Result := '';
  TestProcToRun := @DoEmptyAlias;
  AssertException('Empty alias', EExprScanner, @RunTestProc);
  TestProcToRun := @DoLetterAlias;
  AssertException('Alias starting with a letter', EExprScanner, @RunTestProc);
  TestProcToRun := @DoNumberTokenAlias;
  AssertException('Alias for a number token', EExprScanner, @RunTestProc);
  AssertEquals('Refused aliases are not added', 0, FP.OperatorAliasCount);
  FP.AddOperatorAlias(cTimesSign, ttPlus);
  FP.AddOperatorAlias(cTimesSign, ttMul);
  AssertEquals('Same alias added twice', 1, FP.OperatorAliasCount);
  AssertEquals('Alias text', cTimesSign, FP.OperatorAliases[0].Alias);
  AssertEqualsToken('Second token type replaces the first', ttMul, FP.OperatorAliases[0].TokenType);
  FP.ClearOperatorAliases;
  AssertEquals('No aliases after clearing', 0, FP.OperatorAliasCount);
  TestProcToRun := @DoClearedAlias;
  AssertException('Times sign is unknown after clearing', EExprScanner, @RunTestProc);
end;

function TestExpressionScanner_TestParserScanner: TTestString;
var
  lParser: TFPExpressionParser;
begin
  Result := '';
  lParser := TFPExpressionParser.Create(nil);
  try
    lParser.Scanner.AddOperatorAlias(cDotOperator, ttMul);
    lParser.Scanner.IdentifierQuoteChar := '`';
    lParser.Scanner.StringQuoteChar := '"';
    lParser.Expression := '2' + cDotOperator + '3';
    AssertEquals('Parser evaluates the dot operator', 6, lParser.AsInteger);
    lParser.Expression := '"ab"+"c"';
    AssertEquals('Parser evaluates double-quoted strings', 'abc', lParser.AsString);
  finally
    lParser.Free;
  end;
  lParser := TTimesSignParser.Create(nil);
  try
    AssertTrue('CreateScanner result is used', lParser.Scanner is TTimesSignScanner);
    lParser.Expression := '4' + cTimesSign + '5';
    AssertEquals('Parser evaluates the times sign', 20, lParser.AsInteger);
  finally
    lParser.Free;
  end;
end;

procedure RegisterTests(aTop : PSuite);

var
  lSuite : PSuite;

begin
  lSuite:=AddSuite('TExpressionScannerTests', @TestExpressionScanner_SetUp, @TestExpressionScanner_TearDown, aTop, True);
  AddTest('TestCreate', @TestExpressionScanner_TestCreate, lSuite);
  AddTest('TestSetSource', @TestExpressionScanner_TestSetSource, lSuite);
  AddTest('TestWhiteSpace', @TestExpressionScanner_TestWhiteSpace, lSuite);
  AddTest('TestTokens', @TestExpressionScanner_TestTokens, lSuite);
  AddTest('TestNumber', @TestExpressionScanner_TestNumber, lSuite);
  AddTest('TestInvalidCharacter', @TestExpressionScanner_TestInvalidCharacter, lSuite);
  AddTest('TestUnterminatedString', @TestExpressionScanner_TestUnterminatedString, lSuite);
  AddTest('TestQuotesInString', @TestExpressionScanner_TestQuotesInString, lSuite);
  AddTest('TestIdentifiers', @TestExpressionScanner_TestIdentifiers, lSuite);
  AddTest('TestDefaultConfig', @TestExpressionScanner_TestDefaultConfig, lSuite);
  AddTest('TestDoubleQuoteString', @TestExpressionScanner_TestDoubleQuoteString, lSuite);
  AddTest('TestInvalidQuoteChars', @TestExpressionScanner_TestInvalidQuoteChars, lSuite);
  AddTest('TestOperatorAliasMul', @TestExpressionScanner_TestOperatorAliasMul, lSuite);
  AddTest('TestOperatorAliasLongest', @TestExpressionScanner_TestOperatorAliasLongest, lSuite);
  AddTest('TestOperatorAliasManagement', @TestExpressionScanner_TestOperatorAliasManagement, lSuite);
  AddTest('TestParserScanner', @TestExpressionScanner_TestParserScanner, lSuite);
end;

end.
