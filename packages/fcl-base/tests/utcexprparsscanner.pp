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
  raise EExprScanner.Create('Invalid number');
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

procedure RegisterTests(aTop : PSuite);
begin
  AddSuite('TExpressionScannerTests', @TestExpressionScanner_SetUp, @TestExpressionScanner_TearDown, aTop, True);
  AddTest('TestCreate', @TestExpressionScanner_TestCreate, 'TExpressionScannerTests');
  AddTest('TestSetSource', @TestExpressionScanner_TestSetSource, 'TExpressionScannerTests');
  AddTest('TestWhiteSpace', @TestExpressionScanner_TestWhiteSpace, 'TExpressionScannerTests');
  AddTest('TestTokens', @TestExpressionScanner_TestTokens, 'TExpressionScannerTests');
  AddTest('TestNumber', @TestExpressionScanner_TestNumber, 'TExpressionScannerTests');
  AddTest('TestInvalidCharacter', @TestExpressionScanner_TestInvalidCharacter, 'TExpressionScannerTests');
  AddTest('TestUnterminatedString', @TestExpressionScanner_TestUnterminatedString, 'TExpressionScannerTests');
  AddTest('TestQuotesInString', @TestExpressionScanner_TestQuotesInString, 'TExpressionScannerTests');
  AddTest('TestIdentifiers', @TestExpressionScanner_TestIdentifiers, 'TExpressionScannerTests');
end;

end.
