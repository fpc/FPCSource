{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt (michael@freepascal.org)

    Test EBNF Scanner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utcscanner;

interface

uses
  sysutils, fpcunit, testregistry, ebnf.scanner;

type

  { TTestEBNFScanner }

  TTestEBNFScanner = class(TTestCase)
  private
    FScanner: TEBNFScanner;
    procedure CheckToken(aType: TEBNFTokenType; aValue: string; AMessage: string);
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    procedure AssertEquals(const Msg : String; aExpected, aActual : TEBNFTokenType); overload;
    procedure CheckEquals(aExpected, aActual : TEBNFTokenType; const Msg : String = ''); overload;
    property Scanner : TEBNFScanner Read FScanner Write FScanner;
  published
    // Test methods for each token type
    procedure TestIdentifier;
    procedure TestStringLiteralSingleQuote;
    procedure TestStringLiteralDoubleQuote;
    procedure TestEquals;
    procedure TestComment;
    procedure TestPipe;
    procedure TestOpenParen;
    procedure TestCloseParen;
    procedure TestOpenBracket;
    procedure TestCloseBracket;
    procedure TestOpenBrace;
    procedure TestCloseBrace;
    procedure TestSemicolon;
    procedure TestQuestion;
    procedure TestEOF;
    procedure TestWhitespaceHandling;
    procedure TestMultipleTokens;
    procedure TestUnknownTokenError;
    procedure TestUnterminatedStringError;
  end;

implementation

uses typinfo;

{ TTestEBNFScanner }

procedure TTestEBNFScanner.SetUp;
begin
  inherited SetUp;
  FreeAndNil(FScanner);
end;

procedure TTestEBNFScanner.TearDown;
begin
  FreeAndNil(FScanner);
  inherited TearDown;
end;

procedure TTestEBNFScanner.AssertEquals(const Msg: String; aExpected, aActual: TEBNFTokenType);
begin
  AssertEquals(Msg,GetEnumName(typeInfo(TEBNFTokenType),ord(aExpected)),
                  GetEnumName(typeInfo(TEBNFTokenType),ord(aActual)));
end;

procedure TTestEBNFScanner.CheckEquals(aExpected, aActual: TEBNFTokenType; const Msg: String);
begin
  AssertEquals(Msg,aExpected,aActual);
end;

procedure TTestEBNFScanner.CheckToken(aType : TEBNFTokenType; aValue : string; AMessage : string);

var
  Token: TToken;
begin
  Token := Scanner.GetNextToken;
  CheckEquals(aType, Token.TokenType, 'Expected token type');
  if aType<>ttEOF then
    CheckEquals(aValue, Token.Value, 'Expected token value');
end;

procedure TTestEBNFScanner.TestIdentifier;

begin
  Scanner := TEBNFScanner.Create('myRuleName another_id Rule123');
  CheckToken(ttIdentifier,'myRuleName', 'first identifier');
  CheckToken(ttIdentifier,'another_id', 'second identifier');
  CheckToken(ttIdentifier,'Rule123', 'third identifier');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestStringLiteralSingleQuote;

begin
  Scanner := TEBNFScanner.Create('''hello world'' ''a''');
  CheckToken(ttStringLiteral,'hello world','first literal');
  CheckToken(ttStringLiteral,'a','second literal');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestStringLiteralDoubleQuote;
begin
  Scanner := TEBNFScanner.Create('"another string" "123"');
  CheckToken(ttStringLiteral,'another string','first literal');
  CheckToken(ttStringLiteral,'123','second literal');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestEquals;

begin
  Scanner := TEBNFScanner.Create('=');
  CheckToken(ttEquals,'','Equals');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestComment;

begin
  Scanner := TEBNFScanner.Create('(* some comment *) =');
  CheckToken(ttEquals,'','Equals');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestPipe;

begin
  Scanner := TEBNFScanner.Create('|');
  CheckToken(ttPipe,'','Pipe');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestOpenParen;

begin
  Scanner := TEBNFScanner.Create('(');
  CheckToken(ttOpenParen,'','open parenthesis');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestCloseParen;

begin
  Scanner := TEBNFScanner.Create(')');
  CheckToken(ttCloseParen,'','close parenthesis');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestOpenBracket;

begin
  Scanner := TEBNFScanner.Create('[');
  CheckToken(ttOpenBracket,'','open bracket');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestCloseBracket;

begin
  Scanner := TEBNFScanner.Create(']');
  CheckToken(ttCloseBracket,'','close bracket');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestOpenBrace;

begin
  Scanner := TEBNFScanner.Create('{');
  CheckToken(ttOpenBrace,'','open brace');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestCloseBrace;

begin
  Scanner := TEBNFScanner.Create('}');
  CheckToken(ttCloseBrace,'','close brace');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestSemicolon;

begin
  Scanner := TEBNFScanner.Create(';');
  CheckToken(ttSemicolon,'','semicolon');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestQuestion;

begin
  Scanner := TEBNFScanner.Create('?');
  CheckToken(ttQuestion,'','Question');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestEOF;

begin
  Scanner := TEBNFScanner.Create('');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestWhitespaceHandling;

begin
  Scanner := TEBNFScanner.Create(#13#10'  rule = "test" ;   ');
  CheckToken(ttIdentifier,'rule','first');
  CheckToken(ttEquals,'','second');
  CheckToken(ttStringLiteral,'test','third');
  CheckToken(ttsemicolon,'','fourth');
  CheckToken(ttEOF,'', 'EOF');
end;

procedure TTestEBNFScanner.TestMultipleTokens;

begin
  Scanner := TEBNFScanner.Create('grammar = rule { "|" rule } ;');
  CheckToken(ttIdentifier, 'grammar', 'first');
  CheckToken(ttEquals,'','second');
  CheckToken(ttIdentifier, 'rule', 'third');
  CheckToken(ttOpenBrace, '','fourth');
  CheckToken(ttStringLiteral, '|', 'fifth');
  CheckToken(ttIdentifier, 'rule', 'sixth');
  CheckToken(ttCloseBrace, '','seventh');
  CheckToken(ttSemicolon, '','eighth');
  CheckToken(ttEOF, '', 'EOF');
end;

procedure TTestEBNFScanner.TestUnknownTokenError;
begin
  Scanner := TEBNFScanner.Create('@');
  try
    Scanner.GetNextToken;
    Fail('Expected an exception for unknown token');
  except
    on E: Exception do
      Check(Pos('Unknown token: "@"', E.Message) > 0, 'Expected "Unknown token: "@"" error message');
  end;
end;

procedure TTestEBNFScanner.TestUnterminatedStringError;

begin
  Scanner := TEBNFScanner.Create('''unterminated');
  try
    Scanner.GetNextToken;
    Fail('Expected an exception for unterminated string');
  except
    on E: Exception do
      Check(Pos('Unterminated string literal', E.Message) > 0, 'Expected "Unterminated string literal" error message');
  end;
end;

initialization
  RegisterTest(TTestEBNFScanner);
end.

