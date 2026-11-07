{
    This file is part of the Free Component Library

    Tests for the XPathScanner unit: tokens, keywords and lexer errors.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcxpathscanner;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, TypInfo, fpcunit, testregistry, xmlutils, XPathScanner;

type

  { TTestXPathLexer }

  TTestXPathLexer = class(TTestCase)
  private
    FExpr: XMLString;
    FLexer: TXPathLexer;
    procedure ScanAll;
  protected
    procedure TearDown; override;
    procedure CreateLexer(const aExpr: XMLString);
    procedure AssertToken(const aMsg: String; aExpected, aActual: TXPathToken);
    procedure AssertTokens(const aExpr: XMLString; const aTokens: array of TXPathToken);
    procedure AssertSingleToken(const aExpr: XMLString; aToken: TXPathToken; const aText: XMLString = '');
    procedure AssertScanError(const aExpr: XMLString; const aMessage: String);
  published
    procedure TestEmpty;
    procedure TestWhitespaceOnly;
    procedure TestEndOfStreamRepeats;
    procedure TestLeftBracket;
    procedure TestRightBracket;
    procedure TestAsterisk;
    procedure TestPlus;
    procedure TestComma;
    procedure TestMinus;
    procedure TestDot;
    procedure TestDotDot;
    procedure TestSlash;
    procedure TestSlashSlash;
    procedure TestColonColon;
    procedure TestLess;
    procedure TestLessEqual;
    procedure TestEqual;
    procedure TestNotEqual;
    procedure TestGreater;
    procedure TestGreaterEqual;
    procedure TestAt;
    procedure TestLeftSquareBracket;
    procedure TestRightSquareBracket;
    procedure TestPipe;
    procedure TestInteger;
    procedure TestDecimal;
    procedure TestLeadingDot;
    procedure TestTrailingDot;
    procedure TestTwoDots;
    procedure TestDoubleQuotedString;
    procedure TestSingleQuotedString;
    procedure TestEmptyString;
    procedure TestStringWithOtherQuote;
    procedure TestStringKeepsWhitespace;
    procedure TestUnclosedString;
    procedure TestVariable;
    procedure TestQualifiedVariable;
    procedure TestDollarAlone;
    procedure TestDollarDigit;
    procedure TestIdentifier;
    procedure TestIdentifierWithHyphenAndDot;
    procedure TestIdentifierUnderscore;
    procedure TestIdentifierNonAscii;
    procedure TestQName;
    procedure TestNSNameTest;
    procedure TestMalformedQName;
    procedure TestAxisNotPrefix;
    procedure TestInvalidCharacter;
    procedure TestBangAlone;
    procedure TestColonAlone;
    procedure TestKeyword;
    procedure TestKeywordAfterAxis;
    procedure TestNoKeyword;
    procedure TestPrefixedKeyword;
    procedure TestKeywordOnlyForIdentifier;
    procedure TestPeekToken;
    procedure TestSkipToken;
    procedure TestSkipTokenMismatch;
    procedure TestLocationPath;
    procedure TestFilterAndUnion;
    procedure TestArithmetic;
    procedure TestFunctionCall;
    procedure TestNoWhitespaceNeeded;
    procedure TestAllWhitespaceKinds;
    procedure TestExpressionCopied;
    procedure TestLookupAllKeywords;
    procedure TestLookupUnknown;
    procedure TestLookupCaseSensitive;
    procedure TestLookupPointer;
    procedure TestKeywordNameNone;
    procedure TestKeywordNames;
    procedure TestErrorHierarchy;
  end;

implementation


function TokenName(aToken: TXPathToken): String;

begin
  Result:=GetEnumName(TypeInfo(TXPathToken),Ord(aToken));
end;


function KeywordName(aKeyword: TXPathKeyword): String;

begin
  Result:=GetEnumName(TypeInfo(TXPathKeyword),Ord(aKeyword));
end;


procedure TTestXPathLexer.TearDown;

begin
  FreeAndNil(FLexer);
  inherited TearDown;
end;


procedure TTestXPathLexer.CreateLexer(const aExpr: XMLString);

begin
  FreeAndNil(FLexer);
  FLexer:=TXPathLexer.Create(aExpr);
end;


procedure TTestXPathLexer.ScanAll;

var
  lLexer: TXPathLexer;

begin
  lLexer:=TXPathLexer.Create(FExpr);
  try
    while lLexer.CurToken<>tkEndOfStream do
      lLexer.NextToken;
  finally
    lLexer.Free;
  end;
end;


procedure TTestXPathLexer.AssertToken(const aMsg: String; aExpected, aActual: TXPathToken);

begin
  AssertEquals(aMsg,TokenName(aExpected),TokenName(aActual));
end;


procedure TTestXPathLexer.AssertTokens(const aExpr: XMLString; const aTokens: array of TXPathToken);

var
  I: Integer;

begin
  CreateLexer(aExpr);
  for I:=0 to Length(aTokens)-1 do
    begin
    AssertToken(Format('Token %d of "%s"',[I,String(aExpr)]),aTokens[I],FLexer.CurToken);
    FLexer.NextToken;
    end;
  AssertToken(Format('End of "%s"',[String(aExpr)]),tkEndOfStream,FLexer.CurToken);
end;


procedure TTestXPathLexer.AssertSingleToken(const aExpr: XMLString; aToken: TXPathToken; const aText: XMLString);

begin
  CreateLexer(aExpr);
  AssertToken(Format('Token of "%s"',[String(aExpr)]),aToken,FLexer.CurToken);
  if aText<>'' then
    AssertEquals(Format('Token text of "%s"',[String(aExpr)]),String(aText),String(FLexer.CurTokenString));
  AssertToken(Format('Only one token in "%s"',[String(aExpr)]),tkEndOfStream,FLexer.NextToken);
end;


procedure TTestXPathLexer.AssertScanError(const aExpr: XMLString; const aMessage: String);

begin
  FExpr:=aExpr;
  AssertException(Format('Scanning "%s" fails',[String(aExpr)]),EXPathLexerError,@ScanAll,aMessage);
end;


procedure TTestXPathLexer.TestEmpty;

begin
  CreateLexer('');
  AssertToken('Empty expression',tkEndOfStream,FLexer.CurToken);
end;


procedure TTestXPathLexer.TestWhitespaceOnly;

begin
  CreateLexer('  '#9#10#13'  ');
  AssertToken('Whitespace only expression',tkEndOfStream,FLexer.CurToken);
end;


procedure TTestXPathLexer.TestEndOfStreamRepeats;

begin
  CreateLexer('a');
  AssertToken('Identifier first',tkIdentifier,FLexer.CurToken);
  AssertToken('Then end of stream',tkEndOfStream,FLexer.NextToken);
  AssertToken('End of stream again',tkEndOfStream,FLexer.NextToken);
  AssertToken('Current token stays end of stream',tkEndOfStream,FLexer.CurToken);
end;


procedure TTestXPathLexer.TestLeftBracket;

begin
  AssertSingleToken('(',tkLeftBracket);
end;


procedure TTestXPathLexer.TestRightBracket;

begin
  AssertSingleToken(')',tkRightBracket);
end;


procedure TTestXPathLexer.TestAsterisk;

begin
  AssertSingleToken('*',tkAsterisk);
end;


procedure TTestXPathLexer.TestPlus;

begin
  AssertSingleToken('+',tkPlus);
end;


procedure TTestXPathLexer.TestComma;

begin
  AssertSingleToken(',',tkComma);
end;


procedure TTestXPathLexer.TestMinus;

begin
  AssertSingleToken('-',tkMinus);
end;


procedure TTestXPathLexer.TestDot;

begin
  AssertSingleToken('.',tkDot);
end;


procedure TTestXPathLexer.TestDotDot;

begin
  AssertSingleToken('..',tkDotDot);
end;


procedure TTestXPathLexer.TestSlash;

begin
  AssertSingleToken('/',tkSlash);
end;


procedure TTestXPathLexer.TestSlashSlash;

begin
  AssertSingleToken('//',tkSlashSlash);
end;


procedure TTestXPathLexer.TestColonColon;

begin
  AssertSingleToken('::',tkColonColon);
end;


procedure TTestXPathLexer.TestLess;

begin
  AssertSingleToken('<',tkLess);
end;


procedure TTestXPathLexer.TestLessEqual;

begin
  AssertSingleToken('<=',tkLessEqual);
end;


procedure TTestXPathLexer.TestEqual;

begin
  AssertSingleToken('=',tkEqual);
end;


procedure TTestXPathLexer.TestNotEqual;

begin
  AssertSingleToken('!=',tkNotEqual);
end;


procedure TTestXPathLexer.TestGreater;

begin
  AssertSingleToken('>',tkGreater);
end;


procedure TTestXPathLexer.TestGreaterEqual;

begin
  AssertSingleToken('>=',tkGreaterEqual);
end;


procedure TTestXPathLexer.TestAt;

begin
  AssertSingleToken('@',tkAt);
end;


procedure TTestXPathLexer.TestLeftSquareBracket;

begin
  AssertSingleToken('[',tkLeftSquareBracket);
end;


procedure TTestXPathLexer.TestRightSquareBracket;

begin
  AssertSingleToken(']',tkRightSquareBracket);
end;


procedure TTestXPathLexer.TestPipe;

begin
  AssertSingleToken('|',tkPipe);
end;


procedure TTestXPathLexer.TestInteger;

begin
  AssertSingleToken('1234',tkNumber,'1234');
end;


procedure TTestXPathLexer.TestDecimal;

begin
  AssertSingleToken('3.25',tkNumber,'3.25');
end;


procedure TTestXPathLexer.TestLeadingDot;

begin
  AssertSingleToken('.5',tkNumber,'.5');
end;


procedure TTestXPathLexer.TestTrailingDot;

begin
  AssertSingleToken('7.',tkNumber,'7.');
end;


procedure TTestXPathLexer.TestTwoDots;

begin
  CreateLexer('1.2.3');
  AssertToken('First number',tkNumber,FLexer.CurToken);
  AssertEquals('First number stops at the second dot','1.2',String(FLexer.CurTokenString));
  AssertToken('Second number',tkNumber,FLexer.NextToken);
  AssertEquals('Second number starts with a dot','.3',String(FLexer.CurTokenString));
  AssertToken('End of stream',tkEndOfStream,FLexer.NextToken);
end;


procedure TTestXPathLexer.TestDoubleQuotedString;

begin
  AssertSingleToken('"abc"',tkString,'abc');
end;


procedure TTestXPathLexer.TestSingleQuotedString;

begin
  AssertSingleToken('''abc''',tkString,'abc');
end;


procedure TTestXPathLexer.TestEmptyString;

begin
  CreateLexer('""');
  AssertToken('Empty string literal',tkString,FLexer.CurToken);
  AssertEquals('Empty string literal text','',String(FLexer.CurTokenString));
  AssertToken('End of stream',tkEndOfStream,FLexer.NextToken);
end;


procedure TTestXPathLexer.TestStringWithOtherQuote;

begin
  AssertSingleToken('"it''s"',tkString,'it''s');
  AssertSingleToken('''say "hi"''',tkString,'say "hi"');
end;


procedure TTestXPathLexer.TestStringKeepsWhitespace;

begin
  AssertSingleToken('  " a b "  ',tkString,' a b ');
end;


procedure TTestXPathLexer.TestUnclosedString;

begin
  AssertScanError('"abc',SScannerUnclosedString);
  AssertScanError('''abc"',SScannerUnclosedString);
end;


procedure TTestXPathLexer.TestVariable;

begin
  AssertSingleToken('$foo',tkVariable,'foo');
end;


procedure TTestXPathLexer.TestQualifiedVariable;

begin
  CreateLexer('$ns:foo');
  AssertToken('Qualified variable',tkVariable,FLexer.CurToken);
  AssertEquals('Variable name without the dollar','ns:foo',String(FLexer.CurTokenString));
  AssertEquals('Prefix length of the variable name',2,FLexer.PrefixLength);
end;


procedure TTestXPathLexer.TestDollarAlone;

begin
  AssertScanError('$',SScannerExpectedVarName);
  AssertScanError('$ foo',SScannerExpectedVarName);
end;


procedure TTestXPathLexer.TestDollarDigit;

begin
  AssertScanError('$1',SScannerExpectedVarName);
end;


procedure TTestXPathLexer.TestIdentifier;

begin
  CreateLexer('para');
  AssertToken('Identifier',tkIdentifier,FLexer.CurToken);
  AssertEquals('Identifier text','para',String(FLexer.CurTokenString));
  AssertEquals('Identifier without prefix',0,FLexer.PrefixLength);
end;


procedure TTestXPathLexer.TestIdentifierWithHyphenAndDot;

begin
  AssertSingleToken('foo-bar.baz',tkIdentifier,'foo-bar.baz');
end;


procedure TTestXPathLexer.TestIdentifierUnderscore;

begin
  AssertSingleToken('_x1',tkIdentifier,'_x1');
end;


procedure TTestXPathLexer.TestIdentifierNonAscii;

var
  lName: XMLString;

begin
  lName:=XMLString(#$00E9'l'#$00E8've');
  AssertSingleToken(lName,tkIdentifier,lName);
end;


procedure TTestXPathLexer.TestQName;

begin
  CreateLexer('ns:local');
  AssertToken('QName is one identifier',tkIdentifier,FLexer.CurToken);
  AssertEquals('QName text','ns:local',String(FLexer.CurTokenString));
  AssertEquals('QName prefix length',2,FLexer.PrefixLength);
  AssertToken('End of stream',tkEndOfStream,FLexer.NextToken);
end;


procedure TTestXPathLexer.TestNSNameTest;

begin
  CreateLexer('ns:*');
  AssertToken('Namespace name test',tkNSNameTest,FLexer.CurToken);
  AssertEquals('Namespace name test text is the prefix','ns',String(FLexer.CurTokenString));
  AssertToken('End of stream',tkEndOfStream,FLexer.NextToken);
end;


procedure TTestXPathLexer.TestMalformedQName;

begin
  AssertScanError('ns:1',SScannerMalformedQName);
  AssertScanError('ns:',SScannerMalformedQName);
end;


procedure TTestXPathLexer.TestAxisNotPrefix;

begin
  AssertTokens('child::para',[tkIdentifier,tkColonColon,tkIdentifier]);
  CreateLexer('child::para');
  AssertEquals('Axis name text','child',String(FLexer.CurTokenString));
  AssertEquals('Axis name has no prefix',0,FLexer.PrefixLength);
end;


procedure TTestXPathLexer.TestInvalidCharacter;

begin
  AssertScanError('#',SScannerInvalidChar);
  AssertScanError('a ~ b',SScannerInvalidChar);
  AssertScanError('a ? b',SScannerInvalidChar);
end;


procedure TTestXPathLexer.TestBangAlone;

begin
  AssertScanError('!',SScannerInvalidChar);
  AssertScanError('a ! b',SScannerInvalidChar);
end;


procedure TTestXPathLexer.TestColonAlone;

begin
  AssertScanError(':',SScannerInvalidChar);
  AssertScanError(': a',SScannerInvalidChar);
end;


procedure TTestXPathLexer.TestKeyword;

begin
  CreateLexer('ancestor-or-self');
  AssertToken('Keyword is an identifier',tkIdentifier,FLexer.CurToken);
  AssertEquals('Keyword of the identifier',KeywordName(xkAncestorOrSelf),KeywordName(FLexer.CurKeyword));
end;


procedure TTestXPathLexer.TestKeywordAfterAxis;

begin
  CreateLexer('child::text()');
  AssertEquals('Keyword of the axis',KeywordName(xkChild),KeywordName(FLexer.CurKeyword));
  FLexer.NextToken;
  AssertEquals('No keyword for "::"',KeywordName(xkNone),KeywordName(FLexer.CurKeyword));
  FLexer.NextToken;
  AssertEquals('Keyword of the node test',KeywordName(xkText),KeywordName(FLexer.CurKeyword));
end;


procedure TTestXPathLexer.TestNoKeyword;

begin
  CreateLexer('book');
  AssertEquals('Ordinary name has no keyword',KeywordName(xkNone),KeywordName(FLexer.CurKeyword));
end;


procedure TTestXPathLexer.TestPrefixedKeyword;

begin
  CreateLexer('ns:child');
  AssertEquals('Prefixed name is not a keyword',KeywordName(xkNone),KeywordName(FLexer.CurKeyword));
end;


procedure TTestXPathLexer.TestKeywordOnlyForIdentifier;

begin
  CreateLexer('"child"');
  AssertToken('String literal',tkString,FLexer.CurToken);
  AssertEquals('String literal has no keyword',KeywordName(xkNone),KeywordName(FLexer.CurKeyword));
  CreateLexer('$child');
  AssertEquals('Variable has no keyword',KeywordName(xkNone),KeywordName(FLexer.CurKeyword));
end;


procedure TTestXPathLexer.TestPeekToken;

begin
  CreateLexer('a/b');
  AssertToken('Current token',tkIdentifier,FLexer.CurToken);
  AssertToken('Peeked token',tkSlash,FLexer.PeekToken);
  AssertToken('Peeking keeps the current token',tkIdentifier,FLexer.CurToken);
  AssertEquals('Peeking keeps the current token text','a',String(FLexer.CurTokenString));
  AssertToken('Peeking twice gives the same token',tkSlash,FLexer.PeekToken);
  AssertToken('Next token is the peeked token',tkSlash,FLexer.NextToken);
  AssertToken('Token after the slash',tkIdentifier,FLexer.NextToken);
  AssertEquals('Text after the slash','b',String(FLexer.CurTokenString));
end;


procedure TTestXPathLexer.TestSkipToken;

begin
  CreateLexer('@id');
  AssertTrue('Skipping the current token succeeds',FLexer.SkipToken(tkAt));
  AssertToken('Skip moves to the next token',tkIdentifier,FLexer.CurToken);
end;


procedure TTestXPathLexer.TestSkipTokenMismatch;

begin
  CreateLexer('@id');
  AssertFalse('Skipping another token fails',FLexer.SkipToken(tkSlash));
  AssertToken('Failed skip keeps the current token',tkAt,FLexer.CurToken);
end;


procedure TTestXPathLexer.TestLocationPath;

begin
  AssertTokens('/descendant-or-self::node()/child::para[@type="x"]',
    [tkSlash,tkIdentifier,tkColonColon,tkIdentifier,tkLeftBracket,tkRightBracket,
     tkSlash,tkIdentifier,tkColonColon,tkIdentifier,tkLeftSquareBracket,
     tkAt,tkIdentifier,tkEqual,tkString,tkRightSquareBracket]);
  AssertTokens('//a/../b/.',[tkSlashSlash,tkIdentifier,tkSlash,tkDotDot,tkSlash,tkIdentifier,tkSlash,tkDot]);
end;


procedure TTestXPathLexer.TestFilterAndUnion;

begin
  AssertTokens('$x[1] | //book[@price >= 35.00]/title',
    [tkVariable,tkLeftSquareBracket,tkNumber,tkRightSquareBracket,tkPipe,
     tkSlashSlash,tkIdentifier,tkLeftSquareBracket,tkAt,tkIdentifier,tkGreaterEqual,tkNumber,
     tkRightSquareBracket,tkSlash,tkIdentifier]);
end;


procedure TTestXPathLexer.TestArithmetic;

begin
  AssertTokens('1 + 2 - 3 * 4 div 5 mod 6',
    [tkNumber,tkPlus,tkNumber,tkMinus,tkNumber,tkAsterisk,tkNumber,tkIdentifier,tkNumber,
     tkIdentifier,tkNumber]);
  AssertTokens('-a!=b<c<=d>e',
    [tkMinus,tkIdentifier,tkNotEqual,tkIdentifier,tkLess,tkIdentifier,tkLessEqual,tkIdentifier,
     tkGreater,tkIdentifier]);
end;


procedure TTestXPathLexer.TestFunctionCall;

begin
  AssertTokens('concat("a", $b, 3)',
    [tkIdentifier,tkLeftBracket,tkString,tkComma,tkVariable,tkComma,tkNumber,tkRightBracket]);
end;


procedure TTestXPathLexer.TestNoWhitespaceNeeded;

begin
  AssertTokens('a|b',[tkIdentifier,tkPipe,tkIdentifier]);
  AssertTokens('ns:*|@*',[tkNSNameTest,tkPipe,tkAt,tkAsterisk]);
end;


procedure TTestXPathLexer.TestAllWhitespaceKinds;

begin
  AssertTokens(' a'#9'/'#10'b'#13'| c ',[tkIdentifier,tkSlash,tkIdentifier,tkPipe,tkIdentifier]);
end;


procedure TTestXPathLexer.TestExpressionCopied;

var
  lExpr: XMLString;

begin
  lExpr:='first/second';
  UniqueString(lExpr);
  CreateLexer(lExpr);
  lExpr[1]:='X';
  lExpr:='';
  AssertEquals('First token text','first',String(FLexer.CurTokenString));
  AssertToken('Slash',tkSlash,FLexer.NextToken);
  AssertToken('Second identifier',tkIdentifier,FLexer.NextToken);
  AssertEquals('Lexer reads its own copy of the expression','second',String(FLexer.CurTokenString));
end;


procedure TTestXPathLexer.TestLookupAllKeywords;

var
  lKeyword: TXPathKeyword;

begin
  for lKeyword:=Succ(xkNone) to High(TXPathKeyword) do
    AssertEquals('Lookup of the name of '+KeywordName(lKeyword),KeywordName(lKeyword),
      KeywordName(LookupXPathKeyword(XPathKeywordName(lKeyword))));
end;


procedure TTestXPathLexer.TestLookupUnknown;

begin
  AssertEquals('Empty name',KeywordName(xkNone),KeywordName(LookupXPathKeyword('')));
  AssertEquals('Unknown name',KeywordName(xkNone),KeywordName(LookupXPathKeyword('book')));
  AssertEquals('Keyword with extra character',KeywordName(xkNone),KeywordName(LookupXPathKeyword('childx')));
  AssertEquals('Truncated keyword',KeywordName(xkNone),KeywordName(LookupXPathKeyword('chil')));
  AssertEquals('Name starting with z',KeywordName(xkNone),KeywordName(LookupXPathKeyword('zzz')));
  AssertEquals('Name starting with a digit',KeywordName(xkNone),KeywordName(LookupXPathKeyword('1and')));
end;


procedure TTestXPathLexer.TestLookupCaseSensitive;

begin
  AssertEquals('Upper case keyword',KeywordName(xkNone),KeywordName(LookupXPathKeyword('Child')));
  AssertEquals('All upper case keyword',KeywordName(xkNone),KeywordName(LookupXPathKeyword('AND')));
end;


procedure TTestXPathLexer.TestLookupPointer;

var
  lText: XMLString;

begin
  lText:='count(x)';
  AssertEquals('Lookup of the first 5 characters',KeywordName(xkCount),
    KeywordName(LookupXPathKeyword(PWideChar(lText),5)));
  AssertEquals('Lookup of the first 4 characters',KeywordName(xkNone),
    KeywordName(LookupXPathKeyword(PWideChar(lText),4)));
end;


procedure TTestXPathLexer.TestKeywordNameNone;

begin
  AssertEquals('Name of xkNone','',String(XPathKeywordName(xkNone)));
end;


procedure TTestXPathLexer.TestKeywordNames;

begin
  AssertEquals('Name of xkAncestorOrSelf','ancestor-or-self',String(XPathKeywordName(xkAncestorOrSelf)));
  AssertEquals('Name of xkProcessingInstruction','processing-instruction',String(XPathKeywordName(xkProcessingInstruction)));
  AssertEquals('Name of xkNamespaceUri','namespace-uri',String(XPathKeywordName(xkNamespaceUri)));
  AssertEquals('Name of xkDiv','div',String(XPathKeywordName(xkDiv)));
  AssertEquals('Name of xkRound','round',String(XPathKeywordName(xkRound)));
end;


procedure TTestXPathLexer.TestErrorHierarchy;

begin
  AssertTrue('EXPathError descends from Exception',EXPathError.InheritsFrom(Exception));
  AssertTrue('EXPathLexerError descends from EXPathError',EXPathLexerError.InheritsFrom(EXPathError));
  AssertTrue('EXPathParserError descends from EXPathError',EXPathParserError.InheritsFrom(EXPathError));
  AssertTrue('EXPathEvaluationError descends from EXPathError',EXPathEvaluationError.InheritsFrom(EXPathError));
  AssertFalse('EXPathLexerError is not a parser error',EXPathLexerError.InheritsFrom(EXPathParserError));
  AssertFalse('EXPathParserError is not an evaluation error',EXPathParserError.InheritsFrom(EXPathEvaluationError));
end;


initialization
  RegisterTest(TTestXPathLexer);
end.
