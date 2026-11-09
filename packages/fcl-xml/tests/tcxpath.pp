{
    This file is part of the Free Component Library

    Tests for the XPath unit: compatibility names, parsing and evaluation.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcxpath;

{$mode objfpc}{$h+}

interface

uses
  SysUtils, Classes, TypInfo, fpcunit, testregistry, DOM, XMLRead, XPath;

type

  { TTestXPathCompat }

  TTestXPathCompat = class(TTestCase)
  published
    procedure TestTokenOrdinals;
    procedure TestTokenCase;
    procedure TestTokenSet;
    procedure TestTokenNames;
    procedure TestKeywordType;
    procedure TestKeywordOrdinals;
    procedure TestKeywordCase;
    procedure TestKeywordSet;
    procedure TestErrorTypes;
    procedure TestScannerTokens;
    procedure TestScannerPeekAndSkip;
    procedure TestScannerTokenStrings;
  end;

  { TTestXPathEvaluate }

  TTestXPathEvaluate = class(TTestCase)
  private
    FDoc: TXMLDocument;
    FExpr: DOMString;
    FResolver: TXPathNSResolver;
    procedure EvaluateExpr;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    function Eval(const aExpr: DOMString): TXPathVariable;
    procedure AssertText(const aExpr, aExpected: DOMString);
    procedure AssertNumber(const aExpr: DOMString; aExpected: Extended);
    procedure AssertBool(const aExpr: DOMString; aExpected: Boolean);
    procedure AssertCount(const aExpr: DOMString; aExpected: Integer);
    procedure AssertError(const aExpr: DOMString; aClass: ExceptClass; const aMessage: String);
  published
    procedure TestRootCount;
    procedure TestChildPath;
    procedure TestDescendants;
    procedure TestAttributes;
    procedure TestPredicatePosition;
    procedure TestPredicateLast;
    procedure TestPredicateAttribute;
    procedure TestAxes;
    procedure TestNodeTests;
    procedure TestUnion;
    procedure TestArithmetic;
    procedure TestComparison;
    procedure TestBooleanOperators;
    procedure TestStringFunctions;
    procedure TestNumberFunctions;
    procedure TestNodeSetFunctions;
    procedure TestLiterals;
    procedure TestNamespaceNameTest;
    procedure TestWithoutResolver;
    procedure TestExpressionObject;
    procedure TestIncompleteExpressionAllowed;
    procedure TestUnknownFunction;
    procedure TestUnknownVariable;
    procedure TestInvalidArgCount;
    procedure TestNoConversion;
    procedure TestGarbageAfterExpression;
    procedure TestMissingRightBracket;
    procedure TestMissingRightSquareBracket;
    procedure TestBadAxisName;
    procedure TestInvalidNodeTest;
    procedure TestScannerErrorThroughParser;
    procedure TestUnionConversionError;
    procedure TestFilterConversionError;
    procedure TestNestedParseError;
  end;

implementation

const
  TestDocument =
    '<?xml version="1.0"?>'+
    '<library xmlns:p="urn:pub">'+
      '<book id="b1" price="10"><title>Alpha</title><author>Ann</author></book>'+
      '<book id="b2" price="25"><title>Beta</title><author>Bob</author><author>Bea</author></book>'+
      '<book id="b3" price="40"><title>Gamma</title><!--note--><?pi data?></book>'+
      '<p:shelf p:no="7"><p:item>x</p:item><p:item>y</p:item></p:shelf>'+
    '</library>';


function TokenName(aToken: TXPathToken): String;

begin
  Result:=GetEnumName(TypeInfo(TXPathToken),Ord(aToken));
end;


procedure TTestXPathCompat.TestTokenOrdinals;

begin
  AssertEquals('Ordinal of tkInvalid',0,Ord(tkInvalid));
  AssertEquals('Ordinal of tkEndOfStream',1,Ord(tkEndOfStream));
  AssertEquals('Ordinal of tkIdentifier',2,Ord(tkIdentifier));
  AssertEquals('Ordinal of tkNSNameTest',3,Ord(tkNSNameTest));
  AssertEquals('Ordinal of tkString',4,Ord(tkString));
  AssertEquals('Ordinal of tkNumber',5,Ord(tkNumber));
  AssertEquals('Ordinal of tkVariable',6,Ord(tkVariable));
  AssertEquals('Ordinal of tkLeftBracket',7,Ord(tkLeftBracket));
  AssertEquals('Ordinal of tkRightBracket',8,Ord(tkRightBracket));
  AssertEquals('Ordinal of tkAsterisk',9,Ord(tkAsterisk));
  AssertEquals('Ordinal of tkPlus',10,Ord(tkPlus));
  AssertEquals('Ordinal of tkComma',11,Ord(tkComma));
  AssertEquals('Ordinal of tkMinus',12,Ord(tkMinus));
  AssertEquals('Ordinal of tkDot',13,Ord(tkDot));
  AssertEquals('Ordinal of tkDotDot',14,Ord(tkDotDot));
  AssertEquals('Ordinal of tkSlash',15,Ord(tkSlash));
  AssertEquals('Ordinal of tkSlashSlash',16,Ord(tkSlashSlash));
  AssertEquals('Ordinal of tkColonColon',17,Ord(tkColonColon));
  AssertEquals('Ordinal of tkLess',18,Ord(tkLess));
  AssertEquals('Ordinal of tkLessEqual',19,Ord(tkLessEqual));
  AssertEquals('Ordinal of tkEqual',20,Ord(tkEqual));
  AssertEquals('Ordinal of tkNotEqual',21,Ord(tkNotEqual));
  AssertEquals('Ordinal of tkGreater',22,Ord(tkGreater));
  AssertEquals('Ordinal of tkGreaterEqual',23,Ord(tkGreaterEqual));
  AssertEquals('Ordinal of tkAt',24,Ord(tkAt));
  AssertEquals('Ordinal of tkLeftSquareBracket',25,Ord(tkLeftSquareBracket));
  AssertEquals('Ordinal of tkRightSquareBracket',26,Ord(tkRightSquareBracket));
  AssertEquals('Ordinal of tkPipe',27,Ord(tkPipe));
  AssertEquals('Last token is tkPipe',Ord(tkPipe),Ord(High(TXPathToken)));
  AssertEquals('First token is tkInvalid',Ord(tkInvalid),Ord(Low(TXPathToken)));
end;


procedure TTestXPathCompat.TestTokenCase;

var
  lToken: TXPathToken;
  lCount: Integer;

begin
  lCount:=0;
  for lToken:=Low(TXPathToken) to High(TXPathToken) do
    case lToken of
      tkInvalid, tkEndOfStream, tkIdentifier, tkNSNameTest, tkString, tkNumber,
      tkVariable, tkLeftBracket, tkRightBracket, tkAsterisk, tkPlus, tkComma,
      tkMinus, tkDot, tkDotDot, tkSlash, tkSlashSlash, tkColonColon, tkLess,
      tkLessEqual, tkEqual, tkNotEqual, tkGreater, tkGreaterEqual, tkAt,
      tkLeftSquareBracket, tkRightSquareBracket, tkPipe:
        Inc(lCount);
    end;
  AssertEquals('Every token matches a case label',28,lCount);
end;


procedure TTestXPathCompat.TestTokenSet;

var
  lSet: set of TXPathToken;

begin
  lSet:=[tkLess..tkGreaterEqual];
  AssertTrue('tkEqual in the comparison range',tkEqual in lSet);
  AssertFalse('tkAt outside the comparison range',tkAt in lSet);
  AssertTrue('tkPipe in a literal set',tkPipe in [tkSlash,tkPipe]);
end;


procedure TTestXPathCompat.TestTokenNames;

begin
  AssertEquals('Type information of tkNSNameTest','tkNSNameTest',TokenName(tkNSNameTest));
  AssertEquals('Type information of tkPipe','tkPipe',TokenName(tkPipe));
end;


procedure TTestXPathCompat.TestKeywordType;

var
  lKeyword: TXPathKeyword;

begin
  lKeyword:=Low(TXPathKeyword);
  AssertEquals('First keyword','xkNone',GetEnumName(TypeInfo(TXPathKeyword),Ord(lKeyword)));
  lKeyword:=High(TXPathKeyword);
  AssertEquals('Last keyword','xkRound',GetEnumName(TypeInfo(TXPathKeyword),Ord(lKeyword)));
end;


procedure TTestXPathCompat.TestKeywordOrdinals;

begin
  AssertEquals('Ordinal of xkNone',0,Ord(xkNone));
  AssertEquals('Ordinal of xkAncestor',1,Ord(xkAncestor));
  AssertEquals('Ordinal of xkAncestorOrSelf',2,Ord(xkAncestorOrSelf));
  AssertEquals('Ordinal of xkAttribute',3,Ord(xkAttribute));
  AssertEquals('Ordinal of xkChild',4,Ord(xkChild));
  AssertEquals('Ordinal of xkDescendant',5,Ord(xkDescendant));
  AssertEquals('Ordinal of xkDescendantOrSelf',6,Ord(xkDescendantOrSelf));
  AssertEquals('Ordinal of xkFollowing',7,Ord(xkFollowing));
  AssertEquals('Ordinal of xkFollowingSibling',8,Ord(xkFollowingSibling));
  AssertEquals('Ordinal of xkNamespace',9,Ord(xkNamespace));
  AssertEquals('Ordinal of xkParent',10,Ord(xkParent));
  AssertEquals('Ordinal of xkPreceding',11,Ord(xkPreceding));
  AssertEquals('Ordinal of xkPrecedingSibling',12,Ord(xkPrecedingSibling));
  AssertEquals('Ordinal of xkSelf',13,Ord(xkSelf));
  AssertEquals('Ordinal of xkComment',14,Ord(xkComment));
  AssertEquals('Ordinal of xkText',15,Ord(xkText));
  AssertEquals('Ordinal of xkProcessingInstruction',16,Ord(xkProcessingInstruction));
  AssertEquals('Ordinal of xkNode',17,Ord(xkNode));
  AssertEquals('Ordinal of xkAnd',18,Ord(xkAnd));
  AssertEquals('Ordinal of xkOr',19,Ord(xkOr));
  AssertEquals('Ordinal of xkDiv',20,Ord(xkDiv));
  AssertEquals('Ordinal of xkMod',21,Ord(xkMod));
  AssertEquals('Ordinal of xkLast',22,Ord(xkLast));
  AssertEquals('Ordinal of xkPosition',23,Ord(xkPosition));
  AssertEquals('Ordinal of xkCount',24,Ord(xkCount));
  AssertEquals('Ordinal of xkId',25,Ord(xkId));
  AssertEquals('Ordinal of xkLocalName',26,Ord(xkLocalName));
  AssertEquals('Ordinal of xkNamespaceUri',27,Ord(xkNamespaceUri));
  AssertEquals('Ordinal of xkName',28,Ord(xkName));
  AssertEquals('Ordinal of xkString',29,Ord(xkString));
  AssertEquals('Ordinal of xkConcat',30,Ord(xkConcat));
  AssertEquals('Ordinal of xkStartsWith',31,Ord(xkStartsWith));
  AssertEquals('Ordinal of xkContains',32,Ord(xkContains));
  AssertEquals('Ordinal of xkSubstringBefore',33,Ord(xkSubstringBefore));
  AssertEquals('Ordinal of xkSubstringAfter',34,Ord(xkSubstringAfter));
  AssertEquals('Ordinal of xkSubstring',35,Ord(xkSubstring));
  AssertEquals('Ordinal of xkStringLength',36,Ord(xkStringLength));
  AssertEquals('Ordinal of xkNormalizeSpace',37,Ord(xkNormalizeSpace));
  AssertEquals('Ordinal of xkTranslate',38,Ord(xkTranslate));
  AssertEquals('Ordinal of xkBoolean',39,Ord(xkBoolean));
  AssertEquals('Ordinal of xkNot',40,Ord(xkNot));
  AssertEquals('Ordinal of xkTrue',41,Ord(xkTrue));
  AssertEquals('Ordinal of xkFalse',42,Ord(xkFalse));
  AssertEquals('Ordinal of xkLang',43,Ord(xkLang));
  AssertEquals('Ordinal of xkNumber',44,Ord(xkNumber));
  AssertEquals('Ordinal of xkSum',45,Ord(xkSum));
  AssertEquals('Ordinal of xkFloor',46,Ord(xkFloor));
  AssertEquals('Ordinal of xkCeiling',47,Ord(xkCeiling));
  AssertEquals('Ordinal of xkRound',48,Ord(xkRound));
  AssertEquals('Last keyword is xkRound',Ord(xkRound),Ord(High(TXPathKeyword)));
end;


procedure TTestXPathCompat.TestKeywordCase;

var
  lKeyword: TXPathKeyword;
  lCount: Integer;

begin
  lCount:=0;
  for lKeyword:=Low(TXPathKeyword) to High(TXPathKeyword) do
    case lKeyword of
      xkNone, xkAncestor, xkAncestorOrSelf, xkAttribute, xkChild, xkDescendant, xkDescendantOrSelf,
      xkFollowing, xkFollowingSibling, xkNamespace, xkParent, xkPreceding, xkPrecedingSibling,
      xkSelf, xkComment, xkText, xkProcessingInstruction, xkNode, xkAnd, xkOr, xkDiv, xkMod,
      xkLast, xkPosition, xkCount, xkId, xkLocalName, xkNamespaceUri, xkName, xkString, xkConcat,
      xkStartsWith, xkContains, xkSubstringBefore, xkSubstringAfter, xkSubstring, xkStringLength,
      xkNormalizeSpace, xkTranslate, xkBoolean, xkNot, xkTrue, xkFalse, xkLang, xkNumber, xkSum,
      xkFloor, xkCeiling, xkRound:
        Inc(lCount);
    end;
  AssertEquals('Every keyword matches a case label',49,lCount);
end;


procedure TTestXPathCompat.TestKeywordSet;

var
  lAxes: set of TXPathKeyword;

begin
  lAxes:=[xkAncestor..xkSelf];
  AssertTrue('xkChild is an axis name',xkChild in lAxes);
  AssertFalse('xkText is not an axis name',xkText in lAxes);
  AssertTrue('xkDiv in a literal set',xkDiv in [xkAnd,xkOr,xkDiv,xkMod]);
end;


procedure TTestXPathCompat.TestErrorTypes;

var
  lError: EXPathEvaluationError;

begin
  lError:=EXPathEvaluationError.Create('x');
  try
    AssertTrue('Evaluation error is an XPath error',lError is EXPathError);
    AssertEquals('Class name','EXPathEvaluationError',lError.ClassName);
  finally
    lError.Free;
  end;
  AssertTrue('EXPathError descends from Exception',EXPathError.InheritsFrom(Exception));
  AssertTrue('Lexer error is an XPath error',EXPathLexerError.InheritsFrom(EXPathError));
  AssertTrue('Parser error is an XPath error',EXPathParserError.InheritsFrom(EXPathError));
end;


procedure TTestXPathCompat.TestScannerTokens;

const
  Expected: array[0..9] of TXPathToken = (
    tkSlashSlash,tkIdentifier,tkLeftSquareBracket,tkAt,tkIdentifier,tkGreaterEqual,
    tkNumber,tkRightSquareBracket,tkPipe,tkVariable);

var
  lScanner: TXPathScanner;
  I: Integer;

begin
  lScanner:=TXPathScanner.Create('//book[@price >= 2] | $v');
  try
    for I:=0 to High(Expected) do
      begin
      AssertEquals(Format('Token %d',[I]),TokenName(Expected[I]),TokenName(lScanner.CurToken));
      lScanner.NextToken;
      end;
    AssertEquals('End of stream',TokenName(tkEndOfStream),TokenName(lScanner.CurToken));
  finally
    lScanner.Free;
  end;
end;


procedure TTestXPathCompat.TestScannerPeekAndSkip;

var
  lScanner: TXPathScanner;

begin
  lScanner:=TXPathScanner.Create('a/b');
  try
    AssertEquals('Peek returns the slash',TokenName(tkSlash),TokenName(lScanner.PeekToken));
    AssertEquals('Peek keeps the identifier',TokenName(tkIdentifier),TokenName(lScanner.CurToken));
    AssertFalse('Skip of a wrong token fails',lScanner.SkipToken(tkSlash));
    AssertTrue('Skip of the identifier succeeds',lScanner.SkipToken(tkIdentifier));
    AssertTrue('Skip of the slash succeeds',lScanner.SkipToken(tkSlash));
    AssertEquals('Last identifier','b',String(lScanner.CurTokenString));
  finally
    lScanner.Free;
  end;
end;


procedure TTestXPathCompat.TestScannerTokenStrings;

var
  lScanner: TXPathScanner;

begin
  lScanner:=TXPathScanner.Create('p:x "s" 1.5 $v p:*');
  try
    AssertEquals('QName text','p:x',String(lScanner.CurTokenString));
    lScanner.NextToken;
    AssertEquals('String text','s',String(lScanner.CurTokenString));
    lScanner.NextToken;
    AssertEquals('Number text','1.5',String(lScanner.CurTokenString));
    lScanner.NextToken;
    AssertEquals('Variable text','v',String(lScanner.CurTokenString));
    lScanner.NextToken;
    AssertEquals('Name test token',TokenName(tkNSNameTest),TokenName(lScanner.CurToken));
    AssertEquals('Name test text','p',String(lScanner.CurTokenString));
  finally
    lScanner.Free;
  end;
end;


procedure TTestXPathEvaluate.SetUp;

var
  lParser: TDOMParser;
  lSource: TXMLInputSource;

begin
  lParser:=TDOMParser.Create;
  try
    lParser.Options.Namespaces:=True;
    lSource:=TXMLInputSource.Create(TestDocument);
    try
      lParser.Parse(lSource,FDoc);
    finally
      lSource.Free;
    end;
  finally
    lParser.Free;
  end;
  FResolver:=TXPathNSResolver.Create(FDoc.DocumentElement);
end;


procedure TTestXPathEvaluate.TearDown;

begin
  FreeAndNil(FResolver);
  FreeAndNil(FDoc);
end;


function TTestXPathEvaluate.Eval(const aExpr: DOMString): TXPathVariable;

begin
  Result:=EvaluateXPathExpression(aExpr,FDoc.DocumentElement,FResolver);
end;


procedure TTestXPathEvaluate.EvaluateExpr;

begin
  Eval(FExpr).Free;
end;


procedure TTestXPathEvaluate.AssertText(const aExpr, aExpected: DOMString);

var
  lRes: TXPathVariable;

begin
  lRes:=Eval(aExpr);
  try
    AssertEquals('Text of '+String(aExpr),String(aExpected),String(lRes.AsText));
  finally
    lRes.Free;
  end;
end;


procedure TTestXPathEvaluate.AssertNumber(const aExpr: DOMString; aExpected: Extended);

var
  lRes: TXPathVariable;

begin
  lRes:=Eval(aExpr);
  try
    AssertEquals('Number of '+String(aExpr),aExpected,lRes.AsNumber,1e-9);
  finally
    lRes.Free;
  end;
end;


procedure TTestXPathEvaluate.AssertBool(const aExpr: DOMString; aExpected: Boolean);

var
  lRes: TXPathVariable;

begin
  lRes:=Eval(aExpr);
  try
    AssertEquals('Boolean of '+String(aExpr),aExpected,lRes.AsBoolean);
  finally
    lRes.Free;
  end;
end;


procedure TTestXPathEvaluate.AssertCount(const aExpr: DOMString; aExpected: Integer);

var
  lRes: TXPathVariable;

begin
  lRes:=Eval(aExpr);
  try
    AssertTrue('Node set from '+String(aExpr),lRes is TXPathNodeSetVariable);
    AssertEquals('Node count of '+String(aExpr),aExpected,lRes.AsNodeSet.Count);
  finally
    lRes.Free;
  end;
end;


procedure TTestXPathEvaluate.AssertError(const aExpr: DOMString; aClass: ExceptClass; const aMessage: String);

begin
  FExpr:=aExpr;
  AssertException('Evaluating '+String(aExpr),aClass,@EvaluateExpr,aMessage);
end;


procedure TTestXPathEvaluate.TestRootCount;

begin
  AssertCount('/',1);
  AssertCount('/library',1);
  AssertCount('.',1);
end;


procedure TTestXPathEvaluate.TestChildPath;

begin
  AssertCount('book',3);
  AssertCount('book/title',3);
  AssertText('book/title','Alpha');
  AssertCount('/library/book/author',3);
end;


procedure TTestXPathEvaluate.TestDescendants;

begin
  AssertCount('//author',3);
  AssertCount('.//title',3);
  AssertCount('//book//text()',6);
end;


procedure TTestXPathEvaluate.TestAttributes;

begin
  AssertCount('book/@id',3);
  AssertText('book[2]/@id','b2');
  AssertCount('book/@*',6);
end;


procedure TTestXPathEvaluate.TestPredicatePosition;

begin
  AssertText('book[3]/title','Gamma');
  AssertText('book[position()=2]/title','Beta');
  AssertCount('book[position()>1]',2);
end;


procedure TTestXPathEvaluate.TestPredicateLast;

begin
  AssertText('book[last()]/title','Gamma');
  AssertText('book[2]/author[last()]','Bea');
end;


procedure TTestXPathEvaluate.TestPredicateAttribute;

begin
  AssertText('book[@id="b2"]/title','Beta');
  AssertCount('book[@price > 20]',2);
  AssertCount('book[author]',2);
  AssertCount('book[author][2]',1);
end;


procedure TTestXPathEvaluate.TestAxes;

begin
  AssertText('book[1]/following-sibling::book[1]/title','Beta');
  AssertText('book[3]/preceding-sibling::book[1]/title','Beta');
  AssertCount('book[1]/title/ancestor::*',2);
  AssertCount('book[1]/title/ancestor-or-self::*',3);
  AssertText('book[2]/author[1]/parent::book/@id','b2');
  AssertText('book[1]/author/../@id','b1');
  AssertCount('book[1]/descendant::*',2);
  AssertCount('book[1]/descendant-or-self::*',3);
  AssertCount('book[1]/self::book',1);
  AssertCount('book[1]/attribute::id',1);
  AssertCount('book[1]/child::title',1);
  AssertCount('book[1]/following::title',2);
  AssertCount('book[3]/preceding::title',2);
end;


procedure TTestXPathEvaluate.TestNodeTests;

begin
  AssertCount('book[3]/comment()',1);
  AssertText('book[3]/comment()','note');
  AssertCount('book[3]/processing-instruction()',1);
  AssertCount('book[3]/processing-instruction("pi")',1);
  AssertCount('book[3]/processing-instruction("other")',0);
  AssertCount('book[3]/node()',3);
  AssertCount('book[1]/*',2);
  AssertCount('book[1]/title/text()',1);
end;


procedure TTestXPathEvaluate.TestUnion;

begin
  AssertCount('book/title | book/author',6);
  AssertCount('book[1] | book[1]',1);
end;


procedure TTestXPathEvaluate.TestArithmetic;

begin
  AssertNumber('1 + 2 * 3',7);
  AssertNumber('(1 + 2) * 3',9);
  AssertNumber('7 div 2',3.5);
  AssertNumber('7 mod 3',1);
  AssertNumber('-3 - -2',-1);
  AssertNumber('book[1]/@price + book[2]/@price',35);
  AssertNumber('.5 + 1.',1.5);
end;


procedure TTestXPathEvaluate.TestComparison;

begin
  AssertBool('1 < 2',True);
  AssertBool('2 <= 2',True);
  AssertBool('3 > 4',False);
  AssertBool('4 >= 5',False);
  AssertBool('"a" = "a"',True);
  AssertBool('"a" != "b"',True);
  AssertBool('book/@price = 25',True);
  AssertBool('book/@price > 100',False);
end;


procedure TTestXPathEvaluate.TestBooleanOperators;

begin
  AssertBool('true() and false()',False);
  AssertBool('true() or false()',True);
  AssertBool('not(false())',True);
  AssertBool('1 = 1 and 2 = 2 or 1 = 0',True);
  AssertBool('boolean(book)',True);
  AssertBool('boolean(nothing)',False);
end;


procedure TTestXPathEvaluate.TestStringFunctions;

begin
  AssertText('concat("a", "b", "c")','abc');
  AssertBool('starts-with("abcdef", "abc")',True);
  AssertBool('contains("abcdef", "cde")',True);
  AssertText('substring-before("1999/04/01", "/")','1999');
  AssertText('substring-after("1999/04/01", "/")','04/01');
  AssertText('substring("12345", 2, 3)','234');
  AssertText('substring("12345", 2)','2345');
  AssertNumber('string-length("abc")',3);
  AssertText('normalize-space("  a   b  ")','a b');
  AssertText('translate("bar", "abc", "ABC")','BAr');
  AssertText('string(book[2]/title)','Beta');
  AssertText('string(12)','12');
end;


procedure TTestXPathEvaluate.TestNumberFunctions;

begin
  AssertNumber('number("12.5")',12.5);
  AssertNumber('sum(book/@price)',75);
  AssertNumber('floor(2.7)',2);
  AssertNumber('ceiling(2.1)',3);
  AssertNumber('round(2.5)',3);
  AssertNumber('round(-2.5)',-2);
end;


procedure TTestXPathEvaluate.TestNodeSetFunctions;

begin
  AssertNumber('count(book)',3);
  AssertNumber('count(//author)',3);
  AssertText('name(book[1])','book');
  AssertText('local-name(p:shelf)','shelf');
  AssertText('name(p:shelf)','p:shelf');
  AssertText('namespace-uri(p:shelf)','urn:pub');
  AssertNumber('count(book[last()]/preceding-sibling::*)',2);
end;


procedure TTestXPathEvaluate.TestLiterals;

begin
  AssertText('"double"','double');
  AssertText('''single''','single');
  AssertNumber('42',42);
end;


procedure TTestXPathEvaluate.TestNamespaceNameTest;

begin
  AssertCount('p:shelf',1);
  AssertCount('p:shelf/p:item',2);
  AssertCount('p:shelf/p:*',2);
  AssertText('p:shelf/@p:no','7');
end;


procedure TTestXPathEvaluate.TestWithoutResolver;

var
  lRes: TXPathVariable;

begin
  lRes:=EvaluateXPathExpression('count(book)',FDoc.DocumentElement);
  try
    AssertEquals('Evaluation without resolver',3,lRes.AsNumber,0);
  finally
    lRes.Free;
  end;
end;


procedure TTestXPathEvaluate.TestExpressionObject;

var
  lScanner: TXPathScanner;
  lExpr: TXPathExpression;
  lRes: TXPathVariable;

begin
  lScanner:=TXPathScanner.Create('book[@id="b3"]/title');
  try
    lExpr:=TXPathExpression.Create(lScanner,True,FResolver);
    try
      lRes:=lExpr.Evaluate(FDoc.DocumentElement);
      try
        AssertEquals('First evaluation','Gamma',String(lRes.AsText));
      finally
        lRes.Free;
      end;
      lRes:=lExpr.Evaluate(FDoc.DocumentElement);
      try
        AssertEquals('An expression can be evaluated twice','Gamma',String(lRes.AsText));
      finally
        lRes.Free;
      end;
    finally
      lExpr.Free;
    end;
  finally
    lScanner.Free;
  end;
end;


procedure TTestXPathEvaluate.TestIncompleteExpressionAllowed;

var
  lScanner: TXPathScanner;
  lExpr: TXPathExpression;
  lRes: TXPathVariable;

begin
  lScanner:=TXPathScanner.Create('count(book) ]');
  try
    lExpr:=TXPathExpression.Create(lScanner,False);
    try
      AssertEquals('Scanner stops at the unparsed token',TokenName(tkRightSquareBracket),TokenName(lScanner.CurToken));
      lRes:=lExpr.Evaluate(FDoc.DocumentElement);
      try
        AssertEquals('Parsed part is evaluated',3,lRes.AsNumber,0);
      finally
        lRes.Free;
      end;
    finally
      lExpr.Free;
    end;
  finally
    lScanner.Free;
  end;
end;


procedure TTestXPathEvaluate.TestUnknownFunction;

begin
  AssertError('nosuchfunction()',EXPathEvaluationError,Format(SEvalUnknownFunction,['nosuchfunction']));
end;


procedure TTestXPathEvaluate.TestUnknownVariable;

begin
  AssertError('$nosuchvar',EXPathEvaluationError,Format(SEvalUnknownVariable,['nosuchvar']));
end;


procedure TTestXPathEvaluate.TestInvalidArgCount;

begin
  AssertError('count()',EXPathEvaluationError,SEvalInvalidArgCount);
  AssertError('true(1)',EXPathEvaluationError,SEvalInvalidArgCount);
end;


procedure TTestXPathEvaluate.TestNoConversion;

begin
  AssertError('count("x")',EXPathEvaluationError,Format(SVarNoConversion,[SString,SNodeSet]));
  AssertError('sum(1)',EXPathEvaluationError,Format(SVarNoConversion,[SNumber,SNodeSet]));
end;


procedure TTestXPathEvaluate.TestGarbageAfterExpression;

begin
  AssertError('book book',EXPathParserError,SParserGarbageAfterExpression);
end;


procedure TTestXPathEvaluate.TestMissingRightBracket;

begin
  AssertError('count(book',EXPathParserError,SParserExpectedRightBracket);
  AssertError('(1 + 2',EXPathParserError,SParserExpectedRightBracket);
end;


procedure TTestXPathEvaluate.TestMissingRightSquareBracket;

begin
  AssertError('book[1',EXPathParserError,SParserExpectedRightSquareBracket);
end;


procedure TTestXPathEvaluate.TestBadAxisName;

begin
  AssertError('nosuchaxis::book',EXPathParserError,SParserBadAxisName);
end;


procedure TTestXPathEvaluate.TestInvalidNodeTest;

begin
  AssertError('1 + )',EXPathParserError,SParserInvalidNodeTest);
  AssertError('book/[1]',EXPathParserError,SParserInvalidNodeTest);
end;


procedure TTestXPathEvaluate.TestUnionConversionError;

begin
  AssertError('1 | book',EXPathEvaluationError,Format(SVarNoConversion,[SNumber,SNodeSet]));
end;


procedure TTestXPathEvaluate.TestFilterConversionError;

begin
  AssertError('1[1]',EXPathEvaluationError,Format(SVarNoConversion,[SNumber,SNodeSet]));
end;


procedure TTestXPathEvaluate.TestNestedParseError;

begin
  AssertError('count(book[@id="b1" and (1 + )])',EXPathParserError,SParserInvalidNodeTest);
  AssertError('concat("a", book[1]/title, (2 * 3)',EXPathParserError,SParserExpectedRightBracket);
end;


procedure TTestXPathEvaluate.TestScannerErrorThroughParser;

begin
  AssertError('book[#]',EXPathLexerError,'Invalid character');
  AssertError('"open',EXPathLexerError,'String literal was not closed');
end;


initialization
  RegisterTests([TTestXPathCompat,TTestXPathEvaluate]);
end.
