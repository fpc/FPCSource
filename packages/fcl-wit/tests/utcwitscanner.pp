{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 Michael Van Canneyt (michael@freepascal.org)

    Test WIT scanner

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit utcwitscanner;

interface

uses
  TestUtils, FPCUnit, Classes, SysUtils, testregistry, streamex, WIT.Scanner;

type

  { TWITScannerTests }

  TWITScannerTests = class(TTestCase)
  Private
    FToken: TToken;
  protected
    Procedure TestToken(aSource: string; aToken: TTokenType);
  public
    Procedure SetUp; override;
    class procedure AssertEquals(Msg : string; aExpected,aActual : TTokenType); overload;
  published
    procedure TestUnknown;
    procedure TestWhitespace;
    procedure TestEOF;
    procedure TestOpenRoundBrace;
    procedure TestCloseRoundBrace;
    procedure TestOpenCurlyBrace;
    procedure TestCloseCurlyBrace;
    procedure TestOpenSquareBrace;
    procedure TestCloseSquareBrace;
    procedure TestColon;
    procedure TestSemicolon;
    procedure TestComma;
    procedure TestEqual;
    procedure TestDot;
    procedure TestPlus;
    procedure TestMinus;
    procedure TestStar;
    procedure TestDiv;
    procedure TestLessThan;
    procedure TestGreaterThan;
    procedure TestAt;
    procedure TestArrow;
    procedure TestNumber;
    procedure TestString;
    procedure TestInterface;
    procedure TestRecord;
    procedure TestFlags;
    procedure TestEnum;
    procedure TestType;
    procedure TestResource;
    procedure TestImport;
    procedure TestExport;
    procedure TestUse;
    procedure TestWorld;
    procedure TestPackage;
    procedure TestAs;
    procedure TestAsync;
    procedure TestBool;
    procedure TestBorrow;
    procedure TestChar;
    procedure TestConstructor;
    procedure TestFrom;
    procedure TestFunc;
    procedure TestFuture;
    procedure TestInclude;
    procedure TestList;
    procedure TestOption;
    procedure TestOwn;
    procedure TestResult;
    procedure TestStatic;
    procedure TestStream;
    procedure TestStringKW;
    procedure TestTuple;
    procedure TestVariant;
    procedure TestWith;
    procedure TestIdentifier;
    procedure TestIdentifierEscapedKeyword;
    procedure TestIdentifierDash;
    Procedure TestSingleComment;
    Procedure TestMultiComment;
    Procedure TestMultiComment2;
    Procedure TestMultiCommentNested;
  end;

implementation

uses TypInfo;

{ TWITScannerTests }

procedure TWITScannerTests.TestToken(aSource: string; aToken: TTokenType);
var
  Scanner: TWITScanner;
  Reader: TTextReader;
  lToken : TToken;
begin
  Reader := TStringReader.Create(aSource);
  try
    Scanner := TWITScanner.Create(Reader);
    try
      Reader:=nil;
      FToken := Scanner.GetToken;
      AssertEquals('Token type mismatch', aToken, FToken.TokenType);
      if FToken.TokenType<>ttEOF then
        begin
        LToken := Scanner.GetToken;
        AssertEquals('EOF reached',ttEOF,lToken.TokenType);
        end;
    finally
      Scanner.Free;
    end;
  finally
    Reader.Free;
  end;
end;

procedure TWITScannerTests.SetUp;
begin
  inherited SetUp;
  FToken:=Default(TToken);
end;

class procedure TWITScannerTests.AssertEquals(Msg: string; aExpected,
  aActual: TTokenType);
begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TTokenType),ord(aExpected)),
                   GetEnumName(TypeInfo(TTokenType),ord(aActual)));
end;

procedure TWITScannerTests.TestUnknown;
begin
  TestToken('?', ttUnknown);
end;

procedure TWITScannerTests.TestWhitespace;
begin
  TestToken('  ', ttWhitespace);
end;

procedure TWITScannerTests.TestEOF;
begin
  TestToken('', ttEOF);
end;

procedure TWITScannerTests.TestOpenRoundBrace;
begin
  TestToken('(', ttOpenRoundBrace);
end;

procedure TWITScannerTests.TestCloseRoundBrace;
begin
  TestToken(')', ttCloseRoundBrace);
end;

procedure TWITScannerTests.TestOpenCurlyBrace;
begin
  TestToken('{', ttOpenCurlyBrace);
end;

procedure TWITScannerTests.TestCloseCurlyBrace;
begin
  TestToken('}', ttCloseCurlyBrace);
end;

procedure TWITScannerTests.TestOpenSquareBrace;
begin
  TestToken('[', ttOpenSquareBrace);
end;

procedure TWITScannerTests.TestCloseSquareBrace;
begin
  TestToken(']', ttCLoseSquareBrace);
end;

procedure TWITScannerTests.TestColon;
begin
  TestToken(':', ttColon);
end;

procedure TWITScannerTests.TestSemicolon;
begin
  TestToken(';', ttSemicolon);
end;

procedure TWITScannerTests.TestComma;
begin
  TestToken(',', ttComma);
end;

procedure TWITScannerTests.TestEqual;
begin
  TestToken('=', ttEqual);
end;

procedure TWITScannerTests.TestDot;
begin
  TestToken('.', ttDot);
end;

procedure TWITScannerTests.TestPlus;
begin
  TestToken('+', ttPlus);
end;

procedure TWITScannerTests.TestMinus;
begin
  TestToken('-', ttMinus);
end;

procedure TWITScannerTests.TestStar;
begin
  TestToken('*', ttStar);
end;

procedure TWITScannerTests.TestDiv;
begin
  TestToken('/', ttDiv);
end;

procedure TWITScannerTests.TestLessThan;
begin
  TestToken('<', ttLessThan);
end;

procedure TWITScannerTests.TestGreaterThan;
begin
  TestToken('>', ttGreaterThan);
end;

procedure TWITScannerTests.TestAt;
begin
  TestToken('@', ttAt);
end;

procedure TWITScannerTests.TestArrow;
begin
  TestToken('->', ttArrow);
end;

procedure TWITScannerTests.TestNumber;
begin
  TestToken('123', ttNumber);
end;

procedure TWITScannerTests.TestString;
begin
  TestToken('"test"', ttStringLiteral);
end;

procedure TWITScannerTests.TestInterface;
begin
  TestToken('interface', ttInterface);
end;

procedure TWITScannerTests.TestRecord;
begin
  TestToken('record', ttRecord);
end;

procedure TWITScannerTests.TestFlags;
begin
  TestToken('flags', ttFlags);
end;

procedure TWITScannerTests.TestEnum;
begin
  TestToken('enum', ttEnum);
end;

procedure TWITScannerTests.TestType;
begin
  TestToken('type', ttType);
end;

procedure TWITScannerTests.TestResource;
begin
  TestToken('resource', ttResource);
end;

procedure TWITScannerTests.TestImport;
begin
  TestToken('import', ttImport);
end;

procedure TWITScannerTests.TestExport;
begin
  TestToken('export', ttExport);
end;

procedure TWITScannerTests.TestUse;
begin
  TestToken('use', ttUse);
end;

procedure TWITScannerTests.TestWorld;
begin
  TestToken('world', ttWorld);
end;

procedure TWITScannerTests.TestPackage;
begin
  TestToken('package', ttPackage);
end;

procedure TWITScannerTests.TestAs;
begin
  TestToken('as', ttAs);
end;

procedure TWITScannerTests.TestAsync;
begin
  TestToken('async', ttAsync);
end;

procedure TWITScannerTests.TestBool;
begin
  TestToken('bool', ttBool);
end;

procedure TWITScannerTests.TestBorrow;
begin
  TestToken('borrow', ttBorrow);
end;

procedure TWITScannerTests.TestChar;
begin
  TestToken('char', ttChar);
end;

procedure TWITScannerTests.TestConstructor;
begin
  TestToken('constructor', ttConstructor);
end;

procedure TWITScannerTests.TestFrom;
begin
  TestToken('from', ttFrom);
end;

procedure TWITScannerTests.TestFunc;
begin
  TestToken('func', ttFunc);
end;

procedure TWITScannerTests.TestFuture;
begin
  TestToken('future', ttFuture);
end;

procedure TWITScannerTests.TestInclude;
begin
  TestToken('include', ttInclude);
end;

procedure TWITScannerTests.TestList;
begin
  TestToken('list', ttList);
end;

procedure TWITScannerTests.TestOption;
begin
  TestToken('option', ttOption);
end;

procedure TWITScannerTests.TestOwn;
begin
  TestToken('own', ttOwn);
end;

procedure TWITScannerTests.TestResult;
begin
  TestToken('result', ttResult);
end;

procedure TWITScannerTests.TestStatic;
begin
  TestToken('static', ttStatic);
end;

procedure TWITScannerTests.TestStream;
begin
  TestToken('stream', ttStream);
end;

procedure TWITScannerTests.TestStringKW;
begin
  TestToken('string', ttStringType);
end;

procedure TWITScannerTests.TestTuple;
begin
  TestToken('tuple', ttTuple);
end;

procedure TWITScannerTests.TestVariant;
begin
  TestToken('variant', ttVariant);
end;

procedure TWITScannerTests.TestWith;
begin
  TestToken('with', ttWith);
end;

procedure TWITScannerTests.TestIdentifier;
begin
  TestToken('myIdentifier', ttIdentifier);
end;

procedure TWITScannerTests.TestIdentifierEscapedKeyword;
begin
  TestToken('%with', ttIdentifier);
  AssertEquals('Escape not part of token','with',FToken.Value);
end;

procedure TWITScannerTests.TestIdentifierDash;
begin
  TestToken('ali-baba', ttIdentifier);
  AssertEquals('dash part of token','ali-baba',FToken.Value);
end;


procedure TWITScannerTests.TestSingleComment;
begin
  TestToken('// A comment', ttComment);
  AssertEquals('Comment',' A comment',FToken.Value);
end;

procedure TWITScannerTests.TestMultiComment;
begin
  TestToken('/* A comment*/', ttComment);
  AssertEquals('Comment',' A comment',FToken.Value);
end;

procedure TWITScannerTests.TestMultiComment2;
begin
  TestToken('/* A'#10'comment*/', ttComment);
  AssertEquals('Comment',' A'#10'comment',FToken.Value);
end;

procedure TWITScannerTests.TestMultiCommentNested;
begin
  TestToken('/* A /* second */ comment*/', ttComment);
  AssertEquals('Comment',' A /* second */ comment',FToken.Value);
end;

initialization
  RegisterTest(TWITScannerTests);

end.

