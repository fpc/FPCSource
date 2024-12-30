{
    This file is part of the Free Component Library
    Copyright (c) 2024 by Michael Van Canneyt michael@freepascal.org

    YAML scanner unit tests

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit utyamlscanner;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, fpyaml.scanner;


Type

  { TTestYAMLScanner }

  TTestYAMLScanner= class(TTestCase)
  private
    FDocument : String;
    FInput : TStream;
    FLineBreak : String;
    FScanner: TYAMLScanner;
    FLastToken : TYAMLTokenData;
    procedure AssertEscape(aEscape, aResult: TYAMLString);
  protected
    procedure StartDocument(const aDoc: array of AnsiString);
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure StartDocument(aDoc : AnsiString);
    procedure ReadToken;
  Public
    class procedure AssertEquals(const msg: string; aExpected, aActual: TYAMLToken); overload;
    procedure AssertToken(const msg: string; aTokenData: TYAMLTokenData; aToken: TYAMLToken; aRow: Integer; aCol: Integer);
    function AssertToken(const msg: string; aToken: TYAMLToken; aRow: Integer; aCol: Integer): TYAMLTokenData;
    function AssertToken(const msg: string; aToken: TYAMLToken; const aValue: string; aRow: Integer; aCol: Integer): TYAMLTokenData;
    procedure AssertEOF;

    Property Document : String read FDocument;
    Property Scanner : TYAMLScanner Read FScanner;
    Property ScannerInput : TStream Read FInput;
    Property LineBreak : String Read FLineBreak;

  published
    procedure TestHookUp;
    procedure TestEmpty;
    procedure TestDocumentStart;
    procedure TestDocumentEnd;
    procedure TestComment;
    procedure TestAnchor;
    procedure TestAnchorFailNoSpace;
    procedure TestAlias;
    procedure TestAliasFailNoSpace;
    procedure TestScalar;
    procedure TestNumScalar;
    procedure TestSingleQuotedScalar;
    procedure TestSingleQuotedScalarSpaces;
    procedure TestDoubleQuotedScalar;
    procedure TestDoubleQuotedScalarSpacesNewline;
    procedure TestDoubleQuotedScalarError;
    procedure TestLiteralScalar;
    procedure TestLiteralScalar2;
    procedure TestFoldedScalar;
    procedure TestFoldedScalar2;
    procedure TestPlainMultilineScalar;
    procedure TestYAMLDirective;
    procedure TestTAGDirectiveLocal;
    procedure TestTAGDirectiveGlobal;
    procedure TestSequence;
    procedure TestFlowSequence;
    procedure TestFlowSequenceEnd;
    procedure TestSimplekey;
    procedure TestSimpleLongkey;
    procedure TestExplicitkey;
    procedure TestEscape0;
    procedure TestEscapeA;
    procedure TestEscapeB;
    procedure TestEscapeT;
    procedure TestEscapeNMin;
    procedure TestEscapeV;
    procedure TestEscapeF;
    procedure TestEscapeR;
    procedure TestEscapeE;
    procedure TestEscapeSpace;
    procedure TestEscapeQuote;
    procedure TestEscapeSlash;
    procedure TestEscapeBackSlash;
    procedure TestEscapeNMaj;
    procedure TestEscapeUnderscore;
    procedure TestEscapeL;
    procedure TestEscapeP;
    procedure TestEscapeX;
    procedure TestEscapeUMin;
    procedure TestEscapeUMAj;
  end;

implementation

uses typinfo;

{ TTestYAMLScanner }

procedure TTestYAMLScanner.SetUp;
begin
  inherited SetUp;
  FLineBreak:=#10;
  FDocument:='';
  FInput:=Nil;
  FScanner:=Nil;

end;

procedure TTestYAMLScanner.TearDown;
begin
  FDocument:='';
  FreeAndNil(FScanner);
  FreeAndNil(FInput);
  inherited TearDown;
end;




procedure TTestYAMLScanner.StartDocument(const aDoc: array of AnsiString);

var
  lDoc,S : String;

begin
  lDoc:='';
  for S in aDoc do
    begin
    if lDoc<>'' then
      lDoc:=lDoc+FLineBreak;
    lDoc:=lDoc+S;
    end;
  StartDocument(lDoc);
end;

procedure TTestYAMLScanner.StartDocument(aDoc: AnsiString);
begin
  FDocument:=aDoc;
  FInput:=TStringStream.Create(aDoc);
  FScanner:=TYAMLScanner.Create(FInput);
end;

procedure TTestYAMLScanner.ReadToken;
begin
  AssertNotNull('have scanner',Scanner);
  FLastToken:=FScanner.GetNextToken;
end;

procedure TTestYAMLScanner.TestHookUp;
begin
  AssertNull('No scanner',Scanner);
  AssertNull('No input',ScannerInput);
  AssertEquals('LineBreak',#10,LineBreak);
  AssertEquals('No document','',Document);
end;

class procedure TTestYAMLScanner.AssertEquals(const msg: string; aExpected,aActual: TYAMLToken);

begin
  AssertEquals(Msg,GetEnumName(TypeInfo(TYAMLToken),ord(aExpected)),
                   GetEnumName(TypeInfo(TYAMLToken),ord(aActual)));
end;

function TTestYAMLScanner.AssertToken(const msg: string; aToken: TYAMLToken; aRow: Integer; aCol: Integer) : TYAMLTokenData;

var
  lToken : TYAMLTokenData;

begin
  AssertNotNull(Scanner);
  lToken:=Scanner.GetNextToken;
  AssertToken(Msg,lToken,aToken,aRow,aCol);
  FLastToken:=lToken;
  Result:=lToken;
end;

procedure TTestYAMLScanner.AssertToken(const msg: string; aTokenData : TYAMLTokenData; aToken: TYAMLToken; aRow: Integer; aCol: Integer);

begin
  AssertEquals(msg+': Correct token',aToken,aTokenData.token);
  if aRow<>-1 then
    AssertEquals(msg+': Correct row',aRow,aTokenData.beginpos.Line);
  if aCol<>-1 then
    AssertEquals(msg+': Correct col',aCol,aTokenData.beginpos.Column);
end;

function TTestYAMLScanner.AssertToken(const msg: string; aToken: TYAMLToken; const aValue: string; aRow: Integer; aCol: Integer) : TYAMLTokenData;
var
  lToken : TYAMLTokenData;

begin
  AssertNotNull(Scanner);
  lToken:=Scanner.GetNextToken;
  AssertToken(Msg,lToken,aToken,aRow,aCol);
  AssertEquals(Msg+' token value',aValue,lToken.value);
  FLastToken:=lToken;
  Result:=lToken;
end;

procedure TTestYAMLScanner.AssertEOF;
begin
  AssertToken('Empty',ytEOF,-1,-1);
end;

procedure TTestYAMLScanner.TestEmpty;
begin
  StartDocument('');
  AssertToken('Empty',ytEOF,0,0);
end;

procedure TTestYAMLScanner.TestDocumentStart;
begin
  StartDocument('---');
  AssertToken('Document start',ytDocumentStart,1,1);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestDocumentEnd;
begin
  StartDocument('...');
  AssertToken('Document start',ytDocumentEnd,1,1);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestComment;
begin
  StartDocument('#');
  AssertToken('Empty',ytEOF,1,0);
end;

procedure TTestYAMLScanner.TestAnchor;
begin
  StartDocument('&one ');
  AssertToken('anchor',ytAnchor,'one',1,2);
end;

procedure TTestYAMLScanner.TestAnchorFailNoSpace;
begin
  StartDocument('&one');
  AssertException('Need space',EYAMLScanner,@ReadToken);
end;

procedure TTestYAMLScanner.TestAlias;
begin
  StartDocument('*one ');
  AssertToken('Alias',ytAlias,'one',1,2);
end;

procedure TTestYAMLScanner.TestAliasFailNoSpace;
begin
  StartDocument('*alias');
  AssertException('Need space',EYAMLScanner,@ReadToken);
end;

procedure TTestYAMLScanner.TestScalar;
begin
  StartDocument('one');
  AssertToken('scalar',ytScalarPlain,'one',1,1);
end;

procedure TTestYAMLScanner.TestNumScalar;
begin
  StartDocument('123');
  AssertToken('scalar',ytScalarPlain,'123',1,1);
end;

procedure TTestYAMLScanner.TestSingleQuotedScalar;
begin
  StartDocument('''123''');
  AssertToken('scalar',ytScalarSingle,'123',1,1);
end;

procedure TTestYAMLScanner.TestSingleQuotedScalarSpaces;
begin
  StartDocument('''123 456''');
  AssertToken('scalar',ytScalarSingle,'123 456',1,1);
end;

procedure TTestYAMLScanner.TestDoubleQuotedScalarSpacesNewline;
begin
  StartDocument('"123 456\'#10'  \ abc def"');
  AssertToken('scalar',ytScalarDouble,'123 456 abc def',1,1);
end;

procedure TTestYAMLScanner.TestDoubleQuotedScalar;
begin
  StartDocument('"123"');
  AssertToken('scalar',ytScalarDouble,'123',1,1);
end;

procedure TTestYAMLScanner.TestDoubleQuotedScalarError;
begin
  StartDocument('"\"');
  AssertException('End of stream',EYAMLScanner,@ReadToken);
end;

procedure TTestYAMLScanner.TestLiteralScalar;
begin
  // Example 8.7
  StartDocument(['|',
                 ' literal',
                 ' '#9'text',
                 '']);
  AssertToken('Literal scalar',ytScalarLiteral,'literal'#10#9'text'#10,1,1);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestLiteralScalar2;
begin
  // Example 8.8
  StartDocument(['|',
                 ' ',
                 '  ',
                 '  literal',
                 '   ',
                 '  ',
                 '  text',
                 '',
                 ' # Comment']);
  AssertToken('Literal scalar',ytScalarLiteral,#10#10'literal'#10' '#10#10'text'#10,1,1);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestFoldedScalar;
begin
  // Example 8.9
  StartDocument(['>',
                 ' folded',
                 ' text',
                 '']);
  AssertToken('Literal scalar',ytScalarFolded,'folded text'#10,1,1);
  AssertEOF;
end;


procedure TTestYAMLScanner.TestFoldedScalar2;
begin
  // Example 8.10
  // The example seems wrong in the sense that none of the scanners I tried
  // returns a space for the empty line between bullet and list.
  // Adapted the test accordingly
  StartDocument(['>',
                 '',
                 ' folded',
                 ' line',
                 '',
                 ' next',
                 ' line',
                 '   * bullet',
                 '',
                 '   * list',
                 '   * lines',
                 '',
                 ' last',
                 ' line',
                 '',
                 '# comment']);
  AssertToken('Literal scalar',ytScalarFolded,#10'folded line'#10+
                                             'next line'#10+
                                             '  * bullet'#10+
                                             ''#10+
                                             '  * list'#10+
                                             '  * lines'#10+
                                             #10+
                                             'last line'#10,1,1);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestPlainMultilineScalar;
begin
  StartDocument(['5',
                 '  00']);
  AssertToken('Plain scalar',ytScalarPlain,'5 00',1,1);
end;


procedure TTestYAMLScanner.TestYAMLDirective;
var
  lToken : TYAMLTokenData;
begin
  StartDocument('%YAML 1.2');
  lToken:=AssertToken('Directive',ytVersionDirective,'1',1,1);
  AssertEquals('minor','2',lToken.Value2);
end;

procedure TTestYAMLScanner.TestTAGDirectiveLocal;

var
  lToken : TYAMLTokenData;

begin
  StartDocument('%TAG !me! !you');
  lToken:=AssertToken('Tag directive',ytTagDirective,'!me!',1,1);
  AssertEquals('local prefix','!you',lToken.Value2);
end;

procedure TTestYAMLScanner.TestTAGDirectiveGlobal;
var
  lToken : TYAMLTokenData;

begin
  StartDocument('%TAG !me! tag:example.com,2000:app/');
  lToken:=AssertToken('Tag directive',ytTagDirective,'!me!',1,1);
  AssertEquals('local prefix','tag:example.com,2000:app/',lToken.Value2);
end;

procedure TTestYAMLScanner.TestSequence;
begin
  StartDocument('- ');
  AssertToken('Sequence start',ytBlockSequenceStart,'',1,1);
  AssertFalse('Block context',scanner.InFlowContext);
end;

procedure TTestYAMLScanner.TestFlowSequence;
begin
  StartDocument('[');
  AssertToken('Flow sequence start',ytFlowSequenceStart,'',1,1);
  AssertTrue('Flow context',scanner.InFlowContext);
end;

procedure TTestYAMLScanner.TestFlowSequenceEnd;
begin
  StartDocument(']');
  AssertToken('Flow sequence end',ytFlowSequenceEnd,'',1,1);
  AssertFalse('Block context',scanner.InFlowContext);
end;

procedure TTestYAMLScanner.TestSimplekey;
begin
  StartDocument('key: ');
  AssertToken('Start mapping',ytBlockMappingStart,'',1,1);
  AssertFalse('Block context',scanner.InFlowContext);
  AssertToken('Key directive',ytKey,'',1,1);
  AssertToken('scalar key value',ytScalarPlain,'key',1,1);
  AssertToken('value directive',ytValue,'',1,4);
  AssertToken('end mapping',ytBlockEnd,'',1,0);
  AssertFalse('Block context',scanner.InFlowContext);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestSimpleLongkey;
begin
  StartDocument('long key: ');
  AssertToken('Start mapping',ytBlockMappingStart,'',1,1);
  AssertFalse('Block context',scanner.InFlowContext);
  AssertToken('Key directive',ytKey,'',1,1);
  AssertToken('scalar key value',ytScalarPlain,'long key',1,1);
  AssertToken('value directive',ytValue,'',1,9);
  AssertToken('end mapping',ytBlockEnd,'',1,0);
  AssertFalse('Block context',scanner.InFlowContext);
  AssertEOF;
end;

procedure TTestYAMLScanner.TestExplicitkey;
begin
  StartDocument('? key');
  AssertToken('Start mapping',ytBlockMappingStart,'',1,1);
  AssertFalse('Block context',scanner.InFlowContext);
  AssertToken('Key directive',ytKey,'',1,1);
  AssertToken('scalar key value',ytScalarPlain,'key',1,3);
  AssertToken('end mapping',ytBlockEnd,'',1,0);
  AssertFalse('Block context',scanner.InFlowContext);
  AssertToken('end',ytEOF,'',1,0);
end;

procedure TTestYAMLScanner.AssertEscape(aEscape,aResult : TYAMLString);
begin
  StartDocument('"a\'+aEscape+'"');
  AssertToken('Token',ytScalarDouble,'a'+aResult,1,1);
end;

procedure TTestYAMLScanner.TestEscape0;
begin
  AssertEscape('0',#0);
end;

procedure TTestYAMLScanner.TestEscapeA;
begin
  AssertEscape('a',#7);
end;

procedure TTestYAMLScanner.TestEscapeB;
begin
  AssertEscape('b',#8);
end;

procedure TTestYAMLScanner.TestEscapeT;
begin
  AssertEscape('t',#9);
end;

procedure TTestYAMLScanner.TestEscapeNMin;
begin
  AssertEscape('n',#10);
end;

procedure TTestYAMLScanner.TestEscapeV;
begin
  AssertEscape('v',#11);
end;

procedure TTestYAMLScanner.TestEscapeF;
begin
  AssertEscape('f',#12);
end;

procedure TTestYAMLScanner.TestEscapeR;
begin
  AssertEscape('r',#13);
end;

procedure TTestYAMLScanner.TestEscapeE;
begin
  AssertEscape('e',#$1B);
end;

procedure TTestYAMLScanner.TestEscapeSpace;
begin
  AssertEscape(' ',' ');
end;

procedure TTestYAMLScanner.TestEscapeQuote;
begin
  AssertEscape('"','"');
end;

procedure TTestYAMLScanner.TestEscapeSlash;
begin
  AssertEscape('/','/');
end;

procedure TTestYAMLScanner.TestEscapeBackSlash;
begin
  AssertEscape('\','\');
end;

procedure TTestYAMLScanner.TestEscapeNMaj;
begin
  AssertEscape('N',#$C2#$85);
end;

procedure TTestYAMLScanner.TestEscapeUnderscore;
begin
  AssertEscape('_',#$C2#$A0);
end;

procedure TTestYAMLScanner.TestEscapeL;
begin
  AssertEscape('L',#$E2#$80#$A8);
end;

procedure TTestYAMLScanner.TestEscapeP;
begin
  AssertEscape('P',#$E2#$80#$A9);
end;

procedure TTestYAMLScanner.TestEscapeX;
begin
  AssertEscape('xA0',#$C2#$A0);
end;

procedure TTestYAMLScanner.TestEscapeUMin;
begin
  AssertEscape('u2029',#$E2#$80#$A9);
end;

procedure TTestYAMLScanner.TestEscapeUMAj;
begin
  AssertEscape('U000E0030',#$F3#$A0#$80#$B0);
end;



initialization
  RegisterTest(TTestYAMLScanner);
end.

