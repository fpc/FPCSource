{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022- by Michael Van Canneyt (michael@freepascal.org)

    This file contains the tests for the CSS parser

    See the File COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit tcCSSParser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry, fpcssparser, fpcsstree;

type

  { TTestBaseCSSParser }

  TTestBaseCSSParser = class(TTestCase)
  Private
    FParseResult: TCSSElement;
    FSource : TStringStream;
    FParser : TCSSParser;
    FToFree: TCSSElement;
    procedure clear;
    function GetRule: TCSSRuleElement;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure CreateParser(Const ASource : string);
    procedure Parse;
    procedure Parse(Const aSource : String);
    function ParseRule(Const aSource : String) :  TCSSRuleElement;
    procedure AssertEquals(AMessage: String; AExpected, AActual: TCSSUnits);   overload;
    procedure AssertEquals(AMessage: String; AExpected, AActual: TCSSBinaryOperation);   overload;
    Function CheckClass(Const aMsg : String; aExpectedClass : TCSSElementClass; aActual : TCSSElement) : TCSSElement;
    Function CheckDeclaration(aRule : TCSSRuleElement; aIndex : Integer) : TCSSDeclarationElement;
    Function CheckDeclaration(aRule : TCSSRuleElement; aIndex : Integer; const AKey : String) : TCSSDeclarationElement;
    Function CheckSelector(aRule : TCSSRuleElement; aIndex : Integer) : TCSSElement;
    Function CheckSelector(aRule : TCSSRuleElement; aIndex : Integer; const aName : String) : TCSSElement;
    function CheckList(aList: TCSSListElement; aIndex: Integer): TCSSElement;
    function CheckList(aList: TCSSListElement; aIndex: Integer; const aName: String): TCSSElement;
    function CheckLiteral(Msg: String; aEl: TCSSelement; aValue: String) : TCSSStringElement; overload;
    function CheckLiteral(Msg: String; aEl: TCSSelement; aValue: Integer) : TCSSIntegerElement;  overload;
    function CheckLiteral(Msg: String; aEl: TCSSelement; aValue: Integer; AUnits : TCSSUnits) : TCSSIntegerElement;  overload;
    Function GetCalArg(aCall : TCSSCallElement; aIndex : Integer) : TCSSElement;
  Public
    Property ParseResult : TCSSElement read FParseResult;
    Property FirstRule : TCSSRuleElement Read GetRule;
    Property ToFree : TCSSElement Read FToFree Write FToFree;
  end;

  { TTestCSSParser }

  TTestCSSParser = class(TTestBaseCSSParser)
  private
  Published
    Procedure TestEmpty;
    Procedure TestEmptyRule;
    Procedure TestPrefixedEmptyRule;
    Procedure TestClassPrefixedEmptyRule;
    Procedure TestHashPrefixedEmptyRule;
    procedure TestDoublePrefixedEmptyRule;
    procedure TestDoubleMixedPrefixedEmptyRule;
    procedure TestAttributePrefixedEmptyRule;
    procedure TestPseudoPrefixedEmptyRule;
    procedure TestPseudoFunctionEmptyRule;
    procedure TestFuncPrefixedEmptyRule;
    procedure TestQueryPrefixedEmptyRule;
    Procedure TestCommaPrefixedEmptyRule;
    Procedure TestOneDeclarationIDValue;
    Procedure TestOneDeclarationIDValueAndEmpty;
    Procedure TestOneDeclarationIntValue;
    Procedure TestOneDeclarationStringValue;
    Procedure TestOneDeclarationHashValue;
    Procedure TestOneDeclarationURLValue;
    Procedure TestOneDeclarationMultiValue;
    Procedure TestOneDeclarationMultiListValue;
    Procedure TestOneDeclarationExprValue;
    Procedure TestOneDeclarationUnicodeRangeValue;
    Procedure TestOneDeclarationNoColon;
    Procedure TestTwoDeclarationNoColon;
    Procedure TestOneEmptyDeclaration;
    Procedure TestImportAtKeyWord;
    Procedure TestMediaPrint;
    Procedure TestSupportsFunction;
  end;

  { TTestCSSFilesParser }

  TTestCSSFilesParser = class(TTestBaseCSSParser)
  private
    FTestDir: String;
    procedure SetTestDir(AValue: String);
  Public
    Procedure SetUp;override;
    Procedure RunFileTest(aFile : String='');
    Property TestDir : String Read FTestDir Write SetTestDir;
  Published
    // lowercase name must match 'test'+filename
    Procedure Testabsolute;
    Procedure Testanimation;
    Procedure Testanon;
    Procedure Testbigbig;
    Procedure Testclass;
    Procedure Testcolor;
    Procedure Testfont_face;
    Procedure Testfont_face2;
    Procedure Testfont;
    Procedure Testhello;
    Procedure Testid;
    Procedure Testinput_type;
    Procedure Testmargin;
    Procedure Testmedia_query;
    Procedure Testmystyle;
    Procedure Testnews;
    Procedure Testpadding;
    Procedure Teststyle;
    Procedure Teststyle2;
    Procedure Teststyle_big;
    Procedure TesTwildcard;
  end;

implementation

uses inifiles, typinfo;

{ TTestCSSFilesParser }

procedure TTestCSSFilesParser.SetTestDir(AValue: String);
begin
  if FTestDir=AValue then Exit;
  FTestDir:=AValue;
end;

procedure TTestCSSFilesParser.SetUp;
begin
  inherited SetUp;
  With TMemIniFile.Create(ChangeFileExt(Paramstr(0),'.ini')) do
    try
      TestDir:=ReadString('CSS','SourceDir','css');
    finally
      Free
    end;
end;

procedure TTestCSSFilesParser.RunFileTest(aFile: String);

var
  fn : string;
  OK : Boolean;

begin
  if Afile='' then
    begin
    aFile:=LowerCase(TestName);
    if Copy(aFile,1,4)='test' then
      Delete(aFile,1,4);
    end;
  OK:=False;
  With TStringList.Create do
    try
      fn:=IncludeTrailingPathDelimiter(TestDir)+aFile+'.css';
      fn:=ExpandFileName(FN);
      // Writeln('Analysing file ',FN);
      LoadFromFile(fn);
      Parse(Text);
      OK:=True;
    finally
      if not OK then
        begin
        Writeln('Source generating error: ',FN);
        Writeln(Text);
        end;
      Free;
    end;
end;


procedure TTestCSSFilesParser.Testabsolute;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testanimation;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testanon;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testbigbig;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testclass;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testcolor;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testfont_face;
begin
  RunFileTest('font-face');
end;

procedure TTestCSSFilesParser.Testfont_face2;
begin
  RunFileTest('font-face2');
end;

procedure TTestCSSFilesParser.Testfont;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testhello;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testid;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testinput_type;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testmargin;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testmedia_query;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testmystyle;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testnews;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Testpadding;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Teststyle;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Teststyle2;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.Teststyle_big;
begin
  RunFileTest;
end;

procedure TTestCSSFilesParser.TesTwildcard;
begin
  RunFileTest;
end;

{ TTestCSSParser }

procedure TTestCSSParser.TestEmpty;
var
  L : TCSSCompoundElement;
begin
  Parse('');
  L:=TCSSCompoundElement(CheckClass('list',TCSSCompoundElement,ParseResult));
  AssertEquals('No children',0,L.ChildCount);
end;

procedure TTestCSSParser.TestEmptyRule;

var
  R : TCSSRuleElement;
begin
  R:=ParseRule('{}');
  AssertEquals('No rule children',0,R.ChildCount);
end;

procedure TTestCSSParser.TestPrefixedEmptyRule;

var
  R : TCSSRuleElement;
  sel: TCSSIdentifierElement;

begin
  ParseRule('a { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,R.Selectors[0]));
  AssertEquals('Sel name','a',Sel.Value);
end;

procedure TTestCSSParser.TestClassPrefixedEmptyRule;

var
  R : TCSSRuleElement;
  sel: TCSSClassNameElement;

begin
  ParseRule('.a { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSClassNameElement(CheckClass('Selector', TCSSClassNameElement,R.Selectors[0]));
  AssertEquals('Sel name','.a',Sel.Value);
end;

procedure TTestCSSParser.TestHashPrefixedEmptyRule;
var
  R : TCSSRuleElement;
  sel: TCSSStringElement;

begin
  ParseRule('#a { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSStringElement(CheckClass('Selector', TCSSStringElement,R.Selectors[0]));
  AssertEquals('Sel name','#a',Sel.Value);
end;

procedure TTestCSSParser.TestDoublePrefixedEmptyRule;

var
  R : TCSSRuleElement;
  sel: TCSSIdentifierElement;
  List : TCSSListElement;

begin
  ParseRule('a b { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  List:=TCSSListElement(CheckClass('Selector', TCSSListElement,R.Selectors[0]));
  AssertEquals('selector list count',2,List.ChildCount);
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,List[0]));
  AssertEquals('Sel 1 name','a',Sel.Value);
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,List[1]));
  AssertEquals('Sel 2 name','b',Sel.Value);
end;

procedure TTestCSSParser.TestDoubleMixedPrefixedEmptyRule;

var
  R : TCSSRuleElement;
  sel: TCSSIdentifierElement;
  List : TCSSListElement;

begin
  ParseRule('a .b { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  List:=TCSSListElement(CheckClass('Selector', TCSSListElement,R.Selectors[0]));
  AssertEquals('selector list count',2,List.ChildCount);
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,List[0]));
  AssertEquals('Sel 1 name','a',Sel.Value);
  sel:=TCSSClassNameElement(CheckClass('Selector', TCSSClassNameElement,List[1]));
  AssertEquals('Sel 2 name','.b',Sel.Value);
end;

procedure TTestCSSParser.TestAttributePrefixedEmptyRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  id : TCSSIdentifierElement;
  bin : TCSSBinaryElement;

begin
  ParseRule('a[b="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSArrayElement(CheckClass('Selector', TCSSArrayElement,R.Selectors[0]));
  Id:=TCSSIdentifierElement(CheckClass('Array prefix',TCSSIdentifierElement,Sel.Prefix));
  AssertEquals('Prefix name','a',Id.Value);
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boEquals,Bin.Operation);
end;

procedure TTestCSSParser.TestPseudoPrefixedEmptyRule;
var
  R : TCSSRuleElement;
  Sel : TCSSPseudoClassElement;

begin
  ParseRule(':a { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSPseudoClassElement(CheckClass('Selector', TCSSPseudoClassElement,R.Selectors[0]));
  AssertEquals('Pseudo name',':a',sel.Value);
end;

procedure TTestCSSParser.TestPseudoFunctionEmptyRule;
var
  R : TCSSRuleElement;
  Sel : TCSSCallElement;
  Id : TCSSIdentifierElement;

begin
  ParseRule(':a(b) { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSCallElement(CheckClass('Selector', TCSSCallElement,R.Selectors[0]));
  AssertEquals('Pseudo name',':a',sel.Name);
  AssertEquals('argument count',1,Sel.ChildCount);
  Id:=TCSSIdentifierElement(CheckClass('Argument 1',TCSSIdentifierElement,Sel[0]));
  AssertEquals('Argument name','b',id.Name);
end;

procedure TTestCSSParser.TestFuncPrefixedEmptyRule;

var
  R : TCSSRuleElement;
  List : TCSSListElement;

begin
  R:=ParseRule('input:enabled:read-write:-webkit-any(:focus,:hover)::-webkit-clear-button {  }');
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  List:=TCSSListElement(CheckClass('List',TCSSListElement,R.Selectors[0]));
  CheckList(List,0,'input');
  CheckList(List,1,':enabled');
  CheckList(List,2,':read-write');
  CheckList(List,4,'::-webkit-clear-button');
end;

procedure TTestCSSParser.TestQueryPrefixedEmptyRule;
var
  R : TCSSRuleElement;
begin
  R:=ParseRule('@media only screen and (-webkit-min-device-pixel-ratio: 2), only screen and (min-device-pixel-ratio: 3) { }');
end;

procedure TTestCSSParser.TestCommaPrefixedEmptyRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;

begin
  R:=ParseRule('#facebox .tl,#facebox .tl { }');
end;

procedure TTestCSSParser.TestOneDeclarationIDValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  Id : TCSSIdentifierElement;

begin
  R:=ParseRule('{ a : b; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  ID:=TCSSIdentifierElement(CheckClass('Value', TCSSIdentifierElement,D.Children[0]));
  AssertEquals('Value','b',id.Value);
end;

procedure TTestCSSParser.TestOneDeclarationIDValueAndEmpty;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  Id : TCSSIdentifierElement;

begin
  R:=ParseRule('{ a : b;; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  ID:=TCSSIdentifierElement(CheckClass('Value', TCSSIdentifierElement,D.Children[0]));
  AssertEquals('Value','b',id.Value);
end;

procedure TTestCSSParser.TestOneDeclarationIntValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  U : TCSSUnits;

begin
  For U in TCSSUnits do
    begin
    R:=ParseRule('{ a : 1'+CSSUnitNames[U]+'; }');
    AssertEquals('selector count',0,R.SelectorCount);
    D:=CheckDeclaration(R,0,'a');
    AssertEquals('Value count', 1, D.ChildCount);
    CheckLiteral('Value for '+CSSUnitNames[U],D.Children[0],1,U);
    end;
end;

procedure TTestCSSParser.TestOneDeclarationStringValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;

begin
  R:=ParseRule('{ a : "b"; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  CheckLiteral('Value',D.Children[0],'b');
end;

procedure TTestCSSParser.TestOneDeclarationHashValue;

var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  S : TCSSStringElement;

begin
  R:=ParseRule('{ a : #ABABAB; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  S:=TCSSStringElement(CheckClass('Value', TCSSStringElement,D.Children[0]));
  AssertEquals('Value ','#ABABAB',S.Value);
end;

procedure TTestCSSParser.TestOneDeclarationURLValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  U : TCSSURLElement;

begin
  R:=ParseRule('{ a : url("b.c"); }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  U:=TCSSURLElement(CheckClass('Value', TCSSURLElement,D.Children[0]));
  AssertEquals('Value ','b.c',U.Value);
end;

procedure TTestCSSParser.TestOneDeclarationMultiValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  L : TCSSListElement;

begin
  R:=ParseRule('{ a : 1px 2px 3px 4px; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  L:=TCSSListElement(CheckClass('List',TCSSListElement,D.Children[0]));
  AssertEquals('List element count', 4, L.ChildCount);
  CheckLiteral('Value 1 ',L.Children[0],1,cuPX);
  CheckLiteral('Value 2 ',L.Children[1],2,cuPX);
  CheckLiteral('Value 3 ',L.Children[2],3,cuPX);
  CheckLiteral('Value 4 ',L.Children[3],4,cuPX);
end;

procedure TTestCSSParser.TestOneDeclarationMultiListValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  L : TCSSListElement;

begin
  R:=ParseRule('{ a : 1px 2px, 3px 4px; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 2, D.ChildCount);
  L:=TCSSListElement(CheckClass('List',TCSSListElement,D.Children[0]));
  AssertEquals('List element count', 2, L.ChildCount);
  CheckLiteral('Value 1 ',L.Children[0],1,cuPX);
  CheckLiteral('Value 2 ',L.Children[1],2,cuPX);
  L:=TCSSListElement(CheckClass('List',TCSSListElement,D.Children[1]));
  AssertEquals('List element count', 2, L.ChildCount);
  CheckLiteral('Value 3 ',L.Children[0],3,cuPX);
  CheckLiteral('Value 4 ',L.Children[1],4,cuPX);
end;

procedure TTestCSSParser.TestOneDeclarationExprValue;
begin
  // Todo
end;

procedure TTestCSSParser.TestOneDeclarationUnicodeRangeValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
begin
  R:=ParseRule('{ unicode-range: U+0400-045F, U+0490-0491, U+04B0-04B1, U+2116; }');
  D:=CheckDeclaration(R,0);
  AssertEquals('Count values', 4, D.ChildCount);
  CheckClass('Value 0',TCSSUnicodeRangeElement,D.Children[0]);
  CheckClass('Value 1',TCSSUnicodeRangeElement,D.Children[1]);
  CheckClass('Value 2',TCSSUnicodeRangeElement,D.Children[2]);
  CheckClass('Value 3',TCSSUnicodeRangeElement,D.Children[3]);
end;

procedure TTestCSSParser.TestOneDeclarationNoColon;

Var
  R : TCSSRuleElement;

begin
  R:=ParseRule('@a b { 0% { d: e; } }');
end;

procedure TTestCSSParser.TestTwoDeclarationNoColon;
Var
  R : TCSSRuleElement;

begin
  R:=ParseRule('@a b { 0% { d: e; } 100% { f : g; }  }');
end;

procedure TTestCSSParser.TestOneEmptyDeclaration;
var
  R : TCSSRuleElement;
begin
  R:=ParseRule('{ ; }');
  AssertEquals('selector count',0,R.SelectorCount);
  AssertEquals('declaration count',0,R.ChildCount);

end;

procedure TTestCSSParser.TestImportAtKeyWord;
var
  R : TCSSAtRuleElement;
//  D : TCSSDeclarationElement;
begin
  R:=TCSSAtRuleElement(CheckClass('at',TCSSAtRuleElement,ParseRule('@import url("abc.css");')));
  AssertEquals('selector count',1,R.SelectorCount);
  AssertEquals('declaration count',0,R.ChildCount);

end;

procedure TTestCSSParser.TestMediaPrint;
begin
  ParseRule('@media print { *, *:before {} }');
end;

procedure TTestCSSParser.TestSupportsFunction;
begin
  ParseRule('@supports ((position: -webkit-sticky) or (position: sticky)) {'+ sLineBreak+
'  .sticky-top { '+ sLineBreak+
'    position: -webkit-sticky; '+ sLineBreak+
'    position: sticky; '+ sLineBreak+
'    top: 0; '+ sLineBreak+
'    z-index: 1020; '+ sLineBreak+
'  } '+ sLineBreak+
'} '
);
end;


{ TTestBaseCSSParser }

function TTestBaseCSSParser.GetRule: TCSSRuleElement;
var
  L : TCSSCompoundElement;
begin
  L:=TCSSCompoundElement(CheckClass('list',TCSSCompoundElement,ParseResult));
  AssertTrue('Result has at least 1 child',L.ChildCount>0);
  if L.Children[0] is TCSSAtRuleElement then
    Result:=TCSSAtRuleElement(CheckClass('First element is rule',TCSSAtRuleElement,L.Children[0]))
  else
    Result:=TCSSRuleElement(CheckClass('First element is rule',TCSSRuleElement,L.Children[0]));
end;

procedure TTestBaseCSSParser.SetUp;
begin
  inherited SetUp;
  FParser:=Nil;
  FSource:=Nil;
end;

procedure TTestBaseCSSParser.clear;

begin
  if FParseResult<>FToFree then
    FreeAndNil(FToFree);
  FreeAndNil(FParseResult);
  FreeAndNil(FParser);
  FReeAndNil(FSource);
end;

procedure TTestBaseCSSParser.TearDown;
begin
  Clear;
  inherited TearDown;
end;

procedure TTestBaseCSSParser.CreateParser(const ASource: string);
begin
  Clear;
  FSource:=TStringStream.Create(ASource);
  FParser:=TCSSParser.Create(FSource);
end;

procedure TTestBaseCSSParser.Parse;
begin
  FParseResult:=FParser.Parse;
  FToFree:=FParseResult;
end;

procedure TTestBaseCSSParser.Parse(const aSource: String);
begin
  CreateParser(aSource);
  Parse;
end;

function TTestBaseCSSParser.ParseRule(const aSource: String): TCSSRuleElement;
begin
  Parse(aSource);
  if ParseResult is TCSSRuleElement then
    Result:=ParseResult as TCSSRuleElement
  else
    Result:=FirstRule;
end;

procedure TTestBaseCSSParser.AssertEquals(AMessage : String; AExpected, AActual: TCSSUnits);

Var
  S,EN1,EN2 : String;

begin
  If (AActual<>AExpected) then
    begin
    EN1:=GetEnumName(TypeINfo(TCSSUnits),Ord(AExpected));
    EN2:=GetEnumName(TypeINfo(TCSSUnits),Ord(AActual));
    S:=Format('%s : %s <> %s',[AMessage,EN1,EN2]);
    Fail(S);
    end;
end;

procedure TTestBaseCSSParser.AssertEquals(AMessage: String; AExpected, AActual: TCSSBinaryOperation);
Var
  S,EN1,EN2 : String;

begin
  If (AActual<>AExpected) then
    begin
    EN1:=GetEnumName(TypeINfo(TCSSBinaryOperation),Ord(AExpected));
    EN2:=GetEnumName(TypeINfo(TCSSBinaryOperation),Ord(AActual));
    S:=Format('%s : %s <> %s',[AMessage,EN1,EN2]);
    Fail(S);
    end;
end;


function TTestBaseCSSParser.CheckClass(const aMsg: String; aExpectedClass: TCSSElementClass; aActual: TCSSElement): TCSSElement;
begin
  AssertNotNull(aMsg+': Not null element',aExpectedClass);
  AssertNotNull(aMsg+': Not null class',aActual);
  AssertEquals(aMsg,aExpectedClass,aActual.ClassType);
  Result:=aActual;
end;

function TTestBaseCSSParser.CheckDeclaration(aRule: TCSSRuleElement; aIndex: Integer): TCSSDeclarationElement;
begin
  AssertTrue('Have rule child '+IntToStr(aIndex),aIndex<aRule.ChildCount);
  Result:=TCSSDeclarationElement(CheckClass('Decl', TCSSDeclarationElement,aRule.Children[aIndex]));
end;

function TTestBaseCSSParser.CheckDeclaration(aRule: TCSSRuleElement; aIndex: Integer; const AKey: String): TCSSDeclarationElement;

var
  ID : TCSSIdentifierElement;

begin
  Result:=CheckDeclaration(aRule,aIndex);
  AssertEquals('Key count', 1, Result.KeyCount);
  ID:=TCSSIdentifierElement(CheckClass('key 0', TCSSIdentifierElement,Result.Keys[0]));
  AssertEquals('Key 0  name',aKey,id.Value);
end;

function TTestBaseCSSParser.CheckSelector(aRule: TCSSRuleElement; aIndex: Integer): TCSSElement;
begin
  AssertTrue('Have rule selector '+IntToStr(aIndex),aIndex<aRule.SelectorCount);
  Result:=aRule.Selectors[aIndex];
  AssertNotNull('Have selector non-nil',Result);
end;

function TTestBaseCSSParser.CheckSelector(aRule: TCSSRuleElement; aIndex: Integer; const aName: String): TCSSElement;
begin
  Result:=CheckSelector(aRule,aIndex);
  if Result is TCSSIdentifierElement then
    AssertEquals('Selector '+IntToStr(aIndex)+'name',aName,TCSSIdentifierElement(Result).Name)
  else if Result is TCSSStringElement then
    AssertEquals('Selector '+IntToStr(aIndex)+'name',aName,TCSSStringElement(Result).Value)
  else
    Fail('Selector '+IntToStr(aIndex)+' has no known type')
end;

function TTestBaseCSSParser.CheckList(aList: TCSSListElement; aIndex: Integer): TCSSElement;
begin
  AssertTrue('Have list index '+IntToStr(aIndex),aIndex<aList.ChildCount);
  Result:=aList[aIndex];
  AssertNotNull('Have element non-nil',Result);
end;

function TTestBaseCSSParser.CheckList(aList: TCSSListElement; aIndex: Integer; const aName: String): TCSSElement;
begin
  Result:=CheckList(aList,aIndex);
  if Result is TCSSIdentifierElement then
    AssertEquals('List element '+IntToStr(aIndex)+'name',aName,TCSSIdentifierElement(Result).Name)
  else if Result is TCSSStringElement then
    AssertEquals('List element '+IntToStr(aIndex)+'name',aName,TCSSStringElement(Result).Value)
  else
    Fail('List element '+IntToStr(aIndex)+' has no known type')
end;

function TTestBaseCSSParser.CheckLiteral(Msg: String; aEl: TCSSelement; aValue: String): TCSSStringElement;

begin
  Result:=TCSSStringElement(CheckClass(Msg+': class', TCSSStringElement,aEl));
  AssertEquals(Msg+': String Value',aValue,Result.Value);
end;

function TTestBaseCSSParser.CheckLiteral(Msg: String; aEl: TCSSelement; aValue: Integer): TCSSIntegerElement;
begin
  Result:=TCSSIntegerElement(CheckClass(Msg+': Class', TCSSIntegerElement,aEl));
  AssertEquals(Msg+': Value ',aValue,Result.Value);

end;

function TTestBaseCSSParser.CheckLiteral(Msg: String; aEl: TCSSelement; aValue: Integer; AUnits: TCSSUnits): TCSSIntegerElement;
begin
  Result:=CheckLiteral(Msg,aEl,aValue);
  AssertEquals('Units',aUnits,Result.Units);
end;

function TTestBaseCSSParser.GetCalArg(aCall: TCSSCallElement; aIndex: Integer): TCSSElement;
begin
  AssertNotNull('Have call element',aCall);
  AssertTrue('Have argument '+IntToStr(aIndex),aIndex<aCall.ChildCount);
  Result:=aCall.Children[0];
  AssertNotNull('Have call argument',Result);

end;
 
initialization
  RegisterTests([TTestCSSParser,TTestCSSFilesParser]);
end.

