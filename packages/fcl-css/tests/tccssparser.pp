{
    This file is part of the Free Pascal Run time library.
    Copyright (c) 2022 by Michael Van Canneyt (michael@freepascal.org)

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
  Classes, SysUtils, fpcunit, testregistry, fpcssparser, fpcsstree,
  fpCSSScanner;

type

  { TTestBaseCSSParser }

  TTestBaseCSSParser = class(TTestCase)
  Private
    FParseResult: TCSSElement;
    FSkipInvalid: boolean;
    FSource : TStringStream;
    FParser : TCSSParser;
    FToFree: TCSSElement;
    procedure Clear;
    function GetFirstRule: TCSSRuleElement;
    function GetFirstInlineDeclaration: TCSSDeclarationElement;
    function OnScannerWarn(Sender: TObject; Msg: string; aRow, aCol: integer): boolean;
  protected
    procedure SetUp; override;
    procedure TearDown; override;
    Procedure CreateParser(Const ASource : string);
    procedure Parse;
    procedure ParseInline;
    procedure Parse(Const aSource : String);
    procedure ParseInline(Const aSource : String);
    function ParseRule(Const aSource : String) : TCSSRuleElement;
    procedure AssertEquals(AMessage: String; AExpected, AActual: TCSSUnit);   overload;
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
    function CheckLiteral(Msg: String; aEl: TCSSelement; aValue: Integer; AUnits : TCSSUnit) : TCSSIntegerElement;  overload;
    Function GetCalArg(aCall : TCSSCallElement; aIndex : Integer) : TCSSElement;
    function GetSecondRule: TCSSRuleElement;
  Public
    Property ParseResult : TCSSElement read FParseResult;
    Property FirstRule : TCSSRuleElement Read GetFirstRule;
    Property FirstInlineDeclaration : TCSSDeclarationElement Read GetFirstInlineDeclaration;
    Property ToFree : TCSSElement Read FToFree Write FToFree;
    Property SkipInvalid: boolean read FSkipInvalid write FSkipInvalid;
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
    procedure TestDescendantPrefixedEmptyRule;
    procedure TestDescendantMixedPrefixedEmptyRule;
    procedure TestAttributePrefixedEmptyRule;
    procedure TestAttributeSquaredEqualRule;
    procedure TestAttributePipeEqualRule;
    procedure TestAttributeStarEqualRule;
    procedure TestAttributeDollarEqualRule;
    procedure TestAttributeTildeEqualRule;
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
    Procedure TestOneDeclarationFloatValue;
    Procedure TestOneDeclarationMultiValue;
    Procedure TestOneDeclarationMultiListValue;
    Procedure TestOneDeclarationExprValue;
    Procedure TestOneDeclarationUnicodeRangeValue;
    Procedure TestOneDeclarationNoColon;
    Procedure TestTwoDeclarationNoColon;
    Procedure TestOneEmptyDeclaration;
    Procedure TestImportAtKeyWord;
    Procedure TestMediaBoolean;
    Procedure TestMediaNotBoolean;
    Procedure TestMediaCommaBoolean;
    Procedure TestMediaCommaNotBoolean;
    Procedure TestMediaPlain;
    Procedure TestMediaNotPlain;
    Procedure TestMediaNotIdentifier;
    Procedure TestMediaOnlyIdentifier;
    Procedure TestMediaRangeNameValue;
    Procedure TestMediaRangeValueName;
    Procedure TestMediaRangeValueLtNameLtValue;
    Procedure TestMediaRangeValueGtNameGtValue;
    Procedure TestMediaPlainAndPlain;
    Procedure TestMediaPlainAndPlainBrackets;
    Procedure TestMediaPlainOrPlain;
    Procedure TestMediaPlainOrPlainBrackets;
    Procedure TestMediaPlainCommaPlain;
    Procedure TestMediaRatio;
    Procedure TestMediaNestedBracket;
    Procedure TestSupportsFunction;
    Procedure TestSkipUnknownFunction;
    Procedure TestNestedRule;
    Procedure TestNestedAndSpaceRule;
    Procedure TestNestedAndNoSpaceRule;
    Procedure TestNestedPlusRule;
    Procedure TestNestedAndPlusRule;
    Procedure TestNestedRule_AppendedAndOperator;
    Procedure TestNestedRule_NestedDeclarations;
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
  With TMemIniFile.Create(ChangeFileExt(Paramstr(0),TCSSString('.ini'))) do
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
  SkipInvalid:=true;
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
  AssertEquals('Sel name','a',Sel.Value);
end;

procedure TTestCSSParser.TestHashPrefixedEmptyRule;
var
  R : TCSSRuleElement;
  sel: TCSSHashIdentifierElement;

begin
  ParseRule('#a { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSHashIdentifierElement(CheckClass('Selector', TCSSHashIdentifierElement,R.Selectors[0]));
  AssertEquals('Sel name','a',Sel.Value);
end;

procedure TTestCSSParser.TestDescendantPrefixedEmptyRule;

var
  R : TCSSRuleElement;
  sel: TCSSIdentifierElement;
  Bin: TCSSBinaryElement;

begin
  ParseRule('a b { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  Bin:=TCSSBinaryElement(CheckClass('Selector', TCSSBinaryElement,R.Selectors[0]));
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,Bin.Left));
  AssertEquals('Sel 1 name','a',Sel.Value);
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,Bin.Right));
  AssertEquals('Sel 2 name','b',Sel.Value);
end;

procedure TTestCSSParser.TestDescendantMixedPrefixedEmptyRule;

var
  R : TCSSRuleElement;
  sel: TCSSIdentifierElement;
  Bin: TCSSBinaryElement;

begin
  ParseRule('a .b { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  Bin:=TCSSBinaryElement(CheckClass('Selector', TCSSBinaryElement,R.Selectors[0]));
  sel:=TCSSIdentifierElement(CheckClass('Selector', TCSSIdentifierElement,Bin.Left));
  AssertEquals('Sel 1 name','a',Sel.Value);
  sel:=TCSSClassNameElement(CheckClass('Selector', TCSSClassNameElement,Bin.Right));
  AssertEquals('Sel 2 name','b',Sel.Value);
end;

procedure TTestCSSParser.TestAttributePrefixedEmptyRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  id : TCSSIdentifierElement;
  bin : TCSSBinaryElement;
  List: TCSSListElement;

begin
  ParseRule('a[b="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  List:=TCSSListElement(CheckClass('Selector', TCSSListElement,R.Selectors[0]));
  AssertEquals('list selector count',2,List.ChildCount);
  Id:=TCSSIdentifierElement(CheckClass('prefix',TCSSIdentifierElement,List[0]));
  sel:=TCSSArrayElement(CheckClass('Attribute Selector', TCSSArrayElement,List[1]));
  AssertEquals('Prefix name','a',Id.Value);
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boEquals,Bin.Operation);
end;

procedure TTestCSSParser.TestAttributeSquaredEqualRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  bin : TCSSBinaryElement;
  Left: TCSSIdentifierElement;

begin
  ParseRule('[b^="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSArrayElement(CheckClass('Selector', TCSSArrayElement,R.Selectors[0]));
  if Sel.Prefix<>nil then
    Fail('no prefix');
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boSquaredEqual,Bin.Operation);
  Left:=TCSSIdentifierElement(CheckClass('Bin.Left',TCSSIdentifierElement,Bin.Left));
  AssertEquals('left=b','b',Left.Value);
  CheckClass('Bin.Right',TCSSStringElement,Bin.Right);
end;

procedure TTestCSSParser.TestAttributePipeEqualRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  bin : TCSSBinaryElement;
  Left: TCSSIdentifierElement;

begin
  ParseRule('[b|="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSArrayElement(CheckClass('Selector', TCSSArrayElement,R.Selectors[0]));
  if Sel.Prefix<>nil then
    Fail('no prefix');
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boPipeEqual,Bin.Operation);
  Left:=TCSSIdentifierElement(CheckClass('Bin.Left',TCSSIdentifierElement,Bin.Left));
  AssertEquals('left=b','b',Left.Value);
  CheckClass('Bin.Right',TCSSStringElement,Bin.Right);
end;

procedure TTestCSSParser.TestAttributeStarEqualRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  bin : TCSSBinaryElement;
  Left: TCSSIdentifierElement;

begin
  ParseRule('[b*="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSArrayElement(CheckClass('Selector', TCSSArrayElement,R.Selectors[0]));
  if Sel.Prefix<>nil then
    Fail('no prefix');
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boStarEqual,Bin.Operation);
  Left:=TCSSIdentifierElement(CheckClass('Bin.Left',TCSSIdentifierElement,Bin.Left));
  AssertEquals('left=b','b',Left.Value);
  CheckClass('Bin.Right',TCSSStringElement,Bin.Right);
end;

procedure TTestCSSParser.TestAttributeDollarEqualRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  bin : TCSSBinaryElement;
  Left: TCSSIdentifierElement;

begin
  ParseRule('[b$="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSArrayElement(CheckClass('Selector', TCSSArrayElement,R.Selectors[0]));
  if Sel.Prefix<>nil then
    Fail('no prefix');
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boDollarEqual,Bin.Operation);
  Left:=TCSSIdentifierElement(CheckClass('Bin.Left',TCSSIdentifierElement,Bin.Left));
  AssertEquals('left=b','b',Left.Value);
  CheckClass('Bin.Right',TCSSStringElement,Bin.Right);
end;

procedure TTestCSSParser.TestAttributeTildeEqualRule;
var
  R : TCSSRuleElement;
  sel: TCSSArrayElement;
  bin : TCSSBinaryElement;
  Left: TCSSIdentifierElement;

begin
  ParseRule('[b~="c"] { }');
  R:=TCSSRuleElement(CheckClass('Rule',TCSSRuleElement,FirstRule));
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  sel:=TCSSArrayElement(CheckClass('Selector', TCSSArrayElement,R.Selectors[0]));
  if Sel.Prefix<>nil then
    Fail('no prefix');
  AssertEquals('Array count',1,Sel.ChildCount);
  Bin:=TCSSBinaryElement(CheckClass('Bin',TCSSBinaryElement,sel.children[0]));
  AssertEquals('Binary op',boTildeEqual,Bin.Operation);
  Left:=TCSSIdentifierElement(CheckClass('Bin.Left',TCSSIdentifierElement,Bin.Left));
  AssertEquals('left=b','b',Left.Value);
  CheckClass('Bin.Right',TCSSStringElement,Bin.Right);
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
  R:=ParseRule('input:enabled:read-write:-webkit-any(:focus,:hover) {  }');
  AssertEquals('No rule children',0,R.ChildCount);
  AssertEquals('selector count',1,R.SelectorCount);
  List:=TCSSListElement(CheckClass('List',TCSSListElement,R.Selectors[0]));
  CheckList(List,0,'input');
  CheckList(List,1,':enabled');
  CheckList(List,2,':read-write');
end;

procedure TTestCSSParser.TestQueryPrefixedEmptyRule;
begin
  ParseRule('@media only screen and (-webkit-min-device-pixel-ratio: 2), only screen and (min-device-pixel-ratio: 3) { }');
end;

procedure TTestCSSParser.TestCommaPrefixedEmptyRule;
begin
  ParseRule('#facebox .tl,#facebox .tl { }');
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
  U : TCSSUnit;

begin
  For U in TCSSUnit do
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
  S:=TCSSStringElement(CheckClass('Value', TCSSHashValueElement,D.Children[0]));
  AssertEquals('Value ','ABABAB',S.Value);
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

procedure TTestCSSParser.TestOneDeclarationFloatValue;
var
  R : TCSSRuleElement;
  D : TCSSDeclarationElement;
  F : TCSSFloatElement;

begin
  R:=ParseRule('{ a : -.5em; }');
  AssertEquals('selector count',0,R.SelectorCount);
  D:=CheckDeclaration(R,0,'a');
  AssertEquals('Value count', 1, D.ChildCount);
  F:=TCSSFloatElement(CheckClass('Value', TCSSFloatElement,D.Children[0]));
  AssertEquals('Value ',-0.5,F.Value);
  if F.Units<>cu_em then
    Fail('Units expected unit em, but found '+IntToStr(ord(F.Units)));
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
  CheckLiteral('Value 1 ',L.Children[0],1,cu_px);
  CheckLiteral('Value 2 ',L.Children[1],2,cu_px);
  CheckLiteral('Value 3 ',L.Children[2],3,cu_px);
  CheckLiteral('Value 4 ',L.Children[3],4,cu_px);
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
  CheckLiteral('Value 1 ',L.Children[0],1,cu_px);
  CheckLiteral('Value 2 ',L.Children[1],2,cu_px);
  L:=TCSSListElement(CheckClass('List',TCSSListElement,D.Children[1]));
  AssertEquals('List element count', 2, L.ChildCount);
  CheckLiteral('Value 3 ',L.Children[0],3,cu_px);
  CheckLiteral('Value 4 ',L.Children[1],4,cu_px);
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
begin
  SkipInvalid:=true;
  ParseRule('@a b { 0% { d: e; } }');
end;

procedure TTestCSSParser.TestTwoDeclarationNoColon;
begin
  SkipInvalid:=true;
  ParseRule('@a b { 0% { d: e; } 100% { f : g; }  }');
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
  Rule: TCSSRuleElement;
  R : TCSSAtRuleElement;
begin
  Rule:=ParseRule('@import url("abc.css");');
  R:=TCSSAtRuleElement(CheckClass('at',TCSSAtRuleElement,Rule));
  AssertEquals('selector count',1,R.SelectorCount);
  AssertEquals('declaration count',0,R.ChildCount);
end;

procedure TTestCSSParser.TestMediaBoolean;
var
  R: TCSSAtRuleElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media print { *, *:before {} }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aSel:=TCSSIdentifierElement(CheckClass('media selector',TCSSIdentifierElement,R.Selectors[0]));
  AssertEquals('media selector name','print',aSel.Value);
end;

procedure TTestCSSParser.TestMediaNotBoolean;
var
  R: TCSSAtRuleElement;
  aList: TCSSListElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media not (print) { }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aList:=TCSSListElement(CheckClass('media selector',TCSSListElement,R.Selectors[0]));
  AssertEquals('selector list count',2,aList.ChildCount);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[0]',TCSSIdentifierElement,aList[0]));
  AssertEquals('selector list[0] value','not',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[1]',TCSSIdentifierElement,aList[1]));
  AssertEquals('selector list[1] value','print',aSel.Value);
  AssertEquals('declaration count',0,R.ChildCount);
  AssertEquals('nested rule count',0,R.NestedRuleCount);
end;

procedure TTestCSSParser.TestMediaCommaBoolean;
var
  R: TCSSAtRuleElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media print, screen { }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',2,R.SelectorCount);
  aSel:=TCSSIdentifierElement(CheckClass('selector 0',TCSSIdentifierElement,R.Selectors[0]));
  AssertEquals('selector 0 value','print',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector 1',TCSSIdentifierElement,R.Selectors[1]));
  AssertEquals('selector 1 value','screen',aSel.Value);
end;

procedure TTestCSSParser.TestMediaCommaNotBoolean;
begin
  ParseRule('@media not print, not screen { }');
end;

procedure TTestCSSParser.TestMediaPlain;
var
  R: TCSSAtRuleElement;
  aBin: TCSSBinaryElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media (any-hover: hover) {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('selector 0',TCSSBinaryElement,R.Selectors[0]));
  AssertEquals('selector operation',boColon,aBin.Operation);
  aSel:=TCSSIdentifierElement(CheckClass('selector left',TCSSIdentifierElement,aBin.Left));
  AssertEquals('selector left value','any-hover',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector right',TCSSIdentifierElement,aBin.Right));
  AssertEquals('selector right value','hover',aSel.Value);
end;

procedure TTestCSSParser.TestMediaNotPlain;
begin
  ParseRule('@media not (any-hover: hover) {  }');
end;

procedure TTestCSSParser.TestMediaNotIdentifier;
begin
  ParseRule('@media not screen {  }');
end;

procedure TTestCSSParser.TestMediaOnlyIdentifier;
begin
  ParseRule('@media only print {  }');
end;

procedure TTestCSSParser.TestMediaRangeNameValue;
var
  R: TCSSAtRuleElement;
  aBin: TCSSBinaryElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media (width > 100px) {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('selector 0',TCSSBinaryElement,R.Selectors[0]));
  AssertEquals('selector operation',boGT,aBin.Operation);
  aSel:=TCSSIdentifierElement(CheckClass('selector left',TCSSIdentifierElement,aBin.Left));
  AssertEquals('selector left value','width',aSel.Value);
  CheckLiteral('selector right',aBin.Right,100,cu_px);
end;

procedure TTestCSSParser.TestMediaRangeValueName;
var
  R: TCSSAtRuleElement;
  aBin: TCSSBinaryElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media (100px <= width) {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('selector 0',TCSSBinaryElement,R.Selectors[0]));
  AssertEquals('selector operation',boLE,aBin.Operation);
  CheckLiteral('selector left',aBin.Left,100,cu_px);
  aSel:=TCSSIdentifierElement(CheckClass('selector right',TCSSIdentifierElement,aBin.Right));
  AssertEquals('selector right value','width',aSel.Value);
end;

procedure TTestCSSParser.TestMediaRangeValueLtNameLtValue;
begin
  ParseRule('@media (100px <= width < 200px) {  }');
end;

procedure TTestCSSParser.TestMediaRangeValueGtNameGtValue;
var
  R: TCSSAtRuleElement;
  aBinOuter, aBinInner: TCSSBinaryElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media (1000px > height >= 200px) {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aBinOuter:=TCSSBinaryElement(CheckClass('selector 0',TCSSBinaryElement,R.Selectors[0]));
  AssertEquals('selector outer operation',boGE,aBinOuter.Operation);
  aBinInner:=TCSSBinaryElement(CheckClass('selector outer left',TCSSBinaryElement,aBinOuter.Left));
  AssertEquals('selector inner operation',boGT,aBinInner.Operation);
  CheckLiteral('selector inner left',aBinInner.Left,1000,cu_px);
  aSel:=TCSSIdentifierElement(CheckClass('selector inner right',TCSSIdentifierElement,aBinInner.Right));
  AssertEquals('selector inner right value','height',aSel.Value);
  CheckLiteral('selector outer right',aBinOuter.Right,200,cu_px);
end;

procedure TTestCSSParser.TestMediaPlainAndPlain;
var
  R: TCSSAtRuleElement;
  aList: TCSSListElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media print and screen {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aList:=TCSSListElement(CheckClass('media selector',TCSSListElement,R.Selectors[0]));
  AssertEquals('selector list count',3,aList.ChildCount);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[0]',TCSSIdentifierElement,aList[0]));
  AssertEquals('selector list[0] value','print',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[1]',TCSSIdentifierElement,aList[1]));
  AssertEquals('selector list[1] value','and',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[2]',TCSSIdentifierElement,aList[2]));
  AssertEquals('selector list[2] value','screen',aSel.Value);
end;

procedure TTestCSSParser.TestMediaPlainAndPlainBrackets;
var
  R: TCSSAtRuleElement;
  aList: TCSSListElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media (print and screen) {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aList:=TCSSListElement(CheckClass('media selector',TCSSListElement,R.Selectors[0]));
  AssertEquals('selector list count',3,aList.ChildCount);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[0]',TCSSIdentifierElement,aList[0]));
  AssertEquals('selector list[0] value','print',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[1]',TCSSIdentifierElement,aList[1]));
  AssertEquals('selector list[1] value','and',aSel.Value);
  aSel:=TCSSIdentifierElement(CheckClass('selector list[2]',TCSSIdentifierElement,aList[2]));
  AssertEquals('selector list[2] value','screen',aSel.Value);
end;

procedure TTestCSSParser.TestMediaPlainOrPlain;
begin
  ParseRule('@media print or screen {  }');
end;

procedure TTestCSSParser.TestMediaPlainOrPlainBrackets;
begin
  ParseRule('@media (print or screen) {  }');
end;

procedure TTestCSSParser.TestMediaPlainCommaPlain;
begin
  ParseRule('@media print, screen {  }');
end;

procedure TTestCSSParser.TestMediaRatio;
var
  R: TCSSAtRuleElement;
  aBin, aRatio: TCSSBinaryElement;
  aSel: TCSSIdentifierElement;
begin
  R:=TCSSAtRuleElement(ParseRule('@media (aspect-ratio > 3/2) {  }'));
  AssertEquals('at keyword','@media',R.AtKeyWord);
  AssertEquals('selector count',1,R.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('selector 0',TCSSBinaryElement,R.Selectors[0]));
  AssertEquals('selector operation',boGT,aBin.Operation);
  aSel:=TCSSIdentifierElement(CheckClass('selector left',TCSSIdentifierElement,aBin.Left));
  AssertEquals('selector left value','aspect-ratio',aSel.Value);
  aRatio:=TCSSBinaryElement(CheckClass('selector right',TCSSBinaryElement,aBin.Right));
  AssertEquals('selector right operation',boDIV,aRatio.Operation);
  CheckLiteral('selector right left',aRatio.Left,3);
  CheckLiteral('selector right right',aRatio.Right,2);
end;

procedure TTestCSSParser.TestMediaNestedBracket;
begin
  ParseRule('@media ((print)) {  }');
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

procedure TTestCSSParser.TestSkipUnknownFunction;
begin
  SkipInvalid:=true;
  ParseRule(':-webkit-any(table, thead, tbody, tfoot, tr) > form:-internal-is-html {'+sLineBreak
    +'  display: none !important;'+sLineBreak
    +'}');
end;

procedure TTestCSSParser.TestNestedRule;
var
  aRule, aNestedRule: TCSSRuleElement;
  aSel: TCSSClassNameElement;
begin
  aRule:=ParseRule('.parent { .child { } }');
  AssertEquals('selector count',1,aRule.SelectorCount);
  aSel:=TCSSClassNameElement(CheckClass('Selector',TCSSClassNameElement,aRule.Selectors[0]));
  AssertEquals('Sel name','parent',aSel.Value);
  AssertEquals('No declarations',0,aRule.ChildCount);
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aSel:=TCSSClassNameElement(CheckClass('Nested selector',TCSSClassNameElement,aNestedRule.Selectors[0]));
  AssertEquals('Nested sel name','child',aSel.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
end;

procedure TTestCSSParser.TestNestedAndSpaceRule;
var
  aRule, aNestedRule: TCSSRuleElement;
  aSel: TCSSClassNameElement;
  aAndSel: TCSSIdentifierElement;
  aBin: TCSSBinaryElement;
begin
  aRule:=ParseRule('.parent { & .child { } }');
  AssertEquals('selector count',1,aRule.SelectorCount);
  aSel:=TCSSClassNameElement(CheckClass('Selector',TCSSClassNameElement,aRule.Selectors[0]));
  AssertEquals('Sel name','parent',aSel.Value);
  AssertEquals('No declarations',0,aRule.ChildCount);
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('Nested selector',TCSSBinaryElement,aNestedRule.Selectors[0]));
  AssertEquals('Nested selector operation',boWhiteSpace,aBin.Operation);
  aAndSel:=TCSSIdentifierElement(CheckClass('Nested selector left',TCSSIdentifierElement,aBin.Left));
  AssertEquals('Nested selector left value','&',aAndSel.Value);
  aSel:=TCSSClassNameElement(CheckClass('Nested selector right',TCSSClassNameElement,aBin.Right));
  AssertEquals('Nested selector right value','child',aSel.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
end;

procedure TTestCSSParser.TestNestedAndNoSpaceRule;
var
  aRule, aNestedRule: TCSSRuleElement;
  aSel: TCSSClassNameElement;
  aAndSel: TCSSIdentifierElement;
  aList: TCSSListElement;
begin
  aRule:=ParseRule('.parent { &.child { } }');
  AssertEquals('selector count',1,aRule.SelectorCount);
  aSel:=TCSSClassNameElement(CheckClass('Selector',TCSSClassNameElement,aRule.Selectors[0]));
  AssertEquals('Sel name','parent',aSel.Value);
  AssertEquals('No declarations',0,aRule.ChildCount);
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aList:=TCSSListElement(CheckClass('Nested selector',TCSSListElement,aNestedRule.Selectors[0]));
  AssertEquals('Nested selector list count',2,aList.ChildCount);
  aAndSel:=TCSSIdentifierElement(CheckClass('Nested selector[0]',TCSSIdentifierElement,aList[0]));
  AssertEquals('Nested selector[0] value','&',aAndSel.Value);
  aSel:=TCSSClassNameElement(CheckClass('Nested selector[1]',TCSSClassNameElement,aList[1]));
  AssertEquals('Nested selector[1] value','child',aSel.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
end;

procedure TTestCSSParser.TestNestedPlusRule;
var
  aRule, aNestedRule: TCSSRuleElement;
  aIdent: TCSSIdentifierElement;
  aUnary: TCSSUnaryElement;
begin
  aRule:=ParseRule('h1 { + p { } }');
  AssertEquals('selector count',1,aRule.SelectorCount);
  aIdent:=TCSSIdentifierElement(CheckClass('Selector',TCSSIdentifierElement,aRule.Selectors[0]));
  AssertEquals('Sel name','h1',aIdent.Value);
  AssertEquals('No declarations',0,aRule.ChildCount);
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aUnary:=TCSSUnaryElement(CheckClass('Nested selector',TCSSUnaryElement,aNestedRule.Selectors[0]));
  if aUnary.Operation<>uoPlus then
    Fail('Nested selector operation expected uoPlus, but found '+GetEnumName(TypeInfo(TCSSUnaryOperation),Ord(aUnary.Operation)));
  aIdent:=TCSSIdentifierElement(CheckClass('Nested selector right',TCSSIdentifierElement,aUnary.Right));
  AssertEquals('Nested selector right value','p',aIdent.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
end;

procedure TTestCSSParser.TestNestedAndPlusRule;
var
  aRule, aNestedRule: TCSSRuleElement;
  aIdent: TCSSIdentifierElement;
  aBin: TCSSBinaryElement;
begin
  aRule:=ParseRule('h1 { & + p { } }');
  AssertEquals('selector count',1,aRule.SelectorCount);
  aIdent:=TCSSIdentifierElement(CheckClass('Selector',TCSSIdentifierElement,aRule.Selectors[0]));
  AssertEquals('Sel name','h1',aIdent.Value);
  AssertEquals('No declarations',0,aRule.ChildCount);
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('Nested selector',TCSSBinaryElement,aNestedRule.Selectors[0]));
  AssertEquals('Nested selector operation',boPlus,aBin.Operation);
  aIdent:=TCSSIdentifierElement(CheckClass('Nested selector left',TCSSIdentifierElement,aBin.Left));
  AssertEquals('Nested selector left value','&',aIdent.Value);
  aIdent:=TCSSIdentifierElement(CheckClass('Nested selector right',TCSSIdentifierElement,aBin.Right));
  AssertEquals('Nested selector right value','p',aIdent.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
end;

procedure TTestCSSParser.TestNestedRule_AppendedAndOperator;
var
  aRule, aNestedRule: TCSSRuleElement;
  aBin: TCSSBinaryElement;
  aClass: TCSSClassNameElement;
  aIdent: TCSSIdentifierElement;
begin
  aRule:=ParseRule(
   '.foo {'+LineEnding
  +'  .bar & {'+LineEnding
  +'  }'+LineEnding
  +'}');
  // outer rule: .foo { }
  AssertEquals('selector count',1,aRule.SelectorCount);
  aClass:=TCSSClassNameElement(CheckClass('Selector',TCSSClassNameElement,aRule.Selectors[0]));
  AssertEquals('Sel name','foo',aClass.Value);
  AssertEquals('No declarations',0,aRule.ChildCount);
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  // nested rule: .bar & { }
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('Nested selector',TCSSBinaryElement,aNestedRule.Selectors[0]));
  AssertEquals('Nested selector operation',boWhiteSpace,aBin.Operation);
  aClass:=TCSSClassNameElement(CheckClass('Nested selector left',TCSSClassNameElement,aBin.Left));
  AssertEquals('Nested selector left value','bar',aClass.Value);
  aIdent:=TCSSIdentifierElement(CheckClass('Nested selector right',TCSSIdentifierElement,aBin.Right));
  AssertEquals('Nested selector right value','&',aIdent.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
end;

procedure TTestCSSParser.TestNestedRule_NestedDeclarations;
var
  aRule, aNestedRule: TCSSRuleElement;
  aIdent: TCSSIdentifierElement;
  aBin: TCSSBinaryElement;
  aDecl: TCSSDeclarationElement;
begin
  aRule:=ParseRule(
   'div {'+LineEnding
  +'  & span {'+LineEnding
  +'  }'+LineEnding
  +'  color: blue;'+LineEnding
  +'}');
  AssertEquals('selector count',1,aRule.SelectorCount);
  aIdent:=TCSSIdentifierElement(CheckClass('Selector',TCSSIdentifierElement,aRule.Selectors[0]));
  AssertEquals('Sel name','div',aIdent.Value);
  AssertEquals('Declaration count',0,aRule.ChildCount);
  AssertEquals('Nested rule count',2,aRule.NestedRuleCount);
  // Check nested rule: & span { }
  aNestedRule:=aRule.NestedRules[0];
  AssertEquals('Nested selector count',1,aNestedRule.SelectorCount);
  aBin:=TCSSBinaryElement(CheckClass('Nested selector',TCSSBinaryElement,aNestedRule.Selectors[0]));
  AssertEquals('Nested selector operation',boWhiteSpace,aBin.Operation);
  aIdent:=TCSSIdentifierElement(CheckClass('Nested selector left',TCSSIdentifierElement,aBin.Left));
  AssertEquals('Nested selector left value','&',aIdent.Value);
  aIdent:=TCSSIdentifierElement(CheckClass('Nested selector right',TCSSIdentifierElement,aBin.Right));
  AssertEquals('Nested selector right value','span',aIdent.Value);
  AssertEquals('No nested declarations',0,aNestedRule.ChildCount);
  AssertEquals('No nested rules',0,aNestedRule.NestedRuleCount);
  // Check nested declaration rule
  aNestedRule:=aRule.NestedRules[1];
  AssertEquals('Nested Declaration selector count',0,aNestedRule.SelectorCount);
  // declaration: color: blue
  aDecl:=CheckDeclaration(aNestedRule,0,'color');
  AssertEquals('Declaration value count',1,aDecl.ChildCount);
  aIdent:=TCSSIdentifierElement(CheckClass('Declaration value',TCSSIdentifierElement,aDecl.Children[0]));
  AssertEquals('Declaration value','blue',aIdent.Value);
end;



{ TTestBaseCSSParser }

function TTestBaseCSSParser.GetFirstRule: TCSSRuleElement;
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

function TTestBaseCSSParser.GetFirstInlineDeclaration: TCSSDeclarationElement;
var
  Rule: TCSSRuleElement;
begin
  Rule:=TCSSRuleElement(CheckClass('rule',TCSSRuleElement,ParseResult));
  if Rule.ChildCount=0 then
    Fail('No valid child found');
  Result:=TCSSDeclarationElement(CheckClass('declaration',TCSSDeclarationElement,Rule.Children[0]));
end;

function TTestBaseCSSParser.OnScannerWarn(Sender: TObject; Msg: string; aRow, aCol: integer
  ): boolean;
var
  aScanner: TCSSScanner;
begin
  Result:=true;
  aScanner:=FParser.Scanner;
  writeln('TTestBaseCSSParser.OnScannerWarn ',aScanner.CurFilename+'('+IntToStr(aRow)+','+IntToStr(aCol)+') ',Msg);
end;

procedure TTestBaseCSSParser.SetUp;
begin
  inherited SetUp;
  FParser:=Nil;
  FSource:=Nil;
end;

procedure TTestBaseCSSParser.Clear;

begin
  if FParseResult<>FToFree then
    FreeAndNil(FToFree);
  FreeAndNil(FParseResult);
  FreeAndNil(FParser);
  FreeAndNil(FSource);
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
  if SkipInvalid then
    FParser.Scanner.OnWarn:=@OnScannerWarn;
end;

procedure TTestBaseCSSParser.Parse;
begin
  FParseResult:=FParser.Parse;
  FToFree:=FParseResult;
end;

procedure TTestBaseCSSParser.ParseInline;
begin
  FParseResult:=FParser.ParseInline;
  FToFree:=FParseResult;
end;

procedure TTestBaseCSSParser.Parse(const aSource: String);
begin
  CreateParser(aSource);
  Parse;
end;

procedure TTestBaseCSSParser.ParseInline(const aSource: String);
begin
  CreateParser(aSource);
  ParseInline;
end;

function TTestBaseCSSParser.ParseRule(const aSource: String): TCSSRuleElement;
begin
  Parse(aSource);
  if ParseResult is TCSSRuleElement then
    Result:=ParseResult as TCSSRuleElement
  else
    Result:=FirstRule;
end;

procedure TTestBaseCSSParser.AssertEquals(AMessage : String; AExpected, AActual: TCSSUnit);

Var
  S,EN1,EN2 : String;

begin
  If (AActual<>AExpected) then
    begin
    EN1:=GetEnumName(TypeINfo(TCSSUnit),Ord(AExpected));
    EN2:=GetEnumName(TypeINfo(TCSSUnit),Ord(AActual));
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

function TTestBaseCSSParser.CheckLiteral(Msg: String; aEl: TCSSelement; aValue: Integer; AUnits: TCSSUnit): TCSSIntegerElement;
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

function TTestBaseCSSParser.GetSecondRule: TCSSRuleElement;
var
  L : TCSSCompoundElement;
begin
  L:=TCSSCompoundElement(CheckClass('list',TCSSCompoundElement,ParseResult));
  AssertTrue('Result has at least 2 children',L.ChildCount>1);
  if L.Children[1] is TCSSAtRuleElement then
    Result:=TCSSAtRuleElement(CheckClass('Second element is rule',TCSSAtRuleElement,L.Children[1]))
  else
    Result:=TCSSRuleElement(CheckClass('Second element is rule',TCSSRuleElement,L.Children[1]));
end;

initialization
  RegisterTests([TTestCSSParser,TTestCSSFilesParser]);
end.

