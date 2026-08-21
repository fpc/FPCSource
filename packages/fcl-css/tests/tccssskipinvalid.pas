unit tcCSSSkipInvalid;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcCSSParser, fpCSSTree;

type

  { TTestCSSSkipInline }

  TTestCSSSkipInline = class(TTestBaseCSSParser)
  protected
    procedure SetUp; override;
    procedure ParseInline_FirstValidDecl(aSource, aKey: string);
    procedure ParseRules_FirstRule(aSource, aName: string);
    procedure ParseRules_SecondRule(aSource, aName: string);
    function ParseRules_FirstAtRule(aSource, aKeyword: string): TCSSAtRuleElement;
  published
    // skip invalid inline
    procedure TestSkipInline_AttrMissingColon;
    procedure TestSkipInline_AttrCommaMissingKey;
    procedure TestSkipInline_AttrMissingCloseParenthesis;
    procedure TestSkipInline_AttrMissingCloseParenthesis2;
    procedure TestSkipInline_AttrMissingCloseParenthesis3;
    procedure TestSkipInline_AttrMissingCloseParenthesis4;
    procedure TestSkipInline_AttrMissingCloseBracket;
    procedure TestSkipInline_AttrMissingCloseBracket2;
    procedure TestSkipInline_AttrMissingCloseBracket3;
    procedure TestSkipInline_AttrMissingStringEnd;
    procedure TestSkipInline_AttrMissingFuncArg;
    procedure TestSkipInline_AttrMissingFuncArg2;
    procedure TestSkipInline_InvalidFloatUnit;
    procedure TestSkipInline_InvalidFloatValue;
    procedure TestSkipInline_InvalidFloatValue2;
    procedure TestSkipInline_AttrUnknownChar;

    // skip invalid rules
    procedure TestSkipRule_AtEOF;
    procedure TestSkipRule_AtCurlyEOF;
    procedure TestSkipRule_AtNameCurlyEOF;
    procedure TestSkipRule_AtNameCurlyNameEOF;
    procedure TestSkipRule_AtNameCurlyNameColonEOF;
    procedure TestSkipRule_AtMediaEOF;
    procedure TestSkipRule_AtMediaParenthesisEOF;
    procedure TestSkipRule_AtMediaBinaryEOF;
    procedure TestSkipRule_AtMediaCurlyEOF;
    procedure TestSkipRule_AtMediaCurlyRuleCurlyEOF;
    procedure TestSkipRule_AtMediaInvalidConditionBlock;
    procedure TestSkipRule_AtMediaInvalidConditionNestedBlock;
    procedure TestSkipRule_AtFontFaceCurlyEOF;
    procedure TestSkipRule_AtFontFaceCurlyNameColonEOF;
    procedure TestSkipRule_AtKeyframesEOF;
    procedure TestSkipRule_AtKeyframesNameEOF;
    procedure TestSkipRule_AtKeyframesCurlyEOF;
    procedure TestSkipRule_AtKeyframesNoName;
    procedure TestSkipRule_AtKeyframesSelectorCurlyEOF;
    procedure TestSkipRule_AtKeyframesInvalidSelector;
    procedure TestSkipRule_AtKeyframesMissingUnit;
    procedure TestSkipRule_NameEOF;
    procedure TestSkipRule_NameCurlyEOF;
    procedure TestSkipRule_NameCurlyNestedCurlyEOF;
    procedure TestSkipRule_NameCurlyDeclMissingColon;
    procedure TestSkipRule_NameCurlyNameEOF;
    procedure TestSkipRule_NameCurlyNameColonEOF;
    procedure TestSkipRule_NameBracketEOF;
    procedure TestSkipRule_NameBracketsEOF;
    procedure TestSkipRule_DotEOF;
    procedure TestSkipRule_HashEOF;
    procedure TestSkipRule_BinaryOpMissingRHS;
    procedure TestSkipRule_AttrSelectorInvalidValue;
    procedure TestSkipRule_AttrSelectorNoIdent;
    procedure TestSkipRule_InvalidDotRule;
    procedure TestSkipRule_InvalidHashRule;
    procedure TestSkipRule_InvalidPercentageRule;
  end;


implementation

{ TTestCSSSkipInline }

procedure TTestCSSSkipInline.SetUp;
begin
  inherited SetUp;
  SkipInvalid:=true;
end;

procedure TTestCSSSkipInline.ParseInline_FirstValidDecl(aSource, aKey: string);
var
  Decl: TCSSDeclarationElement;
  ID: TCSSIdentifierElement;
begin
  ParseInline(aSource);
  Decl:=FirstInlineDeclaration;
  AssertEquals('Key count', 1, Decl.KeyCount);
  ID:=TCSSIdentifierElement(CheckClass('key 0', TCSSIdentifierElement,Decl.Keys[0]));
  AssertEquals('Key 0  name',aKey,ID.Value);
end;

procedure TTestCSSSkipInline.ParseRules_FirstRule(aSource, aName: string);
var
  aRule: TCSSRuleElement;
begin
  Parse(aSource);
  aRule:=FirstRule;
  CheckSelector(aRule,0,aName);
end;

procedure TTestCSSSkipInline.ParseRules_SecondRule(aSource, aName: string);
var
  aRule: TCSSRuleElement;
begin
  Parse(aSource);
  aRule:=GetSecondRule;
  CheckSelector(aRule,0,aName);
end;

function TTestCSSSkipInline.ParseRules_FirstAtRule(aSource, aKeyword: string
  ): TCSSAtRuleElement;
begin
  Parse(aSource);
  Result:=TCSSAtRuleElement(CheckClass('at rule',TCSSAtRuleElement,FirstRule));
  AssertEquals('at keyword',aKeyword,Result.AtKeyWord);
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingColon;
begin
  ParseInline_FirstValidDecl('a; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrCommaMissingKey;
begin
  ParseInline_FirstValidDecl('a,; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseParenthesis;
begin
  ParseInline_FirstValidDecl('a: bla( ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseParenthesis2;
begin
  ParseInline_FirstValidDecl('a: b(c ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseParenthesis3;
begin
  ParseInline_FirstValidDecl('a: ( ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseParenthesis4;
begin
  ParseInline_FirstValidDecl('a: (b ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseBracket;
begin
  ParseInline_FirstValidDecl('a: [ ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseBracket2;
begin
  ParseInline_FirstValidDecl('a: b[ ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingCloseBracket3;
begin
  ParseInline_FirstValidDecl('a: b[c ; color: red;','color');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingStringEnd;
begin
  ParseInline('a: " ; color: red;');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingFuncArg;
begin
  ParseInline_FirstValidDecl('a: b() ; color: red;','a');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrMissingFuncArg2;
begin
  ParseInline_FirstValidDecl('a: b(,) ; color: red;','a');
end;

procedure TTestCSSSkipInline.TestSkipInline_InvalidFloatUnit;
begin
  ParseInline_FirstValidDecl('a: 1foo ; color: red;','a');
end;

procedure TTestCSSSkipInline.TestSkipInline_InvalidFloatValue;
begin
  ParseInline_FirstValidDecl('a: 1E9999999 ; color: red;','a');
end;

procedure TTestCSSSkipInline.TestSkipInline_InvalidFloatValue2;
begin
  ParseInline_FirstValidDecl('a: 1234567890123456789; color: red;','a');
end;

procedure TTestCSSSkipInline.TestSkipInline_AttrUnknownChar;
begin
  ParseInline_FirstValidDecl('a: ?; color: red;','a');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtEOF;
begin
  Parse('@');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtCurlyEOF;
begin
  Parse('@{');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtNameCurlyEOF;
begin
  Parse('@a{');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtNameCurlyNameEOF;
begin
  Parse('@a{b');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtNameCurlyNameColonEOF;
begin
  Parse('@a{b:');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaEOF;
begin
  // the rule is auto closed at EOF
  ParseRules_FirstAtRule('@media','@media');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaParenthesisEOF;
begin
  // the rule is auto closed at EOF
  ParseRules_FirstAtRule('@media (','@media');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaBinaryEOF;
begin
  ParseRules_FirstAtRule('@media (width>','@media');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaCurlyEOF;
var
  aRule: TCSSAtRuleElement;
begin
  aRule:=ParseRules_FirstAtRule('@media(width>3){','@media');
  AssertEquals('selector count',1,aRule.SelectorCount);
  AssertEquals('nested rule count',0,aRule.NestedRuleCount);
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaCurlyRuleCurlyEOF;
var
  aRule: TCSSAtRuleElement;
begin
  // both the nested rule and the @media rule are auto closed at EOF
  aRule:=ParseRules_FirstAtRule('@media(width>3){div{','@media');
  AssertEquals('selector count',1,aRule.SelectorCount);
  AssertEquals('nested rule count',1,aRule.NestedRuleCount);
  CheckSelector(aRule.NestedRules[0],0,'div');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaInvalidConditionBlock;
begin
  // SkipRule must skip the whole block, not only its first token
  ParseRules_FirstAtRule('@media 5 { .foo{top:1px} }','@media');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtMediaInvalidConditionNestedBlock;
begin
  // SkipRule must count the nested blocks
  ParseRules_FirstAtRule('@media 5 { .foo{ .bar{top:1px} } }','@media');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtFontFaceCurlyEOF;
begin
  ParseRules_FirstAtRule('@font-face{','@font-face');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtFontFaceCurlyNameColonEOF;
begin
  ParseRules_FirstAtRule('@font-face{src:','@font-face');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesEOF;
begin
  ParseRules_FirstAtRule('@keyframes','@keyframes');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesNameEOF;
begin
  ParseRules_FirstAtRule('@keyframes fade','@keyframes');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesCurlyEOF;
var
  R: TCSSAtRuleElement;
begin
  // the unclosed at-rule is auto closed at EOF
  R:=ParseRules_FirstAtRule('@keyframes fade{','@keyframes');
  AssertEquals('keyframe count',0,R.NestedRuleCount);
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesNoName;
var
  R: TCSSAtRuleElement;
begin
  R:=ParseRules_FirstAtRule('@keyframes { 0% {top:1px} }','@keyframes');
  AssertEquals('selector count',0,R.SelectorCount);
  AssertEquals('keyframe count',1,R.NestedRuleCount);
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesSelectorCurlyEOF;
var
  R: TCSSAtRuleElement;
begin
  R:=ParseRules_FirstAtRule('@keyframes fade{0%{','@keyframes');
  AssertEquals('keyframe count',1,R.NestedRuleCount);
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesInvalidSelector;
var
  R: TCSSAtRuleElement;
begin
  // the keyframe with the invalid selector is skipped, the next one is parsed
  R:=ParseRules_FirstAtRule('@keyframes fade{ .foo {top:1px} to {top:2px} }','@keyframes');
  AssertEquals('keyframe count',1,R.NestedRuleCount);
  CheckSelector(R.NestedRules[0],0,'to');
end;

procedure TTestCSSSkipInline.TestSkipRule_AtKeyframesMissingUnit;
var
  R: TCSSAtRuleElement;
begin
  // a keyframe selector must be a percentage
  R:=ParseRules_FirstAtRule('@keyframes fade{ 0 {top:1px} }','@keyframes');
  AssertEquals('keyframe count',1,R.NestedRuleCount);
end;

procedure TTestCSSSkipInline.TestSkipRule_NameEOF;
begin
  Parse('a');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameCurlyEOF;
begin
  // the unclosed rule is auto closed at EOF
  ParseRules_FirstRule('a{','a');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameCurlyNestedCurlyEOF;
var
  aRule, aNestedRule: TCSSRuleElement;
begin
  // the nested rule is closed, the outer rule is auto closed at EOF
  Parse('a{b{}');
  aRule:=FirstRule;
  CheckSelector(aRule,0,'a');
  AssertEquals('Nested rule count',1,aRule.NestedRuleCount);
  aNestedRule:=aRule.NestedRules[0];
  CheckSelector(aNestedRule,0,'b');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameCurlyDeclMissingColon;
var
  aRule: TCSSRuleElement;
begin
  // 'b c' has no '{', so it is an invalid declaration, not a nested rule
  Parse('a{b c; color:red}');
  aRule:=FirstRule;
  CheckSelector(aRule,0,'a');
  AssertEquals('Nested rule count',0,aRule.NestedRuleCount);
  CheckDeclaration(aRule,0,'color');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameCurlyNameEOF;
begin
  Parse('a{a');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameCurlyNameColonEOF;
begin
  Parse('a{a:');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameBracketEOF;
begin
  Parse('a[');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameBracketsEOF;
begin
  Parse('a[]');
end;

procedure TTestCSSSkipInline.TestSkipRule_DotEOF;
begin
  Parse('.');
end;

procedure TTestCSSSkipInline.TestSkipRule_HashEOF;
begin
  Parse('#');
end;

procedure TTestCSSSkipInline.TestSkipRule_BinaryOpMissingRHS;
begin
  Parse('"a"=');
end;

procedure TTestCSSSkipInline.TestSkipRule_AttrSelectorInvalidValue;
begin
  Parse('a[b=;]');
end;

procedure TTestCSSSkipInline.TestSkipRule_AttrSelectorNoIdent;
begin
  Parse('a[1=foo]');
end;

procedure TTestCSSSkipInline.TestSkipRule_InvalidDotRule;
begin
  ParseRules_SecondRule('. {}'+LineEnding+'div{}','div');
end;

procedure TTestCSSSkipInline.TestSkipRule_InvalidHashRule;
begin
  ParseRules_SecondRule('# {}'+LineEnding+'div{}','div');
end;

procedure TTestCSSSkipInline.TestSkipRule_InvalidPercentageRule;
begin
  ParseRules_SecondRule('%invalid {}'+LineEnding+'div{}','div');
end;

initialization
  RegisterTests([TTestCSSSkipInline]);
end.

