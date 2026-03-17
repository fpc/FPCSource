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
    procedure ParseRules_FirstValidRule(aSource, aName: string);
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

    // skip invalid rules
    procedure TestSkipRule_AtEOF;
    procedure TestSkipRule_AtCurlyEOF;
    procedure TestSkipRule_AtNameCurlyEOF;
    procedure TestSkipRule_AtNameCurlyNameEOF;
    procedure TestSkipRule_AtNameCurlyNameColonEOF;
    procedure TestSkipRule_NameEOF;
    procedure TestSkipRule_NameCurlyEOF;
    procedure TestSkipRule_NameCurlyNameEOF;
    procedure TestSkipRule_NameCurlyNameColonEOF;
    procedure TestSkipRule_NameBracketEOF;
    procedure TestSkipRule_NameBracketsEOF;
    procedure TestSkipRule_DotEOF;
    procedure TestSkipRule_HashEOF;
    procedure TestSkipInline_AttrUnknownChar;
    procedure TestSkipRule_BinaryOpMissingRHS;
    procedure TestSkipRule_AttrSelectorInvalidValue;
    procedure TestSkipRule_AttrSelectorNoIdent;
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

procedure TTestCSSSkipInline.ParseRules_FirstValidRule(aSource, aName: string);
var
  aRule: TCSSRuleElement;
begin
  Parse(aSource);
  aRule:=FirstRule;
  CheckSelector(aRule,0,aName);
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

procedure TTestCSSSkipInline.TestSkipRule_NameEOF;
begin
  Parse('a');
end;

procedure TTestCSSSkipInline.TestSkipRule_NameCurlyEOF;
begin
  Parse('a{');
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

procedure TTestCSSSkipInline.TestSkipInline_AttrUnknownChar;
begin
  ParseInline_FirstValidDecl('a: ?; color: red;','a');
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

initialization
  RegisterTests([TTestCSSSkipInline]);
end.

