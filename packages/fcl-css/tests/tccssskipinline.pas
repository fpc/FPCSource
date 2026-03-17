unit tcCSSSkipInline;

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
  published
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
  SkipInvalid:=true;
  ParseInline(aSource);
  Decl:=FirstInlineDeclaration;
  AssertEquals('Key count', 1, Decl.KeyCount);
  ID:=TCSSIdentifierElement(CheckClass('key 0', TCSSIdentifierElement,Decl.Keys[0]));
  AssertEquals('Key 0  name',aKey,ID.Value);
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

initialization
  RegisterTests([TTestCSSSkipInline]);
end.

