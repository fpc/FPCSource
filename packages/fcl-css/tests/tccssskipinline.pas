unit tcCSSSkipInline;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, fpcunit, testregistry, tcCSSParser, fpCSSTree;

type

  { TTestCSSSkipInline }

  TTestCSSSkipInline = class(TTestBaseCSSParser)
  protected
    procedure ParseInline_FirstValidDecl(aSource, aKey: string);
  published
    // ToDo: invalid keyword in attribute value is skipped
    // ToDo: invalid keyword in attribute value is skipped
    // test skip invalid value  color: 3 red;
    // test skip invalid attribute  color: 3;
    procedure TestSkipInline_AttrMissingColon;
    procedure TestSkipInline_AttrCommaMissingKey;
    procedure TestSkipInline_AttrMissingCloseParenthesis;
  end;


implementation

{ TTestCSSSkipInline }

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
  SkipInvalid:=true;
  ParseInline('a: bla( ; color: red;');
end;

initialization
  RegisterTests([TTestCSSSkipInline]);
end.

