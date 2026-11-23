{
  Tests for the categories of TSyntaxToken.
}
unit unittest.tokencategories;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Types, fpcunit, testregistry,
  syntax.highlighter, syntax.pascal;

type

  { TTestTokenCategories }

  TTestTokenCategories = class(TTestCase)
  private
    FToken: TSyntaxToken;
    procedure AddOneCategoryTooMany;
    procedure CreateWithOneCategoryTooMany;
    procedure AddTooLargeID;
    procedure AddTooSmallID;
    procedure CreateWithTooLargeID;
    procedure CreateArrayWithTooLargeID;
    procedure HighlightWithTooLargeDefault;
  published
    procedure TestRecordSize;
    procedure TestNoCategories;
    procedure TestOneCategory;
    procedure TestCategoryArray;
    procedure TestAddUpToMaximum;
    procedure TestAddBeyondMaximum;
    procedure TestCreateBeyondMaximum;
    procedure TestIDRange;
    procedure TestIDOutOfRange;
    procedure TestCopiesAreIndependent;
    procedure TestCategoriesReturnsCopy;
    procedure TestHighlighterDefaultCategory;
    procedure TestHighlighterWithoutDefaultCategory;
    procedure TestHighlighterDefaultCategoryOutOfRange;
  end;

implementation

procedure TTestTokenCategories.AddOneCategoryTooMany;

var
  I: Integer;
begin
  FToken := TSyntaxToken.Create('x', shDefault);
  for I := 1 to MaxTokenCategories + 1 do
    FToken.AddCategory(I);
end;


procedure TTestTokenCategories.CreateWithOneCategoryTooMany;

begin
  FToken := TSyntaxToken.Create('x', shDefault, [1, 2, 3, 4, 5, 6, 7, 8]);
end;


procedure TTestTokenCategories.AddTooLargeID;

begin
  FToken := TSyntaxToken.Create('x', shDefault);
  FToken.AddCategory(MaxCategoryID + 1);
end;


procedure TTestTokenCategories.AddTooSmallID;

begin
  FToken := TSyntaxToken.Create('x', shDefault);
  FToken.AddCategory(Low(SmallInt) - 1);
end;


procedure TTestTokenCategories.CreateWithTooLargeID;

begin
  FToken := TSyntaxToken.Create('x', shDefault, MaxCategoryID + 1);
end;


procedure TTestTokenCategories.CreateArrayWithTooLargeID;

begin
  FToken := TSyntaxToken.Create('x', shDefault, [1, MaxCategoryID + 1]);
end;


procedure TTestTokenCategories.HighlightWithTooLargeDefault;

var
  lHighlighter: TPascalSyntaxHighlighter;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.DefaultCategory := MaxCategoryID + 1;
    lHighlighter.Execute('x');
  finally
    lHighlighter.Free;
  end;
end;


procedure TTestTokenCategories.TestRecordSize;

begin
{$IFDEF CPU64}
  AssertEquals('token record size on a 64-bit target', 32, SizeOf(TSyntaxToken));
{$ENDIF}
  AssertEquals('category slots', 7, MaxTokenCategories);
end;


procedure TTestTokenCategories.TestNoCategories;

var
  lToken: TSyntaxToken;
begin
  lToken := TSyntaxToken.Create('x', shKeyword);
  AssertEquals('category count', 0, lToken.CategoryCount);
  AssertEquals('categories array length', 0, Length(lToken.Categories));
  AssertFalse('has no category 1', lToken.HasCategory(1));
  AssertEquals('text', 'x', lToken.Text);
  AssertEquals('kind', shKeyword.ToString, lToken.Kind.ToString);
  AssertEquals('line', 0, lToken.Line);
end;


procedure TTestTokenCategories.TestOneCategory;

var
  lToken: TSyntaxToken;
begin
  lToken := TSyntaxToken.Create('x', shDefault, 7);
  AssertEquals('category count', 1, lToken.CategoryCount);
  AssertTrue('has category 7', lToken.HasCategory(7));
  AssertFalse('has no category 8', lToken.HasCategory(8));
  AssertEquals('categories array length', 1, Length(lToken.Categories));
  AssertEquals('first category', 7, lToken.Categories[0]);
end;


procedure TTestTokenCategories.TestCategoryArray;

var
  lToken: TSyntaxToken;
  lCategories: TIntegerDynArray;
begin
  lToken := TSyntaxToken.Create('x', shDefault, [3, 5, 9]);
  AssertEquals('category count', 3, lToken.CategoryCount);
  lCategories := lToken.Categories;
  AssertEquals('categories array length', 3, Length(lCategories));
  AssertEquals('first category', 3, lCategories[0]);
  AssertEquals('second category', 5, lCategories[1]);
  AssertEquals('third category', 9, lCategories[2]);
  lToken := TSyntaxToken.Create('x', shDefault, []);
  AssertEquals('empty category array', 0, lToken.CategoryCount);
end;


procedure TTestTokenCategories.TestAddUpToMaximum;

var
  lToken: TSyntaxToken;
  I: Integer;
begin
  lToken := TSyntaxToken.Create('x', shDefault, [1, 2]);
  for I := 3 to MaxTokenCategories do
    lToken.AddCategory(I);
  AssertEquals('category count', MaxTokenCategories, lToken.CategoryCount);
  for I := 1 to MaxTokenCategories do
    AssertTrue('has category ' + IntToStr(I), lToken.HasCategory(I));
  AssertEquals('last category', MaxTokenCategories, lToken.Categories[MaxTokenCategories - 1]);
end;


procedure TTestTokenCategories.TestAddBeyondMaximum;

begin
  AssertException('adding one category more than the maximum', ESyntaxHighlighter, @AddOneCategoryTooMany);
end;


procedure TTestTokenCategories.TestCreateBeyondMaximum;

begin
  AssertException('creating a token with one category more than the maximum', ESyntaxHighlighter, @CreateWithOneCategoryTooMany);
end;


procedure TTestTokenCategories.TestIDRange;

var
  lToken: TSyntaxToken;
begin
  lToken := TSyntaxToken.Create('x', shDefault, MaxCategoryID);
  lToken.AddCategory(-1);
  lToken.AddCategory(Low(SmallInt));
  AssertEquals('category count', 3, lToken.CategoryCount);
  AssertTrue('has the highest ID', lToken.HasCategory(MaxCategoryID));
  AssertTrue('has -1', lToken.HasCategory(-1));
  AssertTrue('has the lowest ID', lToken.HasCategory(Low(SmallInt)));
  AssertEquals('highest ID in the categories array', MaxCategoryID, lToken.Categories[0]);
end;


procedure TTestTokenCategories.TestIDOutOfRange;

begin
  AssertException('adding an ID above the maximum', ESyntaxHighlighter, @AddTooLargeID);
  AssertException('adding an ID below the minimum', ESyntaxHighlighter, @AddTooSmallID);
  AssertException('creating with an ID above the maximum', ESyntaxHighlighter, @CreateWithTooLargeID);
  AssertException('creating with an array containing an ID above the maximum', ESyntaxHighlighter, @CreateArrayWithTooLargeID);
end;


procedure TTestTokenCategories.TestCopiesAreIndependent;

var
  lFirst, lSecond: TSyntaxToken;
begin
  lFirst := TSyntaxToken.Create('x', shDefault, [1, 2, 3]);
  lSecond := lFirst;
  lFirst.AddCategory(10);
  lSecond.AddCategory(20);
  AssertEquals('first copy count', 4, lFirst.CategoryCount);
  AssertEquals('second copy count', 4, lSecond.CategoryCount);
  AssertTrue('first copy has its own category', lFirst.HasCategory(10));
  AssertFalse('first copy lacks the category of the second', lFirst.HasCategory(20));
  AssertTrue('second copy has its own category', lSecond.HasCategory(20));
  AssertFalse('second copy lacks the category of the first', lSecond.HasCategory(10));
end;


procedure TTestTokenCategories.TestCategoriesReturnsCopy;

var
  lToken: TSyntaxToken;
  lCategories: TIntegerDynArray;
begin
  lToken := TSyntaxToken.Create('x', shDefault, [4, 6]);
  lCategories := lToken.Categories;
  lCategories[0] := 99;
  AssertTrue('token keeps its category', lToken.HasCategory(4));
  AssertFalse('token does not see the change', lToken.HasCategory(99));
end;

procedure TTestTokenCategories.TestHighlighterDefaultCategory;

var
  lTokens: TSyntaxTokenArray;
begin
  lTokens := DoPascalHighlighting('x');
  AssertEquals('one token', 1, Length(lTokens));
  AssertEquals('one category', 1, lTokens[0].CategoryCount);
  AssertTrue('the Pascal category', lTokens[0].HasCategory(TPascalSyntaxHighlighter.CategoryPascal));
  AssertEquals('line', 1, lTokens[0].Line);
end;


procedure TTestTokenCategories.TestHighlighterWithoutDefaultCategory;

var
  lHighlighter: TPascalSyntaxHighlighter;
  lTokens: TSyntaxTokenArray;
begin
  lHighlighter := TPascalSyntaxHighlighter.Create;
  try
    lHighlighter.DefaultCategory := 0;
    lTokens := lHighlighter.Execute('x');
  finally
    lHighlighter.Free;
  end;
  AssertEquals('one token', 1, Length(lTokens));
  AssertEquals('no categories', 0, lTokens[0].CategoryCount);
end;


procedure TTestTokenCategories.TestHighlighterDefaultCategoryOutOfRange;

begin
  AssertException('default category above the maximum', ESyntaxHighlighter, @HighlightWithTooLargeDefault);
end;

initialization
  RegisterTest(TTestTokenCategories);
end.
