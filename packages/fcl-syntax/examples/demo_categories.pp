program test_categories;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.pascal;

procedure TestCategorySystem;
var
  highlighter: TSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  categories: TStringList;
  i: Integer;
  pascalCategoryID: Integer;
begin
  WriteLn('Testing Category System');
  WriteLn('=======================');
  WriteLn;

  // Test category registration
  pascalCategoryID := TSyntaxHighlighter.RegisterCategory('Pascal');
  WriteLn('Pascal category registered with ID: ', pascalCategoryID);

  // Test duplicate registration
  if TSyntaxHighlighter.RegisterCategory('Pascal') = pascalCategoryID then
    WriteLn('PASS - Duplicate registration returns same ID')
  else
    WriteLn('FAIL - Duplicate registration issue');

  // Test category retrieval
  if TSyntaxHighlighter.GetRegisteredCategoryID('Pascal') = pascalCategoryID then
    WriteLn('PASS - Category ID retrieval works')
  else
    WriteLn('FAIL - Category ID retrieval failed');

  // Test non-existent category
  if TSyntaxHighlighter.GetRegisteredCategoryID('NonExistent') = -1 then
    WriteLn('PASS - Non-existent category returns -1')
  else
    WriteLn('FAIL - Non-existent category handling');

  WriteLn;

  // Test category listing
  categories := TStringList.Create;
  try
    TSyntaxHighlighter.GetRegisteredCategories(categories);
    WriteLn('Registered categories:');
    for i := 0 to categories.Count - 1 do
      WriteLn('  ', categories[i], ' = ', PtrInt(categories.Objects[i]));
  finally
    categories.Free;
  end;

  WriteLn;

  // Test token categorization
  highlighter := TSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute('begin end');

    WriteLn('Token categorization test:');
    for i := 0 to High(tokens) do begin
      if tokens[i].HasCategory(pascalCategoryID) then
        WriteLn('  Token "', tokens[i].Text, '" has correct Pascal category ID')
      else
        WriteLn('  Token "', tokens[i].Text, '" has wrong category ID: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;
end;

begin
  TestCategorySystem;
  WriteLn;
  WriteLn('Category test completed.');
end.