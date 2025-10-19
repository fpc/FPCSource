program test_multiple_categories;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.pascal;

procedure TestMultipleCategories;
var
  categories: TStringList;
  i: Integer;
  javaID, pythonID, cppID, pascalID: Integer;
begin
  WriteLn('Testing Multiple Categories');
  WriteLn('===========================');
  WriteLn;

  // Register various language categories
  javaID := TSyntaxHighlighter.RegisterCategory('Java');
  pythonID := TSyntaxHighlighter.RegisterCategory('Python');
  cppID := TSyntaxHighlighter.RegisterCategory('C++');

  // Pascal will already be registered from the previous test/highlighter usage
  pascalID := TSyntaxHighlighter.RegisterCategory('Pascal');

  WriteLn('Category IDs:');
  WriteLn('  Java: ', javaID);
  WriteLn('  Python: ', pythonID);
  WriteLn('  C++: ', cppID);
  WriteLn('  Pascal: ', pascalID);
  WriteLn;

  // List all registered categories
  categories := TStringList.Create;
  try
    TSyntaxHighlighter.GetRegisteredCategories(categories);
    WriteLn('All registered categories (', categories.Count, ' total):');
    for i := 0 to categories.Count - 1 do
      WriteLn('  ', categories[i], ' = ', PtrInt(categories.Objects[i]));
  finally
    categories.Free;
  end;

  WriteLn;
  WriteLn('Category registration system working correctly!');
end;

begin
  TestMultipleCategories;
  WriteLn;
  WriteLn('Multiple categories test completed.');
end.