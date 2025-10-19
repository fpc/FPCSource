program test_html;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, syntax.highlighter, syntax.html;

procedure TestHtmlBasicTags;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML Basic Tags:');
  WriteLn('=======================');

  htmlCode := '<html><head><title>Test</title></head><body><p>Hello</p></body></html>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestHtmlAttributes;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML Attributes:');
  WriteLn('=======================');

  htmlCode := '<div class="container" id="main" style="color: red;">Content</div>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestHtmlComments;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML Comments:');
  WriteLn('=====================');

  htmlCode := '<!-- This is a comment --><p>Text</p>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestHtmlEntities;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML Entities:');
  WriteLn('=====================');

  htmlCode := '<p>&lt;div&gt; &amp; &quot;test&quot; &#123; &#x41;</p>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestHtmlEmbeddedCss;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML Embedded CSS:');
  WriteLn('==========================');

  htmlCode := '<style>body { color: red; font-size: 16px; }</style>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestHtmlEmbeddedJavaScript;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML Embedded JavaScript:');
  WriteLn('=================================');

  htmlCode := '<script>function test() { console.log("hello"); }</script>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestHtmlDoctype;
var
  highlighter: THtmlSyntaxHighlighter;
  tokens: TSyntaxTokenArray;
  i: Integer;
  htmlCode: string;
begin
  WriteLn('Testing HTML DOCTYPE:');
  WriteLn('====================');

  htmlCode := '<!DOCTYPE html><html><head></head></html>';

  highlighter := THtmlSyntaxHighlighter.Create;
  try
    tokens := highlighter.Execute(htmlCode);

    WriteLn('HTML: ', htmlCode);
    WriteLn('Tokens (', Length(tokens), '):');
    for i := 0 to High(tokens) do begin
      if Trim(tokens[i].Text) <> '' then
        WriteLn('  "', tokens[i].Text, '" - Kind: ', Ord(tokens[i].Kind),
                ' (', tokens[i].Kind, ') - Category: ', tokens[i].CategoriesAsString);
    end;

  finally
    highlighter.Free;
  end;

  WriteLn;
end;

procedure TestCategorySystem;
var
  categories: TStringList;
  htmlCategoryID: Integer;
  i: Integer;
begin
  WriteLn('Testing Category System for HTML:');
  WriteLn('=================================');

  htmlCategoryID := TSyntaxHighlighter.RegisterCategory('HTML');
  WriteLn('HTML category ID: ', htmlCategoryID);

  categories := TStringList.Create;
  try
    TSyntaxHighlighter.GetRegisteredCategories(categories);
    WriteLn('All registered categories:');
    for i := 0 to categories.Count - 1 do
      WriteLn('  ', categories[i], ' = ', PtrInt(categories.Objects[i]));
  finally
    categories.Free;
  end;

  WriteLn;
end;

begin
  WriteLn('HTML Syntax Highlighter Test');
  WriteLn('============================');
  WriteLn;

  TestCategorySystem;
  TestHtmlBasicTags;
  TestHtmlAttributes;
  TestHtmlComments;
  TestHtmlEntities;
  TestHtmlEmbeddedCss;
  TestHtmlEmbeddedJavaScript;
  TestHtmlDoctype;

  WriteLn('Test completed. Press Enter to exit.');
  ReadLn;
end.