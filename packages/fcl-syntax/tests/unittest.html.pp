{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    HTML highlighter unit test

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit unittest.html;

interface

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fpcunit, testregistry,
  syntax.highlighter, syntax.html;

type
  TTestHtmlHighlighter = class(TTestCase)
  protected
    procedure SetUp; override;
    procedure TearDown; override;
  private
    function DoHtmlHighlighting(const source: string): TSyntaxTokenArray;
  published
    procedure TestHtmlBasicTags;
    procedure TestHtmlAttributes;
    procedure TestHtmlComments;
    procedure TestHtmlEntities;
    procedure TestHtmlEmbeddedCSS;
    procedure TestHtmlEmbeddedJavaScript;
    procedure TestHtmlDoctype;
    procedure TestHtmlSelfClosingTags;
    procedure TestHtmlNestedTags;
    procedure TestComplexHtmlDocument;
    procedure TestHtmlCDATA;
    procedure TestCategorySystem;
  end;

implementation

procedure TTestHtmlHighlighter.SetUp;
begin

end;

procedure TTestHtmlHighlighter.TearDown;
begin
  // Nothing to do
end;

function TTestHtmlHighlighter.DoHtmlHighlighting(const source: string): TSyntaxTokenArray;
var
  highlighter: THtmlSyntaxHighlighter;
begin
  highlighter := THtmlSyntaxHighlighter.Create;
  try
    Result := highlighter.Execute(source);
  finally
    highlighter.Free;
  end;
end;

procedure TTestHtmlHighlighter.TestHtmlBasicTags;
var
  tokens: TSyntaxTokenArray;
begin
  // Test simple div tag
  tokens := DoHtmlHighlighting('<div>');
  AssertTrue('Should have at least 3 tokens', Length(tokens) >= 3);
  AssertEquals('First token should be opening bracket', '<', tokens[0].Text);
  AssertEquals('Opening bracket should be symbol', Ord(shSymbol), Ord(tokens[0].Kind));
  AssertEquals('Tag name should be div', 'div', tokens[1].Text);
  AssertEquals('Tag name should be keyword', Ord(shKeyword), Ord(tokens[1].Kind));
  AssertEquals('Closing bracket should be >', '>', tokens[2].Text);
  AssertEquals('Closing bracket should be symbol', Ord(shSymbol), Ord(tokens[2].Kind));

  // Test closing tag
  tokens := DoHtmlHighlighting('</div>');
  AssertTrue('Should have at least 3 tokens', Length(tokens) >= 3);
  AssertEquals('First token should be opening bracket with slash', '</', tokens[0].Text);
  AssertEquals('Tag name should be div', 'div', tokens[1].Text);
  AssertEquals('Closing bracket should be >', '>', tokens[2].Text);

  // Test self-closing tag
  tokens := DoHtmlHighlighting('<br/>');
  AssertTrue('Should have at least 4 tokens', Length(tokens) >= 4);
  AssertEquals('First token should be <', '<', tokens[0].Text);
  AssertEquals('Tag name should be br', 'br', tokens[1].Text);
  AssertEquals('Slash should be symbol', Ord(shSymbol), Ord(tokens[2].Kind));
  AssertEquals('Closing bracket should be >', '>', tokens[3].Text);
end;

procedure TTestHtmlHighlighter.TestHtmlAttributes;
var
  tokens: TSyntaxTokenArray;
  i: Integer;
  hasAttribute, hasValue: Boolean;
begin
  tokens := DoHtmlHighlighting('<div class="container">');
  AssertTrue('Should have multiple tokens', Length(tokens) > 5);

  hasAttribute := False;
  hasValue := False;
  for i := 0 to High(tokens) do
    begin
    if tokens[i].Text = 'class' then
      hasAttribute := True;
    if tokens[i].Text = '"container"' then
      hasValue := True;
    end;

  AssertTrue('Should contain class attribute', hasAttribute);
  AssertTrue('Should contain attribute value', hasValue);
end;

procedure TTestHtmlHighlighter.TestHtmlComments;
var
  tokens: TSyntaxTokenArray;
  foundComment: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<!-- This is a comment -->');
  foundComment := False;

  for i := 0 to High(tokens) do
    if (tokens[i].Kind = shComment) or (tokens[i].Kind = shSymbol) then
      foundComment := True;

  AssertTrue('Should contain comment tokens', foundComment);
  AssertTrue('Should have multiple tokens', Length(tokens) >= 1);
end;

procedure TTestHtmlHighlighter.TestHtmlEntities;
var
  tokens: TSyntaxTokenArray;
  foundEntity: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('&amp;');
  foundEntity := False;

  for i := 0 to High(tokens) do
    begin
    if (tokens[i].Text = '&amp;') and (tokens[i].Kind = shEscape) then
      foundEntity := True;
    end;

  AssertTrue('Should recognize HTML entity', foundEntity);

  // Test numeric entity
  tokens := DoHtmlHighlighting('&#123;');
  foundEntity := False;

  for i := 0 to High(tokens) do
    begin
    if (tokens[i].Text = '&#123;') and (tokens[i].Kind = shEscape) then
      foundEntity := True;
    end;

  AssertTrue('Should recognize numeric HTML entity', foundEntity);
end;

procedure TTestHtmlHighlighter.TestHtmlEmbeddedCSS;
var
  tokens: TSyntaxTokenArray;
  lToken : TSyntaxToken;
  hasStyleTag, hasCSS: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<style>body { color: red; }</style>');
  hasStyleTag := False;
  hasCSS := False;

  for i := 0 to High(tokens) do
  begin
    lToken:=tokens[i];
    if (lToken.Text = 'style') and (lToken.Kind = shKeyword) then
      hasStyleTag := True;
    if (lToken.CategoryCount> 0) and (lToken.Text = 'body') then
      hasCSS := True;
  end;

  AssertTrue('Should contain style tag', hasStyleTag);
  AssertTrue('Should have multiple tokens', Length(tokens) > 5);
  AssertTrue('Should have CSS', hasCSS);
  // Note: CSS parsing depends on embedded highlighter
end;

procedure TTestHtmlHighlighter.TestHtmlEmbeddedJavaScript;
var
  tokens: TSyntaxTokenArray;
  hasScriptTag: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<script>var x = 5;</script>');
  hasScriptTag := False;

  for i := 0 to High(tokens) do
    if (tokens[i].Text = 'script') and (tokens[i].Kind = shKeyword) then
      hasScriptTag := True;

  AssertTrue('Should contain script tag', hasScriptTag);
  AssertTrue('Should have multiple tokens', Length(tokens) > 5);
  // Note: JavaScript parsing depends on embedded highlighter
end;

procedure TTestHtmlHighlighter.TestHtmlDoctype;
var
  tokens: TSyntaxTokenArray;
  foundDoctype: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<!DOCTYPE html>');
  foundDoctype := False;

  for i := 0 to High(tokens) do
    if (tokens[i].Kind = shDirective) and (Pos('DOCTYPE', tokens[i].Text) > 0) then
      foundDoctype := True;

  AssertTrue('Should recognize DOCTYPE as directive', foundDoctype);
end;

procedure TTestHtmlHighlighter.TestHtmlSelfClosingTags;
var
  tokens: TSyntaxTokenArray;
  hasImg, hasSlash: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<img src="test.jpg" />');
  hasImg := False;
  hasSlash := False;

  for i := 0 to High(tokens) do
    begin
    if (tokens[i].Text = 'img') and (tokens[i].Kind = shKeyword) then
      hasImg := True;
    if (tokens[i].Text = '/') and (tokens[i].Kind = shSymbol) then
      hasSlash := True;
    end;

  AssertTrue('Should contain img tag', hasImg);
  AssertTrue('Should contain closing slash', hasSlash);
end;

procedure TTestHtmlHighlighter.TestHtmlNestedTags;
var
  tokens: TSyntaxTokenArray;
  tagCount: Integer;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<div><p>Hello</p></div>');
  tagCount := 0;

  for i := 0 to High(tokens) do
    begin
    if tokens[i].Kind = shKeyword then
      Inc(tagCount);
    end;

  AssertTrue('Should contain multiple tags', tagCount >= 4); // div, p, p, div
  AssertTrue('Should have many tokens', Length(tokens) > 10);
end;

procedure TTestHtmlHighlighter.TestComplexHtmlDocument;
var
  tokens: TSyntaxTokenArray;
  document: string;
  hasHtml, hasHead, hasBody, hasTitle: Boolean;
  i: Integer;
begin
  document := '<html><head><title>Test</title></head><body><h1>Hello</h1></body></html>';
  tokens := DoHtmlHighlighting(document);

  hasHtml := False;
  hasHead := False;
  hasBody := False;
  hasTitle := False;

  for i := 0 to High(tokens) do
    begin
    if (tokens[i].Text = 'html') and (tokens[i].Kind = shKeyword) then
      hasHtml := True;
    if (tokens[i].Text = 'head') and (tokens[i].Kind = shKeyword) then
      hasHead := True;
    if (tokens[i].Text = 'body') and (tokens[i].Kind = shKeyword) then
      hasBody := True;
    if (tokens[i].Text = 'title') and (tokens[i].Kind = shKeyword) then
      hasTitle := True;
    end;

  AssertTrue('Should contain html tag', hasHtml);
  AssertTrue('Should contain head tag', hasHead);
  AssertTrue('Should contain body tag', hasBody);
  AssertTrue('Should contain title tag', hasTitle);
  AssertTrue('Should have many tokens for complex document', Length(tokens) > 20);
end;

procedure TTestHtmlHighlighter.TestHtmlCDATA;
var
  tokens: TSyntaxTokenArray;
  foundCDATA: Boolean;
  i: Integer;
begin
  tokens := DoHtmlHighlighting('<![CDATA[Some data here]]>');
  foundCDATA := False;

  for i := 0 to High(tokens) do
    if (tokens[i].Kind = shRawString) or
       ((tokens[i].Kind = shSymbol) and (tokens[i].Text = '<![CDATA[')) then
      foundCDATA := True;

  AssertTrue('Should recognize CDATA section', foundCDATA);
end;

procedure TTestHtmlHighlighter.TestCategorySystem;
var
  tokens: TSyntaxTokenArray;
  htmlCategoryFound, cssCategoryFound, jsCategoryFound: Boolean;
  lCat,i: Integer;
begin
  // Test basic HTML category
  tokens := DoHtmlHighlighting('<div>text</div>');
  htmlCategoryFound := False;

  for i := 0 to High(tokens) do
    if tokens[i].HasCategory(THtmlSyntaxHighlighter.CategoryHTML) then
      htmlCategoryFound := True;

  AssertTrue('Should have HTML category tokens', htmlCategoryFound);

  // Test embedded CSS category
  tokens := DoHtmlHighlighting('<style>body { color: red; }</style>');
  cssCategoryFound := False;
  lCat:=TSyntaxHighLighter.GetRegisteredCategoryID('EmbeddedCSS');
  for i := 0 to High(tokens) do
    if tokens[i].HasCategory(lCat) then
      cssCategoryFound := True;

  AssertTrue('Should have category tokens for CSS', cssCategoryFound);

  // Test embedded JavaScript category
  tokens := DoHtmlHighlighting('<script>var x = 5;</script>');
  jsCategoryFound := False;
  lCat:=TSyntaxHighlighter.GetRegisteredCategoryID('EmbeddedJS');
  for i := 0 to High(tokens) do
    if tokens[i].HasCategory(lCat) then
      jsCategoryFound := True;

  AssertTrue('Should have category tokens for JavaScript', jsCategoryFound);
end;

initialization
  RegisterTest(TTestHtmlHighlighter);
end.
