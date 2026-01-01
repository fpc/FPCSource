{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Annotated Token stream to HTML renderer

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

unit syntax.htmlrender;

interface

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.Classes, System.SysUtils, syntax.highlighter;
{$ELSE}
  Classes, SysUtils, syntax.highlighter;
{$ENDIF}

type
  THTMLRenderOption = (hroNoDefaultSpan, hroPreserveLineStructure);
  THTMLRenderOptions = set of THTMLRenderOption;

  { THtmlSyntaxRenderer }

  THtmlSyntaxRenderer = class
  private
    FOptions: THTMLRenderOptions;
    FExtraClasses: string;
  protected
    function GetCategoryName(category: Integer): string;
    function EscapeHtml(const text: string): string; virtual;
    function GetKindClassName(kind: TSyntaxHighlightKind): string; virtual;
    function BuildClassNames(kind: TSyntaxHighlightKind; categories : Array of Integer): string; virtual;
    procedure RenderTokensWithLineBreaks(const tokens: TSyntaxTokenArray; output: TStrings); virtual;
    procedure RenderTokensWithoutLineBreaks(const tokens: TSyntaxTokenArray; output: TStrings); virtual;
  public
    constructor Create;
    property Options: THTMLRenderOptions read FOptions write FOptions;
    property ExtraClasses: string read FExtraClasses write FExtraClasses;
    procedure RenderTokens(const tokens: TSyntaxTokenArray; output: TStrings);
    procedure RenderTokensToString(const tokens: TSyntaxTokenArray; var output: string);
  end;

implementation

{ THtmlSyntaxRenderer }

constructor THtmlSyntaxRenderer.Create;
begin
  inherited Create;
  FOptions := [];
  FExtraClasses := '';
end;

function THtmlSyntaxRenderer.EscapeHtml(const text: string): string;
var
  i: Integer;
  ch: Char;
begin
  Result := '';
  for i := 1 to Length(text) do
    begin
    ch := text[i];
    case ch of
      '<': Result := Result + '&lt;';
      '>': Result := Result + '&gt;';
      '&': Result := Result + '&amp;';
      '"': Result := Result + '&quot;';
      '''': Result := Result + '&#39;';
      else
        Result := Result + ch;
    end;
    end;
end;

function THtmlSyntaxRenderer.GetKindClassName(kind: TSyntaxHighlightKind): string;
begin
  Result:=LowerCase(Kind.ToString);
end;

function THtmlSyntaxRenderer.GetCategoryName(category: Integer): string;
begin
  Result := '';
  if category = 0 then Exit;
  Result:=LowerCase(TSyntaxHighlighter.GetCategoryName(category));
end;

function THtmlSyntaxRenderer.BuildClassNames(kind: TSyntaxHighlightKind; categories: array of Integer): string;
var
  kindName, lName, categoryNames: string;
  category : integer;

begin
  kindName := GetKindClassName(kind);
  categoryNames:='';
  for category in categories do
    begin
    lName:=GetCategoryName(category);
    if lName<>'' then
      categoryNames := categoryNames + ' ' + kindName + '-' + lName;
    end;

  Result := kindName;
  if categoryNames<>'' then
    Result:=Result+categoryNames;

  // Add extra classes if specified
  if FExtraClasses <> '' then
    Result := Result + ' ' + FExtraClasses;
end;

procedure THtmlSyntaxRenderer.RenderTokensWithLineBreaks(const tokens: TSyntaxTokenArray; output: TStrings);
var
  i: Integer;
  token: TSyntaxToken;
  escapedText, classNames, result, currentLine: string;
  lines: TStringArray;
begin
  // Build complete HTML first
  result := '';
  for i := 0 to High(tokens) do
    begin
    token := tokens[i];
    escapedText := EscapeHtml(token.Text);

    // Skip span wrapping for default tokens if hroNoDefaultSpan is set
    if (hroNoDefaultSpan in FOptions) and (token.Kind = shDefault) then
      result := result + escapedText
    else
      begin
      classNames := BuildClassNames(token.Kind, token.Categories);

      if classNames <> '' then
        result := result + '<span class="' + classNames + '">' + escapedText + '</span>'
      else
        result := result + escapedText;
      end;
    end;

  // Split by newlines and add each line separately
  if result <> '' then
    begin
    lines := result.Split([#10]);
    for i := 0 to High(lines) do
      begin
      currentLine := lines[i];
      // Remove trailing #13 if present (for #13#10 line endings)
      if (Length(currentLine) > 0) and (currentLine[Length(currentLine)] = #13) then
        Delete(currentLine, Length(currentLine), 1);
      output.Add(currentLine);
      end;
    end;
end;

procedure THtmlSyntaxRenderer.RenderTokensWithoutLineBreaks(const tokens: TSyntaxTokenArray; output: TStrings);
var
  i: Integer;
  token: TSyntaxToken;
  escapedText, classNames, result : string;

begin
  result := '';
  for i := 0 to High(tokens) do
    begin
    token := tokens[i];
    escapedText := EscapeHtml(token.Text);

    // Skip span wrapping for default tokens if hroNoDefaultSpan is set
    if (hroNoDefaultSpan in FOptions) and (token.Kind = shDefault) then
      result := result + escapedText
    else
      begin
      classNames := BuildClassNames(token.Kind, token.Categories);
      if classNames <> '' then
        result := result + '<span class="' + classNames + '">' + escapedText + '</span>'
      else
        result := result + escapedText;
      end;
    end;

  // Add the complete result as a single line
  if result <> '' then
    output.Add(result);
end;


procedure THtmlSyntaxRenderer.RenderTokens(const tokens: TSyntaxTokenArray; output: TStrings);
begin
  if (output=nil) then
    Exit;
  if hroPreserveLineStructure in FOptions then
    RenderTokensWithLineBreaks(Tokens,Output)
  else
    RenderTokensWithOutLineBreaks(Tokens,Output);
end;

procedure THtmlSyntaxRenderer.RenderTokensToString(const tokens: TSyntaxTokenArray; var output: string);
var
  i: Integer;
  token: TSyntaxToken;
  escapedText, classNames: string;
  SkipSpan : Boolean;
begin
  SkipSpan:=(hroNoDefaultSpan in FOptions);
  output := '';
  for i := 0 to High(tokens) do
    begin
    token := tokens[i];
    escapedText := EscapeHtml(token.Text);
    // Skip span wrapping for default tokens if hroNoDefaultSpan is set
    if not ((token.Kind = shDefault) and SkipSpan) then
      begin
      classNames := BuildClassNames(token.Kind, token.Categories);
      if classNames <> '' then
        EscapedText := '<span class="' + classNames + '">' + escapedText + '</span>'
      end;
    output := output + escapedText;
    end;
end;

end.
