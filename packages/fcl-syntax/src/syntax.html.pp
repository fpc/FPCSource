{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    HTML syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.html;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, System.SysUtils, syntax.highlighter, syntax.css, syntax.javascript;
  {$ELSE}
  Types, SysUtils, syntax.highlighter, syntax.css, syntax.javascript;
  {$ENDIF}

type
  THtmlParseState = (
    hsText,           // Outside tags - regular text content
    hsTagOpen,        // Inside opening tag < >
    hsTagClose,       // Inside closing tag </ >
    hsComment,        // Inside <!-- -->
    hsDoctype,        // Inside <!DOCTYPE >
    hsCDATA,          // Inside <![CDATA[ ]]>
    hsScript,         // Inside <script> - JavaScript content
    hsStyle           // Inside <style> - CSS content
  );

  { THtmlSyntaxHighlighter }

  THtmlSyntaxHighlighter = class(TSyntaxHighlighter)
  private
    FSource: string;
    FSourceLen : integer;
    FPos: Integer;
    FState: THtmlParseState;
    FCurrentTag: string;
    FCssHighlighter: TCssSyntaxHighlighter;
    FJsHighlighter: TJavaScriptSyntaxHighlighter;
    procedure EmbeddedToken(Sender: TObject; const aToken: TSyntaxToken);
    function MatchAt(aPos: Integer; const aValue: string): Boolean;
    function FindEndTag(const aTag: string): Integer;
  protected
    // Emit the whitespace at the cursor as one token
    procedure ProcessWhiteSpace;
    procedure ProcessText(var endPos: Integer);
    procedure ProcessTagOpen(var endPos: Integer);
    procedure ProcessTagClose(var endPos: Integer);
    procedure ProcessComment(var endPos: Integer);
    procedure ProcessDoctype(var endPos: Integer);
    procedure ProcessCDATA(var endPos: Integer);
    procedure ProcessScript(var endPos: Integer);
    procedure ProcessStyle(var endPos: Integer);
    procedure ProcessEntity(var endPos: Integer);
    function IsWordChar(ch: Char): Boolean;
    function IsTagChar(ch: Char): Boolean;
    function PeekString(const s: string): Boolean;
    function ExtractTagName(const tagContent: string): string;
    class function GetLanguages: TStringDynArray; override;
    class procedure RegisterDefaultCategories; override;
    procedure CheckCategories;
    procedure DoExecute(const source: string); override;
    property CssHighlighter: TCssSyntaxHighlighter read FCssHighlighter;
    property JsHighlighter: TJavaScriptSyntaxHighlighter read FJsHighlighter;
  public
    class var
     CategoryHTML : Integer;
     CategoryHTMLStyleAttr : Integer;
     CategoryHTMLAttribute : Integer;
     CategoryHTMLComment : integer;
    constructor Create; override;
    destructor Destroy; override;
    procedure reset; override;
  end;

const
  MaxKeywordLength = 15;
  MaxKeyword = 46;

  HtmlTagTable: array[0..MaxKeyword] of String = (
    'a', 'abbr', 'address', 'area', 'article', 'aside', 'audio', 'b', 'base', 'bdi',
    'bdo', 'blockquote', 'body', 'br', 'button', 'canvas', 'caption', 'cite', 'code', 'col',
    'colgroup', 'data', 'datalist', 'dd', 'del', 'details', 'dfn', 'dialog', 'div', 'dl',
    'dt', 'em', 'embed', 'fieldset', 'figure', 'footer', 'form', 'h1', 'h2', 'h3',
    'h4', 'h5', 'h6', 'head', 'header', 'hr', 'html'
  );

function DoHtmlHighlighting(const source: string): TSyntaxTokenArray;

implementation

{ THtmlSyntaxHighlighter }

constructor THtmlSyntaxHighlighter.Create;

begin
  inherited Create;
  CheckCategories;
  DefaultCategory:=CategoryHTML;
  // Create them first so they are available in reset
  FCssHighlighter := TCssSyntaxHighlighter.Create;
  FCssHighlighter.DefaultCategory:=TSyntaxHighlighter.GetRegisteredCategoryID('EmbeddedCSS');
  FJsHighlighter := TJavaScriptSyntaxHighlighter.Create;
  FJsHighlighter.DefaultCategory:=TSyntaxHighlighter.GetRegisteredCategoryID('EmbeddedJS');
end;


destructor THtmlSyntaxHighlighter.Destroy;

begin
  FCssHighlighter.Free;
  FJsHighlighter.Free;
  inherited Destroy;
end;


procedure THtmlSyntaxHighlighter.reset;

begin
  inherited reset;
  FState:=hsText;
end;


procedure THtmlSyntaxHighlighter.ProcessText(var endPos: Integer);

var
  startPos: Integer;

begin
  startPos := FPos;
  while (FPos <= FSourceLen) do
    begin
    if FSource[FPos] = '<' then
      break;
    if FSource[FPos] = '&' then
      begin
      // Save text before entity
      if FPos > startPos then
        begin
        endPos := FPos - 1;
        AddSourceToken(FSource, startPos, endPos - startPos + 1, shDefault);
        FPos := endPos + 1;
        end;
      ProcessEntity(endPos);
      Exit;
      end;
    Inc(FPos);
    end;
  endPos := FPos - 1;
  if endPos >= startPos then
    AddSourceToken(FSource, startPos, endPos - startPos + 1, shDefault);
end;


procedure THtmlSyntaxHighlighter.ProcessWhiteSpace;

var
  lStart: Integer;
begin
  lStart := FPos;
  while (FPos <= FSourceLen) and (FSource[FPos] in [' ', #9, #10, #13]) do
    Inc(FPos);
  if FPos > lStart then
    AddSourceToken(FSource, lStart, FPos - lStart, shDefault);
end;


procedure THtmlSyntaxHighlighter.ProcessTagOpen(var endPos: Integer);

var
  startPos, nameStart, nameEnd, attrStart: Integer;
  tagName, attrName: string;
  quoteChar: Char;

begin
  startPos := FPos;

  // Process opening <
  AddToken('<', shSymbol);
  Inc(FPos);
  ProcessWhiteSpace;

  // Process tag name
  nameStart := FPos;
  while (FPos <= FSourceLen) and IsTagChar(FSource[FPos]) do
    Inc(FPos);

  if FPos > nameStart then
    begin
    nameEnd := FPos - 1;
    tagName := LowerCase(Copy(FSource, nameStart, nameEnd - nameStart + 1));
    FCurrentTag := tagName;
    AddSourceToken(FSource, nameStart, nameEnd - nameStart + 1, shKeyword);
    end;

  // Process attributes
  while (FPos <= FSourceLen) and (FSource[FPos] <> '>') do
    begin
    attrStart := FPos;
    // Skip whitespace
    while (FPos <= FSourceLen) and (FSource[FPos] in [' ', #9, #10, #13]) do
      begin
      startPos := FPos;
      while (FPos <= FSourceLen) and (FSource[FPos] in [' ', #9, #10, #13]) do
        Inc(FPos);
      AddSourceToken(FSource, startPos, FPos - startPos, shDefault);
      end;
    if (FPos <= FSourceLen) and (FSource[FPos] = '>') then
      break;
    // Self-closing tag
    if (FPos <= FSourceLen) and (FSource[FPos] = '/') then
      begin
      AddToken('/', shSymbol);
      Inc(FPos);
      continue;
      end;
    // Process attribute name
    startPos := FPos;
    while (FPos <= FSourceLen) and IsWordChar(FSource[FPos]) do
      Inc(FPos);

    if FPos > startPos then
      begin
      attrName := SourceText(FSource, startPos, FPos - startPos);
      if attrName = 'style' then
        AddToken(attrName, shDefault, CategoryHTMLStyleAttr)
      else
        AddToken(attrName, shDefault, CategoryHTMLAttribute);
      end;

    // Skip whitespace around =
    while (FPos <= FSourceLen) and (FSource[FPos] in [' ', #9]) do
      begin
      AddSourceToken(FSource, FPos, 1, shDefault);
      Inc(FPos);
      end;

    // Process = sign
    if (FPos <= FSourceLen) and (FSource[FPos] = '=') then
      begin
      AddToken('=', shSymbol);
      Inc(FPos);

      // Skip whitespace after =
      while (FPos <= FSourceLen) and (FSource[FPos] in [' ', #9]) do
        begin
        AddSourceToken(FSource, FPos, 1, shDefault);
        Inc(FPos);
        end;

      // Process attribute value
      if (FPos <= FSourceLen) and (FSource[FPos] in ['"', '''']) then
        begin
        quoteChar := FSource[FPos];
        startPos := FPos;
        Inc(FPos);

        while (FPos <= FSourceLen) and (FSource[FPos] <> quoteChar) do
          Inc(FPos);

        if (FPos <= FSourceLen) and (FSource[FPos] = quoteChar) then
          Inc(FPos);

        if (attrName = 'style') and (FPos > startPos + 2) then
          begin
          // Delegate CSS content to CSS highlighter (excluding quotes)
          AddSourceToken(FSource, startPos, 1, shSymbol); // Opening quote
          if FPos > startPos + 2 then
            AddSourceToken(FSource, startPos + 1, FPos - startPos - 2, shDefault, TCssSyntaxHighlighter.CategoryEmbeddedCSS);
          AddSourceToken(FSource, FPos - 1, 1, shSymbol); // Closing quote
          end
        else
          AddSourceToken(FSource, startPos, FPos - startPos, shStrings);
      end;
      end;
    // A character that starts no attribute, such as < or ", is a token of its own
    if FPos = attrStart then
      begin
      AddSourceToken(FSource, FPos, 1, shDefault);
      Inc(FPos);
      end;
    end;

  // Process closing >
  if (FPos <= FSourceLen) and (FSource[FPos] = '>') then
    begin
    AddToken('>', shSymbol);
    Inc(FPos);

    // Check if we're entering script or style context
    Case FCurrentTag of
    'script':
      FState := hsScript;
    'style':
      FState := hsStyle;
    else
      FState := hsText;
    end;
    end;

  endPos := FPos - 1;
end;

procedure THtmlSyntaxHighlighter.ProcessTagClose(var endPos: Integer);

var
  nameStart, nameEnd: Integer;

begin

  // Process </
  AddToken('</', shSymbol);
  Inc(FPos, 2);
  ProcessWhiteSpace;

  // Process tag name
  nameStart := FPos;
  while (FPos <= FSourceLen) and IsTagChar(FSource[FPos]) do
    Inc(FPos);

  if FPos > nameStart then
    begin
    nameEnd := FPos - 1;
    AddSourceToken(FSource, nameStart, nameEnd - nameStart + 1, shKeyword);
    end;
  ProcessWhiteSpace;

  // Process closing >
  if (FPos <= FSourceLen) and (FSource[FPos] = '>') then
    begin
    AddToken('>', shSymbol);
    Inc(FPos);
    end;

  FState := hsText;
  endPos := FPos - 1;
end;


procedure THtmlSyntaxHighlighter.ProcessComment(var endPos: Integer);

var
  startPos: Integer;

begin
  startPos := FPos;

  while (FPos <= FSourceLen) do
    begin
    if (FPos + 2 <= FSourceLen) and (FSource[FPos] = '-') and (FSource[FPos + 1] = '-') and (FSource[FPos + 2] = '>') then
      begin
      Inc(FPos, 3);
      FState := hsText;
      break;
      end;
    Inc(FPos);
    end;

  endPos := FPos - 1;
  AddSourceToken(FSource, startPos, endPos - startPos + 1, shComment, CategoryHTMLComment);
end;


procedure THtmlSyntaxHighlighter.ProcessDoctype(var endPos: Integer);

var
  startPos: Integer;

begin
  startPos := FPos;
  while (FPos <= FSourceLen) and (FSource[FPos] <> '>') do
    Inc(FPos);
  if (FPos <= FSourceLen) and (FSource[FPos] = '>') then
    Inc(FPos);
  endPos := FPos - 1;
  AddSourceToken(FSource, startPos, endPos - startPos + 1, shDirective);
  FState := hsText;
end;


procedure THtmlSyntaxHighlighter.ProcessCDATA(var endPos: Integer);

var
  startPos: Integer;

begin
  startPos := FPos;

  while (FPos <= FSourceLen) do
    begin
    if (FPos + 2 <= FSourceLen) and (FSource[FPos] = ']') and (FSource[FPos + 1] = ']') and (FSource[FPos + 2] = '>') then
      begin
      Inc(FPos, 3);
      FState := hsText;
      break;
      end;
    Inc(FPos);
    end;

  endPos := FPos - 1;
  AddSourceToken(FSource, startPos, endPos - startPos + 1, shRawString);
end;


// Emit a token of an embedded JavaScript or CSS highlighter
procedure THtmlSyntaxHighlighter.EmbeddedToken(Sender: TObject; const aToken: TSyntaxToken);

var
  lToken: TSyntaxToken;
begin
  lToken := aToken;
  EmitToken(lToken);
end;


procedure THtmlSyntaxHighlighter.ProcessScript(var endPos: Integer);

var
  startPos: Integer;
  scriptContent: string;

begin
  startPos := FPos;
  FPos := FindEndTag('</script>');
  endPos := FPos - 1;
  if endPos >= startPos then
    begin
    scriptContent := Copy(FSource, startPos, endPos - startPos + 1);
    FJsHighlighter.Execute(scriptContent, @EmbeddedToken);
    end;

  FState := hsText;
end;

procedure THtmlSyntaxHighlighter.ProcessStyle(var endPos: Integer);

var
  startPos: Integer;
  styleContent: string;

begin
  startPos := FPos;
  FPos := FindEndTag('</style>');
  endPos := FPos - 1;
  if endPos >= startPos then
    begin
    styleContent := Copy(FSource, startPos, endPos - startPos + 1);
    FCssHighlighter.Execute(styleContent, @EmbeddedToken);
    end;
  FState := hsText;
end;


procedure THtmlSyntaxHighlighter.ProcessEntity(var endPos: Integer);

var
  startPos: Integer;

begin
  startPos := FPos;
  Inc(FPos); // Skip &

  // Numeric entity &#123; or &#xABC;
  if (FPos <= FSourceLen) and (FSource[FPos] = '#') then
    begin
    Inc(FPos);
    if (FPos <= FSourceLen) and (FSource[FPos] = 'x') then
      Inc(FPos); // Hex entity
    while (FPos <= FSourceLen) and (FSource[FPos] in ['0'..'9', 'A'..'F', 'a'..'f']) do
      Inc(FPos);
    end
  // Named entity &amp; &lt; etc.
  else
    begin
    while (FPos <= FSourceLen) and (FSource[FPos] in ['a'..'z', 'A'..'Z', '0'..'9']) do
      Inc(FPos);
    end;

  // Skip closing ;
  if (FPos <= FSourceLen) and (FSource[FPos] = ';') then
    Inc(FPos);

  endPos := FPos - 1;
  AddSourceToken(FSource, startPos, endPos - startPos + 1, shEscape);
end;


function THtmlSyntaxHighlighter.IsWordChar(ch: Char): Boolean;
begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '_', '-'];
end;

function THtmlSyntaxHighlighter.IsTagChar(ch: Char): Boolean;

begin
  Result := ch in ['a'..'z', 'A'..'Z', '0'..'9', '-', ':'];
end;


function THtmlSyntaxHighlighter.PeekString(const s: string): Boolean;

begin
  Result := MatchAt(FPos, s);
end;


// Compare the source at aPos with aValue, ASCII case-insensitive
function THtmlSyntaxHighlighter.MatchAt(aPos: Integer; const aValue: string): Boolean;

var
  I: Integer;
  lSource, lValue: Char;
begin
  if aPos + Length(aValue) - 1 > FSourceLen then
    Exit(False);
  for I := 1 to Length(aValue) do
    begin
    lSource := FSource[aPos + I - 1];
    lValue := aValue[I];
    if lSource <> lValue then
      begin
      if lSource in ['A'..'Z'] then
        lSource := Chr(Ord(lSource) + 32);
      if lValue in ['A'..'Z'] then
        lValue := Chr(Ord(lValue) + 32);
      if lSource <> lValue then
        Exit(False);
      end;
    end;
  Result := True;
end;


// Position of aTag, which starts with '<', at or after FPos; FSourceLen + 1 if not found
function THtmlSyntaxHighlighter.FindEndTag(const aTag: string): Integer;

var
  lOffset: SizeInt;
begin
  Result := FPos;
  while Result + Length(aTag) - 1 <= FSourceLen do
    begin
    lOffset := IndexByte(FSource[Result], FSourceLen - Result + 1, Ord('<'));
    if lOffset < 0 then
      Break;
    Inc(Result, lOffset);
    if MatchAt(Result, aTag) then
      Exit;
    Inc(Result);
    end;
  Result := FSourceLen + 1;
end;


function THtmlSyntaxHighlighter.ExtractTagName(const tagContent: string): string;

var
  i: Integer;

begin
  Result := '';
  i := 1;
  while (i <= Length(tagContent)) and (tagContent[i] in [' ', #9, #10, #13]) do
    Inc(i);

  while (i <= Length(tagContent)) and IsTagChar(tagContent[i]) do
    begin
    Result := Result + tagContent[i];
    Inc(i);
    end;

  Result := LowerCase(Result);
end;

class function THtmlSyntaxHighlighter.GetLanguages: TStringDynArray;
begin
  Result:=['html','htm']
end;


class procedure THtmlSyntaxHighlighter.RegisterDefaultCategories;

begin
  CategoryHTML:=RegisterCategory('HTML');
  CategoryHTMLStyleAttr:=RegisterCategory('HTMLStyleAttr');
  CategoryHTMLAttribute:=RegisterCategory('HTMLAttribute');
  inherited ;
end;


procedure THtmlSyntaxHighlighter.CheckCategories;
begin
  if CategoryHTML=0 then
    RegisterDefaultCategories;
end;


procedure THtmlSyntaxHighlighter.DoExecute(const source: string);
var
  endPos: Integer;
begin
  FSourceLen:=Length(source);
  if FSourceLen=0 then
    Exit;
  FSource := source;
  FPos := 1;
  endpos:=0;
  FState := hsText;
  while FPos <= FSourceLen do
    begin
    case FState of
      hsText:
        begin
        if (FPos <= FSourceLen) and (FSource[FPos] = '<') then
          begin
          if PeekString('<!--') then
            begin
            AddToken('<!--', shSymbol);
            Inc(FPos, 4);
            FState := hsComment;
            ProcessComment(endPos);
            end
          else if PeekString('<!DOCTYPE') then
            begin
            FState := hsDoctype;
            ProcessDoctype(endPos);
            end
          else if PeekString('<![CDATA[') then
            begin
            AddToken('<![CDATA[', shSymbol);
            Inc(FPos, 9);
            FState := hsCDATA;
            ProcessCDATA(endPos);
            end
          else if (FPos < FSourceLen) and (FSource[FPos + 1] = '/') then
            begin
            FState := hsTagClose;
            ProcessTagClose(endPos);
            end
          else
            begin
            FState := hsTagOpen;
            ProcessTagOpen(endPos);
            end;
          end
        else
          ProcessText(endPos);
        end;
      hsComment: ProcessComment(endPos);
      hsScript: ProcessScript(endPos);
      hsStyle: ProcessStyle(endPos);
      hsCDATA: ProcessCDATA(endPos);
      hsDoctype: ProcessDoctype(endPos);
      hsTagOpen: ProcessTagOpen(endPos);
      hsTagClose: ProcessTagClose(endPos);
    end;
    if FPos = endPos then
      Inc(FPos);
    end;
end;


function DoHtmlHighlighting(const source: string): TSyntaxTokenArray;

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

initialization
  THtmlSyntaxHighlighter.Register;
end.
