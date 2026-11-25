{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Code highlighting hook for markdown renderers.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Markdown.CodeHighlight;

{$mode ObjFPC}{$H+}

interface

uses
  Markdown.Utils;

Type
  // Kind of a token in highlighted source code.
  TMarkdownCodeTokenKind = (ctkDefault, ctkKeyword, ctkComment, ctkDirective, ctkNumber, ctkString, ctkEscape,
                            ctkSymbol, ctkOperator, ctkAttribute, ctkKey, ctkSection, ctkError);

  // One token of highlighted source code.
  TMarkdownCodeToken = record
    Text : TMarkdownString;
    Kind : TMarkdownCodeTokenKind;
  end;
  TMarkdownCodeTokenArray = array of TMarkdownCodeToken;

  { TMarkdownCodeHighlighter }

  // Splits source code into tokens for the renderers.
  TMarkdownCodeHighlighter = class(TObject)
  private
    class var _DefaultHighlighter : TMarkdownCodeHighlighter;
  public
    class destructor Done;
    // Install aHighlighter as default highlighter and free the previous one. Nil removes the default highlighter.
    class procedure SetDefaultHighlighter(aHighlighter : TMarkdownCodeHighlighter);
    // Split aSource, written in language aLang, into tokens. Returns False when aLang is not supported.
    function Highlight(const aLang, aSource : TMarkdownString; out aTokens : TMarkdownCodeTokenArray) : Boolean; virtual; abstract;
    // Highlighter for renderers without highlighter of their own. Owned by this class.
    class property DefaultHighlighter : TMarkdownCodeHighlighter read _DefaultHighlighter;
  end;

const
  CodeTokenKindNames : Array[TMarkdownCodeTokenKind] of TMarkdownString = (
    'default', 'keyword', 'comment', 'directive', 'number', 'string', 'escape',
    'symbol', 'operator', 'attribute', 'key', 'section', 'error');

implementation

{ TMarkdownCodeHighlighter }

class destructor TMarkdownCodeHighlighter.Done;

begin
  SetDefaultHighlighter(Nil);
end;


class procedure TMarkdownCodeHighlighter.SetDefaultHighlighter(aHighlighter: TMarkdownCodeHighlighter);

begin
  if _DefaultHighlighter=aHighlighter then
    Exit;
  _DefaultHighlighter.Free;
  _DefaultHighlighter:=aHighlighter;
end;


end.
