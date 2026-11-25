{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2025 by Michael Van Canneyt

    Code highlighter for fcl-md renderers using the registered syntax highlighters.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$MODE objfpc}
{$H+}

unit Syntax.MdHighlighter;

interface

uses
  Markdown.Utils, Markdown.CodeHighlight, syntax.highlighter;

type
  { TSyntaxMarkdownCodeHighlighter }

  // Installed as default markdown code highlighter when this unit is used.
  // Supports the languages of the syntax.* units in the program, e.g. syntax.all for all of them.
  TSyntaxMarkdownCodeHighlighter = class(TMarkdownCodeHighlighter)
  public
    // Markdown code token kind for syntax token kind aKind.
    class function MapKind(aKind : TSyntaxHighlightKind) : TMarkdownCodeTokenKind; virtual;
    // Split aSource into tokens with the syntax highlighter registered for aLang, case insensitive.
    function Highlight(const aLang, aSource : TMarkdownString; out aTokens : TMarkdownCodeTokenArray) : Boolean; override;
  end;

implementation

uses
{$IFDEF FPC_DOTTEDUNITS}
  System.SysUtils;
{$ELSE}
  SysUtils;
{$ENDIF}

const
  KindMap : Array[TSyntaxHighlightKind] of TMarkdownCodeTokenKind = (
    ctkDefault,    // shDefault
    ctkError,      // shInvalid
    ctkSymbol,     // shSymbol
    ctkKeyword,    // shKeyword
    ctkComment,    // shComment
    ctkDirective,  // shDirective
    ctkNumber,     // shNumbers
    ctkString,     // shCharacters
    ctkString,     // shStrings
    ctkDefault,    // shAssembler
    ctkString,     // shRegex
    ctkEscape,     // shInterpolation
    ctkEscape,     // shEscape
    ctkString,     // shRawString
    ctkString,     // shTemplate
    ctkOperator,   // shLambda
    ctkOperator,   // shOperator
    ctkError,      // shError
    ctkError,      // shWarning
    ctkAttribute,  // shAttribute
    ctkSection,    // shSection
    ctkKey         // shKey
  );

{ TSyntaxMarkdownCodeHighlighter }

class function TSyntaxMarkdownCodeHighlighter.MapKind(aKind: TSyntaxHighlightKind): TMarkdownCodeTokenKind;

begin
  Result:=KindMap[aKind];
end;


function TSyntaxMarkdownCodeHighlighter.Highlight(const aLang, aSource: TMarkdownString; out aTokens: TMarkdownCodeTokenArray): Boolean;

var
  lClass : TSyntaxHighlighterClass;
  lHighlighter : TSyntaxHighlighter;
  lSyntaxTokens : TSyntaxTokenArray;
  I : Integer;

begin
  aTokens:=[];
  lClass:=TSyntaxHighlighterRegistry.Instance.FindSyntaxHighlighterClass(LowerCase(MarkdownToString(aLang)));
  Result:=Assigned(lClass);
  if not Result then
    Exit;
  lHighlighter:=lClass.Create;
  try
    lSyntaxTokens:=lHighlighter.Execute(MarkdownToString(aSource));
  finally
    lHighlighter.Free;
  end;
  SetLength(aTokens,Length(lSyntaxTokens));
  for I:=0 to Length(lSyntaxTokens)-1 do
    begin
    aTokens[I].Text:=StringToMarkdown(lSyntaxTokens[I].Text);
    aTokens[I].Kind:=MapKind(lSyntaxTokens[I].Kind);
    end;
end;


initialization
  TMarkdownCodeHighlighter.SetDefaultHighlighter(TSyntaxMarkdownCodeHighlighter.Create);
end.
