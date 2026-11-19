{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Diff and patch syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.diff;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TDiffSyntaxHighlighter }

  TDiffSyntaxHighlighter = class(TByteSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
  public
    // Split aSource in tokens, one per line
    function Execute(const aSource: string): TSyntaxTokenArray; override;
  end;

implementation

class function TDiffSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['diff', 'patch'];
end;


function TDiffSyntaxHighlighter.Execute(const aSource: string): TSyntaxTokenArray;

var
  lFirst: SizeInt;
  lKind: TSyntaxHighlightKind;
begin
  Start(aSource);
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    lKind := shDefault;
    if At('+++') or At('---') or At('***') or At('diff ') or At('index ') then
      lKind := shDirective
    else if At('@@') then
      lKind := shSection
    else if Ch = '+' then
      lKind := shStrings
    else if Ch = '-' then
      lKind := shError
    else if Ch = '!' then
      lKind := shWarning
    else if Ch = '\' then
      lKind := shComment;
    LineEnd;
    if Ch = #13 then
      Inc(FCursor);
    if Ch = #10 then
      Inc(FCursor);
    Emit(lFirst, lKind);
    end;
  Result := Finish;
end;

initialization
  TDiffSyntaxHighlighter.Register;
end.
