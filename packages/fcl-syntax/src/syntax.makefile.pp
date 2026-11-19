{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Makefile syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.makefile;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TMakeSyntaxHighlighter }

  TMakeSyntaxHighlighter = class(TByteSyntaxHighlighter)
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens
    procedure DoExecute(const aSource: string); override;
  end;

implementation

const
  MakeKeywords = 'define endef undefine ifdef ifndef ifeq ifneq else endif include -include sinclude override export unexport private vpath';

class function TMakeSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['makefile', 'make'];
end;


procedure TMakeSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst, lPos, lDepth: SizeInt;
  lStack: array of char;
  lValue: string;
  lKind: TSyntaxHighlightKind;
  lLineStart, lRecipe, lTarget: boolean;
begin
  Start(aSource);
  lLineStart := True;
  lRecipe := False;
  lTarget := True;
  lStack := nil;
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    lKind := shDefault;
    if lLineStart then
      begin
      lRecipe := Ch = #9;
      lLineStart := False;
      lTarget := not lRecipe;
      end;
    if Ch in [#10, #13] then
      begin
      if At(#13#10) then
        Inc(FCursor, 2)
      else
        Inc(FCursor);
      lLineStart := Ch(-(FCursor - lFirst) - 1) <> '\';
      end
    else if At('$(') or At('${') then
      begin
      lDepth := 1;
      SetLength(lStack, 16);
      if Ch(1) = '(' then
        lStack[0] := ')'
      else
        lStack[0] := '}';
      Inc(FCursor, 2);
      while (FCursor <= FLimit) and (lDepth > 0) do
        begin
        if At('$(') or At('${') then
          begin
          if lDepth = Length(lStack) then
            SetLength(lStack, lDepth * 2);
          if Ch(1) = '(' then
            lStack[lDepth] := ')'
          else
            lStack[lDepth] := '}';
          Inc(lDepth);
          Inc(FCursor, 2);
          end
        else if Ch in ['(', '{'] then
          begin
          if lDepth = Length(lStack) then
            SetLength(lStack, lDepth * 2);
          if Ch = '(' then
            lStack[lDepth] := ')'
          else
            lStack[lDepth] := '}';
          Inc(lDepth);
          Inc(FCursor);
          end
        else if Ch = lStack[lDepth - 1] then
          begin
          Dec(lDepth);
          Inc(FCursor);
          end
        else
          Inc(FCursor);
        end;
      lKind := shInterpolation;
      end
    else if Ch = '$' then
      begin
      Inc(FCursor);
      if FCursor <= FLimit then
        Unknown;
      lKind := shInterpolation;
      end
    else if (Ch = '#') and not lRecipe then
      begin
      LineEnd;
      lKind := shComment;
      end
    else if At('\#') or At('\$') or At('\\') then
      Inc(FCursor, 2)
    else if Ch in ['=', ':', '?', '+', '!'] then
      begin
      Inc(FCursor);
      if Ch = ':' then
        Inc(FCursor);
      if Ch = '=' then
        Inc(FCursor);
      lTarget := False;
      lKind := shOperator;
      end
    else if Ch in NameStartChars + ['.', '%', '-', '/'] then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in NameChars + ['.', '%', '-', '/']) do
        Inc(FCursor);
      lValue := Copy(FText, lFirst, FCursor - lFirst);
      if not lRecipe then
        begin
        if WordsContain(MakeKeywords, lValue) then
          lKind := shKeyword
        else
          begin
          lPos := FCursor;
          while (lPos <= FLimit) and (FText[lPos] in [' ', #9]) do
            Inc(lPos);
          if (lPos <= FLimit) and (FText[lPos] in ['=', '?', '+', '!']) then
            lKind := shKey
          else if lTarget then
            lKind := shAttribute;
          end;
        end;
      end
    else if Ch in [' ', #9] then
      begin
      while (FCursor <= FLimit) and (Ch in [' ', #9]) do
        Inc(FCursor);
      end
    else
      Unknown;
    Emit(lFirst, lKind);
    end;
  Finish;
end;

initialization
  TMakeSyntaxHighlighter.Register;
end.
