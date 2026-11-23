{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 2026 by Michael Van Canneyt

    Barye syntax highlighter

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE objfpc}
{$H+}

unit syntax.barye;

interface

uses
  {$IFDEF FPC_DOTTEDUNITS}
  System.Types, syntax.highlighter, syntax.table;
  {$ELSE}
  Types, syntax.highlighter, syntax.table;
  {$ENDIF}

type

  { TBaryeSyntaxHighlighter }

  TBaryeSyntaxHighlighter = class(TByteSyntaxHighlighter)
  private
    procedure IncludeToken;
    procedure StringToken;
    procedure Classify(var aTokens: TSyntaxTokenArray);
  protected
    class function GetLanguages: TStringDynArray; override;
    // Split aSource in tokens, then refine their kinds from the local context
    procedure DoExecute(const aSource: string); override;
  end;

implementation

const
  BaryeNameStart = ['A'..'Z', 'a'..'z', '_'];
  BaryeNameRest = BaryeNameStart + ['0'..'9'];
  BaryeReserved = 'return defer pass leave';
  BaryeConstants = 'Some None Ok Err true false';
  BaryeOpcodes = 'mov lea jmp nop ret syscall call cmp jne je jnz jz add sub push pop xor and or test';
  BaryeRegisters =
    'rax eax ax al ah rbx ebx bx bl bh rcx ecx cx cl ch rdx edx dx dl dh rsi esi si sil rdi edi di dil rbp ebp bp bpl rsp esp sp spl rip eip '
    + 'r8 r9 r10 r11 r12 r13 r14 r15 r8d r9d r10d r11d r12d r13d r14d r15d r8w r9w r10w r11w r12w r13w r14w r15w '
    + 'r8b r9b r10b r11b r12b r13b r14b r15b';

type
  TBaryeRole = (brBlock, brFunction, brCode, brStruct, brMain, brControl, brWith, brMatch, brLet);

  TBaryeFrame = record
    CloseText: string;
    Role: TBaryeRole;
    ResultBearing, CountedMatch: boolean;
    MarkerState: integer;
  end;

  TPendingHeader = (phNone, phFunction, phCode, phStruct);

class function TBaryeSyntaxHighlighter.GetLanguages: TStringDynArray;

begin
  Result := ['barye', 'bry'];
end;


// Tokens for [#include "file"#] or [#include <file>#]
procedure TBaryeSyntaxHighlighter.IncludeToken;

var
  lFirst: SizeInt;
  lQuote: char;
begin
  lFirst := FCursor;
  Inc(FCursor, 2);
  Emit(lFirst, shDirective);
  lFirst := FCursor;
  while (FCursor <= FLimit) and (Ch in [' ', #9]) do
    Inc(FCursor);
  Emit(lFirst, shDefault);
  if not At('include') or not (Ch(7) in [' ', #9]) then
    Exit;
  lFirst := FCursor;
  Inc(FCursor, 7);
  Emit(lFirst, shDirective);
  lFirst := FCursor;
  while (FCursor <= FLimit) and (Ch in [' ', #9]) do
    Inc(FCursor);
  Emit(lFirst, shDefault);
  if not (Ch in ['"', '<']) then
    Exit;
  lQuote := Ch;
  lFirst := FCursor;
  Inc(FCursor);
  Emit(lFirst, shSymbol);
  lFirst := FCursor;
  while (FCursor <= FLimit) and not (Ch in [#0, #10, #13, '"', '<', '>']) do
    Inc(FCursor);
  Emit(lFirst, shStrings);
  if ((lQuote = '"') and (Ch <> '"')) or ((lQuote = '<') and (Ch <> '>')) then
    Exit;
  lFirst := FCursor;
  Inc(FCursor);
  Emit(lFirst, shSymbol);
  lFirst := FCursor;
  while (FCursor <= FLimit) and (Ch in [' ', #9]) do
    Inc(FCursor);
  Emit(lFirst, shDefault);
  if At('#]') then
    begin
    lFirst := FCursor;
    Inc(FCursor, 2);
    Emit(lFirst, shDirective);
    end;
end;


// Tokens for a string literal, with escapes as separate tokens
procedure TBaryeSyntaxHighlighter.StringToken;

var
  lFirst: SizeInt;
begin
  lFirst := FCursor;
  Inc(FCursor);
  while FCursor <= FLimit do
    begin
    if (Ch < #32) or (Ch = #127) then
      Break;
    if Ch = '"' then
      begin
      Inc(FCursor);
      Break;
      end;
    if (Ch = '\') and (Ch(1) in ['\', '"', 'n', 'r', 't', '0']) then
      begin
      Emit(lFirst, shStrings);
      lFirst := FCursor;
      Inc(FCursor, 2);
      Emit(lFirst, shEscape);
      lFirst := FCursor;
      end
    else
      Inc(FCursor);
    end;
  Emit(lFirst, shStrings);
end;


procedure TBaryeSyntaxHighlighter.DoExecute(const aSource: string);

var
  lFirst: SizeInt;
  lValue: string;
  lKind: TSyntaxHighlightKind;
  lTokens: TSyntaxTokenArray;
  I: Integer;
begin
  Start(aSource, True);
  while FCursor <= FLimit do
    begin
    lFirst := FCursor;
    if (FCursor = 1) and (Ch = '#') then
      begin
      LineEnd;
      Emit(lFirst, shComment);
      end
    else if At('//') then
      begin
      LineEnd;
      Emit(lFirst, shComment);
      end
    else if At('/*') then
      begin
      Block('/*', '*/', False);
      Emit(lFirst, shComment);
      end
    else if At('[#') then
      IncludeToken
    else if Ch = '"' then
      StringToken
    else if Ch in [' ', #9, #10, #13] then
      begin
      White;
      Emit(lFirst, shDefault);
      end
    else if Ch in BaryeNameStart then
      begin
      Inc(FCursor);
      while (FCursor <= FLimit) and (Ch in BaryeNameRest) do
        Inc(FCursor);
      lValue := Copy(FText, lFirst, FCursor - lFirst);
      lKind := shDefault;
      if WordsContain(BaryeReserved, lValue) then
        lKind := shKeyword
      else if WordsContain(BaryeConstants, lValue) then
        lKind := shKey;
      Emit(lFirst, lKind);
      end
    else if Ch in ['0'..'9'] then
      begin
      while (FCursor <= FLimit) and (Ch in ['0'..'9']) do
        Inc(FCursor);
      Emit(lFirst, shNumbers);
      end
    else if At('<<=') or At('>>=') then
      begin
      Inc(FCursor, 3);
      Emit(lFirst, shOperator);
      end
    else if At('[{') or At('}]') or At('#]') then
      begin
      Inc(FCursor, 2);
      Emit(lFirst, shSymbol);
      end
    else if At('<<') or At('>>') or At('<=') or At('>=') or At('!=') or At('==') or At(':=')
            or At('+=') or At('-=') or At('*=') or At('/=') or At('%=') then
      begin
      Inc(FCursor, 2);
      Emit(lFirst, shOperator);
      end
    else if At('..') then
      begin
      Inc(FCursor, 2);
      Emit(lFirst, shSymbol);
      end
    else
      begin
      lKind := shDefault;
      if Ch in ['+', '-', '*', '/', '%', '=', '<', '>'] then
        lKind := shOperator
      else if Ch in ['{', '}', '(', ')', '[', ']', ',', ';', ':', '?', '.'] then
        lKind := shSymbol;
      Unknown;
      Emit(lFirst, lKind);
      end;
    end;
  lTokens := Finish;
  Classify(lTokens);
  for I := 0 to High(lTokens) do
    EmitToken(lTokens[I]);
end;


// True if aValue is an identifier
function IsName(const aValue: string): boolean;

var
  I: Integer;
begin
  Result := False;
  if (aValue = '') or not (aValue[1] in BaryeNameStart) then
    Exit;
  for I := 2 to Length(aValue) do
    if not (aValue[I] in BaryeNameRest) then
      Exit;
  Result := True;
end;


// True if aValue contains a line break
function HasEol(const aValue: string): boolean;

begin
  Result := (Pos(#10, aValue) > 0) or (Pos(#13, aValue) > 0);
end;


// True for whitespace and comment tokens
function Trivia(const aToken: TSyntaxToken): boolean;

begin
  Result := (aToken.Kind = shComment)
            or ((aToken.Kind = shDefault) and (aToken.Text <> '') and (aToken.Text[1] in [' ', #9, #10, #13]));
end;


procedure TBaryeSyntaxHighlighter.Classify(var aTokens: TSyntaxTokenArray);

var
  lNextToken: array of SizeInt;
  lGapEol: array of boolean;
  lFrames: array of TBaryeFrame;
  I: Integer;
  lNext, lCount, lFollowing, lPrevious, lDepth, lRoundDepth, lSquareDepth, lReceiverDepth, lHeaderRound,
  lTypeRound, lTypeAngle, lOpaqueAt: SizeInt;
  lValue, lAfterText, lBeforeText: string;
  lRole, lPendingRole: TBaryeRole;
  lHeader: TPendingHeader;
  lRootStart, lProgramHeader, lHeaderName, lExpectName, lReceiver, lHeaderResult, lTypePosition, lStatementStart,
  lPendingBody, lElseAllowed, lInAssembly, lAssemblyHead, lBranchOperand, lLetBinding, lImporting, lPolicy, lOnLine,
  lCurrentResult, lValidClose, lPendingCounted: boolean;

  function Root: boolean;
  begin
    Result := (lDepth = 0) and (lRoundDepth = 0) and (lSquareDepth = 0);
  end;

  function InRoutine: boolean;
  begin
    Result := (lDepth > 0) and (lFrames[lDepth - 1].Role <> brStruct) and (lFrames[lDepth - 1].Role <> brMain);
  end;

  function TopRole(aRole: TBaryeRole): boolean;
  begin
    Result := (lDepth > 0) and (lFrames[lDepth - 1].Role = aRole);
  end;

  function ReceiverShape(aOpenIndex: SizeInt): boolean;
  var
    lParamIndex, lColonIndex: SizeInt;
  begin
    Result := False;
    if aOpenIndex >= lCount then
      Exit;
    lParamIndex := lNextToken[aOpenIndex];
    if (lParamIndex >= lCount) or not IsName(aTokens[lParamIndex].Text) then
      Exit;
    lColonIndex := lNextToken[lParamIndex];
    Result := (lColonIndex < lCount) and (aTokens[lColonIndex].Text = ':');
  end;

  function StartsWithDigit(const aText: string): boolean;
  begin
    Result := (aText <> '') and (aText[1] in ['0'..'9']);
  end;

  procedure Push(const aCloseText: string; aRole: TBaryeRole);
  begin
    if lDepth = Length(lFrames) then
      SetLength(lFrames, lDepth * 2 + 16);
    lFrames[lDepth].CloseText := aCloseText;
    lFrames[lDepth].Role := aRole;
    lFrames[lDepth].ResultBearing := lCurrentResult;
    lFrames[lDepth].CountedMatch := (aRole = brMatch) and lPendingCounted;
    lFrames[lDepth].MarkerState := 0;
    Inc(lDepth);
  end;

begin
  lCount := Length(aTokens);
  lNextToken := nil;
  lGapEol := nil;
  SetLength(lNextToken, lCount);
  SetLength(lGapEol, lCount);
  lFollowing := lCount;
  lOnLine := False;
  for I := lCount - 1 downto 0 do
    begin
    lNextToken[I] := lFollowing;
    lGapEol[I] := lOnLine;
    if Trivia(aTokens[I]) then
      lOnLine := lOnLine or HasEol(aTokens[I].Text)
    else
      begin
      lFollowing := I;
      lOnLine := False;
      end;
    end;
  lFrames := nil;
  lDepth := 0;
  lRoundDepth := 0;
  lSquareDepth := 0;
  lReceiverDepth := 0;
  lHeaderRound := 0;
  lTypeRound := 0;
  lTypeAngle := 0;
  lPrevious := -1;
  lOpaqueAt := -1;
  lRootStart := True;
  lProgramHeader := False;
  lHeaderName := False;
  lHeader := phNone;
  lExpectName := False;
  lReceiver := False;
  lHeaderResult := False;
  lTypePosition := False;
  lStatementStart := True;
  lPendingBody := False;
  lPendingRole := brBlock;
  lElseAllowed := False;
  lCurrentResult := False;
  lAssemblyHead := True;
  lBranchOperand := False;
  lLetBinding := False;
  lImporting := False;
  lPolicy := False;
  lPendingCounted := False;
  for I := 0 to lCount - 1 do
    begin
    lValue := aTokens[I].Text;
    if lOpaqueAt >= 0 then
      begin
      aTokens[I].Kind := shDefault;
      Continue;
      end;
    if Trivia(aTokens[I]) then
      begin
      if HasEol(lValue) then
        begin
        lStatementStart := True;
        lAssemblyHead := True;
        lBranchOperand := False;
        lLetBinding := False;
        if (lHeader = phNone) and (lTypeRound = 0) and (lTypeAngle = 0) then
          lTypePosition := False;
        end;
      Continue;
      end;
    lNext := lNextToken[I];
    lAfterText := '';
    if lNext < lCount then
      lAfterText := aTokens[lNext].Text;
    lBeforeText := '';
    if lPrevious >= 0 then
      lBeforeText := aTokens[lPrevious].Text;
    if TopRole(brMain) and (lFrames[lDepth - 1].CloseText = '}]') and (lValue <> '}]') then
      begin
      if (lFrames[lDepth - 1].MarkerState = 0) and (lValue = 'pass') then
        lFrames[lDepth - 1].MarkerState := 1
      else if (lFrames[lDepth - 1].MarkerState = 1) and (lValue = ';') then
        lFrames[lDepth - 1].MarkerState := 2
      else
        lFrames[lDepth - 1].MarkerState := -1;
      end;
    lInAssembly := TopRole(brCode);
    if lInAssembly and (lValue <> '}') then
      begin
      if IsName(lValue) then
        begin
        aTokens[I].Kind := shDefault;
        if lAssemblyHead then
          begin
          if WordsContain(BaryeOpcodes, lValue) and (lAfterText <> ':') then
            aTokens[I].Kind := shAssembler;
          lBranchOperand := lValue = 'jmp';
          lAssemblyHead := False;
          end
        else if lBranchOperand then
          begin
          aTokens[I].Kind := shAttribute;
          lBranchOperand := False;
          end
        else if WordsContain(BaryeRegisters, lValue) then
          aTokens[I].Kind := shKey;
        end
      else if lValue = ':' then
        lAssemblyHead := True
      else if lValue = ';' then
        begin
        lAssemblyHead := True;
        lBranchOperand := False;
        end;
      lPrevious := I;
      Continue;
      end;
    // Literals and directives cannot start declarations or delimit blocks
    if aTokens[I].Kind in [shStrings, shEscape, shDirective] then
      begin
      lPrevious := I;
      lStatementStart := False;
      Continue;
      end;
    if lHeaderName and IsName(lValue) then
      begin
      if not WordsContain(BaryeReserved, lValue) then
        aTokens[I].Kind := shAttribute;
      lHeaderName := False;
      lStatementStart := False;
      end
    else if lExpectName and IsName(lValue) then
      begin
      if not WordsContain(BaryeReserved, lValue) then
        aTokens[I].Kind := shAttribute;
      lExpectName := False;
      lStatementStart := False;
      end
    else if IsName(lValue) then
      begin
      if WordsContain(BaryeReserved, lValue) then
        aTokens[I].Kind := shKeyword
      else if lRootStart and Root and lStatementStart and ((lValue = 'program') or (lValue = 'unit'))
              and IsName(lAfterText) and not lGapEol[I] then
        begin
        aTokens[I].Kind := shKeyword;
        lProgramHeader := lValue = 'program';
        lHeaderName := True;
        lRootStart := False;
        end
      else if Root and lStatementStart and (lHeader = phNone) and ((lValue = 'fn') or (lValue = 'codefn') or (lValue = 'struct'))
              and ((IsName(lAfterText) and not lGapEol[I]) or ((lValue = 'fn') and (lAfterText = '(') and ReceiverShape(lNext))) then
        begin
        aTokens[I].Kind := shKeyword;
        if lValue = 'fn' then
          lHeader := phFunction
        else if lValue = 'codefn' then
          lHeader := phCode
        else
          lHeader := phStruct;
        lExpectName := True;
        lHeaderResult := False;
        lHeaderRound := lRoundDepth;
        lTypePosition := False;
        end
      else if Root and lStatementStart and (lValue = 'pub') and WordsContain('fn codefn struct', lAfterText) and not lGapEol[I] then
        begin
        aTokens[I].Kind := shKeyword;
        lPrevious := I;
        Continue;
        end
      else if Root and lStatementStart and WordsContain('uses use from', lValue) and IsName(lAfterText) and not lGapEol[I] then
        begin
        aTokens[I].Kind := shKeyword;
        lImporting := True;
        end
      else if lImporting and WordsContain('use as', lValue) then
        aTokens[I].Kind := shKeyword
      else if lImporting then
        aTokens[I].Kind := shAttribute
      else if lTypePosition then
        begin
        aTokens[I].Kind := shKey;
        if (lHeader in [phFunction, phCode]) and (lRoundDepth = lHeaderRound) then
          lHeaderResult := True;
        end
      else if (lBeforeText = '.') and (lPrevious >= 0) then
        aTokens[I].Kind := shAttribute
      else if InRoutine and lStatementStart and (lValue = 'let') and not lGapEol[I]
              and (IsName(lAfterText) or (lAfterText = ':') or (lAfterText = '[{') or ((lAfterText = '(') and (lNext > I + 1))) then
        begin
        aTokens[I].Kind := shKeyword;
        lLetBinding := True;
        end
      else if lLetBinding and (lValue = 'mut') then
        aTokens[I].Kind := shKeyword
      else if lLetBinding then
        aTokens[I].Kind := shAttribute
      else if TopRole(brLet) and lStatementStart and (lValue = 'mut') then
        aTokens[I].Kind := shKeyword
      else if TopRole(brLet) and ((lAfterText = ':') or (lAfterText = '=')) then
        aTokens[I].Kind := shAttribute
      else if InRoutine and (lValue = 'result') and lCurrentResult and (lAfterText <> '(') then
        aTokens[I].Kind := shKey
      else if ((lValue = 'if') or (lValue = 'while')) and InRoutine and (lStatementStart or TopRole(brMatch))
              and not lGapEol[I] and (IsName(lAfterText) or StartsWithDigit(lAfterText) or (lAfterText = '.')) then
        begin
        aTokens[I].Kind := shKeyword;
        lPendingBody := True;
        lPendingRole := brControl;
        end
      else if InRoutine and lStatementStart and (lValue = 'with') and not lGapEol[I] and IsName(lAfterText) then
        begin
        aTokens[I].Kind := shKeyword;
        lPendingBody := True;
        lPendingRole := brWith;
        end
      else if InRoutine and lStatementStart and (lValue = 'match') and not lGapEol[I]
              and (WordsContain('first all one', lAfterText) or StartsWithDigit(lAfterText)) then
        begin
        aTokens[I].Kind := shKeyword;
        lPendingBody := True;
        lPendingRole := brMatch;
        lPolicy := WordsContain('first all one', lAfterText);
        lPendingCounted := StartsWithDigit(lAfterText);
        end
      else if lPolicy and WordsContain('first all one', lValue) then
        begin
        aTokens[I].Kind := shKeyword;
        lPolicy := False;
        end
      else if (lValue = 'else') and (lAfterText <> '(') and (lElseAllowed or TopRole(brMatch)) then
        begin
        aTokens[I].Kind := shKeyword;
        lPendingBody := True;
        lPendingRole := brControl;
        end
      else if (lValue = 'error') and (lAfterText <> '(') and lStatementStart and (lDepth > 0) and lFrames[lDepth - 1].CountedMatch then
        aTokens[I].Kind := shKeyword
      else if (lHeader <> phNone) and (lRoundDepth > lHeaderRound) and (lAfterText = ':') then
        aTokens[I].Kind := shAttribute
      else if TopRole(brStruct) and (lAfterText = ':') then
        aTokens[I].Kind := shAttribute;
      lStatementStart := False;
      end
    else if (lValue = '{') or (lValue = '[{') then
      begin
      lRole := brBlock;
      if lHeader <> phNone then
        begin
        // An unfinished receiver or parameter list cannot start a body
        if not lExpectName and (lRoundDepth = lHeaderRound) then
          begin
          case lHeader of
            phFunction:
              lRole := brFunction;
            phCode:
              if (lValue = '{') and (lSquareDepth = 0) then
                lRole := brCode;
            phStruct:
              if (lValue = '{') and (lSquareDepth = 0) then
                lRole := brStruct;
          else
            ;
          end;
          lCurrentResult := lHeaderResult;
          end;
        lHeader := phNone;
        lExpectName := False;
        lReceiver := False;
        end
      else if Root and lProgramHeader and not lPendingBody and (lRoundDepth = 0) then
        begin
        // Only a brace at a statement start is the main block
        if lStatementStart or (lBeforeText = ';') then
          lRole := brMain;
        end
      else if lLetBinding and (lValue = '[{') then
        lRole := brLet
      else if lPendingBody then
        lRole := lPendingRole;
      if lValue = '{' then
        Push('}', lRole)
      else
        Push('}]', lRole);
      lTypePosition := False;
      lTypeAngle := 0;
      lTypeRound := 0;
      lPendingBody := False;
      lPolicy := False;
      lElseAllowed := False;
      lAssemblyHead := True;
      lBranchOperand := False;
      lStatementStart := True;
      lLetBinding := False;
      end
    else if (lValue = '}') or (lValue = '}]') then
      begin
      lValidClose := (lDepth > 0) and (lFrames[lDepth - 1].CloseText = lValue);
      lElseAllowed := False;
      if lValidClose then
        begin
        lRole := lFrames[lDepth - 1].Role;
        Dec(lDepth);
        lElseAllowed := lRole = brControl;
        if (lRole = brMain) and (lAfterText = '.') and (lNext = I + 1) and ((lValue = '}') or (lFrames[lDepth].MarkerState > 0)) then
          lOpaqueAt := lNext;
        if lDepth > 0 then
          lCurrentResult := lFrames[lDepth - 1].ResultBearing
        else
          lCurrentResult := False;
        end;
      lTypePosition := False;
      lPendingBody := False;
      lLetBinding := False;
      lStatementStart := True;
      end
    else if lValue = '(' then
      begin
      Inc(lRoundDepth);
      if (lHeader = phFunction) and lExpectName then
        begin
        lExpectName := False;
        lReceiver := True;
        lReceiverDepth := lRoundDepth;
        end;
      if lTypePosition then
        Inc(lTypeRound);
      lStatementStart := False;
      end
    else if lValue = ')' then
      begin
      if lRoundDepth > 0 then
        Dec(lRoundDepth);
      if lReceiver and (lRoundDepth < lReceiverDepth) then
        begin
        lReceiver := False;
        lExpectName := True;
        end;
      if lTypeRound > 0 then
        Dec(lTypeRound)
      else
        lTypePosition := False;
      lStatementStart := False;
      end
    else if lValue = ':' then
      begin
      lTypePosition := (lAfterText <> '[{') and (lAfterText <> '{') and ((lHeader <> phNone) or lLetBinding or IsName(lBeforeText));
      if lTypePosition and (lHeader in [phFunction, phCode]) and (lRoundDepth = lHeaderRound) then
        lHeaderResult := True;
      lTypeRound := 0;
      lTypeAngle := 0;
      end
    else if lValue = '<' then
      begin
      if lTypePosition and (lBeforeText = 'Result') then
        Inc(lTypeAngle);
      end
    else if lValue = '>' then
      begin
      if lTypeAngle > 0 then
        Dec(lTypeAngle);
      end
    else if lValue = ',' then
      begin
      if (lTypeRound = 0) and (lTypeAngle = 0) then
        lTypePosition := False;
      end
    else if (lValue = '=') or (lValue = ':=') then
      begin
      lTypePosition := False;
      lLetBinding := False;
      end
    else if lValue = ';' then
      begin
      lStatementStart := True;
      lTypePosition := False;
      lElseAllowed := False;
      lLetBinding := False;
      lImporting := False;
      lPolicy := False;
      if lHeader <> phNone then
        begin
        lHeader := phNone;
        lExpectName := False;
        end;
      end
    else if lValue = '*' then
      begin
      if lImporting then
        aTokens[I].Kind := shSymbol;
      end
    else if lValue = '[' then
      begin
      Inc(lSquareDepth);
      lStatementStart := False;
      end
    else if lValue = ']' then
      begin
      if lSquareDepth > 0 then
        Dec(lSquareDepth);
      lStatementStart := False;
      end
    else if lValue = '.' then
      lStatementStart := False;
    if (lValue <> '}') and (lValue <> '}]') and (lValue <> 'else') then
      lElseAllowed := False;
    if not Trivia(aTokens[I]) then
      lPrevious := I;
    end;
  if lOpaqueAt >= 0 then
    aTokens[lOpaqueAt].Kind := shSymbol;
end;

initialization
  TBaryeSyntaxHighlighter.Register;
end.
