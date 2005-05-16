{
    $Id: sh_pas.pp,v 1.4 2005/02/14 17:13:17 peter Exp $

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// Syntax highlighting class for Pascal sources

{$MODE objfpc}
{$H+}

{$IFDEF Debug}
{$ASSERTIONS On}
{$ENDIF}

unit sh_pas;

interface
uses doc_text, shedit;

type

  TSHPasEdit = class(TSHTextEdit)
  protected
    procedure DoHighlighting(var flags: Byte; source, dest: PChar); override;
    procedure KeyReturn; override;
  public
    // Syntax highlighter style indices
    shInvalid, shSymbol, shKeyword, shComment, shDirective, shNumbers,
      shCharacters, shStrings, shAssembler: Integer;
  end;


implementation

uses Strings;

const

  LF_SH_Comment1 = LF_SH_Multiline1;    { Normal braced Comments}
  LF_SH_Comment2 = LF_SH_Multiline2;    { (* *) Comments}
  LF_SH_Asm      = LF_SH_Multiline3;

  MaxKeywordLength = 15;
  MaxKeyword = 60;

  KeywordTable: array[0..MaxKeyword] of PChar =
    ('AND', 'ARRAY', 'ASM', 'ASSEMBLER',
     'BEGIN', 'BREAK',
     'CASE', 'CONST', 'CONSTRUCTOR', 'CLASS',
     'DEFAULT', 'DESTRUCTOR', 'DIV', 'DO', 'DOWNTO',
     'ELSE', 'END', 'EXCEPT', 'EXIT',
     'FINALIZATION', 'FINALLY', 'FOR', 'FUNCTION',
     'GOTO',
     'IF', 'IMPLEMENTATION', 'IN', 'INHERITED', 'INITIALIZATION', 'INTERFACE',
     'NIL', 'NOT',
     'OBJECT', 'OF', 'ON', 'OR', 'OVERRIDE',
     'PACKED', 'PRIVATE', 'PROCEDURE', 'PROGRAM', 'PROPERTY', 'PROTECTED',
       'PUBLIC', 'PUBLISHED',
     'RAISE', 'RECORD', 'REPEAT', 'RESOURCESTRING',
     'SET',
     'THEN', 'TRY', 'TYPE',
     'UNIT', 'UNTIL', 'USES',
     'VAR', 'VIRTUAL',
     'WHILE', 'WITH',
     'XOR');

  KeywordAsmIndex = 2;


procedure TSHPasEdit.KeyReturn;
var
  s: String;
  i, count: Integer;
begin
  // Get # of spaces in front of previous line
  s := FDoc.LineText[CursorY - 1];
  i := 1; count := 0;
  while (i <= Length(s)) and (s[i] = ' ') do begin
    Inc(i);
    Inc(count);
  end;

  FDoc.LineText[CursorY] := Copy(s, 1, count) + FDoc.LineText[CursorY];
  Inc(FCursorX, count);
  AddUndoInfo(TUndoEdit.Create(count), True);
  ChangeInLine(CursorY);
end;


procedure TSHPasEdit.DoHighlighting(var flags: Byte; source, dest: PChar);
var
  dp: Integer;          // Destination position - current offset in dest
  LastSHPos: Integer;   // Position of last highlighting character, or 0

  procedure AddSH(sh: Byte);
  begin
    ASSERT(sh > 0);
    if (LastSHPos > 0) and (dp = LastSHPos + 1) then Dec(dp, 2);
    dest[dp] := LF_Escape; Inc(dp);
    LastSHPos := dp;
    dest[dp] := Chr(sh); Inc(dp);
  end;

  procedure PutChar;
  begin
    dest[dp] := source[0]; Inc(dp); Inc(source);
  end;

  procedure ProcessComment1;
  begin
    while source[0] <> #0 do begin
      if source[0] = '}' then begin
        PutChar;
        flags := flags and not LF_SH_Comment1;
        AddSH(shDefault);
        break;
      end;
      PutChar;
    end;
  end;

  procedure ProcessComment2;
  begin
    while source[0] <> #0 do begin
      if (source[0] = '*') and (source[1] = ')') then begin
        PutChar; PutChar;
        flags := flags and not LF_SH_Comment2;
        AddSH(shDefault);
        break;
      end;
      PutChar;
    end;
  end;


  { Checks if we are at the beginning of a comment (or directive) and processes
    all types of comments and directives, or returns False }

  function CheckForComment: Boolean;
  begin
    Result := True;
    if source[0] = '{' then begin
      if source[1] = '$' then
        AddSH(shDirective)
      else
        AddSH(shComment);
      PutChar;
      flags := flags or LF_SH_Comment1;
      ProcessComment1;
    end else if (source[0] = '(') and (source[1] = '*') then begin
      AddSH(shComment);
      PutChar; PutChar;
      flags := flags or LF_SH_Comment2;
      ProcessComment2;
    end else if (source[0] = '/') and (source[1] = '/') then begin
      AddSH(shComment);
      repeat PutChar until source[0] = #0;
      AddSH(shDefault);
    end else
      Result := False;
  end;

  procedure ProcessAsm;
  var
    LastChar: Char;
  begin
    LastChar := ' ';
    while source[0] <> #0 do begin
      if (LastChar in [' ', #9, #10, #13]) and
        (UpCase(source[0]) = 'E') and (UpCase(source[1]) = 'N') and
        (UpCase(source[2]) = 'D') then begin
        AddSH(shKeyword);
        PutChar; PutChar; PutChar;
        flags := flags and not LF_SH_Asm;
        AddSH(shDefault);
        break;
      end else
        if CheckForComment then LastChar := ' '
        else begin
          LastChar := source[0];
          PutChar;
        end;
    end;
  end;

  procedure ProcessSymbol;
  begin
    AddSH(shSymbol);
    if (source[0] = ':') and (source[1] = '=') then
      PutChar;
    PutChar;
    AddSH(shDefault);
  end;

  function CheckForKeyword: Boolean;
  var
    keyword, ukeyword: array[0..MaxKeywordLength] of Char;
    i, j: Integer;
  begin
    i := 0;
    while (source[i] <> #0) and (i < MaxKeywordLength) and
      (source[i] in ['0'..'9', 'A'..'Z', 'a'..'z']) do begin
      keyword[i] := source[i];
      ukeyword[i] := UpCase(source[i]);
      Inc(i);
    end;
    keyword[i] := #0; ukeyword[i] := #0;
    Result := False;
    if i < MaxKeywordLength then
      for j := 0 to MaxKeyword do
        if StrIComp(KeywordTable[j], ukeyword) = 0 then begin
          Result := True; break;
        end;
    if not Result then exit;
    Inc(source, i);
    AddSH(shKeyword);
    StrCopy(dest + dp, keyword);
    Inc(dp, i);
    if j <> KeywordAsmIndex then
      AddSH(shDefault)
    else begin
      AddSH(shAssembler);
      flags := flags or LF_SH_Asm;
      ProcessAsm;
    end;
  end;

var
  StringLength: Integer;
begin
  dp := 0;
  LastSHPos := 0;

  if (flags and LF_SH_Comment1) <> 0 then begin
    AddSH(shComment);
    ProcessComment1;
  end;

  if (flags and LF_SH_Comment2) <> 0 then begin
    AddSH(shComment);
    ProcessComment2;
  end;

  if (flags and LF_SH_Asm) <> 0 then begin
    AddSH(shAssembler);
    ProcessAsm;
  end;

  while source[0] <> #0 do begin

    if CheckForComment then continue;

    case source[0] of
      ',', ';', ':', '.', '(', ')', '[', ']', '<', '>', '=',
      '*', '/', '+', '-', '^', '&', '@': ProcessSymbol;
      '#': begin
          AddSH(shCharacters);
          PutChar;
          if source[0] = '$' then PutChar;
          while (source[0] >= '0') and (source[0] <= '9') do PutChar;
          AddSH(shDefault);
        end;
      '$': begin
          AddSH(shNumbers);
          PutChar;
          while source[0] in ['0'..'9', 'A'..'F', 'a'..'f'] do PutChar;
          AddSH(shDefault);
        end;
      '0'..'9': begin
          AddSH(shNumbers);
          PutChar;
          while (source[0] >= '0') and (source[0] <= '9') do PutChar;
          AddSH(shDefault);
        end;
      '''': begin
          AddSH(shStrings);
          PutChar;
          StringLength := 0;
          while source[0] <> #0  do begin
            if source[0] = '''' then
              if source[1] = '''' then PutChar
              else begin
                PutChar; break;
              end;
            Inc(StringLength);
            PutChar;
          end;
          if StringLength = 1 then
            dest[LastSHPos] := Chr(shCharacters);
          if (source[0] = #0) and (dest[dp - 1] <> '''') then
            dest[LastSHPos] := Chr(shInvalid);
          AddSH(shDefault);
        end;
      '_', 'A'..'Z', 'a'..'z': begin
          if not CheckForKeyword then
            repeat
              PutChar
            until not (source[0] in ['0'..'9', '_', 'A'..'Z', 'a'..'z']);
        end;
      ' ': PutChar;
      else begin
        AddSH(shInvalid);
        PutChar;  // = found an invalid char!
        AddSH(shDefault);
      end;
    end;
  end;

  dest[dp] := #0;
end;


end.


{
  $Log: sh_pas.pp,v $
  Revision 1.4  2005/02/14 17:13:17  peter
    * truncate log

}
