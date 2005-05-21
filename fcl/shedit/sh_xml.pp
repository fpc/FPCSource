{

    "SHEdit" - Text editor with syntax highlighting
    Copyright (C) 1999-2000 by Sebastian Guenther (sg@freepascal.org)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


// viewer class for XML files

{$MODE objfpc}
{$H+}

unit sh_xml;

interface
uses doc_text, shedit;

type

  TSHXMLEdit = class(TSHTextEdit)
  protected
    procedure DoHighlighting(var flags: Byte; source, dest: PChar); override;
  public
    // Syntax highlighter style indices
    shTag, shTagName, shDefTagName, shArgName, shString, shReference,
      shInvalid, shComment, shCDATA: Integer;
  end;


implementation

uses Strings;

const

  LF_SH_Tag     = LF_SH_Multiline1;
  LF_SH_Comment = LF_SH_Multiline2;
  LF_SH_String1 = LF_SH_Multiline3;     // Single quotation mark
  LF_SH_String2 = LF_SH_Multiline4;     // Double quotation mark
  LF_SH_CDATA   = LF_SH_Multiline5;


procedure TSHXMLEdit.DoHighlighting(var flags: Byte; source, dest: PChar);
var
  dp: Integer;    {Destination postion - current offset in dest}
  LastSHPos: Integer; {Position of last highlighting character, or 0}

  procedure AddSH(sh: Byte);
  begin
    if (LastSHPos > 0) and (dp = LastSHPos + 1) then Dec(dp, 2);
    dest[dp] := LF_Escape; Inc(dp);
    LastSHPos := dp;
    dest[dp] := Chr(sh); Inc(dp);
  end;

  procedure PutChar;
  begin
    dest[dp] := source[0]; Inc(dp); Inc(source);
  end;

  procedure ProcessComment;
  begin
    flags := flags or LF_SH_Comment;
    AddSH(shComment);
    while source[0] <> #0 do begin
      if (source[0] = '-') and (source[1] = '-') and (source[2] = '>') then begin
        PutChar; PutChar; PutChar;
        flags := flags and not LF_SH_Comment;
        AddSH(shDefault);
        break;
      end;
      PutChar;
    end;
  end;

  procedure ProcessReference;
  begin
    AddSH(shReference);
    while source[0] <> #0 do begin
      if source[0] = ';' then begin
        PutChar;
        AddSH(shDefault);
        break;
      end else if (source[0] = '''') or (source[0] = '"') then begin
        AddSH(shString);
        break;
      end else
        PutChar;
    end;
  end;

  procedure ProcessString(EndChar: Char);
  begin
    while source[0] <> #0 do begin
      if source[0] = EndChar then begin
        PutChar;
        AddSH(shDefault);
        flags := flags and not (LF_SH_String1 or LF_SH_String2);
        break;
      end else if source[0] = '&' then
        ProcessReference
      else
        PutChar;
    end;
  end;

  procedure ProcessTagContd;
  var
    c: Char;
  begin
    while source[0] <> #0 do begin
      if (source[0] in ['/', '?']) and (source[1] = '>') then begin
        AddSH(shTag);
        PutChar;
        PutChar;
        AddSH(shDefault);
        flags := flags and not LF_SH_Tag;
        break;
      end else if (source[0] = '>') then begin
        AddSH(shTag);
        PutChar;
        AddSH(shDefault);
        flags := flags and not LF_SH_Tag;
        break;
      end else if (source[0] = '''') or (source[0] = '"') then begin
        c := source[0];
        if source[0] = '''' then
          flags := flags or LF_SH_String1
        else
          flags := flags or LF_SH_String2;
        AddSH(shString);
        PutChar;
        ProcessString(c);
      end else if source[0] in [#9, ' ', '=', '(', ')', '+', '*', '?', ','] then begin
        AddSH(shDefault);
        PutChar;
      end else begin
        AddSH(shArgName);
        PutChar;
      end;
    end;
  end;

  procedure ProcessTag;
  begin
    flags := flags or LF_SH_Tag;
    AddSH(shTag);
    PutChar;
    if source[0] = '/' then PutChar;
    if (source[0] = '!') or (source[0] = '?') then
      AddSH(shDefTagName)
    else
      AddSH(shTagName);
    while not (source[0] in [#0, ' ', '/', '>']) do
      PutChar;
    AddSH(shDefault);
    ProcessTagContd;
  end;

  procedure ProcessCDATAContd;
  begin
    AddSH(shCDATA);
    while source[0] <> #0 do begin
      if (source[0] = ']') and (source[1] = ']') and
         (source[2] = '>') then begin
        AddSH(shTag);
        PutChar; PutChar; PutChar;
        AddSH(shDefault);
        flags := flags and not LF_SH_CDATA;
        break;
      end;
      PutChar;
    end;
  end;

  procedure ProcessCDATA;
  var
    i: Integer;
  begin
    flags := flags or LF_SH_CDATA;
    AddSH(shTag);
    for i := 1 to 9 do PutChar;
    ProcessCDATAContd;
  end;

begin
  dp := 0;
  LastSHPos := 0;

  if (flags and LF_SH_Comment) <> 0 then begin
    AddSH(shComment);
    ProcessComment;
  end;

  if (flags and LF_SH_String1) <> 0 then begin
    AddSH(shString);
    ProcessString('''');
  end;
  if (flags and LF_SH_String2) <> 0 then begin
    AddSH(shString);
    ProcessString('"');
  end;

  if (flags and LF_SH_Tag) <> 0 then
    ProcessTagContd;

  if (flags and LF_SH_CDATA) <> 0 then
    ProcessCDATAContd;


  while source[0] <> #0 do begin

    case source[0] of
      '<':
          if (source[1] = '!') and (source[2] = '-') and (source[3] = '-') then
            ProcessComment
          else if (source[1] = '!') and (source[2] = '[') and (source[3] = 'C')
            and (source[4] = 'D') and (source[5] = 'A') and (source[6] = 'T')
            and (source[7] = 'A') and (source[8] = '[') then
            ProcessCDATA
          else
            ProcessTag;
      '&': ProcessReference;
      else
        PutChar;
    end;
  end;

  dest[dp] := #0;
end;


end.
