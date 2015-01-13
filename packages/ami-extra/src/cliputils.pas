{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    Clipboard helper functions for Amiga-likes

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$MODE OBJFPC}
{$H+}
unit cliputils;

interface

function GetTextFromClip(ClipUnit: Byte): string;
function PutTextToClip(ClipUnit: Byte; Text: string): Boolean;

implementation

uses
  exec, clipboard, iffparse;

const
  ID_FTXT = 1179932756;
  ID_CHRS = 1128813139;

function GetTextFromClip(ClipUnit: Byte): string;
var
  Iff: PIffHandle;
  Error: LongInt;
  Cn: PContextNode;
  Buf: PChar;
  Len: Integer;
  Cu: LongInt;
begin
  Result := '';
  Cu := ClipUnit;
  Iff := AllocIff;
  if Assigned(Iff) then
  begin
    InitIffAsClip(iff);
    Iff^.iff_Stream := LongWord(OpenClipboard(Cu));
    if Iff^.iff_Stream<>0 then
    begin
      if OpenIff(Iff, IFFF_READ) = 0 then
      begin
        if StopChunk(iff, ID_FTXT, ID_CHRS) = 0 then
        begin
          while True do
          begin
            Error := ParseIff(iff, IFFPARSE_SCAN);
            if (Error <> 0) and (Error <> IFFERR_EOC) then
              Break;
            Cn := CurrentChunk(Iff);
            if not Assigned(Cn) then
            begin
              Continue;
            end;
            Len := Cn^.cn_Size;
            if (Cn^.cn_Type = ID_FTXT) and (Cn^.cn_ID = ID_CHRS) and (Len > 0) then
            begin
              GetMem(Buf, Len + 1);
              FillChar(Buf^, Len + 1, #0);
              try
                ReadChunkBytes(Iff, Buf, Len);
                Result := Result + string(Buf);
              finally
                FreeMem(Buf);
              end;
            end;
          end;
        end;
        CloseIff(Iff);
      end;
      CloseClipboard(PClipBoardHandle(iff^.iff_Stream));
    end;
    FreeIFF(Iff);
  end;
end;

function PutTextToClip(ClipUnit: Byte; Text: string): Boolean;
var
  Iff: PIffHandle;
  TText: string;
  Len: Integer;
begin
  Result := False;
  Iff := AllocIff;
  if Assigned(Iff) then
  begin
    InitIffAsClip(iff);
    Iff^.iff_Stream := LongWord(OpenClipboard(ClipUnit));
    if Iff^.iff_Stream <> 0 then
    begin
      if OpenIff(Iff, IFFF_WRITE) = 0 then
      begin
        if PushChunk(iff, ID_FTXT, ID_FORM, IFFSIZE_UNKNOWN) = 0 then
        begin
          if PushChunk(iff, 0, ID_CHRS, IFFSIZE_UNKNOWN) = 0 then
          begin
            Len := Length(Text);
            TText := Text + #0;
            Result := WriteChunkBytes(iff, @(TText[1]), Len) = len;
            PopChunk(iff);
          end;
          PopChunk(iff);
        end;
        CloseIff(iff);
      end;
      CloseClipboard(PClipBoardHandle(iff^.iff_Stream));
    end;
    FreeIFF(Iff);
  end;
end;

begin
{$IF DEFINED(MORPHOS)}
  InitIFFParseLibrary;
{$ENDIF}
end.
