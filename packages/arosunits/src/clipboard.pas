{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2014 by Free Pascal development team

    clipboard functions

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit clipboard;

interface

{$mode objfpc}{$H+}

uses exec;

const
    CBD_POST            = CMD_NONSTD + 0;
    CBD_CURRENTREADID   = CMD_NONSTD + 1;
    CBD_CURRENTWRITEID  = CMD_NONSTD + 2;
    CBD_CHANGEHOOK      = CMD_NONSTD + 3;

    CBERR_OBSOLETEID    = 1;

type

    PClipboardUnitPartial = ^TClipboardUnitPartial;
    TClipboardUnitPartial = record
        cu_Node         : TNode;         { list of units }
        cu_UnitNum      : LongWord;      { unit number for this unit }
    { the remaining unit data is private to the device }
    end;


    PIOClipReq = ^TIOClipReq;
    TIOClipReq = record
        io_Message      : TMessage;
        io_Device       : PDevice;      { device node pointer   }
        io_Unit         : PClipboardUnitPartial;      { unit (driver private) }
        io_Command      : Word;        { device command        }
        io_Flags        : Byte;         { including QUICK and SATISFY }
        io_Error        : ShortInt;     { error or warning num  }
        io_Actual       : LongWord;        { number of bytes transferred }
        io_Length       : LongWord;        { number of bytes requested }
        io_Data         : STRPTR;       { either clip stream or post port }
        io_Offset       : LongWord;        { offset in clip stream }
        io_ClipID       : LongInt;      { ordinal clip identifier }
    end;

const
    PRIMARY_CLIP        = 0;    { primary clip unit }

type

    PSatisfyMsg = ^TSatisfyMsg;
    TSatisfyMsg = record
        sm_Msg  : TMessage;      { the length will be 6 }
        sm_Unit : Word;          { which clip unit this is }
        sm_ClipID : LongInt;     { the clip identifier of the post }
    end;

   PClipHookMsg = ^TClipHookMsg;
   TClipHookMsg = record
    chm_Type   : LongWord;          { zero for this structure format }
    chm_ChangeCmd,               { command that caused this hook invocation: }
                                 { either CMD_UPDATE OR CBD_POST }
    chm_ClipID : LongInt;        { the clip identifier of the new data }
   END;

function GetTextFromClip(ClipUnit: Byte): string;
function PutTextToClip(ClipUnit: Byte; Text: string): Boolean;

implementation


uses
  iffparse;

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
    Iff^.iff_Stream := LongWord(OpenClipboard(Cu));
    if Iff^.iff_Stream<>0 then
    begin
      InitIffAsClip(iff);
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
    Iff^.iff_Stream := LongWord(OpenClipboard(ClipUnit));
    if Iff^.iff_Stream <> 0 then
    begin
      InitIffAsClip(iff);
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

end.
