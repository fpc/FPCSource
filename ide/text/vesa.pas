{
    $Id$
    This file is part of the PinGUI - Platform Independent GUI Project
    Copyright (c) 1999 by Berczi Gabor

    VESA support routines

    See the file COPYING.GUI, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit VESA;

interface

uses
  Dos,
  Objects,Strings,WUtils;

const
     { Video Mode Attributes mask constants }
     vesa_vma_CanBeSetInCurrentConfig = $0001;
     vesa_vma_OptionalBlockPresent    = $0002;
     vesa_vma_BIOSSupport             = $0004;
     vesa_vma_ColorMode               = $0008; { else mono }
     vesa_vma_GraphicsMode            = $0010; { else text }
     { -- VBE 2.0 --- }
     vesa_vma_VGACompatibleMode       = $0020;
     vesa_vma_VGACompWindowedAvail    = $0040;
     vesa_vma_LinearFrameBufferAvail  = $0080;

     { Windows Attributes mask constants }
     vesa_wa_Present                  = $0001;
     vesa_wa_Readable                 = $0002;
     vesa_wa_Writeable                = $0004;

     { Memory Model value constants }
     vesa_mm_Text                     = $0000;
     vesa_mm_CGAGraphics              = $0001;
     vesa_mm_HerculesGraphics         = $0002;
     vesa_mm_4planePlanar             = $0003;
     vesa_mm_PackedPixel              = $0004;
     vesa_mm_NonChain4_256color       = $0005;
     vesa_mm_DirectColor              = $0006;
     vesa_mm_YUV                      = $0007;

     { Memory Window value constants }
     vesa_mw_WindowA                  = $0000;
     vesa_mw_WindowB                  = $0001;

type
     {$ifdef FPC}tregisters=registers;{$endif}
     {$ifdef TP}tregisters=registers;{$endif}

     PtrRec16 = record
       Ofs,Seg: word;
     end;

     TVESAInfoBlock = record
       Signature    : longint; {  'VESA' }
       Version      : word;
       OEMString    : PString;
       Capabilities : longint;
       VideoModeList: PWordArray;
       TotalMemory  : word; { in 64KB blocks }
       Fill         : array[1..236] of byte;
       VBE2Fill     : array[1..256] of byte;
     end;

     TVESAModeInfoBlock = record
       Attributes      : word;
       WinAAttrs       : byte;
       WinBAttrs       : byte;
       Granularity     : word;
       Size            : word;
       ASegment        : word;
       BSegment        : word;
       FuncPtr         : pointer;
       BytesPerLine    : word;
     { optional }
       XResolution     : word;
       YResolution     : word;
       XCharSize       : byte;
       YCharSize       : byte;
       NumberOfPlanes  : byte;
       BitsPerPixel    : byte;
       NumberOfBanks   : byte;
       MemoryModel     : byte;
       BankSize        : byte;
       NumberOfImagePages: byte;
       Reserved        : byte;
     { direct color fields }
       RedMaskSize     : byte;
       RedFieldPosition: byte;
       GreenMaskSize   : byte;
       GreenFieldPosition: byte;
       BlueMaskSize    : byte;
       BlueFieldPosition: byte;
       ReservedMaskSize: byte;
       ReservedPosition: byte;
       DirectColorModeInfo: byte;
      { --- VBE 2.0 optional --- }
       LinearFrameAddr : longint;
       OffScreenAddr   : longint;
       OffScreenSize   : word;
       Reserved2       : array[1..216-(4+4+2)] of byte;
     end;

     TVESAModeList = record
       Count        : word;
       Modes        : array[1..256] of word;
     end;

function VESAInit: boolean;
function VESAGetInfo(var B: TVESAInfoBlock): boolean;
function VESAGetModeInfo(Mode: word; var B: TVESAModeInfoBlock): boolean;
function VESAGetModeList(var B: TVESAModeList): boolean;
function VESASearchMode(XRes,YRes,BPX: word; LFB: boolean; var Mode: word; var ModeInfo: TVESAModeInfoBlock): boolean;
function VESAGetOemString: string;
function VESASetMode(Mode: word): boolean;
function VESAGetMode(var Mode: word): boolean;
function VESASelectMemoryWindow(Window: byte; Position: word): boolean;
function VESAReturnMemoryWindow(Window: byte; var Position: word): boolean;

implementation

uses pmode;

function VESAGetInfo(var B: TVESAInfoBlock): boolean;
var r: registers;
    OK: boolean;
    M: MemPtr;
begin
  StrToMem('VBE2',B.Signature);
  GetDosMem(M,SizeOf(B));
  M.MoveDataTo(B,sizeof(B));
  r.ah:=$4f; r.al:=0;
  r.es:=M.DosSeg; r.di:=M.DosOfs;
  realintr($10,r);
  M.MoveDataFrom(sizeof(B),B);
  FreeDosMem(M);
  OK:=(r.ax=$004f){ and (MemToStr(B.Signature,4)='VESA')};
  VESAGetInfo:=OK;
end;

function VESAGetModeList(var B: TVESAModeList): boolean;
var OK: boolean;
    VI: TVESAInfoBlock;
begin
  FillChar(B,SizeOf(B),0);
  OK:=VESAGetInfo(VI);
  if OK then
  begin
    OK:=MoveDosToPM(VI.VideoModeList,@B.Modes,sizeof(B.Modes));
    if OK then
      while (B.Modes[B.Count+1]<>$ffff) and (B.Count<High(B.Modes)) do
            Inc(B.Count);
  end;
  VESAGetModeList:=OK;
end;

function VESASearchMode(XRes,YRes,BPX: word; LFB: boolean; var Mode: word; var ModeInfo: TVESAModeInfoBlock): boolean;
var B: TVESAModeList;
    OK: boolean;
    I: integer;
    MI: TVESAModeInfoBlock;
begin
  OK:=VESAGetModeList(B);
  I:=1; Mode:=0;
  repeat
    OK:=VESAGetModeInfo(B.Modes[I],MI);
    if OK and (MI.XResolution=XRes) and (MI.YResolution=YRes) and
       (MI.BitsPerPixel=BPX) and
       ((LFB=false) or ((MI.Attributes and vesa_vma_LinearFrameBufferAvail)<>0)) then
      begin Mode:=B.Modes[I]; ModeInfo:=MI; end;
    Inc(I);
  until (OK=false) or (I>=B.Count) or (Mode<>0);
  OK:=Mode<>0;
  VESASearchMode:=OK;
end;

function VESAGetOemString: string;
var OK: boolean;
    VI: TVESAInfoBlock;
    S: array[0..256] of char;
begin
  FillChar(S,SizeOf(S),0);
  OK:=VESAGetInfo(VI);
  if OK then
    OK:=MoveDosToPM(VI.OemString,@S,sizeof(S));
  VESAGetOemString:=StrPas(@S);
end;

function VESAGetModeInfo(Mode: word; var B: TVESAModeInfoBlock): boolean;
var r : registers;
    M : MemPtr;
    OK: boolean;
begin
  r.ah:=$4f; r.al:=$01; r.cx:=Mode;
  GetDosMem(M,sizeof(B));
  r.es:=M.DosSeg; r.di:=M.DosOfs; {r.ds:=r.es;}
  realintr($10,r);
  M.MoveDataFrom(sizeof(B),B);
  FreeDosMem(M);
  OK:=(r.ax=$004f);
  VESAGetModeInfo:=OK;
end;

function VESASetMode(Mode: word): boolean;
var r: registers;
    OK: boolean;
begin
  r.ah:=$4f; r.al:=$02; r.bx:=Mode;
  dos.intr($10,r);
  OK:=(r.ax=$004f);
  VESASetMode:=OK;
end;

function VESAGetMode(var Mode: word): boolean;
var r : registers;
    OK: boolean;
begin
  r.ah:=$4f; r.al:=$03;
  dos.intr($10,r);
  OK:=(r.ax=$004f);
  if OK then Mode:=r.bx;
  VESAGetMode:=OK;
end;

function VESASelectMemoryWindow(Window: byte; Position: word): boolean;
var r : registers;
    OK : boolean;
begin
  r.ah:=$4f; r.al:=$05; r.bh:=0; r.bl:=Window; r.dx:=Position;
  dos.intr($10,r);
  OK:=(r.ax=$004f);
  VESASelectMemoryWindow:=OK;
end;

function VESAReturnMemoryWindow(Window: byte; var Position: word): boolean;
var r  : registers;
    OK : boolean;
begin
  r.ah:=$4f; r.al:=$05; r.bh:=1; r.bl:=Window;
  dos.intr($10,r);
  OK:=(r.ax=$004f);
  if OK then Position:=r.dx;
  VESAReturnMemoryWindow:=OK;
end;

function VESAInit: boolean;
var OK: boolean;
    VI: TVESAInfoBlock;
begin
  OK:=VESAGetInfo(VI);
  VESAInit:=OK;
end;

BEGIN
END.
{
  $Log$
  Revision 1.1  2000-07-13 09:48:36  michael
  + Initial import

  Revision 1.8  2000/06/22 09:07:13  pierre
   * Gabor changes: see fixes.txt

  Revision 1.7  2000/03/21 23:22:37  pierre
   Gabor fixes to avoid unused vars

  Revision 1.6  2000/01/03 11:38:35  michael
  Changes from Gabor

  Revision 1.4  1999/04/07 21:55:58  peter
    + object support for browser
    * html help fixes
    * more desktop saving things
    * NODEBUG directive to exclude debugger

  Revision 1.3  1999/04/01 10:04:18  pierre
   * uses typo errror fixed

  Revision 1.2  1999/03/26 19:09:44  peter
    * fixed for go32v2

  Revision 1.1  1999/03/23 15:11:39  peter
    * desktop saving things
    * vesa mode
    * preferences dialog

}
