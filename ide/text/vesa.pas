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
  {$ifdef TP}
    {$ifdef DPMI}
    WinDos,WinAPI,
    {$endif}
  {$endif}
  {$ifdef FPC}
    {$ifdef GO32V2}
    Go32,Video,
    {$endif}
  {$endif}
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

function MemToStr(var B; Count: byte): string;

implementation

{$IFDEF DPMI}
const
    DPMI_INTR      = $31;

type
    TDPMIRegisters = record     { DPMI call structure }
      EDI     : LongInt;
      ESI     : LongInt;
      EBP     : LongInt;
      Reserved: LongInt;
      EBX     : LongInt;
      EDX     : LongInt;
      ECX     : LongInt;
      EAX     : LongInt;
      Flags   : Word;
      ES      : Word;
      DS      : Word;
      FS      : Word;
      GS      : Word;
      IP      : Word;
      CS      : Word;
      SP      : Word;
      SS      : Word;
    end;

  MemPtr = record
  {$ifdef TP}
    Selector: Word;  {Protected mode}
    Segment : Word;  {Real mode}
  {$endif}
  {$ifdef FPC}
    Selector: Word;  {Real mode}
    Segment : Word;  {Protected mode}
  {$endif}
  end;

  Function GetMem(var Mem : MemPtr; Size : Word): Boolean;
    begin
      if (Size > 0) then
      begin
      {$ifdef TP}
        LongInt(Mem) := GlobalDOSAlloc(Size);
      {$endif}
      {$ifdef FPC}
        longint(Mem) := global_dos_alloc(Size);
        if int31error<>0 then longint(Mem):=0;
      {$endif}
        GetMem := (LongInt(Mem) <> 0);
      end

      else
      begin
        LongInt(Mem) := 0;
        GetMem := True;
      end;
    end;

  Procedure FreeMem(Mem : MemPtr; Size : Word);
    begin
      {$ifdef TP}
      if (Size > 0) then
        GlobalDOSFree(Mem.Selector);
      {$endif}
      {$ifdef FPC}
      if (Size > 0) then
        global_dos_free(Mem.Selector);
      {$endif}
    end;

  Function MakePtr(Mem : MemPtr): Pointer;
    begin
      MakePtr := Ptr(Mem.Selector, 0);
    end;

  {$ifdef TP}
  var
    DPMIRegs: TDPMIRegisters;

  procedure realintr(IntNo: byte; var r: tregisters);
  var Regs: TRegisters;
  begin
    FillChar(DPMIRegs, SizeOf(TDPMIRegisters), 0);
    DPMIRegs.EAX := r.ax;
    DPMIRegs.EBX := r.bx;
    DPMIRegs.ECX := r.cx;
    DPMIRegs.EDX := r.dx;
    DPMIRegs.EDI := r.di;
    DPMIRegs.ESI := r.si;
    DPMIRegs.EBP := r.bp;
    DPMIRegs.DS := r.ds;
    DPMIRegs.ES := r.es;
    DPMIRegs.Flags := r.flags;
    Regs.AX := $0300;
    Regs.BL := IntNo;
    Regs.BH := 0;
    Regs.CX := 0;
    Regs.ES := Seg(DPMIRegs);
    Regs.DI := Ofs(DPMIRegs);
    Dos.Intr(DPMI_INTR, Regs);
    r.ax := DPMIRegs.EAX;
    r.bx := DPMIRegs.EBX;
    r.cx := DPMIRegs.ECX;
    r.dx := DPMIRegs.EDX;
    r.di := DPMIRegs.EDI;
    r.si := DPMIRegs.ESI;
    r.bp := DPMIRegs.EBP;
    r.ds := DPMIRegs.DS;
    r.es := DPMIRegs.ES;
    r.Flags := DPMIRegs.Flags;
  end;
  {$endif}
{$ENDIF}

function MemToStr(var B; Count: byte): string;
var S: string;
begin
  S[0]:=chr(Count);
  if Count>0 then Move(B,S[1],Count);
  MemToStr:=S;
end;

procedure StrToMem(S: string; var B);
begin
  if length(S)>0 then Move(S[1],B,length(S));
end;

function VESAGetInfo(var B: TVESAInfoBlock): boolean;
{$IFNDEF DPMI}
var r : registers;
{$ELSE}
var r : tregisters;
    pB : MemPtr;
{$ENDIF}
    OK: boolean;
begin
  StrToMem('VBE2',B.Signature);
  r.ah:=$4f; r.al:=0;
{$IFNDEF DPMI}
  r.es:=seg(B); r.di:=ofs(B);
  intr($10,r);
{$ELSE}
  GetMem(pB, SizeOf(B));
  {$ifdef TP}
  Move(B,MakePtr(pB)^,SizeOf(B));
  {$endif}
  {$ifdef FPC}
  dosmemput(pB.Segment,0,B,SizeOf(B));
  {$endif}
  r.es:=pB.Segment; r.di:=0; r.ds:=r.es;
  realintr($10,r);
{$ENDIF}
{$IFDEF DPMI}
  {$ifdef TP}
  Move(MakePtr(pB)^,B,SizeOf(B));
  {$endif}
  {$ifdef FPC}
  dosmemget(pB.Segment,0,B,SizeOf(B));
  {$endif}
  FreeMem(pB, SizeOf(B));
{$ENDIF}
  OK:=(r.ax=$004f){ and (MemToStr(B.Signature,4)='VESA')};
  VESAGetInfo:=OK;
end;

function VESAGetModeList(var B: TVESAModeList): boolean;
var OK: boolean;
    VI: TVESAInfoBlock;
    Sel: word;
begin
  FillChar(B,SizeOf(B),0);
  OK:=VESAGetInfo(VI);
  if OK then
  begin
    {$ifdef TP}
    {$ifdef DPMI}
    Sel:=AllocSelector(0);
    OK:=Sel<>0;
    if OK then
    begin
      SetSelectorBase(Sel,(longint(VI.VideoModeList) shr 16)*16+longint(VI.VideoModeList) and $ffff);
      SetSelectorLimit(Sel,SizeOf(B.Modes));
      Move(ptr(Sel,0)^,B.Modes,SizeOf(B.Modes));
      FreeSelector(Sel);
    end;
    {$endif}
    {$endif}
    {$ifdef FPC}
      with VI do
      dosmemget(PtrRec(VideoModeList).Seg,PtrRec(VideoModeList).Ofs,B.Modes,SizeOf(B.Modes));
    {$endif}
    if OK then
    while (B.Modes[B.Count+1]<>$ffff) and (B.Count<255) do
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
    Sel: word;
    S: array[0..256] of char;
begin
  FillChar(S,SizeOf(S),0);
  OK:=VESAGetInfo(VI);
  {$IFDEF DPMI}
  if OK then
  begin
    {$ifdef TP}
    Sel:=AllocSelector(0);
    OK:=Sel<>0;
    if OK then
    begin
      SetSelectorBase(Sel,longint(PtrRec16(VI.OemString).Seg)*16+PtrRec16(VI.OemString).Ofs);
      SetSelectorLimit(Sel,SizeOf(S));
      Move(ptr(Sel,0)^,S,SizeOf(S));
      FreeSelector(Sel);
    end;
    {$endif}
    {$ifdef FPC}
    dosmemget(PtrRec16(VI.OemString).Seg,PtrRec16(VI.OemString).Ofs,S,SizeOf(S));
    {$endif}
  end;
  {$ELSE}
    Move(VI.OemString^,S,SizeOf(S));
  {$ENDIF}
  VESAGetOemString:=StrPas(@S);
end;

function VESAGetModeInfo(Mode: word; var B: TVESAModeInfoBlock): boolean;
{$IFNDEF DPMI}
var r : registers;
{$ELSE}
var r : tregisters;
{$ENDIF}
    OK: boolean;
{$ifdef DPMI}
    pB: MemPtr;
{$endif}
begin
  r.ah:=$4f; r.al:=$01; r.cx:=Mode;
{$IFDEF DPMI}
  GetMem(pB, SizeOf(B));
  {$ifdef TP}
  Move(B,MakePtr(pB)^,SizeOf(B));
  {$endif}
  {$ifdef FPC}
  dosmemput(pB.Segment,0,B,SizeOf(B));
  {$endif}
  r.es:=pB.Segment; r.di:=0; {r.ds:=r.es;}
  realintr($10,r);
{$ELSE}
  r.es:=seg(B); r.di:=ofs(B);
  intr($10,r);
{$ENDIF}
{$IFDEF DPMI}
  {$ifdef TP}
  Move(MakePtr(pB)^,B,SizeOf(B));
  {$endif}
  {$ifdef FPC}
  dosmemget(pB.Segment,0,B,SizeOf(B));
  {$endif}
  FreeMem(pB, SizeOf(B));
{$ENDIF}
  OK:=(r.ax=$004f);
  VESAGetModeInfo:=OK;
end;

function VESASetMode(Mode: word): boolean;
var r: registers;
    OK: boolean;
{$ifdef FPC}
    B: TVESAModeInfoBlock;
{$endif FPC}
begin
  r.ah:=$4f; r.al:=$02; r.bx:=Mode;
  dos.intr($10,r);
  OK:=(r.ax=$004f);
{$ifdef FPC}
  VESAGetModeInfo(Mode,B);
  { cheat to get a correct mouse }
  { mem[$40:$84]:=B.XResolution-1;
    memw[$40:$4a]:=B.YResolution;}
  { memw[$40:$4c]:=ScreenHeight*((ScreenWidth shl 1)-1); }
{$endif FPC}
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
  Revision 1.5  1999-12-23 23:33:43  pierre
   * use FPC syntax for procvar args

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
