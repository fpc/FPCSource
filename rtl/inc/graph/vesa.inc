{

    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1993,99 by Carl Eric Codere

    This include implements VESA basic access.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type

  palrec = packed record              { record used for set/get DAC palette }
       align: byte;
       blue : byte;
       green: byte;
       red: byte;
  end;

const
  { VESA attributes     }
  attrSwitchDAC        = $01;    { DAC is switchable           (1.2)   }
  attrNotVGACompatible = $02;    { Video is NOT VGA compatible (2.0)   }
  attrSnowCheck        = $04;    { Video must use snow checking(2.0)   }

  { mode attribute bits }
  modeAvail          = $01;      { Hardware supports this mode (1.0)   }
  modeExtendInfo     = $02;      { Extended information        (1.0)   }
  modeBIOSSupport    = $04;      { TTY BIOS Support            (1.0)   }
  modeColor          = $08;      { This is a color mode        (1.0)   }
  modeGraphics       = $10;      { This is a graphics mode     (1.0)   }
  modeNotVGACompatible = $20;    { this mode is NOT I/O VGA compatible (2.0)}
  modeNoWindowed     = $40;      { This mode does not support Windows (2.0) }
  modeLinearBuffer   = $80;      { This mode supports linear buffers  (2.0) }

  { window attributes }
  winSupported       = $01;
  winReadable        = $02;
  winWritable        = $04;

  { memory model }
  modelText          = $00;
  modelCGA           = $01;
  modelHerc          = $02;
  model4plane        = $03;
  modelPacked        = $04;
  modelModeX         = $05;
  modelRGB           = $06;
  modelYUV           = $07;

{$ifndef dpmi}
{$i vesah.inc}
{ otherwise it's already included in graph.pp }
{$endif dpmi}

var

  BytesPerLine: word;              { Number of bytes per scanline }

  { window management }
  ReadWindow : byte;      { Window number for reading. }
  WriteWindow: byte;      { Window number for writing. }
  winReadSeg : word;      { Address of segment for read  }
  winWriteSeg: word;      { Address of segment for writes}
  CurrentReadBank : integer; { active read bank          }
  CurrentWriteBank: integer; { active write bank         }

  BankShift : word;       { address to shift by when switching banks. }

function hexstr(val : longint;cnt : byte) : string;
const
  HexTbl : array[0..15] of char='0123456789ABCDEF';
var
  i : longint;
begin
  hexstr[0]:=char(cnt);
  for i:=cnt downto 1 do
   begin
     hexstr[i]:=hextbl[val and $f];
     val:=val shr 4;
   end;
end;


{$IFDEF DPMI}

  function getVESAInfo(var VESAInfo: TVESAInfo) : boolean;
   var
    ptrlong : longint;
    VESAPtr : ^TVESAInfo;
    regs : TDPMIRegisters;
{$ifndef fpc}
    ModeSel: word;
    offs: longint;
{$endif}
    { added... }
    modelist: PmodeList;
    i: longint;
    RealSeg : word;
   begin
    { Allocate real mode buffer }
{$ifndef fpc}
    Ptrlong:=GlobalDosAlloc(sizeof(TVESAInfo));
    { Get selector value }
    VESAPtr := pointer(Ptrlong shl 16);
{$else fpc}
    Ptrlong:=Global_Dos_Alloc(sizeof(TVESAInfo));
    New(VESAPtr);
{$endif fpc}
    { Get segment value }
    RealSeg := word(Ptrlong shr 16);
    if not assigned(VESAPtr) then
      RunError(203);
    FillChar(regs, sizeof(regs), #0);

    { Get VESA Mode information ... }
    regs.eax := $4f00;
    regs.es := RealSeg;
    regs.edi := $00;
    RealIntr($10, regs);
{$ifdef fpc}
   { no far pointer support in FPC yet, so move the vesa info into a memory }
   { block in the DS slector space (JM)                                     }
    dosmemget(RealSeg,0,VesaPtr^,SizeOf(TVESAInfo));
{$endif fpc}
    if VESAPtr^.Signature <> 'VESA' then
     begin
{$ifdef logging}
         LogLn('No VESA detected.');
{$endif logging}
         getVesaInfo := FALSE;
{$ifndef fpc}
         GlobalDosFree(word(PtrLong and $ffff));
{$else fpc}
         If not Global_Dos_Free(word(PtrLong and $ffff)) then
           RunError(216);
         { also free the extra allocated buffer }
         Dispose(VESAPtr);
{$endif fpc}
         exit;
     end
    else
      getVesaInfo := TRUE;

{$ifndef fpc}
    { The mode pointer buffer points to a real mode memory }
    { Therefore steps to get the modes:                    }
    {  1. Allocate Selector and SetLimit to max number of  }
    {     of possible modes.                               }
    ModeSel := AllocSelector(0);
    SetSelectorLimit(ModeSel, 256*sizeof(word));

    {  2. Set Selector linear address to the real mode pointer }
    {     returned.                                            }
    offs := longint(longint(VESAPtr^.ModeList) shr 16) shl 4;
   {shouldn't the OR in the next line be a + ?? (JM)}
    offs :=  offs OR (Longint(VESAPtr^.ModeList) and $ffff);
    SetSelectorBase(ModeSel, offs);

     { copy VESA mode information to a protected mode buffer and }
     { then free the real mode buffer...                         }
     Move(VESAPtr^, VESAInfo, sizeof(VESAInfo));
     GlobalDosFree(word(PtrLong and $ffff));

    { ModeList points to the mode list     }
    { We must copy it somewhere...         }
    ModeList := Ptr(ModeSel, 0);

{$else fpc}
    { No far pointer support, so the Ptr(ModeSel, 0) doesn't work.     }
    { Immediately copy everything to a buffer in the DS selector space }
     New(ModeList);
    { The following may copy data from outside the VESA buffer, but it   }
    { shouldn't get past the 1MB limit, since that would mean the buffer }
    { has been allocated in the BIOS or high memory region, which seems  }
    { impossible to me (JM)}
     DosMemGet(word(longint(VESAPtr^.ModeList) shr 16),
        word(longint(VESAPtr^.ModeList) and $ffff), ModeList^,256*sizeof(word));

     { copy VESA mode information to a protected mode buffer and }
     { then free the real mode buffer...                         }
     Move(VESAPtr^, VESAInfo, sizeof(VESAInfo));
     If not Global_Dos_Free(word(PtrLong and $ffff)) then
       RunError(216);
     Dispose(VESAPtr);
{$endif fpc}

    i:=0;
    new(VESAInfo.ModeList);
    while ModeList^[i]<> $ffff do
     begin
      VESAInfo.ModeList^[i] := ModeList^[i];
      Inc(i);
     end;
    VESAInfo.ModeList^[i]:=$ffff;
    { Free the temporary selector used to get mode information }
{$ifdef logging}
    LogLn(strf(i) + ' modes found.');
{$endif logging}
{$ifndef fpc}
    FreeSelector(ModeSel);
{$else fpc}
    Dispose(ModeList);
{$endif fpc}
   end;

  function getVESAModeInfo(var ModeInfo: TVESAModeInfo;mode:word):boolean;
   var
    Ptr: longint;
{$ifndef fpc}
    VESAPtr : ^TVESAModeInfo;
{$endif fpc}
    regs : TDPMIRegisters;
    RealSeg: word;
   begin
    { Alllocate real mode buffer }
{$ifndef fpc}
    Ptr:=GlobalDosAlloc(sizeof(TVESAModeInfo));
    { get the selector value }
    VESAPtr := pointer(longint(Ptr shl 16));
    if not assigned(VESAPtr) then
      RunError(203);
{$else fpc}
    Ptr:=Global_Dos_Alloc(sizeof(TVESAModeInfo));
{$endif fpc}
    { get the segment value }
    RealSeg := word(Ptr shr 16);
    { setup interrupt registers }
    FillChar(regs, sizeof(regs), #0);
    { call VESA mode information...}
    regs.eax := $4f01;
    regs.es := RealSeg;
    regs.edi := $00;
    regs.ecx := mode;
    RealIntr($10, regs);
    if word(regs.eax) <> $4f then
      getVESAModeInfo := FALSE
    else
      getVESAModeInfo := TRUE;
    { copy to protected mode buffer ... }
{$ifndef fpc}
    Move(VESAPtr^, ModeInfo, sizeof(ModeInfo));
{$else fpc}
    DosMemGet(RealSeg,0,ModeInfo,sizeof(ModeInfo));
{$endif fpc}
    { free real mode memory  }
{$ifndef fpc}
    GlobalDosFree(Word(Ptr and $ffff));
{$else fpc}
    If not Global_Dos_Free(Word(Ptr and $ffff)) then
      RunError(216);
{$endif fpc}
   end;

{$ELSE}
  function getVESAInfo(var VESAInfo: TVESAInfo) : boolean; assembler;
  asm
       mov ax,4F00h
       les di,VESAInfo
       int 10h
       sub ax,004Fh  {make sure we got 004Fh back}
       cmp ax,1
       sbb al,al
       cmp word ptr es:[di],'V'or('E'shl 8)  {signature should be 'VESA'}
       jne @@ERR
       cmp word ptr es:[di+2],'S'or('A'shl 8)
       je @@X
     @@ERR:
       mov al,0
     @@X:
  end;


  function getModeInfo(var ModeInfo: TVESAModeInfo;mode:word):boolean;assembler;
   asm
     mov ax,4F01h
     mov cx,mode
     les di,ModeInfo
     int 10h
     sub ax,004Fh   {make sure it's 004Fh}
     cmp ax,1
     sbb al,al
   end;

{$ENDIF}

  function SearchVESAModes(mode: Word): boolean;
  {********************************************************}
  { Searches for a specific DEFINED vesa mode. If the mode }
  { is not available for some reason, then returns FALSE   }
  { otherwise returns TRUE.                                }
  {********************************************************}
   var
     i: word;
     ModeSupported : Boolean;
    begin
      i:=0;
      { let's assume it's not available ... }
      ModeSupported := FALSE;
      { This is a STUB VESA implementation  }
      if VESAInfo.ModeList^[0] = $FFFF then exit;
      repeat
        if VESAInfo.ModeList^[i] = mode then
         begin
            { we found it, the card supports this mode... }
            ModeSupported := TRUE;
            break;
         end;
        Inc(i);
      until VESAInfo.ModeList^[i] = $ffff;
      { now check if the hardware supports it... }
      If ModeSupported then
        begin
          { we have to init everything to zero, since VBE < 1.1  }
          { may not setup fields correctly.                      }
          FillChar(VESAModeInfo, sizeof(VESAModeInfo), #0);
          If GetVESAModeInfo(VESAModeInfo, Mode) And
             ((VESAModeInfo.attr and modeAvail) <> 0) then
            ModeSupported := TRUE
          else
            ModeSupported := FALSE;
        end;
       SearchVESAModes := ModeSupported;
    end;



  procedure SetBankIndex(win: byte; BankNr: Integer); assembler;
   asm
     mov  ax,4f05h
     mov  bh,00h
     mov  bl,[Win]
     mov  dx,[BankNr]
{$ifdef fpc}
     push ebp
{$endif fpc}
     int  10h
{$ifdef fpc}
     pop ebp
{$endif fpc}
   end;

  {********************************************************}
  { There are two routines for setting banks. This may in  }
  { in some cases optimize a bit some operations, if the   }
  { hardware supports it, because one window is used for   }
  { reading and one window is used for writing.            }
  {********************************************************}
  procedure SetReadBank(BankNr: Integer);
   begin
     { check if this is the current bank... if so do nothing. }
     if BankNr = CurrentReadBank then exit;
{$ifdef logging}
{     LogLn('Setting read bank to '+strf(BankNr));}
{$endif logging}
     CurrentReadBank := BankNr;          { save current bank number     }
     BankNr := BankNr shl BankShift;     { adjust to window granularity }
     { we set both banks, since one may read only }
     SetBankIndex(ReadWindow, BankNr);
     { if the hardware supports only one window }
     { then there is only one single bank, so   }
     { update both bank numbers.                }
     if ReadWindow = WriteWindow then
       CurrentWriteBank := CurrentReadBank;
   end;

  procedure SetWriteBank(BankNr: Integer);
   begin
     { check if this is the current bank... if so do nothing. }
     if BankNr = CurrentWriteBank then exit;
{$ifdef logging}
{     LogLn('Setting write bank to '+strf(BankNr));}
{$endif logging}
     CurrentWriteBank := BankNr;          { save current bank number     }
     BankNr := BankNr shl BankShift;     { adjust to window granularity }
     { we set both banks, since one may read only }
     SetBankIndex(WriteWindow, BankNr);
     { if the hardware supports only one window }
     { then there is only one single bank, so   }
     { update both bank numbers.                }
     if ReadWindow = WriteWindow then
       CurrentReadBank := CurrentWriteBank;
   end;

 {************************************************************************}
 {*                     8-bit pixels VESA mode routines                  *}
 {************************************************************************}

  procedure PutPixVESA256(x, y : integer; color : word); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     { convert to absolute coordinates and then verify clipping...}
     if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
     offs := longint(y) * BytesPerLine + x;
     SetWriteBank(integer(offs shr 16));
     mem[WinWriteSeg : word(offs)] := byte(color);
  end;

  procedure DirectPutPixVESA256(x, y : integer); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
     col : byte;
  begin
     offs := longint(y) * BytesPerLine + x;
     SetWriteBank(integer(offs shr 16));
     Case CurrentWriteMode of
       XorPut:
         Begin
           SetReadBank(integer(offs shr 16));
           mem[WinWriteSeg : word(offs)] := mem[WinReadSeg : word(offs)] xor byte(CurrentColor);
         End;
       AndPut:
         Begin
           SetReadBank(integer(offs shr 16));
           mem[WinWriteSeg : word(offs)] := mem[WinReadSeg : word(offs)] And byte(CurrentColor);
         End;
       OrPut:
         Begin
           SetReadBank(integer(offs shr 16));
           mem[WinWriteSeg : word(offs)] := mem[WinReadSeg : word(offs)] or byte(currentcolor);
         End
       else
         Begin
           If CurrentWriteMode <> NotPut then
             col := Byte(CurrentColor)
           else col := Not(Byte(CurrentColor));
           mem[WinWriteSeg : word(offs)] := Col;
         End
     End;
  end;

  function GetPixVESA256(x, y : integer): word; {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     offs := longint(y) * BytesPerLine + x;
     SetReadBank(integer(offs shr 16));
     GetPixVESA256:=mem[WinReadSeg : word(offs)];
  end;

  procedure HLineVESA256(x,x2,y: integer); {$ifndef fpc}far;{$endif fpc}

   var Offs: Longint;
       mask, l, bankrest: longint;
       curbank, hlength: integer;
   Begin
    { must we swap the values? }
    if x > x2 then
      Begin
        x := x xor x2;
        x2 := x xor x2;
        x:= x xor x2;
      end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    X2  := X2 + StartXViewPort;
    Y   := Y + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x2,y,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    {$ifdef logging}
    LogLn('hline '+strf(x)+' - '+strf(x2)+' on '+strf(y)+' in mode '+strf(currentwritemode));
    {$endif logging}
    HLength := x2 - x + 1;
    {$ifdef logging}
    LogLn('length: '+strf(hlength));
    {$endif logging}
    if HLength>0 then
      begin
         Offs:=Longint(y)*bytesperline+x;
         {$ifdef logging}
         LogLn('Offs: '+strf(offs)+' -- '+hexstr(offs,8));
         {$endif logging}
         Mask := byte(CurrentColor)+byte(CurrentColor) shl 8;
         Mask := Mask + Mask shl 16;
         Case CurrentWriteMode of
           AndPut:
             Begin
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If HLength > 3 Then
                 { allign target }
                   Begin
                     l := 0;
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging}
                         LogLn('Alligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] :=
                             Mem[WinReadSeg:word(offs)+l-1] And Byte(CurrentColor);
                       End;
                     Dec(HLength, l);
                     inc(offs, l);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes alligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     {$ifdef logging}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] And Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     x := offs mod bytesperline;
                     For l := 0 to HLength - 1 do
                       DirectPutPixVESA256(x+l,y);
                     HLength := 0
                   End
               Until HLength = 0;
             End;
           XorPut:
             Begin
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If HLength > 3 Then
                 { allign target }
                   Begin
                     l := 0;
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging}
                         LogLn('Alligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] :=
                             Mem[WinReadSeg:word(offs)+l-1] Xor Byte(CurrentColor);
                       End;
                     Dec(HLength, l);
                     inc(offs, l);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes alligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     {$ifdef logging}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] Xor Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     x := offs mod bytesperline;
                     For l := 0 to HLength - 1 do
                       DirectPutPixVESA256(x+l,y);
                     HLength := 0
                   End
               Until HLength = 0;
             End;
           OrPut:
             Begin
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If HLength > 3 Then
                 { allign target }
                   Begin
                     l := 0;
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging}
                         LogLn('Alligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] :=
                             Mem[WinReadSeg:word(offs)+l-1] Or Byte(CurrentColor);
                       End;
                     Dec(HLength, l);
                     inc(offs, l);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes alligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     {$ifdef logging}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:offs+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] Or Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     x := offs mod bytesperline;
                     For l := 0 to HLength - 1 do
                       DirectPutPixVESA256(x+l,y);
                     HLength := 0
                   End
               Until HLength = 0;
             End
           Else
             Begin
               If CurrentWriteMode = NotPut Then
                 Mask := Not(Mask);
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8)+' -- '+strf(offs));
                 {$endif logging}
                 If HLength > 3 Then
                 { allign target }
                   Begin
                     l := 0;
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging}
                         LogLn('Alligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] := Byte(Mask);
                       End;
                     Dec(HLength, l);
                     inc(offs, l);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes alligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     {$ifdef logging}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest)+' -- '+hexstr(bankrest,8));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] := Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     x := offs mod bytesperline;
                     For l := 0 to HLength - 1 do
                       DirectPutPixVESA256(x+l,y);
                     HLength := 0
                   End
               Until HLength = 0;
             End;
         End;
       end;
   end;

  procedure VLineVESA256(x,y,y2: integer); {$ifndef fpc}far;{$endif fpc}

   var Offs: Longint;
       l, bankrest: longint;
       curbank, vlength: integer;
       col: byte;
   Begin
    { must we swap the values? }
    if y > y2 then
      Begin
        y := y xor y2;
        y2 := y xor y2;
        y:= y xor y2;
      end;
    { First convert to global coordinates }
    X   := X + StartXViewPort;
    Y   := Y + StartYViewPort;
    Y2  := Y2 + StartYViewPort;
    if ClipPixels then
      Begin
         if LineClipped(x,y,x,y2,StartXViewPort,StartYViewPort,
                StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
            exit;
      end;
    {$ifdef logging}
    LogLn('vline '+strf(y)+' - '+strf(y2)+' on '+strf(x)+' in mode '+strf(currentwritemode));
    {$endif logging}
    VLength := y2 - y + 1;
    {$ifdef logging}
    LogLn('length: '+strf(vlength));
    {$endif logging}
    if VLength>0 then
      begin
         Offs:=Longint(y)*bytesperline+x;
         {$ifdef logging}
         LogLn('Offs: '+strf(offs)+' -- '+hexstr(offs,8));
         {$endif logging}
         Case CurrentWriteMode of
           AndPut:
             Begin
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($10000-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($10000 - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] :=
                       Mem[WinReadSeg:word(offs)] And Byte(CurrentColor);
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
           XorPut:
             Begin
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($10000-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($10000 - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] :=
                       Mem[WinReadSeg:word(offs)] Xor Byte(CurrentColor);
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
           OrPut:
             Begin
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($10000-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($10000 - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] :=
                       Mem[WinReadSeg:word(offs)] Or Byte(CurrentColor);
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
           Else
             Begin
               If CurrentWriteMode = NotPut Then
                 Col := Not(CurrentColor);
               Repeat
                 curbank := integer(offs shr 16);
                 SetWriteBank(curbank);
                 {$ifdef logging}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($10000-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($10000 - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] := Col;
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
         End;
       end;
   end;

 {************************************************************************}
 {*                    15/16bit pixels VESA mode routines                *}
 {************************************************************************}

  procedure PutPixVESA32k(x, y : integer; color : word); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     { convert to absolute coordinates and then verify clipping...}
     if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
     offs := longint(y) * BytesPerLine + 2*x;
     SetWriteBank(integer(offs shr 16));
     memW[WinWriteSeg : word(offs)] := color;
  end;

  procedure PutPixVESA64k(x, y : integer; color : word); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     { convert to absolute coordinates and then verify clipping...}
     if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
    offs := longint(y) * BytesPerLine + 2*x;
    SetWriteBank(integer(offs shr 16));
    memW[WinWriteSeg : word(offs)] := color;
  end;

  function GetPixVESA32k(x, y : integer): word; {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     offs := longint(y) * BytesPerLine + 2*x;
     SetReadBank(integer(offs shr 16));
     GetPixVESA32k:=memW[WinReadSeg : word(offs)];
  end;

  function GetPixVESA64k(x, y : integer): word; {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     offs := longint(y) * BytesPerLine + 2*x;
     SetReadBank(integer(offs shr 16));
     GetPixVESA64k:=memW[WinReadSeg : word(offs)];
  end;

  procedure DirectPutPixVESA32k(x, y : integer); {$ifndef fpc}far;{$endif fpc}
  var
     col : word;
     offs : longint;
  begin
     offs := longint(y) * BytesPerLine + 2*x;
     SetWriteBank(integer((offs shr 16) and $ff));
     Case CurrentWriteMode of
       XorPut:
         Begin
           SetReadBank(integer(offs shr 16));
           memW[WinWriteSeg : word(offs)] := memW[WinReadSeg : word(offs)] xor currentcolor;
         End;
       AndPut:
         Begin
           SetReadBank(integer(offs shr 16));
           memW[WinWriteSeg : word(offs)] := memW[WinReadSeg : word(offs)] And currentcolor;
         End;
       OrPut:
         Begin
           SetReadBank(integer(offs shr 16));
           memW[WinWriteSeg : word(offs)] := memW[WinReadSeg : word(offs)] or currentcolor;
         End
       else
         Begin
           If CurrentWriteMode <> NotPut Then
             col := CurrentColor
           Else col := Not(CurrentColor);
           memW[WinWriteSeg : word(offs)] := Col;
         End
     End;
  end;

  procedure DirectPutPixVESA64k(x, y : integer); {$ifndef fpc}far;{$endif fpc}
  var
     Col : word;
     offs : longint;
  begin
     offs := longint(y) * BytesPerLine + 2*x;
     SetWriteBank(integer(offs shr 16));
     Case CurrentWriteMode of
       XorPut:
         Begin
           SetReadBank(integer(offs shr 16));
           memW[WinWriteSeg : word(offs)] := memW[WinReadSeg : word(offs)] xor currentcolor;
         End;
       AndPut:
         Begin
           SetReadBank(integer(offs shr 16));
           memW[WinWriteSeg : word(offs)] := memW[WinReadSeg : word(offs)] And currentcolor;
         End;
       OrPut:
         Begin
           SetReadBank(integer(offs shr 16));
           memW[WinWriteSeg : word(offs)] := memW[WinReadSeg : word(offs)] or currentcolor;
         End
       Else
         Begin
           If CurrentWriteMode <> NotPut Then
             col := CurrentColor
           Else col := Not(CurrentColor);
           memW[WinWriteSeg : word(offs)] := Col;
         End
     End;
  end;

 {************************************************************************}
 {*                     4-bit pixels VESA mode routines                  *}
 {************************************************************************}

  procedure PutPixVESA16(x, y : integer; color : word); {$ifndef fpc}far;{$endif fpc}
    var
     offs : longint;
     dummy_read : byte;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     { convert to absolute coordinates and then verify clipping...}
    if ClipPixels then
     Begin
       if (X < StartXViewPort) or (X > (StartXViewPort + ViewWidth)) then
         exit;
       if (Y < StartYViewPort) or (Y > (StartYViewPort + ViewHeight)) then
         exit;
     end;
     { this can be done only once at InitGraph }
     PortW[$3C4] := $0f02;
     PortW[$3CE] := $0003;
     PortW[$3CE] := $0205;
     { }
     offs := longint(y) * BytesPerLine + (x div 8);
     SetWriteBank(integer(offs shr 16));
     port[$3CE] := $08;
     port[$3CF] := ($80 shr (x and 7));
     dummy_read := mem[WinWriteSeg : word(offs)];
     mem[winWriteSeg : offs] := byte(color);
     { this can be done only once at DoneGraph..}
     PortW[$3CE] := $FF08;
     PortW[$3CE] := $0005;
     { }
  end;

  procedure DirectPutPixVESA16(x, y : integer); {$ifndef fpc}far;{$endif fpc}
    var
     offs : longint;
     dummy_read : byte;
  begin
     { this can be done only once at InitGraph }
     PortW[$3C4] := $0f02;
     PortW[$3CE] := $0003;
     PortW[$3CE] := $0205;
     { }
     offs := longint(y) * BytesPerLine + (x div 8);
     SetWriteBank(integer(offs shr 16));
     port[$3CE] := $08;
     port[$3CF] := ($80 shr (x and 7));
     dummy_read := mem[WinWriteSeg : word(offs)];
     mem[winWriteSeg : offs] := byte(CurrentColor);
     { this can be done only once at DoneGraph..}
     PortW[$3CE] := $FF08;
     PortW[$3CE] := $0005;
     { }
  end;


 {************************************************************************}
 {*                     VESA Palette entries                             *}
 {************************************************************************}


{$IFDEF DPMI}
   Procedure SetVESARGBPalette(ColorNum, RedValue, GreenValue,
      BlueValue : Integer);
    var
     FunctionNr : byte;   { use blankbit or normal RAMDAC programming? }
     pal: palrec;
     Error : boolean;     { VBE call error                             }
     regs: TDPMIRegisters;
     Ptr: longint;
{$ifndef fpc}
     PalPtr : ^PalRec;
{$endif fpc}
     RealSeg: word;
    begin
      if DirectColor then
        Begin
          _GraphResult := grError;
          exit;
        end;
        Error := TRUE;
        pal.align := 0;
        pal.red := byte(RedValue);
        pal.green := byte(GreenValue);
        pal.blue := byte(BlueValue);
        { use the set/get palette function }
        if VESAInfo.Version >= $0200 then
          Begin
            { check if blanking bit must be set when programming }
            { the RAMDAC.                                        }
            if (VESAInfo.caps and attrSnowCheck) <> 0 then
              FunctionNr := $80
            else
              FunctionNr := $00;

            { Alllocate real mode buffer }
{$ifndef fpc}
            Ptr:=GlobalDosAlloc(sizeof(palrec));
            { get the selector values }
            PalPtr := pointer(Ptr shl 16);
            if not assigned(PalPtr) then
               RunError(203);
{$else fpc}
            Ptr:=Global_Dos_Alloc(sizeof(palrec));
{$endif fpc}
            {get the segment value}
            RealSeg := word(Ptr shr 16);
            { setup interrupt registers }
            FillChar(regs, sizeof(regs), #0);
            { copy palette values to real mode buffer }
{$ifndef fpc}
            move(pal, palptr^, sizeof(pal));
{$else fpc}
            DosMemPut(RealSeg,0,pal,sizeof(pal));
{$endif fpc}
            regs.eax := $4F09;
            regs.ebx := FunctionNr;
            regs.ecx := $01;
            regs.edx := ColorNum;
            regs.es  := RealSeg;
            regs.edi := 0;         { offset is always zero }
            RealIntr($10, regs);

            { free real mode memory  }
{$ifndef fpc}
            GlobalDosFree(word(Ptr and $ffff));
{$else fpc}
            If not Global_Dos_Free(word(Ptr and $ffff)) then
              RunError(216);
{$endif fpc}

            if word(regs.eax) <> $004F then
              begin
                _GraphResult := grError;
                exit;
              end;
          end
        else
          { assume it's fully VGA compatible palette-wise. }
          Begin
            SetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);
          end;
    end;


  Procedure GetVESARGBPalette(ColorNum: integer; Var
      RedValue, GreenValue, BlueValue : integer);
   var
    pal: PalRec;
{$ifndef fpc}
    palptr : ^PalRec;
{$endif}
    regs : TDPMIRegisters;
    RealSeg: word;
    ptr: longint;
   begin
      if DirectColor then
        Begin
          _GraphResult := grError;
          exit;
        end;
        { use the set/get palette function }
        if VESAInfo.Version >= $0200 then
          Begin
            { Alllocate real mode buffer }
{$ifndef fpc}
            Ptr:=GlobalDosAlloc(sizeof(palrec));
            { get the selector value }
            PalPtr := pointer(longint(Ptr and $0000ffff) shl 16);
            if not assigned(PalPtr) then
               RunError(203);
{$else fpc}
            Ptr:=Global_Dos_Alloc(sizeof(palrec));
{$endif fpc}
            { get the segment value }
            RealSeg := word(Ptr shr 16);
            { setup interrupt registers }
            FillChar(regs, sizeof(regs), #0);

            regs.eax := $4F09;
            regs.ebx := $01;       { get palette data      }
            regs.ecx := $01;
            regs.edx := ColorNum;
            regs.es  := RealSeg;
            regs.edi := 0;         { offset is always zero }
            RealIntr($10, regs);

           { copy to protected mode buffer ... }
{$ifndef fpc}
           Move(PalPtr^, Pal, sizeof(pal));
{$else fpc}
           DosMemGet(RealSeg,0,Pal,sizeof(pal));
{$endif fpc}
           { free real mode memory  }
{$ifndef fpc}
           GlobalDosFree(word(Ptr and $ffff));
{$else fpc}
           If not Global_Dos_Free(word(Ptr and $ffff)) then
             RunError(216);
{$endif fpc}

            if word(regs.eax) <> $004F then
              begin
                _GraphResult := grError;
                exit;
              end
            else
              begin
                RedValue := Integer(pal.Red);
                GreenValue := Integer(pal.Green);
                BlueValue := Integer(pal.Blue);
              end;
          end
        else
            GetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);
   end;
{$ELSE}

   Procedure SetVESARGBPalette(ColorNum, RedValue, GreenValue,
      BlueValue : Integer); far;
    var
     FunctionNr : byte;   { use blankbit or normal RAMDAC programming? }
     pal: ^palrec;
     Error : boolean;     { VBE call error                             }
    begin
      if DirectColor then
        Begin
          _GraphResult := grError;
          exit;
        end;
        Error := FALSE;
        new(pal);
        if not assigned(pal) then RunError(203);
        pal^.align := 0;
        pal^.red := byte(RedValue);
        pal^.green := byte(GreenValue);
        pal^.blue := byte(BlueValue);
        { use the set/get palette function }
        if VESAInfo.Version >= $0200 then
          Begin
            { check if blanking bit must be set when programming }
            { the RAMDAC.                                        }
            if (VESAInfo.caps and attrSnowCheck) <> 0 then
              FunctionNr := $80
            else
              FunctionNr := $00;
            asm
              mov  ax, 4F09h         { Set/Get Palette data    }
              mov  bl, [FunctionNr]  { Set palette data        }
              mov  cx, 01h           { update one palette reg. }
              mov  dx, [ColorNum]    { register number to update }
              les  di, [pal]         { get palette address     }
              int  10h
              cmp  ax, 004Fh         { check if success        }
              jz   @noerror
              mov  [Error], TRUE
             @noerror:
            end;
            if not Error then
                Dispose(pal)
            else
              begin
                _GraphResult := grError;
                exit;
              end;
          end
        else
          { assume it's fully VGA compatible palette-wise. }
          Begin
            SetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);
          end;
    end;




  Procedure GetVESARGBPalette(ColorNum: integer; Var
      RedValue, GreenValue, BlueValue : integer); far;
   var
    Error: boolean;
    pal: ^palrec;
   begin
      if DirectColor then
        Begin
          _GraphResult := grError;
          exit;
        end;
      Error := FALSE;
      new(pal);
      if not assigned(pal) then RunError(203);
      FillChar(pal^, sizeof(palrec), #0);
      { use the set/get palette function }
      if VESAInfo.Version >= $0200 then
        Begin
          asm
            mov  ax, 4F09h         { Set/Get Palette data    }
            mov  bl, 01h           { Set palette data        }
            mov  cx, 01h           { update one palette reg. }
            mov  dx, [ColorNum]    { register number to update }
            les  di, [pal]         { get palette address     }
            int  10h
            cmp  ax, 004Fh         { check if success        }
            jz   @noerror
            mov  [Error], TRUE
          @noerror:
          end;
          if not Error then
            begin
              RedValue := Integer(pal^.Red);
              GreenValue := Integer(pal^.Green);
              BlueValue := Integer(pal^.Blue);
              Dispose(pal);
            end
          else
            begin
              _GraphResult := grError;
              exit;
            end;
        end
        else
            GetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);

   end;
{$ENDIF}


  procedure SetupLinear(var ModeInfo: TVESAModeInfo);
   begin
     { !!!!!!!!!!!!!!!!!!!!!!!!!!!!!! }
   end;

  procedure SetupWindows(var ModeInfo: TVESAModeInfo);
   begin
     { now we check the windowing scheme ...}
     if (ModeInfo.WinAAttr and WinSupported) <> 0 then
       { is this window supported ... }
       begin
         { now check if the window is R/W }
         if (ModeInfo.WinAAttr and WinReadable) <> 0 then
         begin
           ReadWindow := 0;
           WinReadSeg := ModeInfo.WinASeg;
         end;
         if (ModeInfo.WinAAttr and WinWritable) <> 0 then
         begin
           WriteWindow := 0;
           WinWriteSeg := ModeInfo.WinASeg;
         end;
       end;
     if (ModeInfo.WinBAttr and WinSupported) <> 0 then
       { is this window supported ... }
       begin

         { OPTIMIZATION ... }
         { if window A supports both read/write, then we try to optimize }
         { everything, by using a different window for Read and/or write.}
         if (WinReadSeg <> 0) and (WinWriteSeg <> 0) then
           begin
              { check if winB supports read }
              if (ModeInfo.WinBAttr and winReadable) <> 0 then
                begin
                  WinReadSeg := ModeInfo.WinBSeg;
                  ReadWindow := 1;
                end
              else
              { check if WinB supports write }
              if (ModeInfo.WinBAttr and WinWritable) <> 0 then
                begin
                  WinWriteSeg := ModeInfo.WinBSeg;
                  WriteWindow := 1;
                end;
           end
         else
         { Window A only supported Read OR Write, no we have to make }
         { sure that window B supports the other mode.               }
         if (WinReadSeg = 0) and (WinWriteSeg<>0) then
           begin
              if (ModeInfo.WinBAttr and WinReadable <> 0) then
                begin
                  ReadWindow := 1;
                  WinReadSeg := ModeInfo.WinBSeg;
                end
              else
                { impossible, this VESA mode is WRITE only! }
                begin
                  WriteLn('Invalid VESA Window attribute.');
                  Halt(255);
                end;
           end
         else
         if (winWriteSeg = 0) and (WinReadSeg<>0) then
           begin
             if (ModeInfo.WinBAttr and WinWritable) <> 0 then
               begin
                 WriteWindow := 1;
                 WinWriteSeg := ModeInfo.WinBSeg;
               end
             else
               { impossible, this VESA mode is READ only! }
               begin
                  WriteLn('Invalid VESA Window attribute.');
                  Halt(255);
               end;
           end
         else
         if (winReadSeg = 0) and (winWriteSeg = 0) then
         { no read/write in this mode! }
           begin
                  WriteLn('Invalid VESA Window attribute.');
                  Halt(255);
           end;
       end;

     { if both windows are not supported, then we can assume }
     { that there is ONE single NON relocatable window.      }
     if (WinWriteSeg = 0) and (WinReadSeg = 0) then
       begin
         WinWriteSeg := ModeInfo.WinASeg;
         WinReadSeg := ModeInfo.WinASeg;
       end;

    { 16-bit Protected mode checking code...  }
    { change segment values to protected mode }
    { selectors.                              }
    if WinReadSeg = $A000 then
      WinReadSeg := SegA000
    else
    if WinReadSeg = $B000 then
      WinReadSeg := SegB000
    else
    if WinReadSeg = $B800 then
      WinReadSeg := SegB800
    else
      begin
        WriteLn('Invalid segment address.');
        Halt(255);
      end;
    if WinWriteSeg = $A000 then
      WinWriteSeg := SegA000
    else
    if WinWriteSeg = $B000 then
      WinWriteSeg := SegB000
    else
    if WinWriteSeg = $B800 then
      WinWriteSeg := SegB800
    else
      begin
        WriteLn('Invalid segment address.');
        Halt(255);
      end;

   end;



  function setVESAMode(mode:word):boolean;
    var i:word;
  begin
   { Init mode information, for compatibility with VBE < 1.1 }
   FillChar(VESAModeInfo, sizeof(TVESAModeInfo), #0);
   { get the video mode information }
   if getVESAModeInfo(VESAmodeinfo, mode) then
   begin
     { checks if the hardware supports the video mode. }
     if (VESAModeInfo.attr and modeAvail) <> 0 then
       begin
         SetVESAMode := TRUE;
       end
     else
       begin
         SetVESAmode := FALSE;
         _GraphResult := grError;
         exit;
       end;

     BankShift := 0;
     while (64 shr BankShift) <> VESAModeInfo.WinGranularity do
        Inc(BankShift);
     CurrentWriteBank := -1;
     CurrentReadBank := -1;
     BytesPerLine := VESAModeInfo.BytesPerScanLine;

     { These are the window adresses ... }
     WinWriteSeg := 0;  { This is the segment to use for writes }
     WinReadSeg := 0;   { This is the segment to use for reads  }
     ReadWindow := 0;
     WriteWindow := 0;

     { VBE 2.0 and higher supports >= non VGA linear buffer types...}
     { this is backward compatible.                                 }
     if ((VESAModeInfo.Attr and ModeNoWindowed) <> 0) and
          ((VESAModeInfo.Attr and ModeLinearBuffer) <> 0) then
        SetupLinear(VESAModeInfo)
     else
     { if linear and windowed is supported, then use windowed }
     { method.                                                }
        SetUpWindows(VESAModeInfo);


   asm
    mov ax,4F02h
    mov bx,mode
{$ifdef fpc}
    push ebp
{$endif fpc}
    int 10h
{$ifdef fpc}
    pop ebp
{$endif fpc}
    sub ax,004Fh
    cmp ax,1
    sbb al,al
    mov @RESULT,al
   end;
  end;
 end;


 function getVESAMode:word;assembler;
   asm  {return -1 if error}
    mov ax,4F03h
{$ifdef fpc}
    push ebp
{$endif fpc}
    int 10h
{$ifdef fpc}
    pop ebp
{$endif fpc}
    cmp ax,004Fh
    je @@OK
    mov ax,-1
    jmp @@X
  @@OK:
    mov ax,bx
  @@X:
   end;




 {************************************************************************}
 {*                     VESA Modes inits                                 *}
 {************************************************************************}

 procedure Init1280x1024x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVesaMode(m1280x1024x64k);
  end;

 procedure Init1280x1024x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1280x1024x32k);
  end;

 procedure Init1280x1024x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1280x1024x256);
  end;


 procedure Init1280x1024x16; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1280x1024x16);
  end;

 procedure Init1024x768x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x64k);
  end;

 procedure Init640x480x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x480x32k);
  end;

 procedure Init1024x768x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x256);
  end;

 procedure Init1024x768x16; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x16);
  end;

 procedure Init800x600x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m800x600x64k);
  end;

 procedure Init800x600x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m800x600x32k);
  end;

 procedure Init800x600x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m800x600x256);
  end;

 procedure Init800x600x16; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVesaMode(m800x600x16);
  end;

 procedure Init640x480x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x480x64k);
  end;


 procedure Init640x480x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x480x256);
  end;

 procedure Init640x400x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x400x256);
  end;

 procedure Init320x200x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m320x200x64k);
  end;

 procedure Init320x200x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m320x200x32k);
  end;


{$IFDEF DPMI}

 Procedure SaveStateVESA; {$ifndef fpc}far;{$endif fpc}
 var
  PtrLong: longint;
  regs: TDPMIRegisters;
  begin
    SaveSupported := FALSE;
    SavePtr := nil;
    { Get the video mode }
    asm
      mov  ah,0fh
{$ifdef fpc}
      push ebp
{$endif fpc}
      int  10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
      mov  [VideoMode], al
    end;
    { Prepare to save video state...}
    asm
      mov  ax, 4F04h       { get buffer size to save state }
      mov  dx, 00h
      mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
{$ifdef fpc}
      push ebp
{$endif fpc}
      int  10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
      mov  [StateSize], bx
      cmp  al,04fh
      jnz  @notok
      mov  [SaveSupported],TRUE
     @notok:
    end;
    if SaveSupported then
      begin
{$ifdef logging}
        LogLn('allocating VESA save buffer of '+strf(64*StateSize));
{$endif logging}
{$ifndef fpc}
        PtrLong:=GlobalDosAlloc(64*StateSize);  { values returned in 64-byte blocks }
{$else fpc}
        PtrLong:=Global_Dos_Alloc(64*StateSize);  { values returned in 64-byte blocks }
{$endif fpc}
        if PtrLong = 0 then
           RunError(203);
        SavePtr := pointer(longint(PtrLong and $0000ffff) shl 16);
{$ifndef fpc}
        { In FPC mode, we can't do anything with this (no far pointers)  }
        { However, we still need to keep it to be able to free the       }
        { memory afterwards. Since this data is not accessed in PM code, }
        { there's no need to save it in a seperate buffer (JM)           }
        if not assigned(SavePtr) then
           RunError(203);
{$endif fpc}
        RealStateSeg := word(PtrLong shr 16);

        FillChar(regs, sizeof(regs), #0);
        { call the real mode interrupt ... }
        regs.eax := $4F04;      { save the state buffer                   }
        regs.ecx := $0F;        { Save DAC / Data areas / Hardware states }
        regs.edx := $01;        { save state                              }
        regs.es := RealStateSeg;
        regs.ebx := 0;
        RealIntr($10,regs);
        FillChar(regs, sizeof(regs), #0);
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
        regs.eax := $4F04;      { restore the state buffer                }
        regs.ecx := $0F;        { rest DAC / Data areas / Hardware states }
        regs.edx := $02;
        regs.es := RealStateSeg;
        regs.ebx := 0;
        RealIntr($10,regs);
      end;
  end;

 procedure RestoreStateVESA; {$ifndef fpc}far;{$endif fpc}
  var
   regs:TDPMIRegisters;
  begin
     { go back to the old video mode...}
     asm
      mov  ah,00
      mov  al,[VideoMode]
{$ifdef fpc}
      push ebp
{$endif fpc}
      int  10h
{$ifdef fpc}
      pop ebp
{$endif fpc}
     end;
     { then restore all state information }
{$ifndef fpc}
     if assigned(SavePtr) and (SaveSupported=TRUE) then
{$else fpc}
     { No far pointer support, so it's possible that that assigned(SavePtr) }
     { would return false under FPC. Just check if it's different from nil. }
     if (SavePtr <> nil) and (SaveSupported=TRUE) then
{$endif fpc}
       begin
        FillChar(regs, sizeof(regs), #0);
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
         regs.eax := $4F04;      { restore the state buffer                }
         regs.ecx := $0F;        { rest DAC / Data areas / Hardware states }
         regs.edx := $02;        { restore state                           }
         regs.es := RealStateSeg;
         regs.ebx := 0;
         RealIntr($10,regs);
{$ifndef fpc}
         if GlobalDosFree(longint(SavePtr) shr 16)<>0 then
{$else fpc}
         if Not(Global_Dos_Free(longint(SavePtr) shr 16)) then
{$endif fpc}
          RunError(216);
         SavePtr := nil;
       end;
  end;

{$ELSE}

      {**************************************************************}
      {*                     Real mode routines                     *}
      {**************************************************************}

 Procedure SaveStateVESA; far;
  begin
    SavePtr := nil;
    SaveSupported := FALSE;
    { Get the video mode }
    asm
      mov  ah,0fh
      int  10h
      mov  [VideoMode], al
    end;
    { Prepare to save video state...}
    asm
      mov  ax, 1C00h       { get buffer size to save state }
      mov  cx, 00000111b   { Save DAC / Data areas / Hardware states }
      int  10h
      mov  [StateSize], bx
      cmp  al,01ch
      jnz  @notok
      mov  [SaveSupported],TRUE
     @notok:
    end;
    if SaveSupported then
      Begin
        GetMem(SavePtr, 64*StateSize); { values returned in 64-byte blocks }
        if not assigned(SavePtr) then
           RunError(203);
        asm
         mov  ax, 4F04h       { save the state buffer                   }
         mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
         mov  dx, 01h
         mov  es, WORD PTR [SavePtr+2]
         mov  bx, WORD PTR [SavePtr]
         int  10h
        end;
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
        asm
         mov  ax, 4F04h       { save the state buffer                   }
         mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
         mov  dx, 02h
         mov  es, WORD PTR [SavePtr+2]
         mov  bx, WORD PTR [SavePtr]
         int  10h
        end;
      end;
  end;

 procedure RestoreStateVESA; far;
  begin
     { go back to the old video mode...}
     asm
      mov  ah,00
      mov  al,[VideoMode]
      int  10h
     end;

     { then restore all state information }
     if assigned(SavePtr) and (SaveSupported=TRUE) then
       begin
         { restore state, according to Ralph Brown Interrupt list }
         asm
           mov  ax, 4F04h       { save the state buffer                   }
           mov  cx, 00001111b   { Save DAC / Data areas / Hardware states }
           mov  dx, 02h         { restore state                           }
           mov  es, WORD PTR [SavePtr+2]
           mov  bx, WORD PTR [SavePtr]
           int  10h
         end;
         FreeMem(SavePtr, 64*StateSize);
         SavePtr := nil;
       end;
  end;
{$ENDIF DPMI}

 {************************************************************************}
 {*                     VESA Page flipping routines                      *}
 {************************************************************************}
 { Note: These routines, according  to the VBE3 specification, will NOT   }
 { work with the 24 bpp modes, because of the alignment.                  }
 {************************************************************************}
 procedure SetVisualVESA(page: word); {$ifndef fpc}far;{$endif fpc}
  { two page support... }
  begin
    if page > HardwarePages then exit;
  end;

 procedure SetActiveVESA(page: word); {$ifndef fpc}far;{$endif fpc}
  { two page support... }
  begin
  end;

{
$Log$
Revision 1.17  1999-09-27 23:34:42  peter
  * new graph unit is default for go32v2
  * removed warnings/notes

Revision 1.16  1999/09/26 13:31:07  jonas
  * changed name of modeinfo variable to vesamodeinfo and fixed
    associated errors (fillchar(modeinfo,sizeof(tmodeinfo),#0) instead
    of sizeof(TVesamodeinfo) etc)
  * changed several sizeof(type) to sizeof(varname) to avoid similar
    errors in the future

Revision 1.15  1999/09/24 22:52:39  jonas
  * optimized patternline a bit (always use hline when possible)
  * isgraphmode stuff cleanup
  * vesainfo.modelist now gets disposed in cleanmode instead of in
    closegraph (required moving of some declarations from vesa.inc to
    new vesah.inc)
  * queryadapter gets no longer called from initgraph (is called from
    initialization of graph unit)
  * bugfix for notput in 32k and 64k vesa modes
  * a div replaced by / in fillpoly

Revision 1.14  1999/09/23 14:00:42  jonas
  * -dlogging no longer required to fuction correctly
  * some typo's fixed

Revision 1.13  1999/09/20 09:34:30  florian
  * conflicts solved

Revision 1.12  1999/09/18 22:21:11  jonas
  + hlinevesa256 and vlinevesa256
  + support for not/xor/or/andput in vesamodes with 32k/64k colors
  * lots of changes to avoid warnings under FPC

Revision 1.11  1999/09/15 11:40:30  jonas
  * fixed PutPixVESA256

Revision 1.10  1999/09/11 19:43:02  jonas
  * FloodFill: did not take into account current viewport settings
  * GetScanLine: only get line inside viewport, data outside of it
    is not used anyway
  * InternalEllipseDefault: fix for when xradius or yradius = 0 and
    increase xradius and yradius always by one (TP does this too)
  * fixed conlict in vesa.inc from last update
  * some conditionals to avoid range check and overflow errors in
    places where it doesn't matter

Revision 1.9  1999/08/01 14:51:07  jonas
  * removed and/or/xorput support from vesaputpix256 (not in TP either)
  * added notput support to directputpix256

Revision 1.8  1999/07/18 15:07:21  jonas
  + xor-, and and- orput support for VESA256 modes
  * compile with -dlogging if you wnt some info to be logged to grlog.txt

Revision 1.7  1999/07/14 15:21:49  jonas
  * fixed initialization of bankshift var ('64 shr banshift' instead of shl)

Revision 1.6  1999/07/14 13:17:29  jonas
  * bugfix in getmodeinfo (SizeOf(TModeInfo) -> SizeOf(TVESAModeInfo))
  * as the result of the above bugfix, the graph unit doesn't crash
    anymore under FPC if compiler with -dsupportVESA, but it doesn't
    work yet either...

Revision 1.5  1999/07/12 13:28:33  jonas
  * forgot log tag in previous commit

}
