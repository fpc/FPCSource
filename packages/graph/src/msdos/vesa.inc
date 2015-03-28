{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Carl Eric Codere

    This include implements VESA basic access.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
type

  palrec = packed record              { record used for set/get DAC palette }
       blue, green, red, align: byte;
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

var

  BytesPerLine: word;              { Number of bytes per scanline }
  YOffset : word;                  { Pixel offset for VESA page flipping }

  { window management }
  ReadWindow : byte;      { Window number for reading. }
  WriteWindow: byte;      { Window number for writing. }
  winReadSeg : word;      { Address of segment for read  }
  winWriteSeg: word;      { Address of segment for writes}
  CurrentReadBank : smallint; { active read bank          }
  CurrentWriteBank: smallint; { active write bank         }

  BankShift : word;       { address to shift by when switching banks. }

  ScanLines: word;        { maximum number of scan lines for mode }


  function getVESAInfo(var VESAInfo: TVESAInfo) : boolean;
   var
    st : string[4];
    regs : Registers;
    i: smallint;
   begin
    { Get VESA Mode information ... }
    regs.ax := $4f00;
    regs.es := Seg(VESAInfo);
    regs.di := Ofs(VESAInfo);
    Intr($10, regs);
    St:=VESAInfo.signature;
    if st<>'VESA' then
     begin
{$ifdef logging}
         LogLn('No VESA detected.');
{$endif logging}
         getVesaInfo := FALSE;
         exit;
     end
    else
      getVesaInfo := TRUE;

    i:=0;
    while VESAInfo.ModeList^[i]<> $ffff do
     begin
{$ifdef logging}
      LogLn('Found mode $'+hexstr(VESAInfo.ModeList^[i],4));
{$endif loggin}
      Inc(i);
     end;
{$ifdef logging}
    LogLn(strf(i) + ' modes found.');
{$endif logging}
   end;

  function getVESAModeInfo(var ModeInfo: TVESAModeInfo;mode:word):boolean;
   var
    regs : Registers;
   begin
    { we have to init everything to zero, since VBE < 1.1  }
    { may not setup fields correctly.                      }
    FillChar(ModeInfo, sizeof(ModeInfo), #0);
    { call VESA mode information...}
    regs.ax := $4f01;
    regs.es := Seg(ModeInfo);
    regs.di := Ofs(ModeInfo);
    regs.cx := mode;
    Intr($10, regs);
    if regs.ax <> $4f then
      getVESAModeInfo := FALSE
    else
      getVESAModeInfo := TRUE;
   end;


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
          If GetVESAModeInfo(VESAModeInfo, Mode) And
             ((VESAModeInfo.attr and modeAvail) <> 0) then
            ModeSupported := TRUE
          else
            ModeSupported := FALSE;
        end;
       SearchVESAModes := ModeSupported;
    end;

procedure SetBankIndex(win: byte; BankNr: smallint);
{I don't know why but the previous assembler version changed by some mechanism
 unknown to me some places in memory what lead to changing some variables not
 belonging to this procedure (Laaca)}
var r:Registers;
begin
  r.ax:=$4f05;
  r.bx:=win;
  r.dx:=BankNr;
  Intr($10,r);
end;

  {********************************************************}
  { There are two routines for setting banks. This may in  }
  { in some cases optimize a bit some operations, if the   }
  { hardware supports it, because one window is used for   }
  { reading and one window is used for writing.            }
  {********************************************************}
  procedure SetReadBank(BankNr: smallint);
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

  procedure SetWriteBank(BankNr: smallint);
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

  procedure PutPixVESA256(x, y : smallint; color : word); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     { verify clipping and then convert to absolute coordinates...}
     if ClipPixels then
     begin
       if (X < 0) or (X > ViewWidth) then
         exit;
       if (Y < 0) or (Y > ViewHeight) then
         exit;
     end;
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     Y := Y + YOffset; { adjust pixel for correct virtual page }
     offs := longint(y) * BytesPerLine + x;
       begin
         SetWriteBank(smallint(offs shr 16));
         mem[WinWriteSeg : word(offs)] := byte(color);
       end;
  end;

  procedure DirectPutPixVESA256(x, y : smallint); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
     col : byte;
  begin
     offs := (longint(y) + YOffset) * BytesPerLine + x;
     Case CurrentWriteMode of
       XorPut:
         Begin
           SetReadBank(smallint(offs shr 16));
           col := mem[WinReadSeg : word(offs)] xor byte(CurrentColor);
         End;
       AndPut:
         Begin
           SetReadBank(smallint(offs shr 16));
           col := mem[WinReadSeg : word(offs)] And byte(CurrentColor);
         End;
       OrPut:
         Begin
           SetReadBank(smallint(offs shr 16));
           col := mem[WinReadSeg : word(offs)] or byte(currentcolor);
         End
       else
         Begin
           If CurrentWriteMode <> NotPut then
             col := Byte(CurrentColor)
           else col := Not(Byte(CurrentColor));
         End
     End;
     SetWriteBank(smallint(offs shr 16));
     mem[WinWriteSeg : word(offs)] := Col;
  end;

  function GetPixVESA256(x, y : smallint): word; {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort + YOffset;
     offs := longint(y) * BytesPerLine + x;
     SetReadBank(smallint(offs shr 16));
     GetPixVESA256:=mem[WinReadSeg : word(offs)];
  end;

  Procedure GetScanLineVESA256(x1, x2, y: smallint; var data); {$ifndef fpc}far;{$endif}
  var offs: Longint;
      l, amount, bankrest, index, pixels: longint;
      curbank: smallint;
  begin
    inc(x1,StartXViewPort);
    inc(x2,StartXViewPort);
    {$ifdef logging}
    LogLn('getscanline256 '+strf(x1)+' - '+strf(x2)+' at '+strf(y+StartYViewPort));
    {$endif logging}
    index := 0;
    amount := x2-x1+1;
    Offs:=(Longint(y)+StartYViewPort+YOffset)*bytesperline+x1;
    Repeat
      curbank := smallint(offs shr 16);
      SetReadBank(curbank);
      {$ifdef logging}
      LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
      {$endif logging}
      If ((amount >= 4) and
          ((offs and 3) = 0)) or
         (amount >= 4+4-(offs and 3)) Then
      { align target }
        Begin
          If (offs and 3) <> 0 then
          { this cannot go past a window boundary bacause the }
          { size of a window is always a multiple of 4        }
            Begin
              {$ifdef logging}
              LogLn('Aligning by reading '+strf(4-(offs and 3))+' pixels');
              {$endif logging}
              for l := 1 to 4-(offs and 3) do
                WordArray(Data)[index+l-1] :=
                  Mem[WinReadSeg:word(offs)+l-1];
              inc(index, l);
              inc(offs, l);
              dec(amount, l);
            End;
          {$ifdef logging}
          LogLn('Offset is now '+hexstr(offs,8)+', amount left: '+strf(amount));
          {$endif logging}
          { offs is now 4-bytes aligned }
          If amount <= ($10000-(Offs and $ffff)) Then
             bankrest := amount
          else {the rest won't fit anymore in the current window }
            bankrest := $10000 - (Offs and $ffff);
          { it is possible that by aligning, we ended up in a new }
          { bank, so set the correct bank again to make sure      }
          setreadbank(offs shr 16);
          {$ifdef logging}
          LogLn('Rest to be read from this window: '+strf(bankrest));
          {$endif logging}
          For l := 0 to (Bankrest div 4)-1 Do
            begin
              pixels := MemL[WinReadSeg:word(offs)+l*4];
              WordArray(Data)[index+l*4] := pixels and $ff;
              pixels := pixels shr 8;
              WordArray(Data)[index+l*4+1] := pixels and $ff;
              pixels := pixels shr 8;
              WordArray(Data)[index+l*4+2] := pixels and $ff;
              pixels := pixels shr 8;
              WordArray(Data)[index+l*4+3] := pixels{ and $ff};
            end;
          inc(index,l*4+4);
          inc(offs,l*4+4);
          dec(amount,l*4+4);
          {$ifdef logging}
          LogLn('Offset is now '+hexstr(offs,8)+', amount left: '+strf(amount));
          {$endif logging}
        End
      Else
        Begin
          {$ifdef logging}
          LogLn('Leftover: '+strf(amount)+' at offset '+hexstr(offs,8));
          {$endif logging}
          For l := 0 to amount - 1 do
            begin
              { this may cross a bank at any time, so adjust          }
              { because this loop alwys runs for very little pixels,  }
              { there's little gained by splitting it up              }
              setreadbank(offs shr 16);
              WordArray(Data)[index+l] := mem[WinReadSeg:word(offs)];
              inc(offs);
            end;
          amount := 0
        End
    Until amount = 0;
  end;

  procedure HLineVESA256(x,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}

   var Offs: Longint;
       mask, l, bankrest: longint;
       curbank, hlength: smallint;
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
    {$ifdef logging2}
    LogLn('hline '+strf(x)+' - '+strf(x2)+' on '+strf(y)+' in mode '+strf(currentwritemode));
    {$endif logging2}
    HLength := x2 - x + 1;
    {$ifdef logging2}
    LogLn('length: '+strf(hlength));
    {$endif logging2}
    if HLength>0 then
      begin
         Offs:=(Longint(y)+YOffset)*bytesperline+x;
         {$ifdef logging2}
         LogLn('Offs: '+strf(offs)+' -- '+hexstr(offs,8));
         {$endif logging2}
         Mask := byte(CurrentColor)+byte(CurrentColor) shl 8;
         Mask := Mask + Mask shl 16;
         Case CurrentWriteMode of
           AndPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging2}
                 If ((HLength >= 4) and
                     ((offs and 3) = 0)) or
                    (HLength >= 4+4-(offs and 3)) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging2}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] :=
                             Mem[WinReadSeg:word(offs)+l-1] And Byte(CurrentColor);
                         Dec(HLength, l);
                         inc(offs, l);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] And Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     For l := 0 to HLength - 1 do
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { becauese this loop alwys runs for very little pixels, }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         Mem[WinWriteSeg:word(offs)] :=
                           Mem[WinReadSeg:word(offs)] And byte(currentColor);
                         inc(offs);
                       end;
                     HLength := 0
                   End
               Until HLength = 0;
             End;
           XorPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If ((HLength >= 4) and
                     ((offs and 3) = 0)) or
                    (HLength >= 4+4-(offs and 3)) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] :=
                             Mem[WinReadSeg:word(offs)+l-1] Xor Byte(CurrentColor);
                         Dec(HLength, l);
                         inc(offs, l);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] Xor Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     For l := 0 to HLength - 1 do
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop alwys runs for very little pixels,  }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         Mem[WinWriteSeg:word(offs)] :=
                           Mem[WinReadSeg:word(offs)] xor byte(currentColor);
                         inc(offs);
                       end;
                     HLength := 0
                   End
               Until HLength = 0;
             End;
           OrPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If ((HLength >= 4) and
                     ((offs and 3) = 0)) or
                    (HLength >= 4+4-(offs and 3)) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] :=
                             Mem[WinReadSeg:word(offs)+l-1] Or Byte(CurrentColor);
                         Dec(HLength, l);
                         inc(offs, l);
                       End;
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:offs+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] Or Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     For l := 0 to HLength - 1 do
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop alwys runs for very little pixels,  }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         Mem[WinWriteSeg:word(offs)] :=
                           Mem[WinReadSeg:word(offs)] Or byte(currentColor);
                         inc(offs);
                       end;
                     HLength := 0
                   End
               Until HLength = 0;
             End
           Else
             Begin
               If CurrentWriteMode = NotPut Then
                 Mask := Not(Mask);
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8)+' -- '+strf(offs));
                 {$endif logging}
                 If ((HLength >= 4) and
                     ((offs and 3) = 0)) or
                    (HLength >= 4+4-(offs and 3)) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary bacause the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing '+strf(4-(offs and 3))+' pixels');
                         {$endif logging}
                         for l := 1 to 4-(offs and 3) do
                           Mem[WinWriteSeg:word(offs)+l-1] := Byte(Mask);
                         Dec(HLength, l);
                         inc(offs, l);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= ($10000-(Offs and $ffff)) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := $10000 - (Offs and $ffff);
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest)+' -- '+hexstr(bankrest,8));
                     {$endif logging}
                     For l := 0 to (Bankrest div 4)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] := Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*4+4);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     For l := 0 to HLength - 1 do
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop alwys runs for very little pixels,  }
                         { there's little gained by splitting it up              }
                         setwritebank(offs shr 16);
                         Mem[WinWriteSeg:word(offs)] := byte(mask);
                         inc(offs);
                       end;
                     HLength := 0
                   End
               Until HLength = 0;
             End;
         End;
       end;
   end;

  procedure VLineVESA256(x,y,y2: smallint); {$ifndef fpc}far;{$endif fpc}

   var Offs: Longint;
       l, bankrest: longint;
       curbank, vlength: smallint;
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
    Col := Byte(CurrentColor);
    {$ifdef logging2}
    LogLn('vline '+strf(y)+' - '+strf(y2)+' on '+strf(x)+' in mode '+strf(currentwritemode));
    {$endif logging}
    VLength := y2 - y + 1;
    {$ifdef logging2}
    LogLn('length: '+strf(vlength));
    {$endif logging}
    if VLength>0 then
      begin
         Offs:=(Longint(y)+YOffset)*bytesperline+x;
         {$ifdef logging2}
         LogLn('Offs: '+strf(offs)+' -- '+hexstr(offs,8));
         {$endif logging}
         Case CurrentWriteMode of
           AndPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($ffff-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($ffff - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging2}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] :=
                       Mem[WinReadSeg:word(offs)] And Col;
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging2}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
           XorPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($ffff-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($ffff - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging2}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] :=
                       Mem[WinReadSeg:word(offs)] Xor Col;
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging2}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
           OrPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($ffff-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($ffff - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging2}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] :=
                       Mem[WinReadSeg:word(offs)] Or Col;
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging2}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
           Else
             Begin
               If CurrentWriteMode = NotPut Then
                 Col := Not(Col);
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging}
                 If (VLength-1)*bytesperline <= ($ffff-(Offs and $ffff)) Then
                   bankrest := VLength
                 else {the rest won't fit anymore in the current window }
                   bankrest := (($ffff - (Offs and $ffff)) div bytesperline)+1;
                 {$ifdef logging2}
                 LogLn('Rest to be drawn in this window: '+strf(bankrest));
                 {$endif logging}
                 For l := 0 to Bankrest-1 Do
                   begin
                     Mem[WinWriteSeg:word(offs)] := Col;
                     inc(offs,bytesperline);
                   end;
                 dec(VLength,l+1);
                 {$ifdef logging2}
                 LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(vlength));
                 {$endif logging}
               Until VLength = 0;
             End;
         End;
       end;
   end;

  procedure PatternLineVESA256(x1,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
  {********************************************************}
  { Draws a horizontal patterned line according to the     }
  { current Fill Settings.                                 }
  {********************************************************}
  { Important notes:                                       }
  {  - CurrentColor must be set correctly before entering  }
  {    this routine.                                       }
  {********************************************************}
   type
     TVESA256Fill = Record
       case byte of
         0: (data1, data2: longint);
         1: (pat: array[0..7] of byte);
     end;

   var
    fill: TVESA256Fill;
    bankrest, l : longint;
    offs, amount: longint;
    i           : smallint;
    j           : smallint;
    OldWriteMode : word;
    TmpFillPattern, patternPos : byte;
   begin
     { convert to global coordinates ... }
     x1 := x1 + StartXViewPort;
     x2 := x2 + StartXViewPort;
     y  := y + StartYViewPort;
     { if line was fully clipped then exit...}
     if LineClipped(x1,y,x2,y,StartXViewPort,StartYViewPort,
        StartXViewPort+ViewWidth, StartYViewPort+ViewHeight) then
         exit;
     OldWriteMode := CurrentWriteMode;
     CurrentWriteMode := NormalPut;
     { Get the current pattern }
     TmpFillPattern := FillPatternTable
       [FillSettings.Pattern][((y + startYViewPort) and $7)+1];
     {$ifdef logging2}
     LogLn('patternline '+strf(x1)+' - '+strf(x2)+' on '+strf(y));
     {$endif logging2}
     { how long is the line }
     amount := x2 - x1 + 1;
     { offset to start at }
     offs := (longint(y)+yoffset)*bytesperline+x1;
     { convert the pattern data into the actual color sequence }
     j := 1;
     FillChar(fill,sizeOf(fill),byte(currentBkColor));
     for i := 0 to 7 do
       begin
         if TmpFillPattern and j <> 0 then
           fill.pat[7-i] := currentColor;
{$push}
{$q-}
         j := j shl 1;
{$pop}
       end;
     Repeat
       SetWriteBank(smallint(offs shr 16));
       If (amount > 7) and
          (((offs and 7) = 0) or
           (amount > 7+8-(offs and 7))) Then
         Begin
           { align target }
           If (offs and 7) <> 0 then
           { this cannot go past a window boundary bacause the }
           { size of a window is always a multiple of 8        }
             Begin
               { position in the pattern where to start }
               patternPos := offs and 7;
               {$ifdef logging2}
               LogLn('Aligning by drawing '+strf(8-(offs and 7))+' pixels');
               {$endif logging2}
               for l := 1 to 8-(offs and 7) do
                 begin
                   Mem[WinWriteSeg:word(offs)+l-1] := fill.pat[patternPos and 7];
                   inc(patternPos)
                 end;
               Dec(amount, l);
               inc(offs, l);
             End;
           {$ifdef logging2}
           LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(amount));
           {$endif logging2}
           { offs is now 8-bytes aligned }
           If amount <= ($10000-(Offs and $ffff)) Then
              bankrest := amount
           else {the rest won't fit anymore in the current window }
             bankrest := $10000 - (Offs and $ffff);
           { it is possible that by aligningm we ended up in a new }
           { bank, so set the correct bank again to make sure      }
           setwritebank(offs shr 16);
           {$ifdef logging2}
           LogLn('Rest to be drawn in this window: '+strf(bankrest));
           {$endif logging2}
           for l := 0 to (bankrest div 8)-1 Do
             begin
               MemL[WinWriteSeg:word(offs)+l*8] := fill.data1;
               MemL[WinWriteSeg:word(offs)+l*8+4] := fill.data2;
             end;
           inc(offs,l*8+8);
           dec(amount,l*8+8);
           {$ifdef logging2}
           LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(amount));
           {$endif logging2}
         End
       Else
         Begin
           {$ifdef logging2}
           LogLn('Drawing leftover: '+strf(amount)+' at offset '+hexstr(offs,8));
           {$endif logging2}
           patternPos := offs and 7;
           For l := 0 to amount - 1 do
             begin
               { this may cross a bank at any time, so adjust          }
               { because this loop alwys runs for very little pixels,  }
               { there's little gained by splitting it up              }
               setwritebank(offs shr 16);
               Mem[WinWriteSeg:word(offs)] := fill.pat[patternPos and 7];
               inc(offs);
               inc(patternPos);
             end;
           amount := 0;
         End
     Until amount = 0;
     currentWriteMode := oldWriteMode;
   end;


 {************************************************************************}
 {*                    15/16bit pixels VESA mode routines                *}
 {************************************************************************}

  procedure PutPixVESA32kOr64k(x, y : smallint; color : word); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
     place: word;
     bank : shortint;

  begin
{$ifdef logging}
     logln('putpixvesa32kor64k('+strf(x)+','+strf(y)+')');
{$endif logging}
     { verify clipping and then convert to absolute coordinates...}
     if ClipPixels then
     begin
       if (X < 0) or (X > ViewWidth) then
         exit;
       if (Y < 0) or (Y > ViewHeight) then
         exit;
     end;
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     Y := Y + YOffset; { adjust pixel for correct virtual page }
     offs := longint(y) * BytesPerLine + 2*x;
     bank := offs div 65536;
     place:= offs mod 65536;
     SetWriteBank(bank);

{$ifdef logging}
     logln('putpixvesa32kor64k offset: '+strf(word(offs)));
{$endif logging}
     memW[WinWriteSeg : place] := color;
  end;

  function GetPixVESA32kOr64k(x, y : smallint): word; {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
  begin
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort + YOffset;
     offs := longint(y) * BytesPerLine + 2*x;
     SetReadBank(smallint(offs shr 16));
     GetPixVESA32kOr64k:=memW[WinReadSeg : word(offs)];
  end;

  procedure DirectPutPixVESA32kOr64k(x, y : smallint); {$ifndef fpc}far;{$endif fpc}
  var
     offs : longint;
     bank : smallint;
     place,col : word;
  begin
{$ifdef logging}
     logln('directputpixvesa32kor64k('+strf(x)+','+strf(y)+')');
{$endif logging}
     y:= Y + YOffset;

     offs := longint(y) * BytesPerLine + 2*x;
     bank:=offs div 65536;
     place:=offs mod 65536;

     SetWriteBank(bank and $FF); // unknown why this and $FF is here.
     Case CurrentWriteMode of
       XorPut:
         Begin
           SetReadBank(bank);
           memW[WinWriteSeg : place] := memW[WinReadSeg : place] xor currentcolor;
         End;
       AndPut:
         Begin
           SetReadBank(bank);
           memW[WinWriteSeg : place] := memW[WinReadSeg : place] And currentcolor;
         End;
       OrPut:
         Begin
           SetReadBank(bank);
           memW[WinWriteSeg : place] := memW[WinReadSeg : place] or currentcolor;
         End
       else
         Begin
           If CurrentWriteMode <> NotPut Then
             col := CurrentColor
           Else col := Not(CurrentColor);
{$ifdef logging}
           logln('directputpixvesa32kor64k offset: '+strf(word(offs)));
{$endif logging}
           memW[WinWriteSeg : place] := Col;
         End
     End;
  end;

  procedure HLineVESA32kOr64k(x,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}

   var Offs: Longint;
       mask, l, bankrest: longint;
       curbank, hlength: smallint;
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
    {$ifdef logging2}
    LogLn('hline '+strf(x)+' - '+strf(x2)+' on '+strf(y)+' in mode '+strf(currentwritemode));
    {$endif logging2}
    HLength := x2 - x + 1;
    {$ifdef logging2}
    LogLn('length: '+strf(hlength));
    {$endif logging2}
    if HLength>0 then
      begin
         Offs:=(Longint(y)+YOffset)*bytesperline+2*x;
         {$ifdef logging2}
         LogLn('Offs: '+strf(offs)+' -- '+hexstr(offs,8));
         {$endif logging2}
         Mask := longint(word(CurrentColor))+(longint(word(CurrentColor)) shl 16);
         Case CurrentWriteMode of
           AndPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging2}
                 If ((HLength >= 2) and
                     ((offs and 3) = 0)) or
                    (HLength >= 3) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary because the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing 1 pixel');
                         {$endif logging2}
                         MemW[WinWriteSeg:word(offs)] :=
                           MemW[WinReadSeg:word(offs)] And Word(CurrentColor);
                         Dec(HLength);
                         inc(offs, 2);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= (($10000-(Offs and $ffff)) shr 1) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := ($10000 - (Offs and $ffff)) shr 1;
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 2)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] And Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*2+2);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     if HLength > 0 then
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop always runs for very little pixels, }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         MemW[WinWriteSeg:word(offs)] :=
                           MemW[WinReadSeg:word(offs)] And Word(currentColor);
                         HLength := 0
                       end;
                   End
               Until HLength = 0;
             End;
           XorPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging2}
                 If ((HLength >= 2) and
                     ((offs and 3) = 0)) or
                    (HLength >= 3) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary because the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing 1 pixel');
                         {$endif logging2}
                         MemW[WinWriteSeg:word(offs)] :=
                           MemW[WinReadSeg:word(offs)] Xor Word(CurrentColor);
                         Dec(HLength);
                         inc(offs, 2);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= (($10000-(Offs and $ffff)) shr 1) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := ($10000 - (Offs and $ffff)) shr 1;
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 2)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] Xor Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*2+2);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     if HLength > 0 then
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop always runs for very little pixels, }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         MemW[WinWriteSeg:word(offs)] :=
                           MemW[WinReadSeg:word(offs)] Xor Word(currentColor);
                         HLength := 0
                       end;
                   End
               Until HLength = 0;
             End;
           OrPut:
             Begin
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging2}
                 If ((HLength >= 2) and
                     ((offs and 3) = 0)) or
                    (HLength >= 3) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary because the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing 1 pixel');
                         {$endif logging2}
                         MemW[WinWriteSeg:word(offs)] :=
                           MemW[WinReadSeg:word(offs)] Or Word(CurrentColor);
                         Dec(HLength);
                         inc(offs, 2);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= (($10000-(Offs and $ffff)) shr 1) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := ($10000 - (Offs and $ffff)) shr 1;
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 2)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] :=
                         MemL[WinReadSeg:word(offs)+l*4] Or Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*2+2);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     if HLength > 0 then
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop always runs for very little pixels, }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         MemW[WinWriteSeg:word(offs)] :=
                           MemW[WinReadSeg:word(offs)] Or Word(currentColor);
                         HLength := 0
                       end;
                   End
               Until HLength = 0;
             End
           Else
             Begin
               If CurrentWriteMode = NotPut Then
                 Mask := Not(Mask);
               Repeat
                 curbank := smallint(offs shr 16);
                 SetWriteBank(curbank);
                 SetReadBank(curbank);
                 {$ifdef logging2}
                 LogLn('set bank '+strf(curbank)+' for offset '+hexstr(offs,8));
                 {$endif logging2}
                 If ((HLength >= 2) and
                     ((offs and 3) = 0)) or
                    (HLength >= 3) Then
                 { align target }
                   Begin
                     If (offs and 3) <> 0 then
                     { this cannot go past a window boundary because the }
                     { size of a window is always a multiple of 4        }
                       Begin
                         {$ifdef logging2}
                         LogLn('Aligning by drawing 1 pixel');
                         {$endif logging2}
                         MemW[WinWriteSeg:word(offs)] := Word(Mask);
                         Dec(HLength);
                         inc(offs, 2);
                       End;
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                     { offs is now 4-bytes aligned }
                     If HLength <= (($10000-(Offs and $ffff)) shr 1) Then
                        bankrest := HLength
                     else {the rest won't fit anymore in the current window }
                       bankrest := ($10000 - (Offs and $ffff)) shr 1;
                     { it is possible that by aligningm we ended up in a new }
                     { bank, so set the correct bank again to make sure      }
                     setwritebank(offs shr 16);
                     setreadbank(offs shr 16);
                     {$ifdef logging2}
                     LogLn('Rest to be drawn in this window: '+strf(bankrest));
                     {$endif logging}
                     For l := 0 to (Bankrest div 2)-1 Do
                       MemL[WinWriteSeg:word(offs)+l*4] := Mask;
                     inc(offs,l*4+4);
                     dec(hlength,l*2+2);
                     {$ifdef logging2}
                     LogLn('Offset is now '+hexstr(offs,8)+', length left: '+strf(hlength));
                     {$endif logging}
                   End
                 Else
                   Begin
                     {$ifdef logging2}
                     LogLn('Drawing leftover: '+strf(HLength)+' at offset '+hexstr(offs,8));
                     {$endif logging}
                     if HLength > 0 then
                       begin
                         { this may cross a bank at any time, so adjust          }
                         { because this loop always runs for very little pixels, }
                         { there's little gained by splitting it up              }
                         setreadbank(offs shr 16);
                         setwritebank(offs shr 16);
                         MemW[WinWriteSeg:word(offs)] := Word(Mask);
                         HLength := 0
                       end;
                   End
               Until HLength = 0;
             End;
         End;
       end;
   end;


 {************************************************************************}
 {*                     4-bit pixels VESA mode routines                  *}
 {************************************************************************}

  procedure PutPixVESA16(x, y : smallint; color : word); {$ifndef fpc}far;{$endif fpc}
    var
     offs : longint;
     dummy : byte;
  begin
     { verify clipping and then convert to absolute coordinates...}
     if ClipPixels then
     begin
       if (X < 0) or (X > ViewWidth) then
         exit;
       if (Y < 0) or (Y > ViewHeight) then
         exit;
     end;
     X:= X + StartXViewPort;
     Y:= Y + StartYViewPort;
     Y := Y + YOffset; { adjust pixel for correct virtual page }
     { }
     offs := longint(y) * BytesPerLine + (x div 8);
     SetReadBank(smallint(offs shr 16));
     SetWriteBank(smallint(offs shr 16));

     PortW[$3ce] := $0f01;       { Index 01 : Enable ops on all 4 planes }
     PortW[$3ce] := color shl 8; { Index 00 : Enable correct plane and write color }

     { Index 08 : Bitmask register.          }
     PortW[$3ce] := ($8000 shr (x and $7)) or 8; { Select correct bits to modify }

     dummy := Mem[WinReadSeg: word(offs)];  { Latch the data into host space.  }
     Mem[WinWriteSeg: word(offs)] := dummy;  { Write the data into video memory }
     PortW[$3ce] := $ff08;         { Enable all bit planes.           }
     PortW[$3ce] := $0001;         { Index 01 : Disable ops on all four planes.         }
     { }
  end;


 Function GetPixVESA16(X,Y: smallint):word; {$ifndef fpc}far;{$endif fpc}
 Var dummy: Word;
     offset: longint;
     shift: byte;
  Begin
    X:= X + StartXViewPort;
    Y:= Y + StartYViewPort + YOffset;
    offset := longint(Y) * BytesPerLine + (x div 8);
    SetReadBank(smallint(offset shr 16));
    PortW[$3ce] := $0004;
    shift := 7 - (X and 7);
    dummy := (Mem[WinReadSeg:word(offset)] shr shift) and 1;
    Port[$3cf] := 1;
    dummy := dummy or (((Mem[WinReadSeg:word(offset)] shr shift) and 1) shl 1);
    Port[$3cf] := 2;
    dummy := dummy or (((Mem[WinReadSeg:word(offset)] shr shift) and 1) shl 2);
    Port[$3cf] := 3;
    dummy := dummy or (((Mem[WinReadSeg:word(offset)] shr shift) and 1) shl 3);
    GetPixVESA16 := dummy;
  end;


  procedure DirectPutPixVESA16(x, y : smallint); {$ifndef fpc}far;{$endif fpc}
    var
     offs : longint;
     dummy : byte;
     Color : word;
  begin
     If CurrentWriteMode <> NotPut Then
       Color := CurrentColor
     else Color := not CurrentColor;

     case CurrentWriteMode of
        XORPut:
          PortW[$3ce]:=((3 shl 3) shl 8) or 3;
        ANDPut:
          PortW[$3ce]:=((1 shl 3) shl 8) or 3;
        ORPut:
          PortW[$3ce]:=((2 shl 3) shl 8) or 3;
        {not needed, this is the default state (e.g. PutPixel16 requires it)}
        {NormalPut, NotPut:
          PortW[$3ce]:=$0003
        else
          PortW[$3ce]:=$0003}
     end;

     Y := Y + YOffset;
     offs := longint(y) * BytesPerLine + (x div 8);
     SetReadBank(smallint(offs shr 16));
     SetWriteBank(smallint(offs shr 16));
     PortW[$3ce] := $0f01;       { Index 01 : Enable ops on all 4 planes }
     PortW[$3ce] := color shl 8; { Index 00 : Enable correct plane and write color }

     { Index 08 : Bitmask register.          }
     PortW[$3ce] := ($8000 shr (x and $7)) or 8; { Select correct bits to modify }

     dummy := Mem[WinReadSeg: word(offs)];  { Latch the data into host space.  }
     Mem[WinWriteSeg: word(offs)] := dummy;  { Write the data into video memory }
     PortW[$3ce] := $ff08;         { Enable all bit planes.           }
     PortW[$3ce] := $0001;         { Index 01 : Disable ops on all four planes.         }
     if (CurrentWriteMode = XORPut) or
        (CurrentWriteMode = ANDPut) or
        (CurrentWriteMode = ORPut) then
       PortW[$3ce] := $0003;
  end;


  procedure HLineVESA16(x,x2,y: smallint); {$ifndef fpc}far;{$endif fpc}
  var
      xtmp: smallint;
      ScrOfs, BankRest: longint;
      HLength : word;
      LMask,RMask : byte;
  begin

    { must we swap the values? }
    if x > x2 then
      Begin
        xtmp := x2;
        x2 := x;
        x:= xtmp;
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
    Y := Y + YOffset;
    ScrOfs := longint(y) * BytesPerLine + (x div 8);
    SetReadBank(smallint(ScrOfs shr 16));
    SetWriteBank(smallint(ScrOfs shr 16));
    HLength:=x2 div 8-x div 8;
    LMask:=$ff shr (x and 7);
{$push}
{$r-}
{$q-}
    RMask:=$ff shl (7-(x2 and 7));
{$pop}
    if HLength=0 then
      LMask:=LMask and RMask;
    If CurrentWriteMode <> NotPut Then
      PortW[$3ce]:= CurrentColor shl 8
    else PortW[$3ce]:= (not CurrentColor) shl 8;
    PortW[$3ce]:=$0f01;
    case CurrentWriteMode of
       XORPut:
         PortW[$3ce]:=((3 shl 3) shl 8) or 3;
       ANDPut:
         PortW[$3ce]:=((1 shl 3) shl 8) or 3;
       ORPut:
         PortW[$3ce]:=((2 shl 3) shl 8) or 3;
       NormalPut, NotPut:
         PortW[$3ce]:=$0003
       else
         PortW[$3ce]:=$0003
    end;

    PortW[$3ce]:=(LMask shl 8) or 8;
{$push}
{$r-}
{$q-}
    Mem[WinWriteSeg:word(ScrOfs)]:=Mem[WinReadSeg:word(ScrOfs)]+1;
{$pop}
    {Port[$3ce]:=8;}{not needed, the register is already selected}
    if HLength>0 then
      begin
         dec(HLength);
         inc(ScrOfs);
         while (HLength>0) do
           begin
              SetReadBank(smallint(ScrOfs shr 16));
              SetWriteBank(smallint(ScrOfs shr 16));
              Port[$3cf]:=$ff;
              if HLength <= ($10000-(ScrOfs and $ffff)) Then
                 BankRest := HLength
              else {the rest won't fit anymore in the current window }
                BankRest := $10000 - (ScrOfs and $ffff);
{$ifndef tp}
              seg_bytemove(WinReadSeg,word(ScrOfs),WinWriteSeg,word(ScrOfs),BankRest);
{$else}
              move(Ptr(WinReadSeg,word(ScrOfs))^, Ptr(WinWriteSeg,word(ScrOfs))^, BankRest);
{$endif}
              ScrOfs := ScrOfs + BankRest;
              HLength := HLength - BankRest;
           end;
         SetReadBank(smallint(ScrOfs shr 16));
         SetWriteBank(smallint(ScrOfs shr 16));
         Port[$3cf]:=RMask;
{$push}
{$r-}
{$q-}
         Mem[WinWriteSeg:word(ScrOfs)]:=Mem[WinReadSeg:word(ScrOfs)]+1;
{$pop}
      end;
    { clean up }
    {Port[$3cf]:=0;}{not needed, the register is reset by the next operation:}
    PortW[$3ce]:=$ff08;
    PortW[$3ce]:=$0001;
    PortW[$3ce]:=$0003;
   end;




 {************************************************************************}
 {*                     VESA Palette entries                             *}
 {************************************************************************}


   Procedure SetVESARGBAllPalette(const Palette:PaletteType);
    var
     pal: array[0..255] of palrec;
     regs: Registers;
     c: smallint;
     FunctionNr : byte;   { use blankbit or normal RAMDAC programming? }
    begin
      if DirectColor then
        Begin
          _GraphResult := grError;
          exit;
        end;
      { use the set/get palette function }
      if VESAInfo.Version >= $0200 then
        Begin
          { check if blanking bit must be set when programming }
          { the RAMDAC.                                        }
          if (VESAInfo.caps and attrSnowCheck) <> 0 then
            FunctionNr := $80
          else
            FunctionNr := $00;

          fillChar(pal,sizeof(pal),0);
          { Convert to vesa format }
          for c := 0 to 255 do
            begin
              pal[c].red := byte(palette.colors[c].red);
              pal[c].green := byte(palette.colors[c].green);
              pal[c].blue := byte(palette.colors[c].blue);
            end;

          { copy palette values to real mode buffer }
          regs.ax := $4F09;
          regs.bx := FunctionNr;
          regs.cx := 256;
          regs.dx := 0;
          regs.es := Seg(pal);
          regs.di := Ofs(pal);
          Intr($10, regs);

          if regs.ax <> $004F then
            begin
              _GraphResult := grError;
              exit;
            end;
        end
      else
        { assume it's fully VGA compatible palette-wise. }
        Begin
          SetVGARGBAllPalette(palette);
        end;
      setallpalettedefault(palette);
    end;

   Procedure SetVESARGBPalette(ColorNum, RedValue, GreenValue,
      BlueValue : smallint);
    var
     pal: palrec;
     regs: Registers;
     FunctionNr : byte;   { use blankbit or normal RAMDAC programming? }
    begin
      if DirectColor then
        Begin
{$ifdef logging}
          logln('setvesargbpalette called with directcolor = true');
{$endif logging}
          _GraphResult := grError;
          exit;
        end;
        pal.align := 0;
        pal.red := byte(RedValue) shr 2;
        pal.green := byte(GreenValue) shr 2;
        pal.blue := byte(BlueValue) shr 2;
        { use the set/get palette function }
        if VESAInfo.Version >= $0200 then
          Begin
            { check if blanking bit must be set when programming }
            { the RAMDAC.                                        }
            if (VESAInfo.caps and attrSnowCheck) <> 0 then
              FunctionNr := $80
            else
              FunctionNr := $00;

            regs.ax := $4F09;
            regs.bx := FunctionNr;
            regs.cx := $01;
            regs.dx := ColorNum;
            regs.es := Seg(pal);
            regs.di := Ofs(pal);
            Intr($10, regs);

            if regs.ax <> $004F then
              begin
{$ifdef logging}
                logln('setvesargbpalette failed while directcolor = false!');
{$endif logging}
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


  Procedure GetVESARGBPalette(ColorNum: smallint; Var
      RedValue, GreenValue, BlueValue : smallint);
   var
    pal: PalRec;
    regs : Registers;
   begin
      if DirectColor then
        Begin
{$ifdef logging}
         logln('getvesargbpalette called with directcolor = true');
{$endif logging}
          _GraphResult := grError;
          exit;
        end;
        { use the set/get palette function }
        if VESAInfo.Version >= $0200 then
          Begin
            regs.ax := $4F09;
            regs.bx := $01;       { get palette data      }
            regs.cx := $01;
            regs.dx := ColorNum;
            regs.es := Seg(pal);
            regs.di := Ofs(pal);
            Intr($10, regs);

            if regs.ax <> $004F then
              begin
{$ifdef logging}
                logln('getvesargbpalette failed while directcolor = false!');
{$endif logging}
                _GraphResult := grError;
                exit;
              end
            else
              begin
                RedValue := smallint(pal.Red);
                GreenValue := smallint(pal.Green);
                BlueValue := smallint(pal.Blue);
              end;
          end
        else
            GetVGARGBPalette(ColorNum, RedValue, GreenValue, BlueValue);
   end;



  procedure SetupWindows(var ModeInfo: TVESAModeInfo);
   begin
     BytesPerLine := VESAModeInfo.BytesPerScanLine;

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
         YOffset := 0;
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
        res: boolean;
  begin
   { Init mode information, for compatibility with VBE < 1.1 }
   FillChar(VESAModeInfo, sizeof(TVESAModeInfo), #0);
   { get the video mode information }
   if getVESAModeInfo(VESAmodeinfo, mode) then
   begin
     { checks if the hardware supports the video mode. }
     if ((VESAModeInfo.attr and modeAvail) = 0) or
        ((VESAModeInfo.Attr and ModeNoWindowed) <> 0) then
       begin
         SetVESAmode := FALSE;
{$ifdef logging}
         logln('  vesa mode '+strf(mode)+' not supported!!!');
{$endif logging}
         _GraphResult := grError;
         exit;
       end;

     SetVESAMode := TRUE;
     BankShift := 0;
     while (64 shr BankShift) <> VESAModeInfo.WinGranularity do
        Inc(BankShift);
     CurrentWriteBank := -1;
     CurrentReadBank := -1;
{    nickysn: setting BytesPerLine moved to SetupLinear and SetupWindowed
     BytesPerLine := VESAModeInfo.BytesPerScanLine;}

     { These are the window adresses ... }
     WinWriteSeg := 0;  { This is the segment to use for writes }
     WinReadSeg := 0;   { This is the segment to use for reads  }
     ReadWindow := 0;
     WriteWindow := 0;

     SetUpWindows(VESAModeInfo);

{$ifdef logging}
  LogLn('Entering vesa mode '+strf(mode));
  LogLn('Read segment: $'+hexstr(winreadseg,4));
  LogLn('Write segment: $'+hexstr(winwriteseg,4));
  LogLn('Window granularity: '+strf(VESAModeInfo.WinGranularity)+'kb');
  LogLn('Window size: '+strf(VESAModeInfo.winSize)+'kb');
  LogLn('Bytes per line: '+strf(bytesperline));
{$endif logging}

   asm
    mov ax,4F02h
    mov bx,mode
{$ifdef fpc}
    push bp
    push si
    push di
    push bx
{$endif fpc}
    int 10h
{$ifdef fpc}
    pop bx
    pop di
    pop si
    pop bp
{$endif fpc}
    sub ax,004Fh
    cmp ax,1
    sbb al,al
    mov res,al
   end ['BX','AX'];
   if not res then
     _GraphResult := GrNotDetected
   else _GraphResult := grOk;
  end;
 end;



 {************************************************************************}
 {*                     VESA Modes inits                                 *}
 {************************************************************************}

  {******************************************************** }
  { Function GetMaxScanLines()                              }
  {-------------------------------------------------------- }
  { This routine returns the maximum number of scan lines   }
  { possible for this mode. This is done using the Get      }
  { Scan Line length VBE function.                          }
  {******************************************************** }
  function GetMaxScanLines: word;
   var
    regs : Registers;
   begin
     regs.ax := $4f06; {_ setup function      }
     regs.bx := $0001; { get scan line length }
     Intr($10, regs);
     GetMaxScanLines := regs.dx;
   end;

 procedure Init1280x1024x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVesaMode(m1280x1024x64k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init1280x1024x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1280x1024x32k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init1280x1024x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1280x1024x256);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;


 procedure Init1280x1024x16; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1280x1024x16);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init1024x768x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x64k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init1024x768x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x32k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init1024x768x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x256);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init1024x768x16; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m1024x768x16);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init800x600x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m800x600x64k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init800x600x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m800x600x32k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init800x600x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m800x600x256);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init800x600x16; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVesaMode(m800x600x16);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init640x480x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x480x64k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init640x480x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x480x32k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init640x480x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x480x256);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init640x400x256; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m640x400x256);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init320x200x64k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m320x200x64k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;

 procedure Init320x200x32k; {$ifndef fpc}far;{$endif fpc}
  begin
    SetVESAMode(m320x200x32k);
    { Get maximum number of scanlines for page flipping }
    ScanLines := GetMaxScanLines;
  end;



 Procedure SaveStateVESA; {$ifndef fpc}far;{$endif fpc}
 var
  regs: Registers;
  begin
    SaveSupported := FALSE;
    SavePtr := nil;
{$ifdef logging}
        LogLn('Get the video mode...');
{$endif logging}
    { Get the video mode }
    regs.ah:=$0f;
    intr($10,regs);
    VideoMode:=regs.al;
    { saving/restoring video state screws up Windows (JM) }
    if inWindows then
      exit;
{$ifdef logging}
        LogLn('Prepare to save VESA video state');
{$endif logging}
    { Prepare to save video state...}
    regs.ax:=$4F04;        { get buffer size to save state }
    regs.dx:=$00;
    regs.cx:=%00001111;    { Save DAC / Data areas / Hardware states }
    intr($10,regs);
    StateSize:=regs.bx;
    SaveSupported:=(regs.al=$4f);
    if SaveSupported then
      begin
{$ifdef logging}
        LogLn('allocating VESA save buffer of '+strf(64*StateSize));
{$endif logging}
        GetMem(SavePtr, 64*StateSize); { values returned in 64-byte blocks }
        if not assigned(SavePtr) then
           RunError(203);
        { call the real mode interrupt ... }
        regs.ax := $4F04;      { save the state buffer                   }
        regs.cx := $0F;        { Save DAC / Data areas / Hardware states }
        regs.dx := $01;        { save state                              }
        regs.es := Seg(SavePtr^);
        regs.bx := Ofs(SavePtr^);
        Intr($10,regs);
        { restore state, according to Ralph Brown Interrupt list }
        { some BIOS corrupt the hardware after a save...         }
        regs.ax := $4F04;      { restore the state buffer                }
        regs.cx := $0F;        { rest DAC / Data areas / Hardware states }
        regs.dx := $02;
        regs.es := Seg(SavePtr^);
        regs.bx := Ofs(SavePtr^);
        Intr($10,regs);
      end;
  end;

 procedure RestoreStateVESA; {$ifndef fpc}far;{$endif fpc}
  var
   regs:Registers;
   SavePtrCopy: Pointer;
  begin
     { go back to the old video mode...}
     regs.ax:=VideoMode;
     intr($10,regs);
     { then restore all state information }
     if assigned(SavePtr) and SaveSupported then
       begin
         regs.ax := $4F04;      { restore the state buffer                }
         regs.cx := $0F;        { rest DAC / Data areas / Hardware states }
         regs.dx := $02;        { restore state                           }
         regs.es := Seg(SavePtr^);
         regs.bx := Ofs(SavePtr^);
         Intr($10,regs);

         SavePtrCopy := SavePtr;
         SavePtr := nil;
         FreeMem(SavePtrCopy, 64*StateSize);
       end;
  end;



 {************************************************************************}
 {*                     VESA Page flipping routines                      *}
 {************************************************************************}
 { Note: These routines, according  to the VBE3 specification, will NOT   }
 { work with the 24 bpp modes, because of the alignment.                  }
 {************************************************************************}

  {******************************************************** }
  { Procedure SetVisualVESA()                               }
  {-------------------------------------------------------- }
  { This routine changes the page which will be displayed   }
  { on the screen, since the method has changed somewhat    }
  { between VBE versions , we will use the old method where }
  { the new pixel offset is used to display different pages }
  {******************************************************** }
 procedure SetVisualVESA(page: word); {$ifndef fpc}far;{$endif fpc}
  var
   newStartVisible : word;
  begin
    if page > HardwarePages then
      begin
        _graphresult := grError;
        exit;
      end;
    newStartVisible := (MaxY+1)*page;
    if newStartVisible > ScanLines then
      begin
        _graphresult := grError;
        exit;
      end;
    asm
      mov ax, 4f07h
      mov bx, 0000h   { set display start }
      mov cx, 0000h   { pixel zero !      }
      mov dx, [NewStartVisible]  { new scanline }
{$ifdef fpc}
      push    bp
      push    si
      push    di
      push    bx
{$endif}
      int     10h
{$ifdef fpc}
      pop     bx
      pop     di
      pop     si
      pop     bp
{$endif}
    end ['DX','CX','BX','AX'];
  end;

 procedure SetActiveVESA(page: word); {$ifndef fpc}far;{$endif fpc}
  begin
    { video offset is in pixels under VESA VBE! }
    { This value is reset after a mode set to page ZERO = YOffset = 0 ) }
    if page > HardwarePages then
      begin
        _graphresult := grError;
        exit;
      end;
    YOffset := (MaxY+1)*page;
  end;

