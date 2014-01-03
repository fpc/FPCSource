{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Borland Pascal 7 Compatible CRT Unit - Go32V2 implementation

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit crt;

{$GOTO on}

interface

{$i crth.inc}

Var
  ScreenWidth,
  ScreenHeight : word;

implementation

uses
  dos;

{$ASMMODE INTEL}

var
  DelayCnt : Longint;
  VidSeg : Word;

{****************************************************************************
                           Low level Routines
****************************************************************************}

procedure dosmemfillword(segm, ofs: Word; count: Word; w: Word); assembler;
asm
  mov ax, segm
  mov es, ax
  mov di, ofs
  mov ax, w
  mov cx, count
  rep stosw
end;

procedure dosmemmove(sseg, sofs, dseg, dofs: Word; count: Word); assembler;
asm
  mov ax, dseg
  mov es, ax
  mov di, dofs
  mov si, sofs
  mov dx, count
  mov cx, dx
  mov ax, sseg
  push ds
  mov ds, ax
  shr cx, 1
  jz @@1
  rep movsw
@@1:
  and dl, 1
  jz @@2
  rep movsb
@@2:
  pop ds
end;

procedure setscreenmode(mode : byte);
var
  regs : registers;
begin
  regs.ax:=mode;
  intr($10,regs);
end;


function GetScreenHeight : word;
begin
  getscreenheight:=mem[$40:$84]+1;
  If mem[$40:$84]=0 then
    getscreenheight := 25;
end;


function GetScreenWidth : word;
begin
  getscreenwidth:=memw[$40:$4a];
end;


procedure SetScreenCursor(x,y : smallint);
var
  regs : registers;
begin
  regs.ax:=$0200;
  regs.bx:=0;
  regs.dx:=(y-1) shl 8+(x-1);
  intr($10,regs);
end;


procedure GetScreenCursor(var x,y : smallint);
begin
  x:=mem[$40:$50]+1;
  y:=mem[$40:$51]+1;
end;


procedure DetectSnow;
var
  regs: Registers;
  ega_switches: Byte;
begin
  { the CGA snow bug exists only in 80x25 text modes. The 40x25 text modes and
    the graphics modes have no snow even on a true CGA. The monochrome 80x25
    text mode (lastmode=7) is not supported by CGA and is not snowy on every
    video card that support it (MDA, Hercules, EGA, VGA+) }
  if (lastmode<>2) and (lastmode<>3) then
  begin
    CheckSnow:=false;
    exit;
  end;

  { MCGA/VGA+ test }
  regs.ax:=$1A00;
  intr($10,regs);
  { function supported? }
  if regs.al=$1A then
    begin
      { at this point we have established that an MCGA or VGA+ card is present
        in the system. However there could still be two video cards present
        (i.e. an oldschool dual monitor configuration), and one of them could be
        a CGA, so check BL (=active display code) as well. }
      CheckSnow:=regs.bl=2;
      exit;
    end;

  { EGA test }
  regs.ah:=$12;
  regs.bx:=$FF10;
  intr($10,regs);
  { function supported? }
  if regs.bh<>$FF then
    begin
      ega_switches:=regs.cl and $0f;
      { in all the following cases a CGA card is also present and the EGA only
        works in monochrome mode, but we've already checked that we're not in a
        monochrome text mode (because lastmode<>7), so it must be the CGA
        currently active }
      CheckSnow:=(ega_switches=4)   { primary CGA 40x25, secondary EGA+ 80x25 mono }
              or (ega_switches=5)   { primary CGA 80x25, secondary EGA+ 80x25 mono }
              or (ega_switches=10)  { primary EGA+ 80x25 mono, secondary CGA 40x25 (optional) }
              or (ega_switches=11); { primary EGA+ 80x25 mono, secondary CGA 80x25 (optional) }
      exit;
    end;

  CheckSnow:=true;
end;


{****************************************************************************
                              Helper Routines
****************************************************************************}

var
  WinMin: packed record
    X, Y: Byte;
  end absolute WindMin;

  WinMax: packed record
    X, Y: Byte;
  end absolute WindMax;


Function FullWin:boolean;
{
  Full Screen 80x25? Window(1,1,80,25) is used, allows faster routines
}
begin
  FullWin:=(WinMin.X=0) and (WinMin.Y=0) and
           (word(WinMax.X+1)=ScreenWidth) and (word(WinMax.Y+1)=ScreenHeight);
end;


{****************************************************************************
                             Public Crt Functions
****************************************************************************}


procedure textmode (Mode: word);

var
   regs : registers;

begin
  lastmode:=mode;
  mode:=mode and $ff;
  setscreenmode(mode);

  { set 8x8 font }
  if (lastmode and $100)<>0 then
    begin
       regs.ax:=$1112;
       regs.bx:=$0;
       intr($10,regs);
    end;

  screenwidth:=getscreenwidth;
  screenheight:=getscreenheight;
  windmin:=0;
  windmax:=(screenwidth-1) or ((screenheight-1) shl 8);

  DetectSnow;
end;


Procedure TextColor(Color: Byte);
{
  Switch foregroundcolor
}
Begin
  TextAttr:=(Color and $f) or (TextAttr and $70);
  If (Color>15) Then TextAttr:=TextAttr Or Blink;
End;



Procedure TextBackground(Color: Byte);
{
  Switch backgroundcolor
}
Begin
  TextAttr:=((Color shl 4) and ($f0 and not Blink)) or (TextAttr and ($0f OR Blink) );
End;



Procedure HighVideo;
{
  Set highlighted output.
}
Begin
  TextColor(TextAttr Or $08);
End;



Procedure LowVideo;
{
  Set normal output
}
Begin
  TextColor(TextAttr And $77);
End;



Procedure NormVideo;
{
  Set normal back and foregroundcolors.
}
Begin
  TextColor(7);
  TextBackGround(0);
End;


Procedure GotoXy(X: tcrtcoord; Y: tcrtcoord);
{
  Go to coordinates X,Y in the current window.
}
Begin
  If (X>0) and (X<=WinMax.X- WinMin.X+1) and
     (Y>0) and (Y<=WinMax.Y-WinMin.Y+1) Then
   Begin
     Inc(X,WinMin.X);
     Inc(Y,WinMin.Y);
     SetScreenCursor(x,y);
   End;
End;


Procedure Window(X1, Y1, X2, Y2: Byte);
{
  Set screen window to the specified coordinates.
}
Begin
  if (X1>X2) or (word(X2)>ScreenWidth) or
     (Y1>Y2) or (word(Y2)>ScreenHeight) then
   exit;
  WindMin:=((Y1-1) Shl 8)+(X1-1);
  WindMax:=((Y2-1) Shl 8)+(X2-1);
  GoToXY(1,1);
End;


Procedure ClrScr;
{
  Clear the current window, and set the cursor on 1,1
}
var
  fil : word;
  y   : word;
begin
  fil:=32 or (textattr shl 8);
  if FullWin then
   DosmemFillWord(VidSeg,0,ScreenHeight*ScreenWidth,fil)
  else
   begin
     for y:=WinMin.Y to WinMax.Y do
      DosmemFillWord(VidSeg,(y*ScreenWidth+word(WinMin.X))*2,WinMax.X-WinMin.X+1,fil);
   end;
  Gotoxy(1,1);
end;


Procedure ClrEol;
{
  Clear from current position to end of line.
}
var
  x,y : smallint;
  fil : word;
Begin
  GetScreenCursor(x,y);
  fil:=32 or (textattr shl 8);
  if x<=(WinMax.X+1) then
   DosmemFillword(VidSeg,(word(y-1)*ScreenWidth+word(x-1))*2,WinMax.X-x+2,fil);
End;



Function WhereX: tcrtcoord;
{
  Return current X-position of cursor.
}
var
  x,y : smallint;
Begin
  GetScreenCursor(x,y);
  WhereX:=x-WinMin.X;
End;



Function WhereY: tcrtcoord;
{
  Return current Y-position of cursor.
}
var
  x,y : smallint;
Begin
  GetScreenCursor(x,y);
  WhereY:=y-WinMin.Y;
End;


{*************************************************************************
                            KeyBoard
*************************************************************************}

var
   keyboard_type: byte;  { 0=83/84-key keyboard, $10=101/102+ keyboard }
   is_last : boolean;
   last    : char;

procedure DetectKeyboard;
var
  regs: registers;
begin
  keyboard_type:=0;
  if (Mem[$40:$96] and $10)<>0 then
    begin
      regs.ax:=$1200;
      intr($16,regs);
      if regs.ax<>$1200 then
        keyboard_type:=$10;
    end;
end;

function readkey : char;
var
  char2 : char;
  char1 : char;
  regs : registers;
begin
  if is_last then
   begin
     is_last:=false;
     readkey:=last;
   end
  else
   begin
     regs.ah:=keyboard_type;
     intr($16,regs);
     if (regs.al=$e0) and (regs.ah<>0) then
      regs.al:=0;
     char1:=chr(regs.al);
     char2:=chr(regs.ah);
     if char1=#0 then
      begin
        is_last:=true;
        last:=char2;
      end;
     readkey:=char1;
   end;
end;


function keypressed : boolean;
var
  regs : registers;
begin
  if is_last then
   begin
     keypressed:=true;
     exit;
   end
  else
   begin
     regs.ah:=keyboard_type+1;
     intr($16,regs);
     keypressed:=((regs.flags and fZero) = 0);
   end;
end;


{*************************************************************************
                                   Delay
*************************************************************************}

procedure Delayloop;assembler;nostackframe;
label
  LDelayLoop1, LDelayLoop2;
asm
{ input:
    es:di = $40:$6c
    bx    = value of [es:dx] before the call
    dx:ax = counter }
LDelayLoop1:
        sub     ax, 1
        sbb     dx, 0
        jc      .LDelayLoop2
        cmp     bx, word es:[di]
        je      .LDelayLoop1
LDelayLoop2:
end;


procedure initdelay;
label
  LInitDel1;
begin
  asm
        { for some reason, using int $31/ax=$901 doesn't work here }
        { and interrupts are always disabled at this point when    }
        { running a program inside gdb(pas). Web bug 1345 (JM)     }
        sti
        mov     ax, $40
        mov     es, ax
        mov     di, $6c
        mov     bx, es:[di]
LInitDel1:
        cmp     bx, es:[di]
        je      LInitDel1
        mov     bx, es:[di]
        mov     ax, $FFFF
        mov     dx, $FFFF
        call    DelayLoop

        mov     [DelayCnt], ax
        mov     [DelayCnt + 2], dx
  end ['AX','BX','DX', 'DI'];
  DelayCnt := -DelayCnt div $55;
end;


procedure Delay(MS: Word);assembler;
label
  LDelay1, LDelay2;
asm
        mov     ax, $40
        mov     es, ax
        xor     di, di

        mov     cx, MS
        test    cx, cx
        jz      LDelay2
        mov     si, [DelayCnt + 2]
        mov     bx, es:[di]
LDelay1:
        mov     ax, [DelayCnt]
        mov     dx, si
        call    DelayLoop
        loop    LDelay1
LDelay2:
end;


procedure sound(hz : word);
label
  Lsound_next;
begin
  if hz=0 then
   begin
     nosound;
     exit;
   end;
  asm
        mov     cx, hz
        { dx:ax = 1193046 }
        mov     ax, $3456
        mov     dx, $12
        div     cx
        mov     cx, ax
        in      al, $61
        test    al, 3
        jnz     Lsound_next
        or      al, 3
        out     $61, al
        mov     al, $b6
        out     $43, al
     Lsound_next:
        mov     al, cl
        out     $42, al
        mov     al, ch
        out     $42, al
  end ['AX','CX','DX'];
end;


procedure nosound; assembler; nostackframe;
asm
        in      al, $61
        and     al, $fc
        out     $61, al
end;



{****************************************************************************
                          HighLevel Crt Functions
****************************************************************************}

procedure removeline(y : word);
var
  fil : word;
begin
  fil:=32 or (textattr shl 8);
  y:=WinMin.Y+y;
  While (y<=WinMax.Y) do
   begin
     dosmemmove(VidSeg,(y*ScreenWidth+word(WinMin.X))*2,
                VidSeg,((y-1)*ScreenWidth+word(WinMin.X))*2,(WinMax.X-WinMin.X+1)*2);
     inc(y);
   end;
  dosmemfillword(VidSeg,(word(WinMax.Y)*ScreenWidth+word(WinMin.X))*2,(WinMax.X-WinMin.X+1),fil);
end;


procedure delline;
begin
  removeline(wherey);
end;


procedure insline;
var
  my,y : smallint;
  fil : word;
begin
  fil:=32 or (textattr shl 8);
  y:=WhereY;
  my:=WinMax.Y-WinMin.Y;
  while (my>=y) do
   begin
     dosmemmove(VidSeg,(word(WinMin.Y+my-1)*ScreenWidth+word(WinMin.X))*2,
                VidSeg,(word(WinMin.Y+my)*ScreenWidth+word(WinMin.X))*2,(WinMax.X-WinMin.X+1)*2);
     dec(my);
   end;
  dosmemfillword(VidSeg,(word(WinMin.Y+y-1)*ScreenWidth+word(WinMin.X))*2,(WinMax.X-WinMin.X+1),fil);
end;




{****************************************************************************
                             Extra Crt Functions
****************************************************************************}

procedure cursoron;
var
  regs : registers;
begin
  regs.ax:=$0100;
  If VidSeg=$b800 then
    regs.cx:=$0607
  else
    regs.cx:=$b0d;
  intr($10,regs);
end;


procedure cursoroff;
var
  regs : registers;
begin
  regs.ax:=$0100;
  regs.cx:=$2000;
  intr($10,regs);
end;


procedure cursorbig;
var
  regs : registers;
begin
  regs.ax:=$0100;
  regs.cx:=$0007;
  intr($10,regs);
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}

var
  CurrX,CurrY : smallint;

Procedure VidMemWriteWord(vidmem_offset,w: word);assembler;
label
  in_retrace, no_retrace, no_snow, done;
asm
  mov di, vidmem_offset
  xor ax, ax
  mov es, ax
  mov dx, es:[$463]
  add dx, 6         { DX = CRT Status Register }
  mov ax, VidSeg
  mov es, ax
  test CheckSnow, 1
  jz no_snow
  mov bx, w

  { time critical code follows }
  { if you ever need to change this code, make sure you test it on a real }
  { 4.77 MHz 8088 with an original IBM CGA card and make sure it doesn't  }
  { produce snow }
  cli
in_retrace:
  in al, dx
  shr al, 1
  jc in_retrace
no_retrace:
  in al, dx
  shr al, 1
  jnc no_retrace
  xchg ax, bx
  stosw
  sti
  { time critical code ends here }
  jmp done

  { separate code path to avoid the unnecessary sti }
no_snow:
  mov ax, w
  stosw
done:
end;

Procedure WriteChar(c:char);
var
  regs : registers;
begin
  case c of
   #10 : inc(CurrY);
   #13 : CurrX:=WinMin.X+1;
    #8 : begin
           if CurrX>(WinMin.X+1) then
            dec(CurrX);
         end;
    #7 : begin { beep }
           regs.dl:=7;
           regs.ah:=2;
           intr($21,regs);
         end;
  else
   begin
     VidMemWriteWord((word(CurrY-1)*ScreenWidth+word(CurrX-1))*2,(textattr shl 8) or byte(c));
     inc(CurrX);
   end;
  end;
  if CurrX>(WinMax.X+1) then
   begin
     CurrX:=(WinMin.X+1);
     inc(CurrY);
   end;
  while CurrY>(WinMax.Y+1) do
   begin
     removeline(1);
     dec(CurrY);
   end;
end;


Procedure CrtWrite(var f : textrec);
var
  i : smallint;
begin
  GetScreenCursor(CurrX,CurrY);
  for i:=0 to f.bufpos-1 do
   WriteChar(f.buffer[i]);
  SetScreenCursor(CurrX,CurrY);
  f.bufpos:=0;
end;


Procedure CrtRead(Var F: TextRec);

  procedure BackSpace;
  begin
    if (f.bufpos>0) and (f.bufpos=f.bufend) then
     begin
       WriteChar(#8);
       WriteChar(' ');
       WriteChar(#8);
       dec(f.bufpos);
       dec(f.bufend);
     end;
  end;

var
  ch : Char;
Begin
  GetScreenCursor(CurrX,CurrY);
  f.bufpos:=0;
  f.bufend:=0;
  repeat
    if f.bufpos>f.bufend then
     f.bufend:=f.bufpos;
    SetScreenCursor(CurrX,CurrY);
    ch:=readkey;
    case ch of
    #0 : case readkey of
          #71 : while f.bufpos>0 do
                 begin
                   dec(f.bufpos);
                   WriteChar(#8);
                 end;
          #75 : if f.bufpos>0 then
                 begin
                   dec(f.bufpos);
                   WriteChar(#8);
                 end;
          #77 : if f.bufpos<f.bufend then
                 begin
                   WriteChar(f.bufptr^[f.bufpos]);
                   inc(f.bufpos);
                 end;
          #79 : while f.bufpos<f.bufend do
                 begin
                   WriteChar(f.bufptr^[f.bufpos]);
                   inc(f.bufpos);
                 end;
         end;
    ^S,
    #8 : BackSpace;
    ^Y,
   #27 : begin
           while f.bufpos<f.bufend do begin
            WriteChar(f.bufptr^[f.bufpos]);
            inc(f.bufpos);
           end;
           while f.bufend>0 do
            BackSpace;
         end;
   #13 : begin
           WriteChar(#13);
           WriteChar(#10);
           f.bufptr^[f.bufend]:=#13;
           f.bufptr^[f.bufend+1]:=#10;
           inc(f.bufend,2);
           break;
         end;
   #26 : if CheckEOF then
          begin
            f.bufptr^[f.bufend]:=#26;
            inc(f.bufend);
            break;
          end;
    else
     begin
       if f.bufpos<f.bufsize-2 then
        begin
          f.buffer[f.bufpos]:=ch;
          inc(f.bufpos);
          WriteChar(ch);
        end;
     end;
    end;
  until false;
  f.bufpos:=0;
  SetScreenCursor(CurrX,CurrY);
End;


Procedure CrtReturn(Var F: TextRec);
Begin
end;


Procedure CrtClose(Var F: TextRec);
Begin
  F.Mode:=fmClosed;
End;


Procedure CrtOpen(Var F: TextRec);
Begin
  If F.Mode=fmOutput Then
   begin
     TextRec(F).InOutFunc:=@CrtWrite;
     TextRec(F).FlushFunc:=@CrtWrite;
   end
  Else
   begin
     F.Mode:=fmInput;
     TextRec(F).InOutFunc:=@CrtRead;
     TextRec(F).FlushFunc:=@CrtReturn;
   end;
  TextRec(F).CloseFunc:=@CrtClose;
End;


procedure AssignCrt(var F: Text);
begin
  Assign(F,'');
  TextRec(F).OpenFunc:=@CrtOpen;
end;

{ use the C version to avoid using dpmiexcp unit
  which makes sysutils and exceptions working incorrectly  PM }

//function __djgpp_set_ctrl_c(enable : longint) : boolean;cdecl;external;

var
  x,y : smallint;
begin
{ Detect keyboard type }
  DetectKeyboard;
{ Load startup values }
  ScreenWidth:=GetScreenWidth;
  ScreenHeight:=GetScreenHeight;
  WindMax:=(ScreenWidth-1) or ((ScreenHeight-1) shl 8);
{ Load TextAttr }
  GetScreenCursor(x,y);
  lastmode := mem[$40:$49];
  if screenheight>25 then
    lastmode:=lastmode or $100;
  DetectSnow;
  If not(lastmode=Mono) then
    VidSeg := $b800
  else
    VidSeg := $b000;
  TextAttr:=mem[VidSeg:(word(y-1)*ScreenWidth+word(x-1))*2+1];
{ Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;
{ Calculates delay calibration }
  initdelay;
{ Enable ctrl-c input (JM) }
//  __djgpp_set_ctrl_c(0);
end.
