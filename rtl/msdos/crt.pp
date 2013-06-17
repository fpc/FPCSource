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
  ScreenHeight : longint;

implementation

uses
  dos;

{$ASMMODE INTEL}

var
  DelayCnt : Longint;
  VidSeg : Word;

{
  definition of textrec is in textrec.inc
}
{$i textrec.inc}


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


function GetScreenHeight : longint;
begin
  getscreenheight:=mem[$40:$84]+1;
  If mem[$40:$84]=0 then
    getscreenheight := 25;
end;


function GetScreenWidth : longint;
begin
  getscreenwidth:=memw[$40:$4a];
end;


procedure SetScreenCursor(x,y : longint);
var
  regs : registers;
begin
  regs.ax:=$0200;
  regs.bx:=0;
  regs.dx:=(y-1) shl 8+(x-1);
  intr($10,regs);
end;


procedure GetScreenCursor(var x,y : longint);
begin
  x:=mem[$40:$50]+1;
  y:=mem[$40:$51]+1;
end;


{****************************************************************************
                              Helper Routines
****************************************************************************}

Function WinMinX: Byte;
{
  Current Minimum X coordinate
}
Begin
  WinMinX:=(WindMin and $ff)+1;
End;



Function WinMinY: Byte;
{
  Current Minimum Y Coordinate
}
Begin
  WinMinY:=(WindMin shr 8)+1;
End;



Function WinMaxX: Byte;
{
  Current Maximum X coordinate
}
Begin
  WinMaxX:=(WindMax and $ff)+1;
End;



Function WinMaxY: Byte;
{
  Current Maximum Y coordinate;
}
Begin
  WinMaxY:=(WindMax shr 8) + 1;
End;



Function FullWin:boolean;
{
  Full Screen 80x25? Window(1,1,80,25) is used, allows faster routines
}
begin
  FullWin:=(WinMinX=1) and (WinMinY=1) and
           (WinMaxX=ScreenWidth) and (WinMaxY=ScreenHeight);
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
  If (X>0) and (X<=WinMaxX- WinMinX+1) and
     (Y>0) and (Y<=WinMaxY-WinMinY+1) Then
   Begin
     Inc(X,WinMinX-1);
     Inc(Y,WinMinY-1);
     SetScreenCursor(x,y);
   End;
End;


Procedure Window(X1, Y1, X2, Y2: Byte);
{
  Set screen window to the specified coordinates.
}
Begin
  if (X1>X2) or (X2>ScreenWidth) or
     (Y1>Y2) or (Y2>ScreenHeight) then
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
  y   : longint;
begin
  fil:=32 or (textattr shl 8);
  if FullWin then
   DosmemFillWord(VidSeg,0,ScreenHeight*ScreenWidth,fil)
  else
   begin
     for y:=WinMinY to WinMaxY do
      DosmemFillWord(VidSeg,((y-1)*ScreenWidth+(WinMinX-1))*2,WinMaxX-WinMinX+1,fil);
   end;
  Gotoxy(1,1);
end;


Procedure ClrEol;
{
  Clear from current position to end of line.
}
var
  x,y : longint;
  fil : word;
Begin
  GetScreenCursor(x,y);
  fil:=32 or (textattr shl 8);
  if x<=WinMaxX then
   DosmemFillword(VidSeg,((y-1)*ScreenWidth+(x-1))*2,WinMaxX-x+1,fil);
End;



Function WhereX: tcrtcoord;
{
  Return current X-position of cursor.
}
var
  x,y : longint;
Begin
  GetScreenCursor(x,y);
  WhereX:=x-WinMinX+1;
End;



Function WhereY: tcrtcoord;
{
  Return current Y-position of cursor.
}
var
  x,y : longint;
Begin
  GetScreenCursor(x,y);
  WhereY:=y-WinMinY+1;
End;


{*************************************************************************
                            KeyBoard
*************************************************************************}

var
   is_last : boolean;
   last    : char;

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
     regs.ah:=$10;
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
     regs.ah:=$11;
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

procedure removeline(y : longint);
var
  fil : word;
begin
  fil:=32 or (textattr shl 8);
  y:=WinMinY+y-1;
  While (y<WinMaxY) do
   begin
     dosmemmove(VidSeg,(y*ScreenWidth+(WinMinX-1))*2,
                VidSeg,((y-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1)*2);
     inc(y);
   end;
  dosmemfillword(VidSeg,((WinMaxY-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1),fil);
end;


procedure delline;
begin
  removeline(wherey);
end;


procedure insline;
var
  my,y : longint;
  fil : word;
begin
  fil:=32 or (textattr shl 8);
  y:=WhereY;
  my:=WinMaxY-WinMinY;
  while (my>=y) do
   begin
     dosmemmove(VidSeg,(((WinMinY+my-1)-1)*ScreenWidth+(WinMinX-1))*2,
                VidSeg,(((WinMinY+my)-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1)*2);
     dec(my);
   end;
  dosmemfillword(VidSeg,(((WinMinY+y-1)-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1),fil);
end;




{****************************************************************************
                             Extra Crt Functions
****************************************************************************}

procedure cursoron;
var
  regs : registers;
begin
  regs.ax:=$0100;
  regs.cx:=$90A;
  If VidSeg=$b800 then
    regs.cx:=$90A
  else
    regs.cx:=$b0d;
  intr($10,regs);
end;


procedure cursoroff;
var
  regs : registers;
begin
  regs.ax:=$0100;
  regs.cx:=$ffff;
  intr($10,regs);
end;


procedure cursorbig;
var
  regs : registers;
begin
  regs.ax:=$0100;
  regs.cx:=$10A;
  intr($10,regs);
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}

var
  CurrX,CurrY : longint;

Procedure WriteChar(c:char);
var
  regs : registers;
begin
  case c of
   #10 : inc(CurrY);
   #13 : CurrX:=WinMinX;
    #8 : begin
           if CurrX>WinMinX then
            dec(CurrX);
         end;
    #7 : begin { beep }
           regs.dl:=7;
           regs.ah:=2;
           intr($21,regs);
         end;
  else
   begin
     memw[VidSeg:((CurrY-1)*ScreenWidth+(CurrX-1))*2]:=(textattr shl 8) or byte(c);
     inc(CurrX);
   end;
  end;
  if CurrX>WinMaxX then
   begin
     CurrX:=WinMinX;
     inc(CurrY);
   end;
  while CurrY>WinMaxY do
   begin
     removeline(1);
     dec(CurrY);
   end;
end;


Function CrtWrite(var f : textrec):integer;
var
  i : longint;
begin
  GetScreenCursor(CurrX,CurrY);
  for i:=0 to f.bufpos-1 do
   WriteChar(f.buffer[i]);
  SetScreenCursor(CurrX,CurrY);
  f.bufpos:=0;
  CrtWrite:=0;
end;


Function CrtRead(Var F: TextRec): Integer;

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
  CrtRead:=0;
End;


Function CrtReturn(Var F: TextRec): Integer;
Begin
  CrtReturn:=0;
end;


Function CrtClose(Var F: TextRec): Integer;
Begin
  F.Mode:=fmClosed;
  CrtClose:=0;
End;


Function CrtOpen(Var F: TextRec): Integer;
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
  CrtOpen:=0;
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
  x,y : longint;
begin
{ Load startup values }
  ScreenWidth:=GetScreenWidth;
  ScreenHeight:=GetScreenHeight;
  WindMax:=(ScreenWidth-1) or ((ScreenHeight-1) shl 8);
{ Load TextAttr }
  GetScreenCursor(x,y);
  lastmode := mem[$40:$49];
  if screenheight>25 then
    lastmode:=lastmode or $100;
  If not(lastmode=Mono) then
    VidSeg := $b800
  else
    VidSeg := $b000;
  TextAttr:=mem[VidSeg:((y-1)*ScreenWidth+(x-1))*2+1];
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
