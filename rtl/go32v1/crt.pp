{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team.

    Borland Pascal 7 Compatible CRT Unit for Go32V1 and Go32V2

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit crt;
interface

const
{ CRT modes }
  BW40          = 0;            { 40x25 B/W on Color Adapter }
  CO40          = 1;            { 40x25 Color on Color Adapter }
  BW80          = 2;            { 80x25 B/W on Color Adapter }
  CO80          = 3;            { 80x25 Color on Color Adapter }
  Mono          = 7;            { 80x25 on Monochrome Adapter }
  Font8x8       = 256;          { Add-in for ROM font }

{ Mode constants for 3.0 compatibility }
  C40           = CO40;
  C80           = CO80;

{ Foreground and background color constants }
  Black         = 0;
  Blue          = 1;
  Green         = 2;
  Cyan          = 3;
  Red           = 4;
  Magenta       = 5;
  Brown         = 6;
  LightGray     = 7;

{ Foreground color constants }
  DarkGray      = 8;
  LightBlue     = 9;
  LightGreen    = 10;
  LightCyan     = 11;
  LightRed      = 12;
  LightMagenta  = 13;
  Yellow        = 14;
  White         = 15;

{ Add-in for blinking }
  Blink         = 128;

var

{ Interface variables }
  CheckBreak: Boolean;    { Enable Ctrl-Break }
  CheckEOF: Boolean;      { Enable Ctrl-Z }
  DirectVideo: Boolean;   { Enable direct video addressing }
  CheckSnow: Boolean;     { Enable snow filtering }
  LastMode: Word;         { Current text mode }
  TextAttr: Byte;         { Current text attribute }
  WindMin: Word;          { Window upper left coordinates }
  WindMax: Word;          { Window lower right coordinates }

{ Interface procedures }
procedure AssignCrt(var F: Text);
function KeyPressed: Boolean;
function ReadKey: Char;
procedure TextMode(Mode: Integer);
procedure Window(X1,Y1,X2,Y2: Byte);
procedure GotoXY(X,Y: Byte);
function WhereX: Byte;
function WhereY: Byte;
procedure ClrScr;
procedure ClrEol;
procedure InsLine;
procedure DelLine;
procedure TextColor(Color: Byte);
procedure TextBackground(Color: Byte);
procedure LowVideo;
procedure HighVideo;
procedure NormVideo;
procedure Delay(MS: Word);
procedure Sound(Hz: Word);
procedure NoSound;

{Extra Functions}
procedure cursoron;
procedure cursoroff;
procedure cursorbig;


implementation

uses
  go32;


{$ASMMODE ATT}

var
  DelayCnt,  { don't modify this var name, as it is hard coded }
  ScreenWidth,
  ScreenHeight : longint;


{
  definition of textrec is in textrec.inc
}
{$i textrec.inc}


{****************************************************************************
                           Low level Routines
****************************************************************************}

procedure setscreenmode(mode : byte);
begin
  asm
        movb    8(%ebp),%al
        xorb    %ah,%ah
        pushl   %ebp
        int     $0x10
        popl    %ebp
  end;
end;


function GetScreenHeight : longint;
begin
  dosmemget($40,$84,getscreenheight,1);
  inc(getscreenheight);
end;


function GetScreenWidth : longint;
begin
  dosmemget($40,$4a,getscreenwidth,1);
end;


procedure SetScreenCursor(x,y : longint);
begin
  asm
        movb    $0x02,%ah
        movb    $0,%bh
        movb    y,%dh
        movb    x,%dl
        subw    $0x0101,%dx
        pushl   %ebp
        int     $0x10
        popl    %ebp
  end;
end;


procedure GetScreenCursor(var x,y : longint);
begin
  x:=0;
  y:=0;
  dosmemget($40,$50,x,1);
  dosmemget($40,$51,y,1);
  inc(x);
  inc(y);
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


procedure textmode(mode : integer);
begin
  lastmode:=mode;
  mode:=mode and $ff;
  setscreenmode(mode);
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


Procedure GotoXy(X: Byte; Y: Byte);
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
   DosmemFillWord($b800,0,ScreenHeight*ScreenWidth,fil)
  else
   begin
     for y:=WinMinY to WinMaxY do
      DosmemFillWord($b800,((y-1)*ScreenWidth+(WinMinX-1))*2,WinMaxX-WinMinX+1,fil);
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
  if x<WinMaxX then
   DosmemFillword($b800,((y-1)*ScreenWidth+(x-1))*2,WinMaxX-x+1,fil);
End;



Function WhereX: Byte;
{
  Return current X-position of cursor.
}
var
  x,y : longint;
Begin
  GetScreenCursor(x,y);
  WhereX:=x-WinMinX+1;
End;



Function WhereY: Byte;
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
begin
  if is_last then
   begin
     is_last:=false;
     readkey:=last;
   end
  else
   begin
     asm
        movb    $0,%ah
        pushl   %ebp
        int     $0x16
        popl    %ebp
        movb    %al,char1
        movb    %ah,char2
     end;
     if char1=#0 then
      begin
        is_last:=true;
        last:=char2;
      end;
     readkey:=char1;
   end;
end;


function keypressed : boolean;
begin
  if is_last then
   begin
     keypressed:=true;
     exit;
   end
  else
   begin
     asm
        movb    $1,%ah
        pushl   %ebp
        int     $0x16
        popl    %ebp
        setnz   %al
        movb    %al,__RESULT
     end;
   end;
end;


{*************************************************************************
                                   Delay
*************************************************************************}

procedure Delayloop;assembler;
asm
.LDelayLoop1:
        subl    $1,%eax
        jc      .LDelayLoop2
        cmpl    %fs:(%edi),%ebx
        je      .LDelayLoop1
.LDelayLoop2:
end;


procedure initdelay;assembler;
asm
        movl    $0x46c,%edi
        movl    $-28,%edx
        movl    %fs:(%edi),%ebx
.LInitDel1:
        cmpl    %fs:(%edi),%ebx
        je      .LInitDel1
        movl    %fs:(%edi),%ebx
        movl    %edx,%eax
        call    DelayLoop

        notl    %eax
        xorl    %edx,%edx
        movl    $55,%ecx
        divl    %ecx
        movl    %eax,DelayCnt
end;


procedure Delay(MS: Word);assembler;
asm
        movzwl  MS,%ecx
        jecxz   .LDelay2
        movl    $0x400,%edi
        movl    DelayCnt,%edx
        movl    %fs:(%edi),%ebx
.LDelay1:
        movl    %edx,%eax
        call    DelayLoop
        loop    .LDelay1
.LDelay2:
end;


procedure sound(hz : word);
begin
  if hz=0 then
   begin
     nosound;
     exit;
   end;
  asm
        movzwl  hz,%ecx
        movl    $1193046,%eax
        cltd
        divl    %ecx
        movl    %eax,%ecx
        movb    $0xb6,%al
        outb    %al,$0x43
        movb    %cl,%al
        outb    %al,$0x42
        movb    %ch,%al
        outb    %al,$0x42
        inb     $0x61,%al
        orb     $0x3,%al
        outb    %al,$0x61
  end ['EAX','ECX','EDX'];
end;


procedure nosound;
begin
  asm
        inb     $0x61,%al
        andb    $0xfc,%al
        outb    %al,$0x61
  end ['EAX'];
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
     dosmemmove($b800,(y*ScreenWidth+(WinMinX-1))*2,
                $b800,((y-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1)*2);
     inc(y);
   end;
  dosmemfillword($b800,((WinMaxY-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1),fil);
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
     dosmemmove($b800,(((WinMinY+my-1)-1)*ScreenWidth+(WinMinX-1))*2,
                $b800,(((WinMinY+my)-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1)*2);
     dec(my);
   end;
  dosmemfillword($b800,(((WinMinY+y-1)-1)*ScreenWidth+(WinMinX-1))*2,(WinMaxX-WinMinX+1),fil);
end;




{****************************************************************************
                             Extra Crt Functions
****************************************************************************}

procedure cursoron;
begin
  asm
        movb    $1,%ah
        movb    $10,%cl
        movb    $9,%ch
        pushl   %ebp
        int     $0x10
        popl    %ebp
  end;
end;


procedure cursoroff;
begin
  asm
        movb    $1,%ah
        movb    $-1,%cl
        movb    $-1,%ch
        pushl   %ebp
        int     $0x10
        popl    %ebp
  end;
end;


procedure cursorbig;
begin
  asm
        movb    $1,%ah
        movw    $110,%cx
        pushl   %ebp
        int     $0x10
        popl    %ebp
  end;
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}

var
  CurrX,CurrY : longint;

Procedure WriteChar(c:char);
var
  chattr : word;
begin
  case c of
   #10 : inc(CurrY);
   #13 : CurrX:=WinMinX;
    #8 : begin
           if CurrX>WinMinX then
            dec(CurrX);
         end;
    #7 : begin { beep }
         end;
  else
   begin
     chattr:=(textattr shl 8) or byte(c);
     dosmemput($b800,((CurrY-1)*ScreenWidth+(CurrX-1))*2,chattr,2);
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
           f.bufpos:=f.bufend;
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


Function CrtReturn:Integer;
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


var
  x,y : longint;
begin
{ Load startup values }
  ScreenWidth:=GetScreenWidth;
  ScreenHeight:=GetScreenHeight;
  WindMax:=(ScreenWidth-1) or ((ScreenHeight-1) shl 8);
{ Load TextAttr }
  GetScreenCursor(x,y);
  dosmemget($b800,((y-1)*ScreenWidth+(x-1))*2+1,TextAttr,1);
  dosmemget($40,$49,lastmode,1);
{ Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;
{ Calculates delay calibration }
  initdelay;
end.

{
  $Log$
  Revision 1.1  2000-07-13 06:30:34  michael
  + Initial import

  Revision 1.5  2000/04/14 12:18:11  pierre
   * fix for bug 923

  Revision 1.4  2000/01/07 16:41:29  daniel
    * copyright 2000

  Revision 1.3  2000/01/07 16:32:23  daniel
    * copyright 2000 added

  Revision 1.2  1999/06/09 16:46:08  peter
    * fixed fullwin,textbackground

  Revision 1.1  1998/12/21 13:07:02  peter
    * use -FE

  Revision 1.17  1998/12/15 22:42:49  peter
    * removed temp symbols

  Revision 1.16  1998/12/09 23:04:36  jonas
    * fixed bug in InsLine (changed "my" from "WinMaxY -1" to "WinMaxY - WinMinY")

  Revision 1.15  1998/11/28 14:09:48  peter
    * NOATTCDQ define

  Revision 1.14  1998/11/26 23:14:52  jonas
    * changed cdq to cltd in AT&T assembler block

  Revision 1.13  1998/08/26 10:01:54  peter
    * fixed readln cursor position

  Revision 1.12  1998/08/19 17:57:55  peter
    * fixed crtread with wrong cursor position

  Revision 1.11  1998/08/19 14:55:44  peter
    * fixed removeline which scrolled too much lines

  Revision 1.10  1998/08/18 13:32:46  carl
    * bugfix to make it work with FPC 0.99.5 (Delayloop is not correctly
  converted by ATT parser)

  Revision 1.9  1998/08/15 17:00:10  peter
    * moved delaycnt from interface to implementation

  Revision 1.8  1998/08/08 21:56:45  peter
    * updated crt with new delay, almost like bp7 routine

  Revision 1.5  1998/05/31 14:18:12  peter
    * force att or direct assembling
    * cleanup of some files

  Revision 1.4  1998/05/28 10:21:38  pierre
    * Handles of input and output restored

  Revision 1.3  1998/05/27 00:19:16  peter
    * fixed crt input

  Revision 1.2  1998/05/21 19:30:46  peter
    * objects compiles for linux
    + assign(pchar), assign(char), rename(pchar), rename(char)
    * fixed read_text_as_array
    + read_text_as_pchar which was not yet in the rtl
}

