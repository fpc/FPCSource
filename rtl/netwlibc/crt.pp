{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2004 by the Free Pascal development team.

    Borland Pascal 7 Compatible CRT Unit for Netware (libc version)

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit crt;

interface

{$i crth.inc}

Const
  ScreenHeight : longint=25;
  ScreenWidth  : longint=80;

implementation

uses Libc;


{$ASMMODE ATT}

var
  ScreenHandle : scr_t;

{ Definition of textrec is in textrec.inc }
{$i textrec.inc}


{****************************************************************************
                           Low level Routines
****************************************************************************}

procedure setscreenmode(mode : byte);
begin
  setscreenmode (mode);
end;


function GetScreenHeight : longint;
VAR Height, Width : WORD;
begin
  GetScreenSize(Height, Width);
  GetScreenHeight := Height;
end;


function GetScreenWidth : longint;
VAR Height, Width : WORD;
begin
  GetScreenSize(Height, Width);
  GetScreenWidth := Width;
end;

procedure GetScreenCursor(var x,y : longint);
begin
  x := wherecol+1;
  y := whererow+1;
end;


{****************************************************************************
                              Helper Routines
****************************************************************************}

Function WinMinX: Longint;
{
  Current Minimum X coordinate
}
Begin
  WinMinX:=(WindMin and $ff)+1;
End;



Function WinMinY: Longint;
{
  Current Minimum Y Coordinate
}
Begin
  WinMinY:=(WindMin shr 8)+1;
End;



Function WinMaxX: Longint;
{
  Current Maximum X coordinate
}
Begin
  WinMaxX:=(WindMax and $ff)+1;
End;



Function WinMaxY: Longint;
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


procedure TextMode (Mode: word);
begin
  Window (1,1,byte(ScreenWidth),byte(ScreenHeight));
  ClrScr;
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
     X := X + WinMinX - 1;
     Y := Y + WinMinY - 1;
     gotorowcol (y-1,x-1);
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
  rowlen,rows: longint;
begin
  if FullWin then
  begin
    clearscreen;  {seems to swich cursor off}
    //_DisplayInputCursor;
  end else
  begin
    rowlen := WinMaxX-WinMinX+1;
    rows   := WinMaxY-WinMinY+1;
    FillScreenArea(ScreenHandle,WinMinY-1,WinMinX-1,rows,rowlen,' ',textattr);
  end;
  Gotoxy(1,1);
end;


Procedure ClrEol;
{
  Clear from current position to end of line.
}
var
  x,y : longint;
  rowlen : word;
Begin
  GetScreenCursor(x,y);
  if x<WinMaxX then
  begin
    rowlen := WinMaxX-x+1;
    FillScreenArea(ScreenHandle,y-1,x-1,1,rowlen,' ',textattr);
  end;
End;



Function WhereX: tcrtcoord;
{
  Return current X-position of cursor.
}
Begin
  WhereX:=wherecol-WinMinX+1;
End;



Function WhereY: tcrtcoord;
{
  Return current Y-position of cursor.
}
Begin
  WhereY:=whererow-WinMinY+1;
End;


{*************************************************************************
                            Keyboard
*************************************************************************}

var
   is_last : boolean;

{
function readkey : char;
var
  keytype,modifier,scancode : longint;
begin
  if is_last then
  begin
     is_last:=false;
     readkey:=getch;
  end else
  begin
    // _SetCtrlCharCheckMode (CheckBreak);
    WaitForKey (ScreenHandle);
    getkey(keytype,modifer,scancode):longint;
    char1 := getch;
    if char1 = #0 then is_last := true;
    readkey:=char1;
  end;
end;
}

function readkey : char;  // for now
begin
  readkey := char(getcharacter);
end;


function keypressed : boolean;
begin
  if is_last then
  begin
    keypressed:=true;
    exit;
  end else
    keypressed := (kbhit <> 0);
end;


{*************************************************************************
                                   Delay
*************************************************************************}

procedure Delay(MS: Word);
begin
  libc.delay (MS);
end;

procedure sound(hz : word);
begin
  RingBell;
end;

procedure nosound;
begin
end;



{****************************************************************************
                          HighLevel Crt Functions
****************************************************************************}

{procedure removeline(y : longint);
var
  fil : word;
  rowlen : word;
  p : pointer;
begin
  fil:=32 or (textattr shl 8);
  rowlen:=WinMaxX-WinMinX+1;
  GetMem (p, rowlen*2);
  y:=WinMinY+y-1;
  While (y<=WinMaxY) do
   begin
     _CopyFromScreenMemory (1,rowlen,p,WinMinX-1,word(y));
     _CopyToScreenMemory (1,rowlen,p,WinMinX-1,word(y-1));
     inc(y);
   end;
  FillWord (p^,rowlen,fil);
  _CopyToScreenMemory (1,rowlen,p,WinMinX-1,WinMaxY-1);
  FreeMem (p, rowlen*2);
end;}
procedure removeline(y : longint);
var rowlen : longint;
begin
  rowlen:=WinMaxX-WinMinX+1;
  y:=WinMinY+y-1-1;
  ScrollScreenArea(ScreenHandle,y,WinMinX-1,WinMaxY-WinMinY+1,rowlen,1,0,SCROLL_UP);
end;


procedure delline;
begin
  removeline(wherey);
end;


procedure insline;
var rowlen : longint;
begin
  rowlen:=WinMaxX-WinMinX+1;
  ScrollScreenArea(ScreenHandle,wherecol,WinMinX-1,WinMaxY-WinMinY+1,rowlen,1,textattr,SCROLL_DOWN);
end;




{****************************************************************************
                             Extra Crt Functions
****************************************************************************}

procedure cursoron;
begin
  SetCursorStyle(ScreenHandle,CURSOR_NORMAL);
  EnableInputCursor(ScreenHandle);
end;


procedure cursoroff;
begin
  DisableInputCursor (ScreenHandle);
end;


procedure cursorbig;
begin
  SetCursorStyle(ScreenHandle,CURSOR_BLOCK);
  EnableInputCursor(ScreenHandle);
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}

var
  CurrX,CurrY : longint;

Procedure WriteChar(c:char);
var st : array [0..1] of char;
begin
  case c of
   #10 : inc(CurrY);
   #13 : CurrX:=WinMinX;
    #8 : begin
           if CurrX>WinMinX then
            dec(CurrX);
         end;
    #7 : begin { beep }
           RingBell;
         end;
  else
   begin
     //WriteScreenCharacterAttribute(ScreenHandle,CurrY-1,CurrX-1,c,textattr);  {not available in protected mode}
     st[0] := c;
     st[1] := #0;
     OutputToScreenWithAttribute(ScreenHandle,textattr,@st);
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
    WriteChar(f.buffer[i]);  { ad: may be better to use a buffer but i think it's fast enough }
  gotorowcol (CurrY-1,CurrX-1);
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
    gotorowcol (CurrY-1,CurrX-1);
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
  gotorowcol (CurrY-1,CurrX-1);
  CrtRead:=0;
End;

{$Warnings off}
Function CrtReturn(Var F: TextRec): Integer;
Begin
  CrtReturn:=0;
end;
{$Warnings on}


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

procedure InitScreenMode;
var
  s_mode : dword;
begin
  getscreenmode (s_mode);
  lastmode := s_mode;
end;

var
  x,y : longint;
begin
  ScreenHandle := getscreenhandle;
{ Load startup values }
  ScreenWidth:=GetScreenWidth;
  ScreenHeight:=GetScreenHeight;
  lastmode := CO80;
  GetScreenCursor(x,y);
  TextColor (LightGray);
  TextBackground (Black);
  InitScreenMode;
{ Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;
  CheckBreak := FALSE;
  CheckEOF := FALSE;
  //_SetCtrlCharCheckMode (CheckBreak);
  //_SetAutoScreenDestructionMode (TRUE);
end.
