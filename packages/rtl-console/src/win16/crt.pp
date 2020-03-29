{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by the Free Pascal development team.

    Borland Pascal 7 Compatible CRT Unit - Win16 implementation

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
  video, keyboard, WinProcs, WinTypes;

{****************************************************************************
                           Low level Routines
****************************************************************************}

function GetScreenHeight : word;
begin
  getscreenheight:=video.ScreenHeight;
end;


function GetScreenWidth : word;
begin
  getscreenwidth:=video.ScreenWidth;
end;


procedure SetScreenCursor(x,y : smallint);
begin
  video.SetCursorPos(x-1,y-1);
end;


procedure GetScreenCursor(var x,y : smallint);
begin
  x:=video.CursorX+1;
  y:=video.CursorY+1;
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
begin
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
    FillWord(VideoBuf^,ScreenHeight*ScreenWidth,fil)
  else
    begin
      for y:=WinMin.Y to WinMax.Y do
        FillWord(VideoBuf^[y*ScreenWidth+word(WinMin.X)],WinMax.X-WinMin.X+1,fil);
    end;
  UpdateScreen(false);
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
    begin
      FillWord(VideoBuf^[(word(y-1)*ScreenWidth+word(x-1))],WinMax.X-x+2,fil);
      UpdateScreen(false);
    end;
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
   is_last : boolean;
   last    : char;

function readkey : char;
var
  k: TKeyEvent;
  char1, char2: char;
begin
  if is_last then
  begin
    is_last:=false;
    readkey:=last;
  end
  else
  begin
    k:=GetKeyEvent;
    char1:=Chr(Byte(k));
    char2:=Chr(Word(k) shr 8);
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
    keypressed:=PollKeyEvent<>0;
end;


{*************************************************************************
                                   Delay
*************************************************************************}

procedure Delay(MS: Word);
var
  ticks: LongInt;
  m: MSG;
begin
  ticks:=GetTickCount;
  repeat
    if PeekMessage(FarAddr(m),0,0,0,1) then
    begin
      TranslateMessage(FarAddr(m));
      DispatchMessage(FarAddr(m));
    end;
  until (GetTickCount-ticks)>=MS;
end;


procedure sound(hz : word);
begin
end;


procedure nosound;
begin
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
      Move(VideoBuf^[(y*ScreenWidth+word(WinMin.X))],
           VideoBuf^[((y-1)*ScreenWidth+word(WinMin.X))],(WinMax.X-WinMin.X+1)*2);
      inc(y);
    end;
  FillWord(VideoBuf^[(word(WinMax.Y)*ScreenWidth+word(WinMin.X))],(WinMax.X-WinMin.X+1),fil);
end;


procedure delline;
begin
  removeline(wherey);
  UpdateScreen(false);
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
      Move(VideoBuf^[(word(WinMin.Y+my-1)*ScreenWidth+word(WinMin.X))],
           VideoBuf^[(word(WinMin.Y+my)*ScreenWidth+word(WinMin.X))],(WinMax.X-WinMin.X+1)*2);
      dec(my);
    end;
  FillWord(VideoBuf^[(word(WinMin.Y+y-1)*ScreenWidth+word(WinMin.X))],(WinMax.X-WinMin.X+1),fil);
  UpdateScreen(false);
end;




{****************************************************************************
                             Extra Crt Functions
****************************************************************************}

procedure cursoron;
begin
  SetCursorType(crUnderLine);
end;


procedure cursoroff;
begin
  SetCursorType(crHidden);
end;


procedure cursorbig;
begin
  SetCursorType(crBlock);
end;


{*****************************************************************************
                          Read and Write routines
*****************************************************************************}

var
  CurrX,CurrY : smallint;

Procedure WriteChar(c:char);
begin
  case c of
   #10 : inc(CurrY);
   #13 : CurrX:=WinMin.X+1;
    #8 : begin
           if CurrX>(WinMin.X+1) then
            dec(CurrX);
         end;
    #7 : begin { beep }
//           regs.dl:=7;
//           regs.ah:=2;
//           intr($21,regs);
         end;
  else
   begin
     VideoBuf^[word(CurrY-1)*ScreenWidth+word(CurrX-1)]:=(textattr shl 8) or byte(c);
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
  UpdateScreen(false);
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
           UpdateScreen(false);
           break;
         end;
   #26 : if CheckEOF then
          begin
            f.bufptr^[f.bufend]:=#26;
            inc(f.bufend);
            UpdateScreen(false);
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
    UpdateScreen(false);
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

begin
  InitVideo;
  InitKeyboard;
{ Load startup values }
  ScreenWidth:=GetScreenWidth;
  ScreenHeight:=GetScreenHeight;
  WindMax:=(ScreenWidth-1) or ((ScreenHeight-1) shl 8);
  TextAttr:=$07;
{ Redirect the standard output }
  assigncrt(Output);
  Rewrite(Output);
  TextRec(Output).Handle:=StdOutputHandle;
  assigncrt(Input);
  Reset(Input);
  TextRec(Input).Handle:=StdInputHandle;
end.
