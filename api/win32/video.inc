{
   $Id$
   System independent low-level video interface for win32

   Copyright (c) 1999 by Florian Klaempfl

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


uses
  windows,dos;

var
  OldVideoBuf : PVideoBuf;
  ConsoleInfo : TConsoleScreenBufferInfo;
  ConsoleCursorInfo : TConsoleCursorInfo;
  MaxVideoBufSize : DWord;

procedure InitVideo;
begin
  ScreenColor:=true;
  GetConsoleScreenBufferInfo(TextRec(Output).Handle, ConsoleInfo);
  GetConsoleCursorInfo(TextRec(Output).Handle, ConsoleCursorInfo);

  with ConsoleInfo.srWindow do
    begin
       ScreenWidth:=right-left+1;
       ScreenHeight:=bottom-top+1;
    end;

  { srWindow is sometimes bigger then dwMaximumWindowSize
    this led to wrong ScreenWidth and ScreenHeight values PM }
  { damned: its also sometimes less !! PM }
  with ConsoleInfo.dwMaximumWindowSize do
    begin
       {if ScreenWidth>X then}
         ScreenWidth:=X;
       {if ScreenHeight>Y then}
         ScreenHeight:=Y;
    end;

  CursorX:=ConsoleInfo.dwCursorPosition.x;
  CursorY:=ConsoleInfo.dwCursorPosition.y;
  if not ConsoleCursorInfo.bvisible then
    CursorLines:=0
  else
    CursorLines:=ConsoleCursorInfo.dwSize;

  { allocate back buffer }
  MaxVideoBufSize:= ScreenWidth * ScreenHeight * 2;
  VideoBufSize:=ScreenWidth*ScreenHeight*2;

  GetMem(VideoBuf,MaxVideoBufSize);
  GetMem(OldVideoBuf,MaxVideoBufSize);

  {ClearScreen; not needed PM }
end;


procedure DoneVideo;
begin
  { ClearScreen; also not needed PM }
  SetCursorType(crUnderLine);
  { SetCursorPos(0,0); also not needed PM }
  FreeMem(VideoBuf,MaxVideoBufSize);
  FreeMem(OldVideoBuf,MaxVideoBufSize);
  VideoBufSize:=0;
end;


function GetCapabilities: Word;
begin
  GetCapabilities:=cpColor or cpChangeCursor;
end;


procedure SetCursorPos(NewCursorX, NewCursorY: Word);
var
  pos : COORD;
begin
   pos.x:=NewCursorX;
   pos.y:=NewCursorY;
   SetConsoleCursorPosition(TextRec(Output).Handle,pos);
   CursorX:=pos.x;
   CursorY:=pos.y;
end;


function GetCursorType: Word;
begin
   GetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
   if not ConsoleCursorInfo.bvisible then
     GetCursorType:=crHidden
   else
     case ConsoleCursorInfo.dwSize of
        1..30:
          GetCursorType:=crUnderline;
        31..70:
          GetCursorType:=crHalfBlock;
        71..100:
          GetCursorType:=crBlock;
     end;
end;


procedure SetCursorType(NewType: Word);
begin
   GetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
   if newType=crHidden then
     ConsoleCursorInfo.bvisible:=false
   else
     begin
        ConsoleCursorInfo.bvisible:=true;
        case NewType of
           crUnderline:
             ConsoleCursorInfo.dwSize:=10;

           crHalfBlock:
             ConsoleCursorInfo.dwSize:=50;

           crBlock:
             ConsoleCursorInfo.dwSize:=99;
        end
     end;
   SetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
end;


function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;
begin
end;


procedure ClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize div 2,$0720);
  UpdateScreen(true);
end;


{$IFDEF FPC}
function WriteConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:pointer; dwBufferSize:COORD; dwBufferCoord:COORD;
   var lpWriteRegion:SMALL_RECT):WINBOOL; external 'kernel32' name 'WriteConsoleOutputA';
{$ENDIF}

procedure UpdateScreen(Force: Boolean);
type TmpRec = Array[0..(1024*32) - 1] of TCharInfo;

type WordRec = record
                  One, Two: Byte;
               end; { wordrec }

var
   BufSize,
   BufCoord    : COORD;
   WriteRegion : SMALL_RECT;
   LineBuf     : ^TmpRec;
   BufCounter  : Longint;
   LineCounter,
   ColCounter  : Longint;
   smallforce  : boolean;
{
begin
  if LockUpdateScreen<>0 then
   exit;
  if not force then
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        orl     %ecx,%ecx
        jz      .Lno_update
        movb    $1,force
.Lno_update:
     end;
   end;
  if Force then
   begin
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      BufCoord.X := 0;
      BufCoord.Y := 0;
      with WriteRegion do
        begin
           Top :=0;
           Left :=0;
           Bottom := ScreenHeight-1;
           Right := ScreenWidth-1;
        end;
      New(LineBuf);
      BufCounter := 0;

      for LineCounter := 1 to ScreenHeight do
        begin
           for ColCounter := 1 to ScreenWidth do
             begin
               LineBuf^[BufCounter].UniCodeChar := WordRec(VideoBuf^[BufCounter]).One;
               LineBuf^[BufCounter].Attributes := WordRec(VideoBuf^[BufCounter]).Two;

               Inc(BufCounter);
             end; { for }
        end; { for }

      WriteConsoleOutput(TextRec(Output).Handle, LineBuf, BufSize, BufCoord, WriteRegion);
      Dispose(LineBuf);

      move(VideoBuf^,OldVideoBuf^,VideoBufSize);
   end;
end;
}
var
   x1,y1,x2,y2 : longint;

begin
  if LockUpdateScreen<>0 then
   exit;
  if force then
   smallforce:=true
  else
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        orl     %ecx,%ecx
        jz      .Lno_update
        movb    $1,smallforce
.Lno_update:
     end;
   end;
  if SmallForce then
   begin
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      BufCoord.X := 0;
      BufCoord.Y := 0;
      with WriteRegion do
        begin
           Top :=0;
           Left :=0;
           Bottom := ScreenHeight-1;
           Right := ScreenWidth-1;
        end;
      New(LineBuf);
      BufCounter := 0;
      x1:=ScreenWidth+1;
      x2:=-1;
      y1:=ScreenHeight+1;
      y2:=-1;
      for LineCounter := 1 to ScreenHeight do
        begin
           for ColCounter := 1 to ScreenWidth do
             begin
               if (WordRec(VideoBuf^[BufCounter]).One<>WordRec(OldVideoBuf^[BufCounter]).One) or
                 (WordRec(VideoBuf^[BufCounter]).Two<>WordRec(OldVideoBuf^[BufCounter]).Two) then
                 begin
                    if ColCounter<x1 then
                      x1:=ColCounter;
                    if ColCounter>x2 then
                      x2:=ColCounter;
                    if LineCounter<y1 then
                      y1:=LineCounter;
                    if LineCounter>y2 then
                      y2:=LineCounter;
                 end;
               LineBuf^[BufCounter].UniCodeChar := WordRec(VideoBuf^[BufCounter]).One;
               { If (WordRec(VideoBuf^[BufCounter]).Two and $80)<>0 then
                 LineBuf^[BufCounter].Attributes := $100+WordRec(VideoBuf^[BufCounter]).Two
               else }
                 LineBuf^[BufCounter].Attributes := WordRec(VideoBuf^[BufCounter]).Two;

               Inc(BufCounter);
             end; { for }
        end; { for }
      BufSize.X := ScreenWidth;
      BufSize.Y := ScreenHeight;

      with WriteRegion do
        begin
           if force then
             begin
               Top := 0;
               Left :=0;
               Bottom := ScreenHeight-1;
               Right := ScreenWidth-1;
               BufCoord.X := 0;
               BufCoord.Y := 0;
             end
           else
             begin
               Top := y1-1;
               Left :=x1-1;
               Bottom := y2-1;
               Right := x2-1;
               BufCoord.X := x1-1;
               BufCoord.Y := y1-1;
             end;
        end;
      {
      writeln('X1: ',x1);
      writeln('Y1: ',y1);
      writeln('X2: ',x2);
      writeln('Y2: ',y2);
      }
      WriteConsoleOutput(TextRec(Output).Handle, LineBuf, BufSize, BufCoord, WriteRegion);
      Dispose(LineBuf);

      move(VideoBuf^,OldVideoBuf^,VideoBufSize);
   end;
end;

procedure RegisterVideoModes;
begin
  { don't know what to do for win32 (FK) }
  RegisterVideoMode(80, 25, True, @DefaultVideoModeSelector, $00000003);
end;

{
  $Log$
  Revision 1.1  2000-07-13 06:29:41  michael
  + Initial import

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.2  1999/12/09 21:36:47  pierre
   * freeze screen size

  Revision 1.9  1999/11/24 23:37:00  peter
    * moved to packages dir

  Revision 1.8  1999/10/14 10:13:57  pierre
   * Screen size problem solved

  Revision 1.7  1999/09/22 12:57:38  pierre
   + support for Screen switches : ClearScreen removed

  Revision 1.6  1999/08/01 16:10:27  florian
    * fixed cursor size

  Revision 1.5  1999/07/14 22:04:04  florian
    * noch mehr Fehler behoben, TV-Programme laufen nun so lala

  Revision 1.4  1999/07/11 21:57:48  florian
    * small fixes to get at least some output

  Revision 1.3  1999/06/21 16:43:55  peter
    * win32 updates from Maarten Bekkers

  Revision 1.2  1999/01/08 16:50:05  florian
    + complete, but undebugged implementation

  Revision 1.1  1999/01/08 14:37:03  florian
    + initial version, not working yet
}
