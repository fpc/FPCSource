{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for Win32

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;
interface

{$i videoh.inc}

implementation

uses
  windows,dos;

{$i video.inc}

const
    LastCursorType: word = crUnderline;
    OrigScreen: PVideoBuf = nil;
    OrigScreenSize: cardinal = 0;


var ConsoleInfo : TConsoleScreenBufferInfo;
    ConsoleCursorInfo : TConsoleCursorInfo;

    OrigCP: cardinal;
    OrigConsoleCursorInfo : TConsoleCursorInfo;
    OrigConsoleInfo : TConsoleScreenBufferInfo;

procedure SysInitVideo;

begin
  ScreenColor:=true;
  GetConsoleScreenBufferInfo(TextRec(Output).Handle, OrigConsoleInfo);
  GetConsoleCursorInfo(TextRec(Output).Handle, OrigConsoleCursorInfo);
  OrigCP := GetConsoleCP;
  ConsoleInfo:=OrigConsoleInfo;
  ConsoleCursorInfo:=OrigConsoleCursorInfo;
  {
    About the ConsoleCursorInfo record: There are 3 possible
    structures in it that can be regarded as the 'screen':
    - dwsize   : contains the cols & row in current screen buffer.
    - srwindow : Coordinates (relative to buffer) of upper left
                 & lower right corners of visible console.
    - dmMaximumWindowSize : Maximal size of Screen buffer.
    The first implementation of video used srWindow. After some
    bug-reports, this was switched to dwMaximumWindowSize.
  }
  with ConsoleInfo.dwMaximumWindowSize do
    begin
    ScreenWidth:=X;
    ScreenHeight:=Y;
    end;
  { TDrawBuffer only has FVMaxWidth elements
    larger values lead to crashes }
  if ScreenWidth> FVMaxWidth then
    ScreenWidth:=FVMaxWidth;
  CursorX:=ConsoleInfo.dwCursorPosition.x;
  CursorY:=ConsoleInfo.dwCursorPosition.y;
  if not ConsoleCursorInfo.bvisible then
    CursorLines:=0
  else
    CursorLines:=ConsoleCursorInfo.dwSize;
end;


procedure SysDoneVideo;
begin
  SetConsoleScreenBufferSize (TextRec (Output).Handle, OrigConsoleInfo.dwSize);
  SetConsoleWindowInfo (cardinal (TextRec (Output).Handle), true, OrigConsoleInfo.srWindow);
  SetConsoleCursorInfo(TextRec(Output).Handle, OrigConsoleCursorInfo);
  SetConsoleCP(OrigCP);
end;


function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpColor or cpChangeCursor;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
var
  pos : COORD;
begin
   pos.x:=NewCursorX;
   pos.y:=NewCursorY;
   SetConsoleCursorPosition(TextRec(Output).Handle,pos);
   CursorX:=pos.x;
   CursorY:=pos.y;
end;


function SysGetCursorType: Word;
begin
   GetConsoleCursorInfo(TextRec(Output).Handle,ConsoleCursorInfo);
   if not ConsoleCursorInfo.bvisible then
     SysGetCursorType:=crHidden
   else
     case ConsoleCursorInfo.dwSize of
        1..30:
          SysGetCursorType:=crUnderline;
        31..70:
          SysGetCursorType:=crHalfBlock;
        71..100:
          SysGetCursorType:=crBlock;
     end;
end;


procedure SysSetCursorType(NewType: Word);
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

function SysVideoModeSelector (const VideoMode: TVideoMode): boolean;

var MI: Console_Screen_Buffer_Info;
    C: Coord;
    SR: Small_Rect;

begin
  if not (GetConsoleScreenBufferInfo (TextRec (Output).Handle, MI)) then
    SysVideoModeSelector := false
  else
    begin
      with MI do
        begin
          C.X := VideoMode.Col;
          C.Y := VideoMode.Row;
        end;
      with SR do
        begin
          Top := 0;
          Left := 0;
          { First, we need to make sure we reach the minimum window size
            to always fit in the new buffer after changing buffer size. }
          Right := MI.srWindow.Right - MI.srWindow.Left;
          if VideoMode.Col <= Right then
            Right := Pred (VideoMode.Col);
          Bottom := MI.srWindow.Bottom - MI.srWindow.Top;
          if VideoMode.Row <= Bottom then
            Bottom := Pred (VideoMode.Row);
        end;
      if SetConsoleWindowInfo (cardinal (TextRec (Output).Handle), true, SR) then
        if SetConsoleScreenBufferSize (TextRec (Output).Handle, C) then
          begin
            with SR do
              begin
                { Now, we can resize the window to the final size. }
                Right := Pred (VideoMode.Col);
                Bottom := Pred (VideoMode.Row);
              end;
            if SetConsoleWindowInfo (cardinal (TextRec (Output).Handle), true, SR) then
              begin
                SysVideoModeSelector := true;
                SetCursorType (LastCursorType);
                ClearScreen;
              end
            else
              begin
                SysVideoModeSelector := false;
                SetConsoleScreenBufferSize (TextRec (Output).Handle, MI.dwSize);
                SetConsoleWindowInfo (cardinal (TextRec (Output).Handle), true, MI.srWindow);
                SetCursorType (LastCursorType);
              end
          end
        else
          begin
            SysVideoModeSelector := false;
            SetConsoleWindowInfo (cardinal (TextRec (Output).Handle), true, MI.srWindow);
            SetCursorType (LastCursorType);
          end
      else
        SysVideoModeSelector := false;
    end;
end;

Const
  SysVideoModeCount = 6;
  SysVMD : Array[0..SysVideoModeCount-1] of TVideoMode = (
   (Col: 40; Row: 25; Color: True),
   (Col: 80; Row: 25; Color: True),
   (Col: 80; Row: 30; Color: True),
   (Col: 80; Row: 43; Color: True),
   (Col: 80; Row: 50; Color: True),
   (Col: 80; Row: 25; Color: True) // Reserved for TargetEntry
  );


Function SysSetVideoMode (Const Mode : TVideoMode) : Boolean;

Var
  I : Integer;

begin
  I:=SysVideoModeCount-1;
  SysSetVideoMode:=False;
  While (I>=0) and Not SysSetVideoMode do
    If (Mode.col=SysVMD[i].col) and
       (Mode.Row=SysVMD[i].Row) and
       (Mode.Color=SysVMD[i].Color) then
      SysSetVideoMode:=True
    else
      Dec(I);
  If SysSetVideoMode then
    begin
    if SysVideoModeSelector(Mode) then
      begin
      ScreenWidth:=SysVMD[I].Col;
      ScreenHeight:=SysVMD[I].Row;
      ScreenColor:=SysVMD[I].Color;
      end else SysSetVideoMode := false;
    end;
end;

Function SysGetVideoModeData (Index : Word; Var Data : TVideoMode) : boolean;

begin
  SysGetVideoModeData:=(Index<=high(SysVMD));
  If SysGetVideoModeData then
    Data:=SysVMD[Index];
end;

Function SysGetVideoModeCount : Word;

begin
  SysGetVideoModeCount:=SysVideoModeCount;
end;

procedure SysClearScreen;
begin
  UpdateScreen(true);
end;

{$IFDEF FPC}
function WriteConsoleOutput(hConsoleOutput:HANDLE; lpBuffer:pointer; dwBufferSize:COORD; dwBufferCoord:COORD;
   var lpWriteRegion:SMALL_RECT):WINBOOL; stdcall;external 'kernel32' name 'WriteConsoleOutputA';
{$ENDIF}

procedure SysUpdateScreen(Force: Boolean);
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
   x1,y1,x2,y2 : longint;
begin
  if force then
   smallforce:=true
  else
   begin
     asm
        pushl   %esi
        pushl   %edi
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        setne   smallforce
        popl    %edi
        popl    %esi
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
               LineBuf^[BufCounter].UniCodeChar := Widechar(WordRec(VideoBuf^[BufCounter]).One);
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

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    ClearScreen : @SysClearScreen;
    SetVideoMode : @SysSetVideoMode;
    GetVideoModeCount : @SysGetVideoModeCount;
    GetVideoModeData : @SysGetVideoModeData;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities

  );

procedure TargetEntry;
var
  C: Coord;
  SR: Small_Rect;
  VioMode: TConsoleScreenBufferInfo;
begin
  GetConsoleScreenBufferInfo (TextRec (Output).Handle, VioMode);
  { Register the curent video mode in reserved slot in System Modes}
  with VioMode do
    begin
      {Assume we have at least 16 colours available in "colour" modes}
      SysVMD[SysVideoModeCount-1].Col:=dwMaximumWindowSize.X;
      SysVMD[SysVideoModeCount-1].Row:=dwMaximumWindowSize.Y;
      SysVMD[SysVideoModeCount-1].Color:=true;
      OrigScreenSize := max(dwMaximumWindowSize.X,dwSize.X) * max(dwMaximumWindowSize.Y,dwSize.Y) * SizeOf (Char_Info);
    end;
  GetMem (OrigScreen, OrigScreenSize);
  with C do
    begin
      X := 0;
      Y := 0;
    end;
  with SR do
    begin
      Top := 0;
      Left := 0;
      Right := Pred (VioMode.dwSize.X);
      Bottom := Pred (VioMode.dwSize.Y);
    end;
  if not (ReadConsoleOutput (TextRec (Output).Handle, OrigScreen, VioMode.dwSize, C, SR)) then
    begin
      FreeMem (OrigScreen, OrigScreenSize);
      OrigScreen := nil;
      OrigScreenSize := 0;
    end;
end;


initialization
  SetVideoDriver(SysVideoDriver);
  TargetEntry;
end.
