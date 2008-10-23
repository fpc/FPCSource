{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006 by Karoly Balogh
    member of the Free Pascal development team

    Video unit for Amiga and MorphOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;

interface

uses
  intuition;

{$i videoh.inc}

var
   videoWindow   : pWindow; 

implementation

uses
//   dos
   exec,graphics;

{$i video.inc}

{$i videodata.inc}

const
    LastCursorType: word = crUnderline;
    OrigScreen: PVideoBuf = nil;
    OrigScreenSize: cardinal = 0;

var
   videoColorMap : pColorMap;
   videoPens     : array[0..15] of longint;

   oldCursorX, oldCursorY: longint;
   visibleCursor: boolean;
   oldvisibleCursor: boolean;

procedure SysInitVideo;
var counter: longint;
begin
   writeln('sysinitvideo');
   InitGraphicsLibrary;
   InitIntuitionLibrary;
{
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
}
   videoWindow:=OpenWindowTags(Nil, [
      WA_Left,50,
      WA_Top,50,
      WA_InnerWidth,80*8,
      WA_InnerHeight,25*16,
//      WA_IDCMP,IDCMP_MOUSEBUTTONS Or IDCMP_RAWKEYS,
      WA_IDCMP,IDCMP_VANILLAKEY Or IDCMP_RAWKEY,
      WA_Title,DWord(PChar('Free Pascal Video Output')),
      WA_Flags,(WFLG_GIMMEZEROZERO Or WFLG_SMART_REFRESH Or WFLG_NOCAREREFRESH Or WFLG_ACTIVATE Or WFLG_DRAGBAR Or WFLG_DEPTHGADGET)
   ]);

   ScreenWidth := 80;
   ScreenHeight := 25;
   ScreenColor := true;

   videoColorMap := pScreen(videoWindow^.WScreen)^.ViewPort.ColorMap;
   for counter:=0 to 15 do begin
     videoPens[counter]:=ObtainPen(videoColorMap,-1,
         vgacolors[counter,0] shl 24,vgacolors[counter,1] shl 24,vgacolors[counter,2] shl 24,
         PEN_EXCLUSIVE);
//     writeln(videoPens[counter]);
     // XXX: do checks for -1 colors (KB)
   end;

   CursorX:=0;
   CursorY:=0;
   oldCursorX:=0;
   oldCursorY:=0;
   visibleCursor:=true;
   oldvisibleCursor:=true;
end;


procedure SysDoneVideo;
var counter: longint;
begin
   if videoWindow<>nil then CloseWindow(videoWindow);
   for counter:=0 to 15 do ReleasePen(videoColorMap,videoPens[counter]);
 
{
  SetConsoleScreenBufferSize (TextRec (Output).Handle, OrigConsoleInfo.dwSize);
  SetConsoleWindowInfo (cardinal (TextRec (Output).Handle), true, OrigConsoleInfo.srWindow);
  SetConsoleCursorInfo(TextRec(Output).Handle, OrigConsoleCursorInfo);
  SetConsoleCP(OrigCP);
}
end;



function SysVideoModeSelector (const VideoMode: TVideoMode): boolean;

{
var MI: Console_Screen_Buffer_Info;
    C: Coord;
    SR: Small_Rect;
}
begin
{
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
}
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


procedure DrawChar(x,y: longint; bitmap: pBitmap; drawCursor: boolean);
var tmpCharData: word;
    tmpChar    : byte;
    tmpFGColor : byte;
    tmpBGColor : byte;
var
    counterX, counterY:longint;
    sX,sY: longint;
begin
  tmpCharData:=VideoBuf^[y*ScreenWidth+x];
  tmpChar    :=tmpCharData and $0ff;
  tmpFGColor :=(tmpCharData shr 8) and %00001111;
  tmpBGColor :=(tmpCharData shr 12) and %00000111;
  
  sX:=x*8;
  sY:=y*16;
  
  SetAPen(videoWindow^.Rport,videoPens[tmpFGColor]);
  SetBPen(videoWindow^.RPort,videoPens[tmpBGColor]);
  BltTemplate(@vgafont[tmpChar,0],0,1,videoWindow^.RPort,sX,sY,8,16);
  
  if drawCursor then begin
     gfxMove(videoWindow^.RPort,sX,sY+14); Draw(videoWindow^.RPort,sX+7,sY+14);
     gfxMove(videoWindow^.RPort,sX,sY+15); Draw(videoWindow^.RPort,sX+7,sY+15);
  end;
end;


procedure SysUpdateScreen(force: boolean);
var
   BufCounter  : Longint;
   smallforce  : boolean;

   counter, counterX, counterY: longint;
var
   tmpBitmap   : tBitmap;
begin
  if force then
    smallforce:=true
  else begin
    counter:=0;
    while not smallforce and (counter<(VideoBufSize div 4)-1) do begin
      if PDWord(VideoBuf)[counter]<>PDWord(OldVideoBuf)[counter] then smallforce:=true;
      counter+=1;
    end;
  end;

  BufCounter:=0;
  if smallforce then begin
    for counterY:=0 to ScreenHeight-1 do begin
      for counterX:=0 to ScreenWidth-1 do begin
        if VideoBuf^[BufCounter]<>OldVideoBuf^[BufCounter] then
          DrawChar(counterX,counterY,@tmpBitmap,false);
        Inc(BufCounter);
      end;
    end;
    move(VideoBuf^,OldVideoBuf^,VideoBufSize);
  end;

  if (oldvisibleCursor<>visibleCursor) or (CursorX<>oldCursorX) or (CursorY<>oldCursorY) then begin
    writeln('kurzor:',cursorx,' ',cursory);
    DrawChar(oldCursorY,oldCursorX,@tmpBitmap,false);
    DrawChar(CursorY,CursorX,@tmpBitmap,visibleCursor);
    oldCursorX:=CursorX;
    oldCursorY:=CursorY;
    oldVisibleCursor:=visibleCursor;
  end;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
begin
  CursorX:=NewCursorY;
  CursorY:=NewCursorX;
  SysUpdateScreen(false);
end;

function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpColor or cpChangeCursor;
end;

function SysGetCursorType: Word;
begin
  if not visibleCursor then SysGetCursorType:=crHidden 
                       else SysGetCursorType:=crUnderline;
 
{
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
}
end;


procedure SysSetCursorType(NewType: Word);
begin
  if newType=crHidden then visibleCursor:=false
                      else visibleCursor:=true;
  SysUpdateScreen(false);
{
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
}
end;



const
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


initialization
  SetVideoDriver(SysVideoDriver);
end.
