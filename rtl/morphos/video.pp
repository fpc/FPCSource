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


{ Amiga specific calls, to help interaction between Keyboard, Mouse and
  Video units, and Free Vision }
procedure GotCloseWindow;
function HasCloseWindow: boolean;
procedure GotResizeWindow;
function HasResizeWindow(var winw:longint; var winh: longint): boolean;

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
   cursorType: word;
   oldcursorType: word;

   gotCloseWindowMsg: boolean;
   gotResizeWindowMsg: boolean;

procedure SysInitVideo;
var counter: longint;
begin
   InitGraphicsLibrary;
   InitIntuitionLibrary;

   // fill videobuf and oldvideobuf with different bytes, to allow proper first draw
   FillDword(VideoBuf^,VideoBufSize Div 4,$1234D3AD);
   FillDword(OldVideoBuf^,VideoBufSize Div 4,$4321BEEF);

   videoWindow:=OpenWindowTags(Nil, [
      WA_Left,50,
      WA_Top,50,
      WA_InnerWidth,80*8,
      WA_InnerHeight,25*16,
      WA_MaxWidth,32768,
      WA_MaxHeight,32768,
//      WA_IDCMP,IDCMP_MOUSEBUTTONS Or IDCMP_RAWKEYS,
      WA_IDCMP,IDCMP_VANILLAKEY Or IDCMP_RAWKEY Or
               IDCMP_CLOSEWINDOW Or IDCMP_CHANGEWINDOW,
      WA_Title,DWord(PChar('Free Pascal Video Output')),
      WA_Flags,(WFLG_GIMMEZEROZERO Or WFLG_SMART_REFRESH Or WFLG_NOCAREREFRESH Or 
                WFLG_ACTIVATE Or WFLG_DRAGBAR Or WFLG_DEPTHGADGET Or
                WFLG_SIZEGADGET Or WFLG_SIZEBBOTTOM Or
                WFLG_CLOSEGADGET)
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
   cursorType:=crHidden;
   oldcursorType:=crHidden;

   gotCloseWindowMsg:=false;
   gotResizeWindowMsg:=false;
end;


procedure SysDoneVideo;
var counter: longint;
begin
   if videoWindow<>nil then CloseWindow(videoWindow);
   for counter:=0 to 15 do ReleasePen(videoColorMap,videoPens[counter]);
end;



function SysSetVideoMode (Const Mode : TVideoMode) : Boolean;

var
  I : Integer;
  dx : integer;
  dy : integer;
begin
  dx := (Mode.col * 8) - videoWindow^.GZZWidth;
  dy := (Mode.row * 16) - videoWindow^.GZZHeight;
  SizeWindow(videoWindow,dx,dy);
  
  ScreenWidth:=Mode.col;
  ScreenHeight:=Mode.row;
  ScreenColor:=Mode.color;
  SysSetVideoMode:=true;
end;


procedure SysClearScreen;
begin
  UpdateScreen(true);
end;


procedure DrawChar(x,y: longint; crType: word);
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

  if crType <> crBlock then begin
    SetABPenDrMd(videoWindow^.RPort,videoPens[tmpFGColor],videoPens[tmpBGColor],JAM2);
  end else begin
    { in case of block cursor, swap fg/bg colors 
      and BltTemplate() below will take care of everything }
    SetABPenDrMd(videoWindow^.RPort,videoPens[tmpBGColor],videoPens[tmpFGColor],JAM2);
  end;

  BltTemplate(@vgafont[tmpChar,0],0,1,videoWindow^.RPort,sX,sY,8,16);

  if crType = crUnderLine then begin
    { draw two lines at the bottom of the char, in case of underline cursor }
    gfxMove(videoWindow^.RPort,sX,sY+14); Draw(videoWindow^.RPort,sX+7,sY+14);
    gfxMove(videoWindow^.RPort,sX,sY+15); Draw(videoWindow^.RPort,sX+7,sY+15);
  end;
end;


procedure SysUpdateScreen(force: boolean);
var
   BufCounter  : Longint;
   smallforce  : boolean;
   cursormoved : boolean;
   counter, counterX, counterY: longint;
begin
  smallforce:=false;
  cursormoved:=false;

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
          DrawChar(counterX,counterY,crHidden);
        Inc(BufCounter);
      end;
    end;
    move(VideoBuf^,OldVideoBuf^,VideoBufSize);
  end;

  if (cursorType<>oldcursorType) or 
     (CursorX<>oldCursorX) or (CursorY<>oldCursorY) or
     smallforce then begin
    DrawChar(oldCursorY,oldCursorX,crHidden);
    DrawChar(CursorY,CursorX,cursorType);
    oldCursorX:=CursorX;
    oldCursorY:=CursorY;
    oldcursorType:=cursorType;
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
  SysGetCursorType:=cursorType;
end;


procedure SysSetCursorType(NewType: Word);
begin
  cursorType:=newType;
  { FIXME: halfBlock cursors are not supported for now 
           by the rendering code }
  if cursorType = crHalfBlock then cursorType:=crBlock;

  SysUpdateScreen(false);
end;


// Amiga specific calls
procedure GotCloseWindow;
begin
  gotCloseWindowMsg:=true;
end;

function HasCloseWindow: boolean;
begin
  HasCloseWindow:=gotCloseWindowMsg;
  gotCloseWindowMsg:=false;
end;

procedure GotResizeWindow;
begin
  gotResizeWindowMsg:=true;
end;

function HasResizeWindow(var winw:longint; var winh: longint): boolean;
begin
  HasResizeWindow:=gotResizeWindowMsg;
  winw:=videoWindow^.GZZWidth div 8;
  winh:=videoWindow^.GZZHeight div 16;
  gotResizeWindowMsg:=false;
end;


const
  SysVideoDriver : TVideoDriver = (
    InitDriver : @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    ClearScreen : @SysClearScreen;
    SetVideoMode : @SysSetVideoMode;
    GetVideoModeCount : nil;
    GetVideoModeData : nil;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities
  );


initialization
  SetVideoDriver(SysVideoDriver);
end.
