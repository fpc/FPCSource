{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Armin Diehl
    member of the Free Pascal development team

    Video unit for netware libc

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
  Libc;

{$i video.inc}

var
  MaxVideoBufSize : DWord;
  ScreenHandle : scr_t;
  CursorIsHidden : boolean;


procedure SysSetCursorType(NewType: Word);
begin
   if newType=crHidden then
   begin
     Libc.DisableInputCursor (ScreenHandle);
     cursorIsHidden := true;
   end else
   begin
     cursorIsHidden := false;
     case NewType of
       crUnderline: Libc.SetCursorStyle (ScreenHandle,CURSOR_NORMAL);
       crHalfBlock: Libc.SetCursorStyle (ScreenHandle,CURSOR_TOP);
       crBlock    : Libc.SetCursorStyle (ScreenHandle,CURSOR_BLOCK);
     end;
     Libc.EnableInputCursor (ScreenHandle);
   end;
end;


procedure SysInitVideo;
VAR height,width,x,y : WORD;
    startline, endline : BYTE;
    sType,sColorFlag : dword;
begin
  DoneVideo;
  Libc.ReturnScreenType (sType,sColorFlag);
  ScreenColor:= (sColorFlag > 0);
  Libc.GetScreenSize(height,width);
  ScreenWidth := width;
  ScreenHeight:= height;

  { TDrawBuffer only has FVMaxWidth elements
    larger values lead to crashes }
  if ScreenWidth> FVMaxWidth then
    ScreenWidth:=FVMaxWidth;
  GetOutputCursorPosition(ScreenHandle,y,x);
  CursorX := x;
  CursorY := y;
  SysSetCursorType (crBlock);
end;


procedure SysDoneVideo;
begin
  SetCursorType(crUnderLine);
end;


function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpColor or cpChangeCursor;
end;

procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
begin
  Libc.PositionInputCursor(ScreenHandle,NewCursorY,NewCursorX);
end;



function SysGetCursorType: Word;
var style : word;
begin
  if cursorIsHidden then
  begin
    SysGetCursorType := crHidden;
    exit;
  end;
  Libc.GetCursorStyle (ScreenHandle,style);
  case style of
    CURSOR_THICK  : SysGetCursorType := crBlock;
    CURSOR_BLOCK  : SysGetCursorType := crBlock;
    CURSOR_TOP    : SysGetCursorType := crHalfBlock
  else
    SysGetCursorType := crUnderline;
  end;
end;


procedure SysUpdateScreen(Force: Boolean);
begin
  if VideoBuf = nil then
    exit;
  if (LockUpdateScreen<>0) or (VideoBufSize = 0) then
   exit;
  if not force then
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
        setne   force
        popl    %edi
        popl    %esi
     end;
   end;
  if Force then
    Libc.RestoreScreenArea(ScreenHandle,0,0,ScreenHeight,ScreenWidth,VideoBuf);
end;


Const
  SysVideoModeCount = 1;
  SysVMD : Array[0..SysVideoModeCount-1] of TVideoMode = (
       (Col: 80; Row : 25;  Color : True));

Function SysSetVideoMode (Const Mode : TVideoMode) : Boolean;
begin
  SysSetVideoMode := ((Mode.Col = 80) AND (Mode.Row = 25) AND (Mode.Color));
end;

Function SysGetVideoModeData (Index : Word; Var Data : TVideoMode) : boolean;
begin
  SysGetVideoModeData:=(Index<=SysVideoModeCount);
  If SysGetVideoModeData then
    Data:=SysVMD[Index];
end;

Function SysGetVideoModeCount : Word;
begin
  SysGetVideoModeCount:=SysVideoModeCount;
end;

Const
  SysVideoDriver : TVideoDriver = (
  InitDriver        : @SysInitVideo;
  DoneDriver        : @SysDoneVideo;
  UpdateScreen      : @SysUpdateScreen;
  ClearScreen       : Nil;
  SetVideoMode      : @SysSetVideoMode;
  GetVideoModeCount : @SysGetVideoModeCount;
  GetVideoModeData  : @SysGetVideoModedata;
  SetCursorPos      : @SysSetCursorPos;
  GetCursorType     : @SysGetCursorType;
  SetCursorType     : @SysSetCursorType;
  GetCapabilities   : @SysGetCapabilities
);



initialization
  VideoBuf := nil;
  VideoBufSize := 0;
  ScreenHandle := Libc.getscreenhandle;
  SetVideoDriver (SysVideoDriver);
end.
