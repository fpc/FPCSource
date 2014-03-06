{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for netware

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ 2001/04/16 armin: first version for netware
  2002/02/26 armin: changes for current fpc }
unit Video;
interface

{$i videoh.inc}

implementation

uses
  dos;

{$i video.inc}
{$i nwsys.inc}

var
  MaxVideoBufSize : DWord;
  VideoBufAllocated: boolean;


procedure SysInitVideo;
VAR height,width : WORD;
    startline, endline : BYTE;
begin
  DoneVideo;
  ScreenColor:= (_IsColorMonitor <> 0);
  _GetSizeOfScreen (height, width);
  ScreenWidth := width;
  ScreenHeight:= height;

  { TDrawBuffer only has FVMaxWidth elements
    larger values lead to crashes }
  if ScreenWidth> FVMaxWidth then
    ScreenWidth:=FVMaxWidth;

  CursorX := _wherex;
  CursorY := _wherey;
  _GetCursorShape (startline,endline);
  {if not ConsoleCursorInfo.bvisible then
    CursorLines:=0
  else
    CursorLines:=ConsoleCursorInfo.dwSize;}

  { allocate back buffer }
  MaxVideoBufSize:= ScreenWidth * ScreenHeight * 2;
  VideoBufSize   := ScreenWidth * ScreenHeight * 2;

  LockUpdateScreen := 0;
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
  _GotoXY (NewCursorX, NewCursorY);
end;


function SysGetCursorType: Word;
var startline, endline : byte;
begin
  _GetCursorShape (startline, endline);
  CASE startline of
    1 : SysGetCursorType := crBlock;
    5 : SysGetCursorType := crHalfBlock
    ELSE
       SysGetCursorType := crUnderline;
  END;
  {crHidden ?}
end;


procedure SysSetCursorType(NewType: Word);
begin
   if newType=crHidden then
     _HideInputCursor
   else
     begin
        case NewType of
           crUnderline:
             _SetCursorShape (9,$A);
           crHalfBlock:
             _SetCursorShape (5,$A);
           crBlock:
             _SetCursorShape (1,$A);
        end;
        _DisplayInputCursor;
     end;
end;


{procedure ClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize div 2,$0720);
  UpdateScreen(true);
end;}


procedure SysUpdateScreen(Force: Boolean);
begin
  if VideoBuf = nil then exit;
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
    _CopyToScreenMemory (ScreenHeight, ScreenWidth, VideoBuf, 0, 0);
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
  VideoBufAllocated := false;
  VideoBufSize := 0;
  VideoBuf := nil;
  SetVideoDriver (SysVideoDriver);
end.
