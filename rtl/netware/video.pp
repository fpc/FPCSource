{
    $Id$
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
{ 2001/04/16 armin: first version for netware }
unit Video;
interface

{$i videoh.inc}

implementation

uses
  dos;

{$i video.inc}
{$i nwsys.inc}

var
  OldVideoBuf : PVideoBuf;
  MaxVideoBufSize : DWord;
  VideoBufAllocated: boolean;


procedure InitVideo;
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

  GetMem(VideoBuf,MaxVideoBufSize);
  GetMem(OldVideoBuf,MaxVideoBufSize);
  VideoBufAllocated := true;

  {grab current screen contents}
  _CopyFromScreenMemory (ScreenHeight, ScreenWidth, VideoBuf, 0, 0);
  Move (VideoBuf^, OldVideoBuf^, MaxVideoBufSize);
  LockUpdateScreen := 0;

  {ClearScreen; not needed PM }
end;


procedure DoneVideo;
begin
  { ClearScreen; also not needed PM }
  SetCursorType(crUnderLine);
  { SetCursorPos(0,0); also not needed PM }
  if videoBufAllocated then
  begin
    FreeMem(VideoBuf,MaxVideoBufSize);
    FreeMem(OldVideoBuf,MaxVideoBufSize);
    videoBufAllocated := false;
  end;
  VideoBufSize:=0;
end;


function GetCapabilities: Word;
begin
  GetCapabilities:=cpColor or cpChangeCursor;
end;


procedure SetCursorPos(NewCursorX, NewCursorY: Word);
begin
  _GotoXY (NewCursorX, NewCursorY);
end;


function GetCursorType: Word;
var startline, endline : byte;
begin
  _GetCursorShape (startline, endline);
  CASE startline of
    1 : GetCursorType := crBlock;
    5 : GetCursorType := crHalfBlock
    ELSE
       GetCursorType := crUnderline;
  END;
  {crHidden ?}
end;


procedure SetCursorType(NewType: Word);
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


function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;
begin
  DefaultVideoModeSelector:=true;
end;


procedure ClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize div 2,$0720);
  UpdateScreen(true);
end;


procedure UpdateScreen(Force: Boolean);
begin
  if (LockUpdateScreen<>0) or (VideoBufSize = 0) then
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
        setne   force
     end;
   end;
  if Force then
    _CopyToScreenMemory (ScreenHeight, ScreenWidth, VideoBuf, 0, 0);
end;

procedure RegisterVideoModes;
begin
  { don't know what to do for netware }
  RegisterVideoMode(80, 25, True, @DefaultVideoModeSelector, $00000003);
end;


initialization
  VideoBufAllocated := false;
  VideoBufSize := 0;
  RegisterVideoModes;

finalization
  UnRegisterVideoModes;
end.

