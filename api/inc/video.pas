{
   $Id$

   System independent low-level video interface
   Based on Daniel Mantion's interface designs

   Copyright (c) 1997 Balazs Scheidler (bazsi@balabit.hu)

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

 ****************************************************************************

  Todo:
   - getting escape sequences from termcap
   - implement library on other platforms (OS/2)

 ****************************************************************************}
unit Video;

interface

uses
  ApiComm;

{$i platform.inc}

type
  PVideoMode = ^TVideoMode;
  TVideoMode = record
    Col,Row : Word;
    Color   : Boolean;
  end;
  TVideoModeSelector = function (const VideoMode: TVideoMode; Params: Longint): Boolean;

  TVideoCell = Word;
  PVideoCell = ^TVideoCell;

  TVideoBuf = array[0..32767] of TVideoCell;
  PVideoBuf = ^TVideoBuf;

const
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

  { Capabilities bitmask }
  cpUnderLine     = $0001;
  cpBlink         = $0002;
  cpColor         = $0004;
  cpChangeFont    = $0008;
  cpChangeMode    = $0010;
  cpChangeCursor  = $0020;

  { Possible cursor types }
  crHidden        = 0;
  crUnderLine     = 1;
  crBlock         = 2;
  crHalfBlock     = 3;

  { Possible error codes }
  vioOK              = 0;
  errVioInit         = errVioBase + 1; { Initialization error, shouldn't occur on DOS, but may
                         on Linux }
  errVioNotSupported = errVioBase + 2; { call to an unsupported function }
  errVioNoSuchMode   = errVioBase + 3; { No such video mode }

const
  ScreenWidth  : Word = 0;
  ScreenHeight : Word = 0;

var
  ScreenColor  : Boolean;
  CursorX,
  CursorY      : Word;
  LockUpdateScreen : Word;
  VideoBuf     : PVideoBuf;
  VideoBufSize : Longint;
  CursorLines  : Byte;
const
  LowAscii     : Boolean=true;
  FVMaxWidth = 132;

procedure InitVideo;
{ Initializes the video subsystem }
procedure DoneVideo;
{ Deinitializes the video subsystem }
function GetCapabilities: Word;
{ Return the capabilities of the current environment }
procedure ClearScreen;
{ Clears the screen }
procedure UpdateScreen(Force: Boolean);
{ Force specifies whether the whole screen has to be redrawn, or (if target
  platform supports it) its parts only }
procedure SetCursorPos(NewCursorX, NewCursorY: Word);
{ Position the cursor to the given position }
function GetCursorType: Word;
{ Return the cursor type: Hidden, UnderLine or Block }
procedure SetCursorType(NewType: Word);
{ Set the cursor to the given type }
function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;

procedure GetVideoMode(var Mode: TVideoMode);
{ Return dimensions of the current video mode }
procedure SetVideoMode(Mode: TVideoMode);
{ Set video-mode to have Mode dimensions, may return errVioNoSuchMode }
procedure RegisterVideoMode(Col, Row: Word; Color: Boolean; VideoModeSelector: TVideoModeSelector; Params: Longint);
{ Registers a video mode to be selectable by SetVideoMode }

{ moved to interface because we need a way to retrieve the modes }
{ System independent part }
type
  PVideoModeList = ^TVideoModeList;
  TVideoModeList = record
    Col, Row: Word;
    Color: Boolean;
    VideoModeSelector: TVideoModeSelector;
    Params: Longint;
    Next: PVideoModeList;
  end;

const
  Modes: PVideoModeList = nil;
{$ifdef go32v2}
var
  VideoSeg    : word;
{$endif go32v2}
implementation


{ Include system dependent part }
{ must declare TargetEntry and TargetExit procedures
  which can be empty of course }
{$i video.inc}

procedure GetVideoMode(var Mode: TVideoMode);
begin
  Mode.Col := ScreenWidth;
  Mode.Row := ScreenHeight;
  Mode.Color := ScreenColor;
end;

procedure SetVideoMode(Mode: TVideoMode);
var
  P: PVideoModeList;
begin
  P := Modes;
  while (P<>Nil) and ((P^.Row <> Mode.Row) or (P^.Col <> Mode.Col) or (P^.Color<>Mode.Color)) do
    P := P^.Next;
  if P <> nil then begin
    DoneVideo;
    ScreenWidth:=$ffff;
    ScreenHeight:=$ffff;
    P^.VideoModeSelector(PVideoMode(P)^, P^.Params);
    InitVideo;
   end
   else begin
    ErrorHandler(errVioNoSuchMode, @Mode);
  end;
end;

procedure RegisterVideoMode(Col, Row: Word; Color: Boolean; VideoModeSelector: TVideoModeSelector; Params: Longint);
var
  P: PVideoModeList;
begin
  New(P);
  P^.Col := Col;
  P^.Row := Row;
  P^.Color := Color;
  P^.VideoModeSelector := VideoModeSelector;
  P^.Params := Params;
  P^.Next := Modes;
  Modes := P;
end;


var
  OldExitProc : pointer;

procedure UnRegisterVideoModes;{$ifdef PPC_BP}far;{$endif}
var
  P: PVideoModeList;
begin
  ExitProc:=OldExitProc;
  TargetExit;
  while assigned(modes) do
   begin
     p:=modes;
     modes:=modes^.next;
     dispose(p);
   end;
end;


begin
  RegisterVideoModes;
  TargetEntry;
  OldExitProc:=ExitProc;
  ExitProc:=@UnRegisterVideoModes;
end.
{
  $Log$
  Revision 1.4  2000-10-15 09:22:40  peter
    * FVMaxWidth

  Revision 1.3  2000/10/04 11:53:31  pierre
   Add TargetEntry and TargetExit (merged)

  Revision 1.2  2000/09/24 19:52:21  hajny
    * max TVideoBuf size extended

  Revision 1.1  2000/07/13 06:29:39  michael
  + Initial import

}