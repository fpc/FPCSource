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

  TVideoBuf = array[0..3999] of TVideoCell;
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
  while assigned(modes) do
   begin
     p:=modes;
     modes:=modes^.next;
     dispose(p);
   end;
end;


begin
  RegisterVideoModes;
  OldExitProc:=ExitProc;
  ExitProc:=@UnRegisterVideoModes;
end.
{
  $Log$
  Revision 1.3  2000-02-29 11:43:16  pierre
    Common renamed APIComm to avoid problems with free vision

  Revision 1.2  2000/02/06 14:28:19  florian
    * mouse support for vesa resolutions under go32v2, needs currently the define
      custommouse

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.9  1999/03/14 22:15:48  florian
    * my last changes doesn't work correctly, fixed more
      the screen height calculation works incorrect in 80x50 mode

  Revision 1.8  1999/03/14 17:43:00  florian
    + 80x50 mode support added
    * some bugs in VESA mode support removed

  Revision 1.7  1999/03/13 17:34:01  florian
    * again SetVideoMode fixed

  Revision 1.6  1999/03/13 17:30:47  florian
    * endless loop in SetVideoMode fixed

  Revision 1.5  1999/02/22 12:46:15  peter
    + lowascii boolean if ascii < #32 is handled correctly

  Revision 1.4  1998/12/23 22:41:08  peter
    + color consts

  Revision 1.3  1998/12/11 00:13:18  peter
    + SetMouseXY
    * use far for exitproc procedure

  Revision 1.2  1998/12/08 10:09:56  peter
    * unregister videomodes at the end

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.14  1998/11/01 20:29:10  peter
    + lockupdatescreen counter to not let updatescreen() update

  Revision 1.13  1998/10/28 21:18:23  peter
    * more fixes

  Revision 1.12  1998/10/28 00:02:07  peter
    + mouse
    + video.clearscreen, video.videobufsize

  Revision 1.11  1998/10/27 11:24:20  peter
    * fixed the log


   Date       Version   Who       Comments
   07/06/97   0.1       bazsi     Initial implementation
                                  Console mode (Linux) ready
   07/28/97   0.2       bazsi     Linux on foreign terminals ready
   08/27/97   0.3       bazsi     Noone else did it, so I did it: DOS support
                                  (I had to boot DOS... ;-(
                                  Mode-switching implemented
   07/28/97   0.3.1     bazsi     added support for terminfo. remote terminal
                                  support is broken now
}