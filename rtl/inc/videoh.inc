{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by the Free Pascal development team

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

type
  PVideoMode = ^TVideoMode;
  TVideoMode = record
    Col,Row : Word;
    Color   : Boolean;
  end;
  TVideoModeSelector = function (const VideoMode: TVideoMode; Params: Longint): Boolean;

  TVideoCell = Word;
  PVideoCell = ^TVideoCell;

  TVideoBuf = array[0..32759] of TVideoCell;
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
  errVioBase         = 1000;
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
  LowAscii     : Boolean = true;
  NoExtendedFrame : Boolean = false;
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

type
  TErrorHandlerReturnValue = (errRetry, errAbort, errContinue);
  { errRetry = retry the operation,
    errAbort = abort, return error code,
    errContinue = abort, without returning errorcode }

  TErrorHandler = function (Code: Longint; Info: Pointer): TErrorHandlerReturnValue;
    { ErrorHandler is the standard procedural interface for all error functions.
      Info may contain any data type specific to the error code passed to the
      function. }

function DefaultErrorHandler(AErrorCode: Longint; AErrorInfo: Pointer): TErrorHandlerReturnValue;
{ Default error handler, simply sets error code, and returns errContinue }

const
  errOk              = 0;
  ErrorCode: Longint = ErrOK;
  ErrorInfo: Pointer = nil;
  ErrorHandler: TErrorHandler = @DefaultErrorHandler;

{
  $Log$
  Revision 1.2  2001-06-06 17:20:22  jonas
    * fixed wrong typed constant procvars in preparation of my fix which will
      disallow them in FPC mode (plus some other unmerged changes since
      LAST_MERGE)

  Revision 1.1  2001/01/13 11:13:12  peter
    * API 2 RTL

}
