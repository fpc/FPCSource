{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2006-2014 by Karoly Balogh
    member of the Free Pascal development team

    Video unit for Amiga, MorphOS and AROS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

unit Video;

{.$define VIDEODEBUG}
{.$define WITHBUFFERING}


{
  History
  2013-01-09  Add on demand support for full-screen video
}


interface

uses
  amigados, intuition, utility, sysutils;

{$i videoh.inc}


{ Amiga specific calls, to help interaction between Keyboard, Mouse and
  Video units, and Free Vision }
procedure GotCloseWindow;
function  HasCloseWindow: boolean;
procedure GotResizeWindow;
function  HasResizeWindow(var winw:longint; var winh: longint): boolean;
procedure GotRefreshWindow;
procedure ToggleCursor(forceOff: boolean);
procedure GotActiveWindow;
function HasActiveWindow: boolean;
procedure GotInactiveWindow;
function HasInactiveWindow: boolean;
procedure SetWindowTitle(const winTitle: AnsiString; const screenTitle: AnsiString);
procedure TranslateToCharXY(const X,Y: LongInt; var CX,CY: LongInt);

var
  VideoWindow: PWindow;

implementation

uses
   exec, agraphics;

procedure SysUpdateScreen(Force: Boolean); forward;

{$i video.inc}

{$i videodata.inc}

const
  VIDEOSCREENNAME = 'FPC Video Screen Output';

var
  OS_Screen             : PScreen   = nil;    // Holds optional screen pointer
  FPC_VIDEO_FULLSCREEN  : Boolean   = False;  // Global that defines when we need to attempt opening on own screen

var
  VideoColorMap         : PColorMap;
  VideoPens             : array[0..15] of LongInt;
  VideoFont             : PByte;
  VideoFontHeight       : DWord;

  OldSH, OldSW          : longint;

  OldCursorX,
  OldCursorY            : LongInt;
  CursorType            : Word;
  OldCursorType         : Word;
  CursorUpdateCnt       : Word;
  CursorUpdateSpeed     : Word;
  CursorState           : boolean;
  ForceCursorUpdate     : boolean;

  {$ifdef WITHBUFFERING}
  BitmapWidth, BitmapHeight: Integer;
  BufRp: PRastPort;
  {$endif}

  GotCloseWindowMsg     : Boolean;
  GotResizeWindowMsg    : Boolean;
  GotActiveWindowMsg    : Boolean;
  GotInactiveWindowMsg  : Boolean;
  LastL, LastT: Integer;
  LastW, LastH: Integer;
  WindowForReqSave: PWindow;
  Process: PProcess;

  FontBitmap: PBitmap;
  CharPointers: array[0..255] of Pointer;
  SrcMod: Integer = 1;

(*
  GetScreen: pScreen;

  Tries to open a custom screen, which attempt to clone the workbench,
  and returns the pointer to the screen. Result can be nil when failed
  otherwise the screen got opened correctly.
*)
function _OpenScreenTags(a: Pointer; tags: array of PtrUInt): pScreen;
begin
  _OpenScreenTags:=OpenScreenTagList(a, @tags);
end;

Function GetScreen: pScreen;
begin
  GetScreen:=_OpenScreenTags(nil,[
    SA_Title          , PtrUInt(PChar(VIDEOSCREENNAME)),
    SA_Left           , 0,
    SA_Top            , 0,
    SA_ShowTitle      , 0,    // Do not show the screen's TitleBar
    SA_Type           , PUBLICSCREEN_F, // pubscreen
    SA_PubName        , PtrUInt(PChar(VIDEOSCREENNAME)),
    SA_Quiet          , 1,
    SA_LikeWorkbench  , 1,     // Let OS
    TAG_END, TAG_END
  ]);
  {$ifdef VIDEODEBUG}
  if (GetScreen <> nil) then
    Writeln('DEBUG: Opened a new screen')
  else
    Writeln('ERROR: Failed to open new screen');
  {$endif}
end;

(*
  GetWindow: pWindow;

  Tries to create and open a window. Returns the pointer to
  the window or nil in case of failure.

  The routine keeps the global FPC_FULL_SCREEM option into
  account and act accordingly.

  In windowed mode it returns a window with another kind of
  settings then when it has to reside on it's own customscreen.
*)
function _OpenWindowTags(a: Pointer; tags: array of PtrUInt): pWindow;
begin
  _OpenWindowTags:=OpenWindowTagList(a, @tags);
end;

const
  VIDEO_IDCMP_DEFAULTS = IDCMP_RAWKEY       or
                         IDCMP_MOUSEBUTTONS or
                         IDCMP_CHANGEWINDOW or IDCMP_CLOSEWINDOW or
                         IDCMP_ACTIVEWINDOW or IDCMP_INACTIVEWINDOW or
                         IDCMP_REFRESHWINDOW or
                         IDCMP_INTUITICKS;
  { simple refresh would be nicer here, but smart refresh gives better
    results when moving around the window with the input blocked.
    (eg. compiling in the IDE) }
  VIDEO_WFLG_DEFAULTS = WFLG_RMBTRAP or WFLG_SMART_REFRESH;

Function GetWindow: PWindow;
var
  envBuf: array[0..15] of char;
  videoDefaultFlags: PtrUInt;
begin
  videoDefaultFlags:=VIDEO_WFLG_DEFAULTS;
  if GetVar('FPC_VIDEO_SIMPLEREFRESH',@envBuf,sizeof(envBuf),0) > -1 then
    videoDefaultFlags:=videoDefaultFlags and not WFLG_SMART_REFRESH;

  if FPC_VIDEO_FULLSCREEN then
  begin
    OS_Screen := GetScreen;
    If OS_Screen = nil then
      Exit;

    {$ifdef VIDEODEBUG}
    WriteLn('DEBUG: Opened customscreen succesfully');
    {$endif}
    GetWindow:=_OpenWindowTags(nil, [
      WA_CustomScreen, PtrUint(OS_Screen),
      WA_Left       , 0,
      WA_Top        , 0,
      WA_InnerWidth , (OS_Screen^.Width div 8) * 8,
      WA_InnerHeight, (OS_Screen^.Height div 16) * 16,
      WA_AutoAdjust , 1,
      WA_Activate   , 1,
      WA_Borderless , 1,
      WA_BackDrop   , 1,
      WA_FLAGS      , VIDEO_WFLG_DEFAULTS,
      WA_IDCMP      , VIDEO_IDCMP_DEFAULTS,
      TAG_END, TAG_END
    ]);
  end else
  begin      // Windowed Mode
    GetWindow:=_OpenWindowTags(nil, [
      WA_Left       , LastL,
      WA_Top        , LastT,
      WA_MinWidth   , 70*8,
      WA_MinHeight  , 16*VideoFontHeight-10,
      WA_InnerWidth , LastW*8,
      WA_InnerHeight, LastH*VideoFontHeight,
      WA_MaxWidth   , 32768,
      WA_MaxHeight  , 32768,
      WA_Title      , PtrUInt(PChar('FPC Video Window Output')),
      WA_Activate   , 1,
      WA_FLAGS      , (VIDEO_WFLG_DEFAULTS or
                       WFLG_DRAGBAR       or WFLG_DEPTHGADGET   or WFLG_SIZEGADGET or
                       WFLG_SIZEBBOTTOM   or WFLG_CLOSEGADGET),
      WA_IDCMP      , VIDEO_IDCMP_DEFAULTS,
      TAG_END, TAG_END
    ]);
  end;

  Process := PProcess(FindTask(nil));
  WindowForReqSave := Process^.pr_WindowPtr;
  Process^.pr_WindowPtr := GetWindow;

  {$ifdef VIDEODEBUG}
  If GetWindow <> nil then
    WriteLn('DEBUG: Sucessfully opened videounit Window')
  else
    WriteLn('ERROR: Failed to open videounit Window');
  {$endif}
end;


// ==========================================================================
// ==
// ==  Original source code continues, with minor adjustments
// ==
// ==========================================================================


procedure SysInitVideo;
var
  Counter,
  Counter2: LongInt;
  P: PWord;
  flags: DWord;
  i: LongInt;
  envBuf: array[0..15] of char;
begin
{$IFDEF MORPHOS}
  InitGraphicsLibrary;
  InitIntuitionLibrary;
{$ENDIF}

  {$ifdef VIDEODEBUG}
  WriteLn('FULLSCREEN VIDEO UNIT MODIFICATION v2');
  if FPC_VIDEO_FULLSCREEN then
    WriteLn('DEBUG: Recognized fullscreen mode')
  else
    WriteLn('DEBUG: Recognized windowed mode');
  {$endif}

  { FIXME/TODO: next to the hardwired selection, there could be some heuristics,
    which sets the font size correctly on screens according to the aspect
    ratio. (KB) }
  VideoFont:=@vgafont;
  VideoFontHeight:=16;
  if GetVar('FPC_VIDEO_BUILTINFONT',@envBuf,sizeof(envBuf),0) > -1 then
    begin
      case lowerCase(envBuf) of
        'vga8':
          begin
            VideoFont:=@vgafont8;
            VideoFontHeight:=8;
          end;
        'vga14':
          begin
            VideoFont:=@vgafont14;
            VideoFontHeight:=14;
          end;
      end;
    end;

  // fill videobuf and oldvideobuf with different bytes, to allow proper first draw
  FillDword(VideoBuf^, VideoBufSize div 4, $1234D3AD);
  FillDword(OldVideoBuf^, VideoBufSize div 4, $4321BEEF);

  VideoWindow := GetWindow;

  // nice hardcode values are probably going to mess things up
  // so we need a way to determine how many characters would fit
  // the screen in both directions. Try to be as accurate as possible.
  if FPC_VIDEO_FULLSCREEN then
  begin
    // just to make sure that we are going to use the window width
    // and height instead of the screen dimensions.
    // This is to circumvent that the window (or virtual window from
    // vision based on characters pixels * characters in both
    // dimensions) is actually smaller then the window it resides on.
    //
    // Can happen for instance when the window does not hide its
    // borders or titlebar as intended.
    ScreenWidth := VideoWindow^.GZZWidth div 8;
    ScreenHeight := VideoWindow^.GZZHeight div VideoFontHeight;
    ScreenColor := False;

    {$ifdef VIDEODEBUG}
    Writeln('DEBUG: Fullscreen - windowed - Width * Heigth = ',ScreenWidth,' * ',ScreenHeight);
    {$endif}
  end else
  begin
    ScreenWidth := LastW;
    ScreenHeight := LastH;
    ScreenColor := True;
  end;
  {$ifdef WITHBUFFERING}
  BufRp^.Bitmap := AllocBitmap(VideoWindow^.Width, VideoWindow^.Height, VideoWindow^.RPort^.Bitmap^.Depth, BMF_CLEAR, VideoWindow^.RPort^.Bitmap);
  BitmapWidth := VideoWindow^.Width;
  BitmapHeight := VideoWindow^.Height;
  {$endif}
  { viewpostcolormap info }
  videoColorMap := pScreen(videoWindow^.WScreen)^.ViewPort.ColorMap;

  for Counter := 0 to 15 do
  begin
    VideoPens[Counter] := ObtainBestPenA(VideoColorMap,
        vgacolors[counter, 0] shl 24, vgacolors[counter, 1] shl 24, vgacolors[counter, 2] shl 24, nil);
    {$ifdef VIDEODEBUG}
    If VideoPens[Counter] = -1 then
      WriteLn('errr color[',Counter,'] = ', VideoPens[Counter])
    else
      WriteLn('good color[',Counter,'] = ', VideoPens[Counter]);
    {$endif}
  end;

  { Obtain Friend bitmap for font blitting }
  FontBitmap:=AllocBitMap(16,VideoFontHeight*256,1,0, VideoWindow^.RPort^.Bitmap);

  if (FontBitmap <> nil) then
  begin
    flags:=GetBitmapAttr(FontBitmap,BMA_FLAGS);
    if (Flags and BMF_STANDARD) > 0 then
    begin
      {$ifdef VIDEODEBUG}
      writeln('Using fontbitmap mode.');
      {$endif}
      { Locking the bitmap would be better, but that requires CGFX/P96/etc specific calls }
      Forbid();
      { We need to make the data word wide, otherwise the blit will fail
        miserably on classics (tested on 3.1 + AGA) }
      p:=PWord(FontBitmap^.Planes[0]);
      for counter:=0 to 255 do
        for counter2:=0 to VideoFontHeight-1 do
        begin
          p^:=VideoFont[counter * VideoFontHeight + counter2] shl 8;
          inc(p);
        end;
      Permit();
    end
    else
    begin
      {$ifdef VIDEODEBUG}
      writeln('Using direct-from-fontdata mode.');
      {$endif}
      FreeBitmap(FontBitmap);
      FontBitmap:=nil;
    end;
  end;

  if FontBitmap <> nil then
  begin
    SrcMod := 2;
    for i := 0 to 255 do
    begin
      CharPointers[i] := @(PWord(FontBitmap^.Planes[0])[i * VideoFontHeight]);
    end;
  end
  else
  begin
    SrcMod := 1;
    for i := 0 to 255 do
    begin
      CharPointers[i] := @VideoFont[i * VideoFontHeight];
    end;
  end;

  CursorX := 0;
  CursorY := 0;
  OldCursorX := 0;
  OldCursorY := 0;
  CursorType := crHidden;
  OldCursorType := crHidden;
  CursorState := true;
  ForceCursorUpdate:=false;
  CursorUpdateSpeed:=2; // this could come from an env-var or something
  CursorUpdateCnt:=0;

  GotCloseWindowMsg := false;
  GotResizeWindowMsg := false;
  GotActiveWindowMsg := false;
  GotInactiveWindowMsg := false;

  //
end;

procedure SysDoneVideo;
var
  Counter: LongInt;
  msg: PMessage;
begin
  if VideoWindow <> nil then
  begin
    Process^.pr_WindowPtr := WindowForReqSave;
    if not FPC_VIDEO_FULLSCREEN then
    begin
      LastL := VideoWindow^.LeftEdge;
      LastT := VideoWindow^.TopEdge;
    end;
    // clean up the messages from our window before closing
    Forbid();
    repeat
      msg:=GetMsg(videoWindow^.UserPort);
      if (msg <> nil) then ReplyMsg(msg);
    until msg = nil;
    ModifyIDCMP(videoWindow,0);
    Permit();
    CloseWindow(videoWindow);
    VideoWindow := nil;
  end;

  FreeBitMap(FontBitmap);

  {$ifdef WITHBUFFERING}
  FreeBitmap(BufRp^.Bitmap);
  BufRp^.Bitmap := nil;
  {$endif}
  for Counter := 0 to 15 do
    ReleasePen(VideoColorMap, VideoPens[Counter]);
  if ((FPC_VIDEO_FULLSCREEN) and (OS_Screen <> nil)) then
  begin
    CloseScreen(OS_Screen);
  end;
end;

function SysSetVideoMode(const Mode: TVideoMode): Boolean;
var
  dx: integer;
  dy: integer;
begin
  if ScreenColor <> Mode.Color then
  begin
    SysDoneVideo;
    FPC_VIDEO_FULLSCREEN := not Mode.color;
    if not FPC_VIDEO_FULLSCREEN then
    begin
      LastT := 50;
      LastL := 50;

      LastW := 80;
      LastH := 25;
    end;
    SysInitVideo;
  end else
    if not FPC_VIDEO_FULLSCREEN then
    begin
      dx := (Mode.col * 8) - VideoWindow^.GZZWidth;
      dy := (Mode.row * VideoFontHeight) - VideoWindow^.GZZHeight;
      SizeWindow(videoWindow, dx, dy);
    end;
  ScreenWidth := Mode.col;
  ScreenHeight := Mode.row;
  LastW := Mode.Col;
  LastH := Mode.Row;
  ScreenColor := Mode.color;
  SysSetVideoMode := True;
end;


procedure SysClearScreen;
var
  Value: Word;
begin
  //oldSH := -1;
  //oldSW := -1;
  //UpdateScreen(True);
  OldSH := ScreenHeight;
  OldSW := ScreenWidth;
  Value := (LightGray shl 8) or Ord(' '); // fill with light gray space
  FillWord(VideoBuf^, ScreenWidth * ScreenHeight, Value);
  FillWord(OldVideoBuf^, ScreenWidth * ScreenHeight, Value);
  SetAPen(VideoWindow^.RPort, VideoPens[Black]);
  RectFill(VideoWindow^.RPort, videoWindow^.borderLeft, videoWindow^.borderTop, videoWindow^.width - videoWindow^.borderRight - 1, videoWindow^.Height - videoWindow^.borderBottom - 1);
  ForceCursorUpdate := True;
  SysUpdateScreen(False);
  ForceCursorUpdate := False;
end;

procedure DrawChar(rp: PRastPort; x, y: LongInt; crType: Word);
var
  TmpCharData: Word;
  TmpChar: Byte;
  TmpFGColor: Byte;
  TmpBGColor: Byte;
  sX, sY: LongInt;
begin
  TmpCharData := VideoBuf^[y * ScreenWidth + x];
  TmpChar    := byte(TmpCharData);
  TmpFGColor := (TmpCharData shr 8) and %00001111;
  TmpBGColor := (TmpCharData shr 12) and %00000111;

  sX := x * 8 + videoWindow^.borderLeft;
  sY := y * VideoFontHeight + videoWindow^.borderTop;

  if crType <> crBlock then
  begin
    SetABPenDrMd(rp, VideoPens[TmpFGColor], VideoPens[tmpBGColor], JAM2);
  end else
  begin
    { in case of block cursor, swap fg/bg colors
      and BltTemplate() below will take care of everything }
    SetABPenDrMd(rp, VideoPens[tmpBGColor], VideoPens[tmpFGColor], JAM2);
  end;

  BltTemplate(CharPointers[tmpChar], 0, SrcMod, rp, sX, sY, 8, VideoFontHeight);

  if crType = crUnderLine then
  begin
    { draw two lines at the bottom of the char, in case of underline cursor }
    if videoFontHeight = 8 then
      begin
        GfxMove(rp, sX, sY + 7); Draw(rp, sX + 7, sY + 7);
      end
    else
      begin
        GfxMove(rp, sX, sY + videoFontHeight - 2); Draw(rp, sX + 7, sY + videoFontHeight - 2);
        GfxMove(rp, sX, sY + videoFontHeight - 1); Draw(rp, sX + 7, sY + videoFontHeight - 1);
      end;
  end;
end;

procedure SysUpdateScreen(Force: Boolean);
var
  BufCounter: Longint;
  SmallForce: Boolean;
  Counter, CounterX, CounterY: LongInt;
  NumChanged: Integer;
  LocalRP: PRastPort;
  sY, sX: LongInt;
  TmpCharData: Word;
  {$ifdef VideoSpeedTest}
  t,ta: Double;
  {$endif}
begin
  {$ifdef VideoSpeedTest}
  ta := now();
  {$endif}
  SmallForce := False;

  // override forced update when screen dimensions haven't changed
  if Force then
  begin
    if (OldSH = ScreenHeight) and (OldSW = ScreenWidth) then
      Force := false
    else
    begin
      OldSH := ScreenHeight;
      OldSW := ScreenWidth;
    end;
  end;

  if Force then
  begin
    SmallForce:=true;
  end else
  begin
    Counter:=0;
    if not ForceCursorUpdate then
      while not smallforce and (Counter < (VideoBufSize div 4) - 1) do
      begin
        SmallForce := (PDWord(VideoBuf)[Counter] <> PDWord(OldVideoBuf)[Counter]);
        inc(Counter);
      end;
  end;

  LocalRP := VideoWindow^.RPort;

  {$ifdef WITHBUFFERING}
  if (VideoWindow^.Width > BitmapWidth) or (VideoWindow^.Height > BitmapHeight) then
  begin
    FreeBitmap(BufRp^.Bitmap);
    BufRp^.Bitmap := AllocBitmap(VideoWindow^.Width, VideoWindow^.Height, VideoWindow^.RPort^.Bitmap^.Depth, BMF_CLEAR, VideoWindow^.RPort^.Bitmap);
    BitmapWidth := VideoWindow^.Width;
    BitmapHeight := VideoWindow^.Height;
    Force := True;
    Smallforce := True;
  end;
  LocalRP := BufRp;
  {$endif}

  BufCounter:=0;
  NumChanged:=0;


  if Smallforce then
  begin
    {$ifdef VideoSpeedTest}
    t := now();
    {$endif}
    sY := videoWindow^.borderTop;
    for CounterY := 0 to ScreenHeight - 1 do
    begin
      sX := videoWindow^.borderLeft;
      for CounterX := 0 to ScreenWidth - 1 do
      begin
        if (VideoBuf^[BufCounter] <> OldVideoBuf^[BufCounter]) or Force then
        begin
          TmpCharData := VideoBuf^[BufCounter];
          SetABPenDrMd(LocalRP, VideoPens[(TmpCharData shr 8) and %00001111], VideoPens[(TmpCharData shr 12) and %00000111], JAM2);
          BltTemplate(CharPointers[TmpCharData and $FF], 0, SrcMod, LocalRP, sX, sY, 8, VideoFontHeight);
          OldVideoBuf^[BufCounter] := VideoBuf^[BufCounter];
          Inc(NumChanged);
        end;
        Inc(BufCounter);
        sX := sX + 8;
      end;
      sY := sY + VideoFontHeight;
    end;
    {$ifdef VideoSpeedTest}
    if NumChanged > 100 then
      writeln('redraw time: ', floattoStrF((Now-t)* 24 * 60 * 60 * 1000000 / NumChanged, fffixed, 8,3), ' us/char' ); // ms
    {$endif}
  end;

  if (CursorType <> OldCursorType) or
     (CursorX <> OldCursorX) or (CursorY <> OldCursorY) or
     SmallForce or ForceCursorUpdate then
  begin
    if (OldCursorX >= 0) and (OldCursorX < ScreenWidth) and (OldCursorY >= 0) and (OldCursorY < ScreenHeight) then DrawChar(LocalRP, OldCursorX, OldCursorY, crHidden);
    if CursorState and (CursorX >= 0) and (CursorX < ScreenWidth) and (CursorY >= 0) and (CursorY < ScreenHeight) then DrawChar(LocalRP, CursorX, CursorY, CursorType);
    OldCursorX := CursorX;
    OldCursorY := CursorY;
    OldcursorType := CursorType;
  end;
  {$ifdef WITHBUFFERING}
  BltBitMapRastPort(BufRp^.Bitmap, VideoWindow^.borderLeft, VideoWindow^.borderTop, VideoWindow^.RPort, VideoWindow^.borderLeft, VideoWindow^.borderTop, ScreenWidth * 8, ScreenHeight * 16, $00C0);
  {$endif}
  {$ifdef VideoSpeedTest}
  if NumChanged > 100 then
    writeln('overall redraw time: ', floattoStrF((Now-ta)* 24 * 60 * 60 * 1000, fffixed, 8,3), ' ms' ); // ms
  {$endif}
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
begin
  CursorX := NewCursorX;
  CursorY := NewCursorY;
  SysUpdateScreen(False);
end;

function SysGetCapabilities: Word;
begin
  SysGetCapabilities := cpColor or cpChangeCursor;
end;

function SysGetCursorType: Word;
begin
  SysGetCursorType := cursorType;
end;


procedure SysSetCursorType(NewType: Word);
begin
  cursorType := newType;
  { FIXME: halfBlock cursors are not supported for now
           by the rendering code }
  if CursorType = crHalfBlock then
    cursorType := crBlock;

  SysUpdateScreen(False);
end;


// Amiga specific calls
procedure GotCloseWindow;
begin
  GotCloseWindowMsg := True;
end;

function HasCloseWindow: Boolean;
begin
  HasCloseWindow := GotCloseWindowMsg;
  GotCloseWindowMsg := False;
end;

procedure GotResizeWindow;
begin
  GotResizeWindowMsg := True;
end;

function HasResizeWindow(var WinW: LongInt; var WinH: LongInt): Boolean;
begin
  WinW := 0;
  WinH := 0;
  HasResizeWindow := GotResizeWindowMsg;
  if GotResizeWindowMsg then
  begin
    //writeln('Has resize ', GotResizeWindowMsg);
    if Assigned(VideoWindow) then
    begin
      WinW := VideoWindow^.GZZWidth div 8;
      WinH := VideoWindow^.GZZHeight div VideoFontHeight;
//      writeln('resize', winw, ' ',winh);
      LastW := WinW;
      LastH := WinH;
    end
  end
  else
  begin
    WinW := LastW;
    WinH := LastH;
  end;
  GotResizeWindowMsg := False;
end;

procedure GotRefreshWindow;
begin
  if assigned(VideoWindow) then
  begin
    oldSH := -1;
    oldSW := -1;
    BeginRefresh(VideoWindow);
    SysUpdateScreen(true);
    EndRefresh(VideoWindow, true);
  end;
end;

procedure ToggleCursor(forceOff: boolean);
begin
  if CursorType = crHidden then exit;

  if forceOff then
  begin
    CursorState:=false;
    // to immediately turn on cursor on the next toggle
    CursorUpdateCnt:=CursorUpdateSpeed;
  end
  else
  begin
    Inc(CursorUpdateCnt);
    if CursorUpdateCnt >= CursorUpdateSpeed then
    begin
      CursorState:=not CursorState;
      CursorUpdateCnt:=0;
    end
    else
      exit;
  end;
  ForceCursorUpdate:=true;
  SysUpdateScreen(False);
  ForceCursorUpdate:=false;
end;

procedure GotActiveWindow;
begin
  GotActiveWindowMsg:=true;
end;

function HasActiveWindow: boolean;
begin
  HasActiveWindow:=GotActiveWindowMsg;
  GotActiveWindowMsg:=false;
end;

procedure GotInactiveWindow;
begin
  GotInactiveWindowMsg:=true;
end;

function HasInactiveWindow: boolean;
begin
  HasInactiveWindow:=GotInactiveWindowMsg;
  GotInactiveWindowMsg:=false;
end;

{ SetWindowTitles seems not to copy the buffer, at least on AROS.
  So we better keep a reference of the strings to ourselves... }
var
  globWinT: AnsiString;
  globScreenT: AnsiString;

procedure SetWindowTitle(const winTitle: AnsiString; const screenTitle: AnsiString);
var
  winT: PChar;
  screenT: PChar;
begin
  globWinT:=winTitle;
  globScreenT:=screenTitle;
  if VideoWindow <> nil then
  begin
    if globWinT = '' then
      winT:=PChar(PtrInt(-1))
    else
      winT:=PChar(globWinT);
    if globScreenT = '' then
      screenT:=PChar(PtrInt(-1))
    else
      screenT:=PChar(globScreenT);
    SetWindowTitles(VideoWindow, winT, screenT);
  end;
end;

procedure TranslateToCharXY(const X,Y: LongInt; var CX,CY: LongInt);
begin
  CX:=X div 8;
  CY:=Y div VideoFontHeight;
end;

function SysGetVideoModeCount: Word;
begin
  SysGetVideoModeCount := 2;
end;

function SysGetVideoModeData(Index: Word; var Mode: TVideoMode): Boolean;
var
   Screen: PScreen;
begin
  case Index of
    0: begin
         Mode.Col := 80;
         Mode.Row := 25;
         Mode.Color := True;
       end;
    1: begin
        Screen := LockPubScreen('Workbench');
        Mode.Col := Screen^.Width div 8;
        Mode.Row := Screen^.Height div VideoFontHeight;
        UnlockPubScreen('Workbench', Screen);
        Mode.Color := False;
      end;
  end;
  SysGetVideoModeData := True;
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
{$ifdef Amiga68k}
function CreateRastport: PRastPort;
begin
  CreateRastport := AllocMem(SizeOf(TRastPort));
  InitRastPort(CreateRastport);
end;

procedure FreeRastPort(RP: PRastPort);
begin
  FreeMem(RP);
end;
{$endif}


initialization
  SetVideoDriver(SysVideoDriver);
  LastT := 50;
  LastL := 50;
  LastW := 80;
  LastH := 25;
  {$ifdef WITHBUFFERING}
  BufRp := CreateRastPort;
  BufRp^.Layer := nil;
  BufRp^.Bitmap := nil;
  {$endif}
finalization
  {$ifdef WITHBUFFERING}
  if Assigned(BufRp^.Bitmap) then
    FreeBitmap(BufRp^.Bitmap);
  FreeRastPort(BufRp);
  {$endif}
end.
