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
  Date: 2013-01-09
  What: Adjusted FPC video unit for AROS (/AmigaOS?)

  goal:
  ---------------------------------------------------------------------------
  Attempt to add user-on-demand support for AROS Fullscreen to the FPC video 
  unit.
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

var
  VideoWindow: PWindow;

implementation

uses
   exec, agraphics;

{$i video.inc}

{$i videodata.inc}

const
  VIDEOSCREENNAME = 'FPC Video Screen Output';

var
  OS_Screen             : PScreen   = nil;    // To hold our screen, when necessary
  FPC_VIDEO_FULLSCREEN  : Boolean   = False;  // Global that defines when we need to attempt opening on own scren

var
  VideoColorMap         : PColorMap;
  VideoPens             : array[0..15] of LongInt;

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

{$IFNDEF AROS}
  FontBitmap: PBitmap;
{$ENDIF}
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
    SA_LikeWorkbench  , 1     // Let OS
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
begin
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
      WA_IDCMP      , VIDEO_IDCMP_DEFAULTS
    ]); 
  end else  
  begin      // Windowed Mode
    GetWindow:=_OpenWindowTags(nil, [
      WA_Left       , LastL,
      WA_Top        , LastT,
      WA_InnerWidth , LastW*8,
      WA_InnerHeight, LastH*16,
      WA_MaxWidth   , 32768,
      WA_MaxHeight  , 32768,
      WA_Title      , PtrUInt(PChar('FPC Video Window Output')),
      WA_Activate   , 1,
      WA_FLAGS      , (VIDEO_WFLG_DEFAULTS or
                       WFLG_DRAGBAR       or WFLG_DEPTHGADGET   or WFLG_SIZEGADGET or
                       WFLG_SIZEBBOTTOM   or WFLG_CLOSEGADGET),
      WA_IDCMP      , VIDEO_IDCMP_DEFAULTS
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
{$IFNDEF AROS}
var
  Counter, Counter2: LongInt;
  P: PWord;
{$ENDIF}
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

  // fill videobuf and oldvideobuf with different bytes, to allow proper first draw
  FillDword(VideoBuf^, VideoBufSize div 4, $1234D3AD);
  FillDword(OldVideoBuf^, VideoBufSize div 4, $4321BEEF);

  VideoWindow := GetWindow;

  // nice hardcode values are probably going to screw up things
  // so wee neeed a way to detrmined how many chars could be on
  // the screen in both directions. And a bit accurate.
  if FPC_VIDEO_FULLSCREEN then
  begin
    // just to make sure that we are going to use the window width 
    // and height instead of the one from the screen. 
    // This is to circumvent that the window (or virtual window from
    // vision based on characters pixels * characters in both 
    // dimensions) is actually smaller then the window it resides on.
    //
    // Can happen for instance when the window does not hide it's 
    // borders or title as intended.
    ScreenWidth := VideoWindow^.GZZWidth div 8;
    ScreenHeight := VideoWindow^.GZZHeight div 16;
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
   BufRp^.Bitmap := AllocBitmap(VideoWindow^.InnerWidth, VideoWindow^.InnerHeight, VideoWindow^.RPort^.Bitmap^.Depth, BMF_CLEAR, VideoWindow^.RPort^.Bitmap);
   BitmapWidth := VideoWindow^.InnerWidth;
   BitmapHeight := VideoWindow^.InnerHeight;
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

{$IFNDEF AROS}
   { Obtain Friend bitmap for font blitting }
   FontBitmap:=AllocBitMap(16,16*256,1,0,VideoWindow^.RPort^.Bitmap);

   { We need to make the data word wide, otherwise the blit will fail
     miserably on classics (tested on 3.1 + AGA) }
   if FontBitmap <> nil then
   begin
     { Locking the bitmap would be better, but that requires CGFX/P96/etc specific calls }
     Forbid();
     p:=PWord(FontBitmap^.Planes[0]);
     for counter:=0 to 255 do
       for counter2:=0 to 15 do
         begin
           p^:=vgafont[counter,counter2] shl 8;
           inc(p);
         end;
     Permit();
   end;
{$ENDIF}

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

{$IFNDEF AROS}
  FreeBitMap(FontBitmap);
{$ENDIF}

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
      dy := (Mode.row * 16) - VideoWindow^.GZZHeight;
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
begin
  oldSH := -1;
  oldSW := -1;
  UpdateScreen(True);
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
  sY := y * 16 + videoWindow^.borderTop;

  if crType <> crBlock then
  begin
    SetABPenDrMd(rp, VideoPens[TmpFGColor], VideoPens[tmpBGColor], JAM2);
  end else
  begin
    { in case of block cursor, swap fg/bg colors
      and BltTemplate() below will take care of everything }
    SetABPenDrMd(rp, VideoPens[tmpBGColor], VideoPens[tmpFGColor], JAM2);
  end;

{$IFNDEF AROS}
  BltTemplate(@(PWord(FontBitmap^.Planes[0])[tmpChar * 16]), 0, 2, rp, sX, sY, 8, 16);
{$ELSE}
  BltTemplate(@Vgafont[tmpChar, 0], 0, 1, rp, sX, sY, 8, 16);
{$ENDIF}

  if crType = crUnderLine then
  begin
    { draw two lines at the bottom of the char, in case of underline cursor }
    GfxMove(rp, sX, sY + 14); Draw(rp, sX + 7, sY + 14);
    GfxMove(rp, sX, sY + 15); Draw(rp, sX + 7, sY + 15);
  end;
end;

procedure SysUpdateScreen(Force: Boolean);
var
  BufCounter: Longint;
  SmallForce: Boolean;
  Counter, CounterX, CounterY: LongInt;
  //BufRp: PRastPort;
  t: Double;
  NumChanged: Integer;
begin
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

  {$ifdef WITHBUFFERING}
  if (VideoWindow^.InnerWidth > BitmapWidth) or (VideoWindow^.InnerHeight > BitmapHeight) then
  begin
    FreeBitmap(BufRp^.Bitmap);
    BufRp^.Bitmap := AllocBitmap(VideoWindow^.InnerWidth, VideoWindow^.InnerHeight, VideoWindow^.RPort^.Bitmap^.Depth, BMF_CLEAR, VideoWindow^.RPort^.Bitmap);
    BitmapWidth := VideoWindow^.InnerWidth;
    BitmapHeight := VideoWindow^.InnerHeight;
    Force := True;
    Smallforce := True;
  end;
  {$endif}

  BufCounter:=0;
  NumChanged:=0;
  if Smallforce then
  begin
    //t := now();
    for CounterY := 0 to ScreenHeight - 1 do
    begin
      for CounterX := 0 to ScreenWidth - 1 do
      begin
        if (VideoBuf^[BufCounter] <> OldVideoBuf^[BufCounter]) or Force then
        begin
          {$ifdef WITHBUFFERING}
          DrawChar(BufRp, CounterX, CounterY, crHidden);
          {$else}
          DrawChar(VideoWindow^.RPort, CounterX, CounterY, crHidden);
          {$endif}
          OldVideoBuf^[BufCounter] := VideoBuf^[BufCounter];
          Inc(NumChanged);
        end;
        Inc(BufCounter);
      end;
    end;
    //if NumChanged > 100 then
    //  writeln('redraw time: ', floattoStrF((Now-t)* 24 * 60 * 60 * 1000000 / NumChanged, fffixed, 8,3), ' us/char' ); // ms
  end;

  if (CursorType <> OldCursorType) or
     (CursorX <> OldCursorX) or (CursorY <> OldCursorY) or
     SmallForce or ForceCursorUpdate then
  begin
    {$ifdef WITHBUFFERING}
    DrawChar(BufRp, OldCursorY, OldCursorX, crHidden);
    if CursorState then DrawChar(BufRp, CursorY, CursorX, CursorType);
    {$else}
    DrawChar(VideoWindow^.RPort, OldCursorY, OldCursorX, crHidden);
    if CursorState then DrawChar(VideoWindow^.RPort, CursorY, CursorX, CursorType);
    {$endif}
    OldCursorX := CursorX;
    OldCursorY := CursorY;
    OldcursorType := CursorType;
  end;
  {$ifdef WITHBUFFERING}
  BltBitMapRastPort(BufRp^.Bitmap, 0, 0, VideoWindow^.RPort, 0, 0, ScreenWidth * 8, ScreenHeight * 16, $00C0);
  {$endif}
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
begin
  CursorX := NewCursorY;
  CursorY := NewCursorX;
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
      WinH := VideoWindow^.GZZHeight div 16;
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
        Mode.Row := Screen^.Height div 16;
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
