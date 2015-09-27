{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2015 by Nikolay Nikolov
    member of the Free Pascal development team

    Video unit for Win16

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit video;

interface

uses
  WinTypes;

{$I videoh.inc}

var
  KeyEventWndProc: WNDPROC;

implementation

uses
  WinProcs;

{$I video.inc}

const
  ColorRefs: array[0..15] of COLORREF=
    ($000000,$aa0000,$00aa00,$aaaa00,$0000aa,$aa00aa,$0055aa,$aaaaaa,
     $555555,$ff5555,$55ff55,$ffff55,$5555ff,$ff55ff,$55ffff,$ffffff);

var
  VideoWindow: HWND;

procedure WindowPaint(hwnd: HWND);
var
  dc: HDC;
  ps: PAINTSTRUCT;
  oldfont: HFONT;
  oldtextcolor,oldbkcolor: COLORREF;
  Metrics: TEXTMETRIC;
  y,y1,y2,x,x1,x2: SmallInt;
  ch: TVideoCell;
  CharWidth,CharHeight: SmallInt;
begin
  dc:=BeginPaint(hwnd,@ps);
  oldfont:=SelectObject(dc,GetStockObject(SYSTEM_FIXED_FONT));
  GetTextMetrics(dc,@Metrics);
  CharWidth:=Metrics.tmMaxCharWidth;
  CharHeight:=Metrics.tmHeight+Metrics.tmExternalLeading;
  x1:=ps.rcPaint.left div CharWidth;
  x2:=1+ps.rcPaint.right div CharWidth;
  y1:=ps.rcPaint.top div CharHeight;
  y2:=1+ps.rcPaint.bottom div CharHeight;
  if x1<0 then
    x1:=0;
  if y1<0 then
    y1:=0;
  if x2>=ScreenWidth then
    x2:=ScreenWidth-1;
  if y2>=ScreenHeight then
    y2:=ScreenHeight-1;
  oldtextcolor:=GetTextColor(dc);
  oldbkcolor:=GetBkColor(dc);
  for y:=y1 to y2 do
    for x:=x1 to x2 do
    begin
      ch:=videobuf^[y*ScreenWidth+x];
      SetTextColor(dc,ColorRefs[(ch shr 8) and 15]);
      SetBkColor(dc,ColorRefs[(ch shr 12) and 15]);
      TextOut(dc,x*CharWidth,y*CharHeight,@ch,1);
    end;
  SetTextColor(dc,oldtextcolor);
  SetBkColor(dc,oldbkcolor);
  SelectObject(dc,oldfont);
  EndPaint(hwnd,@ps);
end;

function MainWndProc(hwnd: HWND; msg: UINT; wParam: WPARAM; lParam: LPARAM): LRESULT; export;
begin
  case msg of
    WM_KEYDOWN,
    WM_KEYUP,
    WM_SYSKEYDOWN,
    WM_SYSKEYUP:
      MainWndProc:=KeyEventWndProc(hwnd,msg,wParam,lParam);
    WM_PAINT:
      WindowPaint(hwnd);
    WM_DESTROY:
      PostQuitMessage(0);
    else
      MainWndProc:=DefWindowProc(hwnd,msg,wParam,lParam);
  end;
end;

procedure InitWinClass;
var
  wc: WNDCLASS;
begin
  wc.style:=0;
  wc.lpfnWndProc:=@MainWndProc;
  wc.cbClsExtra:=0;
  wc.cbWndExtra:=0;
  wc.hInstance:=HInstance;
  wc.hIcon:=LoadIcon(0,IDI_APPLICATION);
  wc.hCursor:=LoadCursor(0,IDC_ARROW);
  wc.hbrBackground:=GetStockObject(BLACK_BRUSH);
  wc.lpszMenuName:=nil;
  wc.lpszClassName:='FPCConsoleWndClass';
  if not RegisterClass(wc) then
  begin
    MessageBox(0,'Error registering window class',nil,MB_OK or MB_ICONHAND or MB_TASKMODAL);
    Halt(1);
  end;
end;

procedure InitWindow;
begin
  VideoWindow:=CreateWindow(
    'FPCConsoleWndClass',
    'Console',
    WS_OVERLAPPEDWINDOW,
    CW_USEDEFAULT,
    CW_USEDEFAULT,
    CW_USEDEFAULT,
    CW_USEDEFAULT,
    0,
    0,
    HInstance,
    nil);
  if VideoWindow=0 then
  begin
    MessageBox(0,'Error creating window',nil,MB_OK or MB_ICONHAND or MB_TASKMODAL);
    Halt(1);
  end;
  ShowWindow(VideoWindow,CmdShow);
  UpdateWindow(VideoWindow);
end;

procedure ProcessMessages;
var
  m: MSG;
begin
  while PeekMessage(@m,0,0,0,1) do
  begin
    TranslateMessage(@m);
    DispatchMessage(@m);
  end;
end;

procedure SysInitVideo;
begin
  if hPrevInst=0 then
    InitWinClass;
  InitWindow;
  ProcessMessages;
  ScreenWidth:=80;
  ScreenHeight:=25;
end;

procedure SysDoneVideo;
begin
end;

procedure SysUpdateScreen(Force: Boolean);
var
  dc: HDC;
  oldfont: HFONT;
  oldtextcolor,oldbkcolor: COLORREF;
  Metrics: TEXTMETRIC;
  y,x: SmallInt;
  ch: TVideoCell;
  CharWidth,CharHeight: SmallInt;
begin
  dc:=GetDC(VideoWindow);
  oldfont:=SelectObject(dc,GetStockObject(SYSTEM_FIXED_FONT));
  GetTextMetrics(dc,@Metrics);
  CharWidth:=Metrics.tmMaxCharWidth;
  CharHeight:=Metrics.tmHeight+Metrics.tmExternalLeading;
  oldtextcolor:=GetTextColor(dc);
  oldbkcolor:=GetBkColor(dc);
  for y:=0 to ScreenHeight-1 do
    for x:=0 to ScreenWidth-1 do
    begin
      ch:=videobuf^[y*ScreenWidth+x];
      if force or (ch<>oldvideobuf^[y*ScreenWidth+x]) then
      begin
        oldvideobuf^[y*ScreenWidth+x]:=videobuf^[y*ScreenWidth+x];
        SetTextColor(dc,ColorRefs[(ch shr 8) and 15]);
        SetBkColor(dc,ColorRefs[(ch shr 12) and 15]);
        TextOut(dc,x*CharWidth,y*CharHeight,@ch,1);
      end;
    end;
  SetTextColor(dc,oldtextcolor);
  SetBkColor(dc,oldbkcolor);
  SelectObject(dc,oldfont);
  ReleaseDC(VideoWindow,dc);
  ProcessMessages;
end;

function SysGetCapabilities: Word;
begin
  SysGetCapabilities:=cpUnderLine+cpBlink+cpColor;
end;

procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
begin
  CursorX:=NewCursorX;
  CursorY:=NewCursorY;
end;

function SysGetCursorType: Word;
begin
end;

procedure SysSetCursorType(NewType: Word);
begin
end;

function SysSetVideoMode(const mode:Tvideomode):boolean;
begin
end;

const
  SysVideoDriver: TVideoDriver = (
    InitDriver: @SysInitVideo;
    DoneDriver: @SysDoneVideo;
    UpdateScreen: @SysUpdateScreen;
    ClearScreen: nil;
    SetVideoMode: @SysSetVideoMode;
    GetVideoModeCount: nil;
    GetVideoModeData: nil;
    SetCursorPos: @SysSetCursorPos;
    GetCursorType: @SysGetCursorType;
    SetCursorType: @SysSetCursorType;
    GetCapabilities: @SysGetCapabilities;
  );

begin
  KeyEventWndProc:=@DefWindowProc;
  SetVideoDriver(SysVideoDriver);
end.
