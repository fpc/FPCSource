{
    $Id$
    This file is part of the Free Pascal Integrated Development Environment
    Copyright (c) 1998 by Berczi Gabor

    User screen support routines

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$i globdir.inc}
unit FPUsrScr;

interface

uses
{$ifdef win32}
  windows,
{$endif win32}
  video,Objects;

type

    PScreen = ^TScreen;
    TScreen = object(TObject)
      function    GetWidth: integer; virtual;
      function    GetHeight: integer; virtual;
      procedure   GetLine(Line: integer; var Text, Attr: string); virtual;
      procedure   GetCursorPos(var P: TPoint); virtual;
      { copy the initial video screen in the ide screen }
      procedure   Capture; virtual;
      { move up or down if supported by OS }
      function    Scroll(i : integer) : integer; virtual;
      { saves the current IDE screen }
      procedure   SaveIDEScreen; virtual;
      { saves the current console screen }
      procedure   SaveConsoleScreen; virtual;
      { restores the saved console screen }
      procedure   SwitchToConsoleScreen; virtual;
      { restores the saved IDE screen }
      procedure   SwitchBackToIDEScreen; virtual;
    end;

{$ifdef DOS}
    TDOSVideoInfo = record
      Mode      : word;
      ScreenSize: word;
      Page      : byte;
      Rows,Cols : integer;
      CurPos    : TPoint;
      CurShapeT : integer;
      CurShapeB : integer;
      StateSize : word;
      StateBuf  : pointer;
    end;

    PDOSScreen = ^TDOSScreen;
    TDOSScreen = object(TScreen)
      constructor Init;
      destructor  Done; virtual;
    public
      function    GetWidth: integer; virtual;
      function    GetHeight: integer; virtual;
      procedure   GetLine(Line: integer; var Text, Attr: string); virtual;
      procedure   GetCursorPos(var P: TPoint); virtual;
      procedure   Capture; virtual;
      procedure   SaveIDEScreen; virtual;
      procedure   SaveConsoleScreen; virtual;
      procedure   SwitchToConsoleScreen; virtual;
      procedure   SwitchBackToIDEScreen; virtual;
    private
      ConsoleVideoInfo : TDOSVideoInfo;
      VBufferSize  : longint;
      VIDEBufferSize : longint;
      VBuffer      : PByteArray;
      VIDEBuffer   : PByteArray;
      IDEVideoInfo : TDOSVideoInfo;
      ctrl_c_state : boolean;
      function    GetLineStartOfs(Line: integer): word;
      procedure   GetBuffer(Size: word);
      procedure   FreeBuffer;
      procedure   GetVideoMode(var MI: TDOSVideoInfo);
      procedure   SetVideoMode(MI: TDOSVideoInfo);
    end;
{$endif}

{$ifdef Unix}
    PLinuxScreen = ^TLinuxScreen;
    TLinuxScreen = object(TScreen)
      constructor Init;
      destructor  Done; virtual;
    public
      function    GetWidth: integer; virtual;
      function    GetHeight: integer; virtual;
      procedure   GetLine(Line: integer; var Text, Attr: string); virtual;
      procedure   GetCursorPos(var P: TPoint); virtual;
      procedure   Capture; virtual;
      procedure   SaveIDEScreen; virtual;
      procedure   SaveConsoleScreen; virtual;
      procedure   SwitchToConsoleScreen; virtual;
      procedure   SwitchBackToIDEScreen; virtual;
    private
      IDE_screen: pvideobuf;
      IDE_size : longint;
    end;
{$endif}

{$ifdef win32}
    PWin32Screen = ^TWin32Screen;
    TWin32Screen = object(TScreen)
      constructor Init;
      destructor  Done; virtual;
    public
      function    GetWidth: integer; virtual;
      function    GetHeight: integer; virtual;
      procedure   GetLine(Line: integer; var Text, Attr: string); virtual;
      procedure   GetCursorPos(var P: TPoint); virtual;
      function    Scroll(i : integer) : integer; virtual;
      procedure   Capture; virtual;
      procedure   SaveIDEScreen; virtual;
      procedure   SaveConsoleScreen; virtual;
      procedure   SwitchToConsoleScreen; virtual;
      procedure   SwitchBackToIDEScreen; virtual;
    private
      DosScreenBufferHandle,
      IDEScreenBufferHandle,
      StartScreenBufferHandle,
      DummyScreenBufferHandle,
      NewScreenBufferHandle : THandle;
      IDEActive : boolean;
      ConsoleMode,IdeMode      : Dword;
      procedure BufferCopy(src,dest : THandle);
    end;
{$endif}

procedure InitUserScreen;
procedure DoneUserScreen;

const UserScreen : PScreen = nil;

implementation

uses
  Dos
(*  {$ifdef TP}
    {$ifdef DPMI}
    ,WinAPI
    {$endif}
  {$endif}*)
  {$ifdef FPC}
    {$ifdef GO32V2}
    ,Dpmiexcp, Go32
    {$endif}
  {$endif}
  {$ifdef fvision}
    ,Drivers
  {$endif}
  {$ifdef VESA}
    ,VESA
  {$endif}
  ;

function TScreen.GetWidth: integer;
begin
  Getwidth:=0;
  Abstract;
end;

function TScreen.GetHeight: integer;
begin
  Getheight:=0;
  Abstract;
end;

procedure TScreen.GetLine(Line: integer; var Text, Attr: string);
begin
  Abstract;
end;

procedure TScreen.GetCursorPos(var P: TPoint);
begin
  Abstract;
end;

procedure TScreen.Capture;
begin
  Abstract;
end;

procedure TScreen.SwitchToConsoleScreen;
begin
  Abstract;
end;

procedure TScreen.SwitchBackToIDEScreen;
begin
  Abstract;
end;

procedure TScreen.SaveIDEScreen;
begin
  Abstract;
end;

function TScreen.Scroll(i : integer) : integer;
begin
  Scroll:=0;
end;

procedure TScreen.SaveConsoleScreen;
begin
  Abstract;
end;


{****************************************************************************
                                 TDOSScreen
****************************************************************************}

{$ifdef DOS}

constructor TDOSScreen.Init;
begin
  inherited Init;
  Capture;
  { get the current ctrl-C state }
  Ctrl_c_state:=djgpp_set_ctrl_c(false);
  djgpp_set_ctrl_c(Ctrl_c_state);
end;


destructor TDOSScreen.Done;
begin
  FreeBuffer;
  if assigned(VIDEBuffer) then
    FreeMem(VIDEBuffer,VIDEBufferSize);
  inherited Done;
end;


function TDOSScreen.GetWidth: integer;
begin
  GetWidth:=ConsoleVideoInfo.Cols;
end;


function TDOSScreen.GetHeight: integer;
begin
  GetHeight:=ConsoleVideoInfo.Rows;
end;


procedure TDOSScreen.GetLine(Line: integer; var Text, Attr: string);
var X: integer;
    W: word;
begin
  Text:=''; Attr:='';
  if Line<GetHeight then
  begin
    W:=GetLineStartOfs(Line);
    for X:=0 to GetWidth-1 do
     begin
       {Text:=Text+chr(VBuffer^[W+X*2]);
       Attr:=Attr+chr(VBuffer^[W+X*2+1]);}
       System.Insert(chr(VBuffer^[W+X*2]),Text,Length(Text)+1);
       System.Insert(chr(VBuffer^[W+X*2+1]),Attr,Length(Attr)+1);
     end;
  end;
end;


procedure TDOSScreen.GetCursorPos(var P: TPoint);
begin
  P:=ConsoleVideoInfo.CurPos;
end;


procedure TDOSScreen.Capture;
begin
  SaveConsoleScreen;
end;

procedure TDosScreen.SaveIDEScreen;
var
  VSeg,SOfs: word;
begin
  GetVideoMode(IDEVideoInfo);
  { First keep a copy of IDE screen }
  if ConsoleVideoInfo.Mode=7 then
   VSeg:=SegB000
  else
   VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
  if not assigned(VIDEBuffer) or (VIDEBufferSize<>IDEVideoInfo.ScreenSize) then
    begin
      if assigned(VIDEBuffer) then
        FreeMem(VIDEBuffer,VIDEBufferSize);
      GetMem(VIDEBuffer,IDEVideoInfo.ScreenSize);
      VIDEBufferSize:=IDEVideoInfo.ScreenSize;
    end;
{$ifdef FPC}
  DosmemGet(VSeg,SOfs,VIDEBuffer^,IDEVideoInfo.ScreenSize);
{$else}
  Move(ptr(VSeg,SOfs)^,VIDEBuffer^,IDEVideoInfo.ScreenSize);
{$endif}
end;

procedure TDosScreen.SaveConsoleScreen;
var
  VSeg,SOfs: word;
begin
  GetVideoMode(ConsoleVideoInfo);
  GetBuffer(ConsoleVideoInfo.ScreenSize);
  if ConsoleVideoInfo.Mode=7 then
   VSeg:=SegB000
  else
   VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
{$ifdef FPC}
  DosmemGet(VSeg,SOfs,VBuffer^,ConsoleVideoInfo.ScreenSize);
{$else}
  Move(ptr(VSeg,SOfs)^,VBuffer^,ConsoleVideoInfo.ScreenSize);
{$endif}
end;

procedure TDOSScreen.SwitchToConsoleScreen;
var
  VSeg,SOfs: word;
begin
  SetVideoMode(ConsoleVideoInfo);

  if ConsoleVideoInfo.Mode=7 then
    VSeg:=SegB000
  else
    VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
{$ifdef FPC}
  DosmemPut(VSeg,SOfs,VBuffer^,ConsoleVideoInfo.ScreenSize);
  djgpp_set_ctrl_c(Ctrl_c_state);
{$else}
  Move(VBuffer^,ptr(VSeg,SOfs)^,ConsoleVideoInfo.ScreenSize);
{$endif}
end;


procedure TDOSScreen.SwitchBackToIDEScreen;
var
  VSeg,SOfs: word;
begin
  SetVideoMode(IDEVideoInfo);
  if ConsoleVideoInfo.Mode=7 then
   VSeg:=SegB000
  else
   VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
  if assigned(VIDEBuffer) then
{$ifdef FPC}
    DosmemPut(VSeg,SOfs,VIDEBuffer^,IDEVideoInfo.ScreenSize);
    Ctrl_c_state := djgpp_set_ctrl_c(false);
{$else}
    Move(VIDEBuffer^,ptr(VSeg,SOfs)^,IDEVideoInfo.ScreenSize);
{$endif}
end;


function TDOSScreen.GetLineStartOfs(Line: integer): word;
begin
  GetLineStartOfs:=(ConsoleVideoInfo.Cols*Line)*2;
end;


procedure TDOSScreen.GetBuffer(Size: word);
begin
  if (VBuffer<>nil) and (VBufferSize=Size) then Exit;
  if VBuffer<>nil then FreeBuffer;
  VBufferSize:=Size;
  GetMem(VBuffer,VBufferSize);
end;


procedure TDOSScreen.FreeBuffer;
begin
  if (VBuffer<>nil) and (VBufferSize>0) then FreeMem(VBuffer,VBufferSize);
  VBuffer:=nil;
end;


procedure TDOSScreen.GetVideoMode(var MI: TDOSVideoInfo);
var
  r: registers;
{$ifdef TP}
  P: pointer;
  Sel: longint;
(*  {$I realintr.inc} *)
{$endif}
begin
  if (MI.StateSize>0) and (MI.StateBuf<>nil) then
     begin FreeMem(MI.StateBuf,MI.StateSize); MI.StateBuf:=nil; end;

  MI.ScreenSize:=MemW[Seg0040:$4c];
  r.ah:=$0f;
  intr($10,r);
  MI.Mode:=r.al;
  MI.Page:=r.bh;
  MI.Cols:=r.ah;
{$ifdef VESA}
  VESAGetMode(MI.Mode);
{$endif}
  MI.Rows:=MI.ScreenSize div (MI.Cols*2);
  if MI.Rows=51 then MI.Rows:=50;
  r.ah:=$03;
  r.bh:=MI.Page;
  intr($10,r);
  with MI do
  begin
    CurPos.X:=r.dl; CurPos.Y:=r.dh;
    CurShapeT:=r.ch; CurShapeB:=r.cl;
  end;

(*
{$ifdef TP}
  { check VGA functions }
  MI.StateSize:=0;
  r.ah:=$1c; r.al:=0; r.cx:=7; intr($10,r);
  if (r.al=$1c) and ((r.flags and fCarry)=0) and (r.bx>0) then
  begin
    MI.StateSize:=r.bx;
    GetMem(MI.StateBuf,MI.StateSize); FillChar(MI.StateBuf^,MI.StateSize,0);
    P:=MI.StateBuf;
{$ifdef DPMI}
    Sel:=GlobalDosAlloc(MI.StateSize);
    P:=Ptr(Sel shr 16,0);
{$endif}
    r.ah:=$1c; r.al:=1; r.cx:=7;
    r.es:=PtrRec(P).Seg; r.bx:=PtrRec(P).Ofs;
    {$ifdef DPMI}realintr($10,r);{$else}intr($10,r);{$endif}
{$ifdef DPMI}
    Move(Ptr(Sel and $ffff,0)^,MI.StateBuf^,MI.StateSize);
    GlobalDosFree(Sel and $ffff);
{$endif}
  end;
{$endif}
*)
end;


procedure TDOSScreen.SetVideoMode(MI: TDOSVideoInfo);
var r: registers;
    CM: TDOSVideoInfo;
{$ifdef TP}
    P: pointer;
    Sel: longint;
{$I realintr.inc}
{$endif}
begin
  FillChar(CM,sizeof(CM),0);
  GetVideoMode(CM);

  if (CM.Mode<>MI.Mode) or (CM.Cols<>MI.Cols) or (CM.Rows<>MI.Rows) then
   begin
     {$ifdef VESA}
     if MI.Mode>=$100 then
       VESASetMode(MI.Mode)
     else
     {$endif}
       begin
         r.ah:=$00; r.al:=MI.Mode; intr($10,r);
       end;
     if (MI.Mode=3) and (MI.Cols=80) and (MI.Rows=50) then
     begin
       r.ax:=$1112; r.bx:=$0;
       intr($10,r);
     end;
   end;
  r.ah:=$05; r.al:=MI.Page; intr($10,r);
  r.ah:=$02; r.bh:=MI.Page; r.dl:=MI.CurPos.X; r.dh:=MI.CurPos.Y; intr($10,r);
  r.ah:=$01; r.ch:=MI.CurShapeT; r.cl:=MI.CurShapeB; intr($10,r);

  (*
{$ifdef TP}
  if (MI.StateSize>0) and (MI.StateBuf<>nil) then
  begin
    P:=MI.StateBuf;
{$ifdef DPMI}
    Sel:=GlobalDosAlloc(MI.StateSize);
    Move(MI.StateBuf^,ptr(Sel and $ffff,0)^,MI.StateSize);
    P:=Ptr(Sel shr 16,0);
{$endif}
    r.ah:=$1c; r.al:=2; r.cx:=7;
    r.es:=PtrRec(P).Seg; r.bx:=PtrRec(P).Ofs;
    {$ifdef DPMI}realintr($10,r);{$else}intr($10,r);{$endif}
{$ifdef DPMI}
    GlobalDosFree(Sel and $ffff);
{$endif}
  end;
{$endif}
*)
end;

{$endif}


{****************************************************************************
                                 TLinuxScreen
****************************************************************************}

{$ifdef Unix}

constructor TLinuxScreen.Init;
begin
  inherited Init;
  IDE_screen := nil;
end;


destructor TLinuxScreen.Done;
begin
  inherited Done;
end;


function TLinuxScreen.GetWidth: integer;
begin
  GetWidth:=ScreenWidth;
end;


function TLinuxScreen.GetHeight: integer;
begin
  GetHeight:=ScreenHeight;
end;


procedure TLinuxScreen.GetLine(Line: integer; var Text, Attr: string);
begin
  Text:='';
  Attr:='';
end;


procedure TLinuxScreen.GetCursorPos(var P: TPoint);
begin
  P.X:=0;
  P.Y:=0;
end;


procedure TLinuxScreen.Capture;
begin
end;

procedure TLinuxScreen.SaveIDEScreen;
begin
  if assigned(IDE_screen) then
    dispose(IDE_screen);
  getmem(IDE_screen,videobufsize);
  Ide_size:=videobufsize;
  move(videobuf^,IDE_screen^,videobufsize);
end;

procedure TLinuxScreen.SaveConsoleScreen;
begin
end;


procedure TLinuxScreen.SwitchToConsoleScreen;
begin
end;

procedure TLinuxScreen.SwitchBackToIDEScreen;
begin
  if IDE_screen = nil then
    exit;
  move(IDE_screen^,videobuf^,videobufsize);
  freemem(IDE_screen,Ide_size);
  IDE_screen := nil;
end;

{$endif}

{****************************************************************************
                                 TWin32Screen
****************************************************************************}

{$ifdef win32}

procedure UpdateFileHandles;
begin
  {StdInputHandle:=longint(GetStdHandle(STD_INPUT_HANDLE));}
  StdOutputHandle:=longint(GetStdHandle(STD_OUTPUT_HANDLE));
  {StdErrorHandle:=longint(GetStdHandle(STD_ERROR_HANDLE));}
  TextRec(Output).Handle:=StdOutputHandle;
  TextRec(StdOut).Handle:=StdOutputHandle;
  {TextRec(StdErr).Handle:=StdErrorHandle;}
end;

constructor TWin32Screen.Init;
var
  SecurityAttr : Security_attributes;
  BigWin : Coord;
  res : longbool;
  Error : dword;
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  inherited Init;
  {if GetConsoleOutputCP<>437 then
    res:=SetConsoleOutputCP(437);}
  SecurityAttr.nLength:=SizeOf(Security_attributes);
  SecurityAttr.lpSecurityDescriptor:=nil;
  SecurityAttr.bInheritHandle:=true;
  NewScreenBufferHandle:=CreateConsoleScreenBuffer(
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE,SecurityAttr,
    CONSOLE_TEXTMODE_BUFFER,nil);
  DummyScreenBufferHandle:=CreateConsoleScreenBuffer(
    GENERIC_READ or GENERIC_WRITE,
    FILE_SHARE_READ or FILE_SHARE_WRITE,SecurityAttr,
    CONSOLE_TEXTMODE_BUFFER,nil);
  StartScreenBufferHandle:=GetStdHandle(STD_OUTPUT_HANDLE);
  GetConsoleMode(GetStdHandle(Std_Input_Handle), @ConsoleMode);
  IdeMode:=ConsoleMode;
{$ifdef debug}
{define win32bigwin}
{$endif debug}
{$ifdef win32bigwin}
  GetConsoleScreenBufferInfo(StartScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  BigWin.X:=ConsoleScreenBufferInfo.dwSize.X;
  BigWin.Y:=200;
  { Try to allow to store more info }
  res:=SetConsoleScreenBufferSize(NewScreenBufferHandle,BigWin);
  if not res then
    error:=GetLastError;
  res:=SetConsoleScreenBufferSize(StartScreenBufferHandle,BigWin);
  if not res then
    error:=GetLastError;
{$endif win32bigwin}
  { make sure that both Screen Handle have the sme buffer }
  GetConsoleScreenBufferInfo(StartScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  res:=SetConsoleScreenBufferSize(NewScreenBufferHandle,
         ConsoleScreenBufferInfo.dwSize);
  if not res then
    error:=GetLastError;
  IDEScreenBufferHandle:=NewScreenBufferHandle;
  DosScreenBufferHandle:=StartScreenBufferHandle;
  Capture;
{$ifdef fvision}
  if TextModeGFV then
{$endif fvision}
  SwitchBackToIDEScreen;
end;

destructor TWin32Screen.Done;
begin
  { copy the Dos buffer content into the original ScreenBuffer
    which remains the startup std_output_handle PM }
  {if StartScreenBufferHandle=IDEScreenBufferHandle then}
    BufferCopy(DosScreenBufferHandle,IDEScreenBufferHandle);
  SetConsoleActiveScreenBuffer(StartScreenBufferHandle);
  SetStdHandle(Std_Output_Handle,StartScreenBufferHandle);
  UpdateFileHandles;
  CloseHandle(NewScreenBufferHandle);
  CloseHandle(DummyScreenBufferHandle);
  inherited Done;
end;

function TWin32Screen.GetWidth: integer;
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetConsoleScreenBufferInfo(DosScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  GetWidth:=ConsoleScreenBufferInfo.dwSize.X;
end;

function TWin32Screen.GetHeight: integer;
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetConsoleScreenBufferInfo(DosScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  GetHeight:=ConsoleScreenBufferInfo.dwSize.Y;
end;

function TWin32Screen.Scroll(i : integer) : integer;
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
  ConsoleWindow : Small_rect;
begin
  GetConsoleScreenBufferInfo(DosScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  if (ConsoleScreenBufferInfo.srWindow.Top + i < 0) then
    i:= -ConsoleScreenBufferInfo.srWindow.Top;
  if (ConsoleScreenBufferInfo.srWindow.Bottom + i > ConsoleScreenBufferInfo.dwSize.Y) then
    i:= ConsoleScreenBufferInfo.dwSize.Y - ConsoleScreenBufferInfo.srWindow.Bottom;
  if i<>0 then
    begin
      ConsoleWindow.Left:=ConsoleScreenBufferInfo.srWindow.Left;
      ConsoleWindow.Right:=ConsoleScreenBufferInfo.srWindow.Right;
      ConsoleWindow.Top:=ConsoleScreenBufferInfo.srWindow.Top+i;
      ConsoleWindow.Bottom:=ConsoleScreenBufferInfo.srWindow.Bottom+i;
      SetConsoleWindowInfo(DosScreenBufferHandle,true,ConsoleWindow);
      Scroll:=i;
    end
  else
    Scroll:=0;
end;

procedure TWin32Screen.GetLine(Line: integer; var Text, Attr: string);
type
  CharInfoArray = Array [0..255] of Char_Info;
var
  LineBuf : ^CharInfoArray;
  BufSize,BufCoord : Coord;
  i,LineSize : longint;
  WriteRegion : SMALL_RECT;
begin
  GetMem(LineBuf,SizeOf(CharInfoArray));
  LineSize:=ScreenWidth;
  If LineSize>256 then
    LineSize:=256;
  BufSize.X:=LineSize;
  BufSize.Y:=1;
  BufCoord.X:=0;
  BufCoord.Y:=0;
  with WriteRegion do
    begin
      Top :=Line;
      Left :=0;
      Bottom := Line+1;
      Right := LineSize-1;
    end;
  ReadConsoleOutput(DosScreenBufferHandle, PChar_info(LineBuf),
    BufSize, BufCoord, @WriteRegion);
  for i:=1 to LineSize do
    begin
      Text[i]:=LineBuf^[i-1].AsciiChar;
      Attr[i]:=char(byte(LineBuf^[i-1].Attributes));
    end;
  FreeMem(LineBuf,SizeOf(CharInfoArray));
  Text[0]:=char(byte(LineSize));
  Attr[0]:=char(byte(LineSize));
end;


procedure TWin32Screen.GetCursorPos(var P: TPoint);
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetConsoleScreenBufferInfo(DosScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  P.X:=ConsoleScreenBufferInfo.dwCursorPosition.X;
  P.Y:=ConsoleScreenBufferInfo.dwCursorPosition.Y;
end;

procedure TWin32Screen.BufferCopy(Src, Dest : THandle);
type
  CharInfoArray = Array [0..256*255-1] of Char_Info;
var
  LineBuf : ^CharInfoArray;
  BufSize,BufCoord : Coord;
  Error, LineSize,
  Part, OnePartY: longint;
  res : boolean;
  WriteRegion : SMALL_RECT;
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
  DestConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetConsoleScreenBufferInfo(Src,
    @ConsoleScreenBufferInfo);
  GetConsoleScreenBufferInfo(Dest,
    @DestConsoleScreenBufferInfo);
  GetMem(LineBuf,SizeOf(CharInfoArray));
  FillChar(LineBuf^,SizeOf(CharInfoArray),#0);

  LineSize:=ConsoleScreenBufferInfo.dwSize.X;
  If LineSize>256 then
    LineSize:=256;
  BufSize.X:=LineSize;
  BufSize.Y:=ConsoleScreenBufferInfo.dwSize.Y;
  BufCoord.X:=0;
  BufCoord.Y:=0;
  with WriteRegion do
    begin
      Top :=0;
      Left :=0;
      Bottom := ConsoleScreenBufferInfo.dwSize.Y-1;
      Right := LineSize-1;
    end;
  if BufSize.X*BufSize.Y*Sizeof(CHAR_INFO) >= $8000 then
    begin
      OnePartY := ($8000  -1) div (BufSize.X * SizeOf(Char_Info) );
      BufSize.Y:=OnePartY;
      Part:=0;
      while ((Part+1)*OnePartY < ConsoleScreenBufferInfo.dwSize.Y) do
        begin
          WriteRegion.Top := Part*OnePartY;
          WriteRegion.Bottom := (Part+1)*OnePartY-1;
          res:=ReadConsoleOutput(Src, PChar_info(LineBuf),
            BufSize, BufCoord, @WriteRegion);
          if not res then
            Error:=GetLastError;
          res:=WriteConsoleOutput(Dest, PChar_info(LineBuf),
            BufSize, BufCoord, @WriteRegion);
          if not res then
            Error:=GetLastError;
          Inc(Part);
        end;
      BufSize.Y:=ConsoleScreenBufferInfo.dwSize.Y - Part*OnePartY;
      WriteRegion.Top := Part*OnePartY;
      WriteRegion.Bottom := ConsoleScreenBufferInfo.dwSize.Y-1;
      res:=ReadConsoleOutput(Src, PChar_info(LineBuf),
        BufSize, BufCoord, @WriteRegion);
      if not res then
        Error:=GetLastError;
      res:=WriteConsoleOutput(Dest, PChar_info(LineBuf),
        BufSize, BufCoord, @WriteRegion);
      if not res then
        Error:=GetLastError;
    end
  else
    begin
      res:=ReadConsoleOutput(Src, PChar_info(LineBuf),
        BufSize, BufCoord, @WriteRegion);
      if not res then
        Error:=GetLastError;
      res:=WriteConsoleOutput(Dest, PChar_info(LineBuf),
        BufSize, BufCoord, @WriteRegion);
      if not res then
        Error:=GetLastError;
    end;
  FreeMem(LineBuf,SizeOf(CharInfoArray));
  SetConsoleCursorPosition(Dest, ConsoleScreenBufferInfo.dwCursorPosition);
end;

procedure TWin32Screen.Capture;
begin
  {if StartScreenBufferHandle=IdeScreenBufferHandle then
    BufferCopy(IDEScreenBufferHandle,DosScreenBufferHandle)
  else
    BufferCopy(DosScreenBufferHandle,IDEScreenBufferHandle);}
  SaveConsoleScreen;
end;

{ dummy for win32 as the Buffer screen
  do hold all the info }
procedure TWin32Screen.SaveIDEScreen;
begin
{$ifdef fvision}
  if TextModeGFV then
{$endif fvision}
    begin
      GetConsoleMode(GetStdHandle(Std_Input_Handle), @IdeMode);
      { set the dummy buffer as active already now PM }
      SetStdHandle(Std_Output_Handle,DummyScreenBufferHandle);
      UpdateFileHandles;
    end;
end;

{ dummy for win32 as the Buffer screen
  do hold all the info }
procedure TWin32Screen.SaveConsoleScreen;
begin
{$ifdef fvision}
  if TextModeGFV then
{$endif fvision}
    begin
      GetConsoleMode(GetStdHandle(Std_Input_Handle), @ConsoleMode);
      { set the dummy buffer as active already now PM }
      SetStdHandle(Std_Output_Handle,DummyScreenBufferHandle);
      UpdateFileHandles;
    end;
end;

procedure TWin32Screen.SwitchToConsoleScreen;
begin
{$ifdef fvision}
  if TextModeGFV then
{$endif fvision}
    begin
      SetConsoleActiveScreenBuffer(DosScreenBufferHandle);
      SetStdHandle(Std_Output_Handle,DosScreenBufferHandle);
      SetConsoleMode(GetStdHandle(Std_Input_Handle), ConsoleMode);
      UpdateFileHandles;
    end;
  IDEActive:=false;
end;

procedure TWin32Screen.SwitchBackToIDEScreen;
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
  WindowPos : Small_rect;
  res : boolean;
  error : longint;
begin
{$ifdef fvision}
  if TextModeGFV then
{$endif fvision}
    begin
      SetStdHandle(Std_Output_Handle,IDEScreenBufferHandle);
      UpdateFileHandles;
      GetConsoleScreenBufferInfo(IDEScreenBufferHandle,
        @ConsoleScreenBufferInfo);
      SetConsoleActiveScreenBuffer(IDEScreenBufferHandle);
      IdeMode:=(IdeMode or ENABLE_MOUSE_INPUT or ENABLE_WINDOW_INPUT) and not ENABLE_PROCESSED_INPUT;
      SetConsoleMode(GetStdHandle(Std_Input_Handle), IdeMode);
      WindowPos.left:=0;
      WindowPos.right:=ConsoleScreenBufferInfo.srWindow.right
                       -ConsoleScreenBufferInfo.srWindow.left;
      WindowPos.top:=0;
      WindowPos.bottom:=ConsoleScreenBufferInfo.srWindow.bottom
                       -ConsoleScreenBufferInfo.srWindow.top;
      with ConsoleScreenBufferInfo.dwMaximumWindowSize do
        begin
        if WindowPos.Right<X-1 then
          WindowPos.right:=X-1;
        if WindowPos.Bottom<Y-1 then
          WindowPos.Bottom:=Y-1;
        end;
      res:=SetConsoleWindowInfo(IDEScreenBufferHandle,true,WindowPos);
      if not res then
        error:=GetLastError;
    end;
  IDEActive:=true;
end;

{$endif}


{****************************************************************************
                                 Initialize
****************************************************************************}

procedure InitUserScreen;
begin
{$ifdef DOS}
  UserScreen:=New(PDOSScreen, Init);
{$else}
  {$ifdef Unix}
    UserScreen:=New(PLinuxScreen, Init);
  {$else}

    {$ifdef Win32}
      UserScreen:=New(PWin32Screen, Init);
    {$else}
      UserScreen:=New(PScreen, Init);
    {$endif Win32}
  {$endif Unix}
{$endif Dos}
end;


procedure DoneUserScreen;
begin
  if UserScreen<>nil then
   begin
     UserScreen^.SwitchToConsoleScreen;
     Dispose(UserScreen, Done);
     UserScreen:=nil;
   end;
end;

end.
{
  $Log$
  Revision 1.11  2002-06-06 14:10:34  pierre
   * allow window input for fvsion system messages

  Revision 1.10  2002/06/06 06:46:28  pierre
   * No videobuffer switch necessary for fvision win32 graphic version

  Revision 1.9  2002/04/25 13:34:17  pierre
   * fix the disappearing desktop for win32

  Revision 1.8  2002/01/22 16:29:52  pierre
    * try to fix win32 problem with Dos program ouptut in command shell
      Warning, to debug under win32 with GDB you must use "set new-console on"

  Revision 1.7  2001/11/08 17:06:22  pierre
   * impose the correct size for win32 console window

  Revision 1.6  2001/11/08 16:38:25  pierre
    * fix win32 scrolling
    + always go back to 0,0 position in IDE mode

  Revision 1.5  2001/11/08 16:07:41  pierre
   * overcome buffer win32 problem due to a bug in ReadConsoleOutput

  Revision 1.4  2001/10/24 14:17:27  pierre
   * try to fix the Win2000 mouse problem

  Revision 1.3  2001/09/09 20:44:53  carl
  * bugfix of console sharing mode (on NT this would bug all
  std_input access).

  Revision 1.2  2001/08/12 00:04:50  pierre
   * some speed improvements for string operations

  Revision 1.1  2001/08/04 11:30:24  peter
    * ide works now with both compiler versions

  Revision 1.1.2.10  2001/06/14 09:15:16  pierre
      TScreen methods reorganized:
      SwitchTo method renamed SwitchToConsoleScreen
      SwitchBack method renamed SwitchBackToIDEScreen
    + method Scroll added
    + SaveIDEScreen and SaveConsoleScreen methods added

  Revision 1.1.2.9  2001/04/04 08:52:01  pierre
   * allow inheritance for win32 DosScreenBufferHandle

  Revision 1.1.2.8  2001/03/16 17:45:54  pierre
   * free VIDEBuffer of TDosScreen

  Revision 1.1.2.7  2000/11/30 13:04:01  pierre
   * fix for bug 1205

  Revision 1.1.2.6  2000/11/29 00:54:45  pierre
   + preserve window number and save special windows

  Revision 1.1.2.5  2000/11/22 12:47:21  pierre
   * fix the screen saving at start for win32

  Revision 1.1.2.4  2000/11/14 09:23:56  marco
   * Second batch

  Revision 1.1.2.3  2000/10/10 21:24:56  pierre
   * avoid writing past IDE_screen buffer length

  Revision 1.1.2.2  2000/08/21 12:10:19  jonas
    * fixed errors in my previous commit, it now works properly

  Revision 1.1.2.1  2000/08/21 10:51:13  jonas
    * IDE screen saving/restoring implemented for Linux

  Revision 1.1  2000/07/13 09:48:36  michael
  + Initial import

  Revision 1.13  2000/06/16 15:00:20  pierre
   * accord to new WriteConsoleOuput declarations

  Revision 1.12  2000/04/25 08:42:33  pierre
   * New Gabor changes : see fixes.txt

  Revision 1.11  2000/04/18 11:42:37  pierre
   lot of Gabor changes : see fixes.txt

  Revision 1.10  2000/03/13 20:30:37  pierre
   + stores IDE screen before Switching for DOS

  Revision 1.9  2000/02/04 23:17:25  pierre
   * Keep the entry ScreenBuffer at exit

  Revision 1.8  1999/12/01 16:17:18  pierre
   * Restore std_output_handle correctly at exit for GDB

  Revision 1.7  1999/11/10 17:12:00  pierre
   * Win32 screen problems solved

  Revision 1.6  1999/09/22 13:02:00  pierre
   + Twin32Screen added

  Revision 1.5  1999/08/16 18:25:24  peter
    * Adjusting the selection when the editor didn't contain any line.
    * Reserved word recognition redesigned, but this didn't affect the overall
      syntax highlight speed remarkably (at least not on my Amd-K6/350).
      The syntax scanner loop is a bit slow but the main problem is the
      recognition of special symbols. Switching off symbol processing boosts
      the performance up to ca. 200%...
    * The editor didn't allow copying (for ex to clipboard) of a single character
    * 'File|Save as' caused permanently run-time error 3. Not any more now...
    * Compiler Messages window (actually the whole desktop) did not act on any
      keypress when compilation failed and thus the window remained visible
    + Message windows are now closed upon pressing Esc
    + At 'Run' the IDE checks whether any sources are modified, and recompiles
      only when neccessary
    + BlockRead and BlockWrite (Ctrl+K+R/W) implemented in TCodeEditor
    + LineSelect (Ctrl+K+L) implemented
    * The IDE had problems closing help windows before saving the desktop

  Revision 1.4  1999/06/28 19:32:25  peter
    * fixes from gabor

  Revision 1.3  1999/02/02 16:41:42  peter
    + automatic .pas/.pp adding by opening of file
    * better debuggerscreen changes

  Revision 1.2  1999/01/04 11:49:51  peter
   * 'Use tab characters' now works correctly
   + Syntax highlight now acts on File|Save As...
   + Added a new class to syntax highlight: 'hex numbers'.
   * There was something very wrong with the palette managment. Now fixed.
   + Added output directory (-FE<xxx>) support to 'Directories' dialog...
   * Fixed some possible bugs in Running/Compiling, and the compilation/run
     process revised

  Revision 1.1  1998/12/28 15:47:53  peter
    + Added user screen support, display & window
    + Implemented Editor,Mouse Options dialog
    + Added location of .INI and .CFG file
    + Option (INI) file managment implemented (see bottom of Options Menu)
    + Switches updated
    + Run program

  Revision 1.0  1998/12/24 09:55:49  gabor
    Original implementation

}
