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
  Objects;

type

    PScreen = ^TScreen;
    TScreen = object(TObject)
      function    GetWidth: integer; virtual;
      function    GetHeight: integer; virtual;
      procedure   GetLine(Line: integer; var Text, Attr: string); virtual;
      procedure   GetCursorPos(var P: TPoint); virtual;
      procedure   Capture; virtual;
      procedure   SwitchTo; virtual;
      procedure   SwitchBack; virtual;
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
      procedure   SwitchTo; virtual;
      procedure   SwitchBack; virtual;
    private
      VideoInfo    : TDOSVideoInfo;
      VBufferSize  : longint;
      VIDEBufferSize : longint;
      VBuffer      : PByteArray;
      VIDEBuffer   : PByteArray;
      TM           : TDOSVideoInfo;
      function    GetLineStartOfs(Line: integer): word;
      procedure   GetBuffer(Size: word);
      procedure   FreeBuffer;
      procedure   GetVideoMode(var MI: TDOSVideoInfo);
      procedure   SetVideoMode(MI: TDOSVideoInfo);
    end;
{$endif}

{$ifdef Linux}
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
      procedure   SwitchTo; virtual;
      procedure   SwitchBack; virtual;
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
      procedure   Capture; virtual;
      procedure   SwitchTo; virtual;
      procedure   SwitchBack; virtual;
    private
      DosScreenBufferHandle,
      IDEScreenBufferHandle : THandle;
      IDEActive : boolean;
      procedure BufferCopy(src,dest : THandle);
    end;
{$endif}

procedure InitUserScreen;
procedure DoneUserScreen;

const UserScreen : PScreen = nil;

implementation

uses
  Dos,Video
(*  {$ifdef TP}
    {$ifdef DPMI}
    ,WinAPI
    {$endif}
  {$endif}*)
  {$ifdef FPC}
    {$ifdef GO32V2}
    ,Go32
    {$endif}
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

procedure TScreen.SwitchTo;
begin
  Abstract;
end;

procedure TScreen.SwitchBack;
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
end;


destructor TDOSScreen.Done;
begin
  inherited Done;
  FreeBuffer;
end;


function TDOSScreen.GetWidth: integer;
begin
  GetWidth:=VideoInfo.Cols;
end;


function TDOSScreen.GetHeight: integer;
begin
  GetHeight:=VideoInfo.Rows;
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
       Text:=Text+chr(VBuffer^[W+X*2]);
       Attr:=Attr+chr(VBuffer^[W+X*2+1]);
     end;
  end;
end;


procedure TDOSScreen.GetCursorPos(var P: TPoint);
begin
  P:=VideoInfo.CurPos;
end;


procedure TDOSScreen.Capture;
var
  VSeg,SOfs: word;
begin
  GetVideoMode(VideoInfo);
  GetBuffer(VideoInfo.ScreenSize);
  if VideoInfo.Mode=7 then
   VSeg:=SegB000
  else
   VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
{$ifdef FPC}
  DosmemGet(VSeg,SOfs,VBuffer^,VideoInfo.ScreenSize);
{$else}
  Move(ptr(VSeg,SOfs)^,VBuffer^,VideoInfo.ScreenSize);
{$endif}
end;

procedure TDOSScreen.SwitchTo;
var
  VSeg,SOfs: word;
begin
  GetVideoMode(TM);
  { First keep a copy of IDE screen }
  if VideoInfo.Mode=7 then
   VSeg:=SegB000
  else
   VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
  if not assigned(VIDEBuffer) or (VIDEBufferSize<>TM.ScreenSize) then
    begin
      if assigned(VIDEBuffer) then
        FreeMem(VIDEBuffer,VIDEBufferSize);
      GetMem(VIDEBuffer,TM.ScreenSize);
      VIDEBufferSize:=TM.ScreenSize;
    end;
{$ifdef FPC}
  DosmemGet(VSeg,SOfs,VIDEBuffer^,TM.ScreenSize);
{$else}
  Move(ptr(VSeg,SOfs)^,VIDEBuffer^,TM.ScreenSize);
{$endif}

  SetVideoMode(VideoInfo);

  if VideoInfo.Mode=7 then
    VSeg:=SegB000
  else
    VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
{$ifdef FPC}
  DosmemPut(VSeg,SOfs,VBuffer^,VideoInfo.ScreenSize);
{$else}
  Move(VBuffer^,ptr(VSeg,SOfs)^,VideoInfo.ScreenSize);
{$endif}
end;


procedure TDOSScreen.SwitchBack;
var
  VSeg,SOfs: word;
begin
  Capture;
  SetVideoMode(TM);
  if VideoInfo.Mode=7 then
   VSeg:=SegB000
  else
   VSeg:=SegB800;
  SOfs:=MemW[Seg0040:$4e];
  if assigned(VIDEBuffer) then
{$ifdef FPC}
    DosmemPut(VSeg,SOfs,VIDEBuffer^,TM.ScreenSize);
{$else}
    Move(VIDEBuffer^,ptr(VSeg,SOfs)^,TM.ScreenSize);
{$endif}
end;


function TDOSScreen.GetLineStartOfs(Line: integer): word;
begin
  GetLineStartOfs:=(VideoInfo.Cols*Line)*2;
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

{$ifdef Linux}

constructor TLinuxScreen.Init;
begin
  inherited Init;
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


procedure TLinuxScreen.SwitchTo;
begin
end;


procedure TLinuxScreen.SwitchBack;
begin
end;

{$endif}

{****************************************************************************
                                 TWin32Screen
****************************************************************************}

{$ifdef win32}

constructor TWin32Screen.Init;
var
  SecurityAttr : Security_attributes;
  BigWin : Coord;
  res : boolean;
  Error : dword;
begin
  inherited Init;
  SecurityAttr.nLength:=SizeOf(Security_attributes);
  SecurityAttr.lpSecurityDescriptor:=nil;
  SecurityAttr.bInheritHandle:=false;
  DosScreenBufferHandle:=CreateConsoleScreenBuffer(
    GENERIC_READ or GENERIC_WRITE,
    0,SecurityAttr,
    CONSOLE_TEXTMODE_BUFFER,nil);
  IDEScreenBufferHandle:=GetStdHandle(STD_OUTPUT_HANDLE);
{$ifdef win32bigwin}
  BigWin.X:=80;
  BigWin.Y:=50;
  SetConsoleScreenBufferSize(DosScreenBufferHandle,BigWin);
  SetConsoleScreenBufferSize(IDEScreenBufferHandle,BigWin);
  BigWin.X:=80;
  BigWin.Y:=50;
  { Try to allow to store more info }
  res:=SetConsoleScreenBufferSize(DosScreenBufferHandle,BigWin);
  if not res then
    error:=GetLastError;
{$endif win32bigwin}
  Capture;
  SwitchBack;
end;

destructor TWin32Screen.Done;
begin
  { copy the Dos buffer content into the original ScreenBuffer
    which remains the startup std_output_handle PM }
  BufferCopy(DosScreenBufferHandle,IDEScreenBufferHandle);
  SetConsoleActiveScreenBuffer(IDEScreenBufferHandle);
  SetStdHandle(Std_Output_Handle,IDEScreenBufferHandle);
  CloseHandle(DosScreenBufferHandle);
  inherited Done;
end;

function TWin32Screen.GetWidth: integer;
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetConsoleScreenBufferInfo(DosScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  GetWidth:=ConsoleScreenBufferInfo.dwSize.X;
  {GetWidth:=ScreenWidth;}
end;

function TWin32Screen.GetHeight: integer;
var
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetConsoleScreenBufferInfo(DosScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  GetHeight:=ConsoleScreenBufferInfo.dwSize.Y;
  {GetHeight:=ScreenHeight;}
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
  LineSize : longint;
  WriteRegion : SMALL_RECT;
  ConsoleScreenBufferInfo : Console_screen_buffer_info;
begin
  GetMem(LineBuf,SizeOf(CharInfoArray));
  LineSize:=ScreenWidth;
  If LineSize>256 then
    LineSize:=256;
  BufSize.X:=LineSize;
  BufSize.Y:=ScreenHeight;
  BufCoord.X:=0;
  BufCoord.Y:=0;
  with WriteRegion do
    begin
      Top :=0;
      Left :=0;
      Bottom := ScreenHeight-1;
      Right := LineSize-1;
    end;
  ReadConsoleOutput(Src, PChar_info(LineBuf),
    BufSize, BufCoord, @WriteRegion);
  WriteConsoleOutput(Dest, PChar_info(LineBuf)^,
    BufSize, BufCoord, @WriteRegion);
  FreeMem(LineBuf,SizeOf(CharInfoArray));
  GetConsoleScreenBufferInfo(Src,
    @ConsoleScreenBufferInfo);
  SetConsoleCursorPosition(Dest, ConsoleScreenBufferInfo.dwCursorPosition);
end;

procedure TWin32Screen.Capture;
begin
  BufferCopy(IDEScreenBufferHandle,DosScreenBufferHandle);
end;

procedure TWin32Screen.SwitchTo;
begin
  SetConsoleActiveScreenBuffer(DosScreenBufferHandle);
  SetStdHandle(Std_Output_Handle,DosScreenBufferHandle);
  IDEActive:=false;
end;

procedure TWin32Screen.SwitchBack;
begin
  SetConsoleActiveScreenBuffer(IDEScreenBufferHandle);
  SetStdHandle(Std_Output_Handle,IDEScreenBufferHandle);
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
  {$ifdef LINUX}
    UserScreen:=New(PLinuxScreen, Init);
  {$else}

    {$ifdef Win32}
      UserScreen:=New(PWin32Screen, Init);
    {$else}
      UserScreen:=New(PScreen, Init);
    {$endif Win32}
  {$endif Linux}
{$endif Dos}
end;


procedure DoneUserScreen;
begin
  if UserScreen<>nil then
   begin
     UserScreen^.SwitchTo;
     Dispose(UserScreen, Done);
     UserScreen:=nil;
   end;
end;

end.
{
  $Log$
  Revision 1.12  2000-04-25 08:42:33  pierre
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