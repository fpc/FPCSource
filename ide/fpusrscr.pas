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
{$ifdef TEST_GRAPH_SWITCH}
      GraphImageSize : longint;
      GraphBuffer : pointer;
      ConsoleGraphDriver, ConsoleGraphMode : word;
{$endif TEST_GRAPH_SWITCH}
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
      IsXterm : boolean;
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
      IdeScreenMode : TVideoMode;
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
    ,Drivers,App
  {$ifdef TEST_GRAPH_SWITCH}
    ,Graph,VESA
  {$else not TEST_GRAPH_SWITCH}
  {$ifdef VESA}
    ,VESA
  {$endif VESA}
  {$endif not TEST_GRAPH_SWITCH}
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
{$ifdef TEST_GRAPH_SWITCH}
  saved : boolean;
  GraphDriver,GraphMode : integer;
{$endif TEST_GRAPH_SWITCH}
begin
  GetVideoMode(ConsoleVideoInfo);
{$ifdef TEST_GRAPH_SWITCH}
  saved:=false;
  if assigned(GraphBuffer) then
    begin
      FreeMem(GraphBuffer,GraphImageSize);
      GraphBuffer:=nil;
      GraphImageSize:=0;
    end;
  if (ConsoleVideoInfo.Mode>= $100) or
     (ConsoleVideoInfo.Mode=$13) or
     (ConsoleVideoInfo.Mode=$12) or
     (ConsoleVideoInfo.Mode=$10) or
     (ConsoleVideoInfo.Mode=$E) then
    begin
      if VesaSetMode(ConsoleVideoInfo.Mode or $8000) then
        begin
          Graph.DontClearGraphMemory:=true;
          if ConsoleVideoInfo.Mode>=$100 then
            begin
              GraphDriver:=Graph.Vesa;
              GraphMode:=ConsoleVideoInfo.Mode and $fff;
            end
          else
            begin
              GraphDriver:=Graph.VGA;
              case ConsoleVideoInfo.Mode of
               $E : GraphMode:=VGALo;
               $10 : GraphMode:=VGAMed;
               $12 : GraphMode:=VGAHi;
               $13 : begin
                       GraphDriver:=Graph.LowRes;
                       GraphMode:=0;
                     end;
              end;
            end;
          Graph.InitGraph(GraphDriver,GraphMode,'');
          if graphresult=grOk then
            begin
              ConsoleGraphDriver:=GraphDriver;
              ConsoleGraphMode:=GraphMode;
              Graph.DontClearGraphMemory:=false;
              GraphImageSize:=ImageSize(0,0,Graph.GetmaxX,Graph.GetMaxY);
              GetMem(GraphBuffer,GraphImageSize);
              FillChar(GraphBuffer^,GraphImageSize,#0);
              GetImage(0,0,Graph.GetmaxX,Graph.GetMaxY,GraphBuffer^);
              saved:=true;
            end
{$ifdef DEBUG}
          else
            Writeln(stderr,'Error in InitGraph ',Graphdriver, ' ',Graphmode)
{$endif DEBUG}
            ;
        end;
    end;
  { mode < $100 so use standard Save code }
  if not saved then
{$endif TEST_GRAPH_SWITCH}
  begin
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
end;

procedure TDOSScreen.SwitchToConsoleScreen;
var
  VSeg,SOfs: word;
{$ifdef TEST_GRAPH_SWITCH}
  restored : boolean;
  GraphDriver,GraphMode : integer;
{$endif TEST_GRAPH_SWITCH}
begin
  SetVideoMode(ConsoleVideoInfo);
{$ifdef TEST_GRAPH_SWITCH}
  restored:=false;
  if assigned(GraphBuffer) then
    begin
      if VesaSetMode(ConsoleVideoInfo.Mode) then
        begin
          if ConsoleVideoInfo.Mode>=$100 then
            begin
              GraphDriver:=Graph.Vesa;
              GraphMode:=ConsoleVideoInfo.Mode and $fff;
            end
          else
            begin
              GraphDriver:=Graph.VGA;
              case ConsoleVideoInfo.Mode of
               $E : GraphMode:=VGALo;
               $10 : GraphMode:=VGAMed;
               $12 : GraphMode:=VGAHi;
               $13 : begin
                       GraphDriver:=Graph.LowRes;
                       GraphMode:=0;
                     end;
              end;
            end;
          if (ConsoleGraphDriver<>GraphDriver) or
             (ConsoleGraphMode<>GraphMode) then
            Graph.InitGraph(GraphDriver,GraphMode,'');
          if graphresult=grOk then
            begin
              PutImage(0,0,GraphBuffer^,CopyPut);
              FreeMem(GraphBuffer,GraphImageSize);
              GraphBuffer:=nil;
              GraphImageSize:=0;
              restored:=true;
            end;
        end;
    end;
  { mode < $100 so use standard Save code }
  if not restored then
{$endif TEST_GRAPH_SWITCH}
    begin
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
  MI.Mode:=MI.Mode and $fff;
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
  IsXterm:=getenv('TERM')='xterm';
  Capture;
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
  SaveConsoleScreen;
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
  if IsXTerm then
    write(#27'7'#27'[?47h');
end;


procedure TLinuxScreen.SwitchToConsoleScreen;
begin
  if IsXterm then
    begin
      write(#27'[0m');
      write(#27'[?47l'#27'8'#27'[m');
    end;
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
  GetConsoleScreenBufferInfo(StartScreenBufferHandle,
    @ConsoleScreenBufferInfo);
  { make sure that the IDE Screen Handle has the maximum display size
    this removes the scroll bars if it is maximized }
  res:=SetConsoleScreenBufferSize(NewScreenBufferHandle,
         ConsoleScreenBufferInfo.dwMaximumWindowSize);
  if not res then
    error:=GetLastError;
  IDEScreenBufferHandle:=NewScreenBufferHandle;
  DosScreenBufferHandle:=StartScreenBufferHandle;
  Capture;
{$ifdef fvision}
  if TextModeGFV then
{$endif fvision}
  IdeScreenMode.row:=0;
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
      IdeScreenMode:=ScreenMode;
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
{$ifdef fvision}
      { Needed to force InitSystemMsg to use the right console handle }
      DoneEvents;
      InitEvents;
{$endif fvision}
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
{$ifdef DEBUG}
      IdeScreenMode.row:=WindowPos.bottom+1;
      IdeScreenMode.col:=WindowPos.right+1;
{$endif DEBUG}
      { needed to force the correct size for videobuf }
      if Assigned(Application) and (IdeScreenMode.row<>0)then
        Application^.SetScreenVideoMode(IdeScreenMode);
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
  Revision 1.17  2002-09-07 15:40:46  peter
    * old logs removed and tabs fixed

  Revision 1.16  2002/09/04 08:35:31  pierre
   * remember IDE screen mode for win32
     to avoid videobuf writes after allocated size.

  Revision 1.15  2002/09/03 05:45:39  pierre
   * fix compilation without DEBUG conditional

  Revision 1.14  2002/09/02 09:29:55  pierre
   + new test code for go32v2 graphic screen saves (only with -dDEBUG)

  Revision 1.13  2002/06/13 11:18:32  pierre
   + xterm window switching support

  Revision 1.12  2002/06/07 14:10:24  pierre
   * try to get resizing to work

  Revision 1.11  2002/06/06 14:10:34  pierre
   * allow window input for fvsion system messages

  Revision 1.10  2002/06/06 06:46:28  pierre
   * No videobuffer switch necessary for fvision win32 graphic version

  Revision 1.9  2002/04/25 13:34:17  pierre
   * fix the disappearing desktop for win32

  Revision 1.8  2002/01/22 16:29:52  pierre
    * try to fix win32 problem with Dos program ouptut in command shell
      Warning, to debug under win32 with GDB you must use "set new-console on"

}
