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

unit FPUsrScr;

interface

{$ifdef TP}
  {$define DOS}
{$else}
  {$ifdef GO32V2}
    {$define DOS}
  {$endif}
{$endif}

uses Objects;

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
      VBufferSize  : word;
      VBuffer      : PByteArray;
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
begin
  Capture;
  SetVideoMode(TM);
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
  MI.Rows:=MI.ScreenSize div (MI.Cols*2);
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
{$ifdef TP}
    P: pointer;
    Sel: longint;
{$I realintr.inc}
{$endif}
begin
  r.ah:=$0f;
  intr($10,r);
  if r.al<>MI.Mode then
   begin
     r.ah:=$00; r.al:=MI.Mode; intr($10,r);
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
    UserScreen:=New(PScreen, Init);
  {$endif}
{$endif}
end;


procedure DoneUserScreen;
begin
  if UserScreen<>nil then
   begin
     UserScreen^.SwitchTo;
     Dispose(UserScreen, Done);
   end;
end;

end.
{
  $Log$
  Revision 1.4  1999-06-28 19:32:25  peter
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
