{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Video unit for DOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Video;

interface

{$i videoh.inc}

var
  VideoSeg : word;


implementation

uses
  mouse,
  go32;

{$i video.inc}

{$ASMMODE ATT}

var
  OldVideoBuf : PVideoBuf;

  { used to know if LastCursorType is valid }
const
  InitVideoCalled : boolean = false;
  LastCursorType : word = crUnderline;

{ allways set blink state again }

procedure SetHighBitBlink;
var
  regs : trealregs;
begin
  regs.ax:=$1003;
  regs.bx:=$0001;
  realintr($10,regs);
end;

function BIOSGetScreenMode(var Cols,Rows: word; var Color: boolean): boolean;
var r: trealregs;
    L: longint;
    LSel,LSeg: word;
    B: array[0..63] of byte;
type
  TWord = word;
  PWord = ^TWord;
var
  OK: boolean;
begin
  L:=global_dos_alloc(64);
  LSeg:=(L shr 16);
  LSel:=(L and $ffff);

  r.ah:=$1b; r.bx:=0;
  r.es:=LSeg; r.di:=0;
  realintr($10,r);
  OK:=(r.al=$1b);
  if OK then
  begin
    dpmi_dosmemget(LSeg,0,B,64);
    Cols:=PWord(@B[5])^; Rows:=B[$22];
    Color:=PWord(@B[$27])^<>0;
  end;
  global_dos_free(LSel);
  BIOSGetScreenMode:=OK;
end;

procedure SysInitVideo;
var
  regs : trealregs;
begin
  VideoSeg:=$b800;
  if (ScreenWidth=$ffff) or (ScreenHeight=$ffff) or
    (ScreenWidth=0) or (ScreenHeight=0) then
    begin
       ScreenColor:=true;
       regs.ah:=$0f;
       realintr($10,regs);
       if (regs.al and 1)=0 then
         ScreenColor:=false;
       if regs.al=7 then
         begin
            ScreenColor:=false;
            VideoSeg:=$b000;
         end
       else
         VideoSeg:=$b800;
       ScreenWidth:=regs.ah;
       regs.ax:=$1130;
       regs.bx:=0;
       realintr($10,regs);
       ScreenHeight:=regs.dl+1;
       BIOSGetScreenMode(ScreenWidth,ScreenHeight,ScreenColor);
    end;
  regs.ah:=$03;
  regs.bh:=0;
  realintr($10,regs);
  CursorLines:=regs.cl;
  CursorX:=regs.dl;
  CursorY:=regs.dh;
  If InitVideoCalled then
    Begin
      FreeMem(VideoBuf,VideoBufSize);
      FreeMem(OldVideoBuf,VideoBufSize);
    End;
{ allocate pmode memory buffer }
  VideoBufSize:=ScreenWidth*ScreenHeight*2;
  GetMem(VideoBuf,VideoBufSize);
  GetMem(OldVideoBuf,VideoBufSize);
  SetHighBitBlink;
  SetCursorType(LastCursorType);
  { ClearScreen; removed here
    to be able to catch the content of the monitor }
end;


procedure SysDoneVideo;
begin
  LastCursorType:=GetCursorType;
  ClearScreen;
  SetCursorType(crUnderLine);
  SetCursorPos(0,0);
  FreeMem(VideoBuf,VideoBufSize);
  VideoBuf:=nil;
  FreeMem(OldVideoBuf,VideoBufSize);
  OldVideoBuf:=nil;
  VideoBufSize:=0;
end;


function SysGetCapabilities: Word;
begin
  SysGetCapabilities := $3F;
end;


procedure SysSetCursorPos(NewCursorX, NewCursorY: Word);
var
  regs : trealregs;
begin
  regs.ah:=$02;
  regs.bh:=0;
  regs.dh:=NewCursorY;
  regs.dl:=NewCursorX;
  realintr($10,regs);
  CursorY:=regs.dh;
  CursorX:=regs.dl;
end;

{ I don't know the maximum value for the scan line
  probably 7 or 15 depending on resolution !!
  }
function SysGetCursorType: Word;
var
  regs : trealregs;
begin
  regs.ah:=$03;
  regs.bh:=0;
  realintr($10,regs);
  SysGetCursorType:=crHidden;
  if (regs.ch and $60)=0 then
   begin
     SysGetCursorType:=crBlock;
     if (regs.ch and $1f)<>0 then
      begin
        SysGetCursorType:=crHalfBlock;
        if regs.cl+1=(regs.ch and $1F) then
         SysGetCursorType:=crUnderline;
      end;
   end;
end;


procedure SysSetCursorType(NewType: Word);
var
  regs : trealregs;
const
  MaxCursorLines = 7;
begin
  regs.ah:=$01;
  regs.bx:=NewType;
  case NewType of
   crHidden    : regs.cx:=$2000;
   crHalfBlock : begin
                   regs.ch:=MaxCursorLines shr 1;
                   regs.cl:=MaxCursorLines;
                 end;
   crBlock     : begin
                   regs.ch:=0;
                   regs.cl:=MaxCursorLines;
                 end;
   else          begin
                   regs.ch:=MaxCursorLines-1;
                   regs.cl:=MaxCursorLines;
                 end;
  end;
  realintr($10,regs);
end;


function DefaultVideoModeSelector(const VideoMode: TVideoMode; Params: Longint): Boolean;
type
  wordrec=packed record
    lo,hi : word;
  end;
var
  regs : trealregs;
begin
  regs.ax:=wordrec(Params).lo;
  regs.bx:=wordrec(Params).hi;
  realintr($10,regs);
  defaultvideomodeselector:=true;
  DoCustomMouse(false);
end;

function VideoModeSelector8x8(const VideoMode: TVideoMode; Params: Longint): Boolean;
type
  wordrec=packed record
    lo,hi : word;
  end;
var
  regs : trealregs;
begin
  regs.ax:=3;
  regs.bx:=0;
  realintr($10,regs);
  regs.ax:=$1112;
  regs.bx:=$0;
  realintr($10,regs);
  videomodeselector8x8:=true;
  ScreenColor:=true;
  ScreenWidth:=80;
  ScreenHeight:=50;
  DoCustomMouse(false);
end;

procedure SysClearScreen;
begin
  FillWord(VideoBuf^,VideoBufSize shr 1,$0720);
  UpdateScreen(true);
end;


procedure SysUpdateScreen(Force: Boolean);
begin
  if LockUpdateScreen<>0 then
   exit;
  if not force then
   begin
     asm
        movl    VideoBuf,%esi
        movl    OldVideoBuf,%edi
        movl    VideoBufSize,%ecx
        shrl    $2,%ecx
        repe
        cmpsl
        setne   force
     end;
   end;
  if Force then
   begin
     dosmemput(videoseg,0,videobuf^,VideoBufSize);
     move(videobuf^,oldvideobuf^,VideoBufSize);
   end;
end;


procedure RegisterVideoModes;
begin
  RegisterVideoMode(40, 25, False,@DefaultVideoModeSelector, $00000000);
  RegisterVideoMode(40, 25, True, @DefaultVideoModeSelector, $00000001);
  RegisterVideoMode(80, 25, False,@DefaultVideoModeSelector, $00000002);
  RegisterVideoMode(80, 25, True, @DefaultVideoModeSelector, $00000003);
  RegisterVideoMode(80, 50, True, @VideoModeSelector8x8, 0);
end;

Const
  SysVideoDriver : TVideoDriver = (
    InitDriver : @SysInitVideo;
    DoneDriver : @SysDoneVideo;
    UpdateScreen : @SysUpdateScreen;
    ClearScreen : @SysClearScreen;
    SetVideoMode : Nil;
    HasVideoMode : Nil;
    SetCursorPos : @SysSetCursorPos;
    GetCursorType : @SysGetCursorType;
    SetCursorType : @SysSetCursorType;
    GetCapabilities : @SysGetCapabilities
  );

initialization
  SetVideoDriver(SysVideoDriver);
  RegisterVideoModes;

finalization
  UnRegisterVideoModes;
end.
{
  $Log$
  Revision 1.3  2001-09-21 19:50:18  michael
  + Merged driver support from fixbranch


  Revision 1.2  2001/05/09 19:53:28  peter
    * removed asm for copy, use dosmemput (merged)

  Revision 1.1.2.4  2001/09/21 18:42:08  michael
  + Implemented support for custom video drivers.

  Revision 1.1.2.3  2001/05/06 21:54:23  carl
  * bugfix of Windows NT double exception crash

  Revision 1.1.2.2  2001/04/16 10:56:13  peter
    * fixes for stricter compiler

  Revision 1.1.2.1  2001/01/30 21:52:01  peter
    * moved api utils to rtl

  Revision 1.1  2001/01/13 11:03:58  peter
    * API 2 RTL commit

}

