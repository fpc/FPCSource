{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Florian Klaempfl
    member of the Free Pascal development team

    Mouse unit for linux

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit Mouse;
interface

{$ifdef NOMOUSE}
{$DEFINE NOGPM}
{$ENDIF}

{$i mouseh.inc}

implementation

uses
  Unix,Video
{$ifndef NOGPM}
  ,gpm
{$endif ndef NOGPM}
  ;

{$i mouse.inc}

{$ifndef NOMOUSE}

const
  mousecur    : boolean = false;
  mousecurofs : longint = -1;

var
  mousecurcell : TVideoCell;

const
  gpm_fs : longint = -1;

procedure PlaceMouseCur(ofs:longint);

var
  upd : boolean;

begin
  if VideoBuf=nil then
   exit;
  upd:=false;
  if (MouseCurOfs<>-1) and (VideoBuf^[MouseCurOfs]=MouseCurCell) then
   begin
     VideoBuf^[MouseCurOfs]:=MouseCurCell xor $7f00;
     upd:=true;
   end;
  MouseCurOfs:=ofs;
  if (MouseCurOfs<>-1) then
   begin
     MouseCurCell:=VideoBuf^[MouseCurOfs] xor $7f00;
     VideoBuf^[MouseCurOfs]:=MouseCurCell;
     upd:=true;
   end;
  if upd then
   Updatescreen(false);
end;

procedure SysInitMouse;
{$ifndef NOGPM}
var
  connect : TGPMConnect;
{$endif ndef NOGPM}
begin
{$ifndef NOGPM}
  if gpm_fs=-1 then
    begin
    { open gpm }
      connect.EventMask:=GPM_MOVE or GPM_DRAG or GPM_DOWN or GPM_UP;
      connect.DefaultMask:=0;
      connect.MinMod:=0;
      connect.MaxMod:=0;
      gpm_fs:=Gpm_Open(connect,0);
      if (gpm_fs=-2) and (getenv('TERM')<>'xterm') then
        begin
          gpm_fs:=-1;
          Gpm_Close;
        end;
    end;
  { show mousepointer }
  if gpm_fs<>-1 then
    ShowMouse;
{$else ifdef NOGPM}
      if (getenv('TERM')='xterm') then
        begin
          gpm_fs:=-2;
          Write(#27'[?1001s'); { save old hilit tracking }
          Write(#27'[?1000h'); { enable mouse tracking }
        end;
{$endif NOGPM}
end;


procedure SysDoneMouse;
begin
  If gpm_fs<>-1 then
    begin
      HideMouse;
{$ifndef NOGPM}
      Gpm_Close;
{$else ifdef NOGPM}
      Write(#27'[?1000l'); { disable mouse tracking }
      Write(#27'[?1001r'); { Restore old hilit tracking }
{$endif ifdef NOGPM}
      gpm_fs:=-1;
    end;
end;


function SysDetectMouse:byte;
{$ifndef NOGPM}
var
  x : longint;
  e : TGPMEvent;
  connect : TGPMConnect;
{$endif ndef NOGPM}
begin
{$ifndef NOGPM}
  if gpm_fs=-1 then
    begin
      connect.EventMask:=GPM_MOVE or GPM_DRAG or GPM_DOWN or GPM_UP;
      connect.DefaultMask:=0;
      connect.MinMod:=0;
      connect.MaxMod:=0;
      gpm_fs:=Gpm_Open(connect,0);
      if (gpm_fs=-2) and (getenv('TERM')<>'xterm') then
        begin
          Gpm_Close;
          gpm_fs:=-1;
        end;
    end;
{ always a mouse deamon present }
  if gpm_fs<>-1 then
    begin
      x:=Gpm_GetSnapshot(e);
      if x<>-1 then
        SysDetectMouse:=x
      else
        SysDetectMouse:=2;
    end
  else
    SysDetectMouse:=0;
{$else ifdef NOGPM}
  if (getenv('TERM')='xterm') then
    SysDetectMouse:=2;
{$endif NOGPM}
end;


procedure SysShowMouse;
begin
  PlaceMouseCur(MouseCurOfs);
  mousecur:=true;
end;


procedure SysHideMouse;
begin
  PlaceMouseCur(-1);
  mousecur:=false;
end;


function SysGetMouseX:word;
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  Gpm_GetSnapshot(e);
  SysGetMouseX:=e.x-1;
{$endif ndef NOGPM}
end;


function SysGetMouseY:word;
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  Gpm_GetSnapshot(e);
  if e.y>0 then
   SysGetMouseY:=e.y-1
  else
   SysGetMouseY:=0;
{$endif ndef NOGPM}
end;


function SysGetMouseButtons:word;
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  Gpm_GetSnapshot(e);
  SysGetMouseButtons:=e.buttons;
{$endif ndef NOGPM}
end;


procedure SysGetMouseEvent(var MouseEvent: TMouseEvent);
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  if gpm_fs<0 then
   exit;
{$ifndef NOGPM}
  Gpm_GetEvent(e);
  if e.x>0 then
   MouseEvent.x:=e.x-1
  else
   MouseEvent.x:=0;
  if e.y>0 then
   MouseEvent.y:=e.y-1
  else
   MouseEvent.y:=0;
  MouseEvent.buttons:=0;
  if e.buttons and Gpm_b_left<>0 then
   inc(MouseEvent.buttons,1);
  if e.buttons and Gpm_b_right<>0 then
   inc(MouseEvent.buttons,2);
  if e.buttons and Gpm_b_middle<>0 then
   inc(MouseEvent.buttons,4);
  case (e.EventType and $f) of
    GPM_MOVE,
    GPM_DRAG : MouseEvent.Action:=MouseActionMove;
    GPM_DOWN : MouseEvent.Action:=MouseActionDown;
    GPM_UP   : MouseEvent.Action:=MouseActionUp;
  else
   MouseEvent.Action:=0;
  end;
{ update mouse cursor }
  if mousecur then
   PlaceMouseCur(MouseEvent.y*ScreenWidth+MouseEvent.x);
{$endif ndef NOGPM}
end;



function SysPollMouseEvent(var MouseEvent: TMouseEvent):boolean;

{$ifndef NOGPM}
var
  e : TGPMEvent;
  fds : FDSet;
{$endif ndef NOGPM}
begin
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
{$ifndef NOGPM}
  if gpm_fs<0 then
   exit(false);
  if gpm_fs>0 then
    begin
      FD_Zero(fds);
      FD_Set(gpm_fd,fds);
    end;
  if (gpm_fs=-2) or (Select(gpm_fs+1,@fds,nil,nil,1)>0) then
   begin
     FillChar(e,SizeOf(e),#0);
     Gpm_GetSnapshot(e);
     if e.x>0 then
      MouseEvent.x:=e.x-1
     else
      MouseEvent.x:=0;
     if e.y>0 then
      MouseEvent.y:=e.y-1
     else
      MouseEvent.y:=0;
     MouseEvent.buttons:=0;
     if e.buttons and Gpm_b_left<>0 then
      inc(MouseEvent.buttons,1);
     if e.buttons and Gpm_b_right<>0 then
      inc(MouseEvent.buttons,2);
     if e.buttons and Gpm_b_middle<>0 then
      inc(MouseEvent.buttons,4);
     case (e.EventType and $f) of
      GPM_MOVE,
      GPM_DRAG : MouseEvent.Action:=MouseActionMove;
      GPM_DOWN : MouseEvent.Action:=MouseActionDown;
      GPM_UP   : MouseEvent.Action:=MouseActionUp;
     else
      MouseEvent.Action:=0;
     end;
     if {(gpm_fs<>-2) or} (MouseEvent.Action<>0) then
       SysPollMouseEvent:=true
     else
       SysPollMouseEvent:=false;
   end
  else
   SysPollMouseEvent:=false;
{$endif ndef NOGPM}
end;

Const
  SysMouseDriver : TMouseDriver = (
    UseDefaultQueue : true;
    InitDriver      : @SysInitMouse;
    DoneDriver      : @SysDoneMouse;
    DetectMouse     : @SysDetectMouse;
    ShowMouse       : @SysShowMouse;
    HideMouse       : @SysHideMouse;
    GetMouseX       : @SysGetMouseX;
    GetMouseY       : @SysGetMouseY;
    GetMouseButtons : @SysGetMouseButtons;
    SetMouseXY      : Nil;
    GetMouseEvent   : @SysGetMouseEvent;
    PollMouseEvent  : @SysPollMouseEvent;
    PutMouseEvent   : Nil;
  );

{$else ifndef NOMOUSE}

Const
  SysMouseDriver : TMouseDriver = (
    UseDefaultQueue : true;
    InitDriver      : Nil;
    DoneDriver      : Nil;
    DetectMouse     : Nil;
    ShowMouse       : Nil;
    HideMouse       : Nil;
    GetMouseX       : Nil;
    GetMouseY       : Nil;
    GetMouseButtons : Nil;
    SetMouseXY      : Nil;
    GetMouseEvent   : Nil;
    PollMouseEvent  : Nil;
    PutMouseEvent   : Nil;
  );

{$endif}

Begin
  SetMouseDriver(SysMouseDriver);
end.

{
  $Log$
  Revision 1.6  2001-12-02 17:21:25  peter
    * merged fixes from 1.0

  Revision 1.5  2001/09/22 00:01:43  michael
  + Merged driver support for mouse from fixbranch

  Revision 1.4  2001/09/17 21:36:31  peter
    * merged fixes

  Revision 1.2.2.6  2001/09/21 23:53:48  michael
  + Added mouse driver support.

  Revision 1.2.2.5  2001/09/06 09:05:08  pierre
   * fix NOGPM code

  Revision 1.2.2.4  2001/09/06 08:33:34  pierre
   * fix NOGPM cond to not include gpm unit

  Revision 1.2.2.3  2001/08/05 12:25:55  peter
    * fix possible range check errors

  Revision 1.2.2.2  2001/01/30 22:23:44  peter
    * unix back to linux

  Revision 1.3  2001/08/05 12:24:20  peter
    * m68k merges

  Revision 1.2  2001/01/21 20:21:40  marco
   * Rename fest II. Rtl OK

  Revision 1.1  2001/01/13 11:03:58  peter
    * API 2 RTL commit

}
