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

const
  MouseEventBufSize = 16;

{$i mouseh.inc}

implementation

uses
  Unix,Video
{$ifndef NOMOUSE}
  ,gpm
{$endif ndef NOMOUSE}
  ;

const
  mousecur    : boolean = false;
  mousecurofs : longint = -1;

var
  mousecurcell : TVideoCell;


const
  gpm_fs : longint = -1;


procedure PlaceMouseCur(ofs:longint);
{$ifndef NOMOUSE}
var
  upd : boolean;
{$endif ndef NOMOUSE}
begin
{$ifndef NOMOUSE}
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
{$endif ndef NOMOUSE}
end;


procedure InitMouse;
{$ifndef NOGPM}
var
  connect : TGPMConnect;
{$endif ndef NOGPM}
begin
{$ifndef NOMOUSE}
{$ifndef NOGPM}
  PendingMouseHead:=@PendingMouseEvent;
  PendingMouseTail:=@PendingMouseEvent;
  PendingMouseEvents:=0;
  FillChar(LastMouseEvent,sizeof(TMouseEvent),0);
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
{$endif ndef NOMOUSE}
end;


procedure DoneMouse;
begin
{$ifndef NOMOUSE}
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
{$endif ndef NOMOUSE}
end;


function DetectMouse:byte;
{$ifndef NOGPM}
var
  x : longint;
  e : TGPMEvent;
  connect : TGPMConnect;
{$endif ndef NOGPM}
begin
{$ifdef NOMOUSE}
  DetectMouse:=0;
{$else ndef NOMOUSE}
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
        DetectMouse:=x
      else
        DetectMouse:=2;
    end
  else
    DetectMouse:=0;
{$else ifdef NOGPM}
  if (getenv('TERM')='xterm') then
    DetectMouse:=2;
{$endif NOGPM}
{$endif ndef NOMOUSE}
end;


procedure ShowMouse;
begin
  PlaceMouseCur(MouseCurOfs);
  mousecur:=true;
end;


procedure HideMouse;
begin
  PlaceMouseCur(-1);
  mousecur:=false;
end;


function GetMouseX:word;
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
{$ifdef NOMOUSE}
  GetMouseX:=0;
{$else ndef NOMOUSE}
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  Gpm_GetSnapshot(e);
  GetMouseX:=e.x-1;
{$endif ndef NOGPM}
{$endif ndef NOMOUSE}
end;


function GetMouseY:word;
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
{$ifdef NOMOUSE}
  GetMouseY:=0;
{$else ndef NOMOUSE}
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  Gpm_GetSnapshot(e);
  if e.y>0 then
   GetMouseY:=e.y-1
  else
   GetMouseY:=0;
{$endif ndef NOGPM}
{$endif ndef NOMOUSE}
end;


function GetMouseButtons:word;
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
{$ifdef NOMOUSE}
  GetMouseButtons:=0;
{$else ndef NOMOUSE}
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  Gpm_GetSnapshot(e);
  GetMouseButtons:=e.buttons;
{$endif ndef NOGPM}
{$endif ndef NOMOUSE}
end;


procedure SetMouseXY(x,y:word);
begin
end;


procedure GetMouseEvent(var MouseEvent: TMouseEvent);
{$ifndef NOGPM}
var
  e : TGPMEvent;
{$endif ndef NOGPM}
begin
{$ifdef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
{$else ndef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  if PendingMouseEvents>0 then
    begin
      MouseEvent:=PendingMouseHead^;
      inc(PendingMouseHead);
      if longint(PendingMouseHead)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
       PendingMouseHead:=@PendingMouseEvent;
      dec(PendingMouseEvents);
      if (LastMouseEvent.x<>MouseEvent.x) or (LastMouseEvent.y<>MouseEvent.y) then
       MouseEvent.Action:=MouseActionMove;
      if (LastMouseEvent.Buttons<>MouseEvent.Buttons) then
       begin
         if (LastMouseEvent.Buttons=0) then
          MouseEvent.Action:=MouseActionDown
         else
          MouseEvent.Action:=MouseActionUp;
       end;
      LastMouseEvent:=MouseEvent;
      exit;
    end;
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
  LastMouseEvent:=MouseEvent;
{ update mouse cursor }
  if mousecur then
   PlaceMouseCur(MouseEvent.y*ScreenWidth+MouseEvent.x);
{$endif ndef NOGPM}
{$endif ndef NOMOUSE}
end;


procedure PutMouseEvent(const MouseEvent: TMouseEvent);
begin
{$ifndef NOMOUSE}
  if PendingMouseEvents<MouseEventBufSize then
   begin
     PendingMouseTail^:=MouseEvent;
     inc(PendingMouseTail);
     if longint(PendingMouseTail)=longint(@PendingMouseEvent)+sizeof(PendingMouseEvent) then
      PendingMouseTail:=@PendingMouseEvent;
      { why isn't this done here ?
        so the win32 version do this by hand:}
       inc(PendingMouseEvents);
   end;
{$endif ndef NOMOUSE}
end;


function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;
{$ifndef NOGPM}
var
  e : TGPMEvent;
  fds : FDSet;
{$endif ndef NOGPM}
begin
{$ifdef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  exit(false);
{$else ndef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  if PendingMouseEvents>0 then
   begin
     MouseEvent:=PendingMouseHead^;
     PollMouseEvent:=true;
     exit;
   end
  else if gpm_fs<0 then
   exit(false);
{$ifndef NOGPM}
  if gpm_fs>0 then
    begin
      FD_Zero(fds);
      FD_Set(gpm_fd,fds);
    end;
  if (gpm_fs=-2) or (Select(gpm_fs+1,@fds,nil,nil,1)>0) then
   begin
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
     if (gpm_fs<>-2) or (MouseEvent.Action<>0) then
       PollMouseEvent:=true
     else
       PollMouseEvent:=false;
   end
  else
   PollMouseEvent:=false;
{$endif ndef NOGPM}
{$endif ndef NOMOUSE}
end;

end.
{
  $Log$
  Revision 1.3  2001-08-05 12:24:20  peter
    * m68k merges

  Revision 1.2  2001/01/21 20:21:40  marco
   * Rename fest II. Rtl OK

  Revision 1.1  2001/01/13 11:03:58  peter
    * API 2 RTL commit

}
