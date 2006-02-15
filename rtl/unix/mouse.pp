{
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
  BaseUnix,Video
{$ifndef NOGPM}
  ,gpm
{$endif ndef NOGPM}
  ;

{$i mouse.inc}

{$ifndef NOMOUSE}

const
  WaitMouseMove : boolean = false;
  PrintMouseCur : boolean = false;
  mousecurofs : longint = -1;

var
  mousecurcell : TVideoCell;
  SysLastMouseEvent : TMouseEvent;

const
  gpm_fs : longint = -1;

{$ifndef NOGPM}
procedure GPMEvent2MouseEvent(const e:Tgpm_event;var mouseevent:tmouseevent);
var
  PrevButtons : byte;

begin
  PrevButtons:=SysLastMouseEvent.Buttons;
  if e.x>0 then
   mouseevent.x:=e.x-1
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
    GPM_DRAG :
      begin
        MouseEvent.Action:=MouseActionMove;
        WaitMouseMove:=false;
      end;
    GPM_DOWN :
      begin
        MouseEvent.Action:=MouseActionDown;
        WaitMouseMove:=false;
      end;
    GPM_UP :
      begin
        { gpm apparently sends the button that is left up
          while mouse unit expects the button state after
          the button was released PM }
        if MouseEvent.Buttons<>0 then
          begin
            MouseEvent.Buttons:=MouseEvent.Buttons xor PrevButtons;
            MouseEvent.Action:=MouseActionUp;
          end
        { this does probably never happen...
          but its just a security PM }
        else
          MouseEvent.Action:=MouseActionMove;
        WaitMouseMove:=false;
      end;
  else
   MouseEvent.Action:=MouseActionMove;
  end;
end;
{$ENDIF}

procedure PlaceMouseCur(ofs:longint);
var
  upd : boolean;
begin
  if (VideoBuf=nil) or (MouseCurOfs=Ofs) then
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
  E : Tgpm_event;
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
      if (gpm_fs=-2) and (fpgetenv('TERM')<>'xterm') then
        begin
          gpm_fs:=-1;
          Gpm_Close;
        end;
      { initialize SysLastMouseEvent }
      if gpm_fs<>-1 then
       begin
         Gpm_GetSnapshot(e);
         GPMEvent2MouseEvent(e,SysLastMouseEvent);
       end;
    end;
  { show mousepointer }
  if gpm_fs<>-1 then
    ShowMouse;
{$else ifdef NOGPM}
      if (fpgetenv('TERM')='xterm') then
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
  connect : TGPMConnect;
  fds : tFDSet;
  e : Tgpm_event;
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
      if (gpm_fs=-2) and (fpgetenv('TERM')<>'xterm') then
        begin
          Gpm_Close;
          gpm_fs:=-1;
        end;
    end;
  if gpm_fs>=0 then
    begin
      fpFD_ZERO(fds);
      fpFD_SET(gpm_fs,fds);
      while fpSelect(gpm_fs+1,@fds,nil,nil,1)>0 do
        begin
          fillchar(e,sizeof(e),#0);
          Gpm_GetEvent(e);
        end;
    end;
  if gpm_fs<>-1 then
    SysDetectMouse:=Gpm_GetSnapshot(nil)
  else
    SysDetectMouse:=0;
{$else ifdef NOGPM}
{ always a mouse deamon present }
  if (fpgetenv('TERM')='xterm') then
    SysDetectMouse:=2;
{$endif NOGPM}
end;


procedure SysGetMouseEvent(var MouseEvent: TMouseEvent);
{$ifndef NOGPM}
var
  e : Tgpm_event;
{$endif ndef NOGPM}
begin
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  if gpm_fs<0 then
   exit;
{$ifndef NOGPM}
  Gpm_GetEvent(e);
  GPMEvent2MouseEvent(e,MouseEvent);
  SysLastMouseEvent:=MouseEvent;
{ update mouse cursor }
  if PrintMouseCur then
   PlaceMouseCur(MouseEvent.y*ScreenWidth+MouseEvent.x);
{$endif ndef NOGPM}
end;



function SysPollMouseEvent(var MouseEvent: TMouseEvent):boolean;
{$ifndef NOGPM}
var
  e : Tgpm_event;
  fds : tFDSet;
{$endif ndef NOGPM}
begin
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
{$ifndef NOGPM}
  if gpm_fs<0 then
   exit(false);
  if gpm_fs>0 then
    begin
      fpFD_ZERO(fds);
      fpFD_SET(gpm_fs,fds);
    end;
  if (fpSelect(gpm_fs+1,@fds,nil,nil,1)>0) then
   begin
     FillChar(e,SizeOf(e),#0);
     { Gpm_snapshot does not work here PM }
     Gpm_GetEvent(e);
     GPMEvent2MouseEvent(e,MouseEvent);
     SysLastMouseEvent:=MouseEvent;
     if (MouseEvent.Action<>0) then
       begin
         { As we now use Gpm_GetEvent, we need to put in
           in the MouseEvent queue PM }
         PutMouseEvent(MouseEvent);
         SysPollMouseEvent:=true;
         { update mouse cursor is also required here
           as next call will read MouseEvent from queue }
         if PrintMouseCur then
           PlaceMouseCur(MouseEvent.y*ScreenWidth+MouseEvent.x);
       end
     else
       SysPollMouseEvent:=false;
   end
  else
{$endif NOGPM}
   SysPollMouseEvent:=false;
end;

function SysGetMouseX:word;
{$ifndef NOGPM}
var
  me : TMouseEvent;
{$endif ndef NOGPM}
begin
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  if PollMouseEvent(ME) then
   begin
     { Remove mouse event, we are only interrested in
       the X,Y so all other events can be thrown away }
     GetMouseEvent(ME);
     SysGetMouseX:=ME.X
   end
  else
    begin
      SysGetMouseX:=SysLastMouseEvent.x;
    end;
{$endif ndef NOGPM}
end;


function SysGetMouseY:word;
{$ifndef NOGPM}
var
  me : TMouseEvent;
{$endif ndef NOGPM}
begin
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  if PollMouseEvent(ME) then
   begin
     { Remove mouse event, we are only interrested in
       the X,Y so all other events can be thrown away }
     GetMouseEvent(ME);
     SysGetMouseY:=ME.Y
   end
  else
    begin
      SysGetMouseY:=SysLastMouseEvent.y;
    end;
{$endif ndef NOGPM}
end;


procedure SysShowMouse;
var
  x,y : word;
begin
  PrintMouseCur:=true;
  { Wait with showing the cursor until the mouse has moved. Else the
    cursor updates will be to quickly }
  if WaitMouseMove then
   exit;
  if (MouseCurOfs>=0) or (gpm_fs=-1) then
    PlaceMouseCur(MouseCurOfs)
  else
    begin
      x:=SysGetMouseX;
      y:=SysGetMouseY;
      if (x<=ScreenWidth) and (y<=ScreenHeight) then
        PlaceMouseCur(Y*ScreenWidth+X)
      else
        PlaceMouseCur(MouseCurOfs);
    end;
end;


procedure SysHideMouse;
begin
  if (MouseCurOfs>=0) then
    PlaceMouseCur(-1);
  WaitMouseMove:=true;
  PrintMouseCur:=false;
end;


function SysGetMouseButtons:word;
{$ifndef NOGPM}
var
  me : TMouseEvent;
{$endif ndef NOGPM}
begin
  if gpm_fs<0 then
   exit(0);
{$ifndef NOGPM}
  if PollMouseEvent(ME) then
   begin
     { Remove mouse event, we are only interrested in
       the buttons so all other events can be thrown away }
     GetMouseEvent(ME);
     SysGetMouseButtons:=ME.Buttons;
   end
  else
    begin
      SysGetMouseButtons:=SysLastMouseEvent.buttons;
    end;
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
