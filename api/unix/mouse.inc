{
  System independent mouse interface for linux

  $Id$
}

uses
  Linux,Video
{$ifndef NOMOUSE}
{$ifdef OLDGPM}
  ,gpm114
{$else}
  ,gpm
{$endif}
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
{$ifndef NOMOUSE}
var
  connect : TGPMConnect;
{$endif ndef NOMOUSE}
begin
{$ifndef NOMOUSE}
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
{$endif ndef NOMOUSE}
end;


procedure DoneMouse;
begin
{$ifndef NOMOUSE}
  If gpm_fs<>-1 then
    begin
      HideMouse;
      Gpm_Close;
      gpm_fs:=-1;
    end;
{$endif ndef NOMOUSE}
end;


function DetectMouse:byte;
{$ifndef NOMOUSE}
var
  x : longint;
  e : TGPMEvent;
  connect : TGPMConnect;
{$endif ndef NOMOUSE}
begin
{$ifdef NOMOUSE}
  DetectMouse:=0;
{$else ndef NOMOUSE}
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
{$ifndef NOMOUSE}
var
  e : TGPMEvent;
{$endif ndef NOMOUSE}
begin
{$ifdef NOMOUSE}
  GetMouseX:=0;
{$else ndef NOMOUSE}
  if gpm_fd<0 then
   exit(0);
  Gpm_GetSnapshot(e);
  GetMouseX:=e.x-1;
{$endif ndef NOMOUSE}
end;


function GetMouseY:word;
{$ifndef NOMOUSE}
var
  e : TGPMEvent;
{$endif ndef NOMOUSE}
begin
{$ifdef NOMOUSE}
  GetMouseY:=0;
{$else ndef NOMOUSE}
  if gpm_fd<0 then
   exit(0);
  Gpm_GetSnapshot(e);
  GetMouseY:=e.y-1;
{$endif ndef NOMOUSE}
end;


function GetMouseButtons:word;
{$ifndef NOMOUSE}
var
  e : TGPMEvent;
{$endif ndef NOMOUSE}
begin
{$ifdef NOMOUSE}
  GetMouseButtons:=0;
{$else ndef NOMOUSE}
  if gpm_fd<0 then
   exit(0);
  Gpm_GetSnapshot(e);
  GetMouseButtons:=e.buttons;
{$endif ndef NOMOUSE}
end;


procedure SetMouseXY(x,y:word);
begin
end;


procedure GetMouseEvent(var MouseEvent: TMouseEvent);
{$ifndef NOMOUSE}
var
  e : TGPMEvent;
{$endif ndef NOMOUSE}
begin
{$ifdef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
{$else ndef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  if gpm_fd<0 then
   exit;
  Gpm_GetEvent(e);
  MouseEvent.x:=e.x-1;
  MouseEvent.y:=e.y-1;
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
{$endif ndef NOMOUSE}
end;


function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;

{$ifndef NOMOUSE}
var
  e : TGPMEvent;
  fds : FDSet;
{$endif ndef NOMOUSE}
begin
{$ifdef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  exit(false);
{$else ndef NOMOUSE}
  fillchar(MouseEvent,SizeOf(TMouseEvent),#0);
  if gpm_fd<0 then
   exit(false);
  if gpm_fd>0 then
    begin
      FD_Zero(fds);
      FD_Set(gpm_fd,fds);
    end;
  if (gpm_fd=-2) or (Select(gpm_fd+1,@fds,nil,nil,1)>0) then
   begin
     Gpm_GetSnapshot(e);
     MouseEvent.x:=e.x-1;
     MouseEvent.y:=e.y-1;
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
     if (gpm_fd<>-2) or (MouseEvent.Action<>0) then
       PollMouseEvent:=true
     else
       PollMouseEvent:=false;
   end
  else
   PollMouseEvent:=false;
{$endif ndef NOMOUSE}
end;

{
  $Log$
  Revision 1.2  2000-10-26 23:08:48  peter
    * merged freebsd from fixes

  Revision 1.1.2.1  2000/10/25 12:23:20  marco
   * Linux dir split up

  Revision 1.1.2.1  2000/10/24 07:58:49  pierre
   * get mouse to not crash on xterm, its now completely disabled

  Revision 1.1  2000/07/13 06:29:39  michael
  + Initial import

  Revision 1.3  2000/06/30 09:00:33  jonas
    * compiles again with -dnomouse

  Revision 1.2  2000/04/17 08:51:38  pierre
   + set conditional NOMOUSE to get dummy mouse unit

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/11/24 23:36:38  peter
    * moved to packages dir

  Revision 1.5  1999/07/01 19:41:26  peter
    * define OLDGPM to compile with old gpm (for v1.14) else the new
      gpm unit from rtl will be used (v1.17)

  Revision 1.4  1999/06/23 00:01:30  peter
    * check for videobuf=nil

  Revision 1.3  1999/03/31 20:20:18  michael
  + Fixed probmem preventing IDE to run in x-term.

  Revision 1.2  1998/12/11 00:13:20  peter
    + SetMouseXY
    * use far for exitproc procedure

  Revision 1.1  1998/12/04 12:48:30  peter
    * moved some dirs

  Revision 1.3  1998/12/01 15:08:16  peter
    * fixes for linux

  Revision 1.2  1998/10/29 12:49:49  peter
    * more fixes

}