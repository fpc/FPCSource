{
  System independent mouse interface for linux

  $Id$
}

uses
  Linux,Video
{$ifdef OLDGPM}
  ,gpm114
{$else}
  ,gpm
{$endif}
  ;

const
  mousecur    : boolean = false;
  mousecurofs : longint = -1;

var
  mousecurcell : TVideoCell;


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


procedure InitMouse;
var
  connect : TGPMConnect;
begin
  PendingMouseHead:=@PendingMouseEvent;
  PendingMouseTail:=@PendingMouseEvent;
  PendingMouseEvents:=0;
  FillChar(LastMouseEvent,sizeof(TMouseEvent),0);
{ open gpm }
  connect.EventMask:=GPM_MOVE or GPM_DRAG or GPM_DOWN or GPM_UP;
  connect.DefaultMask:=0;
  connect.MinMod:=0;
  connect.MaxMod:=0;
  Gpm_Open(connect,0);
{ show mousepointer }
  ShowMouse;
end;


procedure DoneMouse;
begin
  HideMouse;
  Gpm_Close;
end;


function DetectMouse:byte;
begin
{ always a mouse deamon present }
  DetectMouse:=2;
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
var
  e : TGPMEvent;
begin
  if gpm_fd<0 then
   exit(0);
  Gpm_GetSnapshot(e);
  GetMouseX:=e.x-1;
end;


function GetMouseY:word;
var
  e : TGPMEvent;
begin
  if gpm_fd<0 then
   exit(0);
  Gpm_GetSnapshot(e);
  GetMouseY:=e.y-1;
end;


function GetMouseButtons:word;
var
  e : TGPMEvent;
begin
  if gpm_fd<0 then
   exit(0);
  Gpm_GetSnapshot(e);
  GetMouseButtons:=e.buttons;
end;


procedure SetMouseXY(x,y:word);
begin
end;


procedure GetMouseEvent(var MouseEvent: TMouseEvent);
var
  e : TGPMEvent;
begin
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
end;


function PollMouseEvent(var MouseEvent: TMouseEvent):boolean;

var
  e : TGPMEvent;
  fds : FDSet;
begin
  if gpm_fd<0 then
   exit(false);
  FD_Zero(fds);
  FD_Set(gpm_fd,fds);
  if (Select(gpm_fd+1,@fds,nil,nil,1)>0) then
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
     PollMouseEvent:=true;
   end
  else
   PollMouseEvent:=false;
end;

{
  $Log$
  Revision 1.1  2000-01-06 01:20:31  peter
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
