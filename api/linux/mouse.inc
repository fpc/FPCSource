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
{ open gpm }
  connect.EventMask:=GPM_MOVE or GPM_DRAG or GPM_DOWN or GPM_UP;
  connect.DefaultMask:=0;
  connect.MinMod:=0;
  connect.MaxMod:=0;
  Gpm_Open(connect,0);
{ show mousepointer }
  ShowMouse;
{$endif ndef NOMOUSE}
end;


procedure DoneMouse;
begin
{$ifndef NOMOUSE}
  HideMouse;
  Gpm_Close;
{$endif ndef NOMOUSE}
end;


function DetectMouse:byte;
begin
{$ifdef NOMOUSE}
  DetectMouse:=0;
{$else ndef NOMOUSE}
{ always a mouse deamon present }
  DetectMouse:=2;
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
{$endif ndef NOMOUSE}
end;

{
  $Log$
  Revision 1.2  2000-07-13 11:32:25  michael
  + removed logs
 
}
