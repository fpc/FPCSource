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

{$if defined(aix) or defined(bsd) or defined(solaris)}
{$define NOMOUSE}
{$endif}

{$if defined(haiku)}
{$define NOGPM}
{$endif}

{$ifdef NOMOUSE}
{$DEFINE NOGPM}
{$ENDIF}

{$i mouseh.inc}

implementation

uses
  BaseUnix,Video
{$ifndef NOGPM}
  ,gpm,linuxvcs
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

{Note: libgpm will initialize an xterm mouse if TERM=xterm.
       However, this check sucks, because xterm is not the only terminal
       with mouse. To make it worse, it assumes gpm should be used on
       anything not xterm, while in reality only the Linux console has gpm.

       Some distributions use a patched libgpm to work around this, but
       to avoid this mess, we detect the xterm mouse ourselves (we need to
       be able to do this anyway for the NOGPM case), and don't do any libgpm
       call at all if an xterm mouse is detected. Of course, we use the
       Pascal libgpm translation, doing it here allows us to keep the Pascal
       one compatible with the external C one.
       }

function detect_xterm_mouse:word;

const mouse_terminals:array[0..6] of string[7]=('cons','eterm','gnome',
                                                'konsole','rxvt','screen',
                                                'xterm');
      xterm=6;
      mouse_1003_capable=[xterm]; {xterm only for now}

var term:string;
    i,t:shortint;

begin
  detect_xterm_mouse:=0;
  t:=-1;
  term:=fpgetenv('TERM');
  for i:=low(mouse_terminals) to high(mouse_terminals) do
    if copy(term,1,length(mouse_terminals[i]))=mouse_terminals[i] then
      begin
        t:=i;
        break;
      end;
  if t=xterm then 
    begin
      {Rxvt sets TERM=xterm and COLORTERM=rxvt. Gnome does something similar.}
      term:=fpgetenv('COLORTERM');
      for i:=low(mouse_terminals) to high(mouse_terminals) do
        if copy(term,1,length(mouse_terminals[i]))=mouse_terminals[i] then
          begin
            t:=i;
            break;
          end;
    end;
  if t>0 then
    begin
      detect_xterm_mouse:=1000;
      {Can the terminal report all mouse events?}
      if t in mouse_1003_capable then
        detect_xterm_mouse:=1003;
    end;
end;

procedure SysInitMouse;

{$ifndef NOGPM}
var connect:TGPMConnect;
    e:Tgpm_event;
{$endif ndef NOGPM}

begin
{  if gpm_fs<>-1 then
    runerror(240);}
  {Test wether to use X-terminals.}
  case detect_xterm_mouse of
    1000:
      begin
        {Use the xterm mouse, report button events only.}
        gpm_fs:=-1000;
        {write(#27'[?1001s');} { save old hilit tracking }
        write(#27'[?1000h'); { enable mouse tracking }
      end;
    1003:
      begin
        {Use the xterm mouse, report all mouse events.}
        gpm_fs:=-1003;
        write(#27'[?1003h'); { enable mouse tracking }
      end;
  end;
{$ifndef NOGPM}
  {Use the gpm mouse?}
  if (gpm_fs=-1) and (vcs_device<>-1) then
    begin
      { open gpm }
      connect.EventMask:=GPM_MOVE or GPM_DRAG or GPM_DOWN or GPM_UP;
      connect.DefaultMask:=0;
      connect.MinMod:=0;
      connect.MaxMod:=0;
      gpm_fs:=gpm_open(connect,0);
      { initialize SysLastMouseEvent }
      if gpm_fs<>-1 then
       begin
         Gpm_GetSnapshot(e);
         GPMEvent2MouseEvent(e,SysLastMouseEvent);
       end;
    end;
{$endif NOGPM}
end;


procedure SysDoneMouse;

begin
  case gpm_fs of
    -1:
      HideMouse;
    -1000:
      begin
        {xterm mouse}
        write(#27'[?1000l'); { disable mouse tracking }
        {write(#27'[?1001r');} { Restore old hilit tracking }
      end;
    -1003:
      write(#27'[?1003l'); { disable mouse tracking }
{$ifndef NOGPM}
    else
      gpm_close;
{$endif}
  end;
  gpm_fs:=-1;
end;


function SysDetectMouse:byte;
{$ifndef NOGPM}
var
  connect : TGPMConnect;
  fds : tFDSet;
  e : Tgpm_event;
{$endif ndef NOGPM}
begin
  if detect_xterm_mouse<>0 then
    SysDetectMouse:=2
{$ifndef NOGPM}
  else
    begin
      if gpm_fs=-1 then
        begin
          connect.EventMask:=GPM_MOVE or GPM_DRAG or GPM_DOWN or GPM_UP;
          connect.DefaultMask:=0;
          connect.MinMod:=0;
          connect.MaxMod:=0;
          gpm_fs:=gpm_open(connect,0);
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
    end
{$endif NOGPM};
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
