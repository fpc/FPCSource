{
   System dependent system messages for AmigaOS/MorphOS

   Copyright (c) 2008 by Karoly Balogh

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
   MA 02110-1301, USA.
}



uses
  video;

Const
  SystemEventActive : Boolean = false;

var
  lastxsize,lastysize : longint;

procedure InitSystemMsg;
begin
  If SystemEventActive then
    exit;
  { Code to enable size tracking should go here }
  PendingSystemHead:=@PendingSystemEvent;
  PendingSystemTail:=@PendingSystemEvent;
  PendingSystemEvents:=0;
  FillChar(LastSystemEvent,sizeof(TSystemEvent),0);
  Video.HasResizeWindow(LastXSize,LastYSize);

  If LastXSize=0 then
    LastXSize:=80;
  If LastYSize=0 then
    LastYSize:=25;

  SystemEventActive:=true;
end;


procedure DoneSystemMsg;
begin
  if not SystemEventActive then
    exit;
  { Code to disable size tracking should go here }
  SystemEventActive:=false;
end;

procedure GetSystemEvent(var SystemEvent: TSystemEvent);
begin
  if PendingSystemEvents=0 then
    PollSystemEvent(SystemEvent);
  if PendingSystemEvents=0 then
    exit;
  SystemEvent:=PendingSystemHead^;
  inc(PendingSystemHead);
  if longint(PendingSystemHead)=longint(@PendingSystemEvent)+sizeof(PendingSystemEvent) then
   PendingSystemHead:=@PendingSystemEvent;
  dec(PendingSystemEvents);
  LastSystemEvent:=SystemEvent;
end;


function PollSystemEvent(var SystemEvent: TSystemEvent):boolean;
var width, height : longint;
begin
  SystemEvent.typ:=SysNothing;
  if not SystemEventActive then
    exit(false);
  if PendingSystemEvents>0 then
   begin
     SystemEvent:=PendingSystemHead^;
     PollSystemEvent:=true;
   end
  else
   begin
     PollSystemEvent:=false;
     if Video.HasCloseWindow then begin
       SystemEvent.typ:=SysClose;
       SystemEvent.CloseTyp:=0;
       PutSystemEvent(SystemEvent);
       PollSystemEvent:=true;
     end else if Video.HasResizeWindow(width, height) then begin
       if (width>0) and (height>0) and 
          ((width<>lastxsize) or (height<>lastysize)) then begin
         SystemEvent.typ:=SysResize;
         SystemEvent.x:=width;
         SystemEvent.y:=height;
         PutSystemEvent(SystemEvent);
         LastXSize:=width;
         LastYSize:=height;
         PollSystemEvent:=true;
       end;
     end else if Video.HasActiveWindow then begin
       SystemEvent.typ:=SysSetFocus;
       PutSystemEvent(SystemEvent);
       PollSystemEvent:=true;
     end else if Video.HasInactiveWindow then begin
       SystemEvent.typ:=SysReleaseFocus;
       PutSystemEvent(SystemEvent);
       PollSystemEvent:=true;
     end;
   end;
end;
