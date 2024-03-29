{
   System dependent system messages for unix

   Copyright (c) 2002 by Pierre Muller

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


{ This file is still a dummy,
  it should use ioctl to get information about resizing of windows }

{$IFDEF FPC_DOTTEDUNITS}
uses
  UnixApi.Base,UnixApi.TermIO;
{$ELSE FPC_DOTTEDUNITS}
uses
  BaseUnix,termio;
{$ENDIF FPC_DOTTEDUNITS}

Const
  SystemEventActive : Boolean = false;

var
  lastxsize,lastysize : longint;

procedure InitSystemMsg;
var
  WinSize : TWinSize;
begin
  If SystemEventActive then
    exit;
  { Code to enable size tracking should go here }
  PendingSystemHead:=@PendingSystemEvent;
  PendingSystemTail:=@PendingSystemEvent;
  PendingSystemEvents:=0;
  FillChar(LastSystemEvent,sizeof(TSystemEvent),0);
  FillChar(WinSize,sizeof(WinSize),0);
  fpioctl(stdinputhandle,TIOCGWINSZ,@winsize);
  LastXSize:=WinSize.ws_row;
  LastYSize:=WinSize.ws_col;
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
var
  CloseState : word;
  WinSize : TWinSize;
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
     FillChar(WinSize,sizeof(WinSize),0);
     fpioctl(stdinputhandle,TIOCGWINSZ,@winsize);
     if (winsize.ws_col<>0) and (winsize.ws_row<>0) and
        ((winsize.ws_row<>lastxsize) or (winsize.ws_col<>lastysize)) then
       begin
         SystemEvent.typ:=SysResize;
         SystemEvent.x:=WinSize.ws_col;
         SystemEvent.y:=WinSize.ws_row;
         PutSystemEvent(SystemEvent);
         LastXSize:=WinSize.ws_row;
         LastYSize:=WinSize.ws_col;
         PollSystemEvent:=true;
       end
     else
       PollSystemEvent:=false;
    end;
end;

