{

   Unit to handle system events

   Copyright 2000 by Pierre Muller <muller@ics.u-strasbg.fr>

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

 ****************************************************************************}
Unit sysmsg;

interface
{$i platform.inc}

type
  TSystemMessage = (
    SysNothing,
    SysSetFocus,
    SysReleaseFocus,
    SysClose,
    SysResize );

  TSystemEvent = Record
    case typ : TSystemMessage of
      SysClose : ( CloseTyp : Longint);
      SysResize : (X,Y : Longint);
    end;

  PSystemEvent = ^TSystemEvent;

const
  SystemEventBufSize = 16;

var
  PendingSystemEvent  : array[0..SystemEventBufSize-1] of TSystemEvent;
  PendingSystemHead,
  PendingSystemTail   : PSystemEvent;
  PendingSystemEvents : byte;

  LastSystemEvent : TSystemEvent;


procedure InitSystemMsg;

procedure DoneSystemMsg;

procedure GetSystemEvent(var SystemEvent:TSystemEvent);
{ Returns the last SystemEvent, and waits for one if not available }

procedure PutSystemEvent(const SystemEvent: TSystemEvent);
{ Adds the given SystemEvent to the input queue. }

function PollSystemEvent(var SystemEvent: TSystemEvent):boolean;
{ Checks if a SystemEvent is available, and returns it if one is found. If no
  event is pending, it returns 0 }

implementation

{$undef HAS_SYSMSG}

{$ifdef go32v2}
{$i go32smsg.inc}
{$define HAS_SYSMSG}
{$endif go32v2}
{$ifdef OS_WINDOWS}
{$i w32smsg.inc}
{$define HAS_SYSMSG}
{$endif OS_WINDOWS}
{$ifdef unix}
{$i unixsmsg.inc}
{$define HAS_SYSMSG}
{$endif unix}

{$ifdef amiga}
{$i amismsg.inc}
{$define HAS_SYSMSG}
{$endif amiga}
{$ifdef morphos}
{$i amismsg.inc}
{$define HAS_SYSMSG}
{$endif morphos}
{$ifdef aros}
{$i amismsg.inc}
{$define HAS_SYSMSG}
{$endif aros}

{$ifdef HAS_SYSMSG}

procedure PutSystemEvent(const SystemEvent: TSystemEvent);
begin
  if PendingSystemEvents<SystemEventBufSize then
   begin
     PendingSystemTail^:=SystemEvent;
     inc(PendingSystemTail);
     if longint(PendingSystemTail)=longint(@PendingSystemEvent)+sizeof(PendingSystemEvent) then
      PendingSystemTail:=@PendingSystemEvent;
       inc(PendingSystemEvents);
   end;
end;

{$else HAS_SYSMSG}

procedure InitSystemMsg;
begin
end;

procedure DoneSystemMsg;
begin
end;

procedure GetSystemEvent(var SystemEvent:TSystemEvent);
begin
  FillChar(SystemEvent,SizeOf(SystemEvent),#0);
end;

function PollSystemEvent(var SystemEvent: TSystemEvent):boolean;
begin
  FillChar(SystemEvent,SizeOf(SystemEvent),#0);
  PollSystemEvent:=false;
end;

procedure PutSystemEvent(const SystemEvent: TSystemEvent);
begin
end;

{$endif not HAS_SYSMSG}

end.
