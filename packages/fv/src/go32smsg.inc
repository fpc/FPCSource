{

   System independent system interface for go32v2

   Copyright (c) 2000 by Pierre Muller

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

Const
  SystemEventActive : Boolean = false;


procedure InitSystemMsg;
var
  res : word;
begin
  if SystemEventActive then
    exit;
  { enable close }
  asm
    movl $0x168f,%eax
    movl $1,%edx
    int $0x2f
    movw %ax,Res
  end;
  SystemEventActive:=(Res=0);
end;


procedure DoneSystemMsg;
begin
  if not SystemEventActive then
    exit;
  { disable close }
  asm
    movl $0x168f,%eax
    movl $0,%edx
    int $0x2f
  end;
  SystemEventActive:=false;
end;

procedure GetSystemEvent(var SystemEvent: TSystemEvent);
begin
  PollSystemEvent(SystemEvent);
end;

function PollSystemEvent(var SystemEvent: TSystemEvent):boolean;
var
  CloseState : word;
begin
  SystemEvent.typ:=SysNothing;
  if not SystemEventActive then
    exit(false);
  { Query close }
  asm
    movl $0x168f,%eax
    movl $100,%edx
    int $0x2f
    movw %ax,CloseState
  end;
  if (CloseState = 0) then
    begin
      { acknowledge Close }
      asm
        movl $0x168f,%eax
        movl $200,%edx
        int $0x2f
        movw %ax,CloseState
      end;
      { non zero means error ! }
      if CloseState=0 then
        begin
          PollSystemEvent:=true;
          SystemEvent.typ:=SysClose;
        end;
    end;
end;

