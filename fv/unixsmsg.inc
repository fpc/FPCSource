{
   $Id$
   System dependent system messages for unix

   Copyright (c) 2002 by Pierre Muller

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
}


{ This file is still a dummy,
  it should use ioctl to get information about resizing of windows }

Const
  SystemEventActive : Boolean = false;


procedure InitSystemMsg;
begin
  If SystemEventActive then
    exit;
  { Code to enable size tracking should go here }
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
  PollSystemEvent(SystemEvent);
end;

function PollSystemEvent(var SystemEvent: TSystemEvent):boolean;
var
  CloseState : word;
begin
  SystemEvent.typ:=SysNothing;
  if not SystemEventActive then
    exit(false);
  PollSystemEvent:=false;
end;

{
  $Log$
  Revision 1.1  2002-05-21 11:59:57  pierre
   + system messages unit added

}
