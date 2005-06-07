{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{ some declarations for Netware API calls }
{$I nwsys.inc}
{$I errno.inc}


var
  CloseAllRemainingSemaphores : TSysCloseAllRemainingSemaphores = nil;
  ReleaseThreadVars : TSysReleaseThreadVars = nil;
  SetThreadDataAreaPtr : TSysSetThreadDataAreaPtr = nil;
  TerminatingThreadID : longint = 0;  {used for unload, the signal handler will}
                                      {be called from the console thread. avoid}
                                      {calling _exit in another thread}

procedure NWSysSetThreadFunctions (crs:TSysCloseAllRemainingSemaphores;
                                   rtv:TSysReleaseThreadVars;
                                   stdata:TSysSetThreadDataAreaPtr);
begin
  CloseAllRemainingSemaphores := crs;
  ReleaseThreadVars := rtv;
  SetThreadDataAreaPtr := stdata;
end;



