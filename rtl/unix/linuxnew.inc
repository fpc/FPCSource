{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   New linux unit. Linux only calls only. Will be renamed to linux.pp
   when 1.0.x support is killed off.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
unit Linux;

interface

Type
  TSysinfo = packed record
    uptime    : longint;
    loads     : array[1..3] of longint;
    totalram,
    freeram,
    sharedram,
    bufferram,
    totalswap,
    freeswap  : longint;
    procs     : integer;
    s         : string[18];
  end;
  PSysInfo = ^TSysInfo;


Function Sysinfo(var Info:TSysinfo):Boolean;

implementation

Uses Syscall;

Function Sysinfo(var Info:TSysinfo):Boolean;
{
  Get system info
}
Begin
  Sysinfo:=do_SysCall(SysCall_nr_Sysinfo,longint(@info))=0;
End;


end.

{
   $Log$
   Revision 1.2  2003-09-15 20:08:49  marco
    * small fixes. FreeBSD now cycles

   Revision 1.1  2003/09/15 14:12:17  marco
    * moved linux.pp contents to linuxold etc


}
