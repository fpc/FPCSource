{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Michael Van Canneyt,
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$ifdef linux}
{$ifdef i386}
  {$define usestime}
{$endif}
{$endif}

{$ifdef usestime}

{$ifdef FPC_USE_LIBC}
function stime (t:ptime_t):cint; cdecl; external name 'stime';
{$else}
function stime (t:ptime_t):cint; 
begin
 stime:=do_SysCall(Syscall_nr_stime,TSysParam(t));
end;
{$endif}

function settimeofday(tp:ptimeval;tzp:ptimezone):cint;

begin
  settimeofday:=stime(@tp^.tv_sec);
end;

{$else}

{$ifdef FPC_USE_LIBC}
function settimeofday(tp:ptimeval;tzp:ptimezone):cint; cdecl; external clib name 'settimeofday';
{$else}
function settimeofday(tp:ptimeval;tzp:ptimezone):cint;

begin
  settimeofday:=do_SysCall(Syscall_nr_settimeofday,TSysParam(@tp),TSysParam(tzp));
end;
{$endif}
{$endif}

{
  $Log$
  Revision 1.5  2004-10-31 18:49:02  marco
   * cdecl for FPC_USE_LIBC

  Revision 1.4  2004/10/30 21:18:23  marco
   * more commits from unix interface cleanup

  Revision 1.3  2004/10/27 04:22:29  marco
  *** empty log message ***

  Revision 1.2  2004/10/27 04:22:04  marco
   * cvs log and extra endif

}

