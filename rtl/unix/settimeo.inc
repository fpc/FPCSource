{
   This file is part of the Free Pascal run time library.
   Copyright (c) 2004 by Michael Van Canneyt,
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

{$ifdef usestime}

function stime (t:ptime_t):cint;
begin
 stime:=do_SysCall(Syscall_nr_stime,TSysParam(t));
end;

function fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;
begin
  fpsettimeofday:=stime(@tp^.tv_sec);
end;

{$else}

function fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;
begin
  fpsettimeofday:=do_SysCall(Syscall_nr_settimeofday,TSysParam(tp),TSysParam(tzp));
end;

{$endif}


