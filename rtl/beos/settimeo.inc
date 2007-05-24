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
{$ifdef beos}
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

