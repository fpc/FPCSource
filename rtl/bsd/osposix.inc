{
    $Id$
    Copyright (c) 2002 by Marco van de Voort.

    Declarations to import basic syscalls declarations into other base
    units.

    The conditional uselibc can be used to switch from libc to syscall
    usage for basic primitives, but it is best to use unit POSIX if
    possible. Note that the system unit must also be compiled using uselibc.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}

Uses Sysctl;

{$I bsdtypes.inc}
{$I bsdmacro.inc}

{$ifdef uselibc}
  {$Linklib c}

{   var
     Errno : cint; external name 'errno';}

    function sys_time(var tloc:time_t): time_t; cdecl; external name 'time';
    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function sys_close(fd : cint): cint; cdecl; external name 'close';
    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function sys_unlink(const path: pchar): cint; cdecl; external name 'unlink';
    function sys_rename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function sys_stat(const path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    function sys_chdir(const path : pchar): cint; cdecl; external name 'chdir';
    function sys_mkdir(const path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function sys_rmdir(const path : pchar): cint; cdecl; external name 'rmdir';
    function sys_opendir(const dirname : pchar): pdir; cdecl; external name 'opendir';
    function sys_readdir(var dirp : dir) : pdirent;cdecl; external name 'readdir';
    function sys_closedir(var dirp : dir): cint; cdecl; external name 'closedir';
    procedure sys_exit(status : cint); cdecl; external name '_exit';
    function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
    function sys_ftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function sys_rename(const old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function sys_fstat(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function sys_fork : pid_t; cdecl; external name 'fork';
    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; cdecl; external name 'execve';
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    function sys_access(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';
//  function sys_uname(var name: utsname): cint; cdecl; external name 'uname';
    function sys_Dup(oldd:cint):cint; cdecl; external name 'dup';
    function sys_Dup2(oldd:cint;newd:cint):cint; cdecl; external name 'dup2';

{$else}

{*****************************************************************************
                     --- Main:The System Call Self ---
*****************************************************************************}

{ The system designed for Linux can't be used for FreeBSD so easily, since
  FreeBSD pushes arguments, instead of loading them to registers.}

function sys_time(var tloc:time_t): time_t; external name 'FPC_SYSC_TIME';
function sys_open(const path: pchar; flags : cint; mode: mode_t):cint;  external name 'FPC_SYSC_OPEN';
function sys_close(fd : cint): cint;  external name 'FPC_SYSC_CLOSE';
function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t; external name 'FPC_SYSC_LSEEK';
function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t; external name 'FPC_SYSC_READ';
function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t;  external name 'FPC_SYSC_WRITE';
function sys_unlink(const path: pchar): cint;  external name 'FPC_SYSC_UNLINK';
function sys_rename(const old : pchar; const newpath: pchar): cint;  external name 'FPC_SYSC_RENAME';
function sys_stat(const path: pchar; var buf : stat):cint;  external name 'FPC_SYSC_STAT';
function sys_chdir(const path : pchar): cint; external name 'FPC_SYSC_CHDIR';
function sys_mkdir(const path : pchar; mode: mode_t):cint; external name 'FPC_SYSC_MKDIR';
function sys_rmdir(const path : pchar): cint; external name 'FPC_SYSC_RMDIR';
function sys_opendir(const dirname : pchar): pdir; external name 'FPC_SYSC_OPENDIR';
function sys_closedir(var dirp : dir): cint; external name 'FPC_SYSC_CLOSEDIR';
function sys_readdir(var dirp : dir) : pdirent; external name 'FPC_SYSC_READDIR';
procedure sys_exit(status : cint); external name 'FPC_SYSC_EXIT';
function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint;  external name 'FPC_SYSC_SIGACTION';
function sys_ftruncate(fd : cint; flength : off_t): cint; external name 'FPC_SYSC_FTRUNCATE';
function sys_fstat(fd : cint; var sb : stat): cint; external name 'FPC_SYSC_FSTAT';
function sys_fork : pid_t; external name 'FPC_SYSC_FORK';
function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint; external name 'FPC_SYSC_EXECVE';
function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; external name 'FPC_SYSC_WAITPID';
function sys_access(const pathname : pchar; amode : cint): cint;external name 'FPC_SYSC_ACCESS';
function sys_Dup(oldd:cint):cint;  external name 'FPC_SYSC_DUP';
function sys_Dup2(oldd:cint;newd:cint):cint; external name 'FPC_SYSC_DUP2';
function geterrno:cint; external name  'FPC_SYS_GETERRNO';
procedure seterrno (i:cint); external name  'FPC_SYS_SETERRNO';

{$endif}

{$I bsdfuncs.inc}



{
 $Log$
 Revision 1.6  2002-10-26 18:27:51  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.5  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.4  2002/08/21 07:03:16  marco
  * Fixes from Tuesday.

 Revision 1.3  2002/08/19 12:29:11  marco
  * First working POSIX *BSD system unit.



}
