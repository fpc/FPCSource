{
    $Id$
    Copyright (c) 2002 by Marco van de Voort.

    Implementation of the POSIX unit for *BSD. In practice only includes
    other files, or specifies libc bindings.

    The conditional uselibc can be used to switch from libc to syscall
    usage for basic primitives, but it is best to use unit POSIX if
    possible. Note that the system unit must also be compiled using uselibc.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ****************************************************************************
}

Uses Sysctl;

{$I ostypes.inc}
{$I bunxmacr.inc}

{$ifdef uselibc}
  {$Linklib c}

{   var
     Errno : cint; external name 'errno';}

    function Fptime(var tloc:time_t): time_t; cdecl; external name 'time';
    function Fpopen(path: pchar; flags : cint; mode: mode_t):cint; cdecl; external name 'open';
    function Fpclose(fd : cint): cint; cdecl; external name 'close';
    function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; cdecl; external name 'lseek';
    function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; cdecl; external name 'read';
    function Fpwrite(fd: cint;buf:pchar; nbytes : size_t): ssize_t; cdecl; external name 'write';
    function Fpunlink(path: pchar): cint; cdecl; external name 'unlink';
    function Fprename(old : pchar; const newpath: pchar): cint; cdecl;external name 'rename';
    function Fpstat( path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    function Fpchdir(path : pchar): cint; cdecl; external name 'chdir';
    function Fpmkdir(path : pchar; mode: mode_t):cint; cdecl; external name 'mkdir';
    function Fprmdir(path : pchar): cint; cdecl; external name 'rmdir';
    function Fpopendir(dirname : pchar): pdir; cdecl; external name 'opendir';
    function Fpreaddir(var dirp : dir) : pdirent;cdecl; external name 'readdir';
    function Fpclosedir(var dirp : dir): cint; cdecl; external name 'closedir';
    procedure Fpexit(status : cint); cdecl; external name '_exit';
    function Fpsigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
    function Fpftruncate(fd : cint; flength : off_t): cint; cdecl; external name 'ftruncate';
    function Fprename(old : pchar; newpath: pchar): cint; cdecl;external name 'rename';
    function Fpfstat(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function Fpfork : pid_t; cdecl; external name 'fork';
    function Fpexecve(path : pchar; argv : ppchar; envp: ppchar): cint; cdecl; external name 'execve';
    function Fpwaitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    function Fpaccess(pathname : pchar; amode : cint): cint; cdecl; external name 'access';
    function Fpuname(var name: utsname): cint; cdecl; external name 'uname';
    function FpDup(fildes:cint):cint; cdecl; external name 'dup';
    function FpDup2(fildes:cint;fildes2:cint):cint; cdecl; external name 'dup2';

{$else}

// uses syscalls.

function Fptime(var tloc:time_t): time_t; external name 'FPC_SYSC_TIME';
function Fpopen(path: pchar; flags : cint; mode: mode_t):cint;  external name 'FPC_SYSC_OPEN';
function Fpclose(fd : cint): cint;  external name 'FPC_SYSC_CLOSE';
function Fplseek(fd : cint; offset : off_t; whence : cint): off_t; external name 'FPC_SYSC_LSEEK';
function Fpread(fd: cint; buf: pchar; nbytes : size_t): ssize_t; external name 'FPC_SYSC_READ';
function Fpwrite(fd: cint;buf:pchar; nbytes : size_t): ssize_t;  external name 'FPC_SYSC_WRITE';
function Fpunlink(path: pchar): cint;  external name 'FPC_SYSC_UNLINK';
function Fprename(old : pchar; newpath: pchar): cint;  external name 'FPC_SYSC_RENAME';
function Fpstat(path: pchar; var buf : stat):cint;  external name 'FPC_SYSC_STAT';
function Fpchdir(path : pchar): cint; external name 'FPC_SYSC_CHDIR';
function Fpmkdir(path : pchar; mode: mode_t):cint; external name 'FPC_SYSC_MKDIR';
function Fprmdir(path : pchar): cint; external name 'FPC_SYSC_RMDIR';
function Fpopendir(dirname : pchar): pdir; external name 'FPC_SYSC_OPENDIR';
function Fpclosedir(var dirp : dir): cint; external name 'FPC_SYSC_CLOSEDIR';
function Fpreaddir(var dirp : dir) : pdirent; external name 'FPC_SYSC_READDIR';
procedure Fpexit(status : cint); external name 'FPC_SYSC_EXIT';
function Fpsigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint;  external name 'FPC_SYSC_SIGACTION';
function Fpftruncate(fd : cint; flength : off_t): cint; external name 'FPC_SYSC_FTRUNCATE';
function Fpfstat(fd : cint; var sb : stat): cint; external name 'FPC_SYSC_FSTAT';
function Fpfork : pid_t; external name 'FPC_SYSC_FORK';
// function Fpexecve(path : pchar; argv : ppchar;envp: ppchar): cint; external name 'FPC_SYSC_EXECVE';
function Fpwaitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; external name 'FPC_SYSC_WAITPID';
function Fpaccess(pathname : pchar; amode : cint): cint;external name 'FPC_SYSC_ACCESS';
function FpDup(fildes:cint):cint;  external name 'FPC_SYSC_DUP';
function FpDup2(fildes:cint;fildes2:cint):cint; external name 'FPC_SYSC_DUP2';
function geterrno:cint; external name  'FPC_SYS_GETERRNO';
procedure seterrno (i:cint); external name  'FPC_SYS_SETERRNO';

{$endif}

{$I bunxfunc.inc}

{
 $Log$
 Revision 1.1  2003-01-05 19:01:28  marco
  * FreeBSD compiles now with baseunix mods.

 Revision 1.8  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.7  2002/10/27 11:58:30  marco
  * Modifications from Saturday.

 Revision 1.6  2002/10/26 18:27:51  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.5  2002/09/07 16:01:17  peter
   * old logs removed and tabs fixed

 Revision 1.4  2002/08/21 07:03:16  marco
  * Fixes from Tuesday.

 Revision 1.3  2002/08/19 12:29:11  marco
  * First working POSIX *BSD system unit.



}
