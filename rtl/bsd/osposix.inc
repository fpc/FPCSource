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
    function sys_execve(path : pchar; argv : ppchar; envp: ppchar): cint; cdecl; external name 'execve';
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; cdecl; external name 'waitpid';
    function sys_access(const pathname : pchar; amode : cint): cint; cdecl; external name 'access';
    function sys_uname(var name: utsname): cint; cdecl; external name 'uname';
    function sys_Dup(fildes:cint):cint; cdecl; external name 'dup';
    function sys_Dup2(fildes:cint;fildes2:cint):cint; cdecl; external name 'dup2';

{$else}

// uses syscalls.

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
// function sys_execve(path : pchar; argv : ppchar;envp: ppchar): cint; external name 'FPC_SYSC_EXECVE';
function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t; external name 'FPC_SYSC_WAITPID';
function sys_access(const pathname : pchar; amode : cint): cint;external name 'FPC_SYSC_ACCESS';
function sys_Dup(fildes:cint):cint;  external name 'FPC_SYSC_DUP';
function sys_Dup2(fildes:cint;fildes2:cint):cint; external name 'FPC_SYSC_DUP2';
function geterrno:cint; external name  'FPC_SYS_GETERRNO';
procedure seterrno (i:cint); external name  'FPC_SYS_SETERRNO';

{$endif}

{$I bsdfuncs.inc}

{
 $Log$
 Revision 1.8  2002-10-27 17:21:29  marco
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
