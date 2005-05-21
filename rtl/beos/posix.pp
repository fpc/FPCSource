{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Carl Eric Codere
    development team

    POSIX Compliant interface unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit posix;

interface

{***********************************************************************}
{                       POSIX PUBLIC INTERFACE                          }
{***********************************************************************}


{$i errno.inc}
{$i osposixh.inc}


    function sys_fork : pid_t;
    function sys_execve(const path : pchar; const argv : ppchar; const envp: ppchar): cint;
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t;
    procedure sys_exit(status : cint);
    { get system specific information }
    function sys_uname(var name: utsname): cint;
    function sys_opendir(const dirname : pchar): pdir;
    function sys_readdir(dirp : pdir) : pdirent;
    function sys_closedir(dirp : pdir): cint;
    function sys_chdir(const path : pchar): cint;
    function sys_open(const path: pchar; flags : cint; mode: mode_t):cint;
    function sys_mkdir(const path : pchar; mode: mode_t):cint;
    function sys_unlink(const path: pchar): cint;
    function sys_rmdir(const path : pchar): cint;
    function sys_rename(const old : pchar; const newpath: pchar): cint;
    function sys_fstat(fd : cint; var sb : stat): cint;
    function sys_stat(const path: pchar; var buf : stat): cint;
    function sys_access(const pathname : pchar; amode : cint): cint;
    function sys_close(fd : cint): cint;
    function sys_read(fd: cint; buf: pchar; nbytes : size_t): ssize_t;
    function sys_write(fd: cint;const buf:pchar; nbytes : size_t): ssize_t;
    function sys_lseek(fd : cint; offset : off_t; whence : cint): off_t;
    function sys_time(var tloc:time_t): time_t;


    function sys_sigaction(sig: cint; var act : sigactionrec; var oact : sigactionrec): cint;
    function sys_ftruncate(fd : cint; flength : off_t): cint;

    function S_ISDIR(m : mode_t): boolean;
    function S_ISCHR(m : mode_t): boolean;
    function S_ISBLK(m : mode_t): boolean;
    function S_ISREG(m : mode_t): boolean;
    function S_ISFIFO(m : mode_t): boolean;

    function wifexited(status : cint): cint;
    function wexitstatus(status : cint): cint;
    function wstopsig(status : cint): cint;
    function wifsignaled(status : cint): cint;




implementation

{$i osposix.inc}




end.
