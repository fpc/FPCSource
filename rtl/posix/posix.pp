{
    $Id$
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

type tgrparr = array[0..0] of gid_t;		{ C style array workarounds}
     tfildes = array[0..1] of cint;

    function sys_sigemptyset(var _set : sigset_t): cint;
    function sys_sigfillset(var _set : sigset_t): cint;
    function sys_sigaddset(var _set : sigset_t; signo : cint): cint;
    function sys_sigdelset(var _set : sigset_t; signo : cint): cint;
    function sys_sigismember(const _set : sigset_t; signo : cint): cint;
    function sys_sigprocmask(how : cint; var _set : sigset_t; var _oset : sigset_t): cint;
    function sys_sigpending(var _set : sigset_t): cint;
    function sys_sigsuspend(const sigmask : sigset_t): cint;

    function sys_umask(cmask : mode_t): mode_t;
    function sys_link(existing : pchar; _new : pchar): cint;
    function sys_mkfifo(path : pchar; mode : mode_t): cint;
    function sys_chmod(path : pchar; mode : mode_t): cint;
    function sys_chown(path: pchar; owner : uid_t; group : gid_t): cint;
    function sys_utime(path : pchar; times : putimbuf): cint;
    function sys_pipe(var fildes : tfildes):cint;
    function sys_dup(fildes : cint): cint;
    function sys_dup2(fildes, fildes2 : cint): cint;
    function sys_times(var buffer : tms): clock_t;

    function sys_alarm(seconds : cuint): cuint;
    function sys_pause : cint;
    function sys_sleep(seconds : cuint): cuint;
 
    function sys_getpid : pid_t;
    function sys_getppid : pid_t;
    function sys_getuid : uid_t;
    function sys_geteuid : uid_t;
    function sys_getgid : gid_t;
    function sys_getegid : gid_t;
    function sys_setuid(uid : uid_t): cint;
    function sys_setgid(gid : gid_t): cint;
    function sys_getgroups(gidsetsize : cint; var grouplist : tgrparr): cint;
    function sys_getpgrp : pid_t;
    function sys_setsid : pid_t;
    function sys_fcntl(fildes : cint; cmd : cint): cint;
    function sys_fcntl(fildes : cint; cmd : cint; arg : cint): cint;
    function sys_fcntl(fildes : cint; cmd : cint; var arg : flock): cint;

    function sys_getcwd(pt:pchar; _size:size_t):pchar;
    function sys_fork : pid_t;
    function sys_execve(path : pchar; argv : ppchar; envp: ppchar): cint;
    function sys_execv(path : pchar; argv : ppchar): cint;
    function sys_waitpid(pid : pid_t; var stat_loc : cint; options: cint): pid_t;
    function sys_wait(var stat_loc : cint): pid_t;
    procedure sys_exit(status : cint);
    function sys_kill(pid : pid_t; sig: cint): cint;
    { get system specific information }
    function sys_uname(var name: utsname): cint;
    function sys_opendir(const dirname : pchar): pdir;
    function sys_readdir(var dirp : dir) : pdirent;
    function sys_closedir(var dirp : dir): cint;
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
    function sys_ftruncate(fd : cint; flength : off_t): cint;

    function sys_getenv(const name : pchar): pchar;
    function sys_getenv(name : string): pchar;

    function S_ISDIR(m : mode_t): boolean;
    function S_ISCHR(m : mode_t): boolean;
    function S_ISBLK(m : mode_t): boolean;
    function S_ISREG(m : mode_t): boolean;
    function S_ISFIFO(m : mode_t): boolean;

    function wifexited(status : cint): cint;
    function wexitstatus(status : cint): cint;
    function wstopsig(status : cint): cint;
    function wifsignaled(status : cint): cint;
    function wtermsig(status : cint): cint;
    
implementation

{$i osposix.inc}

end.

{
  $Log$
  Revision 1.2  2002-11-14 12:17:28  marco
   * for now.

  Revision 1.2  2002/08/10 13:42:36  marco
   * Fixes Posix dir copied to devel branch

  Revision 1.1.2.2  2001/12/09 03:31:56  carl
  + wifsignaled() added

  Revision 1.1.2.1  2001/12/04 02:29:59  carl
  + posix unit template file

  Revision 1.1.2.1  2001/08/15 01:06:32  carl
  + first version of posix unit

}