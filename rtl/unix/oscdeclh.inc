{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file should become an alternative to the syscalls in due time,
    to import the base calls from libc.
    Be very careful though. Kernel types and libc types are often not the
    same on Linux.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 ***********************************************************************}

{   var
     Errno : cint; external clib name 'errno';}

Type TGrpArr = Array [0..0] of TGid;            { C style array workarounds}
     pGrpArr = ^TGrpArr;
     TFilDes = Array [0..1] of cInt;
     pFilDes = ^TFilDes;

    function  FpAccess  (pathname : pchar; amode : cint): cint; cdecl; external clib name 'access';
    function  FpChdir	(path : pchar): cint; cdecl; external clib name 'chdir';
    function  FpClose	(fd : cint): cint; cdecl; external clib name 'close';
    Function  FpClosedir (var dirp : Dir): cInt; cdecl; external clib name 'closedir';
    function  FpClosedir (dirp : pdir): cint; cdecl; external clib name 'closedir';
    function  FpDup	(oldd:cint):cint; cdecl; external clib name 'dup';
    function  FpDup2	(oldd:cint;newd:cint):cint; cdecl; external clib name 'dup2';
    function  FpExecve	(path : pchar; argv : ppchar; envp: ppchar): cint; cdecl; external clib name 'execve';
    function  FpFork  : TPid; cdecl; external clib name 'fork';
    function  FpFstat	(fd : cint; var sb : stat): cint; cdecl; external clib name 'fstat';
    function  FpFtruncate(fd : cint; flength : TOff): cint; cdecl; external clib name 'ftruncate';
    function  FpLseek	(fd : cint; offset : TOff; whence : cint): TOff; cdecl; external clib name 'lseek';
    function  FpMkdir	(path : pchar; mode: TMode):cint; cdecl; external clib name 'mkdir';
    function  FpOpen	(path: pchar; flags : cint; mode: TMode):cint; cdecl; external clib name 'open';
    function  FpOpendir	(dirname : pchar): pdir; cdecl; external clib name 'opendir';
    function  FpRead	(fd: cint; buf: pchar; nbytes : TSize): TSSize; cdecl; external clib name 'read';
    function  FpReaddir	(dirp : pdir) : pdirent;cdecl; external clib name 'readdir';
    Function  FpReaddir    (var dirp : Dir) : pDirent;cdecl; external clib name 'readdir';
    function  FpRename	(old : pchar; newpath: pchar): cint; cdecl;external clib name 'rename';
    function  FpRmdir	(path : pchar): cint; cdecl; external clib name 'rmdir';
    function  FpSigaction (sig: cint;var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external clib name 'sigaction';
    Function  FPSigaction (sig: cInt; act :pSigActionRec;oact:pSigActionRec):cint;cdecl; external clib name 'sigaction';
    Function  FpChmod      (path : pChar; Mode : TMode): cInt; cdecl; external clib name 'chmod';
    Function  FPUtime(path:pchar;times:putimbuf):cint; cdecl; external clib name 'utime';

    {$ifdef BSD}
    function FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint;cdecl; external clib name 'sigprocmask';
    {$endif}
    function  FpStat	  (path: pchar; var buf : stat): cint; cdecl; external clib name 'stat';
//    {$ifdef FPC_IS_SYSTEM}
    function  FpTime	   (tloc:ptime_t): time_t; cdecl; external clib name 'time';
//    {$else}
    Function  FpTime       (var tloc : TTime): TTime; cdecl; external clib name 'time';
//    {$endif}
    function  FpUname	(var name: utsname): cint; cdecl; external clib name 'uname';
    function  FpUnlink	(path: pchar): cint; cdecl; external clib name 'unlink';
    function  FpWaitpid	(pid : TPid; stat_loc : pcint; options: cint): TPid; cdecl; external clib name 'waitpid';
    function  FpWrite	(fd: cint;buf:pchar; nbytes : TSize): TSSize; cdecl; external clib name 'write';
    procedure FpExit	(status : cint); cdecl; external clib name '_exit';
    function  fpmmap	(addr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;ofs:off_t):pointer; cdecl; external clib name 'mmap';
    function  FpIOCtl   (Handle:cint;Ndx: culong;Data: Pointer):cint; cdecl; external clib name 'ioctl';
    Function  FPSelect  (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint; cdecl; external clib name 'select';
    function  fpgetenv  (name : pchar):pchar; cdecl; external clib name 'getenv';
    Function  FpKill       (pid : TPid; sig: cInt): cInt; cdecl; external clib name 'kill';
    Function  FpGetpid  : TPid; cdecl;external clib name 'getpid';
    Function  FpGetppid : TPid;cdecl;external clib name 'getppid';
    Function  FpGetuid  : TUid;cdecl;external clib name 'getuid';
    Function  FpGeteuid : TUid;cdecl;external clib name 'geteuid';
    Function  FpGetgid  : TGid;cdecl;external clib name 'getgid';
    Function  FpGetegid : TGid;cdecl;external clib name 'getegid';
    Function  FpSetuid     (uid : TUid): cInt;cdecl;external clib name 'setuid';
    Function  FpSetgid     (gid : TGid): cInt;cdecl;external clib name 'setgid';
    Function  FpGetgroups  (gidsetsize : cInt; var grouplist : tgrparr):cint; cdecl;external clib name 'getgroups';
    Function  FpGetpgrp : TPid;  cdecl;external clib name 'getpgrp';
    Function  FpSetsid  : TPid; cdecl;external clib name 'setsid';
    Function  FpPipe       (var fildes : tfildes):cInt; cdecl;external clib name 'pipe';
    Function  FpFcntl      (fildes : cInt; cmd : cInt): cInt; cdecl; external clib name 'fcntl';   
    Function  FpFcntl      (fildes : cInt; cmd : cInt; arg :cInt): cInt; cdecl; external clib name 'fcntl';
    Function  FpFcntl      (fildes : cInt; cmd : cInt; var arg : flock): cInt; cdecl external clib name 'fcntl';

{
   $Log$
   Revision 1.7  2004-01-12 23:43:22  marco
    * some semi colons added

   Revision 1.6  2004/01/04 21:05:01  jonas
     * declare C-library routines as external in libc so we generate proper
       import entries for Darwin

   Revision 1.5  2004/01/01 16:10:23  marco
    * fpreadlink(pathstr) moved to unxovl (since not platform specific),
   	small fixes for "make all OPT='-dFPC_USE_LIBC'

   Revision 1.4  2004/01/01 14:09:14  marco
    * FPC_USE_LIBC fixes

   Revision 1.3  2003/12/30 15:43:20  marco
    * linux now compiles with FPC_USE_LIBC

   Revision 1.2  2003/12/30 12:24:01  marco
    * FPC_USE_LIBC

   Revision 1.1  2003/06/01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....

}
