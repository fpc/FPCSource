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
     Errno : cint; external name 'errno';}

Type TGrpArr = Array [0..0] of TGid;            { C style array workarounds}
     pGrpArr = ^TGrpArr;
     TFilDes = Array [0..1] of cInt;
     pFilDes = ^TFilDes;

    function  FpAccess  (pathname : pchar; amode : cint): cint; cdecl; external name 'access';
    function  FpChdir	(path : pchar): cint; cdecl; external name 'chdir';
    function  FpClose	(fd : cint): cint; cdecl; external name 'close';
//{$ifndef FPC_IS_SYSTEM}
    Function  FpClosedir (var dirp : Dir): cInt; cdecl; external name 'closedir';
//{$else}
    function  FpClosedir (dirp : pdir): cint; cdecl; external name 'closedir';
//{$endif}
    function  FpDup	(oldd:cint):cint; cdecl; external name 'dup';
    function  FpDup2	(oldd:cint;newd:cint):cint; cdecl; external name 'dup2';
    function  FpExecve	(path : pchar; argv : ppchar; envp: ppchar): cint; cdecl; external name 'execve';
    function  FpFork  : TPid; cdecl; external name 'fork';
    function  FpFstat	(fd : cint; var sb : stat): cint; cdecl; external name 'fstat';
    function  FpFtruncate(fd : cint; flength : TOff): cint; cdecl; external name 'ftruncate';
    function  FpLseek	(fd : cint; offset : TOff; whence : cint): TOff; cdecl; external name 'lseek';
    function  FpMkdir	(path : pchar; mode: TMode):cint; cdecl; external name 'mkdir';
    function  FpOpen	(path: pchar; flags : cint; mode: TMode):cint; cdecl; external name 'open';
    function  FpOpendir	(dirname : pchar): pdir; cdecl; external name 'opendir';
    function  FpRead	(fd: cint; buf: pchar; nbytes : TSize): TSSize; cdecl; external name 'read';
//{$ifdef FPC_IS_SYSTEM}
    function  FpReaddir	(dirp : pdir) : pdirent;cdecl; external name 'readdir';
//{$ELSE}
    Function  FpReaddir    (var dirp : Dir) : pDirent;cdecl; external name 'readdir';
//{$endif}
    function  FpRename	(old : pchar; newpath: pchar): cint; cdecl;external name 'rename';
    function  FpRmdir	(path : pchar): cint; cdecl; external name 'rmdir';
//    {$ifdef FPC_IS_SYSTEM}
    function  FpSigaction (sig: cint;var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external name 'sigaction';
//    {$else}
    Function  FPSigaction (sig: cInt; act :pSigActionRec;oact:pSigActionRec):cint;cdecl; external name 'sigaction';
//    {$endif}

    {$ifdef BSD}
    function FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint; external name 'sigprocmask';
    {$endif}
    function  FpStat	  (path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    {$ifdef FPC_IS_SYSTEM}
    function  FpTime	   (tloc:ptime_t): time_t; cdecl; external name 'time';
    {$else}
    Function  FpTime       (var tloc : TTime): TTime; cdecl; external name 'time';
    {$endif}
    function  FpUname	(var name: utsname): cint; cdecl; external name 'uname';
    function  FpUnlink	(path: pchar): cint; cdecl; external name 'unlink';
    function  FpWaitpid	(pid : TPid; stat_loc : pcint; options: cint): TPid; cdecl; external name 'waitpid';
    function  FpWrite	(fd: cint;buf:pchar; nbytes : TSize): TSSize; cdecl; external name 'write';
    procedure FpExit	(status : cint); cdecl; external name '_exit';
    function  fpmmap	(addr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;ofs:off_t):pointer; cdecl; external name 'mmap';
    function  FpIOCtl   (Handle:cint;Ndx: culong;Data: Pointer):cint; cdecl; external name 'ioctl';
    Function  FPSelect  (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint; cdecl; external name 'select';
    function  fpgetenv  (name : pchar):pchar; cdecl; external name 'getenv';
    Function  FpKill       (pid : TPid; sig: cInt): cInt; cdecl; external name 'kill';
    Function  FpGetpid  : TPid; cdecl;external name 'getpid';
    Function  FpGetppid : TPid;cdecl;external name 'getppid';
    Function  FpGetuid  : TUid;cdecl;external name 'getuid';
    Function  FpGeteuid : TUid;cdecl;external name 'geteuid';
    Function  FpGetgid  : TGid;cdecl;external name 'getgid';
    Function  FpGetegid : TGid;cdecl;external name 'getegid';
    Function  FpSetuid     (uid : TUid): cInt;cdecl;external name 'setuid';
    Function  FpSetgid     (gid : TGid): cInt;cdecl;external name 'setgid';
    Function  FpGetgroups (gidsetsize : cInt; var grouplist : tgrparr):cint; cdecl;external name 'getgroups';
    Function  FpGetpgrp : TPid;  cdecl;external name 'getpgrp';
    Function  FpSetsid  : TPid; cdecl;external name 'setsid';
    Function  FpPipe       (var fildes : tfildes):cInt; cdecl;external name 'pipe';

{
   $Log$
   Revision 1.3  2003-12-30 15:43:20  marco
    * linux now compiles with FPC_USE_LIBC

   Revision 1.2  2003/12/30 12:24:01  marco
    * FPC_USE_LIBC

   Revision 1.1  2003/06/01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....


}