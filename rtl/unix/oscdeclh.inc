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

    function  FpAccess  (pathname : pchar; amode : cint): cint; cdecl; external name 'access';
    function  FpChdir	(path : pchar): cint; cdecl; external name 'chdir';
    function  FpClose	(fd : cint): cint; cdecl; external name 'close';
    function  FpClosedir(var dirp : dir): cint; cdecl; external name 'closedir';
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
    function  FpReaddir	(var dirp : dir) : pdirent;cdecl; external name 'readdir';
    function  FpRename	(old : pchar; newpath: pchar): cint; cdecl;external name 'rename';
    function  FpRmdir	(path : pchar): cint; cdecl; external name 'rmdir';
    function  FpSigaction(sig: cint; act : psigactionrec; oact : psigactionrec): cint; cdecl; external name 'sigaction';
    function  FpStat	(path: pchar; var buf : stat): cint; cdecl; external name 'stat';
    function  FpTime	(tloc:ptime_t): time_t; cdecl; external name 'time';
    function  FpUname	(var name: utsname): cint; cdecl; external name 'uname';
    function  FpUnlink	(path: pchar): cint; cdecl; external name 'unlink';
    function  FpWaitpid	(pid : TPid; stat_loc : pcint; options: cint): TPid; cdecl; external name 'waitpid';
    function  FpWrite	(fd: cint;buf:pchar; nbytes : TSize): TSSize; cdecl; external name 'write';
    procedure FpExit	(status : cint); cdecl; external name '_exit';

{
   $Log$
   Revision 1.1  2003-06-01 15:23:46  marco
    * Shared for now

   Revision 1.1  2002/12/18 16:43:26  marco
    * new unix rtl, linux part.....


}