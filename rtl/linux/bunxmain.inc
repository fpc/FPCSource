{
    $Id$
    Copyright (c) 2002 by Marco van de Voort.

    Implementation of the baseunix unit for *BSD^H^H^Hlinux. In practice only
    includes other files, or specifies libc bindings.

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

{ $I ostypes.inc}
{ $I posmacro.inc}

{$ifdef uselibc}
 {$I oscdecl.inc}
{$else}

function fptime		(var tloc:time_t): time_t; external name 'FPC_SYSC_TIME';
function fpopen		(path: pchar; flags : cint; mode: TMode):cint;  external name 'FPC_SYSC_OPEN';
function fpclose	(fd : cint): cint;  external name 'FPC_SYSC_CLOSE';
function fplseek	(fd : cint; offset : TOff; whence : cint): TOff; external name 'FPC_SYSC_LSEEK';
function fpread		(fd: cint; buf: pchar; nbytes : TSize): TSSize; external name 'FPC_SYSC_READ';
function fpwrite	(fd: cint; buf:pchar; nbytes : TSize): TSSize;  external name 'FPC_SYSC_WRITE';
function fpunlink	(path: pchar): cint;  external name 'FPC_SYSC_UNLINK';
function fprename	(old : pchar; newpath: pchar): cint;  external name 'FPC_SYSC_RENAME';
function fpstat		(path: pchar; var buf : stat):cint;  external name 'FPC_SYSC_STAT';
function fpchdir	(path : pchar): cint; external name 'FPC_SYSC_CHDIR';
function fpmkdir	(path : pchar; mode: TMode):cint; external name 'FPC_SYSC_MKDIR';
function fprmdir	(path : pchar): cint; external name 'FPC_SYSC_RMDIR';
function fpopendir	(dirname : pchar): pdir; external name 'FPC_SYSC_OPENDIR';
function fpclosedir	(var dirp : dir): cint; external name 'FPC_SYSC_CLOSEDIR';
function fpreaddir	(var dirp : dir) : pdirent; external name 'FPC_SYSC_READDIR';
procedure fpexit	(status : cint); external name 'FPC_SYSC_EXIT';
function fpsigaction	(sig: cint; act : psigactionrec; oact : psigactionrec): cint;  external name 'FPC_SYSC_SIGACTION';
function fpftruncate	(fd : cint; flength : TOff): cint; external name 'FPC_SYSC_FTRUNCATE';
function fpfstat	(fd : cint; var sb : stat): cint; external name 'FPC_SYSC_FSTAT';
function fpfork       : pid_t; external name 'FPC_SYSC_FORK';
// function fpexecve	(path : pchar; argv : ppchar;envp: ppchar): cint; external name 'FPC_SYSC_EXECVE';
function fpwaitpid	(pid : pid_t; var stat_loc : cint; options: cint): pid_t; external name 'FPC_SYSC_WAITPID';
function fpaccess	(pathname : pchar; amode : cint): cint;external name 'FPC_SYSC_ACCESS';
function fpDup		(fildes:cint):cint;  external name 'FPC_SYSC_DUP';
function fpDup2		(fildes:cint;fildes2:cint):cint; external name 'FPC_SYSC_DUP2';
function geterrno     : cint; external name  'FPC_SYS_GETERRNO';
procedure seterrno 	(i:cint); external name  'FPC_SYS_SETERRNO';

{$endif}

{$I bunxfunc.inc}
{$I genfuncs.inc}

{
 $Log$
 Revision 1.1  2002-12-18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.1  2002/11/12 14:37:59  marco
  * Parts of new unix rtl

}
