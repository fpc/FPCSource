{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Functions that are not part of the libc<->syscall decision.
    - string and file type overloads
    - functions that are macro's in C.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function  FpLink (existing : AnsiString; newone : AnsiString): cInt;
Function  FpMkfifo  (path : AnsiString; Mode : TMode): cInt;
Function  FpChmod   (path : AnsiString; Mode : TMode): cInt;
Function  FpChown   (path : AnsiString; owner : TUid; group : TGid): cInt;
Function  FpUtime   (path : AnsiString; times : putimbuf): cInt;
Function  FpGetcwd  (path:AnsiString; siz:TSize):AnsiString;
Function  FpExecve  (path : AnsiString; argv : ppchar; envp: ppchar): cInt;
Function  FpExecv   (path : AnsiString; argv : ppchar): cInt;
Function  FpOpendir (dirname : AnsiString): pDir;
Function  FpOpendir (dirname : shortString): pDir;
Function  FpOpen    (path : pChar; flags : cInt):cInt;
Function  FpOpen    (path : AnsiString; flags : cInt):cInt;
Function  FpOpen    (path : AnsiString; flags : cInt; Mode: TMode):cInt;
Function  FpOpen    (path : String; flags : cInt):cInt;
Function  FpOpen    (path : String; flags : cInt; Mode: TMode):cInt;
Function  FpChdir   (path : AnsiString): cInt;
Function  FpMkdir   (path : AnsiString; Mode: TMode):cInt;
Function  FpUnlink  (path : AnsiString): cInt;
Function  FpRmdir   (path : AnsiString): cInt;
Function  FpRename  (old  : AnsiString;newpath: AnsiString): cInt;
Function  FpStat    (path: AnsiString; var buf : stat): cInt;
Function  FpStat    (path: String; var buf : stat): cInt;
Function  FpAccess  (pathname : AnsiString; aMode : cInt): cInt; 

Function  FPFStat   (var F:Text;Var Info:stat):Boolean;
Function  FPFStat   (var F:File;Var Info:stat):Boolean;

// added. Is a depreciated POSIX function that can be considered alias to sigaction

Function  FpSignal  (signum:longint;Handler:signalhandler):signalhandler;
Function  FpRead    (fd : cInt; var buf; nbytes : TSize): TSsize;
Function  FpWrite   (fd : cInt; const buf; nbytes : TSize): TSsize;
Function  FpDup	    (var oldfile,newfile:text):cint;
Function  FpDup     (var oldfile,newfile:file):cint;
Function  FpDup2    (var oldfile,newfile:text):cint;
Function  FpDup2    (var oldfile,newfile:file):cint;
function  fptime    :time_t;


Function fpSelect   (N:cint;readfds,writefds,exceptfds:pfdset;TimeOut:cint):cint;
Function fpSelect   (var T:Text;TimeOut :PTimeval):cint;
Function fpSelect   (var T:Text;TimeOut :time_t):cint;
Function FpGetEnv   (name : String): pChar;

// macro's

Function fpFD_SET   (fdno:cint;var nset : TFDSet): cint;
Function fpFD_CLR   (fdno:cint;var nset : TFDSet): cint;
Function fpFD_ZERO  (var nset : TFDSet):cint;
Function fpFD_ISSET (fdno:cint;const nset : TFDSet): cint;
Function fpfdfillset(var nset : TFDSet):cint;

Function FpsigEmptySet(var nset : TSigSet): cint;
Function FpSigFillSet (var nset : TSigSet): cInt;
Function FpSigAddSet  (var nset : TSigSet; signo : cInt): cInt;
Function FpSigDelSet  (var nset : TSigSet; signo : cInt): cInt;
Function FpSigIsMember(Const nset : TSigSet; signo : cInt): cInt;
Function  fpS_ISDIR    (m : TMode): Boolean;
Function  fpS_ISCHR    (m : TMode): Boolean;
Function  fpS_ISBLK    (m : TMode): Boolean;
Function  fpS_ISREG    (m : TMode): Boolean;
Function  fpS_ISFIFO   (m : TMode): Boolean;

// The following two are very common, but not POSIX.
Function  fpS_ISLNK       (m:TMode) : Boolean;
Function  fpS_ISSOCK      (m:TMode) : Boolean;

Function  wifexited    (Status : cInt): cInt;
Function  wexitStatus  (Status : cInt): cInt;
Function  wstopsig     (Status : cInt): cInt;
Function  wifsignaled  (Status : cInt): cInt;
Function  wtermsig     (Status : cInt): cInt;

Function fpReadLink(Name:ansistring):ansistring;
{
 $Log$
 Revision 1.5  2004-11-14 12:21:08  marco
  * moved some calls from unix to baseunix. Darwin untested.

 Revision 1.4  2003/12/30 12:24:01  marco
  * FPC_USE_LIBC

 Revision 1.3  2003/09/14 20:15:01  marco
  * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

 Revision 1.2  2003/06/01 16:28:41  marco
  * Enhancements to make the compiler baseunix using.

 Revision 1.1  2002/12/18 16:49:02  marco
  * New RTL. Linux system unit and baseunix operational.


}

 