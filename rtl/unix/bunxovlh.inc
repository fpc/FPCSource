{
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

Function  FpLink (existing : AnsiString; newone : AnsiString): cInt; inline;
Function  FpMkfifo  (path : AnsiString; Mode : TMode): cInt; inline;
Function  FpChmod   (path : AnsiString; Mode : TMode): cInt; inline;
Function  FpChown   (path : AnsiString; owner : TUid; group : TGid): cInt; inline;
Function  FpUtime   (path : AnsiString; times : putimbuf): cInt; inline;
Function  FpGetcwd : AnsiString;
Function  FpExecve  (path : AnsiString; argv : ppchar; envp: ppchar): cInt; inline;
Function  FpExecv   (path : AnsiString; argv : ppchar): cInt; inline;
Function  FpOpendir (dirname : AnsiString): pDir; inline;
Function  FpOpendir (dirname : shortString): pDir; inline;
Function  FpOpen    (path : pChar; flags : cInt):cInt; inline;
Function  FpOpen    (path : AnsiString; flags : cInt):cInt; inline;
Function  FpOpen    (path : AnsiString; flags : cInt; Mode: TMode):cInt; inline;
Function  FpOpen    (path : String; flags : cInt):cInt;
Function  FpOpen    (path : String; flags : cInt; Mode: TMode):cInt;
Function  FpChdir   (path : AnsiString): cInt; inline;
Function  FpMkdir   (path : AnsiString; Mode: TMode):cInt; inline;
Function  FpUnlink  (path : AnsiString): cInt; inline;
Function  FpRmdir   (path : AnsiString): cInt; inline;
Function  FpRename  (old  : AnsiString;newpath: AnsiString): cInt; inline;
Function  FpStat    (path: AnsiString; var buf : stat): cInt; inline;
Function  FpStat    (path: String; var buf : stat): cInt;
Function  FpAccess  (pathname : AnsiString; aMode : cInt): cInt; inline;
function  FpWaitPid (pid : TPid; Var Status : cInt; Options : cint) : TPid;

Function  FPFStat   (var F:Text;Var Info:stat):Boolean; inline;
Function  FPFStat   (var F:File;Var Info:stat):Boolean; inline;

// added. Is a depreciated POSIX function that can be considered alias to sigaction

Function  FpSignal  (signum:longint;Handler:signalhandler):signalhandler;
Function  FpRead    (fd : cInt; var buf; nbytes : TSize): TSsize; inline;
Function  FpWrite   (fd : cInt; const buf; nbytes : TSize): TSsize; inline;
{$ifdef linux}
function  FppRead   (fd : cInt; var buf; nbytes : TSize; offset:Toff): TSsize; inline;
function  FppWrite  (fd : cInt; const buf; nbytes : TSize; offset:Toff): TSsize; inline;
{$endif}
Function  FpDup     (var oldfile,newfile:text):cint;
Function  FpDup     (var oldfile,newfile:file):cint;
Function  FpDup2    (var oldfile,newfile:text):cint;
Function  FpDup2    (var oldfile,newfile:file):cint;
function  fptime    :time_t; inline;


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

Function  wifexited    (Status : cInt): boolean;
Function  wexitStatus  (Status : cInt): cInt;
Function  wstopsig     (Status : cInt): cInt;
Function  wifsignaled  (Status : cInt): boolean;
Function  wtermsig     (Status : cInt): cInt;

Function fpReadLink(Name:ansistring):ansistring;


