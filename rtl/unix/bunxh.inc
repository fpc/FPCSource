{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    The interface part of the baseunix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Type TGrpArr = Array [0..0] of TGid;            { C style array workarounds}
     pGrpArr = ^TGrpArr;
     TFilDes = Array [0..1] of cInt;
     pFilDes = ^TFilDes;

// if you are looking for macro definitions or non C template overloaded versions, they are moved to bunxovlh.inc

    Function  FpSigProcMask(how : cInt; nset : pSigSet; oset : pSigSet): cInt; external name 'FPC_SYSC_SIGPROCMASK';
    Function  FpSigProcMask(how : cInt; Const nset : TSigSet; var oset : TSigSet): cInt; external name 'FPC_SYSC_SIGPROCMASK';
    Function  FpSigPending (var nset : TSigSet): cInt;
    Function  FpSigSuspend (Const sigmask : TSigSet): cInt;

    Function  FpUmask      (cmask : TMode): TMode;
    Function  FpLink       (existing : pChar; newone : pChar): cInt;
    Function  FpMkfifo     (path : pChar; Mode : TMode): cInt;
    Function  FpChmod      (path : pChar; Mode : TMode): cInt;
    Function  FpChown      (path : pChar; owner : TUid; group : TGid): cInt;
    Function  FpUtime      (path : pChar; times : putimbuf): cInt;
    Function  FpPipe       (var fildes : tfildes):cInt;
    Function  FpDup        (fildes : cInt): cInt;  external name 'FPC_SYSC_DUP';
    Function  FpDup2       (fildes, fildes2 : cInt): cInt; external name 'FPC_SYSC_DUP2';
    Function  FpTimes      (var buffer : tms): TClock;

    Function  FpAlarm      (seconds : cuint): cuint;
    Function  FpPause : cInt;
    Function  FpSleep      (seconds : cuint): cuint;

    Function  FpGetpid  : TPid; external name  'FPC_SYSC_GETPID';
    Function  FpGetppid : TPid;
    Function  FpGetuid  : TUid;
    Function  FpGeteuid : TUid;
    Function  FpGetgid  : TGid;
    Function  FpGetegid : TGid;
    Function  FpSetuid     (uid : TUid): cInt;
    Function  FpSetgid     (gid : TGid): cInt;
    Function  FpGetgroups (gidsetsize : cInt; var grouplist : tgrparr): cInt;
    Function  FpGetpgrp : TPid;
    Function  FpSetsid  : TPid;
    Function  FpFcntl      (fildes : cInt; cmd : cInt): cInt;
    Function  FpFcntl      (fildes : cInt; cmd : cInt; arg : cInt): cInt;
    Function  FpFcntl      (fildes : cInt; cmd : cInt; var arg : flock): cInt;

    Function  FpGetcwd     (path:pChar; siz:TSize):pChar;  external name 'FPC_SYSC_GETCWD';
    Function  FpFork : TPid; external name 'FPC_SYSC_FORK';
    Function  FpExecve     (path : pChar; argv : ppChar; envp: ppChar): cInt;
    Function  FpExecv      (path : pChar; argv : ppChar): cInt;
    Function  FpWaitpid    (pid : TPid; stat_loc : pcInt; options: cInt): TPid; external name 'FPC_SYSC_WAITPID';
    Function  FpWait       (var stat_loc : cInt): TPid;
    Procedure FpExit       (Status : cInt);  external name 'FPC_SYSC_EXIT';
    Function  FpKill       (pid : TPid; sig: cInt): cInt;
    Function  FpUname      (var name: utsname): cInt;
    Function  FpOpendir    (dirname : pChar): pDir;  external name 'FPC_SYSC_OPENDIR';
    Function  FpReaddir    (var dirp : Dir) : pDirent; external name 'FPC_SYSC_READDIR';
    Function  FpClosedir   (var dirp : Dir): cInt; external name 'FPC_SYSC_CLOSEDIR';
    Function  FpChdir      (path : pChar): cInt;  external name 'FPC_SYSC_CHDIR';
    Function  FpOpen       (path : pChar; flags : cInt; Mode: TMode):cInt; external name 'FPC_SYSC_OPEN';
    Function  FpMkdir      (path : pChar; Mode: TMode):cInt;  external name 'FPC_SYSC_MKDIR';
    Function  FpUnlink     (path : pChar): cInt;  external name 'FPC_SYSC_UNLINK';
    Function  FpRmdir      (path : pChar): cInt; external name 'FPC_SYSC_RMDIR';
    Function  FpRename     (old  : pChar; newpath: pChar): cInt;   external name 'FPC_SYSC_RENAME';
    Function  FpFStat      (fd : cInt; var sb : stat): cInt; external name 'FPC_SYSC_FSTAT';
    Function  FpStat       (path: pChar; var buf : stat): cInt;  external name 'FPC_SYSC_STAT';
    Function  FpAccess     (pathname : pChar; aMode : cInt): cInt; external name 'FPC_SYSC_ACCESS';
    Function  FpClose      (fd : cInt): cInt;  external name 'FPC_SYSC_CLOSE';

    Function  FpRead       (fd : cInt; buf: pChar; nbytes : TSize): TSsize; external name 'FPC_SYSC_READ';
    Function  FpWrite      (fd : cInt; buf:pChar; nbytes : TSize): TSsize; external name 'FPC_SYSC_WRITE';
    Function  FpLseek      (fd : cInt; offset : TOff; whence : cInt): TOff; external name 'FPC_SYSC_LSEEK';
    Function  FpTime       (var tloc : TTime): TTime; external name 'FPC_SYSC_TIME';
    Function  FpFtruncate  (fd : cInt; flength : TOff): cInt;  external name 'FPC_SYSC_FTRUNCATE';
    Function  FPSigaction  (sig: cInt; act : pSigActionRec; oact : pSigActionRec): cint;  external name 'FPC_SYSC_SIGACTION';
    Function  FPSelect     (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint;
    Function  FpIOCtl      (Handle:cint;Ndx: culong;Data: Pointer):cint; external name  'FPC_SYSC_IOCTL';
    Function  FpNanoSleep  (req : ptimespec;rem : ptimespec):cint; external name 'FPC_SYSC_NANOSLEEP';
    Function  fpLstat(path:pchar;Info:pstat):cint;
    Function  fpLstat(Filename: ansistring;Info:pstat):cint;
    Function  fpSymlink(oldname,newname:pchar):cint;
    Function  fpReadLink(name,linkname:pchar;maxlen:size_t):cint; external name  'FPC_SYSC_READLINK';

    function  fpNice(N:cint):cint;
    Function  fpGetPriority(Which,Who:cint):cint;
    Function  fpSetPriority(Which,Who,What:cint):cint;
    Function  Fpmmap(start:pointer;len:size_t;prot:cint;flags:cint;fd:cint;offst:off_t):pointer; external name 'FPC_SYSC_MMAP';
    Function  Fpmunmap(start:pointer;len:size_t):cint;  external name 'FPC_SYSC_MUNMAP';

    Function  FpGetEnv     (name : pChar): pChar;
    function  fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;

{
  $Log$
  Revision 1.15  2005-02-13 20:01:38  peter
    * include file cleanup

  Revision 1.14  2004/11/21 11:28:21  peter
    * fixed bootstrap with 1.0.10 and 1.9.4

  Revision 1.13  2004/11/19 13:15:14  marco
   * external rework. Mostly done.

  Revision 1.12  2004/11/14 12:21:08  marco
   * moved some calls from unix to baseunix. Darwin untested.

  Revision 1.11  2003/12/30 12:24:01  marco
   * FPC_USE_LIBC

  Revision 1.10  2003/09/27 13:49:41  peter
    * fpnanosleep exported

  Revision 1.9  2003/09/27 12:51:33  peter
    * fpISxxx macros renamed to C compliant fpS_ISxxx

  Revision 1.8  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.7  2003/09/17 11:24:46  marco
   * fixes for new macro's

  Revision 1.6  2003/09/17 11:14:25  marco
   * two extra FPIS added

  Revision 1.5  2003/09/16 16:13:56  marco
   * fdset functions renamed to fp<posix name>

  Revision 1.4  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.3  2003/06/01 16:28:41  marco
   * Enhancements to make the compiler baseunix using.

  Revision 1.2  2003/01/05 19:11:32  marco
   * small changes originating from introduction of Baseunix to FreeBSD

  Revision 1.1  2002/12/18 16:49:02  marco
   * New RTL. Linux system unit and baseunix operational.


}