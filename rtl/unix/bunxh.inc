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

Type TGrpArr = Array [0..0] of TGid;		{ C style array workarounds}
     pGrpArr = ^TGrpArr;
     TFilDes = Array [0..1] of cInt;
     pFilDes = ^TFilDes;

    Function  FpsigEmptySet(var nset : TSigSet): cint;
    Function  FpSigFillSet (var nset : TSigSet): cInt;
    Function  FpSigAddSet  (var nset : TSigSet; signo : cInt): cInt;
    Function  FpSigDelSet  (var nset : TSigSet; signo : cInt): cInt;
    Function  FpSigIsMember(Const nset : TSigSet; signo : cInt): cInt;
    Function  FpSigProcMask(how : cInt; nset : pSigSet; oset : pSigSet): cInt; 
    Function  FpSigProcMask(how : cInt; Const nset : TSigSet; var oset : TSigSet): cInt; 
    Function  FpSigPending (var nset : TSigSet): cInt;
    Function  FpSigSuspend (Const sigmask : TSigSet): cInt;

    Function  FpUmask	   (cmask : TMode): TMode;
    Function  FpLink	   (existing : pChar; newone : pChar): cInt;
    Function  FpMkfifo	   (path : pChar; Mode : TMode): cInt;
    Function  FpChmod	   (path : pChar; Mode : TMode): cInt;
    Function  FpChown	   (path : pChar; owner : TUid; group : TGid): cInt;
    Function  FpUtime	   (path : pChar; times : putimbuf): cInt;
    Function  FpPipe	   (var fildes : tfildes):cInt;
    Function  FpDup	   (fildes : cInt): cInt;
    Function  FpDup2	   (fildes, fildes2 : cInt): cInt;
    Function  FpTimes	   (var buffer : tms): TClock;

    Function  FpAlarm	   (seconds : cuint): cuint;
    Function  FpPause : cInt;
    Function  FpSleep      (seconds : cuint): cuint;

    Function  FpGetpid  : TPid;
    Function  FpGetppid : TPid;
    Function  FpGetuid  : TUid;
    Function  FpGeteuid : TUid;
    Function  FpGetgid  : TGid;
    Function  FpGetegid : TGid;
    Function  FpSetuid 	   (uid : TUid): cInt;
    Function  FpSetgid 	   (gid : TGid): cInt;
    Function  FpGetgroups (gidsetsize : cInt; var grouplist : tgrparr): cInt;
    Function  FpGetpgrp : TPid;
    Function  FpSetsid  : TPid;
    Function  FpFcntl	   (fildes : cInt; cmd : cInt): cInt;
    Function  FpFcntl	   (fildes : cInt; cmd : cInt; arg : cInt): cInt;
    Function  FpFcntl	   (fildes : cInt; cmd : cInt; var arg : flock): cInt;

    Function  FpGetcwd	   (path:pChar; siz:TSize):pChar;
    Function  FpFork : TPid;
    Function  FpExecve	   (path : pChar; argv : ppChar; envp: ppChar): cInt;
    Function  FpExecv	   (path : pChar; argv : ppChar): cInt;
    Function  FpWaitpid	   (pid : TPid; var stat_loc : cInt; options: cInt): TPid;
    Function  FpWait	   (var stat_loc : cInt): TPid;
    Procedure FpExit	   (Status : cInt);
    Function  FpKill	   (pid : TPid; sig: cInt): cInt;
    Function  FpUname	   (var name: utsname): cInt;
    Function  FpOpendir    (dirname : pChar): pDir;
    Function  FpReaddir	   (var dirp : Dir) : pDirent;
    Function  FpClosedir   (var dirp : Dir): cInt;
    Function  FpChdir	   (path : pChar): cInt;
    Function  FpOpen	   (path : pChar; flags : cInt; Mode: TMode):cInt;
    Function  FpMkdir	   (path : pChar; Mode: TMode):cInt;
    Function  FpUnlink	   (path : pChar): cInt;
    Function  FpRmdir	   (path : pChar): cInt;
    Function  FpRename	   (old  : pChar; newpath: pChar): cInt;
    Function  FpFStat	   (fd : cInt; var sb : stat): cInt;
    Function  FpStat	   (path: pChar; var buf : stat): cInt;
    Function  FpAccess	   (pathname : pChar; aMode : cInt): cInt;
    Function  FpClose	   (fd : cInt): cInt;
    Function  FpRead	   (fd : cInt; buf: pChar; nbytes : TSize): TSsize;
    Function  FpWrite	   (fd : cInt; buf:pChar; nbytes : TSize): TSsize;
    Function  FpLseek	   (fd : cInt; offset : TOff; whence : cInt): TOff;
    Function  FpTime	   (var tloc : TTime): TTime;
    Function  FpFtruncate  (fd : cInt; flength : TOff): cInt;

    Function  FpGetEnv	   (name : pChar): pChar;
    Function  FpGetEnv	   (name : String): pChar;

    Function  FPIsDir	   (m : TMode): Boolean;
    Function  FPIsChr	   (m : TMode): Boolean;
    Function  FPIsBlk	   (m : TMode): Boolean;
    Function  FPIsReg	   (m : TMode): Boolean;
    Function  FPIsFIFO	   (m : TMode): Boolean;

    Function  wifexited    (Status : cInt): cInt;
    Function  wexitStatus  (Status : cInt): cInt;
    Function  wstopsig	   (Status : cInt): cInt;
    Function  wifsignaled  (Status : cInt): cInt;
    Function  wtermsig	   (Status : cInt): cInt;

{$i bunxovlh.inc}	// overloads and/or redefs.

{
  $Log$
  Revision 1.2  2003-01-05 19:11:32  marco
   * small changes originating from introduction of Baseunix to FreeBSD

  Revision 1.1  2002/12/18 16:49:02  marco
   * New RTL. Linux system unit and baseunix operational.


}