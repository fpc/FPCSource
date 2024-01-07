{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by the Free Pascal development team

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

{$if defined(aix) and not defined(FPC_IS_SYSTEM)}
{ in AIX, several UNIX routines are part of the "compatibility interface for
  older Berkeley Software Distribution (BSD) system programs" }
{$linklib libbsd}
{$endif aix}

Type TGrpArr = Array [0..0] of TGid;            { C style array workarounds}
     pGrpArr = ^TGrpArr;
     TFilDes = Array [0..1] of cInt;
     pFilDes = ^TFilDes;

     TTimespecArr = array[0..1] of ttimespec;

const
{$if (defined(linux) and defined(cpu32) and not defined(fs32bit)) or defined(aix)}
    suffix64bit = '64';
    darwinsuffix64bit = '';
{$elseif defined(darwin_new_iostructs) and (defined(cpui386) or defined(cpux86_64))}
    suffix64bit = '';
    darwinsuffix64bit = '$INODE64';
{$else}
    suffix64bit = '';
    darwinsuffix64bit = '';
{$endif}

    function  FPSigProcMask(how:cint;nset : psigset;oset : psigset):cint;cdecl; external clib name 'sigprocmask';
    function  FPSigProcMask(how:cint;const nset : sigset;var oset : sigset):cint;cdecl; external clib name 'sigprocmask';
    Function  FpSigPending   (var nset : sigset): cInt; external clib name 'sigpending';
    Function  FpSigSuspend   (constref sigmask : TSigSet): cInt; external clib name 'sigsuspend';
{$if not defined(darwin) and not defined(haiku) and not defined(beos)}
    Function  FpSigTimedWait (Constref sigset : TSigSet; info : Psiginfo; timeout:Ptimespec): cInt; cdecl; external clib name 'sigtimedwait';
{$endif}
    Function  FpUmask       (cmask : TMode): TMode; cdecl; external clib name 'umask';
    Function  FpLink       (existing : PAnsiChar; newone : PAnsiChar): cInt; cdecl; external clib name 'link';
    Function  FpMkfifo     (path: PAnsiChar; mode: tmode): cint; cdecl; external clib name 'mkfifo';
    Function  FpChmod      (path : PAnsiChar; Mode : TMode): cInt; cdecl; external clib name 'chmod';
    Function  FpChown   (path : PAnsiChar; owner : TUid; group : TGid): cInt; cdecl; external clib name 'chown';
    Function  FPUtime(path:PAnsiChar;times:putimbuf):cint; cdecl; external clib name 'utime';
    Function  FPFUtimens(handle: cint;constref times: TTimespecArr):cint; cdecl; external clib name 'futimens';
    Function  FpPipe       (var fildes : tfildes):cInt; cdecl;external clib name 'pipe';
    function  FpDup     (oldd:cint):cint; cdecl; external clib name 'dup';
    function  FpDup2    (oldd:cint;newd:cint):cint; cdecl; external clib name 'dup2';
    function  FpTimes	(var buffer : tms): TClock; cdecl; external clib name 'times';

    Function  FpAlarm      (seconds : cuint) : cuint; cdecl;external clib name 'alarm';
    Function  FpPause   : cInt; cdecl; external clib name 'pause';
    Function  FpSleep      (seconds : cuint): cuint; external clib name 'sleep';

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
    Function  FpGetsid (pid:TPid)  : TPid; cdecl; external clib name 'getsid';

    { The libc version has "..." as third parameter -> wrap for }
    { interface compatibility with syscalls                     }
    Function  FpFcntl      (fildes : cInt; cmd : cInt): cInt; {$ifdef FPC_IS_SYSTEM}forward;{$endif}
    Function  FpFcntl      (fildes : cInt; cmd : cInt; arg :cInt): cInt; {$ifdef FPC_IS_SYSTEM}forward;{$endif}
    Function  FpFcntl      (fildes : cInt; cmd : cInt; var arg : flock): cInt; {$ifdef FPC_IS_SYSTEM}forward;{$endif}

    Function  FpGetcwd  (path:PAnsiChar; siz:TSize):PAnsiChar; cdecl;  external clib name 'getcwd';
    function  FpFork  : TPid; cdecl; external clib name 'fork';
    function  FpExecve  (path : PAnsiChar; argv : PPAnsiChar; envp: PPAnsiChar): cint; cdecl; external clib name 'execve';
    function  FpExecv  (path : PAnsiChar; argv : PPAnsiChar): cint; cdecl; external clib name 'execv';
    function  FpWaitpid (pid : TPid; stat_loc : pcint; options: cint): TPid; cdecl; external clib name 'waitpid';
    Function  FpWait       (var stat_loc : cInt): TPid; cdecl; external clib name 'wait';
    procedure FpExit    (status : cint); cdecl; external clib name '_exit';
    Function  FpKill       (pid : TPid; sig: cInt): cInt; cdecl; external clib name 'kill';
    function  FpUname   (var name: utsname): cint; cdecl; external clib name 'uname';
    function  FpOpendir (dirname : PAnsiChar): pdir; cdecl; external clib name 'opendir'{$ifdef aix}+suffix64bit{$endif};
    Function  FpReaddir (var dirp : Dir) : pDirent;cdecl; external clib name 'readdir'+suffix64bit+darwinsuffix64bit;
    Function  FpClosedir (var dirp : Dir): cInt; cdecl; external clib name 'closedir'{$ifdef aix}+suffix64bit{$endif};
    function  FpChdir   (path : PAnsiChar): cint; cdecl; external clib name 'chdir';
    { emulate the bunxovl version that automatically passes
     638 as mode }
    function  FpOpen    (path: PAnsiChar; flags : cint; mode: TMode):cint; {$ifdef FPC_IS_SYSTEM}forward;{$endif}
    function  FpMkdir   (path : PAnsiChar; mode: TMode):cint; cdecl; external clib name 'mkdir';
    function  FpUnlink  (path: PAnsiChar): cint; cdecl; external clib name 'unlink';
    function  FpRmdir   (path : PAnsiChar): cint; cdecl; external clib name 'rmdir';
    function  FpRename  (old : PAnsiChar; newpath: PAnsiChar): cint; cdecl;external clib name 'rename';
{$ifdef linux}
{$ifndef FPC_IS_SYSTEM}
    { those functions are macros on linux }
    function  FpFstat   (fd : cint; var sb : stat): cint;inline;
    Function  fpLstat       (path:PAnsiChar;Info:pstat):cint;inline;
    function  FpStat      (path: PAnsiChar; var buf : stat): cint;inline;
{$endif FPC_IS_SYSTEM}
{$else linux}
    function  FpFstat   (fd : cint; var sb : stat): cint; cdecl; external clib name 'fstat'+suffix64bit+darwinsuffix64bit;
    Function  fpLstat   (path:PAnsiChar;Info:pstat):cint;     cdecl; external clib name 'lstat'+suffix64bit+darwinsuffix64bit;
    function  FpStat    (path: PAnsiChar; var buf : stat): cint; cdecl; external clib name 'stat'+suffix64bit+darwinsuffix64bit;
{$endif linux}
    function  FpAccess  (pathname : PAnsiChar; amode : cint): cint; cdecl; external clib name 'access';
    function  FpClose   (fd : cint): cint; cdecl; external clib name 'close';

    function  FpRead    (fd: cint; buf: PAnsiChar; nbytes : TSize): TSSize; cdecl; external clib name 'read';
    Function  FpPRead      (fd : cInt; buf: PAnsiChar; nbytes : TSize; offset:Toff): TSsize;   cdecl; external clib name 'pread'+suffix64bit;
    function  FpReadV	   (fd: cint; const iov : piovec; iovcnt : cint):TSSize;  cdecl; external clib name 'readv';
    function  FpWrite   (fd: cint;buf:PAnsiChar; nbytes : TSize): TSSize; cdecl; external clib name 'write';
    Function  FpPWrite     (fd : cInt; buf:PAnsiChar; nbytes : TSize; offset:Toff): TSSize;  cdecl; external clib name 'pwrite'+suffix64bit;
    function  FpWriteV	   (fd: cint; const iov : piovec; iovcnt : cint):TSSize;  cdecl; external clib name 'writev';

    function  FpLseek   (fd : cint; offset : TOff; whence : cint): TOff; cdecl; external clib name 'lseek'+suffix64bit;
    Function  FpTime       (var tloc : Time_t): Time_t; cdecl; external clib name 'time';
    function  FpFtruncate(fd : cint; flength : TOff): cint; cdecl; external clib name 'ftruncate'+suffix64bit;
    Function  FPSigaction (sig: cInt; act :pSigActionRec;oact:pSigActionRec):cint;cdecl; external clib name 'sigaction';
{$ifdef beos}
  {$ifdef haiku}
    Function  FPSelect  (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint; cdecl; external 'network' name 'select';
    Function  FpPoll    (fds: ppollfd; nfds: cuint; timeout: clong): cint; cdecl; external clib name 'poll';
  {$else}
    Function  FPSelect  (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint; cdecl; external 'net' name 'select';
    Function  FpPoll    (fds: ppollfd; nfds: cuint; timeout: clong): cint; cdecl; external clib name 'poll';
  {$endif}  
{$else}
    Function  FPSelect  (N:cint;readfds,writefds,exceptfds:pfdSet;TimeOut:PTimeVal):cint; cdecl; external clib name 'select';
    Function  FpPoll    (fds: ppollfd; nfds: cuint; timeout: clong): cint; cdecl; external clib name 'poll';
{$endif}
    { The libc version has "..." as third parameter -> wrap for }
    { interface compatibility with syscalls                     }
    function  FpIOCtl   (Handle:cint;Ndx: TIOCtlRequest;Data: Pointer):cint; {$ifdef FPC_IS_SYSTEM}forward;{$endif}
{$ifdef solaris}   
{$linklib md5} 
{$linklib aio}
    Function  FPnanosleep  (const rqtp: ptimespec; rmtp: ptimespec): cint; cdecl; external 'rt' name 'nanosleep';
{$else solaris}
{$if not defined(beos) or defined(haiku)}
    Function  FPnanosleep  (const rqtp: ptimespec; rmtp: ptimespec): cint; cdecl; external clib name 'nanosleep';
{$endif}
{$endif solaris}
    Function  fpSymlink    (oldname,newname:PAnsiChar):cint;   cdecl; external clib name 'symlink';
    Function  fpReadLink           (name,linkname:PAnsiChar;maxlen:size_t):cint;  cdecl; external clib name 'readlink';

    function  fpNice       (N:cint):cint; cdecl; external clib name 'nice';
    Function  fpGetPriority (Which,Who:cint):cint;      cdecl; external clib name 'getpriority';
    Function  fpSetPriority (Which,Who,What:cint):cint; cdecl; external clib name 'setpriority';
    function  fpmmap    (addr:pointer;len:size_t;prot:cint;flags:cint;fd:cint;ofs:off_t):pointer; cdecl; external clib name 'mmap'+suffix64bit;
    function  fpmunmap  (addr:pointer;len:size_t):cint; cdecl; external clib name 'munmap';
    function  fpmprotect(addr:pointer;len:size_t;prot:cint):cint; cdecl; external clib name 'mprotect';

    function  fpgetenv  (name : PAnsiChar):PAnsiChar; cdecl; external clib name 'getenv';
{$ifndef beos}
    function  fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint; cdecl; external clib name 'settimeofday';
{$else}
//    function  fpsettimeofday(tp:ptimeval;tzp:ptimezone):cint;
{$endif}

    function FpGetRLimit(resource : cInt; rlim : PRLimit) : cInt; cdecl; external clib name 'getrlimit';
    function FpSetRLimit(resource : cInt; rlim : PRLimit) : cInt; cdecl; external clib name 'setrlimit';

    { not supported with syscalls }
    function  FpvFork  : TPid; cdecl; external clib name 'vfork';

    { overloads not available in bunxh.inc }
    function  FpReaddir (dirp : pdir) : pdirent;cdecl; external clib name 'readdir'+suffix64bit;
    function  FpClosedir (dirp : pdir): cint; cdecl; external clib name 'closedir'{$ifdef aix}+suffix64bit{$endif};
    function  FpSigaction (sig: cint;var act : sigactionrec; var oact : sigactionrec): cint; cdecl; external clib name 'sigaction';
{$if not defined(darwin) and not defined(haiku) and not defined(beos)}
    Function  FpSigTimedWait (Const sigset : TSigSet; info : Psiginfo; var timeout:Ttimespec): cInt; cdecl; external clib name 'sigtimedwait';
{$endif}
    function  FpTime       (tloc:ptime_t): time_t; cdecl; external clib name 'time';

{$if defined(linux)}
    function  FpSchedGetAffinity(pid : pid_t;cpusetsize : size_t;mask : pcpu_set_t) : cint; cdecl; external clib name 'sched_getaffinity';
{$endif}
    Function FpPathconf(path : PAnsiChar;name : cint) : clong; cdecl; external clib name 'pathconf';

