{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY;without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
Unit Linux;
Interface

{ Get Types and Constants }
{$i sysconst.inc}
{$i systypes.inc}

{ Get System call numbers and error-numbers}
{$i sysnr.inc}
{$i errno.inc}

var
  ErrNo,
  LinuxError : Longint;


{********************
      Process
********************}

const
  { cloning flags }
  CSIGNAL       = $000000ff; // signal mask to be sent at exit
  CLONE_VM      = $00000100; // set if VM shared between processes
  CLONE_FS      = $00000200; // set if fs info shared between processes
  CLONE_FILES   = $00000400; // set if open files shared between processes
  CLONE_SIGHAND = $00000800; // set if signal handlers shared
  CLONE_PID     = $00001000; // set if pid shared
type
  TCloneFunc=function(args:pointer):longint;cdecl;

const
  { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;

  WNOHANG   = $1;
  WUNTRACED = $2;
  __WCLONE  = $80000000;


{********************
      File
********************}

Const
  P_IN  = 1;
  P_OUT = 2;

Const
  LOCK_SH = 1;
  LOCK_EX = 2;
  LOCK_UN = 8;
  LOCK_NB = 4;


Type
  Tpipe = array[1..2] of longint;

  pglob = ^tglob;
  tglob = record
    name : pchar;
    next : pglob;
  end;

  ComStr  = String[255];
  PathStr = String[255];
  DirStr  = String[255];
  NameStr = String[255];
  ExtStr  = String[255];

const

  { For testing  access rights }
  R_OK = 4;
  W_OK = 2;
  X_OK = 1;
  F_OK = 0;

  { For File control mechanism }
  F_GetFd  = 1;
  F_SetFd  = 2;
  F_GetFl  = 3;
  F_SetFl  = 4;
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_GetOwn = 8;
  F_SetOwn = 9;



{********************
      Signal
********************}

Const
  { For sending a signal }
  SA_NOCLDSTOP = 1;
  SA_SHIRQ     = $04000000;
  SA_STACK     = $08000000;
  SA_RESTART   = $10000000;
  SA_INTERRUPT = $20000000;
  SA_NOMASK    = $40000000;
  SA_ONESHOT   = $80000000;

  SIG_BLOCK   = 0;
  SIG_UNBLOCK = 1;
  SIG_SETMASK = 2;

  SIG_DFL = 0 ;
  SIG_IGN = 1 ;
  SIG_ERR = -1 ;

  SIGHUP     = 1;
  SIGINT     = 2;
  SIGQUIT    = 3;
  SIGILL     = 4;
  SIGTRAP    = 5;
  SIGABRT    = 6;
  SIGIOT     = 6;
  SIGBUS     = 7;
  SIGFPE     = 8;
  SIGKILL    = 9;
  SIGUSR1    = 10;
  SIGSEGV    = 11;
  SIGUSR2    = 12;
  SIGPIPE    = 13;
  SIGALRM    = 14;
  SIGTerm    = 15;
  SIGSTKFLT  = 16;
  SIGCHLD    = 17;
  SIGCONT    = 18;
  SIGSTOP    = 19;
  SIGTSTP    = 20;
  SIGTTIN    = 21;
  SIGTTOU    = 22;
  SIGURG     = 23;
  SIGXCPU    = 24;
  SIGXFSZ    = 25;
  SIGVTALRM  = 26;
  SIGPROF    = 27;
  SIGWINCH   = 28;
  SIGIO      = 29;
  SIGPOLL    = SIGIO;
  SIGPWR     = 30;
  SIGUNUSED  = 31;

Type
  SignalHandler   = Procedure(Sig : LongInt);cdecl;
  PSignalHandler  = ^SignalHandler;
  SignalRestorer  = Procedure;cdecl;
  PSignalRestorer = ^SignalRestorer;

  SigSet  = Longint;
  PSigSet = ^SigSet;

  SigActionRec = packed record
    Sa_Handler  : SignalHandler;
    Sa_Mask     : SigSet;
    Sa_Flags    : Longint;
  {  Sa_restorer : SignalRestorer; }
  end;
  PSigActionRec = ^SigActionRec;


{********************
   IOCtl(TermIOS)
********************}

Const
  { Amount of Control Chars }
  NCCS = 19;
  NCC = 8;

  { For Terminal handling }
  TCGETS          = $5401;
  TCSETS          = $5402;
  TCSETSW         = $5403;
  TCSETSF         = $5404;
  TCGETA          = $5405;
  TCSETA          = $5406;
  TCSETAW         = $5407;
  TCSETAF         = $5408;
  TCSBRK          = $5409;
  TCXONC          = $540A;
  TCFLSH          = $540B;
  TIOCEXCL        = $540C;
  TIOCNXCL        = $540D;
  TIOCSCTTY       = $540E;
  TIOCGPGRP       = $540F;
  TIOCSPGRP       = $5410;
  TIOCOUTQ        = $5411;
  TIOCSTI         = $5412;
  TIOCGWINSZ      = $5413;
  TIOCSWINSZ      = $5414;
  TIOCMGET        = $5415;
  TIOCMBIS        = $5416;
  TIOCMBIC        = $5417;
  TIOCMSET        = $5418;
  TIOCGSOFTCAR    = $5419;
  TIOCSSOFTCAR    = $541A;
  FIONREAD        = $541B;
  TIOCINQ         = FIONREAD;
  TIOCLINUX       = $541C;
  TIOCCONS        = $541D;
  TIOCGSERIAL     = $541E;
  TIOCSSERIAL     = $541F;
  TIOCPKT         = $5420;
  FIONBIO         = $5421;
  TIOCNOTTY       = $5422;
  TIOCSETD        = $5423;
  TIOCGETD        = $5424;
  TCSBRKP         = $5425;
  TIOCTTYGSTRUCT  = $5426;
  FIONCLEX        = $5450;
  FIOCLEX         = $5451;
  FIOASYNC        = $5452;
  TIOCSERCONFIG   = $5453;
  TIOCSERGWILD    = $5454;
  TIOCSERSWILD    = $5455;
  TIOCGLCKTRMIOS  = $5456;
  TIOCSLCKTRMIOS  = $5457;
  TIOCSERGSTRUCT  = $5458;
  TIOCSERGETLSR   = $5459;
  TIOCSERGETMULTI = $545A;
  TIOCSERSETMULTI = $545B;

  TIOCMIWAIT      = $545C;
  TIOCGICOUNT     = $545D;
  TIOCPKT_DATA       = 0;
  TIOCPKT_FLUSHREAD  = 1;
  TIOCPKT_FLUSHWRITE = 2;
  TIOCPKT_STOP       = 4;
  TIOCPKT_START      = 8;
  TIOCPKT_NOSTOP     = 16;
  TIOCPKT_DOSTOP     = 32;


Type
  winsize = packed record
    ws_row,
    ws_col,
    ws_xpixel,
    ws_ypixel : word;
  end;
  TWinSize=winsize;

  Termio = packed record
    c_iflag,                             { input mode flags }
    c_oflag,                             { output mode flags }
    c_cflag,                             { control mode flags }
    c_lflag  : Word;                    { local mode flags }
    c_line   : Word;                    { line discipline - careful, only High byte in use}
    c_cc     : array [0..NCC-1] of char;{ control characters }
  end;
  TTermio=Termio;

  Termios = packed record
    c_iflag,
    c_oflag,
    c_cflag,
    c_lflag  : longint;
    c_line   : char;
    c_cc     : array[0..NCCS-1] of byte;
  end;
  TTermios=Termios;

const
  InitCC:array[0..NCCS-1] of byte=(3,34,177,25,4,0,1,0,21,23,32,0,22,17,27,26,0,0,0);

const
{c_cc characters}
   VINTR    = 0;
   VQUIT    = 1;
   VERASE   = 2;
   VKILL    = 3;
   VEOF     = 4;
   VTIME    = 5;
   VMIN     = 6;
   VSWTC    = 7;
   VSTART   = 8;
   VSTOP    = 9;
   VSUSP    = 10;
   VEOL     = 11;
   VREPRINT = 12;
   VDISCARD = 13;
   VWERASE  = 14;
   VLNEXT   = 15;
   VEOL2    = 16;

{c_iflag bits}
   IGNBRK  = $0000001;
   BRKINT  = $0000002;
   IGNPAR  = $0000004;
   PARMRK  = $0000008;
   INPCK   = $0000010;
   ISTRIP  = $0000020;
   INLCR   = $0000040;
   IGNCR   = $0000080;
   ICRNL   = $0000100;
   IUCLC   = $0000200;
   IXON    = $0000400;
   IXANY   = $0000800;
   IXOFF   = $0001000;
   IMAXBEL = $0002000;

{c_oflag bits}
   OPOST  = $0000001;
   OLCUC  = $0000002;
   ONLCR  = $0000004;
   OCRNL  = $0000008;
   ONOCR  = $0000010;
   ONLRET = $0000020;
   OFILL  = $0000040;
   OFDEL  = $0000080;
   NLDLY  = $0000100;
     NL0  = $0000000;
     NL1  = $0000100;
   CRDLY  = $0000600;
     CR0  = $0000000;
     CR1  = $0000200;
     CR2  = $0000400;
     CR3  = $0000600;
   TABDLY = $0001800;
     TAB0 = $0000000;
     TAB1 = $0000800;
     TAB2 = $0001000;
     TAB3 = $0001800;
    XTABS = $0001800;
   BSDLY  = $0002000;
     BS0  = $0000000;
     BS1  = $0002000;
   VTDLY  = $0004000;
     VT0  = $0000000;
     VT1  = $0004000;
   FFDLY  = $0008000;
     FF0  = $0000000;
     FF1  = $0008000;

{c_cflag bits}
   CBAUD   = $000100F;
   B0      = $0000000;
   B50     = $0000001;
   B75     = $0000002;
   B110    = $0000003;
   B134    = $0000004;
   B150    = $0000005;
   B200    = $0000006;
   B300    = $0000007;
   B600    = $0000008;
   B1200   = $0000009;
   B1800   = $000000A;
   B2400   = $000000B;
   B4800   = $000000C;
   B9600   = $000000D;
   B19200  = $000000E;
   B38400  = $000000F;
   EXTA    = B19200;
   EXTB    = B38400;
   CSIZE   = $0000030;
     CS5   = $0000000;
     CS6   = $0000010;
     CS7   = $0000020;
     CS8   = $0000030;
   CSTOPB  = $0000040;
   CREAD   = $0000080;
   PARENB  = $0000100;
   PARODD  = $0000200;
   HUPCL   = $0000400;
   CLOCAL  = $0000800;
   CBAUDEX = $0001000;
   B57600  = $0001001;
   B115200 = $0001002;
   B230400 = $0001003;
   B460800 = $0001004;
   CIBAUD  = $100F0000;
   CMSPAR  = $40000000;
   CRTSCTS = $80000000;

{c_lflag bits}
   ISIG    = $0000001;
   ICANON  = $0000002;
   XCASE   = $0000004;
   ECHO    = $0000008;
   ECHOE   = $0000010;
   ECHOK   = $0000020;
   ECHONL  = $0000040;
   NOFLSH  = $0000080;
   TOSTOP  = $0000100;
   ECHOCTL = $0000200;
   ECHOPRT = $0000400;
   ECHOKE  = $0000800;
   FLUSHO  = $0001000;
   PENDIN  = $0004000;
   IEXTEN  = $0008000;

{c_line bits}
   TIOCM_LE   = $001;
   TIOCM_DTR  = $002;
   TIOCM_RTS  = $004;
   TIOCM_ST   = $008;
   TIOCM_SR   = $010;
   TIOCM_CTS  = $020;
   TIOCM_CAR  = $040;
   TIOCM_RNG  = $080;
   TIOCM_DSR  = $100;
   TIOCM_CD   = TIOCM_CAR;
   TIOCM_RI   = TIOCM_RNG;
   TIOCM_OUT1 = $2000;
   TIOCM_OUT2 = $4000;

{TCSetAttr}
   TCSANOW   = 0;
   TCSADRAIN = 1;
   TCSAFLUSH = 2;

{TCFlow}
   TCOOFF = 0;
   TCOON  = 1;
   TCIOFF = 2;
   TCION  = 3;

{TCFlush}
   TCIFLUSH  = 0;
   TCOFLUSH  = 1;
   TCIOFLUSH = 2;


{********************
      Info
********************}

Type
  UTimBuf = packed record {in BSD array[0..1] of timeval, but this is
				backwards compatible with linux version}
    actime,
    dummy1,
    modtime,
    dummy2  : Longint;
   end;

  UTimeBuf=UTimBuf;
  TUTimeBuf=UTimeBuf;
  PUTimeBuf=^UTimeBuf;

  TSysinfo = packed record
    uptime    : longint;
    loads     : array[1..3] of longint;
    totalram,
    freeram,
    sharedram,
    bufferram,
    totalswap,
    freeswap  : longint;
    procs     : integer;
    s         : string[18];
  end;
  PSysInfo = ^TSysInfo;

{******************************************************************************
                            Procedure/Functions
******************************************************************************}

//Function SysCall(callnr:longint;var regs:SysCallregs):longint;

{**************************
     Time/Date Handling
***************************}

var
  tzdaylight : boolean;
  tzseconds  : longint;
  tzname     : array[boolean] of pchar;

{ timezone support }
procedure GetLocalTimezone(timer:longint;var leap_correct,leap_hit:longint);
procedure GetLocalTimezone(timer:longint);
procedure ReadTimezoneFile(fn:string);
function  GetTimezoneFile:string;

Procedure GetTimeOfDay(var tv:timeval);
Function  GetTimeOfDay:longint;
Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
Function  LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
procedure GetTime(var hour,min,sec,msec,usec:word);
procedure GetTime(var hour,min,sec,sec100:word);
procedure GetTime(var hour,min,sec:word);
Procedure GetDate(Var Year,Month,Day:Word);
Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);

{**************************
     Process Handling
***************************}

function  CreateShellArgV(const prog:string):ppchar;
function  CreateShellArgV(const prog:Ansistring):ppchar;
Procedure Execve(Path:pathstr;args:ppchar;ep:ppchar);
Procedure Execve(path:pchar;args:ppchar;ep:ppchar);
Procedure Execv(const path:pathstr;args:ppchar);
Procedure Execvp(Path:Pathstr;Args:ppchar;Ep:ppchar);
Procedure Execl(const Todo:string);
Procedure Execle(Todo:string;Ep:ppchar);
Procedure Execlp(Todo:string;Ep:ppchar);
Function  Shell(const Command:String):Longint;
Function  Shell(const Command:AnsiString):Longint;
Function  Fork:longint;
function  Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
Procedure ExitProcess(val:longint);
Function  WaitPid(Pid:longint;Status:pointer;Options:Integer):Longint;
Procedure Nice(N:integer);
Function  GetPriority(Which,Who:longint):longint;
Procedure SetPriority(Which,Who,What:longint);
Function  GetPid:LongInt;
Function  GetPPid:LongInt;
Function  GetUid:Longint;
Function  GetEUid:Longint;
Function  GetGid:Longint;
Function  GetEGid:Longint;

{**************************
     File Handling
***************************}

Function  fdOpen(pathname:string;flags:longint):longint;
Function  fdOpen(pathname:string;flags,mode:longint):longint;
Function  fdOpen(pathname:pchar;flags:longint):longint;
Function  fdOpen(pathname:pchar;flags,mode:longint):longint;
Function  fdClose(fd:longint):boolean;
Function  fdRead(fd:longint;var buf;size:longint):longint;
Function  fdWrite(fd:longint;var buf;size:longint):longint;
Function  fdTruncate(fd,size:longint):boolean;
Function  fdSeek (fd,pos,seektype :longint): longint;
Function  fdFlush (fd : Longint) : Boolean;
Function  Link(OldPath,NewPath:pathstr):boolean;
Function  SymLink(OldPath,NewPath:pathstr):boolean;
Function  UnLink(Path:pathstr):boolean;
Function  UnLink(Path:pchar):Boolean;
Function  FReName (OldName,NewName : Pchar) : Boolean;
Function  FReName (OldName,NewName : String) : Boolean;
Function  Chown(path:pathstr;NewUid,NewGid:longint):boolean;
Function  Chmod(path:pathstr;Newmode:longint):boolean;
Function  Utime(path:pathstr;utim:utimebuf):boolean;
Function  Access(Path:Pathstr ;mode:longint):boolean;
Function  Umask(Mask:Integer):integer;
Function  Flock (fd,mode : longint) : boolean;
Function  Flock (var T : text;mode : longint) : boolean;
Function  Flock (var F : File;mode : longint) : boolean;
Function  FStat(Path:Pathstr;Var Info:stat):Boolean;
Function  FStat(Fd:longint;Var Info:stat):Boolean;
Function  FStat(var F:Text;Var Info:stat):Boolean;
Function  FStat(var F:File;Var Info:stat):Boolean;
Function  Lstat(Filename: PathStr;var Info:stat):Boolean;
Function  FSStat(Path:Pathstr;Var Info:statfs):Boolean;
Function  FSStat(Fd: Longint;Var Info:statfs):Boolean;
Function  Fcntl(Fd:longint;Cmd:longint):longint;
Procedure Fcntl(Fd:longint;Cmd:longint;Arg:Longint);
Function  Fcntl(var Fd:Text;Cmd:longint):longint;
Procedure Fcntl(var Fd:Text;Cmd:longint;Arg:Longint);
Function  Dup(oldfile:longint;var newfile:longint):Boolean;
Function  Dup(var oldfile,newfile:text):Boolean;
Function  Dup(var oldfile,newfile:file):Boolean;
Function  Dup2(oldfile,newfile:longint):Boolean;
Function  Dup2(var oldfile,newfile:text):Boolean;
Function  Dup2(var oldfile,newfile:file):Boolean;

Function  Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):longint;
Function  Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:Longint):longint;

Function  SelectText(var T:Text;TimeOut :PTimeVal):Longint;

{**************************
   Directory Handling
***************************}

Function  OpenDir(f:pchar):pdir;
Function  OpenDir(f: String):pdir;
function  CloseDir(p:pdir):integer;
Function  ReadDir(p:pdir):pdirent;
procedure SeekDir(p:pdir;off:longint);
function  TellDir(p:pdir):longint;

{**************************
    Pipe/Fifo/Stream
***************************}

Function  AssignPipe(var pipe_in,pipe_out:longint):boolean;
Function  AssignPipe(var pipe_in,pipe_out:text):boolean;
Function  AssignPipe(var pipe_in,pipe_out:file):boolean;
Function  PClose(Var F:text) : longint;
Function  PClose(Var F:file) : longint;
Procedure POpen(var F:text;const Prog:String;rw:char);
Procedure POpen(var F:file;const Prog:String;rw:char);

Function  mkFifo(pathname:string;mode:longint):boolean;

Procedure AssignStream(Var StreamIn,Streamout:text;Const Prog:String);
function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): LongInt;

{**************************
    General information
***************************}

{
Function  GetDomainName:String;
Function  GetHostName:String;
Function  Sysinfo(var Info:TSysinfo):Boolean;
Function  Uname(var unamerec:utsname):Boolean;
}
Function  GetEnv(P:string):Pchar;

{**************************
        Signal
***************************}

Procedure SigAction(Signum:Integer;Var Act,OldAct:PSigActionRec );
Procedure SigProcMask (How:Integer;SSet,OldSSet:PSigSet);
Function  SigPending:SigSet;
Procedure SigSuspend(Mask:Sigset);
//Function  Signal(Signum:Integer;Handler:SignalHandler):SignalHandler;
Function  Kill(Pid:longint;Sig:integer):integer;
Procedure SigRaise(Sig:integer);
{Function  Alarm(Sec : Longint) : longint;
Procedure Pause;
}
{**************************
  IOCtl/Termios Functions
***************************}

Function  IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
Function  TCGetAttr(fd:longint;var tios:TermIOS):boolean;
Function  TCSetAttr(fd:longint;OptAct:longint;var tios:TermIOS):boolean;
Procedure CFSetISpeed(var tios:TermIOS;speed:Longint);
Procedure CFSetOSpeed(var tios:TermIOS;speed:Longint);
Procedure CFMakeRaw(var tios:TermIOS);
Function  TCSendBreak(fd,duration:longint):boolean;
Function  TCSetPGrp(fd,id:longint):boolean;
Function  TCGetPGrp(fd:longint;var id:longint):boolean;
Function  TCFlush(fd,qsel:longint):boolean;
Function  TCDrain(fd:longint):boolean;
Function  TCFlow(fd,act:longint):boolean;
Function  IsATTY(Handle:Longint):Boolean;
Function  IsATTY(f:text):Boolean;
function  TTYname(Handle:Longint):string;
function  TTYname(var F:Text):string;

{**************************
     Memory functions
***************************}

const
  PROT_READ  = $1;             { page can be read }
  PROT_WRITE = $2;             { page can be written }
  PROT_EXEC  = $4;             { page can be executed }
  PROT_NONE  = $0;             { page can not be accessed }

  MAP_SHARED    = $1;          { Share changes }
  MAP_PRIVATE   = $2;          { Changes are private }
  MAP_TYPE      = $f;          { Mask for type of mapping }
  MAP_FIXED     = $10;         { Interpret addr exactly }
  MAP_ANONYMOUS = $20;         { don't use a file }

  MAP_GROWSDOWN  = $100;       { stack-like segment }
  MAP_DENYWRITE  = $800;       { ETXTBSY }
  MAP_EXECUTABLE = $1000;      { mark it as an executable }
  MAP_LOCKED     = $2000;      { pages are locked }
  MAP_NORESERVE  = $4000;      { don't check for reservations }

type
  tmmapargs=record
    address : longint;
    size    : longint;
    prot    : longint;
    flags   : longint;
    fd      : longint;
    offset  : longint;
  end;

function MMap(const m:tmmapargs):longint;


{**************************
     Port IO functions
***************************}

{
Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
}
{$IFDEF I386}
Procedure WritePort (Port : Longint; Value : Byte);
Procedure WritePort (Port : Longint; Value : Word);
Procedure WritePort (Port : Longint; Value : Longint);
Procedure WritePortl (Port : Longint; Var Buf; Count: longint);
Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
Procedure ReadPort (Port : Longint; Var Value : Byte);
Procedure ReadPort (Port : Longint; Var Value : Word);
Procedure ReadPort (Port : Longint; Var Value : Longint);
Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
{$ENDIF}

{**************************
    Utility functions
***************************}

Function  Octal(l:longint):longint;
Function  FExpand(Const Path: PathStr):PathStr;
Function  FSearch(const path:pathstr;dirlist:string):pathstr;
Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Function  Dirname(Const path:pathstr):pathstr;
Function  Basename(Const path:pathstr;Const suf:pathstr):pathstr;
Function  FNMatch(const Pattern,Name:string):Boolean;
Function  Glob(Const path:pathstr):pglob;
Procedure Globfree(var p:pglob);
Function  StringToPPChar(Var S:STring):ppchar;
Function  GetFS(var T:Text):longint;
Function  GetFS(Var F:File):longint;
{Filedescriptorsets}
Procedure FD_Zero(var fds:fdSet);
Procedure FD_Clr(fd:longint;var fds:fdSet);
Procedure FD_Set(fd:longint;var fds:fdSet);
Function  FD_IsSet(fd:longint;var fds:fdSet):boolean;
{Stat.Mode Types}
Function S_ISLNK(m:word):boolean;
Function S_ISREG(m:word):boolean;
Function S_ISDIR(m:word):boolean;

Function S_ISCHR(m:word):boolean;
Function S_ISBLK(m:word):boolean;
Function S_ISFIFO(m:word):boolean;
Function S_ISSOCK(m:word):boolean;


{******************************************************************************
                            Implementation
******************************************************************************}

Implementation

Uses Strings;


{ Get the definitions of textrec and filerec }
{$i textrec.inc}
{$i filerec.inc}

{This seems ridiculus, but the proc name gets $linux$ prefixed, the external
not}
procedure _actualsyscall; external '_actualsyscall';

{ Raw System calls are in Syscalls.inc}
{$i syscalls.inc}



{******************************************************************************
                          Process related calls
******************************************************************************}

function CreateShellArgV(const prog:string):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
}
var
  pp,p : ppchar;
  temp : string;
begin
  getmem(pp,4*4);
  temp:='/bin/sh'#0'-c'#0+prog+#0;
  p:=pp;
  p^:=@temp[1];
  inc(p);
  p^:=@temp[9];
  inc(p);
  p^:=@temp[12];
  inc(p);
  p^:=Nil;
  CreateShellArgV:=pp;
end;

function CreateShellArgV(const prog:Ansistring):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
  using a AnsiString;
}
var
  pp,p : ppchar;
  temp : AnsiString;
begin
  getmem(pp,4*4);
  temp:='/bin/sh'#0'-c'#0+prog+#0;
  p:=pp;
  GetMem(p^,Length(Temp));
  Move(@Temp[1],p^^,Length(Temp));
  inc(p);
  p^:=@pp[0][8];
  inc(p);
  p^:=@pp[0][11];
  inc(p);
  p^:=Nil;
  CreateShellArgV:=pp;
end;


Function Fork:longint;
{
  This function issues the 'fork' System call. the program is duplicated in memory
  and Execution continues in parent and child process.
  In the parent process, fork returns the PID of the child. In the child process,
  zero is returned.
  A negative value indicates that an error has occurred, the error is returned in
  LinuxError.
}

Begin
 fork:=Do_syscall(SysCall_nr_fork);
 LinuxError:=ErrNo;
End;


function clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
{NOT IMPLEMENTED YET UNDER BSD}
begin
 HALT;
END;
{
  if (pointer(func)=nil) or (sp=nil) then
   begin
     LinuxError:=Sys_EInval;
     exit;
   end;
  asm
        { Insert the argument onto the new stack. }
        movl    sp,%ecx
        subl    $8,%ecx
        movl    args,%eax
        movl    %eax,4(%ecx)

        { Save the function pointer as the zeroth argument.
          It will be popped off in the child in the ebx frobbing below. }
        movl    func,%eax
        movl    %eax,0(%ecx)

        { Do the system call }
        pushl   %ebx
        pushl   %ebx
      //  movl    flags,%ebx
        movl    $251,%eax
        int     $0x80
        popl    %ebx
        popl    %ebx
        test    %eax,%eax
        jnz     .Lclone_end

        { We're in the new thread }
        subl    %ebp,%ebp       { terminate the stack frame }
        call    *%ebx
        { exit process }
        movl    %eax,%ebx
        movl    $1,%eax
        int     $0x80

.Lclone_end:
        movl    %eax,__RESULT
  end;
end;
}

Procedure Execve(path:pathstr;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

Begin
  path:=path+#0;
  do_syscall(syscall_nr_Execve,longint(@path[1]),longint(Args),longint(ep));
 LinuxError:=ErrNo;
End;

Procedure Execve(path:pchar;args:ppchar;ep:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  environment specified in ep is passed on.
}

Begin
  do_syscall(syscall_nr_Execve,longint(path),longint(Args),longint(ep));
 LinuxError:=ErrNo;
End;


Procedure Execv(const path:pathstr;args:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  the current environment is passed on.
}
begin
  Execve(path,args,envp); {On error linuxerror will get set there}
end;

Procedure Execvp(Path:Pathstr;Args:ppchar;Ep:ppchar);
{
  This does the same as Execve, only it searches the PATH environment
  for the place of the Executable, except when Path starts with a slash.
  if the PATH environment variable is unavailable, the path is set to '.'
}
var
  thepath : string;
begin
  if path[1]<>'/' then
   begin
     Thepath:=strpas(getenv('PATH'));
     if thepath='' then
      thepath:='.';
     Path:=FSearch(path,thepath)
   end
  else
   Path:='';
  if Path='' then
   linuxerror:=Sys_enoent
  else
   Execve(Path,args,ep);{On error linuxerror will get set there}
end;

Procedure Execle(Todo:string;Ep:ppchar);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is not searched for 'command'.
  The specified environment(in 'ep') is passed on to command
}
var
  p : ppchar;
begin
  p:=StringToPPChar(ToDo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVE(p^,p,EP);
end;



Procedure Execl(const Todo:string);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is not searched for 'command'.
  The current environment is passed on to command
}
begin
  ExecLE(ToDo,EnvP);
end;



Procedure Execlp(Todo:string;Ep:ppchar);
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is searched for 'command'.
  The specified environment (in 'ep') is passed on to command
}
var
  p : ppchar;
begin
  p:=StringToPPchar(todo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVP(StrPas(p^),p,EP);
end;


Procedure ExitProcess(val:longint);


begin
 do_syscall(Syscall_nr_exit,val);
 LinuxError:=ErrNo;
end;

Function WaitPid(Pid:longint;Status:pointer;Options:Integer):Longint;
{
  Waits until a child with PID Pid exits, or returns if it is exited already.
  Any resources used by the child are freed.
  The exit status is reported in the adress referred to by Status. It should
  be a longint.
}


begin
WaitPID:=do_syscall(syscall_nr_WaitPID,PID,longint(Status),options,0);
 LinuxError:=ErrNo;
end;

Function Shell(const Command:String):Longint;
{
  Executes the shell, and passes it the string Command. (Through /bin/sh -c)
  The current environment is passed to the shell.
  It waits for the shell to exit, and returns its exit status.
  If the Exec call failed exit status 127 is reported.
}
var
  p        : ppchar;
  temp,pid : longint;
begin
  pid:=fork;
  if pid=-1 then
   exit; {Linuxerror already set in Fork}
  if pid=0 then
   begin
     {This is the child.}
     p:=CreateShellArgv(command);
     Execve(p^,p,envp);
     exit(127);
   end;
  temp:=0;
  WaitPid(pid,@temp,0);{Linuxerror is set there}
  Shell:=temp;{ Return exit status }
end;

Function Shell(const Command:AnsiString):Longint;
{
  AnsiString version of Shell
}
var
  p        : ppchar;
  temp,pid : longint;
begin
  pid:=fork;
  if pid=-1 then
   exit; {Linuxerror already set in Fork}
  if pid=0 then
   begin
     {This is the child.}
     p:=CreateShellArgv(command);
     Execve(p^,p,envp);
     exit(127);
   end;
  temp:=0;
  WaitPid(pid,@temp,0);{Linuxerror is set there}
  Shell:=temp;{ Return exit status }
end;


Function GetPriority(Which,Who:longint):longint;
{
  Get Priority of process, process group, or user.
   Which : selects what kind of priority is used.
           can be one of the following predefined Constants :
              Prio_User.
              Prio_PGrp.
              Prio_Process.
   Who : depending on which, this is , respectively :
              Uid
              Pid
              Process Group id
   Errors are reported in linuxerror _only_. (priority can be negative)
}
begin
  errno:=0;
  if (which<prio_process) or (which>prio_user) then
   begin
     { We can save an interrupt here }
     getpriority:=0;
     linuxerror:=Sys_einval;
   end
  else
   begin
     GetPriority:=do_syscall(syscall_nr_GetPriority,which,who);
     LinuxError:=ErrNo;
   end;
end;

Procedure SetPriority(Which,Who,What:longint);
{
 Set Priority of process, process group, or user.
   Which : selects what kind of priority is used.
           can be one of the following predefined Constants :
              Prio_User.
              Prio_PGrp.
              Prio_Process.
   Who : depending on value of which, this is, respectively :
              Uid
              Pid
              Process Group id
   what : A number between -20 and 20. -20 is most favorable, 20 least.
          0 is the default.
}
begin
  errno:=0;
  if ((which<prio_process) or (which>prio_user)) or ((what<-20) or (what>20)) then
   linuxerror:=Sys_einval  { We can save an interrupt here }
  else
   begin
     do_syscall(Syscall_nr_Setpriority,which,who,what);
     LinuxError:=ErrNo;
   end;
end;


Procedure Nice(N:integer);
{
  Set process priority. A positive N means a lower priority.
  A negative N decreases priority.

Doesn't exist in BSD. Linux emu uses setpriority in a construct as below:
}

begin
  SetPriority(Prio_Process,0,N);
end;

Function GetPid:LongInt;
{
  Get Process ID.
}

begin
 GetPID:=do_syscall(Syscall_nr_GetPID);
 LinuxError:=errno;
end;

Function GetPPid:LongInt;
{
  Get Process ID of parent process.
}


begin
  GetPPid:=do_syscall(Syscall_nr_GetPPid);
  LinuxError:=errno;
end;

Function GetUid:Longint;
{
  Get User ID.
}

begin
  GetUID:=do_syscall(Syscall_nr_GetUID);
  LinuxError:=ErrNo;
end;



Function GetEUid:Longint;
{
  Get _effective_ User ID.
}


begin
  GetEUID:=do_syscall(Syscall_nr_GetEUID);
  LinuxError:=ErrNo;
end;


Function GetGid:Longint;
{
  Get Group ID.
}

begin
  GetGID:=do_syscall(Syscall_nr_getgid);
  LinuxError:=ErrNo;
end;


Function GetEGid:Longint;
{
  Get _effective_ Group ID.
}

begin
 GetEGID:=do_syscall(syscall_nr_getegid);
 LinuxError:=ErrNo;
end;

{******************************************************************************
                       Date and Time related calls
******************************************************************************}

Const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;

Function GregorianToJulian(Year,Month,Day:Longint):LongInt;
Var
  Century,XYear: LongInt;
Begin
  If Month<=2 Then
   Begin
     Dec(Year);
     Inc(Month,12);
   End;
  Dec(Month,3);
  Century:=(longint(Year Div 100)*D1) shr 2;
  XYear:=(longint(Year Mod 100)*D0) shr 2;
  GregorianToJulian:=((((Month*153)+2) div 5)+Day)+D2+XYear+Century;
End;



Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word);
Var
  YYear,XYear,Temp,TempMonth : LongInt;
Begin
  Temp:=((JulianDN-D2) shl 2)-1;
  JulianDN:=Temp Div D1;
  XYear:=(Temp Mod D1) or 3;
  YYear:=(XYear Div D0);
  Temp:=((((XYear mod D0)+4) shr 2)*5)-3;
  Day:=((Temp Mod 153)+5) Div 5;
  TempMonth:=Temp Div 153;
  If TempMonth>=10 Then
   Begin
     inc(YYear);
     dec(TempMonth,12);
   End;
  inc(TempMonth,3);
  Month := TempMonth;
  Year:=YYear+(JulianDN*100);
end;

Procedure GetTimeOfDay(var tv:timeval);
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}

var  tz : timezone;

begin
 do_syscall(syscall_nr_gettimeofday,longint(@tv),longint(@tz));
 LinuxError:=Errno;
end;

Function GetTimeOfDay: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  GetTimeOfDay:=Sys_time;
  LinuxError:=Errno;
end;


Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into local time (hour, minute,seconds)
}
Var
  DateNum: LongInt;
Begin
  inc(Epoch,TZSeconds);
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Epoch Mod 86400;
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;


Function LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
{
  Transforms local time (year,month,day,hour,minutes,second) to Epoch time
   (seconds since 00:00, january 1 1970, corrected for local time zone)
}
Begin
  LocalToEpoch:=((GregorianToJulian(Year,Month,Day)-c1970)*86400)+
                (LongInt(Hour)*3600)+(Minute*60)+Second-TZSeconds;
End;


procedure GetTime(var hour,min,sec,msec,usec:word);
{
  Gets the current time, adjusted to local time
}
var
  year,day,month:Word;
  t : timeval;
begin
  gettimeofday(t);
  EpochToLocal(t.sec,year,month,day,hour,min,sec);
  msec:=t.usec div 1000;
  usec:=t.usec mod 1000;
end;


procedure GetTime(var hour,min,sec,sec100:word);
{
  Gets the current time, adjusted to local time
}
var
  usec : word;
begin
  gettime(hour,min,sec,sec100,usec);
  sec100:=sec100 div 10;
end;


Procedure GetTime(Var Hour,Min,Sec:Word);
{
  Gets the current time, adjusted to local time
}
var
  msec,usec : Word;
Begin
  gettime(hour,min,sec,msec,usec);
End;


Procedure GetDate(Var Year,Month,Day:Word);
{
  Gets the current date, adjusted to local time
}
var
  hour,minute,second : word;
Begin
  EpochToLocal(GetTimeOfDay,year,month,day,hour,minute,second);
End;


Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);
{
  Gets the current date, adjusted to local time
}
Begin
  EpochToLocal(GetTimeOfDay,year,month,day,hour,minute,second);
End;

{ Include timezone handling routines which use /usr/share/timezone info }
{$i timezone.inc}


{******************************************************************************
                           FileSystem calls
******************************************************************************}

Function fdOpen(pathname:string;flags:longint):longint;
begin
  pathname:=pathname+#0;
  fdOpen:=Sys_Open(@pathname[1],flags,438);
  LinuxError:=Errno;
end;



Function fdOpen(pathname:string;flags,mode:longint):longint;
begin
  pathname:=pathname+#0;
  fdOpen:=Sys_Open(@pathname[1],flags,mode);
  LinuxError:=Errno;
end;



Function  fdOpen(pathname:pchar;flags:longint):longint;
begin
  fdOpen:=Sys_Open(pathname,flags,0);
  LinuxError:=Errno;
end;



Function  fdOpen(pathname:pchar;flags,mode:longint):longint;
begin
  fdOpen:=Sys_Open(pathname,flags,mode);
  LinuxError:=Errno;
end;



Function fdClose(fd:longint):boolean;
begin
  fdClose:=(Sys_Close(fd)=0);
  LinuxError:=Errno;
end;



Function fdRead(fd:longint;var buf;size:longint):longint;
begin
  fdRead:=Sys_Read(fd,pchar(@buf),size);
  LinuxError:=Errno;
end;



Function fdWrite(fd:longint;var buf;size:longint):longint;
begin
  fdWrite:=Sys_Write(fd,pchar(@buf),size);
  LinuxError:=Errno;
end;



Function fdTruncate(fd,size:longint):boolean;

begin
 fdtruncate:=do_syscall(syscall_nr_ftruncate,fd,size,0)=0;
 LinuxError:=Errno;
end;

Function  fdSeek (fd,pos,seektype :longint): longint;
{
  Do a Seek on a file descriptor fd to position pos, starting from seektype

}
begin
   fdseek:=Sys_LSeek (fd,pos,seektype);
   LinuxError:=Errno;
end;

Function  fdFlush (fd : Longint) : Boolean;

begin
  fdflush:=do_syscall(syscall_nr_fsync,fd)=0;
  LinuxError:=Errno;
end;

function sys_fcntl(Fd:longint;Cmd:longint;Arg:Longint):longint;

begin
 sys_fcntl:=do_syscall(syscall_nr_fcntl,fd,cmd,arg);
 LinuxError:=Errno;
end;

Function Fcntl(Fd:longint;Cmd:longint):longint;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
}

begin
  if (cmd in [F_GetFd,F_GetFl,F_GetOwn]) then
   begin
     Linuxerror:=sys_fcntl(fd,cmd,0);
     if linuxerror=-1 then
      begin
        linuxerror:=errno;
        fcntl:=0;
      end
     else
      begin
        fcntl:=linuxerror;
        linuxerror:=0;
      end;
   end
  else
   begin
     linuxerror:=Sys_einval;
     Fcntl:=0;
   end;
end;


Procedure Fcntl(Fd:longint;Cmd:longint;Arg:Longint);
{
  Read or manipulate a file. (See also fcntl (2) )
  Possible values for Cmd are :
    F_setFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkW,F_SetOwn;
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=Sys_eninval.
  F_DupFD is not allowed, due to the structure of Files in Pascal.
}
begin
  if (cmd in [F_SetFd,F_SetFl,F_GetLk,F_SetLk,F_SetLkw,F_SetOwn]) then
   begin
     sys_fcntl(fd,cmd,arg);
     LinuxError:=ErrNo;
   end
  else
   linuxerror:=Sys_einval;
end;


Function Fcntl(var Fd:Text;Cmd:longint):longint;
begin
  Fcntl := Fcntl(textrec(Fd).handle, Cmd);
end;

Procedure Fcntl(var Fd:Text;Cmd:longint;Arg:Longint);
begin
  Fcntl(textrec(Fd).handle, Cmd, Arg);
end;


Function Chmod(path:pathstr;Newmode:longint):Boolean;
{
  Changes the permissions of a file.
}

begin
  path:=path+#0;
  chmod:=do_syscall(syscall_nr_chmod,longint(@path[1]),newmode)=0;
  LinuxError:=Errno;
end;

Function Chown(path:pathstr;NewUid,NewGid:longint):boolean;
{
  Change the owner and group of a file.
  A user can only change the group to a group of which he is a member.
  The super-user can change uid and gid of any file.
}

begin
  path:=path+#0;
  ChOwn:=do_syscall(syscall_nr_chown,longint(@path[1]),newuid,newgid)=0;
 LinuxError:=Errno;
end;

Function Utime(path:pathstr;utim:utimebuf):boolean;

begin
  UTime:=do_syscall(syscall_nr_utimes,longint(@path[1]),longint(@utim))=0;
  LinuxError:=Errno;
end;

Function  Flock (fd,mode : longint) : boolean;

begin
 Flock:=do_syscall(syscall_nr_flock,fd,mode)=0;
 LinuxError:=Errno;
end;

Function Flock (var T : text;mode : longint) : boolean;
begin
  Flock:=Flock(TextRec(T).Handle,mode);
end;

Function  Flock (var F : File;mode : longint) : boolean;
begin
  Flock:=Flock(FileRec(F).Handle,mode);
end;

Function FStat(Path:Pathstr;Var Info:stat):Boolean;
{
  Get all information on a file, and return it in Info.
}

begin
  path:=path+#0;
  FStat:=(Sys_stat(@(path[1]),Info)=0);
  LinuxError:=errno;
end;

Function Fstat(Fd:Longint;var Info:stat):Boolean;
{
  Get all information on a file descriptor, and return it in info.
}

begin
 FStat:=do_syscall(syscall_nr_fstat,fd,longint(@info))=0;
 LinuxError:=Errno;
end;

Function  FStat(var F:Text;Var Info:stat):Boolean;
{
  Get all information on a text file, and return it in info.
}
begin
  FStat:=Fstat(TextRec(F).Handle,INfo);
end;

Function  FStat(var F:File;Var Info:stat):Boolean;
{
  Get all information on a untyped file, and return it in info.
}
begin
  FStat:=Fstat(FileRec(F).Handle,Info);
end;

Function Lstat(Filename: PathStr;var Info:stat):Boolean;
{
  Get all information on a link (the link itself), and return it in info.
}

begin
 FileName:=FileName+#0;
 LStat:=Do_syscall(syscall_nr_lstat,longint(@filename[1]),longint(@info))=0;
 LinuxError:=Errno;
end;

Function FSStat(Path:Pathstr;Var Info:statfs):Boolean;

{
  Get all information on a fileSystem, and return it in Info.
  Path is the name of a file/directory on the fileSystem you wish to
  investigate.
}

begin
  path:=path+#0;
  FSStat:=Do_Syscall(syscall_nr_statfs,longint(@path[1]),longint(@info))=0;
  LinuxError:=Errno;
end;

Function FSStat(Fd:Longint;Var Info:statfs):Boolean;
{
  Get all information on a fileSystem, and return it in Info.
  Fd is the file descriptor of a file/directory on the fileSystem
  you wish to investigate.
}

begin
 FSStat:=do_syscall(syscall_nr_fstatfs,fd,longint(@info))=0;
 LinuxError:=Errno;
end;

Function Link(OldPath,NewPath:pathstr):boolean;
{
  Proceduces a hard link from new to old.
  In effect, new will be the same file as old.
}
begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  Link:=Do_Syscall(syscall_nr_link,longint(@oldpath[1]),longint(@newpath[1]))=0;
 LinuxError:=Errno;
end;

Function SymLink(OldPath,newPath:pathstr):boolean;
{
  Proceduces a soft link from new to old.
}

begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  SymLink:=Do_Syscall(syscall_nr_symlink,longint(@oldpath[1]),longint(@newpath[1]))=0;
  LinuxError:=Errno;
end;

Function UnLink(Path:pathstr):boolean;
{
  Removes the file in 'Path' (that is, it decreases the link count with one.
  if the link count is zero, the file is removed from the disk.
}
begin
  path:=path+#0;
  Unlink:=Sys_unlink(pchar(@(path[1])))=0;
  linuxerror:=errno;
end;

Function  UnLink(Path:pchar):Boolean;
{
  Removes the file in 'Path' (that is, it decreases the link count with one.
  if the link count is zero, the file is removed from the disk.
}
begin
  Unlink:=(Sys_unlink(path)=0);
  linuxerror:=errno;
end;


Function  FRename (OldName,NewName : Pchar) : Boolean;
begin
  FRename:=Sys_rename(OldName,NewName)=0;
  LinuxError:=Errno;
end;


Function  FRename (OldName,NewName : String) : Boolean;
begin
  OldName:=OldName+#0;
  NewName:=NewName+#0;
  FRename:=FRename (@OldName[1],@NewName[1]);
end;

{!!}
Function Umask(Mask:Integer):integer;
{
  Sets file creation mask to (Mask and 0777 (octal) ), and returns the
  previous value.
}
begin
 UMask:=Do_syscall(syscall_nr_umask,mask);
 LinuxError:=0;
end;

Function Access(Path:Pathstr ;mode:longint):boolean;
{
  Test users access rights on the specified file.
  Mode is a mask xosisting of one or more of R_OK, W_OK, X_OK, F_OK.
  R,W,X stand for read,write and Execute access, simultaneously.
  F_OK checks whether the test would be allowed on the file.
  i.e. It checks the search permissions in all directory components
  of the path.
  The test is done with the real user-ID, instead of the effective.
  If access is denied, or an error occurred, false is returned.
  If access is granted, true is returned.
  Errors other than no access,are reported in linuxerror.
}

begin
  path:=path+#0;
 Access:=do_syscall(syscall_nr_access,mode,longint(@path[1]))=0;
 LinuxError:=Errno;
end;

Function  Dup(oldfile:longint;var newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}

begin
 newfile:=Do_syscall(syscall_nr_dup,oldfile);
 LinuxError:=Errno;
 Dup:=(LinuxError=0);
end;

Function Dup(var oldfile,newfile:text):Boolean;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
begin
  flush(oldfile);{ We cannot share buffers, so we flush them. }
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  Dup:=Dup(textrec(oldfile).handle,textrec(newfile).handle);
end;


Function Dup(var oldfile,newfile:file):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  filerec(newfile):=filerec(oldfile);
  Dup:=Dup(filerec(oldfile).handle,filerec(newfile).handle);
end;


Function Dup2(oldfile,newfile:longint):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}

begin
 do_syscall(syscall_nr_dup2,oldfile,newfile);
 LinuxError:=Errno;
 Dup2:=(LinuxError=0);
end;

Function Dup2(var oldfile,newfile:text):Boolean;
{
  Copies the filedescriptor oldfile to newfile, after flushing the buffer of
  oldfile. It closes newfile if it was still open.
  After which the two textfiles are, in effect, the same, except
  that they don't share the same buffer, and don't share the same
  close_on_exit flag.
}
var
  tmphandle : word;
begin
  case TextRec(oldfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(oldfile);{ We cannot share buffers, so we flush them. }
  end;
  case TextRec(newfile).mode of
    fmOutput, fmInOut, fmAppend :
      flush(newfile);
  end;
  tmphandle:=textrec(newfile).handle;
  textrec(newfile):=textrec(oldfile);
  textrec(newfile).handle:=tmphandle;
  textrec(newfile).bufptr:=@(textrec(newfile).buffer);{ No shared buffer. }
  Dup2:=Dup2(textrec(oldfile).handle,textrec(newfile).handle);
end;


Function Dup2(var oldfile,newfile:file):Boolean;
{
  Copies the filedescriptor oldfile to newfile
}
begin
  filerec(newfile):=filerec(oldfile);
  Dup2:=Dup2(filerec(oldfile).handle,filerec(newfile).handle);
end;


Function Select(N:longint;readfds,writefds,exceptfds:PFDSet;TimeOut:PTimeVal):longint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
}

begin
 Select:=do_syscall(syscall_nr_select,n,longint(readfds),longint(writefds),longint(exceptfds),longint(timeout));
 LinuxError:=Errno;
end;

Function  Select(N:longint;readfds,writefds
	 ,exceptfds:PFDSet;TimeOut:Longint):longint;
{
  Select checks whether the file descriptor sets in readfs/writefs/exceptfs
  have changed.
  This function allows specification of a timeout as a longint.
}
var
  p  : PTimeVal;
  tv : TimeVal;
begin
  if TimeOut=-1 then
   p:=nil
  else
   begin
     tv.Sec:=Timeout div 1000;
     tv.Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  Select:=Select(N,Readfds,WriteFds,ExceptFds,p);
end;



Function SelectText(var T:Text;TimeOut :PTimeval):Longint;
Var
  F:FDSet;
begin
  if textrec(t).mode=fmclosed then
   begin
     LinuxError:=Sys_EBADF;
     exit(-1);
   end;
  FD_Zero(f);
  FD_Set(textrec(T).handle,f);
  if textrec(T).mode=fminput then
   SelectText:=select(textrec(T).handle+1,@f,nil,nil,TimeOut)
  else
   SelectText:=select(textrec(T).handle+1,nil,@f,nil,TimeOut);
end;


{******************************************************************************
                               Directory
******************************************************************************}

Function OpenDir(F:String):PDir;
begin
  F:=F+#0;
  OpenDir:=OpenDir(@F[1]);
end;


procedure SeekDir(p:pdir;off:longint);
begin
  if p=nil then
   begin
     errno:=Sys_EBADF;
     exit;
   end;
  p^.nextoff:=Sys_lseek(p^.fd,off,seek_set);
  p^.size:=0;
  p^.loc:=0;
end;


function TellDir(p:pdir):longint;
begin
  if p=nil then
   begin
     errno:=Sys_EBADF;
     telldir:=-1;
     exit;
   end;
  telldir:=Sys_lseek(p^.fd,0,seek_cur)
  { We could try to use the nextoff field here, but on my 1.2.13
    kernel, this gives nothing... This may have to do with
    the readdir implementation of libc... I also didn't find any trace of
    the field in the kernel code itself, So I suspect it is an artifact of libc.
    Michael. }
end;



Function ReadDir(P:pdir):pdirent;
begin
  ReadDir:=Sys_ReadDir(p);
  LinuxError:=Errno;
end;


{******************************************************************************
                               Pipes/Fifo
******************************************************************************}

Procedure OpenPipe(var F:Text);
begin
  case textrec(f).mode of
    fmoutput :
      if textrec(f).userdata[1]<>P_OUT then
        textrec(f).mode:=fmclosed;
    fminput :
      if textrec(f).userdata[1]<>P_IN then
        textrec(f).mode:=fmclosed;
    else
      textrec(f).mode:=fmclosed;
  end;
end;


Procedure IOPipe(var F:text);
begin
  case textrec(f).mode of
    fmoutput :
      begin
        { first check if we need something to write, else we may
          get a SigPipe when Close() is called (PFV) }
        if textrec(f).bufpos>0 then
          Sys_write(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufpos);
      end;
    fminput :
      textrec(f).bufend:=Sys_read(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufsize);
  end;
  textrec(f).bufpos:=0;
end;


Procedure FlushPipe(var F:Text);
begin
  if (textrec(f).mode=fmoutput) and (textrec(f).bufpos<>0) then
   IOPipe(f);
  textrec(f).bufpos:=0;
end;


Procedure ClosePipe(var F:text);
begin
  textrec(f).mode:=fmclosed;
  Sys_close(textrec(f).handle);
end;



Function AssignPipe(var pipe_in,pipe_out:longint):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  pip  : tpipe;

begin
 do_syscall(syscall_nr_pipe,longint(@pip));
 LinuxError:=Errno;
 pipe_in:=pip[1];
 pipe_out:=pip[2];
 AssignPipe:=(LinuxError=0);
end;

Function AssignPipe(var pipe_in,pipe_out:text):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  f_in,f_out : longint;
begin
  if not AssignPipe(f_in,f_out) then
   begin
     AssignPipe:=false;
     exit;
   end;
{ Set up input }
  Assign(Pipe_in,'');
  Textrec(Pipe_in).Handle:=f_in;
  Textrec(Pipe_in).Mode:=fmInput;
  Textrec(Pipe_in).userdata[1]:=P_IN;
  TextRec(Pipe_in).OpenFunc:=@OpenPipe;
  TextRec(Pipe_in).InOutFunc:=@IOPipe;
  TextRec(Pipe_in).FlushFunc:=@FlushPipe;
  TextRec(Pipe_in).CloseFunc:=@ClosePipe;
{ Set up output }
  Assign(Pipe_out,'');
  Textrec(Pipe_out).Handle:=f_out;
  Textrec(Pipe_out).Mode:=fmOutput;
  Textrec(Pipe_out).userdata[1]:=P_OUT;
  TextRec(Pipe_out).OpenFunc:=@OpenPipe;
  TextRec(Pipe_out).InOutFunc:=@IOPipe;
  TextRec(Pipe_out).FlushFunc:=@FlushPipe;
  TextRec(Pipe_out).CloseFunc:=@ClosePipe;
  AssignPipe:=true;
end;


Function AssignPipe(var pipe_in,pipe_out:file):boolean;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful, linuxerror is set.
}
var
  f_in,f_out : longint;
begin
  if not AssignPipe(f_in,f_out) then
   begin
     AssignPipe:=false;
     exit;
   end;
{ Set up input }
  Assign(Pipe_in,'');
  Filerec(Pipe_in).Handle:=f_in;
  Filerec(Pipe_in).Mode:=fmInput;
  Filerec(Pipe_in).recsize:=1;
  Filerec(Pipe_in).userdata[1]:=P_IN;
{ Set up output }
  Assign(Pipe_out,'');
  Filerec(Pipe_out).Handle:=f_out;
  Filerec(Pipe_out).Mode:=fmoutput;
  Filerec(Pipe_out).recsize:=1;
  Filerec(Pipe_out).userdata[1]:=P_OUT;
  AssignPipe:=true;
end;


Function PClose(Var F:text) :longint;
var
  pl  : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,Textrec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(textrec(f).userdata[2]);
  waitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Function PClose(Var F:file) : longint;
var
  pl : ^longint;
  res : longint;

begin
  do_syscall(syscall_nr_close,filerec(F).Handle);
{ closed our side, Now wait for the other - this appears to be needed ?? }
  pl:=@(filerec(f).userdata[2]);
  waitpid(pl^,@res,0);
  pclose:=res shr 8;
end;

Procedure PCloseText(Var F:text);
{
  May not use @PClose due overloading
}
begin
  PClose(f);
end;

Procedure POpen(var F:text;const Prog:String;rw:char);
{
  Starts the program in 'Prog' and makes it's input or out put the
  other end of a pipe. If rw is 'w' or 'W', then whatever is written to
  F, will be read from stdin by the program in 'Prog'. The inverse is true
  for 'r' or 'R' : whatever the program in 'Prog' writes to stdout, can be
  read from 'f'.
}
var
  pipi,
  pipo : text;
  pid  : longint;
  pl   : ^longint;
  pp   : ppchar;
begin
  LinuxError:=0;
  rw:=upcase(rw);
  if not (rw in ['R','W']) then
   begin
     LinuxError:=Sys_enoent;
     exit;
   end;
  AssignPipe(pipi,pipo);
  if Linuxerror<>0 then
   exit;
  pid:=fork;
  if linuxerror<>0 then
   begin
     close(pipi);
     close(pipo);
     exit;
   end;
  if pid=0 then
   begin
   { We're in the child }
     if rw='W' then
      begin
        close(pipo);
        dup2(pipi,input);
        close(pipi);
        if linuxerror<>0 then
         halt(127);
      end
     else
      begin
        close(pipi);
        dup2(pipo,output);
        close(pipo);
        if linuxerror<>0 then
         halt(127);
      end;
     pp:=createshellargv(prog);
     Execve(pp^,pp,envp);
     halt(127);
   end
  else
   begin
   { We're in the parent }
     if rw='W' then
      begin
        close(pipi);
        f:=pipo;
        textrec(f).bufptr:=@textrec(f).buffer;
      end
     else
      begin
        close(pipo);
        f:=pipi;
        textrec(f).bufptr:=@textrec(f).buffer;
      end;
   {Save the process ID - needed when closing }
     pl:=@(textrec(f).userdata[2]);
     pl^:=pid;
     textrec(f).closefunc:=@PCloseText;
   end;
end;


Procedure POpen(var F:file;const Prog:String;rw:char);
{
  Starts the program in 'Prog' and makes it's input or out put the
  other end of a pipe. If rw is 'w' or 'W', then whatever is written to
  F, will be read from stdin by the program in 'Prog'. The inverse is true
  for 'r' or 'R' : whatever the program in 'Prog' writes to stdout, can be
  read from 'f'.
}
var
  pipi,
  pipo : file;
  pid  : longint;
  pl   : ^longint;
  p,pp : ppchar;
  temp : string[255];
begin
  LinuxError:=0;
  rw:=upcase(rw);
  if not (rw in ['R','W']) then
   begin
     LinuxError:=Sys_enoent;
     exit;
   end;
  AssignPipe(pipi,pipo);
  if Linuxerror<>0 then
   exit;
  pid:=fork;
  if linuxerror<>0 then
   begin
     close(pipi);
     close(pipo);
     exit;
   end;
  if pid=0 then
   begin
   { We're in the child }
     if rw='W' then
      begin
        close(pipo);
        dup2(filerec(pipi).handle,stdinputhandle);
        close(pipi);
        if linuxerror<>0 then
         halt(127);
      end
     else
      begin
        close(pipi);
        dup2(filerec(pipo).handle,stdoutputhandle);
        close(pipo);
        if linuxerror<>0 then
         halt(127);
      end;
     getmem(pp,sizeof(pchar)*4);
     temp:='/bin/sh'#0'-c'#0+prog+#0;
     p:=pp;
     p^:=@temp[1];
     inc(p);
     p^:=@temp[9];
     inc(p);
     p^:=@temp[12];
     inc(p);
     p^:=Nil;
     Execve('/bin/sh',pp,envp);
     halt(127);
   end
  else
   begin
   { We're in the parent }
     if rw='W' then
      begin
        close(pipi);
        f:=pipo;
      end
     else
      begin
        close(pipo);
        f:=pipi;
      end;
   {Save the process ID - needed when closing }
     pl:=@(filerec(f).userdata[2]);
     pl^:=pid;
   end;
end;


Function mkFifo(pathname:string;mode:longint):boolean;

begin
  pathname:=pathname+#0;
  mkfifo:=do_syscall(syscall_nr_mknod,longint(@pathname[1]),mode or STAT_IFIFO,0)=0;
  LinuxError:=Errno;
end;

Procedure AssignStream(Var StreamIn,Streamout:text;Const Prog:String);
{
  Starts the program in 'Prog' and makes its input and output the
  other end of two pipes, which are the stdin and stdout of a program
  specified in 'Prog'.
  streamout can be used to write to the program, streamin can be used to read
  the output of the program. See the following diagram :
  Parent          Child
  STreamout -->  Input
  Streamin  <--  Output
}
var
  pipi,
  pipo : text;
  pid  : longint;
  pl   : ^Longint;
begin
  LinuxError:=0;
  AssignPipe(streamin,pipo);
  if Linuxerror<>0 then
   exit;
  AssignPipe(pipi,streamout);
  if Linuxerror<>0 then
   exit;
  pid:=fork;
  if linuxerror<>0 then
   begin
     close(pipi);
     close(pipo);
     close (streamin);
     close (streamout);
     exit;
   end;
  if pid=0 then
   begin
     { We're in the child }
     { Close what we don't need }
     close(streamout);
     close(streamin);
     dup2(pipi,input);
     if linuxerror<>0 then
      halt(127);
     close(pipi);
     dup2(pipo,output);
     if linuxerror<>0 then
       halt (127);
     close(pipo);
     Execl(Prog);
     halt(127);
   end
  else
   begin
     { we're in the parent}
     close(pipo);
     close(pipi);
     {Save the process ID - needed when closing }
     pl:=@(textrec(StreamIn).userdata[2]);
     pl^:=pid;
     textrec(StreamIn).closefunc:=@PCloseText;
     {Save the process ID - needed when closing }
     pl:=@(textrec(StreamOut).userdata[2]);
     pl^:=pid;
     textrec(StreamOut).closefunc:=@PCloseText;
   end;
end;


function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): LongInt;
{
  Starts the program in 'prog' and makes its input, output and error output the
  other end of three pipes, which are the stdin, stdout and stderr of a program
  specified in 'prog'.
  StreamOut can be used to write to the program, StreamIn can be used to read
  the output of the program, StreamErr reads the error output of the program.
  See the following diagram :
  Parent          Child
  StreamOut -->  StdIn  (input)
  StreamIn  <--  StdOut (output)
  StreamErr <--  StdErr (error output)
}
var
  PipeIn, PipeOut, PipeErr: text;
  pid: LongInt;
  pl: ^LongInt;
begin
  LinuxError := 0;
  AssignStream := -1;

  // Assign pipes
  AssignPipe(StreamIn, PipeOut);
  if LinuxError <> 0 then exit;

  AssignPipe(StreamErr, PipeErr);
  if LinuxError <> 0 then begin
    Close(StreamIn);
    Close(PipeOut);
    exit;
  end;

  AssignPipe(PipeIn, StreamOut);
  if LinuxError <> 0 then begin
    Close(StreamIn);
    Close(PipeOut);
    Close(StreamErr);
    Close(PipeErr);
    exit;
  end;

  // Fork

  pid := Fork;
  if LinuxError <> 0 then begin
    Close(StreamIn);
    Close(PipeOut);
    Close(StreamErr);
    Close(PipeErr);
    Close(PipeIn);
    Close(StreamOut);
    exit;
  end;

  if pid = 0 then begin
    // *** We are in the child ***
    // Close what we don not need
    Close(StreamOut);
    Close(StreamIn);
    Close(StreamErr);
    // Connect pipes
    dup2(PipeIn, Input);
    if LinuxError <> 0 then Halt(127);
    Close(PipeIn);
    dup2(PipeOut, Output);
    if LinuxError <> 0 then Halt(127);
    Close(PipeOut);
    dup2(PipeErr, StdErr);
    if LinuxError <> 0 then Halt(127);
    Close(PipeErr);
    // Execute program
    Execl(Prog);
    Halt(127);
  end else begin
    // *** We are in the parent ***
    Close(PipeErr);
    Close(PipeOut);
    Close(PipeIn);
    // Save the process ID - needed when closing
    pl := @(TextRec(StreamIn).userdata[2]);
    pl^ := pid;
    TextRec(StreamIn).closefunc := @PCloseText;
    // Save the process ID - needed when closing
    pl := @(TextRec(StreamOut).userdata[2]);
    pl^ := pid;
    TextRec(StreamOut).closefunc := @PCloseText;
    // Save the process ID - needed when closing
    pl := @(TextRec(StreamErr).userdata[2]);
    pl^ := pid;
    TextRec(StreamErr).closefunc := @PCloseText;
    AssignStream := pid;
  end;
end;


{******************************************************************************
                        General information calls
******************************************************************************}

{
Function Sysinfo(var Info:TSysinfo):Boolean;
{
  Get system info
}
var
  regs : SysCallregs;
Begin
  regs.reg2:=longint(@info);
  Sysinfo:=SysCall(SysCall_nr_Sysinfo,regs)=0;
End;
}

{
Function Uname(var unamerec:utsname):Boolean;
{
  Get machine's names
}
var
  regs : SysCallregs;
Begin
  regs.reg2:=longint(@unamerec);
  Uname:=SysCall(SysCall_nr_uname,regs)=0;
  LinuxError:=Errno;
End;
}


Function GetEnv(P:string):Pchar;
{
  Searches the environment for a string with name p and
  returns a pchar to it's value.
  A pchar is used to accomodate for strings of length > 255
}
var
  ep    : ppchar;
  found : boolean;
Begin
  p:=p+'=';            {Else HOST will also find HOSTNAME, etc}
  ep:=envp;
  found:=false;
  if ep<>nil then
   begin
     while (not found) and (ep^<>nil) do
      begin
        if strlcomp(@p[1],(ep^),length(p))=0 then
         found:=true
        else
         inc(ep);
      end;
   end;
  if found then
   getenv:=ep^+length(p)
  else
   getenv:=nil;
end;

{

Function GetDomainName:String;
{
  Get machines domain name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  Uname(Sysn);
  linuxerror:=errno;
  If linuxerror<>0 then
   getdomainname:=''
  else
   getdomainname:=strpas(@Sysn.domainname[0]);
end;

Function GetHostName:String;
{
  Get machines name. Returns empty string if not set.
}

Var
  Sysn : utsname;
begin
  uname(Sysn);
  linuxerror:=errno;
  If linuxerror<>0 then
   gethostname:=''
  else
   gethostname:=strpas(@Sysn.nodename[0]);
end;
}

{******************************************************************************
                          Signal handling calls
******************************************************************************}

Function Kill(Pid:longint;Sig:integer):integer;
{
  Send signal 'sig' to a process, or a group of processes.
  If Pid >  0 then the signal is sent to pid
     pid=-1                         to all processes except process 1
     pid < -1                         to process group -pid
  Return value is zero, except for case three, where the return value
  is the number of processes to which the signal was sent.
}

begin
 kill:=do_syscall(syscall_nr_kill,pid,sig);
 if kill<0 THEN
  Kill:=0;
 LinuxError:=Errno;
end;

Procedure SigAction(Signum:Integer;Var Act,OldAct:PSigActionRec );
{
  Change action of process upon receipt of a signal.
  Signum specifies the signal (all except SigKill and SigStop).
  If Act is non-nil, it is used to specify the new action.
  If OldAct is non-nil the previous action is saved there.
}

begin
 do_syscall(syscall_nr_sigaction,longint(signum),longint(act),longint(oldact));
 LinuxError:=Errno;
end;

Procedure SigProcMask(How:Integer;SSet,OldSSet:PSigSet);
{
  Change the list of currently blocked signals.
  How determines which signals will be blocked :
   SigBlock   : Add SSet to the current list of blocked signals
   SigUnBlock : Remove the signals in SSet from the list of blocked signals.
   SigSetMask : Set the list of blocked signals to SSet
  if OldSSet is non-null, the old set will be saved there.
}

begin
  do_syscall(syscall_nr_sigprocmask,longint(how),longint(sset),longint(oldsset));
 LinuxError:=Errno;
end;

Function SigPending:SigSet;
{
  Allows examination of pending signals. The signal mask of pending
  signals is set in SSet
}
Var
  dummy : Sigset;
begin
  do_syscall(syscall_nr_sigpending,longint(@dummy));
  LinuxError:=Errno;
  sigpending:=dummy;
end;

Procedure SigSuspend(Mask:Sigset);
{
 Set the signal mask with Mask, and suspend the program until a signal
 is received.
}

begin
  do_syscall(syscall_nr_sigsuspend,mask);
  LinuxError:=Errno;
end;

{
Function Signal(Signum:Integer;Handler:SignalHandler):SignalHandler;
{
  Install a new handler for signal Signum.
  The old signal handler is returned.
  This call does, in fact, the same as SigAction.
}

begin
  sr.reg2:=signum;
  sr.reg3:=longint(handler);
  Linuxerror:=SysCall(Syscall_nr_signal,sr);
  If linuxerror=Sig_Err then
   begin
     Signal:=nil;
     Linuxerror:=errno;
   end
  else
   begin
     Signal:=signalhandler(Linuxerror);
     linuxerror:=0;
   end;
end;
}

procedure SigRaise(sig:integer);
begin
  Kill(GetPid,Sig);
end;

{
Function  Alarm(Sec : Longint) : longint;

Var Sr : Syscallregs;

begin
  sr.reg2:=Sec;
  Alarm:=Syscall(syscall_nr_alarm,sr);
end;

Procedure Pause;

Var Sr : Syscallregs;

begin
  syscall(syscall_nr_pause,sr);
end;
}
{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}

Function IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}

begin
  IOCtl:=Do_Syscall(syscall_nr_ioctl,handle,ndx,longint(data))=0;
 LinuxError:=Errno;
end;



Function TCGetAttr(fd:longint;var tios:TermIOS):boolean;
begin
  TCGetAttr:=IOCtl(fd,TCGETS,@tios);
end;



Function TCSetAttr(fd:longint;OptAct:longint;var tios:TermIOS):boolean;
var
  nr:longint;
begin
  case OptAct of
   TCSANOW   : nr:=TCSETS;
   TCSADRAIN : nr:=TCSETSW;
   TCSAFLUSH : nr:=TCSETSF;
  else
   begin
     ErrNo:=Sys_EINVAL;
     TCSetAttr:=false;
     exit;
   end;
  end;
  TCSetAttr:=IOCtl(fd,nr,@Tios);
end;



Procedure CFSetISpeed(var tios:TermIOS;speed:Longint);
begin
  tios.c_cflag:=(tios.c_cflag and (not CBAUD)) or speed;
end;



Procedure CFSetOSpeed(var tios:TermIOS;speed:Longint);
begin
  CFSetISpeed(tios,speed);
end;



Procedure CFMakeRaw(var tios:TermIOS);
begin
  with tios do
   begin
     c_iflag:=c_iflag and (not (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON));
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or CS8;
   end;
end;



Function TCSendBreak(fd,duration:longint):boolean;
begin
  TCSendBreak:=IOCtl(fd,TCSBRK,pointer(duration));
end;



Function TCSetPGrp(fd,id:longint):boolean;
begin
  TCSetPGrp:=IOCtl(fd,TIOCSPGRP,pointer(id));
end;



Function TCGetPGrp(fd:longint;var id:longint):boolean;
begin
  TCGetPGrp:=IOCtl(fd,TIOCGPGRP,@id);
end;



Function TCDrain(fd:longint):boolean;
begin
  TCDrain:=IOCtl(fd,TCSBRK,pointer(1));
end;



Function TCFlow(fd,act:longint):boolean;
begin
  TCFlow:=IOCtl(fd,TCXONC,pointer(act));
end;



Function TCFlush(fd,qsel:longint):boolean;
begin
  TCFlush:=IOCtl(fd,TCFLSH,pointer(qsel));
end;



Function IsATTY(Handle:Longint):Boolean;
{
  Check if the filehandle described by 'handle' is a TTY (Terminal)
}
var
  t : Termios;
begin
 IsAtty:=TCGetAttr(Handle,t);
end;



Function IsATTY(f: text):Boolean;
{
  Idem as previous, only now for text variables.
}
begin
  IsATTY:=IsaTTY(textrec(f).handle);
end;



function TTYName(Handle:Longint):string;
{
  Return the name of the current tty described by handle f.
  returns empty string in case of an error.
}
Const
  dev='/dev';
var
  name      : string;
  st        : stat;
  mydev,
  myino     : longint;
  dirstream : pdir;
  d         : pdirent;
begin
  TTYName:='';
  fstat(handle,st);
  if (errno<>0) and isatty (handle) then
   exit;
  mydev:=st.dev;
  myino:=st.ino;
  dirstream:=opendir(dev);
  if (linuxerror<>0) then
   exit;
  d:=Readdir(dirstream);
  while (d<>nil) do
   begin
     if (d^.ino=myino) then
      begin
        name:=dev+'/'+strpas(@(d^.name));
        fstat(name,st);
        if (linuxerror=0) and (st.dev=mydev) then
         begin
           closedir(dirstream);
           ttyname:=name;
           exit;
         end;
      end;
     d:=Readdir(dirstream);
   end;
  closedir(dirstream);
end;



function TTYName(var F:Text):string;
{
  Idem as previous, only now for text variables;
}
begin
  TTYName:=TTYName(textrec(f).handle);
end;



{******************************************************************************
                             Utility calls
******************************************************************************}

Function Octal(l:longint):longint;
{
  Convert an octal specified number to decimal;
}
var
  octnr,
  oct : longint;
begin
  octnr:=0;
  oct:=0;
  while (l>0) do
   begin
     oct:=oct or ((l mod 10) shl octnr);
     l:=l div 10;
     inc(octnr,3);
   end;
  Octal:=oct;
end;



Function StringToPPChar(Var S:STring):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
}
var
  nr  : longint;
  Buf : ^char;
  p   : ppchar;
begin
  s:=s+#0;
  buf:=@s[1];
  nr:=0;
  while(buf^<>#0) do
   begin
     while (buf^ in [' ',#8,#10]) do
      inc(buf);
     inc(nr);
     while not (buf^ in [' ',#0,#8,#10]) do
      inc(buf);
   end;
  getmem(p,nr*4);
  StringToPPChar:=p;
  if p=nil then
   begin
     LinuxError:=sys_enomem;
     exit;
   end;
  buf:=@s[1];
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#8,#10]) do
      begin
        buf^:=#0;
        inc(buf);
      end;
     p^:=buf;
     inc(p);
     p^:=nil;
     while not (buf^ in [' ',#0,#8,#10]) do
      inc(buf);
   end;
end;



Function FExpand(Const Path:PathStr):PathStr;
var
  temp  : pathstr;
  i,j   : longint;
  p     : pchar;
Begin
{Remove eventual drive - doesn't exist in Linux}
  if path[2]=':' then
   i:=3
  else
   i:=1;
  temp:='';
{Replace ~/ with $HOME}
  if (path[i]='~') and ((i+1>length(path)) or (path[i+1]='/'))  then
   begin
     p:=getenv('HOME');
     if not (p=nil) then
      Insert(StrPas(p),temp,i);
     i:=1;
     temp:=temp+Copy(Path,2,255);
   end;
{Do we have an absolute path ? No - prefix the current dir}
  if temp='' then
   begin
     if path[i]<>'/' then
      begin
        {$I-}
         getdir(0,temp);
        {$I+}
        if ioresult<>0 then;
      end
     else
      inc(i);
     temp:=temp+'/'+copy(path,i,length(path)-i+1)+'/';
   end;
{First remove all references to '/./'}
  while pos('/./',temp)<>0 do
   delete(temp,pos('/./',temp),2);
{Now remove also all references to '/../' + of course previous dirs..}
  repeat
    i:=pos('/../',temp);
   {Find the pos of the previous dir}
    if i>1 then
     begin
       j:=i-1;
       while (j>1) and (temp[j]<>'/') do
        dec (j);{temp[1] is always '/'}
       delete(temp,j,i-j+3);
      end
     else
      if i=1 then               {i=1, so we have temp='/../something', just delete '/../'}
       delete(temp,1,3);
  until i=0;
  { Remove ending /.. }
  i:=pos('/..',temp);
  if (i<>0) and (i =length(temp)-2) then
    begin
    j:=i-1;
    while (j>1) and (temp[j]<>'/') do
      dec (j);
    delete (temp,j,i-j+3);
    end;
  { if last character is / then remove it - dir is also a file :-) }
  if (length(temp)>0) and (temp[length(temp)]='/') then
   dec(byte(temp[0]));
  fexpand:=temp;
End;



Function FSearch(const path:pathstr;dirlist:string):pathstr;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'
}
Var
  NewDir : PathStr;
  p1     : Longint;
  Info   : Stat;
Begin
{Replace ':' with ';'}
  for p1:=1to length(dirlist) do
   if dirlist[p1]=':' then
    dirlist[p1]:=';';
{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     Dirlist:='.;'+dirlist;{Make sure current dir is first to be searched.}
     Repeat
       p1:=Pos(';',DirList);
       If p1=0 Then
        p1:=255;
       NewDir:=Copy(DirList,1,P1 - 1);
       if NewDir[Length(NewDir)]<>'/' then
        NewDir:=NewDir+'/';
       NewDir:=NewDir+Path;
       Delete(DirList,1,p1);
       if FStat(NewDir,Info) then
        Begin
          If Pos('./',NewDir)=1 Then
           Delete(NewDir,1,2);
        {DOS strips off an initial .\}
        End
       Else
        NewDir:='';
     Until (DirList='') or (Length(NewDir) > 0);
     FSearch:=NewDir;
   End;
End;



Procedure FSplit(const Path:PathStr;Var Dir:DirStr;Var Name:NameStr;Var Ext:ExtStr);
Var
  DotPos,SlashPos,i : longint;
Begin
  SlashPos:=0;
  DotPos:=256;
  i:=Length(Path);
  While (i>0) and (SlashPos=0) Do
   Begin
     If (DotPos=256) and (Path[i]='.') Then
      DotPos:=i;
     If (Path[i]='/') Then
      SlashPos:=i;
     Dec(i);
   End;
  Ext:=Copy(Path,DotPos,255);
  Dir:=Copy(Path,1,SlashPos);
  Name:=Copy(Path,SlashPos + 1,DotPos - SlashPos - 1);
End;



Function Dirname(Const path:pathstr):pathstr;
{
  This function returns the directory part of a complete path.
  Unless the directory is root '/', The last character is not
  a slash.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if length(Dir)>1 then
   Delete(Dir,length(Dir),1);
  DirName:=Dir;
end;



Function Basename(Const path:pathstr;Const suf:pathstr):pathstr;
{
  This function returns the filename part of a complete path. If suf is
  supplied, it is cut off the filename.
}
var
  Dir  : PathStr;
  Name : NameStr;
  Ext  : ExtStr;
begin
  FSplit(Path,Dir,Name,Ext);
  if Suf<>Ext then
   Name:=Name+Ext;
  BaseName:=Name;
end;



Function FNMatch(const Pattern,Name:string):Boolean;
Var
  LenPat,LenName : longint;

  Function DoFNMatch(i,j:longint):Boolean;
  Var
    Found : boolean;
  Begin
  Found:=true;
  While Found and (i<=LenPat) Do
   Begin
     Case Pattern[i] of
      '?' : Found:=(j<=LenName);
      '*' : Begin
            {find the next character in pattern, different of ? and *}
              while Found and (i<LenPat) do
                begin
                inc(i);
                case Pattern[i] of
                  '*' : ;
                  '?' : begin
                          inc(j);
                          Found:=(j<=LenName);
                        end;
                else
                  Found:=false;
                end;
               end;
            {Now, find in name the character which i points to, if the * or ?
             wasn't the last character in the pattern, else, use up all the
             chars in name}
              Found:=true;
              if (i<=LenPat) then
                begin
                repeat
                {find a letter (not only first !) which maches pattern[i]}
                while (j<=LenName) and (name[j]<>pattern[i]) do
                  inc (j);
                 if (j<LenName) then
                  begin
                    if DoFnMatch(i+1,j+1) then
                     begin
                       i:=LenPat;
                       j:=LenName;{we can stop}
                       Found:=true;
                     end
                    else
                     inc(j);{We didn't find one, need to look further}
                  end;
               until (j>=LenName);
                end
              else
                j:=LenName;{we can stop}
            end;
     else {not a wildcard character in pattern}
       Found:=(j<=LenName) and (pattern[i]=name[j]);
     end;
     inc(i);
     inc(j);
   end;
  DoFnMatch:=Found and (j>LenName);
  end;

Begin {start FNMatch}
  LenPat:=Length(Pattern);
  LenName:=Length(Name);
  FNMatch:=DoFNMatch(1,1);
End;



Procedure Globfree(var p : pglob);
{
  Release memory occupied by pglob structure, and names in it.
  sets p to nil.
}
var
  temp : pglob;
begin
  while p<>nil do
   begin
     temp:=p^.next;
     if p^.name<>nil then
      freemem(p^.name,strlen(p^.name)+1);
     dispose(p);
     p:=temp;
   end;
end;



Function Glob(Const path:pathstr):pglob;
{
  Fills a tglob structure with entries matching path,
  and returns a pointer to it. Returns nil on error,
  linuxerror is set accordingly.
}
var
  temp     : string[255];
  thedir   : pdir;
  buffer   : pdirent;
  root,run : pglob;
begin
{ Get directory }
  if dirname(path)='' then
   temp:='.'
  else
   temp:=dirname(path);
  temp:=temp+#0;
  thedir:=opendir(@temp[1]);
  if thedir=nil then
   begin
     glob:=nil;
     linuxerror:=errno;
     exit;
   end;
  temp:=basename(path,'');{ get the pattern }
  if thedir^.fd<0 then
   begin
     linuxerror:=errno;
     glob:=nil;
     exit;
   end;
{get the entries}
  new(root);
  root^.next:=nil;
  root^.name:=nil;
  run:=root;
  repeat
    buffer:=Sys_readdir(thedir);
    if buffer<>nil then
     begin
       if fnmatch(temp,strpas(@(buffer^.name[0]))) then
        begin
        { get memory for pglob }
          new(run^.next);
          if run^.next=nil then
           begin
             linuxerror:=Sys_ENOMEM;
             globfree(root);
             glob:=nil;
             exit;
           end
          else
           begin
             run:=run^.next;
             run^.next:=nil;
           end;
        { Get memory for name }
          getmem(run^.name,strlen(@(buffer^.name[0]))+1);
          if run^.name=nil then
           begin
             linuxerror:=Sys_ENOMEM;
             globfree(root);
             glob:=nil;
             exit;
           end;
          move(buffer^.name[0],run^.name^,strlen(@(buffer^.name[0]))+1);
        end;{ if fnmatch }
     end { buffer <> nil }
    else
     begin
       run:=root;
       if root^.next<>nil then
        root:=root^.next;{ put root on first entry}
       if run<>nil then
        begin
          run^.next:=nil;
          globfree(run);
        end;
     end;
  until buffer=nil;
  if root^.name=nil then
   begin
     dispose(root);
     linuxerror:=0;
     glob:=nil;
   end
  else
   glob:=root;
end;


{--------------------------------
      FiledescriptorSets
--------------------------------}

Procedure FD_Zero(var fds:fdSet);
{
  Clear the set of filedescriptors
}
begin
  FillChar(fds,sizeof(fdSet),0);
end;

Procedure FD_Clr(fd:longint;var fds:fdSet);
{
  Remove fd from the set of filedescriptors
}
begin
  fds[fd shr 5]:=fds[fd shr 5] and (not (1 shl (fd and 31)));
end;



Procedure FD_Set(fd:longint;var fds:fdSet);
{
  Add fd to the set of filedescriptors
}
begin
  fds[fd shr 5]:=fds[fd shr 5] or (1 shl (fd and 31));
end;



Function FD_IsSet(fd:longint;var fds:fdSet):boolean;
{
  Test if fd is part of the set of filedescriptors
}
begin
  FD_IsSet:=((fds[fd shr 5] and (1 shl (fd and 31)))<>0);
end;



Function GetFS (var T:Text):longint;
{
  Get File Descriptor of a text file.
}
begin
  if textrec(t).mode=fmclosed then
   exit(-1)
  else
   GETFS:=textrec(t).Handle
end;



Function GetFS(Var F:File):longint;
{
  Get File Descriptor of an unTyped file.
}
begin
  { Handle and mode are on the same place in textrec and filerec. }
  if filerec(f).mode=fmclosed then
   exit(-1)
  else
   GETFS:=filerec(f).Handle
end;


{--------------------------------
      Stat.Mode Macro's
--------------------------------}

Function S_ISLNK(m:word):boolean;
{
  Check mode field of inode for link.
}
begin
  S_ISLNK:=(m and STAT_IFMT)=STAT_IFLNK;
end;



Function S_ISREG(m:word):boolean;
{
  Check mode field of inode for regular file.
}
begin
  S_ISREG:=(m and STAT_IFMT)=STAT_IFREG;
end;



Function S_ISDIR(m:word):boolean;

{
  Check mode field of inode for directory.
}
begin
  S_ISDIR:=(m and STAT_IFMT)=STAT_IFDIR;
end;



Function S_ISCHR(m:word):boolean;
{
  Check mode field of inode for character device.
}
begin
  S_ISCHR:=(m and STAT_IFMT)=STAT_IFCHR;
end;



Function S_ISBLK(m:word):boolean;
{
  Check mode field of inode for block device.
}
begin
  S_ISBLK:=(m and STAT_IFMT)=STAT_IFBLK;
end;



Function S_ISFIFO(m:word):boolean;
{
  Check mode field of inode for named pipe (FIFO).
}
begin
  S_ISFIFO:=(m and STAT_IFMT)=STAT_IFIFO;
end;



Function S_ISSOCK(m:word):boolean;
{
  Check mode field of inode for socket.
}
begin
  S_ISSOCK:=(m and STAT_IFMT)=STAT_IFSOCK;
end;


{--------------------------------
      Memory functions
--------------------------------}

function MMap(const m:tmmapargs):longint;

begin
  {Last argument (offset) is actually 64-bit under BSD. Therefore extra 0}
 MMap:=do_syscall(syscall_nr_mmap,m.address,m.size,m.prot,m.flags,m.fd,m.offset,0);
 LinuxError:=Errno;
end;


{--------------------------------
      Port IO functions
--------------------------------}
{
Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
{
  Set permissions on NUM ports starting with port FROM to VALUE
  this works ONLY as root.
}

Var
  Sr : Syscallregs;
begin
  Sr.Reg2:=From;
  Sr.Reg3:=Num;
  Sr.Reg4:=Value;
  IOPerm:=Syscall(Syscall_nr_ioperm,sr)=0;
  LinuxError:=Errno;
end;
}
{$IFDEF I386}

{$asmmode direct}

Procedure WritePort (Port : Longint; Value : Byte);
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl 8(%ebp),%edx
        movb 12(%ebp),%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePort (Port : Longint; Value : Word);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl 8(%ebp),%edx
        movw 12(%ebp),%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePort (Port : Longint; Value : Longint);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl 8(%ebp),%edx
        movl 12(%ebp),%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortl (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' longints from 'Buf' to Port
}
begin
  asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%esi
        movl 8(%ebp),%edx
        cld
        rep
        outsl
  end ['ECX','ESI','EDX'];
end;



Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' words from 'Buf' to Port
}
begin
  asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%esi
        movl 8(%ebp),%edx
        cld
        rep
        outsw
  end ['ECX','ESI','EDX'];
end;



Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' bytes from 'Buf' to Port
}
begin
  asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%esi
        movl 8(%ebp),%edx
        cld
        rep
        outsb
  end ['ECX','ESI','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Byte);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl 8(%ebp),%edx
        inb %dx,%al
        andl $255,%eax
        movl %eax,12(%ebp)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Word);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl 8(%ebp),%edx
        inw %dx,%ax
        andl $65535,%eax
        movl %eax,12(%ebp)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Longint);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl 8(%ebp),%edx
        inl %dx,%eax
        movl %eax,12(%ebp)
        end ['EAX','EDX'];
end;


Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' longints from port 'Port' to 'Buf'.
}
begin
  asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%edi
        movl 8(%ebp),%edx
        cld
        rep
        insl
  end ['ECX','EDI','EDX'];
end;



Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' words from port 'Port' to 'Buf'.
}
begin
  asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%edi
        movl 8(%ebp),%edx
        cld
        rep
        insw
  end ['ECX','EDI','EDX'];
end;



Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' bytes from port 'Port' to 'Buf'.
}
begin
  asm
        movl 16(%ebp),%ecx
        movl 12(%ebp),%edi
        movl 8(%ebp),%edx
        cld
        rep
        insb
  end ['ECX','EDI','EDX'];
end;
{$ENDIF}


Initialization
  InitLocalTime;

finalization
  DoneLocalTime;

End.

{
  $Log$
  Revision 1.11  2000-04-07 20:51:16  marco
   * updates.

  Revision 1.10  2000/04/05 13:46:22  marco
   * rest of syscalls has constants now

  Revision 1.9  2000/04/05 13:07:03  marco
   * replaced about half of the syscall nr's by symbols from sysnr.inc

  Revision 1.8  2000/03/17 12:58:57  marco
   * some changes to ftruncate based procs. Added a "0" as extra parameter

  Revision 1.7  2000/03/16 16:19:28  marco
   * fixes that made ppc386 -h working

  Revision 1.6  2000/03/02 15:33:20  marco
   * fixed some types and errors that needed longint(@ typecasting.

  Revision 1.5  2000/03/01 20:04:38  marco
   * some small fixes.

  Revision 1.4  2000/03/01 17:27:46  marco
   * Fixed first half of linux unit to portable syscall struct.

  Revision 1.3  2000/02/04 16:53:26  marco
   * Finished Linux (and rest syscalls) roughly. Some things still need to be
    tested, and checked (off_t calls specially)

  Revision 1.2  2000/02/04 12:05:04  marco
   * a few functions added.

  Revision 1.1  2000/02/03 17:03:36  marco
   * initial version. Ported till line +/- 2000


}