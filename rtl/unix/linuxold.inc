{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   The old 1.0.x linux unit fixed to work under 1.1.x. 
   At the moment the errorhandling can be different.
   Probably it won't be supported for anything else then FreeBSD
   and Linux/i386. 

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}

// unit clause is linux or oldlinux depending on compiler version
{$ifdef VER1_0}
unit linux;
{$else}
unit oldlinux;
{$endif}

Interface

Uses BaseUnix,Unix;

{ Get Types and Constants }
{$i sysconst.inc}
//{$i systypes.inc}

{$i ptypes.inc}
{$i ostypes.inc}
{$i sysnr.inc}
{$i syscallh.inc}

{ Get System call numbers and error-numbers}
{$i errno.inc}
{$I signal.inc}

{$IFNDEF VER1_0}		// ugly voodoo. Sacrifice a chicken and hope
				// that it works.
Type stat=baseunix.tstat;
{$ELSE}
type
{$I stat.inc}
{$ENDIF}


var
  LinuxError : Longint;


{********************
      Process
********************}

const
  { For getting/setting priority }
  Prio_Process = 0;
  Prio_PGrp    = 1;
  Prio_User    = 2;

{$ifdef Solaris}
  WNOHANG   = $100;
  WUNTRACED = $4;
{$ELSE}
  WNOHANG   = $1;
  WUNTRACED = $2;
  __WCLONE  = $80000000;
{$ENDIF}


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
{$ifdef Solaris}
  F_DupFd  = 0;
  F_Dup2Fd = 9;
  F_GetOwn = 23;
  F_SetOwn = 24;
  F_GetLk  = 14;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_FreeSp = 11;
{$else}
  F_GetLk  = 5;
  F_SetLk  = 6;
  F_SetLkW = 7;
  F_SetOwn = 8;
  F_GetOwn = 9;
{$endif}

{********************
   IOCtl(TermIOS)
********************}

{Is too freebsd/Linux specific}

{$I termios.inc}

{********************
      Info
********************}

Type
{
  UTimBuf = packed record{in BSD array[0..1] of timeval, but this is
                                backwards compatible with linux version}
    actime,
    {$ifdef BSD}
    uactime,            {BSD Micro seconds}
    {$endif}
    modtime
    {$ifdef BSD}
         ,
    umodtime             {BSD Micro seconds}
    {$endif}
         : longint;
  end;
}
  UTimeBuf=UTimBuf;
  TUTimeBuf=UTimeBuf;
  PUTimeBuf=^UTimeBuf;


{******************************************************************************
                            Procedure/Functions
******************************************************************************}

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
Function  GetEpochTime: longint;
Procedure EpochToLocal(epoch:longint;var year,month,day,hour,minute,second:Word);
Function  LocalToEpoch(year,month,day,hour,minute,second:Word):Longint;
procedure GetTime(var hour,min,sec,msec,usec:word);
procedure GetTime(var hour,min,sec,sec100:word);
procedure GetTime(var hour,min,sec:word);
Procedure GetDate(Var Year,Month,Day:Word);
Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);
function SetTime(Hour,Min,Sec:word) : Boolean;
function SetDate(Year,Month,Day:Word) : Boolean;
function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;

{**************************
     Process Handling
***************************}

function  CreateShellArgV(const prog:string):ppchar;
function  CreateShellArgV(const prog:Ansistring):ppchar;
procedure FreeShellArgV(p:ppchar);
Procedure Execve(Path: pathstr;args:ppchar;ep:ppchar);
Procedure Execve(Path: AnsiString;args:ppchar;ep:ppchar);
Procedure Execve(path: pchar;args:ppchar;ep:ppchar);
Procedure Execv(const path:pathstr;args:ppchar);
Procedure Execv(const path: AnsiString;args:ppchar);
Procedure Execvp(Path: Pathstr;Args:ppchar;Ep:ppchar);
Procedure Execvp(Path: AnsiString; Args:ppchar;Ep:ppchar);
Procedure Execl(const Todo: String);
Procedure Execl(const Todo: Ansistring);
Procedure Execle(Todo: String;Ep:ppchar);
Procedure Execle(Todo: AnsiString;Ep:ppchar);
Procedure Execlp(Todo: string;Ep:ppchar);
Procedure Execlp(Todo: Ansistring;Ep:ppchar);
Function  Shell(const Command:String):Longint;
Function  Shell(const Command:AnsiString):Longint;
Function  Fork:longint;
{Clone for FreeBSD is copied from the LinuxThread port, and rfork based}
//function  Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
Procedure ExitProcess(val:longint);
Function  WaitPid(Pid:longint;Status:pointer;Options:Longint):Longint;  {=>PID (Status Valid), 0 (No Status), -1: Error, special case errno=EINTR }
Function  WaitProcess(Pid:longint):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
Procedure Nice(N:integer);
Function  GetPriority(Which,Who:longint):longint;
procedure SetPriority(Which,Who,What:longint);

Function  GetPid:LongInt;
Function  GetPPid:LongInt;
Function  GetUid:Longint;
Function  GetEUid:Longint;
Function  GetGid:Longint;
Function  GetEGid:Longint;

{$ifdef solaris}
// Set the real userid/groupid (uid/gid from calling process)
Function SetUid(aUID:Longint):Boolean;
Function SetGid(aGID:Longint):Boolean;

// Set the real and effective userid/groupid (like setuid/setgid bit in file permissions)
function SetreUid(aRealUID,aEffUid:Longint):Boolean; overload;
function SetreUid(aUID:Longint):Boolean;overload;

function SetreGid(aRealGid,aEffGid:Longint):Boolean; overload;
function SetreGid(aGid:Longint):Boolean;overload;
{$endif}

{**************************
     File Handling
***************************}

Function  fdOpen(pathname:string;flags:longint):longint;
Function  fdOpen(pathname:string;flags,mode:longint):longint;
Function  fdOpen(pathname:pchar;flags:longint):longint;
Function  fdOpen(pathname:pchar;flags,mode:longint):longint;
Function  fdClose(fd:longint):boolean;
Function  fdRead(fd:longint;var buf;tsize:longint):longint;
Function  fdWrite(fd:longint;const buf;size:longint):longint;
Function  fdTruncate(fd,size:longint):boolean;
Function  fdSeek (fd,pos,seektype :longint): longint;
Function  fdFlush (fd : Longint) : Boolean;
Function  Link(OldPath,NewPath:pathstr):boolean;
Function  SymLink(OldPath,NewPath:pathstr):boolean;
Function  ReadLink(name,linkname:pchar;maxlen:longint):longint;
Function  ReadLink(name:pathstr):pathstr;
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
Function  StatFS(Path:Pathstr;Var Info:tstatfs):Boolean;
Function  StatFS(Fd: Longint;Var Info:tstatfs):Boolean;
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
Function  Select(N:longint;readfds,writefds,exceptfds:pfdset;TimeOut:PTimeVal):longint;
Function  Select(N:longint;readfds,writefds,exceptfds:pfdset;TimeOut:Longint):longint;
Function  SelectText(var T:Text;TimeOut :PTimeVal):Longint;
Function  SelectText(var T:Text;TimeOut :Longint):Longint;

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

function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : longint;
function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): LongInt;

{**************************
    General information
***************************}

Function  GetEnv(P:string):Pchar;

{$ifndef BSD}
Function  GetDomainName:String;
Function  GetHostName:String;
Function  Uname(var unamerec:utsname):Boolean;
{$endif}
{**************************
        Signal
***************************}

Procedure SigAction(Signum:longint;Act,OldAct:baseunix.PSigActionRec );
Procedure SigProcMask (How:longint;SSet,OldSSet:PSigSet);
Function  SigPending:SigSet;
Procedure SigSuspend(Mask:Sigset);
Function  Signal(Signum:longint;Handler:SignalHandler):SignalHandler;
Function  Kill(Pid:longint;Sig:longint):integer;
Procedure SigRaise(Sig:integer);
{$ifndef BSD}
Function  Alarm(Sec : Longint) : longint;
Procedure Pause;
{$endif}
Function NanoSleep(const req : timespec;var rem : timespec) : longint;

{**************************
  IOCtl/Termios Functions
***************************}

Function  IOCtl(Handle,Ndx: Longint;Data: Pointer):boolean;
Function  TCGetAttr(fd:longint;var tios:TermIOS):boolean;
Function  TCSetAttr(fd:longint;OptAct:longint;const tios:TermIOS):boolean;
Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal);
Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal);
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
(* the consts are System-dependend, not checked for solaris *)
const
  PROT_READ  = $1;             { page can be read }
  PROT_WRITE = $2;             { page can be written }
  PROT_EXEC  = $4;             { page can be executed }
  PROT_NONE  = $0;             { page can not be accessed }

  MAP_SHARED    = $1;          { Share changes }
//  MAP_PRIVATE   = $2;          { Changes are private }
  MAP_TYPE      = $f;          { Mask for type of mapping }
  MAP_FIXED     = $10;         { Interpret addr exactly }
//  MAP_ANONYMOUS = $20;         { don't use a file }

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

function MMap(const m:tmmapargs):pointer;
function MUnMap (P : Pointer; Size : Longint) : Boolean;

{**************************
     Port IO functions
***************************}

//Function  IOperm (From,Num : Cardinal; Value : Longint) : boolean;
//Function  IoPL(Level : longint) : Boolean;
{$ifdef i386}
Procedure WritePort (Port : Longint; Value : Byte);
Procedure WritePort (Port : Longint; Value : Word);
Procedure WritePort (Port : Longint; Value : Longint);
Procedure WritePortB (Port : Longint; Value : Byte);
Procedure WritePortW (Port : Longint; Value : Word);
Procedure WritePortL (Port : Longint; Value : Longint);
Procedure WritePortL (Port : Longint; Var Buf; Count: longint);
Procedure WritePortW (Port : Longint; Var Buf; Count: longint);
Procedure WritePortB (Port : Longint; Var Buf; Count: longint);
Procedure ReadPort (Port : Longint; Var Value : Byte);
Procedure ReadPort (Port : Longint; Var Value : Word);
Procedure ReadPort (Port : Longint; Var Value : Longint);
function  ReadPortB (Port : Longint): Byte;
function  ReadPortW (Port : Longint): Word;
function  ReadPortL (Port : Longint): LongInt;
Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortW (Port : Longint; Var Buf; Count: longint);
Procedure ReadPortB (Port : Longint; Var Buf; Count: longint);
{$endif}

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
Function  StringToPPChar(Var S:String):ppchar;
Function  StringToPPChar(Var S:AnsiString):ppchar;
Function  StringToPPChar(S : Pchar):ppchar;
Function  GetFS(var T:Text):longint;
Function  GetFS(Var F:File):longint;
{Filedescriptorsets}
Procedure FD_Zero(var fds:tfdset);
Procedure FD_Clr(fd:longint;var fds:tfdset);
Procedure FD_Set(fd:longint;var fds:tfdset);
Function  FD_IsSet(fd:longint;var fds:tfdset):boolean;
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

{$i ossysch.inc}


{ Get the definitions of textrec and filerec }
{$i textrec.inc}
{$i filerec.inc}

{ Raw System calls are in Syscalls.inc}
{$DEFINE FROMLINUXUNIT}
{$i syscalls.inc}

{$i liunsysc.inc}   {Syscalls only used in unit Unix/Linux}

{$ifndef Linux}
Function FpNanoSleep(const req : timespec;rem : ptimespec) : longint; external name 'FPC_SYSC_NANOSLEEP';
{$endif}

{******************************************************************************
                          Process related calls
******************************************************************************}

{ Most calls of WaitPID do not handle the result correctly, this funktion treats errors more correctly }
Function  WaitProcess(Pid:longint):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
var     r,s     : LongInt;
begin
  repeat
    s:=$7F00;
    r:=WaitPid(Pid,@s,0);
  until (r<>-1) or (LinuxError<>ESysEINTR);
  if (r=-1) or (r=0) then // 0 is not a valid return and should never occur (it means status invalid when using WNOHANG)
    WaitProcess:=-1 // return -1 to indicate an error
  else
   begin
{$ifndef Solaris}
     WaitProcess:=s; // s<0 should not occur, but wie return also a negativ value
{$else}
     if (s and $FF)=0 then // Only this is a valid returncode
      WaitProcess:=s shr 8
     else if (s>0) then  // Until now there is not use of the highest bit , but check this for the future
      WaitProcess:=-s // normal case
     else
      WaitProcess:=s; // s<0 should not occur, but wie return also a negativ value
{$endif}
   end;
end;

function InternalCreateShellArgV(cmd:pChar; len:longint):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
}
const   Shell   = '/bin/sh'#0'-c'#0;
var
  pp,p : ppchar;
//  temp : string; !! Never pass a local var back!!
begin
  getmem(pp,4*4);
  p:=pp;
  p^:=@Shell[1];
  inc(p);
  p^:=@Shell[9];
  inc(p);
  getmem(p^,len+1);
  move(cmd^,p^^,len);
  pchar(p^)[len]:=#0;
  inc(p);
  p^:=Nil;
  InternalCreateShellArgV:=pp;
end;

function CreateShellArgV(const prog:string):ppchar;
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog));
end;

function CreateShellArgV(const prog:Ansistring):ppchar;
{
  Create an argv which executes a command in a shell using /bin/sh -c
  using a AnsiString;
}
begin
  CreateShellArgV:=InternalCreateShellArgV(@prog[1],length(prog)); // if ppc works like delphi this also work when @prog[1] is invalid (len=0)
end;


procedure FreeShellArgV(p:ppchar);
begin
  if (p<>nil) then begin
    freemem(p[2]);
    freemem(p);
   end;
end;


Procedure Execve(Path: AnsiString;args:ppchar;ep:ppchar);
{
  overloaded ansistring version.
}
begin
  ExecVE(PChar(Path),args,ep);
end;

Procedure Execv(const path: AnsiString;args:ppchar);
{
  Overloaded ansistring version.
}
begin
  ExecVe(Path,Args,envp)
end;

Procedure Execvp(Path: AnsiString; Args:ppchar;Ep:ppchar);
{
  Overloaded ansistring version
}
var
  thepath : Ansistring;
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
   linuxerror:=ESysenoent
  else
   Execve(Path,args,ep);{On error linuxerror will get set there}
end;

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
   linuxerror:=ESysenoent
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


Procedure Execle(Todo:AnsiString;Ep:ppchar);
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


Procedure Execlp(Todo: Ansistring;Ep:ppchar);
{
  Overloaded ansistring version.
}
var
  p : ppchar;
begin
  p:=StringToPPchar(todo);
  if (p=nil) or (p^=nil) then
   exit;
  ExecVP(StrPas(p^),p,EP);
end;


Function Shell(const Command:String):Longint;
{
  Executes the shell, and passes it the string Command. (Through /bin/sh -c)
  The current environment is passed to the shell.
  It waits for the shell to exit, and returns its exit status.
  If the Exec call failed exit status 127 is reported.
}
{ Changed the structure:
- the previous version returns an undefinied value if fork fails
- it returns the status of Waitpid instead of the Process returnvalue (see the doc to Shell)
- it uses exit(127) not ExitProc (The Result in pp386: going on Compiling in 2 processes!)
- ShellArgs are now released
- The Old CreateShellArg gives back pointers to a local var
}
var
  p      : ppchar;
  pid    : longint;
begin
  p:=CreateShellArgv(command);
  pid:=fork;
  if pid=0 then // We are in the Child
   begin
     {This is the child.}
     Execve(p^,p,envp);
     ExitProcess(127);  // was Exit(127)
   end
  else if (pid<>-1) then // Successfull started
   Shell:=WaitProcess(pid) {Linuxerror is set there}
  else // no success
   Shell:=-1; // indicate an error
  FreeShellArgV(p);
end;


Function Shell(const Command:AnsiString):Longint;
{
  AnsiString version of Shell
}
var
  p     : ppchar;
  pid   : longint;
begin { Changes as above }
  p:=CreateShellArgv(command);
  pid:=fork;
  if pid=0 then // We are in the Child
   begin
     Execve(p^,p,envp);
     ExitProcess(127); // was exit(127)!! We must exit the Process, not the function
   end
  else if (pid<>-1) then // Successfull started
   Shell:=WaitProcess(pid) {Linuxerror is set there}
  else // no success
   Shell:=-1;
  FreeShellArgV(p);
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

Function GetEpochTime: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  GetEpochTime:=GetTimeOfDay;
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
  Epoch:=Abs(Epoch Mod 86400);
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
  EpochToLocal(t.tv_sec,year,month,day,hour,min,sec);
  msec:=t.tv_usec div 1000;
  usec:=t.tv_usec mod 1000;
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

{$ifndef BSD}     {Start 1.0.x compiler FreeBSD cycle defines linux}
{$ifdef Linux}
Function stime (t : longint) : Boolean;
var
  sr : Syscallregs;
begin
  sr.reg2:=longint(@t);
  SysCall(Syscall_nr_stime,sr);
  linuxerror:=fpgeterrno;
   stime:=linuxerror=0;
end;
{$endif}
{$endif}

{$ifdef bsd}
Function stime (t : longint) : Boolean;
begin

end;
{$endif}

Function SetTime(Hour,Min,Sec:word) : boolean;
var
  Year, Month, Day : Word;
begin
  GetDate (Year, Month, Day);
  SetTime:=stime ( LocalToEpoch ( Year, Month, Day, Hour, Min, Sec ) );
end;

Function SetDate(Year,Month,Day:Word) : boolean;
var
  Hour, Minute, Second, Sec100 : Word;
begin
  GetTime ( Hour, Minute, Second, Sec100 );
  SetDate:=stime ( LocalToEpoch ( Year, Month, Day, Hour, Minute, Second ) );
end;

Function SetDateTime(Year,Month,Day,hour,minute,second:Word) : Boolean;

begin
  SetDateTime:=stime ( LocalToEpoch ( Year, Month, Day, Hour, Minute, Second ) );
end;

Procedure Execl(const Todo:Ansistring);

{
  Overloaded AnsiString Version of ExecL.
}

begin
  ExecLE(ToDo,EnvP);
end;

{ Include timezone handling routines which use /usr/share/timezone info }
{$i timezone.inc}

Function NanoSleep(const req : timespec;var rem : timespec) : longint;

begin
 nanosleep:=fpnanosleep(req,@rem);
end;

Function  OpenDir(f:pchar):pdir;

begin
   OpenDir:=fpopendir(f);
  linuxerror:=fpgeterrno;
end;

function  CloseDir(p:pdir):integer;

begin
  CloseDir:=Fpclosedir(p^);
  linuxerror:=fpgeterrno;
end;

Procedure SigAction(Signum:longint;Act:baseunix.psigactionrec;OldAct:baseunix.PSigActionRec );

begin
   fpsigaction(signum,act,oldact);
  linuxerror:=fpgeterrno;
end;


Function  sys_ReadDir(p:pdir):pdirent;

begin
  sys_readdir:=fpreaddir(p^);
  linuxerror:=fpgeterrno;
end;

function sys_unlink(p:pchar):longint;

begin
  sys_unlink:=fpunlink(p);
  linuxerror:=fpgeterrno;
end;

function sys_rename(p,p2:pchar):longint;

begin
  sys_rename:=fprename(p,p2);
  linuxerror:=fpgeterrno;
end;

Function sys_SymLink(OldPath,newPath:pchar):longint;

begin
  sys_Symlink:=fpsymlink(oldpath,newpath);
  linuxerror:=fpgeterrno;
end;


Function sys_ReadLink(name,linkname:pchar;maxlen:longint):longint;
{
  Read a link (where it points to)
}
begin
  sys_Readlink:=fpreadlink(Name,LinkName,maxlen);
  linuxerror:=fpgeterrno;
end;

function sys_open(p:pchar;f,i:longint):longint;

begin
  sys_open:=fpopen(p,f,i);
  linuxerror:=fpgeterrno;
end;

Function Sys_Lseek(F:longint;Off:longint;Whence:longint):longint;
begin
 sys_lseek:=fplseek(f,off,whence);
 linuxerror:=fpgeterrno;
end;

Function Sys_Stat(Filename:pchar;var Buffer: stat):longint;

begin
 Sys_stat:=fpstat(filename,buffer); 
 linuxerror:=fpgeterrno;
end;

function sys_close(f:longint):longint;

begin
 sys_close:=fpclose(f);
 linuxerror:=fpgeterrno;
end;

function sys_read(f:longint;p:pchar;i:longint):longint;

begin
 sys_read:=fpread(f,p,i);
 linuxerror:=fpgeterrno;
end;

function sys_write(f:longint;p:pchar;i:longint):longint;

begin
 sys_write:=fpwrite(f,p,i);
 linuxerror:=fpgeterrno;
end;


Function  Select(N:longint;readfds,writefds,exceptfds:pfdset;TimeOut:PTimeVal):longint;

begin
   select:=fpSelect(N,readfds,writefds,exceptfds,TimeOut^.tv_sec);
   linuxerror:=fpgeterrno;
end;

{******************************************************************************
                           FileSystem calls
******************************************************************************}

Function fdOpen(pathname:string;flags:longint):longint;
begin
  pathname:=pathname+#0;
  fdOpen:=Sys_Open(@pathname[1],flags,438);
end;


Function fdOpen(pathname:string;flags,mode:longint):longint;
begin
  pathname:=pathname+#0;
  fdOpen:=Sys_Open(@pathname[1],flags,mode);
end;



Function  fdOpen(pathname:pchar;flags:longint):longint;
begin
  fdOpen:=Sys_Open(pathname,flags,0);
end;



Function  fdOpen(pathname:pchar;flags,mode:longint):longint;
begin
  fdOpen:=Sys_Open(pathname,flags,mode);
end;



Function fdClose(fd:longint):boolean;
begin
  fdClose:=(Sys_Close(fd)=0);
end;



Function fdRead(fd:longint;var buf;tsize:longint):longint;
begin
  fdRead:=Sys_Read(fd,pchar(@buf),tsize);
end;



Function fdWrite(fd:longint;const buf;size:longint):longint;
begin
  fdWrite:=Sys_Write(fd,pchar(@buf),size);
end;




Function  fdSeek (fd,pos,seektype :longint): longint;
{
  Do a Seek on a file descriptor fd to position pos, starting from seektype

}
begin
   fdseek:=Sys_LSeek (fd,pos,seektype);
   linuxerror:=fpgeterrno;
end;

{$ifdef xBSD}
Function Fcntl(Fd:longint;Cmd:longint):longint;
{
  Read or manipulate a file.(See also fcntl (2) )
  Possible values for Cmd are :
    F_GetFd,F_GetFl,F_GetOwn
  Errors are reported in Linuxerror;
  If Cmd is different from the allowed values, linuxerror=ESyseninval.
}

begin
  if (cmd in [F_GetFd,F_GetFl,F_GetOwn]) then
   begin
     Linuxerror:=fpfcntl(fd,cmd,0);
     if linuxerror=-1 then
      begin
        linuxerror:=fpgeterrno;
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
     linuxerror:=ESyseinval;
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
     fpfcntl(fd,cmd,arg);
     linuxerror:=fpgeterrno;
   end
  else
   linuxerror:=ESyseinval;
end;
{$endif}


Function Fcntl(var Fd:Text;Cmd:longint):longint;
begin
  Fcntl := Fcntl(textrec(Fd).handle, Cmd);
end;

Procedure Fcntl(var Fd:Text;Cmd,Arg:Longint);

begin
  Fcntl(textrec(Fd).handle, Cmd, Arg);
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
  linuxerror:=fpgeterrno;
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

Function SymLink(OldPath,newPath:pathstr):boolean;
{
  Proceduces a soft link from new to old.
}
begin
  oldpath:=oldpath+#0;
  newpath:=newpath+#0;
  Symlink:=Sys_symlink(pchar(@(oldpath[1])),pchar(@(newpath[1])))=0;
  linuxerror:=fpgeterrno;
end;


Function ReadLink(name,linkname:pchar;maxlen:longint):longint;
{
  Read a link (where it points to)
}
begin
  Readlink:=Sys_readlink(Name,LinkName,maxlen);
  linuxerror:=fpgeterrno;
end;


Function ReadLink(Name:pathstr):pathstr;
{
  Read a link (where it points to)
}
var
  LinkName : pathstr;
  i : longint;
begin
  Name:=Name+#0;
  i:=ReadLink(@Name[1],@LinkName[1],high(linkname));
  if i>0 then
   begin
     linkname[0]:=chr(i);
     ReadLink:=LinkName;
   end
  else
   ReadLink:='';
end;


Function UnLink(Path:pathstr):boolean;
{
  Removes the file in 'Path' (that is, it decreases the link count with one.
  if the link count is zero, the file is removed from the disk.
}
begin
  path:=path+#0;
  Unlink:=Sys_unlink(pchar(@(path[1])))=0;
  linuxerror:=fpgeterrno;
end;


Function  UnLink(Path:pchar):Boolean;
{
  Removes the file in 'Path' (that is, it decreases the link count with one.
  if the link count is zero, the file is removed from the disk.
}
begin
  Unlink:=(Sys_unlink(path)=0);
  linuxerror:=fpgeterrno;
end;


Function  FRename (OldName,NewName : Pchar) : Boolean;
begin
  FRename:=Sys_rename(OldName,NewName)=0;
  linuxerror:=fpgeterrno;
end;


Function  FRename (OldName,NewName : String) : Boolean;
begin
  OldName:=OldName+#0;
  NewName:=NewName+#0;
  FRename:=FRename (@OldName[1],@NewName[1]);
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

Function  Select(N:longint;readfds,writefds,exceptfds:pfdset;TimeOut:Longint):longint;
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
     tv.tv_Sec:=Timeout div 1000;
     tv.tv_Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  Select:=Select(N,Readfds,WriteFds,ExceptFds,p);
end;



Function SelectText(var T:Text;TimeOut :PTimeval):Longint;
Var
  F:tfdset;
begin
  if textrec(t).mode=fmclosed then
   begin
     LinuxError:=ESysEBadf;
     exit(-1);
   end;
  FD_Zero(f);
  FD_Set(textrec(T).handle,f);
  if textrec(T).mode=fminput then
   SelectText:=select(textrec(T).handle+1,@f,nil,nil,TimeOut)
  else
   SelectText:=select(textrec(T).handle+1,nil,@f,nil,TimeOut);
end;


Function SelectText(var T:Text;TimeOut :Longint):Longint;
var
  p  : PTimeVal;
  tv : TimeVal;
begin
  if TimeOut=-1 then
   p:=nil
  else
   begin
     tv.tv_Sec:=Timeout div 1000;
     tv.tv_Usec:=(Timeout mod 1000)*1000;
     p:=@tv;
   end;
  SelectText:=SelectText(T,p);
end;


{******************************************************************************
                               Directory
******************************************************************************}

Function OpenDir(F:String):PDir;
begin
  F:=F+#0;
  OpenDir:=OpenDir(@F[1]);
  linuxerror:=fpgeterrno;
end;


procedure SeekDir(p:pdir;off:longint);
begin
  if p=nil then
   begin
     fpseterrno(ESysEBadf);
     exit;
   end;
 {$ifndef bsd}                         
  {$ifndef Solaris}
  p^.dd_nextoff:=Sys_lseek(p^.dd_fd,off,seek_set);
  {$endif}
 {$endif}
  p^.dd_size:=0;
  p^.dd_loc:=0;
end;


function TellDir(p:pdir):longint;
begin
  if p=nil then
   begin
     fpseterrno(ESysEBadf);
     telldir:=-1;
     exit;
   end;
  telldir:=Sys_lseek(p^.dd_fd,0,seek_cur)
  { We could try to use the nextoff field here, but on my 1.2.13
    kernel, this gives nothing... This may have to do with
    the readdir implementation of libc... I also didn't find any trace of
    the field in the kernel code itself, So I suspect it is an artifact of libc.
    Michael. }
end;



Function ReadDir(P:pdir):pdirent;
begin
  ReadDir:=Sys_ReadDir(p);
  linuxerror:=fpgeterrno;
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
     LinuxError:=ESysENOENT;
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
     LinuxError:=ESysENOENT;
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


Function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : longint;
{
  Starts the program in 'Prog' and makes its input and output the
  other end of two pipes, which are the stdin and stdout of a program
  specified in 'Prog'.
  streamout can be used to write to the program, streamin can be used to read
  the output of the program. See the following diagram :
  Parent          Child
  STreamout -->  Input
  Streamin  <--  Output
  Return value is the process ID of the process being spawned, or -1 in case of failure.
}
var
  pipi,
  pipo : text;
  pid  : longint;
  pl   : ^Longint;
begin
  LinuxError:=0;
  AssignStream:=-1;
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
     AssignStream:=Pid;
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


{$ifndef bsd}
Function GetDomainName:String;
{
  Get machines domain name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  Uname(Sysn);
  linuxerror:=fpgeterrno;
  If linuxerror<>0 then
   getdomainname:=''
  else
   getdomainname:=strpas(@Sysn.domain[0]);
end;



Function GetHostName:String;
{
  Get machines name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  uname(Sysn);
  linuxerror:=fpgeterrno;
  If linuxerror<>0 then
   gethostname:=''
  else
   gethostname:=strpas(@Sysn.nodename[0]);
end;
{$endif}

{******************************************************************************
                          Signal handling calls
******************************************************************************}

procedure SigRaise(sig:integer);
begin
  Kill(GetPid,Sig);
end;


{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}


Function TCGetAttr(fd:longint;var tios:TermIOS):boolean;
begin
 {$ifndef BSD}
  TCGetAttr:=IOCtl(fd,TCGETS,@tios);
 {$else}
  TCGETAttr:=IoCtl(Fd,TIOCGETA,@tios);
 {$endif}
end;



Function TCSetAttr(fd:longint;OptAct:longint;const tios:TermIOS):boolean;
var
  nr:longint;
begin
 {$ifndef BSD}
  case OptAct of
   TCSANOW   : nr:=TCSETS;
   TCSADRAIN : nr:=TCSETSW;
   TCSAFLUSH : nr:=TCSETSF;
 {$else}
  case OptAct of
   TCSANOW   : nr:=TIOCSETA;
   TCSADRAIN : nr:=TIOCSETAW;
   TCSAFLUSH : nr:=TIOCSETAF;
  {$endif}
  else
   begin
     fpsetErrNo(ESysEINVAL);
     TCSetAttr:=false;
     exit;
   end;
  end;
  TCSetAttr:=IOCtl(fd,nr,@Tios);
end;



Procedure CFSetISpeed(var tios:TermIOS;speed:Cardinal);
begin
 {$ifndef BSD}
  tios.c_cflag:=(tios.c_cflag and (not CBAUD)) or speed;
 {$else}
  tios.c_ispeed:=speed; {Probably the Bxxxx speed constants}
 {$endif}
end;



Procedure CFSetOSpeed(var tios:TermIOS;speed:Cardinal);
begin
  {$ifndef BSD}
   CFSetISpeed(tios,speed);
  {$else}
   tios.c_ospeed:=speed;
  {$endif}
end;




Procedure CFMakeRaw(var tios:TermIOS);
begin
 {$ifndef BSD}
  with tios do
   begin
     c_iflag:=c_iflag and (not (IGNBRK or BRKINT or PARMRK or ISTRIP or
                                INLCR or IGNCR or ICRNL or IXON));
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHONL or ICANON or ISIG or IEXTEN));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or CS8;
   end;
 {$else}
  with tios do
   begin
     c_iflag:=c_iflag and (not (IMAXBEL or IXOFF or INPCK or BRKINT or
                PARMRK or ISTRIP or INLCR or IGNCR or ICRNL or IXON or
                IGNPAR));
     c_iflag:=c_iflag OR IGNBRK;
     c_oflag:=c_oflag and (not OPOST);
     c_lflag:=c_lflag and (not (ECHO or ECHOE or ECHOK or ECHONL or ICANON or
                                ISIG or IEXTEN or NOFLSH or TOSTOP or PENDIN));
     c_cflag:=(c_cflag and (not (CSIZE or PARENB))) or (CS8 OR cread);
     c_cc[VMIN]:=1;
     c_cc[VTIME]:=0;
   end;
 {$endif}
end;


Function TCSendBreak(fd,duration:longint):boolean;
begin
  {$ifndef BSD}
  TCSendBreak:=IOCtl(fd,TCSBRK,pointer(duration));
  {$else}
  TCSendBreak:=IOCtl(fd,TIOCSBRK,0);
  {$endif}
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
 {$ifndef BSD}
  TCDrain:=IOCtl(fd,TCSBRK,pointer(1));
 {$else}
  TCDrain:=IOCtl(fd,TIOCDRAIN,0); {Should set timeout to 1 first?}
 {$endif}
end;



Function TCFlow(fd,act:longint):boolean;
begin
  {$ifndef BSD}
   TCFlow:=IOCtl(fd,TCXONC,pointer(act));
  {$else}
    case act OF
     TCOOFF :  TCFlow:=Ioctl(fd,TIOCSTOP,0);
     TCOOn  :  TCFlow:=IOctl(Fd,TIOCStart,0);
     TCIOFF :  {N/I}
    end;
  {$endif}
end;


Function TCFlush(fd,qsel:longint):boolean;
begin
 {$ifndef BSD}
  TCFlush:=IOCtl(fd,TCFLSH,pointer(qsel));
 {$else}
  TCFlush:=IOCtl(fd,TIOCFLUSH,pointer(qsel));
 {$endif}
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
var
  mydev,
  myino     : longint;
  st        : stat;

  function mysearch(n:string): boolean;
  {searches recursively for the device in the directory given by n,
    returns true if found and sets the name of the device in ttyname}
  var dirstream : pdir;
      d         : pdirent;
      name      : string;
      st        : stat;
  begin
    dirstream:=opendir(n);
    if (linuxerror<>0) then
     exit;
    d:=Readdir(dirstream);
    while (d<>nil) do
     begin
       name:=n+'/'+strpas(@(d^.d_name));
       fstat(name,st);
       if linuxerror=0 then
        begin
          if ((st.st_mode and $E000)=$4000) and  { if it is a directory }
             (strpas(@(d^.d_name))<>'.') and    { but not ., .. and fd subdirs }
             (strpas(@(d^.d_name))<>'..') and
             (strpas(@(d^.d_name))<>'') and
             (strpas(@(d^.d_name))<>'fd') then
           begin                      {we found a directory, search inside it}
             if mysearch(name) then
              begin                 {the device is here}
                closedir(dirstream);  {then don't continue searching}
                mysearch:=true;
                exit;
              end;
           end
          else if (d^.d_fileno=myino) and (st.st_dev=mydev) then
           begin
             closedir(dirstream);
             ttyname:=name;
             mysearch:=true;
             exit;
           end;
        end;
       d:=Readdir(dirstream);
     end;
    closedir(dirstream);
    mysearch:=false;
  end;

begin
  TTYName:='';
  fstat(handle,st);
  if (fpgeterrno<>0) and isatty (handle) then
   exit;
  mydev:=st.st_dev;
  myino:=st.st_ino;
  mysearch('/dev');
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

Function StringToPPChar(S: PChar):ppchar;
var
  nr  : longint;
  Buf : ^char;
  p   : ppchar;

begin
  buf:=s;
  nr:=0;
  while(buf^<>#0) do
   begin
     while (buf^ in [' ',#9,#10]) do
      inc(buf);
     inc(nr);
     while not (buf^ in [' ',#0,#9,#10]) do
      inc(buf);
   end;
  getmem(p,nr*4+1);
  StringToPPChar:=p;
  if p=nil then
   begin
     LinuxError:=ESysENOMEM;
     exit;
   end;
  buf:=s;
  while (buf^<>#0) do
   begin
     while (buf^ in [' ',#9,#10]) do
      begin
        buf^:=#0;
        inc(buf);
      end;
     p^:=buf;
     inc(p);
     p^:=nil;
     while not (buf^ in [' ',#0,#9,#10]) do
      inc(buf);
   end;
end;


{
function FExpand (const Path: PathStr): PathStr;
- declared in fexpand.inc
}

{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

const
  LFNSupport = true;
  FileNameCaseSensitive = true;

{$I fexpand.inc}

{$UNDEF FPC_FEXPAND_GETENVPCHAR}
{$UNDEF FPC_FEXPAND_TILDE}


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
      begin
        DotPos:=i;
      end;
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

Function StringToPPChar(Var S:String):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
  Note that the string S is destroyed by this call.
}

begin
  S:=S+#0;
  StringToPPChar:=StringToPPChar(@S[1]);
end;

Function StringToPPChar(Var S:AnsiString):ppchar;
{
  Create a PPChar to structure of pchars which are the arguments specified
  in the string S. Especially usefull for creating an ArgV for Exec-calls
}

begin
  StringToPPChar:=StringToPPChar(PChar(S));
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
  while assigned(p) do
   begin
     temp:=p^.next;
     if assigned(p^.name) then
      freemem(p^.name);
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
  temp,
  temp2   : string[255];
  thedir  : pdir;
  buffer  : pdirent;
  root,
  current : pglob;
begin
{ Get directory }
  temp:=dirname(path);
  if temp='' then
   temp:='.';
  temp:=temp+#0;
  thedir:=opendir(@temp[1]);
  if thedir=nil then
   begin
     glob:=nil;
     linuxerror:=fpgeterrno;
     exit;
   end;
  temp:=basename(path,''); { get the pattern }
  if thedir^.dd_fd<0 then
   begin
     linuxerror:=fpgeterrno;
     glob:=nil;
     exit;
   end;
{get the entries}
  root:=nil;
  current:=nil;
  repeat
    buffer:=Sys_readdir(thedir);
    if buffer=nil then
     break;
    temp2:=strpas(@(buffer^.d_name[0]));
    if fnmatch(temp,temp2) then
     begin
       if root=nil then
        begin
          new(root);
          current:=root;
        end
       else
        begin
          new(current^.next);
          current:=current^.next;
        end;
       if current=nil then
        begin
          linuxerror:=ESysENOMEM;
          globfree(root);
          break;
        end;
       current^.next:=nil;
       getmem(current^.name,length(temp2)+1);
       if current^.name=nil then
        begin
          linuxerror:=ESysENOMEM;
          globfree(root);
          break;
        end;
       move(buffer^.d_name[0],current^.name^,length(temp2)+1);
     end;
  until false;
  closedir(thedir);
  glob:=root;
end;


{--------------------------------
      FiledescriptorSets
--------------------------------}

Procedure FD_Zero(var fds:tfdset);
{
  Clear the set of filedescriptors
}
begin
  FillChar(fds,sizeof(tfdset),0);
end;



Procedure FD_Clr(fd:longint;var fds:tfdset);
{
  Remove fd from the set of filedescriptors
}
begin
  fds[fd shr 5]:=fds[fd shr 5] and (not (1 shl (fd and 31)));
end;



Procedure FD_Set(fd:longint;var fds:tfdset);
{
  Add fd to the set of filedescriptors
}
begin
  fds[fd shr 5]:=fds[fd shr 5] or (1 shl (fd and 31));
end;



Function FD_IsSet(fd:longint;var fds:tfdset):boolean;
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

{$IFDEF I386}
Procedure WritePort (Port : Longint; Value : Byte);
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl port,%edx
        movb value,%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePort (Port : Longint; Value : Word);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePort (Port : Longint; Value : Longint);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movl value,%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;


Procedure WritePortB (Port : Longint; Value : Byte);
{
  Writes 'Value' to port 'Port'
}
begin
        asm
        movl port,%edx
        movb value,%al
        outb %al,%dx
        end ['EAX','EDX'];
end;

Procedure WritePortW (Port : Longint; Value : Word);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movw value,%ax
        outw %ax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortL (Port : Longint; Value : Longint);
{
  Writes 'Value' to port 'Port'
}

begin
        asm
        movl port,%edx
        movl value,%eax
        outl %eax,%dx
        end ['EAX','EDX'];
end;



Procedure WritePortl (Port : Longint; Var Buf; Count: longint);
{
  Writes 'Count' longints from 'Buf' to Port
}
begin
  asm
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
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
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
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
        movl count,%ecx
        movl buf,%esi
        movl port,%edx
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
        movl port,%edx
        inb %dx,%al
        movl value,%edx
        movb %al,(%edx)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Word);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inw %dx,%ax
        movl value,%edx
        movw %ax,(%edx)
        end ['EAX','EDX'];
end;



Procedure ReadPort (Port : Longint; Var Value : Longint);
{
  Reads 'Value' from port 'Port'
}
begin
        asm
        movl port,%edx
        inl %dx,%eax
        movl value,%edx
        movl %eax,(%edx)
        end ['EAX','EDX'];
end;



function ReadPortB (Port : Longint): Byte; assembler;
{
  Reads a byte from port 'Port'
}

asm
  xorl %eax,%eax
  movl port,%edx
  inb %dx,%al
end ['EAX','EDX'];



function ReadPortW (Port : Longint): Word; assembler;
{
  Reads a word from port 'Port'
}
asm
  xorl %eax,%eax
  movl port,%edx
  inw %dx,%ax
end ['EAX','EDX'];



function ReadPortL (Port : Longint): LongInt; assembler;
{
  Reads a LongInt from port 'Port'
}
asm
  movl port,%edx
  inl %dx,%eax
end ['EAX','EDX'];



Procedure ReadPortL (Port : Longint; Var Buf; Count: longint);
{
  Reads 'Count' longints from port 'Port' to 'Buf'.
}
begin
  asm
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
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
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
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
        movl count,%ecx
        movl buf,%edi
        movl port,%edx
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

end.
{
  $Log$
  Revision 1.4  2003-09-16 15:39:31  marco
   * voodoo....

  Revision 1.3  2003/09/15 21:07:41  marco
   * second round of linux fixes. oldlinux now works

  Revision 1.2  2003/09/15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.1  2003/09/15 14:12:17  marco
   * moved linux.pp contents to linuxold etc

  Revision 1.31  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.30  2003/05/30 19:58:40  marco
   * Getting NetBSD/i386 to compile.

  Revision 1.29  2003/05/30 19:37:14  marco
   * small changes

  Revision 1.28  2003/03/11 08:26:50  michael
  * stringtoppchar should use tabs instead of backspace as delimiter

  Revision 1.27  2003/01/05 19:11:32  marco
   * small changes originating from introduction of Baseunix to FreeBSD

  Revision 1.26  2002/12/18 17:52:07  peter
    * replaced some Fp with Sys_ to get cycle working

  Revision 1.25  2002/12/18 16:50:39  marco
   * Unix RTL generic parts. Linux working, *BSD will follow shortly

  Revision 1.24  2002/09/13 13:03:27  jonas
    * fixed buffer overflow error in StringToPPChar(), detected using
      DIOTA (http://www.elis/rug.ac.be/~ronsse/diota) (which I also work on :)
      (merged)

  Revision 1.23  2002/09/07 16:01:27  peter
    * old logs removed and tabs fixed

  Revision 1.22  2002/08/06 13:30:46  sg
  * replaced some Longints with Cardinals, to mach the C headers
  * updated the termios record

  Revision 1.21  2002/03/06 11:34:04  michael
  + Forgot to patch linux.pp

  Revision 1.20  2002/01/02 12:22:54  marco
   * Removed ifdef arround getepoch.

}
