{
   $Id$
   This file is part of the Free Pascal run time library.
   Copyright (c) 1999-2000 by Michael Van Canneyt,
   BSD parts (c) 2000 by Marco van de Voort
   members of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

**********************************************************************}
Unit Unix;
Interface

Uses UnixUtil,BaseUnix;

{// i ostypes.inc}
{ Get Types and Constants }
{$i sysconst.inc}
{$i systypes.inc}

{ Get System call numbers and error-numbers}
{$i sysnr.inc}
{$i errno.inc}
{$I signal.inc}
{$i ostypes.inc}

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

const

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

{******************************************************************************
                            Procedure/Functions
******************************************************************************}

{**************************
     Time/Date Handling
***************************}

var
  tzdaylight : boolean;
  tzname     : array[boolean] of pchar;

{ timezone support }
procedure GetLocalTimezone(timer:longint;var leap_correct,leap_hit:longint);
procedure GetLocalTimezone(timer:longint);
procedure ReadTimezoneFile(fn:string);
function  GetTimezoneFile:string;

Function  GetEpochTime: longint;
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
//Procedure Execve(Path: pathstr;args:ppchar;ep:ppchar);
//Procedure Execve(Path: AnsiString;args:ppchar;ep:ppchar);
//Procedure Execve(path: pchar;args:ppchar;ep:ppchar);
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
{Clone for FreeBSD is copied from the LinuxThread port, and rfork based}
function  Clone(func:TCloneFunc;sp:pointer;flags:longint;args:pointer):longint;
Function  WaitProcess(Pid:longint):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}

Function WIFSTOPPED(Status: Integer): Boolean;
Function W_EXITCODE(ReturnCode, Signal: Integer): Integer;
Function W_STOPCODE(Signal: Integer): Integer;

{**************************
     File Handling
***************************}

Function  fdFlush (fd : Longint) : Boolean;

Function  Flock (fd,mode : longint) : boolean;
Function  Flock (var T : text;mode : longint) : boolean;
Function  Flock (var F : File;mode : longint) : boolean;

Function  StatFS(Path:Pathstr;Var Info:tstatfs):Boolean;
Function  StatFS(Fd: Longint;Var Info:tstatfs):Boolean;

Function  SelectText(var T:Text;TimeOut :PTimeVal):Longint;
Function  SelectText(var T:Text;TimeOut :Longint):Longint;

{**************************
   Directory Handling
***************************}

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

function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : longint;
function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): LongInt;

{$ifndef BSD}
Function  GetDomainName:String;
Function  GetHostName:String;
{$endif}

{**************************
  IOCtl/Termios Functions
***************************}

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

{**************************
    Utility functions
***************************}

Function  Octal(l:longint):longint;
Function  FExpand(Const Path: PathStr):PathStr;
Function  FSearch(const path:pathstr;dirlist:string):pathstr;
Function  Glob(Const path:pathstr):pglob;
Procedure Globfree(var p:pglob);
{Filedescriptorsets}
{Stat.Mode Types}
procedure SigRaise(sig:integer);
{******************************************************************************
                            Implementation
******************************************************************************}

{$i unxsysch.inc}

Implementation

Uses Strings;

{$i syscallh.inc}
{$i ossysch.inc}

{$i unxsysc.inc}

{ Get the definitions of textrec and filerec }
{$i textrec.inc}
{$i filerec.inc}

{ Raw System calls are in Syscalls.inc}
{$i syscalls.inc}

{$i unixsysc.inc}   {Syscalls only used in unit Unix/Linux}

Function getenv(name:string):Pchar; external name 'FPC_SYSC_FPGETENV';


{******************************************************************************
                          Process related calls
******************************************************************************}

{ Most calls of WaitPID do not handle the result correctly, this funktion treats errors more correctly }
Function  WaitProcess(Pid:longint):Longint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
var     r,s     : LongInt;
begin
  repeat
    s:=$7F00;
    r:=fpWaitPid(Pid,@s,0);
  until (r<>-1) or (LinuxError<>ESysEINTR);
  if (r=-1) or (r=0) then // 0 is not a valid return and should never occur (it means status invalid when using WNOHANG)
    WaitProcess:=-1 // return -1 to indicate an error
  else
   begin
{$ifndef Solaris}
     { at least correct for Linux and Darwin (JM) }
     if (s and $7F)=0 then // Only this is a valid returncode
{$else}
     if (s and $FF)=0 then // Only this is a valid returncode
{$endif}
      WaitProcess:=s shr 8
     else if (s>0) then  // Until now there is not use of the highest bit , but check this for the future
      WaitProcess:=-s // normal case
     else
      WaitProcess:=s; // s<0 should not occur, but wie return also a negativ value
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

Procedure Execv(const path: AnsiString;args:ppchar);
{
  Overloaded ansistring version.
}
begin
  fpExecVe(Path,Args,envp)
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
     Thepath:=strpas(fpgetenv('PATH'));
     if thepath='' then
      thepath:='.';
     Path:=FSearch(path,thepath)
   end
  else
   Path:='';
  if Path='' then
   linuxerror:=ESysEnoent
  else
   fpExecve(Path,args,ep);{On error linuxerror will get set there}
end;

Procedure Execv(const path:pathstr;args:ppchar);
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  the current environment is passed on.
}
begin
  fpExecve(path,args,envp); {On error linuxerror will get set there}
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
     Thepath:=strpas(fpgetenv('PATH'));
     if thepath='' then
      thepath:='.';
     Path:=FSearch(path,thepath)
   end
  else
   Path:='';
  if Path='' then
   linuxerror:=ESysEnoent
  else
   fpExecve(Path,args,ep);{On error linuxerror will get set there}
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
  fpExecVE(p^,p,EP);
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
  fpExecVE(p^,p,EP);
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
  pid:=fpfork;
  if pid=0 then // We are in the Child
   begin
     {This is the child.}
     fpExecve(p^,p,envp);
     fpExit(127);  // was Exit(127)
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
  pid:=fpfork;
  if pid=0 then // We are in the Child
   begin
     fpExecve(p^,p,envp);
     fpExit(127); // was exit(127)!! We must exit the Process, not the function
   end
  else if (pid<>-1) then // Successfull started
   Shell:=WaitProcess(pid) {Linuxerror is set there}
  else // no success
   Shell:=-1;
  FreeShellArgV(p);
end;


Function WIFSTOPPED(Status: Integer): Boolean;
begin
  WIFSTOPPED:=((Status and $FF)=$7F);
end;

Function W_EXITCODE(ReturnCode, Signal: Integer): Integer;
begin
  W_EXITCODE:=(ReturnCode shl 8) or Signal;
end;

Function W_STOPCODE(Signal: Integer): Integer;

begin
  W_STOPCODE:=(Signal shl 8) or $7F;
end;

{******************************************************************************
                       Date and Time related calls
******************************************************************************}

Function GetEpochTime: longint;
{
  Get the number of seconds since 00:00, January 1 1970, GMT
  the time NOT corrected any way
}
begin
  GetEpochTime:=fptime;
end;

procedure GetTime(var hour,min,sec,msec,usec:word);
{
  Gets the current time, adjusted to local time
}
var
  year,day,month:Word;
  tz:timeval;
begin
  fpgettimeofday(@tz,nil);
  EpochToLocal(tz.tv_sec,year,month,day,hour,min,sec);
  msec:=tz.tv_usec div 1000;
  usec:=tz.tv_usec mod 1000;
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
  EpochToLocal(fptime,year,month,day,hour,minute,second);
End;

Procedure GetDateTime(Var Year,Month,Day,hour,minute,second:Word);
{
  Gets the current date, adjusted to local time
}
Begin
  EpochToLocal(fptime,year,month,day,hour,minute,second);
End;

{$ifndef BSD}
{$ifdef linux}
Function stime (t : longint) : Boolean;
var
  sr : Syscallregs;
begin
  sr.reg2:=longint(@t);
  SysCall(Syscall_nr_stime,sr);
  linuxerror:=fpgeterrno;;
   stime:=linuxerror=0;
end;
{$endif}
{$endif}

{$ifdef BSD}
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

{ Include timezone handling routines which use /usr/share/timezone info }
{$i timezone.inc}

{******************************************************************************
                           FileSystem calls
******************************************************************************}

Procedure Execl(const Todo:Ansistring);

{
  Overloaded AnsiString Version of ExecL.
}

begin
  ExecLE(ToDo,EnvP);
end;

Function Flock (var T : text;mode : longint) : boolean;
begin
  Flock:=Flock(TextRec(T).Handle,mode);
end;


Function  Flock (var F : File;mode : longint) : boolean;
begin
  Flock:=Flock(FileRec(F).Handle,mode);
end;

Function SelectText(var T:Text;TimeOut :PTimeval):Longint;
Var
  F:TfdSet;
begin
  if textrec(t).mode=fmclosed then
   begin
     LinuxError:=ESysEBADF;
     exit(-1);
   end;
  FpFD_ZERO(f);
  fpFD_SET(textrec(T).handle,f);
  if textrec(T).mode=fminput then
   SelectText:=fpselect(textrec(T).handle+1,@f,nil,nil,TimeOut)
  else
   SelectText:=fpselect(textrec(T).handle+1,nil,@f,nil,TimeOut);
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

procedure SeekDir(p:pdir;off:longint);
begin
  if p=nil then
   begin
     fpseterrno(ESysEBADF);
     exit;
   end;
 {$ifndef bsd}
  p^.dd_nextoff:=fplseek(p^.dd_fd,off,seek_set);
 {$endif}
  p^.dd_size:=0;
  p^.dd_loc:=0;
end;

function TellDir(p:pdir):longint;
begin
  if p=nil then
   begin
     fpseterrno(ESysEBADF);
     telldir:=-1;
     exit;
   end;
  telldir:=fplseek(p^.dd_fd,0,seek_cur)
  { We could try to use the nextoff field here, but on my 1.2.13
    kernel, this gives nothing... This may have to do with
    the readdir implementation of libc... I also didn't find any trace of
    the field in the kernel code itself, So I suspect it is an artifact of libc.
    Michael. }
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
          fpwrite(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufpos);
      end;
    fminput :
      textrec(f).bufend:=fpread(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufsize);
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
  fpclose(textrec(f).handle);
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
     LinuxError:=ESysEnoent;
     exit;
   end;
  AssignPipe(pipi,pipo);
  if Linuxerror<>0 then
   exit;
  pid:=fpfork;
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
        fpdup2(pipi,input);
        close(pipi);
        if linuxerror<>0 then
         halt(127);
      end
     else
      begin
        close(pipi);
        fpdup2(pipo,output);
        close(pipo);
        if linuxerror<>0 then
         halt(127);
      end;
     pp:=createshellargv(prog);
     fpExecve(pp^,pp,envp);
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
     LinuxError:=ESysEnoent;
     exit;
   end;
  AssignPipe(pipi,pipo);
  if Linuxerror<>0 then
   exit;
  pid:=fpfork;
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
        fpdup2(filerec(pipi).handle,stdinputhandle);
        close(pipi);
        if linuxerror<>0 then
         halt(127);
      end
     else
      begin
        close(pipi);
        fpdup2(filerec(pipo).handle,stdoutputhandle);
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
     fpExecve(ansistring('/bin/sh'),pp,envp);
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
  pid:=fpfork;
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
     fpdup2(pipi,input);
     if linuxerror<>0 then
      halt(127);
     close(pipi);
     fpdup2(pipo,output);
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

  pid := fpFork;
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
    fpdup2(PipeIn, Input);
    if LinuxError <> 0 then Halt(127);
    Close(PipeIn);
    fpdup2(PipeOut, Output);
    if LinuxError <> 0 then Halt(127);
    Close(PipeOut);
    fpdup2(PipeErr, StdErr);
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

{$ifndef BSD}
Function GetDomainName:String;  { linux only!}
// domainname is a glibc extension.

{
  Get machines domain name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  fpUname(Sysn);
  linuxerror:=fpgeterrno;;
  If linuxerror<>0 then
   getdomainname:=''
  else
   getdomainname:=strpas(@Sysn.domain[0]);
end;
{$endif}

Function GetHostName:String;
{
  Get machines name. Returns empty string if not set.
}
Var
  Sysn : utsname;
begin
  fpuname(Sysn);
  linuxerror:=fpgeterrno;;
  If linuxerror<>0 then
   gethostname:=''
  else
   gethostname:=strpas(@Sysn.nodename[0]);
end;

{******************************************************************************
                          Signal handling calls
******************************************************************************}

procedure SigRaise(sig:integer);
begin
  fpKill(fpGetPid,Sig);
end;

{******************************************************************************
                         IOCtl and Termios calls
******************************************************************************}

Function TCGetAttr(fd:longint;var tios:TermIOS):boolean;
begin
 {$ifndef BSD}
  TCGetAttr:=fpIOCtl(fd,TCGETS,@tios)>0;
 {$else}
  TCGETAttr:=fpIoCtl(Fd,TIOCGETA,@tios)>0;
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
  TCSetAttr:=fpIOCtl(fd,nr,@Tios)>0;
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
  TCSendBreak:=fpIOCtl(fd,TCSBRK,pointer(duration))>0;
  {$else}
  TCSendBreak:=fpIOCtl(fd,TIOCSBRK,0)>0;
  {$endif}
end;


Function TCSetPGrp(fd,id:longint):boolean;
begin
  TCSetPGrp:=fpIOCtl(fd,TIOCSPGRP,pointer(id))>0;
end;


Function TCGetPGrp(fd:longint;var id:longint):boolean;
begin
  TCGetPGrp:=fpIOCtl(fd,TIOCGPGRP,@id)>0;
end;

Function TCDrain(fd:longint):boolean;
begin
 {$ifndef BSD}
  TCDrain:=fpIOCtl(fd,TCSBRK,pointer(1))>0;
 {$else}
  TCDrain:=fpIOCtl(fd,TIOCDRAIN,0)>0; {Should set timeout to 1 first?}
 {$endif}
end;


Function TCFlow(fd,act:longint):boolean;
begin
  {$ifndef BSD}
   TCFlow:=fpIOCtl(fd,TCXONC,pointer(act))>0;
  {$else}
    case act OF
     TCOOFF :  TCFlow:=fpIoctl(fd,TIOCSTOP,0)>0;
     TCOOn  :  TCFlow:=fpIOctl(Fd,TIOCStart,0)>0;
     TCIOFF :  {N/I}
    end;
  {$endif}
end;

Function TCFlush(fd,qsel:longint):boolean;
begin
 {$ifndef BSD}
  TCFlush:=fpIOCtl(fd,TCFLSH,pointer(qsel))>0;
 {$else}
  TCFlush:=fpIOCtl(fd,TIOCFLUSH,pointer(qsel))>0;
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
    dirstream:=fpopendir(n);
    if (linuxerror<>0) then
     exit;
    d:=fpReaddir(dirstream^);
    while (d<>nil) do
     begin
       name:=n+'/'+strpas(@(d^.d_name));
       fpstat(name,st);
       if linuxerror=0 then
        begin
          if (fpS_ISDIR(st.st_mode)) and  { if it is a directory }
             (strpas(@(d^.d_name))<>'.') and    { but not ., .. and fd subdirs }
             (strpas(@(d^.d_name))<>'..') and
             (strpas(@(d^.d_name))<>'') and
             (strpas(@(d^.d_name))<>'fd') then
           begin                      {we found a directory, search inside it}
             if mysearch(name) then
              begin                 {the device is here}
                fpclosedir(dirstream^);  {then don't continue searching}
                mysearch:=true;
                exit;
              end;
           end
          else if (d^.d_fileno=myino) and (st.st_dev=mydev) then
           begin
             fpclosedir(dirstream^);
             ttyname:=name;
             mysearch:=true;
             exit;
           end;
        end;
       d:=fpReaddir(dirstream^);
     end;
    fpclosedir(dirstream^);
    mysearch:=false;
  end;

begin
  TTYName:='';
  fpfstat(handle,st);
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


{$DEFINE FPC_FEXPAND_TILDE} { Tilde is expanded to home }
{$DEFINE FPC_FEXPAND_GETENVPCHAR} { GetEnv result is a PChar }

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
       if FpStat(NewDir,Info)>=0 then
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
  thedir:=fpopendir(@temp[1]);
  if thedir=nil then
   begin
     glob:=nil;
     linuxerror:=fpgeterrno;;
     exit;
   end;
  temp:=basename(path,''); { get the pattern }
  if thedir^.dd_fd<0 then
   begin
     linuxerror:=fpgeterrno;;
     glob:=nil;
     exit;
   end;
{get the entries}
  root:=nil;
  current:=nil;
  repeat
    buffer:=fpreaddir(thedir^);
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
  fpclosedir(thedir^);
  glob:=root;
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


Initialization
  InitLocalTime;

finalization
  DoneLocalTime;
End.

{
  $Log$
  Revision 1.39  2003-09-27 12:51:33  peter
    * fpISxxx macros renamed to C compliant fpS_ISxxx

  Revision 1.38  2003/09/20 12:38:29  marco
   * FCL now compiles for FreeBSD with new 1.1. Now Linux.

  Revision 1.37  2003/09/17 19:07:44  marco
   * more fixes for Unix<->unixutil

  Revision 1.36  2003/09/17 17:30:46  marco
   * Introduction of unixutil

  Revision 1.35  2003/09/16 21:46:27  marco
   * small fixes, checking things on linux

  Revision 1.34  2003/09/16 20:52:24  marco
   * small cleanups. Mostly killing of already commented code in unix etc

  Revision 1.33  2003/09/16 16:13:56  marco
   * fdset functions renamed to fp<posix name>

  Revision 1.32  2003/09/15 20:08:49  marco
   * small fixes. FreeBSD now cycles

  Revision 1.31  2003/09/14 20:15:01  marco
   * Unix reform stage two. Remove all calls from Unix that exist in Baseunix.

  Revision 1.30  2003/07/08 21:23:24  peter
    * sparc fixes

  Revision 1.29  2003/05/30 19:58:40  marco
   * Getting NetBSD/i386 to compile.

  Revision 1.28  2003/05/29 19:16:16  marco
   * fixed a small *BSD gotcha

  Revision 1.27  2003/05/24 20:39:54  jonas
    * fixed ExitCode translation in WaitProcess for Linux and Darwin (and
      probably other BSD's as well)

  Revision 1.26  2003/03/11 08:27:59  michael
  * stringtoppchar should use tabs instead of backspace as delimiter

  Revision 1.25  2002/12/18 16:50:39  marco
   * Unix RTL generic parts. Linux working, *BSD will follow shortly

  Revision 1.24  2002/09/07 16:01:28  peter
    * old logs removed and tabs fixed

  Revision 1.23  2002/08/06 13:30:46  sg
  * replaced some Longints with Cardinals, to mach the C headers
  * updated the termios record

  Revision 1.22  2002/03/05 20:04:25  michael
  + Patch from Sebastian for FCNTL call

  Revision 1.21  2002/01/02 12:22:54  marco
   * Removed ifdef arround getepoch.

}
