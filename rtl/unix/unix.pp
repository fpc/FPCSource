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

{ Get Types and Constants }
{$i sysconst.inc}
{$ifndef FPC_USE_LIBC}
{$i systypes.inc}
{$endif FPC_USE_LIBC}

{Get error numbers, some more signal definitions and other OS dependant
 types (that are not POSIX) }
{i errno.inc}
{$I signal.inc}
{$i ostypes.inc}

{********************
      File
********************}

Const
  P_IN  = 1;                    // pipes (?)
  P_OUT = 2;

Const
  LOCK_SH = 1;                  // flock constants ?
  LOCK_EX = 2;
  LOCK_UN = 8;
  LOCK_NB = 4;

Type
  Tpipe = baseunix.tfildes;	// compability.

  pglob = ^tglob;
  tglob = record
    name : pchar;
    next : pglob;
  end;


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
procedure GetLocalTimezone(timer:cint;var leap_correct,leap_hit:cint);
procedure GetLocalTimezone(timer:cint);
procedure ReadTimezoneFile(fn:string);
function  GetTimezoneFile:string;

Function  GetEpochTime: cint;
procedure GetTime     (var hour,min,sec,msec,usec:word);
procedure GetTime     (var hour,min,sec,sec100:word);
procedure GetTime     (var hour,min,sec:word);
Procedure GetDate     (Var Year,Month,Day:Word);
Procedure GetDateTime (Var Year,Month,Day,hour,minute,second:Word);
function  SetTime     (Hour,Min,Sec:word) : Boolean;
function  SetDate     (Year,Month,Day:Word) : Boolean;
function  SetDateTime (Year,Month,Day,hour,minute,second:Word) : Boolean;

{**************************
     Process Handling
***************************}

function CreateShellArgV (const prog:string):ppchar;
function CreateShellArgV (const prog:Ansistring):ppchar;

// These are superceded by the fpExec functions that are more pascallike
// and have less limitations. However I'll leave them in for a while, to
// not frustrate things too much
// but they might not make it to 2.0
Function Execv   (const path:pathstr;args:ppchar):cint;          {$ifndef ver1_0}deprecated; {$endif}
Function Execv   (const path: AnsiString;args:ppchar):cint;  	 {$ifndef ver1_0}deprecated; {$endif}
Function Execvp  (Path: Pathstr;Args:ppchar;Ep:ppchar):cint;     {$ifndef ver1_0}deprecated; {$endif}
Function Execvp  (Path: AnsiString; Args:ppchar;Ep:ppchar):cint; {$ifndef ver1_0}deprecated; {$endif}
Function Execl   (const Todo: String):cint;			 {$ifndef ver1_0}deprecated; {$endif}
Function Execl   (const Todo: Ansistring):cint;			 {$ifndef ver1_0}deprecated; {$endif}
Function Execle  (Todo: String;Ep:ppchar):cint;			 {$ifndef ver1_0}deprecated; {$endif}           
Function Execle  (Todo: AnsiString;Ep:ppchar):cint;		 {$ifndef ver1_0}deprecated; {$endif}
Function Execlp  (Todo: string;Ep:ppchar):cint;			 {$ifndef ver1_0}deprecated; {$endif}
Function Execlp  (Todo: Ansistring;Ep:ppchar):cint;		 {$ifndef ver1_0}deprecated; {$endif}

//
// These are much better, in nearly all ways.
//

function FpExecLE (Const PathName:AnsiString;const S:Array Of AnsiString;MyEnv:ppchar):cint;
function FpExecL(Const PathName:AnsiString;const S:Array Of AnsiString):cint;
function FpExecLP(Const PathName:AnsiString;const S:Array Of AnsiString):cint;
function FpExecV(Const PathName:AnsiString;args:ppchar):cint;
function FpExecVP(Const PathName:AnsiString;args:ppchar):cint;
function FpExecVPE(Const PathName:AnsiString;args,env:ppchar):cint;

Function Shell   (const Command:String):cint;
Function Shell   (const Command:AnsiString):cint;

Function WaitProcess (Pid:cint):cint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}

Function WIFSTOPPED (Status: Integer): Boolean;
Function W_EXITCODE (ReturnCode, Signal: Integer): Integer;
Function W_STOPCODE (Signal: Integer): Integer;

{**************************
     File Handling
***************************}

{$ifndef FPC_USE_LIBC} // defined using cdecl for libc.
Function  fsync (fd : cint) : cint;
Function  Flock   (fd,mode : cint)   : cint ;
Function  fStatFS (Fd: cint;Var Info:tstatfs):cint;
Function  StatFS  (Path:pchar;Var Info:tstatfs):cint; 
{$endif}

Function  Flock   (var T : text;mode : cint) : cint;
Function  Flock   (var F : File;mode : cint) : cint;


Function  SelectText (var T:Text;TimeOut :PTimeVal):cint;
Function  SelectText (var T:Text;TimeOut :cint):cint;

{**************************
   Directory Handling
***************************}

procedure SeekDir(p:pdir;loc:clong);
function  TellDir(p:pdir):clong;

{**************************
    Pipe/Fifo/Stream
***************************}

Function AssignPipe  (var pipe_in,pipe_out:cint):cint;
Function AssignPipe  (var pipe_in,pipe_out:text):cint;
Function AssignPipe  (var pipe_in,pipe_out:file):cint;
//Function PClose      (Var F:text) : cint;
//Function PClose      (Var F:file) : cint;
Function POpen       (var F:text;const Prog:String;rw:char):cint;
Function POpen       (var F:file;const Prog:String;rw:char):cint;
function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : cint;
function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String): cint;

{$ifndef BSD}
Function  GetDomainName:String;
Function  GetHostName:String;
{$endif}


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

{$ifdef Linux}
  MAP_GROWSDOWN  = $100;       { stack-like segment }
  MAP_DENYWRITE  = $800;       { ETXTBSY }
  MAP_EXECUTABLE = $1000;      { mark it as an executable }
  MAP_LOCKED     = $2000;      { pages are locked }
  MAP_NORESERVE  = $4000;      { don't check for reservations }
{$else}
  {$ifdef FreeBSD}
  // FreeBSD defines MAP_COPY=MAP_PRIVATE=$2;
  MAP_FILE         = $0000;  { map from file (default) }
  MAP_ANON         = $1000;  { allocated from memory, swap space }

  MAP_RENAME       = $0020; { Sun: rename private pages to file }
  MAP_NORESERVE    = $0040; { Sun: don't reserve needed swap area }
  MAP_INHERIT      = $0080; { region is retained after exec }
  MAP_NOEXTEND     = $0100; { for MAP_FILE, don't change file size }
  MAP_HASSEMAPHORE = $0200; { region may contain semaphores }
  MAP_STACK        = $0400; { region grows down, like a stack }
  MAP_NOSYNC       = $0800; { page to but do not sync underlying file}
  MAP_NOCORE       = $20000;{ dont include these pages in a coredump}
  {$endif}
{$endif}
{**************************
    Utility functions
***************************}

Type
	TFSearchOptions = (NoCurrentDirectory,
		           CurrentDirectoryFirst,
	                   CurrentDirectoryLast);

Function  FExpand  (Const Path: PathStr):PathStr;
Function  FSearch  (const path:pathstr;dirlist:string):pathstr;

Function  FSearch  (const path:AnsiString;dirlist:Ansistring;AddCurrentPath:TFSearchOptions):AnsiString;
Function  FSearch  (const path:AnsiString;dirlist:AnsiString):AnsiString;
Function  Glob     (Const path:pathstr):pglob;
Procedure Globfree (var p:pglob);

procedure SigRaise (sig:integer);

{$ifdef FPC_USE_LIBC}
const clib = 'c';
{$i unxdeclh.inc}
{$else}
{$i unxsysch.inc} //  calls used in system and not reexported from baseunix
{$endif}

{******************************************************************************
                            Implementation
******************************************************************************}

{$i unxovlh.inc}

Implementation


Uses Strings{$ifndef FPC_USE_LIBC},Syscall{$endif};

{$i unxovl.inc}

{$ifndef FPC_USE_LIBC}
{$i syscallh.inc}
{$i ossysch.inc}
{$i unxsysc.inc}
{$endif}

{ Get the definitions of textrec and filerec }
{$i textrec.inc}
{$i filerec.inc}

{$ifndef FPC_USE_LIBC}
{ Raw System calls are in Syscalls.inc}
{$ifdef Linux}                  // Linux has more "oldlinux" compability.
{$i sysc11.inc}
{$else}
{$i syscalls.inc}
{$endif}

{$endif}

{$i unixsysc.inc}   {Has platform specific libc part under ifdef, besides
			syscalls}

Function getenv(name:string):Pchar; external name 'FPC_SYSC_FPGETENV';

{******************************************************************************
                          Process related calls
******************************************************************************}

{ Most calls of WaitPID do not handle the result correctly, this funktion treats errors more correctly }
Function  WaitProcess(Pid:cint):cint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
var     ret,r,s     : cint;
begin
  s:=$7F00;

  repeat
    r:=fpWaitPid(Pid,@s,0);
    if (r=-1) and (fpgeterrno=ESysEIntr) Then
      r:=0;
  until (r<>0);
  if (r=-1) or (r=0) then // 0 is not a valid return and should never occur (it means status invalid when using WNOHANG)
    WaitProcess:=-1 // return -1 to indicate an error.  fpwaitpid updated it.
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

function InternalCreateShellArgV(cmd:pChar; len:cint):ppchar;
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

Function Execv(const path: AnsiString;args:ppchar):cint;
{
  Overloaded ansistring version.
}
begin
  Execv:=fpExecVe(Path,Args,envp);
end;

Function Execvp(Path: AnsiString; Args:ppchar;Ep:ppchar):cint;
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
   Begin
     fpsetErrno(ESysEnoEnt);
     Exit(-1);
   end
  else
   Execvp:=fpExecve(Path,args,ep);
end;

Function Execv(const path:pathstr;args:ppchar):cint;
{
  Replaces the current program by the program specified in path,
  arguments in args are passed to Execve.
  the current environment is passed on.
}
begin
  Execv:=fpExecve(path,args,envp);
end;

Function Execvp(Path:Pathstr;Args:ppchar;Ep:ppchar):cint;
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
   Begin
     fpsetErrno(ESysEnoEnt);
     Exit(-1);
   end
  else
   execvp:=fpExecve(Path,args,ep);
end;

Function Execle(Todo:string;Ep:ppchar):cint;
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
   Begin
     fpsetErrno(ESysEnoEnt);
     Exit(-1);
   end
  else
    execle:=fpExecVE(p^,p,EP);
end;

function Execle(Todo:AnsiString;Ep:ppchar):cint;
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
   Begin
     fpsetErrno(ESysEnoEnt);
     Exit(-1);
   end;
  ExecLe:=fpExecVE(p^,p,EP);
end;

Function Execl(const Todo:string):cint;
{
  This procedure takes the string 'Todo', parses it for command and
  command options, and Executes the command with the given options.
  The string 'Todo' shoud be of the form 'command options', options
  separated by commas.
  the PATH environment is not searched for 'command'.
  The current environment is passed on to command
}
begin
  Execl:=ExecLE(ToDo,EnvP);
end;

Function Execlp(Todo:string;Ep:ppchar):cint;
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
   Begin
     fpsetErrno(ESysEnoEnt);
     Exit(-1);
   end;
  Execlp:=ExecVP(StrPas(p^),p,EP);
end;

Function Execlp(Todo: Ansistring;Ep:ppchar):cint;
{
  Overloaded ansistring version.
}
var
  p : ppchar;
begin
  p:=StringToPPchar(todo);
  if (p=nil) or (p^=nil) then
   Begin
     fpsetErrno(ESysEnoEnt);
     Exit(-1);
   end;
  execlp:=ExecVP(StrPas(p^),p,EP);
end;

function ArrayStringToPPchar(const S:Array of AnsiString;reserveentries:Longint):ppchar; // const ?
// Extra allocate reserveentries pchar's at the beginning (default param=0 after 1.0.x ?)
// Note: for internal use by skilled programmers only
// if "s" goes out of scope in the parent procedure, the pointer is dangling.

var p   : ppchar;
    Res,
    i   : LongInt;
begin
  if High(s)<Low(s) Then Exit(NIL);
  Getmem(p,sizeof(pchar)*(high(s)-low(s)+ReserveEntries+2));  // one more for NIL, one more
					      // for cmd
  if p=nil then
    begin
      {$ifdef xunix}
      fpseterrno(ESysEnomem);
      {$endif}
      exit(NIL);
    end;
  for i:=low(s) to high(s) do
     p[i+Reserveentries]:=pchar(s[i]);
  p[high(s)+1+Reserveentries]:=nil; 
  ArrayStringToPPchar:=p;
end;

function intFpExecVEMaybeP (Const PathName:AnsiString;Args,MyEnv:ppchar;SearchPath:Boolean):cint;
// does an ExecVE, but still has to handle P
// execv variants call this directly, execl variants indirectly via
//     intfpexecl

Var 
  NewCmd  : ansistring;
  ThePath : AnsiString;
  Error   : cint;
  NrParam : longint;

Begin
  If SearchPath and (pos('/',pathname)=0) Then
    Begin
      // The above could be better. (check if not escaped/quoted '/' 's) ?
      // (Jilles says this is ok)
      // Stevens says only search if newcmd contains no '/'
      // fsearch is not ansistring clean yet.
      ThePath:=fpgetenv('PATH');
      if thepath='' then
        thepath:='.';     // FreeBSD uses _PATH_DEFPATH = /usr/bin:/bin
			  // but a quick check showed that _PATH_DEFPATH 
			  // varied from OS to OS
			
      newcmd:=FSearch(pathname,thepath,NoCurrentDirectory);
      // FreeBSD libc keeps on trying till a file is successfully run.
      // Stevens says "try each path prefix"
	
      // execp puts newcmd here.
        args^:=pchar(newcmd);
   End;
 // repeat
//	if searchpath then args^:=pchar(commandtorun)

  IntFpExecVEMaybeP:=fpExecVE(Args^,Args,MyEnv);
{ 
// Code that if exec fails due to permissions, tries to run it with sh 
// Should we deallocate p on fail? -> no fpexit is run no matter what
// 
}
// if intfpexecvemaybep=-1 then zoekvolgende file.
// until (Goexit) or SearchExit;


{
 If IntFpExec=-1 Then
    Begin
      Error:=fpGetErrno
      Case Error of
        ESysE2Big  : Exit(-1);
	ESysELoop,
	  : Exit(-1);	

}
end;

function intFpExecl (Const PathName:AnsiString;const s:array of ansistring;MyEnv:ppchar;SearchPath:Boolean):cint;
{ Handles the array of ansistring -> ppchar conversion. 
  Base for the the "l" variants.
}
var p:ppchar;

begin
  If PathName='' Then 
    Begin
      fpsetErrno(ESysEnoEnt);
      Exit(-1);			// Errno?
    End;
  p:=ArrayStringToPPchar(s,1);
  if p=NIL Then
    Begin
      GetMem(p,2*sizeof(pchar));
      if p=nil then
       begin
        {$ifdef xunix}
          fpseterrno(ESysEnoMem);
        {$endif}
         fpseterrno(ESysEnoEnt);
         exit(-1);
       end;
      p[1]:=nil;
    End;
  p^:=pchar(PathName);
  IntFPExecL:=intFpExecVEMaybeP(PathName,p,MyEnv,SearchPath);
end;

function FpExecLE (Const PathName:AnsiString;const S:Array Of AnsiString;MyEnv:ppchar):cint;

Begin
  FpExecLE:=intFPExecl(PathName,s,MyEnv,false);
End;

function FpExecL(Const PathName:AnsiString;const S:Array Of AnsiString):cint;

Begin
  FpExecL:=intFPExecl(PathName,S,EnvP,false);
End;

function FpExecLP(Const PathName:AnsiString;const S:Array Of AnsiString):cint;

Begin
  FpExecLP:=intFPExecl(PathName,S,EnvP,True);
End;

function FpExecV(Const PathName:AnsiString;args:ppchar):cint;

Begin
 fpexecV:=intFpExecVEMaybeP (PathName,args,envp,false);
End;

function FpExecVP(Const PathName:AnsiString;args:ppchar):cint;

Begin
 fpexecVP:=intFpExecVEMaybeP (PathName,args,envp,true);
End;

function FpExecVPE(Const PathName:AnsiString;args,env:ppchar):cint;

Begin
 fpexecVPE:=intFpExecVEMaybeP (PathName,args,env,true);
End;

// exect and execvP (ExecCapitalP) are not implement
// Non POSIX anyway.  
// Exect turns on tracing for the process
// execvP has the searchpath as array of ansistring ( const char *search_path)

Function Shell(const Command:String):cint;
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
  pid    : cint;
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
   Shell:=WaitProcess(pid)
  else // no success
   Shell:=-1; // indicate an error
  FreeShellArgV(p);
end;

Function Shell(const Command:AnsiString):cint;
{
  AnsiString version of Shell
}
var
{$ifndef FPC_USE_FPEXEC}
  p     : ppchar;
{$endif}
  pid   : cint;
begin { Changes as above }
{$ifndef FPC_USE_FPEXEC}
  p:=CreateShellArgv(command);
{$endif}
  pid:=fpfork;
  if pid=0 then // We are in the Child
   begin
    {$ifdef FPC_USE_FPEXEC}
      fpexecl('/bin/sh',['-c',Command]);	
    {$else}
     fpExecve(p^,p,envp);
    {$endif}   
     fpExit(127); // was exit(127)!! We must exit the Process, not the function
   end
  else if (pid<>-1) then // Successfull started
   Shell:=WaitProcess(pid)
  else // no success
   Shell:=-1;
 {$ifndef FPC_USE_FPEXEC}
  FreeShellArgV(p);
 {$ENDIF}
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

Function GetEpochTime: cint;
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

{$ifndef BSD}			// this can be done nicer, but I still have
				// to think about what to do with this func.
	
{$ifdef linux}
{$ifdef FPC_USE_LIBC}
function intstime (t:ptime_t):cint; external name 'stime';
{$endif}

Function stime (t : cint) : boolean;
begin
  {$ifdef FPC_USE_LIBC}
   stime:=intstime(@t)=0;
  {$else}
   stime:=do_SysCall(Syscall_nr_stime,cint(@t))=0;
  {$endif}
end;
{$endif}
{$endif}

{$ifdef BSD}
Function stime (t : cint) : Boolean;
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

Function Execl(const Todo:Ansistring):cint;

{
  Overloaded AnsiString Version of ExecL.
}

begin
  Execl:=ExecLE(ToDo,EnvP);
end;

Function Flock (var T : text;mode : cint) : cint;
begin
  Flock:=Flock(TextRec(T).Handle,mode);
end;


Function  Flock (var F : File;mode : cint) :cint;
begin
  Flock:=Flock(FileRec(F).Handle,mode);
end;

Function SelectText(var T:Text;TimeOut :PTimeval):cint;
Var
  F:TfdSet;
begin
  if textrec(t).mode=fmclosed then
   begin
     fpseterrno(ESysEBADF);
     exit(-1);
   end;
  FpFD_ZERO(f);
  fpFD_SET(textrec(T).handle,f);
  if textrec(T).mode=fminput then
   SelectText:=fpselect(textrec(T).handle+1,@f,nil,nil,TimeOut)
  else
   SelectText:=fpselect(textrec(T).handle+1,nil,@f,nil,TimeOut);
end;

Function SelectText(var T:Text;TimeOut :cint):cint;
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

procedure SeekDir(p:pdir;loc:clong);
begin
  if p=nil then
   begin
     fpseterrno(ESysEBADF);
     exit;
   end;
 {$ifndef bsd}
  p^.dd_nextoff:=fplseek(p^.dd_fd,loc,seek_set);
 {$endif}
  p^.dd_size:=0;
  p^.dd_loc:=0;
end;

function TellDir(p:pdir):clong;
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

Function IOPipe(var F:text):cint;
begin
  IOPipe:=0;
  case textrec(f).mode of
    fmoutput :
      begin
        { first check if we need something to write, else we may
          get a SigPipe when Close() is called (PFV) }
        if textrec(f).bufpos>0 then
          IOPipe:=fpwrite(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufpos);
      end;
    fminput : Begin
                textrec(f).bufend:=fpread(textrec(f).handle,pchar(textrec(f).bufptr),textrec(f).bufsize);
                IOPipe:=textrec(f).bufend;
              End;
  end;
  textrec(f).bufpos:=0;
end;

Function FlushPipe(var F:Text):cint;
begin
  FlushPipe:=0;
  if (textrec(f).mode=fmoutput) and (textrec(f).bufpos<>0) then
   FlushPipe:=IOPipe(f);
  textrec(f).bufpos:=0;
end;

Function ClosePipe(var F:text):cint;
begin
  textrec(f).mode:=fmclosed;
  ClosePipe:=fpclose(textrec(f).handle);
end;


Function AssignPipe(var pipe_in,pipe_out:text):cint;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
}
var
  f_in,f_out : cint;
begin
  if AssignPipe(f_in,f_out)=-1 then
     exit(-1);
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
  AssignPipe:=0;
end;

Function AssignPipe(var pipe_in,pipe_out:file):cint;
{
  Sets up a pair of file variables, which act as a pipe. The first one can
  be read from, the second one can be written to.
  If the operation was unsuccesful,
}
var
  f_in,f_out : cint;
begin
  if AssignPipe(f_in,f_out)=-1 then
     exit(-1);
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
  AssignPipe:=0;
end;


Function PCloseText(Var F:text):cint;
{
  May not use @PClose due overloading
}
begin
  PCloseText:=PClose(f);
end;


function POpen(var F:text;const Prog:String;rw:char):cint;
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
  pid  : pid_t;
  pl   : ^cint;
  pp   : ppchar;
  ret  : cint;
begin
  rw:=upcase(rw);
  if not (rw in ['R','W']) then
   begin
     FpSetErrno(ESysEnoent);
     exit(-1);
   end;
  if AssignPipe(pipi,pipo)=-1 Then
    Exit(-1);
  pid:=fpfork;
  if pid=-1 then
   begin
     close(pipi);
     close(pipo);
     exit(-1);
   end;
  if pid=0 then
   begin
   { We're in the child }
     if rw='W' then
      begin
        close(pipo);
        ret:=fpdup2(pipi,input);
        close(pipi);
        if ret=-1 then
         halt(127);
      end
     else
      begin
        close(pipi);
        ret:=fpdup2(pipo,output);
        close(pipo);
        if ret=-1 then
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
 ret:=0;
end;

Function POpen(var F:file;const Prog:String;rw:char):cint;
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
  pid  : cint;
  pl   : ^cint;
  p,pp : ppchar;
  temp : string[255];
  ret  : cint;
begin
  rw:=upcase(rw);
  if not (rw in ['R','W']) then
   begin
     FpSetErrno(ESysEnoent);
     exit(-1);
   end;
  ret:=AssignPipe(pipi,pipo);
  if ret=-1 then
   exit(-1);
  pid:=fpfork;
  if pid=-1 then
   begin
     close(pipi);
     close(pipo);
     exit(-1);
   end;
  if pid=0 then
   begin
   { We're in the child }
     if rw='W' then
      begin
        close(pipo);
        ret:=fpdup2(filerec(pipi).handle,stdinputhandle);
        close(pipi);
        if ret=-1 then
         halt(127);
      end
     else
      begin
        close(pipi);
        ret:=fpdup2(filerec(pipo).handle,stdoutputhandle);
        close(pipo);
        if ret=1 then
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
 POpen:=0;
end;

Function AssignStream(Var StreamIn,Streamout:text;Const Prog:String) : cint;
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
  pid  : cint;
  pl   : ^cint;
begin
  AssignStream:=-1;
  if AssignPipe(streamin,pipo)=-1 Then
   exit(-1);
  if AssignPipe(pipi,streamout)=-1 Then // shouldn't this close streamin and pipo?
   exit(-1);
  pid:=fpfork;
  if pid=-1 then
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
     if fpdup2(pipi,input)=-1 Then
      halt(127);
     close(pipi);
     If fpdup2(pipo,output)=-1 Then
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

function AssignStream(var StreamIn, StreamOut, StreamErr: Text; const prog: String):cint;
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
  pid: cint;
  pl: ^cint;
begin
  AssignStream := -1;

  // Assign pipes
  if AssignPipe(StreamIn, PipeOut)=-1 Then
   Exit(-1);

  If AssignPipe(StreamErr, PipeErr)=-1 Then
  begin
    Close(StreamIn);
    Close(PipeOut);
    exit(-1);
  end;

  if AssignPipe(PipeIn, StreamOut)=-1 Then
  begin
    Close(StreamIn);
    Close(PipeOut);
    Close(StreamErr);
    Close(PipeErr);
    exit(-1);
  end;

  // Fork

  pid := fpFork;
  if pid=-1 then begin
    Close(StreamIn);
    Close(PipeOut);
    Close(StreamErr);
    Close(PipeErr);
    Close(PipeIn);
    Close(StreamOut);
    exit(-1);
  end;

  if pid = 0 then begin
    // *** We are in the child ***
    // Close what we don not need
    Close(StreamOut);
    Close(StreamIn);
    Close(StreamErr);
    // Connect pipes
    if fpdup2(PipeIn, Input)=-1 Then
     Halt(127);
    Close(PipeIn);
    if fpdup2(PipeOut, Output)=-1 Then
     Halt(127);
    Close(PipeOut);
    if fpdup2(PipeErr, StdErr)=-1 Then
     Halt(127);
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
  If fpUname(sysn)<>0 then
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
  If fpuname(sysn)=-1 then
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
                             Utility calls
******************************************************************************}

{
Function Octal(l:cint):cint;
{
  Convert an octal specified number to decimal;
}
var
  octnr,
  oct : cint;
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
}

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
  p1     : cint;
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
       if (FpStat(NewDir,Info)>=0) and
          (not fpS_ISDIR(Info.st_Mode)) then
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

Function FSearch(const path:AnsiString;dirlist:Ansistring;AddCurrentPath:TFSearchOptions):AnsiString;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'

This function tries to make FSearch use ansistrings, and decrease
stringhandling overhead at the same time.

}
Var
  NewDir : PathStr;
  p1     : cint;
  Info   : Stat;
  i,j      : cint; 
  p      : pchar;
Begin

 if AddCurrentPath=CurrentDirectoryFirst Then
     Dirlist:='.:'+dirlist;		{Make sure current dir is first to be searched.}
 if AddCurrentPath=CurrentDirectoryLast Then
     Dirlist:=dirlist+':.';		{Make sure current dir is last to be searched.}

{Replace ':' and ';' with #0}

 for p1:=1 to length(dirlist) do
   if (dirlist[p1]=':') or (dirlist[p1]=';') then
    dirlist[p1]:=#0;

{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     p:=pchar(dirlist);
     i:=length(dirlist);
     j:=1;
     Repeat
       NewDir:=p+'/'+Path;
       if (FpStat(NewDir,Info)>=0) and
          (not fpS_ISDIR(Info.st_Mode)) then
        Begin
          If Pos('./',NewDir)=1 Then
           Delete(NewDir,1,2);
        {DOS strips off an initial .\}
        End
       Else
        NewDir:='';
       while (j<=i) and (p^<>#0) do begin inc(j); inc(p); end;
       if p^=#0 then inc(p);
     Until (j>=i) or (Length(NewDir) > 0);
     FSearch:=NewDir;
   End;
End;

Function FSearch(const path:AnsiString;dirlist:Ansistring):AnsiString;

Begin
 FSearch:=FSearch(path,dirlist,CurrentDirectoryFirst);
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
    exit(nil);
  temp:=basename(path,''); { get the pattern }
  if thedir^.dd_fd<0 then
     exit(nil);
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
           fpseterrno(ESysENOMEM);
          globfree(root);
          break;
        end;
       current^.next:=nil;
       getmem(current^.name,length(temp2)+1);
       if current^.name=nil then
        begin
          fpseterrno(ESysENOMEM);
          globfree(root);
          break;
        end;
       move(buffer^.d_name[0],current^.name^,length(temp2)+1);
     end;
  until false;
  fpclosedir(thedir^);
  glob:=root;
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
  Revision 1.62  2004-02-12 16:20:58  marco
   * currentpath stuff fixed for fsearch

  Revision 1.61  2004/02/12 15:31:06  marco
   * First version of fpexec change. Still under ifdef or silently overloaded

  Revision 1.60  2004/01/23 08:11:18  jonas
    * only include systypes.inc if FPC_USE_LIBC is not defined

  Revision 1.59  2004/01/22 13:46:14  marco
  bsd

  Revision 1.58  2004/01/04 21:05:01  jonas
    * declare C-library routines as external in libc so we generate proper
      import entries for Darwin

  Revision 1.57  2004/01/04 20:53:02  jonas
    * don't use systypes if FPC_USE_LIBC is defined

  Revision 1.56  2004/01/04 16:24:05  jonas
    * fixed WaitProcess in case of SysEintr

  Revision 1.55  2003/12/31 20:24:25  marco
   * export statfs(pchar)

  Revision 1.54  2003/12/30 15:43:20  marco
   * linux now compiles with FPC_USE_LIBC

  Revision 1.53  2003/12/30 12:24:01  marco
   * FPC_USE_LIBC

  Revision 1.52  2003/12/08 17:16:30  peter
    * fsearch should only find files

  Revision 1.51  2003/11/19 17:11:40  marco
   * termio unit

  Revision 1.50  2003/11/19 10:54:32  marco
   * some simple restructures

  Revision 1.49  2003/11/17 11:28:08  marco
   * Clone moved to linux, + few small unit unix changes

  Revision 1.48  2003/11/17 10:05:51  marco
   * threads for FreeBSD. Not working tho

  Revision 1.47  2003/11/14 17:30:14  marco
   * weeehoo linuxerror is no more :-)

  Revision 1.46  2003/11/14 16:44:48  marco
   * stream functions converted to work without linuxerror

  Revision 1.45  2003/11/13 18:44:06  marco
   * small fi

  Revision 1.44  2003/11/12 22:19:45  marco
   * more linuxeror fixes

  Revision 1.43  2003/11/03 09:42:28  marco
   * Peter's Cardinal<->Longint fixes patch

  Revision 1.42  2003/10/30 16:42:58  marco
   * fixes for old syscall() convention removing

  Revision 1.41  2003/10/12 19:40:43  marco
   * ioctl fixes. IDE now starts, but

  Revision 1.40  2003/09/29 14:36:06  peter
    * fixed for stricter compiler

  Revision 1.39  2003/09/27 12:51:33  peter
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
