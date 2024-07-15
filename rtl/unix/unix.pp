{
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
{$MACRO ON}
{$IFNDEF FPC_DOTTEDUNITS}
Unit Unix;
{$ENDIF FPC_DOTTEDUNITS}
Interface

{$IFDEF FPC_DOTTEDUNITS}
Uses
  UnixApi.Base,UnixApi.Types;
{$ELSE FPC_DOTTEDUNITS}
Uses
  BaseUnix,UnixType;
{$ENDIF FPC_DOTTEDUNITS}

// If you deprecated new symbols, please annotate the version.
// this makes it easier to decide if they can already be removed.

{$if (defined(BSD) or defined(SUNOS)) and defined(FPC_USE_LIBC)}
{$define USE_VFORK}
{$endif}

{$i aliasptp.inc}
{$i unxconst.inc} { Get Types and Constants only exported in this unit }

{$IFNDEF FPC_DOTTEDUNITS}
{$DEFINE BU:=baseunix}
{$ELSE}
{$DEFINE BU:=UnixApi.Base}
{$ENDIF}


{**  File handling **}

Const
  P_IN  = 1;                    // pipes (?)
  P_OUT = 2;

  LOCK_SH = 1;                  // flock constants ?
  LOCK_EX = 2;
  LOCK_UN = 8;
  LOCK_NB = 4;

// The portable MAP_* and PROT_ constants are exported from unit Unix for compability.
  PROT_READ  = BU.PROT_READ;             { page can be read }
  PROT_WRITE = BU.PROT_WRITE;             { page can be written }
  PROT_EXEC  = BU.PROT_EXEC;             { page can be executed }
  PROT_NONE  = BU.PROT_NONE;             { page can not be accessed }

  MAP_FAILED    = BU.MAP_FAILED;	      { mmap() failed }
  MAP_SHARED    = BU.MAP_SHARED;        { Share changes }
  MAP_PRIVATE   = BU.MAP_PRIVATE;       { Changes are private }
  MAP_TYPE      = BU.MAP_TYPE;          { Mask for type of mapping }
  MAP_FIXED     = BU.MAP_FIXED;         { Interpret addr exactly }

{** Time/Date Handling **}

type
  TTZInfo = record
    daylight     : boolean;
    seconds      : Longint; // difference from UTC
    validsince   : int64;   // UTC timestamp
    validuntil   : int64;   // UTC timestamp
  end;
  TTZInfoEx = record
    name         : array[boolean] of RawByteString; { False = StandardName, True = DaylightName }
    leap_correct : longint;
    leap_hit     : longint;
  end;

  Function GetTzseconds : Longint;
  property Tzseconds : Longint read GetTzseconds;
  function Gettzdaylight : boolean;
  property tzdaylight : boolean read Gettzdaylight;
  function Gettzname(const b : boolean) : string;
  property tzname[b : boolean] : string read Gettzname;
  function GetTZInfo : TTZInfo;
  property TZInfo : TTZInfo read GetTZInfo;
  function GetTZInfoEx : TTZInfoEx;
  property TZInfoEx : TTZInfoEx read GetTZInfoEx;
  procedure SetTZInfo(const ATZInfo: TTZInfo; const ATZInfoEx: TTZInfoEx);

{************     Procedure/Functions     ************}

{$ifdef android}
  {$define DONT_READ_TIMEZONE}
{$endif android}

{$IFNDEF DONT_READ_TIMEZONE}  // allows to disable linking in and trying for platforms
                       // it doesn't (yet) work for.

{ timezone support }
function GetLocalTimezone(timer:int64;timerIsUTC:Boolean;var ATZInfo:TTZInfo;var ATZInfoEx:TTZInfoEx):Boolean;
function GetLocalTimezone(timer:int64;timerIsUTC:Boolean;var ATZInfo:TTZInfo):Boolean;
procedure RefreshTZInfo;
function  ReadTimezoneFile(fn:string) : Boolean;
function  GetTimezoneFile:string;
Procedure ReReadLocalTime;
{$ENDIF}

Function UniversalToEpoch(year,month,day,hour,minute,second:Word):int64; // use DateUtils.DateTimeToUnix for cross-platform applications
Function LocalToEpoch(year,month,day,hour,minute,second:Word):int64; // use DateUtils.DateTimeToUnix for cross-platform applications
Procedure EpochToLocal(epoch:int64;var year,month,day,hour,minute,second:Word); // use DateUtils.UnixToDateTime for cross-platform applications
Procedure EpochToUniversal(epoch:int64;var year,month,day,hour,minute,second:Word); // use DateUtils.UnixToDateTime for cross-platform applications
Procedure JulianToGregorian(JulianDN:LongInt;Var Year,Month,Day:Word); // use DateUtils.DateTimetoJulianDate for cross-platform applications
Function GregorianToJulian(Year,Month,Day:Longint):LongInt; // use DateUtils.JulianDateToDateTime for cross-platform applications

{**  Process Handling  **}

function FpExecLE (Const PathName:RawByteString;const S:Array Of RawByteString;MyEnv:PPAnsiChar):cint;
function FpExecL  (Const PathName:RawByteString;const S:Array Of RawByteString):cint;
function FpExecLP (Const PathName:RawByteString;const S:Array Of RawByteString):cint;
function FpExecLPE(Const PathName:RawByteString;const S:Array Of RawByteString;env:PPAnsiChar):cint;
function FpExecV  (Const PathName:RawByteString;args:PPAnsiChar):cint;
function FpExecVP (Const PathName:RawByteString;args:PPAnsiChar):cint;
function FpExecVPE(Const PathName:RawByteString;args,env:PPAnsiChar):cint;

Function fpSystem(const Command:RawByteString):cint;

Function WaitProcess (Pid:cint):cint; 

Function WIFSTOPPED (Status: Integer): Boolean;
Function W_EXITCODE (ReturnCode, Signal: Integer): Integer;
Function W_STOPCODE (Signal: Integer): Integer;

{**      File Handling     **}
Function  fpFlock   (var T : text;mode : cint) : cint;
Function  fpFlock   (var F : File;mode : cint) : cint;

{**  Directory Handling  **}

procedure SeekDir(p:pdir;loc:clong);
function  TellDir(p:pdir):TOff;

{**     Pipe/Fifo/Stream     **}

Function AssignPipe  (var pipe_in,pipe_out:cint):cint;
Function AssignPipe  (var pipe_in,pipe_out:text):cint;
Function AssignPipe  (var pipe_in,pipe_out:file):cint;
Function POpen       (var F:text;const Prog:RawByteString;rw:AnsiChar):cint;
Function POpen       (var F:file;const Prog:RawByteString;rw:AnsiChar):cint;
Function POpen       (var F:text;const Prog:UnicodeString;rw:AnsiChar):cint;
Function POpen       (var F:file;const Prog:UnicodeString;rw:AnsiChar):cint;
Function AssignStream(Var StreamIn,Streamout:text;Const Prog:ansiString;const args : array of ansistring) : cint;
Function AssignStream(Var StreamIn,Streamout,streamerr:text;Const Prog:ansiString;const args : array of ansistring) : cint;
Function GetDomainName:String; deprecated; // because linux only.
Function GetHostName:String;

{** Utility functions  **}

Type
        TFSearchOption  = (NoCurrentDirectory,
                           CurrentDirectoryFirst,
                           CurrentDirectoryLast);

Function  FSearch  (const path:RawByteString;dirlist:RawByteString;CurrentDirStrategy:TFSearchOption):RawByteString;
Function  FSearch  (const path:RawByteString;dirlist:RawByteString):RawByteString;
Function  FSearch  (const path:UnicodeString;dirlist:UnicodeString;CurrentDirStrategy:TFSearchOption):UnicodeString;
Function  FSearch  (const path:UnicodeString;dirlist:UnicodeString):UnicodeString;

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

{$ifndef FPC_USE_LIBC}
{$IFDEF FPC_DOTTEDUNITS}
Uses
  UnixApi.SysCall;
{$ELSE FPC_DOTTEDUNITS}
Uses
  Syscall;
{$ENDIF FPC_DOTTEDUNITS}
{$endif}

{$i unxovl.inc}

{$ifndef FPC_USE_LIBC}
  {$i syscallh.inc}
  {$i unxsysc.inc}
{$endif}

{$i unxfunc.inc}   { Platform specific implementations }

Function getenv(name:string):PAnsiChar; external name 'FPC_SYSC_FPGETENV';

{******************************************************************************
                          timezone support
******************************************************************************}

var
  CurrentTZinfo : array [0..1] of TTZInfo;
  CurrentTzinfoEx : array [0..1] of TTZInfoEx;
  CurrentTZindex : LongInt = 0; // current index for CurrentTZinfo/CurrentTZinfoEx - can be only 0 or 1
{$ifdef FPC_HAS_FEATURE_THREADING}
  UseTZThreading: Boolean = false;
  TZInfoCS: TRTLCriticalSection;
{$endif}

procedure LockTZInfo;
begin
  {$if declared(UseTZThreading)}
  if UseTZThreading then
    EnterCriticalSection(TZInfoCS);
  {$endif}
end;

procedure UnlockTZInfo;
begin
  {$if declared(UseTZThreading)}
  if UseTZThreading then
    LeaveCriticalSection(TZInfoCS);
  {$endif}
end;

Function GetTzseconds : Longint;
begin
  GetTzseconds:=Tzinfo.seconds;
end;

function Gettzdaylight : boolean;
begin
  Gettzdaylight:=Tzinfo.daylight;
end;

function Gettzname(const b : boolean) : string;
begin
  Gettzname:=TzinfoEx.name[b];
end;

function GetTZInfo : TTZInfo;
{$IFNDEF DONT_READ_TIMEZONE}
var
  curtime: time_t;
{$ENDIF}
begin
  GetTZInfo:=CurrentTZinfo[InterlockedExchangeAdd(CurrentTZindex, 0)];
{$IFNDEF DONT_READ_TIMEZONE}
  curtime:=fptime;
  if not((GetTZInfo.validsince+GetTZInfo.seconds<=curtime) and (curtime<GetTZInfo.validuntil+GetTZInfo.seconds)) then
    begin
    RefreshTZInfo;
    GetTZInfo:=CurrentTZinfo[InterlockedExchangeAdd(CurrentTZindex, 0)];
    end;
{$ENDIF}
end;

function GetTZInfoEx : TTZInfoEx;
begin
  GetTZInfoEx:=CurrentTzinfoEx[InterlockedExchangeAdd(CurrentTZindex, 0)];
end;

procedure SetTZInfo(const ATZInfo: TTZInfo; const ATZInfoEx: TTZInfoEx);
var
  OldTZindex,NewTZindex: longint;
begin
  LockTZInfo;
  OldTZindex:=InterlockedExchangeAdd(CurrentTZindex,0);
  if OldTZindex=0 then
    NewTZindex:=1
  else
    NewTZindex:=0;
  CurrentTzinfo[NewTZindex]:=ATZInfo;
  CurrentTzinfoEx[NewTZindex]:=ATZInfoEx;
  InterlockedExchangeAdd(CurrentTZindex,NewTZindex-OldTZindex);
  UnlockTZInfo;
end;

Const
{Date Translation}
  C1970=2440588;
  D0   =   1461;
  D1   = 146097;
  D2   =1721119;

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

Procedure EpochToLocal(epoch:Int64;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into local time (hour, minute,seconds)
}
Var
  lTZInfo: TTZInfo;
Begin
  {$if declared(GetLocalTimezone)}
  if GetLocalTimezone(epoch,true,lTZInfo) then
    inc(Epoch,lTZInfo.seconds)
  else { fallback }
  {$endif}
    inc(Epoch,TZInfo.seconds);

  EpochToUniversal(epoch,year,month,day,hour,minute,second);
End;

Procedure EpochToUniversal(epoch:Int64;var year,month,day,hour,minute,second:Word);
{
  Transforms Epoch time into universal time (hour, minute,seconds)
}
Var
  DateNum: LongInt;
Begin
  Datenum:=(Epoch Div 86400) + c1970;
  JulianToGregorian(DateNum,Year,Month,day);
  Epoch:=Abs(Epoch Mod 86400);
  Hour:=Epoch Div 3600;
  Epoch:=Epoch Mod 3600;
  Minute:=Epoch Div 60;
  Second:=Epoch Mod 60;
End;

Function LocalToEpoch(year,month,day,hour,minute,second:Word):Int64;
{
  Transforms local time (year,month,day,hour,minutes,second) to Epoch time
   (seconds since 00:00, january 1 1970, corrected for local time zone)
}
Var
  lTZInfo: TTZInfo;
  LocalEpoch: Int64;
Begin
  LocalEpoch:=UniversalToEpoch(year,month,day,hour,minute,second);

  {$if declared(GetLocalTimezone)}
  if GetLocalTimezone(LocalEpoch,false,lTZInfo) then
    LocalToEpoch:=LocalEpoch-lTZInfo.seconds
  else { fallback }
  {$endif}
    LocalToEpoch:=LocalEpoch-TZInfo.seconds;
End;

Function UniversalToEpoch(year,month,day,hour,minute,second:Word):Int64;
{
  Transforms universal time (year,month,day,hour,minutes,second) to Epoch time
   (seconds since 00:00, january 1 1970, corrected for local time zone)
}
Begin
  UniversalToEpoch:=(Int64(GregorianToJulian(Year,Month,Day)-c1970)*86400)+
                (LongInt(Hour)*3600)+(Longint(Minute)*60)+Second;
End;

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

{******************************************************************************
                          Process related calls
******************************************************************************}

{ Most calls of WaitPID do not handle the result correctly, this funktion treats errors more correctly }
Function  WaitProcess(Pid:cint):cint; { like WaitPid(PID,@result,0) Handling of Signal interrupts (errno=EINTR), returning the Exitcode of Process (>=0) or -Status if terminated}
var
  r,s     : cint;
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
     if wifexited(s) then
      WaitProcess:=wexitstatus(s)
     else if (s>0) then  // Until now there is not use of the highest bit , but check this for the future
      WaitProcess:=-s // normal case
     else
      WaitProcess:=s; // s<0 should not occur, but wie return also a negativ value
   end;
end;

function intFpExecVEMaybeP (Const PathName:RawByteString;Args,MyEnv:PPAnsiChar;SearchPath:Boolean):cint;
// does an ExecVE, but still has to handle P
// execv variants call this directly, execl variants indirectly via
//     intfpexecl

Var
  NewCmd  : RawByteString;
  ThePath : RawByteString;

Begin
  If SearchPath and (pos('/',pathname)=0) Then
    Begin
      // The above could be better. (check if not escaped/quoted '/'s) ?
      // (Jilles says this is ok)
      // Stevens says only search if newcmd contains no '/'
      // fsearch is not ansistring clean yet.
      ThePath:=fpgetenv('PATH');
      SetCodePage(ThePath,DefaultSystemCodePage,false);
      SetCodePage(ThePath,DefaultFileSystemCodePage,true);
      if thepath='' then
        thepath:='.';     // FreeBSD uses _PATH_DEFPATH = /usr/bin:/bin
                          // but a quick check showed that _PATH_DEFPATH
                          // varied from OS to OS

      newcmd:=ToSingleByteFileSystemEncodedFileName(FSearch(pathname,thepath,NoCurrentDirectory));
      // FreeBSD libc keeps on trying till a file is successfully run.
      // Stevens says "try each path prefix"

      // execp puts newcmd here.
        args^:=PAnsiChar(newcmd);
   End else
      newcmd:=ToSingleByteFileSystemEncodedFileName(pathname);
 // repeat
//      if searchpath then args^:=PAnsiChar(commandtorun)

  IntFpExecVEMaybeP:=fpExecVE(newcmd,Args,MyEnv);
{
// Code that if exec fails due to permissions, tries to run it with sh
// Should we deallocate p on fail? -> no fpexit is run no matter what
//
}
// if intfpexecvemaybep=-1 then seach next file.
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

function intFpExecl (Const PathName:RawByteString;const s:array of RawByteString;MyEnv:PPAnsiChar;SearchPath:Boolean):cint;
{ Handles the array of ansistring -> PPAnsiChar conversion.
  Base for the the "l" variants.
}
var p:PPAnsiChar;
    i:integer;
    s2:array of Rawbytestring;
begin
  If PathName='' Then
    Begin
      fpsetErrno(ESysEnoEnt);
      Exit(-1);                 // Errno?
    End;
  setlength(s2,high(s)+1);
  for i:=low(s) to high(s) do
    s2[i]:=ToSingleByteFileSystemEncodedFileName(s[i]);
  p:=ArrayStringToPPchar(s2,1);
  if p=NIL Then
    Begin
      GetMem(p,2*sizeof(PAnsiChar));
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
  p^:=PAnsiChar(PathName);
  IntFPExecL:=intFpExecVEMaybeP(PathName,p,MyEnv,SearchPath);
  // If we come here, no attempts were executed successfully.
  Freemem(p);
end;

function FpExecLE (Const PathName:RawByteString;const S:Array Of RawByteString;MyEnv:PPAnsiChar):cint;

Begin
  FpExecLE:=intFPExecl(PathName,s,MyEnv,false);
End;


function FpExecL(Const PathName:RawByteString;const S:Array Of RawByteString):cint;

Begin
  FpExecL:=intFPExecl(PathName,S,EnvP,false);
End;

function FpExecLP(Const PathName:RawByteString;const S:Array Of RawByteString):cint;

Begin
  FpExecLP:=intFPExecl(PathName,S,EnvP,True);
End;

function FpExecLPE(Const PathName:RawByteString;const S:Array Of RawByteString;env:PPAnsiChar):cint;

Begin
  FpExecLPE:=intFPExecl(PathName,S,Env,True);
End;

function FpExecV(Const PathName:RawByteString;args:PPAnsiChar):cint;

Begin
 fpexecV:=intFpExecVEMaybeP (PathName,args,envp,false);
End;

function FpExecVP(Const PathName:RawByteString;args:PPAnsiChar):cint;

Begin
 fpexecVP:=intFpExecVEMaybeP (PathName,args,envp,true);
End;

function FpExecVPE(Const PathName:RawByteString;args,env:PPAnsiChar):cint;

Begin
 fpexecVPE:=intFpExecVEMaybeP (PathName,args,env,true);
End;

// exect and execvP (ExecCapitalP) are not implement
// Non POSIX anyway.
// Exect turns on tracing for the process
// execvP has the searchpath as array of ansistring ( const AnsiChar *search_path)

{$define FPC_USE_FPEXEC}
{$if defined(FPC_USE_FPEXEC) and not defined(USE_VFORK)}
{$define SHELL_USE_FPEXEC}
{$endif}

{$ifdef FPC_USE_LIBC}
function xfpsystem(p:PAnsiChar):cint; cdecl; external clib name 'system';

Function fpSystem(const Command:RawByteString):cint;
var
  cmd: RawByteString;
begin
  cmd:=ToSingleByteFileSystemEncodedFileName(Command);
  fpsystem:=xfpsystem(PAnsiChar(cmd));
end;

{$else}
Function fpSystem(const Command:RawByteString):cint;
var
  pid,savedpid   : cint;
  pstat          : cint;
  ign,intact,
  quitact        : SigactionRec;
  newsigblock,
  oldsigblock    : tsigset;
 {$ifndef SHELL_USE_FPEXEC}
   p      : PPAnsiChar;
 {$endif}
  cmd     : RawByteString;

begin { Changes as above }
  { fpexec* take care of converting the command to the right code page }
  if command='' then exit(1);
  {$ifndef SHELL_USE_FPEXEC}
    p:=CreateShellArgv(command);
  {$endif}
  ign.sa_handler:=SigActionHandler(SIG_IGN);
  fpsigemptyset(ign.sa_mask);
  ign.sa_flags:=0;
  fpsigaction(SIGINT, @ign, @intact);
  fpsigaction(SIGQUIT, @ign, @quitact);
  fpsigemptyset(newsigblock);
  fpsigaddset(newsigblock,SIGCHLD);
  fpsigprocmask(SIG_BLOCK,newsigblock,oldsigblock);
  {$ifdef USE_VFORK}
    pid:=fpvfork;
  {$else USE_VFORK}
    pid:=fpfork;
  {$endif USE_VFORK}
  if pid=0 then // We are in the Child
   begin
     fpsigaction(SIGINT,@intact,NIL);
     fpsigaction(SIGQUIT,@quitact,NIL);
     fpsigprocmask(SIG_SETMASK,@oldsigblock,NIL);
     {$ifndef SHELL_USE_FPEXEC}
       fpExecve(p^,p,envp);
     {$else}
       fpexecl('/bin/sh',['-c',Command]);
     {$endif}
     fpExit(127); // was exit(127)!! We must exit the Process, not the function
   end
  else if (pid<>-1) then // Successfull started
     begin
        savedpid:=pid;
        repeat
          pid:=fpwaitpid(savedpid,@pstat,0);
        until (pid<>-1) and (fpgeterrno()<>ESysEintr);
        if pid=-1 Then
         fpsystem:=-1
        else
         fpsystem:=pstat;
     end
  else // no success
   fpsystem:=-1;
  fpsigaction(SIGINT,@intact,NIL);
  fpsigaction(SIGQUIT,@quitact,NIL);
  fpsigprocmask(SIG_SETMASK,@oldsigblock,NIL);
  {$ifndef SHELL_USE_FPEXEC}
    FreeShellArgV(p);
  {$endif}
end;
{$endif}

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


{$IFNDEF DONT_READ_TIMEZONE}
{ Include timezone handling routines which use /usr/share/timezone info }
{$i timezone.inc}

{$endif}
{******************************************************************************
                           FileSystem calls
******************************************************************************}

Function fpFlock (var T : text;mode : cint) : cint;
begin
{$ifndef beos}
  fpFlock:=fpFlock(TextRec(T).Handle,mode);
{$endif}
end;


Function  fpFlock (var F : File;mode : cint) :cint;
begin
{$ifndef beos}
  fpFlock:=fpFlock(FileRec(F).Handle,mode);
{$endif}
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
 {$if not(defined(bsd)) and not(defined(solaris)) and not(defined(beos)) and not(defined(aix)) }
  p^.dd_nextoff:=fplseek(p^.dd_fd,loc,seek_set);
 {$endif}
 {$if not(defined(beos))}
  p^.dd_size:=0;
  p^.dd_loc:=0;
 {$endif} 
end;

function TellDir(p:pdir):TOff;
begin
  if p=nil then
   begin
     fpseterrno(ESysEBADF);
     telldir:=-1;
     exit;
   end;
 {$ifndef beos}   
  telldir:=fplseek(p^.dd_fd,0,seek_cur)
 {$endif}     
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
          IOPipe:=fpwrite(textrec(f).handle,PAnsiChar(textrec(f).bufptr),textrec(f).bufpos);
      end;
    fminput : Begin
                textrec(f).bufend:=fpread(textrec(f).handle,PAnsiChar(textrec(f).bufptr),textrec(f).bufsize);
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


Function POpen_internal(var F:text;const Prog:RawByteString;rw:AnsiChar):cint;
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
  pid  : cint;
  pl   : ^cint;
{$if not defined(FPC_USE_FPEXEC) or defined(USE_VFORK)}
  pp : array[0..3] of PAnsiChar;
  temp : string[255];
{$endif not FPC_USE_FPEXEC or USE_VFORK}
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
{$ifdef USE_VFORK}
  pid:=fpvfork;
{$else USE_VFORK}
  pid:=fpfork;
{$endif USE_VFORK}
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
        if (textrec(pipi).handle <> stdinputhandle) then
          begin
            ret:=fpdup2(pipi,input);
{$ifdef USE_VFORK}
            fpclose(textrec(pipi).handle);
{$else USE_VFORK}
            close(pipi);
{$endif USE_VFORK}
          end;
{$ifdef USE_VFORK}
        fpclose(textrec(pipo).handle);
{$else USE_VFORK}
        close(pipo);
{$endif USE_VFORK}
        if ret=-1 then
         fpexit(127);
      end
     else
      begin
{$ifdef USE_VFORK}
        fpclose(textrec(pipi).handle);
{$else USE_VFORK}
        close(pipi);
{$endif USE_VFORK}
        if (textrec(pipo).handle <> stdoutputhandle) then
          begin
            ret:=fpdup2(pipo,output);
{$ifdef USE_VFORK}
            fpclose(textrec(pipo).handle);
{$else USE_VFORK}
            close(pipo);
{$endif USE_VFORK}
          end;
        if ret=-1 then
         fpexit(127);
      end;
     {$if defined(FPC_USE_FPEXEC) and not defined(USE_VFORK)}
     fpexecl(PAnsiChar('/bin/sh'),['-c',Prog]);
     {$else}
     temp:='/bin/sh'#0'-c'#0;
     pp[0]:=@temp[1];
     pp[1]:=@temp[9];
     pp[2]:=@prog[1];
     pp[3]:=Nil;
     fpExecve('/bin/sh',@pp,envp);
     {$endif}
     fpexit(127);
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
     textrec(f).bufptr:=@textrec(f).buffer;
   {Save the process ID - needed when closing }
     pl:=pcint(@textrec(f).userdata[2]);
     { avoid alignment error on sparc }
     move(pid,pl^,sizeof(pid));
     textrec(f).closefunc:=@PCloseText;
   end;
 POpen_internal:=0;
end;

Function POpen_internal(var F:file;const Prog:RawByteString;rw:AnsiChar):cint;
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
{$if not defined(FPC_USE_FPEXEC) or defined(USE_VFORK)}
  pp : array[0..3] of PAnsiChar;
  temp : string[255];
{$endif not FPC_USE_FPEXEC or USE_VFORK}
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
{$ifdef USE_VFORK}
  pid:=fpvfork;
{$else USE_VFORK}
  pid:=fpfork;
{$endif USE_VFORK}
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
        if (filerec(pipi).handle <> stdinputhandle) then
          begin
            ret:=fpdup2(filerec(pipi).handle,stdinputhandle);
{$ifdef USE_VFORK}
            fpclose(filerec(pipi).handle);
{$else USE_VFORK}
            close(pipi);
{$endif USE_VFORK}
          end;
{$ifdef USE_VFORK}
        fpclose(filerec(pipo).handle);
{$else USE_VFORK}
        close(pipo);
{$endif USE_VFORK}
        if ret=-1 then
         fpexit(127);
      end
     else
      begin
{$ifdef USE_VFORK}
        fpclose(filerec(pipi).handle);
{$else USE_VFORK}
        close(pipi);
{$endif USE_VFORK}
        if (filerec(pipo).handle <> stdoutputhandle) then
          begin
            ret:=fpdup2(filerec(pipo).handle,stdoutputhandle);
{$ifdef USE_VFORK}
            fpclose(filerec(pipo).handle);
{$else USE_VFORK}
            close(pipo);
{$endif USE_VFORK}
          end;
        if ret=-1 then
         fpexit(127);
      end;
     {$if defined(FPC_USE_FPEXEC) and not defined(USE_VFORK)}
     fpexecl(PAnsiChar('/bin/sh'),['-c',Prog]);
     {$else}
     temp:='/bin/sh'#0'-c'#0;
     pp[0]:=@temp[1];
     pp[1]:=@temp[9];
     pp[2]:=@prog[1];
     pp[3]:=Nil;
     fpExecve('/bin/sh',@pp,envp);
     {$endif}
     fpexit(127);
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
     pl:=pcint(@filerec(f).userdata[2]);
     { avoid alignment error on sparc }
     move(pid,pl^,sizeof(pid));
   end;
 POpen_internal:=0;
end;

Function POpen(var F:text;const Prog:RawByteString;rw:AnsiChar):cint;
begin
  { can't do the ToSingleByteFileSystemEncodedFileName() conversion inside
    POpen_internal, because this may destroy the temp rawbytestring result
    of that function in the parent before the child is finished with it }
  POpen:=POpen_internal(F,ToSingleByteFileSystemEncodedFileName(Prog),rw);
end;

Function POpen(var F:file;const Prog:RawByteString;rw:AnsiChar):cint;
begin
  { can't do the ToSingleByteFileSystemEncodedFileName() conversion inside
    POpen_internal, because this may destroy the temp rawbytestring result
    of that function in the parent before the child is finished with it }
  POpen:=POpen_internal(F,ToSingleByteFileSystemEncodedFileName(Prog),rw);
end;

function POpen(var F: text; const Prog: UnicodeString; rw: AnsiChar): cint;
begin
  POpen:=POpen_internal(F,ToSingleByteFileSystemEncodedFileName(Prog),rw);
end;


function POpen(var F: file; const Prog: UnicodeString; rw: AnsiChar): cint;
begin
  POpen:=POpen_internal(F,ToSingleByteFileSystemEncodedFileName(Prog),rw);
end;


Function AssignStream(Var StreamIn,Streamout:text;Const Prog:ansiString;const args : array of ansistring) : cint;
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
  if fpAccess(prog,X_OK)<>0 then
    exit(-1);
  if AssignPipe(streamin,pipo)=-1 Then
   exit(-1);
  if AssignPipe(pipi,streamout)=-1 Then
    begin
      close(streamin);
      close(pipo);
      exit(-1);
    end;
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
     fpExecl(Prog,args);
     halt(127);
   end
  else
   begin
     { we're in the parent}
     close(pipo);
     close(pipi);
     {Save the process ID - needed when closing }
     pl:=pcint(@textrec(StreamIn).userdata[2]);
     { avoid alignment error on sparc }
     move(pid,pl^,sizeof(pid));
     textrec(StreamIn).closefunc:=@PCloseText;
     {Save the process ID - needed when closing }
     pl:=pcint(@textrec(StreamOut).userdata[2]);
     { avoid alignment error on sparc }
     move(pid,pl^,sizeof(pid));
     textrec(StreamOut).closefunc:=@PCloseText;
     AssignStream:=Pid;
   end;
end;

Function AssignStream(Var StreamIn,Streamout,streamerr:text;Const Prog:ansiString;const args : array of ansistring) : cint;

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
  if fpAccess(prog,X_OK)<>0 then
    exit(-1);
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
    fpExecl(Prog,args);
    Halt(127);
  end else begin
    // *** We are in the parent ***
    Close(PipeErr);
    Close(PipeOut);
    Close(PipeIn);
    // Save the process ID - needed when closing
    pl := pcint(@TextRec(StreamIn).userdata[2]);
    { avoid alignment error on sparc }
    move(pid,pl^,sizeof(pid));
    TextRec(StreamIn).closefunc := @PCloseText;
    // Save the process ID - needed when closing
    pl := pcint(@TextRec(StreamOut).userdata[2]);
    { avoid alignment error on sparc }
    move(pid,pl^,sizeof(pid));
    TextRec(StreamOut).closefunc := @PCloseText;
    // Save the process ID - needed when closing
    pl := pcint(@TextRec(StreamErr).userdata[2]);
    { avoid alignment error on sparc }
    move(pid,pl^,sizeof(pid));
    TextRec(StreamErr).closefunc := @PCloseText;
    AssignStream := pid;
  end;
end;

{******************************************************************************
                        General information calls
******************************************************************************}

{$if defined(Linux)}
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

{$ifdef sunos}
{ sunos doesn't support GetDomainName, see also
  http://www.sun.com/software/solaris/programs/abi/appcert_faq.xml#q18
}
Function GetDomainName:String;
  begin
    GetDomainName:='';
  end;
{$endif sunos}

{$ifdef android}
{ android doesn't seem to implement GetDomainName
}
Function GetDomainName:String;
  begin
    GetDomainName:='';
  end;
{$endif}

{$if defined(BSD) or defined(aix)}

function intGetDomainName(Name:PAnsiChar; NameLen:Cint):cint;
{$ifndef FPC_USE_LIBC}
 external name 'FPC_SYSC_GETDOMAINNAME';
{$else FPC_USE_LIBC}
 cdecl; external clib name 'getdomainname';
{$endif FPC_USE_LIBC}

Function GetDomainName:String;  { linux only!}
// domainname is a glibc extension.

{
  Get machines domain name. Returns empty string if not set.
}

var
  s : ShortString;

begin
  if intGetDomainName(@s[1],255)=-1 then
   s:=''
  else
   SetLength(s,strlen(@s[1]));
  getdomainname:=s;  
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
                             Utility calls
******************************************************************************}

Function FSearch(const path:RawByteString;dirlist:RawByteString;CurrentDirStrategy:TFSearchOption):RawByteString;
{
  Searches for a file 'path' in the list of direcories in 'dirlist'.
  returns an empty string if not found. Wildcards are NOT allowed.
  If dirlist is empty, it is set to '.'

This function tries to make FSearch use ansistrings, and decrease
stringhandling overhead at the same time.

}
Var
  mypath,
  mydir,NewDir : RawByteString;
  p1     : cint;
  Info   : Stat;
  p,pe   : PAnsiChar;
Begin
 SetCodePage(dirlist,DefaultFileSystemCodePage);
 if CurrentDirStrategy=CurrentDirectoryFirst Then
     Dirlist:=ToSingleByteFileSystemEncodedFileName('.:')+dirlist             {Make sure current dir is first to be searched.}
 else if CurrentDirStrategy=CurrentDirectoryLast Then
     Dirlist:=dirlist+ToSingleByteFileSystemEncodedFileName('.:');             {Make sure current dir is last to be searched.}

{Replace ':' and ';' with #0}

 for p1:=1 to length(dirlist) do
   if (dirlist[p1]=':') or (dirlist[p1]=';') then
    dirlist[p1]:=#0;

{Check for WildCards}
  If (Pos('?',Path) <> 0) or (Pos('*',Path) <> 0) Then
   FSearch:='' {No wildcards allowed in these things.}
  Else
   Begin
     mypath:=ToSingleByteFileSystemEncodedFileName(path);
     p:=PAnsiChar(dirlist);
     pe:=p+length(dirlist); { Points to terminating #0. }
     Repeat
       mydir:=RawByteString(p);
       if (length(mydir)>0) and (mydir[length(mydir)]<>'/') then
          begin
            { concatenate character without influencing code page }
            setlength(mydir,length(mydir)+1);
            mydir[length(mydir)]:='/';
          end;
       NewDir:=mydir+mypath;
       if (FpStat(NewDir,Info)>=0) and
          (not fpS_ISDIR(Info.st_Mode)) then
        Begin
          If Pos('./',NewDir)=1 Then
           Delete(NewDir,1,2);
        {DOS strips off an initial .\}
        End
       Else
        NewDir:='';
       inc(p,IndexByte(p^,-1,0)+1); { Can increment to pe + 1 (at most). }
     Until (p>=pe) or (Length(NewDir) > 0);
     FSearch:=NewDir;
     SetCodePage(FSearch,DefaultRTLFileSystemCodePage);
   End;
End;


Function FSearch(const path:RawByteString;dirlist:RawByteString):RawByteString;
Begin
 FSearch:=FSearch(path,dirlist,CurrentDirectoryFirst);
End;

function FSearch(const path: UnicodeString; dirlist: UnicodeString; CurrentDirStrategy: TFSearchOption): UnicodeString;
begin
  FSearch:=FSearch(ToSingleByteFileSystemEncodedFileName(path),ToSingleByteFileSystemEncodedFileName(dirlist),CurrentDirStrategy);
end;

function FSearch(const path: UnicodeString; dirlist: UnicodeString): UnicodeString;
begin
  FSearch:=FSearch(ToSingleByteFileSystemEncodedFileName(path),ToSingleByteFileSystemEncodedFileName(dirlist),CurrentDirectoryFirst);
end;

{$ifdef android}
  {$I unixandroid.inc}
{$endif android}

{$if declared(UseTZThreading)}
procedure InitTZThreading;
begin
  UseTZThreading:=True;
  InitCriticalSection(TZInfoCS);
end;
{$endif}

Initialization
{$if declared(UseTZThreading)}
  RegisterLazyInitThreadingProc(@InitTZThreading);
{$endif}
{$IFNDEF DONT_READ_TIMEZONE}
  InitLocalTime;
{$endif}
{$ifdef android}
  InitLocalTime;
{$endif android}

finalization
{$IFNDEF DONT_READ_TIMEZONE}
  DoneLocalTime;
{$endif}
{$if declared(UseTZThreading)}
  if UseTZThreading then
    DoneCriticalSection(TZInfoCS);
{$endif}
End.
