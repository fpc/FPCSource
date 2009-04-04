{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2000 by Marco van de Voort
    member of the Free Pascal development team.

    System unit for the *BSD's.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{ These things are set in the makefile, }
{ But you can override them here.}

{ If you use an aout system, set the conditional AOUT}
{ $Define AOUT}

Unit System;

Interface


{$define FPC_USE_SIGPROCMASK}
{$define FPC_USE_SIGALTSTACK}

{$ifndef FPC_USE_LIBC}
{$define FPC_USE_SYSCALL}
{$endif}


{$define FPC_IS_SYSTEM}

{$I sysunixh.inc}

{$ifdef Darwin}
var argc:longint;
    argv:PPchar;
    envp:PPchar;
{$endif}

CONST SIGSTKSZ = 40960;

{$if defined(CPUARM)}

{$define fpc_softfpu_interface}
{$i softfpu.pp}
{$undef fpc_softfpu_interface}

{$endif defined(CPUARM)}


Implementation

{$if defined(CPUARM) or defined(CPUM68K)}

{$define fpc_softfpu_implementation}
{$i softfpu.pp}
{$undef fpc_softfpu_implementation}

{ we get these functions and types from the softfpu code }
{$define FPC_SYSTEM_HAS_float64}
{$define FPC_SYSTEM_HAS_float32}
{$define FPC_SYSTEM_HAS_flag}
{$define FPC_SYSTEM_HAS_extractFloat64Frac0}
{$define FPC_SYSTEM_HAS_extractFloat64Frac1}
{$define FPC_SYSTEM_HAS_extractFloat64Exp}
{$define FPC_SYSTEM_HAS_extractFloat64Sign}
{$define FPC_SYSTEM_HAS_ExtractFloat32Frac}
{$define FPC_SYSTEM_HAS_extractFloat32Exp}
{$define FPC_SYSTEM_HAS_extractFloat32Sign}

{$endif defined(CPUARM) or defined(CPUM68K)}


{$I system.inc}

{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

{$ifdef darwin}
procedure normalexit(status: cint); cdecl; external 'c' name 'exit';
{$endif}

procedure System_exit;
{$ifndef darwin}
begin
   Fpexit(cint(ExitCode));
end;
{$else darwin}
begin
   { make sure the libc atexit handlers are called, needed for e.g. profiling }
   normalexit(cint(ExitCode));
end;
{$endif darwin}


Function ParamCount: Longint;
Begin
  Paramcount:=argc-1
End;


function BackPos(c:char; const s: shortstring): integer;
var
 i: integer;
Begin
  for i:=length(s) downto 0 do
    if s[i] = c then break;
  if i=0 then
    BackPos := 0
  else
    BackPos := i;
end;


 { variable where full path and filename and executable is stored }
 { is setup by the startup of the system unit.                    }
//var
// execpathstr : shortstring;

function paramstr(l: longint) : string;
 begin
   { stricly conforming POSIX applications  }
   { have the executing filename as argv[0] }
//   if l=0 then
//     begin
//       paramstr := execpathstr;
//     end
//   else
     if (l < argc) then
       paramstr:=strpas(argv[l])
     else
       paramstr:='';
 end;

Procedure Randomize;
Begin
  randseed:=longint(Fptime(nil));
End;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

function  reenable_signal(sig : longint) : boolean;
var
  e,oe : TSigSet;
  i,j : byte;
  olderrno: cint;
begin
  fillchar(e,sizeof(e),#0);
  fillchar(oe,sizeof(oe),#0);
  { set is 1 based PM }
  dec(sig);
  i:=sig mod 32;
  j:=sig div 32;
  e[j]:=1 shl i;
  { this routine is called from a signal handler, so must not change errno }
  olderrno:=geterrno;
  fpsigprocmask(SIG_UNBLOCK,@e,@oe);
  reenable_signal:=geterrno=0;
  seterrno(olderrno);
end;

{$i sighnd.inc}

var
  act: SigActionRec;

Procedure InstallSignals;
var
  oldact: SigActionRec;
begin
  { Initialize the sigaction structure }
  { all flags and information set to zero }
  FillChar(act, sizeof(SigActionRec),0);
  { initialize handler                    }
  act.sa_handler :=@SignalToRunError;
  act.sa_flags:=SA_SIGINFO;
{$if defined(darwin) and defined(cpu64)}
  act.sa_flags:=SA_SIGINFO or SA_64REGSET;
{$else}
  act.sa_flags:=SA_SIGINFO;
{$endif}
  FpSigAction(SIGFPE,act,oldact);
  FpSigAction(SIGSEGV,act,oldact);
  FpSigAction(SIGBUS,act,oldact);
  FpSigAction(SIGILL,act,oldact);
end;


procedure SetupCmdLine;
var
  bufsize,
  len,j,
  size,i : longint;
  found  : boolean;
  buf    : pchar;

  procedure AddBuf;
  begin
    reallocmem(cmdline,size+bufsize);
    move(buf^,cmdline[size],bufsize);
    inc(size,bufsize);
    bufsize:=0;
  end;

begin
  GetMem(buf,ARG_MAX);
  size:=0;
  bufsize:=0;
  i:=0;
  while (i<argc) do
   begin
     len:=strlen(argv[i]);
     if len>ARG_MAX-2 then
      len:=ARG_MAX-2;
     found:=false;
     for j:=1 to len do
      if argv[i][j]=' ' then
       begin
         found:=true;
         break;
       end;
     if bufsize+len>=ARG_MAX-2 then
      AddBuf;
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     move(argv[i]^,buf[bufsize],len);
     inc(bufsize,len);
     if found then
      begin
        buf[bufsize]:='"';
        inc(bufsize);
      end;
     if i<argc then
      buf[bufsize]:=' '
     else
      buf[bufsize]:=#0;
     inc(bufsize);
     inc(i);
   end;
  AddBuf;
  FreeMem(buf,ARG_MAX);
end;

procedure SysInitStdIO;
begin
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(ErrOutput,fmOutput,StdErrorHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
end;


{$ifdef FPC_USE_LIBC}

{ can also be used with other BSD's if they use the system's crtX instead of prtX }

{$ifdef Darwin}

procedure pascalmain;cdecl;external name 'PASCALMAIN';

procedure FPC_SYSTEMMAIN(argcparam: Longint; argvparam: ppchar; envpparam: ppchar); cdecl; [public];

begin
  argc:= argcparam;
  argv:= argvparam;
  envp:= envpparam;
{$ifdef cpui386}
  Set8087CW(Default8087CW);  
{$endif cpui386}
  pascalmain;  {run the pascal main program}
end;
{$endif Darwin}
{$endif FPC_USE_LIBC}

function GetProcessID: SizeUInt;
begin
 GetProcessID := SizeUInt (fpGetPID);
end;

function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
begin
  result := stklen;
end;

Begin
  IsConsole := TRUE;
  StackLength := CheckInitialStkLen(InitialStkLen);
  StackBottom := Sptr - StackLength;
  { Set up signals handlers }
  InstallSignals;

  SysResetFPU;
  if not(IsLibrary) then
    SysInitFPU;

{$if defined(cpui386) or defined(cpuarm)}
  fpc_cpucodeinit;
{$endif cpui386}
  { Setup heap }
  InitHeap;
  SysInitExceptions;
  { Setup stdin, stdout and stderr }
  SysInitStdIO;
  { Reset IO Error }
  InOutRes:=0;
  { Arguments }
  SetupCmdLine;
  { threading }
  InitSystemThreads;
  initvariantmanager;
{$ifdef VER2_2}
  initwidestringmanager;
{$else VER2_2}
  initunicodestringmanager;
{$endif VER2_2}
End.
