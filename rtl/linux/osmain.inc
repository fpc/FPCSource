{
    $Id$
    This file is part of the Free Pascal run time library.

    POSIX Interface to the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This is the core of the system unit *nix systems (now FreeBSD
     and Unix).

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


{*****************************************************************************
                       Misc. System Dependent Functions
*****************************************************************************}

procedure haltproc(e:longint);cdecl;external name '_haltproc';

procedure System_exit;
begin
  haltproc(ExitCode);
End;


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
var
 execpathstr : shortstring;

function paramstr(l: longint) : string;
 begin
   { stricly conforming POSIX applications  }
   { have the executing filename as argv[0] }
   if l=0 then
     begin
       paramstr := execpathstr;
     end
   else
     paramstr:=strpas(argv[l]);
 end;

Procedure Randomize;
Begin
  randseed:=longint(Fptime(nil));
End;


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

{
  The lowlevel file functions should take care of setting the InOutRes to the
  correct value if an error has occured, else leave it untouched
}

Function PosixToRunError  (PosixErrno : longint) : longint;
{
  Convert ErrNo error to the correct Inoutres value
}

begin
  if PosixErrNo=0 then { Else it will go through all the cases }
   exit(0);
  case PosixErrNo of
   ESysENFILE,
   ESysEMFILE : Inoutres:=4;
   ESysENOENT : Inoutres:=2;
    ESysEBADF : Inoutres:=6;
   ESysENOMEM,
   ESysEFAULT : Inoutres:=217;
   ESysEINVAL : Inoutres:=218;
    ESysEPIPE,
    ESysEINTR,
      ESysEIO,
   ESysEAGAIN,
   ESysENOSPC : Inoutres:=101;
 ESysENAMETOOLONG : Inoutres := 3;
    ESysEROFS,
   ESysEEXIST,
   ESysENOTEMPTY,
   ESysEACCES : Inoutres:=5;
   ESysEISDIR : InOutRes:=5;
  else
    begin
       InOutRes := Integer(PosixErrno);
    end;
  end;
 PosixToRunError:=InOutRes;
end;


Function Errno2InoutRes : longint;
begin
  Errno2InoutRes:=PosixToRunError(getErrno);
  InoutRes:=Errno2InoutRes;
end;


{*****************************************************************************
                         SystemUnit Initialization
*****************************************************************************}

// signal handler is arch dependant due to processorexception to language
// exception translation

{$i sighnd.inc}

var
  act: SigActionRec;

Procedure InstallSignals;
begin
  { Initialize the sigaction structure }
  { all flags and information set to zero }
  FillChar(act, sizeof(SigActionRec),0);
  { initialize handler                    }
  act.sa_handler := SigActionHandler(@SignalToRunError);
  act.sa_flags:=SA_SIGINFO
{$ifdef cpux86_64}
    or $4000000
{$endif cpux86_64}
    ;
  FpSigAction(SIGFPE,@act,nil);
  FpSigAction(SIGSEGV,@act,nil);
  FpSigAction(SIGBUS,@act,nil);
  FpSigAction(SIGILL,@act,nil);
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

{
 $Log$
 Revision 1.27  2005-02-06 13:06:20  peter
   * moved file and dir functions to sysfile/sysdir
   * win32 thread in systemunit

 Revision 1.26  2005/02/05 22:53:43  peter
   * use typecasted sigactionhandler, needed for arm

 Revision 1.25  2005/02/03 21:42:17  peter
   * readded magic value $4000000 for sa_flags for x86_64

 Revision 1.24  2005/01/31 20:13:24  peter
   * rt_sigaction for all cpus

 Revision 1.23  2005/01/30 18:01:15  peter
   * signal cleanup for linux
   * sigactionhandler instead of tsigaction for bsds
   * sigcontext moved to cpu dir

 Revision 1.22  2004/11/02 14:49:48  florian
   * fixed baseunix.signal for CPU using rt_sigaction
   * fixed it for x86_64 too

 Revision 1.21  2004/10/25 15:38:59  peter
   * compiler defined HEAP and HEAPSIZE removed

 Revision 1.20  2004/08/04 19:27:09  florian
   * fixed floating point and integer exception handling on sparc/linux

 Revision 1.19  2004/05/31 20:25:04  peter
   * removed warnings

 Revision 1.18  2004/05/16 18:51:20  peter
   * use thandle in do_*

 Revision 1.17  2004/05/01 15:59:17  florian
   * x86_64 exception handling fixed

 Revision 1.16  2004/04/27 20:47:00  florian
   * tried to fix x86-64 signal handling

 Revision 1.15  2004/04/22 21:16:35  peter
   * do_write/do_read fix

 Revision 1.14  2004/03/27 14:33:45  florian
   * tell sigaction to pass siginfo on arm

 Revision 1.13  2004/03/10 20:35:33  peter
   * call _haltproc instead of exit(). This is required for libc linking

 Revision 1.12  2004/01/01 14:19:55  marco
  * use_getcwd updates because FPC_USE_LIBC uses that

 Revision 1.11  2003/12/30 16:26:10  marco
  * some more fixes. Testing on idefix

 Revision 1.10  2003/12/21 20:30:49  peter
   * don't exit in getdir when fpstat gives a failure

 Revision 1.9  2003/12/14 14:28:36  peter
   * only check errno if the syscall failed

 Revision 1.8  2003/11/01 01:58:11  marco
  * more small fixes.

 Revision 1.7  2003/10/31 20:36:01  marco
  * i386 specific fixes that hopefully fix texception4.
        Only the "generic" signal handler was ported to the unix rtl.

 Revision 1.6  2003/09/27 12:51:33  peter
   * fpISxxx macros renamed to C compliant fpS_ISxxx

 Revision 1.5  2003/05/01 08:05:23  florian
   * started to make the rtl 64 bit save by introducing SizeInt and SizeUInt (similar to size_t of C)

 Revision 1.4  2002/12/24 19:45:40  peter
   * Fix do_erase which was wrong with inoutres setting

 Revision 1.3  2002/12/23 22:23:43  peter
   * fixed Getdir to not set Inoutres
   * broken symlinks are now ignored in getdir instead of aborting
     the search

 Revision 1.2  2002/12/18 20:43:27  peter
   * removed stackcheck, the generic stackcheck is used
   * fixed return value for error conversion when no error was passed

 Revision 1.1  2002/12/18 16:43:26  marco
  * new unix rtl, linux part.....

 Revision 1.7  2002/11/14 12:18:03  marco
  * fixed Fptime call to (NIL)

 Revision 1.6  2002/10/27 17:21:29  marco
  * Only "difficult" functions + execvp + termios + rewinddir left to do

 Revision 1.5  2002/10/26 18:27:52  marco
  * First series POSIX calls commits. Including getcwd.

 Revision 1.4  2002/09/07 16:01:26  peter
   * old logs removed and tabs fixed

 Revision 1.3  2002/08/20 12:50:22  marco
  * New errno handling. Should be libc compatible.

 Revision 1.2  2002/08/10 13:42:36  marco
  * Fixes Posix dir copied to devel branch

 Revision 1.1.2.18  2002/03/10 11:45:02  carl
 * InOutRes := 16 with rmdir()
 * InOutRes := 5 more checking

 Revision 1.1.2.17  2002/03/03 15:11:51  carl
 * erase() bugfix (erasing a directory is done via rmdir() only!)

 Revision 1.1.2.16  2002/02/15 18:13:35  carl
 * bugfix for paramstr(0)

}