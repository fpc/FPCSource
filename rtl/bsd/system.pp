{
    $Id$
    This file is part of the Free Pascal run time librar~y.
    Copyright (c) 2000 by Marco van de Voort
    member of the Free Pascal development team.

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

Unit {$ifdef VER1_0}SysBSD{$else}System{$endif};
Interface

{$I sysunixh.inc}

Implementation

Var Errno : longint;

function geterrno:longint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 GetErrno:=Errno;
end;

{ OS independant parts}

{$I system.inc}
{ OS dependant parts  }

{$I errno.inc}
{$I osposixh.inc}
{$I bsdsysc.inc} 
{$I sysposix.inc}
{$I text.inc}      
{$I heap.inc}



{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}


{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}



Begin
  IsConsole := TRUE;
  IsLibrary := FALSE;
  StackBottom := Sptr - StackLength;
{ Set up signals handlers }
  InstallSignals;
{ Setup heap }
  InitHeap;
  InitExceptions;
{ Arguments }
  SetupCmdLine;
{ Setup stdin, stdout and stderr }
  OpenStdIO(Input,fmInput,StdInputHandle);
  OpenStdIO(Output,fmOutput,StdOutputHandle);
  OpenStdIO(StdOut,fmOutput,StdOutputHandle);
  OpenStdIO(StdErr,fmOutput,StdErrorHandle);
{ Reset IO Error }
  InOutRes:=0;
End.

{
  $Log$
  Revision 1.1  2002-08-19 12:29:11  marco
   * First working POSIX *BSD system unit.

  Revision 1.1  2000/10/15 08:19:48  peter
    * system unit rename for 1.1 branch

  Revision 1.2  2000/09/18 13:42:35  marco
   * FreeBSD support into 1.1

  Revision 1.1.2.1  2000/09/16 11:19:08  marco
   * Moved files from BSD to FreeBSD directory, with some small changes

  Revision 1.1.2.4  2000/09/16 11:10:43  marco
   * Introduced using sysunix and sysunixh

  Revision 1.1.2.3  2000/09/10 16:12:40  marco
  The rearrangement to linux for

  Revision 1.1.2.2  2000/08/05 18:33:29  peter
    * paramstr(0) fix for linux 2.0 kernels

  Revision 1.1.2.1  2000/07/14 07:33:15  michael
  + Fixed do_open call. Directory checking must not be performed

  Revision 1.1  2000/07/13 06:30:54  michael
  + Initial import

  Revision 1.49  2000/07/08 18:02:39  peter
    * do_open checks for directory, if directory then ioerror 2

  Revision 1.48  2000/06/30 22:14:03  peter
    * removed obsolete crtlib code
    * support EINTR for read/write to restart the syscall

  Revision 1.47  2000/05/11 17:55:13  peter
    * changed order of fpustate checking to first check the more
      specific states

  Revision 1.46  2000/05/08 14:27:36  peter
    * released newsignal
    * newsignal gives now better backtraces using the sigcontext eip/ebp
      fields

  Revision 1.45  2000/04/16 16:07:58  marco
   * BSD fixes

  Revision 1.44  2000/04/14 13:04:53  marco
   * Merged bsd/syslinux.pp and 1.43 linux/syslinux.pp to this file with ifdefs

  Revision 1.43  2000/04/07 14:56:36  peter
    * switch to direct asm if not correctfldcw defined

  Revision 1.42  2000/03/31 23:26:32  pierre
   * FPU needs reset for all SIGFPE even from integer division by zero

  Revision 1.41  2000/03/31 23:21:19  pierre
    * multiple exception handling works
      (for linux only if syslinux is compiled with -dnewsignal)

  Revision 1.40  2000/03/31 13:24:28  jonas
    * signal handling using sigaction when compiled with -dnewsignal
      (allows multiple signals to be received in one run)

  Revision 1.39  2000/03/25 12:28:37  peter
    * patch for getdir from Pierre

  Revision 1.38  2000/03/23 15:24:18  peter
    * remove handle check for do_close

  Revision 1.37  2000/02/09 16:59:32  peter
    * truncated log

  Revision 1.36  2000/02/09 12:17:51  peter
    * moved halt to system.inc
    * syslinux doesn't use direct asm anymore

  Revision 1.35  2000/02/08 11:47:09  peter
    * paramstr(0) support

  Revision 1.34  2000/01/20 23:38:02  peter
    * support fm_inout as stdoutput for assign(f,'');rewrite(f,1); becuase
      rewrite opens always with filemode 2

  Revision 1.33  2000/01/16 22:25:38  peter
    * check handle for file closing

  Revision 1.32  2000/01/07 16:41:41  daniel
    * copyright 2000

  Revision 1.31  2000/01/07 16:32:28  daniel
    * copyright 2000 added

  Revision 1.30  1999/12/01 22:57:31  peter
    * cmdline support

  Revision 1.29  1999/11/06 14:39:12  peter
    * truncated log

  Revision 1.28  1999/10/28 09:50:06  peter
    * use mmap instead of brk

  Revision 1.27  1999/09/10 15:40:35  peter
    * fixed do_open flags to be > $100, becuase filemode can be upto 255

  Revision 1.26  1999/09/08 16:14:43  peter
    * pointer fixes

  Revision 1.25  1999/07/28 23:18:36  peter
    * closedir fixes, which now disposes the pdir itself

}
