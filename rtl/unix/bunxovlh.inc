{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2002 by Marco van de Voort

    Some generic overloads for stringfunctions in the baseunix unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

Function  FpLink (existing : AnsiString; newone : AnsiString): cInt;
Function  FpMkfifo (path : AnsiString; Mode : TMode): cInt;
Function  FpChmod (path : AnsiString; Mode : TMode): cInt;
Function  FpChown (path : AnsiString; owner : TUid; group : TGid): cInt;
Function  FpUtime (path : AnsiString; times : putimbuf): cInt;
Function  FpGetcwd (path:AnsiString; siz:TSize):AnsiString;
Function  FpExecve (path : AnsiString; argv : ppchar; envp: ppchar): cInt;
Function  FpExecv (path : AnsiString; argv : ppchar): cInt;
Function  FpOpendir (dirname : AnsiString): pDir;
Function  FpChdir (path : AnsiString): cInt;
Function  FpOpen (path : AnsiString; flags : cInt; Mode: TMode):cInt;
Function  FpMkdir (path : AnsiString; Mode: TMode):cInt;
Function  FpUnlink (path : AnsiString): cInt;
Function  FpRmdir (path : AnsiString): cInt;
Function  FpRename (old  : AnsiString;newpath: AnsiString): cInt;
Function  FpStat (path: AnsiString; var buf : stat): cInt;
Function  FpAccess (pathname : AnsiString; aMode : cInt): cInt; 

Function  FPFStat(var F:Text;Var Info:stat):Boolean;
Function  FPFStat(var F:File;Var Info:stat):Boolean;

// added. Is a depreciated POSIX function that can be considered alias to
// sigaction
Function FpSignal(signum:longint;Handler:signalhandler):signalhandler;


{
 $Log$
 Revision 1.2  2003-06-01 16:28:41  marco
  * Enhancements to make the compiler baseunix using.

 Revision 1.1  2002/12/18 16:49:02  marco
  * New RTL. Linux system unit and baseunix operational.


}

 