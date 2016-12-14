{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by Free Pascal development team

    This file implements all the base types and limits required
    for a minimal POSIX compliant subset required to port the compiler
    to a new OS.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$ifdef FPC_USE_LIBC}

const clib = 'c';

type libcint=longint;
     plibcint=^libcint;

function geterrnolocation: Plibcint; cdecl;external clib name'__errno_location';

function geterrno:libcint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 geterrno:=geterrnolocation^;
end;

procedure seterrno(err:libcint); [public, alias: 'FPC_SYS_SETERRNO']; register;
begin
  geterrnolocation^:=err;
end;

{$else}

ThreadVar
  Errno : longint;

function geterrno:longint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 GetErrno:=Errno;
end;

procedure seterrno(err:longint); [public, alias: 'FPC_SYS_SETERRNO'];

begin
 Errno:=err;
end;
{$endif}

{ OS dependant parts  }

{$I errno.inc}                          // error numbers
{$I ostypes.inc}                        // c-types, unix base types, unix base structures

{$ifdef FPC_USE_LIBC}
  {$Linklib c}
  {$i oscdeclh.inc}
  {$i oscdecl.inc}
{$else}
  {$I syscallh.inc}
  {$I syscall.inc}
  {$I sysnr.inc}
  {$I ossysc.inc}
  {$I syscgen.inc}
{$endif}

{$I osmacro.inc}

{*****************************************************************************
                            Error conversion
*****************************************************************************}

{
  The lowlevel file functions should take care of setting the InOutRes to the
  correct value if an error has occurred, else leave it untouched
}

function PosixToRunError  (PosixErrno : longint):word;
{
  Convert ErrNo error to the correct Inoutres value
}

var r:word; {Inoutres is declared as word.}

begin
(*
  if PosixErrNo=0 then {Else it will go through all the cases}
    exit(0);

  Statement commented out. It will not go through all the cases. (DM)
*)
  case PosixErrNo of
    ESysENFILE,
    ESysEMFILE:         r:=4;
    ESysENOENT:         r:=2;
    ESysEBADF:          r:=6;
    ESysENOMEM,
    ESysEFAULT:         r:=217;
    ESysEINVAL:         r:=218;
    ESysEPIPE,
    ESysEINTR,
    ESysEIO,
    ESysEAGAIN,
    ESysENOSPC:         r:=101;
    ESysENAMETOOLONG:   r:=3;
    ESysEROFS,
    ESysEEXIST,
    ESysENOTEMPTY,
    ESysEACCES:         r:=5;
    ESysEBusy,
    ESysENOTDIR,        // busy, enotdir, mantis #25931      
    ESysEISDIR:         r:=5;
  else
    r:=PosixErrno;
  end;
  inoutres:=r;
  PosixToRunError:=r;
end;


function Errno2InoutRes : word;

begin
  Errno2InoutRes:=PosixToRunError(getErrno);
  InoutRes:=Errno2InoutRes;
end;


{*****************************************************************************
                          Low Level File Routines
*****************************************************************************}

Function Do_IsDevice(Handle:THandle):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
const
{$if defined(PowerPC) or defined(PowerPc64)}
  IOCtl_TCGETS=$402c7413;
{$else}
{$if defined(sparc)}
  IOCtl_TCGETS=$40245408;
{$else}
  IOCtl_TCGETS=$5401; // TCGETS is also in termios.inc, but the sysunix needs only this
{$endif}
{$endif}
var
  Data : array[0..255] of byte; {Large enough for termios info}
begin
  Do_IsDevice:=(Fpioctl(handle,IOCTL_TCGETS,@data)<>-1);
end;




