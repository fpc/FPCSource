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

{$if defined(FreeBSD) or defined(DragonFly)} // tested on x86
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef NetBSD} // from a sparc dump.
function geterrnolocation: Plibcint; cdecl;external clib name '__errno';
{$else}
{$ifdef Darwin}
function geterrnolocation: Plibcint; cdecl;external clib name '__error';
{$else}
{$ifdef OpenBSD}

var libcerrno : libcint; cvar;

function geterrnolocation: Plibcint; cdecl;

begin
 geterrnolocation:=@libcerrno;
end;

{$else}
{$endif}
{$endif}
{$endif}
{$endif}

function geterrno:libcint; [public, alias: 'FPC_SYS_GETERRNO'];

begin
 geterrno:=geterrnolocation^;
end;

procedure seterrno(err:libcint); [public, alias: 'FPC_SYS_SETERRNO'];
begin
  geterrnolocation^:=err;
end;

{$else}
threadvar
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
{$I osmacro.inc}

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


{*****************************************************************************
                            Error conversion
*****************************************************************************}

{
  The lowlevel file functions should take care of setting the InOutRes to the
  correct value if an error has occurred, else leave it untouched
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
   ESysEBusy,
   ESysENOTDIR,        // busy, enotdir, mantis #25931
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
                          Low Level File Routines
*****************************************************************************}

Function Do_IsDevice(Handle:Longint):boolean;
{
  Interface to Unix ioctl call.
  Performs various operations on the filedescriptor Handle.
  Ndx describes the operation to perform.
  Data points to data needed for the Ndx function. The structure of this
  data is function-dependent.
}
CONST
  IOCtl_TCGETS=$40000000+$2C7400+ 19;
var
  Data : array[0..255] of byte; {Large enough for termios info}
begin
  Do_IsDevice:=(Fpioctl(handle,IOCTL_TCGETS,@data)<>-1);
end;




