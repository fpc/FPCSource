{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001-2005 by Free Pascal development team

    Low level system functions for MacOS

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{*********************** MacOS API *********************}

{This implementation uses StdCLib: }
{$define MACOS_USE_STDCLIB}

{Some MacOS API routines and StdCLib included for internal use:}
{$I macostp.inc}

{Note, because the System unit is the most low level, it should not
depend on any other units, and thus the macos api must be accessed
as an include file and not a unit.}

{The reason StdCLib is used is that it can easily be connected
to either SIOW or, in case of MPWTOOL, to MPW }

{If the Apples Universal Interfaces are used, the qd variable is required
to be allocated somewhere, so we do it here for the convenience to the user.}

var
  qd: QDGlobals; cvar;


{$ifdef MACOS_USE_STDCLIB}

{************** API to StdCLib in MacOS ***************}
{The reason StdCLib is used is that it can easily be connected
to either SIOW or, in case of MPWTOOL, to MPW }

{$endif}


{*********************** Macutils *********************}

{And also include the same utilities as in the macutils.pp unit.}

var
  {emulated working directory}
  workingDirectorySpec: FSSpec; cvar;

  {The above variable is also declared in macutils.pp as external. Declared }
  {here to be available to macutils.inc and below in this file.}

{$I macutils.inc}

{******************************************************}

function GetAppFileLocation (var spec: FSSpec): Boolean;
{Requires >= System 7}

  var
   PSN: ProcessSerialNumber;
   info: ProcessInfoRec;
   appFileRefNum: Integer;
   appName: Str255;
   dummy: Mac_Handle;

begin
  begin
    PSN.highLongOfPSN := 0;
    PSN.lowLongOfPSN := kCurrentProcess;
    info.processInfoLength := SizeOf(info);
    info.processName := nil;
    info.processAppSpec := @spec;
    if GetProcessInformation(PSN, info) = noErr then
      begin
        spec.name := '';
        GetAppFileLocation := true;
      end
    else
      GetAppFileLocation := false;
  end
end;

Procedure Errno2InOutRes;
{
  Convert ErrNo error to the correct InOutRes value.
  It seems that some of the errno is, in macos,
  used for other purposes than its original definition.
}

begin
  if errno = 0 then { Else it will go through all the cases }
    exit;
  case Errno of
   Sys_ENFILE,
   Sys_EMFILE : Inoutres:=4;
   Sys_ENOENT : Inoutres:=2;
    Sys_EBADF : Inoutres:=6;
   Sys_ENOMEM,
   Sys_EFAULT : Inoutres:=217; //TODO Exchange to something better
   Sys_EINVAL : Inoutres:=218; //TODO RTE 218 doesn't exist
   Sys_EAGAIN,
   Sys_ENOSPC : Inoutres:=101;
  Sys_ENOTDIR : Inoutres:=3;
    Sys_EPERM,
    Sys_EROFS,
   Sys_EEXIST,
   Sys_EISDIR,
    Sys_EINTR,  //Happens when attempt to rename a file fails
    Sys_EBUSY,  //Happens when attempt to remove a locked file
   Sys_EACCES,
   Sys_EMLINK : Inoutres:=5; //Happens when attempt to remove open file
    Sys_ENXIO : InOutRes:=152;
   Sys_ESPIPE : InOutRes:=156; //Illegal seek
  else
    InOutRes := Integer(errno);//TODO Exchange to something better
  end;
  errno:=0;
end;

Procedure OSErr2InOutRes(err: OSErr);
begin
  InOutRes:= MacOSErr2RTEerr(err);
end;

{*****************************************************************************
                              MacOS specific functions
*****************************************************************************}
var
  defaultCreator: OSType =  $4D505320; {'MPS '   MPW Shell}
  //defaultCreator: OSType =  $74747874; {'ttxt'   Simple Text}
  defaultFileType: OSType = $54455854; {'TEXT'}

procedure Yield;

begin
  if StandAlone = 0 then
    SpinCursor(1);
end;

procedure SetDefaultMacOSFiletype(ftype: ShortString);

begin
  if Length(ftype) = 4 then
    defaultFileType:= PLongWord(@ftype[1])^;
end;

procedure SetDefaultMacOSCreator(creator: ShortString);

begin
  if Length(creator) = 4 then
    defaultCreator:= PLongWord(@creator[1])^;
end;




