{****************************************************************************

   $Id$

   Common types, and definitions

   Copyright (c) 1997 Balazs Scheidler (bazsi@balabit.hu)

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Library General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.


   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Library General Public License for more details.

   You should have received a copy of the GNU Library General Public
   License along with this library; if not, write to the Free
   Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
   $Id$
 ****************************************************************************

 Changelog:

   Common version 0.2.6

   Date       Version  Who     Comments
   07/12/97   0.1      Bazsi   Initial implementation (bazsi)
   07/18/97   0.2      Bazsi   Linux specific error codes added
   07/18/97   0.2.1    Bazsi   Some syntactical errors removed...
   07/28/97   0.2.2    Bazsi   Base error code for Video registered
   07/29/97   0.2.3    Bazsi   Some basic types added (PByte, PWord etc)
   08/08/97   0.2.4    Bazsi   Finalized error handling code (Brad's code added)
   08/27/97   0.2.5    Bazsi   BP doesn't like function types as return-values
                               for functions, returning Pointer instead
   09/06/97   0.2.6    Bazsi   Added base error code for Keyboard
   11/06/97   0.2.7    Bazsi   Added base error code for FileCtrl

 Todo:

 *  Some types found in the DOS unit should be included here (TextRec maybe
    renamed to TTextRec, FileRec etc.)
 *  Other platform specific error codes.

 ****************************************************************************}
unit ApiComm;

interface

{$i platform.inc}

const
  { Error codes }
  errOk                 = 0;
  errVioBase            = 1000;
  errKbdBase            = 1010;
  errFileCtrlBase	= 1020;
  errMouseBase          = 1030;
  { The following ranges have been defined for error codes:
     0      - 1000           OS dependent codes
     1000   - 10000          API reserved codes
     10000  -                Add-on unit errors
    Before anyone adding a unit, contact bazsi@tas.vein.hu to assign a base
    error code, to avoid collisions.

    The symbolic names of error codes should be defined in the unit which uses
    those error codes, except for OS dependent ones, which should be defined here
    enclosed in IFDEFs. (The reason is that not always you can't always decide
    which error-code belongs to one unit or the other) }

{$IFDEF OS_Unix}
  { for a more complete description of each error check /usr/include/asm/errno.h }
  errNotPermitted         =   1;
  errFileNotFound         =   2;
  errNoSuchProcess        =   3;
  errInterruptedCall      =   4;
  errIOError              =   5;
  errNoDevAddr            =   6;
  errTooManyArguments     =   7;
  errExecError            =   8;
  errBadFileHandle        =   9;
  errNoChild              =  10;
  errTryAgain             =  11;
  errWouldBlock           =  errTryAgain;
  errOutOfMemory          =  12;
  errNoPermission         =  13;
  errInvalidAddress       =  14;  { Invalid pointer passed to kernel }
  errNotBlockDev          =  15;
  errDeviceBusy           =  16;
  errFileExists           =  17;
  errCrossDevice          =  18;
  errNoSuchDev            =  19;
  errNotDirectory         =  20;
  errIsDirectory          =  21;
  errInvalidArgument      =  22;
  errFileTableOverflow    =  23;
  errTooManyOpenFiles     =  24;
  errNotATTY              =  25;
  errTextBusy             =  26;
  errFileTooLarge         =  27;
  errDiskFull             =  28;
  errIllegalSeek          =  29;
  errReadOnlyFS           =  30;
  errTooManyLinks         =  31;
  errBrokenPipe		  =  32;
  errMathDomain           =  33;  { Math domain error, what does this mean? }
  errMathRange            =  34;  { Math result out of range }
  errDeadLock             =  35;
  errFileNameTooLong      =  36;
  errNoLock               =  37;  { No record locks available }
  errNotImplemented       =  38;  { Kernel function not implemented }
  errDirNotEmpty          =  39;
  errSymlinkLoop          =  40;
  errNoMessage            =  41;  { ??? maybe the IPC getmsg call returns this }
  { Here are some errors that are too cryptic for me, I think they are not used
    under Linux, only on some mainframes (channel errors etc) }
  errBadFont              =  59;
  errNotStream            =  60;
  errNoData               =  61;
  errTimeOut              =  62;
  errNoMoreStreams        =  63;
  errNoNetwork            =  64;
  errPackageNotInstalled  =  65; { ??? }
  errRemote               =  66;
  errSevereLink           =  67;
  errAdvertise            =  68; { Advertise error??? }
  errSrMount              =  69;
  errCommunication        =  70; { Communication error on send? }
  errProtocol             =  71; { Protocol error }
  errMuiltiHop            =  72;
  errDotDot               =  73; { RFS specific error ???}
  errBadMessage           =  74; { Not a data message }
  errOverflow             =  75;
  errNotUnique            =  76; { Network name not unique }
  errBadFileHandleState   =  77;
  errRemoteChange         =  78; { Remote address changed }
  errLibAccess            =  79; { Cannot access the needed shared lib }
  errLibCorrupt           =  80; { Shared library corrupted }
  errLibScn               =  81;
  errLibTooMany           =  82; { Too many shared libraries }
  errLibExec              =  83; { Attempting to execute a shared lib }
  errIllegalSequence      =  84; { Illegal byte sequence ??? }
  errRestart              =  85; { interrupted system call should be restarted }
  errStreamPipe           =  86;
  errTooManyUsers         =  87;
  errNotSocket            =  88;
  errDestAddrRequired     =  89;
  errMessageTooLong       =  90;
  errProtocolType         =  91;
  errNoSuchProtocol       =  92;
  errProtocolNotSupported =  93;
  errSocketTypeNotSupported = 94;
  errOperationNotSupported=  95;
  errPFamilyNotSupported  =  96; { Protocol family not supported }
  errAFamilyNotSupported  =  97; { Address family not supported }
  errAddressInUse         =  98;
  errAddressNotAvailable  =  99;
  errNetDown              = 100;
  errNetUnreachable       = 101;
  errNetReset             = 102;
  errConnAborted          = 103;
  errConnReset            = 104;
  errNoBufs               = 105;
  errAlreadyConn          = 106;
  errNotConn              = 107;
  errShutdown             = 108;
  errTooManyRefs          = 109;
  errConnTimeOut          = 110;
  errConnRefused          = 111;
  errHostDown             = 112;
  errNoRoute              = 113; { No route to host }
  errOperationProgress    = 114; { Operation already in progress }
  errStaleNFSHandle       = 115;
  errStrucClean           = 116; { Structure needs cleaning ? }
  { Xenix specific codes left out }
  errRemoteIO             = 121;
  errQuotaExceeded        = 122;
{$ENDIF}
{$IFDEF OS_DOS}
  { DOS specific error-codes to be added }
{$ENDIF}



type
{$IFDEF BIT_32}
  CPUWord = Longint;
  CPUInt = Longint;
{$ELSE}
  CPUWord = Word;
  CPUInt = Integer;
{$ENDIF}

  PByte = ^Byte;
  PWord = ^Word;
  PLongint = ^Longint;

{ This code is taken from Brad Williams code, with some modifications }
type
  TErrorHandlerReturnValue = (errRetry, errAbort, errContinue);
  { errRetry = retry the operation,
    errAbort = abort, return error code,
    errContinue = abort, without returning errorcode }

  TErrorHandler = function (Code: Longint; Info: Pointer): TErrorHandlerReturnValue;
    { ErrorHandler is the standard procedural interface for all error functions.
      Info may contain any data type specific to the error code passed to the
      function. }

function GetErrorCode: Longint;
{ Returns the last error code, and resets it to 0 (errOK) }
function GetErrorInfo: Pointer;
{ Returns the info assigned to the previous error, doesn't reset the value to nil }
{$IFDEF PPC_BP}
function SetErrorHandler(AErrorHandler: TErrorHandler): Pointer;
{$ELSE}
 {$IFDEF PPC_VIRTUAL}
function SetErrorHandler(AErrorHandler: TErrorHandler): Pointer;
 {$ELSE}
function SetErrorHandler(AErrorHandler: TErrorHandler): TErrorHandler;
 {$ENDIF}
{$ENDIF}
{ Sets ErrorHandler to AErrorHandler, and returns the old one }
function DefaultErrorHandler(AErrorCode: Longint; AErrorInfo: Pointer): TErrorHandlerReturnValue;
{ Default error handler, simply sets error code, and returns errContinue }

const
  ErrorCode: Longint = errOk;
  ErrorInfo: Pointer = nil;
  ErrorHandler: TErrorHandler = DefaultErrorHandler;

implementation

function GetErrorCode: Longint;
begin
  GetErrorCode := ErrorCode;
  ErrorCode := 0;
end;

function GetErrorInfo: Pointer;
begin
  GetErrorInfo := ErrorInfo;
end;

{$IFDEF PPC_BP}
function SetErrorHandler(AErrorHandler: TErrorHandler): Pointer;
begin
  SetErrorHandler := @ErrorHandler;
  ErrorHandler := AErrorHandler;
end;
{$ELSE}
 {$IFDEF PPC_VIRTUAL}
function SetErrorHandler(AErrorHandler: TErrorHandler): Pointer;
begin
  SetErrorHandler := @ErrorHandler;
  ErrorHandler := AErrorHandler;
end;
 {$ELSE}
function SetErrorHandler(AErrorHandler: TErrorHandler): TErrorHandler;
begin
  SetErrorHandler := ErrorHandler;
  ErrorHandler := AErrorHandler;
end;
 {$ENDIF}
{$ENDIF}

function DefaultErrorHandler(AErrorCode: Longint; AErrorInfo: Pointer): TErrorHandlerReturnValue;
begin
  ErrorCode := AErrorCode;
  ErrorInfo := AErrorInfo;
  DefaultErrorHandler := errAbort; { return error code }
end;

end.
{
  $Log$
  Revision 1.3  2000-11-14 11:07:18  pierre
   * fix comment bug

  Revision 1.2  2000/11/13 14:35:57  marco
   * Unix Renamefest for defines.

  Revision 1.1  2000/07/13 06:29:38  michael
  + Initial import

  Revision 1.2  2000/07/09 07:41:47  hajny
    * changes needed for VP/2 to compile it

  Revision 1.1  2000/02/29 11:43:16  pierre
    Common renamed APIComm to avoid problems with free vision

  Revision 1.1  2000/01/06 01:20:31  peter
    * moved out of packages/ back to topdir

  Revision 1.1  1999/12/23 19:36:47  peter
    * place unitfiles in target dirs

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.1  1998/12/04 12:48:24  peter
    * moved some dirs

  Revision 1.5  1998/10/28 00:02:05  peter
    + mouse
    + video.clearscreen, video.videobufsize

  Revision 1.4  1998/10/26 11:22:48  peter
    * updates


   Date       Version  Who     Comments
   07/12/97   0.1      Bazsi   Initial implementation (bazsi)
   07/18/97   0.2      Bazsi   Linux specific error codes added
   07/18/97   0.2.1    Bazsi   Some syntactical errors removed...
   07/28/97   0.2.2    Bazsi   Base error code for Video registered
   07/29/97   0.2.3    Bazsi   Some basic types added (PByte, PWord etc)
   08/08/97   0.2.4    Bazsi   Finalized error handling code (Brad's code added)
   08/27/97   0.2.5    Bazsi   BP doesn't like function types as return-values
                               for functions, returning Pointer instead
   09/06/97   0.2.6    Bazsi   Added base error code for Keyboard
   11/06/97   0.2.7    Bazsi   Added base error code for FileCtrl
}
