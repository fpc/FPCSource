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

var
  ProcessID: SizeUInt;

function GetProcessID:SizeUInt;
begin
 GetProcessID := ProcessID;
end;


type
  TSysDateTime=packed record
    Hour,
    Minute,
    Second,
    Sec100,
    Day,
    Month: byte;
    Year: word;
    TimeZone: smallint;
    WeekDay: byte;
  end;


procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                                        var Handle: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 318;

function DosQueryModuleHandle (DLLName: PChar; var Handle: THandle): cardinal;
                                                                         cdecl;
external 'DOSCALLS' index 319;

function DosQueryProcAddr (Handle, Ordinal: cardinal; ProcName: PChar;
                                        var Address: pointer): cardinal; cdecl;
external 'DOSCALLS' index 321;

function DosSetRelMaxFH (var ReqCount: longint; var CurMaxFH: cardinal):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 382;

function DosSetCurrentDir (Name:PChar): cardinal; cdecl;
external 'DOSCALLS' index 255;

procedure DosQueryCurrentDisk(var DiskNum:cardinal;var Logical:cardinal); cdecl;
external 'DOSCALLS' index 275;

function DosSetDefaultDisk (DiskNum:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 220;

{ This is not real prototype, but is close enough }
{ for us (the 2nd parameter is actually a pointer }
{ to a structure).                                }
function DosCreateDir (Name: PChar; P: pointer): cardinal; cdecl;
external 'DOSCALLS' index 270;

function DosDeleteDir (Name: PChar): cardinal; cdecl;
external 'DOSCALLS' index 226;

function DosQueryCurrentDir(DiskNum:cardinal;var Buffer;
                            var BufLen:cardinal): cardinal; cdecl;
external 'DOSCALLS' index 274;

function DosMove(OldFile,NewFile:PChar):cardinal; cdecl;
    external 'DOSCALLS' index 271;

function DosDelete(FileName:PChar):cardinal; cdecl;
    external 'DOSCALLS' index 259;

procedure DosExit(Action, Result: cardinal); cdecl;
    external 'DOSCALLS' index 234;

// EAs not used in System unit
function DosOpen(FileName:PChar;var Handle: THandle;var Action:cardinal;
                 InitSize,Attrib,OpenFlags,FileMode:cardinal;
                 EA:Pointer): cardinal; cdecl;
    external 'DOSCALLS' index 273;

function DosClose(Handle: THandle): cardinal; cdecl;
    external 'DOSCALLS' index 257;

function DosRead(Handle: THandle; Buffer: Pointer; Count: cardinal;
                                      var ActCount: cardinal): cardinal; cdecl;
    external 'DOSCALLS' index 281;

function DosWrite(Handle: THandle; Buffer: Pointer;Count: cardinal;
                                      var ActCount: cardinal): cardinal; cdecl;
    external 'DOSCALLS' index 282;

function DosSetFilePtr(Handle: THandle; Pos:longint; Method:cardinal;
                                     var PosActual: cardinal): cardinal; cdecl;
    external 'DOSCALLS' index 256;

function DosSetFileSize(Handle: THandle; Size: cardinal): cardinal; cdecl;
    external 'DOSCALLS' index 272;

function DosQueryHType(Handle: THandle; var HandType: cardinal;
                                          var Attr: cardinal): cardinal; cdecl;
    external 'DOSCALLS' index 224;

function DosQueryModuleName (Handle: THandle; NameLen: cardinal; Name: PChar):
                                                               cardinal; cdecl;
    external 'DOSCALLS' index 320;

function DosGetDateTime(var Buf:TSysDateTime): cardinal; cdecl;
    external 'DOSCALLS' index 230;


type
  TDosOpenL = function (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                                 EA: pointer): cardinal; cdecl;

  TDosSetFilePtrL = function (Handle: THandle; Pos: int64; Method: cardinal;
                                        var PosActual: int64): cardinal; cdecl;

  TDosSetFileSizeL = function (Handle: THandle; Size: int64): cardinal; cdecl;


function DummyDosOpenL (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                                 EA: pointer): cardinal; cdecl;
begin
  DummyDosOpenL := DosOpen (FileName, Handle, Action, InitSize, Attrib,
                                                      OpenFlags, FileMode, EA);
end;


function DummyDosSetFilePtrL (Handle: THandle; Pos: int64; Method: cardinal;
                                        var PosActual: int64): cardinal; cdecl;
var
  PosAct0: cardinal;
begin
  DummyDosSetFilePtrL := DosSetFilePtr (Handle, Pos, Method, PosAct0);
  PosActual := PosAct0;
end;


function DummyDosSetFileSizeL (Handle: THandle; Size: int64): cardinal; cdecl;
begin
  DummyDosSetFileSizeL := DosSetFileSize (Handle, Size);
end;


const
  DosOpenL: TDosOpenL = @DummyDosOpenL;
  DosSetFilePtrL: TDosSetFilePtrL = @DummyDosSetFilePtrL;
  DosSetFileSizeL: TDosSetFileSizeL = @DummyDosSetFileSizeL;

  OrdDosOpenL = 981;
  OrdDosSetFilePtrL = 988;
  OrdDosSetFileSizeL = 989;

   { converts an OS/2 error code to a TP compatible error }
   { code. Same thing exists under most other supported   }
   { systems.                                             }
   { Only call for OS/2 DLL imported routines             }
   Procedure Errno2InOutRes;
   Begin
     { errors 1..18 are the same as in DOS }
     case InOutRes of
      { simple offset to convert these error codes }
      { exactly like the error codes in Win32      }
      19..31 : InOutRes := InOutRes + 131;
      { gets a bit more complicated ... }
      32..33 : InOutRes := 5;
      38 : InOutRes := 100;
      39 : InOutRes := 101;
      112 : InOutRes := 101;
      110 : InOutRes := 5;
      114 : InOutRes := 6;
      290 : InOutRes := 290;
     end;
     { all other cases ... we keep the same error code }
   end;


{$IFDEF OS2EXCEPTIONS}
(*
The operating system defines a class of error conditions called exceptions, and specifies the default actions that are taken when these exceptions occur. The system default action in most cases is to terminate the thread that caused the exception.

Exception values have the following 32-bit format:

 3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
ÚÄÄÄÂÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³Sev³C³       Facility          ³               Code            ³
ÀÄÄÄÁÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


Sev Severity code. Possible values are described in the following list:

00 Success
01 Informational
10 Warning
11 Error

C Customer code flag.

Facility Facility code.

Code Facility's status code.

Exceptions that are specific to OS/2 Version 2.X (for example, XCPT_SIGNAL)
have a facility code of 1.

System exceptions include both synchronous and asynchronous exceptions.
Synchronous exceptions are caused by events that are internal to a thread's
execution. For example, synchronous exceptions could be caused by invalid
parameters, or by a thread's request to end its own execution.

Asynchronous exceptions are caused by events that are external to a thread's
execution. For example, an asynchronous exception can be caused by a user's
entering a Ctrl+C or Ctrl+Break key sequence, or by a process' issuing
DosKillProcess to end the execution of another process.

The Ctrl+Break and Ctrl+C exceptions are also known as signals, or as signal
exceptions.

The following tables show the symbolic names of system exceptions, their
numerical values, and related information fields.

Portable, Non-Fatal, Software-Generated Exceptions

ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿
³Exception Name                       ³Value     ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´
³XCPT_GUARD_PAGE_VIOLATION            ³0x80000001³
³  ExceptionInfo[0] - R/W flag        ³          ³
³  ExceptionInfo[1] - FaultAddr       ³          ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´
³XCPT_UNABLE_TO_GROW_STACK            ³0x80010001³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ


Portable, Fatal, Hardware-Generated Exceptions

ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³Exception Name                       ³Value     ³Related Trap ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_ACCESS_VIOLATION                ³0xC0000005³0x09, 0x0B,  ³
³  ExceptionInfo[0] - Flags           ³          ³0x0C, 0x0D,  ³
³    XCPT_UNKNOWN_ACCESS  0x0         ³          ³0x0E         ³
³    XCPT_READ_ACCESS     0x1         ³          ³             ³
³    XCPT_WRITE_ACCESS    0x2         ³          ³             ³
³    XCPT_EXECUTE_ACCESS  0x4         ³          ³             ³
³    XCPT_SPACE_ACCESS    0x8         ³          ³             ³
³    XCPT_LIMIT_ACCESS    0x10        ³          ³             ³
³  ExceptionInfo[1] - FaultAddr       ³          ³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_INTEGER_DIVIDE_BY_ZERO          ³0xC000009B³0            ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_DIVIDE_BY_ZERO            ³0xC0000095³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_INVALID_OPERATION         ³0xC0000097³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_ILLEGAL_INSTRUCTION             ³0xC000001C³0x06         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_PRIVILEGED_INSTRUCTION          ³0xC000009D³0x0D         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_INTEGER_OVERFLOW                ³0xC000009C³0x04         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_OVERFLOW                  ³0xC0000098³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_UNDERFLOW                 ³0xC000009A³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_DENORMAL_OPERAND          ³0xC0000094³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_INEXACT_RESULT            ³0xC0000096³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_FLOAT_STACK_CHECK               ³0xC0000099³0x10         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_DATATYPE_MISALIGNMENT           ³0xC000009E³0x11         ³
³  ExceptionInfo[0] - R/W flag        ³          ³             ³
³  ExceptionInfo[1] - Alignment       ³          ³             ³
³  ExceptionInfo[2] - FaultAddr       ³          ³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_BREAKPOINT                      ³0xC000009F³0x03         ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_SINGLE_STEP                     ³0xC00000A0³0x01         ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


Portable, Fatal, Software-Generated Exceptions

ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³Exception Name                       ³Value     ³Related Trap ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_IN_PAGE_ERROR                   ³0xC0000006³0x0E         ³
³  ExceptionInfo[0] - FaultAddr       ³          ³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_PROCESS_TERMINATE               ³0xC0010001³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_ASYNC_PROCESS_TERMINATE         ³0xC0010002³             ³
³  ExceptionInfo[0] - TID of          ³          ³             ³
³      terminating thread             ³          ³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_NONCONTINUABLE_EXCEPTION        ³0xC0000024³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_INVALID_DISPOSITION             ³0xC0000025³             ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


Non-Portable, Fatal Exceptions

ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
³Exception Name                       ³Value     ³Related Trap ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_INVALID_LOCK_SEQUENCE           ³0xC000001D³             ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄÄÄÄ´
³XCPT_ARRAY_BOUNDS_EXCEEDED           ³0xC0000093³0x05         ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÄÄÄÙ


Unwind Operation Exceptions

ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿
³Exception Name                       ³Value     ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´
³XCPT_UNWIND                          ³0xC0000026³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´
³XCPT_BAD_STACK                       ³0xC0000027³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´
³XCPT_INVALID_UNWIND_TARGET           ³0xC0000028³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ


Fatal Signal Exceptions

ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÂÄÄÄÄÄÄÄÄÄÄ¿
³Exception Name                       ³Value     ³
ÃÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÅÄÄÄÄÄÄÄÄÄÄ´
³XCPT_SIGNAL                          ³0xC0010003³
³  ExceptionInfo[ 0 ] - Signal        ³          ³
³      Number                         ³          ³
ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÁÄÄÄÄÄÄÄÄÄÄÙ
*)
{$ENDIF OS2EXCEPTIONS}


