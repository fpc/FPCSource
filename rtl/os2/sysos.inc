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

type
  TByteArray = array [0..$FFFF] of byte;
  PByteArray = ^TByteArray;

  TSysThreadIB = record
    TID,
    Priority,
    Version: cardinal;
    MCCount,
    MCForceFlag: word;
  end;
  PSysThreadIB = ^TSysThreadIB;

  TThreadInfoBlock = record
    PExChain,
    Stack,
    StackLimit: pointer;
    TIB2: PSysThreadIB;
    Version,
    Ordinal: cardinal;
  end;
  PThreadInfoBlock = ^TThreadInfoBlock;
  PPThreadInfoBlock = ^PThreadInfoBlock;

  TProcessInfoBlock = record
    PID,
    ParentPid,
    Handle: cardinal;
    Cmd,
    Env: PByteArray;
    Status,
    ProcType: cardinal;
  end;
  PProcessInfoBlock = ^TProcessInfoBlock;
  PPProcessInfoBlock = ^PProcessInfoBlock;

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


{
Already declared in interface part:
type
  TDosOpenL = function (FileName: PChar; var Handle: THandle;
                        var Action: cardinal; InitSize: int64;
                        Attrib, OpenFlags, FileMode: cardinal;
                                                 EA: pointer): cardinal; cdecl;

  TDosSetFilePtrL = function (Handle: THandle; Pos: int64; Method: cardinal;
                                        var PosActual: int64): cardinal; cdecl;

  TDosSetFileSizeL = function (Handle: THandle; Size: int64): cardinal; cdecl;
}

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


(* Types and constants for exception handler support *)
const
  deHardErr           = 1;    {Pop-ups for hard errors are enabled, to disable
                               do not give this switch.}
  deDisableExceptions = 2;    {Pop-ups for exceptions are disabled, to enable
                               do not give this switch.}
  MaxExceptionParameters = 4; {Enough for all system exceptions.}

  Xcpt_Continue_Search            = $00000000;
  Xcpt_Continue_Execution         = $ffffffff;
  Xcpt_Continue_Stop              = $00716668;

  Xcpt_Signal_Intr                = 1;
  Xcpt_Signal_KillProc            = 3;
  Xcpt_Signal_Break               = 4;

  Xcpt_Fatal_Exception            = $c0000000;
  Xcpt_Severity_Code              = $c0000000;
  Xcpt_Customer_Code              = $20000000;
  Xcpt_Facility_Code              = $1fff0000;
  Xcpt_Exception_Code             = $0000ffff;

  Xcpt_Unknown_Access             = $00000000;
  Xcpt_Read_Access                = $00000001;
  Xcpt_Write_Access               = $00000002;
  Xcpt_Execute_Access             = $00000004;
  Xcpt_Space_Access               = $00000008;
  Xcpt_Limit_Access               = $00000010;
  Xcpt_Data_Unknown               = $ffffffff;

  Xcpt_Guard_Page_Violation       = $80000001;
  Xcpt_Unable_To_Grow_Stack       = $80010001;
  Xcpt_Access_Violation           = $c0000005;
  Xcpt_In_Page_Error              = $c0000006;
  Xcpt_Illegal_Instruction        = $c000001c;
  Xcpt_Invalid_Lock_Sequence      = $c000001d;
  Xcpt_Noncontinuable_Exception   = $c0000024;
  Xcpt_Invalid_Disposition        = $c0000025;
  Xcpt_Unwind                     = $c0000026;
  Xcpt_Bad_Stack                  = $c0000027;
  Xcpt_Invalid_Unwind_Target      = $c0000028;
  Xcpt_Array_Bounds_Exceeded      = $c0000093;
  Xcpt_Float_Denormal_Operand     = $c0000094;
  Xcpt_Float_Divide_By_Zero       = $c0000095;
  Xcpt_Float_Inexact_Result       = $c0000096;
  Xcpt_Float_Invalid_Operation    = $c0000097;
  Xcpt_Float_Overflow             = $c0000098;
  Xcpt_Float_Stack_Check          = $c0000099;
  Xcpt_Float_Underflow            = $c000009a;
  Xcpt_Integer_Divide_By_Zero     = $c000009b;
  Xcpt_Integer_Overflow           = $c000009c;
  Xcpt_Privileged_Instruction     = $c000009d;
  Xcpt_Datatype_Misalignment      = $c000009e;
  Xcpt_Breakpoint                 = $c000009f;
  Xcpt_Single_Step                = $c00000a0;
  Xcpt_Process_Terminate          = $c0010001;
  Xcpt_Async_Process_Terminate    = $c0010002;
  Xcpt_Signal                     = $c0010003;

  Context_Control        = $00000001; { SS:ESP, CS:EIP, EFLAGS and EBP set }
  Context_Integer        = $00000002; { EAX, EBX, ECX, EDX, ESI and EDI set }
  Context_Segments       = $00000004; { DS, ES, FS, and GS set }
  Context_Floating_Point = $00000008; { numeric coprocessor state set }
  Context_Full           = Context_Control or
                           Context_Integer or
                           Context_Segments or
                           Context_Floating_Point;

type
  PExceptionRegistrationRecord = ^TExceptionRegistrationRecord;
  PExceptionReportRecord = ^TExceptionReportRecord;
  PContextRecord = ^TContextRecord;

  TExceptionHandler = function (Report: PExceptionReportRecord;
                                RegRec: PExceptionRegistrationRecord;
                                Context: PContextRecord;
                                DispContext: pointer): cardinal; cdecl;

  TExceptionRegistrationRecord = record
    Prev_Structure: PExceptionRegistrationRecord;
    ExceptionHandler: TExceptionHandler;
  end;

  TExceptionReportRecord = record
    Exception_Num,
    HandlerFlags: cardinal;
    Nested_RepRec: PExceptionReportRecord;
    Address: pointer;
    ParamCount: cardinal;
    Parameters: array [0..MaxExceptionParameters] of cardinal;
  end;

  TContextRecord = packed record
    ContextFlags: cardinal;
    Env: array [1..7] of cardinal;
    FPUStack: array [0..7] of extended;
    Reg_GS,
    Reg_FS,
    Reg_ES,
    Reg_DS,
    Reg_EDI,
    Reg_ESI,
    Reg_EAX,
    Reg_EBX,
    Reg_ECX,
    Reg_EDX,
    Reg_EBP,
    Reg_EIP,
    Reg_CS,
    Flags,
    Reg_ESP,
    Reg_SS: cardinal;
  end;


function DosSetExceptionHandler (var RegRec: TExceptionRegistrationRecord):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 354;

function DosUnsetExceptionHandler (var RegRec: TExceptionRegistrationRecord):
                                                               cardinal; cdecl;
external 'DOSCALLS' index 355;

{Full screen applications can get Ctrl-C and Ctrl-Break focus. For all
 processes sharing one screen, only one can have Ctrl-C focus.
 Enable     = 0 = Release focus, 1 = Get focus.
 Times      = Number of times focus has been get minus number of times it
              has been released.}
function DosSetSignalExceptionFocus (Enable: cardinal;
                                         var Times: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 378;

{Tell we want further signal exceptions.
 SignalNum  = Signal number to acknowlegde.}
function DosAcknowledgeSignalException (SignalNum: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 418;

function DosError (Error: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 212;


type
  TWinMessageBox = function (Parent, Owner: cardinal;
         BoxText, BoxTitle: PChar; Identity, Style: cardinal): cardinal; cdecl;
  TWinInitialize = function (Options: cardinal): cardinal; cdecl;
  TWinCreateMsgQueue = function (Handle: cardinal; cmsg: longint): cardinal;
                                                                         cdecl;

const
  ErrorBufferLength = 1024;
  mb_OK = $0000;
  mb_Error = $0040;
  mb_Moveable = $4000;
  MBStyle = mb_OK or mb_Error or mb_Moveable;

  mfPag_Read      = $00001;   {Give read access to memory.}
  mfPag_Write     = $00002;   {Give write access to memory.}
  mfPag_Execute   = $00004;   {Allow code execution in memory.}
  mfPag_Guard     = $00008;   {Used for dynamic memory growing. Create
                               uncommitted memory and make the first
                               page guarded. Once it is accessed it
                               will be made committed, and the next
                               uncommitted page will be made guarded.}
  mfPag_Commit    = $00010;   {Make the memory committed.}
  mfPag_Decommit  = $00020;   {Decommit the page.}
  mfObj_Tile      = $00040;   {Also allocate 16-bit segments of 64k
                               which map the memory. (Makes 16<>32 bit
                               pointer conversion possible.}
  mfObj_Protected = $00080;
  mfObj_Gettable  = $00100;
  mfObj_Giveable  = $00200;
  mfObj_Any       = $00400;   {Allow using high memory (> 512 MB).}
  mfPag_Default   = $00400;
  mfPag_Shared    = $02000;
  mfPag_Free      = $04000;
  mfPag_Base      = $10000;

  mfSub_Init      = $00001;   {Use base, if not set, choose a base
                               address yourself.}
  mfSub_Grow      = $00002;   {Grow the specified heap, instead of
                               allocating it. Ignore mfSub_Init.}
  mfSub_Sparse    = $00004;
  mfSub_Serialize = $00008;

function DosQueryMem (P: pointer; var Size, Flag: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 306;
