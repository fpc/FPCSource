{
 $Id$
 ****************************************************************************

    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2002 by Free Pascal development team

    Free Pascal - OS/2 runtime library

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

****************************************************************************}

unit {$ifdef VER1_0}sysos2{$else}System{$endif};

interface

{Link the startup code.}
{$ifdef VER1_0}
 {$l prt1.oo2}
{$else}
 {$l prt1.o}
{$endif}

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
  {$define IODEBUG}
{$endif SYSTEMDEBUG}

{ $DEFINE OS2EXCEPTIONS}

{$I systemh.inc}

{$IFDEF OS2EXCEPTIONS}
(* Types and constants for exception handler support *)
type
{x}   PEXCEPTION_FRAME = ^TEXCEPTION_FRAME;
{x}   TEXCEPTION_FRAME = record
{x}     next : PEXCEPTION_FRAME;
{x}     handler : pointer;
{x}   end;

{$ENDIF OS2EXCEPTIONS}

{$I heaph.inc}

{Platform specific information}
type
  THandle = Longint;

const
  LineEnding = #13#10;
{ LFNSupport is defined separately below!!! }
  DirectorySeparator = '\';
  DriveSeparator = ':';
  PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }

type    Tos=(osDOS,osOS2,osDPMI);

const   os_mode: Tos = osOS2;
        first_meg: pointer = nil;

{$IFDEF OS2EXCEPTIONS}
{x}  System_exception_frame : PEXCEPTION_FRAME =nil;
{$ENDIF OS2EXCEPTIONS}

type    TByteArray = array [0..$ffff] of byte;
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

const   UnusedHandle=-1;
        StdInputHandle=0;
        StdOutputHandle=1;
        StdErrorHandle=2;

        LFNSupport: boolean = true;
        FileNameCaseSensitive: boolean = false;

        sLineBreak = LineEnding;
        DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

var
{ C-compatible arguments and environment }
  argc  : longint;external name '_argc';
  argv  : ppchar;external name '_argv';
  envp  : ppchar;external name '_environ';
  EnvC: cardinal; external name '_envc';

(* Pointer to the block of environment variables - used e.g. in unit Dos. *)
  Environment: PChar;

var
(* Type / run mode of the current process: *)
(* 0 .. full screen OS/2 session           *)
(* 1 .. DOS session                        *)
(* 2 .. VIO windowable OS/2 session        *)
(* 3 .. Presentation Manager OS/2 session  *)
(* 4 .. detached (background) OS/2 process *)
  ApplicationType: cardinal;

implementation

{$I system.inc}

var
    heap_base: pointer; external name '__heap_base';
    heap_brk: pointer; external name '__heap_brk';
    heap_end: pointer; external name '__heap_end';

(* Maximum heap size - only used if heap is allocated as continuous block. *)
{$IFDEF CONTHEAP}
    BrkLimit: cardinal;
{$ENDIF CONTHEAP}

procedure DosGetInfoBlocks (PATIB: PPThreadInfoBlock;
                            PAPIB: PPProcessInfoBlock); cdecl;
                            external 'DOSCALLS' index 312;

function DosLoadModule (ObjName: PChar; ObjLen: cardinal; DLLName: PChar;
                                        var Handle: cardinal): cardinal; cdecl;
external 'DOSCALLS' index 318;

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
function DosOpen(FileName:PChar;var Handle:longint;var Action:cardinal;
                 InitSize,Attrib,OpenFlags,FileMode:cardinal;
                 EA:Pointer):longint; cdecl;
    external 'DOSCALLS' index 273;

function DosClose(Handle:longint): longint; cdecl;
    external 'DOSCALLS' index 257;

function DosRead(Handle:longint; Buffer: Pointer;Count:longint;
                 var ActCount:longint):longint; cdecl;
    external 'DOSCALLS' index 281;
function DosWrite(Handle:longint; Buffer: Pointer;Count:longint;
                  var ActCount:longint):longint; cdecl;
    external 'DOSCALLS' index 282;

function DosSetFilePtr(Handle:longint;Pos:longint;Method:cardinal;
                       var PosActual:longint):longint; cdecl;
    external 'DOSCALLS' index 256;

function DosSetFileSize(Handle:longint;Size:cardinal):longint; cdecl;
    external 'DOSCALLS' index 272;

function DosQueryHType(Handle:longint;var HandType:longint;
                       var Attr:longint):longint; cdecl;
    external 'DOSCALLS' index 224;

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

function DosGetDateTime(var Buf:TSysDateTime):longint; cdecl;
    external 'DOSCALLS' index 230;

{This is the correct way to call external assembler procedures.}
procedure syscall; external name '___SYSCALL';

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

Exceptions that are specific to OS/2 Version 2.X (for example, XCPT_SIGNAL) have a facility code of 1.

System exceptions include both synchronous and asynchronous exceptions. Synchronous exceptions are caused by events that are internal to a thread's execution. For example, synchronous exceptions could be caused by invalid parameters, or by a thread's request to end its own execution.

Asynchronous exceptions are caused by events that are external to a thread's execution. For example, an asynchronous exception can be caused by a user's entering a Ctrl+C or Ctrl+Break key sequence, or by a process' issuing DosKillProcess to end the execution of another process.

The Ctrl+Break and Ctrl+C exceptions are also known as signals, or as signal exceptions.

The following tables show the symbolic names of system exceptions, their numerical values, and related information fields.

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



{****************************************************************************

                    Miscellaneous related routines.

****************************************************************************}

procedure system_exit;
begin
  DosExit(1{process}, exitcode);
end;

{$ASMMODE ATT}

function paramcount:longint;assembler;
asm
    movl argc,%eax
    decl %eax
end {['EAX']};

function args:pointer;assembler;
asm
  movl argv,%eax
end {['EAX']};


function paramstr(l:longint):string;

var p:^Pchar;

begin
  if (l>=0) and (l<=paramcount) then
  begin
    p:=args;
    paramstr:=strpas(p[l]);
  end
    else paramstr:='';
end;

procedure randomize;
var
  dt: TSysDateTime;
begin
  // Hmm... Lets use timer
  DosGetDateTime(dt);
  randseed:=dt.hour+(dt.minute shl 8)+(dt.second shl 16)+(dt.sec100 shl 32);
end;

{$ASMMODE ATT}

{****************************************************************************

                    Heap management releated routines.

****************************************************************************}


{ this function allows to extend the heap by calling
syscall $7f00 resizes the brk area}

function sbrk(size:longint):pointer;
{$IFDEF DUMPGROW}
var
  L: longword;
begin
  WriteLn ('Trying to grow heap by ', Size, ' to ', HeapSize + Size);
{$IFDEF CONTHEAP}
  WriteLn ('BrkLimit is ', BrkLimit);
{$ENDIF CONTHEAP}
  asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall     { result directly in EAX }
    inc %eax         { Result in EAX, -1 = error (has to be transformed to 0) }
    jz .LSbrk_End
    dec %eax         { No error - back to previous value }
.LSbrk_End:
    mov  %eax,L
  end ['eax', 'edx'];
  WriteLn ('New heap at ', L);
  Sbrk := pointer (L);
end;
{$ELSE DUMPGROW}
                                     assembler;
asm
    movl size,%edx
    movw $0x7f00,%ax
    call syscall
    inc %eax         { Result in EAX, -1 = error (has to be transformed to 0) }
    jz .LSbrk_End
    dec %eax         { No error - back to previous value }
.LSbrk_End:
end {['eax', 'edx']};
{$ENDIF DUMPGROW}

function getheapstart:pointer;assembler;

asm
    movl heap_base,%eax
end {['EAX']};

function getheapsize:longint;assembler;
asm
    movl heap_brk,%eax
end {['EAX']};

{$i heap.inc}

{****************************************************************************

                          Low Level File Routines

****************************************************************************}

procedure allowslash(p:Pchar);
{Allow slash as backslash.}
var i:longint;
begin
    for i:=0 to strlen(p) do
        if p[i]='/' then p[i]:='\';
end;

procedure do_close(h:longint);
begin
{ Only three standard handles under real OS/2 }
  if h>2 then
  begin
    InOutRes:=DosClose(h);
  end;
{$ifdef IODEBUG}
  writeln('do_close: handle=', H, ', InOutRes=', InOutRes);
{$endif}
end;

procedure do_erase(p:Pchar);
begin
  allowslash(p);
  inoutres:=DosDelete(p);
end;

procedure do_rename(p1,p2:Pchar);
begin
  allowslash(p1);
  allowslash(p2);
  inoutres:=DosMove(p1, p2);
end;

function do_read(h,addr,len:longint):longint;
Var
  T: Longint;
begin
{$ifdef IODEBUG}
  write('do_read: handle=', h, ', addr=', addr, ', length=', len);
{$endif}
  InOutRes:=DosRead(H, Pointer(Addr), Len, T);
  do_read:=T;
{$ifdef IODEBUG}
  writeln(', actual_len=', t, ', InOutRes=', InOutRes);
{$endif}
end;

function do_write(h,addr,len:longint) : longint;
Var
  T: Longint;
begin
{$ifdef IODEBUG}
  write('do_write: handle=', h, ', addr=', addr, ', length=', len);
{$endif}
  InOutRes:=DosWrite(H, Pointer(Addr), Len, T);
  do_write:=T;
{$ifdef IODEBUG}
  writeln(', actual_len=', t, ', InOutRes=', InOutRes);
{$endif}
end;

function do_filepos(handle:longint): longint;
var
  PosActual: Longint;
begin
  InOutRes:=DosSetFilePtr(Handle, 0, 1, PosActual);
  do_filepos:=PosActual;
{$ifdef IODEBUG}
  writeln('do_filepos: handle=', Handle, ', actual_pos=', PosActual, ', InOutRes=', InOutRes);
{$endif}
end;

procedure do_seek(handle,pos:longint);
var
  PosActual: Longint;
begin
  InOutRes:=DosSetFilePtr(Handle, Pos, 0 {ZeroBased}, PosActual);
{$ifdef IODEBUG}
  writeln('do_seek: handle=', Handle, ', pos=', pos, ', actual_pos=', PosActual, ', InOutRes=', InOutRes);
{$endif}
end;

function do_seekend(handle:longint):longint;
var
  PosActual: Longint;
begin
  InOutRes:=DosSetFilePtr(Handle, 0, 2 {EndBased}, PosActual);
  do_seekend:=PosActual;
{$ifdef IODEBUG}
  writeln('do_seekend: handle=', Handle, ', actual_pos=', PosActual, ', InOutRes=', InOutRes);
{$endif}
end;

function do_filesize(handle:longint):longint;
var aktfilepos:longint;
begin
  aktfilepos:=do_filepos(handle);
  do_filesize:=do_seekend(handle);
  do_seek(handle,aktfilepos);
end;

procedure do_truncate(handle,pos:longint);
begin
  InOutRes:=DosSetFileSize(Handle, Pos);
  do_seekend(handle);
end;

const
    FileHandleCount: cardinal = 20;

function Increase_File_Handle_Count: boolean;
var Err: word;
    L1: longint;
    L2: cardinal;
begin
  L1 := 10;
  if DosSetRelMaxFH (L1, L2) <> 0 then
    Increase_File_Handle_Count := false
  else
    if L2 > FileHandleCount then
    begin
      FileHandleCount := L2;
      Increase_File_Handle_Count := true;
    end
    else
      Increase_File_Handle_Count := false;
end;

procedure do_open(var f;p:pchar;flags:longint);
{
  filerec and textrec have both handle and mode as the first items so
  they could use the same routine for opening/creating.

  when (flags and $100)   the file will be append
  when (flags and $1000)  the file will be truncate/rewritten
  when (flags and $10000) there is no check for close (needed for textfiles)
}
// Helper constants
const
  fmShareCompat    = $0000;
  fmShareExclusive = $0010;
  fmShareDenyWrite = $0020;
  fmShareDenyRead  = $0030;
  fmShareDenyNone  = $0040;
var
  Action, Attrib, OpenFlags, FM: Cardinal;
begin
  // convert unix slashes to normal slashes
  allowslash(p);

  // close first if opened
  if ((flags and $10000)=0) then
  begin
    case filerec(f).mode of
      fminput,fmoutput,fminout : Do_Close(filerec(f).handle);
      fmclosed:;
    else
      begin
        inoutres:=102; {not assigned}
        exit;
      end;
    end;
  end;

  // reset file handle
  filerec(f).handle := UnusedHandle;

  Attrib:=0;
  OpenFlags:=0;

  // convert filesharing
  FM := Flags and $FF and not (8);
(* DenyNone if sharing not specified. *)
  if FM and 112 = 0 then
    FM := FM or 64;
  // convert filemode to filerec modes and access mode
  case (FM and 3) of
    0: filerec(f).mode:=fminput;
    1: filerec(f).mode:=fmoutput;
    2: filerec(f).mode:=fminout;
  end;

  if (flags and $1000)<>0 then
    OpenFlags:=OpenFlags or 2 {doOverwrite} or 16 {doCreate} // Create/overwrite
  else
    OpenFlags:=OpenFlags or 1 {doOpen}; // Open existing

  // Handle Std I/O
  if p[0]=#0 then
  begin
    case FileRec(f).mode of
      fminput :
        FileRec(f).Handle:=StdInputHandle;
      fminout, // this is set by rewrite
      fmoutput :
        FileRec(f).Handle:=StdOutputHandle;
      fmappend :
        begin
          FileRec(f).Handle:=StdOutputHandle;
          FileRec(f).mode:=fmoutput; // fool fmappend
        end;
    end;
    exit;
  end;

  Attrib:=32 {faArchive};

  InOutRes:=DosOpen(p, FileRec(F).Handle, Action, 0, Attrib, OpenFlags, FM, nil);

  // If too many open files try to set more file handles and open again
  if (InOutRes = 4) then
    if Increase_File_Handle_Count then
      InOutRes:=DosOpen(p, FileRec(F).Handle, Action, 0, Attrib, OpenFlags, FM, nil);

  If InOutRes<>0 then FileRec(F).Handle:=UnusedHandle;

  // If Handle created -> make some things
  if (FileRec(F).Handle <> UnusedHandle) then
  begin

    // Move to end of file for Append command
    if ((Flags and $100) <> 0) then
    begin
      do_seekend(FileRec(F).Handle);
      FileRec(F).Mode := fmOutput;
    end;

  end;

{$ifdef IODEBUG}
  writeln('do_open,', filerec(f).handle, ',', filerec(f).name, ',', filerec(f).mode, ', InOutRes=', InOutRes);
{$endif}
end;

function do_isdevice (Handle: longint): boolean;
var
  HT, Attr: longint;
begin
  do_isdevice:=false;
  If DosQueryHType(Handle, HT, Attr)<>0 then exit;
  if ht=1 then do_isdevice:=true;
end;
{$ASMMODE ATT}


{*****************************************************************************
                           UnTyped File Handling
*****************************************************************************}

{$i file.inc}

{*****************************************************************************
                           Typed File Handling
*****************************************************************************}

{$i typefile.inc}

{*****************************************************************************
                           Text File Handling
*****************************************************************************}

{$DEFINE EOF_CTRLZ}

{$i text.inc}

{****************************************************************************

                          Directory related routines.

****************************************************************************}

{*****************************************************************************
                           Directory Handling
*****************************************************************************}

procedure MkDir (const S: string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  If (s='') or (InOutRes <> 0) then
   exit;
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosCreateDir(buffer,nil);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
end;


procedure rmdir(const s : string);[IOCHECK];
var buffer:array[0..255] of char;
    Rc : word;
begin
  if (s = '.' ) then
    InOutRes := 16;
  If (s='') or (InOutRes <> 0) then
   exit;
      move(s[1],buffer,length(s));
      buffer[length(s)]:=#0;
      allowslash(Pchar(@buffer));
      Rc := DosDeleteDir(buffer);
      if Rc <> 0 then
       begin
         InOutRes := Rc;
         Errno2Inoutres;
       end;
end;

{$ASMMODE INTEL}

procedure ChDir (const S: string);[IOCheck];

var RC: cardinal;
    Buffer: array [0..255] of char;

begin
  If (s='') or (InOutRes <> 0) then exit;
  if (Length (S) >= 2) and (S [2] = ':') then
  begin
    RC := DosSetDefaultDisk ((Ord (S [1]) and not ($20)) - $40);
    if RC <> 0 then
      InOutRes := RC
    else
      if Length (S) > 2 then
      begin
        Move (S [1], Buffer, Length (S));
        Buffer [Length (S)] := #0;
        AllowSlash (PChar (@Buffer));
        RC := DosSetCurrentDir (@Buffer);
        if RC <> 0 then
        begin
          InOutRes := RC;
          Errno2InOutRes;
        end;
      end;
  end else begin
    Move (S [1], Buffer, Length (S));
    Buffer [Length (S)] := #0;
    AllowSlash (PChar (@Buffer));
    RC := DosSetCurrentDir (@Buffer);
    if RC <> 0 then
    begin
      InOutRes:= RC;
      Errno2InOutRes;
    end;
  end;
end;

{$ASMMODE ATT}

procedure GetDir (DriveNr: byte; var Dir: ShortString);
{Written by Michael Van Canneyt.}
var sof: Pchar;
    i:byte;
    l,l2:cardinal;
begin
    Dir [4] := #0;
    { Used in case the specified drive isn't available }
    sof:=pchar(@dir[4]);
    { dir[1..3] will contain '[drivenr]:\', but is not }
    { supplied by DOS, so we let dos string start at   }
    { dir[4]                                           }
    { Get dir from drivenr : 0=default, 1=A etc... }
    l:=255-3;
    InOutRes:=longint (DosQueryCurrentDir(DriveNr, sof^, l));
{$WARNING Result code should be translated in some cases!}
    { Now Dir should be filled with directory in ASCIIZ, }
    { starting from dir[4]                               }
    dir[0]:=#3;
    dir[2]:=':';
    dir[3]:='\';
    i:=4;
    {Conversion to Pascal string }
    while (dir[i]<>#0) do
        begin
            { convert path name to DOS }
            if dir[i]='/' then
            dir[i]:='\';
            dir[0]:=char(i);
            inc(i);
        end;
    { upcase the string (FPC function) }
    if drivenr<>0 then   { Drive was supplied. We know it }
        dir[1]:=chr(64+drivenr)
    else
        begin
            { We need to get the current drive from DOS function 19H  }
            { because the drive was the default, which can be unknown }
            DosQueryCurrentDisk(l, l2);
            dir[1]:=chr(64+l);
        end;
    if not (FileNameCaseSensitive) then dir:=upcase(dir);
end;


{*****************************************************************************

                        System unit initialization.

****************************************************************************}

{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

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
  WinInitialize: TWinInitialize = nil;
  WinCreateMsgQueue: TWinCreateMsgQueue = nil;
  WinMessageBox: TWinMessageBox = nil;
  EnvSize: cardinal = 0;

var
  ErrorBuf: array [0..ErrorBufferLength] of char;
  ErrorLen: longint;
  PMWinHandle: cardinal;

function ErrorWrite (var F: TextRec): integer;
{
  An error message should always end with #13#10#13#10
}
var
  P: PChar;
  I: longint;
begin
  if F.BufPos > 0 then
   begin
     if F.BufPos + ErrorLen > ErrorBufferLength then
       I := ErrorBufferLength - ErrorLen
     else
       I := F.BufPos;
     Move (F.BufPtr^, ErrorBuf [ErrorLen], I);
     Inc (ErrorLen, I);
     ErrorBuf [ErrorLen] := #0;
   end;
  if ErrorLen > 3 then
   begin
     P := @ErrorBuf [ErrorLen];
     for I := 1 to 4 do
      begin
        Dec (P);
        if not (P^ in [#10, #13]) then
          break;
      end;
   end;
   if ErrorLen = ErrorBufferLength then
     I := 4;
   if (I = 4) then
    begin
      WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
      ErrorLen := 0;
    end;
  F.BufPos := 0;
  ErrorWrite := 0;
end;

function ErrorClose (var F: TextRec): integer;
begin
  if ErrorLen > 0 then
   begin
     WinMessageBox (0, 0, @ErrorBuf, PChar ('Error'), 0, MBStyle);
     ErrorLen := 0;
   end;
  ErrorLen := 0;
  ErrorClose := 0;
end;

function ErrorOpen (var F: TextRec): integer;
begin
  TextRec(F).InOutFunc := @ErrorWrite;
  TextRec(F).FlushFunc := @ErrorWrite;
  TextRec(F).CloseFunc := @ErrorClose;
  ErrorOpen := 0;
end;


procedure AssignError (var T: Text);
begin
  Assign (T, '');
  TextRec (T).OpenFunc := @ErrorOpen;
  Rewrite (T);
end;

procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in a messagebox }
(*
  StdInputHandle := longint(GetStdHandle(cardinal(STD_INPUT_HANDLE)));
  StdOutputHandle := longint(GetStdHandle(cardinal(STD_OUTPUT_HANDLE)));
  StdErrorHandle := longint(GetStdHandle(cardinal(STD_ERROR_HANDLE)));

  if not IsConsole then
    begin
      if (DosLoadModule (nil, 0, 'PMWIN', PMWinHandle) = 0) and
       (DosQueryProcAddr (PMWinHandle, 789, nil, pointer (WinMessageBox)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 763, nil, pointer (WinInitialize)) = 0)
                                                                           and
       (DosQueryProcAddr (PMWinHandle, 716, nil, pointer (WinCreateMsgQueue))
                                                                           = 0)
        then
          begin
            WinInitialize (0);
            WinCreateMsgQueue (0, 0);
          end
        else
          HandleError (2);
     AssignError (StdErr);
     AssignError (StdOut);
     Assign (Output, '');
     Assign (Input, '');
   end
  else
   begin
*)
     OpenStdIO (Input, fmInput, StdInputHandle);
     OpenStdIO (Output, fmOutput, StdOutputHandle);
     OpenStdIO (StdOut, fmOutput, StdOutputHandle);
     OpenStdIO (StdErr, fmOutput, StdErrorHandle);
(*
   end;
*)
end;


function GetFileHandleCount: longint;
var L1: longint;
    L2: cardinal;
begin
    L1 := 0; (* Don't change the amount, just check. *)
    if DosSetRelMaxFH (L1, L2) <> 0 then GetFileHandleCount := 50
                                                 else GetFileHandleCount := L2;
end;

var TIB: PThreadInfoBlock;
    PIB: PProcessInfoBlock;

begin
    IsLibrary := FALSE;
{$ASMMODE INTEL}
    asm
    {Enable the brk area by initializing it with the initial heap size.}
        mov eax, 7F01h
        mov edx, heap_brk
        add edx, heap_base
        call syscall
        cmp eax, -1
        jnz @heapok
        push dword 204
        call HandleError
    @heapok:
{$IFDEF CONTHEAP}
{ Find out brk limit }
        mov eax, 7F02h
        mov ecx, 3
        call syscall
        jcxz @heaplimitknown
        mov eax, 0
    @heaplimitknown:
        mov BrkLimit, eax
{$ELSE CONTHEAP}
{ Change sbrk behaviour to allocate arbitrary (non-contiguous) memory blocks }
        mov eax, 7F0Fh
        mov ecx, 0Ch
        mov edx, 8
        call syscall
{$ENDIF CONTHEAP}
    end;

    (* Initialize the amount of file handles *)
    FileHandleCount := GetFileHandleCount;
    DosGetInfoBlocks (@TIB, @PIB);
    StackBottom := TIB^.Stack;
    Environment := pointer (PIB^.Env);
    ApplicationType := PIB^.ProcType;
    IsConsole := ApplicationType <> 3;
    exitproc:=nil;

    {Initialize the heap.}
    initheap;

    { ... and exceptions }
    SysInitExceptions;

    { ... and I/O }
    SysInitStdIO;

    { no I/O-Error }
    inoutres:=0;

{$ifdef HASVARIANT}
    initvariantmanager;
{$endif HASVARIANT}

{$IFDEF DUMPGROW}
 {$IFDEF CONTHEAP}
    WriteLn ('Initial brk size is ', GetHeapSize);
    WriteLn ('Brk limit is ', BrkLimit);
 {$ENDIF CONTHEAP}
{$ENDIF DUMPGROW}
end.
{
  $Log$
  Revision 1.56  2003-11-03 09:42:28  marco
   * Peter's Cardinal<->Longint fixes patch

  Revision 1.55  2003/11/02 00:51:17  hajny
    * corrections for do_open and os_mode back

  Revision 1.54  2003/10/28 14:57:31  yuri
  * do_* functions now native

  Revision 1.53  2003/10/27 04:33:58  yuri
  * os_mode removed (not required anymore)

  Revision 1.52  2003/10/25 22:45:37  hajny
    * file handling related fixes

  Revision 1.51  2003/10/19 12:13:41  hajny
    * UnusedHandle value made the same as with other targets

  Revision 1.50  2003/10/19 09:37:00  hajny
    * minor fix in non-default sbrk code

  Revision 1.49  2003/10/19 09:06:28  hajny
    * fix for terrible long-time bug in do_open

  Revision 1.48  2003/10/18 16:58:39  hajny
    * stdcall fixes again

  Revision 1.47  2003/10/16 15:43:13  peter
    * THandle is platform dependent

  Revision 1.46  2003/10/14 21:10:06  hajny
    * another longint2cardinal fix

  Revision 1.45  2003/10/13 21:17:31  hajny
    * longint to cardinal corrections

  Revision 1.44  2003/10/12 18:07:30  hajny
    * wrong use of Intel syntax

  Revision 1.43  2003/10/12 17:59:40  hajny
    * wrong use of Intel syntax

  Revision 1.42  2003/10/12 17:52:28  hajny
    * wrong use of Intel syntax

  Revision 1.41  2003/10/12 10:45:36  hajny
    * sbrk error handling corrected

  Revision 1.40  2003/10/07 21:26:35  hajny
    * stdcall fixes and asm routines cleanup

  Revision 1.39  2003/10/06 16:58:27  yuri
  * Another set of native functions.

  Revision 1.38  2003/10/06 14:22:40  yuri
  * Some emx code removed. Now withous so stupid error as with dos ;)

  Revision 1.37  2003/10/04 08:30:59  yuri
  * at&t syntax instead of intel syntax was used

  Revision 1.36  2003/10/03 21:46:41  peter
    * stdcall fixes

  Revision 1.35  2003/10/01 18:42:49  yuri
  * Unclosed comment

  Revision 1.34  2003/09/29 18:39:59  hajny
    * append fix applied to GO32v2, OS/2 and EMX

  Revision 1.33  2003/09/27 11:52:36  peter
    * sbrk returns pointer

  Revision 1.32  2003/03/30 09:20:30  hajny
    * platform extension unification

  Revision 1.31  2003/01/15 22:16:12  hajny
    * default sharing mode changed to DenyNone

  Revision 1.30  2002/12/15 22:41:41  hajny
    * First_Meg fixed + Environment initialization under Dos

  Revision 1.29  2002/12/08 16:39:58  hajny
    - WriteLn in GUI mode support commented out until fixed

  Revision 1.28  2002/12/07 19:17:14  hajny
    * GetEnv correction, better PM support, ...

  Revision 1.27  2002/11/17 22:31:02  hajny
    * type corrections (longint x cardinal)

  Revision 1.26  2002/10/27 14:29:00  hajny
    * heap management (hopefully) fixed

  Revision 1.25  2002/10/14 19:39:17  peter
    * threads unit added for thread support

  Revision 1.24  2002/10/13 09:28:45  florian
    + call to initvariantmanager inserted

  Revision 1.23  2002/09/07 16:01:25  peter
    * old logs removed and tabs fixed

  Revision 1.22  2002/07/01 16:29:05  peter
    * sLineBreak changed to normal constant like Kylix

  Revision 1.21  2002/04/21 15:54:20  carl
  + initialize some global variables

  Revision 1.20  2002/04/12 17:42:16  carl
  + generic stack checking

  Revision 1.19  2002/03/11 19:10:33  peter
    * Regenerated with updated fpcmake

  Revision 1.18  2002/02/10 13:46:20  hajny
    * heap management corrected (heap_brk)

}
