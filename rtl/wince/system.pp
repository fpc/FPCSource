{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl and Pavel Ozerski
    and Yury Sidorov member of the Free Pascal development team.

    FPC Pascal system unit for the WinCE.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
unit System;
interface

{$ifdef SYSTEMDEBUG}
  {$define SYSTEMEXCEPTIONDEBUG}
{$endif SYSTEMDEBUG}

{$ifdef cpui386}
  {$define Set_i386_Exception_handler}
{$endif cpui386}

{ include system-independent routine headers }
{$I systemh.inc}

const
 LineEnding = #13#10;
 LFNSupport = true;
 DirectorySeparator = '\';
 DriveSeparator = ':';
 PathSeparator = ';';
{ FileNameCaseSensitive is defined separately below!!! }
 maxExitCode = 65535;
 MaxPathLen = 260;

type
   PEXCEPTION_FRAME = ^TEXCEPTION_FRAME;
   TEXCEPTION_FRAME = record
     next : PEXCEPTION_FRAME;
     handler : pointer;
   end;

const
{ Default filehandles }
  UnusedHandle    : THandle = -1;
  StdInputHandle  : THandle = 0;
  StdOutputHandle : THandle = 0;
  StdErrorHandle  : THandle = 0;

  FileNameCaseSensitive : boolean = true;
  CtrlZMarksEOF: boolean = true; (* #26 not considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

  { Thread count for DLL }
  Thread_count : longint = 0;
  System_exception_frame : PEXCEPTION_FRAME =nil;

var
{ C compatible arguments }
  argc : longint;
  argv : ppchar;
{ Win32 Info }
  hprevinst,
  HInstance,
  MainInstance,
  DLLreason,DLLparam:longint;
  Win32StackTop : Dword;

type
  TDLL_Process_Entry_Hook = function (dllparam : longint) : longbool;
  TDLL_Entry_Hook = procedure (dllparam : longint);

const
  Dll_Process_Attach_Hook : TDLL_Process_Entry_Hook = nil;
  Dll_Process_Detach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Attach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Detach_Hook : TDLL_Entry_Hook = nil;

type
  HMODULE = THandle;

{ Wrappers for some WinAPI calls }
function EventModify(h: THandle; func: DWORD): LONGBOOL;
    stdcall; external KernelDLL name 'EventModify';
function TlsCall(p1, p2: DWORD): DWORD;
    stdcall; external KernelDLL name 'TlsCall';
function ResetEvent(h: THandle): LONGBOOL;
function SetEvent(h: THandle): LONGBOOL;
function GetCurrentProcessId:DWORD;
function Win32GetCurrentThreadId:DWORD;
function TlsAlloc : DWord;
function TlsFree(dwTlsIndex : DWord) : LongBool;

implementation

{ used by wstrings.inc because wstrings.inc is included before sysos.inc
  this is put here (FK) }
(*
function SysAllocStringLen(psz:pointer;len:dword):pointer;stdcall;
 external 'oleaut32.dll' name 'SysAllocStringLen';

procedure SysFreeString(bstr:pointer);stdcall;
 external 'oleaut32.dll' name 'SysFreeString';

function SysReAllocStringLen(var bstr:pointer;psz: pointer;
  len:dword): Integer; stdcall;external 'oleaut32.dll' name 'SysReAllocStringLen';
*)

{ include system independent routines }
{$I system.inc}

{*****************************************************************************
                      WinAPI wrappers implementation
*****************************************************************************}

const
  EVENT_PULSE =     1;
  EVENT_RESET =     2;
  EVENT_SET   =     3;

function ResetEvent(h: THandle): LONGBOOL;
begin
	ResetEvent := EventModify(h,EVENT_RESET);
end;

function SetEvent(h: THandle): LONGBOOL;
begin
	SetEvent := EventModify(h,EVENT_SET);
end;

const
{$ifdef CPUARM}
  UserKData = $FFFFC800;
{$else CPUARM}
  UserKData = $00005800;
{$endif CPUARM}
  SYSHANDLE_OFFSET = $004;
  SYS_HANDLE_BASE	 = 64;
  SH_CURTHREAD     = 1;
  SH_CURPROC       = 2;

type
  PHandle = ^THandle;

function GetCurrentProcessId:DWORD;
var
  p: PHandle;
begin
  p:=PHandle(UserKData+SYSHANDLE_OFFSET + SH_CURPROC*SizeOf(THandle));
  GetCurrentProcessId := p^;
end;

function Win32GetCurrentThreadId:DWORD;
var
  p: PHandle;
begin
  p:=PHandle(UserKData+SYSHANDLE_OFFSET + SH_CURTHREAD*SizeOf(THandle));
  Win32GetCurrentThreadId := p^;
end;

const
  TLS_FUNCALLOC = 0;
  TLS_FUNCFREE  = 1;

function TlsAlloc : DWord;
begin
  TlsAlloc := TlsCall(TLS_FUNCALLOC, 0);
end;

function TlsFree(dwTlsIndex : DWord) : LongBool;
begin
  TlsFree := LongBool(TlsCall(TLS_FUNCFREE, dwTlsIndex));
end;


{*****************************************************************************
                              Parameter Handling
*****************************************************************************}

var
  ModuleName : array[0..255] of char;

function GetCommandFile:pchar;
begin
  GetModuleFileName(0,@ModuleName,255);
  GetCommandFile:=@ModuleName;
end;


procedure setup_arguments;
var
  arglen,
  count   : longint;
  argstart,
  pc,arg  : pchar;
  quote   : char;
  argvlen : longint;

  procedure allocarg(idx,len:longint);
    var
      oldargvlen : longint;
    begin
      if idx>=argvlen then
       begin
         oldargvlen:=argvlen;
         argvlen:=(idx+8) and (not 7);
         sysreallocmem(argv,argvlen*sizeof(pointer));
         fillchar(argv[oldargvlen],(argvlen-oldargvlen)*sizeof(pointer),0);
       end;
      { use realloc to reuse already existing memory }
      { always allocate, even if length is zero, since }
      { the arg. is still present!                     }
      sysreallocmem(argv[idx],len+1);
    end;

begin
  { create commandline, it starts with the executed filename which is argv[0] }
  { Win32 passes the command NOT via the args, but via getmodulefilename}
  count:=0;
  argv:=nil;
  argvlen:=0;
  pc:=getcommandfile;
  Arglen:=0;
  repeat
    Inc(Arglen);
  until (pc[Arglen]=#0);
  allocarg(count,arglen);
  move(pc^,argv[count]^,arglen+1);
  { Setup cmdline variable }
  cmdline:=GetCommandLine;
  { process arguments }
  pc:=cmdline;
{$IfDef SYSTEM_DEBUG_STARTUP}
  Writeln(stderr,'Win32 GetCommandLine is #',pc,'#');
{$EndIf }
  while pc^<>#0 do
   begin
     { skip leading spaces }
     while pc^ in [#1..#32] do
      inc(pc);
     if pc^=#0 then
      break;
     { calc argument length }
     quote:=' ';
     argstart:=pc;
     arglen:=0;
     while (pc^<>#0) do
      begin
        case pc^ of
          #1..#32 :
            begin
              if quote<>' ' then
               inc(arglen)
              else
               break;
            end;
          '"' :
            begin
              if quote<>'''' then
               begin
                 if pchar(pc+1)^<>'"' then
                  begin
                    if quote='"' then
                     quote:=' '
                    else
                     quote:='"';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          '''' :
            begin
              if quote<>'"' then
               begin
                 if pchar(pc+1)^<>'''' then
                  begin
                    if quote=''''  then
                     quote:=' '
                    else
                     quote:='''';
                  end
                 else
                  inc(pc);
               end
              else
               inc(arglen);
            end;
          else
            inc(arglen);
        end;
        inc(pc);
      end;
     { copy argument }
     { Don't copy the first one, it is already there.}
     If Count<>0 then
      begin
        allocarg(count,arglen);
        quote:=' ';
        pc:=argstart;
        arg:=argv[count];
        while (pc^<>#0) do
         begin
           case pc^ of
             #1..#32 :
               begin
                 if quote<>' ' then
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end
                 else
                  break;
               end;
             '"' :
               begin
                 if quote<>'''' then
                  begin
                    if pchar(pc+1)^<>'"' then
                     begin
                       if quote='"' then
                        quote:=' '
                       else
                        quote:='"';
                     end
                    else
                     inc(pc);
                  end
                 else
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end;
               end;
             '''' :
               begin
                 if quote<>'"' then
                  begin
                    if pchar(pc+1)^<>'''' then
                     begin
                       if quote=''''  then
                        quote:=' '
                       else
                        quote:='''';
                     end
                    else
                     inc(pc);
                  end
                 else
                  begin
                    arg^:=pc^;
                    inc(arg);
                  end;
               end;
             else
               begin
                 arg^:=pc^;
                 inc(arg);
               end;
           end;
           inc(pc);
         end;
        arg^:=#0;
      end;
 {$IfDef SYSTEM_DEBUG_STARTUP}
     Writeln(stderr,'dos arg ',count,' #',arglen,'#',argv[count],'#');
 {$EndIf SYSTEM_DEBUG_STARTUP}
     inc(count);
   end;
  { get argc and create an nil entry }
  argc:=count;
  allocarg(argc,0);
  { free unused memory }
  sysreallocmem(argv,(argc+1)*sizeof(pointer));
end;


function paramcount : longint;
begin
  paramcount := argc - 1;
end;

function paramstr(l : longint) : string;
begin
  if (l>=0) and (l<argc) then
    paramstr:=strpas(argv[l])
  else
    paramstr:='';
end;


procedure randomize;
begin
  randseed:=GetTickCount;
end;


{*****************************************************************************
                         System Dependent Exit code
*****************************************************************************}

procedure install_exception_handlers;forward;
procedure remove_exception_handlers;forward;
procedure PascalMain;stdcall;external name 'PASCALMAIN';
procedure fpc_do_exit;stdcall;external name 'FPC_DO_EXIT';
Procedure ExitDLL(Exitcode : longint); forward;
procedure asm_exit(Exitcode : longint);external name 'asm_exit';

Procedure system_exit;
begin
  { don't call ExitProcess inside
    the DLL exit code !!
    This crashes Win95 at least PM }
  if IsLibrary then
    ExitDLL(ExitCode);
  if not IsConsole then
   begin
     Close(stderr);
     Close(stdout);
     { what about Input and Output ?? PM }
   end;
  remove_exception_handlers;

  { call exitprocess, with cleanup as required }
  asm_exit(exitcode);
end;

var
  { value of the stack segment
    to check if the call stack can be written on exceptions }
  _SS : Cardinal;

procedure Exe_entry;[public, alias : '_FPC_EXE_Entry'];
  begin
     IsLibrary:=false;
     { install the handlers for exe only ?
       or should we install them for DLL also ? (PM) }
     install_exception_handlers;
     { This strange construction is needed to solve the _SS problem
       with a smartlinked syswin32 (PFV) }
     PASCALMAIN;
     { if we pass here there was no error ! }
     system_exit;
  end;

Const
  { DllEntryPoint  }
     DLL_PROCESS_ATTACH = 1;
     DLL_THREAD_ATTACH = 2;
     DLL_PROCESS_DETACH = 0;
     DLL_THREAD_DETACH = 3;
Var
     DLLBuf : Jmp_buf;
Const
     DLLExitOK : boolean = true;

function Dll_entry : longbool;[public, alias : '_FPC_DLL_Entry'];
var
  res : longbool;

  begin
     IsLibrary:=true;
     Dll_entry:=false;
     case DLLreason of
       DLL_PROCESS_ATTACH :
         begin
           If SetJmp(DLLBuf) = 0 then
             begin
               if assigned(Dll_Process_Attach_Hook) then
                 begin
                   res:=Dll_Process_Attach_Hook(DllParam);
                   if not res then
                     exit(false);
                 end;
               PASCALMAIN;
               Dll_entry:=true;
             end
           else
             Dll_entry:=DLLExitOK;
         end;
       DLL_THREAD_ATTACH :
         begin
           inc(Thread_count);
{$warning Allocate Threadvars !}
           if assigned(Dll_Thread_Attach_Hook) then
             Dll_Thread_Attach_Hook(DllParam);
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_THREAD_DETACH :
         begin
           dec(Thread_count);
           if assigned(Dll_Thread_Detach_Hook) then
             Dll_Thread_Detach_Hook(DllParam);
{$warning Release Threadvars !}
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_PROCESS_DETACH :
         begin
           Dll_entry:=true; { return value is ignored }
           If SetJmp(DLLBuf) = 0 then
             begin
               FPC_DO_EXIT;
             end;
           if assigned(Dll_Process_Detach_Hook) then
             Dll_Process_Detach_Hook(DllParam);
         end;
     end;
  end;

Procedure ExitDLL(Exitcode : longint);
begin
    DLLExitOK:=ExitCode=0;
    LongJmp(DLLBuf,1);
end;

{$ifdef Set_i386_Exception_handler}

function GetCurrentProcess : dword;
 stdcall;external 'coredll' name 'GetCurrentProcess';

function ReadProcessMemory(process : dword;address : pointer;dest : pointer;size : dword;bytesread : pdword) :  longbool;
 stdcall;external 'coredll' name 'ReadProcessMemory';

function is_prefetch(p : pointer) : boolean;
  var
    a : array[0..15] of byte;
    doagain : boolean;
    instrlo,instrhi,opcode : byte;
    i : longint;
  begin
    result:=false;
    { read memory savely without causing another exeception }
    if not(ReadProcessMemory(GetCurrentProcess,p,@a,sizeof(a),nil)) then
      exit;
    i:=0;
    doagain:=true;
    while doagain and (i<15) do
      begin
        opcode:=a[i];
        instrlo:=opcode and $f;
        instrhi:=opcode and $f0;
        case instrhi of
          { prefix? }
          $20,$30:
            doagain:=(instrlo and 7)=6;
          $60:
            doagain:=(instrlo and $c)=4;
          $f0:
            doagain:=instrlo in [0,2,3];
          $0:
            begin
              result:=(instrlo=$f) and (a[i+1] in [$d,$18]);
              exit;
            end;
          else
            doagain:=false;
        end;
        inc(i);
      end;
  end;


//
// Hardware exception handling
//

{
  Error code definitions for the Win32 API functions


  Values are 32 bit values layed out as follows:
   3 3 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1
   1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0 9 8 7 6 5 4 3 2 1 0
  +---+-+-+-----------------------+-------------------------------+
  |Sev|C|R|     Facility          |               Code            |
  +---+-+-+-----------------------+-------------------------------+

  where
      Sev - is the severity code
          00 - Success
          01 - Informational
          10 - Warning
          11 - Error

      C - is the Customer code flag
      R - is a reserved bit
      Facility - is the facility code
      Code - is the facility's status code
}

const
  SEVERITY_SUCCESS                = $00000000;
  SEVERITY_INFORMATIONAL  = $40000000;
  SEVERITY_WARNING                = $80000000;
  SEVERITY_ERROR                  = $C0000000;

const
  STATUS_SEGMENT_NOTIFICATION             = $40000005;
  DBG_TERMINATE_THREAD                    = $40010003;
  DBG_TERMINATE_PROCESS                   = $40010004;
  DBG_CONTROL_C                                   = $40010005;
  DBG_CONTROL_BREAK                               = $40010008;

  STATUS_GUARD_PAGE_VIOLATION             = $80000001;
  STATUS_DATATYPE_MISALIGNMENT    = $80000002;
  STATUS_BREAKPOINT                               = $80000003;
  STATUS_SINGLE_STEP                              = $80000004;
  DBG_EXCEPTION_NOT_HANDLED               = $80010001;

  STATUS_ACCESS_VIOLATION                 = $C0000005;
  STATUS_IN_PAGE_ERROR                    = $C0000006;
  STATUS_INVALID_HANDLE                   = $C0000008;
  STATUS_NO_MEMORY                                = $C0000017;
  STATUS_ILLEGAL_INSTRUCTION              = $C000001D;
  STATUS_NONCONTINUABLE_EXCEPTION = $C0000025;
  STATUS_INVALID_DISPOSITION              = $C0000026;
  STATUS_ARRAY_BOUNDS_EXCEEDED    = $C000008C;
  STATUS_FLOAT_DENORMAL_OPERAND   = $C000008D;
  STATUS_FLOAT_DIVIDE_BY_ZERO             = $C000008E;
  STATUS_FLOAT_INEXACT_RESULT             = $C000008F;
  STATUS_FLOAT_INVALID_OPERATION  = $C0000090;
  STATUS_FLOAT_OVERFLOW                   = $C0000091;
  STATUS_FLOAT_STACK_CHECK                = $C0000092;
  STATUS_FLOAT_UNDERFLOW                  = $C0000093;
  STATUS_INTEGER_DIVIDE_BY_ZERO   = $C0000094;
  STATUS_INTEGER_OVERFLOW                 = $C0000095;
  STATUS_PRIVILEGED_INSTRUCTION   = $C0000096;
  STATUS_STACK_OVERFLOW                   = $C00000FD;
  STATUS_CONTROL_C_EXIT                   = $C000013A;
  STATUS_FLOAT_MULTIPLE_FAULTS    = $C00002B4;
  STATUS_FLOAT_MULTIPLE_TRAPS             = $C00002B5;
  STATUS_REG_NAT_CONSUMPTION              = $C00002C9;

  EXCEPTION_EXECUTE_HANDLER               = 1;
  EXCEPTION_CONTINUE_EXECUTION    = -1;
  EXCEPTION_CONTINUE_SEARCH               = 0;

  EXCEPTION_MAXIMUM_PARAMETERS    = 15;

  CONTEXT_X86                                     = $00010000;
  CONTEXT_CONTROL                         = CONTEXT_X86 or $00000001;
  CONTEXT_INTEGER                         = CONTEXT_X86 or $00000002;
  CONTEXT_SEGMENTS                        = CONTEXT_X86 or $00000004;
  CONTEXT_FLOATING_POINT          = CONTEXT_X86 or $00000008;
  CONTEXT_DEBUG_REGISTERS         = CONTEXT_X86 or $00000010;
  CONTEXT_EXTENDED_REGISTERS      = CONTEXT_X86 or $00000020;

  CONTEXT_FULL                            = CONTEXT_CONTROL or CONTEXT_INTEGER or CONTEXT_SEGMENTS;

  MAXIMUM_SUPPORTED_EXTENSION     = 512;

type
  PFloatingSaveArea = ^TFloatingSaveArea;
  TFloatingSaveArea = packed record
          ControlWord : Cardinal;
          StatusWord : Cardinal;
          TagWord : Cardinal;
          ErrorOffset : Cardinal;
          ErrorSelector : Cardinal;
          DataOffset : Cardinal;
          DataSelector : Cardinal;
          RegisterArea : array[0..79] of Byte;
          Cr0NpxState : Cardinal;
  end;

  PContext = ^TContext;
  TContext = packed record
      //
      // The flags values within this flag control the contents of
      // a CONTEXT record.
      //
          ContextFlags : Cardinal;

      //
      // This section is specified/returned if CONTEXT_DEBUG_REGISTERS is
      // set in ContextFlags.  Note that CONTEXT_DEBUG_REGISTERS is NOT
      // included in CONTEXT_FULL.
      //
          Dr0, Dr1, Dr2,
          Dr3, Dr6, Dr7 : Cardinal;

      //
      // This section is specified/returned if the
      // ContextFlags word contains the flag CONTEXT_FLOATING_POINT.
      //
          FloatSave : TFloatingSaveArea;

      //
      // This section is specified/returned if the
      // ContextFlags word contains the flag CONTEXT_SEGMENTS.
      //
          SegGs, SegFs,
          SegEs, SegDs : Cardinal;

      //
      // This section is specified/returned if the
      // ContextFlags word contains the flag CONTEXT_INTEGER.
      //
          Edi, Esi, Ebx,
          Edx, Ecx, Eax : Cardinal;

      //
      // This section is specified/returned if the
      // ContextFlags word contains the flag CONTEXT_CONTROL.
      //
          Ebp : Cardinal;
          Eip : Cardinal;
          SegCs : Cardinal;
          EFlags, Esp, SegSs : Cardinal;

      //
      // This section is specified/returned if the ContextFlags word
      // contains the flag CONTEXT_EXTENDED_REGISTERS.
      // The format and contexts are processor specific
      //
          ExtendedRegisters : array[0..MAXIMUM_SUPPORTED_EXTENSION-1] of Byte;
  end;

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = packed record
          ExceptionCode   : Longint;
          ExceptionFlags  : Longint;
          ExceptionRecord : PExceptionRecord;
          ExceptionAddress : Pointer;
          NumberParameters : Longint;
          ExceptionInformation : array[0..EXCEPTION_MAXIMUM_PARAMETERS-1] of Pointer;
  end;

  PExceptionPointers = ^TExceptionPointers;
  TExceptionPointers = packed record
          ExceptionRecord   : PExceptionRecord;
          ContextRecord     : PContext;
  end;

{ type of functions that should be used for exception handling }
  TTopLevelExceptionFilter = function (excep : PExceptionPointers) : Longint;stdcall;

function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter : TTopLevelExceptionFilter) : TTopLevelExceptionFilter;
        stdcall;external 'kernel32' name 'SetUnhandledExceptionFilter';

const
  MaxExceptionLevel = 16;
  exceptLevel : Byte = 0;

var
  exceptEip       : array[0..MaxExceptionLevel-1] of Longint;
  exceptError     : array[0..MaxExceptionLevel-1] of Byte;
  resetFPU        : array[0..MaxExceptionLevel-1] of Boolean;

{$ifdef SYSTEMEXCEPTIONDEBUG}
procedure DebugHandleErrorAddrFrame(error, addr, frame : longint);
begin
  if IsConsole then
    begin
      write(stderr,'HandleErrorAddrFrame(error=',error);
      write(stderr,',addr=',hexstr(addr,8));
      writeln(stderr,',frame=',hexstr(frame,8),')');
    end;
  HandleErrorAddrFrame(error,addr,frame);
end;
{$endif SYSTEMEXCEPTIONDEBUG}

procedure JumpToHandleErrorFrame;
var
        eip, ebp, error : Longint;
begin
        // save ebp
        asm
                movl (%ebp),%eax
                movl %eax,ebp
        end;
        if (exceptLevel > 0) then
                dec(exceptLevel);

        eip:=exceptEip[exceptLevel];
        error:=exceptError[exceptLevel];
{$ifdef SYSTEMEXCEPTIONDEBUG}
        if IsConsole then
          writeln(stderr,'In JumpToHandleErrorFrame error=',error);
{$endif SYSTEMEXCEPTIONDEBUG}
        if resetFPU[exceptLevel] then asm
                fninit
                fldcw   fpucw
        end;
        { build a fake stack }
        asm
{$ifdef REGCALL}
                movl   ebp,%ecx
                movl   eip,%edx
                movl   error,%eax
                pushl  eip
                movl   ebp,%ebp // Change frame pointer
{$else}
                movl   ebp,%eax
                pushl  %eax
                movl   eip,%eax
                pushl  %eax
                movl   error,%eax
                pushl  %eax
                movl   eip,%eax
                pushl  %eax
                movl   ebp,%ebp // Change frame pointer
{$endif}

{$ifdef SYSTEMEXCEPTIONDEBUG}
                jmpl   DebugHandleErrorAddrFrame
{$else not SYSTEMEXCEPTIONDEBUG}
                jmpl   HandleErrorAddrFrame
{$endif SYSTEMEXCEPTIONDEBUG}
        end;
end;

function syswin32_i386_exception_handler(excep : PExceptionPointers) : Longint;stdcall;
var
        frame,
        res  : longint;

function SysHandleErrorFrame(error, frame : Longint; must_reset_fpu : Boolean) : Longint;
begin
        if (frame = 0) then
                SysHandleErrorFrame:=EXCEPTION_CONTINUE_SEARCH
        else begin
                if (exceptLevel >= MaxExceptionLevel) then exit;

                exceptEip[exceptLevel] := excep^.ContextRecord^.Eip;
                exceptError[exceptLevel] := error;
                resetFPU[exceptLevel] := must_reset_fpu;
                inc(exceptLevel);

                excep^.ContextRecord^.Eip := Longint(@JumpToHandleErrorFrame);
                excep^.ExceptionRecord^.ExceptionCode := 0;

                SysHandleErrorFrame := EXCEPTION_CONTINUE_EXECUTION;
{$ifdef SYSTEMEXCEPTIONDEBUG}
                if IsConsole then begin
                        writeln(stderr,'Exception Continue Exception set at ',
                                hexstr(exceptEip[exceptLevel],8));
                        writeln(stderr,'Eip changed to ',
                                hexstr(longint(@JumpToHandleErrorFrame),8), ' error=', error);
                end;
{$endif SYSTEMEXCEPTIONDEBUG}
        end;
end;

begin
        if excep^.ContextRecord^.SegSs=_SS then
                frame := excep^.ContextRecord^.Ebp
        else
                frame := 0;
        res := EXCEPTION_CONTINUE_SEARCH;
{$ifdef SYSTEMEXCEPTIONDEBUG}
        if IsConsole then Writeln(stderr,'Exception  ',
                hexstr(excep^.ExceptionRecord^.ExceptionCode, 8));
{$endif SYSTEMEXCEPTIONDEBUG}
        case cardinal(excep^.ExceptionRecord^.ExceptionCode) of
                STATUS_INTEGER_DIVIDE_BY_ZERO,
                STATUS_FLOAT_DIVIDE_BY_ZERO :
                        res := SysHandleErrorFrame(200, frame, true);
                STATUS_ARRAY_BOUNDS_EXCEEDED :
                        res := SysHandleErrorFrame(201, frame, false);
                STATUS_STACK_OVERFLOW :
                        res := SysHandleErrorFrame(202, frame, false);
                STATUS_FLOAT_OVERFLOW :
                        res := SysHandleErrorFrame(205, frame, true);
                STATUS_FLOAT_DENORMAL_OPERAND,
                STATUS_FLOAT_UNDERFLOW :
                        res := SysHandleErrorFrame(206, frame, true);
{excep^.ContextRecord^.FloatSave.StatusWord := excep^.ContextRecord^.FloatSave.StatusWord and $ffffff00;}
                STATUS_FLOAT_INEXACT_RESULT,
                STATUS_FLOAT_INVALID_OPERATION,
                STATUS_FLOAT_STACK_CHECK :
                        res := SysHandleErrorFrame(207, frame, true);
                STATUS_INTEGER_OVERFLOW :
                        res := SysHandleErrorFrame(215, frame, false);
                STATUS_ILLEGAL_INSTRUCTION:
                  res := SysHandleErrorFrame(216, frame, true);
                STATUS_ACCESS_VIOLATION:
                  { Athlon prefetch bug? }
                  if is_prefetch(pointer(excep^.ContextRecord^.Eip)) then
                    begin
                      { if yes, then retry }
                      excep^.ExceptionRecord^.ExceptionCode := 0;
                      res:=EXCEPTION_CONTINUE_EXECUTION;
                    end
                  else
                    res := SysHandleErrorFrame(216, frame, true);

                STATUS_CONTROL_C_EXIT:
                        res := SysHandleErrorFrame(217, frame, true);
                STATUS_PRIVILEGED_INSTRUCTION:
                  res := SysHandleErrorFrame(218, frame, false);
                else
                  begin
                    if ((excep^.ExceptionRecord^.ExceptionCode and SEVERITY_ERROR) = SEVERITY_ERROR) then
                      res  :=  SysHandleErrorFrame(217, frame, true)
                    else
                      res := SysHandleErrorFrame(255, frame, true);
                  end;
        end;
        syswin32_i386_exception_handler := res;
end;


procedure install_exception_handlers;
{$ifdef SYSTEMEXCEPTIONDEBUG}
var
        oldexceptaddr,
        newexceptaddr : Longint;
{$endif SYSTEMEXCEPTIONDEBUG}

begin
{$ifdef SYSTEMEXCEPTIONDEBUG}
        asm
                movl $0,%eax
                movl %fs:(%eax),%eax
                movl %eax,oldexceptaddr
        end;
{$endif SYSTEMEXCEPTIONDEBUG}
        SetUnhandledExceptionFilter(@syswin32_i386_exception_handler);
{$ifdef SYSTEMEXCEPTIONDEBUG}
        asm
                movl $0,%eax
                movl %fs:(%eax),%eax
                movl %eax,newexceptaddr
        end;
        if IsConsole then
                writeln(stderr,'Old exception  ',hexstr(oldexceptaddr,8),
                        ' new exception  ',hexstr(newexceptaddr,8));
{$endif SYSTEMEXCEPTIONDEBUG}
end;

procedure remove_exception_handlers;
begin
        SetUnhandledExceptionFilter(nil);
end;

{$else not cpui386 (Processor specific !!)}
procedure install_exception_handlers;
begin
end;

procedure remove_exception_handlers;
begin
end;

{$endif Set_i386_Exception_handler}


{****************************************************************************
                      OS dependend widestrings
****************************************************************************}

function CharUpperBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD; stdcall; external KernelDLL name 'CharUpperBuffW';
function CharLowerBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD; stdcall; external KernelDLL name 'CharLowerBuffW';


function Win32WideUpper(const s : WideString) : WideString;
  begin
    result:=s;
    UniqueString(result);
    if length(result)>0 then
      CharUpperBuff(LPWSTR(result),length(result));
  end;


function Win32WideLower(const s : WideString) : WideString;
  begin
    result:=s;
    UniqueString(result);
    if length(result)>0 then
      CharLowerBuff(LPWSTR(result),length(result));
  end;


{ there is a similiar procedure in sysutils which inits the fields which
  are only relevant for the sysutils units }
procedure InitWin32Widestrings;
  begin
    widestringmanager.UpperWideStringProc:=@Win32WideUpper;
    widestringmanager.LowerWideStringProc:=@Win32WideLower;
  end;



{****************************************************************************
                    Error Message writing using messageboxes
****************************************************************************}

function MessageBox(w1:longint;l1,l2:pointer;w2:longint):longint;
   stdcall;external 'coredll' name 'MessageBoxW';

const
  ErrorBufferLength = 1024;
var
  ErrorBuf : array[0..ErrorBufferLength] of char;
  ErrorLen : longint;

Function ErrorWrite(Var F: TextRec): Integer;
{
  An error message should always end with #13#10#13#10
}
var
  p : pchar;
  i : longint;
Begin
  if F.BufPos>0 then
   begin
     if F.BufPos+ErrorLen>ErrorBufferLength then
       i:=ErrorBufferLength-ErrorLen
     else
       i:=F.BufPos;
     Move(F.BufPtr^,ErrorBuf[ErrorLen],i);
     inc(ErrorLen,i);
     ErrorBuf[ErrorLen]:=#0;
   end;
  if ErrorLen>3 then
   begin
     p:=@ErrorBuf[ErrorLen];
     for i:=1 to 4 do
      begin
        dec(p);
        if not(p^ in [#10,#13]) then
         break;
      end;
   end;
   if ErrorLen=ErrorBufferLength then
     i:=4;
   if (i=4) then
    begin
      MessageBox(0,@ErrorBuf,pchar('Error'),0);
      ErrorLen:=0;
    end;
  F.BufPos:=0;
  ErrorWrite:=0;
End;


Function ErrorClose(Var F: TextRec): Integer;
begin
  if ErrorLen>0 then
   begin
     MessageBox(0,@ErrorBuf,pchar('Error'),0);
     ErrorLen:=0;
   end;
  ErrorLen:=0;
  ErrorClose:=0;
end;


Function ErrorOpen(Var F: TextRec): Integer;
Begin
  TextRec(F).InOutFunc:=@ErrorWrite;
  TextRec(F).FlushFunc:=@ErrorWrite;
  TextRec(F).CloseFunc:=@ErrorClose;
  ErrorOpen:=0;
End;


procedure AssignError(Var T: Text);
begin
  Assign(T,'');
  TextRec(T).OpenFunc:=@ErrorOpen;
  Rewrite(T);
end;


procedure SysInitStdIO;
begin
  { Setup stdin, stdout and stderr, for GUI apps redirect stderr,stdout to be
    displayed in and messagebox }
  AssignError(stderr);
  AssignError(stdout);
  Assign(Output,'');
  Assign(Input,'');
  Assign(ErrOutput,'');
end;

(* ProcessID cached to avoid repeated calls to GetCurrentProcess. *)

var
  ProcessID: SizeUInt;

function GetProcessID: SizeUInt;
begin
 GetProcessID := ProcessID;
end;


const
   Exe_entry_code : pointer = @Exe_entry;
   Dll_entry_code : pointer = @Dll_entry;

begin
  StackLength := InitialStkLen;
  StackBottom := Sptr - StackLength;
  { some misc Win32 stuff }
  hprevinst:=0;
  if not IsLibrary then
    HInstance:=getmodulehandle(GetCommandFile);
  MainInstance:=HInstance;
  { Setup heap }
  InitHeap;
  SysInitExceptions;
  SysInitStdIO;
  { Arguments }
  setup_arguments;
  { Reset IO Error }
  InOutRes:=0;
  ProcessID := GetCurrentProcessID;
  { threading }
  InitSystemThreads;
  { Reset internal error variable }
  errno:=0;
  initvariantmanager;
  initwidestringmanager;
  InitWin32Widestrings
end.
