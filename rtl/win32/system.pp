{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2005 by Florian Klaempfl and Pavel Ozerski
    member of the Free Pascal development team.

    FPC Pascal system unit for the Win32 API.

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

{$define FPC_HAS_INDIRECT_MAIN_INFORMATION}

{$ifdef cpui386}
  {$define Set_i386_Exception_handler}
{$endif cpui386}

{$define DISABLE_NO_THREAD_MANAGER}

{$define FPC_RTLSTRING_UTF16}
{$define HAS_CMDLINE}

{ include system-independent routine headers }
{$I systemh.inc}

const
 LineEnding = #13#10;
 LFNSupport = true;
 DirectorySeparator = '\';
 DriveSeparator = ':';
 ExtensionSeparator = '.';
 PathSeparator = ';';
 AllowDirectorySeparators : set of char = ['\','/'];
 AllowDriveSeparators : set of char = [':'];

{ FileNameCaseSensitive is defined separately below!!! }
 maxExitCode = 65535;
 MaxPathLen = 260;
 AllFilesMask = '*';

type
   PEXCEPTION_FRAME = ^TEXCEPTION_FRAME;
   TEXCEPTION_FRAME = record
     next : PEXCEPTION_FRAME;
     handler : pointer;
   end;

const
{ Default filehandles }
  UnusedHandle    : THandle = THandle(-1);
  StdInputHandle  : THandle = 0;
  StdOutputHandle : THandle = 0;
  StdErrorHandle  : THandle = 0;

  FileNameCaseSensitive : boolean = true;
  CtrlZMarksEOF: boolean = true; (* #26 is considered as end of file *)

  sLineBreak = LineEnding;
  DefaultTextLineBreakStyle : TTextLineBreakStyle = tlbsCRLF;

  { Thread count for DLL }
  Thread_count : longint = 0;
  System_exception_frame : PEXCEPTION_FRAME =nil;

type
  TStartupInfo=packed record
    cb : longint;
    lpReserved : Pointer;
    lpDesktop : Pointer;
    lpTitle : Pointer;
    dwX : longint;
    dwY : longint;
    dwXSize : longint;
    dwYSize : longint;
    dwXCountChars : longint;
    dwYCountChars : longint;
    dwFillAttribute : longint;
    dwFlags : longint;
    wShowWindow : Word;
    cbReserved2 : Word;
    lpReserved2 : Pointer;
    hStdInput : longint;
    hStdOutput : longint;
    hStdError : longint;
  end;

var
{ Win32 Info }
  startupinfo : tstartupinfo;
  hprevinst,
  MainInstance,
  cmdshow     : longint;
  DLLreason,DLLparam:longint;
  StartupConsoleMode : DWORD;

type
  TDLL_Process_Entry_Hook = function (dllparam : longint) : longbool;
  TDLL_Entry_Hook = procedure (dllparam : longint);

const
  Dll_Process_Attach_Hook : TDLL_Process_Entry_Hook = nil;
  Dll_Process_Detach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Attach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Detach_Hook : TDLL_Entry_Hook = nil;

function CmdLine: PRtlChar;
{ C compatible arguments ANSI only}
function argc: longint;
function argv: PPAnsiChar;

implementation

function _W(const s: RtlString): PWideChar; inline;
begin
  Result:=PWideChar(UnicodeString(s));
end;

function _W(p: PWideChar): PWideChar; inline;
begin
  Result:=p;
end;

var
  EntryInformation : TEntryInformation;
  SysInstance : Longint;public name '_FPC_SysInstance';

{ used by wstrings.inc because wstrings.inc is included before sysos.inc
  this is put here (FK) }

function SysAllocStringLen(psz:pointer;len:dword):pointer;stdcall;
 external 'oleaut32.dll' name 'SysAllocStringLen';

procedure SysFreeString(bstr:pointer);stdcall;
 external 'oleaut32.dll' name 'SysFreeString';

function SysReAllocStringLen(var bstr:pointer;psz: pointer;
  len:dword): Integer; stdcall;external 'oleaut32.dll' name 'SysReAllocStringLen';


{ include system independent routines }
{$I system.inc}

{*****************************************************************************
                              Parameter Handling
*****************************************************************************}

var
  FArgs: PRtlChar;
  FCmdLine: PRtlChar;
  Fargc: longint;
  Fargv: PPRtlChar;
{$ifdef FPC_RTLSTRING_UTF16}
  FAnsiArgs: PAnsiChar;
  FAnsiArgv: PPAnsiChar;
{$else}
  FCmdLineStr: RtlString;
{$endif FPC_RTLSTRING_UTF16}

procedure setup_arguments;
var
  i, argvlen: longint;
  pc, dst, argstart: PRtlChar;
  quote: Boolean;
  buf: array[0..259] of WideChar;  // need MAX_PATH bytes, not 256!
{$ifndef FPC_RTLSTRING_UTF16}
  s: RtlString;
{$endif FPC_RTLSTRING_UTF16}
begin
  if FCmdLine <> nil then exit;
  // Alloc argv buffer
  argvlen:=20;
  Fargv:=SysGetMem(argvlen*SizeOf(pointer));
  // Get command line
{$ifdef FPC_RTLSTRING_UTF16}
  FCmdLine:=GetCommandLine;
{$else}
  FCmdLineStr:=GetCommandLine;
  FCmdLine:=PRtlChar(FCmdLineStr);
{$endif FPC_RTLSTRING_UTF16}
  // Get exe name
  i:=GetModuleFileName(0, buf, High(buf)-1);
  buf[i]:=#0; // be safe
{$ifndef FPC_RTLSTRING_UTF16}
  s:=buf;
  i:=Length(s);
{$endif FPC_RTLSTRING_UTF16}
  Inc(i);
  // Alloc space for arguments
  FArgs:=SysGetMem((i + strlen(FCmdLine) + 2)*SizeOf(RtlChar));
  // Copy exe name
  Move(buf, FArgs^, i*SizeOf(RtlChar));
  Fargv[0]:=FArgs;
  Fargc:=0;

  // Process arguments
  pc:=FCmdLine;
  dst:=FArgs + i;
  while pc^ <> #0 do
   begin
     { skip leading spaces }
     while (pc^ <> #0) and (pc^ <= ' ') do
       Inc(pc);
     if pc^ = #0 then
       break;

     argstart:=dst;

     { copy argument }
     quote:=False;
     while pc^ <> #0 do
      begin
        case pc^ of
          #1..#32 :
            if not quote then
              break;
          '"' :
            begin
              Inc(pc);
              if pc^ <> '"' then
               begin
                 quote := not quote;
                 continue;
               end;
            end;
        end;
        // don't copy the first argument. It is exe name
        if Fargc > 0 then
          begin
            dst^:=pc^;
            Inc(dst);
          end;
        Inc(pc);
      end;

      if Fargc > 0 then
        begin
          // null-terminate the argument
          dst^:=#0;
          Inc(dst);
          if Fargc >= argvlen then
            begin
              Inc(argvlen, 20);
              SysReAllocMem(Fargv, argvlen*SizeOf(pointer));
            end;
          Fargv[Fargc]:=argstart;
        end;

      Inc(Fargc);
   end;
   // Terminate FArgs with double null
   dst^:=#0;
   Inc(dst);
   // Truncate buffers
   SysReAllocMem(FArgs, pointer(dst) - pointer(FArgs));
   SysReAllocMem(Fargv, Fargc*SizeOf(pointer));
end;

function CmdLine: PRtlChar;
begin
  setup_arguments;
  Result:=FCmdLine;
end;

function argc: longint;
begin
  setup_arguments;
  Result:=Fargc;
end;

function paramcount : longint;
begin
  paramcount := argc - 1;
end;

function paramstr(l : longint) : RtlString;
begin
  setup_arguments;
  if (l>=0) and (l<Fargc) then
    paramstr:=Fargv[l]
  else
    paramstr:='';
end;

function argv: PPAnsiChar;
var
  s: AnsiString;
  i, j, len: cardinal;
begin
  if FAnsiArgv = nil then begin
    setup_arguments;
    FAnsiArgv:=SysGetMem((ParamCount + 1)*SizeOf(pointer));
    len:=0;
    for i:=0 to ParamCount do begin
      s:=ParamStr(i);
      j:=Length(s) + 1;
      SysReAllocMem(FAnsiArgs, len + j);
      Move(s[1], FAnsiArgs[len], j);
      FAnsiArgv[i]:=pointer(len);
      Inc(len, j);
    end;
    for i:=0 to ParamCount do
      FAnsiArgv[i]:=pointer(FAnsiArgs) + ptruint(FAnsiArgv[i]);
  end;
  Result:=FAnsiArgv;
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
{$ifndef FPC_HAS_INDIRECT_MAIN_INFORMATION}
procedure PascalMain;stdcall;external name 'PASCALMAIN';
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
procedure fpc_do_exit;stdcall;external name 'FPC_DO_EXIT';
Procedure ExitDLL(Exitcode : longint); forward;
procedure asm_exit;stdcall;external name 'asm_exit';

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
     Close(erroutput);
     Close(Input);
     Close(Output);
     { what about Input and Output ?? PM }
     { now handled, FPK }
   end;
  remove_exception_handlers;

  { in 2.0 asm_exit does an exitprocess }
{$ifndef ver2_0}
  { do cleanup required by the startup code }
{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}
  EntryInformation.asm_exit();
{$else FPC_HAS_INDIRECT_MAIN_INFORMATION}
  asm_exit;
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
{$endif ver2_0}

  { call exitprocess, with cleanup as required }
  ExitProcess(exitcode);
end;

var
  { value of the stack segment
    to check if the call stack can be written on exceptions }
  _SS : Cardinal;

procedure Exe_entry(const info : TEntryInformation);[public,alias:'_FPC_EXE_Entry'];
  var
    ST : pointer;
  begin
     EntryInformation:=info;
     IsLibrary:=false;
     { install the handlers for exe only ?
       or should we install them for DLL also ? (PM) }
     install_exception_handlers;
     { This strange construction is needed to solve the _SS problem
       with a smartlinked syswin32 (PFV) }
     asm
         { allocate space for an exception frame }
        pushl $0
        pushl %fs:(0)
        { movl  %esp,%fs:(0)
          but don't insert it as it doesn't
          point to anything yet
          this will be used in signals unit }
        movl %esp,%eax
        movl %eax,System_exception_frame
        pushl %ebp
        movl %esp,%eax
        movl %eax,st
     end;
     StackTop:=st;
     asm
        xorl %eax,%eax
        movw %ss,%ax
        movl %eax,_SS
        xorl %ebp,%ebp
     end;
{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}
     EntryInformation.PascalMain();
{$else FPC_HAS_INDIRECT_MAIN_INFORMATION}
     PascalMain;
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
     asm
        popl %ebp
     end;
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

function Dll_entry(const info : TEntryInformation) : longbool; [public,alias:'_FPC_DLL_Entry'];
  var
    res : longbool;
  begin
     EntryInformation:=info;
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
{$ifdef FPC_HAS_INDIRECT_MAIN_INFORMATION}
               EntryInformation.PascalMain();
{$else FPC_HAS_INDIRECT_MAIN_INFORMATION}
               PascalMain;
{$endif FPC_HAS_INDIRECT_MAIN_INFORMATION}
               Dll_entry:=true;
             end
           else
             Dll_entry:=DLLExitOK;
         end;
       DLL_THREAD_ATTACH :
         begin
           inclocked(Thread_count);
{ Allocate Threadvars ?!}
           if assigned(Dll_Thread_Attach_Hook) then
             Dll_Thread_Attach_Hook(DllParam);
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_THREAD_DETACH :
         begin
           declocked(Thread_count);
           if assigned(Dll_Thread_Detach_Hook) then
             Dll_Thread_Detach_Hook(DllParam);
{ Release Threadvars ?!}
           Dll_entry:=true; { return value is ignored }
         end;
       DLL_PROCESS_DETACH :
         begin
           Dll_entry:=true; { return value is ignored }
           If SetJmp(DLLBuf) = 0 then
             FPC_Do_Exit;
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


function GetCurrentProcess : dword;
 stdcall;external 'kernel32' name 'GetCurrentProcess';

function ReadProcessMemory(process : dword;address : pointer;dest : pointer;size : dword;bytesread : pdword) :  longbool;
 stdcall;external 'kernel32' name 'ReadProcessMemory';

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

{$ifdef Set_i386_Exception_handler}

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
          ExceptionCode   : cardinal;
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
    if resetFPU[exceptLevel] then
      SysResetFPU;
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
    res: longint;
    err: byte;
    must_reset_fpu: boolean;
  begin
    res := EXCEPTION_CONTINUE_SEARCH;
    if excep^.ContextRecord^.SegSs=_SS then begin
      err := 0;
      must_reset_fpu := true;
{$ifdef SYSTEMEXCEPTIONDEBUG}
      if IsConsole then Writeln(stderr,'Exception  ',
              hexstr(excep^.ExceptionRecord^.ExceptionCode, 8));
{$endif SYSTEMEXCEPTIONDEBUG}
      case excep^.ExceptionRecord^.ExceptionCode of
        STATUS_INTEGER_DIVIDE_BY_ZERO,
        STATUS_FLOAT_DIVIDE_BY_ZERO :
          err := 200;
        STATUS_ARRAY_BOUNDS_EXCEEDED :
          begin
            err := 201;
            must_reset_fpu := false;
          end;
        STATUS_STACK_OVERFLOW :
          begin
            err := 202;
            must_reset_fpu := false;
          end;
        STATUS_FLOAT_OVERFLOW :
          err := 205;
        STATUS_FLOAT_DENORMAL_OPERAND,
        STATUS_FLOAT_UNDERFLOW :
          err := 206;
    {excep^.ContextRecord^.FloatSave.StatusWord := excep^.ContextRecord^.FloatSave.StatusWord and $ffffff00;}
        STATUS_FLOAT_INEXACT_RESULT,
        STATUS_FLOAT_INVALID_OPERATION,
        STATUS_FLOAT_STACK_CHECK :
          err := 207;
        STATUS_INTEGER_OVERFLOW :
          begin
            err := 215;
            must_reset_fpu := false;
          end;
        STATUS_ILLEGAL_INSTRUCTION:
          { if we're testing sse support, simply set the flag and continue }
          if sse_check then
            begin
              os_supports_sse:=false;
              { skip the offending movaps %xmm7, %xmm6 instruction }
              inc(excep^.ContextRecord^.Eip,3);
              excep^.ExceptionRecord^.ExceptionCode := 0;
              res:=EXCEPTION_CONTINUE_EXECUTION;
            end
          else
            err := 216;
        STATUS_ACCESS_VIOLATION:
          { Athlon prefetch bug? }
          if is_prefetch(pointer(excep^.ContextRecord^.Eip)) then
            begin
              { if yes, then retry }
              excep^.ExceptionRecord^.ExceptionCode := 0;
              res:=EXCEPTION_CONTINUE_EXECUTION;
            end
          else
            err := 216;

        STATUS_CONTROL_C_EXIT:
          err := 217;
        STATUS_PRIVILEGED_INSTRUCTION:
          begin
            err := 218;
            must_reset_fpu := false;
          end;
        else
          begin
            if ((excep^.ExceptionRecord^.ExceptionCode and SEVERITY_ERROR) = SEVERITY_ERROR) then
              err := 217
            else
              err := 255;
          end;
      end;

      if (err <> 0) and (exceptLevel < MaxExceptionLevel) then begin
        exceptEip[exceptLevel] := excep^.ContextRecord^.Eip;
        exceptError[exceptLevel] := err;
        resetFPU[exceptLevel] := must_reset_fpu;
        inc(exceptLevel);

        excep^.ContextRecord^.Eip := Longint(@JumpToHandleErrorFrame);
        excep^.ExceptionRecord^.ExceptionCode := 0;

        res := EXCEPTION_CONTINUE_EXECUTION;
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

const
  { MultiByteToWideChar  }
     MB_PRECOMPOSED = 1;
     CP_ACP = 0;
     WC_NO_BEST_FIT_CHARS = $400;

function MultiByteToWideChar(CodePage:UINT; dwFlags:DWORD; lpMultiByteStr:PChar; cchMultiByte:longint; lpWideCharStr:PWideChar;cchWideChar:longint):longint;
    stdcall; external 'kernel32' name 'MultiByteToWideChar';
function WideCharToMultiByte(CodePage:UINT; dwFlags:DWORD; lpWideCharStr:PWideChar; cchWideChar:longint; lpMultiByteStr:PChar;cchMultiByte:longint; lpDefaultChar:PChar; lpUsedDefaultChar:pointer):longint;
    stdcall; external 'kernel32' name 'WideCharToMultiByte';
function CharUpperBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD;
    stdcall; external 'user32' name 'CharUpperBuffW';
function CharLowerBuff(lpsz:LPWSTR; cchLength:DWORD):DWORD;
    stdcall; external 'user32' name 'CharLowerBuffW';

{******************************************************************************
                              Widestring
 ******************************************************************************}

procedure Win32Wide2AnsiMove(source:pwidechar;var dest:ansistring;len:SizeInt);
  var
    destlen: SizeInt;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    destlen:=WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS, source, len, nil, 0, nil, nil);
    // this will null-terminate
    setlength(dest, destlen);
    WideCharToMultiByte(CP_ACP, WC_NO_BEST_FIT_CHARS, source, len, @dest[1], destlen, nil, nil);
  end;

procedure Win32Ansi2WideMove(source:pchar;var dest:widestring;len:SizeInt);
  var
    destlen: SizeInt;
  begin
    // retrieve length including trailing #0
    // not anymore, because this must also be usable for single characters
    destlen:=MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, source, len, nil, 0);
    // this will null-terminate
    setlength(dest, destlen);
    MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, source, len, @dest[1], destlen);
  end;


function Win32WideUpper(const s : WideString) : WideString;
  begin
    result:=s;
    if length(result)>0 then
      CharUpperBuff(LPWSTR(result),length(result));
  end;


function Win32WideLower(const s : WideString) : WideString;
  begin
    result:=s;
    if length(result)>0 then
      CharLowerBuff(LPWSTR(result),length(result));
  end;

{******************************************************************************}
{ include code common with win64 }

{$I syswin.inc}
{******************************************************************************}


function CheckInitialStkLen(stklen : SizeUInt) : SizeUInt;
	type
	  tdosheader = packed record
	     e_magic : word;
	     e_cblp : word;
	     e_cp : word;
	     e_crlc : word;
	     e_cparhdr : word;
	     e_minalloc : word;
	     e_maxalloc : word;
	     e_ss : word;
	     e_sp : word;
	     e_csum : word;
	     e_ip : word;
	     e_cs : word;
	     e_lfarlc : word;
	     e_ovno : word;
	     e_res : array[0..3] of word;
	     e_oemid : word;
	     e_oeminfo : word;
	     e_res2 : array[0..9] of word;
	     e_lfanew : longint;
	  end;
	  tpeheader = packed record
	     PEMagic : longint;
	     Machine : word;
	     NumberOfSections : word;
	     TimeDateStamp : longint;
	     PointerToSymbolTable : longint;
	     NumberOfSymbols : longint;
	     SizeOfOptionalHeader : word;
	     Characteristics : word;
	     Magic : word;
	     MajorLinkerVersion : byte;
	     MinorLinkerVersion : byte;
	     SizeOfCode : longint;
	     SizeOfInitializedData : longint;
	     SizeOfUninitializedData : longint;
	     AddressOfEntryPoint : longint;
	     BaseOfCode : longint;
	     BaseOfData : longint;
	     ImageBase : longint;
	     SectionAlignment : longint;
	     FileAlignment : longint;
	     MajorOperatingSystemVersion : word;
	     MinorOperatingSystemVersion : word;
	     MajorImageVersion : word;
	     MinorImageVersion : word;
	     MajorSubsystemVersion : word;
	     MinorSubsystemVersion : word;
	     Reserved1 : longint;
	     SizeOfImage : longint;
	     SizeOfHeaders : longint;
	     CheckSum : longint;
	     Subsystem : word;
	     DllCharacteristics : word;
	     SizeOfStackReserve : longint;
	     SizeOfStackCommit : longint;
	     SizeOfHeapReserve : longint;
	     SizeOfHeapCommit : longint;
	     LoaderFlags : longint;
	     NumberOfRvaAndSizes : longint;
	     DataDirectory : array[1..$80] of byte;
	  end;
	begin
	  result:=tpeheader((pointer(SysInstance)+(tdosheader(pointer(SysInstance)^).e_lfanew))^).SizeOfStackReserve;
	end;


initialization
  { get some helpful informations }
  GetStartupInfo(@startupinfo);

  SysResetFPU;
  if not(IsLibrary) then
    SysInitFPU;

  { some misc Win32 stuff }
  hprevinst:=0;
  if not IsLibrary then
    SysInstance:=getmodulehandle(nil);

  MainInstance:=SysInstance;

  { pass dummy value }
  StackLength := CheckInitialStkLen($1000000);
  StackBottom := StackTop - StackLength;

  cmdshow:=startupinfo.wshowwindow;
  { Setup heap }
  InitHeap;
  SysInitExceptions;
  { setup fastmove stuff }
  fpc_cpucodeinit;
  SetupProcVars;
  { Reset IO Error }
  InOutRes:=0;
  ProcessID := GetCurrentProcessID;
  { threading }
  InitSystemThreads;
  { Reset internal error variable }
  errno:=0;
  initvariantmanager;
  initwidestringmanager;
{$ifndef VER2_2}
  initunicodestringmanager;
{$endif VER2_2}
  InitWin32Widestrings;
  DispCallByIDProc:=@DoDispCallByIDError;
  SysInitStdIO;

finalization
  SysFreeMem(FArgs);
  SysFreeMem(FArgv);
{$ifdef FPC_RTLSTRING_UTF16}
  SysFreeMem(FAnsiArgs);
  SysFreeMem(FAnsiArgv);
{$endif FPC_RTLSTRING_UTF16}

end.

