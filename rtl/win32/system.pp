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
{$define HAS_WIDESTRINGMANAGER}

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
{ C compatible arguments }
  argc : longint; public name 'operatingsystem_parameter_argc';
  argv : ppchar; public name 'operatingsystem_parameter_argv';
{ Win32 Info }
  startupinfo : tstartupinfo;
  MainInstance,
  cmdshow     : longint;
  DLLreason : dword; public name 'operatingsystem_dllreason';
  DLLparam : PtrInt; public name 'operatingsystem_dllparam';
  StartupConsoleMode : DWORD;
const
  hprevinst: longint=0;

type
  TDLL_Entry_Hook = procedure (dllparam : PtrInt);

const
  Dll_Process_Detach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Attach_Hook : TDLL_Entry_Hook = nil;
  Dll_Thread_Detach_Hook : TDLL_Entry_Hook = nil;

Const
  { it can be discussed whether fmShareDenyNone means read and write or read, write and delete, see
    also http://bugs.freepascal.org/view.php?id=8898, this allows users to configure the used
	value
  }
  fmShareDenyNoneFlags : DWord = 3;

implementation

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
                         System Dependent Exit code
*****************************************************************************}

procedure install_exception_handlers;forward;
procedure remove_exception_handlers;forward;
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

  { do cleanup required by the startup code }
  EntryInformation.asm_exit();

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
     EntryInformation.PascalMain();
     asm
        popl %ebp
     end;
     { if we pass here there was no error ! }
     system_exit;
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

{******************************************************************************}
{ include code common with win64 }

{$I syswin.inc}
{******************************************************************************}

//
// Hardware exception handling
//

{$ifdef Set_i386_Exception_handler}

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
procedure DebugHandleErrorAddrFrame(error : longint; addr, frame : pointer);
begin
  if IsConsole then
    begin
      write(stderr,'HandleErrorAddrFrame(error=',error);
      write(stderr,',addr=',hexstr(ptruint(addr),8));
      writeln(stderr,',frame=',hexstr(ptruint(frame),8),')');
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
      movl   ebp,%ecx
      movl   eip,%edx
      movl   error,%eax
      pushl  eip
      movl   ebp,%ebp // Change frame pointer

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
                  hexstr(longint(@JumpToHandleErrorFrame),8), ' error=', err);
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


begin
  { get some helpful informations }
  GetStartupInfo(@startupinfo);
  { some misc Win32 stuff }
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
{$ifndef VER2_2}
  initunicodestringmanager;
{$endif VER2_2}
  InitWin32Widestrings;
  DispCallByIDProc:=@DoDispCallByIDError;
end.

