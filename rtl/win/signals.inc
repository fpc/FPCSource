{
  Signals unit implementation for win32 and win64: a vectored exception handler, which runs
  before the SEH frame handlers, maps hardware exceptions to signals. Part of the Free Pascal
  run time library; see the file COPYING.FPC, included in this distribution, for the copyright.
}

const
  EXCEPTION_ACCESS_VIOLATION = $c0000005;
  EXCEPTION_SINGLE_STEP = $80000004;
  EXCEPTION_ARRAY_BOUNDS_EXCEEDED = $c000008c;
  EXCEPTION_FLT_DENORMAL_OPERAND = $c000008d;
  EXCEPTION_FLT_DIVIDE_BY_ZERO = $c000008e;
  EXCEPTION_FLT_INVALID_OPERATION = $c0000090;
  EXCEPTION_FLT_OVERFLOW = $c0000091;
  EXCEPTION_FLT_STACK_CHECK = $c0000092;
  EXCEPTION_FLT_UNDERFLOW = $c0000093;
  EXCEPTION_INT_DIVIDE_BY_ZERO = $c0000094;
  EXCEPTION_INT_OVERFLOW = $c0000095;
  EXCEPTION_PRIV_INSTRUCTION = $c0000096;
  EXCEPTION_STACK_OVERFLOW = $c00000fd;
  EXCEPTION_ILLEGAL_INSTRUCTION = $C000001D;
  EXCEPTION_IN_PAGE_ERROR = $C0000006;
  STATUS_FLOAT_MULTIPLE_FAULTS = $C00002B4;
  STATUS_FLOAT_MULTIPLE_TRAPS = $C00002B5;

  EXCEPTION_CONTINUE_EXECUTION = -1;
  EXCEPTION_CONTINUE_SEARCH = 0;

  { ContextFlags bit for the extended processor state stored after the CONTEXT record }
  CONTEXT_XSTATE_BIT = $40;

type
  PVECTORED_EXCEPTION_HANDLER = function(aExceptionInfo : PEXCEPTION_POINTERS) : longint; stdcall;

function AddVectoredExceptionHandler(aFirst : DWord; aHandler : PVECTORED_EXCEPTION_HANDLER) : pointer;
  stdcall; external 'kernel32' name 'AddVectoredExceptionHandler';
function RemoveVectoredExceptionHandler(aHandle : pointer) : DWord;
  stdcall; external 'kernel32' name 'RemoveVectoredExceptionHandler';
function NtContinue(aContext : PCONTEXT; aTestAlert : ByteBool) : longint;
  stdcall; external 'ntdll' name 'NtContinue';

var
  signal_list : Array[SIGABRT..SIGMAX] of SignalHandler;

const
  VectoredHandler : pointer = nil;

threadvar
  PendingSignal : longint;
  PendingResetFPU : boolean;
  PendingContext : array[0..SizeOf(CONTEXT)-1] of byte;


// Signal for a hardware exception code, 0 for other codes; aResetFPU tells whether the FPU must be reset
function ExceptionCodeToSignal(aCode : cardinal; var aResetFPU : boolean) : longint;

begin
  aResetFPU:=false;
  case aCode of
    EXCEPTION_ACCESS_VIOLATION,
    EXCEPTION_ARRAY_BOUNDS_EXCEEDED,
    EXCEPTION_STACK_OVERFLOW,
    EXCEPTION_ILLEGAL_INSTRUCTION,
    EXCEPTION_PRIV_INSTRUCTION,
    EXCEPTION_IN_PAGE_ERROR,
    EXCEPTION_SINGLE_STEP:
      ExceptionCodeToSignal:=SIGSEGV;
    EXCEPTION_FLT_DENORMAL_OPERAND,
    EXCEPTION_FLT_DIVIDE_BY_ZERO,
    EXCEPTION_FLT_INVALID_OPERATION,
    EXCEPTION_FLT_OVERFLOW,
    EXCEPTION_FLT_STACK_CHECK,
    EXCEPTION_FLT_UNDERFLOW,
    STATUS_FLOAT_MULTIPLE_FAULTS,
    STATUS_FLOAT_MULTIPLE_TRAPS:
      begin
      ExceptionCodeToSignal:=SIGFPE;
      aResetFPU:=true;
      end;
    EXCEPTION_INT_DIVIDE_BY_ZERO,
    EXCEPTION_INT_OVERFLOW:
      ExceptionCodeToSignal:=SIGFPE;
  else
    ExceptionCodeToSignal:=0;
  end;
end;


{ Call the handler of the signal stored by SignalsExceptionHandler. A handler result of 0
  ends the program with that signal as runtime error, any other result resumes the
  faulting code with its registers as they were. }
procedure JumpToHandleSignal;

var
  lBuffer : array[0..SizeOf(CONTEXT)+15] of byte;
  lContext : PCONTEXT;
  lSignal : longint;

begin
  lContext:=PCONTEXT(Align(@lBuffer[0],16));
  Move(PendingContext,lContext^,SizeOf(CONTEXT));
  lSignal:=PendingSignal;
  PendingSignal:=0;
  if PendingResetFPU then
    SysResetFPU;
  if signal_list[lSignal](lSignal)=0 then
    RunError(lSignal);
  NtContinue(lContext,false);
end;


{ Vectored exception handler: continues a hardware exception whose signal has a handler
  in JumpToHandleSignal; other exceptions are left to the next handlers. }
function SignalsExceptionHandler(aExceptionInfo : PEXCEPTION_POINTERS) : longint; stdcall;

var
  lSignal : longint;
  lResetFPU : boolean;
  lContext : PCONTEXT;

begin
  SignalsExceptionHandler:=EXCEPTION_CONTINUE_SEARCH;
  lSignal:=ExceptionCodeToSignal(aExceptionInfo^.ExceptionRecord^.ExceptionCode,lResetFPU);
  if (lSignal=0) or (signal_list[lSignal]=@SIG_DFL) or (PendingSignal<>0) then
    exit;
  lContext:=aExceptionInfo^.ContextRecord;
  Move(lContext^,PendingContext,SizeOf(CONTEXT));
  PCONTEXT(@PendingContext)^.ContextFlags:=PCONTEXT(@PendingContext)^.ContextFlags and not CONTEXT_XSTATE_BIT;
  PendingSignal:=lSignal;
  PendingResetFPU:=lResetFPU;
  { Continue in JumpToHandleSignal with the stack of a call made below the faulting code }
{$ifdef CPUX86_64}
  lContext^.Rsp:=((lContext^.Rsp-64) and not DWORD64(15))-8;
  lContext^.Rip:=DWORD64(@JumpToHandleSignal);
{$else CPUX86_64}
  lContext^.Esp:=((lContext^.Esp-64) and not DWORD(15))-4;
  lContext^.Eip:=DWORD(@JumpToHandleSignal);
{$endif CPUX86_64}
  SignalsExceptionHandler:=EXCEPTION_CONTINUE_EXECUTION;
end;


// Install SignalsExceptionHandler as first vectored exception handler
procedure install_exception_handler;

begin
  if VectoredHandler=nil then
    VectoredHandler:=AddVectoredExceptionHandler(1,@SignalsExceptionHandler);
end;


// Remove SignalsExceptionHandler
procedure remove_exception_handler;

begin
  if VectoredHandler<>nil then
    begin
    RemoveVectoredExceptionHandler(VectoredHandler);
    VectoredHandler:=nil;
    end;
end;


function SIG_ERR(x:longint):longint; cdecl;
begin
  SIG_ERR:=-1;
end;


function SIG_IGN(x:longint):longint; cdecl;
begin
  SIG_IGN:=-1;
end;


function SIG_DFL(x:longint):longint; cdecl;
begin
  SIG_DFL:=0;
end;

function signal(sig : longint;func : SignalHandler) : SignalHandler;
var
  temp : SignalHandler;
begin
  if ((sig < SIGABRT) or (sig > SIGMAX) or (sig = SIGKILL)) then
   begin
     signal:=@SIG_ERR;
     runerror(201);
   end;
  install_exception_handler;
  temp := signal_list[sig];
  signal_list[sig] := func;
  signal:=temp;
end;


var
  i : longint;
initialization
  for i:=SIGABRT to SIGMAX do
    signal_list[i]:=@SIG_DFL;
finalization
  remove_exception_handler;
