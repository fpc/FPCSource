unit signals;

interface

  { Signals }
  const
    SIGABRT   = 288;
    SIGFPE    = 289;
    SIGILL    = 290;
    SIGSEGV   = 291;
    SIGTERM   = 292;
    SIGALRM   = 293;
    SIGHUP    = 294;
    SIGINT    = 295;
    SIGKILL   = 296;
    SIGPIPE   = 297;
    SIGQUIT   = 298;
    SIGUSR1   = 299;
    SIGUSR2   = 300;
    SIGNOFP   = 301;
    SIGTRAP   = 302;
    SIGTIMR   = 303;    { Internal for setitimer (SIGALRM, SIGPROF) }
    SIGPROF   = 304;
    SIGMAX    = 320;

    SIG_BLOCK   = 1;
    SIG_SETMASK = 2;
    SIG_UNBLOCK = 3;

  function SIG_DFL( x: longint) : longint;

  function SIG_ERR( x: longint) : longint;

  function SIG_IGN( x: longint) : longint;

  type

    SignalHandler  = function (v : longint) : longint;

    PSignalHandler = ^SignalHandler; { to be compatible with linux.pp }

  function signal(sig : longint;func : SignalHandler) : SignalHandler;

  const

     EXCEPTION_MAXIMUM_PARAMETERS = 15;

  type

     FLOATING_SAVE_AREA = record
          ControlWord : DWORD;
          StatusWord : DWORD;
          TagWord : DWORD;
          ErrorOffset : DWORD;
          ErrorSelector : DWORD;
          DataOffset : DWORD;
          DataSelector : DWORD;
          RegisterArea : array[0..79] of BYTE;
          Cr0NpxState : DWORD;
       end;
     _FLOATING_SAVE_AREA = FLOATING_SAVE_AREA;
     TFLOATINGSAVEAREA = FLOATING_SAVE_AREA;
     PFLOATINGSAVEAREA = ^FLOATING_SAVE_AREA;

     CONTEXT = record
          ContextFlags : DWORD;
          Dr0 : DWORD;
          Dr1 : DWORD;
          Dr2 : DWORD;
          Dr3 : DWORD;
          Dr6 : DWORD;
          Dr7 : DWORD;
          FloatSave : FLOATING_SAVE_AREA;
          SegGs : DWORD;
          SegFs : DWORD;
          SegEs : DWORD;
          SegDs : DWORD;
          Edi : DWORD;
          Esi : DWORD;
          Ebx : DWORD;
          Edx : DWORD;
          Ecx : DWORD;
          Eax : DWORD;
          Ebp : DWORD;
          Eip : DWORD;
          SegCs : DWORD;
          EFlags : DWORD;
          Esp : DWORD;
          SegSs : DWORD;
       end;
     LPCONTEXT = ^CONTEXT;
     _CONTEXT = CONTEXT;
     TCONTEXT = CONTEXT;
     PCONTEXT = ^CONTEXT;


  type
     pexception_record = ^exception_record;
     EXCEPTION_RECORD  = record
       ExceptionCode   : longint;
       ExceptionFlags  : longint;
       ExceptionRecord : pexception_record;
       ExceptionAddress : pointer;
       NumberParameters : longint;
       ExceptionInformation : array[0..EXCEPTION_MAXIMUM_PARAMETERS-1] of pointer;
     end;

     PEXCEPTION_POINTERS = ^EXCEPTION_POINTERS;
     EXCEPTION_POINTERS = record
       ExceptionRecord   : PEXCEPTION_RECORD ;
       ContextRecord     : PCONTEXT ;
     end;



implementation


const
     EXCEPTION_ACCESS_VIOLATION = $c0000005;
     EXCEPTION_BREAKPOINT = $80000003;
     EXCEPTION_DATATYPE_MISALIGNMENT = $80000002;
     EXCEPTION_SINGLE_STEP = $80000004;
     EXCEPTION_ARRAY_BOUNDS_EXCEEDED = $c000008c;
     EXCEPTION_FLT_DENORMAL_OPERAND = $c000008d;
     EXCEPTION_FLT_DIVIDE_BY_ZERO = $c000008e;
     EXCEPTION_FLT_INEXACT_RESULT = $c000008f;
     EXCEPTION_FLT_INVALID_OPERATION = $c0000090;
     EXCEPTION_FLT_OVERFLOW = $c0000091;
     EXCEPTION_FLT_STACK_CHECK = $c0000092;
     EXCEPTION_FLT_UNDERFLOW = $c0000093;
     EXCEPTION_INT_DIVIDE_BY_ZERO = $c0000094;
     EXCEPTION_INT_OVERFLOW = $c0000095;
     EXCEPTION_INVALID_HANDLE = $c0000008;
     EXCEPTION_PRIV_INSTRUCTION = $c0000096;
     EXCEPTION_NONCONTINUABLE_EXCEPTION = $c0000025;
     EXCEPTION_NONCONTINUABLE = $1;
     EXCEPTION_STACK_OVERFLOW = $c00000fd;
     EXCEPTION_INVALID_DISPOSITION = $c0000026;
     EXCEPTION_ILLEGAL_INSTRUCTION = $C000001D;
     EXCEPTION_IN_PAGE_ERROR = $C0000006;

     EXCEPTION_EXECUTE_HANDLER = 1;
     EXCEPTION_CONTINUE_EXECUTION = -(1);
     EXCEPTION_CONTINUE_SEARCH = 0;

  type
     { type of functions that should be used for exception handling }
     LPTOP_LEVEL_EXCEPTION_FILTER = function(excep :PEXCEPTION_POINTERS) : longint;stdcall;

     function SetUnhandledExceptionFilter(lpTopLevelExceptionFilter : LPTOP_LEVEL_EXCEPTION_FILTER)
       : LPTOP_LEVEL_EXCEPTION_FILTER;
       external 'kernel32' name 'SetUnhandledExceptionFilter';

var
  signal_list : Array[SIGABRT..SIGMAX] of SignalHandler;
var
  { value of the stack segment
    to check if the call stack can be written on exceptions }
  _SS : longint;

const
  fpucw : word = $1332;
  Exception_handler_installed : boolean = false;
  MAX_Level = 16;
  except_level : byte = 0;
var
  except_eip   : array[0..Max_level-1] of longint;
  except_signal : array[0..Max_level-1] of longint;
  reset_fpu    : array[0..max_level-1] of boolean;


  procedure JumpToHandleSignal;
    var
      res, eip, ebp, sigtype : longint;
    begin
      asm
        pushal
        movl (%ebp),%eax
        movl %eax,ebp
      end;
      if except_level>0 then
        dec(except_level)
      else
        exit;
      eip:=except_eip[except_level];

      sigtype:=except_signal[except_level];
      if reset_fpu[except_level] then
        asm
          fninit
          fldcw   fpucw
        end;
      if (sigtype>=SIGABRT) and (sigtype<=SIGMAX) and
         (signal_list[sigtype]<>@SIG_DFL) then
        begin
          res:=signal_list[sigtype](sigtype);
        end
      else
        res:=0;

      if res=0 then
        RunError(sigtype)
      else
        { jump back to old code }
        asm
          popal
          movl eip,%eax
          movl %eax,4(%ebp)
          ret
        end;
    end;



  function Signals_exception_handler(excep :PEXCEPTION_POINTERS) : longint;stdcall;
    var frame,res  : longint;
        function CallSignal(sigtype,frame : longint;must_reset_fpu : boolean) : longint;
          begin
            if frame=0 then
              CallSignal:=Exception_Continue_Search
            else
              begin
                 if except_level >= Max_level then
                   exit;
                 except_eip[except_level]:=excep^.ContextRecord^.Eip;
                 except_signal[except_level]:=sigtype;
                 reset_fpu[except_level]:=must_reset_fpu;
                 inc(except_level);
                 dec(excep^.ContextRecord^.Esp,4);
                 plongint (excep^.ContextRecord^.Esp)^ := excep^.ContextRecord^.Eip;
                 excep^.ContextRecord^.Eip:=longint(@JumpToHandleSignal);
                 CallSignal:=Exception_Continue_Execution;

              end;
          end;

    begin
{$ifdef i386}
       if excep^.ContextRecord^.SegSs=_SS then
         frame:=excep^.ContextRecord^.Ebp
       else
{$endif i386}
         frame:=0;
       { default : unhandled !}
       res:=Exception_Continue_Search;
{$ifdef SYSTEMEXCEPTIONDEBUG}
       if IsConsole then
         writeln(stderr,'Exception  ',
           hexstr(excep^.ExceptionRecord^.ExceptionCode,8));
{$endif SYSTEMEXCEPTIONDEBUG}
       case excep^.ExceptionRecord^.ExceptionCode of
         EXCEPTION_ACCESS_VIOLATION :
           res:=CallSignal(SIGSEGV,frame,false);
         { EXCEPTION_BREAKPOINT = $80000003;
         EXCEPTION_DATATYPE_MISALIGNMENT = $80000002;
         EXCEPTION_SINGLE_STEP = $80000004; }
         EXCEPTION_ARRAY_BOUNDS_EXCEEDED :
           res:=CallSignal(SIGSEGV,frame,false);
         EXCEPTION_FLT_DENORMAL_OPERAND :
           begin
             res:=CallSignal(SIGFPE,frame,true);
           end;
         EXCEPTION_FLT_DIVIDE_BY_ZERO :
           begin
             res:=CallSignal(SIGFPE,frame,true);
             {excep^.ContextRecord^.FloatSave.StatusWord:=excep^.ContextRecord^.FloatSave.StatusWord and $ffffff00;}
           end;
         {EXCEPTION_FLT_INEXACT_RESULT = $c000008f; }
         EXCEPTION_FLT_INVALID_OPERATION :
           begin
             res:=CallSignal(SIGFPE,frame,true);
           end;
         EXCEPTION_FLT_OVERFLOW :
           begin
             res:=CallSignal(SIGFPE,frame,true);
           end;
         EXCEPTION_FLT_STACK_CHECK :
           begin
             res:=CallSignal(SIGFPE,frame,true);
           end;
         EXCEPTION_FLT_UNDERFLOW :
           begin
             res:=CallSignal(SIGFPE,frame,true); { should be accepted as zero !! }
           end;
         EXCEPTION_INT_DIVIDE_BY_ZERO :
           res:=CallSignal(SIGFPE,frame,false);
         EXCEPTION_INT_OVERFLOW :
           res:=CallSignal(SIGFPE,frame,false);
         {EXCEPTION_INVALID_HANDLE = $c0000008;
         EXCEPTION_PRIV_INSTRUCTION = $c0000096;
         EXCEPTION_NONCONTINUABLE_EXCEPTION = $c0000025;
         EXCEPTION_NONCONTINUABLE = $1;}
         EXCEPTION_STACK_OVERFLOW :
           res:=CallSignal(SIGSEGV,frame,false);
         {EXCEPTION_INVALID_DISPOSITION = $c0000026;}
         EXCEPTION_ILLEGAL_INSTRUCTION,
         EXCEPTION_PRIV_INSTRUCTION,
         EXCEPTION_IN_PAGE_ERROR,
         EXCEPTION_SINGLE_STEP : res:=CallSignal(SIGSEGV,frame,false);
         end;
       Signals_exception_handler:=res;
    end;


  procedure install_exception_handler;
{$ifdef SYSTEMEXCEPTIONDEBUG}
    var
      oldexceptaddr,newexceptaddr : longint;
{$endif SYSTEMEXCEPTIONDEBUG}
    begin
      if Exception_handler_installed then
        exit;
{$ifdef SYSTEMEXCEPTIONDEBUG}
      asm
        movl $0,%eax
        movl %fs:(%eax),%eax
        movl %eax,oldexceptaddr
      end;
{$endif SYSTEMEXCEPTIONDEBUG}
      SetUnhandledExceptionFilter(@Signals_exception_handler);
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
      Exception_handler_installed := true;
    end;

  procedure remove_exception_handler;
    begin
      if not Exception_handler_installed then
        exit;
      SetUnhandledExceptionFilter(nil);
    end;


function SIG_ERR(x:longint):longint;
begin
  SIG_ERR:=-1;
end;


function SIG_IGN(x:longint):longint;
begin
  SIG_IGN:=-1;
end;


function SIG_DFL(x:longint):longint;
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
  if not Exception_handler_installed then
    install_exception_handler;
  temp := signal_list[sig];
  signal_list[sig] := func;
  signal:=temp;
end;


var
  i : longint;
initialization

{$ifdef i386}
  asm
    xorl %eax,%eax
    movw %ss,%ax
    movl %eax,_SS
  end;
{$endif i386}

  for i:=SIGABRT to SIGMAX do
    signal_list[i]:=@SIG_DFL;

  { install_exception_handler;
  delay this to first use
  as other units also might install their handlers PM }

finalization

  remove_exception_handler;
end.
