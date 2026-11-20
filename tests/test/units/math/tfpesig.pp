{ %CPU=aarch64 }
{ %TARGET=linux }
{ The SIGFPE handler has to raise the exception class that matches si_code.
  On aarch64-linux every SIGFPE used to become EInvalidOp. The signals are
  queued to the process itself with rt_sigqueueinfo, which may set a
  positive si_code for its own pid, so the test does not depend on whether
  the processor traps floating point exceptions. }
program tfpesig;

{$mode objfpc}

uses
  BaseUnix, SysCall, SysUtils;

const
  FPE_FLTDIV = 3;
  FPE_FLTOVF = 4;
  FPE_FLTUND = 5;
  FPE_FLTINV = 7;

var
  failed: Integer;
  fpeset, oldset: TSigSet;

procedure Check(aCode: LongInt; aExpected: ExceptClass; aNum: Integer);
var
  info: TSigInfo;
  res: TSysResult;
  got: string;
begin
  FillChar(info,SizeOf(info),0);
  info.si_signo:=SIGFPE;
  info.si_code:=aCode;
  got:='no exception';
  try
    res:=do_syscall(syscall_nr_rt_sigqueueinfo,TSysParam(fpgetpid),TSysParam(SIGFPE),TSysParam(@info));
    if res<>0 then
      begin
        writeln('rt_sigqueueinfo failed: ',res);
        Halt(10);
      end;
  except
    on e: Exception do
      got:=e.ClassName;
  end;
  if got<>aExpected.ClassName then
    begin
      writeln('si_code ',aCode,': got ',got,', expected ',aExpected.ClassName);
      failed:=aNum;
    end;
end;

begin
  failed:=0;
  { the signal mask is inherited across execve: make sure SIGFPE is not blocked }
  fpsigemptyset(fpeset);
  fpsigaddset(fpeset,SIGFPE);
  if fpsigprocmask(SIG_UNBLOCK,@fpeset,@oldset)<>0 then
    Halt(11);
  Check(FPE_FLTDIV,EZeroDivide,1);
  Check(FPE_FLTOVF,EOverflow,2);
  Check(FPE_FLTUND,EUnderflow,3);
  Check(FPE_FLTINV,EInvalidOp,4);
  fpsigprocmask(SIG_SETMASK,@oldset,nil);
  if failed<>0 then
    Halt(failed);
  writeln('ok');
end.
