{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2000 by Michael Van Canneyt,
    member of the Free Pascal development team.

    Signal handler is arch dependant due to processor to language
    exception conversion.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}


const
  FPU_All = $7f;

function GetFPUState(const SigContext : TSigContext) : word;
  begin
    if assigned(SigContext.fpstate) then
      GetfpuState:=SigContext.fpstate^.swd;
  {$ifdef SYSTEM_DEBUG}
    writeln('xx:',sigcontext.en_tw,' ',sigcontext.en_cw);
  {$endif SYSTEM_DEBUG}
  {$ifdef SYSTEM_DEBUG}
    Writeln(stderr,'FpuState = ',result);
  {$endif SYSTEM_DEBUG}
  end;


function signr_to_runerrornr(sig:longint;context:Psigcontext):word;

var fpustate:word;

begin
  signr_to_runerrornr:=0;
  case sig of
    SIGFPE :
      begin
        { this is not allways necessary but I don't know yet
          how to tell if it is or not PM }
        signr_to_runerrornr:=200;
        fpustate:=GetFPUState(context^);
        if (FpuState and FPU_All) <> 0 then
          begin
            { first check the more precise options }
            if (FpuState and FPU_DivisionByZero)<>0 then
              signr_to_runerrornr:=200
            else if (FpuState and FPU_Overflow)<>0 then
              signr_to_runerrornr:=205
            else if (FpuState and FPU_Underflow)<>0 then
              signr_to_runerrornr:=206
            else if (FpuState and FPU_Denormal)<>0 then
              signr_to_runerrornr:=216
            else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 Then
              signr_to_runerrornr:=207
            else if (FpuState and FPU_Invalid)<>0 then
              signr_to_runerrornr:=216
            else
              signr_to_runerrornr:=207;  {'Coprocessor Error'}
          end;
        SysResetFPU;
      end;
    SIGILL,
    SIGBUS,
    SIGSEGV:
      signr_to_runerrornr:=216;
    SIGINT:
      signr_to_runerrornr:=217;
    SIGQUIT:
      signr_to_runerrornr:=233;
    end;
end;

procedure SignalToAbort(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;
var
  s:string[5];
begin
  exitcode:=signr_to_runerrornr(sig,sigcontext);
  reenable_signal(sig);

  {I had written a small stack dumper, but decided to remove it, because programs that
   activate the microexe mode are most likely exe size benchmarks. In the case they are not
   they are likely so primitive that it is unlikely that they require a stackdump to debug.
  dump_stack_micro(pointer(ucontext^.uc_mcontext.eip));}

  {Write runtime error message.}
  int_str(exitcode,s);  {int_str instead of str pulls in less code}
  write_micro('Runtime error '+s+' at $'+
              hexstr(sigcontext^.rip,16)+
              lineending);
  haltproc(exitcode);
end;

procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
  res,fpustate : word;
begin
  res:=signr_to_runerrornr(sig,SigContext);
  reenable_signal(sig);
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(SigContext^.rip),pointer(SigContext^.rbp));
end;


