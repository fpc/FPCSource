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


function signr_to_runerrornr(sig:longint;ucontext:Pucontext):word;

var fpustate:word;

begin
  signr_to_runerrornr:=0;
  case sig of
    SIGFPE :
      begin
        { this is not allways necessary but I don't know yet
          how to tell if it is or not PM }
        signr_to_runerrornr:=200;
        if assigned(ucontext^.uc_mcontext.fpstate) then
          begin
            FpuState:=ucontext^.uc_mcontext.fpstate^.sw;
            if (FpuState and FPU_ExceptionMask) <> 0 then
              begin
                { first check the more precise options }
                if (FpuState and FPU_DivisionByZero)<>0 then
                  signr_to_runerrornr:=200
                else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow or FPU_Invalid))<>0 Then
                  signr_to_runerrornr:=207
                else if (FpuState and FPU_Overflow)<>0 then
                  signr_to_runerrornr:=205
                else if (FpuState and FPU_Underflow)<>0 then
                  signr_to_runerrornr:=206
                else if (FpuState and FPU_Denormal)<>0 then
                  signr_to_runerrornr:=216
                else
                  signr_to_runerrornr:=207;  {'Coprocessor Error'}
              end;
            with ucontext^.uc_mcontext.fpstate^ do
              sw:=sw and not FPU_ExceptionMask;
          end;
      end;
    SIGBUS:
      signr_to_runerrornr:=214;
    SIGILL:
      if sse_check then
        begin
          os_supports_sse:=false;
          signr_to_runerrornr:=0;
          inc(ucontext^.uc_mcontext.eip,3);
        end
      else
        signr_to_runerrornr:=216;
    SIGSEGV :
      signr_to_runerrornr:=216;
    SIGINT:
        signr_to_runerrornr:=217;
    SIGQUIT:
        signr_to_runerrornr:=233;
  end;
end;

procedure SignalToAbort(sig : longint; SigInfo: PSigInfo; UContext: Pucontext);cdecl;
var
  s:string[5];
begin
  exitcode:=signr_to_runerrornr(sig,ucontext);
  reenable_signal(sig);

  {I had written a small stack dumper, but decided to remove it, because programs that
   activate the microexe mode are most likely exe size benchmarks. In the case they are not
   they are likely so primitive that it is unlikely that they require a stackdump to debug.
  dump_stack_micro(pointer(ucontext^.uc_mcontext.eip));}
  
  {Write runtime error message.}
  int_str(exitcode,s);  {int_str instead of str pulls in less code}
  write_micro('Runtime error '+s+' at $'+
              hexstr(longint(ucontext^.uc_mcontext.eip),8)+   {typecast to longint to prevent pulling in int64 support}
              lineending);
  haltproc(exitcode);
end;

procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; UContext: Pucontext);public name '_FPC_DEFAULTSIGHANDLER';cdecl;
var
  res : word;
begin
  res:=signr_to_runerrornr(sig,ucontext);
  reenable_signal(sig);
{ give runtime error at the position where the signal was raised }
  if res<>0 then
  begin
    ucontext^.uc_mcontext.eax := res;
    ucontext^.uc_mcontext.edx := ucontext^.uc_mcontext.eip;
    ucontext^.uc_mcontext.ecx := ucontext^.uc_mcontext.ebp;
    ucontext^.uc_mcontext.eip := ptruint(@HandleErrorAddrFrame);
  end;
end;



