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


{ use a trampoline which pushes the return address for proper unwinding }
Procedure SignalToHandleErrorAddrFrame (Errno : longint;addr : CodePointer; frame : Pointer); nostackframe; assembler;
asm
  pushl addr
  jmp HandleErrorAddrFrame
end;

procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; UContext: Pucontext);public name '_FPC_DEFAULTSIGHANDLER';cdecl;
var
  res,fpustate,MMState : word;
begin
  res:=0;
  case sig of
    SIGFPE :
      begin
        { this is not allways necessary but I don't know yet
          how to tell if it is or not PM }
        res:=200;
  {$ifdef SYSTEM_DEBUG}
        if assigned(ucontext^.uc_mcontext.fpstate) then
          begin
            writeln('magic: $',hexstr(ucontext^.uc_mcontext.fpstate^.magic,4));
            writeln('magic1: $',hexstr(ucontext^.uc_mcontext.fpstate^.sw_reserved.magic1,8));
          end
        else
          writeln('fpstate=nil');
  {$endif SYSTEM_DEBUG}
        if SigInfo^.si_code<>FPE_INTDIV then
          begin
            if assigned(ucontext^.uc_mcontext.fpstate) then
              begin
                FpuState:=ucontext^.uc_mcontext.fpstate^.sw;
                if (FpuState and FPU_ExceptionMask) <> 0 then
                  begin
                    { first check the more precise options }
                    if (FpuState and FPU_DivisionByZero)<>0 then
                      res:=208
                    else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow or FPU_Invalid))<>0 Then
                      res:=207
                    else if (FpuState and FPU_Overflow)<>0 then
                      res:=205
                    else if (FpuState and FPU_Underflow)<>0 then
                      res:=206
                    else if (FpuState and FPU_Denormal)<>0 then
                      res:=206
                    else
                      res:=207;  {'Coprocessor Error'}
                  end
                else
                  begin
                    { SSE data? }
                    if ucontext^.uc_mcontext.fpstate^.magic=0 then
                      begin
                        MMState:=ucontext^.uc_mcontext.fpstate^.mxcsr;
                        if (MMState and MM_ExceptionMask)<>0 then
                          begin
                            { first check the more precise options }
                            if (MMState and MM_DivisionByZero)<>0 then
                              res:=208
                            else if (MMState and MM_Invalid)<>0 Then
                              res:=207
                            else if (MMState and MM_Overflow)<>0 then
                              res:=205
                            else if (MMState and MM_Underflow)<>0 then
                              res:=206
                            else if (MMState and MM_Denormal)<>0 then
                              res:=206
                            else
                              res:=207;  {'Coprocessor Error'}
                          end;
                      end;
                  end;
               end;
          end;
        if assigned(ucontext^.uc_mcontext.fpstate) then
          with ucontext^.uc_mcontext.fpstate^ do
            begin
              { Reset Status word }
              sw:=sw and not(FPU_ExceptionMask);
              if magic=0 then
                mxcsr:=mxcsr and not(MM_ExceptionMask);
              cw:=Default8087CW;
              { Reset Tag word to $ffff for all empty }
              tag:=$ffff;
            end;
      end;
    SIGBUS:
      res:=214;
    SIGILL:
      if sse_check then
        begin
          os_supports_sse:=false;
          res:=0;
          inc(ucontext^.uc_mcontext.eip,3);
        end
      else
        res:=216;
    SIGSEGV :
      res:=216;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
  end;
  reenable_signal(sig);

  { give runtime error at the position where the signal was raised }
  if res<>0 then
    begin
      ucontext^.uc_mcontext.eax := res;
      ucontext^.uc_mcontext.edx := ucontext^.uc_mcontext.eip;
      ucontext^.uc_mcontext.ecx := ucontext^.uc_mcontext.ebp;
      ucontext^.uc_mcontext.eip := ptruint(@SignalToHandleErrorAddrFrame);
    end;
end;

