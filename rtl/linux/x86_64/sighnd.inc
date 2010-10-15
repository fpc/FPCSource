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


procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;
  var
    res,fpustate : word;
  begin
    res:=0;
    case sig of
      SIGFPE :
        begin
          { this is not allways necessary but I don't know yet
            how to tell if it is or not PM }
          res:=200;
          fpustate:=GetFPUState(SigContext^);
          if (FpuState and FPU_All) <> 0 then
            begin
              { first check the more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                res:=200
              else if (FpuState and FPU_Overflow)<>0 then
                res:=205
              else if (FpuState and FPU_Underflow)<>0 then
                res:=206
              else if (FpuState and FPU_Denormal)<>0 then
                res:=216
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 Then
                res:=207
              else if (FpuState and FPU_Invalid)<>0 then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
            SysResetFPU;
        end;
      SIGILL,
      SIGBUS,
      SIGSEGV:
        res:=216;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
    end;
    reenable_signal(sig);
    if res<>0 then
      HandleErrorAddrFrame(res,pointer(SigContext^.rip),pointer(SigContext^.rbp));
  end;


