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

function GetFPUState(const SigContext : TSigContext) : longint;
begin
  if assigned(SigContext.fpstate) then
    GetfpuState:=SigContext.fpstate^.sw;
{$ifdef SYSTEM_DEBUG}
  writeln('xx:',sigcontext.en_tw,' ',sigcontext.en_cw);
{$endif SYSTEM_DEBUG}
{$ifdef SYSTEM_DEBUG}
  Writeln(stderr,'FpuState = ',GetFpuState);
{$endif SYSTEM_DEBUG}
end;


procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; UContext: Pucontext);cdecl;
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
          fpustate:=GetFPUState(UContext^.uc_mcontext);
          if (FpuState and FPU_All) <> 0 then
            begin
              { first check the more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                res:=200
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow or FPU_Invalid))<>0 Then
                res:=207
              else if (FpuState and FPU_Overflow)<>0 then
                res:=205
              else if (FpuState and FPU_Underflow)<>0 then
                res:=206
              else if (FpuState and FPU_Denormal)<>0 then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
            sysResetFPU;
        end;
    SIGBUS:
        res:=214;
    SIGILL,
    SIGSEGV :
        res:=216;
  end;
  reenable_signal(sig);
{ give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(UContext^.uc_mcontext.eip),pointer(UContext^.uc_mcontext.ebp));
end;

