{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   (c) 2025 adapted for FreeBSD/PowerPC64 ELFv2

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Signalhandler for FreeBSD/PowerPC64

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

procedure SignalToRunerror(sig: cint; info: psiginfo; sigcontext: PSigContext);
  public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
  res: word;

begin
  res:=0;

  { exception flags are turned off by kernel }
  SysResetFPU;
  case sig of
    SIGFPE:
      begin
        case info^.si_code of
          FPE_FLTDIV: res := 208;  { float divide fault }
          FPE_INTDIV: res := 200;  { integer divide fault }
          FPE_FLTOVF: res := 205;  { overflow trap }
          FPE_FLTUND: res := 206;  { underflow }
          FPE_FLTRES: res := 207;  { inexact result / device not available }
          FPE_FLTINV: res := 207;  { invalid floating point op }
        else
          res := 207;              { generic coprocessor error }
        end;
        sysResetFPU;               { reset FPSCR on PowerPC64 }
      end;
    SIGBUS :
      res:=214;
    SIGILL,
    SIGSEGV :
      res:=216;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
  end;

  {$ifdef FPC_USE_SIGPROCMASK}
  reenable_signal(sig);
  {$endif}

  { give runtime error at the position where the signal was raised }
  if res <> 0 then
    begin
    HandleError(res);
    end;
end;

