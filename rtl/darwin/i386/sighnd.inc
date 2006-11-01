{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Signalhandler for FreeBSD/i386

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

procedure SignalToRunerror(Sig: cint; var info : tsiginfo_t;Var SigContext:SigContextRec); cdecl;

var
  res : word;

begin
  res:=0;
  case sig of
    SIGFPE :
      begin
        Case Info.si_code Of
          FPE_INTDIV,             { integer divide by zero -NOTIMP on Mac OS X 10.4.7 }
          FPE_FLTDIV : Res:=200;  { floating point divide by zero }
          FPE_FLTOVF : Res:=205;  { floating point overflow }
          FPE_FLTUND : Res:=206;  { floating point underflow }
          FPE_FLTRES,             { floating point inexact result }
          FPE_FLTINV : Res:=207;  { invalid floating point operation }
          Else
            Res:=207; { coprocessor error }
        end;
        { the following is true on ppc, but fortunately not on x86 }
        { FPU exceptions are completely disabled by the kernel if one occurred, it  }
        { seems this is necessary to be able to return to user mode. They can be    }
        { enabled by executing a sigreturn, however then the exception is triggered }
        { triggered again immediately if we don't turn off the "exception occurred" }
        { flags in fpscr                                                            }
        sysResetFPU;
      end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
  end;
  {$ifdef FPC_USE_SIGPROCMASK}
   reenable_signal(sig);
  {$endif }

  if (res <> 0) then
    HandleErrorAddrFrame(res,pointer(sigcontext.ts.eip),pointer(sigcontext.ts.ebp));
end;

