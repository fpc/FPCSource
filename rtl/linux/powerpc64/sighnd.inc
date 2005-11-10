{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2005 by Thomas Schatzl,
    member of the Free Pascal development team.

    Signal handler is arch dependant due to processor to language
    exception conversion.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; context: PUContext); cdecl;
var
  res : word;
begin
  res:=0;

  { exception flags are turned off by kernel }
  fpc_enable_ppc_fpu_exceptions;
  case sig of
    SIGFPE :
      { distuingish between different FPU exceptions }
      case (SigInfo^.si_code) of
        FPE_FLTDIV : res := 200;
        FPE_FLTOVF : res := 205;
        FPE_FLTUND : res := 206;
        else
          res := 207;
      end;
    SIGBUS :
      res:=214;
    SIGILL,
    SIGSEGV :
      res:=216;
  end;

  { reenable signal }
  reenable_signal(sig);

  { handle error }
  if res<>0 then
    HandleErrorAddrFrame(res,
      Pointer(context^.uc_mcontext.gp_regs[PT_NIP]),
      Pointer(context^.uc_mcontext.gp_regs[PT_R1]));
end;

