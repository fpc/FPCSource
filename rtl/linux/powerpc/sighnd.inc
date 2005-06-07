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


procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;
var
  res : word;
{  fpustate: longint; }
begin
  res:=0;
  { exception flags are turned off by kernel }
  fpc_enable_ppc_fpu_exceptions;
  case sig of
    SIGFPE :
        begin
{
         fpscr is cleared by the kernel -> can't find out cause :(
          fpustate := fpc_get_ppc_fpscr;
          if (fpustate and ppc_fpu_underflow) <> 0 then
            res := 206
          else if (fpustate and ppc_fpu_overflow) <> 0 then
            res := 205
          else if (fpustate and ppc_fpu_divbyzero) <> 0 then
            res := 200
          else
}
            res := 207;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
  end;
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(SigContext^.pt_regs^.nip),pointer(SigContext^.pt_regs^.gpr[1]));
end;


