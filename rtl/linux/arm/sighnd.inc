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


procedure SignalToRunerror(Sig: longint; { _a2,_a3,_a4 : dword; } SigContext: PSigInfo; uContext : PuContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
  res,fpustate : word;
begin
  res:=0;
  case sig of
    SIGFPE :
        begin
          { don't know how to find the different causes, maybe via xer? }
          res := 207;
        end;
    SIGILL:
        if in_edsp_test then
          begin
            res:=0;
            cpu_has_edsp:=false;
            inc(uContext^.uc_mcontext.arm_pc,4);
          end
        else
          res:=216;
    SIGSEGV :
        res:=216;
    SIGBUS:
        res:=214;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
  end;
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(uContext^.uc_mcontext.arm_pc),pointer(uContext^.uc_mcontext.arm_fp));
end;


