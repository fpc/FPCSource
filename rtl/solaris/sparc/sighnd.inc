{
    This file is part of the Free Pascal run time library.
    Copyright (c) 1999-2006 by Michael Van Canneyt,
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
  REG_PC = 1;
  REG_nPC = 2;
  REG_O6 = 17;
  REG_SP = REG_O6;

const
  FPE_INTDIV = 1;
  FPE_INTOVF = 2;
  FPE_FLTDIV = 3;
  FPE_FLTOVF = 4;
  FPE_FLTUND = 5;
  FPE_FLTRES = 6;
  FPE_FLTINV = 7;
  FPE_FLTSUB = 8;


procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;
var
  res : word;
  addr : pointer;
  frame : pointer;
begin
  res:=0;
  if assigned(sigcontext) then
    begin
      addr := pointer(sigcontext^.uc_mcontext.gregs[REG_PC]);
      frame := pointer(sigcontext^.uc_mcontext.gregs[REG_SP])
    end
  else
    begin
      addr := nil;
      frame := nil;
    end;
  case sig of
    SIGFPE :
        begin
          case  siginfo^.si_code of
            FPE_INTDIV:
              res:=200;
            FPE_INTOVF:
              res:=205;
            FPE_FLTDIV:
              res:=200;
            FPE_FLTOVF:
              res:=205;
            FPE_FLTUND:
              res:=206;
            else
              res:=207;
          end;
        end;
    SIGILL,
    SIGSEGV :
        begin
          res:=216;
        end;
    SIGBUS :
      begin
        res:=214;
      end;
  end;
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,addr,frame);
end;


