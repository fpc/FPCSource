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
  FPE_INTDIV = 1;
  FPE_INTOVF = 2;
  FPE_FLTDIV = 3;
  FPE_FLTOVF = 4;
  FPE_FLTUND = 5;
  FPE_FLTRES = 6;
  FPE_FLTINV = 7;
  FPE_FLTSUB = 8;

function signr_to_runerrornr(sig:longint;siginfo:Psiginfo;var addr:pointer):word;

begin
  signr_to_runerrornr:=0;
  case sig of
    SIGFPE :
        begin
          addr := siginfo^._sifields._sigfault.si_addr;
          signr_to_runerrornr := 207;
          case  siginfo^.si_code of
            FPE_INTDIV:
              signr_to_runerrornr:=200;
            FPE_INTOVF:
              signr_to_runerrornr:=205;
            FPE_FLTDIV:
              signr_to_runerrornr:=200;
            FPE_FLTOVF:
              signr_to_runerrornr:=205;
            FPE_FLTUND:
              signr_to_runerrornr:=206;
            FPE_FLTRES,
            FPE_FLTINV,
            FPE_FLTSUB:
              signr_to_runerrornr:=216;
            else
              signr_to_runerrornr:=207;
          end;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        begin
          addr := siginfo^._sifields._sigfault.si_addr;
          signr_to_runerrornr:=216;
        end;
  end;
end;

procedure SignalToAbort(sig : longint; SigInfo: PSigInfo; UContext: Pucontext);cdecl;
var
  s:string[5];
begin
  addr:=nil;
  exitcode:=signr_to_runerrornr(sig,siginfo,addr);
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

procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; SigContext: PSigContext);cdecl;
var
  res : word;
  addr : pointer;
begin
  addr:=nil;
  res:=signr_to_runerrornr(sig,siginfo,addr);
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,addr,nil);
end;
