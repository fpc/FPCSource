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

function signr_to_runerrornr(sig:longint;siginfo:Psiginfo):word;

begin
  case sig of
    SIGFPE :
      case (SigInfo^.si_code) of
        FPE_FLTDIV : signr_to_runerrornr := 200;
        FPE_FLTOVF : signr_to_runerrornr := 205;
        FPE_FLTUND : signr_to_runerrornr := 206;
        else
          signr_to_runerrornr := 207;
      end;
    SIGBUS :
      signr_to_runerrornr:=214;
    SIGILL,
    SIGSEGV :
      signr_to_runerrornr:=216;
    SIGINT:
      signr_to_runerrornr:=217;
    SIGQUIT:
      signr_to_runerrornr:=233;
  end;
end;

procedure SignalToAbort(sig : longint; SigInfo: PSigInfo; context: Pucontext);cdecl;
var
  s:string[5];
begin
  exitcode:=signr_to_runerrornr(sig,siginfo);
{$ifndef FPUNONE}
  { exception flags are turned off by kernel }
  fpc_enable_ppc_fpu_exceptions;
{$endif}
  reenable_signal(sig);

  {I had written a small stack dumper, but decided to remove it, because programs that
   activate the microexe mode are most likely exe size benchmarks. In the case they are not
   they are likely so primitive that it is unlikely that they require a stackdump to debug.
  dump_stack_micro(pointer(ucontext^.uc_mcontext.eip));}

  {Write runtime error message.}
  int_str(exitcode,s);  {int_str instead of str pulls in less code}
  write_micro('Runtime error '+s+' at $'+
              hexstr(longint(context^.uc_mcontext.pt_regs^.nip),8)+   {typecast to longint to prevent pulling in int64 support}
              lineending);
  haltproc(exitcode);
end;


procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; context: PUContext);public name '_FPC_DEFAULTSIGHANDLER';cdecl;
var
  res : word;
begin
  res:=signr_to_runerrornr(sig,siginfo);
{$ifndef FPUNONE}
  { exception flags are turned off by kernel }
  fpc_enable_ppc_fpu_exceptions;
{$endif}
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res, pointer(context^.uc_mcontext.pt_regs^.nip),
      pointer(context^.uc_mcontext.pt_regs^.gpr[1]));
end;


