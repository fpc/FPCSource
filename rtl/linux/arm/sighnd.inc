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

function signr_to_runerrornr(sig:longint;ucontext:Pucontext):word;

begin
  signr_to_runerrornr:=0;
  case sig of
    SIGFPE :
        begin
          { don't know how to find the different causes, maybe via xer? }
          signr_to_runerrornr := 207;
        end;
    SIGILL:
        if in_edsp_test then
          begin
            signr_to_runerrornr:=0;
            cpu_has_edsp:=false;
            inc(uContext^.uc_mcontext.arm_pc,4);
          end
        else
          signr_to_runerrornr:=216;
    SIGSEGV :
        signr_to_runerrornr:=216;
    SIGBUS:
        signr_to_runerrornr:=214;
    SIGINT:
        signr_to_runerrornr:=217;
    SIGQUIT:
        signr_to_runerrornr:=233;
  end;
end;

procedure SignalToAbort(sig : longint; SigInfo: PSigInfo; UContext: Pucontext);cdecl;
var
  res : word;
  s:string[5];
begin
  exitcode:=signr_to_runerrornr(sig,ucontext);
  reenable_signal(sig);

  {I had written a small stack dumper, but decided to remove it, because programs that
   activate the microexe mode are most likely exe size benchmarks. In the case they are not
   they are likely so primitive that it is unlikely that they require a stackdump to debug.
  dump_stack_micro(pointer(ucontext^.uc_mcontext.eip));}

  {Write runtime error message.}
  int_str(exitcode,s);  {int_str instead of str pulls in less code}
  write_micro('Runtime error '+s+' at $'+
              hexstr(longint(ucontext^.uc_mcontext.arm_pc),8)+   {typecast to longint to prevent pulling in int64 support}
              lineending);
  haltproc(exitcode);
end;

procedure SignalToRunerror(Sig: longint; { _a2,_a3,_a4 : dword; } SigContext: PSigInfo; uContext : PuContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
  res : word;
begin
  res:=signr_to_runerrornr(sig,ucontext);
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    HandleErrorAddrFrame(res,pointer(uContext^.uc_mcontext.arm_pc),pointer(uContext^.uc_mcontext.arm_fp));
end;


