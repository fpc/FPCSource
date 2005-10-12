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
  fpustate : qword;
  {$IFDEF EXCDEBUG}
  p : pbyte;
  i, j : integer;
  {$ENDIF}
begin
  res:=0;
  {$IFDEF EXCDEBUG}
  writeln('signaltorunerror');
  { dump SigContext }    
  p := pbyte(SigContext);
  for j := 0 to sizeof(TSigContext) div 8 do begin
    for i := 0 to 7 do begin
      write(hexstr(p^, 2));
      inc(p);
    end;
    write('  ');
  end;
  writeln;
  {$ENDIF}

  { SigContext is actally a pointer to a ucontext structure. 
  So we do some ugly pointer casting to get it right again.
  
  See also in the *kernel* sources arch/ppc64/kernel/signal.c, 
  function setup_rt_sigframe() }
    
  SigContext := @(PUContext(SigContext)^.uc_mcontext);
  
  { exception flags are turned off by kernel }
  fpc_enable_ppc_fpu_exceptions;
  case sig of
    SIGFPE :
      begin
        { ugly typecast to get the FPSCR register contents }
        fpustate := QWord(PQWord(@SigContext^.fp_regs[PT_FPSCR])^);
        {$IFDEF EXCDEBUG}
        writeln('fpustate = ', hexstr(fpustate, sizeof(fpustate)*2));
        {$ENDIF}

        { TODO: distinguishing FPU signal type does not work as it should 
        if (fpustate and ppc_fpu_underflow) <> 0 then
          res := 206
        else if (fpustate and ppc_fpu_overflow) <> 0 then
          res := 205
        else if (fpustate and ppc_fpu_divbyzero) <> 0 then
          res := 200
        else }
          res := 207;
      end;
    SIGBUS :
      res:=214;
    SIGILL,
    SIGSEGV :
      res:=216;
  end;
  {$IFDEF EXCDEBUG}
  writeln('sig = ', sig);
  writeln('siginfo = ', hexstr(ptrint(siginfo), sizeof(siginfo)*2));
  writeln('sigcontext = ', hexstr(ptrint(sigcontext), sizeof(sigcontext)*2));

  writeln('sigcontext...signal = ', hexstr(sigcontext^.signal, 16));
  
  writeln('sigcontext^...regs = ', hexstr(ptrint(sigcontext^.regs), 16));
  {$ENDIF}
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }

  if res<>0 then
    HandleErrorAddrFrame(res, pointer(SigContext^.gp_regs[PT_NIP]), pointer(SigContext^.gp_regs[PT_R1]));
  {$IFDEF EXCDEBUG}
  readln;
  {$ENDIF}
end;

