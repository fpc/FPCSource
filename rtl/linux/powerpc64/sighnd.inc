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
  fpustate : dword;
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
        fpustate := DWord(PDWord(@SigContext^.fp_regs[PT_FPSCR])^);
        {$IFDEF EXCDEBUG}
        writeln('fpustate = ', hexstr(fpustate, sizeof(fpustate)*2));
        {$ENDIF}
        { distinguish between the different FPU exceptions }
        if (fpustate and ppc_fpu_underflow) <> 0 then
          res := 206
        else if (fpustate and ppc_fpu_overflow) <> 0 then
          res := 205
        else if (fpustate and ppc_fpu_divbyzero) <> 0 then
          res := 200
        else
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
  { reenable signal }
  reenable_signal(sig);

  { handle error }
  if res<>0 then
    HandleErrorAddrFrame(res, Pointer(SigContext^.gp_regs[PT_NIP]), Pointer(SigContext^.gp_regs[PT_R1]));
end;

