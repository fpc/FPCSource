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
  { Bits in control register }
  RoundingMode = $30;
  RoundingPrecision = $c0;
  InexactDecimal = $100;
  InexactOperation = $200;
  DivideByZero = $400;
  UnderFlow = $800;
  OverFlow = $1000;
  OperandError = $2000;
  SignalingNaN = $4000;
  BranchOnUnordered = $800;

  fpucw : longint = {InexactOperation or }DivideByZero or
    OverFlow or OperandError or
    SignalingNaN or BranchOnUnordered;
  fpust : longint = 0;
  { Bits in status register }
  FPU_Invalid = $80;
  FPU_Denormal = $8;
  FPU_DivisionByZero = $10;
  FPU_Overflow = $40;
  FPU_Underflow = $20;
  { m68k is not stack based }
  FPU_StackUnderflow = $0;
  FPU_StackOverflow = $0;
  FPU_All = $f8;


Procedure ResetFPU;
begin
  asm
    fmove.l fpucw,fpcr
    fmove.l fpust,fpsr
  end;
end;


function GetFPUState(const SigContext : TSigContext) : longint;
begin
  GetfpuState:=SigContext.psr;
{$ifdef SYSTEM_DEBUG}
  Writeln(stderr,'FpuState = ',GetFpuState);
{$endif SYSTEM_DEBUG}
end;


function signr_to_runerrornr(sig:longint;var sigcontext:Tsigcontext):word;

var fpustate:word;

begin
  signr_to_runerrornr:=0;
  case sig of
    SIGFPE :
        begin
          { this is not allways necessary but I don't know yet
            how to tell if it is or not PM }
          signr_to_runerrornr:=200;
          fpustate:=GetFPUState(SigContext);

          if (FpuState and FPU_All) <> 0 then
            begin
              { first check the more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                signr_to_runerrornr:=200
              else if (FpuState and FPU_Overflow)<>0 then
                signr_to_runerrornr:=205
              else if (FpuState and FPU_Underflow)<>0 then
                signr_to_runerrornr:=206
              else if (FpuState and FPU_Denormal)<>0 then
                signr_to_runerrornr:=216
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 then
                signr_to_runerrornr:=207
              else if (FpuState and FPU_Invalid)<>0 then
                signr_to_runerrornr:=216
              else
                signr_to_runerrornr:=207;  {'Coprocessor Error'}
            end;
          ResetFPU;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        signr_to_runerrornr:=216;
    SIGINT:
        signr_to_runerrornr:=217;
    SIGQUIT:
        signr_to_runerrornr:=233;
  end;
end;

procedure SignalToAbort(sig : longint; SigInfo: PSigInfo;var SigContext: TSigcontext);cdecl;
var
  s:string[5];
  addr:pointer;
begin
  addr:=nil;
  exitcode:=signr_to_runerrornr(sig,sigcontext);
  reenable_signal(sig);

  {I had written a small stack dumper, but decided to remove it, because programs that
   activate the microexe mode are most likely exe size benchmarks. In the case they are not
   they are likely so primitive that it is unlikely that they require a stackdump to debug.
  dump_stack_micro(pointer(ucontext^.uc_mcontext.eip));}

  {Write runtime error message.}
  int_str(exitcode,s);  {int_str instead of str pulls in less code}
  write_micro('Runtime error '+s+' at $'+
              hexstr(longint(addr),8)+   {typecast to longint to prevent pulling in int64 support}
              lineending);
  haltproc(exitcode);
end;

procedure SignalToRunerror(Sig: longint; Info : pointer; var SigContext: TSigContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;
var
  res : word;
begin
  res:=signr_to_runerrornr(sig,SigContext);

  reenable_signal(sig);

  { give runtime error at the position where the signal was raised }
  if res<>0 then
   begin
     { HandleErrorAddrFrame(res,SigContext.sc_pc,SigContext.sc_fp);}
     { fp is not saved in context record :( }
     HandleError(res);
     HandleError(res);
   end;
end;
