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
{$ifdef CPU68020}
  asm
    fmove.l fpucw,fpcr
    fmove.l fpust,fpsr
  end;
{$endif}
end;


function GetFPUState(const SigContext : TSigContext) : longint;
begin
  GetfpuState:=SigContext.psr;
{$ifdef SYSTEM_DEBUG}
  Writeln(stderr,'FpuState = ',GetFpuState);
{$endif SYSTEM_DEBUG}
end;



procedure SignalToRunerror(Sig: longint; Info : pointer; var SigContext: TSigContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;
var
  res,fpustate : word;
begin
  res:=0;
  case sig of
    SIGFPE :
          begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
          res:=200;
          fpustate:=GetFPUState(SigContext);

          if (FpuState and FPU_All) <> 0 then
            begin
              { first check the more precise options }
              if (FpuState and FPU_DivisionByZero)<>0 then
                res:=200
              else if (FpuState and FPU_Overflow)<>0 then
                res:=205
              else if (FpuState and FPU_Underflow)<>0 then
                res:=206
              else if (FpuState and FPU_Denormal)<>0 then
                res:=216
              else if (FpuState and (FPU_StackOverflow or FPU_StackUnderflow))<>0 then
                res:=207
              else if (FpuState and FPU_Invalid)<>0 then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
          ResetFPU;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        res:=216;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
  end;

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
