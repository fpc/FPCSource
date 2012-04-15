{
   This file is part of the Free Pascal run time library.
   (c) 2000-2003 by Marco van de Voort
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Signalhandler for FreeBSD/i386

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{
/* SIGFPE */
#define	FPE_INTDIV	1	/* Integer divide by zero		*/
#define	FPE_INTOVF	2	/* Integer overflow			*/
#define	FPE_FLTDIV	3	/* Floating point divide by zero	*/
#define	FPE_FLTOVF	4	/* Floating point overflow		*/
#define	FPE_FLTUND	5	/* Floating point underflow		*/
#define	FPE_FLTRES	6	/* Floating point inexact result	*/
#define	FPE_FLTINV	7	/* Invalid Floating point operation	*/
#define	FPE_FLTSUB	8	/* Subscript out of range		*/
}

const
  FPE_IntDiv = 1;
  FPE_IntOvf = 2;
  FPE_FltDiv = 3;
  FPE_FltOvf = 4;
  FPE_FltUnd = 5;
  FPE_FltRes = 6;
  FPE_FltInv = 7;
  FPE_FltSub = 8;

procedure SignalToRunerror(Sig: longint;info : PSigInfo;SigContext: PSigContextRec); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
  code : ptruint;
  res,fpustate : word;
begin
  res:=0;
{$ifdef BSD}
{$ifdef cpui386}
  fpustate:=0;
  asm
    fnstsw fpustate
  end;
{$endif cpui386}
{$endif BSD}
{$ifdef DEBUG}
         Writeln(stderr,'Info is 0x',hexstr(ptruint(info),8));
{$endif}
  case sig of
    SIGFPE :
          begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
          res:=200;
          { In netbsd, the second parameter is a code, not a pointer to a siginfo structure
            at least as long as we use sigaction14 }
          code:=ptruint(info);
          if code < high(fpustate) then
            fpustate:=code
          else
            fpustate:=FPE_IntDiv;

          { if (FpuState and FPU_All) <> 0 then }
           begin
              { first check the more precise options }
              if FpuState = FPE_IntDiv then
                res:=200
              else if (FpuState = FPE_IntOvf) or (FpuState = FPE_FltOvf) then
                res:=205
              else if FpuState = FPE_FltUnd then
                res:=206
              { else if FpuState and FPU_Denormal)<>0 then
                res:=216 }
              else if FpuState = FPE_FltSub then
                res:=207
             else if FpuState = FPE_FltInv then
                res:=216
              else
                res:=207;  {'Coprocessor Error'}
            end;
          SysResetFPU;
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
{$ifdef cpui386}
      if assigned(SigContext) then
        HandleErrorAddrFrame(res,pointer(SigContext^.sc_eip),pointer(SigContext^.sc_ebp))
      else
        HandleErrorAddrFrame(res,nil,nil);
{$endif}
   end;
end;


