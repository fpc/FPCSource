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

CONST FPU_ALL=$7F;

{$ifdef cpui386}
function getfpustate(sininfo:psiginfo):longint; {inline;}
begin
  if ptruint(sininfo)> high(word) then
    getfpustate:=sininfo^.si_code
  else
    getfpustate:=word(ptruint(sininfo));
end;

function getaltfpustate(sigcontext:psigcontextrec):longint; {inline;}
begin
  if assigned(sigcontext) then
    getaltfpustate:=sigcontext^.sc_fpustate^.en_sw 
  else
    getaltfpustate:=0;
end;
{$endif}

procedure SignalToRunerror(Sig: longint;sininfo:psiginfo; SigContext: PSigContextRec); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
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
  case sig of
    SIGFPE :
          begin
    { this is not allways necessary but I don't know yet
      how to tell if it is or not PM }
          res:=200;
{$ifdef cpui386}
          fpustate:=GetaltFPUState(sigcontext);
{$else}
          fpustate:=0;
{$endif}
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
{$ifdef cpux86_64}
      HandleErrorAddrFrame(res,pointer(SigContext^.sc_rip),pointer(SigContext^.sc_rbp));
{$endif}
   end;
end;


