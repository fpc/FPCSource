{
   This file is part of the Free Pascal run time library.
   (c) 2008 by Jonas Maebe
   member of the Free Pascal development team.

   See the file COPYING.FPC, included in this distribution,
   for details about the copyright.

   Signalhandler for Darwin/arm

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY;without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}


procedure SignalToRunerror(Sig: cint; info : PSigInfo; SigContext:PSigContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;
var
  fpuexceptionflags: cardinal;
  res : word;
begin
  res:=0;
  case sig of
    SIGFPE :
      begin
        Case Info^.si_code Of
          FPE_FLTDIV : Res:=208;  { floating point divide by zero }
          FPE_INTDIV : Res:=200;  { integer divide by zero }
          FPE_FLTOVF : Res:=205;  { floating point overflow }
          FPE_FLTUND : Res:=206;  { floating point underflow }
          FPE_FLTRES,             { floating point inexact result }
          FPE_FLTINV : Res:=207;  { invalid floating point operation }
          Else
            Res:=207; {coprocessor error}
          SigContext^.uc_mcontext^.__ns.__fpsr:=SigContext^.uc_mcontext^.__ns.__fpsr and not(fpu_exception_mask shr fpu_exception_mask_to_status_mask_shift);
        end;
      end;
    SIGBUS:
        res:=214;
    SIGILL:
      begin
        { right now, macOS generates SIGILL signals for fpu exceptions on AArch64.
          Additionally, fpsr is 0 in the context when this happens. Fortunately,
          the esr is valid, so we can decode that one. }
        if (Info^.si_code=ILL_ILLTRP) and
           { Trapped AArch64 floating point exception }
           ((SigContext^.uc_mcontext^.__es.__esr and __ESR_EC_Mask)=__ESR_EC_TrappedAArch64_FloatingPoint) then
          begin
            { the FPU status bits in the ESR are valid }
            if (SigContext^.uc_mcontext^.__es.__esr and __ESR_ISS_TFV)<>0 then
              begin
                fpuexceptionflags:=(SigContext^.uc_mcontext^.__es.__esr shl fpu_exception_mask_to_status_mask_shift) and fpu_exception_mask;
                if (fpuexceptionflags and fpu_dze)<>0 then
                  res:=208
                else if (fpuexceptionflags and fpu_ofe)<>0 then
                  res:=205
                else if (fpuexceptionflags and fpu_ufe)<>0 then
                  res:=206
                else if (fpuexceptionflags and fpu_ioe)<>0 then
                  res:=207
                else if (fpuexceptionflags and fpu_ixe)<>0 then
                  res:=207
                else if (fpuexceptionflags and fpu_ide)<>0 then
                  res:=216
                else
                  { unknown FPU exception }
                  res:=207
              end
            else
              { unknown FPU exception }
              res:=207;
          end
        else
          res:=216;
        { for safety, always clear in case we had a SIGILL to prevent potential
          infinite trap loops, even if it can cause us to miss some FPU
          exceptions in case we process an actual illegal instruction }
        SigContext^.uc_mcontext^.__ns.__fpsr:=SigContext^.uc_mcontext^.__ns.__fpsr and not(fpu_exception_mask shr fpu_exception_mask_to_status_mask_shift);
      end;
    SIGSEGV :
        res:=216;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
  end;
  {$ifdef FPC_USE_SIGPROCMASK}
   reenable_signal(sig);
  {$endif }

  { return to trampoline }
  if res <> 0 then
    begin
      SigContext^.uc_mcontext^.__ss.__r[0] := res;
      SigContext^.uc_mcontext^.__ss.__r[1] := SigContext^.uc_mcontext^.__ss.__pc;
      SigContext^.uc_mcontext^.__ss.__r[2] := SigContext^.uc_mcontext^.__ss.__fp;
      pointer(SigContext^.uc_mcontext^.__ss.__pc) := @HandleErrorAddrFrame;
    end;
end;

