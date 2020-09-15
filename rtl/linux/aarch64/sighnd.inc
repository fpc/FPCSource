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

{ $define SYSTEM_DEBUG}

procedure SignalToRunerror(Sig: longint; SigInfo: PSigInfo; UContext: PUContext); public name '_FPC_DEFAULTSIGHANDLER'; cdecl;

var
  res : word;
begin
  res:=0;
  case sig of
    SIGFPE:
      begin
        res:=207;
{$ifdef SYSTEM_DEBUG}
        writeln('magic of FPSIMD_Context: $',hexstr(uContext^.uc_mcontext.FPSIMD_Context.head.magic,8));
        writeln('size of FPSIMD_Context: $',hexstr(uContext^.uc_mcontext.FPSIMD_Context.head.size,8));
{$endif SYSTEM_DEBUG}
        if (uContext^.uc_mcontext.FPSIMD_Context.head.magic=$46508001) and
           (uContext^.uc_mcontext.FPSIMD_Context.head.size=$210) then
           begin
             with uContext^.uc_mcontext.FPSIMD_Context do
               fpsr:=fpsr and not(fpu_exception_mask shr fpu_exception_mask_to_status_mask_shift);
           end;
      end;
    SIGILL:
        res:=216;
    SIGSEGV :
        res:=216;
    SIGBUS:
        res:=214;
    SIGINT:
        res:=217;
    SIGQUIT:
        res:=233;
  end;
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    begin
      uContext^.uc_mcontext.regs[0]:=res;
      uContext^.uc_mcontext.regs[1]:=uContext^.uc_mcontext.pc;
      uContext^.uc_mcontext.regs[2]:=uContext^.uc_mcontext.regs[29];
      pointer(uContext^.uc_mcontext.pc):=@HandleErrorAddrFrame;
    end;
end;
