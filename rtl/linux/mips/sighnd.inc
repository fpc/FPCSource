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

procedure SignalToRunerror(sig : longint; SigInfo: PSigInfo; UContext: PUContext);cdecl;public name '_FPC_DEFAULTSIGHANDLER';
var
  res : word;
  addr : pointer;
  frame : pointer;
begin
  res:=0;
  addr:=nil;
  case sig of
    SIGFPE :
        begin
          addr := siginfo^._sifields._sigfault._addr;
          res := 207;
          
          case  siginfo^.si_code of
            FPE_INTDIV:
              res:=200;  // maps to EDivByZero
            FPE_INTOVF:
              res:=215;
            FPE_FLTDIV:
              res:=208;  // maps to EZeroDivide
            FPE_FLTOVF:
              res:=205;
            FPE_FLTUND:
              res:=206;
            else
              res:=207;
          end;
        end;
    SIGILL,
    SIGBUS,
    SIGSEGV :
        begin
          addr := siginfo^._sifields._sigfault._addr;
          res:=216;
        end;
  end;
  reenable_signal(sig);
  { give runtime error at the position where the signal was raised }
  if res<>0 then
    begin
      if assigned(UContext) then
        begin
          frame:=pointer(ptruint(UContext^.uc_mcontext.sigc_regs[30]));   { base pointer }
          addr:=pointer(ptruint(UContext^.uc_mcontext.sigc_pc));  { program counter }
          if sig=SIGFPE then
            begin
              { Clear FPU exception bits }
              UContext^.uc_mcontext.sigc_fpc_csr := UContext^.uc_mcontext.sigc_fpc_csr 
                and not (fpu_cause_mask or fpu_flags_mask);
            end;             
          { Change $a0, $a1, $a2 and sig_pc to HandleErrorAddrFrame parameters }
          UContext^.uc_mcontext.sigc_regs[4]:=res;
          UContext^.uc_mcontext.sigc_regs[5]:=ptrint(addr);
          UContext^.uc_mcontext.sigc_regs[6]:=ptrint(frame);
          UContext^.uc_mcontext.sigc_pc:=ptrint(@HandleErrorAddrFrame);
{$ifdef FPC_PIC}
          { With PIC, we also have to set $t9 to procedure address }
          UContext^.uc_mcontext.sigc_regs[25]:=ptrint(@HandleErrorAddrFrame);
{$endif FPC_PIC}
          { Let the system call HandleErrorAddrFrame }
          exit;
        end
      else
        begin
          frame:=nil;
          addr:=nil;
        end;
      if sig=SIGFPE then
        set_fsr(get_fsr and not (fpu_cause_mask or fpu_flags_mask));
      HandleErrorAddrFrame(res,addr,frame);
    end;
end;
