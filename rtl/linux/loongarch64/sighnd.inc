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

const
  fpu_ctx_magic = $46505501;
  lsx_ctx_magic = $53580001;
  lasx_ctx_magic = $41535801;
  used_fp = 1 shl 0;
  adrerr_rd = 1 shl 30;
  adrerr_wr = 1 shl 31;
  fcsr_bits_clear_exceptions = $e0ffffff;

var
  PCtxInfo : PContext_Info;
  PFPU_Ctx : PFPU_Context;
  PLSX_Ctx : PLSX_Context;
  PLASX_Ctx : PLASX_Context;
  PX_Context : pointer;
  res : word;
  ferr: cUint;

begin
  res:=0;
  case sig of
    SIGFPE:
      begin
        res:=207;
        if (used_fp and uContext^.uc_mcontext.flags)<>0 then
          begin
            ferr:=0;
            PCtxInfo:=PContext_Info(uContext+1);
            PX_Context:=PCtxInfo+1;
            case (PCtxInfo^.magic) of
              fpu_ctx_magic: begin
                  PFPU_Ctx:=PX_Context;
                  ferr:=PFPU_Ctx^.fcsr;
                  PFPU_Ctx^.fcsr:=PFPU_Ctx^.fcsr and fcsr_bits_clear_exceptions;
                end;
              lsx_ctx_magic: begin
                  PLSX_Ctx:=PX_Context;
                  ferr:=PLSX_Ctx^.fcsr;
                  PLSX_Ctx^.fcsr:=PLSX_Ctx^.fcsr and fcsr_bits_clear_exceptions;
                end;
              lasx_ctx_magic: begin
                  PLASX_Ctx:=PX_Context;
                  ferr:=PLASX_Ctx^.fcsr;
                  PLASX_Ctx^.fcsr:=PLASX_Ctx^.fcsr and fcsr_bits_clear_exceptions;
                end;
            end;
            ferr:=ferr shr 24;
            case ferr of
              1: res:=207;
              2: res:=206;
              4: res:=205;
              8: res:=208;
              16: res:=207;
            else
              ;
            end;
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
    HandleErrorAddrFrame(res,
      pointer(uContext^.uc_mcontext.pc),
      pointer(uContext^.uc_mcontext.regs[22]));
end;
