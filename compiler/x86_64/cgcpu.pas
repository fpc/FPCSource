{
    $Id$
    Copyright (c) 2002 by Florian Klaempfl

    This unit implements the code generator for the x86-64.

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation; either version 2 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program; if not, write to the Free Software
    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

 ****************************************************************************
}
{ This unit implements the code generator for the x86-64.
}
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       cgbase,cgobj,cg64f64,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,cpupara,
       node,symconst,rgx86;

    type
      tcgx86_64 = class(tcgx86)
        procedure init_register_allocators;override;
        class function reg_cgsize(const reg: tregister): tcgsize; override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;const funcretparaloc:tparalocation);override;
      end;


  implementation

    uses
       globtype,globals,verbose,systems,cutils,
       symdef,symsym,defutil,paramgr,
       rgobj,tgobj,rgcpu;


    procedure Tcgx86_64.init_register_allocators;
      begin
        inherited init_register_allocators;
        if cs_create_pic in aktmoduleswitches then
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_RAX,RS_RDX,RS_RCX,RS_RSI,RS_RDI,
            RS_R8,RS_R9,RS_R10,RS_R11,RS_R12,RS_R13,RS_R14,RS_R15],first_int_imreg,[RS_EBP,RS_EBX])
        else
          rg[R_INTREGISTER]:=trgcpu.create(R_INTREGISTER,R_SUBWHOLE,[RS_RAX,RS_RDX,RS_RCX,RS_RBX,RS_RSI,RS_RDI,
          RS_R8,RS_R9,RS_R10,RS_R11,RS_R12,RS_R13,RS_R14,RS_R15],first_int_imreg,[RS_EBP]);
        rg[R_MMXREGISTER]:=trgcpu.create(R_MMXREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7,
          RS_XMM8,RS_XMM9,RS_XMM10,RS_XMM11,RS_XMM12,RS_XMM13,RS_XMM14,RS_XMM15],first_sse_imreg,[]);
        rg[R_MMREGISTER]:=trgcpu.create(R_MMREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7],first_sse_imreg,[]);
        rgfpu:=Trgx86fpu.create;
      end;


    class function tcgx86_64.reg_cgsize(const reg: tregister): tcgsize;
    const subreg2cgsize:array[Tsubregister] of Tcgsize =
          (OS_NO,OS_8,OS_8,OS_16,OS_32,OS_64,OS_NO,OS_NO);

    begin
      case getregtype(reg) of
        R_INTREGISTER :
          reg_cgsize:=subreg2cgsize[getsubreg(reg)];
        R_FPUREGISTER :
          reg_cgsize:=OS_F80;
        R_MMXREGISTER:
          reg_cgsize:=OS_M64;
        R_MMREGISTER:
          reg_cgsize:=OS_M128;
        R_SPECIALREGISTER :
          case reg of
            NR_CS,NR_DS,NR_ES,NR_SS,NR_FS,NR_GS:
              reg_cgsize:=OS_16
            else
              reg_cgsize:=OS_32
          end
        else
            internalerror(200303181);
        end;
      end;


    procedure tcgx86_64.g_save_all_registers(list : taasmoutput);
      begin
        {$warning todo tcgx86_64.g_save_all_registers}
      end;


    procedure tcgx86_64.g_restore_all_registers(list : taasmoutput;const funcretparaloc:tparalocation);
      begin
        {$warning todo tcgx86_64.g_restore_all_registers}
      end;

begin
  cg:=tcgx86_64.create;
  cg64:=tcg64f64.create;
end.
{
  $Log$
  Revision 1.12  2004-02-22 18:27:21  florian
    * fixed exception reason size for 64 bit systems

  Revision 1.11  2004/02/09 22:14:17  peter
    * more x86_64 parameter fixes
    * tparalocation.lochigh is now used to indicate if registerhigh
      is used and what the type is

  Revision 1.10  2004/02/04 22:01:13  peter
    * first try to get cpupara working for x86_64

  Revision 1.9  2004/01/14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.8  2004/01/13 18:08:58  florian
    * x86-64 compilation fixed

  Revision 1.7  2003/12/24 01:47:23  florian
    * first fixes to compile the x86-64 system unit

  Revision 1.6  2003/12/22 19:00:17  florian
    * fixed some x86-64 issues

  Revision 1.5  2003/09/25 13:13:32  florian
    * more x86-64 fixes

  Revision 1.4  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.3  2003/01/05 13:36:54  florian
    * x86-64 compiles
    + very basic support for float128 type (x86-64 only)

  Revision 1.2  2002/07/25 22:55:33  florian
    * several fixes, small test units can be compiled

  Revision 1.1  2002/07/24 22:38:15  florian
    + initial release of x86-64 target code

}
