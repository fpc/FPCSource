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
unit cgcpu;

{$i fpcdefs.inc}

  interface

    uses
       cgbase,cgobj,cg64f64,cgx86,
       aasmbase,aasmtai,aasmcpu,
       cpubase,cpuinfo,cpupara,parabase,
       node,symconst,rgx86,procinfo;

    type
      tcgx86_64 = class(tcgx86)
        procedure init_register_allocators;override;
        procedure g_save_all_registers(list : taasmoutput);override;
        procedure g_restore_all_registers(list : taasmoutput;const funcretparaloc:tcgpara);override;
        procedure g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);override;
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
        rg[R_MMREGISTER]:=trgcpu.create(R_MMREGISTER,R_SUBNONE,[RS_XMM0,RS_XMM1,RS_XMM2,RS_XMM3,RS_XMM4,RS_XMM5,RS_XMM6,RS_XMM7,
          RS_XMM8,RS_XMM9,RS_XMM10,RS_XMM11,RS_XMM12,RS_XMM13,RS_XMM14,RS_XMM15],first_mm_imreg,[]);
        rgfpu:=Trgx86fpu.create;
      end;


    procedure tcgx86_64.g_save_all_registers(list : taasmoutput);
      begin
        {$warning todo tcgx86_64.g_save_all_registers}
      end;


    procedure tcgx86_64.g_restore_all_registers(list : taasmoutput;const funcretparaloc:tcgpara);
      begin
        {$warning todo tcgx86_64.g_restore_all_registers}
      end;


    procedure tcgx86_64.g_proc_exit(list : taasmoutput;parasize:longint;nostackframe:boolean);
      var
        stacksize : longint;
      begin
        { Release PIC register }
        if cs_create_pic in aktmoduleswitches then
          list.concat(tai_regalloc.dealloc(NR_PIC_OFFSET_REG,nil));

        { remove stackframe }
        if not nostackframe then
          begin
            if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
              begin
                stacksize:=current_procinfo.calc_stackframe_size;
                if (stacksize<>0) then
                  cg.a_op_const_reg(list,OP_ADD,OS_ADDR,stacksize,current_procinfo.framepointer);
              end
            else
              list.concat(Taicpu.op_none(A_LEAVE,S_NO));
            list.concat(tai_regalloc.dealloc(NR_FRAME_POINTER_REG,nil));
          end;

        list.concat(Taicpu.Op_none(A_RET,S_NO));
      end;

begin
  cg:=tcgx86_64.create;
{$ifndef cpu64bit}
  cg64:=tcg64f64.create;
{$endif cpu64bit}
end.
{
  $Log$
  Revision 1.17  2004-10-05 20:41:02  peter
    * more spilling rewrites

  Revision 1.16  2004/09/21 17:25:13  peter
    * paraloc branch merged

  Revision 1.15.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.15  2004/07/09 23:30:13  jonas
    *  changed first_sse_imreg to first_mm_imreg

  Revision 1.14  2004/06/20 08:55:32  florian
    * logs truncated

  Revision 1.13  2004/06/16 20:07:11  florian
    * dwarf branch merged

  Revision 1.12.2.6  2004/05/27 23:36:18  peter
    * nostackframe procdirective added

  Revision 1.12.2.5  2004/05/02 21:34:01  florian
    * i386 compilation fixed

  Revision 1.12.2.4  2004/05/02 20:20:59  florian
    * started to fix callee side result value handling

  Revision 1.12.2.3  2004/05/01 11:12:24  florian
    * spilling of registers with size<>4 fixed

}
