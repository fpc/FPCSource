{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i8086 inline nodes

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
unit n8086inl;

{$i fpcdefs.inc}

interface

    uses
       nx86inl,node;

    type

       { ti8086inlinenode }

       ti8086inlinenode = class(tx86inlinenode)
         procedure second_round_real; override;
         procedure second_trunc_real; override;

         function typecheck_seg: tnode; override;
         function first_seg: tnode; override;
         procedure second_seg; override;
       private
         procedure load_fpu_location;
       end;

implementation

  uses
    ninl,
    systems,
    globtype,globals,
    cutils,verbose,
    symconst,
    defutil,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    symtype,symdef,
    cgbase,pass_2,
    cpuinfo,cpubase,paramgr,
    nbas,ncon,ncal,ncnv,nld,ncgutil,
    tgobj,
    cga,cgutils,cgx86,cgobj,hlcgobj,
    htypechk;

     procedure ti8086inlinenode.second_round_real;
       begin
         load_fpu_location;
         location_reset_ref(location,LOC_REFERENCE,OS_S64,0);
         tg.GetTemp(current_asmdata.CurrAsmList,resultdef.size,resultdef.alignment,tt_normal,location.reference);
         emit_ref(A_FISTP,S_IQ,location.reference);
         tcgx86(cg).dec_fpu_stack;
         emit_none(A_FWAIT,S_NO);
       end;

     procedure ti8086inlinenode.second_trunc_real;
       var
         oldcw,newcw : treference;
       begin
         tg.GetTemp(current_asmdata.CurrAsmList,2,2,tt_normal,oldcw);
         tg.GetTemp(current_asmdata.CurrAsmList,2,2,tt_normal,newcw);
         emit_ref(A_FNSTCW,S_NO,newcw);
         emit_ref(A_FNSTCW,S_NO,oldcw);
         emit_const_ref(A_OR,S_W,$0f00,newcw);
         load_fpu_location;
         emit_ref(A_FLDCW,S_NO,newcw);
         location_reset_ref(location,LOC_REFERENCE,OS_S64,0);
         tg.GetTemp(current_asmdata.CurrAsmList,resultdef.size,resultdef.alignment,tt_normal,location.reference);
         emit_ref(A_FISTP,S_IQ,location.reference);
         tcgx86(cg).dec_fpu_stack;
         emit_ref(A_FLDCW,S_NO,oldcw);
         emit_none(A_FWAIT,S_NO);
         tg.UnGetTemp(current_asmdata.CurrAsmList,oldcw);
         tg.UnGetTemp(current_asmdata.CurrAsmList,newcw);
       end;

     function ti8086inlinenode.typecheck_seg: tnode;
       begin
         result := nil;
         resultdef:=u16inttype;
       end;

     function ti8086inlinenode.first_seg: tnode;
       begin
         expectloc:=LOC_REGISTER;
         result:=nil;
       end;

     procedure ti8086inlinenode.second_seg;
       begin
         location_reset(location,LOC_REGISTER,OS_16);
         location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
         current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_DS,location.register));
       end;

     procedure ti8086inlinenode.load_fpu_location;
       var
         lnode: tnode;
       begin
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         location.register:=NR_FPU_RESULT_REG;
         if left.nodetype <> callparan then
           internalerror(2013031501);
         lnode := tcallparanode(left).left;
         secondpass(lnode);
         case lnode.location.loc of
           LOC_FPUREGISTER:
             ;
           LOC_CFPUREGISTER:
             begin
               cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,lnode.location.size,
                 lnode.location.size,lnode.location.register,location.register);
             end;
           LOC_REFERENCE,LOC_CREFERENCE:
             begin
               cg.a_loadfpu_ref_reg(current_asmdata.CurrAsmList,
                  lnode.location.size,lnode.location.size,
                  lnode.location.reference,location.register);
             end;
           LOC_MMREGISTER,LOC_CMMREGISTER:
             begin
               location:=lnode.location;
               location_force_fpureg(current_asmdata.CurrAsmList,location,false);
             end;
           else
             internalerror(309991);
         end;
       end;

begin
   cinlinenode:=ti8086inlinenode;
end.
