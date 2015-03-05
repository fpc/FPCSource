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
         function typecheck_seg: tnode; override;
         function first_seg: tnode; override;
         procedure second_seg; override;
         procedure second_get_frame;override;
         function first_IncDec: tnode;override;
         procedure second_incdec;override;
       end;

implementation

  uses
    ninl,
    systems,
    globtype,globals,
    cutils,verbose,
    constexp,
    symconst,
    defutil,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    symtype,symdef,symcpu,
    cgbase,pass_1,pass_2,
    cpuinfo,cpubase,paramgr,
    nbas,nadd,ncon,ncal,ncnv,nld,ncgutil,
    tgobj,
    cga,cgutils,cgx86,cgobj,hlcgobj,
    htypechk,procinfo;

     function ti8086inlinenode.typecheck_seg: tnode;
       begin
         result := nil;
         resultdef:=u16inttype;

         { don't allow constants }
         if is_constnode(left) then
          begin
            CGMessagePos(left.fileinfo,type_e_no_addr_of_constant);
            exit;
          end;
       end;

     function ti8086inlinenode.first_seg: tnode;
       begin
         expectloc:=LOC_REGISTER;
         result:=nil;
       end;

     procedure ti8086inlinenode.second_seg;
       var
         segref: treference;
       begin
         secondpass(left);

         if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           internalerror(2013040101);

         { if a segment register is specified in ref, we use that }
         if left.location.reference.segment<>NR_NO then
           begin
             location_reset(location,LOC_REGISTER,OS_16);
             if is_segment_reg(left.location.reference.segment) then
               begin
                 location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
                 current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,left.location.reference.segment,location.register));
               end
             else
               location.register:=left.location.reference.segment;
           end
         { references relative to a symbol use the segment of the symbol,
           which can be obtained by the SEG directive }
         else if assigned(left.location.reference.symbol) then
           begin
             location_reset(location,LOC_REGISTER,OS_16);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
             reference_reset_symbol(segref,left.location.reference.symbol,0,0);
             segref.refaddr:=addr_seg;
             cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_16,OS_16,segref,location.register);
           end
         else if left.location.reference.base=NR_BP then
           begin
             location_reset(location,LOC_REGISTER,OS_16);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
             current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_SS,location.register));
           end
         else
           begin
             location_reset(location,LOC_REGISTER,OS_16);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
             current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_DS,location.register));
           end;
       end;

     procedure ti8086inlinenode.second_get_frame;
       begin
         if current_settings.x86memorymodel in x86_far_data_models then
           begin
             if current_procinfo.framepointer=NR_STACK_POINTER_REG then
               internalerror(2014030201);
             location_reset(location,LOC_REGISTER,OS_32);
             location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
             emit_reg_reg(A_MOV,S_W,current_procinfo.framepointer,location.register);
             current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_SS,GetNextReg(location.register)));
           end
         else
           inherited second_get_frame;
       end;

     function ti8086inlinenode.first_IncDec: tnode;
       var
         procname:string;
         elesize: Tconstexprint;
         hp: tnode;
       begin
         if is_hugepointer(tcallparanode(left).left.resultdef) then
           begin
             case inlinenumber of
               in_inc_x:
                 procname:='fpc_hugeptr_inc_longint';
               in_dec_x:
                 procname:='fpc_hugeptr_dec_longint';
               else
                 internalerror(2014121001);
             end;
             if cs_hugeptr_arithmetic_normalization in current_settings.localswitches then
               procname:=procname+'_normalized';

             if is_void(tpointerdef(tcallparanode(left).left.resultdef).pointeddef) then
               elesize:=1
             else
               elesize:=tpointerdef(tcallparanode(left).left.resultdef).pointeddef.size;

             hp := cordconstnode.create(elesize,s32inttype,false);
             { extra parameter? }
             if assigned(tcallparanode(left).right) then
               hp:=caddnode.create(muln,hp,tcallparanode(tcallparanode(left).right).left.getcopy);

             result:=ccallnode.createintern(procname,
                       ccallparanode.create(hp,
                       ccallparanode.create(tcallparanode(left).left.getcopy,nil)));
             typecheckpass(result);
             firstpass(result);
           end
         else
           result:=inherited;
       end;

     procedure ti8086inlinenode.second_incdec;
       const
         addsubop:array[in_inc_x..in_dec_x] of TOpCG=(OP_ADD,OP_SUB);
       var
         addvalue : TConstExprInt;
         addconstant : boolean;
         hregister : tregister;
         tmploc: tlocation;
       begin
         if is_farpointer(tcallparanode(left).left.resultdef) then
           begin
             { set defaults }
             addconstant:=true;
             hregister:=NR_NO;

             { first secondpass second argument, because if the first arg }
             { is used in that expression then SSL may move it to another }
             { register                                                   }
             if assigned(tcallparanode(left).right) then
               secondpass(tcallparanode(tcallparanode(left).right).left);
             { load first parameter, must be a reference }
             secondpass(tcallparanode(left).left);
             tmploc:=tcallparanode(left).left.location;
             tmploc.size:=OS_S16;
             { get addvalue }
             case tcallparanode(left).left.resultdef.typ of
               pointerdef :
                 begin
                   if is_void(tpointerdef(tcallparanode(left).left.resultdef).pointeddef) then
                     addvalue:=1
                   else
                     addvalue:=tpointerdef(tcallparanode(left).left.resultdef).pointeddef.size;
                 end;
               else
                 internalerror(10081);
             end;
             { second_ argument specified?, must be a s16bit in register }
             if assigned(tcallparanode(left).right) then
               begin
                 { when constant, just multiply the addvalue }
                 if is_constintnode(tcallparanode(tcallparanode(left).right).left) then
                    addvalue:=addvalue*get_ordinal_value(tcallparanode(tcallparanode(left).right).left)
                 else if is_constpointernode(tcallparanode(tcallparanode(left).right).left) then
                    addvalue:=addvalue*tpointerconstnode(tcallparanode(tcallparanode(left).right).left).value
                 else
                   begin
                     hlcg.location_force_reg(current_asmdata.CurrAsmList,tcallparanode(tcallparanode(left).right).left.location,tcallparanode(tcallparanode(left).right).left.resultdef,s16inttype,addvalue<=1);
                     hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
                     { insert multiply with addvalue if its >1 }
                     if addvalue>1 then
                       hlcg.a_op_const_reg(current_asmdata.CurrAsmList,OP_IMUL,s16inttype,addvalue.svalue,hregister);
                     addconstant:=false;
                   end;
               end;
             { write the add instruction }
             if addconstant then
               begin
                 hlcg.a_op_const_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],s16inttype,
                   smallint(addvalue.svalue),
                   tmploc);
               end
             else
               begin
                 hlcg.a_op_reg_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],s16inttype,
                   hregister,tmploc);
               end;
           end
         else
           inherited second_incdec;
       end;

begin
   cinlinenode:=ti8086inlinenode;
end.
