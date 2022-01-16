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
         function pass_typecheck_cpu: tnode; override;
         procedure pass_generate_code_cpu;override;
         function typecheck_faraddr: tnode;
         function typecheck_seg: tnode; override;
         function first_seg: tnode; override;
         procedure second_seg; override;
         procedure second_get_frame;override;
         function first_IncDec: tnode;override;
         procedure second_incdec;override;
         procedure second_abs_long;override;
       end;

implementation

  uses
    ninl,
    systems,
    globtype,globals,
    cutils,verbose,compinnr,
    constexp,
    symconst,
    defutil,
    aasmbase,aasmtai,aasmdata,aasmcpu,
    symtype,symdef,symsym,symcpu,
    cgbase,pass_1,pass_2,
    cpuinfo,cpubase,paramgr,
    nbas,nadd,ncon,ncal,ncnv,nld,nmem,nmat,ncgutil,
    tgobj,
    cga,cgutils,cgx86,cgobj,hlcgobj,
    htypechk,procinfo;

     function ti8086inlinenode.pass_typecheck_cpu: tnode;
       begin
         case inlinenumber of
           in_faraddr_x:
             result:=typecheck_faraddr;
           else
             result:=inherited;
         end;
       end;

     procedure ti8086inlinenode.pass_generate_code_cpu;
       begin
         case inlinenumber of
           in_x86_inportl,
           in_x86_outportl:
             internalerror(2018070302);
           else
             inherited pass_generate_code_cpu;
         end;
       end;

     function ti8086inlinenode.typecheck_faraddr: tnode;
       var
         addr_node: tnode;
         addr_node_resultdef: tdef;
         seg_node: tnode;
       begin
         addr_node:=caddrnode.create(left);
         typecheckpass(addr_node);
         addr_node_resultdef:=addr_node.resultdef;
         if is_farpointer(addr_node.resultdef) or is_farprocvar(addr_node.resultdef) then
           begin
             left:=nil;
             result:=addr_node;
           end
         else
           begin
             seg_node:=geninlinenode(in_seg_x,false,left.getcopy);
             inserttypeconv_internal(seg_node,u32inttype);
             seg_node:=cshlshrnode.create(shln,seg_node,cordconstnode.create(16,u8inttype,false));
             inserttypeconv_internal(addr_node,u32inttype);
             left:=nil;
             result:=caddnode.create(addn,seg_node,addr_node);
             if addr_node_resultdef.typ=pointerdef then
               inserttypeconv_internal(result,tcpupointerdef.getreusablex86(tpointerdef(addr_node_resultdef).pointeddef,x86pt_far))
             else
               inserttypeconv_internal(result,voidfarpointertype);
           end;
       end;

     function ti8086inlinenode.typecheck_seg: tnode;
       var
         isprocvar,need_conv_to_voidptr: Boolean;
         procpointertype: tdef;
         hsym: tfieldvarsym;
       begin
         result := nil;
         resultdef:=u16inttype;

         { don't allow constants }
         if is_constnode(left) then
          begin
            CGMessagePos(left.fileinfo,type_e_no_addr_of_constant);
            exit;
          end;

         { Handle Seg(proc) special, also Seg(procvar) in tp-mode needs
           special handling }
         if (left.resultdef.typ=procdef) or
            (
             { in case of nf_internal, follow the normal FPC semantics so that
               we can easily get the actual address of a procvar }
             not(nf_internal in flags) and
             (left.resultdef.typ=procvardef) and
             ((m_tp_procvar in current_settings.modeswitches) or
              (m_mac_procvar in current_settings.modeswitches))
            ) then
           begin
             isprocvar:=(left.resultdef.typ=procvardef);
             need_conv_to_voidptr:=
               (m_tp_procvar in current_settings.modeswitches) or
               (m_mac_procvar in current_settings.modeswitches);

             if not isprocvar then
               begin
                 if current_settings.x86memorymodel in x86_far_code_models then
                   begin
                     left:=ctypeconvnode.create_proc_to_procvar(left);
                     if need_conv_to_voidptr then
                       include(ttypeconvnode(left).convnodeflags,tcnf_proc_2_procvar_2_voidpointer);
                     left.fileinfo:=fileinfo;
                     typecheckpass(left);
                   end
                 else
                   exit;
               end;

             { In tp procvar mode for methodpointers we need to load the proc field }
             if need_conv_to_voidptr then
               begin
                 if not tabstractprocdef(left.resultdef).is_addressonly then
                   begin
                     { For procvars and for nested routines we need to return
                       the proc field of the methodpointer }
                     if isprocvar or
                        is_nested_pd(tabstractprocdef(left.resultdef)) then
                       begin
                         if tabstractprocdef(left.resultdef).is_methodpointer then
                           procpointertype:=methodpointertype
                         else
                           procpointertype:=nestedprocpointertype;
                         { find proc field in methodpointer record }
                         hsym:=tfieldvarsym(trecorddef(procpointertype).symtable.Find('proc'));
                         if not assigned(hsym) then
                           internalerror(2004120404);
                         { Load tmehodpointer(left).proc }
                         result:=csubscriptnode.create(
                                      hsym,
                                      ctypeconvnode.create_internal(left,procpointertype));
                         left:=nil;
                       end
                     else
                       CGMessage(type_e_variable_id_expected);
                   end;
               end;
(*             else
               begin
                 { Return the typeconvn only }
                 result:=left;
                 left:=nil;
               end;*)
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

         if left.resultdef.typ=procvardef then
           begin
             if left.resultdef.size<>4 then
               CGMessage(type_e_seg_procvardef_wrong_memory_model);
             case left.location.loc of
               LOC_REGISTER,LOC_CREGISTER:
                 begin
                   location_reset(location,LOC_REGISTER,OS_16);
                   location.register:=cg.GetNextReg(left.location.register);
                 end;
               LOC_CREFERENCE,LOC_REFERENCE:
                  begin
                    location_reset(location,LOC_REGISTER,OS_16);
                    segref:=left.location.reference;
                    inc(segref.offset,2);
                    location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
                    current_asmdata.CurrAsmList.concat(Taicpu.op_ref_reg(A_MOV,S_W,segref,location.register));
                  end;
               else
                 internalerror(2017121301);
             end;
           end
         else
           begin
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
             else
               begin
                 location_reset(location,LOC_REGISTER,OS_16);
                 location.register:=cg.getintregister(current_asmdata.CurrAsmList,OS_16);
                 current_asmdata.CurrAsmList.concat(Taicpu.op_reg_reg(A_MOV,S_W,get_default_segment_of_ref(left.location.reference),location.register));
               end;
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
             current_asmdata.CurrAsmList.Concat(Taicpu.op_reg_reg(A_MOV,S_W,NR_SS,cg.GetNextReg(location.register)));
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
                 internalerror(2020100815);
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


     procedure ti8086inlinenode.second_abs_long;
       var
         opsize: TCgSize;
       begin
         opsize:=def_cgsize(left.resultdef);
         if opsize in [OS_64,OS_S64] then
           begin
            secondpass(left);
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
            location:=left.location;
            location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            cg64.a_load64_reg_reg(current_asmdata.CurrAsmList,left.location.register64,location.register64);
            cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_16,15,cg.GetNextReg(left.location.register64.reghi));
            cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_16,cg.GetNextReg(left.location.register64.reghi),location.register64.reglo);
            cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_16,cg.GetNextReg(left.location.register64.reghi),cg.GetNextReg(location.register64.reglo));
            cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_16,cg.GetNextReg(left.location.register64.reghi),location.register64.reghi);
            cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_16,cg.GetNextReg(left.location.register64.reghi),cg.GetNextReg(location.register64.reghi));
            emit_reg_reg(A_SUB,S_W,cg.GetNextReg(left.location.register64.reghi),location.register64.reglo);
            emit_reg_reg(A_SBB,S_W,cg.GetNextReg(left.location.register64.reghi),cg.GetNextReg(location.register64.reglo));
            emit_reg_reg(A_SBB,S_W,cg.GetNextReg(left.location.register64.reghi),location.register64.reghi);
            emit_reg_reg(A_SBB,S_W,cg.GetNextReg(left.location.register64.reghi),cg.GetNextReg(location.register64.reghi));
           end
         else if opsize in [OS_32,OS_S32] then
           begin
            secondpass(left);
            hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
            location:=left.location;
            location.register:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,opsize,left.location.register,location.register);
            cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SAR,OS_16,15,cg.GetNextReg(left.location.register));
            cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_16,cg.GetNextReg(left.location.register),location.register);
            cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_XOR,OS_16,cg.GetNextReg(left.location.register),cg.GetNextReg(location.register));
            emit_reg_reg(A_SUB,S_W,cg.GetNextReg(left.location.register),location.register);
            emit_reg_reg(A_SBB,S_W,cg.GetNextReg(left.location.register),cg.GetNextReg(location.register));
           end
         else
           inherited second_abs_long;
       end;

begin
   cinlinenode:=ti8086inlinenode;
end.
