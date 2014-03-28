{
    Copyright (c) 1998-2002 by Florian Klaempfl and Carl Eric Codere

    Generate generic inline nodes

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
unit ncginl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl;

    type
       tcginlinenode = class(tinlinenode)
          procedure pass_generate_code;override;
          procedure second_sizeoftypeof;virtual;
          procedure second_length;virtual;
          procedure second_predsucc;virtual;
          procedure second_incdec;virtual;
          procedure second_typeinfo;virtual;
          procedure second_includeexclude;virtual;
          procedure second_pi; virtual;
          procedure second_arctan_real; virtual;
          procedure second_abs_real; virtual;
          procedure second_sqr_real; virtual;
          procedure second_sqrt_real; virtual;
          procedure second_ln_real; virtual;
          procedure second_cos_real; virtual;
          procedure second_sin_real; virtual;
          procedure second_assigned; virtual;
          procedure second_get_frame;virtual;
          procedure second_get_caller_frame;virtual;
          procedure second_get_caller_addr;virtual;
          procedure second_prefetch; virtual;
          procedure second_round_real; virtual;
          procedure second_trunc_real; virtual;
          procedure second_abs_long; virtual;
          procedure second_rox_sar; virtual;
          procedure second_bsfbsr; virtual;
          procedure second_new; virtual;
          procedure second_setlength; virtual; abstract;
          procedure second_box; virtual; abstract;
          procedure second_popcnt; virtual;
          procedure second_seg; virtual; abstract;
       end;

implementation

    uses
      globtype,systems,constexp,
      cutils,verbose,globals,
      symconst,symdef,defutil,symsym,
      aasmbase,aasmtai,aasmdata,aasmcpu,parabase,
      cgbase,pass_1,pass_2,
      cpuinfo,cpubase,paramgr,procinfo,
      nbas,ncon,ncal,ncnv,nld,ncgrtti,
      tgobj,ncgutil,
      cgutils,cgobj,hlcgobj
{$ifndef cpu64bitalu}
      ,cg64f32
{$endif not cpu64bitalu}
      ;


{*****************************************************************************
                              TCGINLINENODE
*****************************************************************************}


    procedure tcginlinenode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         case inlinenumber of
            in_sizeof_x,
            in_typeof_x :
              second_SizeofTypeOf;
            in_length_x :
              second_Length;
            in_pred_x,
            in_succ_x:
               second_PredSucc;
            in_dec_x,
            in_inc_x :
              second_IncDec;
            in_typeinfo_x:
              second_TypeInfo;
            in_include_x_y,
            in_exclude_x_y:
              second_IncludeExclude;
            in_pi_real:
              second_pi;
            in_sin_real:
              second_sin_real;
            in_arctan_real:
              second_arctan_real;
            in_abs_real:
              second_abs_real;
            in_abs_long:
              second_abs_long;
            in_round_real:
              second_round_real;
            in_trunc_real:
              second_trunc_real;
            in_sqr_real:
              second_sqr_real;
            in_sqrt_real:
              second_sqrt_real;
            in_ln_real:
              second_ln_real;
            in_cos_real:
               second_cos_real;
            in_prefetch_var:
              second_prefetch;
            in_assigned_x:
              second_assigned;
            in_get_frame:
              second_get_frame;
            in_get_caller_frame:
              second_get_caller_frame;
            in_get_caller_addr:
              second_get_caller_addr;
            in_unaligned_x:
              begin
                secondpass(tcallparanode(left).left);
                location:=tcallparanode(left).left.location;
                if location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  location.reference.alignment:=1;
              end;
            in_aligned_x:
              begin
                secondpass(tcallparanode(left).left);
                location:=tcallparanode(left).left.location;
                if location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  location.reference.alignment:=0;
              end;
{$ifdef SUPPORT_MMX}
            in_mmx_pcmpeqb..in_mmx_pcmpgtw:
              begin
                 location_reset(location,LOC_MMXREGISTER,OS_NO);
                 if left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else if tcallparanode(left).left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else
                   begin
                      {!!!!!!!}
                   end;
              end;
{$endif SUPPORT_MMX}
            in_rol_x,
            in_rol_x_y,
            in_ror_x,
            in_ror_x_y,
            in_sar_x,
            in_sar_x_y:
              second_rox_sar;
            in_bsf_x,
            in_bsr_x:
               second_BsfBsr;
            in_new_x:
               second_new;
            in_setlength_x:
               second_setlength;
            in_box_x:
               second_box;
            in_popcnt_x:
               second_popcnt;
            in_seg_x:
               second_seg;
            else internalerror(9);
         end;
      end;



{*****************************************************************************
                          SIZEOF / TYPEOF GENERIC HANDLING
*****************************************************************************}

    { second_handle_ the sizeof and typeof routines }
    procedure tcginlinenode.second_SizeOfTypeOf;
      var
         href,
         hrefvmt   : treference;
         hregister : tregister;
      begin
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        { for both cases load vmt }
        if left.nodetype=typen then
          begin
            hregister:=hlcg.getaddressregister(current_asmdata.CurrAsmList,voidpointertype);
            reference_reset_symbol(href,current_asmdata.RefAsmSymbol(tobjectdef(left.resultdef).vmt_mangledname,AT_DATA),0,voidpointertype.size);
            hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,href,hregister);
          end
        else
          begin
            secondpass(left);
            hregister:=hlcg.getaddressregister(current_asmdata.CurrAsmList,voidpointertype);

            { handle self inside a method of a class }
            case left.location.loc of
              LOC_CREGISTER,
              LOC_REGISTER :
                begin
                  if (left.resultdef.typ=classrefdef) or
                     (po_staticmethod in current_procinfo.procdef.procoptions) then
                    hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,left.location.register,hregister)
                  else
                   begin
                     { load VMT pointer }
                     hlcg.reference_reset_base(hrefvmt,voidpointertype,left.location.register,tobjectdef(left.resultdef).vmt_offset,voidpointertype.size);
                     hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,hrefvmt,hregister);
                   end
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  if is_class(left.resultdef) then
                   begin
                     { deref class }
                     hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,left.location.reference,hregister);
                     hlcg.g_maybe_testself(current_asmdata.CurrAsmList,left.resultdef,hregister);
                     { load VMT pointer }
                     hlcg.reference_reset_base(hrefvmt,voidpointertype,hregister,tobjectdef(left.resultdef).vmt_offset,voidpointertype.size);
                     hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,hrefvmt,hregister);
                   end
                  else
                   begin
                     { load VMT pointer, but not for classrefdefs }
                     if (left.resultdef.typ=objectdef) then
                       begin
                         inc(left.location.reference.offset,tobjectdef(left.resultdef).vmt_offset);
                         left.location.reference.alignment:=newalignment(left.location.reference.alignment,tobjectdef(left.resultdef).vmt_offset);
                       end;
                     hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,left.location.reference,hregister);
                   end;
                end;
              else
                internalerror(200301301);
            end;
          end;
        { in sizeof load size }
        if inlinenumber=in_sizeof_x then
           begin
             hlcg.reference_reset_base(href,voidpointertype,hregister,0,voidpointertype.size);
             hregister:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
             cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,href,hregister);
           end;
        location.register:=hregister;
     end;


{*****************************************************************************
                          LENGTH GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_Length;
      var
        lengthlab : tasmlabel;
        hregister : tregister;
        href : treference;
      begin
        secondpass(left);
        if is_shortstring(left.resultdef) then
         begin
           location_copy(location,left.location);
           location.size:=OS_8;
         end
        else
         begin
           { length in ansi/wide strings and high in dynamic arrays is at offset -sizeof(pint) }
           hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);
           current_asmdata.getjumplabel(lengthlab);
           cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_ADDR,OC_EQ,0,left.location.register,lengthlab);
           if is_widestring(left.resultdef) and (tf_winlikewidestring in target_info.flags) then
             begin
               reference_reset_base(href,left.location.register,-sizeof(dword),sizeof(dword));
               hregister:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_INT);
               cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_INT,href,hregister);
             end
           else
             begin
               reference_reset_base(href,left.location.register,-sizeof(pint),sizeof(pint));
               hregister:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_INT);
               cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,href,hregister);
             end;
           if is_widestring(left.resultdef) then
             cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,OS_INT,1,hregister);

           { Dynamic arrays do not have their length attached but their maximum index }
           if is_dynamic_array(left.resultdef) then
             cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_ADD,OS_INT,1,hregister);

           cg.a_label(current_asmdata.CurrAsmList,lengthlab);
           location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
           location.register:=hregister;
         end;
      end;


{*****************************************************************************
                         PRED/SUCC GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_PredSucc;
      var
         cgop : topcg;
      begin
        secondpass(left);
        if inlinenumber=in_pred_x then
           cgop:=OP_SUB
        else
           cgop:=OP_ADD;

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        if not(left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
          hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,false);

{$ifndef cpu64bitalu}
        if def_cgsize(resultdef) in [OS_64,OS_S64] then
          begin
            location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
            cg64.a_op64_const_reg_reg(current_asmdata.CurrAsmList,cgop,def_cgsize(resultdef),1,left.location.register64,location.register64);
          end
        else
{$endif not cpu64bitalu}
          begin
            location.register:=hlcg.getregisterfordef(current_asmdata.CurrAsmList,resultdef);
            hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,cgop,resultdef,1,left.location.register,location.register);
          end;
      end;


{*****************************************************************************
                         INC/DEC GENERIC HANDLING
*****************************************************************************}
      procedure tcginlinenode.second_IncDec;
       const
         addsubop:array[in_inc_x..in_dec_x] of TOpCG=(OP_ADD,OP_SUB);
        var
         addvalue : TConstExprInt;
         addconstant : boolean;
{$ifndef cpu64bitalu}
         hregisterhi,
{$endif not cpu64bitalu}
         hregister : tregister;
        begin
          { set defaults }
          addconstant:=true;
          hregister:=NR_NO;
{$ifndef cpu64bitalu}
          hregisterhi:=NR_NO;
{$endif not cpu64bitalu}

          { first secondpass second argument, because if the first arg }
          { is used in that expression then SSL may move it to another }
          { register                                                   }
          if assigned(tcallparanode(left).right) then
            secondpass(tcallparanode(tcallparanode(left).right).left);
          { load first parameter, must be a reference }
          secondpass(tcallparanode(left).left);
          { get addvalue }
          case tcallparanode(left).left.resultdef.typ of
            orddef,
            enumdef :
                addvalue:=1;
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
          { second_ argument specified?, must be a s32bit in register }
          if assigned(tcallparanode(left).right) then
            begin
              { when constant, just multiply the addvalue }
              if is_constintnode(tcallparanode(tcallparanode(left).right).left) then
                 addvalue:=addvalue*get_ordinal_value(tcallparanode(tcallparanode(left).right).left)
              else if is_constpointernode(tcallparanode(tcallparanode(left).right).left) then
                 addvalue:=addvalue*tpointerconstnode(tcallparanode(tcallparanode(left).right).left).value
              else
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,tcallparanode(tcallparanode(left).right).left.location,tcallparanode(tcallparanode(left).right).left.resultdef,left.resultdef,addvalue<=1);
                  hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
{$ifndef cpu64bitalu}
                  hregisterhi:=tcallparanode(tcallparanode(left).right).left.location.register64.reghi;
{$endif not cpu64bitalu}
                  { insert multiply with addvalue if its >1 }
                  if addvalue>1 then
                    hlcg.a_op_const_reg(current_asmdata.CurrAsmList,OP_IMUL,left.resultdef,addvalue.svalue,hregister);
                  addconstant:=false;
                end;
            end;
          { write the add instruction }
          if addconstant then
            begin
{$ifndef cpu64bitalu}
              if def_cgsize(left.resultdef) in [OS_64,OS_S64] then
                cg64.a_op64_const_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],def_cgsize(left.resultdef),addvalue,tcallparanode(left).left.location)
              else
{$endif not cpu64bitalu}
                hlcg.a_op_const_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],left.resultdef,
{$ifdef cpu64bitalu}
                  aint(addvalue.svalue),
{$else cpu64bitalu}
                  longint(addvalue.svalue),  // can't use aint, because it breaks 16-bit and 8-bit CPUs
{$endif cpu64bitalu}
                  tcallparanode(left).left.location);
            end
           else
             begin
{$ifndef cpu64bitalu}
               if def_cgsize(left.resultdef) in [OS_64,OS_S64] then
                 cg64.a_op64_reg_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],def_cgsize(left.resultdef),
                   joinreg64(hregister,hregisterhi),tcallparanode(left).left.location)
               else
{$endif not cpu64bitalu}
                 hlcg.a_op_reg_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],left.resultdef,
                   hregister,tcallparanode(left).left.location);
             end;
          { no overflow checking for pointers (see ninl), and range checking }
          { is not applicable for them                                       }
          if (tcallparanode(left).left.resultdef.typ <> pointerdef) then
            begin
              { things which can overflow must NOT pass via here, but have to be  }
              { handled via a regular add node (conversion in tinlinenode.pass_1) }
              { Or someone has to rewrite the above to use a_op_const_reg_reg_ov  }
              { and friends in case of overflow checking, and ask everyone to     }
              { implement these methods since they don't exist for all cpus (JM)  }
              { Similarly, range checking also has to be handled separately, }
              { see mantis #14841 (JM)                                       }
              if ([cs_check_overflow,cs_check_range] * current_settings.localswitches <> []) then
                internalerror(2006111010);
//              cg.g_overflowcheck(current_asmdata.CurrAsmList,tcallparanode(left).left.location,tcallparanode(left).resultdef);
//              cg.g_rangecheck(current_asmdata.CurrAsmList,tcallparanode(left).left.location,tcallparanode(left).left.resultdef,
//                 tcallparanode(left).left.resultdef);
            end;
        end;


{*****************************************************************************
                         TYPEINFO GENERIC HANDLING
*****************************************************************************}
      procedure tcginlinenode.second_typeinfo;
        begin
          internalerror(2013060301);
        end;


{*****************************************************************************
                     INCLUDE/EXCLUDE GENERIC HANDLING
*****************************************************************************}

      procedure tcginlinenode.second_IncludeExclude;
        var
          setpara, elepara: tnode;
        begin
          { the set }
          secondpass(tcallparanode(left).left);
          { the element to set }
          secondpass(tcallparanode(tcallparanode(left).right).left);

          setpara:=tcallparanode(left).left;
          elepara:=tcallparanode(tcallparanode(left).right).left;

          if elepara.location.loc=LOC_CONSTANT then
            begin
              hlcg.a_bit_set_const_loc(current_asmdata.CurrAsmList,(inlinenumber=in_include_x_y),
                setpara.resultdef,elepara.location.value-tsetdef(setpara.resultdef).setbase,setpara.location);
            end
          else
            begin
              hlcg.location_force_reg(current_asmdata.CurrAsmList,elepara.location,elepara.resultdef,u32inttype,true);
              register_maybe_adjust_setbase(current_asmdata.CurrAsmList,elepara.location,tsetdef(setpara.resultdef).setbase);
              hlcg.a_bit_set_reg_loc(current_asmdata.CurrAsmList,(inlinenumber=in_include_x_y),
                u32inttype,setpara.resultdef,elepara.location.register,setpara.location);
            end;
        end;


{*****************************************************************************
                            FLOAT GENERIC HANDLING
*****************************************************************************}

{
  These routines all call internal RTL routines, so if they are
  called here, they give an internal error
}
    procedure tcginlinenode.second_pi;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_arctan_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_abs_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_round_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_trunc_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_sqr_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_sqrt_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_ln_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_cos_real;
      begin
        internalerror(20020718);
      end;

    procedure tcginlinenode.second_sin_real;
      begin
        internalerror(20020718);
      end;


    procedure tcginlinenode.second_prefetch;
      begin
      end;

    procedure tcginlinenode.second_abs_long;
      var
        tempreg1, tempreg2: tregister;
      begin
        secondpass(left);
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
        location:=left.location;
        location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,left.resultdef);

        tempreg1:=hlcg.getintregister(current_asmdata.CurrAsmList,left.resultdef);
        tempreg2:=hlcg.getintregister(current_asmdata.CurrAsmList,left.resultdef);
	
        hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SAR,left.resultdef,left.resultdef.size*8-1,left.location.register,tempreg1);
        hlcg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,OP_XOR,left.resultdef,left.location.register,tempreg1,tempreg2);
        hlcg.a_op_reg_reg_reg(current_asmdata.CurrAsmlist,OP_SUB,left.resultdef,tempreg1,tempreg2,location.register);
      end;


{*****************************************************************************
                         ASSIGNED GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_assigned;
      begin
        internalerror(2013091602);
      end;

    procedure Tcginlinenode.second_get_frame;

    begin
{$if defined(x86) or defined(arm)}
      if current_procinfo.framepointer=NR_STACK_POINTER_REG then
        begin
          location_reset(location,LOC_CONSTANT,OS_ADDR);
          location.value:=0;
        end
      else
{$endif defined(x86) or defined(arm)}
        begin
          location_reset(location,LOC_CREGISTER,OS_ADDR);
          location.register:=current_procinfo.framepointer;
        end;
    end;

    procedure Tcginlinenode.second_get_caller_frame;

    var
      frame_reg:Tregister;
      use_frame_pointer:boolean;

    begin
      frame_reg:=NR_NO;

      if left<>nil then
        begin
          secondpass(left);
          if left.location.loc=LOC_CONSTANT then
            use_frame_pointer:=true
          else
            begin
              hlcg.location_force_reg(current_asmdata.currasmlist,left.location,left.resultdef,voidpointertype,false);
              frame_reg:=left.location.register;
              use_frame_pointer:=false;
            end
        end
      else
        begin
          use_frame_pointer:=current_procinfo.framepointer=NR_STACK_POINTER_REG;
          frame_reg:=current_procinfo.framepointer;
        end;

      if use_frame_pointer then
        begin
          location_reset(location,LOC_CREGISTER,OS_ADDR);
          location.register:=NR_FRAME_POINTER_REG;
        end
      else
        begin
          location_reset_ref(location,LOC_REFERENCE,OS_ADDR,sizeof(pint));
          location.reference.base:=frame_reg;
        end;
    end;

    procedure Tcginlinenode.second_get_caller_addr;
      var
        frame_ref:Treference;
      begin
        if current_procinfo.framepointer=NR_STACK_POINTER_REG then
          begin
            location_reset(location,LOC_REGISTER,OS_ADDR);
            location.register:=cg.getaddressregister(current_asmdata.currasmlist);
            reference_reset_base(frame_ref,NR_STACK_POINTER_REG,{current_procinfo.calc_stackframe_size}tg.lasttemp,sizeof(pint));
            cg.a_load_ref_reg(current_asmdata.currasmlist,OS_ADDR,OS_ADDR,frame_ref,location.register);
          end
        else
          begin
            location_reset(location,LOC_REGISTER,OS_ADDR);
            location.register:=cg.getaddressregister(current_asmdata.currasmlist);
            reference_reset_base(frame_ref,current_procinfo.framepointer,sizeof(pint),sizeof(pint));
            cg.a_load_ref_reg(current_asmdata.currasmlist,OS_ADDR,OS_ADDR,frame_ref,location.register);
          end;
      end;


    procedure tcginlinenode.second_rox_sar;
      var
        op : topcg;
        op1,op2 : tnode;
      begin
        { one or two parameters? }
        if (left.nodetype=callparan) and
           assigned(tcallparanode(left).right) then
          begin
            op1:=tcallparanode(tcallparanode(left).right).left;
            op2:=tcallparanode(left).left;
            secondpass(op2);
          end
        else
          begin
            op1:=left;
            op2:=nil;
          end;

        secondpass(op1);
        case inlinenumber of
          in_ror_x,
          in_ror_x_y:
            op:=OP_ROR;
          in_rol_x,
          in_rol_x_y:
            op:=OP_ROL;
          in_sar_x,
          in_sar_x_y:
            op:=OP_SAR;
          else
            internalerror(2013120110);
        end;

        hlcg.location_force_reg(current_asmdata.CurrAsmList,op1.location,op1.resultdef,resultdef,true);

        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);

        if assigned(op2) then
          begin
             { rotating by a constant directly coded: }
             if op2.nodetype=ordconstn then
               hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,op,resultdef,
                 tordconstnode(op2).value.uvalue and (resultdef.size*8-1),
                 op1.location.register, location.register)
             else
               begin
                 hlcg.location_force_reg(current_asmdata.CurrAsmList,op2.location,
                                         op2.resultdef,resultdef,true);
                 hlcg.a_op_reg_reg_reg(current_asmdata.CurrAsmList,op,resultdef,
                                       op2.location.register,op1.location.register,
                                       location.register);
               end;
          end
        else
          hlcg.a_op_const_reg_reg(current_asmdata.CurrAsmList,op,resultdef,1,
                                  op1.location.register,location.register);
      end;


    procedure tcginlinenode.second_BsfBsr;
    var
      reverse: boolean;
      opsize: tcgsize;
    begin
      reverse:=(inlinenumber = in_bsr_x);
      secondpass(left);

      opsize:=tcgsize2unsigned[left.location.size];
      if opsize < OS_32 then
        opsize:=OS_32;

      if (left.location.loc <> LOC_REGISTER) or
         (left.location.size <> opsize) then
        hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,cgsize_orddef(opsize),true);

      location_reset(location,LOC_REGISTER,opsize);
      location.register := cg.getintregister(current_asmdata.CurrAsmList,opsize);
      cg.a_bit_scan_reg_reg(current_asmdata.CurrAsmList,reverse,opsize,left.location.register,location.register);
    end;


    procedure tcginlinenode.second_new;
      begin
        internalerror(2011012202);
      end;


    procedure tcginlinenode.second_popcnt;
      begin
        internalerror(2012082601);
      end;


begin
   cinlinenode:=tcginlinenode;
end.  s
