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
          procedure second_assert;virtual;
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
       end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,fmodule,
      symconst,symdef,defutil,symsym,
      aasmbase,aasmtai,aasmdata,aasmcpu,parabase,
      cgbase,pass_1,pass_2,
      cpuinfo,cpubase,paramgr,procinfo,
      nbas,ncon,ncal,ncnv,nld,ncgrtti,
      tgobj,ncgutil,
      cgutils,cgobj
{$ifndef cpu64bit}
      ,cg64f32
{$endif cpu64bit}
      ;


{*****************************************************************************
                              TCGINLINENODE
*****************************************************************************}


    procedure tcginlinenode.pass_generate_code;
      begin
         location_reset(location,LOC_VOID,OS_NO);

         case inlinenumber of
            in_assert_x_y:
              second_Assert;
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
{$ifdef SUPPORT_UNALIGNED}
            in_unaligned_x:
              begin
                secondpass(tcallparanode(left).left);
                location:=tcallparanode(left).left.location;
                if location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                  location.reference.alignment:=1;
              end;
{$endif SUPPORT_UNALIGNED}
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
            else internalerror(9);
         end;
      end;


{*****************************************************************************
                          ASSERT GENERIC HANDLING
*****************************************************************************}
    procedure tcginlinenode.second_Assert;
     var
       hp2,hp3 : tnode;
       otlabel,oflabel : tasmlabel;
       paraloc1,paraloc2,
       paraloc3,paraloc4 : tcgpara;
     begin
       { the node should be removed in the firstpass }
       if not (cs_do_assertion in current_settings.localswitches) then
          internalerror(7123458);
       paraloc1.init;
       paraloc2.init;
       paraloc3.init;
       paraloc4.init;
       paramanager.getintparaloc(pocall_default,1,paraloc1);
       paramanager.getintparaloc(pocall_default,2,paraloc2);
       paramanager.getintparaloc(pocall_default,3,paraloc3);
       paramanager.getintparaloc(pocall_default,4,paraloc4);
       otlabel:=current_procinfo.CurrTrueLabel;
       oflabel:=current_procinfo.CurrFalseLabel;
       current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
       current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
       secondpass(tcallparanode(left).left);
       maketojumpbool(current_asmdata.CurrAsmList,tcallparanode(left).left,lr_load_regvars);
       cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
       { First call secondpass() before we can push the parameters, otherwise
         parameters allocated in the registers can be destroyed }
       { generate filename string parameter }
       hp2:=ctypeconvnode.create(cstringconstnode.createstr(current_module.sourcefiles.get_file_name(current_filepos.fileindex)),cshortstringtype);
       firstpass(hp2);
       secondpass(hp2);
       if codegenerror then
          exit;
       { message parameter }
       hp3:=tcallparanode(tcallparanode(left).right).left;
       secondpass(hp3);
       if codegenerror then
          exit;
       { push erroraddr }
       paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc4);
       cg.a_param_reg(current_asmdata.CurrAsmList,OS_ADDR,NR_FRAME_POINTER_REG,paraloc4);
       { push lineno }
       paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc3);
       cg.a_param_const(current_asmdata.CurrAsmList,OS_INT,current_filepos.line,paraloc3);
       { push filename }
       paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc2);
       cg.a_paramaddr_ref(current_asmdata.CurrAsmList,hp2.location.reference,paraloc2);
       { push msg }
       paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
       cg.a_paramaddr_ref(current_asmdata.CurrAsmList,hp3.location.reference,paraloc1);
       { call }
       paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
       paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc2);
       paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc3);
       paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc4);
       cg.allocallcpuregisters(current_asmdata.CurrAsmList);
       cg.a_call_name(current_asmdata.CurrAsmList,'FPC_ASSERT');
       cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
       location_freetemp(current_asmdata.CurrAsmList,hp3.location);
       location_freetemp(current_asmdata.CurrAsmList,hp2.location);
       cg.a_label(current_asmdata.CurrAsmList,current_procinfo.CurrTrueLabel);
       current_procinfo.CurrTrueLabel:=otlabel;
       current_procinfo.CurrFalseLabel:=oflabel;
       paraloc1.done;
       paraloc2.done;
       paraloc3.done;
       paraloc4.done;
       hp2.free;
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
        if inlinenumber=in_sizeof_x then
          location_reset(location,LOC_REGISTER,OS_INT)
        else
          location_reset(location,LOC_REGISTER,OS_ADDR);
        { for both cases load vmt }
        if left.nodetype=typen then
          begin
            hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);
            reference_reset_symbol(href,current_asmdata.RefAsmSymbol(tobjectdef(left.resultdef).vmt_mangledname),0);
            cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,hregister);
          end
        else
          begin
            secondpass(left);
            hregister:=cg.getaddressregister(current_asmdata.CurrAsmList);

            { handle self inside a method of a class }
            case left.location.loc of
              LOC_CREGISTER,
              LOC_REGISTER :
                begin
                  if (left.resultdef.typ=classrefdef) or
                     (po_staticmethod in current_procinfo.procdef.procoptions) then
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.register,hregister)
                  else
                   begin
                     { load VMT pointer }
                     reference_reset_base(hrefvmt,left.location.register,tobjectdef(left.resultdef).vmt_offset);
                     cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,hrefvmt,hregister);
                   end
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  if is_class(left.resultdef) then
                   begin
                     { deref class }
                     cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,hregister);
                     cg.g_maybe_testself(current_asmdata.CurrAsmList,hregister);
                     { load VMT pointer }
                     reference_reset_base(hrefvmt,hregister,tobjectdef(left.resultdef).vmt_offset);
                     cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,hrefvmt,hregister);
                   end
                  else
                   begin
                     { load VMT pointer, but not for classrefdefs }
                     if (left.resultdef.typ=objectdef) then
                       inc(left.location.reference.offset,tobjectdef(left.resultdef).vmt_offset);
                     cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,hregister);
                   end;
                end;
              else
                internalerror(200301301);
            end;
          end;
        { in sizeof load size }
        if inlinenumber=in_sizeof_x then
           begin
             reference_reset_base(href,hregister,0);
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
           { length in ansi/wide strings is at offset -sizeof(aint) }
           location_force_reg(current_asmdata.CurrAsmList,left.location,OS_ADDR,false);
           current_asmdata.getjumplabel(lengthlab);
           cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_ADDR,OC_EQ,0,left.location.register,lengthlab);
           if is_widestring(left.resultdef) and (tf_winlikewidestring in target_info.flags) then
             begin
               reference_reset_base(href,left.location.register,-sizeof(dword));
               hregister:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_INT);
               cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_32,OS_INT,href,hregister);
             end
           else
             begin
               reference_reset_base(href,left.location.register,-sizeof(aint));
               hregister:=cg.makeregsize(current_asmdata.CurrAsmList,left.location.register,OS_INT);
               cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_INT,OS_INT,href,hregister);
             end;
           if is_widestring(left.resultdef) then
             cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHR,OS_INT,1,hregister);
           cg.a_label(current_asmdata.CurrAsmList,lengthlab);
           location_reset(location,LOC_REGISTER,OS_INT);
           location.register:=hregister;
         end;
      end;


{*****************************************************************************
                         PRED/SUCC GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_PredSucc;
      var
         cgsize : TCGSize;
         cgop : topcg;
      begin
        secondpass(left);
        if inlinenumber=in_pred_x then
           cgop:=OP_SUB
        else
           cgop:=OP_ADD;
        cgsize:=def_cgsize(resultdef);

        { we need a value in a register }
        location_copy(location,left.location);
        location_force_reg(current_asmdata.CurrAsmList,location,cgsize,false);

{$ifndef cpu64bit}
        if cgsize in [OS_64,OS_S64] then
          cg64.a_op64_const_reg(current_asmdata.CurrAsmList,cgop,cgsize,1,location.register64)
        else
{$endif cpu64bit}
          cg.a_op_const_reg(current_asmdata.CurrAsmList,cgop,location.size,1,location.register);

        cg.g_rangecheck(current_asmdata.CurrAsmList,location,resultdef,resultdef);
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
{$ifndef cpu64bit}
         hregisterhi,
{$endif cpu64bit}
         hregister : tregister;
         cgsize : tcgsize;
        begin
          { set defaults }
          addconstant:=true;
          { first secondpass second argument, because if the first arg }
          { is used in that expression then SSL may move it to another }
          { register                                                   }
          if assigned(tcallparanode(left).right) then
            secondpass(tcallparanode(tcallparanode(left).right).left);
          { load first parameter, must be a reference }
          secondpass(tcallparanode(left).left);
          cgsize:=def_cgsize(tcallparanode(left).left.resultdef);
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
              else
                begin
                  location_force_reg(current_asmdata.CurrAsmList,tcallparanode(tcallparanode(left).right).left.location,cgsize,addvalue<=1);
                  hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
{$ifndef cpu64bit}
                  hregisterhi:=tcallparanode(tcallparanode(left).right).left.location.register64.reghi;
{$endif cpu64bit}
                  { insert multiply with addvalue if its >1 }
                  if addvalue>1 then
                    cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_IMUL,cgsize,addvalue,hregister);
                  addconstant:=false;
                end;
            end;
          { write the add instruction }
          if addconstant then
            begin
{$ifndef cpu64bit}
              if cgsize in [OS_64,OS_S64] then
                cg64.a_op64_const_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],cgsize,addvalue,tcallparanode(left).left.location)
              else
{$endif cpu64bit}
                cg.a_op_const_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],
                  aint(addvalue),tcallparanode(left).left.location);
            end
           else
             begin
{$ifndef cpu64bit}
               if cgsize in [OS_64,OS_S64] then
                 cg64.a_op64_reg_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],cgsize,
                   joinreg64(hregister,hregisterhi),tcallparanode(left).left.location)
               else
{$endif cpu64bit}
                 cg.a_op_reg_loc(current_asmdata.CurrAsmList,addsubop[inlinenumber],
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
              if (cs_check_overflow in current_settings.localswitches) then
                internalerror(2006111010);
    //          cg.g_overflowcheck(current_asmdata.CurrAsmList,tcallparanode(left).left.location,tcallparanode(left).resultdef);
              cg.g_rangecheck(current_asmdata.CurrAsmList,tcallparanode(left).left.location,tcallparanode(left).left.resultdef,
                 tcallparanode(left).left.resultdef);
            end;
        end;


{*****************************************************************************
                         TYPEINFO GENERIC HANDLING
*****************************************************************************}
      procedure tcginlinenode.second_typeinfo;
        var
         href : treference;
        begin
          location_reset(location,LOC_REGISTER,OS_ADDR);
          location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
          reference_reset_symbol(href,RTTIWriter.get_rtti_label(left.resultdef,fullrtti),0);
          cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,location.register);
        end;


{*****************************************************************************
                     INCLUDE/EXCLUDE GENERIC HANDLING
*****************************************************************************}

      procedure tcginlinenode.second_IncludeExclude;
        var
          bitsperop,l : longint;
          opsize : tcgsize;
          cgop : topcg;
          addrreg2,addrreg,
          hregister,hregister2: tregister;
          use_small : boolean;
          href : treference;
        begin
          opsize:=OS_32;
          bitsperop:=(8*tcgsize2size[opsize]);
          secondpass(tcallparanode(left).left);
          if tcallparanode(tcallparanode(left).right).left.nodetype=ordconstn then
            begin
              { calculate bit position }
              l:=1 shl (tordconstnode(tcallparanode(tcallparanode(left).right).left).value mod bitsperop);

              { determine operator }
              if inlinenumber=in_include_x_y then
                cgop:=OP_OR
              else
                begin
                  cgop:=OP_AND;
                  l:=not(l);
                end;
              case tcallparanode(left).left.location.loc of
                LOC_REFERENCE :
                  begin
                    inc(tcallparanode(left).left.location.reference.offset,
                      (tordconstnode(tcallparanode(tcallparanode(left).right).left).value div bitsperop)*tcgsize2size[opsize]);
                    cg.a_op_const_ref(current_asmdata.CurrAsmList,cgop,opsize,l,tcallparanode(left).left.location.reference);
                  end;
                LOC_CREGISTER :
                  cg.a_op_const_reg(current_asmdata.CurrAsmList,cgop,tcallparanode(left).left.location.size,l,tcallparanode(left).left.location.register);
                else
                  internalerror(200405021);
              end;
            end
          else
            begin
              use_small:=
                 { set type }
                 (tsetdef(tcallparanode(left).left.resultdef).settype=smallset)
                  and
                   { elemenut number between 1 and 32 }
                  ((tcallparanode(tcallparanode(left).right).left.resultdef.typ=orddef) and
                   (torddef(tcallparanode(tcallparanode(left).right).left.resultdef).high<=32) or
                   (tcallparanode(tcallparanode(left).right).left.resultdef.typ=enumdef) and
                   (tenumdef(tcallparanode(tcallparanode(left).right).left.resultdef).max<=32));

              { generate code for the element to set }
              secondpass(tcallparanode(tcallparanode(left).right).left);

              { bitnumber - which must be loaded into register }
              hregister:=cg.getintregister(current_asmdata.CurrAsmList,opsize);
              hregister2:=cg.getintregister(current_asmdata.CurrAsmList,opsize);

              cg.a_load_loc_reg(current_asmdata.CurrAsmList,opsize,
                  tcallparanode(tcallparanode(left).right).left.location,hregister);

              if use_small then
                begin
                  { hregister contains the bitnumber to add }
                  cg.a_load_const_reg(current_asmdata.CurrAsmList, opsize, 1, hregister2);
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_SHL, opsize, hregister, hregister2);

                  { possiblities :
                       bitnumber : LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER
                       set value : LOC_REFERENCE, LOC_REGISTER
                  }
                  { location of set }
                  if inlinenumber=in_include_x_y then
                    begin
                      cg.a_op_reg_loc(current_asmdata.CurrAsmList, OP_OR, hregister2,
                      tcallparanode(left).left.location);
                    end
                  else
                    begin
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_NOT, opsize, hregister2,hregister2);
                      cg.a_op_reg_loc(current_asmdata.CurrAsmList, OP_AND, hregister2,
                          tcallparanode(left).left.location);
                    end;
                end
              else
                begin
                  { possiblities :
                       bitnumber : LOC_REFERENCE, LOC_REGISTER, LOC_CREGISTER
                       set value : LOC_REFERENCE
                  }
                  { hregister contains the bitnumber (div 32 to get the correct offset) }
                  { hregister contains the bitnumber to add }

                  cg.a_op_const_reg_reg(current_asmdata.CurrAsmList, OP_SHR, opsize, 5, hregister,hregister2);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_SHL, opsize, 2, hregister2);
                  addrreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
                  { we need an extra address register to be able to do an ADD operation }
                  addrreg2:=cg.getaddressregister(current_asmdata.CurrAsmList);
                  cg.a_load_reg_reg(current_asmdata.CurrAsmList,opsize,OS_ADDR,hregister2,addrreg2);
                  { calculate the correct address of the operand }
                  cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList, tcallparanode(left).left.location.reference,addrreg);
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_ADD, OS_ADDR, addrreg2, addrreg);

                  { hregister contains the bitnumber to add }
                  cg.a_load_const_reg(current_asmdata.CurrAsmList, opsize, 1, hregister2);
                  cg.a_op_const_reg(current_asmdata.CurrAsmList, OP_AND, opsize, 31, hregister);
                  cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_SHL, opsize, hregister, hregister2);

                  reference_reset_base(href,addrreg,0);

                  if inlinenumber=in_include_x_y then
                    cg.a_op_reg_ref(current_asmdata.CurrAsmList, OP_OR, opsize, hregister2, href)
                  else
                    begin
                      cg.a_op_reg_reg(current_asmdata.CurrAsmList, OP_NOT, opsize, hregister2, hregister2);
                      cg.a_op_reg_ref(current_asmdata.CurrAsmList, OP_AND, opsize, hregister2, href);
                    end;
                end;
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


{*****************************************************************************
                         ASSIGNED GENERIC HANDLING
*****************************************************************************}

    procedure tcginlinenode.second_assigned;
      begin
        secondpass(tcallparanode(left).left);
        { force left to be an OS_ADDR, since in case of method procvars }
        { the size is 2*OS_ADDR (JM)                                    }
        cg.a_cmp_const_loc_label(current_asmdata.CurrAsmList,OS_ADDR,OC_NE,0,tcallparanode(left).left.location,current_procinfo.CurrTrueLabel);
        cg.a_jmp_always(current_asmdata.CurrAsmList,current_procinfo.CurrFalseLabel);
        location_reset(location,LOC_JUMP,OS_NO);
      end;

    procedure Tcginlinenode.second_get_frame;

    begin
{$ifdef x86}
      if current_procinfo.framepointer=NR_STACK_POINTER_REG then
        begin
          location_reset(location,LOC_CONSTANT,OS_ADDR);
          location.value:=0;
        end
      else
{$endif x86}
        begin
          location_reset(location,LOC_CREGISTER,OS_ADDR);
          location.register:=current_procinfo.framepointer;
        end;
    end;

    procedure Tcginlinenode.second_get_caller_frame;

    var frame_ref:Treference;

    begin
      if current_procinfo.framepointer=NR_STACK_POINTER_REG then
        begin
          location_reset(location,LOC_CREGISTER,OS_ADDR);
          location.register:=NR_FRAME_POINTER_REG;
{          location_reset(location,LOC_REGISTER,OS_ADDR);
          location.register:=cg.getaddressregister(current_asmdata.currasmlist);
          cg.a_load_reg_reg(current_asmdata.currasmlist,OS_ADDR,OS_ADDR,NR_FRAME_POINTER_REG,location.register);}
        end
      else
        begin
          location_reset(location,LOC_REGISTER,OS_ADDR);
          location.register:=cg.getaddressregister(current_asmdata.currasmlist);
          reference_reset_base(frame_ref,current_procinfo.framepointer,0);
          cg.a_load_ref_reg(current_asmdata.currasmlist,OS_ADDR,OS_ADDR,frame_ref,location.register);
        end;
    end;

    procedure Tcginlinenode.second_get_caller_addr;

    var frame_ref:Treference;

    begin
      if current_procinfo.framepointer=NR_STACK_POINTER_REG then
        begin
          location_reset(location,LOC_REGISTER,OS_ADDR);
          location.register:=cg.getaddressregister(current_asmdata.currasmlist);
          reference_reset_base(frame_ref,NR_STACK_POINTER_REG,0);
          cg.a_load_ref_reg(current_asmdata.currasmlist,OS_ADDR,OS_ADDR,frame_ref,location.register);
        end
      else
        begin
          location_reset(location,LOC_REGISTER,OS_ADDR);
          location.register:=cg.getaddressregister(current_asmdata.currasmlist);
        {$ifdef cpu64bit}
          reference_reset_base(frame_ref,current_procinfo.framepointer,8);
        {$else}
          reference_reset_base(frame_ref,current_procinfo.framepointer,4);
        {$endif}
          cg.a_load_ref_reg(current_asmdata.currasmlist,OS_ADDR,OS_ADDR,frame_ref,location.register);
        end;
    end;

begin
   cinlinenode:=tcginlinenode;
end.
