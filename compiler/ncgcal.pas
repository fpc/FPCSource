 {
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for call nodes

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
unit ncgcal;

{$i fpcdefs.inc}

interface

    uses
      cpubase,
      globtype,
      parabase,cgutils,
      symdef,node,ncal;

    type
       tcgcallparanode = class(tcallparanode)
       private
          tempcgpara : tcgpara;
          procedure push_addr_para;
          procedure push_value_para;
       public
          constructor create(expr,next : tnode);override;
          destructor destroy;override;
          procedure secondcallparan;override;
       end;

       tcgcallnode = class(tcallnode)
       private
          procedure handle_return_value;
          procedure release_unused_return_value;
          procedure release_para_temps;
          procedure pushparas;
          procedure freeparas;
       protected
          framepointer_paraloc : tcgpara;
          {# This routine is used to push the current frame pointer
             on the stack. This is used in nested routines where the
             value of the frame pointer is always pushed as an extra
             parameter.

             The default handling is the standard handling used on
             most stack based machines, where the frame pointer is
             the first invisible parameter.
          }
          procedure pop_parasize(pop_size:longint);virtual;
          procedure extra_interrupt_code;virtual;
          procedure extra_call_code;virtual;
          procedure extra_post_call_code;virtual;
          procedure do_syscall;virtual;abstract;
       public
          procedure pass_generate_code;override;
       end;


implementation

    uses
      systems,
      cutils,verbose,globals,
      cpuinfo,
      symconst,symtable,defutil,paramgr,
      cgbase,pass_2,
      aasmbase,aasmtai,aasmdata,
      nbas,nmem,nld,ncnv,nutils,
{$ifdef x86}
      cga,cgx86,aasmcpu,
{$endif x86}
      ncgutil,
      cgobj,tgobj,
      procinfo;


{*****************************************************************************
                             TCGCALLPARANODE
*****************************************************************************}

    constructor tcgcallparanode.create(expr,next : tnode);
      begin
        inherited create(expr,next);
        tempcgpara.init;
      end;


    destructor tcgcallparanode.destroy;
      begin
        tempcgpara.done;
        inherited destroy;
      end;


    procedure tcgcallparanode.push_addr_para;
      begin
        if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
          internalerror(200304235);
        cg.a_paramaddr_ref(current_asmdata.CurrAsmList,left.location.reference,tempcgpara);
      end;


    procedure tcgcallparanode.push_value_para;
{$ifdef i386}
      var
        href   : treference;
        size   : longint;
{$endif i386}
      begin
        { we've nothing to push when the size of the parameter is 0 }
        if left.resultdef.size=0 then
          exit;

        { Move flags and jump in register to make it less complex }
        if left.location.loc in [LOC_FLAGS,LOC_JUMP,LOC_SUBSETREG,LOC_CSUBSETREG,LOC_SUBSETREF,LOC_CSUBSETREF] then
          location_force_reg(current_asmdata.CurrAsmList,left.location,def_cgsize(left.resultdef),false);

        { Handle Floating point types differently

          This doesn't depend on emulator settings, emulator settings should
          be handled by cpupara }

        if left.resultdef.typ=floatdef then
         begin
{$ifdef i386}
           if tempcgpara.location^.loc<>LOC_REFERENCE then
             internalerror(200309291);
           case left.location.loc of
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               begin
                 size:=align(TCGSize2Size[left.location.size],tempcgpara.alignment);
                 if tempcgpara.location^.reference.index=NR_STACK_POINTER_REG then
                   begin
                     cg.g_stackpointer_alloc(current_asmdata.CurrAsmList,size);
                     reference_reset_base(href,NR_STACK_POINTER_REG,0);
                   end
                 else
                   reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
                 cg.a_loadfpu_reg_ref(current_asmdata.CurrAsmList,left.location.size,left.location.size,left.location.register,href);
               end;
             LOC_MMREGISTER,
             LOC_CMMREGISTER:
               begin
                 size:=align(tfloatdef(left.resultdef).size,tempcgpara.alignment);
                 if tempcgpara.location^.reference.index=NR_STACK_POINTER_REG then
                   begin
                     cg.g_stackpointer_alloc(current_asmdata.CurrAsmList,size);
                     reference_reset_base(href,NR_STACK_POINTER_REG,0);
                   end
                 else
                   reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
                 cg.a_loadmm_reg_ref(current_asmdata.CurrAsmList,left.location.size,left.location.size,left.location.register,href,mms_movescalar);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 size:=align(left.resultdef.size,tempcgpara.alignment);
                 if (not use_fixed_stack) and
                    (tempcgpara.location^.reference.index=NR_STACK_POINTER_REG) then
                   cg.a_param_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara)
                 else
                   begin
                     reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
                     cg.g_concatcopy(current_asmdata.CurrAsmList,left.location.reference,href,size);
                   end;
               end;
             else
               internalerror(2002042430);
           end;
{$else i386}
           case left.location.loc of
             LOC_MMREGISTER,
             LOC_CMMREGISTER:
               case tempcgpara.location^.loc of
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   cg.a_parammm_reg(current_asmdata.CurrAsmList,left.location.size,left.location.register,tempcgpara,mms_movescalar);
{$ifdef x86_64}
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     current_asmdata.CurrAsmList.concat(taicpu.op_reg_reg(A_MOVD,S_NO,left.location.register,tempcgpara.location^.register));
                   end;
{$endif x86_64}
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   begin
                     location_force_fpureg(current_asmdata.CurrAsmList,left.location,false);
                     cg.a_paramfpu_reg(current_asmdata.CurrAsmList,left.location.size,left.location.register,tempcgpara);
                   end;
                 else
                   internalerror(200204249);
               end;
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               case tempcgpara.location^.loc of
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   begin
                     location_force_mmregscalar(current_asmdata.CurrAsmList,left.location,false);
                     cg.a_parammm_reg(current_asmdata.CurrAsmList,left.location.size,left.location.register,tempcgpara,mms_movescalar);
                   end;
{$ifdef cpu64bitalu}
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     location_force_mem(current_asmdata.CurrAsmList,left.location);
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     cg.a_param_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara);
                   end;
{$endif cpu64bitalu}
{$ifdef powerpc}
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     { aix abi passes floats of varargs in both fpu and }
                     { integer registers                                }
                     location_force_mem(current_asmdata.CurrAsmList,left.location);
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     if (left.location.size in [OS_32,OS_S32]) then
                       cg.a_param_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara)
                     else
                       cg64.a_param64_ref(current_asmdata.CurrAsmList,left.location.reference,tempcgpara);
                   end;
{$endif powerpc}
{$if defined(sparc) or defined(arm) or defined(m68k)}
                 { sparc and arm pass floats in normal registers }
                 LOC_REGISTER,
                 LOC_CREGISTER,
{$endif sparc}
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   cg.a_paramfpu_reg(current_asmdata.CurrAsmList,left.location.size,left.location.register,tempcgpara);
                 else
                   internalerror(2002042433);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE:
               case tempcgpara.location^.loc of
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   cg.a_parammm_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara,mms_movescalar);
{$ifdef cpu64bitalu}
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     cg.a_param_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara);
                   end;
{$endif cpu64bitalu}
{$ifdef powerpc}
                 { x86_64 pushes s64comp in normal register }
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     if (left.location.size in [OS_32,OS_S32]) then
                       cg.a_param_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara)
                     else
                       cg64.a_param64_ref(current_asmdata.CurrAsmList,left.location.reference,tempcgpara);
                   end;
{$endif powerpc}
{$if defined(sparc) or defined(arm) or defined(m68k)}
                 { sparc and arm pass floats in normal registers }
                 LOC_REGISTER,
                 LOC_CREGISTER,
{$endif}
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   cg.a_paramfpu_ref(current_asmdata.CurrAsmList,left.location.size,left.location.reference,tempcgpara);
                 else
                   internalerror(2002042431);
               end;
             LOC_REGISTER,
             LOC_CREGISTER :
               begin
{$ifndef cpu64bitalu}
                 { use cg64 only for int64, not for 8 byte records }
                 if is_64bit(left.resultdef) then
                   cg64.a_param64_loc(current_asmdata.CurrAsmList,left.location,tempcgpara)
                 else
{$endif not cpu64bitalu}
                   begin
{$ifndef cpu64bitalu}
                     { Only a_param_ref supports multiple locations, when the
                       value is still a const or in a register then write it
                       to a reference first. This situation can be triggered
                       by typecasting an int64 constant to a record of 8 bytes }
                     if left.location.size in [OS_64,OS_S64] then
                       location_force_mem(current_asmdata.CurrAsmList,left.location);
{$endif not cpu64bitalu}
                     cg.a_param_loc(current_asmdata.CurrAsmList,left.location,tempcgpara);
                   end;
               end;
             else
               internalerror(2002042432);
           end;
{$endif i386}
         end
        else
         begin
           case left.location.loc of
             LOC_CONSTANT,
             LOC_REGISTER,
             LOC_CREGISTER,
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
{$ifndef cpu64bitalu}
                 { use cg64 only for int64, not for 8 byte records }
                 if is_64bit(left.resultdef) then
                   cg64.a_param64_loc(current_asmdata.CurrAsmList,left.location,tempcgpara)
                 else
{$endif not cpu64bitalu}
                   begin
{$ifndef cpu64bitalu}
                     { Only a_param_ref supports multiple locations, when the
                       value is still a const or in a register then write it
                       to a reference first. This situation can be triggered
                       by typecasting an int64 constant to a record of 8 bytes }
                     if left.location.size in [OS_64,OS_S64] then
                       location_force_mem(current_asmdata.CurrAsmList,left.location);
{$endif not cpu64bitalu}
                     cg.a_param_loc(current_asmdata.CurrAsmList,left.location,tempcgpara);
                   end;
               end;
{$ifdef SUPPORT_MMX}
             LOC_MMXREGISTER,
             LOC_CMMXREGISTER:
               cg.a_parammm_reg(current_asmdata.CurrAsmList,OS_M64,left.location.register,tempcgpara,nil);
{$endif SUPPORT_MMX}
             else
               internalerror(200204241);
           end;
         end;
      end;


    procedure tcgcallparanode.secondcallparan;
      var
         href    : treference;
         otlabel,
         oflabel : tasmlabel;
      begin
         if not(assigned(parasym)) then
           internalerror(200304242);

         { Skip nothingn nodes which are used after disabling
           a parameter }
         if (left.nodetype<>nothingn) then
           begin
             otlabel:=current_procinfo.CurrTrueLabel;
             oflabel:=current_procinfo.CurrFalseLabel;
             current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
             current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
             secondpass(left);

             maybechangeloadnodereg(current_asmdata.CurrAsmList,left,true);

             { release memory for refcnt out parameters }
             if (parasym.varspez=vs_out) and
                (left.resultdef.needs_inittable) then
               begin
                 location_get_data_ref(current_asmdata.CurrAsmList,left.location,href,false);
                 cg.g_decrrefcount(current_asmdata.CurrAsmList,left.resultdef,href);
               end;

             paramanager.createtempparaloc(current_asmdata.CurrAsmList,aktcallnode.procdefinition.proccalloption,parasym,tempcgpara);

             { handle varargs first, because parasym is not valid }
             if (cpf_varargs_para in callparaflags) then
               begin
                 if paramanager.push_addr_param(vs_value,left.resultdef,
                        aktcallnode.procdefinition.proccalloption) then
                   push_addr_para
                 else
                   push_value_para;
               end
             { hidden parameters }
             else if (vo_is_hidden_para in parasym.varoptions) then
               begin
                 { don't push a node that already generated a pointer type
                   by address for implicit hidden parameters }
                 if (vo_is_funcret in parasym.varoptions) or
                   { pass "this" in C++ classes explicitly as pointer
                     because push_addr_param might not be true for them }
                   (is_cppclass(parasym.vardef) and (vo_is_self in parasym.varoptions)) or
                    (not(left.resultdef.typ in [pointerdef,classrefdef]) and
                     paramanager.push_addr_param(parasym.varspez,parasym.vardef,
                         aktcallnode.procdefinition.proccalloption)) then
                   push_addr_para
                 else
                   push_value_para;
               end
             { formal def }
             else if (parasym.vardef.typ=formaldef) then
               begin
                  { allow passing of a constant to a const formaldef }
                  if (parasym.varspez=vs_const) and
                     (left.location.loc in [LOC_CONSTANT,LOC_REGISTER]) then
                    location_force_mem(current_asmdata.CurrAsmList,left.location);
                  push_addr_para;
               end
             { Normal parameter }
             else
               begin
                 { don't push a node that already generated a pointer type
                   by address for implicit hidden parameters }
                 if (not(
                         (vo_is_hidden_para in parasym.varoptions) and
                         (left.resultdef.typ in [pointerdef,classrefdef])
                        ) and
                     paramanager.push_addr_param(parasym.varspez,parasym.vardef,
                         aktcallnode.procdefinition.proccalloption)) and
                     { dyn. arrays passed to an array of const must be passed by value, see tests/webtbs/tw4219.pp }
                     not(
                         is_array_of_const(parasym.vardef) and
                         is_dynamic_array(left.resultdef)
                        ) then
                   begin
                      { Passing a var parameter to a var parameter, we can
                        just push the address transparently }
                      if (left.nodetype=loadn) and
                         (tloadnode(left).is_addr_param_load) then
                        begin
                          if (left.location.reference.index<>NR_NO) or
                             (left.location.reference.offset<>0) then
                            internalerror(200410107);
                          cg.a_param_reg(current_asmdata.CurrAsmList,OS_ADDR,left.location.reference.base,tempcgpara)
                        end
                      else
                        begin
                          { Force to be in memory }
                          if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                            location_force_mem(current_asmdata.CurrAsmList,left.location);
                          push_addr_para;
                        end;
                   end
                 else
                   push_value_para;
               end;
             current_procinfo.CurrTrueLabel:=otlabel;
             current_procinfo.CurrFalseLabel:=oflabel;

             { update return location in callnode when this is the function
               result }
             if assigned(parasym) and
                (vo_is_funcret in parasym.varoptions) then
               location_copy(aktcallnode.location,left.location);
           end;

         { next parameter }
         if assigned(right) then
           tcallparanode(right).secondcallparan;
      end;


{*****************************************************************************
                             TCGCALLNODE
*****************************************************************************}

    procedure tcgcallnode.extra_interrupt_code;
      begin
      end;


    procedure tcgcallnode.extra_call_code;
      begin
      end;


    procedure tcgcallnode.extra_post_call_code;
      begin
      end;


    procedure tcgcallnode.pop_parasize(pop_size:longint);
      begin
      end;


    procedure tcgcallnode.handle_return_value;
      var
        tmpcgsize,
        cgsize    : tcgsize;
        retloc    : tlocation;
{$ifdef cpu64bitaddr}
        ref       : treference;
{$endif cpu64bitaddr}
{$ifndef x86}
        hregister : tregister;
{$endif not x86}
      begin
        { Check that the return location is set when the result is passed in
          a parameter }
        if (procdefinition.proctypeoption<>potype_constructor) and
           paramanager.ret_in_param(resultdef,procdefinition.proccalloption) then
          begin
            if location.loc<>LOC_REFERENCE then
              internalerror(200304241);
            exit;
          end;

        { Load normal (ordinal,float,pointer) result value from accumulator }
        cgsize:=procdefinition.funcretloc[callerside].size;
        case procdefinition.funcretloc[callerside].loc of
           LOC_FPUREGISTER:
             begin
               location_reset(location,LOC_FPUREGISTER,cgsize);
               location.register:=procdefinition.funcretloc[callerside].register;
{$ifdef x86}
               tcgx86(cg).inc_fpu_stack;
{$else x86}
               if getsupreg(procdefinition.funcretloc[callerside].register)<first_fpu_imreg then
                 cg.ungetcpuregister(current_asmdata.CurrAsmList,procdefinition.funcretloc[callerside].register);
               hregister:=cg.getfpuregister(current_asmdata.CurrAsmList,location.size);
               cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,location.size,location.size,location.register,hregister);
               location.register:=hregister;
{$endif x86}
             end;

           LOC_REGISTER:
             begin
               if cgsize<>OS_NO then
                begin
                  location_reset(location,LOC_REGISTER,cgsize);
{$ifdef cpu64bitaddr}
                  { x86-64 system v abi:
                    structs with up to 16 bytes are returned in registers }
                  if cgsize in [OS_128,OS_S128] then
                    begin
                      tg.GetTemp(current_asmdata.CurrAsmList,16,tt_normal,ref);
                      location_reset(location,LOC_REFERENCE,OS_NO);
                      location.reference:=ref;
                      cg.a_load_reg_ref(current_asmdata.CurrAsmList,OS_64,OS_64,procdefinition.funcretloc[callerside].register,ref);
                      inc(ref.offset,8);
                      cg.a_load_reg_ref(current_asmdata.CurrAsmList,OS_64,OS_64,procdefinition.funcretloc[callerside].registerhi,ref);
                    end
                  else
{$else cpu64bitaddr}
                  if cgsize in [OS_64,OS_S64] then
                    begin
                      retloc:=procdefinition.funcretloc[callerside];
                      if retloc.loc<>LOC_REGISTER then
                        internalerror(200409141);
                      { the function result registers are already allocated }
                      if getsupreg(retloc.register64.reglo)<first_int_imreg then
                        cg.ungetcpuregister(current_asmdata.CurrAsmList,retloc.register64.reglo);
                      location.register64.reglo:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,retloc.register64.reglo,location.register64.reglo);
                      if getsupreg(retloc.register64.reghi)<first_int_imreg then
                        cg.ungetcpuregister(current_asmdata.CurrAsmList,retloc.register64.reghi);
                      location.register64.reghi:=cg.getintregister(current_asmdata.CurrAsmList,OS_32);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_32,OS_32,retloc.register64.reghi,location.register64.reghi);
                    end
                  else
{$endif not cpu64bitaddr}
                    begin
                      { change register size after the unget because the
                        getregister was done for the full register
                        def_cgsize(resultdef) is used here because
                        it could be a constructor call }

                      if getsupreg(procdefinition.funcretloc[callerside].register)<first_int_imreg then
                        cg.ungetcpuregister(current_asmdata.CurrAsmList,procdefinition.funcretloc[callerside].register);

                      { but use def_size only if it returns something valid because in
                        case of odd sized structured results in registers def_cgsize(resultdef)
                        could return OS_NO }
                      if def_cgsize(resultdef)<>OS_NO then
                        tmpcgsize:=def_cgsize(resultdef)
                      else
                        tmpcgsize:=cgsize;

                      location.register:=cg.getintregister(current_asmdata.CurrAsmList,tmpcgsize);
                      cg.a_load_reg_reg(current_asmdata.CurrAsmList,cgsize,tmpcgsize,procdefinition.funcretloc[callerside].register,location.register);
                    end;
{$ifdef arm}
                  if (resultdef.typ=floatdef) and (current_settings.fputype in [fpu_fpa,fpu_fpa10,fpu_fpa11]) then
                    begin
                      location_force_mem(current_asmdata.CurrAsmList,location);
                    end;
{$endif arm}
                end
               else
                begin
                  if resultdef.size>0 then
                    internalerror(200305131);
                end;
             end;

           LOC_MMREGISTER:
             begin
               location_reset(location,LOC_MMREGISTER,cgsize);
               if getsupreg(procdefinition.funcretloc[callerside].register)<first_mm_imreg then
                 cg.ungetcpuregister(current_asmdata.CurrAsmList,procdefinition.funcretloc[callerside].register);
               location.register:=cg.getmmregister(current_asmdata.CurrAsmList,cgsize);
               cg.a_loadmm_reg_reg(current_asmdata.CurrAsmList,cgsize,cgsize,procdefinition.funcretloc[callerside].register,location.register,mms_movescalar);
             end;

           else
             internalerror(200405023);
        end;

        { copy value to the final location if this was already provided to the
          callnode. This must be done after the call node, because the location can
          also be used as parameter and may not be finalized yet }
        if assigned(funcretnode) then
          begin
            funcretnode.pass_generate_code;
            { Decrease refcount for refcounted types, this can be skipped when
              we have used a temp, because then it is already done from tempcreatenode.
              Also no finalize is needed, because there is no risk of exceptions from the
              function since this is code is only executed after the function call has returned }
            if funcretnode.resultdef.needs_inittable and
               (funcretnode.nodetype<>temprefn) then
              cg.g_decrrefcount(current_asmdata.CurrAsmList,funcretnode.resultdef,funcretnode.location.reference);

            case location.loc of
              LOC_REGISTER :
{$ifndef cpu64bitalu}
                if cgsize in [OS_64,OS_S64] then
                  cg64.a_load64_reg_loc(current_asmdata.CurrAsmList,location.register64,funcretnode.location)
                else
{$endif}
                  cg.a_load_reg_loc(current_asmdata.CurrAsmList,cgsize,location.register,funcretnode.location);
              LOC_REFERENCE:
                begin
                  case funcretnode.location.loc of
                    LOC_REGISTER:
                      cg.a_load_ref_reg(current_asmdata.CurrAsmList,cgsize,cgsize,location.reference,funcretnode.location.register);
                    LOC_REFERENCE:
                      cg.g_concatcopy(current_asmdata.CurrAsmList,location.reference,funcretnode.location.reference,resultdef.size);
                    else
                      internalerror(200802121);
                  end;
                end;
              else
                internalerror(200709085);
            end;
            location := funcretnode.location;
          end;
      end;


    procedure tcgcallnode.release_unused_return_value;
      begin
        { When the result is not used we need to finalize the result and
          can release the temp. This need to be after the callcleanupblock
          tree is generated, because that converts the temp from persistent to normal }
        if not(cnf_return_value_used in callnodeflags) then
          begin
           case location.loc of
             LOC_REFERENCE :
               begin
                 if resultdef.needs_inittable then
                    cg.g_finalize(current_asmdata.CurrAsmList,resultdef,location.reference);
                  tg.ungetiftemp(current_asmdata.CurrAsmList,location.reference);
               end;
{$ifdef x86}
             LOC_FPUREGISTER :
                begin
                  { release FPU stack }
                  emit_reg(A_FSTP,S_NO,NR_FPU_RESULT_REG);
                  tcgx86(cg).dec_fpu_stack;
                end;
{$endif x86}
           end;
            if procdefinition.funcretloc[callerside].size<>OS_NO then
              location_free(current_asmdata.CurrAsmList,procdefinition.funcretloc[callerside]);
            location_reset(location,LOC_VOID,OS_NO);
          end;
      end;


    procedure tcgcallnode.release_para_temps;
      var
        hp,
        hp2 : tnode;
        ppn : tcallparanode;
      begin
        { Release temps from parameters }
        ppn:=tcallparanode(left);
        while assigned(ppn) do
          begin
             if assigned(ppn.left) then
               begin
                 { don't release the funcret temp }
                 if not(assigned(ppn.parasym)) or
                    not(vo_is_funcret in ppn.parasym.varoptions) then
                   location_freetemp(current_asmdata.CurrAsmList,ppn.left.location);
                 { process also all nodes of an array of const }
                 hp:=ppn.left;
                 while (hp.nodetype=typeconvn) do
                   hp:=ttypeconvnode(hp).left;
                 if (hp.nodetype=arrayconstructorn) and
                    assigned(tarrayconstructornode(hp).left) then
                   begin
                     while assigned(hp) do
                       begin
                         hp2:=tarrayconstructornode(hp).left;
                         { ignore typeconvs and addrn inserted by arrayconstructn for
                           passing a shortstring }
                         if (hp2.nodetype=typeconvn) and
                            (tunarynode(hp2).left.nodetype=addrn) then
                           hp2:=tunarynode(tunarynode(hp2).left).left;
                         location_freetemp(current_asmdata.CurrAsmList,hp2.location);
                         hp:=tarrayconstructornode(hp).right;
                       end;
                   end;
               end;
             ppn:=tcallparanode(ppn.right);
          end;
      end;


     procedure tcgcallnode.pushparas;
       var
         ppn : tcgcallparanode;
         callerparaloc,
         tmpparaloc : pcgparalocation;
         sizeleft: aint;
         htempref,
         href : treference;
       begin
         { copy all resources to the allocated registers }
         ppn:=tcgcallparanode(left);
         while assigned(ppn) do
           begin
             if (ppn.left.nodetype<>nothingn) then
               begin
                 { better check for the real location of the parameter here, when stack passed parameters
                   are saved temporary in registers, checking for the tmpparaloc.loc is wrong
                 }
                 paramanager.freeparaloc(current_asmdata.CurrAsmList,ppn.tempcgpara);
                 tmpparaloc:=ppn.tempcgpara.location;
                 sizeleft:=ppn.tempcgpara.intsize;
                 callerparaloc:=ppn.parasym.paraloc[callerside].location;
                 while assigned(callerparaloc) do
                   begin
                     { Every paraloc must have a matching tmpparaloc }
                     if not assigned(tmpparaloc) then
                       internalerror(200408224);
                     if callerparaloc^.size<>tmpparaloc^.size then
                       internalerror(200408225);
                     case callerparaloc^.loc of
                       LOC_REGISTER:
                         begin
                           if tmpparaloc^.loc<>LOC_REGISTER then
                             internalerror(200408221);
                           if getsupreg(callerparaloc^.register)<first_int_imreg then
                             cg.getcpuregister(current_asmdata.CurrAsmList,callerparaloc^.register);
                           cg.a_load_reg_reg(current_asmdata.CurrAsmList,tmpparaloc^.size,tmpparaloc^.size,
                               tmpparaloc^.register,callerparaloc^.register);
                         end;
                       LOC_FPUREGISTER:
                         begin
                           if tmpparaloc^.loc<>LOC_FPUREGISTER then
                             internalerror(200408222);
                           if getsupreg(callerparaloc^.register)<first_fpu_imreg then
                             cg.getcpuregister(current_asmdata.CurrAsmList,callerparaloc^.register);
                           cg.a_loadfpu_reg_reg(current_asmdata.CurrAsmList,tmpparaloc^.size,ppn.tempcgpara.size,tmpparaloc^.register,callerparaloc^.register);
                         end;
                       LOC_MMREGISTER:
                         begin
                           if tmpparaloc^.loc<>LOC_MMREGISTER then
                             internalerror(200408223);
                           if getsupreg(callerparaloc^.register)<first_mm_imreg then
                             cg.getcpuregister(current_asmdata.CurrAsmList,callerparaloc^.register);
                           cg.a_loadmm_reg_reg(current_asmdata.CurrAsmList,tmpparaloc^.size,tmpparaloc^.size,
                             tmpparaloc^.register,callerparaloc^.register,mms_movescalar);
                         end;
                       LOC_REFERENCE:
                         begin
                           if use_fixed_stack then
                             begin
                               { Can't have a data copied to the stack, every location
                                 must contain a valid size field }

                              if (ppn.tempcgpara.size=OS_NO) and
                                 ((tmpparaloc^.loc<>LOC_REFERENCE) or
                                  assigned(tmpparaloc^.next)) then
                                internalerror(200501281);
                                reference_reset_base(href,callerparaloc^.reference.index,callerparaloc^.reference.offset);
                              { copy parameters in case they were moved to a temp. location because we've a fixed stack }
                              case tmpparaloc^.loc of
                              LOC_REFERENCE:
                                  begin
                                    reference_reset_base(htempref,tmpparaloc^.reference.index,tmpparaloc^.reference.offset);
                                    { use concatcopy, because it can also be a float which fails when
                                      load_ref_ref is used }
                                    if (ppn.tempcgpara.size <> OS_NO) then
                                      cg.g_concatcopy(current_asmdata.CurrAsmList,htempref,href,tcgsize2size[tmpparaloc^.size])
                                    else
                                      cg.g_concatcopy(current_asmdata.CurrAsmList,htempref,href,sizeleft)
                                  end;
                                LOC_REGISTER:
                                  cg.a_load_reg_ref(current_asmdata.CurrAsmList,tmpparaloc^.size,tmpparaloc^.size,tmpparaloc^.register,href);
                                LOC_FPUREGISTER:
                                  cg.a_loadfpu_reg_ref(current_asmdata.CurrAsmList,tmpparaloc^.size,tmpparaloc^.size,tmpparaloc^.register,href);
                                LOC_MMREGISTER:
                                  cg.a_loadmm_reg_ref(current_asmdata.CurrAsmList,tmpparaloc^.size,tmpparaloc^.size,tmpparaloc^.register,href,mms_movescalar);
                                else
                                  internalerror(200402081);
                             end;
                           end;
                         end;
                     end;
                     dec(sizeleft,tcgsize2size[tmpparaloc^.size]);
                     callerparaloc:=callerparaloc^.next;
                     tmpparaloc:=tmpparaloc^.next;
                   end;
               end;
             ppn:=tcgcallparanode(ppn.right);
           end;
       end;


     procedure tcgcallnode.freeparas;
       var
         ppn : tcgcallparanode;
       begin
         { free the resources allocated for the parameters }
         ppn:=tcgcallparanode(left);
         while assigned(ppn) do
           begin
             if (ppn.left.nodetype<>nothingn) then
               begin
                 if (ppn.parasym.paraloc[callerside].location^.loc <> LOC_REFERENCE) then
                   paramanager.freeparaloc(current_asmdata.CurrAsmList,ppn.parasym.paraloc[callerside]);
               end;
             ppn:=tcgcallparanode(ppn.right);
           end;
       end;



    procedure tcgcallnode.pass_generate_code;
      var
        regs_to_save_int,
        regs_to_save_fpu,
        regs_to_save_mm   : Tcpuregisterset;
        href : treference;
        pop_size : longint;
        vmtoffset : aint;
        pvreg,
        vmtreg : tregister;
        oldaktcallnode : tcallnode;
{$ifdef vtentry}
        sym : tasmsymbol;
{$endif vtentry}
{$ifdef x86_64}
        cgpara : tcgpara;
{$endif x86_64}
      begin
         if not assigned(procdefinition) or
            not procdefinition.has_paraloc_info then
           internalerror(200305264);

         if assigned(callinitblock) then
           secondpass(callinitblock);

         regs_to_save_int:=paramanager.get_volatile_registers_int(procdefinition.proccalloption);
         regs_to_save_fpu:=paramanager.get_volatile_registers_fpu(procdefinition.proccalloption);
         regs_to_save_mm:=paramanager.get_volatile_registers_mm(procdefinition.proccalloption);

         { Include Function result registers }
         if (not is_void(resultdef)) then
          begin
            case procdefinition.funcretloc[callerside].loc of
              LOC_REGISTER,
              LOC_CREGISTER:
                include(regs_to_save_int,getsupreg(procdefinition.funcretloc[callerside].register));
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER:
                include(regs_to_save_fpu,getsupreg(procdefinition.funcretloc[callerside].register));
              LOC_MMREGISTER,
              LOC_CMMREGISTER:
                include(regs_to_save_mm,getsupreg(procdefinition.funcretloc[callerside].register));
              LOC_REFERENCE,
              LOC_VOID:
                ;
              else
                internalerror(2004110213);
              end;
          end;

         { Process parameters, register parameters will be loaded
           in imaginary registers. The actual load to the correct
           register is done just before the call }
         oldaktcallnode:=aktcallnode;
         aktcallnode:=self;
         if assigned(left) then
           tcallparanode(left).secondcallparan;
         aktcallnode:=oldaktcallnode;

         { procedure variable or normal function call ? }
         if (right=nil) then
           begin
             { When methodpointer is typen we don't need (and can't) load
               a pointer. We can directly call the correct procdef (PFV) }
             if (po_virtualmethod in procdefinition.procoptions) and
                assigned(methodpointer) and
                (methodpointer.nodetype<>typen) then
               begin
                 { virtual methods require an index }
                 if tprocdef(procdefinition).extnumber=$ffff then
                   internalerror(200304021);

                 secondpass(methodpointer);

                 { Load VMT from self }
                 if methodpointer.resultdef.typ=objectdef then
                   gen_load_vmt_register(current_asmdata.CurrAsmList,tobjectdef(methodpointer.resultdef),methodpointer.location,vmtreg)
                 else
                   begin
                     { Load VMT value in register }
                     location_force_reg(current_asmdata.CurrAsmList,methodpointer.location,OS_ADDR,false);
                     vmtreg:=methodpointer.location.register;
                   end;

                 { test validity of VMT }
                 if not(is_interface(tprocdef(procdefinition)._class)) and
                    not(is_cppclass(tprocdef(procdefinition)._class)) then
                   cg.g_maybe_testvmt(current_asmdata.CurrAsmList,vmtreg,tprocdef(procdefinition)._class);

                 { Call through VMT, generate a VTREF symbol to notify the linker }
                 vmtoffset:=tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber);
{$ifdef vtentry}
                 if not is_interface(tprocdef(procdefinition)._class) then
                   begin
                     inc(current_asmdata.NextVTEntryNr);
                     current_asmdata.CurrAsmList.Concat(tai_symbol.CreateName('VTREF'+tostr(current_asmdata.NextVTEntryNr)+'_'+tprocdef(procdefinition)._class.vmt_mangledname+'$$'+tostr(vmtoffset div sizeof(pint)),AT_FUNCTION,0));
                   end;
{$endif vtentry}

{$ifndef x86}
                 pvreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_ADDR);
{$endif not x86}
                 reference_reset_base(href,vmtreg,vmtoffset);
{$ifndef x86}
                 cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,pvreg);
{$endif not x86}

                 { Load parameters that are in temporary registers in the
                   correct parameter register }
                 if assigned(left) then
                   begin
                     pushparas;
                     { free the resources allocated for the parameters }
                     freeparas;
                   end;

                 cg.alloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,regs_to_save_int);
                 if cg.uses_registers(R_FPUREGISTER) then
                   cg.alloccpuregisters(current_asmdata.CurrAsmList,R_FPUREGISTER,regs_to_save_fpu);
                 if cg.uses_registers(R_MMREGISTER) then
                   cg.alloccpuregisters(current_asmdata.CurrAsmList,R_MMREGISTER,regs_to_save_mm);

                 { call method }
                 extra_call_code;
{$ifdef x86}
                 cg.a_call_ref(current_asmdata.CurrAsmList,href);
{$else x86}
                 cg.a_call_reg(current_asmdata.CurrAsmList,pvreg);
{$endif x86}
                 extra_post_call_code;
               end
             else
               begin
                  { Load parameters that are in temporary registers in the
                    correct parameter register }
                  if assigned(left) then
                    begin
                      pushparas;
                      { free the resources allocated for the parameters }
                      freeparas;
                    end;

                  cg.alloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,regs_to_save_int);
                  if cg.uses_registers(R_FPUREGISTER) then
                    cg.alloccpuregisters(current_asmdata.CurrAsmList,R_FPUREGISTER,regs_to_save_fpu);
                  if cg.uses_registers(R_MMREGISTER) then
                    cg.alloccpuregisters(current_asmdata.CurrAsmList,R_MMREGISTER,regs_to_save_mm);

                  if procdefinition.proccalloption=pocall_syscall then
                    do_syscall
                  else
                    begin
                      { Calling interrupt from the same code requires some
                        extra code }
                      if (po_interrupt in procdefinition.procoptions) then
                        extra_interrupt_code;
                      extra_call_code;
                      cg.a_call_name(current_asmdata.CurrAsmList,tprocdef(procdefinition).mangledname);
                      extra_post_call_code;
                    end;
               end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(right);

              pvreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_ADDR);
              { Only load OS_ADDR from the reference }
              if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,right.location.reference,pvreg)
              else
                cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_ADDR,right.location,pvreg);
              location_freetemp(current_asmdata.CurrAsmList,right.location);

              { Load parameters that are in temporary registers in the
                correct parameter register }
              if assigned(left) then
                begin
                  pushparas;
                  { free the resources allocated for the parameters }
                  freeparas;
                end;

              cg.alloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,regs_to_save_int);
              if cg.uses_registers(R_FPUREGISTER) then
                cg.alloccpuregisters(current_asmdata.CurrAsmList,R_FPUREGISTER,regs_to_save_fpu);
              if cg.uses_registers(R_MMREGISTER) then
                cg.alloccpuregisters(current_asmdata.CurrAsmList,R_MMREGISTER,regs_to_save_mm);

              { Calling interrupt from the same code requires some
                extra code }
              if (po_interrupt in procdefinition.procoptions) then
                extra_interrupt_code;
              extra_call_code;
              cg.a_call_reg(current_asmdata.CurrAsmList,pvreg);
              extra_post_call_code;
           end;

         { Need to remove the parameters from the stack? }
         if (procdefinition.proccalloption in clearstack_pocalls) then
          begin
            pop_size:=pushedparasize;
            { for Cdecl functions we don't need to pop the funcret when it
              was pushed by para }
            if paramanager.ret_in_param(procdefinition.returndef,procdefinition.proccalloption) then
              dec(pop_size,sizeof(pint));
            { Remove parameters/alignment from the stack }
            pop_parasize(pop_size);
          end;

         { Release registers, but not the registers that contain the
           function result }
         if (not is_void(resultdef)) then
           begin
             case procdefinition.funcretloc[callerside].loc of
               LOC_REGISTER,
               LOC_CREGISTER:
                 begin
{$ifndef cpu64bitalu}
                   if procdefinition.funcretloc[callerside].size in [OS_64,OS_S64] then
                     begin
                       exclude(regs_to_save_int,getsupreg(procdefinition.funcretloc[callerside].register64.reghi));
                       exclude(regs_to_save_int,getsupreg(procdefinition.funcretloc[callerside].register64.reglo));
                     end
                   else
{$endif not cpu64bitalu}
                     exclude(regs_to_save_int,getsupreg(procdefinition.funcretloc[callerside].register));
                 end;
               LOC_FPUREGISTER,
               LOC_CFPUREGISTER:
                 exclude(regs_to_save_fpu,getsupreg(procdefinition.funcretloc[callerside].register));
               LOC_MMREGISTER,
               LOC_CMMREGISTER:
                 exclude(regs_to_save_mm,getsupreg(procdefinition.funcretloc[callerside].register));
               LOC_REFERENCE,
               LOC_VOID:
                 ;
               else
                 internalerror(2004110214);
              end;
           end;

{$if defined(x86) or defined(arm)}
         if procdefinition.proccalloption=pocall_safecall then
           begin
{$ifdef x86_64}
             cgpara.init;
             paramanager.getintparaloc(pocall_default,1,cgpara);
             cg.a_param_reg(current_asmdata.CurrAsmList,OS_ADDR,NR_RAX,cgpara);
             cgpara.done;
{$endif x86_64}
             cg.allocallcpuregisters(current_asmdata.CurrAsmList);
             cg.a_call_name(current_asmdata.CurrAsmList,'FPC_SAFECALLCHECK');
             cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
           end;
{$endif}

         if cg.uses_registers(R_MMREGISTER) then
           cg.dealloccpuregisters(current_asmdata.CurrAsmList,R_MMREGISTER,regs_to_save_mm);
         if cg.uses_registers(R_FPUREGISTER) then
           cg.dealloccpuregisters(current_asmdata.CurrAsmList,R_FPUREGISTER,regs_to_save_fpu);
         cg.dealloccpuregisters(current_asmdata.CurrAsmList,R_INTREGISTER,regs_to_save_int);

         { handle function results }
         if (not is_void(resultdef)) then
           handle_return_value
         else
           location_reset(location,LOC_VOID,OS_NO);

         { convert persistent temps for parameters and function result to normal temps }
         if assigned(callcleanupblock) then
           secondpass(callcleanupblock);

         { release temps and finalize unused return values, must be
           after the callcleanupblock because that converts temps
           from persistent to normal }
         release_unused_return_value;

         { release temps of paras }
         release_para_temps;

         { perhaps i/o check ? }
         if (cs_check_io in current_settings.localswitches) and
            (po_iocheck in procdefinition.procoptions) and
            not(po_iocheck in current_procinfo.procdef.procoptions) and
            { no IO check for methods and procedure variables }
            (right=nil) and
            not(po_virtualmethod in procdefinition.procoptions) then
           begin
              cg.allocallcpuregisters(current_asmdata.CurrAsmList);
              cg.a_call_name(current_asmdata.CurrAsmList,'FPC_IOCHECK');
              cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
           end;
      end;


begin
   ccallparanode:=tcgcallparanode;
   ccallnode:=tcgcallnode;
end.
