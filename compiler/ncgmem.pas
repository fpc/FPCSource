{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for memory related nodes which are
    the same for all (most?) processors

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
unit ncgmem;

{$i fpcdefs.inc}

interface

    uses
      globtype,cgbase,cpuinfo,cpubase,
      node,nmem;

    type
       tcgloadvmtaddrnode = class(tloadvmtaddrnode)
          procedure pass_generate_code;override;
       end;

       tcgloadparentfpnode = class(tloadparentfpnode)
          procedure pass_generate_code;override;
       end;

       tcgaddrnode = class(taddrnode)
          procedure pass_generate_code;override;
       end;

       tcgderefnode = class(tderefnode)
          procedure pass_generate_code;override;
       end;

       tcgsubscriptnode = class(tsubscriptnode)
          procedure pass_generate_code;override;
       end;

       tcgwithnode = class(twithnode)
          procedure pass_generate_code;override;
       end;

       tcgvecnode = class(tvecnode)
         function get_mul_size : aint;
       private
         procedure rangecheck_array;
       protected
         {# This routine is used to calculate the address of the reference.
            On entry reg contains the index in the array,
           and l contains the size of each element in the array.
           This routine should update location.reference correctly,
           so it points to the correct address.
         }
         procedure update_reference_reg_mul(maybe_const_reg:tregister;l:aint);virtual;
         procedure update_reference_reg_packed(maybe_const_reg:tregister;l:aint);virtual;
         procedure second_wideansistring;virtual;
         procedure second_dynamicarray;virtual;
       public
         procedure pass_generate_code;override;
       end;


implementation

    uses
      systems,
      cutils,cclasses,verbose,globals,constexp,
      symconst,symdef,symsym,symtable,defutil,paramgr,
      aasmbase,aasmtai,aasmdata,
      procinfo,pass_2,parabase,
      pass_1,nld,ncon,nadd,nutils,
      cgutils,cgobj,
      tgobj,ncgutil,objcgutl
      ;


{*****************************************************************************
                              TCGLOADVMTADDRNODE
*****************************************************************************}

    procedure tcgloadvmtaddrnode.pass_generate_code;
      var
        href    : treference;
        pool    : THashSet;
        entry   : PHashSetItem;
        typename: string;

      begin
         location_reset(location,LOC_REGISTER,OS_ADDR);
         if (left.nodetype=typen) then
           begin
             location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
             if not is_objcclass(left.resultdef) then
               begin
                 reference_reset_symbol(href,
                   current_asmdata.RefAsmSymbol(tobjectdef(tclassrefdef(resultdef).pointeddef).vmt_mangledname),0,
                   sizeof(pint));
                 cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,href,location.register);
               end
             else
               begin
                 { find/add necessary classref/classname pool entries }
                 if current_asmdata.ConstPools[sp_objcmetaclass]=nil then
                   current_asmdata.ConstPools[sp_objcmetaclass]:=THashSet.Create(64, True, False);
                 pool:=current_asmdata.ConstPools[sp_objcmetaclass];
                 typename:=left.resultdef.gettypename;
                 entry:=pool.FindOrAdd(@typename[1],length(typename));
                 objcfinishstringrefpoolentry(entry,sec_objc_cls_refs,sec_objc_class_names);
                 reference_reset_symbol(href,tasmlabel(entry^.Data),0,sizeof(pint));
                 cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,location.register);
               end;
           end
         else
           begin
             { left contains self, load vmt from self }
             secondpass(left);
             gen_load_vmt_register(current_asmdata.CurrAsmList,tobjectdef(left.resultdef),left.location,location.register);
           end;
      end;


{*****************************************************************************
                        TCGLOADPARENTFPNODE
*****************************************************************************}

    procedure tcgloadparentfpnode.pass_generate_code;
      var
        currpi : tprocinfo;
        hsym   : tparavarsym;
        href   : treference;
      begin
        if (current_procinfo.procdef.parast.symtablelevel=parentpd.parast.symtablelevel) then
          begin
            location_reset(location,LOC_REGISTER,OS_ADDR);
            location.register:=current_procinfo.framepointer;
          end
        else
          begin
            currpi:=current_procinfo;
            location_reset(location,LOC_REGISTER,OS_ADDR);
            location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
            { load framepointer of current proc }
            hsym:=tparavarsym(currpi.procdef.parast.Find('parentfp'));
            if not assigned(hsym) then
              internalerror(200309281);
            cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_ADDR,hsym.localloc,location.register);
            { walk parents }
            while (currpi.procdef.owner.symtablelevel>parentpd.parast.symtablelevel) do
              begin
                currpi:=currpi.parent;
                if not assigned(currpi) then
                  internalerror(200311201);
                hsym:=tparavarsym(currpi.procdef.parast.Find('parentfp'));
                if not assigned(hsym) then
                  internalerror(200309282);

                if hsym.localloc.loc<>LOC_REFERENCE then
                  internalerror(200309283);

                reference_reset_base(href,location.register,hsym.localloc.reference.offset,sizeof(pint));
                cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,href,location.register);
              end;
          end;
      end;


{*****************************************************************************
                             TCGADDRNODE
*****************************************************************************}

    procedure tcgaddrnode.pass_generate_code;
      begin
         secondpass(left);

         location_reset(location,LOC_REGISTER,OS_ADDR);
         location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
         if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           internalerror(2006111510);
         cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.location.reference,location.register);
      end;


{*****************************************************************************
                           TCGDEREFNODE
*****************************************************************************}

    procedure tcgderefnode.pass_generate_code;
      var
        paraloc1 : tcgpara;
      begin
         secondpass(left);
         { assume natural alignment, except for packed records }
         if not(resultdef.typ in [recorddef,objectdef]) or
            (tabstractrecordsymtable(tabstractrecorddef(resultdef).symtable).usefieldalignment<>1) then
           location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),resultdef.alignment)
         else
           location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),1);
         case left.location.loc of
            LOC_CREGISTER,
            LOC_REGISTER:
              begin
                maybechangeloadnodereg(current_asmdata.CurrAsmList,left,true);
              {$ifdef cpu_uses_separate_address_registers}
                if getregtype(left.location.register)<>R_ADDRESSREGISTER then
                  begin
                    location.reference.base := cg.getaddressregister(current_asmdata.CurrAsmList);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.register,
                      location.reference.base);
                  end
                else
              {$endif}
                  location.reference.base := left.location.register;
              end;
            LOC_CREFERENCE,
            LOC_REFERENCE:
              begin
                 location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                 cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_ADDR,left.location,location.reference.base);
              end;
            LOC_CONSTANT:
              begin
                location.reference.offset:=left.location.value;
              end;
            else
              internalerror(200507031);
         end;
         if (cs_use_heaptrc in current_settings.globalswitches) and
            (cs_checkpointer in current_settings.localswitches) and
            not(cs_compilesystem in current_settings.moduleswitches) and
            not(tpointerdef(left.resultdef).is_far) and
            not(nf_no_checkpointer in flags) and
            { can be NR_NO in case of LOC_CONSTANT }
            (location.reference.base<>NR_NO) then
          begin
            paraloc1.init;
            paramanager.getintparaloc(pocall_default,1,paraloc1);
            paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
            cg.a_param_reg(current_asmdata.CurrAsmList, OS_ADDR,location.reference.base,paraloc1);
            paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
            paraloc1.done;
            cg.allocallcpuregisters(current_asmdata.CurrAsmList);
            cg.a_call_name(current_asmdata.CurrAsmList,'FPC_CHECKPOINTER',false);
            cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
          end;
      end;


{*****************************************************************************
                          TCGSUBSCRIPTNODE
*****************************************************************************}

    procedure tcgsubscriptnode.pass_generate_code;
      var
        paraloc1 : tcgpara;
        sref: tsubsetreference;
      begin
         secondpass(left);
         if codegenerror then
           exit;
         paraloc1.init;
         { classes and interfaces must be dereferenced implicitly }
         if is_class_or_interface_or_objc(left.resultdef) then
           begin
             { the contents of a class are aligned to a sizeof(pointer) }
             location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),sizeof(pint));
             case left.location.loc of
                LOC_CREGISTER,
                LOC_REGISTER:
                  begin
                  {$ifdef cpu_uses_separate_address_registers}
                    if getregtype(left.location.register)<>R_ADDRESSREGISTER then
                      begin
                        location.reference.base:=rg.getaddressregister(current_asmdata.CurrAsmList);
                        cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,
                          left.location.register,location.reference.base);
                      end
                    else
                  {$endif}
                      location.reference.base := left.location.register;
                  end;
                LOC_CREFERENCE,
                LOC_REFERENCE:
                  begin
                     location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                     cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_ADDR,left.location,location.reference.base);
                  end;
             end;
             { implicit deferencing }
             if (cs_use_heaptrc in current_settings.globalswitches) and
                (cs_checkpointer in current_settings.localswitches) and
                not(cs_compilesystem in current_settings.moduleswitches) then
              begin
                paramanager.getintparaloc(pocall_default,1,paraloc1);
                paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
                cg.a_param_reg(current_asmdata.CurrAsmList, OS_ADDR,location.reference.base,paraloc1);
                paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
                cg.allocallcpuregisters(current_asmdata.CurrAsmList);
                cg.a_call_name(current_asmdata.CurrAsmList,'FPC_CHECKPOINTER',false);
                cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
              end;
           end
         else if is_interfacecom(left.resultdef) then
           begin
             location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),sizeof(pint));
             tg.GetTempTyped(current_asmdata.CurrAsmList,left.resultdef,tt_normal,location.reference);
             cg.a_load_loc_ref(current_asmdata.CurrAsmList,OS_ADDR,left.location,location.reference);
             { implicit deferencing also for interfaces }
             if (cs_use_heaptrc in current_settings.globalswitches) and
                (cs_checkpointer in current_settings.localswitches) and
                not(cs_compilesystem in current_settings.moduleswitches) then
              begin
                paramanager.getintparaloc(pocall_default,1,paraloc1);
                paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
                cg.a_param_reg(current_asmdata.CurrAsmList, OS_ADDR,location.reference.base,paraloc1);
                paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
                cg.allocallcpuregisters(current_asmdata.CurrAsmList);
                cg.a_call_name(current_asmdata.CurrAsmList,'FPC_CHECKPOINTER',false);
                cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
              end;
           end
         else
           begin
             location_copy(location,left.location);
             { some abi's require that functions return (some) records in }
             { registers                                                  }
             case location.loc of
               LOC_REFERENCE,
               LOC_CREFERENCE:
                 ;
               LOC_REGISTER,
               LOC_CREGISTER:
                 begin
                   if (left.resultdef.size > sizeof(pint)) then
                     location_force_mem(current_asmdata.CurrAsmList,location)
                   else
                     begin
                       if (left.location.loc = LOC_REGISTER) then
                         location.loc := LOC_SUBSETREG
                       else
                         location.loc := LOC_CSUBSETREG;
                       location.size:=def_cgsize(resultdef);
                       location.sreg.subsetreg := left.location.register;
                       location.sreg.subsetregsize := left.location.size;
                       if not is_packed_record_or_object(left.resultdef) then
                         begin
                           if (target_info.endian = ENDIAN_BIG) then
                             location.sreg.startbit := (tcgsize2size[location.sreg.subsetregsize] - tcgsize2size[location.size] - vs.fieldoffset) * 8
                           else
                             location.sreg.startbit := (vs.fieldoffset * 8);
                           location.sreg.bitlen := tcgsize2size[location.size] * 8;
                         end
                       else
                         begin
                           location.sreg.bitlen := resultdef.packedbitsize;
                           if (target_info.endian = ENDIAN_BIG) then
                             location.sreg.startbit := (tcgsize2size[location.sreg.subsetregsize]*8 - location.sreg.bitlen) - vs.fieldoffset
                           else
                             location.sreg.startbit := vs.fieldoffset;
                         end;
                     end;
                 end;
               LOC_SUBSETREG,
               LOC_CSUBSETREG:
                 begin
                   location.size:=def_cgsize(resultdef);
                   if not is_packed_record_or_object(left.resultdef) then
                     begin
                       if (target_info.endian = ENDIAN_BIG) then
                         inc(location.sreg.startbit, (left.resultdef.size - tcgsize2size[location.size] - vs.fieldoffset) * 8)
                       else
                         inc(location.sreg.startbit, vs.fieldoffset * 8);
                       location.sreg.bitlen := tcgsize2size[location.size] * 8;
                     end
                   else
                     begin
                       location.sreg.bitlen := resultdef.packedbitsize;
                       if (target_info.endian = ENDIAN_BIG) then
                         inc(location.sreg.startbit, left.location.sreg.bitlen - location.sreg.bitlen - vs.fieldoffset)
                       else
                         inc(location.sreg.startbit, vs.fieldoffset);
                     end;
                 end;
               else
                 internalerror(2006031901);
             end;
           end;

         if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           begin
             if not is_packed_record_or_object(left.resultdef) then
               begin
                 inc(location.reference.offset,vs.fieldoffset);
                 location.reference.alignment:=newalignment(location.reference.alignment,vs.fieldoffset);
               end
             else if (vs.fieldoffset mod 8 = 0) and
                     (resultdef.packedbitsize mod 8 = 0) and
                     { is different in case of e.g. packenum 2 and an enum }
                     { which fits in 8 bits                                }
                     (resultdef.size*8 = resultdef.packedbitsize) then
               begin
                 inc(location.reference.offset,vs.fieldoffset div 8);
                 location.reference.alignment:=newalignment(location.reference.alignment,vs.fieldoffset div 8);
               end
             else
               begin
                 sref.ref:=location.reference;
                 sref.ref.alignment:=1;
                 sref.bitindexreg:=NR_NO;
                 inc(sref.ref.offset,vs.fieldoffset div 8);
                 sref.startbit:=vs.fieldoffset mod 8;
                 sref.bitlen:=resultdef.packedbitsize;
                 if (left.location.loc=LOC_REFERENCE) then
                   location.loc:=LOC_SUBSETREF
                 else
                   location.loc:=LOC_CSUBSETREF;
                 location.sref:=sref;
               end;
             { also update the size of the location }
             location.size:=def_cgsize(resultdef);
           end;
         paraloc1.done;
      end;


{*****************************************************************************
                            TCGWITHNODE
*****************************************************************************}

    procedure tcgwithnode.pass_generate_code;
      begin
        location_reset(location,LOC_VOID,OS_NO);

        if assigned(left) then
          secondpass(left);
       end;


{*****************************************************************************
                            TCGVECNODE
*****************************************************************************}

     function tcgvecnode.get_mul_size : aint;
       begin
         if nf_memindex in flags then
          get_mul_size:=1
         else
          begin
            if (left.resultdef.typ=arraydef) then
             if not is_packed_array(left.resultdef) then
              get_mul_size:=tarraydef(left.resultdef).elesize
             else
              get_mul_size:=tarraydef(left.resultdef).elepackedbitsize
            else
             get_mul_size:=resultdef.size;
          end
       end;


     { this routine must, like any other routine, not change the contents }
     { of base/index registers of references, as these may be regvars.    }
     { The register allocator can coalesce one LOC_REGISTER being moved   }
     { into another (as their live ranges won't overlap), but not a       }
     { LOC_CREGISTER moved into a LOC_(C)REGISTER most of the time (as    }
     { the live range of the LOC_CREGISTER will most likely overlap the   }
     { the live range of the target LOC_(C)REGISTER)                      }
     { The passed register may be a LOC_CREGISTER as well.                }
     procedure tcgvecnode.update_reference_reg_mul(maybe_const_reg:tregister;l:aint);
       var
         hreg: tregister;
       begin
         if l<>1 then
           begin
             hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
             cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_IMUL,OS_ADDR,l,maybe_const_reg,hreg);
             maybe_const_reg:=hreg;
           end;
         if location.reference.base=NR_NO then
           location.reference.base:=maybe_const_reg
         else if location.reference.index=NR_NO then
           location.reference.index:=maybe_const_reg
         else
          begin
            hreg:=cg.getaddressregister(current_asmdata.CurrAsmList);
            cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,location.reference,hreg);
            reference_reset_base(location.reference,hreg,0,location.reference.alignment);
            { insert new index register }
            location.reference.index:=maybe_const_reg;
          end;
          { update alignment }
          if (location.reference.alignment=0) then
            internalerror(2009020704);
          location.reference.alignment:=newalignment(location.reference.alignment,l);
       end;


     { see remarks for tcgvecnode.update_reference_reg_mul above }
     procedure tcgvecnode.update_reference_reg_packed(maybe_const_reg:tregister;l:aint);
       var
         sref: tsubsetreference;
         offsetreg, hreg: tregister;
         alignpower: aint;
         temp : longint;
       begin
         { only orddefs are bitpacked. Even then we only need special code in }
         { case the bitpacked *byte size* is not a power of two, otherwise    }
         { everything can be handled using the the regular array code.        }
         if ((l mod 8) = 0) and
            (ispowerof2(l div 8,temp) or
             not is_ordinal(resultdef)) then
           begin
             update_reference_reg_mul(maybe_const_reg,l div 8);
             exit;
           end;
         if (l > 8*sizeof(aint)) then
           internalerror(200608051);
         sref.ref := location.reference;
         hreg := cg.getaddressregister(current_asmdata.CurrAsmList);
         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SUB,OS_INT,tarraydef(left.resultdef).lowrange,maybe_const_reg,hreg);
         cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_IMUL,OS_INT,l,hreg);
         { keep alignment for index }
         sref.ref.alignment := left.resultdef.alignment;
         if not ispowerof2(sref.ref.alignment,temp) then
           internalerror(2006081201);
         alignpower:=temp;
         offsetreg := cg.getaddressregister(current_asmdata.CurrAsmList);
         cg.a_op_const_reg_reg(current_asmdata.CurrAsmList,OP_SHR,OS_ADDR,3+alignpower,hreg,offsetreg);
         cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_SHL,OS_ADDR,alignpower,offsetreg);
         if (sref.ref.base = NR_NO) then
           sref.ref.base := offsetreg
         else if (sref.ref.index = NR_NO) then
           sref.ref.index := offsetreg
         else
           begin
             cg.a_op_reg_reg(current_asmdata.CurrAsmList,OP_ADD,OS_ADDR,sref.ref.base,offsetreg);
             sref.ref.base := offsetreg;
           end;
         cg.a_op_const_reg(current_asmdata.CurrAsmList,OP_AND,OS_INT,(1 shl (3+alignpower))-1,hreg);
         sref.bitindexreg := hreg;
         sref.startbit := 0;
         sref.bitlen := resultdef.packedbitsize;
         if (left.location.loc = LOC_REFERENCE) then
           location.loc := LOC_SUBSETREF
         else
           location.loc := LOC_CSUBSETREF;
         location.sref := sref;
       end;


     procedure tcgvecnode.second_wideansistring;
       begin
       end;

     procedure tcgvecnode.second_dynamicarray;
       begin
       end;


     procedure tcgvecnode.rangecheck_array;
       var
         hightree : tnode;
         poslabel,
         neglabel : tasmlabel;
         hreg : tregister;
         paraloc1,paraloc2 : tcgpara;
       begin
         paraloc1.init;
         paraloc2.init;
         if is_open_array(left.resultdef) or
            is_array_of_const(left.resultdef) then
          begin
            { cdecl functions don't have high() so we can not check the range }
            { (can't use current_procdef, since it may be a nested procedure) }
            if not(tprocdef(tparasymtable(tparavarsym(tloadnode(left).symtableentry).owner).defowner).proccalloption in [pocall_cdecl,pocall_cppdecl]) then
             begin
               { Get high value }
               hightree:=load_high_value_node(tparavarsym(tloadnode(left).symtableentry));
               { it must be available }
               if not assigned(hightree) then
                 internalerror(200212201);
               firstpass(hightree);
               secondpass(hightree);
               { generate compares }
               if (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                 hreg:=cg.makeregsize(current_asmdata.CurrAsmList,right.location.register,OS_INT)
               else
                 begin
                   hreg:=cg.getintregister(current_asmdata.CurrAsmList,OS_INT);
                   cg.a_load_loc_reg(current_asmdata.CurrAsmList,OS_INT,right.location,hreg);
                 end;
               current_asmdata.getjumplabel(neglabel);
               current_asmdata.getjumplabel(poslabel);
               cg.a_cmp_const_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_LT,0,hreg,poslabel);
               cg.a_cmp_loc_reg_label(current_asmdata.CurrAsmList,OS_INT,OC_BE,hightree.location,hreg,neglabel);
               cg.a_label(current_asmdata.CurrAsmList,poslabel);
               cg.a_call_name(current_asmdata.CurrAsmList,'FPC_RANGEERROR',false);
               cg.a_label(current_asmdata.CurrAsmList,neglabel);
               { release hightree }
               hightree.free;
             end;
          end
         else
          if is_dynamic_array(left.resultdef) then
            begin
               paramanager.getintparaloc(pocall_default,1,paraloc1);
               paramanager.getintparaloc(pocall_default,2,paraloc2);
               paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc2);
               cg.a_param_loc(current_asmdata.CurrAsmList,right.location,paraloc2);
               paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
               cg.a_param_loc(current_asmdata.CurrAsmList,left.location,paraloc1);
               paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
               paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc2);
               cg.allocallcpuregisters(current_asmdata.CurrAsmList);
               cg.a_call_name(current_asmdata.CurrAsmList,'FPC_DYNARRAY_RANGECHECK',false);
               cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
            end
         else
           cg.g_rangecheck(current_asmdata.CurrAsmList,right.location,right.resultdef,left.resultdef);
         paraloc1.done;
         paraloc2.done;
       end;


    procedure tcgvecnode.pass_generate_code;

      var
         offsetdec,
         extraoffset : aint;
         t        : tnode;
         href     : treference;
         otl,ofl  : tasmlabel;
         newsize  : tcgsize;
         mulsize,
         bytemulsize,
         alignpow : aint;
         isjump   : boolean;
         paraloc1,
         paraloc2 : tcgpara;
         subsetref : tsubsetreference;
         temp : longint;
      begin
         paraloc1.init;
         paraloc2.init;
         mulsize:=get_mul_size;
         if not is_packed_array(left.resultdef) then
           bytemulsize:=mulsize
         else
           bytemulsize:=mulsize div 8;

         newsize:=def_cgsize(resultdef);
         secondpass(left);
         if left.location.loc=LOC_CREFERENCE then
           location_reset_ref(location,LOC_CREFERENCE,newsize,left.location.reference.alignment)
         else
           location_reset_ref(location,LOC_REFERENCE,newsize,left.location.reference.alignment);

         { an ansistring needs to be dereferenced }
         if is_ansistring(left.resultdef) or
            is_widestring(left.resultdef) or
            is_unicodestring(left.resultdef) then
           begin
              if nf_callunique in flags then
                internalerror(200304236);

              {DM!!!!!}
              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  begin
{$ifdef m68k}
                    location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                    cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.register,location.reference.base);
{$else m68k}
                    location.reference.base:=left.location.register;
{$endif m68k}
                  end;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                    cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032218);
              end;

              { check for a zero length string,
                we can use the ansistring routine here }
              if (cs_check_range in current_settings.localswitches) then
                begin
                   paramanager.getintparaloc(pocall_default,1,paraloc1);
                   paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
                   cg.a_param_reg(current_asmdata.CurrAsmList,OS_ADDR,location.reference.base,paraloc1);
                   paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
                   cg.allocallcpuregisters(current_asmdata.CurrAsmList);
                   cg.a_call_name(current_asmdata.CurrAsmList,'FPC_'+upper(tstringdef(left.resultdef).stringtypname)+'_CHECKZERO',false);
                   cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
                end;

              { in ansistrings/widestrings S[1] is p<w>char(S)[0] !! }
              if is_ansistring(left.resultdef) then
                offsetdec:=1
              else
                offsetdec:=2;
              location.reference.alignment:=offsetdec;
              dec(location.reference.offset,offsetdec);
           end
         else if is_dynamic_array(left.resultdef) then
           begin
              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  location.reference.base:=left.location.register;
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                     location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                     cg.a_load_ref_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,
                      left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032219);
              end;
              { a dynarray points to the start of a memory block, which
                we assume to be always aligned to a multiple of the
                pointer size
              }
              location.reference.alignment:=sizeof(pint);
           end
         else
           location_copy(location,left.location);

         { location must be memory }
         if not(location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           internalerror(200411013);

         { offset can only differ from 0 if arraydef }
         if (left.resultdef.typ=arraydef) and
            not(is_dynamic_array(left.resultdef)) and
            (not(is_packed_array(left.resultdef)) or
             ((mulsize mod 8 = 0) and
              ispowerof2(mulsize div 8,temp)) or
              { only orddefs are bitpacked }
              not is_ordinal(resultdef)) then
           dec(location.reference.offset,bytemulsize*tarraydef(left.resultdef).lowrange);

         if right.nodetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              case left.resultdef.typ of
                arraydef :
                  begin
		     { do not do any range checking when this is an array access to a pointer which has been
		       typecasted from an array }
		     if (not (ado_isconvertedpointer in tarraydef(left.resultdef).arrayoptions)) then
		       begin
                     	if not(is_open_array(left.resultdef)) and
                           not(is_array_of_const(left.resultdef)) and
                           not(is_dynamic_array(left.resultdef)) then
                          begin
                            if (tordconstnode(right).value.svalue>tarraydef(left.resultdef).highrange) or
                               (tordconstnode(right).value.svalue<tarraydef(left.resultdef).lowrange) then
                              begin
                                { this should be caught in the typecheckpass! (JM) }
                                if (cs_check_range in current_settings.localswitches) then
                                  CGMessage(parser_e_range_check_error)
                                else
                                  CGMessage(parser_w_range_check_error);
                              end;
                           end
                         else
                           begin
                              { range checking for open and dynamic arrays needs
                                runtime code }
                              secondpass(right);
                              if (cs_check_range in current_settings.localswitches) then
                                rangecheck_array;
                           end;
		       end;
                  end;
                stringdef :
                  begin
                    if (cs_check_range in current_settings.localswitches) then
                     begin
                       case tstringdef(left.resultdef).stringtype of
                         { it's the same for ansi- and wide strings }
                         st_unicodestring,
                         st_widestring,
                         st_ansistring:
                           begin
                              paramanager.getintparaloc(pocall_default,1,paraloc1);
                              paramanager.getintparaloc(pocall_default,2,paraloc2);
                              paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc2);
                              cg.a_param_const(current_asmdata.CurrAsmList,OS_INT,tordconstnode(right).value.svalue,paraloc2);
                              href:=location.reference;
                              paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
                              if not(tf_winlikewidestring in target_info.flags) or
                                 (tstringdef(left.resultdef).stringtype<>st_widestring) then
                                begin
                                  dec(href.offset,sizeof(pint)-offsetdec);
                                  cg.a_param_ref(current_asmdata.CurrAsmList,OS_ADDR,href,paraloc1);
                                end
                              else
                                begin
                                  { winlike widestrings have a 4 byte length }
                                  dec(href.offset,4-offsetdec);
                                  cg.a_param_ref(current_asmdata.CurrAsmList,OS_32,href,paraloc1);
                                end;
                              paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
                              paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc2);
                              cg.allocallcpuregisters(current_asmdata.CurrAsmList);
                              cg.a_call_name(current_asmdata.CurrAsmList,'FPC_'+upper(tstringdef(left.resultdef).stringtypname)+'_RANGECHECK',false);
                              cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
                           end;

                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                              { if this one is implemented making use of the high parameter for openshortstrings, update ncgutils.do_get_used_regvars() too (JM) }
                           end;

                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                       end;
                     end;
                   end;
              end;
              if not(is_packed_array(left.resultdef)) or
                 ((mulsize mod 8 = 0) and
                  (ispowerof2(mulsize div 8,temp) or
                   { only orddefs are bitpacked }
                   not is_ordinal(resultdef))) then
                begin
                  extraoffset:=bytemulsize*tordconstnode(right).value.svalue;
                  inc(location.reference.offset,extraoffset);
                  { adjust alignment after to this change }
                  location.reference.alignment:=newalignment(location.reference.alignment,extraoffset);
                  { don't do this for floats etc.; needed to properly set the }
                  { size for bitpacked arrays (e.g. a bitpacked array of      }
                  { enums who are size 2 but fit in one byte -> in the array  }
                  { they will be one byte and have to be stored like that)    }
                  if is_packed_array(left.resultdef) and
                     (tcgsize2size[newsize] <> bytemulsize) then
                    newsize:=int_cgsize(bytemulsize);
                end
              else
                begin
                  subsetref.ref := location.reference;
                  subsetref.ref.alignment := left.resultdef.alignment;
                  if not ispowerof2(subsetref.ref.alignment,temp) then
                    internalerror(2006081212);
                  alignpow:=temp;
                  inc(subsetref.ref.offset,((mulsize * (tordconstnode(right).value.svalue-tarraydef(left.resultdef).lowrange)) shr (3+alignpow)) shl alignpow);
                  subsetref.bitindexreg := NR_NO;
                  subsetref.startbit := (mulsize * (tordconstnode(right).value.svalue-tarraydef(left.resultdef).lowrange)) and ((1 shl (3+alignpow))-1);
                  subsetref.bitlen := resultdef.packedbitsize;
                  if (left.location.loc = LOC_REFERENCE) then
                    location.loc := LOC_SUBSETREF
                  else
                    location.loc := LOC_CSUBSETREF;
                  location.sref := subsetref;
                end;
           end
         else
         { not nodetype=ordconstn }
           begin
              if (cs_opt_level1 in current_settings.optimizerswitches) and
                 { if we do range checking, we don't }
                 { need that fancy code (it would be }
                 { buggy)                            }
                 not(cs_check_range in current_settings.localswitches) and
                 (left.resultdef.typ=arraydef) and
                 not is_packed_array(left.resultdef) then
                begin
                   extraoffset:=0;
                   if (right.nodetype=addn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).right).value.svalue;
                             t:=taddnode(right).left;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end
                        else if taddnode(right).left.nodetype=ordconstn then
                          begin
                             extraoffset:=tordconstnode(taddnode(right).left).value.svalue;
                             t:=taddnode(right).right;
                             taddnode(right).right:=nil;
                             right.free;
                             right:=t;
                          end;
                     end
                   else if (right.nodetype=subn) then
                     begin
                        if taddnode(right).right.nodetype=ordconstn then
                          begin
                             extraoffset:=-tordconstnode(taddnode(right).right).value.svalue;
                             t:=taddnode(right).left;
                             taddnode(right).left:=nil;
                             right.free;
                             right:=t;
                          end;
                     end;
                   inc(location.reference.offset,
                       mulsize*extraoffset);
                end;
              { calculate from left to right }
              if not(location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                internalerror(200304237);
              isjump:=(right.expectloc=LOC_JUMP);
              if isjump then
               begin
                 otl:=current_procinfo.CurrTrueLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
                 ofl:=current_procinfo.CurrFalseLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
               end;
              secondpass(right);

              { if mulsize = 1, we won't have to modify the index }
              location_force_reg(current_asmdata.CurrAsmList,right.location,OS_ADDR,true);

              if isjump then
               begin
                 current_procinfo.CurrTrueLabel:=otl;
                 current_procinfo.CurrFalseLabel:=ofl;
               end
              else if (right.location.loc = LOC_JUMP) then
                internalerror(2006010801);

              { only range check now, we can't range check loc_flags/loc_jump }
              if cs_check_range in current_settings.localswitches then
               begin
                 if left.resultdef.typ=arraydef then
                   rangecheck_array;
               end;

            { produce possible range check code: }
              if cs_check_range in current_settings.localswitches then
               begin
                 if left.resultdef.typ=arraydef then
                   begin
                     { done defore (PM) }
                   end
                 else if (left.resultdef.typ=stringdef) then
                   begin
                      case tstringdef(left.resultdef).stringtype of
                         { it's the same for ansi- and wide strings }
                         st_unicodestring,
                         st_widestring,
                         st_ansistring:
                           begin
                              paramanager.getintparaloc(pocall_default,1,paraloc1);
                              paramanager.getintparaloc(pocall_default,2,paraloc2);
                              paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc2);
                              cg.a_param_reg(current_asmdata.CurrAsmList,OS_INT,right.location.register,paraloc2);
                              href:=location.reference;
                              dec(href.offset,sizeof(pint)-offsetdec);

                              href:=location.reference;
                              paramanager.allocparaloc(current_asmdata.CurrAsmList,paraloc1);
                              if not(tf_winlikewidestring in target_info.flags) or
                                 (tstringdef(left.resultdef).stringtype<>st_widestring) then
                                begin
                                  dec(href.offset,sizeof(pint)-offsetdec);
                                  cg.a_param_ref(current_asmdata.CurrAsmList,OS_ADDR,href,paraloc1);
                                end
                              else
                                begin
                                  { winlike widestrings have a 4 byte length }
                                  dec(href.offset,4-offsetdec);
                                  cg.a_param_ref(current_asmdata.CurrAsmList,OS_32,href,paraloc1);
                                end;

                              paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc1);
                              paramanager.freeparaloc(current_asmdata.CurrAsmList,paraloc2);
                              cg.allocallcpuregisters(current_asmdata.CurrAsmList);
                              cg.a_call_name(current_asmdata.CurrAsmList,'FPC_'+upper(tstringdef(left.resultdef).stringtypname)+'_RANGECHECK',false);
                              cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
                           end;
                         st_shortstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                         st_longstring:
                           begin
                              {!!!!!!!!!!!!!!!!!}
                           end;
                      end;
                   end;
               end;

              { insert the register and the multiplication factor in the
                reference }
              if not is_packed_array(left.resultdef) then
                update_reference_reg_mul(right.location.register,mulsize)
              else
                update_reference_reg_packed(right.location.register,mulsize);
           end;

        location.size:=newsize;
        paraloc1.done;
        paraloc2.done;
      end;


begin
   cloadvmtaddrnode:=tcgloadvmtaddrnode;
   cloadparentfpnode:=tcgloadparentfpnode;
   caddrnode:=tcgaddrnode;
   cderefnode:=tcgderefnode;
   csubscriptnode:=tcgsubscriptnode;
   cwithnode:=tcgwithnode;
   cvecnode:=tcgvecnode;
end.
