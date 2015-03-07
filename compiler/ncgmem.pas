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
      globtype,cgbase,cgutils,cpuinfo,cpubase,
      symtype,
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
         procedure rangecheck_string;
       protected
         {# This routine is used to calculate the address of the reference.
            On entry reg contains the index in the array,
           and l contains the size of each element in the array.
           This routine should update location.reference correctly,
           so it points to the correct address.
         }
         procedure update_reference_reg_mul(maybe_const_reg: tregister;regsize: tdef; l: aint);virtual;
         procedure update_reference_reg_packed(maybe_const_reg: tregister; regsize: tdef; l: aint);virtual;
         procedure update_reference_offset(var ref: treference; index, mulsize: aint); virtual;
         procedure second_wideansistring;virtual;
         procedure second_dynamicarray;virtual;
         function valid_index_size(size: tcgsize): boolean;virtual;
       public
         procedure pass_generate_code;override;
       end;


implementation

    uses
      systems,
      cutils,cclasses,verbose,globals,constexp,
      symconst,symbase,symdef,symsym,symcpu,symtable,defutil,paramgr,
      aasmbase,aasmtai,aasmdata,
      procinfo,pass_2,parabase,
      pass_1,nld,ncon,nadd,ncnv,nutils,
      cgobj,hlcgobj,
      tgobj,ncgutil,objcgutl,
      defcmp
      ;


{*****************************************************************************
                              TCGLOADVMTADDRNODE
*****************************************************************************}

    procedure tcgloadvmtaddrnode.pass_generate_code;
      var
        href    : treference;
        pool    : THashSet;
        entry   : PHashSetItem;

      begin
         location_reset(location,LOC_REGISTER,def_cgsize(voidpointertype));
         if (left.nodetype=typen) then
           begin
             location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,voidpointertype);
             if not is_objcclass(left.resultdef) then
               begin
                 reference_reset_symbol(href,
                   current_asmdata.RefAsmSymbol(tobjectdef(tclassrefdef(resultdef).pointeddef).vmt_mangledname,AT_DATA),0,
                   voidpointertype.size);
                 hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,href,location.register);
               end
             else
               begin
                 pool:=current_asmdata.ConstPools[sp_objcclassnamerefs];
                 entry:=pool.FindOrAdd(@tobjectdef(left.resultdef).objextname^[1],length(tobjectdef(left.resultdef).objextname^));
                 if (target_info.system in systems_objc_nfabi) then
                   begin
                     { find/add necessary classref/classname pool entries }
                     objcfinishclassrefnfpoolentry(entry,tobjectdef(left.resultdef));
                   end
                 else
                   begin
                     { find/add necessary classref/classname pool entries }
                     objcfinishstringrefpoolentry(entry,sp_objcclassnames,sec_objc_cls_refs,sec_objc_class_names);
                   end;
                 reference_reset_symbol(href,tasmlabel(entry^.Data),0,voidpointertype.size);
                 hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,voidpointertype,voidpointertype,href,location.register);
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
            location_reset(location,LOC_REGISTER,def_cgsize(parentfpvoidpointertype));
            location.register:=current_procinfo.framepointer;
          end
        else
          begin
            currpi:=current_procinfo;
            location_reset(location,LOC_REGISTER,def_cgsize(parentfpvoidpointertype));
            location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,parentfpvoidpointertype);
            { load framepointer of current proc }
            hsym:=tparavarsym(currpi.procdef.parast.Find('parentfp'));
            if not assigned(hsym) then
              internalerror(200309281);
            hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,parentfpvoidpointertype,parentfpvoidpointertype,hsym.localloc,location.register);
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

                hlcg.reference_reset_base(href,parentfpvoidpointertype,location.register,hsym.localloc.reference.offset,sizeof(pint));
                hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,parentfpvoidpointertype,parentfpvoidpointertype,href,location.register);
              end;
          end;
      end;


{*****************************************************************************
                             TCGADDRNODE
*****************************************************************************}

    procedure tcgaddrnode.pass_generate_code;
      begin
         secondpass(left);

         location_reset(location,LOC_REGISTER,int_cgsize(resultdef.size));
         location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,resultdef);
         if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
           { on x86_64-win64, array of chars can be returned in registers, however,
             when passing these arrays to other functions, the compiler wants to take
             the address of the array so when the addrnode has been created internally,
             we have to force the data into memory, see also tw14388.pp
           }
           if nf_internal in flags then
             hlcg.location_force_mem(current_asmdata.CurrAsmList,left.location,left.resultdef)
           else
             internalerror(2006111510);
         hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.resultdef,resultdef,left.location.reference,location.register);
      end;


{*****************************************************************************
                           TCGDEREFNODE
*****************************************************************************}

    procedure tcgderefnode.pass_generate_code;
      var
        paraloc1 : tcgpara;
        pd : tprocdef;
        sym : tsym;
        st : tsymtable;
        hp : pnode;
        hp2 : tnode;
        extraoffset : tcgint;
      begin
         sym:=nil;
         { assume natural alignment, except for packed records }
         if not(resultdef.typ in [recorddef,objectdef]) or
            (tabstractrecordsymtable(tabstractrecorddef(resultdef).symtable).usefieldalignment<>1) then
           location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),resultdef.alignment)
         else
           location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),1);

         { can we fold an add/sub node into the offset of the deref node? }
         extraoffset:=0;
         hp:=actualtargetnode(@left);
         if (hp^.nodetype=subn) and is_constintnode(taddnode(hp^).right) then
           begin
             extraoffset:=-tcgint(tordconstnode(taddnode(hp^).right).value);
             replacenode(hp^,taddnode(hp^).left);
           end
         else if (hp^.nodetype=addn) and is_constintnode(taddnode(hp^).right) then
           begin
             extraoffset:=tcgint(tordconstnode(taddnode(hp^).right).value);
             replacenode(hp^,taddnode(hp^).left);
           end
         else if (hp^.nodetype=addn) and is_constintnode(taddnode(hp^).left) then
           begin
             extraoffset:=tcgint(tordconstnode(taddnode(hp^).left).value);
             replacenode(hp^,taddnode(hp^).right);
           end;

         secondpass(left);

         if not(left.location.loc in [LOC_CREGISTER,LOC_REGISTER,LOC_CREFERENCE,LOC_REFERENCE,LOC_CONSTANT]) then
           hlcg.location_force_reg(current_asmdata.CurrAsmList,left.location,left.resultdef,left.resultdef,true);
         case left.location.loc of
            LOC_CREGISTER,
            LOC_REGISTER:
              begin
                hlcg.maybe_change_load_node_reg(current_asmdata.CurrAsmList,left,true);
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
                 hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,left.location,location.reference.base);
              end;
            LOC_CONSTANT:
              begin
                location.reference.offset:=left.location.value;
              end;
            else
              internalerror(200507031);
         end;
         location.reference.offset:=location.reference.offset+extraoffset;
         if (cs_use_heaptrc in current_settings.globalswitches) and
            (cs_checkpointer in current_settings.localswitches) and
            not(cs_compilesystem in current_settings.moduleswitches) and
{$ifdef x86}
            (tcpupointerdef(left.resultdef).x86pointertyp = tcpupointerdefclass(cpointerdef).default_x86_data_pointer_type) and
{$endif x86}
            not(nf_no_checkpointer in flags) and
            { can be NR_NO in case of LOC_CONSTANT }
            (location.reference.base<>NR_NO) then
          begin
            if not searchsym_in_named_module('HEAPTRC','CHECKPOINTER',sym,st) or
               (sym.typ<>procsym) then
              internalerror(2012010601);
            pd:=tprocdef(tprocsym(sym).ProcdefList[0]);
            paraloc1.init;
            paramanager.getintparaloc(pd,1,paraloc1);
            hlcg.a_load_reg_cgpara(current_asmdata.CurrAsmList,left.resultdef,location.reference.base,paraloc1);
            paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
            paraloc1.done;
            hlcg.allocallcpuregisters(current_asmdata.CurrAsmList);
            hlcg.a_call_name(current_asmdata.CurrAsmList,pd,'FPC_CHECKPOINTER',nil,false);
            hlcg.deallocallcpuregisters(current_asmdata.CurrAsmList);
          end;
      end;


{*****************************************************************************
                          TCGSUBSCRIPTNODE
*****************************************************************************}

    procedure tcgsubscriptnode.pass_generate_code;
      var
        asmsym: tasmsymbol;
        paraloc1 : tcgpara;
        tmpref: treference;
        sref: tsubsetreference;
        offsetcorrection : aint;
        pd : tprocdef;
        sym : tsym;
        st : tsymtable;
      begin
         sym:=nil;
         secondpass(left);
         if codegenerror then
           exit;
         paraloc1.init;
         { several object types must be dereferenced implicitly }
         if is_implicit_pointer_object_type(left.resultdef) then
           begin
             if (not is_managed_type(left.resultdef)) or
                (target_info.system in systems_garbage_collected_managed_types) then
               begin
                 { the contents of a class are aligned to a sizeof(pointer) }
                 location_reset_ref(location,LOC_REFERENCE,def_cgsize(resultdef),voidpointertype.size);
                 case left.location.loc of
                    LOC_CREGISTER,
                    LOC_REGISTER:
                      begin
                      {$ifdef cpu_uses_separate_address_registers}
                        if getregtype(left.location.register)<>R_ADDRESSREGISTER then
                          begin
                            location.reference.base:=cg.getaddressregister(current_asmdata.CurrAsmList);
                            hlcg.a_load_reg_reg(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,
                              left.location.register,location.reference.base);
                          end
                        else
                      {$endif}
                          hlcg.reference_reset_base(location.reference,left.resultdef,left.location.register,0,location.reference.alignment);
                      end;
                    LOC_CREFERENCE,
                    LOC_REFERENCE,
                    { tricky type casting of parameters can cause these locations, see tb0592.pp on x86_64-linux }
                    LOC_SUBSETREG,
                    LOC_CSUBSETREG,
                    LOC_SUBSETREF,
                    LOC_CSUBSETREF:
                      begin
                         hlcg.reference_reset_base(location.reference,left.resultdef,
                           hlcg.getaddressregister(current_asmdata.CurrAsmList,left.resultdef),0,location.reference.alignment);
                         hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,left.location,location.reference.base);
                      end;
                    LOC_CONSTANT:
                      begin
                        { can happen with @classtype(pointerconst).field }
                        location.reference.offset:=left.location.value;
                      end;
                    else
                      internalerror(2009092401);
                 end;
                 { implicit deferencing }
                 if (cs_use_heaptrc in current_settings.globalswitches) and
                    (cs_checkpointer in current_settings.localswitches) and
                    not(cs_compilesystem in current_settings.moduleswitches) then
                  begin
                    if not searchsym_in_named_module('HEAPTRC','CHECKPOINTER',sym,st) or
                       (sym.typ<>procsym) then
                      internalerror(2012010602);
                    pd:=tprocdef(tprocsym(sym).ProcdefList[0]);
                    paramanager.getintparaloc(pd,1,paraloc1);
                    hlcg.a_load_reg_cgpara(current_asmdata.CurrAsmList,left.resultdef,location.reference.base,paraloc1);
                    paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
                    hlcg.allocallcpuregisters(current_asmdata.CurrAsmList);
                    hlcg.a_call_name(current_asmdata.CurrAsmList,pd,'FPC_CHECKPOINTER',nil,false);
                    hlcg.deallocallcpuregisters(current_asmdata.CurrAsmList);
                  end;
               end
             else
               { reference-counted implicit pointer object types don't have
                 fields -> cannot be subscripted (calls are handled via call
                 nodes) }
               internalerror(2011011901);
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
               LOC_CONSTANT,
               LOC_REGISTER,
               LOC_CREGISTER,
               { if a floating point value is casted into a record, it
                 can happen that we get here an fpu or mm register }
               LOC_MMREGISTER,
               LOC_FPUREGISTER,
               LOC_CMMREGISTER,
               LOC_CFPUREGISTER:
                 begin
                   { in case the result is not something that can be put
                     into an integer register (e.g.
                     function_returning_record().non_regable_field, or
                     a function returning a value > sizeof(intreg))
                     -> force to memory
                   }

                   if not tstoreddef(left.resultdef).is_intregable or
                      not tstoreddef(resultdef).is_intregable or
                      { if the field spans multiple registers, we must force the record into
                        memory as well }
                      ((left.location.size in [OS_PAIR,OS_SPAIR]) and
                       (vs.fieldoffset div sizeof(aword)<>(vs.fieldoffset+vs.getsize-1) div sizeof(aword))) or
                      (location.loc in [LOC_MMREGISTER,LOC_FPUREGISTER,LOC_CMMREGISTER,LOC_CFPUREGISTER,
                        { actually, we should be able to "subscript" a constant, but this would require some code
                          which enables dumping and reading constants from a temporary memory buffer. This
                          must be done a CPU dependent way, so it is not easy and probably not worth the effort (FK)
                        }
                        LOC_CONSTANT]) then
                     hlcg.location_force_mem(current_asmdata.CurrAsmList,location,left.resultdef)
                   else
                     begin
                       if (left.location.loc = LOC_REGISTER) then
                         location.loc := LOC_SUBSETREG
                       else
                         location.loc := LOC_CSUBSETREG;
                       location.size:=def_cgsize(resultdef);

                       offsetcorrection:=0;
                       if (left.location.size in [OS_PAIR,OS_SPAIR]) then
                         begin
                           if (vs.fieldoffset>=sizeof(aword)) xor (target_info.endian=endian_big) then
                             location.sreg.subsetreg := left.location.registerhi
                           else
                             location.sreg.subsetreg := left.location.register;

                           if (vs.fieldoffset>=sizeof(aword)) then
                             offsetcorrection:=sizeof(aword)*8;

                           location.sreg.subsetregsize := OS_INT;
                         end
                       else
                         begin
                           location.sreg.subsetreg := left.location.register;
                           location.sreg.subsetregsize := left.location.size;
                         end;

                       if not is_packed_record_or_object(left.resultdef) then
                         begin
                           if (target_info.endian = ENDIAN_BIG) then
                             location.sreg.startbit := (tcgsize2size[location.sreg.subsetregsize] - tcgsize2size[location.size] - vs.fieldoffset) * 8+offsetcorrection
                           else
                             location.sreg.startbit := (vs.fieldoffset * 8)-offsetcorrection;
                           location.sreg.bitlen := tcgsize2size[location.size] * 8;
                         end
                       else
                         begin
                           location.sreg.bitlen := resultdef.packedbitsize;
                           if (target_info.endian = ENDIAN_BIG) then
                             location.sreg.startbit := (tcgsize2size[location.sreg.subsetregsize]*8 - location.sreg.bitlen) - vs.fieldoffset+offsetcorrection
                           else
                             location.sreg.startbit := vs.fieldoffset-offsetcorrection;
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

         if is_objc_class_or_protocol(left.resultdef) and
            (target_info.system in systems_objc_nfabi) then
           begin
             if (location.loc<>LOC_REFERENCE) or
                (location.reference.index<>NR_NO) then
               internalerror(2009092402);
             { the actual field offset is stored in memory (to solve the
               "fragile base class" problem: this way the layout of base
               classes can be changed without breaking programs compiled against
               earlier versions)
             }
             asmsym:=current_asmdata.RefAsmSymbol(vs.mangledname);
             reference_reset_symbol(tmpref,asmsym,0,sizeof(pint));
             location.reference.index:=hlcg.getintregister(current_asmdata.CurrAsmList,ptruinttype);
             hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,ptruinttype,ptruinttype,tmpref,location.reference.index);
             { always packrecords C -> natural alignment }
             location.reference.alignment:=vs.vardef.alignment;
           end
         else if is_java_class_or_interface(left.resultdef) or
                 ((target_info.system in systems_jvm) and
                  (left.resultdef.typ=recorddef)) then
           begin
             if (location.loc<>LOC_REFERENCE) or
                (location.reference.index<>NR_NO) or
                assigned(location.reference.symbol) then
               internalerror(2011011301);
             location.reference.symbol:=current_asmdata.RefAsmSymbol(vs.mangledname);
           end
         else if (location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
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
     procedure tcgvecnode.update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);
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
     procedure tcgvecnode.update_reference_reg_packed(maybe_const_reg: tregister; regsize: tdef; l:aint);
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
             not is_ordinal(resultdef)
{$ifndef cpu64bitalu}
             or is_64bitint(resultdef)
{$endif not cpu64bitalu}
             ) then
           begin
             update_reference_reg_mul(maybe_const_reg,regsize,l div 8);
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
         if not ispowerof2(packedbitsloadsize(l),temp) then
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


     procedure tcgvecnode.update_reference_offset(var ref: treference; index, mulsize: aint);
       begin
         inc(ref.offset,index*mulsize);
       end;


     procedure tcgvecnode.second_wideansistring;
       begin
       end;

     procedure tcgvecnode.second_dynamicarray;
       begin
       end;


     function tcgvecnode.valid_index_size(size: tcgsize): boolean;
       begin
         result:=
           tcgsize2signed[size]=tcgsize2signed[OS_ADDR];
       end;


     procedure tcgvecnode.rangecheck_array;
       var
         hightree : tnode;
         poslabel,
         neglabel : tasmlabel;
         hreg : tregister;
         paraloc1,paraloc2 : tcgpara;
         pd : tprocdef;
       begin
         { omit range checking when this is an array access to a pointer which has been
           typecasted from an array }
         if (ado_isconvertedpointer in tarraydef(left.resultdef).arrayoptions) then
           exit;
         paraloc1.init;
         paraloc2.init;
         if is_open_array(left.resultdef) or
            is_array_of_const(left.resultdef) then
          begin
            { cdecl functions don't have high() so we can not check the range }
            { (can't use current_procdef, since it may be a nested procedure) }
            if not(tprocdef(tparasymtable(tparavarsym(tloadnode(get_open_const_array(left)).symtableentry).owner).defowner).proccalloption in cdecl_pocalls) then
             begin
               { Get high value }
               hightree:=load_high_value_node(tparavarsym(tloadnode(get_open_const_array(left)).symtableentry));
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
                   hlcg.a_load_loc_reg(current_asmdata.CurrAsmList,right.resultdef,osuinttype,right.location,hreg);
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
               pd:=search_system_proc('fpc_dynarray_rangecheck');
               paramanager.getintparaloc(pd,1,paraloc1);
               paramanager.getintparaloc(pd,2,paraloc2);
               if pd.is_pushleftright then
                 begin
                   cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,left.location,paraloc1);
                   cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,right.location,paraloc2);
                 end
               else
                 begin
                   cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,right.location,paraloc2);
                   cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,left.location,paraloc1);
                 end;
               paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
               paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc2);
               cg.allocallcpuregisters(current_asmdata.CurrAsmList);
               cg.a_call_name(current_asmdata.CurrAsmList,'FPC_DYNARRAY_RANGECHECK',false);
               cg.deallocallcpuregisters(current_asmdata.CurrAsmList);
            end;
{ for regular arrays, we don't have to do anything because the index has been
  type converted to the index type, which already inserted a range check if
  necessary }
         paraloc1.done;
         paraloc2.done;
       end;

    procedure tcgvecnode.rangecheck_string;
      var
        paraloc1,
        paraloc2: tcgpara;
        helpername: TIDString;
        pd: tprocdef;
      begin
        paraloc1.init;
        paraloc2.init;
        case tstringdef(left.resultdef).stringtype of
          { it's the same for ansi- and wide strings }
          st_unicodestring,
          st_widestring,
          st_ansistring:
            begin
              helpername:='fpc_'+tstringdef(left.resultdef).stringtypname+'_rangecheck';
              pd:=search_system_proc(helpername);
              paramanager.getintparaloc(pd,1,paraloc1);
              paramanager.getintparaloc(pd,2,paraloc2);
              if pd.is_pushleftright then
                begin
                  cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,left.location,paraloc1);
                  cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,right.location,paraloc2);
                end
              else
                begin
                  cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,right.location,paraloc2);
                  cg.a_load_loc_cgpara(current_asmdata.CurrAsmList,left.location,paraloc1);
                end;

              paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc1);
              paramanager.freecgpara(current_asmdata.CurrAsmList,paraloc2);
              cg.allocallcpuregisters(current_asmdata.CurrAsmList);
              cg.a_call_name(current_asmdata.CurrAsmList,helpername,false);
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
        paraloc1.done;
        paraloc2.done;
      end;

    procedure tcgvecnode.pass_generate_code;

      var
         offsetdec,
         extraoffset : aint;
         rightp      : pnode;
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
         indexdef : tdef;
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
            is_wide_or_unicode_string(left.resultdef) then
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
                    hlcg.reference_reset_base(location.reference,left.resultdef,left.location.register,0,location.reference.alignment);
{$endif m68k}
                  end;
                LOC_CREFERENCE,
                LOC_REFERENCE :
                  begin
                    hlcg.reference_reset_base(location.reference,left.resultdef,hlcg.getaddressregister(current_asmdata.CurrAsmList,left.resultdef),0,location.reference.alignment);
                    hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,left.location.reference,location.reference.base);
                  end;
                LOC_CONSTANT:
                  begin
                    hlcg.reference_reset_base(location.reference,left.resultdef,NR_NO,left.location.value,location.reference.alignment);
                  end;
                else
                  internalerror(2002032218);
              end;

              if is_ansistring(left.resultdef) then
                offsetdec:=1
              else
                offsetdec:=2;
              location.reference.alignment:=offsetdec;

              { in ansistrings/widestrings S[1] is p<w>char(S)[0] }
              if not(cs_zerobasedstrings in current_settings.localswitches) then
                update_reference_offset(location.reference,-1,offsetdec);
           end
         else if is_dynamic_array(left.resultdef) then
           begin
              case left.location.loc of
                LOC_REGISTER,
                LOC_CREGISTER :
                  hlcg.reference_reset_base(location.reference,left.resultdef,left.location.register,0,location.reference.alignment);
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                     hlcg.reference_reset_base(location.reference,left.resultdef,hlcg.getaddressregister(current_asmdata.CurrAsmList,left.resultdef),0,location.reference.alignment);
                     hlcg.a_load_ref_reg(current_asmdata.CurrAsmList,left.resultdef,left.resultdef,
                      left.location.reference,location.reference.base);
                  end;
                else
                  internalerror(2002032219);
              end;
              { a dynarray points to the start of a memory block, which
                we assume to be always aligned to a multiple of the
                pointer size
              }
              location.reference.alignment:=voidpointertype.size;
           end
         else
           begin
              { may happen in case of function results }
              case left.location.loc of
                LOC_CREGISTER,
                LOC_CMMREGISTER,
                LOC_REGISTER,
                LOC_MMREGISTER:
                  hlcg.location_force_mem(current_asmdata.CurrAsmList,left.location,left.resultdef);
              end;
             location_copy(location,left.location);
           end;

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
              not is_ordinal(resultdef)
{$ifndef cpu64bitalu}
              or is_64bitint(resultdef)
{$endif not cpu64bitalu}
              ) then
           update_reference_offset(location.reference,-tarraydef(left.resultdef).lowrange,bytemulsize);

         if right.nodetype=ordconstn then
           begin
              { offset can only differ from 0 if arraydef }
              if cs_check_range in current_settings.localswitches then
                begin
                  secondpass(right);
                  case left.resultdef.typ of
                    arraydef :
                      rangecheck_array;
                    stringdef :
                      rangecheck_string;
                  end;
                end;
              if not(is_packed_array(left.resultdef)) or
                 ((mulsize mod 8 = 0) and
                  (ispowerof2(mulsize div 8,temp) or
                   { only orddefs are bitpacked }
                   not is_ordinal(resultdef))) then
                begin
                  extraoffset:=tordconstnode(right).value.svalue;
                  update_reference_offset(location.reference,extraoffset,bytemulsize);
                  { adjust alignment after this change }
                  location.reference.alignment:=newalignment(location.reference.alignment,extraoffset*bytemulsize);
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
                  if not ispowerof2(packedbitsloadsize(resultdef.packedbitsize),temp) then
                    internalerror(2006081212);
                  alignpow:=temp;
                  update_reference_offset(subsetref.ref,(mulsize * (tordconstnode(right).value.svalue-tarraydef(left.resultdef).lowrange)) shr (3+alignpow),1 shl alignpow);
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
                   rightp:=actualtargetnode(@right);
                   if rightp^.nodetype=addn then
                     begin
                        if taddnode(rightp^).right.nodetype=ordconstn then
                          begin
                            extraoffset:=tordconstnode(taddnode(rightp^).right).value.svalue;
                            replacenode(rightp^,taddnode(rightp^).left);
                          end
                        else if taddnode(rightp^).left.nodetype=ordconstn then
                          begin
                            extraoffset:=tordconstnode(taddnode(rightp^).left).value.svalue;
                            replacenode(rightp^,taddnode(rightp^).right);
                          end;
                     end
                   else if rightp^.nodetype=subn then
                     begin
                        if taddnode(rightp^).right.nodetype=ordconstn then
                          begin
                            extraoffset:=-tordconstnode(taddnode(rightp^).right).value.svalue;
                            replacenode(rightp^,taddnode(rightp^).left);
                          end;
                     end;
                   update_reference_offset(location.reference,extraoffset,mulsize);
                end;
              { calculate from left to right }
              if not(location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                internalerror(200304237);
              isjump:=(right.expectloc=LOC_JUMP);
              otl:=nil;
              ofl:=nil;
              if isjump then
               begin
                 otl:=current_procinfo.CurrTrueLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrTrueLabel);
                 ofl:=current_procinfo.CurrFalseLabel;
                 current_asmdata.getjumplabel(current_procinfo.CurrFalseLabel);
               end;
              secondpass(right);

              { if mulsize = 1, we won't have to modify the index }
              if not(right.location.loc in [LOC_CREGISTER,LOC_REGISTER]) or
                 not valid_index_size(right.location.size) then
                begin
                  hlcg.location_force_reg(current_asmdata.CurrAsmList,right.location,right.resultdef,ptruinttype,true);
                  indexdef:=ptruinttype
                end
              else
                indexdef:=right.resultdef;

              if isjump then
               begin
                 current_procinfo.CurrTrueLabel:=otl;
                 current_procinfo.CurrFalseLabel:=ofl;
               end
              else if (right.location.loc = LOC_JUMP) then
                internalerror(2006010801);

            { produce possible range check code: }
              if cs_check_range in current_settings.localswitches then
               begin
                 if left.resultdef.typ=arraydef then
                   rangecheck_array
                 else if (left.resultdef.typ=stringdef) then
                   rangecheck_string;
               end;

              { insert the register and the multiplication factor in the
                reference }
              if not is_packed_array(left.resultdef) then
                update_reference_reg_mul(right.location.register,indexdef,mulsize)
              else
                update_reference_reg_packed(right.location.register,indexdef,mulsize);
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
