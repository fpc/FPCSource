{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for nodes that handle loads and assignments which
    are the same for all (most) processors

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
unit ncgld;

{$i fpcdefs.inc}

interface

    uses
      node,nld;

    type
       tcgloadnode = class(tloadnode)
          procedure pass_2;override;
       end;

       tcgassignmentnode = class(tassignmentnode)
          procedure pass_2;override;
       end;

       tcgfuncretnode = class(tfuncretnode)
          procedure pass_2;override;
       end;

       tcgarrayconstructornode = class(tarrayconstructornode)
          procedure pass_2;override;
       end;


implementation

    uses
      systems,
      verbose,globtype,globals,
      symconst,symtype,symdef,symsym,symtable,defbase,paramgr,
      ncnv,ncon,nmem,
      aasmbase,aasmtai,aasmcpu,regvars,
      cginfo,cgbase,pass_2,
      cpubase,cpuinfo,cpupara,
      tgobj,ncgutil,cgobj,cg64f32,rgobj,rgcpu;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure tcgloadnode.pass_2;
      var
        intreg,
        hregister : tregister;
        freereg   : boolean;
        symtabletype : tsymtabletype;
        i : longint;
        href : treference;
        newsize : tcgsize;
        pushed : tpushedsaved;
      begin
         { we don't know the size of all arrays }
         newsize:=def_cgsize(resulttype.def);
         location_reset(location,LOC_REFERENCE,newsize);
         case symtableentry.typ of
            absolutesym :
               begin
                  { this is only for toasm and toaddr }
                  if (tabsolutesym(symtableentry).abstyp=toaddr) then
                   begin
{$ifdef i386}
                     if tabsolutesym(symtableentry).absseg then
                      location.reference.segment:=R_FS;
{$endif i386}
                     location.reference.offset:=tabsolutesym(symtableentry).address;
                   end
                  else
                   location.reference.symbol:=objectlibrary.newasmsymbol(tabsolutesym(symtableentry).mangledname);
               end;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   begin
                      location_reset(location,LOC_CREFERENCE,OS_ADDR);
                      location.reference.symbol:=objectlibrary.newasmsymbol(tconstsym(symtableentry).owner.name^+'_RESOURCESTRINGLIST');
                      location.reference.offset:=tconstsym(symtableentry).resstrindex*16+8;
                   end
                 else
                   internalerror(22798);
              end;
            varsym :
               begin
                  symtabletype:=symtable.symtabletype;
                  hregister:=R_NO;
                  { C variable }
                  if (vo_is_C_var in tvarsym(symtableentry).varoptions) then
                    begin
                       location.reference.symbol:=objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname);
                    end
                  { DLL variable }
                  else if (vo_is_dll_var in tvarsym(symtableentry).varoptions) then
                    begin
                       hregister:=rg.getaddressregister(exprasmlist);
                       location.reference.symbol:=objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,location.reference,hregister);
                       reference_reset_base(location.reference,hregister,0);
                    end
                  { external variable }
                  else if (vo_is_external in tvarsym(symtableentry).varoptions) then
                    begin
                       location.reference.symbol:=objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname);
                    end
                  { thread variable }
                  else if (vo_is_thread_var in tvarsym(symtableentry).varoptions) then
                    begin
                       { we've to allocate the register before we save the used registers }
                       location.reference.base:=rg.getaddressregister(exprasmlist);
                       rg.saveusedregisters(exprasmlist,pushed,[accumulator]);
                       reference_reset_symbol(href,objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname),0);
                       cg.a_param_ref(exprasmlist,OS_ADDR,href,paramanager.getintparaloc(1));
                       { the called procedure isn't allowed to change }
                       { any register except EAX                    }
                       cg.a_call_name(exprasmlist,'FPC_RELOCATE_THREADVAR');
                       cg.a_load_reg_reg(exprasmlist,OS_INT,accumulator,location.reference.base);
                       rg.restoreusedregisters(exprasmlist,pushed);
                    end
                  { normal variable }
                  else
                    begin
                       { in case it is a register variable: }
                       if tvarsym(symtableentry).reg<>R_NO then
                         begin
                            if tvarsym(symtableentry).reg in fpuregs then
                              begin
                                 location_reset(location,LOC_CFPUREGISTER,def_cgsize(resulttype.def));
                                 location.register:=tvarsym(symtableentry).reg;
                              end
                            else
                             begin
                               intreg:=rg.makeregsize(tvarsym(symtableentry).reg,OS_INT);
                               if (intreg in general_registers) and
                                  (not rg.regvar_loaded[intreg]) then
                                 load_regvar(exprasmlist,tvarsym(symtableentry));
                               location_reset(location,LOC_CREGISTER,cg.reg_cgsize(tvarsym(symtableentry).reg));
                               location.register:=tvarsym(symtableentry).reg;
                               exclude(rg.unusedregsint,intreg);
                             end;
                         end
                       else
                         begin
                           case symtabletype of
                              localsymtable,
                              parasymtable,
                              inlinelocalsymtable,
                              inlineparasymtable :
                                begin
                                  location.reference.base:=procinfo.framepointer;
                                  if (symtabletype in [inlinelocalsymtable,
                                                       localsymtable])
{$ifdef powerpc}
                                    { the ifdef is only for speed reasons }
                                    and not(target_info.system in [system_powerpc_linux,system_powerpc_macos])
{$endif powerpc}
                                    then
                                    location.reference.offset:=
                                      tvarsym(symtableentry).address-symtable.address_fixup
                                  else
                                    location.reference.offset:=
                                      tvarsym(symtableentry).address+symtable.address_fixup;

{$ifndef powerpc}
                                  if (symtabletype in [localsymtable,inlinelocalsymtable]) then
                                    begin
                                       if use_esp_stackframe then
                                         dec(location.reference.offset,
                                           tvarsym(symtableentry).getvaluesize)
                                       else
                                         location.reference.offset:=-location.reference.offset;
                                    end;
{$endif powerpc}
                                  if (lexlevel>symtable.symtablelevel) then
                                    begin
                                       hregister:=rg.getaddressregister(exprasmlist);
                                       { make a reference }
                                       reference_reset_base(href,procinfo.framepointer,procinfo.framepointer_offset);
                                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                                       { walk parents }
                                       i:=lexlevel-1;
                                       while (i>symtable.symtablelevel) do
                                         begin
                                            { make a reference }
                                            reference_reset_base(href,hregister,target_info.first_parm_offset);
                                            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                                            dec(i);
                                         end;
                                       location.reference.base:=hregister;
                                    end;
                                end;
                              globalsymtable,
                              staticsymtable :
                                begin
                                  location.reference.symbol:=objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname);
                                end;
                              stt_exceptsymtable:
                                begin
                                   location.reference.base:=procinfo.framepointer;
                                   location.reference.offset:=tvarsym(symtableentry).address;
                                end;
                              objectsymtable:
                                begin
                                   if (sp_static in tvarsym(symtableentry).symoptions) then
                                     location.reference.symbol:=objectlibrary.newasmsymbol(tvarsym(symtableentry).mangledname)
                                   else
                                     begin
                                        rg.getexplicitregisterint(exprasmlist,SELF_POINTER_REG);
                                        location.reference.base:=SELF_POINTER_REG;
                                        location.reference.offset:=tvarsym(symtableentry).address;
                                     end;
                                end;
                              withsymtable:
                                begin
                                   if nf_islocal in tnode(twithsymtable(symtable).withnode).flags then
                                     location.reference:=twithnode(twithsymtable(symtable).withnode).withreference
                                   else
                                     begin
                                       location.reference.base:=rg.getaddressregister(exprasmlist);
                                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,
                                          twithnode(twithsymtable(symtable).withnode).withreference,
                                          location.reference.base);
                                     end;
                                   inc(location.reference.offset,tvarsym(symtableentry).address);
                                end;
                           end;
                         end;
                    end;

                  { handle call by reference variables }
                  if (symtabletype in [parasymtable,inlineparasymtable]) then
                    begin
                      { in case call by reference, then calculate. Open array
                        is always an reference! }
                      if (tvarsym(symtableentry).varspez in [vs_var,vs_out]) or
                         is_open_array(tvarsym(symtableentry).vartype.def) or
                         is_array_of_const(tvarsym(symtableentry).vartype.def) or
                         paramanager.push_addr_param(tvarsym(symtableentry).vartype.def,
                             (tprocdef(symtable.defowner).proccalloption in [pocall_cdecl,pocall_cppdecl])) then
                        begin
                           if hregister=R_NO then
                             hregister:=rg.getaddressregister(exprasmlist);
                           { we need to load only an address }
                           location.size:=OS_ADDR;
                           cg.a_load_loc_reg(exprasmlist,location,hregister);
                           location_reset(location,LOC_REFERENCE,newsize);
                           location.reference.base:=hregister;
                       end;
                    end;
               end;
            procsym:
               begin
                  if assigned(left) then
                    begin
                       {
                         THIS IS A TERRIBLE HACK!!!!!! WHICH WILL NOT WORK
                         ON 64-BIT SYSTEMS: SINCE PROCSYM FOR METHODS
                         CONSISTS OF TWO OS_ADDR, so you cannot set it
                         to OS_64 - how to solve?? Carl
                       }
                       if (sizeof(aword) = 4) then
                          location_reset(location,LOC_CREFERENCE,OS_64)
                       else
                          internalerror(20020520);
                       tg.GetTemp(exprasmlist,2*POINTER_SIZE,tt_normal,location.reference);
                       freereg:=false;

                       { called as type.method, then we only need to return
                         the address of the function, not the self pointer }
                       if left.nodetype=typen then
                        begin
                          { there is no instance, we return 0 }
                          href:=location.reference;
                          inc(href.offset,POINTER_SIZE);
                          cg.a_load_const_ref(exprasmlist,OS_ADDR,0,href);
                        end
                       else
                        begin
                          secondpass(left);

                          { load class instance address }
                          case left.location.loc of
                             LOC_CREGISTER,
                             LOC_REGISTER:
                               begin
                                  hregister:=left.location.register;
                                  if is_object(left.resulttype.def) then
                                    CGMessage(cg_e_illegal_expression);
                               end;
                             LOC_CREFERENCE,
                             LOC_REFERENCE:
                               begin
                                  hregister:=rg.getaddressregister(exprasmlist);
                                  if is_class_or_interface(left.resulttype.def) then
                                    cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,hregister)
                                  else
                                    cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,hregister);
                                  location_release(exprasmlist,left.location);
                                  location_freetemp(exprasmlist,left.location);
                               end;
                             else
                               internalerror(26019);
                          end;

                          { store the class instance address }
                          href:=location.reference;
                          inc(href.offset,POINTER_SIZE);
                          cg.a_load_reg_ref(exprasmlist,OS_ADDR,hregister,href);
                          { hregister will be reused when loading a virtual method }
                          freereg:=true;
                        end;

                       { virtual method ? }
                       if (po_virtualmethod in tprocdef(resulttype.def).procoptions) then
                         begin
                            if not freereg then
                              begin
                                if left.nodetype <> typen then
                                  internalerror(200205161);
                                reference_reset_symbol(href,objectlibrary.newasmsymbol(tobjectdef(left.resulttype.def).vmt_mangledname),
                                  tprocdef(resulttype.def)._class.vmtmethodoffset(tprocdef(resulttype.def).extnumber));
                              end
                            else
                              begin
                                { load vmt pointer }
                                reference_reset_base(href,hregister,0);
                                reference_release(exprasmlist,href);
                                hregister:=rg.getaddressregister(exprasmlist);
                                cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);


                                reference_reset_base(href,hregister,tprocdef(resulttype.def)._class.vmtmethodoffset(
                                                 tprocdef(resulttype.def).extnumber));
                                reference_release(exprasmlist,href);
                              end;

                            { load method address }
                            hregister:=rg.getaddressregister(exprasmlist);
                            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                            { ... and store it }
                            cg.a_load_reg_ref(exprasmlist,OS_ADDR,hregister,location.reference);
                            rg.ungetaddressregister(exprasmlist,hregister);
                         end
                       else
                         begin
                            { we don't use the hregister }
                            if freereg then
                             rg.ungetregister(exprasmlist,hregister);
                            { load address of the function }
                            reference_reset_symbol(href,objectlibrary.newasmsymbol(tprocdef(resulttype.def).mangledname),0);
                            hregister:=cg.get_scratch_reg_address(exprasmlist);
                            cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                            cg.a_load_reg_ref(exprasmlist,OS_ADDR,hregister,location.reference);
                            cg.free_scratch_reg(exprasmlist,hregister);
                         end;
                    end
                  else
                    begin
                       {!!!!! Be aware, work on virtual methods too }
                       location.reference.symbol:=objectlibrary.newasmsymbol(tprocdef(resulttype.def).mangledname);
                    end;
               end;
            typedconstsym :
               begin
                  location.reference.symbol:=objectlibrary.newasmsymbol(ttypedconstsym(symtableentry).mangledname);
               end;
            else internalerror(4);
         end;
      end;


{*****************************************************************************
                             SecondAssignment
*****************************************************************************}

    procedure tcgassignmentnode.pass_2;
      var
         otlabel,hlabel,oflabel : tasmlabel;
         fputyp : tfloattype;
         href : treference;
         releaseright : boolean;
         pushedregs : tmaybesave;
         cgsize : tcgsize;

      begin
        otlabel:=truelabel;
        oflabel:=falselabel;
        objectlibrary.getlabel(truelabel);
        objectlibrary.getlabel(falselabel);

        {
          in most cases we can process first the right node which contains
          the most complex code. But not when the result is in the flags, then
          loading the left node afterwards can destroy the flags.

          when the right node returns as LOC_JUMP then we will generate
          the following code:

          rightnode
          true:
            leftnode
            assign 1
          false:
            leftnode
            assign 0
        }

        { Try to determine which side to calculate first,  }
        if (right.location.loc<>LOC_FLAGS) and
           ((right.location.loc=LOC_JUMP) or
            (right.registers32>=left.registers32)) then
         begin
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            cg.g_incrrefcount(exprasmlist,right.resulttype.def,right.location.reference);
           if codegenerror then
             exit;

           { We skip the generation of the left node when it's a jump, see
             explanation above }
           if (right.location.loc<>LOC_JUMP) and
              not(nf_concat_string in flags) then
            begin
              { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
              { can be false                                             }
              maybe_save(exprasmlist,left.registers32,right.location,pushedregs);
              secondpass(left);
              { decrement destination reference counter }
              if (left.resulttype.def.needs_inittable) then
               cg.g_decrrefcount(exprasmlist,left.resulttype.def,left.location.reference);
              maybe_restore(exprasmlist,right.location,pushedregs);
              if codegenerror then
                exit;
            end;
         end
        else
         begin
           { calculate left sides }
           { don't do it yet if it's a crgister (JM) }
           if not(nf_concat_string in flags) then
            begin
              secondpass(left);
              { decrement destination reference counter }
              if (left.resulttype.def.needs_inittable) then
               cg.g_decrrefcount(exprasmlist,left.resulttype.def,left.location.reference);
              if codegenerror then
               exit;
            end;

           { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
           { can be false                                             }
           maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            cg.g_incrrefcount(exprasmlist,right.resulttype.def,right.location.reference);
           maybe_restore(exprasmlist,left.location,pushedregs);

           if codegenerror then
             exit;
         end;

        if not(left.location.loc in [LOC_REFERENCE,LOC_CFPUREGISTER,
                                     {$ifdef SUPPORT_MMX}LOC_CMMXREGISTER,{$endif}
                                     LOC_CREGISTER]) then
          begin
             CGMessage(cg_e_illegal_expression);
             exit;
          end;

        releaseright:=true;

        { shortstring assignments are handled separately }
        if is_shortstring(left.resulttype.def) then
          begin
            {
              we can get here only in the following situations
              for the right node:
               - empty constant string
               - char
            }

            { empty constant string }
            if (right.nodetype=stringconstn) and
               (tstringconstnode(right).len=0) then
              begin
                cg.a_load_const_ref(exprasmlist,OS_8,0,left.location.reference);
              end
            { char loading }
            else if is_char(right.resulttype.def) then
              begin
                if right.nodetype=ordconstn then
                  begin
                    if (target_info.endian = endian_little) then
                      cg.a_load_const_ref(exprasmlist,OS_16,(tordconstnode(right).value shl 8) or 1,
                          left.location.reference)
                    else
                      cg.a_load_const_ref(exprasmlist,OS_16,tordconstnode(right).value or (1 shl 8),
                          left.location.reference);
                  end
                else
                  begin
                    href:=left.location.reference;
                    cg.a_load_const_ref(exprasmlist,OS_8,1,href);
                    inc(href.offset,1);
                    case right.location.loc of
                      LOC_REGISTER,
                      LOC_CREGISTER :
                        cg.a_load_reg_ref(exprasmlist,OS_8,rg.makeregsize(right.location.register,OS_8),href);
                      LOC_REFERENCE,
                      LOC_CREFERENCE :
                        cg.a_load_ref_ref(exprasmlist,OS_8,right.location.reference,href);
                      else
                        internalerror(200205111);
                    end;
                  end;
              end
            else
              internalerror(200204249);
          end
        else
          begin
            case right.location.loc of
              LOC_CONSTANT :
                begin
                  if right.location.size in [OS_64,OS_S64] then
                   cg64.a_load64_const_loc(exprasmlist,
                       right.location.valueqword,left.location)
                  else
                   cg.a_load_const_loc(exprasmlist,right.location.value,left.location);
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  case left.location.loc of
                    LOC_CREGISTER :
                      begin
                        cgsize:=def_cgsize(left.resulttype.def);
                        if cgsize in [OS_64,OS_S64] then
                         cg64.a_load64_ref_reg(exprasmlist,
                             right.location.reference,left.location.register64)
                        else
                         cg.a_load_ref_reg(exprasmlist,cgsize,
                             right.location.reference,left.location.register);
                        location_release(exprasmlist,right.location);
                      end;
                    LOC_CFPUREGISTER :
                      begin
                        cg.a_loadfpu_ref_reg(exprasmlist,
                            def_cgsize(right.resulttype.def),
                            right.location.reference,
                            left.location.register);
                      end;
                    LOC_REFERENCE,
                    LOC_CREFERENCE :
                      begin
                        cg.g_concatcopy(exprasmlist,right.location.reference,
                                        left.location.reference,left.resulttype.def.size,true,false);
                        { right.location is already released by concatcopy }
                        releaseright:=false;
                      end;
                    else
                      internalerror(200203284);
                  end;
                end;
{$ifdef SUPPORT_MMX}
              LOC_CMMXREGISTER,
              LOC_MMXREGISTER:
                begin
                  if left.location.loc=LOC_CMMXREGISTER then
                    cg.a_loadmm_reg_reg(exprasmlist,right.location.register,left.location.register)
                  else
                    cg.a_loadmm_reg_ref(exprasmlist,right.location.register,left.location.reference);
                end;
{$endif SUPPORT_MMX}
              LOC_REGISTER,
              LOC_CREGISTER :
                begin
                  cgsize:=def_cgsize(left.resulttype.def);
                  if cgsize in [OS_64,OS_S64] then
                   cg64.a_load64_reg_loc(exprasmlist,
                     right.location.register64,left.location)
                  else
                   cg.a_load_reg_loc(exprasmlist,right.location.size,right.location.register,left.location);
                end;
              LOC_FPUREGISTER,LOC_CFPUREGISTER :
                begin
                  if (left.resulttype.def.deftype=floatdef) then
                   fputyp:=tfloatdef(left.resulttype.def).typ
                  else
                   if (right.resulttype.def.deftype=floatdef) then
                    fputyp:=tfloatdef(right.resulttype.def).typ
                  else
                   if (right.nodetype=typeconvn) and
                      (ttypeconvnode(right).left.resulttype.def.deftype=floatdef) then
                    fputyp:=tfloatdef(ttypeconvnode(right).left.resulttype.def).typ
                  else
                    fputyp:=s32real;
                  cg.a_loadfpu_reg_loc(exprasmlist,
                      tfloat2tcgsize[fputyp],
                      right.location.register,left.location);
                end;
              LOC_JUMP :
                begin
                  cgsize:=def_cgsize(left.resulttype.def);
                  objectlibrary.getlabel(hlabel);
                  { generate the leftnode for the true case, and
                    release the location }
                  cg.a_label(exprasmlist,truelabel);
                  maybe_save(exprasmlist,left.registers32,right.location,pushedregs);
                  secondpass(left);
                  maybe_restore(exprasmlist,right.location,pushedregs);
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,1,left.location);
                  location_release(exprasmlist,left.location);
                  cg.a_jmp_always(exprasmlist,hlabel);
                  { generate the leftnode for the false case }
                  cg.a_label(exprasmlist,falselabel);
                  maybe_save(exprasmlist,left.registers32,right.location,pushedregs);
                  secondpass(left);
                  maybe_restore(exprasmlist,right.location,pushedregs);
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,0,left.location);
                  cg.a_label(exprasmlist,hlabel);
                end;
              LOC_FLAGS :
                begin
                  // this can be a wordbool or longbool too, no?
                  if left.location.loc=LOC_CREGISTER then
                    cg.g_flags2reg(exprasmlist,def_cgsize(left.resulttype.def),right.location.resflags,left.location.register)
                  else
                    begin
                      if not(left.location.loc = LOC_REFERENCE) then
                       internalerror(200203273);
                      cg.g_flags2ref(exprasmlist,def_cgsize(left.resulttype.def),right.location.resflags,left.location.reference);
                    end;
                end;
            end;

         end;

        if releaseright then
         location_release(exprasmlist,right.location);
        location_release(exprasmlist,left.location);

        truelabel:=otlabel;
        falselabel:=oflabel;
      end;


{*****************************************************************************
                             SecondFuncRet
*****************************************************************************}

    procedure tcgfuncretnode.pass_2;
      var
         hreg : tregister;
         href : treference;
         pp : tprocinfo;
         hr_valid : boolean;
         i : integer;
      begin
         location_reset(location,LOC_REFERENCE,def_cgsize(resulttype.def));
         hr_valid:=false;
         if (not inlining_procedure) and
            (lexlevel<>funcretsym.owner.symtablelevel) then
           begin
              hreg:=rg.getaddressregister(exprasmlist);
              hr_valid:=true;
              reference_reset_base(href,procinfo.framepointer,procinfo.framepointer_offset);
              cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hreg);

              { walk up the stack frame }
              pp:=procinfo.parent;
              i:=lexlevel-1;
              while i>funcretsym.owner.symtablelevel do
               begin
                 reference_reset_base(href,hreg,pp.framepointer_offset);
                 cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hreg);
                 pp:=pp.parent;
                 dec(i);
               end;
              location.reference.base:=hreg;
              location.reference.offset:=pp.return_offset;
           end
         else
           begin
             location.reference.base:=procinfo.framepointer;
             location.reference.offset:=procinfo.return_offset;
           end;
         if paramanager.ret_in_param(resulttype.def) then
           begin
              { the parameter is actual a pointer to the value }
              if not hr_valid then
                hreg:=rg.getaddressregister(exprasmlist);
              cg.a_load_ref_reg(exprasmlist,OS_ADDR,location.reference,hreg);
              location.reference.base:=hreg;
              location.reference.offset:=0;
           end;
      end;
{*****************************************************************************
                           SecondArrayConstruct
*****************************************************************************}

      const
        vtInteger    = 0;
        vtBoolean    = 1;
        vtChar       = 2;
        vtExtended   = 3;
        vtString     = 4;
        vtPointer    = 5;
        vtPChar      = 6;
        vtObject     = 7;
        vtClass      = 8;
        vtWideChar   = 9;
        vtPWideChar  = 10;
        vtAnsiString = 11;
        vtCurrency   = 12;
        vtVariant    = 13;
        vtInterface  = 14;
        vtWideString = 15;
        vtInt64      = 16;
        vtQWord      = 17;

    procedure tcgarrayconstructornode.pass_2;
      var
        hp    : tarrayconstructornode;
        href  : treference;
        lt    : tdef;
        vaddr : boolean;
        vtype : longint;
        freetemp,
        dovariant : boolean;
        elesize : longint;
        tmpreg  : tregister;
      begin
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        if dovariant then
         elesize:=8
        else
         elesize:=tarraydef(resulttype.def).elesize;
        if not(nf_cargs in flags) then
         begin
           location_reset(location,LOC_REFERENCE,OS_NO);
           { Allocate always a temp, also if no elements are required, to
             be sure that location is valid (PFV) }
            if tarraydef(resulttype.def).highrange=-1 then
              tg.GetTemp(exprasmlist,elesize,tt_normal,location.reference)
            else
              tg.GetTemp(exprasmlist,(tarraydef(resulttype.def).highrange+1)*elesize,tt_normal,location.reference);
            href:=location.reference;
         end;
        hp:=self;
        while assigned(hp) do
         begin
           if assigned(hp.left) then
            begin
              freetemp:=true;
              secondpass(hp.left);
              if codegenerror then
               exit;
              if dovariant then
               begin
                 { find the correct vtype value }
                 vtype:=$ff;
                 vaddr:=false;
                 lt:=hp.left.resulttype.def;
                 case lt.deftype of
                   enumdef,
                   orddef :
                     begin
                       if is_64bitint(lt) then
                         begin
                            case torddef(lt).typ of
                               s64bit:
                                 vtype:=vtInt64;
                               u64bit:
                                 vtype:=vtQWord;
                            end;
                            freetemp:=false;
                            vaddr:=true;
                         end
                       else if (lt.deftype=enumdef) or
                         is_integer(lt) then
                         vtype:=vtInteger
                       else
                         if is_boolean(lt) then
                           vtype:=vtBoolean
                         else
                           if (lt.deftype=orddef) and (torddef(lt).typ=uchar) then
                             vtype:=vtChar;
                     end;
                   floatdef :
                     begin
                       vtype:=vtExtended;
                       vaddr:=true;
                       freetemp:=false;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         vtype:=vtPChar
                       else
                         vtype:=vtPointer;
                     end;
                   classrefdef :
                     vtype:=vtClass;
                   objectdef :
                     begin
                       vtype:=vtObject;
                     end;
                   stringdef :
                     begin
                       if is_shortstring(lt) then
                        begin
                          vtype:=vtString;
                          vaddr:=true;
                          freetemp:=false;
                        end
                       else
                        if is_ansistring(lt) then
                         begin
                           vtype:=vtAnsiString;
                           freetemp:=false;
                         end
                       else
                        if is_widestring(lt) then
                         begin
                           vtype:=vtWideString;
                           freetemp:=false;
                         end;
                     end;
                 end;
                 if vtype=$ff then
                   internalerror(14357);
                 { write C style pushes or an pascal array }
                 if nf_cargs in flags then
                  begin
                    if vaddr then
                     begin
                       location_force_mem(exprasmlist,hp.left.location);
                       cg.a_paramaddr_ref(exprasmlist,hp.left.location.reference,paralocdummy);
                       location_release(exprasmlist,hp.left.location);
                       if freetemp then
                        location_freetemp(exprasmlist,hp.left.location);
                     end
                    else
                     cg.a_param_loc(exprasmlist,hp.left.location,paralocdummy);
                    inc(pushedparasize,pointer_size);
                  end
                 else
                  begin
                    { write changing field update href to the next element }
                    inc(href.offset,4);
                    if vaddr then
                     begin
                       location_force_mem(exprasmlist,hp.left.location);
                       tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                       cg.a_loadaddr_ref_reg(exprasmlist,hp.left.location.reference,tmpreg);
                       cg.a_load_reg_ref(exprasmlist,cg.reg_cgsize(tmpreg),tmpreg,href);
                       cg.free_scratch_reg(exprasmlist,tmpreg);
                       location_release(exprasmlist,hp.left.location);
                       if freetemp then
                        location_freetemp(exprasmlist,hp.left.location);
                     end
                    else
                     begin
                       location_release(exprasmlist,left.location);
                       cg.a_load_loc_ref(exprasmlist,hp.left.location,href);
                     end;
                    { update href to the vtype field and write it }
                    dec(href.offset,4);
                    cg.a_load_const_ref(exprasmlist, OS_INT,vtype,href);
                    { goto next array element }
                    inc(href.offset,8);
                  end;
               end
              else
              { normal array constructor of the same type }
               begin
                 case elesize of
                   1,2,4 :
                     begin
                       location_release(exprasmlist,left.location);
                       cg.a_load_loc_ref(exprasmlist,hp.left.location,href);
                     end;
                   8 :
                     begin
                       if hp.left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        cg64.a_load64_loc_ref(exprasmlist,hp.left.location,href)
                       else
                        cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize,freetemp,false);
                     end;
                   else
                     begin
                       { concatcopy only supports reference }
                       if not(hp.left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                        internalerror(200108012);
                       cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize,freetemp,false);
                     end;
                 end;
                 inc(href.offset,elesize);
               end;
            end;
           { load next entry }
           hp:=tarrayconstructornode(hp.right);
         end;
      end;

begin
   cloadnode:=tcgloadnode;
   cassignmentnode:=tcgassignmentnode;
   cfuncretnode:=tcgfuncretnode;
   carrayconstructornode:=tcgarrayconstructornode;
end.
{
  $Log$
  Revision 1.28  2002-09-01 19:26:32  peter
    * fixed register variable loading from parasymtable, the call by
      reference code was moved wrong

  Revision 1.27  2002/09/01 12:15:40  peter
    * fixed loading of procvar of object when the object is initialized
      with 0

  Revision 1.26  2002/08/25 19:25:18  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.25  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.24  2002/08/17 09:23:35  florian
    * first part of procinfo rewrite

  Revision 1.23  2002/08/14 18:13:28  jonas
    * adapted previous fix to Peter's asmsymbol patch

  Revision 1.22  2002/08/14 18:00:42  jonas
    * fixed tb0403

  Revision 1.21  2002/08/13 21:40:56  florian
    * more fixes for ppc calling conventions

  Revision 1.20  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.19  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.18  2002/08/06 20:55:21  florian
    * first part of ppc calling conventions fix

  Revision 1.17  2002/07/28 09:25:37  carl
    + correct size of parameter (64-bit portability)

  Revision 1.16  2002/07/27 19:53:51  jonas
    + generic implementation of tcg.g_flags2ref()
    * tcg.flags2xxx() now also needs a size parameter

  Revision 1.15  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.14  2002/07/16 09:17:44  florian
    * threadvar relocation result wasn't handled properly, it could cause
      a crash

  Revision 1.13  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.12  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.11  2002/07/01 18:46:23  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.10  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.9  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.8  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.7  2002/05/18 11:17:03  peter
    * fixed internalerror due to releasing an not initialized register

  Revision 1.6  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.5  2002/05/14 19:34:42  peter
    * removed old logs and updated copyright year

  Revision 1.4  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.3  2002/05/12 16:53:07  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.2  2002/04/21 15:24:38  carl
  + a_jmp_cond -> a_jmp_always (a_jmp_cond is NOT portable)
  + changeregsize -> rg.makeregsize

  Revision 1.1  2002/04/19 15:39:34  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

}
