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

       tcgarrayconstructornode = class(tarrayconstructornode)
          procedure pass_2;override;
       end;


implementation

    uses
      systems,
      verbose,globtype,globals,
      symconst,symtype,symdef,symsym,symtable,defutil,paramgr,
      ncnv,ncon,nmem,
      aasmbase,aasmtai,aasmcpu,regvars,
      cginfo,cgbase,pass_2,
      cpubase,cpuinfo,cpupara,
      tgobj,ncgutil,cgobj,rgobj,rgcpu;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure tcgloadnode.pass_2;
      var
        r,hregister : tregister;
        supreg:Tsuperregister;
        symtabletype : tsymtabletype;
        i : longint;
        href : treference;
        newsize : tcgsize;
        pushed : tpushedsavedint;
        dorelocatelab,
        norelocatelab : tasmlabel;
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
                      location.reference.segment.enum:=R_FS;
{$endif i386}
                     location.reference.offset:=tabsolutesym(symtableentry).address;
                   end
                  else
                   location.reference.symbol:=objectlibrary.newasmsymboldata(tabsolutesym(symtableentry).mangledname);
               end;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   begin
                      location_reset(location,LOC_CREFERENCE,OS_ADDR);
                      location.reference.symbol:=objectlibrary.newasmsymboldata(tconstsym(symtableentry).owner.name^+'_RESOURCESTRINGLIST');
                      location.reference.offset:=tconstsym(symtableentry).resstrindex*16+8;
                   end
                 else
                   internalerror(22798);
              end;
            varsym :
               begin
                  if (tvarsym(symtableentry).varspez=vs_const) then
                    location_reset(location,LOC_CREFERENCE,newsize);
                  symtabletype:=symtable.symtabletype;
                  hregister.enum:=R_NO;
                  { C variable }
                  if (vo_is_C_var in tvarsym(symtableentry).varoptions) then
                    begin
                       location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                    end
                  { DLL variable }
                  else if (vo_is_dll_var in tvarsym(symtableentry).varoptions) then
                    begin
                       hregister:=rg.getaddressregister(exprasmlist);
                       location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,location.reference,hregister);
                       reference_reset_base(location.reference,hregister,0);
                    end
                  { external variable }
                  else if (vo_is_external in tvarsym(symtableentry).varoptions) then
                    begin
                       location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                    end
                  { thread variable }
                  else if (vo_is_thread_var in tvarsym(symtableentry).varoptions) then
                    begin
                       objectlibrary.getlabel(dorelocatelab);
                       objectlibrary.getlabel(norelocatelab);
                       { we've to allocate the register before we save the used registers }
                       hregister:=rg.getaddressregister(exprasmlist);
                       reference_reset_symbol(href,objectlibrary.newasmsymboldata('FPC_THREADVAR_RELOCATE'),0);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                       cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_NE,0,hregister,dorelocatelab);
                       { no relocation needed, load the address of the variable only, the
                         layout of a threadvar is (4 bytes pointer):
                           0 - Threadvar index
                           4 - Threadvar value in single threading }
                       reference_reset_symbol(href,objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname),POINTER_SIZE);
                       cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                       cg.a_jmp_always(exprasmlist,norelocatelab);
                       cg.a_label(exprasmlist,dorelocatelab);
                       if hregister.enum<>R_INTREGISTER then
                         internalerror(200301171);
                       { don't save the allocated register else the result will be destroyed later }
                       rg.saveusedintregisters(exprasmlist,pushed,[RS_ACCUMULATOR]-[hregister.number shr 8]);
                       reference_reset_symbol(href,objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname),0);
                       cg.a_param_ref(exprasmlist,OS_ADDR,href,paramanager.getintparaloc(1));
                       { the called procedure isn't allowed to change }
                       { any register except EAX                    }
                       cg.a_call_reg(exprasmlist,hregister);
                       r.enum:=R_INTREGISTER;
                       r.number:=NR_ACCUMULATOR;
                       cg.a_load_reg_reg(exprasmlist,OS_INT,OS_ADDR,r,hregister);
                       rg.restoreusedintregisters(exprasmlist,pushed);
                       cg.a_label(exprasmlist,norelocatelab);
                       location.reference.base:=hregister;
                    end
                  { normal variable }
                  else
                    begin
                       { in case it is a register variable: }
                       if tvarsym(symtableentry).reg.enum<>R_NO then
                         begin
                            if tvarsym(symtableentry).reg.enum in fpuregs then
                              begin
                                 location_reset(location,LOC_CFPUREGISTER,def_cgsize(resulttype.def));
                                 location.register:=tvarsym(symtableentry).reg;
                              end
                            else if Tvarsym(symtableentry).reg.enum=R_INTREGISTER then
                             begin
                               supreg:=Tvarsym(symtableentry).reg.number shr 8;
                               if (supreg in general_superregisters) and
                                  not (supreg in rg.regvar_loaded_int) then
                                 load_regvar(exprasmlist,tvarsym(symtableentry));
                               location_reset(location,LOC_CREGISTER,cg.reg_cgsize(tvarsym(symtableentry).reg));
                               location.register:=tvarsym(symtableentry).reg;
                               exclude(rg.unusedregsint,supreg);
                             end
                           else
                             internalerror(200301172);
                         end
                       else
                         begin
                           case symtabletype of
                              localsymtable,
                              parasymtable,
                              inlinelocalsymtable,
                              inlineparasymtable :
                                begin
                                  location.reference.base:=current_procinfo.framepointer;
                                  location.reference.offset:=tvarsym(symtableentry).adjusted_address;

                                  if (current_procdef.parast.symtablelevel>symtable.symtablelevel) then
                                    begin
                                       hregister:=rg.getaddressregister(exprasmlist);
                                       { make a reference }
                                       reference_reset_base(href,current_procinfo.framepointer,current_procinfo.framepointer_offset);
                                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                                       { walk parents }
                                       i:=current_procdef.parast.symtablelevel-1;
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
                                  location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                                end;
                              stt_exceptsymtable:
                                begin
                                   location.reference.base:=current_procinfo.framepointer;
                                   location.reference.offset:=tvarsym(symtableentry).address;
                                end;
                              else
                                internalerror(200305102);
                           end;
                         end;
                    end;

                  { handle call by reference variables, ignore the reference
                    when we need to load the self pointer for objects }
                  if (symtabletype in [parasymtable,inlineparasymtable]) and
                     not(nf_load_self_pointer in flags) and
                     (
                      (tvarsym(symtableentry).varspez in [vs_var,vs_out]) or
                      paramanager.push_addr_param(tvarsym(symtableentry).vartype.def,tprocdef(symtable.defowner).proccalloption)
                     ) then
                    begin
                      if hregister.enum=R_NO then
                        hregister:=rg.getaddressregister(exprasmlist);
                      { we need to load only an address }
                      location.size:=OS_ADDR;
                      cg.a_load_loc_reg(exprasmlist,location,hregister);
                      if tvarsym(symtableentry).varspez=vs_const then
                       location_reset(location,LOC_CREFERENCE,newsize)
                      else
                       location_reset(location,LOC_REFERENCE,newsize);
                      location.reference.base:=hregister;
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
                      secondpass(left);

                      { load class instance address }
                      case left.location.loc of
                         LOC_CREGISTER,
                         LOC_REGISTER:
                           begin
                              { this is not possible for objects }
                              if is_object(left.resulttype.def) then
                                internalerror(200304234);
                              hregister:=left.location.register;
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

                      { virtual method ? }
                      if (po_virtualmethod in procdef.procoptions) then
                        begin
                          { load vmt pointer }
                          reference_reset_base(href,hregister,0);
                          reference_release(exprasmlist,href);
                          hregister:=rg.getaddressregister(exprasmlist);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);


                          reference_reset_base(href,hregister,
                              procdef._class.vmtmethodoffset(procdef.extnumber));
                          reference_release(exprasmlist,href);

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
                          rg.ungetregisterint(exprasmlist,hregister);
                          { load address of the function }
                          reference_reset_symbol(href,objectlibrary.newasmsymbol(procdef.mangledname),0);
                        {$ifdef newra}
                          hregister:=rg.getaddressregister(exprasmlist);
                        {$else}
                          hregister:=cg.get_scratch_reg_address(exprasmlist);
                        {$endif}
                          cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                          cg.a_load_reg_ref(exprasmlist,OS_ADDR,hregister,location.reference);
                        {$ifdef newra}
                          rg.ungetregisterint(exprasmlist,hregister);
                        {$else newra}
                          cg.free_scratch_reg(exprasmlist,hregister);
                        {$endif}
                        end;
                    end
                  else
                    begin
                       {!!!!! Be aware, work on virtual methods too }
                       location.reference.symbol:=objectlibrary.newasmsymbol(procdef.mangledname);
                    end;
               end;
            typedconstsym :
               begin
                  location.reference.symbol:=objectlibrary.newasmsymboldata(ttypedconstsym(symtableentry).mangledname);
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
         old_allow_multi_pass2,
         releaseright : boolean;
         pushedregs : tmaybesave;
         cgsize : tcgsize;
         r:Tregister;

      begin
        location_reset(location,LOC_VOID,OS_NO);

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
        if (right.expectloc<>LOC_FLAGS) and
           ((right.expectloc=LOC_JUMP) or
            (right.nodetype=calln) or
            (right.registers32>=left.registers32)) then
         begin
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            cg.g_incrrefcount(exprasmlist,right.resulttype.def,right.location.reference,false);
           if codegenerror then
             exit;

           { We skip the generation of the left node when it's a jump, see
             explanation above }
           if (right.location.loc<>LOC_JUMP) and
              not(nf_concat_string in flags) then
            begin
              { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
              { can be false                                             }
            {$ifndef newra}
              maybe_save(exprasmlist,left.registers32,right.location,pushedregs);
            {$endif}
              secondpass(left);
              { decrement destination reference counter }
              if (left.resulttype.def.needs_inittable) then
               cg.g_decrrefcount(exprasmlist,left.resulttype.def,left.location.reference,false);
            {$ifndef newra}
              maybe_restore(exprasmlist,right.location,pushedregs);
            {$endif newra}
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
               cg.g_decrrefcount(exprasmlist,left.resulttype.def,left.location.reference,false);
              if codegenerror then
               exit;
            end;

           { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
           { can be false                                             }
          {$ifndef newra}
           maybe_save(exprasmlist,right.registers32,left.location,pushedregs);
          {$endif newra}
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            cg.g_incrrefcount(exprasmlist,right.resulttype.def,right.location.reference,false);
          {$ifndef newra}
           maybe_restore(exprasmlist,left.location,pushedregs);
          {$endif}

           if codegenerror then
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
                        begin
                          r.enum:=R_INTREGISTER;
                          r.number:=(right.location.register.number and not $ff) or R_SUBL;
                          cg.a_load_reg_ref(exprasmlist,OS_8,r,href);
                        end;
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
                {$ifndef newra}
                  maybe_save(exprasmlist,left.registers32,right.location,pushedregs);
                {$endif newra}
                  secondpass(left);
                {$ifndef newra}
                  maybe_restore(exprasmlist,right.location,pushedregs);
                {$endif newra}
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,1,left.location);
                  location_release(exprasmlist,left.location);
                  cg.a_jmp_always(exprasmlist,hlabel);
                  { generate the leftnode for the false case }
                  cg.a_label(exprasmlist,falselabel);
                {$ifndef newra}
                  maybe_save(exprasmlist,left.registers32,right.location,pushedregs);
                {$endif}
                  old_allow_multi_pass2:=allow_multi_pass2;
                  allow_multi_pass2:=true;
                  secondpass(left);
                  allow_multi_pass2:=old_allow_multi_pass2;
                {$ifndef newra}
                  maybe_restore(exprasmlist,right.location,pushedregs);
                {$endif newra}
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,0,left.location);
                  cg.a_label(exprasmlist,hlabel);
                end;
{$ifdef cpuflags}
              LOC_FLAGS :
                begin
                  {This can be a wordbool or longbool too, no?}
                  if left.location.loc=LOC_CREGISTER then
                    cg.g_flags2reg(exprasmlist,def_cgsize(left.resulttype.def),right.location.resflags,left.location.register)
                  else
                    begin
                      if not(left.location.loc = LOC_REFERENCE) then
                       internalerror(200203273);
                      cg.g_flags2ref(exprasmlist,def_cgsize(left.resulttype.def),right.location.resflags,left.location.reference);
                    end;
                end;
{$endif cpuflags}
            end;

         end;

        if releaseright then
         location_release(exprasmlist,right.location);
        location_release(exprasmlist,left.location);

        truelabel:=otlabel;
        falselabel:=oflabel;
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
        if nf_cargs in flags then
         location_reset(location,LOC_VOID,OS_NO)
        else
         location_reset(location,LOC_CREFERENCE,OS_NO);
        if not(nf_cargs in flags) then
         begin
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
                       if is_64bit(lt) then
                         begin
                            case torddef(lt).typ of
                               s64bit:
                                 vtype:=vtInt64;
                               u64bit:
                                 vtype:=vtQWord;
                            end;
                           if not(nf_cargs in flags) then
                            begin
                              freetemp:=false;
                              vaddr:=true;
                            end;
                         end
                       else if (lt.deftype=enumdef) or
                         is_integer(lt) then
                         vtype:=vtInteger
                       else
                         if is_boolean(lt) then
                           vtype:=vtBoolean
                         else
                           if (lt.deftype=orddef) then
                             begin
                               case torddef(lt).typ of
                                 uchar:
                                   vtype:=vtChar;
                                 uwidechar:
                                   vtype:=vtWideChar;
                               end;
                             end;
                     end;
                   floatdef :
                     begin
                       vtype:=vtExtended;
                       if not(nf_cargs in flags) then
                        begin
                          freetemp:=false;
                          vaddr:=true;
                        end;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         vtype:=vtPChar
                       else
                         vtype:=vtPointer;
                     end;
                   variantdef :
                     begin
                        vtype:=vtVariant;
                        vaddr:=true;
                        freetemp:=false;
                     end;
                   classrefdef :
                     vtype:=vtClass;
                   objectdef :
                     vtype:=vtObject;
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
                       inc(pushedparasize,pointer_size);
                     end
                    else
                      if vtype in [vtInt64,vtQword,vtExtended] then
                        push_value_para(exprasmlist,hp.left,pocall_cdecl,0,4,paralocdummy)
                    else
                      begin
                        cg.a_param_loc(exprasmlist,hp.left.location,paralocdummy);
                        inc(pushedparasize,pointer_size);
                      end;
                  end
                 else
                  begin
                    { write changing field update href to the next element }
                    inc(href.offset,4);
                    if vaddr then
                     begin
                       location_force_mem(exprasmlist,hp.left.location);
                     {$ifdef newra}
                       tmpreg:=rg.getaddressregister(exprasmlist);
                     {$else}
                       tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                     {$endif}
                       cg.a_loadaddr_ref_reg(exprasmlist,hp.left.location.reference,tmpreg);
                       cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                     {$ifdef newra}
                       rg.ungetregisterint(exprasmlist,tmpreg);
                     {$else}
                       cg.free_scratch_reg(exprasmlist,tmpreg);
                     {$endif}
                       location_release(exprasmlist,hp.left.location);
                       if freetemp then
                         location_freetemp(exprasmlist,hp.left.location);
                     end
                    else
                     begin
                       location_release(exprasmlist,hp.left.location);
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
                 if is_ansistring(left.resulttype.def) or
                    is_widestring(left.resulttype.def) or
                    (left.resulttype.def.deftype=variantdef) then
                   freetemp:=false;
                 location_release(exprasmlist,hp.left.location);
                 case hp.left.location.loc of
                   LOC_FPUREGISTER,
                   LOC_CFPUREGISTER :
                     begin
                       location_release(exprasmlist,hp.left.location);
                       cg.a_loadfpu_reg_ref(exprasmlist,hp.left.location.size,hp.left.location.register,href);
                     end;
                   LOC_REFERENCE,
                   LOC_CREFERENCE :
                     begin
                       cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize,freetemp,false);
                     end;
                   else
                     begin
                       if hp.left.location.size in [OS_64,OS_S64] then
                         cg64.a_load64_loc_ref(exprasmlist,hp.left.location,href)
                       else
                         cg.a_load_loc_ref(exprasmlist,hp.left.location,href);
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
   carrayconstructornode:=tcgarrayconstructornode;
end.
{
  $Log$
  Revision 1.59  2003-05-15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.58  2003/05/12 17:22:00  jonas
    * fixed (last?) remaining -tvarsym(X).address to
      tg.direction*tvarsym(X).address...

  Revision 1.57  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.56  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.55  2003/04/29 07:29:14  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.54  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.53  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.52  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.51  2003/04/23 20:16:04  peter
    + added currency support based on int64
    + is_64bit for use in cg units instead of is_64bitint
    * removed cgmessage from n386add, replace with internalerrors

  Revision 1.50  2003/04/23 10:12:14  peter
    * allow multi pass2 changed to global boolean instead of node flag

  Revision 1.49  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.48  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.47  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.46  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.45  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.44  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.43  2003/01/05 22:44:14  peter
    * remove a lot of code to support typen in loadn-procsym

  Revision 1.42  2002/12/20 18:13:46  peter
    * fixes for fpu values in arrayconstructor

  Revision 1.41  2002/11/27 20:04:39  peter
    * cdecl array of const fixes

  Revision 1.40  2002/11/25 17:43:18  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.39  2002/11/22 16:22:45  jonas
    * fixed error in my previous commit (the size of the location of the
      funcretnode must be based on the current resulttype of the node and not
      the resulttype defined by the function; these can be different in case
      of "absolute" declarations)

  Revision 1.38  2002/11/18 17:31:54  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.37  2002/11/15 21:16:39  jonas
    * proper fix for tw2110, also fixes tb0416 (funcretnode of parent
      function was handled wrong inside nested functions/procedures)

  Revision 1.36  2002/11/15 01:58:51  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.35  2002/10/14 19:44:13  peter
    * (hacked) new threadvar relocate code

  Revision 1.34  2002/10/13 11:22:06  florian
    * fixed threadvars

  Revision 1.33  2002/10/03 21:32:02  carl
    * bugfix for 2110 (without -Or), wrong checking was done in returntype

  Revision 1.32  2002/09/30 07:00:46  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.31  2002/09/26 15:02:05  florian
    + support of passing variants to "array of const"

  Revision 1.30  2002/09/17 18:54:02  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.29  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.28  2002/09/01 19:26:32  peter
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

}
