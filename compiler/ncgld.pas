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
      cutils,
      systems,
      verbose,globtype,globals,
      symconst,symtype,symdef,symsym,defutil,paramgr,
      ncnv,ncon,nmem,nbas,
      aasmbase,aasmtai,
      cgbase,pass_2,
      cpubase,cpuinfo,
      tgobj,ncgutil,cgobj,ncgbas;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure tcgloadnode.pass_2;
      var
        r,hregister : tregister;
        supreg:Tsuperregister;
        symtabletype : tsymtabletype;
        href : treference;
        newsize : tcgsize;
        endrelocatelab,
        norelocatelab : tasmlabel;
        paraloc1 : tparalocation;
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
                      location.reference.segment:=NR_FS;
{$endif i386}
                     location.reference.offset:=tabsolutesym(symtableentry).fieldoffset;
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
                  hregister:=NR_NO;
                  { C variable }
                  if (vo_is_C_var in tvarsym(symtableentry).varoptions) then
                    begin
                       location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                    end
                  { DLL variable }
                  else if (vo_is_dll_var in tvarsym(symtableentry).varoptions) then
                    begin
                       hregister:=cg.getaddressregister(exprasmlist);
                       location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,location.reference,hregister);
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
                       {
                         Thread var loading is optimized to first check if
                         a relocate function is available. When the function
                         is available it is called to retrieve the address.
                         Otherwise the address is loaded with the symbol

                         The code needs to be in the order to first handle the
                         call and then the address load to be sure that the
                         register that is used for returning is the same (PFV)
                       }
                       objectlibrary.getlabel(norelocatelab);
                       objectlibrary.getlabel(endrelocatelab);
                       { make sure hregister can't allocate the register necessary for the parameter }
                       paraloc1:=paramanager.getintparaloc(pocall_default,1);
                       hregister:=cg.getaddressregister(exprasmlist);
                       reference_reset_symbol(href,objectlibrary.newasmsymboldata('FPC_THREADVAR_RELOCATE'),0);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,hregister);
                       cg.ungetregister(exprasmlist,hregister);
                       cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,hregister,norelocatelab);
                       { don't save the allocated register else the result will be destroyed later }
                       reference_reset_symbol(href,objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname),0);
                       paramanager.allocparaloc(exprasmlist,paraloc1);
                       cg.a_param_ref(exprasmlist,OS_ADDR,href,paraloc1);
                       paramanager.freeparaloc(exprasmlist,paraloc1);
                       r:=cg.getabtintregister(exprasmlist,OS_ADDR);
                       cg.ungetregister(exprasmlist,r);
                       cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,hregister,r);
                       cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                       cg.a_call_reg(exprasmlist,r);
                       cg.deallocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
                       cg.getexplicitregister(exprasmlist,NR_FUNCTION_RESULT_REG);
                       cg.ungetregister(exprasmlist,NR_FUNCTION_RESULT_REG);
                       hregister:=cg.getaddressregister(exprasmlist);
                       cg.a_load_reg_reg(exprasmlist,OS_INT,OS_ADDR,r,hregister);
                       cg.a_jmp_always(exprasmlist,endrelocatelab);
                       cg.a_label(exprasmlist,norelocatelab);
                       { no relocation needed, load the address of the variable only, the
                         layout of a threadvar is (4 bytes pointer):
                           0 - Threadvar index
                           4 - Threadvar value in single threading }
                       reference_reset_symbol(href,objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname),POINTER_SIZE);
                       cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                       cg.a_label(exprasmlist,endrelocatelab);
                       location.reference.base:=hregister;
                    end
                  { nested variable }
                  else if assigned(left) then
                    begin
                      if not(symtabletype in [localsymtable,parasymtable]) then
                        internalerror(200309285);
                      secondpass(left);
                      if left.location.loc<>LOC_REGISTER then
                        internalerror(200309286);
                      hregister:=left.location.register;
                      location.reference.base:=hregister;
                      location.reference.offset:=tvarsym(symtableentry).localloc.reference.offset;
                    end
                  { normal variable }
                  else
                    begin
                       {$warning fixme regvars}
                       { in case it is a register variable: }
{                       if tvarsym(symtableentry).localloc.loc=LOC_REGISTER then
                         begin
                            case getregtype(tvarsym(symtableentry).localloc.register) of
                              R_FPUREGISTER :
                                begin
                                   location_reset(location,LOC_CFPUREGISTER,def_cgsize(resulttype.def));
                                   location.register:=tvarsym(symtableentry).localloc.register;
                                end;
                              R_INTREGISTER :
                                begin
                                  supreg:=getsupreg(Tvarsym(symtableentry).localloc.register);
                                  if (supreg in general_superregisters) and
                                     not (supreg in rg.regvar_loaded_int) then
                                    load_regvar(exprasmlist,tvarsym(symtableentry));
                                  location_reset(location,LOC_CREGISTER,def_cgsize(resulttype.def));
                                  location.register:=tvarsym(symtableentry).localloc.register;
                                  exclude(rg.unusedregsint,supreg);
                                  hregister := location.register;
                                end;
                              else
                                internalerror(200301172);
                            end;
                         end
                       else}
                         begin
                           case symtabletype of
                              localsymtable,
                              parasymtable :
                                begin
                                  if tvarsym(symtableentry).localloc.loc<>LOC_REFERENCE then
                                    internalerror(2003091816);
                                  location.reference.base:=tvarsym(symtableentry).localloc.reference.index;
                                  location.reference.offset:=tvarsym(symtableentry).localloc.reference.offset;
                                end;
                              globalsymtable,
                              staticsymtable :
                                begin
                                  location.reference.symbol:=objectlibrary.newasmsymboldata(tvarsym(symtableentry).mangledname);
                                end;
                              stt_exceptsymtable:
                                begin
                                  if tvarsym(symtableentry).localloc.loc<>LOC_REFERENCE then
                                    internalerror(2003091817);
                                  location.reference.base:=tvarsym(symtableentry).localloc.reference.index;
                                  location.reference.offset:=tvarsym(symtableentry).localloc.reference.offset;
                                end;
                              else
                                internalerror(200305102);
                           end;
                         end;
                    end;

                  { handle call by reference variables when they are not
                    alreayd copied to local copies. Also ignore the reference
                    when we need to load the self pointer for objects }
                  if (symtabletype=parasymtable) and
                     not(vo_has_local_copy in tvarsym(symtableentry).varoptions) and
                     not(nf_load_self_pointer in flags) and
                     paramanager.push_addr_param(tvarsym(symtableentry).varspez,tvarsym(symtableentry).vartype.def,tprocdef(symtable.defowner).proccalloption) then
                    begin
                      if hregister=NR_NO then
                        hregister:=cg.getaddressregister(exprasmlist);
                      { we need to load only an address }
                      location.size:=OS_ADDR;
                      cg.a_load_loc_reg(exprasmlist,location.size,location,hregister);
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
                              location_release(exprasmlist,left.location);
                              hregister:=cg.getaddressregister(exprasmlist);
                              if is_class_or_interface(left.resulttype.def) then
                                cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,left.location.reference,hregister)
                              else
                                cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,hregister);
                              location_freetemp(exprasmlist,left.location);
                           end;
                         else
                           internalerror(26019);
                      end;

                      { store the class instance address }
                      href:=location.reference;
                      inc(href.offset,POINTER_SIZE);
                      cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,href);

                      { virtual method ? }
                      if (po_virtualmethod in procdef.procoptions) then
                        begin
                          { load vmt pointer }
                          reference_reset_base(href,hregister,0);
                          reference_release(exprasmlist,href);
                          hregister:=cg.getaddressregister(exprasmlist);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,hregister);


                          reference_reset_base(href,hregister,
                              procdef._class.vmtmethodoffset(procdef.extnumber));
                          reference_release(exprasmlist,href);

                          { load method address }
                          hregister:=cg.getaddressregister(exprasmlist);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,hregister);
                          { ... and store it }
                          cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
                          cg.ungetregister(exprasmlist,hregister);
                        end
                      else
                        begin
                          { we don't use the hregister }
                          cg.ungetregister(exprasmlist,hregister);
                          { load address of the function }
                          reference_reset_symbol(href,objectlibrary.newasmsymbol(procdef.mangledname),0);
                          hregister:=cg.getaddressregister(exprasmlist);
                          cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                          cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
                          cg.ungetregister(exprasmlist,hregister);
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
              secondpass(left);
              { decrement destination reference counter }
              if (left.resulttype.def.needs_inittable) then
               cg.g_decrrefcount(exprasmlist,left.resulttype.def,left.location.reference,false);
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
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            cg.g_incrrefcount(exprasmlist,right.resulttype.def,right.location.reference,false);

           if codegenerror then
             exit;
         end;

        releaseright:=true;

        { optimize temp to temp copies }
        if (left.nodetype = temprefn) and
           { we may store certain temps in registers in the future, then this }
           { optimization will have to be adapted                             }
           (left.location.loc = LOC_REFERENCE) and
           (right.location.loc = LOC_REFERENCE) and
           tg.istemp(right.location.reference) and
           (tg.sizeoftemp(exprasmlist,right.location.reference) = tg.sizeoftemp(exprasmlist,left.location.reference)) then
          begin
            { in theory, we should also make sure the left temp type is   }
            { already more or less of the same kind (ie. we must not      }
            { assign an ansistring to a normaltemp). In practice, the     }
            { assignment node will have already taken care of this for us }
            tcgtemprefnode(left).changelocation(right.location.reference);
          end
        { shortstring assignments are handled separately }
        else if is_shortstring(left.resulttype.def) then
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
                          r:=cg.makeregsize(right.location.register,OS_8);
                          cg.a_load_reg_ref(exprasmlist,OS_8,OS_8,r,href);
                        end;
                      LOC_REFERENCE,
                      LOC_CREFERENCE :
                        cg.a_load_ref_ref(exprasmlist,OS_8,OS_8,right.location.reference,href);
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
                          begin
                            cg64.a_load64_ref_reg(exprasmlist,
                               right.location.reference,left.location.register64,false);
                            location_release(exprasmlist,right.location);
                          end
                        else
                          begin
                            location_release(exprasmlist,right.location);
                            cg.a_load_ref_reg(exprasmlist,cgsize,cgsize,
                                right.location.reference,left.location.register);
                          end;
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
                  secondpass(left);
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,1,left.location);
                  location_release(exprasmlist,left.location);
                  cg.a_jmp_always(exprasmlist,hlabel);
                  { generate the leftnode for the false case }
                  cg.a_label(exprasmlist,falselabel);
                  old_allow_multi_pass2:=allow_multi_pass2;
                  allow_multi_pass2:=true;
                  secondpass(left);
                  allow_multi_pass2:=old_allow_multi_pass2;
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
        paraloc : tparalocation;
      begin
        if nf_cargs in flags then
          internalerror(200310054);
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        if dovariant then
         elesize:=8
        else
         elesize:=tarraydef(resulttype.def).elesize;
        location_reset(location,LOC_CREFERENCE,OS_NO);
        fillchar(paraloc,sizeof(paraloc),0);
        { Allocate always a temp, also if no elements are required, to
          be sure that location is valid (PFV) }
         if tarraydef(resulttype.def).highrange=-1 then
           tg.GetTemp(exprasmlist,elesize,tt_normal,location.reference)
         else
           tg.GetTemp(exprasmlist,(tarraydef(resulttype.def).highrange+1)*elesize,tt_normal,location.reference);
         href:=location.reference;
        { Process nodes in array constructor }
        hp:=self;
        while assigned(hp) do
         begin
           if assigned(hp.left) then
            begin
              freetemp:=true;
              secondpass(hp.left);
              if codegenerror then
               exit;
              { Move flags and jump in register }
              if hp.left.location.loc in [LOC_FLAGS,LOC_JUMP] then
                location_force_reg(exprasmlist,hp.left.location,def_cgsize(hp.left.resulttype.def),false);
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
                       freetemp:=false;
                       vaddr:=true;
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
                 { write changing field update href to the next element }
                 inc(href.offset,4);
                 if vaddr then
                  begin
                    location_force_mem(exprasmlist,hp.left.location);
                    location_release(exprasmlist,hp.left.location);
                    tmpreg:=cg.getaddressregister(exprasmlist);
                    cg.a_loadaddr_ref_reg(exprasmlist,hp.left.location.reference,tmpreg);
                    cg.ungetregister(exprasmlist,tmpreg);
                    cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,tmpreg,href);
                    if freetemp then
                      location_freetemp(exprasmlist,hp.left.location);
                  end
                 else
                  begin
                    location_release(exprasmlist,hp.left.location);
                    cg.a_load_loc_ref(exprasmlist,OS_ADDR,hp.left.location,href);
                  end;
                 { update href to the vtype field and write it }
                 dec(href.offset,4);
                 cg.a_load_const_ref(exprasmlist, OS_INT,vtype,href);
                 { goto next array element }
                 inc(href.offset,8);
               end
              else
              { normal array constructor of the same type }
               begin
                 if is_ansistring(left.resulttype.def) or
                    is_widestring(left.resulttype.def) or
                    (left.resulttype.def.deftype=variantdef) then
                   freetemp:=false;
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
                       location_release(exprasmlist,hp.left.location);
                       cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize,freetemp,false);
                     end;
                   else
                     begin
                       if hp.left.location.size in [OS_64,OS_S64] then
                         begin
                           cg64.a_load64_loc_ref(exprasmlist,hp.left.location,href);
                           location_release(exprasmlist,hp.left.location);
                         end
                       else
                         begin
                           location_release(exprasmlist,hp.left.location);
                           cg.a_load_loc_ref(exprasmlist,hp.left.location.size,hp.left.location,href);
                         end;
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
  Revision 1.94  2003-10-11 16:06:42  florian
    * fixed some MMX<->SSE
    * started to fix ppc, needs an overhaul
    + stabs info improve for spilling, not sure if it works correctly/completly
    - MMX_SUPPORT removed from Makefile.fpc

  Revision 1.93  2003/10/10 17:48:13  peter
    * old trgobj moved to x86/rgcpu and renamed to trgx86fpu
    * tregisteralloctor renamed to trgobj
    * removed rgobj from a lot of units
    * moved location_* and reference_* to cgobj
    * first things for mmx register allocation

  Revision 1.92  2003/10/09 21:31:37  daniel
    * Register allocator splitted, ans abstract now

  Revision 1.91  2003/10/07 15:17:07  peter
    * inline supported again, LOC_REFERENCEs are used to pass the
      parameters
    * inlineparasymtable,inlinelocalsymtable removed
    * exitlabel inserting fixed

  Revision 1.90  2003/10/05 21:21:52  peter
    * c style array of const generates callparanodes
    * varargs paraloc fixes

  Revision 1.89  2003/10/01 20:34:48  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.88  2003/09/29 20:58:56  peter
    * optimized releasing of registers

  Revision 1.87  2003/09/28 21:46:18  peter
    * fix allocation of threadvar parameter

  Revision 1.86  2003/09/28 17:55:03  peter
    * parent framepointer changed to hidden parameter
    * tloadparentfpnode added

  Revision 1.85  2003/09/28 13:39:38  peter
    * optimized releasing of registers

  Revision 1.84  2003/09/25 21:27:31  peter
    * rearranged threadvar code so the result register is the same
      for the relocated and address loaded variables

  Revision 1.83  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.82  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.81  2003/09/14 12:57:10  peter
    * save destroyed registers when calling threadvar helper

  Revision 1.80  2003/09/10 08:31:47  marco
   * Patch from Peter for paraloc

  Revision 1.79  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.78  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.77.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.77.2.1  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.77  2003/08/20 20:13:08  daniel
    * Fixed the fixed trouble

  Revision 1.76  2003/08/20 20:11:24  daniel
    * Fixed some R_NO trouble

  Revision 1.75  2003/07/20 16:26:43  jonas
    * fix for threadvars with -dnewra

  Revision 1.74  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.73  2003/07/06 15:25:54  jonas
    * newra fix for threadvars

  Revision 1.72  2003/06/15 15:13:12  jonas
    * fixed register allocation for threadvar loads with newra

  Revision 1.71  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.70  2003/06/12 16:43:07  peter
    * newra compiles for sparc

  Revision 1.69  2003/06/09 16:41:52  jonas
    * fixed regvar optimization for call_by_reference parameters (no need
      to load address in another register)

  Revision 1.68  2003/06/08 18:27:15  jonas
    + ability to change the location of a ttempref node with changelocation()
      method. Useful to use instead of copying the contents from one temp to
      another
    + some shortstring optimizations in tassignmentnode that avoid some
      copying (required some shortstring optimizations to be moved from
      resulttype to firstpass, because they work on callnodes and string
      addnodes are only changed to callnodes in the firstpass)
    * allow setting/changing the funcretnode of callnodes after the
      resulttypepass has been done, funcretnode is now a property
    (all of the above should have a quite big effect on callparatemp)

  Revision 1.67  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.66  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.65  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.64  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.63  2003/05/30 23:54:08  jonas
    * forgot to commit, a_load_loc_reg change

  Revision 1.62  2003/05/26 19:38:28  peter
    * generic fpc_shorstr_concat
    + fpc_shortstr_append_shortstr optimization

  Revision 1.61  2003/05/24 11:47:27  jonas
    * fixed framepointer storage: it's now always stored at r1+12, which is
      a place in the link area reserved for compiler use.

  Revision 1.60  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.59  2003/05/15 18:58:53  peter
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
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.53  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procdef is now always nil when parsing
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
