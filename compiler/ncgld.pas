{
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
          procedure generate_picvaraccess;virtual;
       end;

       tcgassignmentnode = class(tassignmentnode)
          procedure pass_2;override;
       end;

       tcgarrayconstructornode = class(tarrayconstructornode)
          procedure pass_2;override;
       end;

       tcgrttinode = class(trttinode)
          procedure pass_2;override;
       end;


implementation

    uses
      cutils,
      systems,
      verbose,globtype,globals,
      symconst,symtype,symdef,symsym,defutil,paramgr,
      ncnv,ncon,nmem,nbas,
      aasmbase,aasmtai,aasmcpu,
      cgbase,pass_2,
      procinfo,
      cpubase,parabase,
      tgobj,ncgutil,
      cgutils,cgobj,
      ncgbas;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure tcgloadnode.generate_picvaraccess;
      begin
{$ifndef sparc}
        location.reference.base:=current_procinfo.got;
        location.reference.symbol:=objectlibrary.newasmsymbol(tglobalvarsym(symtableentry).mangledname+'@GOT',AB_EXTERNAL,AT_DATA);
{$endif sparc}
      end;


    procedure tcgloadnode.pass_2;
      var
        hregister : tregister;
        symtabletype : tsymtabletype;
        href : treference;
        newsize : tcgsize;
        endrelocatelab,
        norelocatelab : tasmlabel;
        paraloc1 : tcgpara;
      begin
         { we don't know the size of all arrays }
         newsize:=def_cgsize(resulttype.def);
         location_reset(location,LOC_REFERENCE,newsize);
         case symtableentry.typ of
            absolutevarsym :
               begin
                  { this is only for toasm and toaddr }
                  case tabsolutevarsym(symtableentry).abstyp of
                    toaddr :
                      begin
{$ifdef i386}
                        if tabsolutevarsym(symtableentry).absseg then
                          location.reference.segment:=NR_FS;
{$endif i386}
                        location.reference.offset:=tabsolutevarsym(symtableentry).addroffset;
                      end;
                    toasm :
                      location.reference.symbol:=objectlibrary.newasmsymbol(tabsolutevarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA);
                    else
                      internalerror(200310283);
                  end;
               end;
            constsym:
              begin
                 if tconstsym(symtableentry).consttyp=constresourcestring then
                   begin
                      location_reset(location,LOC_CREFERENCE,OS_ADDR);
                      location.reference.symbol:=objectlibrary.newasmsymbol(make_mangledname('RESOURCESTRINGLIST',tconstsym(symtableentry).owner,''),AB_EXTERNAL,AT_DATA);
                      location.reference.offset:=tconstsym(symtableentry).resstrindex*(4+sizeof(aint)*3)+4+sizeof(aint);
                   end
                 else
                   internalerror(22798);
              end;
            globalvarsym,
            localvarsym,
            paravarsym :
               begin
                  symtabletype:=symtable.symtabletype;
                  hregister:=NR_NO;
                  { DLL variable }
                  if (vo_is_dll_var in tabstractvarsym(symtableentry).varoptions) then
                    begin
                      if target_info.system=system_powerpc_darwin then
                        begin
                          generate_picvaraccess;
                          if not(pi_needs_got in current_procinfo.flags) then
                            internalerror(200403022);
                        end
                      else
                        begin
                          hregister:=cg.getaddressregister(exprasmlist);
                          location.reference.symbol:=objectlibrary.newasmsymbol(tglobalvarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,location.reference,hregister);
                          reference_reset_base(location.reference,hregister,0);
                        end;
                    end
                  { Thread variable }
                  else if (vo_is_thread_var in tabstractvarsym(symtableentry).varoptions) then
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
                       paraloc1.init;
                       paramanager.getintparaloc(pocall_default,1,paraloc1);
                       hregister:=cg.getaddressregister(exprasmlist);
                       reference_reset_symbol(href,objectlibrary.newasmsymbol('FPC_THREADVAR_RELOCATE',AB_EXTERNAL,AT_DATA),0);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,hregister);
                       cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,hregister,norelocatelab);
                       { don't save the allocated register else the result will be destroyed later }
                       reference_reset_symbol(href,objectlibrary.newasmsymbol(tglobalvarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA),0);
                       paramanager.allocparaloc(exprasmlist,paraloc1);
                       cg.a_param_ref(exprasmlist,OS_ADDR,href,paraloc1);
                       paramanager.freeparaloc(exprasmlist,paraloc1);
                       paraloc1.done;
                       cg.allocallcpuregisters(exprasmlist);
                       cg.a_call_reg(exprasmlist,hregister);
                       cg.deallocallcpuregisters(exprasmlist);
                       cg.getcpuregister(exprasmlist,NR_FUNCTION_RESULT_REG);
                       cg.ungetcpuregister(exprasmlist,NR_FUNCTION_RESULT_REG);
                       hregister:=cg.getaddressregister(exprasmlist);
                       cg.a_load_reg_reg(exprasmlist,OS_INT,OS_ADDR,NR_FUNCTION_RESULT_REG,hregister);
                       cg.a_jmp_always(exprasmlist,endrelocatelab);
                       cg.a_label(exprasmlist,norelocatelab);
                       { no relocation needed, load the address of the variable only, the
                         layout of a threadvar is (4 bytes pointer):
                           0 - Threadvar index
                           4 - Threadvar value in single threading }
                       reference_reset_symbol(href,objectlibrary.newasmsymbol(tglobalvarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA),sizeof(aint));
                       cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                       cg.a_label(exprasmlist,endrelocatelab);
                       location.reference.base:=hregister;
                    end
                  { Nested variable }
                  else if assigned(left) then
                    begin
                      if not(symtabletype in [localsymtable,parasymtable]) then
                        internalerror(200309285);
                      secondpass(left);
                      if left.location.loc<>LOC_REGISTER then
                        internalerror(200309286);
                      if tabstractnormalvarsym(symtableentry).localloc.loc<>LOC_REFERENCE then
                        internalerror(200409241);
                      hregister:=left.location.register;
                      reference_reset_base(location.reference,hregister,tabstractnormalvarsym(symtableentry).localloc.reference.offset);
                    end
                  { Normal (or external) variable }
                  else
                    begin
{$ifdef OLDREGVARS}
                       { in case it is a register variable: }
                       if tvarsym(symtableentry).localloc.loc in [LOC_REGISTER,LOC_FPUREGISTER] then
                         begin
                            case getregtype(tvarsym(symtableentry).localloc.register) of
                              R_FPUREGISTER :
                                begin
                                  location_reset(location,LOC_CFPUREGISTER,def_cgsize(resulttype.def));
                                  location.register:=tvarsym(symtableentry).localloc.register;
                                end;
                              R_INTREGISTER :
                                begin
                                  location_reset(location,LOC_CREGISTER,def_cgsize(resulttype.def));
                                  location.register:=tvarsym(symtableentry).localloc.register;
                                  hregister := location.register;
                                end;
                              else
                                internalerror(200301172);
                            end;
                         end
                       else
{$endif OLDREGVARS}
                         begin
                           case symtabletype of
                              stt_exceptsymtable,
                              localsymtable,
                              parasymtable :
                                location:=tabstractnormalvarsym(symtableentry).localloc;
                              globalsymtable,
                              staticsymtable :
                                begin
                                  if (target_info.system=system_powerpc_darwin) and
                                    (cs_create_pic in aktmoduleswitches) then
                                    begin
                                      generate_picvaraccess;
                                      if not(pi_needs_got in current_procinfo.flags) then
                                        internalerror(200403023);
                                    end
                                  else
                                    begin
                                      if tabstractnormalvarsym(symtableentry).localloc.loc=LOC_INVALID then
                                        reference_reset_symbol(location.reference,objectlibrary.newasmsymbol(tglobalvarsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA),0)
                                      else
                                        location:=tglobalvarsym(symtableentry).localloc;
                                    end;
                                end;
                              else
                                internalerror(200305102);
                           end;
                         end;
                    end;

                  { handle call by reference variables when they are not
                    alreayd copied to local copies. Also ignore the reference
                    when we need to load the self pointer for objects }
                  if is_addr_param_load then
                    begin
                      if (location.loc in [LOC_CREGISTER,LOC_REGISTER]) then
                        hregister:=location.register
                      else
                        begin
                          hregister:=cg.getaddressregister(exprasmlist);
                          { we need to load only an address }
                          location.size:=OS_ADDR;
                          cg.a_load_loc_reg(exprasmlist,location.size,location,hregister);
                        end;
                      location_reset(location,LOC_REFERENCE,newsize);
                      location.reference.base:=hregister;
                    end;

                  { make const a LOC_CREFERENCE }
                  if (tabstractvarsym(symtableentry).varspez=vs_const) and
                     (location.loc=LOC_REFERENCE) then
                    location.loc:=LOC_CREFERENCE;
               end;
            procsym:
               begin
                  if not assigned(procdef) then
                    internalerror(200312011);
                  if assigned(left) then
                    begin
                      {
                        THIS IS A TERRIBLE HACK!!!!!! WHICH WILL NOT WORK
                        ON 64-BIT SYSTEMS: SINCE PROCSYM FOR METHODS
                        CONSISTS OF TWO OS_ADDR, so you cannot set it
                        to OS_64 - how to solve?? Carl
                        Solved. Florian
                      }
                      if (sizeof(aint) = 4) then
                         location_reset(location,LOC_CREFERENCE,OS_64)
                      else if (sizeof(aint) = 8) then
                         location_reset(location,LOC_CREFERENCE,OS_128)
                      else
                         internalerror(20020520);
                      tg.GetTemp(exprasmlist,2*sizeof(aint),tt_normal,location.reference);
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
                      inc(href.offset,sizeof(aint));
                      cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,href);

                      { virtual method ? }
                      if (po_virtualmethod in procdef.procoptions) then
                        begin
                          { load vmt pointer }
                          reference_reset_base(href,hregister,0);
                          hregister:=cg.getaddressregister(exprasmlist);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,hregister);
                          { load method address }
                          reference_reset_base(href,hregister,procdef._class.vmtmethodoffset(procdef.extnumber));
                          hregister:=cg.getaddressregister(exprasmlist);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,hregister);
                          { ... and store it }
                          cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
                        end
                      else
                        begin
                          { load address of the function }
                          reference_reset_symbol(href,objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION),0);
                          hregister:=cg.getaddressregister(exprasmlist);
                          cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                          cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
                        end;
                    end
                  else
                    begin
                       {!!!!! Be aware, work on virtual methods too }
                       location.reference.symbol:=objectlibrary.newasmsymbol(procdef.mangledname,AB_EXTERNAL,AT_FUNCTION);
                    end;
               end;
            typedconstsym :
               begin
                  location.reference.symbol:=objectlibrary.newasmsymbol(ttypedconstsym(symtableentry).mangledname,AB_EXTERNAL,AT_DATA);
               end;
            labelsym :
               begin
                  location.reference.symbol:=objectlibrary.newasmsymbol(tlabelsym(symtableentry).mangledname,AB_EXTERNAL,AT_FUNCTION);
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
         len : aint;
         r:Tregister;

      begin
        location_reset(location,LOC_VOID,OS_NO);

        otlabel:=truelabel;
        oflabel:=falselabel;
        objectlibrary.getlabel(truelabel);
        objectlibrary.getlabel(falselabel);

        {
          in most cases we can process first the right node which contains
          the most complex code. Exceptions for this are:
            - result is in flags, loading left will then destroy the flags
            - result is a jump, loading left must be already done before the jump is made
            - result need reference count, when left points to a value used in
              right then decreasing the refcnt on left can possibly release
              the memory before right increased the refcnt, result is that an
              empty value is assigned
            - calln, call destroys most registers and is therefor 'complex'

           But not when the result is in the flags, then
          loading the left node afterwards can destroy the flags.
        }
        if not(right.expectloc in [LOC_FLAGS,LOC_JUMP]) and
           ((right.nodetype=calln) or
            (right.resulttype.def.needs_inittable) or
            (right.registersint>=left.registersint)) then
         begin
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            begin
              location_force_mem(exprasmlist,right.location);
              location_get_data_ref(exprasmlist,right.location,href,false);
              cg.g_incrrefcount(exprasmlist,right.resulttype.def,href);
            end;
           if codegenerror then
             exit;

           if not(nf_concat_string in flags) then
            begin
              { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
              { can be false                                             }
              secondpass(left);
              { decrement destination reference counter }
              if (left.resulttype.def.needs_inittable) then
                begin
                  location_get_data_ref(exprasmlist,left.location,href,false);
                  cg.g_decrrefcount(exprasmlist,left.resulttype.def,href);
                end;
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
                 begin
                   location_get_data_ref(exprasmlist,left.location,href,false);
                   cg.g_decrrefcount(exprasmlist,left.resulttype.def,href);
                 end;
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
             begin
               location_force_mem(exprasmlist,right.location);
               location_get_data_ref(exprasmlist,right.location,href,false);
               cg.g_incrrefcount(exprasmlist,right.resulttype.def,href);
             end;

           if codegenerror then
             exit;
         end;

        releaseright:=true;

        { optimize temp to temp copies }
(*        if (left.nodetype = temprefn) and
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
        else *)
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
                          r:=cg.makeregsize(exprasmlist,right.location.register,OS_8);
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
{$ifndef cpu64bit}
                  if right.location.size in [OS_64,OS_S64] then
                   cg64.a_load64_const_loc(exprasmlist,right.location.value64,left.location)
                  else
{$endif cpu64bit}
                   cg.a_load_const_loc(exprasmlist,right.location.value,left.location);
                end;
              LOC_REFERENCE,
              LOC_CREFERENCE :
                begin
                  case left.location.loc of
                    LOC_REGISTER,
                    LOC_CREGISTER :
                      begin
{$ifndef cpu64bit}
                        if left.location.size in [OS_64,OS_S64] then
                          cg64.a_load64_ref_reg(exprasmlist,right.location.reference,left.location.register64)
                        else
{$endif cpu64bit}
                          cg.a_load_ref_reg(exprasmlist,right.location.size,left.location.size,right.location.reference,left.location.register);
                      end;
                    LOC_FPUREGISTER,
                    LOC_CFPUREGISTER :
                      begin
                        cg.a_loadfpu_ref_reg(exprasmlist,
                            right.location.size,
                            right.location.reference,
                            left.location.register);
                      end;
                    LOC_REFERENCE,
                    LOC_CREFERENCE :
                      begin
{$warning HACK: unaligned test, maybe remove all unaligned locations (array of char) from the compiler}
                        { Use unaligned copy when the offset is not aligned }
                        len:=left.resulttype.def.size;
                        if (right.location.reference.offset mod sizeof(aint)<>0) or
                            (left.location.reference.offset mod sizeof(aint)<>0) or
                            (right.resulttype.def.alignment<sizeof(aint)) then
                          cg.g_concatcopy_unaligned(exprasmlist,right.location.reference,left.location.reference,len)
                        else
                          cg.g_concatcopy(exprasmlist,right.location.reference,left.location.reference,len);
                      end;
                    LOC_MMREGISTER,
                    LOC_CMMREGISTER:
                      cg.a_loadmm_ref_reg(exprasmlist,
                        right.location.size,
                        left.location.size,
                        right.location.reference,
                        left.location.register,mms_movescalar);
                    else
                      internalerror(200203284);
                  end;
                end;
{$ifdef SUPPORT_MMX}
              LOC_CMMXREGISTER,
              LOC_MMXREGISTER:
                begin
                  if left.location.loc=LOC_CMMXREGISTER then
                    cg.a_loadmm_reg_reg(exprasmlist,OS_M64,OS_M64,right.location.register,left.location.register,nil)
                  else
                    cg.a_loadmm_reg_ref(exprasmlist,OS_M64,OS_M64,right.location.register,left.location.reference,nil);
                end;
{$endif SUPPORT_MMX}
              LOC_MMREGISTER,
              LOC_CMMREGISTER:
                begin
                  if left.resulttype.def.deftype=arraydef then
                    begin
                    end
                  else
                    begin
                      if left.location.loc=LOC_CMMREGISTER then
                        cg.a_loadmm_reg_reg(exprasmlist,right.location.size,left.location.size,right.location.register,left.location.register,mms_movescalar)
                      else
                        cg.a_loadmm_reg_ref(exprasmlist,right.location.size,left.location.size,right.location.register,left.location.reference,mms_movescalar);
                    end;
                end;
              LOC_REGISTER,
              LOC_CREGISTER :
                begin
{$ifndef cpu64bit}
                  if left.location.size in [OS_64,OS_S64] then
                    cg64.a_load64_reg_loc(exprasmlist,
                      right.location.register64,left.location)
                  else
{$endif cpu64bit}
                    cg.a_load_reg_loc(exprasmlist,right.location.size,right.location.register,left.location);
                end;
              LOC_FPUREGISTER,
              LOC_CFPUREGISTER :
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
                  { we can't do direct moves between fpu and mm registers }
                  if left.location.loc in [LOC_MMREGISTER,LOC_CMMREGISTER] then
                    begin
                      location_force_mmregscalar(exprasmlist,right.location,false);
                      cg.a_loadmm_reg_reg(exprasmlist,
                          tfloat2tcgsize[fputyp],tfloat2tcgsize[fputyp],
                          right.location.register,left.location.register,mms_movescalar);
                    end
                  else
                    cg.a_loadfpu_reg_loc(exprasmlist,
                        tfloat2tcgsize[fputyp],
                        right.location.register,left.location);
                end;
              LOC_JUMP :
                begin
                  objectlibrary.getlabel(hlabel);
                  cg.a_label(exprasmlist,truelabel);
                  cg.a_load_const_loc(exprasmlist,1,left.location);
                  cg.a_jmp_always(exprasmlist,hlabel);
                  cg.a_label(exprasmlist,falselabel);
                  cg.a_load_const_loc(exprasmlist,0,left.location);
                  cg.a_label(exprasmlist,hlabel);
                end;
{$ifdef cpuflags}
              LOC_FLAGS :
                begin
                  {This can be a wordbool or longbool too, no?}
                  if left.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
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
          location_freetemp(exprasmlist,right.location);

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
        vtAnsiString32 = 11;
        vtCurrency   = 12;
        vtVariant    = 13;
        vtInterface  = 14;
        vtWideString = 15;
        vtInt64      = 16;
        vtQWord      = 17;
        vtAnsiString16 = 18;
        vtAnsiString64 = 19;

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
        paraloc : tcgparalocation;
      begin
        dovariant:=(nf_forcevaria in flags) or tarraydef(resulttype.def).isvariant;
        if dovariant then
          elesize:=sizeof(aint)+sizeof(aint)
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
                       if is_currency(lt) then
                         vtype:=vtCurrency
                       else
                         vtype:=vtExtended;
                       freetemp:=false;
                       vaddr:=true;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         vtype:=vtPChar
                       else if is_pwidechar(lt) then
                         vtype:=vtPWideChar
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
                     if is_interface(lt) then
                       vtype:=vtInterface
                     { vtObject really means a class based on TObject }
                     else if is_class(lt) then
                       vtype:=vtObject
                     else
                       internalerror(200505171);
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
                        {$ifdef ansistring_bits}
                         begin
                           case Tstringdef(lt).string_typ of
                             st_ansistring16:
                               vtype:=vtAnsiString16;
                             st_ansistring32:
                               vtype:=vtAnsiString32;
                             st_ansistring64:
                               vtype:=vtAnsiString64;
                           end;
                           freetemp:=false;
                         end
                        {$else}
                         begin
                           vtype:=vtAnsiString;
                           freetemp:=false;
                         end
                        {$endif}
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
                 inc(href.offset,sizeof(aint));
                 if vaddr then
                  begin
                    location_force_mem(exprasmlist,hp.left.location);
                    tmpreg:=cg.getaddressregister(exprasmlist);
                    cg.a_loadaddr_ref_reg(exprasmlist,hp.left.location.reference,tmpreg);
                    cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,tmpreg,href);
                  end
                 else
                  cg.a_load_loc_ref(exprasmlist,OS_ADDR,hp.left.location,href);
                 { update href to the vtype field and write it }
                 dec(href.offset,sizeof(aint));
                 cg.a_load_const_ref(exprasmlist, OS_INT,vtype,href);
                 { goto next array element }
                 inc(href.offset,sizeof(aint)*2);
               end
              else
              { normal array constructor of the same type }
               begin
                 if (is_ansistring(left.resulttype.def) or
                     is_widestring(left.resulttype.def) or
                     (left.resulttype.def.deftype=variantdef)) then
                   freetemp:=false;
                 case hp.left.location.loc of
                   LOC_FPUREGISTER,
                   LOC_CFPUREGISTER :
                     cg.a_loadfpu_reg_ref(exprasmlist,hp.left.location.size,hp.left.location.register,href);
                   LOC_REFERENCE,
                   LOC_CREFERENCE :
                     begin
                       if is_shortstring(hp.left.resulttype.def) then
                         cg.g_copyshortstring(exprasmlist,hp.left.location.reference,href,
                             Tstringdef(hp.left.resulttype.def).len)
                       else
                         cg.g_concatcopy(exprasmlist,hp.left.location.reference,href,elesize);
                     end;
                   else
                     begin
{$ifndef cpu64bit}
                       if hp.left.location.size in [OS_64,OS_S64] then
                         cg64.a_load64_loc_ref(exprasmlist,hp.left.location,href)
                       else
{$endif cpu64bit}
                         cg.a_load_loc_ref(exprasmlist,hp.left.location.size,hp.left.location,href);
                     end;
                 end;
                 inc(href.offset,elesize);
               end;
              if freetemp then
                location_freetemp(exprasmlist,hp.left.location);
            end;
           { load next entry }
           hp:=tarrayconstructornode(hp.right);
         end;
      end;


{*****************************************************************************
                           SecondRTTI
*****************************************************************************}

    procedure tcgrttinode.pass_2;
      begin
        location_reset(location,LOC_CREFERENCE,OS_NO);
        location.reference.symbol:=rttidef.get_rtti_label(rttitype);
      end;



begin
   cloadnode:=tcgloadnode;
   cassignmentnode:=tcgassignmentnode;
   carrayconstructornode:=tcgarrayconstructornode;
   crttinode:=tcgrttinode;
end.
