{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for load/assignment nodes

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
unit n386ld;

{$i defines.inc}

interface

    uses
      node,nld;

    type
       ti386loadnode = class(tloadnode)
          procedure pass_2;override;
       end;

       ti386assignmentnode = class(tassignmentnode)
          procedure pass_2;override;
       end;

       ti386funcretnode = class(tfuncretnode)
          procedure pass_2;override;
       end;


implementation

    uses
      systems,
      cutils,verbose,globals,
      symconst,symdef,symsym,symtable,aasm,types,
      cginfo,cgbase,pass_2,
      nmem,ncon,ncnv,
      cpubase,cpuasm,
      cga,tgobj,n386util,regvars,cgobj,cg64f32,rgobj,rgcpu;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure ti386loadnode.pass_2;
      var
         hregister : tregister;
         symtabletype : tsymtabletype;
         i : longint;
         href : treference;
         newsize : tcgsize;
         popeax : boolean;
      begin
         { we don't know the size of all arrays }
         newsize:=def_cgsize(resulttype.def);
         location_reset(location,LOC_REFERENCE,newsize);
         case symtableentry.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    location.reference.symbol:=nil;
                    if (tabsolutesym(symtableentry).abstyp=toaddr) then
                     begin
                       if tabsolutesym(symtableentry).absseg then
                        location.reference.segment:=R_FS;
                       location.reference.offset:=tabsolutesym(symtableentry).address;
                     end
                    else
                     location.reference.symbol:=newasmsymbol(tabsolutesym(symtableentry).mangledname);
                 end;
              constsym:
                begin
                   if tconstsym(symtableentry).consttyp=constresourcestring then
                     begin
                        location_reset(location,LOC_CREFERENCE,OS_ADDR);
                        location.reference.symbol:=newasmsymbol(tconstsym(symtableentry).owner.name^+'_RESOURCESTRINGLIST');
                        location.reference.offset:=tconstsym(symtableentry).resstrindex*16+8;
                     end
                   else
                     internalerror(22798);
                end;
              varsym :
                 begin
                    hregister:=R_NO;
                    { C variable }
                    if (vo_is_C_var in tvarsym(symtableentry).varoptions) then
                      begin
                         location.reference.symbol:=newasmsymbol(tvarsym(symtableentry).mangledname);
                      end
                    { DLL variable }
                    else if (vo_is_dll_var in tvarsym(symtableentry).varoptions) then
                      begin
                         hregister:=rg.getregisterint(exprasmlist);
                         location.reference.symbol:=newasmsymbol(tvarsym(symtableentry).mangledname);
                         emit_ref_reg(A_MOV,S_L,location.reference,hregister);
                         location.reference.symbol:=nil;
                         location.reference.base:=hregister;
                      end
                    { external variable }
                    else if (vo_is_external in tvarsym(symtableentry).varoptions) then
                      begin
                         location.reference.symbol:=newasmsymbol(tvarsym(symtableentry).mangledname);
                      end
                    { thread variable }
                    else if (vo_is_thread_var in tvarsym(symtableentry).varoptions) then
                      begin
                         popeax:=not(R_EAX in rg.unusedregsint);
                         if popeax then
                           emit_reg(A_PUSH,S_L,R_EAX);
                         reference_reset_symbol(href,newasmsymbol(tvarsym(symtableentry).mangledname),0);
                         emit_ref(A_PUSH,S_L,href);
                         { the called procedure isn't allowed to change }
                         { any register except EAX                    }
                         cg.a_call_name(exprasmlist,'FPC_RELOCATE_THREADVAR');

                         location.reference.base:=rg.getregisterint(exprasmlist);
                         emit_reg_reg(A_MOV,S_L,R_EAX,location.reference.base);
                         if popeax then
                           emit_reg(A_POP,S_L,R_EAX);

                      end
                    { normal variable }
                    else
                      begin
                         symtabletype:=symtable.symtabletype;
                         { in case it is a register variable: }
                         if tvarsym(symtableentry).reg<>R_NO then
                           begin
                              if tvarsym(symtableentry).reg in [R_ST0..R_ST7] then
                                begin
                                   location_reset(location,LOC_CFPUREGISTER,def_cgsize(resulttype.def));
                                   location.register:=tvarsym(symtableentry).reg;
                                end
                              else
                                if not(rg.makeregsize(tvarsym(symtableentry).reg,OS_INT) in [R_EAX..R_EBX]) or
                                   rg.regvar_loaded[rg.makeregsize(tvarsym(symtableentry).reg,OS_INT)] then
                                begin
                                   location_reset(location,LOC_CREGISTER,
                                       cg.reg_cgsize(tvarsym(symtableentry).reg));
                                   location.register:=tvarsym(symtableentry).reg;
                                   exclude(rg.unusedregsint,rg.makeregsize(tvarsym(symtableentry).reg,OS_INT));
                                end
                              else
                                begin
                                  load_regvar(exprasmlist,tvarsym(symtableentry));
                                  location_reset(location,LOC_CREGISTER,
                                      cg.reg_cgsize(tvarsym(symtableentry).reg));
                                  location.register:=tvarsym(symtableentry).reg;
                                  exclude(rg.unusedregsint,rg.makeregsize(tvarsym(symtableentry).reg,OS_INT));
                                end
                           end
                         else
                           begin
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   location.reference.base:=procinfo^.framepointer;
                                   if (symtabletype in [inlinelocalsymtable,
                                                        localsymtable]) then
                                     location.reference.offset:=
                                       tvarsym(symtableentry).address-symtable.address_fixup
                                   else
                                     location.reference.offset:=
                                       tvarsym(symtableentry).address+symtable.address_fixup;

                                   if (symtabletype in [localsymtable,inlinelocalsymtable]) then
                                     begin
                                        if use_esp_stackframe then
                                          dec(location.reference.offset,
                                            tvarsym(symtableentry).getvaluesize)
                                        else
                                          location.reference.offset:=-location.reference.offset;
                                     end;
                                   if (lexlevel>symtable.symtablelevel) then
                                     begin
                                        hregister:=rg.getregisterint(exprasmlist);

                                        { make a reference }
                                        reference_reset_base(href,procinfo^.framepointer,procinfo^.framepointer_offset);

                                        emit_ref_reg(A_MOV,S_L,href,hregister);

                                        i:=lexlevel-1;
                                        while (i>symtable.symtablelevel) do
                                          begin
                                             { make a reference }
                                             reference_reset_base(href,hregister,8);
                                             emit_ref_reg(A_MOV,S_L,href,hregister);
                                             dec(i);
                                          end;
                                        location.reference.base:=hregister;
                                     end;
                                end
                              else
                                case symtabletype of
                                   globalsymtable,
                                   staticsymtable :
                                     begin
                                       location.reference.symbol:=newasmsymbol(tvarsym(symtableentry).mangledname);
                                     end;
                                   stt_exceptsymtable:
                                     begin
                                        location.reference.base:=procinfo^.framepointer;
                                        location.reference.offset:=tvarsym(symtableentry).address;
                                     end;
                                   objectsymtable:
                                     begin
                                        rg.getexplicitregisterint(exprasmlist,R_ESI);
                                        if (sp_static in tvarsym(symtableentry).symoptions) then
                                          begin
                                             location.reference.symbol:=newasmsymbol(tvarsym(symtableentry).mangledname);
                                          end
                                        else
                                          begin
                                             location.reference.base:=R_ESI;
                                             location.reference.offset:=tvarsym(symtableentry).address;
                                          end;
                                     end;
                                   withsymtable:
                                     begin
                                        { make a reference }
                                        { symtable datasize field
                                          contains the offset of the temp
                                          stored }
{                                       hp:=reference_new_base(procinfo^.framepointer,
                                          symtable.datasize);

                                        emit_ref_reg(A_MOV,S_L,hp,hregister);}

                                        if nf_islocal in tnode(twithsymtable(symtable).withnode).flags then
                                         begin
                                           location.reference:=twithnode(twithsymtable(symtable).withnode).withreference;
                                         end
                                        else
                                         begin
                                           hregister:=rg.getregisterint(exprasmlist);
                                           location.reference.base:=hregister;
                                           emit_ref_reg(A_MOV,S_L,
                                             twithnode(twithsymtable(symtable).withnode).withreference,
                                             hregister);
                                         end;
                                        inc(location.reference.offset,tvarsym(symtableentry).address);
                                     end;
                                end;
                           end;
                         { in case call by reference, then calculate. Open array
                           is always an reference! }
                         if (tvarsym(symtableentry).varspez in [vs_var,vs_out]) or
                            is_open_array(tvarsym(symtableentry).vartype.def) or
                            is_array_of_const(tvarsym(symtableentry).vartype.def) or
                            ((tvarsym(symtableentry).varspez=vs_const) and
                             push_addr_param(tvarsym(symtableentry).vartype.def)) then
                           begin
                              if hregister=R_NO then
                                hregister:=rg.getregisterint(exprasmlist);
                              case location.loc of
                                LOC_CREGISTER :
                                   emit_reg_reg(A_MOV,S_L,location.register,hregister);
                                LOC_REFERENCE,
                                LOC_CREFERENCE :
                                   emit_ref_reg(A_MOV,S_L,location.reference,hregister);
                                else
                                  internalerror(2002032218);
                              end;
                              location_reset(location,LOC_REFERENCE,newsize);
                              location.reference.base:=hregister;
                          end;
                      end;
                 end;
              procsym:
                 begin
                    if assigned(left) then
                      begin
                         location_reset(location,LOC_CREFERENCE,OS_64);
                         tg.gettempofsizereference(exprasmlist,8,location.reference);

                         { called as type.method, then we only need to return
                           the address of the function, not the self pointer }
                         if left.nodetype=typen then
                          begin
                            { there is no instance, we return 0 }
                            cg.a_load_const_ref(exprasmlist,OS_ADDR,0,location.reference);
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
                                    rg.getexplicitregisterint(exprasmlist,R_EDI);
                                    hregister:=R_EDI;
                                    if is_class_or_interface(left.resulttype.def) then
                                      emit_ref_reg(A_MOV,S_L,left.location.reference,R_EDI)
                                    else
                                      emit_ref_reg(A_LEA,S_L,left.location.reference,R_EDI);
                                 end;
                               else internalerror(26019);

                            end;
                            location_release(exprasmlist,left.location);
                            location_freetemp(exprasmlist,left.location);

                            { store the class instance address }
                            href:=location.reference;
                            inc(href.offset,4);
                            emit_reg_ref(A_MOV,S_L,hregister,href);
                          end;

                         { virtual method ? }
                         if (po_virtualmethod in tprocdef(resulttype.def).procoptions) then
                           begin
                              reference_reset_base(href,hregister,0);
                              { load vmt pointer }
                              emit_ref_reg(A_MOV,S_L,href,R_EDI);
{$IfDef regallocfix}
                              rg.del_reference(exprasmlist,hp^);
{$EndIf regallocfix}
                              { load method address }
                              reference_reset_base(href,R_EDI,tprocdef(resulttype.def)._class.vmtmethodoffset(
                                                   tprocdef(resulttype.def).extnumber));
                              emit_ref_reg(A_MOV,S_L,href,R_EDI);
                              { ... and store it }
                              cg.a_load_reg_loc(exprasmlist,OS_ADDR,R_EDI,location);
                              rg.ungetregisterint(exprasmlist,R_EDI);
                           end
                         else
                           begin
                              reference_reset_symbol(href,newasmsymbol(tprocdef(resulttype.def).mangledname),0);
                              cg.a_loadaddr_ref_reg(exprasmlist,href,R_EDI);
                              cg.a_load_reg_loc(exprasmlist,OS_ADDR,R_EDI,location);
                              rg.ungetregisterint(exprasmlist,R_EDI);
                           end;
                      end
                    else
                      begin
                         {!!!!! Be aware, work on virtual methods too }
                         location.reference.symbol:=newasmsymbol(tprocdef(resulttype.def).mangledname);
                      end;
                 end;
              typedconstsym :
                 begin
                    location.reference.symbol:=newasmsymbol(ttypedconstsym(symtableentry).mangledname);
                 end;
              else internalerror(4);
         end;
      end;


{*****************************************************************************
                             SecondAssignment
*****************************************************************************}

    procedure ti386assignmentnode.pass_2;
      var
         otlabel,hlabel,oflabel : tasmlabel;
         fputyp : tfloattype;
         href : treference;
         ai : taicpu;
         releaseright,
         pushed : boolean;
         cgsize : tcgsize;

      begin
        otlabel:=truelabel;
        oflabel:=falselabel;
        getlabel(truelabel);
        getlabel(falselabel);

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
              pushed:=maybe_push(left.registers32,right,false);
              secondpass(left);
              { decrement destination reference counter }
              if (left.resulttype.def.needs_inittable) then
               cg.g_decrrefcount(exprasmlist,left.resulttype.def,left.location.reference);
              if pushed then
                restore(right,false);
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
           pushed:=maybe_push(right.registers32,left,is_64bitint(right.resulttype.def));
           secondpass(right);
           { increment source reference counter, this is
             useless for string constants}
           if (right.resulttype.def.needs_inittable) and
              (right.nodetype<>stringconstn) then
            cg.g_incrrefcount(exprasmlist,right.resulttype.def,right.location.reference);
           if pushed  then
             restore(left,false);

           if codegenerror then
             exit;
         end;

        if not(left.location.loc in [LOC_REFERENCE,LOC_CFPUREGISTER,
                                     LOC_CREGISTER,LOC_CMMXREGISTER]) then
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
                emit_const_ref(A_MOV,S_B,0,left.location.reference);
              end
            { char loading }
            else if is_char(right.resulttype.def) then
              begin
                if right.nodetype=ordconstn then
                  emit_const_ref(A_MOV,S_W,tordconstnode(right).value*256+1,left.location.reference)
                else
                  begin
                     if (right.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                       begin
                          href := left.location.reference;
                          emit_const_ref(A_MOV,S_B,1,href);
                          inc(href.offset,1);
                          emit_reg_ref(A_MOV,S_B,rg.makeregsize(right.location.register,OS_8),href);
                       end
                     else
                     { not so elegant (goes better with extra register    }
                       begin
                          { not "movl", because then we may read past the }
                          { end of the heap! "movw" would be ok too, but  }
                          { I don't think that would be faster (JM)       }
                          rg.getexplicitregisterint(exprasmlist,R_EDI);
                          emit_ref_reg(A_MOVZX,S_BL,right.location.reference,R_EDI);
                          emit_const_reg(A_SHL,S_L,8,R_EDI);
                          emit_const_reg(A_OR,S_L,1,R_EDI);
                          emit_reg_ref(A_MOV,S_W,R_DI,left.location.reference);
                          rg.ungetregisterint(exprasmlist,R_EDI);
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
                   tcg64f32(cg).a_load64_const_loc(exprasmlist,
                       right.location.valuelow,right.location.valuehigh,left.location)
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
                         tcg64f32(cg).a_load64_ref_reg(exprasmlist,
                             right.location.reference,left.location.registerlow,left.location.registerhigh)
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
                   emit_reg_reg(A_MOVQ,S_NO,right.location.register,left.location.register)
                  else
                   emit_reg_ref(A_MOVQ,S_NO,right.location.register,left.location.reference);
                end;
{$endif SUPPORT_MMX}
              LOC_REGISTER,
              LOC_CREGISTER :
                begin
                  cgsize:=def_cgsize(left.resulttype.def);
                  if cgsize in [OS_64,OS_S64] then
                   tcg64f32(cg).a_load64_reg_loc(exprasmlist,
                       right.location.registerlow,right.location.registerhigh,left.location)
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
                  getlabel(hlabel);
                  { generate the leftnode for the true case, and
                    release the location }
                  cg.a_label(exprasmlist,truelabel);
                  pushed:=maybe_push(left.registers32,right,false);
                  secondpass(left);
                  if pushed then
                    restore(right,false);
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,1,left.location);
                  location_release(exprasmlist,left.location);
                  cg.a_jmp_always(exprasmlist,hlabel);
                  { generate the leftnode for the false case }
                  cg.a_label(exprasmlist,falselabel);
                  pushed:=maybe_push(left.registers32,right,false);
                  secondpass(left);
                  if pushed then
                    restore(right,false);
                  if codegenerror then
                    exit;
                  cg.a_load_const_loc(exprasmlist,0,left.location);
                  cg.a_label(exprasmlist,hlabel);
                end;
              LOC_FLAGS :
                begin
                  if left.location.loc=LOC_CREGISTER then
                    cg.g_flags2reg(exprasmlist,right.location.resflags,left.location.register)
                  else
                    begin
                      if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                       internalerror(200203273);
                      ai:=Taicpu.Op_ref(A_Setcc,S_B,left.location.reference);
                      ai.SetCondition(flags_to_cond(right.location.resflags));
                      exprasmList.concat(ai);
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

    procedure ti386funcretnode.pass_2;
      var
         hreg : tregister;
         href : treference;
         pp : pprocinfo;
         hr_valid : boolean;
         i : integer;
      begin
         location_reset(location,LOC_REFERENCE,def_cgsize(resulttype.def));
         hr_valid:=false;
         if (not inlining_procedure) and
            (lexlevel<>funcretsym.owner.symtablelevel) then
           begin
              hreg:=rg.getregisterint(exprasmlist);
              hr_valid:=true;
              reference_reset_base(href,procinfo^.framepointer,procinfo^.framepointer_offset);
              emit_ref_reg(A_MOV,S_L,href,hreg);

              { walk up the stack frame }
              pp:=procinfo^.parent;
              i:=lexlevel-1;
              while i>funcretsym.owner.symtablelevel do
               begin
                 reference_reset_base(href,hreg,pp^.framepointer_offset);
                 emit_ref_reg(A_MOV,S_L,href,hreg);
                 pp:=pp^.parent;
                 dec(i);
               end;
              location.reference.base:=hreg;
              location.reference.offset:=pp^.return_offset;
           end
         else
           begin
             location.reference.base:=procinfo^.framepointer;
             location.reference.offset:=procinfo^.return_offset;
           end;
         if ret_in_param(resulttype.def) then
           begin
              if not hr_valid then
                hreg:=rg.getregisterint(exprasmlist);
              emit_ref_reg(A_MOV,S_L,location.reference,hreg);
              location.reference.base:=hreg;
              location.reference.offset:=0;
           end;
      end;

begin
   cloadnode:=ti386loadnode;
   cassignmentnode:=ti386assignmentnode;
   cfuncretnode:=ti386funcretnode;
end.
{
  $Log$
  Revision 1.41  2002-05-12 16:53:17  peter
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

  Revision 1.40  2002/04/26 15:19:05  peter
    * use saveregisters for incr routines, saves also problems with
      the optimizer

  Revision 1.39  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.38  2002/04/22 16:30:06  peter
    * fixed @methodpointer

  Revision 1.37  2002/04/21 15:36:13  carl
  * changeregsize -> rg.makeregsize

  Revision 1.36  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.35  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.34  2002/04/07 09:16:07  carl
  - remove unused variable

  Revision 1.33  2002/04/04 19:06:12  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.32  2002/04/03 10:43:37  jonas
    * fixed regvar-related bugs (the load node set the location to
      LOC_REGISTER instead of LOC_CREGISTER and the assignment node didn't
      support loading constants in LOC_CREGISTER's)

  Revision 1.31  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.30  2002/03/31 20:26:39  jonas
    + a_loadfpu_* and a_loadmm_* methods in tcg
    * register allocation is now handled by a class and is mostly processor
      independent (+rgobj.pas and i386/rgcpu.pas)
    * temp allocation is now handled by a class (+tgobj.pas, -i386\tgcpu.pas)
    * some small improvements and fixes to the optimizer
    * some register allocation fixes
    * some fpuvaroffset fixes in the unary minus node
    * push/popusedregisters is now called rg.save/restoreusedregisters and
      (for i386) uses temps instead of push/pop's when using -Op3 (that code is
      also better optimizable)
    * fixed and optimized register saving/restoring for new/dispose nodes
    * LOC_FPU locations now also require their "register" field to be set to
      R_ST, not R_ST0 (the latter is used for LOC_CFPUREGISTER locations only)
    - list field removed of the tnode class because it's not used currently
      and can cause hard-to-find bugs

  Revision 1.29  2002/03/04 19:10:14  peter
    * removed compiler warnings

  Revision 1.28  2001/12/30 17:24:46  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.27  2001/12/17 23:16:05  florian
    * array of const can now take widestring parameters as well

  Revision 1.26  2001/11/02 22:58:11  peter
    * procsym definition rewrite

  Revision 1.25  2001/10/28 17:22:25  peter
    * allow assignment of overloaded procedures to procvars when we know
      which procedure to take

  Revision 1.24  2001/10/14 11:49:51  jonas
    * finetuned register allocation info for assignments

  Revision 1.23  2001/10/04 14:33:28  jonas
    * fixed range check errors

  Revision 1.22  2001/09/09 08:51:09  jonas
    * fixed bug with assigning ansistrings (left^.location was released too
      early, caused bug reported by Aleksey V. Vaneev in mailing list on
      2001/09/07 regarding 'problems with nested procedures and local vars'
      ("merged" from cga.pas in the fixes branch)

  Revision 1.21  2001/08/30 20:13:57  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.20  2001/08/30 11:57:20  michael
  + Patch for wrong paramsize

  Revision 1.19  2001/08/26 13:36:59  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.18  2001/08/06 21:40:50  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.17  2001/08/05 13:19:51  peter
    * partly fix for proc of obj=nil

  Revision 1.15  2001/07/28 15:13:17  peter
    * fixed opsize for assignment with LOC_JUMP

  Revision 1.14  2001/05/27 14:30:56  florian
    + some widestring stuff added

  Revision 1.13  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.12  2001/04/02 21:20:37  peter
    * resulttype rewrite

  Revision 1.11  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.9  2000/11/29 00:30:48  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.8  2000/11/13 14:44:36  jonas
    * fixes so no more range errors with improved range checking code

  Revision 1.7  2000/11/12 23:24:15  florian
    * interfaces are basically running

  Revision 1.6  2000/11/11 22:59:20  florian
    * fixed resourcestrings, made a stupid mistake yesterday

  Revision 1.5  2000/11/09 18:52:06  florian
    * resourcestrings doesn't need the helper anymore they
      access the table now direct

  Revision 1.4  2000/11/06 23:15:02  peter
    * added copyvaluepara call again

  Revision 1.3  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.1  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

}
