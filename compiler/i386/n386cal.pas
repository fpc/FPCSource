{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Generate i386 assembler for in call nodes

    This program is free software; you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published bymethodpointer
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
unit n386cal;

{$i defines.inc}

interface

{ $define AnsiStrRef}

    uses
      symdef,node,ncal;

    type
       ti386callparanode = class(tcallparanode)
          procedure secondcallparan(defcoll : TParaItem;
                   push_from_left_to_right,inlined,is_cdecl : boolean;
                   para_alignment,para_offset : longint);override;
       end;

       ti386callnode = class(tcallnode)
          procedure pass_2;override;
       end;

       ti386procinlinenode = class(tprocinlinenode)
          procedure pass_2;override;
       end;

implementation

    uses
{$ifdef delphi}
      sysutils,
{$else}
      strings,
{$endif}
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,aasm,types,
{$ifdef GDB}
      gdb,
{$endif GDB}
      cgbase,temp_gen,pass_2,
      cpubase,cpuasm,
      nmem,nld,
      tainst,cga,tgcpu,n386ld,n386util,regvars;

{*****************************************************************************
                             TI386CALLPARANODE
*****************************************************************************}

    procedure ti386callparanode.secondcallparan(defcoll : TParaItem;
                push_from_left_to_right,inlined,is_cdecl : boolean;para_alignment,para_offset : longint);

      procedure maybe_push_high;
        begin
           { open array ? }
           { defcoll.data can be nil for read/write }
           if assigned(defcoll.paratype.def) and
              assigned(hightree) then
            begin
              secondpass(hightree);
              { this is a longint anyway ! }
              push_value_para(hightree,inlined,false,para_offset,4);
            end;
        end;

      var
         otlabel,oflabel : tasmlabel;
         { temporary variables: }
         tempdeftype : tdeftype;
         r : preference;

      begin
         { set default para_alignment to target_info.stackalignment }
         if para_alignment=0 then
          para_alignment:=aktalignment.paraalign;

         { push from left to right if specified }
         if push_from_left_to_right and assigned(right) then
          begin
            if (nf_varargs_para in flags) then
              tcallparanode(right).secondcallparan(defcoll,push_from_left_to_right,
                                                   inlined,is_cdecl,para_alignment,para_offset)
            else
              tcallparanode(right).secondcallparan(TParaItem(defcoll.next),push_from_left_to_right,
                                                   inlined,is_cdecl,para_alignment,para_offset);
          end;

         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(left);
         { handle varargs first, because defcoll is not valid }
         if (nf_varargs_para in flags) then
           begin
             if push_addr_param(left.resulttype.def) then
               begin
                 inc(pushedparasize,4);
                 emitpushreferenceaddr(left.location.reference);
                 del_reference(left.location.reference);
               end
             else
               push_value_para(left,inlined,is_cdecl,para_offset,para_alignment);
           end
         { filter array constructor with c styled args }
         else if is_array_constructor(left.resulttype.def) and (nf_cargs in left.flags) then
           begin
             { nothing, everything is already pushed }
           end
         { in codegen.handleread.. defcoll.data is set to nil }
         else if assigned(defcoll.paratype.def) and
           (defcoll.paratype.def.deftype=formaldef) then
           begin
              { allow @var }
              inc(pushedparasize,4);
              if (left.nodetype=addrn) and
                 (not(nf_procvarload in left.flags)) then
                begin
                { always a register }
                  if inlined then
                    begin
                       r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                       emit_reg_ref(A_MOV,S_L,
                         left.location.register,r);
                    end
                  else
                    emit_reg(A_PUSH,S_L,left.location.register);
                  ungetregister32(left.location.register);
                end
              else
                begin
                   if not(left.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                     CGMessage(type_e_mismatch)
                   else
                     begin
                       if inlined then
                         begin
                           getexplicitregister32(R_EDI);
                           emit_ref_reg(A_LEA,S_L,
                             newreference(left.location.reference),R_EDI);
                           r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                           emit_reg_ref(A_MOV,S_L,R_EDI,r);
                           ungetregister32(R_EDI);
                         end
                      else
                        emitpushreferenceaddr(left.location.reference);
                        del_reference(left.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll.paratyp in [vs_var,vs_out]) then
           begin
              if (left.location.loc<>LOC_REFERENCE) then
                internalerror(200106041);
              maybe_push_high;
              if (defcoll.paratyp=vs_out) and
                 assigned(defcoll.paratype.def) and
                 not is_class(defcoll.paratype.def) and
                 defcoll.paratype.def.needs_inittable then
                finalize(defcoll.paratype.def,left.location.reference,false);
              inc(pushedparasize,4);
              if inlined then
                begin
                   getexplicitregister32(R_EDI);
                   emit_ref_reg(A_LEA,S_L,
                     newreference(left.location.reference),R_EDI);
                   r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                   emit_reg_ref(A_MOV,S_L,R_EDI,r);
                   ungetregister32(R_EDI);
                end
              else
                emitpushreferenceaddr(left.location.reference);
              del_reference(left.location.reference);
           end
         else
           begin
              tempdeftype:=resulttype.def.deftype;
              if tempdeftype=filedef then
               CGMessage(cg_e_file_must_call_by_reference);
              { open array must always push the address, this is needed to
                also push addr of small open arrays and with cdecl functions (PFV) }
              if (
                  assigned(defcoll.paratype.def) and
                  (is_open_array(defcoll.paratype.def) or
                   is_array_of_const(defcoll.paratype.def))
                 ) or
                 (
                  push_addr_param(resulttype.def) and
                  not is_cdecl
                 ) then
                begin
                   maybe_push_high;
                   inc(pushedparasize,4);
                   if inlined then
                     begin
                        getexplicitregister32(R_EDI);
                        emit_ref_reg(A_LEA,S_L,
                          newreference(left.location.reference),R_EDI);
                        r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                        emit_reg_ref(A_MOV,S_L,R_EDI,r);
                        ungetregister32(R_EDI);
                     end
                   else
                     emitpushreferenceaddr(left.location.reference);
                   del_reference(left.location.reference);
                end
              else
                begin
                   push_value_para(left,inlined,is_cdecl,
                     para_offset,para_alignment);
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(right) then
          begin
            if (nf_varargs_para in flags) then
              tcallparanode(right).secondcallparan(defcoll,push_from_left_to_right,
                                                   inlined,is_cdecl,para_alignment,para_offset)
            else
              tcallparanode(right).secondcallparan(TParaItem(defcoll.next),push_from_left_to_right,
                                                   inlined,is_cdecl,para_alignment,para_offset);
          end;
      end;


{*****************************************************************************
                             TI386CALLNODE
*****************************************************************************}

    procedure ti386callnode.pass_2;
      var
         unusedregisters : tregisterset;
         usablecount : byte;
         pushed : tpushed;
         hr,funcretref,refcountedtemp : treference;
         hregister,hregister2 : tregister;
         oldpushedparasize : longint;
         { true if ESI must be loaded again after the subroutine }
         loadesi : boolean;
         { true if a virtual method must be called directly }
         no_virtual_call : boolean;
         { true if we produce a con- or destrutor in a call }
         is_con_or_destructor : boolean;
         { true if a constructor is called again }
         extended_new : boolean;
         { adress returned from an I/O-error }
         iolabel : tasmlabel;
         { lexlevel count }
         i : longint;
         { help reference pointer }
         r : preference;
         hp : tnode;
         pp : tbinarynode;
         params : tnode;
         inlined : boolean;
         inlinecode : tprocinlinenode;
         store_parast_fixup,
         para_alignment,
         para_offset : longint;
         { instruction for alignement correction }
{        corr : paicpu;}
         { we must pop this size also after !! }
{        must_pop : boolean; }
         pop_size : longint;
{$ifdef OPTALIGN}
         pop_esp : boolean;
         push_size : longint;
{$endif OPTALIGN}
         pop_allowed : boolean;
         regs_to_push : byte;
         constructorfailed : tasmlabel;

      label
         dont_call;

      begin
         reset_reference(location.reference);
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         loadesi:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;
         usablecount:=usablereg32;

         { if we allocate the temp. location for ansi- or widestrings }
         { already here, we avoid later a push/pop                    }
         if is_widestring(resulttype.def) then
           begin
             gettempwidestringreference(refcountedtemp);
             decrstringref(resulttype.def,refcountedtemp);
           end
         else if is_ansistring(resulttype.def) then
           begin
             gettempansistringreference(refcountedtemp);
             decrstringref(resulttype.def,refcountedtemp);
           end;

         if (procdefinition.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_stdcall]) then
          para_alignment:=4
         else
          para_alignment:=aktalignment.paraalign;

         if not assigned(procdefinition) then
          exit;

         { Deciding whether we may still need the parameters happens next (JM) }
         if assigned(left) then
           params:=left.getcopy
         else params := nil;

         if (procdefinition.proccalloption=pocall_inline) then
           begin
              inlined:=true;
              inlinecode:=tprocinlinenode(right);
              right:=nil;
              { set it to the same lexical level as the local symtable, becuase
                the para's are stored there }
              tprocdef(procdefinition).parast.symtablelevel:=aktprocdef.localst.symtablelevel;
              if assigned(params) then
                inlinecode.para_offset:=gettempofsizepersistant(inlinecode.para_size);
              store_parast_fixup:=tprocdef(procdefinition).parast.address_fixup;
              tprocdef(procdefinition).parast.address_fixup:=inlinecode.para_offset;
{$ifdef extdebug}
             Comment(V_debug,
               'inlined parasymtable is at offset '
               +tostr(tprocdef(procdefinition).parast.address_fixup));
             exprasmList.concat(Tai_asm_comment.Create(
               strpnew('inlined parasymtable is at offset '
               +tostr(tprocdef(procdefinition).parast.address_fixup))));
{$endif extdebug}
           end;
         { only if no proc var }
         if inlined or
            not(assigned(right)) then
           is_con_or_destructor:=(procdefinition.proctypeoption in [potype_constructor,potype_destructor]);
         { proc variables destroy all registers }
         if (inlined or
            (right=nil)) and
            { virtual methods too }
            not(po_virtualmethod in procdefinition.procoptions) then
           begin
              if (cs_check_io in aktlocalswitches) and
                 (po_iocheck in procdefinition.procoptions) and
                 not(po_iocheck in aktprocdef.procoptions) then
                begin
                   getaddrlabel(iolabel);
                   emitlab(iolabel);
                end
              else
                iolabel:=nil;

              { save all used registers }
              regs_to_push := tprocdef(procdefinition).usedregisters;
              pushusedregisters(pushed,regs_to_push);

              { give used registers through }
              usedinproc:=usedinproc or tprocdef(procdefinition).usedregisters;
           end
         else
           begin
              regs_to_push := $ff;
              pushusedregisters(pushed,regs_to_push);
              usedinproc:=$ff;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         pop_size:=0;
         { no inc esp for inlined procedure
           and for objects constructors PM }
         if inlined or
            ((procdefinition.proctypeoption=potype_constructor) and
            { quick'n'dirty check if it is a class or an object }
             (resulttype.def.deftype=orddef)) then
           pop_allowed:=false
         else
           pop_allowed:=true;
         if pop_allowed then
          begin
          { Old pushedsize aligned on 4 ? }
            i:=oldpushedparasize and 3;
            if i>0 then
             inc(pop_size,4-i);
          { This parasize aligned on 4 ? }
            i:=procdefinition.para_size(para_alignment) and 3;
            if i>0 then
             inc(pop_size,4-i);
          { insert the opcode and update pushedparasize }
          { never push 4 or more !! }
            pop_size:=pop_size mod 4;
            if pop_size>0 then
             begin
               inc(pushedparasize,pop_size);
               emit_const_reg(A_SUB,S_L,pop_size,R_ESP);
{$ifdef GDB}
               if (cs_debuginfo in aktmoduleswitches) and
                  (exprasmList.first=exprasmList.last) then
                 exprasmList.concat(Tai_force_line.Create);
{$endif GDB}
             end;
          end;
{$ifdef OPTALIGN}
         if pop_allowed and (cs_align in aktglobalswitches) then
           begin
              pop_esp:=true;
              push_size:=procdefinition.para_size(para_alignment);
              { !!!! here we have to take care of return type, self
                and nested procedures
              }
              inc(push_size,12);
              emit_reg_reg(A_MOV,S_L,R_ESP,R_EDI);
              if (push_size mod 8)=0 then
                emit_const_reg(A_AND,S_L,$fffffff8,R_ESP)
              else
                begin
                   emit_const_reg(A_SUB,S_L,push_size,R_ESP);
                   emit_const_reg(A_AND,S_L,$fffffff8,R_ESP);
                   emit_const_reg(A_SUB,S_L,push_size,R_ESP);
                end;
              emit_reg(A_PUSH,S_L,R_EDI);
           end
         else
           pop_esp:=false;
{$endif OPTALIGN}
         if (not is_void(resulttype.def)) and
            ret_in_param(resulttype.def) then
           begin
              funcretref.symbol:=nil;
{$ifdef test_dest_loc}
              if dest_loc_known and (dest_loc_tree=p) and
                 (dest_loc.loc in [LOC_REFERENCE,LOC_MEM]) then
                begin
                   funcretref:=dest_loc.reference;
                   if assigned(dest_loc.reference.symbol) then
                     funcretref.symbol:=stringdup(dest_loc.reference.symbol^);
                   in_dest_loc:=true;
                end
              else
{$endif test_dest_loc}
                if inlined then
                  begin
                     reset_reference(funcretref);
                     funcretref.offset:=gettempofsizepersistant(resulttype.def.size);
                     funcretref.base:=procinfo^.framepointer;
{$ifdef extdebug}
                     Comment(V_debug,'function return value is at offset '
                                     +tostr(funcretref.offset));
                     exprasmlist.concat(tai_asm_comment.create(
                                         strpnew('function return value is at offset '
                                                 +tostr(funcretref.offset))));
{$endif extdebug}
                  end
                else
                  gettempofsizereference(resulttype.def.size,funcretref);
           end;
         if assigned(params) then
           begin
              { be found elsewhere }
              if inlined then
                para_offset:=tprocdef(procdefinition).parast.address_fixup+
                  tprocdef(procdefinition).parast.datasize
              else
                para_offset:=0;
              if not(inlined) and
                 assigned(right) then
                tcallparanode(params).secondcallparan(TParaItem(tabstractprocdef(right.resulttype.def).Para.first),
                  (po_leftright in procdefinition.procoptions),inlined,
                  (procdefinition.proccalloption in [pocall_cdecl,pocall_cppdecl]),
                  para_alignment,para_offset)
              else
                tcallparanode(params).secondcallparan(TParaItem(procdefinition.Para.first),
                  (po_leftright in procdefinition.procoptions),inlined,
                  (procdefinition.proccalloption in [pocall_cdecl,pocall_cppdecl]),
                  para_alignment,para_offset);
           end;
         if inlined then
           inlinecode.retoffset:=gettempofsizepersistant(Align(resulttype.def.size,aktalignment.paraalign));
         if ret_in_param(resulttype.def) then
           begin
              { This must not be counted for C code
                complex return address is removed from stack
                by function itself !   }
{$ifdef OLD_C_STACK}
              inc(pushedparasize,4); { lets try without it PM }
{$endif not OLD_C_STACK}
              if inlined then
                begin
                   getexplicitregister32(R_EDI);
                   emit_ref_reg(A_LEA,S_L,
                     newreference(funcretref),R_EDI);
                   r:=new_reference(procinfo^.framepointer,inlinecode.retoffset);
                   emit_reg_ref(A_MOV,S_L,R_EDI,r);
                   ungetregister32(R_EDI);
                end
              else
                emitpushreferenceaddr(funcretref);
           end;
         { procedure variable ? }
         if inlined or
           (right=nil) then
           begin
              { overloaded operator has no symtable }
              { push self }
              if assigned(symtableproc) and
                (symtableproc.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   methodpointer:=ccallparanode.create(nil,nil);
                   methodpointer.location.loc:=LOC_REGISTER;
                   getexplicitregister32(R_ESI);
                   methodpointer.location.register:=R_ESI;
                   { ARGHHH this is wrong !!!
                     if we can init from base class for a child
                     class that the wrong VMT will be
                     transfered to constructor !! }
                   methodpointer.resulttype:=
                     twithnode(twithsymtable(symtableproc).withnode).left.resulttype;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   { if assigned(ptree(twithsymtable(symtable).withnode)^.pref) then
                     begin
                        r^:=ptree(twithsymtable(symtable).withnode)^.pref^;
                     end
                   else
                     begin
                        r^.offset:=symtable.datasize;
                        r^.base:=procinfo^.framepointer;
                     end; }
                   r^:=twithnode(twithsymtable(symtableproc).withnode).withreference^;
                   if ((not(nf_islocal in twithnode(twithsymtable(symtableproc).withnode).flags)) and
                       (not twithsymtable(symtableproc).direct_with)) or
                      is_class_or_interface(methodpointer.resulttype.def) then
                     emit_ref_reg(A_MOV,S_L,r,R_ESI)
                   else
                     emit_ref_reg(A_LEA,S_L,r,R_ESI);
                end;

              { push self }
              if assigned(symtableproc) and
                ((symtableproc.symtabletype=objectsymtable) or
                (symtableproc.symtabletype=withsymtable)) then
                begin
                   if assigned(methodpointer) then
                     begin
                        {
                        if methodpointer^.resulttype.def=classrefdef then
                          begin
                              two possibilities:
                               1. constructor
                               2. class method

                          end
                        else }
                          begin
                             case methodpointer.nodetype of
                               typen:
                                 begin
                                    { direct call to inherited method }
                                    if (po_abstractmethod in procdefinition.procoptions) then
                                      begin
                                         CGMessage(cg_e_cant_call_abstract_method);
                                         goto dont_call;
                                      end;
                                    { generate no virtual call }
                                    no_virtual_call:=true;

                                    if (sp_static in symtableprocentry.symoptions) then
                                      begin
                                         { well lets put the VMT address directly into ESI }
                                         { it is kind of dirty but that is the simplest    }
                                         { way to accept virtual static functions (PM)     }
                                         loadesi:=true;
                                         { if no VMT just use $0 bug0214 PM }
                                         getexplicitregister32(R_ESI);
                                         if not(oo_has_vmt in tobjectdef(methodpointer.resulttype.def).objectoptions) then
                                           emit_const_reg(A_MOV,S_L,0,R_ESI)
                                         else
                                           begin
                                             emit_sym_ofs_reg(A_MOV,S_L,
                                               newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),
                                               0,R_ESI);
                                           end;
                                         { emit_reg(A_PUSH,S_L,R_ESI);
                                           this is done below !! }
                                      end
                                    else
                                      { this is a member call, so ESI isn't modfied }
                                      loadesi:=false;

                                    { a class destructor needs a flag }
                                    if is_class(tobjectdef(methodpointer.resulttype.def)) and
                                       (procdefinition.proctypeoption=potype_destructor) then
                                      begin
                                        push_int(0);
                                        emit_reg(A_PUSH,S_L,R_ESI);
                                      end;

                                    if not(is_con_or_destructor and
                                           is_class(methodpointer.resulttype.def) and
                                           (procdefinition.proctypeoption in [potype_constructor,potype_destructor])
                                          ) then
                                      emit_reg(A_PUSH,S_L,R_ESI);
                                    { if an inherited con- or destructor should be  }
                                    { called in a con- or destructor then a warning }
                                    { will be made                                  }
                                    { con- and destructors need a pointer to the vmt }
                                    if is_con_or_destructor and
                                      is_object(methodpointer.resulttype.def) and
                                      assigned(aktprocdef) then
                                      begin
                                         if not(aktprocdef.proctypeoption in
                                                [potype_constructor,potype_destructor]) then
                                          CGMessage(cg_w_member_cd_call_from_method);
                                      end;
                                    { class destructors get there flag above }
                                    { constructor flags ?                    }
                                    if is_con_or_destructor and
                                      not(
                                        is_class(methodpointer.resulttype.def) and
                                        assigned(aktprocdef) and
                                        (aktprocdef.proctypeoption=potype_destructor)) then
                                      begin
                                         { a constructor needs also a flag }
                                         if is_class(methodpointer.resulttype.def) then
                                           push_int(0);
                                         push_int(0);
                                      end;
                                 end;
                               hnewn:
                                 begin
                                    { extended syntax of new }
                                    { ESI must be zero }
                                    getexplicitregister32(R_ESI);
                                    emit_reg_reg(A_XOR,S_L,R_ESI,R_ESI);
                                    emit_reg(A_PUSH,S_L,R_ESI);
                                    { insert the vmt }
                                    emit_sym(A_PUSH,S_L,
                                      newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname));
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE          }
                                    getexplicitregister32(R_ESI);
                                    emit_ref_reg(A_LEA,S_L,
                                      newreference(methodpointer.location.reference),R_ESI);
                                    del_reference(methodpointer.location.reference);
                                    emit_reg(A_PUSH,S_L,R_ESI);
                                    emit_sym(A_PUSH,S_L,
                                      newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname));
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (symtableproc.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(methodpointer);
                                         getexplicitregister32(R_ESI);
                                         case methodpointer.location.loc of
                                            LOC_CREGISTER,
                                            LOC_REGISTER:
                                              begin
                                                 emit_reg_reg(A_MOV,S_L,methodpointer.location.register,R_ESI);
                                                 ungetregister32(methodpointer.location.register);
                                              end;
                                            else
                                              begin
                                                 if (methodpointer.resulttype.def.deftype=classrefdef) or
                                                    is_class_or_interface(methodpointer.resulttype.def) then
                                                   emit_ref_reg(A_MOV,S_L,
                                                     newreference(methodpointer.location.reference),R_ESI)
                                                 else
                                                   emit_ref_reg(A_LEA,S_L,
                                                     newreference(methodpointer.location.reference),R_ESI);
                                                 del_reference(methodpointer.location.reference);
                                              end;
                                         end;
                                      end;
                                    { when calling a class method, we have to load ESI with the VMT !
                                      But, not for a class method via self }
                                    if not(po_containsself in procdefinition.procoptions) then
                                      begin
                                        if (po_classmethod in procdefinition.procoptions) and
                                           not(methodpointer.resulttype.def.deftype=classrefdef) then
                                          begin
                                             { class method needs current VMT }
                                             getexplicitregister32(R_ESI);
                                             new(r);
                                             reset_reference(r^);
                                             r^.base:=R_ESI;
                                             r^.offset:= tprocdef(procdefinition)._class.vmt_offset;
                                             emit_ref_reg(A_MOV,S_L,r,R_ESI);
                                          end;

                                        { direct call to destructor: remove data }
                                        if (procdefinition.proctypeoption=potype_destructor) and
                                           is_class(methodpointer.resulttype.def) then
                                          emit_const(A_PUSH,S_L,1);

                                        { direct call to class constructor, don't allocate memory }
                                        if (procdefinition.proctypeoption=potype_constructor) and
                                           is_class(methodpointer.resulttype.def) then
                                          begin
                                             emit_const(A_PUSH,S_L,0);
                                             emit_const(A_PUSH,S_L,0);
                                          end
                                        else
                                          begin
                                             { constructor call via classreference => allocate memory }
                                             if (procdefinition.proctypeoption=potype_constructor) and
                                                (methodpointer.resulttype.def.deftype=classrefdef) and
                                                is_class(tclassrefdef(methodpointer.resulttype.def).pointertype.def) then
                                                emit_const(A_PUSH,S_L,1);
                                             emit_reg(A_PUSH,S_L,R_ESI);
                                          end;
                                      end;

                                    if is_con_or_destructor then
                                      begin
                                         { classes don't get a VMT pointer pushed }
                                         if is_object(methodpointer.resulttype.def) then
                                           begin
                                              if (procdefinition.proctypeoption=potype_constructor) then
                                                begin
                                                   { it's no bad idea, to insert the VMT }
                                                   emit_sym(A_PUSH,S_L,newasmsymbol(
                                                     tobjectdef(methodpointer.resulttype.def).vmt_mangledname));
                                                end
                                              { destructors haven't to dispose the instance, if this is }
                                              { a direct call                                           }
                                              else
                                                push_int(0);
                                           end;
                                      end;
                                 end;
                             end;
                          end;
                     end
                   else
                     begin
                        if (po_classmethod in procdefinition.procoptions) and
                          not(
                            assigned(aktprocdef) and
                            (po_classmethod in aktprocdef.procoptions)
                          ) then
                          begin
                             { class method needs current VMT }
                             getexplicitregister32(R_ESI);
                             new(r);
                             reset_reference(r^);
                             r^.base:=R_ESI;
                             r^.offset:= tprocdef(procdefinition)._class.vmt_offset;
                             emit_ref_reg(A_MOV,S_L,r,R_ESI);
                          end
                        else
                          begin
                             { member call, ESI isn't modified }
                             loadesi:=false;
                          end;
                        { direct call to destructor: don't remove data! }
                        if is_class(procinfo^._class) then
                          begin
                             if (procdefinition.proctypeoption=potype_destructor) then
                               begin
                                  emit_const(A_PUSH,S_L,0);
                                  emit_reg(A_PUSH,S_L,R_ESI);
                               end
                             else if (procdefinition.proctypeoption=potype_constructor) then
                               begin
                                  emit_const(A_PUSH,S_L,0);
                                  emit_const(A_PUSH,S_L,0);
                               end
                             else
                               emit_reg(A_PUSH,S_L,R_ESI);
                          end
                        else if is_object(procinfo^._class) then
                          begin
                             emit_reg(A_PUSH,S_L,R_ESI);
                             if is_con_or_destructor then
                               begin
                                  if (procdefinition.proctypeoption=potype_constructor) then
                                    begin
                                       { it's no bad idea, to insert the VMT }
                                       emit_sym(A_PUSH,S_L,newasmsymbol(
                                         procinfo^._class.vmt_mangledname));
                                    end
                                  { destructors haven't to dispose the instance, if this is }
                                  { a direct call                                           }
                                  else
                                    push_int(0);
                               end;
                          end
                        else
                          Internalerror(200006165);
                     end;
                end;

                { call to BeforeDestruction? }
                if (procdefinition.proctypeoption=potype_destructor) and
                   assigned(methodpointer) and
                   (methodpointer.nodetype<>typen) and
                   is_class(tobjectdef(methodpointer.resulttype.def)) and
                   (inlined or
                   (right=nil)) then
                  begin
                     emit_reg(A_PUSH,S_L,R_ESI);
                     new(r);
                     reset_reference(r^);
                     r^.base:=R_ESI;
                     getexplicitregister32(R_EDI);
                     emit_ref_reg(A_MOV,S_L,r,R_EDI);
                     new(r);
                     reset_reference(r^);
                     r^.offset:=72;
                     r^.base:=R_EDI;
                     emit_ref(A_CALL,S_NO,r);
                     ungetregister32(R_EDI);
                  end;

              { push base pointer ?}
              { never when inlining, since if necessary, the base pointer }
              { can/will be gottten from the current procedure's symtable }
              { (JM)                                                      }
              if not inlined then
                if (lexlevel>=normal_function_level) and assigned(tprocdef(procdefinition).parast) and
                  ((tprocdef(procdefinition).parast.symtablelevel)>normal_function_level) then
                  begin
                     { if we call a nested function in a method, we must      }
                     { push also SELF!                                    }
                     { THAT'S NOT TRUE, we have to load ESI via frame pointer }
                     { access                                              }
                     {
                       begin
                          loadesi:=false;
                          emit_reg(A_PUSH,S_L,R_ESI);
                       end;
                     }
                     if lexlevel=(tprocdef(procdefinition).parast.symtablelevel) then
                       begin
                          new(r);
                          reset_reference(r^);
                          r^.offset:=procinfo^.framepointer_offset;
                          r^.base:=procinfo^.framepointer;
                          emit_ref(A_PUSH,S_L,r)
                       end
                       { this is only true if the difference is one !!
                         but it cannot be more !! }
                     else if (lexlevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
                       begin
                          emit_reg(A_PUSH,S_L,procinfo^.framepointer)
                       end
                     else if (lexlevel>(tprocdef(procdefinition).parast.symtablelevel)) then
                       begin
                          hregister:=getregister32;
                          new(r);
                          reset_reference(r^);
                          r^.offset:=procinfo^.framepointer_offset;
                          r^.base:=procinfo^.framepointer;
                          emit_ref_reg(A_MOV,S_L,r,hregister);
                          for i:=(tprocdef(procdefinition).parast.symtablelevel) to lexlevel-1 do
                            begin
                               new(r);
                               reset_reference(r^);
                               {we should get the correct frame_pointer_offset at each level
                               how can we do this !!! }
                               r^.offset:=procinfo^.framepointer_offset;
                               r^.base:=hregister;
                               emit_ref_reg(A_MOV,S_L,r,hregister);
                            end;
                          emit_reg(A_PUSH,S_L,hregister);
                          ungetregister32(hregister);
                       end
                     else
                       internalerror(25000);
                  end;

              saveregvars(regs_to_push);

              if (po_virtualmethod in procdefinition.procoptions) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in ESI }
                   { also class methods                       }
                   { Here it is quite tricky because it also depends }
                   { on the methodpointer                        PM }
                   getexplicitregister32(R_ESI);
                   if assigned(aktprocdef) then
                     begin
                       if (((sp_static in aktprocdef.procsym.symoptions) or
                        (po_classmethod in aktprocdef.procoptions)) and
                        ((methodpointer=nil) or (methodpointer.nodetype=typen)))
                        or
                        (po_staticmethod in procdefinition.procoptions) or
                        ((procdefinition.proctypeoption=potype_constructor) and
                        { esi contains the vmt if we call a constructor via a class ref }
                         assigned(methodpointer) and
                         (methodpointer.resulttype.def.deftype=classrefdef)
                        ) or
                        { is_interface(tprocdef(procdefinition)._class) or }
                        { ESI is loaded earlier }
                        (po_classmethod in procdefinition.procoptions) then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                            { this is one point where we need vmt_offset (PM) }
                            r^.offset:= tprocdef(procdefinition)._class.vmt_offset;
                            getexplicitregister32(R_EDI);
                            emit_ref_reg(A_MOV,S_L,r,R_EDI);
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_EDI;
                         end;
                     end
                   else
                     { aktprocdef should be assigned, also in main program }
                     internalerror(12345);
                   {
                     begin
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_ESI;
                       emit_ref_reg(A_MOV,S_L,r,R_EDI);
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_EDI;
                     end;
                   }
                   if tprocdef(procdefinition).extnumber=-1 then
                     internalerror(44584);
                   r^.offset:=tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber);
                   if not(is_interface(tprocdef(procdefinition)._class)) and
                     not(is_cppclass(tprocdef(procdefinition)._class)) then
                     begin
                        if (cs_check_object_ext in aktlocalswitches) then
                          begin
                             emit_sym(A_PUSH,S_L,
                               newasmsymbol(tprocdef(procdefinition)._class.vmt_mangledname));
                             emit_reg(A_PUSH,S_L,r^.base);
                             emitcall('FPC_CHECK_OBJECT_EXT');
                          end
                        else if (cs_check_range in aktlocalswitches) then
                          begin
                             emit_reg(A_PUSH,S_L,r^.base);
                             emitcall('FPC_CHECK_OBJECT');
                          end;
                     end;
                   emit_ref(A_CALL,S_NO,r);
                   ungetregister32(R_EDI);
                end
              else if not inlined then
                begin
                  { We can call interrupts from within the smae code
                    by just pushing the flags and CS PM }
                  if (po_interrupt in procdefinition.procoptions) then
                    begin
                        emit_none(A_PUSHF,S_L);
                        emit_reg(A_PUSH,S_L,R_CS);
                    end;
                  emitcall(tprocdef(procdefinition).mangledname);
                end
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   { process the inlinecode }
                   secondpass(inlinecode);
                   { free the args }
                   if tprocdef(procdefinition).parast.datasize>0 then
                     ungetpersistanttemp(tprocdef(procdefinition).parast.address_fixup);
                end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(right);
              if (po_interrupt in procdefinition.procoptions) then
                begin
                    emit_none(A_PUSHF,S_L);
                    emit_reg(A_PUSH,S_L,R_CS);
                end;
              { procedure of object? }
              if (po_methodpointer in procdefinition.procoptions) then
                begin
                   { method pointer can't be in a register }
                   hregister:=R_NO;

                   { do some hacking if we call a method pointer }
                   { which is a class member                 }
                   { else ESI is overwritten !             }
                   if (right.location.reference.base=R_ESI) or
                      (right.location.reference.index=R_ESI) then
                     begin
                        del_reference(right.location.reference);
                        getexplicitregister32(R_EDI);
                        emit_ref_reg(A_MOV,S_L,
                          newreference(right.location.reference),R_EDI);
                        hregister:=R_EDI;
                     end;

                   { load self, but not if it's already explicitly pushed }
                   if not(po_containsself in procdefinition.procoptions) then
                     begin
                       { load ESI }
                       inc(right.location.reference.offset,4);
                       getexplicitregister32(R_ESI);
                       emit_ref_reg(A_MOV,S_L,
                         newreference(right.location.reference),R_ESI);
                       dec(right.location.reference.offset,4);
                       { push self pointer }
                       emit_reg(A_PUSH,S_L,R_ESI);
                     end;

                   saveregvars($ff);
                   if hregister=R_NO then
                     emit_ref(A_CALL,S_NO,newreference(right.location.reference))
                   else
                     begin
                       emit_reg(A_CALL,S_NO,hregister);
                       ungetregister32(hregister);
                     end;

                   del_reference(right.location.reference);
                end
              else
                begin
                   saveregvars($ff);
                   case right.location.loc of
                      LOC_REGISTER,LOC_CREGISTER:
                         begin
                             emit_reg(A_CALL,S_NO,right.location.register);
                             ungetregister32(right.location.register);
                         end
                      else
                         begin
                           emit_ref(A_CALL,S_NO,newreference(right.location.reference));
                           del_reference(right.location.reference);
                         end;
                   end;
                end;
           end;

           { this was only for normal functions
             displaced here so we also get
             it to work for procvars PM }
           if (not inlined) and (po_clearstack in procdefinition.procoptions) then
             begin
                { we also add the pop_size which is included in pushedparasize }
                pop_size:=0;
                { better than an add on all processors }
                if pushedparasize=4 then
                  begin
                    getexplicitregister32(R_EDI);
                    emit_reg(A_POP,S_L,R_EDI);
                    ungetregister32(R_EDI);
                  end
                { the pentium has two pipes and pop reg is pairable }
                { but the registers must be different!        }
                else if (pushedparasize=8) and
                  not(cs_littlesize in aktglobalswitches) and
                  (aktoptprocessor=ClassP5) and
                  (procinfo^._class=nil) then
                    begin
                       getexplicitregister32(R_EDI);
                       emit_reg(A_POP,S_L,R_EDI);
                       ungetregister32(R_EDI);
                       exprasmList.concat(Tairegalloc.Alloc(R_ESI));
                       emit_reg(A_POP,S_L,R_ESI);
                       exprasmList.concat(Tairegalloc.DeAlloc(R_ESI));
                    end
                else if pushedparasize<>0 then
                  emit_const_reg(A_ADD,S_L,pushedparasize,R_ESP);
             end;
{$ifdef OPTALIGN}
         if pop_esp then
           emit_reg(A_POP,S_L,R_ESP);
{$endif OPTALIGN}
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;
         usablereg32:=usablecount;
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}

         { a constructor could be a function with boolean result }
         { if calling constructor called fail we
           must jump directly to quickexitlabel  PM
           but only if it is a call of an inherited constructor }
         if (inlined or
             (right=nil)) and
            (procdefinition.proctypeoption=potype_constructor) and
            assigned(methodpointer) and
            (methodpointer.nodetype=typen) and
            (aktprocdef.proctypeoption=potype_constructor) then
           begin
             emitjmp(C_Z,faillabel);
           end;

         { call to AfterConstruction? }
         if is_class(resulttype.def) and
           (inlined or
           (right=nil)) and
           (procdefinition.proctypeoption=potype_constructor) and
           assigned(methodpointer) and
           (methodpointer.nodetype<>typen) then
           begin
              getlabel(constructorfailed);
              emitjmp(C_Z,constructorfailed);
              emit_reg(A_PUSH,S_L,R_ESI);
              new(r);
              reset_reference(r^);
              r^.base:=R_ESI;
              getexplicitregister32(R_EDI);
              emit_ref_reg(A_MOV,S_L,r,R_EDI);
              new(r);
              reset_reference(r^);
              r^.offset:=68;
              r^.base:=R_EDI;
              emit_ref(A_CALL,S_NO,r);
              ungetregister32(R_EDI);
              exprasmList.concat(Tairegalloc.Alloc(R_EAX));
              emitlab(constructorfailed);
              emit_reg_reg(A_MOV,S_L,R_ESI,R_EAX);
           end;

         { handle function results }
         { structured results are easy to handle.... }
         { needed also when result_no_used !! }
         if (not is_void(resulttype.def)) and ret_in_param(resulttype.def) then
           begin
              location.loc:=LOC_MEM;
              location.reference.symbol:=nil;
              location.reference:=funcretref;
           end;
         { we have only to handle the result if it is used, but }
         { ansi/widestrings must be registered, so we can dispose them }
         if (not is_void(resulttype.def)) and ((nf_return_value_used in flags) or
           is_ansistring(resulttype.def) or is_widestring(resulttype.def)) then
           begin
              { a contructor could be a function with boolean result }
              if (inlined or
                  (right=nil)) and
                 (procdefinition.proctypeoption=potype_constructor) and
                 { quick'n'dirty check if it is a class or an object }
                 (resulttype.def.deftype=orddef) then
                begin
                   { this fails if popsize > 0 PM }
                   location.loc:=LOC_FLAGS;
                   location.resflags:=F_NE;


                   if extended_new then
                     begin
{$ifdef test_dest_loc}
                        if dest_loc_known and (dest_loc_tree=p) then
                          mov_reg_to_dest(p,S_L,R_EAX)
                        else
{$endif test_dest_loc}
                          begin
                             hregister:=getexplicitregister32(R_EAX);
                             emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                             location.register:=hregister;
                          end;
                     end;
                end
               { structed results are easy to handle.... }
              else if ret_in_param(resulttype.def) then
                begin
                   {location.loc:=LOC_MEM;
                   stringdispose(location.reference.symbol);
                   location.reference:=funcretref;
                   already done above (PM) }
                end
              else
                begin
                   if (resulttype.def.deftype in [orddef,enumdef]) then
                     begin
                        location.loc:=LOC_REGISTER;
                        case resulttype.def.size of
                          4 :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_L,R_EAX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getexplicitregister32(R_EAX);
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    location.register:=hregister;
                                 end;
                            end;
                          1 :
                            begin
{$ifdef test_dest_loc}
                                 if dest_loc_known and (dest_loc_tree=p) then
                                   mov_reg_to_dest(p,S_B,R_AL)
                                 else
{$endif test_dest_loc}
                                   begin
                                      hregister:=getexplicitregister32(R_EAX);
                                      emit_reg_reg(A_MOV,S_B,R_AL,reg32toreg8(hregister));
                                      location.register:=reg32toreg8(hregister);
                                   end;
                              end;
                          2 :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_W,R_AX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getexplicitregister32(R_EAX);
                                    emit_reg_reg(A_MOV,S_W,R_AX,reg32toreg16(hregister));
                                    location.register:=reg32toreg16(hregister);
                                 end;
                            end;
                           8 :
                             begin
{$ifdef test_dest_loc}
{$error Don't know what to do here}
{$endif test_dest_loc}
                                if R_EDX in unused then
                                  begin
                                     hregister2:=getexplicitregister32(R_EDX);
                                     hregister:=getexplicitregister32(R_EAX);
                                  end
                                else
                                  begin
                                     hregister:=getexplicitregister32(R_EAX);
                                     hregister2:=getexplicitregister32(R_EDX);
                                  end;
                                emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                emit_reg_reg(A_MOV,S_L,R_EDX,hregister2);
                                location.registerlow:=hregister;
                                location.registerhigh:=hregister2;
                             end;
                        else internalerror(7);
                     end

                end
              else if (resulttype.def.deftype=floatdef) then
                begin
                  location.loc:=LOC_FPU;
                  inc(fpuvaroffset);
                end
              else if is_ansistring(resulttype.def) or
                is_widestring(resulttype.def) then
                begin
                   emit_reg_ref(A_MOV,S_L,R_EAX,
                     newreference(refcountedtemp));
                   location.loc:=LOC_MEM;
                   location.reference:=refcountedtemp;
                end
              else
                begin
                   location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                   if dest_loc_known and (dest_loc_tree=p) then
                     mov_reg_to_dest(p,S_L,R_EAX)
                   else
{$endif test_dest_loc}
                    begin
                       hregister:=getexplicitregister32(R_EAX);
                       emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                       location.register:=hregister;
                    end;
                end;
             end;
           end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              emit_sym(A_PUSH,S_L,iolabel);
              emitcall('FPC_IOCHECK');
           end;
         if pop_size>0 then
           emit_const_reg(A_ADD,S_L,pop_size,R_ESP);

         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loadesi then
           maybe_loadself;
         pp:=tbinarynode(params);
         while assigned(pp) do
           begin
              if assigned(pp.left) then
                begin
                  if (pp.left.location.loc in [LOC_REFERENCE,LOC_MEM]) then
                    ungetiftemp(pp.left.location.reference);
                { process also all nodes of an array of const }
                  if pp.left.nodetype=arrayconstructorn then
                    begin
                      if assigned(tarrayconstructornode(pp.left).left) then
                       begin
                         hp:=pp.left;
                         while assigned(hp) do
                          begin
                            if (tarrayconstructornode(tunarynode(hp).left).location.loc in [LOC_REFERENCE,LOC_MEM]) then
                              ungetiftemp(tarrayconstructornode(hp).left.location.reference);
                            hp:=tbinarynode(hp).right;
                          end;
                       end;
                    end;
                end;
              pp:=tbinarynode(pp.right);
           end;
         if inlined then
           begin
             ungetpersistanttemp(inlinecode.retoffset);
             tprocdef(procdefinition).parast.address_fixup:=store_parast_fixup;
             right:=inlinecode;
           end;
         if assigned(params) then
           params.free;

         { from now on the result can be freed normally }
         if inlined and ret_in_param(resulttype.def) then
           persistanttemptonormal(funcretref.offset);

         { if return value is not used }
         if (not(nf_return_value_used in flags)) and (not is_void(resulttype.def)) then
           begin
              if location.loc in [LOC_MEM,LOC_REFERENCE] then
                begin
                   { data which must be finalized ? }
                   if (resulttype.def.needs_inittable) then
                      finalize(resulttype.def,location.reference,false);
                   { release unused temp }
                   ungetiftemp(location.reference)
                end
              else if location.loc=LOC_FPU then
                begin
                  { release FPU stack }
                  emit_reg(A_FSTP,S_NO,R_ST0);
                  {
                    dec(fpuvaroffset);
                    do NOT decrement as the increment before
                    is not called for unused results PM }
                end;
           end;
      end;



{*****************************************************************************
                             TI386PROCINLINENODE
*****************************************************************************}


    procedure ti386procinlinenode.pass_2;
       var st : tsymtable;
           oldprocdef : tprocdef;
           ps, i : longint;
           tmpreg: tregister;
           oldprocinfo : pprocinfo;
           oldinlining_procedure,
           nostackframe,make_global : boolean;
           inlineentrycode,inlineexitcode : TAAsmoutput;
           oldexitlabel,oldexit2label,oldquickexitlabel:tasmlabel;
           oldunused,oldusableregs : tregisterset;
           oldc_usableregs : longint;
           oldreg_pushes : regvar_longintarray;
           oldregvar_loaded,
           oldis_reg_var       : regvar_booleanarray;
{$ifdef TEMPREGDEBUG}
           oldreg_user   : regvar_ptreearray;
           oldreg_releaser : regvar_ptreearray;
{$endif TEMPREGDEBUG}
{$ifdef GDB}
           startlabel,endlabel : tasmlabel;
           pp : pchar;
           mangled_length  : longint;
{$endif GDB}
       begin
          { deallocate the registers used for the current procedure's regvars }
          if assigned(aktprocdef.regvarinfo) then
            begin
              with pregvarinfo(aktprocdef.regvarinfo)^ do
                for i := 1 to maxvarregs do
                  if assigned(regvars[i]) then
                    store_regvar(exprasmlist,regvars[i].reg);
              oldunused := unused;
              oldusableregs := usableregs;
              oldc_usableregs := c_usableregs;
              oldreg_pushes := reg_pushes;
              oldis_reg_var := is_reg_var;
              oldregvar_loaded := regvar_loaded;
{$ifdef TEMPREGDEBUG}
              oldreg_user := reg_user;
              oldreg_releaser := reg_releaser;
{$endif TEMPREGDEBUG}
              { make sure the register allocator knows what the regvars in the }
              { inlined code block are (JM)                                    }
              resetusableregisters;
              clearregistercount;
              cleartempgen;
              if assigned(inlineprocdef.regvarinfo) then
                with pregvarinfo(inlineprocdef.regvarinfo)^ do
                 for i := 1 to maxvarregs do
                  if assigned(regvars[i]) then
                    begin
                      case regsize(regvars[i].reg) of
                        S_B: tmpreg := reg8toreg32(regvars[i].reg);
                        S_W: tmpreg := reg16toreg32(regvars[i].reg);
                        S_L: tmpreg := regvars[i].reg;
                      end;
                      usableregs:=usableregs-[tmpreg];
                      is_reg_var[tmpreg]:=true;
                      dec(c_usableregs);
                    end;
            end;
          oldinlining_procedure:=inlining_procedure;
          oldexitlabel:=aktexitlabel;
          oldexit2label:=aktexit2label;
          oldquickexitlabel:=quickexitlabel;
          getlabel(aktexitlabel);
          getlabel(aktexit2label);
          { we're inlining a procedure }
          inlining_procedure:=true;
          { save old procinfo }
          oldprocdef:=aktprocdef;
          getmem(oldprocinfo,sizeof(tprocinfo));
          move(procinfo^,oldprocinfo^,sizeof(tprocinfo));
          { set new procinfo }
          aktprocdef:=inlineprocdef;
          procinfo^.return_offset:=retoffset;
          procinfo^.para_offset:=para_offset;
          procinfo^.no_fast_exit:=false;
          { arg space has been filled by the parent secondcall }
          st:=aktprocdef.localst;
          { set it to the same lexical level }
          st.symtablelevel:=oldprocdef.localst.symtablelevel;
          if st.datasize>0 then
            begin
              st.address_fixup:=gettempofsizepersistant(st.datasize)+st.datasize;
{$ifdef extdebug}
              Comment(V_debug,'local symtable is at offset '+tostr(st.address_fixup));
              exprasmList.concat(Tai_asm_comment.Create(strpnew(
                'local symtable is at offset '+tostr(st.address_fixup))));
{$endif extdebug}
            end;
          exprasmList.concat(Tai_Marker.Create(InlineStart));
{$ifdef extdebug}
          exprasmList.concat(Tai_asm_comment.Create(strpnew('Start of inlined proc')));
{$endif extdebug}
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
            begin
              getaddrlabel(startlabel);
              getaddrlabel(endlabel);
              emitlab(startlabel);
              inlineprocdef.localst.symtabletype:=inlinelocalsymtable;
              inlineprocdef.parast.symtabletype:=inlineparasymtable;

              { Here we must include the para and local symtable info }
              inlineprocdef.concatstabto(withdebuglist);

              { set it back for safety }
              inlineprocdef.localst.symtabletype:=localsymtable;
              inlineprocdef.parast.symtabletype:=parasymtable;

              mangled_length:=length(oldprocdef.mangledname);
              getmem(pp,mangled_length+50);
              strpcopy(pp,'192,0,0,'+startlabel.name);
              if (target_info.use_function_relative_addresses) then
                begin
                  strpcopy(strend(pp),'-');
                  strpcopy(strend(pp),oldprocdef.mangledname);
                end;
              withdebugList.concat(Tai_stabn.Create(strnew(pp)));
            end;
{$endif GDB}
          { takes care of local data initialization }
          inlineentrycode:=TAAsmoutput.Create;
          inlineexitcode:=TAAsmoutput.Create;
          ps:=para_size;
          make_global:=false; { to avoid warning }
          genentrycode(inlineentrycode,make_global,0,ps,nostackframe,true);
          if po_assembler in aktprocdef.procoptions then
            inlineentrycode.insert(Tai_marker.Create(asmblockstart));
          exprasmList.concatlist(inlineentrycode);
          secondpass(inlinetree);
          genexitcode(inlineexitcode,0,false,true);
          if po_assembler in aktprocdef.procoptions then
            inlineexitcode.concat(Tai_marker.Create(asmblockend));
          exprasmList.concatlist(inlineexitcode);

          inlineentrycode.free;
          inlineexitcode.free;
{$ifdef extdebug}
          exprasmList.concat(Tai_asm_comment.Create(strpnew('End of inlined proc')));
{$endif extdebug}
          exprasmList.concat(Tai_Marker.Create(InlineEnd));

          {we can free the local data now, reset also the fixup address }
          if st.datasize>0 then
            begin
              ungetpersistanttemp(st.address_fixup-st.datasize);
              st.address_fixup:=0;
            end;
          { restore procinfo }
          move(oldprocinfo^,procinfo^,sizeof(tprocinfo));
          freemem(oldprocinfo,sizeof(tprocinfo));
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
            begin
              emitlab(endlabel);
              strpcopy(pp,'224,0,0,'+endlabel.name);
             if (target_info.use_function_relative_addresses) then
               begin
                 strpcopy(strend(pp),'-');
                 strpcopy(strend(pp),oldprocdef.mangledname);
               end;
              withdebugList.concat(Tai_stabn.Create(strnew(pp)));
              freemem(pp,mangled_length+50);
            end;
{$endif GDB}
          { restore }
          aktprocdef:=oldprocdef;
          aktexitlabel:=oldexitlabel;
          aktexit2label:=oldexit2label;
          quickexitlabel:=oldquickexitlabel;
          inlining_procedure:=oldinlining_procedure;

          { reallocate the registers used for the current procedure's regvars, }
          { since they may have been used and then deallocated in the inlined  }
          { procedure (JM)                                                     }
          if assigned(aktprocdef.regvarinfo) then
            begin
              unused := oldunused;
              usableregs := oldusableregs;
              c_usableregs := oldc_usableregs;
              reg_pushes := oldreg_pushes;
              is_reg_var := oldis_reg_var;
              regvar_loaded := oldregvar_loaded;
{$ifdef TEMPREGDEBUG}
              reg_user := oldreg_user;
              reg_releaser := oldreg_releaser;
{$endif TEMPREGDEBUG}
            end;
       end;


begin
   ccallparanode:=ti386callparanode;
   ccallnode:=ti386callnode;
   cprocinlinenode:=ti386procinlinenode;
end.
{
  $Log$
  Revision 1.39  2001-12-29 15:32:13  jonas
    * powerpc/cgcpu.pas compiles :)
    * several powerpc-related fixes
    * cpuasm unit is now based on common tainst unit
    + nppcmat unit for powerpc (almost complete)

  Revision 1.38  2001/11/18 00:00:34  florian
    * handling of ansi- and widestring results improved

  Revision 1.37  2001/11/02 23:24:40  peter
    * fixed crash with inlining after aktprocdef change

  Revision 1.36  2001/11/02 22:58:09  peter
    * procsym definition rewrite

  Revision 1.35  2001/10/25 21:22:41  peter
    * calling convention rewrite

  Revision 1.34  2001/10/21 12:33:07  peter
    * array access for properties added

  Revision 1.33  2001/09/09 08:50:15  jonas
    * when calling an inline procedure inside a nested procedure, the
      framepointer was being pushed on the stack, but this pushed framepointer
      was never used nor removed from the stack again after the inlining was
      done. It's now simply not pushed anymore, because the inlined procedure
      can get the previous framepointer from the procedure in which it is being
      inlined (merged)

  Revision 1.32  2001/09/01 23:02:30  jonas
    * i386*: call and jmp read their first operand
    * cgcal: deallocate hlper register only after call statement (fixes bug
      with "procedure of object" and optimizer reported to bugrep on
      2001/08/30) ('merged')

  Revision 1.31  2001/08/29 12:18:08  jonas
    + new createinternres() constructor for tcallnode to support setting a
      custom resulttype
    * compilerproc typeconversions now set the resulttype from the type
      conversion for the generated call node, because the resulttype of
      of the compilerproc helper isn't always exact (e.g. the ones that
      return shortstrings, actually return a shortstring[x], where x is
      specified by the typeconversion node)
    * ti386callnode.pass_2 now always uses resulttype instead of
      procsym.definition.rettype (so the custom resulttype, if any, is
      always used). Note that this "rettype" stuff is only for use with
      compilerprocs.

  Revision 1.30  2001/08/26 13:36:56  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.29  2001/08/19 21:11:21  florian
    * some bugs fix:
      - overload; with external procedures fixed
      - better selection of routine to do an overloaded
        type case
      - ... some more

  Revision 1.28  2001/08/06 21:40:50  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.27  2001/07/08 21:00:16  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.26  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.25  2001/06/04 11:48:02  peter
    * better const to var checking

  Revision 1.24  2001/05/19 21:22:53  peter
    * function returning int64 inlining fixed

  Revision 1.23  2001/05/16 15:11:42  jonas
    * added missign begin..end pair (noticed by Carl)

  Revision 1.22  2001/04/18 22:02:01  peter
    * registration of targets and assemblers

  Revision 1.21  2001/04/13 01:22:18  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.20  2001/04/02 21:20:36  peter
    * resulttype rewrite

  Revision 1.19  2001/03/11 22:58:51  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.18  2001/01/27 21:29:35  florian
     * behavior -Oa optimized

  Revision 1.17  2001/01/08 21:46:46  peter
    * don't push high value for open array with cdecl;external;

  Revision 1.16  2000/12/25 00:07:32  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.15  2000/12/09 10:45:40  florian
    * AfterConstructor isn't called anymore when a constructor failed

  Revision 1.14  2000/12/07 17:19:46  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.13  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.12  2000/12/03 22:26:54  florian
    * fixed web buzg 1275: problem with int64 functions results

  Revision 1.11  2000/11/29 00:30:46  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.10  2000/11/23 13:26:34  jonas
    * fix for webbug 1066/1126

  Revision 1.9  2000/11/22 15:12:06  jonas
    * fixed inline-related problems (partially "merges")

  Revision 1.8  2000/11/17 09:54:58  florian
    * INT_CHECK_OBJECT_* isn't applied to interfaces anymore

  Revision 1.7  2000/11/12 23:24:14  florian
    * interfaces are basically running

  Revision 1.6  2000/11/07 23:40:49  florian
    + AfterConstruction and BeforeDestruction impemented

  Revision 1.5  2000/11/06 23:15:01  peter
    * added copyvaluepara call again

  Revision 1.4  2000/11/04 14:25:23  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.3  2000/11/04 13:12:14  jonas
    * check for nil pointers before calling getcopy

  Revision 1.2  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.2  2000/10/14 10:14:48  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.1  2000/10/10 17:31:56  florian
    * initial revision

}
