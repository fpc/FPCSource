{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for in call nodes

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
unit cg386cal;
interface

{ $define AnsiStrRef}

    uses
      symtable,tree;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right,inlined,dword_align : boolean;para_offset : longint);
    procedure secondcalln(var p : ptree);
    procedure secondprocinline(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,
      aasm,types,
{$ifdef GDB}
      gdb,
{$endif GDB}
      hcodegen,temp_gen,pass_2,
{$ifndef OLDASM}
      i386base,i386asm,
{$else}
      i386,
{$endif}
      cgai386,tgeni386,cg386ld;

{*****************************************************************************
                             SecondCallParaN
*****************************************************************************}

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right,inlined,dword_align : boolean;para_offset : longint);

      procedure maybe_push_high;
        begin
           { open array ? }
           { defcoll^.data can be nil for read/write }
           if assigned(defcoll^.data) and
              push_high_param(defcoll^.data) then
             begin
               if assigned(p^.hightree) then
                begin
                  secondpass(p^.hightree);
                  { this is a longint anyway ! }
                  push_value_para(p^.hightree,inlined,para_offset,4);
                end
               else
                internalerror(432645);
             end;
        end;

      var
         otlabel,oflabel : plabel;
         align : longint;
         { temporary variables: }
         tempdeftype : tdeftype;
         r : preference;
      begin
         { push from left to right if specified }
         if push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right,
             inlined,dword_align,para_offset);
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(p^.left);
         { filter array constructor with c styled args }
         if is_array_constructor(p^.left^.resulttype) and p^.left^.cargs then
           begin
             { nothing, everything is already pushed }
           end
         { in codegen.handleread.. defcoll^.data is set to nil }
         else if assigned(defcoll^.data) and
           (defcoll^.data^.deftype=formaldef) then
           begin
              { allow @var }
              inc(pushedparasize,4);
              if p^.left^.treetype=addrn then
                begin
                { always a register }
                  if inlined then
                    begin
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                         p^.left^.location.register,r)));
                    end
                  else
                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.left^.location.register)));
                  ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   if not(p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                     CGMessage(type_e_mismatch)
                   else
                     begin
                       if inlined then
                         begin
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                             newreference(p^.left^.location.reference),R_EDI)));
                           r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                           exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                         end
                      else
                        emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) then
           begin
              if (p^.left^.location.loc<>LOC_REFERENCE) then
                CGMessage(cg_e_var_must_be_reference);
              maybe_push_high;
              inc(pushedparasize,4);
              if inlined then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                     newreference(p^.left^.location.reference),R_EDI)));
                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                end
              else
                emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
              del_reference(p^.left^.location.reference);
           end
         else
           begin
              tempdeftype:=p^.resulttype^.deftype;
              if tempdeftype=filedef then
               CGMessage(cg_e_file_must_call_by_reference);
              if push_addr_param(p^.resulttype) then
                begin
                   maybe_push_high;
                   inc(pushedparasize,4);
                   if inlined then
                     begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                          newreference(p^.left^.location.reference),R_EDI)));
                        r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                        exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                          R_EDI,r)));
                     end
                   else
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                   del_reference(p^.left^.location.reference);
                end
              else
                begin
                   align:=target_os.stackalignment;
                   if dword_align then
                     align:=4;
                   push_value_para(p^.left,inlined,para_offset,align);
                end;
           end;
         freelabel(truelabel);
         freelabel(falselabel);
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right,
             inlined,dword_align,para_offset);
      end;


{*****************************************************************************
                             SecondCallN
*****************************************************************************}

    procedure secondcalln(var p : ptree);
      var
         unusedregisters : tregisterset;
         pushed,pushedregs : tpushed;
         hr,funcretref : treference;
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
         iolabel : plabel;
         { lexlevel count }
         i : longint;
         { help reference pointer }
         r : preference;
         hp,
         pp,params : ptree;
         inlined : boolean;
         inlinecode : ptree;
         para_offset : longint;
         { instruction for alignement correction }
{         corr : pai386;}
         { we must pop this size also after !! }
{         must_pop : boolean; }
         pop_size : longint;

      label
         dont_call;

      begin
         reset_reference(p^.location.reference);
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         loadesi:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;

         if not assigned(p^.procdefinition) then
          exit;
         if (p^.procdefinition^.options and poinline)<>0 then
           begin
              inlined:=true;
              inlinecode:=p^.right;
              { set it to the same lexical level as the local symtable, becuase
                the para's are stored there }
              p^.procdefinition^.parast^.symtablelevel:=aktprocsym^.definition^.localst^.symtablelevel;
              if assigned(p^.left) then
                inlinecode^.para_offset:=gettempofsizepersistant(inlinecode^.para_size);
              p^.procdefinition^.parast^.address_fixup:=inlinecode^.para_offset;
{$ifdef extdebug}
             Comment(V_debug,
               'inlined parasymtable is at offset '
               +tostr(p^.procdefinition^.parast^.address_fixup));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew('inlined parasymtable is at offset '
               +tostr(p^.procdefinition^.parast^.address_fixup)))));
{$endif extdebug}
              p^.right:=nil;
              { disable further inlining of the same proc
                in the args }
              p^.procdefinition^.options:=p^.procdefinition^.options and (not poinline);
           end;
         { only if no proc var }
         if not(assigned(p^.right)) then
           is_con_or_destructor:=((p^.procdefinition^.options and poconstructor)<>0)
             or ((p^.procdefinition^.options and podestructor)<>0);
         { proc variables destroy all registers }
         if (p^.right=nil) and
            { virtual methods too }
            ((p^.procdefinition^.options and povirtualmethod)=0) then
           begin
              if ((p^.procdefinition^.options and poiocheck)<>0) and
                 ((aktprocsym^.definition^.options and poiocheck)=0) and
                 (cs_check_io in aktlocalswitches) then
                begin
                   getlabel(iolabel);
                   emitlab(iolabel);
                end
              else
                iolabel:=nil;

              { save all used registers }
              pushusedregisters(pushed,p^.procdefinition^.usedregisters);

              { give used registers through }
              usedinproc:=usedinproc or p^.procdefinition^.usedregisters;
           end
         else
           begin
              pushusedregisters(pushed,$ff);
              usedinproc:=$ff;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         pop_size:=0;
         if (not inlined) then
          begin
          { Old pushedsize aligned on 4 ? }
            i:=oldpushedparasize and 3;
            if i>0 then
             inc(pop_size,4-i);
          { This parasize aligned on 4 ? }
            i:=p^.procdefinition^.para_size and 3;
            if i>0 then
             inc(pop_size,4-i);
          { insert the opcode and update pushedparasize }
            if pop_size>0 then
             begin
               inc(pushedparasize,pop_size);
               exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,pop_size,R_ESP)));
{$ifdef GDB}
               if (cs_debuginfo in aktmoduleswitches) and
                  (exprasmlist^.first=exprasmlist^.last) then
                 exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
             end;
          end;

         if (p^.resulttype<>pdef(voiddef)) and
            ret_in_param(p^.resulttype) then
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
                     funcretref.offset:=gettempofsizepersistant(p^.procdefinition^.retdef^.size);
                     funcretref.base:=procinfo.framepointer;
                  end
                else
                  gettempofsizereference(p^.procdefinition^.retdef^.size,funcretref);
           end;
         if assigned(p^.left) then
           begin
              { be found elsewhere }
              if inlined then
                para_offset:=p^.procdefinition^.parast^.address_fixup+
                  p^.procdefinition^.parast^.datasize
              else
                para_offset:=0;
              if assigned(p^.right) then
                secondcallparan(p^.left,pprocvardef(p^.right^.resulttype)^.para1,
                  (p^.procdefinition^.options and poleftright)<>0,
                  inlined,(p^.procdefinition^.options and (pocdecl or postdcall))<>0,para_offset)
              else
                secondcallparan(p^.left,p^.procdefinition^.para1,
                  (p^.procdefinition^.options and poleftright)<>0,
                  inlined,(p^.procdefinition^.options and (pocdecl or postdcall))<>0,para_offset);
           end;
         params:=p^.left;
         p^.left:=nil;
         if inlined then
           inlinecode^.retoffset:=gettempofsizepersistant(4);
         if ret_in_param(p^.resulttype) then
           begin
              inc(pushedparasize,4);
              if inlined then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                     newreference(funcretref),R_EDI)));
                   r:=new_reference(procinfo.framepointer,inlinecode^.retoffset);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                     R_EDI,r)));
                end
              else
                emitpushreferenceaddr(exprasmlist,funcretref);
           end;
         { procedure variable ? }
         if (p^.right=nil) then
           begin
              { overloaded operator have no symtable }
              { push self }
              if assigned(p^.symtable) and
                (p^.symtable^.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   p^.methodpointer:=genzeronode(callparan);
                   p^.methodpointer^.location.loc:=LOC_REGISTER;
                   p^.methodpointer^.location.register:=R_ESI;
                   { ARGHHH this is wrong !!!
                     if we can init from base class for a child
                     class that the wrong VMT will be
                     transfered to constructor !! }
                   p^.methodpointer^.resulttype:=
                     ptree(pwithsymtable(p^.symtable)^.withnode)^.left^.resulttype;
                   { change dispose type !! }
                   p^.disposetyp:=dt_mbleft_and_method;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   { if assigned(ptree(pwithsymtable(p^.symtable)^.withnode)^.pref) then
                     begin
                        r^:=ptree(pwithsymtable(p^.symtable)^.withnode)^.pref^;
                     end
                   else
                     begin
                        r^.offset:=p^.symtable^.datasize;
                        r^.base:=procinfo.framepointer;
                     end; }
                   r^:=ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^;
                   if (not pwithsymtable(p^.symtable)^.direct_with) or
                      pobjectdef(p^.methodpointer^.resulttype)^.isclass then
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)))
                   else
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,r,R_ESI)));
                end;

              { push self }
              if assigned(p^.symtable) and
                ((p^.symtable^.symtabletype=objectsymtable) or
                (p^.symtable^.symtabletype=withsymtable)) then
                begin
                   if assigned(p^.methodpointer) then
                     begin
                        {
                        if p^.methodpointer^.resulttype=classrefdef then
                          begin
                              two possibilities:
                               1. constructor
                               2. class method

                          end
                        else }
                          begin
                             case p^.methodpointer^.treetype of
                               typen:
                                 begin
                                    { direct call to inherited method }
                                    if (p^.procdefinition^.options and poabstractmethod)<>0 then
                                      begin
                                         CGMessage(cg_e_cant_call_abstract_method);
                                         goto dont_call;
                                      end;
                                    { generate no virtual call }
                                    no_virtual_call:=true;

                                    if (p^.symtableprocentry^.properties and sp_static)<>0 then
                                      begin
                                         { well lets put the VMT address directly into ESI }
                                         { it is kind of dirty but that is the simplest    }
                                         { way to accept virtual static functions (PM)     }
                                         loadesi:=true;
                                         { if no VMT just use $0 bug0214 PM }
                                         if (pobjectdef(p^.methodpointer^.resulttype)^.options and oo_hasvmt)=0 then
                                           exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_L,0,R_ESI)))
                                         else
                                           begin
                                             exprasmlist^.concat(new(pai386,op_sym_ofs_reg(A_MOV,S_L,
                                               newasmsymbol(pobjectdef(
                                               p^.methodpointer^.resulttype)^.vmt_mangledname),0,R_ESI)));
                                             maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                               pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                           end;
                                         { exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                           this is done below !! }
                                      end
                                    else
                                      { this is a member call, so ESI isn't modfied }
                                      loadesi:=false;

                                    { a class destructor needs a flag }
                                    if pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                        assigned(aktprocsym) and
                                        ((aktprocsym^.definition^.options and
                                        (podestructor))<>0) then
                                        begin
                                           push_int(0);
                                           exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                        end;

                                    if not(is_con_or_destructor and
                                      pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                        assigned(aktprocsym) and
                                        ((aktprocsym^.definition^.options and
                                        (poconstructor or podestructor))<>0)) then
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    { if an inherited con- or destructor should be  }
                                    { called in a con- or destructor then a warning }
                                    { will be made                                  }
                                    { con- and destructors need a pointer to the vmt }
                                    if is_con_or_destructor and
                                    not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) and
                                    assigned(aktprocsym) then
                                      begin
                                         if not ((aktprocsym^.definition^.options
                                           and (poconstructor or podestructor))<>0) then

                                          CGMessage(cg_w_member_cd_call_from_method);
                                      end;
                                    { class destructors get there flag below }
                                    if is_con_or_destructor and
                                        not(pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                        assigned(aktprocsym) and
                                        ((aktprocsym^.definition^.options and
                                        (podestructor))<>0)) then
                                      push_int(0);
                                 end;
                               hnewn:
                                 begin
                                    { extended syntax of new }
                                    { ESI must be zero }
                                    exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_ESI,R_ESI)));
                                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    { insert the vmt }
                                    exprasmlist^.concat(new(pai386,op_sym(A_PUSH,S_L,
                                      newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname))));
                                    maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                      pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(p^.methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE              }
                                    exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                      newreference(p^.methodpointer^.location.reference),R_ESI)));
                                    del_reference(p^.methodpointer^.location.reference);
                                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    exprasmlist^.concat(new(pai386,op_sym(A_PUSH,S_L,
                                    newasmsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname))));
                                    maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                      pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (p^.symtable^.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(p^.methodpointer);
                                         case p^.methodpointer^.location.loc of
                                            LOC_CREGISTER,
                                            LOC_REGISTER:
                                              begin
                                                 emit_reg_reg(A_MOV,S_L,p^.methodpointer^.location.register,R_ESI);
                                                 ungetregister32(p^.methodpointer^.location.register);
                                              end;
                                            else
                                              begin
                                                 if (p^.methodpointer^.resulttype^.deftype=classrefdef) or
                                                    ((p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)))
                                                 else
                                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)));
                                                 del_reference(p^.methodpointer^.location.reference);
                                              end;
                                         end;
                                      end;
                                    { when calling a class method, we have
                                      to load ESI with the VMT !
                                      But that's wrong, if we call a class method via self
                                    }
                                    if ((p^.procdefinition^.options and poclassmethod)<>0)
                                       and not(p^.methodpointer^.resulttype^.deftype=classrefdef) then
                                      begin
                                         { class method needs current VMT }
                                         new(r);
                                         reset_reference(r^);
                                         r^.base:=R_ESI;
                                         r^.offset:= p^.procdefinition^._class^.vmt_offset;
                                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                                      end;

                                    { direct call to destructor: don't remove data! }
                                    if ((p^.procdefinition^.options and podestructor)<>0) and
                                      (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                      (pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                      exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,1)));

                                    { direct call to class constructor, don't allocate memory }
                                    if ((p^.procdefinition^.options and poconstructor)<>0) and
                                      (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                      (pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                      exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,0)))
                                    else
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    if is_con_or_destructor then
                                      begin
                                         { classes don't get a VMT pointer pushed }
                                         if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                           begin
                                              if ((p^.procdefinition^.options and poconstructor)<>0) then
                                                begin
                                                   { it's no bad idea, to insert the VMT }
                                                   exprasmlist^.concat(new(pai386,op_sym(A_PUSH,S_L,newasmsymbol(
                                                     pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname))));
                                                   maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                                     pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
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
                        if ((p^.procdefinition^.options and poclassmethod)<>0) and
                          not(
                            assigned(aktprocsym) and
                            ((aktprocsym^.definition^.options and poclassmethod)<>0)
                          ) then
                          begin
                             { class method needs current VMT }
                             new(r);
                             reset_reference(r^);
                             r^.base:=R_ESI;
                             r^.offset:= p^.procdefinition^._class^.vmt_offset;
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                          end
                        else
                          begin
                             { member call, ESI isn't modified }
                             loadesi:=false;
                          end;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                        { but a con- or destructor here would probably almost }
                        { always be placed wrong }
                        if is_con_or_destructor then
                          begin
                             CGMessage(cg_w_member_cd_call_from_method);
                             push_int(0);
                          end;
                     end;
                end;

              { push base pointer ?}
              if (lexlevel>=normal_function_level) and assigned(pprocdef(p^.procdefinition)^.parast) and
                ((p^.procdefinition^.parast^.symtablelevel)>normal_function_level) then
                begin
                   { if we call a nested function in a method, we must      }
                   { push also SELF!                                        }
                   { THAT'S NOT TRUE, we have to load ESI via frame pointer }
                   { access                                                 }
                   {
                     begin
                        loadesi:=false;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                     end;
                   }
                   if lexlevel=(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,r)))
                     end
                     { this is only true if the difference is one !!
                       but it cannot be more !! }
                   else if (lexlevel=p^.procdefinition^.parast^.symtablelevel-1) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,procinfo.framepointer)))
                     end
                   else if (lexlevel>p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        hregister:=getregister32;
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,hregister)));
                        for i:=(p^.procdefinition^.parast^.symtablelevel) to lexlevel-1 do
                          begin
                             new(r);
                             reset_reference(r^);
                             {we should get the correct frame_pointer_offset at each level
                             how can we do this !!! }
                             r^.offset:=procinfo.framepointer_offset;
                             r^.base:=hregister;
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,hregister)));
                          end;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,hregister)));
                        ungetregister32(hregister);
                     end
                   else
                     internalerror(25000);
                end;

              if ((p^.procdefinition^.options and povirtualmethod)<>0) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in ESI }
                   { also class methods                              }
                   { Here it is quite tricky because it also depends }
                   { on the methodpointer                         PM }
                   if assigned(aktprocsym) then
                     begin
                       if ((((aktprocsym^.properties and sp_static)<>0) or
                        ((aktprocsym^.definition^.options and poclassmethod)<>0)) and
                        ((p^.methodpointer=nil) or (p^.methodpointer^.treetype=typen)))
                        or
                        ((p^.procdefinition^.options and postaticmethod)<>0) or
                        ((p^.procdefinition^.options and poconstructor)<>0) or
                        { ESI is loaded earlier }
                        ((p^.procdefinition^.options and poclassmethod)<>0)then
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
                            r^.offset:= p^.procdefinition^._class^.vmt_offset;
                            exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_EDI;
                         end;
                     end
                   else
                     { aktprocsym should be assigned, also in main program }
                     internalerror(12345);
                   {
                     begin
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_ESI;
                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_EDI;
                     end;
                   }
                   if p^.procdefinition^.extnumber=-1 then
                        internalerror($Da);
                   r^.offset:=p^.procdefinition^.extnumber*4+12;
{$ifndef TESTOBJEXT}
                   if (cs_check_range in aktlocalswitches) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,r^.base)));
                        emitcall('FPC_CHECK_OBJECT',true);
                     end;
{$else TESTOBJEXT}
                   if (cs_check_range in aktlocalswitches) then
                     begin
                        exprasmlist^.concat(new(pai386,op_sym(A_PUSH,S_L,
                          newasmsymbol(p^.procdefinition^._class^.vmt_mangledname))));
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,r^.base)));
                        emitcall('FPC_CHECK_OBJECT_EXT',true);
                     end;
{$endif TESTOBJEXT}
                   exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,r)));
                end
              else if not inlined then
                emitcall(p^.procdefinition^.mangledname,
                  (p^.symtableproc^.symtabletype=unitsymtable) or
                  ((p^.symtableproc^.symtabletype=objectsymtable) and
                  (pobjectdef(p^.symtableproc^.defowner)^.owner^.symtabletype=unitsymtable))or
                  ((p^.symtableproc^.symtabletype=withsymtable) and
                  (pobjectdef(p^.symtableproc^.defowner)^.owner^.symtabletype=unitsymtable)))
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   { set poinline again }
                   p^.procdefinition^.options:=p^.procdefinition^.options or poinline;
                   { process the inlinecode }
                   secondpass(inlinecode);
                   { free the args }
                   ungetpersistanttemp(p^.procdefinition^.parast^.address_fixup);
                end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(p^.right);
              { method pointer ? }
              if (p^.procdefinition^.options and pomethodpointer)<>0 then
                begin
                   { method pointer can't be in a register }
                   hregister:=R_NO;

                   { do some hacking if we call a method pointer }
                   { which is a class member                     }
                   { else ESI is overwritten !                   }
                   if (p^.right^.location.reference.base=R_ESI) or
                     (p^.right^.location.reference.index=R_ESI) then
                     begin
                        del_reference(p^.right^.location.reference);
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                          newreference(p^.right^.location.reference),R_EDI)));
                        hregister:=R_EDI;
                     end;

                   inc(p^.right^.location.reference.offset,4);

                   { load ESI }
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                     newreference(p^.right^.location.reference),R_ESI)));
                   { push self pointer }
                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                   dec(p^.right^.location.reference.offset,4);

                   if hregister=R_NO then
                     exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,newreference(p^.right^.location.reference))))
                   else
                     exprasmlist^.concat(new(pai386,op_reg(A_CALL,S_NO,hregister)));

                   del_reference(p^.right^.location.reference);
                end
              else
                begin
                   case p^.right^.location.loc of
                      LOC_REGISTER,LOC_CREGISTER:
                         begin
                             exprasmlist^.concat(new(pai386,op_reg(A_CALL,S_NO,p^.right^.location.register)));
                             ungetregister32(p^.right^.location.register);
                         end
                      else
                         exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,newreference(p^.right^.location.reference))));
                         del_reference(p^.right^.location.reference);
                   end;
                end;
           end;

           { this was only for normal functions
             displaced here so we also get
             it to work for procvars PM }
           if (not inlined) and ((p^.procdefinition^.options and poclearstack)<>0) then
             begin
                { consider the alignment with the rest (PM) }
                inc(pushedparasize,pop_size);
                pop_size:=0;
                { better than an add on all processors }
                if pushedparasize=4 then
                  exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)))
                { the pentium has two pipes and pop reg is pairable }
                { but the registers must be different!              }
                else if (pushedparasize=8) and
                  not(cs_littlesize in aktglobalswitches) and
                  (aktoptprocessor=ClassP5) and
                  (procinfo._class=nil) then
                    begin
                       exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));
                       exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
                    end
                else if pushedparasize<>0 then
                  exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pushedparasize,R_ESP)));
             end;
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;

         { handle function results }
         { structured results are easy to handle.... }
         { needed also when result_no_used !! }
         if (p^.resulttype<>pdef(voiddef)) and ret_in_param(p^.resulttype) then
           begin
              p^.location.loc:=LOC_MEM;
              p^.location.reference.symbol:=nil;
              p^.location.reference:=funcretref;
           end;
         { we have only to handle the result if it is used, but        }
         { ansi/widestrings must be registered, so we can dispose them }
         if (p^.resulttype<>pdef(voiddef)) and (p^.return_value_used or
           is_ansistring(p^.resulttype) or is_widestring(p^.resulttype)) then
           begin
              { a contructor could be a function with boolean result }
              if (p^.right=nil) and
                 ((p^.procdefinition^.options and poconstructor)<>0) and
                 { quick'n'dirty check if it is a class or an object }
                 (p^.resulttype^.deftype=orddef) then
                begin
                   p^.location.loc:=LOC_FLAGS;
                   p^.location.resflags:=F_NE;
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
                             p^.location.register:=hregister;
                          end;
                     end;
                end
               { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype) then
                begin
                   {p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                   already done above (PM) }
                end
              else
                begin
                   if (p^.resulttype^.deftype=orddef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        case porddef(p^.resulttype)^.typ of
                          s32bit,u32bit,bool32bit :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_L,R_EAX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getexplicitregister32(R_EAX);
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    p^.location.register:=hregister;
                                 end;
                            end;
                          uchar,u8bit,bool8bit,s8bit:
                            begin
{$ifdef test_dest_loc}
                                 if dest_loc_known and (dest_loc_tree=p) then
                                   mov_reg_to_dest(p,S_B,R_AL)
                                 else
{$endif test_dest_loc}
                                   begin
                                      hregister:=getexplicitregister32(R_EAX);
                                      emit_reg_reg(A_MOV,S_B,R_AL,reg32toreg8(hregister));
                                      p^.location.register:=reg32toreg8(hregister);
                                   end;
                              end;
                          s16bit,u16bit,bool16bit :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_W,R_AX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getexplicitregister32(R_EAX);
                                    emit_reg_reg(A_MOV,S_W,R_AX,reg32toreg16(hregister));
                                    p^.location.register:=reg32toreg16(hregister);
                                 end;
                            end;
                           s64bitint,u64bit:
                             begin
{$ifdef test_dest_loc}
{$error Don't know what to do here}
{$endif test_dest_loc}
                                hregister:=getexplicitregister32(R_EAX);
                                hregister2:=getexplicitregister32(R_EDX);
                                emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                emit_reg_reg(A_MOV,S_L,R_EDX,hregister2);
                                p^.location.registerlow:=hregister;
                                p^.location.registerhigh:=hregister2;
                             end;
                        else internalerror(7);
                     end

                end
              else if (p^.resulttype^.deftype=floatdef) then
                case pfloatdef(p^.resulttype)^.typ of
                  f32bit:
                    begin
                       p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                       if dest_loc_known and (dest_loc_tree=p) then
                         mov_reg_to_dest(p,S_L,R_EAX)
                       else
{$endif test_dest_loc}
                         begin
                            hregister:=getexplicitregister32(R_EAX);
                            emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                            p^.location.register:=hregister;
                         end;
                    end;
                  else
                    p^.location.loc:=LOC_FPU;
                end
              else if is_ansistring(p^.resulttype) or
                is_widestring(p^.resulttype) then
                begin
                   gettempansistringreference(hr);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EAX,
                     newreference(hr))));
                   p^.location.loc:=LOC_REFERENCE;
                   p^.location.reference:=hr;
                   { unnessary ansi/wide strings are imm. disposed }
                   if not(p^.return_value_used) then
                     begin
                        pushusedregisters(pushedregs,$ff);
                        emitpushreferenceaddr(exprasmlist,hr);
                        if is_ansistring(p^.resulttype) then
                          begin
                             exprasmlist^.concat(new(pai386,
                               op_sym(A_CALL,S_NO,newasmsymbol('FPC_ANSISTR_DECR_REF'))));
                             if not (cs_compilesystem in aktmoduleswitches) then
                               concat_external('FPC_ANSISTR_DECR_REF',EXT_NEAR);
                          end
                        else
                          begin
                             exprasmlist^.concat(new(pai386,
                               op_sym(A_CALL,S_NO,newasmsymbol('FPC_WIDESTR_DECR_REF'))));
                             if not (cs_compilesystem in aktmoduleswitches) then
                               concat_external('FPC_WIDESTR_DECR_REF',EXT_NEAR);
                          end;
                        ungetiftemp(hr);
                        popusedregisters(pushedregs);
                     end;
                end
              else
                begin
                   p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                   if dest_loc_known and (dest_loc_tree=p) then
                     mov_reg_to_dest(p,S_L,R_EAX)
                   else
{$endif test_dest_loc}
                    begin
                       hregister:=getexplicitregister32(R_EAX);
                       emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                       p^.location.register:=hregister;
                    end;
                end;
             end;
           end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              exprasmlist^.concat(new(pai386,op_sym(A_PUSH,S_L,newasmsymbol(lab2str(iolabel)))));
              emitcall('FPC_IOCHECK',true);
           end;
         if pop_size>0 then
           exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pop_size,R_ESP)));

         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loadesi then
           maybe_loadesi;
         pp:=params;
         while assigned(pp) do
           begin
              if assigned(pp^.left) then
                begin
                  if (pp^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) and
                     ungettempoftype(pp^.left^.resulttype) then
                    ungetiftemp(pp^.left^.location.reference);
                { process also all nodes of an array of const }
                  if pp^.left^.treetype=arrayconstructn then
                    begin
                      if assigned(pp^.left^.left) then
                       begin
                         hp:=pp^.left;
                         while assigned(hp) do
                          begin
                            if (hp^.left^.location.loc in [LOC_REFERENCE,LOC_MEM]) and
                               ungettempoftype(hp^.left^.resulttype) then
                              ungetiftemp(hp^.left^.location.reference);
                            hp:=hp^.right;
                          end;
                       end;
                    end;
                end;
              pp:=pp^.right;
           end;
         if inlined then
           ungetpersistanttemp(inlinecode^.retoffset);
         disposetree(params);


         { from now on the result can be freed normally }
         if inlined and ret_in_param(p^.resulttype) then
           persistanttemptonormal(funcretref.offset);

         { if return value is not used }
         if (not p^.return_value_used) and (p^.resulttype<>pdef(voiddef)) then
           begin
              if p^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                { release unused temp }
                ungetiftemp(p^.location.reference)
              else if p^.location.loc=LOC_FPU then
                { release FPU stack }
                exprasmlist^.concat(new(pai386,op_reg(A_FSTP,S_NO,R_ST0)));
           end;
      end;


{*****************************************************************************
                             SecondProcInlineN
*****************************************************************************}


    procedure secondprocinline(var p : ptree);
       var st : psymtable;
           oldprocsym : pprocsym;
           para_size : longint;
           oldprocinfo : tprocinfo;
           { just dummies for genentrycode }
           nostackframe,make_global : boolean;
           proc_names : tstringcontainer;
           inlineentrycode,inlineexitcode : paasmoutput;
           oldexitlabel,oldexit2label,oldquickexitlabel:Plabel;
       begin
          oldexitlabel:=aktexitlabel;
          oldexit2label:=aktexit2label;
          oldquickexitlabel:=quickexitlabel;
          getlabel(aktexitlabel);
          getlabel(aktexit2label);
          oldprocsym:=aktprocsym;
          oldprocinfo:=procinfo;
          { set the return value }
          aktprocsym:=p^.inlineprocsym;
          procinfo.retdef:=aktprocsym^.definition^.retdef;
          procinfo.retoffset:=p^.retoffset;
          { arg space has been filled by the parent secondcall }
          st:=aktprocsym^.definition^.localst;
          { set it to the same lexical level }
          st^.symtablelevel:=oldprocsym^.definition^.localst^.symtablelevel;
          if st^.datasize>0 then
            begin
              st^.address_fixup:=gettempofsizepersistant(st^.datasize);
{$ifdef extdebug}
              Comment(V_debug,'local symtable is at offset '+tostr(st^.address_fixup));
              exprasmlist^.concat(new(pai_asm_comment,init(strpnew(
                'local symtable is at offset '+tostr(st^.address_fixup)))));
{$endif extdebug}
            end;
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init('Start of inlined proc')));
{$endif extdebug}
          { takes care of local data initialization }
          inlineentrycode:=new(paasmoutput,init);
          inlineexitcode:=new(paasmoutput,init);
          proc_names.init;
          para_size:=p^.para_size;
          make_global:=false; { to avoid warning }
          genentrycode(inlineentrycode,proc_names,make_global,0,para_size,nostackframe,true);
          exprasmlist^.concatlist(inlineentrycode);
          secondpass(p^.inlinetree);
          genexitcode(inlineexitcode,0,false,true);
          exprasmlist^.concatlist(inlineexitcode);
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init('End of inlined proc')));
{$endif extdebug}
          {we can free the local data now, reset also the fixup address }
          if st^.datasize>0 then
            begin
              ungetpersistanttemp(st^.address_fixup);
              st^.address_fixup:=0;
            end;
          aktprocsym:=oldprocsym;
          freelabel(aktexitlabel);
          freelabel(aktexit2label);
          aktexitlabel:=oldexitlabel;
          aktexit2label:=oldexit2label;
          quickexitlabel:=oldquickexitlabel;
          procinfo:=oldprocinfo;
       end;



end.
{
  $Log$
  Revision 1.80  1999-05-17 23:51:37  peter
    * with temp vars now use a reference with a persistant temp instead
      of setting datasize

  Revision 1.79  1999/05/17 21:56:59  florian
    * new temporary ansistring handling

  Revision 1.78  1999/05/01 13:24:02  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.77  1999/04/29 22:12:21  pierre
   * fix for ID 388 removing real from stack was wrong

  Revision 1.76  1999/04/25 22:33:19  pierre
   * fix for TESTOBJEXT code

  Revision 1.75  1999/04/19 09:45:46  pierre
    +  cdecl or stdcall push all args with longint size
    *  tempansi stuff cleaned up

  Revision 1.74  1999/04/16 13:42:23  jonas
    * more regalloc fixes (still not complete)

  Revision 1.73  1999/04/16 10:26:56  pierre
   * no add $0,%esp for cdecl functions without parameters

  Revision 1.72  1999/04/09 08:41:48  peter
    * define to get ansistring returns in ref instead of reg

  Revision 1.71  1999/03/31 13:55:04  peter
    * assembler inlining working for ag386bin

  Revision 1.70  1999/03/24 23:16:46  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.69  1999/02/25 21:02:21  peter
    * ag386bin updates
    + coff writer

  Revision 1.68  1999/02/22 02:15:04  peter
    * updates for ag386bin

  Revision 1.67  1999/02/11 09:46:21  pierre
    * fix for normal method calls inside static methods :
      WARNING there were both parser and codegen errors !!
      added static_call boolean to calln tree

  Revision 1.66  1999/02/09 15:45:46  florian
    + complex results for assembler functions, fixes bug0155

  Revision 1.65  1999/02/08 11:29:04  pierre
   * fix for bug0214
     several problems where combined
     search_class_member did not set srsymtable
     => in do_member_read the call node got a wrong symtable
     in cg386cal the vmt was pushed twice without chacking if it exists
     now %esi is set to zero and pushed if not vmt
     (not very efficient but should work !)

  Revision 1.64  1999/02/04 10:49:39  florian
    + range checking for ansi- and widestrings
    * made it compilable with TP

  Revision 1.63  1999/02/03 10:18:14  pierre
   * conditionnal code for extended check of virtual methods

  Revision 1.62  1999/02/02 23:52:32  florian
    * problem with calls to method pointers in methods fixed
    - double ansistrings temp management removed

  Revision 1.61  1999/02/02 11:04:36  florian
    * class destructors fixed, class instances weren't disposed correctly

  Revision 1.60  1999/01/28 23:56:44  florian
    * the reference in the result location of a function call wasn't resetted =>
      problem with unallowed far pointer, is solved now

  Revision 1.59  1999/01/27 00:13:52  florian
    * "procedure of object"-stuff fixed

  Revision 1.58  1999/01/21 22:10:35  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.57  1999/01/21 16:40:51  pierre
   * fix for constructor inside with statements

  Revision 1.56  1998/12/30 13:41:05  peter
    * released valuepara

  Revision 1.55  1998/12/22 13:10:58  florian
    * memory leaks for ansistring type casts fixed

  Revision 1.54  1998/12/19 00:23:41  florian
    * ansistring memory leaks fixed

  Revision 1.53  1998/12/11 00:02:47  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.52  1998/12/10 14:39:29  florian
    * bug with p(const a : ansistring) fixed
    * duplicate constant ansistrings were handled wrong, fixed

  Revision 1.51  1998/12/10 09:47:15  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.50  1998/12/06 13:12:44  florian
    * better code generation for classes which are passed as parameters to
      subroutines

  Revision 1.49  1998/11/30 09:43:00  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.48  1998/11/27 14:50:30  peter
    + open strings, $P switch support

  Revision 1.47  1998/11/26 21:30:03  peter
    * fix for valuepara

  Revision 1.46  1998/11/26 14:39:10  peter
    * ansistring -> pchar fixed
    * ansistring constants fixed
    * ansistring constants are now written once

  Revision 1.45  1998/11/18 15:44:07  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.44  1998/11/16 15:35:36  peter
    * rename laod/copystring -> load/copyshortstring
    * fixed int-bool cnv bug
    + char-ansistring conversion

  Revision 1.43  1998/11/15 16:32:33  florian
    * some stuff of Pavel implement (win32 dll creation)
    * bug with ansistring function results fixed

  Revision 1.42  1998/11/13 15:40:13  pierre
    + added -Se in Makefile cvstest target
    + lexlevel cleanup
      normal_function_level main_program_level and unit_init_level defined
    * tins_cache grown to A_EMMS (gave range check error in asm readers)
      (test added in code !)
    * -Un option was wrong
    * _FAIL and _SELF only keyword inside
      constructors and methods respectively

  Revision 1.41  1998/11/12 11:19:40  pierre
   * fix for first line of function break

  Revision 1.40  1998/11/10 10:09:08  peter
    * va_list -> array of const

  Revision 1.39  1998/11/09 11:44:33  peter
    + va_list for printf support

  Revision 1.38  1998/10/21 15:12:49  pierre
    * bug fix for IOCHECK inside a procedure with iocheck modifier
    * removed the GPF for unexistant overloading
      (firstcall was called with procedinition=nil !)
    * changed typen to what Florian proposed
      gentypenode(p : pdef) sets the typenodetype field
      and resulttype is only set if inside bt_type block !

  Revision 1.37  1998/10/21 08:39:57  florian
    + ansistring operator +
    + $h and string[n] for n>255 added
    * small problem with TP fixed

  Revision 1.36  1998/10/20 08:06:39  pierre
    * several memory corruptions due to double freemem solved
      => never use p^.loc.location:=p^.left^.loc.location;
    + finally I added now by default
      that ra386dir translates global and unit symbols
    + added a first field in tsymtable and
      a nextsym field in tsym
      (this allows to obtain ordered type info for
      records and objects in gdb !)

  Revision 1.35  1998/10/16 08:51:45  peter
    + target_os.stackalignment
    + stack can be aligned at 2 or 4 byte boundaries

  Revision 1.34  1998/10/09 08:56:22  pierre
    * several memory leaks fixed

  Revision 1.33  1998/10/06 17:16:39  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.32  1998/10/01 09:22:52  peter
    * fixed value openarray
    * ungettemp of arrayconstruct

  Revision 1.31  1998/09/28 16:57:15  pierre
    * changed all length(p^.value_str^) into str_length(p)
      to get it work with and without ansistrings
    * changed sourcefiles field of tmodule to a pointer

  Revision 1.30  1998/09/26 15:03:02  florian
    * small problems with DOM and excpetions fixed (code generation
      of raise was wrong and self was sometimes destroyed :()

  Revision 1.29  1998/09/25 00:04:00  florian
    * problems when calling class methods fixed

  Revision 1.28  1998/09/24 14:27:37  peter
    * some better support for openarray

  Revision 1.27  1998/09/24 09:02:13  peter
    * rewritten isconvertable to use case
    * array of .. and single variable are compatible

  Revision 1.26  1998/09/21 08:45:06  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.25  1998/09/20 12:26:35  peter
    * merged fixes

  Revision 1.24  1998/09/17 09:42:10  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.23  1998/09/14 10:43:45  peter
    * all internal RTL functions start with FPC_

  Revision 1.22.2.1  1998/09/20 12:20:06  peter
    * Fixed stack not on 4 byte boundary when doing a call

  Revision 1.22  1998/09/04 08:41:37  peter
    * updated some error CGMessages

  Revision 1.21  1998/09/01 12:47:57  peter
    * use pdef^.size instead of orddef^.typ

  Revision 1.20  1998/08/31 12:22:15  peter
    * secondinline moved to cg386inl

  Revision 1.19  1998/08/31 08:52:03  peter
    * fixed error 10 with succ() and pref()

  Revision 1.18  1998/08/20 21:36:38  peter
    * fixed 'with object do' bug

  Revision 1.17  1998/08/19 16:07:36  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.16  1998/08/18 09:24:36  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.15  1998/08/13 11:00:09  peter
    * fixed procedure<>procedure construct

  Revision 1.14  1998/08/11 14:05:33  peter
    * fixed sizeof(array of char)

  Revision 1.13  1998/08/10 14:49:45  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.12  1998/07/30 13:30:31  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.11  1998/07/24 22:16:52  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.10  1998/07/18 22:54:23  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.9  1998/07/07 17:40:37  peter
    * packrecords 4 works
    * word aligning of parameters

  Revision 1.8  1998/07/06 15:51:15  michael
  Added length checking for string reading

  Revision 1.7  1998/07/06 14:19:51  michael
  + Added calls for reading/writing ansistrings

  Revision 1.6  1998/07/01 15:28:48  peter
    + better writeln/readln handling, now 100% like tp7

  Revision 1.5  1998/06/25 14:04:17  peter
    + internal inc/dec

  Revision 1.4  1998/06/25 08:48:06  florian
    * first version of rtti support

  Revision 1.3  1998/06/09 16:01:33  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.2  1998/06/08 13:13:29  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:10  peter
    * splitted cgi386

}

