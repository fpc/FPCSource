{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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
unit n386cal;

{$i fpcdefs.inc}

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


implementation

    uses
      globtype,systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defbase,
{$ifdef GDB}
  {$ifdef delphi}
      sysutils,
  {$else}
      strings,
  {$endif}
      gdb,
{$endif GDB}
      cginfo,cgbase,pass_2,
      cpubase,paramgr,
      aasmbase,aasmtai,aasmcpu,
      nmem,nld,ncnv,
      ncgutil,cga,cgobj,tgobj,regvars,rgobj,rgcpu,cg64f32,cgcpu,cpuinfo;

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
              push_value_para(hightree,inlined,false,para_offset,4,paralocdummy);
            end;
        end;

      var
         otlabel,oflabel : tasmlabel;
         { temporary variables: }
         tempdeftype : tdeftype;
         tmpreg : tregister;
         href   : treference;

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
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);
         { handle varargs first, because defcoll is not valid }
         if (nf_varargs_para in flags) then
           begin
             if paramanager.push_addr_param(left.resulttype.def,is_cdecl) then
               begin
                 inc(pushedparasize,4);
                 cg.a_paramaddr_ref(exprasmlist,left.location.reference,paralocdummy);
                 location_release(exprasmlist,left.location);
               end
             else
               push_value_para(left,inlined,is_cdecl,para_offset,para_alignment,paralocdummy);
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
              { allow passing of a constant to a const formaldef }
              if (defcoll.paratyp=vs_const) and
                 (left.location.loc=LOC_CONSTANT) then
                location_force_mem(exprasmlist,left.location);

              { allow @var }
              inc(pushedparasize,4);
              if (left.nodetype=addrn) and
                 (not(nf_procvarload in left.flags)) then
                begin
                  if inlined then
                    begin
                       reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                       cg.a_load_loc_ref(exprasmlist,left.location,href);
                    end
                  else
                    cg.a_param_loc(exprasmlist,left.location,paralocdummy);
                  location_release(exprasmlist,left.location);
                end
              else
                begin
                   if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                     CGMessage(type_e_mismatch)
                   else
                     begin
                       if inlined then
                         begin
                           tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                           cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                           reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                           cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                           cg.free_scratch_reg(exprasmlist,tmpreg);
                         end
                       else
                         cg.a_paramaddr_ref(exprasmlist,left.location.reference,paralocdummy);
                       location_release(exprasmlist,left.location);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll.paratyp in [vs_var,vs_out]) then
           begin
              if (left.location.loc<>LOC_REFERENCE) then
               begin
                 { passing self to a var parameter is allowed in
                   TP and delphi }
                 if not((left.location.loc=LOC_CREFERENCE) and
                        (left.nodetype=selfn)) then
                  internalerror(200106041);
               end;
              maybe_push_high;
              if (defcoll.paratyp=vs_out) and
                 assigned(defcoll.paratype.def) and
                 not is_class(defcoll.paratype.def) and
                 defcoll.paratype.def.needs_inittable then
                cg.g_finalize(exprasmlist,defcoll.paratype.def,left.location.reference,false);
              inc(pushedparasize,4);
              if inlined then
                begin
                   tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                   cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                   reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                   cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                   cg.free_scratch_reg(exprasmlist,tmpreg);
                end
              else
                cg.a_paramaddr_ref(exprasmlist,left.location.reference,paralocdummy);
              location_release(exprasmlist,left.location);
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
                  paramanager.push_addr_param(resulttype.def,is_cdecl)
                 ) then
                begin
                   if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                    begin
                      { allow passing nil to a procvardef (methodpointer) }
                      if (left.nodetype=typeconvn) and
                         (left.resulttype.def.deftype=procvardef) and
                         (ttypeconvnode(left).left.nodetype=niln) then
                       begin
                         tg.GetTemp(exprasmlist,tcgsize2size[left.location.size],tt_normal,href);
                         cg.a_load_loc_ref(exprasmlist,left.location,href);
                         location_reset(left.location,LOC_REFERENCE,left.location.size);
                         left.location.reference:=href;
                       end
                      else
                       internalerror(200204011);
                    end;

                   maybe_push_high;
                   inc(pushedparasize,4);
                   if inlined then
                     begin
                        tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                        cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                        reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                        cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                        cg.free_scratch_reg(exprasmlist,tmpreg);
                     end
                   else
                     cg.a_paramaddr_ref(exprasmlist,left.location.reference,paralocdummy);
                   location_release(exprasmlist,left.location);
                end
              else
                begin
                   push_value_para(left,inlined,is_cdecl,
                     para_offset,para_alignment,paralocdummy);
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
         regs_to_push : tregisterset;
         unusedstate: pointer;
         pushed : tpushedsaved;
         funcretref,refcountedtemp : treference;
         tmpreg : tregister;
         hregister : tregister;
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
         href : treference;
         hrefvmt : treference;
         hp : tnode;
         pp : tbinarynode;
         params : tnode;
         inlined : boolean;
         inlinecode : tprocinlinenode;
         store_parast_fixup,
         para_alignment,
         para_offset : longint;
         cgsize : tcgsize;
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
         release_tmpreg : boolean;
         constructorfailed : tasmlabel;
         returnref,
         pararef : treference;

      label
         dont_call;

      begin
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         loadesi:=true;
         no_virtual_call:=false;
         rg.saveunusedstate(unusedstate);

         { if we allocate the temp. location for ansi- or widestrings }
         { already here, we avoid later a push/pop                    }
         if is_widestring(resulttype.def) then
           begin
             tg.GetTemp(exprasmlist,pointer_size,tt_widestring,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp);
           end
         else if is_ansistring(resulttype.def) then
           begin
             tg.GetTemp(exprasmlist,pointer_size,tt_ansistring,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp);
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
               begin
                 tg.GetTemp(exprasmlist,inlinecode.para_size,tt_persistant,pararef);
                 inlinecode.para_offset:=pararef.offset;
               end;
              store_parast_fixup:=tprocdef(procdefinition).parast.address_fixup;
              tprocdef(procdefinition).parast.address_fixup:=inlinecode.para_offset;
{$ifdef extdebug}
             Comment(V_debug,
               'inlined parasymtable is at offset '
               +tostr(tprocdef(procdefinition).parast.address_fixup));
             exprasmList.concat(tai_comment.Create(
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
                   objectlibrary.getaddrlabel(iolabel);
                   cg.a_label(exprasmlist,iolabel);
                end
              else
                iolabel:=nil;

              { save all used registers }
              regs_to_push := tprocdef(procdefinition).usedregisters;
              rg.saveusedregisters(exprasmlist,pushed,regs_to_push);

              { give used registers through }
              rg.usedinproc:=rg.usedinproc + tprocdef(procdefinition).usedregisters;
           end
         else
           begin
              regs_to_push := all_registers;
              rg.saveusedregisters(exprasmlist,pushed,regs_to_push);
              rg.usedinproc:=all_registers;
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

         { Push parameters }
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

         { Allocate return value for inlined routines }
         if inlined and
            (resulttype.def.size>0) then
          begin
            tg.GetTemp(exprasmlist,Align(resulttype.def.size,aktalignment.paraalign),tt_persistant,returnref);
            inlinecode.retoffset:=returnref.offset;
          end;

         { Allocate return value when returned in argument }
         if paramanager.ret_in_param(resulttype.def) then
           begin
             if assigned(funcretrefnode) then
              begin
                secondpass(funcretrefnode);
                if codegenerror then
                 exit;
                if (funcretrefnode.location.loc<>LOC_REFERENCE) then
                 internalerror(200204246);
                funcretref:=funcretrefnode.location.reference;
              end
             else
              begin
                if inlined then
                 begin
                   tg.GetTemp(exprasmlist,resulttype.def.size,tt_persistant,funcretref);
{$ifdef extdebug}
                   Comment(V_debug,'function return value is at offset '
                                   +tostr(funcretref.offset));
                   exprasmlist.concat(tai_comment.create(
                                       strpnew('function return value is at offset '
                                               +tostr(funcretref.offset))));
{$endif extdebug}
                 end
                else
                 tg.GetTemp(exprasmlist,resulttype.def.size,tt_normal,funcretref);
              end;

             { This must not be counted for C code
               complex return address is removed from stack
               by function itself !   }
{$ifdef OLD_C_STACK}
             inc(pushedparasize,4); { lets try without it PM }
{$endif not OLD_C_STACK}
             if inlined then
               begin
                  hregister:=cg.get_scratch_reg_address(exprasmlist);
                  cg.a_loadaddr_ref_reg(exprasmlist,funcretref,hregister);
                  reference_reset_base(href,procinfo.framepointer,inlinecode.retoffset);
                  cg.a_load_reg_ref(exprasmlist,OS_ADDR,hregister,href);
                  cg.free_scratch_reg(exprasmlist,hregister);
               end
             else
               cg.a_paramaddr_ref(exprasmlist,funcretref,paralocdummy);
           end;

         { procedure variable or normal function call ? }
         if inlined or
            (right=nil) then
           begin
              { Normal function call }

              { overloaded operator has no symtable }
              { push self }
              if assigned(symtableproc) and
                (symtableproc.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   methodpointer:=ccallparanode.create(nil,nil);
                   location_reset(methodpointer.location,LOC_REGISTER,OS_ADDR);
                   rg.getexplicitregisterint(exprasmlist,R_ESI);
                   methodpointer.location.register:=R_ESI;
                   { ARGHHH this is wrong !!!
                     if we can init from base class for a child
                     class that the wrong VMT will be
                     transfered to constructor !! }
                   methodpointer.resulttype:=
                     twithnode(twithsymtable(symtableproc).withnode).left.resulttype;
                   { make a reference }
                   href:=twithnode(twithsymtable(symtableproc).withnode).withreference;
                   if ((not(nf_islocal in twithnode(twithsymtable(symtableproc).withnode).flags)) and
                       (not twithsymtable(symtableproc).direct_with)) or
                      is_class_or_interface(methodpointer.resulttype.def) then
                     cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,self_pointer_reg)
                   else
                     cg.a_loadaddr_ref_reg(exprasmlist,href,self_pointer_reg);
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
                                         rg.getexplicitregisterint(exprasmlist,R_ESI);
                                         if not(oo_has_vmt in tobjectdef(methodpointer.resulttype.def).objectoptions) then
                                           cg.a_load_const_reg(exprasmlist,OS_ADDR,0,self_pointer_reg)
                                         else
                                           begin
                                             reference_reset_symbol(href,objectlibrary.newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                                             cg.a_loadaddr_ref_reg(exprasmlist,href,self_pointer_reg);
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
                                        cg.a_param_const(exprasmlist,OS_ADDR,0,paramanager.getintparaloc(2));
                                        cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(1));
                                      end;

                                    if not(is_con_or_destructor and
                                           is_class(methodpointer.resulttype.def) and
                                           (procdefinition.proctypeoption in [potype_constructor,potype_destructor])
                                          ) then
                                      cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(1));
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
                                           cg.a_param_const(exprasmlist,OS_ADDR,0,paramanager.getintparaloc(2));
                                         cg.a_param_const(exprasmlist,OS_ADDR,0,paramanager.getintparaloc(1));
                                      end;
                                 end;
                               hnewn:
                                 begin
                                    { extended syntax of new }
                                    { ESI must be zero }
                                    rg.getexplicitregisterint(exprasmlist,R_ESI);
                                    cg.a_load_const_reg(exprasmlist,OS_ADDR,0,self_pointer_reg);
                                    cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(2));
                                    { insert the vmt }
                                    reference_reset_symbol(href,objectlibrary.newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                                    cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE          }
                                    rg.getexplicitregisterint(exprasmlist,R_ESI);
                                    emit_ref_reg(A_LEA,S_L,methodpointer.location.reference,R_ESI);
                                    reference_release(exprasmlist,methodpointer.location.reference);
                                    cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(2));
                                    reference_reset_symbol(href,objectlibrary.newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                                    cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (symtableproc.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(methodpointer);
                                         rg.getexplicitregisterint(exprasmlist,R_ESI);
                                         case methodpointer.location.loc of
                                            LOC_CREGISTER,
                                            LOC_REGISTER:
                                              begin
                                                 cg.a_load_reg_reg(exprasmlist,OS_ADDR,methodpointer.location.register,R_ESI);
                                                 rg.ungetregisterint(exprasmlist,methodpointer.location.register);
                                              end;
                                            else
                                              begin
                                                 if (methodpointer.resulttype.def.deftype=classrefdef) or
                                                    is_class_or_interface(methodpointer.resulttype.def) then
                                                   cg.a_load_ref_reg(exprasmlist,OS_ADDR,methodpointer.location.reference,R_ESI)
                                                 else
                                                   cg.a_loadaddr_ref_reg(exprasmlist,methodpointer.location.reference,R_ESI);
                                                 reference_release(exprasmlist,methodpointer.location.reference);
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
                                             rg.getexplicitregisterint(exprasmlist,R_ESI);
                                             reference_reset_base(href,R_ESI,tprocdef(procdefinition)._class.vmt_offset);
                                             cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,self_pointer_reg);
                                          end;

                                        { direct call to destructor: remove data }
                                        if (procdefinition.proctypeoption=potype_destructor) and
                                           is_class(methodpointer.resulttype.def) then
                                          cg.a_param_const(exprasmlist,OS_INT,1,paramanager.getintparaloc(1));

                                        { direct call to class constructor, don't allocate memory }
                                        if (procdefinition.proctypeoption=potype_constructor) and
                                           is_class(methodpointer.resulttype.def) then
                                          begin
                                             cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(2));
                                             cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(1));
                                          end
                                        else
                                          begin
                                             { constructor call via classreference => allocate memory }
                                             if (procdefinition.proctypeoption=potype_constructor) and
                                                (methodpointer.resulttype.def.deftype=classrefdef) and
                                                is_class(tclassrefdef(methodpointer.resulttype.def).pointertype.def) then
                                               cg.a_param_const(exprasmlist,OS_INT,1,paramanager.getintparaloc(1));
                                             cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(1));
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
                                                  reference_reset_symbol(href,objectlibrary.newasmsymbol(
                                                     tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                                                  cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
                                                end
                                              { destructors haven't to dispose the instance, if this is }
                                              { a direct call                                           }
                                              else
                                                cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(1));
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
                             rg.getexplicitregisterint(exprasmlist,R_ESI);
                             reference_reset_base(href,R_ESI,tprocdef(procdefinition)._class.vmt_offset);
                             cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,R_ESI);
                          end
                        else
                          begin
                             { member call, ESI isn't modified }
                             loadesi:=false;
                          end;
                        { direct call to destructor: don't remove data! }
                        if is_class(procinfo._class) then
                          begin
                             if (procdefinition.proctypeoption=potype_destructor) then
                               begin
                                  cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(2));
                                  cg.a_param_reg(exprasmlist,OS_ADDR,R_ESI,paramanager.getintparaloc(1));
                               end
                             else if (procdefinition.proctypeoption=potype_constructor) then
                               begin
                                  cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(2));
                                  cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(1));
                               end
                             else
                               cg.a_param_reg(exprasmlist,OS_ADDR,R_ESI,paramanager.getintparaloc(1));
                          end
                        else if is_object(procinfo._class) then
                          begin
                             cg.a_param_reg(exprasmlist,OS_ADDR,R_ESI,paramanager.getintparaloc(1));
                             if is_con_or_destructor then
                               begin
                                  if (procdefinition.proctypeoption=potype_constructor) then
                                    begin
                                      { it's no bad idea, to insert the VMT }
                                      reference_reset_symbol(href,objectlibrary.newasmsymbol(procinfo._class.vmt_mangledname),0);
                                      cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
                                    end
                                  { destructors haven't to dispose the instance, if this is }
                                  { a direct call                                           }
                                  else
                                    cg.a_param_const(exprasmlist,OS_INT,0,paramanager.getintparaloc(1));
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
                     cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(1));
                     reference_reset_base(href,self_pointer_reg,0);
                     tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                     cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,tmpreg);
                     reference_reset_base(href,tmpreg,72);
                     cg.a_call_ref(exprasmlist,href);
                     cg.free_scratch_reg(exprasmlist,tmpreg);
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
                          reference_reset_base(href,procinfo.framepointer,procinfo.framepointer_offset);
                          cg.a_param_ref(exprasmlist,OS_ADDR,href,paralocdummy);
                       end
                       { this is only true if the difference is one !!
                         but it cannot be more !! }
                     else if (lexlevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
                       begin
                          cg.a_param_reg(exprasmlist,OS_ADDR,procinfo.framepointer,paralocdummy);
                       end
                     else if (lexlevel>(tprocdef(procdefinition).parast.symtablelevel)) then
                       begin
                          hregister:=rg.getregisterint(exprasmlist);
                          reference_reset_base(href,procinfo.framepointer,procinfo.framepointer_offset);
                          cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                          for i:=(tprocdef(procdefinition).parast.symtablelevel) to lexlevel-1 do
                            begin
                               {we should get the correct frame_pointer_offset at each level
                               how can we do this !!! }
                               reference_reset_base(href,hregister,procinfo.framepointer_offset);
                               cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                            end;
                          cg.a_param_reg(exprasmlist,OS_ADDR,hregister,paralocdummy);
                          rg.ungetregisterint(exprasmlist,hregister);
                       end
                     else
                       internalerror(25000);
                  end;

              rg.saveregvars(exprasmlist,regs_to_push);

              if (po_virtualmethod in procdefinition.procoptions) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in ESI }
                   { also class methods                       }
                   { Here it is quite tricky because it also depends }
                   { on the methodpointer                        PM }
                   release_tmpreg:=false;
                   rg.getexplicitregisterint(exprasmlist,R_ESI);
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
                            reference_reset_base(href,R_ESI,0);
                         end
                       else
                         begin
                            { this is one point where we need vmt_offset (PM) }
                            reference_reset_base(href,R_ESI,tprocdef(procdefinition)._class.vmt_offset);
                            tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,tmpreg);
                            reference_reset_base(href,tmpreg,0);
                            release_tmpreg:=true;
                         end;
                     end
                   else
                     { aktprocdef should be assigned, also in main program }
                     internalerror(12345);

                   if tprocdef(procdefinition).extnumber=-1 then
                     internalerror(44584);

                   href.offset:=tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber);
                   if not(is_interface(tprocdef(procdefinition)._class)) and
                      not(is_cppclass(tprocdef(procdefinition)._class)) then
                     begin
                        if (cs_check_object in aktlocalswitches) then
                          begin
                             reference_reset_symbol(hrefvmt,objectlibrary.newasmsymbol(tprocdef(procdefinition)._class.vmt_mangledname),0);
                             cg.a_paramaddr_ref(exprasmlist,hrefvmt,paramanager.getintparaloc(2));
                             cg.a_param_reg(exprasmlist,OS_ADDR,href.base,paramanager.getintparaloc(1));
                             cg.a_call_name(exprasmlist,'FPC_CHECK_OBJECT_EXT');
                          end
                        else if (cs_check_range in aktlocalswitches) then
                          begin
                             cg.a_param_reg(exprasmlist,OS_ADDR,href.base,paramanager.getintparaloc(1));
                             cg.a_call_name(exprasmlist,'FPC_CHECK_OBJECT');
                          end;
                     end;
                   cg.a_call_ref(exprasmlist,href);
                   if release_tmpreg then
                     cg.free_scratch_reg(exprasmlist,tmpreg);
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
                  cg.a_call_name(exprasmlist,tprocdef(procdefinition).mangledname);
                end
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   { process the inlinecode }
                   secondpass(inlinecode);
                   { free the args }
                   if tprocdef(procdefinition).parast.datasize>0 then
                     tg.UnGetTemp(exprasmlist,pararef);
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
                        reference_release(exprasmlist,right.location.reference);
                        hregister:=cg.get_scratch_reg_address(exprasmlist);
                        cg.a_load_ref_reg(exprasmlist,OS_ADDR,right.location.reference,hregister);
                     end;

                   { load self, but not if it's already explicitly pushed }
                   if not(po_containsself in procdefinition.procoptions) then
                     begin
                       { load ESI }
                       href:=right.location.reference;
                       inc(href.offset,4);
                       rg.getexplicitregisterint(exprasmlist,R_ESI);
                       cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,self_pointer_reg);
                       { push self pointer }
                       cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paralocdummy);
                     end;

                   rg.saveregvars(exprasmlist,ALL_REGISTERS);
                   if hregister<>R_NO then
                     reference_reset_base(href,hregister,0)
                   else
                     href:=right.location.reference;
                   cg.a_call_ref(exprasmlist,href);

                   if hregister<>R_NO then
                     cg.free_scratch_reg(exprasmlist,hregister);
                   reference_release(exprasmlist,right.location.reference);
                   tg.Ungetiftemp(exprasmlist,right.location.reference);
                end
              else
                begin
                   rg.saveregvars(exprasmlist,ALL_REGISTERS);
                   cg.a_call_loc(exprasmlist,right.location);
                   location_release(exprasmlist,right.location);
                   location_freetemp(exprasmlist,right.location);
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
                    rg.getexplicitregisterint(exprasmlist,R_EDI);
                    emit_reg(A_POP,S_L,R_EDI);
                    rg.ungetregisterint(exprasmlist,R_EDI);
                  end
                { the pentium has two pipes and pop reg is pairable }
                { but the registers must be different!        }
                else if (pushedparasize=8) and
                  not(cs_littlesize in aktglobalswitches) and
                  (aktoptprocessor=ClassP5) and
                  (procinfo._class=nil) then
                    begin
                       rg.getexplicitregisterint(exprasmlist,R_EDI);
                       emit_reg(A_POP,S_L,R_EDI);
                       rg.ungetregisterint(exprasmlist,R_EDI);
                       exprasmList.concat(tai_regalloc.Alloc(R_ESI));
                       emit_reg(A_POP,S_L,R_ESI);
                       exprasmList.concat(tai_regalloc.DeAlloc(R_ESI));
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
         rg.restoreunusedstate(unusedstate);
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
{$ifdef TEST_GENERIC}
{ should be moved to generic version! }
             reference_reset_base(href, procinfo.framepointer,procinfo.selfpointer_offset);
             cg.a_load_ref_reg(exprasmlist, OS_ADDR, href, SELF_POINTER_REG);
{$endif}
           end;

         { call to AfterConstruction? }
         if is_class(resulttype.def) and
           (inlined or
           (right=nil)) and
           (procdefinition.proctypeoption=potype_constructor) and
           assigned(methodpointer) and
           (methodpointer.nodetype<>typen) then
           begin
              objectlibrary.getlabel(constructorfailed);
              emitjmp(C_Z,constructorfailed);
              cg.a_param_reg(exprasmlist,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(1));
              reference_reset_base(href,self_pointer_reg,0);
              tmpreg:=cg.get_scratch_reg_address(exprasmlist);
              cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,tmpreg);
              reference_reset_base(href,tmpreg,68);
              cg.a_call_ref(exprasmlist,href);
              cg.free_scratch_reg(exprasmlist,tmpreg);
              exprasmList.concat(tai_regalloc.Alloc(accumulator));
              cg.a_label(exprasmlist,constructorfailed);
              cg.a_load_reg_reg(exprasmlist,OS_ADDR,self_pointer_reg,accumulator);
           end;

         { handle function results }
         if (not is_void(resulttype.def)) then
          begin
            { structured results are easy to handle.... }
            { needed also when result_no_used !! }
            if paramanager.ret_in_param(resulttype.def) then
             begin
               location_reset(location,LOC_CREFERENCE,def_cgsize(resulttype.def));
               location.reference.symbol:=nil;
               location.reference:=funcretref;
             end
            else
            { ansi/widestrings must be registered, so we can dispose them }
             if is_ansistring(resulttype.def) or
                is_widestring(resulttype.def) then
              begin
                location_reset(location,LOC_CREFERENCE,OS_ADDR);
                location.reference:=refcountedtemp;
                cg.a_reg_alloc(exprasmlist,accumulator);
                cg.a_load_reg_ref(exprasmlist,OS_ADDR,accumulator,location.reference);
                cg.a_reg_dealloc(exprasmlist,accumulator);
              end
            else
            { we have only to handle the result if it is used }
             if (nf_return_value_used in flags) then
              begin
                case resulttype.def.deftype of
                  enumdef,
                  orddef :
                    begin
                      cgsize:=def_cgsize(resulttype.def);
                      { an object constructor is a function with boolean result }
                      if (inlined or (right=nil)) and
                         (procdefinition.proctypeoption=potype_constructor) then
                       begin
                         if extended_new then
                          cgsize:=OS_INT
                         else
                          begin
                            cgsize:=OS_NO;
                            { this fails if popsize > 0 PM }
                            location_reset(location,LOC_FLAGS,OS_NO);
                            location.resflags:=F_NE;
                          end;
                       end;

                      if cgsize<>OS_NO then
                       begin
                         location_reset(location,LOC_REGISTER,cgsize);
                         cg.a_reg_alloc(exprasmlist,accumulator);
                         if cgsize in [OS_64,OS_S64] then
                          begin
                            cg.a_reg_alloc(exprasmlist,accumulatorhigh);
                            if accumulatorhigh in rg.unusedregsint then
                              begin
                                 location.registerhigh:=rg.getexplicitregisterint(exprasmlist,accumulatorhigh);
                                 location.registerlow:=rg.getexplicitregisterint(exprasmlist,accumulator);
                              end
                            else
                              begin
                                 location.registerhigh:=rg.getexplicitregisterint(exprasmlist,accumulatorhigh);
                                 location.registerlow:=rg.getexplicitregisterint(exprasmlist,accumulator);
                              end;
                            cg64.a_load64_reg_reg(exprasmlist,joinreg64(accumulator,accumulatorhigh),
                                location.register64);
                          end
                         else
                          begin
                            location.register:=rg.getexplicitregisterint(exprasmlist,accumulator);
                            hregister:=rg.makeregsize(accumulator,cgsize);
                            location.register:=rg.makeregsize(location.register,cgsize);
                            cg.a_load_reg_reg(exprasmlist,cgsize,hregister,location.register);
                          end;
                       end;
                    end;
                  floatdef :
                    begin
                      location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                      location.register:=R_ST;
                      inc(trgcpu(rg).fpuvaroffset);
                    end;
                  else
                    begin
                      location_reset(location,LOC_REGISTER,OS_INT);
                      location.register:=rg.getexplicitregisterint(exprasmlist,accumulator);
                      cg.a_load_reg_reg(exprasmlist,OS_INT,accumulator,location.register);
                    end;
                end;
             end;
          end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              reference_reset_symbol(href,iolabel,0);
              cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
           end;
         if pop_size>0 then
           emit_const_reg(A_ADD,S_L,pop_size,R_ESP);

         { restore registers }
         rg.restoreusedregisters(exprasmlist,pushed);

         { at last, restore instance pointer (SELF) }
         if loadesi then
           cg.g_maybe_loadself(exprasmlist);
         pp:=tbinarynode(params);
         while assigned(pp) do
           begin
              if assigned(pp.left) then
                begin
                  location_freetemp(exprasmlist,pp.left.location);
                  { process also all nodes of an array of const }
                  if pp.left.nodetype=arrayconstructorn then
                    begin
                      if assigned(tarrayconstructornode(pp.left).left) then
                       begin
                         hp:=pp.left;
                         while assigned(hp) do
                          begin
                            location_freetemp(exprasmlist,tarrayconstructornode(hp).left.location);
                            hp:=tarrayconstructornode(hp).right;
                          end;
                       end;
                    end;
                end;
              pp:=tbinarynode(pp.right);
           end;
         if inlined then
           begin
             tg.UnGetTemp(exprasmlist,returnref);
             tprocdef(procdefinition).parast.address_fixup:=store_parast_fixup;
             right:=inlinecode;
           end;
         if assigned(params) then
           params.free;

         { from now on the result can be freed normally }
         if inlined and paramanager.ret_in_param(resulttype.def) then
           tg.ChangeTempType(funcretref,tt_normal);

         { if return value is not used }
         if (not(nf_return_value_used in flags)) and (not is_void(resulttype.def)) then
           begin
              if location.loc in [LOC_CREFERENCE,LOC_REFERENCE] then
                begin
                   { data which must be finalized ? }
                   if (resulttype.def.needs_inittable) then
                      cg.g_finalize(exprasmlist,resulttype.def,location.reference,false);
                   { release unused temp }
                   tg.ungetiftemp(exprasmlist,location.reference)
                end
              else if location.loc=LOC_FPUREGISTER then
                begin
                  { release FPU stack }
                  emit_reg(A_FSTP,S_NO,R_ST);
                  {
                    dec(trgcpu(rg).fpuvaroffset);
                    do NOT decrement as the increment before
                    is not called for unused results PM }
                end;
           end;
      end;

begin
   ccallparanode:=ti386callparanode;
   ccallnode:=ti386callnode;
end.
{
  $Log$
  Revision 1.68  2002-09-01 12:13:00  peter
    * use a_call_reg
    * ungetiftemp for procvar of object temp

  Revision 1.67  2002/08/25 19:25:21  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.66  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.65  2002/08/18 20:06:30  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.64  2002/08/17 09:23:45  florian
    * first part of procinfo rewrite

  Revision 1.63  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.62  2002/08/11 14:32:30  peter
    * renamed current_library to objectlibrary

  Revision 1.61  2002/08/11 13:24:16  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.60  2002/07/20 11:58:01  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.59  2002/07/11 14:41:33  florian
    * start of the new generic parameter handling

  Revision 1.58  2002/07/07 09:52:34  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.57  2002/07/06 20:27:26  carl
  + generic set handling

  Revision 1.56  2002/07/01 18:46:31  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.55  2002/07/01 16:23:56  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.54  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.53  2002/05/18 13:34:23  peter
    * readded missing revisions

  Revision 1.52  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.50  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.49  2002/05/12 16:53:17  peter
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

  Revision 1.48  2002/04/25 20:16:40  peter
    * moved more routines from cga/n386util

  Revision 1.47  2002/04/21 19:02:07  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.46  2002/04/21 15:34:25  carl
  * changeregsize -> rg.makeregsize

  Revision 1.45  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.44  2002/04/04 19:06:10  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.43  2002/04/02 17:11:35  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.42  2002/03/31 20:26:38  jonas
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

  Revision 1.41  2002/03/04 19:10:13  peter
    * removed compiler warnings

}
