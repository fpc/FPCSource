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
      globtype,
      symdef,
      node,ncal,ncgcal;

    type
       ti386callparanode = class(tcallparanode)
          procedure secondcallparan(push_from_left_to_right:boolean;calloption:tproccalloption;
                para_alignment,para_offset : longint);override;
       end;

       ti386callnode = class(tcgcallnode)
          procedure pass_2;override;
       end;


implementation

    uses
      systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defutil,
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
      nbas,nmem,nld,ncnv,
      ncgutil,cga,cgobj,tgobj,regvars,rgobj,rgcpu,cg64f32,cgcpu,cpuinfo;

{*****************************************************************************
                             TI386CALLPARANODE
*****************************************************************************}

    procedure ti386callparanode.secondcallparan(push_from_left_to_right:boolean;calloption:tproccalloption;para_alignment,para_offset : longint);

      procedure maybe_push_high;
        begin
           { open array ? }
           { paraitem.data can be nil for read/write }
           if assigned(paraitem.paratype.def) and
              assigned(hightree) then
            begin
              secondpass(hightree);
              { this is a longint anyway ! }
              push_value_para(exprasmlist,hightree,calloption,para_offset,4,paralocdummy);
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
              tcallparanode(right).secondcallparan(push_from_left_to_right,
                                                   calloption,para_alignment,para_offset)
            else
              tcallparanode(right).secondcallparan(push_from_left_to_right,
                                                   calloption,para_alignment,para_offset);
          end;

         otlabel:=truelabel;
         oflabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);
         { handle varargs first, because paraitem is not valid }
         if (nf_varargs_para in flags) then
           begin
             if paramanager.push_addr_param(left.resulttype.def,calloption) then
               begin
                 inc(pushedparasize,4);
                 cg.a_paramaddr_ref(exprasmlist,left.location.reference,paralocdummy);
                 location_release(exprasmlist,left.location);
               end
             else
               push_value_para(exprasmlist,left,calloption,para_offset,para_alignment,paralocdummy);
           end
         { filter array constructor with c styled args }
         else if is_array_of_const(left.resulttype.def) and (nf_cargs in left.flags) then
           begin
             { nothing, everything is already pushed }
           end
         { in codegen.handleread.. paraitem.data is set to nil }
         else if assigned(paraitem.paratype.def) and
                 (paraitem.paratype.def.deftype=formaldef) then
           begin
              { allow passing of a constant to a const formaldef }
              if (paraitem.paratyp=vs_const) and
                 (left.location.loc=LOC_CONSTANT) then
                location_force_mem(exprasmlist,left.location);

              { allow @var }
              inc(pushedparasize,4);
              if (left.nodetype=addrn) and
                 (not(nf_procvarload in left.flags)) then
                begin
                  if calloption=pocall_inline then
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
                       if calloption=pocall_inline then
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
         else if (paraitem.paratyp in [vs_var,vs_out]) then
           begin
              if (left.location.loc<>LOC_REFERENCE) then
               begin
                 { passing self to a var parameter is allowed in
                   TP and delphi }
                 if not((left.location.loc=LOC_CREFERENCE) and
                        (left.nodetype=selfn)) then
                  internalerror(200106041);
               end;
{$ifdef unused}
              if not push_from_left_to_right then
{$endif unused}
                maybe_push_high;
              if (paraitem.paratyp=vs_out) and
                 assigned(paraitem.paratype.def) and
                 not is_class(paraitem.paratype.def) and
                 paraitem.paratype.def.needs_inittable then
                cg.g_finalize(exprasmlist,paraitem.paratype.def,left.location.reference,false);
              inc(pushedparasize,4);
              if calloption=pocall_inline then
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
{$ifdef unused}
              if push_from_left_to_right then
                maybe_push_high;
{$endif unused}
           end
         else
           begin
              tempdeftype:=resulttype.def.deftype;
              if tempdeftype=filedef then
               CGMessage(cg_e_file_must_call_by_reference);
              { open array must always push the address, this is needed to
                also push addr of small open arrays and with cdecl functions (PFV) }
              if (
                  assigned(paraitem.paratype.def) and
                  (is_open_array(paraitem.paratype.def) or
                   is_array_of_const(paraitem.paratype.def))
                 ) or
                 (
                  paramanager.push_addr_param(resulttype.def,calloption)
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

{$ifdef unused}
                   if not push_from_left_to_right then
{$endif unused}
                     maybe_push_high;
                   inc(pushedparasize,4);
                   if calloption=pocall_inline then
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
{$ifdef unused}
                   if push_from_left_to_right then
                     maybe_push_high;
{$endif unused}
                end
              else
                begin
                   push_value_para(exprasmlist,left,calloption,
                     para_offset,para_alignment,paralocdummy);
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(right) then
          begin
            if (nf_varargs_para in flags) then
              tcallparanode(right).secondcallparan(push_from_left_to_right,
                                                   calloption,para_alignment,para_offset)
            else
              tcallparanode(right).secondcallparan(push_from_left_to_right,
                                                   calloption,para_alignment,para_offset);
          end;
      end;


{*****************************************************************************
                             TI386CALLNODE
*****************************************************************************}

    procedure ti386callnode.pass_2;
      var
         regs_to_push_int : Tsupregset;
         regs_to_push_other : tregisterset;
         unusedstate: pointer;
         pushed : tpushedsaved;
         pushed_int : tpushedsavedint;
         hregister : tregister;
         oldpushedparasize : longint;
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
         returnref,
         pararef : treference;
         r,rsp : Tregister;
         vmtloc,selfloc : tlocation;
         self_is_vmt,
         vmtrefaddr,
         selfrefaddr : boolean;

      label
         dont_call;

         procedure selfloc_to_register;
         var
           hregister : tregister;
         begin
           case selfloc.loc of
             LOC_REGISTER :
               hregister:=selfloc.register;
             LOC_CREFERENCE,
             LOC_REFERENCE :
               begin
                 hregister:=rg.getaddressregister(exprasmlist);
                 if selfrefaddr then
                   begin
                     cg.a_loadaddr_ref_reg(exprasmlist,selfloc.reference,hregister);
                     selfrefaddr:=false;
                   end
                 else
                   cg.a_load_ref_reg(exprasmlist,OS_ADDR,selfloc.reference,hregister);
                 reference_release(exprasmlist,selfloc.reference);
               end;
             else
               internalerror(200303269);
           end;
           location_reset(selfloc,LOC_REGISTER,OS_ADDR);
           selfloc.register:=hregister;
         end;

      begin
         rsp.enum:=R_INTREGISTER;
         rsp.number:=NR_ESP;
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
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
         else
           params := nil;

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
                 inlinecode.para_size:=tprocdef(procdefinition).para_size(para_alignment);
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

              { save all used registers and possible registers
                used for the return value }
              regs_to_push_int := tprocdef(procdefinition).usedintregisters;
              regs_to_push_other := tprocdef(procdefinition).usedotherregisters;
              if (not is_void(resulttype.def)) and
                 (not paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption)) then
               begin
                 include(regs_to_push_int,RS_ACCUMULATOR);
                 if resulttype.def.size>sizeof(aword) then
                   include(regs_to_push_int,RS_ACCUMULATORHIGH);
               end;
              rg.saveusedintregisters(exprasmlist,pushed_int,regs_to_push_int);
              rg.saveusedotherregisters(exprasmlist,pushed,regs_to_push_other);

              { give used registers through }
              rg.usedintinproc:=rg.usedintinproc + tprocdef(procdefinition).usedintregisters;
              rg.usedinproc:=rg.usedinproc + tprocdef(procdefinition).usedotherregisters;
           end
         else
           begin
              regs_to_push_int := all_intregisters;
              regs_to_push_other:=all_registers;
              rg.saveusedintregisters(exprasmlist,pushed_int,regs_to_push_int);
              rg.saveusedotherregisters(exprasmlist,pushed,regs_to_push_other);
              rg.usedintinproc:=all_intregisters;
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
               emit_const_reg(A_SUB,S_L,pop_size,rsp);
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
              emit_reg_reg(A_MOV,S_L,rsp,R_EDI);
              if (push_size mod 8)=0 then
                emit_const_reg(A_AND,S_L,$fffffff8,rsp)
              else
                begin
                   emit_const_reg(A_SUB,S_L,push_size,rsp);
                   emit_const_reg(A_AND,S_L,$fffffff8,rsp);
                   emit_const_reg(A_SUB,S_L,push_size,rsp);
                end;
              r.enum:=R_INTREGISTER;
              r.number:=R_EDI;
              emit_reg(A_PUSH,S_L,r);
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
                tcallparanode(params).secondcallparan(
                { TParaItem(tabstractprocdef(right.resulttype.def).Para.first), }
                  (po_leftright in procdefinition.procoptions),procdefinition.proccalloption,
                  para_alignment,para_offset)
              else
                tcallparanode(params).secondcallparan(
                  { TParaItem(procdefinition.Para.first), }
                  (po_leftright in procdefinition.procoptions),procdefinition.proccalloption,
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
         if paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
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
              location_reset(vmtloc,LOC_CONSTANT,OS_ADDR);
              location_reset(selfloc,LOC_CONSTANT,OS_ADDR);
              vmtrefaddr:=false;
              selfrefaddr:=false;
              self_is_vmt:=false;

              { push self }
              if assigned(symtableproc) and
                 ((symtableproc.symtabletype in [objectsymtable,withsymtable])) then
                begin
                   { generate fake methodpointer node for withsymtable }
                   if (symtableproc.symtabletype=withsymtable) then
                    begin
                      methodpointer:=cnothingnode.create;
                      methodpointer.resulttype:=twithnode(twithsymtable(symtableproc).withnode).left.resulttype;
                    end;

                   if assigned(methodpointer) then
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
                                   self_is_vmt:=true;
                                   if (oo_has_vmt in tobjectdef(methodpointer.resulttype.def).objectoptions) then
                                     begin
                                       location_reset(vmtloc,LOC_REFERENCE,OS_NO);
                                       reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                                       vmtrefaddr:=true;
                                     end;
                                end
                              else
                                begin
                                  { member call, load self }
                                  location_reset(selfloc,LOC_REGISTER,OS_ADDR);
                                  selfloc.register:=cg.g_load_self(exprasmlist);
                                end;

                              if is_con_or_destructor then
                               begin
                                 if is_object(methodpointer.resulttype.def) then
                                  begin
                                    { object }
                                    { if an inherited con- or destructor should be  }
                                    { called in a con- or destructor then a warning }
                                    { will be made                                  }
                                    { con- and destructors need a pointer to the vmt }
                                    if not(aktprocdef.proctypeoption in
                                           [potype_constructor,potype_destructor]) then
                                      CGMessage(cg_w_member_cd_call_from_method);

                                    { reset self when calling constructor from destructor }
                                    if (procdefinition.proctypeoption=potype_constructor) and
                                       assigned(aktprocdef) and
                                       (aktprocdef.proctypeoption=potype_destructor) then
                                     begin
                                       location_release(exprasmlist,selfloc);
                                       location_reset(selfloc,LOC_CONSTANT,OS_ADDR);
                                     end;
                                  end;
                               end;
                           end;
                         hnewn:
                           begin
                              { constructor with extended syntax called from new }
                              { vmt }
                              location_reset(vmtloc,LOC_REFERENCE,OS_ADDR);
                              reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                              vmtrefaddr:=true;
                              extended_new:=true;
                           end;
                         hdisposen:
                           begin
                              { destructor with extended syntax called from dispose }
                              { hdisposen always deliver LOC_REFERENCE }
                              secondpass(methodpointer);
                              { vmt }
                              location_reset(vmtloc,LOC_REFERENCE,OS_ADDR);
                              reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymbol(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                              vmtrefaddr:=true;
                              { self, load in register first when it requires a virtual call }
                              location_reset(selfloc,LOC_REFERENCE,OS_ADDR);
                              selfloc.reference:=methodpointer.location.reference;
                              selfrefaddr:=true;
                           end;
                         else
                           begin
                              { call to an instance member }
                              if (symtableproc.symtabletype<>withsymtable) then
                                begin
                                   secondpass(methodpointer);
                                   case methodpointer.location.loc of
                                      LOC_CREGISTER,
                                      LOC_REGISTER:
                                        begin
                                          location_reset(selfloc,LOC_REGISTER,OS_ADDR);
                                          selfloc.register:=methodpointer.location.register;
                                        end;
                                      LOC_CREFERENCE,
                                      LOC_REFERENCE :
                                        begin
                                          location_reset(selfloc,LOC_REFERENCE,OS_ADDR);
                                          selfloc.reference:=methodpointer.location.reference;
                                          if (methodpointer.resulttype.def.deftype<>classrefdef) and
                                             not(is_class_or_interface(methodpointer.resulttype.def)) then
                                            selfrefaddr:=true;
                                        end;
                                      else
                                        internalerror(200303212);
                                   end;
                                end
                              else
                                begin
                                  location_reset(selfloc,LOC_REFERENCE,OS_ADDR);
                                  selfloc.reference:=twithnode(twithsymtable(symtableproc).withnode).withreference;
                                  if (nf_islocal in twithnode(twithsymtable(symtableproc).withnode).flags) and
                                     (twithsymtable(symtableproc).direct_with) and
                                     not(is_class_or_interface(twithnode(twithsymtable(symtableproc).withnode).left.resulttype.def)) then
                                    selfrefaddr:=true;
                                end;

                              if (po_staticmethod in procdefinition.procoptions) or
                                 (po_classmethod in procdefinition.procoptions) then
                                begin
                                  self_is_vmt:=true;
                                  { classref are already loaded with VMT }
                                  if (methodpointer.resulttype.def.deftype=classrefdef) then
                                   location_copy(vmtloc,selfloc)
                                  else
                                   begin
                                     if (oo_has_vmt in tprocdef(procdefinition)._class.objectoptions) then
                                       begin
                                         { load VMT from passed self }
                                         selfloc_to_register;
                                         cg.g_maybe_testself(exprasmlist,selfloc.register);
                                         location_copy(vmtloc,selfloc);
                                         reference_reset_base(href,vmtloc.register,tprocdef(procdefinition)._class.vmt_offset);
                                         cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,vmtloc.register);
                                       end;
                                   end;
                                  { reset self }
                                  location_reset(selfloc,LOC_CONSTANT,OS_ADDR);
                                end;

                              if is_con_or_destructor then
                               begin
                                 { constructor call via classreference => allocate memory }
                                 if (methodpointer.resulttype.def.deftype=classrefdef) then
                                   begin
                                     if (procdefinition.proctypeoption=potype_constructor) and
                                        is_class(tclassrefdef(methodpointer.resulttype.def).pointertype.def) then
                                      begin
                                        self_is_vmt:=true;
                                        { vmt load from provided methodpointer that
                                          was already loaded in selfloc }
                                        location_copy(vmtloc,selfloc);
                                        { reset self }
                                        location_reset(selfloc,LOC_CONSTANT,OS_ADDR);
                                      end;
                                  end
                                 else
                                 { class }
                                  if is_class(methodpointer.resulttype.def) then
                                   begin
                                     { destructor: release instance, flag(vmt)=1
                                       constructor: direct call, do nothing, leave vmt=0 }
                                     if (procdefinition.proctypeoption=potype_destructor) then
                                      begin
                                        { flag 1 for destructor: remove data }
                                        location_reset(vmtloc,LOC_CONSTANT,OS_ADDR);
                                        vmtloc.value:=1;
                                      end;
                                   end
                                 else
                                 { object }
                                  begin
                                    { destructor: direct call, no dispose, vmt=0
                                      constructor: initialize object, load vmt }
                                    if (procdefinition.proctypeoption=potype_constructor) then
                                     begin
                                       { vmt }
                                       location_reset(vmtloc,LOC_REFERENCE,OS_ADDR);
                                       reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymbol(
                                          tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                                       vmtrefaddr:=true;
                                     end;
                                  end;
                               end;
                           end;
                       end;
                     end
                   else
                     { No methodpointer }
                     begin
                        if (po_staticmethod in procdefinition.procoptions) or
                           (po_classmethod in procdefinition.procoptions) then
                         begin
                           self_is_vmt:=true;
                           { Load VMT from self? }
                           if (
                               (po_classmethod in procdefinition.procoptions) and
                               not(assigned(aktprocdef) and
                                   (po_classmethod in aktprocdef.procoptions))
                              ) or
                              (
                               (po_staticmethod in procdefinition.procoptions) and
                                not(assigned(aktprocdef) and
                                    (po_staticmethod in aktprocdef.procoptions))
                              ) then
                             begin
                               if (oo_has_vmt in tprocdef(procdefinition)._class.objectoptions) then
                                begin
                                  { load vmt from self passed to the current method }
                                  location_reset(vmtloc,LOC_REGISTER,OS_ADDR);
                                  vmtloc.register:=cg.g_load_self(exprasmlist);
                                  cg.g_maybe_testself(exprasmlist,vmtloc.register);
                                  reference_reset_base(href,vmtloc.register,tprocdef(procdefinition)._class.vmt_offset);
                                  cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,vmtloc.register);
                                end;
                             end
                            else
                             begin
                               { self is already VMT }
                               location_reset(vmtloc,LOC_REGISTER,OS_ADDR);
                               vmtloc.register:=cg.g_load_self(exprasmlist);
                             end;
                          end
                        else
                          begin
                             { member call, load self }
                             location_reset(selfloc,LOC_REGISTER,OS_ADDR);
                             selfloc.register:=cg.g_load_self(exprasmlist);
                          end;
                     end;

                   { Do we need to push the VMT as self for
                     class methods and static methods? }
                   if self_is_vmt then
                     begin
                       location_release(exprasmlist,selfloc);
                       location_copy(selfloc,vmtloc);
                     end;

                   { already load self into register when we know that
                     we need to call a virtual method }
                   if (po_virtualmethod in procdefinition.procoptions) and
                      not(no_virtual_call) then
                     selfloc_to_register;

                   { constructor/destructor need vmt }
                   if is_con_or_destructor then
                    begin
                      if vmtrefaddr then
                        cg.a_paramaddr_ref(exprasmlist,vmtloc.reference,paramanager.getintparaloc(2))
                      else
                        cg.a_param_loc(exprasmlist,vmtloc,paramanager.getintparaloc(2));
                    end;
                   if not self_is_vmt then
                     location_release(exprasmlist,vmtloc);

                   { push self }
                   if selfrefaddr then
                     cg.a_paramaddr_ref(exprasmlist,selfloc.reference,paramanager.getintparaloc(1))
                   else
                     cg.a_param_loc(exprasmlist,selfloc,paramanager.getintparaloc(1));
                end;

              { push base pointer ?}
              { never when inlining, since if necessary, the base pointer }
              { can/will be gottten from the current procedure's symtable }
              { (JM)                                                      }
              if not inlined then
                begin
                  if (lexlevel>=normal_function_level) and assigned(tprocdef(procdefinition).parast) and
                    ((tprocdef(procdefinition).parast.symtablelevel)>normal_function_level) then
                    begin
                       if lexlevel=(tprocdef(procdefinition).parast.symtablelevel) then
                         begin
                            reference_reset_base(href,procinfo.framepointer,procinfo.framepointer_offset);
                            cg.a_param_ref(exprasmlist,OS_ADDR,href,paramanager.getintparaloc(1));
                         end
                       { this is only true if the difference is one !!
                         but it cannot be more !! }
                       else if (lexlevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
                         begin
                            cg.a_param_reg(exprasmlist,OS_ADDR,procinfo.framepointer,paramanager.getintparaloc(1));
                         end
                       else if (lexlevel>(tprocdef(procdefinition).parast.symtablelevel)) then
                         begin
                            hregister:=rg.getregisterint(exprasmlist,OS_ADDR);
                            reference_reset_base(href,procinfo.framepointer,procinfo.framepointer_offset);
                            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                            for i:=(tprocdef(procdefinition).parast.symtablelevel) to lexlevel-1 do
                              begin
                                 {we should get the correct frame_pointer_offset at each level
                                 how can we do this !!! }
                                 reference_reset_base(href,hregister,procinfo.framepointer_offset);
                                 cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                              end;
                            cg.a_param_reg(exprasmlist,OS_ADDR,hregister,paramanager.getintparaloc(1));
                            rg.ungetregisterint(exprasmlist,hregister);
                         end
                       else
                         internalerror(25000);
                    end;
                end;

              rg.saveintregvars(exprasmlist,regs_to_push_int);

              if (po_virtualmethod in procdefinition.procoptions) and
                 not(no_virtual_call) then
                begin
                   { virtual methods require an index }
                   if tprocdef(procdefinition).extnumber=-1 then
                     internalerror(44584);

                   { self should already be loaded in a register }
                   if selfloc.register.number=NR_NO then
                     internalerror(2003032611);

                   { load vmt from self, this is already done
                     for static/class methods }
                   if not self_is_vmt then
                    begin
                      cg.g_maybe_testself(exprasmlist,selfloc.register);
                      { this is one point where we need vmt_offset (PM) }
                      reference_reset_base(href,selfloc.register,tprocdef(procdefinition)._class.vmt_offset);
                      cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,selfloc.register);
                    end;

                   { test validity of VMT }
                   if not(is_interface(tprocdef(procdefinition)._class)) and
                      not(is_cppclass(tprocdef(procdefinition)._class)) then
                     cg.g_maybe_testvmt(exprasmlist,selfloc.register,tprocdef(procdefinition)._class);

                   { call method }
                   reference_reset_base(href,selfloc.register,
                      tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber));
                   cg.a_call_ref(exprasmlist,href);

                   { release self }
                   location_release(exprasmlist,selfloc);
                end
              else
                begin
                  { self is not needed }
                  location_release(exprasmlist,selfloc);

                  if not inlined then
                   begin
                     { We can call interrupts from within the smae code
                       by just pushing the flags and CS PM }
                     if (po_interrupt in procdefinition.procoptions) then
                       begin
                           emit_none(A_PUSHF,S_L);
                           r.enum:=R_INTREGISTER;
                           r.number:=NR_CS;
                           emit_reg(A_PUSH,S_L,r);
                       end;
                     cg.a_call_name(exprasmlist,tprocdef(procdefinition).mangledname);
                   end
                  else { inlined proc }
                   begin
                     { process the inlinecode }
                     secondpass(tnode(inlinecode));
                     { free the args }
                     if tprocdef(procdefinition).parast.datasize>0 then
                       tg.UnGetTemp(exprasmlist,pararef);
                   end;
               end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(right);
              if (po_interrupt in procdefinition.procoptions) then
                begin
                    emit_none(A_PUSHF,S_L);
                    r.enum:=R_INTREGISTER;
                    r.number:=NR_CS;
                    emit_reg(A_PUSH,S_L,r);
                end;
              { procedure of object? }
              if (po_methodpointer in procdefinition.procoptions) then
                begin
                   { push self, but not if it's already explicitly pushed }
                   if not(po_containsself in procdefinition.procoptions) then
                     begin
                       { push self }
                       href:=right.location.reference;
                       inc(href.offset,POINTER_SIZE);
                       cg.a_param_ref(exprasmlist,OS_ADDR,href,paramanager.getintparaloc(1));
                     end;

                   rg.saveintregvars(exprasmlist,ALL_INTREGISTERS);
                   rg.saveotherregvars(exprasmlist,ALL_REGISTERS);
                   cg.a_call_ref(exprasmlist,right.location.reference);

                   reference_release(exprasmlist,right.location.reference);
                   tg.Ungetiftemp(exprasmlist,right.location.reference);
                end
              else
                begin
                   rg.saveintregvars(exprasmlist,ALL_INTREGISTERS);
                   rg.saveotherregvars(exprasmlist,ALL_REGISTERS);
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
                    r:=rg.getexplicitregisterint(exprasmlist,NR_EDI);
                    emit_reg(A_POP,S_L,r);
                    rg.ungetregisterint(exprasmlist,r);
                  end
                { the pentium has two pipes and pop reg is pairable }
                { but the registers must be different!        }
                else if (pushedparasize=8) and
                  not(cs_littlesize in aktglobalswitches) and
                  (aktoptprocessor=ClassP5) and
                  (procinfo._class=nil) then
                    begin
                       r:=rg.getexplicitregisterint(exprasmlist,NR_EDI);
                       emit_reg(A_POP,S_L,r);
                       rg.ungetregisterint(exprasmlist,r);
                       r:=rg.getexplicitregisterint(exprasmlist,NR_ESI);
                       emit_reg(A_POP,S_L,r);
                       rg.ungetregisterint(exprasmlist,r);
                    end
                else if pushedparasize<>0 then
                  emit_const_reg(A_ADD,S_L,pushedparasize,rsp);
             end;
{$ifdef OPTALIGN}
         if pop_esp then
           emit_reg(A_POP,S_L,rsp);
{$endif OPTALIGN}
      dont_call:
         pushedparasize:=oldpushedparasize;
         rg.restoreunusedstate(unusedstate);
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}

         { Called an constructor ? }
         if (inlined or
             (right=nil)) and
            (procdefinition.proctypeoption=potype_constructor) and
            assigned(methodpointer) then
          begin
            { calling inherited constructor failed? Yes, goto fail }
            if (methodpointer.nodetype=typen) and
               (aktprocdef.proctypeoption=potype_constructor) then
             begin
               r.enum:=R_INTREGISTER;
               r.number:=NR_ACCUMULATOR;
               cg.a_reg_alloc(exprasmlist,r);
               cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,r,faillabel);
               cg.a_reg_dealloc(exprasmlist,r);
             end;
           end;

         { handle function results }
         if (not is_void(resulttype.def)) then
          handle_return_value(inlined,extended_new);

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              reference_reset_symbol(href,iolabel,0);
              cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
           end;
         if pop_size>0 then
           emit_const_reg(A_ADD,S_L,pop_size,rsp);

         { restore registers }
         rg.restoreusedotherregisters(exprasmlist,pushed);
         rg.restoreusedintregisters(exprasmlist,pushed_int);

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
             if (resulttype.def.size>0) then
               tg.UnGetTemp(exprasmlist,returnref);
             tprocdef(procdefinition).parast.address_fixup:=store_parast_fixup;
             right:=inlinecode;
           end;
         if assigned(params) then
           params.free;

         { from now on the result can be freed normally }
         if inlined and paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
           tg.ChangeTempType(exprasmlist,funcretref,tt_normal);

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
                  r.enum:=R_ST;
                  emit_reg(A_FSTP,S_NO,r);
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
  Revision 1.86  2003-03-30 20:59:07  peter
    * fix classmethod from classmethod call
    * move BeforeDestruction/AfterConstruction calls to
      genentrycode/genexitcode instead of generating them on the fly
      after a call to a constructor

  Revision 1.85  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.84  2003/03/13 19:52:23  jonas
    * and more new register allocator fixes (in the i386 code generator this
      time). At least now the ppc cross compiler can compile the linux
      system unit again, but I haven't tested it.

  Revision 1.83  2003/03/06 11:35:50  daniel
    * Fixed internalerror 7843 issue

  Revision 1.82  2003/02/19 22:00:15  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.81  2003/01/30 21:46:57  peter
    * self fixes for static methods (merged)

  Revision 1.80  2003/01/13 18:37:44  daniel
    * Work on register conversion

  Revision 1.79  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.78  2002/12/15 21:30:12  florian
    * tcallnode.paraitem introduced, all references to defcoll removed

  Revision 1.77  2002/11/27 20:05:06  peter
    * cdecl array of const fixes

  Revision 1.76  2002/11/25 17:43:26  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.75  2002/11/18 17:32:00  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.74  2002/11/15 01:58:57  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.73  2002/10/05 12:43:29  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.72  2002/09/17 18:54:03  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.71  2002/09/16 19:07:37  peter
    * push 0 instead of VMT when calling a constructor from a member

  Revision 1.70  2002/09/07 15:25:10  peter
    * old logs removed and tabs fixed

  Revision 1.69  2002/09/01 18:43:27  peter
    * include accumulator in regs_to_push list

  Revision 1.68  2002/09/01 12:13:00  peter
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

}
