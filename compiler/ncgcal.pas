{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

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
unit ncgcal;

{$i fpcdefs.inc}

interface

{ $define AnsiStrRef}

    uses
      cpubase,
      globtype,
      symdef,node,ncal;

    type
       tcgcallparanode = class(tcallparanode)
          procedure secondcallparan(push_from_left_to_right:boolean;calloption:tproccalloption;
                para_alignment,para_offset : longint);override;
       end;

       tcgcallnode = class(tcallnode)
       private
          function  push_self_and_vmt(needvmtreg:boolean):tregister;
       protected
//          funcretref : treference;
          refcountedtemp : treference;
          procedure handle_return_value(inlined:boolean);
          {# This routine is used to push the current frame pointer
             on the stack. This is used in nested routines where the
             value of the frame pointer is always pushed as an extra
             parameter.

             The default handling is the standard handling used on
             most stack based machines, where the frame pointer is
             the first invisible parameter.
          }
          function  align_parasize(parasize,para_alignment:longint):longint;virtual;
          procedure pop_parasize(pop_size:longint);virtual;
          procedure push_framepointer;virtual;
          procedure extra_interrupt_code;virtual;
       public
          procedure pass_2;override;
       end;

       tcgprocinlinenode = class(tprocinlinenode)
          procedure pass_2;override;
       end;


implementation

    uses
      systems,
      cutils,verbose,globals,
      symconst,symbase,symsym,symtable,defutil,paramgr,
{$ifdef GDB}
  {$ifdef delphi}
      sysutils,
  {$else}
      strings,
  {$endif}
      gdb,
{$endif GDB}
      cginfo,cgbase,pass_2,
      cpuinfo,cpupi,aasmbase,aasmtai,aasmcpu,
      nbas,nmem,nld,ncnv,
{$ifdef x86}
      cga,
{$endif x86}
{$ifdef cpu64bit}
      cg64f64,
{$else cpu64bit}
      cg64f32,
{$endif cpu64bit}
      ncgutil,cgobj,tgobj,regvars,rgobj,rgcpu,cgcpu;


    var
      { Current callnode, this is needed for having a link
        between the callparanodes and the callnode they belong to }
      aktcallnode : tcallnode;

{*****************************************************************************
                             TCGCALLPARANODE
*****************************************************************************}

    procedure tcgcallparanode.secondcallparan(push_from_left_to_right:boolean;calloption:tproccalloption;para_alignment,para_offset : longint);
      var
         otlabel,
         oflabel : tasmlabel;
         tmpreg  : tregister;
         href    : treference;
         varspez : tvarspez;
      begin
         if not(assigned(paraitem.paratype.def) or
                assigned(paraitem.parasym)) then
           internalerror(200304242);

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
         { retrieve the type of parameter, for hidden parameters
           the value is stored in the parasym }
         varspez:=paraitem.paratyp;
         if varspez=vs_hidden then
           varspez:=tvarsym(paraitem.parasym).varspez;
         { handle varargs first, because defcoll is not valid }
         if (nf_varargs_para in flags) then
           begin
             if paramanager.push_addr_param(left.resulttype.def,calloption) then
               begin
                 inc(pushedparasize,POINTER_SIZE);
                 cg.a_paramaddr_ref(exprasmlist,left.location.reference,paraitem.paraloc);
                 location_release(exprasmlist,left.location);
               end
             else
               push_value_para(exprasmlist,left,calloption,para_offset,para_alignment,paraitem.paraloc);
           end
         { filter array of const c styled args }
         else if is_array_of_const(left.resulttype.def) and (nf_cargs in left.flags) then
           begin
             { nothing, everything is already pushed }
           end
         { in codegen.handleread.. paraitem.data is set to nil }
         else if assigned(paraitem.paratype.def) and
                 (paraitem.paratype.def.deftype=formaldef) then
           begin
              { allow passing of a constant to a const formaldef }
              if (varspez=vs_const) and
                 (left.location.loc=LOC_CONSTANT) then
                location_force_mem(exprasmlist,left.location);

              { allow @var }
              inc(pushedparasize,POINTER_SIZE);
              if (left.nodetype=addrn) and
                 (not(nf_procvarload in left.flags)) then
                begin
                  if calloption=pocall_inline then
                    begin
                       reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                       cg.a_load_loc_ref(exprasmlist,left.location,href);
                    end
                  else
                    cg.a_param_loc(exprasmlist,left.location,paraitem.paraloc);
                  location_release(exprasmlist,left.location);
                end
              else
                begin
                   if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                     internalerror(200304235);

                   if calloption=pocall_inline then
                     begin
                     {$ifdef newra}
                       tmpreg:=rg.getaddressregister(exprasmlist);
                     {$else}
                       tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                     {$endif newra}
                       cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                       reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                       cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                     {$ifdef newra}
                       rg.ungetregisterint(exprasmlist,tmpreg);
                     {$else}
                       cg.free_scratch_reg(exprasmlist,tmpreg);
                     {$endif}
                     end
                   else
                     cg.a_paramaddr_ref(exprasmlist,left.location.reference,paraitem.paraloc);
                   location_release(exprasmlist,left.location);
                end;
           end
         { handle call by reference parameter }
         else if (varspez in [vs_var,vs_out]) then
           begin
              if (left.location.loc<>LOC_REFERENCE) then
               begin
                 { passing self to a var parameter is allowed in
                   TP and delphi }
                 if not((left.location.loc=LOC_CREFERENCE) and
                        (left.nodetype=selfn)) then
                  internalerror(200106041);
               end;
              if (varspez=vs_out) and
                 assigned(paraitem.paratype.def) and
                 not is_class(paraitem.paratype.def) and
                 paraitem.paratype.def.needs_inittable then
                cg.g_finalize(exprasmlist,paraitem.paratype.def,left.location.reference,false);
              inc(pushedparasize,POINTER_SIZE);
              if calloption=pocall_inline then
                begin
                {$ifdef newra}
                   tmpreg:=rg.getaddressregister(exprasmlist);
                {$else}
                   tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                {$endif}
                   cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                   reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                   cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                {$ifdef newra}
                   rg.ungetregisterint(exprasmlist,tmpreg);
                {$else}
                   cg.free_scratch_reg(exprasmlist,tmpreg);
                {$endif}
                end
              else
                cg.a_paramaddr_ref(exprasmlist,left.location.reference,paraitem.paraloc);
              location_release(exprasmlist,left.location);
           end
         else
           begin
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
                         if not (left.location.size in [OS_64,OS_S64]) then
                           cg.a_load_loc_ref(exprasmlist,left.location,href)
                         else
                           cg64.a_load64_loc_ref(exprasmlist,left.location,href);
                         location_reset(left.location,LOC_REFERENCE,left.location.size);
                         left.location.reference:=href;
                       end
                      else
                       internalerror(200204011);
                    end;

                   inc(pushedparasize,POINTER_SIZE);
                   if calloption=pocall_inline then
                     begin
                     {$ifdef newra}
                        tmpreg:=rg.getaddressregister(exprasmlist);
                     {$else}
                        tmpreg:=cg.get_scratch_reg_address(exprasmlist);
                     {$endif}
                        cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                        reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                        cg.a_load_reg_ref(exprasmlist,OS_ADDR,tmpreg,href);
                     {$ifdef newra}
                        rg.ungetregisterint(exprasmlist,tmpreg);
                     {$else}
                        cg.free_scratch_reg(exprasmlist,tmpreg);
                     {$endif}
                     end
                   else
                     cg.a_paramaddr_ref(exprasmlist,left.location.reference,paraitem.paraloc);
                   location_release(exprasmlist,left.location);
                end
              else
                begin
                   push_value_para(exprasmlist,left,calloption,
                     para_offset,para_alignment,paraitem.paraloc);
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;

         { update return location in callnode when this is the function
           result }
         if (vo_is_funcret in tvarsym(paraitem.parasym).varoptions) then
          begin
            location_copy(aktcallnode.location,left.location);
          end;

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
                             TCGCALLNODE
*****************************************************************************}

    procedure tcgcallnode.extra_interrupt_code;
      begin
      end;


    function tcgcallnode.align_parasize(parasize,para_alignment:longint):longint;
      begin
        result:=0;
      end;


    procedure tcgcallnode.pop_parasize(pop_size:longint);
      begin
      end;


    function tcgcallnode.push_self_and_vmt(needvmtreg:boolean):tregister;
      var
         href : treference;
         vmtloc,selfloc : tlocation;
         self_is_vmt,
         vmtrefaddr,
         selfrefaddr : boolean;

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
        result.enum:=R_INTREGISTER;
        result.number:=NR_NO;
        location_reset(vmtloc,LOC_CONSTANT,OS_ADDR);
        location_reset(selfloc,LOC_CONSTANT,OS_ADDR);
        vmtrefaddr:=false;
        selfrefaddr:=false;
        self_is_vmt:=false;

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
                   if (sp_static in symtableprocentry.symoptions) then
                     begin
                        self_is_vmt:=true;
                        if (oo_has_vmt in tobjectdef(methodpointer.resulttype.def).objectoptions) then
                          begin
                            location_reset(vmtloc,LOC_REFERENCE,OS_NO);
                            reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymboldata(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                            vmtrefaddr:=true;
                          end;
                     end
                   else
                     begin
                       { normal member call, load self. Not for classes
                         when we call the constructor }
                       if not(
                              is_class(methodpointer.resulttype.def) and
                              (procdefinition.proctypeoption=potype_constructor) and
                              (current_procdef.proctypeoption<>potype_constructor)
                             ) then
                        begin
                          location_reset(selfloc,LOC_REGISTER,OS_ADDR);
                          selfloc.register:=cg.g_load_self(exprasmlist);
                        end;
                     end;

                   if (procdefinition.proctypeoption in [potype_constructor,potype_destructor]) then
                    begin
                      if is_object(methodpointer.resulttype.def) then
                       begin
                         { reset self when calling constructor from destructor }
                         if (procdefinition.proctypeoption=potype_constructor) and
                            assigned(current_procdef) and
                            (current_procdef.proctypeoption=potype_destructor) then
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
                   reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymboldata(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
                   vmtrefaddr:=true;
                end;
              hdisposen:
                begin
                   { destructor with extended syntax called from dispose }
                   { hdisposen always deliver LOC_REFERENCE }
                   secondpass(methodpointer);
                   { vmt }
                   location_reset(vmtloc,LOC_REFERENCE,OS_ADDR);
                   reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymboldata(tobjectdef(methodpointer.resulttype.def).vmt_mangledname),0);
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

                   if (procdefinition.proctypeoption in [potype_constructor,potype_destructor]) then
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
                            reference_reset_symbol(vmtloc.reference,objectlibrary.newasmsymboldata(
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
                    not(assigned(current_procdef) and
                        (po_classmethod in current_procdef.procoptions))
                   ) or
                   (
                    (po_staticmethod in procdefinition.procoptions) and
                     not(assigned(current_procdef) and
                         (po_staticmethod in current_procdef.procoptions))
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
            selfrefaddr:=vmtrefaddr;
          end;

        { when we need the vmt in a register then we already
          load self in a register so it can generate optimized code }
        if needvmtreg then
          selfloc_to_register;

        { constructor/destructor need vmt }
        if (procdefinition.proctypeoption in [potype_constructor,potype_destructor]) then
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

        if needvmtreg then
          begin
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

            result:=selfloc.register;
          end
        else
          location_release(exprasmlist,selfloc);
     end;


    procedure tcgcallnode.push_framepointer;
      var
        href : treference;
        hregister : tregister;
        i : integer;
      begin
        { this routine is itself not nested }
        if current_procdef.parast.symtablelevel=(tprocdef(procdefinition).parast.symtablelevel) then
          begin
            reference_reset_base(href,current_procinfo.framepointer,current_procinfo.framepointer_offset);
            cg.a_param_ref(exprasmlist,OS_ADDR,href,paramanager.getintparaloc(1));
          end
        { one nesting level }
        else if (current_procdef.parast.symtablelevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
          begin
            cg.a_param_reg(exprasmlist,OS_ADDR,current_procinfo.framepointer,paramanager.getintparaloc(1));
          end
        { very complex nesting level ... }
        else if (current_procdef.parast.symtablelevel>(tprocdef(procdefinition).parast.symtablelevel)) then
          begin
            hregister:=rg.getaddressregister(exprasmlist);
            reference_reset_base(href,current_procinfo.framepointer,current_procinfo.framepointer_offset);
            cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
            i:=current_procdef.parast.symtablelevel;
            while (i>tprocdef(procdefinition).parast.symtablelevel) do
              begin
                reference_reset_base(href,hregister,current_procinfo.framepointer_offset);
                cg.a_load_ref_reg(exprasmlist,OS_ADDR,href,hregister);
                dec(i);
              end;
            cg.a_param_reg(exprasmlist,OS_ADDR,hregister,paramanager.getintparaloc(1));
            rg.ungetaddressregister(exprasmlist,hregister);
          end;
      end;


    procedure tcgcallnode.handle_return_value(inlined:boolean);
      var
        cgsize : tcgsize;
        r,hregister : tregister;
        nr:Tnewregister;
      begin
        { structured results are easy to handle.... }
        { needed also when result_no_used !! }
        if paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
         begin
           { Location should be setup by the funcret para }
           if location.loc<>LOC_REFERENCE then
            internalerror(200304241);
         end
        else
        { ansi/widestrings must be registered, so we can dispose them }
         if is_ansistring(resulttype.def) or
            is_widestring(resulttype.def) then
          begin
            location_reset(location,LOC_CREFERENCE,OS_ADDR);
            location.reference:=refcountedtemp;
            r.enum:=accumulator;
            cg.a_reg_alloc(exprasmlist,r);
            cg.a_load_reg_ref(exprasmlist,OS_ADDR,r,location.reference);
            cg.a_reg_dealloc(exprasmlist,r);
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

                  { an object constructor is a function with pointer result }
                  if (inlined or (right=nil)) and
                     (procdefinition.proctypeoption=potype_constructor) then
                    cgsize:=OS_ADDR;

                  if cgsize<>OS_NO then
                   begin
                     location_reset(location,LOC_REGISTER,cgsize);
{$ifndef cpu64bit}
                     if cgsize in [OS_64,OS_S64] then
                      begin
                        {Move the function result to free registers, preferably the
                         accumulator/accumulatorhigh, so no move is necessary.}
                        r.enum:=R_INTREGISTER;
                        r.number:=NR_ACCUMULATOR;
                        hregister.enum:=R_INTREGISTER;
                        hregister.number:=NR_ACCUMULATORHIGH;
{$ifdef newra}
                        rg.getexplicitregisterint(exprasmlist,NR_ACCUMULATOR);
                        rg.getexplicitregisterint(exprasmlist,NR_ACCUMULATORHIGH);
                        rg.ungetregisterint(exprasmlist,r);
                        rg.ungetregisterint(exprasmlist,hregister);
                        location.registerlow:=rg.getregisterint(exprasmlist,OS_INT);
                        location.registerhigh:=rg.getregisterint(exprasmlist,OS_INT);
{$else newra}
                        cg.a_reg_alloc(exprasmlist,r);
                        cg.a_reg_alloc(exprasmlist,hregister);
                        if RS_ACCUMULATOR in rg.unusedregsint then
                          location.registerlow:=rg.getexplicitregisterint(exprasmlist,NR_ACCUMULATOR)
                        else
                          location.registerlow:=rg.getregisterint(exprasmlist,OS_INT);
                        if RS_ACCUMULATORHIGH in rg.unusedregsint then
                          location.registerhigh:=rg.getexplicitregisterint(exprasmlist,NR_ACCUMULATORHIGH)
                        else
                          location.registerhigh:=rg.getregisterint(exprasmlist,OS_INT);
{$endif newra}
                        cg64.a_load64_reg_reg(exprasmlist,joinreg64(r,hregister),
                            location.register64);
                      end
                     else
{$endif cpu64bit}
                      begin
                        {Move the function result to a free register, preferably the
                         accumulator, so no move is necessary.}
                        nr:=RS_ACCUMULATOR shl 8 or cgsize2subreg(cgsize);
                        r.enum:=R_INTREGISTER;
                        r.number:=nr;
{$ifdef newra}
                        rg.getexplicitregisterint(exprasmlist,nr);
                        rg.ungetregisterint(exprasmlist,r);
                        location.register:=rg.getregisterint(exprasmlist,cgsize);
{$else newra}
                        cg.a_reg_alloc(exprasmlist,r);
                        if RS_ACCUMULATOR in rg.unusedregsint then
                          location.register:=rg.getexplicitregisterint(exprasmlist,nr)
                        else
                          location.register:=rg.getregisterint(exprasmlist,cgsize);
{$endif newra}
                        cg.a_load_reg_reg(exprasmlist,cgsize,cgsize,r,location.register);
                      end;
                   end;
                end;
              floatdef :
                begin
                  location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
{$ifdef cpufpemu}
                  if cs_fp_emulation in aktmoduleswitches then
                    location.register.enum := accumulator
                 else
{$endif cpufpemu}
                  location.register.enum:=FPU_RESULT_REG;
{$ifdef x86}
                  inc(trgcpu(rg).fpuvaroffset);
{$endif x86}
                end;
{$ifdef TEST_WIN32_RECORDS}
              recorddef :
                begin
                  if (target_info.system=system_i386_win32) then
                   begin
                     location_reset(location,LOC_REFERENCE,def_cgsize(resulttype.def));
                     tg.GetTemp(exprasmlist,resulttype.size,tt_normal,location);
{$ifndef cpu64bit}
                     if cgsize in [OS_64,OS_S64] then
                       cg64.a_load64_reg_loc(exprasmlist,joinreg64(accumulator,accumulatorhigh),location)
                     else
{$endif cpu64bit}
                       cg.a_load_reg_loc(exprasmlist,accumulator,location);
                   end
                  else
                   internalerror(200211141);
                end;
{$endif TEST_WIN32_RECORDS}
              else
                begin
                  location_reset(location,LOC_REGISTER,OS_INT);
                  r.enum:=R_INTREGISTER;
                  r.number:=NR_ACCUMULATOR;
{$ifdef newra}
                  rg.getexplicitregisterint(exprasmlist,NR_ACCUMULATOR);
                  rg.ungetregisterint(exprasmlist,r);
                  location.register:=rg.getregisterint(exprasmlist,OS_INT);
{$else newra}
                  if RS_ACCUMULATOR in rg.unusedregsint then
                    location.register:=rg.getexplicitregisterint(exprasmlist,NR_ACCUMULATOR)
                  else
                    location.register:=rg.getregisterint(exprasmlist,OS_INT);
{$endif newra}
                  cg.a_load_reg_reg(exprasmlist,OS_INT,OS_INT,r,location.register);
                end;
            end;
         end
        else
         location_reset(location,LOC_VOID,OS_NO);
      end;


    procedure tcgcallnode.pass_2;
      var
         regs_to_push_int : Tsupregset;
         regs_to_push_other : tregisterset;
         unusedstate: pointer;
         pushed : tpushedsaved;
         pushedint : tpushedsavedint;
         oldpushedparasize : longint;
         { adress returned from an I/O-error }
         iolabel : tasmlabel;
         { help reference pointer }
         href : treference;
         hp : tnode;
         pp : tcallparanode;
         virtual_vmt_call,
         inlined : boolean;
         inlinecode : tprocinlinenode;
         store_parast_fixup,
         para_alignment,
         para_offset : longint;
         pop_size : longint;
         returnref,
         pararef : treference;
         accreg,
         vmtreg : tregister;
         oldaktcallnode : tcallnode;
      begin
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         rg.saveunusedstate(unusedstate);

         { if we allocate the temp. location for ansi- or widestrings }
         { already here, we avoid later a push/pop                    }
         if is_widestring(resulttype.def) then
           begin
             tg.GetTemp(exprasmlist,pointer_size,tt_widestring,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp,false);
           end
         else if is_ansistring(resulttype.def) then
           begin
             tg.GetTemp(exprasmlist,pointer_size,tt_ansistring,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp,false);
           end;

         if (procdefinition.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_stdcall]) then
          para_alignment:=4
         else
          para_alignment:=aktalignment.paraalign;

         if not assigned(procdefinition) then
          exit;

         if (procdefinition.proccalloption=pocall_inline) then
           begin
              inlined:=true;
              inlinecode:=tprocinlinenode(right);
              right:=nil;
              { set it to the same lexical level as the local symtable, becuase
                the para's are stored there }
              tprocdef(procdefinition).parast.symtablelevel:=current_procdef.localst.symtablelevel;
              if assigned(left) then
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

         { proc variables destroy all registers }
         if (inlined or
            (right=nil)) and
            { virtual methods too }
            not(po_virtualmethod in procdefinition.procoptions) then
           begin
              if (cs_check_io in aktlocalswitches) and
                 (po_iocheck in procdefinition.procoptions) and
                 not(po_iocheck in current_procdef.procoptions) then
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
{$ifndef cpu64bit}
                 if resulttype.def.size>sizeof(aword) then
                   include(regs_to_push_int,RS_ACCUMULATORHIGH);
{$endif cpu64bit}
               end;
              rg.saveusedintregisters(exprasmlist,pushedint,regs_to_push_int);
              rg.saveusedotherregisters(exprasmlist,pushed,regs_to_push_other);

              { give used registers through }
              rg.usedintinproc:=rg.usedintinproc + tprocdef(procdefinition).usedintregisters;
              rg.usedinproc:=rg.usedinproc + tprocdef(procdefinition).usedotherregisters;
           end
         else
           begin
              regs_to_push_int := all_intregisters;
              regs_to_push_other := all_registers;
              rg.saveusedintregisters(exprasmlist,pushedint,regs_to_push_int);
              rg.saveusedotherregisters(exprasmlist,pushed,regs_to_push_other);
              rg.usedinproc:=all_registers;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { Initialize for pushing the parameters }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         pop_size:=0;

         { Align stack if required }
         if not inlined then
           pop_size:=align_parasize(oldpushedparasize,para_alignment);

         { Push parameters }
         oldaktcallnode:=aktcallnode;
         aktcallnode:=self;
         if assigned(left) then
           begin
              { be found elsewhere }
              if inlined then
                para_offset:=tprocdef(procdefinition).parast.address_fixup+
                  tprocdef(procdefinition).parast.datasize
              else
                para_offset:=0;
              if not(inlined) and
                 assigned(right) then
                tcallparanode(left).secondcallparan(
                  (po_leftright in procdefinition.procoptions),procdefinition.proccalloption,
                  para_alignment,para_offset)
              else
                tcallparanode(left).secondcallparan(
                  (po_leftright in procdefinition.procoptions),procdefinition.proccalloption,
                  para_alignment,para_offset);
           end;
         aktcallnode:=oldaktcallnode;

         { Allocate return value for inlined routines }
         if inlined and
            (resulttype.def.size>0) then
           begin
             tg.GetTemp(exprasmlist,Align(resulttype.def.size,aktalignment.paraalign),tt_persistant,returnref);
             inlinecode.retoffset:=returnref.offset;
           end;

         { procedure variable or normal function call ? }
         if inlined or
            (right=nil) then
           begin
              { Virtual function call through VMT? }
              vmtreg.enum:=R_INTREGISTER;
              vmtreg.number:=NR_NO;
              virtual_vmt_call:=(po_virtualmethod in procdefinition.procoptions) and
                                not(assigned(methodpointer) and
                                    (methodpointer.nodetype=typen));

              { push self/vmt for methods }
              if assigned(symtableproc) and
                 (symtableproc.symtabletype in [withsymtable,objectsymtable]) then
                vmtreg:=push_self_and_vmt(virtual_vmt_call);

              { push base pointer ?}
              { never when inlining, since if necessary, the base pointer }
              { can/will be gottten from the current procedure's symtable }
              { (JM) }
              if not inlined then
                if (current_procdef.parast.symtablelevel>=normal_function_level) and
                   assigned(tprocdef(procdefinition).parast) and
                   ((tprocdef(procdefinition).parast.symtablelevel)>normal_function_level) then
                  push_framepointer;

              rg.saveintregvars(exprasmlist,regs_to_push_int);
              rg.saveotherregvars(exprasmlist,regs_to_push_other);

              if virtual_vmt_call then
                begin
                   { virtual methods require an index }
                   if tprocdef(procdefinition).extnumber=-1 then
                     internalerror(200304021);
                   { VMT should already be loaded in a register }
                   if vmtreg.number=NR_NO then
                     internalerror(200304022);

                   { test validity of VMT }
                   if not(is_interface(tprocdef(procdefinition)._class)) and
                      not(is_cppclass(tprocdef(procdefinition)._class)) then
                     cg.g_maybe_testvmt(exprasmlist,vmtreg,tprocdef(procdefinition)._class);

                   { call method }
                   reference_reset_base(href,vmtreg,
                      tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber));
                   cg.a_call_ref(exprasmlist,href);

                   { release self }
                   rg.ungetregisterint(exprasmlist,vmtreg);
                end
              else
                begin
                  if not inlined then
                   begin
                     { Calling interrupt from the same code requires some
                       extra code }
                     if (po_interrupt in procdefinition.procoptions) then
                       extra_interrupt_code;

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

              { Calling interrupt from the same code requires some
                extra code }
              if (po_interrupt in procdefinition.procoptions) then
                extra_interrupt_code;

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

         { Need to remove the parameters from the stack? }
         if (not inlined) and (po_clearstack in procdefinition.procoptions) then
          begin
            { the old pop_size was already included in pushedparasize }
            pop_size:=pushedparasize;
          end;

         { Remove parameters/alignment from the stack }
         if pop_size>0 then
           pop_parasize(pop_size);


{$ifdef powerpc}
         { this calculation must be done in pass_1 anyway, so don't worry }
         if tppcprocinfo(current_procinfo).maxpushedparasize<pushedparasize then
           tppcprocinfo(current_procinfo).maxpushedparasize:=pushedparasize;
{$endif powerpc}

         { Restore }
         pushedparasize:=oldpushedparasize;
         rg.restoreunusedstate(unusedstate);
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}

         { Called an inherited constructor? Then
           we need to check the result }
         if (inlined or (right=nil)) and
            (procdefinition.proctypeoption=potype_constructor) and
            assigned(methodpointer) and
            (methodpointer.nodetype=typen) and
            (current_procdef.proctypeoption=potype_constructor) then
          begin
            accreg.enum:=R_INTREGISTER;
            accreg.number:=NR_ACCUMULATOR;
            cg.a_reg_alloc(exprasmlist,accreg);
            cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,accreg,faillabel);
            cg.a_reg_dealloc(exprasmlist,accreg);
          end;

         { handle function results }
         if (not is_void(resulttype.def)) then
          handle_return_value(inlined)
         else
          location_reset(location,LOC_VOID,OS_NO);

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              reference_reset_symbol(href,iolabel,0);
              cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(1));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
           end;

         { restore registers }
         rg.restoreusedotherregisters(exprasmlist,pushed);
         rg.restoreusedintregisters(exprasmlist,pushedint);

         { Release temps from parameters }
         pp:=tcallparanode(left);
         while assigned(pp) do
           begin
              if assigned(pp.left) then
                begin
                  { don't release the funcret temp }
                  if not(vo_is_funcret in tvarsym(pp.paraitem.parasym).varoptions) then
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
              pp:=tcallparanode(pp.right);
           end;

         if inlined then
           begin
             if (resulttype.def.size>0) then
               tg.UnGetTemp(exprasmlist,returnref);
             tprocdef(procdefinition).parast.address_fixup:=store_parast_fixup;
             right:=inlinecode;

             { from now on the result can be freed normally }
             if paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
               tg.ChangeTempType(exprasmlist,funcretnode.location.reference,tt_normal);
           end;

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
{$ifdef x86}
                  { release FPU stack }
                  accreg.enum:=FPU_RESULT_REG;
                  emit_reg(A_FSTP,S_NO,accreg);
                  {
                    dec(trgcpu(rg).fpuvaroffset);
                    do NOT decrement as the increment before
                    is not called for unused results PM }
{$endif x86}
                end;
           end;
      end;



{*****************************************************************************
                             TCGPROCINLINENODE
*****************************************************************************}


    procedure tcgprocinlinenode.pass_2;
       var st : tsymtable;
           oldprocdef : tprocdef;
           ps, i : longint;
           oldprocinfo : tprocinfo;
           oldinlining_procedure,
           nostackframe,make_global : boolean;
           inlineentrycode,inlineexitcode : TAAsmoutput;
           oldexitlabel,oldexit2label,oldquickexitlabel:tasmlabel;
           oldregstate: pointer;
           localsref : treference;
{$ifdef GDB}
           startlabel,endlabel : tasmlabel;
           pp : pchar;
           mangled_length  : longint;
{$endif GDB}
       begin
          { deallocate the registers used for the current procedure's regvars }
          if assigned(current_procdef.regvarinfo) then
            begin
              with pregvarinfo(current_procdef.regvarinfo)^ do
                for i := 1 to maxvarregs do
                  if assigned(regvars[i]) then
                    store_regvar(exprasmlist,regvars[i].reg);
              rg.saveStateForInline(oldregstate);
              { make sure the register allocator knows what the regvars in the }
              { inlined code block are (JM)                                    }
              rg.resetusableregisters;
              rg.clearregistercount;
            {$ifndef newra}
              rg.cleartempgen;
            {$endif}
              if assigned(inlineprocdef.regvarinfo) then
                with pregvarinfo(inlineprocdef.regvarinfo)^ do
                  for i := 1 to maxvarregs do
                    if assigned(regvars[i]) then
                      begin
                        {Fix me!!}
                        {tmpreg:=rg.makeregsize(regvars[i].reg,OS_INT);
                        rg.makeregvar(tmpreg);}
                        internalerror(200301232);
                      end;
            end;
          oldinlining_procedure:=inlining_procedure;
          oldexitlabel:=aktexitlabel;
          oldexit2label:=aktexit2label;
          oldquickexitlabel:=quickexitlabel;
          oldprocdef:=current_procdef;
          oldprocinfo:=current_procinfo;
          objectlibrary.getlabel(aktexitlabel);
          objectlibrary.getlabel(aktexit2label);
          { we're inlining a procedure }
          inlining_procedure:=true;
          current_procdef:=inlineprocdef;

          { clone procinfo, but not the asmlists }
          current_procinfo:=tprocinfo(cprocinfo.newinstance);
          move(pointer(oldprocinfo)^,pointer(current_procinfo)^,cprocinfo.InstanceSize);
          current_procinfo.aktentrycode:=nil;
          current_procinfo.aktexitcode:=nil;
          current_procinfo.aktproccode:=nil;
          current_procinfo.aktlocaldata:=nil;

          { set new procinfo }
          current_procinfo.return_offset:=retoffset;

          { arg space has been filled by the parent secondcall }
          st:=current_procdef.localst;
          { set it to the same lexical level }
          st.symtablelevel:=oldprocdef.localst.symtablelevel;
          if st.datasize>0 then
            begin
              tg.GetTemp(exprasmlist,st.datasize,tt_persistant,localsref);
              st.address_fixup:=localsref.offset+st.datasize;
{$ifdef extdebug}
              Comment(V_debug,'local symtable is at offset '+tostr(st.address_fixup));
              exprasmList.concat(tai_comment.Create(strpnew(
                'local symtable is at offset '+tostr(st.address_fixup))));
{$endif extdebug}
            end;
          exprasmList.concat(Tai_Marker.Create(InlineStart));
{$ifdef extdebug}
          exprasmList.concat(tai_comment.Create(strpnew('Start of inlined proc')));
{$endif extdebug}
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
            begin
              objectlibrary.getaddrlabel(startlabel);
              objectlibrary.getaddrlabel(endlabel);
              cg.a_label(exprasmlist,startlabel);
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
          if po_assembler in current_procdef.procoptions then
            inlineentrycode.insert(Tai_marker.Create(asmblockstart));
          exprasmList.concatlist(inlineentrycode);
          secondpass(inlinetree);
          genexitcode(inlineexitcode,0,false,true);
          if po_assembler in current_procdef.procoptions then
            inlineexitcode.concat(Tai_marker.Create(asmblockend));
          exprasmList.concatlist(inlineexitcode);

          inlineentrycode.free;
          inlineexitcode.free;
{$ifdef extdebug}
          exprasmList.concat(tai_comment.Create(strpnew('End of inlined proc')));
{$endif extdebug}
          exprasmList.concat(Tai_Marker.Create(InlineEnd));

          {we can free the local data now, reset also the fixup address }
          if st.datasize>0 then
            begin
              tg.UnGetTemp(exprasmlist,localsref);
              st.address_fixup:=0;
            end;
          { restore procinfo }
          current_procinfo.free;
          current_procinfo:=oldprocinfo;
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) then
            begin
              cg.a_label(exprasmlist,endlabel);
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
          current_procdef:=oldprocdef;
          aktexitlabel:=oldexitlabel;
          aktexit2label:=oldexit2label;
          quickexitlabel:=oldquickexitlabel;
          inlining_procedure:=oldinlining_procedure;

          { reallocate the registers used for the current procedure's regvars, }
          { since they may have been used and then deallocated in the inlined  }
          { procedure (JM)                                                     }
          if assigned(current_procdef.regvarinfo) then
            begin
              rg.restoreStateAfterInline(oldregstate);
            end;
       end;


begin
   ccallparanode:=tcgcallparanode;
   ccallnode:=tcgcallnode;
   cprocinlinenode:=tcgprocinlinenode;
end.
{
  $Log$
  Revision 1.57  2003-04-30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.56  2003/04/29 07:28:52  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.55  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.54  2003/04/27 07:29:50  peter
    * current_procdef cleanup, current_procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.53  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.52  2003/04/25 08:25:26  daniel
    * Ifdefs around a lot of calls to cleartempgen
    * Fixed registers that are allocated but not freed in several nodes
    * Tweak to register allocator to cause less spills
    * 8-bit registers now interfere with esi,edi and ebp
      Compiler can now compile rtl successfully when using new register
      allocator

  Revision 1.51  2003/04/22 23:50:22  peter
    * firstpass uses expectloc
    * checks if there are differences between the expectloc and
      location.loc from secondpass in EXTDEBUG

  Revision 1.50  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.49  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.48  2003/04/22 10:09:34  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.47  2003/04/22 09:49:44  peter
    * do not load self when calling a non-inherited class constructor

  Revision 1.46  2003/04/21 20:03:32  peter
    * forgot to copy vmtrefaddr to selfrefaddr when self=vmt

  Revision 1.45  2003/04/21 13:53:16  jonas
    - removed copying of all paras when secondpassing a callnode (this used
      to be necessary for inlinign support, but currently the whole inlined
      procedure is already copied in advance). Note that the compiler crashes
      when compiling ucomplex with -dTEST_INLINE (also after fixing the
      syntax errors), but that was also the case before this change.

  Revision 1.44  2003/04/10 17:57:52  peter
    * vs_hidden released

  Revision 1.43  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.42  2003/04/04 15:38:56  peter
    * moved generic code from n386cal to ncgcal, i386 now also
      uses the generic ncgcal

  Revision 1.41  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.40  2003/03/06 11:35:50  daniel
    * Fixed internalerror 7843 issue

  Revision 1.39  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.38  2003/02/15 22:17:38  carl
   * bugfix of FPU emulation code

  Revision 1.37  2003/02/12 22:10:07  carl
    * load_frame_pointer is now generic
    * change fpu emulation routine names

  Revision 1.36  2003/01/30 21:46:57  peter
    * self fixes for static methods (merged)

  Revision 1.35  2003/01/22 20:45:15  mazen
  * making math code in RTL compiling.
  *NB : This does NOT mean necessary that it will generate correct code!

  Revision 1.34  2003/01/17 12:03:45  daniel
    * Optalign conditional code adapted to record Tregister

  Revision 1.33  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.32  2002/12/15 22:50:00  florian
    + some stuff for the new hidden parameter handling added

  Revision 1.31  2002/12/15 21:30:12  florian
    * tcallnode.paraitem introduced, all references to defcoll removed

  Revision 1.30  2002/11/27 20:04:39  peter
    * cdecl array of const fixes

  Revision 1.29  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.28  2002/11/18 17:31:54  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.27  2002/11/16 15:34:30  florian
    * generic location for float results

  Revision 1.26  2002/11/15 01:58:51  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.25  2002/10/05 12:43:25  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.24  2002/09/30 07:00:45  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.23  2002/09/17 18:54:02  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.22  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.21  2002/09/07 11:50:02  jonas
    * fixed small regalloction info bug

  Revision 1.20  2002/09/02 11:25:20  florian
    * fixed generic procedure variable calling

  Revision 1.19  2002/09/01 21:04:48  florian
    * several powerpc related stuff fixed

  Revision 1.18  2002/09/01 18:43:27  peter
    * include accumulator in regs_to_push list

  Revision 1.17  2002/09/01 12:13:00  peter
    * use a_call_reg
    * ungetiftemp for procvar of object temp

  Revision 1.16  2002/08/25 19:25:18  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.15  2002/08/23 16:14:48  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.14  2002/08/20 16:55:38  peter
    * don't write (stabs)line info when inlining a procedure

  Revision 1.13  2002/08/19 19:36:42  peter
    * More fixes for cross unit inlining, all tnodes are now implemented
    * Moved pocall_internconst to po_internconst because it is not a
      calling type at all and it conflicted when inlining of these small
      functions was requested

  Revision 1.12  2002/08/18 20:06:23  peter
    * inlining is now also allowed in interface
    * renamed write/load to ppuwrite/ppuload
    * tnode storing in ppu
    * nld,ncon,nbas are already updated for storing in ppu

  Revision 1.11  2002/08/17 22:09:44  florian
    * result type handling in tcgcal.pass_2 overhauled
    * better tnode.dowrite
    * some ppc stuff fixed

  Revision 1.10  2002/08/17 09:23:35  florian
    * first part of procinfo rewrite

  Revision 1.9  2002/08/13 21:40:55  florian
    * more fixes for ppc calling conventions

  Revision 1.8  2002/08/13 18:01:51  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.7  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.6  2002/08/11 14:32:26  peter
    * renamed current_library to objectlibrary

  Revision 1.5  2002/08/11 13:24:11  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.4  2002/08/06 20:55:20  florian
    * first part of ppc calling conventions fix

  Revision 1.3  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.2  2002/07/13 19:38:43  florian
    * some more generic calling stuff fixed

}
