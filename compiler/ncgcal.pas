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
unit ncgcal;

{$i fpcdefs.inc}

interface

    uses
      cpubase,
      globtype,
      symdef,node,ncal;

    type
       tcgcallparanode = class(tcallparanode)
       private
          tempparaloc : tparalocation;
       public
          procedure secondcallparan(push_from_left_to_right:boolean;calloption:tproccalloption;
                para_alignment,para_offset : longint);override;
       end;

       tcgcallnode = class(tcallnode)
       private
          procedure release_para_temps;
          procedure normal_pass_2;
          procedure inlined_pass_2;
       protected
          refcountedtemp : treference;
          procedure handle_return_value;
          {# This routine is used to push the current frame pointer
             on the stack. This is used in nested routines where the
             value of the frame pointer is always pushed as an extra
             parameter.

             The default handling is the standard handling used on
             most stack based machines, where the frame pointer is
             the first invisible parameter.
          }
          function  align_parasize:longint;virtual;
          procedure pop_parasize(pop_size:longint);virtual;
          procedure push_framepointer;virtual;
          procedure free_pushed_framepointer;virtual;
          procedure extra_interrupt_code;virtual;
       public
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
      cpuinfo,aasmbase,aasmtai,aasmcpu,
      nbas,nmem,nld,ncnv,
{$ifdef x86}
      cga,
{$endif x86}
{$ifdef cpu64bit}
      cg64f64,
{$else cpu64bit}
      cg64f32,
{$endif cpu64bit}
      ncgutil,cgobj,tgobj,regvars,rgobj,rgcpu;


{*****************************************************************************
                             TCGCALLPARANODE
*****************************************************************************}

    procedure tcgcallparanode.secondcallparan(push_from_left_to_right:boolean;calloption:tproccalloption;para_alignment,para_offset : longint);
      var
         otlabel,
         oflabel : tasmlabel;
         tmpreg  : tregister;
         href    : treference;
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
            tcallparanode(right).secondcallparan(push_from_left_to_right,
                                                 calloption,para_alignment,para_offset);
          end;

         otlabel:=truelabel;
         oflabel:=falselabel;
         objectlibrary.getlabel(truelabel);
         objectlibrary.getlabel(falselabel);
         secondpass(left);

         { Allocate (temporary) paralocation }
         tempparaloc:=paraitem.paraloc[callerside];
         if tempparaloc.loc=LOC_REGISTER then
           paramanager.alloctempregs(exprasmlist,tempparaloc)
         else
           paramanager.allocparaloc(exprasmlist,tempparaloc);

         { handle varargs first, because defcoll is not valid }
         if (nf_varargs_para in flags) then
           begin
             if paramanager.push_addr_param(left.resulttype.def,calloption) then
               begin
                 inc(pushedparasize,POINTER_SIZE);
                 cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempparaloc);
                 location_release(exprasmlist,left.location);
               end
             else
               push_value_para(exprasmlist,left,calloption,para_offset,para_alignment,tempparaloc);
           end
         { hidden parameters }
         else if paraitem.is_hidden then
           begin
             { don't push a node that already generated a pointer type
               by address for implicit hidden parameters }
             if (vo_is_funcret in tvarsym(paraitem.parasym).varoptions) or
                (not(left.resulttype.def.deftype in [pointerdef,classrefdef]) and
                 paramanager.push_addr_param(paraitem.paratype.def,calloption)) then
               begin
                  if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                    internalerror(200305071);

                  inc(pushedparasize,POINTER_SIZE);
                  if calloption=pocall_inline then
                    begin
                       tmpreg:=rg.getaddressregister(exprasmlist);
                       cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                       reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                       cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,tmpreg,href);
                       rg.ungetregisterint(exprasmlist,tmpreg);
                    end
                  else
                    cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempparaloc);
                  location_release(exprasmlist,left.location);
               end
             else
               begin
                  push_value_para(exprasmlist,left,calloption,
                    para_offset,para_alignment,tempparaloc);
               end;
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
              if (tvarsym(paraitem.parasym).varspez=vs_const) and
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
                       cg.a_load_loc_ref(exprasmlist,OS_ADDR,left.location,href);
                    end
                  else
                    cg.a_param_loc(exprasmlist,left.location,tempparaloc);
                  location_release(exprasmlist,left.location);
                end
              else
                begin
                   if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                     internalerror(200304235);

                   if calloption=pocall_inline then
                     begin
                       tmpreg:=rg.getaddressregister(exprasmlist);
                       cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                       reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                       cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,tmpreg,href);
                       rg.ungetregisterint(exprasmlist,tmpreg);
                     end
                   else
                     cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempparaloc);
                   location_release(exprasmlist,left.location);
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
                        is_self_node(left)) then
                  internalerror(200106041);
               end;
              if (paraitem.paratyp=vs_out) and
                 assigned(paraitem.paratype.def) and
                 not is_class(paraitem.paratype.def) and
                 paraitem.paratype.def.needs_inittable then
                cg.g_finalize(exprasmlist,paraitem.paratype.def,left.location.reference,false);
              inc(pushedparasize,POINTER_SIZE);
              if calloption=pocall_inline then
                begin
                   tmpreg:=rg.getaddressregister(exprasmlist);
                   cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                   reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                   cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,tmpreg,href);
                   rg.ungetregisterint(exprasmlist,tmpreg);
                end
              else
                cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempparaloc);
              location_release(exprasmlist,left.location);
           end
         else
           begin
              { don't push a node that already generated a pointer type
                by address for implicit hidden parameters }
              if (not(
                      paraitem.is_hidden and
                      (left.resulttype.def.deftype in [pointerdef,classrefdef])
                     ) and
                  paramanager.push_addr_param(paraitem.paratype.def,calloption)) then
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
                           cg.a_load_loc_ref(exprasmlist,left.location.size,left.location,href)
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
                        tmpreg:=rg.getaddressregister(exprasmlist);
                        cg.a_loadaddr_ref_reg(exprasmlist,left.location.reference,tmpreg);
                        reference_reset_base(href,current_procinfo.framepointer,para_offset-pushedparasize);
                        cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,tmpreg,href);
                        rg.ungetregisterint(exprasmlist,tmpreg);
                     end
                   else
                     cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempparaloc);
                   location_release(exprasmlist,left.location);
                end
              else
                begin
                   push_value_para(exprasmlist,left,calloption,
                     para_offset,para_alignment,tempparaloc);
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


    function tcgcallnode.align_parasize:longint;
      begin
        result:=0;
      end;


    procedure tcgcallnode.pop_parasize(pop_size:longint);
      begin
      end;


    procedure tcgcallnode.push_framepointer;
      var
        href : treference;
        hregister : tregister;
      begin
        { this routine is itself not nested }
        if current_procinfo.procdef.parast.symtablelevel=(tprocdef(procdefinition).parast.symtablelevel) then
          begin
            reference_reset_base(href,current_procinfo.framepointer,current_procinfo.parent_framepointer_offset);
            cg.a_param_ref(exprasmlist,OS_ADDR,href,paramanager.getintparaloc(exprasmlist,1));
          end
        { one nesting level }
        else if (current_procinfo.procdef.parast.symtablelevel=(tprocdef(procdefinition).parast.symtablelevel)-1) then
          begin
            cg.a_param_reg(exprasmlist,OS_ADDR,current_procinfo.framepointer,paramanager.getintparaloc(exprasmlist,1));
          end
        { very complex nesting level ... }
       else if (current_procinfo.procdef.parast.symtablelevel>(tprocdef(procdefinition).parast.symtablelevel)) then
          begin
            hregister:=rg.getaddressregister(exprasmlist);
            cg.g_load_parent_framepointer(exprasmlist,tprocdef(procdefinition).parast,hregister);
            cg.a_param_reg(exprasmlist,OS_ADDR,hregister,paramanager.getintparaloc(exprasmlist,1));
            rg.ungetaddressregister(exprasmlist,hregister);
          end;
      end;


    procedure tcgcallnode.free_pushed_framepointer;
      begin
        paramanager.freeintparaloc(exprasmlist,1);
      end;


    procedure tcgcallnode.handle_return_value;
      var
        cgsize : tcgsize;
        hregister : tregister;
        tempnode: tnode;
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
            { the FUNCTION_RESULT_REG is already allocated }
            rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT_REG);
            if not assigned(funcretnode) then
              begin
                location_reset(location,LOC_CREFERENCE,OS_ADDR);
                location.reference:=refcountedtemp;
                cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,NR_FUNCTION_RESULT_REG,location.reference);
              end
            else
              begin
                hregister := rg.getaddressregister(exprasmlist);
                cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,NR_FUNCTION_RESULT_REG,hregister);
                { in case of a regular funcretnode with ret_in_param, the }
                { original funcretnode isn't touched -> make sure it's    }
                { the same here (not sure if it's necessary)              }
                tempnode := funcretnode.getcopy;
                tempnode.pass_2;
                location := tempnode.location;
                tempnode.free;
                cg.g_decrrefcount(exprasmlist,resulttype.def,location.reference, false);
                cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
                rg.ungetregisterint(exprasmlist,hregister);
             end;
          end
        else
        { we have only to handle the result if it is used }
         if (nf_return_value_used in flags) then
          begin
            if (resulttype.def.deftype=floatdef) then
              begin
                location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
{$ifdef cpufpemu}
                if cs_fp_emulation in aktmoduleswitches then
                  location.register:=NR_FUNCTION_RESULT_REG
                else
{$endif cpufpemu}
                  location.register:=NR_FPU_RESULT_REG;
{$ifdef x86}
                inc(trgcpu(rg).fpuvaroffset);
{$else x86}
                hregister := rg.getregisterfpu(exprasmlist,location.size);
                cg.a_loadfpu_reg_reg(exprasmlist,location.size,location.register,hregister);
                location.register := hregister;
{$endif x86}
              end
            else
              begin
                cgsize:=def_cgsize(resulttype.def);

                { an object constructor is a function with pointer result }
                if (procdefinition.proctypeoption=potype_constructor) then
                  cgsize:=OS_ADDR;

                if cgsize<>OS_NO then
                 begin
                   location_reset(location,LOC_REGISTER,cgsize);
{$ifndef cpu64bit}
                   if cgsize in [OS_64,OS_S64] then
                    begin
                      { Move the function result to free registers, preferably the
                        FUNCTION_RESULT_REG/FUNCTION_RESULTHIGH_REG, so no move is necessary.}
                      { the FUNCTION_RESULT_LOW_REG/FUNCTION_RESULT_HIGH_REG
                        are already allocated }
                      rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT64_LOW_REG);
                      location.registerlow:=rg.getregisterint(exprasmlist,OS_INT);
                      cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,NR_FUNCTION_RESULT64_LOW_REG,location.registerlow);
                      rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT64_HIGH_REG);
                      location.registerhigh:=rg.getregisterint(exprasmlist,OS_INT);
                      cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,NR_FUNCTION_RESULT64_HIGH_REG,location.registerhigh);
                    end
                   else
{$endif cpu64bit}
                    begin
                      {Move the function result to a free register, preferably the
                       FUNCTION_RESULT_REG, so no move is necessary.}
                      { the FUNCTION_RESULT_REG is already allocated }
                      rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT_REG);
                      { change register size after the unget because the
                      getregister was done for the full register }
                      location.register:=rg.getregisterint(exprasmlist,cgsize);
                      cg.a_load_reg_reg(exprasmlist,cgsize,cgsize,rg.makeregsize(NR_FUNCTION_RESULT_REG,cgsize),location.register);
                    end;
                 end
                else
                 begin
                   if resulttype.def.size>0 then
                     internalerror(200305131);
                 end;
              end;
          end
        else
          begin
            cgsize:=def_cgsize(resulttype.def);

            { an object constructor is a function with pointer result }
            if (procdefinition.proctypeoption=potype_constructor) then
              cgsize:=OS_ADDR;

            if cgsize<>OS_NO then
{$ifndef cpu64bit}
              if cgsize in [OS_64,OS_S64] then
                begin
                  rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT64_LOW_REG);
                  rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT64_HIGH_REG);
                end
              else
{$endif cpu64bit}
                rg.ungetregisterint(exprasmlist,NR_FUNCTION_RESULT_REG);
            location_reset(location,LOC_VOID,OS_NO);
          end;
      end;


    procedure tcgcallnode.release_para_temps;
      var
        hp  : tnode;
        ppn : tcallparanode;
      begin
        { Release temps from parameters }
        ppn:=tcallparanode(left);
        while assigned(ppn) do
          begin
             if assigned(ppn.left) then
               begin
                 { don't release the funcret temp }
                 if not(vo_is_funcret in tvarsym(ppn.paraitem.parasym).varoptions) then
                   location_freetemp(exprasmlist,ppn.left.location);
                 { process also all nodes of an array of const }
                 if ppn.left.nodetype=arrayconstructorn then
                   begin
                     if assigned(tarrayconstructornode(ppn.left).left) then
                      begin
                        hp:=ppn.left;
                        while assigned(hp) do
                         begin
                           location_freetemp(exprasmlist,tarrayconstructornode(hp).left.location);
                           hp:=tarrayconstructornode(hp).right;
                         end;
                      end;
                   end;
               end;
             ppn:=tcallparanode(ppn.right);
          end;
      end;


    procedure tcgcallnode.normal_pass_2;
      var
         regs_to_push_other : totherregisterset;
         unusedstate: pointer;
         regs_to_alloc,regs_to_free:Tsuperregisterset;
         pushedother : tpushedsavedother;
         oldpushedparasize : longint;
         { adress returned from an I/O-error }
         iolabel : tasmlabel;
         { help reference pointer }
         href,helpref : treference;
         para_alignment,
         pop_size : longint;
         vmtreg,vmtreg2 : tregister;
         oldaktcallnode : tcallnode;

         procedure pushparas;
         var
           ppn : tcgcallparanode;
         begin
           { copy all resources to the allocated registers }
           ppn:=tcgcallparanode(left);
           while assigned(ppn) do
             begin
               if ppn.tempparaloc.loc=LOC_REGISTER then
                 begin
                   paramanager.allocparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
{$ifdef sparc}
                   case ppn.tempparaloc.size of
                     OS_F32 :
                       ppn.tempparaloc.size:=OS_32;
                     OS_F64 :
                       ppn.tempparaloc.size:=OS_64;
                   end;
{$endif sparc}
{$ifndef cpu64bit}
                   if ppn.tempparaloc.size in [OS_64,OS_S64] then
                     begin
                       cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,ppn.tempparaloc.registerlow,
                          ppn.paraitem.paraloc[callerside].registerlow);
                       cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,ppn.tempparaloc.registerhigh,
                          ppn.paraitem.paraloc[callerside].registerhigh);
                     end
                   else
{$endif cpu64bit}
                     cg.a_load_reg_reg(exprasmlist,ppn.tempparaloc.size,ppn.tempparaloc.size,
                         ppn.tempparaloc.register,ppn.paraitem.paraloc[callerside].register);
                 end;
               ppn:=tcgcallparanode(ppn.right);
             end;
         end;

         procedure freeparas;
         var
           ppn : tcgcallparanode;
         begin
           { free the resources allocated for the parameters }
           ppn:=tcgcallparanode(left);
           while assigned(ppn) do
             begin
               if ppn.tempparaloc.loc=LOC_REGISTER then
                 paramanager.freeparaloc(exprasmlist,ppn.tempparaloc);
               paramanager.freeparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
               ppn:=tcgcallparanode(ppn.right);
             end;
           { free pushed base pointer }
           if (right=nil) and
              (current_procinfo.procdef.parast.symtablelevel>=normal_function_level) and
              assigned(tprocdef(procdefinition).parast) and
              ((tprocdef(procdefinition).parast.symtablelevel)>normal_function_level) then
             free_pushed_framepointer;
         end;

      begin
         if not assigned(procdefinition) then
           internalerror(200305264);

         { calculate the parameter info for the procdef }
         if not procdefinition.has_paraloc_info then
           begin
             paramanager.create_paraloc_info(procdefinition,callerside);
             procdefinition.has_paraloc_info:=true;
           end;

         iolabel:=nil;
         rg.saveunusedstate(unusedstate);

         if not assigned(funcretnode) then
           begin
             { if we allocate the temp. location for ansi- or widestrings }
             { already here, we avoid later a push/pop                    }
             if is_widestring(resulttype.def) then
               begin
                 tg.gettemp(exprasmlist,pointer_size,tt_widestring,refcountedtemp);
                 cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp,false);
               end
             else if is_ansistring(resulttype.def) then
               begin
                 tg.GetTemp(exprasmlist,pointer_size,tt_ansistring,refcountedtemp);
                 cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp,false);
               end;
           end;


         if (procdefinition.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_stdcall]) then
          para_alignment:=4
         else
          para_alignment:=aktalignment.paraalign;

         { proc variables destroy all registers }
         if (right=nil) and
            { virtual methods too }
            not(po_virtualmethod in procdefinition.procoptions) then
           begin
              if (cs_check_io in aktlocalswitches) and
                 (po_iocheck in procdefinition.procoptions) and
                 not(po_iocheck in current_procinfo.procdef.procoptions) then
                begin
                   objectlibrary.getaddrlabel(iolabel);
                   cg.a_label(exprasmlist,iolabel);
                end
              else
                iolabel:=nil;

(*
              regs_to_alloc:=Tprocdef(procdefinition).usedintregisters;

{$ifdef i386}
              regs_to_push_other := tprocdef(procdefinition).usedotherregisters;
{$else i386}
              regs_to_push_other := VOLATILE_FPUREGISTERS;
{$endif i386}
              { on the ppc, ever procedure saves the non-volatile registers it uses itself }
              { and must make sure it saves its volatile registers before doing a call     }
{$ifdef i386}
              { give used registers through }
              rg.used_in_proc_int:=rg.used_in_proc_int + tprocdef(procdefinition).usedintregisters;
              rg.used_in_proc_other:=rg.used_in_proc_other + tprocdef(procdefinition).usedotherregisters;
{$endif i386}
*)
           end
         else
           begin
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

        regs_to_alloc:=paramanager.get_volatile_registers_int(procdefinition.proccalloption);
        regs_to_push_other:=paramanager.get_volatile_registers_fpu(procdefinition.proccalloption);

        { Include Function result registers }
        if (not is_void(resulttype.def)) then
          begin
            case procdefinition.funcret_paraloc[callerside].loc of
              LOC_REGISTER,LOC_CREGISTER:
                begin
{$ifndef cpu64bit}
                  if procdefinition.funcret_paraloc[callerside].size in [OS_S64,OS_64] then
                    begin
                      include(regs_to_alloc,getsupreg(procdefinition.funcret_paraloc[callerside].registerlow));
                      include(regs_to_alloc,getsupreg(procdefinition.funcret_paraloc[callerside].registerhigh));
                    end
                 else
{$endif cpu64bit}
                   include(regs_to_alloc,getsupreg(procdefinition.funcret_paraloc[callerside].register));
                end;
            end;
          end;

         { Save registers destroyed by the call }
         rg.saveusedotherregisters(exprasmlist,pushedother,regs_to_push_other);

         { Initialize for pushing the parameters }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;

         { Align stack if required }
         pop_size:=align_parasize;

         { Push parameters }
         oldaktcallnode:=aktcallnode;
         aktcallnode:=self;

{$ifndef i386}
         { process procvar. Done here already, because otherwise it may }
         { destroy registers containing a parameter for the actual      }
         { function call (e.g. if it's a function, its result will      }
         { overwrite r3, which contains the first parameter) (JM)       }
         if assigned(right) then
           secondpass(right);

         if (po_virtualmethod in procdefinition.procoptions) and
            assigned(methodpointer) then
           begin
             secondpass(methodpointer);
             location_force_reg(exprasmlist,methodpointer.location,OS_ADDR,false);

             { virtual methods require an index }
             if tprocdef(procdefinition).extnumber=-1 then
               internalerror(200304021);
             { VMT should already be loaded in a register }
             if methodpointer.location.register=NR_NO then
               internalerror(200304022);

             { test validity of VMT }
             if not(is_interface(tprocdef(procdefinition)._class)) and
                not(is_cppclass(tprocdef(procdefinition)._class)) then
               cg.g_maybe_testvmt(exprasmlist,methodpointer.location.register,tprocdef(procdefinition)._class);
           end;
{$endif not i386}

         { Process parameters }
         if assigned(left) then
           begin
              tcallparanode(left).secondcallparan(
                (procdefinition.proccalloption in pushleftright_pocalls),procdefinition.proccalloption,
                 para_alignment,0);

             pushparas;
           end;
         aktcallnode:=oldaktcallnode;

         { procedure variable or normal function call ? }
         if (right=nil) then
           begin
              { push base pointer ?}
              if (current_procinfo.procdef.parast.symtablelevel>=normal_function_level) and
                 assigned(tprocdef(procdefinition).parast) and
                 ((tprocdef(procdefinition).parast.symtablelevel)>normal_function_level) then
                push_framepointer;

              rg.saveotherregvars(exprasmlist,regs_to_push_other);

              if (po_virtualmethod in procdefinition.procoptions) and
                 assigned(methodpointer) then
                begin
{$ifdef i386}
                   secondpass(methodpointer);
                   location_force_reg(exprasmlist,methodpointer.location,OS_ADDR,false);
                   vmtreg:=methodpointer.location.register;

                   { virtual methods require an index }
                   if tprocdef(procdefinition).extnumber=-1 then
                     internalerror(200304021);
                   { VMT should already be loaded in a register }
                   if vmtreg=NR_NO then
                     internalerror(200304022);

                   { test validity of VMT }
                   if not(is_interface(tprocdef(procdefinition)._class)) and
                      not(is_cppclass(tprocdef(procdefinition)._class)) then
                     cg.g_maybe_testvmt(exprasmlist,vmtreg,tprocdef(procdefinition)._class);
{$else}
                   vmtreg:=methodpointer.location.register;
{$endif}
                   { release self }
                   rg.ungetaddressregister(exprasmlist,vmtreg);
                   vmtreg2:=rg.getabtregisterint(exprasmlist,OS_ADDR);
                   rg.ungetregisterint(exprasmlist,vmtreg2);
                   cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,vmtreg,vmtreg2);
                   { free the resources allocated for the parameters }
                   freeparas;

                   rg.allocexplicitregistersint(exprasmlist,regs_to_alloc);
                   { call method }
                   reference_reset_base(href,vmtreg2,
                      tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber));
                   cg.a_call_ref(exprasmlist,href);
                end
              else
                begin
                  { free the resources allocated for the parameters }
                  freeparas;

                  rg.allocexplicitregistersint(exprasmlist,regs_to_alloc);
                  { Calling interrupt from the same code requires some
                    extra code }
                  if (po_interrupt in procdefinition.procoptions) then
                    extra_interrupt_code;
                  cg.a_call_name(exprasmlist,tprocdef(procdefinition).mangledname);
               end;
           end
         else
           { now procedure variable case }
           begin
{$ifdef i386}
              secondpass(right);
{$endif i386}
              if right.location.loc in  [LOC_REFERENCE,LOC_CREFERENCE] then
                begin
                  helpref:=right.location.reference;
                  if (helpref.index<>NR_NO) and (helpref.index<>NR_FRAME_POINTER_REG) then
                    begin
                      rg.ungetregisterint(exprasmlist,helpref.index);
                      helpref.index:=rg.getabtregisterint(exprasmlist,OS_ADDR);
                      cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,
                                        right.location.reference.index,helpref.index);
                    end;
                  if (helpref.base<>NR_NO) and (helpref.base<>NR_FRAME_POINTER_REG) then
                    begin
                      rg.ungetregisterint(exprasmlist,helpref.base);
                      helpref.base:=rg.getabtregisterint(exprasmlist,OS_ADDR);
                      cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,
                                        right.location.reference.base,helpref.base);
                    end;
                  reference_release(exprasmlist,helpref);
                end
              else
                rg.ungetregisterint(exprasmlist,right.location.register);

              location_freetemp(exprasmlist,right.location);

              { free the resources allocated for the parameters }
              freeparas;

              rg.allocexplicitregistersint(exprasmlist,regs_to_alloc);
              { Calling interrupt from the same code requires some
                extra code }
              if (po_interrupt in procdefinition.procoptions) then
                extra_interrupt_code;

               rg.saveotherregvars(exprasmlist,ALL_OTHERREGISTERS);
               if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                 cg.a_call_ref(exprasmlist,helpref)
               else
                 cg.a_call_reg(exprasmlist,right.location.register);
{               cg.a_call_loc(exprasmlist,right.location);}
               location_release(exprasmlist,right.location);
               location_freetemp(exprasmlist,right.location);
           end;

         { Need to remove the parameters from the stack? }
         if (procdefinition.proccalloption in clearstack_pocalls) then
          begin
            { the old pop_size was already included in pushedparasize }
            pop_size:=pushedparasize;
            { for Cdecl functions we don't need to pop the funcret when it
              was pushed by para }
            if paramanager.ret_in_param(procdefinition.rettype.def,procdefinition.proccalloption) then
              dec(pop_size,POINTER_SIZE);
          end;

         { Remove parameters/alignment from the stack }
         if pop_size>0 then
           pop_parasize(pop_size);

         { Reserve space for storing parameters that will be pushed }
         current_procinfo.allocate_push_parasize(pushedparasize);

         { Restore }
         pushedparasize:=oldpushedparasize;
         rg.restoreunusedstate(unusedstate);
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}

         { Release registers, but not the registers that contain the
           function result }
         regs_to_free:=regs_to_alloc;
         if (not is_void(resulttype.def)) then
           begin
             case procdefinition.funcret_paraloc[callerside].loc of
               LOC_REGISTER,LOC_CREGISTER:
                 begin
{$ifndef cpu64bit}
                   if procdefinition.funcret_paraloc[callerside].size in [OS_S64,OS_64] then
                     begin
                       exclude(regs_to_free,getsupreg(procdefinition.funcret_paraloc[callerside].registerlow));
                       exclude(regs_to_free,getsupreg(procdefinition.funcret_paraloc[callerside].registerhigh));
                     end
                   else
{$endif cpu64bit}
                     exclude(regs_to_free,getsupreg(procdefinition.funcret_paraloc[callerside].register));
                 end;
             end;
           end;
         rg.deallocexplicitregistersint(exprasmlist,regs_to_free);

         { handle function results }
         if (not is_void(resulttype.def)) then
           handle_return_value
         else
           location_reset(location,LOC_VOID,OS_NO);

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              //reference_reset_symbol(href,iolabel,0);
              //cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(exprasmlist,1));
              //paramanager.freeintparaloc(exprasmlist,1);
              rg.allocexplicitregistersint(exprasmlist,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
              rg.deallocexplicitregistersint(exprasmlist,paramanager.get_volatile_registers_int(pocall_default));
           end;

         { restore registers }
         rg.restoreusedotherregisters(exprasmlist,pushedother);

         { release temps of paras }
         release_para_temps;

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
                  emit_reg(A_FSTP,S_NO,NR_FPU_RESULT_REG);
                  {
                    dec(trgcpu(rg).fpuvaroffset);
                    do NOT decrement as the increment before
                    is not called for unused results PM }
{$endif x86}
                end;
           end;
      end;



    procedure tcgcallnode.inlined_pass_2;
      var
         regs_to_push_int : Tsuperregisterset;
         regs_to_push_other : totherregisterset;
         unusedstate: pointer;
         pushedother : tpushedsavedother;
         oldpushedparasize : longint;
         { adress returned from an I/O-error }
         iolabel : tasmlabel;
         oldaktcallnode : tcallnode;
         oldprocdef : tprocdef;
         i : longint;
         oldprocinfo : tprocinfo;
         oldinlining_procedure : boolean;
         inlineentrycode,inlineexitcode : TAAsmoutput;
         oldregstate: pointer;
         old_local_fixup,
         old_para_fixup : longint;
         usesacc,usesacchi,usesfpu : boolean;
         pararef,
         localsref : treference;
{$ifdef GDB}
         startlabel,endlabel : tasmlabel;
         pp : pchar;
         mangled_length  : longint;
{$endif GDB}
      begin
         if not(assigned(procdefinition) and (procdefinition.deftype=procdef)) then
           internalerror(200305262);

         oldinlining_procedure:=inlining_procedure;
         oldprocdef:=current_procinfo.procdef;
         oldprocinfo:=current_procinfo;
         { we're inlining a procedure }
         inlining_procedure:=true;

         { calculate the parameter info for the procdef }
         if not procdefinition.has_paraloc_info then
           begin
             paramanager.create_paraloc_info(procdefinition,callerside);
             procdefinition.has_paraloc_info:=true;
           end;

         { deallocate the registers used for the current procedure's regvars }
         if assigned(current_procinfo.procdef.regvarinfo) then
           begin
             with pregvarinfo(current_procinfo.procdef.regvarinfo)^ do
               for i := 1 to maxvarregs do
                 if assigned(regvars[i]) then
                   store_regvar(exprasmlist,regvars[i].reg);
             rg.saveStateForInline(oldregstate);
             { make sure the register allocator knows what the regvars in the }
             { inlined code block are (JM)                                    }
             rg.resetusableregisters;
             rg.clearregistercount;
             if assigned(tprocdef(procdefinition).regvarinfo) then
               with pregvarinfo(tprocdef(procdefinition).regvarinfo)^ do
                 for i := 1 to maxvarregs do
                   if assigned(regvars[i]) then
                     begin
                       {Fix me!!}
                       {tmpreg:=rg.makeregsize(regvars[i].reg,OS_INT);
                       rg.makeregvar(tmpreg);}
                       internalerror(200301232);
                     end;
           end;

         { create temp procinfo }
         current_procinfo:=cprocinfo.create(nil);
         current_procinfo.procdef:=tprocdef(procdefinition);

         { Localsymtable }
         current_procinfo.procdef.localst.symtablelevel:=oldprocdef.localst.symtablelevel;
         if current_procinfo.procdef.localst.datasize>0 then
           begin
             old_local_fixup:=current_procinfo.procdef.localst.address_fixup;
             tg.GetTemp(exprasmlist,current_procinfo.procdef.localst.datasize,tt_persistent,localsref);
             if tg.direction>0 then
               current_procinfo.procdef.localst.address_fixup:=localsref.offset
             else
               current_procinfo.procdef.localst.address_fixup:=localsref.offset+current_procinfo.procdef.localst.datasize;
{$ifdef extdebug}
             Comment(V_debug,'inlined local symtable ('+tostr(current_procinfo.procdef.localst.datasize)+' bytes) is at offset '+tostr(current_procinfo.procdef.localst.address_fixup));
             exprasmList.concat(tai_comment.Create(strpnew(
               'inlined local symtable ('+tostr(current_procinfo.procdef.localst.datasize)+' bytes) is at offset '+tostr(current_procinfo.procdef.localst.address_fixup))));
{$endif extdebug}
           end;

         { Parasymtable }
         current_procinfo.procdef.parast.symtablelevel:=oldprocdef.localst.symtablelevel;
         if current_procinfo.procdef.parast.datasize>0 then
           begin
             old_para_fixup:=current_procinfo.procdef.parast.address_fixup;
             tg.GetTemp(exprasmlist,current_procinfo.procdef.parast.datasize,tt_persistent,pararef);
             current_procinfo.procdef.parast.address_fixup:=pararef.offset;
{$ifdef extdebug}
             Comment(V_debug,'inlined para symtable ('+tostr(current_procinfo.procdef.parast.datasize)+' bytes) is at offset '+tostr(current_procinfo.procdef.parast.address_fixup));
             exprasmList.concat(tai_comment.Create(strpnew(
               'inlined para symtable ('+tostr(current_procinfo.procdef.parast.datasize)+' bytes) is at offset '+tostr(current_procinfo.procdef.parast.address_fixup))));
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
             tprocdef(procdefinition).localst.symtabletype:=inlinelocalsymtable;
             procdefinition.parast.symtabletype:=inlineparasymtable;

             { Here we must include the para and local symtable info }
             procdefinition.concatstabto(withdebuglist);

             { set it back for safety }
             tprocdef(procdefinition).localst.symtabletype:=localsymtable;
             procdefinition.parast.symtabletype:=parasymtable;

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

         iolabel:=nil;
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

         if (cs_check_io in aktlocalswitches) and
            (po_iocheck in procdefinition.procoptions) and
            not(po_iocheck in current_procinfo.procdef.procoptions) then
           begin
              objectlibrary.getaddrlabel(iolabel);
              cg.a_label(exprasmlist,iolabel);
           end
         else
           iolabel:=nil;

         { save all used registers and possible registers
           used for the return value }
(*
         regs_to_push_int := tprocdef(procdefinition).usedintregisters;
         regs_to_push_other := tprocdef(procdefinition).usedotherregisters;
*)
         regs_to_push_int:=paramanager.get_volatile_registers_int(procdefinition.proccalloption);
         regs_to_push_other:=paramanager.get_volatile_registers_fpu(procdefinition.proccalloption);
         if (not is_void(resulttype.def)) then
           begin
             case procdefinition.funcret_paraloc[callerside].loc of
               LOC_REGISTER,LOC_CREGISTER:
                 begin
{$ifndef cpu64bit}
                   if procdefinition.funcret_paraloc[callerside].size in [OS_S64,OS_64] then
                     begin
                       include(regs_to_push_int,getsupreg(procdefinition.funcret_paraloc[callerside].registerlow));
                       include(regs_to_push_int,getsupreg(procdefinition.funcret_paraloc[callerside].registerhigh));
                     end
                   else
{$endif cpu64bit}
                    include(regs_to_push_int,getsupreg(procdefinition.funcret_paraloc[callerside].register));
                 end;
             end;
           end;

         rg.saveusedotherregisters(exprasmlist,pushedother,regs_to_push_other);

(*
{$ifdef i386}
         { give used registers through }
         rg.used_in_proc_int:=rg.used_in_proc_int + tprocdef(procdefinition).usedintregisters;
         rg.used_in_proc_other:=rg.used_in_proc_other + tprocdef(procdefinition).usedotherregisters;
{$endif i386}
*)

         { Initialize for pushing the parameters }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;

         { Push parameters }
         oldaktcallnode:=aktcallnode;
         aktcallnode:=self;

         if assigned(left) then
           begin
              { we push from right to left, so start with parameters at the end of
                the parameter block }
              tcallparanode(left).secondcallparan(
                  (procdefinition.proccalloption in pushleftright_pocalls),procdefinition.proccalloption,
                  0,procdefinition.parast.address_fixup+procdefinition.parast.datasize);
           end;
         aktcallnode:=oldaktcallnode;

         rg.saveotherregvars(exprasmlist,regs_to_push_other);

         { takes care of local data initialization }
         inlineentrycode:=TAAsmoutput.Create;
         inlineexitcode:=TAAsmoutput.Create;

         gen_initialize_code(inlineentrycode,true);
         if po_assembler in current_procinfo.procdef.procoptions then
           inlineentrycode.insert(Tai_marker.Create(asmblockstart));
         exprasmList.concatlist(inlineentrycode);

         { process the inline code }
         secondpass(inlinecode);

         { Reserve space for storing parameters that will be pushed }
         current_procinfo.allocate_push_parasize(pushedparasize);

         { Restore }
         pushedparasize:=oldpushedparasize;
         rg.restoreunusedstate(unusedstate);
{$ifdef TEMPREGDEBUG}
         testregisters32;
{$endif TEMPREGDEBUG}

         gen_finalize_code(inlineexitcode,true);
         gen_load_return_value(inlineexitcode,usesacc,usesacchi,usesfpu);
         if po_assembler in current_procinfo.procdef.procoptions then
           inlineexitcode.concat(Tai_marker.Create(asmblockend));
         exprasmList.concatlist(inlineexitcode);

         inlineentrycode.free;
         inlineexitcode.free;
{$ifdef extdebug}
         exprasmList.concat(tai_comment.Create(strpnew('End of inlined proc')));
{$endif extdebug}
         exprasmList.concat(Tai_Marker.Create(InlineEnd));

         {we can free the local data now, reset also the fixup address }
         if current_procinfo.procdef.localst.datasize>0 then
           begin
             tg.UnGetTemp(exprasmlist,localsref);
             current_procinfo.procdef.localst.address_fixup:=old_local_fixup;
           end;
         {we can free the para data now, reset also the fixup address }
         if current_procinfo.procdef.parast.datasize>0 then
           begin
             tg.UnGetTemp(exprasmlist,pararef);
             current_procinfo.procdef.parast.address_fixup:=old_para_fixup;
           end;
         { free return reference }
         if (resulttype.def.size>0) then
           begin
             { from now on the result can be freed normally }
//              if assigned(funcretnode) and
//                 paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
//                tg.ChangeTempType(exprasmlist,funcretnode.location.reference,tt_normal);
           end;

         { handle function results }
         if (not is_void(resulttype.def)) then
           handle_return_value
         else
           location_reset(location,LOC_VOID,OS_NO);

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              //reference_reset_symbol(href,iolabel,0);
              //cg.a_paramaddr_ref(exprasmlist,href,paramanager.getintparaloc(exprasmlist,1));
              //paramanager.freeintparaloc(exprasmlist,1);
              rg.allocexplicitregistersint(exprasmlist,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
              rg.deallocexplicitregistersint(exprasmlist,paramanager.get_volatile_registers_int(pocall_default));
           end;

         { restore registers }
         rg.restoreusedotherregisters(exprasmlist,pushedother);

         { release temps of paras }
         release_para_temps;

         { if return value is not used }
         if (not is_void(resulttype.def)) and
            (not(nf_return_value_used in flags)) then
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
                  emit_reg(A_FSTP,S_NO,NR_FPU_RESULT_REG);
                  {
                    dec(trgcpu(rg).fpuvaroffset);
                    do NOT decrement as the increment before
                    is not called for unused results PM }
{$endif x86}
                end;
           end;

         { release procinfo }
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
         current_procinfo.procdef:=oldprocdef;
         inlining_procedure:=oldinlining_procedure;

         { reallocate the registers used for the current procedure's regvars, }
         { since they may have been used and then deallocated in the inlined  }
         { procedure (JM)                                                     }
         if assigned(current_procinfo.procdef.regvarinfo) then
           rg.restoreStateAfterInline(oldregstate);
      end;


    procedure tcgcallnode.pass_2;
      begin
        if assigned(inlinecode) then
          inlined_pass_2
        else
          normal_pass_2;
      end;


begin
   ccallparanode:=tcgcallparanode;
   ccallnode:=tcgcallnode;
end.
{
  $Log$
  Revision 1.111  2003-09-07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.110  2003/09/04 15:39:58  peter
    * released useparatemp

  Revision 1.109  2003/09/03 15:55:00  peter
    * NEWRA branch merged

  Revision 1.108.2.4  2003/09/01 21:02:55  peter
    * sparc updates for new tregister

  Revision 1.108.2.3  2003/08/31 21:07:44  daniel
    * callparatemp ripped

  Revision 1.108.2.2  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.108.2.1  2003/08/27 20:23:55  peter
    * remove old ra code

  Revision 1.108  2003/08/21 22:14:16  olle
    - removed parameter from fpc_iocheck

  Revision 1.107  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.106  2003/08/16 18:56:40  marco
   * fix from Jonas.

  Revision 1.105  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.104  2003/08/11 14:22:06  mazen
  - dupplicated code removed

  Revision 1.103  2003/07/23 11:01:14  jonas
    * several rg.allocexplicitregistersint/rg.deallocexplicitregistersint
      pairs round calls to helpers

  Revision 1.102  2003/07/21 13:51:50  jonas
    * fixed 64bit int results with -dnewra (you can't free both registers and
      then allocate two new ones, because then the registers could be reversed
      afterwards -> you get something like "movl %eax, %edx; movl %edx,%eax")

  Revision 1.101  2003/07/08 21:24:59  peter
    * sparc fixes

  Revision 1.100  2003/07/06 21:50:33  jonas
    * fixed ppc compilation problems and changed VOLATILE_REGISTERS for x86
      so that it doesn't include ebp and esp anymore

  Revision 1.99  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.98  2003/07/06 15:31:20  daniel
    * Fixed register allocator. *Lots* of fixes.

  Revision 1.97  2003/07/05 20:21:26  jonas
     * create_paraloc_info() is now called separately for the caller and
       callee info
     * fixed ppc cycle

  Revision 1.96  2003/07/02 22:18:04  peter
    * paraloc splitted in paraloc[callerside],calleeparaloc
    * sparc calling convention updates

  Revision 1.95  2003/06/17 16:34:44  jonas
    * lots of newra fixes (need getfuncretparaloc implementation for i386)!
    * renamed all_intregisters to paramanager.get_volatile_registers_int(pocall_default) and made it
      processor dependent

  Revision 1.94  2003/06/15 16:52:02  jonas
    * release function result registers if the functino result isn't used
    * don't allocate function result register with -dnewra if there is none
    * some optimizations for non-x86 processor (don't save any registers
      before a call)

  Revision 1.93  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.92  2003/06/12 21:10:50  peter
    * newra fixes

  Revision 1.91  2003/06/12 18:38:45  jonas
    * deallocate parameter registers in time for newra
    * for non-i386, procvars and methodpointers always have to be processed
      in advance, whether or not newra is defined

  Revision 1.90  2003/06/09 14:54:26  jonas
    * (de)allocation of registers for parameters is now performed properly
      (and checked on the ppc)
    - removed obsolete allocation of all parameter registers at the start
      of a procedure (and deallocation at the end)

  Revision 1.89  2003/06/09 12:23:29  peter
    * init/final of procedure data splitted from genentrycode
    * use asmnode getposition to insert final at the correct position
      als for the implicit try...finally

  Revision 1.88  2003/06/08 20:01:53  jonas
    * optimized assignments with on the right side a function that returns
      an ansi- or widestring

  Revision 1.87  2003/06/08 18:21:47  jonas
    * fixed weird error in the copyleft statement :)

  Revision 1.86  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.85  2003/06/04 06:43:36  jonas
    * fixed double secondpassing of procvar loads

  Revision 1.84  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.83  2003/06/03 20:27:02  daniel
    * Restored original methodpointer code for non newra case

  Revision 1.82  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.81  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.80  2003/05/31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.79  2003/05/31 00:59:44  peter
    * typo in FUNCTION_RESULT_REG

  Revision 1.78  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.77  2003/05/29 10:05:40  jonas
    * free callparatemps created for call-by-reference parameters

  Revision 1.76  2003/05/28 23:58:18  jonas
    * added missing initialization of rg.usedintin,byproc
    * ppc now also saves/restores used fpu registers
    * ncgcal doesn't add used registers to usedby/inproc anymore, except for
      i386

  Revision 1.75  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.74  2003/05/25 11:34:17  peter
    * methodpointer self pushing fixed

  Revision 1.73  2003/05/25 08:59:16  peter
    * inline fixes

  Revision 1.72  2003/05/24 13:36:54  jonas
    * save fpu results in a normal fpu register on non-x86 processors

  Revision 1.71  2003/05/23 19:35:50  jonas
    - undid previous commit, it was wrong

  Revision 1.70  2003/05/23 19:11:58  jonas
    * fixed tests for whether a certain int register is unused

  Revision 1.69  2003/05/23 18:01:56  jonas
    * fixed ppc compiler

  Revision 1.68  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.67  2003/05/17 13:30:08  jonas
    * changed tt_persistant to tt_persistent :)
    * tempcreatenode now doesn't accept a boolean anymore for persistent
      temps, but a ttemptype, so you can also create ansistring temps etc

  Revision 1.66  2003/05/16 14:33:31  peter
    * regvar fixes

  Revision 1.65  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.64  2003/05/14 19:36:54  jonas
    * patch from Peter for int64 function results

  Revision 1.63  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.62  2003/05/13 15:18:18  peter
    * generate code for procvar first before pushing parameters. Made
      the already existing code for powerpc available for all platforms

  Revision 1.61  2003/05/12 18:17:55  jonas
    * moved fpc_check_object call earlier for the ppc, so it can't destroy
      already-loaded parameter registers

  Revision 1.60  2003/05/11 21:48:38  jonas
    * fixed procvar bug on the ppc (load procvar before loading para's,
      because the procvar may otherwise destroy the already loaded paras)

  Revision 1.59  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.58  2003/05/05 14:53:16  peter
    * vs_hidden replaced by is_hidden boolean

  Revision 1.57  2003/04/30 20:53:32  florian
    * error when address of an abstract method is taken
    * fixed some x86-64 problems
    * merged some more x86-64 and i386 code

  Revision 1.56  2003/04/29 07:28:52  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.55  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.54  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procinfo.procdef is now always nil when parsing
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
    * include FUNCTION_RETURN_REG in regs_to_push list

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
