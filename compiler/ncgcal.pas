{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate assembler for call nodes

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
          procedure allocate_tempparaloc;
          procedure push_addr_para;
          procedure push_value_para;
       public
          procedure secondcallparan;override;
       end;

       tcgcallnode = class(tcallnode)
       private
          procedure release_para_temps;
          procedure normal_pass_2;
          procedure inlined_pass_2;
          procedure pushparas;
          procedure freeparas;
       protected
          framepointer_paraloc : tparalocation;
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
          procedure pop_parasize(pop_size:longint);virtual;
          procedure extra_interrupt_code;virtual;
          procedure extra_call_code;virtual;
          procedure do_syscall;virtual;abstract;
       public
          procedure pass_2;override;
       end;


implementation

    uses
      systems,
      cutils,verbose,globals,
      symconst,symsym,symtable,defutil,paramgr,
{$ifdef GDB}
  {$ifdef delphi}
      sysutils,
  {$else}
      strings,
  {$endif}
      gdb,
{$endif GDB}
      cgbase,pass_2,
      aasmbase,aasmtai,
      nbas,nmem,nld,ncnv,nutils,
{$ifdef x86}
      cga,cgx86,
{$endif x86}
      ncgutil,
      cgutils,cgobj,tgobj,
      procinfo;


{*****************************************************************************
                             TCGCALLPARANODE
*****************************************************************************}

    procedure tcgcallparanode.allocate_tempparaloc;
      begin
         { Allocate (temporary) paralocation }
         tempparaloc:=paraitem.paraloc[callerside];
         case tempparaloc.loc of
           LOC_REGISTER,LOC_FPUREGISTER,LOC_MMREGISTER:
             paramanager.alloctempregs(exprasmlist,tempparaloc);
{$ifdef cputargethasfixedstack}
           LOC_REFERENCE:
             begin
               { currently, we copy the value always to a secure location }
               if not(assigned(aktcallnode.inlinecode)) then
                 paramanager.alloctempparaloc(exprasmlist,aktcallnode.procdefinition.proccalloption,paraitem,tempparaloc);
             end;
{$endif cputargethasfixedstack}
         end;
      end;


    procedure tcgcallparanode.push_addr_para;
      begin
        if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
          internalerror(200304235);
        location_release(exprasmlist,left.location);
        cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempparaloc);
      end;


    procedure tcgcallparanode.push_value_para;
{$ifdef i386}
      var
        cgsize : tcgsize;
        href   : treference;
        size   : longint;
{$endif i386}
      begin
        { we've nothing to push when the size of the parameter is 0 }
        if left.resulttype.def.size=0 then
          exit;

        { Move flags and jump in register to make it less complex }
        if left.location.loc in [LOC_FLAGS,LOC_JUMP] then
          location_force_reg(exprasmlist,left.location,def_cgsize(left.resulttype.def),false);

        { Handle Floating point types differently }
        if left.resulttype.def.deftype=floatdef then
         begin
           location_release(exprasmlist,left.location);
{$ifdef i386}
           if tempparaloc.loc<>LOC_REFERENCE then
             internalerror(200309291);
           case left.location.loc of
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               begin
                 size:=align(TCGSize2Size[left.location.size],tempparaloc.alignment);
                 if tempparaloc.reference.index=NR_STACK_POINTER_REG then
                   begin
                     cg.g_stackpointer_alloc(exprasmlist,size);
                     reference_reset_base(href,NR_STACK_POINTER_REG,0);
                   end
                 else
                   reference_reset_base(href,tempparaloc.reference.index,tempparaloc.reference.offset);
                 cg.a_loadfpu_reg_ref(exprasmlist,left.location.size,left.location.register,href);
               end;
             LOC_MMREGISTER,
             LOC_CMMREGISTER:
               begin
                 size:=align(tfloatdef(left.resulttype.def).size,tempparaloc.alignment);
                 if tempparaloc.reference.index=NR_STACK_POINTER_REG then
                   begin
                     cg.g_stackpointer_alloc(exprasmlist,size);
                     reference_reset_base(href,NR_STACK_POINTER_REG,0);
                   end
                 else
                   reference_reset_base(href,tempparaloc.reference.index,tempparaloc.reference.offset);
                 cg.a_loadmm_reg_ref(exprasmlist,left.location.size,left.location.size,left.location.register,href,mms_movescalar);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 size:=align(left.resulttype.def.size,tempparaloc.alignment);
                 if tempparaloc.reference.index=NR_STACK_POINTER_REG then
                   begin
                     href:=left.location.reference;
                     inc(href.offset,size);
                     while (size>0) do
                      begin
                        if (size>=4) or (tempparaloc.alignment>=4) then
                         begin
                           cgsize:=OS_32;
                           dec(href.offset,4);
                           dec(size,4);
                         end
                        else
                         begin
                           cgsize:=OS_16;
                           dec(href.offset,2);
                           dec(size,2);
                         end;
                        cg.a_param_ref(exprasmlist,cgsize,href,tempparaloc);
                      end;
                   end
                 else
                   begin
                     reference_reset_base(href,tempparaloc.reference.index,tempparaloc.reference.offset);
                     cg.g_concatcopy(exprasmlist,left.location.reference,href,size,false,false);
                   end;
               end;
             else
               internalerror(2002042430);
           end;
{$else i386}
           case left.location.loc of
             LOC_MMREGISTER,
             LOC_CMMREGISTER:
               case tempparaloc.loc of
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   cg.a_parammm_reg(exprasmlist,left.location.size,left.location.register,tempparaloc,mms_movescalar);
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   begin
                     location_force_fpureg(exprasmlist,left.location,false);
                     cg.a_paramfpu_reg(exprasmlist,left.location.size,left.location.register,tempparaloc);
                   end;
                 else
                   internalerror(2002042433);
               end;
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               case tempparaloc.loc of
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   begin
                     location_force_mmregscalar(exprasmlist,left.location,false);
                     cg.a_parammm_reg(exprasmlist,left.location.size,left.location.register,tempparaloc,mms_movescalar);
                   end;
{$ifdef x86_64}
                 { x86_64 pushes s64comp in normal register }
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     location_force_mem(exprasmlist,left.location);
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     cg.a_param_ref(exprasmlist,left.location.size,left.location.reference,tempparaloc);
                   end;
{$endif x86_64}
{$ifdef sparc}
                 { sparc pushes floats in normal registers }
                 LOC_REGISTER,
                 LOC_CREGISTER,
{$endif sparc}
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   cg.a_paramfpu_reg(exprasmlist,left.location.size,left.location.register,tempparaloc);
                 else
                   internalerror(2002042433);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE:
               case tempparaloc.loc of
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   cg.a_parammm_ref(exprasmlist,left.location.size,left.location.reference,tempparaloc,mms_movescalar);
{$ifdef sparc}
                 { sparc pushes floats in normal registers }
                 LOC_REGISTER,
                 LOC_CREGISTER,
{$endif sparc}
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   cg.a_paramfpu_ref(exprasmlist,left.location.size,left.location.reference,tempparaloc);
                 else
                   internalerror(2002042431);
               end;
             else
               internalerror(2002042432);
           end;
{$endif i386}
         end
        else
         begin
           { copy the value on the stack or use normal parameter push?
             Check for varargs first because that has no paraitem }
           if not(cpf_varargs_para in callparaflags) and
              paramanager.copy_value_on_stack(paraitem.paratyp,left.resulttype.def,
                  aktcallnode.procdefinition.proccalloption) then
            begin
              location_release(exprasmlist,left.location);
{$ifdef i386}
              if tempparaloc.loc<>LOC_REFERENCE then
                internalerror(200309292);
              if not (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                internalerror(200204241);
              { push on stack }
              size:=align(left.resulttype.def.size,tempparaloc.alignment);
              if tempparaloc.reference.index=NR_STACK_POINTER_REG then
                begin
                  cg.g_stackpointer_alloc(exprasmlist,size);
                  reference_reset_base(href,NR_STACK_POINTER_REG,0);
                end
              else
                reference_reset_base(href,tempparaloc.reference.index,tempparaloc.reference.offset);
              cg.g_concatcopy(exprasmlist,left.location.reference,href,size,false,false);
{$else i386}
              cg.a_param_copy_ref(exprasmlist,left.resulttype.def.size,left.location.reference,tempparaloc);
{$endif i386}
            end
           else
            begin
              case left.location.loc of
                LOC_CONSTANT,
                LOC_REGISTER,
                LOC_CREGISTER,
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
{$ifndef cpu64bit}
                    if left.location.size in [OS_64,OS_S64] then
                     begin
                       cg64.a_param64_loc(exprasmlist,left.location,tempparaloc);
                       location_release(exprasmlist,left.location);
                     end
                    else
{$endif cpu64bit}
                     begin
                       location_release(exprasmlist,left.location);
                       cg.a_param_loc(exprasmlist,left.location,tempparaloc);
                     end;
                  end;
{$ifdef SUPPORT_MMX}
                LOC_MMXREGISTER,
                LOC_CMMXREGISTER:
                  begin
                     location_release(exprasmlist,left.location);
                     cg.a_parammm_reg(exprasmlist,left.location.register);
                  end;
{$endif SUPPORT_MMX}
                else
                  internalerror(200204241);
              end;
           end;
         end;
      end;


    procedure tcgcallparanode.secondcallparan;
      var
         otlabel,
         oflabel : tasmlabel;
         hp      : tnode;
      begin
         if not(assigned(paraitem)) or
            not(assigned(paraitem.paratype.def)) or
            not(assigned(paraitem.parasym) or
                (cpf_varargs_para in callparaflags)) then
           internalerror(200304242);

         { Skip nothingn nodes which are used after disabling
           a parameter }
         if (left.nodetype<>nothingn) then
           begin
             otlabel:=truelabel;
             oflabel:=falselabel;
             objectlibrary.getlabel(truelabel);
             objectlibrary.getlabel(falselabel);
             secondpass(left);

             allocate_tempparaloc;

             { handle varargs first, because paraitem.parasym is not valid }
             if (cpf_varargs_para in callparaflags) then
               begin
                 if paramanager.push_addr_param(vs_value,left.resulttype.def,
                        aktcallnode.procdefinition.proccalloption) then
                   push_addr_para
                 else
                   push_value_para;
               end
             { hidden parameters }
             else if paraitem.is_hidden then
               begin
                 { don't push a node that already generated a pointer type
                   by address for implicit hidden parameters }
                 if (vo_is_funcret in tvarsym(paraitem.parasym).varoptions) or
                    (not(left.resulttype.def.deftype in [pointerdef,classrefdef]) and
                     paramanager.push_addr_param(paraitem.paratyp,paraitem.paratype.def,
                         aktcallnode.procdefinition.proccalloption)) then
                   push_addr_para
                 else
                   push_value_para;
               end
             { formal def }
             else if (paraitem.paratype.def.deftype=formaldef) then
               begin
                  { allow passing of a constant to a const formaldef }
                  if (tvarsym(paraitem.parasym).varspez=vs_const) and
                     (left.location.loc=LOC_CONSTANT) then
                    location_force_mem(exprasmlist,left.location);

                  { allow (typecasted) @var }
                  hp:=left;
                  while (hp.nodetype=typeconvn) do
                    hp:=ttypeconvnode(hp).left;
                  if (hp.nodetype=addrn) and
                     (not(nf_procvarload in hp.flags)) then
                    begin
                      location_release(exprasmlist,left.location);
                      cg.a_param_loc(exprasmlist,left.location,tempparaloc);
                    end
                  else
                    push_addr_para;
               end
             { Normal parameter }
             else
               begin
                 { don't push a node that already generated a pointer type
                   by address for implicit hidden parameters }
                 if (not(
                         paraitem.is_hidden and
                         (left.resulttype.def.deftype in [pointerdef,classrefdef])
                        ) and
                     paramanager.push_addr_param(paraitem.paratyp,paraitem.paratype.def,
                         aktcallnode.procdefinition.proccalloption)) then
                   begin
                      { Check for passing a constant to var,out parameter }
                      if (paraitem.paratyp in [vs_var,vs_out]) and
                         (left.location.loc<>LOC_REFERENCE) then
                       begin
                         { passing self to a var parameter is allowed in
                           TP and delphi }
                         if not((left.location.loc=LOC_CREFERENCE) and
                                is_self_node(left)) then
                          internalerror(200106041);
                       end;
                      { Force to be in memory }
                      if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
                        location_force_mem(exprasmlist,left.location);
                      push_addr_para;
                   end
                 else
                   push_value_para;
               end;
             truelabel:=otlabel;
             falselabel:=oflabel;

             { update return location in callnode when this is the function
               result }
             if assigned(paraitem.parasym) and
                (vo_is_funcret in tvarsym(paraitem.parasym).varoptions) then
               location_copy(aktcallnode.location,left.location);
           end;

         { next parameter }
         if assigned(right) then
           tcallparanode(right).secondcallparan;
      end;


{*****************************************************************************
                             TCGCALLNODE
*****************************************************************************}

    procedure tcgcallnode.extra_interrupt_code;
      begin
      end;


    procedure tcgcallnode.extra_call_code;
      begin
      end;


    procedure tcgcallnode.pop_parasize(pop_size:longint);
      begin
      end;


    procedure tcgcallnode.handle_return_value;
      var
        cgsize : tcgsize;
        hregister : tregister;
        tempnode: tnode;
        resultloc : tparalocation;
      begin
        resultloc:=procdefinition.funcret_paraloc[callerside];
        cgsize:=resultloc.size;
        { structured results are easy to handle....
          needed also when result_no_used !! }
        if (procdefinition.proctypeoption<>potype_constructor) and
           paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
          begin
            { Location should be setup by the funcret para }
            if location.loc<>LOC_REFERENCE then
             internalerror(200304241);
          end
        { ansi/widestrings must be registered, so we can dispose them }
        else if resulttype.def.needs_inittable then
          begin
            { the FUNCTION_RESULT_REG is already allocated }
            if not assigned(funcretnode) then
              begin
                location_reset(location,LOC_REFERENCE,OS_ADDR);
                location.reference:=refcountedtemp;
                { a_load_reg_ref may allocate registers! }
                cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,NR_FUNCTION_RESULT_REG,location.reference);
                cg.ungetregister(exprasmlist,NR_FUNCTION_RESULT_REG);
              end
            else
              begin
                cg.ungetregister(exprasmlist,resultloc.register);
                hregister := cg.getaddressregister(exprasmlist);
                cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,resultloc.register,hregister);
                { in case of a regular funcretnode with ret_in_param, the }
                { original funcretnode isn't touched -> make sure it's    }
                { the same here (not sure if it's necessary)              }
                tempnode := funcretnode.getcopy;
                tempnode.pass_2;
                location := tempnode.location;
                tempnode.free;
                cg.g_decrrefcount(exprasmlist,resulttype.def,location.reference,false);
                cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
                cg.ungetregister(exprasmlist,hregister);
             end;
          end
        { we have only to handle the result if it is used }
        else if (cnf_return_value_used in callnodeflags) then
          begin
            location.loc:=resultloc.loc;
            case resultloc.loc of
               LOC_FPUREGISTER:
                 begin
                   location_reset(location,LOC_FPUREGISTER,cgsize);
                   location.register:=procdefinition.funcret_paraloc[callerside].register;
{$ifdef x86}
                   tcgx86(cg).inc_fpu_stack;
{$else x86}
                   cg.ungetregister(exprasmlist,location.register);
                   hregister:=cg.getfpuregister(exprasmlist,location.size);
                   cg.a_loadfpu_reg_reg(exprasmlist,location.size,location.register,hregister);
                   location.register:=hregister;
{$endif x86}
                 end;

               LOC_REGISTER:
                 begin
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
                         cg.ungetregister(exprasmlist,NR_FUNCTION_RESULT64_LOW_REG);
                         location.registerlow:=cg.getintregister(exprasmlist,OS_32);
                         cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,NR_FUNCTION_RESULT64_LOW_REG,location.registerlow);
                         cg.ungetregister(exprasmlist,NR_FUNCTION_RESULT64_HIGH_REG);
                         location.registerhigh:=cg.getintregister(exprasmlist,OS_32);
                         cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,NR_FUNCTION_RESULT64_HIGH_REG,location.registerhigh);
                       end
                      else
{$endif cpu64bit}
                       begin
                         { Move the function result to a free register, preferably the
                           FUNCTION_RESULT_REG, so no move is necessary.}
                         { the FUNCTION_RESULT_REG is already allocated }
                         cg.ungetregister(exprasmlist,resultloc.register);
                         { change register size after the unget because the
                           getregister was done for the full register

                           def_cgsize(resulttype.def) is used here because
                           it could be a constructor call }
                         location.register:=cg.getintregister(exprasmlist,def_cgsize(resulttype.def));
                         cg.a_load_reg_reg(exprasmlist,cgsize,def_cgsize(resulttype.def),resultloc.register,location.register);
                       end;
                    end
                   else
                    begin
                      if resulttype.def.size>0 then
                        internalerror(200305131);
                    end;
                 end;

               LOC_MMREGISTER:
                 begin
                   location_reset(location,LOC_MMREGISTER,cgsize);
                   cg.ungetregister(exprasmlist,resultloc.register);
                   location.register:=cg.getmmregister(exprasmlist,cgsize);
                   cg.a_loadmm_reg_reg(exprasmlist,cgsize,cgsize,resultloc.register,location.register,mms_movescalar);
                 end;

               else
                 internalerror(200405023);
             end;
          end
        else
          begin
            if cgsize<>OS_NO then
              paramanager.freeparaloc(exprasmlist,resultloc);
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
                 if not(assigned(ppn.paraitem.parasym)) or
                    not(vo_is_funcret in tvarsym(ppn.paraitem.parasym).varoptions) then
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


     procedure tcgcallnode.pushparas;
       var
         ppn : tcgcallparanode;
{$ifdef cputargethasfixedstack}
         href : treference;
{$endif cputargethasfixedstack}
       begin
         { copy all resources to the allocated registers }
         ppn:=tcgcallparanode(left);
         while assigned(ppn) do
           begin
             if (ppn.left.nodetype<>nothingn) then
               begin
                 { better check for the real location of the parameter here, when stack passed parameters
                   are saved temporary in registers, checking for the tempparaloc.loc is wrong
                 }
                 case ppn.paraitem.paraloc[callerside].loc of
                   LOC_REGISTER:
                     begin
                       if not assigned(inlinecode) then
                         begin
                           paramanager.freeparaloc(exprasmlist,ppn.tempparaloc);
                           paramanager.allocparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
                         end;
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
                   LOC_FPUREGISTER:
                     begin
                       if not assigned(inlinecode) then
                         begin
                           paramanager.freeparaloc(exprasmlist,ppn.tempparaloc);
                           paramanager.allocparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
                           cg.a_loadfpu_reg_reg(exprasmlist,ppn.tempparaloc.size,
                             ppn.tempparaloc.register,ppn.paraitem.paraloc[callerside].register);
                         end;
                     end;
                   LOC_MMREGISTER:
                     begin
                       if not assigned(inlinecode) then
                         begin
                           paramanager.freeparaloc(exprasmlist,ppn.tempparaloc);
                           paramanager.allocparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
                         end;
                       cg.a_loadmm_reg_reg(exprasmlist,ppn.tempparaloc.size,
                         ppn.tempparaloc.size,ppn.tempparaloc.register,ppn.paraitem.paraloc[callerside].register,mms_movescalar);
                     end;
                   LOC_REFERENCE:
                     begin
                       if not assigned(inlinecode) then
                         begin
{$ifdef cputargethasfixedstack}
                           { copy parameters in case they were moved to a temp. location because we've a fixed stack }
                           paramanager.freeparaloc(exprasmlist,ppn.tempparaloc);
                           paramanager.allocparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
                           case ppn.tempparaloc.loc of
                             LOC_REFERENCE:
                               begin
                                 reference_reset_base(href,ppn.tempparaloc.reference.index,ppn.tempparaloc.reference.offset);
                                 if ppn.paraitem.paraloc[callerside].size=OS_NO then
                                   cg.a_param_copy_ref(exprasmlist,ppn.paraitem.paratype.def.size,href,ppn.paraitem.paraloc[callerside])
                                 else
                                   cg.a_param_ref(exprasmlist,ppn.paraitem.paraloc[callerside].size,href,ppn.paraitem.paraloc[callerside]);
                               end;
                             LOC_REGISTER:
      {$ifndef cpu64bit}
                               if ppn.tempparaloc.size in [OS_64,OS_S64] then
                                 cg64.a_param64_reg(exprasmlist,ppn.tempparaloc.register64,ppn.paraitem.paraloc[callerside])
                               else
      {$endif cpu64bit}
                                 cg.a_param_reg(exprasmlist,ppn.paraitem.paraloc[callerside].size,ppn.tempparaloc.register,ppn.paraitem.paraloc[callerside]);
                             LOC_FPUREGISTER:
                               cg.a_paramfpu_reg(exprasmlist,ppn.paraitem.paraloc[callerside].size,ppn.tempparaloc.register,ppn.paraitem.paraloc[callerside]);
                             else
                               internalerror(200402081);
                           end;
  {$endif cputargethasfixedstack}
                         end;
                     end;
                   else
                     internalerror(200402091);
                 end;
               end;
             ppn:=tcgcallparanode(ppn.right);
           end;
       end;

     procedure tcgcallnode.freeparas;
       var
         ppn : tcgcallparanode;
       begin
         { free the resources allocated for the parameters }
         ppn:=tcgcallparanode(left);
         while assigned(ppn) do
           begin
             if not assigned(inlinecode) or
                (ppn.paraitem.paraloc[callerside].loc <> LOC_REFERENCE) then
               paramanager.freeparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
             ppn:=tcgcallparanode(ppn.right);
           end;
       end;



    procedure tcgcallnode.normal_pass_2;
      var
         regs_to_push_fpu,
         regs_to_alloc,
         regs_to_free : Tcpuregisterset;
         href : treference;
         pop_size : longint;
         pvreg,
         vmtreg : tregister;
         oldaktcallnode : tcallnode;

      begin
         if not assigned(procdefinition) or
            not procdefinition.has_paraloc_info then
           internalerror(200305264);

         if resulttype.def.needs_inittable and
            not paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) and
            not assigned(funcretnode) then
           begin
             tg.gettemptyped(exprasmlist,resulttype.def,tt_normal,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp,false);
           end;

        regs_to_alloc:=paramanager.get_volatile_registers_int(procdefinition.proccalloption);
        regs_to_push_fpu:=paramanager.get_volatile_registers_fpu(procdefinition.proccalloption);

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
              LOC_FPUREGISTER,LOC_CFPUREGISTER:
                begin
                  include(regs_to_push_fpu,getsupreg(procdefinition.funcret_paraloc[callerside].register));
                end;
              LOC_MMREGISTER,LOC_CMMREGISTER:
                begin
                  include(regs_to_alloc,getsupreg(procdefinition.funcret_paraloc[callerside].register));
                end;
            end;
          end;

         { Process parameters, register parameters will be loaded
           in imaginary registers. The actual load to the correct
           register is done just before the call }
         oldaktcallnode:=aktcallnode;
         aktcallnode:=self;
         if assigned(left) then
           tcallparanode(left).secondcallparan;
         aktcallnode:=oldaktcallnode;

         { procedure variable or normal function call ? }
         if (right=nil) then
           begin
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
               {$warning fixme regvars}
{              rg.saveotherregvars(exprasmlist,regs_to_push_other);}

              if (po_virtualmethod in procdefinition.procoptions) and
                 assigned(methodpointer) then
                begin
                   vmtreg:=methodpointer.location.register;

                   { release self }
                   cg.ungetregister(exprasmlist,vmtreg);
                   pvreg:=cg.getintregister(exprasmlist,OS_ADDR);
                   reference_reset_base(href,vmtreg,
                      tprocdef(procdefinition)._class.vmtmethodoffset(tprocdef(procdefinition).extnumber));
                   cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,href,pvreg);

                   { Load parameters that are in temporary registers in the
                     correct parameter register }
                   if assigned(left) then
                     pushparas;

                   { free the resources allocated for the parameters }
                   freeparas;

                   { Release register containing procvar }
                   cg.ungetregister(exprasmlist,pvreg);

                   cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,regs_to_alloc);
                   if cg.uses_registers(R_FPUREGISTER) then
                     cg.allocexplicitregisters(exprasmlist,R_FPUREGISTER,regs_to_push_fpu);
                   if cg.uses_registers(R_MMREGISTER) then
                     cg.allocexplicitregisters(exprasmlist,R_MMREGISTER,paramanager.get_volatile_registers_mm(procdefinition.proccalloption));

                   { call method }
                   extra_call_code;
                   cg.a_call_reg(exprasmlist,pvreg);
                end
              else
                begin
                  { Load parameters that are in temporary registers in the
                    correct parameter register }
                  if assigned(left) then
                    pushparas;

                  { free the resources allocated for the parameters }
                  freeparas;

                  cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,regs_to_alloc);
                  if cg.uses_registers(R_FPUREGISTER) then
                    cg.allocexplicitregisters(exprasmlist,R_FPUREGISTER,regs_to_push_fpu);
                  if cg.uses_registers(R_MMREGISTER) then
                    cg.allocexplicitregisters(exprasmlist,R_MMREGISTER,paramanager.get_volatile_registers_mm(procdefinition.proccalloption));

                  if procdefinition.proccalloption=pocall_syscall then
                    do_syscall
                  else
                    begin
                      { Calling interrupt from the same code requires some
                        extra code }
                      if (po_interrupt in procdefinition.procoptions) then
                        extra_interrupt_code;
                      extra_call_code;
                      cg.a_call_name(exprasmlist,tprocdef(procdefinition).mangledname);
                    end;
               end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(right);

              location_release(exprasmlist,right.location);
              pvreg:=cg.getintregister(exprasmlist,OS_ADDR);
              { Only load OS_ADDR from the reference }
              if right.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                cg.a_load_ref_reg(exprasmlist,OS_ADDR,OS_ADDR,right.location.reference,pvreg)
              else
                cg.a_load_loc_reg(exprasmlist,OS_ADDR,right.location,pvreg);
              location_freetemp(exprasmlist,right.location);

              { Load parameters that are in temporary registers in the
                correct parameter register }
              if assigned(left) then
                pushparas;

              { free the resources allocated for the parameters }
              freeparas;

              { Release register containing procvar }
              cg.ungetregister(exprasmlist,pvreg);

              cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,regs_to_alloc);
              if cg.uses_registers(R_FPUREGISTER) then
                cg.allocexplicitregisters(exprasmlist,R_FPUREGISTER,regs_to_push_fpu);
              if cg.uses_registers(R_MMREGISTER) then
                cg.allocexplicitregisters(exprasmlist,R_MMREGISTER,paramanager.get_volatile_registers_mm(procdefinition.proccalloption));

              { Calling interrupt from the same code requires some
                extra code }
              if (po_interrupt in procdefinition.procoptions) then
                extra_interrupt_code;
              {$warning fixme regvars.}
{              rg.saveotherregvars(exprasmlist,ALL_OTHERREGISTERS);}
              extra_call_code;
              cg.a_call_reg(exprasmlist,pvreg);
           end;

         { Need to remove the parameters from the stack? }
         if (procdefinition.proccalloption in clearstack_pocalls) then
          begin
            pop_size:=pushedparasize;
            { for Cdecl functions we don't need to pop the funcret when it
              was pushed by para }
            if paramanager.ret_in_param(procdefinition.rettype.def,procdefinition.proccalloption) then
              dec(pop_size,sizeof(aint));
            { Remove parameters/alignment from the stack }
            pop_parasize(pop_size);
          end;

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
               LOC_FPUREGISTER,LOC_CFPUREGISTER:
                 begin
                   exclude(regs_to_push_fpu,getsupreg(procdefinition.funcret_paraloc[callerside].register));
                 end;
             end;
           end;
         if cg.uses_registers(R_MMREGISTER) then
           cg.deallocexplicitregisters(exprasmlist,R_MMREGISTER,paramanager.get_volatile_registers_mm(procdefinition.proccalloption));
         if cg.uses_registers(R_FPUREGISTER) then
           cg.deallocexplicitregisters(exprasmlist,R_FPUREGISTER,regs_to_push_fpu);
         cg.deallocexplicitregisters(exprasmlist,R_INTREGISTER,regs_to_free);

         { handle function results }
         if (not is_void(resulttype.def)) then
           handle_return_value
         else
           location_reset(location,LOC_VOID,OS_NO);

         { perhaps i/o check ? }
         if (cs_check_io in aktlocalswitches) and
            (po_iocheck in procdefinition.procoptions) and
            not(po_iocheck in current_procinfo.procdef.procoptions) and
            { no IO check for methods and procedure variables }
            (right=nil) and
            not(po_virtualmethod in procdefinition.procoptions) then
           begin
              cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
              cg.deallocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           end;

         { release temps of paras }
         release_para_temps;

         { if return value is not used }
         if (not(cnf_return_value_used in callnodeflags)) and (not is_void(resulttype.def)) then
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
{$endif x86}
                end;
           end;
      end;


    procedure tcgcallnode.inlined_pass_2;
      var
         oldaktcallnode : tcallnode;
         oldprocinfo : tprocinfo;
         oldinlining_procedure : boolean;
         inlineentrycode,inlineexitcode : TAAsmoutput;
{$ifdef GDB}
         startlabel,endlabel : tasmlabel;
         pp : pchar;
         mangled_length  : longint;
{$endif GDB}
      begin
         if not(assigned(procdefinition) and (procdefinition.deftype=procdef)) then
           internalerror(200305262);

         oldinlining_procedure:=inlining_procedure;
         oldprocinfo:=current_procinfo;
         { we're inlining a procedure }
         inlining_procedure:=true;

         { Add inling start }
{$ifdef GDB}
         exprasmlist.concat(Tai_force_line.Create);
{$endif GDB}
         exprasmList.concat(Tai_Marker.Create(InlineStart));
{$ifdef extdebug}
         exprasmList.concat(tai_comment.Create(strpnew('Start of inlined proc '+tprocdef(procdefinition).procsym.name)));
{$endif extdebug}

         { calculate registers to pass the parameters }
         paramanager.create_inline_paraloc_info(procdefinition);

         { Allocate parameters and locals }
         gen_alloc_inline_parast(exprasmlist,tparasymtable(procdefinition.parast));
         if tprocdef(procdefinition).localst.symtabletype=localsymtable then
           gen_alloc_localst(exprasmlist,tlocalsymtable(tprocdef(procdefinition).localst));

         { if we allocate the temp. location for ansi- or widestrings }
         { already here, we avoid later a push/pop                    }
         if resulttype.def.needs_inittable and
            not paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
           begin
             tg.gettemptyped(exprasmlist,resulttype.def,tt_normal,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp,false);
           end;

         { Push parameters, still use the old current_procinfo. This
           is required that have the correct information available like
           _class and nested procedure }
         oldaktcallnode:=aktcallnode;
         aktcallnode:=self;
         if assigned(left) then
           begin
             tcallparanode(left).secondcallparan;
             pushparas;
           end;
         aktcallnode:=oldaktcallnode;

         { create temp procinfo that will be used for the inlinecode tree }
         current_procinfo:=cprocinfo.create(nil);
         current_procinfo.procdef:=tprocdef(procdefinition);
         current_procinfo.flags:=oldprocinfo.flags;
         current_procinfo.aktlocaldata.destroy;
         current_procinfo.aktlocaldata:=oldprocinfo.aktlocaldata;

         { when the oldprocinfo is also being inlined reuse the
           inlining_procinfo }
         if assigned(oldprocinfo.inlining_procinfo) then
           current_procinfo.inlining_procinfo:=oldprocinfo.inlining_procinfo
         else
           current_procinfo.inlining_procinfo:=oldprocinfo;

         { takes care of local data initialization }
         inlineentrycode:=TAAsmoutput.Create;
         inlineexitcode:=TAAsmoutput.Create;

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
            not(cs_gdb_valgrind in aktglobalswitches) then
           begin
             objectlibrary.getaddrlabel(startlabel);
             objectlibrary.getaddrlabel(endlabel);
             cg.a_label(exprasmlist,startlabel);

             { Here we must include the para and local symtable info }
             procdefinition.concatstabto(withdebuglist);

             mangled_length:=length(current_procinfo.inlining_procinfo.procdef.mangledname);
             getmem(pp,mangled_length+50);
             strpcopy(pp,'192,0,0,'+startlabel.name);
             if (target_info.use_function_relative_addresses) then
               begin
                 strpcopy(strend(pp),'-');
                 strpcopy(strend(pp),current_procinfo.inlining_procinfo.procdef.mangledname);
               end;
             withdebugList.concat(Tai_stabn.Create(strnew(pp)));
           end;
{$endif GDB}

         gen_load_para_value(inlineentrycode);
         { now that we've loaded the para's, free them }
         freeparas;
         gen_initialize_code(inlineentrycode,true);
         if po_assembler in current_procinfo.procdef.procoptions then
           inlineentrycode.insert(Tai_marker.Create(asmblockstart));
         exprasmList.concatlist(inlineentrycode);

         { process the inline code }
         secondpass(inlinecode);

         cg.a_label(exprasmlist,current_procinfo.aktexitlabel);
         gen_finalize_code(inlineexitcode,true);
         gen_load_return_value(inlineexitcode);
         if po_assembler in current_procinfo.procdef.procoptions then
           inlineexitcode.concat(Tai_marker.Create(asmblockend));
         exprasmlist.concatlist(inlineexitcode);

         inlineentrycode.free;
         inlineexitcode.free;
{$ifdef extdebug}
         exprasmList.concat(tai_comment.Create(strpnew('End of inlined proc')));
{$endif extdebug}
         exprasmList.concat(Tai_Marker.Create(InlineEnd));

         { handle function results }
         if (not is_void(resulttype.def)) then
           handle_return_value
         else
           location_reset(location,LOC_VOID,OS_NO);

         { perhaps i/o check ? }
         if (cs_check_io in aktlocalswitches) and
            (po_iocheck in procdefinition.procoptions) and
            not(po_iocheck in current_procinfo.procdef.procoptions) and
            { no IO check for methods and procedure variables }
            (right=nil) and
            not(po_virtualmethod in procdefinition.procoptions) then
           begin
              cg.allocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
              cg.deallocexplicitregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           end;

         { release temps of paras }
         release_para_temps;

         { if return value is not used }
         if (not is_void(resulttype.def)) and
            (not(cnf_return_value_used in callnodeflags)) then
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
{$endif x86}
                end;
           end;

         { Release parameters and locals }
         gen_free_parast(exprasmlist,tparasymtable(current_procinfo.procdef.parast));
         if current_procinfo.procdef.localst.symtabletype=localsymtable then
           gen_free_localst(exprasmlist,tlocalsymtable(current_procinfo.procdef.localst));

{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
            not(cs_gdb_valgrind in aktglobalswitches) then
           begin
             cg.a_label(exprasmlist,endlabel);
             strpcopy(pp,'224,0,0,'+endlabel.name);
             if (target_info.use_function_relative_addresses) then
               begin
                 strpcopy(strend(pp),'-');
                 strpcopy(strend(pp),current_procinfo.inlining_procinfo.procdef.mangledname);
               end;
             withdebugList.concat(Tai_stabn.Create(strnew(pp)));
             freemem(pp,mangled_length+50);
           end;
{$endif GDB}

         { restore }
         current_procinfo.aktlocaldata:=nil;
         current_procinfo.destroy;
         current_procinfo:=oldprocinfo;
         inlining_procedure:=oldinlining_procedure;
      end;


    procedure tcgcallnode.pass_2;
      begin
        if assigned(methodpointerinit) then
          secondpass(methodpointerinit);

        if assigned(inlinecode) then
          inlined_pass_2
        else
          normal_pass_2;

        if assigned(methodpointerdone) then
          secondpass(methodpointerdone);
      end;


begin
   ccallparanode:=tcgcallparanode;
   ccallnode:=tcgcallnode;
end.
{
  $Log$
  Revision 1.172  2004-07-11 19:01:13  peter
    * comps are passed in int registers

  Revision 1.171  2004/07/09 23:41:04  jonas
    * support register parameters for inlined procedures + some inline
      cleanups

  Revision 1.170  2004/06/29 20:56:46  peter
    * constructors don't return in parameter

  Revision 1.169  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.168  2004/06/16 20:07:08  florian
    * dwarf branch merged

  Revision 1.167  2004/05/23 18:28:41  peter
    * methodpointer is loaded into a temp when it was a calln

  Revision 1.166  2004/05/22 23:34:27  peter
  tai_regalloc.allocation changed to ratype to notify rgobj of register size changes

  Revision 1.165  2004/04/28 15:19:03  florian
    + syscall directive support for MorphOS added

  Revision 1.164.2.13  2004/06/12 17:01:01  florian
    * fixed compilation of arm compiler

}
