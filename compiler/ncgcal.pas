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
      parabase,
      symdef,node,ncal;

    type
       tcgcallparanode = class(tcallparanode)
       private
          tempcgpara : tcgpara;
          procedure push_addr_para;
          procedure push_value_para;
       public
          constructor create(expr,next : tnode);override;
          destructor destroy;override;
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
          framepointer_paraloc : tcgpara;
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
      strings,
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

    constructor tcgcallparanode.create(expr,next : tnode);
      begin
        inherited create(expr,next);
        tempcgpara.init;
      end;


    destructor tcgcallparanode.destroy;
      begin
        tempcgpara.done;
        inherited destroy;
      end;


    procedure tcgcallparanode.push_addr_para;
      begin
        if not(left.location.loc in [LOC_CREFERENCE,LOC_REFERENCE]) then
          internalerror(200304235);
        cg.a_paramaddr_ref(exprasmlist,left.location.reference,tempcgpara);
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
{$ifdef i386}
           if tempcgpara.location^.loc<>LOC_REFERENCE then
             internalerror(200309291);
           case left.location.loc of
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               begin
                 size:=align(TCGSize2Size[left.location.size],tempcgpara.alignment);
                 if tempcgpara.location^.reference.index=NR_STACK_POINTER_REG then
                   begin
                     cg.g_stackpointer_alloc(exprasmlist,size);
                     reference_reset_base(href,NR_STACK_POINTER_REG,0);
                   end
                 else
                   reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
                 cg.a_loadfpu_reg_ref(exprasmlist,left.location.size,left.location.register,href);
               end;
             LOC_MMREGISTER,
             LOC_CMMREGISTER:
               begin
                 size:=align(tfloatdef(left.resulttype.def).size,tempcgpara.alignment);
                 if tempcgpara.location^.reference.index=NR_STACK_POINTER_REG then
                   begin
                     cg.g_stackpointer_alloc(exprasmlist,size);
                     reference_reset_base(href,NR_STACK_POINTER_REG,0);
                   end
                 else
                   reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
                 cg.a_loadmm_reg_ref(exprasmlist,left.location.size,left.location.size,left.location.register,href,mms_movescalar);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 size:=align(left.resulttype.def.size,tempcgpara.alignment);
                 if tempcgpara.location^.reference.index=NR_STACK_POINTER_REG then
                   begin
                     href:=left.location.reference;
                     inc(href.offset,size);
                     while (size>0) do
                      begin
                        if (size>=4) or (tempcgpara.alignment>=4) then
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
                        cg.a_param_ref(exprasmlist,cgsize,href,tempcgpara);
                      end;
                   end
                 else
                   begin
                     reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
                     cg.g_concatcopy(exprasmlist,left.location.reference,href,size);
                   end;
               end;
             else
               internalerror(2002042430);
           end;
{$else i386}
           case left.location.loc of
             LOC_MMREGISTER,
             LOC_CMMREGISTER:
               case tempcgpara.location^.loc of
                 LOC_REFERENCE,
                 LOC_CREFERENCE,
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   cg.a_parammm_reg(exprasmlist,left.location.size,left.location.register,tempcgpara,mms_movescalar);
                 LOC_FPUREGISTER,
                 LOC_CFPUREGISTER:
                   begin
                     location_force_fpureg(exprasmlist,left.location,false);
                     cg.a_paramfpu_reg(exprasmlist,left.location.size,left.location.register,tempcgpara);
                   end;
                 else
                   internalerror(2002042433);
               end;
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               case tempcgpara.location^.loc of
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   begin
                     location_force_mmregscalar(exprasmlist,left.location,false);
                     cg.a_parammm_reg(exprasmlist,left.location.size,left.location.register,tempcgpara,mms_movescalar);
                   end;
{$ifdef x86_64}
                 { x86_64 pushes s64comp in normal register }
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     location_force_mem(exprasmlist,left.location);
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     cg.a_param_ref(exprasmlist,left.location.size,left.location.reference,tempcgpara);
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
                   cg.a_paramfpu_reg(exprasmlist,left.location.size,left.location.register,tempcgpara);
                 else
                   internalerror(2002042433);
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE:
               case tempcgpara.location^.loc of
                 LOC_MMREGISTER,
                 LOC_CMMREGISTER:
                   cg.a_parammm_ref(exprasmlist,left.location.size,left.location.reference,tempcgpara,mms_movescalar);
{$ifdef x86_64}
                 { x86_64 pushes s64comp in normal register }
                 LOC_REGISTER,
                 LOC_CREGISTER :
                   begin
                     { force integer size }
                     left.location.size:=int_cgsize(tcgsize2size[left.location.size]);
                     cg.a_param_ref(exprasmlist,left.location.size,left.location.reference,tempcgpara);
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
                   cg.a_paramfpu_ref(exprasmlist,left.location.size,left.location.reference,tempcgpara);
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
{$ifdef i386}
              if tempcgpara.location^.loc<>LOC_REFERENCE then
                internalerror(200309292);
              if not (left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                internalerror(200204241);
              { push on stack }
              size:=align(left.resulttype.def.size,tempcgpara.alignment);
              if tempcgpara.location^.reference.index=NR_STACK_POINTER_REG then
                begin
                  cg.g_stackpointer_alloc(exprasmlist,size);
                  reference_reset_base(href,NR_STACK_POINTER_REG,0);
                end
              else
                reference_reset_base(href,tempcgpara.location^.reference.index,tempcgpara.location^.reference.offset);
              cg.g_concatcopy(exprasmlist,left.location.reference,href,size);
{$else i386}
              cg.a_param_copy_ref(exprasmlist,left.resulttype.def.size,left.location.reference,tempcgpara);
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
                      cg64.a_param64_loc(exprasmlist,left.location,tempcgpara)
                    else
{$endif cpu64bit}
                      cg.a_param_loc(exprasmlist,left.location,tempcgpara);
                  end;
{$ifdef SUPPORT_MMX}
                LOC_MMXREGISTER,
                LOC_CMMXREGISTER:
                  cg.a_parammm_reg(exprasmlist,left.location.register);
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

             if not(assigned(aktcallnode.inlinecode)) then
               paramanager.createtempparaloc(exprasmlist,aktcallnode.procdefinition.proccalloption,paraitem,tempcgpara)
             else
               paramanager.duplicateparaloc(exprasmlist,aktcallnode.procdefinition.proccalloption,paraitem,tempcgpara);

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
                    cg.a_param_loc(exprasmlist,left.location,tempcgpara)
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
                      { Passing a var parameter to a var parameter, we can
                        just push the address transparently }
                      if (left.nodetype=loadn) and
                         (tloadnode(left).is_addr_param_load) then
                        begin
                          if (left.location.reference.index<>NR_NO) or
                             (left.location.reference.offset<>0) then
                            internalerror(200410107);
                          cg.a_param_reg(exprasmlist,OS_ADDR,left.location.reference.base,tempcgpara)
                        end
                      else
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
                        end;
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
        cgsize    : tcgsize;
        retloc    : tlocation;
        hregister : tregister;
        tempnode  : tnode;
        resultparaloc : pcgparalocation;
      begin
        resultparaloc:=procdefinition.funcret_paraloc[callerside].location;
        cgsize:=procdefinition.funcret_paraloc[callerside].size;

        { structured results are easy to handle....
          needed also when result_no_used !! }
        if (procdefinition.proctypeoption<>potype_constructor) and
           paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
          begin
            { Location should be setup by the funcret para }
            if location.loc<>LOC_REFERENCE then
             internalerror(200304241);
          end
        else
          { ansi/widestrings must be registered, so we can dispose them }
          if resulttype.def.needs_inittable then
            begin
              if resultparaloc^.loc<>LOC_REGISTER then
                internalerror(200409261);
              { the FUNCTION_RESULT_REG is already allocated }
              if getsupreg(resultparaloc^.register)<first_mm_imreg then
                cg.ungetcpuregister(exprasmlist,resultparaloc^.register);
              if not assigned(funcretnode) then
                begin
                  location_reset(location,LOC_REFERENCE,OS_ADDR);
                  location.reference:=refcountedtemp;
                  cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,NR_FUNCTION_RESULT_REG,location.reference);
                end
              else
                begin
                  hregister := cg.getaddressregister(exprasmlist);
                  cg.a_load_reg_reg(exprasmlist,OS_ADDR,OS_ADDR,resultparaloc^.register,hregister);
                  { in case of a regular funcretnode with ret_in_param, the }
                  { original funcretnode isn't touched -> make sure it's    }
                  { the same here (not sure if it's necessary)              }
                  tempnode := funcretnode.getcopy;
                  tempnode.pass_2;
                  location := tempnode.location;
                  tempnode.free;
                  cg.g_decrrefcount(exprasmlist,resulttype.def,location.reference);
                  cg.a_load_reg_ref(exprasmlist,OS_ADDR,OS_ADDR,hregister,location.reference);
               end;
            end
        else
          { normal (ordinal,float,pointer) result value }
          begin
            { we have only to handle the result if it is used }
            if (cnf_return_value_used in callnodeflags) then
              begin
                location.loc:=resultparaloc^.loc;
                case resultparaloc^.loc of
                   LOC_FPUREGISTER:
                     begin
                       location_reset(location,LOC_FPUREGISTER,cgsize);
                       location.register:=resultparaloc^.register;
{$ifdef x86}
                       tcgx86(cg).inc_fpu_stack;
{$else x86}
                       if getsupreg(resultparaloc^.register)<first_fpu_imreg then
                         cg.ungetcpuregister(exprasmlist,resultparaloc^.register);
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
                             procdefinition.funcret_paraloc[callerside].get_location(retloc);
                             if retloc.loc<>LOC_REGISTER then
                               internalerror(200409141);
                             { the function result registers are already allocated }
                             if getsupreg(retloc.registerlow)<first_int_imreg then
                               cg.ungetcpuregister(exprasmlist,retloc.registerlow);
                             location.registerlow:=cg.getintregister(exprasmlist,OS_32);
                             cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,retloc.registerlow,location.registerlow);
                             if getsupreg(retloc.registerhigh)<first_int_imreg then
                               cg.ungetcpuregister(exprasmlist,retloc.registerhigh);
                             location.registerhigh:=cg.getintregister(exprasmlist,OS_32);
                             cg.a_load_reg_reg(exprasmlist,OS_32,OS_32,retloc.registerhigh,location.registerhigh);
                           end
                          else
{$endif cpu64bit}
                           begin
                             { change register size after the unget because the
                               getregister was done for the full register
                               def_cgsize(resulttype.def) is used here because
                               it could be a constructor call }
                             if getsupreg(resultparaloc^.register)<first_int_imreg then
                               cg.ungetcpuregister(exprasmlist,resultparaloc^.register);
                             location.register:=cg.getintregister(exprasmlist,def_cgsize(resulttype.def));
                             cg.a_load_reg_reg(exprasmlist,cgsize,def_cgsize(resulttype.def),resultparaloc^.register,location.register);
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
                       if getsupreg(resultparaloc^.register)<first_mm_imreg then
                         cg.ungetcpuregister(exprasmlist,resultparaloc^.register);
                       location.register:=cg.getmmregister(exprasmlist,cgsize);
                       cg.a_loadmm_reg_reg(exprasmlist,cgsize,cgsize,resultparaloc^.register,location.register,mms_movescalar);
                     end;

                   else
                     internalerror(200405023);
                end;
              end
            else
              begin
{$ifdef x86}
                { release FPU stack }
                if resultparaloc^.loc=LOC_FPUREGISTER then
                  emit_reg(A_FSTP,S_NO,NR_FPU_RESULT_REG);
{$endif x86}
                if cgsize<>OS_NO then
                  paramanager.freeparaloc(exprasmlist,procdefinition.funcret_paraloc[callerside]);
                location_reset(location,LOC_VOID,OS_NO);
              end;
           end;

        { When the result is not used we need to finalize the result and
          can release the temp }
        if not(cnf_return_value_used in callnodeflags) then
          begin
            if location.loc=LOC_REFERENCE then
              begin
                if resulttype.def.needs_inittable then
                  cg.g_finalize(exprasmlist,resulttype.def,location.reference);
                tg.ungetiftemp(exprasmlist,location.reference)
              end;
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
         callerparaloc,
         tmpparaloc : pcgparalocation;
{$ifdef cputargethasfixedstack}
         htempref,
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
                   are saved temporary in registers, checking for the tmpparaloc.loc is wrong
                 }
                 if not assigned(inlinecode) then
                   paramanager.freeparaloc(exprasmlist,ppn.tempcgpara);
                 tmpparaloc:=ppn.tempcgpara.location;
                 callerparaloc:=ppn.paraitem.paraloc[callerside].location;
                 while assigned(callerparaloc) do
                   begin
                     { Every paraloc must have a matching tmpparaloc }
                     if not assigned(tmpparaloc) then
                       internalerror(200408224);
                     if callerparaloc^.size<>tmpparaloc^.size then
                       internalerror(200408225);
                     case callerparaloc^.loc of
                       LOC_REGISTER:
                         begin
                           if tmpparaloc^.loc<>LOC_REGISTER then
                             internalerror(200408221);
                           if getsupreg(callerparaloc^.register)<first_int_imreg then
                             cg.getcpuregister(exprasmlist,callerparaloc^.register);
                           cg.a_load_reg_reg(exprasmlist,tmpparaloc^.size,tmpparaloc^.size,
                               tmpparaloc^.register,callerparaloc^.register);
                         end;
                       LOC_FPUREGISTER:
                         begin
                           if tmpparaloc^.loc<>LOC_FPUREGISTER then
                             internalerror(200408222);
                           if getsupreg(callerparaloc^.register)<first_fpu_imreg then
                             cg.getcpuregister(exprasmlist,callerparaloc^.register);
                           cg.a_loadfpu_reg_reg(exprasmlist,ppn.tempcgpara.size,tmpparaloc^.register,callerparaloc^.register);
                         end;
                       LOC_MMREGISTER:
                         begin
                           if tmpparaloc^.loc<>LOC_MMREGISTER then
                             internalerror(200408223);
                           if getsupreg(callerparaloc^.register)<first_mm_imreg then
                             cg.getcpuregister(exprasmlist,callerparaloc^.register);
                           cg.a_loadmm_reg_reg(exprasmlist,tmpparaloc^.size,tmpparaloc^.size,
                             tmpparaloc^.register,callerparaloc^.register,mms_movescalar);
                         end;
                       LOC_REFERENCE:
                         begin
                           if not assigned(inlinecode) then
                             begin
{$ifdef cputargethasfixedstack}
                               reference_reset_base(href,callerparaloc^.reference.index,callerparaloc^.reference.offset);
                               { copy parameters in case they were moved to a temp. location because we've a fixed stack }
                               case tmpparaloc^.loc of
                                 LOC_REFERENCE:
                                   begin
                                     reference_reset_base(htempref,tmpparaloc^.reference.index,tmpparaloc^.reference.offset);
                                     { use concatcopy, because it can also be a float which fails when
                                       load_ref_ref is used }
                                     cg.g_concatcopy(exprasmlist,htempref,href,tcgsize2size[tmpparaloc^.size]);
                                   end;
                                 LOC_REGISTER:
                                   cg.a_load_reg_ref(exprasmlist,tmpparaloc^.size,tmpparaloc^.size,tmpparaloc^.register,href);
                                 LOC_FPUREGISTER:
                                   cg.a_loadfpu_reg_ref(exprasmlist,tmpparaloc^.size,tmpparaloc^.register,href);
                                 LOC_MMREGISTER:
                                   cg.a_loadmm_reg_ref(exprasmlist,tmpparaloc^.size,tmpparaloc^.size,tmpparaloc^.register,href,mms_movescalar);
                                 else
                                   internalerror(200402081);
                               end;
{$endif cputargethasfixedstack}
                             end;
                         end;
                     end;
                     callerparaloc:=callerparaloc^.next;
                     tmpparaloc:=tmpparaloc^.next;
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
                (ppn.paraitem.paraloc[callerside].location^.loc <> LOC_REFERENCE) then
               paramanager.freeparaloc(exprasmlist,ppn.paraitem.paraloc[callerside]);
             ppn:=tcgcallparanode(ppn.right);
           end;
       end;



    procedure tcgcallnode.normal_pass_2;
      var
         regs_to_save_int,
         regs_to_save_fpu,
         regs_to_save_mm   : Tcpuregisterset;
         href : treference;
         pop_size : longint;
         pvreg,
         vmtreg : tregister;
         oldaktcallnode : tcallnode;
         funcretloc : pcgparalocation;
      begin
         if not assigned(procdefinition) or
            not procdefinition.has_paraloc_info then
           internalerror(200305264);

         if resulttype.def.needs_inittable and
            not paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) and
            not assigned(funcretnode) then
           begin
             tg.gettemptyped(exprasmlist,resulttype.def,tt_normal,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp);
           end;

        regs_to_save_int:=paramanager.get_volatile_registers_int(procdefinition.proccalloption);
        regs_to_save_fpu:=paramanager.get_volatile_registers_fpu(procdefinition.proccalloption);
        regs_to_save_mm:=paramanager.get_volatile_registers_mm(procdefinition.proccalloption);

        { Include Function result registers }
        if (not is_void(resulttype.def)) then
          begin
            funcretloc:=procdefinition.funcret_paraloc[callerside].location;
            while assigned(funcretloc) do
              begin
                case funcretloc^.loc of
                  LOC_REGISTER,
                  LOC_CREGISTER:
                    include(regs_to_save_int,getsupreg(funcretloc^.register));
                  LOC_FPUREGISTER,
                  LOC_CFPUREGISTER:
                    include(regs_to_save_fpu,getsupreg(funcretloc^.register));
                  LOC_MMREGISTER,
                  LOC_CMMREGISTER:
                    include(regs_to_save_mm,getsupreg(funcretloc^.register));
                end;
                funcretloc:=funcretloc^.next;
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

                   cg.alloccpuregisters(exprasmlist,R_INTREGISTER,regs_to_save_int);
                   if cg.uses_registers(R_FPUREGISTER) then
                     cg.alloccpuregisters(exprasmlist,R_FPUREGISTER,regs_to_save_fpu);
                   if cg.uses_registers(R_MMREGISTER) then
                     cg.alloccpuregisters(exprasmlist,R_MMREGISTER,regs_to_save_mm);

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

                  cg.alloccpuregisters(exprasmlist,R_INTREGISTER,regs_to_save_int);
                  if cg.uses_registers(R_FPUREGISTER) then
                    cg.alloccpuregisters(exprasmlist,R_FPUREGISTER,regs_to_save_fpu);
                  if cg.uses_registers(R_MMREGISTER) then
                    cg.alloccpuregisters(exprasmlist,R_MMREGISTER,regs_to_save_mm);

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

              cg.alloccpuregisters(exprasmlist,R_INTREGISTER,regs_to_save_int);
              if cg.uses_registers(R_FPUREGISTER) then
                cg.alloccpuregisters(exprasmlist,R_FPUREGISTER,regs_to_save_fpu);
              if cg.uses_registers(R_MMREGISTER) then
                cg.alloccpuregisters(exprasmlist,R_MMREGISTER,regs_to_save_mm);

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
         if (not is_void(resulttype.def)) then
           begin
            funcretloc:=procdefinition.funcret_paraloc[callerside].location;
            while assigned(funcretloc) do
              begin
                case funcretloc^.loc of
                  LOC_REGISTER,
                  LOC_CREGISTER:
                    exclude(regs_to_save_int,getsupreg(funcretloc^.register));
                  LOC_FPUREGISTER,
                  LOC_CFPUREGISTER:
                    exclude(regs_to_save_fpu,getsupreg(funcretloc^.register));
                  LOC_MMREGISTER,
                  LOC_CMMREGISTER:
                    exclude(regs_to_save_mm,getsupreg(funcretloc^.register));
                end;
                funcretloc:=funcretloc^.next;
              end;
           end;
         if cg.uses_registers(R_MMREGISTER) then
           cg.dealloccpuregisters(exprasmlist,R_MMREGISTER,regs_to_save_mm);
         if cg.uses_registers(R_FPUREGISTER) then
           cg.dealloccpuregisters(exprasmlist,R_FPUREGISTER,regs_to_save_fpu);
         cg.dealloccpuregisters(exprasmlist,R_INTREGISTER,regs_to_save_int);

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
              cg.alloccpuregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
              cg.dealloccpuregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           end;

         { release temps of paras }
         release_para_temps;
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
         gen_alloc_inline_parast(exprasmlist,tprocdef(procdefinition));
         gen_alloc_inline_funcret(exprasmlist,tprocdef(procdefinition));
         gen_alloc_symtable(exprasmlist,tlocalsymtable(tprocdef(procdefinition).localst));

         { if we allocate the temp. location for ansi- or widestrings }
         { already here, we avoid later a push/pop                    }
         if resulttype.def.needs_inittable and
            not paramanager.ret_in_param(resulttype.def,procdefinition.proccalloption) then
           begin
             tg.gettemptyped(exprasmlist,resulttype.def,tt_normal,refcountedtemp);
             cg.g_decrrefcount(exprasmlist,resulttype.def,refcountedtemp);
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
         gen_initialize_code(inlineentrycode);
         if po_assembler in current_procinfo.procdef.procoptions then
           inlineentrycode.insert(Tai_marker.Create(asmblockstart));
         exprasmList.concatlist(inlineentrycode);

         { process the inline code }
         secondpass(inlinecode);

         cg.a_label(exprasmlist,current_procinfo.aktexitlabel);
         gen_finalize_code(inlineexitcode);
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
              cg.alloccpuregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(exprasmlist,'FPC_IOCHECK');
              cg.dealloccpuregisters(exprasmlist,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
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
                      cg.g_finalize(exprasmlist,resulttype.def,location.reference);
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
         gen_free_symtable(exprasmlist,tparasymtable(current_procinfo.procdef.parast));
         gen_free_symtable(exprasmlist,tlocalsymtable(current_procinfo.procdef.localst));

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
  Revision 1.180  2004-10-24 11:53:45  peter
    * fixed compilation with removed loadref

  Revision 1.179  2004/10/24 11:44:28  peter
    * small regvar fixes
    * loadref parameter removed from concatcopy,incrrefcount,etc

  Revision 1.178  2004/10/15 09:14:16  mazen
  - remove $IFDEF DELPHI and related code
  - remove $IFDEF FPCPROCVAR and related code

  Revision 1.177  2004/10/10 20:21:18  peter
    * passing a var parameter to var parameter is now also allowed
      for register locations (=regvars)

  Revision 1.176  2004/09/27 15:15:20  peter
    * dealloc function result registers, register allocation is now
      back at pre-paraloc level

  Revision 1.175  2004/09/25 14:23:54  peter
    * ungetregister is now only used for cpuregisters, renamed to
      ungetcpuregister
    * renamed (get|unget)explicitregister(s) to ..cpuregister
    * removed location-release/reference_release

  Revision 1.174  2004/09/21 17:25:12  peter
    * paraloc branch merged

  Revision 1.173.4.3  2004/09/20 20:46:34  peter
    * register allocation optimized for 64bit loading of parameters
      and return values

  Revision 1.173.4.2  2004/09/17 17:19:26  peter
    * fixed 64 bit unaryminus for sparc
    * fixed 64 bit inlining
    * signness of not operation

  Revision 1.173.4.1  2004/08/31 20:43:06  peter
    * paraloc patch

  Revision 1.173  2004/07/12 10:47:42  michael
  + Fix for bug 3207 from Peter

  Revision 1.172  2004/07/11 19:01:13  peter
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
