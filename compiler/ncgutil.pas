{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Helper routines for all code generators

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
unit ncgutil;

{$i fpcdefs.inc}

interface

    uses
      node,cpuinfo,
      globtype,
      cpubase,cgbase,
      aasmbase,aasmtai,aasmcpu,
      symconst,symbase,symdef,symsym,symtype,symtable
{$ifndef cpu64bit}
      ,cg64f32
{$endif cpu64bit}
      ;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

    procedure firstcomplex(p : tbinarynode);
    procedure maketojumpbool(list:TAAsmoutput; p : tnode; loadregvars: tloadregvars);
//    procedure remove_non_regvars_from_loc(const t: tlocation; var regs:Tsuperregisterset);

    procedure location_force_reg(list:TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
    procedure location_force_fpureg(list:TAAsmoutput;var l: tlocation;maybeconst:boolean);
    procedure location_force_mem(list:TAAsmoutput;var l:tlocation);
    procedure location_force_mmregscalar(list:TAAsmoutput;var l: tlocation;maybeconst:boolean);

    function  maybe_pushfpu(list:taasmoutput;needed : byte;var l:tlocation) : boolean;

    procedure gen_proc_symbol(list:Taasmoutput);
    procedure gen_proc_symbol_end(list:Taasmoutput);
    procedure gen_proc_entry_code(list:Taasmoutput);
    procedure gen_proc_exit_code(list:Taasmoutput);
    procedure gen_save_used_regs(list:TAAsmoutput);
    procedure gen_restore_used_regs(list:TAAsmoutput;const funcretparaloc:tparalocation);
    procedure gen_initialize_code(list:TAAsmoutput;inlined:boolean);
    procedure gen_finalize_code(list:TAAsmoutput;inlined:boolean);
    procedure gen_entry_code(list:TAAsmoutput);
    procedure gen_exit_code(list:TAAsmoutput);
    procedure gen_load_para_value(list:TAAsmoutput);
    procedure gen_load_return_value(list:TAAsmoutput);

(*
    procedure geninlineentrycode(list:TAAsmoutput;stackframe:longint);
    procedure geninlineexitcode(list:TAAsmoutput;inlined:boolean);
*)

   {#
      Allocate the buffers for exception management and setjmp environment.
      Return a pointer to these buffers, send them to the utility routine
      so they are registered, and then call setjmp.

      Then compare the result of setjmp with 0, and if not equal
      to zero, then jump to exceptlabel.

      Also store the result of setjmp to a temporary space by calling g_save_exception_reason

      It is to note that this routine may be called *after* the stackframe of a
      routine has been called, therefore on machines where the stack cannot
      be modified, all temps should be allocated on the heap instead of the
      stack.
    }

    const

      EXCEPT_BUF_SIZE = 3*sizeof(aint);
    type
      texceptiontemps=record
        jmpbuf,
        envbuf,
        reasonbuf  : treference;
      end;

    procedure get_exception_temps(list:taasmoutput;var t:texceptiontemps);
    procedure unget_exception_temps(list:taasmoutput;const t:texceptiontemps);
    procedure new_exception(list:TAAsmoutput;const t:texceptiontemps;a:aint;exceptlabel:tasmlabel);
    procedure free_exception(list:TAAsmoutput;const t:texceptiontemps;a:aint;endexceptlabel:tasmlabel;onlyfree:boolean);

    procedure insertconstdata(sym : ttypedconstsym);
    procedure insertbssdata(sym : tvarsym);

    procedure gen_alloc_localst(list:TAAsmoutput;st:tlocalsymtable);
    procedure gen_free_localst(list:TAAsmoutput;st:tlocalsymtable);
    procedure gen_alloc_parast(list:TAAsmoutput;st:tparasymtable);
    procedure gen_alloc_inline_parast(list:TAAsmoutput;st:tparasymtable);
    procedure gen_free_parast(list:TAAsmoutput;st:tparasymtable);

    { rtti and init/final }
    procedure generate_rtti(p:Ttypesym);
    procedure generate_inittable(p:tsym);


implementation

  uses
{$ifdef Delphi}
    Sysutils,
{$else}
    strings,
{$endif}
    cutils,cclasses,
    globals,systems,verbose,
    ppu,defutil,
    procinfo,paramgr,fmodule,
    regvars,dwarf,
{$ifdef GDB}
    gdb,
{$endif GDB}
    pass_1,pass_2,
    ncon,nld,nutils,
    tgobj,cgutils,cgobj;


{*****************************************************************************
                                  Misc Helpers
*****************************************************************************}

   { DO NOT RELY on the fact that the tnode is not yet swaped
     because of inlining code PM }
    procedure firstcomplex(p : tbinarynode);
      var
         hp : tnode;
      begin
         { always calculate boolean AND and OR from left to right }
         if (p.nodetype in [orn,andn]) and
            is_boolean(p.left.resulttype.def) then
           begin
             if nf_swaped in p.flags then
               internalerror(234234);
           end
         else
           if (
               (p.location.loc=LOC_FPUREGISTER) and
               (p.right.registersfpu > p.left.registersfpu)
              ) or
              (
               (
                (
                 ((p.left.registersfpu = 0) and (p.right.registersfpu = 0)) or
                 (p.location.loc<>LOC_FPUREGISTER)
                ) and
                (p.left.registersint<p.right.registersint)
               )
              ) then
            begin
              hp:=p.left;
              p.left:=p.right;
              p.right:=hp;
              if nf_swaped in p.flags then
                exclude(p.flags,nf_swaped)
              else
                include(p.flags,nf_swaped);
            end;
      end;


    procedure maketojumpbool(list:TAAsmoutput; p : tnode; loadregvars: tloadregvars);
    {
      produces jumps to true respectively false labels using boolean expressions

      depending on whether the loading of regvars is currently being
      synchronized manually (such as in an if-node) or automatically (most of
      the other cases where this procedure is called), loadregvars can be
      "lr_load_regvars" or "lr_dont_load_regvars"
    }
      var
        opsize : tcgsize;
        storepos : tfileposinfo;
      begin
         if nf_error in p.flags then
           exit;
         storepos:=aktfilepos;
         aktfilepos:=p.fileinfo;
         if is_boolean(p.resulttype.def) then
           begin
{$ifdef OLDREGVARS}
              if loadregvars = lr_load_regvars then
                load_all_regvars(list);
{$endif OLDREGVARS}
              if is_constboolnode(p) then
                begin
                   if tordconstnode(p).value<>0 then
                     cg.a_jmp_always(list,truelabel)
                   else
                     cg.a_jmp_always(list,falselabel)
                end
              else
                begin
                   opsize:=def_cgsize(p.resulttype.def);
                   case p.location.loc of
                     LOC_CREGISTER,LOC_REGISTER,LOC_CREFERENCE,LOC_REFERENCE :
                       begin
{$ifdef OLDREGVARS}
                         if (p.location.loc = LOC_CREGISTER) then
                           load_regvar_reg(list,p.location.register);
{$endif OLDREGVARS}
                         cg.a_cmp_const_loc_label(list,opsize,OC_NE,
                           0,p.location,truelabel);
                         { !!! should happen right after cmp (JM) }
                         location_release(list,p.location);
                         cg.a_jmp_always(list,falselabel);
                       end;
                     LOC_JUMP:
                       ;
{$ifdef cpuflags}
                     LOC_FLAGS :
                       begin
                         cg.a_jmp_flags(list,p.location.resflags,
                           truelabel);
                         cg.a_jmp_always(list,falselabel);
                       end;
{$endif cpuflags}
                     else
                       begin
                         printnode(output,p);
                         internalerror(200308241);
                       end;
                   end;
                end;
           end
         else
           internalerror(200112305);
         aktfilepos:=storepos;
      end;


        (*
        This code needs fixing. It is not safe to use rgint; on the m68000 it
        would be rgaddr.

    procedure remove_non_regvars_from_loc(const t: tlocation; var regs:Tsuperregisterset);
      begin
        case t.loc of
          LOC_REGISTER:
            begin
              { can't be a regvar, since it would be LOC_CREGISTER then }
              exclude(regs,getsupreg(t.register));
              if t.registerhigh<>NR_NO then
                exclude(regs,getsupreg(t.registerhigh));
            end;
          LOC_CREFERENCE,LOC_REFERENCE:
            begin
              if not(cs_regvars in aktglobalswitches) or
                 (getsupreg(t.reference.base) in cg.rgint.usableregs) then
                exclude(regs,getsupreg(t.reference.base));
              if not(cs_regvars in aktglobalswitches) or
                 (getsupreg(t.reference.index) in cg.rgint.usableregs) then
                exclude(regs,getsupreg(t.reference.index));
            end;
        end;
      end;
        *)


{*****************************************************************************
                            EXCEPTION MANAGEMENT
*****************************************************************************}

    procedure get_exception_temps(list:taasmoutput;var t:texceptiontemps);
      begin
        tg.GetTemp(list,EXCEPT_BUF_SIZE,tt_persistent,t.envbuf);
        tg.GetTemp(list,JMP_BUF_SIZE,tt_persistent,t.jmpbuf);
        tg.GetTemp(list,sizeof(aint),tt_persistent,t.reasonbuf);
      end;


    procedure unget_exception_temps(list:taasmoutput;const t:texceptiontemps);
      begin
        tg.Ungettemp(list,t.jmpbuf);
        tg.ungettemp(list,t.envbuf);
        tg.ungettemp(list,t.reasonbuf);
      end;


    procedure new_exception(list:TAAsmoutput;const t:texceptiontemps;a:aint;exceptlabel:tasmlabel);
      var
        paraloc1,paraloc2,paraloc3 : tparalocation;
      begin
        paraloc1:=paramanager.getintparaloc(pocall_default,1);
        paraloc2:=paramanager.getintparaloc(pocall_default,2);
        paraloc3:=paramanager.getintparaloc(pocall_default,3);
        paramanager.allocparaloc(list,paraloc3);
        cg.a_paramaddr_ref(list,t.envbuf,paraloc3);
        paramanager.allocparaloc(list,paraloc2);
        cg.a_paramaddr_ref(list,t.jmpbuf,paraloc2);
        { push type of exceptionframe }
        paramanager.allocparaloc(list,paraloc1);
        cg.a_param_const(list,OS_S32,1,paraloc1);
        paramanager.freeparaloc(list,paraloc3);
        paramanager.freeparaloc(list,paraloc2);
        paramanager.freeparaloc(list,paraloc1);
        cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cg.a_call_name(list,'FPC_PUSHEXCEPTADDR');
        cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));

        paraloc1:=paramanager.getintparaloc(pocall_default,1);
        paramanager.allocparaloc(list,paraloc1);
        cg.a_param_reg(list,OS_ADDR,NR_FUNCTION_RESULT_REG,paraloc1);
        paramanager.freeparaloc(list,paraloc1);
        cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
        cg.a_call_name(list,'FPC_SETJMP');
        cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));

        cg.g_exception_reason_save(list, t.reasonbuf);
        cg.a_cmp_const_reg_label(list,OS_S32,OC_NE,0,cg.makeregsize(list,NR_FUNCTION_RESULT_REG,OS_S32),exceptlabel);
     end;


    procedure free_exception(list:TAAsmoutput;const t:texceptiontemps;a:aint;endexceptlabel:tasmlabel;onlyfree:boolean);
     begin
         cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
         cg.a_call_name(list,'FPC_POPADDRSTACK');
         cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));

         if not onlyfree then
          begin
            cg.g_exception_reason_load(list, t.reasonbuf);
            cg.a_cmp_const_reg_label(list,OS_INT,OC_EQ,a,NR_FUNCTION_RESULT_REG,endexceptlabel);
          end;
     end;


{*****************************************************************************
                                     TLocation
*****************************************************************************}

{$ifndef cpu64bit}
    { 32-bit version }
    procedure location_force_reg(list:TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister,
        hregisterhi : tregister;
        hreg64 : tregister64;
        hl : tasmlabel;
        oldloc : tlocation;
        const_location: boolean;
     begin
        oldloc:=l;
        if dst_size=OS_NO then
          internalerror(200309144);
        { handle transformations to 64bit separate }
        if dst_size in [OS_64,OS_S64] then
         begin
           if not (l.size in [OS_64,OS_S64]) then
            begin
              { load a smaller size to OS_64 }
              if l.loc=LOC_REGISTER then
               begin
                 hregister:=cg.makeregsize(list,l.registerlow,OS_32);
                 cg.a_load_reg_reg(list,l.size,OS_32,l.registerlow,hregister);
               end
              else
               begin
                 location_release(list,l);
                 hregister:=cg.getintregister(list,OS_INT);
               end;
              { load value in low register }
              case l.loc of
                LOC_FLAGS :
                  cg.g_flags2reg(list,OS_INT,l.resflags,hregister);
                LOC_JUMP :
                  begin
                    cg.a_label(list,truelabel);
                    cg.a_load_const_reg(list,OS_INT,1,hregister);
                    objectlibrary.getlabel(hl);
                    cg.a_jmp_always(list,hl);
                    cg.a_label(list,falselabel);
                    cg.a_load_const_reg(list,OS_INT,0,hregister);
                    cg.a_label(list,hl);
                  end;
                else
                  cg.a_load_loc_reg(list,OS_INT,l,hregister);
              end;
              { reset hi part, take care of the signed bit of the current value }
              hregisterhi:=cg.getintregister(list,OS_INT);
              if (l.size in [OS_S8,OS_S16,OS_S32]) then
               begin
                 if l.loc=LOC_CONSTANT then
                  begin
                    if (longint(l.value)<0) then
                     cg.a_load_const_reg(list,OS_32,aint($ffffffff),hregisterhi)
                    else
                     cg.a_load_const_reg(list,OS_32,0,hregisterhi);
                  end
                 else
                  begin
                    cg.a_op_const_reg_reg(list,OP_SAR,OS_32,31,hregister,
                      hregisterhi);
                  end;
               end
              else
               cg.a_load_const_reg(list,OS_32,0,hregisterhi);
              location_reset(l,LOC_REGISTER,dst_size);
              l.registerlow:=hregister;
              l.registerhigh:=hregisterhi;
            end
           else
            begin
              { 64bit to 64bit }
              if (l.loc=LOC_REGISTER) or
                 ((l.loc=LOC_CREGISTER) and maybeconst) then
               begin
                 hregister:=l.registerlow;
                 hregisterhi:=l.registerhigh;
               end
              else
               begin
                 hregister:=cg.getintregister(list,OS_INT);
                 hregisterhi:=cg.getintregister(list,OS_INT);
                 location_release(list,l);
               end;
              hreg64.reglo:=hregister;
              hreg64.reghi:=hregisterhi;
              { load value in new register }
              cg64.a_load64_loc_reg(list,l,hreg64);
              location_reset(l,LOC_REGISTER,dst_size);
              l.registerlow:=hregister;
              l.registerhigh:=hregisterhi;
            end;
         end
        else
         begin
           {Do not bother to recycle the existing register. The register
            allocator eliminates unnecessary moves, so it's not needed
            and trying to recycle registers can cause problems because
            the registers changes size and may need aditional constraints.

            Not if it's about LOC_CREGISTER's (JM)
            }
           const_location :=
              (maybeconst) and
              (l.loc = LOC_CREGISTER) and
              (TCGSize2Size[l.size] = TCGSize2Size[dst_size]) and
              ((l.size = dst_size) or
               (TCGSize2Size[l.size] = TCGSize2Size[OS_INT]));
           if not const_location then
             begin
               location_release(list,l);
               hregister:=cg.getintregister(list,dst_size)
             end
           else
             hregister := l.register;
           { load value in new register }
           case l.loc of
             LOC_FLAGS :
               cg.g_flags2reg(list,dst_size,l.resflags,hregister);
             LOC_JUMP :
               begin
                 cg.a_label(list,truelabel);
                 cg.a_load_const_reg(list,dst_size,1,hregister);
                 objectlibrary.getlabel(hl);
                 cg.a_jmp_always(list,hl);
                 cg.a_label(list,falselabel);
                 cg.a_load_const_reg(list,dst_size,0,hregister);
                 cg.a_label(list,hl);
               end;
             else
               begin
                 { load_loc_reg can only handle size >= l.size, when the
                   new size is smaller then we need to adjust the size
                   of the orignal and maybe recalculate l.register for i386 }
                 if (TCGSize2Size[dst_size]<TCGSize2Size[l.size]) then
                  begin
                    if (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                      l.register:=cg.makeregsize(list,l.register,dst_size);
                    { for big endian systems, the reference's offset must }
                    { be increased in this case, since they have the      }
                    { MSB first in memory and e.g. byte(word_var) should  }
                    { return  the second byte in this case (JM)           }
                    if (target_info.endian = ENDIAN_BIG) and
                       (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                      inc(l.reference.offset,TCGSize2Size[l.size]-TCGSize2Size[dst_size]);
{$ifdef x86}
                   l.size:=dst_size;
{$endif x86}
                  end;
                 cg.a_load_loc_reg(list,dst_size,l,hregister);
{$ifndef x86}
                 if (TCGSize2Size[dst_size]<TCGSize2Size[l.size]) then
                   l.size:=dst_size;
{$endif not x86}
               end;
           end;
           if not const_location then
             location_reset(l,LOC_REGISTER,dst_size)
           else
             location_reset(l,LOC_CREGISTER,dst_size);
           l.register:=hregister;
         end;
       { Release temp when it was a reference }
       if oldloc.loc=LOC_REFERENCE then
         location_freetemp(list,oldloc);
     end;

{$else cpu64bit}

    { 64-bit version }
    procedure location_force_reg(list:TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister : tregister;
        hl : tasmlabel;
        oldloc : tlocation;
      begin
        oldloc:=l;
        {Do not bother to recycle the existing register. The register
         allocator eliminates unnecessary moves, so it's not needed
         and trying to recycle registers can cause problems because
         the registers changes size and may need aditional constraints.}
        location_release(list,l);
        hregister:=cg.getintregister(list,dst_size);
        { load value in new register }
        case l.loc of
          LOC_FLAGS :
            cg.g_flags2reg(list,dst_size,l.resflags,hregister);
          LOC_JUMP :
            begin
              cg.a_label(list,truelabel);
              cg.a_load_const_reg(list,dst_size,1,hregister);
              objectlibrary.getlabel(hl);
              cg.a_jmp_always(list,hl);
              cg.a_label(list,falselabel);
              cg.a_load_const_reg(list,dst_size,0,hregister);
              cg.a_label(list,hl);
            end;
          else
            begin
              { load_loc_reg can only handle size >= l.size, when the
                new size is smaller then we need to adjust the size
                of the orignal and maybe recalculate l.register for i386 }
              if (TCGSize2Size[dst_size]<TCGSize2Size[l.size]) then
               begin
                 if (l.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   l.register:=cg.makeregsize(list,l.register,dst_size);
                 { for big endian systems, the reference's offset must }
                 { be increased in this case, since they have the      }
                 { MSB first in memory and e.g. byte(word_var) should  }
                 { return  the second byte in this case (JM)           }
                 if (target_info.endian = ENDIAN_BIG) and
                    (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                   inc(l.reference.offset,TCGSize2Size[l.size]-TCGSize2Size[dst_size]);
{$ifdef x86}
                l.size:=dst_size;
{$endif x86}
               end;
              cg.a_load_loc_reg(list,dst_size,l,hregister);
{$ifndef x86}
              if (TCGSize2Size[dst_size]<TCGSize2Size[l.size]) then
                l.size:=dst_size;
{$endif not x86}
            end;
        end;
        if (l.loc <> LOC_CREGISTER) or
           not maybeconst then
          location_reset(l,LOC_REGISTER,dst_size)
        else
          location_reset(l,LOC_CREGISTER,dst_size);
        l.register:=hregister;
        { Release temp when it was a reference }
        if oldloc.loc=LOC_REFERENCE then
          location_freetemp(list,oldloc);
      end;
{$endif cpu64bit}


    procedure location_force_fpureg(list:TAAsmoutput;var l: tlocation;maybeconst:boolean);
      var
        reg : tregister;
        href : treference;
      begin
        if (l.loc<>LOC_FPUREGISTER)  and
           ((l.loc<>LOC_CFPUREGISTER) or (not maybeconst)) then
          begin
            { if it's in an mm register, store to memory first }
            if (l.loc in [LOC_MMREGISTER,LOC_CMMREGISTER]) then
              begin
                tg.GetTemp(list,tcgsize2size[l.size],tt_normal,href);
                cg.a_loadmm_reg_ref(list,l.size,l.size,l.register,href,mms_movescalar);
                location_release(list,l);
                location_reset(l,LOC_REFERENCE,l.size);
                l.reference:=href;
              end;
            reg:=cg.getfpuregister(list,l.size);
            cg.a_loadfpu_loc_reg(list,l,reg);
            location_freetemp(list,l);
            location_release(list,l);
            location_reset(l,LOC_FPUREGISTER,l.size);
            l.register:=reg;
          end;
      end;


    procedure location_force_mmregscalar(list:TAAsmoutput;var l: tlocation;maybeconst:boolean);
      var
        reg : tregister;
        href : treference;
      begin
        if (l.loc<>LOC_MMREGISTER)  and
           ((l.loc<>LOC_CMMREGISTER) or (not maybeconst)) then
          begin
            { if it's in an fpu register, store to memory first }
            if (l.loc in [LOC_FPUREGISTER,LOC_CFPUREGISTER]) then
              begin
                tg.GetTemp(list,tcgsize2size[l.size],tt_normal,href);
                cg.a_loadfpu_reg_ref(list,l.size,l.register,href);
                location_release(list,l);
                location_reset(l,LOC_REFERENCE,l.size);
                l.reference:=href;
              end;
            reg:=cg.getmmregister(list,l.size);
            cg.a_loadmm_loc_reg(list,l.size,l,reg,mms_movescalar);
            location_freetemp(list,l);
            location_release(list,l);
            location_reset(l,LOC_MMREGISTER,l.size);
            l.register:=reg;
          end;
      end;


    procedure location_force_mem(list:TAAsmoutput;var l:tlocation);
      var
        r : treference;
      begin
        case l.loc of
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              tg.GetTemp(list,TCGSize2Size[l.size],tt_normal,r);
              cg.a_loadfpu_reg_ref(list,l.size,l.register,r);
              location_release(list,l);
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_MMREGISTER,
          LOC_CMMREGISTER:
            begin
              tg.GetTemp(list,TCGSize2Size[l.size],tt_normal,r);
              cg.a_loadmm_reg_ref(list,l.size,l.size,l.register,r,mms_movescalar);
              location_release(list,l);
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_CONSTANT,
          LOC_REGISTER,
          LOC_CREGISTER :
            begin
              tg.GetTemp(list,TCGSize2Size[l.size],tt_normal,r);
{$ifndef cpu64bit}
              if l.size in [OS_64,OS_S64] then
                begin
                  cg64.a_load64_loc_ref(list,l,r);
                  location_release(list,l);
                end
              else
{$endif cpu64bit}
                begin
                  location_release(list,l);
                  cg.a_load_loc_ref(list,l.size,l,r);
                end;
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_CREFERENCE,
          LOC_REFERENCE : ;
          else
            internalerror(200203219);
        end;
      end;


{*****************************************************************************
                                  Maybe_Save
*****************************************************************************}

    function maybe_pushfpu(list:taasmoutput;needed : byte;var l:tlocation) : boolean;
      begin
{$ifdef i386}
        if (needed>=maxfpuregs) and
           (l.loc = LOC_FPUREGISTER) then
          begin
            location_force_mem(list,l);
            maybe_pushfpu:=true;
          end
        else
          maybe_pushfpu:=false;
{$else i386}
        maybe_pushfpu:=false;
{$endif i386}
      end;


{****************************************************************************
                            Init/Finalize Code
****************************************************************************}

    procedure copyvalueparas(p : tnamedindexitem;arg:pointer);
      var
        href1,href2 : treference;
        list:TAAsmoutput;
        hsym : tvarsym;
        l    : longint;
        loadref : boolean;
        localcopyloc : tparalocation;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           (tvarsym(p).varspez=vs_value) and
           (paramanager.push_addr_param(tvarsym(p).varspez,tvarsym(p).vartype.def,current_procinfo.procdef.proccalloption)) then
         begin
           loadref:=true;
           case tvarsym(p).localloc.loc of
             LOC_REGISTER :
               begin
                 reference_reset_base(href1,tvarsym(p).localloc.register,0);
                 loadref:=false;
               end;
             LOC_REFERENCE :
               reference_reset_base(href1,tvarsym(p).localloc.reference.index,
                   tvarsym(p).localloc.reference.offset);
             else
               internalerror(200309181);
           end;
           if is_open_array(tvarsym(p).vartype.def) or
              is_array_of_const(tvarsym(p).vartype.def) then
            begin
              { cdecl functions don't have a high pointer so it is not possible to generate
                a local copy }
              if not(current_procinfo.procdef.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
                begin
                  hsym:=tvarsym(tsym(p).owner.search('high'+p.name));
                  if not assigned(hsym) then
                    internalerror(200306061);
                  case hsym.localloc.loc of
                    LOC_REFERENCE :
                      begin
                        { this code is only executed before the code for the body and the entry/exit code is generated
                          so we're allowed to include pi_do_call here; after pass1 is run, this isn't allowed anymore
                        }
                        include(current_procinfo.flags,pi_do_call);
                        reference_reset_base(href2,hsym.localloc.reference.index,hsym.localloc.reference.offset);
                        cg.g_copyvaluepara_openarray(list,href1,href2,tarraydef(tvarsym(p).vartype.def).elesize)
                      end
                    else
                      internalerror(200309182);
                  end;
                end;
            end
           else
            begin
              if tvarsym(p).localloc.loc<>LOC_REFERENCE then
                internalerror(200309183);
              { Allocate space for the local copy }
              l:=tvarsym(p).getvaluesize;
              localcopyloc.loc:=LOC_REFERENCE;
              localcopyloc.size:=int_cgsize(l);
              tg.GetLocal(list,l,tvarsym(p).vartype.def,localcopyloc.reference);
              { Copy data }
              reference_reset_base(href2,localcopyloc.reference.index,localcopyloc.reference.offset);
              if is_shortstring(tvarsym(p).vartype.def) then
                begin
                  { this code is only executed before the code for the body and the entry/exit code is generated
                    so we're allowed to include pi_do_call here; after pass1 is run, this isn't allowed anymore
                  }
                  include(current_procinfo.flags,pi_do_call);
                  cg.g_copyshortstring(list,href1,href2,tstringdef(tvarsym(p).vartype.def).len,false,loadref)
                end
              else
                cg.g_concatcopy(list,href1,href2,tvarsym(p).vartype.def.size,true,loadref);
              { update localloc of varsym }
              tg.Ungetlocal(list,tvarsym(p).localloc.reference);
              tvarsym(p).localloc:=localcopyloc;
            end;
         end;
      end;


    { generates the code for initialisation of local data }
    procedure initialize_data(p : tnamedindexitem;arg:pointer);
      var
        oldexprasmlist : TAAsmoutput;
        hp : tnode;
      begin
        if (tsym(p).typ=varsym) and
           (tvarsym(p).refs>0) and
           not(is_class(tvarsym(p).vartype.def)) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           oldexprasmlist:=exprasmlist;
           exprasmlist:=taasmoutput(arg);
           hp:=initialize_data_node(cloadnode.create(tsym(p),tsym(p).owner));
           firstpass(hp);
           secondpass(hp);
           hp.free;
           exprasmlist:=oldexprasmlist;
         end;
      end;


    { generates the code for finalisation of local data }
    procedure finalize_data(p : tnamedindexitem;arg:pointer);
      var
        oldexprasmlist : TAAsmoutput;
        hp : tnode;
        dofinalize : boolean;
      begin
        dofinalize:=false;
        case tsym(p).typ of
          varsym :
            begin
              if (tvarsym(p).refs>0) and
                 not(vo_is_funcret in tvarsym(p).varoptions) and
                 not(is_class(tvarsym(p).vartype.def)) and
                 tvarsym(p).vartype.def.needs_inittable then
                dofinalize:=true;
            end;
{
          typedconstsym :
            begin
              if ttypedconstsym(p).is_writable and
                 ttypedconstsym(p).typedconsttype.def.needs_inittable then
                dofinalize:=true;
            end;
}
        end;
        if dofinalize then
          begin
            include(current_procinfo.flags,pi_needs_implicit_finally);
            oldexprasmlist:=exprasmlist;
            exprasmlist:=taasmoutput(arg);
            hp:=finalize_data_node(cloadnode.create(tsym(p),tsym(p).owner));
            firstpass(hp);
            secondpass(hp);
            hp.free;
            exprasmlist:=oldexprasmlist;
          end;
      end;


    { generates the code for incrementing the reference count of parameters and
      initialize out parameters }
    procedure init_paras(p : tnamedindexitem;arg:pointer);
      var
        href : treference;
        tmpreg : tregister;
        list:TAAsmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           not is_class(tvarsym(p).vartype.def) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           case tvarsym(p).varspez of
             vs_value :
               begin
                 if tvarsym(p).localloc.loc<>LOC_REFERENCE then
                   internalerror(200309187);
                 reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
                 cg.g_incrrefcount(list,tvarsym(p).vartype.def,href,is_open_array(tvarsym(p).vartype.def));
               end;
             vs_out :
               begin
                 case tvarsym(p).localloc.loc of
                   LOC_REFERENCE :
                     reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
                   else
                     internalerror(2003091810);
                 end;
                 tmpreg:=cg.getaddressregister(list);
                 cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,tmpreg);
                 reference_reset_base(href,tmpreg,0);
                 cg.g_initialize(list,tvarsym(p).vartype.def,href,false);
                 cg.ungetregister(list,tmpreg);
               end;
           end;
         end;
      end;


    { generates the code for decrementing the reference count of parameters }
    procedure final_paras(p : tnamedindexitem;arg:pointer);
      var
        href : treference;
        list:TAAsmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           not is_class(tvarsym(p).vartype.def) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           if (tvarsym(p).varspez=vs_value) then
            begin
              include(current_procinfo.flags,pi_needs_implicit_finally);
              if tvarsym(p).localloc.loc<>LOC_REFERENCE then
                internalerror(200309188);
              reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
              cg.g_decrrefcount(list,tvarsym(p).vartype.def,href,is_open_array(tvarsym(p).vartype.def));
            end;
         end
        else if (tsym(p).typ=varsym) and
          (tvarsym(p).varspez=vs_value) and
          (is_open_array(tvarsym(p).vartype.def) or
           is_array_of_const(tvarsym(p).vartype.def)) then
          begin
            { cdecl functions don't have a high pointer so it is not possible to generate
              a local copy }
            if not(current_procinfo.procdef.proccalloption in [pocall_cdecl,pocall_cppdecl]) then
              begin
                reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
                cg.g_releasevaluepara_openarray(list,href);
              end;
          end;
      end;


    { Initialize temp ansi/widestrings,interfaces }
    procedure inittempvariables(list:taasmoutput);
      var
        hp : ptemprecord;
        href : treference;
      begin
        hp:=tg.templist;
        while assigned(hp) do
         begin
           if assigned(hp^.def) and
              hp^.def.needs_inittable then
            begin
              reference_reset_base(href,current_procinfo.framepointer,hp^.pos);
              cg.g_initialize(list,hp^.def,href,false);
            end;
           hp:=hp^.next;
         end;
      end;


    procedure finalizetempvariables(list:taasmoutput);
      var
        hp : ptemprecord;
        href : treference;
      begin
        hp:=tg.templist;
        while assigned(hp) do
         begin
           if assigned(hp^.def) and
              hp^.def.needs_inittable then
            begin
              include(current_procinfo.flags,pi_needs_implicit_finally);
              reference_reset_base(href,current_procinfo.framepointer,hp^.pos);
              cg.g_finalize(list,hp^.def,href,false);
            end;
           hp:=hp^.next;
         end;
      end;


    procedure gen_load_return_value(list:TAAsmoutput);
      var
        ressym : tvarsym;
        resloc : tlocation;
        hreg   : tregister;
        resultloc : tparalocation;
      begin
        resultloc:=current_procinfo.procdef.funcret_paraloc[calleeside];
        { Is the loading needed? }
        if is_void(current_procinfo.procdef.rettype.def) or
           (
            (po_assembler in current_procinfo.procdef.procoptions) and
            (not(assigned(current_procinfo.procdef.funcretsym)) or
             (tvarsym(current_procinfo.procdef.funcretsym).refs=0))
           ) then
           exit;

        { constructors return self }
        if (current_procinfo.procdef.proctypeoption=potype_constructor) then
          ressym:=tvarsym(current_procinfo.procdef.parast.search('self'))
        else
          ressym := tvarsym(current_procinfo.procdef.funcretsym);
        if (ressym.refs>0) then
          begin
            case ressym.localloc.loc of
              LOC_FPUREGISTER:
                begin
                  location_reset(resloc,LOC_CFPUREGISTER,resultloc.size);
                  resloc.register:=ressym.localloc.register;
                end;

              LOC_REGISTER:
                begin
                  location_reset(resloc,LOC_CREGISTER,resultloc.size);
                  resloc.register:=ressym.localloc.register;
                end;

              LOC_MMREGISTER:
                begin
                  location_reset(resloc,LOC_CMMREGISTER,resultloc.size);
                  resloc.register:=ressym.localloc.register;
                end;

              LOC_REFERENCE:
                begin
                  location_reset(resloc,LOC_REFERENCE,resultloc.size);
                  reference_reset_base(resloc.reference,ressym.localloc.reference.index,ressym.localloc.reference.offset);
                end;
              else
                internalerror(200309184);
            end;

            { Here, we return the function result. In most architectures, the value is
              passed into the FUNCTION_RETURN_REG, but in a windowed architecure like sparc a
              function returns in a register and the caller receives it in an other one }
            case resultloc.loc of
              LOC_REGISTER:
                begin
{$ifndef cpu64bit}
                  if resloc.size in [OS_64,OS_S64] then
                    begin
                      cg.getexplicitregister(list,NR_FUNCTION_RETURN64_LOW_REG);
                      cg.getexplicitregister(list,NR_FUNCTION_RETURN64_HIGH_REG);
                      cg.ungetregister(list,NR_FUNCTION_RETURN64_LOW_REG);
                      cg.ungetregister(list,NR_FUNCTION_RETURN64_HIGH_REG);
                      // for the optimizer
                      cg.a_reg_alloc(list,NR_FUNCTION_RETURN64_LOW_REG);
                      cg.a_reg_alloc(list,NR_FUNCTION_RETURN64_HIGH_REG);
                      cg64.a_load64_loc_reg(list,resloc,joinreg64(NR_FUNCTION_RETURN64_LOW_REG,
                                            NR_FUNCTION_RETURN64_HIGH_REG));
                    end
                  else
{$endif cpu64bit}
                    begin
                      cg.getexplicitregister(list,resultloc.register);
                      hreg:=cg.makeregsize(list,resultloc.register,resloc.size);
                      cg.ungetregister(list,hreg);
                      // for the optimizer
                      cg.a_reg_alloc(list,resultloc.register);
                      cg.a_load_loc_reg(list,resloc.size,resloc,hreg);
                    end;
                end;
              LOC_FPUREGISTER:
                begin
                  cg.a_loadfpu_loc_reg(list,resloc,resultloc.register);
                end;
              LOC_MMREGISTER:
                begin
                  cg.a_loadmm_loc_reg(list,resloc.size,resloc,resultloc.register,mms_movescalar);
                end;
              LOC_INVALID,
              LOC_REFERENCE:
                ;
              else
                internalerror(200405025);
            end;
         end;
      end;


    procedure gen_load_para_value(list:TAAsmoutput);
      var
        hp : tparaitem;
        href : treference;
        gotregvarparas : boolean;
      begin
        { Store register parameters in reference or in register variable }
        if assigned(current_procinfo.procdef.parast) and
           not (po_assembler in current_procinfo.procdef.procoptions) then
          begin
            { move register parameters which aren't regable into memory                               }
            { we do this before init_paras because that one calls routines which may overwrite these  }
            { registers and it also expects the values to be in memory                                }
            hp:=tparaitem(current_procinfo.procdef.para.first);
            gotregvarparas := false;
            while assigned(hp) do
              begin
                case tvarsym(hp.parasym).localloc.loc of
                  LOC_REGISTER,
                  LOC_MMREGISTER,
                  LOC_FPUREGISTER:
                    begin
                      gotregvarparas := true;
                      { cg.a_load_param_reg will first allocate and then deallocate paraloc }
                      { register (if the parameter resides in a register) and then allocate }
                      { the regvar (which is currently not allocated)                       }
                      cg.a_loadany_param_reg(list,hp.paraloc[calleeside],tvarsym(hp.parasym).localloc.register,mms_movescalar);
                    end;
                  LOC_REFERENCE :
                    begin
                      if hp.paraloc[calleeside].loc<>LOC_REFERENCE then
                        begin
                          if getregtype(hp.paraloc[calleeside].register)=R_INTREGISTER then
                            begin
                              if getsupreg(hp.paraloc[calleeside].register)<first_int_imreg then
                                begin
{$ifndef cpu64bit}
                                  if (hp.paraloc[calleeside].size in [OS_S64,OS_64]) then
                                    begin
                                      cg.getexplicitregister(list,hp.paraloc[calleeside].registerlow);
                                      cg.getexplicitregister(list,hp.paraloc[calleeside].registerhigh);
                                    end
                                  else
{$endif cpu64bit}
                                    cg.getexplicitregister(list,hp.paraloc[calleeside].register);
                                end;
                              { Release parameter register }
{$ifndef cpu64bit}
                              if (hp.paraloc[calleeside].size in [OS_S64,OS_64]) then
                                begin
                                  cg.ungetregister(list,hp.paraloc[calleeside].registerlow);
                                  cg.ungetregister(list,hp.paraloc[calleeside].registerhigh);
                                end
                              else
{$endif cpu64bit}
                                cg.ungetregister(list,hp.paraloc[calleeside].register);
                            end;
                          reference_reset_base(href,tvarsym(hp.parasym).localloc.reference.index,tvarsym(hp.parasym).localloc.reference.offset);
                          cg.a_loadany_param_ref(list,hp.paraloc[calleeside],href,mms_movescalar);
                        end;
                    end;
                  else
                    internalerror(200309185);
                end;
                hp:=tparaitem(hp.next);
              end;
            if gotregvarparas then
              begin
                { deallocate all register variables again }
                hp:=tparaitem(current_procinfo.procdef.para.first);
                while assigned(hp) do
                  begin
                    if (tvarsym(hp.parasym).localloc.loc=LOC_REGISTER) then
                      cg.ungetregister(list,tvarsym(hp.parasym).localloc.register);
                    hp:=tparaitem(hp.next);
                  end;
              end;
          end;

        { generate copies of call by value parameters, must be done before
          the initialization and body is parsed because the refcounts are
          incremented using the local copies }
        if not(po_assembler in current_procinfo.procdef.procoptions) then
          current_procinfo.procdef.parast.foreach_static({$ifndef TP}@{$endif}copyvalueparas,list);
      end;


    procedure gen_initialize_code(list:TAAsmoutput;inlined:boolean);
      begin
        { initialize local data like ansistrings }
        case current_procinfo.procdef.proctypeoption of
           potype_unitinit:
             begin
                { this is also used for initialization of variables in a
                  program which does not have a globalsymtable }
                if assigned(current_module.globalsymtable) then
                  tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}initialize_data,list);
                tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}initialize_data,list);
             end;
           { units have seperate code for initilization and finalization }
           potype_unitfinalize: ;
           { program init/final is generated in separate procedure }
           potype_proginit: ;
           else
             current_procinfo.procdef.localst.foreach_static({$ifndef TP}@{$endif}initialize_data,list);
        end;

        { initialisizes temp. ansi/wide string data }
        inittempvariables(list);

        { initialize ansi/widesstring para's }
        current_procinfo.procdef.parast.foreach_static({$ifndef TP}@{$endif}init_paras,list);

{$ifdef OLDREGVARS}
        load_regvars(list,nil);
{$endif OLDREGVARS}
      end;


    procedure gen_finalize_code(list:TAAsmoutput;inlined:boolean);
      begin
{$ifdef OLDREGVARS}
        cleanup_regvars(list);
{$endif OLDREGVARS}

        { finalize temporary data }
        finalizetempvariables(list);

        { finalize local data like ansistrings}
        case current_procinfo.procdef.proctypeoption of
           potype_unitfinalize:
             begin
                { this is also used for initialization of variables in a
                  program which does not have a globalsymtable }
                if assigned(current_module.globalsymtable) then
                  tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data,list);
                tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data,list);
             end;
           { units/progs have separate code for initialization and finalization }
           potype_unitinit: ;
           { program init/final is generated in separate procedure }
           potype_proginit: ;
           else
             current_procinfo.procdef.localst.foreach_static({$ifndef TP}@{$endif}finalize_data,list);
        end;

        { finalize paras data }
        if assigned(current_procinfo.procdef.parast) then
          current_procinfo.procdef.parast.foreach_static({$ifndef TP}@{$endif}final_paras,list);
      end;


    procedure gen_entry_code(list:TAAsmoutput);
      var
        href : treference;
        paraloc1,
        paraloc2 : tparalocation;
        hp   : tused_unit;
      begin
        { the actual profile code can clobber some registers,
          therefore if the context must be saved, do it before
          the actual call to the profile code
        }
        if (cs_profile in aktmoduleswitches) and
           not(po_assembler in current_procinfo.procdef.procoptions) then
          begin
            { non-win32 can call mcout even in main }
            if not (target_info.system in [system_i386_win32,system_i386_wdosx]) or
               not (current_procinfo.procdef.proctypeoption=potype_proginit) then
              begin
                cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_cdecl));
                cg.g_profilecode(list);
                cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_cdecl));
              end;
          end;

        { call startup helpers from main program }
        if (current_procinfo.procdef.proctypeoption=potype_proginit) then
         begin
           { initialize profiling for win32 }
           if (target_info.system in [system_i386_win32,system_i386_wdosx]) and
              (cs_profile in aktmoduleswitches) then
            begin
              reference_reset_symbol(href,objectlibrary.newasmsymbol('etext',AB_EXTERNAL,AT_DATA),0);
              paraloc1:=paramanager.getintparaloc(pocall_default,1);
              paraloc2:=paramanager.getintparaloc(pocall_default,2);
              paramanager.allocparaloc(list,paraloc2);
              cg.a_paramaddr_ref(list,href,paraloc2);
              reference_reset_symbol(href,objectlibrary.newasmsymbol('__image_base__',AB_EXTERNAL,AT_DATA),0);
              paramanager.allocparaloc(list,paraloc1);
              cg.a_paramaddr_ref(list,href,paraloc1);
              paramanager.freeparaloc(list,paraloc2);
              paramanager.freeparaloc(list,paraloc1);
              cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_cdecl));
              cg.a_call_name(list,'_monstartup');
              cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_cdecl));
            end;

           { initialize units }
           cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
           cg.a_call_name(list,'FPC_INITIALIZEUNITS');
           cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));

{$ifdef GDB}
           if (cs_debuginfo in aktmoduleswitches) then
             begin
               { include reference to all debuginfo sections of used units }
               hp:=tused_unit(usedunits.first);
               while assigned(hp) do
                 begin
                   If (hp.u.flags and uf_has_debuginfo)=uf_has_debuginfo then
                     current_procinfo.aktlocaldata.concat(Tai_const.Createname(make_mangledname('DEBUGINFO',hp.u.globalsymtable,''),AT_DATA,0));
                   hp:=tused_unit(hp.next);
                 end;
               { include reference to debuginfo for this program }
               current_procinfo.aktlocaldata.concat(Tai_const.Createname(make_mangledname('DEBUGINFO',current_module.localsymtable,''),AT_DATA,0));
             end;
{$endif GDB}
         end;

{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
         list.concat(Tai_force_line.Create);
{$endif GDB}

{$ifdef OLDREGVARS}
        load_regvars(list,nil);
{$endif OLDREGVARS}
      end;


    procedure gen_exit_code(list:TAAsmoutput);
      begin
        { call __EXIT for main program }
        if (not DLLsource) and
           (current_procinfo.procdef.proctypeoption=potype_proginit) then
          cg.a_call_name(list,'FPC_DO_EXIT');
      end;


{****************************************************************************
                                Entry/Exit
****************************************************************************}

    procedure gen_proc_symbol(list:Taasmoutput);
      var
        hs : string;
      begin
        { add symbol entry point as well as debug information                 }
        { will be inserted in front of the rest of this list.                 }
        { Insert alignment and assembler names }
        { Align, gprof uses 16 byte granularity }
        if (cs_profile in aktmoduleswitches) then
          list.concat(Tai_align.create(16))
        else
          list.concat(Tai_align.create(aktalignment.procalign));

{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            if (po_public in current_procinfo.procdef.procoptions) then
              Tprocsym(current_procinfo.procdef.procsym).is_global:=true;
            current_procinfo.procdef.concatstabto(list);
            Tprocsym(current_procinfo.procdef.procsym).isstabwritten:=true;
          end;
{$endif GDB}

        repeat
          hs:=current_procinfo.procdef.aliasnames.getfirst;
          if hs='' then
            break;
{$ifdef GDB}
          if (cs_debuginfo in aktmoduleswitches) and
             target_info.use_function_relative_addresses then
          list.concat(Tai_stab_function_name.create(strpnew(hs)));
{$endif GDB}
          if (cs_profile in aktmoduleswitches) or
             (po_public in current_procinfo.procdef.procoptions) then
            list.concat(Tai_symbol.createname_global(hs,AT_FUNCTION,0))
          else
            list.concat(Tai_symbol.createname(hs,AT_FUNCTION,0));
        until false;
      end;



    procedure gen_proc_symbol_end(list:Taasmoutput);
{$ifdef GDB}
      var
        stabsendlabel : tasmlabel;
        mangled_length : longint;
        p : pchar;
{$endif GDB}
      begin
        list.concat(Tai_symbol_end.Createname(current_procinfo.procdef.mangledname));
{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) then
          begin
            objectlibrary.getlabel(stabsendlabel);
            cg.a_label(list,stabsendlabel);
            { define calling EBP as pseudo local var PM }
            { this enables test if the function is a local one !! }
            {if  assigned(current_procinfo.parent) and
                (current_procinfo.procdef.parast.symtablelevel>normal_function_level) then
              list.concat(Tai_stabs.Create(strpnew(
               '"parent_ebp:'+tstoreddef(voidpointertype.def).numberstring+'",'+
               tostr(N_LSYM)+',0,0,'+tostr(current_procinfo.parent_framepointer_offset)))); }

            if (not is_void(current_procinfo.procdef.rettype.def)) and
               (tvarsym(current_procinfo.procdef.funcretsym).refs>0) then
              begin
                if tvarsym(current_procinfo.procdef.funcretsym).localloc.loc=LOC_REFERENCE then
                  begin
{$warning Need to add gdb support for ret in param register calling}
                    if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
                      begin
                        list.concat(Tai_stabs.Create(strpnew(
                           '"'+current_procinfo.procdef.procsym.name+':X*'+tstoreddef(current_procinfo.procdef.rettype.def).numberstring+'",'+
                           tostr(N_tsym)+',0,0,'+tostr(tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset))));
                        if (m_result in aktmodeswitches) then
                          list.concat(Tai_stabs.Create(strpnew(
                             '"RESULT:X*'+tstoreddef(current_procinfo.procdef.rettype.def).numberstring+'",'+
                             tostr(N_tsym)+',0,0,'+tostr(tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset))))
                      end
                    else
                      begin
                        list.concat(Tai_stabs.Create(strpnew(
                           '"'+current_procinfo.procdef.procsym.name+':X'+tstoreddef(current_procinfo.procdef.rettype.def).numberstring+'",'+
                           tostr(N_tsym)+',0,0,'+tostr(tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset))));
                        if (m_result in aktmodeswitches) then
                          list.concat(Tai_stabs.Create(strpnew(
                             '"RESULT:X'+tstoreddef(current_procinfo.procdef.rettype.def).numberstring+'",'+
                             tostr(N_tsym)+',0,0,'+tostr(tvarsym(current_procinfo.procdef.funcretsym).localloc.reference.offset))));
                       end;
                  end;
              end;
            mangled_length:=length(current_procinfo.procdef.mangledname);
            getmem(p,2*mangled_length+50);
            strpcopy(p,'192,0,0,');
            strpcopy(strend(p),current_procinfo.procdef.mangledname);
            if (target_info.use_function_relative_addresses) then
              begin
                strpcopy(strend(p),'-');
                strpcopy(strend(p),current_procinfo.procdef.mangledname);
              end;
            list.concat(Tai_stabn.Create(strnew(p)));
            {List.concat(Tai_stabn.Create(strpnew('192,0,0,'
             +current_procinfo.procdef.mangledname))));
            p[0]:='2';p[1]:='2';p[2]:='4';
            strpcopy(strend(p),'_end');}
            strpcopy(p,'224,0,0,'+stabsendlabel.name);
            if (target_info.use_function_relative_addresses) then
              begin
                strpcopy(strend(p),'-');
                strpcopy(strend(p),current_procinfo.procdef.mangledname);
              end;
            list.concatlist(withdebuglist);
            list.concat(Tai_stabn.Create(strnew(p)));
             { strpnew('224,0,0,'
             +current_procinfo.procdef.mangledname+'_end'))));}
            freemem(p,2*mangled_length+50);
          end;
{$endif GDB}
      end;


    procedure gen_proc_entry_code(list:Taasmoutput);
      var
        hitemp,
        lotemp,
        stackframe : longint;
        check      : boolean;
        paraloc1   : tparalocation;
        href       : treference;
      begin
        { generate call frame marker for dwarf call frame info }
        dwarfcfi.start_frame(list);

        { allocate temp for saving the argument used when
          stack checking uses a register for pushing the stackframe size }
        check:=(cs_check_stack in aktlocalswitches) and (current_procinfo.procdef.proctypeoption<>potype_proginit);
        if check then
          begin
            { Allocate tempspace to store register parameter than
              is destroyed when calling stackchecking code }
            paraloc1:=paramanager.getintparaloc(pocall_default,1);
            if paraloc1.loc=LOC_REGISTER then
              tg.GetTemp(list,sizeof(aint),tt_normal,href);
          end;

        { Calculate size of stackframe }
        stackframe:=current_procinfo.calc_stackframe_size;

        { All temps are know, write offsets used for information }
        if (cs_asm_source in aktglobalswitches) then
          begin
            if tg.direction>0 then
              begin
                lotemp:=current_procinfo.tempstart;
                hitemp:=tg.lasttemp;
              end
            else
              begin
                lotemp:=tg.lasttemp;
                hitemp:=current_procinfo.tempstart;
              end;
            list.concat(Tai_comment.Create(strpnew('Temps allocated between '+std_regname(current_procinfo.framepointer)+
              tostr_with_plus(lotemp)+' and '+std_regname(current_procinfo.framepointer)+tostr_with_plus(hitemp))));
          end;

         { generate target specific proc entry code }
         cg.g_proc_entry(list,stackframe,(po_nostackframe in current_procinfo.procdef.procoptions));

         { Add stack checking code? }
         if check then
           begin
             { The tempspace to store original register is already
               allocated above before the stackframe size is calculated. }
             if paraloc1.loc=LOC_REGISTER then
               cg.a_load_reg_ref(list,OS_INT,OS_INT,paraloc1.register,href);
             paramanager.allocparaloc(list,paraloc1);
             cg.a_param_const(list,OS_INT,stackframe,paraloc1);
             paramanager.freeparaloc(list,paraloc1);
             cg.allocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
             cg.a_call_name(list,'FPC_STACKCHECK');
             cg.deallocexplicitregisters(list,R_INTREGISTER,paramanager.get_volatile_registers_int(pocall_default));
             if paraloc1.loc=LOC_REGISTER then
               begin
                 cg.a_load_ref_reg(list,OS_INT,OS_INT,href,paraloc1.register);
                 tg.UnGetTemp(list,href);
               end;
           end;
      end;


    procedure gen_proc_exit_code(list:Taasmoutput);
      var
        parasize : longint;
      begin
        { c style clearstack does not need to remove parameters from the stack, only the
          return value when it was pushed by arguments }
        if current_procinfo.procdef.proccalloption in clearstack_pocalls then
          begin
            parasize:=0;
            if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
              inc(parasize,sizeof(aint));
          end
        else
          parasize:=current_procinfo.para_stack_size;

        { generate target specific proc exit code }
        cg.g_proc_exit(list,parasize,(po_nostackframe in current_procinfo.procdef.procoptions));

        { release return registers, needed for optimizer }
        if current_procinfo.procdef.funcret_paraloc[calleeside].loc<>LOC_INVALID then
          paramanager.freeparaloc(list,
            current_procinfo.procdef.funcret_paraloc[calleeside]);

        { end of frame marker for call frame info }
        dwarfcfi.end_frame(list);
      end;


    procedure gen_save_used_regs(list:TAAsmoutput);
      begin
        { Pure assembler routines need to save the registers themselves }
        if (po_assembler in current_procinfo.procdef.procoptions) then
          exit;

        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in current_procinfo.procdef.procoptions) then
          cg.g_save_all_registers(list)
        else
          if current_procinfo.procdef.proccalloption in savestdregs_pocalls then
            cg.g_save_standard_registers(list);
      end;


    procedure gen_restore_used_regs(list:TAAsmoutput;const funcretparaloc:tparalocation);
      begin
        { Pure assembler routines need to save the registers themselves }
        if (po_assembler in current_procinfo.procdef.procoptions) then
          exit;

        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in current_procinfo.procdef.procoptions) then
          cg.g_restore_all_registers(list,funcretparaloc)
        else
          if current_procinfo.procdef.proccalloption in savestdregs_pocalls then
            cg.g_restore_standard_registers(list);
      end;


{****************************************************************************
                                 Inlining
****************************************************************************}

(*
    procedure load_inlined_return_value(list:TAAsmoutput);
      var
        ressym: tvarsym;
        resloc: tlocation;
        r,r2 : tregister;
      begin
        if not is_void(current_procinfo.procdef.rettype.def) then
         begin
           ressym := tvarsym(current_procinfo.procdef.funcretsym);
           if ressym.reg.enum <> R_NO then
             begin
               if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
                 location_reset(resloc,LOC_CREGISTER,OS_ADDR)
               else
                 if ressym.vartype.def.deftype = floatdef then
                   location_reset(resloc,LOC_CFPUREGISTER,def_cgsize(current_procinfo.procdef.rettype.def))
                 else
                   location_reset(resloc,LOC_CREGISTER,def_cgsize(current_procinfo.procdef.rettype.def));
               resloc.register := ressym.reg;
             end
           else
             begin
               location_reset(resloc,LOC_CREGISTER,def_cgsize(current_procinfo.procdef.rettype.def));
               reference_reset_base(resloc.reference,current_procinfo.framepointer,tvarsym(current_procinfo.procdef.funcretsym).adjusted_address);
             end;
           { Here, we return the function result. In most architectures, the value is
             passed into the FUNCTION_RETURN_REG, but in a windowed architecure like sparc a
             function returns in a register and the caller receives it in an other one }
           case current_procinfo.procdef.rettype.def.deftype of
             orddef,
             enumdef :
               begin
{$ifndef cpu64bit}
                 if resloc.size in [OS_64,OS_S64] then
                  begin
                    r:=cg.getregisterint(list,OS_INT);
                    r2:=cg.getregisterint(list,OS_INT);
                    cg64.a_load64_loc_reg(list,resloc,joinreg64(r,r2));
                  end
                 else
{$endif cpu64bit}
                  begin
                    r:=cg.getregisterint(list,resloc.size);
                    cg.a_load_loc_reg(list,resloc.size,resloc,r);
                  end;
               end;
             floatdef :
               begin
{$ifdef cpufpemu}
                  if cs_fp_emulation in aktmoduleswitches then
                    r.enum := FUNCTION_RETURN_REG
                 else
{$endif cpufpemu}
                  r.enum:=FPU_RESULT_REG;
                 cg.a_loadfpu_loc_reg(list,resloc,r);
               end;
             else
               begin
                 if not paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
                  begin
{$ifndef cpu64bit}
                    { Win32 can return records in EAX:EDX }
                    if resloc.size in [OS_64,OS_S64] then
                     begin
                       r:=cg.getregisterint(list,OS_INT);
                       r2:=cg.getregisterint(list,OS_INT);
                       cg64.a_load64_loc_reg(list,resloc,joinreg64(r,r2));
                     end
                    else
{$endif cpu64bit}
                     begin
                       r:=cg.getregisterint(list,resloc.size);
                       cg.a_load_loc_reg(list,resloc.size,resloc,r);
                     end;
                   end
               end;
           end;
         end;
      end;


    procedure geninlineentrycode(list:TAAsmoutput;stackframe:longint);
      begin
        { initialize return value }
        initretvalue(list);

        current_procinfo.procdef.localst.foreach_static({$ifndef TP}@{$endif}initialize_data,list);

        { initialisizes temp. ansi/wide string data }
        inittempvariables(list);

        { initialize ansi/widesstring para's }
        if assigned(current_procinfo.procdef.parast) then
          current_procinfo.procdef.parast.foreach_static({$ifndef TP}@{$endif}init_paras,list);

        { generate copies of call by value parameters }
        if not(po_assembler in current_procinfo.procdef.procoptions) then
          current_procinfo.procdef.parast.foreach_static({$ifndef TP}@{$endif}copyvalueparas,list);

        load_regvars(list,nil);
      end;


   procedure geninlineexitcode(list:TAAsmoutput;inlined:boolean);
      var
        usesacc,
        usesacchi,
        usesfpu : boolean;
      begin
        if aktexitlabel.is_used then
          cg.a_label(list,aktexitlabel);

        cleanup_regvars(list);

        { finalize temporary data }
        finalizetempvariables(list);

        current_procinfo.procdef.localst.foreach_static({$ifndef TP}@{$endif}finalize_data,list);

        { finalize paras data }
        if assigned(current_procinfo.procdef.parast) then
          current_procinfo.procdef.parast.foreach_static({$ifndef TP}@{$endif}final_paras,list);

        { handle return value, this is not done for assembler routines when
          they didn't reference the result variable }
        if not(po_assembler in current_procinfo.procdef.procoptions) or
           (assigned(current_procinfo.procdef.funcretsym) and
            (tvarsym(current_procinfo.procdef.funcretsym).refcount>1)) then
          begin
            if (current_procinfo.procdef.proctypeoption=potype_constructor) then
             internalerror(200305263);
//            load_inlined_return_value(list);
             load_return_value(list,usesacc,usesacchi,usesfpu)
          end;

        cleanup_regvars(list);
      end;
*)


{****************************************************************************
                               Const Data
****************************************************************************}

    procedure insertconstdata(sym : ttypedconstsym);
    { this does not affect the local stack space, since all
      typed constansts and initialized variables are always
      put in the .data / .rodata section
    }
      var
        storefilepos : tfileposinfo;
        curconstsegment : taasmoutput;
        l : longint;
      begin
        storefilepos:=aktfilepos;
        aktfilepos:=sym.fileinfo;
        if sym.is_writable then
          curconstsegment:=datasegment
        else
          curconstsegment:=consts;
        l:=sym.getsize;
        { insert cut for smartlinking or alignment }
        maybe_new_object_file(curconstSegment);
        new_section(curconstSegment,sec_rodata,lower(sym.mangledname),const_align(l));
        if (sym.owner.symtabletype=globalsymtable) or
           maybe_smartlink_symbol or
           (assigned(current_procinfo) and
            (current_procinfo.procdef.proccalloption=pocall_inline)) or
           DLLSource then
          curconstSegment.concat(Tai_symbol.Createname_global(sym.mangledname,AT_DATA,l))
        else
          curconstSegment.concat(Tai_symbol.Createname(sym.mangledname,AT_DATA,l));
        aktfilepos:=storefilepos;
      end;


    procedure insertbssdata(sym : tvarsym);
      var
        l,varalign : longint;
        storefilepos : tfileposinfo;
      begin
        storefilepos:=aktfilepos;
        aktfilepos:=sym.fileinfo;
        l:=sym.getvaluesize;
        if (vo_is_thread_var in sym.varoptions) then
          inc(l,sizeof(aint));
        varalign:=var_align(l);
        maybe_new_object_file(bssSegment);
        new_section(bssSegment,sec_bss,lower(sym.mangledname),varalign);
        if (sym.owner.symtabletype=globalsymtable) or
           maybe_smartlink_symbol or
           DLLSource or
           (assigned(current_procinfo) and
            (current_procinfo.procdef.proccalloption=pocall_inline)) or
           (vo_is_exported in sym.varoptions) or
           (vo_is_C_var in sym.varoptions) then
          bssSegment.concat(Tai_datablock.Create_global(sym.mangledname,l))
        else
          bssSegment.concat(Tai_datablock.Create(sym.mangledname,l));
        aktfilepos:=storefilepos;
      end;


    procedure gen_alloc_localst(list:TAAsmoutput;st:tlocalsymtable);
      var
        sym : tsym;
      begin
        sym:=tsym(st.symindex.first);
        while assigned(sym) do
          begin
            { Only allocate space for referenced locals }
            if (sym.typ=varsym) and
               (tvarsym(sym).refs>0) then
              begin
                with tvarsym(sym) do
                  begin
{$warning TODO Add support for register variables}
                    localloc.loc:=LOC_REFERENCE;
                    tg.GetLocal(list,getvaluesize,vartype.def,localloc.reference);
                    if cs_asm_source in aktglobalswitches then
                      list.concat(Tai_comment.Create(strpnew('Local '+realname+' located at '+
                          std_regname(localloc.reference.index)+tostr_with_plus(localloc.reference.offset))));
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_free_localst(list:TAAsmoutput;st:tlocalsymtable);
      var
        sym : tsym;
      begin
        sym:=tsym(st.symindex.first);
        while assigned(sym) do
          begin
            if (sym.typ=varsym) and
               (tvarsym(sym).refs>0) then
              begin
                with tvarsym(sym) do
                  begin
                    { Note: We need to keep the data available in memory
                      for the sub procedures that can access local data
                      in the parent procedures }
                    case localloc.loc of
                      LOC_REFERENCE :
                        tg.Ungetlocal(list,localloc.reference);
                      LOC_REGISTER :
                        begin
{$ifndef cpu64bit}
                          if localloc.size in [OS_64,OS_S64] then
                            begin
                              cg.ungetregister(list,localloc.registerlow);
                              cg.ungetregister(list,localloc.registerhigh);
                            end
                          else
{$endif cpu64bit}
                            cg.ungetregister(list,localloc.register);
                        end;
                    end;
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_alloc_parast(list:TAAsmoutput;st:tparasymtable);
      var
        sym : tsym;
      begin
        sym:=tsym(st.symindex.first);
        while assigned(sym) do
          begin
            if sym.typ=varsym then
              begin
                with tvarsym(sym) do
                  begin
                    if not(po_assembler in current_procinfo.procdef.procoptions) then
                      begin
                        case paraitem.paraloc[calleeside].loc of
                          LOC_MMREGISTER,
                          LOC_FPUREGISTER,
                          LOC_REGISTER:
                            begin
                              (*
                              if paraitem.paraloc[calleeside].register=NR_NO then
                                begin
                                  paraitem.paraloc[calleeside].loc:=LOC_REGISTER;
                                  paraitem.paraloc[calleeside].size:=paraitem.paraloc[calleeside].size;
{$ifndef cpu64bit}
                                  if paraitem.paraloc[calleeside].size in [OS_64,OS_S64] then
                                    begin
                                      paraitem.paraloc[calleeside].registerlow:=cg.getregisterint(list,OS_32);
                                      paraitem.paraloc[calleeside].registerhigh:=cg.getregisterint(list,OS_32);
                                    end
                                  else
{$endif cpu64bit}
                                    paraitem.paraloc[calleeside].register:=cg.getregisterint(list,localloc.size);
                                end;
                               *)
                              (*
{$warning TODO Allocate register paras}
                              localloc.loc:=LOC_REGISTER;
                             localloc.size:=paraitem.paraloc[calleeside].size;
{$ifndef cpu64bit}
                              if localloc.size in [OS_64,OS_S64] then
                                begin
                                  localloc.registerlow:=cg.getregisterint(list,OS_32);
                                  localloc.registerhigh:=cg.getregisterint(list,OS_32);
                                end
                              else
{$endif cpu64bit}
                                localloc.register:=cg.getregisterint(list,localloc.size);
                                *)
                              localloc.loc:=LOC_REFERENCE;
                              localloc.size:=paraitem.paraloc[calleeside].size;
                              tg.GetLocal(list,tcgsize2size[localloc.size],vartype.def,localloc.reference);
                            end;
{$ifdef powerpc}
                          LOC_REFERENCE:
                            begin
                              localloc.loc := LOC_REFERENCE;
                              localloc.size:=paraitem.paraloc[calleeside].size;
                              tg.GetLocal(list,tcgsize2size[localloc.size],vartype.def,localloc.reference);
                            end;
{$endif powerpc}
                          else
                            localloc:=paraitem.paraloc[calleeside];
                        end;
                      end
                    else
                      localloc:=paraitem.paraloc[calleeside];
                    if cs_asm_source in aktglobalswitches then
                      case localloc.loc of
                        LOC_REFERENCE :
                          begin
                            list.concat(Tai_comment.Create(strpnew('Para '+realname+' located at '+
                                std_regname(localloc.reference.index)+tostr_with_plus(localloc.reference.offset))));
                          end;
                      end;
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_alloc_inline_parast(list:TAAsmoutput;st:tparasymtable);
      var
        sym : tsym;
      begin
        if (po_assembler in current_procinfo.procdef.procoptions) then
          exit;
        sym:=tsym(st.symindex.first);
        while assigned(sym) do
          begin
            if sym.typ=varsym then
              begin
                with tvarsym(sym) do
                  begin
                    { for localloc <> LOC_REFERENCE, we need regvar support inside inlined procedures }
                    localloc.loc:=LOC_REFERENCE;
                    localloc.size:=int_cgsize(paramanager.push_size(varspez,vartype.def,pocall_inline));
                    tg.GetLocal(list,tcgsize2size[localloc.size],vartype.def,localloc.reference);
                    case paraitem.paraloc[calleeside].loc of
                      LOC_FPUREGISTER:
                        begin
                          paraitem.paraloc[calleeside].register := cg.getfpuregister(list,paraitem.paraloc[calleeside].size);
                          paraitem.paraloc[callerside] := paraitem.paraloc[calleeside];
                        end;
                      LOC_REGISTER:
                        begin
                          paraitem.paraloc[calleeside].register := cg.getintregister(list,paraitem.paraloc[calleeside].size);
                          paraitem.paraloc[callerside] := paraitem.paraloc[calleeside];
                        end;
                      LOC_MMREGISTER:
                        begin
                          paraitem.paraloc[calleeside].register := cg.getmmregister(list,paraitem.paraloc[calleeside].size);
                          paraitem.paraloc[callerside] := paraitem.paraloc[calleeside];
                        end;
                      LOC_REFERENCE:
                        begin
                          paraitem.paraloc[calleeside] := localloc;
                          paraitem.paraloc[callerside] := localloc;
                        end;
                    end;
                    if cs_asm_source in aktglobalswitches then
                      begin
                        case localloc.loc of
                          LOC_REFERENCE :
                            list.concat(Tai_comment.Create(strpnew('Para '+realname+' allocated at '+
                                std_regname(localloc.reference.index)+tostr_with_plus(localloc.reference.offset))));
                        end;
                      end;
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_free_parast(list:TAAsmoutput;st:tparasymtable);
      var
        sym : tsym;
      begin
        sym:=tsym(st.symindex.first);
        while assigned(sym) do
          begin
            if sym.typ=varsym then
              begin
                with tvarsym(sym) do
                  begin
                    { Note: We need to keep the data available in memory
                      for the sub procedures that can access local data
                      in the parent procedures }
                    case localloc.loc of
                      LOC_REFERENCE :
                        tg.UngetLocal(list,localloc.reference);
                      LOC_REGISTER :
                        begin
                          if localloc.register<>paraitem.paraloc[calleeside].register then
                            begin
{$ifndef cpu64bit}
                              if localloc.size in [OS_64,OS_S64] then
                                begin
                                  cg.ungetregister(list,localloc.registerlow);
                                  cg.ungetregister(list,localloc.registerhigh);
                                end
                              else
{$endif cpu64bit}
                                cg.ungetregister(list,localloc.register);
                            end;
                        end;
                    end;
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    { persistent rtti generation }
    procedure generate_rtti(p:Ttypesym);
      var
        rsym : trttisym;
        def  : tstoreddef;
      begin
        { rtti can only be generated for classes that are always typesyms }
        def:=tstoreddef(ttypesym(p).restype.def);
        { there is an error, skip rtti info }
        if (def.deftype=errordef) or (Errorcount>0) then
          exit;
        { only create rtti once for each definition }
        if not(df_has_rttitable in def.defoptions) then
         begin
           { definition should be in the same symtable as the symbol }
           if p.owner<>def.owner then
            internalerror(200108262);
           { create rttisym }
           rsym:=trttisym.create(p.name,fullrtti);
           p.owner.insert(rsym);
           { register rttisym in definition }
           include(def.defoptions,df_has_rttitable);
           def.rttitablesym:=rsym;
           { write rtti data }
           def.write_child_rtti_data(fullrtti);
           maybe_new_object_file(rttilist);
           new_section(rttilist,sec_rodata,rsym.get_label.name,const_align(sizeof(aint)));
           rttiList.concat(Tai_symbol.Create_global(rsym.get_label,0));
           def.write_rtti_data(fullrtti);
           rttiList.concat(Tai_symbol_end.Create(rsym.get_label));
         end;
      end;


    { persistent init table generation }
    procedure generate_inittable(p:tsym);
      var
        rsym : trttisym;
        def  : tstoreddef;
      begin
        { anonymous types are also allowed for records that can be varsym }
        case p.typ of
          typesym :
            def:=tstoreddef(ttypesym(p).restype.def);
          varsym :
            def:=tstoreddef(tvarsym(p).vartype.def);
          else
            internalerror(200108263);
        end;
        { only create inittable once for each definition }
        if not(df_has_inittable in def.defoptions) then
         begin
           { definition should be in the same symtable as the symbol }
           if p.owner<>def.owner then
            internalerror(200108264);
           { create rttisym }
           rsym:=trttisym.create(p.name,initrtti);
           p.owner.insert(rsym);
           { register rttisym in definition }
           include(def.defoptions,df_has_inittable);
           def.inittablesym:=rsym;
           { write inittable data }
           def.write_child_rtti_data(initrtti);
           maybe_new_object_file(rttilist);
           new_section(rttilist,sec_rodata,rsym.get_label.name,const_align(sizeof(aint)));
           rttiList.concat(Tai_symbol.Create_global(rsym.get_label,0));
           def.write_rtti_data(initrtti);
           rttiList.concat(Tai_symbol_end.Create(rsym.get_label));
         end;
      end;

end.
{
  $Log$
  Revision 1.212  2004-07-17 13:14:17  jonas
    * don't finalize typed consts (fixes bug3212, but causes memory leak;
      they should be finalized at the end of the module)

  Revision 1.211  2004/07/09 23:41:04  jonas
    * support register parameters for inlined procedures + some inline
      cleanups

  Revision 1.210  2004/07/04 12:24:59  jonas
    * fixed one regvar problem, but regvars are still broken since the dwarf
      merge...

  Revision 1.209  2004/06/29 20:57:21  peter
    * fixed size of exceptbuf

  Revision 1.208  2004/06/20 08:55:29  florian
    * logs truncated

  Revision 1.207  2004/06/16 20:07:08  florian
    * dwarf branch merged

  Revision 1.206  2004/06/01 20:39:33  jonas
    * fixed bug regarding parameters on the ppc (they were allocated twice
      under some circumstances and not at all in others)

  Revision 1.205  2004/05/30 21:41:15  jonas
    * more regvar optimizations in location_force_reg

  Revision 1.204  2004/05/30 21:18:22  jonas
    * some optimizations and associated fixes for better regvar code

  Revision 1.203  2004/05/28 21:14:13  peter
    * first load para's to temps before calling entry code (profile

}
