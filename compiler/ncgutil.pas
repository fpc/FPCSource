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
      cpubase,
      aasmbase,aasmtai,aasmcpu,
      cginfo,symconst,symbase,symdef,symsym,symtype,symtable,
{$ifndef cpu64bit}
      cg64f32,
{$endif cpu64bit}
      rgobj;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

    procedure firstcomplex(p : tbinarynode);
    procedure maketojumpbool(list:TAAsmoutput; p : tnode; loadregvars: tloadregvars);
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs:Tsuperregisterset);

    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
    procedure location_force_fpureg(list: TAAsmoutput;var l: tlocation;maybeconst:boolean);
    procedure location_force_mem(list: TAAsmoutput;var l:tlocation);

    function  maybe_pushfpu(list:taasmoutput;needed : byte;var l:tlocation) : boolean;

    procedure gen_proc_symbol(list:Taasmoutput);
    procedure gen_proc_symbol_end(list:Taasmoutput);
    procedure gen_stackalloc_code(list:Taasmoutput);
    procedure gen_stackfree_code(list:Taasmoutput;usesacc,usesacchi:boolean);
    procedure gen_save_used_regs(list : TAAsmoutput);
    procedure gen_restore_used_regs(list : TAAsmoutput;usesacc,usesacchi,usesfpu:boolean);
    procedure gen_initialize_code(list:TAAsmoutput;inlined:boolean);
    procedure gen_finalize_code(list : TAAsmoutput;inlined:boolean);
    procedure gen_load_para_value(list:TAAsmoutput);
    procedure gen_load_return_value(list:TAAsmoutput; var uses_acc,uses_acchi,uses_fpu : boolean);

(*
    procedure geninlineentrycode(list : TAAsmoutput;stackframe:longint);
    procedure geninlineexitcode(list : TAAsmoutput;inlined:boolean);
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
    procedure new_exception(list : taasmoutput;const jmpbuf,envbuf, href : treference;
      a : aword; exceptlabel : tasmlabel);
    procedure free_exception(list : taasmoutput;const jmpbuf, envbuf, href : treference;
      a : aword ; endexceptlabel : tasmlabel; onlyfree : boolean);

    procedure insertconstdata(sym : ttypedconstsym);
    procedure insertbssdata(sym : tvarsym);

    procedure gen_alloc_localst(list: taasmoutput;st:tlocalsymtable);
    procedure gen_free_localst(list: taasmoutput;st:tlocalsymtable);
    procedure gen_alloc_parast(list: taasmoutput;st:tparasymtable);
    procedure gen_free_parast(list: taasmoutput;st:tparasymtable);


implementation

  uses
{$ifdef Delphi}
    Sysutils,
{$else}
    strings,
{$endif}
    cutils,cclasses,
    globals,systems,verbose,
    defutil,
    paramgr,fmodule,
    cgbase,regvars,
{$ifdef GDB}
    gdb,
{$endif GDB}
    ncon,
    tgobj,cgobj;


  const
    { Please leave this here, this module should NOT use
      exprasmlist, the lists are always passed as arguments.
      Declaring it as string here results in an error when compiling (PFV) }
    exprasmlist = 'error';


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
                (p.left.registers32<p.right.registers32)
               ) and
               { the following check is appropriate, because all }
               { 4 registers are rarely used and it is thereby   }
               { achieved that the extra code is being dropped   }
               { by exchanging not commutative operators     }
               (p.right.registers32<=c_countusableregsint)
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
              if loadregvars = lr_load_regvars then
                load_all_regvars(list);
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
                         if (p.location.loc = LOC_CREGISTER) then
                           load_regvar_reg(list,p.location.register);
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
                       internalerror(200308241);
                   end;
                end;
           end
         else
           internalerror(200112305);
         aktfilepos:=storepos;
      end;


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
                 (getsupreg(t.reference.base) in rg.usableregsint) then
                exclude(regs,getsupreg(t.reference.base));
              if not(cs_regvars in aktglobalswitches) or
                 (getsupreg(t.reference.index) in rg.usableregsint) then
                exclude(regs,getsupreg(t.reference.index));
            end;
        end;
      end;

{*****************************************************************************
                            EXCEPTION MANAGEMENT
*****************************************************************************}

    procedure new_exception(list : taasmoutput;const jmpbuf,envbuf, href : treference;
      a : aword; exceptlabel : tasmlabel);

      var
        paraloc1,paraloc2,paraloc3 : tparalocation;
      begin
        paraloc1:=paramanager.getintparaloc(pocall_default,1);
        paraloc2:=paramanager.getintparaloc(pocall_default,2);
        paraloc3:=paramanager.getintparaloc(pocall_default,3);
        paramanager.allocparaloc(list,paraloc3);
        cg.a_paramaddr_ref(list,envbuf,paraloc3);
        paramanager.allocparaloc(list,paraloc2);
        cg.a_paramaddr_ref(list,jmpbuf,paraloc2);
        { push type of exceptionframe }
        paramanager.allocparaloc(list,paraloc1);
        cg.a_param_const(list,OS_S32,1,paraloc1);
        paramanager.freeparaloc(list,paraloc3);
        paramanager.freeparaloc(list,paraloc2);
        paramanager.freeparaloc(list,paraloc1);
        rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
        cg.a_call_name(list,'FPC_PUSHEXCEPTADDR');
        rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));

        paramanager.allocparaloc(list,paraloc1);
        cg.a_param_reg(list,OS_ADDR,NR_FUNCTION_RESULT_REG,paraloc1);
        paramanager.freeparaloc(list,paraloc1);
        rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
        cg.a_call_name(list,'FPC_SETJMP');
        rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));

        cg.g_exception_reason_save(list, href);
        cg.a_cmp_const_reg_label(list,OS_S32,OC_NE,0,NR_FUNCTION_RESULT_REG,exceptlabel);
     end;


    procedure free_exception(list : taasmoutput;const jmpbuf, envbuf, href : treference;
     a : aword ; endexceptlabel : tasmlabel; onlyfree : boolean);

     begin
         rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
         cg.a_call_name(list,'FPC_POPADDRSTACK');
         rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));

         if not onlyfree then
          begin
            cg.g_exception_reason_load(list, href);
            cg.a_cmp_const_reg_label(list,OS_S32,OC_EQ,a,NR_FUNCTION_RESULT_REG,endexceptlabel);
          end;
     end;


{*****************************************************************************
                                     TLocation
*****************************************************************************}

{$ifndef cpu64bit}
    { 32-bit version }
    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister,
        hregisterhi : tregister;
        hreg64 : tregister64;
        hl : tasmlabel;
        oldloc : tlocation;
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
                 hregister:=rg.makeregsize(l.registerlow,OS_32);
                 cg.a_load_reg_reg(list,l.size,OS_32,l.registerlow,hregister);
               end
              else
               begin
                 location_release(list,l);
                 hregister:=rg.getregisterint(list,OS_INT);
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
              hregisterhi:=rg.getregisterint(list,OS_INT);
              if (dst_size=OS_S64) and
                 (l.size in [OS_S8,OS_S16,OS_S32]) then
               begin
                 if l.loc=LOC_CONSTANT then
                  begin
                    if (longint(l.value)<0) then
                     cg.a_load_const_reg(list,OS_32,$ffffffff,hregisterhi)
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
                 hregister:=rg.getregisterint(list,OS_INT);
                 hregisterhi:=rg.getregisterint(list,OS_INT);
                 location_release(list,l);
               end;
              hreg64.reglo:=hregister;
              hreg64.reghi:=hregisterhi;
              { load value in new register }
              cg64.a_load64_loc_reg(list,l,hreg64,false);
              location_reset(l,LOC_REGISTER,dst_size);
              l.registerlow:=hregister;
              l.registerhigh:=hregisterhi;
            end;
         end
        else
         begin
           { transformations to 32bit or smaller }
           if (l.loc=LOC_REGISTER) and (l.size in [OS_64,OS_S64]) then
             { if the previous was 64bit release the high register }
             begin
               rg.ungetregisterint(list,l.registerhigh);
               l.registerhigh:=NR_NO;
             end;
           {Do not bother to recycle the existing register. The register
            allocator eliminates unnecessary moves, so it's not needed
            and trying to recycle registers can cause problems because
            the registers changes size and may need aditional constraints.}
           location_release(list,l);
           hregister:=rg.getregisterint(list,dst_size);
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
                      l.register:=rg.makeregsize(l.register,dst_size);
                    { for big endian systems, the reference's offset must }
                    { be increased in this case, since they have the      }
                    { MSB first in memory and e.g. byte(word_var) should  }
                    { return  the second byte in this case (JM)           }
                    if (target_info.endian = ENDIAN_BIG) and
                       (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                      inc(l.reference.offset,TCGSize2Size[l.size]-TCGSize2Size[dst_size]);
{$ifdef i386}
                   l.size:=dst_size;
{$endif i386}
                  end;
                 cg.a_load_loc_reg(list,dst_size,l,hregister);
{$ifndef i386}
                 if (TCGSize2Size[dst_size]<TCGSize2Size[l.size]) then
                   l.size:=dst_size;
{$endif not i386}
               end;
           end;
           if (l.loc <> LOC_CREGISTER) or
              not maybeconst then
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
    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister : tregister;
        hl : tasmlabel;
        oldloc : tlocation;
     begin
        oldloc:=l;
        if dst_size=OS_NO then
          internalerror(200309144);
        { handle transformations to 64bit separate }
        if dst_size in [OS_64,OS_S64] then
          begin
              { load a smaller size to OS_64 }
              if l.loc=LOC_REGISTER then
               hregister:=rg.makeregsize(l.register,OS_INT)
              else
               begin
                 location_release(list,l);
                 hregister:=rg.getregisterint(list,OS_INT);
               end;
              { load value in low register }
              case l.loc of
{$ifdef cpuflags}
                LOC_FLAGS :
                  cg.g_flags2reg(list,OS_INT,l.resflags,hregister);
{$endif cpuflags}

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
              location_reset(l,LOC_REGISTER,dst_size);
              l.register:=hregister;
            end
        else
         begin
           { transformations to 32bit or smaller }
           if l.loc=LOC_REGISTER then
            begin
              hregister:=l.register;
            end
           else
            begin
              { get new register }
              if (l.loc=LOC_CREGISTER) and
                 maybeconst and
                 (TCGSize2Size[dst_size]=TCGSize2Size[l.size]) then
               hregister:=l.register
              else
               begin
                 location_release(list,l);
                 hregister:=rg.getregisterint(list,OS_INT);
               end;
            end;
           hregister:=rg.makeregsize(hregister,dst_size);
           { load value in new register }
           case l.loc of
{$ifdef cpuflags}
             LOC_FLAGS :
               cg.g_flags2reg(list,dst_size,l.resflags,hregister);
{$endif cpuflags}
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
                     l.register:=rg.makeregsize(l.register,dst_size);
                    { for big endian systems, the reference's offset must }
                    { be increased in this case, since they have the      }
                    { MSB first in memory and e.g. byte(word_var) should  }
                    { return  the second byte in this case (JM)           }
                    if (target_info.endian = ENDIAN_BIG) and
                       (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                      inc(l.reference.offset,TCGSize2Size[l.size]-TCGSize2Size[dst_size]);
{$ifdef i386}
                   l.size:=dst_size;
{$endif i386}
                  end;

                 cg.a_load_loc_reg(list,dst_size,l,hregister);
{$ifndef i386}
                 if (TCGSize2Size[dst_size]<TCGSize2Size[l.size]) then
                   l.size:=dst_size;
{$endif not i386}
               end;
           end;
           location_reset(l,LOC_REGISTER,dst_size);
           l.register:=hregister;
         end;
       { Release temp when it was a reference }
       if oldloc.loc=LOC_REFERENCE then
         location_freetemp(list,oldloc);
     end;
{$endif cpu64bit}


    procedure location_force_fpureg(list: TAAsmoutput;var l: tlocation;maybeconst:boolean);
      var
        reg : tregister;
      begin
        if (l.loc<>LOC_FPUREGISTER)  and
           ((l.loc<>LOC_CFPUREGISTER) or (not maybeconst)) then
          begin
            reg:=rg.getregisterfpu(list,l.size);
            cg.a_loadfpu_loc_reg(list,l,reg);
            location_freetemp(list,l);
            location_release(list,l);
            location_reset(l,LOC_FPUREGISTER,l.size);
            l.register:=reg;
          end;
      end;


    procedure location_force_mem(list: TAAsmoutput;var l:tlocation);
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
          LOC_CONSTANT,
          LOC_REGISTER,
          LOC_CREGISTER :
            begin
              tg.GetTemp(list,TCGSize2Size[l.size],tt_normal,r);
              if l.size in [OS_64,OS_S64] then
                begin
                  cg64.a_load64_loc_ref(list,l,r);
                  location_release(list,l);
                end
              else
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
        if (needed>=maxfpuregs) and
           (l.loc = LOC_FPUREGISTER) then
          begin
            location_force_mem(list,l);
            maybe_pushfpu:=true;
          end
        else
          maybe_pushfpu:=false;
      end;


{****************************************************************************
                            Init/Finalize Code
****************************************************************************}

    procedure copyvalueparas(p : tnamedindexitem;arg:pointer);
      var
        href1,href2 : treference;
        list : taasmoutput;
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
              tg.GetLocal(list,l,localcopyloc.reference);
              { Copy data }
              reference_reset_base(href2,localcopyloc.reference.index,localcopyloc.reference.offset);
              if is_shortstring(tvarsym(p).vartype.def) then
                cg.g_copyshortstring(list,href1,href2,tstringdef(tvarsym(p).vartype.def).len,false,loadref)
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
        href : treference;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           (tvarsym(p).refs>0) and
           assigned(tvarsym(p).vartype.def) and
           not(is_class(tvarsym(p).vartype.def)) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           if (cs_implicit_exceptions in aktmoduleswitches) then
            include(current_procinfo.flags,pi_needs_implicit_finally);
           if tvarsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
             begin
               case tvarsym(p).localloc.loc of
                 LOC_REFERENCE :
                   reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
                 else
                   internalerror(2003091810);
               end;
             end
           else
             reference_reset_symbol(href,objectlibrary.newasmsymboldata(tvarsym(p).mangledname),0);
           cg.g_initialize(list,tvarsym(p).vartype.def,href,false);
         end;
      end;


    { generates the code for finalisation of local data }
    procedure finalize_data(p : tnamedindexitem;arg:pointer);
      var
        href : treference;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        case tsym(p).typ of
          varsym :
            begin
              if (tvarsym(p).refs>0) and
                 not(vo_is_funcret in tvarsym(p).varoptions) and
                 assigned(tvarsym(p).vartype.def) and
                 not(is_class(tvarsym(p).vartype.def)) and
                 tvarsym(p).vartype.def.needs_inittable then
               begin
                 if tvarsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
                   begin
                     case tvarsym(p).localloc.loc of
                       LOC_REFERENCE :
                         reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
                       else
                         internalerror(2003091811);
                     end;
                   end
                 else
                   reference_reset_symbol(href,objectlibrary.newasmsymboldata(tvarsym(p).mangledname),0);
                 cg.g_finalize(list,tvarsym(p).vartype.def,href,false);
               end;
            end;
          typedconstsym :
            begin
              if ttypedconstsym(p).is_writable and
                 ttypedconstsym(p).typedconsttype.def.needs_inittable then
               begin
                 reference_reset_symbol(href,objectlibrary.newasmsymboldata(ttypedconstsym(p).mangledname),0);
                 cg.g_finalize(list,ttypedconstsym(p).typedconsttype.def,href,false);
               end;
            end;
        end;
      end;


    { generates the code for incrementing the reference count of parameters and
      initialize out parameters }
    procedure init_paras(p : tnamedindexitem;arg:pointer);
      var
        href : treference;
        tmpreg : tregister;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           not is_class(tvarsym(p).vartype.def) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           case tvarsym(p).varspez of
             vs_value :
               begin
                 if (cs_implicit_exceptions in aktmoduleswitches) then
                  include(current_procinfo.flags,pi_needs_implicit_finally);
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
                 tmpreg:=rg.getaddressregister(list);
                 cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,tmpreg);
                 reference_reset_base(href,tmpreg,0);
                 cg.g_initialize(list,tvarsym(p).vartype.def,href,false);
                 rg.ungetregisterint(list,tmpreg);
               end;
           end;
         end;
      end;


    { generates the code for decrementing the reference count of parameters }
    procedure final_paras(p : tnamedindexitem;arg:pointer);
      var
        href : treference;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           not is_class(tvarsym(p).vartype.def) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           if (tvarsym(p).varspez=vs_value) then
            begin
              if tvarsym(p).localloc.loc<>LOC_REFERENCE then
                internalerror(200309188);
              reference_reset_base(href,tvarsym(p).localloc.reference.index,tvarsym(p).localloc.reference.offset);
              cg.g_decrrefcount(list,tvarsym(p).vartype.def,href,is_open_array(tvarsym(p).vartype.def));
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
           if hp^.temptype in [tt_ansistring,tt_freeansistring,
                               tt_widestring,tt_freewidestring,
                               tt_interfacecom,tt_freeinterfacecom] then
            begin
              if (cs_implicit_exceptions in aktmoduleswitches) then
                include(current_procinfo.flags,pi_needs_implicit_finally);
              reference_reset_base(href,current_procinfo.framepointer,hp^.pos);
              cg.a_load_const_ref(list,OS_ADDR,0,href);
            end;
           hp:=hp^.next;
         end;
      end;


    procedure finalizetempvariables(list:taasmoutput);
      var
        hp : ptemprecord;
        href : treference;
        paraloc1 : tparalocation;
      begin
        paraloc1:=paramanager.getintparaloc(pocall_default,1);
        hp:=tg.templist;
        while assigned(hp) do
         begin
           case hp^.temptype of
             tt_ansistring,
             tt_freeansistring :
               begin
                 reference_reset_base(href,current_procinfo.framepointer,hp^.pos);
                 paramanager.allocparaloc(list,paraloc1);
                 cg.a_paramaddr_ref(list,href,paraloc1);
                 paramanager.freeparaloc(list,paraloc1);
                 rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
                 cg.a_call_name(list,'FPC_ANSISTR_DECR_REF');
                 rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
               end;
             tt_widestring,
             tt_freewidestring :
               begin
                 reference_reset_base(href,current_procinfo.framepointer,hp^.pos);
                 paramanager.allocparaloc(list,paraloc1);
                 cg.a_paramaddr_ref(list,href,paraloc1);
                 paramanager.freeparaloc(list,paraloc1);
                 rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
                 cg.a_call_name(list,'FPC_WIDESTR_DECR_REF');
                 rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
               end;
             tt_interfacecom :
               begin
                 reference_reset_base(href,current_procinfo.framepointer,hp^.pos);
                 paramanager.allocparaloc(list,paraloc1);
                 cg.a_paramaddr_ref(list,href,paraloc1);
                 paramanager.freeparaloc(list,paraloc1);
                 rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
                 cg.a_call_name(list,'FPC_INTF_DECR_REF');
                 rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
               end;
           end;
           hp:=hp^.next;
         end;
      end;


    procedure gen_load_return_value(list:TAAsmoutput; var uses_acc,uses_acchi,uses_fpu : boolean);
      var
        ressym : tvarsym;
        resloc : tlocation;
        href   : treference;
        hreg,r,r2 : tregister;
      begin
        { Is the loading needed? }
        if is_void(current_procinfo.procdef.rettype.def) or
           (
            (po_assembler in current_procinfo.procdef.procoptions) and
            (not(assigned(current_procinfo.procdef.funcretsym)) or
             (tvarsym(current_procinfo.procdef.funcretsym).refcount=0))
           ) then
          exit;

        { Constructors need to return self }
        if (current_procinfo.procdef.proctypeoption=potype_constructor) then
          begin
            r:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN_REG);
            { return the self pointer }
            ressym:=tvarsym(current_procinfo.procdef.parast.search('self'));
            if not assigned(ressym) then
              internalerror(200305058);
            case ressym.localloc.loc of
              LOC_REFERENCE :
                reference_reset_base(href,ressym.localloc.reference.index,ressym.localloc.reference.offset);
              else
                internalerror(2003091810);
            end;
            rg.ungetregisterint(list,r);
            cg.a_load_ref_reg(list,OS_ADDR,OS_ADDR,href,r);
            uses_acc:=true;
            exit;
          end;

        ressym := tvarsym(current_procinfo.procdef.funcretsym);
        if (ressym.refs>0) then
          begin
            case ressym.localloc.loc of
              LOC_FPUREGISTER,
              LOC_REGISTER :
                begin
                  if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
                    location_reset(resloc,LOC_CREGISTER,OS_ADDR)
                  else
                    if ressym.vartype.def.deftype = floatdef then
                      location_reset(resloc,LOC_CFPUREGISTER,def_cgsize(current_procinfo.procdef.rettype.def))
                    else
                      location_reset(resloc,LOC_CREGISTER,def_cgsize(current_procinfo.procdef.rettype.def));
                  resloc.register:=ressym.localloc.register;
                end;
              LOC_REFERENCE :
                begin
                  location_reset(resloc,LOC_REFERENCE,def_cgsize(current_procinfo.procdef.rettype.def));
                  reference_reset_base(resloc.reference,ressym.localloc.reference.index,ressym.localloc.reference.offset);
                end;
              else
                internalerror(200309184);
            end;

            { Here, we return the function result. In most architectures, the value is
              passed into the FUNCTION_RETURN_REG, but in a windowed architecure like sparc a
              function returns in a register and the caller receives it in an other one }
            case current_procinfo.procdef.rettype.def.deftype of
              orddef,
              enumdef :
                begin
                  uses_acc:=true;
    {$ifndef cpu64bit}
                  if resloc.size in [OS_64,OS_S64] then
                   begin
                     uses_acchi:=true;
                     r:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN64_LOW_REG);
                     r2:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN64_HIGH_REG);
                     rg.ungetregisterint(list,r);
                     rg.ungetregisterint(list,r2);
                     cg64.a_load64_loc_reg(list,resloc,joinreg64(r,r2),false);
                   end
                  else
    {$endif cpu64bit}
                   begin
                     hreg:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN_REG);
                     hreg:=rg.makeregsize(hreg,resloc.size);
                     rg.ungetregisterint(list,hreg);
                     cg.a_load_loc_reg(list,resloc.size,resloc,hreg);
                   end;
                end;
              floatdef :
                begin
                  uses_fpu := true;
    {$ifdef cpufpemu}
                  if cs_fp_emulation in aktmoduleswitches then
                    r:=NR_FUNCTION_RETURN_REG
                  else
    {$endif cpufpemu}
                    r:=NR_FPU_RESULT_REG;
                  cg.a_loadfpu_loc_reg(list,resloc,r);
                end;
              else
                begin
                  if not paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
                   begin
                     uses_acc:=true;
    {$ifndef cpu64bit}
                     { Win32 can return records in EAX:EDX }
                     if resloc.size in [OS_64,OS_S64] then
                      begin
                        uses_acchi:=true;
                        r:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN64_LOW_REG);
                        r2:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN64_HIGH_REG);
                        rg.ungetregisterint(list,r);
                        rg.ungetregisterint(list,r2);
                        cg64.a_load64_loc_reg(list,resloc,joinreg64(r,r2),false);
                      end
                     else
    {$endif cpu64bit}
                      begin
                        hreg:=rg.getexplicitregisterint(list,NR_FUNCTION_RETURN_REG);
                        hreg:=rg.makeregsize(hreg,resloc.size);
                        rg.ungetregisterint(list,hreg);
                        cg.a_load_loc_reg(list,resloc.size,resloc,hreg);
                      end;
                    end
                end;
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
                  LOC_REGISTER :
                    begin
                      gotregvarparas := true;
                      { cg.a_load_param_reg will first allocate and then deallocate paraloc }
                      { register (if the parameter resides in a register) and then allocate }
                      { the regvar (which is currently not allocated)                       }
                      cg.a_load_param_reg(list,hp.paraloc[calleeside],tvarsym(hp.parasym).localloc.register);
                    end;
                  LOC_REFERENCE :
                    begin
                      if hp.paraloc[calleeside].loc<>LOC_REFERENCE then
                        begin
                          reference_reset_base(href,tvarsym(hp.parasym).localloc.reference.index,tvarsym(hp.parasym).localloc.reference.offset);
                          cg.a_load_param_ref(list,hp.paraloc[calleeside],href);
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
                      rg.ungetregisterint(list,tvarsym(hp.parasym).localloc.register);
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
      var
        href : treference;
        paraloc1,
        paraloc2 : tparalocation;
      begin
        { the actual profile code can clobber some registers,
          therefore if the context must be saved, do it before
          the actual call to the profile code
        }
        if (cs_profile in aktmoduleswitches) and
           not(po_assembler in current_procinfo.procdef.procoptions) and
           not(inlined) then
          begin
            { non-win32 can call mcout even in main }
            if not (target_info.system in [system_i386_win32,system_i386_wdosx])  then
              cg.g_profilecode(list)
            else
            { wdosx, and win32 should not call mcount before monstartup has been called }
            if not (current_procinfo.procdef.proctypeoption=potype_proginit) then
              cg.g_profilecode(list);
          end;

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

        if (not inlined) then
         begin
           { call startup helpers from main program }
           if (current_procinfo.procdef.proctypeoption=potype_proginit) then
            begin
              { initialize profiling for win32 }
              if (target_info.system in [system_i386_win32,system_i386_wdosx]) and
                 (cs_profile in aktmoduleswitches) then
               begin
                 reference_reset_symbol(href,objectlibrary.newasmsymboldata('etext'),0);
                 paraloc1:=paramanager.getintparaloc(pocall_default,1);
                 paraloc2:=paramanager.getintparaloc(pocall_default,2);
                 paramanager.allocparaloc(list,paraloc2);
                 cg.a_paramaddr_ref(list,href,paraloc2);
                 reference_reset_symbol(href,objectlibrary.newasmsymboldata('__image_base__'),0);
                 paramanager.allocparaloc(list,paraloc1);
                 cg.a_paramaddr_ref(list,href,paraloc1);
                 paramanager.freeparaloc(list,paraloc2);
                 paramanager.freeparaloc(list,paraloc1);
                 rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_cdecl));
                 cg.a_call_name(list,'_monstartup');
                 rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_cdecl));
               end;

              { initialize units }
              rg.allocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
              cg.a_call_name(list,'FPC_INITIALIZEUNITS');
              rg.deallocexplicitregistersint(list,paramanager.get_volatile_registers_int(pocall_default));
            end;

{$ifdef GDB}
           if (cs_debuginfo in aktmoduleswitches) then
            list.concat(Tai_force_line.Create);
{$endif GDB}
         end;

        load_regvars(list,nil);
      end;


    procedure gen_finalize_code(list : TAAsmoutput;inlined:boolean);
      begin
        cg.a_label(list,current_procinfo.aktexitlabel);

        cleanup_regvars(list);

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

        { call __EXIT for main program }
        if (not DLLsource) and
           (not inlined) and
           (current_procinfo.procdef.proctypeoption=potype_proginit) then
          cg.a_call_name(list,'FPC_DO_EXIT');

        cleanup_regvars(list);
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
            list.concat(Tai_symbol.createname_global(hs,0))
          else
            list.concat(Tai_symbol.createname(hs,0));
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
                if tvarsym(current_procinfo.procdef.funcretsym).localloc.loc<>LOC_REFERENCE then
                  internalerror(2003091812);
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


    procedure gen_stackalloc_code(list:Taasmoutput);
      var
        stackframe : longint;
      begin
        { Calculate size of stackframe }
        stackframe:=current_procinfo.calc_stackframe_size;

{$ifndef powerpc}
        { at least for the ppc this applies always, so this code isn't usable (FK) }
        { omit stack frame ? }
        if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
          begin
            CGmessage(cg_d_stackframe_omited);
            if stackframe<>0 then
              cg.g_stackpointer_alloc(list,stackframe);
          end
        else
{$endif powerpc}
          begin
            if (po_interrupt in current_procinfo.procdef.procoptions) then
              cg.g_interrupt_stackframe_entry(list);

            cg.g_stackframe_entry(list,stackframe);

            {Never call stack checking before the standard system unit
             has been initialized.}
             if (cs_check_stack in aktlocalswitches) and (current_procinfo.procdef.proctypeoption<>potype_proginit) then
               cg.g_stackcheck(list,stackframe);
          end;
      end;


    procedure gen_stackfree_code(list:Taasmoutput;usesacc,usesacchi:boolean);
      var
        stacksize,
        retsize : longint;
      begin
{$ifndef powerpc}
        { remove stackframe }
        if (current_procinfo.framepointer=NR_STACK_POINTER_REG) then
          begin
            stacksize:=current_procinfo.calc_stackframe_size;
            if (stacksize<>0) then
              cg.a_op_const_reg(list,OP_ADD,OS_32,stacksize,current_procinfo.framepointer);
          end
        else
          cg.g_restore_frame_pointer(list);
{$endif}
        { at last, the return is generated }
        if (po_interrupt in current_procinfo.procdef.procoptions) then
          cg.g_interrupt_stackframe_exit(list,usesacc,usesacchi)
        else
          begin
            if current_procinfo.procdef.proccalloption in clearstack_pocalls then
              begin
                retsize:=0;
                if paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption) then
                  inc(retsize,POINTER_SIZE);
              end
            else
              retsize:=current_procinfo.para_stack_size;
            cg.g_return_from_proc(list,retsize);
          end;
      end;


    procedure gen_save_used_regs(list : TAAsmoutput);
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
            cg.g_save_standard_registers(list,rg.used_in_proc_int);
      end;


    procedure gen_restore_used_regs(list : TAAsmoutput;usesacc,usesacchi,usesfpu:boolean);
      begin
        { Pure assembler routines need to save the registers themselves }
        if (po_assembler in current_procinfo.procdef.procoptions) then
          exit;

        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in current_procinfo.procdef.procoptions) then
          cg.g_restore_all_registers(list,usesacc,usesacchi)
        else
          if current_procinfo.procdef.proccalloption in savestdregs_pocalls then
            cg.g_restore_standard_registers(list,rg.used_in_proc_int);
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
                    r:=rg.getregisterint(list,OS_INT);
                    r2:=rg.getregisterint(list,OS_INT);
                    cg64.a_load64_loc_reg(list,resloc,joinreg64(r,r2),false);
                  end
                 else
{$endif cpu64bit}
                  begin
                    r:=rg.getregisterint(list,resloc.size);
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
                       r:=rg.getregisterint(list,OS_INT);
                       r2:=rg.getregisterint(list,OS_INT);
                       cg64.a_load64_loc_reg(list,resloc,joinreg64(r,r2),false);
                     end
                    else
{$endif cpu64bit}
                     begin
                       r:=rg.getregisterint(list,resloc.size);
                       cg.a_load_loc_reg(list,resloc.size,resloc,r);
                     end;
                   end
               end;
           end;
         end;
      end;


    procedure geninlineentrycode(list : TAAsmoutput;stackframe:longint);
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


   procedure geninlineexitcode(list : TAAsmoutput;inlined:boolean);
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
        if (cs_create_smart in aktmoduleswitches) then
          curconstSegment.concat(Tai_cut.Create);
        curconstSegment.concat(Tai_align.create(const_align(l)));
{$ifdef GDB}
        if cs_debuginfo in aktmoduleswitches then
          sym.concatstabto(curconstsegment);
{$endif GDB}
        if (sym.owner.symtabletype=globalsymtable) or
           (cs_create_smart in aktmoduleswitches) or
           DLLSource then
          curconstSegment.concat(Tai_symbol.Createdataname_global(sym.mangledname,l))
        else
          curconstSegment.concat(Tai_symbol.Createdataname(sym.mangledname,l));
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
          inc(l,pointer_size);
        varalign:=var_align(l);
        {
        sym.address:=align(datasize,varalign);
        datasize:=tvarsym(sym).address+l;
        }
        { insert cut for smartlinking or alignment }
        if (cs_create_smart in aktmoduleswitches) then
          bssSegment.concat(Tai_cut.Create);
        bssSegment.concat(Tai_align.create(varalign));
{$ifdef GDB}
        if cs_debuginfo in aktmoduleswitches then
           sym.concatstabto(bsssegment);
{$endif GDB}
        if (sym.owner.symtabletype=globalsymtable) or
           (cs_create_smart in aktmoduleswitches) or
           DLLSource or
           (vo_is_exported in sym.varoptions) or
           (vo_is_C_var in sym.varoptions) then
          bssSegment.concat(Tai_datablock.Create_global(sym.mangledname,l))
        else
          bssSegment.concat(Tai_datablock.Create(sym.mangledname,l));
        aktfilepos:=storefilepos;
      end;


    procedure gen_alloc_localst(list: taasmoutput;st:tlocalsymtable);
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
                    tg.GetLocal(list,getvaluesize,localloc.reference);
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_free_localst(list: taasmoutput;st:tlocalsymtable);
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
                              rg.ungetregister(list,localloc.registerlow);
                              rg.ungetregister(list,localloc.registerhigh);
                            end
                          else
{$endif cpu64bit}
                            rg.ungetregister(list,localloc.register);
                        end;
                    end;
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_alloc_parast(list: taasmoutput;st:tparasymtable);
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
                    { Allocate imaginary register for register parameters,
                      this is not required when the parameter is already an
                      imaginary register }
                    if (paraitem.paraloc[calleeside].loc=LOC_REGISTER) and
                       (getsupreg(paraitem.paraloc[calleeside].register)<first_int_imreg) then
                      begin
                        (*
{$warning TODO Allocate register paras}
                        localloc.loc:=LOC_REGISTER;
                        localloc.size:=paraitem.paraloc[calleeside].size;
{$ifndef cpu64bit}
                        if localloc.size in [OS_64,OS_S64] then
                          begin
                            localloc.registerlow:=rg.getregisterint(list,OS_32);
                            localloc.registerhigh:=rg.getregisterint(list,OS_32);
                          end
                        else
{$endif cpu64bit}
                          localloc.register:=rg.getregisterint(list,localloc.size);
                          *)
                        localloc.loc:=LOC_REFERENCE;
                        localloc.size:=paraitem.paraloc[calleeside].size;
                        tg.GetLocal(list,tcgsize2size[localloc.size],localloc.reference);
                      end
                    else
                      localloc:=paraitem.paraloc[calleeside];
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


    procedure gen_free_parast(list: taasmoutput;st:tparasymtable);
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
{$ifndef cpu64bit}
                          if localloc.size in [OS_64,OS_S64] then
                            begin
                              rg.ungetregister(list,localloc.registerlow);
                              rg.ungetregister(list,localloc.registerhigh);
                            end
                          else
{$endif cpu64bit}
                            rg.ungetregister(list,localloc.register);
                        end;
                    end;
                  end;
              end;
            sym:=tsym(sym.indexnext);
          end;
      end;


end.
{
  $Log$
  Revision 1.153  2003-09-30 21:02:37  peter
    * updates for inlining

  Revision 1.152  2003/09/29 20:58:56  peter
    * optimized releasing of registers

  Revision 1.151  2003/09/28 21:47:18  peter
    * register paras and local copies updates

  Revision 1.150  2003/09/28 17:55:03  peter
    * parent framepointer changed to hidden parameter
    * tloadparentfpnode added

  Revision 1.149  2003/09/28 13:39:38  peter
    * optimized releasing of registers

  Revision 1.148  2003/09/25 21:28:00  peter
    * parameter fixes

  Revision 1.147  2003/09/23 21:03:59  peter
    * check for refs>0 in init/final local data

  Revision 1.146  2003/09/23 17:56:05  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.145  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.144  2003/09/14 21:33:37  peter
    * location_force_reg gives IE when size=OS_NO

  Revision 1.143  2003/09/14 19:18:10  peter
    * remove obsolete code already in comments

  Revision 1.142  2003/09/11 11:54:59  florian
    * improved arm code generation
    * move some protected and private field around
    * the temp. register for register parameters/arguments are now released
      before the move to the parameter register is done. This improves
      the code in a lot of cases.

  Revision 1.141  2003/09/10 08:31:47  marco
   * Patch from Peter for paraloc

  Revision 1.140  2003/09/07 22:09:35  peter
    * preparations for different default calling conventions
    * various RA fixes

  Revision 1.139  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.138  2003/09/03 11:18:37  florian
    * fixed arm concatcopy
    + arm support in the common compiler sources added
    * moved some generic cg code around
    + tfputype added
    * ...

  Revision 1.137.2.4  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.137.2.3  2003/08/29 17:28:59  peter
    * next batch of updates

  Revision 1.137.2.2  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.137.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.137  2003/08/20 20:29:06  daniel
    * Some more R_NO changes
    * Preventive code to loadref added

  Revision 1.136  2003/08/20 17:48:49  peter
    * fixed stackalloc to not allocate localst.datasize twice
    * order of stackalloc code fixed for implicit init/final

  Revision 1.135  2003/08/17 16:59:20  jonas
    * fixed regvars so they work with newra (at least for ppc)
    * fixed some volatile register bugs
    + -dnotranslation option for -dnewra, which causes the registers not to
      be translated from virtual to normal registers. Requires support in
      the assembler writer as well, which is only implemented in aggas/
      agppcgas currently

  Revision 1.134  2003/08/11 21:18:20  peter
    * start of sparc support for newra

  Revision 1.133  2003/08/09 18:56:54  daniel
    * cs_regalloc renamed to cs_regvars to avoid confusion with register
      allocator
    * Some preventive changes to i386 spillinh code

  Revision 1.132  2003/08/03 14:09:50  daniel
    * Fixed a register allocator bug
    * Figured out why -dnewra generates superfluous "mov reg1,reg2"
      statements: changes in location_force. These moves are now no longer
      constrained so they are optimized away.

  Revision 1.131  2003/07/23 11:04:15  jonas
    * split en_exit_code into a part that may allocate a register and a part
      that doesn't, so the former can be done before the register colouring
      has been performed

  Revision 1.130  2003/07/06 17:58:22  peter
    * framepointer fixes for sparc
    * parent framepointer code more generic

  Revision 1.129  2003/07/06 15:31:20  daniel
    * Fixed register allocator. *Lots* of fixes.

  Revision 1.128  2003/07/02 22:18:04  peter
    * paraloc splitted in callerparaloc,calleeparaloc
    * sparc calling convention updates

  Revision 1.127  2003/06/17 18:13:51  jonas
    * fixed -dnewra compilation problems

  Revision 1.126  2003/06/17 16:32:44  peter
    * alloc register for function result

  Revision 1.125  2003/06/13 21:19:30  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.124  2003/06/09 12:23:30  peter
    * init/final of procedure data splitted from genentrycode
    * use asmnode getposition to insert final at the correct position
      als for the implicit try...finally

  Revision 1.123  2003/06/07 18:57:04  jonas
    + added freeintparaloc
    * ppc get/freeintparaloc now check whether the parameter regs are
      properly allocated/deallocated (and get an extra list para)
    * ppc a_call_* now internalerrors if pi_do_call is not yet set
    * fixed lot of missing pi_do_call's

  Revision 1.122  2003/06/06 14:43:02  peter
    * g_copyopenarrayvalue gets length reference
    * don't copy open arrays for cdecl

  Revision 1.121  2003/06/03 21:11:09  peter
    * cg.a_load_* get a from and to size specifier
    * makeregsize only accepts newregister
    * i386 uses generic tcgnotnode,tcgunaryminus

  Revision 1.120  2003/06/03 15:49:49  jonas
    * fixed ref/loc problems

  Revision 1.119  2003/06/03 15:06:37  daniel
    * fixed conflict marks

  Revision 1.118  2003/06/03 13:01:59  daniel
    * Register allocator finished

  Revision 1.117  2003/06/02 21:42:05  jonas
    * function results can now also be regvars
    - removed tprocinfo.return_offset, never use it again since it's invalid
      if the result is a regvar

  Revision 1.116  2003/06/01 21:38:06  peter
    * getregisterfpu size parameter added
    * op_const_reg size parameter added
    * sparc updates

  Revision 1.115  2003/05/31 20:28:17  jonas
    * changed copyvalueparas so it also supports register parameters
      (except for copy_value_openarray, but that one is seriously broken
       anyway, since it expects that the high parameter will always be in
       memory right after the pointer to the array, while it could just as
       will be in a register)

  Revision 1.114  2003/05/31 15:05:28  peter
    * FUNCTION_RESULT64_LOW/HIGH_REG added for int64 results

  Revision 1.113  2003/05/31 00:48:15  jonas
    * fixed my previous commit

  Revision 1.112  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.111  2003/05/30 23:49:18  jonas
    * a_load_loc_reg now has an extra size parameter for the destination
      register (properly fixes what I worked around in revision 1.106 of
      ncgutil.pas)

  Revision 1.110  2003/05/30 18:52:10  jonas
    * fixed bug with intregvars
    * locapara.loc can also be LOC_CFPUREGISTER -> also fixed
      rcgppc.a_param_ref, which previously got bogus size values

  Revision 1.109  2003/05/27 21:19:08  jonas
    * fixed ppc cycle

  Revision 1.108  2003/05/27 14:28:14  jonas
    * patch from Peter for nested procedures

  Revision 1.107  2003/05/26 21:17:17  peter
    * procinlinenode removed
    * aktexit2label removed, fast exit removed
    + tcallnode.inlined_pass_2 added

  Revision 1.106  2003/05/24 11:59:42  jonas
    * fixed integer typeconversion problems

  Revision 1.105  2003/05/23 14:27:35  peter
    * remove some unit dependencies
    * current_procinfo changes to store more info

  Revision 1.104  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.103  2003/05/14 19:37:25  jonas
    * patch from Peter for int64 function results

  Revision 1.102  2003/05/13 19:14:41  peter
    * failn removed
    * inherited result code check moven to pexpr

  Revision 1.101  2003/05/13 15:16:13  peter
    * removed ret_in_acc, it's the reverse of ret_in_param
    * fixed ret_in_param for win32 cdecl array

  Revision 1.100  2003/05/12 08:08:27  jonas
    * fixed several initialization and finalization related bugs (missing
      tg.direction's, wrong paralocation for decreasing refcount of
      everything but ansistrings)

  Revision 1.99  2003/05/11 21:37:03  peter
    * moved implicit exception frame from ncgutil to psub
    * constructor/destructor helpers moved from cobj/ncgutil to psub

  Revision 1.98  2003/05/11 14:45:12  peter
    * tloadnode does not support objectsymtable,withsymtable anymore
    * withnode cleanup
    * direct with rewritten to use temprefnode

  Revision 1.97  2003/05/10 13:20:23  jonas
    * moved storing of register parameters to memory even earlier in the
      entry code to fix problems with constructors

  Revision 1.96  2003/05/09 17:47:02  peter
    * self moved to hidden parameter
    * removed hdisposen,hnewn,selfn

  Revision 1.95  2003/04/29 07:28:52  michael
  + Patch from peter to fix wrong pushing of ansistring function results in open array

  Revision 1.94  2003/04/28 21:17:38  peter
    * do not finalize function results

  Revision 1.93  2003/04/27 16:30:34  jonas
    * store register para's to memory before copying the valuepara's, because
      that one requires them to be there already (and it calls subroutines ->
      could overwrite those registers)

  Revision 1.92  2003/04/27 11:21:33  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.91  2003/04/27 07:29:50  peter
    * current_procinfo.procdef cleanup, current_procinfo.procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.90  2003/04/26 17:21:08  florian
    * fixed passing of fpu values by fpu register

  Revision 1.89  2003/04/25 20:59:33  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.88  2003/04/23 12:35:34  florian
    * fixed several issues with powerpc
    + applied a patch from Jonas for nested function calls (PowerPC only)
    * ...

  Revision 1.87  2003/04/22 14:33:38  peter
    * removed some notes/hints

  Revision 1.86  2003/04/22 13:47:08  peter
    * fixed C style array of const
    * fixed C array passing
    * fixed left to right with high parameters

  Revision 1.85  2003/04/22 10:09:35  daniel
    + Implemented the actual register allocator
    + Scratch registers unavailable when new register allocator used
    + maybe_save/maybe_restore unavailable when new register allocator used

  Revision 1.84  2003/04/16 09:26:55  jonas
    * assembler procedures now again get a stackframe if they have local
      variables. No space is reserved for a function result however.
      Also, the register parameters aren't automatically saved on the stack
      anymore in assembler procedures.

  Revision 1.83  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.82  2003/03/30 20:59:07  peter
    * fix classmethod from classmethod call
    * move BeforeDestruction/AfterConstruction calls to
      genentrycode/genexitcode instead of generating them on the fly
      after a call to a constructor

  Revision 1.81  2003/03/28 19:16:56  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.80  2003/03/17 15:52:20  peter
    * fix range error

  Revision 1.79  2003/03/11 21:46:24  jonas
    * lots of new regallocator fixes, both in generic and ppc-specific code
      (ppc compiler still can't compile the linux system unit though)

  Revision 1.78  2003/02/26 21:15:43  daniel
    * Fixed the optimizer

  Revision 1.77  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.76  2003/02/15 22:17:38  carl
   * bugfix of FPU emulation code

  Revision 1.75  2003/01/09 22:00:53  florian
    * fixed some PowerPC issues

  Revision 1.74  2003/01/09 20:41:10  florian
    * fixed broken PowerPC compiler

  Revision 1.73  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.72  2002/12/29 23:51:43  florian
    * web bug 2214 fixed: ie 10 in const array constructors

  Revision 1.71  2002/12/24 15:56:50  peter
    * stackpointer_alloc added for adjusting ESP. Win32 needs
      this for the pageprotection

  Revision 1.70  2002/12/05 14:39:21  florian
    * added missing then, Carl did you really a make fullcycle :) ?

  Revision 1.69  2002/12/03 22:13:39  carl
     * bugfix of problems with profile code which clobbers some registers

  Revision 1.68  2002/12/01 22:06:59  carl
    * warning of portabilitiy problems with parasize / localsize

  Revision 1.67  2002/11/30 18:44:57  carl
    + profiling support for Win32

  Revision 1.66  2002/11/30 14:39:15  carl
    * try to fix profiling for win32

  Revision 1.65  2002/11/28 23:28:14  florian
    * push_value_para didn't release floatdef locations, fixes tw2045

  Revision 1.64  2002/11/27 02:33:19  peter
    * copy_value_on_stack method added for cdecl record passing

  Revision 1.63  2002/11/25 17:43:18  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.62  2002/11/18 17:31:55  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.61  2002/11/17 17:49:08  mazen
  + return_result_reg and FUNCTION_RESULT_REG are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

  Revision 1.60  2002/11/17 16:31:56  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.59  2002/11/15 01:58:51  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.58  2002/11/10 19:07:45  mazen
  * SPARC calling mechanism almost OK (as in GCC./mppcsparc )

  Revision 1.57  2002/11/03 20:22:40  mazen
  * parameter handling updated

  Revision 1.56  2002/10/16 19:01:43  peter
    + $IMPLICITEXCEPTIONS switch to turn on/off generation of the
      implicit exception frames for procedures with initialized variables
      and for constructors. The default is on for compatibility

  Revision 1.55  2002/10/14 19:42:33  peter
    * only use init tables for threadvars

  Revision 1.54  2002/10/06 19:41:30  peter
    * Add finalization of typed consts
    * Finalization of globals in the main program

  Revision 1.53  2002/10/05 15:18:42  carl
    * fix heap leaks

  Revision 1.52  2002/09/30 07:00:46  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.51  2002/09/22 14:02:35  carl
    * stack checking cannot be called before system unit is initialized
    * MC68020 define

  Revision 1.50  2002/09/17 18:54:03  jonas
    * a_load_reg_reg() now has two size parameters: source and dest. This
      allows some optimizations on architectures that don't encode the
      register size in the register name.

  Revision 1.49  2002/09/10 21:48:30  florian
    * improved handling of procedures with register calling conventions

  Revision 1.48  2002/09/07 15:25:03  peter
    * old logs removed and tabs fixed

  Revision 1.47  2002/09/02 18:44:48  peter
    * fixed (not) pushing of empty parameters
    * fixed implicit initialization/finalization generation
    * fixed/optimized local copy of value arguments init/final

  Revision 1.46  2002/09/01 19:27:34  peter
    * use index register when available for generating a reference with
      only a signle register. Using the base register could possibly
      destroy the framepointer

  Revision 1.45  2002/09/01 18:50:20  peter
    * fixed maybe_save that did not support a reference with only
      a index register. It now also updates the location with the new
      base register only

  Revision 1.44  2002/09/01 14:42:41  peter
    * removevaluepara added to fix the stackpointer so restoring of
      saved registers works

  Revision 1.43  2002/08/25 19:25:18  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.42  2002/08/24 18:38:26  peter
    * really use tt_noreuse for exception frame buffers

  Revision 1.41  2002/08/23 16:14:49  peter
    * tempgen cleanup
    * tt_noreuse temp type added that will be used in genentrycode

  Revision 1.40  2002/08/18 10:42:37  florian
    * remaining assembler writer bugs fixed, the errors in the
      system unit are inline assembler problems

  Revision 1.39  2002/08/17 09:23:36  florian
    * first part of procinfo rewrite

  Revision 1.38  2002/08/16 14:24:57  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.37  2002/08/15 15:15:55  carl
    * jmpbuf size allocation for exceptions is now cpu specific (as it should)
    * more generic nodes for maths
    * several fixes for better m68k support

  Revision 1.36  2002/08/14 19:25:09  carl
    * fix Florian's last commit for m68k compilation

  Revision 1.35  2002/08/13 21:40:56  florian
    * more fixes for ppc calling conventions

  Revision 1.34  2002/08/12 15:08:39  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.33  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.32  2002/08/11 13:24:12  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.31  2002/08/09 19:16:57  carl
    * stack allocation is now done separately (at the end) of genentrycode
      so temps. can be allocated before.
    * fix generic exception handling

  Revision 1.30  2002/08/06 20:55:21  florian
    * first part of ppc calling conventions fix

  Revision 1.29  2002/08/04 19:09:22  carl
    + added generic exception support (still does not work!)
    + more documentation

  Revision 1.28  2002/07/29 21:23:42  florian
    * more fixes for the ppc
    + wrappers for the tcnvnode.first_* stuff introduced

}
