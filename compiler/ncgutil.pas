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
      cpubase,cpupara,
      aasmbase,aasmtai,aasmcpu,
      cginfo,symbase,symdef,symtype,
{$ifndef cpu64bit}
      cg64f32,
{$endif cpu64bit}
      rgobj;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

      tmaybesave = record
        saved : boolean;
        ref   : treference;
      end;

    procedure firstcomplex(p : tbinarynode);
    procedure maketojumpbool(list:TAAsmoutput; p : tnode; loadregvars: tloadregvars);
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs:Tsupregset);

    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
    procedure location_force_mem(list: TAAsmoutput;var l:tlocation);

    procedure maybe_save(list:taasmoutput;needed:integer;var l:tlocation;var s:tmaybesave);
    procedure maybe_restore(list:taasmoutput;var l:tlocation;const s:tmaybesave);
    function  maybe_pushfpu(list:taasmoutput;needed : byte;var l:tlocation) : boolean;

    procedure push_value_para(p:tnode;calloption:tproccalloption;
                              para_offset:longint;alignment : longint;
                              const locpara : tparalocation);

    procedure genentrycode(list : TAAsmoutput;
                           make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);
   procedure genexitcode(list : TAAsmoutput;parasize:longint;nostackframe,inlined:boolean);
   procedure genimplicitunitinit(list : TAAsmoutput);
   procedure genimplicitunitfinal(list : TAAsmoutput);

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

implementation

  uses
{$ifdef Delphi}
    Sysutils,
{$else}
    strings,
{$endif}
    cutils,cclasses,
    globals,systems,verbose,
    symconst,symsym,symtable,defutil,
    paramgr,fmodule,
    cgbase,regvars,
{$ifdef GDB}
    gdb,
{$endif GDB}
    ncon,
    tgobj,cgobj,cgcpu;


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
{$ifdef cpuflags}
                     LOC_FLAGS :
                       begin
                         cg.a_jmp_flags(list,p.location.resflags,
                           truelabel);
                         cg.a_jmp_always(list,falselabel);
                       end;
{$endif cpuflags}
                   end;
                end;
           end
         else
           internalerror(200112305);
         aktfilepos:=storepos;
      end;


    procedure remove_non_regvars_from_loc(const t: tlocation; var regs:Tsupregset);
      begin
        case t.loc of
          LOC_REGISTER:
            begin
              { can't be a regvar, since it would be LOC_CREGISTER then }
              if t.register.enum<>R_INTREGISTER then
                internalerror(200301154);
              if t.registerhigh.enum<>R_INTREGISTER then
                internalerror(200301154);
              exclude(regs,t.register.number shr 8);
              if t.registerhigh.enum <> R_NO then
                exclude(regs,t.registerhigh.number shr 8);
            end;
          LOC_CREFERENCE,LOC_REFERENCE:
            begin
              if t.reference.base.enum<>R_INTREGISTER then
                internalerror(200301154);
              if t.reference.index.enum<>R_INTREGISTER then
                internalerror(200301154);
              if not(cs_regalloc in aktglobalswitches) or
                 ((t.reference.base.number shr 8) in rg.usableregsint) then
                exclude(regs,t.reference.base.number shr 8);
              if not(cs_regalloc in aktglobalswitches) or
                 ((t.reference.index.number shr 8) in rg.usableregsint) then
                exclude(regs,t.reference.index.number);
            end;
        end;
      end;

{*****************************************************************************
                            EXCEPTION MANAGEMENT
*****************************************************************************}

    procedure new_exception(list : taasmoutput;const jmpbuf,envbuf, href : treference;
      a : aword; exceptlabel : tasmlabel);

    var r:Tregister;

     begin
       cg.a_paramaddr_ref(list,envbuf,paramanager.getintparaloc(3));
       cg.a_paramaddr_ref(list,jmpbuf,paramanager.getintparaloc(2));
       { push type of exceptionframe }
       cg.a_param_const(list,OS_S32,1,paramanager.getintparaloc(1));
       cg.a_call_name(list,'FPC_PUSHEXCEPTADDR');

       r.enum:=R_INTREGISTER;
       r.number:=NR_ACCUMULATOR;
       cg.a_param_reg(list,OS_ADDR,r,paramanager.getintparaloc(1));
       cg.a_call_name(list,'FPC_SETJMP');

       cg.g_exception_reason_save(list, href);
       cg.a_cmp_const_reg_label(list,OS_S32,OC_NE,0,r,exceptlabel);
     end;


    procedure free_exception(list : taasmoutput;const jmpbuf, envbuf, href : treference;
     a : aword ; endexceptlabel : tasmlabel; onlyfree : boolean);

    var r:Tregister;

     begin
         cg.a_call_name(list,'FPC_POPADDRSTACK');

         if not onlyfree then
          begin
            cg.g_exception_reason_load(list, href);
            r.enum:=R_INTREGISTER;
            r.number:=NR_ACCUMULATOR;
            cg.a_cmp_const_reg_label(list,OS_S32,OC_EQ,a,r,endexceptlabel);
          end;
     end;


{*****************************************************************************
                                     TLocation
*****************************************************************************}

{$ifndef cpu64bit}
    { 32-bit version }
    procedure location_force(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister,
        hregisterhi : tregister;
        hreg64 : tregister64;
        hl : tasmlabel;
     begin
        { handle transformations to 64bit separate }
        if dst_size in [OS_64,OS_S64] then
         begin
           if not (l.size in [OS_64,OS_S64]) then
            begin
              { load a smaller size to OS_64 }
              if l.loc=LOC_REGISTER then
               begin
                 hregister.enum:=R_INTREGISTER;
                 hregister.number:=(l.registerlow.number and not $ff) or R_SUBWHOLE;
                 cg.a_load_reg_reg(list,l.size,OS_32,l.registerlow,hregister);
               end
              else
               hregister:=rg.getregisterint(list,OS_INT);
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
                  cg.a_load_loc_reg(list,l,hregister);
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
           { transformations to 32bit or smaller }
           if l.loc=LOC_REGISTER then
            begin
              { if the previous was 64bit release the high register }
              if l.size in [OS_64,OS_S64] then
               begin
                 rg.ungetregisterint(list,l.registerhigh);
                 l.registerhigh.enum:=R_NO;
               end;
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
               hregister:=rg.getregisterint(list,OS_INT);
            end;
           if hregister.enum<>R_INTREGISTER then
            internalerror(200302022);
           hregister.number:=(hregister.number and not $ff) or cgsize2subreg(dst_size);
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
                     l.register.number:=(l.register.number and not $ff) or cgsize2subreg(dst_size);
                    { for big endian systems, the reference's offset must }
                    { be increased in this case, since they have the      }
                    { MSB first in memory and e.g. byte(word_var) should  }
                    { return  the second byte in this case (JM)           }
                    if (target_info.endian = ENDIAN_BIG) and
                       (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                      inc(l.reference.offset,TCGSize2Size[l.size]-TCGSize2Size[dst_size]);
                    l.size:=dst_size;
                  end;
                 cg.a_load_loc_reg(list,l,hregister);
               end;
           end;
           location_reset(l,LOC_REGISTER,dst_size);
           l.register:=hregister;
         end;
     end;

{$else cpu64bit}

    { 64-bit version }
    procedure location_force(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister : tregister;
        hl : tasmlabel;
     begin
        { handle transformations to 64bit separate }
        if dst_size in [OS_64,OS_S64] then
         begin
              { load a smaller size to OS_64 }
              if l.loc=LOC_REGISTER then
               hregister:=rg.makeregsize(l.register,OS_INT)
              else
               hregister:=rg.getregisterint(list,OS_INT);
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
                  cg.a_load_loc_reg(list,l,hregister);
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
               hregister:=rg.getregisterint(list,OS_INT);
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
                    l.size:=dst_size;
                  end;
                  
                 cg.a_load_loc_reg(list,l,hregister);
               end;
           end;
           location_reset(l,LOC_REGISTER,dst_size);
           l.register:=hregister;
         end;
     end;
{$endif cpu64bit}

    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      begin
        { release previous location before demanding a new register }
        if (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
         begin
           location_freetemp(list,l);
           location_release(list,l);
         end;
        location_force(list, l, dst_size, maybeconst)
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
              location_release(exprasmlist,l);
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_CONSTANT,
          LOC_REGISTER,
          LOC_CREGISTER :
            begin
              tg.GetTemp(list,TCGSize2Size[l.size],tt_normal,r);
              if l.size in [OS_64,OS_S64] then
               cg64.a_load64_loc_ref(list,l,r)
              else
               cg.a_load_loc_ref(list,l,r);
              location_release(exprasmlist,l);
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

    procedure maybe_save(list:taasmoutput;needed:integer;var l:tlocation;var s:tmaybesave);
      begin
        s.saved:=false;
        if l.loc=LOC_CREGISTER then
         begin
           s.saved:=true;
           exit;
         end;
        if needed>rg.countunusedregsint then
         begin
           case l.loc of
             LOC_REGISTER :
               begin
{$ifndef cpu64bit}
                 if l.size in [OS_64,OS_S64] then
                  begin
                    tg.GetTemp(exprasmlist,8,tt_normal,s.ref);
                    cg64.a_load64_reg_ref(exprasmlist,joinreg64(l.registerlow,l.registerhigh),s.ref);
                  end
                 else
{$endif cpu64bit}
                  begin
                    tg.GetTemp(exprasmlist,TCGSize2Size[l.size],tt_normal,s.ref);
                    cg.a_load_reg_ref(exprasmlist,l.size,l.register,s.ref);
                  end;
                 location_release(exprasmlist,l);
                 s.saved:=true;
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 if l.reference.base.enum<>R_INTREGISTER then
                   internalerror(200302101);
                 if l.reference.index.enum<>R_INTREGISTER then
                   internalerror(200302101);
                 if ((l.reference.base.number<>NR_NO) or
                     (l.reference.index.number<>NR_NO)) then
                  begin
                    { load address into a single base register }
                    if l.reference.index.number<>NR_NO then
                     begin
                       cg.a_loadaddr_ref_reg(list,l.reference,l.reference.index);
                       rg.ungetregisterint(list,l.reference.base);
                       reference_reset_base(l.reference,l.reference.index,0);
                     end
                    else
                     begin
                       cg.a_loadaddr_ref_reg(list,l.reference,l.reference.base);
                       rg.ungetregisterint(list,l.reference.index);
                       reference_reset_base(l.reference,l.reference.base,0);
                     end;
                    { save base register }
                    tg.GetTemp(exprasmlist,TCGSize2Size[OS_ADDR],tt_normal,s.ref);
                    cg.a_load_reg_ref(exprasmlist,OS_ADDR,l.reference.base,s.ref);
                    { release }
                    location_release(exprasmlist,l);
                    s.saved:=true;
                  end;
               end;
           end;
         end;
      end;


    procedure maybe_restore(list:taasmoutput;var l:tlocation;const s:tmaybesave);
      begin
        if not s.saved then
         exit;
        if l.loc=LOC_CREGISTER then
         begin
           load_regvar_reg(list,l.register);
           exit;
         end;
        case l.loc of
          LOC_REGISTER :
            begin
{$ifndef cpu64bit}
              if l.size in [OS_64,OS_S64] then
               begin
                 l.registerlow:=rg.getregisterint(exprasmlist,OS_INT);
                 l.registerhigh:=rg.getregisterint(exprasmlist,OS_INT);
                 cg64.a_load64_ref_reg(exprasmlist,s.ref,joinreg64(l.registerlow,l.registerhigh));
               end
              else
{$endif cpu64bit}
               begin
                 l.register:=rg.getregisterint(exprasmlist,OS_INT);
                 cg.a_load_ref_reg(exprasmlist,OS_INT,s.ref,l.register);
               end;
            end;
          LOC_CREFERENCE,
          LOC_REFERENCE :
            begin
              reference_reset(l.reference);
              l.reference.base:=rg.getaddressregister(exprasmlist);
              cg.a_load_ref_reg(exprasmlist,OS_ADDR,s.ref,l.reference.base);
            end;
        end;
        tg.ungetiftemp(exprasmlist,s.ref);
      end;


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


{*****************************************************************************
                                Push Value Para
*****************************************************************************}

    procedure push_value_para(p:tnode;calloption:tproccalloption;
                              para_offset:longint;alignment : longint;
                              const locpara : tparalocation);
      var
        tempreference : treference;
        href : treference;
        hreg : tregister;
        sizetopush,
        size : longint;
        cgsize : tcgsize;
        r:Tregister;
      begin
        { we've nothing to push when the size of the parameter is 0 }
        if p.resulttype.def.size=0 then
         exit;

        { Move flags and jump in register to make it less complex }
        if p.location.loc in [LOC_FLAGS,LOC_JUMP] then
         location_force_reg(exprasmlist,p.location,def_cgsize(p.resulttype.def),false);

        { Handle Floating point types differently }
        if p.resulttype.def.deftype=floatdef then
         begin
           case p.location.loc of
             LOC_FPUREGISTER,
             LOC_CFPUREGISTER:
               begin
{$ifdef i386}
                  size:=align(tfloatdef(p.resulttype.def).size,alignment);
                  inc(pushedparasize,size);

                  if calloption<>pocall_inline then
                   cg.g_stackpointer_alloc(exprasmlist,size);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmList.first=exprasmList.last) then
                    exprasmList.concat(Tai_force_line.Create);
{$endif GDB}

                  { this is the easiest case for inlined !! }
                  r.enum:=stack_pointer_reg;
                  if calloption=pocall_inline then
                   reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize)
                  else
                   reference_reset_base(href,r,0);

                  cg.a_loadfpu_reg_ref(exprasmlist,
                    def_cgsize(p.resulttype.def),p.location.register,href);
{$else i386}
                  cg.a_paramfpu_reg(exprasmlist,def_cgsize(p.resulttype.def),p.location.register,locpara);
{$endif i386}

               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 sizetopush:=align(p.resulttype.def.size,alignment);
                 tempreference:=p.location.reference;
                 inc(tempreference.offset,sizetopush);
                 while (sizetopush>0) do
                  begin
                    if sizetopush>=4 then
                     begin
                       cgsize:=OS_32;
                       inc(pushedparasize,4);
                       dec(tempreference.offset,4);
                       dec(sizetopush,4);
                     end
                    else
                     begin
                       cgsize:=OS_16;
                       inc(pushedparasize,2);
                       dec(tempreference.offset,2);
                       dec(sizetopush,2);
                     end;
                    if calloption=pocall_inline then
                     begin
                       reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                       cg.a_load_ref_ref(exprasmlist,cgsize,tempreference,href);
                     end
                    else
                     cg.a_param_ref(exprasmlist,cgsize,tempreference,locpara);
                  end;
               end;
             else
               internalerror(200204243);
           end;
           location_release(exprasmlist,p.location);
         end
        else
         begin
           { copy the value on the stack or use normal parameter push? }
           if paramanager.copy_value_on_stack(p.resulttype.def,calloption) then
            begin
{$ifdef i386}
              if not (p.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                internalerror(200204241);
              { push on stack }
              size:=align(p.resulttype.def.size,alignment);
              inc(pushedparasize,size);
              cg.g_stackpointer_alloc(exprasmlist,size);
              r.enum:=R_INTREGISTER;
              r.number:=NR_STACK_POINTER_REG;
              reference_reset_base(href,r,0);
              cg.g_concatcopy(exprasmlist,p.location.reference,href,size,false,false);
{$else i386}
              cg.a_param_copy_ref(exprasmlist,p.resulttype.def.size,p.location.reference,locpara);
{$endif i386}
            end
           else
            begin
              case p.location.loc of
                LOC_CONSTANT,
                LOC_REGISTER,
                LOC_CREGISTER,
                LOC_REFERENCE,
                LOC_CREFERENCE :
                  begin
                    cgsize:=def_cgsize(p.resulttype.def);
                    if cgsize in [OS_64,OS_S64] then
                     begin
                       inc(pushedparasize,8);
                       if calloption=pocall_inline then
                        begin
                          reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                          if p.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                            begin
                              size:=align(p.resulttype.def.size,alignment);
                              cg.g_concatcopy(exprasmlist,p.location.reference,href,size,false,false)
                            end
                          else
                            cg64.a_load64_loc_ref(exprasmlist,p.location,href);
                        end
                       else
                        cg64.a_param64_loc(exprasmlist,p.location,locpara);
                     end
                    else
                     begin
                       case cgsize of
                         OS_8,OS_S8 :
                           begin
                             if alignment=4 then
                              cgsize:=OS_32
                             else
                              cgsize:=OS_16;
                           end;
                         OS_16,OS_S16 :
                           begin
                             if alignment=4 then
                              cgsize:=OS_32;
                           end;
                       end;
                       { update register to use to match alignment }
                       if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                        begin
                          if p.location.register.enum<>R_INTREGISTER then
                            internalerror(200302024);
                          hreg:=p.location.register;
                          p.location.register.number:=(p.location.register.number and not $ff) or cgsize2subreg(cgsize);
                        end;
                       inc(pushedparasize,alignment);
                       if calloption=pocall_inline then
                        begin
                          reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                          if p.location.loc in [LOC_REFERENCE,LOC_CREFERENCE] then
                            begin
                              size:=align(p.resulttype.def.size,alignment);
                              cg.g_concatcopy(exprasmlist,p.location.reference,href,size,false,false)
                            end
                          else
                            cg.a_load_loc_ref(exprasmlist,p.location,href);
                        end
                       else
                        cg.a_param_loc(exprasmlist,p.location,locpara);
                       { restore old register }
                       if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                         p.location.register:=hreg;
                     end;
                    location_release(exprasmlist,p.location);
                  end;
{$ifdef SUPPORT_MMX}
                LOC_MMXREGISTER,
                LOC_CMMXREGISTER:
                  begin
                     inc(pushedparasize,8);
                     if calloption=pocall_inline then
                       begin
                          reference_reset_base(href,procinfo.framepointer,para_offset-pushedparasize);
                          cg.a_loadmm_reg_ref(exprasmlist,p.location.register,href);
                       end
                     else
                      cg.a_parammm_reg(exprasmlist,p.location.register);
                  end;
{$endif SUPPORT_MMX}
                else
                  internalerror(200204241);
              end;
           end;
         end;
      end;


{****************************************************************************
                                 Entry/Exit Code
****************************************************************************}

    procedure copyvalueparas(p : tnamedindexitem;arg:pointer);
      var
        href1,href2 : treference;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           (tvarsym(p).varspez=vs_value) and
           (paramanager.push_addr_param(tvarsym(p).vartype.def,procinfo.procdef.proccalloption)) then
         begin
           reference_reset_base(href1,procinfo.framepointer,tvarsym(p).address+procinfo.para_offset);
           if is_open_array(tvarsym(p).vartype.def) or
              is_array_of_const(tvarsym(p).vartype.def) then
             cg.g_copyvaluepara_openarray(list,href1,tarraydef(tvarsym(p).vartype.def).elesize)
           else
            begin
              reference_reset_base(href2,procinfo.framepointer,-tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup);
              if is_shortstring(tvarsym(p).vartype.def) then
               cg.g_copyshortstring(list,href1,href2,tstringdef(tvarsym(p).vartype.def).len,false,true)
              else
               cg.g_concatcopy(list,href1,href2,tvarsym(p).vartype.def.size,true,true);
            end;
         end;
      end;


    procedure removevalueparas(p : tnamedindexitem;arg:pointer);
      var
        href1 : treference;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           (tvarsym(p).varspez=vs_value) and
           (is_open_array(tvarsym(p).vartype.def) or
            is_array_of_const(tvarsym(p).vartype.def)) and
           (paramanager.push_addr_param(tvarsym(p).vartype.def,procinfo.procdef.proccalloption)) then
         begin
           reference_reset_base(href1,procinfo.framepointer,tvarsym(p).address+procinfo.para_offset);
           cg.g_removevaluepara_openarray(list,href1,tarraydef(tvarsym(p).vartype.def).elesize);
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
           not(vo_is_local_copy in tvarsym(p).varoptions) and
           assigned(tvarsym(p).vartype.def) and
           not(is_class(tvarsym(p).vartype.def)) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           if assigned(procinfo) and
              (cs_implicit_exceptions in aktmoduleswitches) then
            procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
           if tsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
            reference_reset_base(href,procinfo.framepointer,-tvarsym(p).address+tvarsym(p).owner.address_fixup)
           else
            reference_reset_symbol(href,objectlibrary.newasmsymbol(tvarsym(p).mangledname),0);
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
              if not(vo_is_local_copy in tvarsym(p).varoptions) and
                 assigned(tvarsym(p).vartype.def) and
                 not(is_class(tvarsym(p).vartype.def)) and
                 tvarsym(p).vartype.def.needs_inittable then
               begin
                 if tsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
                  reference_reset_base(href,procinfo.framepointer,-tvarsym(p).address+tvarsym(p).owner.address_fixup)
                 else
                  reference_reset_symbol(href,objectlibrary.newasmsymbol(tvarsym(p).mangledname),0);
                 cg.g_finalize(list,tvarsym(p).vartype.def,href,false);
               end;
            end;
          typedconstsym :
            begin
              if ttypedconstsym(p).is_writable and
                 ttypedconstsym(p).typedconsttype.def.needs_inittable then
               begin
                 reference_reset_symbol(href,objectlibrary.newasmsymbol(ttypedconstsym(p).mangledname),0);
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
                   procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
                 if assigned(tvarsym(p).localvarsym) then
                  reference_reset_base(href,procinfo.framepointer,
                      -tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup)
                 else
                  reference_reset_base(href,procinfo.framepointer,tvarsym(p).address+procinfo.para_offset);
                 cg.g_incrrefcount(list,tvarsym(p).vartype.def,href);
               end;
             vs_out :
               begin
                 reference_reset_base(href,procinfo.framepointer,tvarsym(p).address+procinfo.para_offset);
                 tmpreg:=cg.get_scratch_reg_address(list);
                 cg.a_load_ref_reg(list,OS_ADDR,href,tmpreg);
                 reference_reset_base(href,tmpreg,0);
                 cg.g_initialize(list,tvarsym(p).vartype.def,href,false);
                 cg.free_scratch_reg(list,tmpreg);
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
              if assigned(tvarsym(p).localvarsym) then
               reference_reset_base(href,procinfo.framepointer,
                   -tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup)
              else
               reference_reset_base(href,procinfo.framepointer,tvarsym(p).address+procinfo.para_offset);
              cg.g_decrrefcount(list,tvarsym(p).vartype.def,href);
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
                procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
              reference_reset_base(href,procinfo.framepointer,hp^.pos);
              cg.a_load_const_ref(list,OS_ADDR,0,href);
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
           case hp^.temptype of
             tt_ansistring,
             tt_freeansistring :
               begin
                 reference_reset_base(href,procinfo.framepointer,hp^.pos);
                 cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(1));
                 cg.a_call_name(list,'FPC_ANSISTR_DECR_REF');
               end;
             tt_widestring,
             tt_freewidestring :
               begin
                 reference_reset_base(href,procinfo.framepointer,hp^.pos);
                 cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
                 cg.a_call_name(list,'FPC_WIDESTR_DECR_REF');
               end;
             tt_interfacecom :
               begin
                 reference_reset_base(href,procinfo.framepointer,hp^.pos);
                 cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
                 cg.a_call_name(list,'FPC_INTF_DECR_REF');
               end;
           end;
           hp:=hp^.next;
         end;
      end;



    procedure handle_return_value(list:TAAsmoutput; inlined : boolean;var uses_acc,uses_acchi,uses_fpu : boolean);
      var
        href : treference;
        hreg,r,r2 : tregister;
        cgsize : TCGSize;
      begin
        if not is_void(aktprocdef.rettype.def) then
         begin
           if (tfuncretsym(aktprocdef.funcretsym).funcretstate<>vs_assigned) and
              (not inlined) then
            CGMessage(sym_w_function_result_not_set);
           reference_reset_base(href,procinfo.framepointer,procinfo.return_offset);
           cgsize:=def_cgsize(aktprocdef.rettype.def);
           case aktprocdef.rettype.def.deftype of
             orddef,
             enumdef :
               begin
                 uses_acc:=true;
{$WARNING accumulator was replaced by return_result_reg}
{Here, we return the function result. In most architectures, the value is
passed into the accumulator, but in a windowed architecure like sparc a
function returns in a register and the caller receives it in an other one}
                  r.enum:=R_INTREGISTER;
                  r.number:=NR_RETURN_RESULT_REG;
                  cg.a_reg_alloc(list,r);
{$ifndef cpu64bit}
                 if cgsize in [OS_64,OS_S64] then
                  begin
                    uses_acchi:=true;
                    r.enum:=accumulatorhigh;
                    cg.a_reg_alloc(list,r);
                    r.enum:=R_INTREGISTER;
                    r.number:=NR_ACCUMULATOR;
                    r2.enum:=R_INTREGISTER;
                    r2.number:=NR_ACCUMULATORHIGH;
                    cg64.a_load64_ref_reg(list,href,joinreg64(r,r2));
                  end
                 else
{$endif cpu64bit}
                  begin
{$WARNING accumulator was replaced by return_result_reg}
{Here, we return the function result. In most architectures, the value is
passed into the accumulator, but in a windowed architecure like sparc a
function returns in a register and the caller receives it in an other one}
                    hreg.enum:=R_INTREGISTER;
                    hreg.number:=RS_RETURN_RESULT_REG shl 8 or cgsize2subreg(cgsize);
                    cg.a_load_ref_reg(list,cgsize,href,hreg);
                  end;
               end;
             floatdef :
               begin
                 uses_fpu := true;
{$ifdef cpufpemu}
                  if cs_fp_emulation in aktmoduleswitches then
                    r.enum := accumulator
                 else
{$endif cpufpemu}
                  r.enum:=fpu_result_reg;
                 cg.a_loadfpu_ref_reg(list,cgsize,href,r);
               end;
             else
               begin
                 if paramanager.ret_in_acc(aktprocdef.rettype.def,aktprocdef.proccalloption) then
                  begin
                    uses_acc:=true;
                    r.enum:=R_INTREGISTER;
                    r.number:=NR_RETURN_RESULT_REG;
                    cg.a_reg_alloc(list,r);
{$ifndef cpu64bit}
                    { Win32 can return records in EAX:EDX }
                    if cgsize in [OS_64,OS_S64] then
                     begin
                       uses_acchi:=true;
                       r.enum:=accumulatorhigh;
                       cg.a_reg_alloc(list,r);
                       r.enum:=R_INTREGISTER;
                       r.number:=NR_ACCUMULATOR;
                       r2.enum:=R_INTREGISTER;
                       r2.number:=NR_ACCUMULATORHIGH;
                       cg64.a_load64_ref_reg(list,href,joinreg64(r,r2));
                     end
                    else
{$endif cpu64bit}
                     r.enum:=R_INTREGISTER;
                     r.number:=NR_ACCUMULATOR;
                     cg.a_load_ref_reg(list,cgsize,href,r);
                   end
               end;
           end;
         end;
      end;


    procedure handle_fast_exit_return_value(list:TAAsmoutput);
      var
        href : treference;
        hreg : tregister;
        cgsize : TCGSize;
        r,r2 : Tregister;
      begin
        if not is_void(aktprocdef.rettype.def) then
         begin
           reference_reset_base(href,procinfo.framepointer,procinfo.return_offset);
           cgsize:=def_cgsize(aktprocdef.rettype.def);
           case aktprocdef.rettype.def.deftype of
             orddef,
             enumdef :
               begin
{$ifndef cpu64bit}
                 r.enum:=accumulator;
                 r2.enum:=accumulatorhigh;
                 if cgsize in [OS_64,OS_S64] then
                   cg64.a_load64_reg_ref(list,joinreg64(r,r2),href)
                 else
{$endif cpu64bit}
                  begin
                    hreg:=rg.makeregsize(r,cgsize);
                    cg.a_load_reg_ref(list,cgsize,hreg,href);
                  end;
               end;
             floatdef :
               begin
{$ifdef cpufpemu}
                  if cs_fp_emulation in aktmoduleswitches then
                    r.enum := accumulator
                 else
{$endif cpufpemu}
                    r.enum:=fpu_result_reg;
                 cg.a_loadfpu_reg_ref(list,cgsize,r,href);
               end;
             else
               begin
                 r.enum:=accumulator;
                 if paramanager.ret_in_acc(aktprocdef.rettype.def,aktprocdef.proccalloption) then
                  cg.a_load_reg_ref(list,cgsize,r,href);
               end;
           end;
         end;
      end;


    procedure genentrycode(list : TAAsmoutput;
                           make_global:boolean;
                           stackframe:longint;
                           var parasize:longint;var nostackframe:boolean;
                           inlined : boolean);
      var
        hs : string;
        href : treference;
        stackalloclist : taasmoutput;
        hp : tparaitem;
        paraloc : tparalocation;
        r:Tregister;

      begin
        if not inlined then
           stackalloclist:=taasmoutput.Create;

        { the actual stack allocation code, symbol entry point and
          gdb stabs information is generated AFTER the rest of this
          code, since temp. allocation might occur before - carl
        }


        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in aktprocdef.procoptions) then
          cg.g_save_all_registers(list)
        else
         { should we save edi,esi,ebx like C ? }
         if (po_savestdregs in aktprocdef.procoptions) then
           cg.g_save_standard_registers(list,aktprocdef.usedintregisters);

        { the actual profile code can clobber some registers,
          therefore if the context must be saved, do it before
          the actual call to the profile code
        }
        if (cs_profile in aktmoduleswitches)
         and not(po_assembler in aktprocdef.procoptions)
         and not(inlined) then
          begin
            { non-win32 can call mcout even in main }
            if not (target_info.system in [system_i386_win32,system_i386_wdosx])  then
              cg.g_profilecode(list)
            else
            { wdosx, and win32 should not call mcount before monstartup has been called }
            if not (aktprocdef.proctypeoption=potype_proginit) then
              cg.g_profilecode(list);
          end;


        { a constructor needs a help procedure }
        if (aktprocdef.proctypeoption=potype_constructor) then
          cg.g_call_constructor_helper(list);

        { don't load ESI, does the caller }
        { we must do it for local function }
        { that can be called from a foreach_static }
        { of another object than self !! PM }
        if assigned(procinfo._class) and  { !!!!! shouldn't we load ESI always? }
           (lexlevel>normal_function_level) then
         cg.g_maybe_loadself(list);

        { When message method contains self as a parameter,
          we must load it into ESI }
        If (po_containsself in aktprocdef.procoptions) then
          begin
             r.enum:=R_INTREGISTER;
             r.number:=NR_SELF_POINTER_REG;
             list.concat(tai_regalloc.Alloc(r));
             reference_reset_base(href,procinfo.framepointer,procinfo.selfpointer_offset);
             cg.a_load_ref_reg(list,OS_ADDR,href,r);
          end;


        if not is_void(aktprocdef.rettype.def) then
          begin
             { for now the pointer to the result can't be a register }
             if not(paramanager.ret_in_reg(aktprocdef.rettype.def,aktprocdef.proccalloption)) then
               begin
                  paraloc:=paramanager.getfuncretparaloc(aktprocdef);
                  reference_reset_base(href,procinfo.framepointer,procinfo.return_offset);
                  case paraloc.loc of
                     LOC_CREGISTER,
                     LOC_REGISTER:
                       if not(paraloc.size in [OS_64,OS_S64]) then
                         cg.a_load_reg_ref(list,paraloc.size,paraloc.register,href)
                       else
                         cg64.a_load64_reg_ref(list,paraloc.register64,href);
                     LOC_CFPUREGISTER,
                     LOC_FPUREGISTER:
                       cg.a_load_reg_ref(list,paraloc.size,paraloc.register,href);
                     LOC_CMMREGISTER,
                     LOC_MMREGISTER:
                       cg.a_loadmm_reg_ref(list,paraloc.register,href);
                  end;
               end;

             { initialize return value }
             if (aktprocdef.rettype.def.needs_inittable) then
               begin
                  if (cs_implicit_exceptions in aktmoduleswitches) then
                    procinfo.flags:=procinfo.flags or pi_needs_implicit_finally;
                  reference_reset_base(href,procinfo.framepointer,procinfo.return_offset);
                  cg.g_initialize(list,aktprocdef.rettype.def,href,paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption));
               end;
          end;

        { initialisize local data like ansistrings }
        case aktprocdef.proctypeoption of
           potype_unitinit:
             begin
                { using current_module.globalsymtable is hopefully      }
                { more robust than symtablestack and symtablestack.next }
                tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}initialize_data,list);
                tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}initialize_data,list);
             end;
           { units have seperate code for initilization and finalization }
           potype_unitfinalize: ;
           { program init/final is generated in separate procedure }
           potype_proginit: ;
           else
             aktprocdef.localst.foreach_static({$ifndef TP}@{$endif}initialize_data,list);
        end;

        { initialisizes temp. ansi/wide string data }
        inittempvariables(list);

        { generate copies of call by value parameters }
        if not(po_assembler in aktprocdef.procoptions) then
          aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}copyvalueparas,list);

        if assigned(aktprocdef.parast) then
          begin
             aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}init_paras,list);

             { move register parameters which aren't regable into memory                                          }
             { we do this after init_paras because it saves some code in init_paras if parameters are in register }
             { instead in memory                                                                                  }
             hp:=tparaitem(procinfo.procdef.para.first);
             while assigned(hp) do
               begin
                  if Tvarsym(hp.parasym).reg.enum>lastreg then
                    internalerror(200301081);
                  if (tvarsym(hp.parasym).reg.enum<>R_NO) then
                    case hp.paraloc.loc of
                       LOC_CREGISTER,
                       LOC_REGISTER:
//                         if not(hp.paraloc.size in [OS_S64,OS_64]) then
                           cg.a_load_reg_reg(list,hp.paraloc.size,OS_32,hp.paraloc.register,tvarsym(hp.parasym).reg);
//                         else
//                           cg64.a_load64_reg_reg(list,hp.paraloc.register64,tvarsym(hp.parasym).reg);
                       LOC_CFPUREGISTER,
                       LOC_FPUREGISTER:
                         cg.a_loadfpu_reg_reg(list,hp.paraloc.register,tvarsym(hp.parasym).reg);
                    end
                  else if (hp.paraloc.loc in [LOC_REGISTER,LOC_FPUREGISTER,LOC_MMREGISTER,
                    LOC_CREGISTER,LOC_CFPUREGISTER,LOC_CMMREGISTER]) and
                    (tvarsym(hp.parasym).reg.enum=R_NO) then
                    begin
                       reference_reset_base(href,procinfo.framepointer,tvarsym(hp.parasym).address+
                         tvarsym(hp.parasym).owner.address_fixup);
                       case hp.paraloc.loc of
                          LOC_CREGISTER,
                          LOC_REGISTER:
                           if not(hp.paraloc.size in [OS_S64,OS_64]) then
                              cg.a_load_reg_ref(list,hp.paraloc.size,hp.paraloc.register,href)
                           else
                              cg64.a_load64_reg_ref(list,hp.paraloc.register64,href);
                          LOC_FPUREGISTER,
                          LOC_CFPUREGISTER:
                            cg.a_loadfpu_reg_ref(list,hp.paraloc.size,hp.paraloc.register,href);
                          else
                            internalerror(2002081302);
                       end;
                    end;
                  hp:=tparaitem(hp.next);
               end;
          end;

        if (not inlined) then
         begin
           { call startup helpers from main program }
           if (aktprocdef.proctypeoption=potype_proginit) then
            begin
              { initialize profiling for win32 }
              if (target_info.system in [system_i386_win32,system_i386_wdosx]) and
                 (cs_profile in aktmoduleswitches) then
               begin
                 reference_reset_symbol(href,objectlibrary.newasmsymbol('etext'),0);
                 cg.a_paramaddr_ref(list,href,paraloc);
                 reference_reset_symbol(href,objectlibrary.newasmsymbol('__image_base__'),0);
                 cg.a_paramaddr_ref(list,href,paraloc);
                 cg.a_call_name(list,'_monstartup');
               end;

              { initialize units }
              cg.a_call_name(list,'FPC_INITIALIZEUNITS');
            end;

           { do we need an exception frame because of ansi/widestrings/interfaces ? }
           if ((procinfo.flags and pi_needs_implicit_finally)<>0) and
              { but it's useless in init/final code of units }
              not(aktprocdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
            begin
              include(rg.usedinproc,accumulator);
              tg.GetTemp(list,JMP_BUF_SIZE,tt_noreuse,procinfo.exception_jmp_ref);
              tg.GetTemp(list,12,tt_noreuse,procinfo.exception_env_ref);
              tg.GetTemp(list,sizeof(aword),tt_noreuse,procinfo.exception_result_ref);
              new_exception(list,procinfo.exception_jmp_ref,
                  procinfo.exception_env_ref,
                  procinfo.exception_result_ref,1,aktexitlabel);
              { probably we've to reload self here }
              cg.g_maybe_loadself(list);
            end;

{$ifdef GDB}
           if (cs_debuginfo in aktmoduleswitches) then
            list.concat(Tai_force_line.Create);
{$endif GDB}
         end;

        if inlined then
         load_regvars(list,nil);

        {************************* Stack allocation **************************}
        { and symbol entry point as well as debug information                 }
        { will be inserted in front of the rest of this list.                 }
        { Insert alignment and assembler names }
        if not inlined then
         begin
           { Align, gprof uses 16 byte granularity }
           if (cs_profile in aktmoduleswitches) then
            stackalloclist.concat(Tai_align.Create(16))
           else
            stackalloclist.concat(Tai_align.Create(aktalignment.procalign));

           if (cs_profile in aktmoduleswitches) or
              (aktprocdef.owner.symtabletype=globalsymtable) or
              (assigned(procinfo._class) and (procinfo._class.owner.symtabletype=globalsymtable)) then
            make_global:=true;

           if make_global or ((procinfo.flags and pi_is_global) <> 0) then
            aktprocsym.is_global := True;

{$ifdef GDB}
           if (cs_debuginfo in aktmoduleswitches) then
            begin
              aktprocdef.concatstabto(stackalloclist);
              aktprocsym.isstabwritten:=true;
            end;
{$endif GDB}

           repeat
             hs:=aktprocdef.aliasnames.getfirst;
             if hs='' then
              break;
{$ifdef GDB}
             if (cs_debuginfo in aktmoduleswitches) and
                target_info.use_function_relative_addresses then
              stackalloclist.concat(Tai_stab_function_name.Create(strpnew(hs)));
{$endif GDB}
             if make_global then
              stackalloclist.concat(Tai_symbol.Createname_global(hs,0))
             else
              stackalloclist.concat(Tai_symbol.Createname(hs,0));
           until false;

          stackframe:=stackframe+tg.gettempsize;
{$ifndef m68k}
          { give a warning if the limit of local variables is reached }
          if stackframe > maxlocalsize then
            Message(cg_w_localsize_too_big);
{$endif}
{$ifndef powerpc}
           { at least for the ppc this applies always, so this code isn't usable (FK) }
           { omit stack frame ? }
           if (procinfo.framepointer.number=NR_STACK_POINTER_REG) then
            begin
              CGMessage(cg_d_stackframe_omited);
              nostackframe:=true;
              if (aktprocdef.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocdef.parast.datasize+procinfo.para_offset-4;
              if stackframe<>0 then
                cg.g_stackpointer_alloc(stackalloclist,stackframe);
            end
           else
{$endif powerpc}
            begin
              nostackframe:=false;
              if (aktprocdef.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocdef.parast.datasize+procinfo.para_offset-target_info.first_parm_offset;

              if (po_interrupt in aktprocdef.procoptions) then
                cg.g_interrupt_stackframe_entry(stackalloclist);

              cg.g_stackframe_entry(stackalloclist,stackframe);

              { never call stack checking before the standard system unit
                has not been initialized
              }
              if (cs_check_stack in aktlocalswitches) and (aktprocdef.proctypeoption<>potype_proginit) then
                cg.g_stackcheck(stackalloclist,stackframe);
            end;
            list.insertlist(stackalloclist);
            stackalloclist.free;
         end;
        {************************* End Stack allocation **************************}
      end;


   procedure genexitcode(list : TAAsmoutput;parasize:longint;nostackframe,inlined:boolean);
      var
{$ifdef GDB}
        stabsendlabel : tasmlabel;
        mangled_length : longint;
        p : pchar;
        st : string[2];
{$endif GDB}
        okexitlabel,
        noreraiselabel,nodestroycall : tasmlabel;
        tmpreg : tregister;
        href : treference;
        usesacc,
        usesacchi,
        usesself,usesfpu : boolean;
        pd : tprocdef;
        r,r2:Tregister;
      begin
        if aktexit2label.is_used and
           ((procinfo.flags and (pi_needs_implicit_finally or pi_uses_exceptions)) <> 0) then
          begin
            cg.a_jmp_always(list,aktexitlabel);
            cg.a_label(list,aktexit2label);
            handle_fast_exit_return_value(list);
          end;

        if aktexitlabel.is_used then
          list.concat(Tai_label.Create(aktexitlabel));

        cleanup_regvars(list);

        { call the destructor help procedure }
        if (aktprocdef.proctypeoption=potype_destructor) and
           assigned(procinfo._class) then
         cg.g_call_destructor_helper(list);

        { finalize temporary data }
        finalizetempvariables(list);

        { finalize local data like ansistrings}
        case aktprocdef.proctypeoption of
           potype_unitfinalize:
             begin
                { using current_module.globalsymtable is hopefully      }
                { more robust than symtablestack and symtablestack.next }
                tsymtable(current_module.globalsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data,list);
                tsymtable(current_module.localsymtable).foreach_static({$ifndef TP}@{$endif}finalize_data,list);
             end;
           { units/progs have separate code for initialization and finalization }
           potype_unitinit: ;
           { program init/final is generated in separate procedure }
           potype_proginit: ;
           else
             aktprocdef.localst.foreach_static({$ifndef TP}@{$endif}finalize_data,list);
        end;

        { finalize paras data }
        if assigned(aktprocdef.parast) then
          aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}final_paras,list);

        { do we need to handle exceptions because of ansi/widestrings ? }
        if not inlined and
           ((procinfo.flags and pi_needs_implicit_finally)<>0) and
           { but it's useless in init/final code of units }
           not(aktprocdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
          begin
             { the exception helper routines modify all registers }
             aktprocdef.usedintregisters:=all_intregisters;
             aktprocdef.usedotherregisters:=all_registers;
             objectlibrary.getlabel(noreraiselabel);
             free_exception(list,
                  procinfo.exception_jmp_ref,
                  procinfo.exception_env_ref,
                  procinfo.exception_result_ref,0,
                  noreraiselabel,false);
             tg.Ungettemp(list,procinfo.exception_jmp_ref);
             tg.Ungettemp(list,procinfo.exception_env_ref);
             tg.Ungettemp(list,procinfo.exception_result_ref);

             if (aktprocdef.proctypeoption=potype_constructor) then
               begin
                  if assigned(procinfo._class) then
                    begin
                       pd:=procinfo._class.searchdestructor;
                       if assigned(pd) then
                         begin
                            objectlibrary.getlabel(nodestroycall);
                            reference_reset_base(href,procinfo.framepointer,procinfo.selfpointer_offset);
                            cg.a_cmp_const_ref_label(list,OS_ADDR,OC_EQ,0,href,nodestroycall);
                            r.enum:=R_INTREGISTER;
                            r.number:=NR_SELF_POINTER_REG;
                            if is_class(procinfo._class) then
                             begin
                               cg.a_param_const(list,OS_INT,1,paramanager.getintparaloc(2));
                               cg.a_param_reg(list,OS_ADDR,r,paramanager.getintparaloc(1));
                             end
                            else if is_object(procinfo._class) then
                             begin
                               cg.a_param_reg(list,OS_ADDR,r,paramanager.getintparaloc(2));
                               reference_reset_symbol(href,objectlibrary.newasmsymbol(procinfo._class.vmt_mangledname),0);
                               cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(1));
                             end
                            else
                             Internalerror(200006164);
                            if (po_virtualmethod in pd.procoptions) then
                             begin
                               reference_reset_base(href,r,0);
                               tmpreg:=cg.get_scratch_reg_address(list);
                               cg.a_load_ref_reg(list,OS_ADDR,href,tmpreg);
                               reference_reset_base(href,tmpreg,procinfo._class.vmtmethodoffset(pd.extnumber));
                               cg.free_scratch_reg(list,tmpreg);
                               cg.a_call_ref(list,href);
                             end
                            else
                             cg.a_call_name(list,pd.mangledname);
                            { not necessary because the result is never assigned in the
                              case of an exception (FK) }
                            cg.a_label(list,nodestroycall);
                         end;
                    end
               end
             else
              begin
                { no constructor }
                { must be the return value finalized before reraising the exception? }
                if (not is_void(aktprocdef.rettype.def)) and
                   (aktprocdef.rettype.def.needs_inittable) and
                   ((aktprocdef.rettype.def.deftype<>objectdef) or
                    not is_class(aktprocdef.rettype.def)) then
                  begin
                     reference_reset_base(href,procinfo.framepointer,procinfo.return_offset);
                     cg.g_finalize(list,aktprocdef.rettype.def,href,paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption));
                  end;
              end;

             cg.a_call_name(list,'FPC_RERAISE');
             cg.a_label(list,noreraiselabel);
          end;

        { call __EXIT for main program }
        if (not DLLsource) and
           (not inlined) and
           (aktprocdef.proctypeoption=potype_proginit) then
         begin
           cg.a_call_name(list,'FPC_DO_EXIT');
         end;

        { handle return value, this is not done for assembler routines when
          they didn't reference the result variable }
        usesacc:=false;
        usesacchi:=false;
        usesself:=false;
        if not(po_assembler in aktprocdef.procoptions) or
           (assigned(aktprocdef.funcretsym) and
            (tfuncretsym(aktprocdef.funcretsym).refcount>1)) then
          begin
            if (aktprocdef.proctypeoption<>potype_constructor) then
              handle_return_value(list,inlined,usesacc,usesacchi,usesfpu)
            else
              begin
                { successful constructor deletes the zero flag }
                { and returns self in eax                   }
                { eax must be set to zero if the allocation failed !!! }
                objectlibrary.getlabel(okexitlabel);
                cg.a_jmp_always(list,okexitlabel);
                cg.a_label(list,faillabel);
                cg.g_call_fail_helper(list);
                cg.a_label(list,okexitlabel);

                { for classes this is done after the call to }
                { AfterConstruction                          }
                if is_object(procinfo._class) then
                  begin
                    r.enum:=R_INTREGISTER;
                    r.number:=NR_SELF_POINTER_REG;
                    r2.enum:=R_INTREGISTER;
                    r2.number:=NR_ACCUMULATOR;
                    cg.a_reg_alloc(list,r2);
                    cg.a_load_reg_reg(list,OS_ADDR,OS_ADDR,r,r2);
                    usesacc:=true;
                  end;
{$ifdef i386}
                r.enum:=R_INTREGISTER;
                r.number:=NR_SELF_POINTER_REG;
                list.concat(taicpu.op_reg_reg(A_TEST,S_L,r,r));
{$else}
{$warning constructor returns in flags for i386}
{$endif i386}
                usesself:=true;
              end;
          end;

        if aktexit2label.is_used and not aktexit2label.is_set then
          cg.a_label(list,aktexit2label);

{$ifdef GDB}
        if ((cs_debuginfo in aktmoduleswitches) and not inlined) then
          begin
            objectlibrary.getlabel(stabsendlabel);
            cg.a_label(list,stabsendlabel);
          end;
{$endif GDB}

        { remove copies of call by value parameters when there are also
          registers saved on the stack }
        if ((po_saveregisters in aktprocdef.procoptions) or
            (po_savestdregs in aktprocdef.procoptions)) and
           not(po_assembler in aktprocdef.procoptions) then
          aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}removevalueparas,list);

        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in aktprocdef.procoptions) then
          cg.g_restore_all_registers(list,usesself,usesacc,usesacchi)
        else
         { should we restore edi ? }
         if (po_savestdregs in aktprocdef.procoptions) then
           cg.g_restore_standard_registers(list,aktprocdef.usedintregisters);

        { remove stackframe }
        if not inlined then
         begin
           if (not nostackframe) then
            cg.g_restore_frame_pointer(list)
           else
            if (tg.gettempsize<>0) then
              begin
                 r.enum:=stack_pointer_reg;
                 cg.a_op_const_reg(list,OP_ADD,tg.gettempsize,r);
              end;
         end;

        { at last, the return is generated }
        if not inlined then
         begin
           if (po_interrupt in aktprocdef.procoptions) then
            cg.g_interrupt_stackframe_exit(list,usesself,usesacc,usesacchi)
           else
             begin
{$ifndef i386}
               { give a warning if the limit of parameters allowed for
                 certain processors is reached.
               }
               if (parasize > maxparasize) then
                 Message(cg_w_parasize_too_big);
{$endif}
               cg.g_return_from_proc(list,parasize);
             end;
         end;

        if not inlined then
          list.concat(Tai_symbol_end.Createname(aktprocdef.mangledname));

{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) and not inlined  then
          begin
            if assigned(procinfo._class) then
              if (not assigned(procinfo.parent) or
                 not assigned(procinfo.parent._class)) then
                begin
                  if (po_classmethod in aktprocdef.procoptions) or
                     ((po_virtualmethod in aktprocdef.procoptions) and
                      (potype_constructor=aktprocdef.proctypeoption)) or
                     (po_staticmethod in aktprocdef.procoptions) then
                    begin
                      list.concat(Tai_stabs.Create(strpnew(
                       '"pvmt:p'+tstoreddef(pvmttype.def).numberstring+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(procinfo.selfpointer_offset))));
                    end
                  else
                    begin
                      if not(is_class(procinfo._class)) then
                        st:='v'
                      else
                        st:='p';
                      list.concat(Tai_stabs.Create(strpnew(
                       '"$t:'+st+procinfo._class.numberstring+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(procinfo.selfpointer_offset))));
                    end;
                end
              else
                begin
                  if not is_class(procinfo._class) then
                    st:='*'
                  else
                    st:='';
                  list.concat(Tai_stabs.Create(strpnew(
                   '"$t:r'+st+procinfo._class.numberstring+'",'+
                   tostr(N_RSYM)+',0,0,'+tostr(stab_regindex[SELF_POINTER_REG]))));
                end;

            { define calling EBP as pseudo local var PM }
            { this enables test if the function is a local one !! }
            if  assigned(procinfo.parent) and (lexlevel>normal_function_level) then
              list.concat(Tai_stabs.Create(strpnew(
               '"parent_ebp:'+tstoreddef(voidpointertype.def).numberstring+'",'+
               tostr(N_LSYM)+',0,0,'+tostr(procinfo.framepointer_offset))));

            if (not is_void(aktprocdef.rettype.def)) then
              begin
                if paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption) then
                  list.concat(Tai_stabs.Create(strpnew(
                   '"'+aktprocsym.name+':X*'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                   tostr(N_tsym)+',0,0,'+tostr(procinfo.return_offset))))
                else
                  list.concat(Tai_stabs.Create(strpnew(
                   '"'+aktprocsym.name+':X'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                   tostr(N_tsym)+',0,0,'+tostr(procinfo.return_offset))));
                if (m_result in aktmodeswitches) then
                  if paramanager.ret_in_param(aktprocdef.rettype.def,aktprocdef.proccalloption) then
                    list.concat(Tai_stabs.Create(strpnew(
                     '"RESULT:X*'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(procinfo.return_offset))))
                  else
                    list.concat(Tai_stabs.Create(strpnew(
                     '"RESULT:X'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(procinfo.return_offset))));
              end;
            mangled_length:=length(aktprocdef.mangledname);
            getmem(p,2*mangled_length+50);
            strpcopy(p,'192,0,0,');
            strpcopy(strend(p),aktprocdef.mangledname);
            if (target_info.use_function_relative_addresses) then
              begin
                strpcopy(strend(p),'-');
                strpcopy(strend(p),aktprocdef.mangledname);
              end;
            list.concat(Tai_stabn.Create(strnew(p)));
            {List.concat(Tai_stabn.Create(strpnew('192,0,0,'
             +aktprocdef.mangledname))));
            p[0]:='2';p[1]:='2';p[2]:='4';
            strpcopy(strend(p),'_end');}
            strpcopy(p,'224,0,0,'+stabsendlabel.name);
            if (target_info.use_function_relative_addresses) then
              begin
                strpcopy(strend(p),'-');
                strpcopy(strend(p),aktprocdef.mangledname);
              end;
            list.concatlist(withdebuglist);
            list.concat(Tai_stabn.Create(strnew(p)));
             { strpnew('224,0,0,'
             +aktprocdef.mangledname+'_end'))));}
            freemem(p,2*mangled_length+50);
          end;
{$endif GDB}

        if inlined then
         cleanup_regvars(list);
      end;


    procedure genimplicitunitinit(list : TAAsmoutput);
      begin
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
            target_info.use_function_relative_addresses then
           list.concat(Tai_stab_function_name.Create(strpnew('INIT$$'+current_module.modulename^)));
{$endif GDB}
         list.concat(Tai_symbol.Createname_global('INIT$$'+current_module.modulename^,0));
         list.concat(Tai_symbol.Createname_global(target_info.cprefix+current_module.modulename^+'_init',0));
         { using current_module.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack.next }
         if assigned(current_module.globalsymtable) then
           tsymtable(current_module.globalsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         tsymtable(current_module.localsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         cg.g_return_from_proc(list,0);
      end;


    procedure genimplicitunitfinal(list : TAAsmoutput);
      begin
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
            target_info.use_function_relative_addresses then
           list.concat(Tai_stab_function_name.Create(strpnew('FINALIZE$$'+current_module.modulename^)));
{$endif GDB}
         list.concat(Tai_symbol.Createname_global('FINALIZE$$'+current_module.modulename^,0));
         list.concat(Tai_symbol.Createname_global(target_info.cprefix+current_module.modulename^+'_finalize',0));
         { using current_module.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack.next }
         if assigned(current_module.globalsymtable) then
           tsymtable(current_module.globalsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         tsymtable(current_module.localsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         cg.g_return_from_proc(list,0);
      end;



end.
{
  $Log$
  Revision 1.79  2003-03-11 21:46:24  jonas
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
  + return_result_reg and function_result_reg are now used, in all plateforms, to pass functions result between called function and its caller. See the explanation of each one

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
