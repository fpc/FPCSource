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
      node,
      cpubase,cpupara,
      aasmbase,aasmtai,aasmcpu,
      cginfo,
      rgobj;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

      tmaybesave = record
        saved : boolean;
        ref   : treference;
      end;

    procedure firstcomplex(p : tbinarynode);
    procedure maketojumpbool(list:TAAsmoutput; p : tnode; loadregvars: tloadregvars);
    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: tregisterset);

    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
    procedure location_force_mem(list: TAAsmoutput;var l:tlocation);

    procedure maybe_save(list:taasmoutput;needed:integer;var l:tlocation;var s:tmaybesave);
    procedure maybe_restore(list:taasmoutput;var l:tlocation;const s:tmaybesave);
    function  maybe_pushfpu(list:taasmoutput;needed : byte;var l:tlocation) : boolean;

    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
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


implementation

  uses
{$ifdef Delphi}
    Sysutils,
{$else}
    strings,
{$endif}
    cutils,cclasses,globtype,globals,systems,verbose,
    symbase,symconst,symtype,symsym,symdef,symtable,defbase,paramgr,
    fmodule,
    cgbase,regvars,
{$ifdef GDB}
    gdb,
{$endif GDB}
    ncon,
    tgobj,cpuinfo,cgobj,cgcpu,cg64f32;


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
            (p.left.resulttype.def.deftype=orddef) and
            (torddef(p.left.resulttype.def).typ in [bool8bit,bool16bit,bool32bit]) then
           begin
             { p.swaped:=false}
             if nf_swaped in p.flags then
               internalerror(234234);
           end
         else
           if (((p.location.loc=LOC_FPUREGISTER) and
                (p.right.registersfpu > p.left.registersfpu)) or
               ((((p.left.registersfpu = 0) and
                  (p.right.registersfpu = 0)) or
                 (p.location.loc<>LOC_FPUREGISTER)) and
                (p.left.registers32<p.right.registers32))) and
           { the following check is appropriate, because all }
           { 4 registers are rarely used and it is thereby   }
           { achieved that the extra code is being dropped   }
           { by exchanging not commutative operators     }
               (p.right.registers32<=c_countusableregsint) then
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
                     LOC_FLAGS :
                       begin
                         cg.a_jmp_flags(list,p.location.resflags,
                           truelabel);
                         cg.a_jmp_always(list,falselabel);
                       end;
                   end;
                end;
           end
         else
           internalerror(200112305);
         aktfilepos:=storepos;
      end;


    procedure remove_non_regvars_from_loc(const t: tlocation; var regs: tregisterset);
      begin
        case t.loc of
          LOC_REGISTER:
            begin
              { can't be a regvar, since it would be LOC_CREGISTER then }
              exclude(regs,t.register);
              if t.registerhigh <> R_NO then
                exclude(regs,t.registerhigh);
            end;
          LOC_CREFERENCE,LOC_REFERENCE:
            begin
              if not(cs_regalloc in aktglobalswitches) or
                 (t.reference.base in rg.usableregsint) then
                exclude(regs,t.reference.base);
              if not(cs_regalloc in aktglobalswitches) or
                 (t.reference.index in rg.usableregsint) then
              exclude(regs,t.reference.index);
            end;
        end;
      end;


{*****************************************************************************
                                     TLocation
*****************************************************************************}

    { 32-bit version }
    procedure location_force_reg32(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
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
               hregister:=rg.makeregsize(l.registerlow,OS_INT)
              else
               hregister:=rg.getregisterint(list);
              { load value in low register }
              case l.loc of
                LOC_FLAGS :
                  cg.g_flags2reg(list,l.resflags,hregister);
                LOC_JUMP :
                  begin
                    cg.a_label(list,truelabel);
                    cg.a_load_const_reg(list,OS_INT,1,hregister);
                    getlabel(hl);
                    cg.a_jmp_always(list,hl);
                    cg.a_label(list,falselabel);
                    cg.a_load_const_reg(list,OS_INT,0,hregister);
                    cg.a_label(list,hl);
                  end;
                else
                  cg.a_load_loc_reg(list,l,hregister);
              end;
              { reset hi part, take care of the signed bit of the current value }
              hregisterhi:=rg.getregisterint(list);
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
                    cg.a_load_reg_reg(list,OS_32,hregister,hregisterhi);
                    cg.a_op_const_reg(list,OP_SAR,31,hregisterhi);
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
                 hregister:=rg.getregisterint(list);
                 hregisterhi:=rg.getregisterint(list);
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
                 l.registerhigh:=R_NO;
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
               hregister:=rg.getregisterint(list);
            end;
           hregister:=rg.makeregsize(hregister,dst_size);
           { load value in new register }
           case l.loc of
             LOC_FLAGS :
               cg.g_flags2reg(list,l.resflags,hregister);
             LOC_JUMP :
               begin
                 cg.a_label(list,truelabel);
                 cg.a_load_const_reg(list,dst_size,1,hregister);
                 getlabel(hl);
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

    { 64-bit version }
    procedure location_force_reg64(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
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
               hregister:=rg.getregisterint(list);
              { load value in low register }
              case l.loc of
                LOC_FLAGS :
                  cg.g_flags2reg(list,l.resflags,hregister);
                LOC_JUMP :
                  begin
                    cg.a_label(list,truelabel);
                    cg.a_load_const_reg(list,OS_INT,1,hregister);
                    getlabel(hl);
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
               hregister:=rg.getregisterint(list);
            end;
           hregister:=rg.makeregsize(hregister,dst_size);
           { load value in new register }
           case l.loc of
             LOC_FLAGS :
               cg.g_flags2reg(list,l.resflags,hregister);
             LOC_JUMP :
               begin
                 cg.a_label(list,truelabel);
                 cg.a_load_const_reg(list,dst_size,1,hregister);
                 getlabel(hl);
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

    procedure location_force_reg(list: TAAsmoutput;var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      begin
        { release previous location before demanding a new register }
        if (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
         begin
           location_freetemp(list,l);
           location_release(list,l);
         end;
        if sizeof(aword) < 8 then
          location_force_reg32(list, l, dst_size, maybeconst)
        else
          location_force_reg64(list, l, dst_size, maybeconst);
      end;


    procedure location_force_mem(list: TAAsmoutput;var l:tlocation);
      var
        r : treference;
      begin
        case l.loc of
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              tg.gettempofsizereference(list,TCGSize2Size[l.size],r);
              cg.a_loadfpu_reg_ref(list,l.size,l.register,r);
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_CONSTANT,
          LOC_REGISTER,
          LOC_CREGISTER :
            begin
              tg.gettempofsizereference(list,TCGSize2Size[l.size],r);
              if l.size in [OS_64,OS_S64] then
               cg64.a_load64_loc_ref(list,l,r)
              else
               cg.a_load_loc_ref(list,l,r);
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
                 if l.size in [OS_64,OS_S64] then
                  begin
                    tg.gettempofsizereference(exprasmlist,8,s.ref);
                    cg64.a_load64_reg_ref(exprasmlist,joinreg64(l.registerlow,l.registerhigh),s.ref);
                  end
                 else
                  begin
                    tg.gettempofsizereference(exprasmlist,TCGSize2Size[l.size],s.ref);
                    cg.a_load_reg_ref(exprasmlist,l.size,l.register,s.ref);
                  end;
                 location_release(exprasmlist,l);
                 s.saved:=true;
               end;
             LOC_REFERENCE,
             LOC_CREFERENCE :
               begin
                 if ((l.reference.base<>R_NO) or
                     (l.reference.index<>R_NO)) then
                  begin
                    { load address into a single base register }
                    cg.a_loadaddr_ref_reg(list,l.reference,l.reference.base);
                    { save base register }
                    tg.gettempofsizereference(exprasmlist,TCGSize2Size[OS_ADDR],s.ref);
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
              if l.size in [OS_64,OS_S64] then
               begin
                 l.registerlow:=rg.getregisterint(exprasmlist);
                 l.registerhigh:=rg.getregisterint(exprasmlist);
                 cg64.a_load64_ref_reg(exprasmlist,s.ref,joinreg64(l.registerlow,l.registerhigh));
               end
              else
               begin
                 l.register:=rg.getregisterint(exprasmlist);
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
        if needed>=maxfpuregs then
          begin
            if l.loc = LOC_FPUREGISTER then
              begin
                location_force_mem(list,l);
                maybe_pushfpu:=true;
              end
            else
              maybe_pushfpu:=false;
          end
        else
          maybe_pushfpu:=false;
      end;


{*****************************************************************************
                                Push Value Para
*****************************************************************************}

    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                              para_offset:longint;alignment : longint;
                              const locpara : tparalocation);
      var
        tempreference : treference;
        href : treference;
        hreg : tregister;
        sizetopush,
        size : longint;
        cgsize : tcgsize;
      begin
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
                  size:=align(tfloatdef(p.resulttype.def).size,alignment);
                  inc(pushedparasize,size);
                  if not inlined then
                   cg.a_op_const_reg(exprasmlist,OP_SUB,size,STACK_POINTER_REG);
{$ifdef GDB}
                  if (cs_debuginfo in aktmoduleswitches) and
                     (exprasmList.first=exprasmList.last) then
                    exprasmList.concat(Tai_force_line.Create);
{$endif GDB}

                  { this is the easiest case for inlined !! }
                  if inlined then
                   reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize)
                  else
                   reference_reset_base(href,stack_pointer_reg,0);

                  cg.a_loadfpu_reg_ref(exprasmlist,
                    def_cgsize(p.resulttype.def),p.location.register,href);
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
                    if inlined then
                     begin
                       reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
                       cg.a_load_ref_ref(exprasmlist,cgsize,tempreference,href);
                     end
                    else
                     cg.a_param_ref(exprasmlist,cgsize,tempreference,locpara);
                  end;
               end;
             else
               internalerror(200204243);
           end;
         end
        else
         begin
           { call by value open array ? }
           if is_cdecl and
              paramanager.push_addr_param(p.resulttype.def) then
            begin
              if not (p.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
                internalerror(200204241);
              { push on stack }
              size:=align(p.resulttype.def.size,alignment);
              inc(pushedparasize,size);
              cg.a_op_const_reg(exprasmlist,OP_SUB,size,STACK_POINTER_REG);
              reference_reset_base(href,STACK_POINTER_REG,0);
              cg.g_concatcopy(exprasmlist,p.location.reference,href,size,false,false);
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
                       if inlined then
                        begin
                          reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
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
                          hreg:=p.location.register;
                          p.location.register:=rg.makeregsize(p.location.register,cgsize);
                        end;
                       inc(pushedparasize,alignment);
                       if inlined then
                        begin
                          reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
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
                     if inlined then
                       begin
                          reference_reset_base(href,procinfo^.framepointer,para_offset-pushedparasize);
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
           (paramanager.push_addr_param(tvarsym(p).vartype.def)) then
         begin
           reference_reset_base(href1,procinfo^.framepointer,tvarsym(p).address+procinfo^.para_offset);
           if is_open_array(tvarsym(p).vartype.def) or
              is_array_of_const(tvarsym(p).vartype.def) then
             cg.g_copyvaluepara_openarray(list,href1,tarraydef(tvarsym(p).vartype.def).elesize)
           else
            begin
              reference_reset_base(href2,procinfo^.framepointer,-tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup);
              if is_shortstring(tvarsym(p).vartype.def) then
               cg.g_copyshortstring(list,href1,href2,tstringdef(tvarsym(p).vartype.def).len,false,true)
              else
               cg.g_concatcopy(list,href1,href2,tvarsym(p).vartype.def.size,true,true);
            end;
         end;
      end;


    procedure initialize_threadvar(p : tnamedindexitem;arg:pointer);
      var
        href : treference;
        list : taasmoutput;
      begin
        list:=taasmoutput(arg);
        if (tsym(p).typ=varsym) and
           (vo_is_thread_var in tvarsym(p).varoptions) then
         begin
           cg.a_param_const(list,OS_INT,tvarsym(p).getsize,paramanager.getintparaloc(2));
           reference_reset_symbol(href,newasmsymbol(tvarsym(p).mangledname),0);
           cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(1));
           rg.saveregvars(list,all_registers);
           cg.a_call_name(list,'FPC_INIT_THREADVAR');
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
           assigned(tvarsym(p).vartype.def) and
           not(is_class(tvarsym(p).vartype.def)) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           if assigned(procinfo) then
            procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
           if tsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
            reference_reset_base(href,procinfo^.framepointer,-tvarsym(p).address+tvarsym(p).owner.address_fixup)
           else
            reference_reset_symbol(href,newasmsymbol(tvarsym(p).mangledname),0);
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
        if (tsym(p).typ=varsym) and
           assigned(tvarsym(p).vartype.def) and
           not(is_class(tvarsym(p).vartype.def)) and
           tvarsym(p).vartype.def.needs_inittable then
         begin
           if tsym(p).owner.symtabletype in [localsymtable,inlinelocalsymtable] then
            reference_reset_base(href,procinfo^.framepointer,-tvarsym(p).address+tvarsym(p).owner.address_fixup)
           else
            reference_reset_symbol(href,newasmsymbol(tvarsym(p).mangledname),0);
           cg.g_finalize(list,tvarsym(p).vartype.def,href,false);
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
                 procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
                 if assigned(tvarsym(p).localvarsym) then
                  reference_reset_base(href,procinfo^.framepointer,
                      -tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup)
                 else
                  reference_reset_base(href,procinfo^.framepointer,tvarsym(p).address+procinfo^.para_offset);
                 cg.g_incrrefcount(list,tvarsym(p).vartype.def,href);
               end;
             vs_out :
               begin
                 reference_reset_base(href,procinfo^.framepointer,tvarsym(p).address+procinfo^.para_offset);
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
               reference_reset_base(href,procinfo^.framepointer,
                   -tvarsym(p).localvarsym.address+tvarsym(p).localvarsym.owner.address_fixup)
              else
               reference_reset_base(href,procinfo^.framepointer,tvarsym(p).address+procinfo^.para_offset);
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
                               tt_interfacecom] then
            begin
              procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
              reference_reset_base(href,procinfo^.framepointer,hp^.pos);
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
                 reference_reset_base(href,procinfo^.framepointer,hp^.pos);
                 cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(1));
                 cg.a_call_name(list,'FPC_ANSISTR_DECR_REF');
               end;
             tt_widestring,
             tt_freewidestring :
               begin
                 reference_reset_base(href,procinfo^.framepointer,hp^.pos);
                 cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
                 cg.a_call_name(list,'FPC_WIDESTR_DECR_REF');
               end;
             tt_interfacecom :
               begin
                 reference_reset_base(href,procinfo^.framepointer,hp^.pos);
                 cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(2));
                 cg.a_call_name(list,'FPC_INTF_DECR_REF');
               end;
           end;
           hp:=hp^.next;
         end;
      end;


    procedure handle_return_value(list:TAAsmoutput; inlined : boolean;var uses_acc,uses_acchi : boolean);
      var
        href : treference;
        hreg : tregister;
        cgsize : TCGSize;
      begin
        if not is_void(aktprocdef.rettype.def) then
         begin
           if (tfuncretsym(aktprocdef.funcretsym).funcretstate<>vs_assigned) and
              (not inlined) then
            CGMessage(sym_w_function_result_not_set);
           reference_reset_base(href,procinfo^.framepointer,procinfo^.return_offset);
           cgsize:=def_cgsize(aktprocdef.rettype.def);
           case aktprocdef.rettype.def.deftype of
             orddef,
             enumdef :
               begin
                 uses_acc:=true;
                 cg.a_reg_alloc(list,accumulator);
                 if cgsize in [OS_64,OS_S64] then
                  begin
                    uses_acchi:=true;
                    cg.a_reg_alloc(list,accumulatorhigh);
                    cg64.a_load64_ref_reg(list,href,joinreg64(accumulator,accumulatorhigh));
                  end
                 else
                  begin
                    hreg:=rg.makeregsize(accumulator,cgsize);
                    cg.a_load_ref_reg(list,cgsize,href,hreg);
                  end;
               end;
             floatdef :
               begin
                 cg.a_loadfpu_ref_reg(list,cgsize,href,FPU_RESULT_REG);
               end;
             else
               begin
                 if paramanager.ret_in_acc(aktprocdef.rettype.def) then
                  begin
                    uses_acc:=true;
                    cg.a_reg_alloc(list,accumulator);
                    cg.a_load_ref_reg(list,cgsize,href,accumulator);
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
      begin
        if not is_void(aktprocdef.rettype.def) then
         begin
           reference_reset_base(href,procinfo^.framepointer,procinfo^.return_offset);
           cgsize:=def_cgsize(aktprocdef.rettype.def);
           case aktprocdef.rettype.def.deftype of
             orddef,
             enumdef :
               begin
                 if cgsize in [OS_64,OS_S64] then
                   cg64.a_load64_reg_ref(list,joinreg64(accumulator,accumulatorhigh),href)
                 else
                  begin
                    hreg:=rg.makeregsize(accumulator,cgsize);
                    cg.a_load_reg_ref(list,cgsize,hreg,href);
                  end;
               end;
             floatdef :
               begin
                 cg.a_loadfpu_reg_ref(list,cgsize,FPU_RESULT_REG,href);
               end;
             else
               begin
                 if paramanager.ret_in_acc(aktprocdef.rettype.def) then
                  cg.a_load_reg_ref(list,cgsize,accumulator,href);
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
        p : tsymtable;
        tempbuf : treference;
        tmpreg : tregister;
      begin
        { Insert alignment and assembler names }
        if not inlined then
         begin
           { Align, gprof uses 16 byte granularity }
           if (cs_profile in aktmoduleswitches) then
            list.concat(Tai_align.Create_op(16,$90))
           else
            list.concat(Tai_align.Create(aktalignment.procalign));

           if (cs_profile in aktmoduleswitches) or
              (aktprocdef.owner.symtabletype=globalsymtable) or
              (assigned(procinfo^._class) and (procinfo^._class.owner.symtabletype=globalsymtable)) then
            make_global:=true;

           if make_global or ((procinfo^.flags and pi_is_global) <> 0) then
            aktprocsym.is_global := True;

{$ifdef GDB}
           if (cs_debuginfo in aktmoduleswitches) then
            begin
              aktprocdef.concatstabto(list);
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
              list.concat(Tai_stab_function_name.Create(strpnew(hs)));
{$endif GDB}
             if make_global then
              list.concat(Tai_symbol.Createname_global(hs,0))
             else
              list.concat(Tai_symbol.Createname(hs,0));
           until false;

           { omit stack frame ? }
           if (procinfo^.framepointer=STACK_POINTER_REG) then
            begin
              CGMessage(cg_d_stackframe_omited);
              nostackframe:=true;
              if (aktprocdef.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocdef.parast.datasize+procinfo^.para_offset-4;
              if stackframe<>0 then
                cg.a_op_const_reg(list,OP_SUB,stackframe,procinfo^.framepointer);
            end
           else
            begin
              nostackframe:=false;
              if (aktprocdef.proctypeoption in [potype_unitinit,potype_proginit,potype_unitfinalize]) then
                parasize:=0
              else
                parasize:=aktprocdef.parast.datasize+procinfo^.para_offset-target_info.first_parm_offset;

              if (po_interrupt in aktprocdef.procoptions) then
                cg.g_interrupt_stackframe_entry(list);

              cg.g_stackframe_entry(list,stackframe);

              if (cs_check_stack in aktlocalswitches) then
                cg.g_stackcheck(list,stackframe);
            end;

           if (cs_profile in aktmoduleswitches) and
              not(po_assembler in aktprocdef.procoptions) then
            cg.g_profilecode(list);
         end;

        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in aktprocdef.procoptions) then
         cg.g_save_all_registers(list)
        else
         { should we save edi,esi,ebx like C ? }
         if (po_savestdregs in aktprocdef.procoptions) then
          cg.g_save_standard_registers(list);

        { a constructor needs a help procedure }
        if (aktprocdef.proctypeoption=potype_constructor) then
          cg.g_call_constructor_helper(list);

        { don't load ESI, does the caller }
        { we must do it for local function }
        { that can be called from a foreach_static }
        { of another object than self !! PM }
        if assigned(procinfo^._class) and  { !!!!! shouldn't we load ESI always? }
           (lexlevel>normal_function_level) then
         cg.g_maybe_loadself(list);

        { When message method contains self as a parameter,
          we must load it into ESI }
        If (po_containsself in aktprocdef.procoptions) then
          begin
             list.concat(tai_regalloc.Alloc(self_pointer_reg));
             reference_reset_base(href,procinfo^.framepointer,procinfo^.selfpointer_offset);
             cg.a_load_ref_reg(list,OS_ADDR,href,self_pointer_reg);
          end;

        { initialize return value }
        if (not is_void(aktprocdef.rettype.def)) and
           (aktprocdef.rettype.def.needs_inittable) then
          begin
             procinfo^.flags:=procinfo^.flags or pi_needs_implicit_finally;
             reference_reset_base(href,procinfo^.framepointer,procinfo^.return_offset);
             cg.g_initialize(list,aktprocdef.rettype.def,href,paramanager.ret_in_param(aktprocdef.rettype.def));
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
           else
             aktprocdef.localst.foreach_static({$ifndef TP}@{$endif}initialize_data,list);
        end;

        { initialisizes temp. ansi/wide string data }
        inittempvariables(list);

        { generate copies of call by value parameters }
        if not(po_assembler in aktprocdef.procoptions) and
           not(aktprocdef.proccalloption in [pocall_cdecl,pocall_cppdecl,pocall_palmossyscall,pocall_system]) then
          aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}copyvalueparas,list);

        if assigned( aktprocdef.parast) then
          aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}init_paras,list);

        if (not inlined) then
         begin
           { call startup helpers from main program }
           if (aktprocdef.proctypeoption=potype_proginit) then
            begin
              { initialize profiling for win32 }
              if (target_info.target in [target_I386_WIN32,target_I386_wdosx]) and
                 (cs_profile in aktmoduleswitches) then
                cg.a_call_name(list,'__monstartup');

              { add local threadvars in units (only if needed because not all platforms
                have threadvar support) }
              if have_local_threadvars then
                cg.a_call_name(list,'FPC_INITIALIZELOCALTHREADVARS');

              { add global threadvars }
              p:=symtablestack;
              while assigned(p) do
               begin
                 p.foreach_static({$ifndef TP}@{$endif}initialize_threadvar,list);
                 p:=p.next;
               end;

              { initialize units }
              cg.a_call_name(list,'FPC_INITIALIZEUNITS');
            end;

           { do we need an exception frame because of ansi/widestrings/interfaces ? }
           if ((procinfo^.flags and pi_needs_implicit_finally)<>0) and
              { but it's useless in init/final code of units }
              not(aktprocdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
            begin
              include(rg.usedinproc,accumulator);

              { allocate exception frame buffer }
              cg.a_op_const_reg(list,OP_SUB,36,STACK_POINTER_REG);
              tmpreg:=rg.getaddressregister(list);
              cg.a_load_reg_reg(list,OS_ADDR,STACK_POINTER_REG,tmpreg);
              reference_reset_base(tempbuf,tmpreg,0);
              cg.g_push_exception(list,tempbuf,1,aktexitlabel);
              reference_release(list,tempbuf);

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
        usesself : boolean;
        pd : tprocdef;
      begin
        if aktexit2label.is_used and
           ((procinfo^.flags and (pi_needs_implicit_finally or pi_uses_exceptions)) <> 0) then
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
           assigned(procinfo^._class) then
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
           { units have seperate code for initialization and finalization }
           potype_unitinit: ;
           else
             aktprocdef.localst.foreach_static({$ifndef TP}@{$endif}finalize_data,list);
        end;

        { finalize paras data }
        if assigned(aktprocdef.parast) then
          aktprocdef.parast.foreach_static({$ifndef TP}@{$endif}final_paras,list);

        { do we need to handle exceptions because of ansi/widestrings ? }
        if not inlined and
           ((procinfo^.flags and pi_needs_implicit_finally)<>0) and
           { but it's useless in init/final code of units }
           not(aktprocdef.proctypeoption in [potype_unitfinalize,potype_unitinit]) then
          begin
             { the exception helper routines modify all registers }
             aktprocdef.usedregisters:=all_registers;
             getlabel(noreraiselabel);
             cg.g_pop_exception(list,noreraiselabel);

             if (aktprocdef.proctypeoption=potype_constructor) then
               begin
                  if assigned(procinfo^._class) then
                    begin
                       pd:=procinfo^._class.searchdestructor;
                       if assigned(pd) then
                         begin
                            getlabel(nodestroycall);
                            reference_reset_base(href,procinfo^.framepointer,procinfo^.selfpointer_offset);
                            cg.a_cmp_const_ref_label(list,OS_ADDR,OC_EQ,0,href,nodestroycall);
                            if is_class(procinfo^._class) then
                             begin
                               cg.a_param_const(list,OS_INT,1,paramanager.getintparaloc(2));
                               cg.a_param_reg(list,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(1));
                             end
                            else if is_object(procinfo^._class) then
                             begin
                               cg.a_param_reg(list,OS_ADDR,self_pointer_reg,paramanager.getintparaloc(2));
                               reference_reset_symbol(href,newasmsymbol(procinfo^._class.vmt_mangledname),0);
                               cg.a_paramaddr_ref(list,href,paramanager.getintparaloc(1));
                             end
                            else
                             Internalerror(200006164);
                            if (po_virtualmethod in pd.procoptions) then
                             begin
                               reference_reset_base(href,self_pointer_reg,0);
                               tmpreg:=cg.get_scratch_reg_address(list);
                               cg.a_load_ref_reg(list,OS_ADDR,href,tmpreg);
                               reference_reset_base(href,tmpreg,procinfo^._class.vmtmethodoffset(pd.extnumber));
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
                     reference_reset_base(href,procinfo^.framepointer,procinfo^.return_offset);
                     cg.g_finalize(list,aktprocdef.rettype.def,href,paramanager.ret_in_param(aktprocdef.rettype.def));
                  end;
              end;

             cg.a_call_name(list,'FPC_RERAISE');
             cg.a_label(list,noreraiselabel);
          end;

        { call __EXIT for main program }
        if (not DLLsource) and
           (not inlined) and
           (aktprocdef.proctypeoption=potype_proginit) then
         cg.a_call_name(list,'FPC_DO_EXIT');

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
              handle_return_value(list,inlined,usesacc,usesacchi)
            else
              begin
                { successful constructor deletes the zero flag }
                { and returns self in eax                   }
                { eax must be set to zero if the allocation failed !!! }
                getlabel(okexitlabel);
                cg.a_jmp_always(list,okexitlabel);
                cg.a_label(list,faillabel);
                cg.g_call_fail_helper(list);
                cg.a_label(list,okexitlabel);

                { for classes this is done after the call to }
                { AfterConstruction                          }
                if is_object(procinfo^._class) then
                  begin
                    cg.a_reg_alloc(list,accumulator);
                    cg.a_load_reg_reg(list,OS_ADDR,self_pointer_reg,accumulator);
                    usesacc:=true;
                  end;
{$ifdef i386}
                list.concat(taicpu.op_reg_reg(A_TEST,S_L,R_ESI,R_ESI));
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
            getlabel(stabsendlabel);
            cg.a_label(list,stabsendlabel);
          end;
{$endif GDB}

        { for the save all registers we can simply use a pusha,popa which
          push edi,esi,ebp,esp(ignored),ebx,edx,ecx,eax }
        if (po_saveregisters in aktprocdef.procoptions) then
          cg.g_restore_all_registers(list,usesself,usesacc,usesacchi)
        else
         { should we restore edi ? }
         if (po_savestdregs in aktprocdef.procoptions) then
           cg.g_restore_standard_registers(list);

        { remove stackframe }
        if not inlined then
         begin
           if (not nostackframe) then
            cg.g_restore_frame_pointer(list)
           else
            if (tg.gettempsize<>0) then
             cg.a_op_const_reg(list,OP_ADD,tg.gettempsize,STACK_POINTER_REG);
         end;

        { at last, the return is generated }
        if not inlined then
         begin
           if (po_interrupt in aktprocdef.procoptions) then
            cg.g_interrupt_stackframe_exit(list,usesself,usesacc,usesacchi)
           else
            cg.g_return_from_proc(list,parasize);
         end;

        if not inlined then
          list.concat(Tai_symbol_end.Createname(aktprocdef.mangledname));

{$ifdef GDB}
        if (cs_debuginfo in aktmoduleswitches) and not inlined  then
          begin
            if assigned(procinfo^._class) then
              if (not assigned(procinfo^.parent) or
                 not assigned(procinfo^.parent^._class)) then
                begin
                  if (po_classmethod in aktprocdef.procoptions) or
                     ((po_virtualmethod in aktprocdef.procoptions) and
                      (potype_constructor=aktprocdef.proctypeoption)) or
                     (po_staticmethod in aktprocdef.procoptions) then
                    begin
                      list.concat(Tai_stabs.Create(strpnew(
                       '"pvmt:p'+tstoreddef(pvmttype.def).numberstring+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(procinfo^.selfpointer_offset))));
                    end
                  else
                    begin
                      if not(is_class(procinfo^._class)) then
                        st:='v'
                      else
                        st:='p';
                      list.concat(Tai_stabs.Create(strpnew(
                       '"$t:'+st+procinfo^._class.numberstring+'",'+
                       tostr(N_tsym)+',0,0,'+tostr(procinfo^.selfpointer_offset))));
                    end;
                end
              else
                begin
                  if not is_class(procinfo^._class) then
                    st:='*'
                  else
                    st:='';
                  list.concat(Tai_stabs.Create(strpnew(
                   '"$t:r'+st+procinfo^._class.numberstring+'",'+
                   tostr(N_RSYM)+',0,0,'+tostr(GDB_i386index[SELF_POINTER_REG]))));
                end;

            { define calling EBP as pseudo local var PM }
            { this enables test if the function is a local one !! }
            if  assigned(procinfo^.parent) and (lexlevel>normal_function_level) then
              list.concat(Tai_stabs.Create(strpnew(
               '"parent_ebp:'+tstoreddef(voidpointertype.def).numberstring+'",'+
               tostr(N_LSYM)+',0,0,'+tostr(procinfo^.framepointer_offset))));

            if (not is_void(aktprocdef.rettype.def)) then
              begin
                if paramanager.ret_in_param(aktprocdef.rettype.def) then
                  list.concat(Tai_stabs.Create(strpnew(
                   '"'+aktprocsym.name+':X*'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                   tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))))
                else
                  list.concat(Tai_stabs.Create(strpnew(
                   '"'+aktprocsym.name+':X'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                   tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))));
                if (m_result in aktmodeswitches) then
                  if paramanager.ret_in_param(aktprocdef.rettype.def) then
                    list.concat(Tai_stabs.Create(strpnew(
                     '"RESULT:X*'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))))
                  else
                    list.concat(Tai_stabs.Create(strpnew(
                     '"RESULT:X'+tstoreddef(aktprocdef.rettype.def).numberstring+'",'+
                     tostr(N_tsym)+',0,0,'+tostr(procinfo^.return_offset))));
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
         { using current_module.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack.next }
         tsymtable(current_module.globalsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         tsymtable(current_module.localsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         list.insert(Tai_symbol.Createname_global('INIT$$'+current_module.modulename^,0));
         list.insert(Tai_symbol.Createname_global(target_info.cprefix+current_module.modulename^+'_init',0));
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
            target_info.use_function_relative_addresses then
           list.insert(Tai_stab_function_name.Create(strpnew('INIT$$'+current_module.modulename^)));
{$endif GDB}
         cg.g_return_from_proc(list,0);
      end;


    procedure genimplicitunitfinal(list : TAAsmoutput);
      begin
         { using current_module.globalsymtable is hopefully      }
         { more robust than symtablestack and symtablestack.next }
         tsymtable(current_module.globalsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         tsymtable(current_module.localsymtable).foreach_static({$ifdef FPCPROCVAR}@{$endif}finalize_data,list);
         list.insert(Tai_symbol.Createname_global('FINALIZE$$'+current_module.modulename^,0));
         list.insert(Tai_symbol.Createname_global(target_info.cprefix+current_module.modulename^+'_finalize',0));
{$ifdef GDB}
         if (cs_debuginfo in aktmoduleswitches) and
            target_info.use_function_relative_addresses then
           list.insert(Tai_stab_function_name.Create(strpnew('FINALIZE$$'+current_module.modulename^)));
{$endif GDB}
         cg.g_return_from_proc(list,0);
      end;



end.
{
  $Log$
  Revision 1.24  2002-07-25 17:58:24  carl
    + FPURESULTREG -> FPU_RESULT_REG

  Revision 1.23  2002/07/20 11:57:54  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.22  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.21  2002/07/11 07:33:25  jonas
    * big-endian fixes for location_force_reg*()

  Revision 1.20  2002/07/07 09:52:32  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.19  2002/07/01 18:46:23  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.18  2002/07/01 16:23:53  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.17  2002/05/20 13:30:40  carl
  * bugfix of hdisponen (base must be set, not index)
  * more portability fixes

  Revision 1.16  2002/05/18 13:34:09  peter
    * readded missing revisions

  Revision 1.15  2002/05/16 19:46:37  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.13  2002/05/13 19:54:37  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.12  2002/05/12 19:58:36  carl
  * some small portability fixes

  Revision 1.11  2002/05/12 16:53:07  peter
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

  Revision 1.10  2002/04/21 19:02:03  peter
    * removed newn and disposen nodes, the code is now directly
      inlined from pexpr
    * -an option that will write the secondpass nodes to the .s file, this
      requires EXTDEBUG define to actually write the info
    * fixed various internal errors and crashes due recent code changes

  Revision 1.9  2002/04/21 15:24:38  carl
  + a_jmp_cond -> a_jmp_always (a_jmp_cond is NOT portable)
  + changeregsize -> rg.makeregsize

  Revision 1.8  2002/04/19 15:39:34  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.7  2002/04/15 18:58:47  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.6  2002/04/06 18:10:42  jonas
    * several powerpc-related additions and fixes

  Revision 1.5  2002/04/04 19:05:57  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.4  2002/04/02 17:11:28  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.3  2002/03/31 20:26:34  jonas
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

  Revision 1.2  2002/03/04 19:10:11  peter
    * removed compiler warnings

}
