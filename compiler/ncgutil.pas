{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

interface

    uses
      node,
      cginfo,
      cpubase;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

    procedure location_force_reg(var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
    procedure location_force_mem(var l:tlocation);

    procedure maketojumpbool(p : tnode; loadregvars: tloadregvars);

implementation

  uses
    globals,systems,verbose,
    types,
    aasm,cgbase,regvars,
    ncon,
    tgobj,cpuinfo,cgobj,cgcpu,rgobj,cg64f32;


{*****************************************************************************
                                     TLocation
*****************************************************************************}

    { 32-bit version }
    procedure location_force_reg32(var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister,
        hregisterhi : tregister;
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
               hregister:=rg.getregisterint(exprasmlist);
              { load value in low register }
              case l.loc of
                LOC_FLAGS :
                  cg.g_flags2reg(exprasmlist,l.resflags,hregister);
                LOC_JUMP :
                  begin
                    cg.a_label(exprasmlist,truelabel);
                    cg.a_load_const_reg(exprasmlist,OS_INT,1,hregister);
                    getlabel(hl);
                    cg.a_jmp_always(exprasmlist,hl);
                    cg.a_label(exprasmlist,falselabel);
                    cg.a_load_const_reg(exprasmlist,OS_INT,0,hregister);
                    cg.a_label(exprasmlist,hl);
                  end;
                else
                  cg.a_load_loc_reg(exprasmlist,l,hregister);
              end;
              { reset hi part, take care of the signed bit of the current value }
              hregisterhi:=rg.getregisterint(exprasmlist);
              if (dst_size=OS_S64) and
                 (l.size in [OS_S8,OS_S16,OS_S32]) then
               begin
                 if l.loc=LOC_CONSTANT then
                  begin
                    if (longint(l.value)<0) then
                     cg.a_load_const_reg(exprasmlist,OS_32,$ffffffff,hregisterhi)
                    else
                     cg.a_load_const_reg(exprasmlist,OS_32,0,hregisterhi);
                  end
                 else
                  begin
                    cg.a_load_reg_reg(exprasmlist,OS_32,hregister,hregisterhi);
                    cg.a_op_const_reg(exprasmlist,OP_SAR,31,hregisterhi);
                  end;
               end
              else
               cg.a_load_const_reg(exprasmlist,OS_32,0,hregisterhi);
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
                 hregister:=rg.getregisterint(exprasmlist);
                 hregisterhi:=rg.getregisterint(exprasmlist);
               end;
              { load value in new register }
              tcg64f32(cg).a_load64_loc_reg(exprasmlist,l,hregister,hregisterhi);
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
                 rg.ungetregisterint(exprasmlist,l.registerhigh);
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
               hregister:=rg.getregisterint(exprasmlist);
            end;
           hregister:=rg.makeregsize(hregister,dst_size);
           { load value in new register }
           case l.loc of
             LOC_FLAGS :
               cg.g_flags2reg(exprasmlist,l.resflags,hregister);
             LOC_JUMP :
               begin
                 cg.a_label(exprasmlist,truelabel);
                 cg.a_load_const_reg(exprasmlist,dst_size,1,hregister);
                 getlabel(hl);
                 cg.a_jmp_always(exprasmlist,hl);
                 cg.a_label(exprasmlist,falselabel);
                 cg.a_load_const_reg(exprasmlist,dst_size,0,hregister);
                 cg.a_label(exprasmlist,hl);
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
                    l.size:=dst_size;
                  end;
                 cg.a_load_loc_reg(exprasmlist,l,hregister);
               end;
           end;
           location_reset(l,LOC_REGISTER,dst_size);
           l.register:=hregister;
         end;
     end;

    { 64-bit version }
    procedure location_force_reg64(var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      var
        hregister,
        hregisterhi : tregister;
        hl : tasmlabel;
     begin
        { handle transformations to 64bit separate }
        if dst_size in [OS_64,OS_S64] then
         begin
              { load a smaller size to OS_64 }
              if l.loc=LOC_REGISTER then
               hregister:=rg.makeregsize(l.register,OS_INT)
              else
               hregister:=rg.getregisterint(exprasmlist);
              { load value in low register }
              case l.loc of
                LOC_FLAGS :
                  cg.g_flags2reg(exprasmlist,l.resflags,hregister);
                LOC_JUMP :
                  begin
                    cg.a_label(exprasmlist,truelabel);
                    cg.a_load_const_reg(exprasmlist,OS_INT,1,hregister);
                    getlabel(hl);
                    cg.a_jmp_always(exprasmlist,hl);
                    cg.a_label(exprasmlist,falselabel);
                    cg.a_load_const_reg(exprasmlist,OS_INT,0,hregister);
                    cg.a_label(exprasmlist,hl);
                  end;
                else
                  cg.a_load_loc_reg(exprasmlist,l,hregister);
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
               hregister:=rg.getregisterint(exprasmlist);
            end;
           hregister:=rg.makeregsize(hregister,dst_size);
           { load value in new register }
           case l.loc of
             LOC_FLAGS :
               cg.g_flags2reg(exprasmlist,l.resflags,hregister);
             LOC_JUMP :
               begin
                 cg.a_label(exprasmlist,truelabel);
                 cg.a_load_const_reg(exprasmlist,dst_size,1,hregister);
                 getlabel(hl);
                 cg.a_jmp_always(exprasmlist,hl);
                 cg.a_label(exprasmlist,falselabel);
                 cg.a_load_const_reg(exprasmlist,dst_size,0,hregister);
                 cg.a_label(exprasmlist,hl);
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
                    l.size:=dst_size;
                  end;
                 cg.a_load_loc_reg(exprasmlist,l,hregister);
               end;
           end;
           location_reset(l,LOC_REGISTER,dst_size);
           l.register:=hregister;
         end;
     end;

    procedure location_force_reg(var l:tlocation;dst_size:TCGSize;maybeconst:boolean);
      begin
        { release previous location before demanding a new register }
        if (l.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
         begin
           location_freetemp(exprasmlist,l);
           location_release(exprasmlist,l);
         end;
        if sizeof(aword) < 8 then
          location_force_reg32(l, dst_size, maybeconst)
        else
          location_force_reg64(l, dst_size, maybeconst);
      end;


    procedure location_force_mem(var l:tlocation);
      var
        r : treference;
      begin
        case l.loc of
          LOC_FPUREGISTER,
          LOC_CFPUREGISTER :
            begin
              tg.gettempofsizereference(exprasmlist,TCGSize2Size[l.size],r);
              cg.a_loadfpu_reg_ref(exprasmlist,l.size,l.register,r);
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_CONSTANT,
          LOC_REGISTER,
          LOC_CREGISTER :
            begin
              tg.gettempofsizereference(exprasmlist,TCGSize2Size[l.size],r);
              if l.size in [OS_64,OS_S64] then
               tcg64f32(cg).a_load64_loc_ref(exprasmlist,l,r)
              else
               cg.a_load_loc_ref(exprasmlist,l,r);
              location_reset(l,LOC_REFERENCE,l.size);
              l.reference:=r;
            end;
          LOC_CREFERENCE,
          LOC_REFERENCE : ;
          else
            internalerror(200203219);
        end;
      end;


    procedure maketojumpbool(p : tnode; loadregvars: tloadregvars);
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
                load_all_regvars(exprasmlist);
              if is_constboolnode(p) then
                begin
                   if tordconstnode(p).value<>0 then
                     cg.a_jmp_always(exprasmlist,truelabel)
                   else
                     cg.a_jmp_always(exprasmlist,falselabel)
                end
              else
                begin
                   opsize:=def_cgsize(p.resulttype.def);
                   case p.location.loc of
                     LOC_CREGISTER,LOC_REGISTER,LOC_CREFERENCE,LOC_REFERENCE :
                       begin
                         if (p.location.loc = LOC_CREGISTER) then
                           load_regvar_reg(exprasmlist,p.location.register);
                         cg.a_cmp_const_loc_label(exprasmlist,opsize,OC_NE,
                           0,p.location,truelabel);
                         { !!! should happen right after cmp (JM) }
                         location_release(exprasmlist,p.location);
                         cg.a_jmp_always(exprasmlist,falselabel);
                       end;
                     LOC_FLAGS :
                       begin
                         cg.a_jmp_flags(exprasmlist,p.location.resflags,
                           truelabel);
                         cg.a_jmp_always(exprasmlist,falselabel);
                       end;
                   end;
                end;
           end
         else
           internalerror(200112305);
         aktfilepos:=storepos;
      end;

end.

{
  $Log$
  Revision 1.10  2002-04-21 19:02:03  peter
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

  Revision 1.1  2001/12/30 17:24:48  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint
}
