{
    $Id$
    Copyright (c) 1998-2002 by Florian Klaempfl

    Generate i386 inline nodes

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
unit n386inl;

{$i fpcdefs.inc}

interface

    uses
       node,ninl;

    type
       ti386inlinenode = class(tinlinenode)
          procedure pass_2;override;
       end;

implementation

    uses
      globtype,systems,
      cutils,verbose,globals,fmodule,
      symconst,symdef,defbase,
      aasmbase,aasmtai,aasmcpu,
      cginfo,cgbase,pass_1,pass_2,
      cpubase,paramgr,
      nbas,ncon,ncal,ncnv,nld,
      cga,tgobj,ncgutil,cgobj,cg64f32,rgobj,rgcpu;


{*****************************************************************************
                              TI386INLINENODE
*****************************************************************************}


    procedure ti386inlinenode.pass_2;
       const
         {tfloattype = (s32real,s64real,s80real,s64bit,f16bit,f32bit);}
{        float_name: array[tfloattype] of string[8]=
           ('S32REAL','S64REAL','S80REAL','S64BIT','F16BIT','F32BIT'); }
         addsubop:array[in_inc_x..in_dec_x] of TOpCG=(OP_ADD,OP_SUB);
       var
         asmop : tasmop;
         {inc/dec}
         addconstant : boolean;
         addvalue : longint;
         href : treference;
         hp2 : tstringconstnode;
         l : longint;
         pushedregs : tmaybesave;
         hregisterhi,
         hregister : tregister;
         lengthlab,
         otlabel,oflabel{,l1}   : tasmlabel;
         oldpushedparasize : longint;
         cgop : TOpCG;
         cgsize : TCGSize;
      begin
      { save & reset pushedparasize }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         case inlinenumber of
            in_assert_x_y:
              begin
                 { the node should be removed in the firstpass }
                 if not (cs_do_assertion in aktlocalswitches) then
                  internalerror(7123458);
                 otlabel:=truelabel;
                 oflabel:=falselabel;
                 getlabel(truelabel);
                 getlabel(falselabel);
                 secondpass(tcallparanode(left).left);
                 maketojumpbool(exprasmlist,tcallparanode(left).left,lr_load_regvars);
                 cg.a_label(exprasmlist,falselabel);
                 { erroraddr }
                 cg.a_param_reg(exprasmlist,OS_ADDR,R_EBP,paramanager.getintparaloc(4));
                 { lineno }
                 cg.a_param_const(exprasmlist,OS_INT,aktfilepos.line,paramanager.getintparaloc(3));
                 { filename string }
                 hp2:=cstringconstnode.createstr(current_module.sourcefiles.get_file_name(aktfilepos.fileindex),st_shortstring);
                 firstpass(hp2);
                 secondpass(hp2);
                 if codegenerror then
                  exit;
                 cg.a_paramaddr_ref(exprasmlist,hp2.location.reference,paramanager.getintparaloc(2));
                 hp2.free;
                 { push msg }
                 secondpass(tcallparanode(tcallparanode(left).right).left);
                 cg.a_paramaddr_ref(exprasmlist,tcallparanode(tcallparanode(left).right).left.location.reference,paramanager.getintparaloc(1));
                 { call }
                 cg.a_call_name(exprasmlist,'FPC_ASSERT');
                 cg.a_label(exprasmlist,truelabel);
                 truelabel:=otlabel;
                 falselabel:=oflabel;
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 location_reset(location,LOC_REGISTER,OS_ADDR);
                 { for both cases load vmt }
                 if left.nodetype=typen then
                   begin
                      hregister:=rg.getaddressregister(exprasmlist);
                      reference_reset_symbol(href,newasmsymbol(tobjectdef(left.resulttype.def).vmt_mangledname),0);
                      cg.a_loadaddr_ref_reg(exprasmlist,href,hregister);
                   end
                 else
                   begin
                      secondpass(left);
                      location_release(exprasmlist,left.location);
                      hregister:=rg.getaddressregister(exprasmlist);
                      { load VMT pointer }
                      inc(left.location.reference.offset,tobjectdef(left.resulttype.def).vmt_offset);
                      cg.a_load_ref_reg(exprasmlist,OS_ADDR,left.location.reference,hregister);
                   end;
                 { in sizeof load size }
                 if inlinenumber=in_sizeof_x then
                   begin
                      reference_reset_base(href,hregister,0);
                      rg.ungetaddressregister(exprasmlist,hregister);
                      hregister:=rg.getregisterint(exprasmlist);
                      cg.a_load_ref_reg(exprasmlist,OS_INT,href,hregister);
                   end;
                 location.register:=hregister;
              end;
            in_length_x :
              begin
                 secondpass(left);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(left.resulttype.def) or
                    is_widestring(left.resulttype.def) then
                  begin
                    location_force_reg(exprasmlist,left.location,OS_ADDR,false);
                    hregister:=left.location.register;
                    getlabel(lengthlab);
                    cg.a_cmp_const_reg_label(exprasmlist,OS_ADDR,OC_EQ,0,hregister,lengthlab);
                    reference_reset_base(href,hregister,-8);
                    cg.a_load_ref_reg(exprasmlist,OS_INT,href,hregister);
                    cg.a_label(exprasmlist,lengthlab);
                    location_reset(location,LOC_REGISTER,OS_INT);
                    location.register:=hregister;
                  end
                 else
                  begin
                    location_copy(location,left.location);
                    location.size:=OS_8;
                  end;
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(left);
                 if inlinenumber=in_pred_x then
                  cgop:=OP_SUB
                 else
                  cgop:=OP_ADD;
                 cgsize:=def_cgsize(resulttype.def);

                 { we need a value in a register }
                 location_copy(location,left.location);
                 location_force_reg(exprasmlist,location,cgsize,false);

                 if cgsize in [OS_64,OS_S64] then
                  cg64.a_op64_const_reg(exprasmlist,cgop,1,
                      location.register64)
                 else
                  cg.a_op_const_reg(exprasmlist,cgop,1,location.register);

                 cg.g_overflowcheck(exprasmlist,self);
                 cg.g_rangecheck(exprasmlist,self,resulttype.def);
              end;
            in_dec_x,
            in_inc_x :
              begin
                { set defaults }
                addconstant:=true;
                { load first parameter, must be a reference }
                secondpass(tcallparanode(left).left);
                cgsize:=def_cgsize(tcallparanode(left).left.resulttype.def);
                { get addvalue }
                case tcallparanode(left).left.resulttype.def.deftype of
                  orddef,
                  enumdef :
                    addvalue:=1;
                  pointerdef :
                    begin
                      if is_void(tpointerdef(tcallparanode(left).left.resulttype.def).pointertype.def) then
                       addvalue:=1
                      else
                       addvalue:=tpointerdef(tcallparanode(left).left.resulttype.def).pointertype.def.size;
                    end;
                  else
                    internalerror(10081);
                end;
                { second argument specified?, must be a s32bit in register }
                if assigned(tcallparanode(left).right) then
                 begin
                   maybe_save(exprasmlist,tcallparanode(tcallparanode(left).right).left.registers32,
                     tcallparanode(left).left.location,pushedregs);
                   secondpass(tcallparanode(tcallparanode(left).right).left);
                   maybe_restore(exprasmlist,tcallparanode(left).left.location,pushedregs);
                   { when constant, just multiply the addvalue }
                   if is_constintnode(tcallparanode(tcallparanode(left).right).left) then
                    addvalue:=addvalue*get_ordinal_value(tcallparanode(tcallparanode(left).right).left)
                   else
                    begin
                      location_force_reg(exprasmlist,tcallparanode(tcallparanode(left).right).left.location,cgsize,false);
                      hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
                      hregisterhi:=tcallparanode(tcallparanode(left).right).left.location.registerhigh;
                      { insert multiply with addvalue if its >1 }
                      if addvalue>1 then
                        cg.a_op_const_reg(exprasmlist,OP_IMUL,addvalue,hregister);
                      addconstant:=false;
                    end;
                 end;
              { write the add instruction }
                if addconstant then
                 begin
                   if cgsize in [OS_64,OS_S64] then
                    cg64.a_op64_const_loc(exprasmlist,addsubop[inlinenumber],
                       addvalue,tcallparanode(left).left.location)
                   else
                    cg.a_op_const_loc(exprasmlist,addsubop[inlinenumber],
                       addvalue,tcallparanode(left).left.location);
                 end
                else
                 begin
                   if cgsize in [OS_64,OS_S64] then
                     cg64.a_op64_reg_loc(exprasmlist,addsubop[inlinenumber],
                       joinreg64(hregister,hregisterhi),tcallparanode(left).left.location)
                   else
                    cg.a_op_reg_loc(exprasmlist,addsubop[inlinenumber],
                       hregister,tcallparanode(left).left.location);
                   location_release(exprasmlist,tcallparanode(tcallparanode(left).right).left.location);
                 end;
                cg.g_overflowcheck(exprasmlist,tcallparanode(left).left);
                cg.g_rangecheck(exprasmlist,tcallparanode(left).left,tcallparanode(left).left.resulttype.def);
              end;

            in_typeinfo_x:
               begin
                  location_reset(location,LOC_REGISTER,OS_ADDR);
                  location.register:=rg.getregisterint(exprasmlist);
                  reference_reset_symbol(href,tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).get_rtti_label(fullrtti),0);
                  emit_ref_reg(A_LEA,S_L,href,location.register);
               end;

            in_assigned_x :
              begin
                 secondpass(tcallparanode(left).left);
                 location_release(exprasmlist,tcallparanode(left).left.location);
                 if (tcallparanode(left).left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   begin
                      emit_reg_reg(A_OR,S_L,
                        tcallparanode(left).left.location.register,
                        tcallparanode(left).left.location.register);
                   end
                 else
                   begin
                      emit_const_ref(A_CMP,S_L,0,tcallparanode(left).left.location.reference);
                   end;
                 location_reset(location,LOC_FLAGS,OS_NO);
                 location.resflags:=F_NE;
              end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 location_copy(location,left.location);
                 secondpass(tcallparanode(left).left);
                 if tcallparanode(tcallparanode(left).right).left.nodetype=ordconstn then
                   begin
                      { calculate bit position }
                      l:=1 shl (tordconstnode(tcallparanode(tcallparanode(left).right).left).value mod 32);

                      { determine operator }
                      if inlinenumber=in_include_x_y then
                        asmop:=A_OR
                      else
                        begin
                           asmop:=A_AND;
                           l:=not(l);
                        end;
                      if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                        begin
                           inc(tcallparanode(left).left.location.reference.offset,
                             (tordconstnode(tcallparanode(tcallparanode(left).right).left).value div 32)*4);
                           emit_const_ref(asmop,S_L,l,tcallparanode(left).left.location.reference);
                           location_release(exprasmlist,tcallparanode(left).left.location);
                        end
                      else
                        { LOC_CREGISTER }
                        begin
                          secondpass(tcallparanode(left).left);
                          emit_const_reg(asmop,S_L,
                            l,tcallparanode(left).left.location.register);
                        end;
                   end
                 else
                   begin
                      { generate code for the element to set }
                      maybe_save(exprasmlist,tcallparanode(tcallparanode(left).right).left.registers32,
                        tcallparanode(left).left.location,pushedregs);
                      secondpass(tcallparanode(tcallparanode(left).right).left);
                      maybe_restore(exprasmlist,tcallparanode(left).left.location,pushedregs);
                      { determine asm operator }
                      if inlinenumber=in_include_x_y then
                        asmop:=A_BTS
                      else
                        asmop:=A_BTR;

                      if tcallparanode(tcallparanode(left).right).left.location.loc in [LOC_CREGISTER,LOC_REGISTER] then
                        { we don't need a mod 32 because this is done automatically  }
                        { by the bts instruction. For proper checking we would       }

                        { note: bts doesn't do any mod'ing, that's why we can also use }
                        { it for normalsets! (JM)                                      }

                        { need a cmp and jmp, but this should be done by the         }
                        { type cast code which does range checking if necessary (FK) }
                        begin
                          hregister := rg.makeregsize(tcallparanode(tcallparanode(left).right).left.location.register,OS_INT);
                        end
                      else
                        begin
                          rg.getexplicitregisterint(exprasmlist,R_EDI);
                          hregister:=R_EDI;
                        end;
                      cg.a_load_loc_reg(exprasmlist,tcallparanode(tcallparanode(left).right).left.location,hregister);
                      if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                        emit_reg_ref(asmop,S_L,hregister,tcallparanode(left).left.location.reference)
                      else
                        emit_reg_reg(asmop,S_L,hregister,tcallparanode(left).left.location.register);
                      if hregister = R_EDI then
                        rg.ungetregisterint(exprasmlist,R_EDI);
                   end;
              end;
            in_pi:
              begin
                location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                emit_none(A_FLDPI,S_NO);
                inc(trgcpu(rg).fpuvaroffset);
                location.register:=R_ST;
              end;
            in_sin_extended,
            in_arctan_extended,
            in_abs_extended,
            in_sqr_extended,
            in_sqrt_extended,
            in_ln_extended,
            in_cos_extended:
              begin
                 location_reset(location,LOC_FPUREGISTER,def_cgsize(resulttype.def));
                 location.register:=R_ST;
                 secondpass(left);
                 case left.location.loc of
                    LOC_FPUREGISTER:
                      ;
                    LOC_CFPUREGISTER:
                      begin
                         cg.a_loadfpu_reg_reg(exprasmlist,
                           left.location.register,location.register);
                      end;
                    LOC_REFERENCE,LOC_CREFERENCE:
                      begin
                         cg.a_loadfpu_ref_reg(exprasmlist,
                           def_cgsize(left.resulttype.def),
                           left.location.reference,location.register);
                         location_release(exprasmlist,left.location);
                      end
                    else
                      internalerror(309991);
                 end;
                 case inlinenumber of
                    in_sin_extended,
                    in_cos_extended:
                      begin
                         if inlinenumber=in_sin_extended then
                           emit_none(A_FSIN,S_NO)
                         else
                           emit_none(A_FCOS,S_NO);
                         {
                         getlabel(l1);
                         emit_reg(A_FNSTSW,S_NO,R_AX);
                         emit_none(A_SAHF,S_NO);
                         emitjmp(C_NP,l1);
                         emit_reg(A_FSTP,S_NO,R_ST0);
                         emit_none(A_FLDZ,S_NO);
                         cg.a_label(exprasmlist,l1);
                         }
                      end;
                    in_arctan_extended:
                      begin
                         emit_none(A_FLD1,S_NO);
                         emit_none(A_FPATAN,S_NO);
                      end;
                    in_abs_extended:
                      emit_none(A_FABS,S_NO);
                    in_sqr_extended:
                      begin
                         (* emit_reg(A_FLD,S_NO,R_ST0);
                         { emit_none(A_FMULP,S_NO); nasm does not accept this PM }
                         emit_reg_reg(A_FMULP,S_NO,R_ST0,R_ST1);
                           can be shorten to *)
                         emit_reg_reg(A_FMUL,S_NO,R_ST0,R_ST0);
                      end;
                    in_sqrt_extended:
                      emit_none(A_FSQRT,S_NO);
                    in_ln_extended:
                      begin
                         emit_none(A_FLDLN2,S_NO);
                         emit_none(A_FXCH,S_NO);
                         emit_none(A_FYL2X,S_NO);
                      end;
                 end;
              end;
{$ifdef SUPPORT_MMX}
            in_mmx_pcmpeqb..in_mmx_pcmpgtw:
              begin
                 location_reset(location,LOC_MMXREGISTER,OS_NO);
                 if left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else if tcallparanode(left).left.location.loc=LOC_REGISTER then
                   begin
                      {!!!!!!!}
                   end
                 else
                   begin
                      {!!!!!!!}
                   end;
              end;
{$endif SUPPORT_MMX}
            else internalerror(9);
         end;
         { reset pushedparasize }
         pushedparasize:=oldpushedparasize;
      end;

begin
   cinlinenode:=ti386inlinenode;
end.
{
  $Log$
  Revision 1.49  2002-07-20 11:58:02  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.48  2002/07/11 14:41:33  florian
    * start of the new generic parameter handling

  Revision 1.47  2002/07/07 09:52:34  florian
    * powerpc target fixed, very simple units can be compiled
    * some basic stuff for better callparanode handling, far from being finished

  Revision 1.46  2002/07/01 18:46:33  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.45  2002/07/01 16:23:56  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.44  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.43  2002/05/16 19:46:51  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.41  2002/05/13 19:54:38  peter
    * removed n386ld and n386util units
    * maybe_save/maybe_restore added instead of the old maybe_push

  Revision 1.40  2002/05/12 16:53:17  peter
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

  Revision 1.39  2002/04/23 19:16:35  peter
    * add pinline unit that inserts compiler supported functions using
      one or more statements
    * moved finalize and setlength from ninl to pinline

  Revision 1.38  2002/04/21 15:35:54  carl
  * changeregsize -> rg.makeregsize

  Revision 1.37  2002/04/19 15:39:35  peter
    * removed some more routines from cga
    * moved location_force_reg/mem to ncgutil
    * moved arrayconstructnode secondpass to ncgld

  Revision 1.36  2002/04/15 19:44:21  peter
    * fixed stackcheck that would be called recursively when a stack
      error was found
    * generic changeregsize(reg,size) for i386 register resizing
    * removed some more routines from cga unit
    * fixed returnvalue handling
    * fixed default stacksize of linux and go32v2, 8kb was a bit small :-)

  Revision 1.35  2002/04/04 19:06:11  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.34  2002/04/02 17:11:36  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.33  2002/03/31 20:26:39  jonas
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

  Revision 1.32  2002/03/04 19:10:14  peter
    * removed compiler warnings

}
