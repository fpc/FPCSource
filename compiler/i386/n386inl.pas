{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

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

{$i defines.inc}

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
      symconst,symtype,symdef,aasm,types,
      cginfo,cgbase,pass_1,pass_2,
      cpubase,
      nbas,ncon,ncal,ncnv,nld,
      cga,tgobj,n386util,ncgutil,cgobj,cg64f32,rgobj,rgcpu;


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
         pushed : tpushedsaved;
         {inc/dec}
         addconstant : boolean;
         addvalue : longint;
         hp : tnode;

      var
         href,href2 : treference;
         hp2 : tstringconstnode;
         dummycoll  : tparaitem;
         l : longint;
         ispushed : boolean;
         hregisterhi,
         hregister : tregister;
         lengthlab,
         otlabel,oflabel{,l1}   : tasmlabel;
         oldpushedparasize : longint;
         def : tdef;
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
                 maketojumpbool(tcallparanode(left).left,lr_load_regvars);
                 emitlab(falselabel);
                 { erroraddr }
                 emit_reg(A_PUSH,S_L,R_EBP);
                 { lineno }
                 emit_const(A_PUSH,S_L,aktfilepos.line);
                 { filename string }
                 hp2:=cstringconstnode.createstr(current_module.sourcefiles.get_file_name(aktfilepos.fileindex),st_shortstring);
                 firstpass(hp2);
                 secondpass(hp2);
                 if codegenerror then
                  exit;
                 emitpushreferenceaddr(hp2.location.reference);
                 hp2.free;
                 { push msg }
                 secondpass(tcallparanode(tcallparanode(left).right).left);
                 emitpushreferenceaddr(tcallparanode(tcallparanode(left).right).left.location.reference);
                 { call }
                 emitcall('FPC_ASSERT');
                 emitlab(truelabel);
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
                      location.register:=rg.getregisterint(exprasmlist);
                      emit_sym_ofs_reg(A_MOV,
                        S_L,newasmsymbol(tobjectdef(left.resulttype.def).vmt_mangledname),0,
                        location.register);
                   end
                 else
                   begin
                      secondpass(left);
                      location_release(exprasmlist,left.location);
                      location.register:=rg.getregisterint(exprasmlist);
                      { load VMT pointer }
                      inc(left.location.reference.offset,
                        tobjectdef(left.resulttype.def).vmt_offset);
                      emit_ref_reg(A_MOV,S_L,left.location.reference,location.register);
                   end;
                 { in sizeof load size }
                 if inlinenumber=in_sizeof_x then
                   begin
                      reference_reset_base(href,location.register,0);
                      emit_ref_reg(A_MOV,S_L,href,location.register);
                   end;
              end;
            in_length_x :
              begin
                 secondpass(left);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(left.resulttype.def) or
                    is_widestring(left.resulttype.def) then
                  begin
                    if left.location.loc<>LOC_REGISTER then
                     begin
                       location_release(exprasmlist,left.location);
                       hregister:=rg.getregisterint(exprasmlist);
                       cg.a_load_loc_reg(exprasmlist,left.location,hregister);
                     end
                    else
                     hregister:=left.location.register;
                    reference_reset_base(href,hregister,-8);
                    getlabel(lengthlab);
                    emit_reg_reg(A_OR,S_L,hregister,hregister);
                    emitjmp(C_Z,lengthlab);
                    emit_ref_reg(A_MOV,S_L,href,hregister);
                    emitlab(lengthlab);
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
                 location_force_reg(location,cgsize,false);

                 if cgsize in [OS_64,OS_S64] then
                  tcg64f32(cg).a_op64_const_reg(exprasmlist,cgop,1,0,
                      location.registerlow,location.registerhigh)
                 else
                  cg.a_op_const_reg(exprasmlist,cgop,1,location.register);

                 emitoverflowcheck(self);
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
                   ispushed:=maybe_push(tcallparanode(tcallparanode(left).right).left.registers32,
                     tcallparanode(left).left,false);
                   secondpass(tcallparanode(tcallparanode(left).right).left);
                   if ispushed then
                     restore(tcallparanode(left).left,false);
                   { when constant, just multiply the addvalue }
                   if is_constintnode(tcallparanode(tcallparanode(left).right).left) then
                    addvalue:=addvalue*get_ordinal_value(tcallparanode(tcallparanode(left).right).left)
                   else
                    begin
                      location_force_reg(tcallparanode(tcallparanode(left).right).left.location,cgsize,false);
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
                    tcg64f32(cg).a_op64_const_loc(exprasmlist,addsubop[inlinenumber],
                       addvalue,0,tcallparanode(left).left.location)
                   else
                    cg.a_op_const_loc(exprasmlist,addsubop[inlinenumber],
                       addvalue,tcallparanode(left).left.location);
                 end
                else
                 begin
                   if cgsize in [OS_64,OS_S64] then
                    tcg64f32(cg).a_op64_reg_loc(exprasmlist,addsubop[inlinenumber],
                       hregister,hregisterhi,tcallparanode(left).left.location)
                   else
                    cg.a_op_reg_loc(exprasmlist,addsubop[inlinenumber],
                       hregister,tcallparanode(left).left.location);
                   location_release(exprasmlist,tcallparanode(tcallparanode(left).right).left.location);
                 end;
                emitoverflowcheck(tcallparanode(left).left);
                cg.g_rangecheck(exprasmlist,tcallparanode(left).left,tcallparanode(left).left.resulttype.def);
              end;

            in_typeinfo_x:
               begin
                  location_reset(location,LOC_REGISTER,OS_ADDR);
                  location.register:=rg.getregisterint(exprasmlist);
                  reference_reset_symbol(href,tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).get_rtti_label(fullrtti),0);
                  emit_ref_reg(A_LEA,S_L,href,location.register);
               end;

             in_finalize_x:
               begin
                  rg.saveusedregisters(exprasmlist,pushed,all_registers);
                  { if a count is passed, push size, typeinfo and count }
                  if assigned(tcallparanode(left).right) then
                    begin
                       secondpass(tcallparanode(tcallparanode(left).right).left);
                       push_int(tcallparanode(left).left.resulttype.def.size);
                       if codegenerror then
                        exit;
                       cg.a_param_loc(exprasmlist,tcallparanode(tcallparanode(left).right).left.location,1);
                    end;

                  { generate a reference }
                  reference_reset_symbol(href,tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).get_rtti_label(initrtti),0);
                  emitpushreferenceaddr(href);

                  { data to finalize }
                  secondpass(tcallparanode(left).left);
                  if codegenerror then
                    exit;
                  emitpushreferenceaddr(tcallparanode(left).left.location.reference);
                  rg.saveregvars(exprasmlist,all_registers);
                  if assigned(tcallparanode(left).right) then
                    emitcall('FPC_FINALIZEARRAY')
                  else
                    emitcall('FPC_FINALIZE');
                  rg.restoreusedregisters(exprasmlist,pushed);
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
            in_setlength_x:
               begin
                  rg.saveusedregisters(exprasmlist,pushed,all_registers);
                  l:=0;
                  { push dimensions }
                  hp:=left;
                  while assigned(tcallparanode(hp).right) do
                    begin
                       inc(l);
                       hp:=tcallparanode(hp).right;
                    end;
                  def:=tcallparanode(hp).left.resulttype.def;
                  hp:=left;
                  if is_dynamic_array(def) then
                    begin
                       { get temp. space }
                       tg.gettempofsizereference(exprasmlist,l*4,href);
                       { keep data start }
                       href2:=href;
                       { copy dimensions }
                       hp:=left;
                       while assigned(tcallparanode(hp).right) do
                         begin
                            secondpass(tcallparanode(hp).left);
                            location_release(exprasmlist,tcallparanode(hp).left.location);
                            cg.a_load_loc_ref(exprasmlist,tcallparanode(hp).left.location,href);
                            inc(href.offset,4);
                            hp:=tcallparanode(hp).right;
                         end;
                    end
                  else
                    begin
                       secondpass(tcallparanode(hp).left);
                       cg.a_param_loc(exprasmlist,tcallparanode(hp).left.location,1);
                       hp:=tcallparanode(hp).right;
                    end;
                  { handle shortstrings separately since the hightree must be }
                  { pushed too (JM)                                           }
                  if not(is_dynamic_array(def)) and
                     (tstringdef(def).string_typ = st_shortstring) then
                    begin
                      dummycoll:=TParaItem.Create;
                      dummycoll.paratyp:=vs_var;
                      dummycoll.paratype:=openshortstringtype;
                      tcallparanode(hp).secondcallparan(dummycoll,false,false,false,0,0);
                      dummycoll.free;
                      if codegenerror then
                        exit;
                    end
                  else secondpass(tcallparanode(hp).left);
                  if is_dynamic_array(def) then
                    begin
                       emitpushreferenceaddr(href2);
                       push_int(l);
                       reference_reset_symbol(href2,tstoreddef(def).get_rtti_label(initrtti),0);
                       emitpushreferenceaddr(href2);
                       emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                       rg.saveregvars(exprasmlist,all_registers);
                       emitcall('FPC_DYNARR_SETLENGTH');
                       tg.ungetiftemp(exprasmlist,href);
                    end
                  else
                    { must be string }
                    begin
                       case tstringdef(def).string_typ of
                          st_widestring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              rg.saveregvars(exprasmlist,all_registers);
                              emitcall('FPC_WIDESTR_SETLENGTH');
                            end;
                          st_ansistring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              rg.saveregvars(exprasmlist,all_registers);
                              emitcall('FPC_ANSISTR_SETLENGTH');
                            end;
                          st_shortstring:
                            begin
                              rg.saveregvars(exprasmlist,all_registers);
                              emitcall('FPC_SHORTSTR_SETLENGTH');
                            end;
                       end;
                    end;
                  rg.restoreusedregisters(exprasmlist,pushed);
                  maybe_loadself;
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
                      ispushed:=maybe_push(tcallparanode(tcallparanode(left).right).left.registers32,
                        tcallparanode(left).left,false);
                      secondpass(tcallparanode(tcallparanode(left).right).left);
                      if ispushed then
                        restore(tcallparanode(left).left,false);
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
                         emitlab(l1);
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
  Revision 1.38  2002-04-21 15:35:54  carl
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

  Revision 1.31  2001/12/30 17:24:46  jonas
    * range checking is now processor independent (part in cgobj,
      part in cg64f32) and should work correctly again (it needed
      some changes after the changes of the low and high of
      tordef's to int64)
    * maketojumpbool() is now processor independent (in ncgutil)
    * getregister32 is now called getregisterint

  Revision 1.30  2001/12/10 14:34:04  jonas
    * fixed type conversions from dynamic arrays to open arrays

  Revision 1.29  2001/12/04 15:59:03  jonas
    * converted lo/hi to processor independent code, generated code is the
      same as before (when turning on the optimizer)

  Revision 1.28  2001/12/02 16:19:17  jonas
    * less unnecessary regvar loading with if-statements

  Revision 1.26  2001/09/28 20:38:51  jonas
    * fixed big bug in my previous changes (the arguent for bts/btr is always
      a 32 bit register, but it wasn't cleared properly if the value was only
      an 8 bit one)

  Revision 1.25  2001/09/27 13:03:18  jonas
    * fixed bug reported by sg about self not being restored after calling
      setlength

  Revision 1.24  2001/09/04 14:32:45  jonas
    * simplified det_resulttype code for include/exclude
    * include/exclude doesn't use any helpers anymore in the i386 secondpass

  Revision 1.23  2001/08/30 20:13:57  peter
    * rtti/init table updates
    * rttisym for reusable global rtti/init info
    * support published for interfaces

  Revision 1.22  2001/08/28 13:24:47  jonas
    + compilerproc implementation of most string-related type conversions
    - removed all code from the compiler which has been replaced by
      compilerproc implementations (using ($ifdef hascompilerproc) is not
      necessary in the compiler)

  Revision 1.21  2001/08/26 13:36:58  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.20  2001/08/24 12:33:54  jonas
    * fixed big bug in handle_str that caused it to (almost) always call
      fpc_<stringtype>_longint
    * fixed small bug in handle_read_write that caused wrong warnigns about
      uninitialized vars with read(ln)
    + handle_val (processor independent val() handling)

  Revision 1.19  2001/08/23 14:28:36  jonas
    + tempcreate/ref/delete nodes (allows the use of temps in the
      resulttype and first pass)
    * made handling of read(ln)/write(ln) processor independent
    * moved processor independent handling for str and reset/rewrite-typed
      from firstpass to resulttype pass
    * changed names of helpers in text.inc to be generic for use as
      compilerprocs + added "iocheck" directive for most of them
    * reading of ordinals is done by procedures instead of functions
      because otherwise FPC_IOCHECK overwrote the result before it could
      be stored elsewhere (range checking still works)
    * compilerprocs can now be used in the system unit before they are
      implemented
    * added note to errore.msg that booleans can't be read using read/readln

  Revision 1.18  2001/08/13 15:39:52  jonas
    * made in_reset_typedfile/in_rewrite_typedfile handling processor
      independent

  Revision 1.17  2001/08/13 12:41:57  jonas
    * made code for str(x,y) completely processor independent

  Revision 1.16  2001/07/10 18:01:08  peter
    * internal length for ansistring and widestrings

  Revision 1.15  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.14  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.13  2001/04/02 21:20:37  peter
    * resulttype rewrite

  Revision 1.12  2001/03/13 11:52:48  jonas
    * fixed some memory leaks

  Revision 1.11  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.10  2000/12/09 22:51:37  florian
    * helper name of val for qword fixed

  Revision 1.9  2000/12/07 17:19:46  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype.def wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.8  2000/12/05 11:44:33  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.7  2000/11/29 00:30:47  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.6  2000/11/12 23:24:15  florian
    * interfaces are basically running

  Revision 1.5  2000/11/09 17:46:56  florian
    * System.TypeInfo fixed
    + System.Finalize implemented
    + some new keywords for interface support added

  Revision 1.4  2000/10/31 22:02:56  peter
    * symtable splitted, no real code changes

  Revision 1.3  2000/10/26 14:15:07  jonas
    * fixed setlength for shortstrings

  Revision 1.2  2000/10/21 18:16:13  florian
    * a lot of changes:
       - basic dyn. array support
       - basic C++ support
       - some work for interfaces done
       ....

  Revision 1.1  2000/10/15 09:33:31  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.2  2000/10/15 09:08:58  peter
    * use System for the systemunit instead of target dependent

  Revision 1.1  2000/10/14 10:14:49  peter
    * moehrendorf oct 2000 rewrite

}
