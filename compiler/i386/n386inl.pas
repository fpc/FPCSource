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
      cgbase,temp_gen,pass_1,pass_2,
      cpubase,
      nbas,ncon,ncal,ncnv,nld,
      cga,tgcpu,n386util;


{*****************************************************************************
                              TI386INLINENODE
*****************************************************************************}


    procedure ti386inlinenode.pass_2;
       const
         {tfloattype = (s32real,s64real,s80real,s64bit,f16bit,f32bit);}
{        float_name: array[tfloattype] of string[8]=
           ('S32REAL','S64REAL','S80REAL','S64BIT','F16BIT','F32BIT'); }
         incdecop:array[in_inc_x..in_dec_x] of tasmop=(A_INC,A_DEC);
         addsubop:array[in_inc_x..in_dec_x] of tasmop=(A_ADD,A_SUB);
       var
         opsize : topsize;
         op,
         asmop : tasmop;
         pushed : tpushed;
         {inc/dec}
         addconstant : boolean;
         addvalue : longint;
         hp : tnode;

      var
         r : preference;
         //hp : tcallparanode;
         hp2 : tstringconstnode;
         dummycoll  : tparaitem;
         l : longint;
         ispushed : boolean;
         hregister : tregister;
         lengthlab,
         otlabel,oflabel{,l1}   : tasmlabel;
         oldpushedparasize : longint;
         def : tdef;
         hr,hr2 : treference;

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
                 { for both cases load vmt }
                 if left.nodetype=typen then
                   begin
                      location.register:=getregister32;
                      emit_sym_ofs_reg(A_MOV,
                        S_L,newasmsymbol(tobjectdef(left.resulttype.def).vmt_mangledname),0,
                        location.register);
                   end
                 else
                   begin
                      secondpass(left);
                      del_reference(left.location.reference);
                      location.loc:=LOC_REGISTER;
                      location.register:=getregister32;
                      { load VMT pointer }
                      inc(left.location.reference.offset,
                        tobjectdef(left.resulttype.def).vmt_offset);
                      emit_ref_reg(A_MOV,S_L,
                      newreference(left.location.reference),
                        location.register);
                   end;
                 { in sizeof load size }
                 if inlinenumber=in_sizeof_x then
                   begin
                      new(r);
                      reset_reference(r^);
                      r^.base:=location.register;
                      emit_ref_reg(A_MOV,S_L,r,
                        location.register);
                   end;
              end;
            in_length_x :
              begin
                 secondpass(left);
                 set_location(location,left.location);
                 { length in ansi strings is at offset -8 }
                 if is_ansistring(left.resulttype.def) or
                    is_widestring(left.resulttype.def) then
                  begin
                    if left.location.loc<>LOC_REGISTER then
                     begin
                       del_location(left.location);
                       hregister:=getregister32;
                       emit_mov_loc_reg(left.location,hregister);
                     end
                    else
                     hregister:=left.location.register;
                    reset_reference(hr);
                    hr.base:=hregister;
                    hr.offset:=-8;
                    getlabel(lengthlab);
                    emit_reg_reg(A_OR,S_L,hregister,hregister);
                    emitjmp(C_Z,lengthlab);
                    emit_ref_reg(A_MOV,S_L,newreference(hr),hregister);
                    emitlab(lengthlab);
                    location.loc:=LOC_REGISTER;
                    location.register:=hregister;
                  end;
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(left);
                 if not (cs_check_overflow in aktlocalswitches) then
                   if inlinenumber=in_pred_x then
                     asmop:=A_DEC
                   else
                     asmop:=A_INC
                 else
                   if inlinenumber=in_pred_x then
                     asmop:=A_SUB
                   else
                     asmop:=A_ADD;
                 case resulttype.def.size of
                   8 : opsize:=S_L;
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 location.loc:=LOC_REGISTER;
                 if resulttype.def.size=8 then
                   begin
                      if left.location.loc<>LOC_REGISTER then
                        begin
                           if left.location.loc=LOC_CREGISTER then
                             begin
                                location.registerlow:=getregister32;
                                location.registerhigh:=getregister32;
                                emit_reg_reg(A_MOV,opsize,left.location.registerlow,
                                  location.registerlow);
                                emit_reg_reg(A_MOV,opsize,left.location.registerhigh,
                                  location.registerhigh);
                             end
                           else
                             begin
                                del_reference(left.location.reference);
                                location.registerlow:=getregister32;
                                location.registerhigh:=getregister32;
                                emit_ref_reg(A_MOV,opsize,newreference(left.location.reference),
                                  location.registerlow);
                                r:=newreference(left.location.reference);
                                inc(r^.offset,4);
                                emit_ref_reg(A_MOV,opsize,r,
                                  location.registerhigh);
                             end;
                        end
                      else
                        begin
                           location.registerhigh:=left.location.registerhigh;
                           location.registerlow:=left.location.registerlow;
                        end;
                      if inlinenumber=in_succ_x then
                        begin
                           emit_const_reg(A_ADD,opsize,1,
                             location.registerlow);
                           emit_const_reg(A_ADC,opsize,0,
                             location.registerhigh);
                        end
                      else
                        begin
                           emit_const_reg(A_SUB,opsize,1,
                             location.registerlow);
                           emit_const_reg(A_SBB,opsize,0,
                             location.registerhigh);
                        end;
                   end
                 else
                   begin
                      if left.location.loc<>LOC_REGISTER then
                        begin
                           { first, we've to release the source location ... }
                           if left.location.loc in [LOC_MEM,LOC_REFERENCE] then
                             del_reference(left.location.reference);

                           location.register:=getregister32;
                           if (resulttype.def.size=2) then
                             location.register:=reg32toreg16(location.register);
                           if (resulttype.def.size=1) then
                             location.register:=reg32toreg8(location.register);
                           if left.location.loc=LOC_CREGISTER then
                             emit_reg_reg(A_MOV,opsize,left.location.register,
                               location.register)
                           else
                           if left.location.loc=LOC_FLAGS then
                             emit_flag2reg(left.location.resflags,location.register)
                           else
                             emit_ref_reg(A_MOV,opsize,newreference(left.location.reference),
                               location.register);
                        end
                      else location.register:=left.location.register;
                      if not (cs_check_overflow in aktlocalswitches) then
                        emit_reg(asmop,opsize,
                        location.register)
                      else
                        emit_const_reg(asmop,opsize,1,
                        location.register);
                   end;
                 emitoverflowcheck(self);
                 emitrangecheck(self,resulttype.def);
              end;
            in_dec_x,
            in_inc_x :
              begin
              { set defaults }
                addvalue:=1;
                addconstant:=true;
              { load first parameter, must be a reference }
                secondpass(tcallparanode(left).left);
                case tcallparanode(left).left.resulttype.def.deftype of
                  orddef,
                 enumdef : begin
                             case tcallparanode(left).left.resulttype.def.size of
                              1 : opsize:=S_B;
                              2 : opsize:=S_W;
                              4 : opsize:=S_L;
                              8 : opsize:=S_L;
                             end;
                           end;
              pointerdef : begin
                             opsize:=S_L;
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
                      case tcallparanode(tcallparanode(left).right).left.location.loc of
                   LOC_REGISTER,
                  LOC_CREGISTER : hregister:=tcallparanode(tcallparanode(left).right).left.location.register;
                        LOC_MEM,
                  LOC_REFERENCE : begin
                                    del_reference(tcallparanode(tcallparanode(left).right).left.location.reference);
                                    hregister:=getregister32;
                                    emit_ref_reg(A_MOV,S_L,
                                      newreference(tcallparanode(tcallparanode(left).right).left.location.reference),hregister);
                                  end;
                       else
                        internalerror(10082);
                       end;
                    { insert multiply with addvalue if its >1 }
                      if addvalue>1 then
                       emit_const_reg(A_IMUL,opsize,
                         addvalue,hregister);
                      addconstant:=false;
                    end;
                 end;
              { write the add instruction }
                if addconstant then
                 begin
                   if (addvalue=1) and not(cs_check_overflow in aktlocalswitches) then
                     begin
                        if tcallparanode(left).left.location.loc=LOC_CREGISTER then
                          emit_reg(incdecop[inlinenumber],opsize,
                            tcallparanode(left).left.location.register)
                        else
                          emit_ref(incdecop[inlinenumber],opsize,
                            newreference(tcallparanode(left).left.location.reference))
                     end
                   else
                     begin
                        if tcallparanode(left).left.location.loc=LOC_CREGISTER then
                          emit_const_reg(addsubop[inlinenumber],opsize,
                            addvalue,tcallparanode(left).left.location.register)
                        else
                          emit_const_ref(addsubop[inlinenumber],opsize,
                            addvalue,newreference(tcallparanode(left).left.location.reference));
                     end
                 end
                else
                 begin
                    { BUG HERE : detected with nasm :
                      hregister is allways 32 bit
                      it should be converted to 16 or 8 bit depending on op_size  PM }
                    { still not perfect :
                      if hregister is already a 16 bit reg ?? PM }
                    { makeregXX is the solution (FK) }
                    case opsize of
                      S_B : hregister:=makereg8(hregister);
                      S_W : hregister:=makereg16(hregister);
                    end;
                    if tcallparanode(left).left.location.loc=LOC_CREGISTER then
                      emit_reg_reg(addsubop[inlinenumber],opsize,
                        hregister,tcallparanode(left).left.location.register)
                    else
                      emit_reg_ref(addsubop[inlinenumber],opsize,
                        hregister,newreference(tcallparanode(left).left.location.reference));
                    case opsize of
                      S_B : hregister:=reg8toreg32(hregister);
                      S_W : hregister:=reg16toreg32(hregister);
                    end;
                   ungetregister32(hregister);
                 end;
                emitoverflowcheck(tcallparanode(left).left);
                emitrangecheck(tcallparanode(left).left,tcallparanode(left).left.resulttype.def);
              end;

            in_typeinfo_x:
               begin
                  location.register:=getregister32;
                  new(r);
                  reset_reference(r^);
                  r^.symbol:=tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).get_rtti_label(fullrtti);
                  emit_ref_reg(A_LEA,S_L,r,location.register);
               end;

             in_finalize_x:
               begin
                  pushusedregisters(pushed,$ff);
                  { if a count is passed, push size, typeinfo and count }
                  if assigned(tcallparanode(left).right) then
                    begin
                       secondpass(tcallparanode(tcallparanode(left).right).left);
                       push_int(tcallparanode(left).left.resulttype.def.size);
                       if codegenerror then
                        exit;
                       emit_push_loc(tcallparanode(tcallparanode(left).right).left.location);
                    end;

                  { generate a reference }
                  reset_reference(hr);
                  hr.symbol:=tstoreddef(ttypenode(tcallparanode(left).left).resulttype.def).get_rtti_label(initrtti);
                  emitpushreferenceaddr(hr);

                  { data to finalize }
                  secondpass(tcallparanode(left).left);
                  if codegenerror then
                    exit;
                  emitpushreferenceaddr(tcallparanode(left).left.location.reference);
                  saveregvars($ff);
                  if assigned(tcallparanode(left).right) then
                    emitcall('FPC_FINALIZEARRAY')
                  else
                    emitcall('FPC_FINALIZE');
                  popusedregisters(pushed);
               end;

            in_assigned_x :
              begin
                 secondpass(tcallparanode(left).left);
                 location.loc:=LOC_FLAGS;
                 if (tcallparanode(left).left.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                   begin
                      emit_reg_reg(A_OR,S_L,
                        tcallparanode(left).left.location.register,
                        tcallparanode(left).left.location.register);
                      ungetregister32(tcallparanode(left).left.location.register);
                   end
                 else
                   begin
                      emit_const_ref(A_CMP,S_L,0,
                        newreference(tcallparanode(left).left.location.reference));
                      del_reference(tcallparanode(left).left.location.reference);
                   end;
                 location.resflags:=F_NE;
              end;
            in_setlength_x:
               begin
                  pushusedregisters(pushed,$ff);
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
                       gettempofsizereference(l*4,hr);
                       { keep data start }
                       hr2:=hr;
                       { copy dimensions }
                       hp:=left;
                       while assigned(tcallparanode(hp).right) do
                         begin
                            secondpass(tcallparanode(hp).left);
                            emit_mov_loc_ref(tcallparanode(hp).left.location,hr,
                              S_L,true);
                            inc(hr.offset,4);
                            hp:=tcallparanode(hp).right;
                         end;
                    end
                  else
                    begin
                       secondpass(tcallparanode(hp).left);
                       emit_push_loc(tcallparanode(hp).left.location);
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
                       emitpushreferenceaddr(hr2);
                       push_int(l);
                       reset_reference(hr2);
                       hr2.symbol:=tstoreddef(def).get_rtti_label(initrtti);
                       emitpushreferenceaddr(hr2);
                       emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                       saveregvars($ff);
                       emitcall('FPC_DYNARR_SETLENGTH');
                       ungetiftemp(hr);
                    end
                  else
                    { must be string }
                    begin
                       case tstringdef(def).string_typ of
                          st_widestring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              saveregvars($ff);
                              emitcall('FPC_WIDESTR_SETLENGTH');
                            end;
                          st_ansistring:
                            begin
                              emitpushreferenceaddr(tcallparanode(hp).left.location.reference);
                              saveregvars($ff);
                              emitcall('FPC_ANSISTR_SETLENGTH');
                            end;
                          st_shortstring:
                            begin
                              saveregvars($ff);
                              emitcall('FPC_SHORTSTR_SETLENGTH');
                            end;
                       end;
                    end;
                  popusedregisters(pushed);
                  maybe_loadself;
               end;
            in_include_x_y,
            in_exclude_x_y:
              begin
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
                           emit_const_ref(asmop,S_L,
                             l,newreference(tcallparanode(left).left.location.reference));
                           del_reference(tcallparanode(left).left.location.reference);
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
                          hregister := tcallparanode(tcallparanode(left).right).left.location.register;
                          emit_to_reg32(hregister);
                        end
                      else
                        begin
                           getexplicitregister32(R_EDI);
                           hregister:=R_EDI;
                           opsize:=def2def_opsize(
                             tcallparanode(tcallparanode(left).right).left.resulttype.def,u32bittype.def);
                           if opsize = S_L then
                            op:=A_MOV
                           else
                            op:=A_MOVZX;
                           emit_ref_reg(op,opsize,
                             newreference(
                               tcallparanode(tcallparanode(left).right).left.location.reference),R_EDI);
                        end;
                      if (tcallparanode(left).left.location.loc=LOC_REFERENCE) then
                        emit_reg_ref(asmop,S_L,hregister,
                          newreference(tcallparanode(left).left.location.reference))
                      else
                        emit_reg_reg(asmop,S_L,hregister,
                          tcallparanode(left).left.location.register);
                      if hregister = R_EDI then
                        ungetregister32(R_EDI);
                   end;
              end;
            in_pi:
              begin
                emit_none(A_FLDPI,S_NO);
                inc(fpuvaroffset);
              end;
            in_sin_extended,
            in_arctan_extended,
            in_abs_extended,
            in_sqr_extended,
            in_sqrt_extended,
            in_ln_extended,
            in_cos_extended:
              begin
                 secondpass(left);
                 case left.location.loc of
                    LOC_FPU:
                      ;
                    LOC_CFPUREGISTER:
                      begin
                         emit_reg(A_FLD,S_NO,
                           correct_fpuregister(left.location.register,fpuvaroffset));
                         inc(fpuvaroffset);
                      end;
                    LOC_REFERENCE,LOC_MEM:
                      begin
                         floatload(tfloatdef(left.resulttype.def).typ,left.location.reference);
                         del_reference(left.location.reference);
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
  Revision 1.30  2001-12-10 14:34:04  jonas
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
