{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate m68k assembler for in call nodes

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
unit cg68kcal;
interface

    uses
      symtable,tree;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right : boolean);
    procedure secondcalln(var p : ptree);
    procedure secondprocinline(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,systems,
      aasm,types,
      hcodegen,temp_gen,pass_2,
      m68k,cga68k,tgen68k,cg68kld;

{*****************************************************************************
                             SecondCallParaN
*****************************************************************************}

    { save the size of pushed parameter }
    var
       pushedparasize : longint;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right : boolean);

      procedure maybe_push_open_array_high;
        var
           r : preference;
        begin
           { open array ? }
           { defcoll^.data can be nil for read/write }
           if assigned(defcoll^.data) and
              is_open_array(defcoll^.data) then
             begin
                inc(pushedparasize,4);
                { push high }
                if is_open_array(p^.left^.resulttype) then
                 begin
                   new(r);
                   reset_reference(r^);
                   r^.base:=highframepointer;
                   r^.offset:=highoffset+4;
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_SPPUSH)));
                 end
                else
                 push_int(parraydef(p^.left^.resulttype)^.highrange-parraydef(p^.left^.resulttype)^.lowrange);
             end;
        end;

      var
         size : longint;
         stackref : treference;
         otlabel,hlabel,oflabel : plabel;
         { temporary variables: }
         reg : tregister;
         tempdeftype : tdeftype;
         tempreference : treference;
         r : preference;
         s : topsize;
         op : tasmop;

      begin
         { push from left to right if specified }
         if push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right);
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         secondpass(p^.left);
         { in codegen.handleread.. defcoll^.data is set to nil }
         if assigned(defcoll^.data) and
           (defcoll^.data^.deftype=formaldef) then
           begin
              { allow @var }
              if p^.left^.treetype=addrn then
                begin
                   { allways a register }
                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,p^.left^.location.register,R_SPPUSH)));
                   ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   if (p^.left^.location.loc<>LOC_REFERENCE) and
                      (p^.left^.location.loc<>LOC_MEM) then
                     CGMessage(type_e_mismatch)
                   else
                     begin
                        emitpushreferenceaddr(p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
              inc(pushedparasize,4);
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) then
           begin
              if (p^.left^.location.loc<>LOC_REFERENCE) then
                CGMessage(cg_e_var_must_be_reference);
              maybe_push_open_array_high;
              inc(pushedparasize,4);
              emitpushreferenceaddr(p^.left^.location.reference);
              del_reference(p^.left^.location.reference);
           end
         else
           begin
              tempdeftype:=p^.resulttype^.deftype;
              if tempdeftype=filedef then
               CGMessage(cg_e_file_must_call_by_reference);
              if (defcoll^.paratyp=vs_const) and
                 dont_copy_const_param(p^.resulttype) then
                begin
                   maybe_push_open_array_high;
                   inc(pushedparasize,4);
                   emitpushreferenceaddr(p^.left^.location.reference);
                   del_reference(p^.left^.location.reference);
                end
              else
                case p^.left^.location.loc of
                   LOC_REGISTER,
                   LOC_CREGISTER : begin
                                   { HERE IS A BIG PROBLEM }
                                   { --> We *MUST* know the data size to push     }
                                   { for the moment, we can say that the savesize }
                                   { indicates the parameter size to push, but    }
                                   { that is CERTAINLY NOT TRUE!                  }
                                   { CAN WE USE LIKE LOC_MEM OR LOC_REFERENCE??   }
                                     case integer(p^.left^.resulttype^.savesize) of
                                     1 : Begin
                                     { A byte sized value normally increments       }
                                     { the SP by 2, BUT because how memory has      }
                                     { been setup OR because of GAS, a byte sized   }
                                     { push CRASHES the Amiga, therefore, we do it  }
                                     { by hand instead.                             }
                                     {  PUSH A WORD SHIFTED LEFT 8                  }
                                           reg := getregister32;
                                           emit_reg_reg(A_MOVE, S_B, p^.left^.location.register, reg);
                                           exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_W,
                                             8, reg)));
                                           exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,
                                            reg,R_SPPUSH)));
                                           { offset will be TWO greater              }
                                           inc(pushedparasize,2);
                                           ungetregister32(reg);
                                           ungetregister32(p^.left^.location.register);
                                         end;
                                     2 :
                                              Begin
                                                 exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,
                                                   p^.left^.location.register,R_SPPUSH)));
                                                 inc(pushedparasize,2);
                                                 ungetregister32(p^.left^.location.register);
                                              end;
                                      4 : Begin
                                             exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                                                 p^.left^.location.register,R_SPPUSH)));
                                             inc(pushedparasize,4);
                                             ungetregister32(p^.left^.location.register);
                                          end;
                                      else
                                       Begin
                                         exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,
                                           p^.left^.location.register,R_SPPUSH)));
                                         inc(pushedparasize,4);
                                         ungetregister32(p^.left^.location.register);
                                       end;
                                     end; { end case }
                                   end;
                   LOC_FPU : begin
                                        size:=pfloatdef(p^.left^.resulttype)^.size;
                                        inc(pushedparasize,size);
                                        exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_L,size,R_SP)));
                                        new(r);
                                        reset_reference(r^);
                                        r^.base:=R_SP;
                                        s:=getfloatsize(pfloatdef(p^.left^.resulttype)^.typ);
                                        if (cs_fp_emulation in aktmoduleswitches) or (s=S_FS) then
                                        begin
                                          { when in emulation mode... }
                                          { only single supported!!!  }
                                          exprasmlist^.concat(new(pai68k,op_reg_ref(A_MOVE,S_L,
                                             p^.left^.location.fpureg,r)));
                                        end
                                        else
                                          { convert back from extended to normal type }
                                          exprasmlist^.concat(new(pai68k,op_reg_ref(A_FMOVE,s,
                                             p^.left^.location.fpureg,r)));
                                     end;
                   LOC_REFERENCE,LOC_MEM :
                               begin
                                  tempreference:=p^.left^.location.reference;
                                  del_reference(p^.left^.location.reference);
                                  case p^.resulttype^.deftype of
                                    enumdef,
                                     orddef : begin
                                                   case p^.resulttype^.size of
                                                    4 : begin
                                                           emit_push_mem(tempreference);
                                                           inc(pushedparasize,4);
                                                        end;
                                                    1 : Begin
                                                          { We push a BUT, the SP is incremented by 2      }
                                                          { as specified in the Motorola Prog's Ref Manual }
                                                          { Therefore offet increments BY 2!!!             }
                                                          { BUG??? ...                                     }
                                                          { SWAP OPERANDS:                                 }
                                                          if tempreference.isintvalue then
                                                          Begin
                                                            exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,
                                                             tempreference.offset shl 8,R_SPPUSH)));
                                                          end
                                                          else
                                                          Begin
                                                           { A byte sized value normally increments       }
                                                           { the SP by 2, BUT because how memory has      }
                                                           { been setup OR because of GAS, a byte sized   }
                                                           { push CRASHES the Amiga, therefore, we do it  }
                                                           { by hand instead.                             }
                                                           {  PUSH A WORD SHIFTED LEFT 8                  }
                                                            reg:=getregister32;
                                                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_B,
                                                             newreference(tempreference),reg)));
                                                            exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_W,
                                                             8, reg)));
                                                            exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,
                                                             reg,R_SPPUSH)));
                                                            ungetregister32(reg);
{                                                           exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                                             newreference(tempreference),R_SPPUSH))); }
                                                          end;
                                                          inc(pushedparasize,2);
                                                        end;
                                                    2 : begin
                                                          exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                                            newreference(tempreference),R_SPPUSH)));
                                                          inc(pushedparasize,2);
                                                        end;
                                                   end;
                                              end;
                                     floatdef : begin
                                                   case pfloatdef(p^.resulttype)^.typ of
                                                      f32bit,
                                                      s32real :
                                                        begin
                                                           emit_push_mem(tempreference);
                                                           inc(pushedparasize,4);
                                                        end;
                                                      s64real:
                                                      {s64bit }
                                                                begin
                                                                   inc(tempreference.offset,4);
                                                                   emit_push_mem(tempreference);
                                                                   dec(tempreference.offset,4);
                                                                   emit_push_mem(tempreference);
                                                                   inc(pushedparasize,8);
                                                                end;
{$ifdef use48}
                                                      s48real : begin
                                                                end;
{$endif}
                                                      s80real : begin
                                                                    CGMessage(cg_f_extended_cg68k_not_supported);
{                                                                   inc(tempreference.offset,6);
                                                                   emit_push_mem(tempreference);
                                                                   dec(tempreference.offset,4);
                                                                   emit_push_mem(tempreference);
                                                                   dec(tempreference.offset,2);
                                                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_W,
                                                                     newreference(tempreference),R_SPPUSH)));
                                                                   inc(pushedparasize,extended_size);}
                                                                end;
                                                   end;
                                                end;
                                     pointerdef,procvardef,
                                         classrefdef:  begin
                                                      emit_push_mem(tempreference);
                                                      inc(pushedparasize,4);
                                                   end;
                                     arraydef,recorddef,stringdef,setdef,objectdef :
                                                begin
                                                   if ((p^.resulttype^.deftype=setdef) and
                                                     (psetdef(p^.resulttype)^.settype=smallset)) then
                                                     begin
                                                        emit_push_mem(tempreference);
                                                        inc(pushedparasize,4);
                                                     end
                                                   else
                                                     begin
                                                        size:=p^.resulttype^.size;

                                                        { Alignment }
                                                        {
                                                        if (size>=4) and ((size and 3)<>0) then
                                                          inc(size,4-(size and 3))
                                                        else if (size>=2) and ((size and 1)<>0) then
                                                          inc(size,2-(size and 1))
                                                        else
                                                        if size=1 then size:=2;
                                                        }
                                                        { create stack space }
                                                        if (size > 0) and (size < 9) then
                                                            exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBQ,S_L,size,R_SP)))
                                                        else
                                                            exprasmlist^.concat(new(pai68k,op_const_reg(A_SUBA,
                                                              S_L,size,R_SP)));
                                                        inc(pushedparasize,size);
                                                        { create stack reference }
                                                        stackref.symbol := nil;
                                                        clear_reference(stackref);
                                                        stackref.base:=R_SP;
                                                        { produce copy }
                                                        if p^.resulttype^.deftype=stringdef then
                                                          begin
                                                             copystring(stackref,p^.left^.location.reference,
                                                               pstringdef(p^.resulttype)^.len);
                                                          end
                                                        else
                                                          begin
                                                             concatcopy(p^.left^.location.reference,
                                                             stackref,p^.resulttype^.size,true);
                                                          end;
                                                     end;
                                                end;
                                     else CGMessage(cg_e_illegal_expression);
                                  end;
                               end;
                 LOC_JUMP     : begin
                                   getlabel(hlabel);
                                   inc(pushedparasize,2);
                                   emitl(A_LABEL,truelabel);
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,1 shl 8,R_SPPUSH)));
                                   emitl(A_JMP,hlabel);
                                   emitl(A_LABEL,falselabel);
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_W,0,R_SPPUSH)));
                                   emitl(A_LABEL,hlabel);
                                end;
                 LOC_FLAGS    : begin
                                   exprasmlist^.concat(new(pai68k,op_reg(flag_2_set[p^.left^.location.resflags],S_B,
                                     R_D0)));
                                   exprasmlist^.concat(new(pai68k,op_reg(A_NEG, S_B, R_D0)));
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_AND,S_W,$ff, R_D0)));
                                   inc(pushedparasize,2);
                                   { ----------------- HACK ----------------------- }
                                   { HERE IS THE BYTE SIZED PUSH HACK ONCE AGAIN    }
                                   { SHIFT LEFT THE BYTE TO MAKE IT WORK!           }
                                   exprasmlist^.concat(new(pai68k,op_const_reg(A_LSL,S_W,8, R_D0)));
                                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_W,R_D0,R_SPPUSH)));
                                end;
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right);
      end;


{*****************************************************************************
                             SecondCallN
*****************************************************************************}

    procedure secondcalln(var p : ptree);

      var
         unusedregisters : tregisterset;
         pushed : tpushed;
         funcretref : treference;
         hregister : tregister;
         oldpushedparasize : longint;
         { true if a5 must be loaded again after the subroutine }
         loada5 : boolean;
         { true if a virtual method must be called directly }
         no_virtual_call : boolean;
         { true if we produce a con- or destrutor in a call }
         is_con_or_destructor : boolean;
         { true if a constructor is called again }
         extended_new : boolean;
         { adress returned from an I/O-error }
         iolabel : plabel;
         { lexlevel count }
         i : longint;
         { help reference pointer }
         r : preference;
         pp,params : ptree;
         { temp register allocation }
         reg: tregister;
         { help reference pointer }
         ref: preference;

      label
         dont_call;

      begin
         extended_new:=false;
         iolabel:=nil;
         loada5:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;
         if not assigned(p^.procdefinition) then
           exit;
         { only if no proc var }
         if not(assigned(p^.right)) then
           is_con_or_destructor:=((p^.procdefinition^.options and poconstructor)<>0)
             or ((p^.procdefinition^.options and podestructor)<>0);
         { proc variables destroy all registers }
         if (p^.right=nil) and
         { virtual methods too }
           ((p^.procdefinition^.options and povirtualmethod)=0) then
           begin
              if ((p^.procdefinition^.options and poiocheck)<>0)
                and (cs_check_io in aktlocalswitches) then
                begin
                       getlabel(iolabel);
                   emitl(A_LABEL,iolabel);
                end
              else iolabel:=nil;

              { save all used registers }
              pushusedregisters(pushed,p^.procdefinition^.usedregisters);

              { give used registers through }
              usedinproc:=usedinproc or p^.procdefinition^.usedregisters;
           end
         else
           begin
              pushusedregisters(pushed,$ffff);
              usedinproc:=$ffff;

              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         if (p^.resulttype<>pdef(voiddef)) and
            ret_in_param(p^.resulttype) then
           begin
              funcretref.symbol:=nil;
{$ifdef test_dest_loc}
              if dest_loc_known and (dest_loc_tree=p) and
                 (dest_loc.loc in [LOC_REFERENCE,LOC_MEM]) then
                begin
                   funcretref:=dest_loc.reference;
                   if assigned(dest_loc.reference.symbol) then
                     funcretref.symbol:=stringdup(dest_loc.reference.symbol^);
                   in_dest_loc:=true;
                end
              else
{$endif test_dest_loc}
              gettempofsizereference(p^.procdefinition^.retdef^.size,funcretref);
           end;
         if assigned(p^.left) then
           begin
              pushedparasize:=0;
              { be found elsewhere }
              if assigned(p^.right) then
                secondcallparan(p^.left,pprocvardef(p^.right^.resulttype)^.para1,
                  (p^.procdefinition^.options and poleftright)<>0)
              else
                secondcallparan(p^.left,p^.procdefinition^.para1,
                  (p^.procdefinition^.options and poleftright)<>0);
           end;
         params:=p^.left;
         p^.left:=nil;
         if ret_in_param(p^.resulttype) then
           begin
              emitpushreferenceaddr(funcretref);
              inc(pushedparasize,4);
           end;
         { overloaded operator have no symtable }
         if (p^.right=nil) then
           begin
              { push self }
              if assigned(p^.symtable) and
                (p^.symtable^.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   p^.methodpointer:=genzeronode(callparan);
                   p^.methodpointer^.location.loc:=LOC_REGISTER;
                   p^.methodpointer^.location.register:=R_A5;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   r^.offset:=p^.symtable^.datasize;
                   r^.base:=procinfo.framepointer;
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_A5)));
                end;

              { push self }
              if assigned(p^.symtable) and
                ((p^.symtable^.symtabletype=objectsymtable) or
                (p^.symtable^.symtabletype=withsymtable)) then
                begin
                   if assigned(p^.methodpointer) then
                     begin
                        case p^.methodpointer^.treetype of
                           typen : begin
                                      { direct call to inherited method }
                                      if (p^.procdefinition^.options and poabstractmethod)<>0 then
                                        begin
                                           CGMessage(cg_e_cant_call_abstract_method);
                                           goto dont_call;
                                        end;
                                      { generate no virtual call }
                                      no_virtual_call:=true;
                             if (p^.symtableprocentry^.properties and sp_static)<>0 then
                                 begin
                                    { well lets put the VMT address directly into a5 }
                                    { it is kind of dirty but that is the simplest    }
                                    { way to accept virtual static functions (PM)     }
                                    loada5:=true;
                                    exprasmlist^.concat(new(pai68k,op_csymbol_reg(A_MOVE,S_L,
                                      newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_A5)));
                                    concat_external(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                    exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                 end
                               else

                                  { this is a member call, so A5 isn't modfied }
                                  loada5:=false;

                               if not(is_con_or_destructor and
                                  pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                  assigned(aktprocsym) and
                                  ((aktprocsym^.definition^.options and
                                  (poconstructor or podestructor))<>0)) then
                                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                 { if an inherited con- or destructor should be  }
                                 { called in a con- or destructor then a warning }
                                 { will be made                                  }
                                 { con- and destructors need a pointer to the vmt }
                                 if is_con_or_destructor and
                                   ((pobjectdef(p^.methodpointer^.resulttype)^.options and oois_class)=0) and
                                   assigned(aktprocsym) then
                                   begin
                                    if not ((aktprocsym^.definition^.options
                                      and (poconstructor or podestructor))<>0) then
                                        CGMessage(cg_w_member_cd_call_from_method);
                                   end;
                                      { con- and destructors need a pointer to the vmt }
                                      if is_con_or_destructor then
                                        begin
                                           { classes need the mem ! }
                                           if ((pobjectdef(p^.methodpointer^.resulttype)^.options and

                                            oois_class)=0) then
                                             push_int(0)
                                           else
                                               begin
                                                  exprasmlist^.concat(new(pai68k,op_csymbol(A_PEA,
                                                   S_L,newcsymbol(pobjectdef(p^.methodpointer^.
                                                   resulttype)^.vmt_mangledname,0))));
                                                   concat_external(pobjectdef(p^.methodpointer^.resulttype)^.
                                                  vmt_mangledname,EXT_NEAR);
                                               end;
                                        end;
                                   end;
                           hnewn : begin
                                     { extended syntax of new }
                                     { A5 must be zero }
                                     exprasmlist^.concat(new(pai68k,op_const_reg(A_MOVE,S_L,0,R_A5)));
                                     emit_reg_reg(A_MOVE,S_L,R_A5, R_SPPUSH);
                                     { insert the vmt }
                                     exprasmlist^.concat(new(pai68k,op_csymbol(A_PEA,S_L,
                                       newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                     concat_external(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                              extended_new:=true;
                                  end;
                           hdisposen : begin
                                          secondpass(p^.methodpointer);

                                          { destructor with extended syntax called from dispose }
                                          { hdisposen always deliver LOC_REFRENZ }
                                          exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                                            newreference(p^.methodpointer^.location.reference),R_A5)));
                                          del_reference(p^.methodpointer^.location.reference);
                                          exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                          exprasmlist^.concat(new(pai68k,op_csymbol(A_PEA,S_L,
                                            newcsymbol(pobjectdef
                                               (p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                          concat_external(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                       end;
                           else
                             begin
                                { call to a instance member }
                                if (p^.symtable^.symtabletype<>withsymtable) then
                                  begin
                                     secondpass(p^.methodpointer);


                                     case p^.methodpointer^.location.loc of
                                        LOC_REGISTER :
                                           begin
                                             ungetregister32(p^.methodpointer^.location.register);
                                             emit_reg_reg(A_MOVE,S_L,p^.methodpointer^.location.register,R_A5);
                                           end;
                                        else
                                           begin
                                                 if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.isclass then
                                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_A5)))
                                                 else
                                                  Begin
                                                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_A5)));
                                                  end;

                                                del_reference(p^.methodpointer^.location.reference);
                                             end;
                                     end;
                                  end;
                                    { when calling a class method, we have
                                      to load ESI with the VMT !
                                      But that's wrong, if we call a class method via self
                                    }
                                    if ((p^.procdefinition^.options and poclassmethod)<>0)
                                       and not(p^.methodpointer^.treetype=selfn) then
                                      begin
                                         { class method needs current VMT }
                                         new(r);
                                         reset_reference(r^);
                                         r^.base:=R_A5;
                                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_A5)));
                                      end;

                                   exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
                                   if is_con_or_destructor then
                                   begin
                                         { classes don't get a VMT pointer pushed }
                                         if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                           begin

                                            if ((p^.procdefinition^.options and poconstructor)<>0) then
                                              begin
                                               { it's no bad idea, to insert the VMT }
                                                      exprasmlist^.concat(new(pai68k,op_csymbol(A_PEA,S_L,
                                               newcsymbol(pobjectdef(
                                                 p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                               concat_external(pobjectdef(
                                                 p^.methodpointer^.resulttype)^.vmt_mangledname,EXT_NEAR);
                                              end
                                            { destructors haven't to dispose the instance, if this is }
                                            { a direct call                                           }
                                            else
                                              push_int(0);
                                           end;
                                  end;
                             end;
                        end;
                     end
                   else
                     begin
                         if ((p^.procdefinition^.options and poclassmethod)<>0) and
                          not(
                            assigned(aktprocsym) and
                            ((aktprocsym^.definition^.options and poclassmethod)<>0)
                          ) then
                          begin
                             { class method needs current VMT }
                             new(r);
                             reset_reference(r^);
                             r^.base:=R_A5;
                             exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_A5)));
                          end
                        else
                          begin
                             { member call, A5 isn't modified }
                             loada5:=false;
                          end;
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,R_A5,R_SPPUSH)));
            { but a con- or destructor here would probably almost }
                        { always be placed wrong }
                        if is_con_or_destructor then
                          begin
                             CGMessage(cg_w_member_cd_call_from_method);
                             { not insert VMT pointer }                             { VMT-Zeiger nicht eintragen }
                             push_int(0);
                          end;
                     end;
                end;

              { push base pointer ?}
              if (lexlevel>1) and assigned(pprocdef(p^.procdefinition)^.parast) and
            ((p^.procdefinition^.parast^.symtablelevel)>2) then
                    begin
                   { if we call a nested function in a method, we must      }
                   { push also SELF!                                        }
                   { THAT'S NOT TRUE, we have to load ESI via frame pointer }
                   { access                                                 }
                   {
                     begin
                        loadesi:=false;
                        exprasmlist^.concat(new(pai68k,op_reg(A_PUSH,S_L,R_ESI)));
                     end;
                   }
                   if lexlevel=(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_SPPUSH)))
                     end
                     { this is only true if the difference is one !!
                       but it cannot be more !! }
                   else if lexlevel=(p^.procdefinition^.parast^.symtablelevel)-1 then
                     begin
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,procinfo.framepointer,R_SPPUSH)))
                     end
                   else if lexlevel>(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        hregister:=getaddressreg;
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,hregister)));
                        for i:=(p^.procdefinition^.parast^.symtablelevel) to lexlevel-1 do
                          begin
                             new(r);
                             reset_reference(r^);
                             {we should get the correct frame_pointer_offset at each level
                             how can we do this !!! }
                             r^.offset:=procinfo.framepointer_offset;
                             r^.base:=hregister;
                             exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,hregister)));
                          end;
                        exprasmlist^.concat(new(pai68k,op_reg_reg(A_MOVE,S_L,hregister,R_SPPUSH)));
                        ungetregister32(hregister);
                     end
                   else
                     internalerror(25000);
                end;

              { exported methods should be never called direct }
              if (p^.procdefinition^.options and poexports)<>0 then
               CGMessage(cg_e_dont_call_exported_direct);

              if ((p^.procdefinition^.options and povirtualmethod)<>0) and
                 not(no_virtual_call) then
                begin
                   { static functions contain the vmt_address in ESI }
                   { also class methods                              }
                   if assigned(aktprocsym) then
                     begin
                       if ((aktprocsym^.properties and sp_static)<>0) or
                        ((aktprocsym^.definition^.options and poclassmethod)<>0) or
                        ((p^.procdefinition^.options and postaticmethod)<>0) or
                        { A5 is already loaded  }
                        ((p^.procdefinition^.options and poclassmethod)<>0)then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_a5;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_a5;
                            exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_a0)));
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_a0;
                         end;
                     end
                   else
                     begin
                       new(r);
                       reset_reference(r^);
                         r^.base:=R_a5;
                       exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,r,R_a0)));
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_a0;
                     end;
                  if p^.procdefinition^.extnumber=-1 then
                        internalerror($Da);
                  r^.offset:=p^.procdefinition^.extnumber*4+12;
                  if (cs_check_range in aktlocalswitches) then
                    begin
                     { If the base is already A0, the no instruction will }
                     { be emitted!                                        }
                     emit_reg_reg(A_MOVE,S_L,r^.base,R_A0);
                        emitcall('FPC_CHECK_OBJECT',true);
                    end;
                   { This was wrong we must then load the address into the }
                   { register a0 and/or a5                                 }
                   { Because doing an indirect call with offset is NOT     }
                   { allowed on the m68k!                                  }
                   exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,newreference(r^),R_A0)));
                   { clear the reference }
                   reset_reference(r^);
                   r^.base := R_A0;
                  exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,r)));
                end
              else if (p^.procdefinition^.options and popalmossyscall)<>0 then
                begin
                   exprasmlist^.concat(new(pai68k,op_const(A_TRAP,S_NO,15)));
                   exprasmlist^.concat(new(pai_const,init_16bit(p^.procdefinition^.extnumber)));
                end
              else
                emitcall(p^.procdefinition^.mangledname,
                  p^.symtableproc^.symtabletype=unitsymtable);
              if ((p^.procdefinition^.options and poclearstack)<>0) then
                begin
                   if (pushedparasize > 0) and (pushedparasize < 9) then
                     { restore the stack, to its initial value }
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_ADDQ,S_L,pushedparasize,R_SP)))
                   else
                     { restore the stack, to its initial value }
                     exprasmlist^.concat(new(pai68k,op_const_reg(A_ADDA,S_L,pushedparasize,R_SP)));
                end;
           end
         else
           begin
              secondpass(p^.right);
              case p^.right^.location.loc of
                 LOC_REGISTER,
                 LOC_CREGISTER : begin
                                   if p^.right^.location.register in [R_D0..R_D7] then
                                    begin
                                       reg := getaddressreg;
                                       emit_reg_reg(A_MOVE,S_L,p^.right^.location.register,reg);
                                       new(ref);
                                       reset_reference(ref^);
                                       ref^.base := reg;
                                       exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,ref)));
                                       ungetregister(reg);
                                    end
                                   else
                                    begin
                                        new(ref);
                                        reset_reference(ref^);
                                        ref^.base := p^.right^.location.register;
                                        exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,ref)));
                                    end;
                                   ungetregister32(p^.right^.location.register);
                                end
                 else
                    begin
                      if assigned(p^.right^.location.reference.symbol) then
                      { Here we have a symbolic name to the routine, so solve  }
                      { problem by loading the address first, and then emitting }
                      { the call.                                              }
                       begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                           newreference(p^.right^.location.reference),R_A1)));
                         new(ref);
                         reset_reference(ref^);
                         ref^.base := R_A1;
                         exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,newreference(ref^))));
                       end
                       else
                       begin
                         exprasmlist^.concat(new(pai68k,op_ref_reg(A_MOVE,S_L,
                           newreference(p^.right^.location.reference),R_A1)));
                         new(ref);
                         reset_reference(ref^);
                         ref^.base := R_A1;
                         exprasmlist^.concat(new(pai68k,op_ref(A_JSR,S_NO,newreference(ref^))));
                       end;
                       del_reference(p^.right^.location.reference);
                    end;
              end;
           end;
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;

         { handle function results }
         if p^.resulttype<>pdef(voiddef) then
           begin

              { a contructor could be a function with boolean result }
              if (p^.right=nil) and
                 ((p^.procdefinition^.options and poconstructor)<>0) and
                 { quick'n'dirty check if it is a class or an object }
                 (p^.resulttype^.deftype=orddef) then
                begin
                   p^.location.loc:=LOC_FLAGS;
                   p^.location.resflags:=F_NE;
                   if extended_new then
                     begin
{$ifdef test_dest_loc}
                        if dest_loc_known and (dest_loc_tree=p) then
                          mov_reg_to_dest(p,S_L,R_EAX)
                        else
{$endif test_dest_loc}
                               hregister:=getregister32;
                               emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                               p^.location.register:=hregister;
                     end;
                end
              { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype) then
                begin
                   p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                end
              else
                begin
                   if (p^.resulttype^.deftype=orddef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                  case porddef(p^.resulttype)^.typ of
                     s32bit,u32bit :
                        begin
                             hregister:=getregister32;
                             emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                             p^.location.register:=hregister;
                        end;
                     uchar,u8bit,bool8bit,s8bit :
                        begin
                            hregister:=getregister32;
                            emit_reg_reg(A_MOVE,S_B,R_D0,hregister);
                            p^.location.register:=hregister;
                        end;
                     s16bit,u16bit :
                       begin
                           hregister:=getregister32;
                           emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                           p^.location.register:=hregister;
                       end;
                           else internalerror(7);
                        end
                     end
                   else if (p^.resulttype^.deftype=floatdef) then
                      case pfloatdef(p^.resulttype)^.typ of
                           f32bit :
                              begin
                                p^.location.loc:=LOC_REGISTER;
                                hregister:=getregister32;
                                emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                                p^.location.register:=hregister;
                      end;
                     s32real :  Begin
                                   p^.location.loc:=LOC_FPU;
                                   hregister:=getregister32;
                                   emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                                   p^.location.fpureg:=hregister;
                                end;
                     s64bit,s64real,s80real: begin
                                              if cs_fp_emulation in aktmoduleswitches then
                                              begin
                                                p^.location.loc:=LOC_FPU;
                                                hregister:=getregister32;
                                                emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
                                                p^.location.fpureg:=hregister;
                                              end
                                              else
                                              begin
                                                { TRUE FPU mode }
                                                p^.location.loc:=LOC_FPU;
                                                { on exit of function result in R_FP0 }
                                                p^.location.fpureg:=R_FP0;
                                              end;
                                             end;
                           else
                      begin
                              p^.location.loc:=LOC_FPU;
                              p^.location.fpureg:=R_FP0;
                      end;
             end {end case }
       else
        begin
            p^.location.loc:=LOC_REGISTER;
            hregister:=getregister32;
            emit_reg_reg(A_MOVE,S_L,R_D0,hregister);
            p^.location.register:=hregister;
                end;
           end;
         end;
         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              exprasmlist^.concat(new(pai68k,op_csymbol(A_PEA,S_L,newcsymbol(lab2str(iolabel),0))));
              emitcall('FPC_IOCHECK',true);
           end;

         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loada5 then
           maybe_loada5;
         pp:=params;
         while assigned(pp) do
           begin
             if assigned(pp^.left) then
               if (pp^.left^.location.loc=LOC_REFERENCE) or
                 (pp^.left^.location.loc=LOC_MEM) then
                 ungetiftemp(pp^.left^.location.reference);
               pp:=pp^.right;
           end;
         disposetree(params);
      end;


{*****************************************************************************
                             SecondProcInlineN
*****************************************************************************}

    procedure secondprocinline(var p : ptree);
       begin
         InternalError(132421);
       end;



end.
{
  $Log$
  Revision 1.5  1998-09-17 09:42:22  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.4  1998/09/14 10:43:55  peter
    * all internal RTL functions start with FPC_

  Revision 1.3  1998/09/04 08:41:43  peter
    * updated some error CGMessages

  Revision 1.2  1998/09/01 12:47:59  peter
    * use pdef^.size instead of orddef^.typ

  Revision 1.1  1998/09/01 09:07:09  peter
    * m68k fixes, splitted cg68k like cgi386

}

