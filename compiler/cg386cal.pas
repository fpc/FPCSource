{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for in call nodes

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
unit cg386cal;
interface

    uses
      symtable,tree;

    { save the size of pushed parameter }
    var
       pushedparasize : longint;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right,inlined : boolean;para_offset : longint);
    procedure secondcalln(var p : ptree);
    procedure secondprocinline(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,systems,
      aasm,types,
      hcodegen,temp_gen,pass_2,
      i386,cgai386,tgeni386,cg386ld;

{*****************************************************************************
                             SecondCallParaN
*****************************************************************************}


    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right,inlined : boolean;para_offset : longint);

      procedure maybe_push_open_array_high;
        var
           r    : preference;
           hreg : tregister;
           href : treference;
           len  : longint;
        begin
           { open array ? }
           { defcoll^.data can be nil for read/write }
           if assigned(defcoll^.data) and
              is_open_array(defcoll^.data) then
             begin
              { push high }
               case p^.left^.resulttype^.deftype of
                arraydef : begin
                             if is_open_array(p^.left^.resulttype) then
                              begin
                                r:=new_reference(highframepointer,highoffset+4);
                                len:=-1;
                              end
                             else
                              len:=parraydef(p^.left^.resulttype)^.highrange-
                                   parraydef(p^.left^.resulttype)^.lowrange
                           end;
               stringdef : begin
                             if p^.left^.treetype=stringconstn then
                              len:=length(p^.left^.value_str^)
                             else
                              begin
                                href:=p^.left^.location.reference;
                                dec(href.offset);
                                hreg:=reg32toreg8(getregister32);
                                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_B,newreference(href),hreg)));
                                emit_to_reg32(hreg);
                                len:=-2;
                              end;
                           end;
               else
                len:=0;
               end;
             { Push from the reference? }
               if len=-1 then
                begin
                  if inlined then
                   begin
                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                   end
                  else
                   exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,r)));
                end
               else
               { Push from a register? }
                if len=-2 then
                 begin
                   if inlined then
                    begin
                      r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                      exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,hreg,r)));
                    end
                   else
                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,hreg)));
                   ungetregister32(hreg);
                 end
               else
               { Push direct value }
                begin
                  if inlined then
                    begin
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,len,r)));
                    end
                  else
                    push_int(len);
                end;
               inc(pushedparasize,4);
             end;
        end;

      var
         size : longint;
         stackref : treference;
         otlabel,hlabel,oflabel : plabel;
         { temporary variables: }
         tempdeftype : tdeftype;
         tempreference : treference;
         r : preference;
         s : topsize;
         op : tasmop;

      begin
         { push from left to right if specified }
         if push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right,inlined,para_offset);
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
              inc(pushedparasize,4);
              if p^.left^.treetype=addrn then
                begin
                { always a register }
                  if inlined then
                    begin
                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                       exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                         p^.left^.location.register,r)));
                    end
                  else
                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.left^.location.register)));
                  ungetregister32(p^.left^.location.register);
                end
              else
                begin
                   if not(p^.left^.location.loc in [LOC_MEM,LOC_REFERENCE]) then
                     CGMessage(type_e_mismatch)
                   else
                     begin
                       if inlined then
                         begin
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                             newreference(p^.left^.location.reference),R_EDI)));
                           r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                           exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                         end
                      else
                        emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) then
           begin
              if (p^.left^.location.loc<>LOC_REFERENCE) then
                CGMessage(cg_e_var_must_be_reference);
              maybe_push_open_array_high;
              inc(pushedparasize,4);
              if inlined then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                     newreference(p^.left^.location.reference),R_EDI)));
                   r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                end
              else
                emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
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
                   if inlined then
                     begin
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                          newreference(p^.left^.location.reference),R_EDI)));
                        r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                        exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                          R_EDI,r)));
                     end
                   else
                     emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                   del_reference(p^.left^.location.reference);
                end
              else
                case p^.left^.location.loc of
                   LOC_REGISTER,
                   LOC_CREGISTER:
                     begin
                        case p^.left^.location.register of
                           R_EAX,R_EBX,R_ECX,R_EDX,R_ESI,
                           R_EDI,R_ESP,R_EBP :
                             begin
                                inc(pushedparasize,4);
                                if inlined then
                                  begin
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                       p^.left^.location.register,r)));
                                  end
                                else
                                  exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,p^.left^.location.register)));
                                ungetregister32(p^.left^.location.register);
                             end;
                           R_AX,R_BX,R_CX,R_DX,R_SI,R_DI:
                             begin
                                 inc(pushedparasize,2);
                                 if inlined then
                                   begin
                                      r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                      exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                                        p^.left^.location.register,r)));
                                   end
                                 else
                                   exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,p^.left^.location.register)));
                                 ungetregister32(reg16toreg32(p^.left^.location.register));
                              end;
                           R_AL,R_BL,R_CL,R_DL:
                             begin
                                inc(pushedparasize,2);
                                { we must push always 16 bit }
                                if inlined then
                                  begin
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                       reg8toreg16(p^.left^.location.register),r)));
                                  end
                                else
                                  exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,
                                    reg8toreg16(p^.left^.location.register))));
                                ungetregister32(reg8toreg32(p^.left^.location.register));
                             end;
                        end;
                     end;
                   LOC_FPU:
                     begin
                        size:=pfloatdef(p^.left^.resulttype)^.size;
                        inc(pushedparasize,size); { must be before for inlined }
                        if not inlined then
                        exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,size,R_ESP)));
                        r:=new_reference(R_ESP,0);
                        floatstoreops(pfloatdef(p^.left^.resulttype)^.typ,op,s);
                        { this is the easiest case for inlined !! }
                        if inlined then
                          begin
                             r^.base:=procinfo.framepointer;
                             r^.offset:=para_offset-pushedparasize;
                          end;
                        exprasmlist^.concat(new(pai386,op_ref(op,s,r)));
                     end;
                   LOC_REFERENCE,LOC_MEM:
                     begin
                        tempreference:=p^.left^.location.reference;
                        del_reference(p^.left^.location.reference);
                        case p^.resulttype^.deftype of
                        enumdef,
                        orddef :
                          begin
                            case p^.resulttype^.size of
                             4 : begin
                                    inc(pushedparasize,4);
                                    if inlined then
                                      begin
                                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                           newreference(tempreference),R_EDI)));
                                         r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                         exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                      end
                                    else
                                      emit_push_mem(tempreference);
                                 end;
                           1,2 : begin
                                   inc(pushedparasize,2);
                                   if inlined then
                                     begin
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,
                                          newreference(tempreference),R_DI)));
                                        r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                        exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,R_DI,r)));
                                     end
                                   else
                                     exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,
                                       newreference(tempreference))));
                                 end;
                             else
                              internalerror(234231);
                             end;
                          end;
                        floatdef :
                          begin
                            case pfloatdef(p^.resulttype)^.typ of
                            f32bit,
                            s32real :
                              begin
                                 inc(pushedparasize,4);
                                 if inlined then
                                   begin
                                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                        newreference(tempreference),R_EDI)));
                                      r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                      exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                   end
                                 else
                                   emit_push_mem(tempreference);
                              end;
                            s64real,
                            s64bit :
                              begin
                                inc(pushedparasize,4);
                                inc(tempreference.offset,4);
                                if inlined then
                                  begin
                                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                       newreference(tempreference),R_EDI)));
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                  end
                                else
                                  emit_push_mem(tempreference);
                                inc(pushedparasize,4);
                                dec(tempreference.offset,4);
                                if inlined then
                                  begin
                                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                       newreference(tempreference),R_EDI)));
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                  end
                                else
                                  emit_push_mem(tempreference);
                              end;
                            s80real :
                              begin
                                inc(pushedparasize,4);
                                inc(tempreference.offset,6);
                                if inlined then
                                  begin
                                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                       newreference(tempreference),R_EDI)));
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                  end
                                else
                                  emit_push_mem(tempreference);
                                dec(tempreference.offset,4);
                                inc(pushedparasize,4);
                                if inlined then
                                  begin
                                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                       newreference(tempreference),R_EDI)));
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                  end
                                else
                                  emit_push_mem(tempreference);
                                dec(tempreference.offset,2);
                                inc(pushedparasize,2);
                                if inlined then
                                  begin
                                     exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,
                                       newreference(tempreference),R_DI)));
                                     r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                     exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,R_DI,r)));
                                  end
                                else
                                  exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,
                                    newreference(tempreference))));
                              end;
                            end;
                          end;
                        pointerdef,procvardef,
                        classrefdef:
                          begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                  exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                    newreference(tempreference),R_EDI)));
                                  r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                               end
                             else
                               emit_push_mem(tempreference);
                          end;
                        arraydef,recorddef,stringdef,setdef,objectdef :
                          begin
                             { 32 bit type set ? }
                             if is_widestring(p^.resulttype) or
                               is_ansistring(p^.resulttype) or
                                ((p^.resulttype^.deftype=setdef) and
                                 (psetdef(p^.resulttype)^.settype=smallset)) then
                               begin
                                  inc(pushedparasize,4);
                                  if inlined then
                                    begin
                                      r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                      concatcopy(tempreference,r^,4,false);
                                    end
                                  else
                                    emit_push_mem(tempreference);
                               end
                             { call by value open array ? }
                             else
                              if (p^.resulttype^.deftype=arraydef) and
                                 assigned(defcoll^.data) and
                                 is_open_array(defcoll^.data) then
                               begin
                                  { first, push high }
                                  maybe_push_open_array_high;
                                  inc(pushedparasize,4);
                                  if inlined then
                                    begin
                                       exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                         newreference(p^.left^.location.reference),R_EDI)));
                                       r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                       exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                         R_EDI,r)));
                                    end
                                  else
                                    emitpushreferenceaddr(exprasmlist,p^.left^.location.reference);
                                end
                              else
                               begin
                                  size:=p^.resulttype^.size;
                                  { Word Alignment }
                                  if Odd(size) then
                                   inc(size);
                                  { create stack space }
                                  if not inlined then
                                    exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,size,R_ESP)));
                                  inc(pushedparasize,size);
                                  { create stack reference }
                                  stackref.symbol := nil;
                                  if not inlined then
                                    begin
                                      clear_reference(stackref);
                                      stackref.base:=R_ESP;
                                    end
                                  else
                                    begin
                                      clear_reference(stackref);
                                      stackref.base:=procinfo.framepointer;
                                      stackref.offset:=para_offset-pushedparasize;
                                    end;
                                  { generate copy }
                                  if is_shortstring(p^.resulttype) then
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
                        else
                          CGMessage(cg_e_illegal_expression);
                        end;
                     end;
                   LOC_JUMP:
                     begin
                        getlabel(hlabel);
                        inc(pushedparasize,2);
                        emitl(A_LABEL,truelabel);
                        if inlined then
                          begin
                             r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                             exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_W,1,r)));
                          end
                        else
                          exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_W,1)));
                        emitl(A_JMP,hlabel);
                        emitl(A_LABEL,falselabel);
                        if inlined then
                          begin
                             r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                             exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_W,0,r)));
                          end
                        else
                          exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_W,0)));
                        emitl(A_LABEL,hlabel);
                     end;
                   LOC_FLAGS:
                     begin
                        if not(R_EAX in unused) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EAX,R_EDI)));

                        { clear full EAX is faster }
                        { but dont you set the equal flag ? }
                        {exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_EAX,R_EAX)));}
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.left^.location.resflags],S_B,
                          R_AL)));
                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVZX,S_BW,R_AL,R_AX)));
                        {exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_EAX,R_EAX)));}
                        inc(pushedparasize,2);
                        if inlined then
                          begin
                             r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                             exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                               R_AX,r)));
                          end
                        else
                          exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_W,R_AX)));
                        { this is also false !!!
                        if not(R_EAX in unused) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EAX,R_EDI)));}
                        if not(R_EAX in unused) then
                          exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,R_EDI,R_EAX)));
                     end;
{$ifdef SUPPORT_MMX}
                   LOC_MMXREGISTER,
                   LOC_CMMXREGISTER:
                     begin
                        inc(pushedparasize,8); { was missing !!! (PM) }
                        exprasmlist^.concat(new(pai386,op_const_reg(
                          A_SUB,S_L,8,R_ESP)));
                        if inlined then
                          begin
                             r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                             exprasmlist^.concat(new(pai386,op_reg_ref(A_MOVQ,S_NO,
                               p^.left^.location.register,r)));
                          end
                        else
                           begin
                              r:=new_reference(R_ESP,0);
                              exprasmlist^.concat(new(pai386,op_reg_ref(
                             A_MOVQ,S_NO,p^.left^.location.register,r)));
                        end;
                     end;
{$endif SUPPORT_MMX}
                end;
           end;
         truelabel:=otlabel;
         falselabel:=oflabel;
         { push from right to left }
         if not push_from_left_to_right and assigned(p^.right) then
           secondcallparan(p^.right,defcoll^.next,push_from_left_to_right,inlined,para_offset);
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
         { true if ESI must be loaded again after the subroutine }
         loadesi : boolean;
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
         inlined : boolean;
         inlinecode : ptree;
         para_offset : longint;
         { instruction for alignement correction }
{         corr : pai386;}
         { we must pop this size also after !! }
{         must_pop : boolean; }
         pop_size : longint;

      label
         dont_call;

      begin
         extended_new:=false;
         iolabel:=nil;
         inlinecode:=nil;
         inlined:=false;
         loadesi:=true;
         no_virtual_call:=false;
         unusedregisters:=unused;

         if not assigned(p^.procdefinition) then
          exit;
         if (p^.procdefinition^.options and poinline)<>0 then
           begin
              inlined:=true;
              inlinecode:=p^.right;
              { set it to the same lexical level }
              p^.procdefinition^.parast^.symtablelevel:=
                aktprocsym^.definition^.parast^.symtablelevel;
              if assigned(p^.left) then
                inlinecode^.para_offset:=
                  gettempofsizepersistant(inlinecode^.para_size);
              p^.procdefinition^.parast^.call_offset:=
                inlinecode^.para_offset;
{$ifdef extdebug}
             Comment(V_debug,
               'inlined parasymtable is at offset '
               +tostr(p^.procdefinition^.parast^.call_offset));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew('inlined parasymtable is at offset '
               +tostr(p^.procdefinition^.parast^.call_offset)))));
{$endif extdebug}
              p^.right:=nil;
              { disable further inlining of the same proc
                in the args }
              p^.procdefinition^.options:=p^.procdefinition^.options and (not poinline);
           end;
         { only if no proc var }
         if not(assigned(p^.right)) then
           is_con_or_destructor:=((p^.procdefinition^.options and poconstructor)<>0)
             or ((p^.procdefinition^.options and podestructor)<>0);
         { proc variables destroy all registers }
         if (p^.right=nil) and
            { virtual methods too }
            ((p^.procdefinition^.options and povirtualmethod)=0) then
           begin
              if ((p^.procdefinition^.options and poiocheck)<>0) and
                 (cs_check_io in aktlocalswitches) then
                begin
                   getlabel(iolabel);
                   emitl(A_LABEL,iolabel);
                end
              else
                iolabel:=nil;

              { save all used registers }
              pushusedregisters(pushed,p^.procdefinition^.usedregisters);

              { give used registers through }
              usedinproc:=usedinproc or p^.procdefinition^.usedregisters;
           end
         else
           begin
              pushusedregisters(pushed,$ff);
              usedinproc:=$ff;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         pop_size:=0;
         if (not inlined) then
          begin
          { Old pushedsize aligned on 4 ? }
            i:=oldpushedparasize and 3;
            if i>0 then
             inc(pop_size,4-i);
          { This parasize aligned on 4 ? }
            i:=p^.procdefinition^.para_size and 3;
            if i>0 then
             inc(pop_size,4-i);
          { insert the opcode and update pushedparasize }
            if pop_size>0 then
             begin
               inc(pushedparasize,pop_size);
               exprasmlist^.concat(new(pai386,op_const_reg(A_SUB,S_L,pop_size,R_ESP)));
             end;
          end;

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
                if inlined then
                  begin
                     reset_reference(funcretref);
                     funcretref.offset:=gettempofsizepersistant(p^.procdefinition^.retdef^.size);
                     funcretref.base:=procinfo.framepointer;
                  end
                else
                  gettempofsizereference(p^.procdefinition^.retdef^.size,funcretref);
           end;
         if assigned(p^.left) then
           begin
              { be found elsewhere }
              if inlined then
                para_offset:=p^.procdefinition^.parast^.call_offset+
                  p^.procdefinition^.parast^.datasize
              else
                para_offset:=0;
              if assigned(p^.right) then
                secondcallparan(p^.left,pprocvardef(p^.right^.resulttype)^.para1,
                  (p^.procdefinition^.options and poleftright)<>0,inlined,para_offset)
              else
                secondcallparan(p^.left,p^.procdefinition^.para1,
                  (p^.procdefinition^.options and poleftright)<>0,inlined,para_offset);
           end;
         params:=p^.left;
         p^.left:=nil;
         if inlined then
           inlinecode^.retoffset:=gettempofsizepersistant(4);
         if ret_in_param(p^.resulttype) then
           begin
              inc(pushedparasize,4);
              if inlined then
                begin
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                     newreference(funcretref),R_EDI)));
                   r:=new_reference(procinfo.framepointer,inlinecode^.retoffset);
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                     R_EDI,r)));
                end
              else
                emitpushreferenceaddr(exprasmlist,funcretref);
           end;
         { procedure variable ? }
         if (p^.right=nil) then
           begin
              { overloaded operator have no symtable }
              { push self }
              if assigned(p^.symtable) and
                (p^.symtable^.symtabletype=withsymtable) then
                begin
                   { dirty trick to avoid the secondcall below }
                   p^.methodpointer:=genzeronode(callparan);
                   p^.methodpointer^.location.loc:=LOC_REGISTER;
                   p^.methodpointer^.location.register:=R_ESI;
                   p^.methodpointer^.resulttype:=p^.symtable^.defowner;
                   { make a reference }
                   new(r);
                   reset_reference(r^);
                   r^.offset:=p^.symtable^.datasize;
                   r^.base:=procinfo.framepointer;
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                end;

              { push self }
              if assigned(p^.symtable) and
                ((p^.symtable^.symtabletype=objectsymtable) or
                (p^.symtable^.symtabletype=withsymtable)) then
                begin
                   if assigned(p^.methodpointer) then
                     begin
                        {
                        if p^.methodpointer^.resulttype=classrefdef then
                          begin
                              two possibilities:
                               1. constructor
                               2. class method

                          end
                        else }
                          begin
                             case p^.methodpointer^.treetype of
                               typen:
                                 begin
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
                                         { well lets put the VMT address directly into ESI }
                                         { it is kind of dirty but that is the simplest    }
                                         { way to accept virtual static functions (PM)     }
                                         loadesi:=true;
                                         exprasmlist^.concat(new(pai386,op_csymbol_reg(A_MOV,S_L,
                                           newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0),R_ESI)));
                                         maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                           pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                         exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                      end
                                    else
                                      { this is a member call, so ESI isn't modfied }
                                      loadesi:=false;
                                    if not(is_con_or_destructor and
                                      pobjectdef(p^.methodpointer^.resulttype)^.isclass and
                                        assigned(aktprocsym) and
                                        ((aktprocsym^.definition^.options and
                                        (poconstructor or podestructor))<>0)) then
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    { if an inherited con- or destructor should be  }
                                    { called in a con- or destructor then a warning }
                                    { will be made                                  }
                                    { con- and destructors need a pointer to the vmt }
                                    if is_con_or_destructor and
                                    not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) and
                                    assigned(aktprocsym) then
                                      begin
                                         if not ((aktprocsym^.definition^.options
                                           and (poconstructor or podestructor))<>0) then

                                          CGMessage(cg_w_member_cd_call_from_method);
                                      end;
                                    if is_con_or_destructor then
                                      push_int(0)
                                 end;
                               hnewn:
                                 begin
                                    { extended syntax of new }
                                    { ESI must be zero }
                                    exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_L,R_ESI,R_ESI)));
                                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    { insert the vmt }
                                    exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,
                                      newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                    maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                      pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                    extended_new:=true;
                                 end;
                               hdisposen:
                                 begin
                                    secondpass(p^.methodpointer);

                                    { destructor with extended syntax called from dispose }
                                    { hdisposen always deliver LOC_REFERENCE              }
                                    exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                      newreference(p^.methodpointer^.location.reference),R_ESI)));
                                    del_reference(p^.methodpointer^.location.reference);
                                    exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,
                                    newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,0))));
                                    maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                      pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                 end;
                               else
                                 begin
                                    { call to an instance member }
                                    if (p^.symtable^.symtabletype<>withsymtable) then
                                      begin
                                         secondpass(p^.methodpointer);
                                         case p^.methodpointer^.location.loc of
                                            LOC_CREGISTER,
                                            LOC_REGISTER:
                                              begin
                                                 ungetregister32(p^.methodpointer^.location.register);
                                                 emit_reg_reg(A_MOV,S_L,p^.methodpointer^.location.register,R_ESI);
                                              end;
                                            else
                                              begin
                                                 if (p^.methodpointer^.resulttype^.deftype=classrefdef) or
                                                    ((p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)))
                                                 else
                                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                                                     newreference(p^.methodpointer^.location.reference),R_ESI)));
                                                 del_reference(p^.methodpointer^.location.reference);
                                              end;
                                         end;
                                      end;
                                    { when calling a class method, we have
                                      to load ESI with the VMT !
                                      But that's wrong, if we call a class method via self
                                    }
                                    if ((p^.procdefinition^.options and poclassmethod)<>0)
                                       and not(p^.methodpointer^.resulttype^.deftype=classrefdef) then
                                      begin
                                         { class method needs current VMT }
                                         new(r);
                                         reset_reference(r^);
                                         r^.base:=R_ESI;
                                         r^.offset:= p^.procdefinition^._class^.vmt_offset;
                                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                                      end;

                                    { direct call to class constructor, don't allocate memory }
                                    if is_con_or_destructor and
                                      (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                      (pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                      exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,0)))
                                    else
                                      exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                                    if is_con_or_destructor then
                                      begin
                                         { classes don't get a VMT pointer pushed }
                                         if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                           not(pobjectdef(p^.methodpointer^.resulttype)^.isclass) then
                                           begin
                                              if ((p^.procdefinition^.options and poconstructor)<>0) then
                                                begin
                                                   { it's no bad idea, to insert the VMT }
                                                   exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,
                                                     newcsymbol(pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname,
                                                     0))));
                                                   maybe_concat_external(pobjectdef(p^.methodpointer^.resulttype)^.owner,
                                                     pobjectdef(p^.methodpointer^.resulttype)^.vmt_mangledname);
                                                end
                                              { destructors haven't to dispose the instance, if this is }
                                              { a direct call                                           }
                                              else
                                                push_int(0);
                                           end;
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
                             r^.base:=R_ESI;
                             r^.offset:= p^.procdefinition^._class^.vmt_offset;
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                          end
                        else
                          begin
                             { member call, ESI isn't modified }
                             loadesi:=false;
                          end;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                        { but a con- or destructor here would probably almost }
                        { always be placed wrong }
                        if is_con_or_destructor then
                          begin
                             CGMessage(cg_w_member_cd_call_from_method);
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
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,R_ESI)));
                     end;
                   }
                   if lexlevel=(p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,r)))
                     end
                     { this is only true if the difference is one !!
                       but it cannot be more !! }
                   else if (lexlevel=p^.procdefinition^.parast^.symtablelevel-1) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,procinfo.framepointer)))
                     end
                   else if (lexlevel>p^.procdefinition^.parast^.symtablelevel) then
                     begin
                        hregister:=getregister32;
                        new(r);
                        reset_reference(r^);
                        r^.offset:=procinfo.framepointer_offset;
                        r^.base:=procinfo.framepointer;
                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,hregister)));
                        for i:=(p^.procdefinition^.parast^.symtablelevel) to lexlevel-1 do
                          begin
                             new(r);
                             reset_reference(r^);
                             {we should get the correct frame_pointer_offset at each level
                             how can we do this !!! }
                             r^.offset:=procinfo.framepointer_offset;
                             r^.base:=hregister;
                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,hregister)));
                          end;
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,hregister)));
                        ungetregister32(hregister);
                     end
                   else
                     internalerror(25000);
                end;

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
                        ((p^.procdefinition^.options and poconstructor)<>0) or
                        { ESI is loaded earlier }
                        ((p^.procdefinition^.options and poclassmethod)<>0)then
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                         end
                       else
                         begin
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_ESI;
                            { this is one point where we need vmt_offset (PM) }
                            r^.offset:= p^.procdefinition^._class^.vmt_offset;
                            exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                            new(r);
                            reset_reference(r^);
                            r^.base:=R_EDI;
                         end;
                     end
                   else
                     { aktprocsym should be assigned, also in main program }
                     internalerror(12345);
                   {
                     begin
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_ESI;
                       exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_EDI)));
                       new(r);
                       reset_reference(r^);
                       r^.base:=R_EDI;
                     end;
                   }
                   if p^.procdefinition^.extnumber=-1 then
                        internalerror($Da);
                   r^.offset:=p^.procdefinition^.extnumber*4+12;
                   if (cs_check_range in aktlocalswitches) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,r^.base)));
                        emitcall('FPC_CHECK_OBJECT',true);
                     end;
                   exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,r)));
                end
              else if not inlined then
                emitcall(p^.procdefinition^.mangledname,
                  (p^.symtableproc^.symtabletype=unitsymtable) or
                  ((p^.symtableproc^.symtabletype=objectsymtable) and
                  (pobjectdef(p^.symtableproc^.defowner)^.owner^.symtabletype=unitsymtable)))
              else { inlined proc }
                { inlined code is in inlinecode }
                begin
                   secondpass(inlinecode);
                   { set poinline again }
                   p^.procdefinition^.options:=p^.procdefinition^.options or poinline;
                   { free the args }
                   ungetpersistanttemp(p^.procdefinition^.parast^.call_offset,
                     p^.procdefinition^.parast^.datasize);
                end;
           end
         else
           { now procedure variable case }
           begin
              secondpass(p^.right);
              { method pointer ? }
              if (p^.procdefinition^.options and pomethodpointer)<>0 then
                begin
                   { method pointer can't be in a register }
                   inc(p^.right^.location.reference.offset,4);
                   { push self pointer }
                   exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_L,newreference(p^.right^.location.reference))));
                   del_reference(p^.right^.location.reference);
                   dec(p^.right^.location.reference.offset,4);
                end;
              case p^.right^.location.loc of
                 LOC_REGISTER,LOC_CREGISTER:
                    begin
                        exprasmlist^.concat(new(pai386,op_reg(A_CALL,S_NO,p^.right^.location.register)));
                        ungetregister32(p^.right^.location.register);
                    end
                 else
                    exprasmlist^.concat(new(pai386,op_ref(A_CALL,S_NO,newreference(p^.right^.location.reference))));
                    del_reference(p^.right^.location.reference);
              end;
             end;

           { this was only for normal functions
             displaced here so we also get
             it to work for procvars PM }
           if (not inlined) and ((p^.procdefinition^.options and poclearstack)<>0) then
             begin
                { consider the alignment with the rest (PM) }
                inc(pushedparasize,pop_size);
                pop_size:=0;
                { better than an add on all processors }
                if pushedparasize=4 then
                  exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)))
                { the pentium has two pipes and pop reg is pairable }
                { but the registers must be different!              }
                else if (pushedparasize=8) and
                  not(cs_littlesize in aktglobalswitches) and
                  (aktoptprocessor=ClassP5) and
                  (procinfo._class=nil) then
                    begin
                       exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));
                       exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
                    end
                else exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pushedparasize,R_ESP)));
             end;
      dont_call:
         pushedparasize:=oldpushedparasize;
         unused:=unusedregisters;

         { handle function results }
         { structured results are easy to handle.... }
         { needed also when result_no_used !! }
         if (p^.resulttype<>pdef(voiddef)) and ret_in_param(p^.resulttype) then
           begin
              p^.location.loc:=LOC_MEM;
              stringdispose(p^.location.reference.symbol);
              p^.location.reference:=funcretref;
           end;
         if (p^.resulttype<>pdef(voiddef)) and p^.return_value_used then
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
                          begin
                             hregister:=getregister32;
                             emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                             p^.location.register:=hregister;
                          end;
                     end;
                end
               { structed results are easy to handle.... }
              else if ret_in_param(p^.resulttype) then
                begin
                   {p^.location.loc:=LOC_MEM;
                   stringdispose(p^.location.reference.symbol);
                   p^.location.reference:=funcretref;
                   already done above (PM) }
                end
              else
                begin
                   if (p^.resulttype^.deftype=orddef) then
                     begin
                        p^.location.loc:=LOC_REGISTER;
                        case porddef(p^.resulttype)^.typ of
                          s32bit,u32bit,bool32bit :
                            begin
{$ifdef test_dest_loc}
                               if dest_loc_known and (dest_loc_tree=p) then
                                 mov_reg_to_dest(p,S_L,R_EAX)
                               else
{$endif test_dest_loc}
                                 begin
                                    hregister:=getregister32;
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    p^.location.register:=hregister;
                                 end;
                            end;
                          uchar,u8bit,bool8bit,s8bit :
                                  begin
{$ifdef test_dest_loc}
                                     if dest_loc_known and (dest_loc_tree=p) then
                                       mov_reg_to_dest(p,S_B,R_AL)
                                     else
{$endif test_dest_loc}
                                       begin
                                          hregister:=getregister32;
                                          emit_reg_reg(A_MOV,S_B,R_AL,reg32toreg8(hregister));
                                          p^.location.register:=reg32toreg8(hregister);
                                       end;
                                  end;
                                s16bit,u16bit,bool16bit :
                                  begin
{$ifdef test_dest_loc}
                                     if dest_loc_known and (dest_loc_tree=p) then
                                       mov_reg_to_dest(p,S_W,R_AX)
                                     else
{$endif test_dest_loc}
                                       begin
                                          hregister:=getregister32;
                                          emit_reg_reg(A_MOV,S_W,R_AX,reg32toreg16(hregister));
                                          p^.location.register:=reg32toreg16(hregister);
                                       end;
                                  end;
                             else internalerror(7);
                              end

                          end
                       else if (p^.resulttype^.deftype=floatdef) then
                           case pfloatdef(p^.resulttype)^.typ of
                                 f32bit : begin
                                             p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                                             if dest_loc_known and (dest_loc_tree=p) then
                                               mov_reg_to_dest(p,S_L,R_EAX)
                                             else
{$endif test_dest_loc}
                                               begin
                                                  hregister:=getregister32;
                                                  emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                                  p^.location.register:=hregister;
                                               end;
                                          end;
                                 else
                                     p^.location.loc:=LOC_FPU;
                           end
                       else
                          begin
                              p^.location.loc:=LOC_REGISTER;
{$ifdef test_dest_loc}
                              if dest_loc_known and (dest_loc_tree=p) then
                                mov_reg_to_dest(p,S_L,R_EAX)
                              else
{$endif test_dest_loc}
                                begin
                                    hregister:=getregister32;
                                    emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                                    p^.location.register:=hregister;
                                end;
                          end;
                end;
           end;

         { perhaps i/o check ? }
         if iolabel<>nil then
           begin
              exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,newcsymbol(lab2str(iolabel),0))));
              emitcall('FPC_IOCHECK',true);
           end;
         if pop_size>0 then
           exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pop_size,R_ESP)));
         { restore registers }
         popusedregisters(pushed);

         { at last, restore instance pointer (SELF) }
         if loadesi then
           maybe_loadesi;
         pp:=params;
         while assigned(pp) do
           begin
              if assigned(pp^.left) then
                if (pp^.left^.location.loc=LOC_REFERENCE) or
                  (pp^.left^.location.loc=LOC_MEM) then
                  ungetiftemp(pp^.left^.location.reference);
              pp:=pp^.right;
           end;
         if inlined then
           ungetpersistanttemp(inlinecode^.retoffset,4);
         disposetree(params);


         { from now on the result can be freed normally }
         if inlined and ret_in_param(p^.resulttype) then
           persistanttemptonormal(funcretref.offset);

         { if return value is not used }
         if (not p^.return_value_used) and (p^.resulttype<>pdef(voiddef)) then
           begin
              if p^.location.loc in [LOC_MEM,LOC_REFERENCE] then
                { release unused temp }
                ungetiftemp(p^.location.reference)
              else if p^.location.loc=LOC_FPU then
                { release FPU stack }
                exprasmlist^.concat(new(pai386,op_none(A_FDECSTP,S_NO)));
           end;
      end;


{*****************************************************************************
                             SecondProcInlineN
*****************************************************************************}

    { implementation not complete yet }

    var
      addr_correction : longint;

    procedure correct_address(p : psym);{$ifndef FPC}far;{$endif}
      begin
         if p^.typ=varsym then
           begin
             inc(pvarsym(p)^.address,addr_correction);
{$ifdef extdebug}
             Comment(V_debug,pvarsym(p)^.name+' is at offset -'
               +tostr(pvarsym(p)^.address));
             exprasmlist^.concat(new(pai_asm_comment,init(
               strpnew(pvarsym(p)^.name+' is at offset -'
               +tostr(pvarsym(p)^.address)))));
{$endif extdebug}
           end;
      end;


    procedure secondprocinline(var p : ptree);
       var st : psymtable;
           oldprocsym : pprocsym;
           para_size : longint;
           oldprocinfo : tprocinfo;
           { just dummies for genentrycode }
           nostackframe,make_global : boolean;
           proc_names : tstringcontainer;
           inlineentrycode,inlineexitcode : paasmoutput;
           oldexitlabel,oldexit2label,oldquickexitlabel:Plabel;
       begin
          oldexitlabel:=aktexitlabel;
          oldexit2label:=aktexit2label;
          oldquickexitlabel:=quickexitlabel;
          getlabel(aktexitlabel);
          getlabel(aktexit2label);
          oldprocsym:=aktprocsym;
          oldprocinfo:=procinfo;
          { set the return value }
          procinfo.retdef:=p^.inlineprocdef^.retdef;
          procinfo.retoffset:=p^.retoffset;
          { arg space has been filled by the parent secondcall }
          st:=p^.inlineprocdef^.localst;
              { set it to the same lexical level }
          st^.symtablelevel:=
            oldprocsym^.definition^.localst^.symtablelevel;
          if st^.datasize>0 then
            st^.call_offset:=gettempofsizepersistant(st^.datasize);
{$ifdef extdebug}
             Comment(V_debug,'local symtable is at offset '
               +tostr(st^.call_offset));
          exprasmlist^.concat(new(pai_asm_comment,init(
          strpnew('local symtable is at offset '
               +tostr(st^.call_offset)))));
{$endif extdebug}
          addr_correction:=-st^.call_offset-st^.datasize;
          st^.foreach(correct_address);
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init('Start of inlined proc')));
{$endif extdebug}
          { takes care of local data initialization }
          inlineentrycode:=new(paasmoutput,init);
          inlineexitcode:=new(paasmoutput,init);
          proc_names.init;
          para_size:=p^.para_size;
          make_global:=false; { to avoid warning }
          genentrycode(inlineentrycode,proc_names,make_global,0,para_size,nostackframe,true);
          exprasmlist^.concatlist(inlineentrycode);
          secondpass(p^.left);
          genexitcode(inlineexitcode,0,false,true);
          exprasmlist^.concatlist(inlineexitcode);
{$ifdef extdebug}
          exprasmlist^.concat(new(pai_asm_comment,init('End of inlined proc')));
{$endif extdebug}
          {we can free the local data now }
          if st^.datasize>0 then
            ungetpersistanttemp(st^.call_offset,st^.datasize);
          { set the real address again }
          addr_correction:=-addr_correction;
          st^.foreach(correct_address);
          aktprocsym:=oldprocsym;
          aktexitlabel:=oldexitlabel;
          aktexit2label:=oldexit2label;
          quickexitlabel:=oldquickexitlabel;
          procinfo:=oldprocinfo;
       end;



end.
{
  $Log$
  Revision 1.30  1998-09-26 15:03:02  florian
    * small problems with DOM and excpetions fixed (code generation
      of raise was wrong and self was sometimes destroyed :()

  Revision 1.29  1998/09/25 00:04:00  florian
    * problems when calling class methods fixed

  Revision 1.28  1998/09/24 14:27:37  peter
    * some better support for openarray

  Revision 1.27  1998/09/24 09:02:13  peter
    * rewritten isconvertable to use case
    * array of .. and single variable are compatible

  Revision 1.26  1998/09/21 08:45:06  pierre
    + added vmt_offset in tobjectdef.write for fututre use
      (first steps to have objects without vmt if no virtual !!)
    + added fpu_used field for tabstractprocdef  :
      sets this level to 2 if the functions return with value in FPU
      (is then set to correct value at parsing of implementation)
      THIS MIGHT refuse some code with FPU expression too complex
      that were accepted before and even in some cases
      that don't overflow in fact
      ( like if f : float; is a forward that finally in implementation
       only uses one fpu register !!)
      Nevertheless I think that it will improve security on
      FPU operations !!
    * most other changes only for UseBrowser code
      (added symtable references for record and objects)
      local switch for refs to args and local of each function
      (static symtable still missing)
      UseBrowser still not stable and probably broken by
      the definition hash array !!

  Revision 1.25  1998/09/20 12:26:35  peter
    * merged fixes

  Revision 1.24  1998/09/17 09:42:10  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.23  1998/09/14 10:43:45  peter
    * all internal RTL functions start with FPC_

  Revision 1.22.2.1  1998/09/20 12:20:06  peter
    * Fixed stack not on 4 byte boundary when doing a call

  Revision 1.22  1998/09/04 08:41:37  peter
    * updated some error CGMessages

  Revision 1.21  1998/09/01 12:47:57  peter
    * use pdef^.size instead of orddef^.typ

  Revision 1.20  1998/08/31 12:22:15  peter
    * secondinline moved to cg386inl

  Revision 1.19  1998/08/31 08:52:03  peter
    * fixed error 10 with succ() and pref()

  Revision 1.18  1998/08/20 21:36:38  peter
    * fixed 'with object do' bug

  Revision 1.17  1998/08/19 16:07:36  jonas
    * changed optimizer switches + cleanup of DestroyRefs in daopt386.pas

  Revision 1.16  1998/08/18 09:24:36  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.15  1998/08/13 11:00:09  peter
    * fixed procedure<>procedure construct

  Revision 1.14  1998/08/11 14:05:33  peter
    * fixed sizeof(array of char)

  Revision 1.13  1998/08/10 14:49:45  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.12  1998/07/30 13:30:31  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.11  1998/07/24 22:16:52  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.10  1998/07/18 22:54:23  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.9  1998/07/07 17:40:37  peter
    * packrecords 4 works
    * word aligning of parameters

  Revision 1.8  1998/07/06 15:51:15  michael
  Added length checking for string reading

  Revision 1.7  1998/07/06 14:19:51  michael
  + Added calls for reading/writing ansistrings

  Revision 1.6  1998/07/01 15:28:48  peter
    + better writeln/readln handling, now 100% like tp7

  Revision 1.5  1998/06/25 14:04:17  peter
    + internal inc/dec

  Revision 1.4  1998/06/25 08:48:06  florian
    * first version of rtti support

  Revision 1.3  1998/06/09 16:01:33  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.2  1998/06/08 13:13:29  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:10  peter
    * splitted cgi386

}

