{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for in call/inline nodes

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

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right,inlined : boolean;para_offset : longint);
    procedure secondcalln(var p : ptree);
    procedure secondinline(var p : ptree);
    procedure secondprocinline(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,systems,
      aasm,i386,types,
      cgi386,cgai386,temp_gen,tgeni386,hcodegen,
      cg386ld;


{*****************************************************************************
                             SecondCallParaN
*****************************************************************************}

    { save the size of pushed parameter }
    var
       pushedparasize : longint;

    procedure secondcallparan(var p : ptree;defcoll : pdefcoll;
                push_from_left_to_right,inlined : boolean;para_offset : longint);

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
                     r:=new_reference(highframepointer,highoffset+4);
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
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_L,
                            parraydef(p^.left^.resulttype)^.highrange-
                            parraydef(p^.left^.resulttype)^.lowrange,r)));
                       end
                     else
                  push_int(parraydef(p^.left^.resulttype)^.highrange-
                           parraydef(p^.left^.resulttype)^.lowrange);
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
                   if (p^.left^.location.loc<>LOC_REFERENCE) and
                      (p^.left^.location.loc<>LOC_MEM) then
                     Message(sym_e_type_mismatch)
                   else
                     begin
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,
                            newreference(p^.left^.location.reference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                        emitpushreferenceaddr(p^.left^.location.reference);
                        del_reference(p^.left^.location.reference);
                     end;
                end;
           end
         { handle call by reference parameter }
         else if (defcoll^.paratyp=vs_var) then
           begin
              if (p^.left^.location.loc<>LOC_REFERENCE) then
                Message(cg_e_var_must_be_reference);
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
              emitpushreferenceaddr(p^.left^.location.reference);
              del_reference(p^.left^.location.reference);
           end
         else
           begin
              tempdeftype:=p^.resulttype^.deftype;
              if tempdeftype=filedef then
               Message(cg_e_file_must_call_by_reference);
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
                     emitpushreferenceaddr(p^.left^.location.reference);
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
                           orddef :
                             begin
                                case porddef(p^.resulttype)^.typ of
                                  s32bit,u32bit,bool32bit :
                                    begin
                                       inc(pushedparasize,4);
                                       if inlined then
                                         begin
                                            exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                              newreference(tempreference),R_EDI)));
                                            r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                            exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                            R_EDI,r)));
                                         end
                                       else
                                         emit_push_mem(tempreference);
                                    end;
                                  s8bit,u8bit,uchar,bool8bit,bool16bit,s16bit,u16bit :
                                    begin
                                      inc(pushedparasize,2);
                                      if inlined then
                                        begin
                                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,
                                             newreference(tempreference),R_DI)));
                                           r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                                           exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                                           R_DI,r)));
                                        end
                                      else
                                        exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,
                                          newreference(tempreference))));
                                    end;
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
                                         exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                                           R_EDI,r)));
                                      end
                                    else
                                      emit_push_mem(tempreference);
                                              end;
                                            s64real,
                                            s64bit : begin
                                                         inc(pushedparasize,4);
                                                         inc(tempreference.offset,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
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
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                                         emit_push_mem(tempreference);
                                                      end;
                                            s80real : begin
                                                         inc(pushedparasize,4);
                                                         inc(tempreference.offset,6);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
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
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
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
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_W,
                            R_DI,r)));
                       end
                     else
                                                         exprasmlist^.concat(new(pai386,op_ref(A_PUSH,S_W,
                                                           newreference(tempreference))));
                                                      end;
                                         end;
                                      end;
                           pointerdef,procvardef,
                           enumdef,classrefdef:
                             begin
                                inc(pushedparasize,4);
                     if inlined then
                       begin
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                            newreference(tempreference),R_EDI)));
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,
                            R_EDI,r)));
                       end
                     else
                                emit_push_mem(tempreference);
                             end;
                           arraydef,recorddef,stringdef,setdef,objectdef :
                             begin
                                { small set ? }
                                if ((p^.resulttype^.deftype=setdef) and
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
                                else if (p^.resulttype^.deftype=arraydef) and
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
                                     emitpushreferenceaddr(p^.left^.location.reference);
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
                           else Message(cg_e_illegal_expression);
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
                          exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_W,
                            1,r)));
                       end
                     else
                        exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_W,1)));
                        emitl(A_JMP,hlabel);
                        emitl(A_LABEL,falselabel);
                     if inlined then
                       begin
                          r:=new_reference(procinfo.framepointer,para_offset-pushedparasize);
                          exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_W,
                            0,r)));
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
         corr : pai386;
         { we must pop this size also after !! }
         must_pop : boolean;
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
              if ((p^.procdefinition^.options and poiocheck)<>0)
                and (cs_iocheck in aktswitches) then
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
              pushusedregisters(pushed,$ff);
              usedinproc:=$ff;
              { no IO check for methods and procedure variables }
              iolabel:=nil;
           end;

         { generate the code for the parameter and push them }
         oldpushedparasize:=pushedparasize;
         pushedparasize:=0;
         corr:=new(pai386,op_const_reg(A_SUB,S_L,0,R_ESP));
         exprasmlist^.concat(corr);
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
              pushedparasize:=0;
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
                emitpushreferenceaddr(funcretref);
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
                                         Message(cg_e_cant_call_abstract_method);
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

                                          Message(cg_w_member_cd_call_from_method);
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
                                            LOC_REGISTER:
                                              begin
                                                 ungetregister32(p^.methodpointer^.location.register);
                                                 emit_reg_reg(A_MOV,S_L,p^.methodpointer^.location.register,R_ESI);
                                              end;
                                            else
                                              begin
                                                 if (p^.methodpointer^.resulttype^.deftype=objectdef) and
                                                   pobjectdef(p^.methodpointer^.resulttype)^.isclass then
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
                                       and not(p^.methodpointer^.treetype=selfn) then
                                      begin
                                         { class method needs current VMT }
                                         new(r);
                                         reset_reference(r^);
                                         r^.base:=R_ESI;
                                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,R_ESI)));
                                      end;

                                    { direct call to class constructor, don't allocate memory }
                                    if is_con_or_destructor and (p^.methodpointer^.resulttype^.deftype=objectdef) and
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
                             Message(cg_w_member_cd_call_from_method);
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

              { exported methods should be never called direct.
                Why? Bp7 Allows it (PFV)

              if (p^.procdefinition^.options and poexports)<>0 then
                Message(cg_e_dont_call_exported_direct);  }

              if (not inlined) and ((pushedparasize mod 4)<>0) then
                begin
                   corr^.op1:=pointer(4-(pushedparasize mod 4));
                   must_pop:=true;
                   pop_size:=4-(pushedparasize mod 4);
                end
              else
                begin
                   exprasmlist^.remove(corr);
                   must_pop:=false;
                   pop_size:=0;
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
                   if (cs_rangechecking in aktswitches) then
                     begin
                        exprasmlist^.concat(new(pai386,op_reg(A_PUSH,S_L,r^.base)));
                        emitcall('CHECK_OBJECT',true);
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
              if (not inlined) and ((p^.procdefinition^.options and poclearstack)<>0) then
                begin
                   { consider the alignment with the rest (PM) }
                   pushedparasize:=pushedparasize+pop_size;
                   must_pop:=false;
                   if pushedparasize=4 then
                     { better than an add on all processors }
                     exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)))
                   { the pentium has two pipes and pop reg is pairable }
                   { but the registers must be different!              }
                   else if (pushedparasize=8) and
                     not(cs_littlesize in aktswitches) and
                     (aktoptprocessor=pentium) and
                     (procinfo._class=nil) then
                       begin
                          exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_EDI)));
                          exprasmlist^.concat(new(pai386,op_reg(A_POP,S_L,R_ESI)));
                       end
                   else exprasmlist^.concat(new(pai386,op_const_reg(A_ADD,S_L,pushedparasize,R_ESP)));
                end;
           end
         else
           begin
              if (pushedparasize mod 4)<>0 then
                begin
                   corr^.op1:=pointer(4-(pushedparasize mod 4));
                   must_pop:=true;
                   pop_size:=4-(pushedparasize mod 4);
                end
              else
                begin
                   exprasmlist^.remove(corr);
                   must_pop:=false;
                   pop_size:=0;
                end;
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
              { this was wrong, probably an error due to diff3
                emitcall(p^.procdefinition^.mangledname);}
              emitcall('IOCHECK',true);
           end;
         { this should be optimized (PM) }
         if must_pop then
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

    { reverts the parameter list }
    var nb_para : integer;

    function reversparameter(p : ptree) : ptree;

       var
         hp1,hp2 : ptree;

      begin
         hp1:=nil;
         nb_para := 0;
         while assigned(p) do
           begin
              { pull out }
              hp2:=p;
              p:=p^.right;
              inc(nb_para);
              { pull in }
              hp2^.right:=hp1;
              hp1:=hp2;
           end;
         reversparameter:=hp1;
       end;


{*****************************************************************************
                             SecondInLine
*****************************************************************************}

    procedure secondinline(var p : ptree);
     const     in2size:array[in_inc_byte..in_dec_dword] of Topsize=
                         (S_B,S_W,S_L,S_B,S_W,S_L);
               in2instr:array[in_inc_byte..in_dec_dword] of Tasmop=
                         (A_INC,A_INC,A_INC,A_DEC,A_DEC,A_DEC);
               ad2instr:array[in_inc_byte..in_dec_dword] of Tasmop=
                         (A_ADD,A_ADD,A_ADD,A_SUB,A_SUB,A_SUB);
            { tfloattype = (f32bit,s32real,s64real,s80real,s64bit); }
            float_name: array[tfloattype] of string[8]=
                ('FIXED','SINGLE','REAL','EXTENDED','COMP','FIXED16');
       var
         aktfile : treference;
         ft : tfiletype;
         opsize : topsize;
         asmop : tasmop;
         pushed : tpushed;

      { produces code for READ(LN) and WRITE(LN) }

      procedure handlereadwrite(doread,callwriteln : boolean);

        procedure loadstream;
          const
            io:array[0..1] of string[7]=('_OUTPUT','_INPUT');
          var
            r : preference;
          begin
            new(r);
            reset_reference(r^);
            r^.symbol:=stringdup('U_'+upper(target_info.system_unit)+io[byte(doread)]);
{           if not (cs_compilesystem in aktswitches) then }
              concat_external(r^.symbol^,EXT_NEAR);
            exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,r,R_EDI)))
          end;

        var
           node,hp : ptree;
           typedtyp,pararesult : pdef;
           doflush,has_length : boolean;
           dummycoll : tdefcoll;
           iolabel : plabel;
           npara : longint;

        begin
           { I/O check }
           if cs_iocheck in aktswitches then
                begin
                getlabel(iolabel);
                emitl(A_LABEL,iolabel);
             end
           else iolabel:=nil;
           { no automatic call from flush }
           doflush:=false;
           { for write of real with the length specified }
           has_length:=false;
           hp:=nil;
           { reserve temporary pointer to data variable }
             aktfile.symbol:=nil;
           gettempofsizereference(4,aktfile);
           { first state text data }
           ft:=ft_text;
           { and state a parameter ? }
           if p^.left=nil then
             begin
                { state screen address}
                doflush:=true;
                { the following instructions are for "writeln;" }
                loadstream;
                { save @Dateivarible in temporary variable }
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile))));
             end
           else
             begin
                { revers paramters }
                node:=reversparameter(p^.left);

                p^.left := node;
                npara := nb_para;
                { calculate data variable }
                { is first parameter a file type ? }
                if node^.left^.resulttype^.deftype=filedef then
                  begin
                     ft:=pfiledef(node^.left^.resulttype)^.filetype;
                     if ft=ft_typed then
                       typedtyp:=pfiledef(node^.left^.resulttype)^.typed_as;
                     secondpass(node^.left);
                     if codegenerror then
                       exit;

                     { save reference in temporary variables }                     { reference in tempor„re Variable retten }
                     if node^.left^.location.loc<>LOC_REFERENCE then
                       begin
                          Message(cg_e_illegal_expression);
                          exit;
                       end;

                     exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(node^.left^.location.reference),R_EDI)));

                     { skip to the next parameter }
                     node:=node^.right;
                  end
                else
                  begin
                     { if we write to stdout/in then flush after the write(ln) }
                     doflush:=true;
                     loadstream;
                  end;

                    { save @Dateivarible in temporary variable }
                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,S_L,R_EDI,newreference(aktfile))));
                if doread then
                  { parameter by READ gives call by reference }
                  dummycoll.paratyp:=vs_var
                { an WRITE Call by "Const" }
                else dummycoll.paratyp:=vs_const;

                { because of secondcallparan, which otherwise attaches }
                if ft=ft_typed then
                  begin
                     { this is to avoid copy of simple const parameters }
                     dummycoll.data:=new(pformaldef,init);
                  end
                else
                  { I think, this isn't a good solution (FK) }
                  dummycoll.data:=nil;

                while assigned(node) do
                  begin
                     pushusedregisters(pushed,$ff);
                     hp:=node;
                     node:=node^.right;
                     hp^.right:=nil;
                     if hp^.is_colon_para then
                       Message(parser_e_illegal_colon_qualifier);
                     if ft=ft_typed then
                       never_copy_const_param:=true;
                     secondcallparan(hp,@dummycoll,false
                       ,false,0
                     );
                     if ft=ft_typed then
                       never_copy_const_param:=false;
                     hp^.right:=node;
                          if codegenerror then
                       exit;

                     emit_push_mem(aktfile);
                     if (ft=ft_typed) then
                       begin
                          { OK let's try this }
                          { first we must only allow the right type }
                            { we have to call blockread or blockwrite }
                                   { but the real problem is that            }
                            { reset and rewrite should have set       }
                            { the type size                           }
                                   { as recordsize for that file !!!!        }
                            { how can we make that                    }
                            { I think that is only possible by adding }
                            { reset and rewrite to the inline list a call        }
                                   { allways read only one record by element }
                            push_int(typedtyp^.size);
                            if doread then
                              emitcall('TYPED_READ',true)
                            else
                              emitcall('TYPED_WRITE',true)
                          {!!!!!!!}
                       end
                     else
                       begin
                          { save current position }
                          pararesult:=hp^.left^.resulttype;
                          { handle possible field width  }
                          { of course only for write(ln) }
                          if not doread then
                               begin
                               { handle total width parameter }
                               if assigned(node) and node^.is_colon_para then
                                 begin
                                    hp:=node;
                                    node:=node^.right;
                                    hp^.right:=nil;
                                    secondcallparan(hp,@dummycoll,false
                                      ,false,0
                                    );
                                    hp^.right:=node;
                                    if codegenerror then
                                      exit;
                                    has_length:=true;
                                 end
                               else
                                 if pararesult^.deftype<>floatdef then
                                   push_int(0)
                                 else
                                  push_int(-32767);
                              { a second colon para for a float ? }
                              if assigned(node) and node^.is_colon_para then
                                begin
                                    hp:=node;
                                    node:=node^.right;
                                    hp^.right:=nil;
                                    secondcallparan(hp,@dummycoll,false
                                      ,false,0
                                    );
                                    hp^.right:=node;
                                    if pararesult^.deftype<>floatdef then
                                      Message(parser_e_illegal_colon_qualifier);
                                    if codegenerror then
                                      exit;
                                end
                              else
                                begin
                                if pararesult^.deftype=floatdef then
                                    push_int(-1);
                                end
                              end;
                          case pararesult^.deftype of
                             stringdef:
                               begin
                                  if doread then
                                    emitcall('READ_TEXT_STRING',true)
                                  else
                                    begin
                                      emitcall('WRITE_TEXT_STRING',true);
                                      {ungetiftemp(hp^.left^.location.reference);}
                                    end;
                               end;
                                    pointerdef : begin
                                                        if is_equal(ppointerdef(pararesult)^.definition,cchardef) then
                                                          begin
                                                              if doread then
                                                                 emitcall('READ_TEXT_PCHAR_AS_POINTER',true)
                                                              else
                                                                 emitcall('WRITE_TEXT_PCHAR_AS_POINTER',true);
                                                          end
                                                        else
                                                         Message(parser_e_illegal_parameter_list);
                                                    end;
                                    arraydef : begin
                                                     if (parraydef(pararesult)^.lowrange=0)
                                                        and is_equal(parraydef(pararesult)^.definition,cchardef) then
                                                        begin
                                                            if doread then
                                                                 emitcall('READ_TEXT_PCHAR_AS_ARRAY',true)
                                                            else
                                                                 emitcall('WRITE_TEXT_PCHAR_AS_ARRAY',true);
                                                        end
                                                     else
                                                      Message(parser_e_illegal_parameter_list);
                                                  end;

                             floatdef:
                               begin
                                  if doread then
                                    emitcall('READ_TEXT_'+float_name[pfloatdef(pararesult)^.typ],true)
                                  else
                                    emitcall('WRITE_TEXT_'+float_name[pfloatdef(pararesult)^.typ],true);
                               end;
                                    orddef : begin
                                                     case porddef(pararesult)^.typ of
                                                         u8bit : if doread then
                                                                       emitcall('READ_TEXT_BYTE',true);
                                                         s8bit : if doread then
                                                                       emitcall('READ_TEXT_SHORTINT',true);
                                                         u16bit : if doread then
                                                                       emitcall('READ_TEXT_WORD',true);
                                                         s16bit : if doread then
                                                                       emitcall('READ_TEXT_INTEGER',true);
                                                         s32bit : if doread then
                                                                       emitcall('READ_TEXT_LONGINT',true)
                                                                    else
                                                                       emitcall('WRITE_TEXT_LONGINT',true);
                                                         u32bit : if doread then
                                                                       emitcall('READ_TEXT_CARDINAL',true)
                                                                    else
                                                                       emitcall('WRITE_TEXT_CARDINAL',true);
                                                         uchar : if doread then
                                                                       emitcall('READ_TEXT_CHAR',true)
                                                                    else
                                                                       emitcall('WRITE_TEXT_CHAR',true);
                                                         bool8bit,
                                                         bool16bit,
                                                         bool32bit : if  doread then
                                                                       { emitcall('READ_TEXT_BOOLEAN',true) }
                                                                       Message(parser_e_illegal_parameter_list)
                                                                    else
                                                                       emitcall('WRITE_TEXT_BOOLEAN',true);
                                                         else Message(parser_e_illegal_parameter_list);
                                                         end;
                                                     end;
                                    else Message(parser_e_illegal_parameter_list);
                                end;
                            end;
                          { load ESI in methods again }
                          popusedregisters(pushed);
                          maybe_loadesi;
                  end;
             end;
           if callwriteln then
             begin
                pushusedregisters(pushed,$ff);
                emit_push_mem(aktfile);
                { pushexceptlabel; }
                if ft<>ft_text then
                  Message(parser_e_illegal_parameter_list)                                    ;
                emitcall('WRITELN_TEXT',true);
                popusedregisters(pushed);
                maybe_loadesi;
             end;
           if doflush and not(doread) then
             begin
                pushusedregisters(pushed,$ff);
                { pushexceptlabel; }
                emitcall('FLUSH_STDOUT',true);
                popusedregisters(pushed);
                maybe_loadesi;
             end;
           if iolabel<>nil then
             begin
                { registers are saved in the procedure }
                exprasmlist^.concat(new(pai386,op_csymbol(A_PUSH,S_L,newcsymbol(lab2str(iolabel),0))));
                emitcall('IOCHECK',true);
             end;
           ungetiftemp(aktfile);
           if assigned(p^.left) then
             begin
                p^.left:=reversparameter(p^.left);
                    if npara<>nb_para then
                     Message(cg_f_internal_error_in_secondinline);
                    hp:=p^.left;
                    while assigned(hp) do
                  begin
                     if assigned(hp^.left) then
                       if (hp^.left^.location.loc=LOC_REFERENCE) or
                         (hp^.left^.location.loc=LOC_MEM) then
                         ungetiftemp(hp^.left^.location.reference);
                     hp:=hp^.right;
                  end;
            end;
        end;

      procedure handle_str;

        var
           hp,node : ptree;
           dummycoll : tdefcoll;
           is_real,has_length : boolean;

          begin
           pushusedregisters(pushed,$ff);
           node:=p^.left;
           is_real:=false;
           has_length:=false;
           while assigned(node^.right) do node:=node^.right;
           { if a real parameter somewhere then call REALSTR }
           if (node^.left^.resulttype^.deftype=floatdef) then
             is_real:=true;

           node:=p^.left;
           { we have at least two args }
           { with at max 2 colon_para in between }

           { first arg longint or float }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           dummycoll.data:=hp^.resulttype;
           { string arg }

           dummycoll.paratyp:=vs_var;
           secondcallparan(hp,@dummycoll,false
             ,false,0
             );
           if codegenerror then
             exit;

           dummycoll.paratyp:=vs_const;
           { second arg }
           hp:=node;
           node:=node^.right;
           hp^.right:=nil;
           { frac  para }
           if hp^.is_colon_para and assigned(node) and
              node^.is_colon_para then
             begin
                dummycoll.data:=hp^.resulttype;
                secondcallparan(hp,@dummycoll,false
                  ,false,0
                  );
                if codegenerror then
                  exit;
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
                has_length:=true;
             end
           else
             if is_real then
             push_int(-1);

           { third arg, length only if is_real }
           if hp^.is_colon_para then
             begin
                dummycoll.data:=hp^.resulttype;
                secondcallparan(hp,@dummycoll,false
                  ,false,0
                  );
                if codegenerror then
                  exit;
                hp:=node;
                node:=node^.right;
                hp^.right:=nil;
             end
           else
             if is_real then
               push_int(-32767)
             else
               push_int(-1);

           { last arg longint or real }
           secondcallparan(hp,@dummycoll,false
             ,false,0
             );
           if codegenerror then
             exit;

           if is_real then
             emitcall('STR_'+float_name[pfloatdef(hp^.resulttype)^.typ],true)
           else if porddef(hp^.resulttype)^.typ=u32bit then
             emitcall('STR_CARDINAL',true)
           else
             emitcall('STR_LONGINT',true);
           popusedregisters(pushed);
        end;

      var
         r : preference;
         l : longint;
         ispushed : boolean;
         hregister : tregister;

      begin
         case p^.inlinenumber of
            in_lo_word,
            in_hi_word :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                     if p^.left^.location.loc=LOC_CREGISTER then
                       begin
                          p^.location.register:=reg32toreg16(getregister32);
                          emit_reg_reg(A_MOV,S_W,p^.left^.location.register,
                            p^.location.register);
                       end
                     else
                       begin
                          del_reference(p^.left^.location.reference);
                          p^.location.register:=reg32toreg16(getregister32);
                          exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_W,newreference(p^.left^.location.reference),
                            p^.location.register)));
                       end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_word then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_W,8,p^.location.register)));
                 p^.location.register:=reg16toreg8(p^.location.register);
              end;
            in_high_x :
              begin
                 if is_open_array(p^.left^.resulttype) then
                   begin
                      secondpass(p^.left);
                      del_reference(p^.left^.location.reference);
                      p^.location.register:=getregister32;
                      new(r);
                      reset_reference(r^);
                      r^.base:=highframepointer;
                      r^.offset:=highoffset+4;
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                        r,p^.location.register)));
                   end
              end;
            in_sizeof_x,
            in_typeof_x :
              begin
                 { for both cases load vmt }
                 if p^.left^.treetype=typen then
                   begin
                      p^.location.register:=getregister32;
                      exprasmlist^.concat(new(pai386,op_csymbol_reg(A_MOV,
                        S_L,newcsymbol(pobjectdef(p^.left^.resulttype)^.vmt_mangledname,0),
                        p^.location.register)));
                   end
                 else
                   begin
                      secondpass(p^.left);
                      del_reference(p^.left^.location.reference);
                      p^.location.loc:=LOC_REGISTER;
                      p^.location.register:=getregister32;
                      { load VMT pointer }
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                      newreference(p^.left^.location.reference),
                        p^.location.register)));
                   end;
                 { in sizeof load size }
                 if p^.inlinenumber=in_sizeof_x then
                   begin
                      new(r);
                      reset_reference(r^);
                      r^.base:=p^.location.register;
                      exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,r,
                        p^.location.register)));
                   end;
              end;
            in_lo_long,
            in_hi_long :
              begin
                 secondpass(p^.left);
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      if p^.left^.location.loc=LOC_CREGISTER then
                        begin
                           p^.location.register:=getregister32;
                           emit_reg_reg(A_MOV,S_L,p^.left^.location.register,
                             p^.location.register);
                        end
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           p^.location.register:=getregister32;
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 if p^.inlinenumber=in_hi_long then
                   exprasmlist^.concat(new(pai386,op_const_reg(A_SHR,S_L,16,p^.location.register)));
                 p^.location.register:=reg32toreg16(p^.location.register);
              end;
{***CHARBUG}
{We can now comment them out, as they are handled as typecast.
 Saves an incredible amount of 8 bytes code.
 I'am not lucky about this, because it's _not_ a type cast (FK) }
{              in_ord_char,
               in_chr_byte,}
{***}
            in_length_string :
              begin
                 secondpass(p^.left);
                 set_location(p^.location,p^.left^.location);
                 { length in ansi strings is at offset -8 }
{$ifdef UseAnsiString}
                 if is_ansistring(p^.left^.resulttype) then
                   dec(p^.location.reference.offset,8);
{$endif UseAnsiString}
              end;
            in_pred_x,
            in_succ_x:
              begin
                 secondpass(p^.left);
                 if p^.inlinenumber=in_pred_x then
                   asmop:=A_DEC
                 else
                   asmop:=A_INC;
                 case p^.resulttype^.size of
                   4 : opsize:=S_L;
                   2 : opsize:=S_W;
                   1 : opsize:=S_B;
                 else
                   internalerror(10080);
                 end;
                 p^.location.loc:=LOC_REGISTER;
                 if p^.left^.location.loc<>LOC_REGISTER then
                   begin
                      p^.location.register:=getregister32;
                      if (p^.resulttype^.size=2) then
                        p^.location.register:=reg32toreg16(p^.location.register);
                      if (p^.resulttype^.size=1) then
                        p^.location.register:=reg32toreg8(p^.location.register);
                      if p^.left^.location.loc=LOC_CREGISTER then
                        emit_reg_reg(A_MOV,opsize,p^.left^.location.register,
                          p^.location.register)
                      else
                      if p^.left^.location.loc=LOC_FLAGS then
                        exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.left^.location.resflags],S_B,
                                  p^.location.register)))
                      else
                        begin
                           del_reference(p^.left^.location.reference);
                           exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,newreference(p^.left^.location.reference),
                             p^.location.register)));
                        end;
                   end
                 else p^.location.register:=p^.left^.location.register;
                 exprasmlist^.concat(new(pai386,op_reg(asmop,opsize,
                   p^.location.register)))
                 { here we should insert bounds check ? }
                 { and direct call to bounds will crash the program }
                 { if we are at the limit }
                 { we could also simply say that pred(first)=first and succ(last)=last }
                 { could this be usefull I don't think so (PM)
                 emitoverflowcheck;}
              end;
            in_inc_byte..in_dec_dword:
              begin
                 secondpass(p^.left);
                 if cs_check_overflow in aktswitches then
                   begin
                   { SINCE THE CARRY FLAG IS NEVER SET BY DEC/INC, we must use  }
                   { ADD and SUB to check for overflow for unsigned operations. }
                     exprasmlist^.concat(new(pai386,op_const_ref(ad2instr[p^.inlinenumber],
                       in2size[p^.inlinenumber],1,newreference(p^.left^.location.reference))));
                     emitoverflowcheck(p^.left);
                   end
                 else
                 exprasmlist^.concat(new(pai386,op_ref(in2instr[p^.inlinenumber],
                   in2size[p^.inlinenumber],newreference(p^.left^.location.reference))));
              end;
            in_assigned_x :
              begin
                 secondpass(p^.left^.left);
                 p^.location.loc:=LOC_FLAGS;
                 if (p^.left^.left^.location.loc=LOC_REGISTER) or
                    (p^.left^.left^.location.loc=LOC_CREGISTER) then
                   begin
                      exprasmlist^.concat(new(pai386,op_reg_reg(A_OR,S_L,
                        p^.left^.left^.location.register,
                        p^.left^.left^.location.register)));
                      ungetregister32(p^.left^.left^.location.register);
                   end
                 else
                   begin
                      exprasmlist^.concat(new(pai386,op_const_ref(A_CMP,S_L,0,
                        newreference(p^.left^.left^.location.reference))));
                      del_reference(p^.left^.left^.location.reference);
                   end;
                 p^.location.resflags:=F_NE;
              end;
             in_reset_typedfile,in_rewrite_typedfile :
               begin
                  pushusedregisters(pushed,$ff);
                  exprasmlist^.concat(new(pai386,op_const(A_PUSH,S_L,pfiledef(p^.left^.resulttype)^.typed_as^.size)));
                  secondload(p^.left);
                  emitpushreferenceaddr(p^.left^.location.reference);
                  if p^.inlinenumber=in_reset_typedfile then
                    emitcall('RESET_TYPED',true)
                  else
                    emitcall('REWRITE_TYPED',true);
                  popusedregisters(pushed);
               end;
            in_write_x :
              handlereadwrite(false,false);
            in_writeln_x :
              handlereadwrite(false,true);
            in_read_x :
              handlereadwrite(true,false);
            in_readln_x :
              begin
                handlereadwrite(true,false);
                pushusedregisters(pushed,$ff);
                emit_push_mem(aktfile);
                { pushexceptlabel; }
                if ft<>ft_text then
                  Message(parser_e_illegal_parameter_list);
                emitcall('READLN_TEXT',true);
                popusedregisters(pushed);
                maybe_loadesi;
              end;
            in_str_x_string :
              begin
                 handle_str;
                 maybe_loadesi;
              end;
            in_include_x_y,
            in_exclude_x_y:
              begin
                 secondpass(p^.left^.left);
                 if p^.left^.right^.left^.treetype=ordconstn then
                   begin
                      { calculate bit position }
                      l:=1 shl (p^.left^.right^.left^.value mod 32);

                      { determine operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_OR
                      else
                        begin
                           asmop:=A_AND;
                           l:=not(l);
                        end;
                      if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                        begin
                           inc(p^.left^.left^.location.reference.offset,(p^.left^.right^.left^.value div 32)*4);
                           exprasmlist^.concat(new(pai386,op_const_ref(asmop,S_L,
                             l,newreference(p^.left^.left^.location.reference))));
                           del_reference(p^.left^.left^.location.reference);
                        end
                      else
                        { LOC_CREGISTER }
                        exprasmlist^.concat(new(pai386,op_const_reg(asmop,S_L,
                          l,p^.left^.left^.location.register)));
                   end
                 else
                   begin
                      { generate code for the element to set }
                      ispushed:=maybe_push(p^.left^.right^.left^.registers32,p^.left^.left);
                      secondpass(p^.left^.right^.left);
                      if ispushed then
                        restore(p^.left^.left);
                      { determine asm operator }
                      if p^.inlinenumber=in_include_x_y then
                        asmop:=A_BTS
                      else
                        asmop:=A_BTR;
                      if psetdef(p^.left^.resulttype)^.settype=smallset then
                        begin
                           if p^.left^.right^.left^.location.loc in
                             [LOC_CREGISTER,LOC_REGISTER] then
                             hregister:=p^.left^.right^.left^.location.register
                           else
                             begin
                                hregister:=R_EDI;
                                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                  newreference(p^.left^.right^.left^.location.reference),
                                  R_EDI)));
                             end;
                          if (p^.left^.left^.location.loc=LOC_REFERENCE) then
                            exprasmlist^.concat(new(pai386,op_reg_ref(asmop,S_L,R_EDI,
                              newreference(p^.left^.right^.left^.location.reference))))
                          else
                            exprasmlist^.concat(new(pai386,op_reg_reg(asmop,S_L,R_EDI,
                              p^.left^.right^.left^.location.register)));
                        end
                      else
                        begin
                        end;
                   end;
              end;
            else internalerror(9);
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
  Revision 1.2  1998-06-08 13:13:29  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:10  peter
    * splitted cgi386

}

