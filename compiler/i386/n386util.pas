{
    $Id$
    Copyright (c) 1998-2000 by Florian Klaempfl

    Helper routines for the i386 code generator

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
unit n386util;

{$i defines.inc}

interface

    uses
      symtype,node;

    type
      tloadregvars = (lr_dont_load_regvars, lr_load_regvars);

    function maybe_push(needed : byte;p : tnode;isint64 : boolean) : boolean;
    function maybe_pushfpu(needed : byte;p : tnode) : boolean;
{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;
{$endif TEMPS_NOT_PUSH}
    procedure restore(p : tnode;isint64 : boolean);
{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : tnode;isint64 : boolean);
{$endif TEMPS_NOT_PUSH}
    procedure pushsetelement(p : tnode);
    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                              para_offset:longint;alignment : longint);
    procedure loadshortstring(source,dest : tnode);
    procedure loadlongstring(p:tbinarynode);
    procedure loadansi2short(source,dest : tnode);
    procedure loadwide2short(source,dest : tnode);
    procedure loadinterfacecom(p: tbinarynode);

    procedure maketojumpbool(p : tnode; loadregvars: tloadregvars);
    procedure emitoverflowcheck(p:tnode);
    procedure emitrangecheck(p:tnode;todef:tdef);
    procedure firstcomplex(p : tbinarynode);

implementation

    uses
       globtype,globals,systems,verbose,
       cutils,
       aasm,cpubase,cpuasm,cpuinfo,
       symconst,symbase,symdef,symsym,symtable,
{$ifdef GDB}
       gdb,
{$endif GDB}
       types,
       ncon,nld,
       pass_1,pass_2,
       cgbase,tgcpu,temp_gen,
       cga,regvars;


{*****************************************************************************
                           Emit Push Functions
*****************************************************************************}

    function maybe_push(needed : byte;p : tnode;isint64 : boolean) : boolean;
      var
         pushed : boolean;
         {hregister : tregister; }
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         if p.location.loc = LOC_CREGISTER then
           begin
             maybe_push := true;
             exit;
           end;
         if needed>usablereg32 then
           begin
              if (p.location.loc=LOC_REGISTER) then
                begin
                   if isint64 then
                     begin
{$ifdef TEMPS_NOT_PUSH}
                        gettempofsizereference(href,8);
                        p.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmList.concat(Taicpu.Op_reg(A_MOV,S_L,p.location.registerhigh,href));
                        href.offset:=href.offset-4;
{$else TEMPS_NOT_PUSH}
                        exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.location.registerhigh));
{$endif TEMPS_NOT_PUSH}
                        ungetregister32(p.location.registerhigh);
                     end
{$ifdef TEMPS_NOT_PUSH}
                   else
                     begin
                        gettempofsizereference(href,4);
                        p.temp_offset:=href.offset;
                     end
{$endif TEMPS_NOT_PUSH}
                     ;
                   pushed:=true;
{$ifdef TEMPS_NOT_PUSH}
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,p.location.register,href));
{$else TEMPS_NOT_PUSH}
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.location.register));
{$endif TEMPS_NOT_PUSH}
                   ungetregister32(p.location.register);
                end
              else if (p.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p.location.reference.base<>R_NO) or
                       (p.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p.location.reference);
                     getexplicitregister32(R_EDI);
                     emit_ref_reg(A_LEA,S_L,newreference(p.location.reference),R_EDI);
{$ifdef TEMPS_NOT_PUSH}
                     gettempofsizereference(href,4);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,href));
                     p.temp_offset:=href.offset;
{$else TEMPS_NOT_PUSH}
                     exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,R_EDI));
{$endif TEMPS_NOT_PUSH}
                     ungetregister32(R_EDI);
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;


     function maybe_pushfpu(needed : byte;p : tnode) : boolean;
       begin
         if needed>=maxfpuregs then
           begin
             if p.location.loc = LOC_FPU then
               begin
                 emit_to_mem(p.location,p.resulttype.def);
                 maybe_pushfpu:=true;
               end
             else
               maybe_pushfpu:=false;
           end
         else
           maybe_pushfpu:=false;
       end;


{$ifdef TEMPS_NOT_PUSH}
    function maybe_savetotemp(needed : byte;p : tnode;isint64 : boolean) : boolean;

      var
         pushed : boolean;
         href : treference;

      begin
         if needed>usablereg32 then
           begin
              if (p^.location.loc=LOC_REGISTER) then
                begin
                   if isint64(p^.resulttype.def) then
                     begin
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmList.concat(Taicpu.Op_reg(A_MOV,S_L,p^.location.registerhigh,href));
                        href.offset:=href.offset-4;
                        ungetregister32(p^.location.registerhigh);
                     end
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end;
                   pushed:=true;
                   exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,p^.location.register,href));
                   ungetregister32(p^.location.register);
                end
              else if (p^.location.loc in [LOC_MEM,LOC_REFERENCE]) and
                      ((p^.location.reference.base<>R_NO) or
                       (p^.location.reference.index<>R_NO)
                      ) then
                  begin
                     del_reference(p^.location.reference);
                     getexplicitregister32(R_EDI);
                     emit_ref_reg(A_LEA,S_L,newreference(p^.location.reference),
                       R_EDI);
                     gettempofsizereference(href,4);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,href));
                     ungetregister32(R_EDI);
                     p^.temp_offset:=href.offset;
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
      end;
{$endif TEMPS_NOT_PUSH}


    procedure restore(p : tnode;isint64 : boolean);
      var
         hregister :  tregister;
{$ifdef TEMPS_NOT_PUSH}
         href : treference;
{$endif TEMPS_NOT_PUSH}
      begin
         if p.location.loc = LOC_CREGISTER then
           begin
             load_regvar_reg(exprasmlist,p.location.register);
             exit;
           end;
         hregister:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
{$else  TEMPS_NOT_PUSH}
         exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,hregister));
{$endif TEMPS_NOT_PUSH}
         if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p.location.register:=hregister;
              if isint64 then
                begin
                   p.location.registerhigh:=getregister32;
{$ifdef TEMPS_NOT_PUSH}
                   href.offset:=p.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p.temp_offset;
{$else  TEMPS_NOT_PUSH}
                   exprasmList.concat(Taicpu.Op_reg(A_POP,S_L,p.location.registerhigh));
{$endif TEMPS_NOT_PUSH}
                end;
           end
         else
           begin
              reset_reference(p.location.reference);
              { any reasons why this was moved into the index register ? }
              { normally usage of base register is much better (FK)      }
              p.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p.left
                because otherwise secondload fails !!!
              set_location(p.left^.location,p.location);}
           end;
{$ifdef TEMPS_NOT_PUSH}
         ungetiftemp(href);
{$endif TEMPS_NOT_PUSH}
      end;


{$ifdef TEMPS_NOT_PUSH}
    procedure restorefromtemp(p : tnode;isint64 : boolean);
      var
         hregister :  tregister;
         href : treference;

      begin
         hregister:=getregister32;
         reset_reference(href);
         href.base:=procinfo^.frame_pointer;
         href.offset:=p.temp_offset;
         emit_ref_reg(A_MOV,S_L,href,hregister);
         if (p.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
           begin
              p.location.register:=hregister;
              if isint64 then
                begin
                   p.location.registerhigh:=getregister32;
                   href.offset:=p.temp_offset+4;
                   emit_ref_reg(A_MOV,S_L,p.location.registerhigh);
                   { set correctly for release ! }
                   href.offset:=p.temp_offset;
                end;
           end
         else
           begin
              reset_reference(p.location.reference);
              p.location.reference.base:=hregister;
              { Why is this done? We can never be sure about p^.left
                because otherwise secondload fails PM
              set_location(p^.left^.location,p^.location);}
           end;
         ungetiftemp(href);
      end;
{$endif TEMPS_NOT_PUSH}


    procedure pushsetelement(p : tnode);
      var
         hr,hr16,hr32 : tregister;
      begin
      { copy the element on the stack, slightly complicated }
        if p.nodetype=ordconstn then
         begin
           if aktalignment.paraalign=4 then
             exprasmList.concat(Taicpu.Op_const(A_PUSH,S_L,tordconstnode(p).value))
           else
             exprasmList.concat(Taicpu.Op_const(A_PUSH,S_W,tordconstnode(p).value));
         end
        else
         begin
           case p.location.loc of
             LOC_REGISTER,
             LOC_CREGISTER :
               begin
                 hr:=p.location.register;
                 case hr of
                   R_EAX,R_EBX,R_ECX,R_EDX,R_EDI,R_ESI,R_ESP :
                     begin
                       hr16:=reg32toreg16(hr);
                       hr32:=hr;
                     end;
                   R_AX,R_BX,R_CX,R_DX,R_DI,R_SI,R_SP :
                     begin
                       hr16:=hr;
                       hr32:=reg16toreg32(hr);
                     end;
                   R_AL,R_BL,R_CL,R_DL :
                     begin
                       hr16:=reg8toreg16(hr);
                       hr32:=reg8toreg32(hr);
                     end;
                 end;
                 if aktalignment.paraalign=4 then
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,hr32))
                 else
                   exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_W,hr16));
                 ungetregister32(hr32);
               end;
           else
             begin
               { you can't push more bytes than the size of the element, }
               { because this may cross a page boundary and you'll get a }
               { sigsegv (JM)                                            }
               emit_push_mem_size(p.location.reference,1);
               del_reference(p.location.reference);
             end;
           end;
         end;
      end;

    procedure push_value_para(p:tnode;inlined,is_cdecl:boolean;
                                para_offset:longint;alignment : longint);
      var
        tempreference : treference;
        r : preference;
        opsize : topsize;
        op : tasmop;
        hreg : tregister;
        size : longint;
        hlabel : tasmlabel;
      begin
        case p.location.loc of
           LOC_REGISTER,
           LOC_CREGISTER:
             begin
                  if p.resulttype.def.size=8 then
                    begin
                       inc(pushedparasize,8);
                       if inlined then
                         begin
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                            exprasmlist.concat(taicpu.op_reg_ref(A_MOV,S_L,p.location.registerlow,r));
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                            exprasmlist.concat(taicpu.op_reg_ref(A_MOV,S_L,p.location.registerhigh,r));
                         end
                       else
                         begin
                           exprasmlist.concat(taicpu.op_reg(A_PUSH,S_L,p.location.registerhigh));
                           exprasmlist.concat(taicpu.op_reg(A_PUSH,S_L,p.location.registerlow));
                         end;
                       ungetregister32(p.location.registerhigh);
                       ungetregister32(p.location.registerlow);
                    end
                  else case p.location.register of
                   R_EAX,R_EBX,R_ECX,R_EDX,R_ESI,
                   R_EDI,R_ESP,R_EBP :
                      begin
                        inc(pushedparasize,4);
                        if inlined then
                         begin
                           r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                           exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,p.location.register,r));
                         end
                        else
                         exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.location.register));
                        ungetregister32(p.location.register);
                      end;
                   R_AX,R_BX,R_CX,R_DX,R_SI,R_DI:
                      begin
                        if alignment=4 then
                          begin
                            opsize:=S_L;
                            hreg:=reg16toreg32(p.location.register);
                            inc(pushedparasize,4);
                          end
                        else
                          begin
                            opsize:=S_W;
                            hreg:=p.location.register;
                            inc(pushedparasize,2);
                          end;
                        if inlined then
                          begin
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                          end
                        else
                          exprasmList.concat(Taicpu.Op_reg(A_PUSH,opsize,hreg));
                        ungetregister32(reg16toreg32(p.location.register));
                      end;
                   R_AL,R_BL,R_CL,R_DL:
                      begin
                        if alignment=4 then
                          begin
                            opsize:=S_L;
                            hreg:=reg8toreg32(p.location.register);
                            inc(pushedparasize,4);
                          end
                        else
                          begin
                            opsize:=S_W;
                            hreg:=reg8toreg16(p.location.register);
                            inc(pushedparasize,2);
                          end;
                        { we must push always 16 bit }
                        if inlined then
                          begin
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                          end
                        else
                          exprasmList.concat(Taicpu.Op_reg(A_PUSH,opsize,hreg));
                        ungetregister32(reg8toreg32(p.location.register));
                      end;
                   else internalerror(1899);
                end;
             end;
           LOC_FPU:
             begin
                size:=align(tfloatdef(p.resulttype.def).size,alignment);
                inc(pushedparasize,size);
                if not inlined then
                 emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmList.first=exprasmList.last) then
                  exprasmList.concat(Tai_force_line.Create);
{$endif GDB}
                r:=new_reference(R_ESP,0);
                floatstoreops(tfloatdef(p.resulttype.def).typ,op,opsize);
                { this is the easiest case for inlined !! }
                if inlined then
                  begin
                     r^.base:=procinfo^.framepointer;
                     r^.offset:=para_offset-pushedparasize;
                  end;
                exprasmList.concat(Taicpu.Op_ref(op,opsize,r));
                dec(fpuvaroffset);
             end;
           LOC_CFPUREGISTER:
             begin
                exprasmList.concat(Taicpu.Op_reg(A_FLD,S_NO,
                  correct_fpuregister(p.location.register,fpuvaroffset)));
                size:=align(tfloatdef(p.resulttype.def).size,alignment);
                inc(pushedparasize,size);
                if not inlined then
                 emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmList.first=exprasmList.last) then
                  exprasmList.concat(Tai_force_line.Create);
{$endif GDB}
                r:=new_reference(R_ESP,0);
                floatstoreops(tfloatdef(p.resulttype.def).typ,op,opsize);
                { this is the easiest case for inlined !! }
                if inlined then
                  begin
                     r^.base:=procinfo^.framepointer;
                     r^.offset:=para_offset-pushedparasize;
                  end;
                exprasmList.concat(Taicpu.Op_ref(op,opsize,r));
             end;
           LOC_REFERENCE,LOC_MEM:
             begin
                tempreference:=p.location.reference;
                del_reference(p.location.reference);
                case p.resulttype.def.deftype of
                  enumdef,
                  orddef :
                    begin
                      case p.resulttype.def.size of
                       8 : begin
                             inc(pushedparasize,8);
                             if inlined then
                               begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                                 getexplicitregister32(R_EDI);
                                 inc(tempreference.offset,4);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                               end
                             else
                               begin
                                 inc(tempreference.offset,4);
                                 emit_push_mem(tempreference);
                                 dec(tempreference.offset,4);
                                 emit_push_mem(tempreference);
                               end;
                           end;
                       4 : begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                               end
                             else
                               emit_push_mem(tempreference);
                           end;
                     1,2 : begin
                             if alignment=4 then
                              begin
                                opsize:=S_L;
                                hreg:=R_EDI;
                                inc(pushedparasize,4);
                              end
                             else
                              begin
                                opsize:=S_W;
                                hreg:=R_DI;
                                inc(pushedparasize,2);
                              end;
                             if inlined then
                              begin
                                getexplicitregister32(R_EDI);
                                emit_ref_reg(A_MOV,opsize,
                                  newreference(tempreference),hreg);
                                r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                                ungetregister32(R_EDI);
                              end
                             else
                              emit_push_mem_size(tempreference,p.resulttype.def.size);
                           end;
                         else
                           internalerror(234231);
                      end;
                    end;
                  floatdef :
                    begin
                      case tfloatdef(p.resulttype.def).typ of
                        s32real :
                          begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                  getexplicitregister32(R_EDI);
                                  emit_ref_reg(A_MOV,S_L,
                                    newreference(tempreference),R_EDI);
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                  ungetregister32(R_EDI);
                               end
                             else
                               emit_push_mem(tempreference);
                          end;
                        s64real,
                        s64comp :
                          begin
                            inc(pushedparasize,4);
                            inc(tempreference.offset,4);
                            if inlined then
                              begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                            inc(pushedparasize,4);
                            dec(tempreference.offset,4);
                            if inlined then
                              begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                          end;
                        s80real :
                          begin
                            inc(pushedparasize,4);
                            if alignment=4 then
                              inc(tempreference.offset,8)
                            else
                              inc(tempreference.offset,6);
                            if inlined then
                              begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                            dec(tempreference.offset,4);
                            inc(pushedparasize,4);
                            if inlined then
                              begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                                 ungetregister32(R_EDI);
                              end
                            else
                              emit_push_mem(tempreference);
                            if alignment=4 then
                              begin
                                opsize:=S_L;
                                hreg:=R_EDI;
                                inc(pushedparasize,4);
                                dec(tempreference.offset,4);
                              end
                            else
                              begin
                                opsize:=S_W;
                                hreg:=R_DI;
                                inc(pushedparasize,2);
                                dec(tempreference.offset,2);
                              end;
                            if inlined then
                              begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,opsize,
                                   newreference(tempreference),hreg);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                                 ungetregister32(R_EDI);
                              end
                            else
                              exprasmList.concat(Taicpu.Op_ref(A_PUSH,opsize,
                                newreference(tempreference)));
                        end;
                      end;
                    end;
                  pointerdef,
                  procvardef,
                  classrefdef:
                    begin
                       inc(pushedparasize,4);
                       if inlined then
                         begin
                            getexplicitregister32(R_EDI);
                            emit_ref_reg(A_MOV,S_L,
                              newreference(tempreference),R_EDI);
                            r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                            exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,S_L,R_EDI,r));
                            ungetregister32(R_EDI);
                         end
                       else
                         emit_push_mem(tempreference);
                    end;
                  arraydef,
                  recorddef,
                  stringdef,
                  setdef,
                  objectdef :
                    begin
                       { even some structured types are 32 bit }
                       if is_widestring(p.resulttype.def) or
                          is_ansistring(p.resulttype.def) or
                          is_smallset(p.resulttype.def) or
                          ((p.resulttype.def.deftype in [recorddef,arraydef]) and
                           (
                            (p.resulttype.def.deftype<>arraydef) or not
                            (tarraydef(p.resulttype.def).IsConstructor or
                             tarraydef(p.resulttype.def).isArrayOfConst or
                             is_open_array(p.resulttype.def))
                           ) and
                           (p.resulttype.def.size<=4)
                          ) or
                          is_class(p.resulttype.def) or
                          is_interface(p.resulttype.def) then
                         begin
                            if (p.resulttype.def.size>2) or
                               ((alignment=4) and (p.resulttype.def.size>0)) then
                              begin
                                inc(pushedparasize,4);
                                if inlined then
                                  begin
                                    r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                    concatcopy(tempreference,r^,4,false,false);
                                  end
                                else
                                  emit_push_mem(tempreference);
                              end
                            else
                              begin
                                if p.resulttype.def.size>0 then
                                  begin
                                    inc(pushedparasize,2);
                                    if inlined then
                                      begin
                                        r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                        concatcopy(tempreference,r^,2,false,false);
                                      end
                                    else
                                      exprasmList.concat(Taicpu.Op_ref(A_PUSH,S_W,newreference(tempreference)));
                                  end;
                              end;
                         end
                       { call by value open array ? }
                       else if is_cdecl then
                         begin
                           { push on stack }
                           size:=align(p.resulttype.def.size,alignment);
                           inc(pushedparasize,size);
                           emit_const_reg(A_SUB,S_L,size,R_ESP);
                           r:=new_reference(R_ESP,0);
                           concatcopy(tempreference,r^,size,false,false);
                           dispose(r);
                         end
                       else
                         internalerror(8954);
                    end;
                  else
                    CGMessage(cg_e_illegal_expression);
                end;
             end;
           LOC_JUMP:
             begin
                getlabel(hlabel);
                if alignment=4 then
                 begin
                   opsize:=S_L;
                   inc(pushedparasize,4);
                 end
                else
                 begin
                   opsize:=S_W;
                   inc(pushedparasize,2);
                 end;
                emitlab(truelabel);
                if inlined then
                  begin
                     r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                     emit_const_ref(A_MOV,opsize,1,r);
                  end
                else
                  exprasmList.concat(Taicpu.Op_const(A_PUSH,opsize,1));
                emitjmp(C_None,hlabel);
                emitlab(falselabel);
                if inlined then
                  begin
                     r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                     emit_const_ref(A_MOV,opsize,0,r);
                  end
                else
                  exprasmList.concat(Taicpu.Op_const(A_PUSH,opsize,0));
                emitlab(hlabel);
             end;
           LOC_FLAGS:
             begin
                if not(R_EAX in unused) then
                  begin
                    getexplicitregister32(R_EDI);
                    emit_reg_reg(A_MOV,S_L,R_EAX,R_EDI);
                  end;
                emit_flag2reg(p.location.resflags,R_AL);
                emit_reg_reg(A_MOVZX,S_BW,R_AL,R_AX);
                if alignment=4 then
                 begin
                   opsize:=S_L;
                   hreg:=R_EAX;
                   inc(pushedparasize,4);
                 end
                else
                 begin
                   opsize:=S_W;
                   hreg:=R_AX;
                   inc(pushedparasize,2);
                 end;
                if inlined then
                  begin
                     r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOV,opsize,hreg,r));
                  end
                else
                  exprasmList.concat(Taicpu.Op_reg(A_PUSH,opsize,hreg));
                if not(R_EAX in unused) then
                  begin
                    emit_reg_reg(A_MOV,S_L,R_EDI,R_EAX);
                    ungetregister32(R_EDI);
                  end;
             end;
{$ifdef SUPPORT_MMX}
           LOC_MMXREGISTER,
           LOC_CMMXREGISTER:
             begin
                inc(pushedparasize,8); { was missing !!! (PM) }
                emit_const_reg(
                  A_SUB,S_L,8,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmList.first=exprasmList.last) then
                  exprasmList.concat(Tai_force_line.Create);
{$endif GDB}
                if inlined then
                  begin
                     r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                     exprasmList.concat(Taicpu.Op_reg_ref(A_MOVQ,S_NO,
                       p.location.register,r));
                  end
                else
                   begin
                      r:=new_reference(R_ESP,0);
                      exprasmList.concat(Taicpu.Op_reg_ref(
                        A_MOVQ,S_NO,p.location.register,r));
                   end;
             end;
{$endif SUPPORT_MMX}
        end;
      end;

{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure maketojumpbool(p : tnode; loadregvars: tloadregvars);
    {
      produces jumps to true respectively false labels using boolean expressions

      depending on whether the loading of regvars is currently being
      synchronized manually (such as in an if-node) or automatically (most of
      the other cases where this procedure is called), loadregvars can be
      "lr_load_regvars" or "lr_dont_load_regvars"
    }
      var
        opsize : topsize;
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
                     emitjmp(C_None,truelabel)
                   else
                     emitjmp(C_None,falselabel);
                end
              else
                begin
                   opsize:=def_opsize(p.resulttype.def);
                   case p.location.loc of
                      LOC_CREGISTER,LOC_REGISTER : begin
                                        if (p.location.loc = LOC_CREGISTER) then
                                          load_regvar_reg(exprasmlist,p.location.register);
                                        emit_reg_reg(A_OR,opsize,p.location.register,
                                          p.location.register);
                                        ungetregister(p.location.register);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_MEM,LOC_REFERENCE : begin
                                        emit_const_ref(
                                          A_CMP,opsize,0,newreference(p.location.reference));
                                        del_reference(p.location.reference);
                                        emitjmp(C_NZ,truelabel);
                                        emitjmp(C_None,falselabel);
                                     end;
                      LOC_FLAGS : begin
                                     emitjmp(flag_2_cond[p.location.resflags],truelabel);
                                     emitjmp(C_None,falselabel);
                                  end;
                   end;
                end;
           end
         else
           CGMessage(type_e_mismatch);
         aktfilepos:=storepos;
      end;


    { produces if necessary overflowcode }
    procedure emitoverflowcheck(p:tnode);
      var
         hl : tasmlabel;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         getlabel(hl);
         if not ((p.resulttype.def.deftype=pointerdef) or
                ((p.resulttype.def.deftype=orddef) and
                 (torddef(p.resulttype.def).typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                                  bool8bit,bool16bit,bool32bit]))) then
           emitjmp(C_NO,hl)
         else
           emitjmp(C_NB,hl);
         emitcall('FPC_OVERFLOW');
         emitlab(hl);
      end;

    { produces range check code, while one of the operands is a 64 bit
      integer }
    procedure emitrangecheck64(p : tnode;todef : tdef);

      var
        neglabel,
        poslabel,
        endlabel: tasmlabel;
        href   : preference;
        hreg   : tregister;
        hdef   :  torddef;
        fromdef : tdef;
        opcode : tasmop;
        opsize   : topsize;
        oldregisterdef: boolean;
        from_signed,to_signed: boolean;

      begin
         fromdef:=p.resulttype.def;
         from_signed := is_signed(fromdef);
         to_signed := is_signed(todef);

         if not is_64bitint(todef) then
           begin
             oldregisterdef := registerdef;
             registerdef := false;

             { get the high dword in a register }
             if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
               hreg := p.location.registerhigh
             else
               begin
                 hreg := getexplicitregister32(R_EDI);
                 href := newreference(p.location.reference);
                 inc(href^.offset,4);
                 emit_ref_reg(A_MOV,S_L,href,hreg);
               end;
             getlabel(poslabel);

             { check high dword, must be 0 (for positive numbers) }
             emit_reg_reg(A_TEST,S_L,hreg,hreg);
             emitjmp(C_E,poslabel);

             { It can also be $ffffffff, but only for negative numbers }
             if from_signed and to_signed then
               begin
                 getlabel(neglabel);
                 emit_const_reg(A_CMP,S_L,longint($ffffffff),hreg);
                 emitjmp(C_E,neglabel);
               end;
             if hreg = R_EDI then
               ungetregister32(hreg);
             { For all other values we have a range check error }
             emitcall('FPC_RANGEERROR');

             { if the high dword = 0, the low dword can be considered a }
             { simple cardinal                                          }
             emitlab(poslabel);
             hdef:=torddef.create(u32bit,0,longint($ffffffff));
             { the real p.resulttype.def is already saved in fromdef }
             p.resulttype.def := hdef;
             emitrangecheck(p,todef);
             hdef.free;
             { restore original resulttype.def }
             p.resulttype.def := todef;

             if from_signed and to_signed then
               begin
                 getlabel(endlabel);
                 emitjmp(C_None,endlabel);
                 { if the high dword = $ffffffff, then the low dword (when }
                 { considered as a longint) must be < 0                    }
                 emitlab(neglabel);
                 if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                   hreg := p.location.registerlow
                 else
                   begin
                     hreg := getexplicitregister32(R_EDI);
                     emit_ref_reg(A_MOV,S_L,
                       newreference(p.location.reference),hreg);
                   end;
                 { get a new neglabel (JM) }
                 getlabel(neglabel);
                 emit_reg_reg(A_TEST,S_L,hreg,hreg);
                 if hreg = R_EDI then
                   ungetregister32(hreg);
                 emitjmp(C_L,neglabel);

                 emitcall('FPC_RANGEERROR');

                 { if we get here, the 64bit value lies between }
                 { longint($80000000) and -1 (JM)               }
                 emitlab(neglabel);
                 hdef:=torddef.create(s32bit,longint($80000000),-1);
                 p.resulttype.def := hdef;
                 emitrangecheck(p,todef);
                 hdef.free;
                 emitlab(endlabel);
               end;
             registerdef := oldregisterdef;
             p.resulttype.def := fromdef;
             { restore p's resulttype.def }
           end
         else
           { todef = 64bit int }
           { no 64bit subranges supported, so only a small check is necessary }

           { if both are signed or both are unsigned, no problem! }
           if (from_signed xor to_signed) and
              { also not if the fromdef is unsigned and < 64bit, since that will }
              { always fit in a 64bit int (todef is 64bit)                       }
              (from_signed or
               (torddef(fromdef).typ = u64bit)) then
             begin
               { in all cases, there is only a problem if the higest bit is set }
               if p.location.loc in [LOC_REGISTER,LOC_CREGISTER] then
                 if is_64bitint(fromdef) then
                   hreg := p.location.registerhigh
                 else
                   hreg := p.location.register
               else
                 begin
                   hreg := getexplicitregister32(R_EDI);
                   case p.resulttype.def.size of
                     1: opsize := S_BL;
                     2: opsize := S_WL;
                     4,8: opsize := S_L;
                   end;
                   if opsize in [S_BL,S_WL] then
                     if from_signed then
                       opcode := A_MOVSX
                     else opcode := A_MOVZX
                   else
                     opcode := A_MOV;
                   href := newreference(p.location.reference);
                   if p.resulttype.def.size = 8 then
                     inc(href^.offset,4);
                   emit_ref_reg(opcode,opsize,href,hreg);
                 end;
               getlabel(poslabel);
               emit_reg_reg(A_TEST,regsize(hreg),hreg,hreg);
               if hreg = R_EDI then
                 ungetregister32(hreg);
               emitjmp(C_GE,poslabel);
               emitcall('FPC_RANGEERROR');
               emitlab(poslabel);
             end;
      end;

     { produces if necessary rangecheckcode }
     procedure emitrangecheck(p:tnode;todef:tdef);
     {
       generate range checking code for the value at location t. The
       type used is the checked against todefs ranges. fromdef (p.resulttype.def)
       is the original type used at that location, when both defs are
       equal the check is also insert (needed for succ,pref,inc,dec)
     }
      var
        neglabel : tasmlabel;
        opsize : topsize;
        op     : tasmop;
        fromdef : tdef;
        lto,hto,
        lfrom,hfrom : TConstExprInt;
        is_reg : boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(todef.deftype in [orddef,enumdef,arraydef]) then
          exit;
        { only check when assigning to scalar, subranges are different,
          when todef=fromdef then the check is always generated }
        fromdef:=p.resulttype.def;
        { no range check if from and to are equal and are both longint/dword or }
        { int64/qword, since such operations can at most cause overflows (JM)   }
        if (fromdef = todef) and
          { then fromdef and todef can only be orddefs }
           (((torddef(fromdef).typ = s32bit) and
             (torddef(fromdef).low = longint($80000000)) and
             (torddef(fromdef).high = $7fffffff)) or
            ((torddef(fromdef).typ = u32bit) and
             (torddef(fromdef).low = 0) and
             (torddef(fromdef).high = longint($ffffffff))) or
            is_64bitint(fromdef)) then
          exit;
        if is_64bitint(fromdef) or is_64bitint(todef) then
          begin
             emitrangecheck64(p,todef);
             exit;
          end;
        {we also need lto and hto when checking if we need to use doublebound!
        (JM)}
        getrange(todef,lto,hto);
        if todef<>fromdef then
         begin
           getrange(p.resulttype.def,lfrom,hfrom);
           { first check for not being u32bit, then if the to is bigger than
             from }
           if (lto<hto) and (lfrom<hfrom) and
              (lto<=lfrom) and (hto>=hfrom) then
            exit;
         end;
        { generate the rangecheck code for the def where we are going to
          store the result }
      { get op and opsize }
        opsize:=def2def_opsize(fromdef,u32bittype.def);
        if opsize in [S_B,S_W,S_L] then
         op:=A_MOV
        else
         if is_signed(fromdef) then
          op:=A_MOVSX
         else
          op:=A_MOVZX;
        is_reg:=(p.location.loc in [LOC_REGISTER,LOC_CREGISTER]);
        { use the trick that                                                 }
        { a <= x <= b <=> 0 <= x-a <= b-a <=> cardinal(x-a) <= cardinal(b-a) }

        { To be able to do that, we have to make sure however that either    }
        { fromdef and todef are both signed or unsigned, or that we leave    }
        { the parts < 0 and > maxlongint out                                 }

        { is_signed now also works for arrays (it checks the rangetype) (JM) }
        if is_signed(fromdef) xor is_signed(todef) then
          if is_signed(fromdef) then
            { from is signed, to is unsigned }
            begin
              { if high(from) < 0 -> always range error }
              if (hfrom < 0) or
                 { if low(to) > maxlongint (== < 0, since we only have }
                 { longints here), also range error                    }
                 (lto < 0) then
                begin
                  emitcall('FPC_RANGEERROR');
                  exit
                end;
              { to is unsigned -> hto < 0 == hto > maxlongint              }
              { since from is signed, values > maxlongint are < 0 and must }
              { be rejected                                                }
              if hto < 0 then
                hto := maxlongint;
            end
          else
            { from is unsigned, to is signed }
            begin
              if (lfrom < 0) or
                 (hto < 0) then
                begin
                  emitcall('FPC_RANGEERROR');
                  exit
                end;
              { since from is unsigned, values > maxlongint are < 0 and must }
              { be rejected                                                  }
              if lto < 0 then
                lto := 0;
            end;

        getexplicitregister32(R_EDI);
        if is_reg and
           (opsize = S_L) then
          emit_ref_reg(A_LEA,opsize,new_reference(p.location.register,-lto),
            R_EDI)
        else
          begin
            if is_reg then
              emit_reg_reg(op,opsize,p.location.register,R_EDI)
            else
              emit_ref_reg(op,opsize,newreference(p.location.reference),R_EDI);
            if lto <> 0 then
              emit_const_reg(A_SUB,S_L,lto,R_EDI);
          end;
        emit_const_reg(A_CMP,S_L,hto-lto,R_EDI);
        ungetregister32(R_EDI);
        getlabel(neglabel);
        emitjmp(C_BE,neglabel);
        emitcall('FPC_RANGEERROR');
        emitlab(neglabel);
      end;


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
           if (((p.location.loc=LOC_FPU) and
                (p.right.registersfpu > p.left.registersfpu)) or
               ((((p.left.registersfpu = 0) and
                  (p.right.registersfpu = 0)) or
                 (p.location.loc<>LOC_FPU)) and
                (p.left.registers32<p.right.registers32))) and
           { the following check is appropriate, because all }
           { 4 registers are rarely used and it is thereby   }
           { achieved that the extra code is being dropped   }
           { by exchanging not commutative operators     }
               (p.right.registers32<=4) then
            begin
              hp:=p.left;
              p.left:=p.right;
              p.right:=hp;
              if nf_swaped in p.flags then
                exclude(p.flags,nf_swaped)
              else
                include(p.flags,nf_swaped);
            end;
         {else
           p.swaped:=false; do not modify }
      end;

{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure push_shortstring_length(p:tnode);
      var
        hightree : tnode;
        srsym    : tsym;
      begin
        if is_open_string(p.resulttype.def) then
         begin
           srsym:=searchsymonlyin(tloadnode(p).symtable,'high'+tvarsym(tloadnode(p).symtableentry).name);
           hightree:=cloadnode.create(tvarsym(srsym),tloadnode(p).symtable);
           firstpass(hightree);
           secondpass(hightree);
           push_value_para(hightree,false,false,0,4);
           hightree.free;
           hightree:=nil;
         end
        else
         begin
           push_int(tstringdef(p.resulttype.def).len);
         end;
      end;

{*****************************************************************************
                           String functions
*****************************************************************************}

    procedure loadshortstring(source,dest : tnode);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
        href: treference;
      begin
         case source.resulttype.def.deftype of
            stringdef:
              begin
                 if (source.nodetype=stringconstn) and
                   (str_length(source)=0) then
                   emit_const_ref(
                      A_MOV,S_B,0,newreference(dest.location.reference))
                 else
                   begin
                     emitpushreferenceaddr(dest.location.reference);
                     emitpushreferenceaddr(source.location.reference);
                     push_shortstring_length(dest);
                     emitcall('FPC_SHORTSTR_COPY');
                     maybe_loadself;
                   end;
              end;
            orddef:
              begin
                 if source.nodetype=ordconstn then
                   emit_const_ref(
                      A_MOV,S_W,tordconstnode(source).value*256+1,newreference(dest.location.reference))
                 else
                   begin
                      if (source.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                        begin
                           href := dest.location.reference;
                           emit_const_ref(A_MOV,S_B,1,newreference(href));
                           inc(href.offset,1);
                           emit_reg_ref(A_MOV,S_B,makereg8(source.location.register),
                                        newreference(href));
                           ungetregister(source.location.register);
                        end
                      else
                      { not so elegant (goes better with extra register    }
                        begin
                           { not "movl", because then we may read past the }
                           { end of the heap! "movw" would be ok too, but  }
                           { I don't think that would be faster (JM)       }
                           getexplicitregister32(R_EDI);
                           emit_ref_reg(A_MOVZX,S_BL,newreference(source.location.reference),R_EDI);
                           del_reference(source.location.reference);
                           emit_const_reg(A_SHL,S_L,8,R_EDI);
                           emit_const_reg(A_OR,S_L,1,R_EDI);
                           emit_reg_ref(A_MOV,S_W,R_DI,newreference(dest.location.reference));
                           ungetregister32(R_EDI);
                        end;
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;

    procedure loadlongstring(p:tbinarynode);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      var
         r : preference;

      begin
         case p.right.resulttype.def.deftype of
            stringdef:
              begin
                 if (p.right.nodetype=stringconstn) and
                   (str_length(p.right)=0) then
                   emit_const_ref(A_MOV,S_L,0,newreference(p.left.location.reference))
                 else
                   begin
                     emitpushreferenceaddr(p.left.location.reference);
                     emitpushreferenceaddr(p.right.location.reference);
                     push_shortstring_length(p.left);
                     emitcall('FPC_LONGSTR_COPY');
                     maybe_loadself;
                   end;
              end;
            orddef:
              begin
                 emit_const_ref(A_MOV,S_L,1,newreference(p.left.location.reference));

                 r:=newreference(p.left.location.reference);
                 inc(r^.offset,4);

                 if p.right.nodetype=ordconstn then
                   emit_const_ref(A_MOV,S_B,tordconstnode(p.right).value,r)
                 else
                   begin
                      case p.right.location.loc of
                         LOC_REGISTER,LOC_CREGISTER:
                           begin
                              emit_reg_ref(A_MOV,S_B,p.right.location.register,r);
                              ungetregister(p.right.location.register);
                           end;
                         LOC_MEM,LOC_REFERENCE:
                           begin
                              if not(R_EAX in unused) then
                                emit_reg(A_PUSH,S_L,R_EAX);
                              emit_ref_reg(A_MOV,S_B,newreference(p.right.location.reference),R_AL);
                              emit_reg_ref(A_MOV,S_B,R_AL,r);

                              if not(R_EAX in unused) then
                                emit_reg(A_POP,S_L,R_EAX);
                              del_reference(p.right.location.reference);
                           end
                         else
                           internalerror(20799);
                        end;
                   end;
              end;
         else
           CGMessage(type_e_mismatch);
         end;
      end;


    procedure loadansi2short(source,dest : tnode);
      var
         pushed : tpushed;
         regs_to_push: byte;
      begin
         { Find out which registers have to be pushed (JM) }
         regs_to_push := $ff;
         remove_non_regvars_from_loc(source.location,regs_to_push);
         { Push them (JM) }
         pushusedregisters(pushed,regs_to_push);
         case source.location.loc of
           LOC_REFERENCE,LOC_MEM:
             begin
                { Now release the location and registers (see cgai386.pas: }
                { loadansistring for more info on the order) (JM)          }
                ungetiftemp(source.location.reference);
                del_reference(source.location.reference);
                emit_push_mem(source.location.reference);
             end;
           LOC_REGISTER,LOC_CREGISTER:
             begin
                emit_reg(A_PUSH,S_L,source.location.register);
                { Now release the register (JM) }
                ungetregister32(source.location.register);
             end;
         end;
         push_shortstring_length(dest);
         emitpushreferenceaddr(dest.location.reference);
         saveregvars($ff);
         emitcall('FPC_ANSISTR_TO_SHORTSTR');
         popusedregisters(pushed);
         maybe_loadself;
      end;


    procedure loadwide2short(source,dest : tnode);
      var
         pushed : tpushed;
         regs_to_push: byte;
      begin
         { Find out which registers have to be pushed (JM) }
         regs_to_push := $ff;
         remove_non_regvars_from_loc(source.location,regs_to_push);
         { Push them (JM) }
         pushusedregisters(pushed,regs_to_push);
         case source.location.loc of
           LOC_REFERENCE,LOC_MEM:
             begin
                { Now release the location and registers (see cgai386.pas: }
                { loadansistring for more info on the order) (JM)          }
                ungetiftemp(source.location.reference);
                del_reference(source.location.reference);
                emit_push_mem(source.location.reference);
             end;
           LOC_REGISTER,LOC_CREGISTER:
             begin
                emit_reg(A_PUSH,S_L,source.location.register);
                { Now release the register (JM) }
                ungetregister32(source.location.register);
             end;
         end;
         push_shortstring_length(dest);
         emitpushreferenceaddr(dest.location.reference);
         saveregvars($ff);
         emitcall('FPC_WIDESTR_TO_SHORTSTR');
         popusedregisters(pushed);
         maybe_loadself;
      end;


    procedure loadinterfacecom(p: tbinarynode);
    {
      copies an com interface from n.right to n.left, we
      assume, that both sides are com interface, firstassignement have
      to take care of that, an com interface can't be a register variable
    }
      var
         pushed : tpushed;
         ungettemp : boolean;
      begin
         { before pushing any parameter, we have to save all used      }
         { registers, but before that we have to release the       }
         { registers of that node to save uneccessary pushed       }
         { so be careful, if you think you can optimize that code (FK) }

         { nevertheless, this has to be changed, because otherwise the }
         { register is released before it's contents are pushed ->     }
         { problems with the optimizer (JM)                         }
         del_reference(p.left.location.reference);
         ungettemp:=false;
         case p.right.location.loc of
            LOC_REGISTER,LOC_CREGISTER:
              begin
                 pushusedregisters(pushed, $ff xor ($80 shr byte(p.right.location.register)));
                 exprasmList.concat(Taicpu.Op_reg(A_PUSH,S_L,p.right.location.register));
                 ungetregister32(p.right.location.register);
              end;
            LOC_REFERENCE,LOC_MEM:
              begin
                 pushusedregisters(pushed,$ff
                   xor ($80 shr byte(p.right.location.reference.base))
                   xor ($80 shr byte(p.right.location.reference.index)));
                 emit_push_mem(p.right.location.reference);
                 del_reference(p.right.location.reference);
                 ungettemp:=true;
              end;
         end;
         emitpushreferenceaddr(p.left.location.reference);
         del_reference(p.left.location.reference);
         saveregvars($ff);
         emitcall('FPC_INTF_ASSIGN');
         maybe_loadself;
         popusedregisters(pushed);
         if ungettemp then
           ungetiftemp(p.right.location.reference);
      end;



end.
{
  $Log$
  Revision 1.24  2001-12-03 21:48:43  peter
    * freemem change to value parameter
    * torddef low/high range changed to int64

  Revision 1.23  2001/12/02 16:19:17  jonas
    * less unnecessary regvar loading with if-statements

  Revision 1.22  2001/10/12 13:51:52  jonas
    * fixed internalerror(10) due to previous fpu overflow fixes ("merged")
    * fixed bug in n386add (introduced after compilerproc changes for string
      operations) where calcregisters wasn't called for shortstring addnodes
    * NOTE: from now on, the location of a binary node must now always be set
       before you call calcregisters() for it

  Revision 1.21  2001/09/17 21:29:14  peter
    * merged netbsd, fpu-overflow from fixes branch

  Revision 1.20  2001/08/26 13:37:01  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.19  2001/08/24 12:22:14  jonas
    * fixed memory leak with coping of array-of-consts as valuepara

  Revision 1.18  2001/07/08 21:00:18  peter
    * various widestring updates, it works now mostly without charset
      mapping supported

  Revision 1.17  2001/07/01 20:16:20  peter
    * alignmentinfo record added
    * -Oa argument supports more alignment settings that can be specified
      per type: PROC,LOOP,VARMIN,VARMAX,CONSTMIN,CONSTMAX,RECORDMIN
      RECORDMAX,LOCALMIN,LOCALMAX. It is possible to set the mimimum
      required alignment and the maximum usefull alignment. The final
      alignment will be choosen per variable size dependent on these
      settings

  Revision 1.16  2001/04/18 22:02:03  peter
    * registration of targets and assemblers

  Revision 1.15  2001/04/13 01:22:19  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.14  2001/04/02 21:20:39  peter
    * resulttype rewrite

  Revision 1.13  2001/03/11 22:58:52  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.12  2001/03/04 10:26:56  jonas
    * new rangecheck code now handles conversion between signed and cardinal types correctly

  Revision 1.11  2001/03/03 12:41:22  jonas
    * simplified and optimized range checking code, FPC_BOUNDCHECK is no longer necessary

  Revision 1.10  2000/12/31 11:02:12  jonas
    * optimized loadshortstring a bit

  Revision 1.9  2000/12/25 00:07:33  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.8  2000/12/11 19:10:19  jonas
    * fixed web bug 1144
    + implemented range checking for 64bit types

  Revision 1.7  2000/12/07 17:19:46  jonas
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

  Revision 1.6  2000/12/05 11:44:34  jonas
    + new integer regvar handling, should be much more efficient

  Revision 1.5  2000/11/29 00:30:49  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.4  2000/11/13 14:47:46  jonas
    * support for range checking when converting from 64bit to something
      smaller (32bit, 16bit, 8bit)
    * fixed range checking between longint/cardinal and for array indexing
      with cardinal (values > $7fffffff were considered negative)

  Revision 1.3  2000/11/04 14:25:25  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.2  2000/10/31 22:02:57  peter
    * symtable splitted, no real code changes

  Revision 1.1  2000/10/15 09:33:32  peter
    * moved n386*.pas to i386/ cpu_target dir

  Revision 1.3  2000/10/14 21:52:54  peter
    * fixed memory leaks

  Revision 1.2  2000/10/14 10:14:50  peter
    * moehrendorf oct 2000 rewrite

  Revision 1.1  2000/10/01 19:58:40  peter
    * new file

}
