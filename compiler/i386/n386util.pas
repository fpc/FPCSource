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

    function  maybe_push(needed : byte;p : tnode;isint64 : boolean) : boolean;
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
    procedure loadinterfacecom(p: tbinarynode);

    procedure maketojumpbool(p : tnode);
    procedure emitoverflowcheck(p:tnode);
    procedure emitrangecheck(p:tnode;todef:pdef);
    procedure firstcomplex(p : tbinarynode);

implementation

    uses
       globtype,globals,systems,verbose,
       cutils,cobjects,
       aasm,cpubase,cpuasm,
       symconst,symbase,symdef,symsym,symtable,
{$ifdef GDB}
       gdb,
{$endif GDB}
       types,
       ncon,nld,
       pass_1,pass_2,
       hcodegen,tgcpu,temp_gen,
       cgai386,regvars;


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
                        exprasmlist^.concat(new(paicpu,op_reg(A_MOV,S_L,p.location.registerhigh,href)));
                        href.offset:=href.offset-4;
{$else TEMPS_NOT_PUSH}
                        exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p.location.registerhigh)));
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
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,p.location.register,href)));
{$else TEMPS_NOT_PUSH}
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p.location.register)));
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
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,href)));
                     p.temp_offset:=href.offset;
{$else TEMPS_NOT_PUSH}
                     exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_EDI)));
{$endif TEMPS_NOT_PUSH}
                     ungetregister32(R_EDI);
                     pushed:=true;
                  end
              else pushed:=false;
           end
         else pushed:=false;
         maybe_push:=pushed;
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
                   if isint64(p^.resulttype) then
                     begin
                        gettempofsizereference(href,8);
                        p^.temp_offset:=href.offset;
                        href.offset:=href.offset+4;
                        exprasmlist^.concat(new(paicpu,op_reg(A_MOV,S_L,p^.location.registerhigh,href)));
                        href.offset:=href.offset-4;
                        ungetregister32(p^.location.registerhigh);
                     end
                   else
                     begin
                        gettempofsizereference(href,4);
                        p^.temp_offset:=href.offset;
                     end;
                   pushed:=true;
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,p^.location.register,href)));
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
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,href)));
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
         exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,hregister)));
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
                   exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,p.location.registerhigh)));
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
           if target_os.stackalignment=4 then
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_L,tordconstnode(p).value)))
           else
             exprasmlist^.concat(new(paicpu,op_const(A_PUSH,S_W,tordconstnode(p).value)));
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
                 if target_os.stackalignment=4 then
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,hr32)))
                 else
                   exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_W,hr16)));
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
        hlabel : pasmlabel;
      begin
        case p.location.loc of
           LOC_REGISTER,
           LOC_CREGISTER:
             begin
                case p.location.register of
                   R_EAX,R_EBX,R_ECX,R_EDX,R_ESI,
                   R_EDI,R_ESP,R_EBP :
                      begin
                        if p.resulttype^.size=8 then
                          begin
                             inc(pushedparasize,8);
                             if inlined then
                               begin
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                    p.location.registerlow,r)));
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                    p.location.registerhigh,r)));
                               end
                             else
                               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p.location.registerhigh)));
                             ungetregister32(p.location.registerhigh);
                               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p.location.registerlow)));
                             ungetregister32(p.location.registerlow);
                          end
                        else
                          begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,
                                    p.location.register,r)));
                               end
                             else
                               exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p.location.register)));
                             ungetregister32(p.location.register);
                          end;
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
                            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                          end
                        else
                          exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
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
                            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                          end
                        else
                          exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
                        ungetregister32(reg8toreg32(p.location.register));
                      end;
                   else internalerror(1899);
                end;
             end;
           LOC_FPU:
             begin
                size:=align(pfloatdef(p.resulttype)^.size,alignment);
                inc(pushedparasize,size);
                if not inlined then
                 emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmlist^.first=exprasmlist^.last) then
                  exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                r:=new_reference(R_ESP,0);
                floatstoreops(pfloatdef(p.resulttype)^.typ,op,opsize);
                { this is the easiest case for inlined !! }
                if inlined then
                  begin
                     r^.base:=procinfo^.framepointer;
                     r^.offset:=para_offset-pushedparasize;
                  end;
                exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));
                dec(fpuvaroffset);
             end;
           LOC_CFPUREGISTER:
             begin
                exprasmlist^.concat(new(paicpu,op_reg(A_FLD,S_NO,
                  correct_fpuregister(p.location.register,fpuvaroffset))));
                size:=align(pfloatdef(p.resulttype)^.size,alignment);
                inc(pushedparasize,size);
                if not inlined then
                 emit_const_reg(A_SUB,S_L,size,R_ESP);
{$ifdef GDB}
                if (cs_debuginfo in aktmoduleswitches) and
                   (exprasmlist^.first=exprasmlist^.last) then
                  exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                r:=new_reference(R_ESP,0);
                floatstoreops(pfloatdef(p.resulttype)^.typ,op,opsize);
                { this is the easiest case for inlined !! }
                if inlined then
                  begin
                     r^.base:=procinfo^.framepointer;
                     r^.offset:=para_offset-pushedparasize;
                  end;
                exprasmlist^.concat(new(paicpu,op_ref(op,opsize,r)));
             end;
           LOC_REFERENCE,LOC_MEM:
             begin
                tempreference:=p.location.reference;
                del_reference(p.location.reference);
                case p.resulttype^.deftype of
                  enumdef,
                  orddef :
                    begin
                      case p.resulttype^.size of
                       8 : begin
                             inc(pushedparasize,8);
                             if inlined then
                               begin
                                 getexplicitregister32(R_EDI);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
                                 ungetregister32(R_EDI);
                                 getexplicitregister32(R_EDI);
                                 inc(tempreference.offset,4);
                                 emit_ref_reg(A_MOV,S_L,
                                   newreference(tempreference),R_EDI);
                                 r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize+4);
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                                ungetregister32(R_EDI);
                              end
                             else
                              emit_push_mem_size(tempreference,p.resulttype^.size);
                           end;
                         else
                           internalerror(234231);
                      end;
                    end;
                  floatdef :
                    begin
                      case pfloatdef(p.resulttype)^.typ of
                        f32bit,
                        s32real :
                          begin
                             inc(pushedparasize,4);
                             if inlined then
                               begin
                                  getexplicitregister32(R_EDI);
                                  emit_ref_reg(A_MOV,S_L,
                                    newreference(tempreference),R_EDI);
                                  r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                                 exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                                 ungetregister32(R_EDI);
                              end
                            else
                              exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,opsize,
                                newreference(tempreference))));
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
                            exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,S_L,R_EDI,r)));
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
                       if is_widestring(p.resulttype) or
                          is_ansistring(p.resulttype) or
                          is_smallset(p.resulttype) or
                          ((p.resulttype^.deftype in [recorddef,arraydef]) and
                           (
                            (p.resulttype^.deftype<>arraydef) or not
                            (parraydef(p.resulttype)^.IsConstructor or
                             parraydef(p.resulttype)^.isArrayOfConst or
                             is_open_array(p.resulttype))
                           ) and
                           (p.resulttype^.size<=4)
                          ) or
                          is_class(p.resulttype) or
                          is_interface(p.resulttype) then
                         begin
                            if (p.resulttype^.size>2) or
                               ((alignment=4) and (p.resulttype^.size>0)) then
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
                                if p.resulttype^.size>0 then
                                  begin
                                    inc(pushedparasize,2);
                                    if inlined then
                                      begin
                                        r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                                        concatcopy(tempreference,r^,2,false,false);
                                      end
                                    else
                                      exprasmlist^.concat(new(paicpu,op_ref(A_PUSH,S_W,newreference(tempreference))));
                                  end;
                              end;
                         end
                       { call by value open array ? }
                       else if is_cdecl then
                         begin
                           { push on stack }
                           size:=align(p.resulttype^.size,alignment);
                           inc(pushedparasize,size);
                           emit_const_reg(A_SUB,S_L,size,R_ESP);
                           r:=new_reference(R_ESP,0);
                           concatcopy(tempreference,r^,size,false,false);
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
                  exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,1)));
                emitjmp(C_None,hlabel);
                emitlab(falselabel);
                if inlined then
                  begin
                     r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                     emit_const_ref(A_MOV,opsize,0,r);
                  end
                else
                  exprasmlist^.concat(new(paicpu,op_const(A_PUSH,opsize,0)));
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
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,hreg,r)));
                  end
                else
                  exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,opsize,hreg)));
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
                   (exprasmlist^.first=exprasmlist^.last) then
                  exprasmlist^.concat(new(pai_force_line,init));
{$endif GDB}
                if inlined then
                  begin
                     r:=new_reference(procinfo^.framepointer,para_offset-pushedparasize);
                     exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVQ,S_NO,
                       p.location.register,r)));
                  end
                else
                   begin
                      r:=new_reference(R_ESP,0);
                      exprasmlist^.concat(new(paicpu,op_reg_ref(
                        A_MOVQ,S_NO,p.location.register,r)));
                   end;
             end;
{$endif SUPPORT_MMX}
        end;
      end;

{*****************************************************************************
                           Emit Functions
*****************************************************************************}

    procedure maketojumpbool(p : tnode);
    {
      produces jumps to true respectively false labels using boolean expressions
    }
      var
        opsize : topsize;
        storepos : tfileposinfo;
      begin
         if nf_error in p.flags then
           exit;
         storepos:=aktfilepos;
         aktfilepos:=p.fileinfo;
         if is_boolean(p.resulttype) then
           begin
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
                   opsize:=def_opsize(p.resulttype);
                   case p.location.loc of
                      LOC_CREGISTER,LOC_REGISTER : begin
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
         hl : pasmlabel;
      begin
         if not(cs_check_overflow in aktlocalswitches) then
          exit;
         getlabel(hl);
         if not ((p.resulttype^.deftype=pointerdef) or
                ((p.resulttype^.deftype=orddef) and
                 (porddef(p.resulttype)^.typ in [u64bit,u16bit,u32bit,u8bit,uchar,
                                                  bool8bit,bool16bit,bool32bit]))) then
           emitjmp(C_NO,hl)
         else
           emitjmp(C_NB,hl);
         emitcall('FPC_OVERFLOW');
         emitlab(hl);
      end;

    { produces range check code, while one of the operands is a 64 bit
      integer }
    procedure emitrangecheck64(p : tnode;todef : pdef);
      var
        neglabel,
        poslabel,
        endlabel: pasmlabel;
        href   : preference;
        hreg   : tregister;
        hdef   :  porddef;
        fromdef : pdef;
        oldregisterdef: boolean;
        from_signed,to_signed: boolean;

      begin
         fromdef:=p.resulttype;
         if is_64bitint(todef) then
           CGMessage(cg_w_64bit_range_check_not_supported)
         else
           begin
             oldregisterdef := registerdef;
             registerdef := false;

             from_signed := is_signed(fromdef);
             to_signed := is_signed(todef);
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
                 emit_const_reg(A_CMP,S_L,$ffffffff,hreg);
                 emitjmp(C_E,neglabel);
               end;
             if hreg = R_EDI then
               ungetregister32(hreg);
             { For all other values we have a range check error }
             emitcall('FPC_RANGEERROR');

             { if the high dword = 0, the low dword can be considered a }
             { simple cardinal                                          }
             emitlab(poslabel);
             new(hdef,init(u32bit,0,$ffffffff));
             { the real p.resulttype is already saved in fromdef }
             p.resulttype := hdef;
             emitrangecheck(p,todef);
             dispose(hdef,done);
             { restore original resulttype }
             p.resulttype := todef;

             if from_signed and to_signed then
               begin
                 getlabel(endlabel);
                 emitjmp(C_NO,endlabel);
                 { if the high dword = $ffffffff, then the low dword (when }
                 { considered as a longint) must be < 0 (JM)               }
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
                 new(hdef,init(s32bit,$80000000,-1));
                 p.resulttype := hdef;
                 emitrangecheck(p,todef);
                 dispose(hdef,done);
                 emitlab(endlabel);
                 { restore p's resulttype }
                 p.resulttype := fromdef;
               end;
             registerdef := oldregisterdef;
           end;
      end;

     { produces if necessary rangecheckcode }
     procedure emitrangecheck(p:tnode;todef:pdef);
     {
       generate range checking code for the value at location t. The
       type used is the checked against todefs ranges. fromdef (p.resulttype)
       is the original type used at that location, when both defs are
       equal the check is also insert (needed for succ,pref,inc,dec)
     }
      var
        neglabel,
        poslabel : pasmlabel;
        href   : treference;
        rstr   : string;
        hreg   : tregister;
        opsize : topsize;
        op     : tasmop;
        fromdef : pdef;
        lto,hto,
        lfrom,hfrom : longint;
        doublebound,
        is_reg,
        popecx : boolean;
      begin
        { range checking on and range checkable value? }
        if not(cs_check_range in aktlocalswitches) or
           not(todef^.deftype in [orddef,enumdef,arraydef]) then
          exit;
        { only check when assigning to scalar, subranges are different,
          when todef=fromdef then the check is always generated }
        fromdef:=p.resulttype;
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
           getrange(p.resulttype,lfrom,hfrom);
           { first check for not being u32bit, then if the to is bigger than
             from }
           if (lto<hto) and (lfrom<hfrom) and
              (lto<=lfrom) and (hto>=hfrom) then
            exit;
         end;
        { generate the rangecheck code for the def where we are going to
          store the result }
        doublebound:=false;
        case todef^.deftype of
          orddef :
            begin
              porddef(todef)^.genrangecheck;
              rstr:=porddef(todef)^.getrangecheckstring;
              doublebound:=
                ((porddef(todef)^.typ=u32bit) and (lto>hto)) or
                (is_signed(todef) and (porddef(fromdef)^.typ=u32bit)) or
                (is_signed(fromdef) and (porddef(todef)^.typ=u32bit));
            end;
          enumdef :
            begin
              penumdef(todef)^.genrangecheck;
              rstr:=penumdef(todef)^.getrangecheckstring;
            end;
          arraydef :
            begin
              parraydef(todef)^.genrangecheck;
              rstr:=parraydef(todef)^.getrangecheckstring;
              doublebound:=(lto>hto);
            end;
        end;
      { get op and opsize }
        opsize:=def2def_opsize(fromdef,u32bitdef);
        if opsize in [S_B,S_W,S_L] then
         op:=A_MOV
        else
         if is_signed(fromdef) then
          op:=A_MOVSX
         else
          op:=A_MOVZX;
        is_reg:=(p.location.loc in [LOC_REGISTER,LOC_CREGISTER]);
        if is_reg then
          hreg:=p.location.register;
        if not target_os.use_bound_instruction then
         begin
           { FPC_BOUNDCHECK needs to be called with
              %ecx - value
              %edi - pointer to the ranges }
           popecx:=false;
           if not(is_reg) or
              (p.location.register<>R_ECX) then
            begin
              if not(R_ECX in unused) then
               begin
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,R_ECX)));
                 popecx:=true;
               end
                 else exprasmlist^.concat(new(pairegalloc,alloc(R_ECX)));
              if is_reg then
               emit_reg_reg(op,opsize,p.location.register,R_ECX)
              else
               emit_ref_reg(op,opsize,newreference(p.location.reference),R_ECX);
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              emit_reg_reg(A_OR,S_L,R_ECX,R_ECX);
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           getexplicitregister32(R_EDI);
           exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),0,R_EDI)));
           emitcall('FPC_BOUNDCHECK');
           ungetregister32(R_EDI);
           { u32bit needs 2 checks }
           if doublebound then
            begin
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              { if a cardinal is > $7fffffff, this is an illegal longint }
              { value (and vice versa)! (JM)                             }
              if ((todef^.deftype = orddef) and
                  ((is_signed(todef) and (porddef(fromdef)^.typ=u32bit)) or
                   (is_signed(fromdef) and (porddef(todef)^.typ=u32bit)))) or
                 { similar for array indexes (JM) }
                 ((todef^.deftype = arraydef) and
                  (((lto < 0) and (porddef(fromdef)^.typ=u32bit)) or
                   ((lto >= 0) and is_signed(fromdef)))) then
                emitcall('FPC_RANGEERROR')
              else
                begin
                  getexplicitregister32(R_EDI);
                  exprasmlist^.concat(new(paicpu,op_sym_ofs_reg(A_MOV,S_L,newasmsymbol(rstr),8,R_EDI)));
                  emitcall('FPC_BOUNDCHECK');
                  ungetregister32(R_EDI);
                end;
              emitlab(poslabel);
            end;
           if popecx then
            exprasmlist^.concat(new(paicpu,op_reg(A_POP,S_L,R_ECX)))
           else exprasmlist^.concat(new(pairegalloc,dealloc(R_ECX)));
         end
        else
         begin
           reset_reference(href);
           href.symbol:=newasmsymbol(rstr);
           { load the value in a register }
           if is_reg then
            begin
              { be sure that hreg is a 32 bit reg, if not load it in %edi }
              if p.location.register in [R_EAX..R_EDI] then
               hreg:=p.location.register
              else
               begin
                 getexplicitregister32(R_EDI);
                 emit_reg_reg(op,opsize,p.location.register,R_EDI);
                 hreg:=R_EDI;
               end;
            end
           else
            begin
              getexplicitregister32(R_EDI);
              emit_ref_reg(op,opsize,newreference(p.location.reference),R_EDI);
              hreg:=R_EDI;
            end;
           if doublebound then
            begin
              getlabel(neglabel);
              getlabel(poslabel);
              emit_reg_reg(A_TEST,S_L,hreg,hreg);
              emitjmp(C_L,neglabel);
            end;
           { insert bound instruction only }
           exprasmlist^.concat(new(paicpu,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
           { u32bit needs 2 checks }
           if doublebound then
            begin
              href.offset:=8;
              emitjmp(C_None,poslabel);
              emitlab(neglabel);
              exprasmlist^.concat(new(paicpu,op_reg_ref(A_BOUND,S_L,hreg,newreference(href))));
              emitlab(poslabel);
            end;
           if hreg = R_EDI then
             ungetregister32(R_EDI);
         end;
      end;


   { DO NOT RELY on the fact that the tnode is not yet swaped
     because of inlining code PM }
    procedure firstcomplex(p : tbinarynode);
      var
         hp : tnode;
      begin
         { always calculate boolean AND and OR from left to right }
         if (p.nodetype in [orn,andn]) and
            (p.left.resulttype^.deftype=orddef) and
            (porddef(p.left.resulttype)^.typ in [bool8bit,bool16bit,bool32bit]) then
           begin
             { p.swaped:=false}
             if nf_swaped in p.flags then
               internalerror(234234);
           end
         else
           if (p.left.registers32<p.right.registers32) and
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
      begin
        if is_open_string(p.resulttype) then
         begin
           getsymonlyin(tloadnode(p).symtable,'high'+pvarsym(tloadnode(p).symtableentry)^.name);
           hightree:=genloadnode(pvarsym(srsym),tloadnode(p).symtable);
           firstpass(hightree);
           secondpass(hightree);
           push_value_para(hightree,false,false,0,4);
           hightree.free;
           hightree:=nil;
         end
        else
         begin
           push_int(pstringdef(p.resulttype)^.len);
         end;
      end;

{*****************************************************************************
                           String functions
*****************************************************************************}

    procedure loadshortstring(source,dest : tnode);
    {
      Load a string, handles stringdef and orddef (char) types
    }
      begin
         case source.resulttype^.deftype of
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
                     maybe_loadesi;
                   end;
              end;
            orddef:
              begin
                 if source.nodetype=ordconstn then
                   emit_const_ref(
                      A_MOV,S_W,tordconstnode(source).value*256+1,newreference(dest.location.reference))
                 else
                   begin
                      { not so elegant (goes better with extra register }
{$ifndef noAllocEdi}
                      getexplicitregister32(R_EDI);
{$endif noAllocEdi}
                      if (source.location.loc in [LOC_REGISTER,LOC_CREGISTER]) then
                        begin
                           emit_reg_reg(A_MOV,S_L,makereg32(source.location.register),R_EDI);
                           ungetregister(source.location.register);
                        end
                      else
                        begin
                           emit_ref_reg(A_MOV,S_L,newreference(source.location.reference),R_EDI);
                           del_reference(source.location.reference);
                        end;
                      emit_const_reg(A_SHL,S_L,8,R_EDI);
                      emit_const_reg(A_OR,S_L,1,R_EDI);
                      emit_reg_ref(A_MOV,S_W,R_DI,newreference(dest.location.reference));
{$ifndef noAllocEdi}
                      ungetregister32(R_EDI);
{$endif noAllocEdi}
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
         case p.right.resulttype^.deftype of
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
                     maybe_loadesi;
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
         maybe_loadesi;
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
                 exprasmlist^.concat(new(paicpu,op_reg(A_PUSH,S_L,p.right.location.register)));
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
         maybe_loadesi;
         popusedregisters(pushed);
         if ungettemp then
           ungetiftemp(p.right.location.reference);
      end;



end.
{
  $Log$
  Revision 1.6  2000-12-05 11:44:34  jonas
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
