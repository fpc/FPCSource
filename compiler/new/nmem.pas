{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

    This unit implements load nodes etc.

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
unit nmem;

  interface

    uses
       tree,symtable;

    type
       ploadnode = ^tloadnode;
       tloadnode = object(tnode)
          symtableentry : psym;
          symtable : psymtable;
          is_absolute,is_first,is_methodpointer : boolean;
          constructor init(v : psym;st : psymtable);
          destructor done;virtual;
          procedure det_temp;virtual;
          procedure det_resulttype;virtual;
          procedure secondpass;virtual;
       end;

       tassigntyp = (at_normal,at_plus,at_minus,at_star,at_slash);

       passignmentnode = ^tassignmentnode;
       tassignmentnode = object(tbinarynode)
          assigntyp : tassigntyp;
          concat_string : boolean;
          constructor init(l,r : pnode);
          destructor done;virtual;
          procedure det_temp;virtual;
          procedure det_resulttype;virtual;
          procedure secondpass;virtual;
          procedure loadansistring;
          procedure loadshortstring;
          procedure loadansi2short(l,r : pnode);
       end;

    var
       { this is necessary for the const section }
       simple_loadn : boolean;

  implementation

    uses
       cobjects,globals,aasm,cgbase,cgobj,types,verbose,tgobj,tgcpu,symconst,
       cpubase,cpuasm,ncon;

{****************************************************************************
                                 TLOADNODE
 ****************************************************************************}

    constructor tloadnode.init(v : psym;st : psymtable);

      var
         p : ptree;

      begin
         inherited init;
         treetype:=loadn;
         if v^.typ=varsym then
           resulttype:=pvarsym(v)^.vartype.def;
         symtableentry:=v;
         symtable:=st;
         is_first := False;
         is_methodpointer:=false;

         { method pointer load nodes can use the left subtree }
         { !!!!! left:=nil; }
      end;

    destructor tloadnode.done;

      begin
         inherited done;
         { method pointer load nodes can use the left subtree }
         { !!!!! dispose(left,done); }
      end;

    procedure tloadnode.det_temp;

      begin
      end;

    procedure tloadnode.det_resulttype;

      begin
      end;

    procedure tloadnode.secondpass;

      var
         hregister : tregister;
         symtabletype : tsymtabletype;
         i : longint;
         hp : preference;

      begin
         simple_loadn:=true;
         reset_reference(location.reference);
         case symtableentry^.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    if (pabsolutesym(symtableentry)^.abstyp=toaddr) then
                     begin
{$ifdef i386}
                       { absseg is go32v2 target specific }
                       if pabsolutesym(symtableentry)^.absseg then
                        location.reference.segment:=R_FS;
{$endif i386}
                       location.reference.offset:=pabsolutesym(symtableentry)^.address;
                     end
                    else
                     location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                 end;
              varsym :
                 begin
                    hregister:=R_NO;
                    { C variable }
                    if (vo_is_C_var in pvarsym(symtableentry)^.varoptions) then
                      begin
                         location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                      end

{$ifdef dummy}
                    { DLL variable, DLL variables are only available on the win32 target }
                    { maybe we've to add this later for the alpha WinNT                  }
                    else if vo_is_dll_var in pvarsym(symtableentry)^.varoptions then
                      begin
                         hregister:=tg.getregisterint;
                         location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                         exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,newreference(location.reference),hregister)));
                         location.reference.symbol:=nil;
                         location.reference.base:=hregister;
                      end
{$endif dummy}
                    else
                      begin
{$ifdef dummy}
                         symtabletype:=symtable^.symtabletype;
                         { in case it is a register variable: }
                         if pvarsym(symtableentry)^.reg<>R_NO then
                           begin
                              if pvarsym(symtableentry)^.reg in fpuregs then
                                begin
                                   location.loc:=LOC_CFPUREGISTER;
                                   tg.unusedregsfpu:=tg.unusedregsfpu-[pvarsym(symtableentry)^.reg];
                                end
                              else
                                begin
                                   location.loc:=LOC_CREGISTER;
                                   tg.unusedregsint:=tg.unusedregsint-[pvarsym(symtableentry)^.reg];
                                end;
                              location.register:=pvarsym(symtableentry)^.reg;
                           end
                         else
                           begin
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   location.reference.base:=procinfo.framepointer;
                                   location.reference.offset:=pvarsym(symtableentry)^.address;
                                   if (symtabletype in [localsymtable,inlinelocalsymtable]) and
                                     not(use_esp_stackframe) then
                                     location.reference.offset:=-location.reference.offset;
                                   if (lexlevel>(symtable^.symtablelevel)) then
                                     begin
                                        hregister:=tg.getregisterint;

                                        { make a reference }
                                        hp:=new_reference(procinfo^.framepointer,
                                          procinfo^.framepointer_offset);


                                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,hp,hregister)));

                                        simple_loadn:=false;
                                        i:=lexlevel-1;
                                        while i>(symtable^.symtablelevel) do
                                          begin
                                             { make a reference }
                                             hp:=new_reference(hregister,8);
                                             exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,hp,hregister)));
                                             dec(i);
                                          end;
                                        location.reference.base:=hregister;
                                     end;
                                end
                              else
                                case symtabletype of
                                   unitsymtable,globalsymtable,
                                   staticsymtable : begin
                                                       location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                                                    end;
                                   stt_exceptsymtable:
                                     begin
                                        location.reference.base:=procinfo^.framepointer;
                                        location.reference.offset:=pvarsym(symtableentry)^.address;
                                     end;
                                   objectsymtable:
                                     begin
                                        if sp_static in pvarsym(symtableentry)^.symoptions then
                                          begin
                                             location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                                          end
                                        else
                                          begin
                                             location.reference.base:=self_pointer;
                                             location.reference.offset:=pvarsym(symtableentry)^.address;
                                          end;
                                     end;
                                   withsymtable:
                                     begin
                                        hregister:=tg.getregisterint;
                                        location.reference.base:=hregister;
                                        { make a reference }
                                        { symtable datasize field
                                          contains the offset of the temp
                                          stored }
                                        hp:=new_reference(procinfo^.framepointer,
                                          symtable^.datasize);

                                        exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,hp,hregister)));

                                        location.reference.offset:=
                                          pvarsym(symtableentry)^.address;
                                     end;
                                end;
                           end;
                         { in case call by reference, then calculate: }
                         if (pvarsym(symtableentry)^.varspez=vs_var) or
                            is_open_array(pvarsym(symtableentry)^.definition) or
                            is_array_of_const(pvarsym(symtableentry)^.definition) or
                            ((pvarsym(symtableentry)^.varspez=vs_const) and
                             push_addr_param(pvarsym(symtableentry)^.definition)) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=tg.getregisterint;
                              if is_open_array(pvarsym(symtableentry)^.definition) or
                                 is_open_string(pvarsym(symtableentry)^.definition) then
                                begin
                                   if (location.reference.base=procinfo^.framepointer) then
                                     begin
                                        highframepointer:=location.reference.base;
                                        highoffset:=location.reference.offset;
                                     end
                                   else
                                     begin
                                        highframepointer:=R_EDI;
                                        highoffset:=location.reference.offset;
                                        exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOV,S_L,
                                          location.reference.base,R_EDI)));
                                     end;
                                end;
                              if location.loc=LOC_CREGISTER then
                                begin
                                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOV,S_L,
                                     location.register,hregister)));
                                   location.loc:=LOC_REFERENCE;
                                end
                              else
                                begin
                                   exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,S_L,
                                     newreference(location.reference),
                                     hregister)));
                                end;
                              reset_reference(location.reference);
                              location.reference.base:=hregister;
                          end;
{$endif dummy}
                      end;
                 end;
              procsym:
                 begin
                    {!!!!!!!!!!}
                 end;
              typedconstsym :
                 begin
                    location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                 end;
              else internalerror(4);
         end;
      end;

{****************************************************************************
                            TASSIGNMENTNODE
 ****************************************************************************}

    constructor tassignmentnode.init(l,r : pnode);

      begin
         inherited init(l,r);
         concat_string:=false;
         assigntyp:=at_normal;
      end;

    destructor tassignmentnode.done;

      begin
         inherited done;
      end;

    procedure tassignmentnode.loadansistring;

      begin
         abstract;
      end;

    procedure tassignmentnode.loadshortstring;

      begin
         abstract;
      end;

    procedure tassignmentnode.loadansi2short(l,r : pnode);

      begin
         abstract;
      end;

    procedure tassignmentnode.det_temp;

      begin
{$ifdef dummy}
         store_valid:=must_be_valid;
         must_be_valid:=false;

         { must be made unique }
         set_unique(p^.left);

         firstpass(p^.left);
         if codegenerror then
           exit;

         { test if we can avoid copying string to temp
           as in s:=s+...; (PM) }
         must_be_valid:=true;
         firstpass(p^.right);
         must_be_valid:=store_valid;
         if codegenerror then
           exit;

         { some string functions don't need conversion, so treat them separatly }
         if is_shortstring(p^.left^.resulttype) and (assigned(p^.right^.resulttype)) then
          begin
            if not (is_shortstring(p^.right^.resulttype) or
                    is_ansistring(p^.right^.resulttype) or
                    is_char(p^.right^.resulttype)) then
             begin
               p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
               firstpass(p^.right);
               if codegenerror then
                exit;
             end;
            { we call STRCOPY }
            procinfo.flags:=procinfo.flags or pi_do_call;
            hp:=p^.right;
          end
         else
          begin
            p^.right:=gentypeconvnode(p^.right,p^.left^.resulttype);
            firstpass(p^.right);
            if codegenerror then
             exit;
          end;

         { set assigned flag for varsyms }
         if (p^.left^.treetype=loadn) and
            (p^.left^.symtableentry^.typ=varsym) and
            (pvarsym(p^.left^.symtableentry)^.varstate=vs_declared) then
           pvarsym(p^.left^.symtableentry)^.varstate:=vs_assigned;

         p^.registersint:=p^.left^.registersint+p^.right^.registersint;
         p^.registersfpu:=max(p^.left^.registersfpu,p^.right^.registersfpu);
         p^.registersmm:=max(p^.left^.registersmm,p^.right^.registersmm);
{$endif dummy}
      end;

    procedure tassignmentnode.det_resulttype;

      begin
         inherited det_resulttype;
         resulttype:=voiddef;
         { assignements to open arrays aren't allowed }
         if is_open_array(left^.resulttype) then
           CGMessage(type_e_mismatch);
      end;

    procedure tassignmentnode.secondpass;

      var
         r : treference;
         opsize : tcgsize;

      begin
         if left^.resulttype^.deftype=stringdef then
           begin
              if is_ansistring(left^.resulttype) then
                begin
                  { the source and destinations are released
                    in loadansistring, because an ansi string can
                    also be in a register
                  }
                  loadansistring;
                end
              else
              if is_shortstring(left^.resulttype) then
                begin
                  if is_ansistring(right^.resulttype) then
                    begin
                      if (right^.treetype=stringconstn) and
                         (pstringconstnode(right)^.length=0) then
                        begin
                           cg^.a_load_const_ref(list,OS_8,0,left^.location.reference);
                           tg.del_reference(left^.location.reference);
                        end
                      else
                        loadansi2short(right,left);
                    end
                  else
                    begin
                       { we do not need destination anymore }
                       tg.del_reference(left^.location.reference);
                       tg.del_reference(right^.location.reference);
                       loadshortstring;
                       tg.ungetiftemp(right^.location.reference);
                    end;
                end
              else if is_longstring(left^.resulttype) then
                begin
                   abstract;
                end
              else
                begin
                  { its the only thing we have to do }
                  tg.del_reference(right^.location.reference);
                end
           end
        else case right^.location.loc of
            LOC_REFERENCE,
            LOC_MEM : begin
{$ifdef dummy}
                         { extra handling for ordinal constants }
                         if (right^.treetype in [ordconstn,fixconstn]) or
                            (loc=LOC_CREGISTER) then
                           begin
                              case p^.left^.resulttype^.size of
                                 1 : opsize:=OS_B;
                                 2 : opsize:=OS_W;
                                 4 : opsize:=OS_L;
                                 { S_L is correct, the copy is done }
                                 { with two moves                   }
                                 8 : opsize:=OS_L;
                              end;
                              if loc=LOC_CREGISTER then
                                begin
                                  exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,opsize,
                                    newreference(p^.right^.location.reference),
                                    p^.left^.location.register)));
                                  if is_64bitint(p^.right^.resulttype) then
                                    begin
                                       r:=newreference(p^.right^.location.reference);
                                       inc(r^.offset,4);
                                       exprasmlist^.concat(new(paicpu,op_ref_reg(A_MOV,opsize,r,
                                         p^.left^.location.registerhigh)));
                                    end;
{$IfDef regallocfix}
                                  del_reference(p^.right^.location.reference);
{$EndIf regallocfix}
                                end
                              else
                                begin
                                  exprasmlist^.concat(new(paicpu,op_const_ref(A_MOV,opsize,
                                    p^.right^.location.reference.offset,
                                    newreference(p^.left^.location.reference))));
                                  if is_64bitint(p^.right^.resulttype) then
                                    begin
                                       r:=newreference(p^.left^.location.reference);
                                       inc(r^.offset,4);
                                       exprasmlist^.concat(new(paicpu,op_const_ref(A_MOV,opsize,
                                         0,r)));
                                    end;
{$IfDef regallocfix}
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                {exprasmlist^.concat(new(paicpu,op_const_loc(A_MOV,opsize,
                                    p^.right^.location.reference.offset,
                                    p^.left^.location)));}
                                end;

                           end
                         else if loc=LOC_CFPUREGISTER then
                           begin
                              floatloadops(pfloatdef(p^.right^.resulttype)^.typ,op,opsize);
                              exprasmlist^.concat(new(paicpu,op_ref(op,opsize,
                                newreference(p^.right^.location.reference))));
                              exprasmlist^.concat(new(paicpu,op_reg(A_FSTP,S_NO,
                                correct_fpuregister(p^.left^.location.register,fpuvaroffset+1))));
                           end
                         else
{$endif dummy}
                           begin
                              if (right^.resulttype^.needs_inittable) and
                                ( (right^.resulttype^.deftype<>objectdef) or
                                  not(pobjectdef(right^.resulttype)^.is_class)) then
                                begin
                                   { this would be a problem }
                                   if not(left^.resulttype^.needs_inittable) then
                                     internalerror(3457);

                                   { increment source reference counter }
                                   r.symbol:=right^.resulttype^.get_inittable_label;
                                   cg^.a_param_ref_addr(list,r,2);
                                   cg^.a_param_ref_addr(list,right^.location.reference,1);
                                   cg^.a_call_name(list,'FPC_ADDREF',0);
                                   { decrement destination reference counter }
                                   r.symbol:=left^.resulttype^.get_inittable_label;
                                   cg^.a_param_ref_addr(list,r,2);
                                   cg^.a_param_ref_addr(list,left^.location.reference,1);
                                   cg^.a_call_name(list,'FPC_DECREF',0)
                                end;
                              cg^.g_concatcopy(list,right^.location.reference,
                                left^.location.reference,left^.resulttype^.size,false);
                              tg.ungetiftemp(right^.location.reference);
                           end;

                      end;
                 end;   { needs to be removed together with the dummy }
{$ifdef dummy}
{$ifdef SUPPORT_MMX}
            LOC_CMMXREGISTER,
            LOC_MMXREGISTER:
              begin
                 if loc=LOC_CMMXREGISTER then
                   exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOVQ,S_NO,
                   p^.right^.location.register,p^.left^.location.register)))
                 else
                   exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOVQ,S_NO,
                     p^.right^.location.register,newreference(p^.left^.location.reference))));
              end;
{$endif SUPPORT_MMX}
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              case p^.right^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                                 8 : opsize:=S_L;
                              end;
                              { simplified with op_reg_loc       }
                              if loc=LOC_CREGISTER then
                                begin
                                  exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOV,opsize,
                                    p^.right^.location.register,
                                    p^.left^.location.register)));
{$IfDef regallocfix}
                                 ungetregister(p^.right^.location.register);
{$EndIf regallocfix}
                                end
                              else
                                Begin
                                  exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,
                                    p^.right^.location.register,
                                    newreference(p^.left^.location.reference))));
{$IfDef regallocfix}
                                  ungetregister(p^.right^.location.register);
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                end;
                              if is_64bitint(p^.right^.resulttype) then
                                begin
                                   { simplified with op_reg_loc  }
                                   if loc=LOC_CREGISTER then
                                     exprasmlist^.concat(new(paicpu,op_reg_reg(A_MOV,opsize,
                                       p^.right^.location.registerhigh,
                                       p^.left^.location.registerhigh)))
                                   else
                                     begin
                                        r:=newreference(p^.left^.location.reference);
                                        inc(r^.offset,4);
                                        exprasmlist^.concat(new(paicpu,op_reg_ref(A_MOV,opsize,
                                          p^.right^.location.registerhigh,r)));
                                     end;
                                end;
                              {exprasmlist^.concat(new(paicpu,op_reg_loc(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location)));      }

                           end;
            LOC_FPU : begin
                              if (p^.left^.resulttype^.deftype=floatdef) then
                               fputyp:=pfloatdef(p^.left^.resulttype)^.typ
                              else
                               if (p^.right^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.resulttype)^.typ
                              else
                               if (p^.right^.treetype=typeconvn) and
                                  (p^.right^.left^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.left^.resulttype)^.typ
                              else
                                fputyp:=s32real;
                              case loc of
                                 LOC_CFPUREGISTER:
                                   begin
                                      exprasmlist^.concat(new(paicpu,op_reg(A_FSTP,S_NO,
                                        correct_fpuregister(p^.left^.location.register,fpuvaroffset))));
                                      dec(fpuvaroffset);
                                   end;
                                 LOC_REFERENCE:
                                   floatstore(fputyp,p^.left^.location.reference);
                                 else
                                   internalerror(48991);
                              end;
                           end;
            LOC_CFPUREGISTER: begin
                              if (p^.left^.resulttype^.deftype=floatdef) then
                               fputyp:=pfloatdef(p^.left^.resulttype)^.typ
                              else
                               if (p^.right^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.resulttype)^.typ
                              else
                               if (p^.right^.treetype=typeconvn) and
                                  (p^.right^.left^.resulttype^.deftype=floatdef) then
                                fputyp:=pfloatdef(p^.right^.left^.resulttype)^.typ
                              else
                                fputyp:=s32real;
                              exprasmlist^.concat(new(paicpu,op_reg(A_FLD,S_NO,
                                correct_fpuregister(p^.right^.location.register,fpuvaroffset))));
                              inc(fpuvaroffset);
                              case loc of
                                 LOC_CFPUREGISTER:
                                   begin
                                      exprasmlist^.concat(new(paicpu,op_reg(A_FSTP,S_NO,
                                        correct_fpuregister(p^.right^.location.register,fpuvaroffset))));
                                      dec(fpuvaroffset);
                                   end;
                                 LOC_REFERENCE:
                                   floatstore(fputyp,p^.left^.location.reference);
                                 else
                                   internalerror(48992);
                              end;
                           end;
            LOC_JUMP     : begin
                              getlabel(hlabel);
                              emitlab(truelabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(paicpu,op_const_reg(A_MOV,S_B,
                                  1,p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(paicpu,op_const_ref(A_MOV,S_B,
                                  1,newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(paicpu,op_const_loc(A_MOV,S_B,
                                  1,p^.left^.location)));}
                              emitjmp(C_None,hlabel);
                              emitlab(falselabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(paicpu,op_reg_reg(A_XOR,S_B,
                                  p^.left^.location.register,
                                  p^.left^.location.register)))
                              else
                                begin
                                  exprasmlist^.concat(new(paicpu,op_const_ref(A_MOV,S_B,
                                    0,newreference(p^.left^.location.reference))));
{$IfDef regallocfix}
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                 end;
                              emitlab(hlabel);
                           end;
            LOC_FLAGS    : begin
                              if loc=LOC_CREGISTER then
                                emit_flag2reg(p^.right^.location.resflags,p^.left^.location.register)
                              else
                                begin
                                  ai:=new(paicpu,op_ref(A_Setcc,S_B,newreference(p^.left^.location.reference)));
                                  ai^.SetCondition(flag_2_cond[p^.right^.location.resflags]);
                                  exprasmlist^.concat(ai);
                                end;
{$IfDef regallocfix}
                              del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                           end;
            else internalerror(68997);
         end;
{$endif dummy}
      end;

end.
{
  $Log$
  Revision 1.16  2000-01-07 01:14:53  peter
    * updated copyright to 2000

  Revision 1.15  1999/12/06 18:17:10  peter
    * newcg compiler compiles again

  Revision 1.14  1999/10/12 21:20:46  florian
    * new codegenerator compiles again

  Revision 1.13  1999/09/15 20:35:46  florian
    * small fix to operator overloading when in MMX mode
    + the compiler uses now fldz and fld1 if possible
    + some fixes to floating point registers
    + some math. functions (arctan, ln, sin, cos, sqrt, sqr, pi) are now inlined
    * .... ???

  Revision 1.12  1999/09/14 11:16:09  florian
    * only small updates to work with the current compiler

  Revision 1.11  1999/08/25 12:00:12  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.10  1999/08/18 17:05:56  florian
    + implemented initilizing of data for the new code generator
      so it should compile now simple programs

  Revision 1.9  1999/08/06 18:05:54  florian
    * implemented some stuff for assignments

  Revision 1.8  1999/08/06 15:53:51  florian
    * made the alpha version compilable

  Revision 1.7  1999/08/05 17:10:57  florian
    * some more additions, especially procedure
      exit code generation

  Revision 1.6  1999/08/05 14:58:13  florian
    * some fixes for the floating point registers
    * more things for the new code generator

  Revision 1.5  1999/08/04 00:23:56  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.4  1999/08/03 17:09:45  florian
    * the alpha compiler can be compiled now

  Revision 1.3  1999/08/02 17:14:08  florian
    + changed the temp. generator to an object

  Revision 1.2  1999/08/01 18:22:35  florian
   * made it again compilable

  Revision 1.1  1999/01/24 22:32:36  florian
    * well, more changes, especially parts of secondload ported
}
