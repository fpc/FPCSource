{
    $Id$
    Copyright (c) 1993-98 by Florian Klaempfl

    Generate i386 assembler for load/assignment nodes

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
unit cg386ld;
interface

    uses
      tree;

    procedure secondload(var p : ptree);
    procedure secondassignment(var p : ptree);
    procedure secondfuncret(var p : ptree);
    procedure secondarrayconstruct(var p : ptree);


implementation

    uses
      globtype,systems,
      cobjects,verbose,globals,files,
      symconst,symtable,aasm,types,
      hcodegen,temp_gen,pass_2,
      cpubase,cpuasm,
      cgai386,tgeni386,cg386cnv,cresstr;

{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure secondload(var p : ptree);
      var
         hregister : tregister;
         symtabletype : tsymtabletype;
         i : longint;
         hp : preference;
         s : pasmsymbol;
         popeax : boolean;
         pushed : tpushed;
         hr : treference;

      begin
         simple_loadn:=true;
         reset_reference(p^.location.reference);
         case p^.symtableentry^.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    p^.location.reference.symbol:=nil;
                    if (pabsolutesym(p^.symtableentry)^.abstyp=toaddr) then
                     begin
                       if pabsolutesym(p^.symtableentry)^.absseg then
                        p^.location.reference.segment:=R_FS;
                       p^.location.reference.offset:=pabsolutesym(p^.symtableentry)^.address;
                     end
                    else
                     p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                 end;
              constsym:
                begin
                   if pconstsym(p^.symtableentry)^.consttype=constresourcestring then
                     begin
                         pushusedregisters(pushed,$ff);
                         emit_const(A_PUSH,S_L,
                           pconstsym(p^.symtableentry)^.resstrindex);
                         emit_sym(A_PUSH,S_L,newasmsymbol(pconstsym(p^.symtableentry)^.owner^.name^+'_RESOURCESTRINGLIST'));
                         emitcall('FPC_GETRESOURCESTRING');

                         hregister:=getexplicitregister32(R_EAX);
                         emit_reg_reg(A_MOV,S_L,R_EAX,hregister);
                         gettempansistringreference(hr);
                         decrstringref(p^.resulttype,hr);
                         emit_reg_ref(A_MOV,S_L,hregister,
                           newreference(hr));
                        ungetregister32(hregister);
                        popusedregisters(pushed);

                        p^.location.loc:=LOC_MEM;
                        p^.location.reference:=hr;
                     end
                   else
                     internalerror(22798);
                end;
              varsym :
                 begin
                    hregister:=R_NO;
                    { C variable }
                    if (vo_is_C_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                      end
                    { DLL variable }
                    else if (vo_is_dll_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         hregister:=getregister32;
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         emit_ref_reg(A_MOV,S_L,newreference(p^.location.reference),hregister);
                         p^.location.reference.symbol:=nil;
                         p^.location.reference.base:=hregister;
                      end
                    { external variable }
                    else if (vo_is_external in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                      end
                    { thread variable }
                    else if (vo_is_thread_var in pvarsym(p^.symtableentry)^.varoptions) then
                      begin
                         popeax:=not(R_EAX in unused);
                         if popeax then
                           emit_reg(A_PUSH,S_L,R_EAX);
                         p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                         emit_ref(A_PUSH,S_L,newreference(p^.location.reference));
                         { the called procedure isn't allowed to change }
                         { any register except EAX                    }
                         emitcall('FPC_RELOCATE_THREADVAR');

                         reset_reference(p^.location.reference);
                         p^.location.reference.base:=getregister32;
                         emit_reg_reg(A_MOV,S_L,R_EAX,p^.location.reference.base);
                         if popeax then
                           emit_reg(A_POP,S_L,R_EAX);

                      end
                    { normal variable }
                    else
                      begin
                         symtabletype:=p^.symtable^.symtabletype;
                         { in case it is a register variable: }
                         if pvarsym(p^.symtableentry)^.reg<>R_NO then
                           begin
                              if pvarsym(p^.symtableentry)^.reg in [R_ST0..R_ST7] then
                                begin
                                   p^.location.loc:=LOC_CFPUREGISTER;
                                   p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                                end
                              else
                                begin
                                   p^.location.loc:=LOC_CREGISTER;
                                   p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                                   unused:=unused-[pvarsym(p^.symtableentry)^.reg];
                                end;
                           end
                         else
                           begin
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   p^.location.reference.base:=procinfo^.framepointer;
                                   p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address+p^.symtable^.address_fixup;

                                   if (symtabletype in [localsymtable,inlinelocalsymtable]) then
                                     begin
                                        if use_esp_stackframe then
                                          dec(p^.location.reference.offset,
                                            pvarsym(p^.symtableentry)^.getsize)
                                        else
                                          p^.location.reference.offset:=-p^.location.reference.offset;
                                     end;
                                   if (lexlevel>(p^.symtable^.symtablelevel)) then
                                     begin
                                        hregister:=getregister32;

                                        { make a reference }
                                        hp:=new_reference(procinfo^.framepointer,
                                          procinfo^.framepointer_offset);

                                        emit_ref_reg(A_MOV,S_L,hp,hregister);

                                        simple_loadn:=false;
                                        i:=lexlevel-1;
                                        while i>(p^.symtable^.symtablelevel) do
                                          begin
                                             { make a reference }
                                             hp:=new_reference(hregister,8);
                                             emit_ref_reg(A_MOV,S_L,hp,hregister);
                                             dec(i);
                                          end;
                                        p^.location.reference.base:=hregister;
                                     end;
                                end
                              else
                                case symtabletype of
                                   unitsymtable,globalsymtable,
                                   staticsymtable :
                                     begin
                                       p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                                     end;
                                   stt_exceptsymtable:
                                     begin
                                        p^.location.reference.base:=procinfo^.framepointer;
                                        p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                     end;
                                   objectsymtable:
                                     begin
                                        if (sp_static in pvarsym(p^.symtableentry)^.symoptions) then
                                          begin
                                             p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                                          end
                                        else
                                          begin
                                             p^.location.reference.base:=R_ESI;
                                             p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                          end;
                                     end;
                                   withsymtable:
                                     begin
                                        { make a reference }
                                        { symtable datasize field
                                          contains the offset of the temp
                                          stored }
{                                       hp:=new_reference(procinfo^.framepointer,
                                          p^.symtable^.datasize);

                                        emit_ref_reg(A_MOV,S_L,hp,hregister);}

                                        if ptree(pwithsymtable(p^.symtable)^.withnode)^.islocal then
                                         begin
                                           p^.location.reference:=ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^;
                                         end
                                        else
                                         begin
                                           hregister:=getregister32;
                                           p^.location.reference.base:=hregister;
                                           emit_ref_reg(A_MOV,S_L,
                                             newreference(ptree(pwithsymtable(p^.symtable)^.withnode)^.withreference^),
                                             hregister);
                                         end;
                                        inc(p^.location.reference.offset,pvarsym(p^.symtableentry)^.address);
                                     end;
                                end;
                           end;
                         { in case call by reference, then calculate. Open array
                           is always an reference! }
                         if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                            is_open_array(pvarsym(p^.symtableentry)^.definition) or
                            is_array_of_const(pvarsym(p^.symtableentry)^.definition) or
                            ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                             push_addr_param(pvarsym(p^.symtableentry)^.definition)) then
                           begin
                              simple_loadn:=false;
                              if hregister=R_NO then
                                hregister:=getregister32;
                              if p^.location.loc=LOC_CREGISTER then
                                begin
                                   emit_reg_reg(A_MOV,S_L,
                                     p^.location.register,hregister);
                                   p^.location.loc:=LOC_REFERENCE;
                                end
                              else
                                begin
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(p^.location.reference),
                                     hregister);
                                end;
                              reset_reference(p^.location.reference);
                              p^.location.reference.base:=hregister;
                          end;
                      end;
                 end;
              procsym:
                 begin
                    if assigned(p^.left) then
                      begin
                         secondpass(p^.left);
                         p^.location.loc:=LOC_MEM;
                         gettempofsizereference(8,p^.location.reference);

                         { load class instance address }
                         case p^.left^.location.loc of

                            LOC_CREGISTER,
                            LOC_REGISTER:
                              begin
                                 hregister:=p^.left^.location.register;
                                 ungetregister32(p^.left^.location.register);
                                 if not(pobjectdef(p^.left^.resulttype)^.is_class) then
                                   CGMessage(cg_e_illegal_expression);
                              end;

                            LOC_MEM,
                            LOC_REFERENCE:
                              begin
                                 hregister:=R_EDI;
                                 if pobjectdef(p^.left^.resulttype)^.is_class then
                                   emit_ref_reg(A_MOV,S_L,
                                     newreference(p^.left^.location.reference),R_EDI)
                                 else
                                   emit_ref_reg(A_LEA,S_L,
                                     newreference(p^.left^.location.reference),R_EDI);
                                 del_reference(p^.left^.location.reference);
                                 ungetiftemp(p^.left^.location.reference);
                              end;
                            else internalerror(26019);
                         end;

                         { store the class instance address }
                         new(hp);
                         hp^:=p^.location.reference;
                         inc(hp^.offset,4);
                         emit_reg_ref(A_MOV,S_L,
                           hregister,hp);

                         { virtual method ? }
                         if (po_virtualmethod in pprocsym(p^.symtableentry)^.definition^.procoptions) then
                           begin
                              new(hp);
                              reset_reference(hp^);
                              hp^.base:=hregister;
                              { load vmt pointer }
                              emit_ref_reg(A_MOV,S_L,
                                hp,R_EDI);
{$IfDef regallocfix}
                              del_reference(hp^);
{$EndIf regallocfix}
                              { load method address }
                              new(hp);
                              reset_reference(hp^);
                              hp^.base:=R_EDI;
                              hp^.offset:=pprocsym(p^.symtableentry)^.definition^._class^.vmtmethodoffset(
                                pprocsym(p^.symtableentry)^.definition^.extnumber);
                              emit_ref_reg(A_MOV,S_L,
                                hp,R_EDI);
                              { ... and store it }
                              emit_reg_ref(A_MOV,S_L,
                                R_EDI,newreference(p^.location.reference));
                           end
                         else
                           begin
                              s:=newasmsymbol(pprocsym(p^.symtableentry)^.definition^.mangledname);
                              emit_sym_ofs_ref(A_MOV,S_L,s,0,
                                newreference(p^.location.reference));
                           end;
                      end
                    else
                      begin
                         {!!!!! Be aware, work on virtual methods too }
                         p^.location.reference.symbol:=newasmsymbol(pprocsym(p^.symtableentry)^.definition^.mangledname);
                      end;
                 end;
              typedconstsym :
                 begin
                    p^.location.reference.symbol:=newasmsymbol(p^.symtableentry^.mangledname);
                 end;
              else internalerror(4);
         end;
      end;


{*****************************************************************************
                             SecondAssignment
*****************************************************************************}

    procedure secondassignment(var p : ptree);
      var
         opsize : topsize;
         otlabel,hlabel,oflabel : pasmlabel;
         fputyp : tfloattype;
         loc : tloc;
         r : preference;
         ai : paicpu;
         op : tasmop;
         pushed : boolean;

      begin
         otlabel:=truelabel;
         oflabel:=falselabel;
         getlabel(truelabel);
         getlabel(falselabel);
         { calculate left sides }
         if not(p^.concat_string) then
           secondpass(p^.left);

         if codegenerror then
           exit;

{$ifdef dummy}
         { we use now the standard mechanism via maybe_push/restore
           to do that (FK)
         }
         case p^.left^.location.loc of
            LOC_REFERENCE : begin
                              { in case left operator uses to register }
                              { but to few are free then LEA }
                              if (p^.left^.location.reference.base<>R_NO) and
                                 (p^.left^.location.reference.index<>R_NO) and
                                 (usablereg32<p^.right^.registers32) then
                                begin
                                   del_reference(p^.left^.location.reference);
                                   hregister:=getregister32;
                                   emit_ref_reg(A_LEA,S_L,newreference(
                                     p^.left^.location.reference),
                                     hregister);
                                   reset_reference(p^.left^.location.reference);
                                   p^.left^.location.reference.base:=hregister;
                                   p^.left^.location.reference.index:=R_NO;
                                end;
                              loc:=LOC_REFERENCE;
                           end;
            LOC_CFPUREGISTER:
              loc:=LOC_CFPUREGISTER;
            LOC_CREGISTER:
              loc:=LOC_CREGISTER;
            LOC_MMXREGISTER:
              loc:=LOC_MMXREGISTER;
            LOC_CMMXREGISTER:
              loc:=LOC_CMMXREGISTER;
            else
               begin
                  CGMessage(cg_e_illegal_expression);
                  exit;
               end;
         end;
{$endif dummy}
         if not(p^.left^.location.loc in [LOC_REFERENCE,LOC_CFPUREGISTER,
           LOC_CREGISTER,LOC_MMXREGISTER,LOC_CMMXREGISTER]) then
           begin
              CGMessage(cg_e_illegal_expression);
              exit;
           end;


         loc:=p^.left^.location.loc;
         { lets try to optimize this (PM)            }
         { define a dest_loc that is the location      }
         { and a ptree to verify that it is the right }
         { place to insert it                    }
{$ifdef test_dest_loc}
         if (aktexprlevel<4) then
           begin
              dest_loc_known:=true;
              dest_loc:=p^.left^.location;
              dest_loc_tree:=p^.right;
           end;
{$endif test_dest_loc}

         { left can't be never a 64 bit LOC_REGISTER, so the 3. arg }
         { can be false                                             }
         pushed:=maybe_push(p^.right^.registers32,p^.left,false);
         secondpass(p^.right);
         if pushed then restore(p^.left,false);

         if codegenerror then
           exit;

{$ifdef test_dest_loc}
         dest_loc_known:=false;
         if in_dest_loc then
           begin
              truelabel:=otlabel;
              falselabel:=oflabel;
              in_dest_loc:=false;
              exit;
           end;
{$endif test_dest_loc}
         if p^.left^.resulttype^.deftype=stringdef then
           begin
              if is_ansistring(p^.left^.resulttype) then
                begin
                  { the source and destinations are released
                    in loadansistring, because an ansi string can
                    also be in a register
                  }
                  loadansistring(p);
                end
              else
              if is_shortstring(p^.left^.resulttype) and
                not (p^.concat_string) then
                begin
                  if is_ansistring(p^.right^.resulttype) then
                    begin
                      if (p^.right^.treetype=stringconstn) and
                         (p^.right^.length=0) then
                        begin
                          emit_const_ref(A_MOV,S_B,
                            0,newreference(p^.left^.location.reference));
{$IfDef regallocfix}
                          del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                        end
                      else
                        loadansi2short(p^.right,p^.left);
                    end
                  else
                    begin
                       { we do not need destination anymore }
                       del_reference(p^.left^.location.reference);
                       {del_reference(p^.right^.location.reference);
                        done in loadshortstring }
                       loadshortstring(p);
                       ungetiftemp(p^.right^.location.reference);
                    end;
                end
              else if is_longstring(p^.left^.resulttype) then
                begin
                end
              else
                begin
                  { its the only thing we have to do }
                  del_reference(p^.right^.location.reference);
                end
           end
        else case p^.right^.location.loc of
            LOC_REFERENCE,
            LOC_MEM : begin
                         { extra handling for ordinal constants }
                         if (p^.right^.treetype in [ordconstn,fixconstn]) or
                            (loc=LOC_CREGISTER) then
                           begin
                              case p^.left^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                                 { S_L is correct, the copy is done }
                                 { with two moves                   }
                                 8 : opsize:=S_L;
                              end;
                              if loc=LOC_CREGISTER then
                                begin
                                  emit_ref_reg(A_MOV,opsize,
                                    newreference(p^.right^.location.reference),
                                    p^.left^.location.register);
                                  if is_64bitint(p^.right^.resulttype) then
                                    begin
                                       r:=newreference(p^.right^.location.reference);
                                       inc(r^.offset,4);
                                       emit_ref_reg(A_MOV,opsize,r,
                                         p^.left^.location.registerhigh);
                                    end;
{$IfDef regallocfix}
                                  del_reference(p^.right^.location.reference);
{$EndIf regallocfix}
                                end
                              else
                                begin
                                  emit_const_ref(A_MOV,opsize,
                                    p^.right^.location.reference.offset,
                                    newreference(p^.left^.location.reference));
                                  if is_64bitint(p^.right^.resulttype) then
                                    begin
                                       r:=newreference(p^.left^.location.reference);
                                       inc(r^.offset,4);
                                       emit_const_ref(A_MOV,opsize,
                                         0,r);
                                    end;
{$IfDef regallocfix}
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                {emit_const_loc(A_MOV,opsize,
                                    p^.right^.location.reference.offset,
                                    p^.left^.location);}
                                end;

                           end
                         else if loc=LOC_CFPUREGISTER then
                           begin
                              floatloadops(pfloatdef(p^.right^.resulttype)^.typ,op,opsize);
                              emit_ref(op,opsize,
                                newreference(p^.right^.location.reference));
                              emit_reg(A_FSTP,S_NO,
                                correct_fpuregister(p^.left^.location.register,fpuvaroffset+1));
                           end
                         else
                           begin
                              if (p^.right^.resulttype^.needs_inittable) and
                                ( (p^.right^.resulttype^.deftype<>objectdef) or
                                  not(pobjectdef(p^.right^.resulttype)^.is_class)) then
                                begin
                                   { this would be a problem }
                                   if not(p^.left^.resulttype^.needs_inittable) then
                                     internalerror(3457);

                                   { increment source reference counter }
                                   new(r);
                                   reset_reference(r^);
                                   r^.symbol:=p^.right^.resulttype^.get_inittable_label;
                                   emitpushreferenceaddr(r^);

                                   emitpushreferenceaddr(p^.right^.location.reference);
                                   emitcall('FPC_ADDREF');
                                   { decrement destination reference counter }
                                   new(r);
                                   reset_reference(r^);
                                   r^.symbol:=p^.left^.resulttype^.get_inittable_label;
                                   emitpushreferenceaddr(r^);
                                   emitpushreferenceaddr(p^.left^.location.reference);
                                   emitcall('FPC_DECREF');
                                end;

{$ifdef regallocfix}
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,true,false);
                              ungetiftemp(p^.right^.location.reference);
{$Else regallocfix}
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,false,false);
                              ungetiftemp(p^.right^.location.reference);
{$endif regallocfix}
                           end;
                      end;
{$ifdef SUPPORT_MMX}
            LOC_CMMXREGISTER,
            LOC_MMXREGISTER:
              begin
                 if loc=LOC_CMMXREGISTER then
                   emit_reg_reg(A_MOVQ,S_NO,
                   p^.right^.location.register,p^.left^.location.register)
                 else
                   emit_reg_ref(A_MOVQ,S_NO,
                     p^.right^.location.register,newreference(p^.left^.location.reference));
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
                                  emit_reg_reg(A_MOV,opsize,
                                    p^.right^.location.register,
                                    p^.left^.location.register);
{$IfDef regallocfix}
                                 ungetregister(p^.right^.location.register);
{$EndIf regallocfix}
                                end
                              else
                                Begin
                                  emit_reg_ref(A_MOV,opsize,
                                    p^.right^.location.register,
                                    newreference(p^.left^.location.reference));
{$IfDef regallocfix}
                                  ungetregister(p^.right^.location.register);
                                  del_reference(p^.left^.location.reference);
{$EndIf regallocfix}
                                end;
                              if is_64bitint(p^.right^.resulttype) then
                                begin
                                   { simplified with op_reg_loc  }
                                   if loc=LOC_CREGISTER then
                                     emit_reg_reg(A_MOV,opsize,
                                       p^.right^.location.registerhigh,
                                       p^.left^.location.registerhigh)
                                   else
                                     begin
                                        r:=newreference(p^.left^.location.reference);
                                        inc(r^.offset,4);
                                        emit_reg_ref(A_MOV,opsize,
                                          p^.right^.location.registerhigh,r);
                                     end;
                                end;
                              {emit_reg_loc(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location);      }

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
                                      emit_reg(A_FSTP,S_NO,
                                        correct_fpuregister(p^.left^.location.register,fpuvaroffset));
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
                              emit_reg(A_FLD,S_NO,
                                correct_fpuregister(p^.right^.location.register,fpuvaroffset));
                              inc(fpuvaroffset);
                              case loc of
                                 LOC_CFPUREGISTER:
                                   begin
                                      emit_reg(A_FSTP,S_NO,
                                        correct_fpuregister(p^.right^.location.register,fpuvaroffset));
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
                                emit_const_reg(A_MOV,S_B,
                                  1,p^.left^.location.register)
                              else
                                emit_const_ref(A_MOV,S_B,
                                  1,newreference(p^.left^.location.reference));
                              {emit_const_loc(A_MOV,S_B,
                                  1,p^.left^.location);}
                              emitjmp(C_None,hlabel);
                              emitlab(falselabel);
                              if loc=LOC_CREGISTER then
                                emit_reg_reg(A_XOR,S_B,
                                  p^.left^.location.register,
                                  p^.left^.location.register)
                              else
                                begin
                                  emit_const_ref(A_MOV,S_B,
                                    0,newreference(p^.left^.location.reference));
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
         end;
         freelabel(truelabel);
         freelabel(falselabel);
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                             SecondFuncRet
*****************************************************************************}

    procedure secondfuncret(var p : ptree);
      var
         hr : tregister;
         hp : preference;
         pp : pprocinfo;
         hr_valid : boolean;
      begin
         reset_reference(p^.location.reference);
         hr_valid:=false;
         if procinfo<>pprocinfo(p^.funcretprocinfo) then
           begin
              hr:=getregister32;
              hr_valid:=true;
              hp:=new_reference(procinfo^.framepointer,
                procinfo^.framepointer_offset);
              emit_ref_reg(A_MOV,S_L,hp,hr);
              pp:=procinfo^.parent;
              { walk up the stack frame }
              while pp<>pprocinfo(p^.funcretprocinfo) do
                begin
                   hp:=new_reference(hr,
                     pp^.framepointer_offset);
                   emit_ref_reg(A_MOV,S_L,hp,hr);
                   pp:=pp^.parent;
                end;
              p^.location.reference.base:=hr;
              p^.location.reference.offset:=pp^.retoffset;
           end
         else
           begin
             p^.location.reference.base:=procinfo^.framepointer;
             p^.location.reference.offset:=procinfo^.retoffset;
           end;
         if ret_in_param(p^.retdef) then
           begin
              if not hr_valid then
                hr:=getregister32;
              emit_ref_reg(A_MOV,S_L,newreference(p^.location.reference),hr);
              p^.location.reference.base:=hr;
              p^.location.reference.offset:=0;
           end;
      end;


{*****************************************************************************
                           SecondArrayConstruct
*****************************************************************************}

      const
        vtInteger    = 0;
        vtBoolean    = 1;
        vtChar       = 2;
        vtExtended   = 3;
        vtString     = 4;
        vtPointer    = 5;
        vtPChar      = 6;
        vtObject     = 7;
        vtClass      = 8;
        vtWideChar   = 9;
        vtPWideChar  = 10;
        vtAnsiString = 11;
        vtCurrency   = 12;
        vtVariant    = 13;
        vtInterface  = 14;
        vtWideString = 15;
        vtInt64      = 16;

    procedure secondarrayconstruct(var p : ptree);
      var
        hp    : ptree;
        href  : treference;
        lt    : pdef;
        vaddr : boolean;
        vtype : longint;
        freetemp,
        dovariant : boolean;
        elesize : longint;
      begin
        dovariant:=p^.forcevaria or parraydef(p^.resulttype)^.isvariant;
        if dovariant then
         elesize:=8
        else
         begin
           elesize:=parraydef(p^.resulttype)^.elesize;
           if elesize>4 then
            internalerror(8765678);
         end;
        if not p^.cargs then
         begin
           reset_reference(p^.location.reference);
           { Allocate always a temp, also if no elements are required, to
             be sure that location is valid (PFV) }
            if parraydef(p^.resulttype)^.highrange=-1 then
              gettempofsizereference(elesize,p^.location.reference)
            else
              gettempofsizereference((parraydef(p^.resulttype)^.highrange+1)*elesize,p^.location.reference);
           href:=p^.location.reference;
         end;
        hp:=p;
        while assigned(hp) do
         begin
           if assigned(hp^.left) then
            begin
              freetemp:=true;
              secondpass(hp^.left);
              if codegenerror then
               exit;
              if dovariant then
               begin
                 { find the correct vtype value }
                 vtype:=$ff;
                 vaddr:=false;
                 lt:=hp^.left^.resulttype;
                 case lt^.deftype of
                   enumdef,
                   orddef :
                     begin
                       if (lt^.deftype=enumdef) or
                          is_integer(lt) then
                         vtype:=vtInteger
                       else
                         if is_boolean(lt) then
                           vtype:=vtBoolean
                         else
                           if (lt^.deftype=orddef) and (porddef(lt)^.typ=uchar) then
                             vtype:=vtChar;
                     end;
                   floatdef :
                     begin
                       vtype:=vtExtended;
                       vaddr:=true;
                     end;
                   procvardef,
                   pointerdef :
                     begin
                       if is_pchar(lt) then
                         vtype:=vtPChar
                       else
                         vtype:=vtPointer;
                     end;
                   classrefdef :
                     vtype:=vtClass;
                   objectdef :
                     begin
                       vtype:=vtObject;
                     end;
                   stringdef :
                     begin
                       if is_shortstring(lt) then
                        begin
                          vtype:=vtString;
                          vaddr:=true;
                          freetemp:=false;
                        end
                       else
                        if is_ansistring(lt) then
                         vtype:=vtAnsiString;
                     end;
                 end;
                 if vtype=$ff then
                   internalerror(14357);
                 { write C style pushes or an pascal array }
                 if p^.cargs then
                  begin
                    if vaddr then
                     begin
                       emit_to_mem(hp^.left);
                       emit_push_lea_loc(hp^.left^.location,freetemp);
                       del_reference(hp^.left^.location.reference);
                     end
                    else
                     emit_push_loc(hp^.left^.location);
                  end
                 else
                  begin
                    { update href to the vtype field and write it }
                    emit_const_ref(A_MOV,S_L,
                      vtype,newreference(href));
                    inc(href.offset,4);
                    { write changing field update href to the next element }
                    if vaddr then
                     begin
                       emit_to_mem(hp^.left);
                       emit_lea_loc_ref(hp^.left^.location,href,freetemp);
                     end
                    else
                     emit_mov_loc_ref(hp^.left^.location,href,S_L);
                    inc(href.offset,4);
                  end;
               end
              else
              { normal array constructor of the same type }
               begin
                 case elesize of
                   1 :
                     emit_mov_loc_ref(hp^.left^.location,href,S_B);
                   2 :
                     emit_mov_loc_ref(hp^.left^.location,href,S_W);
                   4 :
                     emit_mov_loc_ref(hp^.left^.location,href,S_L);
                   else
                     internalerror(87656781);
                 end;
                 inc(href.offset,elesize);
               end;
            end;
           { load next entry }
           hp:=hp^.right;
         end;
      end;


end.
{
  $Log$
  Revision 1.89  1999-10-12 22:35:48  florian
    * compiler didn't complain about l1+l2:=l1+l2; it gave only an assembler
      error, fixed

  Revision 1.88  1999/09/27 23:44:47  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.87  1999/09/26 13:26:06  florian
    * exception patch of Romio nevertheless the excpetion handling
      needs some corections regarding register saving
    * gettempansistring is again a procedure

  Revision 1.86  1999/09/16 07:56:46  pierre
   * double del_reference removed

  Revision 1.85  1999/09/12 08:48:03  florian
    * bugs 593 and 607 fixed
    * some other potential bugs with array constructors fixed
    * for classes compiled in $M+ and it's childs, the default access method
      is now published
    * fixed copyright message (it is now 1993-99)

  Revision 1.84  1999/09/11 09:08:31  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.83  1999/09/01 09:37:14  peter
    * removed warning

  Revision 1.82  1999/09/01 09:26:21  peter
    * fixed temp allocation for arrayconstructor

  Revision 1.81  1999/08/28 15:34:17  florian
    * bug 519 fixed

  Revision 1.80  1999/08/26 20:24:37  michael
  + Hopefuly last fixes for resourcestrings

  Revision 1.79  1999/08/25 16:41:05  peter
    * resources are working again

  Revision 1.78  1999/08/25 11:59:43  jonas
    * changed pai386, paippc and paiapha (same for tai*) to paicpu (taicpu)

  Revision 1.77  1999/08/24 22:38:51  michael
  * more resourcestring changes

  Revision 1.76  1999/08/23 11:45:39  michael
  * Hopefully final attempt at resourcestrings

  Revision 1.75  1999/08/19 13:08:49  pierre
   * emit_??? used

  Revision 1.74  1999/08/17 13:26:06  peter
    * arrayconstructor -> arrayofconst fixed when arraycosntructor was not
      variant.

  Revision 1.73  1999/08/13 21:33:09  peter
    * support for array constructors extended and more error checking

  Revision 1.72  1999/08/09 22:19:50  peter
    * classes vmt changed to only positive addresses
    * sharedlib creation is working

  Revision 1.71  1999/08/07 14:20:55  florian
    * some small problems fixed

  Revision 1.70  1999/08/04 13:45:22  florian
    + floating point register variables !!
    * pairegalloc is now generated for register variables

  Revision 1.69  1999/08/04 00:22:50  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.68  1999/08/03 22:02:43  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.67  1999/07/27 23:36:36  peter
    * try to determine the fpu type needed in assignment

  Revision 1.66  1999/07/24 15:12:56  michael
  changes for resourcestrings

  Revision 1.65  1999/07/23 23:09:06  peter
    * resourcestring fix

  Revision 1.64  1999/07/22 09:37:37  florian
    + resourcestring implemented
    + start of longstring support

  Revision 1.63  1999/07/05 20:13:12  peter
    * removed temp defines

  Revision 1.62  1999/06/30 15:43:18  florian
    * two bugs regarding method variables fixed
      - if you take in a method the address of another method
        don't need self anymore
      - if the class pointer was in a register, wrong code for a method
        variable load was generated

  Revision 1.61  1999/06/28 22:29:11  florian
    * qword division fixed
    + code for qword/int64 type casting added:
      range checking isn't implemented yet

  Revision 1.60  1999/05/31 12:42:43  peter
    * fixed crash with empty array constructor

  Revision 1.59  1999/05/27 19:44:14  peter
    * removed oldasm
    * plabel -> pasmlabel
    * -a switches to source writing automaticly
    * assembler readers OOPed
    * asmsymbol automaticly external
    * jumptables and other label fixes for asm readers

  Revision 1.58  1999/05/23 18:42:02  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.57  1999/05/21 13:54:51  peter
    * NEWLAB for label as symbol

  Revision 1.56  1999/05/17 23:51:38  peter
    * with temp vars now use a reference with a persistant temp instead
      of setting datasize

  Revision 1.55  1999/05/17 21:57:04  florian
    * new temporary ansistring handling

  Revision 1.54  1999/05/12 00:19:43  peter
    * removed R_DEFAULT_SEG
    * uniform float names

  Revision 1.53  1999/05/06 09:05:16  peter
    * generic write_float and str_float
    * fixed constant float conversions

  Revision 1.52  1999/05/01 13:24:10  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.51  1999/04/28 06:01:55  florian
    * changes of Bruessel:
       + message handler can now take an explicit self
       * typinfo fixed: sometimes the type names weren't written
       * the type checking for pointer comparisations and subtraction
         and are now more strict (was also buggy)
       * small bug fix to link.pas to support compiling on another
         drive
       * probable bug in popt386 fixed: call/jmp => push/jmp
         transformation didn't count correctly the jmp references
       + threadvar support
       * warning if ln/sqrt gets an invalid constant argument

  Revision 1.50  1999/04/16 13:42:26  jonas
    * more regalloc fixes (still not complete)

  Revision 1.49  1999/04/13 18:57:48  florian
    * classes which contain ansistring get unnecessary calls
      to addref/decref when they are assigned, fixed

  Revision 1.48  1999/04/09 15:48:47  jonas
    * added fix for missing register deallocation (-dregallocfix)

  Revision 1.47  1999/03/31 13:55:07  peter
    * assembler inlining working for ag386bin

  Revision 1.46  1999/03/24 23:16:52  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.45  1999/02/25 21:02:28  peter
    * ag386bin updates
    + coff writer

  Revision 1.44  1999/02/22 02:15:12  peter
    * updates for ag386bin

  Revision 1.43  1999/01/27 00:13:54  florian
    * "procedure of object"-stuff fixed

  Revision 1.42  1999/01/21 22:10:40  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.41  1999/01/20 10:20:18  peter
    * don't make localvar copies for assembler procedures

  Revision 1.40  1998/12/30 13:41:07  peter
    * released valuepara

  Revision 1.39  1998/12/19 00:23:45  florian
    * ansistring memory leaks fixed

  Revision 1.38  1998/12/11 00:02:51  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.37  1998/12/10 09:47:17  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.36  1998/12/04 10:18:06  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.35  1998/11/30 09:43:04  pierre
    * some range check bugs fixed (still not working !)
    + added DLL writing support for win32 (also accepts variables)
    + TempAnsi for code that could be used for Temporary ansi strings
      handling

  Revision 1.34  1998/11/28 16:20:48  peter
    + support for dll variables

  Revision 1.33  1998/11/27 14:50:33  peter
    + open strings, $P switch support

  Revision 1.32  1998/11/26 09:53:36  florian
    * for classes no init/final. code is necessary, fixed

  Revision 1.31  1998/11/20 15:35:54  florian
    * problems with rtti fixed, hope it works

  Revision 1.30  1998/11/18 17:45:24  peter
    * fixes for VALUEPARA

  Revision 1.29  1998/11/18 15:44:11  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.28  1998/11/17 11:32:44  peter
    * optimize str:='' in H+ mode
    + -! to test ansistrings

  Revision 1.27  1998/11/16 15:35:39  peter
    * rename laod/copystring -> load/copyshortstring
    * fixed int-bool cnv bug
    + char-ansistring conversion

  Revision 1.26  1998/11/10 10:09:10  peter
    * va_list -> array of const

  Revision 1.25  1998/11/05 12:02:35  peter
    * released useansistring
    * removed -Sv, its now available in fpc modes

  Revision 1.24  1998/10/14 08:47:14  pierre
    * bugs in secondfuncret for result in subprocedures removed

  Revision 1.23  1998/10/06 17:16:44  pierre
    * some memory leaks fixed (thanks to Peter for heaptrc !)

  Revision 1.22  1998/10/01 09:22:53  peter
    * fixed value openarray
    * ungettemp of arrayconstruct

  Revision 1.21  1998/09/28 11:07:39  peter
    + floatdef support for array of const

  Revision 1.20  1998/09/24 14:26:03  peter
    * updated for new tvarrec

  Revision 1.19  1998/09/23 17:49:59  peter
    * high(arrayconstructor) is now correct
    * procvardef support for variant record

  Revision 1.18  1998/09/23 09:58:48  peter
    * first working array of const things

  Revision 1.17  1998/09/20 18:00:19  florian
    * small compiling problems fixed

  Revision 1.16  1998/09/20 17:46:48  florian
    * some things regarding ansistrings fixed

  Revision 1.15  1998/09/17 09:42:16  peter
    + pass_2 for cg386
    * Message() -> CGMessage() for pass_1/pass_2

  Revision 1.14  1998/09/14 10:43:50  peter
    * all internal RTL functions start with FPC_

  Revision 1.13  1998/09/04 12:24:24  florian
    * bug0159 fixed

  Revision 1.12  1998/09/04 11:55:17  florian
    * problem with -Or fixed

  Revision 1.11  1998/09/03 16:03:14  florian
    + rtti generation
    * init table generation changed

  Revision 1.10  1998/08/21 14:08:40  pierre
    + TEST_FUNCRET now default (old code removed)
      works also for m68k (at least compiles)

  Revision 1.9  1998/08/20 09:26:37  pierre
    + funcret setting in underproc testing
      compile with _dTEST_FUNCRET

  Revision 1.8  1998/08/10 14:49:48  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.7  1998/07/30 13:30:33  florian
    * final implemenation of exception support, maybe it needs
      some fixes :)

  Revision 1.6  1998/07/26 21:58:57  florian
   + better support for switch $H
   + index access to ansi strings added
   + assigment of data (records/arrays) containing ansi strings

  Revision 1.5  1998/07/24 22:16:54  florian
    * internal error 10 together with array access fixed. I hope
      that's the final fix.

  Revision 1.4  1998/06/11 13:58:45  peter
    * fixed too long line

  Revision 1.3  1998/06/09 16:01:35  pierre
    + added procedure directive parsing for procvars
      (accepted are popstack cdecl and pascal)
    + added C vars with the following syntax
      var C calias 'true_c_name';(can be followed by external)
      reason is that you must add the Cprefix

      which is target dependent

  Revision 1.2  1998/06/08 13:13:34  pierre
    + temporary variables now in temp_gen.pas unit
      because it is processor independent
    * mppc68k.bat modified to undefine i386 and support_mmx
      (which are defaults for i386)

  Revision 1.1  1998/06/05 17:44:12  peter
    * splitted cgi386

}

