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
      tree,i386;

    var
       { this is for open arrays and strings        }
       { but be careful, this data is in the        }
       { generated code destroyed quick, and also   }
       { the next call of secondload destroys this  }
       { data                                       }
       { So be careful using the informations       }
       { provided by this variables                 }
       highframepointer : tregister;
       highoffset : longint;

    procedure secondload(var p : ptree);
    procedure secondassignment(var p : ptree);
    procedure secondfuncret(var p : ptree);


implementation

    uses
      cobjects,verbose,globals,
      symtable,aasm,types,
      cgi386,cgai386,temp_gen,tgeni386,hcodegen;


{*****************************************************************************
                             SecondLoad
*****************************************************************************}

    procedure secondload(var p : ptree);
      var
         hregister : tregister;
         symtabletype : tsymtabletype;
         i : longint;
         hp : preference;
      begin
         simple_loadn:=true;
         reset_reference(p^.location.reference);
         case p^.symtableentry^.typ of
              { this is only for toasm and toaddr }
              absolutesym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    if (pabsolutesym(p^.symtableentry)^.abstyp=toaddr) then
                     begin
                       if pabsolutesym(p^.symtableentry)^.absseg then
                        p^.location.reference.segment:=R_FS;
                       p^.location.reference.offset:=pabsolutesym(p^.symtableentry)^.address;
                     end
                    else
                     p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                    maybe_concat_external(p^.symtableentry^.owner,p^.symtableentry^.mangledname);
                 end;
              varsym :
                 begin
                    hregister:=R_NO;
                    if (pvarsym(p^.symtableentry)^.var_options and vo_is_C_var)<>0 then
                      begin
                         stringdispose(p^.location.reference.symbol);
                         p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                         if (pvarsym(p^.symtableentry)^.var_options and vo_is_external)<>0 then
                           maybe_concat_external(p^.symtableentry^.owner,p^.symtableentry^.mangledname);
                      end
                    else
                      begin
                         symtabletype:=p^.symtable^.symtabletype;
                         { in case it is a register variable: }
                         if pvarsym(p^.symtableentry)^.reg<>R_NO then
                           begin
                              p^.location.loc:=LOC_CREGISTER;
                              p^.location.register:=pvarsym(p^.symtableentry)^.reg;
                              unused:=unused-[pvarsym(p^.symtableentry)^.reg];
                           end
                         else
                           begin
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   p^.location.reference.base:=procinfo.framepointer;
                                   p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                   if (symtabletype=localsymtable) or (symtabletype=inlinelocalsymtable) then
                                     p^.location.reference.offset:=-p^.location.reference.offset;
                                   if (symtabletype=parasymtable) or (symtabletype=inlineparasymtable) then
                                     inc(p^.location.reference.offset,p^.symtable^.call_offset);
                                   if (lexlevel>(p^.symtable^.symtablelevel)) then
                                     begin
                                        hregister:=getregister32;
     
                                        { make a reference }
                                        hp:=new_reference(procinfo.framepointer,
                                          procinfo.framepointer_offset);
     
     
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));
     
                                        simple_loadn:=false;
                                        i:=lexlevel-1;
                                        while i>(p^.symtable^.symtablelevel) do
                                          begin
                                             { make a reference }
                                             hp:=new_reference(hregister,8);
                                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));
                                             dec(i);
                                          end;
                                        p^.location.reference.base:=hregister;
                                     end;
                                end
                              else
                                case symtabletype of
                                   unitsymtable,globalsymtable,
                                   staticsymtable : begin
                                                       stringdispose(p^.location.reference.symbol);
                                                       p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                                                       if symtabletype=unitsymtable then
                                                         concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                                                    end;
                                   objectsymtable : begin
                                                       if (pvarsym(p^.symtableentry)^.properties and sp_static)<>0 then
                                                         begin
                                                            stringdispose(p^.location.reference.symbol);
                                                            p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                                                            if p^.symtable^.defowner^.owner^.symtabletype=unitsymtable then
                                                              concat_external(p^.symtableentry^.mangledname,EXT_NEAR);
                                                         end
                                                       else
                                                         begin
                                                            p^.location.reference.base:=R_ESI;
                                                            p^.location.reference.offset:=pvarsym(p^.symtableentry)^.address;
                                                         end;
                                                    end;
                                   withsymtable:
                                     begin
                                        hregister:=getregister32;
                                        p^.location.reference.base:=hregister;
                                        { make a reference }
                                        { symtable datasize field
                                          contains the offset of the temp
                                          stored }
                                        hp:=new_reference(procinfo.framepointer,
                                          p^.symtable^.datasize);
     
                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));
     
                                        p^.location.reference.offset:=
                                          pvarsym(p^.symtableentry)^.address;
                                     end;
                                end;
                              { in case call by reference, then calculate: }
                              if (pvarsym(p^.symtableentry)^.varspez=vs_var) or
                                 ((pvarsym(p^.symtableentry)^.varspez=vs_const) and
                                  dont_copy_const_param(pvarsym(p^.symtableentry)^.definition)) or
                                  { call by value open arrays are also indirect addressed }
                                  is_open_array(pvarsym(p^.symtableentry)^.definition) then
                                begin
                                   simple_loadn:=false;
                                   if hregister=R_NO then
                                     hregister:=getregister32;
                                   if (p^.location.reference.base=procinfo.framepointer) then
                                     begin
                                        highframepointer:=p^.location.reference.base;
                                        highoffset:=p^.location.reference.offset;
                                     end
                                   else
                                     begin
                                        highframepointer:=R_EDI;
                                        highoffset:=p^.location.reference.offset;
                                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                                          p^.location.reference.base,R_EDI)));
                                     end;
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),
                                     hregister)));
                                   clear_reference(p^.location.reference);
                                   p^.location.reference.base:=hregister;
                               end;
                              {
                              if (pvarsym(p^.symtableentry)^.definition^.deftype=objectdef) and
                                ((pobjectdef(pvarsym(p^.symtableentry)^.definition)^.options and oois_class)<>0) then
                                begin
                                   simple_loadn:=false;
                                   if hregister=R_NO then
                                     hregister:=getregister32;
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),
                                     hregister)));
                                   clear_reference(p^.location.reference);
                                   p^.location.reference.base:=hregister;
                                end;
                              }
                           end;
                      end;
                 end;
              procsym:
                 begin
                    {!!!!! Be aware, work on virtual methods too }
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=
                      stringdup(pprocsym(p^.symtableentry)^.definition^.mangledname);
                    maybe_concat_external(p^.symtable,p^.symtableentry^.mangledname);
                 end;
              typedconstsym :
                 begin
                    stringdispose(p^.location.reference.symbol);
                    p^.location.reference.symbol:=stringdup(p^.symtableentry^.mangledname);
                    maybe_concat_external(p^.symtable,p^.symtableentry^.mangledname);
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
         otlabel,hlabel,oflabel : plabel;
         hregister : tregister;
         loc : tloc;
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
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_LEA,S_L,newreference(
                                     p^.left^.location.reference),
                                     hregister)));
                                   clear_reference(p^.left^.location.reference);
                                   p^.left^.location.reference.base:=hregister;
                                   p^.left^.location.reference.index:=R_NO;
                                end;
                              loc:=LOC_REFERENCE;
                           end;
            LOC_CREGISTER:
              loc:=LOC_CREGISTER;
            LOC_MMXREGISTER:
              loc:=LOC_MMXREGISTER;
            LOC_CMMXREGISTER:
              loc:=LOC_CMMXREGISTER;
            else
               begin
                  Message(cg_e_illegal_expression);
                  exit;
               end;
         end;
         { lets try to optimize this (PM)             }
         { define a dest_loc that is the location      }
         { and a ptree to verify that it is the right }
         { place to insert it                         }
{$ifdef test_dest_loc}
         if (aktexprlevel<4) then
           begin
              dest_loc_known:=true;
              dest_loc:=p^.left^.location;
              dest_loc_tree:=p^.right;
           end;
{$endif test_dest_loc}

         if (p^.right^.treetype=realconstn) then
           begin
              if p^.left^.resulttype^.deftype=floatdef then
                begin
                   case pfloatdef(p^.left^.resulttype)^.typ of
                     s32real : p^.right^.realtyp:=ait_real_32bit;
                     s64real : p^.right^.realtyp:=ait_real_64bit;
                     s80real : p^.right^.realtyp:=ait_real_extended;
                     { what about f32bit and s64bit }
                     end;
                end;
           end;
         secondpass(p^.right);

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
{$ifdef UseAnsiString}
              if is_ansistring(p^.left^.resulttype) then
                begin
                  { the source and destinations are released
                    in loadansistring, because an ansi string can
                    also be in a register
                  }
                  loadansistring(p);
                end
              else
{$endif UseAnsiString}
              if not (p^.concat_string) then
                begin
                  { we do not need destination anymore }
                  del_reference(p^.left^.location.reference);
                  del_reference(p^.right^.location.reference);
                  loadstring(p);
                  ungetiftemp(p^.right^.location.reference);
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
                         { handle ordinal constants trimmed }
                         if (p^.right^.treetype in [ordconstn,fixconstn]) or
                            (loc=LOC_CREGISTER) then
                           begin
                              case p^.left^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                              end;
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,opsize,
                                  newreference(p^.right^.location.reference),
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,opsize,
                                  p^.right^.location.reference.offset,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai386,op_const_loc(A_MOV,opsize,
                                  p^.right^.location.reference.offset,
                                  p^.left^.location)));}
                           end
                         else
                           begin
                              concatcopy(p^.right^.location.reference,
                                p^.left^.location.reference,p^.left^.resulttype^.size,false);
                              ungetiftemp(p^.right^.location.reference);
                           end;
                      end;
{$ifdef SUPPORT_MMX}
            LOC_CMMXREGISTER,
            LOC_MMXREGISTER:
              begin
                 if loc=LOC_CMMXREGISTER then
                   exprasmlist^.concat(new(pai386,op_reg_reg(A_MOVQ,S_NO,
                   p^.right^.location.register,p^.left^.location.register)))
                 else
                   exprasmlist^.concat(new(pai386,op_reg_ref(A_MOVQ,S_NO,
                     p^.right^.location.register,newreference(p^.left^.location.reference))));
              end;
{$endif SUPPORT_MMX}
            LOC_REGISTER,
            LOC_CREGISTER : begin
                              case p^.right^.resulttype^.size of
                                 1 : opsize:=S_B;
                                 2 : opsize:=S_W;
                                 4 : opsize:=S_L;
                              end;
                              { simplified with op_reg_loc         }
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_reg_ref(A_MOV,opsize,
                                  p^.right^.location.register,
                                  newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai386,op_reg_loc(A_MOV,opsize,
                                  p^.right^.location.register,
                                  p^.left^.location)));             }

                           end;
            LOC_FPU : begin
                              if loc<>LOC_REFERENCE then
                                internalerror(10010)
                              else
                                floatstore(pfloatdef(p^.left^.resulttype)^.typ,
                                  p^.left^.location.reference);
                           end;
            LOC_JUMP     : begin
                              getlabel(hlabel);
                              emitl(A_LABEL,truelabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_const_reg(A_MOV,S_B,
                                  1,p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,
                                  1,newreference(p^.left^.location.reference))));
                              {exprasmlist^.concat(new(pai386,op_const_loc(A_MOV,S_B,
                                  1,p^.left^.location)));}
                              emitl(A_JMP,hlabel);
                              emitl(A_LABEL,falselabel);
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_reg_reg(A_XOR,S_B,
                                  p^.left^.location.register,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_const_ref(A_MOV,S_B,
                                  0,newreference(p^.left^.location.reference))));
                              emitl(A_LABEL,hlabel);
                           end;
            LOC_FLAGS    : begin
                              if loc=LOC_CREGISTER then
                                exprasmlist^.concat(new(pai386,op_reg(flag_2_set[p^.right^.location.resflags],S_B,
                                  p^.left^.location.register)))
                              else
                                exprasmlist^.concat(new(pai386,op_ref(flag_2_set[p^.right^.location.resflags],S_B,
                                  newreference(p^.left^.location.reference))));
                           end;
         end;
         truelabel:=otlabel;
         falselabel:=oflabel;
      end;


{*****************************************************************************
                             SecondFuncRetN
*****************************************************************************}

    procedure secondfuncret(var p : ptree);
      var
         hr : tregister;
{$ifdef TEST_FUNCRET}
         hp : preference;
         pp : pprocinfo;
         hr_valid : boolean;
{$endif TEST_FUNCRET}
      begin
         clear_reference(p^.location.reference);
{$ifndef TEST_FUNCRET}
         p^.location.reference.base:=procinfo.framepointer;
         p^.location.reference.offset:=procinfo.retoffset;
         if ret_in_param(procinfo.retdef) then
{$else TEST_FUNCRET}
         hr_valid:=false;
         if @procinfo<>pprocinfo(p^.funcretprocinfo) then
           begin
              hr:=getregister32;
              hr_valid:=false;
              hp:=new_reference(procinfo.framepointer,
                pprocinfo(procinfo.framepointer_offset);
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hr)));
              pp:=procinfo.parent;
              while pp<>pprocinfo(p^.funcretprocinfo) do
                begin
                   hp:=new_reference(hr,
                     pprocinfo(pp^.framepointer_offset);
                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hr)));
                end;
              p^.location.reference.base:=hr;
              { walk up the stack frame }
              { not done yet !! }
           end
         else
           p^.location.reference.base:=procinfo.framepointer;
         p^.location.reference.offset:=pprocinfo(p^.funcretprocinfo)^.retoffset;
         if ret_in_param(p^.retdef) then
{$endif TEST_FUNCRET}
           begin
{$ifdef TEST_FUNCRET}
              if not hr_valid then
{$endif TEST_FUNCRET}
                hr:=getregister32;
              exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(p^.location.reference),hr)));
              p^.location.reference.base:=hr;
              p^.location.reference.offset:=0;
           end;
      end;


end.
{
  $Log$
  Revision 1.3  1998-06-09 16:01:35  pierre
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

