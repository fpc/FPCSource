{
    $Id$
    Copyright (C) 1993-99 by Florian Klaempfl

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
          constructor init(v : pvarsym;st : psymtable);
          destructor done;virtual;

          procedure secondpass;virtual;
       end;

    var
       { this is necessary for the const section }
       simple_loadn : boolean;

  implementation

    uses
       cobjects,aasm,cgbase,cgobj,types,verbose,tgobj,tgcpu,symconst,
       cpubase,cpuasm;

{****************************************************************************
                                 TLOADNODE
 ****************************************************************************}

    constructor tloadnode.init(v : pvarsym;st : psymtable);

      var
         p : ptree;

      begin
         inherited init;
         treetype:=loadn;
         resulttype:=v^.definition;
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

{$ifdef i386}
                    { DLL variable, DLL variables are onyl available on the win32 target }
                    { maybe we've to add this later for the alpha WinNT                  }
                    else if (pvarsym(symtableentry)^.var_options and vo_is_dll_var)<>0 then
                      begin
                         hregister:=tg.getregisterint;
                         location.reference.symbol:=newasmsymbol(symtableentry^.mangledname);
                         exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,newreference(location.reference),hregister)));
                         location.reference.symbol:=nil;
                         location.reference.base:=hregister;
                      end
{$endif i386}
                    else
                      begin
{$ifdef i386}
                         symtabletype:=symtable^.symtabletype;
                         { in case it is a register variable: }
                         if pvarsym(symtableentry)^.reg<>R_NO then
                           begin
                              location.loc:=LOC_CREGISTER;
                              location.register:=pvarsym(symtableentry)^.reg;
                              tg.unusedregsint:=tg.unusedregsint-[pvarsym(symtableentry)^.reg];
                           end
                         else
                           begin
                              { first handle local and temporary variables }
                              if (symtabletype in [parasymtable,inlinelocalsymtable,
                                                   inlineparasymtable,localsymtable]) then
                                begin
                                   location.reference.base:=procinfo.framepointer;
                                   location.reference.offset:=pvarsym(symtableentry)^.address;
                                   if (symtabletype in [localsymtable,inlinelocalsymtable]) then
                                     location.reference.offset:=-location.reference.offset;
                                   if (lexlevel>(symtable^.symtablelevel)) then
                                     begin
                                        hregister:=tg.getregisterint;

                                        { make a reference }
                                        hp:=new_reference(procinfo.framepointer,
                                          procinfo.framepointer_offset);


                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));

                                        simple_loadn:=false;
                                        i:=lexlevel-1;
                                        while i>(symtable^.symtablelevel) do
                                          begin
                                             { make a reference }
                                             hp:=new_reference(hregister,8);
                                             exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));
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
                                        location.reference.base:=procinfo.framepointer;
                                        location.reference.offset:=pvarsym(symtableentry)^.address;
                                     end;
                                   objectsymtable:
                                     begin
                                        if (pvarsym(symtableentry)^.properties and sp_static)<>0 then
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
                                        hp:=new_reference(procinfo.framepointer,
                                          symtable^.datasize);

                                        exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,hp,hregister)));

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
                                   if (location.reference.base=procinfo.framepointer) then
                                     begin
                                        highframepointer:=location.reference.base;
                                        highoffset:=location.reference.offset;
                                     end
                                   else
                                     begin
                                        highframepointer:=R_EDI;
                                        highoffset:=location.reference.offset;
                                        exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                                          location.reference.base,R_EDI)));
                                     end;
                                end;
                              if location.loc=LOC_CREGISTER then
                                begin
                                   exprasmlist^.concat(new(pai386,op_reg_reg(A_MOV,S_L,
                                     location.register,hregister)));
                                   location.loc:=LOC_REFERENCE;
                                end
                              else
                                begin
                                   exprasmlist^.concat(new(pai386,op_ref_reg(A_MOV,S_L,
                                     newreference(location.reference),
                                     hregister)));
                                end;
                              reset_reference(location.reference);
                              location.reference.base:=hregister;
                          end;
{$endif i386}
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

end.
{
  $Log$
  Revision 1.5  1999-08-04 00:23:56  florian
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