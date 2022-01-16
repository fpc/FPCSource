{
    Copyright (c) 1998-2003 by Peter Vreman, Florian Klaempfl and Carl Eric Codere

    Basic stuff for assembler readers

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
unit rasm;

{$i fpcdefs.inc}

  interface

    uses
      cclasses,
      symsym,
      rabase,
      aasmbase,
      aasmdata,
      cpubase,
      cgbase;

    type
       tasmreader = class(tbaseasmreader)
         firsttoken     : boolean;
         _asmsorted     : boolean;
         curlist        : TAsmList;
         c              : char;
         actasmpattern  : string;
         actopcode      : tasmop;
         actasmregister : tregister;
         actcondition   : tasmcond;
         iasmops        : TFPHashList;
         locallabels    : TFPHashObjectList;
         constructor create;override;
         destructor destroy;override;
         function createlocallabel(const s: string; var hl: tasmlabel; emit: boolean): boolean;
         procedure checklocallabels;
       protected
         { allow subscripting a local if it is a:
            1) pointer to a records/object, or a class instance pointer, passed in a register to a pure assembler routine
            2) record located on the stack (passed as parameter, or local variable)
         }
         procedure checklocalsubscript(sym: tabstractnormalvarsym); virtual;
       end;

  implementation

    uses
      verbose,
      procinfo,
      symconst,symdef,
      paramgr;

    type
      TLocalLabel = class(TFPHashObject)
        emitted: boolean;
        lab: tasmlabel;
        function Gettasmlabel: tasmlabel;
      end;


    function TLocalLabel.Gettasmlabel:tasmlabel;
      begin
        if not assigned(lab) then
         begin
           current_asmdata.getjumplabel(lab);
           { this label is forced to be used so it's always written }
           lab.increfs;
         end;
        result:=lab;
      end;


    constructor tasmreader.create;
      begin
        inherited create;
        firsttoken:=true;
        locallabels:=TFPHashObjectList.create;
      end;


    destructor tasmreader.destroy;
      begin
        locallabels.Free;
        iasmops.Free;
        inherited destroy;
      end;


    function tasmreader.createlocallabel(const s: string; var hl: tasmlabel; emit:boolean):boolean;
      var
        lab: TLocalLabel;
      begin
        result:=true;
        { Check if it already is defined }
        lab:=TLocalLabel(locallabels.Find(s));
        if not assigned(lab) then
          lab:=TLocalLabel.Create(locallabels,s);
        { set emitted flag and check for dup syms }
        if emit then
          begin
            if lab.Emitted then
              begin
                Message1(asmr_e_dup_local_sym,lab.Name);
                result:=false;
              end;
            lab.Emitted:=true;
          end;
        hl:=lab.Gettasmlabel;
      end;


    procedure tasmreader.checklocallabels;
      var
        i: longint;
        lab: TLocalLabel;
      begin
        for i:=0 to locallabels.Count-1 do
          begin
            lab:=TLocalLabel(locallabels[i]);
            if not lab.emitted then
              Message1(asmr_e_unknown_label_identifier,lab.name);
          end;
        locallabels.Clear;
      end;


    procedure tasmreader.checklocalsubscript(sym: tabstractnormalvarsym);
      var
        isimplicitpointer: boolean;
      begin
        isimplicitpointer:=
          (sym.typ=paravarsym) and
          (is_implicit_pointer_object_type(sym.vardef) or
           paramanager.push_addr_param(sym.varspez,sym.vardef,current_procinfo.procdef.proccalloption));

        { sym.initiallloc/localloc is not yet initialised here }

        { pointer parameter to aggregate passed in register to pure assembler routine }
        if (po_assembler in current_procinfo.procdef.procoptions) and
           (sym.typ=paravarsym) and
           (tparavarsym(sym).paraloc[calleeside].location^.loc=LOC_REGISTER) and
           { ... however this is only possible if the pointer takes only one register location }
           not assigned(tparavarsym(sym).paraloc[calleeside].location^.Next) and
           isimplicitpointer then
          exit;

        { aggregate parameter passed on the stack to a pure assembler routine }
        if (po_assembler in current_procinfo.procdef.procoptions) and
           (sym.typ=paravarsym) and
           { sym.localloc is not yet initialised here for pure assembler routines }
           (tparavarsym(sym).paraloc[calleeside].location^.loc in [LOC_REFERENCE,LOC_CREFERENCE]) and
           not isimplicitpointer then
          exit;

        { aggregate parameter located on the stack for a non-assembler routine
          (locals accessed from assembler code are never kept in registers) }
        if not(po_assembler in current_procinfo.procdef.procoptions) and
           (sym.typ=paravarsym) and
           not isimplicitpointer then
          exit;

        { local aggregate located on the stack (locals accessed from assembler
          code are never kept in registers) }
        if ((sym.typ=localvarsym) or
           { even if a parameter is passed by reference, it will be copied to
             a local if it's a value parameter to a non assembler routines }
            (not(po_assembler in current_procinfo.procdef.procoptions) and
             (sym.typ=paravarsym) and
             (sym.varspez=vs_value))) and
           not is_implicit_pointer_object_type(sym.vardef) then
          exit;

        Message(asmr_e_cannot_access_field_directly_for_parameters);
      end;

end.
