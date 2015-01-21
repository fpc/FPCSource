{
    Copyright (C) 2010 by Jonas Maebe

    This unit handles the temporary variables for the JVM

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
{
  This unit handles the temporary variables for the JVM.
}
unit tgcpu;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmdata,
       cgutils,
       symtype,tgobj;

    type

       { ttgjvm }

       ttgjvm = class(ttgobj)
        protected
         procedure getimplicitobjtemp(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference);
         function getifspecialtemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference): boolean;
         procedure alloctemp(list: TAsmList; size, alignment: longint; temptype: ttemptype; def: tdef; out ref: treference); override;
        public
         procedure setfirsttemp(l : longint); override;
         procedure getlocal(list: TAsmList; size: longint; alignment: shortint; def: tdef; var ref: treference); override;
         procedure gethltemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference); override;
         procedure gethltemptyped(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference); override;
       end;

  implementation

    uses
       verbose,
       cgbase,
       symconst,symdef,symsym,symcpu,defutil,
       cpubase,aasmcpu,
       hlcgobj,hlcgcpu;


    { ttgjvm }

    procedure ttgjvm.getimplicitobjtemp(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference);
      var
        sym: tsym;
        pd: tprocdef;
      begin
        gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
        list.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(tabstractrecorddef(def).jvm_full_typename(true))));
        { the constructor doesn't return anything, so put a duplicate of the
          self pointer on the evaluation stack for use as function result
          after the constructor has run }
        list.concat(taicpu.op_none(a_dup));
        thlcgjvm(hlcg).incstack(list,2);
        { call the constructor }
        sym:=tsym(tabstractrecorddef(def).symtable.find('CREATE'));
        if assigned(sym) and
           (sym.typ=procsym) then
          begin
            pd:=tprocsym(sym).find_bytype_parameterless(potype_constructor);
            if not assigned(pd) then
              internalerror(2011032701);
          end
        else
          internalerror(2011060301);
        hlcg.a_call_name(list,pd,pd.mangledname,nil,false);
        thlcgjvm(hlcg).decstack(list,1);
        { store reference to instance }
        thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
      end;


    function ttgjvm.getifspecialtemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference): boolean;
      var
        eledef: tdef;
        ndim: longint;
        sym: tsym;
        pd: tprocdef;
      begin
        result:=false;
        case def.typ of
          arraydef:
            begin
              if not is_dynamic_array(def) then
                begin
                  { allocate an array of the right size }
                  gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
                  ndim:=0;
                  eledef:=def;
                  repeat
                    if forcesize<>-1 then
                      thlcgjvm(hlcg).a_load_const_stack(list,s32inttype,forcesize div tarraydef(eledef).elesize,R_INTREGISTER)
                    else
                      thlcgjvm(hlcg).a_load_const_stack(list,s32inttype,tarraydef(eledef).elecount,R_INTREGISTER);
                    eledef:=tarraydef(eledef).elementdef;
                    inc(ndim);
                    forcesize:=-1;
                  until (eledef.typ<>arraydef) or
                        is_dynamic_array(eledef);
                  eledef:=tarraydef(def).elementdef;
                  thlcgjvm(hlcg).g_newarray(list,def,ndim);
                  thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
                  result:=true;
                end;
            end;
          recorddef:
            begin
              getimplicitobjtemp(list,def,temptype,ref);
              result:=true;
            end;
          setdef:
            begin
              if tsetdef(def).elementdef.typ=enumdef then
                begin
                  { load enum class type }
                  list.concat(taicpu.op_sym(a_ldc,current_asmdata.RefAsmSymbol(tcpuenumdef(tenumdef(tsetdef(def).elementdef).getbasedef).classdef.jvm_full_typename(true))));
                  thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
                  { call tenumset.noneOf() class method }
                  sym:=tsym(tobjectdef(java_juenumset).symtable.find('NONEOF'));
                  if assigned(sym) and
                     (sym.typ=procsym) then
                    begin
                      if tprocsym(sym).procdeflist.Count<>1 then
                        internalerror(2011062801);
                      pd:=tprocdef(tprocsym(sym).procdeflist[0]);
                    end;
                  hlcg.a_call_name(list,pd,pd.mangledname,nil,false);
                  { static calls method replaces parameter with set instance
                    -> no change in stack height }
                end
              else
                begin
                  list.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(java_jubitset.jvm_full_typename(true))));
                  { the constructor doesn't return anything, so put a duplicate of the
                    self pointer on the evaluation stack for use as function result
                    after the constructor has run }
                  list.concat(taicpu.op_none(a_dup));
                  thlcgjvm(hlcg).incstack(list,2);
                  { call the constructor }
                  sym:=tsym(java_jubitset.symtable.find('CREATE'));
                  if assigned(sym) and
                     (sym.typ=procsym) then
                    begin
                      pd:=tprocsym(sym).find_bytype_parameterless(potype_constructor);
                      if not assigned(pd) then
                        internalerror(2011062802);
                    end
                  else
                    internalerror(2011062803);
                  hlcg.a_call_name(list,pd,pd.mangledname,nil,false);
                  { duplicate self pointer is removed }
                  thlcgjvm(hlcg).decstack(list,1);
                end;
              { store reference to instance }
              gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
              thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
              result:=true;
            end;
          procvardef:
            begin
              if not tprocvardef(def).is_addressonly then
                begin
                  getimplicitobjtemp(list,tcpuprocvardef(def).classdef,temptype,ref);
                  result:=true;
                end;
            end;
          stringdef:
            begin
              if is_shortstring(def) then
                begin
                  gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
                  { add the maxlen parameter (s8inttype because parameters must
                    be sign extended) }
                  thlcgjvm(hlcg).a_load_const_stack(list,s8inttype,shortint(tstringdef(def).len),R_INTREGISTER);
                  { call the constructor }
                  sym:=tsym(tobjectdef(java_shortstring).symtable.find('CREATEEMPTY'));
                  if assigned(sym) and
                     (sym.typ=procsym) then
                    begin
                      if tprocsym(sym).procdeflist.Count<>1 then
                        internalerror(2011052404);
                      pd:=tprocdef(tprocsym(sym).procdeflist[0]);
                    end;
                  hlcg.a_call_name(list,pd,pd.mangledname,nil,false);
                  { static calls method replaces parameter with string instance
                    -> no change in stack height }
                  { store reference to instance }
                  thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
                  result:=true;
                end;
            end;
        end;
      end;


    procedure ttgjvm.alloctemp(list: TAsmList; size, alignment: longint; temptype: ttemptype; def: tdef; out ref: treference);
      begin
        { the JVM only supports 1 slot (= 4 bytes in FPC) and 2 slot (= 8 bytes in
          FPC) temps on the stack. double and int64 are 2 slots, the rest is one slot.
          There are no problems with reusing the same slot for a value of a different
          type. There are no alignment requirements either. }
        if size<4 then
          size:=4;
        if not(size in [4,8]) then
          internalerror(2010121401);
        { don't pass on "def", since we don't care if a slot is used again for a
          different type }
        inherited alloctemp(list, size shr 2, 1, temptype, nil,ref);
      end;


    procedure ttgjvm.setfirsttemp(l: longint);
      begin
        firsttemp:=l;
        lasttemp:=l;
      end;


    procedure ttgjvm.getlocal(list: TAsmList; size: longint; alignment: shortint; def: tdef; var ref: treference);
      begin
        if not getifspecialtemp(list,def,size,tt_persistent,ref) then
          inherited;
      end;


    procedure ttgjvm.gethltemp(list: TAsmList; def: tdef; forcesize: aint; temptype: ttemptype; out ref: treference);
      begin
        if not getifspecialtemp(list,def,forcesize,temptype,ref) then
          inherited;
      end;

    procedure ttgjvm.gethltemptyped(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference);
      begin
        gethltemp(list,def,def.size,temptype,ref);
      end;


begin
  tgobjclass:=ttgjvm;
end.
