{
    Copyright (C) 2019 Dmitry Boyarintsev

    This unit handles the temporary variables for the WebAssembly

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
unit tgcpu;

{$i fpcdefs.inc}

  interface

    uses
       globtype,
       aasmdata,
       cgutils, cpubase,
       symtype,tgobj;

    type

      { TWasmLocal }

      TWasmLocal = class
        inuse    : Boolean;
        index    : integer;
        typ      : TWasmBasicType;
        next     : TWasmLocal; // next in the same basic type
        nextseq  : TWasmLocal; // from 0 to max
        constructor create(atype: TWasmBasicType; aindex: integer);
      end;

      { TWasmLocalVars }

      TWasmLocalVars = class
      private
        last: TWasmLocal; // need public?
      public
        locv: array[TWasmBasicType] of TWasmLocal;
        ordered: array of integer;
        first: TWasmLocal; // first in sequence
        varindex: integer;
        constructor Create(astartindex: Integer = 0);
        destructor Destroy; override;
        function alloc(bt: TWasmBasicType): integer;
        procedure dealloc(bt: TWasmBasicType; index: integer);
      end;

       { ttgwasm }

       ttgwasm = class(ttgobj)
        //protected
        // procedure getimplicitobjtemp(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference);
        // function getifspecialtemp(list: TAsmList; def: tdef; forcesize: asizeint; temptype: ttemptype; out ref: treference): boolean;
         procedure alloctemp(list: TAsmList; size: asizeint; alignment: shortint; temptype: ttemptype; def: tdef; fini: boolean; out ref: treference); override;

         procedure updateFirstTemp;
        public
         localvars: TWasmLocalVars;
         constructor create; override;
         destructor destroy; override;
         procedure setfirsttemp(l : asizeint); override;
         procedure getlocal(list: TAsmList; size: asizeint; alignment: shortint; def: tdef; var ref: treference); override;
         procedure gethltemp(list: TAsmList; def: tdef; forcesize: asizeint; temptype: ttemptype; out ref: treference); override;
         procedure gethltempmanaged(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference); override;

         procedure allocLocalVarToRef(wbt: TWasmBasicType; out ref: treference);
         procedure deallocLocalVar(wbt: TWasmBasicType; idx: integer);
         procedure LocalVarToRef(idx: integer; size: Integer; out ref: treference);
       end;

    function defToWasmBasic(def: tdef; var wbt: TWasmBasicType): Boolean;

  implementation

    uses
       verbose,
       cgbase,
       symconst,symtable,symdef,symsym,symcpu,defutil,
       aasmbase,aasmcpu,
       hlcgobj,hlcgcpu, procinfo;

    function defToWasmBasic(def: tdef; var wbt: TWasmBasicType): Boolean;
    begin
      Result := assigned(def);
      if not Result then Exit;

      if is_pointer(def) then
        wbt := wbt_i32 // wasm32
      else if is_ordinal(def) then begin
        if is_64bit(def) then wbt := wbt_i64
        else wbt := wbt_i32;
      end else if is_real(def) then begin
        if is_single(def) then wbt := wbt_f32
        else wbt := wbt_f64; // real/double/extended
      end else
        Result := false;
    end;

        { TWasmLocal }

                constructor TWasmLocal.create(atype: TWasmBasicType;
                  aindex: integer);
        begin
          typ:=atype;
          index:=aindex;
        end;

    { TWasmLocalVars }

        constructor TWasmLocalVars.Create(astartindex: Integer = 0);
          begin
            inherited Create;
            varindex := astartindex;
          end;

        destructor TWasmLocalVars.Destroy;
          var
            t : TWasmLocal;
            n : TWasmLocal;
          begin
            t := first;
            while Assigned(t) do
              begin
                n:=t;
                t:=t.nextseq;
                n.Free;
              end;
            inherited Destroy;
          end;


    { ttgwasm }

    //procedure ttgwasm.getimplicitobjtemp(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference);
    //  var
    //    sym: tsym;
    //    pd: tprocdef;
    //  begin
    //    gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
    //    list.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(tabstractrecorddef(def).jvm_full_typename(true),AT_METADATA)));
    //    { the constructor doesn't return anything, so put a duplicate of the
    //      self pointer on the evaluation stack for use as function result
    //      after the constructor has run }
    //    list.concat(taicpu.op_none(a_dup));
    //    thlcgjvm(hlcg).incstack(list,2);
    //    { call the constructor }
    //    sym:=tsym(tabstractrecorddef(def).symtable.find('CREATE'));
    //    if assigned(sym) and
    //       (sym.typ=procsym) then
    //      begin
    //        pd:=tprocsym(sym).find_bytype_parameterless(potype_constructor);
    //        if not assigned(pd) then
    //          internalerror(2011032701);
    //      end
    //    else
    //      internalerror(2011060301);
    //    hlcg.a_call_name(list,pd,pd.mangledname,[],nil,false);
    //    thlcgjvm(hlcg).decstack(list,1);
    //    { store reference to instance }
    //    thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
    //  end;


    //function ttgwasm.getifspecialtemp(list: TAsmList; def: tdef; forcesize: asizeint; temptype: ttemptype; out ref: treference): boolean;
    //  var
    //    eledef: tdef;
    //    ndim: longint;
    //    sym: tsym;
    //    pd: tprocdef;
    //  begin
    //    result:=false;
    //    case def.typ of
    //      arraydef:
    //        begin
    //          if not is_dynamic_array(def) then
    //            begin
    //              { allocate an array of the right size }
    //              gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
    //              ndim:=0;
    //              eledef:=def;
    //              repeat
    //                if forcesize<>-1 then
    //                  thlcgjvm(hlcg).a_load_const_stack(list,s32inttype,forcesize div tarraydef(eledef).elesize,R_INTREGISTER)
    //                else
    //                  thlcgjvm(hlcg).a_load_const_stack(list,s32inttype,tarraydef(eledef).elecount,R_INTREGISTER);
    //                eledef:=tarraydef(eledef).elementdef;
    //                inc(ndim);
    //                forcesize:=-1;
    //              until (eledef.typ<>arraydef) or
    //                    is_dynamic_array(eledef);
    //              eledef:=tarraydef(def).elementdef;
    //              thlcgjvm(hlcg).g_newarray(list,def,ndim);
    //              thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
    //              result:=true;
    //            end;
    //        end;
    //      recorddef:
    //        begin
    //          getimplicitobjtemp(list,def,temptype,ref);
    //          result:=true;
    //        end;
    //      setdef:
    //        begin
    //          if tsetdef(def).elementdef.typ=enumdef then
    //            begin
    //              { load enum class type }
    //              list.concat(taicpu.op_sym(a_ldc,current_asmdata.RefAsmSymbol(tcpuenumdef(tenumdef(tsetdef(def).elementdef).getbasedef).classdef.jvm_full_typename(true),AT_METADATA)));
    //              thlcgjvm(hlcg).incstack(current_asmdata.CurrAsmList,1);
    //              { call tenumset.noneOf() class method }
    //              sym:=tsym(tobjectdef(java_juenumset).symtable.find('NONEOF'));
    //              if assigned(sym) and
    //                 (sym.typ=procsym) then
    //                begin
    //                  if tprocsym(sym).procdeflist.Count<>1 then
    //                    internalerror(2011062801);
    //                  pd:=tprocdef(tprocsym(sym).procdeflist[0]);
    //                  hlcg.a_call_name(list,pd,pd.mangledname,[],nil,false);
    //                end;
    //              { static calls method replaces parameter with set instance
    //                -> no change in stack height }
    //            end
    //          else
    //            begin
    //              list.concat(taicpu.op_sym(a_new,current_asmdata.RefAsmSymbol(java_jubitset.jvm_full_typename(true),AT_METADATA)));
    //              { the constructor doesn't return anything, so put a duplicate of the
    //                self pointer on the evaluation stack for use as function result
    //                after the constructor has run }
    //              list.concat(taicpu.op_none(a_dup));
    //              thlcgjvm(hlcg).incstack(list,2);
    //              { call the constructor }
    //              sym:=tsym(java_jubitset.symtable.find('CREATE'));
    //              if assigned(sym) and
    //                 (sym.typ=procsym) then
    //                begin
    //                  pd:=tprocsym(sym).find_bytype_parameterless(potype_constructor);
    //                  if not assigned(pd) then
    //                    internalerror(2011062802);
    //                end
    //              else
    //                internalerror(2011062803);
    //              hlcg.a_call_name(list,pd,pd.mangledname,[],nil,false);
    //              { duplicate self pointer is removed }
    //              thlcgjvm(hlcg).decstack(list,1);
    //            end;
    //          { store reference to instance }
    //          gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
    //          thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
    //          result:=true;
    //        end;
    //      procvardef:
    //        begin
    //          if not tprocvardef(def).is_addressonly then
    //            begin
    //              getimplicitobjtemp(list,tcpuprocvardef(def).classdef,temptype,ref);
    //              result:=true;
    //            end;
    //        end;
    //      stringdef:
    //        begin
    //          if is_shortstring(def) then
    //            begin
    //              gettemp(list,java_jlobject.size,java_jlobject.alignment,temptype,ref);
    //              { add the maxlen parameter (s8inttype because parameters must
    //                be sign extended) }
    //              thlcgjvm(hlcg).a_load_const_stack(list,s8inttype,shortint(tstringdef(def).len),R_INTREGISTER);
    //              { call the constructor }
    //              sym:=tsym(tobjectdef(java_shortstring).symtable.find('CREATEEMPTY'));
    //              if assigned(sym) and
    //                 (sym.typ=procsym) then
    //                begin
    //                  if tprocsym(sym).procdeflist.Count<>1 then
    //                    internalerror(2011052404);
    //                  pd:=tprocdef(tprocsym(sym).procdeflist[0]);
    //                  hlcg.a_call_name(list,pd,pd.mangledname,[],nil,false);
    //                end;
    //              { static calls method replaces parameter with string instance
    //                -> no change in stack height }
    //              { store reference to instance }
    //              thlcgjvm(hlcg).a_load_stack_ref(list,java_jlobject,ref,0);
    //              result:=true;
    //            end;
    //        end;
    //      filedef:
    //        begin
    //          case tfiledef(def).filetyp of
    //            ft_text:
    //              result:=getifspecialtemp(list,search_system_type('TEXTREC').typedef,forcesize,temptype,ref);
    //            ft_typed,
    //            ft_untyped:
    //              result:=getifspecialtemp(list,search_system_type('FILEREC').typedef,forcesize,temptype,ref);
    //          end;
    //        end;
    //      else
    //        ;
    //    end;
    //  end;


    procedure ttgwasm.alloctemp(list: TAsmList; size: asizeint; alignment: shortint; temptype: ttemptype; def: tdef; fini: boolean; out ref: treference);
      begin
        inherited;
        //Internalerror(2019091802);
        { the WebAssembly only supports 1 slot (= 4 bytes in FPC) and 2 slot (= 8 bytes in
          FPC) temps on the stack. double and int64 are 2 slots, the rest is one slot.
          There are no problems with reusing the same slot for a value of a different
          type. There are no alignment requirements either. }
        {if size<4 then
          size:=4;
        if not(size in [4,8]) then
          internalerror(2010121401);
        inherited alloctemp(list, size shr 2, 1, temptype, def, false, ref);}
      end;

    procedure ttgwasm.updateFirstTemp;
    begin
      firsttemp := localvars.varindex;
      if lasttemp<firsttemp then lasttemp := firsttemp;
    end;

    constructor ttgwasm.create;
      begin
        inherited create;
        direction := 1; // temp variables are allocated as "locals", and it starts with 0 and goes beyond!
        localvars:=TWasmLocalVars.Create;
      end;

    destructor ttgwasm.destroy;
      begin
        localvars.Free;
        inherited destroy;
      end;

    procedure ttgwasm.setfirsttemp(l: asizeint);
      begin
        firsttemp:=l;
        lasttemp:=l;
        localvars.varindex := l; //?
      end;


    procedure ttgwasm.getlocal(list: TAsmList; size: asizeint; alignment: shortint; def: tdef; var ref: treference);
      var
        wbt : TWasmBasicType;
        idx : integer;
      begin
        if defToWasmBasic(def, wbt) then
          alloclocalVarToRef(wbt, ref)
        else begin
          //Internalerror(2019091801); // no support of structural type
          inherited;
        end;
      end;


    procedure ttgwasm.gethltemp(list: TAsmList; def: tdef; forcesize: asizeint; temptype: ttemptype; out ref: treference);
      var
        wbt: TWasmBasicType;
      begin
        if Assigned(def) and defToWasmBasic(def, wbt) then begin
          allocLocalVarToRef(wbt, ref);
        end else
          inherited;
      end;

    procedure ttgwasm.gethltempmanaged(list: TAsmList; def: tdef; temptype: ttemptype; out ref: treference);
      begin
        inherited;
      end;

    procedure ttgwasm.allocLocalVarToRef(wbt: TWasmBasicType; out ref: treference);
      var
        idx : integer;
      begin
        idx := localvars.alloc(wbt);
        localVarToRef(idx, 1, ref);
      end;

    procedure ttgwasm.deallocLocalVar(wbt: TWasmBasicType; idx: integer);
      begin
        localvars.dealloc(wbt, idx);
      end;

    procedure ttgwasm.localVarToRef(idx: integer; size: integer; out ref: treference);
      begin
        reference_reset_base(ref, current_procinfo.framepointer,idx,ctempposinvalid,size,[]);
        ref.islocal := true;
        updateFirstTemp;
      end;

    function TWasmLocalVars.alloc(bt: TWasmBasicType): integer;
      var
        i : integer;
        lc : TWasmLocal;
        t  : TWasmLocal;
      begin
        lc := locv[bt];
        t := nil;
        while Assigned(lc) and (lc.inuse) do begin
          t := lc;
          lc := lc.next;
        end;
        if Assigned(lc) then begin
          lc.inuse := true;
        end else begin
          lc := TWasmLocal.Create(bt, varindex);
          if Assigned(t)
            then t.next := lc
            else locv[bt]:=lc;
          lc.inuse:=true;
          inc(varindex);

          if Assigned(last) then last.nextseq := lc;
          if not Assigned(first) then first := lc;
          last := lc;
        end;
        alloc := lc.index;

      end;

    procedure TWasmLocalVars.dealloc(bt: TWasmBasicType; index: integer);
      var
        lc : TWasmLocal;
      begin
        lc := locv[bt];
        while Assigned(lc) and (lc.index <> index) do
          lc := lc.next;
        if Assigned(lc) then lc.inuse := false;
      end;



initialization
  tgobjclass:=ttgwasm;
end.
