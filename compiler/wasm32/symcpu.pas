{
    Copyright (c) 2014 by Florian Klaempfl

    Symbol table overrides for WebAssembly

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
unit symcpu;

{$i fpcdefs.inc}

interface

uses
  globtype,
  cpubase,
  aasmdata,
  symtype,
  symdef,symsym,
  cgutils;

type
  { defs }
  tcpufiledef = class(tfiledef)
  end;
  tcpufiledefclass = class of tcpufiledef;

  tcpuvariantdef = class(tvariantdef)
  end;
  tcpuvariantdefclass = class of tcpuvariantdef;

  tcpuformaldef = class(tformaldef)
  end;
  tcpuformaldefclass = class of tcpuformaldef;

  tcpuforwarddef = class(tforwarddef)
  end;
  tcpuforwarddefclass = class of tcpuforwarddef;

  tcpuundefineddef = class(tundefineddef)
  end;
  tcpuundefineddefclass = class of tcpuundefineddef;

  tcpuerrordef = class(terrordef)
  end;
  tcpuerrordefclass = class of tcpuerrordef;

  tcpupointerdef = class(tpointerdef)
  protected
    procedure ppuload_platform(ppufile: tcompilerppufile); override;
    procedure ppuwrite_platform(ppufile: tcompilerppufile); override;
  public
    { flag, indicating whether the pointer is a WebAssembly externref reference type }
    is_wasm_externref: boolean;
    constructor create_externref(def: tdef);
    function getcopy: tstoreddef; override;
    function GetTypeName: string; override;
    function compatible_with_pointerdef_size(ptr: tpointerdef): boolean; override;
  end;
  tcpupointerdefclass = class of tcpupointerdef;

  tcpurecorddef = class(trecorddef)
  end;
  tcpurecorddefclass = class of tcpurecorddef;

  tcpuimplementedinterface = class(timplementedinterface)
  end;
  tcpuimplementedinterfaceclass = class of tcpuimplementedinterface;

  tcpuobjectdef = class(tobjectdef)
  end;
  tcpuobjectdefclass = class of tcpuobjectdef;

  tcpuclassrefdef = class(tclassrefdef)
  end;
  tcpuclassrefdefclass = class of tcpuclassrefdef;

  tcpuarraydef = class(tarraydef)
  end;
  tcpuarraydefclass = class of tcpuarraydef;

  tcpuorddef = class(torddef)
  end;
  tcpuorddefclass = class of tcpuorddef;

  tcpufloatdef = class(tfloatdef)
  end;
  tcpufloatdefclass = class of tcpufloatdef;

  { tcpuprocvardef }

  tcpuprocvardef = class(tprocvardef)
    function create_functype: TWasmFuncType;
    function is_pushleftright: boolean; override;
  end;
  tcpuprocvardefclass = class of tcpuprocvardef;

  { tcpuprocdef }

  tcpuprocdef = class(tprocdef)
    frame_pointer_ref,
    base_pointer_ref: treference;
    { generated assembler code; used by WebAssembly backend so it can afterwards
      easily write out all methods grouped per class }
    exprasmlist  : TAsmList;
    promising_first_export_name: string;
    promising_last_export_name: string;
    destructor destroy; override;
    function create_functype: TWasmFuncType;
    function is_pushleftright: boolean; override;
    function suspending_wrapper_name: ansistring;
    function promising_wrapper_name(last:boolean): ansistring;
    procedure add_promising_export(aextname: ansistring;last:boolean);
  end;
  tcpuprocdefclass = class of tcpuprocdef;

  tcpustringdef = class(tstringdef)
  end;
  tcpustringdefclass = class of tcpustringdef;

  tcpuenumdef = class(tenumdef)
  end;
  tcpuenumdefclass = class of tcpuenumdef;

  tcpusetdef = class(tsetdef)
  end;
  tcpusetdefclass = class of tcpusetdef;

  { syms }
  tcpulabelsym = class(tlabelsym)
  end;
  tcpulabelsymclass = class of tcpulabelsym;

  tcpuunitsym = class(tunitsym)
  end;
  tcpuunitsymclass = class of tcpuunitsym;

  tcpuprogramparasym = class(tprogramparasym)
  end;
  tcpuprogramparasymclass = class(tprogramparasym);

  tcpunamespacesym = class(tnamespacesym)
  end;
  tcpunamespacesymclass = class of tcpunamespacesym;

  { tcpuprocsym }

  tcpuprocsym = class(tprocsym)
  end;
  tcpuprocsymclass = class of tcpuprocsym;

  tcputypesym = class(ttypesym)
  end;
  tcpuypesymclass = class of tcputypesym;

  tcpufieldvarsym = class(tfieldvarsym)
  end;
  tcpufieldvarsymclass = class of tcpufieldvarsym;

  tcpulocalvarsym = class(tlocalvarsym)
  end;
  tcpulocalvarsymclass = class of tcpulocalvarsym;

  tcpuparavarsym = class(tparavarsym)
  end;
  tcpuparavarsymclass = class of tcpuparavarsym;

  tcpustaticvarsym = class(tstaticvarsym)
    function is_wasm_global: Boolean;
    function try_get_wasm_global_vardef_type(out res: TWasmBasicType): Boolean;
    function get_wasm_global_vardef_type: TWasmBasicType;
    function has_valid_wasm_global_vardef_type: Boolean;
  end;
  tcpustaticvarsymclass = class of tcpustaticvarsym;

  tcpuabsolutevarsym = class(tabsolutevarsym)
  end;
  tcpuabsolutevarsymclass = class of tcpuabsolutevarsym;

  tcpupropertysym = class(tpropertysym)
  end;
  tcpupropertysymclass = class of tcpupropertysym;

  tcpuconstsym = class(tconstsym)
  end;
  tcpuconstsymclass = class of tcpuconstsym;

  tcpuenumsym = class(tenumsym)
  end;
  tcpuenumsymclass = class of tcpuenumsym;

  tcpusyssym = class(tsyssym)
  end;
  tcpusyssymclass = class of tcpusyssym;


const
  pbestrealtype : ^tdef = @s64floattype;

  {# Returns true if p is a WebAssembly funcref reference type }
  function is_wasm_funcref(p : tdef): boolean;

  {# Returns true if p is a WebAssembly externref reference type }
  function is_wasm_externref(p : tdef): boolean;

  {# Returns true if p is a WebAssembly reference type (funcref or externref) }
  function is_wasm_reference_type(p : tdef): boolean;

implementation

  uses
    verbose,cutils,cclasses,globals,cgbase,
    symconst,symbase,symtable,symcreat,wasmdef,
    defutil,
    pdecsub,pparautl,paramgr,
    // high-level code generator is needed to get access to type index for ncall
    hlcgobj,hlcgcpu,
    tgcpu
    ;

  function is_wasm_funcref(p: tdef): boolean;
    begin
      result:=(p.typ=procvardef) and (po_wasm_funcref in tprocvardef(p).procoptions);
    end;

  function is_wasm_externref(p: tdef): boolean;
    begin
      result:=(p.typ=pointerdef) and (tcpupointerdef(p).is_wasm_externref);
    end;

  function is_wasm_reference_type(p: tdef): boolean;
    begin
      result:=is_wasm_funcref(p) or is_wasm_externref(p);
    end;


  {****************************************************************************
                               tcpuproptertysym
  ****************************************************************************}



{****************************************************************************
                             tcpuenumdef
****************************************************************************}


{****************************************************************************
                             tcpupointerdef
****************************************************************************}


  procedure tcpupointerdef.ppuload_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      is_wasm_externref:=ppufile.getboolean;
    end;


  procedure tcpupointerdef.ppuwrite_platform(ppufile: tcompilerppufile);
    begin
      inherited;
      ppufile.putboolean(is_wasm_externref);
    end;


  constructor tcpupointerdef.create_externref(def: tdef);
    begin
      inherited create(def);
      is_wasm_externref:=true;
    end;


  function tcpupointerdef.getcopy: tstoreddef;
    begin
      result:=inherited;
      tcpupointerdef(result).is_wasm_externref:=is_wasm_externref;
    end;


  function tcpupointerdef.GetTypeName: string;
    begin
      result:=inherited;
      if is_wasm_externref then
        result:=result+';wasmexternref';
    end;


  function tcpupointerdef.compatible_with_pointerdef_size(ptr: tpointerdef): boolean;
    begin
      result:=tcpupointerdef(ptr).is_wasm_externref=is_wasm_externref;
    end;


{****************************************************************************
                             tcpuprocdef
****************************************************************************}


  function create_functype_common(pd: tabstractprocdef): TWasmFuncType;
    var
      i: Integer;
      prm: tcpuparavarsym;
      bt: TWasmBasicType;
    begin
      pd.init_paraloc_info(callerside);
      result:=TWasmFuncType.Create([],[]);
      if Assigned(pd.paras) and (pd.paras.Count>0) then
        begin
          for i:=0 to pd.paras.Count-1 do
            begin
              prm := tcpuparavarsym(pd.paras[i]);
              if is_wasm_funcref(prm.vardef) then
                result.add_param(wbt_funcref)
              else if is_wasm_externref(prm.vardef) then
                result.add_param(wbt_externref)
              else case prm.paraloc[callerside].Size of
                OS_8..OS_32, OS_S8..OS_S32:
                  result.add_param(wbt_i32);
                OS_64, OS_S64:
                  result.add_param(wbt_i64);
                OS_F32:
                  result.add_param(wbt_f32);
                OS_F64:
                  result.add_param(wbt_f64);
              else
                begin
{$ifdef EXTDEBUG}
                  Writeln('Unsupported caller side parameter type: ', prm.paraloc[callerside].Size);
{$endif EXTDEBUG}
                  // unsupported calleeside parameter type
                  Internalerror(2019093001);
                end;
              end;
            end;
        end;
      if Assigned(pd.returndef) and (pd.returndef.size>0) and
         not paramanager.ret_in_param(pd.returndef,pd) then
        begin
          if not defToWasmBasic(pd.returndef,bt) then
            bt:=wbt_i32;
          result.add_result(bt);
        end;
    end;


  destructor tcpuprocdef.destroy;
    begin
      exprasmlist.free;
      inherited destroy;
    end;


  function tcpuprocdef.create_functype: TWasmFuncType;
    begin
      Result:=create_functype_common(self);
    end;


  function tcpuprocdef.is_pushleftright: boolean;
    begin
      Result:=true;
    end;


  function tcpuprocdef.suspending_wrapper_name: ansistring;
    begin
      Result:='__fpc_wasm_suspending_'+procsym.realname;
    end;


  function tcpuprocdef.promising_wrapper_name(last: boolean): ansistring;
    begin
      if last then
        Result:='__fpc_wasm_promising_last_'+procsym.realname
      else
        Result:='__fpc_wasm_promising_first_'+procsym.realname;
    end;


  procedure tcpuprocdef.add_promising_export(aextname: ansistring; last: boolean);
    begin
      if (synthetickind<>tsk_none) and (synthetickind<>tsk_wasm_promising) then
        internalerror(2023061301);
      synthetickind:=tsk_wasm_promising;
      if last then
        begin
          if promising_last_export_name<>'' then
            internalerror(2023061601);
          promising_last_export_name:=aextname;
        end
      else
        begin
          if promising_first_export_name<>'' then
            internalerror(2023061602);
          promising_first_export_name:=aextname;
        end;
    end;


{****************************************************************************
                             tcpuprocvardef
****************************************************************************}

    function tcpuprocvardef.create_functype: TWasmFuncType;
      begin
        Result:=create_functype_common(Self);
      end;

    function tcpuprocvardef.is_pushleftright: boolean;
      begin
        Result:=true;
      end;


{****************************************************************************
                             tcpuprocsym
****************************************************************************}

{****************************************************************************
                             tcpustaticvarsym
****************************************************************************}

    function tcpustaticvarsym.is_wasm_global: Boolean;
      begin
        Result:=UpCase(section)='WEBASSEMBLY.GLOBAL';
      end;

    function tcpustaticvarsym.try_get_wasm_global_vardef_type(out res: TWasmBasicType): Boolean;
      begin
        Result:=True;
        if is_wasm_externref(vardef) then
          res:=wbt_externref
        else if is_wasm_funcref(vardef) then
          res:=wbt_funcref
        else if is_64bitint(vardef) then
          res:=wbt_i64
        else if is_pointer(vardef) then
          res:=wbt_i32
        else if is_32bitint(vardef) then
          res:=wbt_i32
        else if is_single(vardef) then
          res:=wbt_f32
        else if is_double(vardef) then
          res:=wbt_f64
        else
          Result:=False;
      end;

    function tcpustaticvarsym.get_wasm_global_vardef_type: TWasmBasicType;
      begin
        if not try_get_wasm_global_vardef_type(Result) then
          internalerror(2022072501);
      end;

    function tcpustaticvarsym.has_valid_wasm_global_vardef_type: Boolean;
      var
        TempWBT: TWasmBasicType;
      begin
        result:=try_get_wasm_global_vardef_type(TempWBT);
      end;

{****************************************************************************
                             tcpufieldvarsym
****************************************************************************}


initialization
  { used tdef classes }
  cfiledef:=tcpufiledef;
  cvariantdef:=tcpuvariantdef;
  cformaldef:=tcpuformaldef;
  cforwarddef:=tcpuforwarddef;
  cundefineddef:=tcpuundefineddef;
  cerrordef:=tcpuerrordef;
  cpointerdef:=tcpupointerdef;
  crecorddef:=tcpurecorddef;
  cimplementedinterface:=tcpuimplementedinterface;
  cobjectdef:=tcpuobjectdef;
  cclassrefdef:=tcpuclassrefdef;
  carraydef:=tcpuarraydef;
  corddef:=tcpuorddef;
  cfloatdef:=tcpufloatdef;
  cprocvardef:=tcpuprocvardef;
  cprocdef:=tcpuprocdef;
  cstringdef:=tcpustringdef;
  cenumdef:=tcpuenumdef;
  csetdef:=tcpusetdef;

  { used tsym classes }
  clabelsym:=tcpulabelsym;
  cunitsym:=tcpuunitsym;
  cprogramparasym:=tcpuprogramparasym;
  cnamespacesym:=tcpunamespacesym;
  cprocsym:=tcpuprocsym;
  ctypesym:=tcputypesym;
  cfieldvarsym:=tcpufieldvarsym;
  clocalvarsym:=tcpulocalvarsym;
  cparavarsym:=tcpuparavarsym;
  cstaticvarsym:=tcpustaticvarsym;
  cabsolutevarsym:=tcpuabsolutevarsym;
  cpropertysym:=tcpupropertysym;
  cconstsym:=tcpuconstsym;
  cenumsym:=tcpuenumsym;
  csyssym:=tcpusyssym;
end.

