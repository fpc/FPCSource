{
    Copyright (c) 2021 by Nikolay Nikolov

    Contains the WebAssembly binary module format reader and writer

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
unit ogwasm;

{$i fpcdefs.inc}

interface

    uses
      { common }
      globtype,
      { target }
      systems,
      { assembler }
      aasmbase,assemble,
      { WebAssembly module format definitions }
      wasmbase,
      { output }
      ogbase,
      owbase;

    type

      { TWasmObjData }

      TWasmObjData = class(TObjData)
      private
        function is_smart_section(atype:TAsmSectiontype):boolean;
        function sectionname_gas(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;
      public
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
        procedure writeReloc(Data:TRelocDataInt;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);override;
      end;

      { TWasmObjOutput }

      TWasmObjOutput = class(tObjOutput)
      protected
        function writeData(Data:TObjData):boolean;override;
      public
        constructor create(AWriter:TObjectWriter);override;
      end;

      { TWasmAssembler }

      TWasmAssembler = class(tinternalassembler)
        constructor create(info: pasminfo; smart:boolean);override;
      end;

implementation

{****************************************************************************
                                TWasmObjData
****************************************************************************}

    function TWasmObjData.is_smart_section(atype: TAsmSectiontype): boolean;
      begin
        { For bss we need to set some flags that are target dependent,
          it is easier to disable it for smartlinking. It doesn't take up
          filespace }
        result:=not(target_info.system in systems_darwin) and
           create_smartlink_sections and
           (atype<>sec_toc) and
           (atype<>sec_user) and
           { on embedded systems every byte counts, so smartlink bss too }
           ((atype<>sec_bss) or (target_info.system in (systems_embedded+systems_freertos)));
      end;

    function TWasmObjData.sectionname_gas(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '.text',
          '.data',
{ why doesn't .rodata work? (FK) }
{ sometimes we have to create a data.rel.ro instead of .rodata, e.g. for  }
{ vtables (and anything else containing relocations), otherwise those are }
{ not relocated properly on e.g. linux/ppc64. g++ generates there for a   }
{ vtable for a class called Window:                                       }
{ .section .data.rel.ro._ZTV6Window,"awG",@progbits,_ZTV6Window,comdat    }
{ TODO: .data.ro not yet working}
{$if defined(arm) or defined(riscv64) or defined(powerpc)}
          '.rodata',
{$else defined(arm) or defined(riscv64) or defined(powerpc)}
          '.data',
{$endif defined(arm) or defined(riscv64) or defined(powerpc)}
          '.rodata',
          '.bss',
          '.threadvar',
          '.pdata',
          '', { stubs }
          '__DATA,__nl_symbol_ptr',
          '__DATA,__la_symbol_ptr',
          '__DATA,__mod_init_func',
          '__DATA,__mod_term_func',
          '.stab',
          '.stabstr',
          '.idata$2','.idata$4','.idata$5','.idata$6','.idata$7','.edata',
          '.eh_frame',
          '.debug_frame','.debug_info','.debug_line','.debug_abbrev','.debug_aranges','.debug_ranges',
          '.fpc',
          '.toc',
          '.init',
          '.fini',
          '.objc_class',
          '.objc_meta_class',
          '.objc_cat_cls_meth',
          '.objc_cat_inst_meth',
          '.objc_protocol',
          '.objc_string_object',
          '.objc_cls_meth',
          '.objc_inst_meth',
          '.objc_cls_refs',
          '.objc_message_refs',
          '.objc_symbols',
          '.objc_category',
          '.objc_class_vars',
          '.objc_instance_vars',
          '.objc_module_info',
          '.objc_class_names',
          '.objc_meth_var_types',
          '.objc_meth_var_names',
          '.objc_selector_strs',
          '.objc_protocol_ext',
          '.objc_class_ext',
          '.objc_property',
          '.objc_image_info',
          '.objc_cstring_object',
          '.objc_sel_fixup',
          '__DATA,__objc_data',
          '__DATA,__objc_const',
          '.objc_superrefs',
          '__DATA, __datacoal_nt,coalesced',
          '.objc_classlist',
          '.objc_nlclasslist',
          '.objc_catlist',
          '.obcj_nlcatlist',
          '.objc_protolist',
          '.stack',
          '.heap',
          '.gcc_except_table',
          '.ARM.attributes'
        );
      var
        sep     : string[3];
        secname : string;
      begin
        if (atype=sec_fpc) and (Copy(aname,1,3)='res') then
          begin
            result:=secname+'.'+aname;
            exit;
          end;

        if atype=sec_threadvar then
          begin
            if (target_info.system in (systems_windows+systems_wince)) then
              secname:='.tls'
            else if (target_info.system in systems_linux) then
              secname:='.tbss';
          end;

        { go32v2 stub only loads .text and .data sections, and allocates space for .bss.
          Thus, data which normally goes into .rodata and .rodata_norel sections must
          end up in .data section }
        if (atype in [sec_rodata,sec_rodata_norel]) and
          (target_info.system in [system_i386_go32v2,system_m68k_palmos]) then
          secname:='.data';

        { Windows correctly handles reallocations in readonly sections }
        if (atype=sec_rodata) and
          (target_info.system in systems_all_windows+systems_nativent-[system_i8086_win16]) then
          secname:='.rodata';

        { section type user gives the user full controll on the section name }
        if atype=sec_user then
          secname:=aname;

        if is_smart_section(atype) and (aname<>'') then
          begin
            case aorder of
              secorder_begin :
                sep:='.b_';
              secorder_end :
                sep:='.z_';
              else
                sep:='.n_';
            end;
            result:=secname+sep+aname
          end
        else
          result:=secname;
      end;

    function TWasmObjData.sectionname(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      begin
        if (atype=sec_fpc) or (atype=sec_threadvar) then
          atype:=sec_data;
        Result:=sectionname_gas(atype, aname, aorder);
      end;

    procedure TWasmObjData.writeReloc(Data: TRelocDataInt; len: aword;
        p: TObjSymbol; Reloctype: TObjRelocationType);
      begin
      end;

{****************************************************************************
                               TWasmObjOutput
****************************************************************************}

    function TWasmObjOutput.writeData(Data:TObjData):boolean;
      begin
        Writer.write(WasmModuleMagic,SizeOf(WasmModuleMagic));
        Writer.write(WasmVersion,SizeOf(WasmVersion));
        result:=true;
      end;

    constructor TWasmObjOutput.create(AWriter: TObjectWriter);
      begin
        inherited;
        cobjdata:=TWasmObjData;
      end;

{****************************************************************************
                               TWasmAssembler
****************************************************************************}

    constructor TWasmAssembler.Create(info: pasminfo; smart:boolean);
      begin
        inherited;
        CObjOutput:=TWasmObjOutput;
      end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}
{$ifdef wasm32}
    const
       as_wasm32_wasm_info : tasminfo =
          (
            id     : as_wasm32_wasm;
            idtxt  : 'OMF';
            asmbin : '';
            asmcmd : '';
            supported_targets : [system_wasm32_embedded,system_wasm32_wasi];
            flags : [af_outputbinary,af_smartlink_sections];
            labelprefix : '..@';
            labelmaxlen : -1;
            comment : '; ';
            dollarsign: '$';
          );
{$endif wasm32}

initialization
{$ifdef wasm32}
  RegisterAssembler(as_wasm32_wasm_info,TWasmAssembler);
{$endif wasm32}
end.
