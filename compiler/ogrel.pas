{
    Copyright (c) 2020 by Nikolay Nikolov

    Contains the ASCII relocatable object file format (*.rel) reader and writer
    This is the object format used on the Z80 platforms.

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
unit ogrel;

{$i fpcdefs.inc}

interface

    uses
       { common }
       cclasses,globtype,
       { target }
       systems,
       { assembler }
       cpuinfo,cpubase,aasmbase,assemble,link,
       { output }
       ogbase,
       owbase;

    type

      { TRelObjData }

      TRelObjData = class(TObjData)
      public
        function sectionname(atype:TAsmSectiontype;const aname:string;aorder:TAsmSectionOrder):string;override;
        procedure writeReloc(Data:TRelocDataInt;len:aword;p:TObjSymbol;Reloctype:TObjRelocationType);override;
      end;

      { TRelObjOutput }

      TRelObjOutput = class(tObjOutput)
      protected
        function writeData(Data:TObjData):boolean;override;
      public
        constructor create(AWriter:TObjectWriter);override;
      end;

      { TRelAssembler }

      TRelAssembler = class(tinternalassembler)
        constructor create(info: pasminfo; smart:boolean);override;
      end;

implementation

    uses
       SysUtils,
       cutils,verbose,globals,
       fmodule,aasmtai,aasmdata,
       ogmap,
       version
       ;

{*****************************************************************************
                                TRelObjData
*****************************************************************************}

    function TRelObjData.sectionname(atype: TAsmSectiontype; const aname: string; aorder: TAsmSectionOrder): string;
      const
        secnames : array[TAsmSectiontype] of string[length('__DATA, __datacoal_nt,coalesced')] = ('','',
          '_CODE',
          '_DATA',
          '_DATA',
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
      begin
        result:=secnames[atype];
      end;

    procedure TRelObjData.writeReloc(Data: TRelocDataInt; len: aword; p: TObjSymbol; Reloctype: TObjRelocationType);
      begin
      end;

{*****************************************************************************
                                TRelObjOutput
*****************************************************************************}

    function TRelObjOutput.writeData(Data: TObjData): boolean;
      begin
        { todo: implement }
        result:=true;
      end;

    constructor TRelObjOutput.create(AWriter: TObjectWriter);
      begin
        inherited;
        cobjdata:=TRelObjData;
      end;

{*****************************************************************************
                                TRelAssembler
*****************************************************************************}

    constructor TRelAssembler.create(info: pasminfo; smart: boolean);
      begin
        inherited;
        CObjOutput:=TRelObjOutput;
      end;

{*****************************************************************************
                                  Initialize
*****************************************************************************}
    const
       as_z80_rel_info : tasminfo =
          (
            id     : as_z80_rel;
            idtxt  : 'REL';
            asmbin : '';
            asmcmd : '';
            supported_targets : [system_z80_embedded,system_z80_zxspectrum];
            flags : [af_outputbinary,af_smartlink_sections];
            labelprefix : '..@';
            labelmaxlen : -1;
            comment : '; ';
            dollarsign: '$';
          );

initialization
  RegisterAssembler(as_z80_rel_info,TRelAssembler);
end.
