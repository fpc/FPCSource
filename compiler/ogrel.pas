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
      private
        procedure writeString(const S: ansistring);
        procedure writeLine(const S: ansistring);
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

    function tohex(q: qword): string;
      begin
        result:=HexStr(q,16);
        while (Length(result)>1) and (result[1]='0') do
          delete(result,1,1);
      end;

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

    procedure TRelObjOutput.writeString(const S: ansistring);
      begin
        FWriter.write(S[1],Length(S));
      end;

    procedure TRelObjOutput.writeLine(const S: ansistring);
      begin
        writeString(S+#10)
      end;

    function TRelObjOutput.writeData(Data: TObjData): boolean;
      var
        global_symbols_count: Integer = 0;
        secidx, idx, i, j: Integer;
        objsym: TObjSymbol;
        objsec: TObjSection;
      begin
        global_symbols_count:=0;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TObjSymbol(Data.ObjSymbolList[i]);
            if objsym.bind in [AB_EXTERNAL,AB_GLOBAL] then
              Inc(global_symbols_count);
          end;

        writeLine('XL2');
        writeLine('H '+tohex(data.ObjSectionList.Count)+' areas '+tohex(global_symbols_count)+' global symbols');

        idx:=0;
        for i:=0 to Data.ObjSymbolList.Count-1 do
          begin
            objsym:=TObjSymbol(Data.ObjSymbolList[i]);
            if objsym.bind=AB_EXTERNAL then
              begin
                writeLine('S '+ApplyAsmSymbolRestrictions(objsym.Name)+' Ref0000');
                objsym.symidx:=idx;
                Inc(idx);
              end;
          end;
        secidx:=0;
        for i:=0 to Data.ObjSectionList.Count-1 do
          begin
            objsec:=TObjSection(Data.ObjSectionList[i]);
            writeLine('A '+objsec.Name+' size '+tohex(objsec.Size)+' flags 0 addr 0');
            objsec.SecSymIdx:=secidx;
            Inc(secidx);
            for j:=0 to Data.ObjSymbolList.Count-1 do
              begin
                objsym:=TObjSymbol(Data.ObjSymbolList[j]);
                if (objsym.bind=AB_GLOBAL) and (objsym.objsection=objsec) then
                  begin
                    writeLine('S '+ApplyAsmSymbolRestrictions(objsym.Name)+' Def'+HexStr(objsym.offset,4));
                    objsym.symidx:=idx;
                    Inc(idx);
                  end;
              end;
          end;
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
            labelmaxlen : 79;
            comment : '; ';
            dollarsign: '$';
          );

initialization
  RegisterAssembler(as_z80_rel_info,TRelAssembler);
end.
