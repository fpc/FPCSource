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

    function TWasmObjData.sectionname(atype: TAsmSectiontype;
        const aname: string; aorder: TAsmSectionOrder): string;
      begin
        result:='todosectionname';
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
        result:=false;
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
