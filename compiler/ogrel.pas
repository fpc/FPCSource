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

      { TRelObjOutput }

      TRelObjOutput = class(tObjOutput)
      protected
        function writeData(Data:TObjData):boolean;override;
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
                                TRelObjOutput
*****************************************************************************}

    function TRelObjOutput.writeData(Data: TObjData): boolean;
      begin
        { todo: implement }
        result:=true;
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
