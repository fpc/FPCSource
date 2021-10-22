{
    Copyright (c) 2021 by Nikolay Nikolov

    This unit implements code generation for some basic nodes on the
    WebAssembly target.

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
unit nwasmbas;

{$i fpcdefs.inc}

interface

    uses
      nbas,ncgbas;

    type

      { twasmblocknode }

      twasmblocknode=class(tcgblocknode)
        procedure pass_generate_code; override;
      end;

implementation

    uses
      aasmdata,aasmcpu,
      cpubase,
      hlcgobj,hlcgcpu,
      node;

    { twasmblocknode }

    procedure twasmblocknode.pass_generate_code;
      begin
        if nf_block_with_exit in flags then
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_none(a_block));
          end;

        inherited pass_generate_code;

        if nf_block_with_exit in flags then
          begin
            current_asmdata.CurrAsmList.concat(taicpu.op_none(a_end_block));
          end;
      end;

begin
  cblocknode:=twasmblocknode;
end.
