{
    Copyright (c) 1998-2023 by Florian Klaempfl and Nikolay Nikolov

    Generate WebAssembly code for memory related nodes

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

 ****************************************************************************}
unit nwasmmem;

{$i fpcdefs.inc}

interface

    uses
      node,nmem,ncgmem;

    type

      { twasmaddrnode }

      twasmaddrnode = class(tcgaddrnode)
        function pass_typecheck:tnode;override;
      end;

implementation

    uses
      globals,
      verbose,
      symcpu;

    { twasmaddrnode }

    function twasmaddrnode.pass_typecheck: tnode;
    begin
      Result:=inherited;
      if codegenerror then
       exit;

      if assigned(left) and is_wasm_externref(left.resultdef) then
        begin
          CGMessagePos(left.fileinfo,type_e_cannot_take_address_of_wasm_externref);
          result:=nil;
          exit;
        end;
    end;

begin
  caddrnode:=twasmaddrnode;
end.
