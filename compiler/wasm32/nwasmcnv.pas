{
    Copyright (c) 1998-2020 by Florian Klaempfl and Nikolay Nikolov

    Generate WebAssembly code for type converting nodes

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
unit nwasmcnv;

{$i fpcdefs.inc}

interface

    uses
      ncnv,ncgcnv;

    type

       { twasmtypeconvnode }

       twasmtypeconvnode = class(tcgtypeconvnode)
       protected
         procedure second_int_to_bool;override;
       end;

implementation

   uses
      globals,aasmdata,
      defutil,
      cgbase,cgutils,pass_2,
      hlcgobj,hlcgcpu;


{ twasmtypeconvnode }

    procedure twasmtypeconvnode.second_int_to_bool;
      begin
        secondpass(left);
        if codegenerror then
          exit;
        thlcgwasm(hlcg).a_load_loc_stack(current_asmdata.CurrAsmList,left.resultdef,left.location);
        thlcgwasm(hlcg).a_load_const_stack(current_asmdata.CurrAsmList,left.resultdef,0,R_INTREGISTER);
        thlcgwasm(hlcg).a_cmp_stack_stack(current_asmdata.CurrAsmList,left.resultdef,OC_NE);
        thlcgwasm(hlcg).resize_stack_int_val(current_asmdata.CurrAsmList,left.resultdef,resultdef,false);
        location_reset(location,LOC_REGISTER,def_cgsize(resultdef));
        location.register := hlcg.getintregister(current_asmdata.CurrAsmList,resultdef);
        thlcgwasm(hlcg).a_load_stack_loc(current_asmdata.CurrAsmList,resultdef,location);
      end;

begin
  ctypeconvnode:=twasmtypeconvnode;
end.
