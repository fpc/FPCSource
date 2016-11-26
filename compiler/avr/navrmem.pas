{
    Copyright (c) 2016 by Florian Klaempfl

    Code generation for memory related nodes on the AVR

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
unit navrmem;

{$i fpcdefs.inc}

  interface

    uses
      nmem,ncgmem;

    type
      tavraddrnode = class(tcgaddrnode)
        procedure pass_generate_code;override;
      end;

  implementation

    uses
      verbose,
      cpubase,
      aasmdata,
      cgbase,cgutils,
      hlcgobj,
      node,
      pass_2;

    procedure tavraddrnode.pass_generate_code;
      var
        href : treference;
      begin
         secondpass(left);

         href:=left.location.reference;

         if ((href.addressmode=AM_UNCHANGED) and (href.offset=0) and not(assigned(href.symbol))) and
           ((href.base<>NR_NO) xor (href.index<>NR_NO)) then
           begin
             location_reset(location,LOC_CREGISTER,int_cgsize(resultdef.size));
             if href.base<>NR_NO then
               location.register:=href.base
             else if href.index<>NR_NO then
               location.register:=href.index
             else
               internalerror(2016112003);
           end
         else
           begin
             location_reset(location,LOC_REGISTER,int_cgsize(resultdef.size));
             location.register:=hlcg.getaddressregister(current_asmdata.CurrAsmList,resultdef);
             if not(left.location.loc in [LOC_REFERENCE,LOC_CREFERENCE]) then
               { on x86_64-win64, array of chars can be returned in registers, however,
                 when passing these arrays to other functions, the compiler wants to take
                 the address of the array so when the addrnode has been created internally,
                 we have to force the data into memory, see also tw14388.pp
               }
               if nf_internal in flags then
                 hlcg.location_force_mem(current_asmdata.CurrAsmList,left.location,left.resultdef)
               else
                 internalerror(2006111510);
             hlcg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,left.resultdef,resultdef,left.location.reference,location.register);
           end;
      end;

begin
  caddrnode:=tavraddrnode;
end.

