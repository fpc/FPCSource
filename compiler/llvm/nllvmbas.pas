{
    Copyright (c) 2015 by Jonas Maebe

    This unit implements llvm support for some basic nodes

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
unit nllvmbas;

{$i fpcdefs.inc}

interface

    uses
       nbas,ncgbas;

    type
       tllvmtempcreatenode = class(tcgtempcreatenode)
          procedure pass_generate_code;override;
       end;

  implementation

    uses
      aasmdata,
      cgbase,cgutils,
      llvmbase,aasmllvm
      ;

{*****************************************************************************
                          TTEMPCREATENODE
*****************************************************************************}

    procedure tllvmtempcreatenode.pass_generate_code;
      begin
        inherited;

        { if a temp is in a register and we never assign anything to it (e.g.
          because it's the register for an inlined function result that never
          gets assigned a value), then llvm will be confused the first time
          we try to read from it (since it's never been defined) -> always
          immediately assign undef to such registers }
        if tempinfo^.location.loc in [LOC_REGISTER,LOC_CREGISTER,LOC_FPUREGISTER,
             LOC_CFPUREGISTER,LOC_MMREGISTER,LOC_CMMREGISTER] then
          current_asmdata.CurrAsmList.concat(
            taillvm.op_reg_size_undef(la_bitcast,tempinfo^.location.register,tempinfo^.typedef)
          );
      end;


begin
   ctempcreatenode:=tllvmtempcreatenode;
end.
