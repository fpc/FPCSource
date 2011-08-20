{
    Copyright (c) 1998-2011 by Florian Klaempfl and Jonas Maebe

    Generate assembler for constant nodes for the JVM

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
unit njvmcon;

{$i fpcdefs.inc}

interface

    uses
       node,ncon,ncgcon;

    type
       tjvmrealconstnode = class(tcgrealconstnode)
          procedure pass_generate_code;override;
       end;

implementation

    uses
      globtype,
      aasmdata,defutil,
      cgbase,hlcgobj,hlcgcpu,cgutils
      ;


{*****************************************************************************
                           TJVMREALCONSTNODE
*****************************************************************************}

    procedure tjvmrealconstnode.pass_generate_code;
      begin
        location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
        location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);
        thlcgjvm(hlcg).a_loadfpu_const_stack(current_asmdata.CurrAsmList,resultdef,value_real);
        thlcgjvm(hlcg).a_load_stack_reg(current_asmdata.CurrAsmList,resultdef,location.register);
      end;


begin
   crealconstnode:=tjvmrealconstnode;
end.
