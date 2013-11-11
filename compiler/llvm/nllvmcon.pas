{
    Copyright (c) 2013 by Jonas Maebe, member of the Free Pascal Compiler
    development team

    Generate llvm bitcode for constants

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
unit nllvmcon;

{$i fpcdefs.inc}

interface

    uses
       node,ncgcon;

    type
       tllvmrealconstnode = class(tcgrealconstnode)
          function pass_1 : tnode;override;
          procedure pass_generate_code;override;
       end;

implementation

    uses
      globtype,verbose,
      symdef,defutil,
      aasmdata,
      ncon,
      llvmbase,aasmllvm,hlcgobj,
      cgbase,cgutils;

{*****************************************************************************
                           tllvmrealconstnode
*****************************************************************************}

    function tllvmrealconstnode.pass_1 : tnode;
      begin
         result:=nil;
         expectloc:=LOC_FPUREGISTER;
      end;


    procedure tllvmrealconstnode.pass_generate_code;
      begin
         { llvm supports floating point constants directly }
         location_reset(location,LOC_FPUREGISTER,def_cgsize(resultdef));
         location.register:=hlcg.getfpuregister(current_asmdata.CurrAsmList,resultdef);
         case tfloatdef(resultdef).floattype of
           s32real,s64real:
             current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_fpconst_size(la_bitcast,location.register,resultdef,value_real,resultdef));
           { comp and currency are handled as int64 at the llvm level }
           s64comp,
           s64currency:
             current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_const_size(la_bitcast,location.register,resultdef,trunc(value_real),resultdef));
{$ifdef cpuextended}
           s80real,sc80real:
             current_asmdata.CurrAsmList.concat(taillvm.op_reg_size_fpconst80_size(la_bitcast,location.register,resultdef,value_real,resultdef));
{$endif cpuextended}
           else
             internalerror(2013102501);
         end;
      end;


begin
   crealconstnode:=tllvmrealconstnode;
end.
