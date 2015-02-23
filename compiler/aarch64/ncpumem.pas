{
    Copyright (c) 2014 by Jonas Maebe

    Generate AArch64 code for in memory related nodes

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
unit ncpumem;

{$i fpcdefs.inc}

interface

  uses
    globtype,
    node,nmem,ncgmem;

  type
    taarch64loadparentfpnode = class(tcgloadparentfpnode)
      procedure pass_generate_code; override;
    end;

implementation

  uses
    aasmdata,cgbase,cpubase,
    cgobj;

  { taarch64loadparentfpnode }

  procedure taarch64loadparentfpnode.pass_generate_code;
    begin
      inherited pass_generate_code;
      { see the comments in tcgaarch64.g_proc_entry }
      if (location.loc in [LOC_REGISTER,LOC_CREGISTER]) and
         (location.register=NR_STACK_POINTER_REG) then
        if (kind=lpf_forpara) then
          location.register:=NR_FRAME_POINTER_REG
        else
          begin
            { load stack pointer in a different register, as many instructions
              cannot directly work with the stack pointer. The register
              allocator can merge them if possible }
            location.register:=cg.getaddressregister(current_asmdata.CurrAsmList);
            cg.a_load_reg_reg(current_asmdata.CurrAsmList,OS_ADDR,OS_ADDR,NR_STACK_POINTER_REG,location.register);
            location.loc:=LOC_REGISTER;
          end;
    end;

begin
  cloadparentfpnode:=taarch64loadparentfpnode;
end.
