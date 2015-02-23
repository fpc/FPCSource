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
    cgbase,
    node,nmem,ncgmem;

  type
    taarch64loadparentfpnode = class(tcgloadparentfpnode)
      procedure pass_generate_code; override;
    end;

    taarch64vecnode = class(tcgvecnode)
      procedure update_reference_reg_mul(maybe_const_reg: tregister; l: aint); override;
    end;

implementation

  uses
    cutils,verbose,
    aasmdata,cpubase,
    cgutils,
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


  { taarch64vecnode }

  procedure taarch64vecnode.update_reference_reg_mul(maybe_const_reg: tregister; l: aint);
    var
      base: tregister;
      oldoffset: asizeint;
      shift: byte;
    begin
      { we can only scale the index by shl 1..4 }
      if not(l in [2,4,8,16]) then
        begin
          inherited;
          exit;
        end;
      { we need a base set and an index available }
      if (location.reference.base=NR_NO) or
         (location.reference.index<>NR_NO) then
        begin
          { don't integrate the offset yet, make_simple_ref() may be able to
            handle it more efficiently later (unless an offset is all we have
            -> optimization for someone that wants to add support for AArch64
            embedded targets) }
          oldoffset:=location.reference.offset;
          location.reference.offset:=0;
          base:=cg.getaddressregister(current_asmdata.CurrAsmList);
          cg.a_loadaddr_ref_reg(current_asmdata.CurrAsmList,location.reference,base);
          reference_reset_base(location.reference,base,oldoffset,location.reference.alignment);
        end;
      shift:=BsfDWord(l);
      location.reference.index:=maybe_const_reg;
      location.reference.shiftmode:=SM_LSL;
      location.reference.shiftimm:=shift;
      location.reference.alignment:=newalignment(location.reference.alignment,l);
    end;


begin
  cloadparentfpnode:=taarch64loadparentfpnode;
  cvecnode:=taarch64vecnode;
end.
