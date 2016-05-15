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
    symtype,
    node,nmem,ncgmem;

  type
    taarch64loadparentfpnode = class(tcgloadparentfpnode)
      procedure pass_generate_code; override;
    end;

    taarch64vecnode = class(tcgvecnode)
     protected
      function valid_index_size(size: tcgsize): boolean; override;
     public
       procedure update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint); override;
    end;

implementation

  uses
    cutils,verbose,
    defutil,
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

  function taarch64vecnode.valid_index_size(size: tcgsize): boolean;
    begin
      { all sizes are ok if we handle the "reference reg mul", because
         a) we use a 64 bit register for 64 bit values, and a 32 bit one (that
            we can sign/zero-extend inside the reference) for smaller values
         b) for values < 32 bit, the entire 32 bit register always contains the
            sign/zero-extended version of the value }
      result:=
        not is_packed_array(left.resultdef) and
        (get_mul_size in [1,2,4,8,16]);
    end;


  procedure taarch64vecnode.update_reference_reg_mul(maybe_const_reg: tregister; regsize: tdef; l: aint);
    var
      base: tregister;
      oldoffset: asizeint;
      shift: byte;
    begin
      { we can only scale the index by shl 0..4 }
      if not(l in [1,2,4,8,16]) then
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
      { sign/zero-extend? }
      if regsize.size=8 then
        if shift<>0 then
          location.reference.shiftmode:=SM_LSL
        else
          location.reference.shiftmode:=SM_NONE
      else if is_signed(regsize) then
        location.reference.shiftmode:=SM_SXTW
      else if shift<>0 then
        location.reference.shiftmode:=SM_UXTW
      else
        { the upper 32 bits are always already zero-extended -> just use 64 bit
          register }
        location.reference.index:=cg.makeregsize(current_asmdata.CurrAsmList,location.reference.index,OS_64);
      location.reference.shiftimm:=shift;
      location.reference.alignment:=newalignment(location.reference.alignment,l);
    end;


begin
  cloadparentfpnode:=taarch64loadparentfpnode;
  cvecnode:=taarch64vecnode;
end.
