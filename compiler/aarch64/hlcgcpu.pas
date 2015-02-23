{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines to create a pass-through high-level code
    generator. This is used by most regular code generators.

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

unit hlcgcpu;

{$i fpcdefs.inc}

interface

  uses
    symtype,
    aasmdata,
    cgbase,cgutils,
    hlcgobj, hlcg2ll;

  type
    thlcgaarch64 = class(thlcg2ll)
      procedure a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister); override;
      procedure a_load_subsetreg_subsetreg(list: TAsmlist; fromsubsetsize, tosubsetsize: tdef; const fromsreg, tosreg: tsubsetregister); override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    defutil,
    cpubase,aasmcpu,
    cgobj,cgcpu;

  procedure thlcgaarch64.a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);
    var
      op: tasmop;
      tocgsize: tcgsize;
      tmpdestreg: tregister;
    begin
      tocgsize:=def_cgsize(tosize);
      if (sreg.startbit<>0) or
         not(sreg.bitlen in [32,64]) then
        begin
          if is_signed(subsetsize) then
            op:=A_SBFX
          else
            op:=A_UBFX;
          { source and destination register of SBFX/UBFX have to be the same size }
          if (sreg.subsetregsize in [OS_64,OS_S64]) and
             not(tocgsize in [OS_64,OS_S64]) then
            tmpdestreg:=cg.getintregister(list,OS_64)
          else if not(sreg.subsetregsize in [OS_64,OS_S64]) and
             (tocgsize in [OS_64,OS_S64]) then
            tmpdestreg:=cg.getintregister(list,OS_32)
          else
            tmpdestreg:=destreg;
          list.concat(taicpu.op_reg_reg_const_const(op,tmpdestreg,sreg.subsetreg,sreg.startbit,sreg.bitlen));
          { need to sign extend further or truncate? }
          if (sreg.subsetregsize=OS_S64) and
             not(tocgsize in [OS_64,OS_S64]) then
            cg.a_load_reg_reg(list,OS_S64,tocgsize,tmpdestreg,destreg)
          else if is_signed(subsetsize) and
             (tocgsize in [OS_8,OS_16]) then
            cg.a_load_reg_reg(list,OS_32,tocgsize,tmpdestreg,destreg)
          else if tmpdestreg<>destreg then
            cg.a_load_reg_reg(list,def_cgsize(subsetsize),tocgsize,tmpdestreg,destreg)
        end
      else
        cg.a_load_reg_reg(list,def_cgsize(subsetsize),tocgsize,sreg.subsetreg,destreg);
    end;


  procedure thlcgaarch64.a_load_subsetreg_subsetreg(list: TAsmlist; fromsubsetsize, tosubsetsize: tdef; const fromsreg, tosreg: tsubsetregister);
    var
      fromreg, toreg: tregister;

    procedure getfromtoregs;
      begin
        if (fromsreg.subsetregsize in [OS_S64,OS_64])<>
           (tosreg.subsetregsize in [OS_S64,OS_64]) then
          begin
            fromreg:=cg.makeregsize(list,fromsreg.subsetreg,OS_64);
            toreg:=cg.makeregsize(list,tosreg.subsetreg,OS_64);
          end
        else
          begin
            fromreg:=fromsreg.subsetreg;
            toreg:=tosreg.subsetreg;
          end;
      end;

    begin
      if (tosreg.startbit=0) and
         (fromsreg.bitlen>=tosreg.bitlen) then
        begin
          getfromtoregs;
          list.concat(taicpu.op_reg_reg_const_const(A_BFXIL,toreg,fromreg,fromsreg.startbit,tosreg.bitlen))
        end
      else if (fromsreg.startbit=0) and
         (fromsreg.bitlen>=tosreg.bitlen) then
        begin
          getfromtoregs;
          list.concat(taicpu.op_reg_reg_const_const(A_BFI,toreg,fromreg,tosreg.startbit,tosreg.bitlen))
        end
      else
        inherited;
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgaarch64.create;
      create_codegen;
    end;


end.
