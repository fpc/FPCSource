{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains high-level code generator support for ppc32

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
    aasmdata,
    symtype,
    cgbase,cgutils,hlcgobj,hlcgppc;

  type
    thlcgcpu = class(thlcgppcgen)
     procedure a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister); override;
     procedure a_load_subsetreg_subsetreg(list: TAsmlist; fromsubsetsize, tosubsetsize: tdef; const fromsreg, tosreg: tsubsetregister); override;
    protected
     procedure a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt); override;
    end;

  procedure create_hlcodegen;

implementation

  uses
    verbose,
    cpubase,aasmcpu,
    defutil,
    cgobj,cgcpu;

  { thlcgppc }

  procedure thlcgcpu.a_load_subsetreg_reg(list: TAsmList; subsetsize, tosize: tdef; const sreg: tsubsetregister; destreg: tregister);
    var
      cgsubsetsize,
      cgtosize: tcgsize;
    begin
      if (sreg.bitlen > 32) then
        internalerror(2008020701);
      cgsubsetsize:=def_cgsize(subsetsize);
      cgtosize:=def_cgsize(tosize);
      if (sreg.bitlen <> 32) then
        begin
          list.concat(taicpu.op_reg_reg_const_const_const(A_RLWINM,destreg,
            sreg.subsetreg,(32-sreg.startbit) and 31,32-sreg.bitlen,31));
          { types with a negative lower bound are always a base type (8, 16, 32 bits) }
          if (cgsubsetsize in [OS_S8..OS_S128]) then
            if ((sreg.bitlen mod 8) = 0) then
              begin
                cg.a_load_reg_reg(list,tcgsize2unsigned[cgsubsetsize],cgsubsetsize,destreg,destreg);
                cg.a_load_reg_reg(list,cgsubsetsize,cgtosize,destreg,destreg);
              end
            else
              begin
                cg.a_op_const_reg(list,OP_SHL,OS_INT,32-sreg.bitlen,destreg);
                cg.a_op_const_reg(list,OP_SAR,OS_INT,32-sreg.bitlen,destreg);
              end;
        end
      else
        cg.a_load_reg_reg(list,cgsubsetsize,cgtosize,sreg.subsetreg,destreg);
    end;


  procedure thlcgcpu.a_load_subsetreg_subsetreg(list: TAsmlist; fromsubsetsize, tosubsetsize: tdef; const fromsreg, tosreg: tsubsetregister);
    begin
      if (tosreg.bitlen>32) or (tosreg.startbit>31) then
        internalerror(2008020703);
      if (fromsreg.bitlen>=tosreg.bitlen) then
        list.concat(taicpu.op_reg_reg_const_const_const(A_RLWIMI,tosreg.subsetreg, fromsreg.subsetreg,
           (tosreg.startbit-fromsreg.startbit) and 31,
           32-tosreg.startbit-tosreg.bitlen,31-tosreg.startbit))
      else
        inherited a_load_subsetreg_subsetreg(list,fromsubsetsize,tosubsetsize,fromsreg,tosreg);
    end;


  procedure thlcgcpu.a_load_regconst_subsetreg_intern(list: TAsmList; fromsize, subsetsize: tdef; fromreg: tregister; const sreg: tsubsetregister; slopt: tsubsetloadopt);
    begin
      if (slopt in [SL_SETZERO,SL_SETMAX]) then
        inherited a_load_regconst_subsetreg_intern(list,fromsize,subsetsize,fromreg,sreg,slopt)
      else if (sreg.bitlen>32) then
        internalerror(2008020702)
      else if (sreg.bitlen<>32) then
        list.concat(taicpu.op_reg_reg_const_const_const(A_RLWIMI,sreg.subsetreg,fromreg,
          sreg.startbit,32-sreg.startbit-sreg.bitlen,31-sreg.startbit))
      else
        cg.a_load_reg_reg(list,def_cgsize(fromsize),def_cgsize(subsetsize),fromreg,sreg.subsetreg);
    end;



  procedure create_hlcodegen;
    begin
      hlcg:=thlcgcpu.create;
      create_codegen;
    end;


end.

