{
    Copyright (c) 1998-2010 by Florian Klaempfl and Jonas Maebe
    Member of the Free Pascal development team

    This unit contains routines high-level code generator support shared by
    ppc32 and ppc64

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
unit hlcgppc;

{$mode objfpc}

interface

uses
  aasmdata,
  symtype,
  cgbase,cgutils,hlcgobj,hlcg2ll;

type
  thlcgppcgen = class(thlcg2ll)
   protected
    procedure a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tdef; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister); override;
  end;

implementation

  uses
    cpubase,globtype,
    symdef,defutil;

{ thlcgppc }

  procedure thlcgppcgen.a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tdef; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister);
    var
      fromsreg, tosreg: tsubsetregister;
      restbits: byte;
    begin
      restbits:=(sref.bitlen-(loadbitsize-sref.startbit));
      if is_signed(subsetsize) then
        begin
         { sign extend }
         a_op_const_reg(list,OP_SHL,osuinttype,AIntBits-loadbitsize+sref.startbit,valuereg);
         a_op_const_reg(list,OP_SAR,osuinttype,AIntBits-sref.bitlen,valuereg);
        end
      else
        begin
          a_op_const_reg(list,OP_SHL,osuinttype,restbits,valuereg);
          { mask other bits }
          if (sref.bitlen<>AIntBits) then
            a_op_const_reg(list,OP_AND,osuinttype,(aword(1) shl sref.bitlen)-1,valuereg);
        end;
      { use subsetreg routine, it may have been overridden with an optimized version }
      fromsreg.subsetreg:=extra_value_reg;
      fromsreg.subsetregsize:=OS_INT;
      { subsetregs always count bits from right to left }
      fromsreg.startbit:=loadbitsize-restbits;
      fromsreg.bitlen:=restbits;

      tosreg.subsetreg:=valuereg;
      tosreg.subsetregsize:=OS_INT;
      tosreg.startbit:=0;
      tosreg.bitlen:=restbits;

      a_load_subsetreg_subsetreg(list,subsetsize,subsetsize,fromsreg,tosreg);
    end;

end.

