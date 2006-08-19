{
    Copyright (c) 2006 by Florian Klaempfl

    This unit implements the common part of the code generator for the PowerPC

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
unit cgppc;

{$i fpcdefs.inc}

  interface

    uses
       globtype,symtype,symdef,
       cgbase,cgobj,
       aasmbase,aasmcpu,aasmtai,aasmdata,
       cpubase,cpuinfo,cgutils,rgcpu;

    type
      tcgppcgen = class(tcg)
        procedure a_call_ref(list : TAsmList;ref: treference); override;
       protected
        procedure a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tcgsize; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister); override;
     end;

  implementation

    uses
       globals,verbose,systems,cutils,
       symconst,symsym,fmodule,
       rgobj,tgobj,cpupi,procinfo,paramgr;

  procedure tcgppcgen.a_call_ref(list : TAsmList;ref: treference);
    var
      tempreg : TRegister;
    begin
      tempreg := getintregister(list, OS_ADDR);
      a_load_ref_reg(list,OS_ADDR,OS_ADDR,ref,tempreg);
      a_call_reg(list,tempreg);
    end;


  procedure tcgppcgen.a_load_subsetref_regs_noindex(list: TAsmList; subsetsize: tcgsize; loadbitsize: byte; const sref: tsubsetreference; valuereg, extra_value_reg: tregister);
    var
      fromsreg, tosreg: tsubsetregister;
      restbits: byte;
    begin
      restbits := (sref.bitlen - (loadbitsize - sref.startbit));
      a_op_const_reg(list,OP_SHL,OS_INT,restbits,valuereg);
      { mask other bits }
      a_op_const_reg(list,OP_AND,OS_INT,(1 shl sref.bitlen)-1,valuereg);
      { use subsetreg routine, it may have been overridden with an optimized version }
      fromsreg.subsetreg := extra_value_reg;
      fromsreg.subsetregsize := OS_INT;
      { subsetregs always count bits from right to left }
      if (target_info.endian = endian_big) then
        fromsreg.startbit := loadbitsize-restbits
      else
        fromsreg.startbit := 0;
      fromsreg.bitlen := restbits;
  
      tosreg.subsetreg := valuereg;
      tosreg.subsetregsize := OS_INT;
      if (target_info.endian = endian_big) then
        tosreg.startbit := 0
      else
        tosreg.startbit := loadbitsize-sref.startbit;
      tosreg.bitlen := restbits;
  
      a_load_subsetreg_subsetreg(list,subsetsize,subsetsize,fromsreg,tosreg);
    end;

end.
