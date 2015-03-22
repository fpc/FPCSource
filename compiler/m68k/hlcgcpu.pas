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
  globtype,
  aasmbase, aasmdata,
  cgbase, cgutils,
  symconst,symtype,symdef,
  parabase, hlcgobj, hlcg2ll;

  type
    thlcgm68k = class(thlcg2ll)
      procedure a_bit_set_reg_reg(list: TAsmList; doset: boolean; bitnumbersize, destsize: tdef; bitnumber, dest: tregister); override;
      procedure a_bit_set_const_reg(list: TAsmList; doset: boolean; destsize: tdef; bitnumber: tcgint; destreg: tregister); override;
    end;


  procedure create_hlcodegen;

implementation

  uses
    verbose, systems,
    aasmtai,
    aasmcpu,
    cutils,
    globals,
    defutil,
    cgobj,
    cpubase,
    cpuinfo,
    cgcpu;

  procedure thlcgm68k.a_bit_set_reg_reg(list: TAsmList; doset: boolean; bitnumbersize, destsize: tdef; bitnumber, dest: tregister);
    const
      instr: array[boolean] of tasmop = (A_BCLR,A_BSET);
    var
      tmpvalue: tregister;
    begin
      tmpvalue:=getintregister(list,destsize);
      //list.concat(tai_comment.create(strpnew('a_bit_set_reg_reg: called!')));
      a_load_const_reg(list,u32inttype,destsize.size*8-1,tmpvalue);
      a_op_reg_reg(list,OP_SUB,bitnumbersize,bitnumber,tmpvalue);
      list.concat(taicpu.op_reg_reg(instr[doset],S_NO,tmpvalue,dest));
    end;

  procedure thlcgm68k.a_bit_set_const_reg(list: TAsmList; doset: boolean; destsize: tdef; bitnumber: tcgint; destreg: tregister);
    const
      instr: array[boolean] of tasmop = (A_BCLR,A_BSET);
    begin
      //list.concat(tai_comment.create(strpnew('a_bit_set_const_reg: called!')));
      list.concat(taicpu.op_const_reg(instr[doset],S_NO,(destsize.size*8)-bitnumber-1,destreg));
    end;


  procedure create_hlcodegen;
    begin
      hlcg:=thlcgm68k.create;
      create_codegen;
    end;

end.
