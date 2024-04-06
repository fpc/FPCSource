{
    Copyright (c) 1999-2008 by Mazen Neifer and Florian Klaempfl

    Contains the assembler object for the MOS Technology 6502

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
unit aasmcpu;

{$i fpcdefs.inc}

interface

uses
  cclasses,
  globtype,globals,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  cgbase,cgutils,cpubase,cpuinfo,
  ogbase;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

      instabentries = {$i mos6502nop.inc}

    type
      { Addressing modes }
      taddressingmode=(
        AM_IMPLICIT,           {         }
        AM_ACCUMULATOR,        { A       }
        AM_IMMEDIATE,          { #$12    }
        AM_ZERO_PAGE,          { $12     }
        AM_ZERO_PAGE_X,        { $12,X   }
        AM_ZERO_PAGE_Y,        { $12,Y   }
        AM_RELATIVE,           { label   }
        AM_ABSOLUTE,           { $1234   }
        AM_ABSOLUTE_X,         { $1234,X }
        AM_ABSOLUTE_Y,         { $1234,Y }
        AM_INDIRECT,           { ($1234) }
        AM_INDEXED_INDIRECT,   { ($12,X) }
        AM_INDIRECT_INDEXED);  { ($12),Y }

      tinsentry = record
        opcode  : tasmop;
        adrmode : taddressingmode;
        code    : byte;
        flags   : longint;
      end;

      pinsentry=^tinsentry;

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
      private
         { next fields are filled in pass1, so pass2 is faster }
         insentry  : PInsEntry;
         inssize   : shortint;
         insoffset : longint;
         LastInsOffset : longint;

         procedure init; { this need to be called by all constructors }
      public
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : LongInt);
         constructor op_ref(op : tasmop;const _op1 : treference);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: LongInt);
         constructor op_const_reg(op:tasmop; _op1: LongInt; _op2: tregister);
         constructor op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
         constructor op_ref_const(op:tasmop; _op1: treference; _op2: LongInt);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         procedure loadbool(opidx:longint;_b:boolean);
         { register allocation }
         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    //function is_ref_addr16(const ref:treference): Boolean;
    //function is_ref_bc(const ref:treference): Boolean;
    //function is_ref_de(const ref:treference): Boolean;
    //function is_ref_hl(const ref:treference): Boolean;
    //function is_ref_sp(const ref:treference): Boolean;
    //function is_ref_ix(const ref:treference): Boolean;
    //function is_ref_iy(const ref:treference): Boolean;
    //function is_ref_ix_d(const ref:treference): Boolean;
    //function is_ref_iy_d(const ref:treference): Boolean;
    //function is_ref_opertype(const ref:treference;opertype:toperandtype): Boolean;
    //function is_ref_in_opertypes(const ref:treference;const refopertypes:trefoperandtypes): Boolean;

implementation

{****************************************************************************
                                Instruction table
*****************************************************************************}

    type
      TInsTabCache=array[TasmOp] of longint;
      PInsTabCache=^TInsTabCache;

    const
      InsTab:array[0..instabentries-1] of TInsEntry={$i mos6502tab.inc}

    var
      InsTabCache : PInsTabCache;

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    procedure taicpu.loadbool(opidx:longint;_b:boolean);
      begin
        if opidx>=ops then
         ops:=opidx+1;
        with oper[opidx]^ do
         begin
           if typ=top_ref then
            dispose(ref);
           b:=_b;
           typ:=top_bool;
         end;
      end;


    procedure taicpu.init;
      begin
        insentry:=nil;
        LastInsOffset:=-1;
        InsOffset:=0;
        InsSize:=0;
      end;


    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
         init;
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited create(op);
         init;
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
         inherited create(op);
         init;
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : LongInt);
      begin
         inherited create(op);
         init;
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         init;
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: LongInt);
      begin
         inherited create(op);
         init;
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: LongInt; _op2: tregister);
      begin
         inherited create(op);
         init;
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         init;
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         init;
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_ref_const(op: tasmop; _op1: treference; _op2: LongInt);
      begin
        inherited create(op);
        init;
        ops:=2;
        loadref(0,_op1);
        loadconst(1,_op2);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init;
         is_jmp:=op in jmp_instructions;
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         init;
         is_jmp:=op in jmp_instructions;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         init;
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=false;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        internalerror(2024040602);
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        internalerror(2024040603);
        //case getregtype(r) of
        //  R_INTREGISTER :
        //    result:=taicpu.op_reg_ref(A_LD,r,ref)
        //  else
        //    internalerror(200401041);
        //end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        internalerror(2024040604);
        //case getregtype(r) of
        //  R_INTREGISTER :
        //    result:=taicpu.op_ref_reg(A_LD,ref,r);
        //  else
        //    internalerror(2004010403);
        //end;
      end;


    //function is_ref_addr16(const ref: treference): Boolean;
    //  begin
    //    result:=(ref.base=NR_NO) and (ref.index=NR_NO);
    //  end;
    //
    //
    //function is_ref_bc(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_BC) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_BC))) and
    //            (ref.offset=0) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_de(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_DE) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_DE))) and
    //            (ref.offset=0) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_hl(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_HL) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_HL))) and
    //            (ref.offset=0) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_sp(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_SP) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_SP))) and
    //            (ref.offset=0) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_ix(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_IX) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_IX))) and
    //            (ref.offset=0) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_iy(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_IY) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_IY))) and
    //            (ref.offset=0) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_ix_d(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_IX) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_IX))) and
    //            (ref.offset>=-128) and (ref.offset<=127) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_iy_d(const ref: treference): Boolean;
    //  begin
    //    result:=(((ref.base=NR_IY) and (ref.index=NR_NO)) or
    //             ((ref.base=NR_NO) and (ref.index=NR_IY))) and
    //            (ref.offset>=-128) and (ref.offset<=127) and (ref.scalefactor<=1) and
    //            (ref.symbol=nil) and (ref.relsymbol=nil);
    //  end;
    //
    //
    //function is_ref_opertype(const ref: treference; opertype: toperandtype): Boolean;
    //  begin
    //    case opertype of
    //      OT_REF_ADDR16:
    //        result:=is_ref_addr16(ref);
    //      OT_REF_BC:
    //        result:=is_ref_bc(ref);
    //      OT_REF_DE:
    //        result:=is_ref_de(ref);
    //      OT_REF_HL:
    //        result:=is_ref_hl(ref);
    //      OT_REF_SP:
    //        result:=is_ref_sp(ref);
    //      OT_REF_IX:
    //        result:=is_ref_ix(ref);
    //      OT_REF_IY:
    //        result:=is_ref_iy(ref);
    //      OT_REF_IX_d:
    //        result:=is_ref_ix_d(ref);
    //      OT_REF_IY_d:
    //        result:=is_ref_iy_d(ref);
    //      else
    //        internalerror(2020041801);
    //    end;
    //  end;
    //
    //
    //function is_ref_in_opertypes(const ref: treference; const refopertypes: trefoperandtypes): Boolean;
    //  var
    //    ot: trefoperandtype;
    //  begin
    //    result:=true;
    //    for ot:=low(trefoperandtypes) to high(trefoperandtypes) do
    //      if (ot in refopertypes) and is_ref_opertype(ref,ot) then
    //        exit;
    //    result:=false;
    //  end;


{****************************************************************************
                                Instruction table
*****************************************************************************}

    procedure BuildInsTabCache;
      var
        i : longint;
      begin
        new(instabcache);
        FillChar(instabcache^,sizeof(tinstabcache),$ff);
        i:=0;
        while (i<InsTabEntries) do
         begin
           if InsTabCache^[InsTab[i].OPcode]=-1 then
            InsTabCache^[InsTab[i].OPcode]:=i;
           inc(i);
         end;
      end;


    procedure InitAsm;
      begin
        if not assigned(instabcache) then
          BuildInsTabCache;
      end;


    procedure DoneAsm;
      begin
        if assigned(instabcache) then
          begin
            dispose(instabcache);
            instabcache:=nil;
          end;
      end;

begin
  cai_cpu:=taicpu;
  cai_align:=tai_align;
end.
