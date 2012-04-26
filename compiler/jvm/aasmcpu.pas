{
    Copyright (c) 1999-2002 by Mazen Neifer

    Contains the assembler object for the JVM

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
  widestr;

    { fake, there are no "mov reg,reg" instructions here }
    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 0;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

    type

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : aint);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);

         constructor op_sym_const(op : tasmop;_op1 : tasmsymbol;_op2 : aint);

         constructor op_single(op : tasmop;_op1 : single);
         constructor op_double(op : tasmop;_op1 : double);
         constructor op_string(op : tasmop;_op1len : aint;_op1 : pchar);
         constructor op_wstring(op : tasmop;_op1 : pcompilerwidestring);

         procedure loadsingle(opidx:longint;f:single);
         procedure loaddouble(opidx:longint;d:double);
         procedure loadstr(opidx:longint;vallen: aint;pc: pchar);
         procedure loadpwstr(opidx:longint;pwstr:pcompilerwidestring);


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

implementation

{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
        inherited create(op);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
        inherited create(op);
        ops:=1;
        loadreg(0,_op1);
      end;


    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
        inherited create(op);
        ops:=1;
        loadref(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : aint);
      begin
        inherited create(op);
        ops:=1;
        loadconst(0,_op1);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
        inherited create(op);
        ops:=1;
        is_jmp:=op in [a_if_acmpeq, a_if_acmpne, a_if_icmpeq, a_if_icmpge, a_if_icmpgt,
          a_if_icmple, a_if_icmplt, a_if_icmpne,
          a_ifeq, a_ifge, a_ifgt, a_ifle, a_iflt, a_ifne, a_ifnonnull, a_ifnull];
        loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_const(op: tasmop; _op1: tasmsymbol; _op2: aint);
      begin
        inherited create(op);
        ops:=2;
        loadsymbol(0,_op1,0);
        loadconst(1,_op2);
      end;


    constructor taicpu.op_single(op: tasmop; _op1: single);
      begin
        inherited create(op);
        ops:=1;
        loadsingle(0,_op1);
      end;


    constructor taicpu.op_double(op: tasmop; _op1: double);
      begin
        inherited create(op);
        ops:=1;
        loaddouble(0,_op1);
      end;

    constructor taicpu.op_string(op: tasmop; _op1len: aint; _op1: pchar);
      begin
        inherited create(op);
        ops:=1;
        loadstr(0,_op1len,_op1);
      end;

    constructor taicpu.op_wstring(op: tasmop; _op1: pcompilerwidestring);
      begin
        inherited create(op);
        ops:=1;
        loadpwstr(0,_op1);
      end;


    procedure taicpu.loadsingle(opidx:longint;f:single);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_single then
             clearop(opidx);
           sval:=f;
           typ:=top_single;
         end;
      end;


    procedure taicpu.loaddouble(opidx: longint; d: double);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_double then
             clearop(opidx);
           dval:=d;
           typ:=top_double;
         end;
      end;


    procedure taicpu.loadstr(opidx: longint; vallen: aint; pc: pchar);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           clearop(opidx);
           pcvallen:=vallen;
           getmem(pcval,vallen);
           move(pc^,pcval^,vallen);
           typ:=top_string;
         end;
      end;


    procedure taicpu.loadpwstr(opidx:longint;pwstr:pcompilerwidestring);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           clearop(opidx);
           initwidestring(pwstrval);
           copywidestring(pwstr,pwstrval);
           typ:=top_wstring;
         end;
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=false;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        case opcode of
          a_iinc:
            result:=operand_readwrite;
          a_aastore,
          a_astore,
          a_astore_0,
          a_astore_1,
          a_astore_2,
          a_astore_3,
          a_bastore,
          a_castore,
          a_dastore,
          a_dstore,
          a_dstore_0,
          a_dstore_1,
          a_dstore_2,
          a_dstore_3,
          a_fastore,
          a_fstore,
          a_fstore_0,
          a_fstore_1,
          a_fstore_2,
          a_fstore_3,
          a_iastore,
          a_istore,
          a_istore_0,
          a_istore_1,
          a_istore_2,
          a_istore_3,
          a_lastore,
          a_lstore,
          a_lstore_0,
          a_lstore_1,
          a_lstore_2,
          a_lstore_3,
          a_sastore:
            result:=operand_write;
          else
            result:=operand_read;
        end;
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
       internalerror(2010122614);
       result:=nil;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
       internalerror(2010122615);
       result:=nil;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;

begin
  cai_cpu:=taicpu;
  cai_align:=tai_align;
end.
