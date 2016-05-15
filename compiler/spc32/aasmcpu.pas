{
    Copyright (c) 1999-2008 by Mazen Neifer and Florian Klaempfl

    Contains the assembler object for the SPC32

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

      maxinfolen = 5;

    type
      tinsentry = record
        opcode  : tasmop;
        ops     : byte;
        optypes : array[0..3] of longint;
        code    : array[0..maxinfolen] of char;
        flags   : longint;
      end;

      pinsentry=^tinsentry;

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_const(op : tasmop;_op1 : LongInt);
         constructor op_ref(op : tasmop;const _op1 : treference);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1: tregister; _op2 : treference);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         procedure loadbool(opidx:longint;_b:boolean);
         { register allocation }
         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         { register spilling code }
         function spilling_get_operation_type(opnr: longint): topertype;override;

         { assembler }
      public
         { the next will reset all instructions that can change in pass 2 }
         procedure ResetPass1;override;
         procedure ResetPass2;override;
{         function  CheckIfValid:boolean;
         function GetString:string; }
         function  Pass1(objdata:TObjData):longint;override;
         procedure Pass2(objdata:TObjData);override;
         function calcsize(p:PInsEntry):shortint;
      private
         { next fields are filled in pass1, so pass2 is faster }
         inssize   : shortint;
         insoffset : longint;
         insentry  : PInsEntry;
         LastInsOffset : longint; { need to be public to be reset }
         function  FindInsentry(objdata:TObjData):boolean;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    function setcondition(i : taicpu;c : tasmcond) : taicpu;

    { replaces cond. branches by rjmp/jmp and the inverse cond. branch if needed
      and transforms special instructions to valid instruction encodings }
    procedure finalizespc32code(list : TAsmList);

implementation

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


    constructor taicpu.op_const(op : tasmop;_op1 : LongInt);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,_op1);
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op: tasmop; _op1: tregister; _op2: treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         is_jmp:=op in jmp_instructions;
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         is_jmp:=op in jmp_instructions;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        result:=(opcode = A_MOV) and
                (ops = 2) and
                (oper[0]^.reg = oper[1]^.reg);
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        result:=operand_read;
        case opcode of
          A_LOAD,
          A_MOV:
            begin
              if opnr = 0 then
                result:=operand_write
              else
                result:=operand_readwrite;
            end;
          A_STORE:
            begin
              if opnr = 1 then
                result:=operand_readwrite
              else
                result:=operand_read;
            end;

          A_ST:
            result:=operand_write;
          else
            result:=operand_read;
        end;
      end;


    function  taicpu.calcsize(p:PInsEntry):shortint;
      begin
        result:=1;
        if (ops=1) and
           (oper[0]^.typ in [top_ref,top_const]) then
          result:=3;

        if opcode in [A_PUSH,A_POP,A_INC,A_XCHG,A_CAS] then
          inc(result);
      end;


    procedure taicpu.ResetPass1;
      begin
        { we need to reset everything here, because the choosen insentry
          can be invalid for a new situation where the previously optimized
          insentry is not correct }
        InsEntry:=nil;
        InsSize:=0;
        LastInsOffset:=-1;
      end;


    procedure taicpu.ResetPass2;
      begin
        { we are here in a second pass, check if the instruction can be optimized }
{
        if assigned(InsEntry) and
           ((InsEntry^.flags and IF_PASS2)<>0) then
         begin
           InsEntry:=nil;
           InsSize:=0;
         end;
}
        LastInsOffset:=-1;
      end;


    function taicpu.FindInsentry(objdata:TObjData):boolean;
      begin
        result:=true;
      end;


    function taicpu.Pass1(objdata:TObjData):longint;
      begin
        Pass1:=0;
        { Save the old offset and set the new offset }
        InsOffset:=ObjData.CurrObjSec.Size;
        InsSize:=calcsize(InsEntry);
        { Error? }
        if (Insentry=nil) and (InsSize=-1) then
          exit;
        { set the file postion }
        current_filepos:=fileinfo;

        { Get InsEntry }
        if FindInsEntry(objdata) then
         begin
           LastInsOffset:=InsOffset;
           Pass1:=InsSize;
           exit;
         end;
        LastInsOffset:=-1;
      end;


    procedure taicpu.Pass2(objdata: TObjData);
      var
        b, code : longword;
        objsym : TObjSymbol;
      begin
        code:=0;

        case opcode of
          A_LD:   code:=$00;
          A_ST:   code:=$08;
          A_ADD:  code:=$10;
          A_SUB:  code:=$18;
          A_MUL:  code:=$20;
          A_LSL:  code:=$28;
          A_LSR:  code:=$30;
          A_ASR:  code:=$38;
          A_AND:  code:=$40;
          A_ORR:  code:=$48;
          A_XOR:  code:=$50;
          A_LDU:  code:=$58;
          A_ADC:  code:=$60;
          A_SBC:  code:=$68;
          A_NUL:  code:=$70;
          A_LDW:  code:=$78;
          A_LDH:  code:=$80;
          A_LDB:  code:=$88;
          A_STW:  code:=$90;
          A_StH:  code:=$98;
          A_STB:  code:=$A0;
          A_JMP:  code:=$A8;
          A_CALL: code:=$B0;
          A_Jxx:
            case condition of
              C_EQ:  code:=$B8;
              C_NE:  code:=$C0;
              C_LT:  code:=$C8;
              C_LE:  code:=$D0;
              C_B:   code:=$D8;
              C_BE:  code:=$E0;
            end;
          //A_:   code:=$E8;
          A_GS:   code:=$E8;
          A_SS:   code:=$F0;

          A_PUSH: code:=$00;
          A_POP:  code:=$08;
          A_INC:  code:=$10;
          A_XCHG: code:=$18;
          A_CAS:  code:=$20;
          A_IRET: code:=$28;
          else
            internalerror(2014041603);
        end;

        if opcode in [A_PUSH,A_POP,A_INC,A_XCHG,A_CAS,A_IRET] then
          begin
            b:=$F8;

            if opcode = A_CAS then
              begin
                if ops<>2 then
                  internalerror(2014042101);

                case oper[1]^.typ of
                  top_reg:
                    b:=b or getsupreg(oper[1]^.reg);
                  top_const,
                  top_ref:
                    b:=b or $7;
                end;

                if oper[1]^.typ=top_ref then
                  begin
                    objdata.writebytes(b,1);

                    objsym:=objdata.symbolref(oper[1]^.ref^.symbol);

                    if oper[1]^.ref^.refaddr = addr_hi16 then
                      b:=(oper[1]^.ref^.offset shr 16) and $FFFF
                    else if oper[1]^.ref^.refaddr = addr_lo16 then
                      b:=(oper[1]^.ref^.offset) and $FFFF
                    else
                      b:=(oper[1]^.ref^.offset+6) and $FFFF;

                    case oper[1]^.ref^.refaddr of
                      addr_lo16:
                        objdata.writeReloc(b,2,objsym,RELOC_LO16);
                      addr_hi16:
                        objdata.writeReloc(b,2,objsym,RELOC_HI16);
                      else
                        objdata.writeReloc(b,2,objsym,RELOC_RELATIVE);
                    end;
                  end
                else if oper[1]^.typ=top_const then
                  begin
                    b:=b or ((oper[1]^.val and $FFFF) shl 8);

                    objdata.writebytes(b,3);
                  end
                else
                  objdata.writebytes(b,1);
              end
            else
              objdata.writebytes(b, 1);
          end;

        if ops=1 then
          begin
            case oper[0]^.typ of
              top_reg:
                code:=code or getsupreg(oper[0]^.reg);
              top_const,
              top_ref:
                code:=code or $7;
            end;
          end;

        if (ops=1) and
           (oper[0]^.typ=top_ref) then
          begin
            objdata.writebytes(code,1);

            objsym:=objdata.symbolref(oper[0]^.ref^.symbol);

            if oper[0]^.ref^.refaddr = addr_hi16 then
              code:=(oper[0]^.ref^.offset shr 16) and $FFFF
            else if oper[0]^.ref^.refaddr = addr_lo16 then
              code:=(oper[0]^.ref^.offset) and $FFFF
            else
              code:=(oper[0]^.ref^.offset+6) and $FFFF;

            case oper[0]^.ref^.refaddr of
              addr_lo16:
                objdata.writeReloc(code,2,objsym,RELOC_LO16);
              addr_hi16:
                objdata.writeReloc(code,2,objsym,RELOC_HI16);
              else
                objdata.writeReloc(code,2,objsym,RELOC_RELATIVE);
            end;
          end
        else if (ops=1) and
                (oper[0]^.typ=top_const) then
          begin
            code:=code or ((oper[0]^.val and $FFFF) shl 8);

            objdata.writebytes(code,3);
          end
        else
          objdata.writebytes(code,1);
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER,
          R_ADDRESSREGISTER,
          R_FPUREGISTER:
            result:=taicpu.op_reg_ref(A_LOAD,r,ref);
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER,
          R_ADDRESSREGISTER,
          R_FPUREGISTER:
            result:=taicpu.op_reg_ref(A_STORE,r,ref);
          else
            internalerror(200401041);
        end;
      end;


    procedure InitAsm;
      begin
      end;


    procedure DoneAsm;
      begin
      end;


    function setcondition(i : taicpu;c : tasmcond) : taicpu;
      begin
        i.condition:=c;
        result:=i;
      end;


    procedure finalizespc32code(list : TAsmList);
      var
        CurrOffset : longint;
        curtai : tai;
        again : boolean;
        l : tasmlabel;
      begin
        again:=true;
        while again do
          begin
            again:=false;
            CurrOffset:=0;
            curtai:=tai(list.first);
            while assigned(curtai) do
              begin
                { instruction? }
                if not(curtai.typ in SkipInstr) then
                  case curtai.typ of
                    ait_instruction:
                      begin
                        if taicpu(curtai).opcode=A_MOV then
                          begin
                            list.InsertAfter(taicpu.op_reg(A_ST,taicpu(curtai).oper[0]^.reg), curtai);

                            taicpu(curtai).opcode:=A_LD;
                            taicpu(curtai).loadreg(0, taicpu(curtai).oper[1]^.reg);
                            taicpu(curtai).ops:=1;
                          end;

                        taicpu(curtai).InsOffset:=CurrOffset;
                        inc(CurrOffset,taicpu(curtai).calcsize(nil));
                      end;
                    ait_align:
                      inc(CurrOffset,tai_align(curtai).aligntype);
                    ait_marker,
                    ait_const:
                      ;
                    ait_label:
                      begin
                        //tai_label(curtai).labsym.offset:=CurrOffset;
                      end;
                    else
                      internalerror(2011082401);
                  end;
                curtai:=tai(curtai.next);
              end;

            {curtai:=tai(list.first);
            while assigned(curtai) do
              begin
                if (curtai.typ=ait_instruction) and
                  (taicpu(curtai).opcode in [A_BRxx]) and
                  ((taicpu(curtai).InsOffset-taicpu(curtai).oper[0]^.ref^.symbol.offset>64) or
                   (taicpu(curtai).InsOffset-taicpu(curtai).oper[0]^.ref^.symbol.offset<-63)
                  ) then
                  begin
                    current_asmdata.getjumplabel(l);
                    list.insertafter(tai_label.create(l),curtai);
                    list.insertafter(taicpu.op_sym(A_JMP,taicpu(curtai).oper[0]^.ref^.symbol),curtai);
                    taicpu(curtai).oper[0]^.ref^.symbol:=l;
                    taicpu(curtai).condition:=inverse_cond(taicpu(curtai).condition);
                    again:=true;
                  end;
                curtai:=tai(curtai.next);
              end;}
          end;
      end;


begin
  cai_cpu:=taicpu;
  cai_align:=tai_align;
end.
