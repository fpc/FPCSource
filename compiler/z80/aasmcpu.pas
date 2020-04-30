{
    Copyright (c) 1999-2008 by Mazen Neifer and Florian Klaempfl

    Contains the assembler object for the Z80

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

      instabentries = {$i z80nop.inc}
      maxinfolen = 18;

    type
      { Operand types }
      toperandtype=(
        OT_NONE,
        OT_IMM3,               { 3-bit immediate value (bit number: [0..7])                       }
        OT_IMM8,               { 8-bit immediate value                                            }
        OT_IMM16,              { 16-bit immediate value                                           }
        OT_IMM_VAL0,           { the immediate value 0                                            }
        OT_IMM_VAL1,           { the immediate value 1                                            }
        OT_IMM_VAL2,           { the immediate value 2                                            }
        OT_IMM_RST,            { immediate value in [$00,$08,$10,$18,$20,$28,$30,$38]             }
        OT_IMM_PORT,           { 8-bit immediate port number for the IN and OUT instructions      }
        OT_REG8,               { 8-bit register: A/B/C/D/E/H/L                                    }
        OT_REG8_A,             { register A                                                       }
        OT_REG8_I,             { register I                                                       }
        OT_REG8_R,             { register R                                                       }
        OT_REG8_C_PORT,        { implied parameter of the IN and OUT instructions                 }
        OT_REG16_IX,           { register IX                                                      }
        OT_REG16_IY,           { register IY                                                      }
        OT_REG16_SP,           { register SP                                                      }
        OT_REG16_BC_DE_HL_SP,  { 16-bit register pair: BC/DE/HL/SP                                }
        OT_REG16_BC_DE_HL_AF,  { 16-bit register pair: BC/DE/HL/AF                                }
        OT_REG16_BC_DE_IX_SP,  { 16-bit register pair: BC/DE/IX/SP                                }
        OT_REG16_BC_DE_IY_SP,  { 16-bit register pair: BC/DE/IY/SP                                }
        OT_REG16_DE,           { 16-bit register pair DE                                          }
        OT_REG16_HL,           { 16-bit register pair HL                                          }
        OT_REG16_AF,           { 16-bit register pair AF                                          }
        OT_REG16_AF_,          { alternate register set, 16-bit register pair AF'                 }
        OT_RELJMP8,            { 8-bit relative jump offset                                       }
        OT_COND,               { condition: NZ/Z/NC/C/PO/PE/P/M                                   }
        OT_COND_C,             { condition C                                                      }
        OT_COND_NC,            { condition NC                                                     }
        OT_COND_Z,             { condition Z                                                      }
        OT_COND_NZ,            { condition NZ                                                     }
        OT_REF_ADDR16,         { memory contents at address (nn = 16-bit immediate address)       }
        OT_REF_BC,             { memory contents at address in register BC                        }
        OT_REF_DE,             { memory contents at address in register DE                        }
        OT_REF_HL,             { memory contents at address in register HL                        }
        OT_REF_SP,             { memory contents at address in register SP                        }
        OT_REF_IX,             { memory contents at address in register IX                        }
        OT_REF_IY,             { memory contents at address in register IY                        }
        OT_REF_IX_d,           { memory contents at address in register IX+d, d is in [-128..127] }
        OT_REF_IY_d);          { memory contents at address in register IY+d, d is in [-128..127] }
      timmoperandtype = OT_IMM3..OT_IMM_PORT;
      tregoperandtype = OT_REG8..OT_REG16_AF_;
      treg8operandtype = OT_REG8..OT_REG8_C_PORT;
      treg16operandtype = OT_REG16_IX..OT_REG16_AF_;
      tcondoperandtype = OT_COND..OT_COND_NZ;
      trefoperandtype = OT_REF_ADDR16..OT_REF_IY_d;
      trefoperandtypes = set of trefoperandtype;

      tinsentry = record
        opcode  : tasmop;
        ops     : byte;
        optypes : array[0..max_operands-1] of toperandtype;
        code    : array[0..maxinfolen] of char;
        flags   : longint;
      end;

      pinsentry=^tinsentry;

      { taicpu }

      taicpu = class(tai_cpu_abstract_sym)
      private
         { next fields are filled in pass1, so pass2 is faster }
         insentry  : PInsEntry;
         inssize   : shortint;

         function Matches(p:PInsEntry):boolean;
         function FindInsentry(objdata:TObjData):boolean;
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

         function CheckIfValid:boolean;
         function GetString:string;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    procedure InitAsm;
    procedure DoneAsm;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    function is_ref_addr16(const ref:treference): Boolean;
    function is_ref_bc(const ref:treference): Boolean;
    function is_ref_de(const ref:treference): Boolean;
    function is_ref_hl(const ref:treference): Boolean;
    function is_ref_sp(const ref:treference): Boolean;
    function is_ref_ix(const ref:treference): Boolean;
    function is_ref_iy(const ref:treference): Boolean;
    function is_ref_ix_d(const ref:treference): Boolean;
    function is_ref_iy_d(const ref:treference): Boolean;
    function is_ref_opertype(const ref:treference;opertype:toperandtype): Boolean;
    function is_ref_in_opertypes(const ref:treference;const refopertypes:trefoperandtypes): Boolean;

implementation

{****************************************************************************
                                Instruction table
*****************************************************************************}

    type
      TInsTabCache=array[TasmOp] of longint;
      PInsTabCache=^TInsTabCache;

    const
      InsTab:array[0..instabentries-1] of TInsEntry={$i z80tab.inc}

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


    function taicpu.Matches(p: PInsEntry): boolean;

      function OperandsMatch(const oper: toper; const ot: toperandtype): boolean;
        begin
          case ot of
            OT_IMM3:
              result:=(oper.typ=top_const) and (oper.val>=0) and (oper.val<=7);
            OT_IMM8:
              result:=((oper.typ=top_const) and (oper.val>=0) and (oper.val<=255)) or
                      ((oper.typ=top_ref) and
                       (oper.ref^.refaddr in [addr_hi8,addr_lo8]) and assigned(oper.ref^.symbol) and
                       (oper.ref^.base=NR_NO) and (oper.ref^.index=NR_NO));
            OT_IMM16:
              result:=((oper.typ=top_const) and (oper.val>=0) and (oper.val<=65535)) or
                      ((oper.typ=top_ref) and
                       (oper.ref^.refaddr=addr_full) and assigned(oper.ref^.symbol) and
                       (oper.ref^.base=NR_NO) and (oper.ref^.index=NR_NO));
            OT_IMM_VAL0:
              result:=(oper.typ=top_const) and (oper.val=0);
            OT_IMM_VAL1:
              result:=(oper.typ=top_const) and (oper.val=1);
            OT_IMM_VAL2:
              result:=(oper.typ=top_const) and (oper.val=2);
            OT_IMM_RST:
              result:=(oper.typ=top_const) and ((oper.val=$00) or (oper.val=$08) or
                                                (oper.val=$10) or (oper.val=$18) or
                                                (oper.val=$20) or (oper.val=$28) or
                                                (oper.val=$30) or (oper.val=$38));
            OT_IMM_PORT:
              result:=(oper.typ=top_ref) and
                      (oper.ref^.symbol=nil) and (oper.ref^.relsymbol=nil) and
                      (oper.ref^.base=NR_NO) and (oper.ref^.index=NR_NO) and
                      (oper.ref^.offset>=0) and (oper.ref^.offset<=255);
            OT_REG8:
              result:=(oper.typ=top_reg) and ((oper.reg=NR_A) or (oper.reg=NR_B) or
                                              (oper.reg=NR_C) or (oper.reg=NR_D) or
                                              (oper.reg=NR_E) or (oper.reg=NR_H) or
                                              (oper.reg=NR_L));
            OT_REG8_A:
              result:=(oper.typ=top_reg) and (oper.reg=NR_A);
            OT_REG8_I:
              result:=(oper.typ=top_reg) and (oper.reg=NR_I);
            OT_REG8_R:
              result:=(oper.typ=top_reg) and (oper.reg=NR_R);
            OT_REG8_C_PORT:
              result:=(oper.typ=top_ref) and
                     (((oper.ref^.base=NR_C) and (oper.ref^.index=NR_NO)) or
                      ((oper.ref^.base=NR_NO) and (oper.ref^.index=NR_C))) and
                      (oper.ref^.symbol=nil) and (oper.ref^.relsymbol=nil) and
                      (oper.ref^.offset=0);
            OT_REG16_IX:
              result:=(oper.typ=top_reg) and (oper.reg=NR_IX);
            OT_REG16_IY:
              result:=(oper.typ=top_reg) and (oper.reg=NR_IY);
            OT_REG16_SP:
              result:=(oper.typ=top_reg) and (oper.reg=NR_SP);
            OT_REG16_BC_DE_HL_SP:
              result:=(oper.typ=top_reg) and ((oper.reg=NR_BC) or (oper.reg=NR_DE) or (oper.reg=NR_HL) or (oper.reg=NR_SP));
            OT_REG16_BC_DE_HL_AF:
              result:=(oper.typ=top_reg) and ((oper.reg=NR_BC) or (oper.reg=NR_DE) or (oper.reg=NR_HL) or (oper.reg=NR_AF));
            OT_REG16_BC_DE_IX_SP:
              result:=(oper.typ=top_reg) and ((oper.reg=NR_BC) or (oper.reg=NR_DE) or (oper.reg=NR_IX) or (oper.reg=NR_SP));
            OT_REG16_BC_DE_IY_SP:
              result:=(oper.typ=top_reg) and ((oper.reg=NR_BC) or (oper.reg=NR_DE) or (oper.reg=NR_IY) or (oper.reg=NR_SP));
            OT_REG16_DE:
              result:=(oper.typ=top_reg) and (oper.reg=NR_DE);
            OT_REG16_HL:
              result:=(oper.typ=top_reg) and (oper.reg=NR_HL);
            OT_REG16_AF:
              result:=(oper.typ=top_reg) and (oper.reg=NR_AF);
            OT_REG16_AF_:
              result:=(oper.typ=top_reg) and (oper.reg=NR_AF_);
            OT_RELJMP8:
              result:=(oper.typ=top_ref) and
                      (oper.ref^.refaddr=addr_full) and assigned(oper.ref^.symbol) and
                      (oper.ref^.base=NR_NO) and (oper.ref^.index=NR_NO);
            OT_REF_ADDR16,
            OT_REF_BC,
            OT_REF_DE,
            OT_REF_HL,
            OT_REF_SP,
            OT_REF_IX,
            OT_REF_IY,
            OT_REF_IX_d,
            OT_REF_IY_d:
              result:=(oper.typ=top_ref) and is_ref_opertype(oper.ref^,ot);
            else
              internalerror(2020042901);
          end;
        end;

      var
        i: Integer;
      begin
        result:=false;

        { Check the opcode }
        if p^.opcode<>opcode then
          exit;

        { The opcode doesn't support conditions, but we have a condition?
          That's an invalid instruction, don't match it against anything. }
        if (condition<>C_NONE) and not (opcode in cond_instructions) then
          exit;

        { if our opcode supports a condition, but our operation doesn't have
          one, and we're matching it with an instruction entry 'p' that has a
          condition, then it doesn't match }
        if (opcode in cond_instructions) and (condition=C_None) and
           (p^.ops>0) and (p^.optypes[0] in [OT_COND..OT_COND_NZ]) then
          exit;

        { instruction has a condition? }
        if (opcode in cond_instructions) and (condition<>C_None) then
          begin
            { Check the operand count }
            if p^.ops<>(ops+1) then
              exit;

            { Check the condition }
            case p^.optypes[0] of
              OT_COND:
                { any condition accepted };
              OT_COND_C:
                if condition<>C_C then
                  exit;
              OT_COND_NC:
                if condition<>C_NC then
                  exit;
              OT_COND_Z:
                if condition<>C_Z then
                  exit;
              OT_COND_NZ:
                if condition<>C_NZ then
                  exit;
              else
                { no condition in 'p'? Then it's not a match! }
                exit;
            end;
            { Check the operands }
            for i:=1 to p^.ops-1 do
              if not OperandsMatch(oper[i-1]^,p^.optypes[i]) then
                exit;
          end
        else
          { no condition }
          begin
            { Check the operand count }
            if p^.ops<>ops then
              exit;

            { Check the operands }
            for i:=0 to p^.ops-1 do
              if not OperandsMatch(oper[i]^,p^.optypes[i]) then
                exit;
          end;
        result:=true;
      end;


    function taicpu.FindInsentry(objdata: TObjData): boolean;
      var
        i : longint;
      begin
        result:=false;
        { Things which may only be done once, not when a second pass is done to
          optimize }

        if (Insentry=nil) {or (IF_PASS2 in InsEntry^.flags)} then
         begin
           { set the file postion }
           current_filepos:=fileinfo;
         end
        else
         begin
           { we've already an insentry so it's valid }
           result:=true;
           exit;
         end;
        { Lookup opcode in the table }
        InsSize:=-1;
        i:=instabcache^[opcode];
        if i=-1 then
         begin
           Message1(asmw_e_opcode_not_in_table,std_op2str[opcode]);
           exit;
         end;
        insentry:=@instab[i];
        while (insentry^.opcode=opcode) do
         begin
           if matches(insentry) then
             begin
               result:=true;
               exit;
             end;
           inc(insentry);
         end;
        Message1(asmw_e_invalid_opcode_and_operands,GetString);
        { No instruction found, set insentry to nil and inssize to -1 }
        insentry:=nil;
        inssize:=-1;
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

    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: LongInt);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,_op2);
      end;

     constructor taicpu.op_const_reg(op:tasmop; _op1: LongInt; _op2: tregister);
      begin
         inherited create(op);
         ops:=2;
         loadconst(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_ref_reg(op : tasmop;const _op1 : treference;_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadref(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_ref_const(op: tasmop; _op1: treference; _op2: LongInt);
      begin
        inherited create(op);
        ops:=2;
        loadref(0,_op1);
        loadconst(1,_op2);
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
        result:=(
                 ((opcode in [A_LD]) and (regtype = R_INTREGISTER))
                ) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        result:=operand_read;
        case opcode of
          A_LD,
          A_POP:
            if opnr=0 then
              result:=operand_write;
          A_PUSH,
          A_BIT,
          A_CP,
          A_DJNZ,
          A_JR,
          A_JP,
          A_CALL,
          A_RET,
          A_RETI,
          A_RETN,
          A_RST,
          A_IM:
            ;
          A_SET,
          A_RES:
            if opnr=1 then
              result:=operand_readwrite;
          A_EX:
            result:=operand_readwrite;
          else
            begin
              if opnr=0 then
                result:=operand_readwrite;
            end;
        end;
      end;


    function taicpu.CheckIfValid: boolean;
      begin
        result:=FindInsEntry(nil);
      end;


    function taicpu.GetString: string;
      var
        i : longint;
        s : string;
        first: Boolean;
      begin
        s:='['+std_op2str[opcode];
        for i:=0 to ops-1 do
          begin
            with oper[i]^ do
              begin
                if i=0 then
                  begin
                    s:=s+' ';
                    if condition<>C_None then
                      s:=s+cond2str[condition]+',';
                  end
                else
                  s:=s+',';
                case typ of
                  top_reg:
                    s:=s+std_regname(reg);
                  top_const:
                    s:=s+'const';
                  top_ref:
                    case ref^.refaddr of
                      addr_full:
                        s:=s+'addr16';
                      addr_lo8:
                        s:=s+'addr_lo8';
                      addr_hi8:
                        s:=s+'addr_hi8';
                      addr_no:
                        begin
                          s:=s+'(';
                          first:=true;
                          if ref^.base<>NR_NO then
                            begin
                              first:=false;
                              s:=s+std_regname(ref^.base);
                            end;
                          if ref^.index<>NR_NO then
                            begin
                              if not first then
                                s:=s+'+';
                              first:=false;
                              s:=s+std_regname(ref^.index);
                            end;
                          if assigned(ref^.symbol) then
                            begin
                              if not first then
                                s:=s+'+';
                              first:=false;
                              s:=s+'addr16';
                            end;
                          if ref^.offset<>0 then
                            begin
                              if not first then
                                s:=s+'+';
                              if (ref^.offset>=-128) and (ref^.offset<=127) then
                                s:=s+'const8'
                              else
                                s:=s+'const16';
                            end;
                          s:=s+')';
                        end;
                      else
                        ;
                    end;
                  else
                    ;
                end;
              end;
          end;
        GetString:=s+']';
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_LD,r,ref)
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_ref_reg(A_LD,ref,r);
          else
            internalerror(200401041);
        end;
      end;


    function is_ref_addr16(const ref: treference): Boolean;
      begin
        result:=(ref.base=NR_NO) and (ref.index=NR_NO);
      end;


    function is_ref_bc(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_BC) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_BC))) and
                (ref.offset=0) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_de(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_DE) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_DE))) and
                (ref.offset=0) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_hl(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_HL) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_HL))) and
                (ref.offset=0) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_sp(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_SP) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_SP))) and
                (ref.offset=0) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_ix(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_IX) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_IX))) and
                (ref.offset=0) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_iy(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_IY) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_IY))) and
                (ref.offset=0) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_ix_d(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_IX) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_IX))) and
                (ref.offset>=-128) and (ref.offset<=127) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_iy_d(const ref: treference): Boolean;
      begin
        result:=(((ref.base=NR_IY) and (ref.index=NR_NO)) or
                 ((ref.base=NR_NO) and (ref.index=NR_IY))) and
                (ref.offset>=-128) and (ref.offset<=127) and (ref.scalefactor<=1) and
                (ref.symbol=nil) and (ref.relsymbol=nil);
      end;


    function is_ref_opertype(const ref: treference; opertype: toperandtype): Boolean;
      begin
        case opertype of
          OT_REF_ADDR16:
            result:=is_ref_addr16(ref);
          OT_REF_BC:
            result:=is_ref_bc(ref);
          OT_REF_DE:
            result:=is_ref_de(ref);
          OT_REF_HL:
            result:=is_ref_hl(ref);
          OT_REF_SP:
            result:=is_ref_sp(ref);
          OT_REF_IX:
            result:=is_ref_ix(ref);
          OT_REF_IY:
            result:=is_ref_iy(ref);
          OT_REF_IX_d:
            result:=is_ref_ix_d(ref);
          OT_REF_IY_d:
            result:=is_ref_iy_d(ref);
          else
            internalerror(2020041801);
        end;
      end;


    function is_ref_in_opertypes(const ref: treference; const refopertypes: trefoperandtypes): Boolean;
      var
        ot: trefoperandtype;
      begin
        result:=true;
        for ot:=low(trefoperandtypes) to high(trefoperandtypes) do
          if (ot in refopertypes) and is_ref_opertype(ref,ot) then
            exit;
        result:=false;
      end;


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
