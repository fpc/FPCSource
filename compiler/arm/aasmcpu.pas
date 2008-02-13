{
    Copyright (c) 2003 by Florian Klaempfl

    Contains the assembler object for the ARM

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
  cclasses,globtype,globals,verbose,
  aasmbase,aasmtai,aasmdata,aasmsym,
  ogbase,
  symtype,
  cpubase,cpuinfo,cgbase,cgutils;

    const
      { "mov reg,reg" source operand number }
      O_MOV_SOURCE = 1;
      { "mov reg,reg" source operand number }
      O_MOV_DEST = 0;

      { Operand types }
      OT_NONE      = $00000000;

      OT_BITS8     = $00000001;  { size, and other attributes, of the operand  }
      OT_BITS16    = $00000002;
      OT_BITS32    = $00000004;
      OT_BITS64    = $00000008;  { FPU only  }
      OT_BITS80    = $00000010;
      OT_FAR       = $00000020;  { this means 16:16 or 16:32, like in CALL/JMP }
      OT_NEAR      = $00000040;
      OT_SHORT     = $00000080;
      OT_BITSTINY  = $00000100;  { fpu constant }
      OT_BITSSHIFTER =
                     $00000200;

      OT_SIZE_MASK = $000003FF;  { all the size attributes  }
      OT_NON_SIZE  = longint(not OT_SIZE_MASK);

      OT_SIGNED    = $00000100;  { the operand need to be signed -128-127 }

      OT_TO        = $00000200;  { operand is followed by a colon  }
                                 { reverse effect in FADD, FSUB &c  }
      OT_COLON     = $00000400;

      OT_SHIFTEROP = $00000800;
      OT_REGISTER  = $00001000;
      OT_IMMEDIATE = $00002000;
      OT_REGLIST   = $00008000;
      OT_IMM8      = $00002001;
      OT_IMM24     = $00002002;
      OT_IMM32     = $00002004;
      OT_IMM64     = $00002008;
      OT_IMM80     = $00002010;
      OT_IMMTINY   = $00002100;
      OT_IMMSHIFTER= $00002200;
      OT_IMMEDIATE24 = OT_IMM24;
      OT_SHIFTIMM  = OT_SHIFTEROP or OT_IMMSHIFTER;
      OT_SHIFTIMMEDIATE = OT_SHIFTIMM;
      OT_IMMEDIATESHIFTER = OT_IMMSHIFTER;

      OT_IMMEDIATEFPU = OT_IMMTINY;

      OT_REGMEM    = $00200000;  { for r/m, ie EA, operands  }
      OT_REGNORM   = $00201000;  { 'normal' reg, qualifies as EA  }
      OT_REG8      = $00201001;
      OT_REG16     = $00201002;
      OT_REG32     = $00201004;
      OT_REG64     = $00201008;
      OT_VREG      = $00201010;  { vector register }
      OT_MEMORY    = $00204000;  { register number in 'basereg'  }
      OT_MEM8      = $00204001;
      OT_MEM16     = $00204002;
      OT_MEM32     = $00204004;
      OT_MEM64     = $00204008;
      OT_MEM80     = $00204010;
      { word/byte load/store }
      OT_AM2       = $00010000;
      { misc ld/st operations }
      OT_AM3       = $00020000;
      { multiple ld/st operations }
      OT_AM4       = $00040000;
      { co proc. ld/st operations }
      OT_AM5       = $00080000;
      OT_AMMASK    = $000f0000;

      OT_MEMORYAM2 = OT_MEMORY or OT_AM2;
      OT_MEMORYAM3 = OT_MEMORY or OT_AM3;
      OT_MEMORYAM4 = OT_MEMORY or OT_AM4;
      OT_MEMORYAM5 = OT_MEMORY or OT_AM5;

      OT_FPUREG    = $01000000;  { floating point stack registers  }
      OT_REG_SMASK = $00070000;  { special register operands: these may be treated differently  }
                                 { a mask for the following  }

      OT_MEM_OFFS  = $00604000;  { special type of EA  }
                                 { simple [address] offset  }
      OT_ONENESS   = $00800000;  { special type of immediate operand  }
                                 { so UNITY == IMMEDIATE | ONENESS  }
      OT_UNITY     = $00802000;  { for shift/rotate instructions  }

      instabentries = {$i armnop.inc}

      maxinfolen = 5;

      IF_NONE   = $00000000;

      IF_ARMMASK    = $000F0000;
      IF_ARM7       = $00070000;
      IF_FPMASK     = $00F00000;
      IF_FPA        = $00100000;

      { if the instruction can change in a second pass }
      IF_PASS2  = longint($80000000);

    type
      TInsTabCache=array[TasmOp] of longint;
      PInsTabCache=^TInsTabCache;

      tinsentry = record
        opcode  : tasmop;
        ops     : byte;
        optypes : array[0..3] of longint;
        code    : array[0..maxinfolen] of char;
        flags   : longint;
      end;

      pinsentry=^tinsentry;

    const
      InsTab : array[0..instabentries-1] of TInsEntry={$i armtab.inc}

    var
      InsTabCache : PInsTabCache;

    type
      taicpu = class(tai_cpu_abstract_sym)
         oppostfix : TOpPostfix;
         roundingmode : troundingmode;
         procedure loadshifterop(opidx:longint;const so:tshifterop);
         procedure loadregset(opidx:longint;const s:tcpuregisterset);
         constructor op_none(op : tasmop);

         constructor op_reg(op : tasmop;_op1 : tregister);
         constructor op_ref(op : tasmop;const _op1 : treference);
         constructor op_const(op : tasmop;_op1 : longint);

         constructor op_reg_reg(op : tasmop;_op1,_op2 : tregister);
         constructor op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
         constructor op_reg_const(op:tasmop; _op1: tregister; _op2: aint);

         constructor op_ref_regset(op:tasmop; _op1: treference; _op2: tcpuregisterset);

         constructor op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
         constructor op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
         constructor op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
         constructor op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
         constructor op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);
         constructor op_reg_reg_reg_shifterop(op : tasmop;_op1,_op2,_op3 : tregister;_op4 : tshifterop);
         { SFM/LFM }
         constructor op_reg_const_ref(op : tasmop;_op1 : tregister;_op2 : aint;_op3 : treference);

         { *M*LL }
         constructor op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);

         { this is for Jmp instructions }
         constructor op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);

         constructor op_sym(op : tasmop;_op1 : tasmsymbol);
         constructor op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
         constructor op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
         constructor op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);

         function is_same_reg_move(regtype: Tregistertype):boolean; override;

         function spilling_get_operation_type(opnr: longint): topertype;override;

         { assembler }
      public
         { the next will reset all instructions that can change in pass 2 }
         procedure ResetPass1;override;
         procedure ResetPass2;override;
         function  CheckIfValid:boolean;
         function GetString:string;
         function  Pass1(objdata:TObjData):longint;override;
         procedure Pass2(objdata:TObjData);override;
      protected
         procedure ppuloadoper(ppufile:tcompilerppufile;var o:toper);override;
         procedure ppuwriteoper(ppufile:tcompilerppufile;const o:toper);override;
         procedure ppubuildderefimploper(var o:toper);override;
         procedure ppuderefoper(var o:toper);override;
      private
         { next fields are filled in pass1, so pass2 is faster }
         inssize   : shortint;
         insoffset : longint;
         LastInsOffset : longint; { need to be public to be reset }
         insentry  : PInsEntry;
         function  InsEnd:longint;
         procedure create_ot(objdata:TObjData);
         function  Matches(p:PInsEntry):longint;
         function  calcsize(p:PInsEntry):shortint;
         procedure gencode(objdata:TObjData);
         function  NeedAddrPrefix(opidx:byte):boolean;
         procedure Swapoperands;
         function  FindInsentry(objdata:TObjData):boolean;
      end;

      tai_align = class(tai_align_abstract)
        { nothing to add }
      end;

    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
    function spilling_create_store(r:tregister; const ref:treference):Taicpu;

    function setoppostfix(i : taicpu;pf : toppostfix) : taicpu;
    function setroundingmode(i : taicpu;rm : troundingmode) : taicpu;
    function setcondition(i : taicpu;c : tasmcond) : taicpu;

    { inserts pc relative symbols at places where they are reachable }
    procedure insertpcrelativedata(list,listtoinsert : TAsmList);
    { inserts .pdata section and dummy function prolog needed for arm-wince exception handling }
    procedure InsertPData;

    procedure InitAsm;
    procedure DoneAsm;


implementation

  uses
    cutils,rgobj,itcpugas;


    procedure taicpu.loadshifterop(opidx:longint;const so:tshifterop);
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
          begin
            if typ<>top_shifterop then
              begin
                clearop(opidx);
                new(shifterop);
              end;
            shifterop^:=so;
            typ:=top_shifterop;
            if assigned(add_reg_instruction_hook) then
              add_reg_instruction_hook(self,shifterop^.rs);
          end;
      end;


    procedure taicpu.loadregset(opidx:longint;const s:tcpuregisterset);
      var
        i : byte;
      begin
        allocate_oper(opidx+1);
        with oper[opidx]^ do
         begin
           if typ<>top_regset then
             clearop(opidx);
           new(regset);
           regset^:=s;
           typ:=top_regset;
           for i:=RS_R0 to RS_R15 do
             begin
               if assigned(add_reg_instruction_hook) and (i in regset^) then
                 add_reg_instruction_hook(self,newreg(R_INTREGISTER,i,R_SUBWHOLE));
             end;
         end;
      end;


{*****************************************************************************
                                 taicpu Constructors
*****************************************************************************}

    constructor taicpu.op_none(op : tasmop);
      begin
         inherited create(op);
      end;


    { for pld }
    constructor taicpu.op_ref(op : tasmop;const _op1 : treference);
      begin
         inherited create(op);
         ops:=1;
         loadref(0,_op1);
      end;


    constructor taicpu.op_reg(op : tasmop;_op1 : tregister);
      begin
         inherited create(op);
         ops:=1;
         loadreg(0,_op1);
      end;


    constructor taicpu.op_const(op : tasmop;_op1 : longint);
      begin
         inherited create(op);
         ops:=1;
         loadconst(0,aint(_op1));
      end;


    constructor taicpu.op_reg_reg(op : tasmop;_op1,_op2 : tregister);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadreg(1,_op2);
      end;


    constructor taicpu.op_reg_const(op:tasmop; _op1: tregister; _op2: aint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadconst(1,aint(_op2));
      end;


    constructor taicpu.op_ref_regset(op:tasmop; _op1: treference; _op2: tcpuregisterset);
      begin
         inherited create(op);
         ops:=2;
         loadref(0,_op1);
         loadregset(1,_op2);
      end;


    constructor taicpu.op_reg_ref(op : tasmop;_op1 : tregister;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadref(1,_op2);
      end;


    constructor taicpu.op_reg_reg_reg(op : tasmop;_op1,_op2,_op3 : tregister);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
      end;


    constructor taicpu.op_reg_reg_reg_reg(op : tasmop;_op1,_op2,_op3,_op4 : tregister);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadreg(3,_op4);
      end;


     constructor taicpu.op_reg_reg_const(op : tasmop;_op1,_op2 : tregister; _op3: aint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadconst(2,aint(_op3));
      end;


    constructor taicpu.op_reg_const_ref(op : tasmop;_op1 : tregister;_op2 : aint;_op3 : treference);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadconst(1,_op2);
         loadref(2,_op3);
      end;


     constructor taicpu.op_reg_reg_sym_ofs(op : tasmop;_op1,_op2 : tregister; _op3: tasmsymbol;_op3ofs: longint);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadsymbol(0,_op3,_op3ofs);
      end;


     constructor taicpu.op_reg_reg_ref(op : tasmop;_op1,_op2 : tregister; const _op3: treference);
       begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadref(2,_op3);
      end;


     constructor taicpu.op_reg_reg_shifterop(op : tasmop;_op1,_op2 : tregister;_op3 : tshifterop);
      begin
         inherited create(op);
         ops:=3;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadshifterop(2,_op3);
      end;


     constructor taicpu.op_reg_reg_reg_shifterop(op : tasmop;_op1,_op2,_op3 : tregister;_op4 : tshifterop);
      begin
         inherited create(op);
         ops:=4;
         loadreg(0,_op1);
         loadreg(1,_op2);
         loadreg(2,_op3);
         loadshifterop(3,_op4);
      end;


    constructor taicpu.op_cond_sym(op : tasmop;cond:TAsmCond;_op1 : tasmsymbol);
      begin
         inherited create(op);
         condition:=cond;
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym(op : tasmop;_op1 : tasmsymbol);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,0);
      end;


    constructor taicpu.op_sym_ofs(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint);
      begin
         inherited create(op);
         ops:=1;
         loadsymbol(0,_op1,_op1ofs);
      end;


     constructor taicpu.op_reg_sym_ofs(op : tasmop;_op1 : tregister;_op2:tasmsymbol;_op2ofs : longint);
      begin
         inherited create(op);
         ops:=2;
         loadreg(0,_op1);
         loadsymbol(1,_op2,_op2ofs);
      end;


    constructor taicpu.op_sym_ofs_ref(op : tasmop;_op1 : tasmsymbol;_op1ofs:longint;const _op2 : treference);
      begin
         inherited create(op);
         ops:=2;
         loadsymbol(0,_op1,_op1ofs);
         loadref(1,_op2);
      end;


    function taicpu.is_same_reg_move(regtype: Tregistertype):boolean;
      begin
        { allow the register allocator to remove unnecessary moves }
        result:=(((opcode=A_MOV) and (regtype = R_INTREGISTER)) or
                 ((opcode=A_MVF) and (regtype = R_FPUREGISTER) and (oppostfix in [PF_None,PF_D]))
                ) and
                (condition=C_None) and
                (ops=2) and
                (oper[0]^.typ=top_reg) and
                (oper[1]^.typ=top_reg) and
                (oper[0]^.reg=oper[1]^.reg);
      end;


    function spilling_create_load(const ref:treference;r:tregister):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_LDR,r,ref);
          R_FPUREGISTER :
            { use lfm because we don't know the current internal format
              and avoid exceptions
            }
            result:=taicpu.op_reg_const_ref(A_LFM,r,1,ref);
          else
            internalerror(200401041);
        end;
      end;


    function spilling_create_store(r:tregister; const ref:treference):Taicpu;
      begin
        case getregtype(r) of
          R_INTREGISTER :
            result:=taicpu.op_reg_ref(A_STR,r,ref);
          R_FPUREGISTER :
            { use sfm because we don't know the current internal format
              and avoid exceptions
            }
            result:=taicpu.op_reg_const_ref(A_SFM,r,1,ref);
          else
            internalerror(200401041);
        end;
      end;


    function taicpu.spilling_get_operation_type(opnr: longint): topertype;
      begin
        case opcode of
          A_ADC,A_ADD,A_AND,
          A_EOR,A_CLZ,
          A_LDR,A_LDRB,A_LDRBT,A_LDRH,A_LDRSB,
          A_LDRSH,A_LDRT,
          A_MOV,A_MVN,A_MLA,A_MUL,
          A_ORR,A_RSB,A_RSC,A_SBC,A_SUB,
          A_SWP,A_SWPB,
          A_LDF,A_FLT,A_FIX,
          A_ADF,A_DVF,A_FDV,A_FML,
          A_RFS,A_RFC,A_RDF,
          A_RMF,A_RPW,A_RSF,A_SUF,A_ABS,A_ACS,A_ASN,A_ATN,A_COS,
          A_EXP,A_LOG,A_LGN,A_MVF,A_MNF,A_FRD,A_MUF,A_POL,A_RND,A_SIN,A_SQT,A_TAN,
          A_LFM:
            if opnr=0 then
              result:=operand_write
            else
              result:=operand_read;
          A_BIC,A_BKPT,A_B,A_BL,A_BLX,A_BX,
          A_CMN,A_CMP,A_TEQ,A_TST,
          A_CMF,A_CMFE,A_WFS,A_CNF:
            result:=operand_read;
          A_SMLAL,A_UMLAL:
            if opnr in [0,1] then
              result:=operand_readwrite
            else
              result:=operand_read;
           A_SMULL,A_UMULL:
            if opnr in [0,1] then
              result:=operand_write
            else
              result:=operand_read;
          A_STR,A_STRB,A_STRBT,
          A_STRH,A_STRT,A_STF,A_SFM:
            { important is what happens with the involved registers }
            if opnr=0 then
              result := operand_read
            else
              { check for pre/post indexed }
              result := operand_read;
          else
            internalerror(200403151);
        end;
      end;


    procedure BuildInsTabCache;
      var
        i : longint;
      begin
        new(instabcache);
        FillChar(instabcache^,sizeof(tinstabcache),$ff);
        i:=0;
        while (i<InsTabEntries) do
          begin
            if InsTabCache^[InsTab[i].Opcode]=-1 then
              InsTabCache^[InsTab[i].Opcode]:=i;
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


    function setoppostfix(i : taicpu;pf : toppostfix) : taicpu;
      begin
        i.oppostfix:=pf;
        result:=i;
      end;


    function setroundingmode(i : taicpu;rm : troundingmode) : taicpu;
      begin
        i.roundingmode:=rm;
        result:=i;
      end;


    function setcondition(i : taicpu;c : tasmcond) : taicpu;
      begin
        i.condition:=c;
        result:=i;
      end;


    procedure insertpcrelativedata(list,listtoinsert : TAsmList);
      var
        curpos,
        penalty,
        lastpos : longint;
        curop : longint;
        curtai : tai;
        curdatatai,hp,hp2 : tai;
        curdata : TAsmList;
        l : tasmlabel;
        doinsert,
        removeref : boolean;
      begin
        curdata:=TAsmList.create;
        lastpos:=-1;
        curpos:=0;
        curtai:=tai(list.first);
        doinsert:=false;
        while assigned(curtai) do
          begin
            { instruction? }
            if curtai.typ=ait_instruction then
              begin
                { walk through all operand of the instruction }
                for curop:=0 to taicpu(curtai).ops-1 do
                  begin
                    { reference? }
                    if (taicpu(curtai).oper[curop]^.typ=top_ref) then
                      begin
                        { pc relative symbol? }
                        curdatatai:=tai(taicpu(curtai).oper[curop]^.ref^.symboldata);
                        if assigned(curdatatai) and
                          { move only if we're at the first reference of a label }
                          (taicpu(curtai).oper[curop]^.ref^.offset=0) then
                          begin
                            { check if symbol already used. }
                            { if yes, reuse the symbol }
                            hp:=tai(curdatatai.next);
                            removeref:=false;
                            if assigned(hp) and (hp.typ=ait_const) then
                              begin
                                hp2:=tai(curdata.first);
                                while assigned(hp2) do
                                  begin
                                    if (hp2.typ=ait_const) and (tai_const(hp2).sym=tai_const(hp).sym)
                                      and (tai_const(hp2).value=tai_const(hp).value) and (tai(hp2.previous).typ=ait_label)
                                    then
                                      begin
                                        with taicpu(curtai).oper[curop]^.ref^ do
                                          begin
                                            symboldata:=hp2.previous;
                                            symbol:=tai_label(hp2.previous).labsym;
                                          end;
                                        removeref:=true;
                                        break;
                                      end;
                                    hp2:=tai(hp2.next);
                                  end;
                              end;
                            { move or remove symbol reference }
                            repeat
                              hp:=tai(curdatatai.next);
                              listtoinsert.remove(curdatatai);
                              if removeref then
                                curdatatai.free
                              else
                                curdata.concat(curdatatai);
                              curdatatai:=hp;
                            until (curdatatai=nil) or (curdatatai.typ=ait_label);
                            if lastpos=-1 then
                              lastpos:=curpos;
                          end;
                      end;
                  end;
                inc(curpos);
              end
            else
              if curtai.typ=ait_const then
                inc(curpos);

            { special case for case jump tables }
            if assigned(curtai.next) and
              (taicpu(curtai.next).typ=ait_instruction) and
              (taicpu(curtai.next).opcode=A_LDR) and
              (taicpu(curtai.next).oper[0]^.typ=top_reg) and
              (taicpu(curtai.next).oper[0]^.reg=NR_PC) then
              begin
                penalty:=1;
                hp:=tai(curtai.next.next);
                while assigned(hp) and (hp.typ=ait_const) do
                  begin
                    inc(penalty);
                    hp:=tai(hp.next);
                  end;
              end
            else
              penalty:=0;

            { don't miss an insert }
            doinsert:=doinsert or (curpos-lastpos+penalty>1016);

            { split only at real instructions else the test below fails }
            if doinsert and (curtai.typ=ait_instruction) and
              (
                { don't split loads of pc to lr and the following move }
                not(
                    (taicpu(curtai).opcode=A_MOV) and
                    (taicpu(curtai).oper[0]^.typ=top_reg) and
                    (taicpu(curtai).oper[0]^.reg=NR_R14) and
                    (taicpu(curtai).oper[1]^.typ=top_reg) and
                    (taicpu(curtai).oper[1]^.reg=NR_PC)
                   )
              ) then
              begin
                lastpos:=curpos;
                doinsert:=false;
                hp:=tai(curtai.next);
                current_asmdata.getjumplabel(l);
                curdata.insert(taicpu.op_sym(A_B,l));
                curdata.concat(tai_label.create(l));
                list.insertlistafter(curtai,curdata);
                curtai:=hp;
              end
            else
              curtai:=tai(curtai.next);
          end;
        list.concatlist(curdata);
        curdata.free;
      end;

    procedure InsertPData;
      var
        prolog: TAsmList;
      begin
        prolog:=TAsmList.create;
        new_section(prolog,sec_code,'FPC_EH_PROLOG',sizeof(pint),secorder_begin);
        prolog.concat(Tai_const.Createname('_ARM_ExceptionHandler', 0));
        prolog.concat(Tai_const.Create_32bit(0));
        prolog.concat(Tai_symbol.Createname_global('FPC_EH_CODE_START',AT_DATA,0));
        { dummy function }
        prolog.concat(taicpu.op_reg_reg(A_MOV,NR_R15,NR_R14));
        current_asmdata.asmlists[al_start].insertList(prolog);
        prolog.Free;
        new_section(current_asmdata.asmlists[al_end],sec_pdata,'',sizeof(pint));
        current_asmdata.asmlists[al_end].concat(Tai_const.Createname('FPC_EH_CODE_START', 0));
        current_asmdata.asmlists[al_end].concat(Tai_const.Create_32bit(longint($ffffff01)));
      end;

(*
      Floating point instruction format information, taken from the linux kernel
      ARM Floating Point Instruction Classes
      | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
      |c o n d|1 1 0 P|U|u|W|L|   Rn  |v|  Fd |0|0|0|1|  o f f s e t  | CPDT
      |c o n d|1 1 0 P|U|w|W|L|   Rn  |x|  Fd |0|0|1|0|  o f f s e t  | CPDT (copro 2)
      | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |
      |c o n d|1 1 1 0|a|b|c|d|e|  Fn |j|  Fd |0|0|0|1|f|g|h|0|i|  Fm | CPDO
      |c o n d|1 1 1 0|a|b|c|L|e|  Fn |   Rd  |0|0|0|1|f|g|h|1|i|  Fm | CPRT
      |c o n d|1 1 1 0|a|b|c|1|e|  Fn |1|1|1|1|0|0|0|1|f|g|h|1|i|  Fm | comparisons
      | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | | |

      CPDT            data transfer instructions
                      LDF, STF, LFM (copro 2), SFM (copro 2)

      CPDO            dyadic arithmetic instructions
                      ADF, MUF, SUF, RSF, DVF, RDF,
                      POW, RPW, RMF, FML, FDV, FRD, POL

      CPDO            monadic arithmetic instructions
                      MVF, MNF, ABS, RND, SQT, LOG, LGN, EXP,
                      SIN, COS, TAN, ASN, ACS, ATN, URD, NRM

      CPRT            joint arithmetic/data transfer instructions
                      FIX (arithmetic followed by load/store)
                      FLT (load/store followed by arithmetic)
                      CMF, CNF CMFE, CNFE (comparisons)
                      WFS, RFS (write/read floating point status register)
                      WFC, RFC (write/read floating point control register)

      cond            condition codes
      P               pre/post index bit: 0 = postindex, 1 = preindex
      U               up/down bit: 0 = stack grows down, 1 = stack grows up
      W               write back bit: 1 = update base register (Rn)
      L               load/store bit: 0 = store, 1 = load
      Rn              base register
      Rd              destination/source register
      Fd              floating point destination register
      Fn              floating point source register
      Fm              floating point source register or floating point constant

      uv              transfer length (TABLE 1)
      wx              register count (TABLE 2)
      abcd            arithmetic opcode (TABLES 3 & 4)
      ef              destination size (rounding precision) (TABLE 5)
      gh              rounding mode (TABLE 6)
      j               dyadic/monadic bit: 0 = dyadic, 1 = monadic
      i               constant bit: 1 = constant (TABLE 6)
      */

      /*
      TABLE 1
      +-------------------------+---+---+---------+---------+
      |  Precision              | u | v | FPSR.EP | length  |
      +-------------------------+---+---+---------+---------+
      | Single                  | 0 | 0 |    x    | 1 words |
      | Double                  | 1 | 1 |    x    | 2 words |
      | Extended                | 1 | 1 |    x    | 3 words |
      | Packed decimal          | 1 | 1 |    0    | 3 words |
      | Expanded packed decimal | 1 | 1 |    1    | 4 words |
      +-------------------------+---+---+---------+---------+
      Note: x = don't care
      */

      /*
      TABLE 2
      +---+---+---------------------------------+
      | w | x | Number of registers to transfer |
      +---+---+---------------------------------+
      | 0 | 1 |  1                              |
      | 1 | 0 |  2                              |
      | 1 | 1 |  3                              |
      | 0 | 0 |  4                              |
      +---+---+---------------------------------+
      */

      /*
      TABLE 3: Dyadic Floating Point Opcodes
      +---+---+---+---+----------+-----------------------+-----------------------+
      | a | b | c | d | Mnemonic | Description           | Operation             |
      +---+---+---+---+----------+-----------------------+-----------------------+
      | 0 | 0 | 0 | 0 | ADF      | Add                   | Fd := Fn + Fm         |
      | 0 | 0 | 0 | 1 | MUF      | Multiply              | Fd := Fn * Fm         |
      | 0 | 0 | 1 | 0 | SUF      | Subtract              | Fd := Fn - Fm         |
      | 0 | 0 | 1 | 1 | RSF      | Reverse subtract      | Fd := Fm - Fn         |
      | 0 | 1 | 0 | 0 | DVF      | Divide                | Fd := Fn / Fm         |
      | 0 | 1 | 0 | 1 | RDF      | Reverse divide        | Fd := Fm / Fn         |
      | 0 | 1 | 1 | 0 | POW      | Power                 | Fd := Fn ^ Fm         |
      | 0 | 1 | 1 | 1 | RPW      | Reverse power         | Fd := Fm ^ Fn         |
      | 1 | 0 | 0 | 0 | RMF      | Remainder             | Fd := IEEE rem(Fn/Fm) |
      | 1 | 0 | 0 | 1 | FML      | Fast Multiply         | Fd := Fn * Fm         |
      | 1 | 0 | 1 | 0 | FDV      | Fast Divide           | Fd := Fn / Fm         |
      | 1 | 0 | 1 | 1 | FRD      | Fast reverse divide   | Fd := Fm / Fn         |
      | 1 | 1 | 0 | 0 | POL      | Polar angle (ArcTan2) | Fd := arctan2(Fn,Fm)  |
      | 1 | 1 | 0 | 1 |          | undefined instruction | trap                  |
      | 1 | 1 | 1 | 0 |          | undefined instruction | trap                  |
      | 1 | 1 | 1 | 1 |          | undefined instruction | trap                  |
      +---+---+---+---+----------+-----------------------+-----------------------+
      Note: POW, RPW, POL are deprecated, and are available for backwards
            compatibility only.
      */

      /*
      TABLE 4: Monadic Floating Point Opcodes
      +---+---+---+---+----------+-----------------------+-----------------------+
      | a | b | c | d | Mnemonic | Description           | Operation             |
      +---+---+---+---+----------+-----------------------+-----------------------+
      | 0 | 0 | 0 | 0 | MVF      | Move                  | Fd := Fm              |
      | 0 | 0 | 0 | 1 | MNF      | Move negated          | Fd := - Fm            |
      | 0 | 0 | 1 | 0 | ABS      | Absolute value        | Fd := abs(Fm)         |
      | 0 | 0 | 1 | 1 | RND      | Round to integer      | Fd := int(Fm)         |
      | 0 | 1 | 0 | 0 | SQT      | Square root           | Fd := sqrt(Fm)        |
      | 0 | 1 | 0 | 1 | LOG      | Log base 10           | Fd := log10(Fm)       |
      | 0 | 1 | 1 | 0 | LGN      | Log base e            | Fd := ln(Fm)          |
      | 0 | 1 | 1 | 1 | EXP      | Exponent              | Fd := e ^ Fm          |
      | 1 | 0 | 0 | 0 | SIN      | Sine                  | Fd := sin(Fm)         |
      | 1 | 0 | 0 | 1 | COS      | Cosine                | Fd := cos(Fm)         |
      | 1 | 0 | 1 | 0 | TAN      | Tangent               | Fd := tan(Fm)         |
      | 1 | 0 | 1 | 1 | ASN      | Arc Sine              | Fd := arcsin(Fm)      |
      | 1 | 1 | 0 | 0 | ACS      | Arc Cosine            | Fd := arccos(Fm)      |
      | 1 | 1 | 0 | 1 | ATN      | Arc Tangent           | Fd := arctan(Fm)      |
      | 1 | 1 | 1 | 0 | URD      | Unnormalized round    | Fd := int(Fm)         |
      | 1 | 1 | 1 | 1 | NRM      | Normalize             | Fd := norm(Fm)        |
      +---+---+---+---+----------+-----------------------+-----------------------+
      Note: LOG, LGN, EXP, SIN, COS, TAN, ASN, ACS, ATN are deprecated, and are
            available for backwards compatibility only.
      */

      /*
      TABLE 5
      +-------------------------+---+---+
      |  Rounding Precision     | e | f |
      +-------------------------+---+---+
      | IEEE Single precision   | 0 | 0 |
      | IEEE Double precision   | 0 | 1 |
      | IEEE Extended precision | 1 | 0 |
      | undefined (trap)        | 1 | 1 |
      +-------------------------+---+---+
      */

      /*
      TABLE 5
      +---------------------------------+---+---+
      |  Rounding Mode                  | g | h |
      +---------------------------------+---+---+
      | Round to nearest (default)      | 0 | 0 |
      | Round toward plus infinity      | 0 | 1 |
      | Round toward negative infinity  | 1 | 0 |
      | Round toward zero               | 1 | 1 |
      +---------------------------------+---+---+
*)
    function taicpu.GetString:string;
      var
        i : longint;
        s : string;
        addsize : boolean;
      begin
        s:='['+gas_op2str[opcode];
        for i:=0 to ops-1 do
         begin
           with oper[i]^ do
             begin
               if i=0 then
                s:=s+' '
               else
                s:=s+',';
               { type }
               addsize:=false;
               if (ot and OT_VREG)=OT_VREG then
                s:=s+'vreg'
               else
                 if (ot and OT_FPUREG)=OT_FPUREG then
                  s:=s+'fpureg'
               else
                if (ot and OT_REGISTER)=OT_REGISTER then
                 begin
                   s:=s+'reg';
                   addsize:=true;
                 end
               else
                if (ot and OT_REGLIST)=OT_REGLIST then
                 begin
                   s:=s+'reglist';
                   addsize:=false;
                 end
               else
                if (ot and OT_IMMEDIATE)=OT_IMMEDIATE then
                 begin
                   s:=s+'imm';
                   addsize:=true;
                 end
               else
                if (ot and OT_MEMORY)=OT_MEMORY then
                 begin
                   s:=s+'mem';
                   addsize:=true;
                   if (ot and OT_AM2)<>0 then
                     s:=s+' am2 ';
                 end
               else
                 s:=s+'???';
               { size }
               if addsize then
                begin
                  if (ot and OT_BITS8)<>0 then
                    s:=s+'8'
                  else
                   if (ot and OT_BITS16)<>0 then
                    s:=s+'24'
                  else
                   if (ot and OT_BITS32)<>0 then
                    s:=s+'32'
                  else
                   if (ot and OT_BITSSHIFTER)<>0 then
                    s:=s+'shifter'
                  else
                    s:=s+'??';
                  { signed }
                  if (ot and OT_SIGNED)<>0 then
                   s:=s+'s';
                end;
             end;
         end;
        GetString:=s+']';
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
        if assigned(InsEntry) and
           ((InsEntry^.flags and IF_PASS2)<>0) then
         begin
           InsEntry:=nil;
           InsSize:=0;
         end;
        LastInsOffset:=-1;
      end;


    function taicpu.CheckIfValid:boolean;
      begin
      end;


    function taicpu.Pass1(objdata:TObjData):longint;
      var
        ldr2op : array[PF_B..PF_T] of tasmop = (
          A_LDRB,A_LDRSB,A_LDRBT,A_LDRH,A_LDRSH,A_LDRT);
        str2op : array[PF_B..PF_T] of tasmop = (
          A_STRB,A_None,A_STRBT,A_STRH,A_None,A_STRT);
      begin
        Pass1:=0;
        { Save the old offset and set the new offset }
        InsOffset:=ObjData.CurrObjSec.Size;
        { Error? }
        if (Insentry=nil) and (InsSize=-1) then
          exit;
        { set the file postion }
        current_filepos:=fileinfo;

        { tranlate LDR+postfix to complete opcode }
        if (opcode=A_LDR) and (oppostfix<>PF_None) then
          begin
            if (oppostfix in [low(ldr2op)..high(ldr2op)]) then
              opcode:=ldr2op[oppostfix]
            else
              internalerror(2005091001);
            if opcode=A_None then
              internalerror(2005091004);
            { postfix has been added to opcode }
            oppostfix:=PF_None;
          end
        else if (opcode=A_STR) and (oppostfix<>PF_None) then
          begin
            if (oppostfix in [low(str2op)..high(str2op)]) then
              opcode:=str2op[oppostfix]
            else
              internalerror(2005091002);
            if opcode=A_None then
              internalerror(2005091003);
            { postfix has been added to opcode }
            oppostfix:=PF_None;
          end;

        { Get InsEntry }
        if FindInsEntry(objdata) then
         begin
           InsSize:=4;
           LastInsOffset:=InsOffset;
           Pass1:=InsSize;
           exit;
         end;
        LastInsOffset:=-1;
      end;


    procedure taicpu.Pass2(objdata:TObjData);
      begin
        { error in pass1 ? }
        if insentry=nil then
         exit;
        current_filepos:=fileinfo;
        { Generate the instruction }
        GenCode(objdata);
      end;


    procedure taicpu.ppuloadoper(ppufile:tcompilerppufile;var o:toper);
      begin
      end;


    procedure taicpu.ppuwriteoper(ppufile:tcompilerppufile;const o:toper);
      begin
      end;


    procedure taicpu.ppubuildderefimploper(var o:toper);
      begin
      end;


    procedure taicpu.ppuderefoper(var o:toper);
      begin
      end;


    function  taicpu.InsEnd:longint;
      begin
      end;


    procedure taicpu.create_ot(objdata:TObjData);
      var
        i,l,relsize : longint;
        dummy : byte;
        currsym : TObjSymbol;
      begin
        if ops=0 then
         exit;
        { update oper[].ot field }
        for i:=0 to ops-1 do
         with oper[i]^ do
          begin
            case typ of
              top_regset:
                begin
                  ot:=OT_REGLIST;
                end;
              top_reg :
                begin
                  case getregtype(reg) of
                    R_INTREGISTER:
                      ot:=OT_REG32 or OT_SHIFTEROP;
                    R_FPUREGISTER:
                      ot:=OT_FPUREG;
                    else
                      internalerror(2005090901);
                  end;
                end;
              top_ref :
                begin
                  if ref^.refaddr=addr_no then
                    begin
                      { create ot field }
                      { we should get the size here dependend on the
                        instruction }
                      if (ot and OT_SIZE_MASK)=0 then
                        ot:=OT_MEMORY or OT_BITS32
                      else
                        ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
                      if (ref^.base=NR_NO) and (ref^.index=NR_NO) then
                        ot:=ot or OT_MEM_OFFS;
                      { if we need to fix a reference, we do it here }

                      { pc relative addressing }
                      if (ref^.base=NR_NO) and
                        (ref^.index=NR_NO) and
                        (ref^.shiftmode=SM_None)
                        { at least we should check if the destination symbol
                          is in a text section }
                        { and
                        (ref^.symbol^.owner="text") } then
                        ref^.base:=NR_PC;

                      { determine possible address modes }
                      if (ref^.base<>NR_NO) and
                        (
                          (
                            (ref^.index=NR_NO) and
                            (ref^.shiftmode=SM_None) and
                            (ref^.offset>=-4097) and
                            (ref^.offset<=4097)
                          ) or
                          (
                            (ref^.shiftmode=SM_None) and
                            (ref^.offset=0)
                          ) or
                          (
                            (ref^.index<>NR_NO) and
                            (ref^.shiftmode<>SM_None) and
                            (ref^.shiftimm<=31) and
                            (ref^.offset=0)
                          )
                        ) then
                        ot:=ot or OT_AM2;

                      if (ref^.index<>NR_NO) and
                        (oppostfix in [PF_IA,PF_IB,PF_DA,PF_DB,PF_FD,PF_FA,PF_ED,PF_EA]) and
                        (
                          (ref^.base=NR_NO) and
                          (ref^.shiftmode=SM_None) and
                          (ref^.offset=0)
                        ) then
                        ot:=ot or OT_AM4;

                    end
                  else
                    begin
                      l:=ref^.offset;
                      currsym:=ObjData.symbolref(ref^.symbol);
                      if assigned(currsym) then
                        inc(l,currsym.address);
                      relsize:=(InsOffset+2)-l;
                      if (relsize<-33554428) or (relsize>33554428) then
                       ot:=OT_IMM32
                      else
                       ot:=OT_IMM24;
                    end;
                end;
              top_local :
                begin
                  { we should get the size here dependend on the
                    instruction }
                  if (ot and OT_SIZE_MASK)=0 then
                    ot:=OT_MEMORY or OT_BITS32
                  else
                    ot:=OT_MEMORY or (ot and OT_SIZE_MASK);
                end;
              top_const :
                begin
                  ot:=OT_IMMEDIATE;
                  if is_shifter_const(val,dummy) then
                    ot:=OT_IMMSHIFTER
                  else
                    ot:=OT_IMM32
                end;
              top_none :
                begin
                  { generated when there was an error in the
                    assembler reader. It never happends when generating
                    assembler }
                end;
              top_shifterop:
                begin
                  ot:=OT_SHIFTEROP;
                end;
              else
                internalerror(200402261);
            end;
          end;
      end;


    function taicpu.Matches(p:PInsEntry):longint;
      { * IF_SM stands for Size Match: any operand whose size is not
       * explicitly specified by the template is `really' intended to be
       * the same size as the first size-specified operand.
       * Non-specification is tolerated in the input instruction, but
       * _wrong_ specification is not.
       *
       * IF_SM2 invokes Size Match on only the first _two_ operands, for
       * three-operand instructions such as SHLD: it implies that the
       * first two operands must match in size, but that the third is
       * required to be _unspecified_.
       *
       * IF_SB invokes Size Byte: operands with unspecified size in the
       * template are really bytes, and so no non-byte specification in
       * the input instruction will be tolerated. IF_SW similarly invokes
       * Size Word, and IF_SD invokes Size Doubleword.
       *
       * (The default state if neither IF_SM nor IF_SM2 is specified is
       * that any operand with unspecified size in the template is
       * required to have unspecified size in the instruction too...)
      }
      var
        i,j,asize,oprs : longint;
        siz : array[0..3] of longint;
      begin
        Matches:=100;
        writeln(getstring,'---');

        { Check the opcode and operands }
        if (p^.opcode<>opcode) or (p^.ops<>ops) then
         begin
           Matches:=0;
           exit;
         end;

        { Check that no spurious colons or TOs are present }
        for i:=0 to p^.ops-1 do
         if (oper[i]^.ot and (not p^.optypes[i]) and (OT_COLON or OT_TO))<>0 then
          begin
            Matches:=0;
            exit;
          end;

        { Check that the operand flags all match up }
        for i:=0 to p^.ops-1 do
         begin
           if ((p^.optypes[i] and (not oper[i]^.ot)) or
               ((p^.optypes[i] and OT_SIZE_MASK) and
                ((p^.optypes[i] xor oper[i]^.ot) and OT_SIZE_MASK)))<>0 then
            begin
              if ((p^.optypes[i] and (not oper[i]^.ot) and OT_NON_SIZE) or
                  (oper[i]^.ot and OT_SIZE_MASK))<>0 then
               begin
                 Matches:=0;
                 exit;
               end
              else
               Matches:=1;
            end;
         end;

      { check postfixes:
        the existance of a certain postfix requires a
        particular code }

        { update condition flags
          or floating point single }
      if (oppostfix=PF_S) and
        not(p^.code[0] in [#$04]) then
        begin
          Matches:=0;
          exit;
        end;

      { floating point size }
      if (oppostfix in [PF_D,PF_E,PF_P,PF_EP]) and
        not(p^.code[0] in []) then
        begin
          Matches:=0;
          exit;
        end;

      { multiple load/store address modes }
      if (oppostfix in [PF_IA,PF_IB,PF_DA,PF_DB,PF_FD,PF_FA,PF_ED,PF_EA]) and
        not(p^.code[0] in [
          // ldr,str,ldrb,strb
          #$17,
          // stm,ldm
          #$26
        ]) then
        begin
          Matches:=0;
          exit;
        end;

      { we shouldn't see any opsize prefixes here }
      if (oppostfix in [PF_B,PF_SB,PF_BT,PF_H,PF_SH,PF_T]) then
        begin
          Matches:=0;
          exit;
        end;

      if (roundingmode<>RM_None) and not(p^.code[0] in []) then
        begin
          Matches:=0;
          exit;
        end;

      { Check operand sizes }
        { as default an untyped size can get all the sizes, this is different
          from nasm, but else we need to do a lot checking which opcodes want
          size or not with the automatic size generation }
        asize:=longint($ffffffff);
        (*
        if (p^.flags and IF_SB)<>0 then
          asize:=OT_BITS8
        else if (p^.flags and IF_SW)<>0 then
          asize:=OT_BITS16
        else if (p^.flags and IF_SD)<>0 then
          asize:=OT_BITS32;
        if (p^.flags and IF_ARMASK)<>0 then
         begin
           siz[0]:=0;
           siz[1]:=0;
           siz[2]:=0;
           if (p^.flags and IF_AR0)<>0 then
            siz[0]:=asize
           else if (p^.flags and IF_AR1)<>0 then
            siz[1]:=asize
           else if (p^.flags and IF_AR2)<>0 then
            siz[2]:=asize;
         end
        else
         begin
         { we can leave because the size for all operands is forced to be
           the same
           but not if IF_SB IF_SW or IF_SD is set PM }
           if asize=-1 then
             exit;
           siz[0]:=asize;
           siz[1]:=asize;
           siz[2]:=asize;
         end;

        if (p^.flags and (IF_SM or IF_SM2))<>0 then
         begin
           if (p^.flags and IF_SM2)<>0 then
            oprs:=2
           else
            oprs:=p^.ops;
           for i:=0 to oprs-1 do
            if ((p^.optypes[i] and OT_SIZE_MASK) <> 0) then
             begin
               for j:=0 to oprs-1 do
                siz[j]:=p^.optypes[i] and OT_SIZE_MASK;
               break;
             end;
          end
         else
          oprs:=2;

        { Check operand sizes }
        for i:=0 to p^.ops-1 do
         begin
           if ((p^.optypes[i] and OT_SIZE_MASK)=0) and
              ((oper[i]^.ot and OT_SIZE_MASK and (not siz[i]))<>0) and
              { Immediates can always include smaller size }
              ((oper[i]^.ot and OT_IMMEDIATE)=0) and
               (((p^.optypes[i] and OT_SIZE_MASK) or siz[i])<(oper[i]^.ot and OT_SIZE_MASK)) then
            Matches:=2;
         end;
        *)
      end;


    function  taicpu.calcsize(p:PInsEntry):shortint;
      begin
        result:=4;
      end;


    function  taicpu.NeedAddrPrefix(opidx:byte):boolean;
      begin
      end;


    procedure taicpu.Swapoperands;
      begin
      end;


    function taicpu.FindInsentry(objdata:TObjData):boolean;
      var
        i : longint;
      begin
        result:=false;
      { Things which may only be done once, not when a second pass is done to
        optimize }
        if (Insentry=nil) or ((InsEntry^.flags and IF_PASS2)<>0) then
         begin
           { create the .ot fields }
           create_ot(objdata);
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
           Message1(asmw_e_opcode_not_in_table,gas_op2str[opcode]);
           exit;
         end;
        insentry:=@instab[i];
        while (insentry^.opcode=opcode) do
         begin
           if matches(insentry)=100 then
             begin
               result:=true;
               exit;
             end;
           inc(i);
           insentry:=@instab[i];
         end;
        Message1(asmw_e_invalid_opcode_and_operands,GetString);
        { No instruction found, set insentry to nil and inssize to -1 }
        insentry:=nil;
        inssize:=-1;
      end;


    procedure taicpu.gencode(objdata:TObjData);
      var
        bytes : dword;
        i_field : byte;

      procedure setshifterop(op : byte);
        begin
          case oper[op]^.typ of
            top_const:
              begin
                i_field:=1;
                bytes:=bytes or (oper[op]^.val and $fff);
              end;
            top_reg:
              begin
                i_field:=0;
                bytes:=bytes or (getsupreg(oper[op]^.reg) shl 16);

                { does a real shifter op follow? }
                if (op+1<=op) and (oper[op+1]^.typ=top_shifterop) then
                  begin
                  end;
              end;
          else
            internalerror(2005091103);
          end;
        end;

      begin
        bytes:=$0;
        { evaluate and set condition code }

        { condition code allowed? }

        { setup rest of the instruction }
        case insentry^.code[0] of
          #$08:
            begin
              { set instruction code }
              bytes:=bytes or (ord(insentry^.code[1]) shl 26);
              bytes:=bytes or (ord(insentry^.code[2]) shl 21);

              { set destination }
              bytes:=bytes or (getsupreg(oper[0]^.reg) shl 12);

              { create shifter op }
              setshifterop(1);

              { set i field }
              bytes:=bytes or (i_field shl 25);

              { set s if necessary }
              if oppostfix=PF_S then
                bytes:=bytes or (1 shl 20);
            end;
          #$ff:
            internalerror(2005091101);
          else
            internalerror(2005091102);
        end;
        { we're finished, write code }
        objdata.writebytes(bytes,sizeof(bytes));
      end;


{$ifdef dummy}
      (*
static void gencode (long segment, long offset, int bits,
                     insn *ins, char *codes, long insn_end)
{
    int has_S_code;             /* S - setflag */
    int has_B_code;             /* B - setflag */
    int has_T_code;             /* T - setflag */
    int has_W_code;             /* ! => W flag */
    int has_F_code;             /* ^ => S flag */
    int keep;
    unsigned char c;
    unsigned char bytes[4];
    long          data, size;
    static int cc_code[] =      /* bit pattern of cc */
  {                             /* order as enum in  */
    0x0E, 0x03, 0x02, 0x00,     /* nasm.h            */
    0x0A, 0x0C, 0x08, 0x0D,
    0x09, 0x0B, 0x04, 0x01,
    0x05, 0x07, 0x06,
  };

(*
#ifdef DEBUG
static char *CC[] =
  {                                    /* condition code names */
    "AL", "CC", "CS", "EQ",
    "GE", "GT", "HI", "LE",
    "LS", "LT", "MI", "NE",
    "PL", "VC", "VS", "",
    "S"
};
*)

    has_S_code = (ins->condition & C_SSETFLAG);
    has_B_code = (ins->condition & C_BSETFLAG);
    has_T_code = (ins->condition & C_TSETFLAG);
    has_W_code = (ins->condition & C_EXSETFLAG);
    has_F_code = (ins->condition & C_FSETFLAG);
    ins->condition = (ins->condition & 0x0F);

(*
    if (rt_debug)
      {
    printf ("gencode: instruction: %s%s", insn_names[ins->opcode],
            CC[ins->condition & 0x0F]);
    if (has_S_code)
      printf ("S");
    if (has_B_code)
      printf ("B");
    if (has_T_code)
      printf ("T");
    if (has_W_code)
      printf ("!");
    if (has_F_code)
      printf ("^");

    printf ("\n");

    c = *codes;

    printf ("   (%d)  decode - '0x%02X'\n", ins->operands, c);


    bytes[0] = 0xB;
    bytes[1] = 0xE;
    bytes[2] = 0xE;
    bytes[3] = 0xF;
      }
*)
    // First condition code in upper nibble
    if (ins->condition < C_NONE)
      {
        c = cc_code[ins->condition] << 4;
      }
    else
      {
        c = cc_code[C_AL] << 4; // is often ALWAYS but not always
      }


    switch (keep = *codes)
      {
        case 1:
          // B, BL
          ++codes;
          c |= *codes++;
          bytes[0] = c;

          if (ins->oprs[0].segment != segment)
            {
              // fais une relocation
              c = 1;
              data = 0; // Let the linker locate ??
            }
          else
            {
              c = 0;
              data = ins->oprs[0].offset - (offset + 8);

              if (data % 4)
                {
                  errfunc (ERR_NONFATAL, "offset not aligned on 4 bytes");
                }
            }

          if (data >= 0x1000)
            {
              errfunc (ERR_NONFATAL, "too long offset");
            }

          data = data >> 2;
          bytes[1] = (data >> 16) & 0xFF;
          bytes[2] = (data >> 8)  & 0xFF;
          bytes[3] = (data )      & 0xFF;

          if (c == 1)
            {
//            out (offset, segment, &bytes[0], OUT_RAWDATA+1, NO_SEG, NO_SEG);
              out (offset, segment, &bytes[0], OUT_REL3ADR+4, ins->oprs[0].segment, NO_SEG);
            }
          else
            {
              out (offset, segment, &bytes[0], OUT_RAWDATA+4, NO_SEG, NO_SEG);
            }
          return;

        case 2:
          // SWI
          ++codes;
          c |= *codes++;
          bytes[0] = c;
          data = ins->oprs[0].offset;
          bytes[1] = (data >> 16) & 0xFF;
          bytes[2] = (data >> 8) & 0xFF;
          bytes[3] = (data) & 0xFF;
          out (offset, segment, &bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);
          return;
        case 3:
          // BX
          ++codes;
          c |= *codes++;
          bytes[0] = c;
          bytes[1] = *codes++;
          bytes[2] = *codes++;
          bytes[3] = *codes++;
          c = regval (&ins->oprs[0],1);
          if (c == 15)  // PC
            {
              errfunc (ERR_WARNING, "'BX' with R15 has undefined behaviour");
            }
          else if (c > 15)
            {
              errfunc (ERR_NONFATAL, "Illegal register specified for 'BX'");
            }

          bytes[3] |= (c & 0x0F);
          out (offset, segment, bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);
          return;

        case 4:         // AND Rd,Rn,Rm
        case 5:         // AND Rd,Rn,Rm,<shift>Rs
        case 6:         // AND Rd,Rn,Rm,<shift>imm
        case 7:         // AND Rd,Rn,<shift>imm
          ++codes;
#ifdef DEBUG
          if (rt_debug)
            {
              printf ("         decode - '0x%02X'\n", keep);
              printf ("           code - '0x%02X'\n", (unsigned char) ( *codes));
            }
#endif
          bytes[0] = c | *codes;
          ++codes;

          bytes[1] = *codes;
          if (has_S_code)
            bytes[1] |= 0x10;
          c = regval (&ins->oprs[1],1);
          // Rn in low nibble
          bytes[1] |= c;

          // Rd in high nibble
          bytes[2] = regval (&ins->oprs[0],1) << 4;

          if (keep != 7)
            {
              // Rm in low nibble
              bytes[3] = regval (&ins->oprs[2],1);
            }

          // Shifts if any
          if (keep == 5 || keep == 6)
            {
              // Shift in bytes 2 and 3
              if (keep == 5)
                {
                  // Rs
                  c = regval (&ins->oprs[3],1);
                  bytes[2] |= c;

                  c = 0x10;             // Set bit 4 in byte[3]
                }
              if (keep == 6)
                {
                  c = (ins->oprs[3].offset) & 0x1F;

                  // #imm
                  bytes[2] |= c >> 1;
                  if (c & 0x01)
                    {
                      bytes[3] |= 0x80;
                    }
                  c = 0;                // Clr bit 4 in byte[3]
                }
              // <shift>
              c |= shiftval (&ins->oprs[3]) << 5;

              bytes[3] |= c;
            }

          // reg,reg,imm
          if (keep == 7)
            {
              int shimm;

              shimm = imm_shift (ins->oprs[2].offset);

              if (shimm == -1)
                {
                  errfunc (ERR_NONFATAL, "cannot create that constant");
                }
              bytes[3] = shimm & 0xFF;
              bytes[2] |= (shimm & 0xF00) >> 8;
            }

          out (offset, segment, bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);
          return;

        case 8:         // MOV Rd,Rm
        case 9:         // MOV Rd,Rm,<shift>Rs
        case 0xA:       // MOV Rd,Rm,<shift>imm
        case 0xB:       // MOV Rd,<shift>imm
          ++codes;
#ifdef DEBUG
          if (rt_debug)
            {
              printf ("         decode - '0x%02X'\n", keep);
              printf ("           code - '0x%02X'\n", (unsigned char) ( *codes));
            }
#endif
          bytes[0] = c | *codes;
          ++codes;

          bytes[1] = *codes;
          if (has_S_code)
            bytes[1] |= 0x10;

          // Rd in high nibble
          bytes[2] = regval (&ins->oprs[0],1) << 4;

          if (keep != 0x0B)
            {
              // Rm in low nibble
              bytes[3] = regval (&ins->oprs[1],1);
            }

          // Shifts if any
          if (keep == 0x09 || keep == 0x0A)
            {
              // Shift in bytes 2 and 3
              if (keep == 0x09)
                {
                  // Rs
                  c = regval (&ins->oprs[2],1);
                  bytes[2] |= c;

                  c = 0x10;             // Set bit 4 in byte[3]
                }
              if (keep == 0x0A)
                {
                  c = (ins->oprs[2].offset) & 0x1F;

                  // #imm
                  bytes[2] |= c >> 1;
                  if (c & 0x01)
                    {
                      bytes[3] |= 0x80;
                    }
                  c = 0;                // Clr bit 4 in byte[3]
                }
              // <shift>
              c |= shiftval (&ins->oprs[2]) << 5;

              bytes[3] |= c;
            }

          // reg,imm
          if (keep == 0x0B)
            {
              int shimm;

              shimm = imm_shift (ins->oprs[1].offset);

              if (shimm == -1)
                {
                  errfunc (ERR_NONFATAL, "cannot create that constant");
                }
              bytes[3] = shimm & 0xFF;
              bytes[2] |= (shimm & 0xF00) >> 8;
            }

          out (offset, segment, bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);
          return;


        case 0xC:       // CMP Rn,Rm
        case 0xD:       // CMP Rn,Rm,<shift>Rs
        case 0xE:       // CMP Rn,Rm,<shift>imm
        case 0xF:       // CMP Rn,<shift>imm
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes;

          // Implicit S code
          bytes[1] |= 0x10;

          c = regval (&ins->oprs[0],1);
          // Rn in low nibble
          bytes[1] |= c;

          // No destination
          bytes[2] = 0;

          if (keep != 0x0B)
            {
              // Rm in low nibble
              bytes[3] = regval (&ins->oprs[1],1);
            }

          // Shifts if any
          if (keep == 0x0D || keep == 0x0E)
            {
              // Shift in bytes 2 and 3
              if (keep == 0x0D)
                {
                  // Rs
                  c = regval (&ins->oprs[2],1);
                  bytes[2] |= c;

                  c = 0x10;             // Set bit 4 in byte[3]
                }
              if (keep == 0x0E)
                {
                  c = (ins->oprs[2].offset) & 0x1F;

                  // #imm
                  bytes[2] |= c >> 1;
                  if (c & 0x01)
                    {
                      bytes[3] |= 0x80;
                    }
                  c = 0;                // Clr bit 4 in byte[3]
                }
              // <shift>
              c |= shiftval (&ins->oprs[2]) << 5;

              bytes[3] |= c;
            }

          // reg,imm
          if (keep == 0x0F)
            {
              int shimm;

              shimm = imm_shift (ins->oprs[1].offset);

              if (shimm == -1)
                {
                  errfunc (ERR_NONFATAL, "cannot create that constant");
                }
              bytes[3] = shimm & 0xFF;
              bytes[2] |= (shimm & 0xF00) >> 8;
            }

          out (offset, segment, bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);
          return;

        case 0x10:      // MRS Rd,<psr>
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          // Rd
          c = regval (&ins->oprs[0],1);

          bytes[2] = c << 4;

          bytes[3] = 0;

          c = ins->oprs[1].basereg;

          if (c == R_CPSR || c == R_SPSR)
            {
              if (c == R_SPSR)
                {
                  bytes[1] |= 0x40;
                }
            }
          else
            {
              errfunc (ERR_NONFATAL, "CPSR or SPSR expected");
            }

          out (offset, segment, bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);

          return;

        case 0x11:      // MSR <psr>,Rm
        case 0x12:      // MSR <psrf>,Rm
        case 0x13:      // MSR <psrf>,#expression
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          bytes[2] = *codes;


          if (keep == 0x11 || keep == 0x12)
            {
              // Rm
              c = regval (&ins->oprs[1],1);

              bytes[3] = c;
            }
          else
            {
              int shimm;

              shimm = imm_shift (ins->oprs[1].offset);

              if (shimm == -1)
                {
                  errfunc (ERR_NONFATAL, "cannot create that constant");
                }
              bytes[3] = shimm & 0xFF;
              bytes[2] |= (shimm & 0xF00) >> 8;
            }

          c = ins->oprs[0].basereg;

          if ( keep == 0x11)
            {
              if ( c == R_CPSR || c == R_SPSR)
                {
                if ( c== R_SPSR)
                  {
                    bytes[1] |= 0x40;
                  }
                }
            else
              {
                errfunc (ERR_NONFATAL, "CPSR or SPSR expected");
              }
            }
          else
            {
              if ( c == R_CPSR_FLG || c == R_SPSR_FLG)
                {
                  if ( c== R_SPSR_FLG)
                    {
                      bytes[1] |= 0x40;
                    }
                }
              else
                {
                  errfunc (ERR_NONFATAL, "CPSR_flg or SPSR_flg expected");
                }
            }
          break;

        case 0x14:      // MUL  Rd,Rm,Rs
        case 0x15:      // MULA Rd,Rm,Rs,Rn
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          bytes[3] = *codes;

          // Rd
          bytes[1] |= regval (&ins->oprs[0],1);
          if (has_S_code)
            bytes[1] |= 0x10;

          // Rm
          bytes[3] |= regval (&ins->oprs[1],1);

          // Rs
          bytes[2] = regval (&ins->oprs[2],1);

          if (keep == 0x15)
            {
              bytes[2] |= regval (&ins->oprs[3],1) << 4;
            }
          break;

        case 0x16:      // SMLAL RdHi,RdLo,Rm,Rs
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          bytes[3] = *codes;

          // RdHi
          bytes[1] |= regval (&ins->oprs[1],1);
          if (has_S_code)
            bytes[1] |= 0x10;

          // RdLo
          bytes[2] = regval (&ins->oprs[0],1) << 4;
          // Rm
          bytes[3] |= regval (&ins->oprs[2],1);

          // Rs
          bytes[2] |= regval (&ins->oprs[3],1);

          break;

        case 0x17:      // LDR Rd, expression
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          // Rd
          bytes[2] = regval (&ins->oprs[0],1) << 4;
          if (has_B_code)
            bytes[1] |= 0x40;
          if (has_T_code)
            {
              errfunc (ERR_NONFATAL, "'T' not allowed in pre-index mode");
            }
          if (has_W_code)
            {
              errfunc (ERR_NONFATAL, "'!' not allowed");
            }

          // Rn - implicit R15
          bytes[1] |= 0xF;

          if (ins->oprs[1].segment != segment)
            {
              errfunc (ERR_NONFATAL, "label not in same segment");
            }

          data = ins->oprs[1].offset - (offset + 8);

          if (data < 0)
            {
              data = -data;
            }
          else
            {
              bytes[1] |= 0x80;
            }

          if (data >= 0x1000)
            {
              errfunc (ERR_NONFATAL, "too long offset");
            }

          bytes[2] |= ((data & 0xF00) >> 8);
          bytes[3] = data & 0xFF;
          break;

        case 0x18:      // LDR Rd, [Rn]
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          // Rd
          bytes[2] = regval (&ins->oprs[0],1) << 4;
          if (has_B_code)
            bytes[1] |= 0x40;
          if (has_T_code)
            {
              bytes[1] |= 0x20;         // write-back
            }
          else
            {
              bytes[0] |= 0x01;         // implicit pre-index mode
            }

          if (has_W_code)
            {
              bytes[1] |= 0x20;         // write-back
            }

          // Rn
          c = regval (&ins->oprs[1],1);
          bytes[1] |= c;

          if (c == 0x15)                // R15
            data = -8;
          else
            data = 0;

          if (data < 0)
            {
              data = -data;
            }
          else
            {
              bytes[1] |= 0x80;
            }

          bytes[2] |= ((data & 0xF00) >> 8);
          bytes[3] = data & 0xFF;
          break;

        case 0x19:      // LDR Rd, [Rn,#expression]
        case 0x20:      // LDR Rd, [Rn,Rm]
        case 0x21:      // LDR Rd, [Rn,Rm,shift]
          ++codes;

          bytes[0] = c | *codes++;

          bytes[1] = *codes++;

          // Rd
          bytes[2] = regval (&ins->oprs[0],1) << 4;
          if (has_B_code)
            bytes[1] |= 0x40;

          // Rn
          c = regval (&ins->oprs[1],1);
          bytes[1] |= c;

          if (ins->oprs[ins->operands-1].bracket)       // FIXME: Bracket on last operand -> pre-index  <--
            {
              bytes[0] |= 0x01;         // pre-index mode
              if (has_W_code)
                {
                  bytes[1] |= 0x20;
                }
              if (has_T_code)
                {
                  errfunc (ERR_NONFATAL, "'T' not allowed in pre-index mode");
                }
            }
          else
            {
              if (has_T_code)           // Forced write-back in post-index mode
                {
                  bytes[1] |= 0x20;
                }
              if (has_W_code)
                {
                  errfunc (ERR_NONFATAL, "'!' not allowed in post-index mode");
                }
            }

          if (keep == 0x19)
            {
              data = ins->oprs[2].offset;

              if (data < 0)
                {
                  data = -data;
                }
              else
                {
                  bytes[1] |= 0x80;
                }

              if (data >= 0x1000)
                {
                  errfunc (ERR_NONFATAL, "too long offset");
                }

              bytes[2] |= ((data & 0xF00) >> 8);
              bytes[3] = data & 0xFF;
            }
          else
            {
              if (ins->oprs[2].minus == 0)
                {
                  bytes[1] |= 0x80;
                }
              c = regval (&ins->oprs[2],1);
              bytes[3] = c;

              if (keep == 0x21)
                {
                  c = ins->oprs[3].offset;
                  if (c > 0x1F)
                    {
                      errfunc (ERR_NONFATAL, "too large shiftvalue");
                      c = c & 0x1F;
                    }

                  bytes[2] |= c >> 1;
                  if (c & 0x01)
                    {
                      bytes[3] |= 0x80;
                    }
                  bytes[3] |= shiftval (&ins->oprs[3]) << 5;
                }
            }

          break;

        case 0x22:      // LDRH Rd, expression
          ++codes;

          bytes[0] = c | 0x01;          // Implicit pre-index

          bytes[1] = *codes++;

          // Rd
          bytes[2] = regval (&ins->oprs[0],1) << 4;

          // Rn - implicit R15
          bytes[1] |= 0xF;

          if (ins->oprs[1].segment != segment)
            {
              errfunc (ERR_NONFATAL, "label not in same segment");
            }

          data = ins->oprs[1].offset - (offset + 8);

          if (data < 0)
            {
              data = -data;
            }
          else
            {
              bytes[1] |= 0x80;
            }

          if (data >= 0x100)
            {
              errfunc (ERR_NONFATAL, "too long offset");
            }
          bytes[3] = *codes++;

          bytes[2] |= ((data & 0xF0) >> 4);
          bytes[3] |= data & 0xF;
          break;

        case 0x23:      // LDRH Rd, Rn
          ++codes;

          bytes[0] = c | 0x01;          // Implicit pre-index

          bytes[1] = *codes++;

          // Rd
          bytes[2] = regval (&ins->oprs[0],1) << 4;

          // Rn
          c = regval (&ins->oprs[1],1);
          bytes[1] |= c;

          if (c == 0x15)                // R15
            data = -8;
          else
            data = 0;

          if (data < 0)
            {
              data = -data;
            }
          else
            {
              bytes[1] |= 0x80;
            }

          if (data >= 0x100)
            {
              errfunc (ERR_NONFATAL, "too long offset");
            }
          bytes[3] = *codes++;

          bytes[2] |= ((data & 0xF0) >> 4);
          bytes[3] |= data & 0xF;
          break;

        case 0x24:      // LDRH Rd, Rn, expression
        case 0x25:      // LDRH Rd, Rn, Rm
          ++codes;

          bytes[0] = c;

          bytes[1] = *codes++;

          // Rd
          bytes[2] = regval (&ins->oprs[0],1) << 4;

          // Rn
          c = regval (&ins->oprs[1],1);
          bytes[1] |= c;

          if (ins->oprs[ins->operands-1].bracket)       // FIXME: Bracket on last operand -> pre-index  <--
            {
              bytes[0] |= 0x01;         // pre-index mode
              if (has_W_code)
                {
                  bytes[1] |= 0x20;
                }
            }
          else
            {
              if (has_W_code)
                {
                  errfunc (ERR_NONFATAL, "'!' not allowed in post-index mode");
                }
            }

          bytes[3] = *codes++;

          if (keep == 0x24)
            {
              data = ins->oprs[2].offset;

              if (data < 0)
                {
                  data = -data;
                }
              else
                {
                  bytes[1] |= 0x80;
                }

              if (data >= 0x100)
                {
                  errfunc (ERR_NONFATAL, "too long offset");
                }

              bytes[2] |= ((data & 0xF0) >> 4);
              bytes[3] |= data & 0xF;
            }
          else
            {
              if (ins->oprs[2].minus == 0)
                {
                  bytes[1] |= 0x80;
                }
              c = regval (&ins->oprs[2],1);
              bytes[3] |= c;

            }
          break;

        case 0x26:      // LDM/STM Rn, {reg-list}
          ++codes;

          bytes[0] = c;

          bytes[0] |= ( *codes >> 4) & 0xF;
          bytes[1] = ( *codes << 4) & 0xF0;
          ++codes;

          if (has_W_code)
            {
              bytes[1] |= 0x20;
            }
          if (has_F_code)
            {
              bytes[1] |= 0x40;
            }

          // Rn
          bytes[1] |= regval (&ins->oprs[0],1);

          data = ins->oprs[1].basereg;

          bytes[2] = ((data >> 8) & 0xFF);
          bytes[3] = (data & 0xFF);

          break;

        case 0x27:      // SWP Rd, Rm, [Rn]
          ++codes;

          bytes[0] = c;

          bytes[0] |= *codes++;

          bytes[1] = regval (&ins->oprs[2],1);
          if (has_B_code)
            {
              bytes[1] |= 0x40;
            }
          bytes[2] = regval (&ins->oprs[0],1) << 4;
          bytes[3] = *codes++;
          bytes[3] |= regval (&ins->oprs[1],1);
          break;

        default:
          errfunc (ERR_FATAL, "unknown decoding of instruction");

          bytes[0] = c;
          // And a fix nibble
          ++codes;
          bytes[0] |= *codes++;

         if ( *codes == 0x01)           // An I bit
           {

           }
         if ( *codes == 0x02)           // An I bit
           {

           }
         ++codes;
      }
    out (offset, segment, bytes, OUT_RAWDATA+4, NO_SEG, NO_SEG);
}


*)
{$endif dummy
}

begin
  cai_align:=tai_align;
end.

