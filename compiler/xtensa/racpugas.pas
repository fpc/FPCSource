{
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Does the parsing for the Xtensa GNU AS styled inline assembler.

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
Unit racpugas;

{$i fpcdefs.inc}

  Interface

    uses
      raatt,raxtensa,
      cpubase;

    type

      txtensaattreader = class(tattreader)
        actoppostfix : TOpPostfix;
        actIsPrefixed: boolean;

       function is_asmopcode(const s: string):boolean;override;
        function is_register(const s:string):boolean;override;
        procedure handleopcode;override;
        procedure BuildReference(oper : TXtensaOperand);
        procedure BuildOperand(oper : TXtensaOperand);
        procedure BuildSpecialreg(oper : TXtensaOperand);
        procedure BuildOpCode(instr : TXtensaInstruction);
        procedure ReadSym(oper : TXtensaOperand);
        procedure ConvertCalljmp(instr : TXtensaInstruction);
        procedure HandleTargetDirective; override;
      end;

  Implementation

    uses
      { helpers }
      cutils,
      { global }
      globtype,globals,verbose,
      systems,aasmbase,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symsym,symdef,
      procinfo,
      rabase,rautils,
      cgbase,cgutils,paramgr;


    function txtensaattreader.is_register(const s:string):boolean;
      type
        treg2str = record
          name : string[3];
          reg : tregister;
        end;

      const
        extraregs : array[0..0] of treg2str = (
          (name: 'A1'; reg : NR_A0)
          );

      var
        i : longint;

      begin
        result:=inherited is_register(s);
        { reg found?
          possible aliases are always 2 char
        }
        if result or (not (length(s) in [2,3])) then
          exit;
        for i:=low(extraregs) to high(extraregs) do
          begin
            if s=extraregs[i].name then
              begin
                actasmregister:=extraregs[i].reg;
                result:=true;
                actasmtoken:=AS_REGISTER;
                exit;
              end;
          end;
      end;


    procedure txtensaattreader.ReadSym(oper : TXtensaOperand);
      var
         tempstr, mangledname : string;
         typesize,l,k : tcgint;
      begin
        tempstr:=actasmpattern;
        Consume(AS_ID);
        { typecasting? }
        if (actasmtoken=AS_LPAREN) and
           SearchType(tempstr,typesize) then
          begin
            oper.hastype:=true;
            Consume(AS_LPAREN);
            BuildOperand(oper);
            Consume(AS_RPAREN);
            if oper.opr.typ in [OPR_REFERENCE,OPR_LOCAL] then
              oper.SetSize(typesize,true);
          end
        else
          if not oper.SetupVar(tempstr,false) then
            Message1(sym_e_unknown_id,tempstr);
        { record.field ? }
        if actasmtoken=AS_DOT then
          begin
            BuildRecordOffsetSize(tempstr,l,k,mangledname,false);
            if (mangledname<>'') then
              Message(asmr_e_invalid_reference_syntax);
            inc(oper.opr.ref.offset,l);
          end;
      end;


    Procedure txtensaattreader.BuildReference(oper : TXtensaOperand);

      procedure do_error;
        begin
          Message(asmr_e_invalid_reference_syntax);
          RecoverConsume(false);
        end;


      procedure test_end(require_rbracket : boolean);
        begin
          if not(actasmtoken in [AS_SEPARATOR,AS_end]) then
            do_error
          else
            begin
{$IFDEF debugasmreader}
              writeln('TEST_end_FINAL_OK. Created the following ref:');
              writeln('oper.opr.ref.base=',ord(oper.opr.ref.base));
              writeln('oper.opr.ref.signindex=',ord(oper.opr.ref.signindex));
              writeln('oper.opr.ref.addressmode=',ord(oper.opr.ref.addressmode));
              writeln;
{$endIF debugasmreader}
            end;
        end;


      procedure read_index(require_rbracket : boolean);
        var
          recname : string;
          o_int,s_int : tcgint;
        begin
          case actasmtoken of
            AS_ID :
              begin
                recname := actasmpattern;
                Consume(AS_ID);
                BuildRecordOffsetSize(recname,o_int,s_int,recname,false);
                inc(oper.opr.ref.offset,o_int);
                test_end(require_rbracket);
                exit;
              end;
            AS_AT:
              begin
                do_error;
                exit;
              end;
            AS_DOT : // local label
              begin
                BuildConstExpression(true,false);
                test_end(require_rbracket);
                exit;
              end;
            AS_SEPARATOR,AS_end :
              begin
                if not require_rbracket then
                  begin
                    test_end(false);
                    exit;
                  end
                else
                  begin
                    do_error;
                    exit;
                  end;
              end;
            else
              begin
                // unexpected token
                do_error;
                exit;
              end;
          end; // case
        end;

      var
        lab : TASMLABEL;
      begin
        Consume(AS_LBRACKET);
        if actasmtoken=AS_REGISTER then
          begin
            oper.opr.ref.base:=actasmregister;
            Consume(AS_REGISTER);
            case actasmtoken of
              AS_COMMA :
                begin
                  Consume(AS_COMMA);
                  read_index(true);
                  exit;
                end;
              else
                begin
                  Message(asmr_e_invalid_reference_syntax);
                  RecoverConsume(false);
                end;
            end;
          end
        else
          Begin
            case actasmtoken of
              AS_ID :
                begin
                  // TODO: Stackpointer implied,
                   Message(asmr_e_invalid_reference_syntax);
                   RecoverConsume(false);
                   exit;
                end;
              else
                begin
                  Message(asmr_e_invalid_reference_syntax);
                  RecoverConsume(false);
                  exit;
                end;
            end;
          end;
      end;


    Procedure txtensaattreader.BuildOperand(oper : TXtensaOperand);
      var
        expr : string;
        typesize,l : tcgint;


        procedure AddLabelOperand(hl:tasmlabel);
          begin
            oper.opr.typ:=OPR_SYMBOL;
            oper.opr.symbol:=hl;
          end;


        procedure MaybeRecordOffset;
          var
            mangledname: string;
            hasdot  : boolean;
            l,
            toffset,
            tsize   : tcgint;
          begin
            if not(actasmtoken in [AS_DOT,AS_PLUS,AS_MINUS]) then
             exit;
            l:=0;
            mangledname:='';
            hasdot:=(actasmtoken=AS_DOT);
            if hasdot then
              begin
                if expr<>'' then
                  begin
                    BuildRecordOffsetSize(expr,toffset,tsize,mangledname,false);
                    if (oper.opr.typ<>OPR_CONSTANT) and
                       (mangledname<>'') then
                      Message(asmr_e_wrong_sym_type);
                    inc(l,toffset);
                    oper.SetSize(tsize,true);
                  end;
              end;
            if actasmtoken in [AS_PLUS,AS_MINUS] then
              inc(l,BuildConstExpression(true,false));
            case oper.opr.typ of
              OPR_LOCAL :
                begin
                  { don't allow direct access to fields of parameters, because that
                    will generate buggy code. Allow it only for explicit typecasting }
                  if hasdot and
                     (not oper.hastype) then
                     checklocalsubscript(oper.opr.localsym);
                  oper.opr.localforceref:=true;
                  inc(oper.opr.localsymofs,l)
                end;
              OPR_CONSTANT :
                inc(oper.opr.val,l);
              OPR_REFERENCE :
                if (mangledname<>'') then
                  begin
                    if (oper.opr.val<>0) then
                      Message(asmr_e_wrong_sym_type);
                    oper.opr.typ:=OPR_SYMBOL;
                    oper.opr.symbol:=current_asmdata.RefAsmSymbol(mangledname,AT_FUNCTION);
                  end
                else
                  inc(oper.opr.val,l);
              OPR_SYMBOL:
                Message(asmr_e_invalid_symbol_ref);
              else
                internalerror(200309221);
            end;
          end;


        function MaybeBuildReference:boolean;
          { Try to create a reference, if not a reference is found then false
            is returned }
          begin
            MaybeBuildReference:=true;
            case actasmtoken of
              AS_INTNUM,
              AS_MINUS,
              AS_PLUS:
                Begin
                  oper.opr.ref.offset:=BuildConstExpression(True,False);
                  if actasmtoken<>AS_LPAREN then
                    Message(asmr_e_invalid_reference_syntax)
                  else
                    BuildReference(oper);
                end;
              AS_LPAREN:
                BuildReference(oper);
              AS_ID: { only a variable is allowed ... }
                Begin
                  ReadSym(oper);
                  case actasmtoken of
                    AS_end,
                    AS_SEPARATOR,
                    AS_COMMA: ;
                    AS_LPAREN:
                      BuildReference(oper);
                  else
                    Begin
                      Message(asmr_e_invalid_reference_syntax);
                      Consume(actasmtoken);
                    end;
                  end; {end case }
                end;
              else
               MaybeBuildReference:=false;
            end; { end case }
          end;


        procedure BuildDirectRef;

          function GetConstLabel(const symname: string; ofs: aint): TAsmLabel;
            var
              hp: tai;
              newconst: tai_const;
              lab: TAsmLabel;
            begin
              if symname<>'' then
                newconst:=tai_const.Createname(symname,ofs)
              else
                newconst:=tai_const.Create_32bit(ofs);

              hp:=tai(current_procinfo.aktlocaldata.First);
              while assigned(hp) do
                begin
                  if hp.typ=ait_const then
                    begin
                      if (tai_const(hp).sym=newconst.sym) and
                         (tai_const(hp).value=newconst.value) and
                         assigned(hp.Previous) and
                         (tai(hp.previous).typ=ait_label) then
                        begin
                          newconst.Free;
                          result:=tai_label(hp.Previous).labsym;
                          exit;
                        end;
                    end;

                  hp:=tai(hp.Next);
                end;

              current_asmdata.getjumplabel(lab);
              current_procinfo.aktlocaldata.concat(tai_align.create(4));
              current_procinfo.aktlocaldata.concat(tai_label.create(lab));
              current_procinfo.aktlocaldata.concat(newconst);
              result:=lab;
            end;

          var
            symtype: TAsmsymtype;
            sym: string;
            val: tcgint;
          begin
            case actasmtoken of
              AS_INTNUM,
              AS_ID:
                begin
                  BuildConstSymbolExpression(true,false,false,val,sym,symtype);

                  if symtype=AT_NONE then
                    sym:='';

                  reference_reset(oper.opr.ref,4,[]);
                  oper.opr.ref.symbol:=GetConstLabel(sym,val);
                end;
              else
                ;
            end;
          end;


      function getregsetindex(reg: tregister): integer;
        begin
          if getsubreg(reg)=R_SUBFS then
            begin
              result:=getsupreg(reg)*2;
              if result>32 then
                result:=result-63;
            end
          else
            result:=getsupreg(reg);
        end;

      var
        tempreg : tregister;
        ireg : tsuperregister;
        regtype: tregistertype;
        subreg: tsubregister;
        hl : tasmlabel;
        {ofs : longint;}
        registerset : tcpuregisterset;
      Begin
        expr:='';
        case actasmtoken of
          AS_LBRACKET: { Memory reference or constant expression }
            Begin
              oper.InitRef;
              BuildReference(oper);
            end;

          AS_MINUS,
          AS_PLUS,
          AS_INTNUM: { Constant expression  }
            Begin
              BuildConstantOperand(oper);
            end;
          AS_ID: { A constant expression, or a Variable ref.  }
            Begin
              if is_locallabel(actasmpattern) then
               begin
                 CreateLocalLabel(actasmpattern,hl,false);
                 Consume(AS_ID);
                 AddLabelOperand(hl);
               end
              else
               { Check for label }
               if SearchLabel(actasmpattern,hl,false) then
                begin
                  Consume(AS_ID);
                  AddLabelOperand(hl);
                end
              else
               { probably a variable or normal expression }
               { or a procedure (such as in CALL ID)      }
               Begin
                 { is it a constant ? }
                 if SearchIConstant(actasmpattern,l) then
                  Begin
                    if not (oper.opr.typ in [OPR_NONE,OPR_CONSTANT]) then
                     Message(asmr_e_invalid_operand_type);
                    BuildConstantOperand(oper);
                  end
                 else
                  begin
                    expr:=actasmpattern;
                    Consume(AS_ID);
                    { typecasting? }
                    if (actasmtoken=AS_LPAREN) and
                       SearchType(expr,typesize) then
                     begin
                       oper.hastype:=true;
                       Consume(AS_LPAREN);
                       BuildOperand(oper);
                       Consume(AS_RPAREN);
                       if oper.opr.typ in [OPR_REFERENCE,OPR_LOCAL] then
                         oper.SetSize(typesize,true);
                     end
                    else
                     begin
                       if not(oper.SetupVar(expr,false)) then
                        Begin
                          { look for special symbols ... }
                          if expr= '__HIGH' then
                            begin
                              consume(AS_LPAREN);
                              if not oper.setupvar('high'+actasmpattern,false) then
                                Message1(sym_e_unknown_id,'high'+actasmpattern);
                              consume(AS_ID);
                              consume(AS_RPAREN);
                            end
                          else
                           if expr = '__RESULT' then
                            oper.SetUpResult
                          else
                           if expr = '__SELF' then
                            oper.SetupSelf
                          else
                           if expr = '__OLDEBP' then
                            oper.SetupOldEBP
                          else
                            Message1(sym_e_unknown_id,expr);
                        end;
                     end;
                  end;
                  if actasmtoken=AS_DOT then
                    MaybeRecordOffset;
                  { add a constant expression? }
                  if (actasmtoken=AS_PLUS) then
                   begin
                     l:=BuildConstExpression(true,false);
                     case oper.opr.typ of
                       OPR_CONSTANT :
                         inc(oper.opr.val,l);
                       OPR_LOCAL :
                         inc(oper.opr.localsymofs,l);
                       OPR_REFERENCE :
                         inc(oper.opr.ref.offset,l);
                       else
                         internalerror(2003092004);
                     end;
                   end
               end;
              { Do we have a indexing reference, then parse it also }
              if actasmtoken=AS_LPAREN then
                BuildReference(oper);
            end;

          { Register, a variable reference or a constant reference  }
          AS_REGISTER:
            Begin
              { save the type of register used. }
              tempreg:=actasmregister;
              Consume(AS_REGISTER);
              if (actasmtoken in [AS_end,AS_SEPARATOR,AS_COMMA]) then
                Begin
                  if not (oper.opr.typ in [OPR_NONE,OPR_REGISTER]) then
                    Message(asmr_e_invalid_operand_type);
                  oper.opr.typ:=OPR_REGISTER;
                  oper.opr.reg:=tempreg;
                end
              else
                Message(asmr_e_syn_operand);
            end;

          AS_end,
          AS_SEPARATOR,
          AS_COMMA: ;
        else
          Begin
            Message(asmr_e_syn_operand);
            Consume(actasmtoken);
          end;
        end; { end case }
      end;

    procedure txtensaattreader.BuildSpecialreg(oper: TXtensaOperand);
      var
        hs, reg : String;
        ch : char;
        i, t : longint;
        hreg : tregister;
        flags : tspecialregflags;
      begin
        hreg:=NR_NO;
        case actasmtoken of
          AS_REGISTER:
            begin
              oper.opr.typ:=OPR_REGISTER;
              oper.opr.reg:=actasmregister;
              Consume(AS_REGISTER);
            end;
          else
            Message(asmr_e_invalid_operand_type);
        end;
      end;


{*****************************************************************************
                                tarmattreader
*****************************************************************************}

    procedure txtensaattreader.BuildOpCode(instr : TXtensaInstruction);
      var
        operandnum : longint;
      Begin
        { opcode }
        if (actasmtoken<>AS_OPCODE) then
         Begin
           Message(asmr_e_invalid_or_missing_opcode);
           RecoverConsume(true);
           exit;
         end;
        { Fill the instr object with the current state }
        with instr do
          begin
            Opcode:=ActOpcode;
            condition:=ActCondition;
            oppostfix:=actoppostfix;
            opIsPrefixed:=actIsPrefixed;
          end;

        { We are reading operands, so opcode will be an AS_ID }
        operandnum:=1;
        Consume(AS_OPCODE);
        { Zero operand opcode ?  }
        if actasmtoken in [AS_SEPARATOR,AS_end] then
         begin
           operandnum:=0;
           exit;
         end;
        { Read the operands }
        repeat
          case actasmtoken of
            AS_COMMA: { Operand delimiter }
              Begin
                if operandnum>Max_Operands then
                  Message(asmr_e_too_many_operands)
                else
                  Inc(operandnum);
                Consume(AS_COMMA);
              end;
            AS_SEPARATOR,
            AS_end : { End of asm operands for this opcode  }
              begin
                break;
              end;
          else
            BuildOperand(instr.Operands[operandnum] as TXtensaOperand);
          end; { end case }
        until false;
        instr.Ops:=operandnum;
      end;


    function txtensaattreader.is_asmopcode(const s: string):boolean;

      const
        { sorted by length so longer postfixes will match first }
        postfix2strsorted : array[1..112] of string[13] = (
          'IBREAKENABLE', 'DA.HH.LDDEC', 'DA.HH.LDINC', 'DA.HL.LDDEC', 'DA.HL.LDINC',
          'DA.LH.LDDEC', 'DA.LH.LDINC', 'DA.LL.LDDEC', 'DA.LL.LDINC', 'DD.HH.LDDEC',
          'DD.HH.LDINC', 'DD.HL.LDDEC', 'DD.HL.LDINC', 'DD.LH.LDDEC', 'DD.LH.LDINC',
          'DD.LL.LDDEC', 'DD.LL.LDINC', 'ICOUNTLEVEL', 'WINDOWSTART', 'DEBUGCAUSE',
          'WINDOWBASE', 'CCOMPARE0', 'CCOMPARE1', 'CCOMPARE2', 'INTENABLE',
          'INTERRUPT', 'SCOMPARE1', 'CPENABLE', 'DBREAKA0', 'DBREAKA1',
          'DBREAKC0', 'DBREAKC1', 'EXCCAUSE', 'EXCSAVE1', 'EXCSAVE2',
          'EXCSAVE3', 'EXCSAVE4', 'EXCSAVE5', 'EXCSAVE6', 'EXCSAVE7',
          'EXCVADDR', 'IBREAKA0', 'IBREAKA1', 'INTCLEAR', 'PTEVADDR',
          'ATOMCTL', 'DTLBCFG', 'ITLBCFG', 'LITBASE', 'MEVADDR',
          'VECBASE', 'CCOUNT', 'ICOUNT', 'INTSET', 'LCOUNT',
          'MESAVE', 'AA.HH', 'AA.HL', 'AA.LH', 'AA.LL',
          'ACCHI', 'ACCLO', 'AD.HH', 'AD.HL', 'AD.LH',
          'AD.LL', 'DA.HH', 'DA.HL', 'DA.LH', 'DA.LL',
          'DD.HH', 'DD.HL', 'DD.LH', 'DD.LL', 'MISC0',
          'MISC1', 'MISC2', 'MISC3', 'RASID', 'DEPC',
          'EPC1', 'EPC2', 'EPC3', 'EPC4', 'EPC5',
          'EPC6', 'EPC7', 'EPS2', 'EPS3', 'EPS4',
          'EPS5', 'EPS6', 'EPS7', 'LBEG', 'LEND',
          'MECR', 'MEPC', 'MEPS', 'MESR', 'MMID',
          'PRID', 'DDR', 'SAR', 'BR', 'M0',
          'M1', 'M2', 'M3', 'PS', 'L',
          'N', 'S');

        postfixsorted : array[1..112] of TOpPostfix = (
          PF_IBREAKENABLE, PF_DA_HH_LDDEC, PF_DA_HH_LDINC, PF_DA_HL_LDDEC, PF_DA_HL_LDINC,
          PF_DA_LH_LDDEC, PF_DA_LH_LDINC, PF_DA_LL_LDDEC, PF_DA_LL_LDINC, PF_DD_HH_LDDEC,
          PF_DD_HH_LDINC, PF_DD_HL_LDDEC, PF_DD_HL_LDINC, PF_DD_LH_LDDEC, PF_DD_LH_LDINC,
          PF_DD_LL_LDDEC, PF_DD_LL_LDINC, PF_ICOUNTLEVEL, PF_WINDOWSTART, PF_DEBUGCAUSE,
          PF_WINDOWBASE, PF_CCOMPARE0, PF_CCOMPARE1, PF_CCOMPARE2, PF_INTENABLE,
          PF_INTERRUPT, PF_SCOMPARE1, PF_CPENABLE, PF_DBREAKA0, PF_DBREAKA1,
          PF_DBREAKC0, PF_DBREAKC1, PF_EXCCAUSE, PF_EXCSAVE1, PF_EXCSAVE2,
          PF_EXCSAVE3, PF_EXCSAVE4, PF_EXCSAVE5, PF_EXCSAVE6, PF_EXCSAVE7,
          PF_EXCVADDR, PF_IBREAKA0, PF_IBREAKA1, PF_INTCLEAR, PF_PTEVADDR,
          PF_ATOMCTL, PF_DTLBCFG, PF_ITLBCFG, PF_LITBASE, PF_MEVADDR,
          PF_VECBASE, PF_CCOUNT, PF_ICOUNT, PF_INTSET, PF_LCOUNT,
          PF_MESAVE, PF_AA_HH, PF_AA_HL, PF_AA_LH, PF_AA_LL,
          PF_ACCHI, PF_ACCLO, PF_AD_HH, PF_AD_HL, PF_AD_LH,
          PF_AD_LL, PF_DA_HH, PF_DA_HL, PF_DA_LH, PF_DA_LL,
          PF_DD_HH, PF_DD_HL, PF_DD_LH, PF_DD_LL, PF_MISC0,
          PF_MISC1, PF_MISC2, PF_MISC3, PF_RASID, PF_DEPC,
          PF_EPC1, PF_EPC2, PF_EPC3, PF_EPC4, PF_EPC5,
          PF_EPC6, PF_EPC7, PF_EPS2, PF_EPS3, PF_EPS4,
          PF_EPS5, PF_EPS6, PF_EPS7, PF_LBEG, PF_LEND,
          PF_MECR, PF_MEPC, PF_MEPS, PF_MESR, PF_MMID,
          PF_PRID, PF_DDR, PF_SAR, PF_BR, PF_M0,
          PF_M1, PF_M2, PF_M3, PF_PS, PF_L,
          PF_N, PF_S);

      var
        j, j2 : longint;
        hs,hs2 : string;
        maxlen : longint;
        icond : tasmcond;
      Begin
        { making s a value parameter would break other assembler readers }
        hs:=s;
        is_asmopcode:=false;
        { clear op code }
        actopcode:=A_NONE;
        actcondition:=C_None;
        actoppostfix := PF_None;
        actIsPrefixed := false;
        if hs[1]='_' then
          begin
            actIsPrefixed := true;
            delete(hs, 1, 1);
          end;
        if (hs[1]='B') and not(hs='BREAK') then
          begin
            { Branch condition can be followed by a postfix, e.g. BEQZ.N or BBSI.L }
            j:=pos('.', hs);
            if j < 2 then
              j:=length(hs)
            else
              dec(j);
            hs2:=copy(hs, 2, j-1);
            for icond:=low(tasmcond) to high(tasmcond) do
              begin
                if hs2=uppercond2str[icond] then
                  begin
                    actopcode:=A_B;
                    actasmtoken:=AS_OPCODE;
                    actcondition:=icond;
                    is_asmopcode:=true;
                    delete(hs, 1, j);
                    break;
                  end;
              end;
          end
        else
          begin
            j2:=min(length(hs),7);
            hs2:=hs;
            while (j2>=1) and (actopcode=A_NONE) do
              begin
                hs:=hs2;
                while j2>=1 do
                  begin
                    actopcode:=tasmop(PtrUInt(iasmops.Find(copy(hs,1,j2))));
                    if actopcode<>A_NONE then
                      begin
                        actasmtoken:=AS_OPCODE;
                        { strip op code }
                        delete(hs,1,j2);
                        dec(j2);
                        break;
                      end;
                    dec(j2);
                  end;
              end;
            end;

        if actopcode=A_NONE then
          exit;

        { check for postfix }
        if (length(hs)>0) then
          begin
            for j:=low(postfixsorted) to high(postfixsorted) do
              begin
                if copy(hs,2,length(postfix2strsorted[j]))=postfix2strsorted[j] then
                  begin
                    actoppostfix:=postfixsorted[j];
                    { strip postfix }
                    delete(hs,1,length(postfix2strsorted[j])+1);
                    break;
                  end;
              end;
          end;

        { if we stripped all postfixes, it's a valid opcode }
        is_asmopcode:=length(hs)=0;
      end;


    procedure txtensaattreader.ConvertCalljmp(instr : TXtensaInstruction);
      var
        newopr : toprrec;
      begin
        if instr.Operands[1].opr.typ=OPR_REFERENCE then
          begin
            newopr.typ:=OPR_SYMBOL;
            newopr.symbol:=instr.Operands[1].opr.ref.symbol;
            newopr.symofs:=instr.Operands[1].opr.ref.offset;
            if (instr.Operands[1].opr.ref.base<>NR_NO) or
              (instr.Operands[1].opr.ref.index<>NR_NO) then
              Message(asmr_e_syn_operand);
            instr.Operands[1].opr:=newopr;
          end;
      end;


    procedure txtensaattreader.HandleTargetDirective;
      var
        symname,
        symval  : String;
        val     : tcgint;
        symtyp  : TAsmsymtype;
      begin
        case actasmpattern of
          '.code':
            begin
              consume(AS_TARGET_DIRECTIVE);
              val:=BuildConstExpression(false,false);
              if not(val in [16,32]) then
                Message(asmr_e_invalid_code_value);
              curList.concat(tai_directive.create(asd_code,tostr(val)));
            end;
          else
            inherited HandleTargetDirective;
        end;
      end;


    procedure txtensaattreader.handleopcode;
      var
        instr : TXtensaInstruction;
      begin
        instr:=TXtensaInstruction.Create(TXtensaOperand);
        BuildOpcode(instr);
        if is_calljmp(instr.opcode) then
          ConvertCalljmp(instr);
        instr.ConcatInstruction(curlist);
        instr.Free;
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_xtensa_att_info : tasmmodeinfo =
          (
            id    : asmmode_xtensa_gas;
            idtxt : 'DIVIDED';
            casmreader : txtensaattreader;
          );

  asmmode_xtensa_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : txtensaattreader;
          );

initialization
  RegisterAsmMode(asmmode_xtensa_att_info);
  RegisterAsmMode(asmmode_xtensa_standard_info);
end.

