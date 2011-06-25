{
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Does the parsing for the avr32 GNU AS styled inline assembler.

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
Unit raavr32gas;

{$i fpcdefs.inc}

  Interface

    uses
      raatt,raavr32,
      cpubase;

    type

      { tavr32attreader }

      tavr32attreader = class(tattreader)
        actoppostfix : TOpPostfix;
        function is_asmopcode(const s: string):boolean;override;
        function is_register(const s:string):boolean;override;
        procedure handleopcode;override;
        procedure BuildReference(oper : tavr32operand);
        procedure BuildRegset(oper : tavr32operand);
        procedure BuildOperand(oper : tavr32operand);
        function TryBuildShifterOp(oper : tavr32operand) : boolean;
        procedure BuildOpCode(instr : tavr32instruction);
        procedure ReadSym(oper : tavr32operand);
        procedure ConvertCalljmp(instr : tavr32instruction);
        function read_selector : tregisterselector;
      end;


  Implementation

    uses
      { helpers }
      cutils,
      { global }
      globtype,globals,verbose,
      systems,
      { aasm }
      cpuinfo,aasmbase,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symbase,symtype,symsym,symtable,
      { parser }
      scanner,
      procinfo,
      itcpugas,
      rabase,rautils,
      cgbase,cgobj
      ;


    function tavr32attreader.is_register(const s:string):boolean;
      type
        treg2str = record
          name : string[2];
          reg : tregister;
        end;

      const
        extraregs : array[0..3] of treg2str = (
          (name: 'FP'; reg : NR_R11),
          (name: 'SP'; reg : NR_R13),
          (name: 'LR'; reg : NR_R14),
          (name: 'PC'; reg : NR_R15));

      var
        i : longint;

      begin
        result:=inherited is_register(s);
        { reg found?
          possible aliases are always 2 char
        }
        if result or (length(s)<>2) then
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


    procedure tavr32attreader.ReadSym(oper : tavr32operand);
      var
         tempstr, mangledname : string;
         typesize,l,k : longint;
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


    Procedure tavr32attreader.BuildReference(oper : tavr32operand);

      procedure do_error;
        begin
          Message(asmr_e_invalid_reference_syntax);
          RecoverConsume(false);
        end;


      procedure test_end(require_rbracket : boolean);
        begin
          if require_rbracket then begin
            if not(actasmtoken=AS_RBRACKET) then 
              begin 
                do_error; 
                exit; 
              end
            else 
              Consume(AS_RBRACKET);
          end;
          if not(actasmtoken in [AS_COMMA,AS_SEPARATOR,AS_end]) then
            do_error
          else 
            begin
{$IFDEF debugasmreader}
              writeln('TEST_end_FINAL_OK. Created the following ref:');
              writeln('oper.opr.ref.shiftimm=',oper.opr.ref.shiftimm);
              writeln('oper.opr.ref.shiftmode=',ord(oper.opr.ref.shiftmode));
              writeln('oper.opr.ref.index=',ord(oper.opr.ref.index));
              writeln('oper.opr.ref.base=',ord(oper.opr.ref.base));
              writeln('oper.opr.ref.signindex=',ord(oper.opr.ref.signindex));
              writeln('oper.opr.ref.addressmode=',ord(oper.opr.ref.addressmode));
              writeln;
{$endIF debugasmreader}
            end;
        end;


      procedure read_index_shift(require_rbracket : boolean);
        begin
          case actasmtoken of
            AS_SHL :
              begin
                Consume(AS_SHL);
                oper.opr.ref.scalefactor := 1 shl BuildConstExpression(false,true);
                if (oper.opr.ref.scalefactor<1) or (oper.opr.ref.scalefactor>8) then
                  do_error;
                test_end(require_rbracket);
              end;
            AS_RBRACKET : 
              if require_rbracket then 
                test_end(require_rbracket)
              else 
                begin 
                  do_error; 
                  exit; 
                end;
            AS_SEPARATOR,AS_END : 
              if not require_rbracket then 
                test_end(false)
               else 
                 do_error; 
            else 
              begin
                do_error; 
                exit;
              end;
          end;
        end;


      procedure read_index(require_rbracket : boolean);
        var 
          recname : string;
          o_int,s_int : aint;
        begin
          case actasmtoken of
            AS_REGISTER : 
              begin
                oper.opr.ref.index:=actasmregister;
                oper.opr.ref.indexselector:=RS_None;
                Consume(AS_REGISTER);
                if actasmtoken=AS_COLON then
                  oper.opr.ref.indexselector:=read_selector;
                read_index_shift(require_rbracket);
                exit;
              end;
            AS_PLUS,AS_MINUS,AS_INTNUM: // constant
              begin
                o_int := BuildConstExpression(false,true);
                if not in_signed_bits(o_int,16) then
                  begin
                    Message(asmr_e_constant_out_of_bounds);
                    RecoverConsume(false);
                    exit;
                  end 
                else 
                  begin
                    inc(oper.opr.ref.offset,o_int);
                    test_end(require_rbracket);
                    exit;
                  end;
              end;
            AS_ID :
              begin
                recname := actasmpattern;
                Consume(AS_ID);
                BuildRecordOffsetSize(recname,o_int,s_int,recname,false);
                if not in_signed_bits(o_int,16) then
                  begin
                    Message(asmr_e_constant_out_of_bounds);
                    RecoverConsume(false);
                    exit;
                  end 
                else 
                  begin
                    inc(oper.opr.ref.offset,o_int);
                    test_end(require_rbracket);
                    exit;
                  end;
              end;
            AS_AT: 
              begin
                do_error;
                exit;
              end;
            AS_DOT : // local label
              begin
                oper.opr.ref.offset := BuildConstExpression(true,false);
                test_end(require_rbracket);
                exit;
              end;
            AS_RBRACKET :
              begin
                if require_rbracket then 
                  begin
                    test_end(require_rbracket);
                    exit;
                  end 
                else 
                  begin
                    do_error; // unexpected rbracket
                    exit;
                  end;
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
        oper.InitRef;
        case actasmtoken of
          AS_MINUSMINUS:
            begin
              oper.opr.ref.addressmode:=AM_PREINDEXED;
              consume(AS_MINUSMINUS);

              oper.opr.ref.base:=actasmregister;
              Consume(AS_REGISTER);
            end;
          AS_REGISTER:
            begin
              oper.opr.ref.base:=actasmregister;
              Consume(AS_REGISTER);

              if actasmtoken=AS_PLUSPLUS then
                begin
                  oper.opr.ref.addressmode:=AM_POSTINDEXED;
                  consume(AS_PLUSPLUS);
                end
              else if actasmtoken=AS_LBRACKET then
                begin
                  consume(AS_LBRACKET);
                  read_index(true);
                end;
            end;
        end;
      end;

    procedure tavr32attreader.BuildRegset(oper: tavr32operand);
      var
        tempreg : tregister;
        ireg : tsuperregister;
        regtype: tregistertype;
        subreg: tsubregister;
        {ofs : longint;}
        registerset : tcpuregisterset;
      begin
        registerset:=[];
        regtype:=R_INVALIDREGISTER;
        subreg:=R_SUBNONE;
        while true do
          begin
            if actasmtoken=AS_REGISTER then
              begin
                include(registerset,getsupreg(actasmregister));
                if regtype<>R_INVALIDREGISTER then
                  begin
                    if (getregtype(actasmregister)<>regtype) or
                       (getsubreg(actasmregister)<>subreg) then
                      Message(asmr_e_mixing_regtypes);
                  end
                else
                  begin
                    regtype:=getregtype(actasmregister);
                    subreg:=getsubreg(actasmregister);
                  end;
                tempreg:=actasmregister;
                consume(AS_REGISTER);
                if actasmtoken=AS_MINUS then
                  begin
                    consume(AS_MINUS);
                    for ireg:=getsupreg(tempreg) to getsupreg(actasmregister) do
                      include(registerset,ireg);
                    consume(AS_REGISTER);
                  end;
              end
            else
              consume(AS_REGISTER);
            if actasmtoken=AS_COMMA then
              consume(AS_COMMA)
            else
              break;
          end;
        oper.opr.typ:=OPR_REGSET;
        oper.opr.regtype:=regtype;
        oper.opr.subreg:=subreg;
        oper.opr.regset:=registerset;
        if (registerset=[]) then
          Message(asmr_e_empty_regset);
      end;


    function tavr32attreader.TryBuildShifterOp(oper : tavr32operand) : boolean;

      procedure handlepara(sm : tshiftmode);
        begin
          consume(actasmtoken);
          fillchar(oper.opr,sizeof(oper.opr),0);
          oper.opr.typ:=OPR_SHIFTEROP;
          oper.opr.shifterop.shiftmode:=sm;
          case actasmtoken of
            AS_INTNUM,
            AS_PLUS,
            AS_MINUS:
              oper.opr.shifterop.shiftimm:=BuildConstExpression(false,false);
            else
              Message(asmr_e_illegal_shifterop_syntax);
          end;
        end;

      begin
        result:=true;
        if (actasmtoken in [AS_SHL,AS_SHR]) then
          begin
            if (actasmtoken=AS_SHL) then
              handlepara(SM_LSL)
            else if (actasmtoken=AS_SHR) then
              handlepara(SM_LSR)
            else
              result:=false;
          end
        else
          result:=false;
      end;


    Procedure tavr32attreader.BuildOperand(oper : tavr32operand);
      var
        expr : string;
        typesize,l : longint;


        procedure AddLabelOperand(hl:tasmlabel);
          begin
            if not(actasmtoken in [AS_PLUS,AS_MINUS,AS_LPAREN]) and
               is_calljmp(actopcode) then
             begin
               oper.opr.typ:=OPR_SYMBOL;
               oper.opr.symbol:=hl;
             end
            else
             begin
               oper.InitRef;
               oper.opr.ref.symbol:=hl;
             end;
          end;


        procedure MaybeRecordOffset;
          var
            mangledname: string;
            hasdot  : boolean;
            l,
            toffset,
            tsize   : longint;
          begin
            if not(actasmtoken in [AS_DOT,AS_PLUS,AS_MINUS]) then
             exit;
            l:=0;
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
                     (not oper.hastype) and
                     (tabstractnormalvarsym(oper.opr.localsym).owner.symtabletype=parasymtable) and
                     (current_procinfo.procdef.proccalloption<>pocall_register) then
                    Message(asmr_e_cannot_access_field_directly_for_parameters);
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
                    oper.opr.symbol:=current_asmdata.RefAsmSymbol(mangledname);
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

      var
        tempreg : tregister;
        hl : tasmlabel;
      Begin
        expr:='';
        case actasmtoken of
          AS_INTNUM,
          AS_MINUS,
          AS_PLUS:
            BuildConstantOperand(oper);

          AS_ID: { A constant expression, or a Variable ref.  }
            Begin
              { Local Label ? }
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
                         internalerror(200309202);
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
              if (actasmtoken = AS_COLON) then
                begin
                  oper.opr.typ:=OPR_REGISTERSELECTOR;
                  oper.opr.topreg:=tempreg;
                  oper.opr.selector:=read_selector;
                end
              else if (actasmtoken in [AS_SHL,AS_SHR,AS_END,AS_SEPARATOR,AS_COMMA]) then
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


{*****************************************************************************
                                tavr32attreader
*****************************************************************************}

    procedure tavr32attreader.BuildOpCode(instr : tavr32instruction);
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
            AS_SHL,AS_SHR:
              begin
                if not(TryBuildShifterOp(instr.Operands[operandnum+1] as tavr32operand)) then
                  Message(asmr_e_illegal_shifterop_syntax);
                Inc(operandnum);
              end;
            AS_COMMA: { Operand delimiter }
              Begin
                if (instr.opcode in [A_ANDL,A_ANDH]) and
                   (operandnum=2) then
                  begin
                    consume(AS_COMMA);
                    if (actasmtoken=AS_ID) and
                       (actasmpattern='COH') then
                      begin
                        consume(AS_ID);
                        (instr.operands[operandnum+1] as Tavr32Operand).opr.typ:=OPR_COH;
                        Inc(operandnum);
                      end
                    else
                      message(asmr_e_invalid_operand_type);
                  end
                else
                  if (instr.opcode in [A_LD,A_LDDPC,A_LDDSP,A_LDINS,A_LDSWP]) and
                     (operandnum=1) then
                  begin
                    Consume(AS_COMMA);
                    BuildReference(instr.operands[operandnum+1] as Tavr32Operand);
                    Inc(operandnum);
                  end
                else
                  if (instr.opcode in [A_LDM,A_LDMTS,A_STM,A_STMTS]) and
                     (operandnum=1) then
                  begin
                    Consume(AS_COMMA);
                    BuildRegset(instr.operands[operandnum+1] as Tavr32Operand);
                    Inc(operandnum);
                  end
                else
                  if ((instr.opcode=A_MOV) and (operandnum=2)) or
                     ((operandnum=3) and not(instr.opcode in [A_ADD,A_AND])) then
                  begin
                    Consume(AS_COMMA);
                    if not(TryBuildShifterOp(instr.Operands[operandnum+1] as tavr32operand)) then
                      Message(asmr_e_illegal_shifterop_syntax);
                    Inc(operandnum);
                  end
                else
                  begin
                    if operandnum>Max_Operands then
                      Message(asmr_e_too_many_operands)
                    else
                      Inc(operandnum);
                    Consume(AS_COMMA);
                  end;
              end;
            AS_SEPARATOR,
            AS_end : { End of asm operands for this opcode  }
              begin
                break;
              end;
          else
            if (instr.opcode in [A_ST,A_STDSP,A_STHH,A_STSWP,A_LDM,A_LDMTS,A_STM,A_STMTS]) and
               (operandnum=1) then
              BuildReference(instr.operands[operandnum] as Tavr32Operand)
            else if (instr.opcode in [A_PUSHM,A_POPM]) and
                    (operandnum=1) then
              BuildRegset(instr.operands[operandnum] as Tavr32Operand)
            else
              BuildOperand(instr.Operands[operandnum] as tavr32operand);
          end; { end case }
        until false;
        instr.Ops:=operandnum;
      end;


    function tavr32attreader.is_asmopcode(const s: string):boolean;

      const
        { sorted by length so longer postfixes will match first }
        postfix2strsorted : array[1..11] of string[3] = (
          'UB','UH',
          'SB','SH',
          'W','H','D',
          'T','U','L','B');

        postfixsorted : array[1..11] of TOpPostfix = (
          PF_UB,PF_UH,
          PF_SB,PF_SH,
          PF_W,PF_H,PF_D,
          PF_T,PF_U,PF_L,PF_B);

      var
        j  : longint;
        hs : string;
        maxlen : longint;
        icond : tasmcond;
      Begin
        { making s a value parameter would break other assembler readers }
        hs:=s;
        is_asmopcode:=false;

        { clear op code }
        actopcode:=A_None;

        actcondition:=C_None;

        { first, handle B else BLS is read wrong }
        if ((hs[1]='B') and (hs[2]='R') and (length(hs)=4)) then
          begin
            for icond:=low(tasmcond) to high(tasmcond) do
              begin
                if copy(hs,2,3)=uppercond2str[icond] then
                  begin
                    actopcode:=A_BR;
                    actasmtoken:=AS_OPCODE;
                    actcondition:=icond;
                    is_asmopcode:=true;
                    exit;
                  end;
              end;
          end;
        maxlen:=max(length(hs),5);
        actopcode:=A_NONE;
        for j:=maxlen downto 1 do
          begin
            actopcode:=tasmop(PtrUInt(iasmops.Find(copy(hs,1,j))));
            if actopcode<>A_NONE then
              begin
                actasmtoken:=AS_OPCODE;
                { strip op code }
                delete(hs,1,j);
                break;
              end;
          end;
        if actopcode=A_NONE then
          exit;
			 
        { search for condition, conditions are always 2 chars }
        if length(hs)>1 then
          begin
            for icond:=low(tasmcond) to high(tasmcond) do
              begin
                if copy(hs,1,2)=uppercond2str[icond] then
                  begin
                    actcondition:=icond;
                    { strip condition }
                    delete(hs,1,2);
                    break;
                  end;
              end;
          end;
        { check for postfix }
        if (length(hs)>1) and
           (hs[1] = '.') then
          begin
            for j:=low(postfixsorted) to high(postfixsorted) do
              begin
                if copy(hs,2,length(postfix2strsorted[j]))=postfix2strsorted[j] then
                  begin
                    actoppostfix:=postfixsorted[j];
                    { strip postfix }
                    delete(hs,1,1+length(postfix2strsorted[j]));
                    break;
                  end;
              end;
          end;
        { if we stripped all postfixes, it's a valid opcode }
        is_asmopcode:=length(hs)=0;
      end;


    procedure tavr32attreader.ConvertCalljmp(instr : tavr32instruction);
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

    function tavr32attreader.read_selector: tregisterselector;
      begin
        consume(AS_COLON);
        if not ((actasmtoken=AS_ID) and
                (length(actasmpattern)=1)) then
          message(asmr_e_invalid_operand_type);

        case actasmpattern[1] of
          'B':
            read_selector:=RS_B;
          'L':
            read_selector:=RS_L;
          'U':
            read_selector:=RS_U;
          'T':
            read_selector:=RS_T;
          else
            message(asmr_e_invalid_operand_type);
        end;
        consume(AS_ID);
      end;


    procedure tavr32attreader.handleopcode;
      var
        instr : tavr32instruction;
      begin
        instr:=Tavr32Instruction.Create(Tavr32Operand);
        BuildOpcode(instr);
        if is_calljmp(instr.opcode) then
          ConvertCalljmp(instr);
        {
        instr.AddReferenceSizes;
        instr.SetInstructionOpsize;
        instr.CheckOperandSizes;
        }
        instr.ConcatInstruction(curlist);
        instr.Free;
        actoppostfix:=PF_None;
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_avr32_att_info : tasmmodeinfo =
          (
            id    : asmmode_avr32_gas;
            idtxt : 'GAS';
            casmreader : tavr32attreader;
          );

  asmmode_avr32_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : tavr32attreader;
          );

initialization
  RegisterAsmMode(asmmode_avr32_att_info);
  RegisterAsmMode(asmmode_avr32_standard_info);
end.
