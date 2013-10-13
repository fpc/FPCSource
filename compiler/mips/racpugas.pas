{
    Copyright (c) 1998-2002 by Mazen NEIFER

    Does the parsing for the i386 GNU AS styled inline assembler.

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
    rautils,
    raatt;

  type
    tMipsReader = class(tattreader)
      function is_asmopcode(const s: string):boolean;override;
      procedure BuildOperand(oper : TOperand);
      procedure BuildOpCode(instr : TInstruction);
      procedure ReadSym(oper : TOperand);
      procedure ConvertCalljmp(instr : TInstruction);
      procedure handlepercent;override;
      procedure handledollar;override;
      procedure handleopcode;override;
    end;


  Implementation

    uses
      { helpers }
      cutils,
      { global }
      globtype,verbose,
      systems,
      { aasm }
      cpubase,aasmbase,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symsym,
      { parser }
      scanner,
      procinfo,
      rabase,
      rgbase,
      itcpugas,
      cgbase,cgobj
      ;

    procedure TMipsReader.ReadSym(oper : TOperand);
      var
         tempstr, mangledname : string;
         typesize,l,k : aint;
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


    procedure TMipsReader.handledollar;
      begin
        Inherited handledollar;
        if (c in ['0'..'9','a'..'z']) then
          begin
            Consume(AS_DOLLAR);
            if (actasmtoken=AS_INTNUM) or (actasmtoken=AS_ID) then
              begin
                { Try to convert to std register }
                if actasmtoken=AS_INTNUM then
                  actasmregister:=gas_regnum_search('$'+actasmpattern)
                else
                  begin
                    { AS_ID is uppercased by default but register names
                      are lowercase }
                    actasmpattern:=lower(actasmpattern);
                    actasmregister:=gas_regnum_search(actasmpattern);
                    if actasmregister=NR_NO then
                      actasmregister:=std_regnum_search(actasmpattern);
                  end;
                if actasmregister<>NR_NO then
                  begin
                    // Consume(actasmtoken);
                    actasmtoken:=AS_REGISTER;
                  end;
              end;
          end;
      end;


    procedure TMipsReader.handlepercent;
      var
        len : longint;
      begin
        len:=1;
        actasmpattern[len]:='%';
        c:=current_scanner.asmgetchar;
        { to be a register there must be a letter and not a number }
        while c in ['a'..'z','A'..'Z','0'..'9'] do
          Begin
            inc(len);
            actasmpattern[len]:=c;
            c:=current_scanner.asmgetchar;
          end;
         actasmpattern[0]:=chr(len);
         uppervar(actasmpattern);
         if (actasmpattern='%HI') then
           actasmtoken:=AS_HI
         else if (actasmpattern='%LO')then
           actasmtoken:=AS_LO
         else
           Message(asmr_e_invalid_reference_syntax);
      end;


    Procedure TMipsReader.BuildOperand(oper : TOperand);
      var
        expr : string;
        typesize,l : aint;

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
            tsize   : aint;
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
                if (mangledname<>'') then
                  begin
                    if (oper.opr.val<>0) then
                      Message(asmr_e_wrong_sym_type);
                    oper.opr.typ:=OPR_SYMBOL;
                    oper.opr.symbol:=current_asmdata.RefAsmSymbol(mangledname);
                  end
                else
                  inc(oper.opr.val,l);
              OPR_REFERENCE :
                inc(oper.opr.ref.offset,l);
              OPR_SYMBOL:
                Message(asmr_e_invalid_symbol_ref);
              else
                internalerror(200309221);
            end;
          end;

      var
        tempreg : tregister;
        tempstr : string;
        tempsymtyp : TAsmSymType;
        hl : tasmlabel;
        gotplus,
        negative : boolean;
      Begin
        expr:='';
        gotplus:=true;
        negative:=false;
        repeat
          case actasmtoken of
            AS_MINUS :
              begin
                consume(AS_MINUS);
                negative:=true;
                gotplus:=true;
              end;

            AS_PLUS :
              begin
                consume(AS_PLUS);
                negative:=false;
                gotplus:=true;
              end;

            AS_INTNUM,
            AS_MOD:
              Begin
                if not gotplus then
                  Message(asmr_e_invalid_reference_syntax);
                l:=BuildConstExpression(True,False);
                if negative then
                  l:=-l;
                { Constant memory offset }
                oper.InitRef;
                oper.opr.ref.refaddr:=addr_full;
                oper.opr.ref.offset:=l;
                GotPlus:=(prevasmtoken=AS_PLUS) or
                         (prevasmtoken=AS_MINUS);
                if GotPlus then
                  negative:=(prevasmtoken=AS_MINUS);
              end;

            AS_LPAREN :
              begin
                consume(AS_LPAREN);
                oper.InitRef;
                if actasmtoken=AS_REGISTER then
                  oper.opr.ref.base:=actasmregister;
                consume(AS_REGISTER);
                consume(AS_RPAREN);
                gotplus:=false;
              end;

            AS_HI,
            AS_LO:
              begin
                { Low or High part of a constant (or constant
                  memory location) }
                oper.InitRef;
                if actasmtoken=AS_LO then
                  oper.opr.ref.refaddr:=addr_low
                else
                  oper.opr.ref.refaddr:=addr_high;
                Consume(actasmtoken);
                Consume(AS_LPAREN);
                BuildConstSymbolExpression(false, true,false,l,tempstr,tempsymtyp);
                if not assigned(oper.opr.ref.symbol) then
                  oper.opr.ref.symbol:=current_asmdata.RefAsmSymbol(tempstr)
                else
                  Message(asmr_e_cant_have_multiple_relocatable_symbols);
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
                Consume(AS_RPAREN);
                gotplus:=false;
              end;

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
                         if not oper.SetupVar(expr,false) then
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
                gotplus:=false;
              end;


            AS_REGISTER: { Register, a variable reference or a constant reference  }
              Begin
                { save the type of register used. }
                tempreg:=actasmregister;
                Consume(AS_REGISTER);
                  begin
                    if (oper.opr.typ<>OPR_NONE) then
                      Message(asmr_e_invalid_operand_type);
                    oper.opr.typ:=OPR_REGISTER;
                    oper.opr.reg:=tempreg;
                  end;
                gotplus:=false;
              end;

            AS_END,
            AS_SEPARATOR,
            AS_COMMA:
              break;
          else
            Begin
              Message(asmr_e_syn_operand);
              Consume(actasmtoken);
            end;
          end; { end case }
        until false;
      end;


{*****************************************************************************
                                TMipsReader
*****************************************************************************}

    procedure TMipsReader.BuildOpCode(instr : TInstruction);
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
          end;

        { We are reading operands, so opcode will be an AS_ID }
        operandnum:=1;
        Consume(AS_OPCODE);
        { Zero operand opcode ?  }
        if actasmtoken in [AS_SEPARATOR,AS_END] then
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
                  begin
                    { condition operands doesn't set the operand but write to the
                      condition field of the instruction
                    }
                    if instr.Operands[operandnum].opr.typ<>OPR_NONE then
                      Inc(operandnum);
                  end;
                Consume(AS_COMMA);
              end;
            AS_SEPARATOR,
            AS_END : { End of asm operands for this opcode  }
              begin
                break;
              end;
          else
            BuildOperand(instr.Operands[operandnum] as TOperand);
          end; { end case }
        until false;
        if (operandnum=1) and (instr.Operands[operandnum].opr.typ=OPR_NONE) then
          dec(operandnum);
        instr.Ops:=operandnum;
      end;


    function TMipsReader.is_asmopcode(const s: string):boolean;
      var
        cond:TAsmCond;
      Begin
        { making s a value parameter would break other assembler readers }
        is_asmopcode:=false;

        { clear op code }
        actopcode:=A_None;
        { clear condition }
        fillchar(actcondition,sizeof(actcondition),0);

         { Search opcodes }
         actopcode:=tasmop(PtrUInt(iasmops.Find(s)));
         if actopcode<>A_NONE then
           begin
             actasmtoken:=AS_OPCODE;
             result:=TRUE;
             exit;
           end;

        { not found, check branch instructions }
        if (Upcase(s[1])='B') then
          begin
            { we can search here without an extra table which is sorted by string length
              because we take the whole remaining string without the leading B }
            actopcode := A_BC;
            for cond:=low(TAsmCond) to high(TAsmCond) do
              if (Upper(copy(s,2,length(s)-1))=Upper(Cond2Str[cond])) then
                begin
                  actasmtoken:=AS_OPCODE;
                  actcondition:=cond;
                  is_asmopcode:=true;
                end;
          end;
      end;


    procedure TMipsReader.ConvertCalljmp(instr : TInstruction);
      var
        newopr : toprrec;
      begin
        if instr.Operands[1].opr.typ=OPR_REFERENCE then
          with newopr do
            begin
              typ:=OPR_SYMBOL;
              symbol:=instr.Operands[1].opr.ref.symbol;
              symofs:=instr.Operands[1].opr.ref.offset;
              if (instr.Operands[1].opr.ref.base<>NR_NO) or
                (instr.Operands[1].opr.ref.index<>NR_NO) or
                (instr.Operands[1].opr.ref.refaddr<>addr_full) then
                Message(asmr_e_syn_operand);
              instr.Operands[1].opr:=newopr;
            end;
      end;


    procedure TMipsReader.handleopcode;
      var
        instr : TInstruction;
      begin
        instr:=TInstruction.Create(TOperand);
        BuildOpcode(instr);
        with instr do
          begin
            condition := actcondition;
            if is_calljmp(opcode) then
              ConvertCalljmp(instr);
            if (opcode in [A_MTC0,A_MFC0]) then
              begin
                if (ops<2) or (operands[2].opr.typ<>OPR_REGISTER) then
                  message(asmr_e_syn_operand);
                operands[2].opr.reg:=newreg(R_SPECIALREGISTER,getsupreg(operands[2].opr.reg),R_SUBNONE);
              end;
            ConcatInstruction(curlist);
            Free;
          end;
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_mips_att_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'GAS';
            casmreader : TMipsReader;
          );

initialization
  RegisterAsmMode(asmmode_mips_att_info);
end.
