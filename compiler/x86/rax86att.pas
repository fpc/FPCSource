{
    $Id$
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Does the parsing for the x86 GNU AS styled inline assembler.

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
Unit rax86att;

{$i fpcdefs.inc}

Interface

  uses
    cpubase,
    raatt,rax86;

  type
    tx86attreader = class(tattreader)
      ActOpsize : topsize;
      function is_asmopcode(const s: string):boolean;override;
      procedure handleopcode;override;
      procedure BuildReference(oper : tx86operand);
      procedure BuildOperand(oper : tx86operand);
      procedure BuildOpCode(instr : tx86instruction);
      procedure handlepercent;override;
    end;


Implementation

    uses
      { helpers }
      cutils,
      { global }
      globtype,globals,verbose,
      systems,
      { aasm }
      cpuinfo,aasmbase,aasmtai,aasmcpu,
      { symtable }
      symconst,symbase,symtype,symsym,symtable,
      { parser }
      scanner,
      procinfo,
      itcpugas,
      rabase,rautils,
      cgbase,cgobj
      ;

    procedure tx86attreader.handlepercent;
      var
        len : longint;
      begin
        len:=1;
        actasmpattern[len]:='%';
        c:=current_scanner.asmgetchar;
        { to be a register there must be a letter and not a number }
        if c in ['0'..'9'] then
         begin
           actasmtoken:=AS_MOD;
         end
        else
         begin
           while c in ['a'..'z','A'..'Z','0'..'9'] do
            Begin
              inc(len);
              actasmpattern[len]:=c;
              c:=current_scanner.asmgetchar;
            end;
           actasmpattern[0]:=chr(len);
           uppervar(actasmpattern);
           if (actasmpattern = '%ST') and (c='(') then
            Begin
              actasmpattern:=actasmpattern+c;
              c:=current_scanner.asmgetchar;
              if c in ['0'..'9'] then
               actasmpattern:=actasmpattern + c
              else
               Message(asmr_e_invalid_fpu_register);
              c:=current_scanner.asmgetchar;
              if c <> ')' then
               Message(asmr_e_invalid_fpu_register)
              else
               Begin
                 actasmpattern:=actasmpattern + c;
                 c:=current_scanner.asmgetchar; { let us point to next character. }
               end;
            end;
           if is_register(actasmpattern) then
            exit;
           Message(asmr_e_invalid_register);
           actasmtoken:=raatt.AS_NONE;
         end;
      end;


    Procedure tx86attreader.BuildReference(oper : tx86operand);

      procedure Consume_RParen;
        begin
          if actasmtoken <> AS_RPAREN then
           Begin
             Message(asmr_e_invalid_reference_syntax);
             RecoverConsume(true);
           end
          else
           begin
             Consume(AS_RPAREN);
             if not (actasmtoken in [AS_COMMA,AS_SEPARATOR,AS_END]) then
              Begin
                Message(asmr_e_invalid_reference_syntax);
                RecoverConsume(true);
              end;
           end;
        end;


      procedure Consume_Scale;
        var
          l : longint;
        begin
          { we have to process the scaling }
          l:=BuildConstExpression(false,true);
          if ((l = 2) or (l = 4) or (l = 8) or (l = 1)) then
           oper.opr.ref.scalefactor:=l
          else
           Begin
             Message(asmr_e_wrong_scale_factor);
             oper.opr.ref.scalefactor:=0;
           end;
        end;


      begin
        oper.InitRef;
        Consume(AS_LPAREN);
        Case actasmtoken of
          AS_INTNUM,
          AS_MINUS,
          AS_PLUS: { absolute offset, such as fs:(0x046c) }
            Begin
              { offset(offset) is invalid }
              If oper.opr.Ref.Offset <> 0 Then
               Begin
                 Message(asmr_e_invalid_reference_syntax);
                 RecoverConsume(true);
               End
              Else
               Begin
                 oper.opr.Ref.Offset:=BuildConstExpression(false,true);
                 Consume_RParen;
               end;
              exit;
            End;
          AS_REGISTER: { (reg ...  }
            Begin
              { Check if there is already a base (mostly ebp,esp) than this is
                not allowed, because it will give crashing code }
              if ((oper.opr.typ=OPR_REFERENCE) and (oper.opr.ref.base<>NR_NO)) or
                 ((oper.opr.typ=OPR_LOCAL) and (oper.opr.localsym.localloc.loc<>LOC_REGISTER)) then
                message(asmr_e_cannot_index_relative_var);
              oper.opr.ref.base:=actasmregister;
              Consume(AS_REGISTER);
              { can either be a register or a right parenthesis }
              { (reg)        }
              if actasmtoken=AS_RPAREN then
               Begin
                 Consume_RParen;
                 exit;
               end;
              { (reg,reg ..  }
              Consume(AS_COMMA);
              if actasmtoken=AS_REGISTER then
               Begin
                 oper.opr.ref.index:=actasmregister;
                 Consume(AS_REGISTER);
                 { check for scaling ... }
                 case actasmtoken of
                   AS_RPAREN:
                     Begin
                       Consume_RParen;
                       exit;
                     end;
                   AS_COMMA:
                     Begin
                       Consume(AS_COMMA);
                       Consume_Scale;
                       Consume_RParen;
                     end;
                 else
                   Begin
                     Message(asmr_e_invalid_reference_syntax);
                     RecoverConsume(false);
                   end;
                 end; { end case }
               end
              else
               Begin
                 Message(asmr_e_invalid_reference_syntax);
                 RecoverConsume(false);
               end;
            end; {end case }
          AS_COMMA: { (, ...  can either be scaling, or index }
            Begin
              Consume(AS_COMMA);
              { Index }
              if (actasmtoken=AS_REGISTER) then
               Begin
                 oper.opr.ref.index:=actasmregister;
                 Consume(AS_REGISTER);
                 { check for scaling ... }
                 case actasmtoken of
                   AS_RPAREN:
                     Begin
                       Consume_RParen;
                       exit;
                     end;
                   AS_COMMA:
                     Begin
                       Consume(AS_COMMA);
                       Consume_Scale;
                       Consume_RParen;
                     end;
                 else
                   Begin
                     Message(asmr_e_invalid_reference_syntax);
                     RecoverConsume(false);
                   end;
                 end; {end case }
               end
              { Scaling }
              else
               Begin
                 Consume_Scale;
                 Consume_RParen;
                 exit;
               end;
            end;
        else
          Begin
            Message(asmr_e_invalid_reference_syntax);
            RecoverConsume(false);
          end;
        end;
      end;


    Procedure tx86attreader.BuildOperand(oper : tx86operand);
      var
        tempstr,
        expr : string;
        typesize,
        l,k : longint;


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
                   BuildRecordOffsetSize(expr,toffset,tsize);
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
                     (tvarsym(oper.opr.localsym).owner.symtabletype=parasymtable) and
                     (current_procinfo.procdef.proccalloption<>pocall_register) then
                    Message(asmr_e_cannot_access_field_directly_for_parameters);
                  inc(oper.opr.localsymofs,l)
                end;
              OPR_CONSTANT :
                inc(oper.opr.val,l);
              OPR_REFERENCE :
                inc(oper.opr.ref.offset,l);
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
                     BuildRecordOffsetSize(tempstr,l,k);
                     inc(oper.opr.ref.offset,l);
                   end;
                  case actasmtoken of
                    AS_END,
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
        hl      : tasmlabel;
      Begin
        expr:='';
        case actasmtoken of
          AS_LPAREN: { Memory reference or constant expression }
            Begin
              oper.InitRef;
              BuildReference(oper);
            end;

          AS_DOLLAR: { Constant expression  }
            Begin
              Consume(AS_DOLLAR);
              BuildConstantOperand(oper);
            end;

          AS_INTNUM,
          AS_MINUS,
          AS_PLUS:
            Begin
              { Constant memory offset }
              { This must absolutely be followed by (  }
              oper.InitRef;
              oper.opr.ref.offset:=BuildConstExpression(True,False);
              if actasmtoken<>AS_LPAREN then
                Message(asmr_e_invalid_reference_syntax)
              else
                BuildReference(oper);
            end;

          AS_STAR: { Call from memory address }
            Begin
              Consume(AS_STAR);
              if actasmtoken=AS_REGISTER then
               begin
                 oper.opr.typ:=OPR_REGISTER;
                 oper.opr.reg:=actasmregister;
                 oper.SetSize(tcgsize2size[cg.reg_cgsize(actasmregister)],true);
                 Consume(AS_REGISTER);
               end
              else
               begin
                 oper.InitRef;
                 if not MaybeBuildReference then
                  Message(asmr_e_syn_operand);
               end;
              { this is only allowed for call's and jmp's }
              if not is_calljmp(actopcode) then
               Message(asmr_e_syn_operand);
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
                       if oper.SetupVar(expr,false) then
                        begin
                        end
                       else
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
                            { check for direct symbolic names   }
                            { only if compiling the system unit }
                            if (cs_compilesystem in aktmoduleswitches) then
                             begin
                               if not oper.SetupDirectVar(expr) then
                                Begin
                                  { not found, finally ... add it anyways ... }
                                  Message1(asmr_w_id_supposed_external,expr);
                                  oper.InitRef;
                                  oper.opr.ref.symbol:=objectlibrary.newasmsymbol(expr);
                                end;
                             end
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

          AS_REGISTER: { Register, a variable reference or a constant reference  }
            Begin
              { save the type of register used. }
              tempreg:=actasmregister;
              Consume(AS_REGISTER);
              if actasmtoken = AS_COLON then
               Begin
                 Consume(AS_COLON);
                 oper.InitRef;
                 oper.opr.ref.segment:=tempreg;
                 { This must absolutely be followed by a reference }
                 if not MaybeBuildReference then
                  Begin
                    Message(asmr_e_invalid_seg_override);
                    Consume(actasmtoken);
                  end;
               end
              { Simple register  }
              else if (actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
                Begin
                  if not (oper.opr.typ in [OPR_NONE,OPR_REGISTER]) then
                    Message(asmr_e_invalid_operand_type);
                  oper.opr.typ:=OPR_REGISTER;
                  oper.opr.reg:=tempreg;
                  oper.SetSize(tcgsize2size[cg.reg_cgsize(oper.opr.reg)],true);
                end
              else
                Message(asmr_e_syn_operand);
            end;
          AS_END,
          AS_SEPARATOR,
          AS_COMMA: ;
        else
          Begin
            Message(asmr_e_syn_operand);
            Consume(actasmtoken);
          end;
        end; { end case }
      end;


    procedure tx86attreader.BuildOpCode(instr : tx86instruction);
      var
        operandnum : longint;
        PrefixOp,OverrideOp: tasmop;
      Begin
        PrefixOp:=A_None;
        OverrideOp:=A_None;
        { prefix seg opcode / prefix opcode }
        repeat
          if is_prefix(actopcode) then
            begin
              PrefixOp:=ActOpcode;
              with instr do
                begin
                  opcode:=ActOpcode;
                  condition:=ActCondition;
                  opsize:=ActOpsize;
                  ConcatInstruction(curlist);
                end;
              Consume(AS_OPCODE);
            end
          else
            if is_override(actopcode) then
              begin
                OverrideOp:=ActOpcode;
                with instr do
                  begin
                    opcode:=ActOpcode;
                    condition:=ActCondition;
                    opsize:=ActOpsize;
                    ConcatInstruction(curlist);
                  end;
                Consume(AS_OPCODE);
              end
            else
              break;
          { allow for newline as in gas styled syntax }
          while actasmtoken=AS_SEPARATOR do
            Consume(AS_SEPARATOR);
        until (actasmtoken<>AS_OPCODE);
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
            opsize:=ActOpsize;
          end;

        { Valid combination of prefix/override and instruction ?  }

        if (prefixop<>A_NONE) and (NOT CheckPrefix(PrefixOp,actopcode)) then
           Message1(asmr_e_invalid_prefix_and_opcode,actasmpattern);

        if (overrideop<>A_NONE) and (NOT CheckOverride(OverrideOp,ActOpcode)) then
          Message1(asmr_e_invalid_override_and_opcode,actasmpattern);
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
                if operandnum > Max_Operands then
                  Message(asmr_e_too_many_operands)
                else
                  Inc(operandnum);
                Consume(AS_COMMA);
              end;
            AS_SEPARATOR,
            AS_END : { End of asm operands for this opcode  }
              begin
                break;
              end;
          else
            BuildOperand(instr.Operands[operandnum] as tx86operand);
          end; { end case }
        until false;
        instr.Ops:=operandnum;
      end;


    function tx86attreader.is_asmopcode(const s: string):boolean;
      const
        { We need first to check the long prefixes, else we get probs
          with things like movsbl }
        att_sizesuffixstr : array[0..9] of string[2] = (
          '','BW','BL','WL','B','W','L','S','Q','T'
        );
        att_sizesuffix : array[0..9] of topsize = (
          S_NO,S_BW,S_BL,S_WL,S_B,S_W,S_L,S_FS,S_IQ,S_FX
        );
        att_sizefpusuffix : array[0..9] of topsize = (
          S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_FL,S_FS,S_IQ,S_FX
        );
        att_sizefpuintsuffix : array[0..9] of topsize = (
          S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_IL,S_IS,S_IQ,S_NO
        );
      var
        str2opentry: tstr2opentry;
        cond : string[4];
        cnd  : tasmcond;
        len,
        j,
        sufidx : longint;
      Begin
        is_asmopcode:=FALSE;

        actopcode:=A_None;
        actcondition:=C_None;
        actopsize:=S_NO;

        { search for all possible suffixes }
        for sufidx:=low(att_sizesuffixstr) to high(att_sizesuffixstr) do
         begin
           len:=length(s)-length(att_sizesuffixstr[sufidx]);
           if copy(s,len+1,length(att_sizesuffixstr[sufidx]))=att_sizesuffixstr[sufidx] then
            begin
              { here we search the entire table... }
              str2opentry:=nil;
              if {(length(s)>0) and} (len>0) then
                str2opentry:=tstr2opentry(iasmops.search(copy(s,1,len)));
              if assigned(str2opentry) then
                begin
                  actopcode:=str2opentry.op;
                  if gas_needsuffix[actopcode]=attsufFPU then
                   actopsize:=att_sizefpusuffix[sufidx]
                  else if gas_needsuffix[actopcode]=attsufFPUint then
                   actopsize:=att_sizefpuintsuffix[sufidx]
                  else
                   actopsize:=att_sizesuffix[sufidx];
                  actasmtoken:=AS_OPCODE;
                  is_asmopcode:=TRUE;
                  exit;
                end;
              { not found, check condition opcodes }
              j:=0;
              while (j<CondAsmOps) do
               begin
                 if Copy(s,1,Length(CondAsmOpStr[j]))=CondAsmOpStr[j] then
                  begin
                    cond:=Copy(s,Length(CondAsmOpStr[j])+1,len-Length(CondAsmOpStr[j]));
                    if cond<>'' then
                     begin
                       for cnd:=low(TasmCond) to high(TasmCond) do
                        if Cond=Upper(cond2str[cnd]) then
                         begin
                           actopcode:=CondASmOp[j];
                           if gas_needsuffix[actopcode]=attsufFPU then
                            actopsize:=att_sizefpusuffix[sufidx]
                           else if gas_needsuffix[actopcode]=attsufFPUint then
                            actopsize:=att_sizefpuintsuffix[sufidx]
                           else
                            actopsize:=att_sizesuffix[sufidx];
                           actcondition:=cnd;
                           actasmtoken:=AS_OPCODE;
                           is_asmopcode:=TRUE;
                           exit;
                         end;
                     end;
                  end;
                 inc(j);
               end;
           end;
         end;
      end;


    procedure tx86attreader.handleopcode;
      var
        instr : Tx86Instruction;
      begin
        instr:=Tx86Instruction.Create(Tx86Operand);
        instr.OpOrder:=op_att;
        BuildOpcode(instr);
        instr.AddReferenceSizes;
        instr.SetInstructionOpsize;
        instr.CheckOperandSizes;
        instr.ConcatInstruction(curlist);
        instr.Free;
      end;


end.
{
  $Log$
  Revision 1.2  2004-02-09 19:23:48  peter
    * reg_2_opsize replaced with reg_cgsize

  Revision 1.1  2004/01/14 23:39:05  florian
    * another bunch of x86-64 fixes mainly calling convention and
      assembler reader related

  Revision 1.58  2003/11/12 16:05:39  florian
    * assembler readers OOPed
    + typed currency constants
    + typed 128 bit float constants if the CPU supports it

  Revision 1.57  2003/11/10 19:08:32  peter
    * line numbering is now only done when #10, #10#13 is really parsed
      instead of when it is the next character

  Revision 1.56  2003/10/29 16:47:18  peter
    * fix field offset in reference

  Revision 1.55  2003/10/26 13:37:22  florian
    * fixed web bug 2128

  Revision 1.54  2003/10/24 17:39:03  peter
    * more intel parser updates

  Revision 1.53  2003/10/23 17:19:44  peter
    * typecasting fixes
    * reference building more delphi compatible

  Revision 1.52  2003/10/20 19:29:35  peter
    * fix check for register subscription of reference parameter

  Revision 1.51  2003/10/16 21:29:24  peter
    + __HIGH() to retrieve high value

  Revision 1.50  2003/10/07 18:21:18  peter
    * fix crash
    * allow parameter subscription for register parameters

  Revision 1.49  2003/10/01 20:34:49  peter
    * procinfo unit contains tprocinfo
    * cginfo renamed to cgbase
    * moved cgmessage to verbose
    * fixed ppc and sparc compiles

  Revision 1.48  2003/09/23 20:37:53  peter
    * fix global var+offset

  Revision 1.47  2003/09/23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.46  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.45.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.45.2.1  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.45  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.44  2003/05/22 21:32:29  peter
    * removed some unit dependencies

  Revision 1.43  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.42  2003/04/25 12:04:31  florian
    * merged agx64att and ag386att to x86/agx86att

  Revision 1.41  2003/04/21 20:05:10  peter
    * removed some ie checks

  Revision 1.40  2003/03/18 18:15:53  peter
    * changed reg2opsize to function

  Revision 1.39  2003/02/20 15:52:58  pierre
   * fix a range check error

  Revision 1.38  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.37  2003/02/03 22:47:14  daniel
    - Removed reg_2_opsize array

  Revision 1.36  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.35  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.34  2002/12/01 22:08:34  carl
    * some small cleanup (remove some specific operators which are not supported)

  Revision 1.33  2002/11/30 23:16:39  carl
    - removed unused message

  Revision 1.32  2002/11/15 01:58:58  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.31  2002/09/03 16:26:28  daniel
    * Make Tprocdef.defs protected

  Revision 1.30  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.29  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.28  2002/08/11 14:32:31  peter
    * renamed current_library to objectlibrary

  Revision 1.27  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.26  2002/07/26 21:15:44  florian
    * rewrote the system handling

  Revision 1.25  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.23  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.21  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.20  2002/04/14 17:01:52  carl
  + att_reg2str -> gas_reg2str

  Revision 1.19  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.18  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.17  2002/03/28 20:48:25  carl
  - remove go32v1 support

  Revision 1.16  2002/01/24 18:25:53  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
