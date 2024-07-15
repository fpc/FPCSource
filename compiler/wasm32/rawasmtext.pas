{
    Copyright (c) 1998-2008 by Carl Eric Codere and Peter Vreman
    Copyright (c) 2024 by Nikolay Nikolov

    Does the parsing for the WebAssembly styled inline assembler.

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
Unit rawasmtext;

{$i fpcdefs.inc}

  Interface

    uses
      cclasses,
      globtype,
      rasm,rawasm,
      aasmbase,cpubase;

    type
      tasmtoken = (
        AS_NONE,AS_LPAREN,AS_RPAREN,AS_ID,AS_END,AS_OPCODE,AS_INTNUM,
        AS_REALNUM,AS_STRING,AS_PARAM,AS_RESULT,AS_THEN,AS_ELSE,AS_TYPE,AS_VALTYPE
      );
      tasmkeyword = string[10];

    const
      token2str : array[tasmtoken] of tasmkeyword=(
        '','(',')','identifier','end','opcode','integer','float','string',
        'param','result','then','else','type','valtype');

    type

      { twasmreader }

      twasmreader = class(tasmreader)
      private
        actwasmbasictype: TWasmBasicType;
        actasmpattern_origcase: string;
        actasmtoken   : tasmtoken;
        prevasmtoken  : tasmtoken;
        actinttoken   : aint;
        procedure SetupTables;
        procedure GetToken;
        function consume(t : tasmtoken):boolean;
        function is_asmopcode(const s: string):boolean;
        function is_valtype(const s: string):boolean;
        procedure HandleInstruction;
        procedure HandleFoldedInstruction;
        procedure HandlePlainInstruction;
        procedure HandleBlockInstruction;virtual;abstract;
      public
        function Assemble: tlinkedlist;override;
      end;


  Implementation

    uses
      { helpers }
      cutils,
      { global }
      globals,verbose,
      systems,
      { aasm }
      cpuinfo,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symbase,symtype,symsym,symtable,symdef,symutil,
      { parser }
      scanner,pbase,
      procinfo,
      rabase,rautils,
      cgbase,cgutils,cgobj,
      { wasm }
      itcpuwasm
      ;


{*****************************************************************************
                                twasmreader
*****************************************************************************}

    procedure twasmreader.SetupTables;
      var
        i: TAsmOp;
      begin
        iasmops:=TFPHashList.create;
        for i:=firstop to lastop do
          if wasm_op2str[i]<>'end' then
            iasmops.Add(wasm_op2str[i],Pointer(PtrInt(i)));
      end;


    procedure twasmreader.GetToken;

      var
        has_sign, is_hex, is_float: Boolean;

      function GetIntToken: aint;
        var
          s: string;
          u64: UInt64;
        begin
          s:=actasmpattern;
          if has_sign and (s[1]='-') then
            begin
              delete(s,1,1);
              if is_hex then
                begin
                  delete(s,1,2);
                  Val('$'+s,u64);
                end
              else
                Val(s,u64);
{$push} {$R-}{$Q-}
              result:=aint(-u64);
{$pop}
            end
          else
            begin
              if has_sign then
                delete(s,1,1);
              if is_hex then
                begin
                  delete(s,1,2);
                  Val('$'+s,u64);
                end
              else
                Val(s,u64);
              result:=aint(u64);
            end;
        end;

      var
        len: Integer;
        tmpS: string;
        tmpI, tmpCode: Integer;
      begin
        c:=scanner.c;
        { save old token and reset new token }
        prevasmtoken:=actasmtoken;
        actasmtoken:=AS_NONE;
        { reset }
        actasmpattern:='';
        { while space, tab, new line or comment, continue scan... }
        while c in [' ',#9,#13,#10] do
          begin
            c:=current_scanner.asmgetchar;
            case c of
              ';':
                begin
                  c:=current_scanner.asmgetchar;
                  case c of
                    { ;; comment until end of line }
                    ';':
                      begin
                        { skip until end of line }
                        repeat
                          c:=current_scanner.asmgetchar;
                        until c in [#13,#10];
                      end;
                    else
                      current_scanner.illegal_char(c);
                  end;
                end;
              '(':
                begin
                  current_scanner.gettokenpos;
                  c:=current_scanner.asmgetchar;
                  case c of
                    { (; block comment ;) }
                    ';':
                      begin
                        { skip until ;) }
                        repeat
                          c:=current_scanner.asmgetchar;
                          if c=';' then
                            begin
                              c:=current_scanner.asmgetchar;
                              if c=')' then
                                begin
                                  c:=current_scanner.asmgetchar;
                                  break;
                                end;
                            end;
                        until false;
                      end;
                    else
                      begin
                        actasmtoken:=AS_LPAREN;
                        exit;
                      end;
                  end;
                end;
            end;
          end;
        current_scanner.gettokenpos;
        case c of
          ')':
            begin
              c:=current_scanner.asmgetchar;
              actasmtoken:=AS_RPAREN;
            end;
          '$','a'..'z','A'..'Z':
            begin
              len:=0;
              while c in ['A'..'Z','a'..'z','0'..'9',
                          '!','#','$','%','&','''','*','+','-','.','/',
                          ':','<','=','>','?','@','\','^','_','`','|','~'] do
                begin
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                end;
              actasmpattern[0]:=chr(len);
              actasmpattern_origcase:=actasmpattern;
              if actasmpattern[1]='$' then
                actasmtoken:=AS_ID
              else if is_asmopcode(actasmpattern) or
                      is_valtype(actasmpattern) then
                exit
              else if upper(actasmpattern) = 'END' then
                begin
                  uppervar(actasmpattern);
                  actasmtoken:=AS_END;
                  exit;
                end
              else
                begin
                  message1(asmr_e_unknown_opcode,actasmpattern);
                  actasmtoken:=AS_NONE;
                end;
            end;
          '0'..'9','+','-':
            begin
              len:=0;
              has_sign:=false;
              is_hex:=false;
              is_float:=false;
              if c in ['+','-'] then
                begin
                  has_sign:=true;
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                end;
              if c='0' then
                begin
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                  if c='x' then
                    begin
                      is_hex:=true;
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end;
              if is_hex then
                begin
                  while c in ['0'..'9','a'..'f','A'..'F'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end
              else
                begin
                  while c in ['0'..'9'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end;
              if c='.' then
                begin
                  is_float:=true;
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                  { parse the fractional part }
                  if is_hex then
                    begin
                      while c in ['0'..'9','a'..'f','A'..'F'] do
                        begin
                          inc(len);
                          actasmpattern[len]:=c;
                          c:=current_scanner.asmgetchar;
                        end;
                    end
                  else
                    begin
                      while c in ['0'..'9'] do
                        begin
                          inc(len);
                          actasmpattern[len]:=c;
                          c:=current_scanner.asmgetchar;
                        end;
                    end;
                end;
              if (is_hex and (c in ['p','P'])) or
                 ((not is_hex) and (c in ['e','E'])) then
                begin
                  inc(len);
                  actasmpattern[len]:=c;
                  c:=current_scanner.asmgetchar;
                  if c in ['+','-'] then
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                  while c in ['0'..'9'] do
                    begin
                      inc(len);
                      actasmpattern[len]:=c;
                      c:=current_scanner.asmgetchar;
                    end;
                end;
              actasmpattern[0]:=chr(len);
              if is_float then
                actasmtoken:=AS_REALNUM
              else
                begin
                  actasmtoken:=AS_INTNUM;
                  actinttoken:=GetIntToken;
                end;
            end;
          '"':
            begin
              actasmpattern:='';
              repeat
                c:=current_scanner.asmgetchar;
                case c of
                  '\' :
                    begin
                      c:=current_scanner.asmgetchar;
                      case c of
                        't':
                          begin
                            actasmpattern:=actasmpattern+#9;
                            c:=current_scanner.asmgetchar;
                          end;
                        'n':
                          begin
                            actasmpattern:=actasmpattern+#10;
                            c:=current_scanner.asmgetchar;
                          end;
                        'r':
                          begin
                            actasmpattern:=actasmpattern+#13;
                            c:=current_scanner.asmgetchar;
                          end;
                        '"':
                          begin
                            actasmpattern:=actasmpattern+'"';
                            c:=current_scanner.asmgetchar;
                          end;
                        '''':
                          begin
                            actasmpattern:=actasmpattern+'''';
                            c:=current_scanner.asmgetchar;
                          end;
                        '\':
                          begin
                            actasmpattern:=actasmpattern+'\';
                            c:=current_scanner.asmgetchar;
                          end;
                        'u':
                          begin
                            tmpS:='';
                            c:=current_scanner.asmgetchar;
                            while c in ['0'..'9','a'..'f','A'..'F'] do
                              begin
                                tmpS:=tmpS+c;
                                c:=current_scanner.asmgetchar;
                              end;
                            if tmpS<>'' then
                              begin
                                Val('$'+tmpS,tmpI,tmpCode);
                                if (tmpI<$D800) or ((tmpI>=$E000) and (tmpI<$110000)) then
                                  begin
                                    if tmpI<=$7F then
                                      actasmpattern:=actasmpattern+Chr(tmpI)
                                    else if tmpI<=$7FF then
                                      actasmpattern:=actasmpattern+
                                        Chr(%11000000 or (tmpI shr 6))+
                                        Chr(%10000000 or (tmpI and $3F))
                                    else if tmpI<=$FFFF then
                                      actasmpattern:=actasmpattern+
                                        Chr(%11100000 or (tmpI shr 12))+
                                        Chr(%10000000 or ((tmpI shr 6) and $3F))+
                                        Chr(%10000000 or (tmpI and $3F))
                                    else
                                      actasmpattern:=actasmpattern+
                                        Chr(%11110000 or (tmpI shr 18))+
                                        Chr(%10000000 or ((tmpI shr 12) and $3F))+
                                        Chr(%10000000 or ((tmpI shr 6) and $3F))+
                                        Chr(%10000000 or (tmpI and $3F))
                                  end
                                else
                                  Message1(asmr_e_escape_seq_ignored,'u'+tmpS);
                              end
                            else
                              Message1(asmr_e_escape_seq_ignored,'u');
                          end;
                        '0'..'9','a'..'f','A'..'F':
                          begin
                            tmpS:=c;
                            c:=current_scanner.asmgetchar;
                            if c in ['0'..'9','a'..'f','A'..'F'] then
                              begin
                                tmpS:=tmpS+c;
                                c:=current_scanner.asmgetchar;
                                Val('$'+tmpS,tmpI,tmpCode);
                                actasmpattern:=actasmpattern+Chr(tmpI);
                              end
                            else
                              begin
                                Message1(asmr_e_escape_seq_ignored,tmpS+c);
                                c:=current_scanner.asmgetchar;
                              end;
                          end;
                        else
                          begin
                            Message1(asmr_e_escape_seq_ignored,c);
                            c:=current_scanner.asmgetchar;
                          end;
                      end;
                    end;
                  '"' :
                    begin
                      c:=current_scanner.asmgetchar;
                      break;
                    end;
                  #10,#13:
                    Message(scan_f_string_exceeds_line);
                  #0..#9,#11,#12,#14..#31,#127:
                    current_scanner.illegal_char(c);
                  else
                    actasmpattern:=actasmpattern+c;
                end;
              until false;
              actasmtoken:=AS_STRING;
              exit;
            end;
          else
            current_scanner.illegal_char(c);
        end;
      end;


    function twasmreader.consume(t: tasmtoken): boolean;
      begin
        Consume:=true;
        if t<>actasmtoken then
         begin
           Message2(scan_f_syn_expected,token2str[t],token2str[actasmtoken]);
           Consume:=false;
         end;
        repeat
          gettoken;
        until actasmtoken<>AS_NONE;
      end;


    function twasmreader.is_asmopcode(const s: string): boolean;
      begin
        actopcode:=tasmop(PtrUInt(iasmops.Find(s)));
        if actopcode<>A_NONE then
          begin
            actasmtoken:=AS_OPCODE;
            is_asmopcode:=true;
          end
        else
          is_asmopcode:=false;
      end;


    function twasmreader.is_valtype(const s: string): boolean;
      begin
        actwasmbasictype:=wbt_Unknown;
        case s of
          'i32':
             actwasmbasictype:=wbt_i32;
          'i64':
             actwasmbasictype:=wbt_i64;
          'f32':
             actwasmbasictype:=wbt_f32;
          'f64':
             actwasmbasictype:=wbt_f64;
          'funcref':
             actwasmbasictype:=wbt_funcref;
          'externref':
             actwasmbasictype:=wbt_externref;
          'v128':
             actwasmbasictype:=wbt_v128;
        end;
        if actwasmbasictype<>wbt_Unknown then
          begin
            actasmtoken:=AS_VALTYPE;
            is_valtype:=true;
          end
        else
          is_valtype:=false;
      end;


    procedure twasmreader.HandleInstruction;
      begin
        case actasmtoken of
          AS_LPAREN:
            begin
              Consume(AS_LPAREN);
              HandleFoldedInstruction;
            end;
          AS_OPCODE:
            begin
              case actopcode of
                a_block,
                a_loop,
                a_if:
                  HandleBlockInstruction;
                else
                  HandlePlainInstruction;
              end;
            end;
          else
            {error};
        end;
      end;


    procedure twasmreader.HandleFoldedInstruction;
      var
        HasLabel, HasType, HasParam, HasResult, HasInstructions,
          HasThen, HasElse: Boolean;
        instr: TWasmInstruction;
        tmpS: string;
      begin
        //Consume(AS_LPAREN);
        case actasmtoken of
          AS_OPCODE:
            begin
              case actopcode of
                a_block,
                a_loop,
                a_if:
                  begin
                    Consume(AS_OPCODE);
                    HasType:=False;
                    HasParam:=False;
                    HasResult:=False;
                    HasInstructions:=False;
                    HasThen:=False;
                    HasElse:=False;
                    instr:=TWasmInstruction.create(TWasmOperand);
                    instr.opcode:=actopcode;
                    HasLabel:=False;
                    if actasmtoken=AS_ID then
                      begin
                        Consume(AS_ID);
                        HasLabel:=True;
                      end;
                    repeat
                      case actasmtoken of
                        AS_LPAREN:
                          begin
                            Consume(AS_LPAREN);
                            case actasmtoken of
                              AS_TYPE:
                                begin
                                  if HasElse or HasThen or HasInstructions or HasResult or HasParam or HasType then
                                    begin
                                      {TODO: error}
                                    end;
                                  Consume(AS_TYPE);
                                  //TODO: consume u32 or id
                                  Consume(actasmtoken);
                                  Consume(AS_RPAREN);
                                end;
                              AS_PARAM:
                                begin
                                  if HasElse or HasThen or HasInstructions or HasResult then
                                    begin
                                      {TODO: error}
                                    end;
                                  Consume(AS_PARAM);
                                  if actasmtoken=AS_ID then
                                    begin
                                      tmpS:=actasmpattern;
                                      Consume(AS_ID);
                                      if actasmtoken=AS_VALTYPE then
                                        instr.AddParam(actasmpattern,actwasmbasictype);
                                      Consume(AS_VALTYPE);
                                    end
                                  else
                                    begin
                                      while actasmtoken<>AS_RPAREN do
                                        begin
                                          if actasmtoken=AS_VALTYPE then
                                            instr.AddParam('',actwasmbasictype);
                                          Consume(AS_VALTYPE);
                                        end;
                                    end;
                                  Consume(AS_RPAREN);
                                end;
                              AS_RESULT:
                                begin
                                  if HasElse or HasThen or HasInstructions then
                                    begin
                                      {TODO: error}
                                    end;
                                  Consume(AS_RESULT);
                                  while actasmtoken<>AS_RPAREN do
                                    begin
                                      if actasmtoken=AS_VALTYPE then
                                        instr.AddResult(actwasmbasictype);
                                      Consume(AS_VALTYPE);
                                    end;
                                  Consume(AS_RPAREN);
                                end;
                              AS_THEN:
                                begin
                                  if instr.opcode<>a_if then
                                    {error!};
                                  Consume(AS_THEN);
                                  HasThen:=True;
                                  while actasmtoken<>AS_RPAREN do
                                    HandleInstruction;
                                  Consume(AS_RPAREN);
                                end;
                              AS_ELSE:
                                begin
                                  if instr.opcode<>a_if then
                                    {error!};
                                  Consume(AS_ELSE);
                                  HasElse:=True;
                                  while actasmtoken<>AS_RPAREN do
                                    HandleInstruction;
                                  Consume(AS_RPAREN);
                                end;
                              else
                                begin
                                  HasInstructions:=True;
                                  HandleFoldedInstruction;
                                end;
                            end;
                          end;
                        else
                          {todo: error};
                      end;
                    until false;
                  end;
                else
                  begin
                    HandlePlainInstruction;
                    {todo: parse next folded instructions, insert plain instruction after these}
                  end;
              end;
            end;
          else
            {error}
        end;
      end;


    procedure twasmreader.HandlePlainInstruction;
      var
        instr: TWasmInstruction;
      begin
        case actasmtoken of
          AS_OPCODE:
            begin
              instr:=TWasmInstruction.create(TWasmOperand);
              instr.opcode:=actopcode;
              Consume(AS_OPCODE);
              case actopcode of
                { instructions, which require 0 operands }
                a_nop,
                a_unreachable,
                a_return,
                a_ref_is_null,
                a_drop,
                a_memory_size,
                a_memory_grow,
                a_memory_fill,
                a_memory_copy,
                a_i32_clz,a_i32_ctz,a_i32_popcnt,a_i32_add,a_i32_sub,a_i32_mul,a_i32_div_s,a_i32_div_u,a_i32_rem_s,a_i32_rem_u,a_i32_and,a_i32_or,a_i32_xor,a_i32_shl,a_i32_shr_s,a_i32_shr_u,a_i32_rotl,a_i32_rotr,
                a_i64_clz,a_i64_ctz,a_i64_popcnt,a_i64_add,a_i64_sub,a_i64_mul,a_i64_div_s,a_i64_div_u,a_i64_rem_s,a_i64_rem_u,a_i64_and,a_i64_or,a_i64_xor,a_i64_shl,a_i64_shr_s,a_i64_shr_u,a_i64_rotl,a_i64_rotr,
                a_f32_abs,a_f32_neg,a_f32_ceil,a_f32_floor,a_f32_trunc,a_f32_nearest,a_f32_sqrt,a_f32_add,a_f32_sub,a_f32_mul,a_f32_div,a_f32_min,a_f32_max,a_f32_copysign,
                a_f64_abs,a_f64_neg,a_f64_ceil,a_f64_floor,a_f64_trunc,a_f64_nearest,a_f64_sqrt,a_f64_add,a_f64_sub,a_f64_mul,a_f64_div,a_f64_min,a_f64_max,a_f64_copysign,
                a_i32_eqz,a_i32_eq,a_i32_ne,a_i32_lt_s,a_i32_lt_u,a_i32_gt_s,a_i32_gt_u,a_i32_le_s,a_i32_le_u,a_i32_ge_s,a_i32_ge_u,
                a_i64_eqz,a_i64_eq,a_i64_ne,a_i64_lt_s,a_i64_lt_u,a_i64_gt_s,a_i64_gt_u,a_i64_le_s,a_i64_le_u,a_i64_ge_s,a_i64_ge_u,
                a_f32_eq,a_f32_ne,a_f32_lt,a_f32_gt,a_f32_le,a_f32_ge,
                a_f64_eq,a_f64_ne,a_f64_lt,a_f64_gt,a_f64_le,a_f64_ge,

                a_i32_wrap_i64,
                a_i32_trunc_f32_s,
                a_i32_trunc_f32_u,
                a_i32_trunc_f64_s,
                a_i32_trunc_f64_u,
                a_i32_trunc_sat_f32_s,
                a_i32_trunc_sat_f32_u,
                a_i32_trunc_sat_f64_s,
                a_i32_trunc_sat_f64_u,
                a_i64_extend_i32_s,
                a_i64_extend_i32_u,
                a_i64_trunc_f32_s,
                a_i64_trunc_f32_u,
                a_i64_trunc_f64_s,
                a_i64_trunc_f64_u,
                a_i64_trunc_sat_f32_s,
                a_i64_trunc_sat_f32_u,
                a_i64_trunc_sat_f64_u,
                a_i64_trunc_sat_f64_s,
                a_f32_convert_i32_s,
                a_f32_convert_i32_u,
                a_f32_convert_i64_s,
                a_f32_convert_i64_u,
                a_f32_demote_f64,
                a_f64_convert_i32_s,
                a_f64_convert_i32_u,
                a_f64_convert_i64_s,
                a_f64_convert_i64_u,
                a_f64_promote_f32,
                a_i32_reinterpret_f32,
                a_i64_reinterpret_f64,
                a_f32_reinterpret_i32,
                a_f64_reinterpret_i64,

                a_i32_extend8_s,
                a_i32_extend16_s,
                a_i64_extend8_s,
                a_i64_extend16_s,
                a_i64_extend32_s:
                  ;
                { instructions with an integer const operand }
                a_i32_const,
                a_i64_const:
                  begin
                    if actasmtoken=AS_INTNUM then
                      begin
                        instr.operands[1].opr.typ:=OPR_CONSTANT;
                        instr.operands[1].opr.val:=actinttoken;
                        Consume(AS_INTNUM);
                      end
                    else
                      begin
                        { error: expected integer }
                        instr.Free;
                        instr:=nil;
                        Consume(AS_INTNUM);
                      end;
                  end;
                else
                  internalerror(2024071401);
              end;
            end;
          else
            {error};
        end;
      end;


    function twasmreader.Assemble: tlinkedlist;
      begin
        Message1(asmr_d_start_reading,'WebAssembly');
        firsttoken:=TRUE;
        { sets up all opcode and register tables in uppercase }
        if not _asmsorted then
          begin
            SetupTables;
            _asmsorted:=TRUE;
          end;
        curlist:=TAsmList.Create;

        { we might need to know which parameters are passed in registers }
        if not parse_generic then
          current_procinfo.generate_parameter_info;

        { start tokenizer }
        gettoken;
        { main loop }
        repeat
          Writeln(actasmtoken);
          case actasmtoken of
            AS_END:
              break; { end assembly block }
            else
              begin
                Consume(actasmtoken);
                //Message(asmr_e_syntax_error);
                //RecoverConsume(false);
              end;
          end;
        until false;

        { Return the list in an asmnode }
        assemble:=curlist;
        Message1(asmr_d_finish_reading,'WebAssembly');
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_wasm_standard_info : tasmmodeinfo =
          (
            id    : asmmode_standard;
            idtxt : 'STANDARD';
            casmreader : twasmreader;
          );

initialization
  RegisterAsmMode(asmmode_wasm_standard_info);
end.
