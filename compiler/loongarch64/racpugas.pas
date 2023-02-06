{
    Copyright (c) 2019 by Jeppe Johansen

    Does the parsing for the LoongArch64 GNU AS styled inline assembler.

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
unit racpugas;

{$I fpcdefs.inc}

  interface

    uses
      globtype,
      raatt,racpu,
      cpubase;

    const
      NRCalMax=64;

    type
      TArithOpers = (LLOR,LLAND,LINOR,LEXOR,LAND,LEQU,LUNE,LSLT,
        LBGT,LSLE,LBGE,LSL,LSR,LADD,LSUB,LMUL,LMOD,LDIV);
      TArithTreeType = (ATT_NUM,ATT_OP);
      TArithTree = record
        left : integer;
        right : integer;
        case typ:TArithTreeType of
          ATT_NUM : (num : tcgint);
          ATT_OP : (op : TArithOpers);
      end;
      TArrCals = array[1..NRCalMax] of TArithTree;
      TArrNums = array[1..(NRCalMax shr 1)] of tcgint;
      TArrAOps = array[1..(NRCalMax shr 1)] of TArithOpers;

      tloongarch64gasreader = class(tattreader)
        function is_register(const s: string):boolean;override;
        function is_asmopcode(const s: string):boolean;override;
        procedure handledollar;override;
        procedure handleopcode;override;
        procedure BuildOperand(oper : tloongarch64operand);
        procedure BuildOpCode(instr : tloongarch64instruction);
        function CalculateExprs(nr,first : tcgint; var nums : TArrNums; var ops : TArrAOps): tcgint;
        function BuildConstLA(from_question,cond : boolean): tcgint;
        function BuildSymLA(oper : tloongarch64operand; maybeconst: boolean): boolean;
      end;

  implementation

    uses
      { helpers }
      cutils,
      { global }
      globals,verbose,
      systems,
      { aasm }
      aasmbase,aasmtai,aasmdata,aasmcpu,
      { symtable }
      symconst,symsym,symdef,
      { parser }
      scanner,
      procinfo,
      rabase,rautils,
      cgbase,cgobj
      ;


    procedure tloongarch64gasreader.BuildOperand(oper: tloongarch64operand);
      var
        expr : string;
        typesize,l : TCGInt;
        tempreg : tregister;
        hl : tasmlabel;
        ofs : aint;
        refaddr: trefaddr;
        entered_paren: Boolean;
      Begin
        expr:='';
        entered_paren:=false;
        refaddr:=addr_no;
        { Although assembler has diverse ways to decode parameters,
          there are four normal ways to describe them.
          1. Registers.
          2. %addrtype(Symbol'addend'), addend means expression.
          3. Symbol'addend'.
          4. Else, expression. }
        case actasmtoken of
          AS_REGISTER:
            begin
              { save the type of register used. }
              tempreg:=actasmregister;
              Consume(AS_REGISTER);
              if (actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
                begin
                  oper.opr.typ:=OPR_REGISTER;
                  oper.opr.reg:=tempreg;
                end
              else
                Message(asmr_e_syn_operand);
            end;
          AS_MOD:
            begin
              Consume(AS_MOD);
              if actasmtoken<>AS_ID then
                Message(asmr_e_syntax_error);
              if lower(actasmpattern)='b16' then
                refaddr:=addr_b16
              else if lower(actasmpattern)='b21' then
                refaddr:=addr_b21
              else if lower(actasmpattern)='b26' then
                refaddr:=addr_b26
              else if lower(actasmpattern)='plt' then
                refaddr:=addr_plt
              else if lower(actasmpattern)='abs_hi20' then
                refaddr:=addr_abs_hi20
              else if lower(actasmpattern)='abs_lo12' then
                refaddr:=addr_abs_lo12
              else if lower(actasmpattern)='abs64_lo20' then
                refaddr:=addr_abs64_lo20
              else if lower(actasmpattern)='abs64_hi12' then
                refaddr:=addr_abs64_hi12
              else if lower(actasmpattern)='pc_hi20' then
                refaddr:=addr_pc_hi20
              else if lower(actasmpattern)='pc_lo12' then
                refaddr:=addr_pc_lo12
              else if lower(actasmpattern)='got_pc_hi20' then
                refaddr:=addr_got_pc_hi20
              else if lower(actasmpattern)='got_pc_lo12' then
                refaddr:=addr_got_pc_lo12
              else
                Message(asmr_e_syntax_error);
              Consume(AS_ID);
              if actasmtoken<>AS_LPAREN then
                Message(asmr_e_syntax_error);
              Consume(AS_LPAREN);
              BuildSymLA(oper,false);
              if actasmtoken<>AS_RPAREN then
                Message(asmr_e_syntax_error);
              Consume(AS_RPAREN);
            end;
          AS_DOT, AS_ID:
            begin
              if actopcode=A_LA_GOT then
                refaddr:=addr_pcrel
              else if actopcode=A_LA_ABS then
                refaddr:=addr_abs
              else
                refaddr:=addr_pcrel;
              if not BuildSymLA(oper,actasmtoken=AS_ID) then
                refaddr:=addr_no;
            end;
          AS_END,
          AS_SEPARATOR,
          AS_COMMA: ;
        else
          begin
            oper.opr.typ:=OPR_CONSTANT;
            oper.opr.val:=BuildConstLA(false,false);
          end;
        end; { end case }

      if refaddr<>addr_no then
        begin
          { Indirectly use parameter can be local and sym is paravarsym. }
          if oper.opr.typ=OPR_LOCAL then
              exit;
          if oper.opr.typ<>OPR_REFERENCE then
            oper.InitRef;
          oper.opr.ref.refaddr:=refaddr;
        end
      else if (oper.opr.typ=OPR_REFERENCE) and
              (oper.opr.ref.refaddr=addr_no) and
              assigned(oper.opr.ref.symbol) then
        oper.opr.ref.refaddr:=addr_pcrel;

      end;


{*****************************************************************************
                                tloongarch64gasreader
*****************************************************************************}

    procedure tloongarch64gasreader.BuildOpCode(instr : tloongarch64instruction);
      var
        operandnum : longint;
      begin
        { opcode }
        if (actasmtoken<>AS_OPCODE) then
         begin
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
              begin
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
            BuildOperand(instr.Operands[operandnum] as tloongarch64operand);
          end; { end case }
        until false;
        if (operandnum=1) and (instr.Operands[operandnum].opr.typ=OPR_NONE) then
          dec(operandnum);
        instr.Ops:=operandnum;
      end;


    function tloongarch64gasreader.is_register(const s: string): boolean;
      var
        reg: TRegister;
      begin
        result:=inherited is_register(s);
        { reg found? search it in abinames?  }
        if not(result) then
          begin
            reg:=is_extra_reg(s);
            if reg<>NR_NO then
              begin
                actasmregister:=reg;
                result:=true;
                actasmtoken:=AS_REGISTER;
              end;
          end;
      end;


    function tloongarch64gasreader.is_asmopcode(const s: string):boolean;
      var
        cond  : tasmcond;
        hs, postfix : string;
        l: longint;
      begin
        { making s a value parameter would break other assembler readers }
        hs:=s;
        is_asmopcode:=false;

        { clear op code }
        actopcode:=A_None;
        { clear condition }
        fillchar(actcondition,sizeof(actcondition),0);

        { check for direction hint }
        actopcode := tasmop(ptruint(iasmops.find(hs)));
        if actopcode <> A_NONE then
          begin
            actasmtoken:=AS_OPCODE;
            is_asmopcode:=true;
            exit;
          end;
      end;


    procedure tloongarch64gasreader.handledollar;
      var
        len: longint;
      begin
        len:=1;
        actasmpattern[len]:='$';
        c:=current_scanner.asmgetchar;
        while c in ['A'..'Z','a'..'z','0'..'9'] do
          begin
            inc(len);
            actasmpattern[len]:=c;
            c:=current_scanner.asmgetchar;
          end;
        actasmpattern[0]:=chr(len);
        actasmpattern:=lower(actasmpattern);
        { TODO Something else. }
        if not is_register(actasmpattern) then
          internalerror(2022062915);
      end;


    procedure tloongarch64gasreader.handleopcode;
      var
        instr : tloongarch64instruction;
      begin
        instr:=tloongarch64instruction.create(tloongarch64operand);
        BuildOpCode(instr);
        { TODO insruction field }
        instr.ConcatInstruction(curlist);
        instr.Free;
      end;


    { In LoongArch binutils gas, the expression calculation is complex.
      The priority is ternary is the lowest and the unary is the highest.
      Ternary '?:', binary ordered by form low to high is '||', '&&',
      '|', '^', '&', '==,!=', '<,<=,>,>=', '<<,>>', '+,-' and '*,/,%',
      Unary is '+,-,~,!'. It is different form CalculateExpression, so
      we should implement ourselves. Collect integers and binary,
      calculate parentheses by recursing call, mark ternay and calculate
      unary in time. }
    function tloongarch64gasreader.CalculateExprs(nr,first : tcgint; var nums : TArrNums; var ops : TArrAOps): tcgint;

      procedure alloc_num(var idx : integer; var arr : TArrCals; num : int64);
        begin
          idx:=idx+1;
          if idx>NRCalMax then
            internalerror(2022081601);
          arr[idx].typ:=ATT_NUM;
          arr[idx].num:=num;
        end;

      procedure alloc_op(var idx : integer; var arr : TArrCals; op : TArithOpers);
        begin
          idx:=idx+1;
          if idx>NRCalMax then
            internalerror(2022081602);
          arr[idx].typ:=ATT_OP;
          arr[idx].op:=op;
        end;

      function priority_less(op1, op2 : TArithOpers): boolean;
        begin
          case op1 of
            LMUL,LMOD,LDIV:
              priority_less:=false;
            LADD,LSUB:
              priority_less:=op2 in [LMUL..LDIV];
            LSL,LSR:
              priority_less:=op2 in [LADD..LDIV];
            LSLT,LBGT,LSLE,LBGE:
              priority_less:=op2 in [LSL..LDIV];
            LEQU,LUNE:
              priority_less:=op2 in [LSLT..LDIV];
            LAND:
              priority_less:=op2 in [LEQU..LDIV];
            LEXOR:
              priority_less:=op2 in [LAND..LDIV];
            LINOR:
              priority_less:=op2 in [LEXOR..LDIV];
            LLAND:
              priority_less:=op2 in [LINOR..LDIV];
            LLOR:
              priority_less:=op2<>LLOR;
          end;
        end;

      function get_where_insert(head,item : integer; var arr : TArrCals) : integer;
        var
          last,t : integer;
        begin
          last:=0;
          t:=head;
          while arr[t].typ=ATT_OP do
            begin
              if priority_less(arr[item].op,arr[t].op) then
                break;
              last:=t;
              t:=arr[t].right;
            end;
          get_where_insert:=last;
        end;

      function arith_treecal(var arr : TArrCals; idx : integer): tcgint;
        var
          lv,rv : tcgint;
        begin
          if arr[idx].typ=ATT_NUM then
            begin
              result:=arr[idx].num;
              exit;
            end;
          if (arr[idx].left=0) and (arr[idx].right=0) then
            internalerror(2022081705);
          lv:=arith_treecal(arr,arr[idx].left);
          rv:=arith_treecal(arr,arr[idx].right);
          case arr[idx].op of
            LLOR: result:=tcgint((lv<>0) or (rv<>0));
            LLAND: result:=tcgint((lv<>0) and (rv<>0));
            LINOR: result:=lv or rv;
            LEXOR: result:=lv xor rv;
            LAND: result:=lv and rv;
            LEQU: result:=tcgint(lv=rv);
            LUNE: result:=tcgint(lv<>rv);
            LSLT: result:=tcgint(lv<rv);
            LBGT: result:=tcgint(lv>rv);
            LSLE: result:=tcgint(lv<=rv);
            LBGE: result:=tcgint(lv>=rv);
            LSL: result:=lv<<rv;
            LSR: result:=lv>>rv;
            LADD: result:=lv+rv;
            LSUB: result:=lv-rv;
            LMUL: result:=lv*rv;
            LMOD: result:=lv mod rv;
            LDIV: result:=lv div rv;
          end;
        end;

      procedure debug_treecal(var arr : TArrCals; idx : integer; first : boolean);
        const
          strops: Array[LLOR..LDIV] of string[3] =
            ('||','&&','|','^','&','==','!=','<','>',
             '<=','>=','<<','>>','+','-','*','%','/');
        begin
          if first then
            writeln('[Debug] ');
          if arr[idx].typ=ATT_NUM then
            begin
              write(arr[idx].num);
              exit;
            end;
          if (arr[idx].left=0) and (arr[idx].right=0) then
            writeln(#10,'[Debug Error]');
          debug_treecal(arr,arr[idx].left,false);
          if arr[idx].typ=ATT_OP then
            write(strops[arr[idx].op]);
          debug_treecal(arr,arr[idx].right,false);
          if first then
            writeln('=',arith_treecal(arr,idx));
        end;

      var
        i,curidx,curhead,curright,cursym,insidx : integer;
        exprs : TArrCals;
      begin
        if nr=0 then
          internalerror(2022081704);
        curidx:=0;
        alloc_num(curidx,exprs,first);
        curhead:=curidx;
        cursym:=0;
        for i := 1  to nr do
          begin
            alloc_op(curidx,exprs,ops[i]);
            cursym:=curidx;
            alloc_num(curidx,exprs,nums[i]);
            curright:=curidx;
            exprs[cursym].right:=curright;
            insidx:=get_where_insert(curhead,cursym,exprs);
            if insidx=0 then
              begin
                exprs[cursym].left:=curhead;
                curhead:=cursym;
              end
            else
              begin
                exprs[cursym].left:=exprs[insidx].right;
                exprs[insidx].right:=cursym;
              end;
          end;
        { debug_treecal(exprs,curhead,true); }
        result:=arith_treecal(exprs,curhead);
      end;

    function tloongarch64gasreader.BuildConstLA(from_question,cond : boolean): tcgint;

      function get_a_int: tcgint;
        var
          l : tcgint;
        begin
          result:=0;
          case actasmtoken of
            AS_PLUS:
              begin
                Consume(AS_PLUS);
                result:=get_a_int();
              end;
            AS_MINUS:
              begin
                Consume(AS_MINUS);
                result:=-get_a_int();
              end;
            AS_NOT:
              begin
                Consume(AS_NOT);
                result:=tcgint(get_a_int()=0);
              end;
            AS_NOR:
              begin
                Consume(AS_NOR);
                result:=not get_a_int();
              end;
            AS_ID:
              begin
                if SearchIConstant(actasmpattern,l) then
                  result:=l
                else
                  internalerror(2022081101);
                Consume(AS_ID);
              end;
            AS_INTNUM:
              begin
                result:=CalculateExpression(actasmpattern);
                Consume(AS_INTNUM);
              end;
            AS_LPAREN:
              begin
                Consume(AS_LPAREN);
                l:=BuildConstLA(false,false);
                if actasmtoken<>AS_RPAREN then
                  Message(asmr_e_syntax_error);
                Consume(AS_RPAREN);
                result:=l;
              end;
          else
            Message(asmr_e_syntax_error);
          end;
        end;

      function get_a_op: TArithOpers;
        begin
          result:=LADD; { As initialized }
          case actasmtoken of
            AS_SHL:
              begin
                Consume(AS_SHL);
                result:=LSL;
              end;
            AS_SHR:
              begin
                Consume(AS_SHR);
                result:=LSR;
              end;
            AS_LT:
              begin
                Consume(AS_LT);
                if actasmtoken=AS_EQUAL then
                  begin
                    Consume(AS_EQUAL);
                    result:=LSLE;
                  end
                else
                  result:=LSLT;
              end;
            AS_GT:
              begin
                Consume(AS_GT);
                if actasmtoken=AS_EQUAL then
                  begin
                    Consume(AS_EQUAL);
                    result:=LBGE;
                  end
                else
                  result:=LBGT;
              end;
            AS_OR:
              begin
                Consume(AS_OR);
                if actasmtoken=AS_OR then
                  begin
                    Consume(AS_OR);
                    result:=LLOR;
                  end
                else
                  result:=LINOR;
              end;
            AS_AND:
              begin
                Consume(AS_AND);
                if actasmtoken=AS_AND then
                  begin
                    Consume(AS_AND);
                    result:=LLAND;
                  end
                else
                  result:=LAND;
              end;
            AS_EQUAL:
              begin
                Consume(AS_EQUAL);
                if actasmtoken<>AS_EQUAL then
                  internalerror(2022081701);
                Consume(AS_EQUAL);
                result:=LEQU;
              end;
            AS_NOT:
              begin
                Consume(AS_NOT);
                if actasmtoken<>AS_EQUAL then
                  internalerror(2022081702);
                Consume(AS_EQUAL);
                result:=LUNE;
              end;
            AS_XOR:
              begin
                Consume(AS_XOR);
                result:=LEXOR;
              end;
            AS_PLUS:
              begin
                Consume(AS_PLUS);
                result:=LADD;
              end;
            AS_MINUS:
              begin
                Consume(AS_MINUS);
                result:=LSUB;
              end;
            AS_STAR:
              begin
                Consume(AS_STAR);
                result:=LMUL;
              end;
            AS_MOD:
              begin
                Consume(AS_MOD);
                result:=LMOD;
              end;
            AS_SLASH:
              begin
                Consume(AS_SLASH);
                result:=LDIV;
              end;
          else
            Message(asmr_e_syntax_error);
          end;
        end;

      var
        firstnum,nr,l,l2 : tcgint;
        IntStack : TArrNums;
        OpsStack : TArrAOps;
        op_or_int: boolean;
      begin
        result:=0;
        nr:=0;

        firstnum:=get_a_int;
        { Most of case will return as they only have an integer. }

        repeat
          case actasmtoken of
            AS_END,AS_SEPARATOR,AS_COMMA,AS_RPAREN: break;
            AS_LPAREN:
              begin
                Consume(AS_LPAREN);
                l:=BuildConstLA(false,false);
                if actasmtoken<>AS_RPAREN then
                  Message(asmr_e_syntax_error);
                Consume(AS_RPAREN);
              end;
            AS_QUESTION:
              begin
                Consume(AS_QUESTION);
                if nr=0 then
                  l:=firstnum
                else
                  l:=CalculateExprs(nr,firstnum,IntStack,OpsStack);
                result:=BuildConstLA(true,l<>0);
                exit;
              end;
            AS_COLON:
              begin
                if not from_question then
                  Message(asmr_e_syntax_error);
                Consume(AS_COLON);
                if nr=0 then
                  l:=firstnum
                else
                  l:=CalculateExprs(nr,firstnum,IntStack,OpsStack);
                l2:=BuildConstLA(false,false);
                if cond then
                  result:=l
                else
                  result:=l2;
                exit;
              end;
          else
            begin
              nr:=nr+1;
              if nr>(NRCalMax shr 1) then
                internalerror(2022081703);
              OpsStack[nr]:=get_a_op;
              IntStack[nr]:=get_a_int;
            end;
          end; { case actasmtoken }
        until false;

        if nr=0 then
          result:=firstnum
        else
          result:=CalculateExprs(nr,firstnum,IntStack,OpsStack);
      end;


    function tloongarch64gasreader.BuildSymLA(oper : tloongarch64operand; maybeconst : boolean): boolean;
      var
        hl : tasmlabel;
        value : tcgint;
        toffset, tsize: tcgint;
        expr,mangledname: string;
      begin
        result:=True;
        if (is_locallabel(actasmpattern)) then
          begin
            CreateLocalLabel(actasmpattern,hl,false);
            Consume(AS_ID);
            oper.InitRef;
            oper.opr.ref.symbol:=hl;
          end
        else if SearchLabel(actasmpattern,hl,false) then
          begin
            Consume(AS_ID);
            oper.InitRef;
            oper.opr.ref.symbol:=hl;
          end
        else if maybeconst then
          begin
            if not SearchIConstant(actasmpattern,value) then
              begin
                expr:=actasmpattern;
                Consume(AS_ID);
                if actasmtoken = AS_DOT then
                  begin
                    mangledname:='';
                    BuildRecordOffsetSize(expr, toffset, tsize, mangledname, false);
                    if mangledname <> '' then
                      Message(asmr_e_wrong_sym_type);
                    oper.opr.typ:=OPR_CONSTANT;
                    oper.opr.val:=toffset;
                    Result:=False;
                  end
                else
                  begin
                    oper.InitRef;
                    oper.SetupVar(expr,false);
                  end;
              end;
          end
        else if actasmtoken=AS_ID then
          begin
            oper.InitRef;
            oper.SetupVar(actasmpattern,false);
            Consume(AS_ID);
          end
        else
          begin
            Message(asmr_e_syntax_error);
            internalerror(2022082501);
          end;

        if actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA,AS_RPAREN] then
          exit;

        value:=BuildConstLA(false,false);
        if assigned(oper.opr.ref.symbol) then
          oper.opr.ref.offset:=value
        else
          begin
            oper.opr.typ:=OPR_CONSTANT;
            oper.opr.val:=value;
            Result:=False;
          end;
      end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

    const
      asmmode_loongarch64_standard_info : tasmmodeinfo =
              (
                id    : asmmode_standard;
                idtxt : 'STANDARD';
                casmreader : tloongarch64gasreader;
              );

initialization
  RegisterAsmMode(asmmode_loongarch64_standard_info);
end.
