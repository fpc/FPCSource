{
    $Id$
    Copyright (c) 1998-2000 by Carl Eric Codere

    This unit does the parsing process for the motorola inline assembler

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
Unit Rasm;
{**********************************************************************}
{ WARNING                                                              }
{**********************************************************************}
{  Any modification in the order or removal of terms in the tables     }
{  in m68k.pas and asmo68k.pas  will BREAK the code in this unit,      }
{  unless the appropriate changes are made to this unit. Addition      }
{  of terms though, will not change the code herein.                   }
{**********************************************************************}

{---------------------------------------------------------------------------}
{ LEFT TO DO                                                                }
{---------------------------------------------------------------------------}
{  o Add support for sized indexing such as in d0.l                         }
{      presently only (an,dn) is supported for indexing --                  }
{        size defaults to LONG.                                             }
{  o Add support for MC68020 opcodes.                                       }
{  o Add support for MC68020 adressing modes.                               }
{  o Add operand checking with m68k opcode table in ConcatOpCode            }
{  o Add Floating point support                                             }
{---------------------------------------------------------------------------}

Interface

Uses
  globtype,cpubase,tree;

   function assemble: ptree;

const
 { this variable is TRUE if the lookup tables have already been setup  }
 { for fast access. On the first call to assemble the tables are setup }
 { and stay set up.                                                    }
 _asmsorted: boolean = FALSE;
 firstasmreg       = R_D0;
 lastasmreg        = R_FPSR;

type
 tiasmops = array[firstop..lastop] of string[7];
 piasmops = ^tiasmops;

 tasmkeyword = string[6];

var
 { sorted tables of opcodes }
 iasmops: piasmops;
 { uppercased tables of registers }
 iasmregs: array[firstasmreg..lastasmreg] of string[6];


Implementation

uses
  files,globals,systems,RAUtils,strings,hcodegen,scanner,aasm,
  cpuasm,cobjects,verbose,symconst,symtable;


type
 tmotorolatoken = (
   AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_HEXNUM,AS_OCTALNUM,
   AS_BINNUM,AS_COMMA,AS_LBRACKET,AS_RBRACKET,AS_LPAREN,
   AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,AS_INTNUM,
   AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_SLASH,AS_APPT,AS_REALNUM,
   AS_ALIGN,
     {------------------ Assembler directives --------------------}
   AS_DB,AS_DW,AS_DD,AS_XDEF,AS_END,
     {------------------ Assembler Operators  --------------------}
   AS_MOD,AS_SHL,AS_SHR,AS_NOT,AS_AND,AS_OR,AS_XOR);

const
   firstdirective = AS_DB;
   lastdirective  = AS_END;
   firstoperator  = AS_MOD;
   lastoperator   = AS_XOR;

   _count_asmdirectives = longint(lastdirective)-longint(firstdirective);
   _count_asmoperators  = longint(lastoperator)-longint(firstoperator);

   _asmdirectives : array[0.._count_asmdirectives] of tasmkeyword =
    ('DC.B','DC.W','DC.L','XDEF','END');

    { problems with shl,shr,not,and,or and xor, they are }
    { context sensitive.                                 }
    _asmoperators : array[0.._count_asmoperators] of tasmkeyword = (
    'MOD','SHL','SHR','NOT','AND','OR','XOR');


const
  newline = #10;
  firsttoken : boolean = TRUE;
  operandnum : byte = 0;
var
 p : paasmoutput;
 actasmtoken: tmotorolatoken;
 actasmpattern: string;
 c: char;
 Instr: TInstruction;
 old_exit : pointer;

   Procedure SetupTables;
   { creates uppercased symbol tables for speed access }
   var
     i: tasmop;
     j: tregister;
   Begin
     {Message(asmr_d_creating_lookup_tables);}
     { opcodes }
     new(iasmops);
     for i:=firstop to lastop do
      iasmops^[i] := upper(mot_op2str[i]);
     { opcodes }
     for j:=firstasmreg to lastasmreg do
      iasmregs[j] := upper(mot_reg2str[j]);
   end;


  {---------------------------------------------------------------------}
  {                     Routines for the tokenizing                     }
  {---------------------------------------------------------------------}


   function is_asmopcode(s: string):Boolean;
  {*********************************************************************}
  { FUNCTION is_asmopcode(s: string):Boolean                            }
  {  Description: Determines if the s string is a valid opcode          }
  {  if so returns TRUE otherwise returns FALSE.                        }
  {  Remark: Suffixes are also checked, as long as they are valid.      }
  {*********************************************************************}
   var
    i: tasmop;
    j: byte;
   Begin
     is_asmopcode := FALSE;
     { first of all we remove the suffix }
     j:=pos('.',s);
     if j<>0 then
      delete(s,j,2);
     for i:=firstop to lastop do
     begin
       if  s = iasmops^[i] then
       begin
          is_asmopcode:=TRUE;
          exit;
       end;
     end;
   end;



   Procedure is_asmdirective(const s: string; var token: tmotorolatoken);
  {*********************************************************************}
  { FUNCTION is_asmdirective(s: string; var token: tinteltoken):Boolean }
  {  Description: Determines if the s string is a valid directive       }
  { (an operator can occur in operand fields, while a directive cannot) }
  {  if so returns the directive token, otherwise does not change token.}
  {*********************************************************************}
   var
    i:byte;
   Begin
     for i:=0 to _count_asmdirectives do
     begin
        if s=_asmdirectives[i] then
        begin
           token := tmotorolatoken(longint(firstdirective)+i);
           exit;
        end;
     end;
   end;


   Procedure is_register(const s: string; var token: tmotorolatoken);
  {*********************************************************************}
  { PROCEDURE is_register(s: string; var token: tinteltoken);           }
  {  Description: Determines if the s string is a valid register, if    }
  {  so return token equal to A_REGISTER, otherwise does not change token}
  {*********************************************************************}
   Var
    i: tregister;
   Begin
     for i:=firstasmreg to lastasmreg do
     begin
      if s=iasmregs[i] then
      begin
        token := AS_REGISTER;
        exit;
      end;
     end;
     { take care of other name for sp }
     if s = 'A7' then
     begin
      token:=AS_REGISTER;
      exit;
     end;
   end;



  Function GetToken: tmotorolatoken;
  {*********************************************************************}
  { FUNCTION GetToken: tinteltoken;                                     }
  {  Description: This routine returns intel assembler tokens and       }
  {  does some minor syntax error checking.                             }
  {*********************************************************************}
  var
   token: tmotorolatoken;
   forcelabel: boolean;
  begin
    forcelabel := FALSE;
    actasmpattern :='';
    {* INIT TOKEN TO NOTHING *}
    token := AS_NONE;
    { while space and tab , continue scan... }
    while c in [' ',#9] do
     c:=current_scanner^.asmgetchar;

    if not (c in [newline,#13,'{',';']) then
     current_scanner^.gettokenpos;
    { Possiblities for first token in a statement:                }
    {   Local Label, Label, Directive, Prefix or Opcode....       }
    if firsttoken and not (c in [newline,#13,'{',';']) then
    begin

      firsttoken := FALSE;
      if c = '@' then
      begin
        token := AS_LLABEL;   { this is a local label }
        { Let us point to the next character }
        c := current_scanner^.asmgetchar;
      end;



      while c in ['A'..'Z','a'..'z','0'..'9','_','@','.'] do
      begin
         { if there is an at_sign, then this must absolutely be a label }
         if c = '@' then forcelabel:=TRUE;
         actasmpattern := actasmpattern + c;
         c := current_scanner^.asmgetchar;
      end;

      uppervar(actasmpattern);

      if c = ':' then
      begin
           case token of
             AS_NONE: token := AS_LABEL;
             AS_LLABEL: ; { do nothing }
           end; { end case }
           { let us point to the next character }
           c := current_scanner^.asmgetchar;
           gettoken := token;
           exit;
      end;

      { Are we trying to create an identifier with }
      { an at-sign...?                             }
      if forcelabel then
       Message(asmr_e_none_label_contain_at);

      If is_asmopcode(actasmpattern) then
      Begin
       gettoken := AS_OPCODE;
       exit;
      end;
      is_asmdirective(actasmpattern, token);
      if (token <> AS_NONE) then
      Begin
        gettoken := token;
        exit
      end
      else
      begin
         gettoken := AS_NONE;
         Message1(asmr_e_invalid_or_missing_opcode,actasmpattern);
      end;
    end
    else { else firsttoken }
    { Here we must handle all possible cases                              }
    begin
      case c of

         '@':   { possiblities : - local label reference , such as in jmp @local1 }
                {                - @Result, @Code or @Data special variables.     }
                            begin
                             actasmpattern := c;
                             c:= current_scanner^.asmgetchar;
                             while c in  ['A'..'Z','a'..'z','0'..'9','_','@','.'] do
                             begin
                               actasmpattern := actasmpattern + c;
                               c := current_scanner^.asmgetchar;
                             end;
                             uppervar(actasmpattern);
                             gettoken := AS_ID;
                             exit;
                            end;
      { identifier, register, opcode, prefix or directive }
         'A'..'Z','a'..'z','_': begin
                             actasmpattern := c;
                             c:= current_scanner^.asmgetchar;
                             while c in  ['A'..'Z','a'..'z','0'..'9','_','.'] do
                             begin
                               actasmpattern := actasmpattern + c;
                               c := current_scanner^.asmgetchar;
                             end;
                             uppervar(actasmpattern);

                             If is_asmopcode(actasmpattern) then
                             Begin
                                    gettoken := AS_OPCODE;
                                    exit;
                             end;
                             is_register(actasmpattern, token);
                             {is_asmoperator(actasmpattern,token);}
                             is_asmdirective(actasmpattern,token);
                             { if found }
                             if (token <> AS_NONE) then
                             begin
                               gettoken := token;
                               exit;
                             end
                             { this is surely an identifier }
                             else
                               token := AS_ID;
                             gettoken := token;
                             exit;
                          end;
           { override operator... not supported }
           '&':       begin
                         c:=current_scanner^.asmgetchar;
                         gettoken := AS_AND;
                      end;
           { string or character }
           '''' :
                      begin
                         actasmpattern:='';
                         while true do
                         begin
                           if c = '''' then
                           begin
                              c:=current_scanner^.asmgetchar;
                              if c=newline then
                              begin
                                 Message(scan_f_string_exceeds_line);
                                 break;
                              end;
                              repeat
                                  if c=''''then
                                   begin
                                       c:=current_scanner^.asmgetchar;
                                       if c='''' then
                                        begin
                                               actasmpattern:=actasmpattern+'''';
                                               c:=current_scanner^.asmgetchar;
                                               if c=newline then
                                               begin
                                                    Message(scan_f_string_exceeds_line);
                                                    break;
                                               end;
                                        end
                                        else break;
                                   end
                                   else
                                   begin
                                          actasmpattern:=actasmpattern+c;
                                          c:=current_scanner^.asmgetchar;
                                          if c=newline then
                                            begin
                                               Message(scan_f_string_exceeds_line);
                                               break
                                            end;
                                   end;
                              until false; { end repeat }
                           end
                           else break; { end if }
                         end; { end while }
                   token:=AS_STRING;
                   gettoken := token;
                   exit;
                 end;
           '$' :  begin
                    c:=current_scanner^.asmgetchar;
                    while c in ['0'..'9','A'..'F','a'..'f'] do
                    begin
                      actasmpattern := actasmpattern + c;
                      c := current_scanner^.asmgetchar;
                    end;
                   gettoken := AS_HEXNUM;
                   exit;
                  end;
           ',' : begin
                   gettoken := AS_COMMA;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           '(' : begin
                   gettoken := AS_LPAREN;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           ')' : begin
                   gettoken := AS_RPAREN;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           ':' : begin
                   gettoken := AS_COLON;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
{           '.' : begin
                   gettoken := AS_DOT;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end; }
           '+' : begin
                   gettoken := AS_PLUS;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           '-' : begin
                   gettoken := AS_MINUS;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           '*' : begin
                   gettoken := AS_STAR;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           '/' : begin
                   gettoken := AS_SLASH;
                   c:=current_scanner^.asmgetchar;
                   exit;
                 end;
           '<' : begin
                   c := current_scanner^.asmgetchar;
                   { invalid characters }
                   if c <> '<' then
                    Message(asmr_e_invalid_char_smaller);
                   { still assume << }
                   gettoken := AS_SHL;
                   c := current_scanner^.asmgetchar;
                   exit;
                 end;
           '>' : begin
                   c := current_scanner^.asmgetchar;
                   { invalid characters }
                   if c <> '>' then
                    Message(asmr_e_invalid_char_greater);
                   { still assume << }
                   gettoken := AS_SHR;
                   c := current_scanner^.asmgetchar;
                   exit;
                 end;
           '|' : begin
                   gettoken := AS_OR;
                   c := current_scanner^.asmgetchar;
                   exit;
                 end;
           '^' : begin
                  gettoken := AS_XOR;
                  c := current_scanner^.asmgetchar;
                  exit;
                 end;
           '#' : begin
                  gettoken:=AS_APPT;
                  c:=current_scanner^.asmgetchar;
                  exit;
                 end;
           '%' : begin
                   c:=current_scanner^.asmgetchar;
                   while c in ['0','1'] do
                   Begin
                     actasmpattern := actasmpattern + c;
                     c := current_scanner^.asmgetchar;
                   end;
                   gettoken := AS_BINNUM;
                   exit;
                 end;
           { integer number }
           '0'..'9': begin
                        actasmpattern := c;
                        c := current_scanner^.asmgetchar;
                        while c in ['0'..'9'] do
                          Begin
                             actasmpattern := actasmpattern + c;
                             c:= current_scanner^.asmgetchar;
                          end;
                        gettoken := AS_INTNUM;
                        exit;
                     end;
         ';' : begin
                  repeat
                     c:=current_scanner^.asmgetchar;
                  until c=newline;
                  firsttoken := TRUE;
                  gettoken:=AS_SEPARATOR;
               end;

         '{',#13,newline : begin
                            c:=current_scanner^.asmgetchar;
                            firsttoken := TRUE;
                            gettoken:=AS_SEPARATOR;
                           end;
            else
             Begin
               Message(scan_f_illegal_char);
             end;

      end; { end case }
    end; { end else if }
  end;


  {---------------------------------------------------------------------}
  {                     Routines for the parsing                        }
  {---------------------------------------------------------------------}

     procedure consume(t : tmotorolatoken);

     begin
       if t<>actasmtoken then
        Message(asmr_e_syntax_error);
       actasmtoken:=gettoken;
       { if the token must be ignored, then }
       { get another token to parse.        }
       if actasmtoken = AS_NONE then
          actasmtoken := gettoken;
      end;





   function findregister(const s : string): tregister;
  {*********************************************************************}
  { FUNCTION findregister(s: string):tasmop;                            }
  {  Description: Determines if the s string is a valid register,       }
  {  if so returns correct tregister token, or R_NO if not found.       }
  {*********************************************************************}
   var
    i: tregister;
   begin
     findregister := R_NO;
     for i:=firstasmreg to lastasmreg do
       if s = iasmregs[i] then
       Begin
         findregister := i;
         exit;
       end;
    if s = 'A7' then
    Begin
      findregister := R_SP;
      exit;
    end;
   end;


   function findopcode(s: string): tasmop;
  {*********************************************************************}
  { FUNCTION findopcode(s: string): tasmop;                             }
  {  Description: Determines if the s string is a valid opcode          }
  {  if so returns correct tasmop token.                                }
  {*********************************************************************}
   var
    i: tasmop;
    j: byte;
    op_size: string;
   Begin
     findopcode := A_NONE;
     j:=pos('.',s);
     if j<>0 then
     begin
       op_size:=copy(s,j+1,1);
       case op_size[1] of
       { For the motorola only opsize size is used to }
       { determine the size of the operands.             }
       'B': instr.opsize := S_B;
       'W': instr.opsize := S_W;
       'L': instr.opsize := S_L;
       'S': instr.opsize := S_FS;
       'D': instr.opsize := S_FL;
       'X': instr.opsize := S_FX;
       else
        Message1(asmr_e_unknown_opcode,s);
       end;
       { delete everything starting from dot }
       delete(s,j,length(s));
     end;
     for i:=firstop to lastop do
       if  s = iasmops^[i] then
       begin
          findopcode:=i;
          exit;
       end;
   end;

  Procedure InitAsmRef(var instr: TInstruction);
  {*********************************************************************}
  {  Description: This routine first check if the instruction is of     }
  {  type OPR_NONE, or OPR_REFERENCE , if not it gives out an error.    }
  {  If the operandtype = OPR_NONE or <> OPR_REFERENCE then it sets up  }
  {  the operand type to OPR_REFERENCE, as well as setting up the ref   }
  {  to point to the default segment.                                   }
  {*********************************************************************}
   Begin
     With instr do
     Begin
        case operands[operandnum]^.opr.typ of
          OPR_REFERENCE: exit;
          OPR_NONE: ;
        else
          Message(asmr_e_invalid_operand_type);
        end;
        operands[operandnum]^.opr.ref.direction := dir_none;
        operands[operandnum]^.opr.typ := OPR_REFERENCE;
        operands[operandnum]^.opr.ref.segment := R_DEFAULT_SEG;
     end;
   end;




  Function CalculateExpression(expression: string): longint;
  var
    expr: TExprParse;
  Begin
   expr.Init;
   CalculateExpression := expr.Evaluate(expression);
   expr.Done;
  end;


  Procedure ConcatOpCode(var instr: TInstruction);
  var
    fits : boolean;
    instruc: tasmop;
  Begin
     fits := FALSE;
    { setup specific instructions for first pass }
    instruc := instr.opcode;

    { Setup special operands }
    { Convert to general form as to conform to the m68k opcode table }
    if (instruc = A_ADDA) or (instruc = A_ADDI)
       then instruc := A_ADD
    else
    { CMPM excluded because of GAS v1.34 BUG }
    if (instruc = A_CMPA) or
       (instruc = A_CMPI) then
       instruc := A_CMP
    else
    if instruc = A_EORI then
      instruc := A_EOR
    else
    if instruc = A_MOVEA then
     instruc := A_MOVE
    else
    if instruc = A_ORI then
      instruc := A_OR
    else
    if (instruc = A_SUBA) or (instruc = A_SUBI) then
      instruc :=  A_SUB;

    { Setup operand types }

(*
    in instruc <> A_MOVEM then
    Begin

      while not(fits) do
        begin
         { set the instruction cache, if the instruction }
         { occurs the first time                         }
         if (it[i].i=instruc) and (ins_cache[instruc]=-1) then
             ins_cache[instruc]:=i;

         if (it[i].i=instruc) and (instr.ops=it[i].ops) then
         begin
            { first fit }
           case instr.ops of
             0 : begin
                   fits:=true;
                   break;
                end;
            1 :
                Begin
                  if (optyp1 and it[i].o1)<>0 then
                  Begin
                    fits:=true;
                     break;
                  end;
                end;
            2 : if ((optyp1 and it[i].o1)<>0) and
                 ((optyp2 and it[i].o2)<>0) then
                 Begin
                       fits:=true;
                       break;
                 end
            3 : if ((optyp1 and it[i].o1)<>0) and
                 ((optyp2 and it[i].o2)<>0) and
                 ((optyp3 and it[i].o3)<>0) then
                 Begin
                   fits:=true;
                   break;
                 end;
           end; { end case }
        end; { endif }
        if it[i].i=A_NONE then
        begin
          { NO MATCH! }
          Message(asmr_e_invalid_combination_opcode_and_operand);
          exit;
        end;
        inc(i);
       end; { end while }
             *)
  fits:=TRUE;

  { We add the opcode to the opcode linked list }
  if fits then
  Begin
    case instr.ops of

     0:
        if instr.opsize <> S_NO then
          p^.concat(new(paicpu,op_none(instruc,instr.opsize)))
        else
          p^.concat(new(paicpu,op_none(instruc,S_NO)));
     1: Begin
          case instr.operands[1]^.opr.typ of
           OPR_SYMBOL: Begin
                             p^.concat(new(paicpu,op_sym_ofs(instruc,
                               instr.opsize, instr.operands[1]^.opr.symbol,instr.operands[1]^.opr.symofs)));
                         end;
           OPR_CONSTANT: Begin
                             p^.concat(new(paicpu,op_const(instruc,
                               instr.opsize, instr.operands[1]^.opr.val)));
                         end;
           OPR_REGISTER:  p^.concat(new(paicpu,op_reg(instruc,
                            instr.opsize,instr.operands[1]^.opr.reg)));
           OPR_REFERENCE:
                          if instr.opsize <> S_NO then
                          Begin
                           p^.concat(new(paicpu,op_ref(instruc,
                            instr.opsize,newreference(instr.operands[1]^.opr.ref))));
                          end
                          else
                          Begin
                              { special jmp and call case with }
                              { symbolic references.           }
                              if instruc in [A_BSR,A_JMP,A_JSR,A_BRA,A_PEA] then
                              Begin
                                p^.concat(new(paicpu,op_ref(instruc,
                                  S_NO,newreference(instr.operands[1]^.opr.ref))));
                              end
                              else
                                Message(asmr_e_invalid_opcode_and_operand);
                          end;
           OPR_NONE: Begin
                       Message(asmr_e_invalid_opcode_and_operand);
                     end;
          else
           Begin
             Message(asmr_e_invalid_opcode_and_operand);
           end;
          end;
        end;
     2:
        Begin
                With instr do
                Begin
                { source }
                  case operands[1]^.opr.typ of
                  { reg,reg     }
                  { reg,ref     }
                   OPR_REGISTER:
                     Begin
                       case operands[2]^.opr.typ of
                         OPR_REGISTER:
                            Begin
                               p^.concat(new(paicpu,op_reg_reg(instruc,
                               opsize,operands[1]^.opr.reg,operands[2]^.opr.reg)));
                            end;
                         OPR_REFERENCE:
                                  p^.concat(new(paicpu,op_reg_ref(instruc,
                                  opsize,operands[1]^.opr.reg,newreference(operands[2]^.opr.ref))));
                       else { else case }
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                       end; { end second operand case for OPR_REGISTER }
                     end;
                  { reglist, ref }
                   OPR_REGLIST:
                          Begin
                            case operands[2]^.opr.typ of
                              OPR_REFERENCE :
                                  p^.concat(new(paicpu,op_reglist_ref(instruc,
                                  opsize,operands[1]^.opr.reglist^,newreference(operands[2]^.opr.ref))));
                            else
                             Begin
                               Message(asmr_e_invalid_opcode_and_operand);
                             end;
                            end; { end second operand case for OPR_REGLIST }
                          end;

                  { const,reg   }
                  { const,const }
                  { const,ref   }
                   OPR_CONSTANT:
                      case instr.operands[2]^.opr.typ of
                      { constant, constant does not have a specific size. }
                        OPR_CONSTANT:
                           p^.concat(new(paicpu,op_const_const(instruc,
                           S_NO,operands[1]^.opr.val,operands[2]^.opr.val)));
                        OPR_REFERENCE:
                           Begin
                                 p^.concat(new(paicpu,op_const_ref(instruc,
                                 opsize,operands[1]^.opr.val,
                                 newreference(operands[2]^.opr.ref))))
                           end;
                        OPR_REGISTER:
                           Begin
                                 p^.concat(new(paicpu,op_const_reg(instruc,
                                 opsize,operands[1]^.opr.val,
                                 operands[2]^.opr.reg)))
                           end;
                      else
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_CONSTANT }
                   { ref,reg     }
                   { ref,ref     }
                   OPR_REFERENCE:
                      case instr.operands[2]^.opr.typ of
                         OPR_REGISTER:
                            Begin
                              p^.concat(new(paicpu,op_ref_reg(instruc,
                               opsize,newreference(operands[1]^.opr.ref),
                               operands[2]^.opr.reg)));
                            end;
                         OPR_REGLIST:
                            Begin
                              p^.concat(new(paicpu,op_ref_reglist(instruc,
                               opsize,newreference(operands[1]^.opr.ref),
                               operands[2]^.opr.reglist^)));
                            end;
                         OPR_REFERENCE: { special opcodes }
                            p^.concat(new(paicpu,op_ref_ref(instruc,
                            opsize,newreference(operands[1]^.opr.ref),
                            newreference(operands[2]^.opr.ref))));
                      else
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_REFERENCE }
           OPR_SYMBOL: case operands[2]^.opr.typ of
                        OPR_REFERENCE:
                           Begin
                                 p^.concat(new(paicpu,op_sym_ofs_ref(instruc,
                                   opsize,instr.operands[1]^.opr.symbol,instr.operands[1]^.opr.symofs,
                                   newreference(operands[2]^.opr.ref))))
                           end;
                        OPR_REGISTER:
                           Begin
                                 p^.concat(new(paicpu,op_sym_ofs_reg(instruc,
                                   opsize,instr.operands[1]^.opr.symbol,instr.operands[1]^.opr.symofs,
                                   operands[2]^.opr.reg)))
                           end;
                      else
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_SYMBOL }
                  else
                     Begin
                       Message(asmr_e_invalid_opcode_and_operand);
                     end;
                  end; { end first operand case }
                end; { end with }
        end;
     3: Begin
           if (instruc = A_DIVSL) or (instruc = A_DIVUL) or (instruc = A_MULU)
           or (instruc = A_MULS) or (instruc = A_DIVS) or (instruc = A_DIVU) then
           Begin
             if (instr.operands[1]^.opr.typ <> OPR_REGISTER)
             or (instr.operands[2]^.opr.typ <> OPR_REGISTER)
             or (instr.operands[3]^.opr.typ <> OPR_REGISTER) then
             Begin
               Message(asmr_e_invalid_opcode_and_operand);
             end
             else
             Begin
               p^.concat(new(paicpu, op_reg_reg_reg(instruc,instr.opsize,
                 instr.operands[1]^.opr.reg,instr.operands[2]^.opr.reg,instr.operands[3]^.opr.reg)));
             end;
           end
           else
            Message(asmr_e_invalid_opcode_and_operand);
        end;
  end; { end case }
 end;
 end;


    Procedure ConcatLabeledInstr(var instr: TInstruction);
    Begin
       if ((instr.opcode >= A_BCC) and (instr.opcode <= A_BVS))
       or (instr.opcode = A_BRA) or (instr.opcode = A_BSR)
       or (instr.opcode = A_JMP) or (instr.opcode = A_JSR)
       or ((instr.opcode >= A_FBEQ) and (instr.opcode <= A_FBNGLE))
       then
       Begin
        if instr.ops > 2 then
          Message(asmr_e_invalid_opcode_and_operand)
        else if instr.operands[1]^.opr.typ <> OPR_SYMBOL then
          Message(asmr_e_invalid_opcode_and_operand)
        else if (instr.operands[1]^.opr.typ = OPR_SYMBOL) and
         (instr.ops = 1) then
           if assigned(instr.operands[1]^.opr.symbol) and
              (instr.operands[1]^.opr.symofs=0) then
             p^.concat(new(pai_labeled,init_sym(instr.opcode,
               instr.operands[1]^.opr.symbol)))
           else
            Message(asmr_e_invalid_opcode_and_operand);
       end
       else
       if ((instr.opcode >= A_DBCC) and (instr.opcode <= A_DBF))
       or ((instr.opcode >= A_FDBEQ) and (instr.opcode <= A_FBDNGLE)) then
       begin
         if (instr.ops<>2) or
            (instr.operands[1]^.opr.typ <> OPR_REGISTER) or
            (instr.operands[2]^.opr.typ <> OPR_SYMBOL) or
            (instr.operands[2]^.opr.symofs <> 0) then
           Message(asmr_e_invalid_opcode_and_operand)
         else
          p^.concat(new(pai_labeled,init_reg_sym(instr.opcode,
           instr.operands[2]^.opr.symbol,instr.operands[1]^.opr.reg)));
       end
       else
        Message(asmr_e_invalid_opcode_and_operand);
    end;

    Function BuildExpression(allow_symbol : boolean; asmsym : pstring) : longint;
  {*********************************************************************}
  { FUNCTION BuildExpression: longint                                   }
  {  Description: This routine calculates a constant expression to      }
  {  a given value. The return value is the value calculated from       }
  {  the expression.                                                    }
  { The following tokens (not strings) are recognized:                  }
  {    (,),SHL,SHR,/,*,NOT,OR,XOR,AND,MOD,+/-,numbers,ID to constants.  }
  {*********************************************************************}
  { ENTRY: On entry the token should be any valid expression token.     }
  { EXIT:  On Exit the token points to either COMMA or SEPARATOR        }
  { ERROR RECOVERY: Tries to find COMMA or SEPARATOR token by consuming }
  {  invalid tokens.                                                    }
  {*********************************************************************}
  var expr: string;
      hs, tempstr: string;
      sym : psym;
      hl : pasmlabel;
      l : longint;
      errorflag: boolean;
  Begin
    errorflag := FALSE;
    expr := '';
    tempstr := '';
    if allow_symbol then
      asmsym^:='';
    Repeat
      Case actasmtoken of
      AS_LPAREN: Begin
                  Consume(AS_LPAREN);
                  expr := expr + '(';
                end;
      AS_RPAREN: Begin
                  Consume(AS_RPAREN);
                  expr := expr + ')';
                end;
      AS_SHL:    Begin
                  Consume(AS_SHL);
                  expr := expr + '<';
                end;
      AS_SHR:    Begin
                  Consume(AS_SHR);
                  expr := expr + '>';
                end;
      AS_SLASH:  Begin
                  Consume(AS_SLASH);
                  expr := expr + '/';
                end;
      AS_MOD:    Begin
                  Consume(AS_MOD);
                  expr := expr + '%';
                end;
      AS_STAR:   Begin
                  Consume(AS_STAR);
                  expr := expr + '*';
                end;
      AS_PLUS:   Begin
                  Consume(AS_PLUS);
                  expr := expr + '+';
                end;
      AS_MINUS:  Begin
                  Consume(AS_MINUS);
                  expr := expr + '-';
                end;
      AS_AND:    Begin
                  Consume(AS_AND);
                  expr := expr + '&';
                end;
      AS_NOT:    Begin
                  Consume(AS_NOT);
                  expr := expr + '~';
                end;
      AS_XOR:    Begin
                  Consume(AS_XOR);
                  expr := expr + '^';
                end;
      AS_OR:     Begin
                  Consume(AS_OR);
                  expr := expr + '|';
                end;
      AS_ID:    Begin
                  if SearchIConstant(actasmpattern,l) then
                  Begin
                    str(l, tempstr);
                    expr := expr + tempstr;
                    Consume(AS_ID);
                  End else
                  if not allow_symbol then
                  Begin
                    Message(asmr_e_syn_constant);
                    l := 0;
                  End else
                  Begin
                    hs:='';
                    if (expr[Length(expr)]='+') then
                      Delete(expr,Length(expr),1)
                    else if expr<>'' then
                      Begin
                        Message(asmr_e_invalid_constant_expression);
                        break;
                      End;
                    tempstr:=actasmpattern;
                    consume(AS_ID);
                    if (length(tempstr)>1) and (tempstr[1]='@') then
                      begin
                        CreateLocalLabel(tempstr,hl,false);
                        hs:=hl^.name
                      end
                    else if SearchLabel(tempstr,hl,false) then
                      hs:=hl^.name
                    else
                      begin
                        getsym(tempstr,false);
                        sym:=srsym;
                        if assigned(sym) then
                          begin
                             case srsym^.typ of
                               varsym :
                                 begin
                                   if sym^.owner^.symtabletype in [localsymtable,parasymtable] then
                                    Message(asmr_e_no_local_or_para_allowed);
                                   hs:=pvarsym(srsym)^.mangledname;
                                 end;
                               typedconstsym :
                                 hs:=ptypedconstsym(srsym)^.mangledname;
                               procsym :
                                 hs:=pprocsym(srsym)^.mangledname;
                               else
                                 Message(asmr_e_wrong_sym_type);
                             end;
                          end
                        else
                           Message1(sym_e_unknown_id,tempstr);
                      end;
                     { symbol found? }
                     if hs<>'' then
                      begin
                        if asmsym^='' then
                         asmsym^:=hs
                        else
                         Message(asmr_e_cant_have_multiple_relocatable_symbols);
                      end;
                  end;
                end;
      AS_INTNUM:  Begin
                   expr := expr + actasmpattern;
                   Consume(AS_INTNUM);
                  end;
      AS_BINNUM:  Begin
                      tempstr := tostr(ValBinary(actasmpattern));
                      if tempstr = '' then
                       Message(asmr_e_error_converting_binary);
                      expr:=expr+tempstr;
                      Consume(AS_BINNUM);
                  end;

      AS_HEXNUM: Begin
                    tempstr := tostr(ValHexadecimal(actasmpattern));
                    if tempstr = '' then
                     Message(asmr_e_error_converting_hexadecimal);
                    expr:=expr+tempstr;
                    Consume(AS_HEXNUM);
                end;
      AS_OCTALNUM: Begin
                    tempstr := tostr(ValOctal(actasmpattern));
                    if tempstr = '' then
                     Message(asmr_e_error_converting_octal);
                    expr:=expr+tempstr;
                    Consume(AS_OCTALNUM);
                  end;
      { go to next term }
      AS_COMMA: Begin
                  if not ErrorFlag then
                    BuildExpression := CalculateExpression(expr)
                  else
                    BuildExpression := 0;
                  Exit;
               end;
      { go to next symbol }
      AS_SEPARATOR: Begin
                      if not ErrorFlag then
                        BuildExpression := CalculateExpression(expr)
                      else
                        BuildExpression := 0;
                      Exit;
                   end;
      else
        Begin
          { only write error once. }
          if not errorflag then
           Message(asmr_e_invalid_constant_expression);
          { consume tokens until we find COMMA or SEPARATOR }
          Consume(actasmtoken);
          errorflag := TRUE;
        End;
      end;
    Until false;
  end;


  Procedure BuildRealConstant(typ : tfloattype);
  {*********************************************************************}
  { PROCEDURE BuilRealConst                                             }
  {  Description: This routine calculates a constant expression to      }
  {  a given value. The return value is the value calculated from       }
  {  the expression.                                                    }
  { The following tokens (not strings) are recognized:                  }
  {    +/-,numbers and real numbers                                     }
  {*********************************************************************}
  { ENTRY: On entry the token should be any valid expression token.     }
  { EXIT:  On Exit the token points to either COMMA or SEPARATOR        }
  { ERROR RECOVERY: Tries to find COMMA or SEPARATOR token by consuming }
  {  invalid tokens.                                                    }
  {*********************************************************************}
  var expr: string;
      r : extended;
      code : word;
      negativ : boolean;
      errorflag: boolean;
  Begin
    errorflag := FALSE;
    Repeat
    negativ:=false;
    expr := '';
    if actasmtoken=AS_PLUS then Consume(AS_PLUS)
    else if actasmtoken=AS_MINUS then
      begin
         negativ:=true;
         consume(AS_MINUS);
      end;
    Case actasmtoken of
      AS_INTNUM:  Begin
                   expr := actasmpattern;
                   Consume(AS_INTNUM);
                 end;
      AS_REALNUM:  Begin
                   expr := actasmpattern;
                   { in ATT syntax you have 0d in front of the real }
                   { should this be forced ?  yes i think so, as to }
                   { conform to gas as much as possible.            }
                   if (expr[1]='0') and (upper(expr[2])='D') then
                     expr:=copy(expr,3,255);
                   Consume(AS_REALNUM);
                 end;
      AS_BINNUM:  Begin
                      { checking for real constants with this should use  }
                      { real DECODING otherwise the compiler will crash!  }
                      Message(asmr_e_invalid_float_expr);
                      expr:='0.0';
                      Consume(AS_BINNUM);
                 end;

      AS_HEXNUM: Begin
                      { checking for real constants with this should use  }
                      { real DECODING otherwise the compiler will crash!  }
                    Message(asmr_e_invalid_float_expr);
                    expr:='0.0';
                    Consume(AS_HEXNUM);
                end;
      AS_OCTALNUM: Begin
                      { checking for real constants with this should use    }
                      { real DECODING otherwise the compiler will crash!    }
                      { xxxToDec using reals could be a solution, but the   }
                      { problem is that these will crash the m68k compiler  }
                      { when compiling -- because of lack of good fpu       }
                      { support.                                           }
                    Message(asmr_e_invalid_float_expr);
                    expr:='0.0';
                    Consume(AS_OCTALNUM);
                  end;
         else
           Begin
             { only write error once. }
             if not errorflag then
              Message(asmr_e_invalid_float_expr);
             { consume tokens until we find COMMA or SEPARATOR }
             Consume(actasmtoken);
             errorflag := TRUE;
           End;

         end;
      { go to next term }
      if (actasmtoken=AS_COMMA) or (actasmtoken=AS_SEPARATOR) then
        Begin
          if negativ then expr:='-'+expr;
          val(expr,r,code);
          if code<>0 then
            Begin
               r:=0;
               Message(asmr_e_invalid_float_expr);
               ConcatRealConstant(p,r,typ);
            End
          else
            Begin
              ConcatRealConstant(p,r,typ);
            End;
        end
      else
        Message(asmr_e_invalid_float_expr);
    Until actasmtoken=AS_SEPARATOR;
  end;



  Procedure BuildScaling(Var instr: TInstruction);
  {*********************************************************************}
  {  Takes care of parsing expression starting from the scaling value   }
  {  up to and including possible field specifiers.                     }
  { EXIT CONDITION:  On exit the routine should point to  AS_SEPARATOR  }
  { or AS_COMMA. On entry should point to the AS_STAR  token.           }
  {*********************************************************************}
  var str:string;
      l: longint;
      code: integer;
  Begin
     Consume(AS_STAR);
     if (instr.operands[operandnum]^.opr.ref.scalefactor <> 0)
     and (instr.operands[operandnum]^.opr.ref.scalefactor <> 1) then
      Message(asmr_e_wrong_base_index);
     case actasmtoken of
        AS_INTNUM: str := actasmpattern;
        AS_HEXNUM: str := Tostr(ValHexadecimal(actasmpattern));
        AS_BINNUM: str := Tostr(ValBinary(actasmpattern));
        AS_OCTALNUM: str := Tostr(ValOctal(actasmpattern));
     else
        Message(asmr_e_syntax_error);
     end;
     val(str, l, code);
     if code <> 0 then
      Message(asmr_e_wrong_scale_factor);
     if ((l = 2) or (l = 4) or (l = 8) or (l = 1)) and (code = 0) then
     begin
        instr.operands[operandnum]^.opr.ref.scalefactor := l;
     end
     else
     Begin
        Message(asmr_e_wrong_scale_factor);
        instr.operands[operandnum]^.opr.ref.scalefactor := 0;
     end;
     if instr.operands[operandnum]^.opr.ref.index = R_NO then
     Begin
        Message(asmr_e_wrong_base_index);
        instr.operands[operandnum]^.opr.ref.scalefactor := 0;
     end;
    { Consume the scaling number }
    Consume(actasmtoken);
    if actasmtoken = AS_RPAREN then
        Consume(AS_RPAREN)
    else
       Message(asmr_e_wrong_scale_factor);
    { // .Field.Field ... or separator/comma // }
    if actasmtoken in [AS_COMMA,AS_SEPARATOR] then
    Begin
    end
    else
     Message(asmr_e_syntax_error);
  end;


  Function BuildRefExpression: longint;
  {*********************************************************************}
  { FUNCTION BuildExpression: longint                                   }
  {  Description: This routine calculates a constant expression to      }
  {  a given value. The return value is the value calculated from       }
  {  the expression.                                                    }
  { The following tokens (not strings) are recognized:                  }
  {    SHL,SHR,/,*,NOT,OR,XOR,AND,MOD,+/-,numbers,ID to constants.      }
  {*********************************************************************}
  { ENTRY: On entry the token should be any valid expression token.     }
  { EXIT:  On Exit the token points to the LPAREN token.                }
  { ERROR RECOVERY: Tries to find COMMA or SEPARATOR token by consuming }
  {  invalid tokens.                                                    }
  {*********************************************************************}
  var tempstr: string;
      expr: string;
    l : longint;
    errorflag : boolean;
  Begin
    errorflag := FALSE;
    tempstr := '';
    expr := '';
    Repeat
      Case actasmtoken of
      AS_RPAREN: Begin
                   Message(asmr_e_syntax_error);
                  Consume(AS_RPAREN);
                end;
      AS_SHL:    Begin
                  Consume(AS_SHL);
                  expr := expr + '<';
                end;
      AS_SHR:    Begin
                  Consume(AS_SHR);
                  expr := expr + '>';
                end;
      AS_SLASH:  Begin
                  Consume(AS_SLASH);
                  expr := expr + '/';
                end;
      AS_MOD:    Begin
                  Consume(AS_MOD);
                  expr := expr + '%';
                end;
      AS_STAR:   Begin
                  Consume(AS_STAR);
                  expr := expr + '*';
                end;
      AS_PLUS:   Begin
                  Consume(AS_PLUS);
                  expr := expr + '+';
                end;
      AS_MINUS:  Begin
                  Consume(AS_MINUS);
                  expr := expr + '-';
                end;
      AS_AND:    Begin
                  Consume(AS_AND);
                  expr := expr + '&';
                end;
      AS_NOT:    Begin
                  Consume(AS_NOT);
                  expr := expr + '~';
                end;
      AS_XOR:    Begin
                  Consume(AS_XOR);
                  expr := expr + '^';
                end;
      AS_OR:     Begin
                  Consume(AS_OR);
                  expr := expr + '|';
                end;
      { End of reference }
      AS_LPAREN: Begin
                     if not ErrorFlag then
                        BuildRefExpression := CalculateExpression(expr)
                     else
                        BuildRefExpression := 0;
                     { no longer in an expression }
                     exit;
                  end;
      AS_ID:
                Begin
                  if NOT SearchIConstant(actasmpattern,l) then
                  Begin
                    Message(asmr_e_syn_constant);
                    l := 0;
                  end;
                  str(l, tempstr);
                  expr := expr + tempstr;
                  Consume(AS_ID);
                end;
      AS_INTNUM:  Begin
                   expr := expr + actasmpattern;
                   Consume(AS_INTNUM);
                 end;
      AS_BINNUM:  Begin
                      tempstr := Tostr(ValBinary(actasmpattern));
                      if tempstr = '' then
                       Message(asmr_e_error_converting_binary);
                      expr:=expr+tempstr;
                      Consume(AS_BINNUM);
                 end;

      AS_HEXNUM: Begin
                    tempstr := Tostr(ValHexadecimal(actasmpattern));
                    if tempstr = '' then
                     Message(asmr_e_error_converting_hexadecimal);
                    expr:=expr+tempstr;
                    Consume(AS_HEXNUM);
                end;
      AS_OCTALNUM: Begin
                    tempstr := Tostr(ValOctal(actasmpattern));
                    if tempstr = '' then
                     Message(asmr_e_error_converting_octal);
                    expr:=expr+tempstr;
                    Consume(AS_OCTALNUM);
                  end;
      else
        Begin
          { write error only once. }
          if not errorflag then
           Message(asmr_e_invalid_constant_expression);
          BuildRefExpression := 0;
          if actasmtoken in [AS_COMMA,AS_SEPARATOR] then exit;
          { consume tokens until we find COMMA or SEPARATOR }
          Consume(actasmtoken);
          errorflag := TRUE;
        end;
      end;
    Until false;
  end;


  Procedure BuildReference(var Instr: TInstruction);
  {*********************************************************************}
  { PROCEDURE BuildBracketExpression                                    }
  {  Description: This routine builds up an expression after a LPAREN   }
  {  token is encountered.                                              }
  {   On entry actasmtoken should be equal to AS_LPAREN                 }
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to either the     }
  {       AS_COMMA or AS_SEPARATOR token.                               }
  {*********************************************************************}
  var
    l:longint;
    code: integer;
    str: string;
  Begin
     Consume(AS_LPAREN);
     Case actasmtoken of
        { // (reg ... // }
        AS_REGISTER: Begin
                        instr.operands[operandnum]^.opr.ref.base :=
                           findregister(actasmpattern);
                        Consume(AS_REGISTER);
                        { can either be a register or a right parenthesis }
                         { // (reg)       // }
                         { // (reg)+      // }
                         if actasmtoken=AS_RPAREN then
                         Begin
                           Consume(AS_RPAREN);
                           if actasmtoken = AS_PLUS then
                           Begin
                             if (instr.operands[operandnum]^.opr.ref.direction <> dir_none) then
                              Message(asmr_e_no_inc_and_dec_together)
                             else
                               instr.operands[operandnum]^.opr.ref.direction := dir_inc;
                             Consume(AS_PLUS);
                           end;
                           if not (actasmtoken in [AS_COMMA,AS_SEPARATOR]) then
                             Begin
                               Message(asmr_e_invalid_reference_syntax);
                               { error recovery ... }
                               while actasmtoken <> AS_SEPARATOR do
                                  Consume(actasmtoken);
                             end;
                             exit;
                         end;
                       { // (reg,reg .. // }
                       Consume(AS_COMMA);
                       if actasmtoken = AS_REGISTER then
                       Begin
                         instr.operands[operandnum]^.opr.ref.index :=
                           findregister(actasmpattern);
                         Consume(AS_REGISTER);
                         { check for scaling ... }
                         case actasmtoken of
                           AS_RPAREN:
                              Begin
                                Consume(AS_RPAREN);
                                if not (actasmtoken in [AS_COMMA,AS_SEPARATOR]) then
                                Begin
                                { error recovery ... }
                                  Message(asmr_e_invalid_reference_syntax);
                                  while actasmtoken <> AS_SEPARATOR do
                                    Consume(actasmtoken);
                                end;
                                exit;
                              end;
                           AS_STAR:
                              Begin
                                BuildScaling(instr);
                              end;
                         else
                           Begin
                             Message(asmr_e_invalid_reference_syntax);
                             while (actasmtoken <> AS_SEPARATOR) do
                               Consume(actasmtoken);
                           end;
                         end; { end case }
                       end
                       else
                          Begin
                             Message(asmr_e_invalid_reference_syntax);
                            while (actasmtoken <> AS_SEPARATOR) do
                                Consume(actasmtoken);
                          end;
                     end;
       AS_HEXNUM,AS_OCTALNUM,   { direct address }
       AS_BINNUM,AS_INTNUM: Begin
                                case actasmtoken of
                                        AS_INTNUM: str := actasmpattern;
                                        AS_HEXNUM: str := Tostr(ValHexadecimal(actasmpattern));
                                        AS_BINNUM: str := Tostr(ValBinary(actasmpattern));
                                        AS_OCTALNUM: str := Tostr(ValOctal(actasmpattern));
                                else
                                        Message(asmr_e_syntax_error);
                                end;
                                Consume(actasmtoken);
                                val(str, l, code);
                                if code <> 0 then
                                     Message(asmr_e_invalid_reference_syntax)
                                else
                                     instr.operands[operandnum]^.opr.ref.offset := l;
                                Consume(AS_RPAREN);
                                if not (actasmtoken in [AS_COMMA,AS_SEPARATOR]) then
                                Begin
                                      { error recovery ... }
                                      Message(asmr_e_invalid_reference_syntax);
                                      while actasmtoken <> AS_SEPARATOR do
                                        Consume(actasmtoken);
                                end;
                                exit;
                            end;
     else
       Begin

         Message(asmr_e_invalid_reference_syntax);
         while (actasmtoken <> AS_SEPARATOR) do
           Consume(actasmtoken);
       end;
     end; { end case }
  end;


  Procedure BuildOperand(var instr: TInstruction);
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to either the     }
  {       AS_COMMA or AS_SEPARATOR token.                               }
  {*********************************************************************}
  var
    tempstr: string;
    expr: string;
    lab: Pasmlabel;
    l : longint;
    i: tregister;
    hl: pasmlabel;
    reg_one, reg_two: tregister;
    reglist: set of tregister;
  Begin
   reglist := [];
   tempstr := '';
   expr := '';
   case actasmtoken of
   { // Memory reference //  }
     AS_LPAREN:
               Begin
                  initAsmRef(instr);
                  BuildReference(instr);
               end;
   { // Constant expression //  }
     AS_APPT:  Begin
                      Consume(AS_APPT);
                      if not (instr.operands[operandnum]^.opr.typ in [OPR_NONE,OPR_CONSTANT]) then
                         Message(asmr_e_invalid_operand_type);
                      { identifiers are handled by BuildExpression }
                      instr.operands[operandnum]^.opr.typ := OPR_CONSTANT;
                      instr.operands[operandnum]^.opr.val :=BuildExpression(true,@tempstr);
                      if tempstr<>'' then
                        begin
                          l:=instr.operands[operandnum]^.opr.val;
                          instr.operands[operandnum]^.opr.typ := OPR_SYMBOL;
                          instr.operands[operandnum]^.opr.symofs := l;
                          instr.operands[operandnum]^.opr.symbol := current_library.newasmsymbol(tempstr);
                        end;
                 end;
   { // Constant memory offset .              // }
   { // This must absolutely be followed by ( // }
     AS_HEXNUM,AS_INTNUM,
     AS_BINNUM,AS_OCTALNUM,AS_PLUS:
                   Begin
                      InitAsmRef(instr);
                      instr.operands[operandnum]^.opr.ref.offset:=BuildRefExpression;
                      BuildReference(instr);
                   end;
   { // A constant expression, or a Variable ref. // }
     AS_ID:  Begin
              if actasmpattern[1] = '@' then
              { // Label or Special symbol reference // }
              Begin
                 if actasmpattern = '@RESULT' then
                   Begin
                      InitAsmRef(instr);
                      instr.operands[operandnum]^.SetUpResult;
                   end
                 else
                  if (actasmpattern = '@CODE') or (actasmpattern = '@DATA') then
                    Message(asmr_w_CODE_and_DATA_not_supported)
                   else
                  Begin
                    delete(actasmpattern,1,1);
                    if actasmpattern = '' then
                     Message(asmr_e_null_label_ref_not_allowed);
                    CreateLocalLabel(actasmpattern,lab,false);
                    instr.operands[operandnum]^.opr.typ := OPR_SYMBOL;
                    instr.operands[operandnum]^.opr.symbol := lab;
                    instr.operands[operandnum]^.opr.symofs := 0;
                    instr.labeled := TRUE;
                  end;
                Consume(AS_ID);
                if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                 Message(asmr_e_syntax_error);
              end
              { probably a variable or normal expression }
              { or a procedure (such as in CALL ID)      }
              else
               Begin
                   { is it a constant ? }
                   if SearchIConstant(actasmpattern,l) then
                   Begin
                      InitAsmRef(instr);
                      instr.operands[operandnum]^.opr.ref.offset:=BuildRefExpression;
                      BuildReference(instr);

{                      if not (instr.operands[operandnum].opr.typ in [OPR_NONE,OPR_CONSTANT]) then
                        Message(asmr_e_invalid_operand_type);
                      instr.operands[operandnum].opr.typ := OPR_CONSTANT;
                      instr.operands[operandnum].val :=BuildExpression;}
                    end
                   else { is it a label variable ? }
                    Begin
                     { // ID[ , ID.Field.Field or simple ID // }
                     { check if this is a label, if so then }
                     { emit it as a label.                  }
                     if SearchLabel(actasmpattern,hl,false) then
                     Begin
                        instr.operands[operandnum]^.opr.typ := OPR_SYMBOL;
                        instr.operands[operandnum]^.opr.symbol := hl;
                        instr.operands[operandnum]^.opr.symofs := 0;
                        instr.labeled := TRUE;
                        Consume(AS_ID);
                        if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                         Message(asmr_e_syntax_error);
                     end
                     else
                     { is it a normal variable ? }
                     Begin
                      initAsmRef(instr);
                      if not instr.operands[operandnum]^.SetUpVar(actasmpattern,false) then
                      Begin
                         { not a variable.. }
                         { check special variables.. }
                         if actasmpattern = 'SELF' then
                          { special self variable }
                         Begin
                           if assigned(procinfo^._class) then
                             Begin
                               instr.operands[operandnum]^.opr.ref.offset := procinfo^.selfpointer_offset;
                               instr.operands[operandnum]^.opr.ref.base := procinfo^.framepointer;
                             end
                           else
                             Message(asmr_e_cannot_use_SELF_outside_a_method);
                         end
                         else
                         if (cs_compilesystem in aktmoduleswitches) then
                         Begin
                           if not assigned(instr.operands[operandnum]^.opr.ref.symbol) then
                            Begin
                             if getasmsymbol(actasmpattern) =nil then
                               Message1(asmr_w_id_supposed_external,actasmpattern);
                             instr.operands[operandnum]^.opr.ref.symbol:=current_library.newasmsymbol(actasmpattern);
                            end
                           else
                             Message(asmr_e_syntax_error);
                         end
                         else
                           Message1(asmr_e_unknown_label_identifier,actasmpattern);
                      end;
                      expr := actasmpattern;
                      Consume(AS_ID);
                      case actasmtoken of
                           AS_LPAREN: { indexing }
                                        BuildReference(instr);
                           AS_SEPARATOR,AS_COMMA: ;
                      else
                           Message(asmr_e_syntax_error);
                      end;
                     end;
                    end;
               end;
            end;
   { // Pre-decrement mode reference or constant mem offset.   // }
     AS_MINUS:    Begin
                   Consume(AS_MINUS);
                   if actasmtoken = AS_LPAREN then
                   Begin
                     InitAsmRef(instr);
                     { indicate pre-decrement mode }
                     instr.operands[operandnum]^.opr.ref.direction := dir_dec;
                     BuildReference(instr);
                   end
                   else
                   if actasmtoken in [AS_OCTALNUM,AS_HEXNUM,AS_BINNUM,AS_INTNUM] then
                   Begin
                      InitAsmRef(instr);
                      instr.operands[operandnum]^.opr.ref.offset:=BuildRefExpression;
                      { negate because was preceded by a negative sign! }
                      instr.operands[operandnum]^.opr.ref.offset:=-instr.operands[operandnum]^.opr.ref.offset;
                      BuildReference(instr);
                   end
                   else
                   Begin
                    Message(asmr_e_syntax_error);
                    while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                       Consume(actasmtoken);
                   end;
                  end;
   { // Register, a variable reference or a constant reference // }
     AS_REGISTER: Begin
                   { save the type of register used. }
                   tempstr := actasmpattern;
                   Consume(AS_REGISTER);
                   { // Simple register // }
                   if (actasmtoken = AS_SEPARATOR) or (actasmtoken = AS_COMMA) then
                   Begin
                        if not (instr.operands[operandnum]^.opr.typ in [OPR_NONE,OPR_REGISTER]) then
                         Message(asmr_e_invalid_operand_type);
                        instr.operands[operandnum]^.opr.typ := OPR_REGISTER;
                        instr.operands[operandnum]^.opr.reg := findregister(tempstr);
                   end
                   else
                   { HERE WE MUST HANDLE THE SPECIAL CASE OF MOVEM AND FMOVEM }
                   { // Individual register listing // }
                   if (actasmtoken = AS_SLASH) then
                   Begin
                     reglist := [findregister(tempstr)];
                     Consume(AS_SLASH);
                     if actasmtoken = AS_REGISTER then
                     Begin
                       While not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                       Begin
                         case actasmtoken of
                          AS_REGISTER: Begin
                                        reglist := reglist + [findregister(actasmpattern)];
                                        Consume(AS_REGISTER);
                                       end;
                          AS_SLASH: Consume(AS_SLASH);
                          AS_SEPARATOR,AS_COMMA: break;
                         else
                          Begin
                            Message(asmr_e_invalid_reg_list_in_movem);
                            Consume(actasmtoken);
                          end;
                         end; { end case }
                       end; { end while }
                       instr.operands[operandnum]^.opr.typ:= OPR_REGLIST;
                       instr.operands[operandnum]^.opr.reglist := newreglist(reglist);
                     end
                     else
                      { error recovery ... }
                      Begin
                            Message(asmr_e_invalid_reg_list_in_movem);
                            while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                               Consume(actasmtoken);
                      end;
                   end
                   else
                   { // Range register listing // }
                   if (actasmtoken = AS_MINUS) then
                   Begin
                     Consume(AS_MINUS);
                     reg_one:=findregister(tempstr);
                     if actasmtoken <> AS_REGISTER then
                     Begin
                       Message(asmr_e_invalid_reg_list_in_movem);
                       while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                         Consume(actasmtoken);
                     end
                     else
                     Begin
                      { determine the register range ... }
                      reg_two:=findregister(actasmpattern);
                      if reg_one > reg_two then
                      begin
                       for i:=reg_two to reg_one do
                         reglist := reglist + [i];
                      end
                      else
                      Begin
                       for i:=reg_one to reg_two do
                         reglist := reglist + [i];
                      end;
                      Consume(AS_REGISTER);
                      if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                      Begin
                       Message(asmr_e_invalid_reg_list_in_movem);
                       while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                         Consume(actasmtoken);
                      end;
                      { set up instruction }
                      instr.operands[operandnum]^.opr.typ:= OPR_REGLIST;
                      instr.operands[operandnum]^.opr.reglist := newreglist(reglist);
                     end;
                   end
                   else
                   { DIVSL/DIVS/MULS/MULU with long for MC68020 only }
                   if (actasmtoken = AS_COLON) then
                   Begin
                     if (aktoptprocessor = MC68020) or (cs_compilesystem in aktmoduleswitches) then
                     Begin
                       Consume(AS_COLON);
                       if (actasmtoken = AS_REGISTER) then
                       Begin
                         { set up old field, since register is valid }
                         instr.operands[operandnum]^.opr.typ := OPR_REGISTER;
                         instr.operands[operandnum]^.opr.reg := findregister(tempstr);
                         Inc(operandnum);
                         instr.operands[operandnum]^.opr.typ := OPR_REGISTER;
                         instr.operands[operandnum]^.opr.reg := findregister(actasmpattern);
                         Consume(AS_REGISTER);
                         if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                         Begin
                          Message(asmr_e_invalid_reg_list_for_opcode);
                          while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                            Consume(actasmtoken);
                         end;
                       end;
                     end
                     else
                     Begin
                        Message(asmr_e_68020_mode_required);
                        if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                        Begin
                          Message(asmr_e_invalid_reg_list_for_opcode);
                          while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                            Consume(actasmtoken);
                        end;
                     end;
                   end
                   else
                    Message(asmr_e_invalid_register);
                 end;
     AS_SEPARATOR, AS_COMMA: ;
    else
     Begin
      Message(asmr_e_invalid_opcode_and_operand);
      Consume(actasmtoken);
     end;
  end; { end case }
 end;



  Procedure BuildConstant(maxvalue: longint);
  {*********************************************************************}
  { PROCEDURE BuildConstant                                             }
  {  Description: This routine takes care of parsing a DB,DD,or DW      }
  {  line and adding those to the assembler node. Expressions, range-   }
  {  checking are fullly taken care of.                                 }
  {   maxvalue: $ff -> indicates that this is a DB node.                }
  {             $ffff -> indicates that this is a DW node.              }
  {             $ffffffff -> indicates that this is a DD node.          }
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to AS_SEPARATOR.  }
  {*********************************************************************}
  var
   strlength: byte;
   expr: string;
   tempstr: string;
   value : longint;
  Begin
      Repeat
        Case actasmtoken of
          AS_STRING: Begin
                      if maxvalue = $ff then
                         strlength := 1
                      else
                         Message(asmr_e_string_not_allowed_as_const);
                      expr := actasmpattern;
                      if length(expr) > 1 then
                       Message(asmr_e_string_not_allowed_as_const);
                      Consume(AS_STRING);
                      Case actasmtoken of
                       AS_COMMA: Consume(AS_COMMA);
                       AS_SEPARATOR: ;
                      else
                       Message(asmr_e_invalid_string_expression);
                      end; { end case }
                      ConcatString(p,expr);
                    end;
          AS_INTNUM,AS_BINNUM,
          AS_OCTALNUM,AS_HEXNUM:
                    Begin
                      value:=BuildExpression(false,nil);
                      ConcatConstant(p,value,maxvalue);
                    end;
          AS_ID:
                     Begin
                      value:=BuildExpression(false,nil);
                      if value > maxvalue then
                      Begin
                         Message(asmr_e_constant_out_of_bounds);
                         { assuming a value of maxvalue }
                         value := maxvalue;
                      end;
                      ConcatConstant(p,value,maxvalue);
                  end;
          { These terms can start an assembler expression }
          AS_PLUS,AS_MINUS,AS_LPAREN,AS_NOT: Begin
                                          value := BuildExpression(false,nil);
                                          ConcatConstant(p,value,maxvalue);
                                         end;
          AS_COMMA:  BEGIN
                       Consume(AS_COMMA);
                     END;
          AS_SEPARATOR: ;

        else
         Begin
           Message(asmr_e_syntax_error);
         end;
    end; { end case }
   Until actasmtoken = AS_SEPARATOR;
  end;


  Procedure BuildStringConstant(asciiz: boolean);
  {*********************************************************************}
  { PROCEDURE BuildStringConstant                                       }
  {  Description: Takes care of a ASCII, or ASCIIZ directive.           }
  {   asciiz: boolean -> if true then string will be null terminated.   }
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to AS_SEPARATOR.  }
  { On ENTRY: Token should point to AS_STRING                           }
  {*********************************************************************}
  var
   expr: string;
   errorflag : boolean;
  Begin
      errorflag := FALSE;
      Repeat
        Case actasmtoken of
          AS_STRING: Begin
                      expr:=actasmpattern;
                      if asciiz then
                       expr:=expr+#0;
                      ConcatPasString(p,expr);
                      Consume(AS_STRING);
                    end;
          AS_COMMA:  BEGIN
                       Consume(AS_COMMA);
                     END;
          AS_SEPARATOR: ;
        else
         Begin
          Consume(actasmtoken);
          if not errorflag then
           Message(asmr_e_invalid_string_expression);
          errorflag := TRUE;
         end;
    end; { end case }
   Until actasmtoken = AS_SEPARATOR;
  end;




  Procedure BuildOpCode;
  {*********************************************************************}
  { PROCEDURE BuildOpcode;                                              }
  {  Description: Parses the intel opcode and operands, and writes it   }
  {  in the TInstruction object.                                        }
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to AS_SEPARATOR.  }
  { On ENTRY: Token should point to AS_OPCODE                           }
  {*********************************************************************}
  var asmtok: tasmop;
      op: tasmop;
      expr: string;
      segreg: tregister;
  Begin
    expr := '';
    asmtok := A_NONE; { assmume no prefix          }
    segreg := R_NO;   { assume no segment override }

    { //  opcode                          // }
    { allow for newline as in gas styled syntax }
    { under DOS you get two AS_SEPARATOR !! }
    while actasmtoken=AS_SEPARATOR do
      Consume(AS_SEPARATOR);
    if (actasmtoken <> AS_OPCODE) then
    Begin
      Message(asmr_e_invalid_or_missing_opcode);
      { error recovery }
      While not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
         Consume(actasmtoken);
      exit;
    end
    else
    Begin
      op := findopcode(actasmpattern);
      instr.opcode:=op;
      Consume(AS_OPCODE);
      { // Zero operand opcode ? // }
      if actasmtoken = AS_SEPARATOR then
        exit
      else
       operandnum := 1;
    end;

    While actasmtoken <> AS_SEPARATOR do
    Begin
       case actasmtoken of
         { //  Operand delimiter // }
         AS_COMMA: Begin
                  if operandnum > MaxOperands then
                    Message(asmr_e_too_many_operands)
                  else
                    Inc(operandnum);
                  Consume(AS_COMMA);
                end;
         { // End of asm operands for this opcode // }
         AS_SEPARATOR: ;
       else
         BuildOperand(instr);
     end; { end case }
    end; { end while }
  end;




  Function Assemble: Ptree;
  {*********************************************************************}
  { PROCEDURE Assemble;                                                 }
  {  Description: Parses the att assembler syntax, parsing is done      }
  {  according to GAs rules.                                            }
  {*********************************************************************}
  Var
   hl: pasmlabel;
   labelptr,nextlabel : pasmlabel;
   commname : string;
   store_p : paasmoutput;

  Begin
    Message(asmr_d_start_reading);
    firsttoken := TRUE;
    operandnum := 0;
    { sets up all opcode and register tables in uppercase }
    if not _asmsorted then
    Begin
      SetupTables;
      _asmsorted := TRUE;
    end;
    p:=new(paasmoutput,init);
    { save pointer code section }
    store_p:=p;
    { setup label linked list }
    new(LocalLabelList,Init);
    c:=current_scanner^.asmgetchar;
    actasmtoken:=gettoken;
    while actasmtoken<>AS_END do
    Begin
      case actasmtoken of
        AS_LLABEL:
          Begin
            if CreateLocalLabel(actasmpattern,hl,true) then
              ConcatLabel(p,hl);
            Consume(AS_LLABEL);
          end;
        AS_LABEL: Begin
                     { when looking for Pascal labels, these must }
                     { be in uppercase.                           }
                     if SearchLabel(upper(actasmpattern),hl,true) then
                       ConcatLabel(p,hl)
                     else
                     Begin
                       Message1(asmr_e_unknown_label_identifier,actasmpattern);
                     end;
                     Consume(AS_LABEL);
                 end;
        AS_DW:   Begin
                   Consume(AS_DW);
                   BuildConstant($ffff);
                 end;
        AS_DB:   Begin
                  Consume(AS_DB);
                  BuildConstant($ff);
                end;
        AS_DD:   Begin
                 Consume(AS_DD);
                 BuildConstant($ffffffff);
                end;
        AS_XDEF:
                  Begin
                   { normal units should not be able to declare }
                   { direct label names like this... anyhow     }
                   { procedural calls in asm blocks are         }
                   { supposedely replaced automatically         }
                   if (cs_compilesystem in aktmoduleswitches) then
                   begin
                     Consume(AS_XDEF);
                      if actasmtoken <> AS_ID then
                       Message(asmr_e_invalid_global_def)
                      else
                        ConcatPublic(p,actasmpattern);
                      Consume(actasmtoken);
                      if actasmtoken <> AS_SEPARATOR then
                      Begin
                        Message(asmr_e_syntax_error);
                        while actasmtoken <> AS_SEPARATOR do
                         Consume(actasmtoken);
                      end;
                   end
                   else
                   begin
                     Message(asmr_w_xdef_not_supported);
                     while actasmtoken <> AS_SEPARATOR do
                       Consume(actasmtoken);
                   end;
                  end;
        AS_ALIGN: Begin
                    Message(asmr_w_align_not_supported);
                    while actasmtoken <> AS_SEPARATOR do
                     Consume(actasmtoken);
                  end;
        AS_OPCODE: Begin
                   instr.init;
                   BuildOpcode;
                   instr.ops := operandnum;
                   if instr.labeled then
                     ConcatLabeledInstr(instr)
                   else
                     ConcatOpCode(instr);
                   instr.done;
                  end;
        AS_SEPARATOR:Begin
                     Consume(AS_SEPARATOR);
                     { let us go back to the first operand }
                     operandnum := 0;
                    end;
        AS_END: ; { end assembly block }
    else
      Begin
         Message(asmr_e_syntax_error);
         { error recovery }
         Consume(actasmtoken);
      end;
    end; { end case }
  end; { end while }
  { Check LocalLabelList }
  LocalLabelList^.CheckEmitted;
  dispose(LocalLabelList,Done);

  assemble := genasmnode(p);
  Message(asmr_d_finish_reading);
end;


    procedure ra68kmot_exit;{$ifndef FPC}far;{$endif}
      begin
         if assigned(iasmops) then
           dispose(iasmops);
         exitproc:=old_exit;
      end;


Begin
   old_exit:=exitproc;
   exitproc:=@ra68kmot_exit;
end.
{
  $Log$
  Revision 1.2  2002-08-11 13:24:18  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.1  2002/08/06 15:15:42  carl
    + more m68k fixes

  Revision 1.1.2.7  2001/08/09 11:41:08  pierre
   + add more errors

  Revision 1.1.2.6  2001/08/08 12:21:49  pierre
   * generate correct labeled instruction for DBRA
   * improoved checking of operands
   + support for OPR_SYMBOL in generic instructions like MOVE
   + support for labels in constants like movel #datalabel + const,d0

  Revision 1.1.2.5  2001/08/07 15:55:33  pierre
   + new code for NetBSD, behaves like FreeBSD for now

  Revision 1.1.2.4  2001/08/01 10:58:50  pierre
   * avoid warning about supposed external in asm label is already known

  Revision 1.1.2.3  2001/07/24 23:41:32  pierre
   * firstreg and lastreg renamed firstasmreg and lastasmreg to aviod confusions

  Revision 1.1.2.2  2001/04/24 11:58:37  carl
  * correction of DBRA problems

  Revision 1.1.2.1  2001/03/04 02:19:54  carl
  - renamefest!

  Revision 1.1.2.1  2001/02/25 01:32:56  carl
  - imported from mian directory

  Revision 1.1.2.2  2001/02/23 11:20:40  pierre
   * fix an update problem

  Revision 1.1.2.1  2001/02/23 10:05:19  pierre
   * first bunch of m68k cpu updates

  Revision 1.1  2000/07/13 06:29:56  michael
  + Initial import

  Revision 1.13  2000/02/09 13:23:02  peter
    * log truncated

  Revision 1.12  2000/01/07 01:14:37  peter
    * updated copyright to 2000

  Revision 1.11  1999/11/10 00:06:08  pierre
   * adapted to procinfo as pointer

  Revision 1.10  1999/11/09 23:06:46  peter
    * esi_offset -> selfpointer_offset to be newcg compatible
    * hcogegen -> cgbase fixes for newcg

  Revision 1.9  1999/09/16 23:05:56  florian
    * m68k compiler is again compilable (only gas writer, no assembler reader)

}

