{
    $Id$
    Copyright (c) 1997-98 by Carl Eric Codere

    Does the parsing process for the intel styled inline assembler.

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
Unit Ra386int;

{**********************************************************************}
{ WARNING                                                              }
{**********************************************************************}
{  Any modification in the order or removal of terms in the tables     }
{  in i386.pas and intasmi3.pas will BREAK the code in this unit,      }
{  unless the appropriate changes are made to this unit. Addition      }
{  of terms though, will not change the code herein.                   }
{**********************************************************************}

{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{--------------------------------------------------------------------}
{ o Add support for floating point opcodes.                          }
{ o Handle module overrides also... such as crt.white or             }
{    crt.delay and local typed constants.                            }
{ o Handle label references                                          }
{ o Add support for TP styled segment overrides, when the opcode     }
{    table will be completed.                                        }
{ o Add imul,shld and shrd support with references and CL            }
{    i386.pas requires to be updated to do this.                     }
{ o Support for (* *) tp styled comments, this support should be     }
{   added in asmgetchar in scanner.pas (it cannot be implemented     }
{   here without causing errors such as in :                         }
{   (* "openbrace" AComment *)                                       }
{   (presently an infinite loop will be created if a (* styled       }
{    comment is found).                                              }
{ o Bugfix of ao_imm8s for IMUL. (Currently the 3 operand imul will  }
{   be considered as invalid because I use ao_imm8 and the table     }
{   uses ao_imm8s).                                                  }
{--------------------------------------------------------------------}

Interface

uses
  tree,i386;

   function assemble: ptree;

const
 { this variable is TRUE if the lookup tables have already been setup  }
 { for fast access. On the first call to assemble the tables are setup }
 { and stay set up.                                                    }
 _asmsorted: boolean = FALSE;
 firstreg       = R_EAX;
 lastreg        = R_ST7;

type
 tiasmops = array[firstop..lastop] of string[7];
 piasmops = ^tiasmops;

var
 { sorted tables of opcodes }
 iasmops: piasmops;
 { uppercased tables of registers }
 iasmregs: array[firstreg..lastreg] of string[6];


Implementation

Uses
  systems,files,aasm,globals,AsmUtils,strings,hcodegen,scanner,
  cobjects,verbose,types;


type
 tinteltoken = (
   AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_HEXNUM,AS_OCTALNUM,
   AS_BINNUM,AS_COMMA,AS_LBRACKET,AS_RBRACKET,AS_LPAREN,
   AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,AS_INTNUM,
   AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_SLASH,
     {------------------ Assembler directives --------------------}
   AS_DB,AS_DW,AS_DD,AS_END,
     {------------------ Assembler Operators  --------------------}
   AS_BYTE,AS_WORD,AS_DWORD,AS_QWORD,AS_TBYTE,AS_NEAR,AS_FAR,
   AS_HIGH,AS_LOW,AS_OFFSET,AS_SEG,AS_TYPE,AS_PTR,AS_MOD,AS_SHL,AS_SHR,AS_NOT,
   AS_AND,AS_OR,AS_XOR);

   tasmkeyword = string[6];
const
   { These tokens should be modified accordingly to the modifications }
   { in the different enumerations.                                   }
   firstdirective = AS_DB;
   lastdirective  = AS_END;
   firstoperator  = AS_BYTE;
   lastoperator   = AS_XOR;
   firstsreg      = R_CS;
   lastsreg       = R_SS;
   { this is a hack to accept all opcodes }
   { in the opcode table.                 }
   { check is done until A_POPFD          }
   { otherwise no check.                  }
   lastop_in_table = A_POPFD;

       _count_asmdirectives = longint(lastdirective)-longint(firstdirective);
       _count_asmoperators  = longint(lastoperator)-longint(firstoperator);
       _count_asmprefixes   = 5;
       _count_asmspecialops = 25;
       _count_asmoverrides  = 3;

       _asmdirectives : array[0.._count_asmdirectives] of tasmkeyword =
       ('DB','DW','DD','END');

       { problems with shl,shr,not,and,or and xor, they are }
       { context sensitive.                                 }
       _asmoperators : array[0.._count_asmoperators] of tasmkeyword = (
        'BYTE','WORD','DWORD','QWORD','TBYTE','NEAR','FAR','HIGH',
        'LOW','OFFSET','SEG','TYPE','PTR','MOD','SHL','SHR','NOT','AND',
        'OR','XOR');

     {------------------ Missing opcodes from std list  ----------------}
       _asmprefixes: array[0.._count_asmprefixes] of tasmkeyword = (
       'REPNE','REPE','REP','REPZ','REPNZ','LOCK');

       _asmoverrides: array[0.._count_asmoverrides] of tasmkeyword =
       ('SEGCS','SEGDS','SEGES','SEGSS');

       _overridetokens: array[0.._count_asmoverrides] of tregister =
       (R_CS,R_DS,R_ES,R_SS);

       _prefixtokens: array[0.._count_asmprefixes] of tasmop = (
       A_REPNE,A_REPE,A_REP,A_REPE,A_REPNE,A_LOCK);

       _specialops: array[0.._count_asmspecialops] of tasmkeyword = (
       'CMPSB','CMPSW','CMPSD','INSB','INSW','INSD','OUTSB','OUTSW','OUTSD',
       'SCASB','SCASW','SCASD','STOSB','STOSW','STOSD','MOVSB','MOVSW','MOVSD',
       'LODSB','LODSW','LODSD','LOCK','SEGCS','SEGDS','SEGES','SEGSS');

       _specialopstokens: array[0.._count_asmspecialops] of tasmop = (
       A_CMPS,A_CMPS,A_CMPS,A_INS,A_INS,A_INS,A_OUTS,A_OUTS,A_OUTS,
       A_SCAS,A_SCAS,A_SCAS,A_STOS,A_STOS,A_STOS,A_MOVS,A_MOVS,A_MOVS,
       A_LODS,A_LODS,A_LODS,A_LOCK,A_NONE,A_NONE,A_NONE,A_NONE);
     {------------------------------------------------------------------}
       { register type definition table for easier searching }
       _regtypes:array[firstreg..lastreg] of longint =
       (ao_reg32,ao_reg32,ao_reg32,ao_reg32,ao_reg32,ao_reg32,ao_reg32,ao_reg32,
       ao_reg16,ao_reg16,ao_reg16,ao_reg16,ao_reg16,ao_reg16,ao_reg16,ao_reg16,
       ao_reg8,ao_reg8,ao_reg8,ao_reg8,ao_reg8,ao_reg8,ao_reg8,ao_reg8,
       ao_none,ao_sreg2,ao_sreg2,ao_sreg2,ao_sreg3,ao_sreg3,ao_sreg2,
       ao_floatacc,ao_floatacc,ao_floatreg,ao_floatreg,ao_floatreg,ao_floatreg,
       ao_floatreg,ao_floatreg,ao_floatreg);

       _regsizes: array[firstreg..lastreg] of topsize =
       (S_L,S_L,S_L,S_L,S_L,S_L,S_L,S_L,
        S_W,S_W,S_W,S_W,S_W,S_W,S_W,S_W,
        S_B,S_B,S_B,S_B,S_B,S_B,S_B,S_B,
        { segment register }
        S_W,S_W,S_W,S_W,S_W,S_W,S_W,
        { can also be S_S or S_T - must be checked at run-time }
        S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL,S_FL);

       {topsize = (S_NO,S_B,S_W,S_L,S_BW,S_BL,S_WL,
                  S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX,S_D);}
       _constsizes: array[S_NO..S_FS] of longint =
       (0,ao_imm8,ao_imm16,ao_imm32,0,0,0,ao_imm16,ao_imm32,0,ao_imm32);




const
  newline = #10;
  firsttoken : boolean = TRUE;
  operandnum : byte = 0;
var
 { context for SHL,SHR,AND,NOT,OR,XOR operators }
 { if set to true GetToken will return these    }
 { as operators, otherwise will return these as }
 { opcodes.                                     }
 inexpression: boolean;
 p : paasmoutput;
 actasmtoken: tinteltoken;
 actasmpattern: string;
 c: char;
 Instr: TInstruction;
 labellist: TAsmLabelList;
 old_exit : pointer;


   Procedure SetupTables;
   { creates uppercased symbol tables for speed access }
   var
     i: tasmop;
     j: tregister;
   Begin
     Message(assem_d_creating_lookup_tables);
     { opcodes }
     new(iasmops);
     for i:=firstop to lastop do
      iasmops^[i] := upper(int_op2str[i]);
     { opcodes }
     for j:=firstreg to lastreg do
      iasmregs[j] := upper(int_reg2str[j]);
   end;


  {---------------------------------------------------------------------}
  {                     Routines for the tokenizing                     }
  {---------------------------------------------------------------------}


   function is_asmopcode(const s: string):Boolean;
  {*********************************************************************}
  { FUNCTION is_asmopcode(s: string):Boolean                            }
  {  Description: Determines if the s string is a valid opcode          }
  {  if so returns TRUE otherwise returns FALSE.                        }
  {*********************************************************************}
   var
    i: tasmop;
    j: byte;
   Begin
     is_asmopcode := FALSE;
     for i:=firstop to lastop do
     begin
       if  s = iasmops^[i] then
       begin
          is_asmopcode:=TRUE;
          exit;
       end;
     end;
     { not found yet, search for extended opcodes }
     for j:=0 to _count_asmspecialops do
     Begin
       if s = _specialops[j] then
       Begin
         is_asmopcode:=TRUE;
         exit;
       end;
     end;
   end;



   Procedure is_asmdirective(const s: string; var token: tinteltoken);
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
           token := tinteltoken(longint(firstdirective)+i);
           exit;
        end;
     end;
   end;

   Procedure is_asmoperator(const s: string; var token: tinteltoken);
  {*********************************************************************}
  { FUNCTION  is_asmoperator(s: string; var token: tinteltoken): Boolean}
  {  Description: Determines if the s string is a valid operator        }
  { (an operator can occur in operand fields, while a directive cannot) }
  {  if so returns the operator token, otherwise does not change token. }
  {*********************************************************************}
   var
    i:longint;
   Begin
     for i:=0 to _count_asmoperators do
     begin
        if s=_asmoperators[i] then
        begin
           token := tinteltoken(longint(firstoperator)+i);
           exit;
        end;
     end;
   end;





   Procedure is_register(const s: string; var token: tinteltoken);
  {*********************************************************************}
  { PROCEDURE is_register(s: string; var token: tinteltoken);           }
  {  Description: Determines if the s string is a valid register, if    }
  {  so return token equal to A_REGISTER, otherwise does not change token}
  {*********************************************************************}
   Var
    i: tregister;
   Begin
     for i:=firstreg to lastreg do
     begin
      if s=iasmregs[i] then
      begin
        token := AS_REGISTER;
        exit;
      end;
     end;
   end;




  Function GetToken: tinteltoken;
  {*********************************************************************}
  { FUNCTION GetToken: tinteltoken;                                     }
  {  Description: This routine returns intel assembler tokens and       }
  {  does some minor syntax error checking.                             }
  {*********************************************************************}
  var
   j: integer;
   token: tinteltoken;
   forcelabel: boolean;
   errorflag : boolean;
  begin
    errorflag := FALSE;
    forcelabel := FALSE;
    actasmpattern :='';
    {* INIT TOKEN TO NOTHING *}
    token := AS_NONE;
    { while space and tab , continue scan... }
    while (c in [' ',#9]) do
      c := asmgetchar;
    { Possiblities for first token in a statement:                }
    {   Local Label, Label, Directive, Prefix or Opcode....       }
    tokenpos.line:=current_module^.current_inputfile^.line_no;
    tokenpos.column:=get_file_col;
    tokenpos.fileindex:=current_module^.current_index;
    if firsttoken and not (c in [newline,#13,'{',';']) then
    begin
      firsttoken := FALSE;
      if c = '@' then
      begin
        token := AS_LLABEL;   { this is a local label }
        { Let us point to the next character }
        c := asmgetchar;
      end;



      while c in ['A'..'Z','a'..'z','0'..'9','_','@'] do
      begin
         { if there is an at_sign, then this must absolutely be a label }
         if c = '@' then forcelabel:=TRUE;
         actasmpattern := actasmpattern + c;
         c := asmgetchar;
      end;

      uppervar(actasmpattern);

      if c = ':' then
      begin
           case token of
             AS_NONE: token := AS_LABEL;
             AS_LLABEL: ; { do nothing }
           end; { end case }
           { let us point to the next character }
           c := asmgetchar;
           gettoken := token;
           exit;
      end;

      { Are we trying to create an identifier with }
      { an at-sign...?                             }
      if forcelabel then
       Message(assem_e_none_label_contain_at);

      If is_asmopcode(actasmpattern) then
      Begin
       gettoken := AS_OPCODE;
       { check if we are in an expression  }
       { then continue with asm directives }
       if not inexpression then
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
         Message1(assem_e_invalid_operand,actasmpattern);
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
                             c:= asmgetchar;
                             while c in  ['A'..'Z','a'..'z','0'..'9','_','@'] do
                             begin
                               actasmpattern := actasmpattern + c;
                               c := asmgetchar;
                             end;
                             uppervar(actasmpattern);
                             gettoken := AS_ID;
                             exit;
                            end;
      { identifier, register, opcode, prefix or directive }
         'A'..'Z','a'..'z','_': begin
                             actasmpattern := c;
                             c:= asmgetchar;
                             while c in  ['A'..'Z','a'..'z','0'..'9','_'] do
                             begin
                               actasmpattern := actasmpattern + c;
                               c := asmgetchar;
                             end;
                             uppervar(actasmpattern);

                             If is_asmopcode(actasmpattern) then
                             Begin
                                    gettoken := AS_OPCODE;
                                    { if we are not in a constant }
                                    { expression than this is an  }
                                    { opcode.                     }
                                    if  not inexpression then
                                    exit;
                             end;
                             is_register(actasmpattern, token);
                             is_asmoperator(actasmpattern,token);
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
                         Message(assem_w_override_op_not_supported);
                         c:=asmgetchar;
                         gettoken := AS_NONE;
                      end;
           { string or character }
           '''' :
                      begin
                         actasmpattern:='';
                         while true do
                         begin
                           if c = '''' then
                           begin
                              c:=asmgetchar;
                              if c=newline then
                              begin
                                 Message(scan_f_string_exceeds_line);
                                 break;
                              end;
                              repeat
                                  if c=''''then
                                   begin
                                       c:=asmgetchar;
                                       if c='''' then
                                        begin
                                               actasmpattern:=actasmpattern+'''';
                                               c:=asmgetchar;
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
                                          c:=asmgetchar;
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
           { string or character }
           '"' :
                      begin
                         actasmpattern:='';
                         while true do
                         begin
                           if c = '"' then
                           begin
                              c:=asmgetchar;
                              if c=newline then
                              begin
                                 Message(scan_f_string_exceeds_line);
                                 break;
                              end;
                              repeat
                                  if c='"'then
                                   begin
                                       c:=asmgetchar;
                                       if c='"' then
                                        begin
                                               actasmpattern:=actasmpattern+'"';
                                               c:=asmgetchar;
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
                                          c:=asmgetchar;
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
                   token := AS_STRING;
                   gettoken := token;
                   exit;
                 end;
           '$' :  begin
                    c:=asmgetchar;
                    while c in ['0'..'9','A'..'F','a'..'f'] do
                    begin
                      actasmpattern := actasmpattern + c;
                      c := asmgetchar;
                    end;
                   gettoken := AS_HEXNUM;
                   exit;
                  end;
           ',' : begin
                   gettoken := AS_COMMA;
                   c:=asmgetchar;
                   exit;
                 end;
           '[' : begin
                   gettoken := AS_LBRACKET;
                   c:=asmgetchar;
                   exit;
                 end;
           ']' : begin
                   gettoken := AS_RBRACKET;
                   c:=asmgetchar;
                   exit;
                 end;
           '(' : begin
                   gettoken := AS_LPAREN;
                   c:=asmgetchar;
                   exit;
                 end;
           ')' : begin
                   gettoken := AS_RPAREN;
                   c:=asmgetchar;
                   exit;
                 end;
           ':' : begin
                   gettoken := AS_COLON;
                   c:=asmgetchar;
                   exit;
                 end;
           '.' : begin
                   gettoken := AS_DOT;
                   c:=asmgetchar;
                   exit;
                 end;
           '+' : begin
                   gettoken := AS_PLUS;
                   c:=asmgetchar;
                   exit;
                 end;
           '-' : begin
                   gettoken := AS_MINUS;
                   c:=asmgetchar;
                   exit;
                 end;
           '*' : begin
                   gettoken := AS_STAR;
                   c:=asmgetchar;
                   exit;
                 end;
           '/' : begin
                   gettoken := AS_SLASH;
                   c:=asmgetchar;
                   exit;
                 end;
           '0'..'9': begin
                          { this flag indicates if there was an error  }
                          { if so, then we use a default value instead.}
                          errorflag := false;
                          actasmpattern := c;
                          c := asmgetchar;
                          { Get the possible characters }
                          while c in ['0'..'9','A'..'F','a'..'f'] do
                          begin
                            actasmpattern := actasmpattern + c;
                            c:= asmgetchar;
                          end;
                          { Get ending character }
                          uppervar(actasmpattern);
                          c:=upcase(c);
                          { possibly a binary number. }
                          if (actasmpattern[length(actasmpattern)] = 'B') and (c <> 'H') then
                          Begin
                                  { Delete the last binary specifier }
                                  delete(actasmpattern,length(actasmpattern),1);
                                  for j:=1 to length(actasmpattern) do
                                   if not (actasmpattern[j] in ['0','1']) then
                                   begin
                                       Message1(assem_e_error_in_binary_const,actasmpattern);
                                       errorflag := TRUE;
                                   end;
                                 { if error, then suppose a binary value of zero. }
                                 if errorflag then
                                   actasmpattern := '0';
                                 gettoken := AS_BINNUM;
                                 exit;
                          end
                          else
                          Begin
                             case c of
                              'O': Begin
                                      for j:=1 to length(actasmpattern) do
                                        if not (actasmpattern[j] in ['0'..'7']) then
                                        begin
                                          Message1(assem_e_error_in_octal_const,actasmpattern);
                                          errorflag := TRUE;
                                        end;
                                 { if error, then suppose an octal value of zero. }
                                     if errorflag then
                                        actasmpattern := '0';
                                      gettoken := AS_OCTALNUM;
                                      c := asmgetchar;
                                      exit;
                                    end;
                              'H': Begin
                                      for j:=1 to length(actasmpattern) do
                                        if not (actasmpattern[j] in ['0'..'9','A'..'F']) then
                                        begin
                                          Message1(assem_e_error_in_hex_const,actasmpattern);
                                          errorflag := TRUE;
                                        end;
                                 { if error, then suppose an hex value of zero. }
                                     if errorflag then
                                        actasmpattern := '0';
                                     gettoken := AS_HEXNUM;
                                     c := asmgetchar;
                                     exit;
                                   end;
                              else { must be an integer number }
                               begin
                                    for j:=1 to length(actasmpattern) do
                                     if not (actasmpattern[j] in ['0'..'9']) then
                                     begin
                                         Message1(assem_e_error_in_integer_const,actasmpattern);
                                         errorflag := TRUE;
                                     end;
                                 { if error, then suppose an int value of zero. }
                                     if errorflag then
                                        actasmpattern := '0';
                                     gettoken := AS_INTNUM;
                                     exit;
                              end;
                          end; { end case }
                      end; { end if }
                     end;
    ';','{',#13,newline : begin
                            c:=asmgetchar;
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
  {                     Routines for the output                         }
  {---------------------------------------------------------------------}

   { returns an appropriate ao_xxxx flag indicating the type }
   { of operand.                                             }
   function findtype(Var Opr: TOperand): longint;
   Begin
    With Opr do
    Begin
     case operandtype of
       OPR_REFERENCE:   Begin
                           if assigned(ref.symbol) then
                           { check if in local label list }
                           { if so then it is considered  }
                           { as a displacement.           }
                           Begin
                             if labellist.search(ref.symbol^) <> nil then
                               findtype := ao_disp
                             else
                               findtype := ao_mem; { probably a mem ref. }
                           end
                           else
                            findtype := ao_mem;
                        end;
       OPR_CONSTANT: Begin
                       { check if there is not already a default size }
                       if opr.size <> S_NO then
                       Begin
                          findtype := _constsizes[opr.size];
                         exit;
                       end;
                       if val < $ff then
                       Begin
                         findtype := ao_imm8;
                         opr.size := S_B;
                       end
                       else if val < $ffff then
                       Begin
                         findtype := ao_imm16;
                         opr.size := S_W;
                       end
                       else
                       Begin
                         findtype := ao_imm32;
                         opr.size := S_L;
                       end
                     end;
       OPR_REGISTER: Begin
                      findtype := _regtypes[reg];
                      exit;
                     end;
         OPR_SYMBOL: Begin
                       findtype := ao_jumpabsolute;
                     end;
       OPR_NONE:     Begin
                       findtype := 0;
                     end;
       else
       Begin
         Message(assem_f_internal_error_in_findtype);
       end;
     end;
    end;
   end;



    Procedure ConcatLabeledInstr(var instr: TInstruction);
    Begin
       if (instr.getinstruction in [A_JO,A_JNO,A_JB,A_JC,A_JNAE,
        A_JNB,A_JNC,A_JAE,A_JE,A_JZ,A_JNE,A_JNZ,A_JBE,A_JNA,A_JNBE,
        A_JA,A_JS,A_JNS,A_JP,A_JPE,A_JNP,A_JPO,A_JL,A_JNGE,A_JNL,A_JGE,
        A_JLE,A_JNG,A_JNLE,A_JG,A_JCXZ,A_JECXZ,A_LOOP,A_LOOPZ,A_LOOPE,
        A_LOOPNZ,A_LOOPNE,A_MOV,A_JMP,A_CALL]) then
       Begin
        if instr.numops > 1 then
         Message(assem_e_invalid_labeled_opcode)
        else if instr.operands[1].operandtype <> OPR_LABINSTR then
          Message(assem_e_invalid_labeled_opcode)
        else if (instr.operands[1].operandtype = OPR_LABINSTR) and
         (instr.numops = 1) then
           if assigned(instr.operands[1].hl) then
            ConcatLabel(p,instr.getinstruction, instr.operands[1].hl)
           else
            Message(assem_f_internal_error_in_findtype);
       end
       else if instr.getinstruction = A_MOV then
       Begin
         { MOV to rel8 }
       end
       else
        Message(assem_e_invalid_operand);
    end;




   Procedure HandleExtend(var instr: TInstruction);
   { Handles MOVZX, MOVSX ... }
   var
     instruc: tasmop;
     opsize: topsize;
   Begin
      instruc:=instr.getinstruction;
      { return the old types ..}
      { these tokens still point to valid intel strings, }
      { but we must convert them to TRUE intel tokens    }
      if instruc in [A_MOVSB,A_MOVSBL,A_MOVSBW,A_MOVSWL] then
        instruc := A_MOVSX;
      if instruc in [A_MOVZB,A_MOVZWL] then
        instruc := A_MOVZX;

     With instr do

         Begin
           if operands[1].size = S_B then
           Begin
              if operands[2].size = S_L then
                 opsize := S_BL
              else
              if operands[2].size = S_W then
                 opsize := S_BW
              else
              begin
                 Message(assem_e_invalid_size_movzx);
                 exit;
              end;

           end
           else
           if operands[1].size = S_W then
           Begin
             if operands[2].size = S_L then
                opsize := S_WL
             else
             begin
                 Message(assem_e_invalid_size_movzx);
                 exit;
             end;
           end
           else
           begin
                 Message(assem_e_invalid_size_movzx);
                 exit;
           end;


           if operands[1].operandtype = OPR_REGISTER then
           Begin
              if operands[2].operandtype <> OPR_REGISTER then
               Message(assem_e_invalid_opcode)
              else
                 p^.concat(new(pai386,op_reg_reg(instruc,opsize,
                   operands[1].reg,operands[2].reg)));
           end
           else
           if operands[1].operandtype = OPR_REFERENCE then
           Begin
              if operands[2].operandtype <> OPR_REGISTER then
               Message(assem_e_invalid_opcode)
              else
                 p^.concat(new(pai386,op_ref_reg(instruc,opsize,
                   newreference(operands[1].ref),operands[2].reg)));
           end
     end; { end with }
   end;


  Procedure ConcatOpCode(var instr: TInstruction);
  {*********************************************************************}
  { First Pass:                                                         }
  {       if instr = Lxxx with a 16bit offset, we emit an error.        }
  {       If the instruction is INS,IN,OUT,OUTS,RCL,ROL,RCR,ROR,        }
  {        SAL,SAR,SHL,SHR,SHLD,SHRD,DIV,IDIV,BT,BTC,BTR,BTS,INT,       }
  {        RET,ENTER,SCAS,CMPS,STOS,LODS,FNSTSW,FSTSW.                  }
  {         set up the optypes variables manually, as well as setting   }
  {         operand sizes.                                              }
  { Second pass:                                                        }
  {  Check if the combination of opcodes and operands are valid, using  }
  {  the opcode table.                                                  }
  { Third pass:                                                         }
  {    If there was no error on the 2nd pass  , then we check the       }
  {    following:                                                       }
  {    - If this is a 0 operand opcode                                  }
  {        we verify if it is a string opcode, if so we emit a size also}
  {        otherwise simply emit the opcode by itself.                  }
  {    - If this is a 1 operand opcode, and it is a reference, we make  }
  {      sure that the operand size is valid; we emit the opcode.       }
  {    - If this is a two operand opcode                                }
  {      o if the opcode is MOVSX or MOVZX then we handle it specially  }
  {      o we check the operand types (most important combinations):    }
  {            if reg,reg we make sure that both registers are of the   }
  {             same size.                                              }
  {            if reg,ref or ref,reg we check if the symbol name is     }
  {             assigned, if so a size must be specified and compared   }
  {             to the register size, both must be equal. If there is   }
  {             no symbol name, then we check :                         }
  {                if refsize = NO_SIZE then OPCODE_SIZE = regsize      }
  {                  else if refsize = regsize then OPCODE_SIZE = regsize}
  {                   else error.                                       }
  {                   if no_error emit the opcode.                      }
  {            if ref,const or const,ref if ref does not have any size  }
  {              then error, otherwise emit the opcode.                 }
  {    - If this is a three operand opcode:                             }
  {          imul,shld,and shrd  -> check them manually.                }
  {*********************************************************************}
  var
    fits : boolean;
    i: longint;
    opsize: topsize;
    optyp1, optyp2, optyp3: longint;
    instruc: tasmop;
  Begin
     fits := FALSE;
     for i:=1 to instr.numops do
     Begin
       case instr.operands[i].operandtype of
         OPR_REGISTER: instr.operands[i].size :=
                         _regsizes[instr.operands[i].reg];
       end; { end case }
     end; { endif }
    { setup specific instructions for first pass }
    instruc := instr.getinstruction;
    if (instruc in [A_LEA,A_LDS,A_LSS,A_LES,A_LFS,A_LGS]) then
    Begin
       if instr.operands[1].size <> S_L then
       Begin
         Message(assem_e_16bit_base_in_32bit_segment);
         exit;
       end; { endif }
    end;

    With instr do
    Begin


      for i:=1 to numops do
      Begin
         With operands[i] do
         Begin
         { check for 16-bit bases/indexes and emit an error.   }
         { we cannot only emit a warning since gas does not    }
         { accept 16-bit indexes and bases.                    }
          if (operandtype = OPR_REFERENCE) and
            ((ref.base <> R_NO) or
            (ref.index <> R_NO)) then
            Begin
            { index or base defined. }
              if (ref.base <> R_NO) then
              Begin
                if not (ref.base in
                  [R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESI,R_EDI,R_ESP]) then
                    Message(assem_e_16bit_base_in_32bit_segment);
              end;
            { index or base defined. }
              if (ref.index <> R_NO) then
              Begin
                  if not (ref.index in
                    [R_EAX,R_EBX,R_ECX,R_EDX,R_EBP,R_ESI,R_EDI,R_ESP]) then
                    Message(assem_e_16bit_index_in_32bit_segment);
              end;
            end;
            { Check for constants without bases/indexes in memory }
            { references.                                         }
            if (operandtype = OPR_REFERENCE) and
               (ref.base = R_NO) and
               (ref.index = R_NO) and
               (ref.symbol = nil) and
               (ref.offset <> 0) then
               Begin
                 ref.isintvalue := TRUE;
                 Message(assem_e_const_ref_not_allowed);
               end;

              opinfo := findtype(operands[i]);

          end; { end with }
      end; {endfor}




       { TAKE CARE OF SPECIAL OPCODES, TAKE CARE OF THEM INDIVUALLY.    }
       { ALL THE REST ARE TAKEN CARE BY OPCODE TABLE AND THIRD PASS.    }
       if instruc = A_FST then
       Begin
       end
       else
       if instruc = A_FILD then
       Begin
       end
       else
       if instruc = A_FLD then
       Begin
            {A_FLDS,A_FLDL,A_FLDT}
       end
       else
       if instruc = A_FIST then
       Begin
            {A_FISTQ,A_FISTS,A_FISTL}
       end
       else
       if instruc = A_FWAIT then
        FWaitWarning
       else
       if instruc = A_MOVSX then
       Begin
         { change the instruction to conform to GAS }
         if operands[1].size = S_W then
         Begin
             addinstr(A_MOVSBW)
         end
         else
         if operands[1].size = S_L then
         Begin
             if operands[2].size = S_B then
                addinstr(A_MOVSBL)
             else
                addinstr(A_MOVSWL);
         end;
         instruc := getinstruction; { reload instruction }
       end
       else
       if instruc = A_MOVZX then
       Begin
         { change the instruction to conform to GAS }
         if operands[1].size = S_W then
         Begin
             addinstr(A_MOVZB)
         end
         else
         if operands[1].size = S_L then
         Begin
             if operands[2].size = S_B then
                addinstr(A_MOVZB)
             else
                addinstr(A_MOVZWL);
         end;
         instruc := getinstruction; { reload instruction }
       end
       else
       if (instruc in [A_BT,A_BTC,A_BTR,A_BTS]) then
       Begin
          if numops = 2 then
            Begin
                if (operands[2].operandtype = OPR_CONSTANT)
                and (operands[2].val <= $ff) then
                  Begin
                     operands[2].opinfo := ao_imm8;
                     { no operand size if using constant. }
                     operands[2].size := S_NO;
                     fits := TRUE;
                  end
            end
          else
            Begin
                Message(assem_e_invalid_opcode_and_operand);
                exit;
            end;
       end
       else
       if instruc = A_ENTER then
       Begin
          if numops =2 then
            Begin
               if (operands[1].operandtype = OPR_CONSTANT) and
                  (operands[1].val <= $ffff) then
                  Begin
                     operands[1].opinfo := ao_imm16;
                  end  { endif }
            end { endif }
          else
            Begin
                Message(assem_e_invalid_opcode_and_operand);
                exit;
            end
       end { endif }
       else
     {  Handle special opcodes for the opcode   }
     {  table. Set them up correctly.           }
       if (instruc in [A_IN,A_INS]) then
       Begin
          if numops =2 then
            Begin
              if (operands[2].operandtype = OPR_REGISTER) and (operands[2].reg = R_DX)
               then
               Begin
                  operands[2].opinfo := ao_inoutportreg;
                  if (operands[1].operandtype = OPR_REGISTER) and
                    (operands[1].reg in [R_EAX,R_AX,R_AL]) and
                    (instruc = A_IN) then
                    Begin
                       operands[1].opinfo := ao_acc;
                    end
               end
              else
              if (operands[2].operandtype = OPR_CONSTANT) and (operands[2].val <= $ff)
                and (instruc = A_IN) then
                Begin
                  operands[2].opinfo := ao_imm8;
                  operands[2].size := S_B;
                 if (operands[1].operandtype = OPR_REGISTER) and
                    (operands[1].reg in [R_EAX,R_AX,R_AL]) and
                    (instruc = A_IN) then
                    Begin
                       operands[1].opinfo := ao_acc;
                    end
                end;
            end
          else
            if not ((numops=0) and (instruc=A_INS)) then
             Begin
               Message(assem_e_invalid_opcode_and_operand);
               exit;
             end;
       end
       else
       if (instruc in [A_OUT,A_OUTS]) then
       Begin
          if numops =2 then
            Begin
              if (operands[1].operandtype = OPR_REGISTER) and (operands[1].reg = R_DX)
               then
               Begin
                  operands[1].opinfo := ao_inoutportreg;
                  if (operands[2].operandtype = OPR_REGISTER) and
                     (operands[2].reg in [R_EAX,R_AX,R_AL]) and
                     (instruc = A_OUT) then
                     Begin
                       operands[2].opinfo := ao_acc;
                       fits := TRUE;
                     end
               end
              else
              if (operands[1].operandtype = OPR_CONSTANT) and (operands[1].val <= $ff)
                and (instruc = A_OUT) then
                Begin
                  operands[1].opinfo := ao_imm8;
                  operands[1].size := S_B;
                  if (operands[2].operandtype = OPR_REGISTER) and
                     (operands[2].reg in [R_EAX,R_AX,R_AL]) and
                     (instruc = A_OUT) then
                     Begin
                       operands[2].opinfo := ao_acc;
                       fits := TRUE;
                     end
                end;
            end
          else
            if not ((numops=0) and (instruc=A_OUTS)) then
             Begin
               Message(assem_e_invalid_opcode_and_operand);
               exit;
             end;
       end
       else
       if instruc in [A_RCL,A_RCR,A_ROL,A_ROR,A_SAL,A_SAR,A_SHL,A_SHR] then
       { if RCL,ROL,... }
       Begin
          if numops =2 then
            Begin
              if (operands[2].operandtype = OPR_REGISTER) and (operands[2].reg = R_CL)
              then
              Begin
                operands[2].opinfo := ao_shiftcount
              end
              else
              if (operands[2].operandtype = OPR_CONSTANT) and
                (operands[2].val <= $ff) then
                Begin
                   operands[2].opinfo := ao_imm8;
                   operands[2].size := S_B;
                end;
            end
          else { if numops = 2 }
            Begin
                Message(assem_e_invalid_opcode_and_operand);
                exit;
            end;
       end
       { endif ROL,RCL ... }
       else
       if instruc in [A_DIV, A_IDIV] then
       Begin
          if (operands[1].operandtype = OPR_REGISTER) and
            (operands[1].reg in [R_AL,R_AX,R_EAX]) then
                operands[1].opinfo := ao_acc;
       end
       else
       if (instruc = A_FNSTSW) or (instruc = A_FSTSW) then
       Begin
          if numops = 1 then
            Begin
                if (operands[1].operandtype = OPR_REGISTER) and
                  (operands[1].reg = R_AX) then
                 operands[1].opinfo := ao_acc;
            end
          else
            Begin
              Message(assem_e_invalid_opcode_and_operand);
              exit;
            end;
       end
       else
       if (instruc = A_SHLD) or (instruc = A_SHRD) then
       { these instruction are fully parsed individually on pass three }
       { so we just do a summary checking here.                        }
       Begin
          if numops = 3 then
            Begin
                if (operands[3].operandtype = OPR_CONSTANT)
                and (operands[3].val <= $ff) then
                Begin
                   operands[3].opinfo := ao_imm8;
                   operands[3].size := S_B;
                end;
            end
          else
            Begin
                Message(assem_e_invalid_opcode_and_operand);
                exit;
            end;
       end
       else
       if instruc = A_INT then
       Begin
          if numops = 1 then
            Begin
               if (operands[1].operandtype = OPR_CONSTANT) and
                 (operands[1].val <= $ff) then
                      operands[1].opinfo := ao_imm8;
            end
       end
       else
       if instruc = A_RET then
       Begin
          if numops =1 then
            Begin
               if (operands[1].operandtype = OPR_CONSTANT) and
                  (operands[1].val <= $ffff) then
                    operands[1].opinfo := ao_imm16;
            end
       end; { endif }

       { all string instructions have default memory }
       { location which are ignored. Take care of    }
       { those.                                      }
       { Here could be added the code for segment    }
       { overrides.                                  }
       if instruc in [A_SCAS,A_CMPS,A_STOS,A_LODS] then
       Begin
          if numops =1 then
            Begin
               if (operands[1].operandtype = OPR_REFERENCE) and
                 (assigned(operands[1].ref.symbol)) then
                 Freemem(operands[1].ref.symbol,length(operands[1].ref.symbol^)+1);
               operands[1].operandtype := OPR_NONE;
               numops := 0;
            end;
       end; { endif }
       if instruc in [A_INS,A_MOVS,A_OUTS] then
       Begin
          if numops =2 then
            Begin
               if (operands[1].operandtype = OPR_REFERENCE) and
                 (assigned(operands[1].ref.symbol)) then
                 Freemem(operands[1].ref.symbol,length(operands[1].ref.symbol^)+1);
               if (operands[2].operandtype = OPR_REFERENCE) and
                 (assigned(operands[2].ref.symbol)) then
                 Freemem(operands[2].ref.symbol,length(operands[1].ref.symbol^)+1);
               operands[1].operandtype := OPR_NONE;
               operands[2].operandtype := OPR_NONE;
               numops := 0;
            end;
       end;
     { handle parameter for segment overrides }
     if instruc = A_XLAT then
     Begin
        { handle special TP syntax case for XLAT }
        { here we accept XLAT, XLATB and XLAT m8 }
        if (numops = 1) or (numops = 0) then
         Begin
               if (operands[1].operandtype = OPR_REFERENCE) and
                 (assigned(operands[1].ref.symbol)) then
                 Freemem(operands[1].ref.symbol,length(operands[1].ref.symbol^)+1);
               operands[1].operandtype := OPR_NONE;
               numops := 0;
               { always a byte for XLAT }
               instr.stropsize := S_B;
         end;
     end;



    { swap the destination and source }
    { to put in AT&T style direction  }
    { only if there are 2/3 operand   }
    { numbers.                        }
    if (instruc <> A_ENTER) then
       SwapOperands(instr);
    { copy them to local variables }
    { for faster access            }
    optyp1:=operands[1].opinfo;
    optyp2:=operands[2].opinfo;
    optyp3:=operands[3].opinfo;

    end; { end with }

    { after reading the operands }
    { search the instruction     }
    { setup startvalue from cache }
    if ins_cache[instruc]<>-1 then
       i:=ins_cache[instruc]
    else i:=0;


    { this makes cpu.pp uncompilable, but i think this code should be }
    { inserted in the system unit anyways.                            }
    if (instruc >= lastop_in_table) then
{       ((cs_compilesystem in aktswitches) or (aktoptprocessor > systems.i386)) then }
      begin
         Message(assem_w_opcode_not_in_table);
         fits:=true;
      end
    else while not(fits) do
      begin
       { set the instruction cache, if the instruction }
       { occurs the first time                         }
       if (it[i].i=instruc) and (ins_cache[instruc]=-1) then
           ins_cache[instruc]:=i;

       if (it[i].i=instruc) and (instr.numops=it[i].ops) then
       begin
          { first fit }
          case instr.numops of
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
                { I consider sign-extended 8bit value to }
                { be equal to immediate 8bit therefore   }
                { convert...                             }
                if (optyp1 = ao_imm8) then
                Begin
                  { check if this is a simple sign extend. }
                  if (it[i].o1<>ao_imm8s) then
                  Begin
                    fits:=true;
                    break;
                  end;
                end;
              end;
          2 : if ((optyp1 and it[i].o1)<>0) and
               ((optyp2 and it[i].o2)<>0) then
               Begin
                     fits:=true;
                     break;
               end
               { if the operands can be swaped }
               { then swap them                }
               else if ((it[i].m and af_d)<>0) and
               ((optyp1 and it[i].o2)<>0) and
               ((optyp2 and it[i].o1)<>0) then
               begin
                 { swap the destination and source }
                 { to put in AT&T style direction  }
{ What does this mean !!!! ???????????????????????? }
{                 if (output_format in [of_o,of_att]) then }
                 { ???????????? }
{                          SwapOperands(instr); }
                 fits:=true;
                 break;
               end;
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
         Message(assem_e_invalid_opcode_and_operand);
         exit;
       end;
       inc(i);
      end; { end while }

  { We add the opcode to the opcode linked list }
  if fits then
  Begin
    if instr.getprefix <> A_NONE then
    Begin
      p^.concat(new(pai386,op_none(instr.getprefix,S_NO)));
    end;
    case instr.numops of
     0:
        if instr.stropsize <> S_NO then
        { is this a string operation opcode or xlat then check }
        { the size of the operation.                           }
          p^.concat(new(pai386,op_none(instruc,instr.stropsize)))
        else
          p^.concat(new(pai386,op_none(instruc,S_NO)));
     1: Begin
          case instr.operands[1].operandtype of
               { all one operand opcodes with constant have no defined sizes }
               { at least that is what it seems in the tasm 2.0 manual.      }
           OPR_CONSTANT:  p^.concat(new(pai386,op_const(instruc,
                             S_NO, instr.operands[1].val)));
           OPR_REGISTER: if instruc in [A_INC,A_DEC, A_NEG,A_NOT] then
                         Begin
                           p^.concat(new(pai386,op_reg(instruc,
                               instr.operands[1].size,instr.operands[1].reg)));
                         end
                         else
                           p^.concat(new(pai386,op_reg(instruc,
                               S_NO,instr.operands[1].reg)));
               { this is where it gets a bit more complicated...              }
           OPR_REFERENCE:
                          if instr.operands[1].size <> S_NO then
                          Begin
                           p^.concat(new(pai386,op_ref(instruc,
                            instr.operands[1].size,newreference(instr.operands[1].ref))));
                          end
                          else
                          Begin
                              { special jmp and call case with }
                              { symbolic references.           }
                              if instruc in [A_CALL,A_JMP] then
                              Begin
                                p^.concat(new(pai386,op_ref(instruc,
                                  S_NO,newreference(instr.operands[1].ref))));
                              end
                              else
                                Message(assem_e_invalid_opcode_and_operand);
                          end;
             OPR_SYMBOL:  Begin
                            p^.concat(new(pai386,op_csymbol(instruc,
                             instr.stropsize, newcsymbol(instr.operands[1].symbol^,0))));
                          End;
           OPR_NONE: Begin
                       Message(assem_f_internal_error_in_concatopcode);
                     end;
          else
           Begin
            Message(assem_f_internal_error_in_concatopcode);
           end;
          end;
        end;
     2:
        Begin
           if instruc in [A_MOVSX,A_MOVZX,A_MOVSB,A_MOVSBL,A_MOVSBW,
             A_MOVSWL,A_MOVZB,A_MOVZWL] then
              { movzx and movsx }
              HandleExtend(instr)
           else
             { other instructions }
             Begin
                With instr do
                Begin
                { source }
                  opsize := operands[1].size;
                  case operands[1].operandtype of
                  { reg,reg     }
                  { reg,ref     }
                   OPR_REGISTER:
                     Begin
                       case operands[2].operandtype of
                         OPR_REGISTER:
                            { see info in ratti386.pas, about the problem }
                            { which can cause gas here.                   }
                            if (opsize = operands[2].size) then
                            begin
                               p^.concat(new(pai386,op_reg_reg(instruc,
                               opsize,operands[1].reg,operands[2].reg)));
                            end
                            else
                            { these do not require any size specification. }
                            if (instruc in [A_IN,A_OUT,A_SAL,A_SAR,A_SHL,A_SHR,A_ROL,
                               A_ROR,A_RCR,A_RCL])  then
                               { outs and ins are already taken care by }
                               { the first pass.                        }
                               p^.concat(new(pai386,op_reg_reg(instruc,
                               S_NO,operands[1].reg,operands[2].reg)))
                            else
                            Begin
                              Message(assem_e_invalid_opcode_and_operand);
                            end;
                         OPR_REFERENCE:
                           { variable name. }
                           { here we must check the instruction type }
                           { before deciding if to use and compare   }
                           { any sizes.                              }
                           if assigned(operands[2].ref.symbol) then
                           Begin
                              if (opsize = operands[2].size) or (instruc in
                               [A_RCL,A_RCR,A_ROL,A_ROR,A_SAL,A_SAR,A_SHR,A_SHL]) then
                                  p^.concat(new(pai386,op_reg_ref(instruc,
                                  opsize,operands[1].reg,newreference(operands[2].ref))))
                              else
                                  Message(assem_e_invalid_size_in_ref);
                           end
                           else
                           Begin
                              { register reference }
                              { possiblities:1) local variable which }
                              { has been replaced by bp and offset   }
                              { in this case size should be valid    }
                              {              2) Indirect register    }
                              { adressing, 1st operand determines    }
                              { size.                                }
                              if (opsize = operands[2].size) or  (operands[2].size = S_NO) then
                                  p^.concat(new(pai386,op_reg_ref(instruc,
                                  opsize,operands[1].reg,newreference(operands[2].ref))))
                              else
                                  Message(assem_e_invalid_size_in_ref);
                           end;
                        OPR_CONSTANT: { const,reg }
                               Begin  { OUT const,reg }
                                 if (instruc = A_OUT) and (opsize = S_B) then
                                   p^.concat(new(pai386,op_reg_const(instruc,
                                    opsize,operands[1].reg,operands[2].val)))
                                 else
                                    Message(assem_e_invalid_size_in_ref);
                               end;
                       else { else case }
                         Begin
                           Message(assem_f_internal_error_in_concatopcode);
                         end;
                       end; { end inner case }
                     end;
                  { const,reg   }
                  { const,const }
                  { const,ref   }
                   OPR_CONSTANT:
                      case instr.operands[2].operandtype of
                      { constant, constant does not have a specific size. }
                        OPR_CONSTANT:
                           p^.concat(new(pai386,op_const_const(instruc,
                           S_NO,operands[1].val,operands[2].val)));
                        OPR_REFERENCE:
                           Begin
                              if (operands[1].val <= $ff) and
                               (operands[2].size in [S_B,S_W,S_L,
                                 S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX]) then
                                 p^.concat(new(pai386,op_const_ref(instruc,
                                 operands[2].size,operands[1].val,
                                 newreference(operands[2].ref))))
                              else
                              if (operands[1].val <= $ffff) and
                               (operands[2].size in [S_W,S_L,
                               S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX]) then
                                 p^.concat(new(pai386,op_const_ref(instruc,
                                 operands[2].size,operands[1].val,
                                 newreference(operands[2].ref))))
                              else
                              if (operands[1].val <= $7fffffff) and
                               (operands[2].size in [S_L,S_IL,S_IQ,S_FS,S_FL,S_FX]) then
                                 p^.concat(new(pai386,op_const_ref(instruc,
                                 operands[2].size,operands[1].val,
                                 newreference(operands[2].ref))))
                              else
                                  Message(assem_e_invalid_size_in_ref);
                           end;
                        OPR_REGISTER:
                           Begin
                              { size of opcode determined by register }
                              if (operands[1].val <= $ff) and
                               (operands[2].size in [S_B,S_W,S_L,S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX]) then
                                 p^.concat(new(pai386,op_const_reg(instruc,
                                 operands[2].size,operands[1].val,
                                 operands[2].reg)))
                              else
                              if (operands[1].val <= $ffff) and
                               (operands[2].size in [S_W,S_L,S_IS,S_IL,S_IQ,S_FS,S_FL,S_FX]) then
                                 p^.concat(new(pai386,op_const_reg(instruc,
                                 operands[2].size,operands[1].val,
                                 operands[2].reg)))
                              else
                              if (operands[1].val <= $7fffffff) and
                               (operands[2].size in [S_L,S_IL,S_IQ,S_FS,S_FL,S_FX]) then
                                 p^.concat(new(pai386,op_const_reg(instruc,
                                 operands[2].size,operands[1].val,
                                 operands[2].reg)))
                              else
                               Message(assem_e_invalid_opcode_size);
                           end;
                      else
                         Begin
                           Message(assem_f_internal_error_in_concatopcode);
                         end;
                      end; { end case }
                   { ref,reg     }
                   { ref,ref     }
                   OPR_REFERENCE:
                      case instr.operands[2].operandtype of
                         OPR_REGISTER:
                            if assigned(operands[1].ref.symbol) then
                            { global variable }
                            Begin
                              if instruc in [A_LEA,A_LDS,A_LES,A_LFS,A_LGS,A_LSS]
                               then
                                 p^.concat(new(pai386,op_ref_reg(instruc,
                                 S_NO,newreference(operands[1].ref),
                                 operands[2].reg)))
                              else
                              if (opsize = operands[2].size) then
                                 p^.concat(new(pai386,op_ref_reg(instruc,
                                 opsize,newreference(operands[1].ref),
                                 operands[2].reg)))
                              else
                                Begin
                                   Message(assem_e_invalid_opcode_and_operand);
                                end;
                            end
                            else
                            Begin
                              { register reference }
                              { possiblities:1) local variable which }
                              { has been replaced by bp and offset   }
                              { in this case size should be valid    }
                              {              2) Indirect register    }
                              { adressing, 2nd operand determines    }
                              { size.                                }
                              if (opsize = operands[2].size) or (opsize = S_NO) then
                              Begin
                                 p^.concat(new(pai386,op_ref_reg(instruc,
                                 operands[2].size,newreference(operands[1].ref),
                                 operands[2].reg)));
                              end
                              else
                                  Message(assem_e_invalid_size_in_ref);
                            end;
                         OPR_REFERENCE: { special opcodes }
                            p^.concat(new(pai386,op_ref_ref(instruc,
                            opsize,newreference(operands[1].ref),
                            newreference(operands[2].ref))));
                      else
                         Begin
                           Message(assem_f_internal_error_in_concatopcode);
                         end;
                   end; { end inner case }
                  end; { end case }
                end; { end with }
             end; {end if movsx... }
        end;
     3: Begin
             { only imul, shld and shrd  }
             { middle must be a register }
             if (instruc in [A_SHLD,A_SHRD]) and (instr.operands[2].operandtype =
                OPR_REGISTER) then
             Begin
               case instr.operands[2].size of
                S_W:  if instr.operands[1].operandtype = OPR_CONSTANT then
                        Begin
                          if instr.operands[1].val <= $ff then
                            Begin
                              if instr.operands[3].size in [S_W] then
                              Begin
                                 case instr.operands[3].operandtype of
                                  OPR_REFERENCE: { MISSING !!!! } ;
                                  OPR_REGISTER:  p^.concat(new(pai386,
                                     op_const_reg_reg(instruc, S_W,
                                     instr.operands[1].val, instr.operands[2].reg,
                                     instr.operands[3].reg)));
                                 else
                                    Message(assem_e_invalid_opcode_and_operand);
                                    Message(assem_e_invalid_opcode_and_operand);
                                 end;
                              end
                              else
                                 Message(assem_e_invalid_opcode_and_operand);
                            end;
                        end
                      else
                        Message(assem_e_invalid_opcode_and_operand);
                S_L:  if instr.operands[1].operandtype = OPR_CONSTANT then
                        Begin
                          if instr.operands[1].val <= $ff then
                            Begin
                              if instr.operands[3].size in [S_L] then
                              Begin
                                 case instr.operands[3].operandtype of
                                  OPR_REFERENCE: { MISSING !!!! } ;
                                  OPR_REGISTER:  p^.concat(new(pai386,
                                     op_const_reg_reg(instruc, S_L,
                                     instr.operands[1].val, instr.operands[2].reg,
                                     instr.operands[3].reg)));
                                 else
                                   Message(assem_e_invalid_opcode_and_operand);
                                 end;
                              end
                              else
                                Message(assem_e_invalid_opcode_and_operand);
                            end;
                        end
                      else
                       Message(assem_e_invalid_opcode_and_operand);
                else
                  Message(assem_e_invalid_opcode_and_operand);
               end; { end case }
             end
             else
             if (instruc in [A_IMUL]) and (instr.operands[3].operandtype
               = OPR_REGISTER) then
             Begin
               case instr.operands[3].size of
                S_W:  if instr.operands[1].operandtype = OPR_CONSTANT then
                        Begin
                          if instr.operands[1].val <= $ffff then
                            Begin
                              if instr.operands[2].size in [S_W] then
                              Begin
                                 case instr.operands[2].operandtype of
                                  OPR_REFERENCE: { MISSING !!!! } ;
                                  OPR_REGISTER:  p^.concat(new(pai386,
                                     op_const_reg_reg(instruc, S_W,
                                     instr.operands[1].val, instr.operands[2].reg,
                                     instr.operands[3].reg)));
                                 else
                                  Message(assem_e_invalid_opcode_and_operand);
                                 end; { end case }
                              end
                              else
                                Message(assem_e_invalid_opcode_and_operand);
                            end;
                        end
                      else
                        Message(assem_e_invalid_opcode_and_operand);
                S_L:  if instr.operands[1].operandtype = OPR_CONSTANT then
                        Begin
                          if instr.operands[1].val <= $7fffffff then
                            Begin
                              if instr.operands[2].size in [S_L] then
                              Begin
                                 case instr.operands[2].operandtype of
                                  OPR_REFERENCE: { MISSING !!!! } ;
                                  OPR_REGISTER:  p^.concat(new(pai386,
                                     op_const_reg_reg(instruc, S_L,
                                     instr.operands[1].val, instr.operands[2].reg,
                                     instr.operands[3].reg)));
                                 else
                                   Message(assem_e_invalid_opcode_and_operand);
                                 end; { end case }
                              end
                              else
                               Message(assem_e_invalid_opcode_and_operand);
                            end;
                        end
                      else
                       Message(assem_e_invalid_opcode_and_operand);
                else
                  Message(assem_e_invalid_middle_sized_operand);
               end; { end case }
             end { endif }
             else
               Message(assem_e_invalid_three_operand_opcode);
        end;
  end; { end case }
 end;
 end;

  {---------------------------------------------------------------------}
  {                     Routines for the parsing                        }
  {---------------------------------------------------------------------}

     procedure consume(t : tinteltoken);

     begin
       if t<>actasmtoken then
         Message(assem_e_syntax_error);
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
     for i:=firstreg to lastreg do
       if s = iasmregs[i] then
       Begin
         findregister := i;
         exit;
       end;
   end;


   function findoverride(const s: string; var reg:tregister): boolean;
   var
    i: byte;
   begin
     findoverride := FALSE;
     reg := R_NO;
     for i:=0 to _count_asmoverrides do
     Begin
       if s = _asmoverrides[i] then
       begin
          reg := _overridetokens[i];
          findoverride := TRUE;
          exit;
       end;
     end;
   end;

   function findprefix(const s: string; var token: tasmop): boolean;
   var i: byte;
   Begin
     findprefix := FALSE;
     for i:=0 to _count_asmprefixes do
     Begin
       if s = _asmprefixes[i] then
       begin
          token := _prefixtokens[i];
          findprefix := TRUE;
          exit;
       end;
     end;
   end;


   function findsegment(const s:string): tregister;
  {*********************************************************************}
  { FUNCTION findsegment(s: string):tasmop;                             }
  {  Description: Determines if the s string is a valid segment register}
  {  if so returns correct tregister token, or R_NO if not found.       }
  {*********************************************************************}
   var
    i: tregister;
   Begin
     findsegment := R_DEFAULT_SEG;
     for i:=firstsreg to lastsreg do
       if s = iasmregs[i] then
       Begin
         findsegment := i;
         exit;
       end;
   end;

   function findopcode(const s: string): tasmop;
  {*********************************************************************}
  { FUNCTION findopcode(s: string): tasmop;                             }
  {  Description: Determines if the s string is a valid opcode          }
  {  if so returns correct tasmop token.                                }
  {*********************************************************************}
   var
    i: tasmop;
    j: byte;
   Begin
     findopcode := A_NONE;
     for i:=firstop to lastop do
       if  s = iasmops^[i] then
       begin
          findopcode:=i;
          exit;
       end;
     { not found yet, search for extended opcodes }
     { now, in this case, we must use the suffix  }
     { to determine the size of the instruction   }
     for j:=0 to _count_asmspecialops do
     Begin
       if s = _specialops[j] then
       Begin
         findopcode := _specialopstokens[j];
         { set the size }
         case s[length(s)] of
         'B': instr.stropsize := S_B;
         'D': instr.stropsize := S_L;
         'W': instr.stropsize := S_W;
         end;
         exit;
       end;
     end;
   end;






   Function CheckPrefix(prefix: tasmop; opcode:tasmop): Boolean;
   { Checks if the prefix is valid with the following instruction }
   { return false if not, otherwise true                          }
   Begin
     CheckPrefix := TRUE;
     Case prefix of
       A_REP,A_REPNE,A_REPE: if not (opcode in [A_SCAS,A_INS,A_OUTS,A_MOVS,
                             A_CMPS,A_LODS,A_STOS]) then
                             Begin
                               CheckPrefix := FALSE;
                               exit;
                             end;
       A_LOCK: if not (opcode in [A_BT,A_BTS,A_BTR,A_BTC,A_XCHG,A_ADD,A_OR,
                        A_ADC,A_SBB,A_AND,A_SUB,A_XOR,A_NOT,A_NEG,A_INC,A_DEC]) then
                  Begin
                     CheckPrefix := FALSE;
                     Exit;
                  end;
       A_NONE: exit; { no prefix here }

     else
       CheckPrefix := FALSE;
     end; { end case }
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
        case operands[operandnum].operandtype of
          OPR_REFERENCE: exit;
          OPR_NONE: ;
        else
          Message(assem_e_invalid_operand_type);
        end;
        operands[operandnum].operandtype := OPR_REFERENCE;
        operands[operandnum].ref.segment := R_DEFAULT_SEG;
     end;
   end;

   Function CheckOverride(segreg: tregister; var instr: TInstruction): Boolean;
   { Check if the override is valid, and if so then }
   { update the instr variable accordingly.         }
   Begin
     CheckOverride := FALSE;
     if instr.getinstruction in [A_MOVS,A_XLAT,A_CMPS] then
     Begin
       CheckOverride := TRUE;
       Message(assem_e_segment_override_not_supported);
     end
   end;




  Function CalculateExpression(expression: string): longint;
  var
    expr: TExprParse;
  Begin
   expr.Init;
   CalculateExpression := expr.Evaluate(expression);
   expr.Done;
  end;







  Function BuildRefExpression: longint;
  {*********************************************************************}
  { FUNCTION BuildExpression: longint                                   }
  {  Description: This routine calculates a constant expression to      }
  {  a given value. The return value is the value calculated from       }
  {  the expression.                                                    }
  { The following tokens (not strings) are recognized:                  }
  {    (,),SHL,SHR,/,*,NOT,OR,XOR,AND,MOD,+/-,numbers,ID to constants.  }
  {*********************************************************************}
  { ENTRY: On entry the token should be any valid expression token.     }
  { EXIT:  On Exit the token points to any token after the closing      }
  {         RBRACKET                                                    }
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
    { tell tokenizer that we are in }
    { an expression.                }
    inexpression := TRUE;
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
      { End of reference }
      AS_RBRACKET: Begin
                     if not ErrorFlag then
                        BuildRefExpression := CalculateExpression(expr)
                     else
                        BuildRefExpression := 0;
                     Consume(AS_RBRACKET);
                     { no longer in an expression }
                     inexpression := FALSE;
                     exit;
                  end;
      AS_ID:
                Begin
                  if NOT SearchIConstant(actasmpattern,l) then
                  Begin
                    Message1(assem_e_invalid_const_symbol,actasmpattern);
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
                      tempstr := BinaryToDec(actasmpattern);
                      if tempstr = '' then
                       Message(assem_f_error_converting_bin);
                      expr:=expr+tempstr;
                      Consume(AS_BINNUM);
                 end;

      AS_HEXNUM: Begin
                    tempstr := HexToDec(actasmpattern);
                    if tempstr = '' then
                     Message(assem_f_error_converting_hex);
                    expr:=expr+tempstr;
                    Consume(AS_HEXNUM);
                end;
      AS_OCTALNUM: Begin
                    tempstr := OctalToDec(actasmpattern);
                    if tempstr = '' then
                     Message(assem_f_error_converting_octal);
                    expr:=expr+tempstr;
                    Consume(AS_OCTALNUM);
                  end;
      else
        Begin
          { write error only once. }
          if not errorflag then
           Message(assem_e_invalid_constant_expression);
          BuildRefExpression := 0;
          if actasmtoken in [AS_COMMA,AS_SEPARATOR] then exit;
          { consume tokens until we find COMMA or SEPARATOR }
          Consume(actasmtoken);
          errorflag := TRUE;
        end;
      end;
    Until false;
  end;



  Procedure BuildRecordOffset(var instr: TInstruction; varname: string);
  {*********************************************************************}
  { PROCEDURE BuildRecordOffset(var Instr: TInstruction)                }
  { Description: This routine takes care of field specifiers of records }
  {  and/or variables in asm operands. It updates the offset accordingly}
  {*********************************************************************}
  { ENTRY: On entry the token should be DOT.                            }
  {    name: should be the name of the variable to be expanded. '' if   }
  {     no variabled specified.                                         }
  { EXIT:  On Exit the token points to SEPARATOR or COMMA.              }
  { ERROR RECOVERY: Tries to find COMMA or SEPARATOR token by consuming }
  {  invalid tokens.                                                    }
  {*********************************************************************}
  var
    firstpass: boolean;
    offset: longint;
    basetypename : string;
  Begin
    basetypename := '';
    firstpass := TRUE;
    { // .ID[REG].ID ...   // }
    { // .ID.ID...         // }
    Consume(AS_DOT);
    Repeat
      case actasmtoken of
        AS_ID: Begin
                  InitAsmRef(instr);
                  { // var_name.typefield.typefield // }
                  if (varname <> '') then
                  Begin
                    if not GetVarOffset(varname,actasmpattern,offset) then
                    Begin
                      Message1(assem_e_unknown_id,actasmpattern);
                    end
                    else
                      Inc(instr.operands[operandnum].ref.offset,Offset);
                  end
                  else
                 {    [ref].var_name.typefield.typefield ...                }
                 {    [ref].var_name[reg]                                   }
                  if not assigned(instr.operands[operandnum].ref.symbol) and
                    firstpass then
                  Begin
                     if not CreateVarInstr(instr,actasmpattern,operandnum) then
                     Begin
                       { type field ? }
                       basetypename := actasmpattern;
                     end
                     else
                       varname := actasmpattern;
                    end
                  else
                  if firstpass then
                 {    [ref].typefield.typefield ...                         }
                 {    where the first typefield must specifiy the base      }
                 {    object or record type.                                }
                  Begin
                     basetypename := actasmpattern;
                  end
                  else
                 {    [ref].typefield.typefield ...                         }
                 {  basetpyename is already set up... now look for fields.  }
                  Begin
                     if not GetTypeOffset(basetypename,actasmpattern,Offset) then
                     Begin
                      Message1(assem_e_unknown_id,actasmpattern);
                     end
                     else
                       Inc(instr.operands[operandnum].ref.offset,Offset);
                  end;
                  Consume(AS_ID);
                 { Take care of index register on this variable }
                 if actasmtoken = AS_LBRACKET then
                 Begin
                   Consume(AS_LBRACKET);
                   Case actasmtoken of
                     AS_REGISTER: Begin
                                   if instr.operands[operandnum].ref.index <> R_NO then
                                    Message(assem_e_defining_index_more_than_once);
                                   instr.operands[operandnum].ref.index :=
                                      findregister(actasmpattern);
                                   Consume(AS_REGISTER);
                                  end;
                    else
                     Begin
                      { add offsets , assuming these are constant expressions... }
                      Inc(instr.operands[operandnum].ref.offset,BuildRefExpression);
                     end;
                   end;
                   Consume(AS_RBRACKET);
                 end;
                 { Here we should either have AS_DOT, AS_SEPARATOR or AS_COMMA }
                 if actasmtoken = AS_DOT then
                    Consume(AS_DOT);
                 firstpass := FALSE;
                 Offset := 0;
              end;
        AS_SEPARATOR: exit;
        AS_COMMA: exit;
      else
       Begin
         Message(assem_e_invalid_field_specifier);
         Consume(actasmtoken);
         firstpass := FALSE;
       end;
      end; { end case }
    Until (actasmtoken = AS_SEPARATOR) or (actasmtoken = AS_COMMA);
  end;


  Function BuildExpression: longint;
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
      tempstr: string;
      l : longint;
      errorflag: boolean;
  Begin
    errorflag := FALSE;
    expr := '';
    tempstr := '';
    { tell tokenizer that we are in an expression. }
    inexpression := TRUE;
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
                  if NOT SearchIConstant(actasmpattern,l) then
                  Begin
                    Message1(assem_e_invalid_const_symbol,actasmpattern);
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
                      tempstr := BinaryToDec(actasmpattern);
                      if tempstr = '' then
                       Message(assem_f_error_converting_bin);
                      expr:=expr+tempstr;
                      Consume(AS_BINNUM);
                 end;

      AS_HEXNUM: Begin
                    tempstr := HexToDec(actasmpattern);
                    if tempstr = '' then
                     Message(assem_f_error_converting_hex);
                    expr:=expr+tempstr;
                    Consume(AS_HEXNUM);
                end;
      AS_OCTALNUM: Begin
                    tempstr := OctalToDec(actasmpattern);
                    if tempstr = '' then
                     Message(assem_f_error_converting_octal);
                    expr:=expr+tempstr;
                    Consume(AS_OCTALNUM);
                  end;
      { go to next term }
      AS_COMMA: Begin
                  if not ErrorFlag then
                    BuildExpression := CalculateExpression(expr)
                  else
                    BuildExpression := 0;
                  inexpression := FALSE;
                  Exit;
               end;
      { go to next symbol }
      AS_SEPARATOR: Begin
                      if not ErrorFlag then
                        BuildExpression := CalculateExpression(expr)
                      else
                        BuildExpression := 0;
                      inexpression := FALSE;
                      Exit;
                   end;
      else
        Begin
          { only write error once. }
          if not errorflag then
           Message(assem_e_invalid_constant_expression);
          { consume tokens until we find COMMA or SEPARATOR }
          Consume(actasmtoken);
          errorflag := TRUE;
        End;
      end;
    Until false;
  end;




  Procedure BuildScaling(Var instr: TInstruction);
  {*********************************************************************}
  {  Takes care of parsing expression starting from the scaling value   }
  {  up to and including possible field specifiers.                     }
  { EXIT CONDITION:  On exit the routine should point to  AS_SEPARATOR  }
  { or AS_COMMA. On entry should point to AS_STAR token.                }
  {*********************************************************************}
  var str:string;
      l: longint;
      code: integer;
  Begin
     Consume(AS_STAR);
     if (instr.operands[operandnum].ref.scalefactor <> 0)
     and (instr.operands[operandnum].ref.scalefactor <> 1) then
     Begin
         Message(assem_f_internal_error_in_buildscale);
     end;
     case actasmtoken of
        AS_INTNUM: str := actasmpattern;
        AS_HEXNUM: str := HexToDec(actasmpattern);
        AS_BINNUM: str := BinaryToDec(actasmpattern);
        AS_OCTALNUM: str := OctalToDec(actasmpattern);
     else
        Message(assem_e_syntax_error);
     end;
     val(str, l, code);
     if code <> 0 then
       Message(assem_e_invalid_scaling_factor);
     if ((l = 2) or (l = 4) or (l = 8) or (l = 1)) and (code = 0) then
     begin
        instr.operands[operandnum].ref.scalefactor := l;
     end
     else
     Begin
        Message(assem_e_invalid_scaling_value);
        instr.operands[operandnum].ref.scalefactor := 0;
     end;
     if instr.operands[operandnum].ref.index = R_NO then
     Begin
        Message(assem_e_scaling_value_only_allowed_with_index);
        instr.operands[operandnum].ref.scalefactor := 0;
     end;
    { Consume the scaling number }
    Consume(actasmtoken);
    case actasmtoken of
        { //  [...*SCALING-expr] ... // }
        AS_MINUS: Begin
                    if instr.operands[operandnum].ref.offset <> 0 then
                     Message(assem_f_internal_error_in_buildscale);
                    instr.operands[operandnum].ref.offset :=
                        BuildRefExpression;
                  end;
        { //  [...*SCALING+expr] ... // }
        AS_PLUS: Begin
                    if instr.operands[operandnum].ref.offset <> 0 then
                     Message(assem_f_internal_error_in_buildscale);
                    instr.operands[operandnum].ref.offset :=
                         BuildRefExpression;
                    end;
        { //  [...*SCALING] ... // }
        AS_RBRACKET: Consume(AS_RBRACKET);
    else
       Message(assem_e_invalid_scaling_value);
    end;
    { // .Field.Field ... or separator/comma // }
    Case actasmtoken of
     AS_DOT: BuildRecordOffset(instr,'');
     AS_COMMA, AS_SEPARATOR: ;
    else
      Message(assem_e_syntax_error);
    end;
  end;



  Procedure BuildReference(var instr: TInstruction);
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to either the     }
  {       AS_COMMA or AS_SEPARATOR token.                               }
  {   On entry: contains the register after the opening bracket if any. }
  {*********************************************************************}
  var
    reg:string;
    segreg: boolean;
    negative: boolean;
    expr: string;
  Begin
     expr := '';
     if instr.operands[operandnum].operandtype <> OPR_REFERENCE then
     Begin
       Message(assem_e_syn_no_ref_with_brackets);
       InitAsmRef(instr);
       consume(AS_REGISTER);
     end
     else
     Begin
       { save the reg }
       reg := actasmpattern;
       { is the syntax of the form: [REG:REG...] }
       consume(AS_REGISTER);
       if actasmtoken = AS_COLON then
       begin
         segreg := TRUE;
         Message(assem_e_expression_form_not_supported);
         if instr.operands[operandnum].ref.segment <> R_NO then
          Message(assem_e_defining_seg_more_than_once);
         instr.operands[operandnum].ref.segment := findsegment(reg);
         { Here we should process the syntax of the form   }
         { [reg:reg...]                                    }
         {!!!!!!!!!!!!!!!!!!!!!!!!                         }
       end
       { This is probably of the following syntax: }
       { SREG:[REG...] where SReg: is optional.    }
       { Therefore we immediately say that reg     }
       { is the base.                              }
       else
       Begin
         if instr.operands[operandnum].ref.base <> R_NO then
          Message(assem_e_defining_base_more_than_once);
         instr.operands[operandnum].ref.base := findregister(reg);
       end;
       { we process this type of syntax immediately... }
       case actasmtoken of

          { //  REG:[REG].Field.Field ...     // }
          { //  REG:[REG].Field[REG].Field... // }
         AS_RBRACKET: Begin
                       Consume(AS_RBRACKET);
                       { check for record fields }
                       if actasmtoken = AS_DOT then
                          BuildRecordOffset(instr,'');
                       if (actasmtoken = AS_SEPARATOR) or (actasmtoken = AS_COMMA) then
                         exit
                       else
                         Message(assem_e_syn_reference);
                     end;
          { //  REG:[REG +/- ...].Field.Field ... // }
         AS_PLUS,AS_MINUS: Begin
                            if actasmtoken = AS_MINUS then
                            Begin
                               expr := '-';
                               negative := TRUE
                            end
                            else
                            Begin
                               negative := FALSE;
                               expr := '+';
                            end;
                            Consume(actasmtoken);
                            { // REG:[REG+REG+/-...].Field.Field // }
                            if actasmtoken = AS_REGISTER then
                            Begin
                              if negative then
                                Message(assem_e_negative_index_register);
                              if instr.operands[operandnum].ref.index <> R_NO then
                                Message(assem_e_defining_index_more_than_once);
                              instr.operands[operandnum].ref.index := findregister(actasmpattern);
                              Consume(AS_REGISTER);
                              case actasmtoken of
                                AS_RBRACKET: { // REG:[REG+REG].Field.Field... // }
                                            Begin
                                              Consume(AS_RBRACKET);
                                              Case actasmtoken of
                                                 AS_DOT: BuildRecordOffset(instr,'');
                                                 AS_COMMA,AS_SEPARATOR: exit;
                                              else
                                                Message(assem_e_syntax_error);
                                              end
                                             end;
                                AS_PLUS,AS_MINUS: { // REG:[REG+REG+/-expr].Field.Field... // }
                                                Begin
                                                  if instr.operands[operandnum].ref.offset <> 0 then
                                                   Message(assem_f_internal_error_in_buildreference);
                                                  instr.operands[operandnum].ref.offset :=
                                                      BuildRefExpression;
                                                  case actasmtoken of
                                                    AS_DOT: BuildRecordOffset(instr,'');
                                                    AS_COMMA,AS_SEPARATOR: ;
                                                  else
                                                    Message(assem_e_syntax_error);
                                                  end; { end case }
                                                end;
                                AS_STAR: Begin  { // REG:[REG+REG*SCALING...].Field.Field... // }
                                             BuildScaling(instr);
                                         end;
                                else
                                Begin
                                  Message(assem_e_syntax_error);
                                end;
                              end; { end case }
                            end
                            else if actasmtoken = AS_STAR then
                            { // REG:[REG*SCALING ... ]     // }
                            Begin
                              BuildScaling(instr);
                            end
                            else
                            { // REG:[REG+expr].Field.Field // }
                             Begin
                               if instr.operands[operandnum].ref.offset <> 0 then
                                Message(assem_f_internal_error_in_buildreference);
                               instr.operands[operandnum].ref.offset := BuildRefExpression;
                               case actasmtoken of
                                  AS_DOT: BuildRecordOffset(instr,'');
                                  AS_COMMA,AS_SEPARATOR: ;
                                else
                                  Message(assem_e_syntax_error);
                               end; { end case }
                             end; { end if }
                         end; { end this case }
     { //  REG:[REG*scaling] ... // }
         AS_STAR: Begin
                     BuildScaling(instr);
                 end;
       end;
     end; { end outer if }
  end;


  Procedure BuildBracketExpression(var Instr: TInstruction; var_prefix: boolean);
  {*********************************************************************}
  { PROCEDURE BuildBracketExpression                                    }
  {  Description: This routine builds up an expression after a LBRACKET }
  {  token is encountered.                                              }
  {   On entry actasmtoken should be equal to AS_LBRACKET.              }
  {  var_prefix : Should be set to true if variable identifier has      }
  {    been defined, such as in ID[                                     }
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to either the     }
  {       AS_COMMA or AS_SEPARATOR token.                               }
  {*********************************************************************}
  var
    l:longint;
  Begin
     Consume(AS_LBRACKET);
     initAsmRef(instr);
     Case actasmtoken of
         { // Constant reference expression OR variable reference expression // }
         AS_ID: Begin
                if actasmpattern[1] = '@' then
                 Message(assem_e_local_symbol_not_allowed_as_ref);
                if SearchIConstant(actasmpattern,l) then
                 Begin
                   { if there was a variable prefix then }
                   { add to offset                       }
                   If var_prefix then
                    Begin
                        Inc(instr.operands[operandnum].ref.offset,  BuildRefExpression);
                    end
                   else
                     instr.operands[operandnum].ref.offset :=BuildRefExpression;
                   if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                      Message(assem_e_invalid_operand_in_bracket_expression);
                 end
                else if NOT var_prefix then
                 Begin
                    InitAsmRef(instr);
                    if not CreateVarInstr(instr,actasmpattern,operandnum) then
                     Message1(assem_e_unknown_id,actasmpattern);
                    Consume(AS_ID);
                   { is there a constant expression following }
                   { the variable name?                       }
                   if actasmtoken <> AS_RBRACKET then
                    Begin
                      Inc(instr.operands[operandnum].ref.offset, BuildRefExpression);
                    end
                   else
                      Consume(AS_RBRACKET);
                 end
                 else
                   Message1(assem_e_invalid_symbol_name,actasmpattern);
                end;
               { Here we handle the special case in tp where   }
               { the + operator is allowed with reg and var    }
               { references, such as in mov al, byte ptr [+bx] }
         AS_PLUS: Begin
                   Consume(AS_PLUS);
                   Case actasmtoken of
                     AS_REGISTER: Begin
                                   BuildReference(instr);
                                 end;
                     AS_ID: Begin
                             if actasmpattern[1] = '@' then
                               Message(assem_e_local_symbol_not_allowed_as_ref);
                             if SearchIConstant(actasmpattern,l) then
                               Begin
                                 { if there was a variable prefix then }
                                 { add to offset                       }
                                 If var_prefix then
                                  Begin
                                    Inc(instr.operands[operandnum].ref.offset,
                                     BuildRefExpression);
                                  end
                                 else
                                   instr.operands[operandnum].ref.offset :=
                                    BuildRefExpression;
                                 if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                                   Message(assem_e_invalid_operand_in_bracket_expression);
                               end
                             else if NOT var_prefix then
                               Begin
                               InitAsmRef(instr);
                               if not CreateVarInstr(instr,actasmpattern,operandnum) then
                                Message1(assem_e_unknown_id,actasmpattern);
                               Consume(AS_ID);
                               { is there a constant expression following }
                               { the variable name?                       }
                                 if actasmtoken <> AS_RBRACKET then
                                   Begin
                                    Inc(instr.operands[operandnum].ref.offset,
                                      BuildRefExpression);
                                   end
                                 else
                                   Consume(AS_RBRACKET);
                               end
                             else
                               Message1(assem_e_invalid_symbol_name,actasmpattern);
                           end;
                     { // Constant reference expression //  }
                   AS_INTNUM,AS_BINNUM,AS_OCTALNUM,
                   AS_HEXNUM: Begin
                               { if there was a variable prefix then }
                               { add to offset instead.              }
                               If var_prefix then
                                Begin
                                  Inc(instr.operands[operandnum].ref.offset,  BuildRefExpression);
                                end
                               else
                               Begin
                                 instr.operands[operandnum].ref.offset :=BuildRefExpression;
                               end;
                               if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                                  Message(assem_e_invalid_operand_in_bracket_expression);
                             end;
                    else
                      Message(assem_e_syntax_error);
                   end;
                 end;
         { // Constant reference expression //  }
         AS_MINUS,AS_NOT,AS_LPAREN:
                     Begin
                       { if there was a variable prefix then }
                       { add to offset instead.              }
                       If var_prefix then
                         Begin
                              Inc(instr.operands[operandnum].ref.offset,  BuildRefExpression);
                         end
                        else
                         Begin
                           instr.operands[operandnum].ref.offset :=BuildRefExpression;
                         end;
                       if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                          Message(assem_e_invalid_operand_in_bracket_expression);
                     end;
         { // Constant reference expression //  }
         AS_INTNUM,AS_OCTALNUM,AS_BINNUM,AS_HEXNUM: Begin
                       { if there was a variable prefix then }
                       { add to offset instead.              }
                       If var_prefix then
                         Begin
                              Inc(instr.operands[operandnum].ref.offset,  BuildRefExpression);
                         end
                        else
                         Begin
                           instr.operands[operandnum].ref.offset :=BuildRefExpression;
                         end;
                       if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                          Message(assem_e_invalid_operand_in_bracket_expression);
                   end;
         { // Variable reference expression // }
         AS_REGISTER: BuildReference(instr);
     else
       Begin
         Message(assem_e_invalid_reference_syntax);
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
    hl: plabel;
  Begin
   tempstr := '';
   expr := '';
   case actasmtoken of
   { // Constant expression //  }
     AS_PLUS,AS_MINUS,AS_NOT,AS_LPAREN:
                                  Begin
                                     if not (instr.operands[operandnum].operandtype in [OPR_NONE,OPR_CONSTANT]) then
                                        Message(assem_e_invalid_operand_type);
                                     instr.operands[operandnum].operandtype := OPR_CONSTANT;
                                     instr.operands[operandnum].val :=BuildExpression;
                                   end;
   { // Constant expression //  }
     AS_STRING:   Begin
                    if not (instr.operands[operandnum].operandtype in [OPR_NONE]) then
                       Message(assem_e_invalid_operand_type);
                    instr.operands[operandnum].operandtype := OPR_CONSTANT;
                    if not PadZero(actasmpattern,4) then
                     Message1(assem_e_invalid_string_as_opcode_operand,actasmpattern);
                    instr.operands[operandnum].val :=
                      ord(actasmpattern[4]) + ord(actasmpattern[3]) shl 8 +
                       Ord(actasmpattern[2]) shl 16 + ord(actasmpattern[1])
                        shl 24;
                    Consume(AS_STRING);
                    Case actasmtoken of
                       AS_COMMA, AS_SEPARATOR: ;
                    else
                      Message(assem_e_invalid_string_expression);
                    end; { end case }
                 end;
   { // Constant expression //  }
     AS_INTNUM,AS_BINNUM,
     AS_OCTALNUM,
     AS_HEXNUM:     Begin
                      if not (instr.operands[operandnum].operandtype in [OPR_NONE,OPR_CONSTANT]) then
                         Message(assem_e_invalid_operand_type);
                      instr.operands[operandnum].operandtype := OPR_CONSTANT;
                      instr.operands[operandnum].val :=BuildExpression;
                    end;
   { // A constant expression, or a Variable ref. // }
     AS_ID:  Begin
              if actasmpattern[1] = '@' then
              { // Label or Special symbol reference // }
              Begin
                 if actasmpattern = '@RESULT' then
                   Begin
                      InitAsmRef(instr);
                      SetUpResult(instr,operandnum);
                   end
                 else
                  if (actasmpattern = '@CODE') or (actasmpattern = '@DATA') then
                      Message(assem_w_CODE_and_DATA_not_supported)
                   else
                  Begin
                    delete(actasmpattern,1,1);
                    if actasmpattern = '' then
                      Message(assem_e_null_label_ref_not_allowed);
                    lab := labellist.search(actasmpattern);
                    { check if the label is already defined   }
                    { if so, we then check if the plabel is   }
                    { non-nil, if so we add it to instruction }
                    if assigned(lab) then
                     Begin
                     if assigned(lab^.lab) then
                       Begin
                         instr.operands[operandnum].operandtype := OPR_LABINSTR;
                         instr.operands[operandnum].hl := lab^.lab;
                         instr.labeled := TRUE;
                       end;
                     end
                    else
                    { the label does not exist, create it }
                    { emit the opcode, but set that the   }
                    { label has not been emitted          }
                     Begin
                        getlabel(hl);
                        labellist.insert(actasmpattern,hl,FALSE);
                        instr.operands[operandnum].operandtype := OPR_LABINSTR;
                        instr.operands[operandnum].hl := hl;
                        instr.labeled := TRUE;
                     end;
                  end;
                Consume(AS_ID);
                if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                Begin
                  Message(assem_e_syntax_error);
                end;
              end
              { probably a variable or normal expression }
              { or a procedure (such as in CALL ID)      }
              else
               Begin
                   { is it a constant ? }
                   if SearchIConstant(actasmpattern,l) then
                   Begin
                      if not (instr.operands[operandnum].operandtype in [OPR_NONE,OPR_CONSTANT]) then
                       Message(assem_e_invalid_operand_type);
                      instr.operands[operandnum].operandtype := OPR_CONSTANT;
                      instr.operands[operandnum].val :=BuildExpression;
                    end
                   else { is it a label variable ? }
                    Begin
                     { // ID[ , ID.Field.Field or simple ID // }
                     { check if this is a label, if so then }
                     { emit it as a label.                  }
                     if SearchLabel(actasmpattern,hl) then
                     Begin
                        instr.operands[operandnum].operandtype := OPR_LABINSTR;
                        instr.operands[operandnum].hl := hl;
                        instr.labeled := TRUE;
                        Consume(AS_ID);
                        if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                          Message(assem_e_syntax_error);
                     end
                     else
                     { is it a normal variable ? }
                     Begin
                      initAsmRef(instr);
                      if not CreateVarInstr(instr,actasmpattern,operandnum) then
                      Begin
                         { not a variable.. }
                         { check special variables.. }
                         if actasmpattern = 'SELF' then
                          { special self variable }
                         Begin
                           if assigned(procinfo._class) then
                             Begin
                               instr.operands[operandnum].ref.offset := procinfo.ESI_offset;
                               instr.operands[operandnum].ref.base := procinfo.framepointer;
                             end
                           else
                             Message(assem_e_cannot_use_SELF_outside_a_method);
                         end
                         else
                           Message1(assem_e_unknown_id,actasmpattern);
                      end;
                      expr := actasmpattern;
                      Consume(AS_ID);
                      case actasmtoken of
                           AS_LBRACKET: { indexing }
                                        BuildBracketExpression(instr,TRUE);
                           AS_DOT: BuildRecordOffset(instr,expr);

                           AS_SEPARATOR,AS_COMMA: ;
                      else
                           Message(assem_e_syntax_error);
                      end;
                     end;
                    end;
               end;
            end;
   { // Register, a variable reference or a constant reference // }
     AS_REGISTER: Begin
                   { save the type of register used. }
                   tempstr := actasmpattern;
                   Consume(AS_REGISTER);
                   if actasmtoken = AS_COLON then
                   Begin
                      Consume(AS_COLON);
                      if actasmtoken <> AS_LBRACKET then
                        Message(assem_e_syn_start_with_bracket)
                      else
                      Begin
                        initAsmRef(instr);
                        instr.operands[operandnum].ref.segment := findsegment(tempstr);
                        BuildBracketExpression(instr,false);
                      end;
                   end
                   { // Simple register // }
                   else if (actasmtoken = AS_SEPARATOR) or (actasmtoken = AS_COMMA) then
                   Begin
                        if not (instr.operands[operandnum].operandtype in [OPR_NONE,OPR_REGISTER]) then
                         Message(assem_e_invalid_operand_type);
                        instr.operands[operandnum].operandtype := OPR_REGISTER;
                        instr.operands[operandnum].reg := findregister(tempstr);
                   end
                   else
                    Message1(assem_e_syn_register,tempstr);
                 end;
    { // a variable reference, register ref. or a constant reference // }
     AS_LBRACKET: Begin
                   BuildBracketExpression(instr,false);
                 end;
    { // Unsupported // }
     AS_SEG,AS_OFFSET: Begin
                         Message(assem_e_SEG_and_OFFSET_not_supported);
                         Consume(actasmtoken);
                         { error recovery }
                         While not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                           Consume(actasmtoken);
                       end;
     AS_SEPARATOR, AS_COMMA: ;
    else
      Message(assem_e_syn_opcode_operand);
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
   value : longint;
  Begin
      strlength := 0; { assume it is a DB }
      Repeat
        Case actasmtoken of
          AS_STRING: Begin
                      if maxvalue = $ffff then
                         strlength := 2
                      else if maxvalue = $ffffffff then
                         strlength := 4;
                      if strlength <> 0 then
                      { DD and DW cases }
                      Begin
                         if Not PadZero(actasmpattern,strlength) then
                          Message(scan_f_string_exceeds_line);
                      end;
                      expr := actasmpattern;
                      Consume(AS_STRING);
                      Case actasmtoken of
                       AS_COMMA: Consume(AS_COMMA);
                       AS_SEPARATOR: ;
                      else
                       Message(assem_e_invalid_string_expression);
                      end; { end case }
                      ConcatString(p,expr);
                    end;
          AS_INTNUM,AS_BINNUM,
          AS_OCTALNUM,AS_HEXNUM:
                    Begin
                      value:=BuildExpression;
                      ConcatConstant(p,value,maxvalue);
                    end;
          AS_ID:
                     Begin
                      value:=BuildExpression;
                      if value > maxvalue then
                      Begin
                         Message(assem_e_expression_out_of_bounds);
                         { assuming a value of maxvalue }
                         value := maxvalue;
                      end;
                      ConcatConstant(p,value,maxvalue);
                  end;
          { These terms can start an assembler expression }
          AS_PLUS,AS_MINUS,AS_LPAREN,AS_NOT: Begin
                                          value := BuildExpression;
                                          ConcatConstant(p,value,maxvalue);
                                         end;
          AS_COMMA:  BEGIN
                       Consume(AS_COMMA);
                     END;
          AS_SEPARATOR: ;

        else
         Begin
           Message(assem_f_internal_error_in_buildconstant);
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

    { //  prefix seg opcode               // }
    { //  prefix opcode                   // }
    if findprefix(actasmpattern,asmtok) then
    Begin
     { standard opcode prefix }
     if asmtok <> A_NONE then
       instr.addprefix(asmtok);
     Consume(AS_OPCODE);
     if findoverride(actasmpattern,segreg) then
     Begin
       Consume(AS_OPCODE);
       Message(assem_w_repeat_prefix_and_seg_override);
     end;
    end
    else
    { //  seg prefix opcode               // }
    { //  seg opcode                      // }
    if findoverride(actasmpattern,segreg) then
    Begin
      Consume(AS_OPCODE);
      if findprefix(actasmpattern,asmtok) then
      Begin
     { standard opcode prefix }
        Message(assem_w_repeat_prefix_and_seg_override);
        if asmtok <> A_NONE then
          instr.addprefix(asmtok);
        Consume(AS_OPCODE);
      end;
    end;
    { //  opcode                          // }
    if (actasmtoken <> AS_OPCODE) then
    Begin
      Message(assem_e_invalid_or_missing_opcode);
      { error recovery }
      While not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
         Consume(actasmtoken);
      exit;
    end
    else
    Begin
      op := findopcode(actasmpattern);
      instr.addinstr(op);
      { // Valid combination of prefix and instruction ? // }
      if (asmtok <> A_NONE) and (NOT CheckPrefix(asmtok,op)) then
        Message1(assem_e_invalid_prefix_and_opcode,actasmpattern);
      { // Valid combination of segment override // }
      if (segreg <> R_NO) and (NOT CheckOverride(segreg,instr)) then
        Message1(assem_e_invalid_override_and_opcode,actasmpattern);
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
                    Message(assem_e_too_many_operands)
                  else
                    Inc(operandnum);
                  Consume(AS_COMMA);
                end;
         { // Typecast, Constant Expression, Type Specifier // }
         AS_DWORD,AS_BYTE,AS_WORD,AS_TBYTE,AS_QWORD: Begin
                                  Case actasmtoken of
                                   AS_DWORD: instr.operands[operandnum].size := S_L;
                                   AS_WORD:  instr.operands[operandnum].size := S_W;
                                   AS_BYTE:  instr.operands[operandnum].size := S_B;
                                   AS_QWORD: instr.operands[operandnum].size := S_IQ;
                                   AS_TBYTE: instr.operands[operandnum].size := S_FX;
                                  end;
                                  Consume(actasmtoken);
                                  Case actasmtoken of
                                  { // Reference // }
                                  AS_PTR: Begin
                                           initAsmRef(instr);
                                           Consume(AS_PTR);
                                           BuildOperand(instr);
                                         end;
                                  { // Possibly a typecast or a constant // }
                                  { // expression.                       // }
                                  AS_LPAREN: Begin
                                              if actasmtoken = AS_ID then
                                              Begin
                                                { Case vartype of                }
                                                {  LOCAL: Replace by offset and  }
                                                {         BP in treference.      }
                                                {  GLOBAL: Replace by mangledname}
                                                {    in symbol of treference     }
                                                { Check if next token = RPAREN   }
                                                { otherwise syntax error.        }
                                                initAsmRef(instr);
                                                if not CreateVarInstr(instr,actasmpattern,
                                                   operandnum) then
                                                Begin
                                                   Message1(assem_e_unknown_id,actasmpattern);
                                                end;
                                              end
                                              else
                                               begin
                                                 instr.operands[operandnum].operandtype := OPR_CONSTANT;
                                                 instr.operands[operandnum].val := BuildExpression;
                                               end;
                                            end;
                                  else
                                    BuildOperand(instr);
                                  end; { end case }
                            end;
         { // Type specifier // }
         AS_NEAR,AS_FAR: Begin
                          if actasmtoken = AS_NEAR then
                            Message(assem_w_near_ignored)
                          else
                            Message(assem_w_far_ignored);
                          Consume(actasmtoken);
                          if actasmtoken = AS_PTR then
                           begin
                             initAsmRef(instr);
                             Consume(AS_PTR);
                           end;
                           BuildOperand(instr);
                       end;
         { // End of asm operands for this opcode // }
         AS_SEPARATOR: ;
         { // Constant expression // }
         AS_LPAREN: Begin
                      instr.operands[operandnum].operandtype := OPR_CONSTANT;
                      instr.operands[operandnum].val := BuildExpression;
                    end;
       else
         BuildOperand(instr);
     end; { end case }
    end; { end while }
  end;


  Function Assemble: Ptree;
  {*********************************************************************}
  { PROCEDURE Assemble;                                                 }
  {  Description: Parses the intel assembler syntax, parsing is done    }
  {  according to the rules in the Turbo Pascal manual.                 }
  {*********************************************************************}
  Var
   hl: plabel;
   labelptr: pasmlabel;
  Begin
    Message(assem_d_start_intel);
    inexpression := FALSE;
    firsttoken := TRUE;
    operandnum := 0;
    if assigned(procinfo.retdef) and
       (is_fpu(procinfo.retdef) or
       ret_in_acc(procinfo.retdef)) then
      procinfo.funcret_is_valid:=true;
   { sets up all opcode and register tables in uppercase }
    if not _asmsorted then
    Begin
      SetupTables;
      _asmsorted := TRUE;
    end;
    p:=new(paasmoutput,init);
    { setup label linked list }
    labellist.init;
    c:=asmgetchar;
    actasmtoken:=gettoken;
    while actasmtoken<>AS_END do
    Begin
      case actasmtoken of
        AS_LLABEL: Begin
                    labelptr := labellist.search(actasmpattern);
                    if not assigned(labelptr) then
                    Begin
                        getlabel(hl);
                        labellist.insert(actasmpattern,hl,TRUE);
                        ConcatLabel(p,A_LABEL,hl);
                    end
                    else
                    { the label has already been inserted into the  }
                    { label list, either as an intruction label (in }
                    { this case it has not been emitted), or as a   }
                    { duplicate local symbol (in this case it has   }
                    { already been emitted).                        }
                    Begin
                       if labelptr^.emitted then
                        Message1(assem_e_dup_local_sym,'@'+labelptr^.name^)
                       else
                        Begin
                          if assigned(labelptr^.lab) then
                            ConcatLabel(p,A_LABEL,labelptr^.lab);
                          labelptr^.emitted := TRUE;
                        end;
                    end;
                    Consume(AS_LLABEL);
                  end;
        AS_LABEL: Begin
                     if SearchLabel(actasmpattern,hl) then
                       ConcatLabel(p,A_LABEL, hl)
                     else
                       Message1(assem_e_unknown_label_identifer,actasmpattern);
                     Consume(AS_LABEL);
                 end;
        AS_DW:    Begin
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
        AS_OPCODE: Begin
                   instr.init;
                   BuildOpcode;
                   instr.numops := operandnum;
                   if instr.labeled then
                     ConcatLabeledInstr(instr)
                   else
                     ConcatOpCode(instr);
                  end;
        AS_SEPARATOR:Begin
                     Consume(AS_SEPARATOR);
                     { let us go back to the first operand }
                     operandnum := 0;
                    end;
        AS_END: ; { end assembly block }
    else
      Begin
         Message(assem_e_assemble_node_syntax_error);
         { error recovery }
         Consume(actasmtoken);
      end;
    end; { end case }
  end; { end while }
  { check if there were undefined symbols.   }
  { if so, then list each of those undefined }
  { labels.                                  }
  if assigned(labellist.First) then
  Begin
    labelptr := labellist.First;
    if labellist.First <> nil then
    Begin
      { first label }
      if not labelptr^.emitted then
       Message1(assem_e_unknown_local_sym,'@'+labelptr^.name^);
      { other labels ... }
      While (labelptr^.Next <> nil) do
       Begin
          labelptr := labelptr^.Next;
          if not labelptr^.emitted then
           Message1(assem_e_unknown_local_sym,'@'+labelptr^.name^);
      end;
    end;
  end;
  assemble := genasmnode(p);
  labellist.done;
  Message(assem_d_finish_intel);
end;


    procedure ra386int_exit;{$ifndef FPC}far;{$endif}

      begin
         if assigned(iasmops) then
           dispose(iasmops);
         exitproc:=old_exit;
      end;


begin
   old_exit:=exitproc;
   exitproc:=@ra386int_exit;
end.
{
  $Log$
  Revision 1.2  1998-06-24 14:06:38  peter
    * fixed the name changes

  Revision 1.1  1998/06/23 14:00:18  peter
    * renamed RA* units

  Revision 1.11  1998/06/16 08:56:28  peter
    + targetcpu
    * cleaner pmodules for newppu

  Revision 1.10  1998/06/12 10:32:33  pierre
    * column problem hopefully solved
    + C vars declaration changed

  Revision 1.9  1998/05/31 14:13:32  peter
    * fixed call bugs with assembler readers
    + OPR_SYMBOL to hold a symbol in the asm parser
    * fixed staticsymtable vars which were acessed through %ebp instead of
      name

  Revision 1.8  1998/05/30 14:31:07  peter
    + $ASMMODE

  Revision 1.7  1998/05/28 16:32:05  carl
    * bugfix with operands main branch version (patched manually)

  Revision 1.6  1998/05/23 01:21:26  peter
    + aktasmmode, aktoptprocessor, aktoutputformat
    + smartlink per module $SMARTLINK-/+ (like MMX) and moved to aktswitches
    + $LIBNAME to set the library name where the unit will be put in
    * splitted cgi386 a bit (codeseg to large for bp7)
    * nasm, tasm works again. nasm moved to ag386nsm.pas

  Revision 1.5  1998/05/20 09:42:36  pierre
    + UseTokenInfo now default
    * unit in interface uses and implementation uses gives error now
    * only one error for unknown symbol (uses lastsymknown boolean)
      the problem came from the label code !
    + first inlined procedures and function work
      (warning there might be allowed cases were the result is still wrong !!)
    * UseBrower updated gives a global list of all position of all used symbols
      with switch -gb

  Revision 1.4  1998/04/29 10:34:03  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.3  1998/04/08 16:58:06  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!
}
