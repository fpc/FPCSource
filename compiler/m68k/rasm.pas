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

{$i fpcdefs.inc}

Uses
  node;

   function assemble: tnode;



Implementation

    uses
       { common }
       cutils,cclasses,
       { global }
       globtype,globals,verbose,
       systems,
       { aasm }
       cpubase,cpuinfo,aasmbase,aasmtai,aasmcpu,
       { symtable }
       symconst,symbase,symtype,symsym,symtable,
       { pass 1 }
       nbas,
       { parser }
       scanner,agcpugas,
       rautils
       ;

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


type
 tasmtoken = (
   AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_HEXNUM,AS_OCTALNUM,
   AS_BINNUM,AS_COMMA,AS_LBRACKET,AS_RBRACKET,AS_LPAREN,
   AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,AS_INTNUM,
   AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_SLASH,AS_APPT,AS_REALNUM,
   AS_ALIGN,
     {------------------ Assembler directives --------------------}
   AS_DB,AS_DW,AS_DD,AS_END,
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
    ('DC.B','DC.W','DC.L','END');

    { problems with shl,shr,not,and,or and xor, they are }
    { context sensitive.                                 }
    _asmoperators : array[0.._count_asmoperators] of tasmkeyword = (
    'MOD','SHL','SHR','NOT','AND','OR','XOR');


const
  newline = #10;
  firsttoken : boolean = TRUE;
  operandnum : byte = 0;
var
 actasmtoken: tasmtoken;
 actasmpattern: string;
 c: char;
 old_exit : pointer;
 curlist : taasmoutput;

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
      iasmops^[i] := upper(gas_op2str[i]);
     { opcodes }
     for j.enum:=firstasmreg to lastasmreg do
      iasmregs[j.enum] := upper(std_reg2str[j.enum]);
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



   Procedure is_asmdirective(const s: string; var token: tasmtoken);
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
           token := tasmtoken(longint(firstdirective)+i);
           exit;
        end;
     end;
   end;


   Procedure is_register(const s: string; var token: tasmtoken);
  {*********************************************************************}
  { PROCEDURE is_register(s: string; var token: tinteltoken);           }
  {  Description: Determines if the s string is a valid register, if    }
  {  so return token equal to A_REGISTER, otherwise does not change token}
  {*********************************************************************}
   Var
    i: tregister;
   Begin
     for i.enum:=firstasmreg to lastasmreg do
     begin
      if s=iasmregs[i.enum] then
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



  Function GetToken: tasmtoken;
  {*********************************************************************}
  { FUNCTION GetToken: tinteltoken;                                     }
  {  Description: This routine returns intel assembler tokens and       }
  {  does some minor syntax error checking.                             }
  {*********************************************************************}
  var
   token: tasmtoken;
   forcelabel: boolean;
  begin
    forcelabel := FALSE;
    actasmpattern :='';
    {* INIT TOKEN TO NOTHING *}
    token := AS_NONE;
    { while space and tab , continue scan... }
    while c in [' ',#9] do
     c:=current_scanner.asmgetchar;

    if not (c in [newline,#13,'{',';']) then
     current_scanner.gettokenpos;
    { Possiblities for first token in a statement:                }
    {   Local Label, Label, Directive, Prefix or Opcode....       }
    if firsttoken and not (c in [newline,#13,'{',';']) then
    begin

      firsttoken := FALSE;
      if c = '@' then
      begin
        token := AS_LLABEL;   { this is a local label }
        { Let us point to the next character }
        c := current_scanner.asmgetchar;
      end;



      while c in ['A'..'Z','a'..'z','0'..'9','_','@','.'] do
      begin
         { if there is an at_sign, then this must absolutely be a label }
         if c = '@' then forcelabel:=TRUE;
         actasmpattern := actasmpattern + c;
         c := current_scanner.asmgetchar;
      end;

      uppervar(actasmpattern);

      if c = ':' then
      begin
           case token of
             AS_NONE: token := AS_LABEL;
             AS_LLABEL: ; { do nothing }
           end; { end case }
           { let us point to the next character }
           c := current_scanner.asmgetchar;
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
                             c:= current_scanner.asmgetchar;
                             while c in  ['A'..'Z','a'..'z','0'..'9','_','@','.'] do
                             begin
                               actasmpattern := actasmpattern + c;
                               c := current_scanner.asmgetchar;
                             end;
                             uppervar(actasmpattern);
                             gettoken := AS_ID;
                             exit;
                            end;
      { identifier, register, opcode, prefix or directive }
         'A'..'Z','a'..'z','_': begin
                             actasmpattern := c;
                             c:= current_scanner.asmgetchar;
                             while c in  ['A'..'Z','a'..'z','0'..'9','_','.'] do
                             begin
                               actasmpattern := actasmpattern + c;
                               c := current_scanner.asmgetchar;
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
                         c:=current_scanner.asmgetchar;
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
                              c:=current_scanner.asmgetchar;
                              if c=newline then
                              begin
                                 Message(scan_f_string_exceeds_line);
                                 break;
                              end;
                              repeat
                                  if c=''''then
                                   begin
                                       c:=current_scanner.asmgetchar;
                                       if c='''' then
                                        begin
                                               actasmpattern:=actasmpattern+'''';
                                               c:=current_scanner.asmgetchar;
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
                                          c:=current_scanner.asmgetchar;
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
                    c:=current_scanner.asmgetchar;
                    while c in ['0'..'9','A'..'F','a'..'f'] do
                    begin
                      actasmpattern := actasmpattern + c;
                      c := current_scanner.asmgetchar;
                    end;
                   gettoken := AS_HEXNUM;
                   exit;
                  end;
           ',' : begin
                   gettoken := AS_COMMA;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           '(' : begin
                   gettoken := AS_LPAREN;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           ')' : begin
                   gettoken := AS_RPAREN;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           ':' : begin
                   gettoken := AS_COLON;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
{           '.' : begin
                   gettoken := AS_DOT;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end; }
           '+' : begin
                   gettoken := AS_PLUS;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           '-' : begin
                   gettoken := AS_MINUS;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           '*' : begin
                   gettoken := AS_STAR;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           '/' : begin
                   gettoken := AS_SLASH;
                   c:=current_scanner.asmgetchar;
                   exit;
                 end;
           '<' : begin
                   c := current_scanner.asmgetchar;
                   { invalid characters }
                   if c <> '<' then
                    Message(asmr_e_invalid_char_smaller);
                   { still assume << }
                   gettoken := AS_SHL;
                   c := current_scanner.asmgetchar;
                   exit;
                 end;
           '>' : begin
                   c := current_scanner.asmgetchar;
                   { invalid characters }
                   if c <> '>' then
                    Message(asmr_e_invalid_char_greater);
                   { still assume << }
                   gettoken := AS_SHR;
                   c := current_scanner.asmgetchar;
                   exit;
                 end;
           '|' : begin
                   gettoken := AS_OR;
                   c := current_scanner.asmgetchar;
                   exit;
                 end;
           '^' : begin
                  gettoken := AS_XOR;
                  c := current_scanner.asmgetchar;
                  exit;
                 end;
           '#' : begin
                  gettoken:=AS_APPT;
                  c:=current_scanner.asmgetchar;
                  exit;
                 end;
           '%' : begin
                   c:=current_scanner.asmgetchar;
                   while c in ['0','1'] do
                   Begin
                     actasmpattern := actasmpattern + c;
                     c := current_scanner.asmgetchar;
                   end;
                   gettoken := AS_BINNUM;
                   exit;
                 end;
           { integer number }
           '0'..'9': begin
                        actasmpattern := c;
                        c := current_scanner.asmgetchar;
                        while c in ['0'..'9'] do
                          Begin
                             actasmpattern := actasmpattern + c;
                             c:= current_scanner.asmgetchar;
                          end;
                        gettoken := AS_INTNUM;
                        exit;
                     end;
         ';' : begin
                  repeat
                     c:=current_scanner.asmgetchar;
                  until c=newline;
                  firsttoken := TRUE;
                  gettoken:=AS_SEPARATOR;
               end;

         '{',#13,newline : begin
                            c:=current_scanner.asmgetchar;
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

     procedure consume(t : tasmtoken);

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
     findregister.enum := R_NO;
     for i.enum:=firstasmreg to lastasmreg do
       if s = iasmregs[i.enum] then
       Begin
         findregister := i;
         exit;
       end;
    if s = 'A7' then
    Begin
      findregister.enum := R_SP;
      exit;
    end;
   end;


   function findopcode(s: string; var opsize: topsize): tasmop;
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
       'B': opsize := S_B;
       'W': opsize := S_W;
       'L': opsize := S_L;
       'S': opsize := S_FS;
       'D': opsize := S_FD;
       'X': opsize := S_FX;
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
      sym : tsym;
      srsymtable : tsymtable;
      hl : tasmlabel;
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
                        hs:=hl.name
                      end
                    else if SearchLabel(tempstr,hl,false) then
                      hs:=hl.name
                    else
                      begin
                        searchsym(tempstr,sym,srsymtable);
                        if assigned(sym) then
                         begin
                           case sym.typ of
                             varsym :
                               begin
                                 if sym.owner.symtabletype in [localsymtable,parasymtable] then
                                      Message(asmr_e_no_local_or_para_allowed);
                                 hs:=tvarsym(sym).mangledname;
                               end;
                             typedconstsym :
                                   hs:=ttypedconstsym(sym).mangledname;
                             procsym :
                               begin
                                 if tprocsym(sym).procdef_count>1 then
                                      Message(asmr_w_calling_overload_func);
                                 hs:=tprocsym(sym).first_procdef.mangledname;
                               end;
                             typesym :
                               begin
                                 if not(ttypesym(sym).restype.def.deftype in [recorddef,objectdef]) then
                                      Message(asmr_e_wrong_sym_type);
                               end;
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
               ConcatRealConstant(curlist,r,typ);
            End
          else
            Begin
              ConcatRealConstant(curlist,r,typ);
            End;
        end
      else
        Message(asmr_e_invalid_float_expr);
    Until actasmtoken=AS_SEPARATOR;
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
                      ConcatString(curlist,expr);
                    end;
          AS_INTNUM,AS_BINNUM,
          AS_OCTALNUM,AS_HEXNUM:
                    Begin
                      value:=BuildExpression(false,nil);
                      ConcatConstant(curlist,value,maxvalue);
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
                      ConcatConstant(curlist,value,maxvalue);
                  end;
          { These terms can start an assembler expression }
          AS_PLUS,AS_MINUS,AS_LPAREN,AS_NOT: Begin
                                          value := BuildExpression(false,nil);
                                          ConcatConstant(curlist,value,maxvalue);
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






{****************************************************************************
                                Tm68kOperand
****************************************************************************}

type
  TM68kOperand=class(TOperand)
    Procedure BuildOperand;override;
  private
    labeled : boolean;
    Procedure BuildReference;
    Function BuildRefExpression: longint;
    Procedure BuildScaling;
  end;


  Procedure TM68kOperand.BuildScaling;
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
     if (opr.ref.scalefactor <> 0)
     and (opr.ref.scalefactor <> 1) then
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
        opr.ref.scalefactor := l;
     end
     else
     Begin
        Message(asmr_e_wrong_scale_factor);
        opr.ref.scalefactor := 0;
     end;
     if opr.ref.index.enum = R_NO then
     Begin
        Message(asmr_e_wrong_base_index);
        opr.ref.scalefactor := 0;
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


  Function TM68kOperand.BuildRefExpression: longint;
  {*********************************************************************}
  { FUNCTION BuildRefExpression: longint                                   }
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



  Procedure TM68kOperand.BuildReference;
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
                        opr.ref.base := findregister(actasmpattern);
                        Consume(AS_REGISTER);
                        { can either be a register or a right parenthesis }
                         { // (reg)       // }
                         { // (reg)+      // }
                         if actasmtoken=AS_RPAREN then
                         Begin
                           Consume(AS_RPAREN);
                           if actasmtoken = AS_PLUS then
                           Begin
                             if (opr.ref.direction <> dir_none) then
                              Message(asmr_e_no_inc_and_dec_together)
                             else
                               opr.ref.direction := dir_inc;
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
                         opr.ref.index :=
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
                                BuildScaling;
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
                                  opr.ref.offset := l;
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




  Procedure TM68kOperand.BuildOperand;
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to either the     }
  {       AS_COMMA or AS_SEPARATOR token.                               }
  {*********************************************************************}
  var
    tempstr: string;
    expr: string;
    lab: tasmlabel;
    l : longint;
    i: tregister;
    hl: tasmlabel;
    reg_one, reg_two: tregister;
    reglist: Tregisterset;
  Begin
   reglist := [];
   tempstr := '';
   expr := '';
   case actasmtoken of
   { // Memory reference //  }
     AS_LPAREN:
               Begin
                  InitRef;
                  BuildReference;
               end;
   { // Constant expression //  }
     AS_APPT:  Begin
                      Consume(AS_APPT);
                      if not (opr.typ in [OPR_NONE,OPR_CONSTANT]) then
                         Message(asmr_e_invalid_operand_type);
                      { identifiers are handled by BuildExpression }
                      opr.typ := OPR_CONSTANT;
                      opr.val :=BuildExpression(true,@tempstr);
                      if tempstr<>'' then
                        begin
                          l:=opr.val;
                          opr.typ := OPR_SYMBOL;
                          opr.symofs := l;
                          opr.symbol := objectlibrary.newasmsymbol(tempstr);
                        end;
                 end;
   { // Constant memory offset .              // }
   { // This must absolutely be followed by ( // }
     AS_HEXNUM,AS_INTNUM,
     AS_BINNUM,AS_OCTALNUM,AS_PLUS:
                   Begin
                      InitRef;
                      opr.ref.offset:=BuildRefExpression;
                      BuildReference;
                   end;
   { // A constant expression, or a Variable ref. // }
     AS_ID:  Begin
              InitRef;
              if actasmpattern[1] = '@' then
              { // Label or Special symbol reference // }
              Begin
                 if actasmpattern = '@RESULT' then
                    SetUpResult
                 else
                 if actasmpattern = 'SELF' then
                    SetUpSelf
                 else
                 if (actasmpattern = '@CODE') or (actasmpattern = '@DATA') then
                    Message(asmr_w_CODE_and_DATA_not_supported)
                 else
                  Begin
                    delete(actasmpattern,1,1);
                    if actasmpattern = '' then
                     Message(asmr_e_null_label_ref_not_allowed);
                    CreateLocalLabel(actasmpattern,lab,false);
                    opr.typ := OPR_SYMBOL;
                    opr.symbol := lab;
                    opr.symofs := 0;
                    labeled := TRUE;
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
                     InitRef;
                     opr.ref.offset:=BuildRefExpression;
                     BuildReference;
                   end
                 else { is it a label variable ? }
                   Begin
                     { // ID[ , ID.Field.Field or simple ID // }
                     { check if this is a label, if so then }
                     { emit it as a label.                  }
                     if SearchLabel(actasmpattern,hl,false) then
                       Begin
                         opr.typ := OPR_SYMBOL;
                         opr.symbol := hl;
                         opr.symofs := 0;
                         labeled := TRUE;
                         Consume(AS_ID);
                         if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                          Message(asmr_e_syntax_error);
                       end
                      else
                      { is it a normal variable ? }
                      if (cs_compilesystem in aktmoduleswitches) then
                        begin
                          if not SetupDirectVar(expr) then
                            Begin
                              { not found, finally ... add it anyways ... }
                              Message1(asmr_w_id_supposed_external,expr);
                              opr.ref.symbol:=objectlibrary.newasmsymbol(expr);
                            end;
                        end
                       else
                          Message1(sym_e_unknown_id,actasmpattern);
                   end;
                   expr := actasmpattern;
                   Consume(AS_ID);
                   case actasmtoken of
                      AS_LPAREN: { indexing }
                        BuildReference;
                      AS_SEPARATOR,AS_COMMA: ;
                    else
                        Message(asmr_e_syntax_error);
                    end;
               end;
             end;
   { // Pre-decrement mode reference or constant mem offset.   // }
     AS_MINUS:    Begin
                   Consume(AS_MINUS);
                   if actasmtoken = AS_LPAREN then
                   Begin
                     InitRef;
                     { indicate pre-decrement mode }
                     opr.ref.direction := dir_dec;
                     BuildReference;
                   end
                   else
                   if actasmtoken in [AS_OCTALNUM,AS_HEXNUM,AS_BINNUM,AS_INTNUM] then
                   Begin
                      InitRef;
                      opr.ref.offset:=BuildRefExpression;
                      { negate because was preceded by a negative sign! }
                      opr.ref.offset:=-opr.ref.offset;
                      BuildReference;
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
                        if not (opr.typ in [OPR_NONE,OPR_REGISTER]) then
                         Message(asmr_e_invalid_operand_type);
                        opr.typ := OPR_REGISTER;
                        opr.reg := findregister(tempstr);
                   end
                   else
                   { HERE WE MUST HANDLE THE SPECIAL CASE OF MOVEM AND FMOVEM }
                   { // Individual register listing // }
                   if (actasmtoken = AS_SLASH) then
                   Begin
                     reglist := [findregister(tempstr).enum];
                     Consume(AS_SLASH);
                     if actasmtoken = AS_REGISTER then
                     Begin
                       While not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                       Begin
                         case actasmtoken of
                          AS_REGISTER: Begin
                                        reglist := reglist + [findregister(actasmpattern).enum];
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
                       opr.typ:= OPR_REGLIST;
                       opr.reglist := reglist;
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
                      if reg_one.enum > reg_two.enum then
                      begin
                       for i.enum:=reg_two.enum to reg_one.enum do
                         reglist := reglist + [i.enum];
                      end
                      else
                      Begin
                       for i.enum:=reg_one.enum to reg_two.enum do
                         reglist := reglist + [i.enum];
                      end;
                      Consume(AS_REGISTER);
                      if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                      Begin
                       Message(asmr_e_invalid_reg_list_in_movem);
                       while not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) do
                         Consume(actasmtoken);
                      end;
                      { set up instruction }
                      opr.typ:= OPR_REGLIST;
                      opr.reglist := reglist;
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
                         opr.typ := OPR_REGISTER;
                         opr.reg := findregister(tempstr);
                         Inc(operandnum);
                         opr.typ := OPR_REGISTER;
                         opr.reg := findregister(actasmpattern);
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
                        Message1(asmr_e_higher_cpu_mode_required,'68020');
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
                      ConcatPasString(curlist,expr);
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


{*****************************************************************************
                                TM68kInstruction
*****************************************************************************}

type
  TM68kInstruction=class(TInstruction)
    procedure InitOperands;override;
    procedure BuildOpcode;override;
    procedure ConcatInstruction(p : taasmoutput);override;
    Procedure ConcatLabeledInstr(p : taasmoutput);
  end;

    procedure TM68kInstruction.InitOperands;
    var
      i : longint;
    begin
      for i:=1 to max_operands do
       Operands[i]:=TM68kOperand.Create;
    end;


  Procedure TM68kInstruction.BuildOpCode;
  {*********************************************************************}
  { PROCEDURE BuildOpcode;                                              }
  {  Description: Parses the intel opcode and operands, and writes it   }
  {  in the TInstruction object.                                        }
  {*********************************************************************}
  { EXIT CONDITION:  On exit the routine should point to AS_SEPARATOR.  }
  { On ENTRY: Token should point to AS_OPCODE                           }
  {*********************************************************************}
  var asmtok: tasmop;
      expr: string;
      operandnum : longint;
  Begin
    expr := '';
    asmtok := A_NONE; { assmume no prefix          }

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
      opcode := findopcode(actasmpattern,opsize);
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
                  if operandnum > Max_Operands then
                    Message(asmr_e_too_many_operands)
                  else
                    Inc(operandnum);
                  Consume(AS_COMMA);
                end;
         { // End of asm operands for this opcode // }
         AS_SEPARATOR: ;
       else
         Operands[operandnum].BuildOperand;
     end; { end case }
    end; { end while }
  end;



 procedure TM68kInstruction.ConcatInstruction(p : taasmoutput);
  var
    fits : boolean;
  Begin
     fits := FALSE;
    { setup specific opcodetions for first pass }

    { Setup special operands }
    { Convert to general form as to conform to the m68k opcode table }
    if (opcode = A_ADDA) or (opcode = A_ADDI)
       then opcode := A_ADD
    else
    { CMPM excluded because of GAS v1.34 BUG }
    if (opcode = A_CMPA) or
       (opcode = A_CMPI) then
       opcode := A_CMP
    else
    if opcode = A_EORI then
      opcode := A_EOR
    else
    if opcode = A_MOVEA then
     opcode := A_MOVE
    else
    if opcode = A_ORI then
      opcode := A_OR
    else
    if (opcode = A_SUBA) or (opcode = A_SUBI) then
      opcode :=  A_SUB;

    { Setup operand types }

(*
    in opcode <> A_MOVEM then
    Begin

      while not(fits) do
        begin
         { set the opcodetion cache, if the opcodetion }
         { occurs the first time                         }
         if (it[i].i=opcode) and (ins_cache[opcode]=-1) then
             ins_cache[opcode]:=i;

         if (it[i].i=opcode) and (instr.ops=it[i].ops) then
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
    case ops of
     0:
        if opsize <> S_NO then
          p.concat((taicpu.op_none(opcode,opsize)))
        else
          p.concat((taicpu.op_none(opcode,S_NO)));
     1: Begin
          case operands[1].opr.typ of
           OPR_SYMBOL:
              Begin
                p.concat((taicpu.op_sym_ofs(opcode,
                  opsize, operands[1].opr.symbol,operands[1].opr.symofs)));
              end;
           OPR_CONSTANT:
              Begin
                p.concat((taicpu.op_const(opcode,
                  opsize, operands[1].opr.val)));
              end;
           OPR_REGISTER:
              p.concat((taicpu.op_reg(opcode,opsize,operands[1].opr.reg)));
           OPR_REFERENCE:
              if opsize <> S_NO then
                Begin
                  p.concat((taicpu.op_ref(opcode,
                    opsize,operands[1].opr.ref)));
                end
               else
                Begin
                  { special jmp and call case with }
                  { symbolic references.           }
                  if opcode in [A_BSR,A_JMP,A_JSR,A_BRA,A_PEA] then
                    Begin
                      p.concat((taicpu.op_ref(opcode,
                        S_NO,operands[1].opr.ref)));
                    end
                  else
                    Message(asmr_e_invalid_opcode_and_operand);
                end;
           OPR_NONE:
                Message(asmr_e_invalid_opcode_and_operand);
          else
           Begin
             Message(asmr_e_invalid_opcode_and_operand);
           end;
          end;
        end;
     2: Begin
                { source }
                  case operands[1].opr.typ of
                  { reg,reg     }
                  { reg,ref     }
                   OPR_REGISTER:
                     Begin
                       case operands[2].opr.typ of
                         OPR_REGISTER:
                            Begin
                               p.concat((taicpu.op_reg_reg(opcode,
                               opsize,operands[1].opr.reg,operands[2].opr.reg)));
                            end;
                         OPR_REFERENCE:
                                  p.concat((taicpu.op_reg_ref(opcode,
                                  opsize,operands[1].opr.reg,operands[2].opr.ref)));
                       else { else case }
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                       end; { end second operand case for OPR_REGISTER }
                     end;
                  { reglist, ref }
                   OPR_REGLIST:
                          Begin
                            case operands[2].opr.typ of
                              OPR_REFERENCE :
                                  p.concat((taicpu.op_reglist_ref(opcode,
                                  opsize,operands[1].opr.reglist,operands[2].opr.ref)));
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
                      case operands[2].opr.typ of
                      { constant, constant does not have a specific size. }
                        OPR_CONSTANT:
                           p.concat((taicpu.op_const_const(opcode,
                           S_NO,operands[1].opr.val,operands[2].opr.val)));
                        OPR_REFERENCE:
                           Begin
                                 p.concat((taicpu.op_const_ref(opcode,
                                 opsize,operands[1].opr.val,
                                 operands[2].opr.ref)))
                           end;
                        OPR_REGISTER:
                           Begin
                                 p.concat((taicpu.op_const_reg(opcode,
                                 opsize,operands[1].opr.val,
                                 operands[2].opr.reg)))
                           end;
                      else
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_CONSTANT }
                   { ref,reg     }
                   { ref,ref     }
                   OPR_REFERENCE:
                      case operands[2].opr.typ of
                         OPR_REGISTER:
                            Begin
                              p.concat((taicpu.op_ref_reg(opcode,
                               opsize,operands[1].opr.ref,
                               operands[2].opr.reg)));
                            end;
                         OPR_REGLIST:
                            Begin
                              p.concat((taicpu.op_ref_reglist(opcode,
                               opsize,operands[1].opr.ref,
                               operands[2].opr.reglist)));
                            end;
                         OPR_REFERENCE: { special opcodes }
                            p.concat((taicpu.op_ref_ref(opcode,
                            opsize,operands[1].opr.ref,
                            operands[2].opr.ref)));
                      else
                         Begin
                           Message(asmr_e_invalid_opcode_and_operand);
                         end;
                      end; { end second operand case for OPR_REFERENCE }
           OPR_SYMBOL: case operands[2].opr.typ of
                        OPR_REFERENCE:
                           Begin
                                 p.concat((taicpu.op_sym_ofs_ref(opcode,
                                   opsize,operands[1].opr.symbol,operands[1].opr.symofs,
                                   operands[2].opr.ref)))
                           end;
                        OPR_REGISTER:
                           Begin
                                 p.concat((taicpu.op_sym_ofs_reg(opcode,
                                   opsize,operands[1].opr.symbol,operands[1].opr.symofs,
                                   operands[2].opr.reg)))
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
        end;
     3: Begin
           if (opcode = A_DIVSL) or (opcode = A_DIVUL) or (opcode = A_MULU)
           or (opcode = A_MULS) or (opcode = A_DIVS) or (opcode = A_DIVU) then
           Begin
             if (operands[1].opr.typ <> OPR_REGISTER)
             or (operands[2].opr.typ <> OPR_REGISTER)
             or (operands[3].opr.typ <> OPR_REGISTER) then
             Begin
               Message(asmr_e_invalid_opcode_and_operand);
             end
             else
             Begin
               p.concat((taicpu. op_reg_reg_reg(opcode,opsize,
                 operands[1].opr.reg,operands[2].opr.reg,operands[3].opr.reg)));
             end;
           end
           else
            Message(asmr_e_invalid_opcode_and_operand);
        end;
  end; { end case }
 end;
 end;


    Procedure TM68kInstruction.ConcatLabeledInstr(p : taasmoutput);
    Begin
       if ((opcode >= A_BCC) and (opcode <= A_BVS))
       or (opcode = A_BRA) or (opcode = A_BSR)
       or (opcode = A_JMP) or (opcode = A_JSR)
       or ((opcode >= A_FBEQ) and (opcode <= A_FBNGLE))
       then
       Begin
        if ops > 2 then
          Message(asmr_e_invalid_opcode_and_operand)
        else if operands[1].opr.typ <> OPR_SYMBOL then
          Message(asmr_e_invalid_opcode_and_operand)
        else if (operands[1].opr.typ = OPR_SYMBOL) and
         (ops = 1) then
           if assigned(operands[1].opr.symbol) and
              (operands[1].opr.symofs=0) then
             p.concat(taicpu.op_sym(opcode,S_NO,
               operands[1].opr.symbol))
           else
            Message(asmr_e_invalid_opcode_and_operand);
       end
       else
       if ((opcode >= A_DBCC) and (opcode <= A_DBF))
       or ((opcode >= A_FDBEQ) and (opcode <= A_FDBNGLE)) then
       begin
         if (ops<>2) or
            (operands[1].opr.typ <> OPR_REGISTER) or
            (operands[2].opr.typ <> OPR_SYMBOL) or
            (operands[2].opr.symofs <> 0) then
           Message(asmr_e_invalid_opcode_and_operand)
         else
          p.concat(taicpu.op_reg_sym(opcode,opsize,operands[1].opr.reg,
           operands[2].opr.symbol));
       end
       else
        Message(asmr_e_invalid_opcode_and_operand);
    end;


  Function Assemble: tnode;
  {*********************************************************************}
  { PROCEDURE Assemble;                                                 }
  {  Description: Parses the att assembler syntax, parsing is done      }
  {  according to GAs rules.                                            }
  {*********************************************************************}
  Var
   hl: tasmlabel;
   labelptr,nextlabel : tasmlabel;
   commname : string;
   instr      : TM68kInstruction;
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
    curlist:=TAAsmoutput.Create;
    { setup label linked list }
    LocalLabelList:=TLocalLabelList.Create;
    c:=current_scanner.asmgetchar;
    actasmtoken:=gettoken;
    while actasmtoken<>AS_END do
    Begin
      case actasmtoken of
        AS_LLABEL:
          Begin
            if CreateLocalLabel(actasmpattern,hl,true) then
              ConcatLabel(curlist,hl);
            Consume(AS_LLABEL);
          end;
        AS_LABEL: Begin
                     { when looking for Pascal labels, these must }
                     { be in uppercase.                           }
                     if SearchLabel(upper(actasmpattern),hl,true) then
                       ConcatLabel(curlist,hl)
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
        AS_ALIGN: Begin
                    Message(asmr_w_align_not_supported);
                    while actasmtoken <> AS_SEPARATOR do
                     Consume(actasmtoken);
                  end;
        AS_OPCODE: Begin
                    instr:=TM68kInstruction.Create;
                    instr.BuildOpcode;
{                    instr.AddReferenceSizes;}
{                    instr.SetInstructionOpsize;}
{                    instr.CheckOperandSizes;}
                    if instr.labeled then
                       instr.ConcatLabeledInstr(curlist)
                    else
                      instr.ConcatInstruction(curlist);
                    instr.Free;
{
                   instr.init;
                   BuildOpcode;
                   instr.ops := operandnum;
                   if instr.labeled then
                     ConcatLabeledInstr(instr)
                   else
                     ConcatOpCode(instr);
                   instr.done;}
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
  LocalLabelList.CheckEmitted;
  LocalLabelList.Free;

  { Return the list in an asmnode }
  assemble:=casmnode.create(curlist);
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
  Revision 1.11  2003-01-08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.10  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.9  2002/11/30 23:33:03  carl
    * merges from Pierre's fixes in m68k fixes branch

  Revision 1.8  2002/09/07 15:25:13  peter
    * old logs removed and tabs fixed

  Revision 1.7  2002/09/03 19:04:18  daniel
    * Fixed PowerPC & M68000 compilation

  Revision 1.6  2002/08/13 18:58:54  carl
    + m68k problems with cvs fixed?()!

  Revision 1.4  2002/08/12 15:08:44  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.3  2002/08/11 14:32:32  peter
    * renamed current_library to objectlibrary

  Revision 1.2  2002/08/11 13:24:18  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.1  2002/08/06 15:15:42  carl
    + more m68k fixes

}

