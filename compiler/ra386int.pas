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
{$ifdef TP}
  {$E+,N+}
{$endif}
Unit Ra386int;
Interface

uses
  tree;

   function assemble: ptree;



Implementation

Uses
  globtype,
  strings,cobjects,systems,verbose,globals,
  files,aasm,types,scanner,hcodegen,symtable
  ,i386base
  ,rautils,ra386;


type
 tasmtoken = (
   AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_INTNUM,
   AS_COMMA,AS_LBRACKET,AS_RBRACKET,AS_LPAREN,
   AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,
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

const
  newline = #10;
  firsttoken : boolean = TRUE;
  operandnum : byte = 0;
var
  _asmsorted     : boolean;
  inexpression   : boolean;
  curlist        : paasmoutput;
  c              : char;
  prevasmtoken   : tasmtoken;
  actasmtoken    : tasmtoken;
  actasmpattern  : string;
  actasmregister : tregister;
  actopcode      : tasmop;
  actopsize      : topsize;
  actcondition   : tasmcond;
  Instr          : TInstruction;
  labellist      : TAsmLabelList;
  iasmops        : ^op2strtable;
  iasmregs       : ^reg2strtable;


Procedure SetupTables;
{ creates uppercased symbol tables for speed access }
var
  i : tasmop;
  j : tregister;
Begin
  Message(assem_d_creating_lookup_tables);
  { opcodes }
  new(iasmops);
  for i:=firstop to lastop do
   iasmops^[i] := upper(int_op2str[i]);
  { registers }
  new(iasmregs);
  for j:=firstreg to lastreg do
   iasmregs^[j] := upper(int_reg2str[j]);
end;


  {---------------------------------------------------------------------}
  {                     Routines for the tokenizing                     }
  {---------------------------------------------------------------------}


   function is_asmopcode(const s: string):boolean;
  {*********************************************************************}
  { FUNCTION is_asmopcode(s: string):Boolean                            }
  { Description: Determines if the s string is a valid opcode          }
  { It sets also actopcode and actcondition }
  { if so returns TRUE otherwise returns FALSE.                        }
  {*********************************************************************}
   var
     i: tasmop;
     cond : string[4];
     cnd : tasmcond;
     j: longint;
   Begin
     is_asmopcode:=FALSE;

     actopcode:=A_None;
     actcondition:=C_None;
     actopsize:=S_NO;

     for i:=firstop to lastop do
      if s=iasmops^[i] then
       begin
         actopcode:=i;
         actasmtoken:=AS_OPCODE;
         is_asmopcode:=TRUE;
         exit;
       end;
     { not found yet, check condition opcodes }
     j:=0;
     while (j<CondAsmOps) do
      begin
        if Copy(s,1,Length(CondAsmOpStr[j]))=CondAsmOpStr[j] then
         begin
           cond:=Copy(s,Length(CondAsmOpStr[j])+1,255);
           if cond<>'' then
            begin
              for cnd:=low(TasmCond) to high(TasmCond) do
               if Cond=Upper(cond2str[cnd]) then
                begin
                  actopcode:=CondASmOp[j];
                  actcondition:=cnd;
                  is_asmopcode:=TRUE;
                  actasmtoken:=AS_OPCODE;
                  exit
                end;
            end;
         end;
        inc(j);
      end;
   end;


function is_asmoperator(const s: string):boolean;
var
  i : longint;
Begin
  for i:=0 to _count_asmoperators do
   if s=_asmoperators[i] then
    begin
      actasmtoken:=tasmtoken(longint(firstoperator)+i);
      is_asmoperator:=true;
      exit;
    end;
  is_asmoperator:=false;
end;


Function is_asmdirective(const s: string):boolean;
var
  i : longint;
Begin
  for i:=0 to _count_asmdirectives do
   if s=_asmdirectives[i] then
    begin
      actasmtoken:=tasmtoken(longint(firstdirective)+i);
      is_asmdirective:=true;
      exit;
    end;
  is_asmdirective:=false;
end;


Function is_register(const s: string):boolean;
Var
  i : tregister;
Begin
  actasmregister:=R_NO;
  for i:=firstreg to lastreg do
   if s=iasmregs^[i] then
    begin
      actasmtoken:=AS_REGISTER;
      actasmregister:=i;
      is_register:=true;
      exit;
    end;
  is_register:=false;
end;


Procedure GetToken;
{*********************************************************************}
{ FUNCTION GetToken: tasmtoken;                                     }
{  Description: This routine returns intel assembler tokens and       }
{  does some minor syntax error checking.                             }
{*********************************************************************}
var
  len : longint;
  forcelabel : boolean;
begin
  { save old token and reset new token }
  prevasmtoken:=actasmtoken;
  actasmtoken:=AS_NONE;
  { reset }
  forcelabel:=FALSE;
  actasmpattern:='';
  { while space and tab , continue scan... }
  while (c in [' ',#9]) do
    c:=current_scanner^.asmgetchar;
  { get token pos }
  if not (c in [newline,#13,'{',';']) then
    current_scanner^.gettokenpos;
{ Local Label, Label, Directive, Prefix or Opcode }
  if firsttoken and not (c in [newline,#13,'{',';']) then
   begin
     firsttoken:=FALSE;
     if c = '@' then
      begin
        actasmtoken:=AS_LLABEL;   { this is a local label }
        { Let us point to the next character }
        c:=current_scanner^.asmgetchar;
      end;
     len:=0;
     while c in ['A'..'Z','a'..'z','0'..'9','_','@'] do
      begin
        { if there is an at_sign, then this must absolutely be a label }
        if c = '@' then
         forcelabel:=TRUE;
        inc(len);
        actasmpattern[len]:=c;
        c:=current_scanner^.asmgetchar;
      end;
     actasmpattern[0]:=chr(len);
     uppervar(actasmpattern);
     { label ? }
     if c = ':' then
      begin
        case actasmtoken of
          AS_NONE:
            actasmtoken:=AS_LABEL;
          AS_LLABEL: ; { do nothing }
        end; { end case }
        { let us point to the next character }
        c:=current_scanner^.asmgetchar;
        exit;
      end;
     { Are we trying to create an identifier with }
     { an at-sign...?                             }
     if forcelabel then
      Message(assem_e_none_label_contain_at);
     { opcode ? }
     If is_asmopcode(actasmpattern) then
      Begin
        { check if we are in an expression  }
        { then continue with asm directives }
        if not inexpression then
         exit;
      end;
     if is_asmdirective(actasmpattern) then
      exit;
     Message1(assem_e_invalid_operand,actasmpattern);
     actasmtoken:=AS_NONE;
     exit;
   end
  else { else firsttoken }
   begin
     case c of
       '@' : { possiblities : - local label reference , such as in jmp @local1 }
             {                - @Result, @Code or @Data special variables.     }
         begin
           actasmpattern:=c;
           c:=current_scanner^.asmgetchar;
           while c in  ['A'..'Z','a'..'z','0'..'9','_','@'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           uppervar(actasmpattern);
           actasmtoken:=AS_ID;
           exit;
         end;

       'A'..'Z','a'..'z','_': { identifier, register, opcode, prefix or directive }
         begin
           actasmpattern:=c;
           c:=current_scanner^.asmgetchar;
           while c in  ['A'..'Z','a'..'z','0'..'9','_'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           uppervar(actasmpattern);
           { after prefix we allow also a new opcode }
           If (operandnum=0) and is_asmopcode(actasmpattern) then
            Begin
              { if we are not in a constant }
              { expression than this is an  }
              { opcode.                     }
              if not inexpression then
               exit;
            end;
           if is_register(actasmpattern) then
            exit;
           if is_asmdirective(actasmpattern) then
            exit;
           if is_asmoperator(actasmpattern) then
            exit;
           actasmtoken:=AS_ID;
           exit;
         end;

       '&' : { override operator... not supported }
         begin
           Message(assem_w_override_op_not_supported);
           c:=current_scanner^.asmgetchar;
           actasmtoken:=AS_NONE;
         end;

       '''' : { string or character }
         begin
           actasmpattern:='';
           repeat
             if c = '''' then
              begin
                c:=current_scanner^.asmgetchar;
                if c=newline then
                 begin
                   Message(scan_f_string_exceeds_line);
                   break;
                 end;
                repeat
                  if c='''' then
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
                     else
                      break;
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
             else
              break; { end if }
           until false;
           actasmtoken:=AS_STRING;
           exit;
         end;

       '"' : { string or character }
         begin
           actasmpattern:='';
           repeat
             if c = '"' then
              begin
                c:=current_scanner^.asmgetchar;
                if c=newline then
                 begin
                   Message(scan_f_string_exceeds_line);
                   break;
                 end;
                repeat
                  if c='"' then
                   begin
                     c:=current_scanner^.asmgetchar;
                     if c='"' then
                      begin
                        actasmpattern:=actasmpattern+'"';
                        c:=current_scanner^.asmgetchar;
                        if c=newline then
                         begin
                           Message(scan_f_string_exceeds_line);
                           break;
                         end;
                      end
                     else
                      break;
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
             else
              break; { end if }
           until false;
           actasmtoken:=AS_STRING;
           exit;
         end;

       '$' :
         begin
           c:=current_scanner^.asmgetchar;
           while c in ['0'..'9','A'..'F','a'..'f'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
           actasmtoken:=AS_INTNUM;
           exit;
         end;

       ',' :
         begin
           actasmtoken:=AS_COMMA;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '[' :
         begin
           actasmtoken:=AS_LBRACKET;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       ']' :
         begin
           actasmtoken:=AS_RBRACKET;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '(' :
         begin
           actasmtoken:=AS_LPAREN;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       ')' :
         begin
           actasmtoken:=AS_RPAREN;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       ':' :
         begin
           actasmtoken:=AS_COLON;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '.' :
         begin
           actasmtoken:=AS_DOT;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '+' :
         begin
           actasmtoken:=AS_PLUS;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '-' :
         begin
           actasmtoken:=AS_MINUS;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '*' :
         begin
           actasmtoken:=AS_STAR;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '/' :
         begin
           actasmtoken:=AS_SLASH;
           c:=current_scanner^.asmgetchar;
           exit;
         end;

       '0'..'9':
         begin
           actasmpattern:=c;
           c:=current_scanner^.asmgetchar;
           { Get the possible characters }
           while c in ['0'..'9','A'..'F','a'..'f'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner^.asmgetchar;
            end;
           { Get ending character }
           uppervar(actasmpattern);
           c:=upcase(c);
           { possibly a binary number. }
           if (actasmpattern[length(actasmpattern)] = 'B') and (c <> 'H') then
            Begin
              { Delete the last binary specifier }
              delete(actasmpattern,length(actasmpattern),1);
              actasmpattern:=tostr(ValBinary(actasmpattern));
              actasmtoken:=AS_INTNUM;
              exit;
            end
           else
            Begin
              case c of
                'O' :
                  Begin
                    actasmpattern:=tostr(ValOctal(actasmpattern));
                    actasmtoken:=AS_INTNUM;
                    c:=current_scanner^.asmgetchar;
                    exit;
                  end;
                'H' :
                  Begin
                    actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
                    actasmtoken:=AS_INTNUM;
                    c:=current_scanner^.asmgetchar;
                    exit;
                  end;
                else { must be an integer number }
                  begin
                    actasmpattern:=tostr(ValDecimal(actasmpattern));
                    actasmtoken:=AS_INTNUM;
                    exit;
                  end;
              end;
            end;
         end;

       ';','{',#13,newline :
         begin
           c:=current_scanner^.asmgetchar;
           firsttoken:=TRUE;
           actasmtoken:=AS_SEPARATOR;
           exit;
         end;

        else
           Begin
             Message(scan_f_illegal_char);
           end;
     end;
   end;
end;


procedure consume(t : tasmtoken);
begin
  if t<>actasmtoken then
    Message(assem_e_syntax_error);
  gettoken;
  { if the token must be ignored, then }
  { get another token to parse.        }
  if actasmtoken = AS_NONE then
    gettoken;
end;


procedure RecoverConsume(allowcomma:boolean);
begin
  While not (actasmtoken in [AS_SEPARATOR,AS_END]) do
   begin
     if allowcomma and (actasmtoken=AS_COMMA) then
      break;
     Consume(actasmtoken);
   end;
end;


{*****************************************************************************
                               Parsing Helpers
*****************************************************************************}

Procedure BuildRecordOffsetSize(const expr: string;var offset:longint;var size:longint);
{ Description: This routine builds up a record offset after a AS_DOT }
{ token is encountered.                                              }
{ On entry actasmtoken should be equal to AS_DOT                     }
var
  s : string;
Begin
  offset:=0;
  size:=0;
  s:=expr;
  while (actasmtoken=AS_DOT) do
   begin
     Consume(AS_DOT);
     if actasmtoken=AS_ID then
      begin
        s:=s+'.'+actasmpattern;
        Consume(AS_ID);
      end
     else
      begin
        Message(assem_e_syntax_error);
        RecoverConsume(true);
        break;
      end;
   end;
  if not GetRecordOffsetSize(s,offset,size) then
   Message(assem_e_syntax_error);
end;


Procedure BuildConstSymbolExpression(needofs:boolean;var value:longint;var asmsym:string);
{*********************************************************************}
{ FUNCTION BuildConstExpression(false): longint                              }
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
var
  tempstr,expr,hs : string;
  parenlevel,l,k : longint;
  errorflag : boolean;
  prevtok : tasmtoken;
  hl : plabel;
  sym : psym;
Begin
  { reset }
  value:=0;
  asmsym:='';
  errorflag:=FALSE;
  tempstr:='';
  expr:='';
  inexpression:=TRUE;
  parenlevel:=0;
  Repeat
    Case actasmtoken of
      AS_LPAREN:
        Begin
          Consume(AS_LPAREN);
          expr:=expr + '(';
          inc(parenlevel);
        end;
      AS_RPAREN:
        Begin
          Consume(AS_RPAREN);
          expr:=expr + ')';
          dec(parenlevel);
        end;
      AS_SHL:
        Begin
          Consume(AS_SHL);
          expr:=expr + '<';
        end;
      AS_SHR:
        Begin
          Consume(AS_SHR);
          expr:=expr + '>';
        end;
      AS_SLASH:
        Begin
          Consume(AS_SLASH);
          expr:=expr + '/';
        end;
      AS_MOD:
        Begin
          Consume(AS_MOD);
          expr:=expr + '%';
        end;
      AS_STAR:
        Begin
          Consume(AS_STAR);
          expr:=expr + '*';
        end;
      AS_PLUS:
        Begin
          Consume(AS_PLUS);
          expr:=expr + '+';
        end;
      AS_MINUS:
        Begin
          Consume(AS_MINUS);
          expr:=expr + '-';
        end;
      AS_AND:
        Begin
          Consume(AS_AND);
          expr:=expr + '&';
        end;
      AS_NOT:
        Begin
          Consume(AS_NOT);
          expr:=expr + '~';
        end;
      AS_XOR:
        Begin
          Consume(AS_XOR);
          expr:=expr + '^';
        end;
      AS_OR:
        Begin
          Consume(AS_OR);
          expr:=expr + '|';
        end;
      AS_INTNUM:
        Begin
          expr:=expr + actasmpattern;
          Consume(AS_INTNUM);
        end;
      AS_OFFSET:
        begin
          Consume(AS_OFFSET);
          if actasmtoken<>AS_ID then
           Comment(V_Error,'assem_e_offset_without_identifier');
        end;
      AS_ID:
        Begin
          tempstr:=actasmpattern;
          prevtok:=prevasmtoken;
          consume(AS_ID);
          if actasmtoken=AS_DOT then
           begin
             BuildRecordOffsetSize(tempstr,l,k);
             str(l, tempstr);
             expr:=expr + tempstr;
           end
          else
           if SearchIConstant(tempstr,l) then
            begin
              str(l, tempstr);
              expr:=expr + tempstr;
            end
          else
           begin
             hs:='';
             if SearchLabel(tempstr,hl) then
              hs:=lab2str(hl)
             else
              begin
                getsym(tempstr,false);
                sym:=srsym;
                if assigned(sym) then
                 begin
                   if sym^.owner^.symtabletype in [localsymtable,parasymtable] then
                     Writeln('can''t use local variable or parameters here');
                   case srsym^.typ of
                     varsym :
                       hs:=pvarsym(srsym)^.mangledname;
                     typedconstsym :
                       hs:=ptypedconstsym(srsym)^.mangledname;
                     procsym :
                       hs:=pprocsym(srsym)^.mangledname;
                     else
                       Writeln('Error: wrong sym type');
                   end;
                 end
                else
                 Message1(assem_e_unknown_id,tempstr);
              end;
             { symbol found? }
             if hs<>'' then
              begin
                if needofs and (prevtok<>AS_OFFSET) then
                 Comment(V_Error,'assem_e_need_offset');
                if asmsym='' then
                 asmsym:=hs
                else
                 Comment(V_Error,'assem_e_cant_have_multiple_relocatable_symbols');
                if (expr='') or (expr[length(expr)]='+') then
                 begin
                   delete(expr,length(expr),1);
                   if not(actasmtoken in [AS_MINUS,AS_PLUS,AS_COMMA,AS_SEPARATOR,AS_END]) then
                    Comment(V_Error,'assem_e_only_add_relocatable_symbol');
                 end
                else
                 Comment(V_Error,'assem_e_only_add_relocatable_symbol');
              end;
           end;
        end;
      AS_RBRACKET,
      AS_SEPARATOR,
      AS_COMMA:
        Begin
          break;
        end;
    else
      Begin
        { write error only once. }
        if not errorflag then
          Message(assem_e_invalid_constant_expression);
        { consume tokens until we find COMMA or SEPARATOR }
        Consume(actasmtoken);
        errorflag:=TRUE;
      end;
    end;
  Until false;
  { calculate expression }
  if not ErrorFlag then
    value:=CalculateExpression(expr)
  else
    value:=0;
  { no longer in an expression }
  inexpression:=FALSE;
end;


Function BuildConstExpression:longint;
var
  l : longint;
  hs : string;
begin
  BuildConstSymbolExpression(false,l,hs);
  if hs<>'' then
   Comment(V_Error,'no relocatable symbols allowed');
  BuildConstExpression:=l;
end;



Procedure BuildBracketExpression(var Instr: TInstruction);
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
  l : longint;
  hs : string;
  code : word;
  hreg,
  oldbase : tregister;
  GotPlus,Negative : boolean;
Begin
  Consume(AS_LBRACKET);
  initAsmRef(instr,operandnum);
  GotPlus:=true;
  Negative:=false;
  repeat
    Case actasmtoken of

      AS_ID: { Constant reference expression OR variable reference expression }
        Begin
          if not GotPlus then
            Message(assem_e_invalid_reference_syntax);
          if actasmpattern[1] = '@' then
           Message(assem_e_local_symbol_not_allowed_as_ref);
          if SearchIConstant(actasmpattern,l) then
           begin
             l:=BuildConstExpression;
             if actasmtoken=AS_STAR then
              instr.operands[operandnum].ref.scalefactor:=l
             else
              begin
                if negative then
                  Dec(instr.operands[operandnum].ref.offset,l)
                else
                  Inc(instr.operands[operandnum].ref.offset,l);
              end;
           end
          else
           Begin
             if instr.operands[operandnum].hasvar then
               writeln('can''t have more than one variable in an reference');
             if negative then
               writeln('can''t substract relocatable variable');
             oldbase:=instr.operands[operandnum].ref.base;
             instr.operands[operandnum].ref.base:=R_NO;
             if not CreateVarInstr(instr,actasmpattern,operandnum) then
               Message1(assem_e_unknown_id,actasmpattern);
             { is the base register loaded by the var ? }
             if (instr.operands[operandnum].ref.base<>R_NO) then
              begin
                { check if we can move the old base to the index register }
                if (instr.operands[operandnum].ref.index<>R_NO) then
                 writeln('invalid base and index register usage')
                else
                 instr.operands[operandnum].ref.index:=oldbase;
              end
             else
              instr.operands[operandnum].ref.base:=oldbase;
             { we can't have a Constant here so add the constant value to the
               offset }
             if instr.operands[operandnum].operandtype=OPR_CONSTANT then
              begin
                instr.operands[operandnum].operandtype:=OPR_REFERENCE;
                inc(instr.operands[operandnum].ref.offset,instr.operands[operandnum].val);
              end;
             Consume(AS_ID);
           end;
          GotPlus:=false;
        end;

      AS_PLUS :
        Begin
          Consume(AS_PLUS);
          Negative:=false;
          GotPlus:=true;
        end;

      AS_MINUS :
        begin
          Consume(AS_MINUS);
          Negative:=true;
          GotPlus:=true;
        end;

      AS_STAR : { Scaling }
        begin
          Consume(AS_STAR);
          hs:='';
          l:=0;
          case actasmtoken of
            AS_LPAREN :
              l:=BuildConstExpression;
            AS_INTNUM:
              Begin
                hs:=actasmpattern;
                Consume(AS_INTNUM);
              end;
            AS_REGISTER :
              begin
                if instr.operands[operandnum].ref.scalefactor=0 then
                 Comment(V_Error,'wrong scale factor specified');
              end;
            else
              Message(assem_e_invalid_reference_syntax);
          end;
          if actasmtoken<>AS_REGISTER then
           begin
             if hs<>'' then
              val(hs,l,code);
             instr.operands[operandnum].ref.scalefactor:=l
           end;
          GotPlus:=false;
        end;

      AS_REGISTER :
        begin
          if (not GotPlus) and (actasmtoken<>AS_STAR) then
            Message(assem_e_invalid_reference_syntax);
          hreg:=actasmregister;
          Consume(AS_REGISTER);
          { this register will be the index }
          if (actasmtoken=AS_STAR) or
             (instr.operands[operandnum].ref.base<>R_NO) then
           begin
             if (instr.operands[operandnum].ref.index<>R_NO) then
               writeln('Error: multiple index register usage');
             instr.operands[operandnum].ref.index:=hreg;
           end
          else
           instr.operands[operandnum].ref.base:=hreg;
          GotPlus:=false;
        end;

      AS_NOT,
      AS_INTNUM,
      AS_LPAREN : { Constant reference expression }
        begin
          if not GotPlus then
            Message(assem_e_invalid_reference_syntax);
          l:=BuildConstExpression;
          if actasmtoken=AS_STAR then
           instr.operands[operandnum].ref.scalefactor:=l
          else
           begin
             if negative then
               Dec(instr.operands[operandnum].ref.offset,l)
             else
               Inc(instr.operands[operandnum].ref.offset,l);
           end;
          GotPlus:=false;
        end;

      AS_RBRACKET :
        begin
          if GotPlus then
            Message(assem_e_invalid_reference_syntax);
          Consume(AS_RBRACKET);
          break;
        end;

      else
        Begin
          Message(assem_e_invalid_reference_syntax);
          RecoverConsume(true);
          break;
        end;
    end;
  until false;
end;


Procedure BuildOperand(var instr: TInstruction);

  Procedure BuildConstOperand(var instr: TInstruction);
  var
    l : longint;
    tempstr : string;
  begin
    BuildConstSymbolExpression(true,l,tempstr);
    if tempstr<>'' then
     begin
       instr.operands[operandnum].operandtype:=OPR_SYMBOL;
       instr.operands[operandnum].symofs:=l;
       instr.operands[operandnum].symbol:=newasmsymbol(tempstr);
     end
    else
     begin
       instr.operands[operandnum].operandtype:=OPR_CONSTANT;
       instr.operands[operandnum].val:=l;
     end;
  end;

var
  expr,
  tempstr : string;
  tempreg : tregister;
  lab     : Pasmlabel;
  l,
  toffset,
  tsize   : longint;
  hl      : plabel;
Begin
  tempstr:='';
  expr:='';
  case actasmtoken of

    AS_OFFSET,
    AS_INTNUM,
    AS_PLUS,
    AS_MINUS,
    AS_NOT,
    AS_LPAREN :
      Begin
        if not (instr.operands[operandnum].operandtype in [OPR_NONE,OPR_CONSTANT]) then
          Message(assem_e_invalid_operand_type);
        BuildConstOperand(instr);
      end;

    AS_STRING :
      Begin
        if not (instr.operands[operandnum].operandtype in [OPR_NONE]) then
          Message(assem_e_invalid_operand_type);
        instr.operands[operandnum].operandtype:=OPR_CONSTANT;
        if not PadZero(actasmpattern,4) then
          Message1(assem_e_invalid_string_as_opcode_operand,actasmpattern);
        instr.operands[operandnum].val:=ord(actasmpattern[4]) + ord(actasmpattern[3]) shl 8 +
                                          Ord(actasmpattern[2]) shl 16 + ord(actasmpattern[1]) shl 24;
        Consume(AS_STRING);
      end;


    AS_ID : { A constant expression, or a Variable ref. }
      Begin
        { Label or Special symbol reference? }
        if actasmpattern[1] = '@' then
         Begin
           if actasmpattern = '@RESULT' then
            Begin
              initAsmRef(instr,operandnum);
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
              lab:=labellist.search(actasmpattern);
              { check if the label is already defined   }
              { if so, we then check if the plabel is   }
              { non-nil, if so we add it to instruction }
              if assigned(lab) then
               Begin
                 if assigned(lab^.lab) then
                   Begin
                     instr.operands[operandnum].operandtype:=OPR_LABINSTR;
                     instr.operands[operandnum].hl:=lab^.lab;
                     instr.labeled:=TRUE;
                   end;
               end
              else
              { the label does not exist, create it }
              { emit the opcode, but set that the   }
              { label has not been emitted          }
               Begin
                 getlabel(hl);
                 labellist.insert(actasmpattern,hl,FALSE);
                 instr.operands[operandnum].operandtype:=OPR_LABINSTR;
                 instr.operands[operandnum].hl:=hl;
                 instr.labeled:=TRUE;
               end;
            end;
           Consume(AS_ID);
           if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
             Message(assem_e_syntax_error);
         end
        else
        { support result for delphi modes }
         if (m_objpas in aktmodeswitches) and (actasmpattern='RESULT') then
          begin
            initAsmRef(instr,operandnum);
            SetUpResult(instr,operandnum);
            Consume(AS_ID);
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
              BuildConstOperand(instr);
            end
           else
           { is it a label variable ? }
            if SearchLabel(actasmpattern,hl) then
             Begin
               instr.operands[operandnum].operandtype:=OPR_LABINSTR;
               instr.operands[operandnum].hl:=hl;
               instr.labeled:=TRUE;
               Consume(AS_ID);
               if not (actasmtoken in [AS_SEPARATOR,AS_COMMA]) then
                Message(assem_e_syntax_error);
             end
            else
            { is it a normal variable ? }
             Begin
               initAsmRef(instr,operandnum);
               if not CreateVarInstr(instr,actasmpattern,operandnum) then
                Begin
                  { not a variable.. }
                  { check special variables.. }
                  if actasmpattern = 'SELF' then
                   Begin
                     if assigned(procinfo._class) then
                      Begin
                        instr.operands[operandnum].ref.offset:=procinfo.ESI_offset;
                        instr.operands[operandnum].ref.base:=procinfo.framepointer;
                      end
                     else
                      Message(assem_e_cannot_use_SELF_outside_a_method);
                   end
                  else
                   Message1(assem_e_unknown_id,actasmpattern);
                end;
               l:=0;
               expr:=actasmpattern;
               Consume(AS_ID);
               if actasmtoken=AS_LBRACKET then
                begin
                  instr.operands[operandnum].operandtype:=OPR_REFERENCE;
                  reset_reference(Instr.Operands[OperandNum].Ref);
                  BuildBracketExpression(instr);
                end;
               if actasmtoken=AS_DOT then
                begin
                  if expr='' then
                   writeln('No type specified!')
                  else
                   begin
                     BuildRecordOffsetSize(expr,toffset,tsize);
                     inc(l,toffset);
                     SetOperandSize(instr,operandnum,tsize);
                   end;
                end;
               if actasmtoken in [AS_PLUS,AS_MINUS] then
                inc(l,BuildConstExpression);
               if instr.operands[operandnum].operandtype=OPR_REFERENCE then
                inc(instr.operands[operandnum].ref.offset,l)
               else
                inc(instr.operands[operandnum].val,l);
             end;
         end;
      end;

    AS_REGISTER : { Register, a variable reference or a constant reference }
      Begin
        { save the type of register used. }
        tempreg:=actasmregister;
        Consume(AS_REGISTER);
        if actasmtoken = AS_COLON then
         Begin
           Consume(AS_COLON);
           if actasmtoken <> AS_LBRACKET then
            Message(assem_e_syn_start_with_bracket)
           else
            Begin
              initAsmRef(instr,operandnum);
              instr.operands[operandnum].ref.segment:=tempreg;
              BuildBracketExpression(instr);
            end;
         end
        else
        { Simple register }
         begin
           if not (instr.operands[operandnum].operandtype in [OPR_NONE,OPR_REGISTER]) then
            Message(assem_e_invalid_operand_type);
           instr.operands[operandnum].operandtype:=OPR_REGISTER;
           instr.operands[operandnum].reg:=tempreg;
           instr.operands[operandnum].size:=reg_2_opsize[instr.operands[operandnum].reg];
         end;
      end;

    AS_LBRACKET: { a variable reference, register ref. or a constant reference }
      Begin
        BuildBracketExpression(instr);
      end;

    AS_SEG :
      Begin
        Comment(V_Error,'assem_e_seg_not_supported');
        Consume(actasmtoken);
      end;

    AS_SEPARATOR,
    AS_COMMA: ;

    else
      Message(assem_e_syn_opcode_operand);
  end;
  if not(actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
   begin
     Message(assem_e_syntax_error);
     RecoverConsume(true);
   end;
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
 asmsym,
 expr: string;
 value : longint;
Begin
  strlength:=0; { assume it is a DB }
  Repeat
    Case actasmtoken of
      AS_STRING:
        Begin
          if maxvalue = $ffff then
            strlength:=2
          else
            if maxvalue = $ffffffff then
              strlength:=4;
          { DD and DW cases }
          if strlength <> 0 then
           Begin
             if Not PadZero(actasmpattern,strlength) then
              Message(scan_f_string_exceeds_line);
           end;
          expr:=actasmpattern;
          Consume(AS_STRING);
          Case actasmtoken of
            AS_COMMA:
              Consume(AS_COMMA);
            AS_SEPARATOR: ;
            else
              Message(assem_e_invalid_string_expression);
          end;
          ConcatString(curlist,expr);
        end;
      AS_PLUS,
      AS_MINUS,
      AS_LPAREN,
      AS_NOT,
      AS_INTNUM,
      AS_ID :
        Begin
          BuildConstSymbolExpression(false,value,asmsym);
          if asmsym<>'' then
           begin
             if maxvalue<>$ffffffff then
              Comment(V_Warning,'32bit constant created for address');
             ConcatConstSymbol(curlist,asmsym,value)
           end
          else
           ConcatConstant(curlist,value,maxvalue);
        end;
      AS_COMMA:
        Consume(AS_COMMA);
      AS_SEPARATOR:
        break;
      else
        Message(assem_f_internal_error_in_buildconstant);
    end;
  Until false;
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
var
  PrefixOp,OverrideOp: tasmop;
  expr : string;
  size : topsize;
Begin
  expr:='';
  PrefixOp:=A_None;
  OverrideOp:=A_None;
  { prefix seg opcode / prefix opcode }
  repeat
    if is_prefix(actopcode) then
     begin
       if (PrefixOp<>A_None) or (OverrideOp<>A_None) then
        Message(assem_w_repeat_prefix_and_seg_override);
       PrefixOp:=ActOpcode;
       instr.opcode:=ActOpcode;
       instr.condition:=ActCondition;
       instr.opsize:=ActOpsize;
       ConcatInstruction(curlist,instr);
       Consume(AS_OPCODE);
     end
    else
     if is_override(actopcode) then
      begin
        if (PrefixOp<>A_None) or (OverrideOp<>A_None) then
         Message(assem_w_repeat_prefix_and_seg_override);
        OverrideOp:=ActOpcode;
        instr.opcode:=ActOpcode;
        instr.condition:=ActCondition;
        instr.opsize:=ActOpsize;
        ConcatInstruction(curlist,instr);
        Consume(AS_OPCODE);
      end
    else
     break;
  until (actasmtoken<>AS_OPCODE);
  { opcode }
  if (actasmtoken <> AS_OPCODE) then
   Begin
     Message(assem_e_invalid_or_missing_opcode);
     RecoverConsume(false);
     exit;
   end;
  { Fill the instr object with the current state }
  instr.Opcode:=ActOpcode;
  instr.condition:=ActCondition;
  instr.opsize:=ActOpsize;
  { Valid combination of prefix/override and instruction ?  }
  if (prefixop<>A_NONE) and (NOT CheckPrefix(PrefixOp,actopcode)) then
    Message1(assem_e_invalid_prefix_and_opcode,actasmpattern);
  if (overrideop<>A_NONE) and (NOT CheckOverride(OverrideOp,ActOpcode)) then
    Message1(assem_e_invalid_override_and_opcode,actasmpattern);
  { We are reading operands, so opcode will be an AS_ID }
  operandnum:=1;
  Consume(AS_OPCODE);
  { Zero operand opcode ?  }
  if actasmtoken in [AS_SEPARATOR,AS_END] then
   begin
     operandnum:=0;
     exit;
   end;
  { Read Operands }
  repeat
    case actasmtoken of

      { End of asm operands for this opcode }
      AS_END,
      AS_SEPARATOR :
        break;

      { Operand delimiter }
      AS_COMMA :
        Begin
          if operandnum > MaxOperands then
            Message(assem_e_too_many_operands)
          else
            Inc(operandnum);
          Consume(AS_COMMA);
        end;

      { Typecast, Constant Expression, Type Specifier }
      AS_DWORD,
      AS_BYTE,
      AS_WORD,
      AS_TBYTE,
      AS_QWORD :
        Begin
          { load the size in a temp variable, so it can be set when the
            operand is read }
          Case actasmtoken of
            AS_DWORD : size:=S_L;
            AS_WORD  : size:=S_W;
            AS_BYTE  : size:=S_B;
            AS_QWORD : size:=S_IQ;
            AS_TBYTE : size:=S_FX;
          end;
          Consume(actasmtoken);
          Case actasmtoken of
            { Reference }
            AS_PTR :
              Begin
                Consume(AS_PTR);
                BuildOperand(instr);
              end;
            { Possibly a typecast or a constant }
            { expression.                       }
            AS_LPAREN :
              Begin
                if actasmtoken = AS_ID then
                 Begin
                   { Case vartype of                }
                   {  LOCAL: Replace by offset and  }
                   {         BP in treference.      }
                   {  GLOBAL: Replace by mangledname}
                   {    in symbol of treference     }
                   { Check if next token = RPAREN   }
                   { otherwise syntax error.        }
                   initAsmRef(instr,operandnum);
                   if not CreateVarInstr(instr,actasmpattern,operandnum) then
                     Message1(assem_e_unknown_id,actasmpattern);
                 end
                else
                 begin
                   instr.operands[operandnum].operandtype:=OPR_CONSTANT;
                   instr.operands[operandnum].val:=BuildConstExpression;
                 end;
              end;
            else
              BuildOperand(instr);
          end;
          { now set the size which was specified by the override }
          instr.operands[operandnum].size:=size;
        end;

      { Type specifier }
      AS_NEAR,
      AS_FAR :
        Begin
          if actasmtoken = AS_NEAR then
            Message(assem_w_near_ignored)
          else
            Message(assem_w_far_ignored);
          Consume(actasmtoken);
          if actasmtoken = AS_PTR then
           begin
             initAsmRef(instr,operandnum);
             Consume(AS_PTR);
           end;
          BuildOperand(instr);
        end;

      { Constant expression }
      AS_LPAREN :
        Begin
          instr.operands[operandnum].operandtype:=OPR_CONSTANT;
          instr.operands[operandnum].val:=BuildConstExpression;
        end;

      else
        BuildOperand(instr);
    end; { end case }
  until false;
end;



Function Assemble: Ptree;
{*********************************************************************}
{ PROCEDURE Assemble;                                                 }
{  Description: Parses the intel assembler syntax, parsing is done    }
{  according to the rules in the Turbo Pascal manual.                 }
{*********************************************************************}
Var
  hl : plabel;
  labelptr : pasmlabel;
Begin
  Message(assem_d_start_intel);
  inexpression:=FALSE;
  firsttoken:=TRUE;
  operandnum:=0;
  if assigned(procinfo.retdef) and
     (is_fpu(procinfo.retdef) or
     ret_in_acc(procinfo.retdef)) then
    procinfo.funcret_is_valid:=true;
 { sets up all opcode and register tables in uppercase }
  if not _asmsorted then
   Begin
     SetupTables;
     _asmsorted:=TRUE;
   end;
  curlist:=new(paasmoutput,init);
  { setup label linked list }
  labellist.init;
  c:=current_scanner^.asmgetchar;
  gettoken;

  repeat
    case actasmtoken of
      AS_LLABEL :
        Begin
          labelptr:=labellist.search(actasmpattern);
          if not assigned(labelptr) then
           Begin
             getlabel(hl);
             labellist.insert(actasmpattern,hl,TRUE);
             ConcatLabel(curlist,hl);
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
                   ConcatLabel(curlist,labelptr^.lab);
                 labelptr^.emitted:=TRUE;
               end;
           end;
          Consume(AS_LLABEL);
        end;

      AS_LABEL :
        Begin
          if SearchLabel(actasmpattern,hl) then
            ConcatLabel(curlist,hl)
          else
            Message1(assem_e_unknown_label_identifer,actasmpattern);
          Consume(AS_LABEL);
        end;

      AS_DW :
        Begin
          inexpression:=true;
          Consume(AS_DW);
          BuildConstant($ffff);
          inexpression:=false;
        end;

      AS_DB :
        Begin
          inexpression:=true;
          Consume(AS_DB);
          BuildConstant($ff);
          inexpression:=false;
        end;

      AS_DD :
        Begin
          inexpression:=true;
          Consume(AS_DD);
          BuildConstant($ffffffff);
          inexpression:=false;
        end;

      AS_OPCODE :
        Begin
          instr.init;
          BuildOpcode;
          instr.ops:=operandnum;
          { We need AT&T style operands }
          SwapOperands(instr);
          AddReferenceSizes(instr);
          SetInstructionOpsize(instr);
          CheckOperandSizes(instr);
          ConcatInstruction(curlist,instr);
          instr.done;
          operandnum:=0;
        end;

      AS_SEPARATOR :
        Begin
          Consume(AS_SEPARATOR);
          { let us go back to the first operand }
          operandnum:=0;
        end;

      AS_END :
        break; { end assembly block }

      else
        Begin
          Message(assem_e_assemble_node_syntax_error);
          { error recovery }
          Consume(actasmtoken);
        end;
    end; { end case }
  until false;

  { check if there were undefined symbols.   }
  { if so, then list each of those undefined }
  { labels.                                  }
  if assigned(labellist.First) then
   Begin
     labelptr:=labellist.First;
     if labellist.First <> nil then
      Begin
        { first label }
        if not labelptr^.emitted then
          Message1(assem_e_unknown_local_sym,'@'+labelptr^.name^);
        { other labels ... }
        While (labelptr^.Next <> nil) do
         Begin
           labelptr:=labelptr^.Next;
           if not labelptr^.emitted then
            Message1(assem_e_unknown_local_sym,'@'+labelptr^.name^);
         end;
      end;
   end;
  assemble:=genasmnode(curlist);
  labellist.done;
  Message(assem_d_finish_intel);
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

var
  old_exit : pointer;

procedure ra386int_exit;{$ifndef FPC}far;{$endif}
begin
  if assigned(iasmops) then
    dispose(iasmops);
  if assigned(iasmregs) then
    dispose(iasmregs);
  exitproc:=old_exit;
end;


begin
   old_exit:=exitproc;
   exitproc:=@ra386int_exit;
end.
{
  $Log$
  Revision 1.31  1999-05-01 13:48:41  peter
    * merged nasm compiler

  Revision 1.6  1999/04/26 23:26:18  peter
    * redesigned record offset parsing to support nested records
    * normal compiler uses the redesigned createvarinstr()

  Revision 1.5  1999/04/20 11:01:24  peter
    * better tokenpos info

  Revision 1.4  1999/04/14 09:07:46  peter
    * asm reader improvements

  Revision 1.3  1999/03/06 17:24:27  peter
    * rewritten intel parser a lot, especially reference reading
    * size checking added for asm parsers

  Revision 1.2  1999/03/02 02:56:31  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.1  1999/03/01 15:46:26  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes
}
