{
    $Id$
    Copyright (c) 1998 Carl Eric Codere

    This unit implements some support routines for assembler parsing

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

 **********************************************************************}

Unit AsmUtils;

{*************************************************************************}
{  This unit implements some objects as well as utilities which will be   }
{  used by all inline assembler parsers (non-processor specific).         }
{                                                                         }
{  Main routines/objects herein:                                          }
{  o Object TExprParse is a simple expression parser to resolve assembler }
{    expressions. (Based generally on some code by Thai Tran from SWAG).  }
{  o Object TInstruction is a simple object used for instructions         }
{  o Record TOperand is a simple record used to store information on      }
{    each operand.                                                        }
{  o String conversion routines from octal,binary and hex to decimal.     }
{  o A linked list object/record for local labels                         }
{  o Routines for retrieving symbols (local and global)                   }
{  o Object for a linked list of strings (with duplicate strings not      }
{    allowed).                                                            }
{  o Non-processor dependant routines for adding instructions to the      }
{    instruction list.                                                    }
{*************************************************************************}


{--------------------------------------------------------------------}
{ LEFT TO DO:                                                        }
{ o Fix the remaining bugs in the expression parser, such as with    }
{     4+-3                                                           }
{ o Add support for local typed constants search.                    }
{ o Add support for private/protected fields in method assembler     }
{    routines.                                                       }
{--------------------------------------------------------------------}
Interface

Uses
  symtable,aasm,hcodegen,verbose,systems,globals,files,strings,
  cobjects,
{$ifdef i386}
  i386;
{$endif}
{$ifdef m68k}
   m68k;
{$endif}


Const
  RPNMax = 10;             { I think you only need 4, but just to be safe }
  OpMax  = 25;

  maxoperands = 3;         { Maximum operands for assembler instructions }


Type


  {---------------------------------------------------------------------}
  {                     Label Management types                          }
  {---------------------------------------------------------------------}


    PAsmLabel = ^TAsmLabel;

    { Each local label has this structure associated with it }
    TAsmLabel = record
      name: PString;    { pointer to a pascal string name of label }
      lab: PLabel;      { pointer to a label as defined in FPC     }
      emitted: boolean; { as the label itself been emitted ?       }
      next: PAsmLabel;  { next node                                }
    end;

    TAsmLabelList = Object
    public
      First: PAsmLabel;
      Constructor Init;
      Destructor Done;
      Procedure Insert(s:string; lab: PLabel; emitted: boolean);
      Function Search(const s: string): PAsmLabel;
    private
      Last: PAsmLabel;
      Function NewPasStr(s:string): PString;
    end;



  {---------------------------------------------------------------------}
  {                 Instruction management types                        }
  {---------------------------------------------------------------------}

  toperandtype = (OPR_NONE,OPR_REFERENCE,OPR_CONSTANT,OPR_REGISTER,OPR_LABINSTR,
                  OPR_REGLIST,OPR_SYMBOL);

    { When the TReference field isintvalue = TRUE }
    { then offset points to an ABSOLUTE address   }
    { otherwise isintvalue should always be false }

    { Special cases:                              }
    {   For the M68k Target, size is UNUSED, the  }
    {   opcode determines the size of the         }
    {   instruction.                              }
    {  DIVS/DIVU/MULS/MULU of the form dn,dn:dn   }
    {  is stored as three operands!!              }


    { Each instruction operand can be of this type }
    TOperand = record
      size: topsize;
      opinfo: longint; { ao_xxxx flags }
      overriden : boolean; { indicates if the opcode has been overriden }
                           { by a pseudo-opcode such as DWORD PTR       }
      case operandtype:toperandtype of
       { the size of the opr_none field should be at least equal to each }
       { other field as to facilitate initialization.                    }
       OPR_NONE: (l: array[1..sizeof(treference)] of byte);
       OPR_REFERENCE: (ref:treference);
       OPR_CONSTANT:  (val: longint);
       OPR_REGISTER:  (reg:tregister);
       OPR_LABINSTR: (hl: plabel);
       { Register list such as in the movem instruction }
       OPR_REGLIST:  (list: set of tregister);
       OPR_SYMBOL : (symbol:pstring);
    end;



    TInstruction = object
    public
      operands: array[1..maxoperands] of TOperand;
      { if numops = zero, a size may still be valid in operands[1] }
      { it still should be checked.                                }
      numops: byte;
      { set to TRUE if the instruction is labeled.                }
      labeled: boolean;
      { This is used for instructions such A_CMPSB... etc, to determine }
      { the size of the instruction.                                    }
      stropsize: topsize;
      procedure init;
      procedure done;
      { sets up the prefix field with the instruction pointed to in s }
      procedure addprefix(tok: tasmop);
      { sets up the instruction with the instruction pointed to in s }
      procedure addinstr(tok: tasmop);
      { get the current instruction of this object }
      function getinstruction: tasmop;
      { get the current prefix of this instruction }
      function getprefix: tasmop;
    private
      prefix: tasmop;
      instruction: tasmop;
    end;




  {---------------------------------------------------------------------}
  {                   Expression parser types                           }
  {---------------------------------------------------------------------}

  { expression parser error codes }
  texpr_error =
  (zero_divide,       { divide by zero.     }
   stack_overflow,    { stack overflow.     }
   stack_underflow,   { stack underflow.    }
   invalid_number,    { invalid conversion  }
   invalid_op);       { invalid operator    }


   TExprOperator = record
    ch: char;           { operator }
    is_prefix: boolean; { was it a prefix, possible prefixes are +,- and not }
   end;

  String15 = String[15];
  {**********************************************************************}
  { The following operators are supported:                              }
  {  '+' : addition                                                     }
  {  '-' : subtraction                                                  }
  {  '*' : multiplication                                               }
  {  '/' : modulo division                                              }
  {  '^' : exclusive or                                                 }
  {  '<' : shift left                                                   }
  {  '>' : shift right                                                  }
  {  '&' : bitwise and                                                  }
  {  '|' : bitwise or                                                   }
  {  '~' : bitwise complement                                           }
  {  '%' : modulo division                                              }
  {  nnn: longint numbers                                               }
  {  ( and ) parenthesis                                                }
  {**********************************************************************}

  TExprParse = Object
    public
     Constructor Init;
     Destructor Done;
     Function Evaluate(Expr:  String): longint;
     Procedure Error(anerror: texpr_error); virtual;
     Function Priority(_Operator: Char): Integer; virtual;
    private
     RPNStack   : Array[1..RPNMax] of longint;        { Stack For RPN calculator }
     RPNTop     : Integer;
     OpStack    : Array[1..OpMax] of TExprOperator;    { Operator stack For conversion }
     OpTop      : Integer;
     Procedure RPNPush(Num: Longint);
     Function RPNPop: Longint;
     Procedure RPNCalc(token: String15; prefix: boolean);
     Procedure OpPush(_Operator: char; prefix: boolean);
     { In reality returns TExprOperaotr }
     Procedure OpPop(var _Operator:TExprOperator);
  end;


  {---------------------------------------------------------------------}
  {                     String routines                                 }
  {---------------------------------------------------------------------}


  {*********************************************************************}
  { PROCEDURE PadZero;                                                  }
  {  Description: Makes sure that the string specified is of the given  }
  {  length, by padding it with binary zeros, or truncating if necessary}
  {  Remark: The return value is determined BEFORE any eventual padding.}
  {  Return Value: TRUE  = if length of string s was <= then n          }
  {                FALSE = if length of string s was > then n           }
  {*********************************************************************}
  Function PadZero(Var s: String; n: byte): Boolean;

  { Converts an Hex digit string to a Decimal string                      }
  { Returns '' if there was an error.                                     }
  Function HexToDec(const S:String): String;

  { Converts a binary digit string to a Decimal string                    }
  { Returns '' if there was an error.                                     }
  Function BinaryToDec(const S:String): String;

  { Converts an octal digit string to a Decimal string                    }
  { Returns '' if there was an error.                                     }
  Function OctalToDec(const S:String): String;

  { Converts a string containing C styled escape sequences to }
  { a pascal style string.                                    }
  Function EscapeToPascal(const s:string): string;

  Procedure ConcatPasString(p : paasmoutput;s:string);
  { Writes the string s directly to the assembler output }
  Procedure ConcatDirect(p : paasmoutput;s:string);


  {---------------------------------------------------------------------}
  {                     Symbol helper routines                          }
  {---------------------------------------------------------------------}

  Function GetTypeOffset(var Instr: TInstruction; const base: string; const field: string;
    Var Offset: longint; operandnum: byte):boolean;
  Function GetVarOffset(var Instr: TInstruction;const base: string; const field: string;
    Var Offset: longint; operandnum: byte):boolean;
  Function SearchIConstant(const s:string; var l:longint): boolean;
  Function SearchLabel(const s: string; var hl: plabel): boolean;
  Function CreateVarInstr(var Instr: TInstruction; const hs:string;
     operandnum:byte):boolean;
  {*********************************************************************}
  { FUNCTION NewPasStr(s:string): PString                               }
  {  Description: This routine allocates a string on the heap and       }
  {  returns a pointer to the allocated string.                         }
  {                                                                     }
  {  Remarks: The string allocated should not be modified, since it's   }
  {  length will be less then 255.                                      }
  {  Remarks: It is assumed that HeapError will be called if an         }
  {  allocation fails.                                                  }
  {*********************************************************************}
  Function newpasstr(s: string): Pointer;
  Procedure SetupResult(Var Instr:TInstruction; operandnum: byte);
{$ifdef i386}

  Procedure FWaitWarning;
{$endif}

  {---------------------------------------------------------------------}
  {                  Instruction generation routines                    }
  {---------------------------------------------------------------------}

  { swaps in the case of a 2/3 operand opcode the destination and the    }
  { source as to put it in AT&T style instruction format.                }
  Procedure SwapOperands(Var instr: TInstruction);
  Procedure ConcatLabel(p : paasmoutput;op : tasmop;var l : plabel);
  Procedure ConcatConstant(p : paasmoutput;value: longint; maxvalue: longint);
  Procedure ConcatRealConstant(p : paasmoutput;value: bestreal; real_typ : tfloattype);
  Procedure ConcatString(p : paasmoutput;s:string);
  Procedure ConcatPublic(p:paasmoutput;const s : string);
  Procedure ConcatLocal(p:paasmoutput;const s : string);
  Procedure ConcatGlobalBss(const s : string;size : longint);
  Procedure ConcatLocalBss(const s : string;size : longint);
  { add to list of external labels }
  Procedure ConcatExternal(const s : string;typ : texternal_typ);
  { add to internal list of labels }
  Procedure ConcatInternal(const s : string;typ : texternal_typ);

Implementation

{*************************************************************************}
{                         Expression Parser                               }
{*************************************************************************}

Constructor TExprParse.Init;
Begin
end;

Procedure TExprParse.Error(anerror:texpr_error);
var
  t : tmsgconst;
Begin
  case anerror of
  zero_divide: t:=assem_f_ev_zero_divide;
  stack_overflow: t:=assem_f_ev_stack_overflow;
  stack_underflow: t:=assem_f_ev_stack_underflow;
  invalid_number: t:=assem_f_ev_invalid_number;
  invalid_op: t:=assem_f_ev_invalid_op;
  else
   t:=assem_f_ev_unknown;
  end;
  Message(t);
end;

Procedure TExprParse.RPNPush(Num : longint); { Add an operand to the top of the RPN stack }
begin
  if RPNTop < RPNMax then
  begin
    Inc(RPNTop);
    RPNStack[RPNTop] := Num;
  end
  else
    Error(stack_overflow); { Put some error handler here }
end;




Function TExprParse.RPNPop : longint;       { Get the operand at the top of the RPN stack }
begin
  if RPNTop > 0 then
  begin
    RPNPop := RPNStack[RPNTop];
    Dec(RPNTop);
  end
  else  { Put some error handler here }
   Error(stack_underflow);
end;

Procedure TExprParse.RPNCalc(Token : String15; prefix:boolean);                       { RPN Calculator }
Var
  Temp  : longint;
  LocalError : Integer;
begin
{  Write(Token, ' ');              This just outputs the RPN expression }

  if (Length(Token) = 1) and (Token[1] in ['+', '-', '*', '/','&','|','%','^','~','<','>']) then
  Case Token[1] of                                   { Handle operators }
    '+' : Begin
       if prefix then
       else
          RPNPush(RPNPop + RPNPop);
     end;
    '-' : Begin
      if prefix then
         RPNPush(-(RPNPop))
      else
         RPNPush(RPNPop - RPNPop);
     end;
    '*' : RPNPush(RPNPop * RPNPop);
    '&' : RPNPush(RPNPop AND RPNPop);
    '|' : RPNPush(RPNPop OR RPNPop);
    '~' : RPNPush(NOT RPNPop);
    '<' : RPNPush(RPNPop SHL RPNPop);
    '>' : RPNPush(RPNPop SHR RPNPop);
    '%' : begin
      Temp := RPNPop;
      if Temp <> 0 then
       RPNPush(RPNPop mod Temp)
      else Error(zero_divide); { Handle divide by zero error }
     end;
    '^' : RPNPush(RPNPop XOR RPNPop);
    '/' :
    begin
      Temp := RPNPop;
      if Temp <> 0 then
   RPNPush(RPNPop div Temp)
      else  Error(zero_divide);{ Handle divide by 0 error }
    end;
  end
  else
  begin                   { Convert String to number and add to stack }
    if token='-2147483648' then
      begin
         temp:=$80000000;
         localerror:=0;
      end
    else
      Val(Token, Temp, LocalError);
    if LocalError = 0 then
      RPNPush(Temp)
    else  Error(invalid_number);{ Handle error }
  end;
end;

Procedure TExprParse.OpPush(_Operator : char;prefix: boolean);  { Add an operator onto top of the stack }
begin
  if OpTop < OpMax then
  begin
    Inc(OpTop);
    OpStack[OpTop].ch := _Operator;
    OpStack[OpTop].is_prefix := prefix;
  end
  else Error(stack_overflow); { Put some error handler here }
end;

Procedure TExprParse.OpPop(var _Operator:TExprOperator);               { Get operator at the top of the stack }
begin
  if OpTop > 0 then
  begin
    _Operator := OpStack[OpTop];
    Dec(OpTop);
  end
  else Error(stack_underflow); { Put some error handler here }
end;

Function TExprParse.Priority(_Operator : Char) : Integer; { Return priority of operator }
{ The greater the priority, the higher the precedence }
begin
  Case _Operator OF
    '('      : Priority := 0;
    '+', '-' : Priority := 1;
    '*', '/','%','<','>' : Priority := 2;
    '|','&','^','~': Priority := 0;
    else  Error(invalid_op);{ More error handling }
  end;
end;


Function TExprParse.Evaluate(Expr : String):longint;
Var
  I     : Integer;
  Token : String15;
  opr: TExprOperator;
begin
  OpTop  := 0;                                              { Reset stacks }
  RPNTop := 0;
  Token  := '';

  For I := 1 to Length(Expr) DO
   begin
     if Expr[I] in ['0'..'9'] then
      begin       { Build multi-digit numbers }
        Token := Token + Expr[I];
        if I = Length(Expr) then          { Send last one to calculator }
         RPNCalc(Token,false);
      end
     else
      if Expr[I] in ['+', '-', '*', '/', '(', ')','^','&','|','%','~','<','>'] then
       begin
         if Token <> '' then
          begin        { Send last built number to calc. }
            RPNCalc(Token,false);
            Token := '';
          end;

         Case Expr[I] OF
          '(' : OpPush('(',false);
          ')' : begin
                  While OpStack[OpTop].ch <> '(' DO
                   Begin
                     OpPop(opr);
                     RPNCalc(opr.ch,opr.is_prefix);
                   end;
                  OpPop(opr);                          { Pop off and ignore the '(' }
                end;
  '+','-','~' : Begin
                  { workaround for -2147483648 }
                  if (expr[I]='-') and (expr[i+1] in ['0'..'9']) then
                   begin
                     token:='-';
                     expr[i]:='+';
                   end;
                  { if start of expression then surely a prefix }
                  { or if previous char was also an operator    }
                  if (I = 1) or (not (Expr[I-1] in ['0'..'9','(',')'])) then
                    OpPush(Expr[I],true)
                  else
                    Begin
                    { Evaluate all higher priority operators }
                      While (OpTop > 0) AND (Priority(Expr[I]) <= Priority(OpStack[OpTop].ch)) DO
                       Begin
                         OpPop(opr);
                         RPNCalc(opr.ch,opr.is_prefix);
                       end;
                      OpPush(Expr[I],false);
                    End;
                end;
     '*', '/',
  '^','|','&',
  '%','<','>' : begin
                  While (OpTop > 0) and (Priority(Expr[I]) <= Priority(OpStack[OpTop].ch)) DO
                   Begin
                     OpPop(opr);
                     RPNCalc(opr.ch,opr.is_prefix);
                   end;
                  OpPush(Expr[I],false);
                end;
         end; { Case }
       end
     else
      Error(invalid_op);  { Handle bad input error }
   end;

{ Pop off the remaining operators }
  While OpTop > 0 do
   Begin
     OpPop(opr);
     RPNCalc(opr.ch,opr.is_prefix);
   end;

{ The result is stored on the top of the stack }
  Evaluate := RPNPop;
end;


Destructor TExprParse.Done;
Begin
end;


{*************************************************************************}
{                         String conversions/utils                        }
{*************************************************************************}

  Function newpasstr(s: string): Pointer;
  Var
   StrPtr: PString;
  Begin
    GetMem(StrPtr, length(s)+1);
    Move(s,StrPtr^,length(s)+1);
    newpasstr:= Strptr;
  end;


  Function EscapeToPascal(const s:string): string;
  { converts a C styled string - which contains escape }
  { characters to a pascal style string.               }
  var
   i,j: word;
   str: string;
   temp: string;
   value: byte;
   code: integer;
  Begin
   str:='';
   i:=1;
   j:=1;
   repeat
     if s[i] = '\' then
     Begin
      Inc(i);
      if i > 255 then
      Begin
       EscapeToPascal:=str;
       exit;
      end;
      case s[i] of
       '\': insert('\',str,j);
       'b': insert(#08,str,j);
       'f': insert(#12,str,j);
       'n': insert(#10,str,j);
       'r': insert(#13,str,j);
       't': insert(#09,str,j);
       '"': insert('"',str,j);
       { octal number }
       '0'..'7': Begin
                  temp:=s[i];
                  temp:=temp+s[i+1];
                  temp:=temp+s[i+2];
                  inc(i,2);
                  val(octaltodec(temp),value,code);
                  if (code <> 0) then
                   Message(assem_w_invalid_numeric);
                  insert(chr(value),str,j);
                 end;
     { hexadecimal number }
     'x': Begin
            temp:=s[i+1];
            temp:=temp+s[i+2];
            inc(i,2);
            val(hextodec(temp),value,code);
            if (code <> 0) then
             Message(assem_w_invalid_numeric);
            insert(chr(value),str,j);
          end;
     else
      Begin
         Message1(assem_e_escape_seq_ignored,s[i]);
         insert(s[i],str,j);
      end;
    end; {end case }
    Inc(i);
   end
   else
   Begin
    Insert(s[i],str,j);
    Inc(i);
    if i > 255 then
    Begin
     EscapeToPascal:=str;
     exit;
    end;
   end;
   Inc(j);
 until (i > length(s)) or (j > 255);
 EscapeToPascal:=str;
end;



  Function OctalToDec(const S:String): String;
  { Converts an octal string to a Decimal string }
  { Returns '' if there was an error.            }
  var vs: longint;
    c: byte;
    st: string;
  Begin
   vs := 0;
   for c:=1 to length(s) do
   begin
     case s[c] of
     '0': vs:=vs shl 3;
     '1': vs:=vs shl 3+1;
     '2': vs:=vs shl 3+2;
     '3': vs:=vs shl 3+3;
     '4': vs:=vs shl 3+4;
     '5': vs:=vs shl 3+5;
     '6': vs:=vs shl 3+6;
     '7': vs:=vs shl 3+7;
    else
      begin
        OctalToDec := '';
        exit;
      end;
    end;
   end;
     str(vs,st);
     OctalToDec := st;
  end;

  Function BinaryToDec(const S:String): String;
  { Converts a binary string to a Decimal string }
  { Returns '' if there was an error.            }
  var vs: longint;
    c: byte;
    st: string;
  Begin
   vs := 0;
   for c:=1 to length(s) do
   begin
     if s[c] = '0' then
       vs:=vs shl 1
     else
     if s[c]='1' then
       vs:=vs shl 1+1
     else
       begin
         BinaryToDec := '';
         exit;
       end;
   end;
     str(vs,st);
     BinaryToDec := st;
  end;


  Function HexToDec(const S:String): String;
  var vs: longint;
    c: byte;
    st: string;
  Begin
   vs := 0;
   for c:=1 to length(s) do
   begin
     case upcase(s[c]) of
     '0': vs:=vs shl 4;
     '1': vs:=vs shl 4+1;
     '2': vs:=vs shl 4+2;
     '3': vs:=vs shl 4+3;
     '4': vs:=vs shl 4+4;
     '5': vs:=vs shl 4+5;
     '6': vs:=vs shl 4+6;
     '7': vs:=vs shl 4+7;
     '8': vs:=vs shl 4+8;
     '9': vs:=vs shl 4+9;
     'A': vs:=vs shl 4+10;
     'B': vs:=vs shl 4+11;
     'C': vs:=vs shl 4+12;
     'D': vs:=vs shl 4+13;
     'E': vs:=vs shl 4+14;
     'F': vs:=vs shl 4+15;
    else
      begin
        HexToDec := '';
        exit;
      end;
    end;
   end;
     str(vs,st);
     HexToDec := st;
  end;

  Function PadZero(Var s: String; n: byte): Boolean;
  Begin
    PadZero := TRUE;
    { Do some error checking first }
    if Length(s) = n then
      exit
    else
    if Length(s) > n then
    Begin
      PadZero := FALSE;
      delete(s,n+1,length(s));
      exit;
    end
    else
      PadZero := TRUE;
    { Fill it up with the specified character }
    fillchar(s[length(s)+1],n-1,#0);
    s[0] := chr(n);
  end;

{*************************************************************************}
{                          Instruction utilities                          }
{*************************************************************************}

 Procedure TInstruction.init;
 var
  k: integer;
 Begin
  numops := 0;
  labeled := FALSE;
  stropsize := S_NO;
  prefix := A_NONE;
  instruction := A_NONE;
  for k:=1 to maxoperands do
  begin
    operands[k].size := S_NO;
    operands[k].overriden := FALSE;
    operands[k].operandtype := OPR_NONE;
    { init to zeros }
    fillchar(operands[k].l, sizeof(operands[k].l),#0);
  end;
 end;

 Procedure TInstruction.addprefix(tok: tasmop);
 Begin
   if tok = A_NONE then
    Message(assem_e_syn_prefix_not_found);
   if Prefix = A_NONE then
    Prefix := tok
   else
    Message(assem_e_syn_try_add_more_prefix);
 end;

 Procedure TInstruction.addinstr(tok: tasmop);
 Begin
   if tok = A_NONE then
    Message(assem_e_syn_opcode_not_found);
   Instruction := tok;
 end;

 function TInstruction.getinstruction: tasmop;
 Begin
   getinstruction := Instruction;
 end;
      { get the current prefix of this instruction }
 function TInstruction.getprefix: tasmop;
 Begin
   getprefix := prefix;
 end;

 Procedure TInstruction.done;
 var
  k: integer;
 Begin
  for k:=1 to numops do
    begin
       if (operands[k].operandtype=OPR_REFERENCE) and
          assigned(operands[k].ref.symbol) then
            stringdispose(operands[k].ref.symbol);
       if (operands[k].operandtype=OPR_SYMBOL) and
          assigned(operands[k].symbol) then
            stringdispose(operands[k].symbol);
    end;
 end;

{*************************************************************************}
{                          Local label utilities                          }
{*************************************************************************}

  Constructor TAsmLabelList.Init;
  Begin
    First := nil;
    Last := nil;
  end;


  Procedure TAsmLabelList.Insert(s:string; lab: PLabel; emitted: boolean);
  {*********************************************************************}
  {  Description: Insert a node at the end of the list with lab and     }
  {  and the name in s. The name is allocated on the heap.              }
  {  Duplicates are not allowed.                                        }
  {  Indicate in emitted if this label itself has been emitted, or is it}
  {  a simple labeled instruction?                                      }
  {*********************************************************************}
  Begin
    if search(s) = nil then
    Begin
      if First = nil then
       Begin
          New(First);
          Last := First;
       end
      else
       Begin
          New(Last^.Next);
          Last := Last^.Next;
       end;
      Last^.name := NewPasStr(s);
      Last^.Lab := lab;
      Last^.Next := nil;
      Last^.emitted := emitted;
    end;
  end;



  Function TAsmLabelList.Search(const s: string): PAsmLabel;
  {*********************************************************************}
  {  Description: This routine searches for a label named s in the      }
  {  linked list, returns a pointer to the label if found, otherwise    }
  {  returns nil.                                                       }
  {*********************************************************************}
  Var
    asmlab: PAsmLabel;
  Begin
    asmlab := First;
    if First = nil then
    Begin
      Search := nil;
      exit;
    end;
    While (asmlab^.name^ <> s) and (asmlab^.Next <> nil) do
       asmlab := asmlab^.Next;
    if asmlab^.name^ = s then
       search := asmlab
    else
       search := nil;
  end;


  Destructor TAsmLabelList.Done;
  {*********************************************************************}
  {  Description: This routine takes care of deallocating all nodes     }
  {  in the linked list, as well as deallocating the string pointers    }
  {  of these nodes.                                                    }
  {                                                                     }
  {  Remark: The PLabel field is NOT freed, the compiler takes care of  }
  {  this.                                                              }
  {*********************************************************************}
  Var
    temp: PAsmLabel;
    temp1: PAsmLabel;
  Begin
    temp := First;
    while temp <> nil do
    Begin
      Freemem(Temp^.name, length(Temp^.name^)+1);
      Temp1 := Temp^.Next;
      Dispose(Temp);
      Temp := Temp1;
      { The plabel could be deleted here, but let us not do }
      { it, FPC will do it instead.                         }
    end;
  end;



  Function TAsmLabelList.newpasstr(s: string): PString;
  {*********************************************************************}
  { FUNCTION NewPasStr(s:string): PString                               }
  {  Description: This routine allocates a string on the heap and       }
  {  returns a pointer to the allocated string.                         }
  {                                                                     }
  {  Remarks: The string allocated should not be modified, since it's   }
  {  length will be less then 255.                                      }
  {  Remarks: It is assumed that HeapError will be called if an         }
  {  allocation fails.                                                  }
  {*********************************************************************}
  Var
   StrPtr: PString;
  Begin
    GetMem(StrPtr, length(s)+1);
    Move(s,StrPtr^,length(s)+1);
    newpasstr:= Strptr;
  end;

{*************************************************************************}
{                      Symbol table helper routines                       }
{*************************************************************************}


  Procedure SwapOperands(Var instr: TInstruction);
  Var
   tempopr: TOperand;
  Begin
    if instr.numops = 2 then
    Begin
      tempopr := instr.operands[1];
      instr.operands[1] := instr.operands[2];
      instr.operands[2] := tempopr;
    end
    else
    if instr.numops = 3 then
    Begin
      tempopr := instr.operands[1];
      instr.operands[1] := instr.operands[3];
      instr.operands[3] := tempopr;
    end;
  end;


  Function SearchIConstant(const s:string; var l:longint): boolean;
  {**********************************************************************}
  {  Description: Searches for a CONSTANT of name s in either the local  }
  {  symbol list, then in the global symbol list, and returns the value  }
  {  of that constant in l. Returns TRUE if successfull, if not found,   }
  {  or if the constant is not of correct type, then returns FALSE       }
  { Remarks: Also handle TRUE and FALSE returning in those cases 1 and 0 }
  {  respectively.                                                       }
  {**********************************************************************}
  var
    sym: psym;
  Begin
    SearchIConstant := FALSE;
    { check for TRUE or FALSE reserved words first }
    if s = 'TRUE' then
     Begin
       SearchIConstant := TRUE;
       l := 1;
     end
    else
     if s = 'FALSE' then
      Begin
        SearchIConstant := TRUE;
        l := 0;
      end
    else
     if assigned(aktprocsym) then
      Begin
        if assigned(aktprocsym^.definition) then
         Begin
         { Check the local constants }
           if assigned(aktprocsym^.definition^.localst) then
            sym := aktprocsym^.definition^.localst^.search(s)
           else
            sym := nil;
           if assigned(sym) then
            Begin
              if (sym^.typ = constsym) and
                 (pconstsym(sym)^.consttype in [constord,constint,constchar,constbool]) then
               Begin
                 l:=pconstsym(sym)^.value;
                 SearchIConstant := TRUE;
                 exit;
               end;
            end;
         end;
      end;
    { Check the global constants }
    getsym(s,false);
    if srsym <> nil then
     Begin
       if (srsym^.typ=constsym) and
          (pconstsym(srsym)^.consttype in [constord,constint,constchar,constbool]) then
        Begin
          l:=pconstsym(srsym)^.value;
          SearchIConstant := TRUE;
          exit;
        end;
     end;
  end;


  Procedure SetupResult(Var Instr:TInstruction; operandnum: byte);
  {**********************************************************************}
  {  Description: This routine changes the correct fields and correct    }
  {  offset in the reference, so that it points to the __RESULT or       }
  {  @Result variable (depending on the inline asm).                     }
  {  Resturns a reference with all correct offset correctly set up.      }
  {  The Operand should already point to a treference on entry.          }
  {**********************************************************************}
  Begin
    { replace by correct offset. }
    if assigned(procinfo.retdef) and
      (procinfo.retdef<>pdef(voiddef)) then
    begin
      instr.operands[operandnum].ref.offset := procinfo.retoffset;
      instr.operands[operandnum].ref.base :=  procinfo.framepointer;
      { always assume that the result is valid. }
      procinfo.funcret_is_valid:=true;
    end
    else
     Message(assem_e_invalid_symbol_ref);
  end;


{$ifdef i386}
  Procedure FWaitWarning;
  begin
    if (target_info.target=target_i386_GO32V2) and (cs_fp_emulation in aktmoduleswitches) then
     Message(assem_w_fwait_emu_prob);
  end;
{$endif i386}



  Function GetVarOffset(var Instr: TInstruction;const base: string; const field: string;
    Var Offset: longint; operandnum: byte):boolean;
  { search and returns the offset of records/objects of the base }
  { with field name setup in field.                              }
  { returns FALSE if not found.                                      }
  { used when base is a variable or a typed constant name.       }
   var
    sym:psym;
    p: psym;
  Begin
     GetVarOffset := FALSE;
     Offset := 0;
     { local list }
     if assigned(aktprocsym) then
     begin
      if assigned(aktprocsym^.definition^.localst) then
        sym:=aktprocsym^.definition^.localst^.search(base)
      else
        sym:=nil;
      if assigned(sym) then
      begin
        { field of local record variable. }
        if (sym^.typ=varsym) and (pvarsym(sym)^.definition^.deftype=recorddef) then
          begin
             p:=pvarsym(precdef(pvarsym(sym)^.definition)^.symtable^.search(field));
             if assigned(pvarsym(p)) then
             Begin
                Offset := pvarsym(p)^.address;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                GetVarOffset := TRUE;
                Exit;
             end;
          end
        else
        if (sym^.typ=varsym) and (pvarsym(sym)^.definition^.deftype=objectdef) then
          begin
             if assigned(pobjectdef(pvarsym(sym)^.definition)^.publicsyms) then
               begin
                  p:=pvarsym(pobjectdef(pvarsym(sym)^.definition)^.publicsyms^.search(field));
                  if assigned(pvarsym(p)) then
                    Begin
                      Offset := pvarsym(p)^.address;
                      { the current size is NOT overriden if it already }
                      { exists, such as in the case of a byte ptr, in   }
                      { front of the identifier.                        }
                      if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                        Begin
                          case pvarsym(p)^.getsize of
                          1: instr.operands[operandnum].size := S_B;
                          2: instr.operands[operandnum].size := S_W{ could be S_IS};
                          4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                          8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                         extended_size: instr.operands[operandnum].size := S_FX;
                         else
                         { this is in the case where the instruction is LEA }
                         { or something like that, in that case size is not }
                         { important.                                       }
                           instr.operands[operandnum].size := S_NO;
                         end; { end case }
                       end;
                     GetVarOffset := TRUE;
                     Exit;
                    end;
               end;
          end;
      end
      else
       begin
        { field of local record parameter to routine. }
         if assigned(aktprocsym^.definition^.parast) then
            sym:=aktprocsym^.definition^.parast^.search(base)
         else
           sym:=nil;
         if assigned(sym) then
         begin
           if (sym^.typ=varsym) and (pvarsym(sym)^.definition^.deftype=recorddef)
           then
           begin
             p:=pvarsym(precdef(pvarsym(sym)^.definition)^.symtable^.search(field));
             if assigned(p) then
             Begin
                Offset := pvarsym(p)^.address;
                GetVarOffset := TRUE;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                Exit;
             end;
           end { endif }
         else
         if (sym^.typ=varsym) and (pvarsym(sym)^.definition^.deftype=objectdef) then
          begin
             if assigned(pobjectdef(pvarsym(sym)^.definition)^.publicsyms) then
               begin
                  p:=pvarsym(pobjectdef(pvarsym(sym)^.definition)^.publicsyms^.search(field));
                  if assigned(pvarsym(p)) then
                    Begin
                      Offset := pvarsym(p)^.address;
                      { the current size is NOT overriden if it already }
                      { exists, such as in the case of a byte ptr, in   }
                      { front of the identifier.                        }
                      if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                        Begin
                          case pvarsym(p)^.getsize of
                          1: instr.operands[operandnum].size := S_B;
                          2: instr.operands[operandnum].size := S_W{ could be S_IS};
                          4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                          8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                         extended_size: instr.operands[operandnum].size := S_FX;
                         else
                         { this is in the case where the instruction is LEA }
                         { or something like that, in that case size is not }
                         { important.                                       }
                           instr.operands[operandnum].size := S_NO;
                         end; { end case }
                       end;
                     GetVarOffset := TRUE;
                     Exit;
                    end;
               end;
          end;
         end;
        end;
      end; { endif assigned(aktprocsym) }

     { not found.. .now look for global variables. }
     getsym(base,false);
     sym:=srsym;
     if assigned(sym) then
     Begin
        { field of global record variable. }
        if (sym^.typ=varsym) and (pvarsym(sym)^.definition^.deftype=recorddef) then
          begin
             p:=pvarsym(precdef(pvarsym(sym)^.definition)^.symtable^.search(field));
             if assigned(p) then
             Begin
                Offset := pvarsym(p)^.address;
                GetVarOffset := TRUE;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                Exit;
             end;
          end
        else
        { field of global record type constant. }
        if (sym^.typ=typedconstsym) and (ptypedconstsym(sym)^.definition^.deftype=recorddef)
          then
          begin
             p:=pvarsym(precdef(pvarsym(sym)^.definition)^.symtable^.search(field));
             if assigned(p) then
             Begin
                Offset := pvarsym(p)^.address;
                GetVarOffset := TRUE;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                Exit;
             end;
          end
        else
        if (sym^.typ=varsym) and (pvarsym(sym)^.definition^.deftype=objectdef) then
          begin
             if assigned(pobjectdef(pvarsym(sym)^.definition)^.publicsyms) then
               begin
                  p:=pvarsym(pobjectdef(pvarsym(sym)^.definition)^.publicsyms^.search(field));
                  if assigned(pvarsym(p)) then
                    Begin
                      Offset := pvarsym(p)^.address;
                      { the current size is NOT overriden if it already }
                      { exists, such as in the case of a byte ptr, in   }
                      { front of the identifier.                        }
                      if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                        Begin
                          case pvarsym(p)^.getsize of
                          1: instr.operands[operandnum].size := S_B;
                          2: instr.operands[operandnum].size := S_W{ could be S_IS};
                          4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                          8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                         extended_size: instr.operands[operandnum].size := S_FX;
                         else
                         { this is in the case where the instruction is LEA }
                         { or something like that, in that case size is not }
                         { important.                                       }
                           instr.operands[operandnum].size := S_NO;
                         end; { end case }
                       end;
                     GetVarOffset := TRUE;
                     Exit;
                    end;
               end;
          end;
     end; { end looking for global variables .. }
  end;



  Function GetTypeOffset(var instr: TInstruction; const base: string; const field: string;
    Var Offset: longint; operandnum: byte):boolean;
  { search and returns the offset of records/objects of the base }
  { with field name setup in field.                              }
  { returns 0 if not found.                                      }
  { used when base is a variable or a typed constant name.       }
   var
    sym:psym;
    p: psym;
  Begin
     Offset := 0;
     GetTypeOffset := FALSE;
     { local list }
     if assigned(aktprocsym) then
     begin
      if assigned(aktprocsym^.definition^.localst) then
        sym:=aktprocsym^.definition^.localst^.search(base)
      else
        sym:=nil;
      if assigned(sym) then
      begin
        { field of local record type. }
        if (sym^.typ=typesym) and (ptypesym(sym)^.definition^.deftype=recorddef) then
          begin
             p:=precdef(ptypesym(sym)^.definition)^.symtable^.search(field);
             if assigned(p) then
             Begin
                Offset := pvarsym(p)^.address;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                GetTypeOffset := TRUE;
                Exit;
             end;
          end;
      end
      else
       begin
        { field of local record type to routine. }
         if assigned(aktprocsym^.definition^.parast) then
            sym:=aktprocsym^.definition^.parast^.search(base)
         else
           sym:=nil;
         if assigned(sym) then
         begin
           if (sym^.typ=typesym) and (ptypesym(sym)^.definition^.deftype=recorddef)
           then
           begin
             p:=precdef(ptypesym(sym)^.definition)^.symtable^.search(field);
             if assigned(p) then
             Begin
                Offset := pvarsym(p)^.address;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                GetTypeOffset := TRUE;
                Exit;
             end;
           end; { endif }
         end; {endif }
       end; { endif }
     end;

     { not found.. .now look for global types. }
     getsym(base,false);
     sym:=srsym;
     if assigned(sym) then
     Begin
        { field of global record types. }
        if (sym^.typ=typesym) and (ptypesym(sym)^.definition^.deftype=recorddef) then
          begin
             p:=precdef(ptypesym(sym)^.definition)^.symtable^.search(field);
             if assigned(p) then
             Begin
                Offset := pvarsym(p)^.address;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                GetTypeOffset := TRUE;
                Exit;
             end
          end
        else
        { public field names of objects }
        if (sym^.typ=typesym) and (ptypesym(sym)^.definition^.deftype=objectdef)then
          begin
             if assigned(pobjectdef(ptypesym(sym)^.definition)^.publicsyms) then
             Begin
               p:=pobjectdef(ptypesym(sym)^.definition)^.publicsyms^.search(field);
               if assigned(p) then
               Begin
                  Offset := pvarsym(p)^.address;
                { the current size is NOT overriden if it already }
                { exists, such as in the case of a byte ptr, in   }
                { front of the identifier.                        }
                if instr.operands[operandnum].size = S_NO then
                Begin
                  case pvarsym(p)^.getsize of
                   1: instr.operands[operandnum].size := S_B;
                   2: instr.operands[operandnum].size := S_W{ could be S_IS};
                   4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                   8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                   extended_size: instr.operands[operandnum].size := S_FX;
                  else
                   { this is in the case where the instruction is LEA }
                   { or something like that, in that case size is not }
                   { important.                                       }
                    instr.operands[operandnum].size := S_NO;
                  end; { end case }
                end;
                  GetTypeOffset := TRUE;
                  Exit;
               end
             end;
          end;
     end; { end looking for global variables .. }
  end;


  Function CreateVarInstr(var Instr: TInstruction; const hs:string;operandnum:byte): Boolean;
  { search and sets up the correct fields in the Instr record }
  { for the NON-constant identifier passed to the routine.    }
  { if not found returns FALSE.                               }
  var
    sym : psym;
    l   : longint;
  Begin
    CreateVarInstr := FALSE;
  { are we in a routine ? }
    if assigned(aktprocsym) then
     begin
     { search the local list for the name of this variable. }
       if assigned(aktprocsym^.definition^.localst) then
        sym:=aktprocsym^.definition^.localst^.search(hs)
       else
        sym:=nil;
       if assigned(sym) then
        begin
          case sym^.typ of
  typedconstsym,
         varsym : begin
                    { we always assume in asm statements that     }
                    { that the variable is valid.                 }
                    pvarsym(sym)^.is_valid:=1;
                    if pvarsym(sym)^.owner^.symtabletype=staticsymtable then
                     begin
                       if assigned(instr.operands[operandnum].ref.symbol) then
                         FreeMem(instr.operands[operandnum].ref.symbol,length(instr.operands[operandnum].ref.symbol^)+1);
                       instr.operands[operandnum].ref.symbol:=newpasstr(pvarsym(sym)^.mangledname);
                     end
                    else
                     begin
                       instr.operands[operandnum].ref.base := procinfo.framepointer;
                       instr.operands[operandnum].ref.offset := -(pvarsym(sym)^.address);
                     end;
                    { the current size is NOT overriden if it already }
                    { exists, such as in the case of a byte ptr, in   }
                    { front of the identifier.                        }
                   if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                    Begin
                      case pvarsym(sym)^.getsize of
                       1: instr.operands[operandnum].size := S_B;
                       2: instr.operands[operandnum].size := S_W{ could be S_IS};
                       4: instr.operands[operandnum].size := S_L{ could be S_IL or S_FS};
                       8: instr.operands[operandnum].size := S_IQ{ could be S_D or S_FL};
                       extended_size: instr.operands[operandnum].size := S_FX;
                      else
                        { this is in the case where the instruction is LEA }
                        { or something like that, in that case size is not }
                        { important.                                       }
                        instr.operands[operandnum].size := S_NO;
                      end; { end case }
                    end;
                    { ok, finished for this var }
                    CreateVarInstr := TRUE;
                    Exit;
                  end;
       constsym : begin
                    if pconstsym(sym)^.consttype in [constint,constchar,constbool] then
                     begin
                       instr.operands[operandnum].operandtype:=OPR_CONSTANT;
                       instr.operands[operandnum].val:=pconstsym(sym)^.value;
                       CreateVarInstr := TRUE;
                       Exit;
                     end;
                  end;
        procsym : begin
                    { free the memory before changing the symbol name. }
                    if assigned(instr.operands[operandnum].ref.symbol) then
                      FreeMem(instr.operands[operandnum].ref.symbol,length(instr.operands[operandnum].ref.symbol^)+1);
                    instr.operands[operandnum].operandtype:=OPR_SYMBOL;
                    instr.operands[operandnum].symbol:=newpasstr(pprocsym(sym)^.definition^.mangledname);
                    CreateVarInstr := TRUE;
                    Exit;
                  end
          else
           begin
             Message(assem_e_unsupported_symbol_type);
             exit;
           end;
          end;
        end;

     { now check for parameters passed to routine }
       if assigned(aktprocsym^.definition^.parast) then
        sym:=aktprocsym^.definition^.parast^.search(hs)
       else
        sym:=nil;
       if assigned(sym) then
        begin
          case sym^.typ of
         varsym : begin
                    l:=pvarsym(sym)^.address;
                    { set offset }
                    inc(l,aktprocsym^.definition^.parast^.call_offset);
                    pvarsym(sym)^.is_valid:=1;
                    instr.operands[operandnum].ref.base := procinfo.framepointer;
                    instr.operands[operandnum].ref.offset := l;
                    { the current size is NOT overriden if it already }
                    { exists, such as in the case of a byte ptr, in   }
                    { front of the identifier.                        }
                    if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                    Begin
                      case pvarsym(sym)^.getsize of
                        1: instr.operands[operandnum].size := S_B;
                        2: instr.operands[operandnum].size := S_W;
                        4: instr.operands[operandnum].size := S_L;
                        8: instr.operands[operandnum].size := S_IQ;
                        extended_size: instr.operands[operandnum].size := S_FX;
                      else
                      { this is in the case where the instruction is LEA }
                      { or something like that, in that case size is not }
                      { important.                                       }
                        instr.operands[operandnum].size := S_NO;
                      end; { end case }
                    end; { endif }
                    CreateVarInstr := TRUE;
                    Exit;
                  end;
          else
           begin
             Message(assem_e_unsupported_symbol_type);
             exit;
           end;
          end; { case }
        end; { endif }
     end;
  { not found.. .now look for global variables. }
    getsym(hs,false);
    sym:=srsym;
    if assigned(sym) then
     Begin
       case sym^.typ of
          varsym,
   typedconstsym : Begin
                   { free the memory before changing the symbol name. }
                     if assigned(instr.operands[operandnum].ref.symbol) then
                      FreeMem(instr.operands[operandnum].ref.symbol,length(instr.operands[operandnum].ref.symbol^)+1);
                     instr.operands[operandnum].ref.symbol:=newpasstr(sym^.mangledname);
                   { the current size is NOT overriden if it already }
                   { exists, such as in the case of a byte ptr, in   }
                   { front of the identifier.                        }
                   if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
                      Begin
                        case pvarsym(sym)^.getsize of
                         1: instr.operands[operandnum].size := S_B;
                         2: instr.operands[operandnum].size := S_W;
                         4: instr.operands[operandnum].size := S_L;
                         8: instr.operands[operandnum].size := S_IQ;
                        else
                      { this is in the case where the instruction is LEA }
                      { or something like that, in that case size is not }
                      { important.                                       }
                         instr.operands[operandnum].size := S_NO;
                        end;
                      end
                     else
                      if (instr.operands[operandnum].size = S_NO) and (sym^.typ = typedconstsym) then
                       Begin
                       { only these are valid sizes, otherwise prefixes are }
                       { required.                                          }
                         case ptypedconstsym(sym)^.definition^.size of
                          1: instr.operands[operandnum].size := S_B;
                          2: instr.operands[operandnum].size := S_W;
                          4: instr.operands[operandnum].size := S_L;
                          8: instr.operands[operandnum].size := S_IQ;
                         else
                         { this is in the case where the instruction is LEA }
                         { or something like that, in that case size is not }
                         { important.                                       }
                           instr.operands[operandnum].size := S_NO;
                         end;
                    end; { endif }
                    CreateVarInstr := TRUE;
                    Exit;
                  end;
       constsym : begin
                    if pconstsym(sym)^.consttype in [constint,constchar,constbool] then
                     begin
                       instr.operands[operandnum].operandtype:=OPR_CONSTANT;
                       instr.operands[operandnum].val:=pconstsym(sym)^.value;
                       CreateVarInstr := TRUE;
                       Exit;
                     end;
                  end;
        procsym : begin
                    if assigned(pprocsym(sym)^.definition^.nextoverloaded) then
                     Message(assem_w_calling_overload_func);
                    { free the memory before changing the symbol name. }
                    if assigned(instr.operands[operandnum].ref.symbol) then
                      FreeMem(instr.operands[operandnum].ref.symbol,length(instr.operands[operandnum].ref.symbol^)+1);
                    instr.operands[operandnum].operandtype:=OPR_SYMBOL;
                    instr.operands[operandnum].size:=S_L;
                    instr.operands[operandnum].symbol:=newpasstr(pprocsym(sym)^.definition^.mangledname);
                    CreateVarInstr := TRUE;
                    Exit;
                  end;
       else
        begin
          Message(assem_e_unsupported_symbol_type);
          exit;
        end;
       end; {case}
     end; { end looking for global variables .. }
  end;



  Function SearchLabel(const s: string; var hl: plabel): boolean;
  {**********************************************************************}
  {  Description: Searches for a pascal label definition, first in the   }
  {  local symbol list and then in the global symbol list. If found then }
  {  return pointer to label and return true, otherwise returns false.   }
  {**********************************************************************}
  var
    sym: psym;
  Begin
    SearchLabel := FALSE;
    if assigned(aktprocsym) then
    Begin
      { Check the local constants }
    if assigned(aktprocsym^.definition) then
    Begin
        if assigned(aktprocsym^.definition^.localst) then
          sym := aktprocsym^.definition^.localst^.search(s)
      else
       sym := nil;
      if assigned(sym) then
      Begin
       if (sym^.typ = labelsym) then
       Begin
          hl:=plabelsym(sym)^.number;
          SearchLabel := TRUE;
          exit;
       end;
      end;
    end;
  end;
    { Check the global label symbols... }
    getsym(s,false);
    if srsym <> nil then
    Begin
      if (srsym^.typ=labelsym) then
      Begin
        hl:=plabelsym(srsym)^.number;
        SearchLabel:= TRUE;
        exit;
      end;
    end;
  end;


 {*************************************************************************}
 {                   Instruction Generation Utilities                      }
 {*************************************************************************}


   Procedure ConcatString(p : paasmoutput;s:string);
  {*********************************************************************}
  { PROCEDURE ConcatString(s:string);                                   }
  {  Description: This routine adds the character chain pointed to in   }
  {  s to the instruction linked list.                                  }
  {*********************************************************************}
  Var
   pc: PChar;
  Begin
     getmem(pc,length(s)+1);
     p^.concat(new(pai_string,init_length_pchar(strpcopy(pc,s),length(s))));
  end;

  Procedure ConcatPasString(p : paasmoutput;s:string);
  {*********************************************************************}
  { PROCEDURE ConcatPasString(s:string);                                }
  {  Description: This routine adds the character chain pointed to in   }
  {  s to the instruction linked list, contrary to ConcatString it      }
  {  uses a pascal style string, so it conserves null characters.       }
  {*********************************************************************}
  Begin
     p^.concat(new(pai_string,init(s)));
  end;

  Procedure ConcatDirect(p : paasmoutput;s:string);
  {*********************************************************************}
  { PROCEDURE ConcatDirect(s:string)                                    }
  {  Description: This routine output the string directly to the asm    }
  {  output, it is only sed when writing special labels in AT&T mode,   }
  {  and should not be used without due consideration, since it may     }
  {  cause problems.                                                    }
  {*********************************************************************}
  Var
   pc: PChar;
  Begin
     getmem(pc,length(s)+1);
     p^.concat(new(pai_direct,init(strpcopy(pc,s))));
  end;




   Procedure ConcatConstant(p: paasmoutput; value: longint; maxvalue: longint);
  {*********************************************************************}
  { PROCEDURE ConcatConstant(value: longint; maxvalue: longint);        }
  {  Description: This routine adds the value constant to the current   }
  {  instruction linked list.                                           }
  {   maxvalue -> indicates the size of the data to initialize:         }
  {                  $ff -> create a byte node.                         }
  {                  $ffff -> create a word node.                       }
  {                  $ffffffff -> create a dword node.                  }
  {*********************************************************************}
  Begin
      if value > maxvalue then
      Begin
         Message(assem_e_constant_out_of_bounds);
         { assuming a value of maxvalue }
         value := maxvalue;
      end;
      if maxvalue = $ff then
          p^.concat(new(pai_const,init_8bit(byte(value))))
      else
      if maxvalue = $ffff then
          p^.concat(new(pai_const,init_16bit(word(value))))
      else
      if maxvalue = $ffffffff then
          p^.concat(new(pai_const,init_32bit(longint(value))));
  end;

  Procedure ConcatRealConstant(p : paasmoutput;value: bestreal; real_typ : tfloattype);
  {***********************************************************************}
  { PROCEDURE ConcatRealConstant(value: bestreal; real_typ : tfloattype); }
  {  Description: This routine adds the value constant to the current     }
  {  instruction linked list.                                             }
  {   real_typ -> indicates the type of the real data to initialize:      }
  {                  s32real -> create a single node.                     }
  {                  s64real -> create a double node.                     }
  {                  s80real -> create an extended node.                  }
  {                  s64bit ->  create a  comp node.                      }
  {                  f32bit ->  create a  fixed node. (not used normally) }
  {***********************************************************************}
    Begin
       case real_typ of
          s32real : p^.concat(new(pai_single,init(value)));
          s64real : p^.concat(new(pai_double,init(value)));
          s80real : p^.concat(new(pai_extended,init(value)));
          s64bit  : p^.concat(new(pai_comp,init(value)));
          f32bit  : p^.concat(new(pai_const,init_32bit(trunc(value*$10000))));
       end;
    end;

   Procedure ConcatLabel(p: paasmoutput;op : tasmop;var l : plabel);
  {*********************************************************************}
  { PROCEDURE ConcatLabel                                               }
  {  Description: This routine either emits a label or a labeled        }
  {  instruction to the linked list of instructions.                    }
  {*********************************************************************}
   begin
         if op=A_LABEL then
           p^.concat(new(pai_label,init(l)))
         else
           p^.concat(new(pai_labeled,init(op,l)))
   end;

   procedure ConcatPublic(p:paasmoutput;const s : string);
  {*********************************************************************}
  { PROCEDURE ConcatPublic                                              }
  {  Description: This routine emits an global   definition to the      }
  {  linked list of instructions.(used by AT&T styled asm)              }
  {*********************************************************************}
   begin
       p^.concat(new(pai_symbol,init_global(s)));
       { concat_internal(s,EXT_NEAR); done in aasm }
   end;

   procedure ConcatLocal(p:paasmoutput;const s : string);
  {*********************************************************************}
  { PROCEDURE ConcatLocal                                               }
  {  Description: This routine emits an local    definition to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       p^.concat(new(pai_symbol,init(s)));
       { concat_internal(s,EXT_NEAR); done in aasm }
   end;

  Procedure ConcatGlobalBss(const s : string;size : longint);
  {*********************************************************************}
  { PROCEDURE ConcatGlobalBss                                           }
  {  Description: This routine emits an global  datablock   to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       bsssegment^.concat(new(pai_datablock,init_global(s,size)));
       { concat_internal(s,EXT_NEAR); done in aasm }
   end;

  Procedure ConcatLocalBss(const s : string;size : longint);
  {*********************************************************************}
  { PROCEDURE ConcatLocalBss                                            }
  {  Description: This routine emits a local datablcok      to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       bsssegment^.concat(new(pai_datablock,init(s,size)));
       { concat_internal(s,EXT_NEAR); done in aasm }
   end;

  { add to list of external labels }
  Procedure ConcatExternal(const s : string;typ : texternal_typ);
  {*********************************************************************}
  { PROCEDURE ConcatExternal                                            }
  {  Description: This routine emits an external definition to the      }
  {  linked list of instructions.(used by AT&T styled asm)              }
  {*********************************************************************}
  { check if in internal list and remove it there                       }
  var p : pai_external;
   begin
       p:=search_assembler_symbol(internals,s,typ);
       if p<>nil then internals^.remove(p);
       concat_external(s,typ);
   end;

  { add to internal list of labels }
  Procedure ConcatInternal(const s : string;typ : texternal_typ);
  {*********************************************************************}
  { PROCEDURE ConcatInternal                                            }
  {  Description: This routine emits an internal definition of a symbol }
  {  (used by AT&T styled asm for undefined labels)                     }
  {*********************************************************************}
   begin
       concat_internal(s,typ);
   end;

end.
{
  $Log$
  Revision 1.13  1998-10-28 00:08:45  peter
    + leal procsym,eax is now allowed
    + constants are now handled also when starting an expression
    + call *pointer is now allowed

  Revision 1.12  1998/10/14 11:28:13  florian
    * emitpushreferenceaddress gets now the asmlist as parameter
    * m68k version compiles with -duseansistrings

  Revision 1.11  1998/10/13 16:49:59  pierre
    * undid some changes of Peter that made the compiler wrong
      for m68k (I had to reinsert some ifdefs)
    * removed several memory leaks under m68k
    * removed the meory leaks for assembler readers
    * cross compiling shoud work again better
      ( crosscompiling sysamiga works
       but as68k still complain about some code !)

  Revision 1.10  1998/10/13 13:10:10  peter
    * new style for m68k/i386 infos and enums

  Revision 1.9  1998/09/24 17:54:15  carl
    * bugfixes from fix branch

  Revision 1.8.2.1  1998/09/24 17:46:25  carl
   * support for objects in asm statements

  Revision 1.8  1998/08/27 00:43:06  carl
    +} now record offsets searches set the operand sizes

  Revision 1.7  1998/08/18 20:51:32  peter
    * fixed bug 42

  Revision 1.6  1998/08/10 14:49:40  peter
    + localswitches, moduleswitches, globalswitches splitting

  Revision 1.5  1998/07/14 21:46:38  peter
    * updated messages file

  Revision 1.4  1998/06/04 23:51:31  peter
    * m68k compiles
    + .def file creation moved to gendef.pas so it could also be used
      for win32

  Revision 1.3  1998/05/31 14:13:30  peter
    * fixed call bugs with assembler readers
    + OPR_SYMBOL to hold a symbol in the asm parser
    * fixed staticsymtable vars which were acessed through %ebp instead of
      name

  Revision 1.2  1998/04/29 10:33:43  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions
}
