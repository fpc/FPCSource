{
    $Id$
    Copyright (c) 1998 Carl Eric Codere

    This unit implements some support routines for assembler parsing
    independent of the processor

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
Unit RAUtils;
Interface

Uses
  globtype,systems,
  symtable,aasm,hcodegen,verbose,globals,files,strings,
  cobjects,
{$ifdef i386}
  i386base;
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

    { Each local label has this structure associated with it }
    PAsmLabel = ^TAsmLabel;
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
      opinfo: longint; { ot_ flags }
      overriden : boolean; { indicates if the opcode has been overriden }
                           { by a pseudo-opcode such as DWORD PTR       }
      hasvar : boolean; { if the operand is loaded with a variable }
      case operandtype:toperandtype of
       { the size of the opr_none field should be at least equal to each }
       { other field as to facilitate initialization.                    }
       OPR_NONE: (l: array[1..sizeof(treference)] of byte);
       OPR_CONSTANT:  (val: longint);
         { the fakeval is here so the val of a constant will not override
           any field in the reference (PFV) }
       OPR_REFERENCE: (fakeval:longint;ref:treference);
       OPR_REGISTER:  (reg:tregister);
       OPR_LABINSTR: (hl: plabel);
       { Register list such as in the movem instruction }
       OPR_REGLIST:  (list: set of tregister);
       OPR_SYMBOL : (symofs:longint;symbol:pasmsymbol);
    end;


    TInstruction = object
    public
      opcode    : tasmop;
      opsize    : topsize;
      condition : tasmcond;
      ops       : byte;
      operands: array[1..maxoperands] of TOperand;
      { set to TRUE if the instruction is labeled }
      labeled: boolean;
      procedure init;
      procedure done;
    end;


  {---------------------------------------------------------------------}
  {                   Expression parser types                           }
  {---------------------------------------------------------------------}

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

  { Evaluate an expression string to a longint }
  Function CalculateExpression(const expression: string): longint;

  {---------------------------------------------------------------------}
  {                     String routines                                 }
  {---------------------------------------------------------------------}

Function ValDecimal(const S:String):longint;
Function ValOctal(const S:String):longint;
Function ValBinary(const S:String):longint;
Function ValHexaDecimal(const S:String):longint;

  {*********************************************************************}
  { PROCEDURE PadZero;                                                  }
  {  Description: Makes sure that the string specified is of the given  }
  {  length, by padding it with binary zeros, or truncating if necessary}
  {  Remark: The return value is determined BEFORE any eventual padding.}
  {  Return Value: TRUE  = if length of string s was <= then n          }
  {                FALSE = if length of string s was > then n           }
  {*********************************************************************}
  Function PadZero(Var s: String; n: byte): Boolean;

  { Converts a string containing C styled escape sequences to }
  { a pascal style string.                                    }
  Function EscapeToPascal(const s:string): string;


  {---------------------------------------------------------------------}
  {                     Symbol helper routines                          }
  {---------------------------------------------------------------------}

  Procedure SetOperandSize(var instr:TInstruction;operandnum,size:longint);
  Function GetRecordOffsetSize(s:string;Var Offset: longint;var Size:longint):boolean;
  Function SearchIConstant(const s:string; var l:longint): boolean;
  Function SearchLabel(const s: string; var hl: plabel): boolean;
  Function SearchDirectVar(var Instr: TInstruction; const hs:string;operandnum:byte): Boolean;
  Function CreateVarInstr(var Instr: TInstruction; const hs:string;
     operandnum:byte):boolean;

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
  Procedure ConcatPasString(p : paasmoutput;s:string);
  { Writes the string s directly to the assembler output }
  Procedure ConcatDirect(p : paasmoutput;s:string);
  Procedure ConcatLabel(p: paasmoutput;var l : plabel);
  Procedure ConcatConstant(p : paasmoutput;value: longint; maxvalue: longint);
  Procedure ConcatConstSymbol(p : paasmoutput;const sym:string;l:longint);
  Procedure ConcatRealConstant(p : paasmoutput;value: bestreal; real_typ : tfloattype);
  Procedure ConcatString(p : paasmoutput;s:string);
  procedure ConcatAlign(p:paasmoutput;l:longint);
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

Procedure TExprParse.RPNPush(Num : longint);
{ Add an operand to the top of the RPN stack }
begin
  if RPNTop < RPNMax then
   begin
     Inc(RPNTop);
     RPNStack[RPNTop]:=Num;
   end
  else
   Message(asmr_e_ev_stack_overflow);
end;


Function TExprParse.RPNPop : longint;
{ Get the operand at the top of the RPN stack }
begin
  if RPNTop > 0 then
   begin
     RPNPop:=RPNStack[RPNTop];
     Dec(RPNTop);
   end
  else
   Message(asmr_e_ev_stack_underflow);
end;


Procedure TExprParse.RPNCalc(Token : String15; prefix:boolean);                       { RPN Calculator }
Var
  Temp  : longint;
  LocalError : Integer;
begin
  { Handle operators }
  if (Length(Token) = 1) and (Token[1] in ['+', '-', '*', '/','&','|','%','^','~','<','>']) then
   Case Token[1] of
    '+' :
      Begin
        if not prefix then
         RPNPush(RPNPop + RPNPop);
      end;
    '-' :
      Begin
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
    '%' :
      begin
        Temp:=RPNPop;
        if Temp <> 0 then
         RPNPush(RPNPop mod Temp)
        else
         Message(asmr_e_ev_zero_divide);
      end;
    '^' : RPNPush(RPNPop XOR RPNPop);
    '/' :
      begin
        Temp:=RPNPop;
        if Temp <> 0 then
         RPNPush(RPNPop div Temp)
        else
         Message(asmr_e_ev_zero_divide);
      end;
   end
  else
   begin
     { Convert String to number and add to stack }
     if token='-2147483648' then
      begin
        temp:=$80000000;
        localerror:=0;
      end
     else
      Val(Token, Temp, LocalError);
     if LocalError = 0 then
      RPNPush(Temp)
     else
      Message(asmr_e_ev_invalid_number);
   end;
end;


Procedure TExprParse.OpPush(_Operator : char;prefix: boolean);
{ Add an operator onto top of the stack }
begin
  if OpTop < OpMax then
   begin
     Inc(OpTop);
     OpStack[OpTop].ch:=_Operator;
     OpStack[OpTop].is_prefix:=prefix;
   end
  else
   Message(asmr_e_ev_stack_overflow);
end;


Procedure TExprParse.OpPop(var _Operator:TExprOperator);
{ Get operator at the top of the stack }
begin
  if OpTop > 0 then
   begin
     _Operator:=OpStack[OpTop];
     Dec(OpTop);
   end
  else
   Message(asmr_e_ev_stack_underflow);
end;


Function TExprParse.Priority(_Operator : Char) : Integer;
{ Return priority of operator }
{ The greater the priority, the higher the precedence }
begin
  Case _Operator OF
    '(' :
      Priority:=0;
    '+', '-' :
      Priority:=1;
    '*', '/','%','<','>' :
      Priority:=2;
    '|','&','^','~' :
      Priority:=0;
  else
    Message(asmr_e_ev_invalid_op);
  end;
end;


Function TExprParse.Evaluate(Expr : String):longint;
Var
  I     : Integer;
  Token : String15;
  opr   : TExprOperator;
begin
  Evaluate:=0;
  { Reset stacks }
  OpTop :=0;
  RPNTop:=0;
  Token :='';
  { nothing to do ? }
  if Expr='' then
   exit;
  For I:=1 to Length(Expr) DO
   begin
     if Expr[I] in ['0'..'9'] then
      begin       { Build multi-digit numbers }
        Token:=Token + Expr[I];
        if I = Length(Expr) then          { Send last one to calculator }
         RPNCalc(Token,false);
      end
     else
      if Expr[I] in ['+', '-', '*', '/', '(', ')','^','&','|','%','~','<','>'] then
       begin
         if Token <> '' then
          begin        { Send last built number to calc. }
            RPNCalc(Token,false);
            Token:='';
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
      Message(asmr_e_ev_invalid_op);  { Handle bad input error }
   end;

{ Pop off the remaining operators }
  While OpTop > 0 do
   Begin
     OpPop(opr);
     RPNCalc(opr.ch,opr.is_prefix);
   end;

{ The result is stored on the top of the stack }
  Evaluate:=RPNPop;
end;


Destructor TExprParse.Done;
Begin
end;


Function CalculateExpression(const expression: string): longint;
var
  expr: TExprParse;
Begin
  expr.Init;
  CalculateExpression:=expr.Evaluate(expression);
  expr.Done;
end;


{*************************************************************************}
{                         String conversions/utils                        }
{*************************************************************************}

Function EscapeToPascal(const s:string): string;
{ converts a C styled string - which contains escape }
{ characters to a pascal style string.               }
var
  i,len : longint;
  hs    : string;
  temp  : string;
  c     : char;
Begin
  hs:='';
  len:=0;
  i:=0;
  while (i<length(s)) and (len<255) do
   begin
     Inc(i);
     if (s[i]='\') and (i<length(s)) then
      Begin
        inc(i);
        case s[i] of
         '\' :
           c:='\';
         'b':
           c:=#8;
         'f':
           c:=#12;
         'n':
           c:=#10;
         'r':
           c:=#13;
         't':
           c:=#9;
         '"':
           c:='"';
         '0'..'7':
           Begin
             temp:=s[i];
             temp:=temp+s[i+1];
             temp:=temp+s[i+2];
             inc(i,2);
             c:=chr(ValOctal(temp));
           end;
         'x':
           Begin
             temp:=s[i+1];
             temp:=temp+s[i+2];
             inc(i,2);
             c:=chr(ValHexaDecimal(temp));
           end;
         else
           Begin
             Message1(asmr_e_escape_seq_ignored,s[i]);
             c:=s[i];
           end;
        end;
      end
     else
      c:=s[i];
     inc(len);
     hs[len]:=c;
   end;
  hs[0]:=chr(len);
  EscapeToPascal:=hs;
end;


Function ValDecimal(const S:String):longint;
{ Converts a decimal string to longint }
var
  vs,c : longint;
Begin
  vs:=0;
  for c:=1 to length(s) do
   begin
     vs:=vs*10;
     if s[c] in ['0'..'9'] then
      inc(vs,ord(s[c])-ord('0'))
     else
      begin
        Message1(asmr_e_error_converting_decimal,s);
        ValDecimal:=0;
        exit;
      end;
   end;
  ValDecimal:=vs;
end;


Function ValOctal(const S:String):longint;
{ Converts an octal string to longint }
var
  vs,c : longint;
Begin
  vs:=0;
  for c:=1 to length(s) do
   begin
     vs:=vs shl 3;
     if s[c] in ['0'..'7'] then
      inc(vs,ord(s[c])-ord('0'))
     else
      begin
        Message1(asmr_e_error_converting_octal,s);
        ValOctal:=0;
        exit;
      end;
   end;
  ValOctal:=vs;
end;


Function ValBinary(const S:String):longint;
{ Converts a binary string to longint }
var
  vs,c : longint;
Begin
  vs:=0;
  for c:=1 to length(s) do
   begin
     vs:=vs shl 1;
     if s[c] in ['0'..'1'] then
      inc(vs,ord(s[c])-ord('0'))
     else
      begin
        Message1(asmr_e_error_converting_binary,s);
        ValBinary:=0;
        exit;
      end;
   end;
  ValBinary:=vs;
end;


Function ValHexadecimal(const S:String):longint;
{ Converts a binary string to longint }
var
  vs,c : longint;
Begin
  vs:=0;
  for c:=1 to length(s) do
   begin
     vs:=vs shl 4;
     case s[c] of
       '0'..'9' :
         inc(vs,ord(s[c])-ord('0'));
       'A'..'F' :
         inc(vs,ord(s[c])-ord('A')+10);
       'a'..'f' :
         inc(vs,ord(s[c])-ord('a')+10);
       else
         begin
           Message1(asmr_e_error_converting_hexadecimal,s);
           ValHexadecimal:=0;
           exit;
         end;
     end;
   end;
  ValHexadecimal:=vs;
end;


Function PadZero(Var s: String; n: byte): Boolean;
Begin
  PadZero:=TRUE;
  { Do some error checking first }
  if Length(s) = n then
    exit
  else
  if Length(s) > n then
  Begin
    PadZero:=FALSE;
    delete(s,n+1,length(s));
    exit;
  end
  else
    PadZero:=TRUE;
  { Fill it up with the specified character }
  fillchar(s[length(s)+1],n-1,#0);
  s[0]:=chr(n);
end;


{****************************************************************************
                                 TInstruction
****************************************************************************}

Procedure TInstruction.init;
Begin
  Opcode:=A_NONE;
  Opsize:=S_NO;
  Condition:=C_NONE;
  labeled:=FALSE;
  Ops:=0;
  FillChar(Operands,sizeof(Operands),0);
end;


Procedure TInstruction.done;
Begin
end;


{*************************************************************************}
{                          Local label utilities                          }
{*************************************************************************}

  Constructor TAsmLabelList.Init;
  Begin
    First:=nil;
    Last:=nil;
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
          Last:=First;
       end
      else
       Begin
          New(Last^.Next);
          Last:=Last^.Next;
       end;
      Last^.name:=stringdup(s);
      Last^.Lab:=lab;
      Last^.Next:=nil;
      Last^.emitted:=emitted;
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
    asmlab:=First;
    if First = nil then
    Begin
      Search:=nil;
      exit;
    end;
    While (asmlab^.name^ <> s) and (asmlab^.Next <> nil) do
       asmlab:=asmlab^.Next;
    if asmlab^.name^ = s then
       search:=asmlab
    else
       search:=nil;
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
    temp:=First;
    while temp <> nil do
    Begin
      Freemem(Temp^.name, length(Temp^.name^)+1);
      Temp1:=Temp^.Next;
      Dispose(Temp);
      Temp:=Temp1;
      { The plabel could be deleted here, but let us not do }
      { it, FPC will do it instead.                         }
    end;
  end;


{*************************************************************************}
{                      Symbol table helper routines                       }
{*************************************************************************}

  Procedure SwapOperands(Var instr: TInstruction);
  Var
   tempopr: TOperand;
  Begin
    if instr.Ops = 2 then
    Begin
      tempopr:=instr.operands[1];
      instr.operands[1]:=instr.operands[2];
      instr.operands[2]:=tempopr;
    end
    else
    if instr.Ops = 3 then
    Begin
      tempopr:=instr.operands[1];
      instr.operands[1]:=instr.operands[3];
      instr.operands[3]:=tempopr;
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
    SearchIConstant:=FALSE;
    { check for TRUE or FALSE reserved words first }
    if s = 'TRUE' then
     Begin
       SearchIConstant:=TRUE;
       l:=1;
     end
    else
     if s = 'FALSE' then
      Begin
        SearchIConstant:=TRUE;
        l:=0;
      end
    else
     if assigned(aktprocsym) then
      Begin
        if assigned(aktprocsym^.definition) then
         Begin
         { Check the local constants }
           if assigned(aktprocsym^.definition^.localst) and
              (lexlevel >= normal_function_level) then
            sym:=aktprocsym^.definition^.localst^.search(s)
           else
            sym:=nil;
           if assigned(sym) then
            Begin
              if (sym^.typ = constsym) and
                 (pconstsym(sym)^.consttype in [constord,constint,constchar,constbool]) then
               Begin
                 l:=pconstsym(sym)^.value;
                 SearchIConstant:=TRUE;
                 exit;
               end;
            end;
         end;
      end;
    { Check the global constants }
    getsym(s,false);
    if srsym <> nil then
     Begin
       case srsym^.typ of
         constsym :
           begin
             if (pconstsym(srsym)^.consttype in [constord,constint,constchar,constbool]) then
              Begin
                l:=pconstsym(srsym)^.value;
                SearchIConstant:=TRUE;
                exit;
              end;
           end;
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
       instr.operands[operandnum].ref.offset:=procinfo.retoffset;
       instr.operands[operandnum].ref.base:= procinfo.framepointer;
       { always assume that the result is valid. }
       procinfo.funcret_is_valid:=true;
     end
    else
     Message(asmr_e_void_function);
  end;


{$ifdef i386}
  Procedure FWaitWarning;
  begin
    if (target_info.target=target_i386_GO32V2) and (cs_fp_emulation in aktmoduleswitches) then
     Message(asmr_w_fwait_emu_prob);
  end;
{$endif i386}


  Procedure SetOperandSize(var instr:TInstruction;operandnum,size:longint);
  begin
    { the current size is NOT overriden if it already }
    { exists, such as in the case of a byte ptr, in   }
    { front of the identifier.                        }
    if (instr.operands[operandnum].size = S_NO) or (instr.operands[operandnum].overriden = FALSE) then
    Begin
      case size of
       1: instr.operands[operandnum].size:=S_B;
       2: instr.operands[operandnum].size:=S_W{ could be S_IS};
       4: instr.operands[operandnum].size:=S_L{ could be S_IL or S_FS};
       8: instr.operands[operandnum].size:=S_IQ{ could be S_D or S_FL};
       extended_size: instr.operands[operandnum].size:=S_FX;
      else
       { this is in the case where the instruction is LEA }
       { or something like that, in that case size is not }
       { important.                                       }
        instr.operands[operandnum].size:=S_NO;
      end; { end case }
    end;
  end;


Function GetRecordOffsetSize(s:string;Var Offset: longint;var Size:longint):boolean;
{ search and returns the offset and size of records/objects of the base }
{ with field name setup in field.                              }
{ returns FALSE if not found.                                  }
{ used when base is a variable or a typed constant name.       }
var
  st   : psymtable;
  sym  : psym;
  i    : longint;
  base : string;
Begin
  GetRecordOffsetSize:=FALSE;
  Offset:=0;
  Size:=0;
  i:=pos('.',s);
  if i=0 then
   i:=255;
  base:=Copy(s,1,i-1);
  delete(s,1,i);
  getsym(base,false);
  sym:=srsym;
  st:=nil;
  { we can start with a var,type,typedconst }
  case sym^.typ of
    varsym :
      begin
        case pvarsym(sym)^.definition^.deftype of
          recorddef :
            st:=precdef(pvarsym(sym)^.definition)^.symtable;
          objectdef :
            st:=pobjectdef(pvarsym(sym)^.definition)^.publicsyms;
        end;
      end;
    typesym :
      begin
        case ptypesym(sym)^.definition^.deftype of
          recorddef :
            st:=precdef(ptypesym(sym)^.definition)^.symtable;
          objectdef :
            st:=pobjectdef(ptypesym(sym)^.definition)^.publicsyms;
        end;
      end;
    typedconstsym :
      begin
        case pvarsym(sym)^.definition^.deftype of
          recorddef :
            st:=precdef(ptypedconstsym(sym)^.definition)^.symtable;
          objectdef :
            st:=pobjectdef(ptypedconstsym(sym)^.definition)^.publicsyms;
        end;
      end;
  end;
  { now walk all recordsymtables }
  while assigned(st) and (s<>'') do
   begin
     { load next field in base }
     i:=pos('.',s);
     if i=0 then
      i:=255;
     base:=Copy(s,1,i-1);
     delete(s,1,i);
     sym:=st^.search(base);
     st:=nil;
     case sym^.typ of
       varsym :
         begin
           inc(Offset,pvarsym(sym)^.address);
           Size:=PVarsym(sym)^.getsize;
           case pvarsym(sym)^.definition^.deftype of
             recorddef :
               st:=precdef(pvarsym(sym)^.definition)^.symtable;
             objectdef :
               st:=pobjectdef(pvarsym(sym)^.definition)^.publicsyms;
           end;
         end;
     end;
   end;
   GetRecordOffsetSize:=(s='');
end;


Function CreateVarInstr(var Instr: TInstruction; const hs:string;operandnum:byte): Boolean;
{ search and sets up the correct fields in the Instr record }
{ for the NON-constant identifier passed to the routine.    }
{ if not found returns FALSE.                               }
var
  sym : psym;
Begin
  CreateVarInstr:=FALSE;
{ are we in a routine ? }
  getsym(hs,false);
  sym:=srsym;
  if sym=nil then
   exit;
  case sym^.typ of
    varsym :
      begin
        { we always assume in asm statements that     }
        { that the variable is valid.                 }
        pvarsym(sym)^.is_valid:=1;
        inc(pvarsym(sym)^.refs);
        case pvarsym(sym)^.owner^.symtabletype of
          unitsymtable,
          globalsymtable,
          staticsymtable :
            instr.operands[operandnum].ref.symbol:=newasmsymbol(pvarsym(sym)^.mangledname);
          parasymtable :
            begin
              instr.operands[operandnum].ref.base:=procinfo.framepointer;
              instr.operands[operandnum].ref.offset:=pvarsym(sym)^.address;
              instr.operands[operandnum].ref.offsetfixup:=aktprocsym^.definition^.parast^.address_fixup;
              instr.operands[operandnum].ref.options:=ref_parafixup;
            end;
          localsymtable :
            begin
              if (pvarsym(sym)^.var_options and vo_is_external)<>0 then
                instr.operands[operandnum].ref.symbol:=newasmsymbol(pvarsym(sym)^.mangledname)
              else
                begin
                  instr.operands[operandnum].ref.base:=procinfo.framepointer;
                  instr.operands[operandnum].ref.offset:=-(pvarsym(sym)^.address);
                  instr.operands[operandnum].ref.options:=ref_localfixup;
                  instr.operands[operandnum].ref.offsetfixup:=aktprocsym^.definition^.localst^.address_fixup;
                end;
            end;
        end;
        case pvarsym(sym)^.definition^.deftype of
          orddef,
          enumdef,
          floatdef :
            SetOperandSize(instr,operandnum,pvarsym(sym)^.getsize);
          arraydef :
            SetOperandSize(instr,operandnum,parraydef(pvarsym(sym)^.definition)^.elesize)
        end;
        instr.operands[operandnum].hasvar:=true;
        CreateVarInstr:=TRUE;
        Exit;
      end;
    typedconstsym :
      begin
        instr.operands[operandnum].ref.symbol:=newasmsymbol(ptypedconstsym(sym)^.mangledname);
        case ptypedconstsym(sym)^.definition^.deftype of
          orddef,
          enumdef,
          floatdef :
            SetOperandSize(instr,operandnum,ptypedconstsym(sym)^.getsize);
          arraydef :
            SetOperandSize(instr,operandnum,parraydef(ptypedconstsym(sym)^.definition)^.elesize)
        end;
        instr.operands[operandnum].hasvar:=true;
        CreateVarInstr:=TRUE;
        Exit;
      end;
    constsym :
      begin
        if pconstsym(sym)^.consttype in [constint,constchar,constbool] then
         begin
           instr.operands[operandnum].operandtype:=OPR_CONSTANT;
           instr.operands[operandnum].val:=pconstsym(sym)^.value;
           instr.operands[operandnum].hasvar:=true;
           CreateVarInstr:=TRUE;
           Exit;
         end;
      end;
    typesym :
      begin
        if ptypesym(sym)^.definition^.deftype in [recorddef,objectdef] then
         begin
           instr.operands[operandnum].operandtype:=OPR_CONSTANT;
           instr.operands[operandnum].val:=0;
           instr.operands[operandnum].hasvar:=true;
           CreateVarInstr:=TRUE;
           Exit;
         end;
      end;
    procsym :
      begin
        if assigned(pprocsym(sym)^.definition^.nextoverloaded) then
          Message(asmr_w_calling_overload_func);
        instr.operands[operandnum].operandtype:=OPR_SYMBOL;
        instr.operands[operandnum].symbol:=newasmsymbol(pprocsym(sym)^.definition^.mangledname);
        instr.operands[operandnum].hasvar:=true;
        CreateVarInstr:=TRUE;
        Exit;
      end;
    else
      begin
        Message(asmr_e_unsupported_symbol_type);
        exit;
      end;
  end;
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
    SearchLabel:=FALSE;
    if assigned(aktprocsym) then
    Begin
      { Check the local constants }
    if assigned(aktprocsym^.definition) then
    Begin
        if assigned(aktprocsym^.definition^.localst) then
          sym:=aktprocsym^.definition^.localst^.search(s)
      else
       sym:=nil;
      if assigned(sym) then
      Begin
       if (sym^.typ = labelsym) then
       Begin
          hl:=plabelsym(sym)^.number;
          SearchLabel:=TRUE;
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
        SearchLabel:=TRUE;
        exit;
      end;
    end;
  end;


{ looks for internal names of variables and routines }
Function SearchDirectVar(var Instr: TInstruction; const hs:string;operandnum:byte): Boolean;
var
  p : pai_external;
Begin
   SearchDirectVar:=false;
   { search in the list of internals }
   p:=search_assembler_symbol(internals,hs,EXT_ANY);
     if p=nil then
       p:=search_assembler_symbol(externals,hs,EXT_ANY);
   if p<>nil then
     begin
       instr.operands[operandnum].ref.symbol:=p^.sym;
        case p^.exttyp of
           EXT_BYTE   : instr.operands[operandnum].size:=S_B;
           EXT_WORD   : instr.operands[operandnum].size:=S_W;
           EXT_NEAR,EXT_FAR,EXT_PROC,EXT_DWORD,EXT_CODEPTR,EXT_DATAPTR:
           instr.operands[operandnum].size:=S_L;
           EXT_QWORD  : instr.operands[operandnum].size:=S_FL;
           EXT_TBYTE  : instr.operands[operandnum].size:=S_FX;
         else
           { this is in the case where the instruction is LEA }
           { or something like that, in that case size is not }
           { important.                                       }
             instr.operands[operandnum].size:=S_NO;
         end;
       instr.operands[operandnum].hasvar:=true;
       SearchDirectVar:=TRUE;
       Exit;
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
     Message(asmr_e_constant_out_of_bounds);
     { assuming a value of maxvalue }
     value:=maxvalue;
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


  Procedure ConcatConstSymbol(p : paasmoutput;const sym:string;l:longint);
  begin
    p^.concat(new(pai_const_symbol,init_offset(sym,l)));
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

   Procedure ConcatLabel(p: paasmoutput;var l : plabel);
  {*********************************************************************}
  { PROCEDURE ConcatLabel                                               }
  {  Description: This routine either emits a label or a labeled        }
  {  instruction to the linked list of instructions.                    }
  {*********************************************************************}
   begin
     p^.concat(new(pai_label,init(l)))
   end;

   procedure ConcatAlign(p:paasmoutput;l:longint);
  {*********************************************************************}
  { PROCEDURE ConcatPublic                                              }
  {  Description: This routine emits an global   definition to the      }
  {  linked list of instructions.(used by AT&T styled asm)              }
  {*********************************************************************}
   begin
     p^.concat(new(pai_align,init(l)));
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
  Revision 1.12  1999-05-05 22:22:04  peter
    * updated messages

  Revision 1.11  1999/05/02 22:41:57  peter
    * moved section names to systems
    * fixed nasm,intel writer

  Revision 1.10  1999/05/01 13:24:41  peter
    * merged nasm compiler
    * old asm moved to oldasm/

  Revision 1.8  1999/04/26 23:26:19  peter
    * redesigned record offset parsing to support nested records
    * normal compiler uses the redesigned createvarinstr()

  Revision 1.7  1999/04/14 09:07:48  peter
    * asm reader improvements

  Revision 1.6  1999/03/31 13:55:34  peter
    * assembler inlining working for ag386bin

  Revision 1.5  1999/03/26 00:01:17  peter
    * first things for optimizer (compiles but cycle crashes)

  Revision 1.4  1999/03/25 16:55:36  peter
    + unitpath,librarypath,includepath,objectpath directives

  Revision 1.3  1999/03/06 17:24:28  peter
    * rewritten intel parser a lot, especially reference reading
    * size checking added for asm parsers

  Revision 1.2  1999/03/02 02:56:33  peter
    + stabs support for binary writers
    * more fixes and missing updates from the previous commit :(

  Revision 1.1  1999/03/01 15:46:26  peter
    * ag386bin finally make cycles correct
    * prefixes are now also normal opcodes
}
