{
    $Id$
    Copyright (c) 1998-2000 by Carl Eric Codere and Peter Vreman

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

{$i defines.inc}

Interface

Uses
  cutils,cclasses,
  globtype,aasm,cpubase,
  symconst,symbase,symtype,symdef;

Const
  RPNMax = 10;             { I think you only need 4, but just to be safe }
  OpMax  = 25;

  maxoperands = 3;         { Maximum operands for assembler instructions }


{---------------------------------------------------------------------
                       Local Label Management
---------------------------------------------------------------------}

Type
  { Each local label has this structure associated with it }
  TLocalLabel = class(TNamedIndexItem)
    Emitted : boolean;
    constructor Create(const n:string);
    function  Gettasmlabel:tasmlabel;
  private
    lab : tasmlabel;
  end;

  TLocalLabelList = class(TDictionary)
    procedure CheckEmitted;
  end;

var
  LocalLabelList : TLocalLabelList;

function CreateLocalLabel(const s: string; var hl: tasmlabel; emit:boolean):boolean;
Function SearchLabel(const s: string; var hl: tasmlabel;emit:boolean): boolean;


{---------------------------------------------------------------------
                 Instruction management
---------------------------------------------------------------------}

type
  TOprType=(OPR_NONE,OPR_CONSTANT,OPR_SYMBOL,
            OPR_REFERENCE,OPR_REGISTER,OPR_REGLIST);

  TOprRec = record
    case typ:TOprType of
      OPR_NONE   : ();
      OPR_CONSTANT  : (val:longint);
      OPR_SYMBOL    : (symbol:tasmsymbol;symofs:longint);
      OPR_REFERENCE : (ref:treference);
      OPR_REGISTER  : (reg:tregister);
{$ifdef m68k}
      OPR_REGLIST   : (reglist:pregisterlist);
{$else not m68k}
      OPR_REGLIST   : ();
{$endif m68k}
  end;

  TOperand = class
    size   : topsize;
    hastype,          { if the operand has typecasted variable }
    hasvar : boolean; { if the operand is loaded with a variable }
    opr    : TOprRec;
    constructor create;
    destructor  destroy;override;
    Procedure BuildOperand;virtual;
    Procedure SetSize(_size:longint;force:boolean);
    Procedure SetCorrectSize(opcode:tasmop);virtual;
    Function  SetupResult:boolean;virtual;
    Function  SetupSelf:boolean;
    Function  SetupOldEBP:boolean;
    Function  SetupVar(const s:string;GetOffset : boolean): Boolean;
    Function  SetupDirectVar(const hs:string): Boolean;
    Procedure InitRef;
  end;

  TInstruction = class
    opcode    : tasmop;
    opsize    : topsize;
    condition : tasmcond;
    ops       : byte;
    labeled   : boolean;
    operands  : array[1..maxoperands] of toperand;
    constructor create;
    destructor  destroy;override;
    Procedure InitOperands;virtual;
    Procedure BuildOpcode;virtual;
    procedure ConcatInstruction(p:TAAsmoutput);virtual;
    Procedure Swatoperands;
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

  TExprParse = class
    public
     Constructor create;
     Destructor Destroy;override;
     Function Evaluate(Expr:  String): longint;
     Function Priority(_Operator: Char): longint;
    private
     RPNStack   : Array[1..RPNMax] of longint;        { Stack For RPN calculator }
     RPNTop     : longint;
     OpStack    : Array[1..OpMax] of TExprOperator;    { Operator stack For conversion }
     OpTop      : longint;
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
Function PadZero(Var s: String; n: byte): Boolean;
Function EscapeToPascal(const s:string): string;

{---------------------------------------------------------------------
                     Symbol helper routines
---------------------------------------------------------------------}

procedure AsmSearchSym(const s:string;var srsym:tsym;var srsymtable:tsymtable);
Function GetRecordOffsetSize(s:string;Var Offset: longint;var Size:longint):boolean;
Function SearchType(const hs:string): Boolean;
Function SearchRecordType(const s:string): boolean;
Function SearchIConstant(const s:string; var l:longint): boolean;


{---------------------------------------------------------------------
                  Instruction generation routines
---------------------------------------------------------------------}

  Procedure ConcatPasString(p : TAAsmoutput;s:string);
  Procedure ConcatDirect(p : TAAsmoutput;s:string);
  Procedure ConcatLabel(p: TAAsmoutput;var l : tasmlabel);
  Procedure ConcatConstant(p : TAAsmoutput;value: longint; maxvalue: longint);
  Procedure ConcatConstSymbol(p : TAAsmoutput;const sym:string;l:longint);
  Procedure ConcatRealConstant(p : TAAsmoutput;value: bestreal; real_typ : tfloattype);
  Procedure ConcatString(p : TAAsmoutput;s:string);
  procedure ConcatAlign(p:TAAsmoutput;l:longint);
  Procedure ConcatPublic(p:TAAsmoutput;const s : string);
  Procedure ConcatLocal(p:TAAsmoutput;const s : string);
  Procedure ConcatGlobalBss(const s : string;size : longint);
  Procedure ConcatLocalBss(const s : string;size : longint);


Implementation

uses
{$ifdef delphi}
  sysutils,
{$else}
  strings,
{$endif}
  types,systems,verbose,globals,
  symsym,symtable,cpuasm,
  cgbase;

{*************************************************************************
                              TExprParse
*************************************************************************}

Constructor TExprParse.create;
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
   Message(asmr_e_expr_illegal);
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
   Message(asmr_e_expr_illegal);
end;


Procedure TExprParse.RPNCalc(Token : String15; prefix:boolean);                       { RPN Calculator }
Var
  Temp  : longint;
  n1,n2 : longint;
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
         begin
           n1:=RPNPop;
           n2:=RPNPop;
           RPNPush(n2 - n1);
         end;
      end;
    '*' : RPNPush(RPNPop * RPNPop);
    '&' :
      begin
        n1:=RPNPop;
        n2:=RPNPop;
        RPNPush(n2 and n1);
      end;
    '|' :
      begin
        n1:=RPNPop;
        n2:=RPNPop;
        RPNPush(n2 or n1);
      end;
    '~' : RPNPush(NOT RPNPop);
    '<' :
      begin
        n1:=RPNPop;
        n2:=RPNPop;
        RPNPush(n2 SHL n1);
      end;
    '>' :
      begin
        n1:=RPNPop;
        n2:=RPNPop;
        RPNPush(n2 SHR n1);
      end;
    '%' :
      begin
        Temp:=RPNPop;
        if Temp <> 0 then
         RPNPush(RPNPop mod Temp)
        else
         begin
           Message(asmr_e_expr_zero_divide);
           { push 1 for error recovery }
           RPNPush(1);
         end;
      end;
    '^' : RPNPush(RPNPop XOR RPNPop);
    '/' :
      begin
        Temp:=RPNPop;
        if Temp <> 0 then
         RPNPush(RPNPop div Temp)
        else
         begin
           Message(asmr_e_expr_zero_divide);
           { push 1 for error recovery }
           RPNPush(1);
         end;
      end;
   end
  else
   begin
     { Convert String to number and add to stack }
      Val(Token, Temp, LocalError);
     if LocalError = 0 then
      RPNPush(Temp)
     else
      begin
        Message(asmr_e_expr_illegal);
        { push 1 for error recovery }
        RPNPush(1);
      end;
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
   Message(asmr_e_expr_illegal);
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
   Message(asmr_e_expr_illegal);
end;


Function TExprParse.Priority(_Operator : Char) : longint;
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
      Message(asmr_e_expr_illegal);
  end;
end;


Function TExprParse.Evaluate(Expr : String):longint;
Var
  I     : LongInt;
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
      Message(asmr_e_expr_illegal);  { Handle bad input error }
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


Destructor TExprParse.Destroy;
Begin
end;


Function CalculateExpression(const expression: string): longint;
var
  expr: TExprParse;
Begin
  expr:=TExprParse.create;
  CalculateExpression:=expr.Evaluate(expression);
  expr.Free;
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
                                   TOperand
****************************************************************************}

constructor TOperand.Create;
begin
  size:=S_NO;
  hastype:=false;
  hasvar:=false;
  FillChar(Opr,sizeof(Opr),0);
end;


destructor TOperand.destroy;
begin
end;


Procedure TOperand.SetCorrectSize(opcode:tasmop);
begin
end;

Procedure TOperand.SetSize(_size:longint;force:boolean);
begin
  if force or
     ((size = S_NO) and (_size<=extended_size)) then
   Begin
     case _size of
      1 : size:=S_B;
      2 : size:=S_W{ could be S_IS};
      4 : size:=S_L{ could be S_IL or S_FS};
      8 : size:=S_IQ{ could be S_D or S_FL};
      extended_size : size:=S_FX;
     end;
   end;
end;


Function TOperand.SetupResult:boolean;
Begin
  SetupResult:=false;
  { replace by correct offset. }
  if (not is_void(aktprocdef.rettype.def)) then
   begin
     if (m_tp7 in aktmodeswitches) and
        ret_in_acc(aktprocdef.rettype.def) then
       begin
         Message(asmr_e_cannot_use_RESULT_here);
         exit;
       end;
     opr.ref.offset:=procinfo^.return_offset;
     opr.ref.base:= procinfo^.framepointer;
     opr.ref.options:=ref_parafixup;
     { always assume that the result is valid. }
     tfuncretsym(aktprocdef.funcretsym).funcretstate:=vs_assigned;
     { increase reference count, this is also used to check
       if the result variable is actually used or not }
     inc(tfuncretsym(aktprocdef.funcretsym).refcount);
     SetupResult:=true;
   end
  else
   Message(asmr_e_void_function);
end;


Function TOperand.SetupSelf:boolean;
Begin
  SetupSelf:=false;
  if assigned(procinfo^._class) then
   Begin
     opr.typ:=OPR_REFERENCE;
     opr.ref.offset:=procinfo^.selfpointer_offset;
     opr.ref.base:=procinfo^.framepointer;
     opr.ref.options:=ref_selffixup;
     SetupSelf:=true;
   end
  else
   Message(asmr_e_cannot_use_SELF_outside_a_method);
end;


Function TOperand.SetupOldEBP:boolean;
Begin
  SetupOldEBP:=false;
  if lexlevel>normal_function_level then
   Begin
     opr.typ:=OPR_REFERENCE;
     opr.ref.offset:=procinfo^.framepointer_offset;
     opr.ref.base:=procinfo^.framepointer;
     opr.ref.options:=ref_parafixup;
     SetupOldEBP:=true;
   end
  else
   Message(asmr_e_cannot_use_OLDEBP_outside_nested_procedure);
end;


Function TOperand.SetupVar(const s:string;GetOffset : boolean): Boolean;
{ search and sets up the correct fields in the Instr record }
{ for the NON-constant identifier passed to the routine.    }
{ if not found returns FALSE.                               }
var
  sym : tsym;
  srsymtable : tsymtable;
  harrdef : tarraydef;
  l : longint;
Begin
  SetupVar:=false;
  asmsearchsym(s,sym,srsymtable);
  if sym = nil then
   exit;
  case sym.typ of
    varsym :
      begin
        { we always assume in asm statements that     }
        { that the variable is valid.                 }
        tvarsym(sym).varstate:=vs_used;
        inc(tvarsym(sym).refs);
        case tvarsym(sym).owner.symtabletype of
          objectsymtable :
            begin
              { this is not allowed, because we don't know if the self
                register is still free, and loading it first is also
                not possible, because this could break code }
              { Be TP/Delphi compatible in Delphi or TP modes }
              if (m_tp7 in aktmodeswitches) or
                 (m_delphi in aktmodeswitches) then
                begin
                  opr.typ:=OPR_CONSTANT;
                  opr.val:=tvarsym(sym).address;
                end
              { I do not agree here people using method vars should ensure
                that %esi is valid there }
              else
                begin
                  opr.ref.base:=self_pointer;
                  opr.ref.offset:=tvarsym(sym).address;
                end;
              hasvar:=true;
              SetupVar:=true;
              Exit;
            end;
          globalsymtable,
          staticsymtable :
            opr.ref.symbol:=newasmsymbol(tvarsym(sym).mangledname);
          parasymtable :
            begin
              { if we only want the offset we don't have to care
                the base will be zeroed after ! }
              if (lexlevel=tvarsym(sym).owner.symtablelevel) or
              { this below is wrong because there are two parast
                for global functions one of interface the second of
                implementation
              if (tvarsym(sym).owner=procinfo^.def.parast) or }
                GetOffset then
                begin
                  opr.ref.base:=procinfo^.framepointer;
                end
              else
                begin
                  if (procinfo^.framepointer=stack_pointer) and
                     assigned(procinfo^.parent) and
                     (lexlevel=tvarsym(sym).owner.symtablelevel+1) and
                     { same problem as above !!
                     (procinfo^.parent^.sym.definition.parast=tvarsym(sym).owner) and }
                     (lexlevel>normal_function_level) then
                    opr.ref.base:=procinfo^.parent^.framepointer
                  else
                    message1(asmr_e_local_para_unreachable,s);
                end;
              opr.ref.offset:=tvarsym(sym).address;
              if (lexlevel=tvarsym(sym).owner.symtablelevel) then
                begin
                  opr.ref.offsetfixup:=aktprocdef.parast.address_fixup;
                  opr.ref.options:=ref_parafixup;
                end
              else
                begin
                  opr.ref.offsetfixup:=0;
                  opr.ref.options:=ref_none;
                end;
              if (tvarsym(sym).varspez=vs_var) or
                 ((tvarsym(sym).varspez=vs_const) and
                 push_addr_param(tvarsym(sym).vartype.def)) then
                SetSize(target_info.size_of_pointer,false);
            end;
          localsymtable :
            begin
              if (vo_is_external in tvarsym(sym).varoptions) then
                opr.ref.symbol:=newasmsymbol(tvarsym(sym).mangledname)
              else
                begin
                  { if we only want the offset we don't have to care
                    the base will be zeroed after ! }
                  if (lexlevel=tvarsym(sym).owner.symtablelevel) or
                  {if (tvarsym(sym).owner=procinfo^.def.localst) or}
                    GetOffset then
                    opr.ref.base:=procinfo^.framepointer
                  else
                    begin
                      if (procinfo^.framepointer=stack_pointer) and
                         assigned(procinfo^.parent) and
                         (lexlevel=tvarsym(sym).owner.symtablelevel+1) and
                         {(procinfo^.parent^.sym.definition.localst=tvarsym(sym).owner) and}
                         (lexlevel>normal_function_level) then
                        opr.ref.base:=procinfo^.parent^.framepointer
                      else
                        message1(asmr_e_local_para_unreachable,s);
                    end;
                  opr.ref.offset:=-(tvarsym(sym).address);
                  if (lexlevel=tvarsym(sym).owner.symtablelevel) then
                    begin
                      opr.ref.offsetfixup:=aktprocdef.localst.address_fixup;
                      opr.ref.options:=ref_localfixup;
                    end
                  else
                    begin
                      opr.ref.offsetfixup:=0;
                      opr.ref.options:=ref_none;
                    end;
                end;
              if (tvarsym(sym).varspez in [vs_var,vs_out]) or
                 ((tvarsym(sym).varspez=vs_const) and
                  push_addr_param(tvarsym(sym).vartype.def)) then
                SetSize(target_info.size_of_pointer,false);
            end;
        end;
        case tvarsym(sym).vartype.def.deftype of
          orddef,
          enumdef,
          pointerdef,
          floatdef :
            SetSize(tvarsym(sym).getsize,false);
          arraydef :
            begin
              { for arrays try to get the element size, take care of
                multiple indexes }
              harrdef:=tarraydef(tvarsym(sym).vartype.def);
              while assigned(harrdef.elementtype.def) and
                    (harrdef.elementtype.def.deftype=arraydef) do
               harrdef:=tarraydef(harrdef.elementtype.def);
              SetSize(harrdef.elesize,false);
            end;
        end;
        hasvar:=true;
        SetupVar:=true;
        Exit;
      end;
    typedconstsym :
      begin
        opr.ref.symbol:=newasmsymbol(ttypedconstsym(sym).mangledname);
        case ttypedconstsym(sym).typedconsttype.def.deftype of
          orddef,
          enumdef,
          pointerdef,
          floatdef :
            SetSize(ttypedconstsym(sym).getsize,false);
          arraydef :
            begin
              { for arrays try to get the element size, take care of
                multiple indexes }
              harrdef:=tarraydef(ttypedconstsym(sym).typedconsttype.def);
              while assigned(harrdef.elementtype.def) and
                    (harrdef.elementtype.def.deftype=arraydef) do
               harrdef:=tarraydef(harrdef.elementtype.def);
              SetSize(harrdef.elesize,false);
            end;
        end;
        hasvar:=true;
        SetupVar:=true;
        Exit;
      end;
    constsym :
      begin
        if tconstsym(sym).consttyp in [constint,constchar,constbool] then
         begin
           opr.typ:=OPR_CONSTANT;
           opr.val:=tconstsym(sym).valueord;
           SetupVar:=true;
           Exit;
         end;
      end;
    typesym :
      begin
        if ttypesym(sym).restype.def.deftype in [recorddef,objectdef] then
         begin
           opr.typ:=OPR_CONSTANT;
           opr.val:=0;
           SetupVar:=TRUE;
           Exit;
         end;
      end;
    procsym :
      begin
        if assigned(tprocsym(sym).defs^.next) then
          Message(asmr_w_calling_overload_func);
        l:=opr.ref.offset;
        opr.typ:=OPR_SYMBOL;
        opr.symbol:=newasmsymbol(tprocsym(sym).defs^.def.mangledname);
        opr.symofs:=l;
        hasvar:=true;
        SetupVar:=TRUE;
        Exit;
      end;
    else
      begin
        Message(asmr_e_unsupported_symbol_type);
        exit;
      end;
  end;
end;


{ looks for internal names of variables and routines }
Function TOperand.SetupDirectVar(const hs:string): Boolean;
var
  p : tasmsymbol;
begin
  SetupDirectVar:=false;
  p:=getasmsymbol(hs);
  if assigned(p) then
   begin
     opr.ref.symbol:=p;
     hasvar:=true;
     SetupDirectVar:=true;
   end;
end;


procedure TOperand.InitRef;
{*********************************************************************}
{  Description: This routine first check if the opcode is of     }
{  type OPR_NONE, or OPR_REFERENCE , if not it gives out an error.    }
{  If the operandtype = OPR_NONE or <> OPR_REFERENCE then it sets up  }
{  the operand type to OPR_REFERENCE, as well as setting up the ref   }
{  to point to the default segment.                                   }
{*********************************************************************}
Begin
  case opr.typ of
    OPR_REFERENCE:
      exit;
    OPR_NONE: ;
    else
      Message(asmr_e_invalid_operand_type);
  end;
  opr.typ := OPR_REFERENCE;
  reset_reference(opr.ref);
end;


procedure TOperand.BuildOperand;
begin
  abstract;
end;


{****************************************************************************
                                 TInstruction
****************************************************************************}

constructor TInstruction.create;
Begin
  Opcode:=A_NONE;
  Opsize:=S_NO;
  Condition:=C_NONE;
  Ops:=0;
  InitOperands;
  Labeled:=false;
end;


destructor TInstruction.destroy;
var
  i : longint;
Begin
  for i:=1 to 3 do
   Operands[i].free;
end;


procedure TInstruction.InitOperands;
var
  i : longint;
begin
  for i:=1 to 3 do
   Operands[i].create;
end;


Procedure TInstruction.Swatoperands;
Var
  p : toperand;
Begin
  case Ops of
   2 :
    begin
      p:=Operands[1];
      Operands[1]:=Operands[2];
      Operands[2]:=p;
    end;
   3 :
    begin
      p:=Operands[1];
      Operands[1]:=Operands[3];
      Operands[3]:=p;
    end;
  end;
end;


procedure TInstruction.ConcatInstruction(p:TAAsmOutput);
begin
  abstract;
end;


procedure TInstruction.BuildOpcode;
begin
  abstract;
end;


{***************************************************************************
                                 TLocalLabel
***************************************************************************}

constructor TLocalLabel.create(const n:string);
begin
  inherited CreateName(n);
  lab:=nil;
  emitted:=false;
end;


function TLocalLabel.Gettasmlabel:tasmlabel;
begin
  if not assigned(lab) then
   begin
     getlabel(lab);
     { this label is forced to be used so it's always written }
     inc(lab.refs);
   end;
  Gettasmlabel:=lab;
end;


{***************************************************************************
                             TLocalLabelList
***************************************************************************}

procedure LocalLabelEmitted(p:tnamedindexitem);
begin
  if not TLocalLabel(p).emitted  then
   Message1(asmr_e_unknown_label_identifier,p.name);
end;

procedure TLocalLabelList.CheckEmitted;
begin
  ForEach_Static({$ifdef FPCPROCVAR}@{$endif}LocalLabelEmitted)
end;


function CreateLocalLabel(const s: string; var hl: tasmlabel; emit:boolean):boolean;
var
  lab : TLocalLabel;
Begin
  CreateLocalLabel:=true;
{ Check if it already is defined }
  lab:=TLocalLabel(LocalLabellist.Search(s));
  if not assigned(lab) then
   begin
     lab:=TLocalLabel.Create(s);
     LocalLabellist.Insert(lab);
   end;
{ set emitted flag and check for dup syms }
  if emit then
   begin
     if lab.Emitted then
      begin
        Message1(asmr_e_dup_local_sym,lab.Name);
        CreateLocalLabel:=false;
      end;
     lab.Emitted:=true;
   end;
  hl:=lab.Gettasmlabel;
end;


{****************************************************************************
                      Symbol table helper routines
****************************************************************************}

procedure AsmSearchSym(const s:string;var srsym:tsym;var srsymtable:tsymtable);
var
  i : integer;
begin
  i:=pos('.',s);
  { allow unit.identifier }
  if i>0 then
   begin
     searchsym(Copy(s,1,i-1),srsym,srsymtable);
     if assigned(srsym) then
      begin
        if (srsym.typ=unitsym) and
           (srsym.owner.unitid=0) then
         srsym:=searchsymonlyin(tunitsym(srsym).unitsymtable,Copy(s,i+1,255))
        else
         srsym:=nil;
      end;
   end
  else
   searchsym(s,srsym,srsymtable);
end;


Function SearchType(const hs:string): Boolean;
var
  srsym : tsym;
  srsymtable : tsymtable;
begin
  asmsearchsym(hs,srsym,srsymtable);
  SearchType:=assigned(srsym) and
             (srsym.typ=typesym);
end;



Function SearchRecordType(const s:string): boolean;
var
  srsym : tsym;
  srsymtable : tsymtable;
Begin
  SearchRecordType:=false;
{ Check the constants in symtable }
  asmsearchsym(s,srsym,srsymtable);
  if srsym <> nil then
   Begin
     case srsym.typ of
       typesym :
         begin
           if ttypesym(srsym).restype.def.deftype in [recorddef,objectdef] then
            begin
              SearchRecordType:=true;
              exit;
            end;
         end;
     end;
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
  srsym : tsym;
  srsymtable : tsymtable;
Begin
  SearchIConstant:=false;
{ check for TRUE or FALSE reserved words first }
  if s = 'TRUE' then
   Begin
     SearchIConstant:=TRUE;
     l:=1;
     exit;
   end;
  if s = 'FALSE' then
   Begin
     SearchIConstant:=TRUE;
     l:=0;
     exit;
   end;
{ Check the constants in symtable }
  asmsearchsym(s,srsym,srsymtable);
  if srsym <> nil then
   Begin
     case srsym.typ of
       constsym :
         begin
           if (tconstsym(srsym).consttyp in [constord,constint,constchar,constbool]) then
            Begin
              l:=tconstsym(srsym).valueord;
              SearchIConstant:=TRUE;
              exit;
            end;
         end;
       enumsym:
         Begin
           l:=tenumsym(srsym).value;
           SearchIConstant:=TRUE;
           exit;
         end;
     end;
   end;
end;


Function GetRecordOffsetSize(s:string;Var Offset: longint;var Size:longint):boolean;
{ search and returns the offset and size of records/objects of the base }
{ with field name setup in field.                              }
{ returns FALSE if not found.                                  }
{ used when base is a variable or a typed constant name.       }
var
  st   : tsymtable;
  harrdef : tarraydef;
  sym  : tsym;
  srsymtable : tsymtable;
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
  if base='SELF' then
   st:=procinfo^._class.symtable
  else
   begin
     asmsearchsym(base,sym,srsymtable);
     st:=nil;
     { we can start with a var,type,typedconst }
     case sym.typ of
       varsym :
         begin
           case tvarsym(sym).vartype.def.deftype of
             recorddef :
               st:=trecorddef(tvarsym(sym).vartype.def).symtable;
             objectdef :
               st:=tobjectdef(tvarsym(sym).vartype.def).symtable;
           end;
         end;
       typesym :
         begin
           case ttypesym(sym).restype.def.deftype of
             recorddef :
               st:=trecorddef(ttypesym(sym).restype.def).symtable;
             objectdef :
               st:=tobjectdef(ttypesym(sym).restype.def).symtable;
           end;
         end;
       typedconstsym :
         begin
           case ttypedconstsym(sym).typedconsttype.def.deftype of
             recorddef :
               st:=trecorddef(ttypedconstsym(sym).typedconsttype.def).symtable;
             objectdef :
               st:=tobjectdef(ttypedconstsym(sym).typedconsttype.def).symtable;
           end;
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
     if st.symtabletype=objectsymtable then
       sym:=search_class_member(tobjectdef(st.defowner),base)
     else
       sym:=tsym(st.search(base));
     if not assigned(sym) then
      begin
        GetRecordOffsetSize:=false;
        exit;
      end;
     st:=nil;
     case sym.typ of
       varsym :
         begin
           inc(Offset,tvarsym(sym).address);
           Size:=tvarsym(sym).getsize;
           case tvarsym(sym).vartype.def.deftype of
             arraydef :
               begin
                 { for arrays try to get the element size, take care of
                   multiple indexes }
                 harrdef:=tarraydef(tvarsym(sym).vartype.def);
                 while assigned(harrdef.elementtype.def) and
                       (harrdef.elementtype.def.deftype=arraydef) do
                  harrdef:=tarraydef(harrdef.elementtype.def);
                 size:=harrdef.elesize;
               end;
             recorddef :
               st:=trecorddef(tvarsym(sym).vartype.def).symtable;
             objectdef :
               st:=tobjectdef(tvarsym(sym).vartype.def).symtable;
           end;
         end;
     end;
   end;
   GetRecordOffsetSize:=(s='');
end;


Function SearchLabel(const s: string; var hl: tasmlabel;emit:boolean): boolean;
var
  sym : tsym;
  srsymtable : tsymtable;
  hs  : string;
Begin
  hl:=nil;
  SearchLabel:=false;
{ Check for pascal labels, which are case insensetive }
  hs:=upper(s);
  asmsearchsym(hs,sym,srsymtable);
  if sym=nil then
   exit;
  case sym.typ of
    labelsym :
      begin
        hl:=tlabelsym(sym).lab;
        if emit then
         tlabelsym(sym).defined:=true
        else
         tlabelsym(sym).used:=true;
        SearchLabel:=true;
        exit;
      end;
  end;
end;


 {*************************************************************************}
 {                   Instruction Generation Utilities                      }
 {*************************************************************************}


   Procedure ConcatString(p : TAAsmoutput;s:string);
  {*********************************************************************}
  { PROCEDURE ConcatString(s:string);                                   }
  {  Description: This routine adds the character chain pointed to in   }
  {  s to the instruction linked list.                                  }
  {*********************************************************************}
  Var
   pc: PChar;
  Begin
     getmem(pc,length(s)+1);
     p.concat(Tai_string.Create_length_pchar(strpcopy(pc,s),length(s)));
  end;

  Procedure ConcatPasString(p : TAAsmoutput;s:string);
  {*********************************************************************}
  { PROCEDURE ConcatPasString(s:string);                                }
  {  Description: This routine adds the character chain pointed to in   }
  {  s to the instruction linked list, contrary to ConcatString it      }
  {  uses a pascal style string, so it conserves null characters.       }
  {*********************************************************************}
  Begin
     p.concat(Tai_string.Create(s));
  end;

  Procedure ConcatDirect(p : TAAsmoutput;s:string);
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
     p.concat(Tai_direct.Create(strpcopy(pc,s)));
  end;




Procedure ConcatConstant(p: TAAsmoutput; value: longint; maxvalue: longint);
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
  if (maxvalue <> longint($ffffffff)) and (value > maxvalue) then
   Begin
     Message(asmr_e_constant_out_of_bounds);
     { assuming a value of maxvalue }
     value:=maxvalue;
   end;
  if maxvalue = $ff then
   p.concat(Tai_const.Create_8bit(byte(value)))
  else
   if maxvalue = $ffff then
    p.concat(Tai_const.Create_16bit(word(value)))
  else
   if maxvalue = longint($ffffffff) then
    p.concat(Tai_const.Create_32bit(longint(value)));
end;


  Procedure ConcatConstSymbol(p : TAAsmoutput;const sym:string;l:longint);
  begin
    p.concat(Tai_const_symbol.Createname_offset(sym,l));
  end;


  Procedure ConcatRealConstant(p : TAAsmoutput;value: bestreal; real_typ : tfloattype);
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
          s32real : p.concat(Tai_real_32bit.Create(value));
          s64real : p.concat(Tai_real_64bit.Create(value));
          s80real : p.concat(Tai_real_80bit.Create(value));
          s64comp : p.concat(Tai_comp_64bit.Create(value));
       end;
    end;

   Procedure ConcatLabel(p: TAAsmoutput;var l : tasmlabel);
  {*********************************************************************}
  { PROCEDURE ConcatLabel                                               }
  {  Description: This routine either emits a label or a labeled        }
  {  instruction to the linked list of instructions.                    }
  {*********************************************************************}
   begin
     p.concat(Tai_label.Create(l));
   end;

   procedure ConcatAlign(p:TAAsmoutput;l:longint);
  {*********************************************************************}
  { PROCEDURE ConcatPublic                                              }
  {  Description: This routine emits an global   definition to the      }
  {  linked list of instructions.(used by AT&T styled asm)              }
  {*********************************************************************}
   begin
     p.concat(Tai_align.Create(l));
   end;

   procedure ConcatPublic(p:TAAsmoutput;const s : string);
  {*********************************************************************}
  { PROCEDURE ConcatPublic                                              }
  {  Description: This routine emits an global   definition to the      }
  {  linked list of instructions.(used by AT&T styled asm)              }
  {*********************************************************************}
   begin
       p.concat(Tai_symbol.Createname_global(s,0));
   end;

   procedure ConcatLocal(p:TAAsmoutput;const s : string);
  {*********************************************************************}
  { PROCEDURE ConcatLocal                                               }
  {  Description: This routine emits an local    definition to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       p.concat(Tai_symbol.Createname(s,0));
   end;

  Procedure ConcatGlobalBss(const s : string;size : longint);
  {*********************************************************************}
  { PROCEDURE ConcatGlobalBss                                           }
  {  Description: This routine emits an global  datablock   to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       bssSegment.concat(Tai_datablock.Create_global(s,size));
   end;

  Procedure ConcatLocalBss(const s : string;size : longint);
  {*********************************************************************}
  { PROCEDURE ConcatLocalBss                                            }
  {  Description: This routine emits a local datablcok      to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       bssSegment.concat(Tai_datablock.Create(s,size));
   end;

end.
{
  $Log$
  Revision 1.26  2002-01-24 18:25:50  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.25  2001/11/02 22:58:06  peter
    * procsym definition rewrite

  Revision 1.24  2001/09/02 21:18:28  peter
    * split constsym.value in valueord,valueordptr,valueptr. The valueordptr
      is used for holding target platform pointer values. As those can be
      bigger than the source platform.

  Revision 1.23  2001/08/26 13:36:48  florian
    * some cg reorganisation
    * some PPC updates

  Revision 1.22  2001/08/12 17:57:07  peter
    * under development flag for targets

  Revision 1.21  2001/08/06 21:40:48  peter
    * funcret moved from tprocinfo to tprocdef

  Revision 1.20  2001/04/18 22:01:58  peter
    * registration of targets and assemblers

  Revision 1.19  2001/04/13 20:06:05  peter
    * allow unit.identifier in asm readers

  Revision 1.18  2001/04/13 01:22:13  peter
    * symtable change to classes
    * range check generation and errors fixed, make cycle DEBUG=1 works
    * memory leaks fixed

  Revision 1.17  2001/04/02 21:20:34  peter
    * resulttype rewrite

  Revision 1.16  2001/03/11 22:58:50  peter
    * getsym redesign, removed the globals srsym,srsymtable

  Revision 1.15  2001/02/26 19:44:54  peter
    * merged generic m68k updates from fixes branch

  Revision 1.14  2000/12/25 00:07:28  peter
    + new tlinkedlist class (merge of old tstringqueue,tcontainer and
      tlinkedlist objects)

  Revision 1.13  2000/12/07 17:19:43  jonas
    * new constant handling: from now on, hex constants >$7fffffff are
      parsed as unsigned constants (otherwise, $80000000 got sign extended
      and became $ffffffff80000000), all constants in the longint range
      become longints, all constants >$7fffffff and <=cardinal($ffffffff)
      are cardinals and the rest are int64's.
    * added lots of longint typecast to prevent range check errors in the
      compiler and rtl
    * type casts of symbolic ordinal constants are now preserved
    * fixed bug where the original resulttype wasn't restored correctly
      after doing a 64bit rangecheck

  Revision 1.12  2000/11/29 00:30:38  florian
    * unused units removed from uses clause
    * some changes for widestrings

  Revision 1.11  2000/11/06 22:30:30  peter
    * more fixes

  Revision 1.10  2000/11/04 14:25:21  florian
    + merged Attila's changes for interfaces, not tested yet

  Revision 1.9  2000/10/31 22:30:13  peter
    * merged asm result patch part 2

  Revision 1.8  2000/10/31 22:02:51  peter
    * symtable splitted, no real code changes

  Revision 1.7  2000/10/08 10:26:33  peter
    * merged @result fix from Pierre

  Revision 1.6  2000/09/24 21:19:51  peter
    * delphi compile fixes

  Revision 1.5  2000/09/24 15:06:26  peter
    * use defines.inc

  Revision 1.4  2000/08/27 16:11:52  peter
    * moved some util functions from globals,cobjects to cutils
    * splitted files into finput,fmodule

  Revision 1.3  2000/08/06 10:42:29  peter
    * merged patches name generation in lib and asm constant eval

  Revision 1.2  2000/07/13 11:32:48  michael
  + removed logs

}
