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
Interface

Uses
  strings,
  cobjects,
  globtype,systems,verbose,globals,files,
  symconst,symtable,aasm,cpubase,cpuasm
{$ifdef NEWCG}
  ,cgbase
{$else}
  ,hcodegen
{$endif}
  ;

Const
  RPNMax = 10;             { I think you only need 4, but just to be safe }
  OpMax  = 25;

  maxoperands = 3;         { Maximum operands for assembler instructions }


{---------------------------------------------------------------------
                       Local Label Management
---------------------------------------------------------------------}

Type
  { Each local label has this structure associated with it }
  PLocalLabel = ^TLocalLabel;
  TLocalLabel = object(TNamedIndexObject)
    Emitted : boolean;
    constructor Init(const n:string);
    function  Getpasmlabel:pasmlabel;
  private
    lab : pasmlabel;
  end;

  PLocalLabelList = ^TLocalLabelList;
  TLocalLabelList = Object(TDictionary)
    procedure CheckEmitted;
  end;

var
  LocalLabelList : PLocalLabelList;

function CreateLocalLabel(const s: string; var hl: pasmlabel; emit:boolean):boolean;
Function SearchLabel(const s: string; var hl: pasmlabel;emit:boolean): boolean;


{---------------------------------------------------------------------
                 Instruction management
---------------------------------------------------------------------}

type
  TOprType=(OPR_NONE,OPR_CONSTANT,OPR_SYMBOL,OPR_REFERENCE,OPR_REGISTER);

  TOprRec = record
    case typ:TOprType of
      OPR_NONE   : ();
      OPR_CONSTANT  : (val:longint);
      OPR_SYMBOL    : (symbol:PAsmSymbol;symofs:longint);
      OPR_REFERENCE : (ref:treference);
      OPR_REGISTER  : (reg:tregister);
  end;

  POperand = ^TOperand;
  TOperand = object
    size   : topsize;
    hasvar : boolean; { if the operand is loaded with a variable }
    opr    : TOprRec;
    constructor init;
    destructor  done;virtual;
    Procedure BuildOperand;virtual;
    Procedure SetSize(_size:longint);
    Function  SetupResult:boolean;
    Function  SetupSelf:boolean;
    Function  SetupOldEBP:boolean;
    Function  SetupVar(const hs:string;GetOffset : boolean): Boolean;
    Function  SetupDirectVar(const hs:string): Boolean;
    Procedure InitRef;
  end;

  PInstruction = ^TInstruction;
  TInstruction = object
    opcode    : tasmop;
    opsize    : topsize;
    condition : tasmcond;
    ops       : byte;
    operands  : array[1..maxoperands] of POperand;
    constructor init;
    destructor  done;virtual;
    Procedure InitOperands;virtual;
    Procedure BuildOpcode;virtual;
    procedure ConcatInstruction(p:PAasmoutput);virtual;
    Procedure SwapOperands;
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
Function PadZero(Var s: String; n: byte): Boolean;
Function EscapeToPascal(const s:string): string;

{---------------------------------------------------------------------
                     Symbol helper routines
---------------------------------------------------------------------}

Function GetRecordOffsetSize(s:string;Var Offset: longint;var Size:longint):boolean;
Function SearchRecordType(const s:string): boolean;
Function SearchIConstant(const s:string; var l:longint): boolean;


{---------------------------------------------------------------------
                  Instruction generation routines
---------------------------------------------------------------------}

  Procedure ConcatPasString(p : paasmoutput;s:string);
  Procedure ConcatDirect(p : paasmoutput;s:string);
  Procedure ConcatLabel(p: paasmoutput;var l : pasmlabel);
  Procedure ConcatConstant(p : paasmoutput;value: longint; maxvalue: longint);
  Procedure ConcatConstSymbol(p : paasmoutput;const sym:string;l:longint);
  Procedure ConcatRealConstant(p : paasmoutput;value: bestreal; real_typ : tfloattype);
  Procedure ConcatString(p : paasmoutput;s:string);
  procedure ConcatAlign(p:paasmoutput;l:longint);
  Procedure ConcatPublic(p:paasmoutput;const s : string);
  Procedure ConcatLocal(p:paasmoutput;const s : string);
  Procedure ConcatGlobalBss(const s : string;size : longint);
  Procedure ConcatLocalBss(const s : string;size : longint);


Implementation


{*************************************************************************
                              TExprParse
*************************************************************************}

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
                                   TOperand
****************************************************************************}

constructor TOperand.init;
begin
  size:=S_NO;
  hasvar:=false;
  FillChar(Opr,sizeof(Opr),0);
end;


destructor TOperand.done;
begin
end;


Procedure TOperand.SetSize(_size:longint);
begin
  if (size = S_NO) and (_size<extended_size) then
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
  if assigned(procinfo^.returntype.def) and
     (procinfo^.returntype.def<>pdef(voiddef)) then
   begin
     opr.ref.offset:=procinfo^.return_offset;
     opr.ref.base:= procinfo^.framepointer;
     { always assume that the result is valid. }
     procinfo^.funcret_state:=vs_assigned;
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
     SetupOldEBP:=true;
   end
  else
   Message(asmr_e_cannot_use_OLDEBP_outside_nested_procedure);
end;


Function TOperand.SetupVar(const hs:string;GetOffset : boolean): Boolean;
{ search and sets up the correct fields in the Instr record }
{ for the NON-constant identifier passed to the routine.    }
{ if not found returns FALSE.                               }
var
  sym : psym;
  harrdef : parraydef;
Begin
  SetupVar:=false;
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
        pvarsym(sym)^.varstate:=vs_used;
        inc(pvarsym(sym)^.refs);
        case pvarsym(sym)^.owner^.symtabletype of
          objectsymtable :
            begin
              { this is not allowed, because we don't know if the self
                register is still free, and loading it first is also
                not possible, because this could break code }
              opr.typ:=OPR_CONSTANT;
              opr.val:=pvarsym(sym)^.address;
              hasvar:=true;
              SetupVar:=true;
              Exit;
            end;
          unitsymtable,
          globalsymtable,
          staticsymtable :
            opr.ref.symbol:=newasmsymbol(pvarsym(sym)^.mangledname);
          parasymtable :
            begin
              { if we only want the offset we don't have to care
                the base will be zeroed after ! }
              if (lexlevel=pvarsym(sym)^.owner^.symtablelevel) or
              { this below is wrong because there are two parast
                for global functions one of interface the second of
                implementation
              if (pvarsym(sym)^.owner=procinfo^.def^.parast) or }
                GetOffset then
                opr.ref.base:=procinfo^.framepointer
              else
                begin
                  if (procinfo^.framepointer=R_ESP) and
                     assigned(procinfo^.parent) and
                     (lexlevel=pvarsym(sym)^.owner^.symtablelevel+1) and
                     { same problem as above !!
                     (procinfo^.parent^.sym^.definition^.parast=pvarsym(sym)^.owner) and }
                     (lexlevel>normal_function_level) then
                    opr.ref.base:=procinfo^.parent^.framepointer
                  else
                    message1(asmr_e_local_para_unreachable,hs);
                end;
              opr.ref.offset:=pvarsym(sym)^.address;
              opr.ref.offsetfixup:=aktprocsym^.definition^.parast^.address_fixup;
              opr.ref.options:=ref_parafixup;
            end;
          localsymtable :
            begin
              if (vo_is_external in pvarsym(sym)^.varoptions) then
                opr.ref.symbol:=newasmsymbol(pvarsym(sym)^.mangledname)
              else
                begin
                  { if we only want the offset we don't have to care
                    the base will be zeroed after ! }
                  if (lexlevel=pvarsym(sym)^.owner^.symtablelevel) or
                  {if (pvarsym(sym)^.owner=procinfo^.def^.localst) or}
                    GetOffset then
                    opr.ref.base:=procinfo^.framepointer
                  else
                    begin
                      if (procinfo^.framepointer=R_ESP) and
                         assigned(procinfo^.parent) and
                         (lexlevel=pvarsym(sym)^.owner^.symtablelevel+1) and
                         {(procinfo^.parent^.sym^.definition^.localst=pvarsym(sym)^.owner) and}
                         (lexlevel>normal_function_level) then
                        opr.ref.base:=procinfo^.parent^.framepointer
                      else
                        message1(asmr_e_local_para_unreachable,hs);
                    end;
                  opr.ref.offset:=-(pvarsym(sym)^.address);
                  opr.ref.options:=ref_localfixup;
                  opr.ref.offsetfixup:=aktprocsym^.definition^.localst^.address_fixup;
                end;
            end;
        end;
        case pvarsym(sym)^.vartype.def^.deftype of
          orddef,
          enumdef,
          pointerdef,
          floatdef :
            SetSize(pvarsym(sym)^.getsize);
          arraydef :
            begin
              { for arrays try to get the element size, take care of
                multiple indexes }
              harrdef:=Parraydef(PVarsym(sym)^.vartype.def);
              while assigned(harrdef^.elementtype.def) and
                    (harrdef^.elementtype.def^.deftype=arraydef) do
               harrdef:=parraydef(harrdef^.elementtype.def);
              SetSize(harrdef^.elesize);
            end;
        end;
        hasvar:=true;
        SetupVar:=true;
        Exit;
      end;
    typedconstsym :
      begin
        opr.ref.symbol:=newasmsymbol(ptypedconstsym(sym)^.mangledname);
        case ptypedconstsym(sym)^.typedconsttype.def^.deftype of
          orddef,
          enumdef,
          pointerdef,
          floatdef :
            SetSize(ptypedconstsym(sym)^.getsize);
          arraydef :
            begin
              { for arrays try to get the element size, take care of
                multiple indexes }
              harrdef:=Parraydef(PTypedConstSym(sym)^.typedconsttype.def);
              while assigned(harrdef^.elementtype.def) and
                    (harrdef^.elementtype.def^.deftype=arraydef) do
               harrdef:=parraydef(harrdef^.elementtype.def);
              SetSize(harrdef^.elesize);
            end;
        end;
        hasvar:=true;
        SetupVar:=true;
        Exit;
      end;
    constsym :
      begin
        if pconstsym(sym)^.consttyp in [constint,constchar,constbool] then
         begin
           opr.typ:=OPR_CONSTANT;
           opr.val:=pconstsym(sym)^.value;
           hasvar:=true;
           SetupVar:=true;
           Exit;
         end;
      end;
    typesym :
      begin
        if ptypesym(sym)^.restype.def^.deftype in [recorddef,objectdef] then
         begin
           opr.typ:=OPR_CONSTANT;
           opr.val:=0;
           hasvar:=true;
           SetupVar:=TRUE;
           Exit;
         end;
      end;
    procsym :
      begin
        if assigned(pprocsym(sym)^.definition^.nextoverloaded) then
          Message(asmr_w_calling_overload_func);
        opr.typ:=OPR_SYMBOL;
        opr.symbol:=newasmsymbol(pprocsym(sym)^.definition^.mangledname);
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
{$ifndef OLDDIRECTVAR}
var
  p : pasmsymbol;
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
{$else}
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
       instr.operands[operandnum].opr.ref.symbol:=p^.sym;
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
{$endif}

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

constructor TInstruction.init;
Begin
  Opcode:=A_NONE;
  Opsize:=S_NO;
  Condition:=C_NONE;
  Ops:=0;
  InitOperands;
end;


destructor TInstruction.done;
var
  i : longint;
Begin
  for i:=1 to 3 do
   Dispose(Operands[i],Done);
end;


procedure TInstruction.InitOperands;
var
  i : longint;
begin
  for i:=1 to 3 do
   New(Operands[i],init);
end;


Procedure TInstruction.SwapOperands;
Var
  p : POperand;
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


procedure TInstruction.ConcatInstruction(p:PAasmOutput);
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

constructor TLocalLabel.Init(const n:string);
begin
  inherited InitName(n);
  lab:=nil;
  emitted:=false;
end;


function TLocalLabel.Getpasmlabel:pasmlabel;
begin
  if not assigned(lab) then
   begin
     getlabel(lab);
     { this label is forced to be used so it's always written }
     inc(lab^.refs);
   end;
  Getpasmlabel:=lab;
end;


{***************************************************************************
                             TLocalLabelList
***************************************************************************}

procedure LocalLabelEmitted(p:PNamedIndexObject);{$ifndef FPC}far;{$endif}
begin
  if not PLocalLabel(p)^.emitted  then
   Message1(asmr_e_unknown_label_identifier,p^.name);
end;

procedure TLocalLabelList.CheckEmitted;
begin
  ForEach({$ifndef TP}@{$endif}LocalLabelEmitted)
end;


function CreateLocalLabel(const s: string; var hl: pasmlabel; emit:boolean):boolean;
var
  lab : PLocalLabel;
Begin
  CreateLocalLabel:=true;
{ Check if it already is defined }
  lab:=PLocalLabel(LocalLabelList^.Search(s));
  if not assigned(lab) then
   begin
     new(lab,init(s));
     LocalLabelList^.Insert(lab);
   end;
{ set emitted flag and check for dup syms }
  if emit then
   begin
     if lab^.Emitted then
      begin
        Message1(asmr_e_dup_local_sym,lab^.Name);
        CreateLocalLabel:=false;
      end;
     lab^.Emitted:=true;
   end;
  hl:=lab^.Getpasmlabel;
end;


{****************************************************************************
                      Symbol table helper routines
****************************************************************************}

Function SearchRecordType(const s:string): boolean;
Begin
  SearchRecordType:=false;
{ Check the constants in symtable }
  getsym(s,false);
  if srsym <> nil then
   Begin
     case srsym^.typ of
       typesym :
         begin
           if ptypesym(srsym)^.restype.def^.deftype in [recorddef,objectdef] then
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
  getsym(s,false);
  if srsym <> nil then
   Begin
     case srsym^.typ of
       constsym :
         begin
           if (pconstsym(srsym)^.consttyp in [constord,constint,constchar,constbool]) then
            Begin
              l:=pconstsym(srsym)^.value;
              SearchIConstant:=TRUE;
              exit;
            end;
         end;
       enumsym:
         Begin
           l:=penumsym(srsym)^.value;
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
  st   : psymtable;
  harrdef : parraydef;
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
  if base='SELF' then
   st:=procinfo^._class^.symtable
  else
   begin
     getsym(base,false);
     sym:=srsym;
     st:=nil;
     { we can start with a var,type,typedconst }
     case sym^.typ of
       varsym :
         begin
           case pvarsym(sym)^.vartype.def^.deftype of
             recorddef :
               st:=precorddef(pvarsym(sym)^.vartype.def)^.symtable;
             objectdef :
               st:=pobjectdef(pvarsym(sym)^.vartype.def)^.symtable;
           end;
         end;
       typesym :
         begin
           case ptypesym(sym)^.restype.def^.deftype of
             recorddef :
               st:=precorddef(ptypesym(sym)^.restype.def)^.symtable;
             objectdef :
               st:=pobjectdef(ptypesym(sym)^.restype.def)^.symtable;
           end;
         end;
       typedconstsym :
         begin
           case ptypedconstsym(sym)^.typedconsttype.def^.deftype of
             recorddef :
               st:=precorddef(ptypedconstsym(sym)^.typedconsttype.def)^.symtable;
             objectdef :
               st:=pobjectdef(ptypedconstsym(sym)^.typedconsttype.def)^.symtable;
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
     if st^.symtabletype=objectsymtable then
       sym:=search_class_member(pobjectdef(st^.defowner),base)
     else
       sym:=st^.search(base);
     if not assigned(sym) then
      begin
        GetRecordOffsetSize:=false;
        exit;
      end;
     st:=nil;
     case sym^.typ of
       varsym :
         begin
           inc(Offset,pvarsym(sym)^.address);
           Size:=PVarsym(sym)^.getsize;
           case pvarsym(sym)^.vartype.def^.deftype of
             arraydef :
               begin
                 { for arrays try to get the element size, take care of
                   multiple indexes }
                 harrdef:=Parraydef(PVarsym(sym)^.vartype.def);
                 while assigned(harrdef^.elementtype.def) and
                       (harrdef^.elementtype.def^.deftype=arraydef) do
                  harrdef:=parraydef(harrdef^.elementtype.def);
                 size:=harrdef^.elesize;
               end;
             recorddef :
               st:=precorddef(pvarsym(sym)^.vartype.def)^.symtable;
             objectdef :
               st:=pobjectdef(pvarsym(sym)^.vartype.def)^.symtable;
           end;
         end;
     end;
   end;
   GetRecordOffsetSize:=(s='');
end;


Function SearchLabel(const s: string; var hl: pasmlabel;emit:boolean): boolean;
var
  sym : psym;
  hs  : string;
Begin
  hl:=nil;
  SearchLabel:=false;
{ Check for pascal labels, which are case insensetive }
  hs:=upper(s);
  getsym(hs,false);
  sym:=srsym;
  if sym=nil then
   exit;
  case sym^.typ of
    labelsym :
      begin
        hl:=plabelsym(sym)^.lab;
        if emit then
         plabelsym(sym)^.defined:=true
        else
         plabelsym(sym)^.used:=true;
        SearchLabel:=true;
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
  if (maxvalue <> $ffffffff) and (value > maxvalue) then
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
    p^.concat(new(pai_const_symbol,initname_offset(sym,l)));
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
          s32real : p^.concat(new(pai_real_32bit,init(value)));
          s64real : p^.concat(new(pai_real_64bit,init(value)));
          s80real : p^.concat(new(pai_real_80bit,init(value)));
          s64comp : p^.concat(new(pai_comp_64bit,init(value)));
          f32bit  : p^.concat(new(pai_const,init_32bit(trunc(value*$10000))));
       end;
    end;

   Procedure ConcatLabel(p: paasmoutput;var l : pasmlabel);
  {*********************************************************************}
  { PROCEDURE ConcatLabel                                               }
  {  Description: This routine either emits a label or a labeled        }
  {  instruction to the linked list of instructions.                    }
  {*********************************************************************}
   begin
     p^.concat(new(pai_label,init(l)));
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
       p^.concat(new(pai_symbol,initname_global(s,0)));
   end;

   procedure ConcatLocal(p:paasmoutput;const s : string);
  {*********************************************************************}
  { PROCEDURE ConcatLocal                                               }
  {  Description: This routine emits an local    definition to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       p^.concat(new(pai_symbol,initname(s,0)));
   end;

  Procedure ConcatGlobalBss(const s : string;size : longint);
  {*********************************************************************}
  { PROCEDURE ConcatGlobalBss                                           }
  {  Description: This routine emits an global  datablock   to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       bsssegment^.concat(new(pai_datablock,init_global(s,size)));
   end;

  Procedure ConcatLocalBss(const s : string;size : longint);
  {*********************************************************************}
  { PROCEDURE ConcatLocalBss                                            }
  {  Description: This routine emits a local datablcok      to the      }
  {  linked list of instructions.                                       }
  {*********************************************************************}
   begin
       bsssegment^.concat(new(pai_datablock,init(s,size)));
   end;

end.
{
  $Log$
  Revision 1.36  2000-03-15 23:10:01  pierre
    * fix for bug 848 (that still genrated wrong code)
    + better testing for variables used in assembler
      (gives an error if variable is not directly reachable !)

  Revision 1.35  2000/02/09 13:23:03  peter
    * log truncated

  Revision 1.34  2000/01/07 01:14:37  peter
    * updated copyright to 2000

  Revision 1.33  1999/12/22 00:57:30  peter
    * label are set to used so an error is given if used but not defined

  Revision 1.32  1999/12/17 10:43:34  florian
    * 761 fixed

  Revision 1.31  1999/11/30 10:40:54  peter
    + ttype, tsymlist

  Revision 1.30  1999/11/17 17:05:04  pierre
   * Notes/hints changes

  Revision 1.29  1999/11/09 23:06:46  peter
    * esi_offset -> selfpointer_offset to be newcg compatible
    * hcogegen -> cgbase fixes for newcg

  Revision 1.28  1999/11/06 14:34:26  peter
    * truncated log to 20 revs

  Revision 1.27  1999/09/27 23:44:58  peter
    * procinfo is now a pointer
    * support for result setting in sub procedure

  Revision 1.26  1999/09/08 16:04:04  peter
    * better support for object fields and more error checks for
      field accesses which create buggy code

  Revision 1.25  1999/09/04 20:29:11  florian
    * bug 577 fixed

  Revision 1.24  1999/08/27 14:37:50  peter
    * fixed crash with typedconst array

  Revision 1.23  1999/08/13 21:28:38  peter
    * more reference types support
    * arraydef size returns elementsize, also for multiple indexing array

  Revision 1.22  1999/08/04 00:23:28  florian
    * renamed i386asm and i386base to cpuasm and cpubase

  Revision 1.21  1999/08/03 22:03:12  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.20  1999/07/29 20:54:06  peter
    * write .size also

}