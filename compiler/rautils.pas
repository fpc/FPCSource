{
    $Id$
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

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

{$i fpcdefs.inc}

Interface

Uses
  cutils,cclasses,
  globtype,aasmbase,aasmtai,cpubase,cpuinfo,cginfo,
  symconst,symbase,symtype,symdef,symsym;

Const
  RPNMax = 10;             { I think you only need 4, but just to be safe }
  OpMax  = 25;



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
  TOprType=(OPR_NONE,OPR_CONSTANT,OPR_SYMBOL,OPR_LOCAL,
            OPR_REFERENCE,OPR_REGISTER,OPR_REGLIST);

  TOprRec = record
    case typ:TOprType of
      OPR_NONE   : ();
      OPR_CONSTANT  : (val:longint);
      OPR_SYMBOL    : (symbol:tasmsymbol;symofs:longint);
      OPR_REFERENCE : (ref:treference);
      OPR_LOCAL     : (localsym:tvarsym;localsymofs:longint);
      OPR_REGISTER  : (reg:tregister);
{$ifdef m68k}
      OPR_REGLIST   : (reglist:Tsupregset);
{$else not m68k}
      OPR_REGLIST   : ();
{$endif m68k}
  end;

  TOperand = class
    hastype,          { if the operand has typecasted variable }
    hasvar : boolean; { if the operand is loaded with a variable }
    size   : TCGSize;
    opr    : TOprRec;
    constructor create;
    destructor  destroy;override;
    Procedure BuildOperand;virtual;
    Procedure SetSize(_size:longint;force:boolean);virtual;
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
    condition : tasmcond;
    ops       : byte;
    labeled   : boolean;
    operands  : array[1..max_operands] of toperand;
    constructor create;
    destructor  destroy;override;
    Procedure InitOperands;virtual;
    Procedure BuildOpcode;virtual;
    procedure ConcatInstruction(p:TAAsmoutput);virtual;
    Procedure Swapoperands;
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
  defutil,systems,verbose,globals,
  symtable,paramgr,
  aasmcpu,
  cgbase,tgobj;

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
  size:=OS_NO;
  hastype:=false;
  hasvar:=false;
  FillChar(Opr,sizeof(Opr),0);
end;


destructor TOperand.destroy;
begin
end;


Procedure TOperand.SetSize(_size:longint;force:boolean);
begin
  if force or
     ((size = OS_NO) and (_size<=extended_size)) then
   Begin
     case _size of
      1 : size:=OS_8;
      2 : size:=OS_16{ could be S_IS};
      4 : size:=OS_32{ could be S_IL or S_FS};
      8 : size:=OS_64{ could be S_D or S_FL};
     else
      begin
        { extended_size can also be 8, resulting in a
          duplicate label }
        if _size=extended_size then
          size:=OS_F80;
      end;
     end;
   end;
end;


Procedure TOperand.SetCorrectSize(opcode:tasmop);
begin
end;


Function TOperand.SetupResult:boolean;
Begin
  SetupResult:=false;
  { replace by correct offset. }
  if (not is_void(current_procinfo.procdef.rettype.def)) then
   begin
     if (m_tp7 in aktmodeswitches) and
        (not paramanager.ret_in_param(current_procinfo.procdef.rettype.def,current_procinfo.procdef.proccalloption)) then
       begin
         Message(asmr_e_cannot_use_RESULT_here);
         exit;
       end;
     SetupResult:=setupvar('result',false)
   end
  else
   Message(asmr_e_void_function);
end;


Function TOperand.SetupSelf:boolean;
Begin
  SetupSelf:=false;
  if assigned(current_procinfo.procdef._class) then
    SetupSelf:=setupvar('self',false)
  else
    Message(asmr_e_cannot_use_SELF_outside_a_method);
end;


Function TOperand.SetupOldEBP:boolean;
Begin
  SetupOldEBP:=false;
  if current_procinfo.procdef.parast.symtablelevel>normal_function_level then
    SetupOldEBP:=setupvar('parentframe',false)
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
  if sym.typ=absolutesym then
    begin
      if (tabsolutesym(sym).abstyp=tovar) then
        sym:=tabsolutesym(sym).ref
      else
        Message(asmr_e_unsupported_symbol_type);
    end;
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
              { We return the address of the field, just like Delphi/TP }
              opr.typ:=OPR_CONSTANT;
              opr.val:=tvarsym(sym).fieldoffset;
              hasvar:=true;
              SetupVar:=true;
              Exit;
            end;
          globalsymtable,
          staticsymtable :
            opr.ref.symbol:=objectlibrary.newasmsymboldata(tvarsym(sym).mangledname);
          parasymtable,
          localsymtable :
            begin
              if (vo_is_external in tvarsym(sym).varoptions) then
                opr.ref.symbol:=objectlibrary.newasmsymboldata(tvarsym(sym).mangledname)
              else
                begin
                  opr.typ:=OPR_LOCAL;
                  if assigned(current_procinfo.parent) and
                     (
                      (tvarsym(sym).owner<>current_procinfo.procdef.localst) or
                      (tvarsym(sym).owner<>current_procinfo.procdef.parast)
                     ) and
                     (current_procinfo.procdef.localst.symtablelevel>normal_function_level) then
                    message1(asmr_e_local_para_unreachable,s);
                  opr.localsym:=tvarsym(sym);
                  opr.localsymofs:=0;
                end;
              if paramanager.push_addr_param(tvarsym(sym).varspez,tvarsym(sym).vartype.def,current_procinfo.procdef.proccalloption) then
                SetSize(pointer_size,false);
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
        opr.ref.symbol:=objectlibrary.newasmsymboldata(ttypedconstsym(sym).mangledname);
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
           opr.val:=tconstsym(sym).value.valueord;
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
        if Tprocsym(sym).procdef_count>1 then
          Message(asmr_w_calling_overload_func);
        l:=opr.ref.offset;
        opr.typ:=OPR_SYMBOL;
        opr.symbol:=objectlibrary.newasmsymbol(tprocsym(sym).first_procdef.mangledname);
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
  p:=objectlibrary.getasmsymbol(hs);
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
  Fillchar(opr.ref,sizeof(treference),0);
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


Procedure TInstruction.Swapoperands;
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
     objectlibrary.getlabel(lab);
     { this label is forced to be used so it's always written }
     lab.increfs;
   end;
  Gettasmlabel:=lab;
end;


{***************************************************************************
                             TLocalLabelList
***************************************************************************}

procedure LocalLabelEmitted(p:tnamedindexitem;arg:pointer);
begin
  if not TLocalLabel(p).emitted  then
   Message1(asmr_e_unknown_label_identifier,p.name);
end;

procedure TLocalLabelList.CheckEmitted;
begin
  ForEach_Static({$ifdef FPCPROCVAR}@{$endif}LocalLabelEmitted,nil)
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
              l:=tconstsym(srsym).value.valueord;
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
   st:=current_procinfo.procdef._class.symtable
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
           inc(Offset,tvarsym(sym).fieldoffset);
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
  Revision 1.67  2003-09-23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.66  2003/09/16 16:17:01  peter
    * varspez in calls to push_addr_param

  Revision 1.65  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.64.2.1  2003/08/27 19:55:54  peter
    * first tregister patch

  Revision 1.64  2003/06/13 21:19:31  peter
    * current_procdef removed, use current_procinfo.procdef instead

  Revision 1.63  2003/06/06 14:43:29  peter
    * absolutesym support

  Revision 1.62  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.61  2003/05/25 08:55:49  peter
    * load result using hidden parameter

  Revision 1.60  2003/05/15 18:58:53  peter
    * removed selfpointer_offset, vmtpointer_offset
    * tvarsym.adjusted_address
    * address in localsymtable is now in the real direction
    * removed some obsolete globals

  Revision 1.59  2003/05/12 17:22:00  jonas
    * fixed (last?) remaining -tvarsym(X).address to
      tg.direction*tvarsym(X).address...

  Revision 1.58  2003/04/27 11:21:34  peter
    * aktprocdef renamed to current_procinfo.procdef
    * procinfo renamed to current_procinfo
    * procinfo will now be stored in current_module so it can be
      cleaned up properly
    * gen_main_procsym changed to create_main_proc and release_main_proc
      to also generate a tprocinfo structure
    * fixed unit implicit initfinal

  Revision 1.57  2003/04/27 07:29:51  peter
    * current_procinfo.procdef cleanup, current_procinfo.procdef is now always nil when parsing
      a new procdef declaration
    * aktprocsym removed
    * lexlevel removed, use symtable.symtablelevel instead
    * implicit init/final code uses the normal genentry/genexit
    * funcret state checking updated for new funcret handling

  Revision 1.56  2003/04/25 20:59:34  peter
    * removed funcretn,funcretsym, function result is now in varsym
      and aliases for result and function name are added using absolutesym
    * vs_hidden parameter for funcret passed in parameter
    * vs_hidden fixes
    * writenode changed to printnode and released from extdebug
    * -vp option added to generate a tree.log with the nodetree
    * nicer printnode for statements, callnode

  Revision 1.55  2003/04/06 21:11:23  olle
    * changed newasmsymbol to newasmsymboldata for data symbols

  Revision 1.54  2003/03/28 19:16:57  peter
    * generic constructor working for i386
    * remove fixed self register
    * esi added as address register for i386

  Revision 1.53  2003/02/19 22:00:14  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.52  2003/01/08 18:43:56  daniel
   * Tregister changed into a record

  Revision 1.51  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.50  2002/11/25 17:43:23  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.49  2002/11/22 22:48:10  carl
  * memory optimization with tconstsym (1.5%)

  Revision 1.48  2002/11/18 17:31:59  peter
    * pass proccalloption to ret_in_xxx and push_xxx functions

  Revision 1.47  2002/11/15 16:29:31  peter
    * made tasmsymbol.refs private (merged)

  Revision 1.46  2002/09/03 16:26:27  daniel
    * Make Tprocdef.defs protected

  Revision 1.45  2002/08/25 19:25:20  peter
    * sym.insert_in_data removed
    * symtable.insertvardata/insertconstdata added
    * removed insert_in_data call from symtable.insert, it needs to be
      called separatly. This allows to deref the address calculation
    * procedures now calculate the parast addresses after the procedure
      directives are parsed. This fixes the cdecl parast problem
    * push_addr_param has an extra argument that specifies if cdecl is used
      or not

  Revision 1.44  2002/08/17 09:23:41  florian
    * first part of procinfo rewrite

  Revision 1.43  2002/08/16 14:24:59  carl
    * issameref() to test if two references are the same (then emit no opcodes)
    + ret_in_reg to replace ret_in_acc
      (fix some register allocation bugs at the same time)
    + save_std_register now has an extra parameter which is the
      usedinproc registers

  Revision 1.42  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.41  2002/08/12 15:08:40  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.40  2002/08/11 14:32:27  peter
    * renamed current_library to objectlibrary

  Revision 1.39  2002/08/11 13:24:13  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.38  2002/07/20 11:57:57  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.37  2002/07/11 14:41:28  florian
    * start of the new generic parameter handling

  Revision 1.36  2002/07/01 18:46:25  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.35  2002/05/18 13:34:17  peter
    * readded missing revisions

  Revision 1.34  2002/05/16 19:46:44  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.31  2002/05/12 16:53:10  peter
    * moved entry and exitcode to ncgutil and cgobj
    * foreach gets extra argument for passing local data to the
      iterator function
    * -CR checks also class typecasts at runtime by changing them
      into as
    * fixed compiler to cycle with the -CR option
    * fixed stabs with elf writer, finally the global variables can
      be watched
    * removed a lot of routines from cga unit and replaced them by
      calls to cgobj
    * u32bit-s32bit updates for and,or,xor nodes. When one element is
      u32bit then the other is typecasted also to u32bit without giving
      a rangecheck warning/error.
    * fixed pascal calling method with reversing also the high tree in
      the parast, detected by tcalcst3 test

  Revision 1.30  2002/04/20 21:32:24  carl
  + generic FPC_CHECKPOINTER
  + first parameter offset in stack now portable
  * rename some constants
  + move some cpu stuff to other units
  - remove unused constents
  * fix stacksize for some targets
  * fix generic size problems which depend now on EXTEND_SIZE constant

  Revision 1.29  2002/04/15 19:02:35  carl
  + target_info.size_of_pointer -> pointer_Size

  Revision 1.28  2002/04/02 17:11:29  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.27  2002/01/29 21:32:03  peter
    * allow accessing locals in other lexlevel when the current assembler
      routine doesn't have locals.

  Revision 1.26  2002/01/24 18:25:50  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
