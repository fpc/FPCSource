{
    $Id$
    Copyright (c) 1998-2002 by Carl Eric Codere and Peter Vreman

    Does the parsing for the AT&T styled inline assembler.

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
Unit Ra386att;

{$i fpcdefs.inc}

Interface

uses
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
       scanner,
       itx86att,
       rax86,rautils,
       cginfo,cgobj
       ;

type
 tasmtoken = (
   AS_NONE,AS_LABEL,AS_LLABEL,AS_STRING,AS_INTNUM,
   AS_REALNUM,AS_COMMA,AS_LPAREN,
   AS_RPAREN,AS_COLON,AS_DOT,AS_PLUS,AS_MINUS,AS_STAR,
   AS_SEPARATOR,AS_ID,AS_REGISTER,AS_OPCODE,AS_SLASH,AS_DOLLAR,
   {------------------ Assembler directives --------------------}
   AS_DB,AS_DW,AS_DD,AS_DQ,AS_GLOBAL,
   AS_ALIGN,AS_BALIGN,AS_P2ALIGN,AS_ASCII,
   AS_ASCIIZ,AS_LCOMM,AS_COMM,AS_SINGLE,AS_DOUBLE,AS_EXTENDED,
   AS_DATA,AS_TEXT,AS_END,
   {------------------ Assembler Operators  --------------------}
   AS_TYPE,AS_MOD,AS_SHL,AS_SHR,AS_NOT,AS_AND,AS_OR,AS_XOR,AS_NOR);

   tasmkeyword = string[10];

const
   { These tokens should be modified accordingly to the modifications }
   { in the different enumerations.                                   }
   firstdirective = AS_DB;
   lastdirective  = AS_END;

  token2str : array[tasmtoken] of tasmkeyword=(
    '','Label','LLabel','string','integer',
    'float',',','(',
    ')',':','.','+','-','*',
    ';','identifier','register','opcode','/','$',
    '.byte','.word','.long','.quad','.globl',
    '.align','.balign','.p2align','.ascii',
    '.asciz','.lcomm','.comm','.single','.double','.tfloat',
    '.data','.text','END',
    'TYPE','%','<<','>>','!','&','|','^','~');

const
  newline = #10;
  firsttoken : boolean = TRUE;
var
  _asmsorted     : boolean;
  curlist        : TAAsmoutput;
  c              : char;
  actasmtoken    : tasmtoken;
  prevasmtoken   : tasmtoken;
  actasmpattern  : string;
  actopcode      : tasmop;
  actasmregister : tregister;
  actopsize      : topsize;
  actcondition   : tasmcond;
  iasmops        : tdictionary;


Procedure SetupTables;
{ creates uppercased symbol tables for speed access }
var
  i : tasmop;
  str2opentry: tstr2opentry;
Begin
  { opcodes }
  iasmops:=TDictionary.Create;
  iasmops.delete_doubles:=true;
  for i:=firstop to lastop do
    begin
      str2opentry:=tstr2opentry.createname(upper(gas_op2str[i]));
      str2opentry.op:=i;
      iasmops.insert(str2opentry);
    end;
end;


  {---------------------------------------------------------------------}
  {                    Routines for the tokenizing                     }
  {---------------------------------------------------------------------}

function is_asmopcode(const s: string):boolean;
const
  { We need first to check the long prefixes, else we get probs
    with things like movsbl }
  att_sizesuffixstr : array[0..9] of string[2] = (
    '','BW','BL','WL','B','W','L','S','Q','T'
  );
  att_sizesuffix : array[0..9] of topsize = (
    S_NO,S_BW,S_BL,S_WL,S_B,S_W,S_L,S_FS,S_IQ,S_FX
  );
  att_sizefpusuffix : array[0..9] of topsize = (
    S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_FL,S_FS,S_IQ,S_FX
  );
  att_sizefpuintsuffix : array[0..9] of topsize = (
    S_NO,S_NO,S_NO,S_NO,S_NO,S_NO,S_IL,S_IS,S_IQ,S_NO
  );
var
  str2opentry: tstr2opentry;
  cond : string[4];
  cnd  : tasmcond;
  len,
  j,
  sufidx : longint;
Begin
  is_asmopcode:=FALSE;

  actopcode:=A_None;
  actcondition:=C_None;
  actopsize:=S_NO;

  { search for all possible suffixes }
  for sufidx:=low(att_sizesuffixstr) to high(att_sizesuffixstr) do
   begin
     len:=length(s)-length(att_sizesuffixstr[sufidx]);
     if copy(s,len+1,length(att_sizesuffixstr[sufidx]))=att_sizesuffixstr[sufidx] then
      begin
        { here we search the entire table... }
        str2opentry:=nil;
        if {(length(s)>0) and} (len>0) then
          str2opentry:=tstr2opentry(iasmops.search(copy(s,1,len)));
        if assigned(str2opentry) then
          begin
            actopcode:=str2opentry.op;
            if gas_needsuffix[actopcode]=attsufFPU then
             actopsize:=att_sizefpusuffix[sufidx]
            else if gas_needsuffix[actopcode]=attsufFPUint then
             actopsize:=att_sizefpuintsuffix[sufidx]
            else
             actopsize:=att_sizesuffix[sufidx];
            actasmtoken:=AS_OPCODE;
            is_asmopcode:=TRUE;
            exit;
          end;
        { not found, check condition opcodes }
        j:=0;
        while (j<CondAsmOps) do
         begin
           if Copy(s,1,Length(CondAsmOpStr[j]))=CondAsmOpStr[j] then
            begin
              cond:=Copy(s,Length(CondAsmOpStr[j])+1,len-Length(CondAsmOpStr[j]));
              if cond<>'' then
               begin
                 for cnd:=low(TasmCond) to high(TasmCond) do
                  if Cond=Upper(cond2str[cnd]) then
                   begin
                     actopcode:=CondASmOp[j];
                     if gas_needsuffix[actopcode]=attsufFPU then
                      actopsize:=att_sizefpusuffix[sufidx]
                     else if gas_needsuffix[actopcode]=attsufFPUint then
                      actopsize:=att_sizefpuintsuffix[sufidx]
                     else
                      actopsize:=att_sizesuffix[sufidx];
                     actcondition:=cnd;
                     actasmtoken:=AS_OPCODE;
                     is_asmopcode:=TRUE;
                     exit;
                   end;
               end;
            end;
           inc(j);
         end;
     end;
   end;
end;


Function is_asmdirective(const s: string):boolean;
var
  i : tasmtoken;
  hs : string;
Begin
  { GNU as is also not casesensitive with this }
  hs:=lower(s);
  for i:=firstdirective to lastdirective do
   if hs=token2str[i] then
    begin
      actasmtoken:=i;
      is_asmdirective:=true;
      exit;
    end;
  is_asmdirective:=false;
end;


function is_register(const s:string):boolean;
begin
  is_register:=false;
  actasmregister:=gas_regnum_search(lower(s));
  if actasmregister<>NR_NO then
    begin
      is_register:=true;
      actasmtoken:=AS_REGISTER;
    end;
end;


Function is_locallabel(const s: string):boolean;
begin
  is_locallabel:=(length(s)>=2) and (s[1]='.') and (s[2]='L');
end;


Procedure GetToken;
var
  len : longint;
  srsym : tsym;
  srsymtable : tsymtable;
begin
  { save old token and reset new token }
  prevasmtoken:=actasmtoken;
  actasmtoken:=AS_NONE;
  { reset }
  actasmpattern:='';
  { while space and tab , continue scan... }
  while c in [' ',#9] do
   c:=current_scanner.asmgetchar;
  { get token pos }
  if not (c in [newline,#13,'{',';']) then
    current_scanner.gettokenpos;
{ Local Label, Label, Directive, Prefix or Opcode }
  if firsttoken and not(c in [newline,#13,'{',';']) then
   begin
     firsttoken:=FALSE;
     len:=0;
     { directive or local label }
     if c = '.' then
      begin
        inc(len);
        actasmpattern[len]:=c;
        { Let us point to the next character }
        c:=current_scanner.asmgetchar;
        while c in ['A'..'Z','a'..'z','0'..'9','_','$'] do
         begin
           inc(len);
           actasmpattern[len]:=c;
           c:=current_scanner.asmgetchar;
         end;
        actasmpattern[0]:=chr(len);
        { this is a local label... }
        if (c=':') and is_locallabel(actasmpattern) then
         Begin
           { local variables are case sensitive }
           actasmtoken:=AS_LLABEL;
           c:=current_scanner.asmgetchar;
           firsttoken:=true;
           exit;
         end
        { must be a directive }
        else
         Begin
           { directives are case sensitive!! }
           if is_asmdirective(actasmpattern) then
            exit;
           Message1(asmr_e_not_directive_or_local_symbol,actasmpattern);
         end;
      end;
     { only opcodes and global labels are allowed now. }
     while c in ['A'..'Z','a'..'z','0'..'9','_'] do
      begin
        inc(len);
        actasmpattern[len]:=c;
        c:=current_scanner.asmgetchar;
      end;
     actasmpattern[0]:=chr(len);
     { Label ? }
     if c = ':' then
      begin
        actasmtoken:=AS_LABEL;
        { let us point to the next character }
        c:=current_scanner.asmgetchar;
        firsttoken:=true;
        exit;
      end;
     { Opcode ? }
     If is_asmopcode(upper(actasmpattern)) then
      Begin
        uppervar(actasmpattern);
        exit;
      end;
     { End of assemblerblock ? }
     if upper(actasmpattern) = 'END' then
      begin
        actasmtoken:=AS_END;
        exit;
      end;
     message1(asmr_e_unknown_opcode,actasmpattern);
     actasmtoken:=AS_NONE;
   end
  else { else firsttoken }
  { Here we must handle all possible cases }
   begin
     case c of
       '.' :  { possiblities : - local label reference , such as in jmp @local1 }
              {               - field of object/record                         }
              {               - directive.                                     }
         begin
           if (prevasmtoken in [AS_ID,AS_RPAREN]) then
            begin
              c:=current_scanner.asmgetchar;
              actasmtoken:=AS_DOT;
              exit;
            end;
           actasmpattern:=c;
           c:=current_scanner.asmgetchar;
           while c in  ['A'..'Z','a'..'z','0'..'9','_','$'] do
            begin
              actasmpattern:=actasmpattern + c;
              c:=current_scanner.asmgetchar;
            end;
           if is_asmdirective(actasmpattern) then
            exit;
           { local label references and directives }
           { are case sensitive                    }
           actasmtoken:=AS_ID;
           exit;
         end;

    { identifier, register, prefix or directive }
       '_','A'..'Z','a'..'z':
         begin
           len:=0;
           while c in ['A'..'Z','a'..'z','0'..'9','_','$'] do
            begin
              inc(len);
              actasmpattern[len]:=c;
              c:=current_scanner.asmgetchar;
            end;
           actasmpattern[0]:=chr(len);
           uppervar(actasmpattern);
           { Opcode, can only be when the previous was a prefix }
           If is_prefix(actopcode) and is_asmopcode(actasmpattern) then
            Begin
              uppervar(actasmpattern);
              exit;
            end;
           { check for end which is a reserved word unlike the opcodes }
           if actasmpattern = 'END' then
            Begin
              actasmtoken:=AS_END;
              exit;
            end;
           if actasmpattern = 'TYPE' then
            Begin
              actasmtoken:=AS_TYPE;
              exit;
            end;
           { if next is a '.' and this is a unitsym then we also need to
             parse the identifier }
           if (c='.') then
            begin
              searchsym(actasmpattern,srsym,srsymtable);
              if assigned(srsym) and
                 (srsym.typ=unitsym) and
                 (srsym.owner.unitid=0) then
               begin
                 actasmpattern:=actasmpattern+c;
                 c:=current_scanner.asmgetchar;
                 while c in  ['A'..'Z','a'..'z','0'..'9','_','$'] do
                  begin
                    actasmpattern:=actasmpattern + upcase(c);
                    c:=current_scanner.asmgetchar;
                  end;
               end;
            end;
           actasmtoken:=AS_ID;
           exit;
         end;

       '%' : { register or modulo }
         begin
           len:=1;
           actasmpattern[len]:='%';
           c:=current_scanner.asmgetchar;
           { to be a register there must be a letter and not a number }
           if c in ['0'..'9'] then
            begin
              actasmtoken:=AS_MOD;
            end
           else
            begin
              while c in ['a'..'z','A'..'Z','0'..'9'] do
               Begin
                 inc(len);
                 actasmpattern[len]:=c;
                 c:=current_scanner.asmgetchar;
               end;
              actasmpattern[0]:=chr(len);
              uppervar(actasmpattern);
              if (actasmpattern = '%ST') and (c='(') then
               Begin
                 actasmpattern:=actasmpattern+c;
                 c:=current_scanner.asmgetchar;
                 if c in ['0'..'9'] then
                  actasmpattern:=actasmpattern + c
                 else
                  Message(asmr_e_invalid_fpu_register);
                 c:=current_scanner.asmgetchar;
                 if c <> ')' then
                  Message(asmr_e_invalid_fpu_register)
                 else
                  Begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar; { let us point to next character. }
                  end;
               end;
              if is_register(actasmpattern) then
               exit;
              Message(asmr_e_invalid_register);
              actasmtoken:=AS_NONE;
            end;
         end;

       '1'..'9': { integer number }
         begin
           len:=0;
           while c in ['0'..'9'] do
            Begin
              inc(len);
              actasmpattern[len]:=c;
              c:=current_scanner.asmgetchar;
            end;
           actasmpattern[0]:=chr(len);
           actasmpattern:=tostr(ValDecimal(actasmpattern));
           actasmtoken:=AS_INTNUM;
           exit;
         end;
       '0' : { octal,hexa,real or binary number. }
         begin
           actasmpattern:=c;
           c:=current_scanner.asmgetchar;
           case upcase(c) of
             'B': { binary }
               Begin
                 c:=current_scanner.asmgetchar;
                 while c in ['0','1'] do
                  Begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                  end;
                 actasmpattern:=tostr(ValBinary(actasmpattern));
                 actasmtoken:=AS_INTNUM;
                 exit;
               end;
             'D': { real }
               Begin
                 c:=current_scanner.asmgetchar;
                 { get ridd of the 0d }
                 if (c in ['+','-']) then
                  begin
                    actasmpattern:=c;
                    c:=current_scanner.asmgetchar;
                  end
                 else
                  actasmpattern:='';
                 while c in ['0'..'9'] do
                  Begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                  end;
                 if c='.' then
                  begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                    while c in ['0'..'9'] do
                     Begin
                       actasmpattern:=actasmpattern + c;
                       c:=current_scanner.asmgetchar;
                     end;
                    if upcase(c) = 'E' then
                     begin
                       actasmpattern:=actasmpattern + c;
                       c:=current_scanner.asmgetchar;
                       if (c in ['+','-']) then
                        begin
                          actasmpattern:=actasmpattern + c;
                          c:=current_scanner.asmgetchar;
                        end;
                       while c in ['0'..'9'] do
                        Begin
                          actasmpattern:=actasmpattern + c;
                          c:=current_scanner.asmgetchar;
                        end;
                     end;
                    actasmtoken:=AS_REALNUM;
                    exit;
                  end
                 else
                  begin
                    Message1(asmr_e_invalid_float_const,actasmpattern+c);
                    actasmtoken:=AS_NONE;
                  end;
               end;
             'X': { hexadecimal }
               Begin
                 c:=current_scanner.asmgetchar;
                 while c in ['0'..'9','a'..'f','A'..'F'] do
                  Begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                  end;
                 actasmpattern:=tostr(ValHexaDecimal(actasmpattern));
                 actasmtoken:=AS_INTNUM;
                 exit;
               end;
             '1'..'7': { octal }
               begin
                 actasmpattern:=actasmpattern + c;
                 while c in ['0'..'7'] do
                  Begin
                    actasmpattern:=actasmpattern + c;
                    c:=current_scanner.asmgetchar;
                  end;
                 actasmpattern:=tostr(ValOctal(actasmpattern));
                 actasmtoken:=AS_INTNUM;
                 exit;
               end;
             else { octal number zero value...}
               Begin
                 actasmpattern:=tostr(ValOctal(actasmpattern));
                 actasmtoken:=AS_INTNUM;
                 exit;
               end;
           end; { end case }
         end;

       '&' :
         begin
           c:=current_scanner.asmgetchar;
           actasmtoken:=AS_AND;
         end;

       '''' : { char }
         begin
           current_scanner.in_asm_string:=true;
           actasmpattern:='';
           repeat
             c:=current_scanner.asmgetchar;
             case c of
               '\' :
                 begin
                   { copy also the next char so \" is parsed correctly }
                   actasmpattern:=actasmpattern+c;
                   c:=current_scanner.asmgetchar;
                   actasmpattern:=actasmpattern+c;
                 end;
               '''' :
                 begin
                   c:=current_scanner.asmgetchar;
                   break;
                 end;
               newline:
                 Message(scan_f_string_exceeds_line);
               else
                 actasmpattern:=actasmpattern+c;
             end;
           until false;
           actasmpattern:=EscapeToPascal(actasmpattern);
           actasmtoken:=AS_STRING;
           current_scanner.in_asm_string:=false;
           exit;
         end;

       '"' : { string }
         begin
           current_scanner.in_asm_string:=true;
           actasmpattern:='';
           repeat
             c:=current_scanner.asmgetchar;
             case c of
               '\' :
                 begin
                   { copy also the next char so \" is parsed correctly }
                   actasmpattern:=actasmpattern+c;
                   c:=current_scanner.asmgetchar;
                   actasmpattern:=actasmpattern+c;
                 end;
               '"' :
                 begin
                   c:=current_scanner.asmgetchar;
                   break;
                 end;
               newline:
                 Message(scan_f_string_exceeds_line);
               else
                 actasmpattern:=actasmpattern+c;
             end;
           until false;
           actasmpattern:=EscapeToPascal(actasmpattern);
           actasmtoken:=AS_STRING;
           current_scanner.in_asm_string:=false;
           exit;
         end;

       '$' :
         begin
           actasmtoken:=AS_DOLLAR;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       ',' :
         begin
           actasmtoken:=AS_COMMA;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       '<' :
         begin
           actasmtoken:=AS_SHL;
           c:=current_scanner.asmgetchar;
           if c = '<' then
            c:=current_scanner.asmgetchar;
           exit;
         end;

       '>' :
         begin
           actasmtoken:=AS_SHL;
           c:=current_scanner.asmgetchar;
           if c = '>' then
            c:=current_scanner.asmgetchar;
           exit;
         end;

       '|' :
         begin
           actasmtoken:=AS_OR;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       '^' :
         begin
           actasmtoken:=AS_XOR;
           c:=current_scanner.asmgetchar;
           exit;
         end;


       '(' :
         begin
           actasmtoken:=AS_LPAREN;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       ')' :
         begin
           actasmtoken:=AS_RPAREN;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       ':' :
         begin
           actasmtoken:=AS_COLON;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       '+' :
         begin
           actasmtoken:=AS_PLUS;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       '-' :
         begin
           actasmtoken:=AS_MINUS;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       '*' :
         begin
           actasmtoken:=AS_STAR;
           c:=current_scanner.asmgetchar;
           exit;
         end;

       '/' :
         begin
           c:=current_scanner.asmgetchar;
           actasmtoken:=AS_SLASH;
           exit;
         end;

       '{',#13,newline,';' :
         begin
           { the comment is read by asmgetchar }
           c:=current_scanner.asmgetchar;
           firsttoken:=TRUE;
           actasmtoken:=AS_SEPARATOR;
           exit;
         end;

       else
         current_scanner.illegal_char(c);
     end;
   end;
end;


function consume(t : tasmtoken):boolean;
begin
  Consume:=true;
  if t<>actasmtoken then
   begin
     Message2(scan_f_syn_expected,token2str[t],token2str[actasmtoken]);
     Consume:=false;
   end;
  repeat
    gettoken;
  until actasmtoken<>AS_NONE;
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
      s:=s+'.'+actasmpattern;
     if not Consume(AS_ID) then
      begin
        RecoverConsume(true);
        break;
      end;
   end;
  if not GetRecordOffsetSize(s,offset,size) then
   Message(asmr_e_building_record_offset);
end;


Procedure BuildConstSymbolExpression(allowref,betweenbracket,needofs:boolean;var value:longint;var asmsym:string);
var
  hs,tempstr,expr : string;
  parenlevel,l,k : longint;
  errorflag : boolean;
  prevtok : tasmtoken;
  sym : tsym;
  srsymtable : tsymtable;
  hl  : tasmlabel;
Begin
  asmsym:='';
  value:=0;
  errorflag:=FALSE;
  tempstr:='';
  expr:='';
  parenlevel:=0;
  Repeat
    Case actasmtoken of
      AS_LPAREN:
        Begin
          { Exit if ref? }
          if allowref and (prevasmtoken in [AS_INTNUM,AS_ID]) then
           break;
          Consume(AS_LPAREN);
          expr:=expr + '(';
          inc(parenlevel);
        end;
      AS_RPAREN:
        Begin
          { end of ref ? }
          if (parenlevel=0) and betweenbracket then
           break;
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
      AS_DOLLAR:
        begin
          Consume(AS_DOLLAR);
          if actasmtoken<>AS_ID then
           Message(asmr_e_dollar_without_identifier);
        end;
      AS_STRING:
        Begin
          l:=0;
          case Length(actasmpattern) of
           1 :
            l:=ord(actasmpattern[1]);
           2 :
            l:=ord(actasmpattern[2]) + ord(actasmpattern[1]) shl 8;
           3 :
            l:=ord(actasmpattern[3]) +
               Ord(actasmpattern[2]) shl 8 + ord(actasmpattern[1]) shl 16;
           4 :
            l:=ord(actasmpattern[4]) + ord(actasmpattern[3]) shl 8 +
               Ord(actasmpattern[2]) shl 16 + ord(actasmpattern[1]) shl 24;
          else
            Message1(asmr_e_invalid_string_as_opcode_operand,actasmpattern);
          end;
          str(l, tempstr);
          expr:=expr + tempstr;
          Consume(AS_STRING);
        end;
      AS_TYPE:
        begin
          l:=0;
          Consume(AS_TYPE);
          if actasmtoken<>AS_ID then
           Message(asmr_e_type_without_identifier)
          else
           begin
             tempstr:=actasmpattern;
             Consume(AS_ID);
             if actasmtoken=AS_DOT then
              BuildRecordOffsetSize(tempstr,k,l)
             else
              begin
                searchsym(tempstr,sym,srsymtable);
                if assigned(sym) then
                 begin
                   case sym.typ of
                     varsym :
                       l:=tvarsym(sym).getsize;
                     typedconstsym :
                       l:=ttypedconstsym(sym).getsize;
                     typesym :
                       l:=ttypesym(sym).restype.def.size;
                     else
                       Message(asmr_e_wrong_sym_type);
                   end;
                 end
                else
                 Message1(sym_e_unknown_id,tempstr);
              end;
           end;
          str(l, tempstr);
          expr:=expr + tempstr;
        end;
      AS_ID:
        Begin
          hs:='';
          tempstr:=actasmpattern;
          prevtok:=prevasmtoken;
          consume(AS_ID);
          if SearchIConstant(tempstr,l) then
           begin
             str(l, tempstr);
             expr:=expr + tempstr;
           end
          else
           begin
             if is_locallabel(tempstr) then
              begin
                CreateLocalLabel(tempstr,hl,false);
                hs:=hl.name
              end
             else
              if SearchLabel(tempstr,hl,false) then
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
                         if Tprocsym(sym).procdef_count>1 then
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
                if needofs and (prevtok<>AS_DOLLAR) then
                 Message(asmr_e_need_dollar);
                if asmsym='' then
                 asmsym:=hs
                else
                 Message(asmr_e_cant_have_multiple_relocatable_symbols);
                if (expr='') or (expr[length(expr)]='+') then
                 begin
                   { don't remove the + if there could be a record field }
                   if actasmtoken<>AS_DOT then
                    delete(expr,length(expr),1);
                 end
                else
                 Message(asmr_e_only_add_relocatable_symbol);
              end;
             if actasmtoken=AS_DOT then
              begin
                BuildRecordOffsetSize(tempstr,l,k);
                str(l, tempstr);
                expr:=expr + tempstr;
              end
             else
              begin
                if (expr='') or (expr[length(expr)] in ['+','-','/','*']) then
                 delete(expr,length(expr),1);
              end;
           end;
          { check if there are wrong operator used like / or mod etc. }
          if (hs<>'') and not(actasmtoken in [AS_MINUS,AS_PLUS,AS_COMMA,AS_SEPARATOR,AS_LPAREN,AS_END]) then
           Message(asmr_e_only_add_relocatable_symbol);
        end;
      AS_END,
      AS_SEPARATOR,
      AS_COMMA:
        Begin
          break;
        end;
    else
      Begin
        { write error only once. }
        if not errorflag then
          Message(asmr_e_invalid_constant_expression);
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
end;


Function BuildConstExpression(allowref,betweenbracket:boolean): longint;
var
  l : longint;
  hs : string;
begin
  BuildConstSymbolExpression(allowref,betweenbracket,false,l,hs);
  if hs<>'' then
   Message(asmr_e_relocatable_symbol_not_allowed);
  BuildConstExpression:=l;
end;


{****************************************************************************
                                T386ATTOperand
****************************************************************************}

type
  T386ATTOperand=class(T386Operand)
    Procedure BuildOperand;override;
  private
    Procedure BuildReference;
    Procedure BuildConstant;
  end;


Procedure T386ATTOperand.BuildReference;

  procedure Consume_RParen;
  begin
    if actasmtoken <> AS_RPAREN then
     Begin
       Message(asmr_e_invalid_reference_syntax);
       RecoverConsume(true);
     end
    else
     begin
       Consume(AS_RPAREN);
       if not (actasmtoken in [AS_COMMA,AS_SEPARATOR,AS_END]) then
        Begin
          Message(asmr_e_invalid_reference_syntax);
          RecoverConsume(true);
        end;
     end;
  end;

  procedure Consume_Scale;
  var
    l : longint;
  begin
    { we have to process the scaling }
    l:=BuildConstExpression(false,true);
    if ((l = 2) or (l = 4) or (l = 8) or (l = 1)) then
     opr.ref.scalefactor:=l
    else
     Begin
       Message(asmr_e_wrong_scale_factor);
       opr.ref.scalefactor:=0;
     end;
  end;

Begin
  Consume(AS_LPAREN);
  Case actasmtoken of
    AS_INTNUM,
    AS_MINUS,
    AS_PLUS: { absolute offset, such as fs:(0x046c) }
      Begin
        { offset(offset) is invalid }
        If opr.Ref.Offset <> 0 Then
         Begin
           Message(asmr_e_invalid_reference_syntax);
           RecoverConsume(true);
         End
        Else
         Begin
           opr.Ref.Offset:=BuildConstExpression(false,true);
           Consume_RParen;
         end;
        exit;
      End;
    AS_REGISTER: { (reg ...  }
      Begin
        { Check if there is already a base (mostly ebp,esp) than this is
          not allowed,becuase it will give crashing code }
        if (opr.ref.base<>NR_NO) then
          message(asmr_e_cannot_index_relative_var);
        opr.ref.base:=actasmregister;
        Consume(AS_REGISTER);
        { can either be a register or a right parenthesis }
        { (reg)        }
        if actasmtoken=AS_RPAREN then
         Begin
           Consume_RParen;
           exit;
         end;
        { (reg,reg ..  }
        Consume(AS_COMMA);
        if actasmtoken=AS_REGISTER then
         Begin
           opr.ref.index:=actasmregister;
           Consume(AS_REGISTER);
           { check for scaling ... }
           case actasmtoken of
             AS_RPAREN:
               Begin
                 Consume_RParen;
                 exit;
               end;
             AS_COMMA:
               Begin
                 Consume(AS_COMMA);
                 Consume_Scale;
                 Consume_RParen;
               end;
           else
             Begin
               Message(asmr_e_invalid_reference_syntax);
               RecoverConsume(false);
             end;
           end; { end case }
         end
        else
         Begin
           Message(asmr_e_invalid_reference_syntax);
           RecoverConsume(false);
         end;
      end; {end case }
    AS_COMMA: { (, ...  can either be scaling, or index }
      Begin
        Consume(AS_COMMA);
        { Index }
        if (actasmtoken=AS_REGISTER) then
         Begin
           opr.ref.index:=actasmregister;
           Consume(AS_REGISTER);
           { check for scaling ... }
           case actasmtoken of
             AS_RPAREN:
               Begin
                 Consume_RParen;
                 exit;
               end;
             AS_COMMA:
               Begin
                 Consume(AS_COMMA);
                 Consume_Scale;
                 Consume_RParen;
               end;
           else
             Begin
               Message(asmr_e_invalid_reference_syntax);
               RecoverConsume(false);
             end;
           end; {end case }
         end
        { Scaling }
        else
         Begin
           Consume_Scale;
           Consume_RParen;
           exit;
         end;
      end;

  else
    Begin
      Message(asmr_e_invalid_reference_syntax);
      RecoverConsume(false);
    end;
  end;
end;


Procedure T386ATTOperand.BuildConstant;
var
  l : longint;
  tempstr : string;
begin
  BuildConstSymbolExpression(false,false,true,l,tempstr);
  if tempstr<>'' then
   begin
     opr.typ:=OPR_SYMBOL;
     opr.symofs:=l;
     opr.symbol:=objectlibrary.newasmsymbol(tempstr);
   end
  else
   begin
     opr.typ:=OPR_CONSTANT;
     opr.val:=l;
   end;
end;


Procedure T386ATTOperand.BuildOperand;
var
  tempstr,tempstr2,
  expr : string;
  l,k : longint;

  procedure AddLabelOperand(hl:tasmlabel);
  begin
    if not(actasmtoken in [AS_PLUS,AS_MINUS,AS_LPAREN]) and
       is_calljmp(actopcode) then
     begin
       opr.typ:=OPR_SYMBOL;
       opr.symbol:=hl;
     end
    else
     begin
       InitRef;
       opr.ref.symbol:=hl;
     end;
  end;

  procedure MaybeRecordOffset;
  var
    hasdot  : boolean;
    l,
    toffset,
    tsize   : longint;
  begin
    if not(actasmtoken in [AS_DOT,AS_PLUS,AS_MINUS]) then
     exit;
    l:=0;
    hasdot:=(actasmtoken=AS_DOT);
    if hasdot then
     begin
       if expr<>'' then
         begin
           BuildRecordOffsetSize(expr,toffset,tsize);
           inc(l,toffset);
           SetSize(tsize,true);
         end;
     end;
    if actasmtoken in [AS_PLUS,AS_MINUS] then
     inc(l,BuildConstExpression(true,false));
    case opr.typ of
      OPR_LOCAL :
        begin
          { don't allow direct access to fields of parameters, becuase that
            will generate buggy code. Allow it only for explicit typecasting }
          if hasdot and
             (not hastype) and
             (tvarsym(pointer(opr.ref.symbol)).owner.symtabletype=parasymtable) then
            Message(asmr_e_cannot_access_field_directly_for_parameters);
          inc(opr.localsymofs,l)
        end;
      OPR_CONSTANT :
        inc(opr.val,l);
      OPR_REFERENCE :
        inc(opr.ref.offset);
      else
        internalerror(200309221);
    end;
  end;

  function MaybeBuildReference:boolean;
  { Try to create a reference, if not a reference is found then false
    is returned }
  begin
    MaybeBuildReference:=true;
    case actasmtoken of
      AS_INTNUM,
      AS_MINUS,
      AS_PLUS:
        Begin
          opr.ref.offset:=BuildConstExpression(True,False);
          if actasmtoken<>AS_LPAREN then
            Message(asmr_e_invalid_reference_syntax)
          else
            BuildReference;
        end;
      AS_LPAREN:
        BuildReference;
      AS_ID: { only a variable is allowed ... }
        Begin
          tempstr:=actasmpattern;
          Consume(AS_ID);
          { typecasting? }
          if (actasmtoken=AS_LPAREN) and
             SearchType(tempstr) then
           begin
             hastype:=true;
             Consume(AS_LPAREN);
             tempstr2:=actasmpattern;
             Consume(AS_ID);
             Consume(AS_RPAREN);
             if not SetupVar(tempstr2,false) then
              Message1(sym_e_unknown_id,tempstr2);
           end
          else
           if not SetupVar(tempstr,false) then
            Message1(sym_e_unknown_id,tempstr);
          { record.field ? }
          if actasmtoken=AS_DOT then
           begin
             BuildRecordOffsetSize(tempstr,l,k);
             inc(opr.ref.offset,l);
           end;
          case actasmtoken of
            AS_END,
            AS_SEPARATOR,
            AS_COMMA: ;
            AS_LPAREN: BuildReference;
          else
            Begin
              Message(asmr_e_invalid_reference_syntax);
              Consume(actasmtoken);
            end;
          end; {end case }
        end;
      else
       MaybeBuildReference:=false;
    end; { end case }
  end;

const
  regsize_2_size: array[S_B..S_L] of longint = (1,2,4);
var
  tempreg : tregister;
  hl      : tasmlabel;
Begin
  expr:='';
  case actasmtoken of
    AS_LPAREN: { Memory reference or constant expression }
      Begin
        InitRef;
        BuildReference;
      end;

    AS_DOLLAR: { Constant expression  }
      Begin
        Consume(AS_DOLLAR);
        BuildConstant;
      end;

    AS_INTNUM,
    AS_MINUS,
    AS_PLUS:
      Begin
        { Constant memory offset }
        { This must absolutely be followed by (  }
        InitRef;
        opr.ref.offset:=BuildConstExpression(True,False);
        if actasmtoken<>AS_LPAREN then
          Message(asmr_e_invalid_reference_syntax)
        else
          BuildReference;
      end;

    AS_STAR: { Call from memory address }
      Begin
        Consume(AS_STAR);
        if actasmtoken=AS_REGISTER then
         begin
           opr.typ:=OPR_REGISTER;
           opr.reg:=actasmregister;
           SetSize(regsize_2_size[reg2opsize(actasmregister)],true);
           Consume(AS_REGISTER);
         end
        else
         begin
           InitRef;
           if not MaybeBuildReference then
            Message(asmr_e_syn_operand);
         end;
        { this is only allowed for call's and jmp's }
        if not is_calljmp(actopcode) then
         Message(asmr_e_syn_operand);
      end;

    AS_ID: { A constant expression, or a Variable ref.  }
      Begin
        { Local Label ? }
        if is_locallabel(actasmpattern) then
         begin
           CreateLocalLabel(actasmpattern,hl,false);
           Consume(AS_ID);
           AddLabelOperand(hl);
         end
        else
         { Check for label }
         if SearchLabel(actasmpattern,hl,false) then
          begin
            Consume(AS_ID);
            AddLabelOperand(hl);
          end
        else
         { probably a variable or normal expression }
         { or a procedure (such as in CALL ID)      }
         Begin
           { is it a constant ? }
           if SearchIConstant(actasmpattern,l) then
            Begin
              if not (opr.typ in [OPR_NONE,OPR_CONSTANT]) then
               Message(asmr_e_invalid_operand_type);
              BuildConstant;
            end
           else
            begin
              InitRef;
              expr:=actasmpattern;
              Consume(AS_ID);
              { typecasting? }
              if (actasmtoken=AS_LPAREN) and
                 SearchType(expr) then
               begin
                 hastype:=true;
                 Consume(AS_LPAREN);
                 tempstr:=actasmpattern;
                 Consume(AS_ID);
                 Consume(AS_RPAREN);
                 if SetupVar(tempstr,false) then
                  begin
                    MaybeRecordOffset;
                    { add a constant expression? }
                    if (actasmtoken=AS_PLUS) then
                     begin
                       l:=BuildConstExpression(true,false);
                       if opr.typ=OPR_CONSTANT then
                        inc(opr.val,l)
                       else
                        inc(opr.ref.offset,l);
                     end
                  end
                 else
                  Message1(sym_e_unknown_id,tempstr);
               end
              else
               begin
                 if SetupVar(expr,false) then
                  begin
                    MaybeRecordOffset;
                    { add a constant expression? }
                    if (actasmtoken=AS_PLUS) then
                     begin
                       l:=BuildConstExpression(true,false);
                       case opr.typ of
                         OPR_CONSTANT :
                           inc(opr.val,l);
                         OPR_LOCAL :
                           inc(opr.localsymofs,l);
                         OPR_REFERENCE :
                           inc(opr.ref.offset,l);
                         else
                           internalerror(200309202);
                       end;
                     end
                  end
                 else
                  Begin
                    { look for special symbols ... }
                    if expr = '__RESULT' then
                      SetUpResult
                    else
                     if expr = '__SELF' then
                      SetupSelf
                    else
                     if expr = '__OLDEBP' then
                      SetupOldEBP
                    else
                      { check for direct symbolic names   }
                      { only if compiling the system unit }
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
                      Message1(sym_e_unknown_id,expr);
                  end;
               end;
            end;
         end;
        { Do we have a indexing reference, then parse it also }
        if actasmtoken=AS_LPAREN then
         begin
           if (opr.typ=OPR_CONSTANT) then
            begin
              l:=opr.val;
              opr.typ:=OPR_REFERENCE;
              Fillchar(opr.ref,sizeof(treference),0);
              opr.Ref.Offset:=l;
            end;
           BuildReference;
         end;
      end;

    AS_REGISTER: { Register, a variable reference or a constant reference  }
      Begin
        { save the type of register used. }
        tempreg:=actasmregister;
        Consume(AS_REGISTER);
        if actasmtoken = AS_COLON then
         Begin
           Consume(AS_COLON);
           InitRef;
           opr.ref.segment:=tempreg;
           { This must absolutely be followed by a reference }
           if not MaybeBuildReference then
            Begin
              Message(asmr_e_invalid_seg_override);
              Consume(actasmtoken);
            end;
         end
        { Simple register  }
        else if (actasmtoken in [AS_END,AS_SEPARATOR,AS_COMMA]) then
         Begin
           if not (opr.typ in [OPR_NONE,OPR_REGISTER]) then
             Message(asmr_e_invalid_operand_type);
           opr.typ:=OPR_REGISTER;
           opr.reg:=tempreg;
           SetSize(tcgsize2size[cg.reg_cgsize(opr.reg)],true);
         end
        else
         Message(asmr_e_syn_operand);
      end;
    AS_END,
    AS_SEPARATOR,
    AS_COMMA: ;
  else
    Begin
      Message(asmr_e_syn_operand);
      Consume(actasmtoken);
    end;
  end; { end case }
end;

{*****************************************************************************
                                T386ATTInstruction
*****************************************************************************}

type
  T386AttInstruction=class(T386Instruction)
    procedure InitOperands;override;
    procedure BuildOpcode;override;
  end;

procedure T386AttInstruction.InitOperands;
var
  i : longint;
begin
  OpOrder:=op_att;
  for i:=1to max_operands do
   Operands[i]:=T386AttOperand.Create;
end;


Procedure T386AttInstruction.BuildOpCode;
var
  operandnum : longint;
  PrefixOp,OverrideOp: tasmop;
Begin
  PrefixOp:=A_None;
  OverrideOp:=A_None;
  { prefix seg opcode / prefix opcode }
  repeat
    if is_prefix(actopcode) then
     begin
       PrefixOp:=ActOpcode;
       opcode:=ActOpcode;
       condition:=ActCondition;
       opsize:=ActOpsize;
       ConcatInstruction(curlist);
       Consume(AS_OPCODE);
     end
    else
     if is_override(actopcode) then
      begin
        OverrideOp:=ActOpcode;
        opcode:=ActOpcode;
        condition:=ActCondition;
        opsize:=ActOpsize;
        ConcatInstruction(curlist);
        Consume(AS_OPCODE);
      end
    else
     break;
    { allow for newline as in gas styled syntax }
    while actasmtoken=AS_SEPARATOR do
      Consume(AS_SEPARATOR);
  until (actasmtoken<>AS_OPCODE);
  { opcode }
  if (actasmtoken <> AS_OPCODE) then
   Begin
     Message(asmr_e_invalid_or_missing_opcode);
     RecoverConsume(true);
     exit;
   end;
  { Fill the instr object with the current state }
  Opcode:=ActOpcode;
  condition:=ActCondition;
  opsize:=ActOpsize;
  { Valid combination of prefix/override and instruction ?  }
  if (prefixop<>A_NONE) and (NOT CheckPrefix(PrefixOp,actopcode)) then
    Message1(asmr_e_invalid_prefix_and_opcode,actasmpattern);
  if (overrideop<>A_NONE) and (NOT CheckOverride(OverrideOp,ActOpcode)) then
    Message1(asmr_e_invalid_override_and_opcode,actasmpattern);
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
        Begin
          if operandnum > Max_Operands then
           Message(asmr_e_too_many_operands)
          else
           Inc(operandnum);
          Consume(AS_COMMA);
        end;
      AS_SEPARATOR,
      AS_END : { End of asm operands for this opcode  }
        begin
          break;
        end;
    else
      Operands[operandnum].BuildOperand;
    end; { end case }
  until false;
  Ops:=operandnum;
end;



Procedure BuildConstant(maxvalue: longint);
var
 asmsym,
 expr: string;
 value : longint;
Begin
  Repeat
    Case actasmtoken of
      AS_STRING:
        Begin
          expr:=actasmpattern;
          if length(expr) > 1 then
           Message(asmr_e_string_not_allowed_as_const);
          Consume(AS_STRING);
          Case actasmtoken of
            AS_COMMA: Consume(AS_COMMA);
            AS_END,
            AS_SEPARATOR: ;
          else
            Message(asmr_e_invalid_string_expression);
          end; { end case }
          ConcatString(curlist,expr);
        end;
      AS_INTNUM,
      AS_PLUS,
      AS_MINUS,
      AS_LPAREN,
      AS_NOT,
      AS_ID :
        Begin
          BuildConstSymbolExpression(false,false,false,value,asmsym);
          if asmsym<>'' then
           begin
             if maxvalue<>longint($ffffffff) then
              Message(asmr_w_32bit_const_for_address);
             ConcatConstSymbol(curlist,asmsym,value)
           end
          else
           ConcatConstant(curlist,value,maxvalue);
        end;
      AS_COMMA:
        Consume(AS_COMMA);
      AS_END,
      AS_SEPARATOR:
        break;
      else
        begin
          Message(asmr_e_syn_constant);
          RecoverConsume(false);
        end
   end; { end case }
 Until false;
end;


Procedure BuildRealConstant(typ : tfloattype);
var
  expr : string;
  r : bestreal;
  code : integer;
  negativ : boolean;
  errorflag: boolean;
Begin
  errorflag:=FALSE;
  Repeat
    negativ:=false;
    expr:='';
    if actasmtoken=AS_PLUS then
      Consume(AS_PLUS)
    else
     if actasmtoken=AS_MINUS then
      begin
        negativ:=true;
        consume(AS_MINUS);
      end;
    Case actasmtoken of
      AS_INTNUM:
        Begin
          expr:=actasmpattern;
          Consume(AS_INTNUM);
          if negativ then
           expr:='-'+expr;
          val(expr,r,code);
          if code<>0 then
           Begin
             r:=0;
             Message(asmr_e_invalid_float_expr);
           End;
          ConcatRealConstant(curlist,r,typ);
        end;
      AS_REALNUM:
        Begin
          expr:=actasmpattern;
          Consume(AS_REALNUM);
          { in ATT syntax you have 0d in front of the real }
          { should this be forced ?  yes i think so, as to }
          { conform to gas as much as possible.            }
          if (expr[1]='0') and (upper(expr[2])='D') then
           Delete(expr,1,2);
          if negativ then
           expr:='-'+expr;
          val(expr,r,code);
          if code<>0 then
           Begin
             r:=0;
             Message(asmr_e_invalid_float_expr);
           End;
          ConcatRealConstant(curlist,r,typ);
        end;
      AS_COMMA:
        begin
          Consume(AS_COMMA);
        end;
      AS_END,
      AS_SEPARATOR:
        begin
          break;
        end;
   else
     Begin
       Consume(actasmtoken);
       if not errorflag then
        Message(asmr_e_invalid_float_expr);
       errorflag:=TRUE;
     end;
   end;
 Until false;
end;


Procedure BuildStringConstant(asciiz: boolean);
var
  expr: string;
  errorflag : boolean;
Begin
  errorflag:=FALSE;
  Repeat
    Case actasmtoken of
      AS_STRING:
        Begin
          expr:=actasmpattern;
          if asciiz then
            expr:=expr+#0;
          ConcatPasString(curlist,expr);
          Consume(AS_STRING);
        end;
      AS_COMMA:
        begin
          Consume(AS_COMMA);
        end;
      AS_END,
      AS_SEPARATOR:
        begin
          break;
        end;
   else
     Begin
       Consume(actasmtoken);
       if not errorflag then
        Message(asmr_e_invalid_string_expression);
       errorflag:=TRUE;
     end;
   end;
 Until false;
end;


Function Assemble: tnode;
Var
  hl         : tasmlabel;
  commname   : string;
  lasTSec    : TSection;
  l1,l2      : longint;
  instr      : T386ATTInstruction;
Begin
  Message1(asmr_d_start_reading,'AT&T');
  firsttoken:=TRUE;
  { sets up all opcode and register tables in uppercase }
  if not _asmsorted then
   Begin
     SetupTables;
     _asmsorted:=TRUE;
   end;
  curlist:=TAAsmoutput.Create;
  lasTSec:=sec_code;
  { setup label linked list }
  LocalLabelList:=TLocalLabelList.Create;
  { start tokenizer }
  c:=current_scanner.asmgetchar;
  gettoken;
  { main loop }
  repeat
    case actasmtoken of
      AS_LLABEL:
        Begin
          if CreateLocalLabel(actasmpattern,hl,true) then
            ConcatLabel(curlist,hl);
          Consume(AS_LLABEL);
        end;

      AS_LABEL:
        Begin
          if SearchLabel(upper(actasmpattern),hl,true) then
           ConcatLabel(curlist,hl)
          else
           Message1(asmr_e_unknown_label_identifier,actasmpattern);
          Consume(AS_LABEL);
        end;

      AS_DW:
        Begin
          Consume(AS_DW);
          BuildConstant($ffff);
        end;

      AS_DATA:
        Begin
          curList.Concat(Tai_section.Create(sec_data));
          lasTSec:=sec_data;
          Consume(AS_DATA);
        end;

      AS_TEXT:
        Begin
          curList.Concat(Tai_section.Create(sec_code));
          lasTSec:=sec_code;
          Consume(AS_TEXT);
        end;

      AS_DB:
        Begin
          Consume(AS_DB);
          BuildConstant($ff);
        end;

      AS_DD:
        Begin
          Consume(AS_DD);
          BuildConstant(longint($ffffffff));
        end;

      AS_DQ:
        Begin
          Consume(AS_DQ);
          BuildRealConstant(s64comp);
        end;

      AS_SINGLE:
        Begin
          Consume(AS_SINGLE);
          BuildRealConstant(s32real);
        end;

      AS_DOUBLE:
        Begin
          Consume(AS_DOUBLE);
          BuildRealConstant(s64real);
        end;

      AS_EXTENDED:
        Begin
          Consume(AS_EXTENDED);
          BuildRealConstant(s80real);
        end;

      AS_GLOBAL:
        Begin
          Consume(AS_GLOBAL);
          if actasmtoken=AS_ID then
            ConcatPublic(curlist,actasmpattern);
          Consume(AS_ID);
          if actasmtoken<>AS_SEPARATOR then
           Consume(AS_SEPARATOR);
        end;

      AS_ALIGN:
        Begin
          Consume(AS_ALIGN);
          l1:=BuildConstExpression(false,false);
          if (target_info.system in [system_i386_GO32V2]) then
            begin
               l2:=1;
               if (l1>=0) and (l1<=16) then
                 while (l1>0) do
                   begin
                     l2:=2*l2;
                     dec(l1);
                   end;
               l1:=l2;
            end;
          ConcatAlign(curlist,l1);
          Message(asmr_n_align_is_target_specific);
          if actasmtoken<>AS_SEPARATOR then
           Consume(AS_SEPARATOR);
        end;

      AS_BALIGN:
        Begin
          Consume(AS_BALIGN);
          ConcatAlign(curlist,BuildConstExpression(false,false));
          if actasmtoken<>AS_SEPARATOR then
           Consume(AS_SEPARATOR);
        end;

      AS_P2ALIGN:
        Begin
          Consume(AS_P2ALIGN);
          l1:=BuildConstExpression(false,false);
          l2:=1;
          if (l1>=0) and (l1<=16) then
            while (l1>0) do
              begin
                 l2:=2*l2;
                 dec(l1);
              end;
          l1:=l2;
          ConcatAlign(curlist,l1);
          if actasmtoken<>AS_SEPARATOR then
           Consume(AS_SEPARATOR);
        end;

      AS_ASCIIZ:
        Begin
          Consume(AS_ASCIIZ);
          BuildStringConstant(TRUE);
        end;

      AS_ASCII:
        Begin
          Consume(AS_ASCII);
          BuildStringConstant(FALSE);
        end;

      AS_LCOMM:
        Begin
          Consume(AS_LCOMM);
          commname:=actasmpattern;
          Consume(AS_ID);
          Consume(AS_COMMA);
          ConcatLocalBss(commname,BuildConstExpression(false,false));
          if actasmtoken<>AS_SEPARATOR then
           Consume(AS_SEPARATOR);
        end;

      AS_COMM:
        Begin
          Consume(AS_COMM);
          commname:=actasmpattern;
          Consume(AS_ID);
          Consume(AS_COMMA);
          ConcatGlobalBss(commname,BuildConstExpression(false,false));
          if actasmtoken<>AS_SEPARATOR then
           Consume(AS_SEPARATOR);
        end;
      AS_OPCODE:
        Begin
          instr:=T386ATTInstruction.Create;
          instr.BuildOpcode;
          instr.AddReferenceSizes;
          instr.SetInstructionOpsize;
          instr.CheckOperandSizes;
          instr.ConcatInstruction(curlist);
          instr.Free;
        end;

      AS_SEPARATOR:
        Begin
          Consume(AS_SEPARATOR);
        end;

      AS_END:
        begin
          break; { end assembly block }
        end;

      else
        Begin
          Message(asmr_e_syntax_error);
          RecoverConsume(false);
        end;
    end;
  until false;
  { Check LocalLabelList }
  LocalLabelList.CheckEmitted;
  LocalLabelList.Free;
  { are we back in the code section? }
  if lasTSec<>sec_code then
   begin
     Message(asmr_w_assembler_code_not_returned_to_text);
     curList.Concat(Tai_section.Create(sec_code));
   end;
  { Return the list in an asmnode }
  assemble:=casmnode.create(curlist);
  Message1(asmr_d_finish_reading,'AT&T');
end;


{*****************************************************************************
                                     Initialize
*****************************************************************************}

const
  asmmode_i386_att_info : tasmmodeinfo =
          (
            id    : asmmode_i386_att;
            idtxt : 'ATT'
          );

initialization
  RegisterAsmMode(asmmode_i386_att_info);

finalization
  if assigned(iasmops) then
    iasmops.Free;

end.
{
  $Log$
  Revision 1.47  2003-09-23 17:56:06  peter
    * locals and paras are allocated in the code generation
    * tvarsym.localloc contains the location of para/local when
      generating code for the current procedure

  Revision 1.46  2003/09/03 15:55:01  peter
    * NEWRA branch merged

  Revision 1.45.2.2  2003/08/31 15:46:26  peter
    * more updates for tregister

  Revision 1.45.2.1  2003/08/28 18:35:08  peter
    * tregister changed to cardinal

  Revision 1.45  2003/05/30 23:57:08  peter
    * more sparc cleanup
    * accumulator removed, splitted in function_return_reg (called) and
      function_result_reg (caller)

  Revision 1.44  2003/05/22 21:32:29  peter
    * removed some unit dependencies

  Revision 1.43  2003/04/30 15:45:35  florian
    * merged more x86-64/i386 code

  Revision 1.42  2003/04/25 12:04:31  florian
    * merged agx64att and ag386att to x86/agx86att

  Revision 1.41  2003/04/21 20:05:10  peter
    * removed some ie checks

  Revision 1.40  2003/03/18 18:15:53  peter
    * changed reg2opsize to function

  Revision 1.39  2003/02/20 15:52:58  pierre
   * fix a range check error

  Revision 1.38  2003/02/19 22:00:16  daniel
    * Code generator converted to new register notation
    - Horribily outdated todo.txt removed

  Revision 1.37  2003/02/03 22:47:14  daniel
    - Removed reg_2_opsize array

  Revision 1.36  2003/01/08 18:43:57  daniel
   * Tregister changed into a record

  Revision 1.35  2002/12/14 15:02:03  carl
    * maxoperands -> max_operands (for portability in rautils.pas)
    * fix some range-check errors with loadconst
    + add ncgadd unit to m68k
    * some bugfix of a_param_reg with LOC_CREFERENCE

  Revision 1.34  2002/12/01 22:08:34  carl
    * some small cleanup (remove some specific operators which are not supported)

  Revision 1.33  2002/11/30 23:16:39  carl
    - removed unused message

  Revision 1.32  2002/11/15 01:58:58  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.31  2002/09/03 16:26:28  daniel
    * Make Tprocdef.defs protected

  Revision 1.30  2002/08/13 18:01:52  carl
    * rename swatoperands to swapoperands
    + m68k first compilable version (still needs a lot of testing):
        assembler generator, system information , inline
        assembler reader.

  Revision 1.29  2002/08/12 15:08:42  carl
    + stab register indexes for powerpc (moved from gdb to cpubase)
    + tprocessor enumeration moved to cpuinfo
    + linker in target_info is now a class
    * many many updates for m68k (will soon start to compile)
    - removed some ifdef or correct them for correct cpu

  Revision 1.28  2002/08/11 14:32:31  peter
    * renamed current_library to objectlibrary

  Revision 1.27  2002/08/11 13:24:17  peter
    * saving of asmsymbols in ppu supported
    * asmsymbollist global is removed and moved into a new class
      tasmlibrarydata that will hold the info of a .a file which
      corresponds with a single module. Added librarydata to tmodule
      to keep the library info stored for the module. In the future the
      objectfiles will also be stored to the tasmlibrarydata class
    * all getlabel/newasmsymbol and friends are moved to the new class

  Revision 1.26  2002/07/26 21:15:44  florian
    * rewrote the system handling

  Revision 1.25  2002/07/01 18:46:34  peter
    * internal linker
    * reorganized aasm layer

  Revision 1.24  2002/05/18 13:34:25  peter
    * readded missing revisions

  Revision 1.23  2002/05/16 19:46:52  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.21  2002/04/15 19:12:09  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables
  * move several constants from cpubase to their specific units
    (where they are used)
  + att_Reg2str -> gas_reg2str
  + int_reg2str -> std_reg2str

  Revision 1.20  2002/04/14 17:01:52  carl
  + att_reg2str -> gas_reg2str

  Revision 1.19  2002/04/04 19:06:13  peter
    * removed unused units
    * use tlocation.size in cg.a_*loc*() routines

  Revision 1.18  2002/04/02 17:11:39  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.17  2002/03/28 20:48:25  carl
  - remove go32v1 support

  Revision 1.16  2002/01/24 18:25:53  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

}
