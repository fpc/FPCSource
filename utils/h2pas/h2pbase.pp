(*
    Copyright (c) 1998-2000 by Florian Klaempfl

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

 ****************************************************************************)

unit h2pbase;

{$modeswitch result}
{$message TODO: warning Unit types is only needed due to issue 7910}

interface

uses
  SysUtils, classes,
  h2poptions,scan,h2pconst,h2plexlib,h2pyacclib, scanbase,h2pout,h2ptypes;

type
  YYSTYPE = presobject;


var
  s,TN,PN  : String;


(* $ define yydebug
 compile with -dYYDEBUG to get debugging info *)




procedure yymsg(const msg : string);

function ellipsisarg : presobject;

function HandleErrorDecl(e1,e2 : presobject) : presobject;
Function HandleDeclarationStatement(decl,type_spec,modifier_spec,decllist_spec,block_spec : presobject) : presobject;
Function HandleDeclarationSysTrap(decl,type_spec,modifier_spec,decllist_spec,sys_trap : presobject) : presobject;
function HandleSpecialType(aType: presobject) : presobject;
function HandleTypedef(type_spec,dec_modifier,declarator,arg_decl_list: presobject) : presobject;
function HandleTypedefList(type_spec,dec_modifier,declarator_list: presobject) : presobject;
function HandleStructDef(dname1,dname2 : presobject) : presobject;
function HandleSimpleTypeDef(tname : presobject) : presobject;

function HandleDeclarator(aTyp : ttyp; aright: presobject): presobject;
function HandleDeclarator2(aTyp : ttyp; aleft,aright: presobject): presobject;
function HandleSizedDeclarator(psym,psize : presobject) : presobject;
function HandleSizedPointerDeclarator(psym,psize : presobject) : presobject;
function HandleSizeOverrideDeclarator(psize,psym : presobject) : presobject;
function HandleDefaultDeclarator(psym,pdefault : presobject) : presobject;
function HandleArgList(aEl,aList : PResObject) : PResObject;
function HandlePointerArgDeclarator(ptype, psym : presobject): presobject;
function HandlePointerAbstractDeclarator(psym : presobject): presobject;
function HandlePointerAbstractListDeclarator(psym,plist : presobject): presobject;
function HandleDeclarationList(plist,pelem : presobject) : presobject;
function handleSpecialSignedType(aType : presobject) : presobject;
function handleSpecialUnSignedType(aType : presobject) : presobject;
function handleArrayDecl(aType : presobject) : presobject;
function handleSizedArrayDecl(aType,aSizeExpr: presobject): presobject;
function handleFuncNoArg(aType: presobject): presobject;
function handleFuncExpr(aType,aList: presobject): presobject;
function handlePointerType(aType,aPointer,aSize : presobject): presobject;
// Returns the cast of aExpr to a pointer to aType, one pointer level for each * in aStars.
function HandlePointerCast(aType,aStars,aExpr : presobject): presobject;
function HandleUnaryDefExpr(aExpr : presobject) : presobject;
function HandleTernary(expr,colonexpr : presobject) : presobject;
// Returns the division aLeft/aRight: div unless an operand is a floating point value.
function HandleDivision(aLeft,aRight : presobject) : presobject;
// Returns aName * aRight with aName as leftmost operand of the operators in aRight that bind as weak or weaker.
function HandleNamedProduct(aName,aRight : presobject) : presobject;

// Macros
function HandleDefineMacro(dname,enum_list,para_def_expr: presobject) : presobject;
function HandleDefineConst(dname,def_expr: presobject) : presobject;
function HandleDefine(dname : presobject) : presobject;
Function CheckWideString(S : String) : presobject;
function CheckUnderScore(pdecl : presobject) : presobject;

Function NewCType(aCType,aPascalType : String) : PresObject;

Implementation

function HandleTernary(expr,colonexpr : presobject) : presobject;

begin
  colonexpr^.p1:=expr;
  Result:=colonexpr;
  inc(if_nb);
  result^.p:=strpnew('if_local'+str(if_nb));
end;


// Returns true when the expression aExpr contains a floating point literal or a cast to a floating point type.
function IsFloatExpr(aExpr : presobject) : boolean;

var
  lStr : string;

begin
  Result:=false;
  if not assigned(aExpr) then
    exit;
  case aExpr^.typ of
    t_id :
      begin
      lStr:=aExpr^.str;
      Result:=(lStr<>'') and (lStr[1] in ['0'..'9']) and ((pos('.',lStr)>0) or (pos('e',lStr)>0) or (pos('E',lStr)>0));
      end;
    t_typespec :
      Result:=(assigned(aExpr^.p1) and (aExpr^.p1^.typ=t_id)
               and ((aExpr^.p1^.str=FLOAT_STR) or (aExpr^.p1^.str=DOUBLE_STR) or (aExpr^.p1^.str=EXTENDED_STR)
                    or (aExpr^.p1^.str=cfloat_STR) or (aExpr^.p1^.str=cdouble_STR) or (aExpr^.p1^.str=clongdouble_STR)))
              or IsFloatExpr(aExpr^.p2);
    t_bop :
      Result:=IsFloatExpr(aExpr^.p1) or IsFloatExpr(aExpr^.p2);
    t_preop,
    t_exprlist :
      Result:=IsFloatExpr(aExpr^.p1);
  end;
end;


function HandleDivision(aLeft,aRight : presobject) : presobject;

begin
  if IsFloatExpr(aLeft) or IsFloatExpr(aRight) then
    Result:=NewBinaryOp('/',aLeft,aRight)
  else
    Result:=NewBinaryOp(' div ',aLeft,aRight);
end;


Function NewCType(aCType,aPascalType : String) : PresObject;

begin
  if UseCTypesUnit then
    Result:=NewIntID(aCType)
  else
    result:=NewIntID(aPascalType);
end;

function HandleUnaryDefExpr(aExpr : presobject) : presobject;

begin
  if aExpr^.typ=t_funexprlist then
    Result:=aExpr
  else
    Result:=NewType2(t_exprlist,aExpr,nil);
  (* if here is a type specifier we know the return type *)
  if (aExpr^.typ=t_typespec) then
    Result^.p3:=aExpr^.p1^.get_copy;
end;

function handleSpecialSignedType(aType : presobject) : presobject;

var
  hp : presobject;
  tc,tp : string;

begin
  tp:='';
  Result:=aType;
  hp:=result;
  if not Assigned(HP) then
    exit;
  tc:=strpas(hp^.p);
  if UseCTypesUnit then
    Case tc of
      cint_STR: tp:=csint_STR;
      cshort_STR: tp:=csshort_STR;
      cchar_STR: tp:=cschar_STR;
      clong_STR: tp:=cslong_STR;
      clonglong_STR: tp:=cslonglong_STR;
      cint8_STR: tp:=cint8_STR;
      cint16_STR: tp:=cint16_STR;
      cint32_STR: tp:=cint32_STR;
      cint64_STR: tp:=cint64_STR;
    else
      tp:='';
    end
  else
    case tc of
      UINT_STR: tp:=INT_STR;
      USHORT_STR: tp:=SHORT_STR;
      USMALL_STR: tp:=SMALL_STR;
      // UCHAR_STR: tp:=CHAR_STR; identical to USHORT_STR....
      CHAR_STR: tp:=SHORT_STR;
      ANSICHAR_STR: tp:=SHORT_STR;
      QWORD_STR: tp:=INT64_STR;
    else
      tp:='';
    end;
  if tp<>'' then
    hp^.setstr(tp);
end;

function handleSpecialUnSignedType(aType : presobject) : presobject;

var
  hp : presobject;
  tc,tp : string;

begin
  hp:=aType;
  Result:=hp;
  if Not assigned(hp) then
    exit;
  tp:='';
  tc:=strpas(hp^.p);
  if UseCTypesUnit then
    case tc of
      cint_STR: tp:=cuint_STR;
      cshort_STR: tp:=cushort_STR;
      cchar_STR : tp:=cuchar_STR;
      clong_STR : tp:=culong_STR;
      clonglong_STR : tp:=culonglong_STR;
      cint8_STR : tp:=cuint8_STR;
      cint16_STR : tp:=cuint16_STR;
      cint32_STR : tp:=cuint32_STR;
      cint64_STR : tp:=cuint64_STR;
    else
      tp:='';
    end
  else
    case tc of
      INT_STR : tp:=UINT_STR;
      SHORT_STR : tp:=USHORT_STR;
      SMALL_STR : tp:=USMALL_STR;
      CHAR_STR :  tp:=UCHAR_STR;
      ANSICHAR_STR : tp:=UCHAR_STR;
      INT64_STR : tp:=QWORD_STR;
    else
      tp:='';
  end;
  if tp<>'' then
    hp^.setstr(tp);
end;

function handleSizedArrayDecl(aType,aSizeExpr: presobject): presobject;

var
  hp : presobject;
begin
  hp:=aType;
  result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType2(t_arraydef,nil,aSizeExpr);
end;

function handleFuncNoArg(aType: presobject): presobject;
var
  hp : presobject;
begin
  hp:=aType;
  Result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType2(t_procdef,nil,nil);
end;

function handleFuncExpr(aType, aList: presobject): presobject;

var
  hp : presobject;

begin
  hp:=NewType1(t_exprlist,aType);
  Result:=NewType3(t_funexprlist,hp,aList,nil);
end;

function HandlePointerCast(aType,aStars,aExpr : presobject): presobject;

var
  lType : presobject;
  lLevel : integer;

begin
  lType:=aType;
  for lLevel:=1 to aStars^.strlength do
    lType:=NewType1(t_pointerdef,lType);
  dispose(aStars,done);
  Result:=NewType2(t_typespec,lType,aExpr);
end;


function handlePointerType(aType, aPointer, aSize: presobject): presobject;

var
  hp : presobject;

begin
  if assigned(aSize) then
    begin
    if not stripinfo then
      emitignore(aSize);
    dispose(aSize,done);
    write_type_specifier(outfile,aType);
    emitwriteln(' ignored *)');
    end;
  hp:=NewType1(t_pointerdef,aType);
  Result:=NewType2(t_typespec,hp,aPointer);
end;

function handleArrayDecl(aType: presobject): presobject;
var
  hp : presobject;
begin
  (* this is translated into a pointer *)
  hp:=aType;
  Result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType1(t_pointerdef,nil);
end;

function HandlePointerAbstractDeclarator(psym: presobject): presobject;
var
  hp : presobject;
begin
  hp:=psym;
  Result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType1(t_pointerdef,nil);
end;

function HandlePointerAbstractListDeclarator(psym, plist: presobject
  ): presobject;
var
  hp : presobject;
begin
  hp:=psym;
  result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType2(t_procdef,nil,plist);
end;

function HandleDeclarationList(plist,pelem : presobject) : presobject;

var
  hp : presobject;

begin
  if not assigned(plist) then
    begin
    Result:=NewType1(t_declist,pelem);
    exit;
    end;
  hp:=plist;
  result:=hp;
  while assigned(hp^.next) do
    hp:=hp^.next;
  hp^.next:=NewType1(t_declist,pelem);
end;

function HandleSizedDeclarator(psym,psize : presobject) : presobject;

var
  hp : presobject;

begin
  hp:=NewType1(t_size_specifier,psize);
  Result:=NewType3(t_dec,nil,psym,hp);
end;


function HandleDefaultDeclarator(psym,pdefault : presobject) : presobject;

var
  hp : presobject;

begin
  EmitIgnoreDefault(psym);
  hp:=NewType1(t_default_value,pdefault);
  HandleDefaultDeclarator:=NewType3(t_dec,nil,psym,hp);
end;

function HandleArgList(aEl, aList: PResObject): PResObject;
begin
  Result:=NewType2(t_arglist,aEl,nil);
  Result^.next:=aList;
end;

function HandlePointerArgDeclarator(ptype, psym : presobject): presobject;

var
  hp : presobject;
begin
  (* type_specifier STAR declarator *)
  hp:=NewType1(t_pointerdef,ptype);
  Result:=NewType2(t_arg,hp,psym);
end;

function HandleSizedPointerDeclarator(psym, psize: presobject): presobject;

var
  hp : presobject;

begin
  emitignore(psize);
  dispose(psize,done);
  hp:=psym;
  Result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType1(t_pointerdef,nil);
end;

function HandleSizeOverrideDeclarator(psize,psym : presobject) : presobject;

var
  hp : presobject;
begin
  EmitIgnore(psize);
  dispose(psize,done);
  hp:=psym;
  HandleSizeOverrideDeclarator:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType1(t_pointerdef,nil);
end;

function HandleDeclarator2(aTyp : ttyp; aleft,aright: presobject): presobject;

var
  hp : presobject;

begin
  hp:=aLeft;
  result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType2(aTyp,nil,aRight);
end;


function HandleDeclarator(aTyp : ttyp; aright: presobject): presobject;

var
  hp : presobject;

begin
  hp:=aright;
  Result:=hp;
  while assigned(hp^.p1) do
    hp:=hp^.p1;
  hp^.p1:=NewType1(atyp,nil);
end;

// Returns the Pascal literal for the body aBody of a C string or character literal, with escapes as character codes.
function CStringToPascal(const aBody : AnsiString) : AnsiString;

var
  lResult : AnsiString;
  lQuoted : boolean;
  i, lCode, lDigits : integer;

  procedure AddChar(c : char);

  begin
    if not lQuoted then
      lResult:=lResult+'''';
    lQuoted:=true;
    if c='''' then
      lResult:=lResult+''''''
    else
      lResult:=lResult+c;
  end;

  procedure AddCode(aCode : integer);

  begin
    if lQuoted then
      lResult:=lResult+'''';
    lQuoted:=false;
    lResult:=lResult+'#'+IntToStr(aCode);
  end;

begin
  lResult:='';
  lQuoted:=false;
  i:=1;
  while i<=length(aBody) do
    begin
    if (aBody[i]<>'\') or (i=length(aBody)) then
      AddChar(aBody[i])
    else
      begin
      inc(i);
      case aBody[i] of
        'n' : AddCode(10);
        't' : AddCode(9);
        'r' : AddCode(13);
        'a' : AddCode(7);
        'b' : AddCode(8);
        'f' : AddCode(12);
        'v' : AddCode(11);
        'e' : AddCode(27);
        '0'..'7' :
          begin
          lCode:=0;
          lDigits:=0;
          while (i<=length(aBody)) and (lDigits<3) and (aBody[i] in ['0'..'7']) do
            begin
            lCode:=lCode*8+ord(aBody[i])-ord('0');
            inc(i);
            inc(lDigits);
            end;
          dec(i);
          AddCode(lCode and 255);
          end;
        'x' :
          begin
          lCode:=0;
          lDigits:=0;
          while (i<length(aBody)) and (lDigits<2) and (aBody[i+1] in ['0'..'9','a'..'f','A'..'F']) do
            begin
            inc(i);
            lCode:=lCode*16+StrToInt('$'+aBody[i]);
            inc(lDigits);
            end;
          AddCode(lCode);
          end;
      else
        AddChar(aBody[i]);
      end;
      end;
    inc(i);
    end;
  if lQuoted then
    lResult:=lResult+'''';
  if lResult='' then
    lResult:='''''';
  Result:=lResult;
end;


function CheckWideString(S: String): presobject;

begin
  if Win32headers and (s[1]='L') then
    delete(s,1,1);
  CheckWideString:=NewID(CStringToPascal(copy(s,2,length(s)-2)));
end;

function CheckUnderScore(pdecl: presobject): presobject;

var
  tn : string;
  len : integer;

begin
  Result:=pdecl;
  tn:=result^.str;
  len:=length(tn);
  if removeunderscore and (len>1) and (tn[1]='_') then
   result^.setstr(Copy(tn,2,len-1));
end;

function yylex : Integer;
begin
  yylex:=scan.yylex;
  line_no:=yylineno;
end;

(* writes an argument list, where p is t_arglist *)

procedure yymsg(const msg : string);
begin
  writeln('line ',line_no,': ',msg);
end;

function ellipsisarg : presobject;

begin
  ellipsisarg:=new(presobject,init_two(t_arg,nil,nil));
end;


// Writes the calling convention of a function declaration: stdcall for no_pop, cdecl when external or for -P.
procedure WriteCallingConvention(aIsExtern : boolean);

begin
  if no_pop then
    write(outfile,';stdcall')
  else if aIsExtern or createdynlib then
    write(outfile,';cdecl');
end;


// Returns true when the argument aArg (t_arg) is a function pointer: its declarator is a pointer to a procdef.
function IsProcVarArg(aArg : presobject) : boolean;

begin
  Result:=assigned(aArg) and assigned(aArg^.p1) and assigned(aArg^.p2)
          and (aArg^.p2^.typ=t_dec) and assigned(aArg^.p2^.p1)
          and (aArg^.p2^.p1^.typ=t_pointerdef) and assigned(aArg^.p2^.p1^.p1)
          and (aArg^.p2^.p1^.p1^.typ=t_procdef);
end;


// Declares a named procedural type aOwner_param for each function pointer argument in aArgs,
// and replaces the type of that argument by the name.
procedure HoistProcVarArgs(const aOwner : string; aArgs : presobject);

var
  lArg, lDec : presobject;
  lIndex : integer;
  lParam, lName : string;

begin
  lIndex:=0;
  while assigned(aArgs) do
    begin
    Inc(lIndex);
    lArg:=aArgs^.p1;
    if IsProcVarArg(lArg) then
      begin
      lDec:=lArg^.p2;
      if assigned(lDec^.p2) and assigned(lDec^.p2^.p) then
        lParam:=lDec^.p2^.str
      else if RemoveUnderscore then
        lParam:='para'+str(lIndex)
      else
        lParam:='_para'+str(lIndex);
      HoistProcVarArgs(aOwner+'_'+lParam,lDec^.p1^.p1^.p2);
      lName:=TypeName(aOwner+'_'+lParam);
      if block_type<>bt_type then
        begin
        if not compactmode then
          writeln(outfile);
        writeln(outfile,aktspace,'type');
        block_type:=bt_type;
        end;
      shift(2);
      write(outfile,aktspace,lName,' = ');
      write_p_a_def(outfile,lDec^.p1,lArg^.p1);
      WriteProcVarDirectives(outfile,false);
      writeln(outfile,';');
      is_procvar:=false;
      WritePointerMarker(outfile,lName);
      popshift;
      dispose(lArg^.p1,done);
      lArg^.p1:=NewIntID(lName);
      dispose(lDec^.p1,done);
      lDec^.p1:=nil;
      end
    else if assigned(lArg) and assigned(lArg^.p2) and assigned(lArg^.p2^.p1) then
      begin
      lDec:=lArg^.p2;
      if assigned(lDec^.p2) and assigned(lDec^.p2^.p) then
        lParam:=lDec^.p2^.str
      else if RemoveUnderscore then
        lParam:='para'+str(lIndex)
      else
        lParam:='_para'+str(lIndex);
      HoistProcVarElement(aOwner+'_'+lParam,lDec^.p1,lArg^.p1);
      end;
    aArgs:=aArgs^.next;
    end;
end;


// Declares a named procedural type aOwner_result for a function pointer result of aProc (t_procdef),
// and replaces the result type aType by the name.
procedure HoistProcVarResult(const aOwner : string; aProc : presobject; var aType : presobject);

var
  lResult : presobject;
  lName : string;

begin
  lResult:=aProc^.p1;
  if not (assigned(lResult) and (lResult^.typ=t_pointerdef) and assigned(lResult^.p1)
          and (lResult^.p1^.typ=t_procdef)) then
    exit;
  HoistProcVarArgs(aOwner+'_result',lResult^.p1^.p2);
  lName:=TypeName(aOwner+'_result');
  if block_type<>bt_type then
    begin
    if not compactmode then
      writeln(outfile);
    writeln(outfile,aktspace,'type');
    block_type:=bt_type;
    end;
  shift(2);
  write(outfile,aktspace,lName,' = ');
  write_p_a_def(outfile,lResult,aType);
  WriteProcVarDirectives(outfile,false);
  writeln(outfile,';');
  is_procvar:=false;
  WritePointerMarker(outfile,lName);
  popshift;
  dispose(aType,done);
  aType:=NewIntID(lName);
  dispose(lResult,done);
  aProc^.p1:=nil;
end;


// Declares named element types for the arrays of and pointers to function pointers among the variables aDecls of type aType.
procedure HoistVariableProcVarElements(aDecls, aType : presobject);

begin
  while assigned(aDecls) do
    begin
    if assigned(aDecls^.p1) and assigned(aDecls^.p1^.p2) and assigned(aDecls^.p1^.p2^.p) then
      begin
      HoistStructProcVarElements(aDecls^.p1^.p2^.str,aType);
      HoistProcVarElement(aDecls^.p1^.p2^.str+'_element',aDecls^.p1^.p1,aType);
      end;
    aDecls:=aDecls^.next;
    end;
end;


// Hoists the function pointer arguments and result of the function declared by aDecl (t_declist).
procedure HoistDeclarationProcVarArgs(aDecl : presobject; var aType : presobject);

begin
  if assigned(aDecl^.p1^.p2) and assigned(aDecl^.p1^.p2^.p) then
    begin
    HoistProcVarArgs(aDecl^.p1^.p2^.str,aDecl^.p1^.p1^.p2);
    HoistProcVarResult(aDecl^.p1^.p2^.str,aDecl^.p1^.p1,aType);
    end;
end;


function HandleDeclarationStatement(decl, type_spec, modifier_spec,
  decllist_spec, block_spec: presobject): presobject;
var
  hp : presobject;
  IsExtern : boolean;
  lSkipEllipsis, lDone, lVarArgs : boolean;
  lUseLib, lDynLib : boolean;

begin
  HandleDeclarationStatement:=Nil;
  IsExtern:=false;
  (* a function with a body is implemented here: not external, no procedure variable *)
  lUseLib:=UseLib;
  lDynLib:=createdynlib;
  UseLib:=false;
  createdynlib:=false;
  (* by default we must pop the args pushed on stack *)
  no_pop:=false;
  if (assigned(decllist_spec)and assigned(decllist_spec^.p1)and assigned(decllist_spec^.p1^.p1))
    and (decllist_spec^.p1^.p1^.typ=t_procdef) then
    begin
        HoistDeclarationProcVarArgs(decllist_spec,type_spec);
        lVarArgs:=false;
        lSkipEllipsis:=false;
        repeat
        IsExtern:=false;
        no_pop:=assigned(modifier_spec) and (modifier_spec^.str='no_pop');

        if (block_type<>bt_func) and not(createdynlib) then
          begin
            writeln(outfile);
            block_type:=bt_func;
          end;

        (* dyn. procedures must be put into a var block *)
        if createdynlib then
          begin
            if (block_type<>bt_var) then
            begin
                if not(compactmode) then
                  writeln(outfile);
                writeln(outfile,aktspace,'var');
                block_type:=bt_var;
            end;
            shift(2);
          end;
        if not CompactMode then
        begin
          write(outfile,aktspace);
          if not IsExtern then
            write(implemfile,aktspace);
        end;
        (* distinguish between procedure and function *)
        if assigned(type_spec) then
        if (type_spec^.typ=t_void) and (decllist_spec^.p1^.p1^.p1=nil) then
          begin
            if createdynlib then
              begin
                write(outfile,decllist_spec^.p1^.p2^.p,' : procedure');
              end
            else
              begin
                shift(10);
                write(outfile,'procedure ',decllist_spec^.p1^.p2^.p);
              end;
            if assigned(decllist_spec^.p1^.p1^.p2) then
              write_args(outfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
            begin
              write(implemfile,'procedure ',decllist_spec^.p1^.p2^.p);
              if assigned(decllist_spec^.p1^.p1^.p2) then
                write_args(implemfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
            end;
          end
        else
          begin
            if createdynlib then
              begin
                write(outfile,decllist_spec^.p1^.p2^.p,' : function');
              end
            else
              begin
                shift(9);
                write(outfile,'function ',decllist_spec^.p1^.p2^.p);
              end;

            if assigned(decllist_spec^.p1^.p1^.p2) then
              write_args(outfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
            write(outfile,':');
            old_in_args:=in_args;
            (* write pointers as P.... instead of ^.... *)
            in_args:=true;
            write_p_a_def(outfile,decllist_spec^.p1^.p1^.p1,type_spec);
            in_args:=old_in_args;
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
              begin
                write(implemfile,'function ',decllist_spec^.p1^.p2^.p);
                if assigned(decllist_spec^.p1^.p1^.p2) then
                  write_args(implemfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
                write(implemfile,':');

                old_in_args:=in_args;
                (* write pointers as P.... instead of ^.... *)
                in_args:=true;
                write_p_a_def(implemfile,decllist_spec^.p1^.p1^.p1,type_spec);
                in_args:=old_in_args;
              end;
          end;
        WriteCallingConvention(IsExtern);
        if lVarArgs then
          write(outfile,';varargs');
        popshift;
        if createdynlib then
          begin
            writeln(outfile,';');
          end
        else if UseLib then
          begin
            if IsExtern then
            begin
              write (outfile,';external');
              If UseName then
                Write(outfile,' External_library name ''',decllist_spec^.p1^.p2^.p,'''');
            end;
            writeln(outfile,';');
          end
        else
          begin
            writeln(outfile,';');
            if not IsExtern then
            begin
              writeln(implemfile,';');
              shift(2);
              if block_spec^.typ=t_statement_list then
                write_statement_block(implemfile,block_spec);
              popshift;
            end;
          end;
        IsExtern:=false;
        if not(compactmode) and not(createdynlib) then
        writeln(outfile);
        lDone:=lSkipEllipsis or createdynlib or not HasEllipsis(decllist_spec^.p1^.p1^.p2);
        lSkipEllipsis:=true;
      until lDone;
    end
  else (* decllist_spec^.p1^.p1^.typ=t_procdef *)
  if assigned(decllist_spec)and assigned(decllist_spec^.p1) then
    begin
        HoistVariableProcVarElements(decllist_spec,type_spec);
        shift(2);
        if block_type<>bt_var then
          begin
            if not(compactmode) then
              writeln(outfile);
            writeln(outfile,aktspace,'var');
          end;
        block_type:=bt_var;

        shift(2);

        IsExtern:=assigned(decl)and(decl^.str='extern');
        (* walk through all declarations *)
        hp:=decllist_spec;
        while assigned(hp) and assigned(hp^.p1) do
          begin
            (* write new var name *)
            if assigned(hp^.p1^.p2) and assigned(hp^.p1^.p2^.p) then
              write(outfile,aktspace,hp^.p1^.p2^.p);
            write(outfile,' : ');
            shift(2);
            (* write its type *)
            is_procvar:=false;
            write_p_a_def(outfile,hp^.p1^.p1,type_spec);
            WriteProcVarDirectives(outfile,false);
            if assigned(hp^.p1^.p2)and assigned(hp^.p1^.p2^.p)then
              begin
                  if isExtern then
                    write(outfile,';cvar;external')
                  else if not (assigned(decl) and (decl^.str='static')) then
                    write(outfile,';cvar;public');
              end;
            writeln(outfile,';');
            popshift;
            hp:=hp^.next;
          end;
        popshift;
        popshift;
    end;
  if assigned(decl) then
    dispose(decl,done);
  if assigned(type_spec) then
    dispose(type_spec,done);
  if assigned(modifier_spec) then
    dispose(modifier_spec,done);
  if assigned(decllist_spec) then
    dispose(decllist_spec,done);
  if assigned(block_spec) then
    dispose(block_spec,done);
  UseLib:=lUseLib;
  createdynlib:=lDynLib;
end;

function HandleDeclarationSysTrap(decl, type_spec, modifier_spec,
  decllist_spec, sys_trap: presobject): presobject;

var
  hp : presobject;
  IsExtern : boolean;
  lSkipEllipsis, lDone, lVarArgs : boolean;

begin
  HandleDeclarationSysTrap:=Nil;
  IsExtern:=false;
  (* by default we must pop the args pushed on stack *)
  no_pop:=false;
  if (assigned(decllist_spec)and assigned(decllist_spec^.p1)and assigned(decllist_spec^.p1^.p1))
    and (decllist_spec^.p1^.p1^.typ=t_procdef)
    and assigned(decl) and (decl^.str='static') then
    begin
      if assigned(decllist_spec^.p1^.p2) and assigned(decllist_spec^.p1^.p2^.p) then
        writeln(outfile,aktspace,'(* static function ',decllist_spec^.p1^.p2^.p,' ignored *)');
    end
  else
  if (assigned(decllist_spec)and assigned(decllist_spec^.p1)and assigned(decllist_spec^.p1^.p1))
    and (decllist_spec^.p1^.p1^.typ=t_procdef) then
    begin
        HoistDeclarationProcVarArgs(decllist_spec,type_spec);
        lVarArgs:=HasEllipsis(decllist_spec^.p1^.p1^.p2) and
          (UseLib or createdynlib or (assigned(decl) and (decl^.str='extern')));
        lSkipEllipsis:=lVarArgs;
        repeat
        If UseLib then
          IsExtern:=true
        else
          IsExtern:=assigned(decl)and(decl^.str='extern');
        no_pop:=assigned(modifier_spec) and (modifier_spec^.str='no_pop');

        if (block_type<>bt_func) and not(createdynlib) then
          begin
            writeln(outfile);
            block_type:=bt_func;
          end;

        (* dyn. procedures must be put into a var block *)
        if createdynlib then
          begin
            if (block_type<>bt_var) then
            begin
                if not(compactmode) then
                  writeln(outfile);
                writeln(outfile,aktspace,'var');
                block_type:=bt_var;
            end;
            shift(2);
          end;
        if not CompactMode then
        begin
          write(outfile,aktspace);
          if not IsExtern then
            write(implemfile,aktspace);
        end;
        (* distinguish between procedure and function *)
        if assigned(type_spec) then
        if (type_spec^.typ=t_void) and (decllist_spec^.p1^.p1^.p1=nil) then
          begin
            if createdynlib then
              begin
                write(outfile,decllist_spec^.p1^.p2^.p,' : procedure');
              end
            else
              begin
                shift(10);
                write(outfile,'procedure ',decllist_spec^.p1^.p2^.p);
              end;
            if assigned(decllist_spec^.p1^.p1^.p2) then
              write_args(outfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
            begin
              write(implemfile,'procedure ',decllist_spec^.p1^.p2^.p);
              if assigned(decllist_spec^.p1^.p1^.p2) then
                write_args(implemfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
            end;
          end
        else
          begin
            if createdynlib then
              begin
                write(outfile,decllist_spec^.p1^.p2^.p,' : function');
              end
            else
              begin
                shift(9);
                write(outfile,'function ',decllist_spec^.p1^.p2^.p);
              end;

            if assigned(decllist_spec^.p1^.p1^.p2) then
              write_args(outfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
            write(outfile,':');
            old_in_args:=in_args;
            (* write pointers as P.... instead of ^.... *)
            in_args:=true;
            write_p_a_def(outfile,decllist_spec^.p1^.p1^.p1,type_spec);
            in_args:=old_in_args;
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
              begin
                write(implemfile,'function ',decllist_spec^.p1^.p2^.p);
                if assigned(decllist_spec^.p1^.p1^.p2) then
                write_args(implemfile,decllist_spec^.p1^.p1^.p2,lSkipEllipsis);
                write(implemfile,':');

                old_in_args:=in_args;
                (* write pointers as P.... instead of ^.... *)
                in_args:=true;
                write_p_a_def(implemfile,decllist_spec^.p1^.p1^.p1,type_spec);
                in_args:=old_in_args;
              end;
          end;
        if assigned(sys_trap) then
          write(outfile,';systrap ',sys_trap^.p);
        WriteCallingConvention(IsExtern);
        if lVarArgs then
          write(outfile,';varargs');
        popshift;
        if createdynlib then
          begin
            writeln(outfile,';');
          end
        else if UseLib then
          begin
            if IsExtern then
            begin
              write (outfile,';external');
              If UseName then
                Write(outfile,' External_library name ''',decllist_spec^.p1^.p2^.p,'''');
            end;
            writeln(outfile,';');
          end
        else
          begin
            writeln(outfile,';');
            if not IsExtern then
            begin
              writeln(implemfile,';');
              writeln(implemfile,aktspace,'begin');
              writeln(implemfile,aktspace,'  { You must implement this function }');
              writeln(implemfile,aktspace,'end;');
            end;
          end;
        IsExtern:=false;
        if not(compactmode) and not(createdynlib) then
        writeln(outfile);
        lDone:=lSkipEllipsis or createdynlib or not HasEllipsis(decllist_spec^.p1^.p1^.p2);
        lSkipEllipsis:=true;
      until lDone;
    end
  else (* decllist_spec^.p1^.p1^.typ=t_procdef *)
  if assigned(decllist_spec)and assigned(decllist_spec^.p1) then
    begin
        HoistVariableProcVarElements(decllist_spec,type_spec);
        shift(2);
        if block_type<>bt_var then
          begin
            if not(compactmode) then
              writeln(outfile);
            writeln(outfile,aktspace,'var');
          end;
        block_type:=bt_var;

        shift(2);

        IsExtern:=assigned(decl)and(decl^.str='extern');
        (* walk through all declarations *)
        hp:=decllist_spec;
        while assigned(hp) and assigned(hp^.p1) do
          begin
            (* write new var name *)
            if assigned(hp^.p1^.p2) and assigned(hp^.p1^.p2^.p) then
              write(outfile,aktspace,hp^.p1^.p2^.p);
            write(outfile,' : ');
            shift(2);
            (* write its type *)
            is_procvar:=false;
            write_p_a_def(outfile,hp^.p1^.p1,type_spec);
            WriteProcVarDirectives(outfile,false);
            if assigned(hp^.p1^.p2)and assigned(hp^.p1^.p2^.p)then
              begin
                  if isExtern then
                    write(outfile,';cvar;external')
                  else if not (assigned(decl) and (decl^.str='static')) then
                    write(outfile,';cvar;public');
              end;
            writeln(outfile,';');
            popshift;
            hp:=hp^.next;
          end;
        popshift;
        popshift;
    end;
  if assigned(decl)then  dispose(decl,done);
  if assigned(type_spec)then  dispose(type_spec,done);
  if assigned(decllist_spec)then  dispose(decllist_spec,done);
end;

function HandleSpecialType(aType: presobject) : presobject;

var
  hp : presobject;

begin
  HandleSpecialType:=Nil;
  if block_type<>bt_type then
    begin
    if not(compactmode) then
      writeln(outfile);
    writeln(outfile,aktspace,'type');
    block_type:=bt_type;
    end;
  if assigned(aType^.p2) and assigned(aType^.p2^.p) then
    HoistStructProcVarElements(aType^.p2^.str,aType);
  shift(2);
  if ( aType^.p2  <> nil ) then
    begin
    (* write new type name *)
    TN:=TypeName(aType^.p2^.p);
    PN:=PointerName(aType^.p2^.p);
    (* define a Pointer type also for structs *)
    if UsePPointers and (not SameText(tn,pn)) and
      assigned(aType) and (aType^.typ in [t_uniondef,t_structdef]) then
      WritePointerTypeDef(outfile,PN,TN);
    write(outfile,aktspace,TN,' = ');
    shift(2);
    hp:=aType;
    write_type_specifier(outfile,hp);
    popshift;
    (* enum_to_const can make a switch to const *)
    if block_type=bt_type then
      begin
      writeln(outfile,';');
      WritePointerMarker(outfile,TN);
      end;
    writeln(outfile);
    flush(outfile);
    popshift;
    if must_write_packed_field then
      write_packed_fields_info(outfile,hp,TN);
    if assigned(hp) then
      dispose(hp,done)
    end
  else
    begin
    TN:=TypeName(aType^.str);
    PN:=PointerName(aType^.str);
    if UsePPointers then
      WritePointerTypeDef(outfile,PN,TN);
    if PackRecords then
      writeln(outfile, aktspace, TN, ' = packed record')
    else
      writeln(outfile, aktspace, TN, ' = record');
    writeln(outfile, aktspace, '    {undefined structure}');
    writeln(outfile, aktspace, '  end;');
    WritePointerMarker(outfile,TN);
    writeln(outfile);
    popshift;
    end;
end;

// Makes the typedef declarator aDecl of a function type a pointer to the function, and registers its name.
procedure WrapFunctionType(aDecl : presobject);

begin
  if not assigned(aDecl) or not assigned(aDecl^.p1) or (aDecl^.p1^.typ<>t_procdef) then
    exit;
  aDecl^.p1:=NewType1(t_pointerdef,aDecl^.p1);
  if assigned(aDecl^.p2) and assigned(aDecl^.p2^.p) then
    RegisterFunctionType(aDecl^.p2^.str);
end;


// Returns true when aList is the argument list (void): one unnamed argument of type void.
function IsVoidArgList(aList : presobject) : boolean;

var
  lArg : presobject;

begin
  Result:=false;
  if not assigned(aList) or (aList^.typ<>t_arglist) or assigned(aList^.next) then
    exit;
  lArg:=aList^.p1;
  Result:=assigned(lArg) and assigned(lArg^.p1) and (lArg^.p1^.typ=t_void)
          and assigned(lArg^.p2) and not assigned(lArg^.p2^.p1) and not assigned(lArg^.p2^.p2);
end;


function HandleTypedef(type_spec,dec_modifier,declarator,arg_decl_list: presobject) : presobject;
var
  hp : presobject;

begin
  hp:=nil;
  HandleTypedef:=nil;
  if IsVoidArgList(arg_decl_list) then
    begin
    dispose(arg_decl_list,done);
    arg_decl_list:=nil;
    end;
  (* TYPEDEF type_specifier LKLAMMER dec_modifier declarator RKLAMMER maybe_space LKLAMMER argument_declaration_list RKLAMMER SEMICOLON *)
  if block_type<>bt_type then
    begin
      if not(compactmode) then
        writeln(outfile);
      writeln(outfile,aktspace,'type');
      block_type:=bt_type;
    end;
  if assigned(declarator) and assigned(declarator^.p2) and assigned(declarator^.p2^.p) then
    HoistProcVarArgs(declarator^.p2^.str,arg_decl_list);
  no_pop:=assigned(dec_modifier) and (dec_modifier^.str='no_pop');
  shift(2);
  (* walk through all declarations *)
  hp:=declarator;
  if assigned(hp) then
  begin
    hp:=declarator;
    while assigned(hp^.p1) do
      hp:=hp^.p1;
    hp^.p1:=new(presobject,init_two(t_procdef,nil,arg_decl_list));
    hp:=declarator;
    if assigned(hp^.p2) and assigned(hp^.p2^.p) then
      begin
      popshift;
      HoistProcVarElement(hp^.p2^.str+'_element',hp^.p1,type_spec);
      shift(2);
      end;
    WrapFunctionType(hp);
    if assigned(hp^.p1) and assigned(hp^.p1^.p1) then
      begin
        writeln(outfile);
        (* write new type name *)
        write(outfile,aktspace,TypeName(hp^.p2^.p),' = ');
        shift(2);
        write_p_a_def(outfile,hp^.p1,type_spec);
        popshift;
        WriteProcVarDirectives(outfile,no_pop);
        writeln(outfile,';');
        WritePointerMarker(outfile,TypeName(hp^.p2^.p));
        flush(outfile);
      end;
  end;
  popshift;
  if assigned(type_spec)then
  dispose(type_spec,done);
  if assigned(dec_modifier)then
  dispose(dec_modifier,done);
  if assigned(declarator)then (* disposes also arg_decl_list *)
  dispose(declarator,done);
end;

function HandleTypedefList(type_spec,dec_modifier,declarator_list: presobject) : presobject;

(* TYPEDEF type_specifier dec_modifier declarator_list SEMICOLON *)

var
  hp,ph : presobject;
  lDecl : presobject;
  lFunctionType, lInlineType : boolean;


begin
  HandleTypedefList:=Nil;
  ph:=nil;
  lDecl:=nil;
  if assigned(declarator_list) then
    lDecl:=declarator_list^.p1;
  (* after a syntax error the declarator list can be missing *)
  if not assigned(type_spec) or
     (not assigned(type_spec^.p2) and not (assigned(lDecl) and assigned(lDecl^.p2))) then
    begin
    if not stripinfo then
      writeln(outfile,'(* typedef without name at line ',line_no,' ignored *)');
    if assigned(type_spec) then
      dispose(type_spec,done);
    if assigned(dec_modifier) then
      dispose(dec_modifier,done);
    if assigned(declarator_list) then
      dispose(declarator_list,done);
    exit;
    end;
  if block_type<>bt_type then
    begin
      if not(compactmode) then
        writeln(outfile);
      writeln(outfile,aktspace,'type');
      block_type:=bt_type;
    end
  else
    writeln(outfile);
  if assigned(type_spec^.p2) and assigned(type_spec^.p2^.p) then
    HoistStructProcVarElements(type_spec^.p2^.str,type_spec)
  else if assigned(lDecl) and assigned(lDecl^.p2) and assigned(lDecl^.p2^.p) then
    HoistStructProcVarElements(lDecl^.p2^.str,type_spec);
  hp:=declarator_list;
  while assigned(hp) do
    begin
    if assigned(hp^.p1) and assigned(hp^.p1^.p2) and assigned(hp^.p1^.p2^.p) then
      HoistProcVarElement(hp^.p1^.p2^.str+'_element',hp^.p1^.p1,type_spec);
    hp:=hp^.next;
    end;
  no_pop:=assigned(dec_modifier) and (dec_modifier^.str='no_pop');
  shift(2);
  (* Get the name to write the type definition for, try
    to use the tag name first *)
  lInlineType:=(type_spec^.typ in [t_structdef,t_uniondef,t_enumdef]) and assigned(type_spec^.p1)
               and assigned(lDecl) and assigned(lDecl^.p1) and assigned(lDecl^.p2) and assigned(lDecl^.p2^.p);
  if lInlineType and not assigned(type_spec^.p2) then
    if type_spec^.typ=t_enumdef then
      type_spec^.p2:=NewID(lDecl^.p2^.str+'_enum')
    else
      type_spec^.p2:=NewID(lDecl^.p2^.str+'_record');
  if assigned(type_spec^.p2) then
    ph:=type_spec^.p2
  else
    ph:=lDecl^.p2;
  lFunctionType:=assigned(lDecl) and assigned(lDecl^.p1) and (lDecl^.p1^.typ=t_procdef);
  if lFunctionType then
    WrapFunctionType(lDecl);
  (* write type definition *)
  is_procvar:=false;
  TN:=TypeName(ph^.p);
  if lFunctionType then
    PN:=TN
  else
    PN:=PointerName(ph^.p);
  if UsePPointers and (not SameText(tn,pn)) and not lFunctionType and
    assigned(type_spec) and (type_spec^.typ<>t_procdef) then
    WritePointerTypeDef(outfile,PN,TN);
  (* write new type name *)
  write(outfile,aktspace,TN,' = ');
  shift(2);
  if assigned(lDecl) and not lInlineType then
    write_p_a_def(outfile,lDecl^.p1,type_spec)
  else
    write_p_a_def(outfile,nil,type_spec);
  popshift;
  WriteProcVarDirectives(outfile,no_pop);
  (* enum_to_const can make a switch to const *)
  if block_type=bt_type then
    writeln(outfile,';');
  WritePointerMarker(outfile,TN);
  flush(outfile);
  (* write alias names, ph points to the name already used *)
  hp:=declarator_list;
  while assigned(hp) do
  begin
    if (hp<>ph) and assigned(hp^.p1) and assigned(hp^.p1^.p2) then
      begin
        PN:=TypeName(ph^.p);
        TN:=TypeName(hp^.p1^.p2^.p);
        if not SameText(TN,PN) then
        begin
          if block_type<>bt_type then
            begin
            writeln(outfile,Copy(aktspace,1,Length(aktspace)-2),'type');
            block_type:=bt_type;
            end;
          write(outfile,aktspace,TN,' = ');
          write_p_a_def(outfile,hp^.p1^.p1,ph);
          writeln(outfile,';');
          PN:=PointerName(hp^.p1^.p2^.p);
          if UsePPointers and (not sametext(tn,pn)) and
            assigned(type_spec) and (type_spec^.typ<>t_procdef) then
            WritePointerTypeDef(outfile,PN,TN);
          WritePointerMarker(outfile,TN);
        end;
      end;
    hp:=hp^.next;
  end;
  popshift;
  if must_write_packed_field then
    if assigned(ph) then
      write_packed_fields_info(outfile,type_spec,ph^.str)
    else if assigned(type_spec^.p2) then
      write_packed_fields_info(outfile,type_spec,type_spec^.p2^.str);
  if assigned(type_spec)then
  dispose(type_spec,done);
  if assigned(dec_modifier)then
  dispose(dec_modifier,done);
  if assigned(declarator_list)then
  dispose(declarator_list,done);
end;

function HandleStructDef(dname1,dname2 : presobject) : presobject;

begin
  HandleStructDef:=nil;
  (* TYPEDEF STRUCT dname dname SEMICOLON *)
  if block_type<>bt_type then
    begin
      if not(compactmode) then
        writeln(outfile);
      writeln(outfile,aktspace,'type');
      block_type:=bt_type;
    end;
  PN:=TypeName(dname1^.p);
  TN:=TypeName(dname2^.p);
  if not SameText(tn,pn) then
  begin
    shift(2);
    writeln(outfile,aktspace,TN,' = ',PN,';');
    WritePointerMarker(outfile,TN);
    popshift;
  end;
  if assigned(dname1) then
    dispose(dname1,done);
  if assigned(dname2) then
    dispose(dname2,done);
end;

function HandleSimpleTypeDef(tname : presobject) : presobject;

begin
  HandleSimpleTypeDef:=Nil;
  if block_type<>bt_type then
    begin
      if not(compactmode) then
        writeln(outfile);
      writeln(outfile,aktspace,'type');
      block_type:=bt_type;
    end
  else
    writeln(outfile);
  shift(2);
  (* write as pointer *)
  writeln(outfile,'(* generic typedef  *)');
  writeln(outfile,aktspace,tname^.p,' = pointer;');
  WritePointerMarker(outfile,tname^.p);
  flush(outfile);
  popshift;
  if assigned(tname) then
  dispose(tname,done);
end;

function HandleErrorDecl(e1,e2 : presobject) : presobject;

begin
  HandleErrorDecl:=Nil;
  writeln(outfile,'in declaration at line ',line_no,' *)');
  in_space_define:=0;
  in_define:=false;
  arglevel:=0;
  if_nb:=0;
  resetshift;
  yyerrok;
end;

function HandleDefine(dname : presobject) : presobject;

begin
  HandleDefine:=Nil;
  writeln(outfile,'{$define ',dname^.p,'}',aktspace,commentstr);
  flush(outfile);
  if assigned(dname)then
  dispose(dname,done);
end;

function HandleDefineConst(dname,def_expr: presobject) : presobject;

var
  hp : presobject;

begin
  HandleDefineConst:=Nil;
  (* DEFINE dname SPACE_DEFINE def_expr NEW_LINE *)
  if (def_expr^.typ=t_exprlist) and
    def_expr^.p1^.is_const and
    not assigned(def_expr^.next) then
    begin
      if block_type<>bt_const then
        begin
          if block_type<>bt_func then
            writeln(outfile);
          writeln(outfile,aktspace,'const');
        end;
      block_type:=bt_const;
      shift(2);
      write(outfile,aktspace,dname^.p);
      write(outfile,' = ');
      flush(outfile);
      write_expr(outfile,def_expr^.p1);
      writeln(outfile,';',aktspace,commentstr);
      popshift;
      if assigned(dname) then
      dispose(dname,done);
      if assigned(def_expr) then
      dispose(def_expr,done);
    end
  else
    begin
      if block_type<>bt_func then
        writeln(outfile);
      if not stripinfo then
        begin
          writeln (outfile,aktspace,'{ was #define dname def_expr }');
          writeln (implemfile,aktspace,'{ was #define dname def_expr }');
        end;
      block_type:=bt_func;
      write(outfile,aktspace,'function ',dname^.p);
      write(implemfile,aktspace,'function ',dname^.p);
      shift(2);
      if not assigned(def_expr^.p3) then
        begin
            writeln(outfile,' : longint; { return type might be wrong }');
            flush(outfile);
            writeln(implemfile,' : longint; { return type might be wrong }');
        end
      else
        begin
            write(outfile,' : ');
            write_cast_type(outfile,def_expr^.p3);
            writeln(outfile,';',aktspace,commentstr);
            flush(outfile);
            write(implemfile,' : ');
            write_cast_type(implemfile,def_expr^.p3);
            writeln(implemfile,';');
        end;
      writeln(outfile);
      flush(outfile);
      hp:=new(presobject,init_two(t_funcname,dname,def_expr));
      write_funexpr(implemfile,hp);
      popshift;
      dispose(hp,done);
      writeln(implemfile);
      flush(implemfile);
    end;
end;

// Returns true when aName is one of the macro parameters in aParams (t_enumlist).
function IsMacroParam(const aName : string; aParams : presobject) : boolean;

begin
  Result:=false;
  while assigned(aParams) and not Result do
    begin
    Result:=assigned(aParams^.p1) and (aParams^.p1^.str=aName);
    aParams:=aParams^.next;
    end;
end;


// Returns the binding strength of the Pascal binary operator aOp, as parsed by the grammar.
function OperatorPrecedence(const aOp : string) : integer;

begin
  case aOp of
    ':=' : Result:=0;
    '=','<>','<','<=','>','>=' : Result:=1;
    ' or ' : Result:=3;
    ' and ' : Result:=4;
    '+','-' : Result:=5;
    ' shl ',' shr ' : Result:=6;
    '*','/',' div ' : Result:=7;
  else
    Result:=9;
  end;
end;


// Rewrites casts to a macro parameter, such as (a)+1 or (a)-1, into binary operations.
// aRotatable collects the rewritten operations that were not between parentheses.
function FixParamCasts(p,aParams : presobject; aRotatable : TFPList) : presobject;

var
  lUnary, lRes : presobject;
  lOp : string;

begin
  Result:=p;
  if not assigned(p) then
    exit;
  p^.p1:=FixParamCasts(p^.p1,aParams,aRotatable);
  p^.p2:=FixParamCasts(p^.p2,aParams,aRotatable);
  p^.p3:=FixParamCasts(p^.p3,aParams,aRotatable);
  p^.next:=FixParamCasts(p^.next,aParams,aRotatable);
  if (p^.typ=t_typespec) and assigned(p^.p1) and (p^.p1^.typ=t_id) and IsMacroParam(p^.p1^.str,aParams)
     and assigned(p^.p2) and (p^.p2^.typ=t_preop) and ((p^.p2^.str='+') or (p^.p2^.str='-') or (p^.p2^.str='@')) then
    begin
    lUnary:=p^.p2;
    lOp:=lUnary^.str;
    if lOp='@' then
      lOp:=' and ';
    lRes:=NewBinaryOp(lOp,p^.p1,lUnary^.p1);
    lRes^.grouped:=p^.grouped;
    lRes^.next:=p^.next;
    lUnary^.p1:=nil;
    p^.p1:=nil;
    p^.next:=nil;
    dispose(p,done);
    if not lRes^.grouped then
      aRotatable.Add(lRes);
    Result:=lRes;
    exit;
    end;
  (* the rewritten operation binds weaker than its parent: re-associate *)
  if (p^.typ=t_bop) and assigned(p^.p2) and (aRotatable.IndexOf(p^.p2)<>-1)
     and (OperatorPrecedence(p^.str)>=OperatorPrecedence(p^.p2^.str)) then
    begin
    Result:=p^.p2;
    p^.p2:=Result^.p1;
    Result^.p1:=p;
    end
  else if (p^.typ=t_bop) and assigned(p^.p1) and (aRotatable.IndexOf(p^.p1)<>-1)
     and (OperatorPrecedence(p^.str)>OperatorPrecedence(p^.p1^.str)) then
    begin
    Result:=p^.p1;
    p^.p1:=Result^.p2;
    Result^.p2:=p;
    end
  else if (p^.typ=t_typespec) and assigned(p^.p2) and (aRotatable.IndexOf(p^.p2)<>-1) then
    begin
    Result:=p^.p2;
    p^.p2:=Result^.p1;
    Result^.p1:=p;
    end
  else if (p^.typ=t_preop) and assigned(p^.p1) and (aRotatable.IndexOf(p^.p1)<>-1) then
    begin
    Result:=p^.p1;
    p^.p1:=Result^.p1;
    Result^.p1:=p;
    end;
  if Result<>p then
    begin
    Result^.next:=p^.next;
    p^.next:=nil;
    if p^.grouped then
      begin
      Result^.grouped:=true;
      p^.grouped:=false;
      aRotatable.Remove(Result);
      end;
    end;
end;


// Returns aExpr with aLeft aOp inserted before its leftmost operand, below the operators that bind as weak or weaker.
function InsertLeftOperand(const aOp : string; aLeft,aExpr : presobject) : presobject;

begin
  if assigned(aExpr) and not aExpr^.grouped and (aExpr^.typ=t_bop)
     and (OperatorPrecedence(aExpr^.str)<=OperatorPrecedence(aOp)) then
    begin
    aExpr^.p1:=InsertLeftOperand(aOp,aLeft,aExpr^.p1);
    Result:=aExpr;
    end
  else if assigned(aExpr) and not aExpr^.grouped and (aExpr^.typ=t_ifexpr) then
    begin
    aExpr^.p1:=InsertLeftOperand(aOp,aLeft,aExpr^.p1);
    Result:=aExpr;
    end
  else
    Result:=NewBinaryOp(aOp,aLeft,aExpr);
end;


function HandleNamedProduct(aName,aRight : presobject) : presobject;

begin
  Result:=InsertLeftOperand('*',aName,aRight);
end;


function HandleDefineMacro(dname,enum_list,para_def_expr: presobject) : presobject;

var
  hp,ph : presobject;
  lRotatable : TFPList;

begin
  HandleDefineMacro:=Nil;
  hp:=nil;
  ph:=nil;
  if assigned(enum_list) then
    begin
    lRotatable:=TFPList.Create;
    para_def_expr^.p1:=FixParamCasts(para_def_expr^.p1,enum_list,lRotatable);
    para_def_expr^.p2:=FixParamCasts(para_def_expr^.p2,enum_list,lRotatable);
    para_def_expr^.next:=FixParamCasts(para_def_expr^.next,enum_list,lRotatable);
    lRotatable.Free;
    (* the result type of a cast to a parameter is no type *)
    if assigned(para_def_expr^.p3) and (para_def_expr^.p3^.typ=t_id)
       and IsMacroParam(para_def_expr^.p3^.str,enum_list) then
      begin
      dispose(para_def_expr^.p3,done);
      para_def_expr^.p3:=nil;
      end;
    end;
  (* DEFINE dname LKLAMMER enum_list RKLAMMER para_def_expr NEW_LINE *)
  if not stripinfo then
  begin
    writeln (outfile,aktspace,'{ was #define dname(params) para_def_expr }');
    writeln (implemfile,aktspace,'{ was #define dname(params) para_def_expr }');
    if assigned(enum_list) then
      begin
        writeln (outfile,aktspace,'{ argument types are unknown }');
        writeln (implemfile,aktspace,'{ argument types are unknown }');
      end;
    if not assigned(para_def_expr^.p3) then
      begin
        writeln(outfile,aktspace,'{ return type might be wrong }   ');
        writeln(implemfile,aktspace,'{ return type might be wrong }   ');
      end;
  end;
  if block_type<>bt_func then
    writeln(outfile);

  block_type:=bt_func;
  write(outfile,aktspace,'function ',dname^.p);
  write(implemfile,aktspace,'function ',dname^.p);

  if assigned(enum_list) then
    begin
      write(outfile,'(');
      write(implemfile,'(');
      ph:=new(presobject,init_one(t_enumdef,enum_list));
      write_def_params(outfile,ph);
      write_def_params(implemfile,ph);
      if assigned(ph) then dispose(ph,done);
      ph:=nil;
      (* types are unknown *)
      write(outfile,' : longint)');
      write(implemfile,' : longint)');
    end;
  if not assigned(para_def_expr^.p3) then
    begin
      writeln(outfile,' : longint;',aktspace,commentstr);
      writeln(implemfile,' : longint;');
      flush(outfile);
    end
  else
    begin
      write(outfile,' : ');
      write_cast_type(outfile,para_def_expr^.p3);
      writeln(outfile,';',aktspace,commentstr);
      flush(outfile);
      write(implemfile,' : ');
      write_cast_type(implemfile,para_def_expr^.p3);
      writeln(implemfile,';');
    end;
  writeln(outfile);
  flush(outfile);
  hp:=new(presobject,init_two(t_funcname,dname,para_def_expr));
  write_funexpr(implemfile,hp);
  writeln(implemfile);
  flush(implemfile);
  if assigned(hp)then dispose(hp,done);
end;


end.
