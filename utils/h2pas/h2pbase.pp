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

interface

uses
  SysUtils, classes,
  h2poptions,scan,h2pconst,h2plexlib,h2pyacclib, scanbase,h2pout,h2ptypes;

type
  YYSTYPE = presobject;


var
  TN,PN  : String;


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
// Returns the C logical operator && or || as aOp (and, or) of aLeft and aRight, operands that are no comparison compared to 0.
function HandleLogicalOp(const aOp : string; aLeft,aRight : presobject) : presobject;
// Returns the C logical not !aExpr: not for a comparison, a comparison to 0 otherwise.
function HandleLogicalNot(aExpr : presobject) : presobject;
// Returns aName * aRight with aName as leftmost operand of the operators in aRight that bind as weak or weaker.
function HandleNamedProduct(aName,aRight : presobject) : presobject;
// Returns (aName) aOperand: a cast of aOperand to the type aName, a product for (aName) *x, aName without operand.
function HandleParenthesizedName(aName,aOperand : presobject) : presobject;
// Returns a struct or union definition (aTyp) with the members aMembers and the tag aName, packed to aPack unless 0.
function NewRecordType(aTyp : ttyp; aMembers, aName : presobject; aPack : integer) : presobject;

// Macros
function HandleDefineMacro(dname,enum_list,para_def_expr: presobject) : presobject;
function HandleDefineConst(dname,def_expr: presobject) : presobject;
// Writes the defines that wait for the declaration of the identifier that is their value, once it is declared;
// with aAll all of them.
procedure FlushPendingDefines(aAll : boolean);
function HandleDefine(dname : presobject) : presobject;
Function CheckWideString(S : String) : presobject;
// Returns the Pascal literal of the adjacent string literals aLeft and aRight, and disposes both.
function ConcatStrings(aLeft,aRight : presobject) : presobject;
function CheckUnderScore(pdecl : presobject) : presobject;
// Returns the Pascal type for the type name aName when it is a standard C type such as uint8_t or size_t,
// and disposes aName; returns CheckUnderScore(aName) otherwise.
function MapCTypeName(aName : presobject) : presobject;
// Returns true when the name aName is a standard C type that MapCTypeName maps.
function IsCTypeName(aName : presobject) : boolean;

Function NewCType(aCType,aPascalType : String) : PresObject;

Implementation

// Disposes aNode when it is assigned, and sets it to nil.
procedure DisposeNode(var aNode : presobject);

begin
  if assigned(aNode) then
    dispose(aNode,done);
  aNode:=nil;
end;


// Returns aExpr without the expression lists of one element around it.
function UnwrappedExpr(aExpr : presobject) : presobject;

begin
  Result:=aExpr;
  while assigned(Result) and (Result^.typ=t_exprlist) and not assigned(Result^.next) and assigned(Result^.p1) do
    Result:=Result^.p1;
end;


// Returns the name of the function that the call aExpr (t_funexprlist) calls, or '' when aExpr is no call of a name.
function CalleeName(aExpr : presobject) : AnsiString;

begin
  Result:='';
  if assigned(aExpr) and (aExpr^.typ=t_funexprlist) and assigned(aExpr^.p1) and assigned(aExpr^.p1^.p1)
     and (aExpr^.p1^.p1^.typ=t_id) then
    Result:=aExpr^.p1^.p1^.str;
end;


// Returns true when aExpr is the identifier aName.
function IsNamedId(aExpr : presobject; const aName : AnsiString) : boolean;

begin
  Result:=assigned(aExpr) and (aExpr^.typ=t_id) and (aExpr^.str=aName);
end;


// Returns the number of elements of the list aList, linked by next.
function ListLength(aList : presobject) : integer;

begin
  Result:=0;
  while assigned(aList) do
    begin
    inc(Result);
    aList:=aList^.next;
    end;
end;


// Returns true when the expression aExpr is a comparison, or an and, or or not of comparisons.
function IsBooleanExpr(aExpr : presobject) : boolean;

var
  lOp : string;

begin
  Result:=false;
  aExpr:=UnwrappedExpr(aExpr);
  if assigned(aExpr) and (aExpr^.typ=t_preop) and (aExpr^.str=' not ') then
    exit(IsBooleanExpr(aExpr^.p1));
  if not assigned(aExpr) or (aExpr^.typ<>t_bop) then
    exit;
  lOp:=aExpr^.str;
  if (lOp='=') or (lOp='<>') or (lOp='<') or (lOp='<=') or (lOp='>') or (lOp='>=') then
    Result:=true
  else if (lOp=' and ') or (lOp=' or ') then
    Result:=IsBooleanExpr(aExpr^.p1) and IsBooleanExpr(aExpr^.p2);
end;


function HandleTernary(expr,colonexpr : presobject) : presobject;

begin
  if not IsBooleanExpr(expr) then
    expr:=NewBinaryOp('<>',expr,NewID('0'));
  colonexpr^.p1:=expr;
  Result:=colonexpr;
  inc(if_nb);
  result^.p:=strpnew('if_local'+IntToStr(if_nb));
end;


// Returns true when aName is the Pascal name of a floating point type.
function IsFloatTypeName(const aName : string) : boolean;

begin
  case aName of
    FLOAT_STR,DOUBLE_STR,EXTENDED_STR,cfloat_STR,cdouble_STR,clongdouble_STR :
      Result:=true;
  else
    Result:=false;
  end;
end;


// Returns true when the expression aExpr contains a floating point literal or a cast to a floating point type.
function IsFloatExpr(aExpr : presobject) : boolean;

var
  lStr : string;
  lType : presobject;

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
      begin
      lType:=aExpr^.p1;
      Result:=(assigned(lType) and (lType^.typ=t_id) and IsFloatTypeName(lType^.str)) or IsFloatExpr(aExpr^.p2);
      end;
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


function HandleLogicalOp(const aOp : string; aLeft,aRight : presobject) : presobject;

begin
  if not IsBooleanExpr(aLeft) then
    aLeft:=NewBinaryOp('<>',aLeft,NewID('0'));
  if not IsBooleanExpr(aRight) then
    aRight:=NewBinaryOp('<>',aRight,NewID('0'));
  Result:=NewBinaryOp(aOp,aLeft,aRight);
end;


function HandleLogicalNot(aExpr : presobject) : presobject;

begin
  if IsBooleanExpr(aExpr) then
    Result:=NewUnaryOp(' not ',aExpr)
  else
    Result:=NewBinaryOp('=',aExpr,NewID('0'));
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

// Returns the last node of the p1 chain of aNode.
function LastInChain(aNode : presobject) : presobject;

begin
  Result:=aNode;
  while assigned(Result^.p1) do
    Result:=Result^.p1;
end;


// Returns the declarator aType with an array of size aSizeExpr appended.
function handleSizedArrayDecl(aType,aSizeExpr: presobject): presobject;

begin
  Result:=HandleDeclarator2(t_arraydef,aType,aSizeExpr);
end;

// Returns the declarator aType with a function without arguments appended.
function handleFuncNoArg(aType: presobject): presobject;

begin
  Result:=HandleDeclarator2(t_procdef,aType,nil);
end;

// Returns the call of aType with the arguments aList.
function handleFuncExpr(aType, aList: presobject): presobject;

begin
  Result:=NewType3(t_funexprlist,NewType1(t_exprlist,aType),aList,nil);
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

// Returns the declarator aType with an open array appended, written as a pointer.
function handleArrayDecl(aType: presobject): presobject;

begin
  Result:=HandleDeclarator(t_pointerdef,aType);
  LastInChain(Result)^.openarray:=true;
end;

// Returns the abstract declarator psym with a pointer appended.
function HandlePointerAbstractDeclarator(psym: presobject): presobject;

begin
  Result:=HandleDeclarator(t_pointerdef,psym);
end;

// Returns the abstract declarator psym with a function with the arguments plist appended.
function HandlePointerAbstractListDeclarator(psym, plist: presobject): presobject;

begin
  Result:=HandleDeclarator2(t_procdef,psym,plist);
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

// Returns the bit field declarator of psym with the size psize.
function HandleSizedDeclarator(psym,psize : presobject) : presobject;

begin
  Result:=NewType3(t_dec,nil,psym,NewType1(t_size_specifier,psize));
end;


// Returns the argument list with aEl before aList.
function HandleArgList(aEl, aList: PResObject): PResObject;

begin
  Result:=NewType2(t_arglist,aEl,nil);
  Result^.next:=aList;
end;

// Returns the argument psym of type pointer to ptype.
function HandlePointerArgDeclarator(ptype, psym : presobject): presobject;

begin
  Result:=NewType2(t_arg,NewType1(t_pointerdef,ptype),psym);
end;

// Returns the declarator psym with a pointer of the ignored size psize appended.
function HandleSizedPointerDeclarator(psym, psize: presobject): presobject;

begin
  Result:=HandleSizeOverrideDeclarator(psize,psym);
end;

// Returns the declarator psym with a pointer of the ignored size psize appended.
function HandleSizeOverrideDeclarator(psize,psym : presobject) : presobject;

begin
  EmitIgnore(psize);
  dispose(psize,done);
  Result:=HandleDeclarator(t_pointerdef,psym);
end;

// Returns the declarator aLeft with a node of type aTyp with p2 aRight appended at the end of its p1 chain.
function HandleDeclarator2(aTyp : ttyp; aleft,aright: presobject): presobject;

begin
  Result:=aLeft;
  LastInChain(aLeft)^.p1:=NewType2(aTyp,nil,aRight);
end;


// Returns the declarator aRight with a node of type aTyp appended at the end of its p1 chain.
function HandleDeclarator(aTyp : ttyp; aright: presobject): presobject;

begin
  Result:=HandleDeclarator2(aTyp,aRight,nil);
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


function ConcatStrings(aLeft,aRight : presobject) : presobject;

var
  lLeft, lRight : AnsiString;

begin
  lLeft:=aLeft^.str;
  lRight:=aRight^.str;
  if (lLeft<>'') and (lLeft[length(lLeft)]='''') and (lRight<>'') and (lRight[1]='''') then
    Result:=NewID(copy(lLeft,1,length(lLeft)-1)+copy(lRight,2,length(lRight)-1))
  else
    Result:=NewID(lLeft+lRight);
  dispose(aLeft,done);
  dispose(aRight,done);
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

function IsCTypeName(aName : presobject) : boolean;

var
  i : integer;

begin
  Result:=false;
  if assigned(aName) and (aName^.typ=t_id) then
    for i:=0 to MAX_CTYPEMAPPINGS do
      if aName^.str=CTypeMappings[i].CName then
        exit(true);
end;


function MapCTypeName(aName : presobject) : presobject;

var
  i : integer;

begin
  for i:=0 to MAX_CTYPEMAPPINGS do
    if aName^.str=CTypeMappings[i].CName then
      begin
      if (CTypeMappings[i].CName='wchar_t') and Win32headers then
        Result:=NewIntID(WCHAR_STR)
      else if CTypeMappings[i].PascalName='' then
        Result:=NewVoid
      else
        Result:=NewCType(CTypeMappings[i].CTypesName,CTypeMappings[i].PascalName);
      dispose(aName,done);
      exit;
      end;
  Result:=CheckUnderScore(aName);
end;


function yylex : Integer;
begin
  yylex:=scan.yylex;
  line_no:=yylineno;
end;

// Writes msg with the current line number.
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
          and (aArg^.p2^.typ=t_dec) and IsProcPointer(aArg^.p2^.p1);
end;


procedure HoistProcVarResult(const aOwner : string; aProc : presobject; var aType : presobject); forward;


// Declares a named procedural type aOwner_param for each function pointer argument in aArgs,
// and replaces the type of that argument by the name.
procedure HoistProcVarArgs(const aOwner : string; aArgs : presobject);

var
  lArg, lDec : presobject;
  lIndex : integer;
  lName : string;

begin
  lIndex:=0;
  while assigned(aArgs) do
    begin
    Inc(lIndex);
    lArg:=aArgs^.p1;
    if assigned(lArg) and assigned(lArg^.p2) then
      begin
      lDec:=lArg^.p2;
      lName:=DeclaratorName(lDec);
      if lName='' then
        lName:=UnnamedParamName(lIndex);
      lName:=aOwner+'_'+lName;
      if IsProcVarArg(lArg) then
        begin
        HoistProcVarArgs(lName,lDec^.p1^.p1^.p2);
        HoistProcVarResult(lName,lDec^.p1^.p1,lArg^.p1);
        lName:=WriteNamedType(lName,lDec^.p1,lArg^.p1);
        dispose(lArg^.p1,done);
        lArg^.p1:=NewIntID(lName);
        dispose(lDec^.p1,done);
        lDec^.p1:=nil;
        end
      else if assigned(lDec^.p1) then
        begin
        HoistProcVarElement(lName,lDec^.p1,lArg^.p1);
        HoistPointedArray(lName,lDec^.p1,lArg^.p1);
        end;
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
  if not IsProcPointer(lResult) then
    exit;
  HoistProcVarArgs(aOwner+'_result',lResult^.p1^.p2);
  HoistProcVarResult(aOwner+'_result',lResult^.p1,aType);
  lName:=WriteNamedType(aOwner+'_result',lResult,aType);
  dispose(aType,done);
  aType:=NewIntID(lName);
  dispose(lResult,done);
  aProc^.p1:=nil;
end;


// Returns true when one of the declarators in aDecls (t_declist) is a pointer.
function HasPointerDeclarator(aDecls : presobject) : boolean;

var
  lChain : presobject;

begin
  Result:=false;
  while assigned(aDecls) and not Result do
    begin
    if assigned(aDecls^.p1) then
      begin
      lChain:=aDecls^.p1^.p1;
      while assigned(lChain) and not Result do
        begin
        Result:=lChain^.typ=t_pointerdef;
        lChain:=lChain^.p1;
        end;
      end;
    aDecls:=aDecls^.next;
    end;
end;


// Declares the struct or union defined in the member aMember (t_memberdec) of aOwner as a record type of its own
// when it has a tag or a pointer declarator, named after the tag or aOwner_member, and makes the member use it.
procedure HoistInlineRecord(const aOwner : AnsiString; aMember : presobject);

var
  lType, lDecls, lRef : presobject;
  lTagged : boolean;
  lMemberName : AnsiString;

begin
  lType:=aMember^.p1;
  lDecls:=aMember^.p2;
  if not (assigned(lType) and (lType^.typ in [t_structdef,t_uniondef]) and assigned(lType^.p1)) then
    exit;
  if not assigned(lDecls) then
    exit;
  lMemberName:=DeclaratorName(lDecls^.p1);
  if lMemberName='' then
    exit;
  lTagged:=assigned(lType^.p2) and assigned(lType^.p2^.p);
  if not lTagged and not HasPointerDeclarator(lDecls) then
    exit;
  if not lTagged then
    begin
    DisposeNode(lType^.p2);
    lType^.p2:=NewID(aOwner+'_'+lMemberName);
    end;
  lRef:=NewID(lType^.p2^.str);
  lRef^.structtag:=true;
  aMember^.p1:=lRef;
  HandleSpecialType(lType);
end;


// Declares named types for the function pointers among the members of the struct or union aType, with type names
// aOwner_member: arrays of and pointers to function pointers, and the function pointer arguments and results
// of function pointer members.
procedure HoistStructProcVarElements(const aOwner : AnsiString; aType : presobject);

var
  lMembers, lMember, lDecls, lChain : presobject;
  lName : AnsiString;
  lSingle : boolean;

begin
  if not (assigned(aType) and (aType^.typ in [t_structdef,t_uniondef])) then
    exit;
  lMembers:=aType^.p1;
  while assigned(lMembers) do
    begin
    lMember:=lMembers^.p1;
    if assigned(lMember) and (lMember^.typ=t_memberdec) then
      begin
      HoistInlineRecord(aOwner,lMember);
      lDecls:=lMember^.p2;
      lSingle:=assigned(lDecls) and not assigned(lDecls^.next);
      while assigned(lDecls) do
        begin
        lName:=DeclaratorName(lDecls^.p1);
        if lName<>'' then
          begin
          lName:=aOwner+'_'+lName;
          HoistStructProcVarElements(lName,lMember^.p1);
          lChain:=lDecls^.p1^.p1;
          if IsProcPointer(lChain) then
            begin
            HoistProcVarArgs(lName,lChain^.p1^.p2);
            if lSingle then
              HoistProcVarResult(lName,lChain^.p1,lMember^.p1);
            end
          else
            begin
            HoistProcVarElement(lName,lChain,lMember^.p1);
            HoistPointedArray(lName,lDecls^.p1^.p1,lMember^.p1);
            end;
          end;
        lDecls:=lDecls^.next;
        end;
      end;
    lMembers:=lMembers^.next;
    end;
end;


// Replaces 0 and NULL as value of the return statements in the statement tree aNode by nil.
procedure NilPointerExits(aNode : presobject);

var
  lValue : presobject;

begin
  if not assigned(aNode) then
    exit;
  if (aNode^.typ=t_preop) and (aNode^.str='exit') and assigned(aNode^.p1) then
    begin
    lValue:=UnwrappedExpr(aNode^.p1);
    if IsNamedId(lValue,'0') or IsNamedId(lValue,'NULL') then
      begin
      dispose(aNode^.p1,done);
      aNode^.p1:=NewIntID('nil');
      end;
    end;
  NilPointerExits(aNode^.p1);
  NilPointerExits(aNode^.p2);
  NilPointerExits(aNode^.p3);
  NilPointerExits(aNode^.next);
end;


// Declares named types for the function pointers among the variables aDecls of type aType: arrays of and pointers
// to function pointers, and the function pointer arguments and results of function pointer variables.
procedure HoistVariableProcVarElements(aDecls : presobject; var aType : presobject);

var
  lName : AnsiString;
  lChain : presobject;
  lSingle : boolean;

begin
  lSingle:=assigned(aDecls) and not assigned(aDecls^.next);
  while assigned(aDecls) do
    begin
    lName:=DeclaratorName(aDecls^.p1);
    if lName<>'' then
      begin
      HoistStructProcVarElements(lName,aType);
      lChain:=aDecls^.p1^.p1;
      if IsProcPointer(lChain) then
        begin
        HoistProcVarArgs(lName,lChain^.p1^.p2);
        if lSingle then
          HoistProcVarResult(lName,lChain^.p1,aType);
        end
      else
        begin
        HoistProcVarElement(lName+'_element',lChain,aType);
        HoistPointedArray(lName+'_array',aDecls^.p1^.p1,aType);
        end;
      end;
    aDecls:=aDecls^.next;
    end;
end;


// Hoists the function pointer arguments and result of the function declared by aDecl (t_declist).
procedure HoistDeclarationProcVarArgs(aDecl : presobject; var aType : presobject);

var
  lDecl : presobject;
  lName : AnsiString;

begin
  lDecl:=aDecl^.p1;
  lName:=DeclaratorName(lDecl);
  if lName<>'' then
    begin
    HoistProcVarArgs(lName,lDecl^.p1^.p2);
    HoistProcVarResult(lName,lDecl^.p1,aType);
    end;
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


type
  // A declared function, as copies of the parts of its declaration.
  PStoredFunction = ^TStoredFunction;
  TStoredFunction = record
    Decl, TypeSpec, Modifier, DeclList : presobject;
    HasBody : boolean;
  end;

var
  // The declared functions by C name, with a PStoredFunction as object.
  StoredFunctions : TStringList = nil;
  // The C name of the function that the function being written is an alias of.
  AliasTarget : AnsiString = '';

// Returns a copy of aNode, or nil.
function CopyOf(aNode : presobject) : presobject;

begin
  if assigned(aNode) then
    Result:=aNode^.get_copy
  else
    Result:=nil;
end;


// Registers the function declared by the parts of a declaration, as the target of later aliases.
procedure StoreFunction(decl, type_spec, modifier_spec, decllist_spec : presobject; aHasBody : boolean);

var
  lFunction : PStoredFunction;
  lName : AnsiString;

begin
  lName:=DeclaratorName(decllist_spec^.p1);
  if (AliasTarget<>'') or (lName='') then
    exit;
  if not assigned(StoredFunctions) then
    begin
    StoredFunctions:=TStringList.Create;
    StoredFunctions.CaseSensitive:=true;
    end;
  if StoredFunctions.IndexOf(lName)>=0 then
    exit;
  New(lFunction);
  lFunction^.Decl:=CopyOf(decl);
  lFunction^.TypeSpec:=CopyOf(type_spec);
  lFunction^.Modifier:=CopyOf(modifier_spec);
  lFunction^.DeclList:=decllist_spec^.get_copy;
  DisposeNode(lFunction^.DeclList^.next);
  lFunction^.HasBody:=aHasBody;
  StoredFunctions.AddObject(lName,TObject(lFunction));
end;


// Frees the registered functions.
procedure FreeStoredFunctions;

var
  i : integer;
  lFunction : PStoredFunction;

begin
  if not assigned(StoredFunctions) then
    exit;
  for i:=0 to StoredFunctions.Count-1 do
    begin
    lFunction:=PStoredFunction(StoredFunctions.Objects[i]);
    DisposeNode(lFunction^.Decl);
    DisposeNode(lFunction^.TypeSpec);
    DisposeNode(lFunction^.Modifier);
    dispose(lFunction^.DeclList,done);
    Dispose(lFunction);
    end;
  StoredFunctions.Free;
end;


// Returns the declared function with the C name aName, or nil.
function FindFunction(const aName : AnsiString) : PStoredFunction;

var
  lIndex : integer;

begin
  Result:=nil;
  if not assigned(StoredFunctions) then
    exit;
  lIndex:=StoredFunctions.IndexOf(aName);
  if lIndex>=0 then
    Result:=PStoredFunction(StoredFunctions.Objects[lIndex]);
end;


// Returns the procdef of the declared function aFunction: its arguments (p2) and result modifiers (p1).
function FunctionProcDef(aFunction : PStoredFunction) : presobject;

begin
  Result:=aFunction^.DeclList^.p1^.p1;
end;


// Returns true when the declared function aFunction has no result.
function IsProcedureFunction(aFunction : PStoredFunction) : boolean;

begin
  Result:=assigned(aFunction) and assigned(aFunction^.TypeSpec) and (aFunction^.TypeSpec^.typ=t_void)
          and not assigned(FunctionProcDef(aFunction)^.p1);
end;


// Returns the number of arguments of the declared function aFunction, -1 when it takes a variable number.
function ArgumentCount(aFunction : PStoredFunction) : integer;

var
  lArgs : presobject;

begin
  lArgs:=FunctionProcDef(aFunction)^.p2;
  if HasEllipsis(lArgs) then
    exit(-1);
  if IsVoidArgList(lArgs) then
    exit(0);
  Result:=ListLength(lArgs);
end;


// Returns the name of the imported symbol of the function declared by decllist_spec: the alias target, if any.
function ExternalName(decllist_spec : presobject) : AnsiString;

begin
  if AliasTarget<>'' then
    Result:=AliasTarget
  else
    Result:=decllist_spec^.p1^.p2^.p;
end;


// Returns the arguments of a call that passes the parameters aArgs (t_arglist) on, as written by write_args;
// an ellipsis passes the array of const args when aArrayOfConst is set, and nothing otherwise.
function CallArguments(aArgs : presobject; aArrayOfConst : boolean) : AnsiString;

var
  lIndex : integer;
  lArg : presobject;

begin
  Result:='';
  if not assigned(aArgs) or IsVoidArgList(aArgs) then
    exit;
  lIndex:=1;
  while assigned(aArgs) do
    begin
    lArg:=aArgs^.p1;
    if not assigned(lArg^.p1) then
      begin
      (* the ellipsis *)
      if aArrayOfConst then
        begin
        if Result<>'' then
          Result:=Result+',';
        Result:=Result+'args';
        end;
      break;
      end;
    if Result<>'' then
      Result:=Result+',';
    if assigned(lArg^.p2^.p2) then
      Result:=Result+FixId(lArg^.p2^.p2^.p)
    else
      Result:=Result+UnnamedParamName(lIndex);
    inc(lIndex);
    aArgs:=aArgs^.next;
    end;
  Result:='('+Result+')';
end;



// Writes the function aAlias as an alias of the declared function aFunction with the C name aTarget:
// an import of the same symbol, or a function that calls it.
procedure WriteFunctionAlias(const aAlias : AnsiString; aFunction : PStoredFunction; const aTarget : AnsiString);

var
  lDeclList, lModifier : presobject;
  lUseLib, lDynLib : boolean;

begin
  lDeclList:=aFunction^.DeclList^.get_copy;
  dispose(lDeclList^.p1^.p2,done);
  lDeclList^.p1^.p2:=NewID(aAlias);
  lModifier:=CopyOf(aFunction^.Modifier);
  lUseLib:=UseLib;
  lDynLib:=createdynlib;
  if aFunction^.HasBody then
    begin
    UseLib:=false;
    createdynlib:=false;
    end;
  AliasTarget:=aTarget;
  if aFunction^.HasBody then
    HandleDeclarationSysTrap(NewID('intern'),CopyOf(aFunction^.TypeSpec),lModifier,lDeclList,nil)
  else
    HandleDeclarationSysTrap(CopyOf(aFunction^.Decl),CopyOf(aFunction^.TypeSpec),lModifier,lDeclList,nil);
  AliasTarget:='';
  UseLib:=lUseLib;
  createdynlib:=lDynLib;
  DisposeNode(lModifier);
end;


// Returns true when the macro with the parameters aParams (t_enumlist) and the body aBody only calls a function
// with its parameters, in order; aTarget is the name of that function.
function IsWrapperMacro(aParams, aBody : presobject; var aTarget : AnsiString) : boolean;

var
  lArgs : presobject;
  lCallee : AnsiString;

begin
  Result:=false;
  aTarget:='';
  aBody:=UnwrappedExpr(aBody);
  lCallee:=CalleeName(aBody);
  if (lCallee='') or assigned(aBody^.p3) then
    exit;
  lArgs:=aBody^.p2;
  while assigned(lArgs) and assigned(aParams) do
    begin
    if not assigned(aParams^.p1) or not IsNamedId(UnwrappedExpr(lArgs^.p1),aParams^.p1^.str) then
      exit;
    lArgs:=lArgs^.next;
    aParams:=aParams^.next;
    end;
  Result:=not assigned(lArgs) and not assigned(aParams);
  if Result then
    aTarget:=lCallee;
end;


// Returns true when aNode is the specifier or modifier aText.
function HasSpecifier(aNode : presobject; const aText : AnsiString) : boolean;

begin
  Result:=assigned(aNode) and (aNode^.str=aText);
end;


// Returns true when the declaration list aDeclList declares a function.
function IsFunctionDeclList(aDeclList : presobject) : boolean;

begin
  Result:=assigned(aDeclList) and assigned(aDeclList^.p1) and assigned(aDeclList^.p1^.p1)
          and (aDeclList^.p1^.p1^.typ=t_procdef);
end;


// Writes the arguments aArgs and, unless aIsProcedure, the result type with modifiers aResult and type aType to aFile.
procedure WriteSignature(var aFile : text; aArgs, aResult, aType : presobject; aIsProcedure, aSkipEllipsis : boolean);

var
  lOldInArgs : boolean;

begin
  if assigned(aArgs) then
    write_args(aFile,aArgs,aSkipEllipsis);
  if aIsProcedure then
    exit;
  write(aFile,':');
  lOldInArgs:=in_args;
  (* write pointers as P.... instead of ^.... *)
  in_args:=true;
  write_p_a_def(aFile,aResult,aType);
  in_args:=lOldInArgs;
end;


// Writes the function declared by aDeclList with result type aType: a procedure variable for -P, an external when
// aExtern, else its header in both files and aBody (t_statement_list) or a stub in the implementation.
// aSysTrap is the PalmOS trap, aVarArgs adds varargs, aSkipEllipsis leaves the ellipsis argument out.
procedure WriteFunctionDeclaration(aDeclList, aType, aSysTrap, aBody : presobject;
                                   aExtern, aVarArgs, aSkipEllipsis : boolean);

var
  lProc, lArgs : presobject;
  lName, lPascalName, lKeyword : AnsiString;
  lIsProcedure : boolean;

begin
  lName:=aDeclList^.p1^.p2^.p;
  lPascalName:=UniqueName(lName,lName);
  lProc:=aDeclList^.p1^.p1;
  lArgs:=lProc^.p2;
  lIsProcedure:=assigned(aType) and (aType^.typ=t_void) and not assigned(lProc^.p1);
  if lIsProcedure then
    lKeyword:='procedure'
  else
    lKeyword:='function';
  if createdynlib then
    WriteSectionMarker(outfile,'V')
  else
    WriteSectionMarker(outfile,'F');
  if (block_type<>bt_func) and not createdynlib then
    begin
    writeln(outfile);
    block_type:=bt_func;
    end;
  (* dyn. procedures must be put into a var block *)
  if createdynlib then
    begin
    OpenSection(bt_var,'var');
    shift(2);
    end;
  if not CompactMode then
    begin
    write(outfile,aktspace);
    if not aExtern then
      write(implemfile,aktspace);
    end;
  if assigned(aType) then
    begin
    if createdynlib then
      write(outfile,lPascalName,' : ',lKeyword)
    else
      begin
      shift(length(lKeyword)+1);
      write(outfile,lKeyword,' ',lPascalName);
      end;
    WriteSignature(outfile,lArgs,lProc^.p1,aType,lIsProcedure,aSkipEllipsis);
    if createdynlib then
      begin
      loaddynlibproc.add('pointer('+lPascalName+'):=GetProcAddress(hlib,'''+ExternalName(aDeclList)+''');');
      freedynlibproc.add(lPascalName+':=nil;');
      end
    else if not aExtern then
      begin
      write(implemfile,lKeyword,' ',lPascalName);
      WriteSignature(implemfile,lArgs,lProc^.p1,aType,lIsProcedure,aSkipEllipsis);
      end;
    end;
  if assigned(aSysTrap) then
    write(outfile,';systrap ',aSysTrap^.p);
  WriteCallingConvention(aExtern);
  if aVarArgs then
    write(outfile,';varargs');
  popshift;
  if createdynlib then
    writeln(outfile,';')
  else if UseLib then
    begin
    if aExtern then
      begin
      write(outfile,';external');
      if UseName then
        write(outfile,' External_library name ''',ExternalName(aDeclList),'''')
      else if AliasTarget<>'' then
        write(outfile,' name ''',AliasTarget,'''')
      else if lPascalName<>lName then
        write(outfile,' name ''',lName,'''');
      end;
    writeln(outfile,';');
    end
  else
    begin
    writeln(outfile,';');
    if not aExtern then
      begin
      writeln(implemfile,';');
      if assigned(aBody) then
        begin
        shift(2);
        if aBody^.typ=t_statement_list then
          write_statement_block(implemfile,aBody);
        popshift;
        end
      else
        begin
        writeln(implemfile,aktspace,'begin');
        if AliasTarget='' then
          writeln(implemfile,aktspace,'  { You must implement this function }')
        else if lIsProcedure then
          writeln(implemfile,aktspace,'  ',AliasTarget,CallArguments(lArgs,not aSkipEllipsis),';')
        else
          writeln(implemfile,aktspace,'  ',lPascalName,':=',AliasTarget,CallArguments(lArgs,not aSkipEllipsis),';');
        writeln(implemfile,aktspace,'end;');
        end;
      end;
    end;
  if not compactmode and not createdynlib then
    writeln(outfile);
end;


// Writes the variables of the declaration list aDeclList with type aType; decl is the extern or static specifier.
procedure WriteVariables(decl : presobject; var aType : presobject; aDeclList : presobject);

var
  hp : presobject;
  lName, lPascalName : AnsiString;

begin
  HoistVariableProcVarElements(aDeclList,aType);
  shift(2);
  WriteSectionMarker(outfile,'V');
  OpenSection(bt_var,'var');
  shift(2);
  hp:=aDeclList;
  while assigned(hp) and assigned(hp^.p1) do
    begin
    lName:=DeclaratorName(hp^.p1);
    lPascalName:='';
    if lName<>'' then
      begin
      lPascalName:=UniqueName(lName,lName);
      write(outfile,aktspace,lPascalName);
      end;
    write(outfile,' : ');
    shift(2);
    is_procvar:=false;
    write_p_a_def(outfile,hp^.p1^.p1,aType);
    WriteProcVarDirectives(outfile,false);
    (* a renamed variable keeps its C name as symbol *)
    if lName<>'' then
      if HasSpecifier(decl,'extern') then
        if lPascalName<>lName then
          write(outfile,';external name ''',lName,'''')
        else
          write(outfile,';cvar;external')
      else if not HasSpecifier(decl,'static') then
        if lPascalName<>lName then
          write(outfile,';public name ''',lName,'''')
        else
          write(outfile,';cvar;public');
    writeln(outfile,';');
    popshift;
    hp:=hp^.next;
    end;
  popshift;
  popshift;
end;


function HandleDeclarationStatement(decl, type_spec, modifier_spec,
  decllist_spec, block_spec: presobject): presobject;

var
  lSkipEllipsis, lDone : boolean;
  lUseLib, lDynLib : boolean;

begin
  HandleDeclarationStatement:=Nil;
  (* a function with a body is implemented here: not external, no procedure variable *)
  lUseLib:=UseLib;
  lDynLib:=createdynlib;
  UseLib:=false;
  createdynlib:=false;
  (* by default we must pop the args pushed on stack *)
  no_pop:=false;
  if IsFunctionDeclList(decllist_spec) then
    begin
    if assigned(decllist_spec^.p1^.p1^.p1) and (decllist_spec^.p1^.p1^.p1^.typ=t_pointerdef) then
      NilPointerExits(block_spec);
    HoistDeclarationProcVarArgs(decllist_spec,type_spec);
    StoreFunction(decl,type_spec,modifier_spec,decllist_spec,true);
    lSkipEllipsis:=false;
    repeat
      no_pop:=HasSpecifier(modifier_spec,'no_pop');
      WriteFunctionDeclaration(decllist_spec,type_spec,nil,block_spec,false,false,lSkipEllipsis);
      lDone:=lSkipEllipsis or not HasEllipsis(decllist_spec^.p1^.p1^.p2);
      lSkipEllipsis:=true;
    until lDone;
    end
  else if assigned(decllist_spec) and assigned(decllist_spec^.p1) then
    WriteVariables(decl,type_spec,decllist_spec);
  DisposeNode(decl);
  DisposeNode(type_spec);
  DisposeNode(modifier_spec);
  DisposeNode(decllist_spec);
  DisposeNode(block_spec);
  UseLib:=lUseLib;
  createdynlib:=lDynLib;
end;

function HandleDeclarationSysTrap(decl, type_spec, modifier_spec,
  decllist_spec, sys_trap: presobject): presobject;

var
  lExtern, lSkipEllipsis, lDone, lVarArgs : boolean;
  lName : AnsiString;

begin
  HandleDeclarationSysTrap:=Nil;
  (* by default we must pop the args pushed on stack *)
  no_pop:=false;
  if IsFunctionDeclList(decllist_spec) and HasSpecifier(decl,'static') then
    begin
    lName:=DeclaratorName(decllist_spec^.p1);
    if lName<>'' then
      writeln(outfile,aktspace,'(* static function ',lName,' ignored *)');
    end
  else if IsFunctionDeclList(decllist_spec) then
    begin
    HoistDeclarationProcVarArgs(decllist_spec,type_spec);
    StoreFunction(decl,type_spec,modifier_spec,decllist_spec,false);
    lExtern:=UseLib or HasSpecifier(decl,'extern');
    lVarArgs:=HasEllipsis(decllist_spec^.p1^.p1^.p2) and (lExtern or createdynlib);
    lSkipEllipsis:=lVarArgs;
    repeat
      no_pop:=HasSpecifier(modifier_spec,'no_pop');
      WriteFunctionDeclaration(decllist_spec,type_spec,sys_trap,nil,lExtern,lVarArgs,lSkipEllipsis);
      lDone:=lSkipEllipsis or createdynlib or not HasEllipsis(decllist_spec^.p1^.p1^.p2);
      lSkipEllipsis:=true;
    until lDone;
    end
  else if assigned(decllist_spec) and assigned(decllist_spec^.p1) then
    WriteVariables(decl,type_spec,decllist_spec);
  DisposeNode(decl);
  DisposeNode(type_spec);
  DisposeNode(decllist_spec);
end;


function HandleSpecialType(aType: presobject) : presobject;

var
  lMoved, lNamed, lRecord : boolean;
  lBlockType : tblocktype;
  lName : AnsiString;

begin
  HandleSpecialType:=Nil;
  lNamed:=assigned(aType^.p2) and assigned(aType^.p2^.p);
  lRecord:=aType^.typ in [t_uniondef,t_structdef];
  lName:='';
  if lNamed then
    lName:=TypeName(aType^.p2^.p);
  (* a struct used before its declaration moves to its first use *)
  lMoved:=lRecord and assigned(aType^.p1) and lNamed and CanMoveRecord(lName,aType);
  lBlockType:=block_type;
  WriteSectionMarker(outfile,'T');
  if lMoved then
    begin
    WriteMovedRecordStart(outfile,lName);
    block_type:=bt_type;
    end
  else
    OpenSection(bt_type,'type');
  if lRecord and lNamed then
    begin
    shift(2);
    TN:=lName;
    PN:=PointerName(aType^.p2^.p);
    (* define a Pointer type also for structs *)
    if UsePPointers and not SameText(TN,PN) then
      WritePointerTypeDef(outfile,PN,TN);
    WriteRecordMarker(outfile,TN);
    popshift;
    end;
  if lNamed then
    HoistStructProcVarElements(aType^.p2^.str,aType);
  shift(2);
  if assigned(aType^.p2) then
    begin
    (* write new type name *)
    TN:=TypeName(aType^.p2^.p);
    write(outfile,aktspace,TN,' = ');
    shift(2);
    write_type_specifier(outfile,aType);
    popshift;
    (* enum_to_const can make a switch to const *)
    if block_type=bt_type then
      begin
      writeln(outfile,';');
      WritePointerMarker(outfile,TN);
      end;
    writeln(outfile);
    popshift;
    if lMoved then
      begin
      WriteMovedRecordEnd(outfile,TN);
      block_type:=lBlockType;
      end;
    if must_write_packed_field then
      write_packed_fields_info(outfile,aType,TN);
    dispose(aType,done)
    end
  else
    begin
    TN:=TypeName(aType^.str);
    PN:=PointerName(aType^.str);
    if UsePPointers then
      WritePointerTypeDef(outfile,PN,TN);
    WriteUndefinedRecord(outfile,aktspace,TN);
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



function HandleTypedef(type_spec,dec_modifier,declarator,arg_decl_list: presobject) : presobject;
var
  lTail : presobject;
  lName : AnsiString;

begin
  HandleTypedef:=nil;
  if IsVoidArgList(arg_decl_list) then
    begin
    dispose(arg_decl_list,done);
    arg_decl_list:=nil;
    end;
  (* TYPEDEF type_specifier LKLAMMER dec_modifier declarator RKLAMMER maybe_space LKLAMMER argument_declaration_list RKLAMMER SEMICOLON *)
  WriteSectionMarker(outfile,'T');
  OpenSection(bt_type,'type');
  if assigned(declarator) and assigned(declarator^.p2) and assigned(declarator^.p2^.p) then
    HoistProcVarArgs(declarator^.p2^.str,arg_decl_list);
  no_pop:=assigned(dec_modifier) and (dec_modifier^.str='no_pop');
  shift(2);
  if assigned(declarator) then
  begin
    lTail:=declarator;
    while assigned(lTail^.p1) do
      lTail:=lTail^.p1;
    lTail^.p1:=NewType2(t_procdef,nil,arg_decl_list);
    if DeclaratorName(declarator)<>'' then
      begin
      popshift;
      HoistProcVarElement(declarator^.p2^.str+'_element',declarator^.p1,type_spec);
      shift(2);
      end;
    WrapFunctionType(declarator);
    if (DeclaratorName(declarator)<>'') and IsProcPointer(declarator^.p1) then
      begin
      popshift;
      HoistProcVarArgs(declarator^.p2^.str,declarator^.p1^.p1^.p2);
      HoistProcVarResult(declarator^.p2^.str,declarator^.p1^.p1,type_spec);
      shift(2);
      end;
    if assigned(declarator^.p1) and assigned(declarator^.p1^.p1) then
      begin
        writeln(outfile);
        (* write new type name *)
        lName:=TypeName(declarator^.p2^.p);
        write(outfile,aktspace,lName,' = ');
        shift(2);
        write_p_a_def(outfile,declarator^.p1,type_spec);
        popshift;
        WriteProcVarDirectives(outfile,no_pop);
        writeln(outfile,';');
        WritePointerMarker(outfile,lName);
      end;
  end;
  popshift;
  DisposeNode(type_spec);
  DisposeNode(dec_modifier);
  if assigned(declarator)then (* disposes also arg_decl_list *)
  dispose(declarator,done);
end;

function HandleTypedefList(type_spec,dec_modifier,declarator_list: presobject) : presobject;

(* TYPEDEF type_specifier dec_modifier declarator_list SEMICOLON *)

var
  hp,ph : presobject;
  lDecl : presobject;
  lFunctionType, lInlineType : boolean;
  lName, lTypeName, lMainName : AnsiString;

begin
  HandleTypedefList:=Nil;
  lDecl:=nil;
  if assigned(declarator_list) then
    lDecl:=declarator_list^.p1;
  (* after a syntax error the declarator list can be missing *)
  if not assigned(type_spec) or
     (not assigned(type_spec^.p2) and not (assigned(lDecl) and assigned(lDecl^.p2))) then
    begin
    if not stripinfo then
      writeln(outfile,'(* typedef without name at line ',line_no,' ignored *)');
    DisposeNode(type_spec);
    DisposeNode(dec_modifier);
    DisposeNode(declarator_list);
    exit;
    end;
  if type_spec^.typ=t_enumdef then
    begin
    hp:=declarator_list;
    while assigned(hp) do
      begin
      lName:=DeclaratorName(hp^.p1);
      if lName<>'' then
        RegisterEnumTypeName(lName,type_spec^.p1);
      hp:=hp^.next;
      end;
    end;
  (* typedef unsigned char Byte: the Pascal name is the type itself *)
  if (type_spec^.typ=t_id) and assigned(lDecl) and not assigned(lDecl^.p1) and assigned(lDecl^.p2)
     and not assigned(declarator_list^.next) then
    begin
    lName:=TypeName(lDecl^.p2^.p);
    if type_spec^.skiptprefix then
      lTypeName:=type_spec^.str
    else
      lTypeName:=TypeName(type_spec^.str);
    if SameText(lName,lTypeName) then
      begin
      if not stripinfo then
        writeln(outfile,aktspace,'(* typedef ',lName,' of the same Pascal type ignored *)');
      dispose(type_spec,done);
      DisposeNode(dec_modifier);
      dispose(declarator_list,done);
      exit;
      end;
    end;
  (* typedef struct tag *name: the struct tag without declaration yet *)
  if (type_spec^.typ=t_id) and type_spec^.structtag and not IsDeclaredType(TypeName(type_spec^.p)) then
    begin
    shift(2);
    WriteOpaqueMarker(outfile,TypeName(type_spec^.p),'');
    popshift;
    block_type:=bt_no;
    end;
  WriteSectionMarker(outfile,'T');
  if block_type=bt_type then
    writeln(outfile)
  else
    OpenSection(bt_type,'type');
  if assigned(type_spec^.p2) and assigned(type_spec^.p2^.p) then
    HoistStructProcVarElements(type_spec^.p2^.str,type_spec)
  else if DeclaratorName(lDecl)<>'' then
    HoistStructProcVarElements(lDecl^.p2^.str,type_spec);
  hp:=declarator_list;
  while assigned(hp) do
    begin
    lName:=DeclaratorName(hp^.p1);
    if lName<>'' then
      HoistProcVarElement(lName+'_element',hp^.p1^.p1,type_spec);
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
  if UsePPointers and not SameText(TN,PN) and (type_spec^.typ<>t_procdef) then
    WritePointerTypeDef(outfile,PN,TN);
  if (type_spec^.typ in [t_uniondef,t_structdef]) and assigned(type_spec^.p1)
     and (lInlineType or not (assigned(lDecl) and assigned(lDecl^.p1))) then
    WriteRecordMarker(outfile,TN);
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
  (* write the other names as aliases *)
  lMainName:=TypeName(ph^.p);
  hp:=declarator_list;
  while assigned(hp) do
  begin
    if assigned(hp^.p1) and assigned(hp^.p1^.p2) then
      begin
        PN:=lMainName;
        TN:=TypeName(hp^.p1^.p2^.p);
        if not SameText(TN,PN) then
        begin
          WriteSectionMarker(outfile,'T');
          if block_type<>bt_type then
            begin
            WriteSectionKeyword(outfile,OuterIndent(aktspace),'type');
            block_type:=bt_type;
            end;
          write(outfile,aktspace,TN,' = ');
          write_p_a_def(outfile,hp^.p1^.p1,ph);
          writeln(outfile,';');
          PN:=PointerName(hp^.p1^.p2^.p);
          if UsePPointers and not SameText(TN,PN) and (type_spec^.typ<>t_procdef) then
            WritePointerTypeDef(outfile,PN,TN);
          WritePointerMarker(outfile,TN);
        end;
      end;
    hp:=hp^.next;
  end;
  popshift;
  if must_write_packed_field then
    write_packed_fields_info(outfile,type_spec,ph^.str);
  DisposeNode(type_spec);
  DisposeNode(dec_modifier);
  DisposeNode(declarator_list);
end;

function HandleStructDef(dname1,dname2 : presobject) : presobject;

begin
  HandleStructDef:=nil;
  (* TYPEDEF STRUCT dname dname SEMICOLON *)
  WriteSectionMarker(outfile,'T');
  PN:=TypeName(dname1^.p);
  TN:=TypeName(dname2^.p);
  if IsDeclaredType(PN) and (block_type<>bt_type) and not SameText(TN,PN) then
    begin
      if not(compactmode) then
        writeln(outfile);
      WriteSectionKeyword(outfile,aktspace,'type');
      block_type:=bt_type;
    end;
  if not IsDeclaredType(PN) then
  begin
    (* a struct without declaration yet: an empty record with its own type keyword, unless it is declared later *)
    shift(2);
    if SameText(TN,PN) then
      WriteOpaqueMarker(outfile,PN,'')
    else
      WriteOpaqueMarker(outfile,PN,TN);
    popshift;
    block_type:=bt_no;
  end
  else if not SameText(tn,pn) then
  begin
    shift(2);
    writeln(outfile,aktspace,TN,' = ',PN,';');
    WritePointerMarker(outfile,TN);
    popshift;
  end;
  DisposeNode(dname1);
  DisposeNode(dname2);
end;

function HandleSimpleTypeDef(tname : presobject) : presobject;

begin
  HandleSimpleTypeDef:=Nil;
  WriteSectionMarker(outfile,'T');
  if block_type=bt_type then
    writeln(outfile)
  else
    OpenSection(bt_type,'type');
  shift(2);
  (* write as pointer *)
  writeln(outfile,'(* generic typedef  *)');
  writeln(outfile,aktspace,tname^.p,' = pointer;');
  WritePointerMarker(outfile,tname^.p);
  popshift;
  DisposeNode(tname);
end;

function HandleErrorDecl(e1,e2 : presobject) : presobject;

begin
  HandleErrorDecl:=Nil;
  EmitErrorEnd('in declaration at line '+IntToStr(line_no)+' *)');
  in_space_define:=0;
  in_define:=false;
  arglevel:=0;
  if_nb:=0;
  resetshift;
  yyerrok;
end;

var
  // Names of the defines converted so far, as written in the header.
  // Names of the defines without value, such as calling convention macros.
  EmptyDefines : TStringList = nil;

function HandleDefine(dname : presobject) : presobject;

begin
  HandleDefine:=Nil;
  writeln(outfile,'{$define ',dname^.p,'}',aktspace,commentstr);
  if not assigned(EmptyDefines) then
    begin
    EmptyDefines:=TStringList.Create;
    EmptyDefines.CaseSensitive:=true;
    end;
  EmptyDefines.Add(dname^.str);
  DisposeNode(dname);
end;

// Returns true, and writes a comment, when the define dname has the Pascal name of an earlier identifier
// that differs from it in case only; registers the name otherwise.
function IsDefineNameClash(dname : presobject) : boolean;

begin
  Result:=IsNameClash(dname^.str);
  if not Result then
    RegisterName(dname^.str)
  else if not stripinfo then
    writeln(outfile,aktspace,'(* #define ',dname^.p,' ignored, the Pascal name of ',RegisteredName(dname^.str),' *)');
end;


var
  // The names of the defines written as constants or functions, with the conditional section as object.
  WrittenDefines : TStringList = nil;

// Returns true, and writes a comment, when a define of the name of dname was written before in the same
// conditional section; registers the name and section otherwise.
function IsRedefinedDefine(dname : presobject) : boolean;

var
  lIndex : integer;

begin
  if not assigned(WrittenDefines) then
    begin
    WrittenDefines:=TStringList.Create;
    WrittenDefines.CaseSensitive:=true;
    WrittenDefines.Sorted:=true;
    end;
  lIndex:=WrittenDefines.IndexOf(dname^.str);
  Result:=(lIndex>=0) and (PtrInt(WrittenDefines.Objects[lIndex])=CondSection);
  if Result then
    begin
    if not stripinfo then
      writeln(outfile,aktspace,'(* #define ',dname^.p,' ignored, defined before *)');
    end
  else if lIndex>=0 then
    WrittenDefines.Objects[lIndex]:=TObject(PtrInt(CondSection))
  else
    WrittenDefines.AddObject(dname^.str,TObject(PtrInt(CondSection)));
end;


// Writes the define dname of the type name aType as a type alias.
procedure WriteDefineTypeAlias(dname, aType : presobject);

var
  lMapped : presobject;

begin
  WriteSectionMarker(outfile,'T');
  if block_type<>bt_type then
    begin
    if block_type<>bt_func then
      writeln(outfile);
    WriteSectionKeyword(outfile,aktspace,'type');
    block_type:=bt_type;
    end;
  shift(2);
  TN:=TypeName(dname^.p);
  write(outfile,aktspace,TN,' = ');
  if IsCTypeName(aType) then
    begin
    lMapped:=MapCTypeName(NewID(aType^.str));
    write_type_specifier(outfile,lMapped);
    dispose(lMapped,done);
    end
  else
    write_type_specifier(outfile,aType);
  writeln(outfile,';',aktspace,commentstr);
  WritePointerMarker(outfile,TN);
  popshift;
end;


// Writes the define dname with the constant value aValue as a constant.
procedure WriteDefineConstant(dname, aValue : presobject);

begin
  if IsPlainConstExpr(aValue) then
    begin
    WriteSectionMarker(outfile,'C');
    RegisterPlainConst(dname^.str);
    end
  else
    WriteSectionMarker(outfile,'D');
  if block_type<>bt_const then
    begin
    if block_type<>bt_func then
      writeln(outfile);
    WriteSectionKeyword(outfile,aktspace,'const');
    end;
  block_type:=bt_const;
  shift(2);
  write(outfile,aktspace,FixId(dname^.p),' = ');
  write_expr(outfile,aValue);
  writeln(outfile,';',aktspace,commentstr);
  popshift;
end;


// Writes the define dname with the value def_expr as a function without parameters, and disposes both.
procedure WriteDefineFunction(dname, def_expr : presobject);

var
  lFunc : presobject;

begin
  WriteSectionMarker(outfile,'F');
  if block_type<>bt_func then
    writeln(outfile);
  if not stripinfo then
    begin
    writeln(outfile,aktspace,'{ was #define dname def_expr }');
    writeln(implemfile,aktspace,'{ was #define dname def_expr }');
    end;
  block_type:=bt_func;
  write(outfile,aktspace,'function ',FixId(dname^.p));
  write(implemfile,aktspace,'function ',FixId(dname^.p));
  shift(2);
  if not assigned(def_expr^.p3) then
    begin
    writeln(outfile,' : longint; { return type might be wrong }');
    writeln(implemfile,' : longint; { return type might be wrong }');
    end
  else
    begin
    write(outfile,' : ');
    write_cast_type(outfile,def_expr^.p3);
    writeln(outfile,';',aktspace,commentstr);
    write(implemfile,' : ');
    write_cast_type(implemfile,def_expr^.p3);
    writeln(implemfile,';');
    end;
  writeln(outfile);
  lFunc:=NewType2(t_funcname,dname,def_expr);
  write_funexpr(implemfile,lFunc);
  popshift;
  dispose(lFunc,done);
  writeln(implemfile);
end;


type
  // A define that waits for the declaration of the identifier that is its value.
  PPendingDefine = ^TPendingDefine;
  TPendingDefine = record
    Name, Value : presobject;
  end;

var
  // The waiting defines, in the order of the header.
  PendingDefines : TFPList = nil;

// Returns true when aName is the name of a waiting define.
function IsPendingDefine(const aName : AnsiString) : boolean;

var
  i : integer;

begin
  Result:=false;
  if assigned(PendingDefines) then
    for i:=0 to PendingDefines.Count-1 do
      if PPendingDefine(PendingDefines[i])^.Name^.str=aName then
        exit(true);
end;


// Returns true when the identifier aName is declared, and is no waiting define.
function IsDeclaredName(const aName : AnsiString) : boolean;

begin
  Result:=((RegisteredName(aName)<>'') or (RegisteredName(FixId(aName))<>'')) and not IsPendingDefine(aName);
end;


procedure FlushPendingDefines(aAll : boolean);

var
  i : integer;
  lDefine : PPendingDefine;
  lDone : boolean;
  lTarget : AnsiString;

begin
  if not assigned(PendingDefines) then
    exit;
  repeat
    lDone:=true;
    i:=0;
    while i<PendingDefines.Count do
      begin
      lDefine:=PPendingDefine(PendingDefines[i]);
      lTarget:=UnwrappedExpr(lDefine^.Value)^.str;
      if aAll or IsDeclaredName(lTarget) then
        begin
        PendingDefines.Delete(i);
        WriteDefineConstant(lDefine^.Name,lDefine^.Value^.p1);
        dispose(lDefine^.Name,done);
        dispose(lDefine^.Value,done);
        Dispose(lDefine);
        lDone:=false;
        end
      else
        inc(i);
      end;
  until lDone;
end;


// Makes the define dname with the value def_expr wait for the declaration of the identifier that is its value.
procedure AddPendingDefine(dname, def_expr : presobject);

var
  lDefine : PPendingDefine;

begin
  if not assigned(PendingDefines) then
    PendingDefines:=TFPList.Create;
  New(lDefine);
  lDefine^.Name:=dname;
  lDefine^.Value:=def_expr;
  PendingDefines.Add(lDefine);
end;


function HandleDefineConst(dname,def_expr: presobject) : presobject;

var
  hp : presobject;
  lName : AnsiString;
  lFunction : PStoredFunction;

begin
  HandleDefineConst:=Nil;
  (* DEFINE dname SPACE_DEFINE def_expr NEW_LINE *)
  hp:=UnwrappedExpr(def_expr);
  lName:='';
  if assigned(hp) and (hp^.typ=t_id) then
    lName:=hp^.str;
  lFunction:=nil;
  if lName<>'' then
    lFunction:=FindFunction(lName);
  if (lName<>'') and SameText(lName,dname^.str) then
    begin
    if not stripinfo then
      writeln(outfile,aktspace,'(* self-referencing #define ',dname^.p,' ignored *)');
    end
  (* the name of a define without value, as #define SQLITE_STDCALL SQLITE_APICALL: a define without value *)
  else if (lName<>'') and assigned(EmptyDefines) and (EmptyDefines.IndexOf(lName)>=0) then
    begin
    HandleDefine(dname);
    dname:=nil;
    end
  else if IsDefineNameClash(dname) or IsRedefinedDefine(dname) then
  (* the name of a declared function: a function alias *)
  else if assigned(lFunction) then
    WriteFunctionAlias(dname^.str,lFunction,lName)
  (* a type keyword, a standard C type name or a declared type: a type alias *)
  else if (lName<>'') and (hp^.skiptprefix or IsCTypeName(hp) or IsDeclaredType(TypeName(lName))) then
    WriteDefineTypeAlias(dname,hp)
  (* an identifier declared later: the constant follows its declaration *)
  else if (lName<>'') and (lName[1] in ['A'..'Z','a'..'z','_']) and not hp^.skiptprefix
          and not IsDeclaredName(lName) then
    begin
    AddPendingDefine(dname,def_expr);
    dname:=nil;
    def_expr:=nil;
    end
  else if (def_expr^.typ=t_exprlist) and def_expr^.p1^.is_const and not assigned(def_expr^.next) then
    WriteDefineConstant(dname,def_expr^.p1)
  else
    begin
    WriteDefineFunction(dname,def_expr);
    dname:=nil;
    def_expr:=nil;
    end;
  DisposeNode(dname);
  DisposeNode(def_expr);
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


// Returns a new cast type for the type aType with the modifiers aModifiers of a declarator, or nil when a modifier
// is no pointer.
function DeclaredType(aType, aModifiers : presobject) : presobject;

begin
  Result:=nil;
  if not assigned(aType) then
    exit;
  Result:=aType^.get_copy;
  while assigned(aModifiers) do
    begin
    if aModifiers^.typ<>t_pointerdef then
      begin
      dispose(Result,done);
      exit(nil);
      end;
    Result:=NewType1(t_pointerdef,Result);
    aModifiers:=aModifiers^.p1;
    end;
  if Result^.typ=t_void then
    begin
    dispose(Result,done);
    Result:=nil;
    end;
end;


// Returns the result type of the declared function aFunction as a cast type, or nil for void or an unsupported type.
function FunctionResultType(aFunction : PStoredFunction) : presobject;

begin
  Result:=DeclaredType(aFunction^.TypeSpec,FunctionProcDef(aFunction)^.p1);
end;


// Returns the type of argument aIndex (from 0) of the declared function aFunction as a cast type, or nil.
function FunctionArgumentType(aFunction : PStoredFunction; aIndex : integer) : presobject;

var
  lArgs : presobject;

begin
  Result:=nil;
  lArgs:=FunctionProcDef(aFunction)^.p2;
  if IsVoidArgList(lArgs) then
    exit;
  while assigned(lArgs) and (aIndex>0) do
    begin
    lArgs:=lArgs^.next;
    dec(aIndex);
    end;
  if not assigned(lArgs) or not assigned(lArgs^.p1) or not assigned(lArgs^.p1^.p1) then
    exit;
  if assigned(lArgs^.p1^.p2) then
    Result:=DeclaredType(lArgs^.p1^.p1,lArgs^.p1^.p2^.p1)
  else
    Result:=DeclaredType(lArgs^.p1^.p1,nil);
end;


// Returns a new type object for the type of the macro body aExpr with the parameters aParams, or nil when unknown.
function MacroResultType(aExpr, aParams : presobject) : presobject;

var
  lType : presobject;
  lFunction : PStoredFunction;

begin
  Result:=nil;
  if not assigned(aExpr) then
    exit;
  case aExpr^.typ of
    t_exprlist :
      if not assigned(aExpr^.next) then
        Result:=MacroResultType(aExpr^.p1,aParams);
    t_typespec :
      if assigned(aExpr^.p1) and not ((aExpr^.p1^.typ=t_id) and IsMacroParam(aExpr^.p1^.str,aParams)) then
        Result:=aExpr^.p1^.get_copy;
    t_preop :
      if aExpr^.str='@' then
        Result:=NewType1(t_pointerdef,NewVoid)
      else if aExpr^.str='^' then
        begin
        lType:=MacroResultType(aExpr^.p1,aParams);
        if assigned(lType) and (lType^.typ=t_pointerdef) and assigned(lType^.p1) and (lType^.p1^.typ<>t_void) then
          Result:=lType^.p1^.get_copy;
        DisposeNode(lType);
        end;
    t_funexprlist :
      if assigned(aExpr^.p3) then
        Result:=aExpr^.p3^.get_copy
      else
        begin
        lFunction:=FindFunction(CalleeName(aExpr));
        if assigned(lFunction) then
          Result:=FunctionResultType(lFunction);
        end;
  end;
end;


// Returns true when the macro body aExpr is a call of a declared function without result.
function IsProcedureCall(aExpr : presobject) : boolean;

var
  lCallee : AnsiString;

begin
  aExpr:=UnwrappedExpr(aExpr);
  lCallee:=CalleeName(aExpr);
  Result:=(lCallee<>'') and not assigned(aExpr^.p3) and IsProcedureFunction(FindFunction(lCallee));
end;


// Returns true when the cast types aLeft and aRight, either of them nil, are the same.
function SameCastType(aLeft, aRight : presobject) : boolean;

begin
  if not assigned(aLeft) or not assigned(aRight) then
    exit(aLeft=aRight);
  Result:=(aLeft^.typ=aRight^.typ) and (aLeft^.str=aRight^.str)
          and SameCastType(aLeft^.p1,aRight^.p1) and SameCastType(aLeft^.p2,aRight^.p2);
end;


// Collects the uses of the macro parameter aName in aExpr: aType is the type of the uses that give one,
// aUntyped is set by a use without type or with another type.
procedure CollectParamType(const aName : string; aExpr : presobject; var aType : presobject; var aUntyped : boolean);

  procedure AddType(aUseType : presobject);

  begin
    if not assigned(aUseType) then
      aUntyped:=true
    else if not assigned(aType) then
      aType:=aUseType
    else
      begin
      if not SameCastType(aType,aUseType) then
        aUntyped:=true;
      dispose(aUseType,done);
      end;
  end;

var
  lFunction : PStoredFunction;
  lArgs : presobject;
  lIndex : integer;

begin
  if not assigned(aExpr) or aUntyped then
    exit;
  case aExpr^.typ of
    t_id :
      if aExpr^.str=aName then
        aUntyped:=true;
    t_typespec :
      begin
      if assigned(aExpr^.p1) and (aExpr^.p1^.typ=t_pointerdef) and IsNamedId(UnwrappedExpr(aExpr^.p2),aName) then
        AddType(NewType1(t_pointerdef,NewVoid))
      else
        CollectParamType(aName,aExpr^.p2,aType,aUntyped);
      end;
    t_funexprlist :
      begin
      lFunction:=FindFunction(CalleeName(aExpr));
      if not assigned(lFunction) then
        CollectParamType(aName,aExpr^.p1,aType,aUntyped);
      lArgs:=aExpr^.p2;
      lIndex:=0;
      while assigned(lArgs) do
        begin
        if assigned(lFunction) and IsNamedId(UnwrappedExpr(lArgs^.p1),aName) then
          AddType(FunctionArgumentType(lFunction,lIndex))
        else
          CollectParamType(aName,lArgs^.p1,aType,aUntyped);
        lArgs:=lArgs^.next;
        inc(lIndex);
        end;
      end;
  else
    begin
    CollectParamType(aName,aExpr^.p1,aType,aUntyped);
    CollectParamType(aName,aExpr^.p2,aType,aUntyped);
    CollectParamType(aName,aExpr^.p3,aType,aUntyped);
    end;
  end;
  CollectParamType(aName,aExpr^.next,aType,aUntyped);
end;


// Returns a new cast type for the macro parameter aName in the macro body aBody, or nil when it is unknown.
function MacroParamType(const aName : string; aBody : presobject) : presobject;

var
  lUntyped : boolean;

begin
  Result:=nil;
  lUntyped:=false;
  CollectParamType(aName,aBody,Result,lUntyped);
  if assigned(Result) and (lUntyped or ((Result^.typ=t_id) and SameText(TypeName(Result^.str),FixId(aName)))) then
    begin
    dispose(Result,done);
    Result:=nil;
    end;
end;


// Writes the macro parameters aParams (t_enumlist) with the types aTypes, longint for a nil type.
procedure WriteMacroParams(var aFile : text; aParams : presobject; aTypes : TFPList);

var
  i : integer;

begin
  i:=0;
  while assigned(aParams) do
    begin
    write(aFile,FixId(aParams^.p1^.p));
    if assigned(aParams^.next) and SameCastType(presobject(aTypes[i]),presobject(aTypes[i+1])) then
      write(aFile,',')
    else
      begin
      write(aFile,' : ');
      if assigned(aTypes[i]) then
        write_cast_type(aFile,presobject(aTypes[i]))
      else
        write(aFile,'longint');
      if assigned(aParams^.next) then
        write(aFile,'; ');
      end;
    aParams:=aParams^.next;
    inc(i);
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
  if assigned(aExpr) and not aExpr^.grouped
     and ((aExpr^.typ=t_ifexpr)
          or ((aExpr^.typ=t_bop) and (OperatorPrecedence(aExpr^.str)<=OperatorPrecedence(aOp)))) then
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


// Writes the macro dname with the parameters aParams and the body aBody as a function alias when it only passes its
// parameters to a declared function with as many arguments; returns true when it did.
function TryWriteMacroAlias(dname, aParams, aBody : presobject) : boolean;

var
  lTarget : AnsiString;
  lFunction : PStoredFunction;

begin
  Result:=false;
  if not IsWrapperMacro(aParams,aBody,lTarget) then
    exit;
  lFunction:=FindFunction(lTarget);
  Result:=assigned(lFunction) and (ArgumentCount(lFunction)=ListLength(aParams));
  if Result then
    WriteFunctionAlias(dname^.str,lFunction,lTarget);
end;


// Prepares the body aBody of a macro with the parameters aParams for writing: casts of parameters, a void cast of
// a call and a boolean result type; returns true for a void cast of a call.
function PrepareMacroBody(aBody, aParams : presobject) : boolean;

var
  lRotatable : TFPList;
  lCast : presobject;

begin
  if assigned(aParams) then
    begin
    lRotatable:=TFPList.Create;
    aBody^.p1:=FixParamCasts(aBody^.p1,aParams,lRotatable);
    aBody^.p2:=FixParamCasts(aBody^.p2,aParams,lRotatable);
    aBody^.next:=FixParamCasts(aBody^.next,aParams,lRotatable);
    lRotatable.Free;
    (* the result type of a cast to a parameter is no type *)
    if assigned(aBody^.p3) and (aBody^.p3^.typ=t_id) and IsMacroParam(aBody^.p3^.str,aParams) then
      DisposeNode(aBody^.p3);
    end;
  (* (void)call: the call as statement *)
  lCast:=nil;
  if aBody^.typ=t_exprlist then
    lCast:=aBody^.p1;
  Result:=assigned(lCast) and (lCast^.typ=t_typespec) and assigned(lCast^.p1) and (lCast^.p1^.typ=t_void)
          and assigned(UnwrappedExpr(lCast^.p2)) and (UnwrappedExpr(lCast^.p2)^.typ=t_funexprlist);
  if Result then
    begin
    aBody^.p1:=lCast^.p2;
    lCast^.p2:=nil;
    dispose(lCast,done);
    DisposeNode(aBody^.p3);
    end;
  if not assigned(aBody^.p3) and IsBooleanExpr(aBody) then
    aBody^.p3:=NewIntID('boolean');
end;


function HandleDefineMacro(dname,enum_list,para_def_expr: presobject) : presobject;

var
  hp : presobject;
  lCount : integer;
  lResultType : presobject;
  lParamTypes : TFPList;
  lUnknownParams, lProcedure : boolean;

  // Writes the comments and the header of the function or procedure to aFile, the interface when aInterface is set.
  procedure WriteHeader(var aFile : text; aInterface : boolean);

  begin
    if not stripinfo then
      begin
      writeln(aFile,aktspace,'{ was #define dname(params) para_def_expr }');
      if lUnknownParams then
        writeln(aFile,aktspace,'{ argument types are unknown }');
      if not assigned(lResultType) and not lProcedure then
        writeln(aFile,aktspace,'{ return type might be wrong }   ');
      end;
    if aInterface then
      begin
      WriteSectionMarker(outfile,'F');
      if block_type<>bt_func then
        writeln(outfile);
      block_type:=bt_func;
      end;
    if lProcedure then
      write(aFile,aktspace,'procedure ',FixId(dname^.p))
    else
      write(aFile,aktspace,'function ',FixId(dname^.p));
    if assigned(enum_list) then
      begin
      write(aFile,'(');
      WriteMacroParams(aFile,enum_list,lParamTypes);
      write(aFile,')');
      end;
    if lProcedure then
      write(aFile,';')
    else if not assigned(lResultType) then
      write(aFile,' : longint;')
    else
      begin
      write(aFile,' : ');
      write_cast_type(aFile,lResultType);
      write(aFile,';');
      end;
    if aInterface then
      writeln(aFile,aktspace,commentstr)
    else
      writeln(aFile);
  end;

begin
  HandleDefineMacro:=Nil;
  if IsDefineNameClash(dname) or IsRedefinedDefine(dname) or TryWriteMacroAlias(dname,enum_list,para_def_expr) then
    begin
    dispose(dname,done);
    DisposeNode(enum_list);
    dispose(para_def_expr,done);
    exit;
    end;
  (* DEFINE dname LKLAMMER enum_list RKLAMMER para_def_expr NEW_LINE *)
  lProcedure:=PrepareMacroBody(para_def_expr,enum_list);
  lProcedure:=lProcedure or (not assigned(para_def_expr^.p3) and IsProcedureCall(para_def_expr));
  if lProcedure then
    lResultType:=nil
  else if assigned(para_def_expr^.p3) then
    lResultType:=para_def_expr^.p3^.get_copy
  else
    lResultType:=MacroResultType(para_def_expr,enum_list);
  lParamTypes:=TFPList.Create;
  lUnknownParams:=false;
  hp:=enum_list;
  while assigned(hp) do
    begin
    lParamTypes.Add(MacroParamType(hp^.p1^.str,para_def_expr));
    if lParamTypes.Last=nil then
      lUnknownParams:=true;
    hp:=hp^.next;
    end;
  WriteHeader(outfile,true);
  WriteHeader(implemfile,false);
  writeln(outfile);
  DisposeNode(enum_list);
  DisposeNode(lResultType);
  for lCount:=0 to lParamTypes.Count-1 do
    if assigned(lParamTypes[lCount]) then
      dispose(presobject(lParamTypes[lCount]),done);
  lParamTypes.Free;
  if lProcedure then
    DisposeNode(dname);
  hp:=NewType2(t_funcname,dname,para_def_expr);
  write_funexpr(implemfile,hp);
  writeln(implemfile);
  dispose(hp,done);
end;


function HandleParenthesizedName(aName,aOperand : presobject) : presobject;

begin
  (* (x) * y is a product rather than the cast of *y *)
  if not assigned(aOperand) then
    Result:=aName
  else if IsCTypeName(aName) then
    Result:=NewType2(t_typespec,MapCTypeName(aName),aOperand)
  else if (aOperand^.typ=t_preop) and (aOperand^.str='^') then
    begin
    Result:=NewBinaryOp('*',aName,aOperand^.p1);
    aOperand^.p1:=nil;
    dispose(aOperand,done);
    end
  else
    Result:=NewType2(t_typespec,aName,aOperand);
end;


function NewRecordType(aTyp : ttyp; aMembers, aName : presobject; aPack : integer) : presobject;

begin
  if aPack>0 then
    EmitPacked(aPack);
  Result:=NewType2(aTyp,aMembers,aName);
end;


initialization
finalization
  PendingDefines.Free;
  WrittenDefines.Free;
  EmptyDefines.Free;
  FreeStoredFunctions;
end.
