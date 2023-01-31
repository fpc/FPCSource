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
  IsExtern : boolean;
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
function HandleUnaryDefExpr(aExpr : presobject) : presobject;
function HandleTernary(expr,colonexpr : presobject) : presobject;

// Macros
function HandleDefineMacro(dname,enum_list,para_def_expr: presobject) : presobject;
function HandleDefineConst(dname,def_expr: presobject) : presobject;
function HandleDefine(dname : presobject) : presobject;
Function CheckWideString(S : String) : presobject;
function CheckUnderScore(pdecl : presobject) : presobject;

Function NewCType(aID,aIntID : String) : PresObject;

Implementation

function HandleTernary(expr,colonexpr : presobject) : presobject;

begin
  colonexpr^.p1:=expr;
  Result:=colonexpr;
  inc(if_nb);
  result^.p:=strpnew('if_local'+str(if_nb));
end;


Function NewCType(aID,aIntID : String) : PresObject;

begin
  if UseCTypesUnit then
    Result:=NewID(aID)
  else
    result:=NewIntID(aIntID);
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

begin
  hp:=aType;
  Result:=hp;
  if Not assigned(hp) then
    exit;
  s:=strpas(hp^.p);
  if UseCTypesUnit then
    begin
    if s=cint_STR then
      s:=cuint_STR
    else if s=cshort_STR then
      s:=cushort_STR
    else if s=cchar_STR then
      s:=cuchar_STR
    else if s=clong_STR then
      s:=culong_STR
    else if s=clonglong_STR then
      s:=culonglong_STR
    else if s=cint8_STR then
      s:=cuint8_STR
    else if s=cint16_STR then
      s:=cuint16_STR
    else if s=cint32_STR then
      s:=cuint32_STR
    else if s=cint64_STR then
      s:=cuint64_STR
    else
      s:='';
    end
  else
    begin
    if s=INT_STR then
      s:=UINT_STR
    else if s=SHORT_STR then
      s:=USHORT_STR
    else if s=SMALL_STR then
      s:=USMALL_STR
    else if s=CHAR_STR then
      s:=UCHAR_STR
    else if s=INT64_STR then
      s:=QWORD_STR
    else
      s:='';
  end;
  if s<>'' then
    hp^.setstr(s);
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

function CheckWideString(S: String): presobject;

begin
  if Win32headers and (s[1]='L') then
    delete(s,1,1);
  CheckWideString:=NewID(''''+copy(s,2,length(s)-2)+'''');
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


function HandleDeclarationStatement(decl, type_spec, modifier_spec,
  decllist_spec, block_spec: presobject): presobject;
var
  hp : presobject;

begin
  HandleDeclarationStatement:=Nil;
  IsExtern:=false;
  (* by default we must pop the args pushed on stack *)
  no_pop:=false;
  if (assigned(decllist_spec)and assigned(decllist_spec^.p1)and assigned(decllist_spec^.p1^.p1))
    and (decllist_spec^.p1^.p1^.typ=t_procdef) then
    begin
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
              write_args(outfile,decllist_spec^.p1^.p1^.p2);
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
            begin
              write(implemfile,'procedure ',decllist_spec^.p1^.p2^.p);
              if assigned(decllist_spec^.p1^.p1^.p2) then
                write_args(implemfile,decllist_spec^.p1^.p1^.p2);
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
              write_args(outfile,decllist_spec^.p1^.p1^.p2);
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
                  write_args(implemfile,decllist_spec^.p1^.p1^.p2);
                write(implemfile,':');

                old_in_args:=in_args;
                (* write pointers as P.... instead of ^.... *)
                in_args:=true;
                write_p_a_def(implemfile,decllist_spec^.p1^.p1^.p1,type_spec);
                in_args:=old_in_args;
              end;
          end;
        (* No CDECL in interface for Uselib *)
        if IsExtern and (not no_pop) then
          write(outfile,';cdecl');
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
      until not NeedEllipsisOverload;
    end
  else (* decllist_spec^.p1^.p1^.typ=t_procdef *)
  if assigned(decllist_spec)and assigned(decllist_spec^.p1) then
    begin
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
            write_p_a_def(outfile,hp^.p1^.p1,type_spec);
            if assigned(hp^.p1^.p2)and assigned(hp^.p1^.p2^.p)then
              begin
                  if isExtern then
                    write(outfile,';cvar;external')
                  else
                    write(outfile,';cvar;public');
              end;
            writeln(outfile,';');
            popshift;
            hp:=hp^.p2;
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
end;

function HandleDeclarationSysTrap(decl, type_spec, modifier_spec,
  decllist_spec, sys_trap: presobject): presobject;

var
  hp : presobject;

begin
  HandleDeclarationSysTrap:=Nil;
  IsExtern:=false;
  (* by default we must pop the args pushed on stack *)
  no_pop:=false;
  if (assigned(decllist_spec)and assigned(decllist_spec^.p1)and assigned(decllist_spec^.p1^.p1))
    and (decllist_spec^.p1^.p1^.typ=t_procdef) then
    begin
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
              write_args(outfile,decllist_spec^.p1^.p1^.p2);
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
            begin
              write(implemfile,'procedure ',decllist_spec^.p1^.p2^.p);
              if assigned(decllist_spec^.p1^.p1^.p2) then
                write_args(implemfile,decllist_spec^.p1^.p1^.p2);
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
              write_args(outfile,decllist_spec^.p1^.p1^.p2);
            write(outfile,':');
            write_p_a_def(outfile,decllist_spec^.p1^.p1^.p1,type_spec);
            if createdynlib then
              begin
                loaddynlibproc.add('pointer('+decllist_spec^.p1^.p2^.p+'):=GetProcAddress(hlib,'''+decllist_spec^.p1^.p2^.p+''');');
                freedynlibproc.add(decllist_spec^.p1^.p2^.p+':=nil;');
              end
            else if not IsExtern then
              begin
                write(implemfile,'function ',decllist_spec^.p1^.p2^.p);
                if assigned(decllist_spec^.p1^.p1^.p2) then
                write_args(implemfile,decllist_spec^.p1^.p1^.p2);
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
        (* No CDECL in interface for Uselib *)
        if IsExtern and (not no_pop) then
          write(outfile,';cdecl');
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
      until not NeedEllipsisOverload;
    end
  else (* decllist_spec^.p1^.p1^.typ=t_procdef *)
  if assigned(decllist_spec)and assigned(decllist_spec^.p1) then
    begin
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
            write_p_a_def(outfile,hp^.p1^.p1,type_spec);
            if assigned(hp^.p1^.p2)and assigned(hp^.p1^.p2^.p)then
              begin
                  if isExtern then
                    write(outfile,';cvar;external')
                  else
                    write(outfile,';cvar;public');
              end;
            writeln(outfile,';');
            popshift;
            hp:=hp^.p2;
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
  shift(2);
  if ( aType^.p2  <> nil ) then
    begin
    (* write new type name *)
    TN:=TypeName(aType^.p2^.p);
    PN:=PointerName(aType^.p2^.p);
    (* define a Pointer type also for structs *)
    if UsePPointers and (Uppercase(tn)<>Uppercase(pn)) and
      assigned(aType) and (aType^.typ in [t_uniondef,t_structdef]) then
    writeln(outfile,aktspace,PN,' = ^',TN,';');
    write(outfile,aktspace,TN,' = ');
    shift(2);
    hp:=aType;
    write_type_specifier(outfile,hp);
    popshift;
    (* enum_to_const can make a switch to const *)
    if block_type=bt_type then
    writeln(outfile,';');
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
    if UsePPointers then writeln(outfile,aktspace,PN,' = ^',TN,';');
    if PackRecords then
      writeln(outfile, aktspace, TN, ' = packed record')
    else
      writeln(outfile, aktspace, TN, ' = record');
    writeln(outfile, aktspace, '    {undefined structure}');
    writeln(outfile, aktspace, '  end;');
    writeln(outfile);
    popshift;
    end;
end;

function HandleTypedef(type_spec,dec_modifier,declarator,arg_decl_list: presobject) : presobject;
var
  hp : presobject;

begin
  hp:=nil;
  HandleTypedef:=nil;
  (* TYPEDEF type_specifier LKLAMMER dec_modifier declarator RKLAMMER maybe_space LKLAMMER argument_declaration_list RKLAMMER SEMICOLON *)
  if block_type<>bt_type then
    begin
      if not(compactmode) then
        writeln(outfile);
      writeln(outfile,aktspace,'type');
      block_type:=bt_type;
    end;
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
    if assigned(hp^.p1) and assigned(hp^.p1^.p1) then
      begin
        writeln(outfile);
        (* write new type name *)
        write(outfile,aktspace,TypeName(hp^.p2^.p),' = ');
        shift(2);
        write_p_a_def(outfile,hp^.p1,type_spec);
        popshift;
        (* if no_pop it is normal fpc calling convention *)
        if is_procvar and
          (not no_pop) then
          write(outfile,';cdecl');
        writeln(outfile,';');
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

var
  hp,ph : presobject;


begin
  HandleTypedefList:=Nil;
  ph:=nil;
  (* TYPEDEF type_specifier dec_modifier declarator_list SEMICOLON *)
  if block_type<>bt_type then
    begin
      if not(compactmode) then
        writeln(outfile);
      writeln(outfile,aktspace,'type');
      block_type:=bt_type;
    end
  else
    writeln(outfile);
  no_pop:=assigned(dec_modifier) and (dec_modifier^.str='no_pop');
  shift(2);
  (* Get the name to write the type definition for, try
    to use the tag name first *)
  if assigned(type_spec^.p2) then
  begin
    ph:=type_spec^.p2;
  end
  else
  begin
    if not assigned(declarator_list) then
      internalerror(5555);
    if not assigned(declarator_list^.p1) then
      internalerror(666);
    if not assigned(declarator_list^.p1^.p2) then
      internalerror(4444);
    ph:=declarator_list^.p1^.p2;
  end;
  (* write type definition *)
  is_procvar:=false;
  TN:=TypeName(ph^.p);
  PN:=PointerName(ph^.p);
  if UsePPointers and (Uppercase(tn)<>Uppercase(pn)) and
    assigned(type_spec) and (type_spec^.typ<>t_procdef) then
    writeln(outfile,aktspace,PN,' = ^',TN,';');
  (* write new type name *)
  write(outfile,aktspace,TN,' = ');
  shift(2);
  write_p_a_def(outfile,declarator_list^.p1^.p1,type_spec);
  popshift;
  (* if no_pop it is normal fpc calling convention *)
  if is_procvar and
    (not no_pop) then
    write(outfile,';cdecl');
  writeln(outfile,';');
  flush(outfile);
  (* write alias names, ph points to the name already used *)
  hp:=declarator_list;
  while assigned(hp) do
  begin
    if (hp<>ph) and assigned(hp^.p1^.p2) then
      begin
        PN:=TypeName(ph^.p);
        TN:=TypeName(hp^.p1^.p2^.p);
        if Uppercase(TN)<>Uppercase(PN) then
        begin
          write(outfile,aktspace,TN,' = ');
          write_p_a_def(outfile,hp^.p1^.p1,ph);
          writeln(outfile,';');
          PN:=PointerName(hp^.p1^.p2^.p);
          if UsePPointers and (Uppercase(tn)<>Uppercase(pn)) and
            assigned(type_spec) and (type_spec^.typ<>t_procdef) then
            writeln(outfile,aktspace,PN,' = ^',TN,';');
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
  if Uppercase(tn)<>Uppercase(pn) then
  begin
    shift(2);
    writeln(outfile,aktspace,PN,' = ',TN,';');
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
  flush(outfile);
  popshift;
  if assigned(tname) then
  dispose(tname,done);
end;

function HandleErrorDecl(e1,e2 : presobject) : presobject;

begin
  HandleErrorDecl:=Nil;
  writeln(outfile,'in declaration at line ',line_no,' *)');
  aktspace:='';
  in_space_define:=0;
  in_define:=false;
  arglevel:=0;
  if_nb:=0;
  aktspace:='    ';
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
            write_type_specifier(outfile,def_expr^.p3);
            writeln(outfile,';',aktspace,commentstr);
            flush(outfile);
            write(implemfile,' : ');
            write_type_specifier(implemfile,def_expr^.p3);
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

function HandleDefineMacro(dname,enum_list,para_def_expr: presobject) : presobject;

var
  hp,ph : presobject;

begin
  HandleDefineMacro:=Nil;
  hp:=nil;
  ph:=nil;
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
      write_type_specifier(outfile,para_def_expr^.p3);
      writeln(outfile,';',aktspace,commentstr);
      flush(outfile);
      write(implemfile,' : ');
      write_type_specifier(implemfile,para_def_expr^.p3);
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
