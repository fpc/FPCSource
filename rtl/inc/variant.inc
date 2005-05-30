{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by the Free Pascal development team

    This include file contains the implementation for variants
    support in FPC as far as it is part of the system unit

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

var
   variantmanager : tvariantmanager;
   
procedure printmissingvariantunit;
  begin
    writeln(stderr);
    writeln(stderr,'Program needs probably the variants unit.');
    writeln(stderr,'Include the variants unit in your uses statements');
    writeln(stderr,'as one of the first units.');
    writeln(stderr);
  end;
  
  
procedure invalidvariantop;
  begin
     printmissingvariantunit;
     HandleErrorFrame(221,get_frame);
  end;


procedure invalidvariantopnovariants;
  begin
    printmissingvariantunit;
    HandleErrorFrame(221,get_frame);
  end;
  
  
procedure vardisperror;
  begin
    printmissingvariantunit;
    HandleErrorFrame(222,get_frame);
  end;


{ ---------------------------------------------------------------------
    Compiler helper routines.
  ---------------------------------------------------------------------}

procedure varclear(var v : tvardata);
begin
   if not(v.vtype in [varempty,varerror,varnull]) then
     invalidvariantop;
end;


procedure variant_init(var v : tvardata);[Public,Alias:'FPC_VARIANT_INIT'];
  begin
     { calling the variant manager here is a problem because the static/global variants
       are initialized while the variant manager isn't assigned }
     fillchar(v,sizeof(variant),0);
  end;


procedure variant_clear(var v : tvardata);[Public,Alias:'FPC_VARIANT_CLEAR'];
  begin
    if assigned(VarClearProc) then
      VarClearProc(v);
  end;


procedure variant_addref(var v : tvardata);[Public,Alias:'FPC_VARIANT_ADDREF'];
  begin
    if assigned(VarAddRefProc) then
      VarAddRefProc(v);
  end;

{ using pointers as argument here makes life for the compiler easier }
procedure fpc_variant_copy(d,s : pointer);compilerproc;
  begin
    if assigned(VarCopyProc) then
      VarCopyProc(tvardata(d^),tvardata(s^));
  end;


Procedure fpc_write_text_variant(Len : Longint;var f : Text;const v : variant); iocheck; [Public,Alias:'FPC_WRITE_TEXT_VARIANT']; compilerproc;
  begin
    if (InOutRes<>0) then
     exit;
    case TextRec(f).mode of
      { fmAppend gets changed to fmOutPut in do_open (JM) }
      fmOutput:
        if len=-1 then
          variantmanager.write0variant(f,v)
        else
          variantmanager.writevariant(f,v,len);
      fmInput:
        InOutRes:=105
      else InOutRes:=103;
    end;
  end;


procedure fpc_vararray_get(var d : variant;const s : variant;indices : psizeint;len : sizeint);compilerproc;
  begin
    d:=variantmanager.vararrayget(s,len,indices);
  end;


procedure fpc_vararray_put(var d : variant;const s : variant;indices : psizeint;len : sizeint);compilerproc;
  begin
    variantmanager.vararrayput(d,s,len,indices);
  end;


function fpc_variant_to_dynarray(const v : variant;typeinfo : pointer) : pointer;compilerproc;
  begin
    variantmanager.vartodynarray(result,v,typeinfo);
  end;


function fpc_dynarray_to_variant(dynarr : pointer;typeinfo : pointer) : variant;compilerproc;
  begin
    variantmanager.varfromdynarray(result,dynarr,typeinfo);
  end;


function fpc_variant_to_interface(const v : variant) : iinterface;compilerproc;
  begin
    variantmanager.vartointf(result,v);
  end;


function fpc_interface_to_variant(const i : iinterface) : variant;compilerproc;
  begin
    variantmanager.varfromintf(result,i);
  end;


{ ---------------------------------------------------------------------
    Overloaded operators.
  ---------------------------------------------------------------------}


{ Integer }

operator :=(const source : byte) dest : variant;

begin
  Variantmanager.varfromInt(Dest,Source,1);
end;


operator :=(const source : shortint) dest : variant;

begin
  Variantmanager.varfromInt(Dest,Source,-1);
end;


operator :=(const source : word) dest : variant;

begin
  Variantmanager.varfromInt(Dest,Source,2);
end;


operator :=(const source : smallint) dest : variant;
begin
  Variantmanager.varfromInt(Dest,Source,-2);
end;


operator :=(const source : dword) dest : variant;
begin
  Variantmanager.varfromInt(Dest,Source,4);
end;


operator :=(const source : longint) dest : variant;
begin
  Variantmanager.varfromInt(Dest,Source,-4);
end;


operator :=(const source : qword) dest : variant;
begin
  Variantmanager.varfromWord64(Dest,Source);
end;


operator :=(const source : int64) dest : variant;
begin
  Variantmanager.varfromInt64(Dest,Source);
end;

{ Boolean }

operator :=(const source : boolean) dest : variant;
begin
  Variantmanager.varfromBool(Dest,Source);
end;


operator :=(const source : wordbool) dest : variant;

begin
  Variantmanager.varfromBool(Dest,Boolean(Source));
end;


operator :=(const source : longbool) dest : variant;

begin
  Variantmanager.varfromBool(Dest,Boolean(Source));
end;


{ Chars }

operator :=(const source : char) dest : variant;

begin
  VariantManager.VarFromPStr(Dest,Source);
end;


operator :=(const source : widechar) dest : variant;

begin
  VariantManager.VarFromWStr(Dest,Source);
end;

{ Strings }

operator :=(const source : shortstring) dest : variant;

begin
  VariantManager.VarFromPStr(Dest,Source);
end;


operator :=(const source : ansistring) dest : variant;

begin
  VariantManager.VarFromLStr(Dest,Source);
end;


operator :=(const source : widestring) dest : variant;

begin
  VariantManager.VarFromWStr(Dest,Source);
end;

{ Floats }

{$ifdef SUPPORT_SINGLE}
operator :=(const source : single) dest : variant;
begin
  VariantManager.VarFromReal(Dest,Source);
end;
{$endif SUPPORT_SINGLE}


{$ifdef SUPPORT_DOUBLE}
operator :=(const source : double) dest : variant;
begin
  VariantManager.VarFromReal(Dest,Source);
end;
{$endif SUPPORT_DOUBLE}


{$ifdef SUPPORT_EXTENDED}
operator :=(const source : extended) dest : variant;
begin
  VariantManager.VarFromReal(Dest,Source);
end;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
Operator :=(const source : comp) dest : variant;
begin
  VariantManager.VarFromReal(Dest,Source);
end;
{$endif SUPPORT_COMP}


{ Misc. }
operator :=(const source : currency) dest : variant;
  begin
    VariantManager.VarFromCurr(Dest,Source);
  end;


operator :=(const source : tdatetime) dest : variant;
  begin
    VariantManager.VarFromTDateTime(Dest,Source);
  end;

{**********************************************************************
                       from Variant assignments
 **********************************************************************}

{ Integer }

operator :=(const source : variant) dest : byte;

begin
  dest:=variantmanager.vartoint(source);
end;


operator :=(const source : variant) dest : shortint;

begin
  dest:=variantmanager.vartoint(source);
end;


operator :=(const source : variant) dest : word;

begin
  dest:=variantmanager.vartoint(source);
end;


operator :=(const source : variant) dest : smallint;

begin
  dest:=variantmanager.vartoint(source);
end;


operator :=(const source : variant) dest : dword;

begin
  dest:=variantmanager.vartoint(source);
end;


operator :=(const source : variant) dest : longint;

begin
  dest:=variantmanager.vartoint(source);
end;


operator :=(const source : variant) dest : qword;

begin
  dest:=variantmanager.vartoword64(source);
end;


operator :=(const source : variant) dest : int64;

begin
  dest:=variantmanager.vartoint64(source);
end;


{ Boolean }

operator :=(const source : variant) dest : boolean;

begin
  dest:=variantmanager.vartobool(source);
end;


operator :=(const source : variant) dest : wordbool;

begin
  dest:=variantmanager.vartobool(source);
end;


operator :=(const source : variant) dest : longbool;

begin
   dest:=variantmanager.vartobool(source);
end;


{ Chars }

operator :=(const source : variant) dest : char;

Var
  S : String;

begin
  VariantManager.VarToPStr(S,Source);
  If Length(S)>0 then
    Dest:=S[1];
end;


operator :=(const source : variant) dest : widechar;

Var
  WS : WideString;

begin
  VariantManager.VarToWStr(WS,Source);
  If Length(WS)>0 then
    Dest:=WS[1];
end;


{ Strings }

operator :=(const source : variant) dest : shortstring;

begin
  VariantManager.VarToPStr(Dest,Source);
end;

operator :=(const source : variant) dest : ansistring;

begin
  VariantManager.vartolstr(dest,source);
end;

operator :=(const source : variant) dest : widestring;

begin
  variantmanager.vartowstr(dest,source);
end;

{ Floats }

{$ifdef SUPPORT_SINGLE}
operator :=(const source : variant) dest : single;
begin
  dest:=variantmanager.vartoreal(source);
end;
{$endif SUPPORT_SINGLE}


{$ifdef SUPPORT_DOUBLE}
operator :=(const source : variant) dest : double;
begin
  dest:=variantmanager.vartoreal(source);
end;
{$endif SUPPORT_DOUBLE}


{$ifdef SUPPORT_EXTENDED}
operator :=(const source : variant) dest : extended;
begin
  dest:=variantmanager.vartoreal(source);
end;
{$endif SUPPORT_EXTENDED}


{$ifdef SUPPORT_COMP}
operator :=(const source : variant) dest : comp;
begin
  dest:=comp(variantmanager.vartoreal(source));
end;
{$endif SUPPORT_COMP}


{ Misc. }
operator :=(const source : variant) dest : currency;
begin
  dest:=variantmanager.vartocurr(source);
end;


{$ifdef HASOVERLOADASSIGNBYUNIQUERESULT}
operator :=(const source : variant) dest : tdatetime;
begin
  dest:=variantmanager.vartotdatetime(source);
end;
{$endif HASOVERLOADASSIGNBYUNIQUERESULT}

{**********************************************************************
                               Operators
 **********************************************************************}

operator or(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opor);
  end;

operator and(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opand);
  end;

operator xor(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opxor);
  end;

operator not(const op : variant) dest : variant;
  begin
     dest:=op;
     variantmanager.varnot(dest);
  end;

operator shl(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opshiftleft);
  end;

operator shr(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opshiftright);
  end;

operator +(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opadd);
  end;

operator -(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opsubtract);
  end;

operator *(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opmultiply);
  end;

operator /(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opdivide);
  end;

operator **(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,oppower);
  end;

operator div(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opintdivide);
  end;

operator mod(const op1,op2 : variant) dest : variant;
  begin
     dest:=op1;
     variantmanager.varop(dest,op2,opmodulus);
  end;

operator -(const op : variant) dest : variant;
  begin
     dest:=op;
     variantmanager.varneg(dest);
  end;

operator =(const op1,op2 : variant) dest : boolean;
  begin
     dest:=variantmanager.cmpop(op1,op2,opcmpeq);
  end;

operator <(const op1,op2 : variant) dest : boolean;
  begin
     dest:=variantmanager.cmpop(op1,op2,opcmplt);
  end;

operator >(const op1,op2 : variant) dest : boolean;
  begin
     dest:=variantmanager.cmpop(op1,op2,opcmpgt);
  end;

operator >=(const op1,op2 : variant) dest : boolean;
  begin
     dest:=variantmanager.cmpop(op1,op2,opcmpge);
  end;

operator <=(const op1,op2 : variant) dest : boolean;
  begin
     dest:=variantmanager.cmpop(op1,op2,opcmplt);
  end;

procedure VarArrayRedim(var A: Variant; HighBound: SizeInt);
  begin
    variantmanager.vararrayredim(a,highbound);
  end;

procedure VarCast(var dest : variant;const source : variant;vartype : longint);

  begin
    variantmanager.varcast(dest,source,vartype);
  end;


{**********************************************************************
                      Variant manager functions
 **********************************************************************}

procedure GetVariantManager(var VarMgr: TVariantManager);
begin
  VarMgr:=VariantManager;
end;

procedure SetVariantManager(const VarMgr: TVariantManager);
begin
  VariantManager:=VarMgr;
end;

function IsVariantManagerSet: Boolean;
var
   i : longint;
begin
   I:=0;
   Result:=True;
   While Result and (I<(sizeof(tvariantmanager) div sizeof(pointer))-1) do
     begin
       Result:=Pointer(ppointer(@variantmanager+i*sizeof(pointer))^)<>Pointer(@invalidvariantop);
       Inc(I);
     end;
end;


procedure initvariantmanager;
  var
     i : longint;
  begin
     VarDispProc:=@vardisperror;
     DispCallByIDProc:=@vardisperror;
     tvardata(Unassigned).VType:=varEmpty;
     tvardata(Null).VType:=varNull;
     for i:=0 to (sizeof(tvariantmanager) div sizeof(pointer))-1 do
       ppointer(@variantmanager+i*sizeof(pointer))^:=@invalidvariantopnovariants;
     pointer(variantmanager.varclear):=@varclear
  end;


{
  $Log: variant.inc,v $
  Revision 1.30  2005/04/28 19:34:19  florian
    + variant<->currency/tdatetime operators

  Revision 1.29  2005/04/10 20:24:31  florian
    + basic operators (int, real and string) for variants implemented

  Revision 1.28  2005/04/10 09:22:38  florian
    + varrarrayredim added and implemented

  Revision 1.27  2005/03/28 13:38:05  florian
    + a lot of vararray stuff

  Revision 1.26  2005/03/25 19:02:59  florian
    + more vararray stuff

  Revision 1.25  2005/02/24 22:36:36  florian
    + some variant stuff fixed and added

  Revision 1.24  2005/02/14 17:13:29  peter
    * truncate log

  Revision 1.23  2005/02/01 20:22:24  florian
    + interface <-> variant conversion from Danny Milosavljevic

  Revision 1.22  2005/01/15 18:47:26  florian
    * several variant init./final. stuff fixed

  Revision 1.21  2005/01/08 20:43:44  florian
    + init/cleaning code for variants added

  Revision 1.20  2005/01/07 21:15:46  florian
    + basic rtl support for variant <-> interface implemented

}
