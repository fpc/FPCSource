{
    $Id$
    This file is part of the Free Pascal run time library.
    Copyright (c) 2001 by the Free Pascal development team

    This include file contains the declarations for variants
    support in FPC

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
const
   varempty = 0;
   varnull = 1;
   varsmallint = 2;
   varinteger = 3;
   varsingle = 4;
   vardouble = 5;
   varcurrency = 6;
   vardate = 7;
   varolestr = 8;
   vardispatch = 9;
   varerror = 10;
   varboolean = 11;
   varvariant = 12;
   varunknown = 13;
   vardecimal = 14;
   varshortint = 16;
   varbyte = 17;
   varword = 18;
   varlongword = 19;
   varint64 = 20;
   varqword = 21;

   varstrarg = $48;
   varstring = $100;
   varany = $101;
   vartypemask = $fff;
   vararray = $2000;
   varbyref = $4000;

   varword64 = varqword;

type
   tvartype = word;

   pvararrayboundarray = ^tvararrayboundarray;
   pvararraycoorarray = ^tvararraycoorarray;
   pvararraybound = ^tvararraybound;
   pvararray = ^tvararray;

   tvararraybound = packed record
      elementcount,lowbound  : longint;
   end;

   tvararray = packed record
      dimcount,flags : word;
      elementsize,lockcount : longint;
      data : pointer;
      bounds : array[0..255] of tvararraybound;
   end;

   tvararrayboundarray = array[0..0] of tvararraybound;
   tvararraycoorarray = array[0..0] of longint;

   tvarop = (opadd,opsubtract,opmultiply,opdivide,opintdivide,opmodulus,
             opshiftleft,opshiftright,opand,opor,opxor,opcompare,opnegate,
             opnot,opcmpeq,opcmpne,opcmplt,opcmple,opcmpgt,opcmpge);

   tvardata = packed record
      vtype : tvartype;
      case integer of
         0:(res1 : word;
            case integer of
               0:
                 (res2,res3 : word;
                  case word of
                     varsmallint : (vsmallint : smallint);
                     varinteger : (vinteger : longint);
                     varsingle : (vsingle : single);
                     vardouble : (vdouble : double);
                     varcurrency : (vcurrency : currency);
                     vardate : (vdate : tdatetime);
                     varolestr : (volestr : pwidechar);
                     vardispatch : (vdispatch : pointer);
                     varerror : (verror : dword);
                     varboolean : (vboolean : wordbool);
                     varunknown : (vunknown : pointer);
                     // vardecimal : ( : );
                     varshortint : (vshortint : shortint);
                     varbyte : (vbyte : byte);
                     varword : (vword : word);
                     varlongword : (vlongword : dword);
                     varint64 : (vint64 : int64);
                     varqword : (vqword : qword);
                     varword64 : (vword64 : qword);
                     varstring : (vstring : pointer);
                     varany :  (vany : pointer);
                     vararray : (varray : pvararray);
                     varbyref : (vpointer : pointer);
                 );
               1:
                 (vlongs : array[0..2] of longint);
           );
         1:(vwords : array[0..6] of word);
         2:(vbytes : array[0..13] of byte);
      end;
   pvardata = ^tvardata;

   pcalldesc = ^tcalldesc;
   tcalldesc = packed record
      calltype,argcount,namedargcount : byte;
      argtypes : array[0..255] of byte;
   end;

   pdispdesc = ^tdispdesc;
   tdispdesc = packed record
      dispid : longint;
      restype : byte;
      calldesc : tcalldesc;
   end;

   tvariantmanager = record
      vartoint : function(const v : variant) : longint;
      vartoint64 : function(const v : variant) : int64;
      vartoword64 : function(const v : variant) : qword;
      vartobool : function(const v : variant) : boolean;
      vartoreal : function(const v : variant) : extended;
      vartocurr : function(const v : variant) : currency;
      vartopstr : procedure(var s ;const v : variant);
      vartolstr : procedure(var s : ansistring;const v : variant);
      vartowstr : procedure(var s : widestring;const v : variant);
      vartointf : procedure(var intf : iinterface;const v : variant);
      vartodisp : procedure(var disp : idispatch;const v : variant);
      vartodynarray : procedure(var dynarr : pointer;const v : variant;
         typeinfo : pointer);

      varfrombool : procedure(var dest : variant;const source : Boolean);
      varfromint : procedure(var dest : variant;const source,Range : longint);
      varfromint64 : procedure(var dest : variant;const source : int64);
      varfromword64 : procedure(var dest : variant;const source : qword);
      varfromreal : procedure(var dest : variant;const source : extended);
      varfrompstr: procedure(var dest: variant; const source: ShortString);
      varfromlstr: procedure(var dest: variant; const source: ansistring);
      varfromwstr: procedure(var dest: variant; const source: WideString);

      {!!!!!!!}

      { operators }
      varop : procedure(var left : variant;const right : variant;opcdoe : tvarop);
      cmpop : function(const left,right : variant;const opcode : tvarop) : boolean;
      varneg : procedure(var v : variant);
      varnot : procedure(var v : variant);

      { misc }
      varinit : procedure(var v : variant);
      varclear : procedure(var v : variant);
      varaddref : procedure(var v : variant);
      varcopy : procedure(var dest : variant;const source : variant);
      varcast : procedure(var dest : variant;const source : variant;vartype : longint);
   end;
   pvariantmanager = ^tvariantmanager;

procedure GetVariantManager(var VarMgr: TVariantManager);
procedure SetVariantManager(const VarMgr: TVariantManager);
function IsVariantManagerSet: Boolean;

var
   VarDispProc : pointer;
   DispCallByIDProc : pointer;

{**********************************************************************
                       to Variant assignments
 **********************************************************************}

{ Integer }
operator :=(const source : byte) dest : variant;
operator :=(const source : shortint) dest : variant;
operator :=(const source : word) dest : variant;
operator :=(const source : smallint) dest : variant;
operator :=(const source : dword) dest : variant;
operator :=(const source : longint) dest : variant;
operator :=(const source : qword) dest : variant;
operator :=(const source : int64) dest : variant;

{ Boolean }
operator :=(const source : boolean) dest : variant;
operator :=(const source : wordbool) dest : variant;
operator :=(const source : longbool) dest : variant;

{ Chars }
operator :=(const source : char) dest : variant;
operator :=(const source : widechar) dest : variant;

{ Strings }
operator :=(const source : shortstring) dest : variant;
operator :=(const source : ansistring) dest : variant;
operator :=(const source : widestring) dest : variant;

{ Floats }
operator :=(const source : single) dest : variant;
operator :=(const source : double) dest : variant;
operator :=(const source : extended) dest : variant;
operator :=(const source : comp) dest : variant;

{ Misc. }
{ Fixme!!!!
operator :=(const source : currency) dest : variant;
operator :=(const source : tdatetime) dest : variant;
}
{**********************************************************************
                       from Variant assignments
 **********************************************************************}

{ Integer }
operator :=(const source : variant) dest : byte;
operator :=(const source : variant) dest : shortint;
operator :=(const source : variant) dest : word;
operator :=(const source : variant) dest : smallint;
operator :=(const source : variant) dest : dword;
operator :=(const source : variant) dest : longint;
operator :=(const source : variant) dest : qword;
operator :=(const source : variant) dest : int64;

{ Boolean }
operator :=(const source : variant) dest : boolean;
operator :=(const source : variant) dest : wordbool;
operator :=(const source : variant) dest : longbool;

{ Chars }
operator :=(const source : variant) dest : char;
operator :=(const source : variant) dest : widechar;

{ Strings }
operator :=(const source : variant) dest : shortstring;
operator :=(const source : variant) dest : ansistring;
operator :=(const source : variant) dest : widestring;

{ Floats }
operator :=(const source : variant) dest : single;
operator :=(const source : variant) dest : double;
operator :=(const source : variant) dest : extended;
operator :=(const source : variant) dest : comp;

{**********************************************************************
                              or operator
 **********************************************************************}

operator or(const op1,op2 : variant) dest : variant;

operator or(const op1 : byte;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : byte) dest : variant;
operator or(const op1 : shortint;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : shortint) dest : variant;
operator or(const op1 : word;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : word) dest : variant;
operator or(const op1 : integer;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : integer) dest : variant;
operator or(const op1 : longint;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : longint) dest : variant;
operator or(const op1 : cardinal;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : cardinal) dest : variant;
operator or(const op1 : int64;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : int64) dest : variant;
operator or(const op1 : qword;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : qword) dest : variant;

operator or(const op1 : boolean;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : boolean) dest : variant;
operator or(const op1 : wordbool;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : wordbool) dest : variant;
operator or(const op1 : longbool;const op2 : variant) dest : variant;
operator or(const op1 : variant;const op2 : longbool) dest : variant;

{**********************************************************************
                              and-operator
 **********************************************************************}

operator and(const op1,op2 : variant) dest : variant;

operator and(const op1 : byte;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : byte) dest : variant;
operator and(const op1 : shortint;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : shortint) dest : variant;
operator and(const op1 : word;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : word) dest : variant;
operator and(const op1 : integer;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : integer) dest : variant;
operator and(const op1 : longint;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : longint) dest : variant;
operator and(const op1 : cardinal;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : cardinal) dest : variant;
operator and(const op1 : int64;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : int64) dest : variant;
operator and(const op1 : qword;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : qword) dest : variant;

operator and(const op1 : boolean;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : boolean) dest : variant;
operator and(const op1 : wordbool;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : wordbool) dest : variant;
operator and(const op1 : longbool;const op2 : variant) dest : variant;
operator and(const op1 : variant;const op2 : longbool) dest : variant;

{**********************************************************************
                              xor operator
 **********************************************************************}

operator xor(const op1,op2 : variant) dest : variant;

operator xor(const op1 : byte;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : byte) dest : variant;
operator xor(const op1 : shortint;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : shortint) dest : variant;
operator xor(const op1 : word;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : word) dest : variant;
operator xor(const op1 : integer;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : integer) dest : variant;
operator xor(const op1 : longint;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : longint) dest : variant;
operator xor(const op1 : cardinal;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : cardinal) dest : variant;
operator xor(const op1 : int64;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : int64) dest : variant;
operator xor(const op1 : qword;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : qword) dest : variant;

operator xor(const op1 : boolean;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : boolean) dest : variant;
operator xor(const op1 : wordbool;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : wordbool) dest : variant;
operator xor(const op1 : longbool;const op2 : variant) dest : variant;
operator xor(const op1 : variant;const op2 : longbool) dest : variant;


{ Misc. }
{ Fixme!!!!
operator :=(const source : variant) dest : currency;
operator :=(const source : variant) dest : tdatetime;
}
{
  $Log$
  Revision 1.7  2002-10-06 22:13:55  florian
    * wrappers for xor, or and and operator with variants added

  Revision 1.6  2002/09/07 15:07:46  peter
    * old logs removed and tabs fixed

  Revision 1.5  2002/06/12 15:45:42  jonas
    * fixed bug in tvariantmanager declaration (string -> ansistring, this
      file is compiled in non-objpas mode!)
}
