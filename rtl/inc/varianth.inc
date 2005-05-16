{
    $Id: varianth.inc,v 1.24 2005/04/28 19:34:19 florian Exp $
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
     elementcount,lowbound  : SizeInt;
   end;

   tvararray = packed record
      dimcount,flags : word;
      elementsize,lockcount : longint;
      data : pointer;
      bounds : array[0..255] of tvararraybound;
   end;

   tvararrayboundarray = array[0..0] of tvararraybound;
   tvararraycoorarray = array[0..0] of SizeInt;

   tvarop = (opadd,opsubtract,opmultiply,opdivide,opintdivide,opmodulus,
             opshiftleft,opshiftright,opand,opor,opxor,opcompare,opnegate,
             opnot,opcmpeq,opcmpne,opcmplt,opcmple,opcmpgt,opcmpge,oppower);

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
                     varerror : (verror : hresult);
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
      vartotdatetime : function(const v : variant) : tdatetime;
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
      varfromtdatetime : procedure(var dest : Variant;const source : TDateTime);
      varfromcurr : procedure(var dest : Variant;const source : Currency);
      varfrompstr: procedure(var dest : variant; const source : ShortString);
      varfromlstr: procedure(var dest : variant; const source : ansistring);
      varfromwstr: procedure(var dest : variant; const source : WideString);
      varfromintf: procedure(var dest : variant;const source : iinterface);
      varfromdisp: procedure(var dest : variant;const source : idispatch);
      varfromdynarray: procedure(var dest : variant;const source : pointer; typeinfo: pointer);
      olevarfrompstr: procedure(var dest : olevariant; const source : shortstring);
      olevarfromlstr: procedure(var dest : olevariant; const source : ansistring);
      olevarfromvar: procedure(var dest : olevariant; const source : variant);
      olevarfromint: procedure(var dest : olevariant; const source : longint;const range : shortint);

      { operators }
      varop : procedure(var left : variant;const right : variant;opcode : tvarop);
      cmpop : function(const left,right : variant;const opcode : tvarop) : boolean;
      varneg : procedure(var v : variant);
      varnot : procedure(var v : variant);

      { misc }
      varinit : procedure(var v : variant);
      varclear : procedure(var v : variant);
      varaddref : procedure(var v : variant);
      varcopy : procedure(var dest : variant;const source : variant);
      varcast : procedure(var dest : variant;const source : variant;vartype : longint);
      varcastole : procedure(var dest : variant; const source : variant;vartype : longint);

      dispinvoke: procedure(dest : pvardata;const source : tvardata;
        calldesc : pcalldesc;params : pointer);cdecl;

      vararrayredim : procedure(var a : variant;highbound : SizeInt);
      vararrayget : function(const a : variant;indexcount : SizeInt;indices : PSizeInt) : variant;cdecl;
      vararrayput: procedure(var a : variant; const value : variant;
        indexcount : SizeInt;indices : PSizeInt);cdecl;
      writevariant : function(var t : text;const v : variant;width : longint) : Pointer;
      write0Variant : function(var t : text;const v : Variant) : Pointer;
   end;
   pvariantmanager = ^tvariantmanager;

procedure GetVariantManager(var VarMgr: TVariantManager);
procedure SetVariantManager(const VarMgr: TVariantManager);
function IsVariantManagerSet: Boolean;

const
  VarClearProc :  procedure(var v : TVarData) = nil;
  VarAddRefProc : procedure(var v : TVarData) = nil;
  VarCopyProc :   procedure(var d : TVarData;const s : TVarData) = nil;
  VarToLStrProc : procedure(var d : AnsiString;const s : TVarData) = nil;
  VarToWStrProc : procedure(var d : WideString;const s : TVarData) = nil;

var
   VarDispProc : pointer;
   DispCallByIDProc : pointer;
   Null,Unassigned : Variant;

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
{$ifdef SUPPORT_SINGLE}
operator :=(const source : single) dest : variant;
{$endif SUPPORT_SINGLE}
{$ifdef SUPPORT_DOUBLE}
operator :=(const source : double) dest : variant;
{$endif SUPPORT_DOUBLE}
{$ifdef SUPPORT_EXTENDED}
operator :=(const source : extended) dest : variant;
{$endif SUPPORT_EXTENDED}
{$ifdef SUPPORT_COMP}
operator :=(const source : comp) dest : variant;
{$endif SUPPORT_COMP}

{ Misc. }
operator :=(const source : currency) dest : variant;
operator :=(const source : tdatetime) dest : variant;

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
{$ifdef SUPPORT_SINGLE}
operator :=(const source : variant) dest : single;
{$endif SUPPORT_SINGLE}
{$ifdef SUPPORT_DOUBLE}
operator :=(const source : variant) dest : double;
{$endif SUPPORT_DOUBLE}
{$ifdef SUPPORT_EXTENDED}
operator :=(const source : variant) dest : extended;
{$endif SUPPORT_EXTENDED}
{$ifdef SUPPORT_EXTENDED}
operator :=(const source : variant) dest : comp;
{$endif SUPPORT_COMP}

{ Misc. }
operator :=(const source : variant) dest : currency;
{$ifdef HASOVERLOADASSIGNBYUNIQUERESULT}
operator :=(const source : variant) dest : tdatetime;
{$endif HASOVERLOADASSIGNBYUNIQUERESULT}

{**********************************************************************
                         Operators
 **********************************************************************}

operator or(const op1,op2 : variant) dest : variant;
operator and(const op1,op2 : variant) dest : variant;
operator xor(const op1,op2 : variant) dest : variant;
operator not(const op : variant) dest : variant;
operator shl(const op1,op2 : variant) dest : variant;
operator shr(const op1,op2 : variant) dest : variant;
operator +(const op1,op2 : variant) dest : variant;
operator -(const op1,op2 : variant) dest : variant;
operator *(const op1,op2 : variant) dest : variant;
operator /(const op1,op2 : variant) dest : variant;
operator **(const op1,op2 : variant) dest : variant;
operator div(const op1,op2 : variant) dest : variant;
operator mod(const op1,op2 : variant) dest : variant;
operator -(const op : variant) dest : variant;
operator =(const op1,op2 : variant) dest : boolean;
operator <(const op1,op2 : variant) dest : boolean;
operator >(const op1,op2 : variant) dest : boolean;
operator >=(const op1,op2 : variant) dest : boolean;
operator <=(const op1,op2 : variant) dest : boolean;

{ variant helpers }
procedure VarArrayRedim(var A: Variant; HighBound: SizeInt);

{
  $Log: varianth.inc,v $
  Revision 1.24  2005/04/28 19:34:19  florian
    + variant<->currency/tdatetime operators

  Revision 1.23  2005/04/10 20:24:31  florian
    + basic operators (int, real and string) for variants implemented

  Revision 1.22  2005/04/10 09:22:38  florian
    + varrarrayredim added and implemented

  Revision 1.21  2005/03/28 13:38:05  florian
    + a lot of vararray stuff

  Revision 1.20  2005/03/25 19:02:59  florian
    + more vararray stuff

  Revision 1.19  2005/03/25 18:03:50  florian
    + some vararray stuff added

  Revision 1.18  2005/02/14 17:13:29  peter
    * truncate log

  Revision 1.17  2005/01/15 18:47:26  florian
    * several variant init./final. stuff fixed
}
