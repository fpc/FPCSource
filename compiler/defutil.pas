{
    Copyright (c) 1998-2002 by Florian Klaempfl

    This unit provides some help routines for type handling

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
unit defutil;

{$i fpcdefs.inc}

interface

    uses
       cclasses,
       globtype,globals,
       symconst,symbase,symtype,symdef,
       cgbase,cpubase;

    type
       tmmxtype = (mmxno,mmxu8bit,mmxs8bit,mmxu16bit,mmxs16bit,
                   mmxu32bit,mmxs32bit,mmxfixed16,mmxsingle);


{*****************************************************************************
                          Basic type functions
 *****************************************************************************}

    {# Returns true, if definition defines an ordinal type }
    function is_ordinal(def : tdef) : boolean;

    {# Returns the minimal integer value of the type }
    function get_min_value(def : tdef) : TConstExprInt;

    {# Returns the maximal integer value of the type }
    function get_max_value(def : tdef) : TConstExprInt;

    {# Returns basetype of the specified integer range }
    function range_to_basetype(l,h:TConstExprInt):tbasetype;

    procedure range_to_type(l,h:TConstExprInt;var tt:ttype);

    procedure int_to_type(v:TConstExprInt;var tt:ttype);

    {# Returns true, if definition defines an integer type }
    function is_integer(def : tdef) : boolean;

    {# Returns true if definition is a boolean }
    function is_boolean(def : tdef) : boolean;

    {# Returns true if definition is a char

       This excludes the unicode char.
    }
    function is_char(def : tdef) : boolean;

    {# Returns true if definition is a widechar }
    function is_widechar(def : tdef) : boolean;

    {# Returns true if definition is a void}
    function is_void(def : tdef) : boolean;

    {# Returns true if definition is a smallset}
    function is_smallset(p : tdef) : boolean;

    {# Returns true, if def defines a signed data type
       (only for ordinal types)
    }
    function is_signed(def : tdef) : boolean;

    {# Returns true whether def_from's range is comprised in def_to's if both are
      orddefs, false otherwise                                              }
    function is_in_limit(def_from,def_to : tdef) : boolean;

    function is_in_limit_value(val_from:TConstExprInt;def_from,def_to : tdef) : boolean;

{*****************************************************************************
                              Array helper functions
 *****************************************************************************}

    {# Returns true, if p points to a zero based (non special like open or
      dynamic array def).

      This is mainly used to see if the array
      is convertable to a pointer
    }
    function is_zero_based_array(p : tdef) : boolean;

    {# Returns true if p points to an open array definition }
    function is_open_array(p : tdef) : boolean;

    {# Returns true if p points to a dynamic array definition }
    function is_dynamic_array(p : tdef) : boolean;

    {# Returns true, if p points to an array of const definition }
    function is_array_constructor(p : tdef) : boolean;

    {# Returns true, if p points to a variant array }
    function is_variant_array(p : tdef) : boolean;

    {# Returns true, if p points to an array of const }
    function is_array_of_const(p : tdef) : boolean;

    {# Returns true, if p points any kind of special array

       That is if the array is an open array, a variant
       array, an array constants constructor, or an
       array of const.
    }
    function is_special_array(p : tdef) : boolean;

    {# Returns true if p is a char array def }
    function is_chararray(p : tdef) : boolean;

    {# Returns true if p is a wide char array def }
    function is_widechararray(p : tdef) : boolean;

    {# Returns true if p is a open char array def }
    function is_open_chararray(p : tdef) : boolean;

    {# Returns true if p is a open wide char array def }
    function is_open_widechararray(p : tdef) : boolean;

{*****************************************************************************
                          String helper functions
 *****************************************************************************}

    {# Returns true if p points to an open string type }
    function is_open_string(p : tdef) : boolean;

    {# Returns true if p is an ansi string type }
    function is_ansistring(p : tdef) : boolean;

    {# Returns true if p is a long string type }
    function is_longstring(p : tdef) : boolean;

    {# returns true if p is a wide string type }
    function is_widestring(p : tdef) : boolean;

    {# Returns true if p is a short string type }
    function is_shortstring(p : tdef) : boolean;

    {# Returns true if p is a pchar def }
    function is_pchar(p : tdef) : boolean;

    {# Returns true if p is a pwidechar def }
    function is_pwidechar(p : tdef) : boolean;

    {# Returns true if p is a voidpointer def }
    function is_voidpointer(p : tdef) : boolean;

    {# Returns true, if definition is a float }
    function is_fpu(def : tdef) : boolean;

    {# Returns true, if def is a currency type }
    function is_currency(def : tdef) : boolean;

    {# Returns true, if def is a single type }
    function is_single(def : tdef) : boolean;

    {# Returns true, if def is a double type }
    function is_double(def : tdef) : boolean;

    {# Returns true, if def is an extended type }
    function is_extended(def : tdef) : boolean;

    {# Returns true, if definition is a "real" real (i.e. single/double/extended) }
    function is_real(def : tdef) : boolean;

    {# Returns true, if def is a 32 bit integer type }
    function is_32bitint(def : tdef) : boolean;

    {# Returns true, if def is a 64 bit integer type }
    function is_64bitint(def : tdef) : boolean;

    {# Returns true, if def is a 64 bit type }
    function is_64bit(def : tdef) : boolean;

    {# If @var(l) isn't in the range of def a range check error (if not explicit) is generated and
      the value is placed within the range
    }
    procedure testrange(def : tdef;var l : tconstexprint;explicit:boolean);

    {# Returns the range of def, where @var(l) is the low-range and @var(h) is
      the high-range.
    }
    procedure getrange(def : tdef;var l : TConstExprInt;var h : TConstExprInt);

    { some type helper routines for MMX support }
    function is_mmx_able_array(p : tdef) : boolean;

    {# returns the mmx type }
    function mmx_type(p : tdef) : tmmxtype;

    {# From a definition return the abstract code generator size enum. It is
       to note that the value returned can be @var(OS_NO) }
    function def_cgsize(def: tdef): tcgsize;


implementation

    uses
       systems,verbose;

    { returns true, if def uses FPU }
    function is_fpu(def : tdef) : boolean;
      begin
         is_fpu:=(def.deftype=floatdef);
      end;


    { returns true, if def is a currency type }
    function is_currency(def : tdef) : boolean;
      begin
         case s64currencytype.def.deftype of
           orddef :
             result:=(def.deftype=orddef) and
                     (torddef(s64currencytype.def).typ=torddef(def).typ);
           floatdef :
             result:=(def.deftype=floatdef) and
                     (tfloatdef(s64currencytype.def).typ=tfloatdef(def).typ);
           else
             internalerror(200304222);
         end;
      end;


    { returns true, if def is a single type }
    function is_single(def : tdef) : boolean;
      begin
        result:=(def.deftype=floatdef) and
          (tfloatdef(def).typ=s32real);
      end;


    { returns true, if def is a double type }
    function is_double(def : tdef) : boolean;
      begin
        result:=(def.deftype=floatdef) and
          (tfloatdef(def).typ=s64real);
      end;


    function is_extended(def : tdef) : boolean;
      begin
        result:=(def.deftype=floatdef) and
          (tfloatdef(def).typ=s80real);
      end;


    { returns true, if definition is a "real" real (i.e. single/double/extended) }
    function is_real(def : tdef) : boolean;
      begin
        result:=(def.deftype=floatdef) and
          (tfloatdef(def).typ in [s32real,s64real,s80real]);
      end;


    function range_to_basetype(l,h:TConstExprInt):tbasetype;
      begin
        { prefer signed over unsigned }
        if (l>=-128) and (h<=127) then
         range_to_basetype:=s8bit
        else if (l>=0) and (h<=255) then
         range_to_basetype:=u8bit
        else if (l>=-32768) and (h<=32767) then
         range_to_basetype:=s16bit
        else if (l>=0) and (h<=65535) then
         range_to_basetype:=u16bit
        else if (l>=low(longint)) and (h<=high(longint)) then
         range_to_basetype:=s32bit
        else if (l>=low(cardinal)) and (h<=high(cardinal)) then
         range_to_basetype:=u32bit
        else
         range_to_basetype:=s64bit;
      end;


    procedure range_to_type(l,h:TConstExprInt;var tt:ttype);
      begin
        { prefer signed over unsigned }
        if (l>=-128) and (h<=127) then
         tt:=s8inttype
        else if (l>=0) and (h<=255) then
         tt:=u8inttype
        else if (l>=-32768) and (h<=32767) then
         tt:=s16inttype
        else if (l>=0) and (h<=65535) then
         tt:=u16inttype
        else if (l>=low(longint)) and (h<=high(longint)) then
         tt:=s32inttype
        else if (l>=low(cardinal)) and (h<=high(cardinal)) then
         tt:=u32inttype
        else
         tt:=s64inttype;
      end;


    procedure int_to_type(v:TConstExprInt;var tt:ttype);
      begin
        range_to_type(v,v,tt);
      end;


    { true if p is an ordinal }
    function is_ordinal(def : tdef) : boolean;
      var
         dt : tbasetype;
      begin
         case def.deftype of
           orddef :
             begin
               dt:=torddef(def).typ;
               is_ordinal:=dt in [uchar,uwidechar,
                                  u8bit,u16bit,u32bit,u64bit,
                                  s8bit,s16bit,s32bit,s64bit,
                                  bool8bit,bool16bit,bool32bit];
             end;
           enumdef :
             is_ordinal:=true;
           else
             is_ordinal:=false;
         end;
      end;


    { returns the min. value of the type }
    function get_min_value(def : tdef) : TConstExprInt;
      begin
         case def.deftype of
           orddef:
             get_min_value:=torddef(def).low;
           enumdef:
             get_min_value:=tenumdef(def).min;
           else
             get_min_value:=0;
         end;
      end;


    { returns the max. value of the type }
    function get_max_value(def : tdef) : TConstExprInt;
      begin
         case def.deftype of
           orddef:
             get_max_value:=torddef(def).high;
           enumdef:
             get_max_value:=tenumdef(def).max;
           else
             get_max_value:=0;
         end;
      end;


    { true if p is an integer }
    function is_integer(def : tdef) : boolean;
      begin
        is_integer:=(def.deftype=orddef) and
                    (torddef(def).typ in [u8bit,u16bit,u32bit,u64bit,
                                          s8bit,s16bit,s32bit,s64bit]);
      end;


    { true if p is a boolean }
    function is_boolean(def : tdef) : boolean;
      begin
        is_boolean:=(def.deftype=orddef) and
                    (torddef(def).typ in [bool8bit,bool16bit,bool32bit]);
      end;


    { true if p is a void }
    function is_void(def : tdef) : boolean;
      begin
        is_void:=(def.deftype=orddef) and
                 (torddef(def).typ=uvoid);
      end;


    { true if p is a char }
    function is_char(def : tdef) : boolean;
      begin
        is_char:=(def.deftype=orddef) and
                 (torddef(def).typ=uchar);
      end;


    { true if p is a wchar }
    function is_widechar(def : tdef) : boolean;
      begin
        is_widechar:=(def.deftype=orddef) and
                 (torddef(def).typ=uwidechar);
      end;


    { true if p is signed (integer) }
    function is_signed(def : tdef) : boolean;
      var
         dt : tbasetype;
      begin
         case def.deftype of
           orddef :
             begin
               dt:=torddef(def).typ;
               is_signed:=(dt in [s8bit,s16bit,s32bit,s64bit,scurrency]);
             end;
           enumdef :
             is_signed:=tenumdef(def).min < 0;
           arraydef :
             is_signed:=is_signed(tarraydef(def).rangetype.def);
           else
             is_signed:=false;
         end;
      end;


    function is_in_limit(def_from,def_to : tdef) : boolean;

      var
        fromqword, toqword: boolean;

      begin
         if (def_from.deftype <> orddef) or
            (def_to.deftype <> orddef) then
           begin
             is_in_limit := false;
             exit;
           end;
         fromqword := torddef(def_from).typ = u64bit;
         toqword := torddef(def_to).typ = u64bit;
         is_in_limit:=(toqword and is_signed(def_from)) or
                      ((not fromqword) and
                       (torddef(def_from).low>=torddef(def_to).low) and
                       (torddef(def_from).high<=torddef(def_to).high));
      end;


    function is_in_limit_value(val_from:TConstExprInt;def_from,def_to : tdef) : boolean;

      begin
         if (def_from.deftype <> orddef) and
            (def_to.deftype <> orddef) then
           internalerror(200210062);
         if (torddef(def_to).typ = u64bit) then
          begin
            is_in_limit_value:=((TConstExprUInt(val_from)>=TConstExprUInt(torddef(def_to).low)) and
                                (TConstExprUInt(val_from)<=TConstExprUInt(torddef(def_to).high)));
          end
         else
          begin;
            is_in_limit_value:=((val_from>=torddef(def_to).low) and
                                (val_from<=torddef(def_to).high));
          end;
      end;


    { true, if p points to an open array def }
    function is_open_string(p : tdef) : boolean;
      begin
         is_open_string:=(p.deftype=stringdef) and
                         (tstringdef(p).string_typ=st_shortstring) and
                         (tstringdef(p).len=0);
      end;


    { true, if p points to a zero based array def }
    function is_zero_based_array(p : tdef) : boolean;
      begin
         result:=(p.deftype=arraydef) and
                 (tarraydef(p).lowrange=0) and
                 not(is_special_array(p));
      end;

    { true if p points to a dynamic array def }
    function is_dynamic_array(p : tdef) : boolean;
      begin
         result:=(p.deftype=arraydef) and
                 (ado_IsDynamicArray in tarraydef(p).arrayoptions);
      end;


    { true, if p points to an open array def }
    function is_open_array(p : tdef) : boolean;
      begin
         { check for s32inttype is needed, because for u32bit the high
           range is also -1 ! (PFV) }
         result:=(p.deftype=arraydef) and
                 (tarraydef(p).rangetype.def=s32inttype.def) and
                 (tarraydef(p).lowrange=0) and
                 (tarraydef(p).highrange=-1) and
                 ((tarraydef(p).arrayoptions * [ado_IsVariant,ado_IsArrayOfConst,ado_IsConstructor,ado_IsDynamicArray])=[]);
      end;

    { true, if p points to an array of const def }
    function is_array_constructor(p : tdef) : boolean;
      begin
         result:=(p.deftype=arraydef) and
                 (ado_IsConstructor in tarraydef(p).arrayoptions);
      end;

    { true, if p points to a variant array }
    function is_variant_array(p : tdef) : boolean;
      begin
         result:=(p.deftype=arraydef) and
                 (ado_IsVariant in tarraydef(p).arrayoptions);
      end;

    { true, if p points to an array of const }
    function is_array_of_const(p : tdef) : boolean;
      begin
         result:=(p.deftype=arraydef) and
                 (ado_IsArrayOfConst in tarraydef(p).arrayoptions);
      end;

    { true, if p points to a special array }
    function is_special_array(p : tdef) : boolean;
      begin
         result:=(p.deftype=arraydef) and
                 (
                  ((tarraydef(p).arrayoptions * [ado_IsVariant,ado_IsArrayOfConst,ado_IsConstructor,ado_IsDynamicArray])<>[]) or
                  is_open_array(p)
                 );
      end;

    { true if p is an ansi string def }
    function is_ansistring(p : tdef) : boolean;
      begin
         is_ansistring:=(p.deftype=stringdef) and
                        (tstringdef(p).string_typ=st_ansistring);
      end;

    { true if p is an long string def }
    function is_longstring(p : tdef) : boolean;
      begin
         is_longstring:=(p.deftype=stringdef) and
                        (tstringdef(p).string_typ=st_longstring);
      end;


    { true if p is an wide string def }
    function is_widestring(p : tdef) : boolean;
      begin
         is_widestring:=(p.deftype=stringdef) and
                        (tstringdef(p).string_typ=st_widestring);
      end;


    { true if p is an short string def }
    function is_shortstring(p : tdef) : boolean;
      begin
         is_shortstring:=(p.deftype=stringdef) and
                         (tstringdef(p).string_typ=st_shortstring);
      end;

    { true if p is a char array def }
    function is_chararray(p : tdef) : boolean;
      begin
        is_chararray:=(p.deftype=arraydef) and
                      is_char(tarraydef(p).elementtype.def) and
                      not(is_special_array(p));
      end;

    { true if p is a widechar array def }
    function is_widechararray(p : tdef) : boolean;
      begin
        is_widechararray:=(p.deftype=arraydef) and
                          is_widechar(tarraydef(p).elementtype.def) and
                          not(is_special_array(p));
      end;


    { true if p is a open char array def }
    function is_open_chararray(p : tdef) : boolean;
      begin
        is_open_chararray:= is_open_array(p) and
                            is_char(tarraydef(p).elementtype.def);
      end;

    { true if p is a open wide char array def }
    function is_open_widechararray(p : tdef) : boolean;
      begin
        is_open_widechararray:= is_open_array(p) and
                                is_widechar(tarraydef(p).elementtype.def);
      end;

    { true if p is a pchar def }
    function is_pchar(p : tdef) : boolean;
      begin
        is_pchar:=(p.deftype=pointerdef) and
                  (is_char(tpointerdef(p).pointertype.def) or
                   (is_zero_based_array(tpointerdef(p).pointertype.def) and
                    is_chararray(tpointerdef(p).pointertype.def)));
      end;

    { true if p is a pchar def }
    function is_pwidechar(p : tdef) : boolean;
      begin
        is_pwidechar:=(p.deftype=pointerdef) and
                      (is_widechar(tpointerdef(p).pointertype.def) or
                       (is_zero_based_array(tpointerdef(p).pointertype.def) and
                        is_widechararray(tpointerdef(p).pointertype.def)));
      end;


    { true if p is a voidpointer def }
    function is_voidpointer(p : tdef) : boolean;
      begin
        is_voidpointer:=(p.deftype=pointerdef) and
                        (tpointerdef(p).pointertype.def.deftype=orddef) and
                        (torddef(tpointerdef(p).pointertype.def).typ=uvoid);
      end;


    { true if p is a smallset def }
    function is_smallset(p : tdef) : boolean;
      begin
        is_smallset:=(p.deftype=setdef) and
                     (tsetdef(p).settype=smallset);
      end;


    { true, if def is a 32 bit int type }
    function is_32bitint(def : tdef) : boolean;
      begin
         result:=(def.deftype=orddef) and (torddef(def).typ in [u32bit,s32bit])
      end;


    { true, if def is a 64 bit int type }
    function is_64bitint(def : tdef) : boolean;
      begin
         is_64bitint:=(def.deftype=orddef) and (torddef(def).typ in [u64bit,s64bit])
      end;


    { true, if def is a 64 bit type }
    function is_64bit(def : tdef) : boolean;
      begin
         is_64bit:=(def.deftype=orddef) and (torddef(def).typ in [u64bit,s64bit,scurrency])
      end;


    { if l isn't in the range of def a range check error (if not explicit) is generated and
      the value is placed within the range }
    procedure testrange(def : tdef;var l : tconstexprint;explicit:boolean);
      var
         lv,hv: TConstExprInt;
         error: boolean;
      begin
         error := false;
         { for 64 bit types we need only to check if it is less than }
         { zero, if def is a qword node                              }
         if is_64bitint(def) then
           begin
              if (l<0) and (torddef(def).typ=u64bit) then
                begin
                   { don't zero the result, because it may come from hex notation
                     like $ffffffffffffffff! (JM)
                   l:=0; }
                   if not explicit then
                    begin
                      if (cs_check_range in aktlocalswitches) then
                        Message(parser_e_range_check_error)
                      else
                        Message(parser_w_range_check_error);
                    end;
                   error := true;
                end;
           end
         else
           begin
              getrange(def,lv,hv);
              if (l<lv) or (l>hv) then
                begin
                   if not explicit then
                    begin
                      if ((def.deftype=enumdef) and
                          { delphi allows range check errors in
                           enumeration type casts FK }
                          not(m_delphi in aktmodeswitches)) or
                         (cs_check_range in aktlocalswitches) then
                        Message(parser_e_range_check_error)
                      else
                        Message(parser_w_range_check_error);
                    end;
                   error := true;
                end;
           end;
         if error then
          begin
             { Fix the value to fit in the allocated space for this type of variable }
             case longint(def.size) of
               1: l := l and $ff;
               2: l := l and $ffff;
               { work around sign extension bug (to be fixed) (JM) }
               4: l := l and (int64($fffffff) shl 4 + $f);
             end;
             { do sign extension if necessary (JM) }
             if is_signed(def) then
              begin
                case longint(def.size) of
                  1: l := shortint(l);
                  2: l := smallint(l);
                  4: l := longint(l);
                end;
              end;
          end;
      end;


    { return the range from def in l and h }
    procedure getrange(def : tdef;var l : TConstExprInt;var h : TConstExprInt);
      begin
        case def.deftype of
          orddef :
            begin
              l:=torddef(def).low;
              h:=torddef(def).high;
            end;
          enumdef :
            begin
              l:=tenumdef(def).min;
              h:=tenumdef(def).max;
            end;
          arraydef :
            begin
              l:=tarraydef(def).lowrange;
              h:=tarraydef(def).highrange;
            end;
        else
          internalerror(987);
        end;
      end;


    function mmx_type(p : tdef) : tmmxtype;
      begin
         mmx_type:=mmxno;
         if is_mmx_able_array(p) then
           begin
              if tarraydef(p).elementtype.def.deftype=floatdef then
                case tfloatdef(tarraydef(p).elementtype.def).typ of
                  s32real:
                    mmx_type:=mmxsingle;
                end
              else
                case torddef(tarraydef(p).elementtype.def).typ of
                   u8bit:
                     mmx_type:=mmxu8bit;
                   s8bit:
                     mmx_type:=mmxs8bit;
                   u16bit:
                     mmx_type:=mmxu16bit;
                   s16bit:
                     mmx_type:=mmxs16bit;
                   u32bit:
                     mmx_type:=mmxu32bit;
                   s32bit:
                     mmx_type:=mmxs32bit;
                end;
           end;
      end;


    function is_mmx_able_array(p : tdef) : boolean;
      begin
{$ifdef SUPPORT_MMX}
         if (cs_mmx_saturation in aktlocalswitches) then
           begin
              is_mmx_able_array:=(p.deftype=arraydef) and
                not(is_special_array(p)) and
                (
                 (
                  (tarraydef(p).elementtype.def.deftype=orddef) and
                  (
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=1) and
                    (torddef(tarraydef(p).elementtype.def).typ in [u32bit,s32bit])
                   )
                   or
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=3) and
                    (torddef(tarraydef(p).elementtype.def).typ in [u16bit,s16bit])
                   )
                  )
                 )
                 or
                (
                 (
                  (tarraydef(p).elementtype.def.deftype=floatdef) and
                  (
                   (tarraydef(p).lowrange=0) and
                   (tarraydef(p).highrange=1) and
                   (tfloatdef(tarraydef(p).elementtype.def).typ=s32real)
                  )
                 )
                )
              );
           end
         else
           begin
              is_mmx_able_array:=(p.deftype=arraydef) and
                (
                 (
                  (tarraydef(p).elementtype.def.deftype=orddef) and
                  (
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=1) and
                    (torddef(tarraydef(p).elementtype.def).typ in [u32bit,s32bit])
                   )
                   or
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=3) and
                    (torddef(tarraydef(p).elementtype.def).typ in [u16bit,s16bit])
                   )
                   or
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=7) and
                    (torddef(tarraydef(p).elementtype.def).typ in [u8bit,s8bit])
                   )
                  )
                 )
                 or
                 (
                  (tarraydef(p).elementtype.def.deftype=floatdef) and
                  (
                   (tarraydef(p).lowrange=0) and
                   (tarraydef(p).highrange=1) and
                   (tfloatdef(tarraydef(p).elementtype.def).typ=s32real)
                  )
                 )
                );
           end;
{$else SUPPORT_MMX}
         is_mmx_able_array:=false;
{$endif SUPPORT_MMX}
      end;


    function def_cgsize(def: tdef): tcgsize;
      begin
        case def.deftype of
          orddef,
          enumdef,
          setdef:
            begin
              result:=int_cgsize(def.size);
              if is_signed(def) then
                result:=tcgsize(ord(result)+(ord(OS_S8)-ord(OS_8)));
            end;
          classrefdef,
          pointerdef:
            result := OS_ADDR;
          procvardef:
            begin
              if tprocvardef(def).is_methodpointer and
                 (not tprocvardef(def).is_addressonly) then
                result := OS_64
              else
                result := OS_ADDR;
            end;
          stringdef :
            begin
              if is_ansistring(def) or is_widestring(def) then
                result := OS_ADDR
              else
                result:=int_cgsize(def.size);
            end;
          objectdef :
            begin
              if is_class_or_interface(def) then
                result := OS_ADDR
              else
                result:=int_cgsize(def.size);
            end;
          floatdef:
            if cs_fp_emulation in aktmoduleswitches then
              result:=int_cgsize(def.size)
            else
              result:=tfloat2tcgsize[tfloatdef(def).typ];
          recorddef :
            result:=int_cgsize(def.size);
          arraydef :
            begin
              if not is_special_array(def) then
                result := int_cgsize(def.size)
              else
                begin
                  if is_dynamic_array(def) then
                    result := OS_ADDR
                  else
                    result := OS_NO;
                end;
            end;
          else
            begin
              { undefined size }
              result:=OS_NO;
            end;
        end;
      end;


end.
