{
    Copyright (c) 1998-2006 by Florian Klaempfl

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
       globtype,globals,constexp,
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
    function range_to_basetype(l,h:TConstExprInt):tordtype;

    procedure range_to_type(l,h:TConstExprInt;var def:tdef);

    procedure int_to_type(v:TConstExprInt;var def:tdef);

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

{    function is_in_limit_value(val_from:TConstExprInt;def_from,def_to : tdef) : boolean;}

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

       Bitpacked arrays aren't special in this regard though.
    }
    function is_special_array(p : tdef) : boolean;

    {# Returns true if p is a bitpacked array }
    function is_packed_array(p: tdef) : boolean;

    {# Returns true if p is a bitpacked record }
    function is_packed_record_or_object(p: tdef) : boolean;

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

    {# If @var(l) isn't in the range of todef a range check error (if not explicit) is generated and
      the value is placed within the range
    }
    procedure testrange(todef : tdef;var l : tconstexprint;explicit:boolean);

    {# Returns the range of def, where @var(l) is the low-range and @var(h) is
      the high-range.
    }
    procedure getrange(def : tdef;out l, h : TConstExprInt);

    { type being a vector? }
    function is_vector(p : tdef) : boolean;

    { some type helper routines for MMX support }
    function is_mmx_able_array(p : tdef) : boolean;

    {# returns the mmx type }
    function mmx_type(p : tdef) : tmmxtype;

    { returns if the passed type (array) fits into an mm register }
    function fits_in_mm_register(p : tdef) : boolean;

    {# From a definition return the abstract code generator size enum. It is
       to note that the value returned can be @var(OS_NO) }
    function def_cgsize(def: tdef): tcgsize;

    {# returns true, if the type passed is can be used with windows automation }
    function is_automatable(p : tdef) : boolean;

    {# returns true, if the type passed is a varset }
    function is_varset(p : tdef) : boolean;

    { # returns true if the procdef has no parameters and no specified return type }
    function is_bareprocdef(pd : tprocdef): boolean;

implementation

    uses
       systems,verbose;

    { returns true, if def uses FPU }
    function is_fpu(def : tdef) : boolean;
      begin
         is_fpu:=(def.typ=floatdef);
      end;


    { returns true, if def is a currency type }
    function is_currency(def : tdef) : boolean;
      begin
         case s64currencytype.typ of
           orddef :
             result:=(def.typ=orddef) and
                     (torddef(s64currencytype).ordtype=torddef(def).ordtype);
           floatdef :
             result:=(def.typ=floatdef) and
                     (tfloatdef(s64currencytype).floattype=tfloatdef(def).floattype);
           else
             internalerror(200304222);
         end;
      end;


    { returns true, if def is a single type }
    function is_single(def : tdef) : boolean;
      begin
        result:=(def.typ=floatdef) and
          (tfloatdef(def).floattype=s32real);
      end;


    { returns true, if def is a double type }
    function is_double(def : tdef) : boolean;
      begin
        result:=(def.typ=floatdef) and
          (tfloatdef(def).floattype=s64real);
      end;


    function is_extended(def : tdef) : boolean;
      begin
        result:=(def.typ=floatdef) and
          (tfloatdef(def).floattype=s80real);
      end;


    { returns true, if definition is a "real" real (i.e. single/double/extended) }
    function is_real(def : tdef) : boolean;
      begin
        result:=(def.typ=floatdef) and
          (tfloatdef(def).floattype in [s32real,s64real,s80real]);
      end;


    function range_to_basetype(l,h:TConstExprInt):tordtype;
      begin
        { prefer signed over unsigned }
        if (l>=int64(-128)) and (h<=127) then
         range_to_basetype:=s8bit
        else if (l>=0) and (h<=255) then
         range_to_basetype:=u8bit
        else if (l>=int64(-32768)) and (h<=32767) then
         range_to_basetype:=s16bit
        else if (l>=0) and (h<=65535) then
         range_to_basetype:=u16bit
        else if (l>=int64(low(longint))) and (h<=high(longint)) then
         range_to_basetype:=s32bit
        else if (l>=low(cardinal)) and (h<=high(cardinal)) then
         range_to_basetype:=u32bit
        else
         range_to_basetype:=s64bit;
      end;


    procedure range_to_type(l,h:TConstExprInt;var def:tdef);
      begin
        { prefer signed over unsigned }
        if (l>=int64(-128)) and (h<=127) then
         def:=s8inttype
        else if (l>=0) and (h<=255) then
         def:=u8inttype
        else if (l>=int64(-32768)) and (h<=32767) then
         def:=s16inttype
        else if (l>=0) and (h<=65535) then
         def:=u16inttype
        else if (l>=int64(low(longint))) and (h<=high(longint)) then
         def:=s32inttype
        else if (l>=low(cardinal)) and (h<=high(cardinal)) then
         def:=u32inttype
        else if (l>=low(int64)) and (h<=high(int64)) then
         def:=s64inttype
        else
         def:=u64inttype;
      end;


    procedure int_to_type(v:TConstExprInt;var def:tdef);
      begin
        range_to_type(v,v,def);
      end;


    { true if p is an ordinal }
    function is_ordinal(def : tdef) : boolean;
      var
         dt : tordtype;
      begin
         case def.typ of
           orddef :
             begin
               dt:=torddef(def).ordtype;
               is_ordinal:=dt in [uchar,uwidechar,
                                  u8bit,u16bit,u32bit,u64bit,
                                  s8bit,s16bit,s32bit,s64bit,
                                  bool8bit,bool16bit,bool32bit,bool64bit];
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
         case def.typ of
           orddef:
             result:=torddef(def).low;
           enumdef:
             result:=int64(tenumdef(def).min);
           else
             result:=0;
         end;
      end;


    { returns the max. value of the type }
    function get_max_value(def : tdef) : TConstExprInt;
      begin
         case def.typ of
           orddef:
             result:=torddef(def).high;
           enumdef:
             result:=tenumdef(def).max;
           else
             result:=0;
         end;
      end;


    { true if p is an integer }
    function is_integer(def : tdef) : boolean;
      begin
        result:=(def.typ=orddef) and
                    (torddef(def).ordtype in [u8bit,u16bit,u32bit,u64bit,
                                          s8bit,s16bit,s32bit,s64bit]);
      end;


    { true if p is a boolean }
    function is_boolean(def : tdef) : boolean;
      begin
        result:=(def.typ=orddef) and
                    (torddef(def).ordtype in [bool8bit,bool16bit,bool32bit,bool64bit]);
      end;


    { true if p is a void }
    function is_void(def : tdef) : boolean;
      begin
        result:=(def.typ=orddef) and
                 (torddef(def).ordtype=uvoid);
      end;


    { true if p is a char }
    function is_char(def : tdef) : boolean;
      begin
        result:=(def.typ=orddef) and
                 (torddef(def).ordtype=uchar);
      end;


    { true if p is a wchar }
    function is_widechar(def : tdef) : boolean;
      begin
        result:=(def.typ=orddef) and
                 (torddef(def).ordtype=uwidechar);
      end;


    { true if p is signed (integer) }
    function is_signed(def : tdef) : boolean;
      begin
         case def.typ of
           orddef :
             result:=torddef(def).low < 0;
           enumdef :
             result:=tenumdef(def).min < 0;
           arraydef :
             result:=is_signed(tarraydef(def).rangedef);
           else
             result:=false;
         end;
      end;


    function is_in_limit(def_from,def_to : tdef) : boolean;

      begin
         if (def_from.typ<>def_to.typ) or
            not(def_from.typ in [orddef,enumdef,setdef]) then
           begin
             is_in_limit := false;
             exit;
           end;
         case def_from.typ of
           orddef:
             is_in_limit:=(torddef(def_from).low>=torddef(def_to).low) and
                          (torddef(def_from).high<=torddef(def_to).high);
           enumdef:
             is_in_limit:=(tenumdef(def_from).min>=tenumdef(def_to).min) and
                          (tenumdef(def_from).max<=tenumdef(def_to).max);
           setdef:
             is_in_limit:=(tsetdef(def_from).setbase>=tsetdef(def_to).setbase) and
                          (tsetdef(def_from).setmax<=tsetdef(def_to).setmax);
         end;
      end;

    { true, if p points to an open array def }
    function is_open_string(p : tdef) : boolean;
      begin
         is_open_string:=(p.typ=stringdef) and
                         (tstringdef(p).stringtype=st_shortstring) and
                         (tstringdef(p).len=0);
      end;


    { true, if p points to a zero based array def }
    function is_zero_based_array(p : tdef) : boolean;
      begin
         result:=(p.typ=arraydef) and
                 (tarraydef(p).lowrange=0) and
                 not(is_special_array(p));
      end;

    { true if p points to a dynamic array def }
    function is_dynamic_array(p : tdef) : boolean;
      begin
         result:=(p.typ=arraydef) and
                 (ado_IsDynamicArray in tarraydef(p).arrayoptions);
      end;


    { true, if p points to an open array def }
    function is_open_array(p : tdef) : boolean;
      begin
         { check for s32inttype is needed, because for u32bit the high
           range is also -1 ! (PFV) }
         result:=(p.typ=arraydef) and
                 (tarraydef(p).rangedef=s32inttype) and
                 (tarraydef(p).lowrange=0) and
                 (tarraydef(p).highrange=-1) and
                 ((tarraydef(p).arrayoptions * [ado_IsVariant,ado_IsArrayOfConst,ado_IsConstructor,ado_IsDynamicArray])=[]);
      end;

    { true, if p points to an array of const def }
    function is_array_constructor(p : tdef) : boolean;
      begin
         result:=(p.typ=arraydef) and
                 (ado_IsConstructor in tarraydef(p).arrayoptions);
      end;

    { true, if p points to a variant array }
    function is_variant_array(p : tdef) : boolean;
      begin
         result:=(p.typ=arraydef) and
                 (ado_IsVariant in tarraydef(p).arrayoptions);
      end;

    { true, if p points to an array of const }
    function is_array_of_const(p : tdef) : boolean;
      begin
         result:=(p.typ=arraydef) and
                 (ado_IsArrayOfConst in tarraydef(p).arrayoptions);
      end;

    { true, if p points to a special array, bitpacked arrays aren't special in this regard though }
    function is_special_array(p : tdef) : boolean;
      begin
         result:=(p.typ=arraydef) and
                 (
                  ((tarraydef(p).arrayoptions * [ado_IsVariant,ado_IsArrayOfConst,ado_IsConstructor,ado_IsDynamicArray])<>[]) or
                  is_open_array(p)
                 );
      end;

    { true if p is an ansi string def }
    function is_ansistring(p : tdef) : boolean;
      begin
         is_ansistring:=(p.typ=stringdef) and
                        (tstringdef(p).stringtype=st_ansistring);
      end;

    { true if p is an long string def }
    function is_longstring(p : tdef) : boolean;
      begin
         is_longstring:=(p.typ=stringdef) and
                        (tstringdef(p).stringtype=st_longstring);
      end;


    { true if p is an wide string def }
    function is_widestring(p : tdef) : boolean;
      begin
         is_widestring:=(p.typ=stringdef) and
                        (tstringdef(p).stringtype=st_widestring);
      end;


    { true if p is an short string def }
    function is_shortstring(p : tdef) : boolean;
      begin
         is_shortstring:=(p.typ=stringdef) and
                         (tstringdef(p).stringtype=st_shortstring);
      end;


    { true if p is bit packed array def }
    function is_packed_array(p: tdef) : boolean;
      begin
        is_packed_array :=
           (p.typ = arraydef) and
           (ado_IsBitPacked in tarraydef(p).arrayoptions);
      end;


    { true if p is bit packed record def }
    function is_packed_record_or_object(p: tdef) : boolean;
      begin
        is_packed_record_or_object :=
           (p.typ in [recorddef,objectdef]) and
           (tabstractrecorddef(p).is_packed);
      end;


    { true if p is a char array def }
    function is_chararray(p : tdef) : boolean;
      begin
        is_chararray:=(p.typ=arraydef) and
                      is_char(tarraydef(p).elementdef) and
                      not(is_special_array(p));
      end;

    { true if p is a widechar array def }
    function is_widechararray(p : tdef) : boolean;
      begin
        is_widechararray:=(p.typ=arraydef) and
                          is_widechar(tarraydef(p).elementdef) and
                          not(is_special_array(p));
      end;


    { true if p is a open char array def }
    function is_open_chararray(p : tdef) : boolean;
      begin
        is_open_chararray:= is_open_array(p) and
                            is_char(tarraydef(p).elementdef);
      end;

    { true if p is a open wide char array def }
    function is_open_widechararray(p : tdef) : boolean;
      begin
        is_open_widechararray:= is_open_array(p) and
                                is_widechar(tarraydef(p).elementdef);
      end;

    { true if p is a pchar def }
    function is_pchar(p : tdef) : boolean;
      begin
        is_pchar:=(p.typ=pointerdef) and
                  (is_char(tpointerdef(p).pointeddef) or
                   (is_zero_based_array(tpointerdef(p).pointeddef) and
                    is_chararray(tpointerdef(p).pointeddef)));
      end;

    { true if p is a pchar def }
    function is_pwidechar(p : tdef) : boolean;
      begin
        is_pwidechar:=(p.typ=pointerdef) and
                      (is_widechar(tpointerdef(p).pointeddef) or
                       (is_zero_based_array(tpointerdef(p).pointeddef) and
                        is_widechararray(tpointerdef(p).pointeddef)));
      end;


    { true if p is a voidpointer def }
    function is_voidpointer(p : tdef) : boolean;
      begin
        is_voidpointer:=(p.typ=pointerdef) and
                        (tpointerdef(p).pointeddef.typ=orddef) and
                        (torddef(tpointerdef(p).pointeddef).ordtype=uvoid);
      end;


    { true if p is a smallset def }
    function is_smallset(p : tdef) : boolean;
      begin
        is_smallset:=(p.typ=setdef) and
                     (tsetdef(p).settype=smallset);
      end;


    { true, if def is a 32 bit int type }
    function is_32bitint(def : tdef) : boolean;
      begin
         result:=(def.typ=orddef) and (torddef(def).ordtype in [u32bit,s32bit])
      end;


    { true, if def is a 64 bit int type }
    function is_64bitint(def : tdef) : boolean;
      begin
         is_64bitint:=(def.typ=orddef) and (torddef(def).ordtype in [u64bit,s64bit])
      end;


    { true, if def is a 64 bit type }
    function is_64bit(def : tdef) : boolean;
      begin
         is_64bit:=(def.typ=orddef) and (torddef(def).ordtype in [u64bit,s64bit,scurrency])
      end;


    { if l isn't in the range of todef a range check error (if not explicit) is generated and
      the value is placed within the range }
    procedure testrange(todef : tdef;var l : tconstexprint;explicit:boolean);
      var
         lv,hv: TConstExprInt;
      begin
         { for 64 bit types we need only to check if it is less than }
         { zero, if def is a qword node                              }
         getrange(todef,lv,hv);
         if (l<lv) or (l>hv) then
           begin
             if not explicit then
               begin
                 if ((todef.typ=enumdef) and
                     { delphi allows range check errors in
                      enumeration type casts FK }
                     not(m_delphi in current_settings.modeswitches)) or
                    (cs_check_range in current_settings.localswitches) then
                   Message(parser_e_range_check_error)
                 else
                   Message(parser_w_range_check_error);
               end;
             { Fix the value to fit in the allocated space for this type of variable }
             case longint(todef.size) of
               1: l := l and $ff;
               2: l := l and $ffff;
               4: l := l and $ffffffff;
             end;
             {reset sign, i.e. converting -1 to qword changes the value to high(qword)}
             l.signed:=false;
             { do sign extension if necessary (JM) }
             if is_signed(todef) then
              begin
                case longint(todef.size) of
                  1: l.svalue := shortint(l.svalue);
                  2: l.svalue := smallint(l.svalue);
                  4: l.svalue := longint(l.svalue);
                end;
                l.signed:=true;
              end;
           end;
      end;


    { return the range from def in l and h }
    procedure getrange(def : tdef;out l, h : TConstExprInt);
      begin
        case def.typ of
          orddef :
            begin
              l:=torddef(def).low;
              h:=torddef(def).high;
            end;
          enumdef :
            begin
              l:=int64(tenumdef(def).min);
              h:=int64(tenumdef(def).max);
            end;
          arraydef :
            begin
              l:=int64(tarraydef(def).lowrange);
              h:=int64(tarraydef(def).highrange);
            end;
          else
            internalerror(200611054);
        end;
      end;


    function mmx_type(p : tdef) : tmmxtype;
      begin
         mmx_type:=mmxno;
         if is_mmx_able_array(p) then
           begin
              if tarraydef(p).elementdef.typ=floatdef then
                case tfloatdef(tarraydef(p).elementdef).floattype of
                  s32real:
                    mmx_type:=mmxsingle;
                end
              else
                case torddef(tarraydef(p).elementdef).ordtype of
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


    function is_vector(p : tdef) : boolean;
      begin
        result:=(p.typ=arraydef) and
                not(is_special_array(p)) and
                (tarraydef(p).elementdef.typ=floatdef) and
                (tfloatdef(tarraydef(p).elementdef).floattype in [s32real,s64real]);
      end;


    { returns if the passed type (array) fits into an mm register }
    function fits_in_mm_register(p : tdef) : boolean;
      begin
{$ifdef x86}
        result:= is_vector(p) and
                 (
                  (tarraydef(p).elementdef.typ=floatdef) and
                  (
                   (tarraydef(p).lowrange=0) and
                   (tarraydef(p).highrange=3) and
                   (tfloatdef(tarraydef(p).elementdef).floattype=s32real)
                  )
                 ) or

                 (
                  (tarraydef(p).elementdef.typ=floatdef) and
                  (
                   (tarraydef(p).lowrange=0) and
                   (tarraydef(p).highrange=1) and
                   (tfloatdef(tarraydef(p).elementdef).floattype=s64real)
                  )
                 );
{$else x86}
        result:=false;
{$endif x86}
      end;


    function is_mmx_able_array(p : tdef) : boolean;
      begin
{$ifdef SUPPORT_MMX}
         if (cs_mmx_saturation in current_settings.localswitches) then
           begin
              is_mmx_able_array:=(p.typ=arraydef) and
                not(is_special_array(p)) and
                (
                 (
                  (tarraydef(p).elementdef.typ=orddef) and
                  (
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=1) and
                    (torddef(tarraydef(p).elementdef).ordtype in [u32bit,s32bit])
                   )
                   or
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=3) and
                    (torddef(tarraydef(p).elementdef).ordtype in [u16bit,s16bit])
                   )
                  )
                 )
                 or
                (
                 (
                  (tarraydef(p).elementdef.typ=floatdef) and
                  (
                   (tarraydef(p).lowrange=0) and
                   (tarraydef(p).highrange=1) and
                   (tfloatdef(tarraydef(p).elementdef).floattype=s32real)
                  )
                 )
                )
              );
           end
         else
           begin
              is_mmx_able_array:=(p.typ=arraydef) and
                (
                 (
                  (tarraydef(p).elementdef.typ=orddef) and
                  (
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=1) and
                    (torddef(tarraydef(p).elementdef).ordtype in [u32bit,s32bit])
                   )
                   or
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=3) and
                    (torddef(tarraydef(p).elementdef).ordtype in [u16bit,s16bit])
                   )
                   or
                   (
                    (tarraydef(p).lowrange=0) and
                    (tarraydef(p).highrange=7) and
                    (torddef(tarraydef(p).elementdef).ordtype in [u8bit,s8bit])
                   )
                  )
                 )
                 or
                 (
                  (tarraydef(p).elementdef.typ=floatdef) and
                  (
                   (tarraydef(p).lowrange=0) and
                   (tarraydef(p).highrange=1) and
                   (tfloatdef(tarraydef(p).elementdef).floattype=s32real)
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
        case def.typ of
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
                if (sizeof(aint) = 4) then
                  result:=OS_64
                else if (sizeof(aint) = 8) then
                  result:=OS_128
                else
                  internalerror(200707141)
              else
                result:=OS_ADDR;
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
            if cs_fp_emulation in current_settings.moduleswitches then
              result:=int_cgsize(def.size)
            else
              result:=tfloat2tcgsize[tfloatdef(def).floattype];
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


    function is_automatable(p : tdef) : boolean;
      begin
        result:=false;
        case p.typ of
          orddef:
            result:=torddef(p).ordtype in [u8bit,s32bit,s16bit,bool16bit];
          floatdef:
            result:=tfloatdef(p).floattype in [s64currency,s64real,s32real];
          stringdef:
            result:=tstringdef(p).stringtype in [st_ansistring,st_widestring];
          variantdef:
            result:=true;
          arraydef:
            result:=(ado_IsConstString in tarraydef(p).arrayoptions);
          objectdef:
            result:=tobjectdef(p).objecttype in [odt_interfacecom,odt_dispinterface,odt_interfacecorba];
        end;
      end;


    {# returns true, if the type passed is a varset }
    function is_varset(p : tdef) : boolean;
      begin
        result:=(p.typ=setdef) and not(p.size in [1,2,4])
      end;


    function is_bareprocdef(pd : tprocdef): boolean;
      begin
        result:=(pd.maxparacount=0) and
                (is_void(pd.returndef) or
                 (pd.proctypeoption = potype_constructor));
      end;

end.
