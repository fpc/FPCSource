{
    $Id$
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
       cpuinfo,
       globals,
       node,
       symconst,symbase,symtype,symdef;

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

    {# Returns basetype of the specified integer range }
    function range_to_basetype(low,high:TConstExprInt):tbasetype;

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

    {# Returns true, if def is a 64 bit integer type }
    function is_64bitint(def : tdef) : boolean;

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


implementation

    uses
       globtype,tokens,systems,verbose,
       symtable;

    { returns true, if def uses FPU }
    function is_fpu(def : tdef) : boolean;
      begin
         is_fpu:=(def.deftype=floatdef);
      end;


    { returns true, if def is a currency type }
    function is_currency(def : tdef) : boolean;
      begin
         is_currency:=(def.deftype=floatdef) and (tfloatdef(def).typ=s64currency);
      end;


      function range_to_basetype(low,high:TConstExprInt):tbasetype;
      begin
        { generate a unsigned range if high<0 and low>=0 }
        if (low>=0) and (high<0) then
         range_to_basetype:=u32bit
        else if (low>=0) and (high<=255) then
         range_to_basetype:=u8bit
        else if (low>=-128) and (high<=127) then
         range_to_basetype:=s8bit
        else if (low>=0) and (high<=65536) then
         range_to_basetype:=u16bit
        else if (low>=-32768) and (high<=32767) then
         range_to_basetype:=s16bit
        else
         range_to_basetype:=s32bit;
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
               is_signed:=(dt in [s8bit,s16bit,s32bit,s64bit]);
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
         is_zero_based_array:=(p.deftype=arraydef) and
                              (tarraydef(p).lowrange=0) and
                              not(is_special_array(p));
      end;

    { true if p points to a dynamic array def }
    function is_dynamic_array(p : tdef) : boolean;
      begin
         is_dynamic_array:=(p.deftype=arraydef) and
           tarraydef(p).IsDynamicArray;
      end;


    { true, if p points to an open array def }
    function is_open_array(p : tdef) : boolean;
      begin
         { check for s32bittype is needed, because for u32bit the high
           range is also -1 ! (PFV) }
         is_open_array:=(p.deftype=arraydef) and
                        (tarraydef(p).rangetype.def=s32bittype.def) and
                        (tarraydef(p).lowrange=0) and
                        (tarraydef(p).highrange=-1) and
                        not(tarraydef(p).IsConstructor) and
                        not(tarraydef(p).IsVariant) and
                        not(tarraydef(p).IsArrayOfConst) and
                        not(tarraydef(p).IsDynamicArray);

      end;

    { true, if p points to an array of const def }
    function is_array_constructor(p : tdef) : boolean;
      begin
         is_array_constructor:=(p.deftype=arraydef) and
                        (tarraydef(p).IsConstructor);
      end;

    { true, if p points to a variant array }
    function is_variant_array(p : tdef) : boolean;
      begin
         is_variant_array:=(p.deftype=arraydef) and
                        (tarraydef(p).IsVariant);
      end;

    { true, if p points to an array of const }
    function is_array_of_const(p : tdef) : boolean;
      begin
         is_array_of_const:=(p.deftype=arraydef) and
                        (tarraydef(p).IsArrayOfConst);
      end;

    { true, if p points to a special array }
    function is_special_array(p : tdef) : boolean;
      begin
         is_special_array:=(p.deftype=arraydef) and
                        ((tarraydef(p).IsVariant) or
                         (tarraydef(p).IsArrayOfConst) or
                         (tarraydef(p).IsConstructor) or
                         (tarraydef(p).IsDynamicArray) or
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


    { true, if def is a 64 bit int type }
    function is_64bitint(def : tdef) : boolean;
      begin
         is_64bitint:=(def.deftype=orddef) and (torddef(def).typ in [u64bit,s64bit])
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
              if (def.deftype=orddef) and
                 (torddef(def).typ=u32bit) then
                begin
                  if (l < cardinal(lv)) or
                     (l > cardinal(hv)) then
                    begin
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
              else if (l<lv) or (l>hv) then
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
             case def.size of
               1: l := l and $ff;
               2: l := l and $ffff;
               { work around sign extension bug (to be fixed) (JM) }
               4: l := l and (int64($fffffff) shl 4 + $f);
             end;
             { do sign extension if necessary (JM) }
             if is_signed(def) then
              begin
                case def.size of
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

end.
{
  $Log$
  Revision 1.3  2003-03-17 19:05:08  peter
    * dynamic array is also a special array

  Revision 1.2  2002/12/23 20:58:03  peter
    * remove unused global var

  Revision 1.1  2002/11/25 17:43:17  peter
    * splitted defbase in defutil,symutil,defcmp
    * merged isconvertable and is_equal into compare_defs(_ext)
    * made operator search faster by walking the list only once

  Revision 1.26  2002/11/17 16:31:55  carl
    * memory optimization (3-4%) : cleanup of tai fields,
       cleanup of tdef and tsym fields.
    * make it work for m68k

  Revision 1.25  2002/11/16 18:00:53  peter
    * fix merged proc-procvar check

  Revision 1.24  2002/11/15 01:58:46  peter
    * merged changes from 1.0.7 up to 04-11
      - -V option for generating bug report tracing
      - more tracing for option parsing
      - errors for cdecl and high()
      - win32 import stabs
      - win32 records<=8 are returned in eax:edx (turned off by default)
      - heaptrc update
      - more info for temp management in .s file with EXTDEBUG

  Revision 1.23  2002/10/20 15:34:16  peter
    * removed df_unique flag. It breaks code. For a good type=type <id>
      a def copy is required

  Revision 1.22  2002/10/10 16:07:57  florian
    + several widestring/pwidechar related stuff added

  Revision 1.21  2002/10/09 21:01:41  florian
    * variants aren't compatible with nil

  Revision 1.20  2002/10/07 09:49:42  florian
    * overloaded :=-operator is now searched when looking for possible
      variant type conversions

  Revision 1.19  2002/10/06 21:02:17  peter
    * fixed limit checking for qword

  Revision 1.18  2002/10/06 15:08:59  peter
    * only check for forwarddefs the definitions that really belong to
      the current procsym

  Revision 1.17  2002/10/06 12:25:04  florian
    + proper support of type <id> = type <another id>;

  Revision 1.16  2002/10/05 12:43:24  carl
    * fixes for Delphi 6 compilation
     (warning : Some features do not work under Delphi)

  Revision 1.15  2002/10/05 00:50:01  peter
    * check parameters from left to right in equal_paras, so default
      parameters are checked at the end

  Revision 1.14  2002/09/30 07:00:44  florian
    * fixes to common code to get the alpha compiler compiled applied

  Revision 1.13  2002/09/22 14:02:34  carl
    * stack checking cannot be called before system unit is initialized
    * MC68020 define

  Revision 1.12  2002/09/16 14:11:12  peter
    * add argument to equal_paras() to support default values or not

  Revision 1.11  2002/09/15 17:54:46  peter
    * allow default parameters in equal_paras

  Revision 1.10  2002/09/08 11:10:17  carl
    * bugfix 2109 (bad imho, but only way)

  Revision 1.9  2002/09/07 15:25:02  peter
    * old logs removed and tabs fixed

  Revision 1.8  2002/09/07 09:16:55  carl
    * fix my stupid copy and paste bug

  Revision 1.7  2002/09/06 19:58:31  carl
   * start bugfix 1996
   * 64-bit typed constant now work correctly and fully (bugfix 2001)

  Revision 1.6  2002/08/20 10:31:26  daniel
   * Tcallnode.det_resulttype rewritten

  Revision 1.5  2002/08/12 20:39:17  florian
    * casting of classes to interface fixed when the interface was
      implemented by a parent class

  Revision 1.4  2002/08/12 14:17:56  florian
    * nil is now recognized as being compatible with a dynamic array

  Revision 1.3  2002/08/05 18:27:48  carl
    + more more more documentation
    + first version include/exclude (can't test though, not enough scratch for i386 :()...

  Revision 1.2  2002/07/23 09:51:22  daniel
  * Tried to make Tprocsym.defs protected. I didn't succeed but the cleanups
    are worth comitting.

  Revision 1.1  2002/07/20 11:57:53  florian
    * types.pas renamed to defbase.pas because D6 contains a types
      unit so this would conflicts if D6 programms are compiled
    + Willamette/SSE2 instructions to assembler added

  Revision 1.75  2002/07/11 14:41:32  florian
    * start of the new generic parameter handling

  Revision 1.74  2002/07/01 16:23:54  peter
    * cg64 patch
    * basics for currency
    * asnode updates for class and interface (not finished)

  Revision 1.73  2002/05/18 13:34:21  peter
    * readded missing revisions

  Revision 1.72  2002/05/16 19:46:47  carl
  + defines.inc -> fpcdefs.inc to avoid conflicts if compiling by hand
  + try to fix temp allocation (still in ifdef)
  + generic constructor calls
  + start of tassembler / tmodulebase class cleanup

  Revision 1.70  2002/05/12 16:53:16  peter
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

  Revision 1.69  2002/04/25 20:16:39  peter
    * moved more routines from cga/n386util

  Revision 1.68  2002/04/15 19:08:22  carl
  + target_info.size_of_pointer -> pointer_size
  + some cleanup of unused types/variables

  Revision 1.67  2002/04/07 13:40:29  carl
  + update documentation

  Revision 1.66  2002/04/02 17:11:32  peter
    * tlocation,treference update
    * LOC_CONSTANT added for better constant handling
    * secondadd splitted in multiple routines
    * location_force_reg added for loading a location to a register
      of a specified size
    * secondassignment parses now first the right and then the left node
      (this is compatible with Kylix). This saves a lot of push/pop especially
      with string operations
    * adapted some routines to use the new cg methods

  Revision 1.65  2002/04/01 20:57:14  jonas
    * fixed web bug 1907
    * fixed some other procvar related bugs (all related to accepting procvar
        constructs with either too many or too little parameters)
    (both merged, includes second typo fix of pexpr.pas)

  Revision 1.64  2002/01/24 18:25:53  peter
   * implicit result variable generation for assembler routines
   * removed m_tp modeswitch, use m_tp7 or not(m_fpc) instead

  Revision 1.63  2002/01/24 12:33:53  jonas
    * adapted ranges of native types to int64 (e.g. high cardinal is no
      longer longint($ffffffff), but just $fffffff in psystem)
    * small additional fix in 64bit rangecheck code generation for 32 bit
      processors
    * adaption of ranges required the matching talgorithm used for selecting
      which overloaded procedure to call to be adapted. It should now always
      select the closest match for ordinal parameters.
    + inttostr(qword) in sysstr.inc/sysstrh.inc
    + abs(int64), sqr(int64), sqr(qword) in systemh.inc/generic.inc (previous
      fixes were required to be able to add them)
    * is_in_limit() moved from ncal to types unit, should always be used
      instead of direct comparisons of low/high values of orddefs because
      qword is a special case

}
