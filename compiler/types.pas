{
    $Id$
    Copyright (C) 1993-98 by Florian Klaempfl

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
unit types;
interface

    uses
       cobjects,symtable;

    type
       tmmxtype = (mmxno,mmxu8bit,mmxs8bit,mmxu16bit,mmxs16bit,
                   mmxu32bit,mmxs32bit,mmxfixed16,mmxsingle);

    const
       { true if we must never copy this parameter }
       never_copy_const_param : boolean = false;

{*****************************************************************************
                          Basic type functions
 *****************************************************************************}

    { returns true, if def defines an ordinal type }
    function is_ordinal(def : pdef) : boolean;

    { returns the min. value of the type }
    function get_min_value(def : pdef) : longint;

    { returns true, if def defines an ordinal type }
    function is_integer(def : pdef) : boolean;

    { true if p is a boolean }
    function is_boolean(def : pdef) : boolean;

    { true if p is a char }
    function is_char(def : pdef) : boolean;

    { true if p is a smallset def }
    function is_smallset(p : pdef) : boolean;

    { returns true, if def defines a signed data type (only for ordinal types) }
    function is_signed(def : pdef) : boolean;

{*****************************************************************************
                              Array helper functions
 *****************************************************************************}

    { true, if p points to a zero based (non special like open or
      dynamic array def, mainly this is used to see if the array
      is convertable to a pointer }
    function is_zero_based_array(p : pdef) : boolean;

    { true if p points to an open array def }
    function is_open_array(p : pdef) : boolean;

    { true, if p points to an array of const def }
    function is_array_constructor(p : pdef) : boolean;

    { true, if p points to a variant array }
    function is_variant_array(p : pdef) : boolean;

    { true, if p points to an array of const }
    function is_array_of_const(p : pdef) : boolean;

    { true, if p points any kind of special array }
    function is_special_array(p : pdef) : boolean;

    { true if p is a char array def }
    function is_chararray(p : pdef) : boolean;

{*****************************************************************************
                          String helper functions
 *****************************************************************************}

    { true if p points to an open string def }
    function is_open_string(p : pdef) : boolean;

    { true if p is an ansi string def }
    function is_ansistring(p : pdef) : boolean;

    { true if p is a long string def }
    function is_longstring(p : pdef) : boolean;

    { true if p is a wide string def }
    function is_widestring(p : pdef) : boolean;

    { true if p is a short string def }
    function is_shortstring(p : pdef) : boolean;

    { true if p is a pchar def }
    function is_pchar(p : pdef) : boolean;

    { returns true, if def uses FPU }
    function is_fpu(def : pdef) : boolean;

    { true if the return value is in EAX }
    function ret_in_acc(def : pdef) : boolean;

    { true if uses a parameter as return value }
    function ret_in_param(def : pdef) : boolean;

    { true, if def is a 64 bit int type }
    function is_64bitint(def : pdef) : boolean;

    function push_high_param(def : pdef) : boolean;

    { true if a parameter is too large to copy and only the address is pushed }
    function push_addr_param(def : pdef) : boolean;

    { true, if def1 and def2 are semantical the same }
    function is_equal(def1,def2 : pdef) : boolean;

    { checks for type compatibility (subgroups of type)  }
    { used for case statements... probably missing stuff }
    { to use on other types                              }
    function is_subequal(def1, def2: pdef): boolean;

    { same as is_equal, but with error message if failed }
    function CheckTypes(def1,def2 : pdef) : boolean;

    { true, if two parameter lists are equal        }
    { if value_equal_const is true, call by value   }
    { and call by const parameter are assumed as    }
    { equal                                         }
    function equal_paras(def1,def2 : pdefcoll;value_equal_const : boolean) : boolean;


    { true if a type can be allowed for another one
      in a func var }
    function convertable_paras(def1,def2 : pdefcoll;value_equal_const : boolean) : boolean;

    { true if a function can be assigned to a procvar }
    function proc_to_procvar_equal(def1:pprocdef;def2:pprocvardef) : boolean;

    { if l isn't in the range of def a range check error is generated and
      the value is placed within the range }
    procedure testrange(def : pdef;var l : longint);

    { returns the range of def }
    procedure getrange(def : pdef;var l : longint;var h : longint);

    { some type helper routines for MMX support }
    function is_mmx_able_array(p : pdef) : boolean;

    { returns the mmx type }
    function mmx_type(p : pdef) : tmmxtype;


implementation

    uses
       strings,globtype,globals,htypechk,
       tree,verbose,symconst;


    function equal_paras(def1,def2 : pdefcoll;value_equal_const : boolean) : boolean;
      begin
         while (assigned(def1)) and (assigned(def2)) do
           begin
              if value_equal_const then
                begin
                   if not(is_equal(def1^.data,def2^.data)) or
                     ((def1^.paratyp<>def2^.paratyp) and
                      ((def1^.paratyp=vs_var) or
                       (def1^.paratyp=vs_var)
                      )
                     ) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end
              else
                begin
                   if not(is_equal(def1^.data,def2^.data)) or
                     (def1^.paratyp<>def2^.paratyp) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              def1:=def1^.next;
              def2:=def2^.next;
           end;
         if (def1=nil) and (def2=nil) then
           equal_paras:=true
         else
           equal_paras:=false;
      end;

    function convertable_paras(def1,def2 : pdefcoll;value_equal_const : boolean) : boolean;
      var doconv : tconverttype;
      begin
         while (assigned(def1)) and (assigned(def2)) do
           begin
              if value_equal_const then
                begin
                   if (isconvertable(def1^.data,def2^.data,doconv,callparan,false)=0) or
                     ((def1^.paratyp<>def2^.paratyp) and
                      ((def1^.paratyp=vs_var) or
                       (def1^.paratyp=vs_var)
                      )
                     ) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end
              else
                begin
                   if (isconvertable(def1^.data,def2^.data,doconv,callparan,false)=0) or
                     (def1^.paratyp<>def2^.paratyp) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              def1:=def1^.next;
              def2:=def2^.next;
           end;
         if (def1=nil) and (def2=nil) then
           convertable_paras:=true
         else
           convertable_paras:=false;
      end;


    { true if a function can be assigned to a procvar }
    function proc_to_procvar_equal(def1:pprocdef;def2:pprocvardef) : boolean;
      const
        po_comp = po_compatibility_options-[po_methodpointer];
      var
        ismethod : boolean;
      begin
         proc_to_procvar_equal:=false;
         if not(assigned(def1)) or not(assigned(def2)) then
           exit;
         { check for method pointer }
         ismethod:=assigned(def1^.owner) and
                   (def1^.owner^.symtabletype=objectsymtable) and
                   assigned(def1^.owner^.defowner) and
                   (pobjectdef(def1^.owner^.defowner)^.is_class);
         if (ismethod and not (po_methodpointer in def2^.procoptions)) or
            (not(ismethod) and (po_methodpointer in def2^.procoptions)) then
          begin
            Message(type_e_no_method_and_procedure_not_compatible);
            exit;
          end;
         { check return value and para's and options, methodpointer is already checked
           parameters may also be convertable }
         if is_equal(def1^.retdef,def2^.retdef) and
            (equal_paras(def1^.para1,def2^.para1,false) or
             convertable_paras(def1^.para1,def2^.para1,false)) and
            ((po_comp * def1^.procoptions)= (po_comp * def2^.procoptions)) then
           proc_to_procvar_equal:=true
         else
           proc_to_procvar_equal:=false;
      end;


    { returns true, if def uses FPU }
    function is_fpu(def : pdef) : boolean;
      begin
         is_fpu:=(def^.deftype=floatdef) and (pfloatdef(def)^.typ<>f32bit);
      end;


    { true if p is an ordinal }
    function is_ordinal(def : pdef) : boolean;
      var
         dt : tbasetype;
      begin
         case def^.deftype of
           orddef :
             begin
               dt:=porddef(def)^.typ;
               is_ordinal:=dt in [uchar,
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
    function get_min_value(def : pdef) : longint;
      begin
         case def^.deftype of
           orddef:
             get_min_value:=porddef(def)^.low;
           enumdef:
             get_min_value:=penumdef(def)^.min;
           else
             get_min_value:=0;
         end;
      end;


    { true if p is an integer }
    function is_integer(def : pdef) : boolean;
      begin
        is_integer:=(def^.deftype=orddef) and
                    (porddef(def)^.typ in [uauto,u8bit,u16bit,u32bit,u64bit,
                                           s8bit,s16bit,s32bit,s64bit]);
      end;


    { true if p is a boolean }
    function is_boolean(def : pdef) : boolean;
      begin
        is_boolean:=(def^.deftype=orddef) and
                    (porddef(def)^.typ in [bool8bit,bool16bit,bool32bit]);
      end;


    { true if p is a char }
    function is_char(def : pdef) : boolean;
      begin
        is_char:=(def^.deftype=orddef) and
                 (porddef(def)^.typ=uchar);
      end;


    { true if p is signed (integer) }
    function is_signed(def : pdef) : boolean;
      var
         dt : tbasetype;
      begin
         case def^.deftype of
           orddef :
             begin
               dt:=porddef(def)^.typ;
               is_signed:=(dt in [s8bit,s16bit,s32bit,s64bit]);
             end;
           enumdef :
             is_signed:=false;
           else
             is_signed:=false;
         end;
      end;


    { true, if p points to an open array def }
    function is_open_string(p : pdef) : boolean;
      begin
         is_open_string:=(p^.deftype=stringdef) and
                         (pstringdef(p)^.string_typ=st_shortstring) and
                         (pstringdef(p)^.len=0);
      end;


    { true, if p points to a zero based array def }
    function is_zero_based_array(p : pdef) : boolean;
      begin
         is_zero_based_array:=(p^.deftype=arraydef) and
                              (parraydef(p)^.lowrange=0) and
                              not(is_special_array(p));
      end;

    { true, if p points to an open array def }
    function is_open_array(p : pdef) : boolean;
      begin
         { check for s32bitdef is needed, because for u32bit the high
           range is also -1 ! (PFV) }
         is_open_array:=(p^.deftype=arraydef) and
                        (parraydef(p)^.rangedef=pdef(s32bitdef)) and
                        (parraydef(p)^.lowrange=0) and
                        (parraydef(p)^.highrange=-1) and
                        not(parraydef(p)^.IsConstructor) and
                        not(parraydef(p)^.IsVariant) and
                        not(parraydef(p)^.IsArrayOfConst);

      end;

    { true, if p points to an array of const def }
    function is_array_constructor(p : pdef) : boolean;
      begin
         is_array_constructor:=(p^.deftype=arraydef) and
                        (parraydef(p)^.IsConstructor);
      end;

    { true, if p points to a variant array }
    function is_variant_array(p : pdef) : boolean;
      begin
         is_variant_array:=(p^.deftype=arraydef) and
                        (parraydef(p)^.IsVariant);
      end;

    { true, if p points to an array of const }
    function is_array_of_const(p : pdef) : boolean;
      begin
         is_array_of_const:=(p^.deftype=arraydef) and
                        (parraydef(p)^.IsArrayOfConst);
      end;

    { true, if p points to a special array }
    function is_special_array(p : pdef) : boolean;
      begin
         is_special_array:=(p^.deftype=arraydef) and
                        ((parraydef(p)^.IsVariant) or
                         (parraydef(p)^.IsArrayOfConst) or
                         (parraydef(p)^.IsConstructor) or
                         is_open_array(p)
                        );
      end;

    { true if p is an ansi string def }
    function is_ansistring(p : pdef) : boolean;
      begin
         is_ansistring:=(p^.deftype=stringdef) and
                        (pstringdef(p)^.string_typ=st_ansistring);
      end;


    { true if p is an long string def }
    function is_longstring(p : pdef) : boolean;
      begin
         is_longstring:=(p^.deftype=stringdef) and
                        (pstringdef(p)^.string_typ=st_longstring);
      end;


    { true if p is an wide string def }
    function is_widestring(p : pdef) : boolean;
      begin
         is_widestring:=(p^.deftype=stringdef) and
                        (pstringdef(p)^.string_typ=st_widestring);
      end;


    { true if p is an short string def }
    function is_shortstring(p : pdef) : boolean;
      begin
         is_shortstring:=(p^.deftype=stringdef) and
                         (pstringdef(p)^.string_typ=st_shortstring);
      end;

    { true if p is a char array def }
    function is_chararray(p : pdef) : boolean;
      begin
        is_chararray:=(p^.deftype=arraydef) and
                      is_equal(parraydef(p)^.definition,cchardef) and
                      not(is_special_array(p));
      end;


    { true if p is a pchar def }
    function is_pchar(p : pdef) : boolean;
      begin
        is_pchar:=(p^.deftype=pointerdef) and
                  is_equal(Ppointerdef(p)^.definition,cchardef);
      end;


    { true if p is a smallset def }
    function is_smallset(p : pdef) : boolean;
      begin
        is_smallset:=(p^.deftype=setdef) and
                     (psetdef(p)^.settype=smallset);
      end;


    { true if the return value is in accumulator (EAX for i386), D0 for 68k }
    function ret_in_acc(def : pdef) : boolean;
      begin
         ret_in_acc:=(def^.deftype in [orddef,pointerdef,enumdef,classrefdef]) or
                     ((def^.deftype=stringdef) and (pstringdef(def)^.string_typ in [st_ansistring,st_widestring])) or
                     ((def^.deftype=procvardef) and not(po_methodpointer in pprocvardef(def)^.procoptions)) or
                     ((def^.deftype=objectdef) and pobjectdef(def)^.is_class) or
                     ((def^.deftype=setdef) and (psetdef(def)^.settype=smallset)) or
                     ((def^.deftype=floatdef) and (pfloatdef(def)^.typ=f32bit));
      end;


    { true, if def is a 64 bit int type }
    function is_64bitint(def : pdef) : boolean;
      begin
         is_64bitint:=(def^.deftype=orddef) and (porddef(def)^.typ in [u64bit,s64bit])
      end;


    { true if uses a parameter as return value }
    function ret_in_param(def : pdef) : boolean;
      begin
         ret_in_param:=(def^.deftype in [arraydef,recorddef]) or
           ((def^.deftype=stringdef) and (pstringdef(def)^.string_typ in [st_shortstring,st_longstring])) or
           ((def^.deftype=procvardef) and (po_methodpointer in pprocvardef(def)^.procoptions)) or
           ((def^.deftype=objectdef) and not(pobjectdef(def)^.is_class)) or
           ((def^.deftype=setdef) and (psetdef(def)^.settype<>smallset));
      end;


    function push_high_param(def : pdef) : boolean;
      begin
         push_high_param:=is_open_array(def) or
                          is_open_string(def) or
                          is_array_of_const(def);
      end;


    { true if a parameter is too large to copy and only the address is pushed }
    function push_addr_param(def : pdef) : boolean;
      begin
         push_addr_param:=never_copy_const_param or
           (def^.deftype = formaldef) or
           { copy directly small records or arrays unless array of const ! PM }
           ((def^.deftype in [arraydef,recorddef]) and
            ((Parraydef(def)^.highrange<Parraydef(def)^.lowrange) 
             or (def^.size>4) or
             ((def^.deftype=arraydef) and
              (parraydef(def)^.IsConstructor or
               parraydef(def)^.isArrayOfConst or
               is_open_array(def)
              )
             )
            )
           ) or
           ((def^.deftype=objectdef) and not(pobjectdef(def)^.is_class)) or
           ((def^.deftype=stringdef) and (pstringdef(def)^.string_typ in [st_shortstring,st_longstring])) or
           ((def^.deftype=procvardef) and (po_methodpointer in pprocvardef(def)^.procoptions)) or
           ((def^.deftype=setdef) and (psetdef(def)^.settype<>smallset));
      end;

    { test if l is in the range of def, outputs error if out of range }
    procedure testrange(def : pdef;var l : longint);
      var
         lv,hv: longint;

      begin
         { for 64 bit types we need only to check if it is less than }
         { zero, if def is a qword node                              }
         if is_64bitint(def) then
           begin
              if (l<0) and (porddef(def)^.typ=u64bit) then
                begin
                   l:=0;
                   if (cs_check_range in aktlocalswitches) then
                     Message(parser_e_range_check_error)
                   else
                     Message(parser_w_range_check_error);
                end;
           end
         else
           begin
              getrange(def,lv,hv);
              if (def^.deftype=orddef) and
                 (porddef(def)^.typ=u32bit) then
                begin
                   if lv<=hv then
                     begin
                        if (l<lv) or (l>hv) then
                          begin
                             if (cs_check_range in aktlocalswitches) then
                               Message(parser_e_range_check_error)
                             else
                               Message(parser_w_range_check_error);
                          end;
                     end
                   else
                     { this happens with the wrap around problem  }
                     { if lv is positive and hv is over $7ffffff  }
                     { so it seems negative                       }
                     begin
                        if ((l>=0) and (l<lv)) or
                           ((l<0) and (l>hv)) then
                          begin
                             if (cs_check_range in aktlocalswitches) then
                               Message(parser_e_range_check_error)
                             else
                               Message(parser_w_range_check_error);
                          end;
                     end;
                end
              else if (l<lv) or (l>hv) then
                begin
                   if (def^.deftype=enumdef) or
                      (cs_check_range in aktlocalswitches) then
                     Message(parser_e_range_check_error)
                   else
                     Message(parser_w_range_check_error);
                   { Fix the value to be in range }
                   l:=lv+(l mod (hv-lv+1));
                end;
           end;
      end;


    { return the range from def in l and h }
    procedure getrange(def : pdef;var l : longint;var h : longint);
      begin
        case def^.deftype of
          orddef :
            begin
              l:=porddef(def)^.low;
              h:=porddef(def)^.high;
            end;
          enumdef :
            begin
              l:=penumdef(def)^.min;
              h:=penumdef(def)^.max;
            end;
          arraydef :
            begin
              l:=parraydef(def)^.lowrange;
              h:=parraydef(def)^.highrange;
            end;
        else
          internalerror(987);
        end;
      end;


    function mmx_type(p : pdef) : tmmxtype;
      begin
         mmx_type:=mmxno;
         if is_mmx_able_array(p) then
           begin
              if parraydef(p)^.definition^.deftype=floatdef then
                case pfloatdef(parraydef(p)^.definition)^.typ of
                  s32real:
                    mmx_type:=mmxsingle;
                  f16bit:
                    mmx_type:=mmxfixed16
                end
              else
                case porddef(parraydef(p)^.definition)^.typ of
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


    function is_mmx_able_array(p : pdef) : boolean;
      begin
{$ifdef SUPPORT_MMX}
         if (cs_mmx_saturation in aktlocalswitches) then
           begin
              is_mmx_able_array:=(p^.deftype=arraydef) and
                not(is_special_array(p)) and
                (
                 (
                  (parraydef(p)^.definition^.deftype=orddef) and
                  (
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=1) and
                    (porddef(parraydef(p)^.definition)^.typ in [u32bit,s32bit])
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=3) and
                    (porddef(parraydef(p)^.definition)^.typ in [u16bit,s16bit])
                   )
                  )
                 )
                 or
                (
                 (
                  (parraydef(p)^.definition^.deftype=floatdef) and
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=3) and
                   (pfloatdef(parraydef(p)^.definition)^.typ=f16bit)
                  ) or
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=1) and
                   (pfloatdef(parraydef(p)^.definition)^.typ=s32real)
                  )
                 )
                )
              );
           end
         else
           begin
              is_mmx_able_array:=(p^.deftype=arraydef) and
                (
                 (
                  (parraydef(p)^.definition^.deftype=orddef) and
                  (
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=1) and
                    (porddef(parraydef(p)^.definition)^.typ in [u32bit,s32bit])
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=3) and
                    (porddef(parraydef(p)^.definition)^.typ in [u16bit,s16bit])
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=7) and
                    (porddef(parraydef(p)^.definition)^.typ in [u8bit,s8bit])
                   )
                  )
                 )
                 or
                 (
                  (parraydef(p)^.definition^.deftype=floatdef) and
                  (
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=3) and
                    (pfloatdef(parraydef(p)^.definition)^.typ=f32bit)
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=1) and
                    (pfloatdef(parraydef(p)^.definition)^.typ=s32real)
                   )
                  )
                 )
                );
           end;
{$else SUPPORT_MMX}
         is_mmx_able_array:=false;
{$endif SUPPORT_MMX}
      end;


    function is_equal(def1,def2 : pdef) : boolean;
      var
         b : boolean;
         hd : pdef;
         hp1,hp2 : pdefcoll;
      begin
         { both types must exists }
         if not (assigned(def1) and assigned(def2)) then
          begin
            is_equal:=false;
            exit;
          end;

         { be sure, that if there is a stringdef, that this is def1 }
         if def2^.deftype=stringdef then
           begin
              hd:=def1;
              def1:=def2;
              def2:=hd;
           end;
         b:=false;

         { both point to the same definition ? }
         if def1=def2 then
           b:=true
         else
         { pointer with an equal definition are equal }
           if (def1^.deftype=pointerdef) and (def2^.deftype=pointerdef) then
             begin
                { here a problem detected in tabsolutesym }
                { the types can be forward type !!        }
                if assigned(def1^.sym) and (sp_forwarddef in def1^.sym^.symoptions) then
                  b:=(def1^.sym=def2^.sym)
                else
                  b:=ppointerdef(def1)^.definition=ppointerdef(def2)^.definition;
             end
         else
         { ordinals are equal only when the ordinal type is equal }
           if (def1^.deftype=orddef) and (def2^.deftype=orddef) then
             begin
                case porddef(def1)^.typ of
                u8bit,u16bit,u32bit,
                s8bit,s16bit,s32bit:
                  b:=((porddef(def1)^.typ=porddef(def2)^.typ) and
                   (porddef(def1)^.low=porddef(def2)^.low) and
                   (porddef(def1)^.high=porddef(def2)^.high));
                uvoid,uchar,
                bool8bit,bool16bit,bool32bit:
                  b:=(porddef(def1)^.typ=porddef(def2)^.typ);
                end;
             end
         else
           if (def1^.deftype=floatdef) and (def2^.deftype=floatdef) then
             b:=pfloatdef(def1)^.typ=pfloatdef(def2)^.typ
         else
           { strings with the same length are equal }
           if (def1^.deftype=stringdef) and (def2^.deftype=stringdef) and
              (pstringdef(def1)^.string_typ=pstringdef(def2)^.string_typ) then
             begin
                b:=not(is_shortstring(def1)) or
                   (pstringdef(def1)^.len=pstringdef(def2)^.len);
             end
         else
           if (def1^.deftype=formaldef) and (def2^.deftype=formaldef) then
             b:=true
         { file types with the same file element type are equal }
         { this is a problem for assign !!                      }
         { changed to allow if one is untyped                   }
         { all typed files are equal to the special             }
         { typed file that has voiddef as elemnt type           }
         { but must NOT match for text file !!!                 }
         else
            if (def1^.deftype=filedef) and (def2^.deftype=filedef) then
              b:=(pfiledef(def1)^.filetype=pfiledef(def2)^.filetype) and
                 ((
                 ((pfiledef(def1)^.typed_as=nil) and
                  (pfiledef(def2)^.typed_as=nil)) or
                 (
                  (pfiledef(def1)^.typed_as<>nil) and
                  (pfiledef(def2)^.typed_as<>nil) and
                  is_equal(pfiledef(def1)^.typed_as,pfiledef(def2)^.typed_as)
                 ) or
                 ( (pfiledef(def1)^.typed_as=pdef(voiddef)) or
                   (pfiledef(def2)^.typed_as=pdef(voiddef))
                 )))
         { sets with the same element type are equal }
         else
           if (def1^.deftype=setdef) and (def2^.deftype=setdef) then
             begin
                if assigned(psetdef(def1)^.setof) and
                   assigned(psetdef(def2)^.setof) then
                  b:=(psetdef(def1)^.setof^.deftype=psetdef(def2)^.setof^.deftype)
                else
                  b:=true;
             end
         else
           if (def1^.deftype=procvardef) and (def2^.deftype=procvardef) then
             begin
                { poassembler isn't important for compatibility }
                { if a method is assigned to a methodpointer    }
                { is checked before                             }
                b:=((pprocvardef(def1)^.procoptions * po_compatibility_options)=
                    (pprocvardef(def2)^.procoptions * po_compatibility_options)) and
                   is_equal(pprocvardef(def1)^.retdef,pprocvardef(def2)^.retdef);
                { now evalute the parameters }
                if b then
                  begin
                     hp1:=pprocvardef(def1)^.para1;
                     hp2:=pprocvardef(def1)^.para1;
                     while assigned(hp1) and assigned(hp2) do
                       begin
                          if not(is_equal(hp1^.data,hp2^.data)) or
                            not(hp1^.paratyp=hp2^.paratyp) then
                            begin
                               b:=false;
                               break;
                            end;
                          hp1:=hp1^.next;
                          hp2:=hp2^.next;
                       end;
                     b:=(hp1=nil) and (hp2=nil);
                  end;
             end
         else
           if (def1^.deftype=arraydef) and (def2^.deftype=arraydef) then
             begin
               if is_open_array(def1) or is_open_array(def2) or
                  is_array_of_const(def1) or is_array_of_const(def2) then
                begin
                  if parraydef(def1)^.IsArrayOfConst or parraydef(def2)^.IsArrayOfConst then
                   b:=true
                  else
                   b:=is_equal(parraydef(def1)^.definition,parraydef(def2)^.definition);
                end
               else
                begin
                  b:=not(m_tp in aktmodeswitches) and
                     not(m_delphi in aktmodeswitches) and
                     (parraydef(def1)^.lowrange=parraydef(def2)^.lowrange) and
                     (parraydef(def1)^.highrange=parraydef(def2)^.highrange) and
                     is_equal(parraydef(def1)^.definition,parraydef(def2)^.definition) and
                     is_equal(parraydef(def1)^.rangedef,parraydef(def2)^.rangedef);
                end;
             end
         else
           if (def1^.deftype=classrefdef) and (def2^.deftype=classrefdef) then
             begin
                { similar to pointerdef: }
                if assigned(def1^.sym) and (sp_forwarddef in def1^.sym^.symoptions) then
                  b:=(def1^.sym=def2^.sym)
                else
                  b:=is_equal(pclassrefdef(def1)^.definition,pclassrefdef(def2)^.definition);
             end;
         is_equal:=b;
      end;


    function is_subequal(def1, def2: pdef): boolean;
      Begin
        if assigned(def1) and assigned(def2) then
        Begin
          is_subequal := FALSE;
          if (def1^.deftype = orddef) and (def2^.deftype = orddef) then
            Begin
              { see p.47 of Turbo Pascal 7.01 manual for the separation of types }
              { range checking for case statements is done with testrange        }
              case porddef(def1)^.typ of
                u8bit,u16bit,u32bit,
                s8bit,s16bit,s32bit :
                  is_subequal:=(porddef(def2)^.typ in [s32bit,u32bit,u8bit,s8bit,s16bit,u16bit]);
                bool8bit,bool16bit,bool32bit :
                  is_subequal:=(porddef(def2)^.typ in [bool8bit,bool16bit,bool32bit]);
                uchar :
                  is_subequal:=(porddef(def2)^.typ=uchar);
              end;
            end
          else
            Begin
              { I assume that both enumerations are equal when the first }
              { pointers are equal.                                      }
              if (def1^.deftype = enumdef) and (def2^.deftype =enumdef) then
                Begin
                  if penumdef(def1)^.firstenum = penumdef(def2)^.firstenum then
                     is_subequal := TRUE;
                end;
            end;
        end; { endif assigned ... }
      end;

    function CheckTypes(def1,def2 : pdef) : boolean;

      var
         s1,s2 : string;

      begin
        if not is_equal(def1,def2) then
         begin
           { Crash prevention }
           if (not assigned(def1)) or (not assigned(def2)) then
             Message(type_e_mismatch)
           else
             begin
                s1:=def1^.typename;
                s2:=def2^.typename;
                if (s1<>'<unknown type>') and (s2<>'<unknown type>') then
                  Message2(type_e_not_equal_types,def1^.typename,def2^.typename)
                else
                  Message(type_e_mismatch);
             end;
           CheckTypes:=false;
         end
        else
         CheckTypes:=true;
      end;

end.
{
  $Log$
  Revision 1.80  1999-08-05 22:42:49  daniel
  * Fixed potential bug for open arrays (Their size is not known at
    compilation time).

  Revision 1.79  1999/08/03 22:03:41  peter
    * moved bitmask constants to sets
    * some other type/const renamings

  Revision 1.78  1999/07/30 12:26:42  peter
    * array is_equal disabled for tp,delphi mode

  Revision 1.77  1999/07/29 11:41:51  peter
    * array is_equal extended

  Revision 1.76  1999/07/27 23:39:15  peter
    * open array checks also for s32bitdef, because u32bit also has a
      high range of -1

  Revision 1.75  1999/07/06 21:48:29  florian
    * a lot bug fixes:
       - po_external isn't any longer necessary for procedure compatibility
       - m_tp_procvar is in -Sd now available
       - error messages of procedure variables improved
       - return values with init./finalization fixed
       - data types with init./finalization aren't any longer allowed in variant
         record

  Revision 1.74  1999/07/01 15:49:24  florian
    * int64/qword type release
    + lo/hi for int64/qword

  Revision 1.73  1999/06/28 22:29:22  florian
    * qword division fixed
    + code for qword/int64 type casting added:
      range checking isn't implemented yet

  Revision 1.72  1999/06/13 22:41:08  peter
    * merged from fixes

  Revision 1.71.2.1  1999/06/13 22:37:17  peter
    * convertable para's doesn't check for equal, added equal para's to
      proc2procvar check

  Revision 1.71  1999/06/03 09:34:13  peter
    * better methodpointer check for proc->procvar

  Revision 1.70  1999/06/02 22:25:55  pierre
  types.pas

  Revision 1.69  1999/06/02 10:11:55  florian
    * make cycle fixed i.e. compilation with 0.99.10
    * some fixes for qword
    * start of register calling conventions

  Revision 1.68  1999/06/01 19:27:58  peter
    * better checks for procvar and methodpointer

  Revision 1.67  1999/05/31 22:54:19  peter
    * when range check error is found then fix the value to be within the
      range

  Revision 1.66  1999/05/28 11:00:51  peter
    * removed ungettempoftype

  Revision 1.65  1999/05/23 18:42:23  florian
    * better error recovering in typed constants
    * some problems with arrays of const fixed, some problems
      due my previous
       - the location type of array constructor is now LOC_MEM
       - the pushing of high fixed
       - parameter copying fixed
       - zero temp. allocation removed
    * small problem in the assembler writers fixed:
      ref to nil wasn't written correctly

  Revision 1.64  1999/05/19 20:55:08  florian
    * fix of my previous commit

  Revision 1.63  1999/05/19 20:40:15  florian
    * fixed a couple of array related bugs:
      - var a : array[0..1] of char;   p : pchar;  p:=a+123; works now
      - open arrays with an odd size doesn't work: movsb wasn't generated
      - introduced some new array type helper routines (is_special_array) etc.
      - made the array type checking in isconvertable more strict, often
        open array can be used where is wasn't allowed etc...

  Revision 1.62  1999/05/19 16:48:29  florian
    * tdef.typename: returns a now a proper type name for the most types

  Revision 1.61  1999/05/19 10:31:56  florian
    * two bugs reported by Romio (bugs 13) are fixed:
        - empty array constructors are now handled correctly (e.g. for sysutils.format)
        - comparsion of ansistrings was sometimes coded wrong

  Revision 1.60  1999/05/18 14:16:01  peter
    * containsself fixes
    * checktypes()

  Revision 1.59  1999/05/18 09:52:24  peter
    * procedure of object and addrn fixes

  Revision 1.58  1999/04/19 09:29:51  pierre
    + ungettempoftype(pdef) boolean function
      returns true (can call ungetiftemp )
      unless the temp should be "unget" with temptoremove
      (currently ansistring or widestring !)

  Revision 1.57  1999/04/14 09:15:08  peter
    * first things to store the symbol/def number in the ppu

  Revision 1.56  1999/03/24 23:17:42  peter
    * fixed bugs 212,222,225,227,229,231,233

  Revision 1.55  1999/03/09 11:45:42  pierre
   * small arrays and records (size <=4) are copied directly

  Revision 1.54  1999/03/02 22:52:20  peter
    * fixed char array, which can start with all possible values

  Revision 1.53  1999/02/25 21:02:57  peter
    * ag386bin updates
    + coff writer

  Revision 1.52  1999/02/24 09:51:44  florian
    * wrong warning fixed, if a non-virtual method was hidden by a virtual
      method (repoerted by Matthias Koeppe)

  Revision 1.51  1999/02/22 23:33:31  florian
    + message directive for integers added

  Revision 1.50  1999/02/22 20:13:42  florian
    + first implementation of message keyword

  Revision 1.49  1999/02/16 00:45:30  peter
    * fixed crashes by forgotten strpnew() for init_symbol

  Revision 1.48  1999/02/09 23:03:08  florian
    * check for duplicate field names in inherited classes/objects
    * bug with self from the mailing list solved (the problem
      was that classes were sometimes pushed wrong)

  Revision 1.47  1999/01/27 00:14:01  florian
    * "procedure of object"-stuff fixed

  Revision 1.46  1999/01/21 22:10:54  peter
    * fixed array of const
    * generic platform independent high() support

  Revision 1.45  1999/01/20 12:34:22  peter
    * fixed typed file read/write

  Revision 1.44  1999/01/15 11:33:03  pierre
   * bug in mmx code removed

  Revision 1.43  1998/12/30 13:41:20  peter
    * released valuepara

  Revision 1.42  1998/12/11 00:04:03  peter
    + globtype,tokens,version unit splitted from globals

  Revision 1.41  1998/12/10 09:47:33  florian
    + basic operations with int64/qord (compiler with -dint64)
    + rtti of enumerations extended: names are now written

  Revision 1.40  1998/12/04 10:18:14  florian
    * some stuff for procedures of object added
    * bug with overridden virtual constructors fixed (reported by Italo Gomes)

  Revision 1.39  1998/11/27 14:50:55  peter
    + open strings, $P switch support

  Revision 1.38  1998/11/18 15:44:24  peter
    * VALUEPARA for tp7 compatible value parameters

  Revision 1.37  1998/11/13 10:15:50  peter
    * fixed ptr() with constants

  Revision 1.36  1998/11/10 10:09:21  peter
    * va_list -> array of const

  Revision 1.35  1998/10/19 08:55:13  pierre
    * wrong stabs info corrected once again !!
    + variable vmt offset with vmt field only if required
      implemented now !!!

  Revision 1.34  1998/10/12 09:50:06  florian
    + support of <procedure var type>:=<pointer> in delphi mode added

  Revision 1.33  1998/10/06 20:43:30  peter
    * fixed set of bugs. like set of false..true set of #1..#255 and
      set of #1..true which was allowed

  Revision 1.32  1998/10/05 21:33:35  peter
    * fixed 161,165,166,167,168

  Revision 1.31  1998/09/23 09:58:56  peter
    * first working array of const things

  Revision 1.30  1998/09/22 15:40:58  peter
    * some extra ifdef GDB

  Revision 1.29  1998/09/16 12:37:31  michael
  Added FPC_ prefix to abstracterror

  Revision 1.28  1998/09/09 16:44:23  florian
    * I hope, the case bug is fixed now

  Revision 1.27  1998/09/07 17:37:07  florian
    * first fixes for published properties

  Revision 1.26  1998/09/04 12:24:31  florian
    * bug0159 fixed

  Revision 1.25  1998/09/04 09:06:36  florian
   * bug0132 fixed

  Revision 1.24  1998/09/04 08:36:49  peter
    * fixed boolean:=integer which is not explicit

  Revision 1.23  1998/09/01 17:39:55  peter
    + internal constant functions

  Revision 1.22  1998/09/01 12:53:28  peter
    + aktpackenum

  Revision 1.21  1998/08/19 00:42:45  peter
    + subrange types for enums
    + checking for bounds type with ranges

  Revision 1.20  1998/08/18 14:17:14  pierre
    * bug about assigning the return value of a function to
      a procvar fixed : warning
      assigning a proc to a procvar need @ in FPC mode !!
    * missing file/line info restored

  Revision 1.19  1998/08/18 09:24:48  pierre
    * small warning position bug fixed
    * support_mmx switches splitting was missing
    * rhide error and warning output corrected

  Revision 1.18  1998/08/14 18:18:49  peter
    + dynamic set contruction
    * smallsets are now working (always longint size)

  Revision 1.17  1998/08/05 16:00:17  florian
    * some fixes for ansi strings

  Revision 1.16  1998/07/20 23:35:50  michael
  Const ansistrings are not copied.

  Revision 1.15  1998/07/18 22:54:32  florian
    * some ansi/wide/longstring support fixed:
       o parameter passing
       o returning as result from functions

  Revision 1.14  1998/06/12 14:50:50  peter
    * removed the tree dependency to types.pas
    * long_fil.pas support (not fully tested yet)

  Revision 1.13  1998/06/03 22:49:07  peter
    + wordbool,longbool
    * rename bis,von -> high,low
    * moved some systemunit loading/creating to psystem.pas

  Revision 1.12  1998/05/12 10:47:00  peter
    * moved printstatus to verb_def
    + V_Normal which is between V_Error and V_Warning and doesn't have a
      prefix like error: warning: and is included in V_Default
    * fixed some messages
    * first time parameter scan is only for -v and -T
    - removed old style messages

  Revision 1.11  1998/05/01 16:38:46  florian
    * handling of private and protected fixed
    + change_keywords_to_tp implemented to remove
      keywords which aren't supported by tp
    * break and continue are now symbols of the system unit
    + widestring, longstring and ansistring type released

  Revision 1.10  1998/04/29 10:34:08  pierre
    + added some code for ansistring (not complete nor working yet)
    * corrected operator overloading
    * corrected nasm output
    + started inline procedures
    + added starstarn : use ** for exponentiation (^ gave problems)
    + started UseTokenInfo cond to get accurate positions

  Revision 1.9  1998/04/21 10:16:49  peter
    * patches from strasbourg
    * objects is not used anymore in the fpc compiled version

  Revision 1.8  1998/04/12 22:39:44  florian
    * problem with read access to properties solved
    * correct handling of hidding methods via virtual (COM)
    * correct result type of constructor calls (COM), the resulttype
      depends now on the type of the class reference

  Revision 1.7  1998/04/10 21:36:56  florian
    + some stuff to support method pointers (procedure of object) added
      (declaration, parameter handling)

  Revision 1.6  1998/04/10 15:39:49  florian
    * more fixes to get classes.pas compiled

  Revision 1.5  1998/04/09 23:02:16  florian
    * small problems solved to get remake3 work

  Revision 1.4  1998/04/08 16:58:09  pierre
    * several bugfixes
      ADD ADC and AND are also sign extended
      nasm output OK (program still crashes at end
      and creates wrong assembler files !!)
      procsym types sym in tdef removed !!

  Revision 1.3  1998/04/08 11:34:22  peter
    * nasm works (linux only tested)
}
