{
    $Id$
    Copyright (C) 1998-2000 by Florian Klaempfl

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
       cobjects,symtable
       {$IFDEF NEWST}
       ,defs
       {$ENDIF NEWST};

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

    { true if p is a void}
    function is_void(def : pdef) : boolean;

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

    { true if p is a voidpointer def }
    function is_voidpointer(p : pdef) : boolean;

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
    { if acp is cp_none, all have to match exactly  }
    { if acp is cp_value_equal_const call by value  }
    { and call by const parameter are assumed as    }
    { equal                                         }
    { if acp is cp_all the var const or nothing are considered equal }
    type
      compare_type = ( cp_none, cp_value_equal_const, cp_all);

    function equal_paras(paralist1,paralist2 : plinkedlist; acp : compare_type) : boolean;


    { true if a type can be allowed for another one
      in a func var }
    function convertable_paras(paralist1,paralist2 : plinkedlist; acp : compare_type) : boolean;

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

    { returns true, if sym needs an entry in the proplist of a class rtti }
    function needs_prop_entry(sym : psym) : boolean;

    { returns true, if p contains data which needs init/final code }
    function needs_init_final(p : psymtable) : boolean;

implementation

    uses
       strings,globtype,globals,htypechk,
       tree,verbose,symconst;

    var
       b_needs_init_final : boolean;

    procedure _needs_init_final(p : pnamedindexobject);{$ifndef FPC}far;{$endif}


      begin
         if (psym(p)^.typ=varsym) and
           assigned(pvarsym(p)^.vartype.def) and
           not((pvarsym(p)^.vartype.def^.deftype=objectdef) and
           pobjectdef(pvarsym(p)^.vartype.def)^.is_class) and
           pvarsym(p)^.vartype.def^.needs_inittable then
           b_needs_init_final:=true;
      end;

    { returns true, if p contains data which needs init/final code }
    function needs_init_final(p : psymtable) : boolean;

      begin
         b_needs_init_final:=false;
         p^.foreach({$ifndef TP}@{$endif}_needs_init_final);
         needs_init_final:=b_needs_init_final;
      end;

    function needs_prop_entry(sym : psym) : boolean;

      begin
         needs_prop_entry:=(sp_published in psym(sym)^.symoptions) and
         (sym^.typ in [propertysym,varsym]);
      end;

    {  compare_type = ( cp_none, cp_value_equal_const, cp_all); }

    function equal_paras(paralist1,paralist2 : plinkedlist; acp : compare_type) : boolean;
      var
        def1,def2 : pparaitem;
      begin
         def1:=pparaitem(paralist1^.first);
         def2:=pparaitem(paralist2^.first);
         while (assigned(def1)) and (assigned(def2)) do
           begin
             case acp of
              cp_value_equal_const :
                begin
                   if not(is_equal(def1^.paratype.def,def2^.paratype.def)) or
                     ((def1^.paratyp<>def2^.paratyp) and
                      ((def1^.paratyp=vs_var) or
                       (def1^.paratyp=vs_var)
                      )
                     ) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              cp_all :
                begin
                   if not(is_equal(def1^.paratype.def,def2^.paratype.def)) or
                     (def1^.paratyp<>def2^.paratyp) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              cp_none :
                begin
                   if not(is_equal(def1^.paratype.def,def2^.paratype.def)) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              end;
              def1:=pparaitem(def1^.next);
              def2:=pparaitem(def2^.next);
           end;
         if (def1=nil) and (def2=nil) then
           equal_paras:=true
         else
           equal_paras:=false;
      end;

    function convertable_paras(paralist1,paralist2 : plinkedlist;acp : compare_type) : boolean;
      var
        def1,def2 : pparaitem;
        doconv : tconverttype;
      begin
         def1:=pparaitem(paralist1^.first);
         def2:=pparaitem(paralist2^.first);
         while (assigned(def1)) and (assigned(def2)) do
           begin
              case acp of
              cp_value_equal_const :
                begin
                   if (isconvertable(def1^.paratype.def,def2^.paratype.def,doconv,callparan,false)=0) or
                     ((def1^.paratyp<>def2^.paratyp) and
                      ((def1^.paratyp=vs_var) or
                       (def1^.paratyp=vs_var)
                      )
                     ) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              cp_all :
                begin
                   if (isconvertable(def1^.paratype.def,def2^.paratype.def,doconv,callparan,false)=0) or
                     (def1^.paratyp<>def2^.paratyp) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              cp_none :
                begin
                   if (isconvertable(def1^.paratype.def,def2^.paratype.def,doconv,callparan,false)=0) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              end;
              def1:=pparaitem(def1^.next);
              def2:=pparaitem(def2^.next);
           end;
         if (def1=nil) and (def2=nil) then
           convertable_paras:=true
         else
           convertable_paras:=false;
      end;


    { true if a function can be assigned to a procvar }
    function proc_to_procvar_equal(def1:pprocdef;def2:pprocvardef) : boolean;
      const
        po_comp = po_compatibility_options-[po_methodpointer,po_classmethod];
      var
        ismethod : boolean;
      begin
         proc_to_procvar_equal:=false;
         if not(assigned(def1)) or not(assigned(def2)) then
           exit;
         { check for method pointer }
         ismethod:=assigned(def1^.owner) and
                   (def1^.owner^.symtabletype=objectsymtable);
                   { I think methods of objects are also not compatible }
                   { with procedure variables! (FK)
                   and
                   assigned(def1^.owner^.defowner) and
                   (pobjectdef(def1^.owner^.defowner)^.is_class); }
         if (ismethod and not (po_methodpointer in def2^.procoptions)) or
            (not(ismethod) and (po_methodpointer in def2^.procoptions)) then
          begin
            Message(type_e_no_method_and_procedure_not_compatible);
            exit;
          end;
         { check return value and para's and options, methodpointer is already checked
           parameters may also be convertable }
         if is_equal(def1^.rettype.def,def2^.rettype.def) and
            (equal_paras(def1^.para,def2^.para,cp_all) or
             convertable_paras(def1^.para,def2^.para,cp_all)) and
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


    { true if p is a void }
    function is_void(def : pdef) : boolean;
      begin
        is_void:=(def^.deftype=orddef) and
                 (porddef(def)^.typ=uvoid);
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
                        (parraydef(p)^.rangetype.def=pdef(s32bitdef)) and
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
                      is_equal(parraydef(p)^.elementtype.def,cchardef) and
                      not(is_special_array(p));
      end;


    { true if p is a pchar def }
    function is_pchar(p : pdef) : boolean;
      begin
        is_pchar:=(p^.deftype=pointerdef) and
                  is_equal(Ppointerdef(p)^.pointertype.def,cchardef);
      end;


    { true if p is a voidpointer def }
    function is_voidpointer(p : pdef) : boolean;
      begin
        is_voidpointer:=(p^.deftype=pointerdef) and
                        is_equal(Ppointerdef(p)^.pointertype.def,voiddef);
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
        push_addr_param:=false;
        if never_copy_const_param then
         push_addr_param:=true
        else
         begin
           case def^.deftype of
             formaldef :
               push_addr_param:=true;
             recorddef :
               push_addr_param:=(def^.size>4);
             arraydef :
               push_addr_param:=((Parraydef(def)^.highrange>Parraydef(def)^.lowrange) and (def^.size>4)) or
                                is_open_array(def) or
                                is_array_of_const(def) or
                                is_array_constructor(def);
             objectdef :
               push_addr_param:=not(pobjectdef(def)^.is_class);
             stringdef :
               push_addr_param:=pstringdef(def)^.string_typ in [st_shortstring,st_longstring];
             procvardef :
               push_addr_param:=(po_methodpointer in pprocvardef(def)^.procoptions);
             setdef :
               push_addr_param:=(psetdef(def)^.settype<>smallset);
           end;
         end;
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
                   { Fix the value to fit in the allocated space for this type of variable }
                     case def^.size of
                       1: l := l and $ff;
                       2: l := l and $ffff;
                     end
{                   l:=lv+(l mod (hv-lv+1));}
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
              if parraydef(p)^.elementtype.def^.deftype=floatdef then
                case pfloatdef(parraydef(p)^.elementtype.def)^.typ of
                  s32real:
                    mmx_type:=mmxsingle;
                  f16bit:
                    mmx_type:=mmxfixed16
                end
              else
                case porddef(parraydef(p)^.elementtype.def)^.typ of
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
                  (parraydef(p)^.elementtype.def^.deftype=orddef) and
                  (
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=1) and
                    (porddef(parraydef(p)^.elementtype.def)^.typ in [u32bit,s32bit])
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=3) and
                    (porddef(parraydef(p)^.elementtype.def)^.typ in [u16bit,s16bit])
                   )
                  )
                 )
                 or
                (
                 (
                  (parraydef(p)^.elementtype.def^.deftype=floatdef) and
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=3) and
                   (pfloatdef(parraydef(p)^.elementtype.def)^.typ=f16bit)
                  ) or
                  (
                   (parraydef(p)^.lowrange=0) and
                   (parraydef(p)^.highrange=1) and
                   (pfloatdef(parraydef(p)^.elementtype.def)^.typ=s32real)
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
                  (parraydef(p)^.elementtype.def^.deftype=orddef) and
                  (
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=1) and
                    (porddef(parraydef(p)^.elementtype.def)^.typ in [u32bit,s32bit])
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=3) and
                    (porddef(parraydef(p)^.elementtype.def)^.typ in [u16bit,s16bit])
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=7) and
                    (porddef(parraydef(p)^.elementtype.def)^.typ in [u8bit,s8bit])
                   )
                  )
                 )
                 or
                 (
                  (parraydef(p)^.elementtype.def^.deftype=floatdef) and
                  (
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=3) and
                    (pfloatdef(parraydef(p)^.elementtype.def)^.typ=f32bit)
                   )
                   or
                   (
                    (parraydef(p)^.lowrange=0) and
                    (parraydef(p)^.highrange=1) and
                    (pfloatdef(parraydef(p)^.elementtype.def)^.typ=s32real)
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
                if assigned(def1^.typesym) and (ppointerdef(def1)^.pointertype.def^.deftype=forwarddef) then
                  b:=(def1^.typesym=def2^.typesym)
                else
                  b:=ppointerdef(def1)^.pointertype.def=ppointerdef(def2)^.pointertype.def;
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
              b:=(pfiledef(def1)^.filetyp=pfiledef(def2)^.filetyp) and
                 ((
                 ((pfiledef(def1)^.typedfiletype.def=nil) and
                  (pfiledef(def2)^.typedfiletype.def=nil)) or
                 (
                  (pfiledef(def1)^.typedfiletype.def<>nil) and
                  (pfiledef(def2)^.typedfiletype.def<>nil) and
                  is_equal(pfiledef(def1)^.typedfiletype.def,pfiledef(def2)^.typedfiletype.def)
                 ) or
                 ( (pfiledef(def1)^.typedfiletype.def=pdef(voiddef)) or
                   (pfiledef(def2)^.typedfiletype.def=pdef(voiddef))
                 )))
         { sets with the same element type are equal }
         else
           if (def1^.deftype=setdef) and (def2^.deftype=setdef) then
             begin
                if assigned(psetdef(def1)^.elementtype.def) and
                   assigned(psetdef(def2)^.elementtype.def) then
                  b:=(psetdef(def1)^.elementtype.def^.deftype=psetdef(def2)^.elementtype.def^.deftype)
                else
                  b:=true;
             end
         else
           if (def1^.deftype=procvardef) and (def2^.deftype=procvardef) then
             begin
                { poassembler isn't important for compatibility }
                { if a method is assigned to a methodpointer    }
                { is checked before                             }
                b:=(pprocvardef(def1)^.proctypeoption=pprocvardef(def2)^.proctypeoption) and
                   (pprocvardef(def1)^.proccalloptions=pprocvardef(def2)^.proccalloptions) and
                   ((pprocvardef(def1)^.procoptions * po_compatibility_options)=
                    (pprocvardef(def2)^.procoptions * po_compatibility_options)) and
                   is_equal(pprocvardef(def1)^.rettype.def,pprocvardef(def2)^.rettype.def) and
                   equal_paras(pprocvardef(def1)^.para,pprocvardef(def2)^.para,cp_all);
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
                   b:=is_equal(parraydef(def1)^.elementtype.def,parraydef(def2)^.elementtype.def);
                end
               else
                begin
                  b:=not(m_tp in aktmodeswitches) and
                     not(m_delphi in aktmodeswitches) and
                     (parraydef(def1)^.lowrange=parraydef(def2)^.lowrange) and
                     (parraydef(def1)^.highrange=parraydef(def2)^.highrange) and
                     is_equal(parraydef(def1)^.elementtype.def,parraydef(def2)^.elementtype.def) and
                     is_equal(parraydef(def1)^.rangetype.def,parraydef(def2)^.rangetype.def);
                end;
             end
         else
           if (def1^.deftype=classrefdef) and (def2^.deftype=classrefdef) then
             begin
                { similar to pointerdef: }
                if assigned(def1^.typesym) and (pclassrefdef(def1)^.pointertype.def^.deftype=forwarddef) then
                  b:=(def1^.typesym=def2^.typesym)
                else
                  b:=is_equal(pclassrefdef(def1)^.pointertype.def,pclassrefdef(def2)^.pointertype.def);
             end;
         is_equal:=b;
      end;


    function is_subequal(def1, def2: pdef): boolean;

      var
         basedef1,basedef2 : penumdef;

      Begin
        is_subequal := false;
        if assigned(def1) and assigned(def2) then
        Begin
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

              { I changed this to assume that the enums are equal }
              { if the basedefs are equal (FK)                    }
              if (def1^.deftype=enumdef) and (def2^.deftype=enumdef) then
                Begin
                   { get both basedefs }
                   basedef1:=penumdef(def1);
                   while assigned(basedef1^.basedef) do
                     basedef1:=basedef1^.basedef;
                   basedef2:=penumdef(def2);
                   while assigned(basedef2^.basedef) do
                     basedef2:=basedef2^.basedef;
                   is_subequal:=basedef1=basedef2;
                   {
                   if penumdef(def1)^.firstenum = penumdef(def2)^.firstenum then
                      is_subequal := TRUE;
                   }
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
  Revision 1.2  2000-07-13 11:32:53  michael
  + removed logs

}
