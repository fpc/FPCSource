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
unit defbase;

{$i fpcdefs.inc}

interface

    uses
       cclasses,
       cpuinfo,
       globals,
       node,
       symconst,symbase,symtype,symdef,symsym;

    type
       tmmxtype = (mmxno,mmxu8bit,mmxs8bit,mmxu16bit,mmxs16bit,
                   mmxu32bit,mmxs32bit,mmxfixed16,mmxsingle);

    const
       {# true if we must never copy this parameter }
       never_copy_const_param : boolean = false;

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

    {# Returns true, if def1 and def2 are semantically the same }
    function is_equal(def1,def2 : tdef) : boolean;

    {# Checks for type compatibility (subgroups of type)
       used for case statements... probably missing stuff
       to use on other types
    }
    function is_subequal(def1, def2: tdef): boolean;

     type
       tconverttype = (
          tc_equal,
          tc_not_possible,
          tc_string_2_string,
          tc_char_2_string,
          tc_char_2_chararray,
          tc_pchar_2_string,
          tc_cchar_2_pchar,
          tc_cstring_2_pchar,
          tc_ansistring_2_pchar,
          tc_string_2_chararray,
          tc_chararray_2_string,
          tc_array_2_pointer,
          tc_pointer_2_array,
          tc_int_2_int,
          tc_int_2_bool,
          tc_bool_2_bool,
          tc_bool_2_int,
          tc_real_2_real,
          tc_int_2_real,
          tc_proc_2_procvar,
          tc_arrayconstructor_2_set,
          tc_load_smallset,
          tc_cord_2_pointer,
          tc_intf_2_string,
          tc_intf_2_guid,
          tc_class_2_intf,
          tc_char_2_char,
          tc_normal_2_smallset,
          tc_dynarray_2_openarray
       );

    function assignment_overloaded(from_def,to_def : tdef) : tprocdef;

    { Returns:
       0 - Not convertable
       1 - Convertable
       2 - Convertable, but not first choice }
    function isconvertable(def_from,def_to : tdef;
             var doconv : tconverttype;
             fromtreetype : tnodetype;
             explicit : boolean) : byte;

    { Same as is_equal, but with error message if failed }
    function CheckTypes(def1,def2 : tdef) : boolean;

    function equal_constsym(sym1,sym2:tconstsym):boolean;

    { if acp is cp_all the var const or nothing are considered equal }
    type
      compare_type = ( cp_none, cp_value_equal_const, cp_all);

    {# true, if two parameter lists are equal
      if acp is cp_none, all have to match exactly
      if acp is cp_value_equal_const call by value
      and call by const parameter are assumed as
      equal
    }
    function equal_paras(paralist1,paralist2 : tlinkedlist; acp : compare_type) : boolean;


    { True if a type can be allowed for another one
      in a func var }
    function convertable_paras(paralist1,paralist2 : tlinkedlist; acp : compare_type) : boolean;

    { True if a function can be assigned to a procvar }
    { changed first argument type to pabstractprocdef so that it can also be }
    { used to test compatibility between two pprocvardefs (JM)               }
    function proc_to_procvar_equal(def1:tabstractprocdef;def2:tprocvardef;exact:boolean) : boolean;

{    function get_proc_2_procvar_def(p:tprocsym;d:tprocvardef):tprocdef;}

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

    {# returns true, if sym needs an entry in the proplist of a class rtti }
    function needs_prop_entry(sym : tsym) : boolean;


implementation

    uses
       globtype,tokens,verbose,
       symtable;


    function needs_prop_entry(sym : tsym) : boolean;

      begin
         needs_prop_entry:=(sp_published in tsym(sym).symoptions) and
         (sym.typ in [propertysym,varsym]);
      end;


    function equal_constsym(sym1,sym2:tconstsym):boolean;
      var
        p1,p2,pend : pchar;
      begin
        equal_constsym:=false;
        if sym1.consttyp<>sym2.consttyp then
         exit;
        case sym1.consttyp of
           constint,
           constbool,
           constchar,
           constord :
             equal_constsym:=(sym1.valueord=sym2.valueord);
           constpointer :
             equal_constsym:=(sym1.valueordptr=sym2.valueordptr);
           conststring,constresourcestring :
             begin
               if sym1.len=sym2.len then
                begin
                  p1:=pchar(sym1.valueptr);
                  p2:=pchar(sym2.valueptr);
                  pend:=p1+sym1.len;
                  while (p1<pend) do
                   begin
                     if p1^<>p2^ then
                      break;
                     inc(p1);
                     inc(p2);
                   end;
                  if (p1=pend) then
                   equal_constsym:=true;
                end;
             end;
           constreal :
             equal_constsym:=(pbestreal(sym1.valueptr)^=pbestreal(sym2.valueptr)^);
           constset :
             equal_constsym:=(pnormalset(sym1.valueptr)^=pnormalset(sym2.valueptr)^);
           constnil :
             equal_constsym:=true;
        end;
      end;


    {  compare_type = ( cp_none, cp_value_equal_const, cp_all); }

    function equal_paras(paralist1,paralist2 : TLinkedList; acp : compare_type) : boolean;
      var
        def1,def2 : TParaItem;
      begin
         def1:=TParaItem(paralist1.first);
         def2:=TParaItem(paralist2.first);
         while (assigned(def1)) and (assigned(def2)) do
           begin
             case acp of
              cp_value_equal_const :
                begin
                   if not(is_equal(def1.paratype.def,def2.paratype.def)) or
                     ((def1.paratyp<>def2.paratyp) and
                      ((def1.paratyp in [vs_var,vs_out]) or
                       (def2.paratyp in [vs_var,vs_out])
                      )
                     ) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              cp_all :
                begin
                   if not(is_equal(def1.paratype.def,def2.paratype.def)) or
                     (def1.paratyp<>def2.paratyp) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                end;
              cp_none :
                begin
                   if not(is_equal(def1.paratype.def,def2.paratype.def)) then
                     begin
                        equal_paras:=false;
                        exit;
                     end;
                   { also check default value if both have it declared }
                   if assigned(def1.defaultvalue) and
                      assigned(def2.defaultvalue) then
                    begin
                      if not equal_constsym(tconstsym(def1.defaultvalue),tconstsym(def2.defaultvalue)) then
                       begin
                         equal_paras:=false;
                         exit;
                       end;
                    end;
                end;
              end;
              def1:=TParaItem(def1.next);
              def2:=TParaItem(def2.next);
           end;
         if (def1=nil) and (def2=nil) then
           equal_paras:=true
         else
           equal_paras:=false;
      end;

    function convertable_paras(paralist1,paralist2 : TLinkedList;acp : compare_type) : boolean;
      var
        def1,def2 : TParaItem;
        doconv : tconverttype;
      begin
         def1:=TParaItem(paralist1.first);
         def2:=TParaItem(paralist2.first);
         while (assigned(def1)) and (assigned(def2)) do
           begin
              case acp of
              cp_value_equal_const :
                begin
                   if (isconvertable(def1.paratype.def,def2.paratype.def,doconv,callparan,false)=0) or
                     ((def1.paratyp<>def2.paratyp) and
                      ((def1.paratyp in [vs_out,vs_var]) or
                       (def2.paratyp in [vs_out,vs_var])
                      )
                     ) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              cp_all :
                begin
                   if (isconvertable(def1.paratype.def,def2.paratype.def,doconv,callparan,false)=0) or
                     (def1.paratyp<>def2.paratyp) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              cp_none :
                begin
                   if (isconvertable(def1.paratype.def,def2.paratype.def,doconv,callparan,false)=0) then
                     begin
                        convertable_paras:=false;
                        exit;
                     end;
                end;
              end;
              def1:=TParaItem(def1.next);
              def2:=TParaItem(def2.next);
           end;
         if (def1=nil) and (def2=nil) then
           convertable_paras:=true
         else
           convertable_paras:=false;
      end;


    { true if a function can be assigned to a procvar }
    { changed first argument type to pabstractprocdef so that it can also be }
    { used to test compatibility between two pprocvardefs (JM)               }
    function proc_to_procvar_equal(def1:tabstractprocdef;def2:tprocvardef;exact:boolean) : boolean;
      const
        po_comp = po_compatibility_options-[po_methodpointer,po_classmethod];
      var
        ismethod : boolean;
      begin
         proc_to_procvar_equal:=false;
         if not(assigned(def1)) or not(assigned(def2)) then
           exit;
         { check for method pointer }
         if def1.deftype=procvardef then
          begin
            ismethod:=(po_methodpointer in def1.procoptions);
          end
         else
          begin
            ismethod:=assigned(def1.owner) and
                      (def1.owner.symtabletype=objectsymtable);
          end;
         if (ismethod and not (po_methodpointer in def2.procoptions)) or
            (not(ismethod) and (po_methodpointer in def2.procoptions)) then
          begin
            Message(type_e_no_method_and_procedure_not_compatible);
            exit;
          end;
         { check return value and para's and options, methodpointer is already checked
           parameters may also be convertable }
         if is_equal(def1.rettype.def,def2.rettype.def) and
            (equal_paras(def1.para,def2.para,cp_all) or
             ((not exact) and convertable_paras(def1.para,def2.para,cp_all))) and
            ((po_comp * def1.procoptions)= (po_comp * def2.procoptions)) then
           proc_to_procvar_equal:=true
         else
           proc_to_procvar_equal:=false;
      end;

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
         is_in_limit:=((not(fromqword xor toqword) and
                        (torddef(def_from).low>=torddef(def_to).low) and
                        (torddef(def_from).high<=torddef(def_to).high)) or
                       (toqword and not is_signed(def_from)));
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
                      is_equal(tarraydef(p).elementtype.def,cchartype.def) and
                      not(is_special_array(p));
      end;

    { true if p is a widechar array def }
    function is_widechararray(p : tdef) : boolean;
      begin
        is_widechararray:=(p.deftype=arraydef) and
                      is_equal(tarraydef(p).elementtype.def,cwidechartype.def) and
                      not(is_special_array(p));
      end;


    { true if p is a pchar def }
    function is_pchar(p : tdef) : boolean;
      begin
        is_pchar:=(p.deftype=pointerdef) and
                  (is_equal(tpointerdef(p).pointertype.def,cchartype.def) or
                   (is_zero_based_array(tpointerdef(p).pointertype.def) and
                    is_chararray(tpointerdef(p).pointertype.def)));
      end;

    { true if p is a pchar def }
    function is_pwidechar(p : tdef) : boolean;
      begin
        is_pwidechar:=(p.deftype=pointerdef) and
                  (is_equal(tpointerdef(p).pointertype.def,cwidechartype.def) or
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


    function is_equal(def1,def2 : tdef) : boolean;
      var
         b : boolean;
         hd : tdef;
      begin
         { both types must exists }
         if not (assigned(def1) and assigned(def2)) then
          begin
            is_equal:=false;
            exit;
          end;

         { be sure, that if there is a stringdef, that this is def1 }
         if def2.deftype=stringdef then
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
           if (def1.deftype=pointerdef) and (def2.deftype=pointerdef) then
             begin
                { check if both are farpointer }
                if (tpointerdef(def1).is_far=tpointerdef(def2).is_far) then
                 begin
                   { here a problem detected in tabsolutesym }
                   { the types can be forward type !!        }
                   if assigned(def1.typesym) and (tpointerdef(def1).pointertype.def.deftype=forwarddef) then
                    b:=(def1.typesym=def2.typesym)
                   else
                    b:=tpointerdef(def1).pointertype.def=tpointerdef(def2).pointertype.def;
                 end
                else
                 b:=false;
             end
         else
         { ordinals are equal only when the ordinal type is equal }
           if (def1.deftype=orddef) and (def2.deftype=orddef) then
             begin
                case torddef(def1).typ of
                u8bit,u16bit,u32bit,
                s8bit,s16bit,s32bit:
                  b:=((torddef(def1).typ=torddef(def2).typ) and
                   (torddef(def1).low=torddef(def2).low) and
                   (torddef(def1).high=torddef(def2).high));
                uvoid,uchar,uwidechar,
                bool8bit,bool16bit,bool32bit:
                  b:=(torddef(def1).typ=torddef(def2).typ);
                end;
             end
         else
           if (def1.deftype=floatdef) and (def2.deftype=floatdef) then
             b:=tfloatdef(def1).typ=tfloatdef(def2).typ
         else
           { strings with the same length are equal }
           if (def1.deftype=stringdef) and (def2.deftype=stringdef) and
              (tstringdef(def1).string_typ=tstringdef(def2).string_typ) then
             begin
                b:=not(is_shortstring(def1)) or
                   (tstringdef(def1).len=tstringdef(def2).len);
             end
         else
           if (def1.deftype=formaldef) and (def2.deftype=formaldef) then
             b:=true
         { file types with the same file element type are equal }
         { this is a problem for assign !!                      }
         { changed to allow if one is untyped                   }
         { all typed files are equal to the special             }
         { typed file that has voiddef as elemnt type           }
         { but must NOT match for text file !!!                 }
         else
            if (def1.deftype=filedef) and (def2.deftype=filedef) then
              b:=(tfiledef(def1).filetyp=tfiledef(def2).filetyp) and
                 ((
                 ((tfiledef(def1).typedfiletype.def=nil) and
                  (tfiledef(def2).typedfiletype.def=nil)) or
                 (
                  (tfiledef(def1).typedfiletype.def<>nil) and
                  (tfiledef(def2).typedfiletype.def<>nil) and
                  is_equal(tfiledef(def1).typedfiletype.def,tfiledef(def2).typedfiletype.def)
                 ) or
                 ( (tfiledef(def1).typedfiletype.def=tdef(voidtype.def)) or
                   (tfiledef(def2).typedfiletype.def=tdef(voidtype.def))
                 )))
         { sets with the same element base type are equal }
         else
           if (def1.deftype=setdef) and (def2.deftype=setdef) then
             begin
                if assigned(tsetdef(def1).elementtype.def) and
                   assigned(tsetdef(def2).elementtype.def) then
                  b:=is_subequal(tsetdef(def1).elementtype.def,tsetdef(def2).elementtype.def)
                else
                  { empty set is compatible with everything }
                  b:=true;
             end
         else
           if (def1.deftype=procvardef) and (def2.deftype=procvardef) then
             begin
                { poassembler isn't important for compatibility }
                { if a method is assigned to a methodpointer    }
                { is checked before                             }
                b:=(tprocvardef(def1).proctypeoption=tprocvardef(def2).proctypeoption) and
                   (tprocvardef(def1).proccalloption=tprocvardef(def2).proccalloption) and
                   ((tprocvardef(def1).procoptions * po_compatibility_options)=
                    (tprocvardef(def2).procoptions * po_compatibility_options)) and
                   is_equal(tprocvardef(def1).rettype.def,tprocvardef(def2).rettype.def) and
                   equal_paras(tprocvardef(def1).para,tprocvardef(def2).para,cp_all);
             end
         else
           if (def1.deftype=arraydef) and (def2.deftype=arraydef) then
             begin
               if is_dynamic_array(def1) and is_dynamic_array(def2) then
                 b:=is_equal(tarraydef(def1).elementtype.def,tarraydef(def2).elementtype.def)
               else
                if is_array_of_const(def1) or is_array_of_const(def2) then
                 begin
                  b:=(is_array_of_const(def1) and is_array_of_const(def2)) or
                     (is_array_of_const(def1) and is_array_constructor(def2)) or
                     (is_array_of_const(def2) and is_array_constructor(def1));
                 end
               else
                if (is_dynamic_array(def1) or is_dynamic_array(def2)) then
                  begin
                    b := is_dynamic_array(def1) and is_dynamic_array(def2) and
                         is_equal(tarraydef(def1).elementtype.def,tarraydef(def2).elementtype.def);
                  end
               else
                if is_open_array(def1) or is_open_array(def2) then
                 begin
                   b:=is_equal(tarraydef(def1).elementtype.def,tarraydef(def2).elementtype.def);
                 end
               else
                begin
                  b:=not(m_tp7 in aktmodeswitches) and
                     not(m_delphi in aktmodeswitches) and
                     (tarraydef(def1).lowrange=tarraydef(def2).lowrange) and
                     (tarraydef(def1).highrange=tarraydef(def2).highrange) and
                     is_equal(tarraydef(def1).elementtype.def,tarraydef(def2).elementtype.def) and
                     is_equal(tarraydef(def1).rangetype.def,tarraydef(def2).rangetype.def);
                end;
             end
         else
           if (def1.deftype=classrefdef) and (def2.deftype=classrefdef) then
             begin
                { similar to pointerdef: }
                if assigned(def1.typesym) and (tclassrefdef(def1).pointertype.def.deftype=forwarddef) then
                  b:=(def1.typesym=def2.typesym)
                else
                  b:=is_equal(tclassrefdef(def1).pointertype.def,tclassrefdef(def2).pointertype.def);
             end;
         is_equal:=b;
      end;


    function is_subequal(def1, def2: tdef): boolean;

      var
         basedef1,basedef2 : tenumdef;

      Begin
        is_subequal := false;
        if assigned(def1) and assigned(def2) then
        Begin
          if (def1.deftype = orddef) and (def2.deftype = orddef) then
            Begin
              { see p.47 of Turbo Pascal 7.01 manual for the separation of types }
              { range checking for case statements is done with testrange        }
              case torddef(def1).typ of
                u8bit,u16bit,u32bit,
                s8bit,s16bit,s32bit,s64bit,u64bit :
                  is_subequal:=(torddef(def2).typ in [s64bit,u64bit,s32bit,u32bit,u8bit,s8bit,s16bit,u16bit]);
                bool8bit,bool16bit,bool32bit :
                  is_subequal:=(torddef(def2).typ in [bool8bit,bool16bit,bool32bit]);
                uchar :
                  is_subequal:=(torddef(def2).typ=uchar);
                uwidechar :
                  is_subequal:=(torddef(def2).typ=uwidechar);
              end;
            end
          else
            Begin
              { I assume that both enumerations are equal when the first }
              { pointers are equal.                                      }

              { I changed this to assume that the enums are equal }
              { if the basedefs are equal (FK)                    }
              if (def1.deftype=enumdef) and (def2.deftype=enumdef) then
                Begin
                   { get both basedefs }
                   basedef1:=tenumdef(def1);
                   while assigned(basedef1.basedef) do
                     basedef1:=basedef1.basedef;
                   basedef2:=tenumdef(def2);
                   while assigned(basedef2.basedef) do
                     basedef2:=basedef2.basedef;
                   is_subequal:=basedef1=basedef2;
                   {
                   if tenumdef(def1).firstenum = tenumdef(def2).firstenum then
                      is_subequal := TRUE;
                   }
                end;
            end;
        end; { endif assigned ... }
      end;

(*    function assignment_overloaded(from_def,to_def : tdef) : tprocdef;
       var
          passprocs : pprocdeflist;
          convtyp : tconverttype;
       begin
          assignment_overloaded:=nil;
          if not assigned(overloaded_operators[_ASSIGNMENT]) then
            exit;

          { look for an exact match first }
          passprocs:=overloaded_operators[_ASSIGNMENT].defs;
          while assigned(passprocs) do
            begin
              if is_equal(passprocs^.def.rettype.def,to_def) and
                (TParaItem(passprocs^.def.Para.first).paratype.def=from_def) then
                begin
                   assignment_overloaded:=passprocs^.def;
                   exit;
                end;
              passprocs:=passprocs^.next;
            end;

          { .... then look for an equal match }
          passprocs:=overloaded_operators[_ASSIGNMENT].defs;
          while assigned(passprocs) do
            begin
              if is_equal(passprocs^.def.rettype.def,to_def) and
                 is_equal(TParaItem(passprocs^.def.Para.first).paratype.def,from_def) then
                begin
                   assignment_overloaded:=passprocs^.def;
                   exit;
                end;
              passprocs:=passprocs^.next;
            end;

          {  .... then for convert level 1 }
          passprocs:=overloaded_operators[_ASSIGNMENT].defs;
          while assigned(passprocs) do
            begin
              if is_equal(passprocs^.def.rettype.def,to_def) and
                 (isconvertable(from_def,TParaItem(passprocs^.def.Para.first).paratype.def,convtyp,ordconstn,false)=1) then
                begin
                   assignment_overloaded:=passprocs^.def;
                   exit;
                end;
              passprocs:=passprocs^.next;
            end;
       end;
*)

    function assignment_overloaded(from_def,to_def : tdef) : tprocdef;

       begin
          assignment_overloaded:=nil;
          if not assigned(overloaded_operators[_ASSIGNMENT]) then
            exit;

          { look for an exact match first }
      assignment_overloaded:=overloaded_operators[_ASSIGNMENT].
       search_procdef_byretdef_by1paradef(to_def,from_def,dm_exact);
      if assigned(assignment_overloaded) then
        exit;

          { .... then look for an equal match }
      assignment_overloaded:=overloaded_operators[_ASSIGNMENT].
       search_procdef_byretdef_by1paradef(to_def,from_def,dm_equal);
      if assigned(assignment_overloaded) then
        exit;

          {  .... then for convert level 1 }
      assignment_overloaded:=overloaded_operators[_ASSIGNMENT].
       search_procdef_byretdef_by1paradef(to_def,from_def,dm_convertl1);
       end;


    { Returns:
       0 - Not convertable
       1 - Convertable
       2 - Convertable, but not first choice }
    function isconvertable(def_from,def_to : tdef;
             var doconv : tconverttype;
             fromtreetype : tnodetype;
             explicit : boolean) : byte;

      { Tbasetype:
           uvoid,
           u8bit,u16bit,u32bit,u64bit,
           s8bit,s16bit,s32bit,s64bit,
           bool8bit,bool16bit,bool32bit,
           uchar,uwidechar }

      type
        tbasedef=(bvoid,bchar,bint,bbool);
      const
        basedeftbl:array[tbasetype] of tbasedef =
          (bvoid,
           bint,bint,bint,bint,
           bint,bint,bint,bint,
           bbool,bbool,bbool,
           bchar,bchar);

        basedefconverts : array[tbasedef,tbasedef] of tconverttype =
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_char_2_char,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_not_possible,tc_bool_2_int,tc_bool_2_bool));

      var
         b : byte;
         hd1,hd2 : tdef;
         hct : tconverttype;
         hd3 : tobjectdef;
      begin
       { safety check }
         if not(assigned(def_from) and assigned(def_to)) then
          begin
            isconvertable:=0;
            exit;
          end;

       { tp7 procvar def support, in tp7 a procvar is always called, if the
         procvar is passed explicit a addrn would be there }
         if (m_tp_procvar in aktmodeswitches) and
            (def_from.deftype=procvardef) and
            (fromtreetype=loadn) and
            { only if the procvar doesn't require any paramters }
            (tprocvardef(def_from).minparacount = 0) then
          begin
            def_from:=tprocvardef(def_from).rettype.def;
          end;

       { we walk the wanted (def_to) types and check then the def_from
         types if there is a conversion possible }
         b:=0;
         case def_to.deftype of
           orddef :
             begin
               case def_from.deftype of
                 orddef :
                   begin
                     doconv:=basedefconverts[basedeftbl[torddef(def_from).typ],basedeftbl[torddef(def_to).typ]];
                     b:=1;
                     if (doconv=tc_not_possible) or
                        ((doconv=tc_int_2_bool) and
                         (not explicit) and
                         (not is_boolean(def_from))) or
                        ((doconv=tc_bool_2_int) and
                         (not explicit) and
                         (not is_boolean(def_to))) then
                       b:=0
                     else
                       { "punish" bad type conversions :) (JM) }
                       if not is_in_limit(def_from,def_to) and
                          (def_from.size > def_to.size) then
                         b := 2;
                   end;
                 enumdef :
                   begin
                     { needed for char(enum) }
                     if explicit then
                      begin
                        doconv:=tc_int_2_int;
                        b:=1;
                      end;
                   end;
               end;
             end;

          stringdef :
             begin
               case def_from.deftype of
                 stringdef :
                   begin
                     doconv:=tc_string_2_string;
                     b:=1;
                   end;
                 orddef :
                   begin
                   { char to string}
                     if is_char(def_from) or
                        is_widechar(def_from) then
                      begin
                        doconv:=tc_char_2_string;
                        b:=1;
                      end;
                   end;
                 arraydef :
                   begin
                   { array of char to string, the length check is done by the firstpass of this node }
                     if is_chararray(def_from) or
                        (is_equal(tarraydef(def_from).elementtype.def,cchartype.def) and
                         is_open_array(def_from)) then
                      begin
                        doconv:=tc_chararray_2_string;
                        if is_open_array(def_from) or
                           (is_shortstring(def_to) and
                            (def_from.size <= 255)) or
                           (is_ansistring(def_to) and
                            (def_from.size > 255)) then
                         b:=1
                        else
                         b:=2;
                      end;
                   end;
                 pointerdef :
                   begin
                   { pchar can be assigned to short/ansistrings,
                     but not in tp7 compatible mode }
                     if is_pchar(def_from) and not(m_tp7 in aktmodeswitches) then
                      begin
                        doconv:=tc_pchar_2_string;
                        { trefer ansistrings because pchars can overflow shortstrings, }
                        { but only if ansistrings are the default (JM)                 }
                        if (is_shortstring(def_to) and
                            not(cs_ansistrings in aktlocalswitches)) or
                           (is_ansistring(def_to) and
                            (cs_ansistrings in aktlocalswitches)) then
                          b:=1
                        else
                          b:=2;
                      end;
                   end;
               end;
             end;

           floatdef :
             begin
               case def_from.deftype of
                 orddef :
                   begin { ordinal to real }
                     if is_integer(def_from) then
                       begin
                         doconv:=tc_int_2_real;
                         b:=1;
                       end;
                   end;
                 floatdef :
                   begin { 2 float types ? }
                     if tfloatdef(def_from).typ=tfloatdef(def_to).typ then
                       doconv:=tc_equal
                     else
                       doconv:=tc_real_2_real;
                     b:=1;
                   end;
               end;
             end;

           enumdef :
             begin
               if (def_from.deftype=enumdef) then
                begin
                  if explicit then
                   begin
                     b:=1;
                     doconv:=tc_int_2_int;
                   end
                  else
                   begin
                     hd1:=def_from;
                     while assigned(tenumdef(hd1).basedef) do
                      hd1:=tenumdef(hd1).basedef;
                     hd2:=def_to;
                     while assigned(tenumdef(hd2).basedef) do
                      hd2:=tenumdef(hd2).basedef;
                     if (hd1=hd2) then
                      begin
                        b:=1;
                        { because of packenum they can have different sizes! (JM) }
                        doconv:=tc_int_2_int;
                      end;
                   end;
                end;
             end;

           arraydef :
             begin
             { open array is also compatible with a single element of its base type }
               if is_open_array(def_to) and
                  is_equal(tarraydef(def_to).elementtype.def,def_from) then
                begin
                  doconv:=tc_equal;
                  b:=1;
                end
               else if is_dynamic_array(def_to) and
                { nil is compatible with dyn. arrays }
                (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end
               else
                begin
                  case def_from.deftype of
                    arraydef :
                      begin
                        { array constructor -> open array }
                        if is_open_array(def_to) and
                           is_array_constructor(def_from) then
                         begin
                           if is_void(tarraydef(def_from).elementtype.def) or
                              is_equal(tarraydef(def_to).elementtype.def,tarraydef(def_from).elementtype.def) then
                            begin
                              doconv:=tc_equal;
                              b:=1;
                            end
                           else
                            if isconvertable(tarraydef(def_from).elementtype.def,
                                             tarraydef(def_to).elementtype.def,hct,arrayconstructorn,false)<>0 then
                             begin
                               doconv:=hct;
                               b:=2;
                             end;
                         end
                        else
                         { dynamic array -> open array }
                         if is_dynamic_array(def_from) and
                            is_open_array(def_to) and
                            is_equal(tarraydef(def_to).elementtype.def,tarraydef(def_from).elementtype.def) then
                           begin
                             doconv := tc_dynarray_2_openarray;
                             b := 2;
                           end
                        else
                        { array of tvarrec -> array of const }
                         if is_array_of_const(def_to) and
                            is_equal(tarraydef(def_to).elementtype.def,tarraydef(def_from).elementtype.def) then
                          begin
                            doconv:=tc_equal;
                            b:=1;
                          end;
                      end;
                    pointerdef :
                      begin
                        if is_zero_based_array(def_to) and
                           is_equal(tpointerdef(def_from).pointertype.def,tarraydef(def_to).elementtype.def) then
                         begin
                           doconv:=tc_pointer_2_array;
                           b:=1;
                         end;
                      end;
                    stringdef :
                      begin
                        { string to char array }
                        if (not is_special_array(def_to)) and
                           is_char(tarraydef(def_to).elementtype.def) then
                         begin
                           doconv:=tc_string_2_chararray;
                           b:=1;
                         end;
                      end;
                    orddef:
                      begin
                        if is_chararray(def_to) and
                           is_char(def_from) then
                          begin
                            doconv:=tc_char_2_chararray;
                            b:=2;
                          end;
                      end;
                    recorddef :
                      begin
                        { tvarrec -> array of constconst }
                         if is_array_of_const(def_to) and
                            is_equal(def_from,tarraydef(def_to).elementtype.def) then
                          begin
                            doconv:=tc_equal;
                            b:=1;
                          end;
                      end;
                  end;
                end;
             end;

           pointerdef :
             begin
               case def_from.deftype of
                 stringdef :
                   begin
                     { string constant (which can be part of array constructor)
                       to zero terminated string constant }
                     if (fromtreetype in [arrayconstructorn,stringconstn]) and
                        is_pchar(def_to) or is_pwidechar(def_to) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        b:=1;
                      end;
                   end;
                 orddef :
                   begin
                     { char constant to zero terminated string constant }
                     if (fromtreetype=ordconstn) then
                      begin
                        if is_equal(def_from,cchartype.def) and
                           is_pchar(def_to) then
                         begin
                           doconv:=tc_cchar_2_pchar;
                           b:=1;
                         end
                        else
                         if is_integer(def_from) then
                          begin
                            doconv:=tc_cord_2_pointer;
                            b:=1;
                          end;
                      end;
                   end;
                 arraydef :
                   begin
                     { chararray to pointer }
                     if is_zero_based_array(def_from) and
                        is_equal(tarraydef(def_from).elementtype.def,tpointerdef(def_to).pointertype.def) then
                      begin
                        doconv:=tc_array_2_pointer;
                        b:=1;
                      end;
                   end;
                 pointerdef :
                   begin
                     { child class pointer can be assigned to anchestor pointers }
                     if (
                         (tpointerdef(def_from).pointertype.def.deftype=objectdef) and
                         (tpointerdef(def_to).pointertype.def.deftype=objectdef) and
                         tobjectdef(tpointerdef(def_from).pointertype.def).is_related(
                           tobjectdef(tpointerdef(def_to).pointertype.def))
                        ) or
                        { all pointers can be assigned to void-pointer }
                        is_equal(tpointerdef(def_to).pointertype.def,voidtype.def) or
                        { in my opnion, is this not clean pascal }
                        { well, but it's handy to use, it isn't ? (FK) }
                        is_equal(tpointerdef(def_from).pointertype.def,voidtype.def) then
                       begin
                         { but don't allow conversion between farpointer-pointer }
                         if (tpointerdef(def_to).is_far=tpointerdef(def_from).is_far) then
                          begin
                            doconv:=tc_equal;
                            b:=1;
                          end;
                       end;
                   end;
                 procvardef :
                   begin
                     { procedure variable can be assigned to an void pointer }
                     { Not anymore. Use the @ operator now.}
                     if not(m_tp_procvar in aktmodeswitches) and
                        (tpointerdef(def_to).pointertype.def.deftype=orddef) and
                        (torddef(tpointerdef(def_to).pointertype.def).typ=uvoid) then
                      begin
                        doconv:=tc_equal;
                        b:=1;
                      end;
                   end;
                 classrefdef,
                 objectdef :
                   begin
                     { class types and class reference type
                       can be assigned to void pointers      }
                     if (
                         is_class_or_interface(def_from) or
                         (def_from.deftype=classrefdef)
                        ) and
                        (tpointerdef(def_to).pointertype.def.deftype=orddef) and
                        (torddef(tpointerdef(def_to).pointertype.def).typ=uvoid) then
                       begin
                         doconv:=tc_equal;
                         b:=1;
                       end;
                   end;
               end;
             end;

           setdef :
             begin
               { automatic arrayconstructor -> set conversion }
               if is_array_constructor(def_from) then
                begin
                  doconv:=tc_arrayconstructor_2_set;
                  b:=1;
                end;
             end;

           procvardef :
             begin
               { proc -> procvar }
               if (def_from.deftype=procdef) and
                  (m_tp_procvar in aktmodeswitches) then
                begin
                  doconv:=tc_proc_2_procvar;
                  if proc_to_procvar_equal(tprocdef(def_from),tprocvardef(def_to),false) then
                   b:=1;
                end
               { procvar -> procvar }
               else
                 if (def_from.deftype=procvardef) and
                    (proc_to_procvar_equal(tprocvardef(def_from),tprocvardef(def_to),false)) then
                   begin
                     doconv:=tc_equal;
                     b := 2;
                   end
               else
                { for example delphi allows the assignement from pointers }
                { to procedure variables                                  }
                if (m_pointer_2_procedure in aktmodeswitches) and
                  (def_from.deftype=pointerdef) and
                  (tpointerdef(def_from).pointertype.def.deftype=orddef) and
                  (torddef(tpointerdef(def_from).pointertype.def).typ=uvoid) then
                begin
                   doconv:=tc_equal;
                   b:=1;
                end
               else
               { nil is compatible with procvars }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           objectdef :
             begin
               { object pascal objects }
               if (def_from.deftype=objectdef) and
                 tobjectdef(def_from).is_related(tobjectdef(def_to)) then
                begin
                  doconv:=tc_equal;
                  b:=1;
                end
               else
               { Class/interface specific }
                if is_class_or_interface(def_to) then
                 begin
                   { void pointer also for delphi mode }
                   if (m_delphi in aktmodeswitches) and
                      is_voidpointer(def_from) then
                    begin
                      doconv:=tc_equal;
                      b:=1;
                    end
                   else
                   { nil is compatible with class instances and interfaces }
                    if (fromtreetype=niln) then
                     begin
                       doconv:=tc_equal;
                       b:=1;
                     end
                   { classes can be assigned to interfaces }
                   else if is_interface(def_to) and
                     is_class(def_from) and
                     assigned(tobjectdef(def_from).implementedinterfaces) then
                     begin
                        { we've to search in parent classes as well }
                        hd3:=tobjectdef(def_from);
                        while assigned(hd3) do
                          begin
                             if hd3.implementedinterfaces.searchintf(def_to)<>-1 then
                               begin
                                  doconv:=tc_class_2_intf;
                                  b:=1;
                                  break;
                               end;
                             hd3:=hd3.childof;
                          end;
                     end
                   { Interface 2 GUID handling }
                   else if (def_to=tdef(rec_tguid)) and
                           (fromtreetype=typen) and
                           is_interface(def_from) and
                           tobjectdef(def_from).isiidguidvalid then
                     begin
                       b:=1;
                       doconv:=tc_equal;
                     end;
                 end;
             end;

           classrefdef :
             begin
               { class reference types }
               if (def_from.deftype=classrefdef) then
                begin
                  doconv:=tc_equal;
                  if tobjectdef(tclassrefdef(def_from).pointertype.def).is_related(
                       tobjectdef(tclassrefdef(def_to).pointertype.def)) then
                   b:=1;
                end
               else
                { nil is compatible with class references }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   b:=1;
                 end;
             end;

           filedef :
             begin
               { typed files are all equal to the abstract file type
               name TYPEDFILE in system.pp in is_equal in types.pas
               the problem is that it sholud be also compatible to FILE
               but this would leed to a problem for ASSIGN RESET and REWRITE
               when trying to find the good overloaded function !!
               so all file function are doubled in system.pp
               this is not very beautiful !!}
               if (def_from.deftype=filedef) and
                  (
                   (
                    (tfiledef(def_from).filetyp = ft_typed) and
                    (tfiledef(def_to).filetyp = ft_typed) and
                    (
                     (tfiledef(def_from).typedfiletype.def = tdef(voidtype.def)) or
                     (tfiledef(def_to).typedfiletype.def = tdef(voidtype.def))
                    )
                   ) or
                   (
                    (
                     (tfiledef(def_from).filetyp = ft_untyped) and
                     (tfiledef(def_to).filetyp = ft_typed)
                    ) or
                    (
                     (tfiledef(def_from).filetyp = ft_typed) and
                     (tfiledef(def_to).filetyp = ft_untyped)
                    )
                   )
                  ) then
                 begin
                    doconv:=tc_equal;
                    b:=1;
                 end
             end;

           recorddef :
             begin
               { interface -> guid }
               if is_interface(def_from) and
                  (def_to=rec_tguid) then
                begin
                  doconv:=tc_intf_2_guid;
                  b:=1;
                end
               else
                begin
                  { assignment overwritten ?? }
                  if assignment_overloaded(def_from,def_to)<>nil then
                    b:=2;
                end;
             end;
           else
             begin
               { assignment overwritten ?? }
               if assignment_overloaded(def_from,def_to)<>nil then
                 b:=2;
             end;
         end;
        isconvertable:=b;
      end;


    function CheckTypes(def1,def2 : tdef) : boolean;

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
                s1:=def1.typename;
                s2:=def2.typename;
                if (s1<>'<unknown type>') and (s2<>'<unknown type>') then
                  Message2(type_e_not_equal_types,def1.typename,def2.typename)
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
  Revision 1.5  2002-08-12 20:39:17  florian
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

  Revision 1.62  2002/01/06 21:50:44  peter
    * proc_to_procvar_equal fixed for procvar-procvar

  Revision 1.61  2002/01/06 12:08:16  peter
    * removed uauto from orddef, use new range_to_basetype generating
      the correct ordinal type for a range

}
