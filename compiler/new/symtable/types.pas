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
       objects,cobjects,symtable,defs;

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
    function equal_paras(paralist1,paralist2:Pcollection;value_equal_const:boolean):boolean;


    { true if a type can be allowed for another one
      in a func var }
    function convertable_paras(paralist1,paralist2:Pcollection;value_equal_const:boolean):boolean;

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

implementation

uses    strings,globtype,globals,htypechk,tree,verbose,symbols,symtablt;

function needs_prop_entry(sym : psym) : boolean;

begin
    needs_prop_entry:=(((typeof(sym^)=typeof(Tpropertysym)) and
     (sp_published in Ppropertysym(sym)^.objprop)) or
     (((typeof(sym^)=typeof(Tvarsym)) and
     (sp_published in Pvarsym(sym)^.objprop))));
end;

function equal_paras(paralist1,paralist2:Pcollection;
                   value_equal_const:boolean):boolean;

var def1,def2:Pparameter;
    i:word;

begin
    equal_paras:=true;
    if paralist1^.count=paralist2^.count then
        for i:=1 to paralist1^.count do
            begin
                if (not is_equal(Pvarsym(def1^.data)^.definition,
                                 Pvarsym(def2^.data)^.definition)) or
                 (def1^.paratyp<>def2^.paratyp) then
                    begin
                        if (not value_equal_const) or
                         ((def1^.paratyp<>vs_var) and
                          (def2^.paratyp<>vs_var)) then
                        equal_paras:=false;
                        break;
                    end;
          end
    else
        equal_paras:=false;
end;

function convertable_paras(paralist1,paralist2:Pcollection;
                           value_equal_const : boolean):boolean;

var def1,def2:Pparameter;
    doconv:Tconverttype;
    i:word;

begin
    convertable_paras:=true;
    if paralist1^.count=paralist2^.count then
        for i:=1 to paralist1^.count do
            begin
                if (isconvertable(Pvarsym(def1^.data)^.definition,
                                   Pvarsym(def2^.data)^.definition,
                                   doconv,callparan,false)=0) or
                 (def1^.paratyp<>def2^.paratyp) then
                    begin
                        if (not value_equal_const) or
                         ((def1^.paratyp<>vs_var) and
                          (def2^.paratyp<>vs_var)) then
                        convertable_paras:=false;
                        break;
                    end;
          end
    else
        convertable_paras:=false;
end;


{ true if a function can be assigned to a procvar }
function proc_to_procvar_equal(def1:pprocdef;def2:pprocvardef):boolean;

const   po_comp=po_compatibility_options-[pomethodpointer];

var ismethod:boolean;

begin
    proc_to_procvar_equal:=false;
    {!!!! This code should never be called with nil parameters. If you really
     want to check this, make it an internalerror instead of an exit!! (DM)
    if not(assigned(def1)) or not(assigned(def2)) then
        exit;}
    {Check for method pointer.}
    ismethod:=(def1^.owner<>nil) and
     (typeof(def1^.owner^)=typeof(Tobjectsymtable));
    if (ismethod and not (pomethodpointer in def2^.options)) or
     (not(ismethod) and (pomethodpointer in def2^.options)) then
        begin
            message(type_e_no_method_and_procedure_not_compatible);
            exit;
        end;
    { check return value and para's and options, methodpointer is already checked
      parameters may also be convertable }
    proc_to_procvar_equal:=is_equal(def1^.retdef,def2^.retdef) and
     (equal_paras(def1^.parameters,def2^.parameters,false) or
      convertable_paras(def1^.parameters,def2^.parameters,false)) and
     ((po_comp*def1^.options)=(po_comp*def2^.options));
end;


{ returns true, if def uses FPU }
function is_fpu(def : pdef) : boolean;

begin
    is_fpu:=(typeof(def^)=typeof(Tfloatdef)) and (Pfloatdef(def)^.typ<>f32bit);
end;


{ true if p is an ordinal }
function is_ordinal(def : pdef) : boolean;

var dt : tbasetype;
begin
    if typeof(def^)=typeof(Torddef) then
        begin
            dt:=porddef(def)^.typ;
            is_ordinal:=dt in [uchar,
                               u8bit,u16bit,u32bit,u64bit,
                               s8bit,s16bit,s32bit,s64bit,
                               bool8bit,bool16bit,bool32bit];
        end
    else
        is_ordinal:=typeof(def^)=typeof(Tenumdef);
end;


{ returns the min. value of the type }
function get_min_value(def:pdef) : longint;

begin
    if typeof(def^)=typeof(Torddef) then
        get_min_value:=porddef(def)^.low.values
    else if typeof(def^)=typeof(Tenumdef) then
        get_min_value:=penumdef(def)^.minval
    else
        internalerror($00022701);
end;


{ true if p is an integer }
function is_integer(def : pdef) : boolean;

begin
    is_integer:=(typeof(Tdef)=typeof(Torddef)) and
                (Porddef(def)^.typ in [uauto,u8bit,u16bit,u32bit,u64bit,
                                       s8bit,s16bit,s32bit,s64bit]);
end;


{ true if p is a boolean }
function is_boolean(def : pdef) : boolean;
begin
  is_boolean:=(typeof(def^)=typeof(Torddef)) and
              (porddef(def)^.typ in [bool8bit,bool16bit,bool32bit]);
end;


{ true if p is a void }
function is_void(def : pdef) : boolean;
begin
  is_void:=(typeof(def^)=typeof(Torddef)) and
           (porddef(def)^.typ=uvoid);
end;


{ true if p is a char }
function is_char(def : pdef):boolean;
begin
  is_char:=(typeof(def^)=typeof(Torddef)) and
           (porddef(def)^.typ=uchar);
end;


{ true if p is signed (integer) }
function is_signed(def : pdef) : boolean;

var dt:Tbasetype;

begin
    if typeof(def^)=typeof(Torddef) then
        begin
            dt:=porddef(def)^.typ;
            is_signed:=(dt in [s8bit,s16bit,s32bit,s64bit]);
        end
    else
        is_signed:=false;
end;


{ true, if p points to an open string def }

function is_open_string(p:Pdef):boolean;

begin
   is_open_string:=(typeof(p^)=typeof(Tstringdef)) and
                   (pstringdef(p)^.string_typ=st_shortstring) and
                   (pstringdef(p)^.len=0);
end;


{ true, if p points to a zero based array def }
function is_zero_based_array(p : pdef) : boolean;
begin
   is_zero_based_array:=(typeof(p^)=typeof(Tarraydef)) and
                        (parraydef(p)^.lowrange.values=0) and
                        not(is_special_array(p));
end;

{ true, if p points to an open array def }
function is_open_array(p : pdef) : boolean;
begin
   is_open_array:=(typeof(p^)=typeof(Tarraydef)) and
                  (parraydef(p)^.lowrange.values=0) and
                  (Parraydef(p)^.highrange.signed) and
                  (parraydef(p)^.highrange.values=-1) and
                  not(ap_constructor in Parraydef(p)^.options) and
                  not(ap_variant in Parraydef(p)^.options) and
                  not(ap_arrayofconst in Parraydef(p)^.options);
end;

{ true, if p points to an array of const def }
function is_array_constructor(p : pdef) : boolean;

begin
    is_array_constructor:=(typeof(p^)=typeof(Tarraydef)) and
                  (ap_constructor in Parraydef(p)^.options);
end;

{ true, if p points to a variant array }
function is_variant_array(p : pdef) : boolean;

begin
    is_variant_array:=(typeof(p^)=typeof(Tarraydef)) and
                  (ap_variant in Parraydef(p)^.options);
end;

{ true, if p points to an array of const }
function is_array_of_const(p : pdef) : boolean;
begin
    is_array_of_const:=(typeof(p^)=typeof(Tarraydef)) and
                  (ap_arrayofconst in Parraydef(p)^.options);
end;

{ true, if p points to a special array }

function is_special_array(p : pdef) : boolean;

begin
    is_special_array:=(typeof(p^)=typeof(Tarraydef)) and
                  ((ap_variant in Parraydef(p)^.options) or
                   (ap_arrayofconst in Parraydef(p)^.options) or
                   (ap_constructor in Parraydef(p)^.options) or
                   is_open_array(p)
                  );
end;

{ true if p is an ansi string def }
function is_ansistring(p : pdef) : boolean;
begin
    is_ansistring:=(typeof(p^)=typeof(Tstringdef)) and
                  (pstringdef(p)^.string_typ=st_ansistring);
end;


{ true if p is an long string def }
function is_longstring(p : pdef) : boolean;
begin
    is_longstring:=(typeof(p^)=typeof(Tstringdef)) and
                  (pstringdef(p)^.string_typ=st_longstring);
end;


{ true if p is an wide string def }
function is_widestring(p : pdef) : boolean;
begin
    is_widestring:=(typeof(p^)=typeof(Tstringdef)) and
                  (pstringdef(p)^.string_typ=st_widestring);
end;


{ true if p is an short string def }
function is_shortstring(p : pdef) : boolean;
begin
    is_shortstring:=(typeof(p^)=typeof(Tstringdef)) and
                   (pstringdef(p)^.string_typ=st_shortstring);
end;

{ true if p is a char array def }
function is_chararray(p : pdef) : boolean;
begin
    is_chararray:=(typeof(p^)=typeof(Tarraydef)) and
                is_equal(parraydef(p)^.definition,cchardef) and
                not(is_special_array(p));
end;


{ true if p is a pchar def }
function is_pchar(p : pdef) : boolean;
begin
    is_pchar:=(typeof(p^)=typeof(Tpointerdef)) and
            is_equal(Ppointerdef(p)^.definition,cchardef);
end;


{ true if p is a voidpointer def }
function is_voidpointer(p : pdef) : boolean;
begin
    is_voidpointer:=(typeof(p^)=typeof(Tpointerdef)) and
                  is_equal(Ppointerdef(p)^.definition,voiddef);
end;


{ true if p is a smallset def }
function is_smallset(p : pdef) : boolean;

begin
    is_smallset:=(typeof(p^)=typeof(Tsetdef)) and
               (psetdef(p)^.settype=smallset);
end;


{ true, if def is a 64 bit int type }
function is_64bitint(def : pdef) : boolean;

begin
    is_64bitint:=(typeof(def^)=typeof(Torddef)) and
     (porddef(def)^.typ in [u64bit,s64bit])
end;


function push_high_param(def : pdef) : boolean;

begin
    push_high_param:=is_open_array(def) or
                    is_open_string(def) or
                    is_array_of_const(def);
end;


{ true if a parameter is too large to copy and only the address is pushed }
function push_addr_param(def : pdef) : boolean;

var r:boolean;

begin
    push_addr_param:=false;
    if never_copy_const_param then
     push_addr_param:=true
    else
     begin
        if typeof(def^)=typeof(Tformaldef) then
           push_addr_param:=true
        else if typeof(def^)=typeof(Trecorddef) then
           push_addr_param:=(def^.size>4)
        else if typeof(def^)=typeof(Tarraydef) then
            begin
                r:=is_open_array(def) or is_array_of_const(def) or
                 is_array_constructor(def);
                if Parraydef(def)^.highrange.signed then
                    r:=r or ((Parraydef(def)^.highrange.values>
                     Parraydef(def)^.lowrange.values) and (def^.size>4))
                else
                    r:=r or ((Parraydef(def)^.highrange.valueu>
                     Parraydef(def)^.lowrange.valueu) and (def^.size>4));
            end
        else if typeof(def^)=typeof(Tobjectdef) then
           push_addr_param:=not (oo_is_class in Pobjectdef(def)^.options)
        else if typeof(def^)=typeof(Tstringdef) then
           push_addr_param:=pstringdef(def)^.string_typ in [st_shortstring,st_longstring]
        else if typeof(def^)=typeof(Tprocvardef) then
           push_addr_param:=(pomethodpointer in pprocvardef(def)^.options)
        else if typeof(def^)=typeof(Tsetdef) then
           push_addr_param:=(psetdef(def)^.settype<>smallset);
     end;
end;

{ test if l is in the range of def, outputs error if out of range }
procedure testrange(def : pdef;var l:longint);

var lsv,hsv:longint;
{$IFDEF TP}
    luv:longint absolute lsv;
    huv:longint absolute hsv;
{$ELSE}
    luv:cardinal absolute lsv;
    huv:cardinal absolute hsv;
{$ENDIF TP}

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
        getrange(def,lsv,hsv);
        if (typeof(def^)=typeof(Torddef)) and
           (porddef(def)^.typ=u32bit) then
          begin
              if (l<luv) or (l>huv) then
                begin
                   if (cs_check_range in aktlocalswitches) then
                     Message(parser_e_range_check_error)
                   else
                     Message(parser_w_range_check_error);
                end;
          end
        else if (l<lsv) or (l>hsv) then
          begin
             if (typeof(def^)=typeof(Tenumdef)) or
                (cs_check_range in aktlocalswitches) then
               Message(parser_e_range_check_error)
             else
               Message(parser_w_range_check_error);
             { Fix the value to fit in the allocated space for this type of variable }
               case def^.size of
                 1: l := l and $ff;
                 2: l := l and $ffff;
               end
          end;
     end;
end;


{ return the range from def in l and h }
procedure getrange(def : pdef;var l:longint;var h : longint);

{Needs fixing for u32bit; low.signed etc....}

begin
    if typeof(def^)=typeof(Torddef) then
        begin
          l:=porddef(def)^.low.values;
          h:=porddef(def)^.high.values;
        end
    else if typeof(def^)=typeof(Tenumdef) then
        begin
          l:=penumdef(def)^.minval;
          h:=penumdef(def)^.maxval;
        end
    else if typeof(def^)=typeof(Tarraydef) then
        begin
          l:=parraydef(def)^.lowrange.values;
          h:=parraydef(def)^.highrange.values;
        end
    else
        internalerror(987);
end;


function mmx_type(p : pdef) : tmmxtype;
begin
   mmx_type:=mmxno;
   if is_mmx_able_array(p) then
     begin
        if typeof((Parraydef(p)^.definition^))=typeof(Tfloatdef) then
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

var b : boolean;
    hd : pdef;
    d1type,d2type:pointer;

begin
    {!!!! This code should never be called with nil parameters. If you really
     want to check this, make it an internalerror instead of an exit!! (DM)
    if not (assigned(def1) and assigned(def2)) then
     begin
       is_equal:=false;
       exit;
     end;}

    { be sure, that if there is a stringdef, that this is def1 }
    if typeof(def2^)=typeof(Tstringdef) then
        begin
            hd:=def1;
            def1:=def2;
            def2:=hd;
        end;
    b:=false;
    d1type:=typeof(def1^);
    d2type:=typeof(def2^);

    { both point to the same definition ? }
    if def1=def2 then
      b:=true
    else
    { pointer with an equal definition are equal }
      if (d1type=typeof(Tpointerdef)) and (d1type=d2type) then
        begin
           { here a problem detected in tabsolutesym }
           { the types can be forward type !!        }
           if assigned(def1^.sym) and
            (typeof((Ppointerdef(def1)^.definition^))=typeof(Tforwarddef)) then
             b:=(def1^.sym=def2^.sym)
           else
             b:=ppointerdef(def1)^.definition=ppointerdef(def2)^.definition;
        end
    else
    { ordinals are equal only when the ordinal type is equal }
      if (d1type=typeof(Torddef)) and (d1type=d2type) then
        begin
           case porddef(def1)^.typ of
           u8bit,u16bit,u32bit,
           s8bit,s16bit,s32bit:
             b:=((porddef(def1)^.typ=porddef(def2)^.typ) and
              (porddef(def1)^.low.values=porddef(def2)^.low.values) and
              (porddef(def1)^.high.values=porddef(def2)^.high.values));
           uvoid,uchar,
           bool8bit,bool16bit,bool32bit:
             b:=(porddef(def1)^.typ=porddef(def2)^.typ);
           end;
        end
    else
      if (d1type=typeof(Tfloatdef)) and (d1type=d2type) then
        b:=pfloatdef(def1)^.typ=pfloatdef(def2)^.typ
    else
      { strings with the same length are equal }
      if (d1type=typeof(Tstringdef)) and (d1type=d2type) and
         (pstringdef(def1)^.string_typ=pstringdef(def2)^.string_typ) then
        begin
           b:=not(is_shortstring(def1)) or
              (pstringdef(def1)^.len=pstringdef(def2)^.len);
        end
    else
      if (d1type=typeof(Tformaldef)) and (d1type=d2type) then
        b:=true
    { file types with the same file element type are equal }
    { this is a problem for assign !!                      }
    { changed to allow if one is untyped                   }
    { all typed files are equal to the special             }
    { typed file that has voiddef as elemnt type           }
    { but must NOT match for text file !!!                 }
    else
       if (d1type=typeof(Tfiledef)) and (d1type=d2type) then
         b:=(pfiledef(def1)^.filetype=pfiledef(def2)^.filetype) and
            ((
            ((pfiledef(def1)^.definition=nil) and
             (pfiledef(def2)^.definition=nil)) or
            (
             (pfiledef(def1)^.definition<>nil) and
             (pfiledef(def2)^.definition<>nil) and
             is_equal(pfiledef(def1)^.definition,pfiledef(def2)^.definition)
            ) or
            ( (pfiledef(def1)^.definition=pdef(voiddef)) or
              (pfiledef(def2)^.definition=pdef(voiddef))
            )))
    { sets with the same element type are equal }
    else
      if (d1type=typeof(Tsetdef)) and (d1type=d2type) then
        begin
            if assigned(psetdef(def1)^.definition) and
            assigned(psetdef(def2)^.definition) then
                b:=(typeof((psetdef(def1)^.definition^))=
                 typeof((psetdef(def2)^.definition^)))
            else
                b:=true;
        end
    else
      if (d1type=typeof(Tprocvardef)) and (d1type=d2type) then
        begin
           { poassembler isn't important for compatibility }
           { if a method is assigned to a methodpointer    }
           { is checked before                             }
           b:=(pprocvardef(def1)^.options=pprocvardef(def2)^.options) and
              (pprocvardef(def1)^.calloptions=pprocvardef(def2)^.calloptions) and
              ((pprocvardef(def1)^.options*po_compatibility_options)=
               (pprocvardef(def2)^.options*po_compatibility_options)) and
              is_equal(pprocvardef(def1)^.retdef,pprocvardef(def2)^.retdef) and
              equal_paras(pprocvardef(def1)^.parameters,pprocvardef(def2)^.parameters,false);
        end
    else
      if (d1type=typeof(Tarraydef)) and (d1type=d2type) then
        begin
          if is_open_array(def1) or is_open_array(def2) or
             is_array_of_const(def1) or is_array_of_const(def2) then
           begin
             if (ap_arrayofconst in parraydef(def1)^.options) or
              (ap_arrayofconst in parraydef(def2)^.options) then
                b:=true
             else
                b:=is_equal(parraydef(def1)^.definition,parraydef(def2)^.definition);
           end
          else
           begin
             b:=not(m_tp in aktmodeswitches) and
                not(m_delphi in aktmodeswitches) and
                (parraydef(def1)^.lowrange.values=parraydef(def2)^.lowrange.values) and
                (parraydef(def1)^.highrange.values=parraydef(def2)^.highrange.values) and
                is_equal(parraydef(def1)^.definition,parraydef(def2)^.definition) and
                is_equal(parraydef(def1)^.rangedef,parraydef(def2)^.rangedef);
           end;
        end
    else
        if (d1type=typeof(Tclassrefdef)) and (d1type=d2type) then
        begin
            {Similar to pointerdef:}
            if (def1^.sym<>nil) and (typeof((pclassrefdef(def1)^.definition^))=
             typeof(Tforwarddef)) then
                b:=(def1^.sym=def2^.sym)
            else
                b:=is_equal(pclassrefdef(def1)^.definition,pclassrefdef(def2)^.definition);
        end;
    is_equal:=b;
end;


function is_subequal(def1, def2: pdef): boolean;

begin
    is_subequal := false;
    if (typeof(def1^)=typeof(Torddef)) and (typeof(def2^)=typeof(Torddef)) then
        { see p.47 of Turbo Pascal 7.01 manual for the separation of types }
        { range checking for case statements is done with testrange        }
        case porddef(def1)^.typ of
            u8bit,u16bit,u32bit,
            s8bit,s16bit,s32bit:
                is_subequal:=(porddef(def2)^.typ in
                 [s32bit,u32bit,u8bit,s8bit,s16bit,u16bit]);
            bool8bit,bool16bit,bool32bit :
                is_subequal:=(porddef(def2)^.typ in
                 [bool8bit,bool16bit,bool32bit]);
            uchar:
                is_subequal:=(porddef(def2)^.typ=uchar);
        end
    else
        { I assume that both enumerations are equal when the first }
        { pointers are equal.                                      }
        if (typeof(def1^)=typeof(Tenumdef)) and (typeof(def2^)=typeof(Tenumdef)) then
          Begin
            if penumdef(def1)^.symbols=penumdef(def2)^.symbols then
               is_subequal := TRUE;
          end;
end;

function CheckTypes(def1,def2 : pdef) : boolean;

var
   s1,s2 : string;

begin
    if not is_equal(def1,def2) then
        begin
            s1:=def1^.typename;
            s2:=def2^.typename;
            if (s1<>'<unknown type>') and (s2<>'<unknown type>') then
                Message2(type_e_not_equal_types,s1,s2)
            else
                Message(type_e_mismatch);
            CheckTypes:=false;
        end
    else
        CheckTypes:=true;
end;

end.
{
  $Log$
  Revision 1.1  2000-02-28 17:23:58  daniel
  * Current work of symtable integration committed. The symtable can be
    activated by defining 'newst', but doesn't compile yet. Changes in type
    checking and oop are completed. What is left is to write a new
    symtablestack and adapt the parser to use it.

  Revision 1.97  2000/02/09 13:23:09  peter
    * log truncated

  Revision 1.96  2000/02/01 09:44:03  peter
    * is_voidpointer

  Revision 1.95  2000/01/07 01:14:49  peter
    * updated copyright to 2000

  Revision 1.94  2000/01/04 16:35:58  jonas
    * when range checking is off, constants that are out of bound are no longer
      truncated to their max/min legal value but left alone (jsut an "and" is done to
      make sure they fit in the allocated space if necessary)

  Revision 1.93  1999/12/31 14:26:28  peter
    * fixed crash with empty array constructors

  Revision 1.92  1999/11/30 10:40:59  peter
    + ttype, tsymlist

  Revision 1.91  1999/11/06 14:34:31  peter
    * truncated log to 20 revs

  Revision 1.90  1999/10/26 12:30:46  peter
    * const parameter is now checked
    * better and generic check if a node can be used for assigning
    * export fixes
    * procvar equal works now (it never had worked at least from 0.99.8)
    * defcoll changed to linkedlist with pparaitem so it can easily be
      walked both directions

  Revision 1.89  1999/10/01 10:04:07  peter
    * fixed is_equal for proc -> procvar which didn't check the
      callconvention and type anymore since the splitting of procoptions

  Revision 1.88  1999/10/01 08:02:51  peter
    * forward type declaration rewritten

  Revision 1.87  1999/09/15 22:09:27  florian
    + rtti is now automatically generated for published classes, i.e.
      they are handled like an implicit property

  Revision 1.86  1999/09/11 09:08:35  florian
    * fixed bug 596
    * fixed some problems with procedure variables and procedures of object,
      especially in TP mode. Procedure of object doesn't apply only to classes,
      it is also allowed for objects !!

  Revision 1.85  1999/08/13 21:27:08  peter
    * more fixes for push_addr

  Revision 1.84  1999/08/13 15:38:23  peter
    * fixed push_addr_param for records < 4, the array high<low range check
      broke this code.

  Revision 1.83  1999/08/07 14:21:06  florian
    * some small problems fixed

  Revision 1.82  1999/08/07 13:36:56  daniel
  * Recommitted the arraydef overflow bugfix.

  Revision 1.80  1999/08/05 22:42:49  daniel
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

}
