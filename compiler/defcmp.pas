{
    Copyright (c) 1998-2002 by Florian Klaempfl

    Compare definitions and parameter lists

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
unit defcmp;

{$i fpcdefs.inc}

interface

    uses
       cclasses,
       globtype,globals,
       node,
       symconst,symtype,symdef;

     type
       { if acp is cp_all the var const or nothing are considered equal }
       tcompare_paras_type = ( cp_none, cp_value_equal_const, cp_all,cp_procvar);
       tcompare_paras_option = (cpo_allowdefaults,cpo_ignorehidden,cpo_allowconvert,cpo_comparedefaultvalue,cpo_openequalisexact);
       tcompare_paras_options = set of tcompare_paras_option;

       tcompare_defs_option = (cdo_internal,cdo_explicit,cdo_check_operator,cdo_allow_variant,cdo_parameter);
       tcompare_defs_options = set of tcompare_defs_option;

       tconverttype = (tc_none,
          tc_equal,
          tc_not_possible,
          tc_string_2_string,
          tc_char_2_string,
          tc_char_2_chararray,
          tc_pchar_2_string,
          tc_cchar_2_pchar,
          tc_cstring_2_pchar,
          tc_cstring_2_int,
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
          tc_real_2_currency,
          tc_proc_2_procvar,
          tc_nil_2_methodprocvar,
          tc_arrayconstructor_2_set,
          tc_set_to_set,
          tc_cord_2_pointer,
          tc_intf_2_string,
          tc_intf_2_guid,
          tc_class_2_intf,
          tc_char_2_char,
          tc_dynarray_2_openarray,
          tc_pwchar_2_string,
          tc_variant_2_dynarray,
          tc_dynarray_2_variant,
          tc_variant_2_enum,
          tc_enum_2_variant,
          tc_interface_2_variant,
          tc_variant_2_interface,
          tc_array_2_dynarray
       );

    function compare_defs_ext(def_from,def_to : tdef;
                              fromtreetype : tnodetype;
                              var doconv : tconverttype;
                              var operatorpd : tprocdef;
                              cdoptions:tcompare_defs_options):tequaltype;

    { Returns if the type def_from can be converted to def_to or if both types are equal }
    function compare_defs(def_from,def_to:tdef;fromtreetype:tnodetype):tequaltype;

    { Returns true, if def1 and def2 are semantically the same }
    function equal_defs(def_from,def_to:tdef):boolean;

    { Checks for type compatibility (subgroups of type)
      used for case statements... probably missing stuff
      to use on other types }
    function is_subequal(def1, def2: tdef): boolean;

     {# true, if two parameter lists are equal
      if acp is cp_none, all have to match exactly
      if acp is cp_value_equal_const call by value
      and call by const parameter are assumed as
      equal
      allowdefaults indicates if default value parameters
      are allowed (in this case, the search order will first
      search for a routine with default parameters, before
      searching for the same definition with no parameters)
    }
    function compare_paras(para1,para2 : TFPObjectList; acp : tcompare_paras_type; cpoptions: tcompare_paras_options):tequaltype;

    { True if a function can be assigned to a procvar }
    { changed first argument type to pabstractprocdef so that it can also be }
    { used to test compatibility between two pprocvardefs (JM)               }
    function proc_to_procvar_equal(def1:tabstractprocdef;def2:tprocvardef):tequaltype;

    { Parentdef is the definition of a method defined in a parent class or interface }
    { Childdef is the definition of a method defined in a child class, interface or  }
    { a class implementing an interface with parentdef.                              }
    { Returns true if the resultdef of childdef can be used to implement/override    }
    { parentdef's resultdef                                                          }
    function compatible_childmethod_resultdef(parentretdef, childretdef: tdef): boolean;


implementation

    uses
      verbose,systems,
      symtable,symsym,
      defutil,symutil;


    function compare_defs_ext(def_from,def_to : tdef;
                              fromtreetype : tnodetype;
                              var doconv : tconverttype;
                              var operatorpd : tprocdef;
                              cdoptions:tcompare_defs_options):tequaltype;

      { tordtype:
           uvoid,
           u8bit,u16bit,u32bit,u64bit,
           s8bit,s16bit,s32bit,s64bit,
           bool8bit,bool16bit,bool32bit,bool64bit,
           uchar,uwidechar }

      type
        tbasedef=(bvoid,bchar,bint,bbool);
      const
        basedeftbl:array[tordtype] of tbasedef =
          (bvoid,
           bint,bint,bint,bint,
           bint,bint,bint,bint,
           bbool,bbool,bbool,bbool,
           bchar,bchar,bint);

        basedefconvertsimplicit : array[tbasedef,tbasedef] of tconverttype =
          { void, char, int, bool }
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_char_2_char,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_int_2_int,tc_not_possible),
          (tc_not_possible,tc_not_possible,tc_not_possible,tc_bool_2_bool));
        basedefconvertsexplicit : array[tbasedef,tbasedef] of tconverttype =
          { void, char, int, bool }
         ((tc_not_possible,tc_not_possible,tc_not_possible,tc_not_possible),
          (tc_not_possible,tc_char_2_char,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_int_2_int,tc_int_2_int,tc_int_2_bool),
          (tc_not_possible,tc_bool_2_int,tc_bool_2_int,tc_bool_2_bool));

      var
         subeq,eq : tequaltype;
         hd1,hd2 : tdef;
         hct : tconverttype;
         hobjdef : tobjectdef;
         hpd : tprocdef;
      begin
         eq:=te_incompatible;
         doconv:=tc_not_possible;

         { safety check }
         if not(assigned(def_from) and assigned(def_to)) then
          begin
            compare_defs_ext:=te_incompatible;
            exit;
          end;

         { same def? then we've an exact match }
         if def_from=def_to then
          begin
            doconv:=tc_equal;
            compare_defs_ext:=te_exact;
            exit;
          end;

         { undefined def? then mark it as equal }
         if (def_from.typ=undefineddef) or
            (def_to.typ=undefineddef) then
          begin
            doconv:=tc_equal;
            compare_defs_ext:=te_equal;
            exit;
          end;

         { undefined def? then mark it as equal }
         if (def_from.typ=undefineddef) or
            (def_to.typ=undefineddef) then
          begin
            doconv:=tc_equal;
            compare_defs_ext:=te_equal;
            exit;
          end;

         { we walk the wanted (def_to) types and check then the def_from
           types if there is a conversion possible }
         case def_to.typ of
           orddef :
             begin
               case def_from.typ of
                 orddef :
                   begin
                     if (torddef(def_from).ordtype=torddef(def_to).ordtype) then
                      begin
                        case torddef(def_from).ordtype of
                          uchar,uwidechar,
                          u8bit,u16bit,u32bit,u64bit,
                          s8bit,s16bit,s32bit,s64bit:
                            begin
                              if (torddef(def_from).low>=torddef(def_to).low) and
                                 (torddef(def_from).high<=torddef(def_to).high) then
                                eq:=te_equal
                              else
                                begin
                                  doconv:=tc_int_2_int;
                                  eq:=te_convert_l1;
                                end;
                            end;
                          uvoid,
                          bool8bit,bool16bit,bool32bit,bool64bit:
                            eq:=te_equal;
                          else
                            internalerror(200210061);
                        end;
                      end
                     else
                      begin
                        if cdo_explicit in cdoptions then
                         doconv:=basedefconvertsexplicit[basedeftbl[torddef(def_from).ordtype],basedeftbl[torddef(def_to).ordtype]]
                        else
                         doconv:=basedefconvertsimplicit[basedeftbl[torddef(def_from).ordtype],basedeftbl[torddef(def_to).ordtype]];
                        if (doconv=tc_not_possible) then
                          eq:=te_incompatible
                        else if (not is_in_limit(def_from,def_to)) then
                          { "punish" bad type conversions :) (JM) }
                          eq:=te_convert_l3
                         else
                          eq:=te_convert_l1;
                      end;
                   end;
                 enumdef :
                   begin
                     { needed for char(enum) }
                     if cdo_explicit in cdoptions then
                      begin
                        doconv:=tc_int_2_int;
                        eq:=te_convert_l1;
                      end;
                   end;
                 floatdef :
                   begin
                     if is_currency(def_to) then
                      begin
                        doconv:=tc_real_2_currency;
                        eq:=te_convert_l2;
                      end;
                   end;
                 objectdef:
                   begin
                     if is_class_or_interface_or_dispinterface(def_from) and (cdo_explicit in cdoptions) then
                      begin
                        eq:=te_convert_l1;
                        if (fromtreetype=niln) then
                         begin
                           { will be handled by the constant folding }
                           doconv:=tc_equal;
                         end
                        else
                         doconv:=tc_int_2_int;
                      end;
                   end;
                 classrefdef,
                 procvardef,
                 pointerdef :
                   begin
                     if cdo_explicit in cdoptions then
                      begin
                        eq:=te_convert_l1;
                        if (fromtreetype=niln) then
                         begin
                           { will be handled by the constant folding }
                           doconv:=tc_equal;
                         end
                        else
                         doconv:=tc_int_2_int;
                      end;
                   end;
                 arraydef :
                   begin
                     if (m_mac in current_settings.modeswitches) and
                        (fromtreetype=stringconstn) then
                       begin
                         eq:=te_convert_l3;
                         doconv:=tc_cstring_2_int;
                       end;
                   end;
               end;
             end;

           stringdef :
             begin
               case def_from.typ of
                 stringdef :
                   begin
                     { Constant string }
                     if (fromtreetype=stringconstn) then
                      begin
                        if (tstringdef(def_from).stringtype=tstringdef(def_to).stringtype) then
                          eq:=te_equal
                        else
                         begin
                           doconv:=tc_string_2_string;
                           { Don't prefer conversions from widestring to a
                             normal string as we can loose information }
                           if tstringdef(def_from).stringtype=st_widestring then
                             eq:=te_convert_l3
                           else if tstringdef(def_to).stringtype=st_widestring then
                             eq:=te_convert_l2
                           else
                             eq:=te_equal;
                         end;
                      end
                     else
                     { Same string type, for shortstrings also the length must match }
                      if (tstringdef(def_from).stringtype=tstringdef(def_to).stringtype) and
                         ((tstringdef(def_from).stringtype<>st_shortstring) or
                          (tstringdef(def_from).len=tstringdef(def_to).len)) then
                        eq:=te_equal
                     else
                       begin
                         doconv:=tc_string_2_string;
                         case tstringdef(def_from).stringtype of
                           st_widestring :
                             begin
                               { Prefer conversions to ansistring }
                               if tstringdef(def_to).stringtype=st_ansistring then
                                 eq:=te_convert_l2
                               else
                                 eq:=te_convert_l3;
                             end;
                           st_shortstring :
                             begin
                               { Prefer shortstrings of different length or conversions
                                 from shortstring to ansistring }
                               if (tstringdef(def_to).stringtype=st_shortstring) then
                                 eq:=te_convert_l1
                               else if tstringdef(def_to).stringtype=st_ansistring then
                                 eq:=te_convert_l2
                               else
                                 eq:=te_convert_l3;
                             end;
                           st_ansistring :
                             begin
                               { Prefer conversion to widestrings }
                               if (tstringdef(def_to).stringtype=st_widestring) then
                                 eq:=te_convert_l2
                               else
                                 eq:=te_convert_l3;
                             end;
                         end;
                       end;
                   end;
                 orddef :
                   begin
                   { char to string}
                     if is_char(def_from) or
                        is_widechar(def_from) then
                      begin
                        doconv:=tc_char_2_string;
                        eq:=te_convert_l1;
                      end;
                   end;
                 arraydef :
                   begin
                     { array of char to string, the length check is done by the firstpass of this node }
                     if is_chararray(def_from) or is_open_chararray(def_from) then
                      begin
                        { "Untyped" stringconstn is an array of char }
                        if fromtreetype=stringconstn then
                          begin
                            doconv:=tc_string_2_string;
                            { prefered string type depends on the $H switch }
                            if not(cs_ansistrings in current_settings.localswitches) and
                               (tstringdef(def_to).stringtype=st_shortstring) then
                              eq:=te_equal
                            else if (cs_ansistrings in current_settings.localswitches) and
                               (tstringdef(def_to).stringtype=st_ansistring) then
                              eq:=te_equal
                            else if tstringdef(def_to).stringtype=st_widestring then
                              eq:=te_convert_l3
                            else
                              eq:=te_convert_l1;
                          end
                        else
                          begin
                          doconv:=tc_chararray_2_string;
                          if is_open_array(def_from) then
                            begin
                              if is_ansistring(def_to) then
                                eq:=te_convert_l1
                              else if is_widestring(def_to) then
                                eq:=te_convert_l3
                              else
                                eq:=te_convert_l2;
                            end
                          else
                            begin
                              if is_shortstring(def_to) then
                                begin
                                  { Only compatible with arrays that fit
                                    smaller than 255 chars }
                                  if (def_from.size <= 255) then
                                    eq:=te_convert_l1;
                                end
                              else if is_ansistring(def_to) then
                                begin
                                  if (def_from.size > 255) then
                                    eq:=te_convert_l1
                                  else
                                    eq:=te_convert_l2;
                                end
                              else if is_widestring(def_to) then
                                eq:=te_convert_l3
                              else
                                eq:=te_convert_l2;
                            end;
                          end;
                      end
                     else
                     { array of widechar to string, the length check is done by the firstpass of this node }
                      if is_widechararray(def_from) or is_open_widechararray(def_from) then
                       begin
                         doconv:=tc_chararray_2_string;
                         if is_widestring(def_to) then
                           eq:=te_convert_l1
                         else
                           { size of widechar array is double due the sizeof a widechar }
                           if not(is_shortstring(def_to) and (def_from.size>255*sizeof(widechar))) then
                             eq:=te_convert_l3
                         else
                           eq:=te_convert_l2;
                       end;
                   end;
                 pointerdef :
                   begin
                   { pchar can be assigned to short/ansistrings,
                     but not in tp7 compatible mode }
                     if not(m_tp7 in current_settings.modeswitches) then
                       begin
                          if is_pchar(def_from) then
                           begin
                             doconv:=tc_pchar_2_string;
                             { prefer ansistrings because pchars can overflow shortstrings, }
                             { but only if ansistrings are the default (JM)                 }
                             if (is_shortstring(def_to) and
                                 not(cs_ansistrings in current_settings.localswitches)) or
                                (is_ansistring(def_to) and
                                 (cs_ansistrings in current_settings.localswitches)) then
                               eq:=te_convert_l1
                             else
                               eq:=te_convert_l2;
                           end
                          else if is_pwidechar(def_from) then
                           begin
                             doconv:=tc_pwchar_2_string;
                             if is_widestring(def_to) then
                               eq:=te_convert_l1
                             else
                               eq:=te_convert_l3;
                           end;
                       end;
                   end;
               end;
             end;

           floatdef :
             begin
               case def_from.typ of
                 orddef :
                   begin { ordinal to real }
                     { only for implicit and internal typecasts in tp/delphi }
                     if (([cdo_explicit,cdo_internal] * cdoptions <> [cdo_explicit]) or
                         ([m_tp7,m_delphi] * current_settings.modeswitches = [])) and
                        (is_integer(def_from) or
                         (is_currency(def_from) and
                          (s64currencytype.typ = floatdef))) then
                       begin
                         doconv:=tc_int_2_real;
                         eq:=te_convert_l4;
                       end
                     else if is_currency(def_from)
                             { and (s64currencytype.typ = orddef)) } then
                       begin
                         { prefer conversion to orddef in this case, unless    }
                         { the orddef < currency (then it will get convert l3, }
                         { and conversion to float is favoured)                }
                         doconv:=tc_int_2_real;
                         eq:=te_convert_l2;
                       end;
                   end;
                 floatdef :
                   begin
                     if tfloatdef(def_from).floattype=tfloatdef(def_to).floattype then
                       eq:=te_equal
                     else
                       begin
                         { Delphi does not allow explicit type conversions for float types like:
                             single_var:=single(double_var);
                           But if such conversion is inserted by compiler (internal) for some purpose,
                           it should be allowed even in Delphi mode. }
                         if (fromtreetype=realconstn) or
                            not((cdoptions*[cdo_explicit,cdo_internal]=[cdo_explicit]) and
                                (m_delphi in current_settings.modeswitches)) then
                           begin
                             doconv:=tc_real_2_real;
                             { do we lose precision? }
                             if def_to.size<def_from.size then
                               eq:=te_convert_l2
                             else
                               eq:=te_convert_l1;
                           end;
                       end;
                   end;
               end;
             end;

           enumdef :
             begin
               case def_from.typ of
                 enumdef :
                   begin
                     if cdo_explicit in cdoptions then
                      begin
                        eq:=te_convert_l1;
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
                            eq:=te_convert_l1;
                            { because of packenum they can have different sizes! (JM) }
                            doconv:=tc_int_2_int;
                          end
                        else
                          begin
                            { assignment of an enum symbol to an unique type? }
                            if (fromtreetype=ordconstn) and
                              (tenumsym(tenumdef(hd1).firstenum)=tenumsym(tenumdef(hd2).firstenum)) then
                              begin
                                { because of packenum they can have different sizes! (JM) }
                                eq:=te_convert_l1;
                                doconv:=tc_int_2_int;
                              end;
                          end;
                      end;
                   end;
                 orddef :
                   begin
                     if cdo_explicit in cdoptions then
                      begin
                        eq:=te_convert_l1;
                        doconv:=tc_int_2_int;
                      end;
                   end;
                 variantdef :
                   begin
                     eq:=te_convert_l1;
                     doconv:=tc_variant_2_enum;
                   end;
                 pointerdef :
                   begin
                     { ugly, but delphi allows it }
                     if (cdo_explicit in cdoptions) and
                       (m_delphi in current_settings.modeswitches) and
                       (eq=te_incompatible) then
                       begin
                         doconv:=tc_int_2_int;
                         eq:=te_convert_l1;
                       end;
                   end;
               end;
             end;

           arraydef :
             begin
               { open array is also compatible with a single element of its base type.
                 the extra check for deftyp is needed because equal defs can also return
                 true if the def types are not the same, for example with dynarray to pointer. }
               if is_open_array(def_to) and
                  (def_from.typ=tarraydef(def_to).elementdef.typ) and
                  equal_defs(def_from,tarraydef(def_to).elementdef) then
                begin
                  doconv:=tc_equal;
                  eq:=te_convert_l1;
                end
               else
                begin
                  case def_from.typ of
                    arraydef :
                      begin
                        { from/to packed array -- packed chararrays are      }
                        { strings in ISO Pascal (at least if the lower bound }
                        { is 1, but GPC makes all equal-length chararrays    }
                        { compatible), so treat those the same as regular    }
                        { char arrays                                        }
                        if (is_packed_array(def_from) and
                            not is_chararray(def_from) and
                            not is_widechararray(def_from)) xor
                           (is_packed_array(def_to) and
                            not is_chararray(def_to) and
                            not is_widechararray(def_to)) then
                          { both must be packed }
                          begin
                            compare_defs_ext:=te_incompatible;
                            exit;
                          end
                        { to dynamic array }
                        else if is_dynamic_array(def_to) then
                         begin
                           if equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) then
                             begin
                               { dynamic array -> dynamic array }
                               if is_dynamic_array(def_from) then
                                 eq:=te_equal
                               { fpc modes only: array -> dyn. array }
                               else if (current_settings.modeswitches*[m_objfpc,m_fpc]<>[]) and
                                 not(is_special_array(def_from)) and
                                 is_zero_based_array(def_from) then
                                 begin
                                   eq:=te_convert_l2;
                                   doconv:=tc_array_2_dynarray;
                                 end;
                             end
                         end
                        else
                         { to open array }
                         if is_open_array(def_to) then
                          begin
                            { array constructor -> open array }
                            if is_array_constructor(def_from) then
                             begin
                               if is_void(tarraydef(def_from).elementdef) then
                                begin
                                  doconv:=tc_equal;
                                  eq:=te_convert_l1;
                                end
                               else
                                begin
                                  subeq:=compare_defs_ext(tarraydef(def_from).elementdef,
                                                       tarraydef(def_to).elementdef,
                                                       { reason for cdo_allow_variant: see webtbs/tw7070a and webtbs/tw7070b }
                                                       arrayconstructorn,hct,hpd,[cdo_check_operator,cdo_allow_variant]);
                                  if (subeq>=te_equal) then
                                    begin
                                      doconv:=tc_equal;
                                      eq:=te_convert_l1;
                                    end
                                  else
                                   if (subeq>te_incompatible) then
                                    begin
                                      doconv:=hct;
                                      eq:=te_convert_l2;
                                    end;
                                end;
                             end
                            else
                             { dynamic array -> open array }
                             if is_dynamic_array(def_from) and
                                equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) then
                               begin
                                 doconv:=tc_dynarray_2_openarray;
                                 eq:=te_convert_l2;
                               end
                            else
                             { open array -> open array }
                             if is_open_array(def_from) and
                                equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) then
                               if tarraydef(def_from).elementdef=tarraydef(def_to).elementdef then
                                 eq:=te_exact
                               else
                                 eq:=te_equal
                            else
                             { array -> open array }
                             if not(cdo_parameter in cdoptions) and
                                equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) then
                               begin
                                 if fromtreetype=stringconstn then
                                   eq:=te_convert_l1
                                 else
                                   eq:=te_equal;
                               end;
                          end
                        else
                         { to array of const }
                         if is_array_of_const(def_to) then
                          begin
                            if is_array_of_const(def_from) or
                               is_array_constructor(def_from) then
                             begin
                               eq:=te_equal;
                             end
                            else
                             { array of tvarrec -> array of const }
                             if equal_defs(tarraydef(def_to).elementdef,tarraydef(def_from).elementdef) then
                              begin
                                doconv:=tc_equal;
                                eq:=te_convert_l1;
                              end;
                          end
                        else
                          { to array of char, from "Untyped" stringconstn (array of char) }
                          if (fromtreetype=stringconstn) and
                             (is_chararray(def_to) or
                              is_widechararray(def_to)) then
                            begin
                              eq:=te_convert_l1;
                              doconv:=tc_string_2_chararray;
                            end
                        else
                         { other arrays }
                          begin
                            { open array -> array }
                            if not(cdo_parameter in cdoptions) and
                               is_open_array(def_from) and
                               equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) then
                              begin
                                eq:=te_equal
                              end
                            else
                            { array -> array }
                             if not(m_tp7 in current_settings.modeswitches) and
                                not(m_delphi in current_settings.modeswitches) and
                                (tarraydef(def_from).lowrange=tarraydef(def_to).lowrange) and
                                (tarraydef(def_from).highrange=tarraydef(def_to).highrange) and
                                equal_defs(tarraydef(def_from).elementdef,tarraydef(def_to).elementdef) and
                                equal_defs(tarraydef(def_from).rangedef,tarraydef(def_to).rangedef) then
                              begin
                                eq:=te_equal
                              end;
                          end;
                      end;
                    pointerdef :
                      begin
                        { nil and voidpointers are compatible with dyn. arrays }
                        if is_dynamic_array(def_to) and
                           ((fromtreetype=niln) or
                            is_voidpointer(def_from)) then
                         begin
                           doconv:=tc_equal;
                           eq:=te_convert_l1;
                         end
                        else
                         if is_zero_based_array(def_to) and
                            equal_defs(tpointerdef(def_from).pointeddef,tarraydef(def_to).elementdef) then
                          begin
                            doconv:=tc_pointer_2_array;
                            eq:=te_convert_l1;
                          end;
                      end;
                    stringdef :
                      begin
                        { string to char array }
                        if (not is_special_array(def_to)) and
                           (is_char(tarraydef(def_to).elementdef)or
                            is_widechar(tarraydef(def_to).elementdef)) then
                         begin
                           doconv:=tc_string_2_chararray;
                           eq:=te_convert_l1;
                         end;
                      end;
                    orddef:
                      begin
                        if is_chararray(def_to) and
                           is_char(def_from) then
                          begin
                            doconv:=tc_char_2_chararray;
                            eq:=te_convert_l2;
                          end;
                      end;
                    recorddef :
                      begin
                        { tvarrec -> array of const }
                         if is_array_of_const(def_to) and
                            equal_defs(def_from,tarraydef(def_to).elementdef) then
                          begin
                            doconv:=tc_equal;
                            eq:=te_convert_l1;
                          end;
                      end;
                    variantdef :
                      begin
                         if is_dynamic_array(def_to) then
                           begin
                              doconv:=tc_variant_2_dynarray;
                              eq:=te_convert_l1;
                           end;
                      end;
                  end;
                end;
             end;

           variantdef :
             begin
               if (cdo_allow_variant in cdoptions) then
                 begin
                   case def_from.typ of
                     enumdef :
                       begin
                         doconv:=tc_enum_2_variant;
                         eq:=te_convert_l1;
                       end;
                     arraydef :
                       begin
                          if is_dynamic_array(def_from) then
                            begin
                               doconv:=tc_dynarray_2_variant;
                               eq:=te_convert_l1;
                            end;
                       end;
                     objectdef :
                       begin
                          if is_interface(def_from) then
                            begin
                               doconv:=tc_interface_2_variant;
                               eq:=te_convert_l1;
                            end;
                       end;
                     variantdef :
                       begin
                         { doing this in the compiler avoids a lot of unncessary
                           copying }
                         if (tvariantdef(def_from).varianttype=vt_olevariant) and
                           (tvariantdef(def_to).varianttype=vt_normalvariant) then
                           begin
                             doconv:=tc_equal;
                             eq:=te_convert_l1;
                           end;
                       end;
                   end;
                 end;
             end;

           pointerdef :
             begin
               case def_from.typ of
                 stringdef :
                   begin
                     { string constant (which can be part of array constructor)
                       to zero terminated string constant }
                     if (((fromtreetype = arrayconstructorn) and
                          { can't use is_chararray, because returns false for }
                          { array constructors                                }
                          is_char(tarraydef(def_from).elementdef)) or
                         (fromtreetype = stringconstn)) and
                        (is_pchar(def_to) or is_pwidechar(def_to)) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        eq:=te_convert_l2;
                      end
                     else
                      if cdo_explicit in cdoptions then
                       begin
                         { pchar(ansistring) }
                         if is_pchar(def_to) and
                            is_ansistring(def_from) then
                          begin
                            doconv:=tc_ansistring_2_pchar;
                            eq:=te_convert_l1;
                          end
                         else
                          { pwidechar(widestring) }
                          if is_pwidechar(def_to) and
                             is_widestring(def_from) then
                           begin
                             doconv:=tc_ansistring_2_pchar;
                             eq:=te_convert_l1;
                           end;
                       end;
                   end;
                 orddef :
                   begin
                     { char constant to zero terminated string constant }
                     if (fromtreetype in [ordconstn,arrayconstructorn]) then
                      begin
                        if (is_char(def_from) or is_widechar(def_from)) and
                           (is_pchar(def_to) or is_pwidechar(def_to)) then
                         begin
                           doconv:=tc_cchar_2_pchar;
                           eq:=te_convert_l1;
                         end
                        else
                         if (m_delphi in current_settings.modeswitches) and is_integer(def_from) then
                          begin
                            doconv:=tc_cord_2_pointer;
                            eq:=te_convert_l5;
                          end;
                      end;
                     { allow explicit typecasts from ordinals to pointer.
                       Support for delphi compatibility
                       Support constructs like pointer(cardinal-cardinal) or pointer(longint+cardinal) where
                        the result of the ordinal operation is int64 also on 32 bit platforms.
                       It is also used by the compiler internally for inc(pointer,ordinal) }
                     if (eq=te_incompatible) and
                        not is_void(def_from) and
                        (
                         (
                          (cdo_explicit in cdoptions) and
                          (
                           (m_delphi in current_settings.modeswitches) or
                           { Don't allow pchar(char) in fpc modes }
                           is_integer(def_from)
                          )
                         ) or
                         (cdo_internal in cdoptions)
                        ) then
                       begin
                         doconv:=tc_int_2_int;
                         eq:=te_convert_l1;
                       end;
                   end;
                 enumdef :
                   begin
                     { allow explicit typecasts from enums to pointer.
                       Support for delphi compatibility
                     }
                     if (eq=te_incompatible) and
                        (((cdo_explicit in cdoptions) and
                          (m_delphi in current_settings.modeswitches)
                          ) or
                         (cdo_internal in cdoptions)
                        ) then
                       begin
                         doconv:=tc_int_2_int;
                         eq:=te_convert_l1;
                       end;
                   end;
                 arraydef :
                   begin
                     { string constant (which can be part of array constructor)
                       to zero terminated string constant }
                     if (((fromtreetype = arrayconstructorn) and
                          { can't use is_chararray, because returns false for }
                          { array constructors                                }
                          is_char(tarraydef(def_from).elementdef)) or
                         (fromtreetype = stringconstn)) and
                        (is_pchar(def_to) or is_pwidechar(def_to)) then
                      begin
                        doconv:=tc_cstring_2_pchar;
                        eq:=te_convert_l2;
                      end
                     else
                      { chararray to pointer }
                      if (is_zero_based_array(def_from) or
                          is_open_array(def_from)) and
                          equal_defs(tarraydef(def_from).elementdef,tpointerdef(def_to).pointeddef) then
                        begin
                          doconv:=tc_array_2_pointer;
                          { don't prefer the pchar overload when a constant
                            string was passed }
                          if fromtreetype=stringconstn then
                            eq:=te_convert_l2
                          else
                            eq:=te_convert_l1;
                        end
                     else
                       { dynamic array to pointer, delphi only }
                       if (m_delphi in current_settings.modeswitches) and
                          is_dynamic_array(def_from) then
                        begin
                          eq:=te_equal;
                        end;
                   end;
                 pointerdef :
                   begin
                     { check for far pointers }
                     if (tpointerdef(def_from).is_far<>tpointerdef(def_to).is_far) then
                       begin
                         eq:=te_incompatible;
                       end
                     else
                      { the types can be forward type, handle before normal type check !! }
                      if assigned(def_to.typesym) and
                         (tpointerdef(def_to).pointeddef.typ=forwarddef) then
                       begin
                         if (def_from.typesym=def_to.typesym) then
                          eq:=te_equal
                       end
                     else
                      { same types }
                      if equal_defs(tpointerdef(def_from).pointeddef,tpointerdef(def_to).pointeddef) then
                       begin
                         eq:=te_equal
                       end
                     else
                      { child class pointer can be assigned to anchestor pointers }
                      if (
                          (tpointerdef(def_from).pointeddef.typ=objectdef) and
                          (tpointerdef(def_to).pointeddef.typ=objectdef) and
                          tobjectdef(tpointerdef(def_from).pointeddef).is_related(
                            tobjectdef(tpointerdef(def_to).pointeddef))
                         ) then
                       begin
                         doconv:=tc_equal;
                         eq:=te_convert_l1;
                       end
                     else
                      { all pointers can be assigned to void-pointer }
                      if is_void(tpointerdef(def_to).pointeddef) then
                       begin
                         doconv:=tc_equal;
                         { give pwidechar,pchar a penalty so it prefers
                           conversion to ansistring }
                         if is_pchar(def_from) or
                            is_pwidechar(def_from) then
                           eq:=te_convert_l2
                         else
                           eq:=te_convert_l1;
                       end
                     else
                      { all pointers can be assigned from void-pointer }
                      if is_void(tpointerdef(def_from).pointeddef) or
                      { all pointers can be assigned from void-pointer or formaldef pointer, check
                        tw3777.pp if you change this }
                        (tpointerdef(def_from).pointeddef.typ=formaldef) then
                       begin
                         doconv:=tc_equal;
                         { give pwidechar a penalty so it prefers
                           conversion to pchar }
                         if is_pwidechar(def_to) then
                           eq:=te_convert_l2
                         else
                           eq:=te_convert_l1;
                       end;
                   end;
                 procvardef :
                   begin
                     { procedure variable can be assigned to an void pointer,
                       this not allowed for methodpointers }
                     if (is_void(tpointerdef(def_to).pointeddef) or
                         (m_mac_procvar in current_settings.modeswitches)) and
                        tprocvardef(def_from).is_addressonly then
                      begin
                        doconv:=tc_equal;
                        eq:=te_convert_l1;
                      end;
                   end;
                 procdef :
                   begin
                     { procedure variable can be assigned to an void pointer,
                       this not allowed for methodpointers }
                     if (m_mac_procvar in current_settings.modeswitches) and
                        tprocdef(def_from).is_addressonly then
                      begin
                        doconv:=tc_proc_2_procvar;
                        eq:=te_convert_l2;
                      end;
                   end;
                 classrefdef,
                 objectdef :
                   begin
                     { class types and class reference type
                       can be assigned to void pointers, but it is less
                       preferred than assigning to a related objectdef }
                     if (
                         is_class_or_interface_or_dispinterface(def_from) or
                         (def_from.typ=classrefdef)
                        ) and
                        (tpointerdef(def_to).pointeddef.typ=orddef) and
                        (torddef(tpointerdef(def_to).pointeddef).ordtype=uvoid) then
                       begin
                         doconv:=tc_equal;
                         eq:=te_convert_l2;
                       end;
                   end;
               end;
             end;

           setdef :
             begin
               case def_from.typ of
                 setdef :
                   begin
                     if assigned(tsetdef(def_from).elementdef) and
                        assigned(tsetdef(def_to).elementdef) then
                      begin
                        { sets with the same element base type and the same range are equal }
                        if equal_defs(tsetdef(def_from).elementdef,tsetdef(def_to).elementdef) and
                          (tsetdef(def_from).setbase=tsetdef(def_to).setbase) and
                          (tsetdef(def_from).setmax=tsetdef(def_to).setmax) then
                          eq:=te_equal
                        else if is_subequal(tsetdef(def_from).elementdef,tsetdef(def_to).elementdef) then
                          begin
                            eq:=te_convert_l1;
                            doconv:=tc_set_to_set;
                          end;
                      end
                     else
                      begin
                        { empty set is compatible with everything }
                        eq:=te_convert_l1;
                        doconv:=tc_set_to_set;
                      end;
                   end;
                 arraydef :
                   begin
                     { automatic arrayconstructor -> set conversion }
                     if is_array_constructor(def_from) then
                      begin
                        doconv:=tc_arrayconstructor_2_set;
                        eq:=te_convert_l1;
                      end;
                   end;
               end;
             end;

           procvardef :
             begin
               case def_from.typ of
                 procdef :
                   begin
                     { proc -> procvar }
                     if (m_tp_procvar in current_settings.modeswitches) or
                        (m_mac_procvar in current_settings.modeswitches) then
                      begin
                        subeq:=proc_to_procvar_equal(tprocdef(def_from),tprocvardef(def_to));
                        if subeq>te_incompatible then
                         begin
                           doconv:=tc_proc_2_procvar;
                           eq:=te_convert_l1;
                         end;
                      end;
                   end;
                 procvardef :
                   begin
                     { procvar -> procvar }
                     eq:=proc_to_procvar_equal(tprocvardef(def_from),tprocvardef(def_to));
                   end;
                 pointerdef :
                   begin
                     { nil is compatible with procvars }
                     if (fromtreetype=niln) then
                      begin
                        if not Tprocvardef(def_to).is_addressonly then
                          {Nil to method pointers requires to convert a single
                           pointer nil value to a two pointer procvardef.}
                          doconv:=tc_nil_2_methodprocvar
                        else
                          doconv:=tc_equal;
                        eq:=te_convert_l1;
                      end
                     else
                      { for example delphi allows the assignement from pointers }
                      { to procedure variables                                  }
                      if (m_pointer_2_procedure in current_settings.modeswitches) and
                         is_void(tpointerdef(def_from).pointeddef) and
                         tprocvardef(def_to).is_addressonly then
                       begin
                         doconv:=tc_equal;
                         eq:=te_convert_l1;
                       end;
                   end;
               end;
             end;

           objectdef :
             begin
               { object pascal objects }
               if (def_from.typ=objectdef) and
                  (tobjectdef(def_from).is_related(tobjectdef(def_to))) then
                begin
                  doconv:=tc_equal;
                  eq:=te_convert_l1;
                end
               else
               { Class/interface specific }
                if is_class_or_interface_or_dispinterface(def_to) then
                 begin
                   { void pointer also for delphi mode }
                   if (m_delphi in current_settings.modeswitches) and
                      is_voidpointer(def_from) then
                    begin
                      doconv:=tc_equal;
                      { prefer pointer-pointer assignments }
                      eq:=te_convert_l2;
                    end
                   else
                   { nil is compatible with class instances and interfaces }
                    if (fromtreetype=niln) then
                     begin
                       doconv:=tc_equal;
                       eq:=te_convert_l1;
                     end
                   { classes can be assigned to interfaces }
                   else if is_interface(def_to) and
                           is_class(def_from) and
                           assigned(tobjectdef(def_from).ImplementedInterfaces) then
                     begin
                        { we've to search in parent classes as well }
                        hobjdef:=tobjectdef(def_from);
                        while assigned(hobjdef) do
                          begin
                             if hobjdef.find_implemented_interface(tobjectdef(def_to))<>nil then
                               begin
                                  doconv:=tc_class_2_intf;
                                  { don't prefer this over objectdef->objectdef }
                                  eq:=te_convert_l2;
                                  break;
                               end;
                             hobjdef:=hobjdef.childof;
                          end;
                     end
                   { Interface 2 GUID handling }
                   else if (def_to=tdef(rec_tguid)) and
                           (fromtreetype=typen) and
                           is_interface(def_from) and
                           assigned(tobjectdef(def_from).iidguid) then
                     begin
                       eq:=te_convert_l1;
                       doconv:=tc_equal;
                     end
                   else if (def_from.typ=variantdef) and is_interface(def_to) then
                     begin
                       doconv:=tc_variant_2_interface;
                       eq:=te_convert_l2;
                     end
                   { ugly, but delphi allows it }
                   else if (eq=te_incompatible) and
                     (def_from.typ=orddef) and
                     (m_delphi in current_settings.modeswitches) and
                     (cdo_explicit in cdoptions) then
                     begin
                       doconv:=tc_int_2_int;
                       eq:=te_convert_l1;
                     end;
                 end;
             end;

           classrefdef :
             begin
               { similar to pointerdef wrt forwards }
               if assigned(def_to.typesym) and
                  (tclassrefdef(def_to).pointeddef.typ=forwarddef) then
                 begin
                   if (def_from.typesym=def_to.typesym) then
                    eq:=te_equal;
                 end
               else
                { class reference types }
                if (def_from.typ=classrefdef) then
                 begin
                   if equal_defs(tclassrefdef(def_from).pointeddef,tclassrefdef(def_to).pointeddef) then
                    begin
                      eq:=te_equal;
                    end
                   else
                    begin
                      doconv:=tc_equal;
                      if (cdo_explicit in cdoptions) or
                         tobjectdef(tclassrefdef(def_from).pointeddef).is_related(
                           tobjectdef(tclassrefdef(def_to).pointeddef)) then
                        eq:=te_convert_l1;
                    end;
                 end
               else
                 if (m_delphi in current_settings.modeswitches) and
                    is_voidpointer(def_from) then
                  begin
                    doconv:=tc_equal;
                    { prefer pointer-pointer assignments }
                    eq:=te_convert_l2;
                  end
                 else
                { nil is compatible with class references }
                if (fromtreetype=niln) then
                 begin
                   doconv:=tc_equal;
                   eq:=te_convert_l1;
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
               if (def_from.typ=filedef) then
                begin
                  if (tfiledef(def_from).filetyp=tfiledef(def_to).filetyp) then
                   begin
                     if
                        (
                         (tfiledef(def_from).typedfiledef=nil) and
                         (tfiledef(def_to).typedfiledef=nil)
                        ) or
                        (
                         (tfiledef(def_from).typedfiledef<>nil) and
                         (tfiledef(def_to).typedfiledef<>nil) and
                         equal_defs(tfiledef(def_from).typedfiledef,tfiledef(def_to).typedfiledef)
                        ) or
                        (
                         (tfiledef(def_from).filetyp = ft_typed) and
                         (tfiledef(def_to).filetyp = ft_typed) and
                         (
                          (tfiledef(def_from).typedfiledef = tdef(voidtype)) or
                          (tfiledef(def_to).typedfiledef = tdef(voidtype))
                         )
                        ) then
                      begin
                        eq:=te_equal;
                      end;
                   end
                  else
                   if ((tfiledef(def_from).filetyp = ft_untyped) and
                       (tfiledef(def_to).filetyp = ft_typed)) or
                      ((tfiledef(def_from).filetyp = ft_typed) and
                       (tfiledef(def_to).filetyp = ft_untyped)) then
                    begin
                      doconv:=tc_equal;
                      eq:=te_convert_l1;
                    end;
                end;
             end;

           recorddef :
             begin
               { interface -> guid }
               if is_interface(def_from) and
                  (def_to=rec_tguid) then
                begin
                  doconv:=tc_intf_2_guid;
                  eq:=te_convert_l1;
                end;
             end;

           formaldef :
             begin
               doconv:=tc_equal;
               if (def_from.typ=formaldef) then
                 eq:=te_equal
               else
                { Just about everything can be converted to a formaldef...}
                if not (def_from.typ in [abstractdef,errordef]) then
                  eq:=te_convert_l2;
             end;
        end;

        { if we didn't find an appropriate type conversion yet
          then we search also the := operator }
        if (eq=te_incompatible) and
           { make sure there is not a single variant if variants   }
           { are not allowed (otherwise if only cdo_check_operator }
           { and e.g. fromdef=stringdef and todef=variantdef, then }
           { the test will still succeed                           }
           ((cdo_allow_variant in cdoptions) or
            ((def_from.typ<>variantdef) and (def_to.typ<>variantdef))
           ) and
           (
            { Check for variants? }
            (
             (cdo_allow_variant in cdoptions) and
             ((def_from.typ=variantdef) or (def_to.typ=variantdef))
            ) or
            { Check for operators? }
            (
             (cdo_check_operator in cdoptions) and
             ((def_from.typ in [objectdef,recorddef,arraydef,stringdef]) or
              (def_to.typ in [objectdef,recorddef,arraydef,stringdef]))
            )
           ) then
          begin
            operatorpd:=search_assignment_operator(def_from,def_to);
            if assigned(operatorpd) then
             eq:=te_convert_operator;
          end;

        { update convtype for te_equal when it is not yet set }
        if (eq=te_equal) and
           (doconv=tc_not_possible) then
          doconv:=tc_equal;

        compare_defs_ext:=eq;
      end;


    function equal_defs(def_from,def_to:tdef):boolean;
      var
        convtyp : tconverttype;
        pd : tprocdef;
      begin
        { Compare defs with nothingn and no explicit typecasts and
          searching for overloaded operators is not needed }
        equal_defs:=(compare_defs_ext(def_from,def_to,nothingn,convtyp,pd,[])>=te_equal);
      end;


    function compare_defs(def_from,def_to:tdef;fromtreetype:tnodetype):tequaltype;
      var
        doconv : tconverttype;
        pd : tprocdef;
      begin
        compare_defs:=compare_defs_ext(def_from,def_to,fromtreetype,doconv,pd,[cdo_check_operator,cdo_allow_variant]);
      end;


    function is_subequal(def1, def2: tdef): boolean;
      var
         basedef1,basedef2 : tenumdef;

      Begin
        is_subequal := false;
        if assigned(def1) and assigned(def2) then
         Begin
           if (def1.typ = orddef) and (def2.typ = orddef) then
            Begin
              { see p.47 of Turbo Pascal 7.01 manual for the separation of types }
              { range checking for case statements is done with testrange        }
              case torddef(def1).ordtype of
                u8bit,u16bit,u32bit,u64bit,
                s8bit,s16bit,s32bit,s64bit :
                  is_subequal:=(torddef(def2).ordtype in [s64bit,u64bit,s32bit,u32bit,u8bit,s8bit,s16bit,u16bit]);
                bool8bit,bool16bit,bool32bit,bool64bit :
                  is_subequal:=(torddef(def2).ordtype in [bool8bit,bool16bit,bool32bit,bool64bit]);
                uchar :
                  is_subequal:=(torddef(def2).ordtype=uchar);
                uwidechar :
                  is_subequal:=(torddef(def2).ordtype=uwidechar);
              end;
            end
           else
            Begin
              { Check if both basedefs are equal }
              if (def1.typ=enumdef) and (def2.typ=enumdef) then
                Begin
                   { get both basedefs }
                   basedef1:=tenumdef(def1);
                   while assigned(basedef1.basedef) do
                     basedef1:=basedef1.basedef;
                   basedef2:=tenumdef(def2);
                   while assigned(basedef2.basedef) do
                     basedef2:=basedef2.basedef;
                   is_subequal:=(basedef1=basedef2);
                end;
            end;
         end;
      end;


    function compare_paras(para1,para2 : TFPObjectList; acp : tcompare_paras_type; cpoptions: tcompare_paras_options):tequaltype;
      var
        currpara1,
        currpara2 : tparavarsym;
        eq,lowesteq : tequaltype;
        hpd       : tprocdef;
        convtype  : tconverttype;
        cdoptions : tcompare_defs_options;
        i1,i2     : byte;
      begin
         compare_paras:=te_incompatible;
         cdoptions:=[cdo_parameter,cdo_check_operator,cdo_allow_variant];
         { we need to parse the list from left-right so the
           not-default parameters are checked first }
         lowesteq:=high(tequaltype);
         i1:=0;
         i2:=0;
         if cpo_ignorehidden in cpoptions then
           begin
             while (i1<para1.count) and
                   (vo_is_hidden_para in tparavarsym(para1[i1]).varoptions) do
               inc(i1);
             while (i2<para2.count) and
                   (vo_is_hidden_para in tparavarsym(para2[i2]).varoptions) do
               inc(i2);
           end;
         while (i1<para1.count) and (i2<para2.count) do
           begin
             eq:=te_incompatible;

             currpara1:=tparavarsym(para1[i1]);
             currpara2:=tparavarsym(para2[i2]);

             { Unique types must match exact }
             if ((df_unique in currpara1.vardef.defoptions) or (df_unique in currpara2.vardef.defoptions)) and
                (currpara1.vardef<>currpara2.vardef) then
               exit;

             { Handle hidden parameters separately, because self is
               defined as voidpointer for methodpointers }
             if (vo_is_hidden_para in currpara1.varoptions) or
                (vo_is_hidden_para in currpara2.varoptions) then
              begin
                { both must be hidden }
                if (vo_is_hidden_para in currpara1.varoptions)<>(vo_is_hidden_para in currpara2.varoptions) then
                  exit;
                eq:=te_exact;
                if not(vo_is_self in currpara1.varoptions) and
                   not(vo_is_self in currpara2.varoptions) then
                 begin
                   if (currpara1.varspez<>currpara2.varspez) then
                    exit;
                   eq:=compare_defs_ext(currpara1.vardef,currpara2.vardef,nothingn,
                                        convtype,hpd,cdoptions);
                 end;
              end
             else
              begin
                case acp of
                  cp_value_equal_const :
                    begin
                       if (
                           (currpara1.varspez<>currpara2.varspez) and
                           ((currpara1.varspez in [vs_var,vs_out]) or
                            (currpara2.varspez in [vs_var,vs_out]))
                          ) then
                         exit;
                       eq:=compare_defs_ext(currpara1.vardef,currpara2.vardef,nothingn,
                                            convtype,hpd,cdoptions);
                    end;
                  cp_all :
                    begin
                       if (currpara1.varspez<>currpara2.varspez) then
                         exit;
                       eq:=compare_defs_ext(currpara1.vardef,currpara2.vardef,nothingn,
                                            convtype,hpd,cdoptions);
                    end;
                  cp_procvar :
                    begin
                       if (currpara1.varspez<>currpara2.varspez) then
                         exit;
                       eq:=compare_defs_ext(currpara1.vardef,currpara2.vardef,nothingn,
                                            convtype,hpd,cdoptions);
                       { Parameters must be at least equal otherwise the are incompatible }
                       if (eq<te_equal) then
                         eq:=te_incompatible;
                    end;
                  else
                    eq:=compare_defs_ext(currpara1.vardef,currpara2.vardef,nothingn,
                                         convtype,hpd,cdoptions);
                 end;
               end;
              { check type }
              if eq=te_incompatible then
                exit;
              { open strings can never match exactly, since you cannot define }
              { a separate "open string" type -> we have to be able to        }
              { consider those as exact when resolving forward definitions.   }
              { The same goes for array of const. Open arrays are handled     }
              { already (if their element types match exactly, they are       }
              { considered to be an exact match)                              }
              { And also for "inline defined" function parameter definitions  }
              { (i.e., function types directly declared in a parameter list)  }
              if (is_array_of_const(currpara1.vardef) or
                  is_open_string(currpara1.vardef) or
                  ((currpara1.vardef.typ = procvardef) and
                   not(assigned(currpara1.vardef.typesym)))) and
                 (eq=te_equal) and
                 (cpo_openequalisexact in cpoptions) then
                eq:=te_exact;
              if eq<lowesteq then
                lowesteq:=eq;
              { also check default value if both have it declared }
              if (cpo_comparedefaultvalue in cpoptions) and
                 assigned(currpara1.defaultconstsym) and
                 assigned(currpara2.defaultconstsym) then
               begin
                 if not equal_constsym(tconstsym(currpara1.defaultconstsym),tconstsym(currpara2.defaultconstsym)) then
                   exit;
               end;
              inc(i1);
              inc(i2);
              if cpo_ignorehidden in cpoptions then
                begin
                  while (i1<para1.count) and
                        (vo_is_hidden_para in tparavarsym(para1[i1]).varoptions) do
                    inc(i1);
                  while (i2<para2.count) and
                        (vo_is_hidden_para in tparavarsym(para2[i2]).varoptions) do
                    inc(i2);
                end;
           end;
         { when both lists are empty then the parameters are equal. Also
           when one list is empty and the other has a parameter with default
           value assigned then the parameters are also equal }
         if ((i1>=para1.count) and (i2>=para2.count)) or
            ((cpo_allowdefaults in cpoptions) and
             (((i1<para1.count) and assigned(tparavarsym(para1[i1]).defaultconstsym)) or
              ((i2<para2.count) and assigned(tparavarsym(para2[i2]).defaultconstsym)))) then
           compare_paras:=lowesteq;
      end;


    function proc_to_procvar_equal(def1:tabstractprocdef;def2:tprocvardef):tequaltype;
      var
        eq : tequaltype;
        po_comp : tprocoptions;
      begin
         proc_to_procvar_equal:=te_incompatible;
         if not(assigned(def1)) or not(assigned(def2)) then
           exit;
         { check for method pointer }
         if (def1.is_methodpointer xor def2.is_methodpointer) or
            (def1.is_addressonly xor def2.is_addressonly) then
           exit;
         { check return value and options, methodpointer is already checked }
         po_comp:=[po_staticmethod,po_interrupt,
                   po_iocheck,po_varargs];
         if (m_delphi in current_settings.modeswitches) then
           exclude(po_comp,po_varargs);
         if (def1.proccalloption=def2.proccalloption) and
            ((po_comp * def1.procoptions)= (po_comp * def2.procoptions)) and
            equal_defs(def1.returndef,def2.returndef) then
          begin
            { return equal type based on the parameters, but a proc->procvar
              is never exact, so map an exact match of the parameters to
              te_equal }
            eq:=compare_paras(def1.paras,def2.paras,cp_procvar,[]);
            if eq=te_exact then
             eq:=te_equal;
            proc_to_procvar_equal:=eq;
          end;
      end;


    function compatible_childmethod_resultdef(parentretdef, childretdef: tdef): boolean;
      begin
        compatible_childmethod_resultdef :=
          (equal_defs(parentretdef,childretdef)) or
          ((parentretdef.typ=objectdef) and
           (childretdef.typ=objectdef) and
           is_class_or_interface(parentretdef) and
           is_class_or_interface(childretdef) and
           (tobjectdef(childretdef).is_related(tobjectdef(parentretdef))))
      end;


end.
