{
/* GStreamer
 * Copyright (C) 2003 David A. Schleef <ds@schleef.org>
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 51 Franklin St, Fifth Floor,
 * Boston, MA 02110-1301, USA.
 */

}
  Type
    P_GstStructure  = ^T_GstStructure;
    Pgboolean  = ^Tgboolean;
    PGDate  = ^TGDate;

//    PGstClockTime  = ^TGstClockTime;
//    PGstDateTime  = ^TGstDateTime;
    PGstStructure  = ^TGstStructure;
    PPGstStructure  = ^PGstStructure;
    PGstStructure_autoptr  = ^TGstStructure_autoptr;
    PGstStructure_listautoptr  = ^TGstStructure_listautoptr;
    PGstStructure_queueautoptr  = ^TGstStructure_queueautoptr;
    PGstStructure_slistautoptr  = ^TGstStructure_slistautoptr;
    Pguint  = ^Tguint;
    Pguint64  = ^Tguint64;
    PGValue  = ^TGValue;
    PGValueArray  = ^TGValueArray;
    PGstClockTime_ = Pointer;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


   PPGValueArray_ = Pointer;


(* Const before type ignored *)

    TGstStructureForeachFunc = function (field_id:TGQuark; value:PGValue; user_data:Tgpointer):Tgboolean;cdecl;

    TGstStructureMapFunc = function (field_id:TGQuark; value:PGValue; user_data:Tgpointer):Tgboolean;cdecl;

    TGstStructureFilterMapFunc = function (field_id:TGQuark; value:PGValue; user_data:Tgpointer):Tgboolean;cdecl;
    T_GstStructure = record
        _type : TGType;
        name : TGQuark;
      end;
    TGstStructure = T_GstStructure;

  TGstStructure_autoptr = PGstStructure;
  TGstStructure_listautoptr = PGList;
  TGstStructure_slistautoptr = PGSList;
  TGstStructure_queueautoptr = PGQueue;


  function gst_structure_get_type:TGType;cdecl;external gstreamerlib name 'gst_structure_get_type';

(* Const before type ignored *)
  function gst_structure_new_empty(name:Pgchar):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new_empty';

  function gst_structure_new_id_empty(quark:TGQuark):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new_id_empty';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_new(name:Pgchar; firstfield:Pgchar; args:array of const):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new';

  function gst_structure_new(name:Pgchar; firstfield:Pgchar):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_new_valist(name:Pgchar; firstfield:Pgchar; varargs:Tva_list):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new_valist';

  function gst_structure_new_id(name_quark:TGQuark; field_quark:TGQuark; args:array of const):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new_id';

  function gst_structure_new_id(name_quark:TGQuark; field_quark:TGQuark):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new_id';

(* Const before type ignored *)
  function gst_structure_new_from_string(_string:Pgchar):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_new_from_string';

(* Const before type ignored *)
  function gst_structure_copy(structure:PGstStructure):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_copy';

  function gst_structure_set_parent_refcount(structure:PGstStructure; refcount:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_set_parent_refcount';

  procedure gst_structure_free(structure:PGstStructure);cdecl;external gstreamerlib name 'gst_structure_free';

  procedure gst_clear_structure(structure_ptr:PPGstStructure);cdecl;external gstreamerlib name 'gst_clear_structure';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_name(structure:PGstStructure):Pgchar;cdecl;external gstreamerlib name 'gst_structure_get_name';

(* Const before type ignored *)
  function gst_structure_get_name_id(structure:PGstStructure):TGQuark;cdecl;external gstreamerlib name 'gst_structure_get_name_id';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_has_name(structure:PGstStructure; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_has_name';

(* Const before type ignored *)
  procedure gst_structure_set_name(structure:PGstStructure; name:Pgchar);cdecl;external gstreamerlib name 'gst_structure_set_name';

(* Const before type ignored *)
  procedure gst_structure_id_set_value(structure:PGstStructure; field:TGQuark; value:PGValue);cdecl;external gstreamerlib name 'gst_structure_id_set_value';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_structure_set_value(structure:PGstStructure; fieldname:Pgchar; value:PGValue);cdecl;external gstreamerlib name 'gst_structure_set_value';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_structure_set_array(structure:PGstStructure; fieldname:Pgchar; array_:PGValueArray);cdecl;external gstreamerlib name 'gst_structure_set_array';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_structure_set_list(structure:PGstStructure; fieldname:Pgchar; array_:PGValueArray);cdecl;external gstreamerlib name 'gst_structure_set_list';

  procedure gst_structure_id_take_value(structure:PGstStructure; field:TGQuark; value:PGValue);cdecl;external gstreamerlib name 'gst_structure_id_take_value';

(* Const before type ignored *)
  procedure gst_structure_take_value(structure:PGstStructure; fieldname:Pgchar; value:PGValue);cdecl;external gstreamerlib name 'gst_structure_take_value';

(* Const before type ignored *)
  procedure gst_structure_set(structure:PGstStructure; fieldname:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_structure_set';

  procedure gst_structure_set(structure:PGstStructure; fieldname:Pgchar);cdecl;external gstreamerlib name 'gst_structure_set';

(* Const before type ignored *)
  procedure gst_structure_set_valist(structure:PGstStructure; fieldname:Pgchar; varargs:Tva_list);cdecl;external gstreamerlib name 'gst_structure_set_valist';

  procedure gst_structure_id_set(structure:PGstStructure; fieldname:TGQuark; args:array of const);cdecl;external gstreamerlib name 'gst_structure_id_set';

  procedure gst_structure_id_set(structure:PGstStructure; fieldname:TGQuark);cdecl;external gstreamerlib name 'gst_structure_id_set';

  procedure gst_structure_id_set_valist(structure:PGstStructure; fieldname:TGQuark; varargs:Tva_list);cdecl;external gstreamerlib name 'gst_structure_id_set_valist';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_valist(structure:PGstStructure; first_fieldname:Pchar; args:Tva_list):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_valist';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get(structure:PGstStructure; first_fieldname:Pchar; args:array of const):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get';

  function gst_structure_get(structure:PGstStructure; first_fieldname:Pchar):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get';

(* Const before type ignored *)
  function gst_structure_id_get_valist(structure:PGstStructure; first_field_id:TGQuark; args:Tva_list):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_id_get_valist';

(* Const before type ignored *)
  function gst_structure_id_get(structure:PGstStructure; first_field_id:TGQuark; args:array of const):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_id_get';

  function gst_structure_id_get(structure:PGstStructure; first_field_id:TGQuark):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_id_get';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_id_get_value(structure:PGstStructure; field:TGQuark):PGValue;cdecl;external gstreamerlib name 'gst_structure_id_get_value';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_value(structure:PGstStructure; fieldname:Pgchar):PGValue;cdecl;external gstreamerlib name 'gst_structure_get_value';

(* Const before type ignored *)
  procedure gst_structure_remove_field(structure:PGstStructure; fieldname:Pgchar);cdecl;external gstreamerlib name 'gst_structure_remove_field';

(* Const before type ignored *)
  procedure gst_structure_remove_fields(structure:PGstStructure; fieldname:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_structure_remove_fields';

  procedure gst_structure_remove_fields(structure:PGstStructure; fieldname:Pgchar);cdecl;external gstreamerlib name 'gst_structure_remove_fields';

(* Const before type ignored *)
  procedure gst_structure_remove_fields_valist(structure:PGstStructure; fieldname:Pgchar; varargs:Tva_list);cdecl;external gstreamerlib name 'gst_structure_remove_fields_valist';

  procedure gst_structure_remove_all_fields(structure:PGstStructure);cdecl;external gstreamerlib name 'gst_structure_remove_all_fields';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_field_type(structure:PGstStructure; fieldname:Pgchar):TGType;cdecl;external gstreamerlib name 'gst_structure_get_field_type';

(* Const before type ignored *)
  function gst_structure_foreach(structure:PGstStructure; func:TGstStructureForeachFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_foreach';

  function gst_structure_map_in_place(structure:PGstStructure; func:TGstStructureMapFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_map_in_place';

  procedure gst_structure_filter_and_map_in_place(structure:PGstStructure; func:TGstStructureFilterMapFunc; user_data:Tgpointer);cdecl;external gstreamerlib name 'gst_structure_filter_and_map_in_place';

(* Const before type ignored *)
  function gst_structure_n_fields(structure:PGstStructure):Tgint;cdecl;external gstreamerlib name 'gst_structure_n_fields';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_nth_field_name(structure:PGstStructure; index:Tguint):Pgchar;cdecl;external gstreamerlib name 'gst_structure_nth_field_name';

(* Const before type ignored *)
  function gst_structure_id_has_field(structure:PGstStructure; field:TGQuark):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_id_has_field';

(* Const before type ignored *)
  function gst_structure_id_has_field_typed(structure:PGstStructure; field:TGQuark; _type:TGType):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_id_has_field_typed';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_has_field(structure:PGstStructure; fieldname:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_has_field';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_has_field_typed(structure:PGstStructure; fieldname:Pgchar; _type:TGType):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_has_field_typed';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_boolean(structure:PGstStructure; fieldname:Pgchar; value:Pgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_boolean';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_int(structure:PGstStructure; fieldname:Pgchar; value:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_int';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_uint(structure:PGstStructure; fieldname:Pgchar; value:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_uint';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_int64(structure:PGstStructure; fieldname:Pgchar; value:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_int64';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_uint64(structure:PGstStructure; fieldname:Pgchar; value:Pguint64):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_uint64';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_double(structure:PGstStructure; fieldname:Pgchar; value:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_double';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_date(structure:PGstStructure; fieldname:Pgchar; value:PPGDate):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_date';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_date_time(structure:PGstStructure; fieldname:Pgchar; value:PPGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_date_time';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_clock_time(structure:PGstStructure; fieldname:Pgchar; value:PGstClockTime_):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_clock_time';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_string(structure:PGstStructure; fieldname:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_structure_get_string';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_enum(structure:PGstStructure; fieldname:Pgchar; enumtype:TGType; value:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_enum';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_fraction(structure:PGstStructure; fieldname:Pgchar; value_numerator:Pgint; value_denominator:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_fraction';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_get_flagset(structure:PGstStructure; fieldname:Pgchar; value_flags:Pguint; value_mask:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_flagset';

(* Const before type ignored *)
  function gst_structure_get_array(structure:PGstStructure; fieldname:Pgchar; array_:PPGValueArray_):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_array';

(* Const before type ignored *)
  function gst_structure_get_list(structure:PGstStructure; fieldname:Pgchar; array_:PPGValueArray_):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_get_list';

(* Const before type ignored *)
  function gst_structure_to_string(structure:PGstStructure):Pgchar;cdecl;external gstreamerlib name 'gst_structure_to_string';

(* Const before type ignored *)
  function gst_structure_from_string(_string:Pgchar; end_:PPgchar):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_from_string';

(* Const before type ignored *)
  function gst_structure_fixate_field_nearest_int(structure:PGstStructure; field_name:Pchar; target:longint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_fixate_field_nearest_int';

(* Const before type ignored *)
  function gst_structure_fixate_field_nearest_double(structure:PGstStructure; field_name:Pchar; target:double):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_fixate_field_nearest_double';

(* Const before type ignored *)
  function gst_structure_fixate_field_boolean(structure:PGstStructure; field_name:Pchar; target:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_fixate_field_boolean';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_fixate_field_string(structure:PGstStructure; field_name:Pchar; target:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_fixate_field_string';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_fixate_field_nearest_fraction(structure:PGstStructure; field_name:Pchar; target_numerator:Tgint; target_denominator:Tgint):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_fixate_field_nearest_fraction';

(* Const before type ignored *)
  function gst_structure_fixate_field(structure:PGstStructure; field_name:Pchar):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_fixate_field';

  procedure gst_structure_fixate(structure:PGstStructure);cdecl;external gstreamerlib name 'gst_structure_fixate';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_is_equal(structure1:PGstStructure; structure2:PGstStructure):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_is_equal';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_is_subset(subset:PGstStructure; superset:PGstStructure):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_is_subset';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_can_intersect(struct1:PGstStructure; struct2:PGstStructure):Tgboolean;cdecl;external gstreamerlib name 'gst_structure_can_intersect';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_structure_intersect(struct1:PGstStructure; struct2:PGstStructure):PGstStructure;cdecl;external gstreamerlib name 'gst_structure_intersect';


  var
    _gst_structure_type : TGType;cvar;external;

