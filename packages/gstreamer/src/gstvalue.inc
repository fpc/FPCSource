{
/* GStreamer
 * Copyright (C) <2003> David A. Schleef <ds@schleef.org>
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
    P_GstValueTable  = ^T_GstValueTable;
    PGstValueSerializeFunc  = ^TGstValueSerializeFunc;
    PGstValueTable  = ^TGstValueTable;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    TGstValueCompareFunc = function (value1:PGValue; value2:PGValue):Tgint;cdecl;
(* Const before type ignored *)

    TGstValueSerializeFunc = function (value1:PGValue):Pgchar;cdecl;
(* Const before type ignored *)

    TGstValueDeserializeFunc = function (dest:PGValue; s:Pgchar):Tgboolean;cdecl;
    T_GstValueTable = record
        _type : TGType;
        compare : TGstValueCompareFunc;
        serialize : TGstValueSerializeFunc;
        deserialize : TGstValueDeserializeFunc;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstValueTable  = T_GstValueTable;


  function gst_int_range_get_type:TGType;cdecl;external gstreamerlib name 'gst_int_range_get_type';

  function gst_int64_range_get_type:TGType;cdecl;external gstreamerlib name 'gst_int64_range_get_type';

  function gst_double_range_get_type:TGType;cdecl;external gstreamerlib name 'gst_double_range_get_type';

  function gst_fraction_range_get_type:TGType;cdecl;external gstreamerlib name 'gst_fraction_range_get_type';

  function gst_fraction_get_type:TGType;cdecl;external gstreamerlib name 'gst_fraction_get_type';

  function gst_value_list_get_type:TGType;cdecl;external gstreamerlib name 'gst_value_list_get_type';

  function gst_value_array_get_type:TGType;cdecl;external gstreamerlib name 'gst_value_array_get_type';

  function gst_bitmask_get_type:TGType;cdecl;external gstreamerlib name 'gst_bitmask_get_type';

  function gst_flagset_get_type:TGType;cdecl;external gstreamerlib name 'gst_flagset_get_type';

  function gst_g_thread_get_type:TGType;cdecl;external gstreamerlib name 'gst_g_thread_get_type';


  procedure gst_value_register(table:PGstValueTable);cdecl;external gstreamerlib name 'gst_value_register';


  procedure gst_value_init_and_copy(dest:PGValue; src:PGValue);cdecl;external gstreamerlib name 'gst_value_init_and_copy';


  function gst_value_serialize(value:PGValue):Pgchar;cdecl;external gstreamerlib name 'gst_value_serialize';


  function gst_value_deserialize(dest:PGValue; src:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_value_deserialize';


  procedure gst_value_list_append_value(value:PGValue; append_value:PGValue);cdecl;external gstreamerlib name 'gst_value_list_append_value';

  procedure gst_value_list_append_and_take_value(value:PGValue; append_value:PGValue);cdecl;external gstreamerlib name 'gst_value_list_append_and_take_value';


  procedure gst_value_list_prepend_value(value:PGValue; prepend_value:PGValue);cdecl;external gstreamerlib name 'gst_value_list_prepend_value';



  procedure gst_value_list_concat(dest:PGValue; value1:PGValue; value2:PGValue);cdecl;external gstreamerlib name 'gst_value_list_concat';



  procedure gst_value_list_merge(dest:PGValue; value1:PGValue; value2:PGValue);cdecl;external gstreamerlib name 'gst_value_list_merge';


  function gst_value_list_get_size(value:PGValue):Tguint;cdecl;external gstreamerlib name 'gst_value_list_get_size';



  function gst_value_list_get_value(value:PGValue; index:Tguint):PGValue;cdecl;external gstreamerlib name 'gst_value_list_get_value';


  procedure gst_value_array_append_value(value:PGValue; append_value:PGValue);cdecl;external gstreamerlib name 'gst_value_array_append_value';

  procedure gst_value_array_append_and_take_value(value:PGValue; append_value:PGValue);cdecl;external gstreamerlib name 'gst_value_array_append_and_take_value';


  procedure gst_value_array_prepend_value(value:PGValue; prepend_value:PGValue);cdecl;external gstreamerlib name 'gst_value_array_prepend_value';


  function gst_value_array_get_size(value:PGValue):Tguint;cdecl;external gstreamerlib name 'gst_value_array_get_size';



  function gst_value_array_get_value(value:PGValue; index:Tguint):PGValue;cdecl;external gstreamerlib name 'gst_value_array_get_value';

  procedure gst_value_set_int_range(value:PGValue; start:Tgint; end_:Tgint);cdecl;external gstreamerlib name 'gst_value_set_int_range';

  procedure gst_value_set_int_range_step(value:PGValue; start:Tgint; end_:Tgint; step:Tgint);cdecl;external gstreamerlib name 'gst_value_set_int_range_step';


  function gst_value_get_int_range_min(value:PGValue):Tgint;cdecl;external gstreamerlib name 'gst_value_get_int_range_min';


  function gst_value_get_int_range_max(value:PGValue):Tgint;cdecl;external gstreamerlib name 'gst_value_get_int_range_max';


  function gst_value_get_int_range_step(value:PGValue):Tgint;cdecl;external gstreamerlib name 'gst_value_get_int_range_step';

  procedure gst_value_set_int64_range(value:PGValue; start:Tgint64; end_:Tgint64);cdecl;external gstreamerlib name 'gst_value_set_int64_range';

  procedure gst_value_set_int64_range_step(value:PGValue; start:Tgint64; end_:Tgint64; step:Tgint64);cdecl;external gstreamerlib name 'gst_value_set_int64_range_step';


  function gst_value_get_int64_range_min(value:PGValue):Tgint64;cdecl;external gstreamerlib name 'gst_value_get_int64_range_min';


  function gst_value_get_int64_range_max(value:PGValue):Tgint64;cdecl;external gstreamerlib name 'gst_value_get_int64_range_max';


  function gst_value_get_int64_range_step(value:PGValue):Tgint64;cdecl;external gstreamerlib name 'gst_value_get_int64_range_step';

  procedure gst_value_set_double_range(value:PGValue; start:Tgdouble; end_:Tgdouble);cdecl;external gstreamerlib name 'gst_value_set_double_range';


  function gst_value_get_double_range_min(value:PGValue):Tgdouble;cdecl;external gstreamerlib name 'gst_value_get_double_range_min';


  function gst_value_get_double_range_max(value:PGValue):Tgdouble;cdecl;external gstreamerlib name 'gst_value_get_double_range_max';



  function gst_value_get_caps(value:PGValue):PGstCaps;cdecl;external gstreamerlib name 'gst_value_get_caps';


  procedure gst_value_set_caps(value:PGValue; caps:PGstCaps);cdecl;external gstreamerlib name 'gst_value_set_caps';



  function gst_value_get_structure(value:PGValue):PGstStructure;cdecl;external gstreamerlib name 'gst_value_get_structure';


  procedure gst_value_set_structure(value:PGValue; structure:PGstStructure);cdecl;external gstreamerlib name 'gst_value_set_structure';



  function gst_value_get_caps_features(value:PGValue):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_value_get_caps_features';


  procedure gst_value_set_caps_features(value:PGValue; features:PGstCapsFeatures);cdecl;external gstreamerlib name 'gst_value_set_caps_features';

  procedure gst_value_set_fraction(value:PGValue; numerator:Tgint; denominator:Tgint);cdecl;external gstreamerlib name 'gst_value_set_fraction';


  function gst_value_get_fraction_numerator(value:PGValue):Tgint;cdecl;external gstreamerlib name 'gst_value_get_fraction_numerator';


  function gst_value_get_fraction_denominator(value:PGValue):Tgint;cdecl;external gstreamerlib name 'gst_value_get_fraction_denominator';



  function gst_value_fraction_multiply(product:PGValue; factor1:PGValue; factor2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_fraction_multiply';



  function gst_value_fraction_subtract(dest:PGValue; minuend:PGValue; subtrahend:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_fraction_subtract';



  procedure gst_value_set_fraction_range(value:PGValue; start:PGValue; end_:PGValue);cdecl;external gstreamerlib name 'gst_value_set_fraction_range';

  procedure gst_value_set_fraction_range_full(value:PGValue; numerator_start:Tgint; denominator_start:Tgint; numerator_end:Tgint; denominator_end:Tgint);cdecl;external gstreamerlib name 'gst_value_set_fraction_range_full';



  function gst_value_get_fraction_range_min(value:PGValue):PGValue;cdecl;external gstreamerlib name 'gst_value_get_fraction_range_min';



  function gst_value_get_fraction_range_max(value:PGValue):PGValue;cdecl;external gstreamerlib name 'gst_value_get_fraction_range_max';


  function gst_value_get_bitmask(value:PGValue):Tguint64;cdecl;external gstreamerlib name 'gst_value_get_bitmask';

  procedure gst_value_set_bitmask(value:PGValue; bitmask:Tguint64);cdecl;external gstreamerlib name 'gst_value_set_bitmask';

  procedure gst_value_set_flagset(value:PGValue; flags:Tguint; mask:Tguint);cdecl;external gstreamerlib name 'gst_value_set_flagset';


  function gst_value_get_flagset_flags(value:PGValue):Tguint;cdecl;external gstreamerlib name 'gst_value_get_flagset_flags';


  function gst_value_get_flagset_mask(value:PGValue):Tguint;cdecl;external gstreamerlib name 'gst_value_get_flagset_mask';



  function gst_value_compare(value1:PGValue; value2:PGValue):Tgint;cdecl;external gstreamerlib name 'gst_value_compare';



  function gst_value_can_compare(value1:PGValue; value2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_can_compare';



  function gst_value_is_subset(value1:PGValue; value2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_is_subset';



  function gst_value_union(dest:PGValue; value1:PGValue; value2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_union';



  function gst_value_can_union(value1:PGValue; value2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_can_union';



  function gst_value_intersect(dest:PGValue; value1:PGValue; value2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_intersect';



  function gst_value_can_intersect(value1:PGValue; value2:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_can_intersect';



  function gst_value_subtract(dest:PGValue; minuend:PGValue; subtrahend:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_subtract';



  function gst_value_can_subtract(minuend:PGValue; subtrahend:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_can_subtract';


  function gst_value_is_fixed(value:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_is_fixed';


  function gst_value_fixate(dest:PGValue; src:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_value_fixate';

  function gst_flagset_register(flags_type:TGType):TGType;cdecl;external gstreamerlib name 'gst_flagset_register';

      var
      _gst_int_range_type : TGType;cvar;external;
      _gst_int64_range_type : TGType;cvar;external;
      _gst_double_range_type : TGType;cvar;external;
      _gst_fraction_range_type : TGType;cvar;external;
      _gst_value_list_type : TGType;cvar;external;
      _gst_value_array_type : TGType;cvar;external;
      _gst_fraction_type : TGType;cvar;external;
      _gst_bitmask_type : TGType;cvar;external;
      _gst_flagset_type : TGType;cvar;external;



