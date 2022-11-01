{* GStreamer
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
 *}

  Type
    P_GstCaps  = ^T_GstCaps;
    P_GstStaticCaps  = ^T_GstStaticCaps;
    PGstCaps_autoptr  = ^TGstCaps_autoptr;
    PGstCaps_listautoptr  = ^TGstCaps_listautoptr;
    PGstCaps_queueautoptr  = ^TGstCaps_queueautoptr;
    PGstCaps_slistautoptr  = ^TGstCaps_slistautoptr;
    PGstCapsFlags  = ^TGstCapsFlags;
    PGstCapsIntersectMode  = ^TGstCapsIntersectMode;
    PGstStaticCaps  = ^TGstStaticCaps;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  T_GstCaps = record
      mini_object : TGstMiniObject;
    end;
  TGstCaps = T_GstCaps;
  PGstCaps = ^TGstCaps;
  PPGstCaps = ^PGstCaps;

(* Const before type ignored *)
  T_GstStaticCaps = record
      caps : PGstCaps;
      _string : Pchar;
      _gst_reserved : array[0..3] of Tgpointer;
    end;
  TGstStaticCaps = T_GstStaticCaps;
  TGstCapsForeachFunc = function (features:PGstCapsFeatures; structure:PGstStructure; user_data:Tgpointer):Tgboolean;cdecl;
  TGstCapsMapFunc = function (features:PGstCapsFeatures; structure:PGstStructure; user_data:Tgpointer):Tgboolean;cdecl;
  TGstCapsFilterMapFunc = function (features:PGstCapsFeatures; structure:PGstStructure; user_data:Tgpointer):Tgboolean;cdecl;

  TGstCaps_autoptr = PGstCaps;
  TGstCaps_listautoptr = PGList;
  TGstCaps_slistautoptr = PGSList;
  TGstCaps_queueautoptr = PGQueue;

  {
  
  static inline GstCaps *
  gst_caps_ref (GstCaps * caps)
  
    return (GstCaps *) gst_mini_object_ref (((GstMiniObject*)(caps)));
  
  # 220 "gstcaps.h"
  
  static inline void
  gst_caps_unref (GstCaps * caps)
  
    gst_mini_object_unref (((GstMiniObject*)(caps)));
  
  # 239 "gstcaps.h"
  static inline void
  gst_clear_caps (GstCaps ** caps_ptr)
  
    do  _Static_assert (sizeof *(((GstMiniObject **) caps_ptr)) == sizeof (gpointer), "Expression evaluates to false"); __typeof__((((GstMiniObject **) caps_ptr))) _pp = (((GstMiniObject **) caps_ptr)); __typeof__(*(((GstMiniObject **) caps_ptr))) _ptr = *_pp; *_pp = 
  # 242 "gstcaps.h" 3 4
   ((void *)0)
  # 242 "gstcaps.h"
   ; if (_ptr) (gst_mini_object_unref) (_ptr);  while (0) ;
  
   }
(* Const before type ignored *)

  function gst_caps_copy(caps:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_copy';

  {
  static inline gboolean
  gst_caps_replace (GstCaps **old_caps, GstCaps *new_caps)
  
      return gst_mini_object_replace ((GstMiniObject **) old_caps, (GstMiniObject *) new_caps);
  
  # 316 "gstcaps.h"
  static inline gboolean
  gst_caps_take (GstCaps **old_caps, GstCaps *new_caps)
  
      return gst_mini_object_take ((GstMiniObject **) old_caps, (GstMiniObject *) new_caps);
  
  
   }


  function gst_caps_get_type:TGType;cdecl;external gstreamerlib name 'gst_caps_get_type';

  function gst_caps_new_empty:PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_empty';

  function gst_caps_new_any:PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_any';

(* Const before type ignored *)
  function gst_caps_new_empty_simple(media_type:Pchar):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_empty_simple';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_new_simple(media_type:Pchar; fieldname:Pchar; args:array of const):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_simple';

  function gst_caps_new_simple(media_type:Pchar; fieldname:Pchar):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_simple';

  function gst_caps_new_full(struct1:PGstStructure; args:array of const):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_full';

  function gst_caps_new_full(struct1:PGstStructure):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_full';

  function gst_caps_new_full_valist(structure:PGstStructure; var_args:Tva_list):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_new_full_valist';

  function gst_static_caps_get_type:TGType;cdecl;external gstreamerlib name 'gst_static_caps_get_type';

  function gst_static_caps_get(static_caps:PGstStaticCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_static_caps_get';

  procedure gst_static_caps_cleanup(static_caps:PGstStaticCaps);cdecl;external gstreamerlib name 'gst_static_caps_cleanup';

  procedure gst_caps_append(caps1:PGstCaps; caps2:PGstCaps);cdecl;external gstreamerlib name 'gst_caps_append';

  procedure gst_caps_append_structure(caps:PGstCaps; structure:PGstStructure);cdecl;external gstreamerlib name 'gst_caps_append_structure';

  procedure gst_caps_append_structure_full(caps:PGstCaps; structure:PGstStructure; features:PGstCapsFeatures);cdecl;external gstreamerlib name 'gst_caps_append_structure_full';

  procedure gst_caps_remove_structure(caps:PGstCaps; idx:Tguint);cdecl;external gstreamerlib name 'gst_caps_remove_structure';

  function gst_caps_merge(caps1:PGstCaps; caps2:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_merge';

  function gst_caps_merge_structure(caps:PGstCaps; structure:PGstStructure):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_merge_structure';

  function gst_caps_merge_structure_full(caps:PGstCaps; structure:PGstStructure; features:PGstCapsFeatures):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_merge_structure_full';

(* Const before type ignored *)
  function gst_caps_get_size(caps:PGstCaps):Tguint;cdecl;external gstreamerlib name 'gst_caps_get_size';

(* Const before type ignored *)
  function gst_caps_get_structure(caps:PGstCaps; index:Tguint):PGstStructure;cdecl;external gstreamerlib name 'gst_caps_get_structure';

  function gst_caps_steal_structure(caps:PGstCaps; index:Tguint):PGstStructure;cdecl;external gstreamerlib name 'gst_caps_steal_structure';

  procedure gst_caps_set_features(caps:PGstCaps; index:Tguint; features:PGstCapsFeatures);cdecl;external gstreamerlib name 'gst_caps_set_features';

  procedure gst_caps_set_features_simple(caps:PGstCaps; features:PGstCapsFeatures);cdecl;external gstreamerlib name 'gst_caps_set_features_simple';

(* Const before type ignored *)
  function gst_caps_get_features(caps:PGstCaps; index:Tguint):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_get_features';

(* Const before type ignored *)
  function gst_caps_copy_nth(caps:PGstCaps; nth:Tguint):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_copy_nth';

  function gst_caps_truncate(caps:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_truncate';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_caps_set_value(caps:PGstCaps; field:Pchar; value:PGValue);cdecl;external gstreamerlib name 'gst_caps_set_value';

(* Const before type ignored *)
  procedure gst_caps_set_simple(caps:PGstCaps; field:Pchar; args:array of const);cdecl;external gstreamerlib name 'gst_caps_set_simple';

  procedure gst_caps_set_simple(caps:PGstCaps; field:Pchar);cdecl;external gstreamerlib name 'gst_caps_set_simple';

(* Const before type ignored *)
  procedure gst_caps_set_simple_valist(caps:PGstCaps; field:Pchar; varargs:Tva_list);cdecl;external gstreamerlib name 'gst_caps_set_simple_valist';

(* Const before type ignored *)
  function gst_caps_foreach(caps:PGstCaps; func:TGstCapsForeachFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_foreach';

  function gst_caps_map_in_place(caps:PGstCaps; func:TGstCapsMapFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_map_in_place';

  procedure gst_caps_filter_and_map_in_place(caps:PGstCaps; func:TGstCapsFilterMapFunc; user_data:Tgpointer);cdecl;external gstreamerlib name 'gst_caps_filter_and_map_in_place';

(* Const before type ignored *)
  function gst_caps_is_any(caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_any';

(* Const before type ignored *)
  function gst_caps_is_empty(caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_empty';

(* Const before type ignored *)
  function gst_caps_is_fixed(caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_fixed';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_always_compatible(caps1:PGstCaps; caps2:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_always_compatible';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_subset(subset:PGstCaps; superset:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_subset';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_subset_structure(caps:PGstCaps; structure:PGstStructure):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_subset_structure';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_subset_structure_full(caps:PGstCaps; structure:PGstStructure; features:PGstCapsFeatures):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_subset_structure_full';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_equal(caps1:PGstCaps; caps2:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_equal';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_equal_fixed(caps1:PGstCaps; caps2:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_equal_fixed';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_can_intersect(caps1:PGstCaps; caps2:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_can_intersect';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_is_strictly_equal(caps1:PGstCaps; caps2:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_is_strictly_equal';

  function gst_caps_intersect(caps1:PGstCaps; caps2:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_intersect';

  function gst_caps_intersect_full(caps1:PGstCaps; caps2:PGstCaps; mode:TGstCapsIntersectMode):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_intersect_full';

  function gst_caps_subtract(minuend:PGstCaps; subtrahend:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_subtract';

  function gst_caps_normalize(caps:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_normalize';

  function gst_caps_simplify(caps:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_simplify';

  function gst_caps_fixate(caps:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_fixate';

(* Const before type ignored *)
  function gst_caps_to_string(caps:PGstCaps):Pgchar;cdecl;external gstreamerlib name 'gst_caps_to_string';

(* Const before type ignored *)
  function gst_caps_from_string(_string:Pgchar):PGstCaps;cdecl;external gstreamerlib name 'gst_caps_from_string';


  var
    _gst_caps_type : TGType;cvar;external;
    _gst_caps_any : PGstCaps;cvar;external;
    _gst_caps_none : PGstCaps;cvar;external;

