{* GStreamer
 * Copyright (C) 2013 Collabora Ltd.
 *   Author: Sebastian Dröge <sebastian.droege@collabora.co.uk>
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
    Pgchar  = ^Tgchar;
    Pgint  = ^Tgint;
    PGList  = ^TGList;
    PGQueue  = ^TGQueue;
    PGSList  = ^TGSList;
    PGstCapsFeatures  = ^TGstCapsFeatures;
    PGstCapsFeatures_autoptr  = ^TGstCapsFeatures_autoptr;
    PGstCapsFeatures_listautoptr  = ^TGstCapsFeatures_listautoptr;
    PGstCapsFeatures_queueautoptr  = ^TGstCapsFeatures_queueautoptr;
    PGstCapsFeatures_slistautoptr  = ^TGstCapsFeatures_slistautoptr;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  TGstCapsFeatures = record end;
  T_GstCapsFeatures = TGstCapsFeatures;
  TGstCapsFeatures_autoptr = PGstCapsFeatures;
  TGstCapsFeatures_listautoptr = PGList;
  TGstCapsFeatures_slistautoptr = PGSList;
  TGstCapsFeatures_queueautoptr = PGQueue;

    var
      _gst_caps_features_type : TGType;cvar;external;
      _gst_caps_features_any : PGstCapsFeatures;cvar;external;
      _gst_caps_features_memory_system_memory : PGstCapsFeatures;cvar;external;

  function gst_caps_features_get_type:TGType;cdecl;external gstreamerlib name 'gst_caps_features_get_type';

  function gst_is_caps_features(obj:Tgconstpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_is_caps_features';

  function gst_caps_features_new_empty:PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new_empty';

  function gst_caps_features_new_any:PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new_any';

(* Const before type ignored *)
  function gst_caps_features_new(feature1:Pgchar; args:array of const):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new';

  function gst_caps_features_new(feature1:Pgchar):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new';

(* Const before type ignored *)
  function gst_caps_features_new_valist(feature1:Pgchar; varargs:Tva_list):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new_valist';

  function gst_caps_features_new_id(feature1:TGQuark; args:array of const):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new_id';

  function gst_caps_features_new_id(feature1:TGQuark):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new_id';

  function gst_caps_features_new_id_valist(feature1:TGQuark; varargs:Tva_list):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_new_id_valist';

  function gst_caps_features_set_parent_refcount(features:PGstCapsFeatures; refcount:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_features_set_parent_refcount';

(* Const before type ignored *)
  function gst_caps_features_copy(features:PGstCapsFeatures):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_copy';

  procedure gst_caps_features_free(features:PGstCapsFeatures);cdecl;external gstreamerlib name 'gst_caps_features_free';

(* Const before type ignored *)
  function gst_caps_features_to_string(features:PGstCapsFeatures):Pgchar;cdecl;external gstreamerlib name 'gst_caps_features_to_string';

(* Const before type ignored *)
  function gst_caps_features_from_string(features:Pgchar):PGstCapsFeatures;cdecl;external gstreamerlib name 'gst_caps_features_from_string';

(* Const before type ignored *)
  function gst_caps_features_get_size(features:PGstCapsFeatures):Tguint;cdecl;external gstreamerlib name 'gst_caps_features_get_size';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_features_get_nth(features:PGstCapsFeatures; i:Tguint):Pgchar;cdecl;external gstreamerlib name 'gst_caps_features_get_nth';

(* Const before type ignored *)
  function gst_caps_features_get_nth_id(features:PGstCapsFeatures; i:Tguint):TGQuark;cdecl;external gstreamerlib name 'gst_caps_features_get_nth_id';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_features_contains(features:PGstCapsFeatures; feature:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_features_contains';

(* Const before type ignored *)
  function gst_caps_features_contains_id(features:PGstCapsFeatures; feature:TGQuark):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_features_contains_id';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_caps_features_is_equal(features1:PGstCapsFeatures; features2:PGstCapsFeatures):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_features_is_equal';

(* Const before type ignored *)
  function gst_caps_features_is_any(features:PGstCapsFeatures):Tgboolean;cdecl;external gstreamerlib name 'gst_caps_features_is_any';

(* Const before type ignored *)
  procedure gst_caps_features_add(features:PGstCapsFeatures; feature:Pgchar);cdecl;external gstreamerlib name 'gst_caps_features_add';

  procedure gst_caps_features_add_id(features:PGstCapsFeatures; feature:TGQuark);cdecl;external gstreamerlib name 'gst_caps_features_add_id';

(* Const before type ignored *)
  procedure gst_caps_features_remove(features:PGstCapsFeatures; feature:Pgchar);cdecl;external gstreamerlib name 'gst_caps_features_remove';

  procedure gst_caps_features_remove_id(features:PGstCapsFeatures; feature:TGQuark);cdecl;external gstreamerlib name 'gst_caps_features_remove_id';



