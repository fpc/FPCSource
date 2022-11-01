{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *                    2005 Wim Taymans <wim@fluendo.com>
 *
 * gstobject.h: Header for base GstObject
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
    PGstObject_autoptr  = ^TGstObject_autoptr;
    PGstObject_listautoptr  = ^TGstObject_listautoptr;
    PGstObject_queueautoptr  = ^TGstObject_queueautoptr;
    PGstObject_slistautoptr  = ^TGstObject_slistautoptr;

    TGstObject_autoptr = PGstObject;
    TGstObject_listautoptr = PGList;
    TGstObject_slistautoptr = PGSList;
    TGstObject_queueautoptr = PGQueue;

  PGstObjectFlags = ^TGstObjectFlags;
  TGstObjectFlags = (
    GST_OBJECT_FLAG_MAY_BE_LEAKED := 1 shl 0,
    GST_OBJECT_FLAG_LAST_ := 1 shl 4
  );
  // T_GstObject moved to gstrec.inc
  // TGstObject moved to gstrec.inc
  // P_GstObject moved to gstrec.inc


(* Const before type ignored *)



function gst_object_get_type:TGType;cdecl;external gstreamerlib name 'gst_object_get_type';

(* Const before type ignored *)
function gst_object_set_name(object_:PGstObject; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_object_set_name';

function gst_object_get_name(object_:PGstObject):Pgchar;cdecl;external gstreamerlib name 'gst_object_get_name';

function gst_object_set_parent(object_:PGstObject; parent:PGstObject):Tgboolean;cdecl;external gstreamerlib name 'gst_object_set_parent';

function gst_object_get_parent(object_:PGstObject):PGstObject;cdecl;external gstreamerlib name 'gst_object_get_parent';

procedure gst_object_unparent(object_:PGstObject);cdecl;external gstreamerlib name 'gst_object_unparent';

function gst_object_has_as_parent(object_:PGstObject; parent:PGstObject):Tgboolean;cdecl;external gstreamerlib name 'gst_object_has_as_parent';

function gst_object_has_as_ancestor(object_:PGstObject; ancestor:PGstObject):Tgboolean;cdecl;external gstreamerlib name 'gst_object_has_as_ancestor';

function gst_object_has_ancestor(object_:PGstObject; ancestor:PGstObject):Tgboolean;cdecl;external gstreamerlib name 'gst_object_has_ancestor';

procedure gst_object_default_deep_notify(object_:PGObject; orig:PGstObject; pspec:PGParamSpec; excluded_props:PPgchar);cdecl;external gstreamerlib name 'gst_object_default_deep_notify';

function gst_object_ref(object_:Tgpointer):Tgpointer;cdecl;external gstreamerlib name 'gst_object_ref';

procedure gst_object_unref(object_:Tgpointer);cdecl;external gstreamerlib name 'gst_object_unref';

procedure gst_clear_object(object_ptr:PPGstObject);cdecl;external gstreamerlib name 'gst_clear_object';

function gst_object_ref_sink(object_:Tgpointer):Tgpointer;cdecl;external gstreamerlib name 'gst_object_ref_sink';

function gst_object_replace(oldobj:PPGstObject; newobj:PGstObject):Tgboolean;cdecl;external gstreamerlib name 'gst_object_replace';

function gst_object_get_path_string(object_:PGstObject):Pgchar;cdecl;external gstreamerlib name 'gst_object_get_path_string';

(* Const before type ignored *)
function gst_object_check_uniqueness(list:PGList; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_object_check_uniqueness';




  function gst_object_suggest_next_sync(object_:PGstObject):TGstClockTime;cdecl;external gstreamerlib name 'gst_object_suggest_next_sync';

  function gst_object_sync_values(object_:PGstObject; timestamp:TGstClockTime):Tgboolean;cdecl;external gstreamerlib name 'gst_object_sync_values';

  function gst_object_has_active_control_bindings(object_:PGstObject):Tgboolean;cdecl;external gstreamerlib name 'gst_object_has_active_control_bindings';

  procedure gst_object_set_control_bindings_disabled(object_:PGstObject; disabled:Tgboolean);cdecl;external gstreamerlib name 'gst_object_set_control_bindings_disabled';

(* Const before type ignored *)
  procedure gst_object_set_control_binding_disabled(object_:PGstObject; property_name:Pgchar; disabled:Tgboolean);cdecl;external gstreamerlib name 'gst_object_set_control_binding_disabled';

  function gst_object_add_control_binding(object_:PGstObject; binding:PGstControlBinding):Tgboolean;cdecl;external gstreamerlib name 'gst_object_add_control_binding';

(* Const before type ignored *)
  function gst_object_get_control_binding(object_:PGstObject; property_name:Pgchar):PGstControlBinding;cdecl;external gstreamerlib name 'gst_object_get_control_binding';

  function gst_object_remove_control_binding(object_:PGstObject; binding:PGstControlBinding):Tgboolean;cdecl;external gstreamerlib name 'gst_object_remove_control_binding';

(* Const before type ignored *)
  function gst_object_get_value(object_:PGstObject; property_name:Pgchar; timestamp:TGstClockTime):PGValue;cdecl;external gstreamerlib name 'gst_object_get_value';

(* Const before type ignored *)
  function gst_object_get_value_array(object_:PGstObject; property_name:Pgchar; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; 
             values:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_object_get_value_array';

(* Const before type ignored *)
  function gst_object_get_g_value_array(object_:PGstObject; property_name:Pgchar; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; 
             values:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_object_get_g_value_array';

  function gst_object_get_control_rate(object_:PGstObject):TGstClockTime;cdecl;external gstreamerlib name 'gst_object_get_control_rate';

  procedure gst_object_set_control_rate(object_:PGstObject; control_rate:TGstClockTime);cdecl;external gstreamerlib name 'gst_object_set_control_rate';




