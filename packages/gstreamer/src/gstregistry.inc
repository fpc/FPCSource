{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *
 * gstregistry.h: Header for registry handling
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
    P_GstRegistry  = ^T_GstRegistry;
    P_GstRegistryClass  = ^T_GstRegistryClass;

    PGstRegistry  = ^TGstRegistry;
    PGstRegistry_autoptr  = ^TGstRegistry_autoptr;
    PGstRegistry_listautoptr  = ^TGstRegistry_listautoptr;
    PGstRegistry_queueautoptr  = ^TGstRegistry_queueautoptr;
    PGstRegistry_slistautoptr  = ^TGstRegistry_slistautoptr;
    PGstRegistryPrivate  = ^TGstRegistryPrivate;

    TGstRegistryPrivate = record end;
    T_GstRegistryPrivate = TGstRegistryPrivate;


    T_GstRegistry = record
        object_ : TGstObject;
        priv : PGstRegistryPrivate;
      end;
    TGstRegistry = T_GstRegistry;


    T_GstRegistryClass = record
        parent_class : TGstObjectClass;
      end;
    TGstRegistryClass = T_GstRegistryClass;


  TGstRegistry_autoptr = PGstRegistry;

  TGstRegistry_listautoptr = PGList;

  TGstRegistry_slistautoptr = PGSList;

  TGstRegistry_queueautoptr = PGQueue;

  function gst_registry_get_type:TGType;cdecl;external gstreamerlib name 'gst_registry_get_type';

  function gst_registry_get:PGstRegistry;cdecl;external gstreamerlib name 'gst_registry_get';

(* Const before type ignored *)
  function gst_registry_scan_path(registry:PGstRegistry; path:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_registry_scan_path';

  function gst_registry_add_plugin(registry:PGstRegistry; plugin:PGstPlugin):Tgboolean;cdecl;external gstreamerlib name 'gst_registry_add_plugin';

  procedure gst_registry_remove_plugin(registry:PGstRegistry; plugin:PGstPlugin);cdecl;external gstreamerlib name 'gst_registry_remove_plugin';

  function gst_registry_add_feature(registry:PGstRegistry; feature:PGstPluginFeature):Tgboolean;cdecl;external gstreamerlib name 'gst_registry_add_feature';

  procedure gst_registry_remove_feature(registry:PGstRegistry; feature:PGstPluginFeature);cdecl;external gstreamerlib name 'gst_registry_remove_feature';

  function gst_registry_get_plugin_list(registry:PGstRegistry):PGList;cdecl;external gstreamerlib name 'gst_registry_get_plugin_list';

  function gst_registry_plugin_filter(registry:PGstRegistry; filter:TGstPluginFilter; first:Tgboolean; user_data:Tgpointer):PGList;cdecl;external gstreamerlib name 'gst_registry_plugin_filter';

  function gst_registry_feature_filter(registry:PGstRegistry; filter:TGstPluginFeatureFilter; first:Tgboolean; user_data:Tgpointer):PGList;cdecl;external gstreamerlib name 'gst_registry_feature_filter';

  function gst_registry_get_feature_list(registry:PGstRegistry; _type:TGType):PGList;cdecl;external gstreamerlib name 'gst_registry_get_feature_list';

(* Const before type ignored *)
  function gst_registry_get_feature_list_by_plugin(registry:PGstRegistry; name:Pgchar):PGList;cdecl;external gstreamerlib name 'gst_registry_get_feature_list_by_plugin';

  function gst_registry_get_feature_list_cookie(registry:PGstRegistry):Tguint32;cdecl;external gstreamerlib name 'gst_registry_get_feature_list_cookie';

(* Const before type ignored *)
  function gst_registry_find_plugin(registry:PGstRegistry; name:Pgchar):PGstPlugin;cdecl;external gstreamerlib name 'gst_registry_find_plugin';

(* Const before type ignored *)
  function gst_registry_find_feature(registry:PGstRegistry; name:Pgchar; _type:TGType):PGstPluginFeature;cdecl;external gstreamerlib name 'gst_registry_find_feature';

(* Const before type ignored *)
  function gst_registry_lookup(registry:PGstRegistry; filename:Pchar):PGstPlugin;cdecl;external gstreamerlib name 'gst_registry_lookup';

(* Const before type ignored *)
  function gst_registry_lookup_feature(registry:PGstRegistry; name:Pchar):PGstPluginFeature;cdecl;external gstreamerlib name 'gst_registry_lookup_feature';

(* Const before type ignored *)
  function gst_registry_check_feature_version(registry:PGstRegistry; feature_name:Pgchar; min_major:Tguint; min_minor:Tguint; min_micro:Tguint):Tgboolean;cdecl;external gstreamerlib name 'gst_registry_check_feature_version';



