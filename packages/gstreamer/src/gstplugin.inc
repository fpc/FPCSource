{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstplugin.h: Header for plugin subsystem
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
    P_GstPluginDesc  = ^T_GstPluginDesc;
    PGError  = ^TGError;
    PGstPlugin  = ^TGstPlugin;
    PGstPlugin_autoptr  = ^TGstPlugin_autoptr;
    PGstPlugin_listautoptr  = ^TGstPlugin_listautoptr;
    PGstPlugin_queueautoptr  = ^TGstPlugin_queueautoptr;
    PGstPlugin_slistautoptr  = ^TGstPlugin_slistautoptr;
    PGstPluginDependencyFlags  = ^TGstPluginDependencyFlags;
    PGstPluginError  = ^TGstPluginError;
    PGstPluginFlags  = ^TGstPluginFlags;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    TGstPlugin = record end;
    T_GstPlugin = TGstPlugin;
    TGstPluginClass = record end;
    T_GstPluginClass = TGstPluginClass;



    TGstPluginError = (
      GST_PLUGIN_ERROR_MODULE,
      GST_PLUGIN_ERROR_DEPENDENCIES,
      GST_PLUGIN_ERROR_NAME_MISMATCH);

    TGstPluginFlags = (
      GST_PLUGIN_FLAG_CACHED := GST_OBJECT_FLAG_LAST shl 0,
      GST_PLUGIN_FLAG_BLACKLISTED := GST_OBJECT_FLAG_LAST shl 1
      );

    TGstPluginDependencyFlags = (GST_PLUGIN_DEPENDENCY_FLAG_NONE := 0,
      GST_PLUGIN_DEPENDENCY_FLAG_RECURSE := 1 shl 0,
      GST_PLUGIN_DEPENDENCY_FLAG_PATHS_ARE_DEFAULT_ONLY := 1 shl 1,
      GST_PLUGIN_DEPENDENCY_FLAG_FILE_NAME_IS_SUFFIX := 1 shl 2,
      GST_PLUGIN_DEPENDENCY_FLAG_FILE_NAME_IS_PREFIX := 1 shl 3,
      GST_PLUGIN_DEPENDENCY_FLAG_PATHS_ARE_RELATIVE_TO_EXE := 1 shl 4
      );

    TGstPluginInitFunc = function (plugin:PGstPlugin):Tgboolean;cdecl;

    TGstPluginInitFullFunc = function (plugin:PGstPlugin; user_data:Tgpointer):Tgboolean;cdecl;

    T_GstPluginDesc = record
        major_version : Tgint;
        minor_version : Tgint;
        name : Pgchar;
        description : Pgchar;
        plugin_init : TGstPluginInitFunc;
        version : Pgchar;
        license : Pgchar;
        source : Pgchar;
        package : Pgchar;
        origin : Pgchar;
        release_datetime : Pgchar;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstPluginDesc = T_GstPluginDesc;


  TGstPluginFilter = function (plugin:PGstPlugin; user_data:Tgpointer):Tgboolean;cdecl;

  TGstPlugin_autoptr = PGstPlugin;

  TGstPlugin_listautoptr = PGList;

  TGstPlugin_slistautoptr = PGSList;

  TGstPlugin_queueautoptr = PGQueue;


  function gst_plugin_error_quark:TGQuark;cdecl;external gstreamerlib name 'gst_plugin_error_quark';

  function gst_plugin_get_type:TGType;cdecl;external gstreamerlib name 'gst_plugin_get_type';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_plugin_register_static(major_version:Tgint; minor_version:Tgint; name:Pgchar; description:Pgchar; init_func:TGstPluginInitFunc; 
             version:Pgchar; license:Pgchar; source:Pgchar; package:Pgchar; origin:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_plugin_register_static';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_plugin_register_static_full(major_version:Tgint; minor_version:Tgint; name:Pgchar; description:Pgchar; init_full_func:TGstPluginInitFullFunc; 
             version:Pgchar; license:Pgchar; source:Pgchar; package:Pgchar; origin:Pgchar; 
             user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_plugin_register_static_full';

(* Const before type ignored *)
  function gst_plugin_get_name(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_name';

(* Const before type ignored *)
  function gst_plugin_get_description(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_description';

(* Const before type ignored *)
  function gst_plugin_get_filename(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_filename';

(* Const before type ignored *)
  function gst_plugin_get_version(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_version';

(* Const before type ignored *)
  function gst_plugin_get_license(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_license';

(* Const before type ignored *)
  function gst_plugin_get_source(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_source';

(* Const before type ignored *)
  function gst_plugin_get_package(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_package';

(* Const before type ignored *)
  function gst_plugin_get_origin(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_origin';

(* Const before type ignored *)
  function gst_plugin_get_release_date_string(plugin:PGstPlugin):Pgchar;cdecl;external gstreamerlib name 'gst_plugin_get_release_date_string';

(* Const before type ignored *)
  function gst_plugin_get_cache_data(plugin:PGstPlugin):PGstStructure;cdecl;external gstreamerlib name 'gst_plugin_get_cache_data';

  procedure gst_plugin_set_cache_data(plugin:PGstPlugin; cache_data:PGstStructure);cdecl;external gstreamerlib name 'gst_plugin_set_cache_data';

  function gst_plugin_is_loaded(plugin:PGstPlugin):Tgboolean;cdecl;external gstreamerlib name 'gst_plugin_is_loaded';

(* Const before type ignored *)
  function gst_plugin_load_file(filename:Pgchar; error:PPGError):PGstPlugin;cdecl;external gstreamerlib name 'gst_plugin_load_file';

  function gst_plugin_load(plugin:PGstPlugin):PGstPlugin;cdecl;external gstreamerlib name 'gst_plugin_load';

(* Const before type ignored *)
  function gst_plugin_load_by_name(name:Pgchar):PGstPlugin;cdecl;external gstreamerlib name 'gst_plugin_load_by_name';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_plugin_add_dependency(plugin:PGstPlugin; env_vars:PPgchar; paths:PPgchar; names:PPgchar; flags:TGstPluginDependencyFlags);cdecl;external gstreamerlib name 'gst_plugin_add_dependency';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_plugin_add_dependency_simple(plugin:PGstPlugin; env_vars:Pgchar; paths:Pgchar; names:Pgchar; flags:TGstPluginDependencyFlags);cdecl;external gstreamerlib name 'gst_plugin_add_dependency_simple';

  procedure gst_plugin_list_free(list:PGList);cdecl;external gstreamerlib name 'gst_plugin_list_free';



