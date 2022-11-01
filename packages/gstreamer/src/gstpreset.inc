{
/* GStreamer
 * Copyright (C) 2006 Stefan Kost <ensonic@users.sf.net>
 *
 * gstpreset.h: helper interface header for element presets
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
    P_GstPresetInterface  = ^T_GstPresetInterface;
    PGstPreset  = ^TGstPreset;

    TGstPreset = record end;
    T_GstPreset = TGstPreset;

    T_GstPresetInterface = record
        parent : TGTypeInterface;
        get_preset_names : function (preset:PGstPreset):PPgchar;cdecl;
        get_property_names : function (preset:PGstPreset):PPgchar;cdecl;
        load_preset : function (preset:PGstPreset; name:Pgchar):Tgboolean;cdecl;
        save_preset : function (preset:PGstPreset; name:Pgchar):Tgboolean;cdecl;
        rename_preset : function (preset:PGstPreset; old_name:Pgchar; new_name:Pgchar):Tgboolean;cdecl;
        delete_preset : function (preset:PGstPreset; name:Pgchar):Tgboolean;cdecl;
        set_meta : function (preset:PGstPreset; name:Pgchar; tag:Pgchar; value:Pgchar):Tgboolean;cdecl;
        get_meta : function (preset:PGstPreset; name:Pgchar; tag:Pgchar; value:PPgchar):Tgboolean;cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstPresetInterface = T_GstPresetInterface ;


  function gst_preset_get_type:TGType;cdecl;external gstreamerlib name 'gst_preset_get_type';

  function gst_preset_get_preset_names(preset:PGstPreset):PPgchar;cdecl;external gstreamerlib name 'gst_preset_get_preset_names';

  function gst_preset_get_property_names(preset:PGstPreset):PPgchar;cdecl;external gstreamerlib name 'gst_preset_get_property_names';

(* Const before type ignored *)
  function gst_preset_load_preset(preset:PGstPreset; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_load_preset';

(* Const before type ignored *)
  function gst_preset_save_preset(preset:PGstPreset; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_save_preset';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_preset_rename_preset(preset:PGstPreset; old_name:Pgchar; new_name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_rename_preset';

(* Const before type ignored *)
  function gst_preset_delete_preset(preset:PGstPreset; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_delete_preset';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_preset_set_meta(preset:PGstPreset; name:Pgchar; tag:Pgchar; value:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_set_meta';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_preset_get_meta(preset:PGstPreset; name:Pgchar; tag:Pgchar; value:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_get_meta';

(* Const before type ignored *)
  function gst_preset_set_app_dir(app_dir:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_set_app_dir';

(* Const before type ignored *)
  function gst_preset_get_app_dir:Pgchar;cdecl;external gstreamerlib name 'gst_preset_get_app_dir';

  function gst_preset_is_editable(preset:PGstPreset):Tgboolean;cdecl;external gstreamerlib name 'gst_preset_is_editable';


