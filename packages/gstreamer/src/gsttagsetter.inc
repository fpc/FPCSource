{
/* GStreamer
 * Copyright (C) 2003 Benjamin Otte <in7y118@public.uni-hamburg.de>
 *
 * gsttagsetter.h: Interfaces for tagging
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
    P_GstTagSetterInterface  = ^T_GstTagSetterInterface;
    PGstTagSetter  = ^TGstTagSetter;

    TGstTagSetter = record end;
    T_GstTagSetter = TGstTagSetter;


    T_GstTagSetterInterface = record
        g_iface : TGTypeInterface;
      end;
    TGstTagSetterInterface = T_GstTagSetterInterface ;

  function gst_tag_setter_get_type:TGType;cdecl;external gstreamerlib name 'gst_tag_setter_get_type';

  procedure gst_tag_setter_reset_tags(setter:PGstTagSetter);cdecl;external gstreamerlib name 'gst_tag_setter_reset_tags';

(* Const before type ignored *)
  procedure gst_tag_setter_merge_tags(setter:PGstTagSetter; list:PGstTagList; mode:TGstTagMergeMode);cdecl;external gstreamerlib name 'gst_tag_setter_merge_tags';

(* Const before type ignored *)
  procedure gst_tag_setter_add_tags(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_tag_setter_add_tags';

  procedure gst_tag_setter_add_tags(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar);cdecl;external gstreamerlib name 'gst_tag_setter_add_tags';

(* Const before type ignored *)
  procedure gst_tag_setter_add_tag_values(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_tag_setter_add_tag_values';

  procedure gst_tag_setter_add_tag_values(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar);cdecl;external gstreamerlib name 'gst_tag_setter_add_tag_values';

(* Const before type ignored *)
  procedure gst_tag_setter_add_tag_valist(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar; var_args:Tva_list);cdecl;external gstreamerlib name 'gst_tag_setter_add_tag_valist';

(* Const before type ignored *)
  procedure gst_tag_setter_add_tag_valist_values(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar; var_args:Tva_list);cdecl;external gstreamerlib name 'gst_tag_setter_add_tag_valist_values';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_tag_setter_add_tag_value(setter:PGstTagSetter; mode:TGstTagMergeMode; tag:Pgchar; value:PGValue);cdecl;external gstreamerlib name 'gst_tag_setter_add_tag_value';

(* Const before type ignored *)
  function gst_tag_setter_get_tag_list(setter:PGstTagSetter):PGstTagList;cdecl;external gstreamerlib name 'gst_tag_setter_get_tag_list';

  procedure gst_tag_setter_set_tag_merge_mode(setter:PGstTagSetter; mode:TGstTagMergeMode);cdecl;external gstreamerlib name 'gst_tag_setter_set_tag_merge_mode';

  function gst_tag_setter_get_tag_merge_mode(setter:PGstTagSetter):TGstTagMergeMode;cdecl;external gstreamerlib name 'gst_tag_setter_get_tag_merge_mode';


