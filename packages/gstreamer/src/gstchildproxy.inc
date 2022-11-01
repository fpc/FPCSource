{* GStreamer
 * Copyright (C) 2005 Stefan Kost <ensonic@users.sf.net>
 *
 * gstchildproxy.h: interface header for multi child elements
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
    P_GstChildProxyInterface  = ^T_GstChildProxyInterface;
    PGstChildProxy  = ^TGstChildProxy;

    TGstChildProxy = record end;
    T_GstChildProxy = TGstChildProxy;


    T_GstChildProxyInterface = record
        parent : TGTypeInterface;
        get_child_by_name : function (parent:PGstChildProxy; name:Pgchar):PGObject;cdecl;
        get_child_by_index : function (parent:PGstChildProxy; index:Tguint):PGObject;cdecl;
        get_children_count : function (parent:PGstChildProxy):Tguint;cdecl;
        child_added : procedure (parent:PGstChildProxy; child:PGObject; name:Pgchar);cdecl;
        child_removed : procedure (parent:PGstChildProxy; child:PGObject; name:Pgchar);cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstChildProxyInterface = T_GstChildProxyInterface;

  function gst_child_proxy_get_type:TGType;cdecl;external gstreamerlib name 'gst_child_proxy_get_type';

(* Const before type ignored *)
  function gst_child_proxy_get_child_by_name(parent:PGstChildProxy; name:Pgchar):PGObject;cdecl;external gstreamerlib name 'gst_child_proxy_get_child_by_name';

  function gst_child_proxy_get_children_count(parent:PGstChildProxy):Tguint;cdecl;external gstreamerlib name 'gst_child_proxy_get_children_count';

  function gst_child_proxy_get_child_by_index(parent:PGstChildProxy; index:Tguint):PGObject;cdecl;external gstreamerlib name 'gst_child_proxy_get_child_by_index';

(* Const before type ignored *)
  function gst_child_proxy_lookup(object_:PGstChildProxy; name:Pgchar; target:PPGObject; pspec:PPGParamSpec):Tgboolean;cdecl;external gstreamerlib name 'gst_child_proxy_lookup';

(* Const before type ignored *)
  procedure gst_child_proxy_get_property(object_:PGstChildProxy; name:Pgchar; value:PGValue);cdecl;external gstreamerlib name 'gst_child_proxy_get_property';

(* Const before type ignored *)
  procedure gst_child_proxy_get_valist(object_:PGstChildProxy; first_property_name:Pgchar; var_args:Tva_list);cdecl;external gstreamerlib name 'gst_child_proxy_get_valist';

(* Const before type ignored *)
  procedure gst_child_proxy_get(object_:PGstChildProxy; first_property_name:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_child_proxy_get';

  procedure gst_child_proxy_get(object_:PGstChildProxy; first_property_name:Pgchar);cdecl;external gstreamerlib name 'gst_child_proxy_get';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_child_proxy_set_property(object_:PGstChildProxy; name:Pgchar; value:PGValue);cdecl;external gstreamerlib name 'gst_child_proxy_set_property';

(* Const before type ignored *)
  procedure gst_child_proxy_set_valist(object_:PGstChildProxy; first_property_name:Pgchar; var_args:Tva_list);cdecl;external gstreamerlib name 'gst_child_proxy_set_valist';

(* Const before type ignored *)
  procedure gst_child_proxy_set(object_:PGstChildProxy; first_property_name:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_child_proxy_set';

  procedure gst_child_proxy_set(object_:PGstChildProxy; first_property_name:Pgchar);cdecl;external gstreamerlib name 'gst_child_proxy_set';

(* Const before type ignored *)
  procedure gst_child_proxy_child_added(parent:PGstChildProxy; child:PGObject; name:Pgchar);cdecl;external gstreamerlib name 'gst_child_proxy_child_added';

(* Const before type ignored *)
  procedure gst_child_proxy_child_removed(parent:PGstChildProxy; child:PGObject; name:Pgchar);cdecl;external gstreamerlib name 'gst_child_proxy_child_removed';


