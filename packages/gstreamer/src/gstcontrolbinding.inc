{* GStreamer
 *
 * Copyright (C) 2011 Stefan Sauer <ensonic@users.sf.net>
 *
 * gstcontrolbinding.h: Attachment for control sources
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
    P_GstControlBinding  = ^T_GstControlBinding;
    P_GstControlBindingClass  = ^T_GstControlBindingClass;
    PGParamSpec  = ^TGParamSpec;
    PGstControlBinding  = ^TGstControlBinding;
    PGstControlBinding_autoptr  = ^TGstControlBinding_autoptr;
    PGstControlBinding_listautoptr  = ^TGstControlBinding_listautoptr;
    PGstControlBinding_queueautoptr  = ^TGstControlBinding_queueautoptr;
    PGstControlBinding_slistautoptr  = ^TGstControlBinding_slistautoptr;
    PGstControlBindingPrivate  = ^TGstControlBindingPrivate;

    TGstControlBindingConvert = procedure (binding:PGstControlBinding; src_value:Tgdouble; dest_value:PGValue);cdecl;

    TGstControlBindingPrivate = record end;

    T_GstControlBinding = record
        parent : TGstObject;
        name : Pgchar;
        pspec : PGParamSpec;
        object_ : PGstObject;
        disabled : Tgboolean;
        ABI : record
            case longint of
              0 : ( abi : record
                  priv : PGstControlBindingPrivate;
                end );
              1 : ( _gst_reserved : array[0..3] of Tgpointer );
            end;
      end;
    TGstControlBinding = T_GstControlBinding;

    T_GstControlBindingClass = record
        parent_class : TGstObjectClass;
        sync_values : function (binding:PGstControlBinding; object_:PGstObject; timestamp:TGstClockTime; last_sync:TGstClockTime):Tgboolean;cdecl;
        get_value : function (binding:PGstControlBinding; timestamp:TGstClockTime):PGValue;cdecl;
        get_value_array : function (binding:PGstControlBinding; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; values:Tgpointer):Tgboolean;cdecl;
        get_g_value_array : function (binding:PGstControlBinding; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; values:PGValue):Tgboolean;cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;

  TGstControlBinding_autoptr = PGstControlBinding;

  TGstControlBinding_listautoptr = PGList;

  TGstControlBinding_slistautoptr = PGSList;

  TGstControlBinding_queueautoptr = PGQueue;

  function gst_control_binding_get_type:TGType;cdecl;external gstreamerlib name 'gst_control_binding_get_type';

  function gst_control_binding_sync_values(binding:PGstControlBinding; object_:PGstObject; timestamp:TGstClockTime; last_sync:TGstClockTime):Tgboolean;cdecl;external gstreamerlib name 'gst_control_binding_sync_values';

  function gst_control_binding_get_value(binding:PGstControlBinding; timestamp:TGstClockTime):PGValue;cdecl;external gstreamerlib name 'gst_control_binding_get_value';

  function gst_control_binding_get_value_array(binding:PGstControlBinding; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; values:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_control_binding_get_value_array';

  function gst_control_binding_get_g_value_array(binding:PGstControlBinding; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; values:PGValue):Tgboolean;cdecl;external gstreamerlib name 'gst_control_binding_get_g_value_array';

  procedure gst_control_binding_set_disabled(binding:PGstControlBinding; disabled:Tgboolean);cdecl;external gstreamerlib name 'gst_control_binding_set_disabled';

  function gst_control_binding_is_disabled(binding:PGstControlBinding):Tgboolean;cdecl;external gstreamerlib name 'gst_control_binding_is_disabled';



