{* GStreamer
 *
 * Copyright (C) 2007 Sebastian Dröge <slomo@circular-chaos.org>
 *
 * gstcontrolsource.h: Interface declaration for control sources
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
    P_GstControlSource  = ^T_GstControlSource;
    P_GstControlSourceClass  = ^T_GstControlSourceClass;
    P_GstTimedValue  = ^T_GstTimedValue;
    PGstControlSource  = ^TGstControlSource;
    PGstControlSource_autoptr  = ^TGstControlSource_autoptr;
    PGstControlSource_listautoptr  = ^TGstControlSource_listautoptr;
    PGstControlSource_queueautoptr  = ^TGstControlSource_queueautoptr;
    PGstControlSource_slistautoptr  = ^TGstControlSource_slistautoptr;
    PGstValueArray  = ^TGstValueArray;
    PGstValueArray_autoptr  = ^TGstValueArray_autoptr;
    PGstValueArray_listautoptr  = ^TGstValueArray_listautoptr;
    PGstValueArray_queueautoptr  = ^TGstValueArray_queueautoptr;
    PGstValueArray_slistautoptr  = ^TGstValueArray_slistautoptr;


    TGstValueArray = record end;
    T_GstValueArray = TGstValueArray;


    T_GstTimedValue = record
        timestamp : TGstClockTime;
        value : Tgdouble;
      end;
    TGstTimedValue = T_GstTimedValue ;


    TGstControlSourceGetValue = function (self:PGstControlSource; timestamp:TGstClockTime; value:Pgdouble):Tgboolean;cdecl;

    TGstControlSourceGetValueArray = function (self:PGstControlSource; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; values:Pgdouble):Tgboolean;cdecl;

    T_GstControlSource = record
        parent : TGstObject;
        get_value : TGstControlSourceGetValue;
        get_value_array : TGstControlSourceGetValueArray;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstControlSource = T_GstControlSource;


    T_GstControlSourceClass = record
        parent_class : TGstObjectClass;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstControlSourceClass = T_GstControlSourceClass;

    TGstControlSource_autoptr = PGstControlSource;

    TGstControlSource_listautoptr = PGList;

    TGstControlSource_slistautoptr = PGSList;

    TGstControlSource_queueautoptr = PGQueue;

    TGstValueArray_autoptr = PGstValueArray;

    TGstValueArray_listautoptr = PGList;

    TGstValueArray_slistautoptr = PGSList;

    TGstValueArray_queueautoptr = PGQueue;

  function gst_control_source_get_type:TGType;cdecl;external gstreamerlib name 'gst_control_source_get_type';

  function gst_control_source_get_value(self:PGstControlSource; timestamp:TGstClockTime; value:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_control_source_get_value';

  function gst_control_source_get_value_array(self:PGstControlSource; timestamp:TGstClockTime; interval:TGstClockTime; n_values:Tguint; values:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_control_source_get_value_array';



