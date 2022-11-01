{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstbin.h: Header for GstBin container object
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


Const
    GST_ELEMENT_FLAG_LAST_BIN = (GST_OBJECT_FLAG_LAST shl 10);

  Type
    P_GstBin  = ^T_GstBin;
    P_GstBinClass  = ^T_GstBinClass;
    PGstBin  = ^TGstBin;
    PGstBin_autoptr  = ^TGstBin_autoptr;
    PGstBin_listautoptr  = ^TGstBin_listautoptr;
    PGstBin_queueautoptr  = ^TGstBin_queueautoptr;
    PGstBin_slistautoptr  = ^TGstBin_slistautoptr;
    PGstBinFlags  = ^TGstBinFlags;
    PGstBinPrivate  = ^TGstBinPrivate;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

    TGstBinPrivate = record end;

    TGstBinFlags = (
      GST_BIN_FLAG_NO_RESYNC := GST_ELEMENT_FLAG_LAST_BIN shl 0,
      GST_BIN_FLAG_STREAMS_AWARE := GST_ELEMENT_FLAG_LAST_BIN shl 1,
      GST_BIN_FLAG_LAST := GST_ELEMENT_FLAG_LAST_BIN shl 5);
    T_GstBin = record
        element : TGstElement;
        numchildren : Tgint;
        children : PGList;
        children_cookie : Tguint32;
        child_bus : PGstBus;
        messages : PGList;
        polling : Tgboolean;
        state_dirty : Tgboolean;
        clock_dirty : Tgboolean;
        provided_clock : PGstClock;
        clock_provider : PGstElement;
        priv : PGstBinPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstBin = T_GstBin;



    T_GstBinClass = record
        parent_class : TGstElementClass;
        pool : PGThreadPool;
        element_added : procedure (bin:PGstBin; child:PGstElement);cdecl;
        element_removed : procedure (bin:PGstBin; child:PGstElement);cdecl;
        add_element : function (bin:PGstBin; element:PGstElement):Tgboolean;cdecl;
        remove_element : function (bin:PGstBin; element:PGstElement):Tgboolean;cdecl;
        handle_message : procedure (bin:PGstBin; message:PGstMessage);cdecl;
        do_latency : function (bin:PGstBin):Tgboolean;cdecl;
        deep_element_added : procedure (bin:PGstBin; sub_bin:PGstBin; child:PGstElement);cdecl;
        deep_element_removed : procedure (bin:PGstBin; sub_bin:PGstBin; child:PGstElement);cdecl;
        _gst_reserved : array[0..(4-2)-1] of Tgpointer;
      end;
    TGstBinClass = T_GstBinClass;
    PGstBinClass = ^TGstBinClass;

  TGstBin_autoptr = PGstBin;

  TGstBin_listautoptr = PGList;

  TGstBin_slistautoptr = PGSList;

  TGstBin_queueautoptr = PGQueue;

  function gst_bin_get_type:TGType;cdecl;external gstreamerlib name 'gst_bin_get_type';


  function gst_bin_new(name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_bin_new';

  function gst_bin_add(bin:PGstBin; element:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_bin_add';

  function gst_bin_remove(bin:PGstBin; element:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_bin_remove';


  function gst_bin_get_by_name(bin:PGstBin; name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_bin_get_by_name';

  function gst_bin_get_by_name_recurse_up(bin:PGstBin; name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_bin_get_by_name_recurse_up';

  function gst_bin_get_by_interface(bin:PGstBin; iface:TGType):PGstElement;cdecl;external gstreamerlib name 'gst_bin_get_by_interface';

  function gst_bin_iterate_elements(bin:PGstBin):PGstIterator;cdecl;external gstreamerlib name 'gst_bin_iterate_elements';

  function gst_bin_iterate_sorted(bin:PGstBin):PGstIterator;cdecl;external gstreamerlib name 'gst_bin_iterate_sorted';

  function gst_bin_iterate_recurse(bin:PGstBin):PGstIterator;cdecl;external gstreamerlib name 'gst_bin_iterate_recurse';

  function gst_bin_iterate_sinks(bin:PGstBin):PGstIterator;cdecl;external gstreamerlib name 'gst_bin_iterate_sinks';

  function gst_bin_iterate_sources(bin:PGstBin):PGstIterator;cdecl;external gstreamerlib name 'gst_bin_iterate_sources';

  function gst_bin_iterate_all_by_interface(bin:PGstBin; iface:TGType):PGstIterator;cdecl;external gstreamerlib name 'gst_bin_iterate_all_by_interface';

  function gst_bin_recalculate_latency(bin:PGstBin):Tgboolean;cdecl;external gstreamerlib name 'gst_bin_recalculate_latency';

  procedure gst_bin_set_suppressed_flags(bin:PGstBin; flags:TGstElementFlags);cdecl;external gstreamerlib name 'gst_bin_set_suppressed_flags';

  function gst_bin_get_suppressed_flags(bin:PGstBin):TGstElementFlags;cdecl;external gstreamerlib name 'gst_bin_get_suppressed_flags';

{ macros }

Function GST_TYPE_BIN : TGType;
function GST_IS_BIN(obj : pointer) : boolean;
Function GST_BIN(obj : Pointer) : PgstBin;
Function GST_BIN_CAST(obj : Pointer) : PGstBin;

Function GST_IS_BIN_CLASS(klass: pointer) : Boolean;
Function GST_BIN_GET_CLASS(obj : Pointer) : PGstBinClass;
Function GST_BIN_CLASS(klass : Pointer) : PGstBinClass;

