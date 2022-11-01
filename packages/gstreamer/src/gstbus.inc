{* GStreamer
 * Copyright (C) 2004 Wim Taymans <wim@fluendo.com>
 *
 * gstbus.h: Header for GstBus subsystem
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
    P_GstBus  = ^T_GstBus;
    P_GstBusClass  = ^T_GstBusClass;
    PGPollFD  = ^TGPollFD;
    PGSource  = ^TGSource;
    PGstBus_autoptr  = ^TGstBus_autoptr;
    PGstBus_listautoptr  = ^TGstBus_listautoptr;
    PGstBus_queueautoptr  = ^TGstBus_queueautoptr;
    PGstBus_slistautoptr  = ^TGstBus_slistautoptr;
    PGstBusFlags  = ^TGstBusFlags;
    PGstBusSyncReply  = ^TGstBusSyncReply;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    TGstBusFlags = (GST_BUS_FLUSHING := GST_OBJECT_FLAG_LAST shl 0,GST_BUS_FLAG_LAST := GST_OBJECT_FLAG_LAST shl 1
      );

    TGstBusSyncReply = (GST_BUS_DROP := 0,GST_BUS_PASS := 1,
      GST_BUS_ASYNC := 2);

    TGstBusSyncHandler = function (bus:PGstBus; message:PGstMessage; user_data:Tgpointer):TGstBusSyncReply;cdecl;

    TGstBusFunc = function (bus:PGstBus; message:PGstMessage; user_data:Tgpointer):Tgboolean;cdecl;

    T_GstBusClass = record
        parent_class : TGstObjectClass;
        message : procedure (bus:PGstBus; message:PGstMessage);cdecl;
        sync_message : procedure (bus:PGstBus; message:PGstMessage);cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstBusClass = T_GstBusClass;


  TGstBus_autoptr = PGstBus;

  TGstBus_listautoptr = PGList;

  TGstBus_slistautoptr = PGSList;

  TGstBus_queueautoptr = PGQueue;


  function gst_bus_get_type:TGType;cdecl;external gstreamerlib name 'gst_bus_get_type';

  function gst_bus_new:PGstBus;cdecl;external gstreamerlib name 'gst_bus_new';

  function gst_bus_post(bus:PGstBus; message:PGstMessage):Tgboolean;cdecl;external gstreamerlib name 'gst_bus_post';

  function gst_bus_have_pending(bus:PGstBus):Tgboolean;cdecl;external gstreamerlib name 'gst_bus_have_pending';

  function gst_bus_peek(bus:PGstBus):PGstMessage;cdecl;external gstreamerlib name 'gst_bus_peek';

  function gst_bus_pop(bus:PGstBus):PGstMessage;cdecl;external gstreamerlib name 'gst_bus_pop';

  function gst_bus_pop_filtered(bus:PGstBus; types:TGstMessageType):PGstMessage;cdecl;external gstreamerlib name 'gst_bus_pop_filtered';

  function gst_bus_timed_pop(bus:PGstBus; timeout:TGstClockTime):PGstMessage;cdecl;external gstreamerlib name 'gst_bus_timed_pop';

  function gst_bus_timed_pop_filtered(bus:PGstBus; timeout:TGstClockTime; types:TGstMessageType):PGstMessage;cdecl;external gstreamerlib name 'gst_bus_timed_pop_filtered';

  procedure gst_bus_set_flushing(bus:PGstBus; flushing:Tgboolean);cdecl;external gstreamerlib name 'gst_bus_set_flushing';

  procedure gst_bus_set_sync_handler(bus:PGstBus; func:TGstBusSyncHandler; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_bus_set_sync_handler';

  procedure gst_bus_get_pollfd(bus:PGstBus; fd:PGPollFD);cdecl;external gstreamerlib name 'gst_bus_get_pollfd';

  function gst_bus_create_watch(bus:PGstBus):PGSource;cdecl;external gstreamerlib name 'gst_bus_create_watch';

  function gst_bus_add_watch_full(bus:PGstBus; priority:Tgint; func:TGstBusFunc; user_data:Tgpointer; notify:TGDestroyNotify):Tguint;cdecl;external gstreamerlib name 'gst_bus_add_watch_full';

  function gst_bus_add_watch(bus:PGstBus; func:TGstBusFunc; user_data:Tgpointer):Tguint;cdecl;external gstreamerlib name 'gst_bus_add_watch';

  function gst_bus_remove_watch(bus:PGstBus):Tgboolean;cdecl;external gstreamerlib name 'gst_bus_remove_watch';

  function gst_bus_poll(bus:PGstBus; events:TGstMessageType; timeout:TGstClockTime):PGstMessage;cdecl;external gstreamerlib name 'gst_bus_poll';

  function gst_bus_async_signal_func(bus:PGstBus; message:PGstMessage; data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_bus_async_signal_func';

  function gst_bus_sync_signal_handler(bus:PGstBus; message:PGstMessage; data:Tgpointer):TGstBusSyncReply;cdecl;external gstreamerlib name 'gst_bus_sync_signal_handler';

  procedure gst_bus_add_signal_watch(bus:PGstBus);cdecl;external gstreamerlib name 'gst_bus_add_signal_watch';

  procedure gst_bus_add_signal_watch_full(bus:PGstBus; priority:Tgint);cdecl;external gstreamerlib name 'gst_bus_add_signal_watch_full';

  procedure gst_bus_remove_signal_watch(bus:PGstBus);cdecl;external gstreamerlib name 'gst_bus_remove_signal_watch';

  procedure gst_bus_enable_sync_message_emission(bus:PGstBus);cdecl;external gstreamerlib name 'gst_bus_enable_sync_message_emission';

  procedure gst_bus_disable_sync_message_emission(bus:PGstBus);cdecl;external gstreamerlib name 'gst_bus_disable_sync_message_emission';



