{/* GStreamer
 * Copyright (C) 2013 Olivier Crete <olivier.crete@collabora.com>
 *
 * gstdevicemonitor.c: Device monitor
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
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 */    }


  Type
    P_GstDeviceMonitor  = ^T_GstDeviceMonitor;
    P_GstDeviceMonitorClass  = ^T_GstDeviceMonitorClass;
    PGstDeviceMonitor  = ^TGstDeviceMonitor;
    PGstDeviceMonitor_autoptr  = ^TGstDeviceMonitor_autoptr;
    PGstDeviceMonitor_listautoptr  = ^TGstDeviceMonitor_listautoptr;
    PGstDeviceMonitor_queueautoptr  = ^TGstDeviceMonitor_queueautoptr;
    PGstDeviceMonitor_slistautoptr  = ^TGstDeviceMonitor_slistautoptr;
    PGstDeviceMonitorPrivate  = ^TGstDeviceMonitorPrivate;


    TGstDeviceMonitorPrivate = record end;
    T_GstDeviceMonitorPrivate = TGstDeviceMonitorPrivate;
    T_GstDeviceMonitor = record
        parent : TGstObject;
        priv : PGstDeviceMonitorPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstDeviceMonitor = T_GstDeviceMonitor;



    T_GstDeviceMonitorClass = record
        parent_class : TGstObjectClass;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstDeviceMonitorClass = T_GstDeviceMonitorClass ;


  TGstDeviceMonitor_autoptr = PGstDeviceMonitor;

  TGstDeviceMonitor_listautoptr = PGList;

  TGstDeviceMonitor_slistautoptr = PGSList;

  TGstDeviceMonitor_queueautoptr = PGQueue;


  function gst_device_monitor_get_type:TGType;cdecl;external gstreamerlib name 'gst_device_monitor_get_type';

  function gst_device_monitor_new:PGstDeviceMonitor;cdecl;external gstreamerlib name 'gst_device_monitor_new';

  function gst_device_monitor_get_bus(monitor:PGstDeviceMonitor):PGstBus;cdecl;external gstreamerlib name 'gst_device_monitor_get_bus';

  function gst_device_monitor_get_devices(monitor:PGstDeviceMonitor):PGList;cdecl;external gstreamerlib name 'gst_device_monitor_get_devices';

  function gst_device_monitor_start(monitor:PGstDeviceMonitor):Tgboolean;cdecl;external gstreamerlib name 'gst_device_monitor_start';

  procedure gst_device_monitor_stop(monitor:PGstDeviceMonitor);cdecl;external gstreamerlib name 'gst_device_monitor_stop';

(* Const before type ignored *)
  function gst_device_monitor_add_filter(monitor:PGstDeviceMonitor; classes:Pgchar; caps:PGstCaps):Tguint;cdecl;external gstreamerlib name 'gst_device_monitor_add_filter';

  function gst_device_monitor_remove_filter(monitor:PGstDeviceMonitor; filter_id:Tguint):Tgboolean;cdecl;external gstreamerlib name 'gst_device_monitor_remove_filter';

  function gst_device_monitor_get_providers(monitor:PGstDeviceMonitor):PPgchar;cdecl;external gstreamerlib name 'gst_device_monitor_get_providers';

  procedure gst_device_monitor_set_show_all_devices(monitor:PGstDeviceMonitor; show_all:Tgboolean);cdecl;external gstreamerlib name 'gst_device_monitor_set_show_all_devices';

  function gst_device_monitor_get_show_all_devices(monitor:PGstDeviceMonitor):Tgboolean;cdecl;external gstreamerlib name 'gst_device_monitor_get_show_all_devices';



