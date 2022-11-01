{
/* GStreamer
 * Copyright (C) 2012 Olivier Crete <olivier.crete@collabora.com>
 *
 * gstdeviceprovider.h: Device probing and monitoring
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
 */
}
  Type
    P_GstDeviceProvider  = ^T_GstDeviceProvider;
    P_GstDeviceProviderClass  = ^T_GstDeviceProviderClass;
    PGstDeviceProvider  = ^TGstDeviceProvider;
    PGstDeviceProvider_autoptr  = ^TGstDeviceProvider_autoptr;
    PGstDeviceProvider_listautoptr  = ^TGstDeviceProvider_listautoptr;
    PGstDeviceProvider_queueautoptr  = ^TGstDeviceProvider_queueautoptr;
    PGstDeviceProvider_slistautoptr  = ^TGstDeviceProvider_slistautoptr;
    PGstDeviceProviderClass  = ^TGstDeviceProviderClass;
    PGstDeviceProviderFactory  = ^TGstDeviceProviderFactory;
    PGstDeviceProviderPrivate  = ^TGstDeviceProviderPrivate;

    T_GstDeviceProviderFactory  = record end;
    TGstDeviceProviderFactory = T_GstDeviceProviderFactory ;

    T_GstDeviceProviderFactoryClass = record end;
    TGstDeviceProviderFactoryClass = T_GstDeviceProviderFactoryClass;

    T_GstDeviceProviderPrivate = record end;
    TGstDeviceProviderPrivate = T_GstDeviceProviderPrivate;

    T_GstDeviceProvider = record
        parent : TGstObject;
        devices : PGList;
        priv : PGstDeviceProviderPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstDeviceProvider = T_GstDeviceProvider;

    T_GstDeviceProviderClass = record
        parent_class : TGstObjectClass;
        factory : PGstDeviceProviderFactory;
        probe : function (provider:PGstDeviceProvider):PGList;cdecl;
        start : function (provider:PGstDeviceProvider):Tgboolean;cdecl;
        stop : procedure (provider:PGstDeviceProvider);cdecl;
        metadata : Tgpointer;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstDeviceProviderClass = T_GstDeviceProviderClass;

    TGstDeviceProvider_autoptr = PGstDeviceProvider;

    TGstDeviceProvider_listautoptr = PGList;

    TGstDeviceProvider_slistautoptr = PGSList;

    TGstDeviceProvider_queueautoptr = PGQueue;


  function gst_device_provider_get_type:TGType;cdecl;external gstreamerlib name 'gst_device_provider_get_type';

  function gst_device_provider_get_devices(provider:PGstDeviceProvider):PGList;cdecl;external gstreamerlib name 'gst_device_provider_get_devices';

  function gst_device_provider_start(provider:PGstDeviceProvider):Tgboolean;cdecl;external gstreamerlib name 'gst_device_provider_start';

  procedure gst_device_provider_stop(provider:PGstDeviceProvider);cdecl;external gstreamerlib name 'gst_device_provider_stop';

  function gst_device_provider_can_monitor(provider:PGstDeviceProvider):Tgboolean;cdecl;external gstreamerlib name 'gst_device_provider_can_monitor';

  function gst_device_provider_get_bus(provider:PGstDeviceProvider):PGstBus;cdecl;external gstreamerlib name 'gst_device_provider_get_bus';

  procedure gst_device_provider_device_add(provider:PGstDeviceProvider; device:PGstDevice);cdecl;external gstreamerlib name 'gst_device_provider_device_add';

  procedure gst_device_provider_device_remove(provider:PGstDeviceProvider; device:PGstDevice);cdecl;external gstreamerlib name 'gst_device_provider_device_remove';

  function gst_device_provider_get_hidden_providers(provider:PGstDeviceProvider):PPgchar;cdecl;external gstreamerlib name 'gst_device_provider_get_hidden_providers';

(* Const before type ignored *)
  procedure gst_device_provider_hide_provider(provider:PGstDeviceProvider; name:Pgchar);cdecl;external gstreamerlib name 'gst_device_provider_hide_provider';

(* Const before type ignored *)
  procedure gst_device_provider_unhide_provider(provider:PGstDeviceProvider; name:Pgchar);cdecl;external gstreamerlib name 'gst_device_provider_unhide_provider';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_device_provider_get_metadata(provider:PGstDeviceProvider; key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_device_provider_get_metadata';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_device_provider_class_set_metadata(klass:PGstDeviceProviderClass; longname:Pgchar; classification:Pgchar; description:Pgchar; author:Pgchar);cdecl;external gstreamerlib name 'gst_device_provider_class_set_metadata';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_device_provider_class_set_static_metadata(klass:PGstDeviceProviderClass; longname:Pgchar; classification:Pgchar; description:Pgchar; author:Pgchar);cdecl;external gstreamerlib name 'gst_device_provider_class_set_static_metadata';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_device_provider_class_add_metadata(klass:PGstDeviceProviderClass; key:Pgchar; value:Pgchar);cdecl;external gstreamerlib name 'gst_device_provider_class_add_metadata';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_device_provider_class_add_static_metadata(klass:PGstDeviceProviderClass; key:Pgchar; value:Pgchar);cdecl;external gstreamerlib name 'gst_device_provider_class_add_static_metadata';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_device_provider_class_get_metadata(klass:PGstDeviceProviderClass; key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_device_provider_class_get_metadata';

  procedure gst_device_provider_device_changed(provider:PGstDeviceProvider; device:PGstDevice; changed_device:PGstDevice);cdecl;external gstreamerlib name 'gst_device_provider_device_changed';

  function gst_device_provider_get_factory(provider:PGstDeviceProvider):PGstDeviceProviderFactory;cdecl;external gstreamerlib name 'gst_device_provider_get_factory';


