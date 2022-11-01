{/* GStreamer
 * Copyright (C) 2012 Olivier Crete <olivier.crete@collabora.com>
 *
 * gstdevice.c: Device discovery
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
 */}

  Type
    P_GstDevice  = ^T_GstDevice;

    P_GstDeviceClass  = ^T_GstDeviceClass;
    PGstDevice_autoptr  = ^TGstDevice_autoptr;
    PGstDevice_listautoptr  = ^TGstDevice_listautoptr;
    PGstDevice_queueautoptr  = ^TGstDevice_queueautoptr;
    PGstDevice_slistautoptr  = ^TGstDevice_slistautoptr;
    PGstDevicePrivate  = ^TGstDevicePrivate;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  TGstDevicePrivate = record end;
  T_GstDevicePrivate = TGstDevicePrivate;


    T_GstDevice = record
        parent : TGstObject;
        priv : PGstDevicePrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    PGstDevice = ^T_GstDevice;
    PPGstDevice = ^PGstDevice;

    T_GstDeviceClass = record
        parent_class : TGstObjectClass;
        create_element : function (device: PGstDevice; name:Pgchar):PGstElement;cdecl;
        reconfigure_element : function (device:PGstDevice; element:PGstElement):Tgboolean;cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;

  TGstDevice_autoptr = PGstDevice;
  TGstDevice_listautoptr = PGList;
  TGstDevice_slistautoptr = PGSList;
  TGstDevice_queueautoptr = PGQueue;

  function gst_device_get_type:TGType;cdecl;external gstreamerlib name 'gst_device_get_type';

(* Const before type ignored *)
  function gst_device_create_element(device:PGstDevice; name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_device_create_element';

  function gst_device_get_caps(device:PGstDevice):PGstCaps;cdecl;external gstreamerlib name 'gst_device_get_caps';

  function gst_device_get_display_name(device:PGstDevice):Pgchar;cdecl;external gstreamerlib name 'gst_device_get_display_name';

  function gst_device_get_device_class(device:PGstDevice):Pgchar;cdecl;external gstreamerlib name 'gst_device_get_device_class';

  function gst_device_get_properties(device:PGstDevice):PGstStructure;cdecl;external gstreamerlib name 'gst_device_get_properties';

  function gst_device_reconfigure_element(device:PGstDevice; element:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_device_reconfigure_element';

  function gst_device_has_classesv(device:PGstDevice; classes:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_device_has_classesv';

(* Const before type ignored *)
  function gst_device_has_classes(device:PGstDevice; classes:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_device_has_classes';



