{/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *               2000,2004 Wim Taymans <wim@fluendo.com>
 *               2012 Olivier Crete <olivier.crete@collabora.com>
 *
 * gstdeviceproviderfactory.h: Header for GstDeviceProviderFactory
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
 *}

Type
    PGstDeviceProviderFactory_autoptr  = ^TGstDeviceProviderFactory_autoptr;
    PGstDeviceProviderFactory_listautoptr  = ^TGstDeviceProviderFactory_listautoptr;
    PGstDeviceProviderFactory_queueautoptr  = ^TGstDeviceProviderFactory_queueautoptr;
    PGstDeviceProviderFactory_slistautoptr  = ^TGstDeviceProviderFactory_slistautoptr;

  TGstDeviceProviderFactory_autoptr = PGstDeviceProviderFactory;

  TGstDeviceProviderFactory_listautoptr = PGList;

  TGstDeviceProviderFactory_slistautoptr = PGSList;

  TGstDeviceProviderFactory_queueautoptr = PGQueue;


  function gst_device_provider_factory_get_type:TGType;cdecl;external gstreamerlib name 'gst_device_provider_factory_get_type';

(* Const before type ignored *)
  function gst_device_provider_factory_find(name:Pgchar):PGstDeviceProviderFactory;cdecl;external gstreamerlib name 'gst_device_provider_factory_find';

  function gst_device_provider_factory_get_device_provider_type(factory:PGstDeviceProviderFactory):TGType;cdecl;external gstreamerlib name 'gst_device_provider_factory_get_device_provider_type';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_device_provider_factory_get_metadata(factory:PGstDeviceProviderFactory; key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_device_provider_factory_get_metadata';

  function gst_device_provider_factory_get_metadata_keys(factory:PGstDeviceProviderFactory):PPgchar;cdecl;external gstreamerlib name 'gst_device_provider_factory_get_metadata_keys';

  function gst_device_provider_factory_get(factory:PGstDeviceProviderFactory):PGstDeviceProvider;cdecl;external gstreamerlib name 'gst_device_provider_factory_get';

(* Const before type ignored *)
  function gst_device_provider_factory_get_by_name(factoryname:Pgchar):PGstDeviceProvider;cdecl;external gstreamerlib name 'gst_device_provider_factory_get_by_name';

(* Const before type ignored *)
  function gst_device_provider_register(plugin:PGstPlugin; name:Pgchar; rank:Tguint; _type:TGType):Tgboolean;cdecl;external gstreamerlib name 'gst_device_provider_register';

  function gst_device_provider_factory_has_classesv(factory:PGstDeviceProviderFactory; classes:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_device_provider_factory_has_classesv';

(* Const before type ignored *)
  function gst_device_provider_factory_has_classes(factory:PGstDeviceProviderFactory; classes:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_device_provider_factory_has_classes';

  function gst_device_provider_factory_list_get_device_providers(minrank:TGstRank):PGList;cdecl;external gstreamerlib name 'gst_device_provider_factory_list_get_device_providers';



