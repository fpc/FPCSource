{* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *               2000,2004 Wim Taymans <wim@fluendo.com>
 *
 * gstelementfactory.h: Header for GstElementFactory
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
    PGstElementFactory  = ^TGstElementFactory;
    PGstElementFactory_autoptr  = ^TGstElementFactory_autoptr;
    PGstElementFactory_listautoptr  = ^TGstElementFactory_listautoptr;
    PGstElementFactory_queueautoptr  = ^TGstElementFactory_queueautoptr;
    PGstElementFactory_slistautoptr  = ^TGstElementFactory_slistautoptr;
    PGstElementFactoryListType  = ^TGstElementFactoryListType;

    T_GstElementFactory = record end;
    TGstElementFactory = T_GstElementFactory;
    T_GstElementFactoryClass = record end;
    TGstElementFactoryClass = T_GstElementFactoryClass ;


  TGstElementFactoryListType = Tguint64;

  TGstElementFactory_autoptr = PGstElementFactory;

  TGstElementFactory_listautoptr = PGList;

  TGstElementFactory_slistautoptr = PGSList;

  TGstElementFactory_queueautoptr = PGQueue;


  function gst_element_factory_get_type:TGType;cdecl;external gstreamerlib name 'gst_element_factory_get_type';

  function gst_element_factory_find(name:Pgchar):PGstElementFactory;cdecl;external gstreamerlib name 'gst_element_factory_find';

  function gst_element_factory_get_element_type(factory:PGstElementFactory):TGType;cdecl;external gstreamerlib name 'gst_element_factory_get_element_type';

  function gst_element_factory_get_metadata(factory:PGstElementFactory; key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_element_factory_get_metadata';

  function gst_element_factory_get_metadata_keys(factory:PGstElementFactory):PPgchar;cdecl;external gstreamerlib name 'gst_element_factory_get_metadata_keys';

  function gst_element_factory_get_num_pad_templates(factory:PGstElementFactory):Tguint;cdecl;external gstreamerlib name 'gst_element_factory_get_num_pad_templates';

  function gst_element_factory_get_static_pad_templates(factory:PGstElementFactory):PGList;cdecl;external gstreamerlib name 'gst_element_factory_get_static_pad_templates';

  function gst_element_factory_get_uri_type(factory:PGstElementFactory):TGstURIType;cdecl;external gstreamerlib name 'gst_element_factory_get_uri_type';

  function gst_element_factory_get_uri_protocols(factory:PGstElementFactory):PPgchar;cdecl;external gstreamerlib name 'gst_element_factory_get_uri_protocols';

  function gst_element_factory_has_interface(factory:PGstElementFactory; interfacename:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_element_factory_has_interface';

  function gst_element_factory_create(factory:PGstElementFactory; name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_element_factory_create';

  function gst_element_factory_make(factoryname:Pgchar; name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_element_factory_make';

  function gst_element_register(plugin:PGstPlugin; name:Pgchar; rank:Tguint; _type:TGType):Tgboolean;cdecl;external gstreamerlib name 'gst_element_register';


  function gst_element_factory_list_is_type(factory:PGstElementFactory; _type:TGstElementFactoryListType):Tgboolean;cdecl;external gstreamerlib name 'gst_element_factory_list_is_type';

  function gst_element_factory_list_get_elements(_type:TGstElementFactoryListType; minrank:TGstRank):PGList;cdecl;external gstreamerlib name 'gst_element_factory_list_get_elements';

  function gst_element_factory_list_filter(list:PGList; caps:PGstCaps; direction: TGstPadDirection; subsetonly:Tgboolean):PGList;cdecl;external gstreamerlib name 'gst_element_factory_list_filter';


