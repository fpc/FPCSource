{
/* GStreamer
 * Copyright (C) 2003 Benjamin Otte <in7y118@public.uni-hamburg.de>
 *
 * gsttypefindfactory.h: typefinding subsystem
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
  Type
    PGstTypeFindFactory  = ^TGstTypeFindFactory;
    PGstTypeFindFactory_autoptr  = ^TGstTypeFindFactory_autoptr;
    PGstTypeFindFactory_listautoptr  = ^TGstTypeFindFactory_listautoptr;
    PGstTypeFindFactory_queueautoptr  = ^TGstTypeFindFactory_queueautoptr;
    PGstTypeFindFactory_slistautoptr  = ^TGstTypeFindFactory_slistautoptr;

    TGstTypeFindFactory = record end;
    TGstTypeFindFactoryClass = record end;

    T_GstTypeFindFactory = TGstTypeFindFactory;
    T_GstTypeFindFactoryClass = TGstTypeFindFactoryClass;


  TGstTypeFindFactory_autoptr = PGstTypeFindFactory;

  TGstTypeFindFactory_listautoptr = PGList;

  TGstTypeFindFactory_slistautoptr = PGSList;

  TGstTypeFindFactory_queueautoptr = PGQueue;


  function gst_type_find_factory_get_type:TGType;cdecl;external gstreamerlib name 'gst_type_find_factory_get_type';

  function gst_type_find_factory_get_list:PGList;cdecl;external gstreamerlib name 'gst_type_find_factory_get_list';

  function gst_type_find_factory_get_extensions(factory:PGstTypeFindFactory):PPgchar;cdecl;external gstreamerlib name 'gst_type_find_factory_get_extensions';

  function gst_type_find_factory_get_caps(factory:PGstTypeFindFactory):PGstCaps;cdecl;external gstreamerlib name 'gst_type_find_factory_get_caps';

  function gst_type_find_factory_has_function(factory:PGstTypeFindFactory):Tgboolean;cdecl;external gstreamerlib name 'gst_type_find_factory_has_function';

  procedure gst_type_find_factory_call_function(factory:PGstTypeFindFactory; find:PGstTypeFind);cdecl;external gstreamerlib name 'gst_type_find_factory_call_function';


