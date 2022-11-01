{
/* GStreamer
 * Copyright (C) 2013 Stefan Sauer <ensonic@users.sf.net>
 *
 * gsttracerfactory.h: tracing subsystem
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
    PGstTracerFactory  = ^TGstTracerFactory;
    PGstTracerFactory_autoptr  = ^TGstTracerFactory_autoptr;
    PGstTracerFactory_listautoptr  = ^TGstTracerFactory_listautoptr;
    PGstTracerFactory_queueautoptr  = ^TGstTracerFactory_queueautoptr;
    PGstTracerFactory_slistautoptr  = ^TGstTracerFactory_slistautoptr;



    TGstTracerFactory = record end;
    TGstTracerFactoryClass = record end;

    T_GstTracerFactory = TGstTracerFactory;
    T_GstTracerFactoryClass = TGstTracerFactoryClass;


    TGstTracerFactory_autoptr = PGstTracerFactory;

    TGstTracerFactory_listautoptr = PGList;

    TGstTracerFactory_slistautoptr = PGSList;

    TGstTracerFactory_queueautoptr = PGQueue;


  function gst_tracer_factory_get_type:TGType;cdecl;external gstreamerlib name 'gst_tracer_factory_get_type';

  function gst_tracer_factory_get_list:PGList;cdecl;external gstreamerlib name 'gst_tracer_factory_get_list';

  function gst_tracer_factory_get_tracer_type(factory:PGstTracerFactory):TGType;cdecl;external gstreamerlib name 'gst_tracer_factory_get_tracer_type';


