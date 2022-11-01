{
/* GStreamer
 * Copyright (C) 2013 Stefan Sauer <ensonic@users.sf.net>
 *
 * gsttracer.h: tracer base class
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
    P_GstTracer  = ^T_GstTracer;
    P_GstTracerClass  = ^T_GstTracerClass;
    PGstTracer  = ^TGstTracer;
    PGstTracer_autoptr  = ^TGstTracer_autoptr;
    PGstTracer_listautoptr  = ^TGstTracer_listautoptr;
    PGstTracer_queueautoptr  = ^TGstTracer_queueautoptr;
    PGstTracer_slistautoptr  = ^TGstTracer_slistautoptr;
    PGstTracerPrivate  = ^TGstTracerPrivate;

    TGstTracerPrivate = record end;
    T_GstTracerPrivate = TGstTracerPrivate;

    T_GstTracer = record
        parent : TGstObject;
        priv : PGstTracerPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTracer = T_GstTracer ;

    T_GstTracerClass = record
        parent_class : TGstObjectClass;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTracerClass =T_GstTracerClass ;

  TGstTracer_autoptr = PGstTracer;

  TGstTracer_listautoptr = PGList;

  TGstTracer_slistautoptr = PGSList;

  TGstTracer_queueautoptr = PGQueue;


function gst_tracer_get_type:TGType;cdecl;external gstreamerlib name 'gst_tracer_get_type';


