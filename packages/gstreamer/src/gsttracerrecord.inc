{
/* GStreamer
 * Copyright (C) 2016 Stefan Sauer <ensonic@users.sf.net>
 *
 * gsttracerrecord.h: tracer log record class
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
    PGstTracerRecord  = ^TGstTracerRecord;
    PGstTracerRecord_autoptr  = ^TGstTracerRecord_autoptr;
    PGstTracerRecord_listautoptr  = ^TGstTracerRecord_listautoptr;
    PGstTracerRecord_queueautoptr  = ^TGstTracerRecord_queueautoptr;
    PGstTracerRecord_slistautoptr  = ^TGstTracerRecord_slistautoptr;
    PGstTracerValueFlags  = ^TGstTracerValueFlags;
    PGstTracerValueScope  = ^TGstTracerValueScope;




    TGstTracerRecord = record end;
    TGstTracerRecordClass = record end;

    T_GstTracerRecord = TGstTracerRecord;
    T_GstTracerRecordClass = TGstTracerRecordClass;

  TGstTracerRecord_autoptr = PGstTracerRecord;

  TGstTracerRecord_listautoptr = PGList;

  TGstTracerRecord_slistautoptr = PGSList;

  TGstTracerRecord_queueautoptr = PGQueue;



  TGstTracerValueScope = (
    GST_TRACER_VALUE_SCOPE_PROCESS,
    GST_TRACER_VALUE_SCOPE_THREAD,
    GST_TRACER_VALUE_SCOPE_ELEMENT,
    GST_TRACER_VALUE_SCOPE_PAD
  );


  TGstTracerValueFlags = (
    GST_TRACER_VALUE_FLAGS_NONE := 0,
    GST_TRACER_VALUE_FLAGS_OPTIONAL := 1 shl 0,
    GST_TRACER_VALUE_FLAGS_AGGREGATED := 1 shl 1
   );

function gst_tracer_record_get_type:TGType;cdecl;external gstreamerlib name 'gst_tracer_record_get_type';

