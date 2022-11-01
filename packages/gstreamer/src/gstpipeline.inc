{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstpipeline.h: Header for GstPipeline element
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
    P_GstPipeline  = ^T_GstPipeline;
    P_GstPipelineClass  = ^T_GstPipelineClass;
    PGstPipeline  = ^TGstPipeline;
    PGstPipeline_autoptr  = ^TGstPipeline_autoptr;
    PGstPipeline_listautoptr  = ^TGstPipeline_listautoptr;
    PGstPipeline_queueautoptr  = ^TGstPipeline_queueautoptr;
    PGstPipeline_slistautoptr  = ^TGstPipeline_slistautoptr;
    PGstPipelineFlags  = ^TGstPipelineFlags;
    PGstPipelinePrivate  = ^TGstPipelinePrivate;

    TGstPipelinePrivate = record end;
    T_GstPipelinePrivate = TGstPipelinePrivate;

    TGstPipelineFlags = (
      GST_PIPELINE_FLAG_FIXED_CLOCK := Ord(GST_BIN_FLAG_LAST) shl 0,
      GST_PIPELINE_FLAG_LAST := Ord(GST_BIN_FLAG_LAST) shl 4
    );

    T_GstPipeline = record
        bin : TGstBin;
        fixed_clock : PGstClock;
        stream_time : TGstClockTime;
        delay : TGstClockTime;
        priv : PGstPipelinePrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstPipeline = T_GstPipeline;

    T_GstPipelineClass = record
        parent_class : TGstBinClass;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstPipelineClass = T_GstPipelineClass;


        TGstPipeline_autoptr = PGstPipeline;

        TGstPipeline_listautoptr = PGList;

        TGstPipeline_slistautoptr = PGSList;

        TGstPipeline_queueautoptr = PGQueue;


  function gst_pipeline_get_type:TGType;cdecl;external gstreamerlib name 'gst_pipeline_get_type';

(* Const before type ignored *)
  function gst_pipeline_new(name:Pgchar):PGstElement;cdecl;external gstreamerlib name 'gst_pipeline_new';

  function gst_pipeline_get_bus(pipeline:PGstPipeline):PGstBus;cdecl;external gstreamerlib name 'gst_pipeline_get_bus';

  procedure gst_pipeline_use_clock(pipeline:PGstPipeline; clock:PGstClock);cdecl;external gstreamerlib name 'gst_pipeline_use_clock';

  function gst_pipeline_set_clock(pipeline:PGstPipeline; clock:PGstClock):Tgboolean;cdecl;external gstreamerlib name 'gst_pipeline_set_clock';

  function gst_pipeline_get_clock(pipeline:PGstPipeline):PGstClock;cdecl;external gstreamerlib name 'gst_pipeline_get_clock';

  function gst_pipeline_get_pipeline_clock(pipeline:PGstPipeline):PGstClock;cdecl;external gstreamerlib name 'gst_pipeline_get_pipeline_clock';

  procedure gst_pipeline_auto_clock(pipeline:PGstPipeline);cdecl;external gstreamerlib name 'gst_pipeline_auto_clock';

  procedure gst_pipeline_set_delay(pipeline:PGstPipeline; delay:TGstClockTime);cdecl;external gstreamerlib name 'gst_pipeline_set_delay';

  function gst_pipeline_get_delay(pipeline:PGstPipeline):TGstClockTime;cdecl;external gstreamerlib name 'gst_pipeline_get_delay';

  procedure gst_pipeline_set_latency(pipeline:PGstPipeline; latency:TGstClockTime);cdecl;external gstreamerlib name 'gst_pipeline_set_latency';

  function gst_pipeline_get_latency(pipeline:PGstPipeline):TGstClockTime;cdecl;external gstreamerlib name 'gst_pipeline_get_latency';

  procedure gst_pipeline_set_auto_flush_bus(pipeline:PGstPipeline; auto_flush:Tgboolean);cdecl;external gstreamerlib name 'gst_pipeline_set_auto_flush_bus';

  function gst_pipeline_get_auto_flush_bus(pipeline:PGstPipeline):Tgboolean;cdecl;external gstreamerlib name 'gst_pipeline_get_auto_flush_bus';


