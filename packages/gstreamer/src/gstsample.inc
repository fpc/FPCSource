{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstsample.h: Header for GstSample object
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
    PGstSample  = ^TGstSample;
    PGstSample_autoptr  = ^TGstSample_autoptr;
    PGstSample_listautoptr  = ^TGstSample_listautoptr;
    PGstSample_queueautoptr  = ^TGstSample_queueautoptr;
    PGstSample_slistautoptr  = ^TGstSample_slistautoptr;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  TGstSample = record
    end  ;
  T_GstSample = TGstSample;


  TGstSample_autoptr = PGstSample;
  TGstSample_listautoptr = PGList;
  TGstSample_slistautoptr = PGSList;
  TGstSample_queueautoptr = PGQueue;
  PPGstSample = ^PGstSample;

  function gst_sample_get_type:TGType;cdecl;external gstreamerlib name 'gst_sample_get_type';

(* Const before type ignored *)
  function gst_sample_new(buffer:PGstBuffer; caps:PGstCaps; segment:PGstSegment; info:PGstStructure):PGstSample;cdecl;external gstreamerlib name 'gst_sample_new';

  function gst_sample_get_buffer(sample:PGstSample):PGstBuffer;cdecl;external gstreamerlib name 'gst_sample_get_buffer';

  function gst_sample_get_caps(sample:PGstSample):PGstCaps;cdecl;external gstreamerlib name 'gst_sample_get_caps';

  function gst_sample_get_segment(sample:PGstSample):PGstSegment;cdecl;external gstreamerlib name 'gst_sample_get_segment';

(* Const before type ignored *)
  function gst_sample_get_info(sample:PGstSample):PGstStructure;cdecl;external gstreamerlib name 'gst_sample_get_info';

  function gst_sample_get_buffer_list(sample:PGstSample):PGstBufferList;cdecl;external gstreamerlib name 'gst_sample_get_buffer_list';

  procedure gst_sample_set_buffer_list(sample:PGstSample; buffer_list:PGstBufferList);cdecl;external gstreamerlib name 'gst_sample_set_buffer_list';

  procedure gst_sample_set_buffer(sample:PGstSample; buffer:PGstBuffer);cdecl;external gstreamerlib name 'gst_sample_set_buffer';

  procedure gst_sample_set_caps(sample:PGstSample; caps:PGstCaps);cdecl;external gstreamerlib name 'gst_sample_set_caps';

(* Const before type ignored *)
  procedure gst_sample_set_segment(sample:PGstSample; segment:PGstSegment);cdecl;external gstreamerlib name 'gst_sample_set_segment';

  function gst_sample_set_info(sample:PGstSample; info:PGstStructure):Tgboolean;cdecl;external gstreamerlib name 'gst_sample_set_info';

  {
  static inline GstSample *
  gst_sample_ref (GstSample * sample)
  
    return ((GstSample *)gst_mini_object_ref (((GstMiniObject*)(sample))))
                 ;
  
  # 113 "gstsample.h"
  static inline void
  gst_sample_unref (GstSample * sample)
  
    gst_mini_object_unref (((GstMiniObject*)(sample)));
  
  
  # 170 "gstsample.h"
  static inline GstSample *
  gst_sample_copy (const GstSample * buf)
  
    return ((GstSample *)gst_mini_object_copy (((const GstMiniObject*)(buf))));
  
   }

  var
    _gst_sample_type : TGType;cvar;external;

