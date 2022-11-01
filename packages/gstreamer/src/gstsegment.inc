{
/* GStreamer
 * Copyright (C) 2005 Wim Taymans <wim@fluendo.com>
 *
 * gstsegment.h: Header for GstSegment subsystem
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
    P_GstSegment  = ^T_GstSegment;
    PGstSeekFlags  = ^TGstSeekFlags;
    PGstSeekType  = ^TGstSeekType;
    PGstSegment  = ^TGstSegment;
    PPGstSegment  = ^PGstSegment;
    PGstSegment_autoptr  = ^TGstSegment_autoptr;
    PGstSegment_listautoptr  = ^TGstSegment_listautoptr;
    PGstSegment_queueautoptr  = ^TGstSegment_queueautoptr;
    PGstSegment_slistautoptr  = ^TGstSegment_slistautoptr;
    PGstSegmentFlags  = ^TGstSegmentFlags;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    TGstSeekType = (
      GST_SEEK_TYPE_NONE := 0,
      GST_SEEK_TYPE_SET := 1,
      GST_SEEK_TYPE_END := 2
    );

    TGstSeekFlags = (
      GST_SEEK_FLAG_NONE := 0,
      GST_SEEK_FLAG_FLUSH := 1 shl 0,
      GST_SEEK_FLAG_ACCURATE := 1 shl 1,
      GST_SEEK_FLAG_KEY_UNIT := 1 shl 2,
      GST_SEEK_FLAG_SEGMENT := 1 shl 3,
      GST_SEEK_FLAG_TRICKMODE := 1 shl 4,
      GST_SEEK_FLAG_SKIP := 1 shl 4,
      GST_SEEK_FLAG_SNAP_BEFORE := 1 shl 5,
      GST_SEEK_FLAG_SNAP_AFTER := 1 shl 6,
      GST_SEEK_FLAG_SNAP_NEAREST := Ord(GST_SEEK_FLAG_SNAP_BEFORE) or ord(GST_SEEK_FLAG_SNAP_AFTER),
      GST_SEEK_FLAG_TRICKMODE_KEY_UNITS := 1 shl 7,
      GST_SEEK_FLAG_TRICKMODE_NO_AUDIO := 1 shl 8
      );

    TGstSegmentFlags = (
      GST_SEGMENT_FLAG_NONE := Ord(GST_SEEK_FLAG_NONE),
      GST_SEGMENT_FLAG_RESET := Ord(GST_SEEK_FLAG_FLUSH),
      GST_SEGMENT_FLAG_TRICKMODE := Ord(GST_SEEK_FLAG_TRICKMODE),
      GST_SEGMENT_FLAG_SKIP := Ord(GST_SEEK_FLAG_TRICKMODE),
      GST_SEGMENT_FLAG_SEGMENT := Ord(GST_SEEK_FLAG_SEGMENT),
      GST_SEGMENT_FLAG_TRICKMODE_KEY_UNITS := Ord(GST_SEEK_FLAG_TRICKMODE_KEY_UNITS),
      GST_SEGMENT_FLAG_TRICKMODE_NO_AUDIO := Ord(GST_SEEK_FLAG_TRICKMODE_NO_AUDIO)
     );
    T_GstSegment = record
        flags : TGstSegmentFlags;
        rate : Tgdouble;
        applied_rate : Tgdouble;
        format : TGstFormat;
        base : Tguint64;
        offset : Tguint64;
        start : Tguint64;
        stop : Tguint64;
        time : Tguint64;
        position : Tguint64;
        duration : Tguint64;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstSegment  = T_GstSegment;

  TGstSegment_autoptr = PGstSegment;

  TGstSegment_listautoptr = PGList;

  TGstSegment_slistautoptr = PGSList;

  TGstSegment_queueautoptr = PGQueue;

  function gst_segment_get_type:TGType;cdecl;external gstreamerlib name 'gst_segment_get_type';

  function gst_segment_new:PGstSegment;cdecl;external gstreamerlib name 'gst_segment_new';

(* Const before type ignored *)
  function gst_segment_copy(segment:PGstSegment):PGstSegment;cdecl;external gstreamerlib name 'gst_segment_copy';

(* Const before type ignored *)
  procedure gst_segment_copy_into(src:PGstSegment; dest:PGstSegment);cdecl;external gstreamerlib name 'gst_segment_copy_into';

  procedure gst_segment_free(segment:PGstSegment);cdecl;external gstreamerlib name 'gst_segment_free';

  procedure gst_segment_init(segment:PGstSegment; format:TGstFormat);cdecl;external gstreamerlib name 'gst_segment_init';

(* Const before type ignored *)
  function gst_segment_to_stream_time_full(segment:PGstSegment; format:TGstFormat; position:Tguint64; stream_time:Pguint64):Tgint;cdecl;external gstreamerlib name 'gst_segment_to_stream_time_full';

(* Const before type ignored *)
  function gst_segment_to_stream_time(segment:PGstSegment; format:TGstFormat; position:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_segment_to_stream_time';

(* Const before type ignored *)
  function gst_segment_position_from_stream_time_full(segment:PGstSegment; format:TGstFormat; stream_time:Tguint64; position:Pguint64):Tgint;cdecl;external gstreamerlib name 'gst_segment_position_from_stream_time_full';

(* Const before type ignored *)
  function gst_segment_position_from_stream_time(segment:PGstSegment; format:TGstFormat; stream_time:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_segment_position_from_stream_time';

(* Const before type ignored *)
  function gst_segment_to_running_time(segment:PGstSegment; format:TGstFormat; position:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_segment_to_running_time';

(* Const before type ignored *)
  function gst_segment_to_running_time_full(segment:PGstSegment; format:TGstFormat; position:Tguint64; running_time:Pguint64):Tgint;cdecl;external gstreamerlib name 'gst_segment_to_running_time_full';

(* Const before type ignored *)
  function gst_segment_to_position(segment:PGstSegment; format:TGstFormat; running_time:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_segment_to_position';

(* Const before type ignored *)
  function gst_segment_position_from_running_time_full(segment:PGstSegment; format:TGstFormat; running_time:Tguint64; position:Pguint64):Tgint;cdecl;external gstreamerlib name 'gst_segment_position_from_running_time_full';

(* Const before type ignored *)
  function gst_segment_position_from_running_time(segment:PGstSegment; format:TGstFormat; running_time:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_segment_position_from_running_time';

  function gst_segment_set_running_time(segment:PGstSegment; format:TGstFormat; running_time:Tguint64):Tgboolean;cdecl;external gstreamerlib name 'gst_segment_set_running_time';

  function gst_segment_offset_running_time(segment:PGstSegment; format:TGstFormat; offset:Tgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_segment_offset_running_time';

(* Const before type ignored *)
  function gst_segment_clip(segment:PGstSegment; format:TGstFormat; start:Tguint64; stop:Tguint64; clip_start:Pguint64; 
             clip_stop:Pguint64):Tgboolean;cdecl;external gstreamerlib name 'gst_segment_clip';

  function gst_segment_do_seek(segment:PGstSegment; rate:Tgdouble; format:TGstFormat; flags:TGstSeekFlags; start_type:TGstSeekType; 
             start:Tguint64; stop_type:TGstSeekType; stop:Tguint64; update:Pgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_segment_do_seek';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_segment_is_equal(s0:PGstSegment; s1:PGstSegment):Tgboolean;cdecl;external gstreamerlib name 'gst_segment_is_equal';



