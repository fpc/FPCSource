{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *                    2005 Wim Taymans <wim@fluendo.com>
 *                    2011 Wim Taymans <wim.taymans@gmail.com>
 *
 * gstquery.h: GstQuery API declaration
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
    PGstQuery  = ^TGstQuery;
    PGstQuery_autoptr  = ^TGstQuery_autoptr;
    PGstQuery_listautoptr  = ^TGstQuery_listautoptr;
    PGstQuery_queueautoptr  = ^TGstQuery_queueautoptr;
    PGstQuery_slistautoptr  = ^TGstQuery_slistautoptr;
    PGstSchedulingFlags  = ^TGstSchedulingFlags;

    PGstQueryType  = ^TGstQueryType;
    PGstQueryTypeFlags  = ^TGstQueryTypeFlags;

    TGstQueryTypeFlags = (GST_QUERY_TYPE_UPSTREAM := 1 shl 0,GST_QUERY_TYPE_DOWNSTREAM := 1 shl 1,
      GST_QUERY_TYPE_SERIALIZED := 1 shl 2);

    TGstQueryType = (
      GST_QUERY_UNKNOWN := (0 shl 8) or 0,
      GST_QUERY_POSITION := (10 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_DURATION := (20 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_LATENCY := (30 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_JITTER := (40 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_RATE := (50 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_SEEKING := (60 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_SEGMENT := (70 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_CONVERT := (80 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_FORMATS := (90 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_BUFFERING := (110 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_CUSTOM := (120 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_URI := (130 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_ALLOCATION := (140 shl 8) or Ord(GST_QUERY_TYPE_DOWNSTREAM) or Ord(GST_QUERY_TYPE_SERIALIZED),
      GST_QUERY_SCHEDULING := (150 shl 8) or Ord(GST_QUERY_TYPE_UPSTREAM),
      GST_QUERY_ACCEPT_CAPS := (160 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_CAPS := (170 shl 8) or (Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM)),
      GST_QUERY_DRAIN := (180 shl 8) or Ord(GST_QUERY_TYPE_DOWNSTREAM) or Ord(GST_QUERY_TYPE_SERIALIZED),
      GST_QUERY_CONTEXT := (190 shl 8) or Ord(GST_QUERY_TYPE_UPSTREAM) or Ord(GST_QUERY_TYPE_DOWNSTREAM),
      GST_QUERY_BITRATE := (200 shl 8) or Ord(GST_QUERY_TYPE_DOWNSTREAM)
    );

    P_GstQuery = ^T_GstQuery;
    T_GstQuery = record
        mini_object : TGstMiniObject;
        _type : TGstQueryType;
      end;
    TGstQuery = T_GstQuery;

    TGstQuery_autoptr = PGstQuery;

    TGstQuery_listautoptr = PGList;

    TGstQuery_slistautoptr = PGSList;

    TGstQuery_queueautoptr = PGQueue;


  TGstSchedulingFlags = (
    GST_SCHEDULING_FLAG_SEEKABLE := 1 shl 0,
    GST_SCHEDULING_FLAG_SEQUENTIAL := 1 shl 1,
    GST_SCHEDULING_FLAG_BANDWIDTH_LIMITED := 1 shl 2
  );



(* Const before type ignored *)

  function gst_query_type_get_name(_type:TGstQueryType):Pgchar;cdecl;external gstreamerlib name 'gst_query_type_get_name';

  function gst_query_type_to_quark(_type:TGstQueryType):TGQuark;cdecl;external gstreamerlib name 'gst_query_type_to_quark';

  function gst_query_type_get_flags(_type:TGstQueryType):TGstQueryTypeFlags;cdecl;external gstreamerlib name 'gst_query_type_get_flags';

  function gst_query_get_type:TGType;cdecl;external gstreamerlib name 'gst_query_get_type';


  {
  static inline GstQuery *
  gst_query_ref (GstQuery * q)
  
    return ((GstQuery*)(gst_mini_object_ref (((GstMiniObject*)(q)))));
  
  # 253 "gstquery.h"
  static inline void
  gst_query_unref (GstQuery * q)
  
    gst_mini_object_unref (((GstMiniObject*)(q)));
  
  # 272 "gstquery.h"
  static inline void
  gst_clear_query (GstQuery ** query_ptr)
  
    do  _Static_assert (sizeof *(((GstMiniObject **) query_ptr)) == sizeof (gpointer), "Expression evaluates to false"); __typeof__((((GstMiniObject **) query_ptr))) _pp = (((GstMiniObject **) query_ptr)); __typeof__(*(((GstMiniObject **) query_ptr))) _ptr = *_pp; *_pp = 
  # 275 "gstquery.h" 3 4
   ((void *)0)
  # 275 "gstquery.h"
   ; if (_ptr) (gst_mini_object_unref) (_ptr);  while (0) ;
  
  # 289 "gstquery.h"
  static inline GstQuery *
  gst_query_copy (const GstQuery * q)
  
    return ((GstQuery*)(gst_mini_object_copy (((const GstMiniObject*)(q)))));
  
  # 327 "gstquery.h"
  static inline gboolean
  gst_query_replace (GstQuery **old_query, GstQuery *new_query)
  
    return gst_mini_object_replace ((GstMiniObject **) old_query, (GstMiniObject *) new_query);
  
  # 350 "gstquery.h"
  static inline gboolean
  gst_query_take (GstQuery **old_query, GstQuery *new_query)
  
    return gst_mini_object_take ((GstMiniObject **) old_query,
        (GstMiniObject *) new_query);
  
   }
  function gst_query_new_custom(_type:TGstQueryType; structure:PGstStructure):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_custom';

(* Const before type ignored *)
  function gst_query_get_structure(query:PGstQuery):PGstStructure;cdecl;external gstreamerlib name 'gst_query_get_structure';

  function gst_query_writable_structure(query:PGstQuery):PGstStructure;cdecl;external gstreamerlib name 'gst_query_writable_structure';

  function gst_query_new_position(format:TGstFormat):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_position';

  procedure gst_query_set_position(query:PGstQuery; format:TGstFormat; cur:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_position';

  procedure gst_query_parse_position(query:PGstQuery; format:PGstFormat; cur:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_position';

  function gst_query_new_duration(format:TGstFormat):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_duration';

  procedure gst_query_set_duration(query:PGstQuery; format:TGstFormat; duration:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_duration';

  procedure gst_query_parse_duration(query:PGstQuery; format:PGstFormat; duration:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_duration';

  function gst_query_new_latency:PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_latency';

  procedure gst_query_set_latency(query:PGstQuery; live:Tgboolean; min_latency:TGstClockTime; max_latency:TGstClockTime);cdecl;external gstreamerlib name 'gst_query_set_latency';

  procedure gst_query_parse_latency(query:PGstQuery; live:Pgboolean; min_latency:PGstClockTime; max_latency:PGstClockTime);cdecl;external gstreamerlib name 'gst_query_parse_latency';

  function gst_query_new_convert(src_format:TGstFormat; value:Tgint64; dest_format:TGstFormat):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_convert';

  procedure gst_query_set_convert(query:PGstQuery; src_format:TGstFormat; src_value:Tgint64; dest_format:TGstFormat; dest_value:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_convert';

  procedure gst_query_parse_convert(query:PGstQuery; src_format:PGstFormat; src_value:Pgint64; dest_format:PGstFormat; dest_value:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_convert';

  function gst_query_new_segment(format:TGstFormat):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_segment';

  procedure gst_query_set_segment(query:PGstQuery; rate:Tgdouble; format:TGstFormat; start_value:Tgint64; stop_value:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_segment';

  procedure gst_query_parse_segment(query:PGstQuery; rate:Pgdouble; format:PGstFormat; start_value:Pgint64; stop_value:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_segment';

  function gst_query_new_seeking(format:TGstFormat):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_seeking';

  procedure gst_query_set_seeking(query:PGstQuery; format:TGstFormat; seekable:Tgboolean; segment_start:Tgint64; segment_end:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_seeking';

  procedure gst_query_parse_seeking(query:PGstQuery; format:PGstFormat; seekable:Pgboolean; segment_start:Pgint64; segment_end:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_seeking';

  function gst_query_new_formats:PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_formats';

  procedure gst_query_set_formats(query:PGstQuery; n_formats:Tgint; args:array of const);cdecl;external gstreamerlib name 'gst_query_set_formats';

  procedure gst_query_set_formats(query:PGstQuery; n_formats:Tgint);cdecl;external gstreamerlib name 'gst_query_set_formats';

(* Const before type ignored *)
  procedure gst_query_set_formatsv(query:PGstQuery; n_formats:Tgint; formats:PGstFormat);cdecl;external gstreamerlib name 'gst_query_set_formatsv';

  procedure gst_query_parse_n_formats(query:PGstQuery; n_formats:Pguint);cdecl;external gstreamerlib name 'gst_query_parse_n_formats';

  procedure gst_query_parse_nth_format(query:PGstQuery; nth:Tguint; format:PGstFormat);cdecl;external gstreamerlib name 'gst_query_parse_nth_format';

  function gst_query_new_buffering(format:TGstFormat):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_buffering';

  procedure gst_query_set_buffering_percent(query:PGstQuery; busy:Tgboolean; percent:Tgint);cdecl;external gstreamerlib name 'gst_query_set_buffering_percent';

  procedure gst_query_parse_buffering_percent(query:PGstQuery; busy:Pgboolean; percent:Pgint);cdecl;external gstreamerlib name 'gst_query_parse_buffering_percent';

  procedure gst_query_set_buffering_stats(query:PGstQuery; mode:TGstBufferingMode; avg_in:Tgint; avg_out:Tgint; buffering_left:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_buffering_stats';

  procedure gst_query_parse_buffering_stats(query:PGstQuery; mode:PGstBufferingMode; avg_in:Pgint; avg_out:Pgint; buffering_left:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_buffering_stats';

  procedure gst_query_set_buffering_range(query:PGstQuery; format:TGstFormat; start:Tgint64; stop:Tgint64; estimated_total:Tgint64);cdecl;external gstreamerlib name 'gst_query_set_buffering_range';

  procedure gst_query_parse_buffering_range(query:PGstQuery; format:PGstFormat; start:Pgint64; stop:Pgint64; estimated_total:Pgint64);cdecl;external gstreamerlib name 'gst_query_parse_buffering_range';

  function gst_query_add_buffering_range(query:PGstQuery; start:Tgint64; stop:Tgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_query_add_buffering_range';

  function gst_query_get_n_buffering_ranges(query:PGstQuery):Tguint;cdecl;external gstreamerlib name 'gst_query_get_n_buffering_ranges';

  function gst_query_parse_nth_buffering_range(query:PGstQuery; index:Tguint; start:Pgint64; stop:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_query_parse_nth_buffering_range';

  function gst_query_new_uri:PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_uri';

  procedure gst_query_parse_uri(query:PGstQuery; uri:PPgchar);cdecl;external gstreamerlib name 'gst_query_parse_uri';

(* Const before type ignored *)
  procedure gst_query_set_uri(query:PGstQuery; uri:Pgchar);cdecl;external gstreamerlib name 'gst_query_set_uri';

  procedure gst_query_parse_uri_redirection(query:PGstQuery; uri:PPgchar);cdecl;external gstreamerlib name 'gst_query_parse_uri_redirection';

(* Const before type ignored *)
  procedure gst_query_set_uri_redirection(query:PGstQuery; uri:Pgchar);cdecl;external gstreamerlib name 'gst_query_set_uri_redirection';

  procedure gst_query_parse_uri_redirection_permanent(query:PGstQuery; permanent:Pgboolean);cdecl;external gstreamerlib name 'gst_query_parse_uri_redirection_permanent';

  procedure gst_query_set_uri_redirection_permanent(query:PGstQuery; permanent:Tgboolean);cdecl;external gstreamerlib name 'gst_query_set_uri_redirection_permanent';

  function gst_query_new_allocation(caps:PGstCaps; need_pool:Tgboolean):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_allocation';

  procedure gst_query_parse_allocation(query:PGstQuery; caps:PPGstCaps; need_pool:Pgboolean);cdecl;external gstreamerlib name 'gst_query_parse_allocation';

  procedure gst_query_add_allocation_pool(query:PGstQuery; pool:PGstBufferPool; size:Tguint; min_buffers:Tguint; max_buffers:Tguint);cdecl;external gstreamerlib name 'gst_query_add_allocation_pool';

  function gst_query_get_n_allocation_pools(query:PGstQuery):Tguint;cdecl;external gstreamerlib name 'gst_query_get_n_allocation_pools';

  procedure gst_query_parse_nth_allocation_pool(query:PGstQuery; index:Tguint; pool: PPGstBufferPool; size:Pguint; min_buffers:Pguint;
              max_buffers:Pguint);cdecl;external gstreamerlib name 'gst_query_parse_nth_allocation_pool';

  procedure gst_query_set_nth_allocation_pool(query:PGstQuery; index:Tguint; pool:PGstBufferPool; size:Tguint; min_buffers:Tguint; 
              max_buffers:Tguint);cdecl;external gstreamerlib name 'gst_query_set_nth_allocation_pool';

  procedure gst_query_remove_nth_allocation_pool(query:PGstQuery; index:Tguint);cdecl;external gstreamerlib name 'gst_query_remove_nth_allocation_pool';

(* Const before type ignored *)
  procedure gst_query_add_allocation_param(query:PGstQuery; allocator:PGstAllocator; params:PGstAllocationParams);cdecl;external gstreamerlib name 'gst_query_add_allocation_param';

  function gst_query_get_n_allocation_params(query:PGstQuery):Tguint;cdecl;external gstreamerlib name 'gst_query_get_n_allocation_params';

  procedure gst_query_parse_nth_allocation_param(query:PGstQuery; index:Tguint; allocator:PPGstAllocator; params:PGstAllocationParams);cdecl;external gstreamerlib name 'gst_query_parse_nth_allocation_param';

(* Const before type ignored *)
  procedure gst_query_set_nth_allocation_param(query:PGstQuery; index:Tguint; allocator:PGstAllocator; params:PGstAllocationParams);cdecl;external gstreamerlib name 'gst_query_set_nth_allocation_param';

  procedure gst_query_remove_nth_allocation_param(query:PGstQuery; index:Tguint);cdecl;external gstreamerlib name 'gst_query_remove_nth_allocation_param';

(* Const before type ignored *)
  procedure gst_query_add_allocation_meta(query:PGstQuery; api:TGType; params:PGstStructure);cdecl;external gstreamerlib name 'gst_query_add_allocation_meta';

  function gst_query_get_n_allocation_metas(query:PGstQuery):Tguint;cdecl;external gstreamerlib name 'gst_query_get_n_allocation_metas';

(* Const before type ignored *)
  function gst_query_parse_nth_allocation_meta(query:PGstQuery; index:Tguint; params:PPGstStructure):TGType;cdecl;external gstreamerlib name 'gst_query_parse_nth_allocation_meta';

  procedure gst_query_remove_nth_allocation_meta(query:PGstQuery; index:Tguint);cdecl;external gstreamerlib name 'gst_query_remove_nth_allocation_meta';

  function gst_query_find_allocation_meta(query:PGstQuery; api:TGType; index:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_query_find_allocation_meta';


  function gst_query_new_scheduling:PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_scheduling';

  procedure gst_query_set_scheduling(query:PGstQuery; flags:TGstSchedulingFlags; minsize:Tgint; maxsize:Tgint; align:Tgint);cdecl;external gstreamerlib name 'gst_query_set_scheduling';

  procedure gst_query_parse_scheduling(query:PGstQuery; flags:PGstSchedulingFlags; minsize:Pgint; maxsize:Pgint; align:Pgint);cdecl;external gstreamerlib name 'gst_query_parse_scheduling';

  procedure gst_query_add_scheduling_mode(query:PGstQuery; mode:TGstPadMode);cdecl;external gstreamerlib name 'gst_query_add_scheduling_mode';

  function gst_query_get_n_scheduling_modes(query:PGstQuery):Tguint;cdecl;external gstreamerlib name 'gst_query_get_n_scheduling_modes';

  function gst_query_parse_nth_scheduling_mode(query:PGstQuery; index:Tguint):TGstPadMode;cdecl;external gstreamerlib name 'gst_query_parse_nth_scheduling_mode';

  function gst_query_has_scheduling_mode(query:PGstQuery; mode:TGstPadMode):Tgboolean;cdecl;external gstreamerlib name 'gst_query_has_scheduling_mode';

  function gst_query_has_scheduling_mode_with_flags(query:PGstQuery; mode:TGstPadMode; flags:TGstSchedulingFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_query_has_scheduling_mode_with_flags';

  function gst_query_new_accept_caps(caps:PGstCaps):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_accept_caps';

  procedure gst_query_parse_accept_caps(query:PGstQuery; caps:PPGstCaps);cdecl;external gstreamerlib name 'gst_query_parse_accept_caps';

  procedure gst_query_set_accept_caps_result(query:PGstQuery; result:Tgboolean);cdecl;external gstreamerlib name 'gst_query_set_accept_caps_result';

  procedure gst_query_parse_accept_caps_result(query:PGstQuery; result:Pgboolean);cdecl;external gstreamerlib name 'gst_query_parse_accept_caps_result';

  function gst_query_new_caps(filter:PGstCaps):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_caps';

  procedure gst_query_parse_caps(query:PGstQuery; filter:PPGstCaps);cdecl;external gstreamerlib name 'gst_query_parse_caps';

  procedure gst_query_set_caps_result(query:PGstQuery; caps:PGstCaps);cdecl;external gstreamerlib name 'gst_query_set_caps_result';

  procedure gst_query_parse_caps_result(query:PGstQuery; caps:PPGstCaps);cdecl;external gstreamerlib name 'gst_query_parse_caps_result';

  function gst_query_new_drain:PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_drain';

(* Const before type ignored *)
  function gst_query_new_context(context_type:Pgchar):PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_context';

(* Const before type ignored *)
  function gst_query_parse_context_type(query:PGstQuery; context_type:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_query_parse_context_type';

  procedure gst_query_set_context(query:PGstQuery; context:PGstContext);cdecl;external gstreamerlib name 'gst_query_set_context';

  procedure gst_query_parse_context(query:PGstQuery; context:PPGstContext);cdecl;external gstreamerlib name 'gst_query_parse_context';

  function gst_query_new_bitrate:PGstQuery;cdecl;external gstreamerlib name 'gst_query_new_bitrate';

  procedure gst_query_set_bitrate(query:PGstQuery; nominal_bitrate:Tguint);cdecl;external gstreamerlib name 'gst_query_set_bitrate';

  procedure gst_query_parse_bitrate(query:PGstQuery; nominal_bitrate:Pguint);cdecl;external gstreamerlib name 'gst_query_parse_bitrate';
  var
    _gst_query_type : TGType;cvar;external;


