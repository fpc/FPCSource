{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *                    2005 Wim Taymans <wim@fluendo.com>
 *
 * gstevent.h: Header for GstEvent subsystem
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
    PGstEvent  = ^TGstEvent;
    PPGstEvent = ^PGstEvent;
    PGstEvent_autoptr  = ^TGstEvent_autoptr;
    PGstEvent_listautoptr  = ^TGstEvent_listautoptr;
    PGstEvent_queueautoptr  = ^TGstEvent_queueautoptr;
    PGstEvent_slistautoptr  = ^TGstEvent_slistautoptr;
    PGstQOSType  = ^TGstQOSType;
    PGstStreamFlags  = ^TGstStreamFlags;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

  TGstEvent_autoptr = PGstEvent;

  TGstEvent_listautoptr = PGList;

  TGstEvent_slistautoptr = PGSList;

  TGstEvent_queueautoptr = PGQueue;

  TGstEventTypeFlags = (
    GST_EVENT_TYPE_UPSTREAM := 1 shl 0,
    GST_EVENT_TYPE_DOWNSTREAM := 1 shl 1,
    GST_EVENT_TYPE_SERIALIZED := 1 shl 2,
    GST_EVENT_TYPE_STICKY := 1 shl 3,
    GST_EVENT_TYPE_STICKY_MULTI := 1 shl 4
  );

  TGstEventType = (
    GST_EVENT_UNKNOWN := (((0) shl (8)) or (0)),


    GST_EVENT_FLUSH_START := (((10) shl (8)) or (ord(GST_EVENT_TYPE_UPSTREAM) or ord(GST_EVENT_TYPE_DOWNSTREAM))),
    GST_EVENT_FLUSH_STOP := (((20) shl (8)) or ((ord(GST_EVENT_TYPE_UPSTREAM) or ord(GST_EVENT_TYPE_DOWNSTREAM)) or ord(GST_EVENT_TYPE_SERIALIZED))),


    GST_EVENT_STREAM_START := (((40) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY))),
    GST_EVENT_CAPS := (((50) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY))),
    GST_EVENT_SEGMENT := (((70) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY))),
    GST_EVENT_STREAM_COLLECTION := (((75) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY) or ord(GST_EVENT_TYPE_STICKY_MULTI))),
    GST_EVENT_TAG := (((80) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY) or ord(GST_EVENT_TYPE_STICKY_MULTI))),
    GST_EVENT_BUFFERSIZE := (((90) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY))),
    GST_EVENT_SINK_MESSAGE := (((100) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY) or ord(GST_EVENT_TYPE_STICKY_MULTI))),
    GST_EVENT_STREAM_GROUP_DONE := (((105) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY))),
    GST_EVENT_EOS := (((110) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY))),
    GST_EVENT_TOC := (((120) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY) or ord(GST_EVENT_TYPE_STICKY_MULTI))),
    GST_EVENT_PROTECTION := (((130) shl (8)) or (ord(GST_EVENT_TYPE_DOWNSTREAM) or ord(GST_EVENT_TYPE_SERIALIZED) or ord(GST_EVENT_TYPE_STICKY) or ord(GST_EVENT_TYPE_STICKY_MULTI))),


    GST_EVENT_SEGMENT_DONE := (((150) shl (8)) or Ord(GST_EVENT_TYPE_DOWNSTREAM) or Ord(GST_EVENT_TYPE_SERIALIZED)),
    GST_EVENT_GAP := (((160) shl (8)) or Ord(GST_EVENT_TYPE_DOWNSTREAM) or Ord(GST_EVENT_TYPE_SERIALIZED)),


    GST_EVENT_QOS := (((190) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_SEEK := (((200) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_NAVIGATION := (((210) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_LATENCY := (((220) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_STEP := (((230) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_RECONFIGURE := (((240) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_TOC_SELECT := (((250) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_SELECT_STREAMS := (((260) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),


    GST_EVENT_CUSTOM_UPSTREAM := (((270) shl (8)) or Ord(GST_EVENT_TYPE_UPSTREAM)),
    GST_EVENT_CUSTOM_DOWNSTREAM := (((280) shl (8)) or Ord(GST_EVENT_TYPE_DOWNSTREAM) or Ord(GST_EVENT_TYPE_SERIALIZED)),
    GST_EVENT_CUSTOM_DOWNSTREAM_OOB := (((290) shl (8)) or Ord(GST_EVENT_TYPE_DOWNSTREAM)),
    GST_EVENT_CUSTOM_DOWNSTREAM_STICKY := (((300) shl (8))
                                           or Ord(GST_EVENT_TYPE_DOWNSTREAM)
                                           or Ord(GST_EVENT_TYPE_SERIALIZED)
                                           or Ord(GST_EVENT_TYPE_STICKY)
                                           or Ord(GST_EVENT_TYPE_STICKY_MULTI)),
    GST_EVENT_CUSTOM_BOTH := (((310) shl (8)) or
                              ((Ord(GST_EVENT_TYPE_UPSTREAM) or Ord(GST_EVENT_TYPE_DOWNSTREAM)) or Ord(GST_EVENT_TYPE_SERIALIZED))),
    GST_EVENT_CUSTOM_BOTH_OOB := (((320) shl (8)) or ((Ord(GST_EVENT_TYPE_UPSTREAM) or ord(GST_EVENT_TYPE_DOWNSTREAM))))
  );


  TGstQOSType = (
    GST_QOS_TYPE_OVERFLOW := 0,
    GST_QOS_TYPE_UNDERFLOW := 1,
    GST_QOS_TYPE_THROTTLE := 2
   );

  T_GstEvent = record
      mini_object : TGstMiniObject ;
      type_ : TGstEventType ;
      timestamp : tguint64 ;
      seqnum : tguint32 ;
  end;
  TGstEvent = T_GstEvent;
  {
  
  static inline gboolean
  gst_event_replace (GstEvent **old_event, GstEvent *new_event)
  
    return gst_mini_object_replace ((GstMiniObject **) old_event, (GstMiniObject *) new_event);
  
  # 347 "gstevent.h"
  static inline GstEvent *
  gst_event_steal (GstEvent **old_event)
  
    return ((GstEvent *)(gst_mini_object_steal ((GstMiniObject **) old_event)));
  
  # 368 "gstevent.h"
  static inline gboolean
  gst_event_take (GstEvent **old_event, GstEvent *new_event)
  
    return gst_mini_object_take ((GstMiniObject **) old_event, (GstMiniObject *) new_event);
  
  # 389 "gstevent.h"
  # 405 "gstevent.h"

  extern
  const gchar* gst_event_type_get_name (GstEventType type);
  
  extern
  GQuark gst_event_type_to_quark (GstEventType type);
  
  extern
  GstEventTypeFlags
                  gst_event_type_get_flags (GstEventType type);
  # 435 "gstevent.h"
  static inline GstEvent *
  gst_event_ref (GstEvent * event)
  
    return (GstEvent *) gst_mini_object_ref (((GstMiniObject*)(event)));
  
  
  
  
  
  
  
  
  static inline void
  gst_event_unref (GstEvent * event)
  
    gst_mini_object_unref (((GstMiniObject*)(event)));
  
  # 466 "gstevent.h"
  static inline void
  gst_clear_event (GstEvent ** event_ptr)
  
    do  _Static_assert (sizeof *(((GstMiniObject **) event_ptr)) == sizeof (gpointer), "Expression evaluates to false"); __typeof__((((GstMiniObject **) event_ptr))) _pp = (((GstMiniObject **) event_ptr)); __typeof__(*(((GstMiniObject **) event_ptr))) _ptr = *_pp; *_pp = 
  # 469 "gstevent.h" 3 4
   ((void *)0)
  # 469 "gstevent.h"
   ; if (_ptr) (gst_mini_object_unref) (_ptr);  while (0) ;
  
  # 481 "gstevent.h"
  static inline GstEvent *
  gst_event_copy (const GstEvent * event)
  
    return ((GstEvent *)(gst_mini_object_copy (((const GstMiniObject*)(event)))));
  
  
   }

  function gst_event_get_type:TGType;cdecl;external gstreamerlib name 'gst_event_get_type';

  function gst_event_new_custom(_type:TGstEventType; structure:PGstStructure):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_custom';

(* Const before type ignored *)
  function gst_event_get_structure(event:PGstEvent):PGstStructure;cdecl;external gstreamerlib name 'gst_event_get_structure';

  function gst_event_writable_structure(event:PGstEvent):PGstStructure;cdecl;external gstreamerlib name 'gst_event_writable_structure';

(* Const before type ignored *)
  function gst_event_has_name(event:PGstEvent; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_event_has_name';

  function gst_event_get_seqnum(event:PGstEvent):Tguint32;cdecl;external gstreamerlib name 'gst_event_get_seqnum';

  procedure gst_event_set_seqnum(event:PGstEvent; seqnum:Tguint32);cdecl;external gstreamerlib name 'gst_event_set_seqnum';

  function gst_event_get_running_time_offset(event:PGstEvent):Tgint64;cdecl;external gstreamerlib name 'gst_event_get_running_time_offset';

  procedure gst_event_set_running_time_offset(event:PGstEvent; offset:Tgint64);cdecl;external gstreamerlib name 'gst_event_set_running_time_offset';

(* Const before type ignored *)
  function gst_event_new_stream_start(stream_id:Pgchar):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_stream_start';

(* Const before type ignored *)
  procedure gst_event_parse_stream_start(event:PGstEvent; stream_id:PPgchar);cdecl;external gstreamerlib name 'gst_event_parse_stream_start';

  procedure gst_event_set_stream(event:PGstEvent; stream:PGstStream);cdecl;external gstreamerlib name 'gst_event_set_stream';

  procedure gst_event_parse_stream(event:PGstEvent; stream:PPGstStream);cdecl;external gstreamerlib name 'gst_event_parse_stream';

  procedure gst_event_set_stream_flags(event:PGstEvent; flags:TGstStreamFlags);cdecl;external gstreamerlib name 'gst_event_set_stream_flags';

  procedure gst_event_parse_stream_flags(event:PGstEvent; flags:PGstStreamFlags);cdecl;external gstreamerlib name 'gst_event_parse_stream_flags';

  procedure gst_event_set_group_id(event:PGstEvent; group_id:Tguint);cdecl;external gstreamerlib name 'gst_event_set_group_id';

  function gst_event_parse_group_id(event:PGstEvent; group_id:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_event_parse_group_id';

  function gst_event_new_flush_start:PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_flush_start';

  function gst_event_new_flush_stop(reset_time:Tgboolean):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_flush_stop';

  procedure gst_event_parse_flush_stop(event:PGstEvent; reset_time:Pgboolean);cdecl;external gstreamerlib name 'gst_event_parse_flush_stop';

  function gst_event_new_stream_collection(collection:PGstStreamCollection):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_stream_collection';

  procedure gst_event_parse_stream_collection(event:PGstEvent; collection:PPGstStreamCollection);cdecl;external gstreamerlib name 'gst_event_parse_stream_collection';

  function gst_event_new_select_streams(streams:PGList):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_select_streams';

  procedure gst_event_parse_select_streams(event:PGstEvent; streams:PPGList);cdecl;external gstreamerlib name 'gst_event_parse_select_streams';

  function gst_event_new_stream_group_done(group_id:Tguint):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_stream_group_done';

  procedure gst_event_parse_stream_group_done(event:PGstEvent; group_id:Pguint);cdecl;external gstreamerlib name 'gst_event_parse_stream_group_done';

  function gst_event_new_eos:PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_eos';

  function gst_event_new_gap(timestamp:TGstClockTime; duration:TGstClockTime):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_gap';

  procedure gst_event_parse_gap(event:PGstEvent; timestamp:PGstClockTime; duration:PGstClockTime);cdecl;external gstreamerlib name 'gst_event_parse_gap';

  function gst_event_new_caps(caps:PGstCaps):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_caps';

  procedure gst_event_parse_caps(event:PGstEvent; caps:PPGstCaps);cdecl;external gstreamerlib name 'gst_event_parse_caps';

(* Const before type ignored *)
  function gst_event_new_segment(segment:PGstSegment):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_segment';

(* Const before type ignored *)
  procedure gst_event_parse_segment(event:PGstEvent; segment:PPGstSegment);cdecl;external gstreamerlib name 'gst_event_parse_segment';

  procedure gst_event_copy_segment(event:PGstEvent; segment:PGstSegment);cdecl;external gstreamerlib name 'gst_event_copy_segment';

  function gst_event_new_tag(taglist:PGstTagList):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_tag';

  procedure gst_event_parse_tag(event:PGstEvent; taglist:PPGstTagList);cdecl;external gstreamerlib name 'gst_event_parse_tag';

  function gst_event_new_toc(toc:PGstToc; updated:Tgboolean):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_toc';

  procedure gst_event_parse_toc(event:PGstEvent; toc:PPGstToc; updated:Pgboolean);cdecl;external gstreamerlib name 'gst_event_parse_toc';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_event_new_protection(system_id:Pgchar; data:PGstBuffer; origin:Pgchar):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_protection';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_event_parse_protection(event:PGstEvent; system_id:PPgchar; data:PPGstBuffer; origin:PPgchar);cdecl;external gstreamerlib name 'gst_event_parse_protection';

  function gst_event_new_buffer_size(format:TGstFormat; minsize:Tgint64; maxsize:Tgint64; async:Tgboolean):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_buffer_size';

  procedure gst_event_parse_buffer_size(event:PGstEvent; format:PGstFormat; minsize:Pgint64; maxsize:Pgint64; async:Pgboolean);cdecl;external gstreamerlib name 'gst_event_parse_buffer_size';

(* Const before type ignored *)
  function gst_event_new_sink_message(name:Pgchar; msg:PGstMessage):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_sink_message';

  procedure gst_event_parse_sink_message(event:PGstEvent; msg:PPGstMessage);cdecl;external gstreamerlib name 'gst_event_parse_sink_message';

  function gst_event_new_qos(_type:TGstQOSType; proportion:Tgdouble; diff:TGstClockTimeDiff; timestamp:TGstClockTime):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_qos';

  procedure gst_event_parse_qos(event:PGstEvent; _type:PGstQOSType; proportion:Pgdouble; diff:PGstClockTimeDiff; timestamp:PGstClockTime);cdecl;external gstreamerlib name 'gst_event_parse_qos';

  function gst_event_new_seek(rate:Tgdouble; format:TGstFormat; flags:TGstSeekFlags; start_type:TGstSeekType; start:Tgint64; 
             stop_type:TGstSeekType; stop:Tgint64):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_seek';

  procedure gst_event_parse_seek(event:PGstEvent; rate:Pgdouble; format:PGstFormat; flags:PGstSeekFlags; start_type:PGstSeekType; 
              start:Pgint64; stop_type:PGstSeekType; stop:Pgint64);cdecl;external gstreamerlib name 'gst_event_parse_seek';

  procedure gst_event_set_seek_trickmode_interval(event:PGstEvent; interval:TGstClockTime);cdecl;external gstreamerlib name 'gst_event_set_seek_trickmode_interval';

  procedure gst_event_parse_seek_trickmode_interval(event:PGstEvent; interval:PGstClockTime);cdecl;external gstreamerlib name 'gst_event_parse_seek_trickmode_interval';

  function gst_event_new_navigation(structure:PGstStructure):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_navigation';

  function gst_event_new_latency(latency:TGstClockTime):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_latency';

  procedure gst_event_parse_latency(event:PGstEvent; latency:PGstClockTime);cdecl;external gstreamerlib name 'gst_event_parse_latency';

  function gst_event_new_step(format:TGstFormat; amount:Tguint64; rate:Tgdouble; flush:Tgboolean; intermediate:Tgboolean):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_step';

  procedure gst_event_parse_step(event:PGstEvent; format:PGstFormat; amount:Pguint64; rate:Pgdouble; flush:Pgboolean; 
              intermediate:Pgboolean);cdecl;external gstreamerlib name 'gst_event_parse_step';

  function gst_event_new_reconfigure:PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_reconfigure';

(* Const before type ignored *)
  function gst_event_new_toc_select(uid:Pgchar):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_toc_select';

  procedure gst_event_parse_toc_select(event:PGstEvent; uid:PPgchar);cdecl;external gstreamerlib name 'gst_event_parse_toc_select';

  function gst_event_new_segment_done(format:TGstFormat; position:Tgint64):PGstEvent;cdecl;external gstreamerlib name 'gst_event_new_segment_done';

  procedure gst_event_parse_segment_done(event:PGstEvent; format:PGstFormat; position:Pgint64);cdecl;external gstreamerlib name 'gst_event_parse_segment_done';



  var
    _gst_event_type : TGType;cvar;external;

