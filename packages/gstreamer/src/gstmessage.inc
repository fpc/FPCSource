{
/* GStreamer
 * Copyright (C) 2004 Wim Taymans <wim@fluendo.com>
 *
 * gstmessage.h: Header for GstMessage subsystem
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

    PGstMessage_autoptr  = ^TGstMessage_autoptr;
    PGstMessage_listautoptr  = ^TGstMessage_listautoptr;
    PGstMessage_queueautoptr  = ^TGstMessage_queueautoptr;
    PGstMessage_slistautoptr  = ^TGstMessage_slistautoptr;
    PGstProgressType  = ^TGstProgressType;
    PGstStreamStatusType  = ^TGstStreamStatusType;
    PGstStructureChangeType  = ^TGstStructureChangeType;



    TGstStructureChangeType = (GST_STRUCTURE_CHANGE_TYPE_PAD_LINK := 0,
      GST_STRUCTURE_CHANGE_TYPE_PAD_UNLINK := 1
      );

    TGstStreamStatusType = (GST_STREAM_STATUS_TYPE_CREATE := 0,
      GST_STREAM_STATUS_TYPE_ENTER := 1,GST_STREAM_STATUS_TYPE_LEAVE := 2,
      GST_STREAM_STATUS_TYPE_DESTROY := 3,
      GST_STREAM_STATUS_TYPE_START := 8,GST_STREAM_STATUS_TYPE_PAUSE := 9,
      GST_STREAM_STATUS_TYPE_STOP := 10);

    TGstProgressType = (GST_PROGRESS_TYPE_START := 0,GST_PROGRESS_TYPE_CONTINUE := 1,
      GST_PROGRESS_TYPE_COMPLETE := 2,GST_PROGRESS_TYPE_CANCELED := 3,
      GST_PROGRESS_TYPE_ERROR := 4);


  TGstMessage_autoptr = PGstMessage;
  TGstMessage_listautoptr = PGList;
  TGstMessage_slistautoptr = PGSList;
  TGstMessage_queueautoptr = PGQueue;

  function gst_message_get_type:TGType;cdecl;external gstreamerlib name 'gst_message_get_type';

(* Const before type ignored *)
  function gst_message_type_get_name(_type:TGstMessageType):Pgchar;cdecl;external gstreamerlib name 'gst_message_type_get_name';

  function gst_message_type_to_quark(_type:TGstMessageType):TGQuark;cdecl;external gstreamerlib name 'gst_message_type_to_quark';

  function gst_message_new_custom(_type:TGstMessageType; src:PGstObject; structure:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_custom';

  function gst_message_get_structure(message:PGstMessage):PGstStructure;cdecl;external gstreamerlib name 'gst_message_get_structure';

  function gst_message_writable_structure(message:PGstMessage):PGstStructure;cdecl;external gstreamerlib name 'gst_message_writable_structure';

(* Const before type ignored *)
  function gst_message_has_name(message:PGstMessage; name:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_message_has_name';

  function gst_message_get_seqnum(message:PGstMessage):Tguint32;cdecl;external gstreamerlib name 'gst_message_get_seqnum';

  procedure gst_message_set_seqnum(message:PGstMessage; seqnum:Tguint32);cdecl;external gstreamerlib name 'gst_message_set_seqnum';

  function gst_message_new_eos(src:PGstObject):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_eos';

(* Const before type ignored *)
  function gst_message_new_error(src:PGstObject; error:PGError; debug:Pgchar):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_error';

(* Const before type ignored *)
  function gst_message_new_error_with_details(src:PGstObject; error:PGError; debug:Pgchar; details:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_error_with_details';

  procedure gst_message_parse_error(message:PGstMessage; gerror:PPGError; debug:PPgchar);cdecl;external gstreamerlib name 'gst_message_parse_error';

(* Const before type ignored *)
  procedure gst_message_parse_error_details(message:PGstMessage; structure:PPGstStructure);cdecl;external gstreamerlib name 'gst_message_parse_error_details';

(* Const before type ignored *)
  function gst_message_new_warning(src:PGstObject; error:PGError; debug:Pgchar):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_warning';

(* Const before type ignored *)
  function gst_message_new_warning_with_details(src:PGstObject; error:PGError; debug:Pgchar; details:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_warning_with_details';

  procedure gst_message_parse_warning(message:PGstMessage; gerror:PPGError; debug:PPgchar);cdecl;external gstreamerlib name 'gst_message_parse_warning';

(* Const before type ignored *)
  procedure gst_message_parse_warning_details(message:PGstMessage; structure:PPGstStructure);cdecl;external gstreamerlib name 'gst_message_parse_warning_details';

(* Const before type ignored *)
  function gst_message_new_info(src:PGstObject; error:PGError; debug:Pgchar):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_info';

(* Const before type ignored *)
  function gst_message_new_info_with_details(src:PGstObject; error:PGError; debug:Pgchar; details:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_info_with_details';

  procedure gst_message_parse_info(message:PGstMessage; gerror:PPGError; debug:PPgchar);cdecl;external gstreamerlib name 'gst_message_parse_info';

(* Const before type ignored *)
  procedure gst_message_parse_info_details(message:PGstMessage; structure:PPGstStructure);cdecl;external gstreamerlib name 'gst_message_parse_info_details';

  function gst_message_new_tag(src:PGstObject; tag_list:PGstTagList):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_tag';

  procedure gst_message_parse_tag(message:PGstMessage; tag_list:PPGstTagList);cdecl;external gstreamerlib name 'gst_message_parse_tag';

  function gst_message_new_buffering(src:PGstObject; percent:Tgint):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_buffering';

  procedure gst_message_parse_buffering(message:PGstMessage; percent:Pgint);cdecl;external gstreamerlib name 'gst_message_parse_buffering';

  procedure gst_message_set_buffering_stats(message:PGstMessage; mode:TGstBufferingMode; avg_in:Tgint; avg_out:Tgint; buffering_left:Tgint64);cdecl;external gstreamerlib name 'gst_message_set_buffering_stats';

  procedure gst_message_parse_buffering_stats(message:PGstMessage; mode:PGstBufferingMode; avg_in:Pgint; avg_out:Pgint; buffering_left:Pgint64);cdecl;external gstreamerlib name 'gst_message_parse_buffering_stats';

  function gst_message_new_state_changed(src:PGstObject; oldstate:TGstState; newstate:TGstState; pending:TGstState):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_state_changed';

  procedure gst_message_parse_state_changed(message:PGstMessage; oldstate:PGstState; newstate:PGstState; pending:PGstState);cdecl;external gstreamerlib name 'gst_message_parse_state_changed';

  function gst_message_new_state_dirty(src:PGstObject):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_state_dirty';

  function gst_message_new_step_done(src:PGstObject; format:TGstFormat; amount:Tguint64; rate:Tgdouble; flush:Tgboolean; 
             intermediate:Tgboolean; duration:Tguint64; eos:Tgboolean):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_step_done';

  procedure gst_message_parse_step_done(message:PGstMessage; format:PGstFormat; amount:Pguint64; rate:Pgdouble; flush:Pgboolean; 
              intermediate:Pgboolean; duration:Pguint64; eos:Pgboolean);cdecl;external gstreamerlib name 'gst_message_parse_step_done';

  function gst_message_new_clock_provide(src:PGstObject; clock:PGstClock; ready:Tgboolean):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_clock_provide';

  procedure gst_message_parse_clock_provide(message:PGstMessage; clock:PPGstClock; ready:Pgboolean);cdecl;external gstreamerlib name 'gst_message_parse_clock_provide';

  function gst_message_new_clock_lost(src:PGstObject; clock:PGstClock):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_clock_lost';

  procedure gst_message_parse_clock_lost(message:PGstMessage; clock:PPGstClock);cdecl;external gstreamerlib name 'gst_message_parse_clock_lost';

  function gst_message_new_new_clock(src:PGstObject; clock:PGstClock):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_new_clock';

  procedure gst_message_parse_new_clock(message:PGstMessage; clock:PPGstClock);cdecl;external gstreamerlib name 'gst_message_parse_new_clock';

  function gst_message_new_application(src:PGstObject; structure:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_application';

  function gst_message_new_element(src:PGstObject; structure:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_element';

  function gst_message_new_segment_start(src:PGstObject; format:TGstFormat; position:Tgint64):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_segment_start';

  procedure gst_message_parse_segment_start(message:PGstMessage; format:PGstFormat; position:Pgint64);cdecl;external gstreamerlib name 'gst_message_parse_segment_start';

  function gst_message_new_segment_done(src:PGstObject; format:TGstFormat; position:Tgint64):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_segment_done';

  procedure gst_message_parse_segment_done(message:PGstMessage; format:PGstFormat; position:Pgint64);cdecl;external gstreamerlib name 'gst_message_parse_segment_done';

  function gst_message_new_duration_changed(src:PGstObject):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_duration_changed';

  function gst_message_new_latency(src:PGstObject):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_latency';

  function gst_message_new_async_start(src:PGstObject):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_async_start';

  function gst_message_new_async_done(src:PGstObject; running_time:TGstClockTime):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_async_done';

  procedure gst_message_parse_async_done(message:PGstMessage; running_time:PGstClockTime);cdecl;external gstreamerlib name 'gst_message_parse_async_done';

  function gst_message_new_structure_change(src:PGstObject; _type:TGstStructureChangeType; owner:PGstElement; busy:Tgboolean):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_structure_change';

  procedure gst_message_parse_structure_change(message:PGstMessage; _type:PGstStructureChangeType; owner:PPGstElement; busy:Pgboolean);cdecl;external gstreamerlib name 'gst_message_parse_structure_change';

  function gst_message_new_stream_status(src:PGstObject; _type:TGstStreamStatusType; owner:PGstElement):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_stream_status';

  procedure gst_message_parse_stream_status(message:PGstMessage; _type:PGstStreamStatusType; owner:PPGstElement);cdecl;external gstreamerlib name 'gst_message_parse_stream_status';

(* Const before type ignored *)
  procedure gst_message_set_stream_status_object(message:PGstMessage; object_:PGValue);cdecl;external gstreamerlib name 'gst_message_set_stream_status_object';

(* Const before type ignored *)
  function gst_message_get_stream_status_object(message:PGstMessage):PGValue;cdecl;external gstreamerlib name 'gst_message_get_stream_status_object';

  function gst_message_new_request_state(src:PGstObject; state:TGstState):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_request_state';

  procedure gst_message_parse_request_state(message:PGstMessage; state:PGstState);cdecl;external gstreamerlib name 'gst_message_parse_request_state';

  function gst_message_new_step_start(src:PGstObject; active:Tgboolean; format:TGstFormat; amount:Tguint64; rate:Tgdouble; 
             flush:Tgboolean; intermediate:Tgboolean):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_step_start';

  procedure gst_message_parse_step_start(message:PGstMessage; active:Pgboolean; format:PGstFormat; amount:Pguint64; rate:Pgdouble; 
              flush:Pgboolean; intermediate:Pgboolean);cdecl;external gstreamerlib name 'gst_message_parse_step_start';

  function gst_message_new_qos(src:PGstObject; live:Tgboolean; running_time:Tguint64; stream_time:Tguint64; timestamp:Tguint64; 
             duration:Tguint64):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_qos';

  procedure gst_message_set_qos_values(message:PGstMessage; jitter:Tgint64; proportion:Tgdouble; quality:Tgint);cdecl;external gstreamerlib name 'gst_message_set_qos_values';

  procedure gst_message_set_qos_stats(message:PGstMessage; format:TGstFormat; processed:Tguint64; dropped:Tguint64);cdecl;external gstreamerlib name 'gst_message_set_qos_stats';

  procedure gst_message_parse_qos(message:PGstMessage; live:Pgboolean; running_time:Pguint64; stream_time:Pguint64; timestamp:Pguint64; 
              duration:Pguint64);cdecl;external gstreamerlib name 'gst_message_parse_qos';

  procedure gst_message_parse_qos_values(message:PGstMessage; jitter:Pgint64; proportion:Pgdouble; quality:Pgint);cdecl;external gstreamerlib name 'gst_message_parse_qos_values';

  procedure gst_message_parse_qos_stats(message:PGstMessage; format:PGstFormat; processed:Pguint64; dropped:Pguint64);cdecl;external gstreamerlib name 'gst_message_parse_qos_stats';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_message_new_progress(src:PGstObject; _type:TGstProgressType; code:Pgchar; text:Pgchar):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_progress';

  procedure gst_message_parse_progress(message:PGstMessage; _type:PGstProgressType; code:PPgchar; text:PPgchar);cdecl;external gstreamerlib name 'gst_message_parse_progress';

  function gst_message_new_toc(src:PGstObject; toc:PGstToc; updated:Tgboolean):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_toc';

  procedure gst_message_parse_toc(message:PGstMessage; toc:PPGstToc; updated:Pgboolean);cdecl;external gstreamerlib name 'gst_message_parse_toc';

  function gst_message_new_reset_time(src:PGstObject; running_time:TGstClockTime):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_reset_time';

  procedure gst_message_parse_reset_time(message:PGstMessage; running_time:PGstClockTime);cdecl;external gstreamerlib name 'gst_message_parse_reset_time';

  function gst_message_new_stream_start(src:PGstObject):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_stream_start';

  procedure gst_message_set_group_id(message:PGstMessage; group_id:Tguint);cdecl;external gstreamerlib name 'gst_message_set_group_id';

  function gst_message_parse_group_id(message:PGstMessage; group_id:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_message_parse_group_id';

(* Const before type ignored *)
  function gst_message_new_need_context(src:PGstObject; context_type:Pgchar):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_need_context';

(* Const before type ignored *)
  function gst_message_parse_context_type(message:PGstMessage; context_type:PPgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_message_parse_context_type';

  function gst_message_new_have_context(src:PGstObject; context:PGstContext):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_have_context';

  procedure gst_message_parse_have_context(message:PGstMessage; context:PPGstContext);cdecl;external gstreamerlib name 'gst_message_parse_have_context';

  function gst_message_new_device_added(src:PGstObject; device:PGstDevice):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_device_added';

  procedure gst_message_parse_device_added(message:PGstMessage; device: PPGstDevice);cdecl;external gstreamerlib name 'gst_message_parse_device_added';

  function gst_message_new_device_removed(src:PGstObject; device:PGstDevice):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_device_removed';

  procedure gst_message_parse_device_removed(message:PGstMessage; device:PPGstDevice);cdecl;external gstreamerlib name 'gst_message_parse_device_removed';

  function gst_message_new_device_changed(src:PGstObject; device:PGstDevice; changed_device:PGstDevice):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_device_changed';

  procedure gst_message_parse_device_changed(message:PGstMessage; device:PPGstDevice; changed_device:PPGstDevice);cdecl;external gstreamerlib name 'gst_message_parse_device_changed';

(* Const before type ignored *)
  function gst_message_new_property_notify(src:PGstObject; property_name:Pgchar; val:PGValue):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_property_notify';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_message_parse_property_notify(message:PGstMessage; object_:PPGstObject; property_name:PPgchar; property_value:PPGValue);cdecl;external gstreamerlib name 'gst_message_parse_property_notify';

  function gst_message_new_stream_collection(src:PGstObject; collection:PGstStreamCollection):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_stream_collection';

  procedure gst_message_parse_stream_collection(message:PGstMessage; collection:PPGstStreamCollection);cdecl;external gstreamerlib name 'gst_message_parse_stream_collection';

  function gst_message_new_streams_selected(src:PGstObject; collection:PGstStreamCollection):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_streams_selected';

  procedure gst_message_streams_selected_add(message:PGstMessage; stream:PGstStream);cdecl;external gstreamerlib name 'gst_message_streams_selected_add';

  procedure gst_message_parse_streams_selected(message:PGstMessage; collection:PPGstStreamCollection);cdecl;external gstreamerlib name 'gst_message_parse_streams_selected';

  function gst_message_streams_selected_get_size(message:PGstMessage):Tguint;cdecl;external gstreamerlib name 'gst_message_streams_selected_get_size';

  function gst_message_streams_selected_get_stream(message:PGstMessage; idx:Tguint):PGstStream;cdecl;external gstreamerlib name 'gst_message_streams_selected_get_stream';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_message_new_redirect(src:PGstObject; location:Pgchar; tag_list:PGstTagList; entry_struct:PGstStructure):PGstMessage;cdecl;external gstreamerlib name 'gst_message_new_redirect';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_message_add_redirect_entry(message:PGstMessage; location:Pgchar; tag_list:PGstTagList; entry_struct:PGstStructure);cdecl;external gstreamerlib name 'gst_message_add_redirect_entry';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_message_parse_redirect_entry(message:PGstMessage; entry_index:Tgsize; location:PPgchar; tag_list:PPGstTagList; entry_struct:PPGstStructure);cdecl;external gstreamerlib name 'gst_message_parse_redirect_entry';

  function gst_message_get_num_redirect_entries(message:PGstMessage):Tgsize;cdecl;external gstreamerlib name 'gst_message_get_num_redirect_entries';


  var
    _gst_message_type : TGType;cvar;external;

// Macros

Function GST_TYPE_MESSAGE : TGType;
function GST_IS_MESSAGE(obj : Pointer) : boolean;
Function GST_MESSAGE_CAST(obj : Pointer) : PGstMessage;
Function GST_MESSAGE(obj : Pointer) : PGstMessage;
Function GST_MESSAGE_TYPE(message : PGStMessage) : TGstMessageType;
function GST_MESSAGE_GET_LOCK(message : PGstmessage): PGMutex;
procedure GST_MESSAGE_LOCK(message : PGstMessage);
procedure GST_MESSAGE_UNLOCK(message : PGstMessage);
function GST_MESSAGE_GET_COND(message : PGstmessage): PGCond;
procedure GST_MESSAGE_WAIT(message : PGstMessage);
procedure GST_MESSAGE_SIGNAL(message : PGSTMessage);

// Inlinc
function gst_message_ref (msg : PGstMessage) : PGstmessage;
procedure gst_message_unref (msg : PGstMessage);
function gst_message_copy (msg : PGstMessage) : PGstMessage;
procedure gst_clear_message (msg_ptr : PPGstMessage);
function gst_message_replace (old_message : PPGstMessage; new_message : PGstMessage) : gboolean;
Function gst_message_take (old_message : PPGstMessage; new_message : PGstMessage) :gboolean;

