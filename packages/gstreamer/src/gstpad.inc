{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *
 * gstpad.h: Header for GstPad object
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
    P_GstPad  = ^T_GstPad;
    P_GstPadClass  = ^T_GstPadClass;
    P_GstPadProbeInfo  = ^T_GstPadProbeInfo;
    PGstFlowReturn  = ^TGstFlowReturn;
    PGstPad_autoptr  = ^TGstPad_autoptr;
    PGstPad_listautoptr  = ^TGstPad_listautoptr;
    PGstPad_queueautoptr  = ^TGstPad_queueautoptr;
    PGstPad_slistautoptr  = ^TGstPad_slistautoptr;
    PGstPadFlags  = ^TGstPadFlags;
    PGstPadIterIntLinkFunction  = ^TGstPadIterIntLinkFunction;
    PGstPadLinkCheck  = ^TGstPadLinkCheck;
    PGstPadProbeInfo  = ^TGstPadProbeInfo;
    PGstPadProbeReturn  = ^TGstPadProbeReturn;
    PGstPadProbeType  = ^TGstPadProbeType;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  TGstPadProbeType = (
    GST_PAD_PROBE_TYPE_INVALID := 0,
    GST_PAD_PROBE_TYPE_IDLE := 1 shl 0,
    GST_PAD_PROBE_TYPE_BLOCK := 1 shl 1,
    GST_PAD_PROBE_TYPE_BUFFER := 1 shl 4,
    GST_PAD_PROBE_TYPE_BUFFER_LIST := 1 shl 5,
    GST_PAD_PROBE_TYPE_EVENT_DOWNSTREAM := 1 shl 6,
    GST_PAD_PROBE_TYPE_EVENT_UPSTREAM := 1 shl 7,
    GST_PAD_PROBE_TYPE_EVENT_FLUSH := 1 shl 8,
    GST_PAD_PROBE_TYPE_QUERY_DOWNSTREAM := 1 shl 9,
    GST_PAD_PROBE_TYPE_QUERY_UPSTREAM := 1 shl 10,
    GST_PAD_PROBE_TYPE_PUSH := 1 shl 12,
    GST_PAD_PROBE_TYPE_PULL := 1 shl 13,
    GST_PAD_PROBE_TYPE_BLOCKING := Ord(GST_PAD_PROBE_TYPE_IDLE) or Ord(GST_PAD_PROBE_TYPE_BLOCK),
    GST_PAD_PROBE_TYPE_DATA_DOWNSTREAM := Ord(GST_PAD_PROBE_TYPE_BUFFER) or Ord(GST_PAD_PROBE_TYPE_BUFFER_LIST) or Ord(GST_PAD_PROBE_TYPE_EVENT_DOWNSTREAM),
    GST_PAD_PROBE_TYPE_DATA_UPSTREAM := Ord(GST_PAD_PROBE_TYPE_EVENT_UPSTREAM),
    GST_PAD_PROBE_TYPE_DATA_BOTH := Ord(GST_PAD_PROBE_TYPE_DATA_DOWNSTREAM) or Ord(GST_PAD_PROBE_TYPE_DATA_UPSTREAM),
    GST_PAD_PROBE_TYPE_BLOCK_DOWNSTREAM := Ord(GST_PAD_PROBE_TYPE_BLOCK) or Ord(GST_PAD_PROBE_TYPE_DATA_DOWNSTREAM),
    GST_PAD_PROBE_TYPE_BLOCK_UPSTREAM := Ord(GST_PAD_PROBE_TYPE_BLOCK) or Ord(GST_PAD_PROBE_TYPE_DATA_UPSTREAM),
    GST_PAD_PROBE_TYPE_EVENT_BOTH := Ord(GST_PAD_PROBE_TYPE_EVENT_DOWNSTREAM) or Ord(GST_PAD_PROBE_TYPE_EVENT_UPSTREAM),
    GST_PAD_PROBE_TYPE_QUERY_BOTH := Ord(GST_PAD_PROBE_TYPE_QUERY_DOWNSTREAM) or Ord(GST_PAD_PROBE_TYPE_QUERY_UPSTREAM),
    GST_PAD_PROBE_TYPE_ALL_BOTH := Ord(GST_PAD_PROBE_TYPE_DATA_BOTH) or Ord(GST_PAD_PROBE_TYPE_QUERY_BOTH),
    GST_PAD_PROBE_TYPE_SCHEDULING := Ord(GST_PAD_PROBE_TYPE_PUSH) or Ord(GST_PAD_PROBE_TYPE_PULL)
    );

  TGstPadProbeReturn = (GST_PAD_PROBE_DROP,GST_PAD_PROBE_OK,GST_PAD_PROBE_REMOVE,
    GST_PAD_PROBE_PASS,GST_PAD_PROBE_HANDLED
    );
  T_GstPadProbeInfo = record
      _type : TGstPadProbeType;
      id : Tgulong;
      data : Tgpointer;
      offset : Tguint64;
      size : Tguint;
      ABI : record
          case longint of
            0 : ( _gst_reserved : array[0..3] of Tgpointer );
            1 : ( abi : record
                flow_ret : TGstFlowReturn;
              end );
          end;
    end;
  TGstPadProbeInfo = T_GstPadProbeInfo;

  TGstPadProbeCallback = function (pad:PGstPad; info:PGstPadProbeInfo; user_data:Tgpointer):TGstPadProbeReturn;cdecl;

  TGstPadStickyEventsForeachFunction = function (pad:PGstPad; event:PPGstEvent; user_data:Tgpointer):Tgboolean;cdecl;


  TGstPadFlags = (GST_PAD_FLAG_BLOCKED := GST_OBJECT_FLAG_LAST shl 0,GST_PAD_FLAG_FLUSHING := GST_OBJECT_FLAG_LAST shl 1,
    GST_PAD_FLAG_EOS := GST_OBJECT_FLAG_LAST shl 2,GST_PAD_FLAG_BLOCKING := GST_OBJECT_FLAG_LAST shl 3,
    GST_PAD_FLAG_NEED_PARENT := GST_OBJECT_FLAG_LAST shl 4,GST_PAD_FLAG_NEED_RECONFIGURE := GST_OBJECT_FLAG_LAST shl 5,
    GST_PAD_FLAG_PENDING_EVENTS := GST_OBJECT_FLAG_LAST shl 6,GST_PAD_FLAG_FIXED_CAPS := GST_OBJECT_FLAG_LAST shl 7,
    GST_PAD_FLAG_PROXY_CAPS := GST_OBJECT_FLAG_LAST shl 8,GST_PAD_FLAG_PROXY_ALLOCATION := GST_OBJECT_FLAG_LAST shl 9,
    GST_PAD_FLAG_PROXY_SCHEDULING := GST_OBJECT_FLAG_LAST shl 10,
    GST_PAD_FLAG_ACCEPT_INTERSECT := GST_OBJECT_FLAG_LAST shl 11,
    GST_PAD_FLAG_ACCEPT_TEMPLATE := GST_OBJECT_FLAG_LAST shl 12,GST_PAD_FLAG_LAST := GST_OBJECT_FLAG_LAST shl 16
    );


  T_GstPadClass = record
      parent_class : TGstObjectClass;
      linked : procedure (pad:PGstPad; peer:PGstPad);cdecl;
      unlinked : procedure (pad:PGstPad; peer:PGstPad);cdecl;
      _gst_reserved : array[0..3] of Tgpointer;
    end;
  TGstPadClass = T_GstPadClass;

  TGstPad_autoptr = PGstPad;

  TGstPad_listautoptr = PGList;

  TGstPad_slistautoptr = PGSList;

  TGstPad_queueautoptr = PGQueue;


  function gst_flow_get_name(ret:TGstFlowReturn):Pgchar;cdecl;external gstreamerlib name 'gst_flow_get_name';

  function gst_flow_to_quark(ret:TGstFlowReturn):TGQuark;cdecl;external gstreamerlib name 'gst_flow_to_quark';

(* Const before type ignored *)
  function gst_pad_link_get_name(ret:TGstPadLinkReturn):Pgchar;cdecl;external gstreamerlib name 'gst_pad_link_get_name';




  function gst_pad_probe_info_get_event(info:PGstPadProbeInfo):PGstEvent;cdecl;external gstreamerlib name 'gst_pad_probe_info_get_event';

  function gst_pad_probe_info_get_query(info:PGstPadProbeInfo):PGstQuery;cdecl;external gstreamerlib name 'gst_pad_probe_info_get_query';

  function gst_pad_probe_info_get_buffer(info:PGstPadProbeInfo):PGstBuffer;cdecl;external gstreamerlib name 'gst_pad_probe_info_get_buffer';

  function gst_pad_probe_info_get_buffer_list(info:PGstPadProbeInfo):PGstBufferList;cdecl;external gstreamerlib name 'gst_pad_probe_info_get_buffer_list';




  function gst_pad_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_get_type';

(* Const before type ignored *)
  function gst_pad_new(name:Pgchar; direction:TGstPadDirection):PGstPad;cdecl;external gstreamerlib name 'gst_pad_new';

(* Const before type ignored *)
  function gst_pad_new_from_template(templ:PGstPadTemplate; name:Pgchar):PGstPad;cdecl;external gstreamerlib name 'gst_pad_new_from_template';

(* Const before type ignored *)
  function gst_pad_new_from_static_template(templ:PGstStaticPadTemplate; name:Pgchar):PGstPad;cdecl;external gstreamerlib name 'gst_pad_new_from_static_template';

  function gst_pad_get_direction(pad:PGstPad):TGstPadDirection;cdecl;external gstreamerlib name 'gst_pad_get_direction';

  function gst_pad_set_active(pad:PGstPad; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_set_active';

  function gst_pad_is_active(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_is_active';

  function gst_pad_activate_mode(pad:PGstPad; mode:TGstPadMode; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_activate_mode';

  function gst_pad_add_probe(pad:PGstPad; mask:TGstPadProbeType; callback:TGstPadProbeCallback; user_data:Tgpointer; destroy_data:TGDestroyNotify):Tgulong;cdecl;external gstreamerlib name 'gst_pad_add_probe';

  procedure gst_pad_remove_probe(pad:PGstPad; id:Tgulong);cdecl;external gstreamerlib name 'gst_pad_remove_probe';

  function gst_pad_is_blocked(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_is_blocked';

  function gst_pad_is_blocking(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_is_blocking';

  procedure gst_pad_mark_reconfigure(pad:PGstPad);cdecl;external gstreamerlib name 'gst_pad_mark_reconfigure';

  function gst_pad_needs_reconfigure(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_needs_reconfigure';

  function gst_pad_check_reconfigure(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_check_reconfigure';

  procedure gst_pad_set_element_private(pad:PGstPad; priv:Tgpointer);cdecl;external gstreamerlib name 'gst_pad_set_element_private';

  function gst_pad_get_element_private(pad:PGstPad):Tgpointer;cdecl;external gstreamerlib name 'gst_pad_get_element_private';

  function gst_pad_get_pad_template(pad:PGstPad):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_pad_get_pad_template';

  function gst_pad_store_sticky_event(pad:PGstPad; event:PGstEvent):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_store_sticky_event';

  function gst_pad_get_sticky_event(pad:PGstPad; event_type:TGstEventType; idx:Tguint):PGstEvent;cdecl;external gstreamerlib name 'gst_pad_get_sticky_event';

  procedure gst_pad_sticky_events_foreach(pad:PGstPad; foreach_func:TGstPadStickyEventsForeachFunction; user_data:Tgpointer);cdecl;external gstreamerlib name 'gst_pad_sticky_events_foreach';

  procedure gst_pad_set_activate_function_full(pad:PGstPad; activate:TGstPadActivateFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_activate_function_full';

  procedure gst_pad_set_activatemode_function_full(pad:PGstPad; activatemode:TGstPadActivateModeFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_activatemode_function_full';

  procedure gst_pad_set_chain_function_full(pad:PGstPad; chain:TGstPadChainFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_chain_function_full';

  procedure gst_pad_set_chain_list_function_full(pad:PGstPad; chainlist:TGstPadChainListFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_chain_list_function_full';

  procedure gst_pad_set_getrange_function_full(pad:PGstPad; get:TGstPadGetRangeFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_getrange_function_full';

  procedure gst_pad_set_event_function_full(pad:PGstPad; event:TGstPadEventFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_event_function_full';

  procedure gst_pad_set_event_full_function_full(pad:PGstPad; event:TGstPadEventFullFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_event_full_function_full';

  procedure gst_pad_set_link_function_full(pad:PGstPad; link:TGstPadLinkFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_link_function_full';

  procedure gst_pad_set_unlink_function_full(pad:PGstPad; unlink:TGstPadUnlinkFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_unlink_function_full';

  function gst_pad_can_link(srcpad:PGstPad; sinkpad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_can_link';

  function gst_pad_link(srcpad:PGstPad; sinkpad:PGstPad):TGstPadLinkReturn;cdecl;external gstreamerlib name 'gst_pad_link';

  function gst_pad_link_full(srcpad:PGstPad; sinkpad:PGstPad; flags:TGstPadLinkCheck):TGstPadLinkReturn;cdecl;external gstreamerlib name 'gst_pad_link_full';

  function gst_pad_unlink(srcpad:PGstPad; sinkpad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_unlink';

  function gst_pad_is_linked(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_is_linked';

  function gst_pad_get_peer(pad:PGstPad):PGstPad;cdecl;external gstreamerlib name 'gst_pad_get_peer';

  function gst_pad_get_pad_template_caps(pad:PGstPad):PGstCaps;cdecl;external gstreamerlib name 'gst_pad_get_pad_template_caps';

  function gst_pad_get_current_caps(pad:PGstPad):PGstCaps;cdecl;external gstreamerlib name 'gst_pad_get_current_caps';

  function gst_pad_has_current_caps(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_has_current_caps';

  function gst_pad_get_allowed_caps(pad:PGstPad):PGstCaps;cdecl;external gstreamerlib name 'gst_pad_get_allowed_caps';

  function gst_pad_get_offset(pad:PGstPad):Tgint64;cdecl;external gstreamerlib name 'gst_pad_get_offset';

  procedure gst_pad_set_offset(pad:PGstPad; offset:Tgint64);cdecl;external gstreamerlib name 'gst_pad_set_offset';

  function gst_pad_push(pad:PGstPad; buffer:PGstBuffer):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_push';

  function gst_pad_push_list(pad:PGstPad; list:PGstBufferList):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_push_list';

  function gst_pad_pull_range(pad:PGstPad; offset:Tguint64; size:Tguint; buffer:PPGstBuffer):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_pull_range';

  function gst_pad_push_event(pad:PGstPad; event:PGstEvent):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_push_event';

  function gst_pad_event_default(pad:PGstPad; parent:PGstObject; event:PGstEvent):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_event_default';

  function gst_pad_get_last_flow_return(pad:PGstPad):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_get_last_flow_return';

  function gst_pad_chain(pad:PGstPad; buffer:PGstBuffer):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_chain';

  function gst_pad_chain_list(pad:PGstPad; list:PGstBufferList):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_chain_list';

  function gst_pad_get_range(pad:PGstPad; offset:Tguint64; size:Tguint; buffer:PPGstBuffer):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_pad_get_range';

  function gst_pad_send_event(pad:PGstPad; event:PGstEvent):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_send_event';

  function gst_pad_start_task(pad:PGstPad; func:TGstTaskFunction; user_data:Tgpointer; notify:TGDestroyNotify):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_start_task';

  function gst_pad_pause_task(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_pause_task';

  function gst_pad_stop_task(pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_stop_task';

  function gst_pad_get_task_state(pad:PGstPad):TGstTaskState;cdecl;external gstreamerlib name 'gst_pad_get_task_state';

  procedure gst_pad_set_iterate_internal_links_function_full(pad:PGstPad; iterintlink:TGstPadIterIntLinkFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_iterate_internal_links_function_full';

  function gst_pad_iterate_internal_links(pad:PGstPad):PGstIterator;cdecl;external gstreamerlib name 'gst_pad_iterate_internal_links';

  function gst_pad_iterate_internal_links_default(pad:PGstPad; parent:PGstObject):PGstIterator;cdecl;external gstreamerlib name 'gst_pad_iterate_internal_links_default';

  function gst_pad_query(pad:PGstPad; query:PGstQuery):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_query';

  function gst_pad_peer_query(pad:PGstPad; query:PGstQuery):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_peer_query';

  procedure gst_pad_set_query_function_full(pad:PGstPad; query:TGstPadQueryFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_pad_set_query_function_full';

  function gst_pad_query_default(pad:PGstPad; parent:PGstObject; query:PGstQuery):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_query_default';

  function gst_pad_forward(pad:PGstPad; forward:TGstPadForwardFunction; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_forward';


