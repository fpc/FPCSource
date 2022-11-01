{* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *               2000,2004 Wim Taymans <wim@fluendo.com>
 *
 * gstelement.h: Header for GstElement
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
 *}


Type
  P_GstElement  = ^T_GstElement;
  PPGstElement = ^PGstElement;
  P_GstElementClass  = ^T_GstElementClass;

  PGstElement_autoptr  = ^TGstElement_autoptr;
  PGstElement_listautoptr  = ^TGstElement_listautoptr;
  PGstElement_queueautoptr  = ^TGstElement_queueautoptr;
  PGstElement_slistautoptr  = ^TGstElement_slistautoptr;
  PGstElementClass  = ^TGstElementClass;
  PGstElementFlags  = ^TGstElementFlags;

  PGstState  = ^TGstState;
  PGstStateChange  = ^TGstStateChange;


  T_GstElementClass = record
    parent_class : TGstObjectClass;
    metadata : Tgpointer;
    elementfactory : PGstElementFactory;
    padtemplates : PGList;
    numpadtemplates : Tgint;
    pad_templ_cookie : Tguint32;
    pad_added : procedure (element:PGstElement; pad:PGstPad);cdecl;
    pad_removed : procedure (element:PGstElement; pad:PGstPad);cdecl;
    no_more_pads : procedure (element:PGstElement);cdecl;
    request_new_pad : function (element:PGstElement; templ:PGstPadTemplate; name:Pgchar; caps:PGstCaps):PGstPad;cdecl;
    release_pad : procedure (element:PGstElement; pad:PGstPad);cdecl;
    get_state : function (element:PGstElement; state:PGstState; pending:PGstState; timeout:TGstClockTime):TGstStateChangeReturn;cdecl;
    set_state : function (element:PGstElement; state:TGstState):TGstStateChangeReturn;cdecl;
    change_state : function (element:PGstElement; transition:TGstStateChange):TGstStateChangeReturn;cdecl;
    state_changed : procedure (element:PGstElement; oldstate:TGstState; newstate:TGstState; pending:TGstState);cdecl;
    set_bus : procedure (element:PGstElement; bus:PGstBus);cdecl;
    provide_clock : function (element:PGstElement):PGstClock;cdecl;
    set_clock : function (element:PGstElement; clock:PGstClock):Tgboolean;cdecl;
    send_event : function (element:PGstElement; event:PGstEvent):Tgboolean;cdecl;
    query : function (element:PGstElement; query:PGstQuery):Tgboolean;cdecl;
    post_message : function (element:PGstElement; message:PGstMessage):Tgboolean;cdecl;
    set_context : procedure (element:PGstElement; context:PGstContext);cdecl;
    _gst_reserved : array[0..(20-2)-1] of Tgpointer;
  end;
  TGstElementClass = T_GstElementClass;


  TGstElement_autoptr = PGstElement;
  TGstElement_listautoptr = PGList;
  TGstElement_slistautoptr = PGSList;
  TGstElement_queueautoptr = PGQueue;


  function gst_make_element_message_details(name:Pchar; args:array of const):PGstStructure;cdecl;external gstreamerlib name 'gst_make_element_message_details';
  function gst_make_element_message_details(name:Pchar):PGstStructure;cdecl;external gstreamerlib name 'gst_make_element_message_details';


  procedure gst_element_class_add_pad_template(klass:PGstElementClass; templ:PGstPadTemplate);cdecl;external gstreamerlib name 'gst_element_class_add_pad_template';

  procedure gst_element_class_add_static_pad_template(klass:PGstElementClass; static_templ:PGstStaticPadTemplate);cdecl;external gstreamerlib name 'gst_element_class_add_static_pad_template';

  procedure gst_element_class_add_static_pad_template_with_gtype(klass:PGstElementClass; static_templ:PGstStaticPadTemplate; pad_type:TGType);cdecl;external gstreamerlib name 'gst_element_class_add_static_pad_template_with_gtype';

  function gst_element_class_get_pad_template(element_class:PGstElementClass; name:Pgchar):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_element_class_get_pad_template';

  function gst_element_class_get_pad_template_list(element_class:PGstElementClass):PGList;cdecl;external gstreamerlib name 'gst_element_class_get_pad_template_list';

  procedure gst_element_class_set_metadata(klass:PGstElementClass; longname:Pgchar; classification:Pgchar; description:Pgchar; author:Pgchar);cdecl;external gstreamerlib name 'gst_element_class_set_metadata';

  procedure gst_element_class_set_static_metadata(klass:PGstElementClass; longname:Pgchar; classification:Pgchar; description:Pgchar; author:Pgchar);cdecl;external gstreamerlib name 'gst_element_class_set_static_metadata';

  procedure gst_element_class_add_metadata(klass:PGstElementClass; key:Pgchar; value:Pgchar);cdecl;external gstreamerlib name 'gst_element_class_add_metadata';

  procedure gst_element_class_add_static_metadata(klass:PGstElementClass; key:Pgchar; value:Pgchar);cdecl;external gstreamerlib name 'gst_element_class_add_static_metadata';

  function gst_element_class_get_metadata(klass:PGstElementClass; key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_element_class_get_metadata';

  function gst_element_get_type:TGType;cdecl;external gstreamerlib name 'gst_element_get_type';

  function gst_element_provide_clock(element:PGstElement):PGstClock;cdecl;external gstreamerlib name 'gst_element_provide_clock';

  function gst_element_get_clock(element:PGstElement):PGstClock;cdecl;external gstreamerlib name 'gst_element_get_clock';

  function gst_element_set_clock(element:PGstElement; clock:PGstClock):Tgboolean;cdecl;external gstreamerlib name 'gst_element_set_clock';

  procedure gst_element_set_base_time(element:PGstElement; time:TGstClockTime);cdecl;external gstreamerlib name 'gst_element_set_base_time';

  function gst_element_get_base_time(element:PGstElement):TGstClockTime;cdecl;external gstreamerlib name 'gst_element_get_base_time';

  procedure gst_element_set_start_time(element:PGstElement; time:TGstClockTime);cdecl;external gstreamerlib name 'gst_element_set_start_time';

  function gst_element_get_start_time(element:PGstElement):TGstClockTime;cdecl;external gstreamerlib name 'gst_element_get_start_time';

  procedure gst_element_set_bus(element:PGstElement; bus:PGstBus);cdecl;external gstreamerlib name 'gst_element_set_bus';

  function gst_element_get_bus(element:PGstElement):PGstBus;cdecl;external gstreamerlib name 'gst_element_get_bus';

  procedure gst_element_set_context(element:PGstElement; context:PGstContext);cdecl;external gstreamerlib name 'gst_element_set_context';

  function gst_element_get_contexts(element:PGstElement):PGList;cdecl;external gstreamerlib name 'gst_element_get_contexts';

(* Const before type ignored *)
  function gst_element_get_context(element:PGstElement; context_type:Pgchar):PGstContext;cdecl;external gstreamerlib name 'gst_element_get_context';

(* Const before type ignored *)
  function gst_element_get_context_unlocked(element:PGstElement; context_type:Pgchar):PGstContext;cdecl;external gstreamerlib name 'gst_element_get_context_unlocked';

  function gst_element_add_pad(element:PGstElement; pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_element_add_pad';

  function gst_element_remove_pad(element:PGstElement; pad:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_element_remove_pad';

  procedure gst_element_no_more_pads(element:PGstElement);cdecl;external gstreamerlib name 'gst_element_no_more_pads';

(* Const before type ignored *)
  function gst_element_get_static_pad(element:PGstElement; name:Pgchar):PGstPad;cdecl;external gstreamerlib name 'gst_element_get_static_pad';

(* Const before type ignored *)
  function gst_element_get_request_pad(element:PGstElement; name:Pgchar):PGstPad;cdecl;external gstreamerlib name 'gst_element_get_request_pad';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_element_request_pad(element:PGstElement; templ:PGstPadTemplate; name:Pgchar; caps:PGstCaps):PGstPad;cdecl;external gstreamerlib name 'gst_element_request_pad';

  procedure gst_element_release_request_pad(element:PGstElement; pad:PGstPad);cdecl;external gstreamerlib name 'gst_element_release_request_pad';

  function gst_element_iterate_pads(element:PGstElement):PGstIterator;cdecl;external gstreamerlib name 'gst_element_iterate_pads';

  function gst_element_iterate_src_pads(element:PGstElement):PGstIterator;cdecl;external gstreamerlib name 'gst_element_iterate_src_pads';

  function gst_element_iterate_sink_pads(element:PGstElement):PGstIterator;cdecl;external gstreamerlib name 'gst_element_iterate_sink_pads';


  type

    TGstElementForeachPadFunc = function (element:PGstElement; pad:PGstPad; user_data:Tgpointer):Tgboolean;cdecl;

  function gst_element_foreach_sink_pad(element:PGstElement; func:TGstElementForeachPadFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_element_foreach_sink_pad';

  function gst_element_foreach_src_pad(element:PGstElement; func:TGstElementForeachPadFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_element_foreach_src_pad';

  function gst_element_foreach_pad(element:PGstElement; func:TGstElementForeachPadFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_element_foreach_pad';

  function gst_element_send_event(element:PGstElement; event:PGstEvent):Tgboolean;cdecl;external gstreamerlib name 'gst_element_send_event';

  function gst_element_seek(element:PGstElement; rate:Tgdouble; format:TGstFormat; flags:TGstSeekFlags; start_type:TGstSeekType; 
             start:Tgint64; stop_type:TGstSeekType; stop:Tgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_element_seek';

  function gst_element_query(element:PGstElement; query:PGstQuery):Tgboolean;cdecl;external gstreamerlib name 'gst_element_query';

  function gst_element_post_message(element:PGstElement; message:PGstMessage):Tgboolean;cdecl;external gstreamerlib name 'gst_element_post_message';

(* Const before type ignored *)
  function _gst_element_error_printf(format:Pgchar; args:array of const):Pgchar;cdecl;external gstreamerlib name '_gst_element_error_printf';

  function _gst_element_error_printf(format:Pgchar):Pgchar;cdecl;external gstreamerlib name '_gst_element_error_printf';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_element_message_full(element:PGstElement; _type:TGstMessageType; domain:TGQuark; code:Tgint; text:Pgchar; 
              debug:Pgchar; afile:Pgchar; _function:Pgchar; line:Tgint);cdecl;external gstreamerlib name 'gst_element_message_full';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_element_message_full_with_details(element:PGstElement; _type:TGstMessageType; domain:TGQuark; code:Tgint; text:Pgchar; 
              debug:Pgchar; afile:Pgchar; _function:Pgchar; line:Tgint; structure:PGstStructure);cdecl;external gstreamerlib name 'gst_element_message_full_with_details';

  function gst_element_is_locked_state(element:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_element_is_locked_state';

  function gst_element_set_locked_state(element:PGstElement; locked_state:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_element_set_locked_state';

  function gst_element_sync_state_with_parent(element:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_element_sync_state_with_parent';

  function gst_element_get_state(element:PGstElement; state:PGstState; pending:PGstState; timeout:TGstClockTime):TGstStateChangeReturn;cdecl;external gstreamerlib name 'gst_element_get_state';

  function gst_element_set_state(element:PGstElement; state:TGstState):TGstStateChangeReturn;cdecl;external gstreamerlib name 'gst_element_set_state';

  procedure gst_element_abort_state(element:PGstElement);cdecl;external gstreamerlib name 'gst_element_abort_state';

  function gst_element_change_state(element:PGstElement; transition:TGstStateChange):TGstStateChangeReturn;cdecl;external gstreamerlib name 'gst_element_change_state';

  function gst_element_continue_state(element:PGstElement; ret:TGstStateChangeReturn):TGstStateChangeReturn;cdecl;external gstreamerlib name 'gst_element_continue_state';

  procedure gst_element_lost_state(element:PGstElement);cdecl;external gstreamerlib name 'gst_element_lost_state';


  type

    TGstElementCallAsyncFunc = procedure (element:PGstElement; user_data:Tgpointer);cdecl;

  procedure gst_element_call_async(element:PGstElement; func:TGstElementCallAsyncFunc; user_data:Tgpointer; destroy_notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_element_call_async';

  function gst_element_get_factory(element:PGstElement):PGstElementFactory;cdecl;external gstreamerlib name 'gst_element_get_factory';

(* Const before type ignored *)
  function gst_element_add_property_notify_watch(element:PGstElement; property_name:Pgchar; include_value:Tgboolean):Tgulong;cdecl;external gstreamerlib name 'gst_element_add_property_notify_watch';

(* Const before type ignored *)
  function gst_element_add_property_deep_notify_watch(element:PGstElement; property_name:Pgchar; include_value:Tgboolean):Tgulong;cdecl;external gstreamerlib name 'gst_element_add_property_deep_notify_watch';

  procedure gst_element_remove_property_notify_watch(element:PGstElement; watch_id:Tgulong);cdecl;external gstreamerlib name 'gst_element_remove_property_notify_watch';

(* Const before type ignored *)
  function gst_element_get_pad_template(element:PGstElement; name:Pgchar):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_element_get_pad_template';

  function gst_element_get_pad_template_list(element:PGstElement):PGList;cdecl;external gstreamerlib name 'gst_element_get_pad_template_list';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_element_get_metadata(element:PGstElement; key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_element_get_metadata';

