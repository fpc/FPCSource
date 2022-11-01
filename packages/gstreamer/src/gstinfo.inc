{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *                    2003 Benjamin Otte <in7y118@public.uni-hamburg.de>
 *
 * gstinfo.h: debugging functions
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
    P_GstDebugCategory  = ^T_GstDebugCategory;
    PGstDebugCategory  = ^TGstDebugCategory;
    PGstDebugColorFlags  = ^TGstDebugColorFlags;
    PGstDebugColorMode  = ^TGstDebugColorMode;
    PGstDebugLevel  = ^TGstDebugLevel;
    PGstDebugMessage  = ^TGstDebugMessage;
    PGstStackTraceFlags  = ^TGstStackTraceFlags;


    TGstDebugLevel = (
      GST_LEVEL_NONE := 0,
      GST_LEVEL_ERROR := 1,
      GST_LEVEL_WARNING := 2,
      GST_LEVEL_FIXME := 3,
      GST_LEVEL_INFO := 4,
      GST_LEVEL_DEBUG := 5,
      GST_LEVEL_LOG := 6,
      GST_LEVEL_TRACE := 7,
      GST_LEVEL_MEMDUMP := 9,
      GST_LEVEL_COUNT
      );

    TGstDebugColorFlags = (
      GST_DEBUG_FG_BLACK := $0000,
      GST_DEBUG_FG_RED := $0001,
      GST_DEBUG_FG_GREEN := $0002,
      GST_DEBUG_FG_YELLOW := $0003,
      GST_DEBUG_FG_BLUE := $0004,
      GST_DEBUG_FG_MAGENTA := $0005,
      GST_DEBUG_FG_CYAN := $0006,
      GST_DEBUG_FG_WHITE := $0007,
      GST_DEBUG_BG_BLACK := $0000,
      GST_DEBUG_BG_RED := $0010,
      GST_DEBUG_BG_GREEN := $0020,
      GST_DEBUG_BG_YELLOW := $0030,
      GST_DEBUG_BG_BLUE := $0040,
      GST_DEBUG_BG_MAGENTA := $0050,
      GST_DEBUG_BG_CYAN := $0060,
      GST_DEBUG_BG_WHITE := $0070,
      GST_DEBUG_BOLD := $0100,
      GST_DEBUG_UNDERLINE := $0200
      );


    TGstStackTraceFlags = (
      GST_STACK_TRACE_SHOW_FULL := 1 shl 0
    );


    TGstDebugColorMode = (
      GST_DEBUG_COLOR_MODE_OFF := 0,
      GST_DEBUG_COLOR_MODE_ON := 1,
      GST_DEBUG_COLOR_MODE_UNIX := 2
    );

    T_GstDebugCategory = record
        threshold : Tgint;
        color : Tguint;
        name : Pgchar;
        description : Pgchar;
      end;
    TGstDebugCategory = T_GstDebugCategory;

    TGstDebugMessage = record end;
    T_GstDebugMessage = TGstDebugMessage;

    TGstLogFunction = procedure (category:PGstDebugCategory; level:TGstDebugLevel; afile:Pgchar; _function:Pgchar; line:Tgint; 
                  aobject:PGObject; message:PGstDebugMessage; user_data:Tgpointer);cdecl;

  TGstDebugFuncPtr = procedure (_para1:pointer);cdecl;

  procedure gst_debug_log(category:PGstDebugCategory; level:TGstDebugLevel; afile:Pgchar; _function:Pgchar; line:Tgint; 
              object_:PGObject; format:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_debug_log';

  procedure gst_debug_log(category:PGstDebugCategory; level:TGstDebugLevel; afile:Pgchar; _function:Pgchar; line:Tgint; 
              object_:PGObject; format:Pgchar);cdecl;external gstreamerlib name 'gst_debug_log';

  procedure gst_debug_log_valist(category:PGstDebugCategory; level:TGstDebugLevel; afile:Pgchar; _function:Pgchar; line:Tgint;
              object_:PGObject; format:Pgchar; args:Tva_list);cdecl;external gstreamerlib name 'gst_debug_log_valist';

  function _gst_debug_category_new(name:Pgchar; color:Tguint; description:Pgchar):PGstDebugCategory;cdecl;external gstreamerlib name '_gst_debug_category_new';

  function _gst_debug_get_category(name:Pgchar):PGstDebugCategory;cdecl;external gstreamerlib name '_gst_debug_get_category';
  procedure _gst_debug_dump_mem(cat:PGstDebugCategory; afile:Pgchar; func:Pgchar; line:Tgint; obj:PGObject;
              msg:Pgchar; data:Pguint8; length:Tguint);cdecl;external gstreamerlib name '_gst_debug_dump_mem';


(* Const before type ignored *)

  procedure _gst_debug_register_funcptr(func:TGstDebugFuncPtr; ptrname:Pgchar);cdecl;external gstreamerlib name '_gst_debug_register_funcptr';

(* Const before type ignored *)
  function _gst_debug_nameof_funcptr(func:TGstDebugFuncPtr):Pgchar;cdecl;external gstreamerlib name '_gst_debug_nameof_funcptr';

(* Const before type ignored *)
  function gst_debug_message_get(message:PGstDebugMessage):Pgchar;cdecl;external gstreamerlib name 'gst_debug_message_get';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_debug_log_default(category:PGstDebugCategory; level:TGstDebugLevel; afile:Pgchar; _function:Pgchar; line:Tgint; 
              object_:PGObject; message:PGstDebugMessage; user_data:Tgpointer);cdecl;external gstreamerlib name 'gst_debug_log_default';

(* Const before type ignored *)
  function gst_debug_level_get_name(level:TGstDebugLevel):Pgchar;cdecl;external gstreamerlib name 'gst_debug_level_get_name';

  procedure gst_debug_add_log_function(func:TGstLogFunction; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_debug_add_log_function';

  function gst_debug_remove_log_function(func:TGstLogFunction):Tguint;cdecl;external gstreamerlib name 'gst_debug_remove_log_function';

  function gst_debug_remove_log_function_by_data(data:Tgpointer):Tguint;cdecl;external gstreamerlib name 'gst_debug_remove_log_function_by_data';

  procedure gst_debug_set_active(active:Tgboolean);cdecl;external gstreamerlib name 'gst_debug_set_active';

  function gst_debug_is_active:Tgboolean;cdecl;external gstreamerlib name 'gst_debug_is_active';

  procedure gst_debug_set_colored(colored:Tgboolean);cdecl;external gstreamerlib name 'gst_debug_set_colored';

  procedure gst_debug_set_color_mode(mode:TGstDebugColorMode);cdecl;external gstreamerlib name 'gst_debug_set_color_mode';

(* Const before type ignored *)
  procedure gst_debug_set_color_mode_from_string(mode:Pgchar);cdecl;external gstreamerlib name 'gst_debug_set_color_mode_from_string';

  function gst_debug_is_colored:Tgboolean;cdecl;external gstreamerlib name 'gst_debug_is_colored';

  function gst_debug_get_color_mode:TGstDebugColorMode;cdecl;external gstreamerlib name 'gst_debug_get_color_mode';

  procedure gst_debug_set_default_threshold(level:TGstDebugLevel);cdecl;external gstreamerlib name 'gst_debug_set_default_threshold';

  function gst_debug_get_default_threshold:TGstDebugLevel;cdecl;external gstreamerlib name 'gst_debug_get_default_threshold';

(* Const before type ignored *)
  procedure gst_debug_set_threshold_for_name(name:Pgchar; level:TGstDebugLevel);cdecl;external gstreamerlib name 'gst_debug_set_threshold_for_name';

(* Const before type ignored *)
  procedure gst_debug_set_threshold_from_string(list:Pgchar; reset:Tgboolean);cdecl;external gstreamerlib name 'gst_debug_set_threshold_from_string';

(* Const before type ignored *)
  procedure gst_debug_unset_threshold_for_name(name:Pgchar);cdecl;external gstreamerlib name 'gst_debug_unset_threshold_for_name';

  procedure gst_debug_category_free(category:PGstDebugCategory);cdecl;external gstreamerlib name 'gst_debug_category_free';

  procedure gst_debug_category_set_threshold(category:PGstDebugCategory; level:TGstDebugLevel);cdecl;external gstreamerlib name 'gst_debug_category_set_threshold';

  procedure gst_debug_category_reset_threshold(category:PGstDebugCategory);cdecl;external gstreamerlib name 'gst_debug_category_reset_threshold';

  function gst_debug_category_get_threshold(category:PGstDebugCategory):TGstDebugLevel;cdecl;external gstreamerlib name 'gst_debug_category_get_threshold';

(* Const before type ignored *)
  function gst_debug_category_get_name(category:PGstDebugCategory):Pgchar;cdecl;external gstreamerlib name 'gst_debug_category_get_name';

  function gst_debug_category_get_color(category:PGstDebugCategory):Tguint;cdecl;external gstreamerlib name 'gst_debug_category_get_color';

(* Const before type ignored *)
  function gst_debug_category_get_description(category:PGstDebugCategory):Pgchar;cdecl;external gstreamerlib name 'gst_debug_category_get_description';

  function gst_debug_get_all_categories:PGSList;cdecl;external gstreamerlib name 'gst_debug_get_all_categories';

  function gst_debug_construct_term_color(colorinfo:Tguint):Pgchar;cdecl;external gstreamerlib name 'gst_debug_construct_term_color';

  function gst_debug_construct_win_color(colorinfo:Tguint):Tgint;cdecl;external gstreamerlib name 'gst_debug_construct_win_color';

(* Const before type ignored *)
  function gst_info_vasprintf(result:PPgchar; format:Pgchar; args:Tva_list):Tgint;cdecl;external gstreamerlib name 'gst_info_vasprintf';

(* Const before type ignored *)
  function gst_info_strdup_vprintf(format:Pgchar; args:Tva_list):Pgchar;cdecl;external gstreamerlib name 'gst_info_strdup_vprintf';

(* Const before type ignored *)
  function gst_info_strdup_printf(format:Pgchar; args:array of const):Pgchar;cdecl;external gstreamerlib name 'gst_info_strdup_printf';

  function gst_info_strdup_printf(format:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_info_strdup_printf';

(* Const before type ignored *)
  procedure gst_print(format:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_print';

  procedure gst_print(format:Pgchar);cdecl;external gstreamerlib name 'gst_print';

(* Const before type ignored *)
  procedure gst_println(format:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_println';

  procedure gst_println(format:Pgchar);cdecl;external gstreamerlib name 'gst_println';

(* Const before type ignored *)
  procedure gst_printerr(format:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_printerr';

  procedure gst_printerr(format:Pgchar);cdecl;external gstreamerlib name 'gst_printerr';

(* Const before type ignored *)
  procedure gst_printerrln(format:Pgchar; args:array of const);cdecl;external gstreamerlib name 'gst_printerrln';

  procedure gst_printerrln(format:Pgchar);cdecl;external gstreamerlib name 'gst_printerrln';


    var
      GST_CAT_DEFAULT : PGstDebugCategory;cvar;external;
      _gst_debug_enabled : Tgboolean;cvar;external;
      _gst_debug_min : TGstDebugLevel;cvar;external;

  procedure gst_debug_print_stack_trace;cdecl;external gstreamerlib name 'gst_debug_print_stack_trace';

  function gst_debug_get_stack_trace(flags:TGstStackTraceFlags):Pgchar;cdecl;external gstreamerlib name 'gst_debug_get_stack_trace';

  procedure gst_debug_add_ring_buffer_logger(max_size_per_thread:Tguint; thread_timeout:Tguint);cdecl;external gstreamerlib name 'gst_debug_add_ring_buffer_logger';

  procedure gst_debug_remove_ring_buffer_logger;cdecl;external gstreamerlib name 'gst_debug_remove_ring_buffer_logger';

  function gst_debug_ring_buffer_logger_get_logs:PPgchar;cdecl;external gstreamerlib name 'gst_debug_ring_buffer_logger_get_logs';


