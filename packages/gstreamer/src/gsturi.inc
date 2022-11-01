{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *                    2014 David Waring, British Broadcasting Corporation
 *                             <david.waring@rd.bbc.co.uk>
 *
 * gsturi.h: Header for uri to element mappings and URI manipulation.
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
    P_GstUri  = ^T_GstUri;
    P_GstURIHandlerInterface  = ^T_GstURIHandlerInterface;
    PGHashTable  = ^TGHashTable;
    PGstUri  = ^TGstUri;
    PGstUri_autoptr  = ^TGstUri_autoptr;
    PGstUri_listautoptr  = ^TGstUri_listautoptr;
    PGstUri_queueautoptr  = ^TGstUri_queueautoptr;
    PGstUri_slistautoptr  = ^TGstUri_slistautoptr;
    PGstURIHandler  = ^TGstURIHandler;

    TGstURIError = (
      GST_URI_ERROR_UNSUPPORTED_PROTOCOL,
      GST_URI_ERROR_BAD_URI,
      GST_URI_ERROR_BAD_STATE,
      GST_URI_ERROR_BAD_REFERENCE
    );

    TGstURIType = (
      GST_URI_UNKNOWN,
      GST_URI_SINK,
      GST_URI_SRC
    );

    T_GstURIHandler = record end;
    TGstURIHandler = T_GstURIHandler;

    T_GstURIHandlerInterface = record
        parent : TGTypeInterface;
        get_type : function (_type:TGType):TGstURIType;cdecl;
        get_protocols : function (_type:TGType):PPgchar;cdecl;
        get_uri : function (handler:PGstURIHandler):Pgchar;cdecl;
        set_uri : function (handler:PGstURIHandler; uri:Pgchar; error:PPGError):Tgboolean;cdecl;
      end;

      
    T_GstUri = record
        {undefined structure}
      end;

    TGstUri = T_GstUri;

  TGstUri_autoptr = PGstUri;

  TGstUri_listautoptr = PGList;

  TGstUri_slistautoptr = PGSList;

  TGstUri_queueautoptr = PGQueue;

(* Const before type ignored *)

  function gst_uri_protocol_is_valid(protocol:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_protocol_is_valid';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_protocol_is_supported(_type:TGstURIType; protocol:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_protocol_is_supported';

(* Const before type ignored *)
  function gst_uri_is_valid(uri:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_is_valid';

(* Const before type ignored *)
  function gst_uri_get_protocol(uri:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_protocol';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_has_protocol(uri:Pgchar; protocol:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_has_protocol';

(* Const before type ignored *)
  function gst_uri_get_location(uri:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_location';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_construct(protocol:Pgchar; location:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_uri_construct';

(* Const before type ignored *)
  function gst_filename_to_uri(filename:Pgchar; error:PPGError):Pgchar;cdecl;external gstreamerlib name 'gst_filename_to_uri';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_element_make_from_uri(_type:TGstURIType; uri:Pgchar; elementname:Pgchar; error:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_element_make_from_uri';

  function gst_uri_handler_get_type:TGType;cdecl;external gstreamerlib name 'gst_uri_handler_get_type';

  function gst_uri_handler_get_uri_type(handler:PGstURIHandler):TGstURIType;cdecl;external gstreamerlib name 'gst_uri_handler_get_uri_type';

(* Const before type ignored *)
(* Const before declarator ignored *)
  function gst_uri_handler_get_protocols(handler:PGstURIHandler):PPgchar;cdecl;external gstreamerlib name 'gst_uri_handler_get_protocols';

  function gst_uri_handler_get_uri(handler:PGstURIHandler):Pgchar;cdecl;external gstreamerlib name 'gst_uri_handler_get_uri';

(* Const before type ignored *)
  function gst_uri_handler_set_uri(handler:PGstURIHandler; uri:Pgchar; error:PPGError):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_handler_set_uri';


  function gst_uri_get_type:TGType;cdecl;external gstreamerlib name 'gst_uri_get_type';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_new(scheme:Pgchar; userinfo:Pgchar; host:Pgchar; port:Tguint; path:Pgchar; 
             query:Pgchar; fragment:Pgchar):PGstUri;cdecl;external gstreamerlib name 'gst_uri_new';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_new_with_base(base:PGstUri; scheme:Pgchar; userinfo:Pgchar; host:Pgchar; port:Tguint; 
             path:Pgchar; query:Pgchar; fragment:Pgchar):PGstUri;cdecl;external gstreamerlib name 'gst_uri_new_with_base';

(* Const before type ignored *)
  function gst_uri_from_string(uri:Pgchar):PGstUri;cdecl;external gstreamerlib name 'gst_uri_from_string';

(* Const before type ignored *)
  function gst_uri_from_string_with_base(base:PGstUri; uri:Pgchar):PGstUri;cdecl;external gstreamerlib name 'gst_uri_from_string_with_base';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_equal(first:PGstUri; second:PGstUri):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_equal';

  function gst_uri_join(base_uri:PGstUri; ref_uri:PGstUri):PGstUri;cdecl;external gstreamerlib name 'gst_uri_join';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_join_strings(base_uri:Pgchar; ref_uri:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_uri_join_strings';

(* Const before type ignored *)
  function gst_uri_is_writable(uri:PGstUri):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_is_writable';

  function gst_uri_make_writable(uri:PGstUri):PGstUri;cdecl;external gstreamerlib name 'gst_uri_make_writable';

(* Const before type ignored *)
  function gst_uri_to_string(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_to_string';

(* Const before type ignored *)
  function gst_uri_is_normalized(uri:PGstUri):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_is_normalized';

  function gst_uri_normalize(uri:PGstUri):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_normalize';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_get_scheme(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_scheme';

(* Const before type ignored *)
  function gst_uri_set_scheme(uri:PGstUri; scheme:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_scheme';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_get_userinfo(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_userinfo';

(* Const before type ignored *)
  function gst_uri_set_userinfo(uri:PGstUri; userinfo:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_userinfo';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_get_host(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_host';

(* Const before type ignored *)
  function gst_uri_set_host(uri:PGstUri; host:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_host';

(* Const before type ignored *)
  function gst_uri_get_port(uri:PGstUri):Tguint;cdecl;external gstreamerlib name 'gst_uri_get_port';

  function gst_uri_set_port(uri:PGstUri; port:Tguint):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_port';

(* Const before type ignored *)
  function gst_uri_get_path(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_path';

(* Const before type ignored *)
  function gst_uri_set_path(uri:PGstUri; path:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_path';

(* Const before type ignored *)
  function gst_uri_get_path_string(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_path_string';

(* Const before type ignored *)
  function gst_uri_set_path_string(uri:PGstUri; path:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_path_string';

(* Const before type ignored *)
  function gst_uri_get_path_segments(uri:PGstUri):PGList;cdecl;external gstreamerlib name 'gst_uri_get_path_segments';

  function gst_uri_set_path_segments(uri:PGstUri; path_segments:PGList):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_path_segments';

(* Const before type ignored *)
  function gst_uri_append_path(uri:PGstUri; relative_path:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_append_path';

(* Const before type ignored *)
  function gst_uri_append_path_segment(uri:PGstUri; path_segment:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_append_path_segment';

(* Const before type ignored *)
  function gst_uri_get_query_string(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_query_string';

(* Const before type ignored *)
  function gst_uri_set_query_string(uri:PGstUri; query:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_query_string';

(* Const before type ignored *)
  function gst_uri_get_query_table(uri:PGstUri):PGHashTable;cdecl;external gstreamerlib name 'gst_uri_get_query_table';

  function gst_uri_set_query_table(uri:PGstUri; query_table:PGHashTable):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_query_table';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_set_query_value(uri:PGstUri; query_key:Pgchar; query_value:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_query_value';

(* Const before type ignored *)
  function gst_uri_remove_query_key(uri:PGstUri; query_key:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_remove_query_key';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_query_has_key(uri:PGstUri; query_key:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_query_has_key';

(* Const before type ignored *)
(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_get_query_value(uri:PGstUri; query_key:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_query_value';

(* Const before type ignored *)
  function gst_uri_get_query_keys(uri:PGstUri):PGList;cdecl;external gstreamerlib name 'gst_uri_get_query_keys';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_uri_get_fragment(uri:PGstUri):Pgchar;cdecl;external gstreamerlib name 'gst_uri_get_fragment';

(* Const before type ignored *)
  function gst_uri_set_fragment(uri:PGstUri; fragment:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_uri_set_fragment';

(* Const before type ignored *)
  function gst_uri_get_media_fragment_table(uri:PGstUri):PGHashTable;cdecl;external gstreamerlib name 'gst_uri_get_media_fragment_table';

  {
  static inline GstUri *
  gst_uri_copy (const GstUri * uri)
  
    return ((GstUri *)(gst_mini_object_copy (((const GstMiniObject*)(uri)))));
  
  # 373 "gsturi.h"
  static inline GstUri *
  gst_uri_ref (GstUri * uri)
  
    return ((GstUri *)(gst_mini_object_ref (((GstMiniObject*)(uri)))));
  
  # 389 "gsturi.h"
  static inline void
  gst_uri_unref (GstUri * uri)
  
    gst_mini_object_unref (((GstMiniObject*)(uri)));
  
   }


