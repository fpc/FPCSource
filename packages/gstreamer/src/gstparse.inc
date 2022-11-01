{* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstparse.h: get a pipeline from a text pipeline description
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
    PGstParseContext  = ^TGstParseContext;
    PGstParseContext_autoptr  = ^TGstParseContext_autoptr;
    PGstParseContext_listautoptr  = ^TGstParseContext_listautoptr;
    PGstParseContext_queueautoptr  = ^TGstParseContext_queueautoptr;
    PGstParseContext_slistautoptr  = ^TGstParseContext_slistautoptr;
    PGstParseError  = ^TGstParseError;
    PGstParseFlags  = ^TGstParseFlags;

   TGstParseError = (
      GST_PARSE_ERROR_SYNTAX,
      GST_PARSE_ERROR_NO_SUCH_ELEMENT,
      GST_PARSE_ERROR_NO_SUCH_PROPERTY,
      GST_PARSE_ERROR_LINK,
      GST_PARSE_ERROR_COULD_NOT_SET_PROPERTY,
      GST_PARSE_ERROR_EMPTY_BIN,
      GST_PARSE_ERROR_EMPTY,
      GST_PARSE_ERROR_DELAYED_LINK);


    TGstParseFlags = (
      GST_PARSE_FLAG_NONE := 0,
      GST_PARSE_FLAG_FATAL_ERRORS := 1 shl 0,
      GST_PARSE_FLAG_NO_SINGLE_ELEMENT_BINS := 1 shl 1,
      GST_PARSE_FLAG_PLACE_IN_BIN := 1 shl 2
    );

    TGstParseContext = record end;
    T_GstParseContext = TGstParseContext;

  TGstParseContext_autoptr = PGstParseContext;

  TGstParseContext_listautoptr = PGList;

  TGstParseContext_slistautoptr = PGSList;

  TGstParseContext_queueautoptr = PGQueue;

  function gst_parse_error_quark:TGQuark;cdecl;external gstreamerlib name 'gst_parse_error_quark';

  function gst_parse_context_get_type:TGType;cdecl;external gstreamerlib name 'gst_parse_context_get_type';

  function gst_parse_context_new:PGstParseContext;cdecl;external gstreamerlib name 'gst_parse_context_new';

  function gst_parse_context_get_missing_elements(context:PGstParseContext):PPgchar;cdecl;external gstreamerlib name 'gst_parse_context_get_missing_elements';

  procedure gst_parse_context_free(context:PGstParseContext);cdecl;external gstreamerlib name 'gst_parse_context_free';

(* Const before type ignored *)
  function gst_parse_context_copy(context:PGstParseContext):PGstParseContext;cdecl;external gstreamerlib name 'gst_parse_context_copy';

(* Const before type ignored *)
  function gst_parse_launch(pipeline_description:Pgchar; error:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_parse_launch';

(* Const before type ignored *)
  function gst_parse_launchv(argv:PPgchar; error:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_parse_launchv';

(* Const before type ignored *)
  function gst_parse_launch_full(pipeline_description:Pgchar; context:PGstParseContext; flags:TGstParseFlags; error:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_parse_launch_full';

(* Const before type ignored *)
  function gst_parse_launchv_full(argv:PPgchar; context:PGstParseContext; flags:TGstParseFlags; error:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_parse_launchv_full';



