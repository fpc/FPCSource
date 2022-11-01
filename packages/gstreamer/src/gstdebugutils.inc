{* GStreamer
 * Copyright (C) 2007 Stefan Kost <ensonic@users.sf.net>
 *
 * gstdebugutils.h: debugging and analysis utilities
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

    PGstDebugGraphDetails  = ^TGstDebugGraphDetails;

    TGstDebugGraphDetails = (GST_DEBUG_GRAPH_SHOW_MEDIA_TYPE := 1 shl 0,
      GST_DEBUG_GRAPH_SHOW_CAPS_DETAILS := 1 shl 1,
      GST_DEBUG_GRAPH_SHOW_NON_DEFAULT_PARAMS := 1 shl 2,
      GST_DEBUG_GRAPH_SHOW_STATES := 1 shl 3,GST_DEBUG_GRAPH_SHOW_FULL_PARAMS := 1 shl 4,
      GST_DEBUG_GRAPH_SHOW_ALL := (1 shl 4)-1,GST_DEBUG_GRAPH_SHOW_VERBOSE := Tgint($ffffffff)
      );

  function gst_debug_bin_to_dot_data(bin:PGstBin; details:TGstDebugGraphDetails):Pgchar;cdecl;external gstreamerlib name 'gst_debug_bin_to_dot_data';

(* Const before type ignored *)
  procedure gst_debug_bin_to_dot_file(bin:PGstBin; details:TGstDebugGraphDetails; file_name:Pgchar);cdecl;external gstreamerlib name 'gst_debug_bin_to_dot_file';

(* Const before type ignored *)
  procedure gst_debug_bin_to_dot_file_with_ts(bin:PGstBin; details:TGstDebugGraphDetails; file_name:Pgchar);cdecl;external gstreamerlib name 'gst_debug_bin_to_dot_file_with_ts';


