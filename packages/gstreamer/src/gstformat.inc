{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *
 * gstformat.h: Header for GstFormat types used in queries and
 *              seeking.
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
    P_GstFormatDefinition  = ^T_GstFormatDefinition;
    PGstFormatDefinition  = ^TGstFormatDefinition;
  //  PGstIterator  = ^TGstIterator;

    T_GstFormatDefinition = record
        value : TGstFormat;
        nick : Pgchar;
        description : Pgchar;
        quark : TGQuark;
      end;
    TGstFormatDefinition = T_GstFormatDefinition;

(* Const before type ignored *)

  function gst_format_get_name(format:TGstFormat):Pgchar;cdecl;external gstreamerlib name 'gst_format_get_name';

  function gst_format_to_quark(format:TGstFormat):TGQuark;cdecl;external gstreamerlib name 'gst_format_to_quark';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_format_register(nick:Pgchar; description:Pgchar):TGstFormat;cdecl;external gstreamerlib name 'gst_format_register';

(* Const before type ignored *)
  function gst_format_get_by_nick(nick:Pgchar):TGstFormat;cdecl;external gstreamerlib name 'gst_format_get_by_nick';

(* Const before type ignored *)
  function gst_formats_contains(formats:PGstFormat; format:TGstFormat):Tgboolean;cdecl;external gstreamerlib name 'gst_formats_contains';

(* Const before type ignored *)
  function gst_format_get_details(format:TGstFormat):PGstFormatDefinition;cdecl;external gstreamerlib name 'gst_format_get_details';

  function gst_format_iterate_definitions:PGstIterator;cdecl;external gstreamerlib name 'gst_format_iterate_definitions';


