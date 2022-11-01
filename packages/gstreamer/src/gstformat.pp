
unit gstformat;
interface

{
  Automatically converted by H2Pas 1.0.0 from gstformat.h
  The following command line parameters were used:
    gstformat.h
}

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  { GStreamer
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
    }
{$ifndef __GST_FORMAT_H__}
{$define __GST_FORMAT_H__}  
{$include <glib.h>}
{$include <gst/gstiterator.h>}
  {*
   * GstFormat:
   * @GST_FORMAT_UNDEFINED: undefined format
   * @GST_FORMAT_DEFAULT: the default format of the pad/element. This can be
   *    samples for raw audio, frames/fields for raw video (some, but not all,
   *    elements support this; use @GST_FORMAT_TIME if you don't have a good
   *    reason to query for samples/frames)
   * @GST_FORMAT_BYTES: bytes
   * @GST_FORMAT_TIME: time in nanoseconds
   * @GST_FORMAT_BUFFERS: buffers (few, if any, elements implement this as of
   *     May 2009)
   * @GST_FORMAT_PERCENT: percentage of stream (few, if any, elements implement
   *     this as of May 2009)
   *
   * Standard predefined formats
    }
  { NOTE: don't forget to update the table in gstformat.c when changing
   * this enum  }
(* error 
typedef enum {
  { must be first in list  }
in declaration at line 59 *)
    { a percentage is always relative to 1000000  }
    {*
     * GST_FORMAT_PERCENT_MAX:
     *
     * The PERCENT format is between 0 and this value
      }

    { was #define dname def_expr }
    function GST_FORMAT_PERCENT_MAX : longint; { return type might be wrong }

    {*
     * GST_FORMAT_PERCENT_SCALE:
     *
     * The value used to scale down the reported PERCENT format value to
     * its real value.
      }
    { was #define dname def_expr }
    function GST_FORMAT_PERCENT_SCALE : longint; { return type might be wrong }


    type
      _GstFormatDefinition = GstFormatDefinition;
    {*
     * GstFormatDefinition:
     * @value: The unique id of this format
     * @nick: A short nick of the format
     * @description: A longer description of the format
     * @quark: A quark for the nick
     *
     * A format definition
      }
(* Const before type ignored *)
(* Const before type ignored *)
      _GstFormatDefinition = record
          value : GstFormat;
          nick : ^gchar;
          description : ^gchar;
          quark : GQuark;
        end;

(* error 
const gchar*    gst_format_get_name             (GstFormat format);
 in declarator_list *)
(* error 
GQuark          gst_format_to_quark             (GstFormat format);
 in declarator_list *)
    { register a new format  }
(* error 
GstFormat       gst_format_register             (const gchar *nick,
(* error 
                                                 const gchar *description);
 in declarator_list *)
 in declarator_list *)
(* error 
GstFormat       gst_format_get_by_nick          (const gchar *nick);
 in declarator_list *)
    { check if a format is in an array of formats  }
(* error 
gboolean        gst_formats_contains            (const GstFormat *formats, GstFormat format);
(* error 
gboolean        gst_formats_contains            (const GstFormat *formats, GstFormat format);
 in declarator_list *)
 in declarator_list *)
    { query for format details  }
(* error 
const GstFormatDefinition*
 in declarator_list *)
(* error 
GstIterator*    gst_format_iterate_definitions  (void);
 in declarator_list *)
{$endif}
    { __GST_FORMAT_H__  }
(* error 
#endif /* __GST_FORMAT_H__ */

implementation

    { was #define dname def_expr }
    function GST_FORMAT_PERCENT_MAX : longint; { return type might be wrong }
      begin
        GST_FORMAT_PERCENT_MAX:=G_GINT64_CONSTANT(1000000);
      end;

    { was #define dname def_expr }
    function GST_FORMAT_PERCENT_SCALE : longint; { return type might be wrong }
      begin
        GST_FORMAT_PERCENT_SCALE:=G_GINT64_CONSTANT(10000);
      end;


end.
