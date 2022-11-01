{
/* GStreamer
 * Copyright (C) 2015 Centricular Ltd
 *  @author: Edward Hervey <edward@centricular.com>
 *  @author: Jan Schmidt <jan@centricular.com>
 *
 * gststreams.h : Header for GstStream subsystem
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
    PGstStream  = ^TGstStream;
    PPGstStream  = ^PGstStream;
    PGstStream_autoptr  = ^TGstStream_autoptr;
    PGstStream_listautoptr  = ^TGstStream_listautoptr;
    PGstStream_queueautoptr  = ^TGstStream_queueautoptr;
    PGstStream_slistautoptr  = ^TGstStream_slistautoptr;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    PGstStreamType = ^TGstStreamType;
    TGstStreamType = (
      GST_STREAM_TYPE_UNKNOWN := 1 shl 0,
      GST_STREAM_TYPE_AUDIO := 1 shl 1,
      GST_STREAM_TYPE_VIDEO := 1 shl 2,
      GST_STREAM_TYPE_CONTAINER := 1 shl 3,
      GST_STREAM_TYPE_TEXT := 1 shl 4
    );
    TGstStreamPrivate = record end;
    T_GstStreamPrivate = TGstStreamPrivate;
    PGstStreamPrivate = ^TGstStreamPrivate;
(* Const before type ignored *)
    P_GstStream = ^T_GstStream;
    T_GstStream = record
        object_ : TGstObject;
        stream_id : Pgchar;
        priv : PGstStreamPrivate;
        _gst_reserved : array[0..(GST_PADDING)-1] of Tgpointer;
      end;
    TGstStream  = T_GstStream ;

    P_GstStreamClass = ^T_GstStreamClass;
    T_GstStreamClass = record
        parent_class : TGstObjectClass;
        _gst_reserved : array[0..(GST_PADDING)-1] of Tgpointer;
     end;
   TGstStreamClass = T_GstStreamClass;




  TGstStream_autoptr = PGstStream;

  TGstStream_listautoptr = PGList;

  TGstStream_slistautoptr = PGSList;

  TGstStream_queueautoptr = PGQueue;



(* Const before type ignored *)

  function gst_stream_new(stream_id:Pgchar; caps:PGstCaps; _type:TGstStreamType; flags:TGstStreamFlags):PGstStream;cdecl;external gstreamerlib name 'gst_stream_new';

(* Const before type ignored *)
  function gst_stream_get_stream_id(stream:PGstStream):Pgchar;cdecl;external gstreamerlib name 'gst_stream_get_stream_id';

  procedure gst_stream_set_stream_flags(stream:PGstStream; flags:TGstStreamFlags);cdecl;external gstreamerlib name 'gst_stream_set_stream_flags';

  function gst_stream_get_stream_flags(stream:PGstStream):TGstStreamFlags;cdecl;external gstreamerlib name 'gst_stream_get_stream_flags';

  procedure gst_stream_set_stream_type(stream:PGstStream; stream_type:TGstStreamType);cdecl;external gstreamerlib name 'gst_stream_set_stream_type';

  function gst_stream_get_stream_type(stream:PGstStream):TGstStreamType;cdecl;external gstreamerlib name 'gst_stream_get_stream_type';

  procedure gst_stream_set_tags(stream:PGstStream; tags:PGstTagList);cdecl;external gstreamerlib name 'gst_stream_set_tags';

  function gst_stream_get_tags(stream:PGstStream):PGstTagList;cdecl;external gstreamerlib name 'gst_stream_get_tags';

  procedure gst_stream_set_caps(stream:PGstStream; caps:PGstCaps);cdecl;external gstreamerlib name 'gst_stream_set_caps';

  function gst_stream_get_caps(stream:PGstStream):PGstCaps;cdecl;external gstreamerlib name 'gst_stream_get_caps';

(* Const before type ignored *)
  function gst_stream_type_get_name(stype:TGstStreamType):Pgchar;cdecl;external gstreamerlib name 'gst_stream_type_get_name';



