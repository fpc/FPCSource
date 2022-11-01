{
/* GStreamer
 * Copyright (C) 2015 Centricular Ltd
 *  @author: Edward Hervey <edward@centricular.com>
 *  @author: Jan Schmidt <jan@centricular.com>
 *
 * gststreams.h : Header for GstStreamCollection subsystem
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
    P_GstStreamCollection  = ^T_GstStreamCollection;
    P_GstStreamCollectionClass  = ^T_GstStreamCollectionClass;
    PGstStreamCollection  = ^TGstStreamCollection;
    PPGstStreamCollection  = ^PGstStreamCollection;
    PGstStreamCollection_autoptr  = ^TGstStreamCollection_autoptr;
    PGstStreamCollection_listautoptr  = ^TGstStreamCollection_listautoptr;
    PGstStreamCollection_queueautoptr  = ^TGstStreamCollection_queueautoptr;
    PGstStreamCollection_slistautoptr  = ^TGstStreamCollection_slistautoptr;
    PGstStreamCollectionPrivate  = ^TGstStreamCollectionPrivate;

    TGstStreamCollectionPrivate = record
    end;
    T_GstStreamCollectionPrivate = TGstStreamCollectionPrivate;

    T_GstStreamCollection = record
        object_ : TGstObject;
        upstream_id : Pgchar;
        priv : PGstStreamCollectionPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstStreamCollection = T_GstStreamCollection;

    T_GstStreamCollectionClass = record
        parent_class : TGstObjectClass;
        stream_notify : procedure (collection:PGstStreamCollection; stream:PGstStream; pspec:PGParamSpec);cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;

  TGstStreamCollection_autoptr = PGstStreamCollection;

  TGstStreamCollection_listautoptr = PGList;

  TGstStreamCollection_slistautoptr = PGSList;

  TGstStreamCollection_queueautoptr = PGQueue;

  function gst_stream_collection_get_type:TGType;cdecl;external gstreamerlib name 'gst_stream_collection_get_type';

(* Const before type ignored *)
  function gst_stream_collection_new(upstream_id:Pgchar):PGstStreamCollection;cdecl;external gstreamerlib name 'gst_stream_collection_new';

(* Const before type ignored *)
  function gst_stream_collection_get_upstream_id(collection:PGstStreamCollection):Pgchar;cdecl;external gstreamerlib name 'gst_stream_collection_get_upstream_id';

  function gst_stream_collection_get_size(collection:PGstStreamCollection):Tguint;cdecl;external gstreamerlib name 'gst_stream_collection_get_size';

  function gst_stream_collection_get_stream(collection:PGstStreamCollection; index:Tguint):PGstStream;cdecl;external gstreamerlib name 'gst_stream_collection_get_stream';

  function gst_stream_collection_add_stream(collection:PGstStreamCollection; stream:PGstStream):Tgboolean;cdecl;external gstreamerlib name 'gst_stream_collection_add_stream';



