{
/* GStreamer
 * Copyright (C) 2009 Wim Taymans <wim.taymans@gmail.be>
 *
 * gstmeta.h: Header for Metadata structures
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
    P_GstMetaInfo  = ^T_GstMetaInfo;
    PGstMeta  = ^TGstMeta;
    PPGstMeta = ^PGstMeta;
    PGstMetaInfo  = ^TGstMetaInfo;
    PGstMetaTransformCopy  = ^TGstMetaTransformCopy;


{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    TGstMetaInitFunction = function (meta:PGstMeta; params:Tgpointer; buffer:PGstBuffer):Tgboolean;cdecl;
    TGstMetaFreeFunction = procedure (meta:PGstMeta; buffer:PGstBuffer);cdecl;
    TGstMetaTransformCopy = record
        region : Tgboolean;
        offset : Tgsize;
        size : Tgsize;
      end;

    TGstMetaTransformFunction = function (transbuf:PGstBuffer; meta:PGstMeta; buffer:PGstBuffer; _type:TGQuark; data:Tgpointer):Tgboolean;cdecl;
    T_GstMetaInfo = record
        api : TGType;
        _type : TGType;
        size : Tgsize;
        init_func : TGstMetaInitFunction;
        free_func : TGstMetaFreeFunction;
        transform_func : TGstMetaTransformFunction;
      end;
    TGstMetaInfo = T_GstMetaInfo;

    TGstMetaFlags = (
      GST_META_FLAG_NONE := 0,
      GST_META_FLAG_READONLY := 1 shl 0,
      GST_META_FLAG_POOLED := 1 shl 1,
      GST_META_FLAG_LOCKED := 1 shl 2,
      GST_META_FLAG_LAST := 1 shl 16);

     P_GstMeta  = ^T_GstMeta;
     PGstMetaFlags  = ^TGstMetaFlags;
     T_GstMeta = record
         flags : TGstMetaFlags;
         info : PGstMetaInfo;
     end;
     TGstMeta = T_GstMeta;


var
  _gst_meta_transform_copy : TGQuark;cvar;external;


(* Const before type ignored *)
(* Const before type ignored *)

  function gst_meta_api_type_register(api:Pgchar; tags:PPgchar):TGType;cdecl;external gstreamerlib name 'gst_meta_api_type_register';

  function gst_meta_api_type_has_tag(api:TGType; tag:TGQuark):Tgboolean;cdecl;external gstreamerlib name 'gst_meta_api_type_has_tag';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_meta_register(api:TGType; impl:Pgchar; size:Tgsize; init_func:TGstMetaInitFunction; free_func:TGstMetaFreeFunction; 
             transform_func:TGstMetaTransformFunction):PGstMetaInfo;cdecl;external gstreamerlib name 'gst_meta_register';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_meta_get_info(impl:Pgchar):PGstMetaInfo;cdecl;external gstreamerlib name 'gst_meta_get_info';

(* Const before type ignored *)
(* Const before declarator ignored *)
  function gst_meta_api_type_get_tags(api:TGType):PPgchar;cdecl;external gstreamerlib name 'gst_meta_api_type_get_tags';

(* Const before type ignored *)
  function gst_meta_get_seqnum(meta:PGstMeta):Tguint64;cdecl;external gstreamerlib name 'gst_meta_get_seqnum';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_meta_compare_seqnum(meta1:PGstMeta; meta2:PGstMeta):Tgint;cdecl;external gstreamerlib name 'gst_meta_compare_seqnum';


    var
      _gst_meta_tag_memory : TGQuark;cvar;external;

