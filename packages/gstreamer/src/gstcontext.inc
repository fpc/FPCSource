{* GStreamer
 * Copyright (C) 2013 Collabora Ltd.
 *   Author: Sebastian Dröge <sebastian.droege@collabora.co.uk>
 * Copyright (C) 2013 Sebastian Dröge <slomo@circular-chaos.org>
 *
 * gstcontext.h: Header for GstContext subsystem
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
    PGstContext  = ^TGstContext;
    PPGstContext = ^PGstContext;
    PGstContext_autoptr  = ^TGstContext_autoptr;
    PGstContext_listautoptr  = ^TGstContext_listautoptr;
    PGstContext_queueautoptr  = ^TGstContext_queueautoptr;
    PGstContext_slistautoptr  = ^TGstContext_slistautoptr;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

    T_GstContext = record end;
    TGstContext = T_GstContext;

  TGstContext_autoptr = PGstContext;

  TGstContext_listautoptr = PGList;

  TGstContext_slistautoptr = PGSList;

  TGstContext_queueautoptr = PGQueue;

  function gst_context_get_type:TGType;cdecl;external gstreamerlib name 'gst_context_get_type';

  {
  static inline GstContext *
  gst_context_ref (GstContext * context)
  
    return (GstContext *) gst_mini_object_ref (((GstMiniObject*)(context)));
  
  # 71 "gstcontext.h"
  static inline void
  gst_context_unref (GstContext * context)
  
    gst_mini_object_unref (((GstMiniObject*)(context)));
  
  # 88 "gstcontext.h"
  static inline GstContext *
  gst_context_copy (const GstContext * context)
  
    return ((GstContext*)(gst_mini_object_copy (((const GstMiniObject*)(context)))));
  
  # 130 "gstcontext.h"
  static inline gboolean
  gst_context_replace (GstContext **old_context, GstContext *new_context)
  
    return gst_mini_object_replace ((GstMiniObject **) old_context, (GstMiniObject *) new_context);
  
   }
(* Const before type ignored *)
  function gst_context_new(context_type:Pgchar; persistent:Tgboolean):PGstContext;cdecl;external gstreamerlib name 'gst_context_new';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_context_get_context_type(context:PGstContext):Pgchar;cdecl;external gstreamerlib name 'gst_context_get_context_type';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_context_has_context_type(context:PGstContext; context_type:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_context_has_context_type';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_context_get_structure(context:PGstContext):PGstStructure;cdecl;external gstreamerlib name 'gst_context_get_structure';

  function gst_context_writable_structure(context:PGstContext):PGstStructure;cdecl;external gstreamerlib name 'gst_context_writable_structure';

(* Const before type ignored *)
  function gst_context_is_persistent(context:PGstContext):Tgboolean;cdecl;external gstreamerlib name 'gst_context_is_persistent';


  var
    _gst_context_type : TGType;cvar;external;


