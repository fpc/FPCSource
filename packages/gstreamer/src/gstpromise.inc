{* GStreamer
 * Copyright (C) 2017 Matthew Waters <matthew@centricular.com>
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
    P_GstPromise  = ^T_GstPromise;

    PGstPromise  = ^TGstPromise;
    PGstPromise_autoptr  = ^TGstPromise_autoptr;
    PGstPromise_listautoptr  = ^TGstPromise_listautoptr;
    PGstPromise_queueautoptr  = ^TGstPromise_queueautoptr;
    PGstPromise_slistautoptr  = ^TGstPromise_slistautoptr;
    PGstPromiseResult  = ^TGstPromiseResult;





    TGstPromiseResult = (GST_PROMISE_RESULT_PENDING,GST_PROMISE_RESULT_INTERRUPTED,
      GST_PROMISE_RESULT_REPLIED,GST_PROMISE_RESULT_EXPIRED
      );

    TGstPromiseChangeFunc = procedure (promise:PGstPromise; user_data:Tgpointer);cdecl;

    T_GstPromise = record
        parent : TGstMiniObject;
      end;
    TGstPromise = T_GstPromise;

  TGstPromise_autoptr = PGstPromise;

  TGstPromise_listautoptr = PGList;

  TGstPromise_slistautoptr = PGSList;

  TGstPromise_queueautoptr = PGQueue;


  function gst_promise_get_type:TGType;cdecl;external gstreamerlib name 'gst_promise_get_type';

  function gst_promise_new:PGstPromise;cdecl;external gstreamerlib name 'gst_promise_new';

  function gst_promise_new_with_change_func(func:TGstPromiseChangeFunc; user_data:Tgpointer; notify:TGDestroyNotify):PGstPromise;cdecl;external gstreamerlib name 'gst_promise_new_with_change_func';

  function gst_promise_wait(promise:PGstPromise):TGstPromiseResult;cdecl;external gstreamerlib name 'gst_promise_wait';

  procedure gst_promise_reply(promise:PGstPromise; s:PGstStructure);cdecl;external gstreamerlib name 'gst_promise_reply';

  procedure gst_promise_interrupt(promise:PGstPromise);cdecl;external gstreamerlib name 'gst_promise_interrupt';

  procedure gst_promise_expire(promise:PGstPromise);cdecl;external gstreamerlib name 'gst_promise_expire';

(* Const before type ignored *)
  function gst_promise_get_reply(promise:PGstPromise):PGstStructure;cdecl;external gstreamerlib name 'gst_promise_get_reply';

  {
  static inline GstPromise *
  gst_promise_ref (GstPromise * promise)
  
    return (GstPromise *) gst_mini_object_ref (((GstMiniObject*)(promise)));
  
  # 121 "gstpromise.h"
  
  static inline void
  gst_promise_unref (GstPromise * promise)
  
    gst_mini_object_unref (((GstMiniObject*)(promise)));
  
  
   }


