{
/* GStreamer
 * Copyright (C) 2005 David Schleef <ds@schleef.org>
 *
 * gstminiobject.h: Header for GstMiniObject
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
    P_GstMiniObject  = ^T_GstMiniObject;
    PGstLockFlags  = ^TGstLockFlags;
//    PGstMiniObject  = ^TGstMiniObject;
    PGstMiniObjectCopyFunction  = ^TGstMiniObjectCopyFunction;
    PGstMiniObjectFlags  = ^TGstMiniObjectFlags;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

(* Const before type ignored *)



    TGstLockFlags = (
      GST_LOCK_FLAG_READ := 1 shl 0,
      GST_LOCK_FLAG_WRITE := 1 shl 1,
      GST_LOCK_FLAG_EXCLUSIVE := 1 shl 2,
      GST_LOCK_FLAG_LAST := 1 shl 8
      );


  procedure gst_mini_object_init(mini_object:PGstMiniObject; flags:Tguint; _type:TGType; copy_func:TGstMiniObjectCopyFunction; dispose_func:TGstMiniObjectDisposeFunction; 
              free_func:TGstMiniObjectFreeFunction);cdecl;external gstreamerlib name 'gst_mini_object_init';

  function gst_mini_object_ref(mini_object:PGstMiniObject):PGstMiniObject;cdecl;external gstreamerlib name 'gst_mini_object_ref';

  procedure gst_mini_object_unref(mini_object:PGstMiniObject);cdecl;external gstreamerlib name 'gst_mini_object_unref';

  procedure gst_clear_mini_object(object_ptr:PPGstMiniObject);cdecl;external gstreamerlib name 'gst_clear_mini_object';

  procedure gst_mini_object_weak_ref(object_:PGstMiniObject; notify:TGstMiniObjectNotify; data:Tgpointer);cdecl;external gstreamerlib name 'gst_mini_object_weak_ref';

  procedure gst_mini_object_weak_unref(object_:PGstMiniObject; notify:TGstMiniObjectNotify; data:Tgpointer);cdecl;external gstreamerlib name 'gst_mini_object_weak_unref';

  function gst_mini_object_lock(object_:PGstMiniObject; flags:TGstLockFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_mini_object_lock';

  procedure gst_mini_object_unlock(object_:PGstMiniObject; flags:TGstLockFlags);cdecl;external gstreamerlib name 'gst_mini_object_unlock';

(* Const before type ignored *)
  function gst_mini_object_is_writable(mini_object:PGstMiniObject):Tgboolean;cdecl;external gstreamerlib name 'gst_mini_object_is_writable';

  function gst_mini_object_make_writable(mini_object:PGstMiniObject):PGstMiniObject;cdecl;external gstreamerlib name 'gst_mini_object_make_writable';

(* Const before type ignored *)
  function gst_mini_object_copy(mini_object:PGstMiniObject):PGstMiniObject;cdecl;external gstreamerlib name 'gst_mini_object_copy';

  procedure gst_mini_object_set_qdata(object_:PGstMiniObject; quark:TGQuark; data:Tgpointer; destroy:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_mini_object_set_qdata';

  function gst_mini_object_get_qdata(object_:PGstMiniObject; quark:TGQuark):Tgpointer;cdecl;external gstreamerlib name 'gst_mini_object_get_qdata';

  function gst_mini_object_steal_qdata(object_:PGstMiniObject; quark:TGQuark):Tgpointer;cdecl;external gstreamerlib name 'gst_mini_object_steal_qdata';

  procedure gst_mini_object_add_parent(object_:PGstMiniObject; parent:PGstMiniObject);cdecl;external gstreamerlib name 'gst_mini_object_add_parent';

  procedure gst_mini_object_remove_parent(object_:PGstMiniObject; parent:PGstMiniObject);cdecl;external gstreamerlib name 'gst_mini_object_remove_parent';

  function gst_mini_object_replace(olddata:PPGstMiniObject; newdata:PGstMiniObject):Tgboolean;cdecl;external gstreamerlib name 'gst_mini_object_replace';

  function gst_mini_object_take(olddata:PPGstMiniObject; newdata:PGstMiniObject):Tgboolean;cdecl;external gstreamerlib name 'gst_mini_object_take';

  function gst_mini_object_steal(olddata:PPGstMiniObject):PGstMiniObject;cdecl;external gstreamerlib name 'gst_mini_object_steal';


