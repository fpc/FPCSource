{
/* GStreamer
 * Copyright (C) 2009-2010 Edward Hervey <bilboed@bilboed.com>
 *           (C) 2011 Wim Taymans <wim.taymans@gmail.com>
 *
 * gstatomicqueue.h:
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
    PGstAtomicQueue  = ^TGstAtomicQueue;
    PGstAtomicQueue_autoptr  = ^TGstAtomicQueue_autoptr;
    PGstAtomicQueue_listautoptr  = ^TGstAtomicQueue_listautoptr;
    PGstAtomicQueue_queueautoptr  = ^TGstAtomicQueue_queueautoptr;
    PGstAtomicQueue_slistautoptr  = ^TGstAtomicQueue_slistautoptr;



    TGstAtomicQueue = record end;
    T_GstAtomicQueue = TGstAtomicQueue;

  TGstAtomicQueue_autoptr = PGstAtomicQueue;

  TGstAtomicQueue_listautoptr = PGList;

  TGstAtomicQueue_slistautoptr = PGSList;

  TGstAtomicQueue_queueautoptr = PGQueue;

  function gst_atomic_queue_get_type:TGType;cdecl;external gstreamerlib name 'gst_atomic_queue_get_type';

  function gst_atomic_queue_new(initial_size:Tguint):PGstAtomicQueue;cdecl;external gstreamerlib name 'gst_atomic_queue_new';

  procedure gst_atomic_queue_ref(queue:PGstAtomicQueue);cdecl;external gstreamerlib name 'gst_atomic_queue_ref';

  procedure gst_atomic_queue_unref(queue:PGstAtomicQueue);cdecl;external gstreamerlib name 'gst_atomic_queue_unref';

  procedure gst_atomic_queue_push(queue:PGstAtomicQueue; data:Tgpointer);cdecl;external gstreamerlib name 'gst_atomic_queue_push';

  function gst_atomic_queue_pop(queue:PGstAtomicQueue):Tgpointer;cdecl;external gstreamerlib name 'gst_atomic_queue_pop';

  function gst_atomic_queue_peek(queue:PGstAtomicQueue):Tgpointer;cdecl;external gstreamerlib name 'gst_atomic_queue_peek';

  function gst_atomic_queue_length(queue:PGstAtomicQueue):Tguint;cdecl;external gstreamerlib name 'gst_atomic_queue_length';


