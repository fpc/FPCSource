{
/* GStreamer
 * Copyright (C) 2004 Wim Taymans <wim@fluendo.com>
 * Copyright (C) 2011 Sebastian Dröge <sebastian.droege@collabora.co.uk>
 *
 * gstiterator.h: Header for GstIterator
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
    P_GstIterator  = ^T_GstIterator;
    PGObject  = ^TGObject;
    PGstIterator  = ^TGstIterator;
    PGstIterator_autoptr  = ^TGstIterator_autoptr;
    PGstIterator_listautoptr  = ^TGstIterator_listautoptr;
    PGstIterator_queueautoptr  = ^TGstIterator_queueautoptr;
    PGstIterator_slistautoptr  = ^TGstIterator_slistautoptr;
    PGstIteratorItem  = ^TGstIteratorItem;
    PGstIteratorResult  = ^TGstIteratorResult;
    Pguint32  = ^Tguint32;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



    TGstIteratorResult = (
      GST_ITERATOR_DONE := 0,
      GST_ITERATOR_OK := 1,
      GST_ITERATOR_RESYNC_ := 2,
      GST_ITERATOR_ERROR := 3
      );

    TGstIteratorItem = (GST_ITERATOR_ITEM_SKIP := 0,GST_ITERATOR_ITEM_PASS := 1,
      GST_ITERATOR_ITEM_END := 2);
(* Const before type ignored *)

    TGstIteratorCopyFunction = procedure (it:PGstIterator; copy:PGstIterator);cdecl;
(* Const before type ignored *)

    TGstIteratorItemFunction = function (it:PGstIterator; item:PGValue):TGstIteratorItem;cdecl;

    TGstIteratorNextFunction = function (it:PGstIterator; result:PGValue):TGstIteratorResult;cdecl;

    TGstIteratorResyncFunction = procedure (it:PGstIterator);cdecl;

    TGstIteratorFreeFunction = procedure (it:PGstIterator);cdecl;
(* Const before type ignored *)

    TGstIteratorForeachFunction = procedure (item:PGValue; user_data:Tgpointer);cdecl;
(* Const before type ignored *)

    TGstIteratorFoldFunction = function (item:PGValue; ret:PGValue; user_data:Tgpointer):Tgboolean;cdecl;
    T_GstIterator = record
        copy : TGstIteratorCopyFunction;
        next : TGstIteratorNextFunction;
        item : TGstIteratorItemFunction;
        resync : TGstIteratorResyncFunction;
        free : TGstIteratorFreeFunction;
        pushed : PGstIterator;
        _type : TGType;
        lock : PGMutex;
        cookie : Tguint32;
        master_cookie : Pguint32;
        size : Tguint;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstIterator = T_GstIterator;

  TGstIterator_autoptr = PGstIterator;

  TGstIterator_listautoptr = PGList;

  TGstIterator_slistautoptr = PGSList;

  TGstIterator_queueautoptr = PGQueue;

  function gst_iterator_get_type:TGType;cdecl;external gstreamerlib name 'gst_iterator_get_type';

  function gst_iterator_new(size:Tguint; _type:TGType; lock:PGMutex; master_cookie:Pguint32; copy:TGstIteratorCopyFunction; 
             next:TGstIteratorNextFunction; item:TGstIteratorItemFunction; resync:TGstIteratorResyncFunction; free:TGstIteratorFreeFunction):PGstIterator;cdecl;external gstreamerlib name 'gst_iterator_new';

  function gst_iterator_new_list(_type:TGType; lock:PGMutex; master_cookie:Pguint32; list:PPGList; owner:PGObject; 
             item:TGstIteratorItemFunction):PGstIterator;cdecl;external gstreamerlib name 'gst_iterator_new_list';

(* Const before type ignored *)
  function gst_iterator_new_single(_type:TGType; object_:PGValue):PGstIterator;cdecl;external gstreamerlib name 'gst_iterator_new_single';

(* Const before type ignored *)
  function gst_iterator_copy(it:PGstIterator):PGstIterator;cdecl;external gstreamerlib name 'gst_iterator_copy';

  function gst_iterator_next(it:PGstIterator; elem:PGValue):TGstIteratorResult;cdecl;external gstreamerlib name 'gst_iterator_next';

  procedure gst_iterator_resync(it:PGstIterator);cdecl;external gstreamerlib name 'gst_iterator_resync';

  procedure gst_iterator_free(it:PGstIterator);cdecl;external gstreamerlib name 'gst_iterator_free';

  procedure gst_iterator_push(it:PGstIterator; other:PGstIterator);cdecl;external gstreamerlib name 'gst_iterator_push';

(* Const before type ignored *)
  function gst_iterator_filter(it:PGstIterator; func:TGCompareFunc; user_data:PGValue):PGstIterator;cdecl;external gstreamerlib name 'gst_iterator_filter';

  function gst_iterator_fold(it:PGstIterator; func:TGstIteratorFoldFunction; ret:PGValue; user_data:Tgpointer):TGstIteratorResult;cdecl;external gstreamerlib name 'gst_iterator_fold';

  function gst_iterator_foreach(it:PGstIterator; func:TGstIteratorForeachFunction; user_data:Tgpointer):TGstIteratorResult;cdecl;external gstreamerlib name 'gst_iterator_foreach';

  function gst_iterator_find_custom(it:PGstIterator; func:TGCompareFunc; elem:PGValue; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_iterator_find_custom';



