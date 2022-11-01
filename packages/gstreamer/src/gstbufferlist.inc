{* GStreamer
 * Copyright (C) 2009 Axis Communications <dev-gstreamer at axis dot com>
 * @author Jonas Holmberg <jonas dot holmberg at axis dot com>
 *
 * gstbufferlist.h: Header for GstBufferList object
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
    PGstBufferList  = ^TGstBufferList;
    PGstBufferList_autoptr  = ^TGstBufferList_autoptr;
    PGstBufferList_listautoptr  = ^TGstBufferList_listautoptr;
    PGstBufferList_queueautoptr  = ^TGstBufferList_queueautoptr;
    PGstBufferList_slistautoptr  = ^TGstBufferList_slistautoptr;


    TGstBufferList = record end;
    T_GstBufferList = TGstBufferList;

    TGstBufferListFunc = function (buffer:PPGstBuffer; idx:Tguint; user_data:Tgpointer):Tgboolean;cdecl;

  TGstBufferList_autoptr = PGstBufferList;

  TGstBufferList_listautoptr = PGList;

  TGstBufferList_slistautoptr = PGSList;

  TGstBufferList_queueautoptr = PGQueue;

  {
  
  static inline GstBufferList *
  gst_buffer_list_ref (GstBufferList * list)
  
    return ((GstBufferList *)gst_mini_object_ref (((GstMiniObject*)(list))))
               ;
  
  # 90 "gstbufferlist.h"
  static inline void
  gst_buffer_list_unref (GstBufferList * list)
  
    gst_mini_object_unref (((GstMiniObject*)(list)));
  
  # 109 "gstbufferlist.h"
  static inline void
  gst_clear_buffer_list (GstBufferList ** list_ptr)
  
    do  _Static_assert (sizeof *(((GstMiniObject **) list_ptr)) == sizeof (gpointer), "Expression evaluates to false"); __typeof__((((GstMiniObject **) list_ptr))) _pp = (((GstMiniObject **) list_ptr)); __typeof__(*(((GstMiniObject **) list_ptr))) _ptr = *_pp; *_pp = 
  # 112 "gstbufferlist.h" 3 4
   ((void *)0)
  # 112 "gstbufferlist.h"
   ; if (_ptr) (gst_mini_object_unref) (_ptr);  while (0) ;
  
  # 126 "gstbufferlist.h"
  static inline GstBufferList *
  gst_buffer_list_copy (const GstBufferList * list)
  
    return ((GstBufferList *)gst_mini_object_copy (((const GstMiniObject*)(list))));
  
  # 150 "gstbufferlist.h"
  static inline gboolean
  gst_buffer_list_replace (GstBufferList **old_list, GstBufferList *new_list)
  
    return gst_mini_object_replace ((GstMiniObject **) old_list,
        (GstMiniObject *) new_list);
  
  # 172 "gstbufferlist.h"
  static inline gboolean
  gst_buffer_list_take (GstBufferList **old_list, GstBufferList *new_list)
  
    return gst_mini_object_take ((GstMiniObject **) old_list,
        (GstMiniObject *) new_list);
  
   }

  function gst_buffer_list_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffer_list_get_type';

  function gst_buffer_list_new:PGstBufferList;cdecl;external gstreamerlib name 'gst_buffer_list_new';

  function gst_buffer_list_new_sized(size:Tguint):PGstBufferList;cdecl;external gstreamerlib name 'gst_buffer_list_new_sized';

  function gst_buffer_list_length(list:PGstBufferList):Tguint;cdecl;external gstreamerlib name 'gst_buffer_list_length';

  function gst_buffer_list_get(list:PGstBufferList; idx:Tguint):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_list_get';

  function gst_buffer_list_get_writable(list:PGstBufferList; idx:Tguint):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_list_get_writable';

  procedure gst_buffer_list_insert(list:PGstBufferList; idx:Tgint; buffer:PGstBuffer);cdecl;external gstreamerlib name 'gst_buffer_list_insert';

  procedure gst_buffer_list_remove(list:PGstBufferList; idx:Tguint; length:Tguint);cdecl;external gstreamerlib name 'gst_buffer_list_remove';

  function gst_buffer_list_foreach(list:PGstBufferList; func:TGstBufferListFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_list_foreach';

(* Const before type ignored *)
  function gst_buffer_list_copy_deep(list:PGstBufferList):PGstBufferList;cdecl;external gstreamerlib name 'gst_buffer_list_copy_deep';

  function gst_buffer_list_calculate_size(list:PGstBufferList):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_list_calculate_size';


  var
    _gst_buffer_list_type : TGType;cvar;external;

