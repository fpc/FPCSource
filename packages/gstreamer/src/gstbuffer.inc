{* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gstbuffer.h: Header for GstBuffer object
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
    P_GstBuffer  = ^T_GstBuffer;
    P_GstParentBufferMeta  = ^T_GstParentBufferMeta;
    P_GstReferenceTimestampMeta  = ^T_GstReferenceTimestampMeta;
    PGBytes  = ^TGBytes;
    Pgpointer  = ^Tgpointer;
    Pgsize  = ^Tgsize;
    PGstBuffer_autoptr  = ^TGstBuffer_autoptr;
    PGstBuffer_listautoptr  = ^TGstBuffer_listautoptr;
    PGstBuffer_queueautoptr  = ^TGstBuffer_queueautoptr;
    PGstBuffer_slistautoptr  = ^TGstBuffer_slistautoptr;
    PGstBufferCopyFlags  = ^TGstBufferCopyFlags;
    PGstBufferFlags  = ^TGstBufferFlags;
    PGstBufferPool_autoptr  = ^TGstBufferPool_autoptr;
    PGstBufferPool_listautoptr  = ^TGstBufferPool_listautoptr;
    PGstBufferPool_queueautoptr  = ^TGstBufferPool_queueautoptr;
    PGstBufferPool_slistautoptr  = ^TGstBufferPool_slistautoptr;
    PGstParentBufferMeta  = ^TGstParentBufferMeta;
    PGstReferenceTimestampMeta  = ^TGstReferenceTimestampMeta;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



  TGstBufferCopyFlags = (
    GST_BUFFER_COPY_NONE := 0,
    GST_BUFFER_COPY_FLAGS := 1 shl 0,
    GST_BUFFER_COPY_TIMESTAMPS := 1 shl 1,
    GST_BUFFER_COPY_META := 1 shl 2,
    GST_BUFFER_COPY_MEMORY := 1 shl 3,
    GST_BUFFER_COPY_MERGE := 1 shl 4,
    GST_BUFFER_COPY_DEEP_ := 1 shl 5
  );

    TGstBufferFlags = (
      GST_BUFFER_FLAG_LIVE := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 0,
      GST_BUFFER_FLAG_DECODE_ONLY := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 1,
      GST_BUFFER_FLAG_DISCONT := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 2,
      GST_BUFFER_FLAG_RESYNC := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 3,
      GST_BUFFER_FLAG_CORRUPTED := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 4,
      GST_BUFFER_FLAG_MARKER := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 5,
      GST_BUFFER_FLAG_HEADER := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 6,
      GST_BUFFER_FLAG_GAP := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 7,
      GST_BUFFER_FLAG_DROPPABLE := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 8,
      GST_BUFFER_FLAG_DELTA_UNIT := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 9,
      GST_BUFFER_FLAG_TAG_MEMORY := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 10,
      GST_BUFFER_FLAG_SYNC_AFTER := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 11,
      GST_BUFFER_FLAG_NON_DROPPABLE := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 12,
      GST_BUFFER_FLAG_LAST := ord(GST_MINI_OBJECT_FLAG_LAST ) shl 16
    );

  // T_GstBuffer moved to gstrec.inc

    T_GstParentBufferMeta = record
        parent : TGstMeta;
        buffer : PGstBuffer;
      end;
    TGstParentBufferMeta = T_GstParentBufferMeta;


    T_GstReferenceTimestampMeta = record
        parent : TGstMeta;
        reference : PGstCaps;
        timestamp : TGstClockTime;
        duration : TGstClockTime;
      end;
    TGstReferenceTimestampMeta = T_GstReferenceTimestampMeta;

    TGstBuffer_autoptr = PGstBuffer;

    TGstBuffer_listautoptr = PGList;

    TGstBuffer_slistautoptr = PGSList;

    TGstBuffer_queueautoptr = PGQueue;
    TGstBufferPool_autoptr = PGstBufferPool;

    TGstBufferPool_listautoptr = PGList;

    TGstBufferPool_slistautoptr = PGSList;

    TGstBufferPool_queueautoptr = PGQueue;

  function gst_buffer_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffer_get_type';

  function gst_buffer_get_max_memory:Tguint;cdecl;external gstreamerlib name 'gst_buffer_get_max_memory';

  function gst_buffer_new:PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_new';

  function gst_buffer_new_allocate(allocator:PGstAllocator; size:Tgsize; params:PGstAllocationParams):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_new_allocate';

  function gst_buffer_new_wrapped_full(flags:TGstMemoryFlags; data:Tgpointer; maxsize:Tgsize; offset:Tgsize; size:Tgsize; 
             user_data:Tgpointer; notify:TGDestroyNotify):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_new_wrapped_full';

  function gst_buffer_new_wrapped(data:Tgpointer; size:Tgsize):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_new_wrapped';

  function gst_buffer_new_wrapped_bytes(bytes:PGBytes):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_new_wrapped_bytes';

  function gst_buffer_n_memory(buffer:PGstBuffer):Tguint;cdecl;external gstreamerlib name 'gst_buffer_n_memory';

  procedure gst_buffer_insert_memory(buffer:PGstBuffer; idx:Tgint; mem:PGstMemory);cdecl;external gstreamerlib name 'gst_buffer_insert_memory';

  procedure gst_buffer_replace_memory_range(buffer:PGstBuffer; idx:Tguint; length:Tgint; mem:PGstMemory);cdecl;external gstreamerlib name 'gst_buffer_replace_memory_range';

  function gst_buffer_peek_memory(buffer:PGstBuffer; idx:Tguint):PGstMemory;cdecl;external gstreamerlib name 'gst_buffer_peek_memory';

  function gst_buffer_get_memory_range(buffer:PGstBuffer; idx:Tguint; length:Tgint):PGstMemory;cdecl;external gstreamerlib name 'gst_buffer_get_memory_range';

  procedure gst_buffer_remove_memory_range(buffer:PGstBuffer; idx:Tguint; length:Tgint);cdecl;external gstreamerlib name 'gst_buffer_remove_memory_range';

  procedure gst_buffer_prepend_memory(buffer:PGstBuffer; mem:PGstMemory);cdecl;external gstreamerlib name 'gst_buffer_prepend_memory';

  procedure gst_buffer_append_memory(buffer:PGstBuffer; mem:PGstMemory);cdecl;external gstreamerlib name 'gst_buffer_append_memory';

  procedure gst_buffer_replace_memory(buffer:PGstBuffer; idx:Tguint; mem:PGstMemory);cdecl;external gstreamerlib name 'gst_buffer_replace_memory';

  procedure gst_buffer_replace_all_memory(buffer:PGstBuffer; mem:PGstMemory);cdecl;external gstreamerlib name 'gst_buffer_replace_all_memory';

  function gst_buffer_get_memory(buffer:PGstBuffer; idx:Tguint):PGstMemory;cdecl;external gstreamerlib name 'gst_buffer_get_memory';

  function gst_buffer_get_all_memory(buffer:PGstBuffer):PGstMemory;cdecl;external gstreamerlib name 'gst_buffer_get_all_memory';

  procedure gst_buffer_remove_memory(buffer:PGstBuffer; idx:Tguint);cdecl;external gstreamerlib name 'gst_buffer_remove_memory';

  procedure gst_buffer_remove_all_memory(buffer:PGstBuffer);cdecl;external gstreamerlib name 'gst_buffer_remove_all_memory';

  function gst_buffer_find_memory(buffer:PGstBuffer; offset:Tgsize; size:Tgsize; idx:Pguint; length:Pguint; 
             skip:Pgsize):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_find_memory';

  function gst_buffer_is_memory_range_writable(buffer:PGstBuffer; idx:Tguint; length:Tgint):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_is_memory_range_writable';

  function gst_buffer_is_all_memory_writable(buffer:PGstBuffer):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_is_all_memory_writable';

  function gst_buffer_fill(buffer:PGstBuffer; offset:Tgsize; src:Tgconstpointer; size:Tgsize):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_fill';

  function gst_buffer_extract(buffer:PGstBuffer; offset:Tgsize; dest:Tgpointer; size:Tgsize):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_extract';

  function gst_buffer_memcmp(buffer:PGstBuffer; offset:Tgsize; mem:Tgconstpointer; size:Tgsize):Tgint;cdecl;external gstreamerlib name 'gst_buffer_memcmp';

  function gst_buffer_memset(buffer:PGstBuffer; offset:Tgsize; val:Tguint8; size:Tgsize):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_memset';

  function gst_buffer_get_sizes_range(buffer:PGstBuffer; idx:Tguint; length:Tgint; offset:Pgsize; maxsize:Pgsize):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_get_sizes_range';

  function gst_buffer_resize_range(buffer:PGstBuffer; idx:Tguint; length:Tgint; offset:Tgssize; size:Tgssize):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_resize_range';

  function gst_buffer_get_sizes(buffer:PGstBuffer; offset:Pgsize; maxsize:Pgsize):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_get_sizes';

  function gst_buffer_get_size(buffer:PGstBuffer):Tgsize;cdecl;external gstreamerlib name 'gst_buffer_get_size';

  procedure gst_buffer_resize(buffer:PGstBuffer; offset:Tgssize; size:Tgssize);cdecl;external gstreamerlib name 'gst_buffer_resize';

  procedure gst_buffer_set_size(buffer:PGstBuffer; size:Tgssize);cdecl;external gstreamerlib name 'gst_buffer_set_size';

  function gst_buffer_map_range(buffer:PGstBuffer; idx:Tguint; length:Tgint; info:PGstMapInfo; flags:TGstMapFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_map_range';

  function gst_buffer_map(buffer:PGstBuffer; info:PGstMapInfo; flags:TGstMapFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_map';

  procedure gst_buffer_unmap(buffer:PGstBuffer; info:PGstMapInfo);cdecl;external gstreamerlib name 'gst_buffer_unmap';

  procedure gst_buffer_extract_dup(buffer:PGstBuffer; offset:Tgsize; size:Tgsize; dest:Pgpointer; dest_size:Pgsize);cdecl;external gstreamerlib name 'gst_buffer_extract_dup';

  function gst_buffer_get_flags(buffer:PGstBuffer):TGstBufferFlags;cdecl;external gstreamerlib name 'gst_buffer_get_flags';

  function gst_buffer_has_flags(buffer:PGstBuffer; flags:TGstBufferFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_has_flags';

  function gst_buffer_set_flags(buffer:PGstBuffer; flags:TGstBufferFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_set_flags';

  function gst_buffer_unset_flags(buffer:PGstBuffer; flags:TGstBufferFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_unset_flags';

  {
  
  static inline GstBuffer *
  gst_buffer_ref (GstBuffer * buf)
  
    return (GstBuffer *) gst_mini_object_ref (((GstMiniObject*)(buf)));
  
  # 441 "gstbuffer.h"
  static inline void
  gst_buffer_unref (GstBuffer * buf)
  
    gst_mini_object_unref (((GstMiniObject*)(buf)));
  
  # 460 "gstbuffer.h"
  static inline void
  gst_clear_buffer (GstBuffer ** buf_ptr)
  
    do  _Static_assert (sizeof *(((GstMiniObject **) buf_ptr)) == sizeof (gpointer), "Expression evaluates to false"); __typeof__((((GstMiniObject **) buf_ptr))) _pp = (((GstMiniObject **) buf_ptr)); __typeof__(*(((GstMiniObject **) buf_ptr))) _ptr = *_pp; *_pp = 
  # 463 "gstbuffer.h" 3 4
   ((void *)0)
  # 463 "gstbuffer.h"
   ; if (_ptr) (gst_mini_object_unref) (_ptr);  while (0) ;
  
  # 479 "gstbuffer.h"
  static inline GstBuffer *
  gst_buffer_copy (const GstBuffer * buf)
  
    return (((GstBuffer *)(gst_mini_object_copy (((const GstMiniObject*)(buf))))));
  
  
   }
(* Const before type ignored *)
  function gst_buffer_copy_deep(buf:PGstBuffer):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_copy_deep';



  function gst_buffer_copy_into(dest:PGstBuffer; src:PGstBuffer; flags:TGstBufferCopyFlags; offset:Tgsize; size:Tgsize):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_copy_into';

  {
  static inline gboolean
  gst_buffer_replace (GstBuffer **obuf, GstBuffer *nbuf)
  
    return gst_mini_object_replace ((GstMiniObject **) obuf, (GstMiniObject *) nbuf);
  
  
   }
  function gst_buffer_copy_region(parent:PGstBuffer; flags:TGstBufferCopyFlags; offset:Tgsize; size:Tgsize):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_copy_region';

  function gst_buffer_append_region(buf1:PGstBuffer; buf2:PGstBuffer; offset:Tgssize; size:Tgssize):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_append_region';

  function gst_buffer_append(buf1:PGstBuffer; buf2:PGstBuffer):PGstBuffer;cdecl;external gstreamerlib name 'gst_buffer_append';


  type

    TGstBufferForeachMetaFunc = function (buffer:PGstBuffer; meta:PPGstMeta; user_data:Tgpointer):Tgboolean;cdecl;

  function gst_buffer_get_meta(buffer:PGstBuffer; api:TGType):PGstMeta;cdecl;external gstreamerlib name 'gst_buffer_get_meta';

  function gst_buffer_get_n_meta(buffer:PGstBuffer; api_type:TGType):Tguint;cdecl;external gstreamerlib name 'gst_buffer_get_n_meta';

(* Const before type ignored *)
  function gst_buffer_add_meta(buffer:PGstBuffer; info:PGstMetaInfo; params:Tgpointer):PGstMeta;cdecl;external gstreamerlib name 'gst_buffer_add_meta';

  function gst_buffer_remove_meta(buffer:PGstBuffer; meta:PGstMeta):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_remove_meta';

  function gst_buffer_iterate_meta(buffer:PGstBuffer; state:Pgpointer):PGstMeta;cdecl;external gstreamerlib name 'gst_buffer_iterate_meta';

  function gst_buffer_iterate_meta_filtered(buffer:PGstBuffer; state:Pgpointer; meta_api_type:TGType):PGstMeta;cdecl;external gstreamerlib name 'gst_buffer_iterate_meta_filtered';

  function gst_buffer_foreach_meta(buffer:PGstBuffer; func:TGstBufferForeachMetaFunc; user_data:Tgpointer):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_foreach_meta';


  function gst_parent_buffer_meta_api_get_type:TGType;cdecl;external gstreamerlib name 'gst_parent_buffer_meta_api_get_type';

(* Const before type ignored *)
  function gst_parent_buffer_meta_get_info:PGstMetaInfo;cdecl;external gstreamerlib name 'gst_parent_buffer_meta_get_info';

  function gst_buffer_add_parent_buffer_meta(buffer:PGstBuffer; ref:PGstBuffer):PGstParentBufferMeta;cdecl;external gstreamerlib name 'gst_buffer_add_parent_buffer_meta';



  function gst_reference_timestamp_meta_api_get_type:TGType;cdecl;external gstreamerlib name 'gst_reference_timestamp_meta_api_get_type';

(* Const before type ignored *)
  function gst_reference_timestamp_meta_get_info:PGstMetaInfo;cdecl;external gstreamerlib name 'gst_reference_timestamp_meta_get_info';

  function gst_buffer_add_reference_timestamp_meta(buffer:PGstBuffer; reference:PGstCaps; timestamp:TGstClockTime; duration:TGstClockTime):PGstReferenceTimestampMeta;cdecl;external gstreamerlib name 'gst_buffer_add_reference_timestamp_meta';

  function gst_buffer_get_reference_timestamp_meta(buffer:PGstBuffer; reference:PGstCaps):PGstReferenceTimestampMeta;cdecl;external gstreamerlib name 'gst_buffer_get_reference_timestamp_meta';



