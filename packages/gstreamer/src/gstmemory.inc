{
/* GStreamer
 * Copyright (C) 2009 Wim Taymans <wim.taymans@gmail.be>
 *
 * gstmemory.h: Header for memory blocks
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
    P_GstMemory  = ^T_GstMemory;
    PGstAllocator  = ^TGstAllocator;
    PGstAllocator_autoptr  = ^TGstAllocator_autoptr;
    PGstAllocator_listautoptr  = ^TGstAllocator_listautoptr;
    PGstAllocator_queueautoptr  = ^TGstAllocator_queueautoptr;
    PGstAllocator_slistautoptr  = ^TGstAllocator_slistautoptr;
    PGstMapFlags  = ^TGstMapFlags;
    PGstMapInfo  = ^TGstMapInfo;
    PGstMemory  = ^TGstMemory;
    PGstMemory_autoptr  = ^TGstMemory_autoptr;
    PGstMemory_listautoptr  = ^TGstMemory_listautoptr;
    PGstMemory_queueautoptr  = ^TGstMemory_queueautoptr;
    PGstMemory_slistautoptr  = ^TGstMemory_slistautoptr;
    PGstMemoryCopyFunction  = ^TGstMemoryCopyFunction;
    PGstMemoryShareFunction  = ^TGstMemoryShareFunction;
    Pguint8  = ^Tguint8;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


    T_GstMemory = record
        mini_object : TGstMiniObject;
        allocator : PGstAllocator;
        parent : PGstMemory;
        maxsize : Tgsize;
        align : Tgsize;
        offset : Tgsize;
        size : Tgsize;
      end;
    TGstMemory = T_GstMemory ;



    TGstMapFlags = (
      GST_MAP_READ := Ord(GST_LOCK_FLAG_READ),
      GST_MAP_WRITE := Ord(GST_LOCK_FLAG_WRITE),
      GST_MAP_FLAG_LAST := 1 shl 16);

    TGstMapInfo = record
        memory : PGstMemory;
        flags : TGstMapFlags;
        data : Pguint8;
        size : Tgsize;
        maxsize : Tgsize;
        user_data : array[0..3] of Tgpointer;
        _gst_reserved : array[0..3] of Tgpointer;
      end;

    TGstMemoryMapFunction = function (mem:PGstMemory; maxsize:Tgsize; flags:TGstMapFlags):Tgpointer;cdecl;

    TGstMemoryMapFullFunction = function (mem:PGstMemory; info:PGstMapInfo; maxsize:Tgsize):Tgpointer;cdecl;

    TGstMemoryUnmapFunction = procedure (mem:PGstMemory);cdecl;

    TGstMemoryUnmapFullFunction = procedure (mem:PGstMemory; info:PGstMapInfo);cdecl;


    TGstMemoryCopyFunction = function (mem:PGstMemory; offset:Tgssize; size:Tgssize):PGstMemory;cdecl;


    TGstMemoryShareFunction = function (mem:PGstMemory; offset:Tgssize; size:Tgssize):PGstMemory;cdecl;

    TGstMemoryIsSpanFunction = function (mem1:PGstMemory; mem2:PGstMemory; offset:Pgsize):Tgboolean;cdecl;


    TGstMemory_autoptr = PGstMemory;

    TGstMemory_listautoptr = PGList;

    TGstMemory_slistautoptr = PGSList;

    TGstMemory_queueautoptr = PGQueue;
    TGstAllocator_autoptr = PGstAllocator;

    TGstAllocator_listautoptr = PGList;

    TGstAllocator_slistautoptr = PGSList;

    TGstAllocator_queueautoptr = PGQueue;

    // Moved here from gstallocator.inc
    TGstAllocatorPrivate = record end;
    T_GstAllocatorPrivate = TGstAllocatorPrivate;
    PGstAllocatorPrivate = ^TGstAllocatorPrivate;

    T_GstAllocator = record
        object_ : TGstObject;
        mem_type : Pgchar;
        mem_map : TGstMemoryMapFunction;
        mem_unmap : TGstMemoryUnmapFunction;
        mem_copy : TGstMemoryCopyFunction;
        mem_share : TGstMemoryShareFunction;
        mem_is_span : TGstMemoryIsSpanFunction;
        mem_map_full : TGstMemoryMapFullFunction;
        mem_unmap_full : TGstMemoryUnmapFullFunction;
        _gst_reserved : array[0..(4-2)-1] of Tgpointer;
        priv : PGstAllocatorPrivate;
      end;
    TGstAllocator = T_GstAllocator;

  function gst_memory_get_type:TGType;cdecl;external gstreamerlib name 'gst_memory_get_type';

  procedure gst_memory_init(mem:PGstMemory; flags:TGstMemoryFlags; allocator:PGstAllocator; parent:PGstMemory; maxsize:Tgsize; 
              align:Tgsize; offset:Tgsize; size:Tgsize);cdecl;external gstreamerlib name 'gst_memory_init';

(* Const before type ignored *)
  function gst_memory_is_type(mem:PGstMemory; mem_type:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_memory_is_type';

  {
  
  static inline GstMemory *
  gst_memory_ref (GstMemory * memory)
  
    return (GstMemory *) gst_mini_object_ref (((GstMiniObject*)(memory)));
  
  
  
  
  
  
  
  
  static inline void
  gst_memory_unref (GstMemory * memory)
  
    gst_mini_object_unref (((GstMiniObject*)(memory)));
  
  
   }
  function gst_memory_get_sizes(mem:PGstMemory; offset:Pgsize; maxsize:Pgsize):Tgsize;cdecl;external gstreamerlib name 'gst_memory_get_sizes';

  procedure gst_memory_resize(mem:PGstMemory; offset:Tgssize; size:Tgsize);cdecl;external gstreamerlib name 'gst_memory_resize';

  function gst_memory_make_mapped(mem:PGstMemory; info:PGstMapInfo; flags:TGstMapFlags):PGstMemory;cdecl;external gstreamerlib name 'gst_memory_make_mapped';

  function gst_memory_map(mem:PGstMemory; info:PGstMapInfo; flags:TGstMapFlags):Tgboolean;cdecl;external gstreamerlib name 'gst_memory_map';

  procedure gst_memory_unmap(mem:PGstMemory; info:PGstMapInfo);cdecl;external gstreamerlib name 'gst_memory_unmap';

  function gst_memory_copy(mem:PGstMemory; offset:Tgssize; size:Tgssize):PGstMemory;cdecl;external gstreamerlib name 'gst_memory_copy';

  function gst_memory_share(mem:PGstMemory; offset:Tgssize; size:Tgssize):PGstMemory;cdecl;external gstreamerlib name 'gst_memory_share';

  function gst_memory_is_span(mem1:PGstMemory; mem2:PGstMemory; offset:Pgsize):Tgboolean;cdecl;external gstreamerlib name 'gst_memory_is_span';


  var
    _gst_memory_type : TGType;cvar;external;


