{* GStreamer
 * Copyright (C) 2009 Wim Taymans <wim.taymans@gmail.be>
 *
 * gstallocator.h: Header for memory allocation
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
    P_GstAllocationParams  = ^T_GstAllocationParams;
    P_GstAllocator  = ^T_GstAllocator;
    PPGstAllocator = ^P_GstAllocator;
    P_GstAllocatorClass  = ^T_GstAllocatorClass;
    PGstAllocationParams  = ^TGstAllocationParams;
    PGstAllocationParams_autoptr  = ^TGstAllocationParams_autoptr;
    PGstAllocationParams_listautoptr  = ^TGstAllocationParams_listautoptr;
    PGstAllocationParams_queueautoptr  = ^TGstAllocationParams_queueautoptr;
    PGstAllocationParams_slistautoptr  = ^TGstAllocationParams_slistautoptr;

    PGstAllocatorFlags  = ^TGstAllocatorFlags;



    T_GstAllocationParams = record
        flags : TGstMemoryFlags;
        align : Tgsize;
        prefix : Tgsize;
        padding : Tgsize;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstAllocationParams = T_GstAllocationParams;

    TGstAllocatorFlags = (
      GST_ALLOCATOR_FLAG_CUSTOM_ALLOC := GST_OBJECT_FLAG_LAST shl 0,
      GST_ALLOCATOR_FLAG_LAST := GST_OBJECT_FLAG_LAST shl 16
    );
(* Const before type ignored *)



    T_GstAllocatorClass = record
        object_class : TGstObjectClass;
        alloc : function (allocator:PGstAllocator; size:Tgsize; params:PGstAllocationParams):PGstMemory;cdecl;
        free : procedure (allocator:PGstAllocator; memory:PGstMemory);cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstAllocatorClass = T_GstAllocatorClass;

    TGstAllocationParams_autoptr = PGstAllocationParams;

    TGstAllocationParams_listautoptr = PGList;

    TGstAllocationParams_slistautoptr = PGSList;

    TGstAllocationParams_queueautoptr = PGQueue;

    function gst_allocation_params_get_type:TGType;cdecl;external gstreamerlib name 'gst_allocation_params_get_type';

  function gst_allocator_get_type:TGType;cdecl;external gstreamerlib name 'gst_allocator_get_type';

(* Const before type ignored *)
  procedure gst_allocator_register(name:Pgchar; allocator:PGstAllocator);cdecl;external gstreamerlib name 'gst_allocator_register';

(* Const before type ignored *)
  function gst_allocator_find(name:Pgchar):PGstAllocator;cdecl;external gstreamerlib name 'gst_allocator_find';

  procedure gst_allocator_set_default(allocator:PGstAllocator);cdecl;external gstreamerlib name 'gst_allocator_set_default';

  procedure gst_allocation_params_init(params:PGstAllocationParams);cdecl;external gstreamerlib name 'gst_allocation_params_init';

(* Const before type ignored *)
  function gst_allocation_params_copy(params:PGstAllocationParams):PGstAllocationParams;cdecl;external gstreamerlib name 'gst_allocation_params_copy';

  procedure gst_allocation_params_free(params:PGstAllocationParams);cdecl;external gstreamerlib name 'gst_allocation_params_free';

  function gst_allocator_alloc(allocator:PGstAllocator; size:Tgsize; params:PGstAllocationParams):PGstMemory;cdecl;external gstreamerlib name 'gst_allocator_alloc';

  procedure gst_allocator_free(allocator:PGstAllocator; memory:PGstMemory);cdecl;external gstreamerlib name 'gst_allocator_free';

  function gst_memory_new_wrapped(flags:TGstMemoryFlags; data:Tgpointer; maxsize:Tgsize; offset:Tgsize; size:Tgsize; 
             user_data:Tgpointer; notify:TGDestroyNotify):PGstMemory;cdecl;external gstreamerlib name 'gst_memory_new_wrapped';



    var
      gst_memory_alignment : Tgsize;cvar;external;

