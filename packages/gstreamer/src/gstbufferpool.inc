{* GStreamer
 * Copyright (C) 2010 Wim Taymans <wim.taymans@gmail.com>
 *
 * gstbufferpool.h: Header for GstBufferPool object
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
    P_GstBufferPoolClass  = ^T_GstBufferPoolClass;

    PGstBufferPoolAcquireParams  = ^TGstBufferPoolAcquireParams;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}





(* Const before type ignored *)
    T_GstBufferPoolClass = record
        object_class : TGstObjectClass;
        get_options : function (pool:PGstBufferPool):PPgchar;cdecl;
        set_config : function (pool:PGstBufferPool; config:PGstStructure):Tgboolean;cdecl;
        start : function (pool:PGstBufferPool):Tgboolean;cdecl;
        stop : function (pool:PGstBufferPool):Tgboolean;cdecl;
        acquire_buffer : function (pool:PGstBufferPool; buffer:PPGstBuffer; params:PGstBufferPoolAcquireParams):TGstFlowReturn;cdecl;
        alloc_buffer : function (pool:PGstBufferPool; buffer:PPGstBuffer; params:PGstBufferPoolAcquireParams):TGstFlowReturn;cdecl;
        reset_buffer : procedure (pool:PGstBufferPool; buffer:PGstBuffer);cdecl;
        release_buffer : procedure (pool:PGstBufferPool; buffer:PGstBuffer);cdecl;
        free_buffer : procedure (pool:PGstBufferPool; buffer:PGstBuffer);cdecl;
        flush_start : procedure (pool:PGstBufferPool);cdecl;
        flush_stop : procedure (pool:PGstBufferPool);cdecl;
        _gst_reserved : array[0..(4-2)-1] of Tgpointer;
      end;
    TGstBufferPoolClass = T_GstBufferPoolClass;


  function gst_buffer_pool_get_type:TGType;cdecl;external gstreamerlib name 'gst_buffer_pool_get_type';

  function gst_buffer_pool_new:PGstBufferPool;cdecl;external gstreamerlib name 'gst_buffer_pool_new';

  function gst_buffer_pool_set_active(pool:PGstBufferPool; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_set_active';

  function gst_buffer_pool_is_active(pool:PGstBufferPool):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_is_active';

  function gst_buffer_pool_set_config(pool:PGstBufferPool; config:PGstStructure):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_set_config';

  function gst_buffer_pool_get_config(pool:PGstBufferPool):PGstStructure;cdecl;external gstreamerlib name 'gst_buffer_pool_get_config';

(* Const before type ignored *)
  function gst_buffer_pool_get_options(pool:PGstBufferPool):PPgchar;cdecl;external gstreamerlib name 'gst_buffer_pool_get_options';

(* Const before type ignored *)
  function gst_buffer_pool_has_option(pool:PGstBufferPool; option:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_has_option';

  procedure gst_buffer_pool_set_flushing(pool:PGstBufferPool; flushing:Tgboolean);cdecl;external gstreamerlib name 'gst_buffer_pool_set_flushing';

  procedure gst_buffer_pool_config_set_params(config:PGstStructure; caps:PGstCaps; size:Tguint; min_buffers:Tguint; max_buffers:Tguint);cdecl;external gstreamerlib name 'gst_buffer_pool_config_set_params';

  function gst_buffer_pool_config_get_params(config:PGstStructure; caps:PPGstCaps; size:Pguint; min_buffers:Pguint; max_buffers:Pguint):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_config_get_params';

(* Const before type ignored *)
  procedure gst_buffer_pool_config_set_allocator(config:PGstStructure; allocator:PGstAllocator; params:PGstAllocationParams);cdecl;external gstreamerlib name 'gst_buffer_pool_config_set_allocator';

  function gst_buffer_pool_config_get_allocator(config:PGstStructure; allocator:PPGstAllocator; params:PGstAllocationParams):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_config_get_allocator';

  function gst_buffer_pool_config_n_options(config:PGstStructure):Tguint;cdecl;external gstreamerlib name 'gst_buffer_pool_config_n_options';

(* Const before type ignored *)
  procedure gst_buffer_pool_config_add_option(config:PGstStructure; option:Pgchar);cdecl;external gstreamerlib name 'gst_buffer_pool_config_add_option';

(* Const before type ignored *)
  function gst_buffer_pool_config_get_option(config:PGstStructure; index:Tguint):Pgchar;cdecl;external gstreamerlib name 'gst_buffer_pool_config_get_option';

(* Const before type ignored *)
  function gst_buffer_pool_config_has_option(config:PGstStructure; option:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_config_has_option';

  function gst_buffer_pool_config_validate_params(config:PGstStructure; caps:PGstCaps; size:Tguint; min_buffers:Tguint; max_buffers:Tguint):Tgboolean;cdecl;external gstreamerlib name 'gst_buffer_pool_config_validate_params';

  function gst_buffer_pool_acquire_buffer(pool:PGstBufferPool; buffer:PPGstBuffer; params:PGstBufferPoolAcquireParams):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_buffer_pool_acquire_buffer';

  procedure gst_buffer_pool_release_buffer(pool:PGstBufferPool; buffer:PGstBuffer);cdecl;external gstreamerlib name 'gst_buffer_pool_release_buffer';


