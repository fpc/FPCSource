{
/* GStreamer
 * Copyright (C) <2009> Wim Taymans <wim.taymans@gmail.com>
 *
 * gsttaskpool.h: Pool for creating streaming threads
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
    P_GstTaskPool  = ^T_GstTaskPool;
    P_GstTaskPoolClass  = ^T_GstTaskPoolClass;
    PGstTaskPool  = ^TGstTaskPool;
    PGstTaskPool_autoptr  = ^TGstTaskPool_autoptr;
    PGstTaskPool_listautoptr  = ^TGstTaskPool_listautoptr;
    PGstTaskPool_queueautoptr  = ^TGstTaskPool_queueautoptr;
    PGstTaskPool_slistautoptr  = ^TGstTaskPool_slistautoptr;
    PGThreadPool  = ^TGThreadPool;

    TGstTaskPoolFunction = procedure (user_data:pointer);cdecl;

    T_GstTaskPool = record
        object_ : TGstObject;
        pool : PGThreadPool;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTaskPool = T_GstTaskPool;


    T_GstTaskPoolClass = record
        parent_class : TGstObjectClass;
        prepare : procedure (pool:PGstTaskPool; error:PPGError);cdecl;
        cleanup : procedure (pool:PGstTaskPool);cdecl;
        push : function (pool:PGstTaskPool; func:TGstTaskPoolFunction; user_data:Tgpointer; error:PPGError):Tgpointer;cdecl;
        join : procedure (pool:PGstTaskPool; id:Tgpointer);cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTaskPoolClass = T_GstTaskPoolClass;



  TGstTaskPool_autoptr = PGstTaskPool;


  TGstTaskPool_listautoptr = PGList;


  TGstTaskPool_slistautoptr = PGSList;


  TGstTaskPool_queueautoptr = PGQueue;

  function gst_task_pool_get_type:TGType;cdecl;external gstreamerlib name 'gst_task_pool_get_type';

  function gst_task_pool_new:PGstTaskPool;cdecl;external gstreamerlib name 'gst_task_pool_new';

  procedure gst_task_pool_prepare(pool:PGstTaskPool; error:PPGError);cdecl;external gstreamerlib name 'gst_task_pool_prepare';

  function gst_task_pool_push(pool:PGstTaskPool; func:TGstTaskPoolFunction; user_data:Tgpointer; error:PPGError):Tgpointer;cdecl;external gstreamerlib name 'gst_task_pool_push';

  procedure gst_task_pool_join(pool:PGstTaskPool; id:Tgpointer);cdecl;external gstreamerlib name 'gst_task_pool_join';

  procedure gst_task_pool_cleanup(pool:PGstTaskPool);cdecl;external gstreamerlib name 'gst_task_pool_cleanup';



