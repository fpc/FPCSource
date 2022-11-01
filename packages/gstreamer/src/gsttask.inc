{
/* GStreamer
 * Copyright (C) <1999> Erik Walthinsen <omega@cse.ogi.edu>
 *               <2005> Wim Taymans <wim@fluendo.com>
 *
 * gsttask.h: Streaming tasks
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
    P_GstTask  = ^T_GstTask;
    P_GstTaskClass  = ^T_GstTaskClass;
    PGRecMutex  = ^TGRecMutex;
    PGstTask  = ^TGstTask;
    PGstTask_autoptr  = ^TGstTask_autoptr;
    PGstTask_listautoptr  = ^TGstTask_listautoptr;
    PGstTask_queueautoptr  = ^TGstTask_queueautoptr;
    PGstTask_slistautoptr  = ^TGstTask_slistautoptr;
    PGstTaskPrivate  = ^TGstTaskPrivate;
    PGstTaskState  = ^TGstTaskState;
    PGThread  = ^TGThread;

    TGstTaskFunction = procedure (user_data:Tgpointer);cdecl;

    TGstTaskPrivate = record end;

    T_GstTaskPrivate = TGstTaskPrivate;


    TGstTaskState = (GST_TASK_STARTED,GST_TASK_STOPPED,GST_TASK_PAUSED
      );

    TGstTaskThreadFunc = procedure (task:PGstTask; thread:PGThread; user_data:Tgpointer);cdecl;

    T_GstTask = record
        object_ : TGstObject;
        state : TGstTaskState;
        cond : TGCond;
        lock : PGRecMutex;
        func : TGstTaskFunction;
        user_data : Tgpointer;
        notify : TGDestroyNotify;
        running : Tgboolean;
        thread : PGThread;
        priv : PGstTaskPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTask = T_GstTask;


    T_GstTaskClass = record
        parent_class : TGstObjectClass;
        pool : PGstTaskPool;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstTaskClass = T_GstTaskClass;

  TGstTask_autoptr = PGstTask;


  TGstTask_listautoptr = PGList;


  TGstTask_slistautoptr = PGSList;


  TGstTask_queueautoptr = PGQueue;

  procedure gst_task_cleanup_all;cdecl;external gstreamerlib name 'gst_task_cleanup_all';

  function gst_task_get_type:TGType;cdecl;external gstreamerlib name 'gst_task_get_type';

  function gst_task_new(func:TGstTaskFunction; user_data:Tgpointer; notify:TGDestroyNotify):PGstTask;cdecl;external gstreamerlib name 'gst_task_new';

  procedure gst_task_set_lock(task:PGstTask; mutex:PGRecMutex);cdecl;external gstreamerlib name 'gst_task_set_lock';

  function gst_task_get_pool(task:PGstTask):PGstTaskPool;cdecl;external gstreamerlib name 'gst_task_get_pool';

  procedure gst_task_set_pool(task:PGstTask; pool:PGstTaskPool);cdecl;external gstreamerlib name 'gst_task_set_pool';

  procedure gst_task_set_enter_callback(task:PGstTask; enter_func:TGstTaskThreadFunc; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_task_set_enter_callback';

  procedure gst_task_set_leave_callback(task:PGstTask; leave_func:TGstTaskThreadFunc; user_data:Tgpointer; notify:TGDestroyNotify);cdecl;external gstreamerlib name 'gst_task_set_leave_callback';

  function gst_task_get_state(task:PGstTask):TGstTaskState;cdecl;external gstreamerlib name 'gst_task_get_state';

  function gst_task_set_state(task:PGstTask; state:TGstTaskState):Tgboolean;cdecl;external gstreamerlib name 'gst_task_set_state';

  function gst_task_start(task:PGstTask):Tgboolean;cdecl;external gstreamerlib name 'gst_task_start';

  function gst_task_stop(task:PGstTask):Tgboolean;cdecl;external gstreamerlib name 'gst_task_stop';

  function gst_task_pause(task:PGstTask):Tgboolean;cdecl;external gstreamerlib name 'gst_task_pause';

  function gst_task_join(task:PGstTask):Tgboolean;cdecl;external gstreamerlib name 'gst_task_join';



