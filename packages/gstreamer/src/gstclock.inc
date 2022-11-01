{* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *                    2005 Wim Taymans <wim@fluendo.com>
 *
 * gstclock.h: Header for clock subsystem
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
    P_GstClock  = ^T_GstClock;
    PPGstClock = ^P_GstClock;
    P_GstClockClass  = ^T_GstClockClass;
    Pgdouble  = ^Tgdouble;
    PGstClock_autoptr  = ^TGstClock_autoptr;
    PGstClock_listautoptr  = ^TGstClock_listautoptr;
    PGstClock_queueautoptr  = ^TGstClock_queueautoptr;
    PGstClock_slistautoptr  = ^TGstClock_slistautoptr;
    PGstClockEntry  = ^TGstClockEntry;
    PGstClockFlags  = ^TGstClockFlags;
    PGstClockTime  = ^TGstClockTime;
    PGstClockTimeDiff  = ^TGstClockTimeDiff;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

TGstClockReturn = (
  GST_CLOCK_OK = 0,
  GST_CLOCK_EARLY = 1,
  GST_CLOCK_UNSCHEDULED = 2,
  GST_CLOCK_BUSY = 3,
  GST_CLOCK_BADTIME = 4,
  GST_CLOCK_ERROR = 5,
  GST_CLOCK_UNSUPPORTED = 6,
  GST_CLOCK_DONE = 7
);

TGstClockEntryType = (
  GST_CLOCK_ENTRY_SINGLE,
  GST_CLOCK_ENTRY_PERIODIC
);
TGstClockID = gpointer ;

 TGstClockCallback = function (clock : PGstClock; time : TGstClockTime; id : TGstClockID ; user_data : gpointer): gboolean; cdecl;

T_GstClockEntry = record
    refcount : gint;
    clock : ^GstClock;
    _type : TGstClockEntryType;
    time : TGstClockTime;
    interval : TGstClockTime;
    status : TGstClockReturn;
    func : TGstClockCallback;
    user_data : gpointer;
    destroy_data : GDestroyNotify;
    unscheduled : gboolean;
    woken_up : gboolean;
    _gst_reserved : array[0..3] of gpointer;
  end;
TGstClockEntry =T_GstClockEntry;

    TGstClockFlags = (
      GST_CLOCK_FLAG_CAN_DO_SINGLE_SYNC := GST_OBJECT_FLAG_LAST shl 0,
      GST_CLOCK_FLAG_CAN_DO_SINGLE_ASYNC := GST_OBJECT_FLAG_LAST shl 1,
      GST_CLOCK_FLAG_CAN_DO_PERIODIC_SYNC := GST_OBJECT_FLAG_LAST shl 2,
      GST_CLOCK_FLAG_CAN_DO_PERIODIC_ASYNC := GST_OBJECT_FLAG_LAST shl 3,
      GST_CLOCK_FLAG_CAN_SET_RESOLUTION := GST_OBJECT_FLAG_LAST shl 4,
      GST_CLOCK_FLAG_CAN_SET_MASTER := GST_OBJECT_FLAG_LAST shl 5,
      GST_CLOCK_FLAG_NEEDS_STARTUP_SYNC := GST_OBJECT_FLAG_LAST shl 6,
      GST_CLOCK_FLAG_LAST := GST_OBJECT_FLAG_LAST shl 8
      );



    T_GstClockClass = record
        parent_class : TGstObjectClass;
        change_resolution : function (clock:PGstClock; old_resolution:TGstClockTime; new_resolution:TGstClockTime):TGstClockTime;cdecl;
        get_resolution : function (clock:PGstClock):TGstClockTime;cdecl;
        get_internal_time : function (clock:PGstClock):TGstClockTime;cdecl;
        wait : function (clock:PGstClock; entry:PGstClockEntry; jitter:PGstClockTimeDiff):TGstClockReturn;cdecl;
        wait_async : function (clock:PGstClock; entry:PGstClockEntry):TGstClockReturn;cdecl;
        unschedule : procedure (clock:PGstClock; entry:PGstClockEntry);cdecl;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstClockClass = T_GstClockClass;

  TGstClock_autoptr = PGstClock;

  TGstClock_listautoptr = PGList;

  TGstClock_slistautoptr = PGSList;

  TGstClock_queueautoptr = PGQueue;


  function gst_clock_get_type:TGType;cdecl;external gstreamerlib name 'gst_clock_get_type';

  function gst_clock_set_resolution(clock:PGstClock; resolution:TGstClockTime):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_set_resolution';

  function gst_clock_get_resolution(clock:PGstClock):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_get_resolution';

  function gst_clock_get_time(clock:PGstClock):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_get_time';

  procedure gst_clock_set_calibration(clock:PGstClock; internal:TGstClockTime; external:TGstClockTime; rate_num:TGstClockTime; rate_denom:TGstClockTime);cdecl;external gstreamerlib name 'gst_clock_set_calibration';

  procedure gst_clock_get_calibration(clock:PGstClock; internal:PGstClockTime; external:PGstClockTime; rate_num:PGstClockTime; rate_denom:PGstClockTime);cdecl;external gstreamerlib name 'gst_clock_get_calibration';

  function gst_clock_set_master(clock:PGstClock; master:PGstClock):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_set_master';

  function gst_clock_get_master(clock:PGstClock):PGstClock;cdecl;external gstreamerlib name 'gst_clock_get_master';

  procedure gst_clock_set_timeout(clock:PGstClock; timeout:TGstClockTime);cdecl;external gstreamerlib name 'gst_clock_set_timeout';

  function gst_clock_get_timeout(clock:PGstClock):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_get_timeout';

  function gst_clock_add_observation(clock:PGstClock; slave:TGstClockTime; master:TGstClockTime; r_squared:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_add_observation';

  function gst_clock_add_observation_unapplied(clock:PGstClock; slave:TGstClockTime; master:TGstClockTime; r_squared:Pgdouble; internal:PGstClockTime; 
             external:PGstClockTime; rate_num:PGstClockTime; rate_denom:PGstClockTime):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_add_observation_unapplied';

  function gst_clock_get_internal_time(clock:PGstClock):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_get_internal_time';

  function gst_clock_adjust_unlocked(clock:PGstClock; internal:TGstClockTime):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_adjust_unlocked';

  function gst_clock_adjust_with_calibration(clock:PGstClock; internal_target:TGstClockTime; cinternal:TGstClockTime; cexternal:TGstClockTime; cnum:TGstClockTime; 
             cdenom:TGstClockTime):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_adjust_with_calibration';

  function gst_clock_unadjust_with_calibration(clock:PGstClock; external_target:TGstClockTime; cinternal:TGstClockTime; cexternal:TGstClockTime; cnum:TGstClockTime; 
             cdenom:TGstClockTime):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_unadjust_with_calibration';

  function gst_clock_unadjust_unlocked(clock:PGstClock; external:TGstClockTime):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_unadjust_unlocked';

  function gst_clock_wait_for_sync(clock:PGstClock; timeout:TGstClockTime):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_wait_for_sync';

  function gst_clock_is_synced(clock:PGstClock):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_is_synced';

  procedure gst_clock_set_synced(clock:PGstClock; synced:Tgboolean);cdecl;external gstreamerlib name 'gst_clock_set_synced';

  function gst_clock_new_single_shot_id(clock:PGstClock; time:TGstClockTime):TGstClockID;cdecl;external gstreamerlib name 'gst_clock_new_single_shot_id';

  function gst_clock_new_periodic_id(clock:PGstClock; start_time:TGstClockTime; interval:TGstClockTime):TGstClockID;cdecl;external gstreamerlib name 'gst_clock_new_periodic_id';

  function gst_clock_id_ref(id:TGstClockID):TGstClockID;cdecl;external gstreamerlib name 'gst_clock_id_ref';

  procedure gst_clock_id_unref(id:TGstClockID);cdecl;external gstreamerlib name 'gst_clock_id_unref';

  function gst_clock_id_compare_func(id1:Tgconstpointer; id2:Tgconstpointer):Tgint;cdecl;external gstreamerlib name 'gst_clock_id_compare_func';

  function gst_clock_id_get_clock(id:TGstClockID):PGstClock;cdecl;external gstreamerlib name 'gst_clock_id_get_clock';

  function gst_clock_id_uses_clock(id:TGstClockID; clock:PGstClock):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_id_uses_clock';

  function gst_clock_id_get_time(id:TGstClockID):TGstClockTime;cdecl;external gstreamerlib name 'gst_clock_id_get_time';

  function gst_clock_id_wait(id:TGstClockID; jitter:PGstClockTimeDiff):TGstClockReturn;cdecl;external gstreamerlib name 'gst_clock_id_wait';

  function gst_clock_id_wait_async(id:TGstClockID; func:TGstClockCallback; user_data:Tgpointer; destroy_data:TGDestroyNotify):TGstClockReturn;cdecl;external gstreamerlib name 'gst_clock_id_wait_async';

  procedure gst_clock_id_unschedule(id:TGstClockID);cdecl;external gstreamerlib name 'gst_clock_id_unschedule';

  function gst_clock_single_shot_id_reinit(clock:PGstClock; id:TGstClockID; time:TGstClockTime):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_single_shot_id_reinit';

  function gst_clock_periodic_id_reinit(clock:PGstClock; id:TGstClockID; start_time:TGstClockTime; interval:TGstClockTime):Tgboolean;cdecl;external gstreamerlib name 'gst_clock_periodic_id_reinit';



