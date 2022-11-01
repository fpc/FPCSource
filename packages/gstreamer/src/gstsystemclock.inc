{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *                    2005 Wim Taymans <wim@fluendo.com>
 *
 * gstsystemclock.h: A clock implementation based on system time
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
    P_GstSystemClock  = ^T_GstSystemClock;
    P_GstSystemClockClass  = ^T_GstSystemClockClass;
    PGstClockType  = ^TGstClockType;
    PGstSystemClock  = ^TGstSystemClock;
    PGstSystemClock_autoptr  = ^TGstSystemClock_autoptr;
    PGstSystemClock_listautoptr  = ^TGstSystemClock_listautoptr;
    PGstSystemClock_queueautoptr  = ^TGstSystemClock_queueautoptr;
    PGstSystemClock_slistautoptr  = ^TGstSystemClock_slistautoptr;
    PGstSystemClockPrivate  = ^TGstSystemClockPrivate;

    TGstSystemClockPrivate = record end;
    T_GstSystemClockPrivate = TGstSystemClockPrivate;

    TGstClockType = (
      GST_CLOCK_TYPE_REALTIME := 0,
      GST_CLOCK_TYPE_MONOTONIC := 1,
      GST_CLOCK_TYPE_OTHER := 2);
    T_GstSystemClock = record
        clock : TGstClock;
        priv : PGstSystemClockPrivate;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstSystemClock = T_GstSystemClock ;

    T_GstSystemClockClass = record
        parent_class : TGstClockClass;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstSystemClockClass = T_GstSystemClockClass ;

        TGstSystemClock_autoptr = PGstSystemClock;

        TGstSystemClock_listautoptr = PGList;

        TGstSystemClock_slistautoptr = PGSList;

        TGstSystemClock_queueautoptr = PGQueue;

  function gst_system_clock_get_type:TGType;cdecl;external gstreamerlib name 'gst_system_clock_get_type';
  function gst_system_clock_obtain:PGstClock;cdecl;external gstreamerlib name 'gst_system_clock_obtain';
  procedure gst_system_clock_set_default(new_clock:PGstClock);cdecl;external gstreamerlib name 'gst_system_clock_set_default';


