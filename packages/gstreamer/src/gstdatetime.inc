{* GStreamer
 * Copyright (C) 2010 Thiago Santos <thiago.sousa.santos@collabora.co.uk>
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
    PGDateTime  = ^TGDateTime;
    PGstDateTime  = ^TGstDateTime;
    PPGstDateTime = ^PGstDateTime;
    PGstDateTime_autoptr  = ^TGstDateTime_autoptr;
    PGstDateTime_listautoptr  = ^TGstDateTime_listautoptr;
    PGstDateTime_queueautoptr  = ^TGstDateTime_queueautoptr;
    PGstDateTime_slistautoptr  = ^TGstDateTime_slistautoptr;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}


  TGstDateTime_autoptr = PGstDateTime;

  TGstDateTime_listautoptr = PGList;

  TGstDateTime_slistautoptr = PGSList;

  TGstDateTime_queueautoptr = PGQueue;

  TGstDateTime = record end;
  T_GstDateTime = TGstDateTime;

    var
      _gst_date_time_type : TGType;cvar;external;

  function gst_date_time_get_type:TGType;cdecl;external gstreamerlib name 'gst_date_time_get_type';

(* Const before type ignored *)
  function gst_date_time_has_year(datetime:PGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_date_time_has_year';

(* Const before type ignored *)
  function gst_date_time_has_month(datetime:PGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_date_time_has_month';

(* Const before type ignored *)
  function gst_date_time_has_day(datetime:PGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_date_time_has_day';

(* Const before type ignored *)
  function gst_date_time_has_time(datetime:PGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_date_time_has_time';

(* Const before type ignored *)
  function gst_date_time_has_second(datetime:PGstDateTime):Tgboolean;cdecl;external gstreamerlib name 'gst_date_time_has_second';

(* Const before type ignored *)
  function gst_date_time_get_year(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_year';

(* Const before type ignored *)
  function gst_date_time_get_month(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_month';

(* Const before type ignored *)
  function gst_date_time_get_day(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_day';

(* Const before type ignored *)
  function gst_date_time_get_hour(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_hour';

(* Const before type ignored *)
  function gst_date_time_get_minute(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_minute';

(* Const before type ignored *)
  function gst_date_time_get_second(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_second';

(* Const before type ignored *)
  function gst_date_time_get_microsecond(datetime:PGstDateTime):Tgint;cdecl;external gstreamerlib name 'gst_date_time_get_microsecond';

(* Const before type ignored *)
  function gst_date_time_get_time_zone_offset(datetime:PGstDateTime):Tgfloat;cdecl;external gstreamerlib name 'gst_date_time_get_time_zone_offset';

  function gst_date_time_new_from_unix_epoch_local_time(secs:Tgint64):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_from_unix_epoch_local_time';

  function gst_date_time_new_from_unix_epoch_utc(secs:Tgint64):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_from_unix_epoch_utc';

  function gst_date_time_new_local_time(year:Tgint; month:Tgint; day:Tgint; hour:Tgint; minute:Tgint; 
             seconds:Tgdouble):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_local_time';

  function gst_date_time_new_y(year:Tgint):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_y';

  function gst_date_time_new_ym(year:Tgint; month:Tgint):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_ym';

  function gst_date_time_new_ymd(year:Tgint; month:Tgint; day:Tgint):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_ymd';

  function gst_date_time_new(tzoffset:Tgfloat; year:Tgint; month:Tgint; day:Tgint; hour:Tgint; 
             minute:Tgint; seconds:Tgdouble):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new';

  function gst_date_time_new_now_local_time:PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_now_local_time';

  function gst_date_time_new_now_utc:PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_now_utc';

  function gst_date_time_to_iso8601_string(datetime:PGstDateTime):Pgchar;cdecl;external gstreamerlib name 'gst_date_time_to_iso8601_string';

(* Const before type ignored *)
  function gst_date_time_new_from_iso8601_string(_string:Pgchar):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_from_iso8601_string';

  function gst_date_time_to_g_date_time(datetime:PGstDateTime):PGDateTime;cdecl;external gstreamerlib name 'gst_date_time_to_g_date_time';

  function gst_date_time_new_from_g_date_time(dt:PGDateTime):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_new_from_g_date_time';

  function gst_date_time_ref(datetime:PGstDateTime):PGstDateTime;cdecl;external gstreamerlib name 'gst_date_time_ref';

  procedure gst_date_time_unref(datetime:PGstDateTime);cdecl;external gstreamerlib name 'gst_date_time_unref';



