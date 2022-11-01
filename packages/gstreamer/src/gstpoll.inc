{
/* GStreamer
 * Copyright (C) 1999 Erik Walthinsen <omega@cse.ogi.edu>
 * Copyright (C) 2004 Wim Taymans <wim.taymans@gmail.com>
 * Copyright (C) 2007 Peter Kjellerstedt <pkj@axis.com>
 *
 * gstpoll.h: File descriptor set
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
    PGstPoll  = ^TGstPoll;
    PGstPollFD  = ^TGstPollFD;

    TGstPoll = record end;
    T_GstPoll = TGstPoll;


    TGstPollFD = record
        fd : longint;
        idx : Tgint;
      end;

  function gst_poll_new(controllable:Tgboolean):PGstPoll;cdecl;external gstreamerlib name 'gst_poll_new';

  function gst_poll_new_timer:PGstPoll;cdecl;external gstreamerlib name 'gst_poll_new_timer';

  procedure gst_poll_free(aset:PGstPoll);cdecl;external gstreamerlib name 'gst_poll_free';

  procedure gst_poll_get_read_gpollfd(aset:PGstPoll; fd:PGPollFD);cdecl;external gstreamerlib name 'gst_poll_get_read_gpollfd';

  procedure gst_poll_fd_init(fd:PGstPollFD);cdecl;external gstreamerlib name 'gst_poll_fd_init';

  function gst_poll_add_fd(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_add_fd';

  function gst_poll_remove_fd(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_remove_fd';

  function gst_poll_fd_ctl_write(aset:PGstPoll; fd:PGstPollFD; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_ctl_write';

  function gst_poll_fd_ctl_read(aset:PGstPoll; fd:PGstPollFD; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_ctl_read';

  function gst_poll_fd_ctl_pri(aset:PGstPoll; fd:PGstPollFD; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_ctl_pri';

  procedure gst_poll_fd_ignored(aset:PGstPoll; fd:PGstPollFD);cdecl;external gstreamerlib name 'gst_poll_fd_ignored';

(* Const before type ignored *)
  function gst_poll_fd_has_closed(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_has_closed';

(* Const before type ignored *)
  function gst_poll_fd_has_error(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_has_error';

(* Const before type ignored *)
  function gst_poll_fd_can_read(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_can_read';

(* Const before type ignored *)
  function gst_poll_fd_can_write(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_can_write';

(* Const before type ignored *)
  function gst_poll_fd_has_pri(aset:PGstPoll; fd:PGstPollFD):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_fd_has_pri';

  function gst_poll_wait(aset:PGstPoll; timeout:TGstClockTime):Tgint;cdecl;external gstreamerlib name 'gst_poll_wait';

  function gst_poll_set_controllable(aset:PGstPoll; controllable:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_set_controllable';

  procedure gst_poll_restart(aset:PGstPoll);cdecl;external gstreamerlib name 'gst_poll_restart';

  procedure gst_poll_set_flushing(aset:PGstPoll; flushing:Tgboolean);cdecl;external gstreamerlib name 'gst_poll_set_flushing';

  function gst_poll_write_control(aset:PGstPoll):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_write_control';

  function gst_poll_read_control(aset:PGstPoll):Tgboolean;cdecl;external gstreamerlib name 'gst_poll_read_control';


