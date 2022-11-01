{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *                    2005 Andy Wingo <wingo@pobox.com>
 *
 * gstghostpad.h: Proxy pads
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
    P_GstGhostPad  = ^T_GstGhostPad;
    P_GstGhostPadClass  = ^T_GstGhostPadClass;
    P_GstProxyPad  = ^T_GstProxyPad;
    P_GstProxyPadClass  = ^T_GstProxyPadClass;
    PGstGhostPad  = ^TGstGhostPad;
    PGstGhostPad_autoptr  = ^TGstGhostPad_autoptr;
    PGstGhostPad_listautoptr  = ^TGstGhostPad_listautoptr;
    PGstGhostPad_queueautoptr  = ^TGstGhostPad_queueautoptr;
    PGstGhostPad_slistautoptr  = ^TGstGhostPad_slistautoptr;
    PGstGhostPadPrivate  = ^TGstGhostPadPrivate;
    PGstProxyPad  = ^TGstProxyPad;
    PGstProxyPad_autoptr  = ^TGstProxyPad_autoptr;
    PGstProxyPad_listautoptr  = ^TGstProxyPad_listautoptr;
    PGstProxyPad_queueautoptr  = ^TGstProxyPad_queueautoptr;
    PGstProxyPad_slistautoptr  = ^TGstProxyPad_slistautoptr;
    PGstProxyPadPrivate  = ^TGstProxyPadPrivate;

    TGstProxyPadPrivate = record end;

    T_GstProxyPadPrivate = TGstProxyPadPrivate;

    T_GstProxyPad = record
        pad : TGstPad;
        priv : PGstProxyPadPrivate;
      end;
    TGstProxyPad = T_GstProxyPad;

    T_GstProxyPadClass = record
        parent_class : TGstPadClass;
        _gst_reserved : array[0..0] of Tgpointer;
      end;
    TGstProxyPadClass = T_GstProxyPadClass;


    TGstGhostPadPrivate = record end;
    T_GstGhostPadPrivate = TGstGhostPadPrivate;


    T_GstGhostPad = record
        pad : TGstProxyPad;
        priv : PGstGhostPadPrivate;
      end;
    TGstGhostPad = T_GstGhostPad;


    T_GstGhostPadClass = record
        parent_class : TGstProxyPadClass;
        _gst_reserved : array[0..3] of Tgpointer;
      end;
    TGstGhostPadClass = T_GstGhostPadClass;

    TGstGhostPad_autoptr = PGstGhostPad;

    TGstGhostPad_listautoptr = PGList;

    TGstGhostPad_slistautoptr = PGSList;

    TGstGhostPad_queueautoptr = PGQueue;

    TGstProxyPad_autoptr = PGstProxyPad;

    TGstProxyPad_listautoptr = PGList;

    TGstProxyPad_slistautoptr = PGSList;

    TGstProxyPad_queueautoptr = PGQueue;


    function gst_proxy_pad_get_type:TGType;cdecl;external gstreamerlib name 'gst_proxy_pad_get_type';

    function gst_proxy_pad_get_internal(pad:PGstProxyPad):PGstProxyPad;cdecl;external gstreamerlib name 'gst_proxy_pad_get_internal';

    function gst_proxy_pad_iterate_internal_links_default(pad:PGstPad; parent:PGstObject):PGstIterator;cdecl;external gstreamerlib name 'gst_proxy_pad_iterate_internal_links_default';

    function gst_proxy_pad_chain_default(pad:PGstPad; parent:PGstObject; buffer:PGstBuffer):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_proxy_pad_chain_default';

    function gst_proxy_pad_chain_list_default(pad:PGstPad; parent:PGstObject; list:PGstBufferList):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_proxy_pad_chain_list_default';

    function gst_proxy_pad_getrange_default(pad:PGstPad; parent:PGstObject; offset:Tguint64; size:Tguint; buffer:PPGstBuffer):TGstFlowReturn;cdecl;external gstreamerlib name 'gst_proxy_pad_getrange_default';


  function gst_ghost_pad_get_type:TGType;cdecl;external gstreamerlib name 'gst_ghost_pad_get_type';

(* Const before type ignored *)
  function gst_ghost_pad_new(name:Pgchar; target:PGstPad):PGstPad;cdecl;external gstreamerlib name 'gst_ghost_pad_new';

(* Const before type ignored *)
  function gst_ghost_pad_new_no_target(name:Pgchar; dir:TGstPadDirection):PGstPad;cdecl;external gstreamerlib name 'gst_ghost_pad_new_no_target';

(* Const before type ignored *)
  function gst_ghost_pad_new_from_template(name:Pgchar; target:PGstPad; templ:PGstPadTemplate):PGstPad;cdecl;external gstreamerlib name 'gst_ghost_pad_new_from_template';

(* Const before type ignored *)
  function gst_ghost_pad_new_no_target_from_template(name:Pgchar; templ:PGstPadTemplate):PGstPad;cdecl;external gstreamerlib name 'gst_ghost_pad_new_no_target_from_template';

  function gst_ghost_pad_get_target(gpad:PGstGhostPad):PGstPad;cdecl;external gstreamerlib name 'gst_ghost_pad_get_target';

  function gst_ghost_pad_set_target(gpad:PGstGhostPad; newtarget:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_ghost_pad_set_target';

  function gst_ghost_pad_construct(gpad:PGstGhostPad):Tgboolean;cdecl;external gstreamerlib name 'gst_ghost_pad_construct';

  function gst_ghost_pad_activate_mode_default(pad:PGstPad; parent:PGstObject; mode:TGstPadMode; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_ghost_pad_activate_mode_default';

  function gst_ghost_pad_internal_activate_mode_default(pad:PGstPad; parent:PGstObject; mode:TGstPadMode; active:Tgboolean):Tgboolean;cdecl;external gstreamerlib name 'gst_ghost_pad_internal_activate_mode_default';



