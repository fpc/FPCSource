{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wim.taymans@chello.be>
 *
 * gstpadtemplate.h: Header for GstPadTemplate object
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
  P_GstPadTemplate  = ^T_GstPadTemplate;
  P_GstPadTemplateClass  = ^T_GstPadTemplateClass;
  P_GstStaticPadTemplate  = ^T_GstStaticPadTemplate;
  PGstPadPresence  = ^TGstPadPresence;
  PGstPadTemplate  = ^TGstPadTemplate;
  PGstPadTemplate_autoptr  = ^TGstPadTemplate_autoptr;
  PGstPadTemplate_listautoptr  = ^TGstPadTemplate_listautoptr;
  PGstPadTemplate_queueautoptr  = ^TGstPadTemplate_queueautoptr;
  PGstPadTemplate_slistautoptr  = ^TGstPadTemplate_slistautoptr;
  PGstPadTemplateFlags  = ^TGstPadTemplateFlags;
  PGstStaticPadTemplate  = ^TGstStaticPadTemplate;



  TGstPadPresence = (
    GST_PAD_ALWAYS,
    GST_PAD_SOMETIMES,
    GST_PAD_REQUEST
  );

  TGstPadTemplateFlags = (
    GST_PAD_TEMPLATE_FLAG_LAST := GST_OBJECT_FLAG_LAST shl 4
  );

  PGstPad = ^TGstPad;

  TGstPadActivateFunction = function (pad:PGstPad; parent:PGstObject):Tgboolean;cdecl;
  TGstPadActivateModeFunction = function (pad:PGstPad; parent:PGstObject; mode:TGstPadMode; active:Tgboolean):Tgboolean;cdecl;
  TGstPadChainFunction = function (pad:PGstPad; parent:PGstObject; buffer:PGstBuffer):TGstFlowReturn;cdecl;
  TGstPadChainListFunction = function (pad:PGstPad; parent:PGstObject; list:PGstBufferList):TGstFlowReturn;cdecl;
  TGstPadGetRangeFunction = function (pad:PGstPad; parent:PGstObject; offset:Tguint64; length:Tguint; buffer:PPGstBuffer):TGstFlowReturn;cdecl;
  TGstPadEventFunction = function (pad:PGstPad; parent:PGstObject; event: PGstEvent):Tgboolean;cdecl;
  TGstPadEventFullFunction = function (pad:PGstPad; parent:PGstObject; event:PGstEvent):TGstFlowReturn;cdecl;

  TGstPadIterIntLinkFunction = function (pad:PGstPad; parent:PGstObject):PGstIterator;cdecl;

  TGstPadQueryFunction = function (pad:PGstPad; parent:PGstObject; query:PGstQuery):Tgboolean;cdecl;

  TGstPadLinkFunction = function (pad:PGstPad; parent:PGstObject; peer:PGstPad):TGstPadLinkReturn;cdecl;

  TGstPadUnlinkFunction = procedure (pad:PGstPad; parent:PGstObject);cdecl;

  TGstPadForwardFunction = function (pad:PGstPad; user_data:Tgpointer):Tgboolean;cdecl;


  T_GstPadPrivate = record end;
  TGstPadPrivate = T_GstPadPrivate;
  PGstPadPrivate = ^TGstPadPrivate;

  T_GstPad = record
      object_ : TGstObject;
      element_private : Tgpointer;
      padtemplate : PGstPadTemplate;
      direction : TGstPadDirection;
      stream_rec_lock : TGRecMutex;
      task : PGstTask;
      block_cond : TGCond;
      probes : TGHookList;
      mode : TGstPadMode;
      activatefunc : TGstPadActivateFunction;
      activatedata : Tgpointer;
      activatenotify : TGDestroyNotify;
      activatemodefunc : TGstPadActivateModeFunction;
      activatemodedata : Tgpointer;
      activatemodenotify : TGDestroyNotify;
      peer : PGstPad;
      linkfunc : TGstPadLinkFunction;
      linkdata : Tgpointer;
      linknotify : TGDestroyNotify;
      unlinkfunc : TGstPadUnlinkFunction;
      unlinkdata : Tgpointer;
      unlinknotify : TGDestroyNotify;
      chainfunc : TGstPadChainFunction;
      chaindata : Tgpointer;
      chainnotify : TGDestroyNotify;
      chainlistfunc : TGstPadChainListFunction;
      chainlistdata : Tgpointer;
      chainlistnotify : TGDestroyNotify;
      getrangefunc : TGstPadGetRangeFunction;
      getrangedata : Tgpointer;
      getrangenotify : TGDestroyNotify;
      eventfunc : TGstPadEventFunction;
      eventdata : Tgpointer;
      eventnotify : TGDestroyNotify;
      offset : Tgint64;
      queryfunc : TGstPadQueryFunction;
      querydata : Tgpointer;
      querynotify : TGDestroyNotify;
      iterintlinkfunc : TGstPadIterIntLinkFunction;
      iterintlinkdata : Tgpointer;
      iterintlinknotify : TGDestroyNotify;
      num_probes : Tgint;
      num_blocked : Tgint;
      priv : PGstPadPrivate;
      ABI : record
          case longint of
            0 : ( _gst_reserved : array[0..3] of Tgpointer );
            1 : ( abi : record
                last_flowret : TGstFlowReturn;
                eventfullfunc : TGstPadEventFullFunction;
              end );
          end;
    end;
  TGstPad = T_GstPad;

  T_GstPadTemplate = record
    object_ : TGstObject;
    name_template : ^gchar;
    direction : TGstPadDirection;
    presence : TGstPadPresence;
    caps : PGstCaps;
    ABI : record
        case longint of
          0 : ( _gst_reserved : array[0..(GST_PADDING)-1] of gpointer );
          1 : ( abi : record
              gtype : GType;
            end );
        end;
  end;
  TGstPadTemplate =T_GstPadTemplate;

  T_GstPadTemplateClass = record
      parent_class : TGstObjectClass;
      pad_created : procedure (templ:PGstPadTemplate; pad: PGstPad);cdecl;
      _gst_reserved : array[0..3] of Tgpointer;
    end;



  T_GstStaticPadTemplate = record
      name_template : Pgchar;
      direction : TGstPadDirection;
      presence : TGstPadPresence;
      static_caps : TGstStaticCaps;
    end;
  TGstStaticPadTemplate = T_GstStaticPadTemplate;

  TGstPadTemplate_autoptr = PGstPadTemplate;
  TGstPadTemplate_listautoptr = PGList;
  TGstPadTemplate_slistautoptr = PGSList;
  TGstPadTemplate_queueautoptr = PGQueue;

function gst_pad_template_get_type:TGType;cdecl;external gstreamerlib name 'gst_pad_template_get_type';

function gst_static_pad_template_get_type:TGType;cdecl;external gstreamerlib name 'gst_static_pad_template_get_type';

function gst_pad_template_new(name_template:Pgchar; direction:TGstPadDirection; presence:TGstPadPresence; caps:PGstCaps):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_pad_template_new';

function gst_pad_template_new_with_gtype(name_template:Pgchar; direction:TGstPadDirection; presence:TGstPadPresence; caps:PGstCaps; pad_type:TGType):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_pad_template_new_with_gtype';

function gst_static_pad_template_get(pad_template:PGstStaticPadTemplate):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_static_pad_template_get';

function gst_pad_template_new_from_static_pad_template_with_gtype(pad_template:PGstStaticPadTemplate; pad_type:TGType):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_pad_template_new_from_static_pad_template_with_gtype';

function gst_static_pad_template_get_caps(templ:PGstStaticPadTemplate):PGstCaps;cdecl;external gstreamerlib name 'gst_static_pad_template_get_caps';

function gst_pad_template_get_caps(templ:PGstPadTemplate):PGstCaps;cdecl;external gstreamerlib name 'gst_pad_template_get_caps';

procedure gst_pad_template_pad_created(templ:PGstPadTemplate; pad:PGstPad);cdecl;external gstreamerlib name 'gst_pad_template_pad_created';


