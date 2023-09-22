
{* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *
 * gst.h: Main header for GStreamer, apps should include this
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

{$IFNDEF FPC_DOTTEDUNITS}
unit gst;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}
{$h+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses Api.Glib2;
{$ELSE FPC_DOTTEDUNITS}
uses glib2;
{$ENDIF FPC_DOTTEDUNITS}

const
  gstreamerlib = 'libgstreamer-1.0'; {Setup as you need}

Const
  GST_PADDING	=	4;
  GST_OBJECT_FLAG_LAST = (1 shl 4);


{ Pointers to basic pascal types, inserted by h2pas conversion program.}
Type
  PLongint  = ^Longint;
  PSmallInt = ^SmallInt;
  PByte     = ^Byte;
  PWord     = ^Word;
  PDWord    = ^DWord;
  PDouble   = ^Double;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

// {$i glib-compat.h>

{$i gstaliases.inc}
{$i gstenum.inc}
{$i gstrec.inc}

{$i gstenumtypes.inc}
{$i gstversion.inc}
// needed in gstcaps
{$i gstminiobject.inc}
// needed in gstcaps.inc
{$i gstcapsfeatures.inc}
// needed in gststructure
{$i gstclock.inc}
// needed in gststructure
{$i gstdatetime.inc}
// needed in gstcaps.inc
{$i gststructure.inc}
// needed in gstvalue
{$i gstcaps.inc}
// needed in gstminiobject
{$i gstvalue.inc}
// needed in gstbuffer
{$i gstmeta.inc}
// Needed in format.inc
{$i gstiterator.inc}
// needed in gstbufferpool.inc
{$i gstformat.inc}
// needed in gstmessage.inc
{$i gstcontext.inc}
// needed in gstelement.inc
{$i gstbus.inc}
// needed in gstevent.inc
{$i gstsegment.inc}
// needed in gstsample
{$i gstbufferlist.inc}
// needed in gsttaglist.inc
{$i gstsample.inc}
// needed in gststreams.inc
{$i gsttaglist.inc}
// needed in gstevent.inc
{$i gststreams.inc}
// needed in gstevent.inc
{$i gststreamcollection.inc}
// needed in gstevent.inc
{$i gsttoc.inc}
// needed in gstpad.inc
{$i gstevent.inc}
// needed in gstelementfactory.inc
{$i gstplugin.inc}
// needed in gstelementfactory.inc
{$i gsturi.inc}
// needed in gstelementfactory.inc
{$i gstpluginfeature.inc}
// needed in gstelement.inc
{$i gstelementfactory.inc}
// needed in gstallocator.inc
{$i gstmemory.inc}
// needed in gstquery.inc
{$i gstallocator.inc}
// needed in gstquery.inc
{$i gstquery.inc}
// needed in gsttask.inc
{$i gsttaskpool.inc}
// needed in gstpad.inc
{$i gsttask.inc}
// needed in gstpad.inc
{$i gstpadtemplate.inc}
// needed in gstelement.inc
{$i gstpad.inc}
// needed in gstdevice.inc
{$i gstelement.inc}
// needed in gstmessage.inc
{$i gstdevice.inc}
// needed in gstevent
{$i gstmessage.inc}
// needed in gstutils
{$i gstbufferpool.inc}
{$i gstbuffer.inc}
// needed in gstutils.h
{$i gstbin.inc}
// needed in gstutils.h
{$i gstparse.inc}
// needed in gstprotection
{$i gstutils.inc}
// needed in gstobject
{$i gstprotection.inc}
// needed in gstobject.inc
{$i gstcontrolbinding.inc}
{$i gstobject.inc}

{$i gstatomicqueue.inc}
{$i gstchildproxy.inc}
{$i gstcontrolsource.inc}
{$i gstdebugutils.inc}
{$i gstdevicemonitor.inc}
{$i gstdeviceprovider.inc}
{$i gstdeviceproviderfactory.inc}
{$i gstdynamictypefactory.inc}
{$i gstelementmetadata.inc}
{$i gsterror.inc}
{$i gstghostpad.inc}
{$i gstinfo.inc}
{$i gstparamspecs.inc}
{$i gstpipeline.inc}
{$i gstpoll.inc}
{$i gstpreset.inc}
{$i gstregistry.inc}
{$i gstpromise.inc}
{$i gstsystemclock.inc}
{$i gsttagsetter.inc}
{$i gsttocsetter.inc}
{$i gsttracer.inc}
{$i gsttracerfactory.inc}
{$i gsttracerrecord.inc}
{$i gsttypefind.inc}

{$i gsttypefindfactory.inc}


procedure gst_init(argc:Plongint; argv:PPPAnsiChar);cdecl;external gstreamerlib name 'gst_init';
function gst_init_check(argc:Plongint; argv:PPPAnsiChar; err:PPGError):Tgboolean;cdecl;external gstreamerlib name 'gst_init_check';
function gst_is_initialized:Tgboolean;cdecl;external gstreamerlib name 'gst_is_initialized';
function gst_init_get_option_group:PGOptionGroup;cdecl;external gstreamerlib name 'gst_init_get_option_group';
procedure gst_deinit;cdecl;external gstreamerlib name 'gst_deinit';
procedure gst_version(major:Pguint; minor:Pguint; micro:Pguint; nano:Pguint);cdecl;external gstreamerlib name 'gst_version';
function gst_version_string:Pgchar;cdecl;external gstreamerlib name 'gst_version_string';
function gst_segtrap_is_enabled:Tgboolean;cdecl;external gstreamerlib name 'gst_segtrap_is_enabled';
procedure gst_segtrap_set_enabled(enabled:Tgboolean);cdecl;external gstreamerlib name 'gst_segtrap_set_enabled';
function gst_registry_fork_is_enabled:Tgboolean;cdecl;external gstreamerlib name 'gst_registry_fork_is_enabled';
procedure gst_registry_fork_set_enabled(enabled:Tgboolean);cdecl;external gstreamerlib name 'gst_registry_fork_set_enabled';
function gst_update_registry:Tgboolean;cdecl;external gstreamerlib name 'gst_update_registry';
function gst_get_main_executable_path:Pgchar;cdecl;external gstreamerlib name 'gst_get_main_executable_path';

implementation

{$i gstbin_impl.inc}
{$i gstmessage_impl.inc}

end. 

