{
/* GStreamer
 * (c) 2010, 2012 Alexander Saprykin <xelfium@gmail.com>
 *
 * gsttoc.h: generic TOC API declaration
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
    Pgint64  = ^Tgint64;
    PGstToc  = ^TGstToc;
    PPGstToc = ^PGstToc;
    PGstToc_autoptr  = ^TGstToc_autoptr;
    PGstToc_listautoptr  = ^TGstToc_listautoptr;
    PGstToc_queueautoptr  = ^TGstToc_queueautoptr;
    PGstToc_slistautoptr  = ^TGstToc_slistautoptr;
    PGstTocEntry  = ^TGstTocEntry;
    PGstTocEntry_autoptr  = ^TGstTocEntry_autoptr;
    PGstTocEntry_listautoptr  = ^TGstTocEntry_listautoptr;
    PGstTocEntry_queueautoptr  = ^TGstTocEntry_queueautoptr;
    PGstTocEntry_slistautoptr  = ^TGstTocEntry_slistautoptr;
    PGstTocEntryType  = ^TGstTocEntryType;
    PGstTocLoopType  = ^TGstTocLoopType;
    PGstTocScope  = ^TGstTocScope;
{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}



    TGstTocEntry = record end;

    T_GstTocEntry = TGstTocEntry;

    TGstToc = record end;
    T_GstToc = TGstToc;

    TGstTocScope = (GST_TOC_SCOPE_GLOBAL := 1,GST_TOC_SCOPE_CURRENT := 2
      );

    TGstTocEntryType = (GST_TOC_ENTRY_TYPE_ANGLE := -(3),GST_TOC_ENTRY_TYPE_VERSION := -(2),
      GST_TOC_ENTRY_TYPE_EDITION := -(1),GST_TOC_ENTRY_TYPE_INVALID := 0,
      GST_TOC_ENTRY_TYPE_TITLE := 1,GST_TOC_ENTRY_TYPE_TRACK := 2,
      GST_TOC_ENTRY_TYPE_CHAPTER := 3);

    TGstTocLoopType = (GST_TOC_LOOP_NONE := 0,GST_TOC_LOOP_FORWARD,
      GST_TOC_LOOP_REVERSE,GST_TOC_LOOP_PING_PONG
      );


  TGstToc_autoptr = PGstToc;

  TGstToc_listautoptr = PGList;

  TGstToc_slistautoptr = PGSList;

  TGstToc_queueautoptr = PGQueue;

  TGstTocEntry_autoptr = PGstTocEntry;

  TGstTocEntry_listautoptr = PGList;

  TGstTocEntry_slistautoptr = PGSList;

  TGstTocEntry_queueautoptr = PGQueue;


  function gst_toc_get_type:TGType;cdecl;external gstreamerlib name 'gst_toc_get_type';

  function gst_toc_entry_get_type:TGType;cdecl;external gstreamerlib name 'gst_toc_entry_get_type';

  function gst_toc_new(scope:TGstTocScope):PGstToc;cdecl;external gstreamerlib name 'gst_toc_new';

(* Const before type ignored *)
  function gst_toc_get_scope(toc:PGstToc):TGstTocScope;cdecl;external gstreamerlib name 'gst_toc_get_scope';

  procedure gst_toc_set_tags(toc:PGstToc; tags:PGstTagList);cdecl;external gstreamerlib name 'gst_toc_set_tags';

  procedure gst_toc_merge_tags(toc:PGstToc; tags:PGstTagList; mode:TGstTagMergeMode);cdecl;external gstreamerlib name 'gst_toc_merge_tags';

(* Const before type ignored *)
  function gst_toc_get_tags(toc:PGstToc):PGstTagList;cdecl;external gstreamerlib name 'gst_toc_get_tags';

  procedure gst_toc_append_entry(toc:PGstToc; entry:PGstTocEntry);cdecl;external gstreamerlib name 'gst_toc_append_entry';

(* Const before type ignored *)
  function gst_toc_get_entries(toc:PGstToc):PGList;cdecl;external gstreamerlib name 'gst_toc_get_entries';

  procedure gst_toc_dump(toc:PGstToc);cdecl;external gstreamerlib name 'gst_toc_dump';

(* Const before type ignored *)
  function gst_toc_entry_new(_type:TGstTocEntryType; uid:Pgchar):PGstTocEntry;cdecl;external gstreamerlib name 'gst_toc_entry_new';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_toc_find_entry(toc:PGstToc; uid:Pgchar):PGstTocEntry;cdecl;external gstreamerlib name 'gst_toc_find_entry';

(* Const before type ignored *)
  function gst_toc_entry_get_entry_type(entry:PGstTocEntry):TGstTocEntryType;cdecl;external gstreamerlib name 'gst_toc_entry_get_entry_type';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_toc_entry_get_uid(entry:PGstTocEntry):Pgchar;cdecl;external gstreamerlib name 'gst_toc_entry_get_uid';

  procedure gst_toc_entry_append_sub_entry(entry:PGstTocEntry; subentry:PGstTocEntry);cdecl;external gstreamerlib name 'gst_toc_entry_append_sub_entry';

(* Const before type ignored *)
  function gst_toc_entry_get_sub_entries(entry:PGstTocEntry):PGList;cdecl;external gstreamerlib name 'gst_toc_entry_get_sub_entries';

  procedure gst_toc_entry_set_tags(entry:PGstTocEntry; tags:PGstTagList);cdecl;external gstreamerlib name 'gst_toc_entry_set_tags';

  procedure gst_toc_entry_merge_tags(entry:PGstTocEntry; tags:PGstTagList; mode:TGstTagMergeMode);cdecl;external gstreamerlib name 'gst_toc_entry_merge_tags';

(* Const before type ignored *)
  function gst_toc_entry_get_tags(entry:PGstTocEntry):PGstTagList;cdecl;external gstreamerlib name 'gst_toc_entry_get_tags';

(* Const before type ignored *)
  function gst_toc_entry_is_alternative(entry:PGstTocEntry):Tgboolean;cdecl;external gstreamerlib name 'gst_toc_entry_is_alternative';

(* Const before type ignored *)
  function gst_toc_entry_is_sequence(entry:PGstTocEntry):Tgboolean;cdecl;external gstreamerlib name 'gst_toc_entry_is_sequence';

  procedure gst_toc_entry_set_start_stop_times(entry:PGstTocEntry; start:Tgint64; stop:Tgint64);cdecl;external gstreamerlib name 'gst_toc_entry_set_start_stop_times';

(* Const before type ignored *)
  function gst_toc_entry_get_start_stop_times(entry:PGstTocEntry; start:Pgint64; stop:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_toc_entry_get_start_stop_times';

  procedure gst_toc_entry_set_loop(entry:PGstTocEntry; loop_type:TGstTocLoopType; repeat_count:Tgint);cdecl;external gstreamerlib name 'gst_toc_entry_set_loop';

(* Const before type ignored *)
  function gst_toc_entry_get_loop(entry:PGstTocEntry; loop_type:PGstTocLoopType; repeat_count:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_toc_entry_get_loop';

  function gst_toc_entry_get_toc(entry:PGstTocEntry):PGstToc;cdecl;external gstreamerlib name 'gst_toc_entry_get_toc';

  function gst_toc_entry_get_parent(entry:PGstTocEntry):PGstTocEntry;cdecl;external gstreamerlib name 'gst_toc_entry_get_parent';

(* Const before type ignored *)
  function gst_toc_entry_type_get_nick(_type:TGstTocEntryType):Pgchar;cdecl;external gstreamerlib name 'gst_toc_entry_type_get_nick';

  {
  static inline void
  _gst_autoptr_toc_unref (GstToc *toc)
  
    gst_mini_object_unref(((GstMiniObject*)(toc)));
  
  
  static inline void
  _gst_autoptr_toc_entry_unref (GstTocEntry *entry)
  
    gst_mini_object_unref(((GstMiniObject*)(entry)));
  
   }


var
  _gst_toc_type : TGType;cvar;external;
  _gst_toc_entry_type : TGType;cvar;external;

