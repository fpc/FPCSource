{
/* GStreamer
 * Copyright (C) 1999,2000 Erik Walthinsen <omega@cse.ogi.edu>
 *                    2000 Wim Taymans <wtay@chello.be>
 *                    2002 Thomas Vander Stichele <thomas@apestaart.org>
 *
 * gstutils.h: Header for various utility functions
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
  PPGValueArray = ^PGValueArray;

{$IFDEF FPC}
{$PACKRECORDS C}
{$ENDIF}

type
  PGstSearchMode = ^TGstSearchMode;
  TGstSearchMode = (GST_SEARCH_MODE_EXACT := 0,GST_SEARCH_MODE_BEFORE,
    GST_SEARCH_MODE_AFTER);



(* Const before type ignored *)

  procedure gst_util_set_value_from_string(value:PGValue; value_str:Pgchar);cdecl;external gstreamerlib name 'gst_util_set_value_from_string';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_util_set_object_arg(object_:PGObject; name:Pgchar; value:Pgchar);cdecl;external gstreamerlib name 'gst_util_set_object_arg';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_util_set_object_array(object_:PGObject; name:Pgchar; array_:PGValueArray):Tgboolean;cdecl;external gstreamerlib name 'gst_util_set_object_array';

(* Const before type ignored *)
  function gst_util_get_object_array(object_:PGObject; name:Pgchar; array_:PPGValueArray):Tgboolean;cdecl;external gstreamerlib name 'gst_util_get_object_array';

(* Const before type ignored *)
  procedure gst_util_dump_mem(mem:Pguchar; size:Tguint);cdecl;external gstreamerlib name 'gst_util_dump_mem';

  procedure gst_util_dump_buffer(buf:PGstBuffer);cdecl;external gstreamerlib name 'gst_util_dump_buffer';

  function gst_util_gdouble_to_guint64(value:Tgdouble):Tguint64;cdecl;external gstreamerlib name 'gst_util_gdouble_to_guint64';

  function gst_util_guint64_to_gdouble(value:Tguint64):Tgdouble;cdecl;external gstreamerlib name 'gst_util_guint64_to_gdouble';

  function gst_util_uint64_scale(val:Tguint64; num:Tguint64; denom:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_util_uint64_scale';

  function gst_util_uint64_scale_round(val:Tguint64; num:Tguint64; denom:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_util_uint64_scale_round';

  function gst_util_uint64_scale_ceil(val:Tguint64; num:Tguint64; denom:Tguint64):Tguint64;cdecl;external gstreamerlib name 'gst_util_uint64_scale_ceil';

  function gst_util_uint64_scale_int(val:Tguint64; num:Tgint; denom:Tgint):Tguint64;cdecl;external gstreamerlib name 'gst_util_uint64_scale_int';

  function gst_util_uint64_scale_int_round(val:Tguint64; num:Tgint; denom:Tgint):Tguint64;cdecl;external gstreamerlib name 'gst_util_uint64_scale_int_round';

  function gst_util_uint64_scale_int_ceil(val:Tguint64; num:Tgint; denom:Tgint):Tguint64;cdecl;external gstreamerlib name 'gst_util_uint64_scale_int_ceil';

  function gst_util_seqnum_next:Tguint32;cdecl;external gstreamerlib name 'gst_util_seqnum_next';

  function gst_util_seqnum_compare(s1:Tguint32; s2:Tguint32):Tgint32;cdecl;external gstreamerlib name 'gst_util_seqnum_compare';

  function gst_util_group_id_next:Tguint;cdecl;external gstreamerlib name 'gst_util_group_id_next';

  {
  static inline guint16 __gst_fast_read16(const guint8 *v) 
    return *(const guint16*)(const void*)(v);
  
  static inline guint32 __gst_fast_read32(const guint8 *v) 
    return *(const guint32*)(const void*)(v);
  
  static inline guint64 __gst_fast_read64(const guint8 *v) 
    return *(const guint64*)(const void*)(v);
  
  static inline guint16 __gst_fast_read_swap16(const guint8 *v) 
    return (((guint16) ( (guint16) ((guint16) (*(const guint16*)(const void*)(v)) >> 8) | (guint16) ((guint16) (*(const guint16*)(const void*)(v)) << 8))));
  
  static inline guint32 __gst_fast_read_swap32(const guint8 *v) 
    return (((guint32) ( (((guint32) (*(const guint32*)(const void*)(v)) & (guint32) 0x000000ffU) << 24) | (((guint32) (*(const guint32*)(const void*)(v)) & (guint32) 0x0000ff00U) << 8) | (((guint32) (*(const guint32*)(const void*)(v)) & (guint32) 0x00ff0000U) >> 8) | (((guint32) (*(const guint32*)(const void*)(v)) & (guint32) 0xff000000U) >> 24))));
  
  static inline guint64 __gst_fast_read_swap64(const guint8 *v) 
    return (((guint64) ( (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x00000000000000ffUL)) << 56) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x000000000000ff00UL)) << 40) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x0000000000ff0000UL)) << 24) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x00000000ff000000UL)) << 8) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x000000ff00000000UL)) >> 8) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x0000ff0000000000UL)) >> 24) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0x00ff000000000000UL)) >> 40) | (((guint64) (*(const guint64*)(const void*)(v)) & (guint64) (0xff00000000000000UL)) >> 56))));
  
  
  
  
  static inline void __gst_fast_write16 (guint8 *p, guint16 v) 
    *(guint16*)(void*)(p) = v;
  
  static inline void __gst_fast_write32 (guint8 *p, guint32 v) 
    *(guint32*)(void*)(p) = v;
  
  static inline void __gst_fast_write64 (guint8 *p, guint64 v) 
    *(guint64*)(void*)(p) = v;
  
  static inline void __gst_fast_write_swap16 (guint8 *p, guint16 v) 
    *(guint16*)(void*)(p) = (((guint16) ( (guint16) ((guint16) (v) >> 8) | (guint16) ((guint16) (v) << 8))));
  
  static inline void __gst_fast_write_swap32 (guint8 *p, guint32 v) 
    *(guint32*)(void*)(p) = (((guint32) ( (((guint32) (v) & (guint32) 0x000000ffU) << 24) | (((guint32) (v) & (guint32) 0x0000ff00U) << 8) | (((guint32) (v) & (guint32) 0x00ff0000U) >> 8) | (((guint32) (v) & (guint32) 0xff000000U) >> 24))));
  
  static inline void __gst_fast_write_swap64 (guint8 *p, guint64 v) 
    *(guint64*)(void*)(p) = (((guint64) ( (((guint64) (v) & (guint64) (0x00000000000000ffUL)) << 56) | (((guint64) (v) & (guint64) (0x000000000000ff00UL)) << 40) | (((guint64) (v) & (guint64) (0x0000000000ff0000UL)) << 24) | (((guint64) (v) & (guint64) (0x00000000ff000000UL)) << 8) | (((guint64) (v) & (guint64) (0x000000ff00000000UL)) >> 8) | (((guint64) (v) & (guint64) (0x0000ff0000000000UL)) >> 24) | (((guint64) (v) & (guint64) (0x00ff000000000000UL)) >> 40) | (((guint64) (v) & (guint64) (0xff00000000000000UL)) >> 56))));
  
  # 321 "gstutils.h"
  static inline guint32 __gst_slow_read24_be (const guint8 * data) 
    return ((((guint32) (((const guint8 *) (data))[0])) << (16)) | (((guint32) (((const guint8 *) (data))[1])) << (8)) | (((guint32) (((const guint8 *) (data))[2])) << (0)));
  
  # 336 "gstutils.h"
  static inline guint32 __gst_slow_read24_le (const guint8 * data) 
    return ((((guint32) (((const guint8 *) (data))[2])) << (16)) | (((guint32) (((const guint8 *) (data))[1])) << (8)) | (((guint32) (((const guint8 *) (data))[0])) << (0)));
  
  # 568 "gstutils.h"
  static inline gfloat
  GFLOAT_SWAP_LE_BE(gfloat in)
  
    union
    
      guint32 i;
      gfloat f;
     u;
  
    u.f = in;
    u.i = (((guint32) ( (((guint32) (u.i) & (guint32) 0x000000ffU) << 24) | (((guint32) (u.i) & (guint32) 0x0000ff00U) << 8) | (((guint32) (u.i) & (guint32) 0x00ff0000U) >> 8) | (((guint32) (u.i) & (guint32) 0xff000000U) >> 24))));
    return u.f;
  
  # 590 "gstutils.h"
  static inline gdouble
  GDOUBLE_SWAP_LE_BE(gdouble in)
  
    union
    
      guint64 i;
      gdouble d;
     u;
  
    u.d = in;
    u.i = (((guint64) ( (((guint64) (u.i) & (guint64) (0x00000000000000ffUL)) << 56) | (((guint64) (u.i) & (guint64) (0x000000000000ff00UL)) << 40) | (((guint64) (u.i) & (guint64) (0x0000000000ff0000UL)) << 24) | (((guint64) (u.i) & (guint64) (0x00000000ff000000UL)) << 8) | (((guint64) (u.i) & (guint64) (0x000000ff00000000UL)) >> 8) | (((guint64) (u.i) & (guint64) (0x0000ff0000000000UL)) >> 24) | (((guint64) (u.i) & (guint64) (0x00ff000000000000UL)) >> 40) | (((guint64) (u.i) & (guint64) (0xff00000000000000UL)) >> 56))));
    return u.d;
  
  # 693 "gstutils.h"
  static inline gfloat
  GST_READ_FLOAT_LE(const guint8 *data)
  
    union
    
      guint32 i;
      gfloat f;
     u;
  
    u.i = __gst_fast_read32((const guint8 *)(data));
    return u.f;
  
  # 714 "gstutils.h"
  static inline gfloat
  GST_READ_FLOAT_BE(const guint8 *data)
  
    union
    
      guint32 i;
      gfloat f;
     u;
  
    u.i = __gst_fast_read_swap32((const guint8 *)(data));
    return u.f;
  
  # 735 "gstutils.h"
  static inline gdouble
  GST_READ_DOUBLE_LE(const guint8 *data)
  
    union
    
      guint64 i;
      gdouble d;
     u;
  
    u.i = __gst_fast_read64((const guint8 *)(data));
    return u.d;
  
  # 756 "gstutils.h"
  static inline gdouble
  GST_READ_DOUBLE_BE(const guint8 *data)
  
    union
    
      guint64 i;
      gdouble d;
     u;
  
    u.i = __gst_fast_read_swap64((const guint8 *)(data));
    return u.d;
  
  # 776 "gstutils.h"
  static inline void
  GST_WRITE_FLOAT_LE(guint8 *data, gfloat num)
  
    union
    
      guint32 i;
      gfloat f;
     u;
  
    u.f = num;
    __gst_fast_write32((guint8 *)(data), (u.i));
  
  # 796 "gstutils.h"
  static inline void
  GST_WRITE_FLOAT_BE(guint8 *data, gfloat num)
  
    union
    
      guint32 i;
      gfloat f;
     u;
  
    u.f = num;
    __gst_fast_write_swap32((guint8 *)(data), (u.i));
  
  # 816 "gstutils.h"
  static inline void
  GST_WRITE_DOUBLE_LE(guint8 *data, gdouble num)
  
    union
    
      guint64 i;
      gdouble d;
     u;
  
    u.d = num;
    __gst_fast_write64((guint8 *)(data), (u.i));
  
  # 836 "gstutils.h"
  static inline void
  GST_WRITE_DOUBLE_BE(guint8 *data, gdouble num)
  
    union
    
      guint64 i;
      gdouble d;
     u;
  
    u.d = num;
    __gst_fast_write_swap64((guint8 *)(data), (u.i));
  
   }
(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_object_default_error(source:PGstObject; error:PGError; debug:Pgchar);cdecl;external gstreamerlib name 'gst_object_default_error';

  procedure gst_element_create_all_pads(element:PGstElement);cdecl;external gstreamerlib name 'gst_element_create_all_pads';

  function gst_element_get_compatible_pad(element:PGstElement; pad:PGstPad; caps:PGstCaps):PGstPad;cdecl;external gstreamerlib name 'gst_element_get_compatible_pad';

  function gst_element_get_compatible_pad_template(element:PGstElement; compattempl:PGstPadTemplate):PGstPadTemplate;cdecl;external gstreamerlib name 'gst_element_get_compatible_pad_template';

(* Const before type ignored *)
  function gst_element_state_get_name(state:TGstState):Pgchar;cdecl;external gstreamerlib name 'gst_element_state_get_name';

(* Const before type ignored *)
  function gst_element_state_change_return_get_name(state_ret:TGstStateChangeReturn):Pgchar;cdecl;external gstreamerlib name 'gst_element_state_change_return_get_name';

(* Const before type ignored *)
  function gst_state_change_get_name(transition:TGstStateChange):Pgchar;cdecl;external gstreamerlib name 'gst_state_change_get_name';

  function gst_element_link(src:PGstElement; dest:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link';

  function gst_element_link_many(element_1:PGstElement; element_2:PGstElement; args:array of const):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link_many';

  function gst_element_link_many(element_1:PGstElement; element_2:PGstElement):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link_many';

  function gst_element_link_filtered(src:PGstElement; dest:PGstElement; filter:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link_filtered';

  procedure gst_element_unlink(src:PGstElement; dest:PGstElement);cdecl;external gstreamerlib name 'gst_element_unlink';

  procedure gst_element_unlink_many(element_1:PGstElement; element_2:PGstElement; args:array of const);cdecl;external gstreamerlib name 'gst_element_unlink_many';

  procedure gst_element_unlink_many(element_1:PGstElement; element_2:PGstElement);cdecl;external gstreamerlib name 'gst_element_unlink_many';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_element_link_pads(src:PGstElement; srcpadname:Pgchar; dest:PGstElement; destpadname:Pgchar):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link_pads';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_element_link_pads_full(src:PGstElement; srcpadname:Pgchar; dest:PGstElement; destpadname:Pgchar; flags:TGstPadLinkCheck):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link_pads_full';

(* Const before type ignored *)
(* Const before type ignored *)
  procedure gst_element_unlink_pads(src:PGstElement; srcpadname:Pgchar; dest:PGstElement; destpadname:Pgchar);cdecl;external gstreamerlib name 'gst_element_unlink_pads';

(* Const before type ignored *)
(* Const before type ignored *)
  function gst_element_link_pads_filtered(src:PGstElement; srcpadname:Pgchar; dest:PGstElement; destpadname:Pgchar; filter:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_element_link_pads_filtered';

  function gst_element_seek_simple(element:PGstElement; format:TGstFormat; seek_flags:TGstSeekFlags; seek_pos:Tgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_element_seek_simple';

(* Const before type ignored *)
  function gst_element_factory_can_sink_all_caps(factory:PGstElementFactory; caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_element_factory_can_sink_all_caps';

(* Const before type ignored *)
  function gst_element_factory_can_src_all_caps(factory:PGstElementFactory; caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_element_factory_can_src_all_caps';

(* Const before type ignored *)
  function gst_element_factory_can_sink_any_caps(factory:PGstElementFactory; caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_element_factory_can_sink_any_caps';

(* Const before type ignored *)
  function gst_element_factory_can_src_any_caps(factory:PGstElementFactory; caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_element_factory_can_src_any_caps';

  function gst_element_query_position(element:PGstElement; format:TGstFormat; cur:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_element_query_position';

  function gst_element_query_duration(element:PGstElement; format:TGstFormat; duration:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_element_query_duration';

  function gst_element_query_convert(element:PGstElement; src_format:TGstFormat; src_val:Tgint64; dest_format:TGstFormat; dest_val:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_element_query_convert';

  procedure gst_pad_use_fixed_caps(pad:PGstPad);cdecl;external gstreamerlib name 'gst_pad_use_fixed_caps';

  function gst_pad_get_parent_element(pad:PGstPad):PGstElement;cdecl;external gstreamerlib name 'gst_pad_get_parent_element';

  function gst_pad_proxy_query_accept_caps(pad:PGstPad; query:PGstQuery):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_proxy_query_accept_caps';

  function gst_pad_proxy_query_caps(pad:PGstPad; query:PGstQuery):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_proxy_query_caps';

  function gst_pad_query_position(pad:PGstPad; format:TGstFormat; cur:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_query_position';

  function gst_pad_query_duration(pad:PGstPad; format:TGstFormat; duration:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_query_duration';

  function gst_pad_query_convert(pad:PGstPad; src_format:TGstFormat; src_val:Tgint64; dest_format:TGstFormat; dest_val:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_query_convert';

  function gst_pad_query_caps(pad:PGstPad; filter:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_pad_query_caps';

  function gst_pad_query_accept_caps(pad:PGstPad; caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_query_accept_caps';

  function gst_pad_link_maybe_ghosting(src:PGstPad; sink:PGstPad):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_link_maybe_ghosting';

  function gst_pad_link_maybe_ghosting_full(src:PGstPad; sink:PGstPad; flags:TGstPadLinkCheck):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_link_maybe_ghosting_full';

  function gst_pad_peer_query_position(pad:PGstPad; format:TGstFormat; cur:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_peer_query_position';

  function gst_pad_peer_query_duration(pad:PGstPad; format:TGstFormat; duration:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_peer_query_duration';

  function gst_pad_peer_query_convert(pad:PGstPad; src_format:TGstFormat; src_val:Tgint64; dest_format:TGstFormat; dest_val:Pgint64):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_peer_query_convert';

  function gst_pad_peer_query_caps(pad:PGstPad; filter:PGstCaps):PGstCaps;cdecl;external gstreamerlib name 'gst_pad_peer_query_caps';

  function gst_pad_peer_query_accept_caps(pad:PGstPad; caps:PGstCaps):Tgboolean;cdecl;external gstreamerlib name 'gst_pad_peer_query_accept_caps';

(* Const before type ignored *)
  function gst_pad_create_stream_id(pad:PGstPad; parent:PGstElement; stream_id:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_pad_create_stream_id';

(* Const before type ignored *)
  function gst_pad_create_stream_id_printf(pad:PGstPad; parent:PGstElement; stream_id:Pgchar; args:array of const):Pgchar;cdecl;external gstreamerlib name 'gst_pad_create_stream_id_printf';

  function gst_pad_create_stream_id_printf(pad:PGstPad; parent:PGstElement; stream_id:Pgchar):Pgchar;cdecl;external gstreamerlib name 'gst_pad_create_stream_id_printf';

(* Const before type ignored *)
  function gst_pad_create_stream_id_printf_valist(pad:PGstPad; parent:PGstElement; stream_id:Pgchar; var_args:Tva_list):Pgchar;cdecl;external gstreamerlib name 'gst_pad_create_stream_id_printf_valist';

  function gst_pad_get_stream_id(pad:PGstPad):Pgchar;cdecl;external gstreamerlib name 'gst_pad_get_stream_id';

  function gst_pad_get_stream(pad:PGstPad):PGstStream;cdecl;external gstreamerlib name 'gst_pad_get_stream';

  procedure gst_bin_add_many(bin:PGstBin; element_1:PGstElement; args:array of const);cdecl;external gstreamerlib name 'gst_bin_add_many';

  procedure gst_bin_add_many(bin:PGstBin; element_1:PGstElement);cdecl;external gstreamerlib name 'gst_bin_add_many';

  procedure gst_bin_remove_many(bin:PGstBin; element_1:PGstElement; args:array of const);cdecl;external gstreamerlib name 'gst_bin_remove_many';

  procedure gst_bin_remove_many(bin:PGstBin; element_1:PGstElement);cdecl;external gstreamerlib name 'gst_bin_remove_many';

  function gst_bin_find_unlinked_pad(bin:PGstBin; direction:TGstPadDirection):PGstPad;cdecl;external gstreamerlib name 'gst_bin_find_unlinked_pad';

  function gst_bin_sync_children_states(bin:PGstBin):Tgboolean;cdecl;external gstreamerlib name 'gst_bin_sync_children_states';

(* Const before type ignored *)
  function gst_parse_bin_from_description(bin_description:Pgchar; ghost_unlinked_pads:Tgboolean; err:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_parse_bin_from_description';

(* Const before type ignored *)
  function gst_parse_bin_from_description_full(bin_description:Pgchar; ghost_unlinked_pads:Tgboolean; context:PGstParseContext; flags:TGstParseFlags; err:PPGError):PGstElement;cdecl;external gstreamerlib name 'gst_parse_bin_from_description_full';

  function gst_util_get_timestamp:TGstClockTime;cdecl;external gstreamerlib name 'gst_util_get_timestamp';


  function gst_util_array_binary_search(array_:Tgpointer; num_elements:Tguint; element_size:Tgsize; search_func:TGCompareDataFunc; mode:TGstSearchMode;
             search_data:Tgconstpointer; user_data:Tgpointer):Tgpointer;cdecl;external gstreamerlib name 'gst_util_array_binary_search';

  function gst_util_greatest_common_divisor(a:Tgint; b:Tgint):Tgint;cdecl;external gstreamerlib name 'gst_util_greatest_common_divisor';

  function gst_util_greatest_common_divisor_int64(a:Tgint64; b:Tgint64):Tgint64;cdecl;external gstreamerlib name 'gst_util_greatest_common_divisor_int64';

  procedure gst_util_fraction_to_double(src_n:Tgint; src_d:Tgint; dest:Pgdouble);cdecl;external gstreamerlib name 'gst_util_fraction_to_double';

  procedure gst_util_double_to_fraction(src:Tgdouble; dest_n:Pgint; dest_d:Pgint);cdecl;external gstreamerlib name 'gst_util_double_to_fraction';

  function gst_util_fraction_multiply(a_n:Tgint; a_d:Tgint; b_n:Tgint; b_d:Tgint; res_n:Pgint; 
             res_d:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_util_fraction_multiply';

  function gst_util_fraction_add(a_n:Tgint; a_d:Tgint; b_n:Tgint; b_d:Tgint; res_n:Pgint; 
             res_d:Pgint):Tgboolean;cdecl;external gstreamerlib name 'gst_util_fraction_add';

  function gst_util_fraction_compare(a_n:Tgint; a_d:Tgint; b_n:Tgint; b_d:Tgint):Tgint;cdecl;external gstreamerlib name 'gst_util_fraction_compare';

(* Const before type ignored *)
  function gst_calculate_linear_regression(xy:PGstClockTime; temp:PGstClockTime; n:Tguint; m_num:PGstClockTime; m_denom:PGstClockTime; 
             b:PGstClockTime; xbase:PGstClockTime; r_squared:Pgdouble):Tgboolean;cdecl;external gstreamerlib name 'gst_calculate_linear_regression';


