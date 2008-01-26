(* cairo - a vector graphics library with display and print output
 *
 * Copyright © 2002 University of Southern California
 * Copyright © 2005 Red Hat, Inc.
 *
 * This library is free software; you can redistribute it and/or
 * modify it either under the terms of the GNU Lesser General Public
 * License version 2.1 as published by the Free Software Foundation
 * (the "LGPL") or, at your option, under the terms of the Mozilla
 * Public License Version 1.1 (the "MPL"). If you do not alter this
 * notice, a recipient may use your version of this file under either
 * the MPL or the LGPL.
 *
 * You should have received a copy of the LGPL along with this library
 * in the file COPYING-LGPL-2.1; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * You should have received a copy of the MPL along with this library
 * in the file COPYING-MPL-1.1
 *
 * The contents of this file are subject to the Mozilla Public License
 * Version 1.1 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY
 * OF ANY KIND, either express or implied. See the LGPL or the MPL for
 * the specific language governing rights and limitations.
 *
 * The Original Code is the cairo graphics library.
 *
 * The Initial Developer of the Original Code is University of Southern
 * California.
 *
 * Contributor(s):
 *	Carl D. Worth <cworth@cworth.org>
 *)



(*  
 *  This FreePascal binding generated August 26, 2005 
 *  by Jeffrey Pohlmeyer <yetanothergeek@yahoo.com>
 *)



unit cairo;

interface
uses x, xlib, xrender, freetypeh;

const
  LIB_CAIRO='cairo';

  CAIRO_HAS_FT_FONT = 1;     
  CAIRO_HAS_PNG_FUNCTIONS = 1;     
  CAIRO_HAS_XLIB_SURFACE = 1;     

  CAIRO_VERSION_MAJOR = 1;     
  CAIRO_VERSION_MICRO = 0;     
  CAIRO_VERSION_MINOR = 0;     
  CAIRO_VERSION_STRING = '1.0.0';     

{$IFDEF FPC}
  {$PACKRECORDS C}
{$ENDIF}

type
  cairo_status_t = (
    CAIRO_STATUS_SUCCESS = 0,
    CAIRO_STATUS_NO_MEMORY,
    CAIRO_STATUS_INVALID_RESTORE,
    CAIRO_STATUS_INVALID_POP_GROUP,
    CAIRO_STATUS_NO_CURRENT_POINT,
    CAIRO_STATUS_INVALID_MATRIX,
    CAIRO_STATUS_INVALID_STATUS,
    CAIRO_STATUS_NULL_POINTER,
    CAIRO_STATUS_INVALID_STRING,
    CAIRO_STATUS_INVALID_PATH_DATA,
    CAIRO_STATUS_READ_ERROR,
    CAIRO_STATUS_WRITE_ERROR,
    CAIRO_STATUS_SURFACE_FINISHED,
    CAIRO_STATUS_SURFACE_TYPE_MISMATCH,
    CAIRO_STATUS_PATTERN_TYPE_MISMATCH,
    CAIRO_STATUS_INVALID_CONTENT,
    CAIRO_STATUS_INVALID_FORMAT,
    CAIRO_STATUS_INVALID_VISUAL,
    CAIRO_STATUS_FILE_NOT_FOUND,
    CAIRO_STATUS_INVALID_DASH
  );

  cairo_operator_t = (
    CAIRO_OPERATOR_CLEAR,
    CAIRO_OPERATOR_SOURCE,
    CAIRO_OPERATOR_OVER,
    CAIRO_OPERATOR_IN,
    CAIRO_OPERATOR_OUT,
    CAIRO_OPERATOR_ATOP,
    CAIRO_OPERATOR_DEST,
    CAIRO_OPERATOR_DEST_OVER,
    CAIRO_OPERATOR_DEST_IN,
    CAIRO_OPERATOR_DEST_OUT,
    CAIRO_OPERATOR_DEST_ATOP,
    CAIRO_OPERATOR_XOR,
    CAIRO_OPERATOR_ADD,
    CAIRO_OPERATOR_SATURATE
  );

  cairo_antialias_t = (
    CAIRO_ANTIALIAS_DEFAULT,
    CAIRO_ANTIALIAS_NONE,
    CAIRO_ANTIALIAS_GRAY,
    CAIRO_ANTIALIAS_SUBPIXEL
  );

  cairo_fill_rule_t = (
    CAIRO_FILL_RULE_WINDING,
    CAIRO_FILL_RULE_EVEN_ODD
  );

  cairo_line_cap_t = (
    CAIRO_LINE_CAP_BUTT,
    CAIRO_LINE_CAP_ROUND,
    CAIRO_LINE_CAP_SQUARE
  );

  cairo_line_join_t = (
    CAIRO_LINE_JOIN_MITER,
    CAIRO_LINE_JOIN_ROUND,
    CAIRO_LINE_JOIN_BEVEL
  );

  cairo_font_slant_t = (
    CAIRO_FONT_SLANT_NORMAL,
    CAIRO_FONT_SLANT_ITALIC,
    CAIRO_FONT_SLANT_OBLIQUE
  );

  cairo_font_weight_t = (
    CAIRO_FONT_WEIGHT_NORMAL,
    CAIRO_FONT_WEIGHT_BOLD
  );

  cairo_subpixel_order_t = (
    CAIRO_SUBPIXEL_ORDER_DEFAULT,
    CAIRO_SUBPIXEL_ORDER_RGB,
    CAIRO_SUBPIXEL_ORDER_BGR,
    CAIRO_SUBPIXEL_ORDER_VRGB,
    CAIRO_SUBPIXEL_ORDER_VBGR
  );

  cairo_hint_style_t = (
    CAIRO_HINT_STYLE_DEFAULT,
    CAIRO_HINT_STYLE_NONE,
    CAIRO_HINT_STYLE_SLIGHT,
    CAIRO_HINT_STYLE_MEDIUM,
    CAIRO_HINT_STYLE_FULL
  );

  cairo_hint_metrics_t = (
    CAIRO_HINT_METRICS_DEFAULT,
    CAIRO_HINT_METRICS_OFF,
    CAIRO_HINT_METRICS_ON
  );

  cairo_path_data_type_t = (
    CAIRO_PATH_MOVE_TO,
    CAIRO_PATH_LINE_TO,
    CAIRO_PATH_CURVE_TO,
    CAIRO_PATH_CLOSE_PATH
  );

  cairo_content_t = (
    CAIRO_CONTENT_COLOR = $1000,
    CAIRO_CONTENT_ALPHA = $2000,
    CAIRO_CONTENT_COLOR_ALPHA = $3000
  );

  cairo_format_t = (
    CAIRO_FORMAT_ARGB32,
    CAIRO_FORMAT_RGB24,
    CAIRO_FORMAT_A8,
    CAIRO_FORMAT_A1
  );

  cairo_extend_t = (
    CAIRO_EXTEND_NONE,
    CAIRO_EXTEND_REPEAT,
    CAIRO_EXTEND_REFLECT
  );


  cairo_filter_t = (
    CAIRO_FILTER_FAST,
    CAIRO_FILTER_GOOD,
    CAIRO_FILTER_BEST,
    CAIRO_FILTER_NEAREST,
    CAIRO_FILTER_BILINEAR,
    CAIRO_FILTER_GAUSSIAN
  );



  FcPattern=pointer;

  PFcPattern = ^FcPattern;
  Pcairo_surface_t = ^cairo_surface_t;
  Pcairo_t = ^cairo_t;
  Pcairo_pattern_t = ^cairo_pattern_t;
  Pcairo_font_options_t = ^cairo_font_options_t;
  Pcairo_font_face_t = ^cairo_font_face_t;
  Pcairo_scaled_font_t = ^cairo_scaled_font_t;
  Pcairo_bool_t = ^cairo_bool_t;
  cairo_bool_t = longint;
  Pcairo_matrix_t = ^cairo_matrix_t;
  Pcairo_user_data_key_t = ^cairo_user_data_key_t;
  Pcairo_glyph_t = ^cairo_glyph_t;
  Pcairo_text_extents_t = ^cairo_text_extents_t;
  Pcairo_font_extents_t = ^cairo_font_extents_t;
  Pcairo_path_data_type_t = ^cairo_path_data_type_t;
  Pcairo_path_data_t = ^cairo_path_data_t;
  Pcairo_path_t = ^cairo_path_t;

  cairo_destroy_func_t = procedure (data:pointer); cdecl;
  cairo_write_func_t = function (closure:pointer; data:Pbyte; length:dword):cairo_status_t; cdecl;
  cairo_read_func_t = function (closure:pointer; data:Pbyte; length:dword):cairo_status_t; cdecl;

  cairo_t              = record {OPAQUE} end;
  cairo_surface_t      = record {OPAQUE} end;
  cairo_pattern_t      = record {OPAQUE} end;
  cairo_scaled_font_t  = record {OPAQUE} end;
  cairo_font_face_t    = record {OPAQUE} end;
  cairo_font_options_t = record {OPAQUE} end;


  cairo_matrix_t = record
    xx : double;
    yx : double;
    xy : double;
    yy : double;
    x0 : double;
    y0 : double;
  end;

  cairo_user_data_key_t = record
    unused : longint;
  end;

  cairo_glyph_t = record
    index : dword;
    x : double;
    y : double;
  end;

  cairo_text_extents_t = record
    x_bearing : double;
    y_bearing : double;
    width : double;
    height : double;
    x_advance : double;
    y_advance : double;
  end;

  cairo_font_extents_t = record
    ascent : double;
    descent : double;
    height : double;
    max_x_advance : double;
    max_y_advance : double;
  end;


  cairo_path_data_t = record
    case longint of
      0 : ( header : record
            _type : cairo_path_data_type_t;
            length : longint;
          end );
      1 : ( point : record
            x : double;
            y : double;
          end );
  end;

  cairo_path_t = record
    status : cairo_status_t;
    data : Pcairo_path_data_t;
    num_data : longint;
  end;


function  cairo_create(target:Pcairo_surface_t):Pcairo_t; cdecl; external LIB_CAIRO;
function  cairo_reference(cr:Pcairo_t):Pcairo_t; cdecl; external LIB_CAIRO;
procedure cairo_destroy(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_save(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_restore(cr:Pcairo_t); cdecl; external LIB_CAIRO;

procedure cairo_set_operator(cr:Pcairo_t; op:cairo_operator_t); cdecl; external LIB_CAIRO;
procedure cairo_set_source(cr:Pcairo_t; source:Pcairo_pattern_t); cdecl; external LIB_CAIRO;
procedure cairo_set_source_rgb(cr:Pcairo_t; red:double; green:double; blue:double); cdecl; external LIB_CAIRO;
procedure cairo_set_source_rgba(cr:Pcairo_t; red:double; green:double; blue:double; alpha:double); cdecl; external LIB_CAIRO;
procedure cairo_set_source_surface(cr:Pcairo_t; surface:Pcairo_surface_t; x:double; y:double); cdecl; external LIB_CAIRO;
procedure cairo_set_tolerance(cr:Pcairo_t; tolerance:double); cdecl; external LIB_CAIRO;
procedure cairo_set_antialias(cr:Pcairo_t; antialias:cairo_antialias_t); cdecl; external LIB_CAIRO;
procedure cairo_set_fill_rule(cr:Pcairo_t; fill_rule:cairo_fill_rule_t); cdecl; external LIB_CAIRO;
procedure cairo_set_line_width(cr:Pcairo_t; width:double); cdecl; external LIB_CAIRO;
procedure cairo_set_line_cap(cr:Pcairo_t; line_cap:cairo_line_cap_t); cdecl; external LIB_CAIRO;
procedure cairo_set_line_join(cr:Pcairo_t; line_join:cairo_line_join_t); cdecl; external LIB_CAIRO;
procedure cairo_set_dash(cr:Pcairo_t; dashes:Pdouble; num_dashes:longint; offset:double); cdecl; external LIB_CAIRO;
procedure cairo_set_miter_limit(cr:Pcairo_t; limit:double); cdecl; external LIB_CAIRO;

procedure cairo_translate(cr:Pcairo_t; tx:double; ty:double); cdecl; external LIB_CAIRO;
procedure cairo_scale(cr:Pcairo_t; sx:double; sy:double); cdecl; external LIB_CAIRO;
procedure cairo_rotate(cr:Pcairo_t; angle:double); cdecl; external LIB_CAIRO;
procedure cairo_transform(cr:Pcairo_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;

procedure cairo_set_matrix(cr:Pcairo_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_identity_matrix(cr:Pcairo_t); cdecl; external LIB_CAIRO;

procedure cairo_user_to_device(cr:Pcairo_t; x:Pdouble; y:Pdouble); cdecl; external LIB_CAIRO;
procedure cairo_user_to_device_distance(cr:Pcairo_t; dx:Pdouble; dy:Pdouble); cdecl; external LIB_CAIRO;
procedure cairo_device_to_user(cr:Pcairo_t; x:Pdouble; y:Pdouble); cdecl; external LIB_CAIRO;
procedure cairo_device_to_user_distance(cr:Pcairo_t; dx:Pdouble; dy:Pdouble); cdecl; external LIB_CAIRO;

procedure cairo_new_path(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_move_to(cr:Pcairo_t; x:double; y:double); cdecl; external LIB_CAIRO;
procedure cairo_line_to(cr:Pcairo_t; x:double; y:double); cdecl; external LIB_CAIRO;
procedure cairo_curve_to(cr:Pcairo_t; x1:double; y1:double; x2:double; y2:double; x3:double; y3:double); cdecl; external LIB_CAIRO;
procedure cairo_arc(cr:Pcairo_t; xc:double; yc:double; radius:double; angle1:double; angle2:double); cdecl; external LIB_CAIRO;
procedure cairo_arc_negative(cr:Pcairo_t; xc:double; yc:double; radius:double; angle1:double; angle2:double); cdecl; external LIB_CAIRO;
procedure cairo_rel_move_to(cr:Pcairo_t; dx:double; dy:double); cdecl; external LIB_CAIRO;
procedure cairo_rel_line_to(cr:Pcairo_t; dx:double; dy:double); cdecl; external LIB_CAIRO;
procedure cairo_rel_curve_to(cr:Pcairo_t; dx1:double; dy1:double; dx2:double; dy2:double; dx3:double; dy3:double); cdecl; external LIB_CAIRO;
procedure cairo_rectangle(cr:Pcairo_t; x:double; y:double; width:double; height:double); cdecl; external LIB_CAIRO;
procedure cairo_close_path(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_paint(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_paint_with_alpha(cr:Pcairo_t; alpha:double); cdecl; external LIB_CAIRO;

procedure cairo_mask(cr:Pcairo_t; pattern:Pcairo_pattern_t); cdecl; external LIB_CAIRO;
procedure cairo_mask_surface(cr:Pcairo_t; surface:Pcairo_surface_t; surface_x:double; surface_y:double); cdecl; external LIB_CAIRO;

procedure cairo_stroke(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_stroke_preserve(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_fill(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_fill_preserve(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_copy_page(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_show_page(cr:Pcairo_t); cdecl; external LIB_CAIRO;
function  cairo_in_stroke(cr:Pcairo_t; x:double; y:double):cairo_bool_t; cdecl; external LIB_CAIRO;
function  cairo_in_fill(cr:Pcairo_t; x:double; y:double):cairo_bool_t; cdecl; external LIB_CAIRO;
procedure cairo_stroke_extents(cr:Pcairo_t; x1:Pdouble; y1:Pdouble; x2:Pdouble; y2:Pdouble); cdecl; external LIB_CAIRO;
procedure cairo_fill_extents(cr:Pcairo_t; x1:Pdouble; y1:Pdouble; x2:Pdouble; y2:Pdouble); cdecl; external LIB_CAIRO;
procedure cairo_reset_clip(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_clip(cr:Pcairo_t); cdecl; external LIB_CAIRO;
procedure cairo_clip_preserve(cr:Pcairo_t); cdecl; external LIB_CAIRO;

function  cairo_font_options_create:Pcairo_font_options_t; cdecl; external LIB_CAIRO;
function  cairo_font_options_copy(original:Pcairo_font_options_t):Pcairo_font_options_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_destroy(options:Pcairo_font_options_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_status(options:Pcairo_font_options_t):cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_merge(options:Pcairo_font_options_t; other:Pcairo_font_options_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_equal(options:Pcairo_font_options_t; other:Pcairo_font_options_t):cairo_bool_t; cdecl; external LIB_CAIRO;
function  cairo_font_options_hash(options:Pcairo_font_options_t):dword; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_antialias(options:Pcairo_font_options_t; antialias:cairo_antialias_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_antialias(options:Pcairo_font_options_t):cairo_antialias_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_subpixel_order(options:Pcairo_font_options_t; subpixel_order:cairo_subpixel_order_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_subpixel_order(options:Pcairo_font_options_t):cairo_subpixel_order_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_hint_style(options:Pcairo_font_options_t; hint_style:cairo_hint_style_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_hint_style(options:Pcairo_font_options_t):cairo_hint_style_t; cdecl; external LIB_CAIRO;
procedure cairo_font_options_set_hint_metrics(options:Pcairo_font_options_t; hint_metrics:cairo_hint_metrics_t); cdecl; external LIB_CAIRO;
function  cairo_font_options_get_hint_metrics(options:Pcairo_font_options_t):cairo_hint_metrics_t; cdecl; external LIB_CAIRO;
procedure cairo_select_font_face(cr:Pcairo_t; family:Pchar; slant:cairo_font_slant_t; weight:cairo_font_weight_t); cdecl; external LIB_CAIRO;
procedure cairo_set_font_size(cr:Pcairo_t; size:double); cdecl; external LIB_CAIRO;
procedure cairo_set_font_matrix(cr:Pcairo_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_get_font_matrix(cr:Pcairo_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_set_font_options(cr:Pcairo_t; options:Pcairo_font_options_t); cdecl; external LIB_CAIRO;
procedure cairo_get_font_options(cr:Pcairo_t; options:Pcairo_font_options_t); cdecl; external LIB_CAIRO;
procedure cairo_show_text(cr:Pcairo_t; utf8:Pchar); cdecl; external LIB_CAIRO;
procedure cairo_show_glyphs(cr:Pcairo_t; glyphs:Pcairo_glyph_t; num_glyphs:longint); cdecl; external LIB_CAIRO;
function  cairo_get_font_face(cr:Pcairo_t):Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_font_extents(cr:Pcairo_t; extents:Pcairo_font_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_set_font_face(cr:Pcairo_t; font_face:Pcairo_font_face_t); cdecl; external LIB_CAIRO;
procedure cairo_text_extents(cr:Pcairo_t; utf8:Pchar; extents:Pcairo_text_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_glyph_extents(cr:Pcairo_t; glyphs:Pcairo_glyph_t; num_glyphs:longint; extents:Pcairo_text_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_text_path(cr:Pcairo_t; utf8:Pchar); cdecl; external LIB_CAIRO;
procedure cairo_glyph_path(cr:Pcairo_t; glyphs:Pcairo_glyph_t; num_glyphs:longint); cdecl; external LIB_CAIRO;
function  cairo_font_face_reference(font_face:Pcairo_font_face_t):Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_font_face_destroy(font_face:Pcairo_font_face_t); cdecl; external LIB_CAIRO;
function  cairo_font_face_status(font_face:Pcairo_font_face_t):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_font_face_get_user_data(font_face:Pcairo_font_face_t; key:Pcairo_user_data_key_t):pointer; cdecl; external LIB_CAIRO;
function  cairo_font_face_set_user_data(font_face:Pcairo_font_face_t; key:Pcairo_user_data_key_t; user_data:pointer; destroy:cairo_destroy_func_t):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_create(font_face:Pcairo_font_face_t; font_matrix:Pcairo_matrix_t; ctm:Pcairo_matrix_t; options:Pcairo_font_options_t):Pcairo_scaled_font_t; cdecl; external LIB_CAIRO;
function  cairo_scaled_font_reference(scaled_font:Pcairo_scaled_font_t):Pcairo_scaled_font_t; cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_destroy(scaled_font:Pcairo_scaled_font_t); cdecl; external LIB_CAIRO;
function  cairo_scaled_font_status(scaled_font:Pcairo_scaled_font_t):cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_extents(scaled_font:Pcairo_scaled_font_t; extents:Pcairo_font_extents_t); cdecl; external LIB_CAIRO;
procedure cairo_scaled_font_glyph_extents(scaled_font:Pcairo_scaled_font_t; glyphs:Pcairo_glyph_t; num_glyphs:longint; extents:Pcairo_text_extents_t); cdecl; external LIB_CAIRO;

function  cairo_get_operator(cr:Pcairo_t):cairo_operator_t; cdecl; external LIB_CAIRO;
function  cairo_get_source(cr:Pcairo_t):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_get_tolerance(cr:Pcairo_t):double; cdecl; external LIB_CAIRO;
function  cairo_get_antialias(cr:Pcairo_t):cairo_antialias_t; cdecl; external LIB_CAIRO;
procedure cairo_get_current_point(cr:Pcairo_t; x:Pdouble; y:Pdouble); cdecl; external LIB_CAIRO;
function  cairo_get_fill_rule(cr:Pcairo_t):cairo_fill_rule_t; cdecl; external LIB_CAIRO;
function  cairo_get_line_width(cr:Pcairo_t):double; cdecl; external LIB_CAIRO;
function  cairo_get_line_cap(cr:Pcairo_t):cairo_line_cap_t; cdecl; external LIB_CAIRO;
function  cairo_get_line_join(cr:Pcairo_t):cairo_line_join_t; cdecl; external LIB_CAIRO;
function  cairo_get_miter_limit(cr:Pcairo_t):double; cdecl; external LIB_CAIRO;
procedure cairo_get_matrix(cr:Pcairo_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
function  cairo_get_target(cr:Pcairo_t):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_copy_path(cr:Pcairo_t):Pcairo_path_t; cdecl; external LIB_CAIRO;
function  cairo_copy_path_flat(cr:Pcairo_t):Pcairo_path_t; cdecl; external LIB_CAIRO;
procedure cairo_append_path(cr:Pcairo_t; path:Pcairo_path_t); cdecl; external LIB_CAIRO;
procedure cairo_path_destroy(path:Pcairo_path_t); cdecl; external LIB_CAIRO;
function  cairo_status(cr:Pcairo_t):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_status_to_string(status:cairo_status_t):Pchar; cdecl; external LIB_CAIRO;

function  cairo_surface_create_similar(other:Pcairo_surface_t; content:cairo_content_t; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_surface_reference(surface:Pcairo_surface_t):Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_surface_destroy(surface:Pcairo_surface_t); cdecl; external LIB_CAIRO;
function  cairo_surface_status(surface:Pcairo_surface_t):cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_surface_finish(surface:Pcairo_surface_t); cdecl; external LIB_CAIRO;
function  cairo_surface_write_to_png(surface:Pcairo_surface_t; filename:Pchar):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_surface_write_to_png_stream(surface:Pcairo_surface_t; write_func:cairo_write_func_t; closure:pointer):cairo_status_t; cdecl; external LIB_CAIRO;
function  cairo_surface_get_user_data(surface:Pcairo_surface_t; key:Pcairo_user_data_key_t):pointer; cdecl; external LIB_CAIRO;
function  cairo_surface_set_user_data(surface:Pcairo_surface_t; key:Pcairo_user_data_key_t; user_data:pointer; destroy:cairo_destroy_func_t):cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_surface_get_font_options(surface:Pcairo_surface_t; options:Pcairo_font_options_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_flush(surface:Pcairo_surface_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_mark_dirty(surface:Pcairo_surface_t); cdecl; external LIB_CAIRO;
procedure cairo_surface_mark_dirty_rectangle(surface:Pcairo_surface_t; x:longint; y:longint; width:longint; height:longint); cdecl; external LIB_CAIRO;
procedure cairo_surface_set_device_offset(surface:Pcairo_surface_t; x_offset:double; y_offset:double); cdecl; external LIB_CAIRO;

function  cairo_image_surface_create(format:cairo_format_t; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_create_for_data(data:Pbyte; format:cairo_format_t; width:longint; height:longint; stride:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_width(surface:Pcairo_surface_t):longint; cdecl; external LIB_CAIRO;
function  cairo_image_surface_get_height(surface:Pcairo_surface_t):longint; cdecl; external LIB_CAIRO;
function  cairo_image_surface_create_from_png(filename:Pchar):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_image_surface_create_from_png_stream(read_func:cairo_read_func_t; closure:pointer):Pcairo_surface_t; cdecl; external LIB_CAIRO;

function  cairo_pattern_create_rgb(red:double; green:double; blue:double):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_rgba(red:double; green:double; blue:double; alpha:double):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_for_surface(surface:Pcairo_surface_t):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_linear(x0:double; y0:double; x1:double; y1:double):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_create_radial(cx0:double; cy0:double; radius0:double; cx1:double; cy1:double; radius1:double):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
function  cairo_pattern_reference(pattern:Pcairo_pattern_t):Pcairo_pattern_t; cdecl; external LIB_CAIRO;
procedure cairo_pattern_destroy(pattern:Pcairo_pattern_t); cdecl; external LIB_CAIRO;
function  cairo_pattern_status(pattern:Pcairo_pattern_t):cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_pattern_add_color_stop_rgb(pattern:Pcairo_pattern_t; offset:double; red:double; green:double; blue:double); cdecl; external LIB_CAIRO;
procedure cairo_pattern_add_color_stop_rgba(pattern:Pcairo_pattern_t; offset:double; red:double; green:double; blue:double; alpha:double); cdecl; external LIB_CAIRO;
procedure cairo_pattern_set_matrix(pattern:Pcairo_pattern_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_pattern_get_matrix(pattern:Pcairo_pattern_t; matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_pattern_set_extend(pattern:Pcairo_pattern_t; extend:cairo_extend_t); cdecl; external LIB_CAIRO;
function  cairo_pattern_get_extend(pattern:Pcairo_pattern_t):cairo_extend_t; cdecl; external LIB_CAIRO;
procedure cairo_pattern_set_filter(pattern:Pcairo_pattern_t; filter:cairo_filter_t); cdecl; external LIB_CAIRO;
function  cairo_pattern_get_filter(pattern:Pcairo_pattern_t):cairo_filter_t; cdecl; external LIB_CAIRO;

procedure cairo_matrix_init(matrix:Pcairo_matrix_t; xx:double; yx:double; xy:double; yy:double; x0:double; y0:double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_identity(matrix:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_translate(matrix:Pcairo_matrix_t; tx:double; ty:double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_scale(matrix:Pcairo_matrix_t; sx:double; sy:double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_init_rotate(matrix:Pcairo_matrix_t; radians:double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_translate(matrix:Pcairo_matrix_t; tx:double; ty:double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_scale(matrix:Pcairo_matrix_t; sx:double; sy:double); cdecl; external LIB_CAIRO;
procedure cairo_matrix_rotate(matrix:Pcairo_matrix_t; radians:double); cdecl; external LIB_CAIRO;
function  cairo_matrix_invert(matrix:Pcairo_matrix_t):cairo_status_t; cdecl; external LIB_CAIRO;
procedure cairo_matrix_multiply(result:Pcairo_matrix_t; a:Pcairo_matrix_t; b:Pcairo_matrix_t); cdecl; external LIB_CAIRO;
procedure cairo_matrix_transform_distance(matrix:Pcairo_matrix_t; dx:Pdouble; dy:Pdouble); cdecl; external LIB_CAIRO;
procedure cairo_matrix_transform_point(matrix:Pcairo_matrix_t; x:Pdouble; y:Pdouble); cdecl; external LIB_CAIRO;

function  cairo_xlib_surface_create(dpy:PDisplay; drawable:TDrawable; visual:PVisual; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_for_bitmap(dpy:PDisplay; bitmap:TPixmap; screen:PScreen; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_size(surface:Pcairo_surface_t; width:longint; height:longint); cdecl; external LIB_CAIRO;
procedure cairo_xlib_surface_set_drawable(surface:Pcairo_surface_t; drawable:TDrawable; width:longint; height:longint); cdecl; external LIB_CAIRO;
function  cairo_xlib_surface_create_with_xrender_format(dpy:PDisplay; drawable:TDrawable; screen:PScreen; format:PXRenderPictFormat; width:longint; height:longint):Pcairo_surface_t; cdecl; external LIB_CAIRO;

function  cairo_ft_font_face_create_for_pattern(pattern:PFcPattern):Pcairo_font_face_t; cdecl; external LIB_CAIRO;
procedure cairo_ft_font_options_substitute(options:Pcairo_font_options_t; pattern:PFcPattern); cdecl; external LIB_CAIRO;
function  cairo_ft_font_face_create_for_ft_face(face:TFT_Face; load_flags:longint):Pcairo_font_face_t; cdecl; external LIB_CAIRO;
function  cairo_ft_scaled_font_lock_face(scaled_font:Pcairo_scaled_font_t):TFT_Face; cdecl; external LIB_CAIRO;
procedure cairo_ft_scaled_font_unlock_face(scaled_font:Pcairo_scaled_font_t); cdecl; external LIB_CAIRO;

// These two functions renamed *_get_* to avoid collision...
function cairo_get_version:longint; cdecl; external LIB_CAIRO name 'cairo_version';
function cairo_get_version_string:Pchar; cdecl; external LIB_CAIRO name 'cairo_version_string';

function CAIRO_VERSION : longint;
function CAIRO_VERSION_ENCODE(major,minor,micro : longint) : longint;  

implementation

function CAIRO_VERSION : longint;
begin
  CAIRO_VERSION:=CAIRO_VERSION_ENCODE(CAIRO_VERSION_MAJOR,CAIRO_VERSION_MINOR,CAIRO_VERSION_MICRO);
end;

function CAIRO_VERSION_ENCODE(major,minor,micro : longint) : longint;
begin
  CAIRO_VERSION_ENCODE:=((major * 10000) + (minor * 100)) + (micro * 1);
end;


end.
