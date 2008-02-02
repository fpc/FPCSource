unit libart;

{$PACKRECORDS C}
{$mode objfpc}

interface

const
 libartdll='art_lgpl';

type
  PPlongint = ^Plongint;

  Part_u8 = ^art_u8;
  art_u8 = Byte;
  Part_u16 = ^art_u16;
  art_u16 = word;
  Part_u32 = ^art_u32;
  art_u32 = longword;
  PArtUtaBbox = ^TArtUtaBbox;
  TArtUtaBbox = art_u32;

  Part_boolean = ^art_boolean;
  art_boolean = boolean;

  Taffine_array = array[0..5] of double;
  Taffine_string = array[0..127] of char;

  TArtPathcode = (ART_MOVETO,ART_MOVETO_OPEN,ART_CURVETO,ART_LINETO,ART_END);
  TArtFilterLevel = (ART_FILTER_NEAREST,ART_FILTER_TILES,ART_FILTER_BILINEAR,ART_FILTER_HYPER);
  TArtPathStrokeJoinType = (ART_PATH_STROKE_JOIN_MITER,ART_PATH_STROKE_JOIN_ROUND,ART_PATH_STROKE_JOIN_BEVEL);
  TArtPathStrokeCapType = (ART_PATH_STROKE_CAP_BUTT,ART_PATH_STROKE_CAP_ROUND,ART_PATH_STROKE_CAP_SQUARE);
  TArtWindRule = (ART_WIND_RULE_NONZERO,ART_WIND_RULE_INTERSECT,ART_WIND_RULE_ODDEVEN,ART_WIND_RULE_POSITIVE);
  TArtPixFormat = (ART_PIX_RGB);

  TArtDestroyNotify = procedure (func_data:pointer; data:pointer);cdecl;

  (* Art Point *)
  PArtPoint = ^TArtPoint;
  TArtPoint = record
    x : double;
    y : double;
  end;

  (* Art Double Rect *)
  PArtDRect = ^TArtDRect;
  TArtDRect = record
    x0 : double;
    y0 : double;
    x1 : double;
    y1 : double;
  end;

  (* Art Longint Rect *)
  PArtIRect = ^TArtIRect;
  TArtIRect = record
    x0 : longint;
    y0 : longint;
    x1 : longint;
    y1 : longint;
  end;

  (* Art Bezier Path *)
  PArtBpath = ^TArtBpath;
  TArtBpath = record
    code : TArtPathcode;
    x1 : double;
    y1 : double;
    x2 : double;
    y2 : double;
    x3 : double;
    y3 : double;
  end;

  PPArtVpath = ^PArtVpath;
  PArtVpath = ^TArtVpath;
  TArtVpath = record
     code : TArtPathcode;
     x : double;
     y : double;
  end;

  PArtVpathDash = ^TArtVpathDash;
  TArtVpathDash = record
    offset : double;
    n_dash : longint;
    dash : Pdouble;
  end;

  PArtAlphaGamma = ^TArtAlphaGamma;
  TArtAlphaGamma = record
    gamma : double;
    invtable_size : longint;
    table : array[0..255] of longint;
    invtable : array[0..0] of art_u8;
  end;

  PArtSVPSeg = ^TArtSVPSeg;
  TArtSVPSeg = record
    n_points : longint;
    dir : longint;
    bbox : TArtDRect;
    points : PArtPoint;
  end;

  PPArtSVP = ^PArtSVP;
  PArtSVP = ^TArtSVP;
  TArtSVP = record
    n_segs : longint;
    segs : array[0..0] of TArtSVPSeg;
  end;

  PArtSVPRenderAAStep = ^TArtSVPRenderAAStep;
  TArtSVPRenderAAStep = record
    x : longint;
    delta : longint;
  end;

  PArtPixBuf = ^TArtPixBuf;
  TArtPixBuf = record
    format : TArtPixFormat;
    n_channels : longint;
    has_alpha : longint;
    bits_per_sample : longint;
    pixels : Part_u8;
    width : longint;
    height : longint;
    rowstride : longint;
    destroy_data : pointer;
    destroy : TArtDestroyNotify;
  end;

  PArtUta = ^TArtUta;
  TArtUta = record
    x0 : longint;
    y0 : longint;
    width : longint;
    height : longint;
    utiles : PArtUtaBbox;
  end;

  Tart_svp_render_aa_callback = procedure (callback_data:pointer; y:longint; start:longint; steps:PArtSVPRenderAAStep; n_steps:longint); cdecl;

const
   ART_FALSE = FALSE;
   ART_TRUE = TRUE;
   M_PI = 3.14159265358979323846;
   M_SQRT2 = 1.41421356237309504880;

const
   ART_UTILE_SHIFT = 5;
   ART_UTILE_SIZE = 1 shl ART_UTILE_SHIFT;

procedure art_die(fmt:Pchar; args:array of const);cdecl;external libartdll name 'art_die';
procedure art_warn(fmt:Pchar; args:array of const);cdecl;external libartdll name 'art_warn';

(* ART longint Rect *)
procedure art_irect_copy(dest:PArtIRect; src:PArtIRect);cdecl;external libartdll name 'art_irect_copy';
procedure art_irect_union(dest:PArtIRect; src1:PArtIRect; src2:PArtIRect);cdecl;external libartdll name 'art_irect_union';
procedure art_irect_intersect(dest:PArtIRect; src1:PArtIRect; src2:PArtIRect);cdecl;external libartdll name 'art_irect_intersect';
function art_irect_empty(src:PArtIRect):longint;cdecl;external libartdll name 'art_irect_empty';

(* ART double Rect *)
procedure art_drect_copy(dest:PArtDRect; src:PArtDRect);cdecl;external libartdll name 'art_drect_copy';
procedure art_drect_union(dest:PArtDRect; src1:PArtDRect; src2:PArtDRect);cdecl;external libartdll name 'art_drect_union';
procedure art_drect_intersect(dest:PArtDRect; src1:PArtDRect; src2:PArtDRect);cdecl;external libartdll name 'art_drect_intersect';
function art_drect_empty(src:PArtDRect):longint;cdecl;external libartdll name 'art_drect_empty';
procedure art_drect_affine_transform(dst:PArtDRect; src:PArtDRect; matrix:Taffine_array);cdecl;external libartdll name 'art_drect_affine_transform';
procedure art_drect_to_irect(dst:PArtIRect; src:PArtDRect);cdecl;external libartdll name 'art_drect_to_irect';
procedure art_drect_svp(bbox:PArtDRect; svp:PArtSVP);cdecl;external libartdll name 'art_drect_svp';
procedure art_drect_svp_union(bbox:PArtDRect; svp:PArtSVP);cdecl;external libartdll name 'art_drect_svp_union';

(* ART bezier's *)
function art_bezier_to_vec(x0:double; y0:double; x1:double; y1:double; x2:double;
           y2:double; x3:double; y3:double; p:PArtPoint; level:longint):PArtPoint;cdecl;external libartdll name 'art_bezier_to_vec';
function art_bez_path_to_vec(bez:PArtBpath; flatness:double):PArtVpath;cdecl;external libartdll name 'art_bez_path_to_vec';
function art_bpath_affine_transform(src:PArtBpath; matrix:Taffine_array):PArtBpath;cdecl;external libartdll name 'art_bpath_affine_transform';

(* ART affine transformations *)
procedure art_affine_point(dst:PArtPoint; src:PArtPoint; affine:Taffine_array);cdecl;external libartdll name 'art_affine_point';
procedure art_affine_invert(dst_affine:Taffine_array; src_affine:Taffine_array);cdecl;external libartdll name 'art_affine_invert';
procedure art_affine_flip(dst_affine:Taffine_array; src_affine:Taffine_array; horz:longint; vert:longint);cdecl;external libartdll name 'art_affine_flip';
procedure art_affine_to_string(str:Taffine_string; src:Taffine_array);cdecl;external libartdll name 'art_affine_to_string';
procedure art_affine_multiply(dst:Taffine_array; src1:Taffine_array; src2:Taffine_array);cdecl;external libartdll name 'art_affine_multiply';
procedure art_affine_identity(dst:Taffine_array);cdecl;external libartdll name 'art_affine_identity';
procedure art_affine_scale(dst:Taffine_array; sx:double; sy:double);cdecl;external libartdll name 'art_affine_scale';
procedure art_affine_rotate(dst:Taffine_array; theta:double);cdecl;external libartdll name 'art_affine_rotate';
procedure art_affine_shear(dst:Taffine_array; theta:double);cdecl;external libartdll name 'art_affine_shear';
procedure art_affine_translate(dst:Taffine_array; tx:double; ty:double);cdecl;external libartdll name 'art_affine_translate';
function art_affine_expansion(src:Taffine_array):double;cdecl;external libartdll name 'art_affine_expansion';
function art_affine_rectilinear(src:Taffine_array):longint;cdecl;external libartdll name 'art_affine_rectilinear';
function art_affine_equal(matrix1:Taffine_array; matrix2:Taffine_array):longint;cdecl;external libartdll name 'art_affine_equal';

(* ART alphagamma *)
function art_alphagamma_new(gamma:double):PArtAlphaGamma;cdecl;external libartdll name 'art_alphagamma_new';
procedure art_alphagamma_free(alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_alphagamma_free';


(* ART SVP *)
function art_svp_add_segment(p_vp:PPArtSVP; pn_segs_max:Plongint; pn_points_max:PPlongint; n_points:longint; dir:longint;
           points:PArtPoint; bbox:PArtDRect):longint;cdecl;external libartdll name 'art_svp_add_segment';
procedure art_svp_free(svp:PArtSVP);cdecl;external libartdll name 'art_svp_free';
function art_svp_seg_compare(s1:pointer; s2:pointer):longint;cdecl;external libartdll name 'art_svp_seg_compare';

function art_svp_union(svp1:PArtSVP; svp2:PArtSVP):PArtSVP;cdecl;external libartdll name 'art_svp_union';
function art_svp_intersect(svp1:PArtSVP; svp2:PArtSVP):PArtSVP;cdecl;external libartdll name 'art_svp_intersect';
function art_svp_diff(svp1:PArtSVP; svp2:PArtSVP):PArtSVP;cdecl;external libartdll name 'art_svp_diff';
function art_svp_minus(svp1:PArtSVP; svp2:PArtSVP):PArtSVP;cdecl;external libartdll name 'art_svp_minus';

function art_svp_point_wind(svp:PArtSVP; x:double; y:double):longint;cdecl;external libartdll name 'art_svp_point_wind';
function art_svp_point_dist(svp:PArtSVP; x:double; y:double):double;cdecl;external libartdll name 'art_svp_point_dist';

procedure art_svp_render_aa(svp:PArtSVP; x0:longint; y0:longint; x1:longint; y1:longint;
            callback: Tart_svp_render_aa_callback; callback_data:pointer);cdecl;external libartdll name 'art_svp_render_aa';

function art_svp_from_vpath(vpath:PArtVpath):PArtSVP;cdecl;external libartdll name 'art_svp_from_vpath';

function art_svp_vpath_stroke(vpath:PArtVpath; join:TArtPathStrokeJoinType; cap:TArtPathStrokeCapType; line_width:double; miter_limit:double;
           flatness:double):PArtSVP;cdecl;external libartdll name 'art_svp_vpath_stroke';
function art_svp_vpath_stroke_raw(vpath:PArtVpath; join:TArtPathStrokeJoinType; cap:TArtPathStrokeCapType; line_width:double; miter_limit:double;
           flatness:double):PArtVpath;cdecl;external libartdll name 'art_svp_vpath_stroke_raw';

function art_svp_uncross(vp:PArtSVP):PArtSVP;cdecl;external libartdll name 'art_svp_uncross';
function art_svp_rewind_uncrossed(vp:PArtSVP; rule:TArtWindRule):PArtSVP;cdecl;external libartdll name 'art_svp_rewind_uncrossed';

(* ART GRAY SVP *)
procedure art_gray_svp_aa(svp:PArtSVP; x0:longint; y0:longint; x1:longint; y1:longint;
            buf:Part_u8; rowstride:longint);cdecl;external libartdll name 'art_gray_svp_aa';

(* ART VPATH *)
procedure art_vpath_add_point(p_vpath:PPArtVpath; pn_points:Plongint; pn_points_max:Plongint; code:TArtPathcode; x:double;
  y:double);cdecl;external libartdll name 'art_vpath_add_point';
function art_vpath_new_circle(x:double; y:double; r:double):PArtVpath;cdecl;external libartdll name 'art_vpath_new_circle';
function art_vpath_affine_transform(src:PArtVpath; matrix:Taffine_array):PArtVpath;cdecl;external libartdll name 'art_vpath_affine_transform';
procedure art_vpath_bbox_drect(vec:PArtVpath; drect:PArtDRect);cdecl;external libartdll name 'art_vpath_bbox_drect';
procedure art_vpath_bbox_irect(vec:PArtVpath; irect:PArtIRect);cdecl;external libartdll name 'art_vpath_bbox_irect';
function art_vpath_perturb(src:PArtVpath):PArtVpath;cdecl;external libartdll name 'art_vpath_perturb';
function art_vpath_from_svp(svp:PArtSVP):PArtVpath;cdecl;external libartdll name 'art_vpath_from_svp';
function art_vpath_dash(vpath:PArtVpath; dash:PArtVpathDash):PArtVpath;cdecl;external libartdll name 'art_vpath_dash';

(* ART PIXBUF *)
function art_pixbuf_new_rgb(pixels:Part_u8; width:longint; height:longint; rowstride:longint):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_new_rgb';
function art_pixbuf_new_rgba(pixels:Part_u8; width:longint; height:longint; rowstride:longint):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_new_rgba';
function art_pixbuf_new_const_rgb(pixels:Part_u8; width:longint; height:longint; rowstride:longint):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_new_const_rgb';
function art_pixbuf_new_const_rgba(pixels:Part_u8; width:longint; height:longint; rowstride:longint):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_new_const_rgba';
function art_pixbuf_new_rgb_dnotify(pixels:Part_u8; width:longint; height:longint; rowstride:longint; dfunc_data:pointer;
           dfunc:TArtDestroyNotify):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_new_rgb_dnotify';
function art_pixbuf_new_rgba_dnotify(pixels:Part_u8; width:longint; height:longint; rowstride:longint; dfunc_data:pointer;
           dfunc:TArtDestroyNotify):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_new_rgba_dnotify';
procedure art_pixbuf_free(pixbuf:PArtPixBuf);cdecl;external libartdll name 'art_pixbuf_free';
procedure art_pixbuf_free_shallow(pixbuf:PArtPixBuf);cdecl;external libartdll name 'art_pixbuf_free_shallow';
function art_pixbuf_duplicate(pixbuf:PArtPixBuf):PArtPixBuf;cdecl;external libartdll name 'art_pixbuf_duplicate';

(* ART RGB *)
procedure art_rgb_fill_run(buf:Part_u8; r:art_u8; g:art_u8; b:art_u8; n:longint);cdecl;external libartdll name 'art_rgb_fill_run';
procedure art_rgb_run_alpha(buf:Part_u8; r:art_u8; g:art_u8; b:art_u8; alpha:longint; n:longint);cdecl;external libartdll name 'art_rgb_run_alpha';

procedure art_rgb_affine(dst:Part_u8; x0:longint; y0:longint; x1:longint; y1:longint;
            dst_rowstride:longint; src:Part_u8; src_width:longint; src_height:longint; src_rowstride:longint;
            affine:Taffine_array; level:TArtFilterLevel; alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_rgb_affine';

procedure art_rgb_bitmap_affine(dst:Part_u8; x0:longint; y0:longint; x1:longint; y1:longint;
            dst_rowstride:longint; src:Part_u8; src_width:longint; src_height:longint; src_rowstride:longint;
            rgba:art_u32; affine:Taffine_array; level:TArtFilterLevel; alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_rgb_bitmap_affine';

procedure art_rgb_pixbuf_affine(dst:Part_u8; x0:longint; y0:longint; x1:longint; y1:longint;
            dst_rowstride:longint; pixbuf:PArtPixBuf; affine:Taffine_array; level:TArtFilterLevel; alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_rgb_pixbuf_affine';

procedure art_rgb_rgba_affine(dst:Part_u8; x0:longint; y0:longint; x1:longint; y1:longint;
            dst_rowstride:longint; src:Part_u8; src_width:longint; src_height:longint; src_rowstride:longint;
            affine:Taffine_array; level:TArtFilterLevel; alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_rgb_rgba_affine';

procedure art_rgb_svp_aa(svp:PArtSVP; x0:longint; y0:longint; x1:longint; y1:longint;
            fg_color:art_u32; bg_color:art_u32; buf:Part_u8; rowstride:longint; alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_rgb_svp_aa';
procedure art_rgb_svp_alpha(svp:PArtSVP; x0:longint; y0:longint; x1:longint; y1:longint;
            rgba:art_u32; buf:Part_u8; rowstride:longint; alphagamma:PArtAlphaGamma);cdecl;external libartdll name 'art_rgb_svp_alpha';

(* ART UTA *)
function art_uta_new(x0:longint; y0:longint; x1:longint; y1:longint):PArtUta;cdecl;external libartdll name 'art_uta_new';
function art_uta_new_coords(x0:longint; y0:longint; x1:longint; y1:longint):PArtUta;cdecl;external libartdll name 'art_uta_new_coords';
procedure art_uta_free(uta:PArtUta);cdecl;external libartdll name 'art_uta_free';

function art_uta_union(uta1:PArtUta; uta2:PArtUta):PArtUta;cdecl;external libartdll name 'art_uta_union';
function art_uta_from_irect(bbox:PArtIRect):PArtUta;cdecl;external libartdll name 'art_uta_from_irect';
function art_uta_from_svp(svp:PArtSVP):PArtUta;cdecl;external libartdll name 'art_uta_from_svp';
function art_uta_from_vpath(vec:PArtVpath):PArtUta;cdecl;external libartdll name 'art_uta_from_vpath';
procedure art_uta_add_line(uta:PArtUta; x0:double; y0:double; x1:double; y1:double;
            rbuf:Plongint; rbuf_rowstride:longint);cdecl;external libartdll name 'art_uta_add_line';
function art_rect_list_from_uta(uta:PArtUta; max_width:longint; max_height:longint; p_nrects:Plongint):PArtIRect;cdecl;external libartdll name 'art_rect_list_from_uta';

function ART_UTA_BBOX_CONS(x0,y0,x1,y1 : longint) : longint;
function ART_UTA_BBOX_X0(ub : longint) : longint;
function ART_UTA_BBOX_Y0(ub : longint) : longint;
function ART_UTA_BBOX_X1(ub : longint) : longint;
function ART_UTA_BBOX_Y1(ub : longint) : longint;

implementation

function ART_UTA_BBOX_CONS(x0,y0,x1,y1 : longint) : longint;
begin
   ART_UTA_BBOX_CONS:=(((x0 shl 24) or (y0 shl 16)) or (x1 shl 8)) or y1;
end;

function ART_UTA_BBOX_X0(ub : longint) : longint;
begin
   ART_UTA_BBOX_X0:=ub shr 24;
end;

function ART_UTA_BBOX_Y0(ub : longint) : longint;
begin
   ART_UTA_BBOX_Y0:=(ub shr 16) and $ff;
end;

function ART_UTA_BBOX_X1(ub : longint) : longint;
begin
   ART_UTA_BBOX_X1:=(ub shr 8) and $ff;
end;

function ART_UTA_BBOX_Y1(ub : longint) : longint;
begin
   ART_UTA_BBOX_Y1:=ub and $ff;
end;

end.
