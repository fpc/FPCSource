{$IfDef read_interface}

type
   PGnomeCanvasPoints = ^TGnomeCanvasPoints;
   TGnomeCanvasPoints = record
        num_points : longint;
        coords : Pdouble;
        ref_count : longint;
     end;

function gnome_canvas_points_new(num_points:longint):PGnomeCanvasPoints;cdecl;external libgnomeuidll name 'gnome_canvas_points_new';
function gnome_canvas_points_ref(points:PGnomeCanvasPoints):PGnomeCanvasPoints;cdecl;external libgnomeuidll name 'gnome_canvas_points_ref';

procedure gnome_canvas_points_unref(points:PGnomeCanvasPoints);cdecl;external libgnomeuidll name 'gnome_canvas_points_free';
procedure gnome_canvas_points_free(points:PGnomeCanvasPoints);cdecl;external libgnomeuidll name 'gnome_canvas_points_free';

function gnome_canvas_get_miter_points(x1:double; y1:double; x2:double; y2:double; x3:double;
           y3:double; width:double; mx1:Pdouble; my1:Pdouble; mx2:Pdouble;
           my2:Pdouble):longint;cdecl;external libgnomeuidll name 'gnome_canvas_get_miter_points';
procedure gnome_canvas_get_butt_points(x1:double; y1:double; x2:double; y2:double; width:double;
            project:longint; bx1:Pdouble; by1:Pdouble; bx2:Pdouble; by2:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_get_butt_points';
function gnome_canvas_polygon_to_point(poly:Pdouble; num_points:longint; x:double; y:double):double;cdecl;external libgnomeuidll name 'gnome_canvas_polygon_to_point';
procedure gnome_canvas_render_svp(buf:PGnomeCanvasBuf; svp:PArtSVP; rgba:guint32);cdecl;external libgnomeuidll name 'gnome_canvas_render_svp';
procedure gnome_canvas_update_svp(canvas:PGnomeCanvas; p_svp:PPArtSVP; new_svp:PArtSVP);cdecl;external libgnomeuidll name 'gnome_canvas_update_svp';
procedure gnome_canvas_update_svp_clip(canvas:PGnomeCanvas; p_svp:PPArtSVP; new_svp:PArtSVP; clip_svp:PArtSVP);cdecl;external libgnomeuidll name 'gnome_canvas_update_svp_clip';
procedure gnome_canvas_item_reset_bounds(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_reset_bounds';
procedure gnome_canvas_item_update_svp(item:PGnomeCanvasItem; p_svp:PPArtSVP; new_svp:PArtSVP);cdecl;external libgnomeuidll name 'gnome_canvas_item_update_svp';
procedure gnome_canvas_item_update_svp_clip(item:PGnomeCanvasItem; p_svp:PPArtSVP; new_svp:PArtSVP; clip_svp:PArtSVP);cdecl;external libgnomeuidll name 'gnome_canvas_item_update_svp_clip';
procedure gnome_canvas_item_request_redraw_svp(item:PGnomeCanvasItem; svp:PArtSVP);cdecl;external libgnomeuidll name 'gnome_canvas_item_request_redraw_svp';
procedure gnome_canvas_update_bbox(item:PGnomeCanvasItem; x1:longint; y1:longint; x2:longint; y2:longint);cdecl;external libgnomeuidll name 'gnome_canvas_update_bbox';
procedure gnome_canvas_buf_ensure_buf(buf:PGnomeCanvasBuf);cdecl;external libgnomeuidll name 'gnome_canvas_buf_ensure_buf';
function gnome_canvas_join_gdk_to_art(gdk_join:TGdkJoinStyle):TArtPathStrokeJoinType;cdecl;external libgnomeuidll name 'gnome_canvas_join_gdk_to_art';
function gnome_canvas_cap_gdk_to_art(gdk_cap:TGdkCapStyle):TArtPathStrokeCapType;cdecl;external libgnomeuidll name 'gnome_canvas_cap_gdk_to_art';

{$EndIf read_interface}

{$Ifdef read_implementation}


{$Endif read_implementation}
