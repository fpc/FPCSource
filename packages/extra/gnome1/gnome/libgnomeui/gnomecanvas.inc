{$IfDef read_interface}

const
   GNOME_CANVAS_EPSILON = 1e-10;

function GNOME_CANVAS_COLOR(r,g,b : longint) : longint;
function GNOME_CANVAS_COLOR_A(r,g,b,a : longint) : longint;

const
   GNOME_CANVAS_ITEM_REALIZED = 1 shl 4;
   GNOME_CANVAS_ITEM_MAPPED = 1 shl 5;
   GNOME_CANVAS_ITEM_ALWAYS_REDRAW = 1 shl 6;
   GNOME_CANVAS_ITEM_VISIBLE = 1 shl 7;
   GNOME_CANVAS_ITEM_NEED_UPDATE = 1 shl 8;
   GNOME_CANVAS_ITEM_NEED_AFFINE = 1 shl 9;
   GNOME_CANVAS_ITEM_NEED_CLIP = 1 shl 10;
   GNOME_CANVAS_ITEM_NEED_VIS = 1 shl 11;
   GNOME_CANVAS_ITEM_AFFINE_FULL = 1 shl 12;

   GNOME_CANVAS_UPDATE_REQUESTED = 1 shl 0;
   GNOME_CANVAS_UPDATE_AFFINE = 1 shl 1;
   GNOME_CANVAS_UPDATE_CLIP = 1 shl 2;
   GNOME_CANVAS_UPDATE_VISIBILITY = 1 shl 3;

Type
   PGnomeCanvasBuf = ^TGnomeCanvasBuf;
   TGnomeCanvasBuf = record
        buf : Pguchar;
        buf_rowstride : longint;
        rect : TArtIRect;
        bg_color : guint32;
        flag0 : word;
     end;

const
   bm_GnomeCanvasBuf_is_bg = $1;
   bp_GnomeCanvasBuf_is_bg = 0;
   bm_GnomeCanvasBuf_is_buf = $2;
   bp_GnomeCanvasBuf_is_buf = 1;

function is_bg(var a : TGnomeCanvasBuf) : dword;
procedure set_is_bg(var a : TGnomeCanvasBuf; __is_bg : dword);
function is_buf(var a : TGnomeCanvasBuf) : dword;
procedure set_is_buf(var a : TGnomeCanvasBuf; __is_buf : dword);

type
   PPGnomeCanvasItem = ^PGnomeCanvasItem;
   PGnomeCanvasItem = ^TGnomeCanvasItem;
   PGnomeCanvasGroup = ^TGnomeCanvasGroup;
   PGnomeCanvas = ^TGnomeCanvas;

   TGnomeCanvasItem = record
        theobject : TGtkObject;
        canvas : PGnomeCanvas;
        parent : PGnomeCanvasItem;
        x1 : double;
        y1 : double;
        x2 : double;
        y2 : double;
        xform : Pdouble;
     end;
   GNOME_CANVAS_ITEM = PGnomeCanvasItem;

   PGnomeCanvasItemClass = ^TGnomeCanvasItemClass;
   TGnomeCanvasItemClass = record
        parent_class : TGtkObjectClass;
        update : procedure (item:PGnomeCanvasItem; affine:Pdouble; clip_path:PArtSVP; flags:longint);cdecl;
        realize : procedure (item:PGnomeCanvasItem);cdecl;
        unrealize : procedure (item:PGnomeCanvasItem);cdecl;
        map : procedure (item:PGnomeCanvasItem);cdecl;
        unmap : procedure (item:PGnomeCanvasItem);cdecl;
        coverage : function (item:PGnomeCanvasItem):PArtUta;cdecl;
        draw : procedure (item:PGnomeCanvasItem; drawable:PGdkDrawable; x:longint; y:longint; width:longint;
                      height:longint);cdecl;
        render : procedure (item:PGnomeCanvasItem; buf:PGnomeCanvasBuf);cdecl;
        point : function (item:PGnomeCanvasItem; x:double; y:double; cx:longint; cy:longint;
                     actual_item:PPGnomeCanvasItem):double;cdecl;
        translate : procedure (item:PGnomeCanvasItem; dx:double; dy:double);cdecl;
        bounds : procedure (item:PGnomeCanvasItem; x1:Pdouble; y1:Pdouble; x2:Pdouble; y2:Pdouble);cdecl;
        event : function (item:PGnomeCanvasItem; event:PGdkEvent):gint;cdecl;
     end;
   GNOME_CANVAS_ITEM_CLASS = PGnomeCanvasItemClass;

   TGnomeCanvasGroup = record
        item : TGnomeCanvasItem;
        item_list : PGList;
        item_list_end : PGList;
        xpos : double;
        ypos : double;
     end;
   GNOME_CANVAS_GROUP = PGnomeCanvasGroup;

   PGnomeCanvasGroupClass = ^TGnomeCanvasGroupClass;
   TGnomeCanvasGroupClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_GROUP_CLASS = PGnomeCanvasGroupClass;

   TGnomeCanvas = record
        layout : TGtkLayout;
        idle_id : guint;
        root : PGnomeCanvasItem;
        root_destroy_id : guint;
        scroll_x1 : double;
        scroll_y1 : double;
        scroll_x2 : double;
        scroll_y2 : double;
        pixels_per_unit : double;
        redraw_x1 : longint;
        redraw_y1 : longint;
        redraw_x2 : longint;
        redraw_y2 : longint;
        redraw_area : PArtUta;
        draw_xofs : longint;
        draw_yofs : longint;
        zoom_xofs : longint;
        zoom_yofs : longint;
        state : longint;
        current_item : PGnomeCanvasItem;
        new_current_item : PGnomeCanvasItem;
        grabbed_item : PGnomeCanvasItem;
        grabbed_event_mask : guint;
        focused_item : PGnomeCanvasItem;
        pick_event : TGdkEvent;
        close_enough : longint;
        cc : PGdkColorContext;
        pixmap_gc : PGdkGC;
        flag0 : word;
     end;
   GNOME_CANVAS = PGnomeCanvas;

   PGnomeCanvasClass = ^TGnomeCanvasClass;
   TGnomeCanvasClass = record
        parent_class : TGtkLayoutClass;
     end;
   GNOME_CANVAS_CLASS = PGnomeCanvasClass;

function GNOME_TYPE_CANVAS_ITEM : TGTKType;
function GNOME_IS_CANVAS_ITEM(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_ITEM_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_item_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_item_get_type';
function gnome_canvas_item_new(parent:PGnomeCanvasGroup; thetype:TGtkType; first_arg_name:Pgchar; args:array of const):PGnomeCanvasItem;cdecl;external libgnomeuidll name 'gnome_canvas_item_new';
function gnome_canvas_item_newv(parent:PGnomeCanvasGroup; thetype:TGtkType; nargs:guint; args:PGtkArg):PGnomeCanvasItem;cdecl;external libgnomeuidll name 'gnome_canvas_item_newv';
procedure gnome_canvas_item_construct(item:PGnomeCanvasItem; parent:PGnomeCanvasGroup; first_arg_name:Pgchar; args:va_list);cdecl;external libgnomeuidll name 'gnome_canvas_item_construct';
procedure gnome_canvas_item_constructv(item:PGnomeCanvasItem; parent:PGnomeCanvasGroup; nargs:guint; args:PGtkArg);cdecl;external libgnomeuidll name 'gnome_canvas_item_constructv';
procedure gnome_canvas_item_set(item:PGnomeCanvasItem; first_arg_name:Pgchar; args:array of const);cdecl;external libgnomeuidll name 'gnome_canvas_item_set';
procedure gnome_canvas_item_setv(item:PGnomeCanvasItem; nargs:guint; args:PGtkArg);cdecl;external libgnomeuidll name 'gnome_canvas_item_setv';
procedure gnome_canvas_item_set_valist(item:PGnomeCanvasItem; first_arg_name:Pgchar; args:va_list);cdecl;external libgnomeuidll name 'gnome_canvas_item_set_valist';
procedure gnome_canvas_item_move(item:PGnomeCanvasItem; dx:double; dy:double);cdecl;external libgnomeuidll name 'gnome_canvas_item_move';
procedure gnome_canvas_item_affine_relative(item:PGnomeCanvasItem; affine:Taffine_array);cdecl;external libgnomeuidll name 'gnome_canvas_item_affine_relative';
procedure gnome_canvas_item_affine_absolute(item:PGnomeCanvasItem; affine:Taffine_array);cdecl;external libgnomeuidll name 'gnome_canvas_item_affine_absolute';
procedure gnome_canvas_item_scale(item:PGnomeCanvasItem; x:double; y:double; scale_x:double; scale_y:double);cdecl;external libgnomeuidll name 'gnome_canvas_item_scale';
procedure gnome_canvas_item_rotate(item:PGnomeCanvasItem; x:double; y:double; angle:double);cdecl;external libgnomeuidll name 'gnome_canvas_item_rotate';
procedure gnome_canvas_item_raise(item:PGnomeCanvasItem; positions:longint);cdecl;external libgnomeuidll name 'gnome_canvas_item_raise';
procedure gnome_canvas_item_lower(item:PGnomeCanvasItem; positions:longint);cdecl;external libgnomeuidll name 'gnome_canvas_item_lower';
procedure gnome_canvas_item_raise_to_top(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_raise_to_top';
procedure gnome_canvas_item_lower_to_bottom(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_lower_to_bottom';
procedure gnome_canvas_item_show(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_show';
procedure gnome_canvas_item_hide(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_hide';
function gnome_canvas_item_grab(item:PGnomeCanvasItem; event_mask:dword; cursor:PGdkCursor; etime:guint32):longint;cdecl;external libgnomeuidll name 'gnome_canvas_item_grab';
procedure gnome_canvas_item_ungrab(item:PGnomeCanvasItem; etime:guint32);cdecl;external libgnomeuidll name 'gnome_canvas_item_ungrab';
procedure gnome_canvas_item_w2i(item:PGnomeCanvasItem; x:Pdouble; y:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_item_w2i';
procedure gnome_canvas_item_i2w(item:PGnomeCanvasItem; x:Pdouble; y:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_item_i2w';
procedure gnome_canvas_item_i2w_affine(item:PGnomeCanvasItem; affine:Taffine_array);cdecl;external libgnomeuidll name 'gnome_canvas_item_i2w_affine';
procedure gnome_canvas_item_i2c_affine(item:PGnomeCanvasItem; affine:Taffine_array);cdecl;external libgnomeuidll name 'gnome_canvas_item_i2c_affine';
procedure gnome_canvas_item_reparent(item:PGnomeCanvasItem; new_group:PGnomeCanvasGroup);cdecl;external libgnomeuidll name 'gnome_canvas_item_reparent';
procedure gnome_canvas_item_grab_focus(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_grab_focus';
procedure gnome_canvas_item_get_bounds(item:PGnomeCanvasItem; x1:Pdouble; y1:Pdouble; x2:Pdouble; y2:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_item_get_bounds';
procedure gnome_canvas_item_request_update(item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_item_request_update';

function GNOME_TYPE_CANVAS_GROUP : TGTKType;
function GNOME_IS_CANVAS_GROUP(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_GROUP_CLASS(klass : Pointer) : Boolean;


function gnome_canvas_group_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_group_get_type';
procedure gnome_canvas_group_child_bounds(group:PGnomeCanvasGroup; item:PGnomeCanvasItem);cdecl;external libgnomeuidll name 'gnome_canvas_group_child_bounds';

const
   bm__GnomeCanvas_need_update = $1;
   bp__GnomeCanvas_need_update = 0;
   bm__GnomeCanvas_need_redraw = $2;
   bp__GnomeCanvas_need_redraw = 1;
   bm__GnomeCanvas_need_repick = $4;
   bp__GnomeCanvas_need_repick = 2;
   bm__GnomeCanvas_left_grabbed_item = $8;
   bp__GnomeCanvas_left_grabbed_item = 3;
   bm__GnomeCanvas_in_repick = $10;
   bp__GnomeCanvas_in_repick = 4;
   bm__GnomeCanvas_aa = $20;
   bp__GnomeCanvas_aa = 5;
   bm__GnomeCanvas_dither = $C0;
   bp__GnomeCanvas_dither = 6;
function need_update(var a : TGnomeCanvas) : dword;
procedure set_need_update(var a : TGnomeCanvas; __need_update : dword);
function need_redraw(var a : TGnomeCanvas) : dword;
procedure set_need_redraw(var a : TGnomeCanvas; __need_redraw : dword);
function need_repick(var a : TGnomeCanvas) : dword;
procedure set_need_repick(var a : TGnomeCanvas; __need_repick : dword);
function left_grabbed_item(var a : TGnomeCanvas) : dword;
procedure set_left_grabbed_item(var a : TGnomeCanvas; __left_grabbed_item : dword);
function in_repick(var a : TGnomeCanvas) : dword;
procedure set_in_repick(var a : TGnomeCanvas; __in_repick : dword);
function aa(var a : TGnomeCanvas) : dword;
procedure set_aa(var a : TGnomeCanvas; __aa : dword);
function dither(var a : TGnomeCanvas) : dword;
procedure set_dither(var a : TGnomeCanvas; __dither : dword);

function GNOME_TYPE_CANVAS : TGTKType;
function GNOME_IS_CANVAS(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_get_type';
function gnome_canvas_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_canvas_new';

{$ifndef GNOME_EXCLUDE_EXPERIMENTAL}
  function gnome_canvas_new_aa:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_canvas_new_aa';
{$endif}

function gnome_canvas_root(canvas:PGnomeCanvas):PGnomeCanvasGroup;cdecl;external libgnomeuidll name 'gnome_canvas_root';
procedure gnome_canvas_set_scroll_region(canvas:PGnomeCanvas; x1:double; y1:double; x2:double; y2:double);cdecl;external libgnomeuidll name 'gnome_canvas_set_scroll_region';
procedure gnome_canvas_get_scroll_region(canvas:PGnomeCanvas; x1:Pdouble; y1:Pdouble; x2:Pdouble; y2:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_get_scroll_region';
procedure gnome_canvas_set_pixels_per_unit(canvas:PGnomeCanvas; n:double);cdecl;external libgnomeuidll name 'gnome_canvas_set_pixels_per_unit';
procedure gnome_canvas_scroll_to(canvas:PGnomeCanvas; cx:longint; cy:longint);cdecl;external libgnomeuidll name 'gnome_canvas_scroll_to';
procedure gnome_canvas_get_scroll_offsets(canvas:PGnomeCanvas; cx:Plongint; cy:Plongint);cdecl;external libgnomeuidll name 'gnome_canvas_get_scroll_offsets';
procedure gnome_canvas_update_now(canvas:PGnomeCanvas);cdecl;external libgnomeuidll name 'gnome_canvas_update_now';
function gnome_canvas_get_item_at(canvas:PGnomeCanvas; x:double; y:double):PGnomeCanvasItem;cdecl;external libgnomeuidll name 'gnome_canvas_get_item_at';
procedure gnome_canvas_request_redraw_uta(canvas:PGnomeCanvas; uta:PArtUta);cdecl;external libgnomeuidll name 'gnome_canvas_request_redraw_uta';
procedure gnome_canvas_request_redraw(canvas:PGnomeCanvas; x1:longint; y1:longint; x2:longint; y2:longint);cdecl;external libgnomeuidll name 'gnome_canvas_request_redraw';
procedure gnome_canvas_w2c_affine(canvas:PGnomeCanvas; affine:Taffine_array);cdecl;external libgnomeuidll name 'gnome_canvas_w2c_affine';
procedure gnome_canvas_w2c(canvas:PGnomeCanvas; wx:double; wy:double; cx:Plongint; cy:Plongint);cdecl;external libgnomeuidll name 'gnome_canvas_w2c';
procedure gnome_canvas_w2c_d(canvas:PGnomeCanvas; wx:double; wy:double; cx:Pdouble; cy:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_w2c_d';
procedure gnome_canvas_c2w(canvas:PGnomeCanvas; cx:longint; cy:longint; wx:Pdouble; wy:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_c2w';
procedure gnome_canvas_window_to_world(canvas:PGnomeCanvas; winx:double; winy:double; worldx:Pdouble; worldy:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_window_to_world';
procedure gnome_canvas_world_to_window(canvas:PGnomeCanvas; worldx:double; worldy:double; winx:Pdouble; winy:Pdouble);cdecl;external libgnomeuidll name 'gnome_canvas_world_to_window';
function gnome_canvas_get_color(canvas:PGnomeCanvas; spec:Pchar; color:PGdkColor):longint;cdecl;external libgnomeuidll name 'gnome_canvas_get_color';
function gnome_canvas_get_color_pixel(canvas:PGnomeCanvas; rgba:guint):gulong;cdecl;external libgnomeuidll name 'gnome_canvas_get_color_pixel';
procedure gnome_canvas_set_stipple_origin(canvas:PGnomeCanvas; gc:PGdkGC);cdecl;external libgnomeuidll name 'gnome_canvas_set_stipple_origin';
procedure gnome_canvas_set_dither(canvas:PGnomeCanvas; dither:TGdkRgbDither);cdecl;external libgnomeuidll name 'gnome_canvas_set_dither';
function gnome_canvas_get_dither(canvas:PGnomeCanvas):TGdkRgbDither;cdecl;external libgnomeuidll name 'gnome_canvas_get_dither';

{$EndIf read_interface}

{$Ifdef read_implementation}

Function GNOME_CANVAS_COLOR(r, g, b : longint) : Longint;
begin
  result := (((r and $ff) shl 24) or ((g and $ff) shl 16) or ((b and $ff) shl 8) or $ff);
end;

Function GNOME_CANVAS_COLOR_A(r, g, b, a : longint)  : longint;
begin
  result := (((r and $ff) shl 24) or ((g and $ff) shl 16) or ((b and $ff) shl 8) or (a and $ff))
end;

function is_bg(var a : TGnomeCanvasBuf) : dword;
begin
   is_bg:=(a.flag0 and bm_GnomeCanvasBuf_is_bg) shr bp_GnomeCanvasBuf_is_bg;
end;

procedure set_is_bg(var a : TGnomeCanvasBuf; __is_bg : dword);
begin
   a.flag0:=a.flag0 or ((__is_bg shl bp_GnomeCanvasBuf_is_bg) and bm_GnomeCanvasBuf_is_bg);
end;

function is_buf(var a : TGnomeCanvasBuf) : dword;
begin
   is_buf:=(a.flag0 and bm_GnomeCanvasBuf_is_buf) shr bp_GnomeCanvasBuf_is_buf;
end;

procedure set_is_buf(var a : TGnomeCanvasBuf; __is_buf : dword);
begin
   a.flag0:=a.flag0 or ((__is_buf shl bp_GnomeCanvasBuf_is_buf) and bm_GnomeCanvasBuf_is_buf);
end;

function GNOME_TYPE_CANVAS_ITEM : TGTKType;
begin
  GNOME_TYPE_CANVAS_ITEM:=gnome_canvas_item_get_type;
end;

function GNOME_IS_CANVAS_ITEM(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_ITEM:=(obj<>nil) and GNOME_IS_CANVAS_ITEM_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_ITEM_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_ITEM_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_ITEM);
end;

function GNOME_TYPE_CANVAS_GROUP : TGTKType;
begin
  GNOME_TYPE_CANVAS_GROUP:=gnome_canvas_group_get_type;
end;

function GNOME_IS_CANVAS_GROUP(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_GROUP:=(obj<>nil) and GNOME_IS_CANVAS_GROUP_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_GROUP_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_GROUP_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_GROUP);
end;

function need_update(var a : TGnomeCanvas) : dword;
begin
   need_update:=(a.flag0 and bm__GnomeCanvas_need_update) shr bp__GnomeCanvas_need_update;
end;

procedure set_need_update(var a : TGnomeCanvas; __need_update : dword);
begin
   a.flag0:=a.flag0 or ((__need_update shl bp__GnomeCanvas_need_update) and bm__GnomeCanvas_need_update);
end;

function need_redraw(var a : TGnomeCanvas) : dword;
begin
   need_redraw:=(a.flag0 and bm__GnomeCanvas_need_redraw) shr bp__GnomeCanvas_need_redraw;
end;

procedure set_need_redraw(var a : TGnomeCanvas; __need_redraw : dword);
begin
   a.flag0:=a.flag0 or ((__need_redraw shl bp__GnomeCanvas_need_redraw) and bm__GnomeCanvas_need_redraw);
end;

function need_repick(var a : TGnomeCanvas) : dword;
begin
   need_repick:=(a.flag0 and bm__GnomeCanvas_need_repick) shr bp__GnomeCanvas_need_repick;
end;

procedure set_need_repick(var a : TGnomeCanvas; __need_repick : dword);
begin
   a.flag0:=a.flag0 or ((__need_repick shl bp__GnomeCanvas_need_repick) and bm__GnomeCanvas_need_repick);
end;

function left_grabbed_item(var a : TGnomeCanvas) : dword;
begin
   left_grabbed_item:=(a.flag0 and bm__GnomeCanvas_left_grabbed_item) shr bp__GnomeCanvas_left_grabbed_item;
end;

procedure set_left_grabbed_item(var a : TGnomeCanvas; __left_grabbed_item : dword);
begin
   a.flag0:=a.flag0 or ((__left_grabbed_item shl bp__GnomeCanvas_left_grabbed_item) and bm__GnomeCanvas_left_grabbed_item);
end;

function in_repick(var a : TGnomeCanvas) : dword;
begin
   in_repick:=(a.flag0 and bm__GnomeCanvas_in_repick) shr bp__GnomeCanvas_in_repick;
end;

procedure set_in_repick(var a : TGnomeCanvas; __in_repick : dword);
begin
   a.flag0:=a.flag0 or ((__in_repick shl bp__GnomeCanvas_in_repick) and bm__GnomeCanvas_in_repick);
end;

function aa(var a : TGnomeCanvas) : dword;
begin
   aa:=(a.flag0 and bm__GnomeCanvas_aa) shr bp__GnomeCanvas_aa;
end;

procedure set_aa(var a : TGnomeCanvas; __aa : dword);
begin
   a.flag0:=a.flag0 or ((__aa shl bp__GnomeCanvas_aa) and bm__GnomeCanvas_aa);
end;

function dither(var a : TGnomeCanvas) : dword;
begin
   dither:=(a.flag0 and bm__GnomeCanvas_dither) shr bp__GnomeCanvas_dither;
end;

procedure set_dither(var a : TGnomeCanvas; __dither : dword);
begin
   a.flag0:=a.flag0 or ((__dither shl bp__GnomeCanvas_dither) and bm__GnomeCanvas_dither);
end;

function GNOME_TYPE_CANVAS : TGTKType;
begin
  GNOME_TYPE_CANVAS:=gnome_canvas_get_type;
end;

function GNOME_IS_CANVAS(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS:=(obj<>nil) and GNOME_IS_CANVAS_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS);
end;

{$Endif read_implementation}
