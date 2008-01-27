{$IfDef read_interface}

type
   PGnomeCanvasPolygon = ^TGnomeCanvasPolygon;
   TGnomeCanvasPolygon = record
        item : TGnomeCanvasItem;
        num_points : longint;
        coords : Pdouble;
        width : double;
        fill_color : guint;
        outline_color : guint;
        fill_pixel : gulong;
        outline_pixel : gulong;
        fill_stipple : PGdkBitmap;
        outline_stipple : PGdkBitmap;
        fill_gc : PGdkGC;
        outline_gc : PGdkGC;
        flag0 : word;
        fill_rgba : guint32;
        fill_svp : PArtSVP;
        outline_rgba : guint32;
        outline_svp : PArtSVP;
     end;
   GNOME_CANVAS_POLYGON = PGnomeCanvasPolygon;

const
   bm__GnomeCanvasPolygon_fill_set = $1;
   bp__GnomeCanvasPolygon_fill_set = 0;
   bm__GnomeCanvasPolygon_outline_set = $2;
   bp__GnomeCanvasPolygon_outline_set = 1;
   bm__GnomeCanvasPolygon_width_pixels = $4;
   bp__GnomeCanvasPolygon_width_pixels = 2;

function fill_set(var a : TGnomeCanvasPolygon) : guint;
procedure set_fill_set(var a : TGnomeCanvasPolygon; __fill_set : guint);
function outline_set(var a : TGnomeCanvasPolygon) : guint;
procedure set_outline_set(var a : TGnomeCanvasPolygon; __outline_set : guint);
function width_pixels(var a : TGnomeCanvasPolygon) : guint;
procedure set_width_pixels(var a : TGnomeCanvasPolygon; __width_pixels : guint);

type
   PGnomeCanvasPolygonClass = ^TGnomeCanvasPolygonClass;
   TGnomeCanvasPolygonClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_POLYGON_CLASS = PGnomeCanvasPolygonClass;

function GNOME_TYPE_CANVAS_POLYGON : TGTKType;
function GNOME_IS_CANVAS_POLYGON(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_POLYGON_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_polygon_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_polygon_get_type';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CANVAS_POLYGON : TGTKType;
begin
  GNOME_TYPE_CANVAS_POLYGON:=gnome_canvas_polygon_get_type;
end;

function GNOME_IS_CANVAS_POLYGON(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_POLYGON:=(obj<>nil) and GNOME_IS_CANVAS_POLYGON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_POLYGON_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_POLYGON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_POLYGON);
end;

function fill_set(var a : TGnomeCanvasPolygon) : guint;
begin
   fill_set:=(a.flag0 and bm__GnomeCanvasPolygon_fill_set) shr bp__GnomeCanvasPolygon_fill_set;
end;

procedure set_fill_set(var a : TGnomeCanvasPolygon; __fill_set : guint);
begin
   a.flag0:=a.flag0 or ((__fill_set shl bp__GnomeCanvasPolygon_fill_set) and bm__GnomeCanvasPolygon_fill_set);
end;

function outline_set(var a : TGnomeCanvasPolygon) : guint;
begin
   outline_set:=(a.flag0 and bm__GnomeCanvasPolygon_outline_set) shr bp__GnomeCanvasPolygon_outline_set;
end;

procedure set_outline_set(var a : TGnomeCanvasPolygon; __outline_set : guint);
begin
   a.flag0:=a.flag0 or ((__outline_set shl bp__GnomeCanvasPolygon_outline_set) and bm__GnomeCanvasPolygon_outline_set);
end;

function width_pixels(var a : TGnomeCanvasPolygon) : guint;
begin
   width_pixels:=(a.flag0 and bm__GnomeCanvasPolygon_width_pixels) shr bp__GnomeCanvasPolygon_width_pixels;
end;

procedure set_width_pixels(var a : TGnomeCanvasPolygon; __width_pixels : guint);
begin
   a.flag0:=a.flag0 or ((__width_pixels shl bp__GnomeCanvasPolygon_width_pixels) and bm__GnomeCanvasPolygon_width_pixels);
end;

{$Endif read_implementation}
