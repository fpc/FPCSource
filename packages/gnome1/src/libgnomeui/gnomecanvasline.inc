{$IfDef read_interface}

type
   PGnomeCanvasLine = ^TGnomeCanvasLine;
   TGnomeCanvasLine = record
        item : TGnomeCanvasItem;
        num_points : longint;
        coords : Pdouble;
        width : double;
        fill_color : guint;
        fill_pixel : gulong;
        stipple : PGdkBitmap;
        cap : TGdkCapStyle;
        join : TGdkJoinStyle;
        line_style : TGdkLineStyle;
        shape_a : double;
        shape_b : double;
        shape_c : double;
        first_coords : Pdouble;
        last_coords : Pdouble;
        spline_steps : longint;
        gc : PGdkGC;
        flag0 : word;
        fill_rgba : guint32;
        fill_svp : PArtSVP;
        first_svp : PArtSVP;
        last_svp : PArtSVP;
     end;
   GNOME_CANVAS_LINE = PGnomeCanvasLine;

const
   bm__GnomeCanvasLine_width_pixels = $1;
   bp__GnomeCanvasLine_width_pixels = 0;
   bm__GnomeCanvasLine_first_arrow = $2;
   bp__GnomeCanvasLine_first_arrow = 1;
   bm__GnomeCanvasLine_last_arrow = $4;
   bp__GnomeCanvasLine_last_arrow = 2;
   bm__GnomeCanvasLine_smooth = $8;
   bp__GnomeCanvasLine_smooth = 3;
function width_pixels(var a : TGnomeCanvasLine) : guint;
procedure set_width_pixels(var a : TGnomeCanvasLine; __width_pixels : guint);
function first_arrow(var a : TGnomeCanvasLine) : guint;
procedure set_first_arrow(var a : TGnomeCanvasLine; __first_arrow : guint);
function last_arrow(var a : TGnomeCanvasLine) : guint;
procedure set_last_arrow(var a : TGnomeCanvasLine; __last_arrow : guint);
function smooth(var a : TGnomeCanvasLine) : guint;
procedure set_smooth(var a : TGnomeCanvasLine; __smooth : guint);
type
   PGnomeCanvasLineClass = ^TGnomeCanvasLineClass;
   TGnomeCanvasLineClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_LINE_CLASS = PGnomeCanvasLineClass;

function GNOME_TYPE_CANVAS_LINE : TGTKType;
function GNOME_IS_CANVAS_LINE(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_LINE_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_line_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_line_get_type';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CANVAS_LINE : TGTKType;
begin
  GNOME_TYPE_CANVAS_LINE:=gnome_canvas_line_get_type;
end;

function GNOME_IS_CANVAS_LINE(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_LINE:=(obj<>nil) and GNOME_IS_CANVAS_LINE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_LINE_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_LINE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_LINE);
end;

function width_pixels(var a : TGnomeCanvasLine) : guint;
begin
   width_pixels:=(a.flag0 and bm__GnomeCanvasLine_width_pixels) shr bp__GnomeCanvasLine_width_pixels;
end;

procedure set_width_pixels(var a : TGnomeCanvasLine; __width_pixels : guint);
begin
   a.flag0:=a.flag0 or ((__width_pixels shl bp__GnomeCanvasLine_width_pixels) and bm__GnomeCanvasLine_width_pixels);
end;

function first_arrow(var a : TGnomeCanvasLine) : guint;
begin
   first_arrow:=(a.flag0 and bm__GnomeCanvasLine_first_arrow) shr bp__GnomeCanvasLine_first_arrow;
end;

procedure set_first_arrow(var a : TGnomeCanvasLine; __first_arrow : guint);
begin
   a.flag0:=a.flag0 or ((__first_arrow shl bp__GnomeCanvasLine_first_arrow) and bm__GnomeCanvasLine_first_arrow);
end;

function last_arrow(var a : TGnomeCanvasLine) : guint;
begin
   last_arrow:=(a.flag0 and bm__GnomeCanvasLine_last_arrow) shr bp__GnomeCanvasLine_last_arrow;
end;

procedure set_last_arrow(var a : TGnomeCanvasLine; __last_arrow : guint);
begin
   a.flag0:=a.flag0 or ((__last_arrow shl bp__GnomeCanvasLine_last_arrow) and bm__GnomeCanvasLine_last_arrow);
end;

function smooth(var a : TGnomeCanvasLine) : guint;
begin
   smooth:=(a.flag0 and bm__GnomeCanvasLine_smooth) shr bp__GnomeCanvasLine_smooth;
end;

procedure set_smooth(var a : TGnomeCanvasLine; __smooth : guint);
begin
   a.flag0:=a.flag0 or ((__smooth shl bp__GnomeCanvasLine_smooth) and bm__GnomeCanvasLine_smooth);
end;

{$Endif read_implementation}
