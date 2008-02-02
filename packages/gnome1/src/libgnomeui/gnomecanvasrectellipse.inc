{$IfDef read_interface}

type
   PGnomeCanvasRE = ^TGnomeCanvasRE;
   TGnomeCanvasRE = record
        item : TGnomeCanvasItem;
        x1 : double;
        y1 : double;
        x2 : double;
        y2 : double;
        width : double;
        fill_color : guint;
        outline_color : guint;
        fill_pixel : gulong;
        outline_pixel : gulong;
        fill_stipple : PGdkBitmap;
        outline_stipple : PGdkBitmap;
        fill_gc : PGdkGC;
        outline_gc : PGdkGC;
        fill_svp : PArtSVP;
        outline_svp : PArtSVP;
        flag0 : word;
     end;
   GNOME_CANVAS_RE = PGnomeCanvasRE;

const
   bm__GnomeCanvasRE_fill_set = $1;
   bp__GnomeCanvasRE_fill_set = 0;
   bm__GnomeCanvasRE_outline_set = $2;
   bp__GnomeCanvasRE_outline_set = 1;
   bm__GnomeCanvasRE_width_pixels = $4;
   bp__GnomeCanvasRE_width_pixels = 2;
function fill_set(var a : TGnomeCanvasRE) : dword;
procedure set_fill_set(var a : TGnomeCanvasRE; __fill_set : dword);
function outline_set(var a : TGnomeCanvasRE) : dword;
procedure set_outline_set(var a : TGnomeCanvasRE; __outline_set : dword);
function width_pixels(var a : TGnomeCanvasRE) : dword;
procedure set_width_pixels(var a : TGnomeCanvasRE; __width_pixels : dword);

type
   PGnomeCanvasREClass = ^TGnomeCanvasREClass;
   TGnomeCanvasREClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_RE_CLASS = PGnomeCanvasREClass;

function GNOME_TYPE_CANVAS_RE : TGTKType;
function GNOME_IS_CANVAS_RE(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_RE_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_re_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_re_get_type';

type
   PGnomeCanvasRect = ^TGnomeCanvasRect;
   TGnomeCanvasRect = record
        rect : TGnomeCanvasRE;
     end;
   GNOME_CANVAS_RECT = PGnomeCanvasRect;

   PGnomeCanvasRectClass = ^TGnomeCanvasRectClass;
   TGnomeCanvasRectClass = record
        parent_class : TGnomeCanvasREClass;
     end;
   GNOME_CANVAS_RECT_CLASS = PGnomeCanvasRectClass;

function GNOME_TYPE_CANVAS_RECT : TGTKType;
function GNOME_IS_CANVAS_RECT(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_RECT_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_rect_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_rect_get_type';

type
   PGnomeCanvasEllipse = ^TGnomeCanvasEllipse;
   TGnomeCanvasEllipse = record
        rect : TGnomeCanvasRE;
     end;
   GNOME_CANVAS_ELLIPSE = PGnomeCanvasEllipse;

   PGnomeCanvasEllipseClass = ^TGnomeCanvasEllipseClass;
   TGnomeCanvasEllipseClass = record
        parent_class : TGnomeCanvasREClass;
     end;
   GNOME_CANVAS_ELLIPSE_CLASS = PGnomeCanvasEllipseClass;

function GNOME_TYPE_CANVAS_ELLIPSE : TGTKType;
function GNOME_IS_CANVAS_ELLIPSE(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_ELLIPSE_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_ellipse_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_ellipse_get_type';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CANVAS_RE : TGTKType;
begin
  GNOME_TYPE_CANVAS_RE:=gnome_canvas_re_get_type;
end;

function GNOME_IS_CANVAS_RE(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_RE:=(obj<>nil) and GNOME_IS_CANVAS_RE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_RE_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_RE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_RE);
end;

function fill_set(var a : TGnomeCanvasRE) : dword;
begin
   fill_set:=(a.flag0 and bm__GnomeCanvasRE_fill_set) shr bp__GnomeCanvasRE_fill_set;
end;

procedure set_fill_set(var a : TGnomeCanvasRE; __fill_set : dword);
begin
   a.flag0:=a.flag0 or ((__fill_set shl bp__GnomeCanvasRE_fill_set) and bm__GnomeCanvasRE_fill_set);
end;

function outline_set(var a : TGnomeCanvasRE) : dword;
begin
   outline_set:=(a.flag0 and bm__GnomeCanvasRE_outline_set) shr bp__GnomeCanvasRE_outline_set;
end;

procedure set_outline_set(var a : TGnomeCanvasRE; __outline_set : dword);
begin
   a.flag0:=a.flag0 or ((__outline_set shl bp__GnomeCanvasRE_outline_set) and bm__GnomeCanvasRE_outline_set);
end;

function width_pixels(var a : TGnomeCanvasRE) : dword;
begin
   width_pixels:=(a.flag0 and bm__GnomeCanvasRE_width_pixels) shr bp__GnomeCanvasRE_width_pixels;
end;

procedure set_width_pixels(var a : TGnomeCanvasRE; __width_pixels : dword);
begin
   a.flag0:=a.flag0 or ((__width_pixels shl bp__GnomeCanvasRE_width_pixels) and bm__GnomeCanvasRE_width_pixels);
end;

function GNOME_TYPE_CANVAS_RECT : TGTKType;
begin
  GNOME_TYPE_CANVAS_RECT:=gnome_canvas_rect_get_type;
end;

function GNOME_IS_CANVAS_RECT(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_RECT:=(obj<>nil) and GNOME_IS_CANVAS_RECT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_RECT_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_RECT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_RECT);
end;

function GNOME_TYPE_CANVAS_ELLIPSE : TGTKType;
begin
  GNOME_TYPE_CANVAS_ELLIPSE:=gnome_canvas_ellipse_get_type;
end;

function GNOME_IS_CANVAS_ELLIPSE(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_ELLIPSE:=(obj<>nil) and GNOME_IS_CANVAS_ELLIPSE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_ELLIPSE_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_ELLIPSE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_ELLIPSE);
end;

{$Endif read_implementation}
