{$IfDef read_interface}

type
   PGnomeCanvasWidget = ^TGnomeCanvasWidget;
   TGnomeCanvasWidget = record
        item : TGnomeCanvasItem;
        widget : PGtkWidget;
        x : double;
        y : double;
        width : double;
        height : double;
        anchor : TGtkAnchorType;
        cx : longint;
        cy : longint;
        cwidth : longint;
        cheight : longint;
        destroy_id : guint;
        flag0 : word;
     end;
   GNOME_CANVAS_WIDGET = PGnomeCanvasWidget;

const
   bm__GnomeCanvasWidget_size_pixels = $1;
   bp__GnomeCanvasWidget_size_pixels = 0;
   bm__GnomeCanvasWidget_in_destroy = $2;
   bp__GnomeCanvasWidget_in_destroy = 1;
function size_pixels(var a : TGnomeCanvasWidget) : guint;
procedure set_size_pixels(var a : TGnomeCanvasWidget; __size_pixels : guint);
function in_destroy(var a : TGnomeCanvasWidget) : guint;
procedure set_in_destroy(var a : TGnomeCanvasWidget; __in_destroy : guint);
type
   PGnomeCanvasWidgetClass = ^TGnomeCanvasWidgetClass;
   TGnomeCanvasWidgetClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_WIDGET_CLASS = PGnomeCanvasWidgetClass;

function GNOME_TYPE_CANVAS_WIDGET : TGTKType;
function GNOME_IS_CANVAS_WIDGET(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_WIDGET_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_widget_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_widget_get_type';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CANVAS_WIDGET : TGTKType;
begin
  GNOME_TYPE_CANVAS_WIDGET:=gnome_canvas_widget_get_type;
end;

function GNOME_IS_CANVAS_WIDGET(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_WIDGET:=(obj<>nil) and GNOME_IS_CANVAS_WIDGET_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_WIDGET_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_WIDGET_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_WIDGET);
end;

function size_pixels(var a : TGnomeCanvasWidget) : guint;
begin
   size_pixels:=(a.flag0 and bm__GnomeCanvasWidget_size_pixels) shr bp__GnomeCanvasWidget_size_pixels;
end;

procedure set_size_pixels(var a : TGnomeCanvasWidget; __size_pixels : guint);
begin
   a.flag0:=a.flag0 or ((__size_pixels shl bp__GnomeCanvasWidget_size_pixels) and bm__GnomeCanvasWidget_size_pixels);
end;

function in_destroy(var a : TGnomeCanvasWidget) : guint;
begin
   in_destroy:=(a.flag0 and bm__GnomeCanvasWidget_in_destroy) shr bp__GnomeCanvasWidget_in_destroy;
end;

procedure set_in_destroy(var a : TGnomeCanvasWidget; __in_destroy : guint);
begin
   a.flag0:=a.flag0 or ((__in_destroy shl bp__GnomeCanvasWidget_in_destroy) and bm__GnomeCanvasWidget_in_destroy);
end;

{$Endif read_implementation}
