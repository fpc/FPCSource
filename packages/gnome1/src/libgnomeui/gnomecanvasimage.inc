{$IfDef read_interface}

type
   PGnomeCanvasImage = ^TGnomeCanvasImage;
   TGnomeCanvasImage = record
        item : TGnomeCanvasItem;
        image : PGdkImlibImage;
        pixmap : PGdkPixmap;
        mask : PGdkBitmap;
        x : double;
        y : double;
        width : double;
        height : double;
        anchor : TGtkAnchorType;
        cx : longint;
        cy : longint;
        cwidth : longint;
        cheight : longint;
        gc : PGdkGC;
        flag0 : word;
        pixbuf : PArtPixBuf;
        affine : Taffine_array;
     end;
   GNOME_CANVAS_IMAGE = PGnomeCanvasImage;

const
   bm__GnomeCanvasImage_need_recalc = $1;
   bp__GnomeCanvasImage_need_recalc = 0;

function need_recalc(var a : TGnomeCanvasImage) : dword;
procedure set_need_recalc(var a : TGnomeCanvasImage; __need_recalc : dword);

type
   PGnomeCanvasImageClass = ^TGnomeCanvasImageClass;
   TGnomeCanvasImageClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_IMAGE_CLASS = PGnomeCanvasImageClass;

function GNOME_TYPE_CANVAS_IMAGE : TGTKType;
function GNOME_IS_CANVAS_IMAGE(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_IMAGE_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_image_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_image_get_type';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_CANVAS_IMAGE : TGTKType;
begin
  GNOME_TYPE_CANVAS_IMAGE:=gnome_canvas_image_get_type;
end;

function GNOME_IS_CANVAS_IMAGE(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_IMAGE:=(obj<>nil) and GNOME_IS_CANVAS_IMAGE_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_IMAGE_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_IMAGE_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_IMAGE);
end;

function need_recalc(var a : TGnomeCanvasImage) : dword;
begin
   need_recalc:=(a.flag0 and bm__GnomeCanvasImage_need_recalc) shr bp__GnomeCanvasImage_need_recalc;
end;

procedure set_need_recalc(var a : TGnomeCanvasImage; __need_recalc : dword);
begin
   a.flag0:=a.flag0 or ((__need_recalc shl bp__GnomeCanvasImage_need_recalc) and bm__GnomeCanvasImage_need_recalc);
end;

{$Endif read_implementation}
