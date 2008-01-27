{$IfDef read_interface}

type
   PGnomeCanvasTextSuckChar = ^TGnomeCanvasTextSuckChar;
   TGnomeCanvasTextSuckChar = record
        left_sb : longint;
        right_sb : longint;
        width : longint;
        ascent : longint;
        descent : longint;
        bitmap_offset : longint;
     end;

   PGnomeCanvasTextSuckFont = ^TGnomeCanvasTextSuckFont;
   TGnomeCanvasTextSuckFont = record
        bitmap : Pguchar;
        bitmap_width : gint;
        bitmap_height : gint;
        ascent : gint;
        chars : array[0..255] of TGnomeCanvasTextSuckChar;
     end;

   PGnomeCanvasText = ^TGnomeCanvasText;
   TGnomeCanvasText = record
        item : TGnomeCanvasItem;
        thetext : Pchar;
        lines : gpointer;
        num_lines : longint;
        x : double;
        y : double;
        font : PGdkFont;
        anchor : TGtkAnchorType;
        justification : TGtkJustification;
        clip_width : double;
        clip_height : double;
        xofs : double;
        yofs : double;
        pixel : gulong;
        stipple : PGdkBitmap;
        gc : PGdkGC;
        cx : longint;
        cy : longint;
        clip_cx : longint;
        clip_cy : longint;
        clip_cwidth : longint;
        clip_cheight : longint;
        max_width : longint;
        height : longint;
        flag0 : word;
        suckfont : PGnomeCanvasTextSuckFont;
        rgba : guint32;
        affine : Taffine_array;
     end;
   GNOME_CANVAS_TEXT = PGnomeCanvasText;

const
   bm__GnomeCanvasText_clip = $1;
   bp__GnomeCanvasText_clip = 0;
function clip(var a : TGnomeCanvasText) : guint;
procedure set_clip(var a : TGnomeCanvasText; __clip : guint);
type
   PGnomeCanvasTextClass = ^TGnomeCanvasTextClass;
   TGnomeCanvasTextClass = record
        parent_class : TGnomeCanvasItemClass;
     end;
   GNOME_CANVAS_TEXT_CLASS = PGnomeCanvasTextClass;

function GNOME_TYPE_CANVAS_TEXT : TGTKType;
function GNOME_IS_CANVAS_TEXT(obj : Pointer) : Boolean;
function GNOME_IS_CANVAS_TEXT_CLASS(klass : Pointer) : Boolean;

function gnome_canvas_text_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_canvas_text_get_type';

{$EndIf read_interface}

{$Ifdef read_implementation}

function clip(var a : TGnomeCanvasText) : guint;
begin
   clip:=(a.flag0 and bm__GnomeCanvasText_clip) shr bp__GnomeCanvasText_clip;
end;

procedure set_clip(var a : TGnomeCanvasText; __clip : guint);
begin
   a.flag0:=a.flag0 or ((__clip shl bp__GnomeCanvasText_clip) and bm__GnomeCanvasText_clip);
end;

function GNOME_TYPE_CANVAS_TEXT : TGTKType;
begin
  GNOME_TYPE_CANVAS_TEXT:=gnome_canvas_text_get_type;
end;

function GNOME_IS_CANVAS_TEXT(obj : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_TEXT:=(obj<>nil) and GNOME_IS_CANVAS_TEXT_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_CANVAS_TEXT_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_CANVAS_TEXT_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_CANVAS_TEXT);
end;

{$Endif read_implementation}
