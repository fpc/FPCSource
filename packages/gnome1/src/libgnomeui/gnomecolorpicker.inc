{$IfDef read_interface}
type
   PGnomeColorPicker = ^TGnomeColorPicker;
   TGnomeColorPicker = record
        button : TGtkButton;
        r : gdouble;
        g : gdouble;
        b : gdouble;
        a : gdouble;
        image : PGdkImlibImage;
        pixmap : PGdkPixmap;
        gc : PGdkGC;
        da : PGtkWidget;
        cs_dialog : PGtkWidget;
        title : Pgchar;
        flag0 : word;
     end;
   GNOME_COLOR_PICKER = PGnomeColorPicker;

const
   bm__GnomeColorPicker_dither = $1;
   bp__GnomeColorPicker_dither = 0;
   bm__GnomeColorPicker_use_alpha = $2;
   bp__GnomeColorPicker_use_alpha = 1;
function dither(var a :TGnomeColorPicker) : guint;
procedure set_dither(var a :TGnomeColorPicker; __dither : guint);
function use_alpha(var a :TGnomeColorPicker) : guint;
procedure set_use_alpha(var a :TGnomeColorPicker; __use_alpha : guint);

type
   PGnomeColorPickerClass = ^TGnomeColorPickerClass;
   TGnomeColorPickerClass = record
        parent_class : TGtkButtonClass;
        color_set : procedure (cp:PGnomeColorPicker; r:guint; g:guint; b:guint; a:guint);cdecl;
     end;
   GNOME_COLOR_PICKER_CLASS = PGnomeColorPickerClass;


function GNOME_TYPE_COLOR_PICKER : TGTKType;
function GNOME_IS_COLOR_PICKER(obj : Pointer) : Boolean;
function GNOME_IS_COLOR_PICKER_CLASS(klass : Pointer) : Boolean;

function gnome_color_picker_get_type:TGtkType;cdecl;external libgnomeuidll name 'gnome_color_picker_get_type';
function gnome_color_picker_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_color_picker_new';
procedure gnome_color_picker_set_d(cp:PGnomeColorPicker; r:gdouble; g:gdouble; b:gdouble; a:gdouble);cdecl;external libgnomeuidll name 'gnome_color_picker_set_d';
procedure gnome_color_picker_get_d(cp:PGnomeColorPicker; r:Pgdouble; g:Pgdouble; b:Pgdouble; a:Pgdouble);cdecl;external libgnomeuidll name 'gnome_color_picker_get_d';
procedure gnome_color_picker_set_i8(cp:PGnomeColorPicker; r:guint8; g:guint8; b:guint8; a:guint8);cdecl;external libgnomeuidll name 'gnome_color_picker_set_i8';
procedure gnome_color_picker_get_i8(cp:PGnomeColorPicker; r:Pguint8; g:Pguint8; b:Pguint8; a:Pguint8);cdecl;external libgnomeuidll name 'gnome_color_picker_get_i8';
procedure gnome_color_picker_set_i16(cp:PGnomeColorPicker; r:gushort; g:gushort; b:gushort; a:gushort);cdecl;external libgnomeuidll name 'gnome_color_picker_set_i16';
procedure gnome_color_picker_get_i16(cp:PGnomeColorPicker; r:Pgushort; g:Pgushort; b:Pgushort; a:Pgushort);cdecl;external libgnomeuidll name 'gnome_color_picker_get_i16';
procedure gnome_color_picker_set_dither(cp:PGnomeColorPicker; dither:gboolean);cdecl;external libgnomeuidll name 'gnome_color_picker_set_dither';
procedure gnome_color_picker_set_use_alpha(cp:PGnomeColorPicker; use_alpha:gboolean);cdecl;external libgnomeuidll name 'gnome_color_picker_set_use_alpha';
procedure gnome_color_picker_set_title(cp:PGnomeColorPicker; title:Pgchar);cdecl;external libgnomeuidll name 'gnome_color_picker_set_title';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_COLOR_PICKER : TGTKType;
begin
  GNOME_TYPE_COLOR_PICKER:=gnome_color_picker_get_type;
end;

function GNOME_IS_COLOR_PICKER(obj : Pointer) : Boolean;
begin
   GNOME_IS_COLOR_PICKER:=(obj<>nil) and GNOME_IS_COLOR_PICKER_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_COLOR_PICKER_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_COLOR_PICKER_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_COLOR_PICKER);
end;

function dither(var a :TGnomeColorPicker) : guint;
begin
   dither:=(a.flag0 and bm__GnomeColorPicker_dither) shr bp__GnomeColorPicker_dither;
end;

procedure set_dither(var a :TGnomeColorPicker; __dither : guint);
begin
   a.flag0:=a.flag0 or ((__dither shl bp__GnomeColorPicker_dither) and bm__GnomeColorPicker_dither);
end;

function use_alpha(var a :TGnomeColorPicker) : guint;
begin
   use_alpha:=(a.flag0 and bm__GnomeColorPicker_use_alpha) shr bp__GnomeColorPicker_use_alpha;
end;

procedure set_use_alpha(var a :TGnomeColorPicker; __use_alpha : guint);
begin
   a.flag0:=a.flag0 or ((__use_alpha shl bp__GnomeColorPicker_use_alpha) and bm__GnomeColorPicker_use_alpha);
end;

{$Endif read_implementation}
