{$IfDef read_interface}
type
   PGnomePixmap = ^TGnomePixmap;
   TGnomePixmap = record
        widget : TGtkWidget;
        pixmap : PGdkPixmap;
        mask : PGdkBitmap;
     end;
   GNOME_PIXMAP = PGnomePixmap;

   PGnomePixmapClass = ^TGnomePixmapClass;
   TGnomePixmapClass = record
        parent_class : TGtkWidgetClass;
     end;
   GNOME_PIXMAP_CLASS = PGnomePixmapClass;

function GNOME_TYPE_PIXMAP : TGtkType;

function GNOME_IS_PIXMAP(obj : pointer) : gboolean;
function GNOME_IS_PIXMAP_CLASS(klass : pointer) : gboolean;

function gnome_pixmap_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_pixmap_get_type';
function gnome_pixmap_new_from_file(filename:Pchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_file';
function gnome_pixmap_new_from_file_at_size(filename:Pchar; width:longint; height:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_file_at_size';
function gnome_pixmap_new_from_xpm_d(xpm_data:PPchar):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_xpm_d';
function gnome_pixmap_new_from_xpm_d_at_size(xpm_data:PPchar; width:longint; height:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_xpm_d_at_size';
function gnome_pixmap_new_from_rgb_d(data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_rgb_d';
function gnome_pixmap_new_from_rgb_d_shaped(data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint; shape_color:PGdkImlibColor):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_rgb_d_shaped';
function gnome_pixmap_new_from_rgb_d_shaped_at_size(data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint; width:longint;
           height:longint; shape_color:PGdkImlibColor):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_rgb_d_shaped_at_size';
function gnome_pixmap_new_from_rgb_d_at_size(data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint; width:longint;
           height:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_rgb_d_at_size';
function gnome_pixmap_new_from_gnome_pixmap(gpixmap:PGnomePixmap):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_gnome_pixmap';
function gnome_pixmap_new_from_imlib(image:PGdkImlibImage):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_imlib';
function gnome_pixmap_new_from_imlib_at_size(image:PGdkImlibImage; width:longint; height:longint):PGtkWidget;cdecl;external libgnomeuidll name 'gnome_pixmap_new_from_imlib_at_size';
procedure gnome_pixmap_load_file(gpixmap:PGnomePixmap; filename:Pchar);cdecl;external libgnomeuidll name 'gnome_pixmap_load_file';
procedure gnome_pixmap_load_file_at_size(gpixmap:PGnomePixmap; filename:Pchar; width:longint; height:longint);cdecl;external libgnomeuidll name 'gnome_pixmap_load_file_at_size';
procedure gnome_pixmap_load_xpm_d(gpixmap:PGnomePixmap; xpm_data:PPchar);cdecl;external libgnomeuidll name 'gnome_pixmap_load_xpm_d';
procedure gnome_pixmap_load_xpm_d_at_size(gpixmap:PGnomePixmap; xpm_data:PPchar; width:longint; height:longint);cdecl;external libgnomeuidll name 'gnome_pixmap_load_xpm_d_at_size';
procedure gnome_pixmap_load_rgb_d(gpixmap:PGnomePixmap; data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint);cdecl;external libgnomeuidll name 'gnome_pixmap_load_rgb_d';
procedure gnome_pixmap_load_rgb_d_shaped(gpixmap:PGnomePixmap; data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint;
            shape_color:PGdkImlibColor);cdecl;external libgnomeuidll name 'gnome_pixmap_load_rgb_d_shaped';
procedure gnome_pixmap_load_rgb_d_shaped_at_size(gpixmap:PGnomePixmap; data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint;
            width:longint; height:longint; shape_color:PGdkImlibColor);cdecl;external libgnomeuidll name 'gnome_pixmap_load_rgb_d_shaped_at_size';
procedure gnome_pixmap_load_rgb_d_at_size(gpixmap:PGnomePixmap; data:Pbyte; alpha:Pbyte; rgb_width:longint; rgb_height:longint;
            width:longint; height:longint);cdecl;external libgnomeuidll name 'gnome_pixmap_load_rgb_d_at_size';
procedure gnome_pixmap_load_imlib(gpixmap:PGnomePixmap; image:PGdkImlibImage);cdecl;external libgnomeuidll name 'gnome_pixmap_load_imlib';
procedure gnome_pixmap_load_imlib_at_size(gpixmap:PGnomePixmap; image:PGdkImlibImage; width:longint; height:longint);cdecl;external libgnomeuidll name 'gnome_pixmap_load_imlib_at_size';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_PIXMAP : TGTKType;
begin
  GNOME_TYPE_PIXMAP:=gnome_pixmap_get_type;
end;

function GNOME_IS_PIXMAP(obj : Pointer) : gBoolean;
begin
   GNOME_IS_PIXMAP:=(obj<>nil) and GNOME_IS_PIXMAP_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_PIXMAP_CLASS(klass : Pointer) : gBoolean;
begin
   GNOME_IS_PIXMAP_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_PIXMAP);
end;

{$Endif read_implementation}
