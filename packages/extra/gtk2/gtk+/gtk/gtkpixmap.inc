// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkPixmap = ^TGtkPixmap;
   TGtkPixmap = record
        misc : TGtkMisc;
        pixmap : PGdkPixmap;
        mask : PGdkBitmap;
        pixmap_insensitive : PGdkPixmap;
        flag0 : word;
     end;

   PGtkPixmapClass = ^TGtkPixmapClass;
   TGtkPixmapClass = record
        parent_class : TGtkMiscClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   bm_TGtkPixmap_build_insensitive = $1;
   bp_TGtkPixmap_build_insensitive = 0;


function GTK_TYPE_PIXMAP : GType;
function GTK_PIXMAP(obj: pointer) : PGtkPixmap;
function GTK_PIXMAP_CLASS(klass: pointer) : PGtkPixmapClass;
function GTK_IS_PIXMAP(obj: pointer) : boolean;
function GTK_IS_PIXMAP_CLASS(klass: pointer) : boolean;
function GTK_PIXMAP_GET_CLASS(obj: pointer) : PGtkPixmapClass;

function build_insensitive(var a : TGtkPixmap) : guint;
procedure set_build_insensitive(var a : TGtkPixmap; __build_insensitive : guint);

function gtk_pixmap_get_type:TGtkType; cdecl; external gtklib;
function gtk_pixmap_new(pixmap:PGdkPixmap; mask:PGdkBitmap):PGtkWidget; cdecl; external gtklib;
procedure gtk_pixmap_set(pixmap:PGtkPixmap; val:PGdkPixmap; mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_pixmap_get(pixmap:PGtkPixmap; var val:PGdkPixmap; var mask:PGdkBitmap); cdecl; external gtklib;
procedure gtk_pixmap_set_build_insensitive(pixmap:PGtkPixmap; build:gboolean); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_PIXMAP : GType;
begin
   GTK_TYPE_PIXMAP:=gtk_pixmap_get_type;
end;

function GTK_PIXMAP(obj: pointer) : PGtkPixmap;
begin
   GTK_PIXMAP:=PGtkPixmap(GTK_CHECK_CAST(obj,GTK_TYPE_PIXMAP));
end;

function GTK_PIXMAP_CLASS(klass: pointer) : PGtkPixmapClass;
begin
   GTK_PIXMAP_CLASS:=PGtkPixmapClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_PIXMAP));
end;

function GTK_IS_PIXMAP(obj: pointer) : boolean;
begin
   GTK_IS_PIXMAP:=GTK_CHECK_TYPE(obj,GTK_TYPE_PIXMAP);
end;

function GTK_IS_PIXMAP_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_PIXMAP_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_PIXMAP);
end;

function GTK_PIXMAP_GET_CLASS(obj: pointer) : PGtkPixmapClass;
begin
   GTK_PIXMAP_GET_CLASS:=PGtkPixmapClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_PIXMAP));
end;

function build_insensitive(var a : TGtkPixmap) : guint;
begin
   build_insensitive:=(a.flag0 and bm_TGtkPixmap_build_insensitive) shr bp_TGtkPixmap_build_insensitive;
end;

procedure set_build_insensitive(var a : TGtkPixmap; __build_insensitive : guint);
begin
   a.flag0:=a.flag0 or ((__build_insensitive shl bp_TGtkPixmap_build_insensitive) and bm_TGtkPixmap_build_insensitive);
end;

{$ENDIF read_implementation}
// included by gtk2.pas

