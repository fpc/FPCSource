// included by gdk2.pp

{$IFDEF read_interface_types}
{ window-system-specific delegate anObject  }
   PGdkPixmapObject = ^TGdkPixmapObject;
   TGdkPixmapObject = record
        parent_instance : TGdkDrawable;
        impl : PGdkDrawable;
        depth : gint;
     end;

   PGdkPixmapObjectClass = ^TGdkPixmapObjectClass;
   TGdkPixmapObjectClass = record
        parent_class : TGdkDrawableClass;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GDK_TYPE_PIXMAP : GType;
function GDK_PIXMAP(anObject : Pointer) : PGdkPixmap;
function GDK_PIXMAP_CLASS(klass : Pointer) : PGdkPixmapObjectClass;
function GDK_IS_PIXMAP(anObject : Pointer) : boolean;
function GDK_IS_PIXMAP_CLASS(klass : Pointer) : boolean;
function GDK_PIXMAP_GET_CLASS(obj : Pointer) : PGdkPixmapObjectClass;

function GDK_PIXMAP_OBJECT(anObject : Pointer) : PGdkPixmapObject;


function gdk_pixmap_get_type:GType; cdecl; external gdklib;


{ Pixmaps }

function gdk_pixmap_new(window:PGdkWindow; width:gint; height:gint; depth:gint):PGdkPixmap; cdecl; external gdklib;
function gdk_bitmap_create_from_data(window:PGdkWindow; data:Pgchar; width:gint; height:gint):PGdkBitmap; cdecl; external gdklib;
function gdk_pixmap_create_from_data(window:PGdkWindow; data:Pgchar; width:gint; height:gint; depth:gint;
           fg:PGdkColor; bg:PGdkColor):PGdkPixmap; cdecl; external gdklib;
function gdk_pixmap_create_from_xpm(window:PGdkWindow; var mask:PGdkBitmap; transparent_color:PGdkColor; filename:Pgchar):PGdkPixmap; cdecl; external gdklib;
function gdk_pixmap_colormap_create_from_xpm(window:PGdkWindow; colormap:PGdkColormap; var mask:PGdkBitmap; transparent_color:PGdkColor; filename:Pgchar):PGdkPixmap; cdecl; external gdklib;
function gdk_pixmap_create_from_xpm_d(window:PGdkWindow; var mask:PGdkBitmap; transparent_color:PGdkColor; data:PPgchar):PGdkPixmap; cdecl; external gdklib;
function gdk_pixmap_colormap_create_from_xpm_d(window:PGdkWindow; colormap:PGdkColormap; var mask:PGdkBitmap; transparent_color:PGdkColor; data:PPgchar):PGdkPixmap; cdecl; external gdklib;
{ Functions to create/lookup pixmaps from their native equivalents }

{$ifndef GDK_MULTIHEAD_SAFE}
function gdk_pixmap_foreign_new(anid:TGdkNativeWindow):PGdkPixmap; cdecl; external gdklib;
function gdk_pixmap_lookup(anid:TGdkNativeWindow):PGdkPixmap; cdecl; external gdklib;
{$endif}
{ GDK_MULTIHEAD_SAFE  }

function gdk_pixmap_foreign_new_for_display(display:PGdkDisplay; anid:TGdkNativeWindow):PGdkPixmap; cdecl; external gdklib;
function gdk_pixmap_lookup_for_display(display:PGdkDisplay; anid:TGdkNativeWindow):PGdkPixmap; cdecl; external gdklib;

{$ifndef GDK_DISABLE_DEPRECATED}
function gdk_bitmap_ref(drawable:PGdkDrawable):PGdkDrawable;
procedure gdk_bitmap_unref(drawable:PGdkDrawable);
function gdk_pixmap_ref(drawable:PGdkDrawable):PGdkDrawable;
procedure gdk_pixmap_unref(drawable:PGdkDrawable);
{$endif}
{$endif read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_PIXMAP : GType;
begin
  GDK_TYPE_PIXMAP:=gdk_pixmap_get_type;
end;

function GDK_PIXMAP(anObject : Pointer) : PGdkPixmap;
begin
  GDK_PIXMAP:=PGdkPixmap(G_TYPE_CHECK_INSTANCE_CAST(anObject,GDK_TYPE_PIXMAP));
end;

function GDK_PIXMAP_CLASS(klass : Pointer) : PGdkPixmapObjectClass;
begin
  GDK_PIXMAP_CLASS:=PGdkPixmapObjectClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                              GDK_TYPE_PIXMAP));
end;

function GDK_IS_PIXMAP(anObject : Pointer) : boolean;
begin
  GDK_IS_PIXMAP:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GDK_TYPE_PIXMAP);
end;

function GDK_IS_PIXMAP_CLASS(klass : Pointer) : boolean;
begin
  GDK_IS_PIXMAP_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GDK_TYPE_PIXMAP);
end;

function GDK_PIXMAP_GET_CLASS(obj : Pointer) : PGdkPixmapObjectClass;
begin
  GDK_PIXMAP_GET_CLASS:=PGdkPixmapObjectClass(G_TYPE_INSTANCE_GET_CLASS(obj,
                                                              GDK_TYPE_PIXMAP));
end;

function GDK_PIXMAP_OBJECT(anObject : Pointer) : PGdkPixmapObject;
begin
  GDK_PIXMAP_OBJECT:=PGdkPixmapObject(GDK_PIXMAP(anObject));
end;

function gdk_bitmap_ref(drawable:PGdkDrawable):PGdkDrawable;
begin
  gdk_bitmap_ref:=GDK_DRAWABLE(g_object_ref(G_OBJECT(drawable)));
end;

procedure gdk_bitmap_unref(drawable:PGdkDrawable);
begin
  g_object_unref(G_OBJECT(drawable));
end;

function gdk_pixmap_ref(drawable:PGdkDrawable):PGdkDrawable;
begin
  gdk_pixmap_ref:=GDK_DRAWABLE(g_object_ref(G_OBJECT(drawable)));
end;

procedure gdk_pixmap_unref(drawable:PGdkDrawable);
begin
  g_object_unref(G_OBJECT(drawable));
end;

{$ENDIF}

