// included by gdk2pixbuf.pp

{$IFDEF read_interface}

type
{< private > }
   PGdkPixbufLoader = ^TGdkPixbufLoader;
   TGdkPixbufLoader = record
        parent_instance : TGObject;
        priv : gpointer;
     end;

{ Last known frame needs a redraw for x, y, width, height  }
   PGdkPixbufLoaderClass = ^TGdkPixbufLoaderClass;
   TGdkPixbufLoaderClass = record
        parent_class : TGObjectClass;
        area_prepared : procedure (loader:PGdkPixbufLoader); cdecl;
        area_updated : procedure (loader:PGdkPixbufLoader; x:longint; y:longint; width:longint; height:longint); cdecl;
        closed : procedure (loader:PGdkPixbufLoader); cdecl;
     end;


function GDK_TYPE_PIXBUF_LOADER : GType;
function GDK_PIXBUF_LOADER(obj : pointer) : PGdkPixbufLoader;
function GDK_PIXBUF_LOADER_CLASS(klass : pointer) : PGdkPixbufLoaderClass;
function GDK_IS_PIXBUF_LOADER(obj : pointer) : boolean;
function GDK_IS_PIXBUF_LOADER_CLASS(klass : pointer) : boolean;
function GDK_PIXBUF_LOADER_GET_CLASS(obj : pointer) : PGdkPixbufLoaderClass;

function gdk_pixbuf_loader_get_type:GType; cdecl; external gdkpixbuflib;
function gdk_pixbuf_loader_new:PGdkPixbufLoader; cdecl; external gdkpixbuflib;
function gdk_pixbuf_loader_new_with_type(image_type:Pchar; error:PPGError):PGdkPixbufLoader; cdecl; external gdkpixbuflib;
function gdk_pixbuf_loader_write(loader:PGdkPixbufLoader; buf:Pguchar; count:gsize; error:PPGError):gboolean; cdecl; external gdkpixbuflib;
function gdk_pixbuf_loader_get_pixbuf(loader:PGdkPixbufLoader):PGdkPixbuf; cdecl; external gdkpixbuflib;
function gdk_pixbuf_loader_get_animation(loader:PGdkPixbufLoader):PGdkPixbufAnimation; cdecl; external gdkpixbuflib;
function gdk_pixbuf_loader_close(loader:PGdkPixbufLoader; error:PPGError):gboolean; cdecl; external gdkpixbuflib;

{$ENDIF read_interface}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GDK_TYPE_PIXBUF_LOADER : GType;
begin
  GDK_TYPE_PIXBUF_LOADER:=gdk_pixbuf_loader_get_type;
end;

function GDK_PIXBUF_LOADER(obj : pointer) : PGdkPixbufLoader;
begin
  GDK_PIXBUF_LOADER:=PGdkPixbufLoader(G_TYPE_CHECK_INSTANCE_CAST(obj,
                                                       GDK_TYPE_PIXBUF_LOADER));
end;

function GDK_PIXBUF_LOADER_CLASS(klass : pointer) : PGdkPixbufLoaderClass;
begin
  GDK_PIXBUF_LOADER_CLASS:=PGdkPixbufLoaderClass(G_TYPE_CHECK_CLASS_CAST(klass,
                                                       GDK_TYPE_PIXBUF_LOADER));
end;

function GDK_IS_PIXBUF_LOADER(obj : pointer) : boolean;
begin
  GDK_IS_PIXBUF_LOADER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GDK_TYPE_PIXBUF_LOADER);
end;

function GDK_IS_PIXBUF_LOADER_CLASS(klass : pointer) : boolean;
begin
  GDK_IS_PIXBUF_LOADER_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,
                                                      GDK_TYPE_PIXBUF_LOADER);
end;

function GDK_PIXBUF_LOADER_GET_CLASS(obj : pointer) : PGdkPixbufLoaderClass;
begin
  GDK_PIXBUF_LOADER_GET_CLASS:=PGdkPixbufLoaderClass(G_TYPE_INSTANCE_GET_CLASS(
                                                   obj,GDK_TYPE_PIXBUF_LOADER));
end;
{$ENDIF}

