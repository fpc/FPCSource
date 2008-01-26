{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglpixmap.h }

{$ifdef read_interface_types}

  { PGdkGLPixmap already defined in gdkgltypes.inc }
  
  TGdkGLPixmap = record
    parent_instance: TGObject;
    drawable: PGdkDrawable;        {* Associated GdkPixmap *}
  end;

  PGdkGLPixmapClass = ^TGdkGLPixmapClass;
  TGdkGLPixmapClass = record
    parent_class: TGObjectClass;
  end;

{$endif read_interface_types}

{$ifdef read_interface_rest}

function GDK_TYPE_GL_PIXMAP: GType;
function GDK_GL_PIXMAP(anObject: Pointer): PGdkGLPixmap;
function GDK_GL_PIXMAP_CLASS(klass: Pointer): PGdkGLPixmapClass;
function GDK_IS_GL_PIXMAP(anObject: Pointer): boolean;
function GDK_IS_GL_PIXMAP_CLASS(klass: Pointer): boolean;
function GDK_GL_PIXMAP_GET_CLASS(obj: Pointer): PGdkGLPixmapClass;

function gdk_gl_pixmap_get_type: GType; cdecl; external GdkGLExtLib;

{*
 * attrib_list is currently unused. This must be set to NULL or empty
 * (first attribute of None). See GLX 1.3 spec.
 *}
function gdk_gl_pixmap_new(
  glconfig: PGdkGLConfig;
  pixmap: PGdkPixmap;
  attrib_list: PLongInt): PGdkGLPixmap;
  cdecl; external GdkGLExtLib;

procedure gdk_gl_pixmap_destroy(glpixmap: PGdkGLPixmap);
  cdecl; external GdkGLExtLib;

function gdk_gl_pixmap_get_pixmap(glpixmap: PGdkGLPixmap): PGdkPixmap;
  cdecl; external GdkGLExtLib;

{*
 * OpenGL extension to GdkPixmap
 *}
 
function gdk_pixmap_set_gl_capability(
  pixmap: PGdkPixmap;
  glconfig: PGdkGLConfig;
  attrib_list: PLongInt): PGdkGLPixmap;
  cdecl; external GdkGLExtLib;

procedure gdk_pixmap_unset_gl_capability(pixmap: PGdkPixmap);
  cdecl; external GdkGLExtLib;

function gdk_pixmap_is_gl_capable(pixmap: PGdkPixmap): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_pixmap_get_gl_pixmap(pixmap: PGdkPixmap): PGdkGLPixmap;
  cdecl; external GdkGLExtLib;

function gdk_pixmap_get_gl_drawable(pixmap: PGdkPixmap): PGdkGLDrawable;

{$endif read_interface_rest}

{$ifdef read_implementation}

function GDK_TYPE_GL_PIXMAP: GType;
begin
 GDK_TYPE_GL_PIXMAP := gdk_gl_pixmap_get_type;
end;

function GDK_GL_PIXMAP(anObject: Pointer): PGdkGLPixmap;
begin
 GDK_GL_PIXMAP := PGdkGLPixmap(
   G_TYPE_CHECK_INSTANCE_CAST(anObject, GDK_TYPE_GL_PIXMAP));
end;

function GDK_GL_PIXMAP_CLASS(klass: Pointer): PGdkGLPixmapClass;
begin
 GDK_GL_PIXMAP_CLASS := PGdkGLPixmapClass(
   G_TYPE_CHECK_CLASS_CAST(klass, GDK_TYPE_GL_PIXMAP));
end;

function GDK_IS_GL_PIXMAP(anObject: Pointer): boolean;
begin
 GDK_IS_GL_PIXMAP := G_TYPE_CHECK_INSTANCE_TYPE(anObject, GDK_TYPE_GL_PIXMAP);
end;

function GDK_IS_GL_PIXMAP_CLASS(klass: Pointer): boolean;
begin
 GDK_IS_GL_PIXMAP_CLASS := G_TYPE_CHECK_CLASS_TYPE(klass, GDK_TYPE_GL_PIXMAP);
end;

function GDK_GL_PIXMAP_GET_CLASS(obj: Pointer): PGdkGLPixmapClass;
begin
 GDK_GL_PIXMAP_GET_CLASS := PGdkGLPixmapClass(
   G_TYPE_INSTANCE_GET_CLASS(obj, GDK_TYPE_GL_PIXMAP));
end;

function gdk_pixmap_get_gl_drawable(pixmap: PGdkPixmap): PGdkGLDrawable;
begin
 gdk_pixmap_get_gl_drawable :=
   GDK_GL_DRAWABLE (gdk_pixmap_get_gl_pixmap (pixmap));
end;

{$endif read_implementation}
