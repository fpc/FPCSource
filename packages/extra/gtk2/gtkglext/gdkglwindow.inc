{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglwindow.h }

{$ifdef read_interface_types}

  { PGdkGLWindow already defined in gdkgltypes.inc }
  
  TGdkGLWindow = record
    parent_instance: TGObject;
    drawable: PGdkDrawable;        {* Associated GdkWindow *}
  end;

  PGdkGLWindowClass = ^TGdkGLWindowClass;
  TGdkGLWindowClass = record
    parent_class: TGObjectClass;
  end;

{$endif read_interface_types}

{$ifdef read_interface_rest}

function GDK_TYPE_GL_WINDOW: GType;
function GDK_GL_WINDOW(anObject: Pointer): PGdkGLWindow;
function GDK_GL_WINDOW_CLASS(klass: Pointer): PGdkGLWindowClass;
function GDK_IS_GL_WINDOW(anObject: Pointer): boolean;
function GDK_IS_GL_WINDOW_CLASS(klass: Pointer): boolean;
function GDK_GL_WINDOW_GET_CLASS(obj: Pointer): PGdkGLWindowClass;

function gdk_gl_window_get_type: GType; cdecl; external GdkGLExtLib;

{*
 * attrib_list is currently unused. This must be set to NULL or empty
 * (first attribute of None). See GLX 1.3 spec.
 *}
function gdk_gl_window_new(
  glconfig: PGdkGLConfig;
  window: PGdkWindow;
  attrib_list: PLongInt): PGdkGLWindow;
  cdecl; external GdkGLExtLib;

procedure gdk_gl_window_destroy(glwindow: PGdkGLWindow);
  cdecl; external GdkGLExtLib;

function gdk_gl_window_get_window(glwindow: PGdkGLWindow): PGdkWindow;
  cdecl; external GdkGLExtLib;

{*
 * OpenGL extension to GdkWindow
 *}

function gdk_window_set_gl_capability(
  window: PGdkWindow;
  glconfig: PGdkGLConfig;
  attrib_list: PLongInt): PGdkGLWindow;
  cdecl; external GdkGLExtLib;

procedure gdk_window_unset_gl_capability(window: PGdkWindow);
  cdecl; external GdkGLExtLib;

function gdk_window_is_gl_capable(window: PGdkWindow): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_window_get_gl_window(window: PGdkWindow): PGdkGLWindow;
  cdecl; external GdkGLExtLib;

function gdk_window_get_gl_drawable(window: PGdkWindow): PGdkGLDrawable;

{$endif read_interface_rest}

{$ifdef read_implementation}

function GDK_TYPE_GL_WINDOW: GType;
begin
 GDK_TYPE_GL_WINDOW := gdk_gl_window_get_type;
end;

function GDK_GL_WINDOW(anObject: Pointer): PGdkGLWindow;
begin
 GDK_GL_WINDOW := PGdkGLWindow(
   G_TYPE_CHECK_INSTANCE_CAST(anObject, GDK_TYPE_GL_WINDOW));
end;

function GDK_GL_WINDOW_CLASS(klass: Pointer): PGdkGLWindowClass;
begin
 GDK_GL_WINDOW_CLASS := PGdkGLWindowClass(
   G_TYPE_CHECK_CLASS_CAST(klass, GDK_TYPE_GL_WINDOW));
end;

function GDK_IS_GL_WINDOW(anObject: Pointer): boolean;
begin
 GDK_IS_GL_WINDOW := G_TYPE_CHECK_INSTANCE_TYPE(anObject, GDK_TYPE_GL_WINDOW);
end;

function GDK_IS_GL_WINDOW_CLASS(klass: Pointer): boolean;
begin
 GDK_IS_GL_WINDOW_CLASS := G_TYPE_CHECK_CLASS_TYPE(klass, GDK_TYPE_GL_WINDOW);
end;

function GDK_GL_WINDOW_GET_CLASS(obj: Pointer): PGdkGLWindowClass;
begin
 GDK_GL_WINDOW_GET_CLASS := PGdkGLWindowClass(
   G_TYPE_INSTANCE_GET_CLASS(obj, GDK_TYPE_GL_WINDOW));
end;

function gdk_window_get_gl_drawable(window: PGdkWindow): PGdkGLDrawable;
begin
 gdk_window_get_gl_drawable := 
   GDK_GL_DRAWABLE (gdk_window_get_gl_window (window));
end;

{$endif read_implementation}
