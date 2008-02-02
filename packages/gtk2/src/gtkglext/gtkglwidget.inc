{ Original file is /usr/include/gtkglext-1.0/gtk/gtkglwidget.h }

{$ifdef read_interface_rest}

function gtk_widget_set_gl_capability(
  widget: PGtkWidget;
  glconfig: PGdkGLConfig;
  share_list: PGdkGLContext;
  direct: gboolean;
  render_type: Integer): gboolean;
  cdecl; external GtkGLExtLib;

function gtk_widget_is_gl_capable(widget: PGtkWidget): gboolean;
  cdecl; external GtkGLExtLib;

function gtk_widget_get_gl_config(widget: PGtkWidget): PGdkGLConfig;
  cdecl; external GtkGLExtLib;

function gtk_widget_create_gl_context(
  widget: PGtkWidget;
  share_list: PGdkGLContext;
  direct: gboolean;
  render_type: Integer): PGdkGLContext;
  cdecl; external GtkGLExtLib;

function gtk_widget_get_gl_context(widget: PGtkWidget): PGdkGLContext;
  cdecl; external GtkGLExtLib;

function gtk_widget_get_gl_window(widget: PGtkWidget): PGdkGLWindow;
  cdecl; external GtkGLExtLib;

function gtk_widget_get_gl_drawable(widget: PGtkWidget): PGdkGLDrawable;

{$endif read_interface_rest}

{$ifdef read_implementation}

function gtk_widget_get_gl_drawable(widget: PGtkWidget): PGdkGLDrawable;
begin
 gtk_widget_get_gl_drawable := 
   GDK_GL_DRAWABLE (gtk_widget_get_gl_window (widget));
end;

{$endif read_implementation}
