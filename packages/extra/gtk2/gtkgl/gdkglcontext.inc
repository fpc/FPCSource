{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglcontext.h }

{$ifdef read_interface_types}

  { PGdkGLContext already defined in gdkgltypes.inc }

  TGdkGLContext = record
    parent_instance: TGObject;
  end;

  PGdkGLContextClass = ^TGdkGLContextClass;
  TGdkGLContextClass = record
    parent_class: TGObjectClass;
  end;

{$endif read_interface_types}

{$ifdef read_interface_rest}

function GDK_TYPE_GL_CONTEXT: GType;
function GDK_GL_CONTEXT(anObject: Pointer): PGdkGLContext;
function GDK_GL_CONTEXT_CLASS(klass: Pointer): PGdkGLContextClass;
function GDK_IS_GL_CONTEXT(anObject: Pointer): boolean;
function GDK_IS_GL_CONTEXT_CLASS(klass: Pointer): boolean;
function GDK_GL_CONTEXT_GET_CLASS(obj: Pointer): PGdkGLContextClass;

function gdk_gl_context_get_type: GType; cdecl; external GdkGLExtLib;

function gdk_gl_context_new(
  gldrawable: PGdkGLDrawable;
  share_list: PGdkGLContext;
  direct: gboolean;
  render_type: LongInt): PGdkGLContext;
  cdecl; external GdkGLExtLib;

procedure gdk_gl_context_destroy(glcontext: PGdkGLContext);
  cdecl; external GdkGLExtLib;

function gdk_gl_context_copy(
  glcontext: PGdkGLContext;
  src: PGdkGLContext;
  mask: LongWord): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_context_get_gl_drawable (glcontext: PGdkGLContext): PGdkGLDrawable;
  cdecl; external GdkGLExtLib;

function gdk_gl_context_get_gl_config   (glcontext: PGdkGLContext): PGdkGLConfig;
  cdecl; external GdkGLExtLib;

function gdk_gl_context_get_share_list  (glcontext: PGdkGLContext): PGdkGLContext;
  cdecl; external GdkGLExtLib;

function gdk_gl_context_is_direct       (glcontext: PGdkGLContext): gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_context_get_render_type (glcontext: PGdkGLContext): LongInt;
  cdecl; external GdkGLExtLib;

function gdk_gl_context_get_current     : PGdkGLContext;
  cdecl; external GdkGLExtLib;

{$endif read_interface_rest}

{$ifdef read_implementation}

function GDK_TYPE_GL_CONTEXT: GType;
begin
 GDK_TYPE_GL_CONTEXT := gdk_gl_context_get_type;
end;

function GDK_GL_CONTEXT(anObject: Pointer): PGdkGLContext;
begin
 GDK_GL_CONTEXT := PGdkGLContext(
   G_TYPE_CHECK_INSTANCE_CAST(anObject, GDK_TYPE_GL_CONTEXT));
end;

function GDK_GL_CONTEXT_CLASS(klass: Pointer): PGdkGLContextClass;
begin
 GDK_GL_CONTEXT_CLASS := PGdkGLContextClass(
   G_TYPE_CHECK_CLASS_CAST(klass, GDK_TYPE_GL_CONTEXT));
end;

function GDK_IS_GL_CONTEXT(anObject: Pointer): boolean;
begin
 GDK_IS_GL_CONTEXT := G_TYPE_CHECK_INSTANCE_TYPE(anObject, GDK_TYPE_GL_CONTEXT);
end;

function GDK_IS_GL_CONTEXT_CLASS(klass: Pointer): boolean;
begin
 GDK_IS_GL_CONTEXT_CLASS := G_TYPE_CHECK_CLASS_TYPE(klass, GDK_TYPE_GL_CONTEXT);
end;

function GDK_GL_CONTEXT_GET_CLASS(obj: Pointer): PGdkGLContextClass;
begin
 GDK_GL_CONTEXT_GET_CLASS := PGdkGLContextClass(
   G_TYPE_INSTANCE_GET_CLASS(obj, GDK_TYPE_GL_CONTEXT));
end;

{$endif read_implementation}
