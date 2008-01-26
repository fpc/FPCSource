{ Original file is /usr/include/gtkglext-1.0/gdk/gdkgldrawable.h

  Michalis comments: note that GdkGLDrawable is an interface,
  not a class, so
  - it's GDK_GL_DRAWABLE_GET_CLASS is implemented using
    GDK_GL_DRAWABLE_GET_INTERFACE instead of GDK_GL_DRAWABLE_GET_CLASS
  - 1st field of class record is
      base_iface: TGTypeInterface;
    instead of
      parent_class: TGObjectClass;
  - Also (this is of course minor thing that does not affect
    code using this unit, but still it is preserved in this translation
    of C headers to Pascal) names of params of macros are vtable and inst.
}

{$ifdef read_interface_types}

  { PGdkGLDrawable already defined in gdkgltypes.inc }

  TGdkGLDrawable = record
    parent_instance: TGObject;
  end;

  PGdkGLDrawableClass = ^TGdkGLDrawableClass;
  TGdkGLDrawableClass = record
    base_iface: TGTypeInterface;

    create_new_context:  function(
      gldrawable: PGdkGLDrawable;
      share_list: PGdkGLContext;
      direct: gboolean;
      render_type: LongInt): PGdkGLContext; cdecl;

    make_context_current: function(
      draw: PGdkGLDrawable;
      a_read: PGdkGLDrawable;
      glcontext: PGdkGLContext): gboolean; cdecl;
    is_double_buffered: function(gldrawable: PGdkGLDrawable): gboolean; cdecl;
    swap_buffers: procedure(gldrawable: PGdkGLDrawable); cdecl;
    wait_gl: procedure(gldrawable: PGdkGLDrawable); cdecl;
    wait_gdk: procedure(gldrawable: PGdkGLDrawable); cdecl;

    gl_begin: function(
      draw: PGdkGLDrawable;
      a_read: PGdkGLDrawable;
      glcontext: PGdkGLContext): gboolean; cdecl;
    gl_end: procedure(gldrawable: PGdkGLDrawable); cdecl;

    get_gl_config: function(gldrawable: PGdkGLDrawable): PGdkGLConfig; cdecl;

    get_size: procedure(
      gldrawable: PGdkGLDrawable;
      width, height: PGInt); cdecl;
  end;

{$endif read_interface_types}

{$ifdef read_interface_rest}

function GDK_TYPE_GL_DRAWABLE: GType;
function GDK_GL_DRAWABLE(inst: Pointer): PGdkGLDrawable;
function GDK_GL_DRAWABLE_CLASS(vtable: Pointer): PGdkGLDrawableClass;
function GDK_IS_GL_DRAWABLE(inst: Pointer): boolean;
function GDK_IS_GL_DRAWABLE_CLASS(vtable: Pointer): boolean;
function GDK_GL_DRAWABLE_GET_CLASS(inst: Pointer): PGdkGLDrawableClass;

function gdk_gl_drawable_get_type: GType;
  cdecl; external GdkGLExtLib;

function gdk_gl_drawable_make_current(
  gldrawable: PGdkGLDrawable;
  glcontext: PGdkGLContext)
  : gboolean;
  cdecl; external GdkGLExtLib;

function gdk_gl_drawable_is_double_buffered(
  gldrawable: PGdkGLDrawable)
  : gboolean;
  cdecl; external GdkGLExtLib;

procedure gdk_gl_drawable_swap_buffers(
  gldrawable: PGdkGLDrawable);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_drawable_wait_gl(
  gldrawable: PGdkGLDrawable);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_drawable_wait_gdk(
  gldrawable: PGdkGLDrawable);
  cdecl; external GdkGLExtLib;

function gdk_gl_drawable_gl_begin(
  gldrawable: PGdkGLDrawable;
  glcontext: PGdkGLContext)
  : gboolean;
  cdecl; external GdkGLExtLib;

procedure gdk_gl_drawable_gl_end(
  gldrawable: PGdkGLDrawable);
  cdecl; external GdkGLExtLib;

function gdk_gl_drawable_get_gl_config(
  gldrawable: PGdkGLDrawable)
  : PGdkGLConfig;
  cdecl; external GdkGLExtLib;

procedure gdk_gl_drawable_get_size(
  gldrawable: PGdkGLDrawable;
  width, height: PGInt);
  cdecl; external GdkGLExtLib;

function gdk_gl_drawable_get_current
  : PGdkGLDrawable;
  cdecl; external GdkGLExtLib;

{$endif read_interface_rest}

{$ifdef read_implementation}

function GDK_TYPE_GL_DRAWABLE: GType;
begin
 GDK_TYPE_GL_DRAWABLE := gdk_gl_drawable_get_type;
end;

function GDK_GL_DRAWABLE(inst: Pointer): PGdkGLDrawable;
begin
 GDK_GL_DRAWABLE := PGdkGLDrawable(
   G_TYPE_CHECK_INSTANCE_CAST(inst, GDK_TYPE_GL_DRAWABLE));
end;

function GDK_GL_DRAWABLE_CLASS(vtable: Pointer): PGdkGLDrawableClass;
begin
 GDK_GL_DRAWABLE_CLASS := PGdkGLDrawableClass(
   G_TYPE_CHECK_CLASS_CAST(vtable, GDK_TYPE_GL_DRAWABLE));
end;

function GDK_IS_GL_DRAWABLE(inst: Pointer): boolean;
begin
 GDK_IS_GL_DRAWABLE := G_TYPE_CHECK_INSTANCE_TYPE(inst, GDK_TYPE_GL_DRAWABLE);
end;

function GDK_IS_GL_DRAWABLE_CLASS(vtable: Pointer): boolean;
begin
 GDK_IS_GL_DRAWABLE_CLASS := G_TYPE_CHECK_CLASS_TYPE(vtable, GDK_TYPE_GL_DRAWABLE);
end;

function GDK_GL_DRAWABLE_GET_CLASS(inst: Pointer): PGdkGLDrawableClass;
begin
 GDK_GL_DRAWABLE_GET_CLASS := PGdkGLDrawableClass(
   G_TYPE_INSTANCE_GET_INTERFACE(inst, GDK_TYPE_GL_DRAWABLE));
end;

{$endif read_implementation}
