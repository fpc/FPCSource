{ Original file is /usr/include/gtkglext-1.0/gdk/gdkglshapes.h }

{$ifdef read_interface_rest}

procedure gdk_gl_draw_cube(
  solid: gboolean;
  size: Double);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_sphere(
  solid: gboolean;
  radius: Double;
  slices: LongInt;
  stacks: LongInt);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_cone(
  solid: gboolean;
  base: Double;
  height: Double;
  slices: LongInt;
  stacks: LongInt);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_torus(
  solid: gboolean;
  inner_radius: Double;
  outer_radius: Double;
  nsides: LongInt;
  rings: LongInt);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_tetrahedron  (solid: gboolean);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_octahedron   (solid: gboolean);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_dodecahedron (solid: gboolean);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_icosahedron  (solid: gboolean);
  cdecl; external GdkGLExtLib;

procedure gdk_gl_draw_teapot(
  solid: gboolean;
  scale: Double);
  cdecl; external GdkGLExtLib;

{$endif read_interface_rest}
