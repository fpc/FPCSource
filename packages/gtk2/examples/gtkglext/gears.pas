{ 3-D gear wheels.  This program is in the public domain.
  Brian Paul
  Conversion to GLUT by Mark J. Kilgard
  Conversion to GtkGLExt by Naofumi Yasufuku
  Conversion to Pascal binding of GtkGLExt by Michalis Kamburelis
}

{$mode delphi}

uses Glib2, Gtk2, Gdk2, GdkGLExt, GtkGLExt, gl;

{*
 * Draw a gear wheel.  You'll probably want to call this function when
 * building a display list since we do a lot of trig here.
 *
 * Input:  inner_radius - radius of hole at center
 * outer_radius - radius at center of teeth
 * width - width of gear
 * teeth - number of teeth
 * tooth_depth - depth of tooth
 *}

procedure gear(
  inner_radius, outer_radius, width: GLfloat;
  teeth: GLint;
  tooth_depth: GLfloat);
var
  i: GLint;
  r0, r1, r2: GLfloat;
  angle, da: GLfloat;
  u, v, len: GLfloat;
begin
  r0 := inner_radius;
  r1 := outer_radius - tooth_depth / 2.0;
  r2 := outer_radius + tooth_depth / 2.0;

  da := 2.0 * Pi / teeth / 4.0;

  glShadeModel(GL_FLAT);

  glNormal3f(0.0, 0.0, 1.0);

  {* draw front face *}
  glBegin(GL_QUAD_STRIP);
  for i := 0 to teeth do
  begin
    angle := i * 2.0 * Pi / teeth;
    glVertex3f(r0 * cos(angle), r0 * sin(angle), width * 0.5);
    glVertex3f(r1 * cos(angle), r1 * sin(angle), width * 0.5);
    if i < teeth then
    begin
      glVertex3f(r0 * cos(angle), r0 * sin(angle), width * 0.5);
      glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
    end;
  end;
  glEnd();

  {* draw front sides of teeth *}
  glBegin(GL_QUADS);
  da := 2.0 * Pi / teeth / 4.0;
  for i := 0 to teeth - 1 do
  begin
    angle := i * 2.0 * Pi / teeth;

    glVertex3f(r1 * cos(angle), r1 * sin(angle), width * 0.5);
    glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), width * 0.5);
    glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), width * 0.5);
    glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
  end;
  glEnd();

  glNormal3f(0.0, 0.0, -1.0);

  {* draw back face *}
  glBegin(GL_QUAD_STRIP);
  for i := 0 to teeth do
  begin
    angle := i * 2.0 * Pi / teeth;
    glVertex3f(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
    glVertex3f(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
    if i < teeth then
    begin
      glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
      glVertex3f(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
    end;
  end;
  glEnd();

  {* draw back sides of teeth *}
  glBegin(GL_QUADS);
  da := 2.0 * Pi / teeth / 4.0;
  for i := 0 to teeth - 1 do
  begin
    angle := i * 2.0 * Pi / teeth;

    glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
    glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), -width * 0.5);
    glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), -width * 0.5);
    glVertex3f(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
  end;
  glEnd();

  {* draw outward faces of teeth *}
  glBegin(GL_QUAD_STRIP);
  for i := 0 to teeth - 1 do
  begin
    angle := i * 2.0 * Pi / teeth;

    glVertex3f(r1 * cos(angle), r1 * sin(angle), width * 0.5);
    glVertex3f(r1 * cos(angle), r1 * sin(angle), -width * 0.5);
    u := r2 * cos(angle + da) - r1 * cos(angle);
    v := r2 * sin(angle + da) - r1 * sin(angle);
    len := sqrt(u * u + v * v);
    u := u / len;
    v := v / len;
    glNormal3f(v, -u, 0.0);
    glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), width * 0.5);
    glVertex3f(r2 * cos(angle + da), r2 * sin(angle + da), -width * 0.5);
    glNormal3f(cos(angle), sin(angle), 0.0);
    glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), width * 0.5);
    glVertex3f(r2 * cos(angle + 2 * da), r2 * sin(angle + 2 * da), -width * 0.5);
    u := r1 * cos(angle + 3 * da) - r2 * cos(angle + 2 * da);
    v := r1 * sin(angle + 3 * da) - r2 * sin(angle + 2 * da);
    glNormal3f(v, -u, 0.0);
    glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), width * 0.5);
    glVertex3f(r1 * cos(angle + 3 * da), r1 * sin(angle + 3 * da), -width * 0.5);
    glNormal3f(cos(angle), sin(angle), 0.0);
  end;

  glVertex3f(r1 * cos(0), r1 * sin(0), width * 0.5);
  glVertex3f(r1 * cos(0), r1 * sin(0), -width * 0.5);

  glEnd();

  glShadeModel(GL_SMOOTH);

  {* draw inside radius cylinder *}
  glBegin(GL_QUAD_STRIP);
  for i := 0 to teeth do
  begin
    angle := i * 2.0 * Pi / teeth;
    glNormal3f(-cos(angle), -sin(angle), 0.0);
    glVertex3f(r0 * cos(angle), r0 * sin(angle), -width * 0.5);
    glVertex3f(r0 * cos(angle), r0 * sin(angle), width * 0.5);
  end;
  glEnd();

end;

var
  view_rotx: GLfloat = 20.0;
  view_roty: GLfloat = 30.0;
  view_rotz: GLfloat = 0.0;
  gear1, gear2, gear3: GLint;
  angle: GLfloat = 0.0;

  timer: PGTimer = nil;
  frames: gint  = 0;

  is_sync: boolean = true;

function draw(
  widget: PGtkWidget;
  event: PGdkEventExpose;
  data: gpointer): gboolean; cdecl;
var
  seconds: gdouble;
  fps: gdouble;
  glcontext: PGdkGLContext;
  gldrawable: PGdkGLDrawable;
begin
  glcontext := gtk_widget_get_gl_context (widget);
  gldrawable := gtk_widget_get_gl_drawable (widget);

  {*** OpenGL BEGIN ***}
  if not gdk_gl_drawable_gl_begin (gldrawable, glcontext) then
    Exit(false);

  glClear (GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);

  glPushMatrix ();
    glRotatef (view_rotx, 1.0, 0.0, 0.0);
    glRotatef (view_roty, 0.0, 1.0, 0.0);
    glRotatef (view_rotz, 0.0, 0.0, 1.0);

    glPushMatrix ();
      glTranslatef (-3.0, -2.0, 0.0);
      glRotatef (angle, 0.0, 0.0, 1.0);
      glCallList (gear1);
    glPopMatrix ();

    glPushMatrix ();
      glTranslatef (3.1, -2.0, 0.0);
      glRotatef (-2.0 * angle - 9.0, 0.0, 0.0, 1.0);
      glCallList (gear2);
    glPopMatrix ();

    glPushMatrix ();
      glTranslatef (-3.1, 4.2, 0.0);
      glRotatef (-2.0 * angle - 25.0, 0.0, 0.0, 1.0);
      glCallList (gear3);
    glPopMatrix ();

  glPopMatrix ();

  if gdk_gl_drawable_is_double_buffered (gldrawable) then
    gdk_gl_drawable_swap_buffers (gldrawable) else
    glFlush ();

  gdk_gl_drawable_gl_end (gldrawable);
  {*** OpenGL END ***}

  Inc(frames);

  seconds := g_timer_elapsed (timer, NULL);
  if seconds >= 5.0 then
  begin
    fps := frames / seconds;
    g_print ('%d frames in %6.3f seconds = %6.3f FPS' + LineEnding, [frames, seconds, fps]);
    g_timer_reset (timer);
    frames := 0;
  end;

  Result := true;
end;

{* new window size or exposure *}
function reshape (
  widget: PGtkWidget;
  event: PGdkEventConfigure;
  data: gpointer): gboolean; cdecl;
var
  glcontext: PGdkGLContext;
  gldrawable: PGdkGLDrawable;
  h: GLfloat;
begin
  glcontext := gtk_widget_get_gl_context (widget);
  gldrawable := gtk_widget_get_gl_drawable (widget);

  h := widget.allocation.height / widget.allocation.width;

  {*** OpenGL BEGIN ***}
  if not gdk_gl_drawable_gl_begin (gldrawable, glcontext) then
    Exit(false);

  glViewport (0, 0, widget.allocation.width, widget.allocation.height);
  glMatrixMode (GL_PROJECTION);
  glLoadIdentity ();
  glFrustum (-1.0, 1.0, -h, h, 5.0, 60.0);
  glMatrixMode (GL_MODELVIEW);
  glLoadIdentity ();
  glTranslatef (0.0, 0.0, -40.0);

  gdk_gl_drawable_gl_end (gldrawable);
  {*** OpenGL END ***}

  Result := true;
end;

procedure init(
  widget: PGtkWidget;
  data: gpointer); cdecl;
const
  pos: array[0..3] of GLfloat = (5.0, 5.0, 10.0, 0.0);
  red: array[0..3] of GLfloat = (0.8, 0.1, 0.0, 1.0);
  green: array[0..3] of GLfloat = (0.0, 0.8, 0.2, 1.0);
  blue: array[0..3] of GLfloat = (0.2, 0.2, 1.0, 1.0);
var
  glcontext: PGdkGLContext;
  gldrawable: PGdkGLDrawable;
begin
  glcontext := gtk_widget_get_gl_context (widget);
  gldrawable := gtk_widget_get_gl_drawable (widget);

  {*** OpenGL BEGIN ***}
  if not gdk_gl_drawable_gl_begin (gldrawable, glcontext) then
    Exit;

  glLightfv (GL_LIGHT0, GL_POSITION, pos);
  glEnable (GL_CULL_FACE);
  glEnable (GL_LIGHTING);
  glEnable (GL_LIGHT0);
  glEnable (GL_DEPTH_TEST);

  {* make the gears *}
  gear1 := glGenLists (1);
  glNewList (gear1, GL_COMPILE);
    glMaterialfv (GL_FRONT, GL_AMBIENT_AND_DIFFUSE, red);
    gear (1.0, 4.0, 1.0, 20, 0.7);
  glEndList ();

  gear2 := glGenLists (1);
  glNewList (gear2, GL_COMPILE);
    glMaterialfv (GL_FRONT, GL_AMBIENT_AND_DIFFUSE, green);
    gear (0.5, 2.0, 2.0, 10, 0.7);
  glEndList ();

  gear3 := glGenLists (1);
  glNewList (gear3, GL_COMPILE);
    glMaterialfv (GL_FRONT, GL_AMBIENT_AND_DIFFUSE, blue);
    gear (1.3, 2.0, 0.5, 10, 0.7);
  glEndList ();

  glEnable (GL_NORMALIZE);

  g_print (LineEnding);
  g_print ('GL_RENDERER   = %s' + LineEnding, [glGetString (GL_RENDERER)]);
  g_print ('GL_VERSION    = %s' + LineEnding, [glGetString (GL_VERSION)]);
  g_print ('GL_VENDOR     = %s' + LineEnding, [glGetString (GL_VENDOR)]);
  g_print ('GL_EXTENSIONS = %s' + LineEnding, [glGetString (GL_EXTENSIONS)]);
  g_print (LineEnding);

  gdk_gl_drawable_gl_end (gldrawable);
  {*** OpenGL END ***}

  {* create timer *}
  if timer = nil then
    timer := g_timer_new ();

  g_timer_start (timer);
end;

function idle (widget: PGtkWidget): gboolean; cdecl;
begin
  angle := angle + 2.0;

  {* Invalidate the whole window. *}
  gdk_window_invalidate_rect (widget.window, @widget.allocation, false);

  {* Update synchronously (fast). *}
  if is_sync then
    gdk_window_process_updates (widget.window, false);

  Result := true;
end;

var
  idle_id: guint  = 0;

procedure idle_add (widget: PGtkWidget); cdecl;
begin
  if idle_id = 0 then
  begin
    idle_id := g_idle_add_full (GDK_PRIORITY_REDRAW,
                               TGSourceFunc(@idle),
                               widget,
                               NULL);
  end;
end;

procedure idle_remove (widget: PGtkWidget); cdecl;
begin
  if idle_id <> 0 then
  begin
    g_source_remove (idle_id);
    idle_id := 0;
  end;
end;

function map (
  widget: PGtkWidget;
  event: PGdkEventAny;
  data: gpointer): gboolean; cdecl;
begin
  idle_add (widget);

  Result := true;
end;

function unmap (
  widget: PGtkWidget;
  event: PGdkEventAny;
  data: gpointer): gboolean; cdecl;
begin
  idle_remove (widget);

  Result := true;
end;

function visible (
  widget: PGtkWidget;
  event: PGdkEventVisibility;
  data: gpointer): gboolean; cdecl;
begin
  if event.state = GDK_VISIBILITY_FULLY_OBSCURED then
    idle_remove (widget) else
    idle_add (widget);

  Result := true;
end;

{* change view angle, exit upon ESC *}
function key (
  widget: PGtkWidget;
  event: PGdkEventKey;
  data: gpointer): gboolean; cdecl;
begin
  case event.keyval of
    GDK_KEY_z         : view_rotz := view_rotz + 5.0;
    GDK_KEY_Capital_Z : view_rotz := view_rotz - 5.0;
    GDK_KEY_Up        : view_roty := view_roty + 5.0;
    GDK_KEY_Down      : view_roty := view_roty - 5.0;
    GDK_KEY_Left      : view_rotx := view_rotx + 5.0;
    GDK_KEY_Right     : view_rotx := view_rotx - 5.0;
    GDK_KEY_Escape    : gtk_main_quit ();
    else Exit(false);
  end;

  gdk_window_invalidate_rect (widget.window, @widget.allocation, FALSE);

  Result := true;
end;

var
  glconfig: PGdkGLConfig;
  window: PGtkWidget;
  vbox: PGtkWidget;
  drawing_area: PGtkWidget;
  button: PGtkWidget;
  i: Integer;
begin

  {*
   * Init GTK.
   *}

  gtk_init (@argc, @argv);

  {*
   * Init GtkGLExt.
   *}

  gtk_gl_init (@argc, @argv);

  {*
   * Command line options.
   *}

  for i := 1 to ParamCount do
    if ParamStr(i) = '--async' then
      is_sync := FALSE;

  {*
   * Configure OpenGL-capable visual.
   *}

  {* Try double-buffered visual *}
  glconfig := gdk_gl_config_new_by_mode (GDK_GL_MODE_RGB or
                                         GDK_GL_MODE_DEPTH or
                                         GDK_GL_MODE_DOUBLE);
  if glconfig = nil then
  begin
    g_print ('*** Cannot find the double-buffered visual.' +LineEnding);
    g_print ('*** Trying single-buffered visual.' +LineEnding);

    {* Try single-buffered visual *}
    glconfig := gdk_gl_config_new_by_mode (GDK_GL_MODE_RGB or
                                           GDK_GL_MODE_DEPTH);
    if glconfig = nil then
    begin
      g_print ('*** No appropriate OpenGL-capable visual found.' +LineEnding);
      Halt(1);
    end;
  end;

  {*
   * Top-level window.
   *}

  window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
  gtk_window_set_title (GTK_WINDOW (window), 'gears');

  {* Get automatically redrawn if any of their children changed allocation. *}
  gtk_container_set_reallocate_redraws (GTK_CONTAINER (window), TRUE);

  g_signal_connect (G_OBJECT (window), 'delete_event',
		    G_CALLBACK (@gtk_main_quit), NULL);

  {*
   * VBox.
   *}

  vbox := gtk_vbox_new (FALSE, 0);
  gtk_container_add (GTK_CONTAINER (window), vbox);
  gtk_widget_show (vbox);

  {*
   * Drawing area for drawing OpenGL scene.
   *}

  drawing_area := gtk_drawing_area_new ();
  gtk_widget_set_size_request (drawing_area, 300, 300);

  {* Set OpenGL-capability to the widget. *}
  gtk_widget_set_gl_capability (drawing_area,
				glconfig,
				NULL,
				TRUE,
				GDK_GL_RGBA_TYPE);

  gtk_widget_add_events (drawing_area,
			 GDK_VISIBILITY_NOTIFY_MASK);

  g_signal_connect_after (G_OBJECT (drawing_area), 'realize',
                          G_CALLBACK (@init), NULL);
  g_signal_connect (G_OBJECT (drawing_area), 'configure_event',
		    G_CALLBACK (@reshape), NULL);
  g_signal_connect (G_OBJECT (drawing_area), 'expose_event',
		    G_CALLBACK (@draw), NULL);
  g_signal_connect (G_OBJECT (drawing_area), 'map_event',
		    G_CALLBACK (@map), NULL);
  g_signal_connect (G_OBJECT (drawing_area), 'unmap_event',
		    G_CALLBACK (@unmap), NULL);
  g_signal_connect (G_OBJECT (drawing_area), 'visibility_notify_event',
		    G_CALLBACK (@visible), NULL);

  g_signal_connect_swapped (G_OBJECT (window), 'key_press_event',
			    G_CALLBACK (@key), drawing_area);

  gtk_box_pack_start (GTK_BOX (vbox), drawing_area, TRUE, TRUE, 0);

  gtk_widget_show (drawing_area);

  {*
   * Simple quit button.
   *}

  button := gtk_button_new_with_label ('Quit');

  g_signal_connect (G_OBJECT (button), 'clicked',
		    G_CALLBACK (@gtk_main_quit), NULL);

  gtk_box_pack_start (GTK_BOX (vbox), button, FALSE, FALSE, 0);

  gtk_widget_show (button);

  {*
   * Show window.
   *}

  gtk_widget_show (window);

  {*
   * Main loop.
   *}

  gtk_main ();

end.
