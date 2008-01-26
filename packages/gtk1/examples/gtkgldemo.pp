{$MODE DELPHI}

{$ifdef win32}
  {$APPTYPE GUI}
{$endif}

program gtkgldemo;

uses  glib, gdk, gtk, gtkglarea,gl;


function init(widget:PGtkWidget ):gint;cdecl;

begin
  if (gint(True)=gtk_gl_area_make_current(PGtkGLArea(widget))) then
    begin

      glViewport(0,0, widget^.allocation.width, widget^.allocation.height);
      glMatrixMode(GL_PROJECTION);
      glLoadIdentity();
      glOrtho(0,100, 100,0, -1,1);
      glMatrixMode(GL_MODELVIEW);
      glLoadIdentity();

    end;

  init:= gint(TRUE);
end;




(* When widget is exposed its contents are redrawn. *)

function draw(widget: PGtkWidget; event: PGdkEventExpose): gint; cdecl;

begin

  (* Draw only last expose. *)
  if (event^.count > 0) then
    exit(gint(TRUE));

  (* OpenGL functions can be called only if make_current returns true *)
  if (gint(True) = gtk_gl_area_make_current(PGtkGLArea(widget))) then
    begin

     glClearColor(0,0,0,1);
     glClear(GL_COLOR_BUFFER_BIT);
     glColor3f(1,1,1);
     glBegin(GL_TRIANGLES);
     glVertex2f(10,10);
     glVertex2f(10,90);
     glVertex2f(90,90);
     glEnd();

     (* Swap backbuffer to front *)
     gtk_gl_area_swapbuffers(PGtkGLArea(widget));

    end;

  exit(gint(TRUE));
end;

(* When glarea widget size changes, viewport size is set to match the new size *)

function  reshape(widget:PGtkWidget; event: PGdkEventConfigure):gint; cdecl;
begin

  (* OpenGL functions can be called only if make_current returns true *)
  if (gint(True) = gtk_gl_area_make_current(PGtkGLArea(widget))) then
    begin

     glViewport(0, 0, widget^.allocation.width, widget^.allocation.height);

    end;
  exit( gint(TRUE));
end;

var
   window,glarea: PGtkWidget;

  (* Attribute list for gtkglarea widget. Specifies a
     list of Boolean attributes and enum/integer
     attribute/value pairs. The last attribute must be
     GDK_GL_NONE. See glXChooseVisual manpage for further
     explanation.
  *)

  const
     attrlist: array [1..11] of LongInt=
                    ( GDK_GL_RGBA,
                    GDK_GL_RED_SIZE, 1,
                    GDK_GL_GREEN_SIZE, 1,
                    GDK_GL_BLUE_SIZE, 1,
                      GDK_GL_DEPTH_SIZE,1,
                    GDK_GL_DOUBLEBUFFER,
                    GDK_GL_None
                    );

(*  int attrlist[] = {
    GDK_GL_RGBA,
    GDK_GL_RED_SIZE,1,
    GDK_GL_GREEN_SIZE,1,
    GDK_GL_BLUE_SIZE,1,
    GDK_GL_DOUBLEBUFFER,
    GDK_GL_NONE
  };
*)

begin
  (* OpenGL functions can be called only if make_current returns true *)
{  if not InitGl then begin
    WriteLn('OpenGL is not supported on this system');
    Halt(2);
  end;}


  (* initialize gtk *)
  gtk_init(@argc, @argv);

  (* Attribute list for gtkglarea widget. Specifies a
      list of Boolean attributes and enum/integer
      attribute/value pairs. The last attribute must be
      GDK_GL_NONE. See glXChooseVisual manpage for further
      explanation.
   *)


  (* vendor dependent version info string *)
 { info_str = gdk_gl_get_info();
  g_print(info_str);
  g_free(info_str);}

  (* Check if OpenGL is supported. *)
 { if (gdk_gl_query() = FALSE) then
  begin
    g_print("OpenGL not supported\n");
    return 0;
  end;}




  (* Create new top level window. *)
    window := gtk_window_new( GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title(GTK_WINDOW(window), 'OpenGL Output');
  gtk_container_set_border_width(GTK_CONTAINER(window), 10);

  (* Quit form main if got delete event *)
    gtk_signal_connect(GTK_OBJECT(window), 'delete_event',
                     GTK_SIGNAL_FUNC(@gtk_main_quit), NULL);


  (* You should always delete gtk_gl_area widgets before exit or else
       GLX contexts are left undeleted, this may cause problems (=core dump)
       in some systems.
       Destroy method of objects is not automatically called on exit.
       You need to manually enable this feature. Do gtk_quit_add_destroy()
       for all your top level windows unless you are certain that they get
       destroy signal by other means.
    *)
    gtk_quit_add_destroy(1, GTK_OBJECT(window));


    (* Create new OpenGL widget. *)
  glarea := GTK_WIDGET(gtk_gl_area_new(pgint(@attrlist)));

  (* Events for widget must be set before X Window is created *)
    gtk_widget_set_events(GTK_WIDGET(glarea),
                        GDK_EXPOSURE_MASK or
                        GDK_BUTTON_PRESS_MASK);

    (* Connect signal handlers *)
    (* Redraw image when exposed. *)
    gtk_signal_connect(GTK_OBJECT(glarea), 'expose_event', GTK_SIGNAL_FUNC(@draw), Nil);

    (* When window is resized viewport needs to be resized also. *)
    gtk_signal_connect(GTK_OBJECT(glarea), 'configure_event', GTK_SIGNAL_FUNC(@reshape), Nil);

    (* Do initialization when widget has been realized. *)
    gtk_signal_connect(GTK_OBJECT(glarea), 'realize', GTK_SIGNAL_FUNC(@init), Nil);

    (* set minimum size *)
    gtk_widget_set_usize(GTK_WIDGET(glarea), 400,400);

    (* put glarea into window and show it all *)
    gtk_container_add(GTK_CONTAINER(window),GTK_WIDGET(glarea));


    gtk_widget_show(GTK_WIDGET(glarea));

    gtk_widget_show(GTK_WIDGET(window));

  gtk_main();

end.
