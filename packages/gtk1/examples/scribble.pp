{

  Converted from C to Pascal by Frank Loemker
  <floemker@techfak.uni-bielefeld.de>
}
program scribble;
uses
  glib,Gdk,Gtk;

{ Backing pixmap for drawing area }
const
  pixmap : PGdkPixmap = NIL;

{ Create a new backing pixmap of the appropriate size }
function  configure_event (widget : PGtkWidget; event: PGdkEventConfigure):boolean; cdecl;
begin
  if pixmap<>NIL then
    gdk_pixmap_unref(pixmap);

  pixmap := gdk_pixmap_new(widget^.window,
                           widget^.allocation.width,
                           widget^.allocation.height,
                           -1);
  gdk_draw_rectangle (pixmap,
                      PGtkStyle(widget^.thestyle)^.white_gc,
                      gint(true),
                      0, 0,
                      widget^.allocation.width,
                      widget^.allocation.height);

  configure_event := TRUE;
end;

{ Redraw the screen from the backing pixmap }
function  expose_event (widget : PGtkWidget ; event : PGdkEventExpose ) : boolean; cdecl;
begin
  gdk_draw_pixmap(widget^.window,
                  PGtkStyle(widget^.thestyle)^.fg_gc[gtk_WIDGET_STATE (widget)],
                  pixmap,
                  event^.area.x, event^.area.y,
                  event^.area.x, event^.area.y,
                  event^.area.width, event^.area.height);
  expose_event:= FALSE;
end;

{ Draw a rectangle on the screen }
procedure draw_brush (widget : PGtkWidget ; x, y: gint16);
var update_rect : TGdkRectangle;
begin
  update_rect.x := x - 5;
  update_rect.y := y - 5;
  update_rect.width := 10;
  update_rect.height := 10;
  gdk_draw_rectangle (pixmap,
                      PGtkStyle(widget^.thestyle)^.black_gc,
                      gint(true),
                      update_rect.x, update_rect.y,
                      update_rect.width, update_rect.height);
  gtk_widget_draw (widget, @update_rect);
end;

function  button_press_event (widget : PGtkWidget ; event: PGdkEventButton ) : boolean; cdecl;
begin
  if (event^.button = 1) and (pixmap <> NIL) then begin
    draw_brush (widget, trunc(event^.x), trunc(event^.y));
  end;
  button_press_event := TRUE;
end;

function  motion_notify_event (widget : PGtkWidget ; event: PGdkEventMotion ) : boolean; cdecl;
var x, y : longint ;
  state  : longint;
begin
  if (event^.is_hint<>0) then begin
    gdk_window_get_pointer (event^.window, @x, @y, @state);
  end else begin
    x := trunc(event^.x);
    y := trunc(event^.y);
    state := event^.state;
  end;

  if ((state and gdk_BUTTON1_MASK)<>0) and (pixmap <> NIL) then
    draw_brush (widget, x, y);

  motion_notify_event := TRUE;
end;

procedure quit;
begin
  gtk_exit (0);
end;

var window, drawing_area, vbox, button : PGtkWidget;
begin
  gtk_init (@argc, @argv);
  gtk_rc_init;

  window := gtk_window_new (gtk_WINDOW_TOPLEVEL);
  gtk_widget_set_name (window, 'Test Input');

  vbox := gtk_vbox_new (false, 0);
  gtk_container_add (PGtkCONTAINER (window), vbox);
  gtk_widget_show (vbox);

  gtk_signal_connect (PGtkOBJECT (window), 'destroy',
                      gtk_SIGNAL_FUNC (@quit), NIL);

  { Create the drawing area }

  drawing_area := gtk_drawing_area_new ();
  gtk_drawing_area_size (PGtkDRAWINGAREA (drawing_area), 200, 200);
  gtk_box_pack_start (PGtkBOX (vbox), drawing_area, true, true, 0);

  gtk_widget_show (drawing_area);

  { Signals used to handle backing pixmap }

  gtk_signal_connect (PGtkOBJECT (drawing_area), 'expose_event',
                      gtk_SIGNAL_FUNC (@expose_event), NIL);
  gtk_signal_connect (PGtkOBJECT(drawing_area),'configure_event',
                      gtk_SIGNAL_FUNC (@configure_event), NIL);

  { Event signals }

  gtk_signal_connect (PGtkOBJECT (drawing_area), 'motion_notify_event',
                      gtk_SIGNAL_FUNC (@motion_notify_event), NIL);
  gtk_signal_connect (PGtkOBJECT (drawing_area), 'button_press_event',
                      gtk_SIGNAL_FUNC (@button_press_event), NIL);

  gtk_widget_set_events (drawing_area, gdk_EXPOSURE_MASK
                         or gdk_LEAVE_NOTIFY_MASK
                         or gdk_BUTTON_PRESS_MASK
                         or gdk_POINTER_MOTION_MASK
                         or gdk_POINTER_MOTION_HINT_MASK);

  { .. And a quit button }
  button := gtk_button_new_with_label ('Quit');
  gtk_box_pack_start (PGtkBOX (vbox), button, false, false, 0);

  gtk_signal_connect_object (PGtkOBJECT (button), 'clicked',
                             gtk_SIGNAL_FUNC (@gtk_widget_destroy),
                             PGtkOBJECT (window));
  gtk_widget_show (button);
  gtk_widget_show (window);

  gtk_main ();
end.
