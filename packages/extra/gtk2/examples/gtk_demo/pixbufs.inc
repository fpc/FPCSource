(* Pixbufs
 *
 * A GdkPixbuf represents an image, normally in RGB or RGBA format.
 * Pixbufs are normally used to load files from disk and perform
 * image scaling.
 *
 * This demo is not all that educational, but looks cool. It was written
 * by Extreme Pixbuf Hacker Federico Mena Quintero. It also shows
 * off how to use GtkDrawingArea to do a simple animation.
 *
 * Look at the Image demo for additional pixbuf usage examples.
 *
 *)


const
  FRAME_DELAY = 50;

  BACKGROUND_NAME = 'background.jpg';

  image_names : array [1..8] of pchar = (
    'apple-red.png',
    'gnome-applets.png',
    'gnome-calendar.png',
    'gnome-foot.png',
    'gnome-gmush.png',
    'gnome-gimp.png',
    'gnome-gsame.png',
    'gnu-keys.png');

  N_IMAGES = high(image_names);

(* demo window *)
var
  pixbufs_window      : PGtkWidget;

(* Current frame *)
  pixbufs_frame       : PGdkPixbuf;

(* Background image *)
  pixbufs_background  : PGdkPixbuf;

  pixbufs_back_width,
  pixbufs_back_height : gint;


(* Images *)
  images              : array [1..N_IMAGES] of PGdkPixbuf;

(* Widgets *)
  pixbufs_da          : PGtkWidget;

(* Loads the images for the demo and returns whether the operation succeeded *)
function load_pixbufs (error : PPGError): gboolean;
var
  i        :   gint;
  filename :   pgchar;

begin

  if pixbufs_background <> NULL then  begin
    load_pixbufs := TRUE;  (* already loaded earlier *)
    exit;
  end;

  (* demo_find_file() looks in the the current directory first,
   * so you can run gtk-demo without installing GTK, then looks
   * in the location where the file is installed.
   *)

  filename := demo_find_file (BACKGROUND_NAME, error);
  if filename = NULL then begin
    load_pixbufs := FALSE;     (* note that "error" was filled in and returned *)
    exit;
  end;

  pixbufs_background := gdk_pixbuf_new_from_file (filename, error);
  g_free (filename);

  if pixbufs_background = NULL then begin
    load_pixbufs := FALSE; (* Note that "error" was filled with a GError *)
    exit;
  end;

  pixbufs_back_width  := gdk_pixbuf_get_width (pixbufs_background);
  pixbufs_back_height := gdk_pixbuf_get_height (pixbufs_background);

  for i := 1 to N_IMAGES do
  begin
      filename := demo_find_file (image_names[i], error);

      if filename = NULL then begin
        load_pixbufs := FALSE; (* Note that "error" was filled with a GError *)
        exit
      end;

      images[i] := gdk_pixbuf_new_from_file (filename, error);
      g_free (filename);

      if images[i] = NULL then begin
        load_pixbufs := FALSE;      (* Note that "error" was filled with a GError *)
        exit;
      end;

  end;

  load_pixbufs := TRUE;
end;

(* Expose callback for the drawing area *)
function expose_cb (widget : PGtkWidget;
                    event  : PGdkEventExpose;
                    data   : gpointer): gint; cdecl;
var
  pixels     : pguchar;
  rowstride  : gint;

begin
  rowstride := gdk_pixbuf_get_rowstride (pixbufs_frame);

  pixels := gdk_pixbuf_get_pixels (pixbufs_frame) + rowstride * event^.area.y + event^.area.x * 3;

  gdk_draw_rgb_image_dithalign (widget^.window,
                                widget^.style^.black_gc,
                                event^.area.x, event^.area.y,
                                event^.area.width, event^.area.height,
                                GDK_RGB_DITHER_NORMAL,
                                pixels, rowstride,
                                event^.area.x, event^.area.y);

  expose_cb := 1;
end;

const
  CYCLE_LEN = 60;

var
  pixbufs_frame_num : integer;


(* Timeout handler to regenerate the frame *)
function timeout (data : gpointer): gboolean;  cdecl;
var
  f      : double;
  i      : integer;
  xmid,
  ymid,
  radius : double;

  ang, r, k  : double;

  alpha,
  xpos, ypos,
  iw, ih     : integer;

  r1, r2,
  dest       : TGdkRectangle;


begin
  gdk_pixbuf_copy_area (pixbufs_background, 0, 0, pixbufs_back_width,
                        pixbufs_back_height, pixbufs_frame, 0, 0);

  f := double(pixbufs_frame_num mod CYCLE_LEN) / CYCLE_LEN;

  xmid := pixbufs_back_width / 2.0;
  ymid := pixbufs_back_height / 2.0;


  radius := min (ymid, xmid) / 2.0;

  for i := 1 to N_IMAGES do
  begin
    ang := 2.0 * G_PI * double (i / N_IMAGES) - f * 2.0 * G_PI;

    iw := gdk_pixbuf_get_width (images[i]);
    ih := gdk_pixbuf_get_height (images[i]);

    r := radius + (radius / 3.0) * sin (f * 2.0 * G_PI);

    xpos :=  floor (xmid + r * cos (ang) - iw / 2.0 + 0.5);
    ypos :=  floor (ymid + r * sin (ang) - ih / 2.0 + 0.5);

    if (i and 1) <> 0 then k:= sin (f * 2.0 * G_PI)
    else k:= cos (f * 2.0 * G_PI);

    k := 2.0 * k * k;
    k := MAX (0.25, k);

    r1.x := xpos;
    r1.y := ypos;
    r1.width := round(iw * k);
    r1.height := round(ih * k);

    r2.x := 0;
    r2.y := 0;
    r2.width  := pixbufs_back_width;
    r2.height := pixbufs_back_height;

    if gdk_rectangle_intersect (@r1, @r2, @dest) then begin
      if (i and 1) <> 0 then
        alpha := round (MAX (127, abs (255 * sin (f * 2.0 * G_PI))))
      else
        alpha := round (MAX (127, abs (255 * cos (f * 2.0 * G_PI))));

      gdk_pixbuf_composite (images[i],
                              pixbufs_frame,
                              dest.x, dest.y,
                              dest.width, dest.height,
                              xpos, ypos,
                              k, k,
                              GDK_INTERP_NEAREST,
                  alpha);
    end;
  end;

  gtk_widget_queue_draw (pixbufs_da);

  inc(pixbufs_frame_num);
  exit (TRUE);
end;

var
  pixbufs_timeout_id : guint;

procedure pixbufs_cleanup_callback (_object : PGtkObject;
                            data    : gpointer);    cdecl;
begin
  g_source_remove (pixbufs_timeout_id);
  pixbufs_timeout_id := 0;
end;



function do_pixbufs        : PGtkWidget;
var
  error  : PGError;
  dialog : PGtkWidget;

begin
  if pixbufs_window = NULL then
  begin

    pixbufs_window := gtk_window_new (GTK_WINDOW_TOPLEVEL);
    gtk_window_set_title (GTK_WINDOW (pixbufs_window), 'Pixbufs');
    gtk_window_set_resizable (GTK_WINDOW (pixbufs_window), FALSE);

    g_signal_connect (pixbufs_window, 'destroy', G_CALLBACK (@gtk_widget_destroyed), @pixbufs_window);
    g_signal_connect (pixbufs_window, 'destroy', G_CALLBACK (@pixbufs_cleanup_callback), NULL);


    error := NULL;
    if not load_pixbufs (@error) then
    begin
          dialog := gtk_message_dialog_new (GTK_WINDOW (pixbufs_window),
                                           GTK_DIALOG_DESTROY_WITH_PARENT,
                                           GTK_MESSAGE_ERROR,
                                           GTK_BUTTONS_CLOSE,
                                           'Failed to load an image: %s',
                                           [error^.message]);

          g_error_free (error);

          g_signal_connect (dialog, 'response',
                            G_CALLBACK (@gtk_widget_destroy), NULL);

          gtk_widget_show (dialog);
    end else
    begin
          gtk_widget_set_size_request (pixbufs_window, pixbufs_back_width, pixbufs_back_height);

          pixbufs_frame := gdk_pixbuf_new (GDK_COLORSPACE_RGB, FALSE, 8, pixbufs_back_width, pixbufs_back_height);

          pixbufs_da := gtk_drawing_area_new ();

          g_signal_connect (pixbufs_da, 'expose_event',
                            G_CALLBACK (@expose_cb), NULL);

          gtk_container_add (GTK_CONTAINER (pixbufs_window), pixbufs_da);

          pixbufs_timeout_id := gtk_timeout_add (FRAME_DELAY, @timeout, NULL);
    end;
  end;

  if not GTK_WIDGET_VISIBLE (pixbufs_window) then
    gtk_widget_show_all (pixbufs_window)
  else begin
    gtk_widget_destroy (pixbufs_window);
    pixbufs_window := NULL;
  end;

  do_pixbufs := pixbufs_window;
end;
