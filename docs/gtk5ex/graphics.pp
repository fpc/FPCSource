program graphics;

{$mode objfpc}
{$h+}

uses glib,gdk,gtk,sysutils;

var
  window,
  area : PGtkWidget;

Function close_application( widget : PGtkWidget ;
                        event : PGdkEvent;
                        data : gpointer) : boolean; cdecl;
Begin
  gtk_main_quit();
  close_application := false;
End;

Function AllocateColor(R,G,B : Integer; Widget : PGtkWidget) : PGdkColor;

begin
  Result:=New(PgdkColor);
  With Result^ do
    begin
    Pixel:=0;
    Red:=R;
    Blue:=B;
    Green:=G;
    end;
  gdk_colormap_alloc_color(gtk_widget_get_colormap(Widget),Result,true,False);
end;


function Exposed(Widget: PGtkWidget;event : PGdkEventExpose; Data : gpointer) : Integer; cdecl;

Const
  Triangle : Array[1..4] of TgdkPoint =
            ((X:10;Y:195),(X:110;Y:195),(X:55;Y:145),(X:10;Y:195));
  LineStyles : Array[1..5] of TgdkLineStyle =
          (GDK_LINE_SOLID, GDK_LINE_ON_OFF_DASH, GDK_LINE_DOUBLE_DASH,
          GDK_LINE_ON_OFF_DASH, GDK_LINE_SOLID);
  capstyles : Array[1..5] of TgdkCapStyle =
          (GDK_CAP_ROUND,GDK_CAP_NOT_LAST, GDK_CAP_BUTT,  GDK_CAP_PROJECTING,
          GDK_CAP_NOT_LAST);

  FontName : Pchar =
   '-*-helvetica-bold-r-normal--*-120-*-*-*-*-iso8859-1';

Var
  SegTriangle : Array[1..3] of TgdkSegment;
  Win : pgdkWindow;
  gc : PgdkGC;
  i,seg : Integer;
  font : PgdkFont;
  Angle1,Angle2 : Longint;

begin
  gc:=gdk_gc_new(widget^.Window);
  Win:=widget^.window;
  With Event^.area do
    gdk_window_clear_area (win,x,y,width,height);
  gdk_gc_set_foreground(gc,allocatecolor(0,0,0,Widget));
  gdk_draw_rectangle(win,gc,0,5,5,590,390);
//  gdk_draw_rectangle(win,gc,-1,15,15,570,370);
  gdk_gc_set_foreground(gc,allocatecolor(0,0,$ffff,Widget));
  for I:=10 to 50 do
    gdk_draw_point(win,gc,I*10,100);
  gdk_gc_set_foreground(gc,allocatecolor($ffff,0,0,Widget));
  for I:=10 to 50 do
    begin
    gdk_gc_set_line_attributes(gc,6,LineStyles[i div 10],CapStyles[i div 10],GDK_JOIN_MITER);
    gdk_draw_line(win,gc,I*10,20,I*10,90)
    end;
  gdk_gc_set_line_attributes(gc,1,GDK_LINE_SOLID,GDK_CAP_BUTT,GDK_JOIN_MITER);
  gdk_gc_set_foreground(gc,allocatecolor($ffff,0,$ffff,Widget));
  seg:=(360 div 20) * 64;
  For I:=1 to 20 do
    gdk_draw_arc(win,gc,0,220-I*4,200-i*4,8*i,8*i,i*seg,seg*19);
  For I:=1 to 20 do
    gdk_draw_arc(win,gc,-1,380-I*4,200-i*4,8*i,8*i,(i-1)*seg,seg);
  gdk_gc_set_foreground(gc,allocatecolor(0,$ffff,$ffff,Widget));
  gdk_draw_polygon(win,gc,0,@triangle[1],4);
  For I:=1 to 4 do
    Triangle[i].Y:=400-Triangle[i].y;
  gdk_draw_polygon(win,gc,-1,@triangle[1],4);
  gdk_gc_set_foreground(gc,allocatecolor(0,$ffff,0,Widget));
  For I:=1 to 4 do
    Triangle[i].X:=600-Triangle[i].x;
  gdk_draw_lines(win,gc,@triangle[1],4);
  For I:=1 to 3 do
    begin
    SegTriangle[i].X1:=Triangle[i].X;
    SegTriangle[i].Y1:=400-Triangle[i].Y;
    SegTriangle[i].X2:=Triangle[i+1].X;
    SegTriangle[i].Y2:=400-Triangle[i+1].Y;
    end;
  gdk_draw_segments(win,gc,@segtriangle[1],3);
  font:=gdk_font_load(FontName);
  gdk_gc_set_foreground(gc,allocatecolor($ffff,$ffff,0,Widget));
  For I:=1 to 4 do
    gdk_draw_string(win,font,gc,I*100,300,Pchar(format('String %d',[i])));
  result:=0;
end;

Begin
  // Initialize GTK and create the main window
  gtk_init( @argc, @argv );
  window := gtk_window_new( GTK_WINDOW_TOPLEVEL );
  gtk_window_set_policy(PgtkWindow(Window),0,0,1);
  gtk_signal_connect (GTK_OBJECT (window), 'delete_event',
          GTK_SIGNAL_FUNC( @close_application ), NIL);
  gtk_container_set_border_width (GTK_CONTAINER (window), 10);
  area := gtk_drawing_area_new();
  gtk_container_add( GTK_CONTAINER(window), Area);
  gtk_signal_connect (GTK_OBJECT (area),'expose_event',
                      GTK_SIGNAL_FUNC(@Exposed),Nil);
  gtk_drawing_area_size (PGTKDRAWINGAREA(area),600,400);
  gtk_widget_show_all( window );
  gtk_main();
end.
