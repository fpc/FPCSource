program breakout;

{$mode objfpc}

uses glib,gdk,gtk,blocks;

Type
  TBreakOutWindow = Class(TObject)
  Public
    window,
    area : PGtkWidget;
    BreakOut : TBreakOut;
  end;

Var
  GameWindow : TBreakOutWindow;

Function Close( widget : PGtkWidget ;
                event : PGdkEvent;
                data : gpointer) : boolean; cdecl;
Begin
  gtk_main_quit();
  Close := false;
End;

function Exposed(Widget: PGtkWidget;
                 event : PGdkEventExpose;
                 Data : gpointer) : Integer; cdecl;

begin
  TBreakOutWindow(Data).BreakOut.Draw(Event);
  result:=0;
end;

function KeyPress (Widget: PGtkWidget;
                   event : PGdkEventKey;
                   Data : gpointer) : Integer; cdecl;

begin
  with TBreakOutWindow(Data).BreakOut do
    Case event^.keyval of
      gdk_left  : Pad.Goleft;
      gdk_right : Pad.GoRight;
      gdk_down  : Pad.Stop;
      Ord(' ')  : NextBall;
    end;
  Result:=0;
end;

function Step (data : Gpointer): integer;cdecl;

Var
 Rect : TGdkRectangle;

begin
  With TBreakOutWindow(Data) do
    begin
    With Breakout do
      begin
      Step;
      Draw(Nil);
      end;
    end;
  Result:=integer(True);
end;

Begin
  gtk_init( @argc, @argv );
  GameWindow:=TBreakOutWindow.Create;
  With GameWindow do
    begin
    window := gtk_window_new( GTK_WINDOW_TOPLEVEL );
    gtk_window_set_policy(PgtkWindow(Window),0,0,1);
    gtk_signal_connect (GTK_OBJECT (window), 'delete_event',
            GTK_SIGNAL_FUNC(@Close), NIL);
    gtk_container_set_border_width (GTK_CONTAINER (window), 10);
    area := gtk_drawing_area_new();
    gtk_container_add( GTK_CONTAINER(window), Area);
    BreakOut:=TBreakOut.Create(area);
    With BreakOut.BlockList do
      begin
      TotalRows:=20;
      TotalColumns:=10;
      StartRow:=15;
      BlockRows:=5;
      BlockSpacing:=2;
      end;
    gtk_signal_connect (GTK_OBJECT (area),'expose_event',
                        GTK_SIGNAL_FUNC(@Exposed),GameWindow);
    gtk_drawing_area_size (PGTKDRAWINGAREA(area),600,400);
    gtk_widget_set_events(window,GDK_KEY_RELEASE_MASK);
    gtk_signal_connect(PGTKObject(Window),'key_press_event',
                       GTK_SIGNAL_FUNC(@KeyPress),GameWindow);
    gtk_timeout_add(50,@Step,GameWindow);
    gtk_widget_show_all( window );
    gtk_main();
    end;
End.

end.
