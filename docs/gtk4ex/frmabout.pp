unit frmabout;

{$mode objfpc}

Interface

uses glib,gdk,gtk;

Type
  TAboutForm = Record
    Window : PGtkDialog;
    OkButton : PgtkButton;
    InfoLabel : PGtkLabel;
  end;
  PAboutForm = ^TAboutForm;

Function NewAboutForm : PAboutForm;
Procedure ShowAboutForm(Form : PAboutForm);

Implementation

Const
  SInfo : PChar = 'File explorer demo'#10'Florian Klaempfl'#10'Michael Van Canneyt';
  SAboutTitle : Pchar = 'About File explorer';

procedure DestroyAbout(Widget : PGtkWidget; About : PAboutForm);cdecl;

begin
  Dispose(About);
end;

Function NewAboutForm : PAboutForm;

begin
  Result:=New(PAboutForm);
  With Result^ do
    begin
    Window:=PgtkDialog(gtk_dialog_new);
    gtk_window_set_modal(PgtkWindow(Window),True);
    gtk_window_set_title(PgtkWindow(Window),SAboutTitle);
    gtk_widget_set_usize(PGtkWidget(Window),250,150);
    gtk_window_set_policy(PgtkWindow(Window),0,0,0);
    gtk_window_set_position(PGtkWindow(Window),GTK_WIN_POS_CENTER);
    OkButton:=PGtkButton(gtk_button_new_with_label(' Ok '));
    gtk_box_pack_start(PgtkBox(Window^.action_area),PGtkWidget(Okbutton),False,False,5);
    gtk_window_set_focus(PGtkWindow(Window),PGtkWidget(OkButton));
    gtk_widget_show(PGtkWidget(OkButton));
    InfoLabel:=PgtkLabel(gtk_label_new(SInfo));
    gtk_box_pack_start(PGtkBox(Window^.vbox),PGtkWidget(InfoLabel),True,True,10);
    gtk_widget_show(PGtkWidget(InfoLabel));
    gtk_signal_connect(PGtkObject(Window),'destroy',
                       TGTKSignalFunc(@DestroyAbout),Result);
    gtk_signal_connect_object(PgtkObject(OKButton),'clicked',
                      GTK_SIGNAL_FUNC(@gtk_widget_destroy),
                      PGTKOBJECT(Window));
    end;
end;

Procedure ShowAboutForm(Form : PAboutForm);

begin
  gtk_window_set_modal(PgtkWindow(Form^.Window),True);
  gtk_widget_show(PgtkWidget(Form^.Window));
end;

end.