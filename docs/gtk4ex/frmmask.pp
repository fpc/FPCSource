unit frmmask;

{$mode objfpc}

Interface

uses glib,gdk,gtk;

Type
  TMaskCallBack = Procedure (Mask : String; Data : Pointer);
  TMaskForm = Record
    Window : PGtkDialog;
    EMask : PGtkEntry;
    LEMAsk : PGtkLabel;
    HBox : PGtkBox;
    OKButton,
    CancelButton : PGtkButton;
    Mask : ShortString;
    CallBack : TMaskCallBack;
    CallBackData : Pointer;
  end;
  PMaskForm = ^TMaskForm;

Function NewMaskForm : PMaskForm;

Implementation

Const
  SEnterMask : PChar = 'Enter new file mask';
  SNewMask : PChar = 'New mask';
  SOK : PChar = ' OK ';
  SCancel : PChar = ' Cancel ';

Procedure ApplyMask(Widget : PGtkWidget; Window : PMaskForm);cdecl;

begin
  With Window^ do
    begin
    Mask:=StrPas(gtk_entry_get_text(EMask));
    If (CallBack<>Nil) then
      CallBack(Mask,CallBackData);
    end;
end;

Procedure DestroyMaskForm(Widget : PGtkWidget; Window : PMaskForm);cdecl;

begin
  Dispose(Window);
end;

Function NewMaskForm : PMaskForm;

begin
  Result:=New(PMaskForm);
  With Result^ do
    begin
    Window:=PGtkDialog(gtk_dialog_new);
    gtk_window_set_title(PgtkWindow(Window),SEnterMask);
    gtk_widget_set_usize(PGtkWidget(Window),350,150);
    gtk_window_set_policy(PgtkWindow(Window),0,0,0);
    gtk_window_set_position(PGtkWindow(Window),GTK_WIN_POS_CENTER);
    OKButton:=PGtkButton(gtk_button_new_with_label(SOK));
    CancelButton:=PGtkButton(gtk_button_new_with_label(SCancel));
    gtk_box_pack_end(PgtkBox(Window^.action_area),PGtkWidget(Okbutton),False,False,5);
    gtk_box_pack_end(PgtkBox(Window^.action_area),PGtkWidget(Cancelbutton),False,False,5);
    Emask:=PGtkEntry(gtk_entry_new_with_max_length(255));
    LEMask:=PGtkLabel(gtk_label_new(SNewMask));
    HBox:=PGtkBox(gtk_hbox_new(False,8));
    gtk_box_pack_start(PgtkBox(HBox),PGtkWidget(LEMask),True,False,0);
    gtk_box_pack_start(PgtkBox(HBox),PGtkWidget(EMask),True,False,0);
    gtk_box_pack_start(PGtkBox(Window^.vbox),PGtkWidget(HBox),True,True,10);
    gtk_window_set_modal(PGtkWindow(Window),TRUE);
    gtk_signal_connect(PgtkObject(OKButton),'clicked',
                      TGtkSignalFunc(@ApplyMask),Result);
    gtk_signal_connect_object(PgtkObject(OKButton),'clicked',
                      GTK_SIGNAL_FUNC(@gtk_widget_destroy),
                      PGTKOBJECT(Window));
    gtk_signal_connect_object(PgtkObject(CancelButton),'clicked',
                      GTK_SIGNAL_FUNC(@gtk_widget_destroy),
                      PGTKOBJECT(Window));
    gtk_signal_connect(PgtkObject(Window),'destroy',
                      TGtkSignalFunc(@DestroyMaskForm),Result);
    CallBack:=Nil;
    CallBackdata:=Nil;
    end;
end;

end.