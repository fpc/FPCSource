Unit frmprops;

{$mode objfpc}

Interface

uses glib,gdk,gtk,sysutils;

Const
  NrTableLines = 8;
  CheckBoxLineStart = 5;


Type
  TFilePropertiesDialog = Record
    Window : PgtkDialog;
    Table  : PGtkTable;
    OkButton : PGtkButton;
    Labels : Array[0..1,0..NrTableLines] of PGtkLabel;
    CheckBoxes : Array[CheckBoxLineStart..NrTableLines] of PgtkCheckButton;
  end;
  PFilePropertiesDialog = ^TFilePropertiesDialog;

Function NewFilePropertiesDialog(FileName : String) : PFilePropertiesDialog;
Procedure ShowFilePropertiesDialog(Dialog : PFilePropertiesDialog);

Implementation

uses Futils;

Const
  SPropsTitle : PChar = 'File properties';
  SOk : PChar = ' OK ';
  SFile = ' File.';
  LabelTexts :  Array[0..NrTableLines] of Pchar = (
               'Name',
               'Directory',
               'Type',
               'Size',
               'Date',
               'Attributes',
               '',
               '',
               ''
               );

  CheckBoxTexts :  Array[CheckBoxLineStart..NrTableLines] of Pchar = (
               'Read-only',
               'Archive',
               'Hidden',
               'System'
               );

procedure DestroyPropDialog(Widget : PGtkWidget; Dlg : PFilePropertiesDialog);cdecl;

begin
  Dispose(Dlg);
end;


Function NewFilePropertiesDialog(FileName : String) : PFilePropertiesDialog;

Const
  CheckAttrs : Array [CheckBoxLineStart..NrTableLines] of Integer
             = (faReadOnly,faArchive,faHidden,faSysFile);

Var
  Info : TSearchRec;
  I : Longint;

begin
  Result:=New(PFilePropertiesDialog);
  With Result^ do
    begin
    Window:=PgtkDialog(gtk_dialog_new);
    gtk_window_set_title(PgtkWindow(Window),SPropsTitle);
    gtk_window_set_modal(PgtkWindow(Window),True);
    gtk_window_set_policy(PgtkWindow(Window),0,0,0);
    gtk_window_set_position(PGtkWindow(Window),GTK_WIN_POS_CENTER);
    OkButton:=PGtkButton(gtk_button_new_with_label(SOK));
    gtk_box_pack_start(PgtkBox(Window^.action_area),PGtkWidget(Okbutton),False,False,5);
    gtk_window_set_focus(PGtkWindow(Window),PGtkWidget(OkButton));
    gtk_widget_show(PGtkWidget(OkButton));
    Table:=PgtkTable(gtk_table_new(NrTableLines+1,2,TRUE));
    gtk_box_pack_start(PGtkBox(Window^.vbox),PGtkWidget(Table),True,True,10);
    For I:=0 to NrTableLines do
      begin
      Labels[0,i]:=PGtkLabel(gtk_label_new(LabelTexts[i]));
      gtk_label_set_justify(Labels[0,I],GTK_JUSTIFY_RIGHT);
      gtk_table_attach_defaults(Table,PgtkWidget(Labels[0,I]),0,1,I,I+1);
      end;
    For I:=0 to CheckboxLineStart-1 do
      begin
      Labels[1,i]:=PGtkLabel(gtk_label_new(''));
      gtk_label_set_justify(Labels[1,I],GTK_JUSTIFY_LEFT);
      gtk_table_attach_defaults(Table,PgtkWidget(Labels[1,I]),1,2,I,I+1);
      end;
    For I:=CheckboxLineStart to NrTableLines do
      begin
      checkBoxes[i]:=PgtkCheckButton(gtk_check_button_new_with_label(CheckBoxTexts[I]));
      gtk_widget_set_state(PGtKWidget(CheckBoxes[i]),GTK_STATE_INSENSITIVE);
      gtk_table_attach_defaults(Table,PgtkWidget(CheckBoxes[i]),1,2,I,I+1);
      end;
    gtk_label_set_text(Labels[1,0],PChar(ExtractFileName(FileName)));
    gtk_label_set_text(Labels[1,1],PChar(ExtractFilePath(FileName)));
    gtk_label_set_text(Labels[1,2],PChar(ExtractFileExt(FileName)+SFile));
    If FindFirst(FileName,faAnyFile,Info)=0 Then
      begin
      gtk_label_set_text(Labels[1,3],PChar(FileSizeToString(Info.Size)));
      gtk_label_set_text(Labels[1,4],PChar(DateTimeToStr(FileDateToDateTime(Info.Time))));
      For I:=CheckboxLineStart to NrTableLines do
        If (CheckAttrs[i] and Info.Attr)=CheckAttrs[i] then
          gtk_toggle_button_set_active(PgtkToggleButton(CheckBoxes[I]),True);
      FindClose(Info);
      end;
    gtk_signal_connect(PGtkObject(Window),'destroy',
                       TGTKSignalFunc(@DestroyPropDialog),Result);
    gtk_signal_connect_object(PgtkObject(OKButton),'clicked',
                      GTK_SIGNAL_FUNC(@gtk_widget_destroy),
                      PGTKOBJECT(Window));
    end;
end;

Procedure ShowFilePropertiesDialog(Dialog : PFilePropertiesDialog);

begin
  gtk_widget_show_all(PgtkWidget(Dialog^.Window));
end;

end.