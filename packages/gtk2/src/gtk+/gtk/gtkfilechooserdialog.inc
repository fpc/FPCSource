
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
  PGtkFileChooserDialogClass = ^TGtkFileChooserDialogClass;
  TGtkFileChooserDialogClass = record
    parent_class : TGtkDialogClass;
  end;

  PGtkFileChooserDialog = ^TGtkFileChooserDialog;
  TGtkFileChooserDialog = record
    parent_instance : TGtkDialog;
    priv : PGtkFileChooserDialogPrivate;
  end;
{$ENDIF} {types}

{$IFDEF read_interface_rest}

function GTK_TYPE_FILE_CHOOSER_DIALOG : GType;
function GTK_FILE_CHOOSER_DIALOG(obj : pointer) : PGtkFileChooserDialog;
function GTK_FILE_CHOOSER_DIALOG_CLASS(klass : pointer) : PGtkFileChooserDialogClass;
function GTK_IS_FILE_CHOOSER_DIALOG(obj : pointer) : gboolean;
function GTK_IS_FILE_CHOOSER_DIALOG_CLASS(klass : pointer) : gboolean;
function GTK_FILE_CHOOSER_DIALOG_GET_CLASS(obj : pointer) : PGtkFileChooserDialogClass;


function gtk_file_chooser_dialog_get_type:GType;cdecl;external gtklib name 'gtk_file_chooser_dialog_get_type';

function gtk_file_chooser_dialog_new(title:Pgchar; parent:PGtkWindow; action:TGtkFileChooserAction; first_button_text:Pgchar; args:array of const):PGtkWidget;cdecl;overload;external gtklib name 'gtk_file_chooser_dialog_new';
function gtk_file_chooser_dialog_new(title:Pgchar; parent:PGtkWindow; action:TGtkFileChooserAction; first_button_text:Pgchar):PGtkWidget;cdecl;overload;varargs;external gtklib name 'gtk_file_chooser_dialog_new';

function gtk_file_chooser_dialog_new_with_backend(title:Pgchar; parent:PGtkWindow; action:TGtkFileChooserAction; backend:Pgchar; first_button_text:Pgchar;
                                                  args:array of const):PGtkWidget;cdecl;overload;external gtklib name 'gtk_file_chooser_dialog_new_with_backend';
function gtk_file_chooser_dialog_new_with_backend(title:Pgchar; parent:PGtkWindow; action:TGtkFileChooserAction; backend:Pgchar; first_button_text:Pgchar):PGtkWidget;cdecl;overload;external gtklib name 'gtk_file_chooser_dialog_new_with_backend';

{$endif} {read_interface_rest}

{************************************************}

{$IFDEF read_implementation}

function GTK_TYPE_FILE_CHOOSER_DIALOG : GType;
begin
   GTK_TYPE_FILE_CHOOSER_DIALOG:=gtk_file_chooser_dialog_get_type;
end;

function GTK_FILE_CHOOSER_DIALOG(obj : pointer) : PGtkFileChooserDialog;
begin
   GTK_FILE_CHOOSER_DIALOG:=PGtkFileChooserDialog(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_CHOOSER_DIALOG));
end;

function GTK_FILE_CHOOSER_DIALOG_CLASS(klass : pointer) : PGtkFileChooserDialogClass;
begin
   GTK_FILE_CHOOSER_DIALOG_CLASS:=G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_FILE_CHOOSER_DIALOG);
end;

function GTK_IS_FILE_CHOOSER_DIALOG(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_DIALOG:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_CHOOSER_DIALOG);
end;

function GTK_IS_FILE_CHOOSER_DIALOG_CLASS(klass : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_DIALOG_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_FILE_CHOOSER_DIALOG);
end;

function GTK_FILE_CHOOSER_DIALOG_GET_CLASS(obj : pointer) : PGtkFileChooserDialogClass;
begin
   GTK_FILE_CHOOSER_DIALOG_GET_CLASS:=PGtkFileChooserDialogClass(G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_FILE_CHOOSER_DIALOG));
end;

{$ENDIF} {read_implementation}
