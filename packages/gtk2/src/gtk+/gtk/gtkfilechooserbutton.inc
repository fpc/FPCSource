{%MainUnit gtkincludes.inc}

{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
  PGtkFileChooserButtonPrivate = pointer;
  PGtkFileChooserButton = ^TGtkFileChooserButton;
  TGtkFileChooserButton = record
       parent : TGtkHBox;
       priv : PGtkFileChooserButtonPrivate;
    end;


  PGtkFileChooserButtonClass = ^TGtkFileChooserButtonClass;
  TGtkFileChooserButtonClass = record
       parent_class : TGtkHBoxClass;
       file_set : procedure (fc:PGtkFileChooserButton);cdecl;
       __gtk_reserved1 : procedure; cdecl;
       __gtk_reserved2 : procedure; cdecl;
       __gtk_reserved3 : procedure; cdecl;
       __gtk_reserved4 : procedure; cdecl;
       __gtk_reserved5 : procedure; cdecl;
       __gtk_reserved6 : procedure; cdecl;
       __gtk_reserved7 : procedure; cdecl;
    end;
{$ENDIF}

{$IFDEF read_interface_rest}
function GTK_TYPE_FILE_CHOOSER_BUTTON : GType;
function GTK_FILE_CHOOSER_BUTTON(anObject : pointer) : PGtkFileChooserButton;
function GTK_FILE_CHOOSER_BUTTON_CLASS(klass : pointer) : PGtkFileChooserButtonClass;
function GTK_IS_FILE_CHOOSER_BUTTON(anObject : pointer) : boolean;
function GTK_IS_FILE_CHOOSER_BUTTON_CLASS(klass : pointer) : boolean;
function GTK_FILE_CHOOSER_BUTTON_GET_CLASS(anObject : pointer) : PGtkFileChooserButtonClass;


function gtk_file_chooser_button_get_type:GType;cdecl;external gtklib;
function gtk_file_chooser_button_new(title:Pgchar; action:TGtkFileChooserAction):PGtkWidget;cdecl;external gtklib;
function gtk_file_chooser_button_new_with_backend(title:Pgchar; action:TGtkFileChooserAction; backend:Pgchar):PGtkWidget;cdecl;external gtklib;
function gtk_file_chooser_button_new_with_dialog(dialog:PGtkWidget):PGtkWidget;cdecl;external gtklib;
function gtk_file_chooser_button_get_title(button:PGtkFileChooserButton):Pgchar;cdecl;external gtklib;
procedure gtk_file_chooser_button_set_title(button:PGtkFileChooserButton; title:Pgchar);cdecl;external gtklib;
function gtk_file_chooser_button_get_width_chars(button:PGtkFileChooserButton):gint;cdecl;external gtklib;
procedure gtk_file_chooser_button_set_width_chars(button:PGtkFileChooserButton; n_chars:gint);cdecl;external gtklib;
function gtk_file_chooser_button_get_focus_on_click(button:PGtkFileChooserButton):gboolean;cdecl;external gtklib;
procedure gtk_file_chooser_button_set_focus_on_click(button:PGtkFileChooserButton; focus_on_click:gboolean);cdecl;external gtklib;
{$ENDIF}

{************************************************}

{$IFDEF read_implementation}
function GTK_TYPE_FILE_CHOOSER_BUTTON : GType;
begin
  GTK_TYPE_FILE_CHOOSER_BUTTON:=gtk_file_chooser_button_get_type;
end;

function GTK_FILE_CHOOSER_BUTTON(anObject : pointer) : PGtkFileChooserButton;
begin
  GTK_FILE_CHOOSER_BUTTON:=PGtkFileChooserButton(G_TYPE_CHECK_INSTANCE_CAST(anObject,GTK_TYPE_FILE_CHOOSER_BUTTON));
end;

function GTK_FILE_CHOOSER_BUTTON_CLASS(klass : pointer) : PGtkFileChooserButtonClass;
begin
  GTK_FILE_CHOOSER_BUTTON_CLASS:=PGtkFileChooserButtonClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_FILE_CHOOSER_BUTTON));
end;

function GTK_IS_FILE_CHOOSER_BUTTON(anObject : pointer) : boolean;
begin
  GTK_IS_FILE_CHOOSER_BUTTON:=G_TYPE_CHECK_INSTANCE_TYPE(anObject,GTK_TYPE_FILE_CHOOSER_BUTTON);
end;

function GTK_IS_FILE_CHOOSER_BUTTON_CLASS(klass : pointer) : boolean;
begin
  GTK_IS_FILE_CHOOSER_BUTTON_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_FILE_CHOOSER_BUTTON);
end;

function GTK_FILE_CHOOSER_BUTTON_GET_CLASS(anObject : pointer) : PGtkFileChooserButtonClass;
begin
  GTK_FILE_CHOOSER_BUTTON_GET_CLASS:=PGtkFileChooserButtonClass(G_TYPE_INSTANCE_GET_CLASS(anObject,GTK_TYPE_FILE_CHOOSER_BUTTON));
end;

{$ENDIF} {read_implementation}
