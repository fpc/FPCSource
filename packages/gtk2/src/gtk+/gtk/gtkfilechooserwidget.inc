
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type
   PGtkFileChooserWidgetPrivate = pointer;
   PPGtkFileChooserWidgetPrivate = ^PGtkFileChooserWidgetPrivate;

   PGtkFileChooserWidgetClass = ^TGtkFileChooserWidgetClass;
   TGtkFileChooserWidgetClass = record
        parent_class : TGtkVBoxClass;
     end;

   PGtkFileChooserWidget = ^TGtkFileChooserWidget;
   TGtkFileChooserWidget = record
        parent_instance : TGtkVBox;
        priv : PGtkFileChooserWidgetPrivate;
     end;

{$ENDIF} {types }

{$IFDEF read_interface_rest}

function GTK_TYPE_FILE_CHOOSER_WIDGET : GType;
function GTK_FILE_CHOOSER_WIDGET(obj : pointer) : PGtkFileChooserWidget;
function GTK_FILE_CHOOSER_WIDGET_CLASS(klass : pointer) : PGtkFileChooserWidgetClass;
function GTK_IS_FILE_CHOOSER_WIDGET(obj : pointer) : gboolean;
function GTK_IS_FILE_CHOOSER_WIDGET_CLASS(klass :pointer) : gboolean;
function GTK_FILE_CHOOSER_WIDGET_GET_CLASS(obj : pointer) : PGtkFileChooserWidgetClass;

function gtk_file_chooser_widget_get_type:GType;cdecl;external gtklib name 'gtk_file_chooser_widget_get_type';
function gtk_file_chooser_widget_new(action:TGtkFileChooserAction):PGtkWidget;cdecl;external gtklib name 'gtk_file_chooser_widget_new';
function gtk_file_chooser_widget_new_with_backend(action:TGtkFileChooserAction; backend:Pgchar):PGtkWidget;cdecl;external gtklib name 'gtk_file_chooser_widget_new_with_backend';

{$endif}

{$IFDEF read_implementation}

function GTK_TYPE_FILE_CHOOSER_WIDGET : GType;
begin
   GTK_TYPE_FILE_CHOOSER_WIDGET:=gtk_file_chooser_widget_get_type;
end;

function GTK_FILE_CHOOSER_WIDGET(obj : pointer) : PGtkFileChooserWidget;
begin
   GTK_FILE_CHOOSER_WIDGET:=PGtkFileChooserWidget(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_CHOOSER_WIDGET));
end;

function GTK_FILE_CHOOSER_WIDGET_CLASS(klass : pointer) : PGtkFileChooserWidgetClass;
begin
   GTK_FILE_CHOOSER_WIDGET_CLASS:=PGtkFileChooserWidgetClass(G_TYPE_CHECK_CLASS_CAST(klass,GTK_TYPE_FILE_CHOOSER_WIDGET));
end;

function GTK_IS_FILE_CHOOSER_WIDGET(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_WIDGET:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_CHOOSER_WIDGET);
end;

function GTK_IS_FILE_CHOOSER_WIDGET_CLASS(klass :pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_WIDGET_CLASS:=G_TYPE_CHECK_CLASS_TYPE(klass,GTK_TYPE_FILE_CHOOSER_WIDGET);
end;

function GTK_FILE_CHOOSER_WIDGET_GET_CLASS(obj : pointer) : PGtkFileChooserWidgetClass;
begin
   GTK_FILE_CHOOSER_WIDGET_GET_CLASS:=PGtkFileChooserWidgetClass (G_TYPE_INSTANCE_GET_CLASS(obj,GTK_TYPE_FILE_CHOOSER_WIDGET));
end;

{$ENDIF}
