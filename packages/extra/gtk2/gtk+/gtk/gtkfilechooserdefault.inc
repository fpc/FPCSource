{%MainUnit gtkincludes.inc}

{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type
   PGtkFileChooserDefault = pointer;
   PPGtkFileChooserDefault = ^PGtkFileChooserDefault;

{$ENDIF}

{$IFDEF read_interface_rest}
function GTK_TYPE_FILE_CHOOSER_DEFAULT : GType;

function GTK_FILE_CHOOSER_DEFAULT(obj : pointer) : PGtkFileChooserDefault;

function GTK_IS_FILE_CHOOSER_DEFAULT(obj : pointer) : gboolean;


function _gtk_file_chooser_default_get_type:GType;cdecl;external gtklib name '_gtk_file_chooser_default_get_type';

function _gtk_file_chooser_default_new(file_system:Pchar):PGtkWidget;cdecl;external gtklib name '_gtk_file_chooser_default_new';

{$ENDIF}



{************************************************}

{$IFDEF read_implementation}
function GTK_TYPE_FILE_CHOOSER_DEFAULT : GType;
begin
   GTK_TYPE_FILE_CHOOSER_DEFAULT:=_gtk_file_chooser_default_get_type;
end;



function GTK_FILE_CHOOSER_DEFAULT(obj : pointer) : PGtkFileChooserDefault;
begin
   GTK_FILE_CHOOSER_DEFAULT:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_CHOOSER_DEFAULT);
end;


function GTK_IS_FILE_CHOOSER_DEFAULT(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_DEFAULT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_CHOOSER_DEFAULT);
end;


{$ENDIF} {read_implementation}
