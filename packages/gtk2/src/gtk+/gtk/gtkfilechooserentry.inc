
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type
   PGtkFileChooserEntry  = pointer;
   PPGtkFileChooserEntry = ^PGtkFileChooserEntry;
{$ENDIF}

{$IFDEF read_interface_rest}

function GTK_TYPE_FILE_CHOOSER_ENTRY : GType;

function GTK_FILE_CHOOSER_ENTRY(obj : pointer) : PGtkFileChooserEntry;

function GTK_IS_FILE_CHOOSER_ENTRY(obj : pointer) : gboolean;

function _gtk_file_chooser_entry_get_type:GType;cdecl;external gtklib name '_gtk_file_chooser_entry_get_type';
function _gtk_file_chooser_entry_new:PGtkWidget;cdecl;external gtklib name '_gtk_file_chooser_entry_new';

procedure _gtk_file_chooser_entry_set_file_system(chooser_entry:PGtkFileChooserEntry; file_system:PGtkFileSystem);cdecl;external gtklib name '_gtk_file_chooser_entry_set_file_system';

procedure _gtk_file_chooser_entry_set_base_folder(chooser_entry:PGtkFileChooserEntry; path:PGtkFilePath);cdecl;external gtklib name '_gtk_file_chooser_entry_set_base_folder';

procedure _gtk_file_chooser_entry_set_file_part(chooser_entry:PGtkFileChooserEntry; file_part:Pgchar);cdecl;external gtklib name '_gtk_file_chooser_entry_set_file_part';

function _gtk_file_chooser_entry_get_current_folder(chooser_entry:PGtkFileChooserEntry):PGtkFilePath;cdecl;external gtklib name '_gtk_file_chooser_entry_get_current_folder';

function _gtk_file_chooser_entry_get_file_part(chooser_entry:PGtkFileChooserEntry):Pgchar;cdecl;external gtklib name '_gtk_file_chooser_entry_get_file_part';

{$endif} {read_interface_rest}

{************************************************}

{$IFDEF read_implementation}

function GTK_TYPE_FILE_CHOOSER_ENTRY : GType;
  begin
     GTK_TYPE_FILE_CHOOSER_ENTRY:=_gtk_file_chooser_entry_get_type;
  end;

{return type}
function GTK_FILE_CHOOSER_ENTRY(obj : pointer) : PGtkFileChooserEntry;
begin
   GTK_FILE_CHOOSER_ENTRY:=G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_FILE_CHOOSER_ENTRY);
end;

function GTK_IS_FILE_CHOOSER_ENTRY(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER_ENTRY:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_CHOOSER_ENTRY);
end;

{$ENDIF} {read_implementation}
