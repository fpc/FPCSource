
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type
    PGtkFileChooser = pointer;
    PPGtkFileChooser = ^PGtkFileChooser;

    PGtkFileChooserAction = ^TGtkFileChooserAction;
    TGtkFileChooserAction = (GTK_FILE_CHOOSER_ACTION_OPEN,
                             GTK_FILE_CHOOSER_ACTION_SAVE,
                             GTK_FILE_CHOOSER_ACTION_SELECT_FOLDER,
                             GTK_FILE_CHOOSER_ACTION_CREATE_FOLDER);

type

       PGtkFileChooserError = ^TGtkFileChooserError;
       TGtkFileChooserError = (GTK_FILE_CHOOSER_ERROR_NONEXISTENT,
                               GTK_FILE_CHOOSER_ERROR_BAD_FILENAME);

{$ENDIF} {types}

{$IFDEF read_interface_rest}

function GTK_TYPE_FILE_CHOOSER: GType;
function GTK_FILE_CHOOSER(obj : pointer) : PGtkFileChooser;
function GTK_IS_FILE_CHOOSER(obj : pointer) : gboolean;
function GTK_FILE_CHOOSER_ERROR : TGQuark;


function gtk_file_chooser_get_type:GType;cdecl;external gtklib name 'gtk_file_chooser_get_type';


function gtk_file_chooser_error_quark:TGQuark;cdecl;external gtklib name 'gtk_file_chooser_error_quark';

{ Configuration }

procedure gtk_file_chooser_set_action(chooser:PGtkFileChooser; action:TGtkFileChooserAction);cdecl;external gtklib name 'gtk_file_chooser_set_action';
function  gtk_file_chooser_get_action(chooser:PGtkFileChooser):TGtkFileChooserAction;cdecl;external gtklib name 'gtk_file_chooser_get_action';
procedure gtk_file_chooser_set_local_only(chooser:PGtkFileChooser; local_only:gboolean);cdecl;external gtklib name 'gtk_file_chooser_set_local_only';
function  gtk_file_chooser_get_local_only(chooser:PGtkFileChooser):gboolean;cdecl;external gtklib name 'gtk_file_chooser_get_local_only';
procedure gtk_file_chooser_set_select_multiple(chooser:PGtkFileChooser; select_multiple:gboolean);cdecl;external gtklib name 'gtk_file_chooser_set_select_multiple';
function  gtk_file_chooser_get_select_multiple(chooser:PGtkFileChooser):gboolean;cdecl;external gtklib name 'gtk_file_chooser_get_select_multiple';

{ Suggested name for the Save-type actions }

procedure gtk_file_chooser_set_current_name(chooser:PGtkFileChooser; name:Pgchar);cdecl;external gtklib name 'gtk_file_chooser_set_current_name';


    { Filename manipulation    }
function gtk_file_chooser_get_filename(chooser:PGtkFileChooser):Pgchar;cdecl;external gtklib name 'gtk_file_chooser_get_filename';
function gtk_file_chooser_set_filename(chooser:PGtkFileChooser; filename:Pchar):gboolean;cdecl;external gtklib name 'gtk_file_chooser_set_filename';

function gtk_file_chooser_select_filename(chooser:PGtkFileChooser; filename:Pchar):gboolean;cdecl;external gtklib name 'gtk_file_chooser_select_filename';
procedure gtk_file_chooser_unselect_filename(chooser:PGtkFileChooser; filename:Pchar);cdecl;external gtklib name 'gtk_file_chooser_unselect_filename';

procedure gtk_file_chooser_select_all(chooser:PGtkFileChooser);cdecl;external gtklib name 'gtk_file_chooser_select_all';
procedure gtk_file_chooser_unselect_all(chooser:PGtkFileChooser);cdecl;external gtklib name 'gtk_file_chooser_unselect_all';

function gtk_file_chooser_get_filenames(chooser:PGtkFileChooser):PGSList;cdecl;external gtklib name 'gtk_file_chooser_get_filenames';

function gtk_file_chooser_set_current_folder(chooser:PGtkFileChooser; filename:Pgchar):gboolean;cdecl;external gtklib name 'gtk_file_chooser_set_current_folder';
function gtk_file_chooser_get_current_folder(chooser:PGtkFileChooser):Pgchar;cdecl;external gtklib name 'gtk_file_chooser_get_current_folder';

{ URI manipulation }
function gtk_file_chooser_get_uri(chooser:PGtkFileChooser):Pgchar;cdecl;external gtklib name 'gtk_file_chooser_get_uri';
function gtk_file_chooser_set_uri(chooser:PGtkFileChooser; uri:Pchar):gboolean;cdecl;external gtklib name 'gtk_file_chooser_set_uri';

function gtk_file_chooser_select_uri(chooser:PGtkFileChooser; uri:Pchar):gboolean;cdecl;external gtklib name 'gtk_file_chooser_select_uri';
procedure gtk_file_chooser_unselect_uri(chooser:PGtkFileChooser; uri:Pchar);cdecl;external gtklib name 'gtk_file_chooser_unselect_uri';

function gtk_file_chooser_get_uris(chooser:PGtkFileChooser):PGSList;cdecl;external gtklib name 'gtk_file_chooser_get_uris';

function gtk_file_chooser_set_current_folder_uri(chooser:PGtkFileChooser; uri:Pgchar):gboolean;cdecl;external gtklib name 'gtk_file_chooser_set_current_folder_uri';
function gtk_file_chooser_get_current_folder_uri(chooser:PGtkFileChooser):Pgchar;cdecl;external gtklib name 'gtk_file_chooser_get_current_folder_uri';


{ Preview widget }

procedure gtk_file_chooser_set_preview_widget(chooser:PGtkFileChooser; preview_widget:PGtkWidget);cdecl;external gtklib name 'gtk_file_chooser_set_preview_widget';
function gtk_file_chooser_get_preview_widget(chooser:PGtkFileChooser):PGtkWidget;cdecl;external gtklib name 'gtk_file_chooser_get_preview_widget';

procedure gtk_file_chooser_set_preview_widget_active(chooser:PGtkFileChooser; active:gboolean);cdecl;external gtklib name 'gtk_file_chooser_set_preview_widget_active';
function gtk_file_chooser_get_preview_widget_active(chooser:PGtkFileChooser):gboolean;cdecl;external gtklib name 'gtk_file_chooser_get_preview_widget_active';

procedure gtk_file_chooser_set_use_preview_label(chooser:PGtkFileChooser; use_label:gboolean);cdecl;external gtklib name 'gtk_file_chooser_set_use_preview_label';
function gtk_file_chooser_get_use_preview_label(chooser:PGtkFileChooser):gboolean;cdecl;external gtklib name 'gtk_file_chooser_get_use_preview_label';

function gtk_file_chooser_get_preview_filename(chooser:PGtkFileChooser):Pchar;cdecl;external gtklib name 'gtk_file_chooser_get_preview_filename';
function gtk_file_chooser_get_preview_uri(chooser:PGtkFileChooser):Pchar;cdecl;external gtklib name 'gtk_file_chooser_get_preview_uri';

{ Extra widget }
procedure gtk_file_chooser_set_extra_widget(chooser:PGtkFileChooser; extra_widget:PGtkWidget);cdecl;external gtklib name 'gtk_file_chooser_set_extra_widget';
function gtk_file_chooser_get_extra_widget(chooser:PGtkFileChooser):PGtkWidget;cdecl;external gtklib name 'gtk_file_chooser_get_extra_widget';

{ List of user selectable filters }
procedure gtk_file_chooser_add_filter(chooser:PGtkFileChooser; filter:PGtkFileFilter);cdecl;external gtklib name 'gtk_file_chooser_add_filter';
procedure gtk_file_chooser_remove_filter(chooser:PGtkFileChooser; filter:PGtkFileFilter);cdecl;external gtklib name 'gtk_file_chooser_remove_filter';
function gtk_file_chooser_list_filters(chooser:PGtkFileChooser):PGSList;cdecl;external gtklib name 'gtk_file_chooser_list_filters';

{ Current filter }
procedure gtk_file_chooser_set_filter(chooser:PGtkFileChooser; filter:PGtkFileFilter);cdecl;external gtklib name 'gtk_file_chooser_set_filter';
function gtk_file_chooser_get_filter(chooser:PGtkFileChooser):PGtkFileFilter;cdecl;external gtklib name 'gtk_file_chooser_get_filter';

{ Per-application shortcut folders  }
function gtk_file_chooser_add_shortcut_folder(chooser:PGtkFileChooser; folder:Pchar; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_chooser_add_shortcut_folder';
function gtk_file_chooser_remove_shortcut_folder(chooser:PGtkFileChooser; folder:Pchar; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_chooser_remove_shortcut_folder';
function gtk_file_chooser_list_shortcut_folders(chooser:PGtkFileChooser):PGSList;cdecl;external gtklib name 'gtk_file_chooser_list_shortcut_folders';

function gtk_file_chooser_add_shortcut_folder_uri(chooser:PGtkFileChooser; uri:Pchar; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_chooser_add_shortcut_folder_uri';
function gtk_file_chooser_remove_shortcut_folder_uri(chooser:PGtkFileChooser; uri:Pchar; error:PPGError):gboolean;cdecl;external gtklib name 'gtk_file_chooser_remove_shortcut_folder_uri';
function gtk_file_chooser_list_shortcut_folder_uris(chooser:PGtkFileChooser):PGSList;cdecl;external gtklib name 'gtk_file_chooser_list_shortcut_folder_uris';

{$ifdef HasGTK2_6}
procedure gtk_file_chooser_set_show_hidden(chooser:PGtkFileChooser; show_hidden:gboolean);cdecl;external gtklib;
function gtk_file_chooser_get_show_hidden(chooser:PGtkFileChooser):gboolean;cdecl;external gtklib;
{$endif}

{$ifdef HasGTK2_8}
function gtk_file_chooser_get_do_overwrite_confirmation(chooser:PGtkFileChooser): gboolean;  cdecl; external gtklib name 'gtk_file_chooser_get_do_overwrite_confirmation';
procedure gtk_file_chooser_set_do_overwrite_confirmation(chooser:PGtkFileChooser; do_overwrite_confirmation:gboolean);  cdecl; external gtklib name 'gtk_file_chooser_set_do_overwrite_confirmation';
{$endif}

{$endif} {read_interface_rest}

{************************************************}

{$IFDEF read_implementation}

function GTK_TYPE_FILE_CHOOSER: GType;
begin
   GTK_TYPE_FILE_CHOOSER:=gtk_file_chooser_get_type;
end;

function GTK_FILE_CHOOSER(obj : pointer) : PGtkFileChooser;
begin
   GTK_FILE_CHOOSER:=G_TYPE_CHECK_INSTANCE_CAST(obj, GTK_TYPE_FILE_CHOOSER);
end;

function GTK_IS_FILE_CHOOSER(obj : pointer) : gboolean;
begin
   GTK_IS_FILE_CHOOSER:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_FILE_CHOOSER);
end;

function GTK_FILE_CHOOSER_ERROR : TGQuark;
begin
   GTK_FILE_CHOOSER_ERROR:=gtk_file_chooser_error_quark;
end;

{$ENDIF}
