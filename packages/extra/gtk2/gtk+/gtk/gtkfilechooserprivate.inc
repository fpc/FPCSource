
{$IFDEF read_forward_definitions}
{$ENDIF}

{$IFDEF read_interface_types}
type
   PGtkFileChooserDialogPrivate = pointer;
   PPGtkFileChooserDialogPrivate = ^PGtkFileChooserDialogPrivate;

   PGtkFileChooserIface = ^TGtkFileChooserIface;
   TGtkFileChooserIface = record
        base_iface : TGTypeInterface;
        { Methods }
        set_current_folder : function (chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;cdecl;
        get_current_folder : function (chooser:PGtkFileChooser):PGtkFilePath;
        set_current_name : procedure (chooser:PGtkFileChooser; name:Pgchar);
        select_path : function (chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;
        unselect_path : procedure (chooser:PGtkFileChooser; path:PGtkFilePath);
        select_all : procedure (chooser:PGtkFileChooser);
        unselect_all : procedure (chooser:PGtkFileChooser);
        get_paths : function (chooser:PGtkFileChooser):PGSList;
        get_preview_path : function (chooser:PGtkFileChooser):PGtkFilePath;
        get_file_system : function (chooser:PGtkFileChooser):PGtkFileSystem;
        add_filter : procedure (chooser:PGtkFileChooser; filter:PGtkFileFilter);
        remove_filter : procedure (chooser:PGtkFileChooser; filter:PGtkFileFilter);
        list_filters : function (chooser:PGtkFileChooser):PGSList;
        add_shortcut_folder : function (chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;
        remove_shortcut_folder : function (chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;
        list_shortcut_folders : function (chooser:PGtkFileChooser):PGSList;

        { Signals }
        current_folder_changed : procedure (chooser:PGtkFileChooser);
        selection_changed : procedure (chooser:PGtkFileChooser);
        update_preview : procedure (chooser:PGtkFileChooser);
        file_activated : procedure (chooser:PGtkFileChooser);
     end;

{$ENDIF} {types}

{$IFDEF read_interface_rest}


(* Geraten: *)
function GTK_FILE_CHOOSER_GET_IFACE(inst : PGTypeInstance) :    PGtkFileChooserIface;



function _gtk_file_chooser_get_file_system(chooser:PGtkFileChooser):PGtkFileSystem;cdecl;external gtklib name '_gtk_file_chooser_get_file_system';

function _gtk_file_chooser_set_current_folder_path(chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name '_gtk_file_chooser_set_current_folder_path';
function _gtk_file_chooser_get_current_folder_path(chooser:PGtkFileChooser):PGtkFilePath;cdecl;external gtklib name '_gtk_file_chooser_get_current_folder_path';

function _gtk_file_chooser_select_path(chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name '_gtk_file_chooser_select_path';
procedure _gtk_file_chooser_unselect_path(chooser:PGtkFileChooser; path:PGtkFilePath);cdecl;external gtklib name '_gtk_file_chooser_unselect_path';

function _gtk_file_chooser_get_paths(chooser:PGtkFileChooser):PGSList;cdecl;external gtklib name '_gtk_file_chooser_get_paths';
function _gtk_file_chooser_get_preview_path(chooser:PGtkFileChooser):PGtkFilePath;cdecl;external gtklib name '_gtk_file_chooser_get_preview_path';

function _gtk_file_chooser_add_shortcut_folder(chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name '_gtk_file_chooser_add_shortcut_folder';
function _gtk_file_chooser_remove_shortcut_folder(chooser:PGtkFileChooser; path:PGtkFilePath; error:PPGError):gboolean;cdecl;external gtklib name '_gtk_file_chooser_remove_shortcut_folder';

{$endif} {interface_rest}

{************************************************}

{$IFDEF read_implementation}


function GTK_FILE_CHOOSER_GET_IFACE(inst : PGTypeInstance) :    PGtkFileChooserIface;

begin
   GTK_FILE_CHOOSER_GET_IFACE:=G_TYPE_INSTANCE_GET_INTERFACE(inst,GTK_TYPE_FILE_CHOOSER);
end;

{$ENDIF} {read_implementation}
