// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkFileSelection = ^TGtkFileSelection;
   TGtkFileSelection = record
        parent_instance : TGtkDialog;
        dir_list : PGtkWidget;
        file_list : PGtkWidget;
        selection_entry : PGtkWidget;
        selection_text : PGtkWidget;
        main_vbox : PGtkWidget;
        ok_button : PGtkWidget;
        cancel_button : PGtkWidget;
        help_button : PGtkWidget;
        history_pulldown : PGtkWidget;
        history_menu : PGtkWidget;
        history_list : PGList;
        fileop_dialog : PGtkWidget;
        fileop_entry : PGtkWidget;
        fileop_file : Pgchar;
        cmpl_state : gpointer;
        fileop_c_dir : PGtkWidget;
        fileop_del_file : PGtkWidget;
        fileop_ren_file : PGtkWidget;
        button_area : PGtkWidget;
        action_area : PGtkWidget;
        selected_names : PGPtrArray;
        last_selected : Pgchar;
     end;

{ Padding for future expansion  }
   PGtkFileSelectionClass = ^TGtkFileSelectionClass;
   TGtkFileSelectionClass = record
        parent_class : TGtkDialogClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_FILE_SELECTION : GType;
function GTK_FILE_SELECTION(obj: pointer) : PGtkFileSelection;
function GTK_FILE_SELECTION_CLASS(klass: pointer) : PGtkFileSelectionClass;
function GTK_IS_FILE_SELECTION(obj: pointer) : boolean;
function GTK_IS_FILE_SELECTION_CLASS(klass: pointer) : boolean;
function GTK_FILE_SELECTION_GET_CLASS(obj: pointer) : PGtkFileSelectionClass;


function gtk_file_selection_get_type:TGtkType; cdecl; external gtklib;
function gtk_file_selection_new(title:Pgchar):PGtkWidget; cdecl; external gtklib;
procedure gtk_file_selection_set_filename(filesel:PGtkFileSelection; filename:Pgchar); cdecl; external gtklib;
{ This function returns the selected filename in the C runtime's
   multibyte string encoding, which may or may not be the same as that
   used by GDK (UTF-8). To convert to UTF-8, call g_filename_to_utf8().
   The returned string points to a statically allocated buffer and
   should be copied away.
  }
function gtk_file_selection_get_filename(filesel:PGtkFileSelection):Pgchar; cdecl; external gtklib;
procedure gtk_file_selection_complete(filesel:PGtkFileSelection; pattern:Pgchar); cdecl; external gtklib;
procedure gtk_file_selection_show_fileop_buttons(filesel:PGtkFileSelection); cdecl; external gtklib;
procedure gtk_file_selection_hide_fileop_buttons(filesel:PGtkFileSelection); cdecl; external gtklib;
function gtk_file_selection_get_selections(filesel:PGtkFileSelection):PPgchar; cdecl; external gtklib;
procedure gtk_file_selection_set_select_multiple(filesel:PGtkFileSelection; select_multiple:gboolean); cdecl; external gtklib;
function gtk_file_selection_get_select_multiple(filesel:PGtkFileSelection):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_FILE_SELECTION : GType;
begin
   GTK_TYPE_FILE_SELECTION:=gtk_file_selection_get_type;
end;

function GTK_FILE_SELECTION(obj: pointer) : PGtkFileSelection;
begin
   GTK_FILE_SELECTION:=PGtkFileSelection(GTK_CHECK_CAST(obj,GTK_TYPE_FILE_SELECTION));
end;

function GTK_FILE_SELECTION_CLASS(klass: pointer) : PGtkFileSelectionClass;
begin
   GTK_FILE_SELECTION_CLASS:=PGtkFileSelectionClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_FILE_SELECTION));
end;

function GTK_IS_FILE_SELECTION(obj: pointer) : boolean;
begin
   GTK_IS_FILE_SELECTION:=GTK_CHECK_TYPE(obj,GTK_TYPE_FILE_SELECTION);
end;

function GTK_IS_FILE_SELECTION_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_FILE_SELECTION_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_FILE_SELECTION);
end;

function GTK_FILE_SELECTION_GET_CLASS(obj: pointer) : PGtkFileSelectionClass;
begin
   GTK_FILE_SELECTION_GET_CLASS:=PGtkFileSelectionClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_FILE_SELECTION));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

