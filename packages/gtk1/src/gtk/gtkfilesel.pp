{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkFileSelection = ^TGtkFileSelection;
     TGtkFileSelection = record
          window : TGtkWindow;
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
          fileop_file : ^gchar;
          cmpl_state : gpointer;
          fileop_c_dir : PGtkWidget;
          fileop_del_file : PGtkWidget;
          fileop_ren_file : PGtkWidget;
          button_area : PGtkWidget;
          action_area : PGtkWidget;
       end;

     PGtkFileSelectionClass = ^TGtkFileSelectionClass;
     TGtkFileSelectionClass = record
          parent_class : TGtkWindowClass;
       end;

Type
  GTK_FILE_SELECTION=PGtkFileSelection;
  GTK_FILE_SELECTION_CLASS=PGtkFileSelectionClass;

function  GTK_FILE_SELECTION_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_file_selection_get_type';
function  GTK_IS_FILE_SELECTION(obj:pointer):boolean;
function  GTK_IS_FILE_SELECTION_CLASS(klass:pointer):boolean;

function  gtk_file_selection_get_type:TGtkType;cdecl;external gtkdll name 'gtk_file_selection_get_type';
function  gtk_file_selection_new (title:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_file_selection_new';
procedure gtk_file_selection_set_filename(filesel:PGtkFileSelection; filename:Pgchar);cdecl;external gtkdll name 'gtk_file_selection_set_filename';
function  gtk_file_selection_get_filename (filesel:PGtkFileSelection):Pgchar;cdecl;external gtkdll name 'gtk_file_selection_get_filename';
procedure gtk_file_selection_complete(filesel:PGtkFileSelection; pattern:Pgchar);cdecl;external gtkdll name 'gtk_file_selection_complete';
procedure gtk_file_selection_show_fileop_buttons(filesel:PGtkFileSelection);cdecl;external gtkdll name 'gtk_file_selection_show_fileop_buttons';
procedure gtk_file_selection_hide_fileop_buttons(filesel:PGtkFileSelection);cdecl;external gtkdll name 'gtk_file_selection_hide_fileop_buttons';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_FILE_SELECTION(obj:pointer):boolean;
begin
  GTK_IS_FILE_SELECTION:=(obj<>nil) and GTK_IS_FILE_SELECTION_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_FILE_SELECTION_CLASS(klass:pointer):boolean;
begin
  GTK_IS_FILE_SELECTION_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_FILE_SELECTION_TYPE);
end;

{$endif read_implementation}


