{
   $Id$
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


{
  $Log$
  Revision 1.2  2000-05-26 16:23:30  peter
    * update for new win32 dll's

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.10  1999/10/21 08:42:01  florian
    * some changes to get it work with gtk 1.3 under Windows 98:
      - removed some trailing space after the import name
      - In gtkbindings.h is
        #define  gtk_binding_entry_add          gtk_binding_entry_clear
        so in the pascal headers the import name of gtk_bindings_entry_add should be
        gtk_binding_entry_clear!
      - removed the declaration of
        gtk_drag_source_unset in gtkdnd.pp it isn't in gtk-1.3.dll!
      - in gdk.pp glibdll must be set to gdk-1.3:
        const
           gdkdll='gdk-1.3';
           glibdll='gdk-1.3';
        else the whole gdk_* calls are imported from glib-1.3.dll which is wrong!

  Revision 1.9  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:18  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:32  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:22  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:12  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:09:51  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:28  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

