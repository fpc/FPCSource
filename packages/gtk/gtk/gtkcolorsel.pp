{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkColorSelection = ^TGtkColorSelection;
       TGtkColorSelection = record
            vbox : TGtkVBox;
            wheel_area : PGtkWidget;
            value_area : PGtkWidget;
            sample_area : PGtkWidget;
            sample_area_eb : PGtkWidget;
            scales : array[0..7] of PGtkWidget;
            entries : array[0..7] of PGtkWidget;
            opacity_label : PGtkWidget;
            wheel_gc : PGdkGC;
            value_gc : PGdkGC;
            sample_gc : PGdkGC;
            policy : TGtkUpdateType;
            use_opacity : gint;
            timer_active : gint;
            timer_tag : gint;
            values : array[0..7] of gdouble;
            old_values : array[0..7] of gdouble;
            wheel_buf : Pguchar;
            value_buf : Pguchar;
            sample_buf : Pguchar;
         end;

       PGtkColorSelectionClass = ^TGtkColorSelectionClass;
       TGtkColorSelectionClass = record
            parent_class : TGtkVBoxClass;
            color_changed : procedure (colorsel:PGtkColorSelection); cdecl;
         end;

       PGtkColorSelectionDialog = ^TGtkColorSelectionDialog;
       TGtkColorSelectionDialog = record
            window : TGtkWindow;
            colorsel : PGtkWidget;
            main_vbox : PGtkWidget;
            ok_button : PGtkWidget;
            reset_button : PGtkWidget;
            cancel_button : PGtkWidget;
            help_button : PGtkWidget;
         end;

       PGtkColorSelectionDialogClass = ^TGtkColorSelectionDialogClass;
       TGtkColorSelectionDialogClass = record
            parent_class : TGtkWindowClass;
         end;

Type
  GTK_COLOR_SELECTION=PGtkColorSelection;
  GTK_COLOR_SELECTION_CLASS=PGtkColorSelectionClass;

  GTK_COLOR_SELECTION_DIALOG=PGtkColorSelectionDialog;
  GTK_COLOR_SELECTION_DIALOG_CLASS=PGtkColorSelectionDialogClass;

{ ColorSelection }
function  GTK_COLOR_SELECTION_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_color_selection_get_type';
function  GTK_IS_COLOR_SELECTION(obj:pointer):boolean;
function  GTK_IS_COLOR_SELECTION_CLASS(klass:pointer):boolean;

function  gtk_color_selection_get_type:TGtkType;cdecl;external gtkdll name 'gtk_color_selection_get_type';
function  gtk_color_selection_new:PGtkWidget;cdecl;external gtkdll name 'gtk_color_selection_new';
procedure gtk_color_selection_set_update_policy(colorsel:PGtkColorSelection; policy:TGtkUpdateType);cdecl;external gtkdll name 'gtk_color_selection_set_update_policy';
procedure gtk_color_selection_set_opacity(colorsel:PGtkColorSelection; use_opacity:gint);cdecl;external gtkdll name 'gtk_color_selection_set_opacity';
procedure gtk_color_selection_set_color(colorsel:PGtkColorSelection; color:Pgdouble);cdecl;external gtkdll name 'gtk_color_selection_set_color';
procedure gtk_color_selection_get_color(colorsel:PGtkColorSelection; color:Pgdouble);cdecl;external gtkdll name 'gtk_color_selection_get_color';

{ ColorSelectionDialog }
function  gtk_color_selection_dialog_get_type:guint;cdecl;external gtkdll name 'gtk_color_selection_dialog_get_type';
function  gtk_color_selection_dialog_new (title:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_color_selection_dialog_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_COLOR_SELECTION(obj:pointer):boolean;
begin
  GTK_IS_COLOR_SELECTION:=(obj<>nil) and GTK_IS_COLOR_SELECTION_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_COLOR_SELECTION_CLASS(klass:pointer):boolean;
begin
  GTK_IS_COLOR_SELECTION_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_COLOR_SELECTION_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

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

  Revision 1.8  1999/07/23 16:12:07  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:18  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:06  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 17:40:16  peter
    * more updates

  Revision 1.4  1998/11/09 10:09:41  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:16  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

