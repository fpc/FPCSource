{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkCheckButton = ^TGtkCheckButton;
       TGtkCheckButton = record
            toggle_button : TGtkToggleButton;
         end;

       PGtkCheckButtonClass = ^TGtkCheckButtonClass;
       TGtkCheckButtonClass = record
            parent_class : TGtkToggleButtonClass;
            indicator_size : guint16;
            indicator_spacing : guint16;
            draw_indicator : procedure (check_button:PGtkCheckButton; area:PGdkRectangle); cdecl;
         end;

Type
  GTK_CHECK_BUTTON=PGtkCheckButton;
  GTK_CHECK_BUTTON_CLASS=PGtkCheckButtonClass;

function  GTK_CHECK_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_check_button_get_type';
function  GTK_IS_CHECK_BUTTON(obj:pointer):boolean;
function  GTK_IS_CHECK_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_check_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_check_button_get_type';
function  gtk_check_button_new : PGtkWidget;cdecl;external gtkdll name 'gtk_check_button_new';
function  gtk_check_button_new_with_label (thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_check_button_new_with_label';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_CHECK_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_CHECK_BUTTON:=(obj<>nil) and GTK_IS_CHECK_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_CHECK_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_CHECK_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_CHECK_BUTTON_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.11  1999/10/21 08:42:01  florian
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

  Revision 1.10  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.9  1999/07/23 16:12:03  peter
    * use packrecords C

  Revision 1.8  1999/05/11 00:38:15  peter
    * win32 fixes

  Revision 1.7  1999/05/10 15:19:02  peter
    * cdecl fixes

  Revision 1.6  1999/05/07 15:09:55  peter
    * more fixes

  Revision 1.5  1999/05/07 10:40:32  peter
    * first things for 1.2

  Revision 1.4  1998/11/09 10:09:38  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:13  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

