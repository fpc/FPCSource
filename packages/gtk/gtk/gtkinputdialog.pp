{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkInputDialog = ^TGtkInputDialog;
     TGtkInputDialog = record
          dialog : TGtkDialog;
          axis_list : PGtkWidget;
          axis_listbox : PGtkWidget;
          mode_optionmenu : PGtkWidget;
          close_button : PGtkWidget;
          save_button : PGtkWidget;
          axis_items : array[0..ord(GDK_AXIS_LAST)-1] of PGtkWidget;
          current_device : guint32;
          keys_list : PGtkWidget;
          keys_listbox : PGtkWidget;
       end;

     PGtkInputDialogClass = ^TGtkInputDialogClass;
     TGtkInputDialogClass = record
          parent_class : TGtkWindowClass;
          enable_device : procedure (inputd:PGtkInputDialog; devid:guint32);cdecl;
          disable_device : procedure (inputd:PGtkInputDialog; devid:guint32);cdecl;
       end;

Type
  GTK_INPUT_DIALOG=PGtkInputDialog;
  GTK_INPUT_DIALOG_CLASS=PGtkInputDialogClass;

function  GTK_INPUT_DIALOG_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_input_dialog_get_type';
function  GTK_IS_INPUT_DIALOG(obj:pointer):boolean;
function  GTK_IS_INPUT_DIALOG_CLASS(klass:pointer):boolean;

function  gtk_input_dialog_get_type:TGtkType;cdecl;external gtkdll name 'gtk_input_dialog_get_type';
function  gtk_input_dialog_new:PGtkWidget;cdecl;external gtkdll name 'gtk_input_dialog_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_INPUT_DIALOG(obj:pointer):boolean;
begin
  GTK_IS_INPUT_DIALOG:=(obj<>nil) and GTK_IS_INPUT_DIALOG_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_INPUT_DIALOG_CLASS(klass:pointer):boolean;
begin
  GTK_IS_INPUT_DIALOG_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_INPUT_DIALOG_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:04  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:49  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:32  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:38:47  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:19:38  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:17  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:04  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:22:42  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

