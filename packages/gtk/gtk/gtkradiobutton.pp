{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkRadioButton = ^TGtkRadioButton;
     TGtkRadioButton = record
          check_button : TGtkCheckButton;
          group : PGSList;
       end;

     PGtkRadioButtonClass = ^TGtkRadioButtonClass;
     TGtkRadioButtonClass = record
          parent_class : TGtkCheckButtonClass;
       end;

Type
  GTK_RADIO_BUTTON=PGtkRadioButton;
  GTK_RADIO_BUTTON_CLASS=PGtkRadioButtonClass;

function  GTK_RADIO_BUTTON_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_radio_button_get_type';
function  GTK_IS_RADIO_BUTTON(obj:pointer):boolean;
function  GTK_IS_RADIO_BUTTON_CLASS(klass:pointer):boolean;

function  gtk_radio_button_get_type:TGtkType;cdecl;external gtkdll name 'gtk_radio_button_get_type';
function  gtk_radio_button_new(group:PGSList):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new';
function  gtk_radio_button_new_from_widget(group:PGtkRadioButton):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new_from_widget';
function  gtk_radio_button_new_with_label(group:PGSList; thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new_with_label';
function  gtk_radio_button_new_with_label_from_widget(group:PGtkRadioButton; thelabel:Pgchar):PGtkWidget;cdecl;external gtkdll name 'gtk_radio_button_new_with_label_from_widget';
function  gtk_radio_button_group(radio_button:PGtkRadioButton):PGSList;cdecl;external gtkdll name 'gtk_radio_button_group';
procedure gtk_radio_button_set_group(radio_button:PGtkRadioButton; group:PGSList);cdecl;external gtkdll name 'gtk_radio_button_set_group';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_RADIO_BUTTON(obj:pointer):boolean;
begin
  GTK_IS_RADIO_BUTTON:=(obj<>nil) and GTK_IS_RADIO_BUTTON_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_RADIO_BUTTON_CLASS(klass:pointer):boolean;
begin
  GTK_IS_RADIO_BUTTON_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_RADIO_BUTTON_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:05  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:36  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:12:58  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:14  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:11  peter
    * cdecl fixes

  Revision 1.5  1999/05/10 09:03:41  peter
    * gtk 1.2 port working

  Revision 1.4  1998/11/09 10:10:23  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:02  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

