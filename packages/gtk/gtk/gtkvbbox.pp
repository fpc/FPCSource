{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkVButtonBox = ^TGtkVButtonBox;
     TGtkVButtonBox = record
          button_box : TGtkButtonBox;
       end;

     PGtkVButtonBoxClass = ^TGtkVButtonBoxClass;
     TGtkVButtonBoxClass = record
          parent_class : TGtkButtonBoxClass;
       end;

Type
  GTK_VBUTTON_BOX=PGtkVButtonBox;
  GTK_VBUTTON_BOX_CLASS=PGtkVButtonBoxClass;

function  GTK_VBUTTON_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vbutton_box_get_type';
function  GTK_IS_VBUTTON_BOX(obj:pointer):boolean;
function  GTK_IS_VBUTTON_BOX_CLASS(klass:pointer):boolean;

function  gtk_vbutton_box_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vbutton_box_get_type';
function  gtk_vbutton_box_new:PGtkWidget;cdecl;external gtkdll name 'gtk_vbutton_box_new';
function  gtk_vbutton_box_get_spacing_default:gint;cdecl;external gtkdll name 'gtk_vbutton_box_get_spacing_default';
procedure gtk_vbutton_box_set_spacing_default(spacing:gint);cdecl;external gtkdll name 'gtk_vbutton_box_set_spacing_default';
function  gtk_vbutton_box_get_layout_default:TGtkButtonBoxStyle;cdecl;external gtkdll name 'gtk_vbutton_box_get_layout_default';
procedure gtk_vbutton_box_set_layout_default(layout:TGtkButtonBoxStyle);cdecl;external gtkdll name 'gtk_vbutton_box_set_layout_default';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VBUTTON_BOX(obj:pointer):boolean;
begin
  GTK_IS_VBUTTON_BOX:=(obj<>nil) and GTK_IS_VBUTTON_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VBUTTON_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VBUTTON_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VBUTTON_BOX_TYPE);
end;

{$endif read_implementation}


{
  $Log$
  Revision 1.1  2000-07-13 06:34:07  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:37  peter
    * moved to packages dir

  Revision 1.9  1999/10/06 17:42:50  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.8  1999/07/23 16:13:21  peter
    * use packrecords C

  Revision 1.7  1999/05/11 00:39:42  peter
    * win32 fixes

  Revision 1.6  1999/05/10 15:20:41  peter
    * cdecl fixes

  Revision 1.5  1999/05/07 15:10:17  peter
    * more fixes

  Revision 1.4  1998/11/09 10:10:42  peter
    + C type casts are now correctly handled

  Revision 1.3  1998/10/21 20:23:24  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}

