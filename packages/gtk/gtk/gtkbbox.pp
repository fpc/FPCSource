{
   $Id$
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    const
       GTK_BUTTONBOX_DEFAULT = -1;

    type
       PGtkButtonBox = ^TGtkButtonBox;
       TGtkButtonBox = record
            box : TGtkBox;
            spacing : gint;
            child_min_width : gint;
            child_min_height : gint;
            child_ipad_x : gint;
            child_ipad_y : gint;
            layout_style : TGtkButtonBoxStyle;
         end;

       PGtkButtonBoxClass = ^TGtkButtonBoxClass;
       TGtkButtonBoxClass = record
            parent_class : TGtkBoxClass;
         end;

Type
  GTK_BUTTON_BOX=PGtkButtonBox;
  GTK_BUTTON_BOX_CLASS=PGtkButtonBoxClass;

function  GTK_BUTTON_BOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_button_box_get_type';
function  GTK_IS_BUTTON_BOX(obj:pointer):boolean;
function  GTK_IS_BUTTON_BOX_CLASS(klass:pointer):boolean;

function  gtk_button_box_get_type:TGtktype;cdecl;external gtkdll name 'gtk_button_box_get_type';
procedure gtk_button_box_get_child_size_default(min_width:Pgint; min_height:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_size_default';
procedure gtk_button_box_get_child_ipadding_default(ipad_x:Pgint; ipad_y:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_ipadding_default';
procedure gtk_button_box_set_child_size_default(min_width:gint; min_height:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_size_default';
procedure gtk_button_box_set_child_ipadding_default(ipad_x:gint; ipad_y:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_ipadding_default';
function  gtk_button_box_get_spacing(widget:PGtkButtonBox):gint;cdecl;external gtkdll name 'gtk_button_box_get_spacing';
function  gtk_button_box_get_layout(widget:PGtkButtonBox):TGtkButtonBoxStyle;cdecl;external gtkdll name 'gtk_button_box_get_layout';
procedure gtk_button_box_get_child_size(widget:PGtkButtonBox; min_width:Pgint; min_height:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_size';
procedure gtk_button_box_get_child_ipadding(widget:PGtkButtonBox; ipad_x:Pgint; ipad_y:Pgint);cdecl;external gtkdll name 'gtk_button_box_get_child_ipadding';
procedure gtk_button_box_set_spacing(widget:PGtkButtonBox; spacing:gint);cdecl;external gtkdll name 'gtk_button_box_set_spacing';
procedure gtk_button_box_set_layout(widget:PGtkButtonBox; layout_style:TGtkButtonBoxStyle);cdecl;external gtkdll name 'gtk_button_box_set_layout';
procedure gtk_button_box_set_child_size(widget:PGtkButtonBox; min_width:gint; min_height:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_size';
procedure gtk_button_box_set_child_ipadding(widget:PGtkButtonBox; ipad_x:gint; ipad_y:gint);cdecl;external gtkdll name 'gtk_button_box_set_child_ipadding';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_BUTTON_BOX(obj:pointer):boolean;
begin
  GTK_IS_BUTTON_BOX:=(obj<>nil) and GTK_IS_BUTTON_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_BUTTON_BOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_BUTTON_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_BUTTON_BOX_TYPE);
end;

{$endif read_implementation}

{
  $Log$
  Revision 1.1  2000-07-13 06:34:03  michael
  + Initial import

  Revision 1.1  1999/11/24 23:36:35  peter
    * moved to packages dir

  Revision 1.12  1999/10/06 17:42:48  peter
    * external is now only in the interface
    * removed gtk 1.0 support

  Revision 1.11  1999/07/23 16:11:57  peter
    * use packrecords C

  Revision 1.10  1999/05/11 00:38:09  peter
    * win32 fixes

  Revision 1.9  1999/05/10 15:18:56  peter
    * cdecl fixes

  Revision 1.8  1999/05/07 17:40:12  peter
    * more updates

  Revision 1.7  1999/05/07 15:09:53  peter
    * more fixes

  Revision 1.6  1999/05/07 10:40:28  peter
    * first things for 1.2

  Revision 1.5  1998/11/09 10:09:34  peter
    + C type casts are now correctly handled

  Revision 1.4  1998/10/22 11:37:39  peter
    * fixes for win32

  Revision 1.3  1998/10/21 20:22:09  peter
    * cdecl, packrecord fixes (from the gtk.tar.gz)
    * win32 support
    * gtk.pp,gdk.pp for an all in one unit

}


