{$IfDef read_interface}

const
   GNOME_PROPERTY_BOX_DIRTY = 'gnome_property_box_dirty';

type
   PGnomePropertyBox = ^TGnomePropertyBox;
   TGnomePropertyBox = record
        dialog : TGnomeDialog;
        notebook : PGtkWidget;
        ok_button : PGtkWidget;
        apply_button : PGtkWidget;
        cancel_button : PGtkWidget;
        help_button : PGtkWidget;
     end;
   GNOME_PROPERTY_BOX = PGnomePropertyBox;

   PGnomePropertyBoxClass = ^TGnomePropertyBoxClass;
   TGnomePropertyBoxClass = record
        parent_class : TGnomeDialogClass;
        apply : procedure (propertybox:PGnomePropertyBox; page_num:gint);cdecl;
        help : procedure (propertybox:PGnomePropertyBox; page_num:gint);cdecl;
     end;
   GNOME_PROPERTY_BOX_CLASS = PGnomePropertyBoxClass;

function GNOME_TYPE_PROPERTY_BOX : TGTKType;
function GNOME_IS_PROPERTY_BOX(obj : Pointer) : Boolean;
function GNOME_IS_PROPERTY_BOX_CLASS(klass : Pointer) : Boolean;

function gnome_property_box_get_type:TGTKType;cdecl;external libgnomeuidll name 'gnome_property_box_get_type';
function gnome_property_box_new:PGtkWidget;cdecl;external libgnomeuidll name 'gnome_property_box_new';
procedure gnome_property_box_changed(property_box:PGnomePropertyBox);cdecl;external libgnomeuidll name 'gnome_property_box_changed';
procedure gnome_property_box_set_modified(property_box:PGnomePropertyBox; state:gboolean);cdecl;external libgnomeuidll name 'gnome_property_box_set_modified';
function gnome_property_box_append_page(property_box:PGnomePropertyBox; child:PGtkWidget; tab_label:PGtkWidget):gint;cdecl;external libgnomeuidll name 'gnome_property_box_append_page';

{$EndIf read_interface}

{$Ifdef read_implementation}

function GNOME_TYPE_PROPERTY_BOX : TGTKType;
begin
  GNOME_TYPE_PROPERTY_BOX:=gnome_property_box_get_type;
end;

function GNOME_IS_PROPERTY_BOX(obj : Pointer) : Boolean;
begin
   GNOME_IS_PROPERTY_BOX:=(obj<>nil) and GNOME_IS_PROPERTY_BOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function GNOME_IS_PROPERTY_BOX_CLASS(klass : Pointer) : Boolean;
begin
   GNOME_IS_PROPERTY_BOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GNOME_TYPE_PROPERTY_BOX);
end;

{$Endif read_implementation}
