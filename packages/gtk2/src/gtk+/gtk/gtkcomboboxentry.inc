{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkComboBoxEntryPrivate = pointer;

   PGtkComboBoxEntry = ^TGtkComboBoxEntry;
   TGtkComboBoxEntry = record
        parent_instance : TGtkComboBox;
        priv : PGtkComboBoxEntryPrivate;
     end;

{ Padding for future expansion  }
   PGtkComboBoxEntryClass = ^TGtkComboBoxEntryClass;
   TGtkComboBoxEntryClass = record
        parent_class : TGtkComboBoxClass;
        _gtk_reserved0 : procedure ;cdecl;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}
function GTK_TYPE_COMBO_BOX_ENTRY : GType;
function GTK_COMBO_BOX_ENTRY(obj : pointer) : PGtkComboBoxEntry;
function GTK_COMBO_BOX_ENTRY_CLASS(vtable : pointer) : PGtkComboBoxEntryClass;
function GTK_IS_COMBO_BOX_ENTRY(obj : pointer) : gboolean;
function GTK_IS_COMBO_BOX_ENTRY_CLASS(vtable : pointer) : gboolean;
function GTK_COMBO_BOX_ENTRY_GET_CLASS(inst : pointer) : PGtkComboBoxEntryClass;

function gtk_combo_box_entry_get_type:GType;cdecl;external gtklib name 'gtk_combo_box_entry_get_type';
function gtk_combo_box_entry_new:PGtkWidget;cdecl;external gtklib name 'gtk_combo_box_entry_new';
function gtk_combo_box_entry_new_with_model(model:PGtkTreeModel; text_column:gint):PGtkWidget;cdecl;external gtklib name 'gtk_combo_box_entry_new_with_model';
procedure gtk_combo_box_entry_set_text_column(entry_box:PGtkComboBoxEntry; text_column:gint);cdecl;external gtklib name 'gtk_combo_box_entry_set_text_column';
function gtk_combo_box_entry_get_text_column(entry_box:PGtkComboBoxEntry):gint;cdecl;external gtklib name 'gtk_combo_box_entry_get_text_column';


{ convenience -- text  }
function gtk_combo_box_entry_new_text:PGtkWidget;cdecl;external gtklib name 'gtk_combo_box_entry_new_text';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_COMBO_BOX_ENTRY : GType;
begin
   GTK_TYPE_COMBO_BOX_ENTRY:=gtk_combo_box_entry_get_type;
end;

function GTK_COMBO_BOX_ENTRY(obj : pointer) : PGtkComboBoxEntry;
begin
   GTK_COMBO_BOX_ENTRY:=PGtkComboBoxEntry(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_COMBO_BOX_ENTRY));
end;

function GTK_COMBO_BOX_ENTRY_CLASS(vtable : pointer) : PGtkComboBoxEntryClass;
begin
   GTK_COMBO_BOX_ENTRY_CLASS:=PGtkComboBoxEntryClass(G_TYPE_CHECK_CLASS_CAST(vtable,GTK_TYPE_COMBO_BOX_ENTRY));
end;

function GTK_IS_COMBO_BOX_ENTRY(obj : pointer) : gboolean;
begin
   GTK_IS_COMBO_BOX_ENTRY:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_COMBO_BOX_ENTRY);
end;

function GTK_IS_COMBO_BOX_ENTRY_CLASS(vtable : pointer) : gboolean;
begin
   GTK_IS_COMBO_BOX_ENTRY_CLASS:=G_TYPE_CHECK_CLASS_TYPE(vtable,GTK_TYPE_COMBO_BOX_ENTRY);
end;

function GTK_COMBO_BOX_ENTRY_GET_CLASS(inst : pointer) : PGtkComboBoxEntryClass;
begin
   GTK_COMBO_BOX_ENTRY_GET_CLASS:=PGtkComboBoxEntryClass(G_TYPE_INSTANCE_GET_CLASS(inst,GTK_TYPE_COMBO_BOX_ENTRY));
end;

{$endif read_implementation}
