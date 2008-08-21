{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkComboBoxPrivate = pointer;

   PGtkComboBox = ^TGtkComboBox;
   TGtkComboBox = record
        parent_instance : TGtkBin;
        priv : PGtkComboBoxPrivate;
     end;

{ signals  }
{ Padding for future expansion  }
   PGtkComboBoxClass = ^TGtkComboBoxClass;
   TGtkComboBoxClass = record
        parent_class : TGtkBinClass;
        changed : procedure (combo_box:PGtkComboBox);cdecl;
        _gtk_reserved0 : procedure ;
        _gtk_reserved1 : procedure ;
        _gtk_reserved2 : procedure ;
        _gtk_reserved3 : procedure ;
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_COMBO_BOX : GType;
function GTK_COMBO_BOX(obj : pointer) : PGtkComboBox;
function GTK_COMBO_BOX_CLASS(vtable : pointer) : PGtkComboBoxClass;
function GTK_IS_COMBO_BOX(obj : pointer) : gboolean;
function GTK_IS_COMBO_BOX_CLASS(vtable : pointer) : gboolean;
function GTK_COMBO_BOX_GET_CLASS(inst : pointer) : PGtkComboBoxClass;


{ construction  }

function gtk_combo_box_get_type:GType;cdecl;external gtklib name 'gtk_combo_box_get_type';
function gtk_combo_box_new:PGtkWidget;cdecl;external gtklib name 'gtk_combo_box_new';
function gtk_combo_box_new_with_model(model:PGtkTreeModel):PGtkWidget;cdecl;external gtklib name 'gtk_combo_box_new_with_model';

{ grids  }
procedure gtk_combo_box_set_wrap_width(combo_box:PGtkComboBox; width:gint);cdecl;external gtklib name 'gtk_combo_box_set_wrap_width';
procedure gtk_combo_box_set_row_span_column(combo_box:PGtkComboBox; row_span:gint);cdecl;external gtklib name 'gtk_combo_box_set_row_span_column';
procedure gtk_combo_box_set_column_span_column(combo_box:PGtkComboBox; column_span:gint);cdecl;external gtklib name 'gtk_combo_box_set_column_span_column';

{ get/set active item  }
function gtk_combo_box_get_active(combo_box:PGtkComboBox):gint;cdecl;external gtklib name 'gtk_combo_box_get_active';
procedure gtk_combo_box_set_active(combo_box:PGtkComboBox; index:gint);cdecl;external gtklib name 'gtk_combo_box_set_active';
function gtk_combo_box_get_active_iter(combo_box:PGtkComboBox; iter:PGtkTreeIter):gboolean;cdecl;external gtklib name 'gtk_combo_box_get_active_iter';
procedure gtk_combo_box_set_active_iter(combo_box:PGtkComboBox; iter:PGtkTreeIter);cdecl;external gtklib name 'gtk_combo_box_set_active_iter';


{ getters and setters  }
procedure gtk_combo_box_set_model(combo_box:PGtkComboBox; model:PGtkTreeModel);cdecl;external gtklib name 'gtk_combo_box_set_model';
function gtk_combo_box_get_model(combo_box:PGtkComboBox):PGtkTreeModel;cdecl;external gtklib name 'gtk_combo_box_get_model';


{ convenience -- text  }
function gtk_combo_box_new_text:PGtkWidget;cdecl;external gtklib name 'gtk_combo_box_new_text';

procedure gtk_combo_box_append_text(combo_box:PGtkComboBox; text:Pgchar);cdecl;external gtklib name 'gtk_combo_box_append_text';

procedure gtk_combo_box_insert_text(combo_box:PGtkComboBox; position:gint; text:Pgchar);cdecl;external gtklib name 'gtk_combo_box_insert_text';

procedure gtk_combo_box_prepend_text(combo_box:PGtkComboBox; text:Pgchar);cdecl;external gtklib name 'gtk_combo_box_prepend_text';
procedure gtk_combo_box_remove_text(combo_box:PGtkComboBox; position:gint);cdecl;external gtklib name 'gtk_combo_box_remove_text';
{ programmatic control  }
procedure gtk_combo_box_popup(combo_box:PGtkComboBox);cdecl;external gtklib name 'gtk_combo_box_popup';
procedure gtk_combo_box_popdown(combo_box:PGtkComboBox);cdecl;external gtklib name 'gtk_combo_box_popdown';

{$ifdef HasGTK2_6}
function gtk_combo_box_get_active_text(combo_box:PGtkComboBox):Pgchar;cdecl;external gtklib;
function gtk_combo_box_get_column_span_column(combo_box:PGtkComboBox):gint;cdecl;external gtklib;
function gtk_combo_box_get_focus_on_click(combo:PGtkComboBox):gboolean;cdecl;external gtklib;
function gtk_combo_box_get_popup_accessible(combo_box:PGtkComboBox):PAtkObject;cdecl;external gtklib;
function gtk_combo_box_get_row_separator_func(combo_box:PGtkComboBox):TGtkTreeViewRowSeparatorFunc;cdecl;external gtklib;
function gtk_combo_box_get_row_span_column(combo_box:PGtkComboBox):gint;cdecl;external gtklib;
function gtk_combo_box_get_wrap_width(combo_box:PGtkComboBox):gint;cdecl;external gtklib;
procedure gtk_combo_box_set_add_tearoffs(combo_box:PGtkComboBox; add_tearoffs:gboolean);cdecl;external gtklib;
procedure gtk_combo_box_set_focus_on_click(combo:PGtkComboBox; focus_on_click:gboolean);cdecl;external gtklib;
procedure gtk_combo_box_set_row_separator_func(combo_box:PGtkComboBox; func:TGtkTreeViewRowSeparatorFunc; data:gpointer; destroy:TGtkDestroyNotify);cdecl;external gtklib;
{$endif}

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_COMBO_BOX : GType;
begin
   GTK_TYPE_COMBO_BOX:=gtk_combo_box_get_type;
end;

function GTK_COMBO_BOX(obj : pointer) : PGtkComboBox;
begin
   GTK_COMBO_BOX:=PGtkComboBox(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_COMBO_BOX));
end;

function GTK_COMBO_BOX_CLASS(vtable : pointer) : PGtkComboBoxClass;
begin
   GTK_COMBO_BOX_CLASS:=PGtkComboBoxClass(G_TYPE_CHECK_CLASS_CAST(vtable,GTK_TYPE_COMBO_BOX));
end;

function GTK_IS_COMBO_BOX(obj : pointer) : gboolean;
begin
   GTK_IS_COMBO_BOX:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_COMBO_BOX);
end;


function GTK_IS_COMBO_BOX_CLASS(vtable : pointer) : gboolean;
begin
   GTK_IS_COMBO_BOX_CLASS:=G_TYPE_CHECK_CLASS_TYPE(vtable,GTK_TYPE_COMBO_BOX);
end;

function GTK_COMBO_BOX_GET_CLASS(inst : pointer) : PGtkComboBoxClass;
begin
   GTK_COMBO_BOX_GET_CLASS:=PGtkComboBoxClass(G_TYPE_INSTANCE_GET_CLASS(inst,GTK_TYPE_COMBO_BOX));
end;

{$endif read_implementation}
