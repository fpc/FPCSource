// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

{$IFDEF read_interface_types}
type
   PGtkCellLayout = pointer;
   PPGtkCellLayout = ^PGtkCellLayout;

   { keep in sync with GtkTreeCellDataFunc  }
   TGtkCellLayoutDataFunc = procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; tree_model:PGtkTreeModel; iter:PGtkTreeIter; data:gpointer);cdecl;
{ Virtual Table  }

   PGtkCellLayoutIface = ^TGtkCellLayoutIface;
   TGtkCellLayoutIface = record
        g_iface : TGTypeInterface;
        pack_start : procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; expand:gboolean);
        pack_end : procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; expand:gboolean);
        clear : procedure (cell_layout:PGtkCellLayout);
        add_attribute : procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; attribute:Pgchar; column:gint);
        set_cell_data_func : procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; func:TGtkCellLayoutDataFunc; func_data:gpointer; destroy:TGDestroyNotify);
        clear_attributes : procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer);
        reorder : procedure (cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; position:gint);
     end;

{$ENDIF read_interface_types}

{$IFDEF read_interface_rest}

function GTK_TYPE_CELL_LAYOUT : GType;
function GTK_CELL_LAYOUT(obj : pointer) : PGtkCellLayout;
function GTK_IS_CELL_LAYOUT(obj : pointer) : boolean;
function GTK_CELL_LAYOUT_GET_IFACE(obj : pointer) : PGtkCellLayoutIface;

function gtk_cell_layout_get_type:GType;cdecl;external gtklib name 'gtk_cell_layout_get_type';

procedure gtk_cell_layout_pack_start(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; expand:gboolean);cdecl;external gtklib name 'gtk_cell_layout_pack_start';
procedure gtk_cell_layout_pack_end(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; expand:gboolean);cdecl;external gtklib name 'gtk_cell_layout_pack_end';

procedure gtk_cell_layout_clear(cell_layout:PGtkCellLayout);cdecl;external gtklib name 'gtk_cell_layout_clear';

procedure gtk_cell_layout_set_attributes(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; args:array of const);cdecl;overload;external gtklib name 'gtk_cell_layout_set_attributes';
procedure gtk_cell_layout_set_attributes(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer);cdecl;overload;varargs;external gtklib name 'gtk_cell_layout_set_attributes';


procedure gtk_cell_layout_add_attribute(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; attribute:Pgchar; column:gint);cdecl;external gtklib name 'gtk_cell_layout_add_attribute';

procedure gtk_cell_layout_set_cell_data_func(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; func:TGtkCellLayoutDataFunc; func_data:gpointer; destroy:TGDestroyNotify);cdecl;external gtklib name 'gtk_cell_layout_set_cell_data_func';

procedure gtk_cell_layout_clear_attributes(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer);cdecl;external gtklib name 'gtk_cell_layout_clear_attributes';

procedure gtk_cell_layout_reorder(cell_layout:PGtkCellLayout; cell:PGtkCellRenderer; position:gint);cdecl;external gtklib name 'gtk_cell_layout_reorder';

{$endif read_interface_rest}

{$ifdef read_implementation}

function GTK_TYPE_CELL_LAYOUT : GType;
begin
   GTK_TYPE_CELL_LAYOUT:=gtk_cell_layout_get_type;
end;

function GTK_CELL_LAYOUT(obj : pointer) : PGtkCellLayout;
begin
   GTK_CELL_LAYOUT:=PGtkCellLayout(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_CELL_LAYOUT));
end;

function GTK_IS_CELL_LAYOUT(obj : pointer) : boolean;
begin
   GTK_IS_CELL_LAYOUT:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_CELL_LAYOUT);
end;

function GTK_CELL_LAYOUT_GET_IFACE(obj : pointer) : PGtkCellLayoutIface;
begin
   GTK_CELL_LAYOUT_GET_IFACE:=PGtkCellLayoutIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_CELL_LAYOUT));
end;

{$endif read_implementation}
