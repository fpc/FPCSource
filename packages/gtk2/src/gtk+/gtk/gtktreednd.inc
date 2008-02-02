// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkTreeDragSource = pointer;
   // internal type
   PGtkTreeDragDest = pointer;

{ VTable - not signals  }
   PGtkTreeDragSourceIface = ^TGtkTreeDragSourceIface;
   TGtkTreeDragSourceIface = record
        g_iface : TGTypeInterface;
        row_draggable : function (drag_source:PGtkTreeDragSource; path:PGtkTreePath):gboolean; cdecl;
        drag_data_get : function (drag_source:PGtkTreeDragSource; path:PGtkTreePath; selection_data:PGtkSelectionData):gboolean; cdecl;
        drag_data_delete : function (drag_source:PGtkTreeDragSource; path:PGtkTreePath):gboolean; cdecl;
     end;


{ VTable - not signals  }
   PGtkTreeDragDestIface = ^TGtkTreeDragDestIface;
   TGtkTreeDragDestIface = record
        g_iface : TGTypeInterface;
        drag_data_received : function (drag_dest:PGtkTreeDragDest; dest:PGtkTreePath; selection_data:PGtkSelectionData):gboolean; cdecl;
        row_drop_possible : function (drag_dest:PGtkTreeDragDest; dest_path:PGtkTreePath; selection_data:PGtkSelectionData):gboolean; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TREE_DRAG_SOURCE : GType;
function GTK_TREE_DRAG_SOURCE(obj: pointer) : PGtkTreeDragSource;
function GTK_IS_TREE_DRAG_SOURCE(obj: pointer) : boolean;
function GTK_TREE_DRAG_SOURCE_GET_IFACE(obj: pointer) : PGtkTreeDragSourceIface;


function gtk_tree_drag_source_get_type:GType; cdecl; external gtklib;
{ Returns whether the given row can be dragged  }
function gtk_tree_drag_source_row_draggable(drag_source:PGtkTreeDragSource; path:PGtkTreePath):gboolean; cdecl; external gtklib;
{ Deletes the given row, or returns FALSE if it can't  }
function gtk_tree_drag_source_drag_data_delete(drag_source:PGtkTreeDragSource; path:PGtkTreePath):gboolean; cdecl; external gtklib;
{ Fills in selection_data with type selection_data->target based on
   the row denoted by path, returns TRUE if it does anything
  }
function gtk_tree_drag_source_drag_data_get(drag_source:PGtkTreeDragSource; path:PGtkTreePath; selection_data:PGtkSelectionData):gboolean; cdecl; external gtklib;
function GTK_TYPE_TREE_DRAG_DEST : GType;
function GTK_TREE_DRAG_DEST(obj: pointer) : PGtkTreeDragDest;
function GTK_IS_TREE_DRAG_DEST(obj: pointer) : boolean;
function GTK_TREE_DRAG_DEST_GET_IFACE(obj: pointer) : PGtkTreeDragDestIface;


function gtk_tree_drag_dest_get_type:GType; cdecl; external gtklib;
{ Inserts a row before dest which contains data in selection_data,
   or returns FALSE if it can't
  }
function gtk_tree_drag_dest_drag_data_received(drag_dest:PGtkTreeDragDest; dest:PGtkTreePath; selection_data:PGtkSelectionData):gboolean; cdecl; external gtklib;
{ Returns TRUE if we can drop before path; path may not exist.  }
function gtk_tree_drag_dest_row_drop_possible(drag_dest:PGtkTreeDragDest; dest_path:PGtkTreePath; selection_data:PGtkSelectionData):gboolean; cdecl; external gtklib;
{ The selection data would normally have target type GTK_TREE_MODEL_ROW in this
   case. If the target is wrong these functions return FALSE.
  }
function gtk_tree_set_row_drag_data(selection_data:PGtkSelectionData; tree_model:PGtkTreeModel; path:PGtkTreePath):gboolean; cdecl; external gtklib;
function gtk_tree_get_row_drag_data(selection_data:PGtkSelectionData; var tree_model:PGtkTreeModel; var path:PGtkTreePath):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_DRAG_SOURCE : GType;
begin
   GTK_TYPE_TREE_DRAG_SOURCE:=gtk_tree_drag_source_get_type;
end;

function GTK_TREE_DRAG_SOURCE(obj: pointer) : PGtkTreeDragSource;
begin
   GTK_TREE_DRAG_SOURCE:=PGtkTreeDragSource(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TREE_DRAG_SOURCE));
end;

function GTK_IS_TREE_DRAG_SOURCE(obj: pointer) : boolean;
begin
   GTK_IS_TREE_DRAG_SOURCE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TREE_DRAG_SOURCE);
end;

function GTK_TREE_DRAG_SOURCE_GET_IFACE(obj: pointer) : PGtkTreeDragSourceIface;
begin
   GTK_TREE_DRAG_SOURCE_GET_IFACE:=PGtkTreeDragSourceIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_TREE_DRAG_SOURCE));
end;

function GTK_TYPE_TREE_DRAG_DEST : GType;
begin
   GTK_TYPE_TREE_DRAG_DEST:=gtk_tree_drag_dest_get_type;
end;

function GTK_TREE_DRAG_DEST(obj: pointer) : PGtkTreeDragDest;
begin
   GTK_TREE_DRAG_DEST:=PGtkTreeDragDest(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_TREE_DRAG_DEST));
end;

function GTK_IS_TREE_DRAG_DEST(obj: pointer) : boolean;
begin
   GTK_IS_TREE_DRAG_DEST:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_TREE_DRAG_DEST);
end;

function GTK_TREE_DRAG_DEST_GET_IFACE(obj: pointer) : PGtkTreeDragDestIface;
begin
   GTK_TREE_DRAG_DEST_GET_IFACE:=PGtkTreeDragDestIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_TREE_DRAG_DEST));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

