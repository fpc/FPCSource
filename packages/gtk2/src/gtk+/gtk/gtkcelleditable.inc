// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkCellEditable = pointer;

{ signals  }
{ virtual table  }
   PGtkCellEditableIface = ^TGtkCellEditableIface;
   TGtkCellEditableIface = record
        g_iface : TGTypeInterface;
        editing_done : procedure (cell_editable:PGtkCellEditable); cdecl;
        remove_widget : procedure (cell_editable:PGtkCellEditable); cdecl;
        start_editing : procedure (cell_editable:PGtkCellEditable; event:PGdkEvent); cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_CELL_EDITABLE : GType;
function GTK_CELL_EDITABLE(obj: pointer) : PGtkCellEditable;
function GTK_CELL_EDITABLE_CLASS(obj: pointer) : PGtkCellEditableIface;
function GTK_IS_CELL_EDITABLE(obj: pointer) : boolean;
function GTK_CELL_EDITABLE_GET_IFACE(obj: pointer) : PGtkCellEditableIface;


function gtk_cell_editable_get_type:GType; cdecl; external gtklib;
procedure gtk_cell_editable_start_editing(cell_editable:PGtkCellEditable; event:PGdkEvent); cdecl; external gtklib;
procedure gtk_cell_editable_editing_done(cell_editable:PGtkCellEditable); cdecl; external gtklib;
procedure gtk_cell_editable_remove_widget(cell_editable:PGtkCellEditable); cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_CELL_EDITABLE : GType;
begin
   GTK_TYPE_CELL_EDITABLE:=gtk_cell_editable_get_type;
end;

function GTK_CELL_EDITABLE(obj: pointer) : PGtkCellEditable;
begin
   GTK_CELL_EDITABLE:=PGtkCellEditable(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_CELL_EDITABLE));
end;

function GTK_CELL_EDITABLE_CLASS(obj: pointer) : PGtkCellEditableIface;
begin
   GTK_CELL_EDITABLE_CLASS:=PGtkCellEditableIface(G_TYPE_CHECK_CLASS_CAST(obj,GTK_TYPE_CELL_EDITABLE));
end;

function GTK_IS_CELL_EDITABLE(obj: pointer) : boolean;
begin
   GTK_IS_CELL_EDITABLE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_CELL_EDITABLE);
end;

function GTK_CELL_EDITABLE_GET_IFACE(obj: pointer) : PGtkCellEditableIface;
begin
   GTK_CELL_EDITABLE_GET_IFACE:=PGtkCellEditableIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,GTK_TYPE_CELL_EDITABLE));
end;
{$ENDIF read_implementation}
// included by gtk2.pas

