// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
  PGtkCellViewPrivate = pointer;

  PGtkCellView = ^TGtkCellView;
  TGtkCellView = record
       parent_instance : TGtkWidget;
       priv : PGtkCellViewPrivate;
    end;

  PGtkCellViewClass = ^TGtkCellViewClass;
  TGtkCellViewClass = record
       parent_class : TGtkWidgetClass;
    end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_CELL_VIEW: GType;
function GTK_CELL_VIEW(obj : pointer) : PGtkCellView;
function GTK_CELL_VIEW_CLASS(vtable : pointer) : PGtkCellViewClass;
function GTK_IS_CELL_VIEW(obj : pointer) : boolean;
function GTK_IS_CELL_VIEW_CLASS(vtable : pointer) : boolean;
function GTK_CELL_VIEW_GET_CLASS(inst : pointer) : PGtkCellViewClass;


function gtk_cell_view_get_type: GType;cdecl;external gtklib;
function gtk_cell_view_new:PGtkWidget;cdecl;external gtklib;
function gtk_cell_view_new_with_text(text:Pgchar):PGtkWidget;cdecl;external gtklib;
function gtk_cell_view_new_with_markup(markup:Pgchar):PGtkWidget;cdecl;external gtklib;
function gtk_cell_view_new_with_pixbuf(pixbuf:PGdkPixbuf):PGtkWidget;cdecl;external gtklib;
procedure gtk_cell_view_set_model(cell_view:PGtkCellView; model:PGtkTreeModel);cdecl;external gtklib;
procedure gtk_cell_view_set_displayed_row(cell_view:PGtkCellView; path:PGtkTreePath);cdecl;external gtklib;
function gtk_cell_view_get_displayed_row(cell_view:PGtkCellView):PGtkTreePath;cdecl;external gtklib;
function gtk_cell_view_get_size_of_row(cell_view:PGtkCellView; path:PGtkTreePath; requisition:PGtkRequisition):gboolean;cdecl;external gtklib;
procedure gtk_cell_view_set_background_color(cell_view:PGtkCellView; color:PGdkColor);cdecl;external gtklib;
function gtk_cell_view_get_cell_renderers(cell_view:PGtkCellView):PGList;cdecl;external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}


function GTK_TYPE_CELL_VIEW: GType;
begin
  GTK_TYPE_CELL_VIEW:=gtk_cell_view_get_type;
end;

function GTK_CELL_VIEW(obj : pointer) : PGtkCellView;
begin
  GTK_CELL_VIEW:=PGtkCellView(G_TYPE_CHECK_INSTANCE_CAST(obj,GTK_TYPE_CELL_VIEW));
end;

function GTK_CELL_VIEW_CLASS(vtable : pointer) : PGtkCellViewClass;
begin
  GTK_CELL_VIEW_CLASS:=PGtkCellViewClass(G_TYPE_CHECK_CLASS_CAST(vtable,GTK_TYPE_CELL_VIEW));
end;

function GTK_IS_CELL_VIEW(obj : pointer) : boolean;
begin
  GTK_IS_CELL_VIEW:=G_TYPE_CHECK_INSTANCE_TYPE(obj,GTK_TYPE_CELL_VIEW);
end;

function GTK_IS_CELL_VIEW_CLASS(vtable : pointer) : boolean;
begin
  GTK_IS_CELL_VIEW_CLASS:=G_TYPE_CHECK_CLASS_TYPE(vtable,GTK_TYPE_CELL_VIEW);
end;

function GTK_CELL_VIEW_GET_CLASS(inst : pointer) : PGtkCellViewClass;
begin
  GTK_CELL_VIEW_GET_CLASS:=PGtkCellViewClass(G_TYPE_INSTANCE_GET_CLASS(inst,GTK_TYPE_CELL_VIEW));
end;

{$ENDIF read_implementation}
// included by gtk2.pas
