// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkTreeSelection = ^TGtkTreeSelection;

   TGtkTreeSelectionFunc = function (selection:PGtkTreeSelection; model:PGtkTreeModel; path:PGtkTreePath; path_currently_selected:gboolean; data:gpointer):gboolean; cdecl;

   TGtkTreeSelectionForeachFunc = procedure (model:PGtkTreeModel; path:PGtkTreePath; iter:PGtkTreeIter; data:gpointer); cdecl;
{< private > }
   TGtkTreeSelection = record
        parent : TGObject;
        tree_view : PGtkTreeView;
        _type : TGtkSelectionMode;
        user_func : TGtkTreeSelectionFunc;
        user_data : gpointer;
        destroy : TGtkDestroyNotify;
     end;

{ Padding for future expansion  }
   PGtkTreeSelectionClass = ^TGtkTreeSelectionClass;
   TGtkTreeSelectionClass = record
        parent_class : TGObjectClass;
        changed : procedure (selection:PGtkTreeSelection); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_TREE_SELECTION : GType;
function GTK_TREE_SELECTION(obj: pointer) : PGtkTreeSelection;
function GTK_TREE_SELECTION_CLASS(klass: pointer) : PGtkTreeSelectionClass;
function GTK_IS_TREE_SELECTION(obj: pointer) : boolean;
function GTK_IS_TREE_SELECTION_CLASS(klass: pointer) : boolean;
function GTK_TREE_SELECTION_GET_CLASS(obj: pointer) : PGtkTreeSelectionClass;



function gtk_tree_selection_get_type:TGtkType; cdecl; external gtklib;
procedure gtk_tree_selection_set_mode(selection:PGtkTreeSelection; _type:TGtkSelectionMode); cdecl; external gtklib;
function gtk_tree_selection_get_mode(selection:PGtkTreeSelection):TGtkSelectionMode; cdecl; external gtklib;
procedure gtk_tree_selection_set_select_function(selection:PGtkTreeSelection; func:TGtkTreeSelectionFunc; data:gpointer; destroy:TGtkDestroyNotify); cdecl; external gtklib;
function gtk_tree_selection_get_user_data(selection:PGtkTreeSelection):gpointer; cdecl; external gtklib;
function gtk_tree_selection_get_tree_view(selection:PGtkTreeSelection):PGtkTreeView; cdecl; external gtklib;
{ Only meaningful if GTK_TREE_SELECTION_SINGLE is set  }
{ Use selected_foreach for GTK_TREE_SELECTION_MULTI  }
function gtk_tree_selection_get_selected(selection:PGtkTreeSelection; model:PPGtkTreeModel; iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
procedure gtk_tree_selection_selected_foreach(selection:PGtkTreeSelection; func:TGtkTreeSelectionForeachFunc; data:gpointer); cdecl; external gtklib;
procedure gtk_tree_selection_select_path(selection:PGtkTreeSelection; path:PGtkTreePath); cdecl; external gtklib;
procedure gtk_tree_selection_unselect_path(selection:PGtkTreeSelection; path:PGtkTreePath); cdecl; external gtklib;
procedure gtk_tree_selection_select_iter(selection:PGtkTreeSelection; iter:PGtkTreeIter); cdecl; external gtklib;
procedure gtk_tree_selection_unselect_iter(selection:PGtkTreeSelection; iter:PGtkTreeIter); cdecl; external gtklib;
function gtk_tree_selection_path_is_selected(selection:PGtkTreeSelection; path:PGtkTreePath):gboolean; cdecl; external gtklib;
function gtk_tree_selection_iter_is_selected(selection:PGtkTreeSelection; iter:PGtkTreeIter):gboolean; cdecl; external gtklib;
procedure gtk_tree_selection_select_all(selection:PGtkTreeSelection); cdecl; external gtklib;
procedure gtk_tree_selection_unselect_all(selection:PGtkTreeSelection); cdecl; external gtklib;
procedure gtk_tree_selection_select_range(selection:PGtkTreeSelection; start_path:PGtkTreePath; end_path:PGtkTreePath); cdecl; external gtklib;

{$IFDEF HasGTK2_2}
function gtk_tree_selection_count_selected_rows(selection:PGtkTreeSelection):gint; cdecl; external gtklib;
function gtk_tree_selection_get_selected_rows(selection:PGtkTreeSelection; model:PPGtkTreeModel):PGList; cdecl; external gtklib;
procedure gtk_tree_selection_unselect_range(selection:PGtkTreeSelection; start_path:PGtkTreePath; end_path:PGtkTreePath); cdecl; external gtklib;
{$ENDIF}
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_TREE_SELECTION : GType;
begin
   GTK_TYPE_TREE_SELECTION:=gtk_tree_selection_get_type;
end;

function GTK_TREE_SELECTION(obj: pointer) : PGtkTreeSelection;
begin
   GTK_TREE_SELECTION:=PGtkTreeSelection(GTK_CHECK_CAST(obj,GTK_TYPE_TREE_SELECTION));
end;

function GTK_TREE_SELECTION_CLASS(klass: pointer) : PGtkTreeSelectionClass;
begin
   GTK_TREE_SELECTION_CLASS:=PGtkTreeSelectionClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_TREE_SELECTION));
end;

function GTK_IS_TREE_SELECTION(obj: pointer) : boolean;
begin
   GTK_IS_TREE_SELECTION:=GTK_CHECK_TYPE(obj,GTK_TYPE_TREE_SELECTION);
end;

function GTK_IS_TREE_SELECTION_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_TREE_SELECTION_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_TREE_SELECTION);
end;

function GTK_TREE_SELECTION_GET_CLASS(obj: pointer) : PGtkTreeSelectionClass;
begin
   GTK_TREE_SELECTION_GET_CLASS:=PGtkTreeSelectionClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_TREE_SELECTION));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

