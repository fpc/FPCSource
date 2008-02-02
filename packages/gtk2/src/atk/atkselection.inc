// included by atk.pp

{
   This AtkSelection interface provides the standard mechanism for an
   assistive technology to determine what the current selected children are,
   as well as modify the selection set. Any anObject that has children that
   can be selected should support the AtkSelection interface.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ signal handlers  }
   PAtkSelectionIface = ^TAtkSelectionIface;
   TAtkSelectionIface = record
        parent : TGTypeInterface;
        add_selection : function (selection:PAtkSelection; i:gint):gboolean; cdecl;
        clear_selection : function (selection:PAtkSelection):gboolean; cdecl;
        ref_selection : function (selection:PAtkSelection; i:gint):PAtkObject; cdecl;
        get_selection_count : function (selection:PAtkSelection):gint; cdecl;
        is_child_selected : function (selection:PAtkSelection; i:gint):gboolean; cdecl;
        remove_selection : function (selection:PAtkSelection; i:gint):gboolean; cdecl;
        select_all_selection : function (selection:PAtkSelection):gboolean; cdecl;
        selection_changed : procedure (selection:PAtkSelection); cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_SELECTION : GType;
function ATK_IS_SELECTION(obj: pointer) : boolean;
function ATK_SELECTION(obj: pointer) : PAtkSelection;
function ATK_SELECTION_GET_IFACE(obj: pointer) : PAtkSelectionIface;


function atk_selection_get_type:GType; cdecl; external atklib;
function atk_selection_add_selection(selection:PAtkSelection; i:gint):gboolean; cdecl; external atklib;
function atk_selection_clear_selection(selection:PAtkSelection):gboolean; cdecl; external atklib;
function atk_selection_ref_selection(selection:PAtkSelection; i:gint):PAtkObject; cdecl; external atklib;
function atk_selection_get_selection_count(selection:PAtkSelection):gint; cdecl; external atklib;
function atk_selection_is_child_selected(selection:PAtkSelection; i:gint):gboolean; cdecl; external atklib;
function atk_selection_remove_selection(selection:PAtkSelection; i:gint):gboolean; cdecl; external atklib;
function atk_selection_select_all_selection(selection:PAtkSelection):gboolean; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_SELECTION : GType;
begin
   ATK_TYPE_SELECTION:=atk_selection_get_type;
end;

function ATK_IS_SELECTION(obj: pointer) : boolean;
begin
   ATK_IS_SELECTION:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_SELECTION);
end;

function ATK_SELECTION(obj: pointer) : PAtkSelection;
begin
   ATK_SELECTION:=PAtkSelection(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_SELECTION));
end;

function ATK_SELECTION_GET_IFACE(obj: pointer) : PAtkSelectionIface;
begin
   ATK_SELECTION_GET_IFACE:=PAtkSelectionIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_SELECTION));
end;

{$ENDIF read_implementation}

