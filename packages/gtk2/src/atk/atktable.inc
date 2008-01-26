// included by atk.pp

{
   AtkTable describes a user-interface component that presents data in
   two-dimensional table format.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{
     signal handlers
    }
   PAtkTableIface = ^TAtkTableIface;
   TAtkTableIface = record
        parent : TGTypeInterface;
        ref_at : function (table:PAtkTable; row:gint; column:gint):PAtkObject; cdecl;
        get_index_at : function (table:PAtkTable; row:gint; column:gint):gint; cdecl;
        get_column_at_index : function (table:PAtkTable; index:gint):gint; cdecl;
        get_row_at_index : function (table:PAtkTable; index:gint):gint; cdecl;
        get_n_columns : function (table:PAtkTable):gint; cdecl;
        get_n_rows : function (table:PAtkTable):gint; cdecl;
        get_column_extent_at : function (table:PAtkTable; row:gint; column:gint):gint; cdecl;
        get_row_extent_at : function (table:PAtkTable; row:gint; column:gint):gint; cdecl;
        get_caption : function (table:PAtkTable):PAtkObject; cdecl;
        get_column_description : function (table:PAtkTable; column:gint):Pgchar; cdecl;
        get_column_header : function (table:PAtkTable; column:gint):PAtkObject; cdecl;
        get_row_description : function (table:PAtkTable; row:gint):Pgchar; cdecl;
        get_row_header : function (table:PAtkTable; row:gint):PAtkObject; cdecl;
        get_summary : function (table:PAtkTable):PAtkObject; cdecl;
        set_caption : procedure (table:PAtkTable; caption:PAtkObject); cdecl;
        set_column_description : procedure (table:PAtkTable; column:gint; description:Pgchar); cdecl;
        set_column_header : procedure (table:PAtkTable; column:gint; header:PAtkObject); cdecl;
        set_row_description : procedure (table:PAtkTable; row:gint; description:Pgchar); cdecl;
        set_row_header : procedure (table:PAtkTable; row:gint; header:PAtkObject); cdecl;
        set_summary : procedure (table:PAtkTable; accessible:PAtkObject); cdecl;
        get_selected_columns : function (table:PAtkTable; selected:PPgint):gint; cdecl;
        get_selected_rows : function (table:PAtkTable; selected:PPgint):gint; cdecl;
        is_column_selected : function (table:PAtkTable; column:gint):gboolean; cdecl;
        is_row_selected : function (table:PAtkTable; row:gint):gboolean; cdecl;
        is_selected : function (table:PAtkTable; row:gint; column:gint):gboolean; cdecl;
        add_row_selection : function (table:PAtkTable; row:gint):gboolean; cdecl;
        remove_row_selection : function (table:PAtkTable; row:gint):gboolean; cdecl;
        add_column_selection : function (table:PAtkTable; column:gint):gboolean; cdecl;
        remove_column_selection : function (table:PAtkTable; column:gint):gboolean; cdecl;
        row_inserted : procedure (table:PAtkTable; row:gint; num_inserted:gint); cdecl;
        column_inserted : procedure (table:PAtkTable; column:gint; num_inserted:gint); cdecl;
        row_deleted : procedure (table:PAtkTable; row:gint; num_deleted:gint); cdecl;
        column_deleted : procedure (table:PAtkTable; column:gint; num_deleted:gint); cdecl;
        row_reordered : procedure (table:PAtkTable); cdecl;
        column_reordered : procedure (table:PAtkTable); cdecl;
        model_changed : procedure (table:PAtkTable); cdecl;
        pad1 : TAtkFunction;
        pad2 : TAtkFunction;
        pad3 : TAtkFunction;
        pad4 : TAtkFunction;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function ATK_TYPE_TABLE : GType;
function ATK_IS_TABLE(obj: pointer) : boolean;
function ATK_TABLE(obj: pointer) : PAtkTable;
function ATK_TABLE_GET_IFACE(obj: pointer) : PAtkTableIface;


function atk_table_get_type:GType; cdecl; external atklib;
function atk_table_ref_at(table:PAtkTable; row:gint; column:gint):PAtkObject; cdecl; external atklib;
function atk_table_get_index_at(table:PAtkTable; row:gint; column:gint):gint; cdecl; external atklib;
function atk_table_get_column_at_index(table:PAtkTable; index:gint):gint; cdecl; external atklib;
function atk_table_get_row_at_index(table:PAtkTable; index:gint):gint; cdecl; external atklib;
function atk_table_get_n_columns(table:PAtkTable):gint; cdecl; external atklib;
function atk_table_get_n_rows(table:PAtkTable):gint; cdecl; external atklib;
function atk_table_get_column_extent_at(table:PAtkTable; row:gint; column:gint):gint; cdecl; external atklib;
function atk_table_get_row_extent_at(table:PAtkTable; row:gint; column:gint):gint; cdecl; external atklib;
function atk_table_get_caption(table:PAtkTable):PAtkObject; cdecl; external atklib;
function atk_table_get_column_description(table:PAtkTable; column:gint):Pgchar; cdecl; external atklib;
function atk_table_get_column_header(table:PAtkTable; column:gint):PAtkObject; cdecl; external atklib;
function atk_table_get_row_description(table:PAtkTable; row:gint):Pgchar; cdecl; external atklib;
function atk_table_get_row_header(table:PAtkTable; row:gint):PAtkObject; cdecl; external atklib;
function atk_table_get_summary(table:PAtkTable):PAtkObject; cdecl; external atklib;
procedure atk_table_set_caption(table:PAtkTable; caption:PAtkObject); cdecl; external atklib;
procedure atk_table_set_column_description(table:PAtkTable; column:gint; description:Pgchar); cdecl; external atklib;
procedure atk_table_set_column_header(table:PAtkTable; column:gint; header:PAtkObject); cdecl; external atklib;
procedure atk_table_set_row_description(table:PAtkTable; row:gint; description:Pgchar); cdecl; external atklib;
procedure atk_table_set_row_header(table:PAtkTable; row:gint; header:PAtkObject); cdecl; external atklib;
procedure atk_table_set_summary(table:PAtkTable; accessible:PAtkObject); cdecl; external atklib;
function atk_table_get_selected_columns(table:PAtkTable; selected:PPgint):gint; cdecl; external atklib;
function atk_table_get_selected_rows(table:PAtkTable; selected:PPgint):gint; cdecl; external atklib;
function atk_table_is_column_selected(table:PAtkTable; column:gint):gboolean; cdecl; external atklib;
function atk_table_is_row_selected(table:PAtkTable; row:gint):gboolean; cdecl; external atklib;
function atk_table_is_selected(table:PAtkTable; row:gint; column:gint):gboolean; cdecl; external atklib;
function atk_table_add_row_selection(table:PAtkTable; row:gint):gboolean; cdecl; external atklib;
function atk_table_remove_row_selection(table:PAtkTable; row:gint):gboolean; cdecl; external atklib;
function atk_table_add_column_selection(table:PAtkTable; column:gint):gboolean; cdecl; external atklib;
function atk_table_remove_column_selection(table:PAtkTable; column:gint):gboolean; cdecl; external atklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function ATK_TYPE_TABLE : GType;
begin
   ATK_TYPE_TABLE:=atk_table_get_type;
end;

function ATK_IS_TABLE(obj: pointer) : boolean;
begin
   ATK_IS_TABLE:=G_TYPE_CHECK_INSTANCE_TYPE(obj,ATK_TYPE_TABLE);
end;

function ATK_TABLE(obj: pointer) : PAtkTable;
begin
   ATK_TABLE:=PAtkTable(G_TYPE_CHECK_INSTANCE_CAST(obj,ATK_TYPE_TABLE));
end;

function ATK_TABLE_GET_IFACE(obj: pointer) : PAtkTableIface;
begin
   ATK_TABLE_GET_IFACE:=PAtkTableIface(G_TYPE_INSTANCE_GET_INTERFACE(obj,ATK_TYPE_TABLE));
end;

{$ENDIF read_implementation}

