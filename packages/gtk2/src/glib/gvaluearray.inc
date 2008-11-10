// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGValueArray = ^TGValueArray;
   TGValueArray = record
        n_values : guint;
        values : PGValue;
        n_prealloced : guint;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function g_value_array_get_nth(value_array:PGValueArray; index:guint):PGValue; cdecl; external gobjectlib;
function g_value_array_new(n_prealloced:guint):PGValueArray; cdecl; external gobjectlib;
procedure g_value_array_free(value_array:PGValueArray); cdecl; external gobjectlib;

function g_value_array_copy(value_array:PGValueArray):PGValueArray; cdecl; external gobjectlib;

function g_value_array_prepend(value_array:PGValueArray; value:PGValue):PGValueArray; cdecl; external gobjectlib;

function g_value_array_append(value_array:PGValueArray; value:PGValue):PGValueArray; cdecl; external gobjectlib;

function g_value_array_insert(value_array:PGValueArray; index:guint; value:PGValue):PGValueArray; cdecl; external gobjectlib;
function g_value_array_remove(value_array:PGValueArray; index:guint):PGValueArray; cdecl; external gobjectlib;
function g_value_array_sort(value_array:PGValueArray; compare_func:TGCompareFunc):PGValueArray; cdecl; external gobjectlib;
function g_value_array_sort_with_data(value_array:PGValueArray; compare_func:TGCompareDataFunc; user_data:gpointer):PGValueArray; cdecl; external gobjectlib;

{$ENDIF read_interface_rest}
