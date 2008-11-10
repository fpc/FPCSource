// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
procedure g_qsort_with_data (pbase        : gconstpointer;
                             total_elems  : gint;
                             size         : gsize;
                             compare_func : TGCompareDataFunc;
                             user_data    : gpointer);cdecl;external gliblib name 'g_qsort_with_data';
{$ENDIF read_interface_rest}
// included by glib2.pas





