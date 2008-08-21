// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
procedure g_on_error_query (prg_name:Pgchar);cdecl;external gliblib name 'g_on_error_query';
procedure g_on_error_stack_trace(prg_name:Pgchar);cdecl;external gliblib name 'g_on_error_stack_trace';
{$ENDIF read_interface_rest}
// included by glib2.pas