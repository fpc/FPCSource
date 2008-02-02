// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

  THtmlStreamBufferCloseFunc = procedure (str:Pgchar; len:gint; user_data:gpointer);cdecl;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  function html_stream_buffer_new(close_func:THtmlStreamBufferCloseFunc; user_data:gpointer):PHtmlStream;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
{$ENDIF read_implementation}

// included by gtkhtml.pas
