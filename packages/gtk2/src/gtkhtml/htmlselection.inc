// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  procedure html_selection_start(view:PHtmlView; event:PGdkEventButton);cdecl; external gtkhtmllib;
  procedure html_selection_end(view:PHtmlView; event:PGdkEventButton);cdecl; external gtkhtmllib;
  procedure html_selection_update(view:PHtmlView; event:PGdkEventMotion);cdecl; external gtkhtmllib;
  procedure html_selection_clear(view:PHtmlView);cdecl; external gtkhtmllib;
  procedure html_selection_set(view:PHtmlView; start:PDomNode; offset:longint; len:longint);cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}

{$ENDIF read_implementation}
// included by gtkhtml.pas
