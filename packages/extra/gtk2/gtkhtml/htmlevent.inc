// included by gtkhtml.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}

  procedure html_event_mouse_move(view:PHtmlView; event:PGdkEventMotion);cdecl; external gtkhtmllib;
  procedure html_event_button_press(view:PHtmlView; button:PGdkEventButton);cdecl; external gtkhtmllib;
  procedure html_event_button_release(view:PHtmlView; event:PGdkEventButton);cdecl; external gtkhtmllib;
  procedure html_event_activate(view:PHtmlView);cdecl; external gtkhtmllib;
  function html_event_key_press(view:PHtmlView; event:PGdkEventKey):gboolean;cdecl; external gtkhtmllib;
  function html_event_find_root_box(self:PHtmlBox; x:gint; y:gint):PHtmlBox;cdecl; external gtkhtmllib;

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}


{$ENDIF read_implementation}
// included by gtkhtml.pas
