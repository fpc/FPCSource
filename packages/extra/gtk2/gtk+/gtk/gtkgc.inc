// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function gtk_gc_get(depth:gint; colormap:PGdkColormap; values:PGdkGCValues; values_mask:TGdkGCValuesMask):PGdkGC; cdecl; external gtklib;
procedure gtk_gc_release(gc:PGdkGC); cdecl; external gtklib;
{$ENDIF read_interface_rest}

