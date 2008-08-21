// included by glib2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
  NULL = nil;
  gTRUE = 1;
  gFALSE = 0;

function CLAMP(x, MinX, MaxX: integer): integer;
// Macros by analogy to GINT_TO_POINTER, GPOINTER_TO_INT
function GPOINTER_TO_SIZE(p: GPointer): GSize;
function GSIZE_TO_POINTER(s: GSize): GPointer;

{$ENDIF read_interface_rest}
// included by glib2.pas


