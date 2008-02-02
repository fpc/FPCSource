// included by gtk2.pas

{$IFDEF read_interface_rest}

{$ifdef G_ENABLE_DEBUG}
type
   PGtkDebugFlag = longint;
const
   GTK_DEBUG_MISC        := 1 shl 0,
   GTK_DEBUG_PLUGSOCKET  := 1 shl 1,
   GTK_DEBUG_TEXT        := 1 shl 2,
   GTK_DEBUG_TREE        := 1 shl 3,
   GTK_DEBUG_UPDATES     := 1 shl 4,
   GTK_DEBUG_KEYBINDINGS := 1 shl 5
{$endif}

{$IFNDEF KYLIX}
  var
     gtk_debug_flags : guint;cvar;public;
{$ENDIF}
{$ENDIF read_interface_rest}

// included by gtk2.pas

