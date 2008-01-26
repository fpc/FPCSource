// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   // internal type
   PGtkClipboard = pointer;

   TGtkClipboardReceivedFunc = procedure (clipboard:PGtkClipboard; selection_data:PGtkSelectionData; data:gpointer); cdecl;

   TGtkClipboardTextReceivedFunc = procedure (clipboard:PGtkClipboard; text:Pgchar; data:gpointer); cdecl;
{ Should these functions have GtkClipboard  clipboard as the first argument?
   right now for ClearFunc, you may have trouble determining _which_ clipboard
   was cleared, if you reuse your ClearFunc for multiple clipboards.
  }

   TGtkClipboardGetFunc = procedure (clipboard:PGtkClipboard; selection_data:PGtkSelectionData; info:guint; user_data_or_owner:gpointer); cdecl;

   TGtkClipboardClearFunc = procedure (clipboard:PGtkClipboard; user_data_or_owner:gpointer); cdecl;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function gtk_clipboard_get_for_display(display:PGdkDisplay; selection:TGdkAtom):PGtkClipboard; cdecl; external gtklib;

{$ifndef GDK_MULTIHEAD_SAFE}
function gtk_clipboard_get(selection:TGdkAtom):PGtkClipboard; cdecl; external gtklib;
{$endif}

function gtk_clipboard_get_display(clipboard:PGtkClipboard):PGdkDisplay; cdecl; external gtklib;
function gtk_clipboard_set_with_data(clipboard:PGtkClipboard; targets:PGtkTargetEntry; n_targets:guint; get_func:TGtkClipboardGetFunc; clear_func:TGtkClipboardClearFunc;
           user_data:gpointer):gboolean; cdecl; external gtklib;
function gtk_clipboard_set_with_owner(clipboard:PGtkClipboard; targets:PGtkTargetEntry; n_targets:guint; get_func:TGtkClipboardGetFunc; clear_func:TGtkClipboardClearFunc;
           owner:PGObject):gboolean; cdecl; external gtklib;
function gtk_clipboard_get_owner(clipboard:PGtkClipboard):PGObject; cdecl; external gtklib;
procedure gtk_clipboard_clear(clipboard:PGtkClipboard); cdecl; external gtklib;
procedure gtk_clipboard_set_text(clipboard:PGtkClipboard; text:Pgchar; len:gint); cdecl; external gtklib;
procedure gtk_clipboard_request_contents(clipboard:PGtkClipboard; target:TGdkAtom; callback:TGtkClipboardReceivedFunc; user_data:gpointer); cdecl; external gtklib;
procedure gtk_clipboard_request_text(clipboard:PGtkClipboard; callback:TGtkClipboardTextReceivedFunc; user_data:gpointer); cdecl; external gtklib;
function gtk_clipboard_wait_for_contents(clipboard:PGtkClipboard; target:TGdkAtom):PGtkSelectionData; cdecl; external gtklib;
function gtk_clipboard_wait_for_text(clipboard:PGtkClipboard):Pgchar; cdecl; external gtklib;
function gtk_clipboard_wait_is_text_available(clipboard:PGtkClipboard):gboolean; cdecl; external gtklib;
{$ENDIF read_interface_rest}
// included by gtk2.pas

