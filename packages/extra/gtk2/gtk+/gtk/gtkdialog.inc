// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
{ Parameters for dialog construction  }
{ call gtk_window_set_modal (win, TRUE)  }
{ call gtk_window_set_destroy_with_parent ()  }
{ no separator bar above buttons  }
   PGtkDialogFlags = ^TGtkDialogFlags;
   TGtkDialogFlags = longint;


{ Convenience enum to use for response_id's.  Positive values are
   totally user-interpreted. GTK will sometimes return
   GTK_RESPONSE_NONE if no response_id is available.

    Typical usage is:
       if (gtk_dialog_run(dialog) = GTK_RESPONSE_ACCEPT) then
         DoSomethingOnSuccess;
  }
{ GTK returns this if a response widget has no response_id,
     or if the dialog gets programmatically hidden or destroyed.
    }
{ GTK won't return these unless you pass them in
     as the response for an action widget. They are
     for your convenience.
    }
{ If the dialog is deleted.  }
{ These are returned from GTK dialogs, and you can also use them
     yourself if you like.
    }

   PGtkResponseType = ^TGtkResponseType;
   TGtkResponseType = longint;


{< private > }
   PGtkDialog = ^TGtkDialog;
   TGtkDialog = record
        window : TGtkWindow;
        vbox : PGtkWidget;
        action_area : PGtkWidget;
        separator : PGtkWidget;
     end;

{ Keybinding signals  }
{ Padding for future expansion  }
   PGtkDialogClass = ^TGtkDialogClass;
   TGtkDialogClass = record
        parent_class : TGtkWindowClass;
        response : procedure (dialog:PGtkDialog; response_id:gint); cdecl;
        close : procedure (dialog:PGtkDialog); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
const
   GTK_DIALOG_MODAL = 1 shl 0;
   GTK_DIALOG_DESTROY_WITH_PARENT = 1 shl 1;
   GTK_DIALOG_NO_SEPARATOR = 1 shl 2;

   GTK_RESPONSE_NONE = -(1);
   GTK_RESPONSE_REJECT = -(2);
   GTK_RESPONSE_ACCEPT = -(3);
   GTK_RESPONSE_DELETE_EVENT = -(4);
   GTK_RESPONSE_OK = -(5);
   GTK_RESPONSE_CANCEL = -(6);
   GTK_RESPONSE_CLOSE = -(7);
   GTK_RESPONSE_YES = -(8);
   GTK_RESPONSE_NO = -(9);
   GTK_RESPONSE_APPLY = -(10);
   GTK_RESPONSE_HELP = -(11);


function GTK_TYPE_DIALOG : GType;
function GTK_DIALOG(obj: pointer) : PGtkDialog;
function GTK_DIALOG_CLASS(klass: pointer) : PGtkDialogClass;
function GTK_IS_DIALOG(obj: pointer) : boolean;
function GTK_IS_DIALOG_CLASS(klass: pointer) : boolean;
function GTK_DIALOG_GET_CLASS(obj: pointer) : PGtkDialogClass;


function gtk_dialog_get_type:TGtkType; cdecl; external gtklib;
function gtk_dialog_new:PGtkWidget; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_dialog_new_with_buttons(title:Pgchar; parent:PGtkWindow; flags:TGtkDialogFlags; first_button_text:Pgchar; args:array of const):PGtkWidget; cdecl; overload; external gtklib;
function gtk_dialog_new_with_buttons(title:Pgchar; parent:PGtkWindow; flags:TGtkDialogFlags; first_button_text:Pgchar):PGtkWidget; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_dialog_new_with_buttons(title:Pgchar; parent:PGtkWindow; flags:TGtkDialogFlags; first_button_text:Pgchar):PGtkWidget; varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_dialog_add_action_widget(dialog:PGtkDialog; child:PGtkWidget; response_id:gint); cdecl; external gtklib;
function gtk_dialog_add_button(dialog:PGtkDialog; button_text:Pgchar; response_id:gint):PGtkWidget; cdecl; external gtklib;
{$IFNDEF KYLIX}
procedure gtk_dialog_add_buttons(dialog:PGtkDialog; first_button_text:Pgchar; args:array of const); overload; cdecl; external gtklib;
procedure gtk_dialog_add_buttons(dialog:PGtkDialog; first_button_text:Pgchar); cdecl; overload; varargs; external gtklib;
{$ELSE}
procedure gtk_dialog_add_buttons(dialog:PGtkDialog; first_button_text:Pgchar); varargs; cdecl; external gtklib;
{$ENDIF}
procedure gtk_dialog_set_response_sensitive(dialog:PGtkDialog; response_id:gint; setting:gboolean); cdecl; external gtklib;
procedure gtk_dialog_set_default_response(dialog:PGtkDialog; response_id:gint); cdecl; external gtklib;
procedure gtk_dialog_set_has_separator(dialog:PGtkDialog; setting:gboolean); cdecl; external gtklib;
function gtk_dialog_get_has_separator(dialog:PGtkDialog):gboolean; cdecl; external gtklib;
{ Emit response signal  }
procedure gtk_dialog_response(dialog:PGtkDialog; response_id:gint); cdecl; external gtklib;
{ Returns response_id  }
function gtk_dialog_run(dialog:PGtkDialog):gint; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_DIALOG : GType;
begin
   GTK_TYPE_DIALOG:=gtk_dialog_get_type;
end;

function GTK_DIALOG(obj: pointer) : PGtkDialog;
begin
   GTK_DIALOG:=PGtkDialog(GTK_CHECK_CAST(obj,GTK_TYPE_DIALOG));
end;

function GTK_DIALOG_CLASS(klass: pointer) : PGtkDialogClass;
begin
   GTK_DIALOG_CLASS:=PGtkDialogClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_DIALOG));
end;

function GTK_IS_DIALOG(obj: pointer) : boolean;
begin
   GTK_IS_DIALOG:=GTK_CHECK_TYPE(obj,GTK_TYPE_DIALOG);
end;

function GTK_IS_DIALOG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_DIALOG_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_DIALOG);
end;

function GTK_DIALOG_GET_CLASS(obj: pointer) : PGtkDialogClass;
begin
   GTK_DIALOG_GET_CLASS:=PGtkDialogClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_DIALOG));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

