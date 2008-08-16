// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkMessageType = ^TGtkMessageType;
   TGtkMessageType = (
     GTK_MESSAGE_INFO,
     GTK_MESSAGE_WARNING,
     GTK_MESSAGE_QUESTION,
     GTK_MESSAGE_ERROR
   );

   PGtkButtonsType = ^TGtkButtonsType;
   TGtkButtonsType = (
     GTK_BUTTONS_NONE,
     GTK_BUTTONS_OK,
     GTK_BUTTONS_CLOSE,
     GTK_BUTTONS_CANCEL,
     GTK_BUTTONS_YES_NO,
     GTK_BUTTONS_OK_CANCEL
   );


{< private > }
   PGtkMessageDialog = ^TGtkMessageDialog;
   TGtkMessageDialog = record
        parent_instance : TGtkDialog;
        image : PGtkWidget;
        _label : PGtkWidget;
     end;

{ Padding for future expansion  }
   PGtkMessageDialogClass = ^TGtkMessageDialogClass;
   TGtkMessageDialogClass = record
        parent_class : TGtkDialogClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;


{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_MESSAGE_DIALOG : GType;
function GTK_MESSAGE_DIALOG(obj: pointer) : PGtkMessageDialog;
function GTK_MESSAGE_DIALOG_CLASS(klass: pointer) : PGtkMessageDialogClass;
function GTK_IS_MESSAGE_DIALOG(obj: pointer) : boolean;
function GTK_IS_MESSAGE_DIALOG_CLASS(klass: pointer) : boolean;
function GTK_MESSAGE_DIALOG_GET_CLASS(obj: pointer) : PGtkMessageDialogClass;

function gtk_message_dialog_get_type:TGtkType; cdecl; external gtklib;
{$IFNDEF KYLIX}
function gtk_message_dialog_new(parent:PGtkWindow; flags:TGtkDialogFlags;
  _type:TGtkMessageType; buttons:TGtkButtonsType; message_format:Pgchar;
  args:array of const):PGtkWidget; cdecl; overload; external gtklib;
function gtk_message_dialog_new(parent:PGtkWindow; flags:TGtkDialogFlags;
  _type:TGtkMessageType; buttons:TGtkButtonsType;
  message_format:Pgchar):PGtkWidget; cdecl; overload; varargs; external gtklib;
{$ELSE}
function gtk_message_dialog_new(parent:PGtkWindow; flags:TGtkDialogFlags;
  _type:TGtkMessageType; buttons:TGtkButtonsType;
  message_format:Pgchar):PGtkWidget; varargs; cdecl; external gtklib;
{$ENDIF}

{$ifdef HasGTK2_4}
function gtk_message_dialog_new_with_markup(parent: PGtkWindow; flags: TGtkDialogFlags;
  _type: TGtkMessageType; buttons: TGtkButtonsType; 
  message_format: Pgchar): PGtkWidget; varargs; cdecl; external gtklib;
procedure gtk_message_dialog_set_markup(message_dialog: PGtkMessageDialog; str:Pgchar); cdecl; external gtklib;
{$endif}

{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_MESSAGE_DIALOG : GType;
begin
   GTK_TYPE_MESSAGE_DIALOG:=gtk_message_dialog_get_type;
end;

function GTK_MESSAGE_DIALOG(obj: pointer) : PGtkMessageDialog;
begin
   GTK_MESSAGE_DIALOG:=PGtkMessageDialog(GTK_CHECK_CAST(obj,GTK_TYPE_MESSAGE_DIALOG));
end;

function GTK_MESSAGE_DIALOG_CLASS(klass: pointer) : PGtkMessageDialogClass;
begin
   GTK_MESSAGE_DIALOG_CLASS:=PGtkMessageDialogClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_MESSAGE_DIALOG));
end;

function GTK_IS_MESSAGE_DIALOG(obj: pointer) : boolean;
begin
   GTK_IS_MESSAGE_DIALOG:=GTK_CHECK_TYPE(obj,GTK_TYPE_MESSAGE_DIALOG);
end;

function GTK_IS_MESSAGE_DIALOG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_MESSAGE_DIALOG_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_MESSAGE_DIALOG);
end;

function GTK_MESSAGE_DIALOG_GET_CLASS(obj: pointer) : PGtkMessageDialogClass;
begin
   GTK_MESSAGE_DIALOG_GET_CLASS:=PGtkMessageDialogClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_MESSAGE_DIALOG));
end;

{$ENDIF read_implementation}
// included by gtk2.pas

