// included by gtk2.pas

{
   NOTE this widget is considered too specialized/little-used for
   GTK+, and will in the future be moved to some other package.  If
   your application needs this widget, feel free to use it, as the
   widget does work and is useful in some applications; it's just not
   of general interest. However, we are not accepting new features for
   the widget, and it will eventually move out of the GTK+
   distribution.
  }

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkInputDialog = ^TGtkInputDialog;
   TGtkInputDialog = record
        dialog : TGtkDialog;
        axis_list : PGtkWidget;
        axis_listbox : PGtkWidget;
        mode_optionmenu : PGtkWidget;
        close_button : PGtkWidget;
        save_button : PGtkWidget;
        axis_items : array[0..(GDK_AXIS_LAST)-1] of PGtkWidget;
        current_device : PGdkDevice;
        keys_list : PGtkWidget;
        keys_listbox : PGtkWidget;
     end;

{ Padding for future expansion  }
   PGtkInputDialogClass = ^TGtkInputDialogClass;
   TGtkInputDialogClass = record
        parent_class : TGtkDialogClass;
        enable_device : procedure (inputd:PGtkInputDialog; device:PGdkDevice); cdecl;
        disable_device : procedure (inputd:PGtkInputDialog; device:PGdkDevice); cdecl;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;

{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_INPUT_DIALOG : GType;
function GTK_INPUT_DIALOG(obj: pointer) : PGtkInputDialog;
function GTK_INPUT_DIALOG_CLASS(klass: pointer) : PGtkInputDialogClass;
function GTK_IS_INPUT_DIALOG(obj: pointer) : boolean;
function GTK_IS_INPUT_DIALOG_CLASS(klass: pointer) : boolean;
function GTK_INPUT_DIALOG_GET_CLASS(obj: pointer) : PGtkInputDialogClass;


function gtk_input_dialog_get_type:TGtkType; cdecl; external gtklib;
function gtk_input_dialog_new:PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_INPUT_DIALOG : GType;
begin
   GTK_TYPE_INPUT_DIALOG:=gtk_input_dialog_get_type;
end;

function GTK_INPUT_DIALOG(obj: pointer) : PGtkInputDialog;
begin
   GTK_INPUT_DIALOG:=PGtkInputDialog(GTK_CHECK_CAST(obj,GTK_TYPE_INPUT_DIALOG));
end;

function GTK_INPUT_DIALOG_CLASS(klass: pointer) : PGtkInputDialogClass;
begin
   GTK_INPUT_DIALOG_CLASS:=PGtkInputDialogClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_INPUT_DIALOG));
end;

function GTK_IS_INPUT_DIALOG(obj: pointer) : boolean;
begin
   GTK_IS_INPUT_DIALOG:=GTK_CHECK_TYPE(obj,GTK_TYPE_INPUT_DIALOG);
end;

function GTK_IS_INPUT_DIALOG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_INPUT_DIALOG_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_INPUT_DIALOG);
end;

function GTK_INPUT_DIALOG_GET_CLASS(obj: pointer) : PGtkInputDialogClass;
begin
   GTK_INPUT_DIALOG_GET_CLASS:=PGtkInputDialogClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_INPUT_DIALOG));
end;
{$ENDIF read_implementation}

// included by gtk2.pas

