// included by gtk2.pas

{$IFDEF read_forward_definitions}
{$ENDIF read_forward_definitions}

//------------------------------------------------------------------------------

{$IFDEF read_interface_types}
   PGtkColorSelectionDialog = ^TGtkColorSelectionDialog;
   TGtkColorSelectionDialog = record
        parent_instance : TGtkDialog;
        colorsel : PGtkWidget;
        ok_button : PGtkWidget;
        cancel_button : PGtkWidget;
        help_button : PGtkWidget;
     end;

{ Padding for future expansion  }
   PGtkColorSelectionDialogClass = ^TGtkColorSelectionDialogClass;
   TGtkColorSelectionDialogClass = record
        parent_class : TGtkDialogClass;
        _gtk_reserved1 : procedure ; cdecl;
        _gtk_reserved2 : procedure ; cdecl;
        _gtk_reserved3 : procedure ; cdecl;
        _gtk_reserved4 : procedure ; cdecl;
     end;
{$ENDIF read_interface_types}

//------------------------------------------------------------------------------

{$IFDEF read_interface_rest}
function GTK_TYPE_COLOR_SELECTION_DIALOG : GType;
function GTK_COLOR_SELECTION_DIALOG(obj: pointer) : PGtkColorSelectionDialog;
function GTK_COLOR_SELECTION_DIALOG_CLASS(klass: pointer) : PGtkColorSelectionDialogClass;
function GTK_IS_COLOR_SELECTION_DIALOG(obj: pointer) : boolean;
function GTK_IS_COLOR_SELECTION_DIALOG_CLASS(klass: pointer) : boolean;
function GTK_COLOR_SELECTION_DIALOG_GET_CLASS(obj: pointer) : PGtkColorSelectionDialogClass;


{ ColorSelectionDialog  }

function gtk_color_selection_dialog_get_type:TGtkType; cdecl; external gtklib;
function gtk_color_selection_dialog_new(title:Pgchar):PGtkWidget; cdecl; external gtklib;
{$ENDIF read_interface_rest}

//------------------------------------------------------------------------------

{$IFDEF read_implementation}
function GTK_TYPE_COLOR_SELECTION_DIALOG : GType;
begin
   GTK_TYPE_COLOR_SELECTION_DIALOG:=gtk_color_selection_dialog_get_type;
end;

function GTK_COLOR_SELECTION_DIALOG(obj: pointer) : PGtkColorSelectionDialog;
begin
   GTK_COLOR_SELECTION_DIALOG:=PGtkColorSelectionDialog(GTK_CHECK_CAST(obj,GTK_TYPE_COLOR_SELECTION_DIALOG));
end;

function GTK_COLOR_SELECTION_DIALOG_CLASS(klass: pointer) : PGtkColorSelectionDialogClass;
begin
   GTK_COLOR_SELECTION_DIALOG_CLASS:=PGtkColorSelectionDialogClass(GTK_CHECK_CLASS_CAST(klass,GTK_TYPE_COLOR_SELECTION_DIALOG));
end;

function GTK_IS_COLOR_SELECTION_DIALOG(obj: pointer) : boolean;
begin
   GTK_IS_COLOR_SELECTION_DIALOG:=GTK_CHECK_TYPE(obj,GTK_TYPE_COLOR_SELECTION_DIALOG);
end;

function GTK_IS_COLOR_SELECTION_DIALOG_CLASS(klass: pointer) : boolean;
begin
   GTK_IS_COLOR_SELECTION_DIALOG_CLASS:=GTK_CHECK_CLASS_TYPE(klass,GTK_TYPE_COLOR_SELECTION_DIALOG);
end;

function GTK_COLOR_SELECTION_DIALOG_GET_CLASS(obj: pointer) : PGtkColorSelectionDialogClass;
begin
   GTK_COLOR_SELECTION_DIALOG_GET_CLASS:=PGtkColorSelectionDialogClass(GTK_CHECK_GET_CLASS(obj,GTK_TYPE_COLOR_SELECTION_DIALOG));
end;

{$ENDIF read_implementation}

