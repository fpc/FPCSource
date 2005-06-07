{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkInputDialog = ^TGtkInputDialog;
     TGtkInputDialog = record
          dialog : TGtkDialog;
          axis_list : PGtkWidget;
          axis_listbox : PGtkWidget;
          mode_optionmenu : PGtkWidget;
          close_button : PGtkWidget;
          save_button : PGtkWidget;
          axis_items : array[0..ord(GDK_AXIS_LAST)-1] of PGtkWidget;
          current_device : guint32;
          keys_list : PGtkWidget;
          keys_listbox : PGtkWidget;
       end;

     PGtkInputDialogClass = ^TGtkInputDialogClass;
     TGtkInputDialogClass = record
          parent_class : TGtkWindowClass;
          enable_device : procedure (inputd:PGtkInputDialog; devid:guint32);cdecl;
          disable_device : procedure (inputd:PGtkInputDialog; devid:guint32);cdecl;
       end;

Type
  GTK_INPUT_DIALOG=PGtkInputDialog;
  GTK_INPUT_DIALOG_CLASS=PGtkInputDialogClass;

function  GTK_INPUT_DIALOG_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_input_dialog_get_type';
function  GTK_IS_INPUT_DIALOG(obj:pointer):boolean;
function  GTK_IS_INPUT_DIALOG_CLASS(klass:pointer):boolean;

function  gtk_input_dialog_get_type:TGtkType;cdecl;external gtkdll name 'gtk_input_dialog_get_type';
function  gtk_input_dialog_new:PGtkWidget;cdecl;external gtkdll name 'gtk_input_dialog_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_INPUT_DIALOG(obj:pointer):boolean;
begin
  GTK_IS_INPUT_DIALOG:=(obj<>nil) and GTK_IS_INPUT_DIALOG_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_INPUT_DIALOG_CLASS(klass:pointer):boolean;
begin
  GTK_IS_INPUT_DIALOG_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_INPUT_DIALOG_TYPE);
end;

{$endif read_implementation}


