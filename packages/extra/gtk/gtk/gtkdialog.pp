{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkDialog = ^TGtkDialog;
     TGtkDialog = record
          window : TGtkWindow;
          vbox : PGtkWidget;
          action_area : PGtkWidget;
       end;

     PGtkDialogClass = ^TGtkDialogClass;
     TGtkDialogClass = record
          parent_class : TGtkWindowClass;
       end;

Type
  GTK_DIALOG=PGtkDialog;
  GTK_DIALOG_CLASS=PGtkDialogClass;

function  GTK_DIALOG_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_dialog_get_type';
function  GTK_IS_DIALOG(obj:pointer):boolean;
function  GTK_IS_DIALOG_CLASS(klass:pointer):boolean;

function  gtk_dialog_get_type:TGtkType;cdecl;external gtkdll name 'gtk_dialog_get_type';
function  gtk_dialog_new : PGtkWidget;cdecl;external gtkdll name 'gtk_dialog_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_DIALOG(obj:pointer):boolean;
begin
  GTK_IS_DIALOG:=(obj<>nil) and GTK_IS_DIALOG_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_DIALOG_CLASS(klass:pointer):boolean;
begin
  GTK_IS_DIALOG_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_DIALOG_TYPE);
end;

{$endif read_implementation}


