{
   $Id$
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


{
  $Log$
  Revision 1.2  2002-09-07 15:42:59  peter
    * old logs removed and tabs fixed

  Revision 1.1  2002/01/29 17:55:10  peter
    * splitted to base and extra

}
