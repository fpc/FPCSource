{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkVBox = ^TGtkVBox;
       TGtkVBox = record
            box : TGtkBox;
         end;

       PGtkVBoxClass = ^TGtkVBoxClass;
       TGtkVBoxClass = record
            parent_class : TGtkBoxClass;
         end;

Type
  GTK_VBOX=PGtkVBox;
  GTK_VBOX_CLASS=PGtkVBoxClass;

function  GTK_VBOX_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vbox_get_type';
function  GTK_IS_VBOX(obj:pointer):boolean;
function  GTK_IS_VBOX_CLASS(klass:pointer):boolean;

function  gtk_vbox_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vbox_get_type';
function  gtk_vbox_new(homogeneous:gboolean; spacing:gint):PGtkWidget;cdecl;external gtkdll name 'gtk_vbox_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VBOX(obj:pointer):boolean;
begin
  GTK_IS_VBOX:=(obj<>nil) and GTK_IS_VBOX_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VBOX_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VBOX_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VBOX_TYPE);
end;

{$endif read_implementation}


