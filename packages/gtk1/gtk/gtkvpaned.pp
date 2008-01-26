{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkVPaned = ^TGtkVPaned;
     TGtkVPaned = record
          paned : TGtkPaned;
       end;

     PGtkVPanedClass = ^TGtkVPanedClass;
     TGtkVPanedClass = record
          parent_class : TGtkPanedClass;
       end;

Type
  GTK_VPANED=PGtkVPaned;
  GTK_VPANED_CLASS=PGtkVPanedClass;

function  GTK_VPANED_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_vpaned_get_type';
function  GTK_IS_VPANED(obj:pointer):boolean;
function  GTK_IS_VPANED_CLASS(klass:pointer):boolean;

function  gtk_vpaned_get_type:TGtkType;cdecl;external gtkdll name 'gtk_vpaned_get_type';
function  gtk_vpaned_new:PGtkWidget;cdecl;external gtkdll name 'gtk_vpaned_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_VPANED(obj:pointer):boolean;
begin
  GTK_IS_VPANED:=(obj<>nil) and GTK_IS_VPANED_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_VPANED_CLASS(klass:pointer):boolean;
begin
  GTK_IS_VPANED_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_VPANED_TYPE);
end;

{$endif read_implementation}


