{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHPaned = ^TGtkHPaned;
     TGtkHPaned = record
          paned : TGtkPaned;
       end;

     PGtkHPanedClass = ^TGtkHPanedClass;
     TGtkHPanedClass = record
          parent_class : TGtkPanedClass;
       end;

Type
  GTK_HPANED=PGtkHPaned;
  GTK_HPANED_CLASS=PGtkHPanedClass;

function  GTK_HPANED_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hpaned_get_type';
function  GTK_IS_HPANED(obj:pointer):boolean;
function  GTK_IS_HPANED_CLASS(klass:pointer):boolean;

function  gtk_hpaned_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hpaned_get_type';
function  gtk_hpaned_new : PGtkWidget;cdecl;external gtkdll name 'gtk_hpaned_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HPANED(obj:pointer):boolean;
begin
  GTK_IS_HPANED:=(obj<>nil) and GTK_IS_HPANED_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HPANED_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HPANED_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HPANED_TYPE);
end;

{$endif read_implementation}


