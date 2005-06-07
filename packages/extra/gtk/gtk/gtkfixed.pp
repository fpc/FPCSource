{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkFixed = ^TGtkFixed;
     TGtkFixed = record
          container : TGtkContainer;
          children : PGList;
       end;

     PGtkFixedClass = ^TGtkFixedClass;
     TGtkFixedClass = record
          parent_class : TGtkContainerClass;
       end;

     PGtkFixedChild = ^TGtkFixedChild;
     TGtkFixedChild = record
          widget : PGtkWidget;
          x : gint16;
          y : gint16;
       end;

Type
  GTK_FIXED=PGtkFixed;
  GTK_FIXED_CLASS=PGtkFixedClass;

function  GTK_FIXED_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_fixed_get_type';
function  GTK_IS_FIXED(obj:pointer):boolean;
function  GTK_IS_FIXED_CLASS(klass:pointer):boolean;

function  gtk_fixed_get_type:TGtkType;cdecl;external gtkdll name 'gtk_fixed_get_type';
function  gtk_fixed_new : PGtkWidget;cdecl;external gtkdll name 'gtk_fixed_new';
procedure gtk_fixed_put(fixed:PGtkFixed; widget:PGtkWidget; x:gint16; y:gint16);cdecl;external gtkdll name 'gtk_fixed_put';
procedure gtk_fixed_move(fixed:PGtkFixed; widget:PGtkWidget; x:gint16; y:gint16);cdecl;external gtkdll name 'gtk_fixed_move';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_FIXED(obj:pointer):boolean;
begin
  GTK_IS_FIXED:=(obj<>nil) and GTK_IS_FIXED_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_FIXED_CLASS(klass:pointer):boolean;
begin
  GTK_IS_FIXED_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_FIXED_TYPE);
end;

{$endif read_implementation}


