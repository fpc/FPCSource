{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

  type
     PGtkHSeparator = ^TGtkHSeparator;
     TGtkHSeparator = record
          separator : TGtkSeparator;
       end;

     PGtkHSeparatorClass = ^TGtkHSeparatorClass;
     TGtkHSeparatorClass = record
          parent_class : TGtkSeparatorClass;
       end;

Type
  GTK_HSEPARATOR=PGtkHSeparator;
  GTK_HSEPARATOR_CLASS=PGtkHSeparatorClass;

function  GTK_HSEPARATOR_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_hseparator_get_type';
function  GTK_IS_HSEPARATOR(obj:pointer):boolean;
function  GTK_IS_HSEPARATOR_CLASS(klass:pointer):boolean;

function  gtk_hseparator_get_type:TGtkType;cdecl;external gtkdll name 'gtk_hseparator_get_type';
function  gtk_hseparator_new:PGtkWidget;cdecl;external gtkdll name 'gtk_hseparator_new';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_HSEPARATOR(obj:pointer):boolean;
begin
  GTK_IS_HSEPARATOR:=(obj<>nil) and GTK_IS_HSEPARATOR_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_HSEPARATOR_CLASS(klass:pointer):boolean;
begin
  GTK_IS_HSEPARATOR_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_HSEPARATOR_TYPE);
end;

{$endif read_implementation}


