{
}

{****************************************************************************
                                 Interface
****************************************************************************}

{$ifdef read_interface}

    type
       PGtkArrow = ^TGtkArrow;
       TGtkArrow = record
            misc : TGtkMisc;
            arrow_type : gint16;
            shadow_type : gint16;
         end;

       PGtkArrowClass = ^TGtkArrowClass;
       TGtkArrowClass = record
            parent_class : TGtkMiscClass;
         end;

Type
  GTK_ARROW = PGtkArrow;
  GTK_ARROW_CLASS = PGtkArrowClass;

function  GTK_ARROW_TYPE:TGtkType;cdecl;external gtkdll name 'gtk_arrow_get_type';
function  GTK_IS_ARROW(obj:pointer):boolean;
function  GTK_IS_ARROW_CLASS(klass:pointer):boolean;

function  gtk_arrow_get_type:TGtkType;cdecl;external gtkdll name 'gtk_arrow_get_type';
function  gtk_arrow_new (arrow_type:TGtkArrowType; shadow_type:TGtkShadowType):PGtkWidget;cdecl;external gtkdll name 'gtk_arrow_new';
procedure gtk_arrow_set(arrow:PGtkArrow; arrow_type:TGtkArrowType; shadow_type:TGtkShadowType);cdecl;external gtkdll name 'gtk_arrow_set';

{$endif read_interface}


{****************************************************************************
                              Implementation
****************************************************************************}

{$ifdef read_implementation}

function  GTK_IS_ARROW(obj:pointer):boolean;
begin
  GTK_IS_ARROW:=(obj<>nil) and GTK_IS_ARROW_CLASS(PGtkTypeObject(obj)^.klass);
end;

function  GTK_IS_ARROW_CLASS(klass:pointer):boolean;
begin
  GTK_IS_ARROW_CLASS:=(klass<>nil) and (PGtkTypeClass(klass)^.thetype=GTK_ARROW_TYPE);
end;

{$endif read_implementation}

